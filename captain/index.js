const Docker = require("dockerode");
const fs = require("fs");
const path = require("path");
const fetch = require("node-fetch");
const md5File = require("md5-file");

const socket = process.env.DOCKER_SOCKET || "/var/run/docker.sock";
const stats = fs.statSync(socket);

if (!stats.isSocket()) {
  throw new Error("Are you sure the docker is running?");
}

const docker = new Docker({ socketPath: socket });

// docker.listContainers({ all: false }, (err, containers) => {
//   console.log("!ALL: " + containers.length);
//   containers.forEach(container => console.log(container.Image));
// });

// docker.createContainer(
//   {
//     Image: "aichallenge/sandbox:latest",
//     AttachStdin: true,
//     AttachStdout: true,
//     AttachStderr: true,
//     Tty: false,
//     Cmd: []
//   },
//   function(err, container) {
//     container.start(function(err) {
//       container.exec(
//         { Cmd: ["echo", "hello"], AttachStdin: true, AttachStdout: true },
//         function(err, exec) {
//           exec.start({ hijack: true, stdin: true }, function(err, stream) {
//             // shasum can't finish until after its stdin has been closed, telling it that it has
//             // read all the bytes it needs to sum. Without a socket upgrade, there is no way to
//             // close the write-side of the stream without also closing the read-side!

//             console.log("lol");
//             // Fortunately, we have a regular TCP socket now, so when the readstream finishes and closes our
//             // stream, it is still open for reading and we will still get our results :-)
//             docker.modem.demuxStream(
//               stream.output,
//               process.stdout,
//               process.stderr
//             );
//           });
//         }
//       );
//     });
//   }
// );

const websiteHost = "http://192.168.1.61";
let apiKey = "apikey";

function getUrl(method) {
  return `${websiteHost}/${method}.php?api_key=${apiKey}`;
}

async function setup() {
  const url = `${websiteHost}/api_server_setup_json.php?api_create_key=${apiKey}`;
  console.log(`Fetching ${url}`);
  const response = await fetch(url);

  if (!response.ok) {
    console.error("Could not get task");
    return;
  }

  return await response.json();
}

async function getTask() {
  const url = getUrl("api_get_task");
  console.log(`Fetching ${url}`);
  const response = await fetch(url);

  if (!response.ok) {
    console.error("Could not get task");
    // return;
  }

  return await response.json();
}

async function getSubmissionHash(submissionId) {
  let url = getUrl("api_get_submission_hash");
  url = `${url}&submission_id=${submissionId}`;
  console.log(`Fetching ${url}`);

  const response = await fetch(url);

  if (!response.ok) {
    console.error(
      `Could not get submission has for submission ${submissionId}`
    );
  }

  return await response.json();
}

async function getMap(mapName) {
  const url = `http://${websiteHost}/map/${mapName}`;
  console.log(`Fetching ${url}`);

  const response = await fetch(url);

  if (!response.ok) {
    console.error(`Could not get map ${mapName}`);
  }

  return await response.text();
}

async function getSubmission(submissionId, downloadDir) {
  let url = getUrl("api_get_submission");
  url = `${url}&submission_id=${submissionId}`;
  console.log(`Fetching ${url}`);

  const response = await fetch(url);

  if (!response.ok) {
    console.error(
      `Could not get submission has for submission ${submissionId}`
    );
  }

  const header = response.headers.get("Content-disposition");
  if (!header) {
    const body = await response.text();
    console.error("FAIIILLL", body);
    return;
  }
  const filename = header.split("filename=")[1];
  const destinationPath = path.join(downloadDir, filename);
  const dest = fs.createWriteStream(destinationPath);
  response.body.pipe(dest);
  return destinationPath;
}

(async () => {
  const { new_key } = await setup();
  apiKey = new_key;
  const task = await getTask();
  const submissionId = task.submission_id;

  if (submissionId === 0) {
    console.log("Submission id is 0, so no new tasks, stopping");
    return;
  }

  if (task.task === "compile") {
    const downloadDir = fs.mkdtempSync("aichallenge-compiler-");

    const { hash } = await getSubmissionHash(submissionId);
    const entryPath = await getSubmission(submissionId, downloadDir);

    console.log(`Downloaded submission ${submissionId} to ${submissionId}`);

    // TODO verify hash
    // md5File(filename, (err, verifyHash) => {
    //   if (err) {
    //     throw err;
    //   }

    //   if (hash === verifyHash) {
    //     console.log("Hashes are equal!");
    //   } else {
    // console.warn(`${hash} did not equal ${localHash}`);
    //   }
    // });

    // Start container with volume equal to entryPath
    // In container check argument for the type of task? Perhaps
    // give multiple arguments, such as --compile

    // If something failed, it will create a last_post.json file somewhere
    // that needs to be sent to the website
  } else if (task.task === "game") {
    // Download all the submissions
    // Compile the submissions
    // Report errors and abort if necessary
    // Get map
    // Run a game?
    const gameTask = {
      task: "game",
      timestamp: new Date(),
      matchup_id: 423,
      map_filename: "maze/maze_p02_16.map",
      max_turns: 200,
      submissions: [],
      game_options: {
        turns: 1500,
        loadtime: 3000,
        turntime: 500,
        viewradius2: 77,
        attackradius2: 5,
        spawnradius2: 1,
        serial: 2,
        food_rate: (5, 11),
        food_turn: (19, 37),
        food_start: (75, 175),
        food_visible: (3, 5),
        food: "symmetric",
        attack: "focus",
        kill_points: 2,
        cutoff_turn: 150,
        cutoff_percent: 0.85
      }
    };

    const map = await getMap(map_filename);
    // TODO cache map
    const options = {
      game_options: gameTask.game_options,
      map,
      turns: gameTask.max_turns
    };
  }
})();
