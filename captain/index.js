const Docker = require("dockerode");
const fs = require("fs");
const path = require("path");
const fetch = require("node-fetch");
const md5File = require("md5-file");
const md5Hex = require("md5-hex");

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
  }
})();
