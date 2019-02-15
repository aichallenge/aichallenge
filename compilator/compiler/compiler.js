const fs = require("fs");
const path = require("path");
const yauzl = require("yauzl");
const walk = require("fs-readdir-recursive");
const glob = require("glob");
const minimatch = require("minimatch");
const { exec } = require("child_process");

const BOT = "MyBot";
const TIMELIMIT = 3000;
const SAFEPATH = /[a-zA-Z0-9_.$-]+$/;
const EXTRACTED_DIR = "/extracted";

class ExternalCompiler {
  constructor(args, separate, outFiles, outExt) {
    this.args = args;
    this.separate = separate || false;
    this.outFiles = outFiles || [];
    this.outExt = outExt;
  }

  async compile(botDirectory, globs, errors, timelimit) {
    const pattern = `${globs.join("|")}`;
    const files = walk(
      botDirectory,
      minimatch.filter(pattern, { matchBase: true })
    );
    // To do filter files?

    if (this.separate) {
      files.forEach(file => {
        console.log(file);
      });
      return Promise.resolve();
    } else {
      const command = `cd ${botDirectory} && ${this.args
        .concat(files)
        .join(" ")}`;
      console.log(`Command: ${command}`);

      return new Promise((resolve, reject) => {
        exec(command, (error, stdout, stderr) => {
          if (error) {
            console.error(`exec error: ${error}`);
            return reject(error);
          }
          console.log(`stdout: ${stdout}`);
          console.log(`stderr: ${stderr}`);
          return resolve();
        });
      });
    }
  }
}

const COMP_ARGS = {
  Java: [["javac", "-J-Xmx256m"], ["jar", "cfe", BOT + ".jar", BOT]]
};

const LANGUAGES = [
  {
    language: "Java",
    outFile: BOT + ".jar",
    mainFile: "MyBot.java",
    startCommand: "java -Xmx 256m -jar MyBot.jar",
    cleanupFiles: ["*.class", "*.jar"],
    compilers: [
      {
        sourceFiles: ["*.java"],
        compiler: new ExternalCompiler(COMP_ARGS["Java"][0])
      },
      {
        sourceFiles: ["*.class"],
        compiler: new ExternalCompiler(COMP_ARGS["Java"][1])
      }
    ]
  },
  {
    language: "Javascript",
    outFile: BOT + ".js",
    mainFile: "MyBot.js",
    startCommand: "node MyBot.js",
    cleanupFiles: [],
    compilers: [{ sourceFiles: "*.js" }]
  }
];

async function extract(directory) {
  const pathToEntry = path.join(directory, "entry.zip");
  fs.mkdirSync(EXTRACTED_DIR);

  return new Promise((resolve, reject) => {
    yauzl.open(pathToEntry, (err, zipfile) => {
      if (err) {
        return reject(err);
      }

      zipfile.on("error", err => {
        return reject(err);
      });

      zipfile.on("entry", entry => {
        // console.log(entry);
        // console.log(entry.getLastModDate());
        if (/\/$/.exec(entry)) {
          return;
        }

        // TODO ensure parent dir exists
        // TODO wait with close until writes are done

        zipfile.openReadStream(entry, (err, readStream) => {
          if (err) {
            return reject(err);
          }

          const writeStream = fs
            .createWriteStream(path.join(EXTRACTED_DIR, entry.fileName))
            .on("close", () => {
              console.log(`Stored ${entry.fileName}`);
            });

          readStream.pipe(writeStream);
        });
      });

      zipfile.on("close", () => {
        return resolve();
      });
    });
  });
}

function detectLanguage() {
  console.log(
    `Files in /extracted: ${fs.readdirSync(EXTRACTED_DIR).join(", ")}`
  );
  const detectedLanguages = LANGUAGES.filter(language =>
    fs.existsSync(path.join(EXTRACTED_DIR, language.mainFile))
  );

  if (detectedLanguages.length === 0) {
    const error = `No languages recognised. Please make sure you have one of the following in your zip file: \n
    ${detectedLanguages.map(l => l.language + ": " + l.mainFile).join("\n")}`;
    console.error(error);
    return { error };
  }

  if (detectedLanguages.length > 1) {
    const error = `Found multiple MyBot.* files: \n 
    ${detectedLanguages.map(l => l.language + ": " + l.mainFile).join("\n")}`;

    console.error(error);
    return { error };
  }

  console.log(
    `Found language: ${detectedLanguages[0].language}: ${
      detectedLanguages[0].mainFile
    }`
  );
  return { detectedLanguage: detectedLanguages[0] };
}

function cleanUpCompiledFiles(detectedLanguage, botDirectory) {
  const files = walk(botDirectory);
  const pattern = `(${detectedLanguage.cleanupFiles.join("|")})`;
  console.log(pattern);
  console.log(
    walk(botDirectory, minimatch.filter(pattern, { matchBase: true }))
  );
  console.log(glob.sync(EXTRACTED_DIR + "/**/" + pattern));

  // TODO FINISH
}

async function compile(detectedLanguage, botDirectory, timelimit) {
  cleanUpCompiledFiles(detectedLanguage, botDirectory);

  for (const index in detectedLanguage.compilers) {
    const { compiler, sourceFiles } = detectedLanguage.compilers[index];
    await compiler.compile(botDirectory, sourceFiles);
    // TODO error handling
  }
}

function functionalTest(detectedLanguage, botDirectory) {}

async function compileAnything(directory) {
  await extract(directory);
  const { detectedLanguage, error } = detectLanguage();

  if (!detectedLanguage) {
    throw new Error(error);
  }

  await compile(detectedLanguage, EXTRACTED_DIR);
  await functionalTest(detectedLanguage, EXTRACTED_DIR);
}

module.exports = {
  compileAnything
};
