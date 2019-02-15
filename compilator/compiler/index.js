const fs = require("fs");
const { compileAnything } = require("./compiler");

console.log(`Processing submission ${process.env.SUBMISSION_ID}`);

const submissionId = process.env.SUBMISSION_ID;

if (!submissionId || submissionId <= 0) {
  console.error(
    "Unable to process submission because we do not have a submission id"
  );
  process.exit(1);
}

// Finding entry zip
const submissionDirectoryContents = fs.readdirSync("/submission");

compileAnything("/submission");
