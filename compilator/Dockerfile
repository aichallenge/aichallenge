FROM ubuntu:18.04

# ARG compile
# ARG game
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  python \
  nodejs \
  npm \
  openjdk-8-jdk

ARG SUBMISSION_ID=-1

ADD compiler /compiler

CMD [ "node", "/compiler/index.js" ]