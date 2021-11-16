#!/usr/bin/env node

const { trim, reject } = require("lodash");
const readline = require("readline");
const { formateerWoord } = require("../dyslexie");
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const closeApp = () => {
  console.log(`\n${formateerWoord("bedankt")}!`);
  process.exit(0);
};

rl.on("close", closeApp);

const main = async () => {
  while (true) {
    await new Promise((resolve) => {
      rl.question("Geef een woord:", function (word) {
        const cleanedInput = word.trim();
        if (cleanedInput.length > 0) {
          console.log(formateerWoord(cleanedInput));
          resolve("");
        } else {
          closeApp();
        }
      });
    });
  }
};

main();
