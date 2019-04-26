#!/usr/bin/env node
const spelLus = require("./spelLus");
const maakHtml = require("./maakHtml");

const commandos = process.argv.slice(2);
const eerste = commandos[0];

if (eerste === "--version" || eerste === "--versie" || eerste === "-V") {
  console.log("Avontuur, versie 1.0");
  process.exit(0);
} else if (eerste === "--help") {
  [
    "Avontuur, versie 1.0",
    "",
    "Gebruik:",
    "  - spelen: avontuur.js bestand.avontuur",

    "  - html versie maken: avontuur.js html bronbestand.avontuur [doelbestand.html]",
    "",
    "Foutjes, suggesties, vragen?",
    "- Github: https://github.com/matthijsgroen/tekst-avontuur",
    "- E-mail: matthijs.groen@gmail.com",
    "- Twitter: @matthijsgroen"
  ].forEach(regel => console.log(regel));
  process.exit(0);
} else if (eerste === "html") {
  const basisNaam = name =>
    name
      .split(".")
      .slice(0, -1)
      .join(".");

  const bron = commandos[1];
  const doel = commandos[2] || `${basisNaam(bron)}.html`;
  maakHtml(bron, doel);
} else {
  spelLus(eerste);
}
