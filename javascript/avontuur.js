#!/usr/bin/env node
const spelLus = require("./spelLus");
const maakHtml = require("./maakHtml");
const statistieken = require("./statistieken");
const pakket = require("./package.json");

const commandos = process.argv
  .slice(2)
  .filter(parameter => !parameter.startsWith("--"));

const vlaggen = process.argv
  .slice(2)
  .filter(parameter => parameter.startsWith("--"))
  .map(vlag => vlag.slice(2).split("="))
  .reduce(
    (resultaat, vlag) => ({ ...resultaat, [vlag[0]]: vlag[1] || true }),
    {}
  );

const eerste = commandos[0];
if (vlaggen.versie || vlaggen.version || eerste === "-V") {
  console.log(`Avontuur, versie ${pakket.version}`);
} else if (vlaggen.help || !eerste) {
  [
    `Avontuur, versie ${pakket.version}`,
    "",
    "Gebruik:",
    "  - spelen: avontuur.js bestand.avontuur [--herstarten]",
    "  - html versie maken: avontuur.js html bronbestand.avontuur [doelbestand.html]",
    "  - informatie: avontuur.js info bronbestand.avontuur",
    "",
    "Foutjes, suggesties, vragen?",
    "- Github: https://github.com/matthijsgroen/tekst-avontuur",
    "- E-mail: matthijs.groen@gmail.com",
    "- Twitter: @matthijsgroen"
  ].forEach(regel => console.log(regel));
  process.exit(0);
} else if (eerste === "info") {
  const bron = commandos[1];
  statistieken(bron);
} else if (eerste === "html") {
  const standaardThema = "boek";
  const basisNaam = name =>
    name
      .split(".")
      .slice(0, -1)
      .join(".");

  const bron = commandos[1];
  const doel = commandos[2] || `${basisNaam(bron)}.html`;
  maakHtml(bron, doel, basisNaam(bron), {
    thema: vlaggen["thema"] || standaardThema
  });
} else {
  const herstarten = vlaggen.restart || vlaggen.herstart || vlaggen.herstarten;
  spelLus(eerste, herstarten);
}
