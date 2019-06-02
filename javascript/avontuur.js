#!/usr/bin/env node
const spelLus = require("./spelLus");
const maakHtml = require("./maakHtml");
const statistieken = require("./statistieken");
const pakket = require("./package.json");
const { genereerCode } = require("./maakCode");
const { toonZin } = require("./dyslexie");

const commandos = process.argv
  .slice(2)
  .filter(parameter => !parameter.startsWith("--"));

const negatieveUitkomst = ["geen", "no"];

const vlaggen = process.argv
  .slice(2)
  .filter(parameter => parameter.startsWith("--"))
  .map(vlag => vlag.slice(2).split("="))
  .map(vlag =>
    negatieveUitkomst.reduce(
      (vlag, negatief) =>
        vlag[0].startsWith(`${negatief}-`)
          ? [vlag[0].slice(negatief.length + 1), false]
          : vlag,
      vlag
    )
  )
  .reduce(
    (resultaat, vlag) => ({
      ...resultaat,
      [vlag[0]]: vlag[1] === undefined ? true : vlag[1]
    }),
    {}
  );

const standaarConfiguratie = {
  thema: "dos",
  analytics: true
};

const eerste = commandos[0];
if (vlaggen.versie || vlaggen.version || eerste === "-V") {
  console.log(`Avontuur, versie ${pakket.version}`);
} else if (vlaggen.help || !eerste || eerste === "help") {
  [
    `Avontuur, versie ${pakket.version}`,
    "",
    "Gebruik:",
    "  - spelen: avontuur.js bestand.avontuur [--herstarten]",
    "  - html versie maken: avontuur.js html bronbestand.avontuur [doelbestand.html]",
    "    [--thema=dos|boek]",
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
} else if (eerste === "code") {
  const encodeDecode = async bron => {
    const code = await genereerCode(bron);
    console.log("Code is:", code);
  };
  const bron = commandos[1];
  encodeDecode(bron);
} else if (eerste === "test") {
  [
    "teruggevonden",
    "boerderij",
    "vertellen",
    "veren",
    "beren",
    "open",
    "gedoe",
    "getver",
    "geluk",
    "eten.",
    "medicijnen",
    "medebewoner",
    "medaille",
    "meteen",
    "meten",
    "messen",
    "snel",
    "Avontuur"
  ].forEach(woord => {
    toonZin(woord);
    console.log("\n");
  });
} else if (eerste === "html") {
  const basisNaam = name =>
    name
      .split(".")
      .slice(0, -1)
      .join(".");

  const bron = commandos[1];
  const doel = commandos[2] || `${basisNaam(bron)}.html`;
  maakHtml(
    bron,
    doel,
    basisNaam(bron),
    {
      ...standaarConfiguratie,
      ...vlaggen
    },
    vlaggen
  );
} else {
  const herstarten = vlaggen.restart || vlaggen.herstart || vlaggen.herstarten;
  spelLus(eerste, { herstarten });
}
