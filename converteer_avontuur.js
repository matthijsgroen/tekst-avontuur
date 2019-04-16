#!/usr/bin/env node
const { promisify } = require("util");
const readFile = promisify(require("fs").readFile);
const writeFile = promisify(require("fs").writeFile);

const basisNaam = name =>
  name
    .split(".")
    .slice(0, -1)
    .join(".");

const leesAvontuur = async bestandsNaam => {
  const inhoud = await readFile(bestandsNaam, "utf8");
  const regels = inhoud.replace(/\r/g, "").split("\n");
  let actieModus = false;
  const actieData = [];
  const schermData = [];

  regels
    .filter(regel => regel.startsWith('"'))
    .forEach(regel => {
      const elementen = JSON.parse(`[${regel}]`);

      if (elementen.length === 1 && elementen[0] === "END") {
        actieModus = true;
      } else {
        actieModus
          ? actieData.push(...elementen)
          : schermData.push(...elementen);
      }
    });

  return { actieData, schermData };
};

const main = async bronBestandsnaam => {
  const { actieData, schermData } = await leesAvontuur(bronBestandsnaam);
  const basicFileStructure = schermData
    .concat("END")
    .concat(actieData)
    .map(e => `"${e}"`)
    .join("\r\n");
  await writeFile("AVONTUUR.DAT", basicFileStructure, "latin1");
  const nieuweSchermData = [];
  let actiefScherm = { schermData: [] };
  schermData.forEach(element => {
    if (!actiefScherm.hasOwnProperty("test")) {
      actiefScherm.test = element.split(";");
    } else {
      if (element.startsWith("&")) {
        if (element.length > 1) {
          actiefScherm.actie = element.slice(1).split(";");
        }
        nieuweSchermData.push(actiefScherm);
        actiefScherm = { schermData: [] };
      } else {
        actiefScherm.schermData.push(element);
      }
    }
  });
  const nieuweActieData = [];
  let actieveActie = {};

  actieData.forEach((element, i) => {
    if (i % 3 === 0) {
      actieveActie.test = element.split(";");
    } else if (i % 3 === 1) {
      actieveActie.tekst = element;
    } else {
      actieveActie.actie = element.split(";");
      nieuweActieData.push(actieveActie);
      actieveActie = {};
    }
  });

  const jsonData = JSON.stringify({
    scherm: nieuweSchermData,
    acties: nieuweActieData
  });

  await writeFile(`${basisNaam(bronBestandsnaam)}.json`, jsonData, "utf8");
  const jsData = `const avontuur = ${jsonData};`;
  await writeFile(`docs/${basisNaam(bronBestandsnaam)}.js`, jsData, "utf8");
};

main(process.argv[2]);
