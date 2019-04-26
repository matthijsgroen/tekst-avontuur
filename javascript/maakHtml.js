const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);
const schrijfBestand = promisify(require("fs").writeFile);
const leesAvontuur = require("./leesAvontuur");

const maakHtml = async (bron, doel) => {
  console.log(`${bron} -> ${doel}`);
  const { actieData, schermData } = await leesAvontuur(bron);

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

  const htmlBasis = await leesBestand(
    `${__dirname}/sjablonen/index.html`,
    "utf8"
  );
  const css = await leesBestand(`${__dirname}/sjablonen/avontuur.css`, "utf8");
  const js = await leesBestand(`${__dirname}/sjablonen/avontuur.js`, "utf8");
  const bovenkant = `<style type="text/css">${css}</style>`;
  const jsData = `const avontuur = ${jsonData};`;
  const onderkant = `<script type="text/javascript">${jsData}${js}</script>`;

  const resultaat = htmlBasis
    .replace("<!-- HEAD -->", bovenkant)
    .replace("<!-- BODY -->", onderkant);

  await schrijfBestand(doel, resultaat, "utf8");
};

module.exports = maakHtml;
