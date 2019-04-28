const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);
const schrijfBestand = promisify(require("fs").writeFile);
const leesAvontuur = require("./leesAvontuur");
const converteerStructuur = require("./converteerStructuur");

const maakHtml = async (bron, doel) => {
  console.log(`${bron} -> ${doel}`);
  const avontuur = await leesAvontuur(bron);
  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );
  const jsonData = JSON.stringify({ scherm, acties });

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
