const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);
const schrijfBestand = promisify(require("fs").writeFile);
const leesAvontuur = require("./leesAvontuur");
const converteerStructuur = require("./converteerStructuur");

const maakMetaTag = eigenschappen =>
  `<meta ${Object.entries(eigenschappen)
    .map(([sleutel, waarde]) => `${sleutel}="${waarde}"`)
    .join(" ")} />`;

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
  const gegevens = avontuur.gegevens;
  const metagegevens = [];
  metagegevens.push(maakMetaTag({ property: "og:type", content: "article" }));
  if (gegevens.Titel) {
    metagegevens.push(
      `<title>Avontuur - ${gegevens.Titel}</title>`,
      maakMetaTag({ property: "og:title", content: gegevens.Titel }),
      maakMetaTag({ name: "twitter:title", content: gegevens.Titel })
    );
  } else {
    metagegevens.push("<title>Avontuur</title>");
  }
  if (gegevens.Omschrijving) {
    metagegevens.push(
      maakMetaTag({ name: "twitter:card", content: "summary" }),
      maakMetaTag({
        property: "og:description",
        content: gegevens.Omschrijving.trim()
      }),
      maakMetaTag({
        name: "description",
        content: gegevens.Omschrijving.trim()
      })
    );
  }
  if (gegevens.Auteur) {
    metagegevens.push(
      maakMetaTag({
        property: "og:article:author:name",
        content: gegevens.Auteur
      })
    );
  }
  if (gegevens.Twitter) {
    metagegevens.push(
      maakMetaTag({ name: "twitter:creator", content: gegevens.Twitter })
    );
  }

  const bovenkant =
    metagegevens.join("\n") + `<style type="text/css">${css}</style>`;
  const jsData = `const avontuur = ${jsonData};`;
  const onderkant = `<script type="text/javascript">${jsData}${js}</script>`;

  const resultaat = htmlBasis
    .replace("<!-- HEAD -->", bovenkant)
    .replace("<!-- BODY -->", onderkant);

  await schrijfBestand(doel, resultaat, "utf8");
};

module.exports = maakHtml;
