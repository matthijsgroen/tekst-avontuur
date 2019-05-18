const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);
const schrijfBestand = promisify(require("fs").writeFile);
const leesAvontuur = require("./leesAvontuur");
const converteerStructuur = require("./converteerStructuur");
const themas = require("./themas");

const maakMetaTag = eigenschappen =>
  `<meta ${Object.entries(eigenschappen)
    .map(([sleutel, waarde]) => `${sleutel}="${waarde}"`)
    .join(" ")} />`;

const verwerkAanpassingen = (aanpassing, resultaat) =>
  aanpassing ? `${aanpassing}\n${resultaat}` : resultaat;

const maakHtml = async (
  bron,
  doel,
  basisNaam,
  { thema, analytics, ...vlaggen }
) => {
  console.log(`${bron} -> ${doel}`);
  const themaData = themas[thema];
  if (!themaData) {
    console.log(`Thema ${thema} niet gevonden!`);
    process.exit(1);
  }
  const themaOpties = Object.entries(vlaggen)
    .filter(([vlag, waarde]) => vlag.startsWith("thema."))
    .reduce(
      (opties, [vlag, waarde]) => ({
        ...opties,
        [vlag.slice(6)]: waarde
      }),
      {}
    );

  const themaAanpassingen = themaData.opties(themaOpties);

  const avontuur = await leesAvontuur(bron);
  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );
  const jsonData = JSON.stringify({ scherm, acties });

  const htmlBasis = await leesBestand(`${__dirname}/${themaData.html}`, "utf8");
  const css = verwerkAanpassingen(
    themaAanpassingen.css,
    await leesBestand(`${__dirname}/${themaData.css}`, "utf8")
  );
  const code = await leesBestand(`${__dirname}/sjablonen/avontuur.js`, "utf8");
  const themaJs = verwerkAanpassingen(
    themaAanpassingen.javascript,
    themaData.javascript
      ? await leesBestand(`${__dirname}/${themaData.javascript}`)
      : ""
  );

  const js = Object.entries(themaData.haken || {}).reduce(
    (code, [haak, functieNaam]) =>
      code.replace(
        new RegExp(`//\\s+--\\s+template:${haak}`, "g"),
        `await ${functieNaam}();`
      ),
    code
  );

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
  const jsData = [
    `const bewaarSleutel = "${basisNaam}";`,
    `const avontuur = ${jsonData};`,
    themaJs
  ].join("\n");

  const stats =
    gegevens["StatHat.Gebruiker"] && gegevens["StatHat.Teller"] && analytics
      ? `<img src="https://api.stathat.com/c?ukey=${
          gegevens["StatHat.Gebruiker"]
        }&key=${
          gegevens["StatHat.Teller"]
        }&count=1" style="display:none;" width="1" height="1">`
      : "";

  const onderkant = `<script type="text/javascript">${jsData}${js}</script>${stats}`;

  const resultaat = htmlBasis
    .replace("<!-- HEAD -->", bovenkant)
    .replace("<!-- BODY -->", onderkant)
    .replace("<!-- TITEL -->", gegevens.Titel)
    .replace("<!-- AUTEUR -->", gegevens.Auteur);

  await schrijfBestand(doel, resultaat, "utf8");
};

module.exports = maakHtml;
