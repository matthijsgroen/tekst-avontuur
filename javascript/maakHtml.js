const postcss = require("postcss");
const autoprefixer = require("autoprefixer");

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

const maakHtml = async (bron, doel, basisNaam, configuratie, vlaggen) => {
  const avontuur = await leesAvontuur(bron);
  const thema = vlaggen.thema || avontuur.gegevens.thema || configuratie.thema;
  console.log(`${bron} -> ${doel} - Thema: ${thema}`);
  const themaData = themas[thema];

  if (!themaData) {
    console.log(`Thema ${thema} niet gevonden!`);
    process.exit(1);
  }
  const instellingen = { ...avontuur.gegevens, ...vlaggen };
  const themaOpties = Object.entries(instellingen)
    .filter(([vlag, waarde]) => vlag.startsWith("thema."))
    .reduce(
      (opties, [vlag, waarde]) => ({
        ...opties,
        [vlag.slice(6)]: waarde
      }),
      {}
    );

  const themaAanpassingen = themaData.opties(themaOpties);

  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );
  const jsonData = JSON.stringify({ scherm, acties });

  const htmlBasis = await leesBestand(`${__dirname}/${themaData.html}`, "utf8");
  const cssPath = `${__dirname}/${themaData.css}`;
  const css = verwerkAanpassingen(
    themaAanpassingen.css,
    await leesBestand(cssPath, "utf8")
  );
  const bestanden = ["dyslexie.js", "avontuur.js"];
  const codeResultaat = [];
  for (const bestand of bestanden) {
    const inhoud = await leesBestand(
      `${__dirname}/sjablonen/${bestand}`,
      "utf8"
    );
    codeResultaat.push(inhoud);
  }
  const code = codeResultaat.join("");
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
  if (gegevens.titel) {
    metagegevens.push(
      `<title>Avontuur - ${gegevens.titel}</title>`,
      maakMetaTag({ property: "og:title", content: gegevens.titel }),
      maakMetaTag({ name: "twitter:title", content: gegevens.titel })
    );
  } else {
    metagegevens.push("<title>Avontuur</title>");
  }
  if (gegevens.omschrijving) {
    metagegevens.push(
      maakMetaTag({ name: "twitter:card", content: "summary" }),
      maakMetaTag({
        property: "og:description",
        content: gegevens.omschrijving.trim()
      }),
      maakMetaTag({
        name: "description",
        content: gegevens.omschrijving.trim()
      })
    );
  }
  if (gegevens.auteur) {
    metagegevens.push(
      maakMetaTag({
        property: "og:article:author:name",
        content: gegevens.auteur
      })
    );
  }
  if (gegevens.twitter) {
    metagegevens.push(
      maakMetaTag({ name: "twitter:creator", content: gegevens.twitter })
    );
  }

  const processor = postcss([autoprefixer]);
  const processedCss = await processor.process(css, { from: cssPath });

  const bovenkant =
    metagegevens.join("\n") +
    `<style type="text/css">${processedCss.css}</style>`;
  const jsData = [
    `const bewaarSleutel = "${basisNaam}";`,
    `const avontuur = ${jsonData};`,
    `const gegevens = ${JSON.stringify(gegevens)};`,
    themaJs
  ].join("\n");

  const stats =
    gegevens["stathat.gebruiker"] &&
    gegevens["stathat.teller"] &&
    configuratie.analytics
      ? `<img src="https://api.stathat.com/c?ukey=${
          gegevens["stathat.gebruiker"]
        }&key=${
          gegevens["stathat.teller"]
        }&count=1" style="display:none;" width="1" height="1">`
      : "";

  const onderkant = `<script type="text/javascript">${jsData}${js}</script>${stats}`;

  const resultaat = htmlBasis
    .replace("<!-- HEAD -->", bovenkant)
    .replace("<!-- BODY -->", onderkant)
    .replace(new RegExp("<!-- TITEL -->", "g"), gegevens.titel)
    .replace(new RegExp("<!-- AUTEUR -->", "g"), gegevens.auteur);

  await schrijfBestand(doel, resultaat, "utf8");
};

module.exports = maakHtml;
