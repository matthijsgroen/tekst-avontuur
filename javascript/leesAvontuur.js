const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);

const leesAvontuur = async bestandsNaam => {
  const inhoud = await leesBestand(bestandsNaam, "utf8");
  const regels = inhoud.replace(/\r/g, "").split("\n");
  let actieModus = false;
  const actieData = [];
  const schermData = [];

  regels
    .map((regel, nr) => ({ regel, nr }))
    .filter(regel => regel.regel.startsWith('"'))
    .forEach(regel => {
      try {
        const elementen = JSON.parse(`[${regel.regel}]`);

        if (elementen.length === 1 && elementen[0] === "END") {
          actieModus = true;
        } else {
          actieModus
            ? actieData.push(...elementen)
            : schermData.push(...elementen);
        }
      } catch (e) {
        throw new Error(`Fout bij het verwerken van regel ${regel.nr + 1}`);
      }
    });

  return { actieData, schermData };
};

module.exports = leesAvontuur;
