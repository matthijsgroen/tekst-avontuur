const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);

const leesAvontuur = async bestandsNaam => {
  const inhoud = await leesBestand(bestandsNaam, "utf8");
  const regels = inhoud.replace(/\r/g, "").split("\n");
  let actieModus = false;
  const actieData = [];
  const schermData = [];
  const gegevens = {};

  let inMeerRegeligeData = false;
  regels.forEach(regel => {
    const enkeleRegelData = regel.match(
      /^'\s*@(?<veld>\w+):\s*(?<waarde>[^\s]+.*)$/
    );
    const meerdereRegelData = regel.match(/^'\s*@(?<veld>\w+):\s*$/);
    if (meerdereRegelData) {
      const veld = meerdereRegelData.groups.veld;
      gegevens[veld] = "";
      inMeerRegeligeData = veld;
      return;
    }

    if (enkeleRegelData) {
      gegevens[enkeleRegelData.groups.veld] = enkeleRegelData.groups.waarde;
      inMeerRegeligeData = false;
      return;
    }

    if (inMeerRegeligeData !== false && regel.startsWith("'")) {
      gegevens[inMeerRegeligeData] += `\n${regel.slice(1).trim()}`;
      return;
    }
    inMeerRegeligeData = false;
  });

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

  return { actieData, schermData, gegevens };
};

module.exports = leesAvontuur;
