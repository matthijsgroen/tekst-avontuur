const { promisify } = require("util");
const leesBestand = promisify(require("fs").readFile);
const schrijfBestand = promisify(require("fs").writeFile);
const leesAvontuur = require("./leesAvontuur");
const converteerStructuur = require("./converteerStructuur");
const { print, color } = require("./basic");

const toonVeld = (veld, waarde) => {
  color(15);
  print(`${veld}: `);
  color(7);
  print(`${waarde}\n`);
};

const statistieken = async bron => {
  const avontuur = await leesAvontuur(bron);
  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );
  const aantalWoordenTekst = scherm.reduce(
    (totaal, element) =>
      totaal +
      element.schermData
        .filter(item => !item.startsWith("*"))
        .reduce((woorden, zin) => woorden + zin.split(/s+/).length, 0),
    0
  );
  const aantalActies = acties.length;
  Object.entries(avontuur.gegevens).forEach(([veld, waarde]) =>
    toonVeld(veld, waarde)
  );

  toonVeld("Aantal woorden", aantalWoordenTekst);
  toonVeld("Aantal acties", aantalActies);
};

module.exports = statistieken;
