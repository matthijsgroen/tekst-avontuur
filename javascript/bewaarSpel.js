const { promisify } = require("util");
const schrijfBestand = promisify(require("fs").writeFile);
const leesBestand = promisify(require("fs").readFile);

const basisNaam = name =>
  name
    .split(".")
    .slice(0, -1)
    .join(".");

const bewaarSpel = (bestandsNaam, data) =>
  schrijfBestand(bestandsNaam, JSON.stringify(data), "utf8");

const laadSpel = async bestandsNaam => {
  try {
    return JSON.parse(await leesBestand(bestandsNaam, "utf8"));
  } catch (e) {
    return null;
  }
};

module.exports = { bewaarSpel, basisNaam, laadSpel };
