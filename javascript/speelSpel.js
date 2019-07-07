const leesAvontuur = require("./leesAvontuur");
const converteerStructuur = require("./converteerStructuur");

const toets = (spelToestand, plek, bewerking, waarde) =>
  (bewerking === "=" && spelToestand[plek] === waarde) ||
  (bewerking === "!" && spelToestand[plek] !== waarde) ||
  (bewerking === ">" && spelToestand[plek] > waarde) ||
  (bewerking === "<" && spelToestand[plek] < waarde);

const toegestaan = (spelToestand, beweringen) =>
  beweringen.every(bewering => {
    const [, plek, bewerking, waarde] = bewering.match(/(\d+)([=!<>])(\d+)/);
    return toets(
      spelToestand,
      parseInt(plek, 10),
      bewerking,
      parseInt(waarde, 10)
    );
  });

const muteer = (spelToestand, plek, bewerking, waarde) => {
  if (bewerking === "=") {
    spelToestand[plek] = waarde;
  }
  if (bewerking === "+") {
    spelToestand[plek] = spelToestand[plek] + waarde;
  }
  if (bewerking === "-") {
    spelToestand[plek] = spelToestand[plek] - waarde;
  }
  if (bewerking === "r") {
    spelToestand[plek] = Math.min(
      waarde,
      Math.max(1, Math.ceil(Math.random() * waarde))
    );
  }
  return spelToestand;
};

const voerActieUit = (spelToestand, instructies) => {
  const resultaat = [].concat(spelToestand);
  instructies.forEach(instructie => {
    const [, plek, bewerking, waarde] = instructie.match(/(\d+)([=+-r])(\d+)/);
    muteer(resultaat, parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  });
  return resultaat;
};

const speelSpel = (acties, scherm, spelToestand, pogingen) => {
  let resultaatToestand = spelToestand;

  let proberen = pogingen;
  let log = [];

  while (proberen > 0) {
    scherm.forEach(teksten => {
      if (!toegestaan(resultaatToestand, teksten.test)) return;

      if (teksten.actie) {
        resultaatToestand = voerActieUit(resultaatToestand, teksten.actie);
      }
    });
    const beschikbareActies = acties.filter(actie =>
      toegestaan(resultaatToestand, actie.test)
    );

    if (beschikbareActies.length === 0) {
      console.log("Uitgespeeld!?");
      return log;
    }

    const keuze = Math.round(Math.random() * (beschikbareActies.length - 1));
    console.log(proberen, beschikbareActies[keuze].tekst);
    resultaatToestand = voerActieUit(
      resultaatToestand,
      beschikbareActies[keuze].actie
    );

    proberen--;
    log.push(keuze);
  }

  console.log("Geen pogingen meer");
  return log;
};

const testSpel = async bestandsnaam => {
  const avontuur = await leesAvontuur(bestandsnaam);

  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );

  let spelToestand = Array(100).fill(0);

  const gameLog = speelSpel(acties, scherm, spelToestand, 1000);
};

module.exports = {
  testSpel
};
