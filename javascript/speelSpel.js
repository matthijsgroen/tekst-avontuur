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

const gelijkeToestand = (a, b) => a.every((v, i) => b[i] === v);

const speelSpel = (acties, scherm, spelToestand, keuzeMaker) => {
  let resultaatToestand = spelToestand;
  let log = [];

  while (true) {
    let schermTeksten = [];
    scherm.forEach(teksten => {
      if (!toegestaan(resultaatToestand, teksten.test)) return;
      schermTeksten.push(teksten);

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

    const actie = keuzeMaker(schermTeksten, beschikbareActies);
    if (actie === false) {
      // speler stopt
      return log;
    }
    resultaatToestand = voerActieUit(resultaatToestand, actie.actie);
    log.push(beschikbareActies.indexOf(actie));
  }

  console.log("Geen pogingen meer");
  return log;
};

const toonOmschrijving = teksten =>
  console.log(
    teksten
      .map(tekst => tekst.schermData.filter(t => !t.startsWith("*")).join("\n"))
      .join("\n")
  );

const willekeurigeKeuzeMaker = pogingen => {
  let huidigePoging = 0;
  let vorigeKeuze = "";

  const keuzeMaker = (teksten, acties) => {
    if (huidigePoging > pogingen) return false;
    huidigePoging++;
    const beschikbareActies = acties.filter(
      actie => actie.tekst !== vorigeKeuze
    );

    const keuze = Math.round(Math.random() * (beschikbareActies.length - 1));
    vorigeKeuze = beschikbareActies[keuze].tekst;

    return beschikbareActies[keuze];
  };
  return keuzeMaker;
};

const logAfspeler = log => {
  let positie = 0;
  const keuzeMaker = (teksten, acties) => {
    if (positie >= log.length) return false;
    //toonOmschrijving(teksten);
    const keuze = acties[log[positie]];
    console.log(keuze.tekst);
    positie++;
    return keuze;
  };
  return keuzeMaker;
};

const testSpel = async bestandsnaam => {
  const avontuur = await leesAvontuur(bestandsnaam);

  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );

  let spelToestand = Array(100).fill(0);

  const gameLog = speelSpel(
    acties,
    scherm,
    spelToestand,
    willekeurigeKeuzeMaker(8000)
  );
  //console.log(gameLog);
  //speelSpel(acties, scherm, spelToestand, logAfspeler(gameLog));
};

module.exports = {
  testSpel
};
