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

const speelSpel = async (acties, scherm, startToestand, keuzeMaker) => {
  let spelToestand = [].concat(startToestand);
  let log = [];

  while (true) {
    let schermTeksten = [];
    scherm.forEach(teksten => {
      if (!toegestaan(spelToestand, teksten.test)) return;
      schermTeksten.push(teksten);

      if (teksten.actie) {
        spelToestand = voerActieUit(spelToestand, teksten.actie);
      }
    });

    const beschikbareActies = acties.filter(actie =>
      toegestaan(spelToestand, actie.test)
    );

    const [actie, logItem] = await keuzeMaker(
      schermTeksten,
      beschikbareActies,
      spelToestand
    );
    if (actie === false) {
      // speler stopt
      return log;
    }
    log.push(logItem);
    spelToestand = voerActieUit(spelToestand, actie.actie);
  }
};

const toonOmschrijving = teksten =>
  console.log(
    teksten
      .map(tekst => tekst.schermData.filter(t => !t.startsWith("*")).join("\n"))
      .join("\n")
  );

const vooruitKijker = (eersteKeuze, pogingen) => {
  let huidigePoging = 0;

  const keuzeMaker = async (teksten, keuzes, spelToestand) => {
    if (huidigePoging >= pogingen) return [false, false];
    if (keuzes.length === 0) return [false, false];
    huidigePoging++;

    const gamestateKey = spelToestand.join(",");
    const keuze = Math.round(Math.random() * (keuzes.length - 1));

    const actie = huidigePoging === 1 ? eersteKeuze : keuzes[keuze];
    return [actie, gamestateKey];
  };
  return keuzeMaker;
};

const VOORUIT_KIJKEN = 6;
const puntenSchaal = [0, 16, 8, 4, 2, 1];

const willekeurigeKeuzeMaker = (acties, scherm, pogingen) => {
  let huidigePoging = 0;

  const gamestateKeuzes = {};
  let laatsteActie = "";

  const keuzeMaker = async (teksten, keuzes, spelToestand) => {
    if (huidigePoging >= pogingen) return [false, false];
    if (keuzes.length === 0) return [false, false];
    huidigePoging++;

    const gamestateKey = spelToestand.join(",");
    const gedaneKeuzes = gamestateKeuzes[gamestateKey] || {};
    gamestateKeuzes[gamestateKey] = gedaneKeuzes;

    let besteKeuzes = [];
    let maxScore = 0;

    for (let optie of keuzes) {
      const optieResultaten = await speelSpel(
        acties,
        scherm,
        spelToestand,
        vooruitKijker(optie, VOORUIT_KIJKEN)
      );

      const score =
        optieResultaten.reduce((som, element, index, list) => {
          return (
            som +
            (gamestateKeuzes[element] === undefined &&
            list.indexOf(element) === index
              ? 1
              : 0) *
              puntenSchaal[index]
          );
        }, 0) +
        (VOORUIT_KIJKEN - optieResultaten.length) * 10;

      if (score === maxScore) {
        besteKeuzes.push(optie);
      }
      if (score > maxScore) {
        maxScore = score;
        besteKeuzes = [optie];
      }
    }

    const gewogenActies = besteKeuzes.map(actie => ({
      actie,
      score:
        1 /
        ((gedaneKeuzes[actie.tekst] || 1) +
          (actie.tekst === laatsteActie ? 100 : 0))
    }));
    const totaalBereik = gewogenActies.reduce((a, e) => a + e.score, 0);
    let keuze = Math.random() * totaalBereik;
    const actie = gewogenActies.find(actie => {
      if (actie.score > keuze) return actie;
      keuze -= actie.score;
    }).actie;

    const kerenGekozen = gedaneKeuzes[actie.tekst] || 1;
    gamestateKeuzes[gamestateKey] = {
      ...gedaneKeuzes,
      [actie.tekst]: kerenGekozen + 1
    };
    laatsteActie = actie.tekst;

    return [actie, keuzes.indexOf(actie)];
  };
  return keuzeMaker;
};

const logAfspeler = log => {
  let positie = 0;
  const keuzeMaker = async (teksten, keuzes) => {
    if (keuzes.length === 0) {
      toonOmschrijving(teksten);
      return [false, false];
    }
    if (positie >= log.length) return [false, false];

    const keuze = log[positie];
    const actie = keuzes[keuze];
    if (log.length - positie < 30) {
      console.log("-", actie.tekst);
    }
    positie++;
    return [actie, keuze];
  };
  return keuzeMaker;
};

const testSpel = async bestandsnaam => {
  const avontuur = await leesAvontuur(bestandsnaam);
  const { actieData: acties, schermData: scherm } = converteerStructuur(
    avontuur
  );

  const spelToestand = Array(100).fill(0);

  const gameLog = await speelSpel(
    acties,
    scherm,
    spelToestand,
    willekeurigeKeuzeMaker(acties, scherm, 10000)
  );
  console.log("replay... ", gameLog.length);
  //console.log(gameLog);
  await speelSpel(acties, scherm, spelToestand, logAfspeler(gameLog));
};

module.exports = {
  testSpel
};
