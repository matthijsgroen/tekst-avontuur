const definities = {
  korteKlinker: ["a", "e", "i", "o", "u"],
  langeKlinker: ["aa", "ee", "oo", "uu"],
  tweeKlank: ["ie", "oe", "eu", "ui", "ei", "ij", "ou", "au"],
  letterGroep: ["aai", "ooi", "oei", "eeuw", "ieuw", "uw"],
  rest: ["ng", "nk", "ch", "sch", "schr"]
};

const verwerk = (resultaat, selecteerder, toets, nieuweKlassificatie) => {
  selecteerder(resultaat).forEach(selectie => {
    if (toets(selectie)) {
      resultaat[selectie.index][0] = nieuweKlassificatie;
    }
  });
  return resultaat;
};

const selecteerKlank = (zoekKlank, zoekKlassificatie) => resultaat =>
  resultaat
    .map(([klasse, klank], index) =>
      (zoekKlank === null || klank === zoekKlank) &&
      (zoekKlassificatie === null || zoekKlassificatie === klasse)
        ? { resultaat, index, klasse, klank }
        : null
    )
    .filter(Boolean);

const en = (...args) => selectie => args.every(test => test(selectie));
const of = (...args) => selectie => args.some(test => test(selectie));
const totaalKlanken = test => selectie => test(selectie.resultaat.length);
const laatsteKlank = (offset = 0) => selectie =>
  selectie.index === selectie.resultaat.length - 1 - offset;
const eersteKlank = (offset = 0) => selectie => selectie.index === offset;
const klank = (klank, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset][1] === klank;
const klasse = (test, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  test(selectie.resultaat[selectie.index + offset][0]);

const reset = () => "\u001b[0m";
const tonen = {
  korteKlinker: klinker =>
    process.stdout.write("\u001b[97;48;5;22m " + klinker + " " + reset()),
  langeKlinker: langeKlinker =>
    process.stdout.write(
      "\u001b[30;43m " +
        langeKlinker +
        (langeKlinker.length === 1 ? "-" : "") +
        " " +
        reset()
    ),
  tweeKlank: tweeKlank =>
    process.stdout.write(`\u001b[97;41m ${tweeKlank} ` + reset()),
  letterGroep: letterGroep =>
    process.stdout.write(`\u001b[30;47m ${letterGroep} ` + reset()),
  rest: rest => process.stdout.write(`\u001b[97;44m ${rest} ` + reset()),
  stommeE: klank =>
    process.stdout.write(`\u001b[97;48;5;202m ${klank} ` + reset())
};

const verwerkLangeKlinkers = resultaat => {
  ["u", "a", "o"].forEach(klinker => {
    resultaat = verwerk(
      resultaat,
      selecteerKlank(klinker, "korteKlinker"),
      laatsteKlank(),
      "langeKlinker"
    );
  });
  ["u", "a", "o", "e"].forEach(klinker => {
    resultaat = verwerk(
      resultaat,
      selecteerKlank(klinker, "korteKlinker"),
      en(klasse(a => a === "rest", -1), klasse(a => a !== "rest", 2)),
      "langeKlinker"
    );
  });

  return resultaat;
};

const verwerkStommeE = resultaat => {
  // Korte woorden
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", "korteKlinker"),
    en(totaalKlanken(a => a === 2), laatsteKlank()),
    "stommeE"
  );
  // verklein woord
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", "korteKlinker"),
    en(totaalKlanken(a => a > 3), laatsteKlank(), klank("j", -1)),
    "stommeE"
  );
  // voorvoegsels. be- ge-, ver-
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(totaalKlanken(a => a > 3), eersteKlank(1), klank("b", -1)),
    "stommeE"
  );
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(totaalKlanken(a => a > 3), eersteKlank(1), klank("g", -1)),
    "stommeE"
  );
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(
      totaalKlanken(a => a > 3),
      eersteKlank(1),
      klank("v", -1),
      klank("r", 1)
    ),
    "stommeE"
  );
  // me-
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(totaalKlanken(a => a > 3), eersteKlank(1), klank("m", -1)),
    "stommeE"
  );
  // achtervoegsels
  resultaat = verwerk(
    // -ig
    resultaat,
    selecteerKlank("i", "korteKlinker"),
    en(totaalKlanken(a => a > 3), laatsteKlank(1), klank("g", 1)),
    "stommeE"
  );
  resultaat = verwerk(
    // -lijk
    resultaat,
    selecteerKlank("ij", "tweeKlank"),
    en(
      totaalKlanken(a => a > 3),
      laatsteKlank(1),
      klank("k", 1),
      klank("l", -1)
    ),
    "stommeE"
  );
  // uitgangen -en, -em, -er, -el, -es, -et
  ["n", "m", "r", "l", "s", "t"].forEach(
    eindLetter =>
      (resultaat = verwerk(
        // -en
        resultaat,
        selecteerKlank("e", null),
        en(totaalKlanken(a => a > 3), laatsteKlank(1), klank(eindLetter, 1)),
        "stommeE"
      ))
  );
  return resultaat;
};

const voegKlassificatiesToe = woord => {
  let start = 0;
  let resultaat = [];
  while (start < woord.length) {
    const rest = woord.slice(start);
    let klassificaties = [];
    Object.entries(definities).forEach(([klasse, klanken]) => {
      const klank = klanken
        .filter(klank => rest.startsWith(klank))
        .reduce(
          (resultaat, klank) =>
            klank.length > resultaat.length ? klank : resultaat,
          ""
        );
      if (klank) {
        klassificaties.push({ klasse, klank });
      }
    });
    const klassificatie = klassificaties.reduce(
      (huidigResultaat, { klasse, klank }) =>
        huidigResultaat === null || klank.length > huidigResultaat.klank.length
          ? { klasse, klank }
          : huidigResultaat,
      null
    );
    if (klassificatie === null) {
      const laatsteResultaat = resultaat[resultaat.length - 1];
      resultaat.push(["rest", rest[0]]);
      start += 1;
    } else {
      resultaat.push([klassificatie.klasse, klassificatie.klank]);
      start += klassificatie.klank.length;
    }
  }

  resultaat = verwerkLangeKlinkers(resultaat);
  resultaat = verwerkStommeE(resultaat);

  return resultaat;
};

const toonWoord = woord => {
  const woordMetKlassificaties = voegKlassificatiesToe(woord);

  woordMetKlassificaties.forEach(([klassificatie, woord]) =>
    tonen[klassificatie](woord)
  );
};

module.exports = { toonWoord };
