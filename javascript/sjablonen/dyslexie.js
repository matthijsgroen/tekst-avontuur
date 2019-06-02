const definities = {
  korteKlinker: ["a", "e", "i", "o", "u", "è"],
  langeKlinker: ["aa", "ee", "oo", "uu", "éé"],
  tweeKlank: ["ie", "oe", "eu", "ui", "ei", "ij", "ou", "au"],
  letterGroep: ["aai", "ooi", "oei", "eeuw", "ieuw", "uw", "eau"],
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
      (zoekKlank === null || klank.toLowerCase() === zoekKlank) &&
      (zoekKlassificatie === null || zoekKlassificatie === klasse)
        ? { resultaat, index, klasse, klank }
        : null
    )
    .filter(Boolean);

const en = (...args) => selectie => args.every(test => test(selectie));
const of = (...args) => selectie => args.some(test => test(selectie));
const niet = test => selectie => !test(selectie);
const totaalKlanken = test => selectie => test(selectie.resultaat.length);
const laatsteKlank = (offset = 0) => selectie =>
  selectie.index === selectie.resultaat.length - 1 - offset;
const eersteKlank = (offset = 0) => selectie => selectie.index === offset;
const klank = (klank, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  selectie.resultaat[selectie.index + offset][1].toLowerCase() === klank;
const klankLengte = (lengte, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  selectie.resultaat[selectie.index + offset][1].length === lengte;
const klasse = (test, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  test(selectie.resultaat[selectie.index + offset][0]);
const totaalKlasse = (klasseTest, test) => selectie =>
  test(selectie.resultaat.reduce((a, e) => a + (klasseTest(e[0]) ? 1 : 0), 0));
const klankIndex = test => selectie => test(selectie.index);

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
      en(
        niet(klasse(a => a !== "rest", -1)),
        klankLengte(1, 1),
        klasse(a => a !== "rest", 2)
      ),
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
    laatsteKlank(),
    "stommeE"
  );
  // verklein woord
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", "korteKlinker"),
    en(totaalKlanken(a => a > 3), laatsteKlank(), klank("j", -1)),
    "stommeE"
  );
  // uitgangen -en, -em, -er, -el, -es, -et
  ["n", "m", "r", "l", "s", "t"].forEach(
    eindLetter =>
      (resultaat = verwerk(
        // -en
        resultaat,
        selecteerKlank("e", null),
        en(
          totaalKlasse(k => k !== "rest" && k !== "stommeE", a => a > 1),
          klankIndex(e => e > 1),
          klank(eindLetter, 1),
          niet(klank(eindLetter, 2))
        ),
        "stommeE"
      ))
  );
  // voorvoegsels. be- ge-, ver- te-
  const isVoorvoegsel = (offset = 0) => selectie =>
    selectie.resultaat[selectie.index + 1 + offset] &&
    selectie.resultaat[selectie.index + 1 + offset][0] === "rest" &&
    selectie.resultaat[selectie.index + 2 + offset] &&
    selectie.resultaat[selectie.index + 2 + offset][0] !== "rest" &&
    totaalKlasse(k => k !== "rest" && k !== "stommeE", a => a > 1)(selectie);

  ["d", "b", "g", "t"].forEach(
    beginLetter =>
      (resultaat = verwerk(
        resultaat,
        selecteerKlank("e", null),
        en(isVoorvoegsel(), klank(beginLetter, -1)),
        "stommeE"
      ))
  );

  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(isVoorvoegsel(1), eersteKlank(1), klank("v", -1), klank("r", 1)),
    "stommeE"
  );
  // me-
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(
      isVoorvoegsel(),
      klank("m", -1),
      niet(en(klank("d", 1), of(klank("i", 2), klank("e", 2))))
    ),
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
  // lidwoorden, de het een
  resultaat = verwerk(
    // het
    resultaat,
    selecteerKlank("e", "korteKlinker"),
    en(
      totaalKlanken(a => a === 3),
      laatsteKlank(1),
      klank("t", 1),
      klank("h", -1)
    ),
    "stommeE"
  );
  resultaat = verwerk(
    // een
    resultaat,
    selecteerKlank("ee", "langeKlinker"),
    en(totaalKlanken(a => a === 2), laatsteKlank(1), klank("n", 1)),
    "stommeE"
  );

  return resultaat;
};

const voegWoordKlassificatiesToe = woord => {
  let start = 0;
  let resultaat = [];
  while (start < woord.length) {
    const rest = woord.slice(start);
    let klassificaties = [];
    Object.entries(definities).forEach(([klasse, klanken]) => {
      const klank = klanken
        .filter(klank => rest.toLowerCase().startsWith(klank))
        .reduce(
          (resultaat, klank) =>
            klank.length > resultaat.length ? klank : resultaat,
          ""
        );
      if (klank) {
        klassificaties.push({ klasse, klank: rest.slice(0, klank.length) });
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

const voegKlassificatiesToe = zin => {
  let resultaat = [];
  let huidigWoord = "";
  //return [["anders", zin]];
  for (const char of zin) {
    if (/\w|[éè]/.test(char)) {
      huidigWoord += char;
    } else {
      if (huidigWoord.length > 0) {
        resultaat = resultaat.concat(voegWoordKlassificatiesToe(huidigWoord));
        huidigWoord = "";
      }
      resultaat.push(["anders", char]);
    }
  }
  if (huidigWoord.length > 0) {
    resultaat = resultaat.concat(voegWoordKlassificatiesToe(huidigWoord));
    huidigWoord = "";
  }
  return resultaat;
};
