const definities = {
  korteKlinker: ["a", "e", "i", "o", "u", "è"],
  langeKlinker: [/^aa+/, /^ee+/, /^oo+/, /^uu+/, /^é+/],
  tweeKlank: ["ie", "oe", "eu", "ui", "ei", "ij", "ou", "au"],
  letterGroep1: ["aai", "ooi", "oei", "eau"],
  letterGroep2: ["eeuw", "ieuw", "uw"],
  speciaal: ["th", "c", "y"],
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
const klankLengte = (lengteTest, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  lengteTest(selectie.resultaat[selectie.index + offset][1].length);
const klasse = (test, offset = 0) => selectie =>
  selectie.resultaat[selectie.index + offset] &&
  test(selectie.resultaat[selectie.index + offset][0]);
const totaalKlasse = (klasseTest, test) => selectie =>
  test(selectie.resultaat.reduce((a, e) => a + (klasseTest(e[0]) ? 1 : 0), 0));
const klankIndex = test => selectie =>
  test(selectie.index, selectie.resultaat.length);
const eindKlank = (klank, offset = 0) => selectie =>
  selectie.resultaat[selectie.resultaat.length - 1 - offset][1] === klank;
const isWoord = test => selectie =>
  test(selectie.resultaat.reduce((w, e) => w + e[1], ""));

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
        niet(
          klasse(
            a => a !== "rest" && a !== "korteKlinker" && a !== "speciaal",
            -1
          )
        ),
        klankLengte(l => l === 1, 1),
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
  ["j", "t"].forEach(
    eindLetter =>
      (resultaat = verwerk(
        resultaat,
        selecteerKlank("e", "korteKlinker"),
        en(totaalKlanken(a => a > 3), laatsteKlank(), klank(eindLetter, -1)),
        "stommeE"
      ))
  );
  // uitgangen -en, -em, -er, -el, -es
  ["n", "m", "r", "l", "s"].forEach(
    eindLetter =>
      (resultaat = verwerk(
        // -en
        resultaat,
        selecteerKlank("e", null),
        en(
          totaalKlasse(k => k !== "rest" && k !== "stommeE", a => a > 1),
          klankIndex(e => e > 1),
          klank(eindLetter, 1),
          niet(klank(eindLetter, 2)),
          of(
            niet(klasse(e => e !== "stommeE" || e !== "korteKlinker", 2)),
            klasse(
              e =>
                e === "langeKlinker" ||
                e === "korteKlinker" ||
                e === "tweeKlank",
              2
            ),
            en(klasse(e => e === "rest", 2), klasse(e => e !== "rest", 3)),
            en(
              klasse(e => e === "rest", 2),
              klasse(e => e === "rest", 3),
              klasse(e => e !== "stommeE", 4)
            ),
            en(laatsteKlank(2), of(klank("s", 2), klank("e", 2)))
          )
        ),
        "stommeE"
      ))
  );

  // voorvoegsels. be- ge-, ver- te-
  const voorvoegsel = (offset = 0) =>
    en(
      klasse(k => k === "rest", offset + 1),
      klasse(k => k !== "rest", offset + 2),
      klankLengte(l => l == 1, offset + 1),
      totaalKlasse(k => k !== "rest" && k !== "stommeE", a => a > 1)
    );
  const voorvoegselErvoor = () =>
    of(
      en(of(klank("b", -3), klank("g", -3), klank("t", -3)), klank("e", -2)),
      en(klank("v", -4), klank("e", -3), klank("r", -2))
    );

  const werkwoord = () =>
    of(eindKlank("d"), eindKlank("t"), en(eindKlank("e", 1), eindKlank("n")));
  const heeftAchtervoegsel = () =>
    of(
      en(eindKlank("l", 2), eindKlank("ij", 1), eindKlank("k")),
      en(eindKlank("i", 1), eindKlank("g"))
    );

  ["d", "b", "t"].forEach(
    beginLetter =>
      (resultaat = verwerk(
        resultaat,
        selecteerKlank("e", null),
        en(
          voorvoegsel(),
          klank(beginLetter, -1),
          niet(voorvoegselErvoor()),
          niet(klank("s", -2))
        ),
        "stommeE"
      ))
  );

  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(
      voorvoegsel(),
      klank("g", -1),
      en(
        of(eersteKlank(1), werkwoord(), heeftAchtervoegsel()),
        of(
          niet(voorvoegselErvoor()),
          klasse(k => k === "tweeKlank" || k === "langeKlinker", 2)
        )
      )
    ),
    "stommeE"
  );

  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(voorvoegsel(1), eersteKlank(1), klank("v", -1), klank("r", 1)),
    "stommeE"
  );
  // me-
  resultaat = verwerk(
    resultaat,
    selecteerKlank("e", null),
    en(
      voorvoegsel(),
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
    en(
      totaalKlanken(a => a > 3),
      of(
        en(laatsteKlank(1), klank("g", 1)),
        en(laatsteKlank(2), klank("g", 1), klank("e", 2))
      )
    ),
    "stommeE"
  );
  resultaat = verwerk(
    // -lijk
    resultaat,
    selecteerKlank("ij", "tweeKlank"),
    en(
      totaalKlanken(a => a > 3),
      of(
        en(laatsteKlank(1), klank("l", -1), klank("k", 1)),
        en(laatsteKlank(2), klank("l", -1), klank("k", 1), klank("e", 2))
      ),
      of(
        niet(
          isWoord(woord =>
            ["ongelijk", "gelijk", "tegelijk", "vergelijk"].includes(woord)
          )
        )
      )
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

const verwerkSpecialeKlanken = resultaat => {
  resultaat = verwerk(
    // ci
    resultaat,
    selecteerKlank("i", "korteKlinker"),
    klank("c", -1),
    "speciaal"
  );

  resultaat = verwerk(
    // isch
    resultaat,
    selecteerKlank("sch", null),
    of(laatsteKlank(), en(laatsteKlank(1), klank("e", 1))),
    "speciaal"
  );

  // lange i
  resultaat = verwerk(
    resultaat,
    selecteerKlank("i", "korteKlinker"),
    of(
      en(
        niet(
          klasse(
            a => a !== "rest" && a !== "korteKlinker" && a !== "speciaal",
            -1
          )
        ),
        niet(klasse(a => a === "rest", -2)),
        klankLengte(l => l === 1, 1),
        klasse(a => a !== "rest", 2)
      ),
      en(klank("sch", 1), klasse(k => k === "speciaal", 1))
    ),
    "speciaal"
  );

  // G als J
  resultaat = verwerk(
    resultaat,
    selecteerKlank("g", "rest"),
    of(
      en(
        klasse(k => k === "langeKlinker", -1),
        klankIndex(i => i > 1),
        klank("e", 1),
        niet(klank("l", 2)),
        niet(klank("n", 2)),
        niet(klank("h", -2)),
        of(klank("a", -1), klank("o", -1))
      ),
      en(
        klank("e", 1),
        klank("l", 2),
        of(klank("ij", 3), klank("a", 3), laatsteKlank(2)),
        niet(klank("k", 4))
      )
    ),
    "speciaal"
  );

  return resultaat;
};

const ontkenning = (resultaat, rest) =>
  resultaat.length === 1 &&
  resultaat[0][1].toLowerCase() === "o" &&
  rest.toLowerCase().startsWith("n") &&
  rest.length > 2;

const voegWoordKlassificatiesToe = woord => {
  let start = 0;
  let resultaat = [];

  while (start < woord.length) {
    const rest = woord.slice(start);
    let klassificaties = [];
    if (ontkenning(resultaat, rest)) {
      resultaat.push(["rest", rest[0]]);
      start += 1;
    } else {
      Object.entries(definities).forEach(([klasse, klanken]) => {
        const klank = klanken
          .filter(klank =>
            klank instanceof RegExp
              ? rest.toLowerCase().match(klank)
              : rest.toLowerCase().startsWith(klank)
          )
          .reduce((resultaat, klank) => {
            if (klank instanceof RegExp) {
              const match = rest.toLowerCase().match(klank);
              return match[0].length > resultaat.length ? match[0] : resultaat;
            } else {
              return klank.length > resultaat.length ? klank : resultaat;
            }
          }, "");
        if (klank) {
          klassificaties.push({ klasse, klank: rest.slice(0, klank.length) });
        }
      });
      const klassificatie = klassificaties.reduce(
        (huidigResultaat, { klasse, klank }) =>
          huidigResultaat === null ||
          klank.length > huidigResultaat.klank.length
            ? { klasse, klank }
            : huidigResultaat,
        null
      );
      if (klassificatie === null) {
        resultaat.push(["rest", rest[0]]);
        start += 1;
      } else {
        resultaat.push([klassificatie.klasse, klassificatie.klank]);
        start += klassificatie.klank.length;
      }
    }
  }

  resultaat = verwerkLangeKlinkers(resultaat);
  resultaat = verwerkStommeE(resultaat);
  resultaat = verwerkSpecialeKlanken(resultaat);

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
