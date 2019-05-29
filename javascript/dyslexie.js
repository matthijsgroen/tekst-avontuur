const definities = {
  korteKlinker: ["a", "e", "i", "o", "u"],
  langeKlinker: ["aa", "ee", "oo", "uu"],
  ieKlinker: ["ie", "oe", "eu", "ui", "ei", "ij", "ou", "au"],
  ooiKlinker: ["aai", "ooi", "oei", "eeuw", "ieuw", "uw"]
};

const reset = () => "\u001b[0m";
const tonen = {
  korteKlinker: klinker =>
    process.stdout.write("\u001b[97;42m" + klinker + reset()),
  langeKlinker: langeKlinker =>
    process.stdout.write("\u001b[30;43m" + langeKlinker + reset()),
  ieKlinker: ieKlinker =>
    process.stdout.write("\u001b[97;41m" + ieKlinker + reset()),
  ooiKlinker: ooiKlinker =>
    process.stdout.write("\u001b[30;47m" + ooiKlinker + reset()),
  rest: rest => process.stdout.write("\u001b[97;44m" + rest + reset()),
  stommeE: () => process.stdout.write("\u001b[97;48;5;202m" + "e" + reset())
};

const voegKlassificatiesToe = woord => {
  let start = 0;
  let resultaat = [];
  while (start < woord.length) {
    const rest = woord.slice(start);
    let klassificaties = [];
    Object.entries(definities).forEach(([klasse, klanken]) => {
      const klank = klanken.find(klank => rest.startsWith(klank));
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
      if (laatsteResultaat && laatsteResultaat[0] === "rest") {
        resultaat[resultaat.length - 1][1] += rest[0];
      } else {
        resultaat.push(["rest", rest[0]]);
      }
      start += 1;
    } else {
      resultaat.push([klassificatie.klasse, klassificatie.klank]);
      start += klassificatie.klank.length;
    }
  }
  return resultaat;
};

const toonWoord = woord => {
  const woordMetKlassificaties = voegKlassificatiesToe(woord);

  woordMetKlassificaties.forEach(([klassificatie, woord]) =>
    tonen[klassificatie](woord)
  );
};

module.exports = { toonWoord };
