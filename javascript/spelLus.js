const { sleep, cls, color, print } = require("./basic");
const { voerActieUit, toegestaan } = require("./spelToestand");
const { bewaarSpel, laadSpel, basisNaam } = require("./bewaarSpel");
const leesAvontuur = require("./leesAvontuur");
const stdin = process.stdin;

const input = prompt =>
  new Promise(resolve => {
    print(prompt + "? ");
    stdin.setRawMode(false);
    stdin.setEncoding("utf8");
    const callback = function(chunk) {
      resolve(chunk.slice(0, -1));

      stdin.setRawMode(true);
      stdin.setEncoding("utf8");
      stdin.removeListener("data", callback);
    };

    stdin.on("data", callback);
  });

let skip = false;
let keyPressed = null;
let naam = "";
let spelToestand = Array(100).fill(0);

const interpoleer = zin =>
  zin
    .replace(/\$n/g, naam)
    .replace(/#\d{2}/g, num => ` ${spelToestand[parseInt(num.slice(1), 10)]}`);

let vorigeZin = "";
const tekst = async (verteller, zin, eerderGelezen) => {
  const MAX_LENGTH = Math.min(process.stdout.columns, 80);
  const inGesprek = /^.*: '/.test(zin);
  const inLijst = /^\s*-\s/.test(zin);

  color(verteller);
  const indent = zin.match(/^\s*/)[0].length;
  const plakIndent = inGesprek ? 2 : inLijst ? indent + 2 : indent;
  if (inLijst) {
    vorigeZin = "";
    print("\n");
  }
  const regels =
    (vorigeZin.length === 0 ? " ".repeat(indent) : " ") +
    zin
      .slice(indent)
      .split(" ")
      .reduce(
        (regels, woord) => {
          const laatsteRegel = regels.slice(-1)[0];
          if (
            (laatsteRegel + " " + woord).length +
              plakIndent +
              vorigeZin.length >=
            MAX_LENGTH
          ) {
            vorigeZin = "";
            return [...regels, woord];
          } else {
            return [
              ...regels.slice(0, -1),
              (laatsteRegel + " " + woord).trim()
            ];
          }
        },
        [""]
      )
      .join(`\n${" ".repeat(plakIndent)}`);

  for (let i = 0; i < regels.length; i++) {
    print(regels[i]);
    await sleep(skip || eerderGelezen ? 0 : 0.02);
  }
  vorigeZin = regels.split("\n").slice(-1)[0];
  if (zin === "") {
    print("\n\n");
    vorigeZin = "";
  }
};

const gelezen = [];
const toonGebeurtenis = async schermData => {
  let verteller = 7;
  skip = false;
  vorigeZin = "";
  cls();

  for (let index = 0; index < schermData.length; index++) {
    let bewering = schermData[index];
    if (toegestaan(spelToestand, bewering)) {
      const eerderGelezen = gelezen.includes(index);
      if (!eerderGelezen) gelezen.push(index);
      do {
        index++;
        const sentence = schermData[index];
        if (sentence[0] === "&") {
          // Einde en acties
          if (sentence.slice(1).length > 0) {
            voerActieUit(spelToestand, sentence.slice(1));
          }
        } else if (sentence[0] === "*") {
          // Opmaak
          const command = sentence[1];
          const data = sentence.slice(2);
          if (command === "c") {
            verteller = parseInt(data, 10);
          }
          if (command === "s") {
            await sleep(skip ? 0 : parseInt(data, 10));
          }
        } else {
          await tekst(verteller, interpoleer(sentence), eerderGelezen);
          await sleep(eerderGelezen ? 0.1 : 0);
        }
      } while (!schermData[index].startsWith("&"));
    } else {
      do {
        index++;
      } while (!schermData[index].startsWith("&"));
    }
  }
  keyPressed = null;
};

const keypress = () =>
  new Promise(resolve => {
    let watcher = setInterval(() => {
      if (keyPressed !== null) {
        resolve(keyPressed);
        keyPressed = null;
        clearInterval(watcher);
      }
    }, 100);
  });

const toetsen = "123456789abcdefghijklmnop";

const heeftWaardeVoorSleutel = (sleutel, standaard) => bewering =>
  bewering
    .split(";")
    .filter(item => item.startsWith(`${sleutel}=`))
    .map(item => item.split("=")[1])[0] || standaard;

const heeftToets = heeftWaardeVoorSleutel("k", null);
const heeftKleur = heeftWaardeVoorSleutel("c", 7);

const toonActies = async actieData => {
  const acties = [];
  let verteller = 2;
  let bewering;
  let geteldeActies = 0;

  for (let index = 0; index < actieData.length; index++) {
    bewering = actieData[index];
    if (toegestaan(spelToestand, bewering)) {
      const toets = heeftToets(bewering) || `${++geteldeActies}`;
      const kleur = heeftKleur(bewering);
      acties.push({
        naam: interpoleer(actieData[index + 1]),
        actie: actieData[index + 2],
        kleur,
        toets
      });
    }
    index += 2;
  }

  for (const actie of acties) {
    await sleep(skip ? 0 : 0.2);
    color(actie.kleur);
    console.log(`${actie.toets.toString().toUpperCase()}. ${actie.naam}`);
  }
  if (acties.length === 0) {
    return false;
  }

  let toets;
  let gekozen = null;
  do {
    toets = await keypress();
    gekozen = acties.find(actie => actie.toets === toets);
  } while (!gekozen);

  voerActieUit(spelToestand, gekozen.actie);
  return true;
};

const spelLus = async (bestandsNaam, herstarten = false) => {
  const data = await leesAvontuur(bestandsNaam);
  const opslagBestandsNaam = `.${basisNaam(bestandsNaam)}.opslag`;
  const eerderSpel = herstarten ? null : await laadSpel(opslagBestandsNaam);

  stdin.resume();
  stdin.setRawMode(true);
  stdin.setEncoding("utf8");

  stdin.on("data", function(key) {
    // ctrl-c ( end of text )
    if (key === "\u0003" || key === "\u001b") {
      cls();
      color(7);
      print("Bedankt voor het spelen!\n");

      process.exit();
    }
    if (key === "\u0020") {
      skip = true;
    }
    keyPressed = key;
  });

  if (eerderSpel) {
    naam = eerderSpel.naam;
    spelToestand = eerderSpel.spelToestand;
  } else {
    cls();
    print("Hallo avonturier!\n");
    print("\n");
    naam = await input("Wat is je naam");
    await bewaarSpel(opslagBestandsNaam, { naam, spelToestand });
  }

  let heeftActies = true;

  do {
    await toonGebeurtenis(data.schermData);
    await print("\n");
    heeftActies = await toonActies(data.actieData);
    if (heeftActies) {
      await bewaarSpel(opslagBestandsNaam, { naam, spelToestand });
    }
  } while (heeftActies);
  process.exit(0);
};

module.exports = spelLus;
