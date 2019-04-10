#!/usr/bin/env node

const { promisify } = require("util");
const readFile = promisify(require("fs").readFile);
const stdin = process.stdin;
stdin.resume();

const sleep = duration =>
  new Promise(resolve =>
    skip ? resolve() : setTimeout(resolve, duration * 1e3)
  );

// https://en.wikipedia.org/wiki/ANSI_escape_code
const color = index =>
  process.stdout.write(
    [
      "\u001b[30m", // 0 = black
      "\u001b[34m", // 1 = blue
      "\u001b[32m", // 2 = green
      "\u001b[36m", // 3 = cyan
      "\u001b[31m", // 4 = red
      "\u001b[35m", // 5 = magenta
      "\u001b[33m", // 6 = yellow
      "\u001b[37m", // 7 = white
      "\u001b[90m", // 8 = grey
      "\u001b[94m", // 9 = bright blue
      "\u001b[92m", // 10 = bright green
      "\u001b[96m", // 11 = bright cyan
      "\u001b[91m", // 12 = bright red
      "\u001b[95m", // 13 = bright magenta
      "\u001b[93m", // 14 = bright yellow
      "\u001b[97m" // 15 = bright white
    ][index]
  );

const print = tekst => process.stdout.write(tekst);
const cls = () => process.stdout.write("\x1Bc");

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

let naam = "";
let geslacht = null;
let screenData = [];
let actieData = [];

const readBasicData = async () => {
  const inhoud = await readFile("AVONTUUR.DAT", "utf8");
  const regels = inhoud.split("\r\n");
  let actieModus = false;

  regels
    .filter(regel => regel.startsWith('"'))
    .forEach(regel => {
      const elementen = JSON.parse(`[${regel}]`);

      if (elementen.length === 1 && elementen[0] === "END") {
        actieModus = true;
      } else {
        actieModus
          ? actieData.push(...elementen)
          : screenData.push(...elementen);
      }
    });
};

const gameState = Array(100).fill(0);

const toets = (plek, bewerking, waarde) =>
  (bewerking === "=" && gameState[plek] === waarde) ||
  (bewerking === "!" && gameState[plek] !== waarde) ||
  (bewerking === ">" && gameState[plek] > waarde) ||
  (bewerking === "<" && gameState[plek] < waarde);

const toegestaan = bewering => {
  if (bewering === "END") return false;

  let plek = "";
  let bewerking = "";
  let waarde = "";
  for (let i = 0; i < bewering.length; i++) {
    const karakter = bewering[i];
    if (/\d/.test(karakter)) {
      bewerking === "" ? (plek += karakter) : (waarde += karakter);
    } else if (karakter === ";") {
      if (!toets(parseInt(plek, 10), bewerking, parseInt(waarde, 10))) {
        return false;
      }
      plek = "";
      waarde = "";
      bewerking = "";
    } else {
      bewerking += karakter;
    }
  }

  if (bewerking !== "") {
    return toets(parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  }

  return true;
};

const interpoleer = zin =>
  zin
    .replace(/\$n/g, naam)
    .replace(/\$h/g, geslacht ? "hij" : "zij")
    .replace(/\$H/g, geslacht ? "Hij" : "Zij")
    .replace(/\$z/g, geslacht ? "zijn" : "haar")
    .replace(/\$Z/g, geslacht ? "Zijn" : "Haar")
    .replace(/#\d{2}/g, num => ` ${gameState[parseInt(num.slice(1), 10)]}`);

const tekst = async (verteller, zin) => {
  color(verteller);
  for (let i = 0; i < zin.length; i++) {
    print(zin[i]);
    await sleep(0.04);
  }
  print("\n");
};

const toonGebeurtenis = async () => {
  let verteller = 2;
  skip = false;
  cls();

  for (let index = 0; index < screenData.length; index++) {
    let bewering = screenData[index];
    if (toegestaan(bewering)) {
      do {
        index++;
        const sentence = screenData[index];
        if (sentence[0] === "&") {
          // Einde en acties
          if (sentence.slice(1).length > 0) {
            voerActieUit(sentence.slice(1));
          }
        } else if (sentence[0] === "*") {
          // Opmaak
          const command = sentence[1];
          const data = sentence.slice(2);
          if (command === "c") {
            verteller = parseInt(data, 10);
          }
          if (command === "s") {
            await sleep(parseInt(data, 10));
          }
        } else {
          await tekst(verteller, interpoleer(sentence));
        }
      } while (!screenData[index].startsWith("&"));
    } else {
      do {
        index++;
      } while (!screenData[index].startsWith("&"));
    }
  }
  keyPressed = null;
};

const muteer = (plek, bewerking, waarde) => {
  if (bewerking === "=") {
    gameState[plek] = waarde;
  }
  if (bewerking === "+") {
    gameState[plek] += waarde;
  }
  if (bewerking === "-") {
    gameState[plek] -= waarde;
  }
};

const voerActieUit = actie => {
  let plek = "";
  let bewerking = "";
  let waarde = "";
  for (let i = 0; i < actie.length; i++) {
    const karakter = actie[i];
    if (/\d/.test(karakter)) {
      bewerking === "" ? (plek += karakter) : (waarde += karakter);
    } else if (karakter === ";") {
      muteer(parseInt(plek, 10), bewerking, parseInt(waarde, 10));
      plek = "";
      waarde = "";
      bewerking = "";
    } else {
      bewerking += karakter;
    }
  }

  if (bewerking !== "") {
    muteer(parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  }
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

const toonActies = async () => {
  const acties = [];
  let verteller = 2;
  let bewering;

  for (let index = 0; index < actieData.length; index++) {
    bewering = actieData[index];
    if (toegestaan(bewering)) {
      acties.push({ naam: actieData[index + 1], actie: actieData[index + 2] });
    }
    index += 2;
  }

  color(7);
  acties.forEach((actie, i) => console.log(`${i + 1} ) ${actie.naam}`));

  let toets;
  let keuze;
  do {
    toets = await keypress();
    keuze = toets && /\d/.test(toets) && parseInt(toets, 10);
  } while (!(keuze > 0 && keuze <= acties.length));

  voerActieUit(acties[keuze - 1].actie);
};

const spelLus = async () => {
  cls();
  print("Hallo avonturier!\n");
  print("\n");
  naam = await input("Wat is je naam");
  print("Ben je een jongen of een meisje? (j/m)\n");
  let toets;
  do {
    toets = await keypress();
  } while (toets !== "j" && toets !== "m");
  geslacht = toets === "j";

  await readBasicData();

  do {
    await toonGebeurtenis();
    await toonActies();
  } while (gameState[0] === 0);
  process.exit(0);
};

spelLus();
