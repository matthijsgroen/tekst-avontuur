const { promisify } = require("util");
const readFile = promisify(require("fs").readFile);

let skip = false;
let keyPressed = null;
const stdin = process.stdin;
stdin.setRawMode(true);
stdin.resume();
stdin.setEncoding("utf8");

const naam = "Hiddo";
const geslacht = true;

let screenData = [];
let actieData = [];

const readBasicData = async () => {
  const fileContents = await readFile("AVONTUUR.BAS", "utf8");
  const lines = fileContents.replace(/\r/g, "").split("\n");
  const dataRead = startToken => {
    let data = "";
    let started = false;
    lines.forEach(line => {
      if (line.startsWith(startToken) && !started) {
        started = true;
      }
      if (started && line.includes('DATA "')) {
        data += line.slice(line.indexOf('"'));
        if (line.includes("END")) {
          started = false;
        } else {
          data += ",";
        }
      }
    });
    return JSON.parse(`[${data}]`);
  };

  screenData = dataRead("1 DATA");
  actieData = dataRead("2 DATA");
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

const sleep = duration =>
  new Promise(resolve =>
    skip ? resolve() : setTimeout(resolve, duration * 1e3)
  );

// https://en.wikipedia.org/wiki/ANSI_escape_code
const color = index =>
  process.stdout.write(
    [
      "\u001b[30m", // 0 = black
      "\u001b[31m", // 1 = blue
      "\u001b[32m", // 2 = green
      "\u001b[36m", // 3 = cyan
      "\u001b[33m", // 4 = red
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

const tekst = async (verteller, zin) => {
  color(verteller);
  for (let i = 0; i < zin.length; i++) {
    print(zin[i]);
    await sleep(0.04);
  }
  print("\n");
};

const toonGebeurtenis = async () => {
  let index = -1;
  let verteller = 2;
  skip = false;
  cls();

  do {
    index++;
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
    } else if (bewering !== "END") {
      do {
        index++;
      } while (!screenData[index].startsWith("&"));
    }
  } while (screenData[index] !== "END");
  keyPressed = null;
};

const muteer = (plek, bewerking, waarde) => {
  if (bewerking === "=") {
    gameState[plek] = waarde;
  }
  if (bewerking === "+") {
    gameState[plek] += waarde;
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
  let index = -1;
  let verteller = 2;
  let bewering;

  do {
    index++;
    bewering = actieData[index];
    if (toegestaan(bewering)) {
      acties.push({ naam: actieData[index + 1], actie: actieData[index + 2] });
    }
    index += 2;
  } while (bewering !== "END");

  print("\n");
  color(15);
  print("Wat ga je doen:\n");
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

const gameLus = async () => {
  await readBasicData();
  do {
    await toonGebeurtenis();
    await toonActies();
  } while (gameState[0] === 0);
  process.exit(0);
};

function toUnicode(theString) {
  var unicodeString = "";
  for (var i = 0; i < theString.length; i++) {
    var theUnicode = theString
      .charCodeAt(i)
      .toString(16)
      .toUpperCase();
    while (theUnicode.length < 4) {
      theUnicode = "0" + theUnicode;
    }
    theUnicode = "\\u" + theUnicode;
    unicodeString += theUnicode;
  }
  return unicodeString;
}

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

gameLus();
