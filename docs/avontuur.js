let skip = false;
let screenData = [];
let actieData = [];

let naam = "";
let gameState = Array(100).fill(0);

const screenElement = document.getElementById("screen");
const saveButton = document.getElementById("save");
confirming = false;
saveButton.addEventListener("click", () => {
  if (confirming) {
    localStorage.removeItem("opslag");
    document.location.reload();
  } else {
    saveButton.textContent = "Druk om reset te bevestigen";
    confirming = true;
    setTimeout(() => {
      saveButton.textContent = "Spel herstarten";
      confirming = false;
    }, 10000);
  }
});

const resizeFont = () => {
  let screenWidth = screenElement.getBoundingClientRect().width - 20;
  const charWidth = screenWidth / 82;
  document.body.setAttribute("style", `font-size: ${Math.floor(charWidth)}px;`);
};

window.addEventListener("resize", () => setTimeout(resizeFont, 100));
resizeFont();

window.addEventListener("mouseup", () => (skip = true));
window.addEventListener("touchend", () => (skip = true));

const cls = () => (screenElement.innerHTML = "");
const sleep = duration =>
  new Promise(resolve =>
    skip ? resolve() : setTimeout(resolve, duration * 1e3)
  );

let actieveKleur = "color7";
const color = index => (actieveKleur = `color${index}`);
const print = (tekst, actie) => {
  let parent = screenElement;
  let linkTag = null;
  if (actie) {
    linkTag = document.createElement("a");
    linkTag.setAttribute("href", "#");
    linkTag.addEventListener("click", e => {
      e.preventDefault();
      keyPressed = `${actie}`;
      return false;
    });
    parent = linkTag;
  }

  tekst.split("\n").forEach((element, index, list) => {
    if (element !== "") {
      // Vervang spaties door niet-brekende spaties
      const textNode = document.createTextNode(
        element.replace(/\u0020/g, "\u00a0")
      );
      const tag = document.createElement("span");
      tag.appendChild(textNode);
      tag.setAttribute("class", actieveKleur);
      parent.appendChild(tag);
    }
    if (index < list.length - 1) {
      const tag = document.createElement("br");
      parent.appendChild(tag);
    }
  });
  if (linkTag) {
    screenElement.appendChild(linkTag);
  }
  if (tekst.includes("\n")) {
    window.scrollTo(0, document.body.scrollHeight);
  }
};

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

  for (const karakter of bewering) {
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
  return (
    bewerking === "" ||
    toets(parseInt(plek, 10), bewerking, parseInt(waarde, 10))
  );
};

const interpoleer = zin =>
  zin
    .replace(/\$n/g, naam)
    .replace(/#\d{2}/g, num => ` ${gameState[parseInt(num.slice(1), 10)]}`);

const tekst = async (verteller, zin) => {
  color(verteller);
  for (const letter of zin) {
    print(letter);
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

document.addEventListener("keypress", event => {
  keyPressed = event.key;
  if (keyPressed == " ") {
    skip = true;
  }
});

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
  acties.forEach((actie, i) => {
    print("\n");
    print(`${i + 1} ) ${actie.naam}\n`, i + 1);
  });
  // geen acties, dan is spel voorbij
  if (acties.length === 0) return false;

  let toets;
  let keuze;
  do {
    toets = await keypress();
    keuze = toets && /\d/.test(toets) && parseInt(toets, 10);
  } while (!(keuze > 0 && keuze <= acties.length));

  voerActieUit(acties[keuze - 1].actie);
  return true;
};

const laadAvontuur = async () => {
  const regels = avontuur.split("\n").map(regel => regel.trim());
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

const krijgNaam = () =>
  new Promise(resolve => {
    const toetsAfwachten = event => {
      if (event.key === "Enter") {
        document.removeEventListener("keypress", toetsAfwachten);
        resolve(document.getElementById("naam").value);
        document.getElementById("welkom").remove();
      }
    };
    document.addEventListener("keypress", toetsAfwachten);
  });

const laadSpel = async () => {
  try {
    const opgeslagen = localStorage.getItem("opslag");
    let data = JSON.parse(opgeslagen);

    naam = data.naam;
    gameState = data.gameState;
    document.getElementById("welkom").remove();
    return true;
  } catch (e) {}
  return false;
};

const bewaarSpel = () => {
  try {
    const opslag = {
      naam,
      gameState
    };
    localStorage.setItem("opslag", JSON.stringify(opslag));
  } catch (e) {}
};

const spelLus = async () => {
  await laadAvontuur();
  const spelGeladen = await laadSpel();
  if (!spelGeladen) {
    naam = await krijgNaam();
    bewaarSpel();
  }

  const menu = document.querySelector(".menu");
  menu.classList.remove("verberg");
  let heeftActies;

  do {
    await toonGebeurtenis();
    heeftActies = await toonActies();
    bewaarSpel();
  } while (heeftActies);
  // Spel afgelopen
};
spelLus();
