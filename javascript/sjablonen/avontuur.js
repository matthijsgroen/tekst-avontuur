let skip = false;
let naam = "";
let spelToestand = Array(100).fill(0);

const screenElement = document.getElementById("screen");
const saveButton = document.getElementById("save");
confirming = false;
saveButton.addEventListener("click", () => {
  if (confirming) {
    localStorage.removeItem("opslag");
    localStorage.removeItem(`opslag-${bewaarSleutel}`);
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

const widthRuler = document.getElementById("width");
widthRuler.textContent = "?".repeat(81);

const resizeFont = () => {
  document.body.setAttribute("style", `font-size: 1rem;`);
  let screenWidth = screenElement.getBoundingClientRect().width;
  let lineWidth = widthRuler.getBoundingClientRect().width;
  const scale = screenWidth / lineWidth;
  document.body.setAttribute("style", `font-size: ${scale}rem;`);
};

window.addEventListener("resize", () => setTimeout(resizeFont, 100));
resizeFont();

window.addEventListener("mouseup", () => (skip = true));
window.addEventListener("touchend", () => (skip = true));

const cls = async () => {
  const pageElement = screenElement.parentElement;
  pageElement.classList.add("turn");

  await sleep(0.6);
  screenElement.innerHTML = "";
  pageElement.classList.remove("turn");
};

const sleep = duration =>
  duration === 0 || skip
    ? true
    : new Promise(resolve =>
        skip ? resolve() : setTimeout(resolve, duration * 1e3)
      );

let actieveKleur = "color7";
const color = index => (actieveKleur = `color${index}`);
const print = (tekst, actie) => {
  let parent = screenElement;
  let container = null;
  if (actie) {
    container = document.createElement("div");
    container.setAttribute("class", "actie");

    let linkTag = document.createElement("a");
    linkTag.setAttribute("href", "#");
    linkTag.addEventListener("click", e => {
      e.preventDefault();
      keyPressed = `${actie}`;
      return false;
    });
    container.appendChild(linkTag);
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
  if (container) {
    screenElement.appendChild(container);
  }
  if (tekst.includes("\n")) {
    screenElement.parentElement.scroll({
      top: screenElement.scrollHeight,
      behavior: "smooth"
    });
  }
};

const toets = (plek, bewerking, waarde) =>
  (bewerking === "=" && spelToestand[plek] === waarde) ||
  (bewerking === "!" && spelToestand[plek] !== waarde) ||
  (bewerking === ">" && spelToestand[plek] > waarde) ||
  (bewerking === "<" && spelToestand[plek] < waarde);

const toegestaan = beweringen =>
  beweringen.every(bewering => {
    const [, plek, bewerking, waarde] = bewering.match(/(\d+)([=!<>])(\d+)/);
    return toets(parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  });

const interpoleer = zin =>
  zin
    .replace(/\$n/g, naam)
    .replace(/#\d{2}/g, num => ` ${spelToestand[parseInt(num.slice(1), 10)]}`);

const tekst = async (verteller, zin, eerderGelezen) => {
  color(verteller);
  for (const letter of zin) {
    print(letter);
    await sleep(eerderGelezen ? 0 : 0.02);
  }
  print("\n");
};

const toonGebeurtenis = async () => {
  let verteller = 7;
  skip = false;
  await cls();
  for (const teksten of avontuur.scherm) {
    if (toegestaan(teksten.test)) {
      const eerderGelezen = teksten.gelezen === true;
      if (!eerderGelezen) teksten.gelezen = true;
      for (const zin of teksten.schermData) {
        if (zin[0] === "*") {
          // Opmaak
          const commando = zin[1];
          const data = zin.slice(2);
          if (commando === "c") {
            verteller = parseInt(data, 10);
          }
          if (commando === "s") {
            await sleep(parseInt(data, 10));
          }
        } else {
          await tekst(verteller, interpoleer(zin), eerderGelezen);
          await sleep(eerderGelezen ? 0.1 : 0);
        }
      }
      if (teksten.actie) {
        voerActieUit(teksten.actie);
      }
    }
  }
  keyPressed = null;
};

const muteer = (plek, bewerking, waarde) => {
  if (bewerking === "=") {
    spelToestand[plek] = waarde;
  }
  if (bewerking === "+") {
    spelToestand[plek] += waarde;
  }
  if (bewerking === "-") {
    spelToestand[plek] -= waarde;
  }
  if (bewerking === "r") {
    spelToestand[plek] = Math.min(
      waarde,
      Math.max(1, Math.ceil(Math.random() * waarde))
    );
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

const voerActieUit = instructies => {
  instructies.forEach(instructie => {
    const [, plek, bewerking, waarde] = instructie.match(/(\d+)([=+-r])(\d+)/);
    muteer(parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  });
};

const toetsen = "123456789abcdefghijklmnop";

const toonActies = async () => {
  const acties = [];
  let geteldeActies = 0;

  for (const actie of avontuur.acties) {
    if (toegestaan(actie.test)) {
      const toets = actie.toets || `${++geteldeActies}`;
      acties.push({
        naam: interpoleer(actie.tekst),
        actie: actie.actie,
        kleur: actie.kleur,
        toets
      });
    }
  }

  for (const actie of acties) {
    await sleep(0.2);
    color(actie.kleur);
    print(`${actie.toets}. ${actie.naam}\n`, actie.toets);
  }
  // geen acties, dan is spel voorbij
  if (acties.length === 0) return false;

  let toets;
  let gekozen = null;
  do {
    toets = await keypress();
    gekozen = acties.find(actie => actie.toets === toets);
  } while (!gekozen);

  voerActieUit(gekozen.actie);
  return true;
};

const krijgNaam = () =>
  new Promise(resolve => {
    const formulier = document.getElementById("welkom");
    formulier.addEventListener("submit", event => {
      event.preventDefault();
      resolve(document.getElementById("naam").value);
      formulier.remove();
    });
  });

const laadSpel = async () => {
  try {
    const opgeslagen =
      localStorage.getItem(`opslag-${bewaarSleutel}`) ||
      (bewaarSleutel === "koerier" && localStorage.getItem("opslag"));
    let data = JSON.parse(opgeslagen);

    naam = data.naam;
    spelToestand = data.gameState;
    document.getElementById("welkom").remove();
    return true;
  } catch (e) {}
  return false;
};

const bewaarSpel = () => {
  try {
    const opslag = {
      naam,
      gameState: spelToestand
    };
    localStorage.setItem(`opslag-${bewaarSleutel}`, JSON.stringify(opslag));
  } catch (e) {}
};

const spelLus = async () => {
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
