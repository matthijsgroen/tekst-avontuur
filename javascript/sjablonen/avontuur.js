let skip = false;
let naam = "";
let spelToestand = Array(100).fill(0);
let verzendCode;

const verzendLink = () => location.href + `#naam=${naam}|code=${verzendCode}`;
const resetSpel = () => {
  localStorage.removeItem("opslag");
  localStorage.removeItem(`opslag-${bewaarSleutel}`);
  document.location.reload();
};

const h = (tagName, attributes = {}, children = []) => {
  const element = document.createElement(tagName);
  []
    .concat(children)
    .filter(Boolean)
    .forEach(child =>
      typeof child === "string"
        ? element.appendChild(document.createTextNode(child))
        : element.appendChild(child)
    );

  Object.entries(attributes).forEach(([key, value]) => {
    const handler = key.match(/^on(.*)$/);
    if (handler) {
      element.addEventListener(handler[1].toLowerCase(), value);
    } else {
      element.setAttribute(key, value);
    }
  });
  return element;
};

const screenElement = document.getElementById("screen");

const widthRuler = document.getElementById("width");
widthRuler.textContent =
  'A Quick brown fox jumps. It jumps fast and HIGH over the "Lazy", sleeping, dog!?';

const resizeFont = () => {
  document.body.setAttribute("style", `font-size: 1rem;`);
  widthRuler.setAttribute("style", "display: block;");
  let screenWidth = screenElement.getBoundingClientRect().width;
  let lineWidth = widthRuler.getBoundingClientRect().width;
  const scale = Math.max(screenWidth / lineWidth, 1);
  document.body.setAttribute("style", `font-size: ${scale}rem;`);
  widthRuler.setAttribute("style", "display: none;");
};

window.addEventListener("resize", () => setTimeout(resizeFont, 100));
resizeFont();

window.addEventListener("mouseup", () => (skip = true));
window.addEventListener("touchend", () => (skip = true));

const sleep = duration =>
  duration === 0 || skip
    ? true
    : new Promise(resolve =>
        skip ? resolve() : setTimeout(resolve, duration * 1e3)
      );

let actieveKleur = "color7";
const color = index => (actieveKleur = `color${index}`);
const printActie = (tekst, actie, parent) => {
  const props = { class: `actie ${actieveKleur}` };
  if (/^[a-z]$/.test(actie)) {
    props.style = "list-style-type: lower-alpha;";
    props.value = `${actie.charCodeAt(0) - 96}`;
  }
  if (/^[0-9]$/.test(actie)) {
    props.style = "list-style-type: decimal;";
    props.value = `${actie.charCodeAt(0) - 48}`;
  }
  const onClick = e => {
    e.preventDefault();
    keyPressed = `${actie}`;
    return false;
  };
  const lijstItem = h(
    "li",
    props,
    h(
      "a",
      {
        href: "#",
        onClick
      },
      h("span", { class: actieveKleur }, tekst)
    )
  );

  parent.appendChild(lijstItem);
};

const print = tekst => {
  let parent = screenElement.children[screenElement.children.length - 1];
  if (parent.tagName === "UL") {
    const items = parent.getElementsByTagName("li");
    parent = items.item(items.length - 1);
  }

  tekst.split("\n").forEach((element, index, list) => {
    if (element !== "") {
      parent.appendChild(h("span", { class: actieveKleur }, element));
    }
    if (index < list.length - 1) {
      const tag = document.createTextNode(" ");
      parent.appendChild(tag);
    }
  });
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
  if (zin === "") {
    const paragraaf = document.createElement("p");
    screenElement.appendChild(paragraaf);
    return;
  }
  const isLijstItem = zin.match(/^\s*-\s(\S.*)$/);
  if (isLijstItem) {
    const parent = screenElement.children[screenElement.children.length - 1];
    if (parent.tagName === "UL") {
      parent.appendChild(h("li"));
    } else {
      screenElement.appendChild(h("ul", { class: actieveKleur }, h("li")));
    }
    if (parent.tagName === "P" && parent.children.length === 0) {
      parent.remove();
    }
    zin = isLijstItem[1];
  }

  const isGesprek = /^.*: '/.test(zin);
  if (isGesprek) {
    const paragrafen = screenElement.getElementsByTagName("p");
    const huidigeParagraaf = paragrafen.item(paragrafen.length - 1);
    huidigeParagraaf.classList.add("gesprek");
  }

  if (eerderGelezen) {
    print(zin);
  } else {
    for (const letter of zin) {
      print(letter);
      await sleep(0.02);
    }
  }
  print("\n");
};

const isEPaper = [" Silk/", " Nook/", " PocketBook/"].some(e =>
  navigator.userAgent.includes(e)
);

const toonGebeurtenis = async () => {
  let verteller = 7;
  const paragraaf = document.createElement("p");
  screenElement.appendChild(paragraaf);

  skip = false;
  for (const teksten of avontuur.scherm) {
    if (toegestaan(teksten.test)) {
      const eerderGelezen = teksten.gelezen === true || isEPaper;
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
      const toets = actie.toets || toetsen[geteldeActies++];
      acties.push({
        naam: interpoleer(actie.tekst),
        actie: actie.actie,
        kleur: actie.kleur,
        toets
      });
    }
  }

  const actieLijst = document.createElement("ul");
  actieLijst.classList.add("acties");
  screenElement.appendChild(actieLijst);
  for (const actie of acties) {
    await sleep(0.2);
    color(actie.kleur);
    printActie(actie.naam, actie.toets, actieLijst);
  }
  // geen acties, dan is spel voorbij
  if (acties.length === 0) {
    return false;
  }

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
    formulier.addEventListener("submit", async event => {
      event.preventDefault();
      const naam = document.getElementById("naam").value;
      if (naam.trim() === "") return;
      // -- template:startSpel
      resolve(naam);
    });
  });

const karakters =
  "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!@" +
  "$%^~*(),.;:\"'+-/[]{}_`<>?îêôâëüïöäøéíóáØÊÎÔÂËÜÖÏÉÚÍÓÁåÅßçÇàèìòùÀ";

const encode = spelStatus => {
  const relevanteData = spelStatus.reduce(
    (result, item, index) =>
      item === 0 ? result : { ...result, [index]: item },
    {}
  );
  const grootsteSleutelDelta = spelStatus
    .map((item, index) => (item > 0 ? index : null))
    .filter(index => index !== null)
    .map((index, i, list) => index - (list[i - 1] || -1) - 1)
    .reduce((currentMax, item) => (item > currentMax ? item : currentMax), 0);
  const grootsteWaarde = spelStatus.reduce(
    (currentMax, item) => (item > currentMax ? item : currentMax),
    0
  );
  const bitSizeKey = grootsteSleutelDelta.toString(2).length;
  const largeValueSize = grootsteWaarde.toString(2).length;

  const predictedWidth = Object.entries(relevanteData).reduce(
    (total, [key, value]) => total + bitSizeKey + largeValueSize,
    0
  );
  const toBits = (number, size) =>
    ("0".repeat(size) + number.toString(2)).slice(-size);

  const pairs = Object.entries(relevanteData);
  const bitStream =
    toBits(pairs.length, 7) +
    pairs
      .map(([key, value], index, list) => {
        const delta = index > 0 ? key - list[index - 1][0] - 1 : key;
        return toBits(delta, bitSizeKey) + toBits(value, largeValueSize);
      })
      .join("");

  const paddedBitStream = bitStream + "0".repeat(bitStream.length % 7);
  const amountOnes = paddedBitStream
    .split("")
    .reduce((result, item) => (item === "1" ? result + 1 : result), 0);

  let code = "" + karakters[bitSizeKey];
  for (let i = 0; i < paddedBitStream.length; i += 7) {
    const karakterIndex = parseInt(paddedBitStream.slice(i, i + 7), 2);
    code += karakters[karakterIndex];
  }
  code += karakters[largeValueSize] + karakters[amountOnes];

  return code;
};

const decode = code => {
  const charToNum = char => karakters.indexOf(char);

  const bitSizeKey = charToNum(code[0]);
  const largeValueSizeKey = charToNum(code.slice(-2, -1));
  const verifyOnes = charToNum(code.slice(-1));

  const bitStream = code
    .slice(1, -2)
    .split("")
    .map(char => ("0".repeat(7) + charToNum(char).toString(2)).slice(-7))
    .join("");

  const amountOnes = bitStream
    .split("")
    .reduce((result, bit) => (bit === "1" ? result + 1 : result), 0);

  if (verifyOnes !== amountOnes) throw new Error("Ongeldige code");

  const amountValues = parseInt(bitStream.slice(0, 7), 2);
  let gameState = Array(100).fill(0);

  let currentIndex = 0;
  for (let i = 0; i < amountValues; i++) {
    const bitIndex = 7 + (bitSizeKey + largeValueSizeKey) * i;
    const keyDelta = parseInt(
      bitStream.slice(bitIndex, bitIndex + bitSizeKey),
      2
    );
    const bitValue = bitStream.slice(
      bitIndex + bitSizeKey,
      bitIndex + bitSizeKey + largeValueSizeKey
    );
    const value = parseInt(bitValue, 2);
    currentIndex += i > 0 ? keyDelta + 1 : keyDelta;
    gameState[currentIndex] = value;
  }
  return gameState;
};

const laadSpel = async () => {
  try {
    try {
      if (location.hash.length > 10) {
        const velden = decodeURI(location.hash)
          .slice(1)
          .split("|")
          .reduce((resultaat, paar) => {
            const [naam, waarde] = paar.split("=");
            return { ...resultaat, [naam]: waarde };
          }, {});

        naam = velden.naam;
        spelToestand = decode(velden.code);
        location.hash = "";
        // -- template:startSpel
        return true;
      }
    } catch (e) {}
    const opgeslagen =
      localStorage.getItem(`opslag-${bewaarSleutel}`) ||
      (bewaarSleutel === "koerier" && localStorage.getItem("opslag"));
    if (!opgeslagen) return false;
    let data = JSON.parse(opgeslagen);

    naam = data.naam;
    spelToestand = data.gameState;
    // -- template:startSpel
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
    skip = false;
    bewaarSpel();
  }

  let heeftActies;

  do {
    verzendCode = encode(spelToestand);
    // -- template:startLus
    await toonGebeurtenis();
    heeftActies = await toonActies();
    bewaarSpel();
    skip = false;
    if (heeftActies) {
      // -- template:eindLus
    }
  } while (heeftActies);
  // Spel afgelopen
  // -- template:eindSpel
};
spelLus();
