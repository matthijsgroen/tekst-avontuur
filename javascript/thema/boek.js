const cls = async () => {
  const pageElement = screenElement.parentElement;
  pageElement.classList.add("turn");

  await sleep(0.6);
  screenElement.innerHTML = "";
  pageElement.classList.remove("turn");
};

const startSpel = () => {
  document.getElementsByClassName("kaft")[0].classList.add("open", "ingevuld");
  setTimeout(() => {
    const menu = document.querySelector(".menu");
    menu && menu.classList.remove("verberg");
  }, 0);
};

const eindSpel = () => {
  const paragraaf = document.createElement("p");
  paragraaf.classList.add("einde");
  const linkTag = document.createElement("a");
  const textNode = document.createTextNode("Sluit boek");
  linkTag.appendChild(textNode);
  linkTag.setAttribute("href", "#");
  linkTag.addEventListener("click", e => {
    e.preventDefault();
    linkTag.classList.add("verberg");
    document.getElementsByClassName("kaft")[0].classList.remove("open");
    document.getElementsByClassName("bedankt")[0].classList.add("zichtbaar");
    return false;
  });
  paragraaf.appendChild(linkTag);
  screenElement.appendChild(paragraaf);
};

document.addEventListener("DOMContentLoaded", function() {
  let menuRef = null;
  const menu = () => menuRef;
  if (isEPaper) {
    document.body.classList.add("ereader");
  }

  const sluitMenu = () => menu().classList.remove("zichtbaar");
  const maakMenuActief = index => {
    for (let i = 0; i < menu().children.length; i++) {
      const child = menu().children[i];
      i == index
        ? child.classList.add("actief")
        : child.classList.remove("actief");
    }
  };

  const optie = (naam, handler) => {
    const onClick = e => {
      e.preventDefault();
      handler();
    };
    return h("li", {}, h("a", { href: "#", class: "color7", onClick }, naam));
  };
  const extraOpties = gegevens.menulink
    ? []
        .concat(gegevens.menulink)
        .map(link => link.match(/^\[([^\]]+)]\(([^)]+)\)$/))
        .filter(item => item && !location.href.endsWith(item[2]))
        .map(([, naam, link]) =>
          h("li", {}, h("a", { href: link, class: "color7" }, naam))
        )
    : [];

  const menuOpties = [
    optie("Info / Contact", () => {
      maakMenuActief(1);
    }),
    optie("Spel herstarten", () => {
      maakMenuActief(2);
    }),
    optie("Spel-voortgangs-link versturen", () => {
      maakMenuActief(3);
    }),

    ...extraOpties,
    optie("Menu sluiten", () => sluitMenu())
  ];

  const hoofdMenu = h("div", { class: "actief" }, [
    h("h1", { class: "color14" }, gegevens.titel),
    h("p", { class: "color7" }, `Geschreven door ${gegevens.auteur}`),
    h("ul", {}, menuOpties)
  ]);

  let emailAddres = "";
  const verzendMenu = h("div", {}, [
    h("h1", { class: "color14" }, "Spel voortgang versturen"),
    h(
      "p",
      { class: "color7" },
      "Hiermee kan je de voortgang van het spel naar jezelf e-mailen, om op een ander apparaat verder te spelen."
    ),
    h("input", {
      type: "email",
      onInput: e => {
        emailAddres = e.target.value;
      }
    }),
    h(
      "button",
      {
        type: "button",
        onClick: e => {
          const subject = `Voortgang ${gegevens.titel}`;
          const link = verzendLink();
          const body = `Hoi ${naam},\n\nGebruik deze link om verder te spelen: ${link}\n\nGroetjes!`;
          open(
            `mailto:${emailAddres}?subject=${encodeURI(
              subject
            )}&body=${encodeURI(body)}`
          );
        }
      },
      "Verstuur"
    ),
    h("ul", {}, [optie("Terug", () => maakMenuActief(0))])
  ]);

  const infoMenu = h("div", {}, [
    gegevens.titel && h("h1", { class: "color14" }, gegevens.titel),
    gegevens.omschrijving && h("p", {}, gegevens.omschrijving),
    gegevens.versie && h("p", {}, `Versie: ${gegevens.versie}`),
    gegevens.datum && h("p", {}, `Datum: ${gegevens.datum}`),
    gegevens.email &&
      h("p", {}, [
        "Email: ",
        h(
          "a",
          { class: "color9", href: `mailto:${gegevens.email}` },
          gegevens.email
        )
      ]),
    gegevens.twitter &&
      h("p", {}, [
        "Twitter: ",
        h(
          "a",
          { class: "color9", href: `https://twitter.com/${gegevens.twitter}` },
          gegevens.twitter
        )
      ]),
    h("ul", {}, [optie("Terug", () => maakMenuActief(0))])
  ]);

  const herstartMenu = h("div", {}, [
    h(
      "p",
      { class: "color12" },
      "Dit herstart het spel en verwijderd alle voortgang!"
    ),
    h("p", { class: "color7" }, "Weet je het zeker?"),
    h("ul", {}, [
      optie("Herstarten", () => {
        resetSpel();
      }),
      optie("Terug", () => maakMenuActief(0))
    ])
  ]);

  const opties = h("div", { id: "opties", class: "opties" }, [
    hoofdMenu,
    infoMenu,
    herstartMenu,
    verzendMenu
  ]);
  menuRef = opties;

  const openMenu = e => {
    e.preventDefault();
    opties.classList.add("zichtbaar");
    maakMenuActief(0);
  };

  const menuKnop = h(
    "div",
    { class: "menu verberg" },
    h("button", { id: "menu", onClick: openMenu }, "Menu")
  );
  document.body.appendChild(menuKnop);
  document.body.appendChild(opties);
});
