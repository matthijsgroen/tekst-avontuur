const cls = () => (screenElement.innerHTML = "");
const startSpel = () => {
  document.getElementById("welkom").remove();
  setTimeout(() => {
    const menu = document.querySelector(".menu");
    menu && menu.classList.remove("verberg");
  }, 0);
};

document.addEventListener("DOMContentLoaded", function() {
  let menuRef = null;
  const menu = () => menuRef;

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

  const hoofdMenu = h("div", { class: "actief" }, [
    h("h1", { class: "color14" }, gegevens.Titel),
    h("p", { class: "color7" }, `Geschreven door ${gegevens.Auteur}`),
    h("ul", {}, [
      optie("Info", () => {
        maakMenuActief(1);
      }),
      optie("Herstarten", () => {
        maakMenuActief(2);
      }),
      optie("Sluiten", () => sluitMenu())
    ])
  ]);

  const infoMenu = h("div", {}, [
    gegevens.Titel && h("h1", { class: "color14" }, gegevens.Titel),
    gegevens.Omschrijving && h("p", {}, gegevens.Omschrijving),
    gegevens.Versie && h("p", {}, `Versie: ${gegevens.Versie}`),
    gegevens.Datum && h("p", {}, `Datum: ${gegevens.Datum}`),
    gegevens.Email &&
      h("p", {}, [
        "Email: ",
        h(
          "a",
          { class: "color9", href: `mailto:${gegevens.Email}` },
          gegevens.Email
        )
      ]),
    gegevens.Twitter &&
      h("p", {}, [
        "Twitter: ",
        h(
          "a",
          { class: "color9", href: `https://twitter.com/${gegevens.Twitter}` },
          gegevens.Twitter
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
    herstartMenu
  ]);
  menuRef = opties;

  console.log(gegevens);

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
