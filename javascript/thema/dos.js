const cls = () => (screenElement.innerHTML = "");
const startSpel = () => document.getElementById("welkom").remove();

document.addEventListener("DOMContentLoaded", function() {
  const menuKnop = document.getElementById("menu");
  const opties = document.getElementById("opties");
  const [
    infoLink,
    herstartenLink,
    sluitenLink,
    resetLink,
    annuleerResetLink,
    sluitInfoLink
  ] = opties.getElementsByTagName("A");

  const maakMenuActief = index => {
    for (let i = 0; i < opties.children.length; i++) {
      const child = opties.children[i];
      i == index
        ? child.classList.add("actief")
        : child.classList.remove("actief");
    }
  };

  menuKnop.addEventListener("click", () => {
    opties.classList.add("zichtbaar");
    maakMenuActief(0);
  });
  sluitenLink.addEventListener("click", () => {
    opties.classList.remove("zichtbaar");
  });

  herstartenLink.addEventListener("click", () => {
    maakMenuActief(1);
  });
  annuleerResetLink.addEventListener("click", () => {
    maakMenuActief(0);
  });
  resetLink.addEventListener("click", () => {
    resetSpel();
  });

  infoLink.addEventListener("click", () => {
    maakMenuActief(2);
  });
  sluitInfoLink.addEventListener("click", () => {
    maakMenuActief(0);
  });

  const info = document.getElementById("spelInfo");
  if (gegevens.Titel) {
    info.appendChild(h("h1", { class: "color14" }, gegevens.Titel));
  }
  if (gegevens.Omschrijving) {
    info.appendChild(h("p", {}, gegevens.Omschrijving));
  }
  if (gegevens.Versie) {
    info.appendChild(h("p", {}, `Versie: ${gegevens.Versie}`));
  }
  if (gegevens.Datum) {
    info.appendChild(h("p", {}, `Datum: ${gegevens.Datum}`));
  }
  if (gegevens.Email) {
    info.appendChild(
      h("p", {}, [
        "Email: ",
        h(
          "a",
          { class: "color9", href: `mailto:${gegevens.Email}` },
          gegevens.Email
        )
      ])
    );
  }
  if (gegevens.Twitter) {
    info.appendChild(
      h("p", {}, [
        "Twitter: ",
        h(
          "a",
          { class: "color9", href: `https://twitter.com/${gegevens.Twitter}` },
          gegevens.Twitter
        )
      ])
    );
  }
  console.log(gegevens);
});
