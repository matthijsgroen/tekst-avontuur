const kaftKleuren = {
  rood: ["0", "80%", "50%"],
  groen: ["120", "50%", "30%"],
  blauw: ["240", "30%", "40%"],
  paars: ["300", "60%", "30%"],
};

const kaftKleur = ([tint, saturatie, helderheid]) =>
  `:root { --kaft-tint: ${tint}; --kaft-saturatie: ${saturatie}; --kaft-helder: ${helderheid}; }`;

const boekThema = {
  css: "thema/boek.css",
  html: "thema/boek.html",
  javascript: "thema/boek.js",
  haken: {
    startSpel: "startSpel",
    eindLus: "cls",
    eindSpel: "eindSpel",
    geladen: "geladen",
  },
  opties: ({ kaft = "rood" }) => ({
    javascript: 'let startThema = "boek";',
    css: kaftKleur(kaftKleuren[kaft]),
  }),
};

const bookThema = {
  css: "thema/book.css",
  html: "thema/book.html",
  javascript: "thema/book.js",
  haken: {
    startSpel: "startSpel",
    eindLus: "cls",
    eindSpel: "eindSpel",
    geladen: "geladen",
  },
  opties: ({ kaft = "rood" }) => ({
    javascript: 'let startThema = "boek";',
    css: kaftKleur(kaftKleuren[kaft]),
  }),
};

const dosThema = {
  css: "thema/boek.css",
  html: "thema/boek.html",
  javascript: "thema/boek.js",
  haken: {
    startSpel: "startSpel",
    eindLus: "cls",
    eindSpel: "eindSpel",
    geladen: "geladen",
  },
  opties: ({ kaft = "rood" }) => ({
    javascript: 'let startThema = "dos";',
    css: kaftKleur(kaftKleuren[kaft]),
  }),
};

module.exports = {
  boek: boekThema,
  book: bookThema,
  dos: dosThema,
};
