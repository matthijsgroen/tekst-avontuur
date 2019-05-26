const kaftKleuren = {
  rood: ["0", "80%", "50%"],
  groen: ["120", "50%", "30%"],
  blauw: ["240", "30%", "40%"],
  paars: ["300", "60%", "30%"]
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
    eindSpel: "eindSpel"
  },
  opties: ({ kaft = "rood" }) => ({
    css: kaftKleur(kaftKleuren[kaft])
  })
};

const dosThema = {
  css: "thema/dos.css",
  html: "thema/dos.html",
  javascript: "thema/dos.js",
  haken: {
    startSpel: "startSpel",
    startLus: "cls"
  },
  opties: () => ({})
};

module.exports = {
  boek: boekThema,
  dos: dosThema
};
