const { expect } = require("chai");
const { voegKlassificatiesToe } = require("../dyslexie");

const R = "rest";
const E = "stommeE";
const K = "korteKlinker";
const L = "langeKlinker";
const O = "letterGroep1";
const W = "letterGroep2";
const T = "tweeKlank";
const S = "speciaal";
const A = "anders";

describe("Dyslexie", () => {
  const teruggevonden = [
    [R, "t"],
    [E, "e"],
    [R, "r"],
    [K, "u"],
    [R, "g"],
    [R, "g"],
    [E, "e"],
    [R, "v"],
    [K, "o"],
    [R, "n"],
    [R, "d"],
    [E, "e"],
    [R, "n"]
  ];
  const boerderij = [
    [R, "b"],
    [T, "oe"],
    [R, "r"],
    [R, "d"],
    [E, "e"],
    [R, "r"],
    [T, "ij"]
  ];
  const vertellen = [
    [R, "v"],
    [E, "e"],
    [R, "r"],
    [R, "t"],
    [K, "e"],
    [R, "l"],
    [R, "l"],
    [E, "e"],
    [R, "n"]
  ];
  const veren = [[R, "v"], [L, "e"], [R, "r"], [E, "e"], [R, "n"]];
  const beren = [[R, "b"], [L, "e"], [R, "r"], [E, "e"], [R, "n"]];
  const open = [[L, "o"], [R, "p"], [E, "e"], [R, "n"]];
  const gedoe = [[R, "g"], [E, "e"], [R, "d"], [T, "oe"]];
  const getver = [[R, "g"], [K, "e"], [R, "t"], [R, "v"], [E, "e"], [R, "r"]];
  const geluk = [[R, "g"], [E, "e"], [R, "l"], [K, "u"], [R, "k"]];
  const eten = [[L, "e"], [R, "t"], [E, "e"], [R, "n"], [A, "."]];
  const dromerige = [
    [R, "d"],
    [R, "r"],
    [L, "o"],
    [R, "m"],
    [E, "e"],
    [R, "r"],
    [E, "i"],
    [R, "g"],
    [E, "e"]
  ];
  const medicijnen = [
    [R, "m"],
    [L, "e"],
    [R, "d"],
    [S, "i"],
    [S, "c"],
    [T, "ij"],
    [R, "n"],
    [E, "e"],
    [R, "n"]
  ];
  const medebewoner = [
    [R, "m"],
    [L, "e"],
    [R, "d"],
    [E, "e"],
    [R, "b"],
    [E, "e"],
    [R, "w"],
    [L, "o"],
    [R, "n"],
    [E, "e"],
    [R, "r"]
  ];
  const medaille = [
    [R, "m"],
    [E, "e"],
    [R, "d"],
    [K, "a"],
    [K, "i"],
    [R, "l"],
    [R, "l"],
    [E, "e"]
  ];
  const meteen = [[R, "m"], [E, "e"], [R, "t"], [L, "ee"], [R, "n"]];
  const meten = [[R, "m"], [L, "e"], [R, "t"], [E, "e"], [R, "n"]];
  const messen = [[R, "m"], [K, "e"], [R, "s"], [R, "s"], [E, "e"], [R, "n"]];
  const snel = [[R, "s"], [R, "n"], [K, "e"], [R, "l"]];
  const avontuur = [
    [L, "A"],
    [R, "v"],
    [K, "o"],
    [R, "n"],
    [R, "t"],
    [L, "uu"],
    [R, "r"]
  ];
  const repareren = [
    [R, "r"],
    [L, "e"],
    [R, "p"],
    [L, "a"],
    [R, "r"],
    [L, "e"],
    [R, "r"],
    [E, "e"],
    [R, "n"]
  ];
  const verwerkte = [
    [R, "v"],
    [E, "e"],
    [R, "r"],
    [R, "w"],
    [K, "e"],
    [R, "r"],
    [R, "k"],
    [R, "t"],
    [E, "e"]
  ];
  const gaaaaap = [[R, "g"], [L, "aaaaa"], [R, "p"]];
  const molenaar = [
    [R, "m"],
    [L, "o"],
    [R, "l"],
    [E, "e"],
    [R, "n"],
    [L, "aa"],
    [R, "r"]
  ];
  const molensteen = [
    [R, "m"],
    [L, "o"],
    [R, "l"],
    [E, "e"],
    [R, "n"],
    [R, "s"],
    [R, "t"],
    [L, "ee"],
    [R, "n"]
  ];
  const overal = [[L, "O"], [R, "v"], [E, "e"], [R, "r"], [K, "a"], [R, "l"]];
  const heuvels = [[R, "h"], [T, "eu"], [R, "v"], [E, "e"], [R, "l"], [R, "s"]];
  const omgeving = [
    [K, "o"],
    [R, "m"],
    [R, "g"],
    [L, "e"],
    [R, "v"],
    [K, "i"],
    [R, "ng"]
  ];
  const omgetikt = [
    [K, "o"],
    [R, "m"],
    [R, "g"],
    [E, "e"],
    [R, "t"],
    [K, "i"],
    [R, "k"],
    [R, "t"]
  ];
  const vergelijk = [
    [R, "v"],
    [E, "e"],
    [R, "r"],
    [R, "g"],
    [E, "e"],
    [R, "l"],
    [T, "ij"],
    [R, "k"]
  ];
  const ongelijk = [
    [K, "o"],
    [R, "n"],
    [R, "g"],
    [E, "e"],
    [R, "l"],
    [T, "ij"],
    [R, "k"]
  ];
  const gelijk = [[R, "g"], [E, "e"], [R, "l"], [T, "ij"], [R, "k"]];
  const ongeldig = [
    [K, "o"],
    [R, "n"],
    [R, "g"],
    [E, "e"],
    [R, "l"],
    [R, "d"],
    [E, "i"],
    [R, "g"]
  ];
  const onzeker = [
    [K, "o"],
    [R, "n"],
    [R, "z"],
    [L, "e"],
    [R, "k"],
    [E, "e"],
    [R, "r"]
  ];
  const onmogelijk = [
    [K, "o"],
    [R, "n"],
    [R, "m"],
    [L, "o"],
    [R, "g"],
    [E, "e"],
    [R, "l"],
    [E, "ij"],
    [R, "k"]
  ];
  const mogelijk = [
    [R, "m"],
    [L, "o"],
    [R, "g"],
    [E, "e"],
    [R, "l"],
    [E, "ij"],
    [R, "k"]
  ];
  const speciale = [
    [R, "S"],
    [R, "p"],
    [L, "e"],
    [S, "c"],
    [S, "i"],
    [L, "a"],
    [R, "l"],
    [E, "e"]
  ];
  const opgegeten = [
    [K, "o"],
    [R, "p"],
    [R, "g"],
    [E, "e"],
    [R, "g"],
    [L, "e"],
    [R, "t"],
    [E, "e"],
    [R, "n"]
  ];
  const vergeten = [
    [R, "v"],
    [E, "e"],
    [R, "r"],
    [R, "g"],
    [L, "e"],
    [R, "t"],
    [E, "e"],
    [R, "n"]
  ];
  const meedenken = [
    [R, "m"],
    [L, "ee"],
    [R, "d"],
    [K, "e"],
    [R, "nk"],
    [E, "e"],
    [R, "n"]
  ];
  const olifant = [
    [L, "o"],
    [R, "l"],
    [S, "i"],
    [R, "f"],
    [K, "a"],
    [R, "n"],
    [R, "t"]
  ];
  const pakket = [[R, "p"], [K, "a"], [R, "k"], [R, "k"], [K, "e"], [R, "t"]];
  const stekelige = [
    [R, "s"],
    [R, "t"],
    [L, "e"],
    [R, "k"],
    [E, "e"],
    [R, "l"],
    [E, "i"],
    [R, "g"],
    [E, "e"]
  ];
  const fantastisch = [
    [R, "f"],
    [K, "a"],
    [R, "n"],
    [R, "t"],
    [K, "a"],
    [R, "s"],
    [R, "t"],
    [S, "i"],
    [S, "sch"]
  ];
  const knipoog = [[R, "k"], [R, "n"], [K, "i"], [R, "p"], [L, "oo"], [R, "g"]];
  const linkerkant = [
    [R, "l"],
    [K, "i"],
    [R, "nk"],
    [E, "e"],
    [R, "r"],
    [R, "k"],
    [K, "a"],
    [R, "n"],
    [R, "t"]
  ];
  const bakkerij = [
    [R, "b"],
    [K, "a"],
    [R, "k"],
    [R, "k"],
    [E, "e"],
    [R, "r"],
    [T, "ij"]
  ];
  const donkere = [[R, "d"], [K, "o"], [R, "nk"], [E, "e"], [R, "r"], [E, "e"]];

  [
    avontuur,
    bakkerij,
    beren,
    boerderij,
    donkere,
    dromerige,
    eten,
    fantastisch,
    gaaaaap,
    gedoe,
    gelijk,
    geluk,
    getver,
    heuvels,
    knipoog,
    linkerkant,
    medaille,
    medebewoner,
    medicijnen,
    meedenken,
    messen,
    meteen,
    meten,
    mogelijk,
    molenaar,
    molensteen,
    olifant,
    omgetikt,
    omgeving,
    ongelijk,
    onmogelijk,
    onzeker,
    open,
    opgegeten,
    overal,
    pakket,
    repareren,
    snel,
    speciale,
    stekelige,
    teruggevonden,
    veren,
    vergelijk,
    vergeten,
    vertellen,
    verwerkte
  ].forEach(resultaat => {
    const woord = resultaat.reduce((r, e) => r + e[1], "");

    it(`de juiste klanken voor "${woord}"`, () => {
      expect(voegKlassificatiesToe(woord)).to.eql(resultaat);
    });
  });

  describe("G als een J", () => {
    ["gelij", "gel", "logeren", "horloge", "garage", "collage"].forEach(
      woord => {
        it(`gebruikt een G als J in ${woord}`, () => {
          const resultaat = voegKlassificatiesToe(woord);
          expect(resultaat).to.deep.include(["speciaal", "g"]);
        });
      }
    );

    ["hoger", "tegen", "ogen", "vlagen"].forEach(woord => {
      it(`gebruikt een G niet als J in ${woord}`, () => {
        const resultaat = voegKlassificatiesToe(woord);
        expect(resultaat).not.to.deep.include(["speciaal", "g"]);
      });
    });
  });
});
