const { expect } = require("chai");
const { voegKlassificatiesToe } = require("../dyslexie");

const R = "rest";
const E = "stommeE";
const K = "korteKlinker";
const L = "langeKlinker";
const W = "letterGroep";
const T = "tweeKlank";
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
  const medicijnen = [
    [R, "m"],
    [L, "e"],
    [R, "d"],
    [K, "i"],
    [R, "c"],
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

  [
    avontuur,
    beren,
    boerderij,
    eten,
    gedoe,
    geluk,
    getver,
    medaille,
    medebewoner,
    medicijnen,
    messen,
    meteen,
    meten,
    open,
    repareren,
    snel,
    teruggevonden,
    veren,
    vertellen,
    verwerkte,
    gaaaaap,
    molenaar,
    molensteen,
    overal,
    heuvels,
    omgeving,
    omgetikt,
    ongelijk,
    vergelijk,
    gelijk,
    onmogelijk,
    onzeker
  ].forEach(resultaat => {
    const woord = resultaat.reduce((r, e) => r + e[1], "");

    it(`de juiste klanken voor "${woord}"`, () => {
      expect(voegKlassificatiesToe(woord)).to.eql(resultaat);
    });
  });
});
