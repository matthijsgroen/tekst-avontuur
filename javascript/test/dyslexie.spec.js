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
  const open = [[L, "o"], [R, "p"], [E, "e"], [R, "n"]];
  const gedoe = [[R, "g"], [E, "e"], [R, "d"], [T, "oe"]];
  const getver = [[R, "g"], [K, "e"], [R, "t"], [R, "v"], [E, "e"], [R, "r"]];
  const geluk = [[R, "g"], [E, "e"], [R, "l"], [K, "u"], [R, "k"]];

  [
    teruggevonden,
    boerderij,
    vertellen,
    veren,
    open,
    gedoe,
    getver,
    geluk
  ].forEach(resultaat => {
    const woord = resultaat.reduce((r, e) => r + e[1], "");

    it(`de juiste klanken voor "${woord}"`, () => {
      expect(voegKlassificatiesToe(woord)).to.eql(resultaat);
    });
  });
});
