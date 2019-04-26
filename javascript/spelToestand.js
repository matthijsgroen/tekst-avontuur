const toets = (spelToestand, plek, bewerking, waarde) =>
  (bewerking === "=" && spelToestand[plek] === waarde) ||
  (bewerking === "!" && spelToestand[plek] !== waarde) ||
  (bewerking === ">" && spelToestand[plek] > waarde) ||
  (bewerking === "<" && spelToestand[plek] < waarde);

const toegestaan = (spelToestand, bewering) => {
  if (bewering === "END") return false;

  let plek = "";
  let bewerking = "";
  let waarde = "";
  for (let i = 0; i < bewering.length; i++) {
    const karakter = bewering[i];
    if (/\d/.test(karakter)) {
      bewerking === "" ? (plek += karakter) : (waarde += karakter);
    } else if (karakter === ";") {
      if (
        !toets(
          spelToestand,
          parseInt(plek, 10),
          bewerking,
          parseInt(waarde, 10)
        )
      ) {
        return false;
      }
      plek = "";
      waarde = "";
      bewerking = "";
    } else {
      bewerking += karakter;
    }
  }

  if (bewerking !== "") {
    return toets(
      spelToestand,
      parseInt(plek, 10),
      bewerking,
      parseInt(waarde, 10)
    );
  }

  return true;
};

const muteer = (spelToestand, plek, bewerking, waarde) => {
  if (bewerking === "=") {
    spelToestand[plek] = waarde;
  }
  if (bewerking === "+") {
    spelToestand[plek] += waarde;
  }
  if (bewerking === "-") {
    spelToestand[plek] -= waarde;
  }
};

const voerActieUit = (spelToestand, actie) => {
  let plek = "";
  let bewerking = "";
  let waarde = "";
  for (let i = 0; i < actie.length; i++) {
    const karakter = actie[i];
    if (/\d/.test(karakter)) {
      bewerking === "" ? (plek += karakter) : (waarde += karakter);
    } else if (karakter === ";") {
      muteer(spelToestand, parseInt(plek, 10), bewerking, parseInt(waarde, 10));
      plek = "";
      waarde = "";
      bewerking = "";
    } else {
      bewerking += karakter;
    }
  }

  if (bewerking !== "") {
    muteer(spelToestand, parseInt(plek, 10), bewerking, parseInt(waarde, 10));
  }
};
module.exports = { toegestaan, voerActieUit };
