const { basisNaam, laadSpel } = require("./bewaarSpel");

// prettier-ignore
const karakters = [
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
  "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5",
  "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
  "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "!", "@",
  "#", "$", "%", "^", "&", "*", "(", ")", ",", ".", ";", ":", '"', "'", "+", "="
];

const genereerCode = async bestandsnaam => {
  const opslag = `.${basisNaam(bestandsnaam)}.opslag`;
  const data = await laadSpel(opslag);
  const spelStatus = data.spelToestand;
  //console.log(data);
  const relevanteData = spelStatus.reduce(
    (result, item, index) =>
      item === 0 ? result : { ...result, [index]: item },
    {}
  );
  const grootsteSleutelDelta = spelStatus
    .map((item, index) => (item > 0 ? index : null))
    .filter(index => index !== null)
    .map((index, i, list) => index - (list[i - 1] || 0))
    .reduce((currentMax, item) => (item > currentMax ? item : currentMax), 0);
  const grootsteWaarde = spelStatus.reduce(
    (currentMax, item) => (item > currentMax ? item : currentMax),
    0
  );
  const bitSizeKey = grootsteSleutelDelta.toString(2).length;
  const largeValueSize = grootsteWaarde.toString(2).length;

  console.log(karakters.length);
  console.log(bitSizeKey, largeValueSize);
  const predictedWidth = Object.entries(relevanteData).reduce(
    (total, [key, value]) => total + bitSizeKey + largeValueSize,
    0
  );
  console.log(predictedWidth / 6);

  console.log("Code is: ", "NOG-N!3T");
};

module.exports = genereerCode;
