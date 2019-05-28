const { basisNaam, laadSpel } = require("./bewaarSpel");

// prettier-ignore
const karakters = [
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
  "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5",
  "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
  "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "!", "@",
  "$", "%", "^", "&", "*", "(", ")", ",", ".", ";", ":", '"', "'", "+", "=", "/"
];

const encode = spelStatus => {
  const relevanteData = spelStatus.reduce(
    (result, item, index) =>
      item === 0 ? result : { ...result, [index]: item },
    {}
  );
  const grootsteSleutelDelta = spelStatus
    .map((item, index) => (item > 0 ? index : null))
    .filter(index => index !== null)
    .map((index, i, list) => index - (list[i - 1] || -1) - 1)
    .reduce((currentMax, item) => (item > currentMax ? item : currentMax), 0);
  const grootsteWaarde = spelStatus.reduce(
    (currentMax, item) => (item > currentMax ? item : currentMax),
    0
  );
  const bitSizeKey = grootsteSleutelDelta.toString(2).length;
  const largeValueSize = grootsteWaarde.toString(2).length;

  const predictedWidth = Object.entries(relevanteData).reduce(
    (total, [key, value]) => total + bitSizeKey + largeValueSize,
    0
  );
  const toBits = (number, size) =>
    ("0".repeat(size) + number.toString(2)).slice(-size);

  const pairs = Object.entries(relevanteData);
  const bitStream =
    toBits(pairs.length, 7) +
    pairs
      .map(([key, value], index, list) => {
        const delta = index > 0 ? key - list[index - 1][0] - 1 : key;
        return toBits(delta, bitSizeKey) + toBits(value, largeValueSize);
      })
      .join("");

  const paddedBitStream = bitStream + "0".repeat(bitStream.length % 6);

  let code = "" + karakters[bitSizeKey];
  for (let i = 0; i < paddedBitStream.length; i += 6) {
    const karakterIndex = parseInt(paddedBitStream.slice(i, i + 6), 2);
    code += karakters[karakterIndex];
  }
  code += karakters[largeValueSize];

  return code;
};

const decode = code => {
  const charToNum = char => karakters.indexOf(char);

  const bitSizeKey = charToNum(code[0]);
  const largeValueSizeKey = charToNum(code.slice(-1));
  const bitStream = code
    .slice(1, -1)
    .split("")
    .map(char => ("0".repeat(6) + charToNum(char).toString(2)).slice(-6))
    .join("");
  const amountValues = parseInt(bitStream.slice(0, 7), 2);
  let gameState = Array(100).fill(0);

  let currentIndex = 0;
  for (let i = 0; i < amountValues; i++) {
    const bitIndex = 7 + (bitSizeKey + largeValueSizeKey) * i;
    const keyDelta = parseInt(
      bitStream.slice(bitIndex, bitIndex + bitSizeKey),
      2
    );
    const bitValue = bitStream.slice(
      bitIndex + bitSizeKey,
      bitIndex + bitSizeKey + largeValueSizeKey
    );
    const value = parseInt(bitValue, 2);
    currentIndex += i > 0 ? keyDelta + 1 : keyDelta;
    gameState[currentIndex] = value;
  }
  return gameState;
};

const genereerCode = async bestandsnaam => {
  const opslag = `.${basisNaam(bestandsnaam)}.opslag`;
  const data = await laadSpel(opslag);
  const spelStatus = data.spelToestand;

  return encode(spelStatus);
};

module.exports = {
  genereerCode,
  encode,
  decode
};
