const { basisNaam, laadSpel } = require("./bewaarSpel");

const karakters =
  "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!@" +
  "$%^~*(),.;:\"'+-/[]{}_`<>?îêôâëüïöäøéíóáØÊÎÔÂËÜÖÏÉÚÍÓÁåÅßçÇàèìòùÀ";

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

  const paddedBitStream = bitStream + "0".repeat(bitStream.length % 7);
  const amountOnes = paddedBitStream
    .split("")
    .reduce((result, item) => (item === "1" ? result + 1 : result), 0);

  let code = "" + karakters[bitSizeKey];
  for (let i = 0; i < paddedBitStream.length; i += 7) {
    const karakterIndex = parseInt(paddedBitStream.slice(i, i + 7), 2);
    code += karakters[karakterIndex];
  }
  code += karakters[largeValueSize] + karakters[amountOnes];

  return code;
};

const decode = code => {
  const charToNum = char => karakters.indexOf(char);

  const bitSizeKey = charToNum(code[0]);
  const largeValueSizeKey = charToNum(code.slice(-2, -1));
  const verifyOnes = charToNum(code.slice(-1));
  const bitStream = code
    .slice(1, -2)
    .split("")
    .map(char => ("0".repeat(7) + charToNum(char).toString(2)).slice(-7))
    .join("");

  const amountOnes = bitStream
    .split("")
    .reduce((result, bit) => (bit === "1" ? result + 1 : result), 0);

  if (verifyOnes !== amountOnes) throw new Error("Ongeldige code");

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
