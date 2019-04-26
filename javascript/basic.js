const sleep = duration =>
  new Promise(resolve => setTimeout(resolve, duration * 1e3));

// https://en.wikipedia.org/wiki/ANSI_escape_code
const color = index =>
  process.stdout.write(
    [
      "\u001b[30m", // 0 = zwart
      "\u001b[34m", // 1 = blauw
      "\u001b[32m", // 2 = groen
      "\u001b[36m", // 3 = cyaan
      "\u001b[31m", // 4 = rood
      "\u001b[35m", // 5 = magenta
      "\u001b[38;5;202m", // 6 = yellow, de DOS versie (oranje)
      "\u001b[37m", // 7 = wit
      "\u001b[90m", // 8 = grijs
      "\u001b[94m", // 9 = fel blauw
      "\u001b[92m", // 10 = fel groen
      "\u001b[96m", // 11 = fel cyaan
      "\u001b[91m", // 12 = fel rood
      "\u001b[95m", // 13 = fel magenta
      "\u001b[93m", // 14 = fel geel
      "\u001b[97m" // 15 = fel wit
    ][index]
  );

const print = tekst => process.stdout.write(tekst);
const cls = () => process.stdout.write("\x1Bc");

module.exports = { sleep, color, print, cls };
