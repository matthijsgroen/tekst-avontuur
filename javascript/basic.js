const sleep = duration =>
  new Promise(resolve => setTimeout(resolve, duration * 1e3));

// https://en.wikipedia.org/wiki/ANSI_escape_code
const color = index =>
  process.stdout.write(
    [
      "\u001b[30m", // 0 = black
      "\u001b[34m", // 1 = blue
      "\u001b[32m", // 2 = green
      "\u001b[36m", // 3 = cyan
      "\u001b[31m", // 4 = red
      "\u001b[35m", // 5 = magenta
      "\u001b[33m", // 6 = yellow
      "\u001b[37m", // 7 = white
      "\u001b[90m", // 8 = grey
      "\u001b[94m", // 9 = bright blue
      "\u001b[92m", // 10 = bright green
      "\u001b[96m", // 11 = bright cyan
      "\u001b[91m", // 12 = bright red
      "\u001b[95m", // 13 = bright magenta
      "\u001b[93m", // 14 = bright yellow
      "\u001b[97m" // 15 = bright white
    ][index]
  );

const print = tekst => process.stdout.write(tekst);
const cls = () => process.stdout.write("\x1Bc");

module.exports = { sleep, color, print, cls };
