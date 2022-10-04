import { HexColor } from "../engine/hexColor";

export const cls = () => process.stdout.write("\x1Bc");

export type TextStyling = {
  color?: HexColor;
  underline?: boolean;
  bold?: boolean;
  strikeThrough?: boolean;
  italic?: boolean;
};

export const setColor = (color: HexColor) => {
  const red = parseInt(color.slice(0, 2), 16);
  const green = parseInt(color.slice(2, 4), 16);
  const blue = parseInt(color.slice(4, 6), 16);
  process.stdout.write(`\x1b[38;2;${red};${green};${blue}m`);
};
export const bold = () => process.stdout.write("\033[1m");
export const italic = () => process.stdout.write("\033[3m");
export const underline = () => process.stdout.write("\033[4m");
export const strikeThrough = () => process.stdout.write("\033[9m");

export const setStyling = (styling: TextStyling) => {
  const sequence: string[] = [];
  if (styling.bold) {
    sequence.push("1");
  }
  if (styling.italic) {
    sequence.push("3");
  }
  if (styling.underline) {
    sequence.push("4");
  }
  if (styling.strikeThrough) {
    sequence.push("9");
  }
  process.stdout.write("\033[" + sequence.join(";") + "m");

  if (styling.color) {
    setColor(styling.color);
  }
};

export const resetStyling = () => process.stdout.write("\033[0m");

export const exitGame = (code = 0) => {
  process.exit(code);
};

let keyPressed: (key: string) => void = () => {};
const stdin = process.stdin;

export const enableKeyPresses = () => {
  stdin.resume();
  stdin.setRawMode(true);
  stdin.setEncoding("utf8");

  stdin.on("data", function (key: string) {
    // ctrl-c ( end of text )
    if (key === "\u0003" || key === "\u001b") {
      exitGame();
    }
    // if (key === "\u0020") {
    //   skip = true;
    // }
    keyPressed(key);
  });
};

export const keypress = () =>
  new Promise<string>((resolve) => {
    keyPressed = (key: string) => {
      resolve(key);
    };
  });
