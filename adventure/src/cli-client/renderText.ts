import { HexColor } from "../engine/hexColor";
import { FormattedText } from "../engine/text/processText";
import { setColor } from "./utils";

export const renderText = (
  text: FormattedText,
  cpm: number,
  color?: HexColor,
  addNewline = true
) => {
  if (color) {
    setColor(color);
  }
  for (const element of text) {
    if (element.type === "text") {
      process.stdout.write(element.text);
    }
    if (element.type === "formatting") {
      renderText(element.contents, cpm, color, false);
    }
  }
  if (addNewline) {
    process.stdout.write("\n");
  }
};
