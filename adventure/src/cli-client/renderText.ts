import { FormattedText } from "../engine/text/types";
import { getSettings } from "./settings";
import { resetStyling, setStyling, TextStyling } from "./utils";

export const renderText = (
  text: FormattedText,
  cpm: number,
  styling: TextStyling,
  addNewline = true
) => {
  if (getSettings().color) {
    setStyling(styling);
  }
  for (const element of text) {
    if (element.type === "text") {
      process.stdout.write(element.text);
    }
    if (element.type === "formatting") {
      const newStyling = {
        ...styling,
      };
      if (element.format === "b") {
        newStyling.bold = true;
      }
      if (element.format === "u") {
        newStyling.underline = true;
      }
      if (element.format === "i") {
        newStyling.italic = true;
      }
      if (element.format === "s") {
        newStyling.strikeThrough = true;
      }
      renderText(element.contents, cpm, newStyling, false);
      if (getSettings().color) {
        resetStyling();
        setStyling(styling);
      }
    }
  }
  if (addNewline) {
    process.stdout.write("\n");
  }
};
