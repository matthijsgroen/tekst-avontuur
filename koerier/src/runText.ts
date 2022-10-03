import "./locaties";
import {
  convertGame,
  exportTranslations,
  Locale,
  runCLIGame,
  TranslationFile,
} from "point-n-click";
import { Game } from "./game";
import { readFile } from "fs/promises";
import { join } from "path";

const main = async (locale: Locale) => {
  let translationData: TranslationFile | undefined = undefined;
  try {
    const data = await readFile(
      join(__dirname, "translations", `${locale}.json`),
      { encoding: "utf-8" }
    );
    translationData = JSON.parse(data) as unknown as TranslationFile;
  } catch (e) {}

  convertGame<Game>(runCLIGame({ color: true, translationData }));
  // convertGame<Game>(
  //   exportTranslations(join(__dirname, "translations"), ["en-US"])
  // );
};

main("en-US");
// main("nl-NL");
