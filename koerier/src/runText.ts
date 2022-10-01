import "./locaties";
import { convertGame, exportTranslations, runCLIGame } from "point-n-click";
import { Game } from "./game";
import { join } from "path";

// convertGame<Game>(runCLIGame({ color: true }));
convertGame<Game>(
  exportTranslations(join(__dirname, "translations"), ["en-US"])
);
