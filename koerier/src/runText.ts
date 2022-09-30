import "./locaties";
import { convertGame, runCLIGame } from "point-n-click";
import { Game } from "./game";

convertGame<Game>(runCLIGame({ color: false }));
