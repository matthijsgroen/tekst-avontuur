import "./locaties";
import { convertGame } from "../dsl/dsl";
import { runGame } from "../cli-client/run";
import { Game } from "./game";

convertGame<Game>(runGame);
