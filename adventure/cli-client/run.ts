import { GameModel, ScriptAST } from "../dsl/dsl";
import { GameState, GameWorld } from "../dsl/world-types";

const runScript = <Game extends GameWorld>(script: ScriptAST<Game>) => {
  console.log(script);
};

const runLocation = <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  gameState: GameState<Game>
) => {
  const locationData = gameModel.locations.find(
    (l) => l.id === gameState.currentLocation
  );

  runScript<Game>(locationData?.describe.script || []);
};

export const runGame = <Game extends GameWorld>(
  gameModel?: GameModel<Game>
) => {
  if (!gameModel) {
    console.log("No valid game file");
    process.exit(1);
    return;
  }

  const gameState: GameState<Game> = {
    ...gameModel.settings.initialState,
  };

  runLocation(gameModel, gameState);
};
