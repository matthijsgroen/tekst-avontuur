import { GameModel } from "../dsl/ast-types";
import { GameStateManager } from "../engine/state/types";
import { GameWorld } from "../dsl/world-types";
import { runScript } from "./runScript";
import { exitGame } from "./utils";

export const describeLocation = async <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  const currentLocation = stateManager.getState().currentLocation;
  const locationData = gameModel.locations.find(
    (l) => l.id === currentLocation
  );
  if (!locationData) {
    console.log(`Location not found: ${String(currentLocation)}`);
    exitGame(1);
  }

  const previousLocation = stateManager.getState().previousLocation;
  if (currentLocation !== previousLocation) {
    const enterScript = locationData?.onEnter.find(
      (item) => item.from === previousLocation
    );
    if (enterScript) {
      await runScript<Game>(enterScript.script, gameModel, stateManager);
      stateManager.updateState((state) => ({
        ...state,
        previousLocation: currentLocation,
      }));
    }
  }

  await runScript<Game>(
    locationData?.describe.script || [],
    gameModel,
    stateManager
  );
};
