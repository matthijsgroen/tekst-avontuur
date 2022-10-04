import { GameModel } from "../dsl/ast-types";
import { GameStateManager } from "../engine/state/types";
import { GameWorld } from "../dsl/world-types";
import { describeLocation } from "./describeLocation";
import { handleInteractions } from "./handleInteractions";
import { runScript } from "./runScript";

export const runLocation = async <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  await describeLocation(gameModel, stateManager);

  const currentLocation = stateManager.getState().currentLocation;
  const locationData = gameModel.locations.find(
    (l) => l.id === currentLocation
  );
  while (stateManager.getState().currentLocation === currentLocation) {
    await handleInteractions(
      locationData?.interactions || [],
      gameModel,
      stateManager
    );
  }
  const newLocation = stateManager.getState().currentLocation;
  const exitScript = locationData?.onLeave.find(
    (item) => item.to === newLocation
  );
  if (exitScript) {
    await runScript<Game>(exitScript.script, gameModel, stateManager);
  }
};
