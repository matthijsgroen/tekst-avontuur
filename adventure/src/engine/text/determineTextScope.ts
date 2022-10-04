import { GameWorld } from "../../dsl/world-types";
import { GameStateManager } from "../state/types";

export const determineTextScope = <Game extends GameWorld>(
  stateManager: GameStateManager<Game>,
  entry: string
): string[] => {
  const overlay = stateManager.getState().overlayStack.at(-1);
  if (overlay) {
    return ["overlays", overlay, entry];
  }
  const location = String(stateManager.getState().currentLocation);
  return ["location", location, entry];
};
