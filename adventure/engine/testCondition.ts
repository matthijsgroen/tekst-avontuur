import { StateCondition } from "../dsl/ast-types";
import { GameStateManager } from "../dsl/engine-types";
import { GameWorld } from "../dsl/world-types";

export const testCondition = <Game extends GameWorld>(
  condition: StateCondition<Game>,
  stateManager: GameStateManager<Game>
): boolean => {
  if (condition.op === "true") {
    return true;
  }
  if (condition.op === "false") {
    return false;
  }
  if (condition.op === "negate") {
    return !testCondition(condition.condition, stateManager);
  }
  if (condition.op === "itemEquals") {
    const item = condition.item;
    const expectedState = condition.state;
    const actualState = stateManager.getState().items[item]?.state ?? "unknown";
    return expectedState === actualState;
  }
  if (condition.op === "characterEquals") {
    const item = condition.item;
    const expectedState = condition.state;
    const actualState =
      stateManager.getState().characters[item]?.state ?? "unknown";
    return expectedState === actualState;
  }
  if (condition.op === "locationEquals") {
    const item = condition.item;
    const expectedState = condition.state;
    const actualState =
      stateManager.getState().locations[item]?.state ?? "unknown";
    return expectedState === actualState;
  }

  return true;
};
