import { GameWorld } from "../../dsl/world-types";
import { GameStateManager } from "../state/types";
import { FormattedText, ParsedText } from "./types";

export const applyState = <Game extends GameWorld>(
  text: ParsedText,
  stateManager: GameStateManager<Game>,
  stateScope: string[]
): FormattedText => {
  const result: FormattedText = [];
  for (const element of text) {
    if (element.type === "text") {
      result.push(element);
    }
    if (element.type === "formatting") {
      result.push({
        ...element,
        contents: applyState(element.contents, stateManager, stateScope),
      });
    }
    if (element.type === "interpolation") {
      const statePath = element.value.split(".");
      const resolveStatePath =
        statePath[0] === ""
          ? [...stateScope].concat(statePath.slice(1))
          : statePath;

      const state = stateManager.getState();
      let value = `STATE NOT FOUND '${element.value}'`;

      if (resolveStatePath[0] === "character") {
        const character = resolveStatePath[1] as keyof Game["characters"];
        const property = resolveStatePath[2];
        if (property === "defaultName") {
          value = state.characters[character].defaultName;
        }
        if (property === "name") {
          value =
            state.characters[character].name ||
            state.characters[character].defaultName;
        }
      }
      result.push({
        type: "text",
        text: value,
      });
    }
  }
  return result;
};
