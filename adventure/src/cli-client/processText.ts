import { GameStateManager } from "../dsl/engine-types";
import { GameWorld } from "../dsl/world-types";
import { getSettings } from "./settings";

export const getTranslationText = (
  scope: string[],
  key: string
): string | undefined => {
  const translationData = getSettings().translationData;
  if (!translationData) return undefined;
  let t = translationData;
  for (const k of scope) {
    const v = t[k];
    if (typeof v !== "string") {
      t = v;
    } else {
      return undefined;
    }
  }

  const v = t[key];
  if (typeof v !== "string") {
    return undefined;
  }
  return v;
};

export const getDisplayText = <Game extends GameWorld>(
  sentence: string,
  stateManager: GameStateManager<Game>,
  scope: string[]
): string => {
  let renderSentence = getTranslationText(scope, sentence) || sentence;
  return renderSentence;
};
