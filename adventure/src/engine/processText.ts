import { GameStateManager } from "./engine-types";
import { GameWorld } from "../dsl/world-types";
import { getSettings } from "../cli-client/settings";

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

type FormattingOptions = "bold" | "underline" | "italic";

type Text = {
  type: "text";
  text: string;
};

type TextFormatting<T> = {
  type: "formatting";
  formatting: FormattingOptions;
  contents: T;
};

type StateText = {
  type: "state";
  key: string;
};

type ParsedTextElement = Text | TextFormatting<ParsedText> | StateText;
export type ParsedText = ParsedTextElement[];

type FormattedTextElement = Text | TextFormatting<FormattedText>;
export type FormattedText = FormattedTextElement[];

const parseText = (text: string): ParsedText => {
  return [{ type: "text", text }];
};

export const getDisplayText = <Game extends GameWorld>(
  sentence: string,
  stateManager: GameStateManager<Game>,
  scope: string[]
): FormattedText => {
  const renderSentence = getTranslationText(scope, sentence) || sentence;
  // 1: Parse text
  const parsedText = parseText(renderSentence);
  // 2: Apply state
  // 3: Return formatted text
  return parsedText as FormattedText;
};
