import { GameStateManager } from "../state/types";
import { GameWorld } from "../../dsl/world-types";
import { getTranslationText } from "./getTranslationText";

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
