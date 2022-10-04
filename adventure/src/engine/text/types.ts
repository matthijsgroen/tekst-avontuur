export type Text = {
  type: "text";
  text: string;
};

export type TextFormatting<T> = {
  type: "formatting";
  format: string;
  value: string | null;
  contents: T;
};

export type StateText = {
  type: "interpolation";
  value: string;
};

type ParsedTextElement = Text | TextFormatting<ParsedText> | StateText;
export type ParsedText = ParsedTextElement[];

type FormattedTextElement = Text | TextFormatting<FormattedText>;
export type FormattedText = FormattedTextElement[];
