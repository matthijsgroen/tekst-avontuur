import { Settings } from "../dsl/ast-types";
import { TranslationFile } from "../export-translations/exportTranslations";

export type CLISettings = {
  /**
   * Wether to use colors in the TTY.
   * @default true
   */
  color?: boolean;

  /**
   * Translation file to use
   */
  translationData?: TranslationFile;
};

let settings: CLISettings = { color: true };

export const getSettings = () => settings;
export const updateSettings = (updates: Partial<CLISettings>) => {
  settings = { ...settings, ...updates };
};
