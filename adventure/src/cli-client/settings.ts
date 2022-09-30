import { Settings } from "../dsl/ast-types";

export type CLISettings = {
  /**
   * Wether to use colors in the TTY.
   * @default true
   */
  color?: boolean;
};

let settings: CLISettings = { color: true };

export const getSettings = () => settings;
export const updateSettings = (updates: Partial<CLISettings>) => {
  settings = { ...settings, ...updates };
};
