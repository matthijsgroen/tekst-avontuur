import { StateCondition } from "./ast-types";

export type WorldObjectSettings = {
  states?: unknown;
  flags?: unknown;
  counters?: unknown;
};

export type GameWorld = {
  locations: Record<string, WorldObjectSettings>;
  characters: Record<string, WorldObjectSettings>;
  items: Record<string, WorldObjectSettings>;
};

export type Script = () => void;

export type LocationScript<Game extends GameWorld> = (events: {
  onEnter: (from: keyof Game["locations"], script: Script) => void;
  onLeave: (from: keyof Game["locations"], script: Script) => void;
  describe: (script: () => void) => void;
  interaction: Interaction<Game>;
}) => void;

export type EvaluateCondition<Game extends GameWorld> = (
  condition: StateCondition<Game>,
  script: Script,
  elseScript?: Script
) => void;

export type Interaction<Game extends GameWorld> = (
  text: string,
  condition: StateCondition<Game>,
  script: Script
) => void;
