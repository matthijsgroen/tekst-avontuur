export type WorldObjectSettings = {
  states: unknown;
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
}) => void;

export type ItemStateCondition<Game extends GameWorld> = <
  K extends keyof Game["items"]
>(
  item: K,
  state: Game["items"][K]["states"] | "unknown",
  script: Script
) => void;

export type Settings<Game extends GameWorld> = {
  defaultLocale: `${string}-${string}`;
  startLocation: keyof Game["locations"];
};
