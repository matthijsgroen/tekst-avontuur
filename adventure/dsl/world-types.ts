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
  interaction: Interaction<Game>;
}) => void;

export type StateCondition<Game extends GameWorld> =
  | GameObjectStateCondition<Game, "item">
  | GameObjectStateCondition<Game, "location">
  | GameObjectStateCondition<Game, "character">
  | TrueCondition
  | FalseCondition
  | NegateCondition<Game>;

export type NegateCondition<Game extends GameWorld> = {
  op: "negate";
  condition: StateCondition<Game>;
};

export type GameObjectStateCondition<
  Game extends GameWorld,
  ItemType extends "item" | "location" | "character"
> = {
  op: `${ItemType}Equals`;
  item: keyof Game[`${ItemType}s`];
  state: Game[`${ItemType}s`][keyof Game[`${ItemType}s`]]["states"] | "unknown";
};

export type TrueCondition = {
  op: "true";
};

export type FalseCondition = {
  op: "false";
};

export type EvaluateCondition<Game extends GameWorld> = (
  condition: StateCondition<Game>,
  script: Script,
  elseScript?: Script
) => void;

export type GameState<Game extends GameWorld> = {
  currentLocation: keyof Game["locations"];
  items?: {
    [k in keyof Game["items"]]?: {
      state: Game["items"][k]["states"];
    };
  };
  characters?: {
    [k in keyof Game["characters"]]?: {
      state: Game["characters"][k]["states"];
    };
  };
  locations?: {
    [k in keyof Game["locations"]]?: {
      state: Game["locations"][k]["states"];
    };
  };
};

export type Settings<Game extends GameWorld> = {
  defaultLocale: `${string}-${string}`;
  initialState: GameState<Game>;
  characterConfigs: Record<
    keyof Game["characters"],
    {
      defaultName: string;
    }
  >;
};

export type Interaction<Game extends GameWorld> = (
  text: string,
  condition: StateCondition<Game>,
  script: Script
) => void;
