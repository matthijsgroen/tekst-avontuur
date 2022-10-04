import { GameState } from "../engine/state/types";
import { HexColor } from "../engine/hexColor";
import { GameWorld } from "./world-types";

export type Settings<Game extends GameWorld> = {
  defaultLocale: `${string}-${string}`;
  initialState: Partial<GameState<Game>>;
  defaultTextColor?: HexColor;
  characterConfigs: Record<
    keyof Game["characters"],
    {
      defaultName: string;
      textColor?: HexColor;
    }
  >;
};

export type GameInteraction<Game extends GameWorld> = {
  label: string;
  condition: StateCondition<Game>;
  script: ScriptAST<Game>;
};

export type GameLocation<Game extends GameWorld> = {
  id: keyof Game["locations"];
  onEnter: { from: keyof Game["locations"]; script: ScriptAST<Game> }[];
  onLeave: { to: keyof Game["locations"]; script: ScriptAST<Game> }[];
  describe: { script: ScriptAST<Game> };
  interactions: GameInteraction<Game>[];
};

export type GameModel<Game extends GameWorld> = {
  settings: Settings<Game>;
  locations: GameLocation<Game>[];
};

export type ScriptAST<Game extends GameWorld> = ScriptStatement<Game>[];

export type ScriptStatement<Game extends GameWorld> =
  | TextStatement
  | TravelStatement<Game>
  | ConditionStatement<Game>
  | UpdateState<Game, "item">
  | UpdateState<Game, "location">
  | UpdateState<Game, "character">
  | UpdateCharacterName<Game>
  | CharacterSay<Game>
  | GameOverlay<Game>;

export type TextStatement = { statementType: "Text"; sentences: string[] };

export type GameOverlay<Game extends GameWorld> =
  | OpenGameOverlay<Game>
  | CloseGameOverlay<Game>;

export type OpenGameOverlay<Game extends GameWorld> = {
  statementType: "OpenOverlay";
  overlayId: string;
  onStart: { script: ScriptAST<Game> };
  onEnd: { script: ScriptAST<Game> };
  interactions: GameInteraction<Game>[];
};

export type CloseGameOverlay<Game extends GameWorld> = {
  statementType: "CloseOverlay";
  overlayId: string;
};

export type TravelStatement<Game extends GameWorld> = {
  statementType: "Travel";
  destination: keyof Game["locations"];
};
export type ConditionStatement<Game extends GameWorld> = {
  statementType: "Condition";
  condition: StateCondition<Game>;
  body: ScriptAST<Game>;
  elseBody: ScriptAST<Game>;
};

export type UpdateState<
  Game extends GameWorld,
  ItemType extends "item" | "location" | "character"
> =
  | {
      statementType: `Update${Capitalize<ItemType>}State`;
      stateItem: keyof Game[`${ItemType}s`];
      newState: Game[`${ItemType}s`][keyof Game[`${ItemType}s`]]["states"];
    }
  | {
      statementType: `Update${Capitalize<ItemType>}Flag`;
      stateItem: keyof Game[`${ItemType}s`];
      flag: Game[`${ItemType}s`][keyof Game[`${ItemType}s`]]["flags"];
      value: boolean;
    };

export type UpdateCharacterName<Game extends GameWorld> = {
  statementType: `UpdateCharacterName`;
  character: keyof Game["characters"];
  newName: string | null;
};

export type CharacterSay<Game extends GameWorld> = {
  statementType: "CharacterSay";
  character: keyof Game["characters"];
  sentences: string[];
};

export type StateCondition<Game extends GameWorld> =
  | GameObjectStateCondition<Game, "item">
  | GameObjectStateCondition<Game, "location">
  | GameObjectStateCondition<Game, "character">
  | GameObjectFlagCondition<Game, "item">
  | GameObjectFlagCondition<Game, "location">
  | GameObjectFlagCondition<Game, "character">
  | TrueCondition
  | FalseCondition
  | NegateCondition<Game>
  | AndCondition<Game>
  | OrCondition<Game>;

export type NegateCondition<Game extends GameWorld> = {
  op: "negate";
  condition: StateCondition<Game>;
};

export type AndCondition<Game extends GameWorld> = {
  op: "and";
  conditions: StateCondition<Game>[];
};

export type OrCondition<Game extends GameWorld> = {
  op: "or";
  conditions: StateCondition<Game>[];
};

export type GameObjectStateCondition<
  Game extends GameWorld,
  ItemType extends "item" | "location" | "character"
> = {
  op: `${ItemType}Equals`;
  item: keyof Game[`${ItemType}s`];
  state: Game[`${ItemType}s`][keyof Game[`${ItemType}s`]]["states"] | "unknown";
};
export type GameObjectFlagCondition<
  Game extends GameWorld,
  ItemType extends "item" | "location" | "character"
> = {
  op: `${ItemType}FlagSet`;
  item: keyof Game[`${ItemType}s`];
  flag: Game[`${ItemType}s`][keyof Game[`${ItemType}s`]]["flags"];
};

export type TrueCondition = {
  op: "true";
};

export type FalseCondition = {
  op: "false";
};
