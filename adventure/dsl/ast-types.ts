import { GameState } from "./engine-types";
import { GameWorld } from "./world-types";

export type Settings<Game extends GameWorld> = {
  defaultLocale: `${string}-${string}`;
  initialState: Partial<GameState<Game>>;
  characterConfigs: Record<
    keyof Game["characters"],
    {
      defaultName: string;
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
  | UpdateStateItem<Game>
  | CharacterSay<Game>;

export type TextStatement = { statementType: "Text"; sentences: string[] };
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
export type UpdateStateItem<Game extends GameWorld> = {
  statementType: "UpdateItemState";
  stateItem: keyof Game["items"];
  newState: Game["items"][keyof Game["items"]]["states"];
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
