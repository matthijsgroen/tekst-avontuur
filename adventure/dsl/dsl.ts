import type {
  GameWorld,
  LocationScript,
  Settings,
  EvaluateCondition,
  GameObjectStateCondition,
  NegateCondition,
  StateCondition,
  TrueCondition,
  FalseCondition,
} from "./world-types";

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

type GameLocation<Game extends GameWorld> = {
  id: keyof Game["locations"];
  onEnter: { from: keyof Game["locations"]; script: ScriptAST<Game> }[];
  onLeave: { to: keyof Game["locations"]; script: ScriptAST<Game> }[];
  describe: { script: ScriptAST<Game> };
  interactions: {
    label: string;
    condition: StateCondition<Game>;
    script: ScriptAST<Game>;
  }[];
};

export type GameModel<Game extends GameWorld> = {
  settings: Settings<Game>;
  locations: GameLocation<Game>[];
};

let worldModel: GameModel<GameWorld> | undefined = undefined;

const always = (): TrueCondition => ({ op: "true" });
const never = (): FalseCondition => ({ op: "false" });

export const world = <Game extends GameWorld>(settings: Settings<Game>) => {
  worldModel = {
    settings: settings as Settings<GameWorld>,
    locations: [],
  };

  let activeScriptScope: ScriptAST<Game> = [];

  const wrapScript = (execution: () => void): ScriptAST<Game> => {
    const previousScript = activeScriptScope;
    const script: ScriptAST<Game> = [];

    activeScriptScope = script;
    execution();
    const result = activeScriptScope;
    activeScriptScope = previousScript;
    return result;
  };

  const onState: EvaluateCondition<Game> = (condition, script, elseScript) => {
    const body = wrapScript(script);
    const elseBody = elseScript ? wrapScript(elseScript) : [];
    activeScriptScope.push({
      statementType: "Condition",
      condition,
      body,
      elseBody,
    });
  };
  const not = (condition: StateCondition<Game>): NegateCondition<Game> => ({
    op: "negate",
    condition,
  });
  const isItemState = <K extends keyof Game["items"]>(
    item: K,
    state: Game["items"][K]["states"] | "unknown"
  ): GameObjectStateCondition<Game, "item"> => ({
    op: "itemEquals",
    item,
    state,
  });

  return {
    location: (
      location: keyof Game["locations"],
      script: LocationScript<Game>
    ) => {
      const locationAST: GameLocation<Game> = {
        id: location,
        describe: { script: [] },
        onEnter: [],
        onLeave: [],
        interactions: [],
      };
      script({
        describe: (script) => {
          const description = wrapScript(script);
          locationAST.describe = { script: description };
        },
        onEnter: (from: keyof Game["locations"], script) => {
          const enterScript = wrapScript(script);
          locationAST.onEnter.push({
            from,
            script: enterScript,
          });
        },
        onLeave: (to: keyof Game["locations"], script) => {
          const enterScript = wrapScript(script);
          locationAST.onLeave.push({
            to,
            script: enterScript,
          });
        },
        interaction: (label, condition, script) => {
          const interactionScript = wrapScript(script);
          locationAST.interactions.push({
            label,
            condition,
            script: interactionScript,
          });
        },
      });
      worldModel?.locations.push(
        locationAST as unknown as GameLocation<GameWorld>
      );
    },
    character: (character: keyof Game["characters"]) => ({
      say: (...sentences: string[]) => {
        activeScriptScope.push({
          statementType: "CharacterSay",
          character,
          sentences,
        });
      },
    }),
    item: <I extends keyof Game["items"]>(item: I) => ({
      setState: (newState: Game["items"][I]["states"]) => {
        activeScriptScope.push({
          statementType: "UpdateItemState",
          stateItem: item,
          newState,
        });
      },
    }),
    text: (...sentences: string[]) => {
      activeScriptScope.push({
        statementType: "Text",
        sentences,
      });
    },
    travel: (location: keyof Game["locations"]) => {
      activeScriptScope.push({
        statementType: "Travel",
        destination: String(location),
      });
    },
    onState,
    isItemState,
    not,
    always,
    never,
  };
};

export type GameConverter = <Game extends GameWorld>(
  model: GameModel<Game> | undefined
) => void;

export const convertGame = <Game extends GameWorld>(converter: GameConverter) =>
  converter(worldModel as unknown as GameModel<Game>);
