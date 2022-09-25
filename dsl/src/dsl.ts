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

type ScriptAST = ScriptStatement[];

type ScriptStatement = TextStatement | TravelStatement;

type TextStatement = { statementType: "Text"; sentences: string[] };
type TravelStatement = { statementType: "Travel"; destination: string };

type GameLocation<Game extends GameWorld> = {
  id: keyof Game["locations"];
  onEnter: { from: keyof Game["locations"]; script: ScriptAST }[];
  onLeave: { to: keyof Game["locations"]; script: ScriptAST }[];
  describe: { script: ScriptAST };
  interactions: {
    label: string;
    condition: StateCondition<Game>;
    script: ScriptAST;
  }[];
};

type GameModel<Game extends GameWorld> = {
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

  let activeScriptScope: ScriptAST = [];

  const wrapScript = (execution: () => void): ScriptAST => {
    const previousScript = activeScriptScope;
    const script: ScriptAST = [];

    activeScriptScope = script;
    execution();
    const result = activeScriptScope;
    activeScriptScope = previousScript;
    return result;
  };

  const onState: EvaluateCondition<Game> = () => {};
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
      say: (...sentences: string[]) => {},
    }),
    item: <I extends keyof Game["items"]>(item: I) => ({
      setState: (newState: Game["items"][I]["states"]) => {},
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

type GameConverter<Game extends GameWorld> = (
  model: GameModel<Game> | undefined
) => void;

export const convertGame = <Game extends GameWorld>(
  converter: GameConverter<Game>
) => converter(worldModel as unknown as GameModel<Game>);
