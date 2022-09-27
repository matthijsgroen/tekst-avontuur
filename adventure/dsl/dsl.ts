import type {
  FalseCondition,
  GameLocation,
  GameModel,
  GameObjectStateCondition,
  NegateCondition,
  ScriptAST,
  Settings,
  StateCondition,
  TrueCondition,
} from "./ast-types";
import type {
  GameWorld,
  LocationScript,
  EvaluateCondition,
} from "./world-types";

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
    defineLocation: (
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
      setFlag: (flag: Game["items"][I]["flags"], value: boolean) => {
        activeScriptScope.push({
          statementType: "UpdateItemFlag",
          stateItem: item,
          flag,
          value,
        });
      },
    }),
    location: <I extends keyof Game["locations"]>(location: I) => ({
      setState: (newState: Game["locations"][I]["states"]) => {
        activeScriptScope.push({
          statementType: "UpdateLocationState",
          stateItem: location,
          newState,
        });
      },
      setFlag: (flag: Game["locations"][I]["flags"], value: boolean) => {
        activeScriptScope.push({
          statementType: "UpdateLocationFlag",
          stateItem: location,
          flag,
          value,
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
