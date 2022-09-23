import {
  GameWorld,
  ItemStateCondition,
  LocationScript,
  Settings,
} from "./world-types";

type ScriptAST = ScriptStatement[];

type ScriptStatement = TextStatement;

type TextStatement = { statementType: "Text"; sentences: string[] };

type GameLocation<Game extends GameWorld> = {
  id: keyof Game["locations"];
  onEnter: { from: keyof Game["locations"]; script: ScriptAST }[];
  onLeave: { to: keyof Game["locations"]; script: ScriptAST }[];
  describe: { script: ScriptAST };
};

type GameModel<Game extends GameWorld> = {
  settings: Settings<Game>;
  locations: GameLocation<Game>[];
};

let worldModel: GameModel<GameWorld> | undefined = undefined;

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

  const ifItem: ItemStateCondition<Game> = () => {};
  const ifItemNot: ItemStateCondition<Game> = () => {};

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
      });
      worldModel?.locations.push(locationAST as GameLocation<GameWorld>);
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
    ifItem,
    ifItemNot,
  };
};

type GameConverter<Game extends GameWorld> = (
  model: GameModel<Game> | undefined
) => void;

export const convertGame = <Game extends GameWorld>(
  converter: GameConverter<Game>
) => converter(worldModel as GameModel<Game>);
