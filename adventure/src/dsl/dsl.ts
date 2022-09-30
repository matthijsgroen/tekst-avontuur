import type {
  GameOverlay,
  GameLocation,
  GameModel,
  ScriptAST,
  Settings,
} from "./ast-types";
import { dslStateConditions } from "./dsl-conditions";
import type {
  GameWorld,
  LocationScript,
  EvaluateCondition,
  ConversationScript as OverlayScript,
} from "./world-types";

let worldModel: GameModel<GameWorld> | undefined = undefined;

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

  return {
    defineLocation: <Location extends keyof Game["locations"]>(
      location: Location,
      script: LocationScript<Game, Location>
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
        onEnter: (from: Exclude<keyof Game["locations"], Location>, script) => {
          const enterScript = wrapScript(script);
          locationAST.onEnter.push({
            from,
            script: enterScript,
          });
        },
        onLeave: (to: Exclude<keyof Game["locations"], Location>, script) => {
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
    overlay: (id: string, handleOverlay: OverlayScript<Game>) => {
      const overlayAST: GameOverlay<Game> = {
        statementType: "OpenOverlay",
        overlayId: id,
        onStart: { script: [] },
        onEnd: { script: [] },
        interactions: [],
      };

      handleOverlay({
        onStart: (script) => {
          const startScript = wrapScript(script);
          overlayAST.onStart.script = startScript;
        },
        onEnd: (script) => {
          const endScript = wrapScript(script);
          overlayAST.onEnd.script = endScript;
        },
        interaction: (label, condition, script) => {
          const interactionScript = wrapScript(script);
          overlayAST.interactions.push({
            label,
            condition,
            script: interactionScript,
          });
        },
        closeOverlay: () => {
          activeScriptScope.push({
            statementType: "CloseOverlay",
            overlayId: id,
          });
        },
      });
      activeScriptScope.push(overlayAST);
    },
    character: <I extends keyof Game["characters"]>(character: I) => ({
      say: (...sentences: string[]) => {
        activeScriptScope.push({
          statementType: "CharacterSay",
          character,
          sentences,
        });
      },
      setName: (newName: string) => {
        activeScriptScope.push({
          statementType: "UpdateCharacterName",
          character,
          newName,
        });
      },
      setState: (newState: Game["characters"][I]["states"]) => {
        activeScriptScope.push({
          statementType: "UpdateCharacterState",
          stateItem: character,
          newState,
        });
      },
      setFlag: (flag: Game["characters"][I]["flags"], value: boolean) => {
        activeScriptScope.push({
          statementType: "UpdateCharacterFlag",
          stateItem: character,
          flag,
          value,
        });
      },
      clearCustomName: () => {
        activeScriptScope.push({
          statementType: "UpdateCharacterName",
          character,
          newName: null,
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
    ...dslStateConditions<Game>(),
  };
};

export type GameConverter = <Game extends GameWorld>(
  model: GameModel<Game> | undefined
) => void;

export const convertGame = <Game extends GameWorld>(converter: GameConverter) =>
  converter(worldModel as unknown as GameModel<Game>);
