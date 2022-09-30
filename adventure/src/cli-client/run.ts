import {
  ScriptStatement,
  ScriptAST,
  GameModel,
  GameInteraction,
} from "../dsl/ast-types";
import { GameStateManager, GameState } from "../dsl/engine-types";
import { GameWorld } from "../dsl/world-types";
import { testCondition } from "../dsl/testCondition";
import { createDefaultState } from "../dsl/createDefaultState";
import { produce } from "immer";

type StatementMap<Game extends GameWorld> = {
  [K in ScriptStatement<Game> as K["statementType"]]: (
    statement: K,
    gameModel: GameModel<Game>,
    stateManager: GameStateManager<Game>
  ) => Promise<void> | void;
};

const exitGame = (code = 0) => {
  process.exit(code);
};

const handleOverlay = async <Game extends GameWorld>(
  overlayId: string,
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>,
  startScript: ScriptAST<Game>,
  endScript: ScriptAST<Game>,
  interactions: GameInteraction<Game>[]
) => {
  stateManager.updateState((state) => ({
    ...state,
    overlayStack: state.overlayStack.concat(overlayId),
  }));

  await runScript(startScript, gameModel, stateManager);

  let currentOverlayId = stateManager.getState().overlayStack.slice(-1)[0];
  do {
    await handleInteractions(interactions || [], gameModel, stateManager);
    currentOverlayId = stateManager.getState().overlayStack.slice(-1)[0];
  } while (currentOverlayId === overlayId);

  await runScript(endScript, gameModel, stateManager);
};

const statementHandler = <
  Game extends GameWorld,
  K extends ScriptStatement<Game>
>(
  statementType: K["statementType"]
): ((
  statement: K,
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => Promise<void> | void) => {
  const statementMap: StatementMap<Game> = {
    Text: (statement) => {
      for (const sentence of statement.sentences) {
        console.log(sentence);
      }
      console.log("");
    },
    Travel: ({ destination }, _gameModel, stateManager) => {
      stateManager.updateState((state) => ({
        ...state,
        currentLocation: destination,
      }));
    },
    UpdateItemState: ({ stateItem, newState }, _gameModel, stateManager) => {
      stateManager.updateState(
        produce((draft) => {
          const item = (draft as GameState<Game>).items[stateItem];
          if (item) {
            item.state = newState;
          } else {
            (draft as GameState<Game>).items[stateItem] = {
              state: newState,
              flags: {},
            };
          }
        })
      );
    },
    UpdateItemFlag: ({ stateItem, flag, value }, _gameModel, stateManager) => {
      stateManager.updateState(
        produce((draft) => {
          const item = (draft as GameState<Game>).items[stateItem];
          if (item) {
            item.flags[String(flag)] = value;
          } else {
            (draft as GameState<Game>).items[stateItem] = {
              state: "unknown",
              flags: { [String(flag)]: value },
            };
          }
        })
      );
    },
    UpdateCharacterState: (
      { stateItem, newState },
      _gameModel,
      stateManager
    ) => {
      stateManager.updateState(
        produce((draft) => {
          (draft as GameState<Game>).characters[stateItem].state = newState;
        })
      );
    },
    UpdateCharacterFlag: (
      { stateItem, flag, value },
      _gameModel,
      stateManager
    ) => {
      stateManager.updateState(
        produce((draft) => {
          (draft as GameState<Game>).characters[stateItem].flags[String(flag)] =
            value;
        })
      );
    },
    UpdateCharacterName: ({ character, newName }, _gameModel, stateManager) => {
      stateManager.updateState(
        produce((draft) => {
          (draft as GameState<Game>).characters[character].name = newName;
        })
      );
    },
    UpdateLocationState: (
      { stateItem, newState },
      _gameModel,
      stateManager
    ) => {
      stateManager.updateState(
        produce((draft) => {
          (draft as GameState<Game>).locations[stateItem].state = newState;
        })
      );
    },
    UpdateLocationFlag: (
      { stateItem, flag, value },
      _gameModel,
      stateManager
    ) => {
      stateManager.updateState(
        produce((draft) => {
          (draft as GameState<Game>).locations[stateItem].flags[String(flag)] =
            value;
        })
      );
    },
    CharacterSay: ({ character, sentences }, gameModel, stateManager) => {
      const name =
        stateManager.getState().characters[character]?.name ??
        gameModel.settings.characterConfigs[character].defaultName;

      if (sentences.length === 1) {
        console.log(`${name}: "${sentences[0]}"`);
      } else {
        for (const index in sentences) {
          if (Number(index) === 0) {
            console.log(`${name}: "${sentences[index]}`);
          } else if (Number(index) === sentences.length - 1) {
            console.log(`  ${sentences[index]}"`);
          } else {
            console.log(`  ${sentences[index]}`);
          }
        }
      }
      console.log("");
    },
    Condition: async (
      { condition, body, elseBody },
      gameModel,
      stateManager
    ) => {
      if (testCondition(condition, stateManager)) {
        await runScript(body, gameModel, stateManager);
      } else {
        await runScript(elseBody, gameModel, stateManager);
      }
    },
    OpenOverlay: async (statement, gameModel, stateManager) => {
      await handleOverlay(
        statement.overlayId,
        gameModel,
        stateManager,
        statement.onStart.script,
        statement.onEnd.script,
        statement.interactions
      );
      if (stateManager.getState().overlayStack.length === 0) {
        await describeLocation(gameModel, stateManager);
      }
    },
    CloseOverlay: ({ overlayId }, _gameModel, stateManager) => {
      stateManager.updateState(
        produce((draft) => {
          draft.overlayStack = draft.overlayStack.filter(
            (id) => id !== overlayId
          );
        })
      );
    },
  };

  return statementMap[statementType] as (
    statement: K,
    gameModel: GameModel<Game>,
    stateManager: GameStateManager<Game>
  ) => Promise<void> | void;
};

const handleInteractions = async <Game extends GameWorld>(
  interactions: GameInteraction<Game>[],
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  // prompt: should be default configured, and can be redefined for overlays
  console.log("Wat ga je doen:");
  const possibleInteractions = interactions
    .filter((interaction) => testCondition(interaction.condition, stateManager))
    .map((action, key) => ({
      action,
      key: `${key + 1}`,
    }));

  for (const interaction of possibleInteractions) {
    console.log(`${interaction.key}) ${interaction.action.label}`);
  }

  let input: string | undefined;
  let chosenAction: { action: GameInteraction<Game>; key: string } | undefined;
  do {
    input = await keypress();
    chosenAction = possibleInteractions.find(
      (interaction) => interaction.key === input
    );
  } while (!chosenAction);
  cls();

  await runScript<Game>(chosenAction.action.script, gameModel, stateManager);
};

const runScript = async <Game extends GameWorld>(
  script: ScriptAST<Game>,
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  for (const statement of script) {
    const handler = statementHandler<Game, ScriptStatement<Game>>(
      statement.statementType
    );
    await handler(statement, gameModel, stateManager);
  }
};

let keyPressed: (key: string) => void = () => {};
const stdin = process.stdin;

const enableKeyPresses = () => {
  stdin.resume();
  stdin.setRawMode(true);
  stdin.setEncoding("utf8");

  stdin.on("data", function (key: string) {
    // ctrl-c ( end of text )
    if (key === "\u0003" || key === "\u001b") {
      exitGame();
    }
    // if (key === "\u0020") {
    //   skip = true;
    // }
    keyPressed(key);
  });
};

const keypress = () =>
  new Promise<string>((resolve) => {
    keyPressed = (key: string) => {
      resolve(key);
    };
  });

const describeLocation = async <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  const currentLocation = stateManager.getState().currentLocation;
  const locationData = gameModel.locations.find(
    (l) => l.id === currentLocation
  );
  if (!locationData) {
    console.log(`Location not found: ${String(currentLocation)}`);
    exitGame(1);
  }

  const previousLocation = stateManager.getState().previousLocation;
  if (currentLocation !== previousLocation) {
    const enterScript = locationData?.onEnter.find(
      (item) => item.from === previousLocation
    );
    if (enterScript) {
      await runScript<Game>(enterScript.script, gameModel, stateManager);
      stateManager.updateState((state) => ({
        ...state,
        previousLocation: currentLocation,
      }));
    }
  }

  await runScript<Game>(
    locationData?.describe.script || [],
    gameModel,
    stateManager
  );
};

const cls = () => process.stdout.write("\x1Bc");

const runLocation = async <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  await describeLocation(gameModel, stateManager);

  const currentLocation = stateManager.getState().currentLocation;
  const locationData = gameModel.locations.find(
    (l) => l.id === currentLocation
  );
  while (stateManager.getState().currentLocation === currentLocation) {
    await handleInteractions(
      locationData?.interactions || [],
      gameModel,
      stateManager
    );
  }
  const newLocation = stateManager.getState().currentLocation;
  const exitScript = locationData?.onLeave.find(
    (item) => item.to === newLocation
  );
  if (exitScript) {
    await runScript<Game>(exitScript.script, gameModel, stateManager);
  }
};

export const runGame = async <Game extends GameWorld>(
  gameModel?: GameModel<Game>
) => {
  if (!gameModel) {
    console.log("No valid game file");
    exitGame(1);
    return;
  }

  let gameState: GameState<Game> = {
    ...createDefaultState(gameModel),
    ...gameModel.settings.initialState,
  };

  const stateManager: GameStateManager<Game> = {
    getState: () => gameState,
    updateState: (mutation) => {
      gameState = mutation(gameState);
    },
  };
  enableKeyPresses();

  cls();
  while (true) {
    await runLocation(gameModel, stateManager);
  }
};
