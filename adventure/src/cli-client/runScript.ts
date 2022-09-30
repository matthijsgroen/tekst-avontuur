import produce from "immer";
import { GameModel, ScriptAST, ScriptStatement } from "../dsl/ast-types";
import { GameState, GameStateManager } from "../dsl/engine-types";
import { testCondition } from "../dsl/testCondition";
import { GameWorld } from "../dsl/world-types";
import { describeLocation } from "./describeLocation";
import { handleOverlay } from "./handleOverlay";

type StatementMap<Game extends GameWorld> = {
  [K in ScriptStatement<Game> as K["statementType"]]: (
    statement: K,
    gameModel: GameModel<Game>,
    stateManager: GameStateManager<Game>
  ) => Promise<void> | void;
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

export const runScript = async <Game extends GameWorld>(
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
