import {
  ScriptStatement,
  ScriptAST,
  GameModel,
  GameInteraction,
} from "../dsl/ast-types";
import { GameStateManager, GameState } from "../dsl/engine-types";
import { GameWorld } from "../dsl/world-types";
import { testCondition } from "../engine/testCondition";

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
    },
    Travel: ({ destination }, _gameModel, stateManager) => {
      stateManager.updateState((state) => ({
        ...state,
        currentLocation: destination,
      }));
    },
    UpdateItemState: ({ stateItem, newState }, _gameModel, stateManager) => {
      stateManager.updateState((state) => ({
        ...state,
        items: {
          ...state.items,
          [stateItem]: {
            state: newState,
          },
        },
      }));
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
  };

  return statementMap[statementType] as (
    statement: K,
    gameModel: GameModel<Game>,
    stateManager: GameStateManager<Game>
  ) => Promise<void> | void;
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
      process.exit();
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

const cls = () => process.stdout.write("\x1Bc");

const runLocation = async <Game extends GameWorld>(
  gameModel: GameModel<Game>,
  stateManager: GameStateManager<Game>
) => {
  const currentLocation = stateManager.getState().currentLocation;
  const locationData = gameModel.locations.find(
    (l) => l.id === currentLocation
  );
  if (!locationData) {
    console.log(`Location not found: ${String(currentLocation)}`);
    process.exit(1);
  }
  cls();

  const describeLocation = async () => {
    const previousLocation = stateManager.getState().previousLocation;
    if (previousLocation !== currentLocation) {
      const enterScript = locationData?.onEnter.find(
        (item) => item.from === previousLocation
      );
      if (enterScript) {
        await runScript<Game>(enterScript.script, gameModel, stateManager);
        stateManager.updateState((state) => ({ ...state, previousLocation }));
      }
    }

    await runScript<Game>(
      locationData?.describe.script || [],
      gameModel,
      stateManager
    );
  };

  const handleInteraction = async () => {
    console.log("");
    // prompt: should be default configured, and can be redefined for overlays
    console.log("Wat ga je doen:");
    const possibleInteractions = (locationData?.interactions || [])
      .filter((interaction) =>
        testCondition(interaction.condition, stateManager)
      )
      .map((action, key) => ({
        action,
        key: `${key + 1}`,
      }));

    for (const interaction of possibleInteractions) {
      console.log(`${interaction.key}) ${interaction.action.label}`);
    }
    let input: string | undefined;
    let chosenAction:
      | { action: GameInteraction<Game>; key: string }
      | undefined;
    do {
      input = await keypress();
      chosenAction = possibleInteractions.find(
        (interaction) => interaction.key === input
      );
    } while (!chosenAction);

    await runScript<Game>(chosenAction.action.script, gameModel, stateManager);
  };

  await describeLocation();

  while (stateManager.getState().currentLocation === currentLocation) {
    await handleInteraction();
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
    process.exit(1);
    return;
  }

  let gameState: GameState<Game> = {
    currentLocation: gameModel.locations[0].id,
    items: {},
    characters: {},
    locations: {},
    ...gameModel.settings.initialState,
  };

  const stateManager: GameStateManager<Game> = {
    getState: () => gameState,
    updateState: (mutation) => {
      gameState = mutation(gameState);
    },
  };
  enableKeyPresses();

  while (true) {
    await runLocation(gameModel, stateManager);
  }
};
