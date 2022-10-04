import { GameModel } from "../dsl/ast-types";
import { GameStateManager, GameState } from "../engine/state/types";
import { GameWorld } from "../dsl/world-types";
import { createDefaultState } from "../engine/state/createDefaultState";
import { CLISettings, updateSettings } from "./settings";
import { cls, enableKeyPresses, exitGame } from "./utils";
import { runLocation } from "./runLocation";

export const runGame =
  ({ color = true, translationData }: CLISettings) =>
  async <Game extends GameWorld>(gameModel?: GameModel<Game>) => {
    if (!gameModel) {
      console.log("No valid game file");
      exitGame(1);
      return;
    }
    updateSettings({ color, translationData });

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
