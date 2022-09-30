import { GameInteraction, GameModel } from "../dsl/ast-types";
import { GameStateManager } from "../dsl/engine-types";
import { testCondition } from "../dsl/testCondition";
import { GameWorld } from "../dsl/world-types";
import { runScript } from "./runScript";
import { cls, keypress } from "./utils";

export const handleInteractions = async <Game extends GameWorld>(
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
