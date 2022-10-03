import { GameInteraction, GameModel } from "../dsl/ast-types";
import { GameStateManager } from "../engine/engine-types";
import { testCondition } from "../engine/testCondition";
import { GameWorld } from "../dsl/world-types";
import {
  determineTextScope,
  FormattedText,
  getDisplayText,
} from "../engine/processText";
import { runScript } from "./runScript";
import { cls, keypress } from "./utils";
import { renderText } from "./renderText";

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

  const textScope = determineTextScope(stateManager, "interactions");

  for (const interaction of possibleInteractions) {
    let text: FormattedText = [];
    text.push({ type: "text", text: `${interaction.key}) ` });
    text.push(
      ...getDisplayText(interaction.action.label, stateManager, textScope)
    );
    renderText(text, 500);
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
