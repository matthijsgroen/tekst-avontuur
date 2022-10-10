import { GameWorld, Interaction } from "point-n-click";
import g from "./game";

g.defineOverlay("inventory", ({ onEnter, interaction, closeOverlay }) => {
  onEnter(() => {
    g.text("You carry the following items:");
    g.onState(g.hasCharacterValue("player", "coins", "moreThan", 1), () => {
      g.text("- [character.player.values.coins] coins");
    });
    g.onState(g.hasCharacterValue("player", "coins", "equals", 1), () => {
      g.text("- [character.player.values.coins] coin");
    });
    g.onState(g.isItemState("branch", "possession"), () => {
      g.text("- A branch, picked up in the forest");
    });
    g.onState(g.isItemState("pickaxe", "broken"), () => {
      g.text("- A pickaxe with a broken hilt");
    });
    g.onState(g.isItemState("pickaxe", "fixed"), () => {
      g.text("- A fixed pickaxe");
    });
  });

  interaction(
    "Repair the pickaxe with the branch from the forest",
    g.and(
      g.isItemState("branch", "possession"),
      g.isItemState("pickaxe", "broken")
    ),
    () => {
      g.text(
        "You remove the old hilt of the pickaxe,",
        "and replace the hilt with the branch from the forest."
      );
      g.text("It fits! The pickaxe is as good as new.");
      g.item("branch").setState("used");
      g.item("pickaxe").setState("fixed");
    }
  );

  interaction("Close your bag", g.always(), () => {
    closeOverlay();
  });
});

export const inventory = <Game extends GameWorld>(
  interaction: Interaction<Game>
) => {
  interaction("Open your bag", g.isItemState("bag", "possession"), () => {
    g.openOverlay("inventory");
  });
};
