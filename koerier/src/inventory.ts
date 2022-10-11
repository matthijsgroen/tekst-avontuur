import { GameWorld, Interaction } from "point-n-click";
import g from "./game";

g.defineOverlay("inventory", ({ onEnter, interaction, closeOverlay }) => {
  onEnter(() => {
    g.text("You carry the following items:");
    g.onState(g.character("player").hasCounter("coins").moreThan(2), () => {
      g.text("- [character.player.values.coins] coins");
    });
    g.onState(g.character("player").hasCounter("coins").equals(1), () => {
      g.text("- [character.player.values.coins] coin");
    });
    g.onState(g.item("branch").hasState("possession"), () => {
      g.text("- A branch, picked up in the forest");
    });
    g.onState(g.item("pickaxe").hasState("broken"), () => {
      g.text("- A pickaxe with a broken hilt");
    });
    g.onState(g.item("pickaxe").hasState("fixed"), () => {
      g.text("- A fixed pickaxe");
    });
  });

  interaction(
    "Repair the pickaxe with the branch from the forest",
    g.and(
      g.item("branch").hasState("possession"),
      g.item("pickaxe").hasState("broken")
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
  interaction("Open your bag", g.item("bag").hasState("possession"), () => {
    g.openOverlay("inventory");
  });
};
