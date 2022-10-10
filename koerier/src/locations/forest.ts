import g from "../game";
import { inventory } from "../inventory";

g.defineLocation("forest", ({ describe, interaction, onLeave }) => {
  onLeave("farmland", () => {
    g.text("You walk east, to the farmlands.");
  });
  onLeave("hills", () => {
    g.text("You walk west, towards the hills.");
  });

  describe(() => {
    g.text(
      "You are in the forest. It is a beautiful day.",
      "The wind is rustling the leaves."
    );
    g.text("There are farmlands in the east.", "There are hills in the west.");
    g.location("forest").setFlag("visited", true);

    g.onState(g.not(g.isItemState("bag", "possession")), () => {
      g.text(
        "Your bag is on the ground, surrounded by shards of glass of the bottle of medicine."
      );
      g.character("player").say("Drat, the medicine is truly lost.");
      g.text("You pick up your bag.");
      g.item("bag").setState("possession");
      g.character("player").setValue("coins", 3);
    });

    g.onState(g.isItemState("branch", "unknown"), () => {
      g.text("There is a freshly broken branch on the ground.");
    });
  });

  inventory(interaction);
  interaction("Jump on horse", g.never(), () => {});

  interaction("Pick up branch", g.isItemState("branch", "unknown"), () => {
    g.text(
      "You pick up the branch. You feel a small bump on your head.",
      "This branch hurt you quite a bit."
    );
    g.item("branch").setState("possession");
  });

  interaction("Go east, to the farmlands", g.always(), () => {
    g.travel("farmland");
  });

  interaction("Go west, to the hills", g.always(), () => {
    g.travel("hills");
  });
});
