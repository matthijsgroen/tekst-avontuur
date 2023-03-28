import g from "./game";

g.defineOverlay("inventory", ({ onEnter, interaction, closeOverlay }) => {
  onEnter(() => {
    g.descriptionText("You carry the following items:");
    // g.textList("inventory", (displayText) => {
    //   displayText(
    //     "coins",
    //     "[characters.player.counters.coins] coins",
    //     g.character("player").hasCounter("coins").moreThan(1)
    //   );
    //   displayText(
    //     "coins",
    //     "[characters.player.counters.coins] coin",
    //     g.character("player").hasCounter("coins").equals(1)
    //   );
    //   displayText("rope", "A long rope");
    //   displayText("branch", "A branch, picked up in the forest");
    //   displayText("brokenPickaxe", "A pickaxe with a broken hilt");
    //   displayText("pickaxe", "A fixed pickaxe");
    //   displayText("cookies", "Delicious cookies");
    //   displayText("fabric", "A giant trunk, probably of a giant");
    //   displayText("gemstone", "A sparkling gemstone");
    // });

    g.onState(g.character("player").hasCounter("coins").moreThan(1), () => {
      g.descriptionText("- [characters.player.counters.coins] coins");
    });
    g.onState(g.character("player").hasCounter("coins").equals(1), () => {
      g.descriptionText("- [characters.player.counters.coins] coin");
    });
    g.onState(g.item("rope").hasState("possession"), () => {
      g.descriptionText("- A long rope");
    });
    g.onState(g.item("branch").hasState("possession"), () => {
      g.descriptionText("- A branch, picked up in the forest");
    });
    g.onState(g.item("pickaxe").hasState("broken"), () => {
      g.descriptionText("- A pickaxe with a broken hilt");
    });
    g.onState(g.item("pickaxe").hasState("fixed"), () => {
      g.descriptionText("- A fixed pickaxe");
    });
    g.onState(g.item("cookies").hasState("possession"), () => {
      g.descriptionText("- Delicious cookies");
    });
    g.onState(g.item("fabric").hasState("possession"), () => {
      g.descriptionText("- A giant trunk, probably of a giant");
    });
    g.onState(g.item("gemstone").hasState("possession"), () => {
      g.descriptionText("- A sparkling gemstone");
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

g.globalInteraction(
  "Open your bag",
  "b",
  g.and(g.item("bag").hasState("possession"), g.not(g.isOverlayOpen())),
  () => {
    g.openOverlay("inventory");
  }
);
