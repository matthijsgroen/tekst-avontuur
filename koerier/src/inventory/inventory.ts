import g from "../game";

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
    g.onState(g.item("gold").hasState("possession"), () => {
      g.descriptionText("- Pieces of gold");
    });
    g.onState(g.item("runeStone").hasState("possession"), () => {
      g.descriptionText("- A runestone with inscriptions");
    });
    g.onState(g.item("treasureNotes").hasState("possession"), () => {
      g.descriptionText("- Notes on treasure");
    });
    g.onState(
      g.and(
        g.item("moonStone").hasState("possession"),
        g.not(g.isLocation("river"))
      ),
      () => {
        g.descriptionText("- Moonstone, it looks dim");
      }
    );
    g.onState(
      g.and(g.item("moonStone").hasState("possession"), g.isLocation("river")),
      () => {
        g.descriptionText("- Moonstone, it seems to glow lightly");
      }
    );
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

  interaction(
    "Check notes on treasure",
    g.item("treasureNotes").hasState("possession"),
    () => {
      g.openOverlay("treasureNotes");
    }
  );
  interaction("Eat cookies", g.item("cookies").hasState("possession"), () => {
    g.text(
      "The cookies smell delicious, but you decide to keep them for later."
    );
  });
  interaction(
    "Wait for darkness",
    g.and(
      g.isLocation("river"),
      g.item("moonStone").hasState("possession"),
      g.not(g.item("treasureHunt").hasFlag("done"))
    ),
    () => {
      g.item("treasureHunt").setFlag("active");
      g.location("treasureRoute").setCounter("steps", 0);
      closeOverlay();
      g.text(
        "You are patiently sitting at the bank of the river, waiting for darkness.",
        "The darker it gets, the more the moonstone starts to {b}glow{/b}."
      );
      g.onState(g.character("horse").hasState("following"), () => {
        g.character("player").say(
          "{b}[characters.horse.name]{/b} will you wait here for me?"
        );
        g.text(
          "{b}[characters.horse.name]{/b} goes to the waterfront to drink some water."
        );
        g.character("horse").setState("river");
      });
    }
  );

  interaction("Close your bag", g.always(), () => {
    closeOverlay();
  });
});

g.globalInteraction(
  "Open your bag",
  "b",
  g.and(
    g.item("bag").hasState("possession"),
    g.not(g.isOverlayOpen()),
    g.not(g.item("treasureHunt").hasFlag("active"))
  ),
  () => {
    g.openOverlay("inventory");
  }
);
