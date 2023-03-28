import g from "../game";

g.defineLocation("farm", ({ describe, interaction, onLeave }) => {
  describe(() => {
    g.text(
      "You are at a large farm.",
      "Next to the farm lies a pile of {b}grain{/b}."
    );
    g.text("Big plumes of smoke rise up from the other side of the farm.");

    g.text(
      "You quickly walk to the other side of the farm.",
      "There was clearly a fire here, but the flames have been extinguished."
    );
    g.text("A farmer looks defeated at the smoldering remains...");
    g.onState(
      g.character("horse").hasState("stable"),
      () => {
        g.text("{b}[characters.horse.name]{/b} is in his stable.");
      },
      () => {
        g.text("The stable is {b}empty{/b}.");
      }
    );
    g.onState(g.item("rope").hasState("unknown"), () => {
      g.text("A {b}rope{/b} is hanging on the side of the stable.");
    });
  });

  interaction("Talk to farmer", g.always(), () => {
    g.openOverlay("farmerConversation");
  });

  interaction(
    "Bring [characters.horse.name] to the stable",
    g.character("horse").hasState("following"),
    () => {
      g.text("You bring [characters.horse.name] to his stable.");
      g.character("horse").setState("stable");
    }
  );

  interaction(
    "Get [characters.horse.name] from the stable",
    g.character("horse").hasState("stable"),
    () => {
      g.text("You lead [characters.horse.name] out of the stable.");
      g.character("player").say("Hi {b}[characters.horse.name]{/b}.");
      g.character("horse").setState("following");
    }
  );

  interaction(
    "Pickup rope",
    g.and(
      g.character("farmer").hasFlag("visited"),
      g.item("rope").hasState("unknown")
    ),
    () => {
      g.text("You pickup the rope");
      g.item("rope").setState("possession");
    }
  );

  onLeave("farmland", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.descriptionText(
          "Together with {b}[characters.horse.name]{/b}, you walk back to the road."
        );
      },
      () => {
        g.descriptionText("You walk back to the road.");
      }
    );
  });

  interaction("Walk back to the road", g.always(), () => {
    g.travel("farmland");
  });
});
