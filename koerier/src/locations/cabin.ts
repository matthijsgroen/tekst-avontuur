import g from "../game";

g.defineLocation("cabin", ({ describe, onLeave, interaction }) => {
  describe(() => {
    g.onState(
      g.location("cabin").hasFlag("visited"),
      () => {
        g.descriptionText(
          "You are in front of the cabin of {b}[characters.witch.name]{/b}.",
          "Behind the cabin the swamp starts to be really wet, so there is no way further.",
          "a small windy trail leads back where you came from."
        );
      },
      () => {
        g.descriptionText(
          "Strange smells and fumes come out. You carefully approach the door and are in doubt if you should {b}knock{/b}."
        );
        g.descriptionText("");
        g.descriptionText(
          "You won't get much time to think about it, because the door sweeps open:"
        );
        g.openOverlay("witchConversation");
      }
    );
  });

  onLeave("swamp", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name] you walk into the swamp."
        );
      },
      () => {
        g.text("You walk over the windy trail, into the swamp.");
      }
    );
  });

  interaction("Knock on door", g.always(), () => {
    g.openOverlay("witchConversation");
  });

  interaction("Go back to swamp", g.always(), () => {
    g.travel("swamp");
  });
});
