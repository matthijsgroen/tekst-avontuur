import g from "../game";

g.defineLocation("mill", ({ describe, onLeave, interaction }) => {
  describe(() => {
    g.text(
      "You are at the windmill.",
      "The mill misses a {b}sail{/b} of one of its blades."
    );
    g.text(
      "A big {b}millstone{/b} lies next to the mill.",
      "An empty {b}carriage{/b} is parked on the other side of the mill."
    );
    g.text("An old miller appears to be working on restoring the blade.");

    g.onState(g.not(g.location("mill").hasFlag("visited")), () => {
      g.character("miller").say(
        "Hey hello over there! Could you help me out?",
        "This old mill needs fixin', but I'm missing a sail!"
      );

      g.character("player").say(
        "I'll keep my eyes open for something that could fit your needs!"
      );
      g.character("miller").say("Thanks!");
      g.location("mill").setFlag("visited");
    });
  });

  interaction("Talk to the miller", g.always(), () => {
    g.openOverlay("millerConversation");
  });

  interaction("Check millstone", g.always(), () => {
    g.text("You check the millstone. It looks really {b}heavy{/b}.");
    g.character("miller").say("Ah yes, that is an old one, really worn out.");
  });

  interaction("Walk back to the road", g.always(), () => {
    g.travel("hills");
  });
});
