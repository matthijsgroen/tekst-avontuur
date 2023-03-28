import g from "../game";

g.defineLocation("bakery", ({ describe, onEnter, onLeave, interaction }) => {
  describe(() => {
    g.text(
      "You are in the bakery. It is surprisingly empty.",
      "No cakes, pies or bread."
    );
    g.text("The only thing for sale seem to be {b}cookies{/b}.");
    g.onState(
      g.location("bakery").hasFlag("visited"),
      () => {
        g.text("The baker looks really sad.");
      },
      () => {
        g.text(
          "You somehow expected to be greeted by the baker, but no luck.",
          "The baker looks really sad."
        );
        g.location("bakery").setFlag("visited");
      }
    );
  });

  onEnter("village", () => {
    g.onState(g.not(g.character("horse").hasState("following")), () => {
      g.text("A nice smell of bread and cakes comes from the bakery.");
      g.text("You step into the shop with a mouth watering.");
    });
  });

  onLeave("village", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "You have no idea how to deal with the situation, so you walk back outside.",
          "You untie {b}[characters.horse.name]{/b}."
        );
      },
      () => {
        g.text(
          "You have no idea how to deal with the situation, so you walk back outside."
        );
      }
    );
  });

  interaction("Talk to baker", g.always(), () => {
    g.openOverlay("bakerConversation");
  });

  interaction("Leave bakery", g.always(), () => {
    g.travel("village");
  });
});
