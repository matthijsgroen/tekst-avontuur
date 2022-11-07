import g from "../game";

g.defineLocation("bakery", ({ describe, interaction }) => {
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
        g.location("bakery").setFlag("visited", true);
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
