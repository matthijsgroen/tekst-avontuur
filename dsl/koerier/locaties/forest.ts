import g from "../game";

g.location("forest", ({ describe, interaction }) => {
  describe(() => {
    g.text(
      "Je staat in het bos. Het is een stralende dag.",
      "De wind laat de blaadjes ritselen.",
      "",
      "In het oosten zijn akkers.",
      "In het westen zijn heuvels."
    );

    g.onState(g.not(g.isItemState("bag", "possession")), () => {
      g.text("Op de grond ligt je tas en scherven van de fles medicijnen.");
      g.character("player").say("Verdorie, de medicijnen zijn echt verloren.");
      g.text("Je raapt de tas op.");
      g.item("bag").setState("possession");
    });

    g.onState(g.isItemState("branch", "unknown"), () => {
      g.text("Op de grond ligt een vers afgebroken tak.");
    });
  });

  interaction("Spring op paard", g.never(), () => {});

  interaction(
    "Raap de tak op",
    g.not(g.isItemState("branch", "unknown")),
    () => {
      g.item("branch").setState("possession");
    }
  );

  interaction("Ga naar het oosten, richting de akkers", g.always(), () => {
    g.travel("farmland");
  });

  interaction("Ga naar het westen, richting de heuvels", g.always(), () => {
    g.travel("hills");
  });
});
