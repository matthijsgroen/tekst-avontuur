import game from "../game";

game.location("forest", ({ onEnter, describe }) => {
  onEnter("farmland", () => {});

  describe(() => {
    game.text(
      "Je staat in het bos. Het is een stralende dag.",
      "De wind laat de blaadjes ritselen.",
      "",
      "In het oosten zijn akkers.",
      "In het westen zijn heuvels."
    );

    game.ifItemNot("bag", "possession", () => {
      game.text("Op de grond ligt je tas en scherven van de fles medicijnen.");
      game
        .character("player")
        .say("Verdorie, de medicijnen zijn echt verloren.");
      game.text("Je raapt de tas op.");
      game.item("bag").setState("possession");
    });

    game.ifItem("branch", "unknown", () => {
      game.text("Op de grond ligt een vers afgebroken tak.");
    });
  });
});
