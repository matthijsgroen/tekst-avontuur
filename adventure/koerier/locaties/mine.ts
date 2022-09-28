import g from "../game";

g.defineLocation("mine", ({ describe, interaction, onLeave }) => {
  onLeave("hills", () => {
    g.text("Je groet de dwerg en loopt weer richting de weg.");
  });

  describe(() => {
    g.onState(
      g.hasCharacterFlag("dwarf", "nameKnown"),
      () => {
        g.text(
          "Je bent bij de mijn",
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit Thorin de dwerg."
        );
      },
      () => {
        g.text(
          "Je bent bij de mijn",
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit een dwerg."
        );
      }
    );
    g.text("Hij ziet er niet al te vrolijk uit.");

    // Move naar 'dwarf' layer?
    g.character("player").say("Hallo, kun je mij helpen?");
    g.text("De dwerg kijkt je een beetje boos aan.");
    g.character("dwarf").say("Grr. Stom ding, stom ding!");
  });

  interaction("Praat met de dwerg", g.always(), () => {
    g.travel("hills");
  });

  interaction("Verlaat mijn", g.always(), () => {
    g.travel("hills");
  });
});
