import g from "../game";

g.location("farmland", ({ describe, interaction }) => {
  describe(() => {
    g.text(
      "Je staat in een boerenlandschap.",
      "Aan de rechterkant van de weg staat een boerderij.",
      "Er komt rook achter de boerderij vandaan.",
      "",
      "In het westen ligt een bos.",
      "In het zuiden loopt een weg richting een dorp."
    );
  });

  interaction("Ga naar het westen, richting het bos", g.always(), () => {
    g.onState(
      g.isItemState("horse", "found"),
      () => {
        g.text(
          "Je wil naar het westen wandelen, richting het bos, maar Teun begint steeds moeilijker te lopen,",
          "en sputtert actief tegen."
        );
        g.character("player").say(
          "Misschien moet ik hem eerst naar de boerderij brengen?"
        );
      },
      () => {
        g.travel("forest");
      }
    );
  });

  interaction("Ga naar het zuiden, richting het dorp", g.always(), () => {
    g.travel("hills");
  });
});
