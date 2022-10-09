import g from "../game";
import { inventory } from "../inventory";

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
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit [character.dwarf.defaultName] de dwerg."
        );
      },
      () => {
        g.character("dwarf").setName("Dwerg"); // This name should become translatable

        g.text(
          "Je bent bij de mijn",
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit een dwerg."
        );
      }
    );
    g.text("Hij ziet er niet al te vrolijk uit.");
  });

  inventory(interaction);

  interaction("Praat met de dwerg", g.always(), () => {
    g.openOverlay("dwarfConversation");
  });

  interaction("Verlaat mijn", g.always(), () => {
    g.travel("hills");
  });
});
