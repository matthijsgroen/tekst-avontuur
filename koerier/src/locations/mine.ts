import g from "../game";
import { inventory } from "../inventory";

g.defineLocation("mine", ({ describe, interaction, onLeave }) => {
  onLeave("hills", () => {
    g.text("You greet the dwarf and walk back towards the road.");
  });

  describe(() => {
    g.onState(
      g.hasCharacterFlag("dwarf", "nameKnown"),
      () => {
        g.text(
          "You are at the mine entrance.",
          "A mining cart lies on its side. [character.dwarf.defaultName] is sitting at the entrance."
        );
      },
      () => {
        g.character("dwarf").setName("Dwarf"); // This name should become translatable

        g.text(
          "You are at the mine entrance.",
          "A mining cart lies on its side. A dwarf is sitting at the entrance."
        );
      }
    );
    g.text("He looks grumpy.");
  });

  inventory(interaction);

  interaction("Talk to the dwarf", g.always(), () => {
    g.openOverlay("dwarfConversation");
  });

  interaction("Leave the mine", g.always(), () => {
    g.travel("hills");
  });
});
