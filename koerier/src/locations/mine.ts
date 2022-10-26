import g from "../game";

g.defineLocation("mine", ({ describe, interaction, onLeave }) => {
  onLeave("hills", () => {
    g.text("You walk back towards the road.");
  });

  describe(() => {
    g.onState(
      g.character("dwarf").hasFlag("nameKnown"),
      () => {
        g.text(
          "You are at the mine entrance.",
          "A mining cart lies on its side. [character.dwarf.defaultName] is sitting at the entrance."
        );
      },
      () => {
        g.character("dwarf").setTranslatableName("Dwarf"); // This name should become translatable

        g.text(
          "You are at the mine entrance.",
          "A mining cart lies on its side. A dwarf is sitting at the entrance."
        );
      }
    );
    g.text("He looks grumpy.");
  });

  interaction("Talk to the dwarf", g.always(), () => {
    g.openOverlay("dwarfConversation");
  });

  interaction("Leave the mine", g.always(), () => {
    g.travel("hills");
  });
});
