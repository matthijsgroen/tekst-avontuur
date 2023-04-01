import g from "../game";

g.defineOverlay("plants", ({ onEnter, interaction, closeOverlay }) => {
  onEnter(() => {
    g.text(
      "You take your time to take a closer look at all the different plants that grow here in the {b}swamp{/b}.",
      "There are plants with {b}thorny{/b} leaves, {b}round{/b} leaves, {b}diamond-shaped{/b} leaves and {b}heart-shaped{/b} leaves."
    );

    // "1=4;2=0;4=1;56=1", "*c3", "", "$n: 'Handig dat ik nu alles over deze planten weet!'", "&"
  });

  interaction("Pick plant with thorny leaves", g.always(), () => {
    g.text(
      "You bend over to pick the plant with {b}thorny{/b} leaves.",
      "Just before you touch it, you stop.",
      "This plant could be {b}poisonous{/b}."
    );
  });
  interaction("Pick plant with round leaves", g.always(), () => {
    g.text(
      "You bend over to pick the plant with {b}round{/b} leaves.",
      "Just before you touch it, you stop.",
      "This plant could be {b}poisonous{/b}."
    );
  });
  interaction("Pick plant with diamond-shaped leaves", g.always(), () => {
    g.text(
      "You bend over to pick the plant with {b}diamond-shaped{/b} leaves.",
      "Just before you touch it, you stop.",
      "This plant could be {b}poisonous{/b}."
    );
    g.onState(
      g.and(
        g.item("treasureNotes").hasFlag("moonStone"),
        g.item("moonStone").hasState("unknown")
      ),
      () => {
        g.text(
          "Then you remember what {b}[characters.baker.name]{/b} said about a {b}Moonstone{/b}.",
          "You start digging at the roots of the plant, and discover a smooth small stone.",
          "The {b}color{/b} seems a bit off. You put it in your bag."
        );
        g.item("moonStone").setState("possession");
        // "*s2", "Dan herinner je wat de bakker had gezegd over de Maansteen."
        // "Je gaat graven bij de wortels van de plant, en vindt een gladde kleine steen."
        // "Hij is iets apart van kleur. Je stopt hem in je tas."
      }
    );
  });
  interaction("Pick plant with heard-shaped leaves", g.always(), () => {
    g.text(
      "You bend over to pick the plant with {b}heart-shaped{/b} leaves.",
      "Just before you touch it, you stop.",
      "This plant could be {b}poisonous{/b}."
    );
  });

  interaction("Stop looking at the plants", g.always(), () => {
    closeOverlay();
  });
});
