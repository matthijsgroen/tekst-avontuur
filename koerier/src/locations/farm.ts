import g from "../game";

g.defineLocation("farm", ({ describe, interaction }) => {
  describe(() => {
    g.text(
      "You are at a large farm.",
      "Next to the farm lies a pile of {b}grain{/b}."
    );
    g.text("Big plumes of smoke rise up from the other side of the farm.");

    g.text(
      "You quickly walk to the other side of the farm.",
      "There was clearly a fire here, but the flames have been extinguished."
    );
    g.text("A farmer looks defeated at the smoldering remains...");
    g.text("The stable is {b}empty{/b}.");
    g.onState(g.item("rope").hasState("unknown"), () => {
      g.text("A {b}rope{/b} is hanging on the side of the stable.");
    });
  });

  interaction("Pickup rope", g.item("rope").hasState("unknown"), () => {
    g.text("You pickup the rope");
    g.item("rope").setState("possession");
  });

  interaction("Walk back to the road", g.always(), () => {
    g.travel("farmland");
  });
});
