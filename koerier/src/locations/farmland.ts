import g from "../game";

g.defineLocation("farmland", ({ describe, interaction, onLeave }) => {
  onLeave("forest", () => {
    g.text("You are walking to the east, towards the forest.");
  });

  describe(() => {
    g.text(
      "You are in the farmlands.",
      "On the right side of the road is a farm.",
      "Smoke is rising up from behind the farm."
    );
    g.text("In the west is a forest.", "A small village lies to the south.");
    g.location("farmland").setFlag("visited", true);
  });

  interaction("Go west, to the forest", g.always(), () => {
    g.onState(
      g.character("horse").hasState("found"),
      () => {
        g.text(
          "You want to walk to the west towards the forest, but [character.horse.defaultName] starts to refuse."
        );
        g.character("player").say(
          "Maybe I have to take him to the farm first?"
        );
      },
      () => {
        g.travel("forest");
      }
    );
  });

  interaction("Go south, towards the village", g.never(), () => {
    g.travel("hills");
  });
});
