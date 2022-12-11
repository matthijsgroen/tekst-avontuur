import g from "../game";

g.defineLocation("darkwoods", ({ describe, interaction, onLeave }) => {
  describe(() => {
    g.text(
      "You are on the edge of a dark wood. It looks dangerous.",
      "The sunlight can barely reach the forest floor, and trees seem to look at you.",
      "There is a thick fog between the bare branches.",
      "It is eerily quiet. No animal lives here."
    );
    g.character("player").say("Brr... I have {b}no reason{/b} to be here.");
  });

  onLeave("village", () => {
    g.text("You walk westwards, towards the village.");
  });

  interaction("Go west, towards the village", g.always(), () => {
    g.travel("village");
  });
});
