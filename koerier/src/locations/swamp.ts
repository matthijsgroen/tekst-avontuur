import g from "../game";

g.defineLocation("swamp", ({ describe, interaction, onLeave }) => {
  onLeave("hills", () => {
    g.text("You climb upwards, to the north, towards the hills.");
  });

  describe(() => {
    g.text("You are at the edge of a swamp.");
    g.text("At the end of the trail, you stop.");

    g.character("player").say(
      "I'm not going to walk into a swamp for {b}no reason{/b}!",
      "That is way too dangerous!"
    );

    g.text("You hear weird bubbling noises in the distance.");
    g.text(
      "There is a trail going to the {b}north{/b}, up into the {b}hills{/b}."
    );
  });

  interaction("Go north, towards the hills", g.always(), () => {
    g.travel("hills");
  });
});
