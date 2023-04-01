import g from "../game";

g.defineLocation("hills", ({ describe, onLeave, interaction }) => {
  onLeave("forest", () => {
    g.text("You walk east, towards the forest.");
  });
  onLeave("mine", () => {
    g.text("You follow the road, towards the entrance of the mine.");
  });
  onLeave("mill", () => {
    g.text("You walk towards the mill.");
  });
  onLeave("swamp", () => {
    g.text("You go south, over a soggy trail, towards the swamp.");
  });

  describe(() => {
    g.text(
      "You are in the hills. The sun is shining lovely.",
      "There is a nice view of the environment."
    );
    g.descriptionText("");
    g.text(
      "A bit farther on the road you see a {b}mine{/b}.",
      "A bit higher up the hill is a {b}windmill{/b}."
    );
    g.descriptionText("");
    g.text(
      "There is a road to the {b}forest{/b} to the {b}east{/b}.",
      "To the {b}south{/b}, there is a soggy path going into a {b}swamp{/b}."
    );
  });

  interaction("Follow the path to the mine", g.always(), () => {
    g.travel("mine");
  });

  interaction("Go towards the mill", g.always(), () => {
    g.travel("mill");
  });

  interaction("Go east, to the forest", g.always(), () => {
    g.travel("forest");
  });

  interaction("Go south, to the swamp", g.always(), () => {
    g.travel("swamp");
  });
});
