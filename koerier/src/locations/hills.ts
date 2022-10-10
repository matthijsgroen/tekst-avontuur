import g from "../game";
import { inventory } from "../inventory";

g.defineLocation("hills", ({ describe, onLeave, interaction }) => {
  onLeave("forest", () => {
    g.text("You walk east, towards the forest.");
  });
  onLeave("mine", () => {
    g.text("You follow the road, towards the entrance of the mine.");
  });

  describe(() => {
    g.text(
      "You are in the hills. The sun is shining lovely.",
      "There is a nice view of the environment."
    );
    g.text(
      "A bit farther on the road you see a mine.",
      "A bit higher up the hill is a windmill.",
      "There is a road to the forest to the east.",
      "To the south, there is a soggy path going into a swamp."
    );
  });

  inventory(interaction);

  interaction("Jump on horse", g.never(), () => {
    // out of scope of proto
  });
  interaction("Follow the path to the mine", g.always(), () => {
    g.travel("mine");
  });

  interaction("Go towards the mill", g.never(), () => {
    // out of scope of proto
  });

  interaction("Go east, to the forest", g.always(), () => {
    g.travel("forest");
  });

  interaction("Go south, to the swamp", g.never(), () => {
    // out of scope of proto
  });
});
