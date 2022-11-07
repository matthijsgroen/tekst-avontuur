import g from "../game";

g.defineLocation("darkwoods", ({ describe, interaction }) => {
  describe(() => {
    g.text("You are in the dark woods");
  });

  interaction("leave", g.always(), () => {
    g.travel("village");
  });
});
