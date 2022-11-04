import g from "../game";

g.defineLocation("village", ({ describe, interaction }) => {
  describe(() => {
    g.text("You are in a village");
  });

  interaction("Go north, to the farmlands", g.always(), () => {
    g.travel("farmland");
  });
});
