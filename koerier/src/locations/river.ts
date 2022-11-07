import g from "../game";

g.defineLocation("river", ({ describe, interaction }) => {
  describe(() => {
    g.text("You are at the river bank");
  });

  interaction("leave", g.always(), () => {
    g.travel("village");
  });
});
