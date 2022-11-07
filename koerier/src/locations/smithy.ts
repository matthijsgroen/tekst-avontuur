import g from "../game";

g.defineLocation("smithy", ({ describe, interaction }) => {
  describe(() => {
    g.text("you are at the smithy");
  });

  interaction("leave", g.always(), () => {
    g.travel("village");
  });
});
