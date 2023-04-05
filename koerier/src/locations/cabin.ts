import g from "../game";

g.defineLocation("cabin", ({ describe, interaction }) => {
  describe(() => {
    g.onState(
      g.location("cabin").hasFlag("visited"),
      () => {
        // TODO
      },
      () => {
        g.descriptionText(
          "Strange smells and fumes come out. You carefully approach the door and are in doubt if you should {b}knock{/b}."
        );
        g.descriptionText("");
        g.descriptionText(
          "You won't get much time to think about it, because the door sweeps open:"
        );
        g.openOverlay("witchConversation");
      }
    );
  });

  interaction("leave", g.always(), () => {
    g.travel("swamp");
  });
});
