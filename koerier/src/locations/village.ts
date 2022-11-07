import g from "../game";

g.defineLocation("village", ({ describe, interaction }) => {
  describe(() => {
    g.text("You are in the village. It is eery quiet.");

    g.text(
      "At the left side of the road is is a {b}bakery{/b}.",
      "At the right side of the road a large {b}smithy{/b}."
    );

    g.text(
      "The road continues {b}southwards{/b}, to a {b}river{/b}.",
      "A small hidden path goes {b}eastwards{/b}, to a {b}dark wood{/b}."
    );
  });

  interaction("Go to bakery", g.always(), () => {
    g.travel("bakery");
  });

  interaction("Go to smithy", g.always(), () => {
    g.travel("smithy");
  });

  interaction("Go north, to the farmlands", g.always(), () => {
    g.travel("farmland");
  });

  interaction("Go east, to the dark woods", g.always(), () => {
    g.travel("darkwoods");
  });

  interaction("Go south, to the river", g.always(), () => {
    g.travel("river");
  });
});
