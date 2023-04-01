import g from "../game";

g.defineLocation("village", ({ describe, interaction, onLeave }) => {
  describe(() => {
    g.text("You are in the village. It is eery quiet.");
    g.descriptionText("");

    g.text(
      "At the left side of the road is is a {b}bakery{/b}.",
      "At the right side of the road a large {b}smithy{/b}."
    );
    g.descriptionText("");

    g.text(
      "The road continues {b}southwards{/b}, to a {b}river{/b}.",
      "A small hidden path goes {b}eastwards{/b}, to a {b}dark wood{/b}."
    );
  });

  onLeave("farmland", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name], you walk northwards, to the farmlands."
        );
      },
      () => {
        g.text("You walk northwards, to the farmlands.");
      }
    );
  });

  onLeave("darkwoods", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name], you walk over a small twisting path, towards the dark woods."
        );
      },
      () => {
        g.text("You walk over a small twisting path, towards the dark woods.");
      }
    );
  });

  onLeave("river", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name] you walk towards the river."
        );
      },
      () => {
        g.text("You walk southwards, towards the river.");
      }
    );
  });

  onLeave("bakery", () => {
    g.onState(g.character("horse").hasState("following"), () => {
      g.text(
        "You tie up [characters.horse.name] to the store. You enter the store with your mouth watering."
      );
    });
  });

  onLeave("smithy", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "You tie up [characters.horse.name] to a pole near the smithy and enter."
        );
      },
      () => {
        g.text("You enter the large smithy.");
      }
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
