import g from "../game";

g.defineLocation("farmland", ({ describe, interaction, onLeave }) => {
  onLeave("forest", () => {
    g.text("You are walking to the east, towards the forest.");
  });
  onLeave("farm", () => {
    g.text("You walk onto the barnyard of the farm.");
  });
  onLeave("village", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name], you walk south towards the village."
        );
      },
      () => {
        g.text("You walk south towards the village.");
      }
    );
  });

  describe(() => {
    g.text(
      "You are in the farmlands.",
      "On the right side of the road is a {b}farm{/b}.",
      "Smoke is rising up from behind the farm."
    );
    g.text(
      "In the {b}west{/b} is a {b}forest{/b}.",
      "A small {b}village{/b} lies to the {b}south{/b}."
    );
    g.location("farmland").setFlag("visited");
  });

  interaction("Go to the farm", g.always(), () => {
    g.travel("farm");
  });

  interaction("Go west, to the forest", g.always(), () => {
    g.onState(
      g.and(
        g.character("horse").hasState("following"),
        g.not(g.character("horse").hasFlag("hooves"))
      ),
      () => {
        g.text(
          "You want to walk to the west towards the forest, but [characters.horse.defaultName] starts to refuse."
        );
        g.character("player").say(
          "Maybe I have to take him to the farm first?"
        );
      },
      () => {
        g.travel("forest");
      }
    );
  });

  interaction("Go south, towards the village", g.always(), () => {
    g.travel("village");
  });
});
