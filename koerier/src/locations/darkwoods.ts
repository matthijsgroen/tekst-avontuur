import g from "../game";

g.defineLocation("darkwoods", ({ describe, interaction, onLeave }) => {
  describe(() => {
    g.text(
      "You are on the edge of a dark wood. It looks dangerous.",
      "The sunlight can barely reach the forest floor, and trees seem to look at you.",
      "There is a thick fog between the bare branches.",
      "It is eerily quiet. No animal lives here."
    );
    g.onState(
      g.character("dragon").hasState("known"),
      () => {
        g.character("player").say(
          "If there really is a monster here, I need to prepare before I dare enter here."
        );
        g.onState(
          g.and(
            g.character("baker").hasFlag("toldDragon"),
            g.character("farmer").hasFlag("toldDragon")
          ),
          () => {
            g.character("player").say("I know enough about the monster.");
          },
          () => {
            g.character("player").say(
              "I don't know enough about the monster yet...",
              "(Maybe other people can {b}tell me more?{/b})"
            );
          }
        );

        g.onState(
          g.character("farmer").hasFlag("toldDragon"),
          () => {
            g.character("player").say(
              "It sounds like a flying firebreathing dragon.",
              "Having a heavy armor is useless."
            );
          },
          () => {
            g.character("player").say("Maybe I need to buy some {b}armor{/b}.");
          }
        );
        g.onState(
          g.character("baker").hasFlag("toldDragon"),
          () => {
            g.onState(
              g.item("sword").hasState("possession"),
              () => {
                g.character("player").say(
                  "At least I have a {b}weapon{/b} to defend myself."
                );
              },
              () => {
                g.character("player").say(
                  "I still need a {b}weapon{/b} to defend myself."
                );
              }
            );
          },
          () => {
            g.character("player").say(
              "And maybe I need a {b}weapon{/b} to defend myself?"
            );
          }
        );
      },
      () => {
        g.character("player").say("Brr... I have {b}no reason{/b} to be here.");
      }
    );
  });

  onLeave("village", () => {
    g.onState(
      g.character("horse").hasState("following"),
      () => {
        g.text(
          "Together with [characters.horse.name] you walk westwards, towards the village."
        );
      },
      () => {
        g.text("You walk westwards, towards the village.");
      }
    );
  });

  interaction("Go west, towards the village", g.always(), () => {
    g.travel("village");
  });
});
