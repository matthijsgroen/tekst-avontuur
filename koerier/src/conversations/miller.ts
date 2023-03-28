import g from "../game";

g.defineOverlay(
  "millerConversation",
  ({ onEnter, interaction, closeOverlay, onLeave }) => {
    onEnter(() => {
      g.character("miller").say("Hey hello there!");
    });

    interaction(
      "Any idea how I could get any transportation?",
      g.always(),
      () => {
        g.character("player").say(
          "Any idea how I could get any transportation?"
        );
        g.text("The old miller looks at you.");
        g.character("miller").say(
          "I no longer have a horse, just a carriage.",
          "If I need a horse, I borrow [characters.horse.defaultName] from farmer {b}[characters.farmer.defaultName]{/b}.",
          "It is a really dependable animal. Ideal for pulling the carriage!"
        );
      }
    );

    interaction("Any idea how I could get any medicine?", g.always(), () => {
      g.character("player").say("Any idea how I could get any medicine?");
      g.text("The old miller looks at you.");

      g.onState(
        g.character("player").hasFlag("male"),
        () => {
          g.character("miller").say("Sorry kid{#male/}, I wouldn't know.");
        },
        () => {
          g.character("miller").say("Sorry kid{#female/}, I wouldn't know.");
        }
      );
    });

    interaction(
      "What did you need again for the repairs?",
      g.not(g.location("mill").hasState("fixed")),
      () => {
        g.character("player").say("What did you need again for the repairs?");
        g.character("miller").say("I need a new sail for one of the blades.");
        g.character("player").say("Ah ok, thanks.");
      }
    );

    interaction(
      "Can I have the millstone?",
      g.item("millstone").hasState("seen"),
      () => {
        g.character("player").say("Can I have the millstone?");
        g.character("miller").say(
          "Whay could you possibly want with this millstone?",
          "It is only {b}heavy{/b} and worn out."
        );
      }
    );

    interaction("Goodbye", g.always(), () => {
      closeOverlay();
    });
  }
);
