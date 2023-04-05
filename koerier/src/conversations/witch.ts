import g from "../game";

g.defineOverlay(
  "witchConversation",
  ({ onEnter, interaction, closeOverlay }) => {
    onEnter(() => {
      g.onState(
        g.character("player").hasFlag("male"),
        () => {
          g.character("witch").say(
            "Hello handsome stud, what can I do for you!?"
          );
        },
        () => {
          g.character("witch").say(
            "Hello gorgeous lady, what can I do for you!?"
          );
        }
      );
      g.text("You startle and jump backwards.");
    });

    const intro1 = () => {
      g.text("You are stammering, not knowing what to say.");
      g.text("");
      g.text("The woman comes across as quite friendly and enthusiastic.");

      g.text("");
      g.text("... is this a witch?");
      g.text("You imagined her very differently.");
      g.character("player").say("...");
      g.character("witch").say("No need to be scared. I was expecting you!");
      g.character("witch").setState("intro");
    };

    interaction("Euhm...", g.character("witch").hasState("unknown"), intro1);
    interaction("Uh oh...", g.character("witch").hasState("unknown"), intro1);
    interaction("Well...", g.character("witch").hasState("unknown"), intro1);

    const intro2 = () => {
      g.character("player").say("...");
      g.text("You are rendered {b}speechless{/b}.");
      g.character("witch").say(
        "My name is {b}[.name]{/b}. You must be {b}[characters.player.name]{/b}.",
        "Let me guess, you are here for a healingpotion?"
      );
      g.text("How does she know all these things?");
      g.character("witch").say("Yes, {i}magic{/i} is a powerful thing!");
      g.text(
        "In the corner of your eye, you see a {b}raven{/b} fly away from a windowsill."
      );
      g.text("You start to suspect she received a letter as well...");
      g.character("witch").setState("visited");
      g.location("cabin").setFlag("visited");
    };

    interaction("But how...", g.character("witch").hasState("intro"), intro2);
    interaction(
      "That is not possible...",
      g.character("witch").hasState("intro"),
      intro2
    );

    interaction(
      "Could you help me with a medicine?",
      g.character("witch").hasState("visited"),
      () => {
        // TODO
      }
    );

    interaction(
      "Give gemstone to [characters.witch.name]",
      g.character("witch").hasState("visited"),
      () => {
        // TODO
      }
    );

    interaction(
      "Give necklace to [characters.witch.name]",
      g.character("witch").hasState("visited"),
      () => {
        // TODO
      }
    );

    interaction(
      "Did you also hear that there is a treasure hidden somewhere?",
      g.character("witch").hasState("visited"),
      () => {
        // TODO
      }
    );
    interaction("Goodbye", g.character("witch").hasState("visited"), () => {
      closeOverlay();
    });
  }
);
