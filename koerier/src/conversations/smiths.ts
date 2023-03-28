import g from "../game";

g.defineOverlay(
  "smithsConversation",
  ({ onEnter, closeOverlay, interaction }) => {
    onEnter(() => {
      g.character("goldsmith").say("Yes?");
    });

    interaction(
      "Do you know how I could get some medication?",
      g.always(),
      () => {
        g.character("player").say(
          "Do you know how I could get some medication?"
        );
        g.character("farrier").say("No clue. I am never sick.");
        g.character("armorer").say("Hah! You big baby! You are plenty sick.");
        g.character("goldsmith").say(
          "And then you are begging at the {b}bakery{/b} for some medication!"
        );
        g.character("farrier").say("Not true!");
        g.text("[characters.farrier.name]'s head is getting red.");
      }
    );

    interaction(
      "Do you know how I could get some transportation?",
      g.always(),
      () => {
        g.character("player").say(
          "Do you know how I could get some transportation?"
        );

        g.onState(
          g.character("horse").hasState("following"),
          () => {
            g.character("goldsmith").say("Euhm, didn't you...");
            g.character("armorer").say("...just get here...");
            g.character("goldsmith").say("...with a horse?");
            g.text("You feel a bit dumb.");
          },
          () => {
            g.character("goldsmith").say(
              "I think {b}farmer [characters.farmer.name]{/b} has a horse?"
            );
            g.character("armorer").say(
              "It is a workhorse, so it won't bring you large distances."
            );
            g.character("farrier").say("We do everything by foot around here.");
            g.character("armorer").say("Walking is good for your health!");
          }
        );
      }
    );

    interaction("I think I'll browse", g.always(), () => {
      g.character("armorer").say("See you later!");
      closeOverlay();
    });
  }
);
