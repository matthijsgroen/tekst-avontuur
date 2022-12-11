import g from "../game";

g.defineOverlay(
  "farmerConversation",
  ({ onEnter, onLeave, interaction, closeOverlay }) => {
    onEnter(() => {
      g.onState(
        g.not(g.character("farmer").hasFlag("visited")),
        () => {
          g.character("player").say("Can I help you? I hope nobody got hurt?");
          g.character("farmer").say(
            "Nobody got hurt, but that is the only good thing.",
            "My entire orchard has been burned to the ground,",
            "and my horse ran off."
          );
          g.character("farmer").say(
            "That monstrous beast in the tower burned everything down! We have to get rid of it!"
          );
          g.text(
            "You want to ask something about possible transportation,",
            "but since his horse is gone, it would not be appropriate to ask."
          );
          g.character("farmer").setFlag("visited");
        },
        () => {
          g.character("player").say("Hey, Can I help you?");
          g.character("farmer").say("Meh.");
        }
      );
    });

    onLeave(() => {
      g.text("You say goodbye to {b}[character.farmer.name]{/b}.");
    });

    interaction("Can I help putting out the fire?", g.always(), () => {
      g.character("player").say("Can I help putting out the fire?");
      g.character("farmer").say(
        "It's of no use, we can best just leave it smoldering for now.",
        "It's a pity of the apple harvest."
      );
    });

    interaction(
      "Can I help find your horse?",
      g.not(g.character("horse").hasFlag("found")),
      () => {
        g.character("player").say("Can I help find your horse?");
        g.character("farmer").say(
          "I think that {b}[character.horse.name]{/b} scared off.",
          "Drinking water calms him, maybe he is at the {b}river{/b}?"
        );
        g.character("horse").setFlag("known");
      }
    );

    interaction(
      "What could you tell me about the monster?",
      g.character("dragon").hasState("known"),
      () => {
        g.character("player").say("What could you tell me about the monster?");
      }
    );

    interaction("Goodbye", g.always(), () => {
      closeOverlay();
    });
  }
);
