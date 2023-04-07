import g from "../game";

g.defineOverlay(
  "witchConversation",
  ({ onEnter, interaction, closeOverlay, onLeave }) => {
    onEnter(() => {
      g.onState(
        g.location("cabin").hasFlag("visited"),
        () => {
          g.character("witch").say(
            "Hi {b}[characters.player.name]{/b}, nice to see you back!"
          );
        },
        () => {
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
        }
      );
    });

    onLeave(() => {
      g.onState(g.isLocation("cabin"), () => {
        g.descriptionText(
          "You say goodbye to {b}[characters.witch.name]{/b} and she closes her door."
        );
      });
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
        g.text(
          "{b}[characters.witch.name]{/b} looks at you. There is a smile on her face."
        );

        g.character("witch").say(
          "Of course I can help you!",
          "But it won't be for free {b}honey{/b}.",
          "I want something from you first. A nice gift befitting of a lady."
        );
        g.character("witch").say(
          "If you can surprise me with a nice {b}Ornament{/b},",
          "I will help you with your with your medicine problem."
        );
        g.text("You let her know you will try your best.");

        g.onState(g.item("necklace").hasState("unknown"), () => {
          g.item("necklace").setState("need");
        });
      }
    );

    interaction(
      "Give gemstone to [characters.witch.name]",
      g.and(
        g.character("witch").hasState("visited"),
        g.item("necklace").hasState("need"),
        g.item("gemstone").hasState("possession")
      ),
      () => {
        g.text(
          "{b}[characters.witch.name]{/b} looks at you. There is a smile on her face."
        );
        g.character("witch").say(
          "That gem is a real beauty! But it is not an {b}Ornament{/b}."
        );
      }
    );

    interaction(
      "Give necklace to [characters.witch.name]",
      g.and(
        g.character("witch").hasState("visited"),
        g.item("necklace").hasState("possession")
      ),
      () => {
        // TODO
        // "1=12;12=1;2=0;4=2;34=8", "*c2", "Eucalypta kijkt je aan. Er komt een grote glimlach op haar gezicht."
        // "", "*c13", "Eucalypta: 'Die halsketting is super! Dit gaat een geweldig kado voor mijn dochter zijn!"
        // "  Momentje, dan schrijf ik een lijstje met benodigdheden.'"
        // "*c2", "", "Eucalypta gaat naar binnen en doet de deur dicht."
        // "Even later komt ze terug en geeft ze je een lijstje met benodigheden.", "", "&"
        // "1=12;12=1;2=0;4=2;34=8;0=2", "*c13"
        // "Eucalypta: 'Hier, dit heb ik allemaal nodig."
        // "  Er zit wel een lastig ingrediënt bij, maar een sterke vrouw zoals jij krijgt dat wel voor elkaar.'"
        // "&4=0;34=9;26=4"
        // "1=12;12=1;2=0;4=2;34=8;0=1", "*c13"
        // "Eucalypta: 'Hier, dit heb ik allemaal nodig."
        // "  Er zit wel een lastig ingrediënt bij, maar een sterke man zoals jij krijgt dat wel voor elkaar.'"
        // "&4=0;34=9;26=4"
      }
    );

    interaction(
      "Did you also hear that there is a treasure hidden somewhere?",
      g.character("witch").hasState("visited"),
      () => {
        g.character("player").say(
          "Did you also hear that there is a treasure hidden somewhere?"
        );
        g.character("witch").say(
          "Ah, you should not care about such things.",
          "The powers of {b}plants{/b} and {b}magic{/b} is much bigger than that of money."
        );
        g.text("This doesn't help you.");
      }
    );
    interaction("Goodbye", g.character("witch").hasState("visited"), () => {
      closeOverlay();
    });
  }
);
