import g from "../game";

g.defineOverlay(
  "dwarfConversation",
  ({ onEnter, interaction, closeOverlay, onLeave }) => {
    onEnter(() => {
      // potential place to do the posing of characters first.

      g.onState(
        g.character("dwarf").hasFlag("nameKnown"),
        () => {
          g.character("dwarf").say("Hello [characters.player.name]");
        },
        () => {
          g.character("player").say("Hello, can you help me?");
          g.text("The dwarf looks grumpy in your direction.");
          g.character("dwarf").say("Grr. Stupid thing, stupid thing!");
        }
      );
    });

    const dwarfIntro = () => {
      g.character("dwarf").say(
        "Pardon my manners, I just have bad luck!",
        "My name is {b}[.defaultName]{/b}, and who might you be?"
      );
      g.character("dwarf").clearCustomName();
      g.character("dwarf").setFlag("nameKnown");

      g.character("player").say("My name is {b}[.name]{/b}.");
      g.character("dwarf").say(
        "Hi {b}[characters.player.name]{/b}. Sorry for being rude.",
        "My {b}pickaxe{/b} just broke, and I just discovered a vein of {b}gemstones{/b}!",
        "I tried so hard to get them out of there.",
        "And now my pickaxe is broken. And I'm hungry!"
      );
      g.text("[characters.dwarf.name] sighs.");
    };

    const dwarfHappy = () => {
      g.text(
        "[characters.dwarf.name] gets up, and walks into the mine. With new energy and a repaired pickaxe, he starts to chop on some {b}gems{/b}."
      );
      g.character("dwarf").say("Hei ho, Hei ho....");
      g.text(
        "Pieces of {b}gemstone{/b} fall on the ground. One piece rolls towards you."
      );
      g.character("dwarf").say(
        "You can have that. I have plenty now! Thanks again for all your help!"
      );
      g.character("dwarf").setState("happy");
      g.item("gemstone").setState("chopped");
    };

    interaction(
      "You are stupid yourself!",
      g.not(g.character("dwarf").hasFlag("nameKnown")),
      () => {
        g.character("player").say("You are stupid yourself!");
        g.character("dwarf").say("I wasn't talking to you!");
        dwarfIntro();
      }
    );
    interaction(
      "Excuse me?",
      g.not(g.character("dwarf").hasFlag("nameKnown")),
      () => {
        g.character("player").say("Excuse me? Who is stupid here?");
        g.character("dwarf").say("Sorry, I wasn't talking to you...");
        dwarfIntro();
      }
    );

    interaction(
      "How can I help you?",
      g.and(
        g.character("dwarf").hasFlag("nameKnown"),
        g.not(g.character("dwarf").hasState("happy"))
      ),
      () => {
        g.character("player").say("How can I help you?");

        g.onState(
          g.and(
            g.not(g.item("pickaxe").hasState("given")),
            g.not(g.item("cookies").hasState("given"))
          ),
          () => {
            g.character("dwarf").say(
              "Do you have any food for me?",
              "And a new pickaxe?"
            );
          },
          () => {
            g.onState(g.not(g.item("cookies").hasState("given")), () => {
              g.character("dwarf").say("Do you have any food for me?");
            });
            g.onState(g.not(g.item("pickaxe").hasState("given")), () => {
              g.character("dwarf").say("I really need a new pickaxe.");
            });
          }
        );
      }
    );

    interaction(
      "Can I have your pickaxe?",
      g.and(
        g.character("dwarf").hasFlag("nameKnown"),
        g.item("pickaxe").hasState("unknown")
      ),
      () => {
        g.character("player").say("Can I have your pickaxe?");
        g.character("dwarf").say(
          "Sure. But only the top half is intact.",
          "The hilt is broken. It's no use to me."
        );
        g.text("You put the top if the pickaxe in your bag.");
        g.item("pickaxe").setState("broken");
      }
    );

    interaction(
      "Give cookies to [characters.dwarf.name]",
      g.and(
        g.character("dwarf").hasFlag("nameKnown"),
        g.item("cookies").hasState("possession")
      ),
      () => {
        g.text("You offer the cookies to [characters.dwarf.name].");
        g.character("player").say("Here you go.");
        g.character("dwarf").say("Wow those smell nice!");
        g.text("[characters.dwarf.name] is enjoying the cookies.");
        g.character("dwarf").say("I really needed that, thanks!");
        g.item("cookies").setState("given");
        g.onState(g.item("pickaxe").hasState("given"), () => {
          dwarfHappy();
        });
      }
    );

    interaction(
      "I was able to repair your pickaxe",
      g.item("pickaxe").hasState("fixed"),
      () => {
        g.character("player").say("I was able to repair your pickaxe.");
        g.character("dwarf").say("Really? show me!");
        g.character("player").say("Here you go.");
        g.text(
          "You give the repaired pickaxe to [characters.dwarf.name]. He gives it a thorough inspection."
        );
        g.character("dwarf").say("Wow, It's as good as new! Thanks!");
        g.item("pickaxe").setState("given");
        g.onState(g.item("cookies").hasState("given"), () => {
          dwarfHappy();
        });
      }
    );

    interaction(
      "Any idea how I could get any medicine?",
      g.character("dwarf").hasFlag("nameKnown"),
      () => {
        g.character("player").say(
          "{b}[characters.dwarf.name]{/b}, would you happen to know how I could get some medicine?"
        );
        g.text("[characters.dwarf.name] thinks.");
        g.character("dwarf").say(
          "No idea, Its best to ask around in the {b}village{/b}.",
          "They have all kinds of things there.",
          "The village is southeast of here.",
          "First {b}east{/b}, then {b}south{/b}."
        );
      }
    );

    interaction(
      "Any idea how I could get any transportation?",
      g.character("dwarf").hasFlag("nameKnown"),
      () => {
        g.character("player").say(
          "{b}[characters.dwarf.name]{/b}, would you happen to know how I could get some transportation?"
        );
        g.text("[characters.dwarf.name] thinks.");
        g.character("dwarf").say(
          "Hmm, the {b}farmer{/b} nearby has a horse.",
          "Maybe you can borrow it?"
        );
      }
    );

    interaction(
      "Did you also hear that there is a treasure hidden somewhere?",
      g.and(
        g.character("dwarf").hasFlag("nameKnown"),
        g.not(g.item("treasureNotes").hasState("unknown"))
      ),
      () => {
        g.character("player").say(
          "Did you also hear that there is a treasure hidden somewhere?"
        );
        g.character("dwarf").say(
          "People are talking about my {i}mine{/i}!",
          "But this mine is {b}mine{/b}! This is {i}my{/i} treasure."
        );
        g.text(
          "[characters.dwarf.name] clearly thinks you mean something else."
        );
      }
    );

    interaction("Sorry, I have to go", g.always(), () => {
      closeOverlay();
    });

    onLeave(() => {
      g.text("You greet the dwarf.");
    });
  }
);
