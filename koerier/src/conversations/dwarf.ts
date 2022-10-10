import g from "../game";

g.defineOverlay(
  "dwarfConversation",
  ({ onEnter, interaction, closeOverlay, onLeave }) => {
    onEnter(() => {
      // potential place to do the posing of characters first.

      g.onState(
        g.character("dwarf").hasFlag("nameKnown"),
        () => {
          g.character("dwarf").say("Hallo [character.player.name]");
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
        "My name is [.defaultName], and who might you be?"
      );
      g.character("dwarf").clearCustomName();
      g.character("dwarf").setFlag("nameKnown", true);

      g.character("player").say("My name is [.name].");
      g.character("dwarf").say(
        "Hi [character.player.name]. Sorry for being rude.",
        "My pickaxe just broke, and I just discovered a vein of gemstones!",
        "I tried so hard to get them out of there.",
        "And now mu pickaxe is broken. And I'm hungry!"
      );
      g.text("[character.dwarf.name] sighs.");
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
      g.character("dwarf").hasFlag("nameKnown"),
      () => {
        g.character("player").say("How can I help you?");

        g.onState(g.not(g.item("pickaxe").hasState("given")), () => {
          g.character("dwarf").say(
            "Do you have any food for me?",
            "And a new pickaxe?"
          );
        });
        g.onState(g.item("pickaxe").hasState("given"), () => {
          g.character("dwarf").say("Do you have any food for me?");
        });

        // "", "*c9", "Thorin: 'Ik heb dringend een nieuwe houweel nodig.'", "&4=0"
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
      "I was able to repair your pickaxe",
      g.item("pickaxe").hasState("fixed"),
      () => {
        g.character("player").say("I was able to repair your pickaxe");
        g.character("dwarf").say("Really? show me!");
        g.character("player").say("Here you go.");
        g.text(
          "You give the repaired pickaxe to [character.dwarf.defaultName]. He gives it a thorough inspection."
        );
        g.character("dwarf").say("Wow, It's as good as new! Thanks!");
        g.item("pickaxe").setState("given");
      }
    );

    // "1=11;2=0;11>1;4=5", "*c3", "$n: 'Thorin, zou jij weten hoe ik aan medicijnen kan komen?'", "", "*c2"
    // "Thorin denkt even na.", "*c9", "", "Thorin: 'Geen idee, dat zou je het beste in het dorp kunnen vragen."
    // "  Ze hebben daar van alles. Het dorp ligt in het zuidoosten vanaf hier."
    // "  Eerst naar het oosten, dan naar het zuiden.'", "&4=0"

    // "1=11;2=0;11>1;4=6", "*c3", "$n: 'Thorin, zou jij weten hoe ik aan vervoer kan komen?'", "", "*c2"
    // "Thorin denkt even na.", "*c9", "", "Thorin: 'Hmm, de boer verderop heeft een paard."
    // "Je zou kunnen vragen of je hem mag lenen?'", "&4=0"

    interaction("Okay, I'm going.", g.always(), () => {
      closeOverlay();
    });

    onLeave(() => {
      // teardown
    });
  }
);
