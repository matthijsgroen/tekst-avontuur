import g from "../game";

g.defineLocation("mine", ({ describe, interaction, onLeave }) => {
  onLeave("hills", () => {
    g.text("Je groet de dwerg en loopt weer richting de weg.");
  });

  describe(() => {
    g.onState(
      g.hasCharacterFlag("dwarf", "nameKnown"),
      () => {
        g.text(
          "Je bent bij de mijn",
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit [character.dwarf.defaultName] de dwerg."
        );
      },
      () => {
        g.character("dwarf").setName("Dwerg"); // This name should become translatable

        g.text(
          "Je bent bij de mijn",
          "Er ligt een mijnkarretje op zijn kant. Voor de ingang zit een dwerg."
        );
      }
    );
    g.text("Hij ziet er niet al te vrolijk uit.");
  });

  interaction("Praat met de dwerg", g.always(), () => {
    g.overlay(
      "dwarfConversation",
      ({ onStart, interaction, closeOverlay, onEnd }) => {
        onStart(() => {
          // potential place to do the posing of characters first.

          g.onState(
            g.hasCharacterFlag("dwarf", "nameKnown"),
            () => {
              g.character("dwarf").say("Hallo [character.player.name]");
            },
            () => {
              g.character("player").say("Hallo, kun je mij helpen?");
              g.text("De dwerg kijkt je een beetje boos aan.");
              g.character("dwarf").say("Grr. Stom ding, stom ding!");
            }
          );
        });

        const dwarfIntro = () => {
          g.character("dwarf").say(
            "Sorry voor mijn manieren, ik heb gewoon pech!",
            "Mijn naam is [.defaultName], en wie mag jij wezen?"
          );
          g.character("dwarf").clearCustomName();
          g.character("dwarf").setFlag("nameKnown", true);

          g.character("player").say("Mijn naam is [.name].");
          g.character("dwarf").say(
            "Hallo [character.player.name]. Sorry dat ik zo ruw deed.",
            "Mijn houweel is gebroken, en ik had net een ader van edelstenen ontdekt!",
            "Ik heb zo hard mijn best gedaan om ze los te krijgen.",
            "Nu is mijn houweel gebroken. En ik heb honger!'"
          );
          g.text("[character.dwarf.name] zucht diep.");
        };

        interaction(
          "Je bent zelf een stom ding!",
          g.not(g.hasCharacterFlag("dwarf", "nameKnown")),
          () => {
            g.character("player").say("Je bent zelf een stom ding!");
            g.character("dwarf").say("Ik heb het niet tegen jou!");
            dwarfIntro();
          }
        );
        interaction(
          "Pardon?",
          g.not(g.hasCharacterFlag("dwarf", "nameKnown")),
          () => {
            g.character("player").say("Pardon? Wie is een stom ding?");
            g.character("dwarf").say("Sorry, ik heb het niet tegen jou...");
            dwarfIntro();
          }
        );

        interaction("Oké, ik ga weer.", g.always(), () => {
          closeOverlay();
        });

        onEnd(() => {
          // teardown
        });
      }
    );
  });

  interaction("Verlaat mijn", g.always(), () => {
    g.travel("hills");
  });
});
