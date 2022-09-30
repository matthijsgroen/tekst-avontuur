import g from "../game";
import { inventory } from "../inventory";

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

  inventory(interaction);

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

        interaction(
          "Hoe kan ik je helpen?",
          g.hasCharacterFlag("dwarf", "nameKnown"),
          () => {
            g.character("player").say("Hoe kan ik je helpen?");

            g.onState(g.not(g.isItemState("pickaxe", "given")), () => {
              g.character("dwarf").say(
                "Heb je iets te eten voor mij?",
                "En een nieuwe houweel?"
              );
            });
            g.onState(g.isItemState("pickaxe", "given"), () => {
              g.character("dwarf").say("Heb je iets te eten voor mij?");
            });

            // "", "*c9", "Thorin: 'Ik heb dringend een nieuwe houweel nodig.'", "&4=0"
          }
        );

        interaction(
          "Zou ik je houweel mogen hebben?",
          g.and(
            g.hasCharacterFlag("dwarf", "nameKnown"),
            g.isItemState("pickaxe", "unknown")
          ),
          () => {
            g.character("player").say("Zou ik je houweel mogen hebben?");
            g.character("dwarf").say(
              "Tuurlijk. alleen de bovenkant is nog heel.",
              "Het heft is gebroken. Ik heb er niets meer aan."
            );
            g.text("Je stopt de bovenkant van de houweel in je tas.");
            g.item("pickaxe").setState("broken");
          }
        );

        interaction(
          "Ik heb je houweel kunnen repareren.",
          g.isItemState("pickaxe", "fixed"),
          () => {
            g.character("player").say("Ik heb je houweel kunnen reparerern.");
            g.character("dwarf").say("Echt waar? Laat zien.");
            g.character("player").say("Alsjeblieft.");
            g.text(
              "Je geeft de gerepareerde houweel aan Thorin. Hij bekijkt hem grondig."
            );
            g.character("dwarf").say(
              "Wauw, hij is zo goed als nieuw! Bedankt!"
            );
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
