import g from "../game";

g.defineLocation("smithy", ({ describe, interaction, onEnter }) => {
  onEnter("village", () => {
    g.onState(
      g.location("smithy").hasFlag("visited"),
      () => {
        g.character("armorer").say("Hey, welcome!");
        g.character("goldsmith").say("How can we help you?");
        // TODO: Update text after 'killing' dragon
      },
      () => {
        g.character("armorer").say("Hey, a new customer! Welcome!");
        g.character("farrier").say(
          "My name is {b}[characters.farrier.name]{/b}, the {b}farrier{/b}. {b}[characters.goldsmith.name]{/b} here is our {b}goldsmith{/b}."
        );
        g.character("goldsmith").say(
          "And our {b}armorer{/b} here is {b}[characters.armorer.name]{/b}."
        );
        g.character("armorer").say("How can we help you?");
        g.location("smithy").setFlag("visited");
      }
    );
  });

  describe(() => {
    g.descriptionText(
      "You are in a large Smithy.",
      "A farrier, a goldsmith and an armorer are standing behind a large counter."
    );
    g.descriptionText(
      "All kinds of {b}armor{/b} and {b}weapons{/b} are on display in the shop."
    );
  });

  interaction("Talk to smiths", g.always(), () => {
    g.openOverlay("smithsConversation");
  });

  interaction("Browse the shop", g.always(), () => {
    g.text(
      "You start browsing the smithy. Everywhere is gear with all kinds of price tags.",
      "You decide to take a closer look."
    );
    g.text(
      "In the meantime, the smiths behind the counter are starting a conversation."
    );
    g.character("armorer").say("No, that is really nonsense.");
    g.character("goldsmith").say(
      "I'm telling you its true. There is a treasure hidden here in the area."
    );
    g.character("armorer").say("And who's treasure would that be?");
    g.character("farrier").say(
      "I heard the rumors as well. There should be gold hidden somewhere."
    );
    g.character("armorer").say(
      "{i}Hush!{/i}. Don't talk so loud, we have customers in the shop."
    );
    g.text(
      "They continue to converse in whispers, but you can't understand it anymore."
    );

    g.item("treasureNotes").setState("existence");
  });

  interaction("Leave the shop", g.always(), () => {
    g.travel("village");
  });
});
