import g from "../game";

g.defineOverlay(
  "bakerConversation",
  ({ onEnter, onLeave, interaction, closeOverlay }) => {
    onEnter(() => {
      g.character("player").say(
        "Hello, my name is {b}[.name]{/b}.",
        "I'm urgently looking for a medicine for the king.",
        "Could you help me?"
      );
      g.text("The baker does not respond and is staring in the distance.");
    });

    onLeave(() => {
      g.onState(
        g.character("baker").hasState("unknown"),
        () => {
          g.text("You leave the baker alone and look around in the shop.");
        },
        () => {
          g.text("You say goodbye and start browsing the shop.");
        }
      );
    });

    interaction(
      "Hello, is everything alright?",
      g.character("baker").hasState("unknown"),
      () => {
        g.text("The baker is staring in the distance.");
        g.character("player").say("Hello? Are you alright?");
        g.text("The baker is startled. He was not aware you were in his shop.");
        g.character("baker").say("Oh. Sorry, I was not paying attention.");
        g.text("The baker bursts out in tears.");
        g.character("baker").say(
          "My {i}poor daughter!{/i} I will never see her again!",
          "That monsterous beast has her! I am sure of it!"
        );
        g.character("baker").say(
          "I baked some {b}cookies{/b} this morning, but I'm out of flour now.",
          "And that is a problem, especially since the {b}mill{/b} broke down.",
          "And I need to make more money so that I can hire a {b}knight{/b},",
          "to slay that monster and save my daughter!"
        );
        g.character("baker").setState("intro");
      }
    );

    interaction(
      "Monster? What are you talking about?",
      g.character("baker").hasState("intro"),
      () => {
        g.character("player").say("Monster? What are you talking about?");
        g.character("baker").say(
          "That ... beast ... in that tower ... in the woods."
        );
        g.text("The baker keeps sobbing.");
        g.character("player").say(
          "Why do you think the 'monster' has your daughter?"
        );
        g.character("baker").say(
          "My daughter is now missing for two days,",
          "and there is a {i}terrifying roar{/i} coming out of the dark woods for two days as well.",
          "It all started with that big fire at farmer {b}[characters.farmer.name]{/b}'s place.",
          "That creature must have my sweet {b}[characters.daughter.name]{/b}..."
        );
        g.character("baker").setState("visited");
        g.onState(g.character("dragon").hasState("unknown"), () => {
          g.character("dragon").setState("known");
        });
      }
    );

    interaction(
      "Could you tell me more about that monster?",
      g.and(
        g.character("baker").hasState("visited"),
        g.not(g.item("cookies").hasState("buying"))
      ),
      () => {
        g.character("player").say("Could you tell me more about that monster?");
        g.character("baker").say(
          "That creature lives {b}east{/b} of here, in the {b}dark woods{/b}."
        );
        g.text("The baker has trouble not to start crying.");
        g.character("baker").say(
          "My daughter is now missing for two days,",
          "and there is a {i}terrifying roar{/i} coming out of the dark woods for two days as well."
        );
        g.character("baker").say(
          "We have to slay the beast! And save my daughter!",
          "You won't happen to have a {b}sword{/b} do you?",
          "You definitely would need a {b}sword{/b} to defend you against that monster."
        );
        g.item("sword").setState("need");
        g.character("baker").setFlag("toldDragon");
      }
    );

    interaction(
      "Do you know where I could get any medicine?",
      g.and(
        g.character("baker").hasState("visited"),
        g.item("medicine").hasState("unknown"),
        g.not(g.item("cookies").hasState("buying"))
      ),
      () => {
        g.character("player").say("Do you sell any medicine?");
        g.text("{b}[characters.baker.name]{/b} turns red.");
        g.character("baker").say(
          "Uh, maybe... There is this lady see... uh...",
          "She lives in the {b}swamp{/b}. Lots of people are calling her a witch.",
          "She could definitely help you."
        );
        g.character("baker").say(
          "Don't tell anyone, but if people come here for medicine I always get those from her.",
          "People don't dare to visit her. But she practically makes all medication for everyone here."
        );
        g.character("baker").say(
          "Normally I would arrange it for you, but with the current situation with my daughter {b}[characters.daughter.name]{/b}..."
        );
        g.character("baker").say(
          "... Let's say I'm avoiding her for the moment."
        );
        g.item("medicine").setState("location");
      }
    );

    interaction(
      "Hello, can I buy something to eat?",
      g.and(
        g.character("baker").hasState("visited"),
        g.item("cookies").hasState("unknown")
      ),
      () => {
        g.character("player").say("Hello? Can I buy something to eat?");
        g.text(
          "The baker suddenly realizes that there is someone in his store."
        );
        g.character("baker").say(
          "Oh sorry, I didn't see you there.",
          "We do have some {b}cookies{/b} for sale.",
          "The price is {b}2 coins{/b}."
        );
        g.item("cookies").setState("price");
      }
    );

    interaction(
      "I would like to buy some cookies",
      g.item("cookies").hasState("price"),
      () => {
        g.character("player").say("I would like to buy some cookies.");
        g.character("baker").say("That will be {b}2 coins{/b}.");
        g.item("cookies").setState("buying");
      }
    );

    interaction(
      "Here you go, 2 coins",
      g.and(
        g.item("cookies").hasState("buying"),
        g.character("player").hasCounter("coins").moreThanEquals(2)
      ),
      () => {
        g.character("player").say("Here you go, 2 coins.");
        g.character("baker").say("Here you go.");
        g.text("The baker gives a few delicious cookies.");
        g.item("cookies").setState("possession");
        g.character("player").decreaseCounter("coins", 2);
      }
    );

    interaction(
      "Hmm, maybe another time",
      g.item("cookies").hasState("buying"),
      () => {
        g.character("player").say("Hmm, maybe another time.");
        g.character("baker").say("Okay, fine");
        g.item("cookies").setState("price");
      }
    );

    interaction("Never mind", g.character("baker").hasState("unknown"), () => {
      closeOverlay();
    });

    interaction(
      "Goodbye",
      g.and(
        g.character("baker").hasState("visited"),
        g.not(g.item("cookies").hasState("buying"))
      ),
      () => {
        closeOverlay();
      }
    );
  }
);
