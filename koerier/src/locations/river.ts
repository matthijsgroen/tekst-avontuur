import g from "../game";

g.defineLocation("river", ({ describe, interaction }) => {
  describe(() => {
    g.text(
      "You are standing near a collapsed bridge that used to cross the river.",
      "There seems no way to cross it now. The river is too wild."
    );
    g.character("player").say(
      "Drat, I can't leave this area. This was the road towards the castle of the King,",
      "and even then I needed a few days on horse to reach it. I have to find a solution to cross the river."
    );
    g.onState(g.item("fabric").hasState("unknown"), () => {
      g.text(
        "A big piece of {b}white cloth, with red dots{/b} lies near the waterfront."
      );
    });
    g.onState(
      g.and(
        g.not(g.character("horse").hasFlag("found")),
        g.character("horse").hasFlag("known")
      ),
      () => {
        g.text(
          "In the corner of your eye, along the water, you see a horse drinking."
        );
      }
    );
    g.onState(
      g.and(
        g.character("horse").hasState("river"),
        g.character("horse").hasFlag("found")
      ),
      () => {
        g.text(
          "In the corner of your eye, along the water, you see {b}[character.horse.name]{/b} drinking."
        );
      }
    );
  });

  interaction(
    "Pick up [character.horse.name]",
    g.and(
      g.character("horse").hasFlag("found"),
      g.character("horse").hasState("river")
    ),
    () => {
      g.character("player").say(
        "Will you come with me {b}[character.horse.name]{/b}?"
      );

      g.text("The horse approaches you enthusiastically.");
      g.character("player").say("Nice to see you again!");
      g.text("{b}[character.horse.name]{/b} is now following you!");

      g.character("horse").setState("following");
    }
  );

  interaction(
    "Try to approach horse",
    g.and(
      g.not(g.character("horse").hasFlag("found")),
      g.character("horse").hasFlag("known")
    ),
    () => {
      g.text("You carefully approach the horse.");
      g.character("player").say(
        "Come with me {b}[character.horse.name]{/b}, I'll take you home."
      );

      g.text(
        "The horse stays calm, and walks along with you.",
        "You notice that the horse is limping a bit."
      );
      g.character("player").say("Okay, we will take it slow.");
      g.text("{b}[character.horse.name]{/b} is now following you!");

      g.character("horse").setState("following");
      g.character("horse").setFlag("found");
    }
  );

  interaction(
    "Let [character.horse.name] drink some water",
    g.character("horse").hasState("following"),
    () => {
      g.text(
        "You bring {b}[character.horse.name]{/b} to the waterfront to let him drink."
      );
      g.character("horse").setState("river");
    }
  );

  interaction(
    "Try to pick up fabric",
    g.item("fabric").hasState("unknown"),
    () => {}
  );

  interaction("Go north, towards the village", g.always(), () => {
    g.travel("village");
  });
});
