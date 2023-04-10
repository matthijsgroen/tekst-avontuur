import g from "../game";

g.defineLocation("treasureRoute", ({ interaction }) => {
  const endRoute = () => {
    g.descriptionText("");
    g.text("You get the feeling you went the wrong way.");
    g.location("treasureRoute").setCounter("steps", -1);
  };

  interaction(
    "Inspect tree",
    g.location("treasureRoute").hasCounter("steps").equals(7),
    () => {
      g.text(
        "After inspection of the tree, you discover that it is {b}hollow{/b}.",
        "There is a small chest inside.",
        "You pick up the chest."
      );
      g.location("treasureRoute").increaseCounter("steps", 1);
    }
  );

  interaction(
    "Go north, to the village",
    g.location("treasureRoute").hasCounter("steps").equals(0),
    () => {
      g.text(
        "It is dark, you are at the {b}village{/b}. All shops are closed."
      );
      g.text("It's now even more quiet than it already was at daytime.");
      g.location("treasureRoute").increaseCounter("steps", 1);
    }
  );

  interaction(
    "Go east, along the waterfront",
    g.location("treasureRoute").hasCounter("steps").equals(0),
    () => {
      g.text("You walk along the river to the {b}east{/b}.");
      endRoute();
    }
  );
  interaction(
    "Go west, along the waterfront",
    g.location("treasureRoute").hasCounter("steps").equals(0),
    () => {
      g.text("You walk along the river to the {b}west{/b}.");
      endRoute();
    }
  );

  interaction(
    "Wait for daylight",
    g.location("treasureRoute").hasCounter("steps").equals(0),
    () => {
      g.text("You wait till the sun comes up.");
      g.item("treasureHunt").clearFlag("active");

      g.travel("river");
    }
  );

  interaction(
    "Go north, towards the farmlands",
    g.location("treasureRoute").hasCounter("steps").equals(1),
    () => {
      g.text("You go {b}north{/b} to the farmlands.");
      g.descriptionText("");
      g.text(
        "You are at the farmlands. It is really dark.",
        "A glow of fire is coming from the other side of the farm"
      );
      g.location("treasureRoute").increaseCounter("steps", 1);
    }
  );

  interaction(
    "Go east, towards the dark wood",
    g.location("treasureRoute").hasCounter("steps").equals(1),
    () => {
      g.text("You go {b}east{/b}, towards the dark woods.");
      endRoute();
    }
  );

  interaction(
    "Go south, towards the river",
    g.location("treasureRoute").hasCounter("steps").equals(1),
    () => {
      g.text("You decide to walk back to the starting point at the river.");
      g.location("treasureRoute").setCounter("steps", 0);

      g.travel("river");
    }
  );

  interaction(
    "Go west, between some shops",
    g.location("treasureRoute").hasCounter("steps").equals(1),
    () => {
      g.text(
        "You go {b}west{/b} between some shops, even if there is no real path."
      );
      endRoute();
    }
  );

  interaction(
    "Go north, between the meadows",
    g.location("treasureRoute").hasCounter("steps").equals(2),
    () => {
      g.text(
        "You go {b}north{/b} across some meadows.",
        "You almost trip in the darkness."
      );
      endRoute();
    }
  );

  interaction(
    "Go east, to the other side of the farm",
    g.location("treasureRoute").hasCounter("steps").equals(2),
    () => {
      g.text(
        "You go {b}east{/b}, to the other side of the farm.",
        "You get some light from the fiery glow, and continue.",
        "You start to feel lost."
      );
      endRoute();
    }
  );

  interaction(
    "Go south, towards village",
    g.location("treasureRoute").hasCounter("steps").equals(2),
    () => {
      g.text(
        "You go {b}south{/b}, back to the village.",
        "You were here before."
      );
      endRoute();
    }
  );

  interaction(
    "Go west, towards the forest",
    g.location("treasureRoute").hasCounter("steps").equals(2),
    () => {
      g.text("You go {b}west{/b}, to the forest.");
      g.descriptionText("");
      g.text(
        "You are at the forest. It is now incredibly dark.",
        "Luckily you can use the {b}Moonstone{/b} for some light."
      );
      g.descriptionText("");
      g.descriptionText(
        "There are {b}farmlands{/b} in the {b}east{/b}.",
        "There are {b}hills{/b} in the {b}west{/b}."
      );
      g.location("treasureRoute").increaseCounter("steps", 1);
    }
  );

  interaction(
    "Go north, between some trees",
    g.and(
      g.location("treasureRoute").hasCounter("steps").moreThan(2),
      g.location("treasureRoute").hasCounter("steps").lessThan(8)
    ),
    () => {
      g.onState(
        g.location("treasureRoute").hasCounter("steps").equals(3),
        () => {
          g.descriptionText(
            "You walk between the trees {b}northwards{/b}, until you come to an open clearing."
          );
          g.location("treasureRoute").increaseCounter("steps", 1);
        },
        () => {
          g.onState(
            g.location("treasureRoute").hasCounter("steps").equals(4),
            () => {
              g.descriptionText(
                "You walk between the trees {b}northwards{/b}, until you come to another open clearing."
              );
              g.location("treasureRoute").increaseCounter("steps", 1);
            },
            () => {
              g.descriptionText(
                "You walk between the trees {b}northwards{/b}."
              );
            }
          );
        }
      );
    }
  );
  interaction(
    "Go east, to the farmlands",
    g.location("treasureRoute").hasCounter("steps").equals(3),
    () => {
      () => {
        g.descriptionText("You walk {b}eastwards{/b}, to the farmlands.");
        g.descriptionText("you were here before.");
        endRoute();
      };
    }
  );
  interaction(
    "Go east, between some trees",
    g.and(
      g.location("treasureRoute").hasCounter("steps").moreThan(3),
      g.location("treasureRoute").hasCounter("steps").lessThan(8)
    ),
    () => {
      g.onState(
        g.location("treasureRoute").hasCounter("steps").equals(5),
        () => {
          g.descriptionText(
            "You walk {b}eastwards{/b} through the forest, using your {b}Moonstone{/b} to illuminate the forest floor.",
            "You continue on until you reach a small stream flowing through the forest."
          );
          g.location("treasureRoute").increaseCounter("steps", 1);
        },
        () => {
          g.descriptionText("You walk between the trees {b}eastwards{/b}.");
          endRoute();
        }
      );
    }
  );
  interaction(
    "Go south, between some trees",
    g.and(
      g.location("treasureRoute").hasCounter("steps").moreThan(2),
      g.location("treasureRoute").hasCounter("steps").lessThan(8)
    ),
    () => {
      g.descriptionText("You walk {b}southwards{/b}, between the trees.");
      g.onState(
        g.location("treasureRoute").hasCounter("steps").equals(6),
        () => {
          g.descriptionText("You reach a fallen tree.");
          g.location("treasureRoute").increaseCounter("steps", 1);
        },
        endRoute
      );
    }
  );
  interaction(
    "Go west, towards the hills",
    g.location("treasureRoute").hasCounter("steps").equals(3),
    () => {
      g.descriptionText("You walk {b}westwards{/b}, towards the hills.");
      g.descriptionText("You see the outline of a mill in the moonlight.");
      endRoute();
    }
  );
  interaction(
    "Go west, between some trees",
    g.and(
      g.location("treasureRoute").hasCounter("steps").moreThan(3),
      g.location("treasureRoute").hasCounter("steps").lessThan(8)
    ),
    () => {
      g.descriptionText("You walk {b}westwards{/b}, between the trees.");
      g.text("You almost trip over a tree stump.");
      g.character("player").say("Ouch! Grmbl!");
      endRoute();
    }
  );

  interaction(
    "Open chest",
    g.location("treasureRoute").hasCounter("steps").equals(8),
    () => {
      g.text("You open the {b}chest{/b}.");
      g.text(
        "There is {b}gold{/b} inside!",
        "It also contains a stone with {b}weird inscriptions{/b}."
      );
      g.descriptionText("");
      g.text(
        "You put the gold and stone in your {b}bag{/b}.",
        "The gold shimmers a bit different than you are used from gold."
      );
      g.descriptionText("");
      g.text(
        "You walk back to the trail in the forest.",
        "The sun is coming up again."
      );
      g.descriptionText("");
      g.item("treasureHunt").clearFlag("active");
      g.item("treasureHunt").setFlag("done");
      g.item("gold").setState("possession");
      g.item("runeStone").setState("possession");
      g.travel("forest");
    }
  );

  interaction(
    "Return to starting point",
    g.location("treasureRoute").hasCounter("steps").equals(-1),
    () => {
      g.text("You decide to walk back to the starting point at the river.");
      g.location("treasureRoute").setCounter("steps", 0);

      g.travel("river");
    }
  );
});
