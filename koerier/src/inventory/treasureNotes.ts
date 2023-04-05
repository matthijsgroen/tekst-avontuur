import g from "../game";

g.defineOverlay("treasureNotes", ({ onEnter, interaction, closeOverlay }) => {
  onEnter(() => {
    g.text("You check the notes you collected on the mysterious treasure.");

    g.note(() => {
      g.onState(g.item("treasureNotes").hasFlag("moonStone"), () => {
        g.text(
          "- In the {b}swamp{/b}, underneath a plant with {b}diamond-shaped{/b} leaves, lies a {b}Moonstone{/b}.",
          "- When this stone {b}glows{/b}, you need to walk {b}a route{/b}."
        );
      });
      g.onState(g.item("treasureNotes").hasFlag("route"), () => {
        g.text("- The route to walk is {b}NNWNNES{/b}");
      });
      g.onState(g.item("treasureNotes").hasFlag("startPoint"), () => {
        g.text("- The route starts at the {b}river{/b}");
      });
    });

    g.onState(
      g.and(
        g.item("treasureNotes").hasFlag("moonStone"),
        g.not(g.item("treasureNotes").hasFlag("route"))
      ),
      () => {
        g.text("You have no clue what this route is.");
      }
    );
    g.onState(
      g.and(
        g.item("treasureNotes").hasFlag("moonStone"),
        g.not(g.item("treasureNotes").hasFlag("startPoint"))
      ),
      () => {
        g.text("You have no clue where this route starts.");
      }
    );

    g.onState(
      g.and(
        g.item("treasureNotes").hasFlag("moonStone"),
        g.item("treasureNotes").hasFlag("route"),
        g.item("treasureNotes").hasFlag("startPoint")
      ),
      () => {
        g.text("You think you have all the information you need.");
      },
      () => {
        g.text("You think you are still missing some information.");
      }
    );
  });

  interaction("Put note away", g.always(), () => {
    closeOverlay();
  });
});
