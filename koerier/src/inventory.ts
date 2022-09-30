import { GameWorld, Interaction } from "point-n-click";
import g from "./game";

export const inventory = <Game extends GameWorld>(
  interaction: Interaction<Game>
) => {
  interaction("Open tas", g.isItemState("bag", "possession"), () => {
    g.overlay("inventory", ({ onStart, interaction, closeOverlay }) => {
      onStart(() => {
        g.text("Je hebt het volgende bij je:");
        g.onState(g.isItemState("branch", "possession"), () => {
          g.text("- Een stok, opgeraapt in het bos");
        });
        g.onState(g.isItemState("pickaxe", "broken"), () => {
          g.text("- Houweel met gebroken heft");
        });
        g.onState(g.isItemState("pickaxe", "fixed"), () => {
          g.text("- Gerepareerde houweel");
        });
      });

      interaction(
        "Repareer houweel met stok uit het bos",
        g.and(
          g.isItemState("branch", "possession"),
          g.isItemState("pickaxe", "broken")
        ),
        () => {
          g.text(
            "Je verwijdert het oude heft van de houweel,",
            "en vervangt het met de stok uit het bos."
          );
          g.text("Het past! Het houweel is weer zo goed als nieuw.");
          g.item("branch").setState("used");
          g.item("pickaxe").setState("fixed");
        }
      );

      interaction("Sluit tas", g.always(), () => {
        closeOverlay();
      });
    });
  });
};
