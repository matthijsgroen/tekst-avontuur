import g from "../game";

g.defineOverlay(
  "millerConversation",
  ({ onEnter, interaction, closeOverlay, onLeave }) => {
    onEnter(() => {
      g.character("miller").say("Hey hello there!");
    });

    interaction("Goodbye.", g.always(), () => {
      closeOverlay();
    });
  }
);
