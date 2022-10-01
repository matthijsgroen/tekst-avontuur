import g from "../game";
import { inventory } from "../inventory";

g.defineLocation("hills", ({ describe, onLeave, interaction }) => {
  onLeave("forest", () => {
    g.text("Je wandelt naar het oosten, richting het bos.");
  });
  onLeave("mine", () => {
    g.text("Je loopt van de weg af, richting de ingang van de mijn.");
  });

  describe(() => {
    g.text(
      "Je staat in de heuvels. De zon schijnt heerlijk.",
      "Er is een mooi uitzicht van de omgeving."
    );
    g.text(
      "Verder op het pad zie je een mijn.",
      "Iets hoger op de heuvel staat een molen.",
      "Naar het oosten loopt het pad naar het bos.",
      "Naar het zuiden loopt een drassig pad een moeras in."
    );
  });

  inventory(interaction);

  interaction("Spring op paard", g.never(), () => {
    // out of scope of proto
  });
  interaction("Volg het pad naar de mijn", g.always(), () => {
    g.travel("mine");
  });

  interaction("Wandel naar de molen", g.never(), () => {
    // out of scope of proto
  });

  interaction("Ga naar het oosten, richting het groene bos", g.always(), () => {
    g.travel("forest");
  });

  interaction("Ga naar het zuiden, richting het moeras", g.never(), () => {
    // out of scope of proto
  });
});
