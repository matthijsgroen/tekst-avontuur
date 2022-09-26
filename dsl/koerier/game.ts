import { world } from "../src/dsl";

type Game = {
  locations: {
    forest: { states: "default" };
    farmland: { states: "default" };
    hills: { states: "default" };
  };
  items: {
    bag: { states: "known" | "possession" };
    branch: { states: "known" | "possession" };
    horse: { states: "known" | "found" | "hooves" | "cart" };
  };
  characters: {
    player: { states: "default" };
  };
};

const game = world<Game>({
  defaultLocale: "nl-NL",
  startLocation: "forest",
});

export default game;
