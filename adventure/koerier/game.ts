import { world } from "../dsl/dsl";

export type Game = {
  locations: {
    forest: { flags: "visited" };
    farmland: { flags: "visited" };
    hills: { flags: "visited" };
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
  initialState: {
    currentLocation: "forest",
  },
  characterConfigs: {
    player: {
      defaultName: "Matthijs",
    },
  },
});

export default game;
