import { world } from "../dsl/dsl";

export type Game = {
  locations: {
    forest: { flags: "visited" };
    farmland: { flags: "visited" };
    hills: { flags: "visited" };
    mine: { flags: "visited" };
  };
  items: {
    bag: { states: "known" | "possession" };
    branch: { states: "possession" };
    horse: { states: "known" | "found" | "hooves" | "cart" };
  };
  characters: {
    player: {};
    dwarf: { flags: "nameKnown" };
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
    dwarf: {
      defaultName: "Thorin",
    },
  },
});

export default game;
