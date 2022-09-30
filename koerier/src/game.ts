import { world } from "point-n-click";

export type Game = {
  locations: {
    forest: { flags: "visited" };
    farmland: { flags: "visited" };
    hills: { flags: "visited" };
    mine: { flags: "visited" };
  };
  items: {
    bag: { states: "known" | "possession" };
    branch: { states: "possession" | "used" };
    pickaxe: { states: "broken" | "fixed" | "given" };
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
