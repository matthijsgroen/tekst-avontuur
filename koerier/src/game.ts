import { hexColor, world, GameDefinition } from "point-n-click";

export type Game = GameDefinition<{
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
  };
  characters: {
    player: { counters: "coins" };
    dwarf: { flags: "nameKnown" };
    horse: { states: "known" | "found" | "hooves" | "cart" };
  };
  overlays: "dwarfConversation" | "inventory";
}>;

const game = world<Game>({
  defaultLocale: "en-US",
  defaultTextColor: hexColor("80ff80"),
  initialState: {
    currentLocation: "forest",
  },
  characterConfigs: {
    player: {
      defaultName: "Matthijs",
      textColor: hexColor("4040ff"),
    },
    dwarf: {
      defaultName: "Thorin",
      textColor: hexColor("ee4040"),
    },
    horse: {
      defaultName: "Teun",
      textColor: hexColor("ee4040"),
    },
  },
});

export default game;
