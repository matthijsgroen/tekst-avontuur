import { hexColor, world, GameDefinition } from "point-n-click";

export type Game = GameDefinition<{
  locations: {
    forest: { flags: "visited" };
    farmland: { flags: "visited" };
    hills: { flags: "visited" };
    mine: { flags: "visited" };
    mill: { flags: "visited"; states: "fixed" };
    swamp: {};
    farm: { flags: "visited" };
  };
  items: {
    bag: { states: "known" | "possession" };
    branch: { states: "possession" | "used" };
    pickaxe: { states: "broken" | "fixed" | "given" };
    rope: { states: "possession" };
  };
  characters: {
    player: { counters: "coins"; flags: "male" };
    dwarf: { flags: "nameKnown" };
    miller: {};
    horse: { states: "known" | "found" | "hooves" | "cart" };
    farmer: {};
  };
  overlays: "dwarfConversation" | "millerConversation" | "inventory";
}>;

const game = world<Game>({
  defaultLocale: "en-US",
  defaultTextColor: hexColor("18a81b"),
  initialState: {
    currentLocation: "forest",
    characters: {
      player: {
        flags: {
          male: true,
        },
      },
    },
  },
  characterConfigs: {
    player: {
      defaultName: "Matthijs",
      textColor: hexColor("1aaaa9"),
    },
    dwarf: {
      defaultName: "Thorin",
      textColor: hexColor("565cfb"),
    },
    miller: {
      defaultName: "Tjasker",
      textColor: hexColor("565cfb"),
    },
    horse: {
      defaultName: "Teun",
      textColor: hexColor("ee4040"),
    },
    farmer: {
      defaultName: "Joe",
      textColor: hexColor("ee4040"),
    },
  },
});

export default game;
