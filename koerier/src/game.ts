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
    village: { flags: "visited" };
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
    dragon: { states: "known" | "found" };
    farmer: { flags: "visited" };
  };
  overlays:
    | "dwarfConversation"
    | "millerConversation"
    | "farmerConversation"
    | "inventory";
}>;

const game = world<Game>({
  gameTitle: "Courier for the king",
  meta: {
    author: "Matthijs Groen",
    credits: [
      { role: "Story & Writing", names: ["Matthijs Groen"] },
      { role: "Programming", names: ["Matthijs Groen"] },
      {
        role: "Play testing",
        names: ["Matthijs Groen", "Hiddo Groen", "Jinte Groen"],
      },
    ],
  },
  themes: [
    {
      name: "Terminal",
      themePackage: "@point-n-click/theme-cli",
      settings: { color: true },
    },
  ],
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
    dragon: {
      defaultName: "Dins",
      textColor: hexColor("cc40cc"),
    },
    farmer: {
      defaultName: "Joe",
      textColor: hexColor("30cc30"),
    },
  },
});

export default game;
