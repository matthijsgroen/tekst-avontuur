import { createColorPalette, hexColor, world } from "point-n-click";
import terminalTheme from "@point-n-click/theme-cli";
import bookTheme from "@point-n-click/theme-book";
import { GameState } from "./initialState";

const palette = createColorPalette([
  "default",
  "player",
  "baker",
  "miller",
  "dwarf",
  "horse",
  "dragon",
  "farmer",
  "daughter",
  "witch",
  "farrier",
  "goldsmith",
  "armorer",
]);

const darkColors = palette.defineColorScheme({
  default: hexColor("18a81b"),
  player: hexColor("1aaaa9"),
  dwarf: hexColor("565cfb"),
  miller: hexColor("565cfb"),
  horse: hexColor("ee4040"),
  dragon: hexColor("cc40cc"),
  farmer: hexColor("30cc30"),
  baker: hexColor("30cc30"),
  daughter: hexColor("30cc30"),
  witch: hexColor("30cc30"),
  farrier: hexColor("e0e3d7"),
  goldsmith: hexColor("ffd700"),
  armorer: hexColor("2879C0"),
});

const lightColors = palette.defineColorScheme({
  default: hexColor("18181b"),
  player: hexColor("1aaaa9"),
  dwarf: hexColor("565cfb"),
  miller: hexColor("565cfb"),
  horse: hexColor("ee4040"),
  dragon: hexColor("cc40cc"),
  farmer: hexColor("30cc30"),
  baker: hexColor("30cc30"),
  daughter: hexColor("30cc30"),
  witch: hexColor("30cc30"),
  farrier: hexColor("30cc30"),
  goldsmith: hexColor("30cc30"),
  armorer: hexColor("30cc30"),
});

const game = world<GameState>({
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
  locales: {
    default: "en-US",
    supported: {
      ["en-US"]: "English",
      ["nl-NL"]: "Nederlands",
    },
  },
  colors: {
    lightPalette: lightColors,
    darkPalette: darkColors,
    defaultTextColor: palette.color("default"),
  },
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
      textColor: palette.color("player"),
    },
    dwarf: {
      defaultName: "Thorin",
      textColor: palette.color("dwarf"),
    },
    miller: {
      defaultName: "Tjasker",
      textColor: palette.color("miller"),
    },
    horse: {
      defaultName: "Teun",
      textColor: palette.color("horse"),
    },
    dragon: {
      defaultName: "Dins",
      textColor: palette.color("dragon"),
    },
    farmer: {
      defaultName: "Joe",
      textColor: palette.color("farmer"),
    },
    baker: {
      defaultName: "Gerst",
      textColor: palette.color("baker"),
    },
    daughter: {
      defaultName: "Bloem",
      textColor: palette.color("daughter"),
    },
    witch: {
      defaultName: "Eucalypta",
      textColor: palette.color("witch"),
    },
    farrier: {
      defaultName: "Luk",
      textColor: palette.color("farrier"),
    },
    goldsmith: {
      defaultName: "Luuk",
      textColor: palette.color("goldsmith"),
    },
    armorer: {
      defaultName: "Lucy",
      textColor: palette.color("armorer"),
    },
  },
})(
  bookTheme("Book", { coverColor: "red" }),
  terminalTheme("Terminal", { color: true }),
  terminalTheme("Terminal (Black & White)", {
    color: false,
  })
);

export default game;
