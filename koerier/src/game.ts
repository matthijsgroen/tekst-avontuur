import { hexColor, world } from "point-n-click";
import terminalTheme from "@point-n-click/theme-cli";
import bookTheme from "@point-n-click/theme-book";
import { GameState } from "./initialState";

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
    baker: {
      defaultName: "Gerst",
      textColor: hexColor("30cc30"),
    },
    daughter: {
      defaultName: "Bloem",
    },
    witch: {
      defaultName: "Eucalypta",
    },
  },
})(
  terminalTheme("Terminal", { color: true }),
  terminalTheme("Terminal (Black & White)", { color: false }),
  bookTheme("Book", { coverColor: "red" })
);

export default game;
