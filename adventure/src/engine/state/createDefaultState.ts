import { GameModel } from "../../dsl/ast-types";
import { GameState } from "./types";
import { GameWorld } from "../../dsl/world-types";

export const createDefaultState = <Game extends GameWorld>(
  gameModel: GameModel<Game>
): GameState<Game> => ({
  currentLocation: gameModel.locations[0].id,
  overlayStack: [],
  items: {},
  characters: Object.entries(gameModel.settings.characterConfigs).reduce<
    Partial<GameState<Game>["characters"]>
  >((map, [characterId, settings]) => {
    return {
      ...map,
      [characterId]: {
        state: "unknown",
        flags: {},
        name: null,
        defaultName: settings.defaultName,
      },
    };
  }, {}) as GameState<Game>["characters"],
  locations: gameModel.locations.reduce<Partial<GameState<Game>["locations"]>>(
    (map, currentLocation) => {
      return {
        ...map,
        [currentLocation.id]: {
          state: "unknown",
          flags: {},
        },
      };
    },
    {}
  ) as GameState<Game>["locations"],
});
