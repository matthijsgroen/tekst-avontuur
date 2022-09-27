import { GameWorld } from "./world-types";

export type GameState<Game extends GameWorld> = {
  currentLocation: keyof Game["locations"];
  previousLocation?: keyof Game["locations"];
  items: {
    [K in keyof Game["items"]]?: {
      state: Game["items"][K]["states"] | "unknown";
    };
  };
  characters: {
    [K in keyof Game["characters"]]?: {
      state: Game["characters"][K]["states"] | "unknown";
      name?: string;
    };
  };
  locations: {
    [K in keyof Game["locations"]]?: {
      state: Game["locations"][K]["states"] | "unknown";
    };
  };
};

export type GameStateManager<Game extends GameWorld> = {
  getState: () => GameState<Game>;
  updateState: (
    mutation: (currentState: GameState<Game>) => GameState<Game>
  ) => void;
};
