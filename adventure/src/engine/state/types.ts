import { GameWorld } from "../../dsl/world-types";

export type GameState<Game extends GameWorld> = {
  currentLocation: keyof Game["locations"];
  previousLocation?: keyof Game["locations"];
  overlayStack: string[];
  items: {
    [K in keyof Game["items"]]?: {
      state: Game["items"][K]["states"] | "unknown";
      flags: Record<string, boolean>;
    };
  };
  characters: {
    [K in keyof Game["characters"]]: {
      state: Game["characters"][K]["states"] | "unknown";
      flags: Record<string, boolean>;
      name: string | null;
      defaultName: string;
    };
  };
  locations: {
    [K in keyof Game["locations"]]: {
      state: Game["locations"][K]["states"] | "unknown";
      flags: Record<string, boolean>;
    };
  };
};

export type GameStateManager<Game extends GameWorld> = {
  getState: () => GameState<Game>;
  updateState: (
    mutation: (currentState: GameState<Game>) => GameState<Game>
  ) => void;
};
