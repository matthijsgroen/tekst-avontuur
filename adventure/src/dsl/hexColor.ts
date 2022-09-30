type Brand<K, T> = K & { __brand: T };

/**
 * You can create a HexColor type with the `hexColor` function
 */
export type HexColor = Brand<string, "HexColor">;

export const hexColor = (color: string): HexColor => {
  return color as HexColor;
};
