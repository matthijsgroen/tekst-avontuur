import { GameDefinition } from "point-n-click";

export type GameState = GameDefinition<
  1,
  {
    version: 1;
    locations: {
      forest: { flags: "visited" };
      farmland: { flags: "visited" };
      farm: { flags: "visited" };
      hills: { flags: "visited" };
      mine: { flags: "visited" };
      mill: { flags: "visited"; states: "fixed" };
      swamp: { flags: "allowEntrance" };
      cabin: { flags: "visited" };
      village: { flags: "visited" };
      bakery: { flags: "visited" };
      smithy: { flags: "visited" };
      darkwoods: { flags: "visited" };
      river: { flags: "visited" };
    };
    items: {
      bag: { states: "known" | "possession" };
      branch: { states: "possession" | "used" };
      pickaxe: { states: "broken" | "fixed" | "given" };
      rope: { states: "possession" };
      millstone: { states: "seen" };
      fabric: { states: "possession" | "used" };
      medicine: { flags: "recipe" };
      cookies: { states: "price" | "buying" | "possession" | "given" };
      gemstone: { states: "chopped" | "possession" };
      sword: { states: "need" | "possession" };
      treasureNotes: {
        states: "existence" | "possession";
        flags: "moonStone" | "route" | "startPoint";
      };
      moonStone: { states: "possession" };
    };
    // lists: { // useful for inventory management
    //   inventory:
    //     | "branch"
    //     | "coins"
    //     | "pickaxe"
    //     | "brokenPickaxe"
    //     | "rope"
    //     | "fabric"
    //     | "cookies"
    //     | "gemstone"
    //     | "sword";
    // };
    characters: {
      player: { counters: "coins"; flags: "male" };
      dwarf: { flags: "nameKnown"; states: "happy" };
      miller: {};
      horse: {
        states: "river" | "following" | "stable";
        flags: "hooves" | "cart" | "found" | "known";
      };
      dragon: { states: "known" | "found" };
      farmer: { flags: "visited" | "toldDragon" | "returnedHorse" };
      daughter: {};
      witch: { states: "intro" | "visited" };
      baker: { states: "intro" | "visited"; flags: "toldDragon" };
      farrier: {};
      goldsmith: {};
      armorer: {};
    };
    overlays:
      | "dwarfConversation"
      | "millerConversation"
      | "farmerConversation"
      | "bakerConversation"
      | "smithsConversation"
      | "witchConversation"
      | "inventory"
      | "treasureNotes"
      | "plants";
  }
>;
