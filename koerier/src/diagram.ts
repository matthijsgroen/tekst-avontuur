import g from "./game";

type MetaData = {
  state: "text" | "progress" | "images" | "animated";
  languages: "en" | "nl";
  location:
    | "darkWood"
    | "farmland"
    | "forest"
    | "hills"
    | "home"
    | "river"
    | "swamp"
    | "village";
};

g.definePuzzleDependencies<MetaData>({
  getOutOfBed: {
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  openWindow: {
    dependsOn: ["getOutOfBed"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  getBagFromDoor: {
    dependsOn: ["openWindow"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  getKeyFromBag: {
    dependsOn: ["getBagFromDoor"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  getFoodFromCloset: {
    dependsOn: ["getKeyFromBag"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  feedRaven: {
    dependsOn: ["getFoodFromCloset"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  getLetterFromRaven: {
    dependsOn: ["feedRaven"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  readLetter: {
    dependsOn: ["getLetterFromRaven"],
    tags: { location: "home" },
    hierarchy: ["home"],
  },
  mainAct: {
    type: "chapter",
    dependsOn: ["readLetter"],
    tags: { location: "forest", state: "text" },
  },
  getWood: {
    dependsOn: ["mainAct"],
    tags: { location: "forest", state: ["text"], languages: ["en", "nl"] },
    hierarchy: ["forest"],
  },
  payTwoCoins: {
    dependsOn: ["mainAct"],
    tags: { location: "village", state: ["progress"], languages: ["en", "nl"] },
    hierarchy: ["village"],
  },
  buyCookies: {
    dependsOn: ["payTwoCoins", "get100CoinsFromFlour"],
    gateType: "or",
    tags: { location: "village", state: "text", languages: ["en", "nl"] },
    hierarchy: ["village", "bakery"],
  },
  giveFood: {
    dependsOn: ["buyCookies", "talkToDwarf"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mine"],
  },
  talkToDwarf: {
    dependsOn: ["mainAct"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mine"],
  },
  getBrokenPickAxe: {
    dependsOn: ["talkToDwarf"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mine"],
  },
  repairPickAxe: {
    dependsOn: ["getWood", "getBrokenPickAxe"],
    tags: { state: "text", languages: ["en", "nl"] },
  },
  givePickAxe: {
    dependsOn: ["repairPickAxe"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mine"],
  },
  getGem: {
    dependsOn: ["giveFood", "givePickAxe"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mine"],
  },
  talkToBaker: {
    dependsOn: ["mainAct"],
    tags: { location: "village", state: "text", languages: ["en", "nl"] },
    hierarchy: ["village", "bakery"],
  },
  talkToWitch: {
    dependsOn: ["getEntranceToSwamp"],
    tags: { location: "swamp", state: "progress", languages: ["en", "nl"] },
    hierarchy: ["swamp", "cabin"],
  },
  createJewelryAtSmith: {
    dependsOn: ["getGem", "getGoldInForest", "talkToWitch"],
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  giveJewelryToWitch: {
    dependsOn: ["createJewelryAtSmith", "talkToWitch"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  getIngredientList: {
    dependsOn: ["giveJewelryToWitch"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  pluckMushRooms: {
    dependsOn: ["getIngredientList", "theMonster"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood"],
  },
  pluckHerbs: {
    dependsOn: ["getIngredientList"],
    tags: { location: "swamp" },
    hierarchy: ["swamp"],
  },
  getAccessToCabin: {
    dependsOn: ["pluckMushRooms", "pluckHerbs", "getDragonTooth"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  getBeaker: {
    dependsOn: ["getAccessToCabin"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  dissolvePotion: {
    dependsOn: ["getAccessToCabin"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  getAccessToBasement: {
    dependsOn: ["dissolvePotion"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getWineFromCellar: {
    dependsOn: ["getBeaker", "getAccessToBasement"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  brewWakeUpPotion: {
    dependsOn: ["getWineFromCellar"],
    tags: { location: "swamp" },
    hierarchy: ["darkWood", "tower"],
  },
  wakeUpWitch: {
    dependsOn: ["brewWakeUpPotion"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  createMedicine: {
    dependsOn: ["wakeUpWitch"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  payFarrier: {
    dependsOn: ["payTwoCoins", "getCoinsFromSmith"],
    gateType: "or",
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  getHorse: {
    dependsOn: ["talkToFarmer"],
    tags: { location: "river", state: "text", languages: ["en", "nl"] },
    hierarchy: ["river"],
  },
  getCoinsFromSmith: {
    dependsOn: ["createJewelryAtSmith"],
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  fixHorseshoe: {
    dependsOn: ["getHorse", "payFarrier"],
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  getGiantTrunks: {
    dependsOn: ["mainAct"],
    tags: { location: "river", state: ["text"], languages: ["en", "nl"] },
    hierarchy: ["river"],
  },
  repairMill: {
    dependsOn: ["getGiantTrunks"],
    tags: { location: "hills" },
    hierarchy: ["hills", "mill"],
  },
  getCart: {
    dependsOn: ["repairMill", "fixHorseshoe"],
    tags: { location: "hills" },
    hierarchy: ["hills", "mill"],
  },
  putGrainOnCart: {
    dependsOn: ["getCart", "talkToFarmer"],
    tags: { location: "hills" },
    hierarchy: ["farm"],
  },
  deliverGrainToMiller: {
    dependsOn: ["putGrainOnCart"],
    tags: { location: "hills" },
    hierarchy: ["hills", "mill"],
  },
  getFlourFromMiller: {
    dependsOn: ["deliverGrainToMiller"],
    tags: { location: "hills" },
    hierarchy: ["hills", "mill"],
  },
  deliverFlourToBaker: {
    dependsOn: ["getFlourFromMiller"],
    tags: { location: "village" },
    hierarchy: ["farm"],
  },
  get100CoinsFromFlour: {
    dependsOn: ["deliverFlourToBaker"],
    tags: { location: "village" },
    hierarchy: ["village", "bakery"],
  },
  buySword: {
    dependsOn: ["talkToBaker", "get100CoinsFromFlour"],
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  talkToSmiths: {
    dependsOn: ["mainAct"],
    tags: { location: "village" },
    hierarchy: ["village", "smithy"],
  },
  getTreasureSubject: {
    dependsOn: ["talkToSmiths"],
    tags: { location: "village", state: "text", languages: ["en", "nl"] },
    hierarchy: ["village", "smithy"],
  },
  getPlantHintOfBaker: {
    dependsOn: ["getTreasureSubject"],
    tags: { location: "village", state: "text", languages: ["en", "nl"] },
    hierarchy: ["village", "bakery"],
  },
  getRiverHintFromMiller: {
    dependsOn: ["getTreasureSubject"],
    tags: { location: "hills", state: "text", languages: ["en", "nl"] },
    hierarchy: ["hills", "mill"],
  },
  getRouteHintFromFarmer: {
    dependsOn: ["getTreasureSubject"],
    tags: { location: "farmland", state: "text", languages: ["en", "nl"] },
    hierarchy: ["farm"],
  },
  getEntranceToSwamp: {
    dependsOn: ["talkToBaker"],
    tags: { location: "village", state: "text", languages: ["en", "nl"] },
    hierarchy: ["village", "bakery"],
  },
  completeTreasureRouteInfo: {
    dependsOn: ["getRiverHintFromMiller", "getRouteHintFromFarmer"],
    tags: { state: "text", languages: ["en", "nl"] },
  },
  findMoonStoneInSwamp: {
    dependsOn: ["getPlantHintOfBaker", "getEntranceToSwamp"],
    tags: { location: "swamp", state: "text", languages: ["en", "nl"] },
    hierarchy: ["swamp"],
  },
  followMoonStoneTrail: {
    dependsOn: ["findMoonStoneInSwamp", "completeTreasureRouteInfo"],
    tags: { location: "river", state: "text", languages: ["en"] },
    hierarchy: ["river"],
  },
  getRuneStone: {
    dependsOn: ["followMoonStoneTrail"],
    tags: { location: "forest" },
    hierarchy: ["forest"],
  },
  talkToFarmer: {
    dependsOn: ["mainAct"],
    tags: { location: "farmland" },
    hierarchy: ["farm"],
  },
  getRope: {
    dependsOn: ["mainAct"],
    tags: { location: "farmland" },
    hierarchy: ["farm"],
  },
  getInfoAboutMonster: {
    dependsOn: ["talkToBaker", "talkToFarmer"],
    tags: { state: "text" },
  },
  theMonster: {
    dependsOn: ["buySword", "getInfoAboutMonster"],
    tags: { location: "darkWood" },
    type: "chapter",
  },
  climbTower: {
    dependsOn: ["theMonster"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  talkToDaughter: {
    dependsOn: ["climbTower"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  putMillstoneOnCart: {
    dependsOn: ["talkToDaughter"],
    tags: { location: "hills" },
    hierarchy: ["hills", "mill"],
  },
  putMillstoneOnElevator: {
    dependsOn: ["putMillstoneOnCart", "talkToDaughter"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  raiseElevatorWithMillstone: {
    dependsOn: ["putMillstoneOnElevator"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  bindRopeToMillstone: {
    dependsOn: ["raiseElevatorWithMillstone", "getRope"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  bindRopeToTeeth: {
    dependsOn: ["getRope", "talkToDaughter"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  cutRopeOfElevator: {
    dependsOn: ["bindRopeToMillstone", "bindRopeToTeeth"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  pullToothOfDragon: {
    dependsOn: ["cutRopeOfElevator"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getDragonTooth: {
    dependsOn: ["getIngredientList", "pullToothOfDragon"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  bringDaughterToBaker: {
    dependsOn: ["pullToothOfDragon"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getApplePieFromBaker: {
    dependsOn: ["bringDaughterToBaker"],
    tags: { location: "village" },
    hierarchy: ["village", "bakery"],
  },
  giveApplePieToDragon: {
    dependsOn: ["getApplePieFromBaker"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getMoss: {
    dependsOn: ["getAccessToCabin"],
    tags: { location: "forest" },
    hierarchy: ["forest"],
  },
  createPotionToTalkToDragon: {
    dependsOn: ["getRuneStone", "getMoss", "getAccessToCabin"],
    tags: { location: "swamp" },
    hierarchy: ["swamp", "cabin"],
  },
  talkToDragon: {
    dependsOn: ["createPotionToTalkToDragon"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getPaintingFromBasement: {
    dependsOn: ["talkToDragon"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  findSaddleInBasement: {
    dependsOn: ["getAccessToBasement"],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  putSaddleOnDragon: {
    dependsOn: [
      "getPaintingFromBasement",
      "findSaddleInBasement",
      "giveApplePieToDragon",
    ],
    tags: { location: "darkWood" },
    hierarchy: ["darkWood", "tower"],
  },
  getGoldInForest: {
    dependsOn: ["followMoonStoneTrail"],
    tags: { location: "forest" },
    hierarchy: ["forest"],
  },
  getMedicineToKing: {
    dependsOn: ["putSaddleOnDragon", "createMedicine"],
    type: "chapter",
  },
});
