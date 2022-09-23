import "./locaties";
import { convertGame } from "../src/dsl";

convertGame((gameModel) => {
  console.log(JSON.stringify(gameModel));
});
