import { mkdir, writeFile } from "fs/promises";
import { join } from "path";
import { exitGame } from "../cli-client/utils";
import { GameModel, ScriptAST } from "../dsl/ast-types";
import { GameWorld } from "../dsl/world-types";

export type Locale = `${string}-${string}`;

export type TranslationFile = {
  [key: string]: string | TranslationFile;
};

const processScript = <Game extends GameWorld>(
  script: ScriptAST<Game>,
  enterScriptScope: string[],
  setTranslationKey: (key: string[], value: string) => void
) => {
  for (const statement of script) {
    if (statement.statementType === "Text") {
      for (const sentence of statement.sentences) {
        setTranslationKey(enterScriptScope.concat("text", sentence), sentence);
      }
    }
    if (statement.statementType === "CharacterSay") {
      for (const sentence of statement.sentences) {
        setTranslationKey(
          enterScriptScope.concat(String(statement.character), sentence),
          sentence
        );
      }
    }
    if (statement.statementType === "Condition") {
      processScript(statement.body, enterScriptScope, setTranslationKey);
      processScript(statement.elseBody, enterScriptScope, setTranslationKey);
    }
    if (statement.statementType === "OpenOverlay") {
      const overlayScope = ["overlays", statement.overlayId];
      processScript(statement.onStart.script, overlayScope, setTranslationKey);
      processScript(statement.onEnd.script, overlayScope, setTranslationKey);
      for (const interaction of statement.interactions) {
        setTranslationKey(
          ["overlays", statement.overlayId, "interactions", interaction.label],
          interaction.label
        );
        processScript(interaction.script, overlayScope, setTranslationKey);
      }
    }
  }
};

export const exportTranslations =
  (folder: string, locales: Locale[]) =>
  async <Game extends GameWorld>(gameModel?: GameModel<Game>) => {
    if (!gameModel) {
      console.log("No valid game file");
      exitGame(1);
      return;
    }

    const translationObject: TranslationFile = {};
    const setTranslationKey = (key: string[], value: string) => {
      let obj = translationObject;
      const path = key.slice(0, -1);
      const tail = key.slice(-1)[0];
      for (const prop of path) {
        obj[prop] = obj[prop] || {};
        obj = obj[prop] as TranslationFile;
      }
      obj[tail] = value;
    };

    for (const [character, settings] of Object.entries(
      gameModel.settings.characterConfigs
    )) {
      setTranslationKey(
        ["characters", character, "defaultName"],
        settings.defaultName
      );
    }

    for (const location of gameModel.locations) {
      const locationScope = ["location", String(location.id)];
      for (const enterScript of location.onEnter) {
        processScript(enterScript.script, locationScope, setTranslationKey);
      }
      for (const leaveScript of location.onLeave) {
        processScript(leaveScript.script, locationScope, setTranslationKey);
      }
      processScript(location.describe.script, locationScope, setTranslationKey);

      for (const interaction of location.interactions) {
        const interactionScope = locationScope.concat(
          "interactions",
          interaction.label
        );
        setTranslationKey(interactionScope, interaction.label);
        processScript(interaction.script, locationScope, setTranslationKey);
      }
    }

    try {
      await mkdir(folder);
    } catch (e) {
      if (
        "code" in (e as Error) &&
        (e as Error & { code: string }).code === "EEXIST"
      ) {
        // no problem
      } else {
        throw e;
      }
    }
    for (const locale of locales) {
      await writeFile(
        join(folder, `${locale}.json`),
        JSON.stringify(translationObject, undefined, 2)
      );
    }
  };
