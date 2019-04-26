const leesAvontuur = async bestandsNaam => {
  const inhoud = await readFile(bestandsNaam, "utf8");
  const regels = inhoud.replace(/\r/g, "").split("\n");
  let actieModus = false;
  const actieData = [];
  const schermData = [];

  regels
    .filter(regel => regel.startsWith('"'))
    .forEach(regel => {
      const elementen = JSON.parse(`[${regel}]`);

      if (elementen.length === 1 && elementen[0] === "END") {
        actieModus = true;
      } else {
        actieModus
          ? actieData.push(...elementen)
          : schermData.push(...elementen);
      }
    });

  return { actieData, schermData };
};

module.export = leesAvontuur;
