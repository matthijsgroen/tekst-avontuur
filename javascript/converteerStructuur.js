const converteerStructuur = ({ actieData, schermData }) => {
  const nieuweSchermData = [];
  let actiefScherm = { schermData: [] };
  schermData.forEach(element => {
    if (!actiefScherm.hasOwnProperty("test")) {
      const origineleTest = element;
      actiefScherm.test = element.split(";");

      const correct = actiefScherm.test.every(
        conditie => conditie && conditie.match(/\d+[!><=]\d+/)
      );
      if (!correct) {
        console.log("fout in conditie: ", origineleTest);
      }
    } else {
      if (element.startsWith("&")) {
        if (element.length > 1) {
          actiefScherm.actie = element.slice(1).split(";");
          const correct = actiefScherm.actie.every(
            actie => actie && actie.match(/\d+[=+-r]\d+/)
          );
          if (!correct) {
            console.log("fout in scherm actie: ", element);
          }
        }
        nieuweSchermData.push(actiefScherm);
        actiefScherm = { schermData: [] };
      } else {
        actiefScherm.schermData.push(element);
      }
    }
  });
  const nieuweActieData = [];
  let actieveActie = {};

  actieData.forEach((element, i) => {
    if (i % 3 === 0) {
      actieveActie.test = element.split(";");
    } else if (i % 3 === 1) {
      actieveActie.tekst = element;
    } else {
      actieveActie.actie = element.split(";");
      nieuweActieData.push(actieveActie);
      actieveActie = {};
    }
  });
  return { actieData: nieuweActieData, schermData: nieuweSchermData };
};

module.exports = converteerStructuur;
