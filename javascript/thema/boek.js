const cls = async () => {
  const pageElement = screenElement.parentElement;
  pageElement.classList.add("turn");

  await sleep(0.6);
  screenElement.innerHTML = "";
  pageElement.classList.remove("turn");
};

const startSpel = () => {
  document.getElementsByClassName("kaft")[0].classList.add("open", "ingevuld");
};

const eindSpel = () => {
  const paragraaf = document.createElement("p");
  paragraaf.classList.add("einde");
  const linkTag = document.createElement("a");
  const textNode = document.createTextNode("Sluit boek");
  linkTag.appendChild(textNode);
  linkTag.setAttribute("href", "#");
  linkTag.addEventListener("click", e => {
    e.preventDefault();
    linkTag.classList.add("verberg");
    document.getElementsByClassName("kaft")[0].classList.remove("open");
    document.getElementsByClassName("bedankt")[0].classList.add("zichtbaar");
    return false;
  });
  paragraaf.appendChild(linkTag);
  screenElement.appendChild(paragraaf);
};
