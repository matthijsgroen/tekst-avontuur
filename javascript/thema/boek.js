const cls = async () => {
  const pageElement = screenElement.parentElement;
  pageElement.classList.add("turn");

  await sleep(0.6);
  screenElement.innerHTML = "";
  pageElement.classList.remove("turn");
};

const startSpel = () => {
  document.getElementsByClassName("kaft")[0].classList.add("open");
};
