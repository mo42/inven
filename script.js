function handleSearch() {
  const searchValue = document.getElementById("search").value.toLowerCase();
  const items = document.querySelectorAll(".grid-container .grid-item");
  items.forEach(item => {
    const itemName = item.textContent.toLowerCase();
    if (itemName.includes(searchValue)) {
      item.style.display = "block";
    } else {
      item.style.display = "none";
    }
  });
}
