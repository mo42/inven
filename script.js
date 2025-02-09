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

function deleteItem(itemId) {
  if (confirm('Are you sure you want to delete the item with ID ' + itemId + '?')) {
    fetch('/inventory/' + itemId, {
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(response => {
      if (response.ok) {
        document.getElementById('delete-' + itemId).closest('.grid-item')
          .remove();
      } else {
        alert('Failed to delete item');
      }
    });
  }
}

function addItem() {
  const description = document.querySelector("#item-description").value;

  const data = {
    description: description
  };

  fetch('/add', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
  })
  .then(response => response.json())
  .catch((error) => {
    console.error('Error:', error);
  });
}
