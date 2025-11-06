function levenshtein(a, b) {
  const matrix = Array(a.length + 1)
    .fill(null)
    .map(() => Array(b.length + 1).fill(null));

  for (let i = 0; i <= a.length; i++) matrix[i][0] = i;
  for (let j = 0; j <= b.length; j++) matrix[0][j] = j;

  for (let i = 1; i <= a.length; i++) {
    for (let j = 1; j <= b.length; j++) {
      const cost = a[i - 1] === b[j - 1] ? 0 : 1;
      matrix[i][j] = Math.min(
        matrix[i - 1][j] + 1,
        matrix[i][j - 1] + 1,
        matrix[i - 1][j - 1] + cost
      );
    }
  }
  return matrix[a.length][b.length];
}

function handleSearch() {
  const searchValue = document.getElementById('search').value.toLowerCase().trim();
  const items = document.querySelectorAll('.grid-container .grid-item');

  if (!searchValue) {
    items.forEach(item => (item.style.display = 'block'));
    return;
  }

  items.forEach(item => {
    const text = item.textContent.toLowerCase();

    if (text.includes(searchValue)) {
      item.style.display = 'block';
      return;
    }

    let isMatch = false;

    for (let i = 0; i <= text.length - searchValue.length; i++) {
      const substring = text.slice(i, i + searchValue.length);
      const distance = levenshtein(searchValue, substring);
      if (distance <= 1) {
        isMatch = true;
        break;
      }
    }

    item.style.display = isMatch ? 'block' : 'none';
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
  const data = {
    description: document.querySelector("#item-description").value,
    value: document.querySelector("#item-value").value,
    price: document.querySelector("#item-price").value,
    category: document.querySelector("#item-category").value,
    container: document.querySelector("#item-container").value,
    location: document.querySelector("#item-location").value,
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
