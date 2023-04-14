# Asynchronous JavaScript

### Sources 

- [The Odin Project](https://www.theodinproject.com/lessons/node-path-javascript-async-and-await)
- [MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)

## Promises

```javascript
const img = document.querySelector('img');
fetch('https://api.giphy.com/v1/gifs/translate?api_key=YOUR_KEY_HERE&s=cats', {mode: 'cors'})
    .then(function(response) {
        return response.json();
    })
    .then(function(response) {
        img.src = response.data.images.original.url;
    });
```

## `async` and `await`

```javascript
const img = document.querySelector('img');
(async function() {
    const response = await fetch('https://api.giphy.com/v1/gifs/translate?api_key=YOUR_KEY_HERE&s=cats', {mode: 'cors'});
    const catData = await response.json();
    img.src = catData.data.images.original.url;
})();
```

## Avoiding "Callback Hell"

### Callback Hell

```javascript
const fetchPromise = fetch('https://mdn.github.io/learning-area/javascript/apis/fetching-data/can-store/products.json');

fetchPromise.then((response) => {
    const jsonPromise = response.json();
    jsonPromise.then((data) => {
        console.log(data[0].name);
    });
});
```

### Promise Chaining

```javascript
const fetchPromise = fetch('https://mdn.github.io/learning-area/javascript/apis/fetching-data/can-store/products.json');

fetchPromise
    .then((response) => response.json())
    .then((data) => {
        console.log(data[0].name);
    });

```

## `async`, `await`, and `Promise.all`

```javascript
async function get_price() {
  const [choice, prices] = await Promise.all([
    prompt_choice(),
    fetch_prices(),
  ]);
  return prices[choice];
}
```
