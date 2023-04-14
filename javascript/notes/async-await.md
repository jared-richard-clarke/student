# Asynchronous JavaScript

### Sources 

- [The Odin Project](https://www.theodinproject.com/lessons/node-path-javascript-async-and-await)
- [MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)

## Sequencing with Callbacks

```javascript
// sequence
function one(value) {
    return value + 1;
}

function two(value) {
    return value + 1;
}

function three(value) {
    return value + 1;
}

function sequence() {
    let result = 0;
    result = one(result);
    result = two(result);
    result = three(result);
    return result;
}

// callback
function call_one(value, callback) {
    const result = value + 1;
    callback(result);
}

function call_two(value, callback) {
    const result = value + 1;
    callback(result);
}

function call_three(value, callback) {
    const result = value + 1;
    callback(result);
}

// callback hell
function call_sequence() {
    call_one(0, (one) => {
        call_two(one, (two) => {
            call_three(two, (three) => {
                return three;
            });
        });
    });
}
```

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

## `async`, `await`, and `Promise.prototype.all`

```javascript
async function get_price() {
  const [choice, prices] = await Promise.all([
    prompt_choice(),
    fetch_prices(),
  ]);
  return prices[choice];
}
```
