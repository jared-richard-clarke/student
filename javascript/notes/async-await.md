# Asynchronous JavaScript

Source: [The Odin Project](https://www.theodinproject.com/lessons/node-path-javascript-async-and-await)

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
