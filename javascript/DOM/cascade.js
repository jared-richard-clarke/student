// === with "this" ===
// const select = function(element) {
//     const node = document.querySelector(element);
//     return {
//       add_text: function(text) {
//         node.textContent = text;
//         return this;
//       },
//       style: function(property, value) {
//         node.style[property] = value;
//         return this;
//       }
//     }
//   };

// === without "this" ===
const select = function(element) {
    const node = document.querySelector(element);
    const methods = Object.create(null);
    methods.add_text = function(text) {
        node.textContent = text;
        return methods;
    };
    methods.style = function(property, value) {
        node.style[property] = value;
        return methods;
    };
    return Object.freeze(methods);
};

  select(".paragraph")
    .add_text("Behold, the cascade!")
    .style("color", "white")
    .style("backgroundColor", "black")
    .style("padding", "1rem");
