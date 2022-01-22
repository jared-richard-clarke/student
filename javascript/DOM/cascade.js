const select = function(element) {
    const node = document.querySelector(element);
    return {
      add_text: function(text) {
        node.textContent = text;
        return this;
      },
      style: function(property, value) {
        node.style[property] = value;
        return this;
      }
    }
  };

  select(".paragraph")
    .add_text("Behold, the cascade!")
    .style("color", "white")
    .style("backgroundColor", "black")
    .style("padding", "1rem");
