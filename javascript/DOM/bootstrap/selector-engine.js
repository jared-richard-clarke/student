/**
 * --------------------------------------------------------------------------
 * Bootstrap dom/selector-engine.js
 * Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE)
 * --------------------------------------------------------------------------
 */

// === helper functions ===

function isElement(object) {
  if (!object || typeof object !== "object") {
    return false;
  }
  if (typeof object.jquery !== "undefined") {
    object = object[0]
  }
  return typeof object.nodeType !== "undefined";
}

function isVisible(element) {
  if (!isElement(element) || element.getClientRects().length === 0) {
    return false
  }
  const elementIsVisible = getComputedStyle(element).getPropertyValue("visibility") === "visible";
  const closedDetails = element.closest("details:not([open])");
  if (!closedDetails) {
    return elementIsVisible
  }
  if (closedDetails !== element) {
    const summary = element.closest("summary");
    if (summary && summary.parentNode !== closedDetails) {
      return false;
    }
    if (summary === null) {
      return false;
    }
  }
  return elementIsVisible;
}

function isDisabled(element) {
  if (!element || element.nodeType !== Node.ELEMENT_NODE) {
    return true;
  }
  if (element.classList.contains("disabled")) {
    return true;
  }
  if (typeof element.disabled !== "undefined") {
    return element.disabled;
  }
  return element.hasAttribute("disabled") && element.getAttribute("disabled") !== "false";
}

function parseSelector(selector( {
  if (selector && window.CSS && window.CSS.escape) {
    selector = selector.replace(/#([^\s"#']+)/g, (match, id) => `#${CSS.escape(id)}`);
  }
  return selector;
}

function getSelector(element) {
  let selector = element.getAttribute("data-bs-target");
  if (!selector || selector === "#") {
    let hrefAttribute = element.getAttribute("href");
    if (!hrefAttribute || (!hrefAttribute.includes("#") && !hrefAttribute.startsWith("."))) {
      return null;
    }
    if (hrefAttribute.includes("#") && !hrefAttribute.startsWith("#")) {
      hrefAttribute = `#${hrefAttribute.split("#")[1]}`;
    }
    selector = hrefAttribute && hrefAttribute !== "#" ? hrefAttribute.trim() : null;
  }
  return selector ? selector.split(",").map(sel => parseSelector(sel)).join(",") : null;
}

// === selector engine ===

const SelectorEngine = {
  find: function(selector, element = document.documentElement) {
    return [].concat(...Element.prototype.querySelectorAll.call(element, selector));
  },

  findOne: function(selector, element = document.documentElement) {
    return Element.prototype.querySelector.call(element, selector);
  },

  children: function(element, selector) {
    return [].concat(...element.children).filter(child => child.matches(selector));
  },

  parents: function(element, selector) {
    const parents = [];
    let ancestor = element.parentNode.closest(selector);
    while (ancestor) {
      parents.push(ancestor);
      ancestor = ancestor.parentNode.closest(selector);
    }
    return parents;
  },

  prev: function(element, selector) {
    let previous = element.previousElementSibling;
    while (previous) {
      if (previous.matches(selector)) {
        return [previous];
      }
      previous = previous.previousElementSibling;
    }
    return []
  },
    
  next: function(element, selector) {
    let next = element.nextElementSibling;
    while (next) {
      if (next.matches(selector)) {
        return [next];
      }
      next = next.nextElementSibling;
    }
    return [];
  },

  focusableChildren: function(element) {
    const focusables = [
      "a",
      "button",
      "input",
      "textarea",
      "select",
      "details",
      "[tabindex]",
      "[contenteditable='true']"
    ].map(selector => `${selector}:not([tabindex^="-"])`).join(",");

    return SelectorEngine.find(focusables, element).filter(el => !isDisabled(el) && isVisible(el));
  },

  getSelectorFromElement: function(element) {
    const selector = getSelector(element);
    if (selector) {
      return SelectorEngine.findOne(selector) ? selector : null;
    }
    return null;
  },

  getElementFromSelector: function(element) {
    const selector = getSelector(element);
    return selector ? SelectorEngine.findOne(selector) : null;
  },

  getMultipleElementsFromSelector: function(element) {
    const selector = getSelector(element);
    return selector ? SelectorEngine.find(selector) : [];
  }
}

export default SelectorEngine;
