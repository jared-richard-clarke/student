/**
 * --------------------------------------------------------------------------
 * Bootstrap (v5.1.0): dom/selector-engine.js
 * Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE)
 * --------------------------------------------------------------------------
 */
// === helper utils ===
const isElement = (object) => {
    if (!object || typeof object !== "object") {
        return false;
    }

    if (typeof object.jquery !== "undefined") {
        object = object[0];
    }

    return typeof object.nodeType !== "undefined";
};

const isVisible = (element) => {
    if (!isElement(element) || element.getClientRects().length === 0) {
        return false;
    }

    const elementIsVisible = getComputedStyle(element).getPropertyValue("visibility") === "visible";
    // Handle `details` element as its content may falsie appear visible when it is closed
    const closedDetails = element.closest("details:not([open])");

    if (!closedDetails) {
        return elementIsVisible;
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
};

const isDisabled = (element) => {
    if (!element || element.nodeType !== Node.ELEMENT_NODE) {
        return true;
    }

    if (element.classList.contains("disabled")) {
        return true;
    }

    if (typeof element.disabled !== "undefined") {
        return element.disabled;
    }

    return (
        element.hasAttribute("disabled") &&
        element.getAttribute("disabled") !== "false"
    );
};

// === constants ===
const NODE_TEXT = 3;

// === module ===
const SelectorEngine = {
    find(selector, element = document.documentElement) {
        return [].concat(
            ...Element.prototype.querySelectorAll.call(element, selector)
        );
    },

    findOne(selector, element = document.documentElement) {
        return Element.prototype.querySelector.call(element, selector);
    },

    children(element, selector) {
        return []
            .concat(...element.children)
            .filter((child) => child.matches(selector));
    },

    parents(element, selector) {
        const parents = [];

        let ancestor = element.parentNode;

        while (
            ancestor &&
            ancestor.nodeType === Node.ELEMENT_NODE &&
            ancestor.nodeType !== NODE_TEXT
        ) {
            if (ancestor.matches(selector)) {
                parents.push(ancestor);
            }

            ancestor = ancestor.parentNode;
        }

        return parents;
    },

    prev(element, selector) {
        let previous = element.previousElementSibling;

        while (previous) {
            if (previous.matches(selector)) {
                return [previous];
            }

            previous = previous.previousElementSibling;
        }

        return [];
    },

    next(element, selector) {
        let next = element.nextElementSibling;

        while (next) {
            if (next.matches(selector)) {
                return [next];
            }

            next = next.nextElementSibling;
        }

        return [];
    },

    focusableChildren(element) {
        const focusables = [
            "a",
            "button",
            "input",
            "textarea",
            "select",
            "details",
            "[tabindex]",
            '[contenteditable="true"]',
        ]
            .map((selector) => `${selector}:not([tabindex^="-"])`)
            .join(", ");

        return this.find(focusables, element).filter(
            (el) => !isDisabled(el) && isVisible(el)
        );
    },
};

export default SelectorEngine;
