// === Design pattern by Douglas Crockford ===

function secure_object() {
    const weakmap = new WeakMap();
    return {
        lock: function (object) {
            const key = Object.freeze(Object.create(null));
            weakmap.set(key, object);
            return key;
        },
        unlock: function (key) {
            return weakmap.get(key);
        },
    };
}

// === example ===
const treasure_chest = secure_object();
const secret_stash = {
    rubies: 7,
    emeralds: 11,
    waffles: "never enough",
};
const secret_key = treasure_chest.lock(secret_stash);
const pilfered_stash = treasure_chest.unlock(secret_key);
