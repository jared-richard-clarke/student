# Jäsentää

[Jäsentää](https://www.destructuring-bind.org/jasentaa/) is a parser-combinator library written in Clojure. 
Jäsentää was created by Richard Hull. Below is his implementation of a monad for that library.

## jasentaa.monad.cljc

```clojure
(ns jasentaa.monad)

(defn failure [& args]
  '())

(defn bind [v f]
  (f v))

(defn return [v]
  (fn [input]
    (list [v input])))

(defn >>= [m f]
  (fn [input]
    (->>
     m
     (bind input)
     (mapcat (fn [[v tail]] (bind tail (f v)))))))

(defn- merge-bind [body bind]
  (if (and (not= #?(:clj clojure.lang.Symbol
                    :cljs cljs.core/Symbol)
                 (type bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

#?(:clj
   (defmacro do* [& forms]
     (reduce merge-bind (last forms) (reverse (butlast forms)))))
```
