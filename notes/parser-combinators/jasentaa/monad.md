# Monad

```clojure
(ns jasentaa.monad)

(defn failure [& args]
  '())

(defn bind [v f] ;; <- Better if it were named "parse" or "run". -- JC
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
