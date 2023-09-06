# Smalltalk Stack

```smalltalk
Object subclass: #Stack
   instanceVariableNames: 'array top'
   classVariableNames: ''
   poolDictionaries: ''

push: item
      | save |
   top := top + 1.
   top > array size ifTrue: 
      "Double array size to prevent overflow."
      [save := array.
       array := Array new: 2 * save size.
       1 to: save size do:
          [:k | array at: k put: (save at: k)]].
   array at: top put: item


pop | item | 
   self isEmpty ifTrue: [self error: 'trying to pop an empty stack'].
   item := array at: top.
   top := top - 1.
   ^ item

isEmpty
   ^ top = 0
```
