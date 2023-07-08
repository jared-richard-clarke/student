# A Joy Interpreter

Original code by Manfred von Thun, **A Joy Interpreter Written in Joy**

A Joy interpreter in Joy is a program which expects a quoted program on top of the stack and executes it.

## Interpreter 1

Joy already has a combinator, the `i` combinator, which removes a program from the top of the stack and executes it.

```joy
joy == i
```

## Interpreter 2

Joy programs are sequences which are executed by stepping through the members of the sequence.

```joy
joy  == 
        [ unitlist 
          i ] 
        step 
```

## Interpreter 3

To be more specific, replace `unitlist` with `opcase`, which examines what is on top of the
stack and executes the appropriate action.

```joy
joy  ==
    [ [ [ joy           body joy ]                       (* user defined *)
        [ 0    ]                                         (* literals *)
        [ []   ]
        [ true ]
        [ 'A   ]
        [ ""   ]
        [ {}   ]
                                                         (* operators *)
        [ +             pop     +    ]
        [ rest          pop     rest ]
        [ dup           pop     dup  ]
        [ swap          pop     swap ]
        [ pop           pop     pop  ]
        [ -             pop     -    ]
        [ and           pop     and  ]
        [ cons          pop     cons ]
                                                         (* unary combinators *)
        [ i             pop [joy] cons    i      ]
        [ dip           pop [joy] cons    dip    ]
        [ map           pop [joy] cons    map    ]
        [ filter        pop [joy] cons    filter ]
                                                         (* binary combinators *)
        [ branch        pop [[joy] cons] app2   branch ]
        [ cleave        pop [[joy] cons] app2   cleave ]
                                                         (* ternary combinators *) 
        [ ifte          pop [[joy] cons] app3   ifte ] 
                                                         (* quaternary combinators *) 
        [ linrec        pop [[joy] cons] app4   linrec ] 
        [ binrec        pop [[joy] cons] app4   binrec ] 
        [               [] cons i ] ]                    (* default *) 
      opcase
      i ]
    step
```

## Interpreter 4

Repeated sequences of code are abstracted for better readability. The default
case now traces each symbol it hands over to the Joy system.

```joy
HIDE 
  cr1  ==  pop [[joy] cons] app1;
  cr2  ==  pop [[joy] cons] app2;
  cr3  ==  pop [[joy] cons] app3;
  cr4  ==  pop [[joy] cons] app4
IN 
  joy  ==
    [ [ [ joy           body joy ]           (* user defined *)
        [ 0    ]                             (* literals *)
        [ []   ]
        [ true ]
        [ 'A   ]
        [ ""   ]
        [ {}   ]
                                             (* operators *)
        [ +             pop     +    ]
        [ rest          pop     rest ]
        [ dup           pop     dup  ]
        [ swap          pop     swap ]
        [ pop           pop     pop  ]
        [ -             pop     -    ]
        [ and           pop     and  ]
        [ cons          pop     cons ]
                                             (* unary combinators *)
        [ i             cr1     i      ]
        [ dip           cr1     dip    ]
        [ map           cr1     map    ]
        [ filter        cr1     filter ]
                                             (* binary combinators *)
        [ branch        cr2     branch ]
        [ cleave        cr2     cleave ]
                                             (* ternary combinators *) 
        [ ifte          cr3     ifte ] 
                                             (* quaternary combinators *) 
        [ linrec        cr4     linrec ] 
        [ binrec        cr4     binrec ] 
        [               dup put [] cons i ]] (* default *) 
      opcase
      i ]
    step
```

## Aside

An interpreter that interprets only itself and defers all else to the default clause.

```joy
joy0  == 
    [ [ [ joy0          body            joy0     ] 
        [ []                                     ] 
        [ pop           pop             pop      ] 
        [ cons          pop             cons     ] 
        [ opcase        pop             opcase   ] 
        [ body          pop             body     ] 
        [ i             pop             joy0     ] 
        [ step          pop [joy0] cons step     ] 
        [               [] cons         i        ] ] 
      opcase 
      i ] 
    step
```
