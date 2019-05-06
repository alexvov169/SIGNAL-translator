# SIGNAL-translator
Simple translator of part of SIGNAL programming language

Grammar:
```HTML
<signal-program> --> <program>
<program> --> PROGRAM <procedure-identifier> ; <block>.
<block> --> <declarations> BEGIN <statements-list> END
<statements-list> --> <empty>
<declarations> --> <constant-declarations>
<constant-declarations> --> CONST <constant-declarations-list> | <empty>
<constant-declarations-list> -->
            <constant-declaration> <constant-declarations-list> 
            | <empty>
<constant-declaration> --> <constant-identifier> = <constant>;
<constant> --> '<complex-number>'
<complex-number> --> <left-part> <right-part> 
<left-part> --> <unsigned-integer> | <empty>
<right-part> --> ,<unsigned-integer> | $EXP( <unsigned-integer> ) | <empty>
<constant-identifier> --> <identifier>
<procedure-identifier> --> <identifier>
<identifier> --> <letter><string>
<string> --> <letter><string> | <digit><string> | <empty>
<unsigned-integer> --> <digit><digits-string>
<digits-string> --> <digit><digits-string> | <empty>
<digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<letter> --> A | B | C | D | ... | Z
```

Examples:

```PASCAL
PROGRAM SIG3;
CONST A = '3$EXP(5)'; (*this is
       	  	       a comment*)
B = '53';
C = '1$EXP(10)';
D = '';
BEGIN
END.

(* add border to ins and outs *)
(* separate err state and unexpected *)
(*** ***
*)

(**) (* *)
  (* багаторядковий
коментар *)
  (*****) (*(()*())*) (*;.:*)
```

![Alt text](https://github.com/alexvov169/SIGNAL-translator/blob/parser-feature/sig3.svg)
