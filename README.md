# SIGNAL-translator
Simple translator of part of SIGNAL programming language

Grammar:
```ANTLR
<signal-program> --> <program>
<program> --> PROGRAM <procedure-identifier> ; <block>
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
