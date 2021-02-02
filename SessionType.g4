grammar SessionType;


/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

start returns [type]
  :
    s=stype {$type = $s.type if $s.type is not None else ""} EOF;

stype returns [type]
  :
        //op CLPAR ID SEMIC stype (COMMA ID SEMIC stype)* CRPAR
        PLUS CLPAR {$type = "["} i=ID s=SEMIC st=stype {$type += "!" + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA i2=ID s2=SEMIC st2=stype {$type += $c.text + "!" + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* {$type += "]"} CRPAR
      | AND CLPAR {$type = "["} i=ID s=SEMIC st=stype {$type += "?" + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA i2=ID s2=SEMIC st2=stype {$type += $c.text + "?" + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* {$type += "]"} CRPAR
      | r=REC i=ID d=DOT g=guarded {$type = $r.text + " V" + $i.text + " " + $d.text + ($g.type if not $g.type == None else "")}
      | i=ID {$type = "V" + $i.text}
      | e=END {$type = $e.text}
      | o=OUT i=ID s=SEMIC st=stype {$type = $o.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }
      | sl=SLPAR o=OUT i=ID s=SEMIC st=stype {$type = $sl.text + $o.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }(c=COMMA o2=OUT i2=ID s2=SEMIC st2=stype {$type += $c.text + $o2.text + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* sr=SRPAR {$type += $sr.text}
      | n=IN i=ID s=SEMIC st=stype {$type = $n.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }
      | sl=SLPAR n=IN i=ID s=SEMIC st=stype {$type = $sl.text + $n.text + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA n2=IN i2=ID s2=SEMIC st2=stype {$type += $c.text + $n2.text + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* sr=SRPAR {$type += $sr.text}
      ;

guarded returns [type]
  :
        //op CLPAR ID SEMIC stype (COMMA ID SEMIC stype)* CRPAR
        PLUS CLPAR {$type = "["} i=ID s=SEMIC st=stype {$type += "!" + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA i2=ID s2=SEMIC st2=stype {$type += $c.text + "!" + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* {$type += "]"} CRPAR
      | AND CLPAR {$type = "["} i=ID s=SEMIC st=stype {$type += "?" + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA i2=ID s2=SEMIC st2=stype {$type += $c.text + "?" + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* {$type += "]"} CRPAR
      | r=REC i=ID d=DOT g=guarded {$type = $r.text + " V" + $i.text + " " + $d.text + ($g.type if not $g.type == None else "")}
      | e=END {$type = $e.text}
      | o=OUT i=ID s=SEMIC st=stype {$type = $o.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }
      | sl=SLPAR o=OUT i=ID s=SEMIC st=stype {$type = $sl.text + $o.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }(c=COMMA o2=OUT i2=ID s2=SEMIC st2=stype {$type += $c.text + $o2.text + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* sr=SRPAR {$type += $sr.text}
      | n=IN i=ID s=SEMIC st=stype {$type = $n.text + $i.text + $s.text + ($st.type if not $st.type == None else "") }
      | sl=SLPAR n=IN i=ID s=SEMIC st=stype {$type = $sl.text + $n.text + $i.text + $s.text + ($st.type if not $st.type == None else "") } (c=COMMA n2=IN i2=ID s2=SEMIC st2=stype {$type += $c.text + $n2.text + $i2.text + $s2.text + ($st2.type if not $st2.type == None else "")})* sr=SRPAR {$type += $sr.text}
      ;

/*ident returns [type]:
    iu=IDU {$type = $iu.text}
   |il=IDL {$type = $il.text}
   ;*/

/*op returnstype [type]
  :
       PLUS
     | AND
     ;*/



/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/

PLUS  	: '+' ;
CLPAR	: '{' ;
CRPAR	: '}' ;
SLPAR   : '[' ;
SRPAR   : ']' ;
SEMIC 	: ';' ;
COMMA	: ',' ;
DOT	    : '.' ;
AND	    : '&';
REC     : 'rec';
END     : 'end';
OUT     : '!';
IN      : '?';
ID      : ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')* ;
//IDU  	: ('A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')* ;
//IDL  	: ('a'..'z')('a'..'z' | 'A'..'Z' | '0'..'9')* ;


WHITESP  : ( '\t' | ' ' | '\r' | '\n' )+    -> channel(HIDDEN) ;

//ERR   	 : . {  print("N Error: ")} -> channel(HIDDEN);