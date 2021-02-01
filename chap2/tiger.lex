type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

val temp_str = ref "";

%% 

ws = [\ \t]+;
digit = [0-9];
id = [a-zA-Z][0-9a-zA-Z_]*;

%s COMMENT STR;
%%
<COMMENT>"*/" => (YYBEGIN INITIAL; continue());
<COMMENT>\n =>(lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
<COMMENT>. => (continue());
<STR>\" => (YYBEGIN INITIAL;Tokens.STRING(!temp_str, yypos, yypos + String.size(!temp_str)));
<STR>\\n => (temp_str := !temp_str ^ "\n"; continue());
<STR>\\t => (temp_str := !temp_str ^ "\t"; continue());
<STR>\\f => (temp_str := !temp_str ^ "!!!TODO!!!"; continue()); 
<STR>\\\" => (temp_str := !temp_str ^ "\""; continue());
<STR>\\\\ => (temp_str := !temp_str ^ "\\"; continue());
<STR>\\{digit}{3} => (temp_str := !temp_str ^ Char.toString(valOf(Char.fromString(yytext))); continue());
<STR>. => (temp_str := !temp_str ^ yytext; continue());
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>"/*" => (YYBEGIN COMMENT; continue());
<INITIAL>\" => (temp_str := ""; YYBEGIN STR; continue());
<INITIAL>{ws} => (continue());
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"	=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"."	=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"	=> (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"	=> (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"	=> (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"	=> (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="	=> (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"	=> (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"	=> (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="	=> (Tokens.LE(yypos,yypos+2));
<INITIAL>">"	=> (Tokens.GT(yypos,yypos+1));
<INITIAL>">="	=> (Tokens.GE(yypos,yypos+2));
<INITIAL>"&"	=> (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"	=> (Tokens.OR(yypos,yypos+1));
<INITIAL>":="	=> (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>for => (Tokens.FOR(yypos, yypos + 3));
<INITIAL>while => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL>to => (Tokens.TO(yypos, yypos + 2));
<INITIAL>break => (Tokens.BREAK(yypos, yypos + 5));
<INITIAL>let => (Tokens.LET(yypos, yypos + 3));
<INITIAL>in => (Tokens.IN(yypos, yypos + 2));
<INITIAL>end => (Tokens.END(yypos, yypos + 3));
<INITIAL>function  => (Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL>var => (Tokens.VAR(yypos,yypos+3));
<INITIAL>type => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL>array => (Tokens.ARRAY(yypos, yypos + 5));
<INITIAL>if => (Tokens.IF(yypos, yypos + 2));
<INITIAL>then => (Tokens.THEN(yypos, yypos + 4));
<INITIAL>else => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL>do => (Tokens.DO(yypos, yypos + 2));
<INITIAL>of => (Tokens.OF(yypos, yypos + 2));
<INITIAL>nil => (Tokens.NIL(yypos, yypos + 3));

<INITIAL>{digit}+ => (Tokens.INT(valOf(Int.fromString(yytext)),yypos,yypos + String.size(yytext)));
<INITIAL>{id}  => (Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

