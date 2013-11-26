%{
(* This is the header *)
(* let _ = Parsing.set_trace true in () *)
 let counter = ref 0
 let line_nums = Hashtbl.create (1000) 
 let parse_error s = 
   let mypos = Parsing.symbol_start_pos() in
   print_string (s ^ " (line number: ");
   print_int mypos.Lexing.pos_lnum;
   print_string (", column number: " ^ (string_of_int (mypos.Lexing.pos_cnum - mypos.Lexing.pos_bol)));
   print_endline ")";
   flush stdout
 let ln () = 
   let mypos = Parsing.symbol_start_pos() in 
   let cnum = (mypos.Lexing.pos_cnum) - (mypos.Lexing.pos_bol) in
   (mypos.Lexing.pos_lnum,cnum)
%}

/* These are the declarations */

/* The tokens */
/* Constant constructors */
%token TPlus TMinus TTimes TDiv TPow TOP TSEMICOLON TCP TEqual TOB TCB TComma TLess TLessEqual TGreater TGreaterEqual TEqualEqual TMod TASYNC
%token And Or Where TXCL TQ TSuspend TAbort TWhile TTrue TFalse TWhile TTrap TXor
%token TLbrack TRbrack TColon TPresent TEof TLShift TRShift TElse TExit TEmit TCase TWeak
%token TMain TIn TOut TOtherwise TPar TFor TSignal TChannel TPause TColon
%token TInt8 TInt16 TInt32 TInt64 TInt8s TInt16s TInt32s TInt64s TFloat8 TFloat32 TFloat64 TFloat16 TInt1s
%token TAwait Timm TExtern TSplit TAT TSend TReceive TNotEqual TOpPlus TOpTimes TBegin TEnd THash TDo TSEMISEMI

/* Constructors with an argument */
%token <string> TInt
%token <string> TFloat
%token <string> TEscapedCode
%token <string> TSymbol

/* operator associative rules */
%left TRShift TLShift
%left TPlus TMinus
%left TTimes TDiv TMod
%left TPow
%left TOP TCP
%left And Or
%left TXCL
%nonassoc TUminus /* useful for difference between -2 and 1 - 2*/

/* The start of the parsing function */
%start ast
%type <St.ast> ast

%%
/* These are the parsing rules */

ast:
    | topstmtlist TEof {Systemj.Apar($1,ln())}
;

topstmtlist:
    | topstmtlist TASYNC stmt {$3::$1}
    | stmt {[$1]}
;

stmtlist:
    | stmtlist stmt {$2::$1}
    | stmt {[$1]}
;

stmt:
    | signal TSEMICOLON {$1}
    | typed_signal TSEMICOLON {$1}
    | channel TSEMICOLON {$1}
    | typed_channel TSEMICOLON {$1}
    | TOB stmtlist TCB {Systemj.Block ($2, ln())}
    | par {$1}
    | present {$1}
    | abort {$1}
    | await TSEMICOLON {$1}
    | suspend {$1}
    /*| TExit TOP symbol TCP TSEMICOLON {Systemj.Exit($3,ln())}*/
    | TEmit symbol TSEMICOLON {Systemj.Emit($2,None,ln())}
    | TPause TSEMICOLON {Systemj.Pause(None,ln(),None)}
    | symbol TColon TPause TSEMICOLON {Systemj.Pause(Some (match $1 with Systemj.Symbol (x,_) -> x),ln(),None)}
    | send TSEMICOLON {$1}
    | receive TSEMICOLON {$1}
    | twhile {$1}
    | dataStmts {Systemj.Data ($1,None)}
    /*| trap {$1}*/
;

typed_signal:
    | dataTypes signal TOpPlus TEqual TInt {Systemj.add_type_and_operator_to_signal $1 Systemj.OpPlus $5 $2 }
    | dataTypes signal TOpTimes TEqual TInt {Systemj.add_type_and_operator_to_signal $1 Systemj.OpTimes $5 $2}
;

signal:
    | TIn TSignal symbol {Systemj.Signal(None,Some Systemj.Input, $3, ln())}
    | TOut TSignal symbol {Systemj.Signal(None,Some Systemj.Output, $3, ln())}
    | TSignal symbol {Systemj.Signal(None,None, $2, ln())}
;

typed_channel:
    | dataTypes channel TEqual TInt {Systemj.add_type_and_operator_to_channel $1 Systemj.OpPlus $4 $2}
;

channel:
    | TIn TChannel symbol {Systemj.Channel(None,Systemj.Input, $3, ln())}
    | TOut TChannel symbol {Systemj.Channel(None,Systemj.Output, $3, ln())}

par:
    | stmt Or Or stmt {Systemj.Spar([$4;$1],ln())}
;

await:
    | TAwait TOP expr TCP {Systemj.Abort($3,Systemj.While(Systemj.True,Systemj.Pause(None ,ln(),None),ln()),ln())}
    | TAwait TOP Timm expr TCP {Systemj.Present($4,Systemj.Noop,
						Some (Systemj.Abort($4,Systemj.While(Systemj.True,Systemj.Pause(None ,ln(),None),ln()),ln())),ln())}
    | symbol TColon TAwait TOP expr TCP {Systemj.Abort($5,Systemj.While(Systemj.True,Systemj.Pause(Some (match $1 with Systemj.Symbol (x,_) -> x)
												      ,ln(),None),ln()),ln())}
;

send:
    | symbol TXCL {Systemj.Send($1,ln())}
;

receive:
    | symbol TQ  {Systemj.Receive($1,ln())}
;

/*trap:
    | TTrap TOP symbol TCP stmt {Systemj.Trap($3,$5,ln())}
;*/

suspend:
    | TSuspend TOP Timm expr TCP stmt {Systemj.Block([Systemj.Abort($4,Systemj.While(Systemj.True,Systemj.Pause(None ,ln(),None),ln()),ln());
						      Systemj.Suspend($4,$6,ln())],ln())}
    | TSuspend TOP expr TCP stmt {Systemj.Suspend($3,$5,ln())}
    | TWeak TSuspend TOP expr TCP stmt {Systemj.Suspend($4,$6,ln())}
    | TWeak TSuspend TOP Timm expr TCP stmt {
      Systemj.Block([Systemj.Present($5,Systemj.Block([PropositionalLogic.surface $7;
						       Systemj.Abort($5,Systemj.While(Systemj.True,Systemj.Pause(None ,ln(),None),ln()),ln())],ln()),
				     None,ln());Systemj.Suspend($5,$7,ln())],ln())}
;
abort:
    | TAbort TOP Timm expr TCP stmt {Systemj.Present($4,Systemj.Noop,Some (Systemj.Abort($4,$6,ln())),ln())}
    | TAbort TOP expr TCP stmt {Systemj.Abort($3,$5,ln())}
    | TWeak TAbort TOP expr TCP stmt {Systemj.Abort($4,$6,ln())}
    | TWeak TAbort TOP Timm expr TCP stmt {Systemj.Present($5,PropositionalLogic.surface $7, Some(Systemj.Abort($5,$7,ln())), ln())}
;

present:
    | TPresent TOP expr TCP stmt {Systemj.Present($3,$5,None,ln())}
    | TPresent TOP expr TCP stmt TElse stmt {Systemj.Present($3,$5,Some $7,ln())}
;

twhile:
    | TWhile stmt {Systemj.While(Systemj.True ,$2,ln())}
;

expr:
    | symbol {Systemj.Esymbol($1,ln(),None)}
    | TXCL expr {Systemj.Not($2,ln())}
    | expr Or Or expr {Systemj.Or($1,$4,ln())}
    | expr And And expr {Systemj.And($1,$4,ln())}
    | TOP expr TCP {Systemj.Brackets($2,ln())}
    | relDataExpr {Systemj.DataExpr ($1)}
;

/*All the data expressions/statements */
relDataExpr:
    | simpleDataExpr TLess simpleDataExpr {Systemj.LessThan($1,$3, ln())}
    | simpleDataExpr TNotEqual simpleDataExpr {Systemj.NotEqualTo($1,$3, ln())}
    | simpleDataExpr TLessEqual simpleDataExpr {Systemj.LessThanEqual($1,$3, ln())}
    | simpleDataExpr TGreater simpleDataExpr {Systemj.GreaterThan($1,$3, ln())}
    | simpleDataExpr TGreaterEqual simpleDataExpr {Systemj.GreaterThanEqual($1,$3, ln())}
    | simpleDataExpr TEqualEqual simpleDataExpr {Systemj.EqualTo($1,$3, ln())}
;

dataStmts:
    | TOB TCB {Systemj.RNoop}
    | TSEMICOLON {Systemj.RNoop}
    | allsym TEqual simpleDataExpr TSEMICOLON {Systemj.Assign($1,$3, ln())}
    | TFor TOP symbol TColon colonExpr TCP dataStmts {Systemj.For($3,$5,$7,None,ln())}
    | doblock TFor TOP symbol TColon colonExpr TCP TSEMICOLON {Systemj.For($4,$6,$1,None,ln())}
    | case {Systemj.CaseDef ($1, ln())}
;

doblock:
    | TDo TOB datastmtlist TCB {Systemj.DataBlock ($3, ln())}
;

case:
    | TCase TOB caseclauselist otherwise TCB {Systemj.Case($3,$4,ln())}
;
caseclauselist:
    | caseclauselist caseclause {$2::$1}
    | caseclause {[$1]}
;
caseclause:
    | TOP expr TCP dataStmts {Systemj.Clause ($2,$4,ln())}
    | TOP expr TCP doblock {Systemj.Clause ($2,$4,ln())}
;
otherwise:
    | TOtherwise dataStmts {Systemj.Otherwise ($2,ln())}
    | TOtherwise doblock {Systemj.Otherwise ($2,ln())}
;

datastmtlist:
    | datastmtlist dataStmts {$2::$1}
    | dataStmts {[$1]}
;

colonExpr:
    | const TColon const TColon simpleDataExpr {Systemj.ColonExpr($1,$3,$5, ln())}
;

allsym:
    | symbol {Systemj.AllSymbol($1)}
    | varsymbol {Systemj.AllTypedSymbol ($1)}
    | signal_data {$1}
;

varsymbol:
    | dataTypes symbol {Systemj.SimTypedSymbol ($1, $2, ln())}
;

signal_data:
    | TQ symbol {Systemj.AllSignalorChannelSymbol $2}
;

simpleDataExpr:
    | simpleDataExpr TPlus simpleDataExpr {Systemj.Plus ($1, $3, ln())}
    | simpleDataExpr TMinus simpleDataExpr {Systemj.Minus ($1, $3, ln())}
    | simpleDataExpr TMod simpleDataExpr {Systemj.Mod ($1, $3, ln())}
    | simpleDataExpr TTimes simpleDataExpr {Systemj.Times ($1, $3, ln())}
    | simpleDataExpr TPow simpleDataExpr {Systemj.Pow ($1, $3, ln())}
    | simpleDataExpr TDiv simpleDataExpr {Systemj.Div ($1, $3, ln())}
    | simpleDataExpr TRShift simpleDataExpr {Systemj.Rshift ($1, $3, ln())}
    | simpleDataExpr TLShift simpleDataExpr {Systemj.Lshift ($1, $3, ln())}
    | TQ symbol {Systemj.SignalOrChannelRef($2, ln())}
    | symbol {Systemj.VarRef ($1, ln())}
    | TOP dataTypes TCP simpleDataExpr {Systemj.Cast ($2,$4,ln())}
    | TMinus simpleDataExpr %prec TUminus {Systemj.Opposite($2,ln())}
    | const {$1}
    | TExtern symbol TOP simpleDataExpr_list TCP {Systemj.Call ($2,$4,ln())}
    | TExtern symbol TOP TCP {Systemj.Call ($2,[],ln())}
;

simpleDataExpr_list:
    | simpleDataExpr TComma simpleDataExpr_list { $1::$3 }
    | simpleDataExpr { [$1] }
;

const:
    | TInt {Systemj.Const (Systemj.Int32s,$1, ln())} /*e.g: 8, the type should be found using type inference, what now??*/
;

/*bool_expr:
    | TTrue {Systemj.True}
    | TFalse {Systemj.False}
;*/

symbol:
    | TSymbol {Systemj.Symbol ($1, ln())} /*e.g.: t*/
;

dataTypes:
    | TInt8s {Systemj.Int8s}
    | TInt16s {Systemj.Int16s}
    | TInt32s {Systemj.Int32s}
;

%%
(* This is the trailer *)
