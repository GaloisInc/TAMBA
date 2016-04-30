%{
  open Pdefs;;
  open Smcdefs;;
  open Gmp;;
  open Gmp.Q.Infixes;;
  open Util;;
%}

%token TEOF

/* language */

%token <string> STRING
%token <int> INT
%token <string> VAR
%token IF PIF THEN ELSE ENDIF ENDPIF WHILE DO ENDWHILE
%token LB RB LP RP LSB RSB RA
%token SEMICOLON COLON DOT COMMA
%token LEQ GEQ LESSER GREATER EQUAL
%token TRUE FALSE
%token PLUS MINUS
%token TIMES DIVIDES
%token LAND LOR
%token SKIP EQUALS
%token UNIFORM
%token DEFINE INCLUDE IN
%token OUTPUT TO

%token <int> TINT
%token TINTDEF
%token TBOOL
%token TRECORD
%token TENUM
%token ASSIGN

%left SEMICOLON
%left EQUALS
%left LOR LAND
%left LEQ GEQ LESSER GREATER EQUAL
%left PLUS MINUS
%left TIMES DIVIDES

%type <Lang.pstmt> pstmt

/* end of language */

/* mock setups */

%token <float> FLOAT
%token COLON SECRET BELIEF QUERY ABOUT
%token POLICY POLICYSIM QUERYDEF

%start pstmt
%start pstmtfile
%start pmock
%start smcmock

%type <Lang.pstmt> pstmtfile
%type <Smcdefs.tsmcmocksetup> smcmock
%type <Pdefs.tpmocksetup> pmock
%type <Gmp.Q.t> rational

/* end of mock setups */
%%

/* lang */

varid:
| VAR { ("", $1) }
;

lbinop :
| LAND    { ("and", Lang.logical_and) }
| LOR     { ("or", Lang.logical_or) }
;

lreln :
| LEQ     { ("<=", Lang.leq) }
| GEQ     { (">=", Lang.geq) }
| LESSER  { ("<", Lang.lesser) }
| GREATER { (">", Lang.greater) }
| EQUAL   { ("==", Lang.equal) }
;

abinop1 :
| TIMES   { ("*", Lang.times) }
| DIVIDES { ("/", Lang.divides) }
;

abinop2 :
| PLUS    { ("+", Lang.plus) }
| MINUS   { ("-", Lang.minus) }
;

lexp :
| TRUE { Lang.LEBool (1) }
| FALSE { Lang.LEBool (0) }
| lexp lbinop lexp %prec LAND { Lang.LEBinop ( $2, $1, $3) }
| aexp lreln aexp %prec LEQ { Lang.LEReln ( $2, $1, $3 ) }
| LP lexp RP { $2 }
;

aexp :
| varid { Lang.AEVar ($1) }
| INT { Lang.AEInt ($1) }
| aexp abinop1 aexp %prec TIMES { Lang.AEBinop ($2, $1, $3) }
| aexp abinop2 aexp %prec PLUS { Lang.AEBinop ($2, $1, $3) }
| LP aexp RP { $2 }
;


pstmt :
| DEFINE varid ASSIGN aexp IN pstmt { Lang.PSSubst ($2, $4, $6) }
| INCLUDE STRING IN pstmt { Lang.PSInc ($2, ! Globals.currently_parsing, $4) }
| INCLUDE STRING { Lang.PSInc ($2, !Globals.currently_parsing, Lang.PSStmt Lang.SSkip) }
| stmt { Lang.PSStmt ($1) }
;

datatype :
| TBOOL { Lang.TBool }
| TINT { Lang.TInt ($1) }
| TINTDEF { Lang.TInt (32) }
;

agent :
| VAR { $1 }
;

agentlist:
| { [] }
| agent { [$1] }
| agent COMMA agentlist { $1 :: $3 }
;

nonemptyagentlist:
| agent { [$1] }
| agent COMMA agentlist { $1 :: $3 }
;

record_body :
| datatype varid ASSIGN aexp SEMICOLON record_body {
  ($1, $2, Some($4), None)::($6) }
| datatype varid ASSIGN aexp {
  [($1, $2, Some($4), None)]
  }
| datatype varid ASSIGN UNIFORM INT INT  {
  [($1, $2, None, Some($5, $6))]
}
| datatype varid ASSIGN UNIFORM INT INT SEMICOLON record_body {
  ($1, $2, None, Some($5, $6))::$8
}

enum_body :
| varid enum_body {
  $1::$2
}
| varid{
  $1
}

stmt :
| stmt SEMICOLON stmt { Lang.SSeq ($1, $3) }
| TENUM varid ASSIGN LB enum_body RB {
    (*Enum type declaration as record*)
    let enum_ids = $5 in
    let (enum_varid_agent, enum_varid_str) = $2 in

    let (typedef, field_ids, stmts, _) =
      List.fold_left (
        fun (datatypes, ids, stmts, ctr) (enum_agent, enum_id) ->
          let new_var_name = enum_agent, enum_varid_str^"."^enum_id in

          let define_var_stmt = Lang.SDefine((new_var_name), Lang.TInt(32)) in
          let assign_var_stmt = Lang.SAssign ((new_var_name), Lang.AEInt(ctr)) in
          let curr_stmt = Lang.SSeq(define_var_stmt, assign_var_stmt) in
          ((enum_varid_str, Lang.TInt(32))::datatypes, varid_str::ids, (Lang.SSeq(curr_stmt, stmts)), ctr+1)

      ) ([],[],Lang.SSkip,0) enum_ids in

    let record_type = Lang.TRecord(typedef) in
    let record_data = Lang.AERecord (field_ids) in
    let record_assign =
      Lang.SSeq(Lang.SDefine($2, record_type), Lang.SAssign($2, record_data)) in

    Lang.SSeq(record_assign, stmts)
}

| TRECORD varid ASSIGN LB record_body RB {
    let fields = $5 in
    let (record_varid_agent, record_varid_str) = $2 in
    let (typedef, field_ids, stmts) =
      List.fold_left (
        fun (datatypes, ids, stmts) (datatype, field_varid, val_opt, dist_opt) ->
          let (varid_agent, varid_str) = field_varid in
          match val_opt with
          | Some (value) ->
              let new_var_name = varid_agent, record_varid_str^"."^varid_str in

              let define_var_stmt = Lang.SDefine((new_var_name), datatype) in
              let assign_var_stmt = Lang.SAssign ((new_var_name), value) in
              let curr_stmt =Lang.SSeq(define_var_stmt, assign_var_stmt) in
              ((varid_str, datatype)::datatypes, varid_str::ids, (Lang.SSeq(curr_stmt, stmts)))
          | None ->
            (match dist_opt with
            | Some (low_b, up_b) ->
              let new_var_name = varid_agent, record_varid_str^"."^varid_str in
              let dist_stmt = Lang.SUniform (new_var_name, low_b, up_b) in
              let define_var_stmt = Lang.SDefine((new_var_name), datatype) in
              let curr_stmt =Lang.SSeq(define_var_stmt, dist_stmt) in
              ((varid_str, datatype)::datatypes, varid_str::ids, (Lang.SSeq(curr_stmt, stmts)))
            | None -> failwith "variable not assigned anything")
      ) ([],[],Lang.SSkip) fields in
    let record_type = Lang.TRecord(typedef) in
    let record_data = Lang.AERecord (field_ids) in
    let record_assign =
      Lang.SSeq(Lang.SDefine($2, record_type), Lang.SAssign($2, record_data)) in

    Lang.SSeq(record_assign, stmts)
  }
| datatype varid ASSIGN aexp { Lang.SSeq (Lang.SDefine ($2, $1),
					  Lang.SAssign ($2, $4))}
| datatype varid ASSIGN UNIFORM INT INT { Lang.SSeq (Lang.SDefine ($2, $1),
						     Lang.SUniform ($2, $5, $6))}
| datatype varid { Lang.SDefine ($2, $1) }
| varid ASSIGN UNIFORM INT INT { Lang.SUniform ($1, $4, $5) }
| varid ASSIGN aexp { Lang.SAssign ($1, $3) }
| PIF INT COLON INT THEN stmt ELSE stmt ENDPIF
  { Lang.SPSeq ($6, $8, Q.from_ints $2 ($2 + $4), $2, $4) }
| PIF INT COLON INT THEN stmt ENDPIF
  { Lang.SPSeq ($6, Lang.SSkip, Q.from_ints $2 ($2 + $4), $2, $4) }
| IF lexp THEN stmt ELSE stmt ENDIF
  { Lang.SIf ($2, $4, $6) }
| IF lexp THEN stmt ENDIF
  { Lang.SIf ($2, $4, Lang.SSkip) }
| WHILE lexp DO stmt ENDWHILE
  { Lang.SWhile ($2, $4) }
| LB stmt RB { $2 }
| SKIP { Lang.SSkip }
| stmt SEMICOLON { $1 }
| OUTPUT varid TO nonemptyagentlist { Lang.SOutput($2, $4) }
;

pstmtfile :
| pstmt TEOF { $1 }
;

/* end of lang */


/* mock up setups */

rational :
| FLOAT { Q.from_float $1 }
| INT DIVIDES INT { Q.from_ints $1 $3 }
;

varlist :
| { [] }
| varid varlist { $1 :: $2 }
  ;

secret :
| SECRET COLON pstmt { ("", $3) }
| SECRET VAR COLON pstmt { ($2, $4) }
;

secretlist :
| { [] }
| secret secretlist { $1 :: $2 }
;

belief :
| BELIEF COLON pstmt { ([], [], $3) }
| BELIEF agentlist ABOUT agentlist COLON pstmt { ($2, $4, $6) }
;

belieflist :
| { [] }
| belief belieflist { $1 :: $2 }
;

querylist :
| { [] }
| query querylist { $1 :: $2 }
;

query :
| QUERY COLON pstmt { $3 }
;

querynamed :
| QUERY VAR COLON pstmt { ($2, $4) }
;

querynamedlist :
| { [] }
| querynamed querynamedlist { $1 :: $2 }
;

querydef :
| QUERYDEF VAR varlist RA varlist COLON pstmt { ($2, ($3, $5, $7)) }
| QUERYDEF VAR varlist COLON pstmt { ($2, ($3, [], $5)) }
;

querydeflist :
| { [] }
| querydef querydeflist { $1 :: $2 }
;

policy :
| POLICY COLON VAR LSB varlist RSB rational {
    {Pdefs.name = $3;
     Pdefs.varlist = $5;
     Pdefs.param = $7}: Pdefs.tpolicy
  }
;

policysmc :
| POLICY VAR ABOUT agentlist COLON LSB varlist RSB rational {
    {Smcdefs.agent = $2;
     Smcdefs.aboutagents = $4;
     Smcdefs.varlist = $7;
     Smcdefs.param = $9}: Smcdefs.tpolicy
  }
;

policylist :
| { [ ] }
| policy policylist { $1 :: $2 }
;

policysmclist :
| { [] }
| policysmc policysmclist { $1 :: $2 }
;

smcmock:
| secretlist belieflist policysmclist querydeflist querynamedlist TEOF
    { {
	Smcdefs.secrets = $1;
	Smcdefs.beliefs = $2;
	Smcdefs.policies = $3;
	Smcdefs.querydefs = $4;
	Smcdefs.queries = $5
      }: Smcdefs.tsmcmocksetup }
;

pmock :
| secret belief policylist querydeflist querynamedlist TEOF {
    {Pdefs.secret = pair_second $1;
     Pdefs.belief = triple_third $2;
     Pdefs.policies = $3;
     Pdefs.querydefs = $4;
     Pdefs.queries = $5}: Pdefs.tpmocksetup  }
;

/* end of mockup setups */

%%
