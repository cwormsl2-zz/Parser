//**************************************************

//Caitlin Wormsley
//11/15/2015
//Implementation of a Lexer and Parser based on the made up language 'Spoooky C'


//Everything should work except the if and while statements 


//***************************************************


type Token = 
    |IF | WHILE | ELSE | PRINT | SCAN | TYPE_IDENT
    |OPEN_P | CLOSE_P | END_IF | END_WHILE | END_LINE
    |LIST_MUTATOR | LIST_ACCESSOR
    |IDENT | LITERAL | ASSIGN
    |BIN1_OP |BIN2_OP |BIN3_OP | UN_OP  | COMMA | EMPTY
    //anything invalid
    |ERROR


//KEYWORDS
//*************************************************

let ifStmt lexSoFar str =
    match str with 
        |"" -> lexSoFar, ERROR, str
        |_ ->
            let tempLex = lexSoFar + string str.[0]
            match str.[0] with
            |'f' -> tempLex, IF, str.[1..]
            |_ -> lexSoFar, ERROR, str

let intKeyword lexSoFar str =
    match str with 
            |"" -> lexSoFar, ERROR, str
            |_ ->
                let tempLex = lexSoFar + string str.[0]
                match str.[0] with
                |'t' -> tempLex, TYPE_IDENT, str.[1..]
                |_ -> lexSoFar, ERROR, str

let letterI lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'f' -> ifStmt tempLex str.[1..]
        |'n' -> intKeyword tempLex str.[1..]
        |_ -> lexSoFar, ERROR, str




let elsStmt lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'l' -> 
              match str.[1] with 
              |'s' -> lexSoFar + string str.[0] + string str.[1], ELSE, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str



let wylStmt lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'y' -> 
              match str.[1] with 
              |'l' -> lexSoFar + string str.[0] + string str.[1], WHILE, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str


let prtStmt lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'r' -> 
              match str.[1] with 
              |'t' -> lexSoFar + string str.[0] + string str.[1], PRINT, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str



let sknStmt lexSoFar str =
    match str with 
            |"" -> lexSoFar, ERROR, str
            |_ ->
                let tempLex = lexSoFar + string str.[0]
                match str.[0] with
                |'n' -> tempLex, SCAN, str.[1..]
                |_ -> lexSoFar, ERROR, str

let strKeyWord lexSoFar str =
    match str with 
            |"" -> lexSoFar, ERROR, str
            |_ ->
                let tempLex = lexSoFar + string str.[0]
                match str.[0] with
                |'r' -> tempLex, TYPE_IDENT, str.[1..]
                |_ -> lexSoFar, ERROR, str


let letterS lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'k' -> sknStmt tempLex str.[1..]
        |'t' -> strKeyWord tempLex str.[1..]
        |_ -> lexSoFar, ERROR, str

let booKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'o' -> 
              match str.[1] with 
              |'o' -> lexSoFar + string str.[0] + string str.[1], TYPE_IDENT, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let dubKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'u' -> 
              match str.[1] with 
              |'b' -> lexSoFar + string str.[0] + string str.[1], TYPE_IDENT, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str


let lstKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'s' -> 
              match str.[1] with 
              |'t' -> lexSoFar + string str.[0] + string str.[1], TYPE_IDENT, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str




//*******************************************************

//LIST MUTATOR and LIST_ACCESSOR
//********************************************************
let app lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'p' -> 
              match str.[1] with 
              |'p' -> lexSoFar + string str.[0] + string str.[1], LIST_MUTATOR, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let rmv lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'m' -> 
              match str.[1] with 
              |'v' -> lexSoFar + string str.[0] + string str.[1], LIST_MUTATOR, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let len lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'e' -> 
              match str.[1] with 
              |'n' -> lexSoFar + string str.[0] + string str.[1], LIST_ACCESSOR, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let get lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'e' -> 
              match str.[1] with 
              |'t' -> lexSoFar + string str.[0] + string str.[1], LIST_ACCESSOR, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let period lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'a' -> app tempLex str.[1..]
        |'r' -> rmv tempLex str.[1..]
        |'l' -> len tempLex str.[1..]
        |'g' -> get tempLex str.[1..]
        |_ -> lexSoFar, ERROR, str
//*******************************************************



//STRING LITERAL
//******************************************************
let rec stringLiteral lexSoFar str =
    match str with 
    |"" -> lexSoFar, ERROR, str
    |_ ->
        let tempLex = lexSoFar + string str.[0] 
        match str.[0] with
        |'"' -> lexSoFar, LITERAL, str.[1..]
        | _ -> stringLiteral tempLex str.[1..]
//******************************************************


//INT LITERAL
//*****************************************************

let rec floatLiteral lexSoFar str =
    match str with 
        |"" -> lexSoFar, LITERAL, str
        |_ ->
            let tempLex = lexSoFar + string str.[0] 
            match str.[0] with
            |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0'|'.' -> floatLiteral tempLex str.[1..]
            |_-> lexSoFar, LITERAL, str



let rec intLiteral lexSoFar str =
    match str with 
    |"" -> lexSoFar, LITERAL, str
    |_ ->
        let tempLex = lexSoFar + string str.[0] 
        match str.[0] with
        |'.' -> floatLiteral tempLex str.[1..]
        |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0' -> intLiteral tempLex str.[1..] 
        | _ -> lexSoFar, LITERAL, str
//*****************************************************

//BOOELAN LITERAL
//*****************************************************
let truKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'r' -> 
              match str.[1] with 
              |'u' -> lexSoFar + string str.[0] + string str.[1], LITERAL, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str


let letterL lexSoFar str =
    match str with 
            |"" -> lexSoFar, ERROR, str
            |_ ->
                let tempLex = lexSoFar + string str.[0]
                match str.[0] with
                |'o' -> tempLex, TYPE_IDENT, str.[1..]
                |'s' -> tempLex, LITERAL, str.[1..]
                |_ -> lexSoFar, ERROR, str

let letterF lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'l' -> letterL tempLex str.[1..]
        |_ -> lexSoFar, ERROR, str

//*****************************************************



//STARTS WITH COLON
//*****************************************************
let colonCarrot lexSoFar str = 
    match str with
    |"" -> lexSoFar, ERROR, str
    | _ ->
        match str.[0] with
        |'|' -> lexSoFar + string str.[0], END_IF, str.[1..]
        |'{' -> lexSoFar + string str.[0], END_WHILE, str.[1..]
        |_ -> lexSoFar, ERROR, str

let endLine lexSoFar str =
 match str with
    |"" -> lexSoFar, ERROR, str
    | _ ->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'{' -> tempLex, END_LINE, str.[1..]
        |_ -> lexSoFar, ERROR, str


let colon lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    | _ ->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'^' -> colonCarrot tempLex str.[1..]
        |'''-> endLine tempLex str.[1..]
        |')' -> tempLex, CLOSE_P, str.[1..]
        |_-> lexSoFar, ERROR, str
//*************************************************


//ASSIGNMENT OPERATOR X^O
//************************************************
let xCarrot lexSoFar str =
    match str with
    |"" -> failwith lexSoFar, ERROR, str
    | _ ->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'O' -> tempLex, ASSIGN, str.[1..]
        | _ -> lexSoFar, ERROR, str


let assign lexSoFar str =
    match str with 
    |"" -> lexSoFar, ERROR, str
    |_ ->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |'^' -> xCarrot tempLex str.[1..]
        |_-> lexSoFar, ERROR, str
//**********************************************

//STAND ALONE FUNCTIONS
//*********************************************
let unOp lexSoFar str =
    lexSoFar, UN_OP, str

let comma lexSoFar str =
    lexSoFar, COMMA, str


let bin3Op lexSoFar str =
    match str with
    |"" -> failwith "Can't lex an empty string"
    | _ ->
        match str.[0] with
        |'=' -> lexSoFar + string str.[0], BIN3_OP, str.[1..]
        |_ -> lexSoFar, BIN3_OP, str

let bin1Op lexSoFar str =
    lexSoFar, BIN1_OP, str

let bin2Op lexSoFar str =
    lexSoFar, BIN2_OP, str







let notKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'o' -> 
              match str.[1] with 
              |'t' -> lexSoFar + string str.[0] + string str.[1], UN_OP, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let andKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'n' -> 
              match str.[1] with 
              |'d' -> lexSoFar + string str.[0] + string str.[1], BIN3_OP, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str

let oorKeyword lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    |_->
        match str.[0] with 
        |'o' -> 
              match str.[1] with 
              |'r' -> lexSoFar + string str.[0] + string str.[1], BIN3_OP, str.[2..]
              |_ -> lexSoFar, ERROR, str
        |_-> lexSoFar, ERROR, str



let openP lexSoFar str =
    match str with
    |"" -> lexSoFar, ERROR, str
    | _ ->
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with
        |':' -> tempLex, OPEN_P, str.[1..]
        |_ -> lexSoFar, ERROR, str
//******************************************



//IDENT - have it so ident can only be made of letters - not numbers or other chars
//******************************************
let rec ident lexSoFar str =
    match str with
    |"" -> failwith "Can't lex empty string"
    |_ -> 
        let tempLex = lexSoFar + string str.[0]
        match str.[0] with 
        |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0' -> tempLex, IDENT, str.[1..]
        |'a'|'A'|'b'|'B'|'c'|'C'|'d'|'D'|'e'|'E'|'f'|'F'|'g'|'G'|'h'|'H'|'i'  -> ident tempLex str.[1..]
        |'I'|'j'|'k'|'K' |'l'|'L'|'m'|'M'|'n'|'o'|'O'|'p'|'P'|'q'|'Q'|'r'|'R'  -> ident tempLex str.[1..]
        |'s'|'S'|'t'|'T'|'u'|'U'|'v'|'V'|'w'|'W'|'x'|'X'|'y'|'Y'|'z'|'Z' -> ident tempLex str.[1..] 
        |_ -> lexSoFar, IDENT, str
//********************************************
        


let rec nextLex progStr =
    match progStr with
    |"" -> failwith "Can't lex an empty string"
    | _ ->
        let currChar = progStr.[0]
        let currStr = string currChar
        let rest = progStr.[1..]
        //find which arrow to follow in the FSM...
        match currChar with
        |'"' -> stringLiteral currStr rest
        |'-' -> unOp currStr rest
        |'+'  -> bin2Op currStr rest
        |'/' | '*'  -> bin1Op currStr rest
        |'>'| '<'|'=' -> bin3Op currStr rest
        |'a' -> andKeyword currStr rest
        |'b' -> booKeyword currStr rest
        |'e' -> elsStmt currStr rest
        |'f' -> letterF currStr rest
        |'i' -> letterI currStr rest
        |'n' -> notKeyword currStr rest
        |'o' -> oorKeyword currStr rest
        |'p' -> prtStmt currStr rest
        |'s' -> letterS currStr rest
        |'t' -> truKeyword currStr rest
        |'w' -> wylStmt currStr rest
        |':' -> colon currStr rest
        |'(' -> openP currStr rest
        |'X' -> assign currStr rest
        |'#' -> ident currStr rest
        |',' -> comma currStr rest
        |'.' -> period currStr rest
        |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0' -> intLiteral currStr rest
        |' '|'\n' -> nextLex rest
        |_ -> currStr, ERROR, rest


let testLexer program = 
    let rec testLexerHelper remainingProgram currLexList = 
        let token, lexeme, rest = nextLex remainingProgram
        match rest with
            |"" ->List.rev ((token, lexeme)::currLexList)
            |_ -> testLexerHelper rest ((token, lexeme)::currLexList)
    testLexerHelper program []




(*
*
*
*
*
*
*
*Parser Begins Here
*
*
*
*
*
*)

//the data structure to represent our parse tree
type ParseNode = 
    |Nonterminal of string * ParseNode list
    |Terminal of Token * string



let parseTreeToString root = 
    let rec childrenToString nodeList indent = 
        match nodeList with
        |[] -> failwith "Nonterminal without terminals"
        |head::[] -> nodeToString head indent
        |head::tail -> (nodeToString head indent) + childrenToString tail indent

    and nodeToString root indent =
        match root with
        |Terminal(token, lexeme) ->  indent + "|"+ (sprintf "%A : " token) + lexeme + "\n" 
        |Nonterminal (name, children) ->
            indent+ "|"+name+"\n" + childrenToString children (indent+"|  ")
    nodeToString root "" 



(*

***********************************************
***********************************************
*************************************************
*************************************************
*)




//************<exp_piece>*************************************
//*************************************************************
(*<exp_piece> → LITERAL | IDENT | UN_OP <exp_piece>*)
let rec expPiece lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <exp_piece> - recieved empty list"
    |head::tail -> 
        let lex, token = head
       // printfn "%A" token
        match token with
        |IDENT -> Nonterminal("<exp_piece>", [Terminal(token, lex)]), tail
        |LITERAL -> Nonterminal("<exp_piece>", [Terminal(token, lex)]), tail
        |UN_OP -> 
            let newExpPiece, rest = expPiece tail
            Nonterminal("<exp_piece>", [Terminal(token, lex); newExpPiece]), rest
        |_-> failwith ("ERROR creating <exp_piece> - IDENT, LITERAL or UN_OP expected but recieved" + (sprintf "%A : " head))


//******************<exp1_piece> and <exp1_rest>******************
//****************************************************************
(*<exp1_piece> → <exp_piece> <exp1_rest>
<exp1_rest> → BIN1_OP <exp1_piece> | EMPTY*)



(*<exp1_piece> → <exp_piece> <exp1_rest>*)
let rec exp1Piece lexList =
    match lexList with
    |[] -> failwith "ERROR creating <exp1_piece> - recieved empty list"
    |head::tail -> 
        let expTree, rest = expPiece lexList
        let restTree, rest1 = exp1Rest rest 
        Nonterminal("<exp1_piece>", [expTree; restTree]), rest1


(*<exp1_rest> → BIN1_OP <exp1_piece> | EMPTY*)
and exp1Rest lexList = 
    match lexList with
    |[] -> Nonterminal("<exp1_rest>", [Terminal(EMPTY, "")]), []
    |head::tail -> 
        let lex, token = head
        match token with
        |BIN1_OP -> 
            let newExp1Rest, rest = exp1Piece tail
            Nonterminal("<exp1_rest>", [Terminal(token, lex); newExp1Rest]), rest
        |_ -> Nonterminal("<exp1_rest>", [Terminal(EMPTY, "")]), lexList




//******************<exp2_piece> and <exp2_rest>******************
//****************************************************************
(*<exp2_piece> → <exp1_piece><exp2_rest>
<exp2_rest> BIN2_OP <exp2_piece> | EMPTY*)

(*<exp2_piece> → <exp1_piece><exp2_rest>*)
let rec exp2Piece lexList =
    match lexList with
    |[] -> failwith "ERROR creating <exp2_piece> - recieved empty list"
    |head::tail -> 
        let expTree, rest = exp1Piece lexList
        let restTree, rest1 = exp2Rest rest 
        Nonterminal("<exp2_piece>", [expTree; restTree]), rest1


(*<exp2_rest> BIN2_OP <exp2_piece> | EMPTY*)
and exp2Rest lexList = 
    match lexList with
    |[] -> Nonterminal("<exp2_rest>", [Terminal(EMPTY, "")]), []
    |head::tail -> 
        let lex, token = head
        match token with
        |BIN2_OP -> 
            let newExp2Rest, rest = exp2Piece tail
            Nonterminal("<exp2_rest>", [Terminal(token, lex); newExp2Rest]), rest
        |_ -> Nonterminal("<exp2_rest>", [Terminal(EMPTY, "")]), lexList




//******************<exp> and <exp_rest>**************************
//****************************************************************
(*<exp> → <exp2_piece><exp_rest>
<exp_rest> → BIN3_OP <exp> | EMPTY*)



(*<exp> → <exp2_piece><exp_rest>*)
let rec exp lexList =
    match lexList with
    |[] -> failwith "ERROR creating <exp> - recieved empty list"
    |head::tail -> 
        let expTree, rest = exp2Piece lexList
        let restTree, rest1 = expRest rest 
        Nonterminal("<exp>", [expTree; restTree]), rest1

(*<exp_rest> → BIN3_OP <exp> | EMPTY*)
and expRest lexList = 
    match lexList with
    |[] -> Nonterminal("<exp_rest>", [Terminal(EMPTY, "")]), lexList
    |head::tail -> 
        let lex, token = head
        match token with
        |BIN3_OP -> 
            let newExpRest, rest = exp tail
            Nonterminal("<exp_rest>", [Terminal(token, lex); newExpRest]), rest
        |_ -> Nonterminal("<exp_rest>", [Terminal(EMPTY, "")]), lexList



//******************SCAN STMT**************************
//****************************************************************
(*<scan_stmt> →  SCAN OPEN_P IDENT CLOSED_P END_LINE*)
let rec scanStmt lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <scan_stmt> - recieved empty list"
    |head::tail->
        let lex, token = head
        match token with
        |SCAN -> 
            let term1 = Terminal(token, lex)
            match tail with
            |[] -> failwith "ERROR creating <scan_stmt> - expected OPEN_P but recieved empty list"
            |head2::tail2 ->
                let lex2, token2 = head2
                match token2 with
                |OPEN_P ->
                    let term2 = Terminal(token2, lex2)
                    match tail2 with
                    |[] -> failwith "ERROR creating <scan_stmt> - expected IDENT but recieved empty list"
                    |head3::tail3 ->
                        let lex3, token3 = head3
                        match token3 with
                        |IDENT ->
                            let term3 = Terminal(token3, lex3)
                            match tail3 with
                            |[] -> failwith "ERROR creating <scan_stmt> - expected CLOSE_P but recieved empty list"
                            |head4::tail4 ->
                                let lex4, token4 = head4
                                match token4 with
                                |CLOSE_P->
                                    let term4 = Terminal(token4, lex4)
                                    match tail4 with
                                    |[] -> failwith "ERROR creating <scan_stmt> - expected END_LINE but recieved empty list"
                                    |head5::tail5 ->
                                        let lex5, token5 = head5
                                        match token5 with
                                        |END_LINE->
                                            Nonterminal("<scan_stmt>", [term1; term2;term3;term4;Terminal(token5, lex5)]), tail5
                                        |_-> failwith ("ERROR creating <scan_stmt> - expected END_LINE but recieved " + (sprintf "%A : " head5))
                                |_-> failwith ("ERROR creating <scan_stmt> - expected CLOSE_P but recieved " + (sprintf "%A : " head4))
                        |_-> failwith ("ERROR creating <scan_stmt> - expected IDENT but recieved " + (sprintf "%A : " head3))
                |_-> failwith ("ERROR creating <scan_stmt> - expected OPEN_P but recieved " + (sprintf "%A : " head2))
        |_-> failwith ("ERROR creating <scan_stmt> - expected SCAN but recieved " + (sprintf "%A : " head))








//******************PRINT*****************************************
//****************************************************************

(*<print_stmt> →  PRINT OPEN_P <to_print> CLOSED_P END_LINE
<to_print> →  <exp> <to_print_rest>
<to_print_rest> → COMMA <exp><to_print_rest> | EMPTY
*)


(*to_print_rest> → COMMA <exp><to_print_rest> | EMPTY*)
let rec toPrintRest lexList = 
    match lexList with
    |[] -> Nonterminal("<to_print_rest>", [Terminal(EMPTY, "")]), []
    |head::tail -> 
        let lex, token = head
        match token with
        |COMMA -> 
            let newToPrintRest, rest = exp tail
            let restTree, rest1 = toPrintRest rest
            Nonterminal("<to_print_rest>", [Terminal(token, lex); newToPrintRest; restTree]), rest1
        |_-> Nonterminal("<to_print_rest>", [Terminal(EMPTY, "")]), lexList

(*<to_print> →  <exp> <to_print_rest>*)
let toPrint lexList =
    match lexList with
    |[] ->failwith "ERROR creating <to_print> - empty list"
    |head::tail ->
        let expTree, rest = exp lexList
        let toPrintRestTree, rest1 = toPrintRest rest
        Nonterminal("<to_print>", [expTree; toPrintRestTree]), rest1



(*<print_stmt> →  PRINT OPEN_P <to_print> CLOSED_P END_LINE*)
let rec printStmt lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <print_stmt> - expected PRINT but recieved empty list"
    |head::tail->
        let lex, token = head
        match token with
        |PRINT -> 
            let term1 = Terminal(token, lex)
            match tail with
            |[] -> failwith ("ERROR creating <print_stmt> - expected OPEN_P but recieved empty list")
            |head2::tail2 ->
                let lex2, token2 = head2
                match token2 with
                |OPEN_P ->
                    let term2 = Terminal(token2, lex2)
                    let tree, rest = toPrint tail2
                    match rest with
                    |[] -> failwith  ("ERROR creating <print_stmt> - expected CLOSE_P but recieved empty list")
                    |head3::tail3 ->
                        let lex3, token3 = head3
                        match token3 with
                        |CLOSE_P ->
                            let term3 = Terminal(token3, lex3)
                            match tail3 with
                            |[] -> failwith  ("ERROR creating <print_stmt> - expected END_LINE but recieved empty list")
                            |head4::tail4 ->
                                let lex4, token4 = head4
                                match token4 with                           
                                |END_LINE->
                                    Nonterminal("<print_stmt>", [term1;term2;tree;term3;Terminal(token4, lex4)]), tail4
                                |_->failwith  ("ERROR creating <print_stmt> - expected END_LINE but recieved " + (sprintf "%A : " head4))
                        |_->failwith  ("ERROR creating <print_stmt> - expected CLOSE_P but recieved " + (sprintf "%A : " head3))
                |_->failwith  ("ERROR creating <print_stmt> - expected OPEN_P but recieved " + (sprintf "%A : " head2))
        |_->failwith  ("ERROR creating <print_stmt> - expected PRINT but recieved " + (sprintf "%A : " head))
                                    
                       
                  



//******************ASSGN OR LIST MUT****************************
//***************************************************************

(*<assn_or_list> →  IDENT <choose_assn_or_list>
<choose_assn_or_list> →  <assn_stmt> |< list_mut_stmt>
<assn_stmt> →  ASSIGN <exp> END_LINE
<list_mut_stmt> → LIST_MUTATOR  OPEN_P <exp> CLOSE_P END_LINE
*)

(*<assn_stmt> →  ASSIGN <exp> END_LINE*)
let assnStmt lexList = 
    match lexList with
        |[] -> failwith "ERROR creating <assn_stmt> - recieved empty list"
        |head::tail->
            let lex, token = head
            match token with
            |ASSIGN->
                let term1 = Terminal(token, lex)
                let expTree, rest = exp tail
                match rest with
                |[] -> failwith "ERROR creating <assn_stmt> - expected END_LINE but recieved empty list"
                |head2::tail2->
                    let lex2, token2 = head2
                    match token2 with
                    |END_LINE ->
                        Nonterminal("<assn_stmt>", [term1; expTree; Terminal(token2, lex2)]), tail2
                    |_-> failwith  ("ERROR creating <assn_stmt> - expected END_LINE but recieved " + (sprintf "%A : " head2))
            |_-> failwith  ("ERROR creating <assn_stmt> - expected ASSIGN but recieved " + (sprintf "%A : " head))


(*<list_mut_stmt> → LIST_MUTATOR  OPEN_P <exp> CLOSE_P END_LINE*)
let rec listMutStmt lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <list_mut_stmt> - recieved empty list"
    |head::tail->
        let lex, token = head
        match token with
        |LIST_MUTATOR -> 
            let term1 = Terminal(token, lex)
            match tail with
            |[] -> failwith "ERROR creating <list_mut_stmt> - expected OPEN_P but recieved empty list"
            |head2::tail2 ->
                let lex2, token2 = head2
                match token2 with
                |OPEN_P ->
                    let term2 = Terminal(token2, lex2)
                    let tree, rest = exp tail2
                    match rest with
                    |[] -> failwith "ERROR creating <list_mut_stmt> - expected CLOSE_P but recieved empty list"
                    |head3::tail3 ->
                        let lex3, token3 = head3
                        match token3 with
                        |CLOSE_P ->
                            let term3 = Terminal(token3, lex3)
                            match tail3 with
                            |[] -> failwith "ERROR creating <list_mut_stmt> - expected END_LINE but recieved empty list"
                            |head4::tail4 ->
                                let lex4, token4 = head4
                                match token4 with                           
                                |END_LINE->
                                    Nonterminal("<list_mut_stmt>", [term1;term2;tree;term3;Terminal(token4, lex4)]), tail4
                                |_->failwith  ("ERROR creating <list_mut_stmt> - expected END_LINE but recieved " + (sprintf "%A : " head4))
                        |_->failwith ("ERROR creating <list_mut_stmt> - expected CLOSE_P but recieved " + (sprintf "%A : " head3))
                |_->failwith ("ERROR creating <list_mut_stmt> - expected OPEN_P but recieved " + (sprintf "%A : " head2))
        |_->failwith ("ERROR creating <list_mut_stmt> - expected LIST_MUTATOR but recieved " + (sprintf "%A : " head))





(*<choose_assn_or_list> →  <assn_stmt> |< list_mut_stmt>*)
let chooseAssnList lexList =
    match lexList with
        |[] -> failwith "ERROR creating <choose_assn_or_list> - expected ASSIGN or LIST_MUTATOR but recieved empty list"
        |head::tail->
            let lex, token = head
            match token with
            |ASSIGN ->
                let assnTree, rest = assnStmt lexList
                Nonterminal("<choose_assn_or_list>", [assnTree]), rest
            |LIST_MUTATOR ->
                let listTree, rest = listMutStmt lexList
                Nonterminal("<choose_assn_or_list>", [listTree]), rest 
            |_-> failwith ("ERROR creating <choose_assn_or_list> - expected ASSIGN or LIST_MUTATOR but recieved " + (sprintf "%A : " head))
        


(*<assn_or_list> →  IDENT <choose_assn_or_list>*)
let assnOrList lexList =
    match lexList with
     |[] -> failwith "ERROR creating <assn_or_list> - expected IDENT but recieved empty list"
     |head::tail->
        let lex, token = head
        match token with
        |IDENT -> 
            let chooseTree, rest = chooseAssnList tail
            Nonterminal("<assn_or_list>", [Terminal(token, lex); chooseTree]), rest
        |_-> failwith ("ERROR creating <assn_or_list> - expected IDENT but recieved " + (sprintf "%A : " head))
        








//******************ASSGN OR DECL STMT**************************
//***************************************************************
(*<declare_stmt> → TYPE_IDENT IDENT <assign_mod> END_LINE
<assign_mod> →ASSIGN <exp> | EMPTY*)


(*<assign_mod> →ASSIGN <exp> | EMPTY*)
let assignMod lexList =
    match lexList with
    |[] -> Nonterminal("<assign_mod>", [Terminal(EMPTY, "")]), lexList
    |head::tail->
        let lex, token = head
        match token with
        |ASSIGN->
            let expTree, rest = exp tail
            Nonterminal("<assign_mod>", [Terminal(token, lex); expTree]), rest
        |_-> Nonterminal("<assign_mod>", [Terminal(EMPTY, "")]), lexList


(*<declare_stmt> → TYPE_IDENT IDENT <assign_mod> END_LINE*)
let rec declareStmt lexList = 
    match lexList with
    |[] -> failwith "ERROR creating decalre_stmt> - expected TYPE_IDENT but recieved empty list"
    |head::tail->
        let lex, token = head
        match token with
        |TYPE_IDENT -> 
            let term1 = Terminal(token, lex)
            match tail with
            |[] -> failwith "ERROR creating decalre_stmt> - expected IDENT but recieved empty list"
            |head2::tail2 ->
                let lex2, token2 = head2
                match token2 with
                |IDENT ->
                    let term2 = Terminal(token2, lex2)
                    let tree, rest = assignMod tail2
                    match rest with
                    |[] -> failwith "ERROR creating decalre_stmt> - expected END_LINE but recieved empty list"
                    |head3::tail3 ->
                        let lex3, token3 = head3
                        match token3 with
                        |END_LINE->
                                Nonterminal("<declare_stmt>", [term1; term2; tree;Terminal(token3, lex3)]), tail3
                        |_-> failwith ("ERROR creating <declare_stmt> - expected END_LINE but recieved " + (sprintf "%A : " head3))
                |_-> failwith ("ERROR creating <declare_stmt> - expected IDENT but recieved " + (sprintf "%A : " head2))
        |_-> failwith ("ERROR creating <declare_stmt> - expected TYPE_IDENT but recieved " + (sprintf "%A : " head))





//******************STMT OR STMTS**************************
//***************************************************************
(*stmts> →<stmt><stmts> | EMPTY
<stmt> → <declare_stmt> | <assn_or_list> | <if_stmt> | <while_loop> | <print_stmt> | <scan_stmt*)


(*stmts> →<stmt><stmts> | EMPTY*)
let rec stmts lexList = 
    match lexList with
    |[] -> Nonterminal("<stmt>", [Terminal(EMPTY, "")]), lexList
    |head::tail->
        let stmtTree, rest = stmt lexList
        let stmtsTree, rest2 = stmts rest
        Nonterminal("<stmts>", [stmtTree; stmtsTree]), rest2



(*<while_loop> → WHILE <exp> <stmts> END_WHILE*)
and whileLoop lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <while_loop> - expected WHILE but recieved empty list"
    |head::tail ->
        let lex, token = head
        match token with
        |WHILE ->
            let expTree, rest = exp tail
            let stmtsTree, rest1 = stmts rest
            match rest1 with
            |[] -> failwith  "ERROR creating <while_loop> - expected END_LINE but recieved empty list"
            |head2::tail2 ->
                let lex2, token2 = head2
                match token2 with
                |END_WHILE ->
                    Nonterminal("<while_loop>", [Terminal(token2, lex2);expTree;stmtsTree]), rest1
                |_-> failwith ("ERROR creating <while_loop> - expected END_LINE but recieved " + (sprintf "%A : " head2))
        |_-> failwith ("ERROR creating <while_loop> - expected WHILE but recieved " + (sprintf "%A : " head))



(*<if_rest>->ENDIF|ELSE<stmts>ENDIF*)
and ifRest lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <if_rest> - expected END_IF or ELSE but recieved empty list"
    |head::tail -> 
        let lex, token = head
        match token with
        |END_IF -> Nonterminal("<if_rest>", [Terminal(token, lex)]), lexList
        |ELSE -> 
            let stmtsTree, rest = stmts tail
            match rest with
            |[] -> failwith "ERROR creating <if_rest> - expected END_IF but recieved empty list"
            |head2::tail2->
                let lex2, token2 = head2
                match token2 with
                |END_IF ->
                    Nonterminal("<if_rest>", [Terminal(token, lex); stmtsTree; Terminal(token2, lex2)]), tail2
                |_-> failwith ("ERROR creating <if_rest> - expected END_IF but recieved " + (sprintf "%A : " head2))
        |_-> failwith ("ERROR creating <if_rest> - expected END_IF or ELSE but recieved " + (sprintf "%A : " head))


(*<if_stmt> → IF <exp> <stmts> <if_rest>*)
and if_Stmt lexList = 
    match lexList with
    |[] -> failwith "ERROR creating <if_stmt> - expected IF but recieved empty list"
    |head::tail->
        let lex, token = head
        match token with
        |IF -> 
            let expTree, rest = exp tail
            let stmtsTree, rest1 = stmts rest
            let ifRestTree, rest2 = ifRest rest1
            Nonterminal("<if_rest>", [Terminal(token, lex); expTree; stmtsTree; ifRestTree]), rest2
        |_-> failwith ("ERROR creating <if_rest> - expected IF but recieved " + (sprintf "%A : " head))


(*<stmt> → <declare_stmt> | <assn_or_list> | <if_stmt> | <while_loop> | <print_stmt> | <scan_stmt*)
and stmt lexList =
    match lexList with
        |[] -> failwith "ERROR creating <stmt> - expected TYPE_IDENT or IDENT or IF or WHILE or  PRINT or SCAN but recieved empty list"
        |head::tail->
            let lex, token = head
            match token with
            |TYPE_IDENT ->
                let declareTree, rest = declareStmt lexList
                Nonterminal("<stmt>", [declareTree]), rest
            |IDENT ->
                let assnOrListTree, rest = assnOrList lexList
                Nonterminal("<stmt>", [assnOrListTree]), rest
            |IF ->
                let ifTree, rest = if_Stmt lexList
                Nonterminal("<stmt>", [ifTree]), rest
            |WHILE ->
                let whileTree, rest = whileLoop lexList
                Nonterminal("<stmt>", [whileTree]), rest
            |PRINT ->
                let printTree, rest = printStmt lexList
                Nonterminal("<stmt>", [printTree]), rest   
            |SCAN ->
                let scanTree, rest = scanStmt lexList
                Nonterminal("<stmt>", [scanTree]), rest  
            |_-> failwith ("ERROR creating <stmt> - expected TYPE_IDENT or IDENT or IF or WHILE or  PRINT or SCAN but recieved " + (sprintf "%A : " head))


//two valid Spoooky C lines of code to test

//let progString = "#list.app(:9:) :'{"
let progString = "prt(:#x, \"big\":):'{"

let ans = testLexer progString

printfn "%A" ans

let tree1, (rest:(string*Token) list) = stmts ans
(* "%A" rest *)

let treePrint = parseTreeToString tree1
printfn "%A" treePrint



