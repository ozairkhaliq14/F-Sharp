﻿//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   << Ozair Khaliq >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation:
    //
    // NOTE: identifier, int_literal and str_literal
    // are special cases because they are followed by
    // the name or literal value. In these cases exact
    // matching will not work, so we match the start 
    // of the token in these cases.
    //
    let next_token = List.head tokens

    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      //
      // next_token starts with identifier, so we have a match:
      //
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
      //
      // next_token starts with str_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      //
      // next_token starts with real_literal, so we have a match:
      //
      List.tail tokens
    else
      List.tail tokens


  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      T2
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      T2
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let T2 = matchToken "identifier" tokens
      T2
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      T2
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      T2
    else
      let T2 = matchToken "real_literal" tokens
      T2


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens
    //
    // if next_token = "+"  ||
    //    next_token = "-"  ||
    //    next_token = "*"  ||
    //    next_token = "/"  ||
    //    next_token = "^"  ||
    //    next_token = "<"  ||
    //    next_token = "<=" ||
    //    next_token = ">"  ||
    //    next_token = ">=" ||
    //    next_token = "==" ||
    //    next_token = "!=" then
    //   //
    let T2 = matchToken next_token tokens
    T2


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let T2 = expr_value tokens
    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T3 = expr_op T2
      let T4 = expr_value T3
      T4
    else
      // just expr_value, that's it
      T2


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2

  let rec private checkForDuplicate symboltable identifier = 
    match symboltable with
    | [] -> ()
    | (id, lit)::tail when id = identifier -> failwith("redefinition of variable '" + identifier + "'")
    | (id, lit)::tail -> checkForDuplicate tail identifier

  //
  // <vardecl> -> int identifier ;
  //
  let rec private vardecl tokens symboltable = 
    let hd = List.head tokens
    if hd = "int" then
      let T2 = matchToken "int" tokens
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      let T5 = (List.head T2).[11..]
      let T6 = "int"
      checkForDuplicate symboltable T5
      (T4, (T5, T6)::symboltable)
    else
      let T2 = matchToken "real" tokens
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      let T5 = (List.head T2).[11..]
      let T6 = "real"
      checkForDuplicate symboltable T5
      (T4, (T5, T6)::symboltable)

  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let T2 = expr_value tokens
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens = 
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3
    let T5 = matchToken ";" T4
    T5


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symboltable = 
    let next_token = List.head tokens
    //
    // use the next token to determine which rule
    // to call; if none match then it's a syntax
    // error:
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symboltable)
    elif next_token = "int" then
      let (T2, symboltable) = vardecl tokens symboltable
      (T2, symboltable)
    elif next_token = "cin" then
      let T2 = input tokens
      (T2, symboltable)
    elif next_token = "cout" then
      let T2 = output tokens
      (T2, symboltable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens
      (T2, symboltable)
    elif next_token = "real" then
      let (T2, symboltable) = vardecl tokens symboltable
      (T2, symboltable)
    else
      let (T2, symboltable) = ifstmt tokens symboltable
      (T2, symboltable)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symboltable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let (T6, T7) = then_part T5 symboltable
    let (T8, T9) = else_part T6 T7
    (T8, T9)
  //
  // <condition> -> <expr>
  //
  and private condition tokens = 
    let T2 = expr tokens
    T2
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symboltable = 
    let (T2, symboltable) = stmt tokens symboltable
    (T2, symboltable)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symboltable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, table) = stmt T2 symboltable
      (T3, table)
    else
      // EMPTY, do nothing but return tokens back
      (tokens, symboltable)


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens symboltable = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token = "real" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      //
      let (T2, symboltable) = stmt tokens symboltable
      let (T3, symboltable) = morestmts T2 symboltable
      (T3, symboltable)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, symboltable)


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symboltable = 
    let (T2, symboltable) = stmt tokens symboltable
    let (T3, symboltable) = morestmts T2 symboltable
    (T3, symboltable)


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens = 
    
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, T8) = stmts T6 []
    let T9 = matchToken "}" T7
    let T10 = matchToken "$" T9  // $ => EOF, there should be no more tokens
    (T10, T8)


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let build_symboltable tokens = 
    try
      let (result, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
