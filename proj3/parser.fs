//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// <<Ozair Khaliq>>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next_token token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  let rec private emptyFunc tokens = 
    matchToken ";" tokens

  let private vardeclFunc tokens = 
    let T2 = matchToken "int" tokens
    let next_token = List.head T2
    if not(next_token.StartsWith("identifier")) then
      failwith("expecting identifier, but found " + next_token)
    else
      let T3 = List.tail T2
      matchToken ";" T3
      

  let private inputFunc tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let next_token = List.head T3

    if next_token.StartsWith("identifier") then
      let T4 = List.tail T3
      matchToken ";" T4
      
    else
      failwith("expecting identifier, but found " + next_token)

  let private exprValue tokens = 
    let next_token:string = List.head tokens
    if (next_token.StartsWith("identifier") || next_token.StartsWith("int_literal") || next_token.StartsWith("str_literal") || next_token = "true" || next_token = "false") then
      List.tail tokens
    else
      failwith("expecting identifier or literal, but found " + next_token)

  let private exprOp tokens = 
    let next_token = List.head tokens
    let opList = ["+";"-";"*";"/";"^";"<";">";"<=";">=";"==";"!=";]
    if next_token = "+" || next_token = "-" || next_token = "*" || next_token = "/" || next_token = "^" || next_token = "<" || next_token = ">" || next_token = "<=" || next_token = ">=" || next_token = "==" || next_token = "!=" then
      List.tail tokens
    elif not(next_token = ";") && not(next_token = ")") then
      failwith("expecting ), but found " + next_token)
    else
      failwith("expecting ;, but found " + next_token)

  let private expr tokens = 
    let T2 = exprValue tokens
    if(List.head T2 = ";" || List.head T2 = ")") then
      T2
    else
      let T3 = exprOp T2
      exprValue T3
      

  let private condition tokens = 
    expr tokens

  let private assignment tokens = 
    let next_token:string = List.head tokens
    if next_token.StartsWith("identifier") then
      let T2 = List.tail tokens
      let T3 = matchToken "=" T2
      let T4 = expr T3
      matchToken ";" T4
      
    else
      failwith("expecting identifier, but found " + next_token)

  let private outputValue tokens = 
    if List.head tokens = "endl" then
      matchToken "endl" tokens
    else
      exprValue tokens

  let private outputFunc tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = outputValue T3
    matchToken ";" T4
    
    
  let rec private stmtFunc tokens =
    let assign = List.head tokens
    if List.head tokens = "if" then
      ifstmt tokens
    elif List.head tokens = ";" then
      emptyFunc tokens
    elif List.head tokens = "int" then
      vardeclFunc tokens
    elif List.head tokens = "cin" then
      inputFunc tokens
     elif List.head tokens = "cout" then
       outputFunc tokens
     elif assign.StartsWith("identifier") then 
       assignment tokens
    else
      failwith("expecting statement, but found " + List.head tokens)
 
  and private then_part tokens =
    stmtFunc tokens

  and private else_part tokens =
    if List.head tokens = "else" then
      let T2 = matchToken "else" tokens
      stmtFunc T2
    else
      tokens

  and private ifstmt tokens =
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let T6 = then_part T5
    else_part T6
    

  let rec private morestmts tokens =
    if List.head tokens = "}" then
      tokens
    else
      let T2 = stmtFunc tokens
      morestmts T2
      

  let rec private stmts tokens = 
    let T2 = stmtFunc tokens
    morestmts T2
    
  
  

  let private simpleC tokens =
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    matchToken "$" T8 // $ => EOF, there should be no more tokens
    


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
