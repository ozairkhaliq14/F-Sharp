//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
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

module checker =
  
  let private matchToken expected_token (tokens: string list) =
    List.tail tokens

  let rec contains(x,y) L =
    match L with 
    | [] -> false
    | (name, hd)::tail when name = x -> true
    | head::tail -> contains (x,y) tail

  let rec contains_type (x,y) L = 
    match L with 
    | [] -> "issue" 
    | (name,hd)::tail when name = x -> hd            
    | head::tail -> contains_type (x,y) tail

  let rec private expr_value tokens symboltable =
    let next_token = List.head tokens

    if next_token = "true" then
      let T2 = matchToken "true" tokens
      (T2, "bool")
    elif next_token = "false" then
      let T2 = matchToken "false" tokens
      (T2, "bool")
    elif next_token.StartsWith("identifier") then
      let var_name = next_token.Substring(11)
      let T2 = matchToken "identifier" tokens
      if contains (var_name,"type") symboltable = false then  
        failwith("variable '" + var_name + "' undefined")
      else
        let the_type = contains_type (var_name,"type") symboltable
        (T2,the_type)
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      (T2,"int")
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      (T2,"str")
    elif next_token.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      (T2,"real")
    else
      (tokens,"not")

  // // if expected_token = "identifier" && next_token.StartsWith("identifier") then
  // //   //
  // //   // next_token starts with identifier, so we have a match:
  // //   //
  // //   List.tail tokens
  // // elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
  // //   //
  // //   // next_token starts with int_literal, so we have a match:
  // //   //
  // //   List.tail tokens
  // // elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
  // //   //
  // //   // next_token starts with str_literal, so we have a match:
  // //   //
  // //   List.tail tokens
  // // elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
  // //   //
  // //   // next_token starts with real_literal, so we have a match:
  // //   //
  // //   List.tail tokens
  // // elif expected_token = next_token then  
  // //   List.tail tokens
  // // else
  // //   failwith ("expecting " + expected_token + ", but found " + next_token)

  let rec private expr_op tokens = 
    let next_token = List.head tokens
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
      let T2 = matchToken next_token tokens
      T2
    else
      // error
      tokens

  let rec private expr tokens symboltable = 
    let (T2, left_type) = expr_value tokens symboltable
    let next_token  = List.head T2

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
      let T3 = expr_op T2
      let (T4, right_type) = expr_value T3 symboltable

      if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  then
        if (left_type = "int" || left_type = "real") && (left_type = right_type) then
          (T4, left_type)
        else
          failwith("operator " + next_token + " must involve 'int' or 'real'")
      else 
        if next_token = "<"  ||
           next_token = "<=" ||
           next_token = ">"  ||
           next_token = ">=" ||
           next_token = "==" ||
           next_token = "!=" then
          if (next_token = "==") && (left_type = "real") && (right_type = "real") then 
             printfn "warning: comparing real numbers with == may never be true"
          if  (left_type = right_type)  then 
           (T4,"bool")
          else
            failwith("type mismatch '" + left_type + "' "+ next_token + " '"+ right_type +  "'")
        else 
          (T4,"Issue,wrong type")
    else
      (T2, left_type)

   //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2

  //
  // <vardecl> -> int identifier ;
  //
  let rec private vardecl tokens symboltable = 
    let hd:string = List.head tokens
    if hd = "int" then
      let T2 = matchToken "int" tokens
      let T3 = List.head T2
      let T4 = T3.Substring(11)
      let T5 = (T4,"int")
      let T6 = matchToken "identifier" T2
      let T7 = matchToken ";" T6
      (T7, T5::symboltable)
    else
      let T2 = matchToken "real" tokens
      let T3 = List.head T2
      let T4 = T3.Substring(11)
      let T5 = (T4,"real")
      let T6 = matchToken "identifier" T2
      let T7 = matchToken ";" T6
      (T7, T5::symboltable)

  let rec private input tokens table = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = List.head T3
    if contains(T4.Substring(11), "type") table = false then
      failwith("variable '" + T4.Substring(11) + "' undefined")
    let T5 = matchToken "identifier" T3
    let T6 = matchToken ";" T5
    T6

  let rec private output_value tokens symboltable = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let (T2, hd) = expr_value tokens symboltable
      T2

  // //
  // // <expr-value> -> identifier
  // //               | int_literal
  // //               | str_literal
  // //               | true
  // //               | false
  // //
  // let rec private expr_value tokens =
  //   let next_token = List.head tokens
  //   //
  //   if next_token = "false" then
  //     let T2 = matchToken "false" tokens
  //     T2
  //   elif next_token = "true" then
  //     let T2 = matchToken "true" tokens
  //     T2
  //   //
  //   // the others are trickier since we have to look 
  //   // at the start of the string for a match:
  //   //
  //   elif next_token.StartsWith("identifier") then
  //     let T2 = matchToken "identifier" tokens
  //     T2
  //   elif next_token.StartsWith("int_literal") then
  //     let T2 = matchToken "int_literal" tokens
  //     T2
  //   elif next_token.StartsWith("str_literal") then
  //     let T2 = matchToken "str_literal" tokens
  //     T2
  //   elif next_token.StartsWith("real_literal") then
  //     let T2 = matchToken "real_literal" tokens
  //     T2
  //   else
  //     failwith ("expecting identifier or literal, but found " + next_token)


  // //
  // // <expr-op> -> +
  // //            | -
  // //            | *
  // //            | /
  // //            | ^
  // //            | <
  // //            | <=
  // //            | >
  // //            | >=
  // //            | ==
  // //            | !=
  // //
  


  // //
  // // <expr> -> <expr-value> <expr-op> <expr-value>
  // //         | <expr-value>
  // //
  // let rec private expr tokens = 
  //   //
  //   // first we have to match expr-value, since both
  //   // rules start with this:
  //   //
  //   let T2 = expr_value tokens
  //   //
  //   // now let's see if there's more to the expression:
  //   //
  //   let next_token = List.head T2
  //   //
  //   if next_token = "+"  ||
  //      next_token = "-"  ||
  //      next_token = "*"  ||
  //      next_token = "/"  ||
  //      next_token = "^"  ||
  //      next_token = "<"  ||
  //      next_token = "<=" ||
  //      next_token = ">"  ||
  //      next_token = ">=" ||
  //      next_token = "==" ||
  //      next_token = "!=" then
  //     //
  //     let T3 = expr_op T2
  //     let T4 = expr_value T3
  //     T4
  //   else
  //     // just expr_value, that's it
  //     T2

  let rec private output tokens symboltable= 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 symboltable
    let T5 = matchToken ";" T4
    T5

  let rec private assignment tokens symboltable= 
    let next_token:string = List.head tokens
    let T2 = next_token.Substring(11)
    let T3 = contains_type (T2, "type") symboltable
    if T3 = "issue" then
      failwith("variable '" + T2 + "' undefined")
    let T4 = matchToken "identifier" tokens
    let T5 = matchToken "=" T4
    let (T6,T7) = expr T5 symboltable
    let T8 = matchToken ";" T6
    if(T3 = T7) || (T3 = "real") && (T7 = "int") then
      let r = T5
      r |> ignore
    else
      failwith("cannot assign '" + T7 + "' to variable of type '" + T3 + "'")
    if contains(T2,"type") symboltable = false then
      failwith("variable '" + T2 + "' undefined")
    else
    T8

  let rec private stmt tokens symboltable= 
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
      let (T2, T3) = vardecl tokens symboltable
      (T2, T3)
    elif next_token = "cin" then
      let T2 = input tokens symboltable
      (T2, symboltable)
    elif next_token = "cout" then
      let T2 = output tokens symboltable
      (T2, symboltable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symboltable
      (T2, symboltable)
    elif next_token = "if" then
      let (T2, T3) = ifstmt tokens symboltable
      (T2, T3)
    elif next_token = "real" then
      let (T2, T3) = vardecl tokens symboltable
      (T2, T3)
    else
      (tokens, symboltable)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symboltable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symboltable
    let T5 = matchToken ")" T4
    let (T6, T7) = then_part T5 symboltable
    let (T8, T9) = else_part T6 T7
    (T8, T9)
  //
  // <condition> -> <expr>
  //
  and private condition tokens symboltable = 
    let (T2, hd) = expr tokens symboltable
    if hd <> "bool" then
      failwith("if condition must be 'bool', but found '" + hd + "'")
    else
    T2
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symboltable= 
    let (T2, T3) = stmt tokens symboltable
    (T2, T3)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symboltable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, T4) = stmt T2 symboltable
      (T3,T4)
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
      let (T2, T3) = stmt tokens symboltable
      let (T4, T5) = morestmts T2 T3
      (T4, T5)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, symboltable)


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symboltable = 
    let (T2, T3) = stmt tokens symboltable
    let (T4, T5) = morestmts T2 T3
    (T4, T5)


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, T8) = stmts T6 symboltable
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
  let typecheck tokens symboltable = 
    try
      let result = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message
    





  


 


  


  // //
  // // <input> -> cin >> identifier ;
  // //
  


  // //
  // // <output-value> -> <expr-value>
  // //                 | endl
  // //
  


  // //
  // // <output> -> cout << <output-value> ;
  // //
  


  // //
  // // <assignment> -> identifier = <expr> ;
  // //
  


  // //
  // // <stmt> -> <empty>
  // //         | <vardecl>
  // //         | <input>
  // //         | <output>
  // //         | <assignment>
  // //         | <ifstmt>
  // //
  

