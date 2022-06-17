(*
Note: This was heavily based upon a tutorial made by Micheal Gilliland available here: https://www.youtube.com/watch?v=hC9U59a1el0&t=628s
along with matt's source code available here: https://gist.github.com/mjgpy3/547db15b0a0b67e8ad3f49421e07b25d.


*)

open System

type Token = //defines the types of tokens for the tokenizer 
   |LParen
   |Rparen
   |Lambda
   |Dot
   |Variable of char


let alphabet = //defines the alphabet for the tokenizer
    List.ofSeq "abcdefghijklmnopqrstuvwxyz"

let rec tokenize (text:char list) = //function to tokenize the expression
    match text with 
        |[] -> []
        |'('::rest -> LParen::tokenize rest
        |')'::rest -> Rparen::tokenize rest
        |'.'::rest -> Dot::tokenize rest
        |'L'::rest -> Lambda::tokenize rest
        |c::rest ->
        (if List.contains c alphabet
        then [Variable c]
        else []) @ tokenize rest //@ is a shortcut in F for list.append

type Env = (char*Term) list 

type Term =  //defines the different expressions of lambda calculus for parsing
    |TermVariable of char
    |TermFunc of char*Term
    |TermClosure of  
    |TermApplication of Term*Term

let rec parseSingle (tokens: Token list): (Term* Token list) = 
    match tokens with
    | (Variable name::rest) -> TermVariable name, rest
    | (Lambda::Variable arg::Dot::funcBody)  -> 
        
        let body, rest = parseSingle funcBody
        
        TermFunc(arg, body), rest
    
    | LParen::code ->
        let fn, afterFirst = parseSingle code
        let value, afterValue = parseSingle afterFirst

        match afterValue with 
            |   Rparen::rest  -> TermApplication(fn, value), rest
            |   _ -> 
                failwith "Error: ) Expected"
    |_ -> 
        failwith "Bad Parse"


let Parse (tokens:Token list) =
    fst <| parseSingle tokens
    
let eavlInEnv (env: Env) (term: Term): Term = 
    match term with
        |TermVariable name ->
            match List.tryFind (fun (aName, term) -> aName = name)env with
            | Some ( _ ,term) -> term
            | None -> failwith "Error: could not find term by name"
        
        |TermFunc(arg, body) ->
            

        |TermApplication
let eval(term:Term): Term =
    evalInEnv [] term

