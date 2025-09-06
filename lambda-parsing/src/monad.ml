(* Basic monad functionalities *)

module type ParserInput = sig
  type stream
  val to_string : stream -> string
end

module MakeParser (Input : ParserInput) = struct
  type stream = Input.stream

  type 'a parser = stream -> ('a * stream) option

  let (>>=) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
    fun input ->
      match p input with
      | Some (result, rest) -> f result rest
      | None -> None

  let return (x : 'a) : 'a parser = 
    fun input -> Some (x, input)

  let (>>) (p : 'a parser) (q : 'b parser) : 'b parser =
    p >>= (fun _ -> q)

  let (<<) (p : 'a parser) (q : 'b parser) : 'a parser =
    p >>= fun result -> 
    q >> return result

  let (<|>) (p : 'a parser) (q : 'a parser) : 'a parser =
    fun input ->
      match p input with
      | Some _ as result -> result
      | None -> q input

  let rec many (p : 'a parser) : 'a list parser =
    (p >>= fun x ->
    many p >>= fun xs ->
    return (x :: xs)) <|> return []

  let some (p : 'a parser) : 'a list parser =
    p >>= fun x ->
    many p >>= fun xs ->
    return (x :: xs)
  
  let fail : 'a parser = fun _ -> None

  let debug (s : string) : unit parser =
    fun input ->
      Printf.printf "Debug: %s; Input: %s\n" s @@ Input.to_string input;
      Some ((), input)

  let fdebug (f : unit -> unit) : unit parser =
    fun input ->
      Printf.printf "Debug: "; f ();
      Some ((), input)
end