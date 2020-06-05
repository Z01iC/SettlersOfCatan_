(** The type [command] represents a player command. *)
type command

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    add    settlement   "] is [AddSettle]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "add" nor "inventory",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "add" and there is an empty object phrase.*)
val parse : string -> command

(**[to_string command] is the command but in a string form *)
val to_data : command -> string * int * string * int * string