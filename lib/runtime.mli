module type S =
sig
  val invoke_handler : unit -> unit
end

module Make(_ : Handler.S) : S
