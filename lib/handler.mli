module type S =
sig
  type input
  type output
  val handle_request : Context.t -> input -> output
  val content_type : string
  val input_of_string : string -> input
  val string_of_output : output -> string
  val is_success : output -> bool
end
