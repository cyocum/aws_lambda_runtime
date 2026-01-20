module Make(Handler : Handler.S) =
struct
  type t = {
    user_agent_header : string;
    handler_error_endpoint : string;
    next_invocation_endpoint : string;
    handler_result_endpoint : string;
  }

  type invocation_response = {
    payload : string;
    content_type : string;
    success : bool;
  }

  exception AWSLambdaRuntimeApiMissing

  let build_lambda_runtime () =
    let endpoint_url =
      match Base.Sys.getenv "AWS_LAMBDA_RUNTIME_API" with
      | Some api_url -> Base.Printf.sprintf "http://%s/2018-06-01/runtime" api_url
      | None ->
        raise AWSLambdaRuntimeApiMissing
    in
    {
      user_agent_header = "AWS_Lambda_OCaml/0.1";
      handler_error_endpoint = Base.Printf.sprintf "%s/%s" endpoint_url "init/error";
      next_invocation_endpoint = Base.Printf.sprintf "%s/%s" endpoint_url "invocation/next";
      handler_result_endpoint = Base.Printf.sprintf "%s/%s" endpoint_url "invocation/";
    }

  let request_next_invocation rt =
    let next_invocation_payload = Ezcurl.get
        ~headers:[("User-Agent", rt.user_agent_header)]
        ~url:rt.next_invocation_endpoint () in
    match next_invocation_payload with
    | Ok c -> c
    | Error (_, s) -> failwith s

  let report_invocation_outcome rt handler_output request_id =
    let url =
      if Handler.is_success handler_output then
        rt.handler_result_endpoint ^ request_id ^ "/reponse"
      else rt.handler_error_endpoint ^ request_id ^ "/error"
    in
    let payload = Handler.string_of_output handler_output in
    let headers =
      [
        ("content-type:", Handler.context_type);
        ( "content-length:",
          string_of_int (String.length payload) );
        ("User-Agent:", rt.user_agent_header);
      ]
    in
    let post_outcome =
      Ezcurl.post ~headers ~url ~content:(`String payload)
        ~params:[] ()
    in
    match post_outcome with Ok _ -> true | Error (_, _) -> false

  let invoke_handler () =
    let rt = build_lambda_runtime () in
    let next_invocation = request_next_invocation rt in
    let context = Context.create_context next_invocation in
    let handler_input = Handler.input_of_string next_invocation.Ezcurl.body in
    let handler_output = Handler.handle_request context handler_input in
    ignore(report_invocation_outcome rt handler_output context.Context.request_id)
end
