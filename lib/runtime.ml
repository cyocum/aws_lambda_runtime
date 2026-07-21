module type S =
sig
  val invoke_handler : unit -> unit
end

module Make(Handler : Handler.S) : S =
struct
  type t = {
    user_agent_header : string;
    next_invocation_endpoint : string;
    handler_result_endpoint : string;
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

  let report_invocation_outcome rt outcome request_id =
    let path, payload, content_type =
      match outcome with
      | Ok handler_output ->
        let endpoint = 
          if Handler.is_success handler_output then
            "/response"
          else
            "/error"
        in
        (endpoint, Handler.string_of_output handler_output, Handler.content_type)
      | Error exn_msg ->
        let json_error =
          Yojson.Basic.to_string
            (`Assoc [
                ("errorMessage", `String exn_msg);
                ("errorType", `String "UnhandledException");
              ])
        in
        ("/error", json_error, "application/json")
    in
    let url = rt.handler_result_endpoint ^ request_id ^ path in
    let headers =
      [
        ("content-type", content_type);
        ("content-length", string_of_int (Base.String.length payload));
        ("User-Agent", rt.user_agent_header);
      ]
    in
    let post_outcome =
      Ezcurl.post ~headers ~url ~content:(`String payload) ~params:[] ()
    in
    match post_outcome with
    | Ok _ -> true
    | Error _ -> false


  let invoke_handler () =
    let rt = build_lambda_runtime () in
    while true do 
      let next_invocation = request_next_invocation rt in
      let req_id =
        match Base.List.Assoc.find
                next_invocation.Ezcurl.headers
                "lambda-runtime-aws-request-id"
                ~equal:Base.String.Caseless.equal  with
        | Some id -> id
        | None -> "unknown-request-id"
      in

      let outcome =
        try 
          let context = Context.create_context next_invocation in
          let handler_input = Handler.input_of_string next_invocation.Ezcurl.body in
          let handler_output = Handler.handle_request context handler_input in
          Ok handler_output
        with
        | exn ->
          Error (Base.Exn.to_string exn)
      in
          ignore(report_invocation_outcome rt outcome req_id)
    done
end 
