type ('a, 'b) t = {
  user_agent_header : string;
  handler_error_endpoint : string;
  next_invocation_endpoint : string;
  handler_result_endpoint : string;
  handler : 'a -> 'b
}

type invocation_response = {
  payload : string;
  content_type : string;
  success : bool;
}

type invocation_request = {
  payload : string;
  request_id : string;
  xray_trace_id : string;
  client_context : string;
  cognito_identity : string;
  function_arn : string;
  deadline : int64;
}

exception AWSLambdaRuntimeApiMissing

let build_lambda_runtime handler =
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
    handler;
  }

let request_next_invocation rt =
  let next_invocation_payload = Ezcurl.get
      ~headers:[("User-Agent", rt.user_agent_header)]
      ~url:rt.next_invocation_endpoint () in
  match next_invocation_payload with
  | Ok c -> c
  | Error (_, s) -> failwith s

let create_invocation_request invocation_payload =
  let headers_hash =
    match Base.Hashtbl.of_alist
            (module Base.String)
            invocation_payload.Ezcurl_core.headers with
    | `Ok h -> h
    | `Duplicate_key s ->
      failwith (Base.Printf.sprintf "Duplicate key in header: %s" s) in
  {
    request_id =
      Base.Hashtbl.find_exn headers_hash "lambda-runtime-aws-request-id";
    xray_trace_id = Base.Hashtbl.find_exn headers_hash "lambda-runtime-trace-id";
    client_context =
      Base.Hashtbl.find_exn headers_hash "lambda-runtime-client-context";
    cognito_identity =
      Base.Hashtbl.find_exn headers_hash "lambda-runtime-cognito-identity";
    function_arn =
      Base.Hashtbl.find_exn headers_hash "lambda-runtime-invoked-function-arn";
    deadline =
      Base.Int64.of_string
        (Base.Hashtbl.find_exn headers_hash
           "lambda-runtime-invoked-function-arn");
    payload = invocation_payload.Ezcurl.body;
  }

let report_invocation_outcome rt invocation_response request_id =
  let url =
    if invocation_response.success then
      rt.handler_result_endpoint ^ request_id ^ "/reponse"
    else rt.handler_error_endpoint ^ request_id ^ "/error"
  in
  let headers =
    [
      ("content-type:", invocation_response.content_type);
      ( "content-length:",
        string_of_int (String.length invocation_response.payload) );
      ("User-Agent:", rt.user_agent_header);
    ]
  in
  let post_outcome =
    Ezcurl.post ~headers ~url ~content:(`String invocation_response.payload)
      ~params:[] ()
  in
  match post_outcome with Ok _ -> true | Error (_, _) -> false
  

let invoke_handler handler =
  let rt = build_lambda_runtime handler in
  let next_invocation = request_next_invocation rt in
  let invocation_request =  create_invocation_request next_invocation in
  let invoke_response = handler invocation_request in
  ignore(report_invocation_outcome rt invoke_response invocation_request.request_id)
  
