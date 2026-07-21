type t = {
  request_id : string;
  xray_trace_id : string;
  client_context : string option;
  cognito_identity : string option;
  function_arn : string;
  deadline : int64;
}

let create_context context_payload =
  let headers_hash =
    let normalised_headers =
      Base.List.map context_payload.Ezcurl_core.headers ~f:(fun (k, v) ->
          (Base.String.lowercase k, v))
    in
      match Base.Hashtbl.of_alist
              (module Base.String)
              normalised_headers with
      | `Ok h -> h
      | `Duplicate_key s ->
        failwith (Base.Printf.sprintf "Duplicate key in header: %s" s) in
    {
      request_id =
        Base.Hashtbl.find_exn headers_hash "lambda-runtime-aws-request-id";
      xray_trace_id = Base.Hashtbl.find_exn headers_hash "lambda-runtime-trace-id";
      client_context =
        Base.Hashtbl.find headers_hash "lambda-runtime-client-context";
      cognito_identity =
        Base.Hashtbl.find headers_hash "lambda-runtime-cognito-identity";
      function_arn =
        Base.Hashtbl.find_exn headers_hash "lambda-runtime-invoked-function-arn";
      deadline =
        Base.Int64.of_string
          (Base.Hashtbl.find_exn headers_hash
             "lambda-runtime-deadline-ms");
    }
