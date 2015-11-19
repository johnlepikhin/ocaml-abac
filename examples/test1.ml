
module Config =
struct
  type resource = [ `Resource of string | `Owner of string ] list
  type request = [ `Resource of string | `UserID of string | `IsAdmin ] list
end

module MyACL = Abac.Make (Config)

open MyACL

let resourceIsComment ~resource = if List.mem (`Resource "comments") resource then `Permit else `NotApplicable

let requestToComment ~request = if List.mem (`Resource "comments") request then `Permit else `NotApplicable

let requestIsAdmin ~request = if List.mem `IsAdmin request then `Permit else `NotApplicable

let isOwner ~resource ~request =
  let rec aux = function
    | [] -> `Failure
    | `Owner userid :: _ -> if List.mem (`UserID userid) request then `Permit else `Deny
    | _ :: tl -> aux tl
  in
  aux resource

(* permit access if resource is comment AND request is to comment AND owner of comment is user or this request
   						 is from admin *)
let acltree =
  denyUnlessAllPermit [
    targetResource resourceIsComment;
    targetRequest requestToComment;
    denyUnlessPermit [
      targetRequest requestIsAdmin;
      condition isOwner;
    ]
  ]


let () =
  (* permit *)
  let r1 = eval ~resource:[`Resource "comments"; `Owner "johndoe"] ~request:[`Resource "comments"; `UserID "johndoe"] acltree in
  string_of_result r1 |> print_endline;

  (* invalid owner *)
  let r2 = eval ~resource:[`Resource "comments"; `Owner "johndoe"] ~request:[`Resource "comments"; `UserID "h@ckEr"] acltree in
  string_of_result r2 |> print_endline;

  (* permit to admin *)
  let r3 = eval ~resource:[`Resource "comments"; `Owner "johndoe"] ~request:[`Resource "comments"; `IsAdmin] acltree in
  string_of_result r3 |> print_endline;

  (* invalid resource *)
  let r4 = eval ~resource:[`Resource "comments"; `Owner "johndoe"] ~request:[`Resource "users"; `UserID "johndoe"] acltree in
  string_of_result r4 |> print_endline;

  ()

