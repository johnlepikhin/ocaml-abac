
type permit = [ `Permit ]

type deny = [ `Deny ]

type permitDeny = [ permit | deny ]

type notApplicable = [ `NotApplicable ]

type failure = [ `Failure ]

type result = [ permit | deny | notApplicable | failure ]

type ('request, 'resource, 'result) logic =
  resource : 'resource -> request : 'request -> 'result

type ('resource, 'result) targetResource =
  resource : 'resource -> 'result
    constraint 'result = [< permit | notApplicable ]

type ('request, 'result) targetRequest =
  request : 'request -> 'result
    constraint 'result = [< permit | notApplicable ]

type ('request, 'resource, 'result) target =
  ('request, 'resource, 'result) logic
    constraint 'result = [< permit | notApplicable ]

type ('request, 'resource, 'result) condition =
  ('request, 'resource, 'result) logic
    constraint 'result = [< result ]

let string_of_result = function
  | `Permit -> "Permit"
  | `Deny -> "Deny"
  | `NotApplicable -> "NotApplicable"
  | `Failure -> "Failure"

module Combinator
  : sig
    type ('request, 'resource, 'inresult, 'outresult) plus =
      ('request, 'resource, 'inresult) t
      -> ('request, 'resource, 'inresult) t list
      -> ('request, 'resource, 'outresult) t

    and ('request, 'resource, 'inresult, 'outresult) star =
      ('request, 'resource, 'inresult) t list
      -> ('request, 'resource, 'outresult) t

    and ('request, 'resource, 'inresult, 'outresult) one =
      ('request, 'resource, 'inresult) t
      -> ('request, 'resource, 'outresult) t

    and ('request, 'resource, 'inresult, 'outresult) two =
      ('request, 'resource, 'inresult) t
      -> ('request, 'resource, 'inresult) t
      -> ('request, 'resource, 'outresult) t

    and ('request, 'resource, 'result) t

    val targetResource: ('res, 'result) targetResource -> ('req, 'res, [> permit | notApplicable ]) t
    val targetRequest: ('req, 'result) targetRequest -> ('req, 'res, [> permit | notApplicable ]) t
    val target: ('req, 'res, 'result) target -> ('req, 'res, [> permit | notApplicable ]) t
        
    val condition: ('req, 'res, 'result) condition -> ('req, 'res, [> result ]) t

    val and': ('req, 'res, permitDeny, [> permitDeny]) plus
        
    val or': ('req, 'res, permitDeny, [> permitDeny]) plus
        
    val xor': ('req, 'res, permitDeny, [> permitDeny]) plus
        
    val not': ('req, 'res, permitDeny, [> permitDeny]) one
        
    val denyUnlessAllPermit: ('req, 'res, result, [> permitDeny]) star
        
    val denyUnlessPermit: ('req, 'res, result, [> permitDeny]) star

    val asPermitDeny: ('req, 'res, result, [> permitDeny]) one

    val ( &&& ) : ('req, 'res, permitDeny, [> permitDeny]) two
    val ( ||| ) : ('req, 'res, permitDeny, [> permitDeny]) two

    val eval:
        resource : 'resource
      ->  request : 'request
      -> ('request, 'resource, [< result]) t
      -> result

    val evalRequest:
        resource : 'resource
      ->  request : 'request
      -> ('request, 'resource, [< result]) t
      -> result

    val evalResource:
        resource : 'resource
      ->  request : 'request
      -> ('request, 'resource, [< result]) t
      -> result

  end
= struct

  type ('request, 'resource, 'inresult, 'outresult) plus =
    ('request, 'resource, 'inresult) t
    -> ('request, 'resource, 'inresult) t list
    -> ('request, 'resource, 'outresult) t

  and ('request, 'resource, 'inresult, 'outresult) star =
    ('request, 'resource, 'inresult) t list
    -> ('request, 'resource, 'outresult) t

  and ('request, 'resource, 'inresult, 'outresult) one =
    ('request, 'resource, 'inresult) t
    -> ('request, 'resource, 'outresult) t

  and ('request, 'resource, 'inresult, 'outresult) two =
    ('request, 'resource, 'inresult) t
    -> ('request, 'resource, 'inresult) t
    -> ('request, 'resource, 'outresult) t

  and ('request, 'resource, 'result) element =
    | TargetResource: (('resource, 'result) targetResource) ->
      ('request, 'resource, [> permit | notApplicable]) element

    | TargetRequest: (('request, 'result) targetRequest) ->
      ('request, 'resource, [> permit | notApplicable]) element

    | Target: ('request, 'resource, 'result) target ->
      ('request, 'resource, [> permit | notApplicable]) element

    | Condition: ('request, 'resource, 'result) condition ->
      ('request, 'resource, [> result ]) element

  and ('request, 'resource, 'result) t =
    | Element of ('request, 'resource, 'result) element
    | And of ('request, 'resource, permitDeny) t
             * ('request, 'resource, permitDeny) t list
    | Or of ('request, 'resource, permitDeny) t
             * ('request, 'resource, permitDeny) t list
    | Xor of ('request, 'resource, permitDeny) t
             * ('request, 'resource, permitDeny) t list
    | Not of ('request, 'resource, permitDeny) t
    | DenyUnlessAllPermit of ('request, 'resource, result) t list
    | DenyUnlessPermit of ('request, 'resource, result) t list
    | AsPermitDeny of ('request, 'resource, result) t

  let element elt = Element elt

  let targetResource v = element (TargetResource v)
      
  let targetRequest v = element (TargetRequest v)
      
  let target v = element (Target v)
      
  let condition v = element (Condition v)

  let and' hd tl = And (hd, tl)
  
  let or' hd tl = Or (hd, tl)
  
  let xor' hd tl = Xor (hd, tl)
      
  let not' cmb = Not cmb

  let denyUnlessAllPermit lst = DenyUnlessAllPermit lst
      
  let denyUnlessPermit lst = DenyUnlessPermit lst

  let asPermitDeny t = AsPermitDeny t

  let ( &&& ) a b = and' a [b]

  let ( ||| ) a b = or' a [b]

  let rec eval_aux
      ~checkResource
      ~checkRequest
      ~(resource : 'resource)
      ~(request : 'request)
      (cmb : ('request, 'resource, [< result]) t)
    : result
    =
    let cmb = Obj.magic cmb in
    match cmb with
    | Element elt ->
      let r : result =
        match elt with
        | Target fn ->
          if checkResource && checkRequest then
            Obj.magic (fn ~resource ~request)
          else
            `Permit
        | TargetResource fn ->
          if checkResource then
            Obj.magic (fn ~resource)
          else
            `Permit
        | TargetRequest fn ->
          if checkRequest then
            Obj.magic (fn ~request)
          else
            `Permit
        | Condition fn ->
          Obj.magic (fn ~resource ~request)
      in
      r

    | And (hd, tl) ->
      let folder prev incmb =
        if prev = `Permit then
          eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb)
        else `Deny
      in
      List.fold_left folder `Permit (hd :: tl)

    | Or (hd, tl) ->
      let folder prev incmb =
        if prev = `Deny then
          eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb)
        else `Permit
      in
      List.fold_left folder `Deny (hd :: tl)

    | Xor (hd, tl) ->
      let folder prev incmb =
        if prev = `Deny then
          eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb)
        else `Permit
      in
      if List.fold_left folder `Deny (hd :: tl) = `Deny then
        `Permit
      else
        `Deny

    | Not incmb ->
      if eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb) = `Permit then
        `Deny
      else
        `Permit

    | DenyUnlessAllPermit lst ->
      let folder prev incmb =
        Printf.printf "folder next, prev=%s\n" (match prev with Some prev -> string_of_result prev | None -> "NONE");
        match prev with
        | None | Some `Permit ->
          Some (eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb))
        | _ -> Some `Deny
      in
      let r = List.fold_left folder None lst in
        Printf.printf "DenyUnlessAllPermit=%s\n" (match r with Some prev -> string_of_result prev | None -> "NONE");
      (
        match r with
        | Some `Permit -> `Permit
        | _ -> `Deny
      )

    | DenyUnlessPermit lst ->
      let folder prev incmb =
        if prev = `Permit then
          `Permit
        else
          eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb)
      in
      List.fold_left folder `Deny lst

    | AsPermitDeny incmb ->
      match eval_aux ~checkRequest ~checkResource ~resource ~request (Obj.magic incmb) with
      | `Permit -> `Permit
      | _ -> `Deny

  let eval ~resource ~request cmb : result
    = eval_aux ~checkRequest:true ~checkResource:true ~resource ~request cmb

  let evalResource ~resource ~request cmb : result
    = eval_aux ~checkRequest:false ~checkResource:true ~resource ~request cmb

  let evalRequest ~resource ~request cmb : result
    = eval_aux ~checkRequest:true ~checkResource:false ~resource ~request cmb

end

  
let sameResource : ('request, 'resource, 'result) Combinator.t =
  let fn ~resource ~request =
    print_endline "sameResource";
    let rec aux = function
      | [] ->
        print_endline "sameResource 1";
        `NotApplicable
      | `ObjectType id :: _ ->
        if List.mem (`Resource id) request then (
        print_endline "sameResource 2";
          `Permit
        ) else (
        print_endline "sameResource 3";
          `NotApplicable
        )
      | _ :: tl -> aux tl
    in
    aux resource
  in
  Combinator.target fn

let resObjectType objtype =
  let fn ~resource =
    if List.mem (`ObjectType objtype) resource then
      `Permit
    else
      `NotApplicable
  in
  Combinator.targetResource fn

let resAction action =
  let fn ~resource =
    if List.mem (`Action action) resource then
      `Permit
    else
      `NotApplicable
  in
  Combinator.targetResource fn

let isOwner =
  let fn ~resource ~request =
    print_endline "isOwner";
    let rec aux = function
      | [] ->
        print_endline "isOwner 1";
        `Failure
      | `Owner id :: _ ->
        if List.mem (`UserId id) request then (
        print_endline "isOwner 2";
          `Permit
        ) else (
        print_endline "isOwner 3";
          `Deny
        )
      | _ :: tl -> aux tl
    in
    aux resource
  in
  Combinator.condition fn

let isAdmin =
  let fn ~request =
    print_endline "isAdmin";
    if List.mem `IsAdmin request then (
      print_endline "isAdmin 1";
      `Permit
    ) else (
      print_endline "isAdmin 2";
      `NotApplicable
    )
  in
  Combinator.targetRequest fn


let isNotApplicable : ('request, 'resource, 'res) Combinator.t =
  let fn ~resource ~request =
    `NotApplicable
  in
  Combinator.target fn

let isPermit : ('request, 'resource, 'res) Combinator.t =
  let fn ~resource ~request =
    `Permit
  in
  Combinator.target fn

(********************************************)

let resComment = resObjectType "comment"

let resCreate = resAction "create"
let resDelete = resAction "delete"
let resEdit = resAction "edit"

open Combinator

let comments = asPermitDeny resComment &&& denyUnlessPermit [
  denyUnlessAllPermit [
    resCreate;
  ];
  denyUnlessAllPermit [
    resDelete;
    denyUnlessPermit [isAdmin; isOwner];
  ];
  denyUnlessAllPermit [
    resEdit;
    denyUnlessPermit [isAdmin; isOwner];
  ];
]

let request = [`UserId "john"; `Resource "comment"; `Action "create"; `IsAdmin ]

let resource = [`ObjectType "comment"; `Action "edit"; `Owner "john1" ]

let () =
  let open Combinator in
  let r = eval ~request ~resource in
  let r = string_of_result r in
  Printf.printf "RESULT = %s\n" r
