

module type CONFIG = Abac_t.CONFIG
module type S = Abac_t.S

module Make (Config : CONFIG) =
struct
  type resource = Config.resource
  
  type request = Config.request
  
  type permit = [ `Permit ]

  type deny = [ `Deny ]

  type permitDeny = [ permit | deny ]

  type notApplicable = [ `NotApplicable ]

  type failure = [ `Failure ]

  type result = [ permit | deny | notApplicable | failure ]

  type targetResult = [ permit | notApplicable ]


  type 'result target = resource : resource -> request : request -> 'result
    constraint 'result = [< targetResult ]

  type 'result targetResource = resource : resource -> 'result
    constraint 'result = [< targetResult ]

  type 'result targetRequest = request : request -> 'result
    constraint 'result = [< targetResult ]

  type 'result condition = resource : resource -> request : request -> 'result
    constraint 'result = [< result ]

  type ('inresult, 'outresult) plus =
    'inresult t
    -> 'inresult t list
    -> 'outresult t

  and ('inresult, 'outresult) star =
    'inresult t list
    -> 'outresult t

  and ('inresult, 'outresult) one =
    'inresult t
    -> 'outresult t

  and ('inresult, 'outresult) two =
    'inresult t
    -> 'inresult t
    -> 'outresult t

  and 'result element =
    | TargetResource: ('result targetResource) ->
      [> permit | notApplicable] element

    | TargetRequest: ('result targetRequest) ->
      [> permit | notApplicable] element

    | Target: 'result target ->
      [> permit | notApplicable] element

    | Condition: 'result condition ->
      [> result ] element

  and 'result t =
    | Element of 'result element
    | And of permitDeny t
             * permitDeny t list
    | Or of permitDeny t
            * permitDeny t list
    | Xor of permitDeny t
             * permitDeny t list
    | Not of permitDeny t
    | DenyUnlessAllPermit of result t list
    | DenyUnlessPermit of result t list
    | AsPermitDeny of result t

  let string_of_result = function
    | `Permit -> "Permit"
    | `Deny -> "Deny"
    | `NotApplicable -> "NotApplicable"
    | `Failure -> "Failure"

  let element elt = Element elt

  let targetResource v =
    element (TargetResource v)

  let targetRequest v = element (TargetRequest v)

  let target v = element (Target v)

  let condition v = element (Condition v)

  let and' (hd : [< permitDeny ] t) (tl : [< permitDeny ] t list) : [> permitDeny ] t = And (hd, tl)

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
      ~(resource : resource)
      ~(request : request)
      (cmb : [< result] t)
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
