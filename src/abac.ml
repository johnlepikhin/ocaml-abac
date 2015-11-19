

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

  type target = resource : resource -> request : request -> targetResult

  type targetResource = resource : resource -> targetResult

  type targetRequest = request : request -> targetResult

  type condition = resource : resource -> request : request -> result

  type 'result t =
    | Result of result
    | Condition of condition
    | TargetResource of targetResource
    | TargetRequest of targetRequest
    | Target of target
    | And of permitDeny t * permitDeny t list
    | Or of permitDeny t * permitDeny t list
    | Xor of permitDeny t * permitDeny t list
    | Not of permitDeny t
    | DenyUnlessAllPermit of result t list
    | DenyUnlessPermit of result t list
    | AsPermitDeny of result t

        
  let string_of_result = function
    | `Permit -> "Permit"
    | `Deny -> "Deny"
    | `NotApplicable -> "NotApplicable"
    | `Failure -> "Failure"

  let result r = Result (r : [< result] :> result)

  let targetResource v =
    TargetResource v

  let targetRequest v = TargetRequest v

  let target v = Target v

  let condition v = Condition v

  let and' hd tl = And ((hd : [< permitDeny] t :> permitDeny t), (tl : [< permitDeny] t list :> permitDeny t list))

  let or' hd tl = Or ((hd : [< permitDeny] t :> permitDeny t), (tl : [< permitDeny] t list :> permitDeny t list))

  let xor' hd tl = Xor ((hd : [< permitDeny] t :> permitDeny t), (tl : [< permitDeny] t list :> permitDeny t list))

  let not' cmb = Not (cmb : [< permitDeny] t :> permitDeny t)

  let denyUnlessAllPermit lst = DenyUnlessAllPermit (lst : [< result] t list :> result t list)

  let denyUnlessPermit lst = DenyUnlessPermit (lst : [< result] t list :> result t list)

  let asPermitDeny t = AsPermitDeny (t : [< result ] t :> result t)

  let ( &&& ) a b = and' a [b]

  let ( ||| ) a b = or' a [b]

  let rec eval
      ~(resource : resource)
      ~(request : request)
      (cmb : result t)
    =
    match cmb with
    | Result r -> r
    | Target fn -> (fn ~resource ~request : targetResult :> result)
    | TargetResource fn -> (fn ~resource : targetResult :> result)
    | TargetRequest fn -> (fn ~request : targetResult :> result)
    | Condition fn -> fn ~resource ~request

    | And (hd, tl) ->
      let folder prev incmb =
        if prev = `Permit then
          eval ~resource ~request (incmb : permitDeny t :> result t)
        else `Deny
      in
      List.fold_left folder `Permit (hd :: tl)

    | Or (hd, tl) ->
      let folder prev incmb =
        if prev = `Deny then
          eval ~resource ~request (incmb : permitDeny t :> result t)
        else `Permit
      in
      List.fold_left folder `Deny (hd :: tl)

    | Xor (hd, tl) ->
      let folder prev incmb =
        if prev = `Deny then
          eval ~resource ~request (incmb : permitDeny t :> result t)
        else `Permit
      in
      if List.fold_left folder `Deny (hd :: tl) = `Deny then
        `Permit
      else
        `Deny

    | Not incmb ->
      if eval ~resource ~request (incmb : permitDeny t :> result t) = `Permit then
        `Deny
      else
        `Permit

    | DenyUnlessAllPermit lst ->
      let folder prev incmb =
        match prev with
        | None | Some `Permit ->
          Some (eval ~resource ~request incmb)
        | _ -> Some `Deny
      in
      let r = List.fold_left folder None lst in
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
          eval ~resource ~request incmb
      in
      List.fold_left folder `Deny lst

    | AsPermitDeny incmb ->
      match eval ~resource ~request incmb with
      | `Permit -> `Permit
      | _ -> `Deny
        
  let string_of_t t =
    let rec aux deep (t : result t) =
      let prefix = String.make (deep*2) ' ' in
      match t with
      | Result v -> prefix ^ "Result " ^ (string_of_result v)
      | Target _ -> prefix ^ "Target"
      | TargetResource _ -> prefix ^ "TargetResource"
      | TargetRequest _ -> prefix ^ "TargetRequest"
      | Condition _ -> prefix ^ "Condition"
      | Not v -> prefix ^ "Not\n" ^ (aux (deep+1) (v : permitDeny t :> result t))
      | And (hd, tl) ->
        let hd = (hd : permitDeny t :> result t) in
        let tl = (tl : permitDeny t list :> result t list) in
        let lst = List.map (aux (deep+1)) (hd :: tl) |> String.concat "\n" in
        prefix ^ "And\n" ^ lst
      | Or (hd, tl) ->
        let hd = (hd : permitDeny t :> result t) in
        let tl = (tl : permitDeny t list :> result t list) in
        let lst = List.map (aux (deep+1)) (hd :: tl) |> String.concat "\n" in
        prefix ^ "Or\n" ^ lst
      | Xor (hd, tl) ->
        let hd = (hd : permitDeny t :> result t) in
        let tl = (tl : permitDeny t list :> result t list) in
        let lst = List.map (aux (deep+1)) (hd :: tl) |> String.concat "\n" in
        prefix ^ "Xor\n" ^ lst
      | DenyUnlessPermit lst ->
        let lst = List.map (aux (deep+1)) lst |> String.concat "\n" in
        prefix ^ "DenyUnlessPermit\n" ^ lst
      | DenyUnlessAllPermit lst ->
        let lst = List.map (aux (deep+1)) lst |> String.concat "\n" in
        prefix ^ "DenyUnlessAllPermit\n" ^ lst
      | AsPermitDeny v -> prefix ^ "AsPermitDeny\n" ^ (aux (deep+1) v)
    in
    aux 0 t
  
  module Apply =
  struct
    type target =
      | EvalResource of resource
      | EvalRequest of request

    let apply ~target cmb =
      let cmb = (cmb : [< result ] t :> result t) in
      let rec aux t =
        match t with
        | Result r ->
          true, Result r

        | TargetResource fn -> (match target with
            | EvalResource resource -> true, Result (fn ~resource : targetResult :> result)
            | EvalRequest _ -> false, t)

        | TargetRequest fn -> (match target with
            | EvalResource _ -> false, t
            | EvalRequest request -> true, Result (fn ~request : targetResult :> result))

        | Target fn -> false, t

        | Condition fn -> false, t

        | And (hd, tl) ->
          let rec aux1 (r : result t list) = function
            | [] -> (
                match r with
                | [] -> true, Result `Permit
                | hd :: [] -> false, hd
                | hd :: tl -> false, and' (Obj.magic hd) (Obj.magic tl)
              )
            | hd :: tl ->
              let v = aux (hd : permitDeny t :> result t) in
              match v with
              | true, Result `Permit -> aux1 r tl
              | true, _ -> true, Result `Deny
              | false, v -> aux1 (v :: r) tl
          in
          aux1 [] (hd :: tl)

        | Or (hd, tl) ->
          let rec aux1 (r : result t list) = function
            | [] -> (
                match r with
                | [] -> true, Result `Deny
                | hd :: [] -> false, hd
                | hd :: tl -> false, or' (Obj.magic hd) (Obj.magic tl)
              )
            | hd :: tl ->
              let v = aux (hd : permitDeny t :> result t) in
              match v with
              | true, Result `Permit -> true, Result `Permit
              | true, _ -> aux1 r tl
              | false, v -> aux1 (v :: r) tl
          in
          aux1 [] (hd :: tl)

        | Xor (hd, tl) ->
          let rec aux1 (r : result t list) = function
            | [] -> (
                match r with
                | [] -> true, Result `Permit
                | hd :: [] -> false, hd
                | hd :: tl -> false, and' (Obj.magic hd) (Obj.magic tl)
              )
            | hd :: tl ->
              let v = aux (hd : permitDeny t :> result t) in
              match v with
              | true, Result `Deny -> aux1 r tl
              | true, _ -> true, Result `Deny
              | false, v -> aux1 (v :: r) tl
          in
          aux1 [] (hd :: tl)

        | Not v ->
          let v = aux (v : permitDeny t :> result t) in
          (match v with
           | true, Result `Permit -> true, Result `Deny
           | true, _ -> true, Result `Permit
           | false, v -> false, not' (Obj.magic v))


        | DenyUnlessPermit lst ->
          let rec aux1 (r : result t list) = function
            | [] -> (
                match r with
                | [] -> true, Result `Deny
                | hd :: [] -> false, hd
                | hd :: tl -> false, denyUnlessPermit r
              )
            | hd :: tl ->
              let v = aux hd in
              match v with
              | true, Result `Permit -> true, Result `Permit
              | true, _ -> aux1 r tl
              | false, v -> aux1 (v :: r) tl
          in
          aux1 [] lst

        | DenyUnlessAllPermit lst ->
          let rec aux1 (r : result t list) = function
            | [] -> (
                match r with
                | [] -> true, Result `Permit
                | hd :: [] -> false, hd
                | hd :: tl -> false, denyUnlessAllPermit r
              )
            | hd :: tl ->
              let v = aux hd in
              match v with
              | true, Result `Permit -> aux1 r tl
              | true, _ -> true, Result `Deny
              | false, v -> aux1 (v :: r) tl
          in
          aux1 [] lst

        | AsPermitDeny v ->
          let v = aux v in
          (match v with
           | true, Result `Permit -> true, Result `Permit
           | true, _ -> true, Result `Deny
           | false, v -> false, asPermitDeny (Obj.magic v))

      in
      let (_, r) = aux cmb in
      r

  end

  let applyResource ~resource t = Apply.(apply ~target:(EvalResource resource) t)
      
  let applyRequest ~request t = Apply.(apply ~target:(EvalRequest request) t)

end
