
let sameResource data =
  print_endline "sameResource";
  let rec aux = function
    | [] ->
      print_endline "sameResource 1";
      `NotApplicable
    | `ObjectType id :: _ ->
      let (id : string) = id in
      if List.mem (`Resource id) data then (
        print_endline "sameResource 2";
        `Permit
      ) else (
        print_endline "sameResource 3";
        `NotApplicable
      )
    | _ :: tl -> aux tl
  in
  aux data

let resObjectType objtype ~resource =
  if List.mem (`ObjectType objtype) resource then
    `Permit
  else
    `NotApplicable

let resAction action ~resource =
  if List.mem (`Action action) resource then
    `Permit
  else
    `NotApplicable

let isOwner ~resource ~request =
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

let isAdmin ~request =
  print_endline "isAdmin";
  if List.mem `IsAdmin request then (
    print_endline "isAdmin 1";
    `Permit
  ) else (
    print_endline "isAdmin 2";
    `NotApplicable
  )
