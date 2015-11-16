
module Config =
struct
  type request = [ `IsAdmin | `UserId of bytes ] list
  type resource = [ `ObjectType of bytes | `Action of bytes | `Owner of bytes ] list
end

module M = Abac.Make(Config)
open M
open AbacCondition

let resObjectType a = resObjectType a |> targetResource

let resAction a = resAction a |> targetResource

let resComment = resObjectType "comment"

let isAdmin = targetRequest isAdmin
    
let isOwner = condition isOwner

let resCreate = resAction "create"
let resDelete = resAction "delete"
let resEdit = resAction "edit"

let comments : M.result M.t = asPermitDeny resComment &&& denyUnlessPermit [
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

let request = [`UserId "john"; `IsAdmin ]

let resource = [`ObjectType "comment"; `Action "edit"; `Owner "john1" ]

let () =
  let r = eval ~resource ~request comments in
  let r = string_of_result r in
  Printf.printf "RESULT = %s\n" r

