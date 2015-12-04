# ocaml-abac
Attribute Based Access Control for OCaml

## Main idea

There are three values:
 * Resource  - description of all resources in the system
 * Request   - description of current reuqest
 * ACL       - ACL itself

Function "eval" evaluates all of them and returns result which is one of:

 * `Permit` - access granted
 * `Deny` - access denied
 * `NotApplicable` - request is not applicable to this resource
 * `Failure` - cannot apply ACL to this request/resource
 
ACL is recursive variant of:

* Atoms:
  * Targets. This functions MUST return `Permit` or `NotApplicable` and should be evaluated very fast.
Can be used for example to compare resourse type. Available types:
    * `targetResource ~resource`      - check just resource
    * `targetRequest ~request`        - check just request
    * `target ~resource ~request`     - check request and resource
  * Conditions. This functions are like targets but can return any result `[ Permit | Deny | NotApplicable | Failure ]`.
* Combinators and converters:
  * Logical `and'`, `or'`, `not'`, `xor'`. Accepts list of `[ Permit | Deny ]` and returns `[ Permit | Deny ]`
  * `denyUnlessPermit` (`Permit` if any of input ACLs evaluates to `Permit` and `Deny` othervise) and 
`denyUnlessAllPermit` (`Permit` if all of input ACLs evaluates to `Permit`)
  * `asPermitDeny`. Takes one ACL and returns `Permit` if ACL evaluates to `Permit` and `Deny` in all other cases.

(to be continued)

## Usage

First of all, create module for your resource and request types:
```
module Config =
struct
  type resource = [ `Resource of string | `Owner of string ] list
  type request = [ `Resource of string | `UserID of string | `IsAdmin ] list
end

module MyACL = Abac.Make (Config)
```

One can construct ACL tree with functions from MyACL:
```
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
```
Now evaluate to result:
```
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
    string_of_result r4 |> print_endline
```
### Partial apply
It is possible to evaluate only resource *targets* or only request *targets*:

```
utop # print_endline @@ string_of_t @@ acltree;;
DenyUnlessAllPermit
  TargetResource
  TargetRequest                                                                             
  DenyUnlessPermit
    TargetRequest
    Condition
- : unit = ()

(* now apply what we know about resource: *)

utop # let commentsACLTree = applyResource ~resource:[`Resource "comments"] acltree;;
val commentsACLTree : result t = <abstr>

(* and view on ACL tree again: *)

utop # print_endline @@ string_of_t @@ commentsACLTree;;
DenyUnlessAllPermit
  DenyUnlessPermit
    Condition
    TargetRequest
  TargetRequest
  - : unit = ()

(* New ACL tree is optimized for usage in resources associated with comments *)

(* The same, pre-apply request target information: *)

utop # let commentsACLTree = applyRequest ~request:[`Resource "comments"; `IsAdmin] acltree;;
val commentsACLTree : result t = <abstr>

utop # print_endline @@ string_of_t @@ commentsACLTree;;
TargetResource
- : unit = ()

(* Most of rules pre-applied. The only remaining rule is about to check `Resourse in resource *)
```
