# ocaml-abac
Attribute Based Access Control for OCaml

## Main idea

There are three values:
 1. Resource  - description of all resources in the system
 2. Request   - description of current reuqest
 3. ACL       - ACL itself

Function "eval" evaluates all of them and returns result which is one of:

 Permit - access granted
 Deny - access denied
 NotApplicable - request is not applicable to this resource
 Failure - cannot apply ACL to this request/resource
 
ACL is recursive variant of:

 1. Atoms:
 1.1. Targets. This functions MUST return Permit or NotApplicable and should be evaluated very fast.
Can be used for example to compare resourse type. Available types:

 targetResource ~resource      - check just resource
 targetRequest ~request        - check just request
 target ~resource ~request     - check request and resource

 1.2. Conditions. This functions are like targets but can return any result [ Permit | Deny | NotApplicable | Failure ].

 2. Combinators and converters:
 2.1. Logical AND, OR, NOT, XOR. Accepts list of [ Permit | Deny ] and returns [ Permit | Deny ]
 2.2. DenyUnlessPermit (Permit if any of input ACLs evaluates to Permit),
DenyUnlessAllPermit (Permit if all of input ACLs evaluates to Permit)
 2.3. AsPermitDeny. Takes one ACL and returns Permit if ACL evaluates to Permit and Deny in all other cases.

(to be continued)

## Usage

First of all, create module for your resource and request types:

 module Config =
 struct
   type resource = [ `Resource of string | `Owner of string ] list
   type request = [ `Resource of string | `UserID of string | `IsAdmin ] list
 end
 
 module MyACL = Abac.Make (Config)

One can construct ACL tree with functions from MyACL:

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

Now evaluate to result:

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
