
module type CONFIG =
sig
  type resource

  type request
end

module type S =
sig
  type resource
  
  type request

  type permit = [ `Permit ]

  type deny = [ `Deny ]

  type permitDeny = [ `Deny | `Permit ]

  type notApplicable = [ `NotApplicable ]

  type failure = [ `Failure ]

  type result = [ `Deny | `Failure | `NotApplicable | `Permit ]

  type targetResult = [ `NotApplicable | `Permit ]

  type 'result target = resource : resource -> request : request -> 'result
    constraint 'result = [< targetResult ]

  type 'result targetResource = resource : resource -> 'result
    constraint 'result = [< targetResult ]

  type 'result targetRequest = request : request -> 'result
    constraint 'result = [< targetResult ]

  type 'result condition = resource : resource -> request : request -> 'result
    constraint 'result = [< result ]

  type ('inresult, 'outresult) plus =
    'inresult t ->
    'inresult t list -> 'outresult t
      
  and ('inresult, 'outresult) star =
    'inresult t list -> 'outresult t
      
  and ('inresult, 'outresult) one =
    'inresult t -> 'outresult t
      
  and ('inresult, 'outresult) two =
    'inresult t -> 'inresult t -> 'outresult t
      
  and 'result t

  val targetResource :
    [< targetResult] targetResource -> [> targetResult] t

  val targetRequest :
    [< targetResult] targetRequest -> [> targetResult] t

  val target : [< targetResult] target -> [> targetResult] t

  val condition : [< result ] condition -> [> result ] t

  val and' : (permitDeny, [> permitDeny ]) plus

  val or' : (permitDeny, [> permitDeny ]) plus

  val xor' : (permitDeny, [> permitDeny ]) plus

  val not' : (permitDeny, [> permitDeny ]) one

  val denyUnlessAllPermit : (result, [> permitDeny ]) star

  val denyUnlessPermit : (result, [> permitDeny ]) star

  val asPermitDeny : (result, [> permitDeny ]) one

  val ( &&& ) : (permitDeny, [> permitDeny ]) two

  val ( ||| ) : (permitDeny, [> permitDeny ]) two

  val eval : resource:resource -> request:request -> [< result ] t -> result

  val evalRequest : resource:resource -> request:request -> [< result ] t -> result

  val evalResource : resource:resource -> request:request -> [< result ] t -> result
  
  val string_of_result :
    [< `Deny | `Failure | `NotApplicable | `Permit ] -> string
end
