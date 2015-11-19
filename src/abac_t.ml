
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

  type target = resource : resource -> request : request -> targetResult

  type targetResource = resource : resource -> targetResult

  type targetRequest = request : request -> targetResult

  type condition = resource : resource -> request : request -> result

  type 'result t

  val result : [< result] -> [< result] t
  
  val targetResource :
    targetResource -> [> targetResult] t

  val targetRequest :
    targetRequest -> [> targetResult] t

  val target : target -> [> targetResult] t

  val condition : condition -> result t


  val and' : [< permitDeny] t -> [< permitDeny] t list -> [> permitDeny] t

  val or' : [< permitDeny] t -> [< permitDeny] t list -> [> permitDeny] t

  val xor' : [< permitDeny] t -> [< permitDeny] t list -> [> permitDeny] t

  val not' : [< permitDeny] t -> [> permitDeny] t

  val denyUnlessAllPermit : [< result] t list -> [> permitDeny] t

  val denyUnlessPermit : [< result] t list -> [> permitDeny] t

  val asPermitDeny : [< result] t -> [> permitDeny ] t

  val ( &&& ) : [< permitDeny] t -> [< permitDeny] t -> [> permitDeny] t
      
  val ( ||| ) : [< permitDeny] t -> [< permitDeny] t -> [> permitDeny] t

  val eval : resource:resource -> request:request -> result t -> result

  val applyResource : resource:resource -> [< result ] t -> result t

  val applyRequest : request:request -> [< result ] t -> result t
  
  val string_of_result : result -> string

  val string_of_t : result t -> string
end
