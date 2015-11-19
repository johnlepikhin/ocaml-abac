
module type CONFIG = Abac_t.CONFIG
module type S = Abac_t.S

module Make(Config : CONFIG) : S
  with type resource = Config.resource
   and type request = Config.request
