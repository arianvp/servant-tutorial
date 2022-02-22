# Outline

How did records in servant work before?:

Servant has no concept of records. Just routes combined with `:<|>`

Servant instead defines a type level function `type ToServant routes mode =
GToServant (Rep (routes mode))` .  That takes the generic representation of a
record and maps each record field to a separate API endpoint combined with
`:<|>`

A user has to manually convert their routes record to a "servant type" by
calling  `toServantApi`. As such records are not first-class in Servant. But we
have a function that translates record types to servant types.

```haskell
data Routes mode = Routes
 { getUser    :: mode :- "users" :> Get '[JSON] User 
 , deleteUSer :: mode :- "users" :> DeleteNoContent
 }

Routes AsApi = Routes { getUser :: "users" :> Get '[JSON] User, deleteUser :: "users" :> DeleteNoContent }

GToServant (Rep (Routes AsApi))
    = typeof getUser :<|> typeof deleteUser
    = ("users" :> Get '[JSON] User) :<|> ("users" :> DeleteNoContent)
```

