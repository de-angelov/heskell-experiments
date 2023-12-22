module DB.Repository (saveNewUser, getUser, getAllUsers) where 

import RIO 
import Types (User, UserPassword)


saveNewUser :: HasLogFunc a => Text -> UserPassword -> RIO a (Maybe User)
saveNewUser username pass = logDebug "saving new User" >> undefined


getUser :: HasLogFunc a => Text -> UserPassword -> RIO a (Maybe User)
getUser username pass = logDebug "saving new User" >> undefined 


getAllUsers :: () -> RIO a [User] 
getAllUsers = undefined 


