{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import RIO
import System.IO (putStrLn)
import Lens.Micro.Platform (makeLenses)

type Children = [Person]
data Person = Person
  { _name :: !Text
  , _age :: !Int
  , _children :: !Children
  } deriving Show

makeLenses ''Person

ivan :: Person
ivan = Person
  { _name = "Ivan"
  , _age = 30
  , _children = [ stefan, stefan, stefan ]
  }

stefan :: Person
stefan = Person
  { _name = "Stefan"
  , _age = 2
  , _children = []
  }

addChild :: Person -> Person -> Person
addChild parent child =
  let updatedChildren = child : _children parent
  in parent { _children = updatedChildren }

addYear :: Person -> Person
addYear person = over age (+ 1) person

getChildren :: Person -> Children
getChildren = view children

removeChildren :: Person -> Person 
removeChildren parent =  parent { _children = [] }

removeFirstChild :: Person -> Person 
removeFirstChild parent = 
  let (x:xs) = children `view` parent
  in parent { _children = xs } 

leaveOnlyFirstChild :: Person -> Person 
leaveOnlyFirstChild parent = 
  let (x:_) = view children parent 
  in set children [x] parent

main :: IO ()
main  = runSimpleApp $ do
  let newIvan = addYear $ addChild ivan stefan
  let newIvan2 = ( addChild . addYear ) ivan stefan
  let new = (getChildren . removeFirstChild) ivan
  let new = ivan & removeChildren & getChildren

  logInfo 
    $ displayShow 
    $ "Children :" 
    <> show  (ivan & removeFirstChild . removeFirstChild . removeFirstChild)

