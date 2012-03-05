module OrgChart(mkPerson, mkOU) where

import Data.Tree
import Control.Arrow

data NodeData p u = Person p | OU u deriving (Eq, Show)

type OrgChart p u = Tree (NodeData p u)

mkPerson   :: p -> OrgChart p u
mkPerson p = Node { rootLabel = Person p, subForest = [] }

mkOU      :: u -> [OrgChart p u] -> OrgChart p u
mkOU u cs = Node { rootLabel = OU u, subForest = cs }

org = mkOU ("The Outfit", 50)
      [ mkPerson ("CEO", 140)
      , mkPerson ("Assistant to CEO", 60)
      , mkOU ("Retail Dept", 70)
        [ mkPerson ("Dir of Retail", 120)
        , mkPerson ("Asst Dir of Retail", 90)
        , mkPerson ("Retail Clerk", 50)
        ]
      , mkOU ("IT Dept", 130)
        [ mkPerson ("Dir of IT", 110)
        , mkPerson ("IT Specialist", 85)
        ]
      ]


incBy          :: (RealFrac a, Integral b) => a -> b -> b
incBy perc num = round (fromIntegral num * (100 + perc) / 100)

raiseNode :: (RealFrac a, Integral b) => 
             a -> NodeData (c, b) (d, b) -> NodeData (c, b) (d, b)
raiseNode perc (Person p) = Person $ second (incBy perc) p
raiseNode perc (OU u)     = OU     $ second (incBy perc) u

org' = fmap (raiseNode 2.5) org
