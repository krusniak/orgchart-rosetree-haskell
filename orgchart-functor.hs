module OrgChart(mkPerson, mkOU) where

import Data.Tree
import Control.Arrow

data NodeData p u = Person p | OU u deriving (Eq, Show)

type OrgChart p u = Tree (NodeData p u)

mkPerson   :: p -> OrgChart p u
mkPerson x = Node { rootLabel = Person x, subForest = [] }

mkOU      :: u -> [OrgChart p u] -> OrgChart p u
mkOU y cs = Node { rootLabel = OU y, subForest = cs }

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

-- give every person and unit a raise

incBy          :: (RealFrac a, Integral b) => a -> b -> b
incBy perc num = round (fromIntegral num * (100 + perc) / 100)

-- would like to apply this uniformly to each node in the tree
-- instance Functor Tree <- already in prelude!
-- problem: NodeData takes two type arguments!
-- instance Functor NodeData <- does not work

newtype F a = F { getF :: NodeData a a } deriving (Eq, Show)

instance Functor F where
  fmap f (F (Person d)) = F . Person $ f d
  fmap f (F (OU     d)) = F . OU     $ f d

-- outer fmap works on tree, inner one on F wrapping around NodeData

org' = fmap (getF . fmap (second $ incBy 2.5) . F) org
