module UI.Names where

data Name =
    Nop
  | Name String
  | ClusterVP
  deriving (Show, Eq, Ord)
