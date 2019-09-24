module DefinitionSorting
    ( sortDefinitions
    ) where


import Data.Graph
import Data.Maybe
import Ast

sortDefinitions :: Ast -> Either String Ast
sortDefinitions (Ast i definitions_list) =
  case topologicalSort definitions_list of
    Right defs -> Right $ Ast i defs
    Left cycles -> Left $ "Cyclic dependencies found:\n" ++ (show cycles)

{- |
   Takes a directed graph representing the dependencies between definitions,
   detects any cycles and returns either a list with all the cycles or
   a list with the vertex topologically sorted. Note that for cycle detection
   this function uses strongly conected components, so the cycles are guaranteed to
   be in the previous list, but it can be also some extra vertex.
-}
topologicalSort :: [Definition] -> Either [[Definition]] [Definition]
topologicalSort definitions =
  let edges = buildGraphEdges definitions in
  case detectCycles edges of
  [] -> let (graph, vertexToNode) = graphFromEdges' edges in
        Right $ map ((\ (node, _, _) -> node) . vertexToNode) $ reverse (topSort graph)
  cycles -> Left cycles


buildGraphEdges :: [Definition] -> [(Definition, String, [String])]
buildGraphEdges definitions =
  map definitionEdges definitions
  where
    definitionEdges (Definition i name params rules) =
      (Definition i name params rules, name, dependencies rules [])
    dependencies [] depList = depList
    dependencies ((Scene _ _ (SceneIdGlobalVariable _ name) _):tl) depList =
      dependencies tl (name:depList)
    dependencies (_:tl) depList = dependencies tl depList


detectCycles :: [(Definition, String, [String])] -> [[Definition]]
detectCycles edges =
  let scc = stronglyConnComp edges in
  cyclicScc scc []
  where
    cyclicScc [] cycList = cycList
    cyclicScc ((CyclicSCC defsList):tl) cycList = cyclicScc tl (defsList:cycList)
    cyclicScc (_:tl) cycList = cyclicScc tl cycList
