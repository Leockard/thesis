(* graphs.m
 * some utility Mathematica code reagarding graphs  *)

(*** VARIABLES ***)
$setSWValues = False;


(*** GENERAL FUNCTIONS ***)
newgraph[n_, p_] := RandomGraph[BernoulliGraphDistribution[n, p], DirectedEdges -> False, SelfLoops -> False];

getSimpleGraphEdges[list_] := 
 DeleteDuplicates[
  DeleteCases[
   list, _?(#[[1]] == #[[2]] &)], #1 == #2 || #1 == Reverse@#2 &]

SetAttributes[getVertexCapacity, Listable];
getVertexCapacity[g_, i_] := PropertyValue[{g, i}, VertexCapacity]

SetAttributes[getVerticesByCapacity, Listable]
getVerticesByCapacity[g_, cap_] :=
   Pick[VertexList@g,
	getVertexCapacity[graph, VertexList@graph],
	cap]


(*** MEASURES ***)
gcc[g_] := GlobalClusteringCoefficient[g] // N
cpl[g_] := 
 Plus @@ Flatten@
     GraphDistanceMatrix[g]/(VertexCount[g] (VertexCount[g] - 1)) // N

(* WARNING: small-worldness depends on measures of an ensemble of random graphs: it may
change! *)
setSWValues[n_, p_]:=
   Module[{},
      {$crand, $lrand} = 
	    Mean /@ Transpose@
	    RandomVariate[GraphPropertyDistribution[{GlobalClusteringCoefficient[gr], 
						     Plus @@ Flatten@GraphDistanceMatrix[gr]/(n (n - 1))}, 
						    Distributed[gr, BernoulliGraphDistribution[n, p]] ], 200] // N;
      $setSWValues = True;]
   
sw[g_, n_, p_] :=
   If[!$setSWValues, setSWValues[n, p], (gcc@g/$crand)/(cpl@g/$lrand)]

sw[g_] := (gcc@g/$crand)/(cpl@g/$lrand)
