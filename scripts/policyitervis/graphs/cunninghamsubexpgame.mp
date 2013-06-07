verbatimtex
%&latex
\documentclass{article}
\usepackage{amsmath,amssymb}
\begin{document}
etex

input "latex";

input .././metapostgraphs/graphicslib
input .././metapostgraphs/graphlib

input cunninghamsubexpgame;			
	
beginfig(1);
	build_graph;
 	graph_draw;
endfig;

end;
