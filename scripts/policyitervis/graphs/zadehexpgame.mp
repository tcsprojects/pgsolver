verbatimtex
%&latex
\documentclass{article}
\usepackage{amsmath,amssymb}
\begin{document}
etex

input "latex";

input ./metapostgraphs/graphicslib
input ./metapostgraphs/graphlib

input zadehexpgame;			
	
beginfig(1);
	build_graph(true);
 	graph_draw;
endfig;

end;
