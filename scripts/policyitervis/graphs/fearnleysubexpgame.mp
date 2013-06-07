verbatimtex
%&latex
\documentclass{article}
\usepackage{amsmath,amssymb}
\begin{document}
etex

input "latex";

input .././metapostgraphs/graphicslib
input .././metapostgraphs/graphlib

input fearnleysubexpgame;			
	
beginfig(1);
	build_graph(false, 3);
 	graph_draw;
endfig;

beginfig(2);
	build_graph(true, 3);
 	graph_draw;
endfig;


end;
