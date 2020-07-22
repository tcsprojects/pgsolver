verbatimtex
%&latex
\documentclass{article}
\usepackage{amsmath,amssymb}
\begin{document}
etex

input "latex";

input ./metapostgraphs/graphicslib
input ./metapostgraphs/graphlib

input randomedgeexpgame;

beginfig(1);
	build_graph(3, true, true, true, true);
 	graph_draw;
endfig;

end;
