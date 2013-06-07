numeric
	node_style_even,
	node_style_odd,
	node_style_prio,
	node_style_prio_small,

	edge_style_default,
	edge_style_strategy,
	edge_style_counter,
	edge_style_improving,
	edge_style_disabled,
	edge_style_discounter,
	style_separator;


node_style_even := node_style_add(primitive_style_add(white, path_style_default, primitive_circle), (1,1));
node_style_odd := node_style_add(primitive_style_add(white, path_style_default, primitive_diamond), (1,1));
node_style_prio := node_style_add(primitive_style_add(white, path_style_default, primitive_diamond), (1,1));
node_style_prio_small := node_style_add(primitive_style_add(white, path_style_add(black, path_shape_dashed, 1), primitive_diamond), (0.75,0.75));
edge_style_default := path_style_add(black, path_shape_solid, 1);

edge_style_default := path_style_add(black, path_shape_solid, 1.75);
edge_style_strategy := path_style_add(blue, path_shape_solid, 6);
edge_style_counter := path_style_add(red, path_shape_solid, 6);
edge_style_improving := path_style_add(0.85green, path_shape_dashed, 4);
edge_style_disabled := path_style_add(0.3white, path_shape_dashed, 0.75);
edge_style_discounter := path_style_add(0.3white, path_shape_dashed, 0.75);

style_separator := path_style_add(black, path_shape_dashed, 0.75);



numeric % Groups
	group_source,
	group_sink,
	group_layers[];
	
numeric % Nodes
	node_a[],
	node_b[],
	node_c[],
	node_d[],
	node_e[],
	node_F[],
	node_s,
	node_t,
	node_g[],
	node_h[];
	
	
numeric % Edges
	edge_a_in[],
	edge_a_out[],
	edge_b_in[],
	edge_b_out[],
	edge_c_in[],
	edge_c_out[],
	edge_d_in[],
	edge_d_out[],
	edge_e_in[],
	edge_e_out[],
	edge_F_in_d[],
	edge_F_in_e[],
	edge_F_out[],
	edge_s,
	edge_t,
	edge_g[],
	edge_h[];

	

vardef build_graph(expr aspg) =
	vardef prio_annot(expr n, priopg, priomdp) =
		if (aspg):
			if (not (priopg = "")):
				node_add_text_line(n, latex(priopg), black);
			fi;
		else:
			if (not (priomdp = "")):
				node_add_text_line(n, latex(priomdp), black);
			fi;
		fi;
	enddef;

	vardef new_even_node(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_even);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		n
	enddef;

	vardef new_odd_node(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_odd);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		n
	enddef;

	vardef new_prio_node(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_prio);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		n
	enddef;

	vardef new_prio_node_small(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_prio_small);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		n
	enddef;

	vardef new_default_edge(expr n, m) =
		edge_register(node_get_coord(n), node_get_coord(m), edge_style_default)
	enddef;

	vardef new_edge_to_coord(expr node, coord) =
		numeric e;
		e := edge_register(node_get_coord(node), coord, edge_style_default);
		edge_set_arrow_style(e, 0, path_arrow_sep);
		e
	enddef;

	vardef new_coord_to_edge(expr coord, node) =
		edge_register(coord, node_get_coord(node), edge_style_default)
	enddef;

	vardef new_coord_to_coord(expr coords, coord) =
		numeric e;
		e := edge_register(coords, coord, edge_style_default);
		edge_set_arrow_style(e, 0, path_arrow_none);
		e
	enddef;

	vardef i_string(expr times_i, add_const) =
		string tmp;
		if(times_i > 0):
			if (times_i > 1):
				if (add_const > 0):
					tmp := "{" & decimal(times_i) & "i{+}" & decimal(add_const) & "}";
				else:
					tmp := "{" & decimal(times_i) & "i}";
				fi;
			else:
				if (add_const > 0):
					tmp := "{i{+}" & decimal(add_const) & "}";
				else:
					tmp := "{i}";
				fi;
			fi;
		else:
			tmp := "{" & decimal(add_const) & "}";
		fi;
		tmp
	enddef;
	
	vardef build_group_layer(expr basey, index, count, times_i, add_const) =
		group_layers[index] := group_add(group_root);
		group_enter(group_layers[index]);	
		coords_enter_base((0,basey));
		
		string indexstr;
		indexstr := i_string(times_i, add_const);	
		
		if (index > 1) :
			node_a[index] := new_even_node(0,0, "$a_" & indexstr & "$", "3", "");
		fi;
		
		if (index < count) :
			if (index > 1):
				node_b[index] := new_even_node(8,0, "$b_" & indexstr & "$", "3", "");
			else:
				node_b[index] := coords_register(8,0);
			fi;
		fi;

		if (index > 1):
			node_c[index] := new_even_node(10,2, "$c_" & indexstr & "$", "3", "");
		else :
			node_c[index] := coords_register(10,2);
		fi;
		if (index > 1):
			node_d[index] := new_even_node(5, 2, "$d_" & indexstr & "$", "5", "");
		fi;
		node_e[index] := new_even_node(5, 4, "$e_" & indexstr & "$", "5", "");
		node_F[index] := new_odd_node(2, 3, "$F_" & indexstr & "$", "6", "");
		node_g[index] := new_prio_node(2, 0, "$g_" & indexstr & "$", decimal(2*index+7), decimal(2*index-1));
		node_h[index] := new_prio_node(2, 5.5, "$h_" & indexstr & "$", decimal(2*index+8), decimal(2*index));
		
		if (index > 1) :
			edge_a_in[index] := new_default_edge(node_a[index], node_g[index]);
			edge_add_text_line(edge_a_in[index], 0, latex("$a_" & indexstr & "^1$"), black, 0.5, true);
		fi;
		
		if (index < count) :
			if (index > 1):
				edge_b_in[index] := new_default_edge(node_b[index], node_g[index]);
				edge_add_text_line(edge_b_in[index], 0, latex("$b_" & indexstr & "^1$"), black, 0.2, true);
			else:
				edge_b_in[index] := -1;
			fi;
		fi;

		if (index > 1):
			edge_c_in[index] := new_default_edge(node_c[index], node_g[index]);
			edge_add_text_line(edge_c_in[index], 0, latex("$c_" & indexstr & "^1$"), black, 0.1, true);
		else:
			edge_c_in[index] := -1;
		fi;

		if (index > 1) :
			edge_d_in[index] := new_default_edge(node_d[index], node_F[index]);
			edge_add_text_line(edge_d_in[index], 0, latex("$d_" & indexstr & "^1$"), black, 0.2, false);
			edge_F_in_d[index] := new_default_edge(node_F[index], node_d[index]);
			edge_path_curl(edge_d_in[index], 0, -0.1);
			edge_path_curl(edge_F_in_d[index], 0, -0.1);
			if (not aspg):
				edge_add_text_line(edge_F_in_d[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.5, false);
			fi;
		fi;
		edge_e_in[index] := new_default_edge(node_e[index], node_F[index]);
		edge_add_text_line(edge_e_in[index], 0, latex("$e_" & indexstr & "^1$"), black, 0.2, true);
		edge_F_in_e[index] := new_default_edge(node_F[index], node_e[index]);
		edge_F_out[index] := new_default_edge(node_F[index], node_h[index]);
		edge_g[index] := new_default_edge(node_g[index], node_F[index]);

		edge_path_curl(edge_e_in[index], 0, 0.1);
		edge_path_curl(edge_F_in_e[index], 0, 0.1);

		if (not aspg) :
			edge_add_text_line(edge_F_out[index], 0, latex("$\varepsilon$"), black, 0.2, false);
			if (index > 1):
				edge_add_text_line(edge_F_in_e[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.5, true);
			else :
				edge_add_text_line(edge_F_in_e[index], 0, latex("$1{-}\varepsilon$"), black, 0.5, true);
			fi;
		fi;
		
		coords_leave_base;
		group_leave;
	enddef;
		
	vardef build_sink(expr basey) =
		group_sink := group_add(group_root);
		group_enter(group_sink);	
		coords_enter_base((0,basey));
		
		node_t        := new_prio_node(2,2, "$t$", "1", "");
		node_s		  := new_prio_node(10,0,"$s$", "8", "0");
		
		edge_t      := new_default_edge(node_t, node_t);
			   	       edge_path_loop(edge_t, 0, 0.5, 0.75, 0);
						 
		coords_leave_base;
		group_leave;
	enddef;

	vardef connect_group_layer_with_next(expr layer, count) =
		string indexstr;
		indexstr := decimal(layer);
		if (layer > 1):
			if (layer < count):
				edge_a_out[layer] := new_default_edge(node_a[layer], node_a[layer+1]);
			else :
				edge_a_out[layer] := new_default_edge(node_a[layer], node_t);
	            edge_register_coord_(edge_a_out[layer], coords_register(0, (ypart (coords_get_abs(node_get_coord(node_t))))));
				
			fi;
			edge_add_text_line(edge_a_out[layer], 0, latex("$a_" & indexstr & "^0$"), black, 0.1, true);
		fi;
		if (layer < count) :
			if (layer > 1) :
				if (layer > 2) :
					edge_b_out[layer] := new_default_edge(node_b[layer], node_b[layer-1]);
				else :
					edge_b_out[layer] := new_default_edge(node_b[layer], node_g[layer-1]);
					edge_register_coord_(edge_b_out[layer], node_b[layer-1]);
				fi;
				edge_add_text_line(edge_b_out[layer], 0, latex("$b_" & indexstr & "^0$"), black, 0.1, true);
			fi;
		fi;
		if (layer > 1) :
			if (layer > 2) :
				edge_c_out[layer] := new_default_edge(node_c[layer], node_c[layer-1]);
			else :
				edge_c_out[layer] := new_default_edge(node_c[layer], node_g[layer-1]);
				edge_register_coord_(edge_c_out[layer], node_c[layer-1]);
			fi;
			edge_add_text_line(edge_c_out[layer], 0, latex("$c_" & indexstr & "^0$"), black, 0.1, true);
		fi;
		
		if (layer > 1) :
			if (layer > 2) :
				edge_d_out[layer] := new_default_edge(node_d[layer], node_b[layer-1]);
			else :
				edge_d_out[layer] := new_edge_to_coord(node_d[layer], node_b[layer-1]);
			fi;
			edge_add_text_line(edge_d_out[layer], 0, latex("$d_" & indexstr & "^0$"), black, 0.05, true);
		fi;

		if (layer < count) :
			edge_h[layer] := new_default_edge(node_h[layer], node_a[layer+1]);
		else :
			edge_h[layer] := new_default_edge(node_h[layer], node_t);
		fi;
		
		edge_e_out[layer] := new_default_edge(node_e[layer], node_s);		
		edge_add_text_line(edge_e_out[layer], 0, latex("$e_" & indexstr & "^0$"), black, 0.5, true);

		if (layer = count) :
			edge_s := new_default_edge(node_s, node_c[count]);
		fi;
	enddef;
	
	graph_reset;
	build_group_layer(0,1,3,0,1);
	build_group_layer(ypart group_get_coord_ur(group_layers[1]) + 1.75,2,3,0,2);
	build_group_layer(ypart group_get_coord_ur(group_layers[2]) + 1.75,3,3,0,3);
	build_sink(ypart group_get_coord_ur(group_layers[3]) + 1.75);
	group_separator_add(group_layers[1], group_layers[2], style_separator, 0.10);
	group_separator_add(group_layers[2], group_layers[3], style_separator, 0.10);
	group_separator_add(group_layers[3], group_sink, style_separator, 0.10);
	connect_group_layer_with_next(1,3);
	connect_group_layer_with_next(2,3);
	connect_group_layer_with_next(3,3);
enddef;


vardef edge_translate(expr source, source_stack, target, target_stack) =
  numeric source_arr[];
  numeric source_arr_len;
  
  source_arr_len := 0;
  
  for i = scantokens(source_stack) :
  	source_arr[source_arr_len] := i;
  	source_arr_len := source_arr_len + 1;
  endfor;
  
  numeric target_arr[];
  numeric target_arr_len;
  
  target_arr_len := 0;
  
  for i = scantokens(target_stack) :
  	target_arr[target_arr_len] := i;
  	target_arr_len := target_arr_len + 1;
  endfor;
  
  
  if     (source="x") and (target="y"): edge_F_out[source_arr[0]+1]
  elseif (source="x") and (target="d"): edge_F_in_d[source_arr[0]+1]
  elseif (source="x") and (target="e"): edge_F_in_e[source_arr[0]+1]
  elseif (source="a") and (target="q"): edge_a_in[source_arr[0]+1]
  elseif (source="a") and (target="a"): edge_a_out[source_arr[0]+1]
  elseif (source="b") and (target="q"): edge_b_in[source_arr[0]]
  elseif (source="b") and (target="b"): edge_b_out[source_arr[0]]
  elseif (source="c") and (target="q"): edge_c_in[source_arr[0]+1]
  elseif (source="c") and (target="c"): edge_c_out[source_arr[0]+1]
  elseif (source="d") and (target="x"): edge_d_in[source_arr[0]+1]
  elseif (source="d") and (target="b"): edge_d_out[source_arr[0]+1]
  elseif (source="e") and (target="x"): edge_e_in[source_arr[0]+1]
  elseif (source="e") and (target="p"): edge_e_out[source_arr[0]+1]
  else:
    -1
  fi
enddef;  
