numeric
	node_style_even,
	node_style_odd,
	node_style_prio,
	node_style_even_small,
	node_style_prio_small,

	edge_style_default,
	edge_style_strategy,
	edge_style_counter,
	edge_style_improving,
	edge_style_disabled,
	edge_style_discounter,
	style_separator;


node_style_even := node_style_add(primitive_style_add(white, path_style_default, primitive_circle), (1,1));
node_style_odd := node_style_add(primitive_style_add(white, path_style_default, primitive_rectangle), (1,1));
node_style_prio := node_style_add(primitive_style_add(white, path_style_default, primitive_diamond), (1,1));
node_style_even_small := node_style_add(primitive_style_add(white, path_style_add(black, path_shape_dashed, 1), primitive_circle), (0.75,0.75));
node_style_prio_small := node_style_add(primitive_style_add(white, path_style_add(black, path_shape_dashed, 1), primitive_diamond), (0.75,0.75));
edge_style_default := path_style_add(black, path_shape_solid, 1);

edge_style_default := path_style_add(black, path_shape_solid, 1.75);
edge_style_strategy := path_style_add(blue, path_shape_solid, 2.5);
edge_style_counter := path_style_add(red, path_shape_solid, 2.5);
edge_style_improving := path_style_add(0.85green, path_shape_dashed, 1.75);
edge_style_disabled := path_style_add(0.3white, path_shape_dashed, 0.75);
edge_style_discounter := path_style_add(0.3white, path_shape_dashed, 0.75);

style_separator := path_style_add(black, path_shape_dashed, 0.75);



numeric % Groups
	group_sink,
	group_layers[];
	
numeric % Nodes
	node_d[],               % g
	node_f[],               % b
	node_b[],               % d00
	node_m_one[],           % c
	node_v[],               % d10
	node_E[],               % F0
	node_X[],               % F1
	node_a[],               % d01
	node_d_zero[],          % g
	node_w[],               % d11
	node_c[],               % h0
	node_g[],               % s0
	node_f_zero[],          % b
	node_s[],               % s1
	node_u[],               % h1
	node_o[],				% e01
	node_p[],               % e00
	node_q[],               % e11
	node_r[],               % e10
	node_t;                 % t
	
	
numeric % Edges
	edge_d_left[],
	edge_d_right[],
	edge_f_in[],
	edge_f_out[],
	edge_b_in[],
	edge_b_out[],
	edge_p_out_m[],
	edge_p_out_d[],
	edge_v_in[],
	edge_v_out[],
	edge_r_out_m[],
	edge_r_out_d[],
	edge_E_in_b[],
	edge_E_in_a[],
	edge_E_out[],
	edge_X_in_v[],
	edge_X_in_w[],
	edge_X_out[],
	edge_a_in[],
	edge_a_out[],
	edge_o_out_m[],
	edge_o_out_d[],
	edge_w_in[],
	edge_w_out[],
	edge_q_out_m[],
	edge_q_out_d[],
	edge_c[],
	edge_g_in[],
	edge_g_out[],
	edge_s_in[],
	edge_s_out[],
	edge_u[],
	edge_t;

	

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

	vardef new_even_node_small(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_even_small);
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
		
		numeric mdpminus;
		mdpminus:=6;

		node_d[index] := new_even_node(8,0, "$g_" & indexstr & "$", decimal(2*index+9), decimal(2*index+9-mdpminus));
		node_f[index] := new_even_node(0,0, "$b_" & indexstr & "$", "3", "");
		node_b[index] := new_even_node(4, 2, "$d_{" & indexstr & ",0,0}$", "3", "");
		node_p[index] := new_even_node(6, 2, "$e_{" & indexstr & ",0,0}$", "3", "");
		node_m_one[index] := new_even_node_small(8, 2, "$b_2$", "3", "");
		node_E[index] := new_odd_node(2, 4, "$F_{" & indexstr & ",0}$", "6", "");
		if (index < count):
			node_v[index] := new_even_node(12, 2, "$d_{" & indexstr & ",1,0}$", "3", "");
			node_r[index] := new_even_node(10, 2, "$e_{" & indexstr & ",1,0}$", "3", "");
			node_X[index] := new_odd_node(14, 4, "$F_{" & indexstr & ",1}$", "4", "");
		fi;
		node_a[index] := new_even_node(4, 6, "$d_{" & indexstr & ",0,1}$", "3", "");
		node_o[index] := new_even_node(6, 6, "$e_{" & indexstr & ",0,1}$", "3", "");
		node_d_zero[index] := new_even_node_small(8, 6, "$g_1$", "3", "");
		node_c[index] := new_prio_node(2, 8, "$h_{" & indexstr & ",0}$", decimal(2*index+10), decimal(2*index+10-mdpminus));
		node_g[index] := new_even_node(4, 8, "$s_{" & indexstr & ",0}$", decimal(10), decimal(10-mdpminus));
		node_f_zero[index] := new_even_node_small(8, 8, "$b_1$", "3", "");
		if (index < count):
			node_w[index] := new_even_node(12, 6, "$d_{" & indexstr & ",1,1}$", "3", "");
			node_q[index] := new_even_node(10, 6, "$e_{" & indexstr & ",1,1}$", "3", "");
			node_s[index] := new_even_node(12, 8, "$s_{" & indexstr & ",1}$", decimal(8), decimal(8-mdpminus));
			node_u[index] := new_prio_node(14, 8, "$h_{" & indexstr & ",1}$", decimal(2*index+10), decimal(2*index+10-mdpminus));
		fi;

		edge_d_left[index] := new_default_edge(node_d[index], node_E[index]);
		edge_add_text_line(edge_d_left[index], 0, latex("$g_" & indexstr & "^0$"), black, 0.3, false);
		edge_f_in[index] := new_default_edge(node_f[index], node_d[index]);
		edge_add_text_line(edge_f_in[index], 0, latex("$b_" & indexstr & "^1$"), black, 0.1, false);
		if (index < count):
			edge_d_right[index] := new_default_edge(node_d[index], node_X[index]);
			edge_add_text_line(edge_d_right[index], 0, latex("$g_" & indexstr & "^1$"), black, 0.7, false);
		fi;
		edge_b_in[index] := new_default_edge(node_b[index], node_E[index]);
		edge_add_text_line(edge_b_in[index], 0, latex("$d_{" & indexstr & ",0,0}^1$"), black, 0.75, true);
		edge_b_out[index] := new_default_edge(node_b[index], node_p[index]);
		edge_add_text_line(edge_b_out[index], 0, latex("$d_{" & indexstr & ",0,0}^0$"), black, 0.5, false);
		edge_p_out_m[index] := new_default_edge(node_p[index], node_m_one[index]);
		edge_add_text_line(edge_p_out_m[index], 0, latex("$e_{" & indexstr & ",0,1}^1$"), black, 0.5, false);
		edge_p_out_d[index] := new_default_edge(node_p[index], node_d_zero[index]);
		edge_add_text_line(edge_p_out_d[index], 0, latex("$e_{" & indexstr & ",0,0}^0$"), black, 0.9, false);
		if (index < count):
			edge_v_in[index] := new_default_edge(node_v[index], node_X[index]);
			edge_add_text_line(edge_v_in[index], 0, latex("$d_{" & indexstr & ",1,0}^1$"), black, 0.75, false);
			edge_v_out[index] := new_default_edge(node_v[index], node_r[index]);
			edge_add_text_line(edge_v_out[index], 0, latex("$d_{" & indexstr & ",1,0}^0$"), black, 0.5, false);
			edge_r_out_m[index] := new_default_edge(node_r[index], node_m_one[index]);
			edge_add_text_line(edge_r_out_m[index], 0, latex("$e_{" & indexstr & ",1,0}^1$"), black, 0.5, false);
			edge_r_out_d[index] := new_default_edge(node_r[index], node_d_zero[index]);
			edge_add_text_line(edge_r_out_d[index], 0, latex("$e_{" & indexstr & ",1,0}^0$"), black, 0.75, false);
		fi;
		edge_E_in_b[index] := new_default_edge(node_E[index], node_b[index]);
		edge_E_in_a[index] := new_default_edge(node_E[index], node_a[index]);
		edge_E_out[index] := new_default_edge(node_E[index], node_g[index]);
		if (index < count):
			edge_X_in_v[index] := new_default_edge(node_X[index], node_v[index]);
			edge_X_in_w[index] := new_default_edge(node_X[index], node_w[index]);
			edge_X_out[index] := new_default_edge(node_X[index], node_s[index]);
		fi;
		edge_a_in[index] := new_default_edge(node_a[index], node_E[index]);
		edge_add_text_line(edge_a_in[index], 0, latex("$d_{" & indexstr & ",0,1}^1$"), black, 0.75, false);
		edge_a_out[index] := new_default_edge(node_a[index], node_o[index]);
		edge_add_text_line(edge_a_out[index], 0, latex("$d_{" & indexstr & ",0,1}^0$"), black, 0.75, false);
		edge_o_out_m[index] := new_default_edge(node_o[index], node_m_one[index]);
		edge_add_text_line(edge_o_out_m[index], 0, latex("$e_{" & indexstr & ",0,1}^1$"), black, 0.75, false);
		edge_o_out_d[index] := new_default_edge(node_o[index], node_d_zero[index]);
		edge_add_text_line(edge_o_out_d[index], 0, latex("$e_{" & indexstr & ",0,1}^0$"), black, 0.5, false);
		if (index < count):
			edge_w_in[index] := new_default_edge(node_w[index], node_X[index]);
			edge_add_text_line(edge_w_in[index], 0, latex("$d_{" & indexstr & ",1,1}^1$"), black, 0.75, true);
			edge_w_out[index] := new_default_edge(node_w[index], node_q[index]);
			edge_add_text_line(edge_w_out[index], 0, latex("$d_{" & indexstr & ",1,1}^0$"), black, 0.5, false);
			edge_q_out_m[index] := new_default_edge(node_q[index], node_m_one[index]);
			edge_add_text_line(edge_q_out_m[index], 0, latex("$e_{" & indexstr & ",1,1}^1$"), black, 0.9, false);
			edge_q_out_d[index] := new_default_edge(node_q[index], node_d_zero[index]);
			edge_add_text_line(edge_q_out_d[index], 0, latex("$e_{" & indexstr & ",1,1}^0$"), black, 0.5, false);
		fi;
		edge_g_in[index] := new_default_edge(node_g[index], node_c[index]);
		edge_add_text_line(edge_g_in[index], 0, latex("$s_{" & indexstr & ",0}^1$"), black, 0.5, false);
		edge_g_out[index] := new_default_edge(node_g[index], node_f_zero[index]);
		edge_add_text_line(edge_g_out[index], 0, latex("$s_{" & indexstr & ",0}^0$"), black, 0.5, false);
		if (index < count):
			edge_s_in[index] := new_default_edge(node_s[index], node_u[index]);
			edge_add_text_line(edge_s_in[index], 0, latex("$s_{" & indexstr & ",1}^1$"), black, 0.5, false);
			edge_s_out[index] := new_default_edge(node_s[index], node_f_zero[index]);
			edge_add_text_line(edge_s_out[index], 0, latex("$s_{" & indexstr & ",1}^0$"), black, 0.5, false);
		fi;
		
		edge_path_curl(edge_b_in[index], 0, -0.3);
		edge_path_curl(edge_E_in_b[index], 0, -0.3);
		edge_path_curl(edge_a_in[index], 0, -0.3);
		edge_path_curl(edge_E_in_a[index], 0, -0.3);
		edge_path_curl(edge_E_out[index], 0, 0.3);
		edge_path_curl(edge_d_left[index], 0, 0.35);
		if (index < count):
			edge_path_curl(edge_d_right[index], 0, -0.35);
			edge_path_curl(edge_v_in[index], 0, -0.3);
			edge_path_curl(edge_w_in[index], 0, -0.3);
			edge_path_curl(edge_X_in_v[index], 0, -0.3);
			edge_path_curl(edge_X_in_w[index], 0, -0.3);
			edge_path_curl(edge_X_out[index], 0, -0.3);
		fi;
		
		if (not aspg) :
			edge_add_text_line(edge_E_out[index], 0, latex("$\varepsilon$"), black, 0.2, false);
			edge_add_text_line(edge_E_in_b[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.2, false);
			edge_add_text_line(edge_E_in_a[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.2, true);
			if (index < count):
				edge_add_text_line(edge_X_out[index], 0, latex("$\varepsilon$"), black, 0.2, false);
				edge_add_text_line(edge_X_in_w[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.2, false);
				edge_add_text_line(edge_X_in_v[index], 0, latex("$0.5(1{-}\varepsilon)$"), black, 0.2, true);
			fi;
		fi;

		coords_leave_base;
		group_leave;
	enddef;
		
	vardef build_sink(expr basey, count) =
		group_sink := group_add(group_root);
		group_enter(group_sink);	
		coords_enter_base((0,basey));
		
		node_f[count+1] := coords_register(0,0);
		
		node_t        := new_prio_node(8,0, "$t$", "1", "");

		edge_t      := new_default_edge(node_t, node_t);
			   	       edge_path_loop(edge_t, 0, 0.5, 0.75, 0);

		edge_f_in[count+1] := new_coord_to_edge(node_f[count+1], node_t);
					   
		coords_leave_base;
		group_leave;
	enddef;

	vardef connect_group_layer_with_next(expr layer, count) =
		
		if (layer < count):
			edge_u[layer] := new_default_edge(node_u[layer], node_d[layer+1]);
			edge_add_text_line(edge_u[layer], 0, latex("$h_{" & decimal(layer) & ",1}^0$"), black, 0.2, false);
			edge_path_curl(edge_u[layer], 0, -0.2);
			edge_c[layer] := new_edge_to_coord(node_c[layer], node_f[layer+2]);
			edge_add_text_line(edge_c[layer], 0, latex("$h_{" & decimal(layer) & ",0}^0$"), black, 0.2, false);
			edge_f_out[layer] := new_default_edge(node_f[layer], node_f[layer+1]);
			edge_add_text_line(edge_f_out[layer], 0, latex("$b_" & decimal(layer) & "^0$"), black, 0.03, false);
		else:
			edge_c[layer] := new_default_edge(node_c[layer], node_t);
			edge_add_text_line(edge_c[layer], 0, latex("$h_{" & decimal(layer) & ",0}^0$"), black, 0.5, false);
			edge_f_out[layer] := new_edge_to_coord(node_f[layer], node_f[layer+1]);
		fi;

	enddef;
	
	graph_reset;
	build_group_layer(0,1,3,0,1);
	build_group_layer(ypart group_get_coord_ur(group_layers[1]) + 1.75,2,3,0,2);
	build_group_layer(ypart group_get_coord_ur(group_layers[2]) + 1.75,3,3,0,3);
	build_sink(ypart group_get_coord_ur(group_layers[3]) + 1.75, 3);
	% group_separator_add(group_source, group_layers[1], style_separator, 0.10);
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
  
  
  if     (source="d") and (target="E"): edge_d_left[source_arr[0]+1]
  elseif (source="d") and (target="X"): edge_d_right[source_arr[0]+1]
  elseif (source="m") and (target="d"): edge_f_in[source_arr[0]+1]
  elseif (source="m") and (target="m"): edge_f_out[source_arr[0]+1]
  elseif (source="m") and (target="Y"): edge_f_out[source_arr[0]+1]
  elseif (source="b") and (target="E"): edge_b_in[source_arr[0]+1]
  elseif (source="b") and (target="p"): edge_b_out[source_arr[0]+1]
  elseif (source="p") and (target="m"): edge_p_out_m[source_arr[0]+1]
  elseif (source="p") and (target="d"): edge_p_out_d[source_arr[0]+1]
  elseif (source="v") and (target="X"): edge_v_in[source_arr[0]+1]
  elseif (source="v") and (target="r"): edge_v_out[source_arr[0]+1]
  elseif (source="r") and (target="m"): edge_r_out_m[source_arr[0]+1]
  elseif (source="r") and (target="d"): edge_r_out_d[source_arr[0]+1]
  elseif (source="E") and (target="b"): edge_E_in_b[source_arr[0]+1]
  elseif (source="E") and (target="a"): edge_E_in_a[source_arr[0]+1]
  elseif (source="E") and (target="g"): edge_E_out[source_arr[0]+1]
  elseif (source="X") and (target="v"): edge_X_in_v[source_arr[0]+1]
  elseif (source="X") and (target="w"): edge_X_in_w[source_arr[0]+1]
  elseif (source="X") and (target="s"): edge_X_out[source_arr[0]+1]
  elseif (source="a") and (target="E"): edge_a_in[source_arr[0]+1]
  elseif (source="a") and (target="o"): edge_a_out[source_arr[0]+1]
  elseif (source="o") and (target="m"): edge_o_out_m[source_arr[0]+1]
  elseif (source="o") and (target="d"): edge_o_out_d[source_arr[0]+1]
  elseif (source="w") and (target="X"): edge_w_in[source_arr[0]+1]
  elseif (source="w") and (target="q"): edge_w_out[source_arr[0]+1]
  elseif (source="q") and (target="m"): edge_q_out_m[source_arr[0]+1]
  elseif (source="q") and (target="d"): edge_q_out_d[source_arr[0]+1]
  elseif (source="g") and (target="c"): edge_g_in[source_arr[0]+1]
  elseif (source="g") and (target="m"): edge_g_out[source_arr[0]+1]
  elseif (source="s") and (target="u"): edge_s_in[source_arr[0]+1]
  elseif (source="s") and (target="m"): edge_s_out[source_arr[0]+1]
  else:
    -1
  fi
enddef;  

