numeric
	node_style_even,
	node_style_odd,
	node_style_prio,
	node_style_plain,
	edge_style_default,
	edge_style_strategy,
	edge_style_counter,
	edge_style_improving,
	edge_style_disabled,
	edge_style_discounter,
	node_style_pointer,
	style_separator;


node_style_even := node_style_add(primitive_style_add(white, path_style_default, primitive_circle), (1,1));
node_style_odd := node_style_add(primitive_style_add(white, path_style_default, primitive_rectangle), (1,1));
node_style_prio := node_style_add(primitive_style_add(white, path_style_default, primitive_diamond), (1,1));
node_style_plain := node_style_add(primitive_style_add(white, path_style_add(white, path_shape_solid, 0), primitive_circle), (1,1));
edge_style_default := path_style_add(black, path_shape_solid, 1);

edge_style_default := path_style_add(black, path_shape_solid, 1.75);
edge_style_strategy := path_style_add(blue, path_shape_solid, 2.5);
edge_style_counter := path_style_add(red, path_shape_solid, 2.5);
edge_style_improving := path_style_add(0.85green, path_shape_dashed, 1.75);
edge_style_disabled := path_style_add(0.3white, path_shape_dashed, 0.75);
edge_style_discounter := path_style_add(0.3white, path_shape_dashed, 0.75);
style_separator := path_style_add(black, path_shape_dashed, 0.75);
node_style_pointer := node_style_add(primitive_style_add(blue, path_style_add(blue, path_shape_dotted, 0.5), primitive_circle), (0.25, 0.25));


numeric % Groups
	group_source,
	group_sink,
	group_layers[];
	
numeric % Nodes
	node_B[],
	node_b[][],
	node_w[],
	node_u[],
	node_d[],
	node_y[],
	node_s,
	node_t;
	
	
numeric % Edges
	edge_B_b[],
	edge_B_y[],
	edge_b_b[][],
	edge_b_B[],
	edge_b_u[],
	edge_b_s[][],
	edge_w_d[],
	edge_w_w[],
	edge_w_t,
	edge_u_d[],
	edge_u_u[],
	edge_u_t,
	edge_d_B[],
	edge_y_w[],
	edge_s_d,
	edge_t_t,
	edge_coord_b_s_s[][],
	% edge_w_coord_w_y[],
	edge_y_w[],
	edge_b_coord_b_u[][],
	edge_coord_b_u_coord_b_u[],
	edge_coord_u_coord_uu;

numeric % Coords
	coord_w_y[],
	coord_b_s[][],
	coord_b_u[][],
	coord_u,
	coord_uu;
	

vardef build_graph =
	vardef new_even_node(expr x, y, txt, prio) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_even);
		node_add_text_line(n, latex(txt), black);
		node_add_text_line(n, latex(prio), blue);
		n
	enddef;

	vardef new_odd_node(expr x, y, txt, prio) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_odd);
		node_add_text_line(n, latex(txt), black);
		node_add_text_line(n, latex(prio), blue);
		n
	enddef;

	vardef new_prio_node(expr x, y, prio) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_prio);
		node_add_text_line(n, latex(prio), blue);
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
	vardef build_group_layer(expr basey, index, times_i, add_const) =
		group_layers[index] := group_add(group_root);
		group_enter(group_layers[index]);	
		coords_enter_base((0,basey));
		
		string indexstr;
		indexstr := i_string(times_i, add_const);	
		
		node_w[index] := new_even_node(0,0, "$w_" & indexstr & "$", "3");
		node_d[index] := new_prio_node(2,0, i_string(2 * times_i, 7 + 2 * add_const));
		node_u[index] := new_even_node(8,0, "$u_" & indexstr & "$", "3");
		node_B[index] := new_odd_node (2,2, "$B_" & indexstr & "$", "6");
		node_y[index] := new_prio_node(2,4.5, i_string(2 * times_i, 8 + 2 * add_const));

		coord_w_y[index] := coords_register(0,4.5);
		
		for i=0 upto index :
			node_b[index][i] := new_even_node(5,2+i*1.5, "$b_{" & indexstr & "," & decimal(i+1) & "}$", "5");
			coord_b_s[index][i] := coords_register(10,2.25+i*1.5);
			group_include_coord(group_current, coord_b_s[index][i]);
			coord_b_u[index][i] := coords_register(9,1.75+i*1.5);
			edge_b_s[index][i] := new_edge_to_coord(node_b[index][i], coord_b_s[index][i]);
							      edge_register_coord_(edge_b_s[index][i], coords_register(6,2.25+i*1.5));
			edge_b_coord_b_u[index][i] := new_edge_to_coord(node_b[index][i], coord_b_u[index][i]);
							   edge_register_coord_(edge_b_coord_b_u[index][i], coords_register(6,1.75+i*1.5));
			if (i > 0) :
				edge_b_b[index][i] := new_default_edge(node_b[index][i], node_b[index][i-1]);
			fi;
		endfor;
			
		edge_w_d[index] := new_default_edge(node_w[index], node_d[index]);
		edge_u_d[index] := new_default_edge(node_u[index], node_d[index]);
		edge_d_B[index] := new_default_edge(node_d[index], node_B[index]);
		edge_B_b[index] := new_default_edge(node_B[index], node_b[index][index]);
		edge_b_B[index] := new_default_edge(node_b[index][0], node_B[index]);
		if (index = 0) :
		   edge_path_curl(edge_B_b[index], 0, 0.35);
		   edge_path_curl(edge_b_B[index], 0, 0.35);
		fi;
		edge_B_y[index] := new_default_edge(node_B[index], node_y[index]);
		
		coords_leave_base;
		group_leave;
	enddef;
		
  	vardef build_source(expr basey) =
		group_source := group_add(group_root);
		group_enter(group_source);	
		coords_enter_base((0,basey));
		
		coord_b_s[-1][0] := coords_register(10,0);
		node_s        := new_prio_node(2,0, "8");
		coord_u		  := coords_register(9,1);
		coord_uu      := coords_register(8,1);
		group_include_coord(group_current, coord_u);
		edge_coord_b_s_s[-1] := new_coord_to_edge(coord_b_s[-1][0], node_s);
		edge_coord_u_coord_uu := new_coord_to_coord(coord_u, coord_uu);
		
		coords_leave_base;
		group_leave;
	enddef;

	vardef build_sink(expr basey,index) =
		group_sink := group_add(group_root);
		group_enter(group_sink);	
		coords_enter_base((0,basey));
		
		node_w[index] := new_even_node(0,0, "$w_{" & decimal(index + 1) & "}$", "3");
		node_u[index] := new_even_node(8,0, "$u_{" & decimal(index + 1) & "}$", "3");
		node_t        := new_prio_node(4,0, "1");
		
		edge_w_t      := new_default_edge(node_w[index], node_t);
		edge_u_t      := new_default_edge(node_u[index], node_t);
		edge_t_t      := new_default_edge(node_t, node_t);
						 edge_path_loop(edge_t_t, 0, 0.5, 0.75, 0);
						 
		coords_leave_base;
		group_leave;
	enddef;

	vardef connect_source_with_group_layer_one =
		edge_s_d := new_default_edge(node_s, node_d[0]);

		edge_coord_b_s_s[0] := new_coord_to_coord(coord_b_s[0][0], coord_b_s[-1][0]);
		edge_u_u[-1] := new_coord_to_edge(coord_uu, node_u[0]);
		edge_coord_b_u_coord_b_u[-1] := new_coord_to_coord(coord_b_u[0][0], coord_u);
	enddef;

	vardef connect_group_layer_with_next(expr layer) =
		edge_w_w[layer] := new_default_edge(node_w[layer], node_w[layer+1]);
		edge_coord_b_u_coord_b_u[layer] := new_coord_to_coord(coord_b_u[layer+1][layer+1], coord_b_u[layer][layer]);
		edge_y_w[layer] := new_default_edge(node_y[layer], node_w[layer+1]);
		edge_u_u[layer] := new_default_edge(node_u[layer], node_u[layer+1]);
		edge_coord_b_s_s[layer] := new_coord_to_coord(coord_b_s[layer+1][layer+1], coord_b_s[layer][layer]);
	enddef;

	vardef connect_sink_with_last_layer(expr layer) =
		edge_w_w[layer] := new_default_edge(node_w[layer], node_w[layer+1]);
		edge_y_w[layer] := new_default_edge(node_y[layer], node_w[layer+1]);
		edge_u_u[layer] := new_default_edge(node_u[layer], node_u[layer+1]);
	enddef;
	
	graph_reset;
	build_source(0);
	build_group_layer(ypart group_get_coord_ur(group_source) + 1.75,0,0,1);
	build_group_layer(ypart group_get_coord_ur(group_layers[0]) + 1.75,1,0,2);
	build_group_layer(ypart group_get_coord_ur(group_layers[1]) + 1.75,2,0,3);
	build_sink(ypart group_get_coord_ur(group_layers[2]) + 1.75, 3);
	group_separator_add(group_source, group_layers[0], style_separator, 0.10);
	group_separator_add(group_layers[0], group_layers[1], style_separator, 0.10);
	group_separator_add(group_layers[1], group_layers[2], style_separator, 0.10);
	group_separator_add(group_layers[2], group_sink, style_separator, 0.10);
	connect_source_with_group_layer_one;
	connect_group_layer_with_next(0);
	connect_group_layer_with_next(1);
	connect_sink_with_last_layer(2);
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
  
  
  
  if     (source="e") and (target="d"): edge_B_b[source_arr[0]]
  elseif (source="e") and (target="y"): edge_B_y[source_arr[0]]
  elseif (source="w") and (target="w"): edge_w_w[source_arr[0]]
  elseif (source="w") and (target="q"): edge_w_d[source_arr[0]]
  elseif (source="w") and (target="s"): edge_w_t
  elseif (source="u") and (target="u"): edge_u_u[source_arr[0]]
  elseif (source="u") and (target="q"): edge_u_d[source_arr[0]]
  elseif (source="u") and (target="s"): edge_u_t
  elseif (source="d") and (target="s"): edge_b_coord_b_u[source_arr[0]][source_arr[0]-source_arr[1]]
  elseif (source="d") and (target="e"): edge_b_B[source_arr[0]]
  elseif (source="d") and (target="d"): edge_b_b[source_arr[0]][source_arr[0]-source_arr[1]]
  elseif (source="d") and (target="p"): edge_b_s[source_arr[0]][source_arr[0]-source_arr[1]]
  elseif (source="d") and (target="u"): edge_b_coord_b_u[source_arr[0]][source_arr[0]-source_arr[1]]
  else:
    -1
  fi
enddef;  