input ./metapostgraphs/graphicslib
input ./metapostgraphs/graphlib


numeric
	node_style_even,
	node_style_odd,
	node_style_prio,
	node_style_plain,
	edge_style_default;
	
node_style_even := node_style_add(primitive_style_add(white, path_style_default, primitive_circle), (1,1));
node_style_odd := node_style_add(primitive_style_add(white, path_style_default, primitive_rectangle), (1,1));
node_style_prio := node_style_add(primitive_style_add(white, path_style_default, primitive_diamond), (1,1));
node_style_plain := node_style_add(primitive_style_add(white, path_style_add(black, path_shape_dashed, 1.75), primitive_diamond), (0.75,0.75));
edge_style_default := path_style_add(black, path_shape_solid, 1);

edge_style_default := path_style_add(black, path_shape_solid, 1.75);
edge_style_strategy := path_style_add(blue, path_shape_solid, 2.5);
edge_style_counter := path_style_add(red, path_shape_solid, 2.5);
edge_style_improving := path_style_add(0.85green, path_shape_dashed, 1.75);
edge_style_disabled := path_style_add(0.3white, path_shape_dashed, 0.75);
edge_style_discounter := path_style_add(0.3white, path_shape_dashed, 0.75);


numeric % Groups
	group_deceleration_lane,
	group_cycle_gates[],
	group_source_sink;


	
numeric % Nodes
	node_lane_t[],
	node_lane_c,
	node_lane_a[],
	node_cycle_d[],
	node_cycle_b[],
	node_cycle_e[],
	node_cycle_f[],
	node_cycle_g[],
	node_cycle_h[],
	node_cycle_k[],
	node_cycle_m[],
	node_cycle_q[],
	node_cycle_p[][],
	node_cycle_y[][],
	node_cycle_z[][],
	node_cycle_u[][],
	node_cycle_v[][],
	node_cycle_vm[][],
	node_cycle_w[][],
	node_cycle_wq[][],
	node_source_r,
	node_source_s,
	node_sink_x;
	
	
	
numeric % Edges
	edge_lane_t_c,
	edge_lane_a_t[],
	edge_lane_t_t[],
	edge_cycle_f_e[],
	edge_cycle_g_f[],
	edge_cycle_g_k[],
	edge_cycle_h_k[],
	edge_cycle_e_h[],
	edge_cycle_m_e[],
	edge_cycle_q_h[],
	edge_cycle_b_d[],
	edge_cycle_e_b[],
	edge_cycle_d_to[],
	edge_cycle_v_to[][],
	edge_cycle_w_to[][],
	edge_cycle_u_v[][],
	edge_cycle_u_w[][],
	edge_cycle_u_p[][],
	edge_cycle_p_y[][],
	edge_cycle_p_z[][],
	edge_cycle_p_out[][],
	edge_cycle_y_w[][],
	edge_cycle_z_v[][],
	edge_sink_x_x,
	edge_source_r_x,
	edge_source_s_x,
	edge_cycle_k_g[][],
	edge_cycle_k_x[],
	edge_cycle_v_m[][],
	edge_cycle_w_q[][],
	edge_cycle_d_a[][],
	edge_cycle_b_a[][],
	edge_source_s_f[],
	edge_source_r_g[],
	edge_cycle_d_coord_d_s[],
	edge_cycle_d_coord_d_r[],
	edge_cycle_b_coord_b_s[],
	edge_cycle_b_coord_b_r[],
	edge_cycle_coord_b_d_r[],
	edge_cycle_coord_d_b_r[],
	edge_cycle_coord_b_d_s[],
	edge_cycle_coord_d_b_s[],
	edge_lane_t_coord_t_s[],
	edge_lane_t_coord_t_r[],
	edge_lane_coord_t_r[],
	edge_lane_coord_t_s[],
	edge_lane_source_r,
	edge_lane_source_s,
	edge_cycle_source_s,
	edge_cycle_source_r,
	edge_con_source_s,
	edge_con_source_r,
	edge_con_source_sx,
	edge_con_source_rx;

	
	
numeric % Coords
	coord_cycle_v_tar[][],
	coord_cycle_w_tar[][],
	coord_source_d_s,
	coord_source_d_r,
	coord_source_t_s,
	coord_source_t_r,
	coord_cycle_d_s[],
	coord_cycle_d_r[],
	coord_cycle_b_s[],
	coord_cycle_b_r[],
	coord_lane_t_s[],
	coord_lane_t_r[];


	
vardef build_graph(expr fearnleygame, bits) =

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

	vardef new_prio_node(expr x, y, txt, prio) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_prio);
		node_add_text_line(n, latex(txt), black);
		node_add_text_line(n, latex(prio), blue);
		n
	enddef;

	vardef new_plain_node(expr x, y, txt) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_plain);
		node_add_text_line(n, latex(txt), black);
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

	vardef new_edge_coord_to(expr coord, node) =
		edge_register(coord, node_get_coord(node), edge_style_default)
	enddef;

	vardef new_edge_coord_to_coord(expr coords, coord) =
		numeric e;
		e := edge_register(coords, coord, edge_style_default);
		edge_set_arrow_style(e, 0, path_arrow_sep);
		e
	enddef;

	vardef build_deceleration_lane(expr base) =
		group_deceleration_lane := group_add(group_root);
		group_enter(group_deceleration_lane);	
		coords_enter_base(base);
		
		node_lane_c		  := new_even_node(0,0,"$c$",decimal (10*bits+6));
		
		coord_cycle_t_s[0] := coords_register(-2.5,0+1);
		edge_lane_t_coord_t_s[0] := new_edge_to_coord(node_lane_c, coord_cycle_t_s[0]);
		coord_cycle_t_r[0] := coords_register(-1.25,0+1);
		edge_lane_t_coord_t_r[0] := new_edge_to_coord(node_lane_c, coord_cycle_t_r[0]);

		for i=1 upto 3 * bits :
			node_lane_t[i] := new_even_node(0,2 * i+0.5,"$t_" & decimal i & "$", decimal (4 * bits + 2 * i + 3));
			node_lane_a[i] := new_prio_node(2,2 * i+0.5,"$a_" & decimal i & "$", decimal (4 * bits + 2 * i + 4));
			coord_lane_high[i] := coords_register(3 + (2*bits-i) * 0.5, 6 * bits);
			coord_lane_mid[i]  := coords_register(3 + (2*bits-i) * 0.5, 2 * i+0.5);
			if (i mod 2 = 1) :
				coord_lane_low[i] := coords_register(3 + (2*bits-i) * 0.5, 2 * i+0.5);
			else :
				coord_lane_low[i] := coords_register(3 + (2*bits-i) * 0.5, 2 * i - 2+0.5);
			fi;

			coord_cycle_t_s[i] := coords_register(-2.5,2 * i+1+0.5);
			edge_lane_t_coord_t_s[i] := new_edge_to_coord(node_lane_t[i], coord_cycle_t_s[i]);
			coord_cycle_t_r[i] := coords_register(-1.25,2 * i+1+0.5);
			edge_lane_t_coord_t_r[i] := new_edge_to_coord(node_lane_t[i], coord_cycle_t_r[i]);
			edge_lane_coord_t_r[i-1] := new_edge_coord_to_coord(coord_cycle_t_r[i-1], coord_cycle_t_r[i]);
			edge_lane_coord_t_s[i-1] := new_edge_coord_to_coord(coord_cycle_t_s[i-1], coord_cycle_t_s[i]);
		endfor;
		
		edge_lane_t_c := new_default_edge(node_lane_t[1], node_lane_c);
		
		for i=1 upto 3 * bits :
			edge_lane_a_t[i] := new_default_edge(node_lane_a[i], node_lane_t[i]);
		endfor;
		
		for i=2 upto 3 * bits :
			edge_lane_t_t[i] := new_default_edge(node_lane_t[i], node_lane_t[i-1]);
		endfor;

		coord_source_t_s := coords_register(-2.5, 6 * bits+6);
		coord_source_t_r := coords_register(-1.25, 6 * bits+4);
		edge_lane_source_s := new_edge_coord_to_coord(coord_cycle_t_s[3*bits], coord_source_t_s);
		edge_lane_source_r := new_edge_coord_to_coord(coord_cycle_t_r[3*bits], coord_source_t_r);

		
		coords_leave_base;
		group_leave;
	enddef;

	vardef build_cycle_gate(expr index, base) =
		group_cycle_gate[index] := group_add(group_root);
		group_enter(group_cycle_gate[index]);	
		coords_enter_base(base);
		
		node_cycle_b[index]	  := new_even_node(-2,-0.5,"$b_" & decimal index & "$",decimal (4*index-1));
		node_cycle_d[index]	  := new_even_node(-2,2.5,"$d_" & decimal index & "$",decimal (4*index-1));
		
		coord_cycle_b_s[index] := coords_register(0.5,1);
		edge_cycle_b_coord_b_s[index] := new_edge_to_coord(node_cycle_b[index], coord_cycle_b_s[index]);
		coord_cycle_b_r[index] := coords_register(-0.75,1);
		edge_cycle_b_coord_b_r[index] := new_edge_to_coord(node_cycle_b[index], coord_cycle_b_r[index]);
		coord_cycle_d_s[index] := coords_register(0.5,4);
		edge_cycle_d_coord_d_s[index] := new_edge_to_coord(node_cycle_d[index], coord_cycle_d_s[index]);
		coord_cycle_d_r[index] := coords_register(-0.75,4);
		edge_cycle_d_coord_d_r[index] := new_edge_to_coord(node_cycle_d[index], coord_cycle_d_r[index]);
		edge_cycle_coord_b_d_r[index] := new_edge_coord_to_coord(coord_cycle_b_r[index], coord_cycle_d_r[index]);
		edge_cycle_coord_b_d_s[index] := new_edge_coord_to_coord(coord_cycle_b_s[index], coord_cycle_d_s[index]);
		
		if fearnleygame :
			for i = 1 upto index - 1 :
				node_cycle_u[index][i]	  := new_even_node(2 + (bits - i - 1) * 6,2.5,"$u_{" & decimal index & "," & decimal i & "}$",decimal 3);
				node_cycle_v[index][i]	  := new_odd_node(7 + (bits - i - 1) * 6,4.5,"$v_{" & decimal index & "," & decimal i & "}$",decimal 3);
				node_cycle_vm[index][i]	  := new_plain_node(7 + (bits - i - 1) * 6,0,"$m_{" & decimal i & "}$");
				node_cycle_w[index][i]	  := new_odd_node(6 + (bits - i - 1) * 6,1.25,"$w_{" & decimal index & "," & decimal i & "}$",decimal 2);
				node_cycle_wq[index][i]	  := new_plain_node(6 + (bits - i - 1) * 6,0,"$q_{" & decimal i & "}$");
				node_cycle_p[index][i]	  := new_even_node(4 + (bits - i - 1) * 6,2.5,"$p_{" & decimal index & "," & decimal i & "}$",decimal 5);
				node_cycle_y[index][i]	  := new_prio_node(4.5 + (bits - i - 1) * 6,1.25,"$y_{" & decimal index & "," & decimal i & "}$",decimal 4);
				node_cycle_z[index][i]	  := new_prio_node(5.5 + (bits - i - 1) * 6,4.5,"$z_{" & decimal index & "," & decimal i & "}$",decimal 4);
			endfor;
			for i = index + 1 upto bits :
				node_cycle_u[index][i]	  := new_even_node(2 + (bits - i) * 6,2.5,"$u_{" & decimal index & "," & decimal i & "}$",decimal 3);
				node_cycle_v[index][i]	  := new_odd_node(4 + (bits - i) * 6,3,"$v_{" & decimal index & "," & decimal i & "}$",decimal 3);
				node_cycle_vm[index][i]	  := new_plain_node(4 + (bits - i) * 6,4.5,"$m_{" & decimal i & "}$");
				node_cycle_w[index][i]	  := new_odd_node(6 + (bits - i) * 6,2,"$w_{" & decimal index & "," & decimal i & "}$",decimal 2);
				node_cycle_wq[index][i]	  := new_plain_node(6 + (bits - i) * 6,4.5,"$q_{" & decimal i & "}$");
			endfor;
			node_cycle_m[index]	:= new_prio_node(4 + (bits - 1) * 6 ,0.5,"$m_" & decimal index & "$",decimal (10*bits+2*index+9));
			node_cycle_q[index]	:= new_prio_node(6 + (bits - 1) * 6 ,0.5,"$q_" & decimal index & "$",decimal (12*bits+6*index+9));
		fi;
		
		node_cycle_e[index]	:= new_odd_node(4 + (bits-1) * 6 ,2.5,"$e_" & decimal index & "$",decimal (4*index+2));
		node_cycle_h[index]	:= new_prio_node(6 + (bits-1) * 6 ,2.5,"$h_" & decimal index & "$",decimal (12*bits+6*index+10));
		node_cycle_f[index]	:= new_prio_node(4 + (bits-1) * 6 ,4.5,"$f_" & decimal index & "$",decimal (12*bits+6*index+7));
		node_cycle_g[index]	:= new_even_node(6 + (bits-1) * 6 ,4.5,"$g_" & decimal index & "$",decimal (4*index+4));
		node_cycle_k[index]	:= new_even_node(8 + (bits-1) * 6 ,4.5,"$k_" & decimal index & "$",decimal (12*bits+6*index+5));
		
		edge_cycle_f_e[index] := new_default_edge(node_cycle_f[index], node_cycle_e[index]);
		edge_cycle_g_f[index] := new_default_edge(node_cycle_g[index], node_cycle_f[index]);
		edge_cycle_g_k[index] := new_default_edge(node_cycle_g[index], node_cycle_k[index]);
		edge_cycle_h_k[index] := new_default_edge(node_cycle_h[index], node_cycle_k[index]);
		edge_cycle_e_h[index] := new_default_edge(node_cycle_e[index], node_cycle_h[index]);

		edge_cycle_b_d[index] := new_default_edge(node_cycle_b[index], node_cycle_d[index]);
		edge_cycle_e_b[index] := new_default_edge(node_cycle_e[index], node_cycle_b[index]);
								 edge_register_coord_(edge_cycle_e_b[index], coords_register(2 + (bits - 1) * 6,-0.5));

		if fearnleygame :					 
			edge_cycle_m_e[index] := new_default_edge(node_cycle_m[index], node_cycle_e[index]);
			edge_cycle_q_h[index] := new_default_edge(node_cycle_q[index], node_cycle_h[index]);
			
			for i = 1 upto index - 1 :
				edge_cycle_u_v[index][i] := new_default_edge(node_cycle_u[index][i], node_cycle_v[index][i]);
				edge_cycle_u_w[index][i] := new_default_edge(node_cycle_u[index][i], node_cycle_w[index][i]);
				numeric tmp;
				if (i = 1) :
					tmp := node_cycle_e[index];
				else :
					tmp := node_cycle_u[index][i-1];
				fi;
				edge_cycle_v_to[index][i] := new_default_edge(node_cycle_v[index][i], tmp);
				edge_cycle_w_to[index][i] := new_default_edge(node_cycle_w[index][i], tmp);			

				edge_cycle_v_m[index][i] := new_default_edge(node_cycle_v[index][i], node_cycle_vm[index][i]);
				edge_cycle_w_q[index][i] := new_default_edge(node_cycle_w[index][i], node_cycle_wq[index][i]);

				edge_cycle_u_p[index][i] := new_default_edge(node_cycle_u[index][i], node_cycle_p[index][i]);
				edge_cycle_p_y[index][i] := new_default_edge(node_cycle_p[index][i], node_cycle_y[index][i]);
				edge_cycle_p_z[index][i] := new_default_edge(node_cycle_p[index][i], node_cycle_z[index][i]);
				edge_cycle_p_out[index][i] := new_default_edge(node_cycle_p[index][i], tmp);
				edge_cycle_y_w[index][i] := new_default_edge(node_cycle_y[index][i], node_cycle_w[index][i]);
				edge_cycle_z_v[index][i] := new_default_edge(node_cycle_z[index][i], node_cycle_v[index][i]);
			endfor;

			for i = index + 1 upto bits :
				edge_cycle_u_v[index][i] := new_default_edge(node_cycle_u[index][i], node_cycle_v[index][i]);
				edge_cycle_u_w[index][i] := new_default_edge(node_cycle_u[index][i], node_cycle_w[index][i]);
				numeric tmp;
				if (i = 2) :
					tmp := node_cycle_e[index];
				else :
					if (i = index + 1) :
						tmp := node_cycle_u[index][i-2];
					else :
						tmp := node_cycle_u[index][i-1];
					fi;
				fi;
				edge_cycle_v_to[index][i] := new_default_edge(node_cycle_v[index][i], tmp);
				edge_cycle_w_to[index][i] := new_default_edge(node_cycle_w[index][i], tmp);			

				edge_cycle_v_m[index][i] := new_default_edge(node_cycle_v[index][i], node_cycle_vm[index][i]);
				edge_cycle_w_q[index][i] := new_default_edge(node_cycle_w[index][i], node_cycle_wq[index][i]);
			endfor;
									 
			if (bits = 1) :
				edge_cycle_d_to[index] := new_default_edge(node_cycle_d[index], node_cycle_e[index]);
			else :
				if (index = bits) :
					edge_cycle_d_to[index] := new_default_edge(node_cycle_d[index], node_cycle_u[index][bits-1]);
				else :
					edge_cycle_d_to[index] := new_default_edge(node_cycle_d[index], node_cycle_u[index][bits]);
				fi;
			fi;
		else :
			edge_cycle_d_to[index] := new_default_edge(node_cycle_d[index], node_cycle_e[index]);
		fi;
		
		if (index = bits) :
			coord_source_d_s := coords_register(0.5, 8.5);
			coord_source_d_r := coords_register(-0.75, 6.5);
			edge_cycle_source_s := new_edge_coord_to_coord(coord_cycle_d_s[index], coord_source_d_s);
			edge_cycle_source_r := new_edge_coord_to_coord(coord_cycle_d_r[index], coord_source_d_r);
		fi;
		
		coords_leave_base;
		group_leave;
	enddef;
	
	vardef build_source_sink(expr base) =
		group_source_sink := group_add(group_root);
		group_enter(group_source_sink);	
		coords_enter_base(base);
		
		node_source_s	  := new_even_node(4,0,"$s$",decimal (10*bits+8));
		node_source_r	  := new_even_node(4,-2,"$r$",decimal (10*bits+10));
		node_sink_x		  := new_prio_node(6,-4,"$x$",decimal 1);
		
		edge_sink_x_x := new_default_edge(node_sink_x, node_sink_x);
						 edge_path_loop(edge_sink_x_x, 0, 0.65, 0.75, 25);
						 
		edge_source_r_x := new_default_edge(node_source_r, node_sink_x);
		edge_source_s_x := new_default_edge(node_source_s, node_sink_x);
		
		coords_leave_base;
		group_leave;
	enddef;
	
	vardef connect_cycle_gate_layers(expr index) =
		for i = index + 1 upto bits :
			edge_cycle_k_g[index][i] := new_default_edge(node_cycle_k[index], node_cycle_g[i]);
			if i > index + 1 :
				edge_path_curl(edge_cycle_k_g[index][i], 0, -0.3);
			fi;
		endfor;
		
		edge_cycle_k_x[index] := new_default_edge(node_cycle_k[index], node_sink_x);

		for i = 1 upto 3 * index :
			edge_cycle_d_a[index][i] := new_default_edge(node_cycle_d[index], node_lane_a[i]);
			edge_cycle_b_a[index][i] := new_default_edge(node_cycle_b[index], node_lane_a[i]);
		endfor;
		
		if (index < bits) :
			edge_cycle_coord_d_b_r[index] := new_edge_coord_to_coord(coord_cycle_d_r[index], coord_cycle_b_r[index+1]);
			edge_cycle_coord_d_b_s[index] := new_edge_coord_to_coord(coord_cycle_d_s[index], coord_cycle_b_s[index+1]);
		fi;
	enddef;
	
	vardef connect_source_sink_layer =
		for i = 1 upto bits :
			edge_source_s_f[i] := new_default_edge(node_source_s, node_cycle_f[i]);
			edge_path_curl(edge_source_s_f[i], 0, 0.25);
			edge_source_r_g[i] := new_default_edge(node_source_r, node_cycle_g[i]);
			edge_path_curl(edge_source_r_g[i], 0, 0.25);
		endfor;
		
		edge_con_source_s := new_edge_coord_to_coord(coord_source_t_s, coord_source_d_s);
		edge_con_source_r := new_edge_coord_to_coord(coord_source_t_r, coord_source_d_r);
		edge_con_source_sx := new_edge_coord_to(coord_source_d_s, node_source_s);
		edge_con_source_rx := new_edge_coord_to(coord_source_d_r, node_source_r);
	enddef;
	
	graph_reset;
	
	build_deceleration_lane((0,0));
	for i = 1 upto bits :
		build_cycle_gate(i, (6 + bits, 6 * i - 2.5));
	endfor;
	build_source_sink((7 * bits + 6, 6 * bits + 6));
	
	for i = bits downto 1 :
		connect_cycle_gate_layers(i);
	endfor;
	
	connect_source_sink_layer;
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
    
  if     (source="c") and (target="s"): edge_lane_t_coord_t_s[0]
  elseif (source="c") and (target="r"): edge_lane_t_coord_t_r[0]
  elseif (source="t") and (target="c"): edge_lane_t_c
  elseif (source="t") and (target="t"): edge_lane_t_t[source_arr[0]+1]
  elseif (source="t") and (target="s"): edge_lane_t_coord_t_s[source_arr[0]+1]
  elseif (source="t") and (target="r"): edge_lane_t_coord_t_r[source_arr[0]+1]

  elseif (source="g") and (target="f"): edge_cycle_g_f[source_arr[0]+1]
  elseif (source="g") and (target="k"): edge_cycle_g_k[source_arr[0]+1]
  elseif (source="e") and (target="d"): edge_cycle_e_b[source_arr[0]+1]
  elseif (source="e") and (target="h"): edge_cycle_e_h[source_arr[0]+1]
  elseif (source="k") and (target="z"): edge_cycle_k_x[source_arr[0]+1]
  elseif (source="k") and (target="g"): edge_cycle_k_g[source_arr[0]+1][target_arr[0]+1]
  elseif (source="r") and (target="z"): edge_source_r_x
  elseif (source="r") and (target="g"): edge_source_r_g[target_arr[0]+1]
  elseif (source="s") and (target="z"): edge_source_s_x
  elseif (source="s") and (target="f"): edge_source_s_f[target_arr[0]+1]
  elseif (source="u") and (target="v"): edge_cycle_u_v[source_arr[0]+1][source_arr[1]+1]
  elseif (source="u") and (target="w"): edge_cycle_u_w[source_arr[0]+1][source_arr[1]+1]
  
  elseif (source="u") and (target="b"): edge_cycle_u_p[source_arr[0]+1][source_arr[1]+1]
  elseif (source="b") and (target="l"): edge_cycle_p_z[source_arr[0]+1][source_arr[1]+1]
  elseif (source="b") and (target="p"): edge_cycle_p_y[source_arr[0]+1][source_arr[1]+1]
  elseif (source="b"): edge_cycle_p_out[source_arr[0]+1][source_arr[1]+1]
  
  elseif (source="v") and (target="m"): edge_cycle_v_m[source_arr[0]+1][source_arr[1]+1]
  elseif (source="v")                 : edge_cycle_v_to[source_arr[0]+1][source_arr[1]+1]
  elseif (source="w") and (target="q"): edge_cycle_w_q[source_arr[0]+1][source_arr[1]+1]
  elseif (source="w")                 : edge_cycle_w_to[source_arr[0]+1][source_arr[1]+1]
  elseif (source="d") and (target="d"): edge_cycle_b_d[source_arr[0]+1]
  elseif (source="d") and (target="a"):
  	if (source_arr[1]=1): edge_cycle_b_a[source_arr[0]+1][target_arr[0]+1]
  	else                : edge_cycle_d_a[source_arr[0]+1][target_arr[0]+1]
  	fi
  elseif (source="d") and (target="r"):
    if (source_arr[1]=1): edge_cycle_b_coord_b_r[source_arr[0]+1]
    else                : edge_cycle_d_coord_d_r[source_arr[0]+1]
    fi
  elseif (source="d") and (target="s"):
    if (source_arr[1]=1): edge_cycle_b_coord_b_s[source_arr[0]+1]
    else                : edge_cycle_d_coord_d_s[source_arr[0]+1]
    fi
  elseif (source="d")                 : edge_cycle_d_to[source_arr[0]+1]
  else:
    -1
  fi
enddef;

