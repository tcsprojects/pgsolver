amul = 2;
bmul = 2;
cmul = 4;
globnn = 3;


numeric
	node_style_even,
	node_style_even_small,
	node_style_odd,
	node_style_odd_small,
	node_style_group_even,
	node_style_group_odd,
	node_style_prio,
	node_style_prio_small,
	edge_style_default,
	edge_style_strategy,
	edge_style_counter,
	edge_style_improving,
	edge_style_disabled,
	edge_style_discounter,
	style_separator;

primitive_style_black_dashed_circle := primitive_style_add(white, path_style_add(black, path_shape_dashed, 1), primitive_circle);
primitive_style_black_dashed_rectangle := primitive_style_add(white, path_style_add(black, path_shape_dashed, 1), primitive_rectangle);

node_style_even := node_style_add(primitive_style_add(white, path_style_default, primitive_circle), (1,1));
node_style_even_small := node_style_add(primitive_style_black_dashed_circle, (0.75,0.75));
node_style_group_even := node_style_add(primitive_style_black_dashed_circle, (1.25,1.25));
node_style_group_odd := node_style_add(primitive_style_black_dashed_rectangle, (1.25,1.25));
node_style_odd := node_style_add(primitive_style_add(white, path_style_default, primitive_rectangle), (1,1));
node_style_odd_small := node_style_add(primitive_style_black_dashed_rectangle, (0.75, 0.75));
node_style_prio := node_style_add(primitive_style_add(white, path_style_default, primitive_rectangle), (1,1));
node_style_prio_small := node_style_add(primitive_style_black_dashed_rectangle, (0.75,0.75));
edge_style_default := path_style_add(black, path_shape_solid, 1.75);
edge_style_strategy := path_style_add(blue, path_shape_solid, 2.5);
edge_style_counter := path_style_add(red, path_shape_solid, 2.5);
edge_style_improving := path_style_add(0.85green, path_shape_dashed, 1.75);
edge_style_disabled := path_style_add(0.3white, path_shape_dashed, 0.75);
edge_style_discounter := path_style_add(0.3white, path_shape_dashed, 0.75);
style_separator := path_style_add(black, path_shape_dashed, 0.75);


numeric
	group_source,
	group_sink,
	group_layers[];

numeric
	node_L,
	node_Q,
	node_c[],
	node_q[],
	node_A[],
	node_a[],
	node_aa[][],
	node_k[],
	node_q_placeholder[],
	node_H[],
	node_e[],
	node_b[],
	node_bb[][],
	node_L_placeholder[],
	node_d[],
	node_dd[][],
	node_r[],
	node_F[],
	node_Q_placeholder[],
	node_M[],
	node_MM[][],
	node_G[],
	node_q_placeholder_top[],
	node_C[],
	node_W[],
	node_P[],
	node_PP[][],
	node_t,
	node_T;


numeric
	edge_L_to_c,
	edge_Q_to_q,
	edge_c_to_c[],
	edge_c_to_A[],
	edge_q_to_q[],
	edge_q_to_H,
	edge_A_to_a[],
	edge_A_to_H[],
	edge_a_to_A[],
	edge_a_to_k[],
	edge_k_to_q[],
	edge_H_to_e[],
	edge_e_to_b[],
	edge_e_to_r[],
	edge_r_to_d[],
	edge_e_to_C[],
	edge_b_to_e[],
	edge_b_to_L[],
	edge_b_to_e[][],
	edge_b_to_L[][],
	edge_d_to_e[],
	edge_r_to_F[],
	edge_d_to_M[],
	edge_dd_to_dd[]
	edge_dd_to_MM[][],
	edge_F_to_Q[],
	edge_F_to_W[],
	edge_M_to_G[],
	edge_MM_to_G[][],
	edge_M_to_P[],
	edge_MM_to_PP[],
	edge_G_to_q[],
	edge_C_to_c[],
	edge_W_to_W[],
	edge_W_to_d[],
	edge_P_to_P[],
	edge_PP_to_PP[][],
	edge_P_to_d[],
	edge_PP_to_dd[][],
	edge_T_to_T,
	edge_t_to_T,
	edge_C_to_t,
	edge_c_to_t,
	edge_q_to_t,
	edge_aa_to_k[][],
	edge_aa_to_aa[][],
	edge_bb_to_L[][],
	edge_bb_to_bb[][];



vardef build_graph(expr nn, aspg, expanda, expandb, expandc) =
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

	vardef new_odd_node_small(expr x, y, txt, priopg, priomdp) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_odd_small);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		n
	enddef;

	vardef new_even_multi_node(expr x, y, txt, priopg, priomdp, annot) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_group_even);
		n := node_register(coords_register(x,y), node_style_even);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		node_add_annot(n, annot, black, 125);
		n
	enddef;

	vardef new_odd_multi_node(expr x, y, txt, priopg, priomdp, annot) =
		numeric n;
		n := node_register(coords_register(x,y), node_style_group_odd);
		n := node_register(coords_register(x,y), node_style_odd);
		node_add_text_line(n, latex(txt), black);
		prio_annot(n, priopg, priomdp);
		node_add_annot(n, annot, black, 125);
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

	vardef connect_group_layer_with_next(expr layer) =
	  edge_c_to_c[layer] := new_default_edge(node_c[layer], node_c[layer+1]);
		edge_q_to_q[layer] := new_default_edge(node_q[layer], node_q[layer+1]);
		edge_C_to_c[layer] := new_default_edge(node_C[layer], node_c[layer+1]);
		if (layer < nn-1) :
		  edge_W_to_W[layer] := new_default_edge(node_W[layer+1], node_W[layer]);
		fi;
		if expandc:
		  for j=0 upto dn-1:
				if (layer < nn-1) :
					edge_PP_to_PP[layer][j] := new_default_edge(node_PP[layer+1][j], node_PP[layer][j]);
				fi;
				edge_MM_to_PP[layer][j] := new_default_edge(node_MM[layer+1][j], node_PP[layer][j]);
				edge_path_curl(edge_MM_to_PP[layer][j], 0, 0.2);
			endfor;
		else
			if (layer < nn-1) :
				edge_P_to_P[layer] := new_default_edge(node_P[layer+1], node_P[layer]);
			fi;
			edge_M_to_P[layer] := new_default_edge(node_M[layer+1], node_P[layer]);
			edge_path_curl(edge_M_to_P[layer], 0, 0.2);
		fi;
		edge_F_to_W[layer] := new_default_edge(node_F[layer+1], node_W[layer]);
		edge_path_curl(edge_F_to_W[layer], 0, 0.3);
		if (not aspg):
			edge_add_text_line(edge_F_to_W[layer], 0, latex("1/2"), red, 0.5, true);
			if expandc:
			  for j=0 upto dn-1:
				  edge_add_text_line(edge_MM_to_PP[layer][j], 0, latex("1/2"), red, 0.5, true);
					if (layer < nn-1) :
  					edge_add_text_line(edge_PP_to_PP[layer][j], 0, latex(decimal(layer-1+1) & "/" & decimal(layer+1)), red, 0.5, false);
					fi;
				endfor;
			else
				edge_add_text_line(edge_M_to_P[layer], 0, latex("1/2"), red, 0.5, true);
				if (layer < nn-1) :
					edge_add_text_line(edge_P_to_P[layer], 0, latex(decimal(layer-1+1) & "/" & decimal(layer+1)), red, 0.5, false);
				fi;
			fi;
			if (layer < nn-1) :
				edge_add_text_line(edge_W_to_W[layer], 0, latex(decimal(layer-1+1) & "/" & decimal(layer+1)), red, 0.5, false);
			fi;
		fi;
	enddef;

	vardef connect_source_with_groups =
    edge_L_to_c := new_default_edge(node_L, node_c[1]);
		edge_Q_to_q := new_default_edge(node_Q, node_q[1]);
	enddef;

	vardef connect_groups_with_sink =
    edge_C_to_t := new_default_edge(node_C[nn], node_t);
		edge_c_to_t := new_default_edge(node_c[nn], node_t);
		edge_path_curl(edge_c_to_t, 0, 0.5);
		edge_q_to_t := new_default_edge(node_q[nn], node_t);
		edge_path_curl(edge_q_to_t, 0, -0.5);
	enddef;

	numeric an, bn, dn, tt;
	an := amul*nn;
	bn := bmul*nn;
	dn := cmul*nn;
	tt := dn;

	a_add := 0;
	if expanda:
		a_add := 2 * (an-1);
	fi;
	b_add := 0;
	if expandb:
		if 8 + a_add < 2 * bn :
			b_add := 2 * bn - 8 - a_add;
		fi;
	fi;
	c_add := 0;
	if expandc:
		c_add := 2 * dn - 2;
	fi;

	vardef build_source =
		group_source := group_add(group_root);
		group_enter(group_source);
		coords_enter_base((0,0));

		node_L        := new_prio_node(0, 0, "$L$", decimal(2 * tt * nn + 2 * nn + 10), decimal(2 * tt * nn + 2 * nn + 10));
		node_Q		    := new_prio_node(24 + c_add,0, "$Q$", decimal(2 * tt * nn + 2 * nn + 8), decimal(2 * tt * nn + 2 * nn + 8));

		coords_leave_base;
		group_leave;
	enddef;

	vardef build_group_layer(expr basey, index, times_i, add_const) =
		group_layers[index] := group_add(group_root);
		group_enter(group_layers[index]);
		coords_enter_base((0,basey));

		string indexstr;
		indexstr := i_string(times_i, add_const);

		if expanda:
			for j = 0 upto an-1 :
				node_aa[index][j] := new_even_node(12, 2 * j, "$a_{" & indexstr & "," & decimal(j+1) & "}$", "5", "");
			endfor;
		else
			node_a[index] := new_even_multi_node(12, 0, "$a_" & indexstr & "$", "5", "", decimal(an));
		fi;
		if expandb:
			for j = 0 upto bn-1 :
				node_bb[index][j] := new_even_node(4, 8 + a_add + b_add - 2 * j, "$b_{" & indexstr & "," & decimal(+1j) & "}$", "5", "");
			endfor;
		else
			node_b[index] := new_even_multi_node(4, 8 + a_add + b_add, "$b_" & indexstr & "$", "5", "", decimal(bn));
		fi;
		if expandc:
			for j = 0 upto dn-1 :
				node_dd[index][j] := new_even_node(12, 8 + a_add + b_add + 2*(dn-1-j), "$d_{" & indexstr & "," & decimal(j+1) & "}$", "5", "");
				node_MM[index][j] := new_odd_node(14, 8 + a_add + b_add + 2 * (dn-1-j), "$M_{" & indexstr & "," & decimal(j+1) & "}$", decimal(2 * tt * index + 7 - 2 * j), decimal(2 * tt * index + 7 - 2*j));
				if (index < nn):
					node_PP[index][j] := new_odd_node(22 + 2*(dn-1-j), 12 + a_add + b_add + 2*(dn-1-j), "$P_{" & indexstr & "," & decimal(j+1) & "}$", "3", "");
				fi;
			endfor;
		else
			node_d[index] := new_even_multi_node(12, 8 + a_add + b_add, "$d_" & indexstr & "$", "5", "", decimal(dn));
  		node_M[index] := new_odd_multi_node(14, 8 + a_add + b_add, "$M_" & indexstr & "$", decimal(2 * tt * (index-1) + 7) & "+2*j", decimal(2 * tt * (index-1) + 7) & "+2*j", decimal(dn));
			if (index < nn):
				node_P[index] := new_odd_multi_node(22, 12 + a_add + b_add, "$P_" & indexstr & "$", "3", "", decimal(dn));
			fi;
		fi;


		node_c[index] := new_even_node(0, 0, "$c_" & indexstr & "$", "3", "");
		node_A[index] := new_odd_node(8, 0, "$A_" & indexstr & "$", "6", "");
		node_k[index] := new_prio_node(14, 0, "$k_" & indexstr & "$", decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 11), decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 11));
		node_q_placeholder[index] := new_even_node_small(16, 0, "$q_1$", "3", "");
		node_q[index] := new_even_node(24 + c_add, 2 + a_add, "$q_" & indexstr & "$", "3", "");
		node_H[index] := new_prio_node(8, 2 + a_add, "$H_" & indexstr & "$", decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 13), decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 13));
		node_e[index] := new_odd_node(8, 8 + a_add + b_add, "$e_" & indexstr & "$", "6", "");
		node_L_placeholder[index] := new_odd_node_small(2, 8 + a_add + b_add, "$L$", decimal(2 * tt * nn + 2 * nn + 10), decimal(2 * tt * nn + 2 * nn + 10));
		node_r[index] := new_even_node(12, 6 + a_add + b_add, "$r_" & indexstr & "$", "5", "");
		node_F[index] := new_odd_node(12, 4 + a_add + b_add, "$F_" & indexstr & "$", "7", "");
		node_Q_placeholder[index] := new_odd_node_small(10, 4 + a_add + b_add, "$Q$", decimal(2 * tt * nn + 2 * nn + 8), decimal(2 * tt * nn + 2 * nn + 8));
		node_G[index] := new_prio_node(16, 8 + a_add + b_add, "$G_" & indexstr & "$", decimal(2 * tt * nn + 2 * (index-1) + 8), decimal(2 * tt * nn + 2 * (index-1) + 8));
		node_q_placeholder_top[index] := new_even_node_small(18, 8 + a_add + b_add, "$q_1$", "3", "");
		node_C[index] := new_prio_node(8, 10 + a_add + b_add, "$C_" & indexstr & "$", decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 14), decimal(2 * tt * nn + 2 * nn + 4 * (index-1) + 14));
		if (index < nn):
			node_W[index] := new_odd_node(20, 10 + a_add + b_add, "$W_" & indexstr & "$", "3", "");
		fi;

		if expanda:
			edge_a_to_A[index] := new_default_edge(node_aa[index][0], node_A[index]);
			edge_A_to_a[index] := new_default_edge(node_A[index], node_aa[index][an-1]);
			for j = 0 upto an-1 :
				edge_aa_to_k[index][j] := new_default_edge(node_aa[index][j], node_k[index]);
				if j < an-1 :
					edge_aa_to_aa[index][j] := new_default_edge(node_aa[index][j+1], node_aa[index][j]);
				fi;
			endfor;
		else
			edge_a_to_A[index] := new_default_edge(node_a[index], node_A[index]);
			edge_A_to_a[index] := new_default_edge(node_A[index], node_a[index]);
			edge_path_curl(edge_a_to_A[index], 0, 0.25);
			edge_path_curl(edge_A_to_a[index], 0, 0.25);
			edge_a_to_k[index] := new_default_edge(node_a[index], node_k[index]);
		fi
		if expandb:
			edge_b_to_e[index] := new_default_edge(node_bb[index][0], node_e[index]);
			edge_e_to_b[index] := new_default_edge(node_e[index], node_bb[index][bn-1]);
			for j = 0 upto bn-1 :
				edge_bb_to_L[index][j] := new_default_edge(node_bb[index][j], node_L_placeholder[index]);
				if j < bn-1 :
					edge_bb_to_bb[index][j] := new_default_edge(node_bb[index][j+1], node_bb[index][j]);
				fi;
			endfor;
		else
			edge_e_to_b[index] := new_default_edge(node_e[index], node_b[index]);
			edge_b_to_e[index] := new_default_edge(node_b[index], node_e[index]);
			edge_path_curl(edge_e_to_b[index], 0, 0.25);
			edge_path_curl(edge_b_to_e[index], 0, 0.25);
			edge_b_to_L[index] := new_default_edge(node_b[index], node_L_placeholder[index]);
		fi
		if expandc:
			for j = 0 upto dn-1 :
			  edge_MM_to_G[index][j] := new_default_edge(node_MM[index][j], node_G[index]);
				edge_dd_to_MM[index][j] := new_default_edge(node_dd[index][j], node_MM[index][j]);
			endfor;
			for j = 0 upto dn-2 :
				edge_dd_to_dd[index][j] := new_default_edge(node_dd[index][j+1], node_dd[index][j]);
			endfor;
			edge_r_to_d[index] := new_default_edge(node_r[index], node_dd[index][dn-1]);
			edge_d_to_e[index] := new_default_edge(node_dd[index][0], node_e[index]);
		else
			edge_M_to_G[index] := new_default_edge(node_M[index], node_G[index]);
			edge_d_to_M[index] := new_default_edge(node_d[index], node_M[index]);
			edge_r_to_d[index] := new_default_edge(node_r[index], node_d[index]);
			edge_d_to_e[index] := new_default_edge(node_d[index], node_e[index]);
		fi;

		edge_e_to_r[index] := new_default_edge(node_e[index], node_r[index]);
		edge_r_to_F[index] := new_default_edge(node_r[index], node_F[index]);
		edge_c_to_A[index] := new_default_edge(node_c[index], node_A[index]);
		edge_k_to_q[index] := new_default_edge(node_k[index], node_q_placeholder[index]);
		edge_A_to_H[index] := new_default_edge(node_A[index], node_H[index]);
		edge_q_to_H[index] := new_default_edge(node_q[index], node_H[index]);
		edge_H_to_e[index] := new_default_edge(node_H[index], node_e[index]);
		edge_F_to_Q[index] := new_default_edge(node_F[index], node_Q_placeholder[index]);
		edge_G_to_q[index] := new_default_edge(node_G[index], node_q_placeholder_top[index]);
		edge_e_to_C[index] := new_default_edge(node_e[index], node_C[index]);
		if (index < nn) :
		  if expandc:
			  edge_W_to_d[index] := new_default_edge(node_W[index], node_dd[index][dn-1]);
				for j = 0 upto dn-1 :
  				edge_PP_to_dd[index][j] := new_default_edge(node_PP[index][j], node_dd[index][j]);
					edge_path_curl(edge_PP_to_dd[index][j], 0, -0.2);
				endfor;
			else
				edge_W_to_d[index] := new_default_edge(node_W[index], node_d[index]);
				edge_P_to_d[index] := new_default_edge(node_P[index], node_d[index]);
				edge_path_curl(edge_P_to_d[index], 0, -0.2);
			fi;
			edge_path_curl(edge_W_to_d[index], 0, -0.2);
		fi;



		if (not aspg):
			edge_add_text_line(edge_A_to_a[index], 0, latex("$1-\epsilon$"), red, 0.25, true);
			edge_add_text_line(edge_A_to_H[index], 0, latex("$\epsilon$"), red, 0.5, true);
			edge_add_text_line(edge_e_to_b[index], 0, latex("$1/2-1/2\epsilon$"), red, 0.25, true);
			edge_add_text_line(edge_e_to_r[index], 0, latex("$1/2-1/2\epsilon$"), red, 0.25, true);
			edge_add_text_line(edge_e_to_C[index], 0, latex("$\epsilon$"), red, 0.5, true);
			if (index > 1):
				if expandc:
					for j = 0 upto dn-1 :
						edge_add_text_line(edge_MM_to_G[index][j], 0, latex("1/2"), red, 0.5, false);
						if (index < nn) :
							edge_add_text_line(edge_PP_to_dd[index][j], 0, latex("1/" & decimal(index)), red, 0.5, false);
						fi;
					endfor;
				else
					edge_add_text_line(edge_M_to_G[index], 0, latex("1/2"), red, 0.5, false);
					if (index < nn) :
						edge_add_text_line(edge_P_to_d[index], 0, latex("1/" & decimal(index)), red, 0.5, false);
					fi;
				fi;
				edge_add_text_line(edge_F_to_Q[index], 0, latex("1/2"), red, 0.5, true);
				if (index < nn) :
					edge_add_text_line(edge_W_to_d[index], 0, latex("1/" & decimal(index)), red, 0.5, false);
				fi;
			fi;
		fi;

		coords_leave_base;
		group_leave;
  enddef;

	vardef build_sink(expr basey) =
		group_sink := group_add(group_root);
		group_enter(group_sink);
		coords_enter_base((0,basey));
		node_T        := new_prio_node(8,2, "$T$", "1", "");
		node_t	  	  := new_prio_node(8,0,"$t$", decimal(2 * tt * nn + 6 * nn + 12), "0");

		edge_T_to_T   := new_default_edge(node_T, node_T);
			   	           edge_path_loop(edge_T_to_T, 0, 0.5, 0.75, 0);
    edge_t_to_T   := new_default_edge(node_t, node_T);

		coords_leave_base;
		group_leave;
	enddef;

	graph_reset;
	build_source;
	build_group_layer(2, 1, 0, 1);
	for i = 2 upto nn :
		build_group_layer(ypart group_get_coord_ur(group_layers[i-1]) + 2, i, 0, i);
	endfor;
	build_sink(ypart group_get_coord_ur(group_layers[nn]) + 2);

  connect_source_with_groups;
	for i = 1 upto nn - 1 :
		connect_group_layer_with_next(i);
	endfor;
	connect_groups_with_sink;

	group_separator_add(group_source, group_layers[1], style_separator, 0.10);
	for i = 1 upto nn-1 :
		group_separator_add(group_layers[i], group_layers[i+1], style_separator, 0.10);
	endfor;
  group_separator_add(group_layers[nn], group_sink, style_separator, 0.10);
enddef;




vardef edge_translate(expr source, source_stack, target, target_stack) =
	nn := globnn;

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


	if (source="c"):
		if (target="A"): edge_c_to_A[source_arr[0]+1]
	  elseif (source_arr[0] + 1 < nn): edge_c_to_c[source_arr[0]+1]
		elseif (source_arr[0] + 1 = nn): edge_c_to_t
		else: -1
		fi
	elseif (source="q"):
		if (target="H"): edge_q_to_H[source_arr[0]+1]
	  elseif (source_arr[0] + 1 < nn): edge_q_to_q[source_arr[0]+1]
		elseif (source_arr[0] + 1 = nn): edge_q_to_t
		else: -1
		fi
	elseif (source="L"): edge_L_to_c
	elseif (source="Q"): edge_Q_to_q
	elseif (source="t"): edge_t_to_T
	elseif (source="T"): edge_T_to_T
	elseif (source="H"): edge_H_to_e[source_arr[0]+1]
	elseif (source="G"): edge_G_to_q[source_arr[0]+1]
	elseif (source="K"): edge_k_to_q[source_arr[0]+1]
	elseif (source="A"):
		if (target="H"): edge_A_to_H[source_arr[0]+1]
		else: edge_A_to_a[source_arr[0]+1]
		fi
	elseif (source="a"):
		if (target="A"): edge_a_to_A[source_arr[0]+1]
		elseif (target="a"): edge_aa_to_aa[source_arr[0]+1][source_arr[1]-1]
		else: edge_aa_to_k[source_arr[0]+1][source_arr[1]]
		fi
	elseif (source="b"):
		if (target="e"): edge_b_to_e[source_arr[0]+1]
		elseif (target="b"): edge_bb_to_bb[source_arr[0]+1][source_arr[1] -1]
		else: edge_bb_to_L[source_arr[0]+1][source_arr[1]]
		fi
	elseif (source="r"):
		if (target="d"): edge_r_to_d[source_arr[0]+1]
		else: edge_r_to_F[source_arr[0]+1]
		fi
	elseif (source="d"):
		if (target="e"): edge_d_to_e[source_arr[0]+1]
		elseif (target="d"): edge_dd_to_dd[source_arr[0]+1][source_arr[1] -1]
		else: edge_dd_to_MM[source_arr[0]+1][source_arr[1]]
		fi
	elseif (source="M"):
		if (target="P"): edge_MM_to_PP[source_arr[0]][source_arr[1]]
		else: edge_MM_to_G[source_arr[0]+1][source_arr[1]]
		fi
	elseif (source="P"):
		if (target="P"): edge_PP_to_PP[source_arr[0]][source_arr[1]]
		else: edge_PP_to_dd[source_arr[0]+1][source_arr[1]]
		fi
	elseif (source="W"):
		if (target="W"): edge_W_to_W[source_arr[0]]
		else: edge_W_to_d[source_arr[0]+1]
		fi
	elseif (source="e"):
		if (target="C"): edge_e_to_C[source_arr[0]+1]
		elseif (target="r"): edge_e_to_r[source_arr[0]+1]
		else: edge_e_to_b[source_arr[0]+1]
		fi
	elseif (source="F"):
		if (target="Q"): edge_F_to_Q[source_arr[0]+1]
		else: edge_F_to_W[source_arr[0]]
		fi
	elseif (source="C"):
		if (source_arr[0] + 1 < nn): edge_C_to_c[source_arr[0]+1]
		elseif (source_arr[0] + 1 = nn): edge_C_to_t
		else: -1
		fi
	else:
		-1
	fi
enddef;
