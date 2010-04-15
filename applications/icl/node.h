/******************************************************************************
 *
 *		N O D E . H	Interface to node.c
 *
 *		Created :	S.K.Robinson	19/12/92
 *		Edited :	I.R.Jenkins	02/06/92
 *				moved #include "control.h" to after
 *				the node typedef, so that
 *				prototyping works in file
 *				'control.h'
 *
 *
 ******************************************************************************
 */
/*
 * 'typedef struct node' now in icl.h
 */

/*
 * node.c function prototypes
 */
extern char *string_op(int yaccopcode );
extern node *node0 ( value (*interpreter)(), value val ),
	    *node1 ( value (*interpreter)(), value val, node *n0 ),
	    *node2 ( value (*interpreter)(), value val, node *n0, node *n1 ),
	    *node3 ( value (*interpreter)(), value val, node *n0, node *n1,
		     node *n2 ),
	    *node4 ( value (*interpreter)(), value val, node *n0, node *n1,
		     node *n2, node *n3 );
extern node *nodecopy( node *n2bcopied );
extern node *node_real    ( double f ),
	    *node_integer ( int i );
extern node *node_string  ( char *s ),
	    *node_logical ( int i );
extern node *node_value   (value v );
extern node *node_comment (node *n, char *commentstring );
extern node *node_builtin (value (*fn)() );
extern node *node_func    ( value (*fn)()  );
extern node *node_nonary_func ( value (*fn)() );
extern node *node_unary_func  ( value (*fn)() );
extern node *node_unary_numeric_func  ( value (*fn)() );
extern node *node_binary_numeric_func ( value (*fn)() );
extern node *node_unary_string_func   ( value (*fn)() );
extern node *node_binary_string_func  ( value (*fn)() );
extern node *node_unary_integer_func  ( int (*fn)() );
extern node *node_binary_integer_func ( int (*fn)() );
