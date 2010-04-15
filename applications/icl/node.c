/******************************************************************************
 *
 *	NODE.C	ICL node related routines
 *
 *	History
 *	Created :	S.K.Robinson	12/11/91
 *	Tidied and reformatted :
 *			B.K.McIlwrath	21/07/93
 *
 ******************************************************************************
 */
#include <stdio.h>
#include "icl.h"
#include "expr.h"
#include "parse.h"
extern value comment_interpret (node *n, int op);	/* control.c */
extern value builtin_interpret (node *n, int op);	/* interp.c  */

/******************************************************************************
 *
 *	USEFUL NULL POINTERS
 *
 * NODENIL - A global variable holding a null node pointer.
 *
 * A node contains a pointer to an array of node pointers
 * ie type node **.  SUBNIL, local to node.c is a null value for this pointer
 *
 ******************************************************************************
 */

node *NODENIL = (node *) 0;
static node **SUBNIL = (node **) 0;

/******************************************************************************
 *
 *	S T R I N G _ O P (int yaccopcode)
 *
 * This is a straight-forward mapping from YACC operator terminal integer
 * code to a printable string.
 *
 ******************************************************************************
 */
char *
string_op(int yaccopcode)
{
    switch (yaccopcode) {
      case POWER:
	return ("**");
      case MULTIPLY:
	return ("*");
      case DIVIDE:
	return ("/");
      case ADD:
	return ("+");
      case SUBTRACT:
	return ("-");
      case EQUAL:
	return ("=");
      case LESS_THAN:
	return ("<");
      case LESS_EQUAL:
	return ("<=");
      case GREATER_THAN:
	return (">");
      case GREATER_EQUAL:
	return (">=");
      case NOT_EQUAL:
	return ("<>");
      case AND:
	return ("AND");
      case OR:
	return ("OR");
      case NOT:
	return ("NOT");
      case CONCAT:
	return ("&");
      case NEGATE:
	return ("-");
      default:
	return ("<unknown operator>");
    }
}

/******************************************************************************
 *
 * 			CONSTRUCTORS
 *
 * Most cases of nodes are one-offs, so generalised node generators are
 * provided which are specialised by the interpreter argument.
 * Some nodes (e.g. node_comment) are created in more than one context, so
 * specialised generators are provided.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	N O D E B U I L D ( value (*interpreter)(), value val, int nsub)
 *
 ******************************************************************************
 */
static node
*nodebuild(value(*interpreter) (), value val, int nsub)
{
    node *new = (node *) malloc(sizeof(node));

    if (new == NODENIL)
	return (NODENIL);
    else {
	new->interpret = interpreter;
	new->n_nodes = nsub;
	new->val = val;
	if (nsub == 0) {
	    new->sub = SUBNIL;
	    return (new);
	} else if ((new->sub =  (node **) malloc((unsigned)
				(sizeof(node *) * nsub))) == SUBNIL)
	    return (NODENIL);
	else
	    return (new);
    }
}

/******************************************************************************
 *
 *	N O D E 0 (value (*interpreter)(), value val)
 *
 ******************************************************************************
 */
node *
node0(value(*interpreter) (), value val)
{
    return (nodebuild(interpreter, val, 0));
}

/******************************************************************************
 *
 *	N O D E 1 (value (*interpreter)(), value val, node *n0)
 *
 ******************************************************************************
 */
node *
node1(value(*interpreter) (), value val, node *n0)
{
    node *new = nodebuild(interpreter, val, 1);

    if (new != NODENIL) {
	new->sub[0] = n0;
	return new;
    } else
	return new;
}

/******************************************************************************
 *
 *	N O D E 2 (value (*interpreter)(), value val, node *n0, node *n1)
 *
 ******************************************************************************
 */
node *
node2(value(*interpreter) (), value val, node *n0, node *n1)
{
    node *new = nodebuild(interpreter, val, 2);

    if (new != NODENIL) {
	new->sub[0] = n0;
	new->sub[1] = n1;
	return new;
    } else
	return new;
}

/******************************************************************************
 *
 *	N O D E 3 (value (*interpreter)(), value val, node *n0, node *n1,
 *		   node *n2)
 *
 ******************************************************************************
 */
node *
node3(value(*interpreter) (), value val, node *n0, node *n1, node *n2)
{
    node *new = nodebuild(interpreter, val, 3);

    if (new != NODENIL) {
	new->sub[0] = n0;
	new->sub[1] = n1;
	new->sub[2] = n2;
	return new;
    } else
	return new;
}

/******************************************************************************
 *
 *	N O D E 4 (value (*interpreter)(), value val, node *n0, node *n1,
 *		   node *n2, node *n3)
 *
 ******************************************************************************
 */
node *
node4(value(*interpreter) (), value val, node *n0, node *n1, node *n2,
      node *n3)
{
    node *new = nodebuild(interpreter, val, 4);

    if (new != NODENIL) {
	new->sub[0] = n0;
	new->sub[1] = n1;
	new->sub[2] = n2;
	new->sub[3] = n3;
	return new;
    } else
	return new;
}

/******************************************************************************
 *
 *	N O D E C O P Y (node *n)
 *
 * Copy the node pointed to by n, including any sub[] nodes.
 *
 ******************************************************************************
 */
node *
nodecopy(node *n)
{
    node *new;

    if (n == NODENIL)
	return (NODENIL);
    else {
	new = nodebuild(n->interpret, n->val, n->n_nodes);
	if (n->val.type == TYPE_STRING ||
	    n->val.type == TYPE_EXCEPTION)
	    new->val.u.string = strcopy(n->val.u.string);
	if (new != NODENIL && new->n_nodes != 0) {
	    int i;

	    for (i = 0; i < new->n_nodes; ++i)
		if (n->sub[i] == NODENIL)
		    new->sub[i] = NODENIL;
		else if ((new->sub[i] = nodecopy(n->sub[i])) == NODENIL)
		    return NODENIL;
	    return new;
	} else
	    return new;
    }
}

/******************************************************************************
 *
 * 		CONSTRUCTORS for C values
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	N O D E _ R E A L (double f)
 *
 ******************************************************************************
 */
node *
node_real(double f)
{
    return node0(real_interpret, value_real(f));
}

/******************************************************************************
 *
 *	N O D E _ I N T E G E R (int i)
 *
 ******************************************************************************
 */
node *
node_integer(int i)
{
    return node0(integer_interpret, value_integer(i));
}

/******************************************************************************
 *
 *	N O D E _ S T R I N G (char *s)
 *
 ******************************************************************************
 */
node *
node_string(char *s)
{
    return node0(string_interpret, value_string(s));
}

/******************************************************************************
 *
 *	N O D E _ L O G I C A L (int i)
 *
 ******************************************************************************
 */
node *
node_logical(int i)
{
    return node0(logical_interpret, value_logical(i));
}

/******************************************************************************
 *
 *	N O D E _ V A L U E (value v)
 *
 * CONSTRUCTOR for ICL values.
 *
 ******************************************************************************
 */
node *
node_value(value v)
{
    switch (thetypeof(v)) {
      case TYPE_INTEGER:
	return node_integer(integer_part(v));
      case TYPE_REAL:
	return node_real(real_part(v));
      case TYPE_LOGICAL:
	return node_logical(logical_part(v));
      case TYPE_STRING:
	return node_string(string_part(v));
      case TYPE_FILE:
	return node0((value(*) ()) 0, value_file(file_part(v)));
      case TYPE_EXCEPTION:
	return node0(except_interpret, exception(string_part(v)));
      case TYPE_UNDEFINED:
	return node0(undefined_interpret, v );
      default:
	{
	    char buf[255];

	    sprintf(buf, "attempt to build node of unknown type (%d)\n",
		thetypeof(v));
	    systemfail(buf);
	    return NODENIL; /* for lint */
	}
    }
}

/******************************************************************************
 *
 *	N O D E _ C O M M E N T (node *n, char *s)
 *
 * Comment node.
 *
 * If the comment is NULL, we return the statement that it would have been
 * attached to. This saves a cumbersome test wherever node_comment is used.
 *
 ******************************************************************************
 */
node *
node_comment(node * n, char *s)
{
    if (s == CHARNIL)
	return n;
    else
	return (node1(comment_interpret, value_string(s), n));
}

/******************************************************************************
 *
 * Most of the generators below are used during initialisation of the
 * built-ins. The generators for function nodes allow preprocessing of
 * the call context to relieve the built-ins of argument processing
 * and result checking. This is sufficiently effective that many ICL
 * built-ins (e.g. trig functions) refer directly to functions in the C library.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	N O D E _ B U I L T I N (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_builtin(value(*fn) ())
{
    return node0(builtin_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_func(value(*fn) ())
{
    return node0(func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ N O N A R Y _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_nonary_func(value(*fn) ())
{
    return node0(nonary_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ U N A R Y _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_unary_func(value(*fn) ())
{
    return node0(unary_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ U N A R Y _ N U M E R I C _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_unary_numeric_func(value(*fn) ())
{
    return node0(unary_numeric_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ B I N A R Y _ N U M E R I C _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_binary_numeric_func(value(*fn) ())
{
    return node0(binary_numeric_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ U N A R Y _ S T R I N G _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_unary_string_func(value(*fn) ())
{
    return node0(unary_string_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ B I N A R Y _ S T R I N G _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_binary_string_func(value(*fn) ())
{
    return node0(binary_string_func_interpret, value_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ U N A R Y _ I N T E G E R _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_unary_integer_func(int (*fn) ())
{
    return node0(unary_integer_func_interpret, value_integer_function(fn));
}

/******************************************************************************
 *
 *	N O D E _ B I N A R Y _ I N T E G E R _ F U N C (value (*fn)())
 *
 ******************************************************************************
 */
node *
node_binary_integer_func(int (*fn) ())
{
    return node0(binary_integer_func_interpret, value_integer_function(fn));
}
