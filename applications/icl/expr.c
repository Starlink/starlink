/******************************************************************************
 *
 *			E X P R . C
 *
 * This file contains routines for interpreting (executing) and printing
 * (regenerating) the parse sub-trees which make up ICL expressions.
 *
 *	History
 *	Created :	S.K.Robinson	04/12/91
 *	Edited  :	S.K.Robinson; I. R. Jenkins 28/4/92
 *			To correct print format for reals in real_interpret()
 *			I.R.Jenkins	02/06/92
 *			Function protoypes, restructuring, removal of unused
 *			variables.
 *	Tidied, improved function headers and reformatted.
 *			B.K. McIlwrath	28/07/93 + 11/11/93
 *      Allow reloperator to handle strings like in IF X = "blob"
 *      (This is a lash-up and will prevent procedure arguments being
 *       compared and strings such as "X+1" being evaluated)
 *                      A.J.Chipperfield 25/01/94
 *      Use restore_adamstring in quoted_string_interpret
 *                      A.J.Chipperfield 28/11/96
 *
 ******************************************************************************
 */
#include <stdio.h>
#include <limits.h>
#include <setjmp.h>
#include "icl.h"
#include "carith.h"
#include "expr.h"
#include "interp.h"
#include "output.h"

extern char *string_op(int yaccopcode);				/* node.c */

/******************************************************************************
 *
 * The functions in the module interpret the abstract node structure
 * representing an expression.  They are all called with a pointer to the node
 * to be interpreted and an integer flag defining the type of operation
 * required.
 * The operations available are:
 * 	OP_INTERPRET	evaluate the node to an ICL value.
 *	OP-PRINT	print the source reconstruction of the node to the
 *			output stream via routines in output.c.
 *	OP_FORMAT	print the evalated node to format_buf[] using
 *			format_width and format_decimals (set up by an
 *			execution of the format operator _:_:_) to specify
 *			layout.
 *	OP_DESTROY	free any embedded contents of the node.
 *
 * All return an ICL value which is either the result of an evaluation
 * (for OP_INTERPRET) or representing success (or otherwise) of OP_PRINT etc.
 *
 ******************************************************************************
 */

/******************************************************************************
 * format_width is set by format_interpret() before interpretation with the
 * action type FORMAT.  Similarly with format_decimals.
 ******************************************************************************
 */
static int format_width, format_decimals;

/******************************************************************************
 * format_buf[] is the buffer used in FORMAT mode to format the operand of
 * _:_:_, the format operator.  Refer to format_interpret() and functions
 * given the action code OP_FORMAT.
 ******************************************************************************
 */
#define MAXFORMATWIDTH 1024
static char format_buf[MAXFORMATWIDTH];

/******************************************************************************
 *
 *	I N T E R P R E T _ F A I L (char *routine_name)
 *
 * This routine is only executed if any of the interpret routines are called
 * with an unknown operation code. It causes an icl abort.
 *
 ******************************************************************************
 */
static void
interpret_fail(char *routine_name)
{
    sprintf(format_buf, "Illegal interpret action in %s()\n", routine_name);
    systemfail(format_buf);
    return;
}

/******************************************************************************
 *
 *	U N D E F I N E D _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
undefined_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return exception("Evaluation of undefined value type");
      case OP_PRINT:
	outfpstring("UNDEFINED");
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%*s", format_width, "UNDEFINED");
	return noval;
      case OP_DESTROY:
	return noval;	/* nothing to destroy */
      default:
	interpret_fail("undefined_interpret");
	return noval;	/* for lint */
    }
}
/******************************************************************************
 *
 *		I C L    C O N S T A N T S
 *
 * string, openstring, real, integer and logical are leaves of the parse tree,
 * so their interpretation is essentially trivial, and are handled by the
 * string_interpret(), openstring()_interpret(), integer_interpret(),
 * real_interpret() and logical_interpret() nodes.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	I N T E G E R _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
integer_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpint(integer_part(n->val));
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%*d", format_width, integer_part(n->val));
	return noval;
      case OP_DESTROY:
	return noval;	/* nothing to destroy */
      default:
	interpret_fail("integer_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	L O G I C A L _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
logical_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpstring(integer_part(n->val) ? "TRUE" : "FALSE");
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%*s", format_width,
		integer_part(n->val) ? "TRUE" : "FALSE");
	return noval;
      case OP_DESTROY:
	return noval;	/* nothing to destroy */
      default:
	interpret_fail("logical_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	R E A L _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
real_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpreal(real_part(n->val));
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%#*.*f", format_width, format_decimals,
		real_part(n->val));
	return noval;
      case OP_DESTROY:
	return noval;	/* nothing to destroy */
      default:
	interpret_fail("real_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *		S T R I N G    C O N S T A N T S
 *
 * Printing strings is elaborate because of the need to regenerate the two
 * formas of quotes (' and ") in such a way that they will reparse correctly
 * to the same internal string.
 *
 * The function restore_iclstring(*string) (utils.c) is used to handle the
 * quoting logic.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	S T R I N G _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
string_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpstring(restore_iclstring(string_part(n->val)) );
	return noval;
      case OP_DESTROY:
#ifdef DEBUG
	fprintf(stderr, "free string %x\n", (string_part(n->val)));
#endif
	free((void *) (string_part(n->val)) );
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%*s", format_width, string_part(n->val));
	return noval;
      default:
	interpret_fail("string_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	Q U O T E D _ S T R I N G _ I N T E R P R E T (node *n, int op)
 *
 * This function is a derivative of string_interpret() but will return the
 * string with the quotes (removed by fixup_quotes() in lex.l) restored.
 * This is required to pass string values to ICL tasks.
 *
 * Note that the parser cannot generate calls to this function. The function
 * setup_args_task() (in interp.c) which alters the command parse tree to be
 * produce a suitable value string to pass to an ADAM task does this by
 * suistituting this function for string_interpret().
 *
 ******************************************************************************
 */
value
quoted_string_interpret(node *n, int op)
{
    if (op != OP_INTERPRET)
	return string_interpret(n, op);

/* Restore original node structure */
    n->interpret = string_interpret;

    return value_string(restore_adamstring(string_part(n->val)));
}

/******************************************************************************
 *
 *	O P E N S T R I N G _ I N T E R P R E T (node *n, int op)
 *
 * An "openstring" in ICL is a non-quoted string of characters.
 *
 ******************************************************************************
 */
value
openstring_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpstring(string_part(n->val));
	return noval;
      case OP_DESTROY:
#ifdef DEBUG
	fprintf(stderr, "free openstring %x\n", (string_part(n->val)));
#endif
	free(string_part(n->val));
	return noval;
      case OP_FORMAT:
	sprintf(format_buf, "%*s", format_width, string_part(n->val));
	return noval;
      default:
	interpret_fail("openstring_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 * 		V A R I A B L E S
 *
 * The parser generates name_interpret() nodes with the value containing the
 * name referred to in the following situations:
 *
 * 1) a variable reference in an expression,
 *
 * 2) a function reference (where the name_interpret() node is the sub[0]
      member of a function_call_interpret() node),
 *
 * 3) a variable reference on the left of an assignment (where the
 *    name_interpret() node is the sub[0] member of an assign_interpret()
 *    node),
 *
 * 4) to contain the name of a formal parameter. The name_interpret() node
 *    is the sub[1] member of a formal parameter explist_interpret() list)
 *
 * 5) and to record the identity of a pass_by_reference actual parameter
 *    (the name_interpret() node is the sub[0] member of a
 *    parameter_interpret() node).
 *
 * Only in the case of a variable reference in an expression will such
 * a node be interpreted. In this case the value of the variable (or
 * formal parameter)'s entry is found from the active symbol_table.
 *
 * Note that to pass ICL variables to ADAM tasks the function setup_hds_args()
 * (in interp.c) alters the parse tree to call name_interpret_hds() (in hds.c)
 * instead of this function.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	N A M E _ I N T E R P R E T (node *n, int op)
 *
 ******************************************************************************
 */
value
name_interpret(node *n, int op)
{
    extern value lookup_variable_value (char *name);		/* symtab.c */

    switch (op) {
      case OP_INTERPRET:
	return lookup_variable_value(string_part(n->val));
      case OP_PRINT:
	outfpstring(string_part(n->val));
	return noval;
      case OP_DESTROY:
#ifdef DEBUG
	fprintf(stderr, "free name %x\n", (string_part(n->val)));
#endif
	free(string_part(n->val));
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("name_interpret");
	return noval;	/* for lint */
    }
}


/******************************************************************************
 *
 *	O P T A R G _ I N T E R P R E T (node *n, int op)
 *
 * Used by the parser to indicate that a formal parameter of a PROCedure
 * (and all subsequent arguments) are optional and, if no actual argument(s)
 * are present the variable within the procedure will be of type <UNDEFINED>.
 * BKM, 04/05/95
 *
 ******************************************************************************
 */
value
optarg_name_interpret(node *n, int op)
{
    switch (op) {
      case OP_PRINT:
	outfpchar('[');
      default:
	return name_interpret(n, op);
    }
}

value
optarg_name_interpret_end(node *n, int op)
{
    switch (op) {
      case OP_PRINT:
	name_interpret(n, op);
	outfpchar(']');
	return noval;
      default:
	return name_interpret(n, op);
    }
}

/******************************************************************************
 *
 *	N A M E _ I N T E R P R E T _ N O H D S (node *n, int op)
 *
 * Used by the parser when it is certain that we want the value of the ICL
 * variable and it will never be passed to a task via HDS
 *
 ******************************************************************************
 */
value
name_interpret_nohds(node *n, int op)
{
    return name_interpret(n, op);
}

/******************************************************************************
 *
 *	P A R E N _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * ( E x p r e s s i o n ) (within an expression or as part of a comand line).
 *
 * The sub[0] member points to the node to be interpreted to evaluate the
 * expression. Note that this can simply be a name_interpret() node in the
 * case of paranthesised variable names.
 *
 ******************************************************************************
 */
value
paren_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return interpret(n->sub[0]);
      case OP_PRINT:
	outfpstring("(");
	print(n->sub[0]);
	outfpstring(")");
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("paren_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	C O N C A T _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * "Expression & Expression".
 *
 * The concat_interpret() node has its sub[0] member pointing to the left
 * operand and its sub[1] member to the right operand.
 *
 ******************************************************************************
 */
value
concat_interpret(node *n, int op)
{
    value val1, val2;
    char *res;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = interpret_to_string(n->sub[0])))
	    return val1;
	else if (isexc(val2 = interpret_to_string(n->sub[1])))
	    return val2;
	else {
	    if ((res = strconcat(string_part(val1),
				 string_part(val2))) == CHARNIL)
		return exception(
			"SYSERR  memory exhausted in concat_interpret()");
	    else
		return (value_string(res));
	}
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" & ");
	print(n->sub[1]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("concat_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	A N D _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * "Expression AND Expression".
 *
 * The and_interpret() node has its sub[0] member pointing to the left
 * operand and its sub[1] member to the right operand.
 *
 ******************************************************************************
 */
value
and_interpret(node *n, int op)
{
    value val1, val2;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = as_nonstring(interpret(n->sub[0]))))
	    return val1;
	else if (isexc(val2 = as_nonstring(interpret(n->sub[1]))))
	    return val2;
	else if (!islogicaltype(val1) || !islogicaltype(val2))
	    return exception(
		"OPNOTLOG  Operands of AND are not both logical values");
	else
	    return value_logical(as_logical(val1) && as_logical(val2));
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" AND ");
	print(n->sub[1]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("and_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	O R _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * "Expression OR Expression".
 *
 * The or_interpret() node has its sub[0] member pointing to the left operand
 * and its sub[1] member the right operand.
 *
 ******************************************************************************
 */
value
or_interpret(node *n, int op)
{
    value val1, val2;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = as_nonstring(interpret(n->sub[0]))))
	    return val1;
	else if (isexc(val2 = as_nonstring(interpret(n->sub[1]))))
	    return val2;
	else if (!islogicaltype(val1) || !islogicaltype(val2))
	    return exception(
		"OPNOTLOG  Operands of OR are not both logical values");
	else
	    return value_logical(as_logical(val1) || as_logical(val2));
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" OR ");
	print(n->sub[1]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("or_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	N O T _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form "NOT Expression".
 *
 * The not_interpret() node has its sub[0] member pointing to the operand.
 *
 ******************************************************************************
 */
value
not_interpret(node *n, int op)
{
    value val1;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = as_nonstring(interpret(n->sub[0]))))
	    return val1;
	else if (!islogicaltype(val1))
	    return exception("OPNOTLOG  Operand of NOT is not logical value");
	else
	    return value_logical(!as_logical(val1));
      case OP_PRINT:
	outfpstring(" NOT ");
	print(n->sub[0]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("not_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	U N I A R Y A R I T H _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * "- (NEGATE) or + (MAKE POSITIVE)  Expression".
 *
 * The uniaryarith_interpret() node has its sub[0] member pointing to the
 * operand and the value.integer component set to the internal code of either
 * SUBTRACT or ADD
 *
 ******************************************************************************
 */
value
unaryarith_interpret(node *n, int op)
{
    value val2;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val2 = as_nonstring(interpret(n->sub[0]))))
	    return val2;
	else
	    return (do_operator(intzeroval, integer_part(n->val), val2));
      case OP_PRINT:
	outfpstring(string_op(integer_part(n->val)));
	outfpstring(" ");
	print(n->sub[0]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("unaryarith_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	B I N A R Y O P E R A T O R _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for syntax of the form
 * "expression BINARY ARITHMETIC OPERATOR expression".
 *
 * The binaryoperator_interpret() node is generated for a binary arithmetic
 * operator (add, subtract etc). The sub[0] and sub[1] members point to the
 * left operands respectively and the internal code of the arithmetic operation
 * is in the nodes value.integer component.
 *
 ******************************************************************************
 */
value
binaryoperator_interpret(node *n, int op)
{
    value val1, val2;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = as_nonstring(interpret(n->sub[0]))))
	    return val1;
	else if (isexc(val2 = as_nonstring(interpret(n->sub[1]))))
	    return val2;
	else
	    return (do_operator(val1, integer_part(n->val), val2));
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" ");
	outfpstring(string_op(integer_part(n->val)));
	outfpstring(" ");
	print(n->sub[1]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("binaryoperator_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	R E L O P E R A T O R _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for ICL syntax of the form
 * "expression R E L A T I O N A L expression".
 *
 * The sub[0] member points to the left operand, sub[1] to the right while
 * the yacc code of the operator is held in the value.integer component of the
 * reloperator_interpret() node. Cannot cause overflow.
 *
 *
 ******************************************************************************
 */
value
reloperator_interpret(node *n, int op)
{
    value val1, val2;

    switch (op) {

      case OP_INTERPRET:
	if( isexc(val1 = interpret(n->sub[0])) )
	    return val1;
	if( isexc(val2 = interpret(n->sub[1])) )
	    return val2;
/*
 * At this point one or both of val1 and val2 may be of TYPE_STRING.
 * do_reloperator() will use string comparison if BOTH are of TYPE_STRING
 * otherwise it will use arithmetic relational operations.
 */
        return (do_reloperator(val1, integer_part(n->val), val2));

      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" ");
	outfpstring(string_op(integer_part(n->val)));
	outfpstring(" ");
	print(n->sub[1]);
	return noval;

      case OP_DESTROY:
	return noval;

      case OP_FORMAT:
	return noval;

      default:
	interpret_fail("reloperator_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	F O R M A T _ I N T E R P R E T (node *n, int op)
 *
 * The parser generates this node for expressions of the form:
 * 	"expression0 : expression1 : expression2"
 * OR	"expression0 : expression1"
 *
 * The sub[0] member of the format_interpret() node points to the expression
 * to be formatted, sub[1] the field_width expression and sub[2] is either NIL
 * or points to the decimal_precision expression.
 *
 * A quirk of the VMS ICL is that operands of the formatting operator (:) have
 * unusual type restrictions. In case this is important the behaviour is
 * faithfully reproduced here!!
 *
 ******************************************************************************
 */
value
format_interpret(node *n, int op)
{
    extern node *node_value (value v);				/* node.c   */

    value val;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val = interpret_to_integer(n->sub[1])))
	    return val;
	else
	    format_width = integer_part(val);
	if (format_width >= MAXFORMATWIDTH)
	    format_width =  MAXFORMATWIDTH - 1;
	if (n->sub[2]) {
	    if (isexc(val = interpret_to_integer(n->sub[2])))
		return val;
	    format_decimals = integer_part(val);
	    if (format_decimals >= format_width)
		format_decimals = format_width - 3; /* 1 for sign, 2 for (at
						     * least) '0.'
						     */
	} else
	    format_decimals = 1;
	if (isexc(val = interpret(n->sub[0])))
	    return val;
	else if (!isintegertype(val) && !isrealtype(val))
	    return exception(
	"OPNOTNUM  First operand of format is not numeric");
	else {
	    char *s;
	    node *v;

	    if ((v = node_value(val)) == NODENIL)
		return exception(
	"SYSERR  memory Exhausted during format operation");
	    if (isexc(val = ((*v->interpret) (v, OP_FORMAT))))
		return val;
	    else if ((s = strcopy(format_buf)) == CHARNIL)
		return exception(
	"SYSERR  memory Exhausted during format operation");
	    else
		return value_string(s);
	}

      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(":");
	print(n->sub[1]);
	if (n->sub[2] == NODENIL)
	    return noval;
	else {
	    outfpstring(":");
	    print(n->sub[2]);
	    return noval;
	}

      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("format_interpret");
	return noval;	/* for lint */
    }
}


/******************************************************************************
 *
 *	S A V E _ A R G S (node ***saveloc)
 *
 * save_args(node (*loc)[]) and restore_args(node (*loc)[], numberofargs) are
 * used when calling functions.
 *
 * When a procedure or function call is parsed it generates a linked list of
 * abbut_interpret() (procedure) or explist_interpret() (function) nodes
 * chained together using their sub[0] components.
 *
 * The arguments themselves are referenced using the node.sub[1] component but
 * in inverse order with the last argument being linked to the first
 * node, the last but one to the next etc.
 * To ease the processing of these lists get_args() copies the argument node
 * pointers into an array arglist[] and counts, in nargs, their number.  Note
 * that the first actual argument in a call is placed in arglist[0] and so we
 * traverse the list in postorder.
 *
 * In evaluating an expression's actual arguments we could call a function and
 * our argument array would be corrupted.  Hence, function_call interpret
 * stacks the array arglist[] and pops it on return using save_args()
 * and restore_args()
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	S A V E _ A R G S (node ***saveloc)
 *
 ******************************************************************************
 */
static int
save_args(node ***saveloc)
{
    extern node **arglistp;
    int i;

    *saveloc = (node **) 0;
    if (nargs == 0)
	return 1;
    else {
	*saveloc = (node **) malloc((unsigned) (sizeof(node *) * nargs));
	if (*saveloc == NULL)
	    return 0;
	else {
	    for (i = 0; i < nargs; i++)
		(*saveloc)[i] = arglistp[i];
	    return 1;
	}
    }
}

/******************************************************************************
 *
 *	R E S T O R E _ A R G S (node ***saveloc)
 *
 ******************************************************************************
 */
static void
restore_args(node **savea, int n)
{
    extern node **arglistp;
    int i;

    if (n == 0)
	return;
    else {
	for (i = 0; i < n; i++)
	    arglistp[i] = savea[i];
	free(savea);
	return;
    }
}

/******************************************************************************
 *
 *	F U N C T I O N _ C A L L _ I N T E R P R E T (node *n, int op)
 *
 * Calls a built-in function by name.
 *
 * We need to save and restore the current argument list as described above,
 * since it may be in use. Note that it must be initialised properly.
 *
 * If the function fails, we do not pop the call stack. This is so that, if the
 * exception is not handled, we can print the call stack at the point of
 * failure when we reach the top level again.
 *
 ******************************************************************************
 */
value
function_call_interpret(node *n, int op)
{
    extern node *lookup_symbol (char *name, int type);		/* symtab.c */

    node *p;
    extern node **arglistp, *all_args;
    extern int nargs;
    extern value checkcallstack(char *), pushcallstack(char *),
		 popcallstack(void);
    extern void get_args(node *);	/* interp.c */
    value val;
    char *funcname = string_part(n->sub[0]->val);

    switch (op) {
      case OP_INTERPRET:
	if ((p = lookup_symbol(funcname, SYM_FUNCTION)) != NODENIL) {
	    int saven;
	    value res;
	    node **savea;

	    saven = nargs;
	    if (save_args(&savea) == 0)
		return exception(
		"SYSERR  memory Exhausted during ICL built-in function call");
	    if (isexc(val = checkcallstack(funcname)))
		return (val);
	    else {
		all_args = n->sub[1];
		arglistp = arglist;
		nargs = 0;
		get_args(n->sub[1]);
		if (isexc(val = pushcallstack(funcname)))
		    return (val);
		if (!isexc(res = interpret(p)))
		    if (isexc(val = popcallstack()))
			return (val);
		restore_args(savea, saven);
		nargs = saven;
		return res;
	    }
	} else
	    return (exception1("FUNCERR unknown function \"%s\"", funcname));
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" (");
	print(n->sub[1]);
	outfpstring(")");
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("function_call_interpret");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 * The following func_*_interpret functions are very similar.
 * There is an opportunity for code merging here.
 * Called directly from interpret() which is called from func_call_interpret().
 ******************************************************************************
 */

/******************************************************************************
 *
 *	N O N A R Y _ F U N C _ I N T E R P R E T (node *n, int op)
 *
 * Interpret a built-in function of no arguments .
 *
 ******************************************************************************
 */
value
nonary_func_interpret(node *n, int op)
{
    value val;

    if (op == OP_INTERPRET)
	if (isexc(val = nargs_should_be(0)))
	    return val;
	else
	    return (*(function_part(n->val))) ();
    else
	return noval;
}

/******************************************************************************
 *
 *	F U N C _ I N T E R P R E T (node *n, int op)
 *
 * Interpret a built-in function which does its own argument processing.
 *
 ******************************************************************************
 */
value
func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET)
	return (*(function_part(n->val))) ();
    else
	return noval;
}

/******************************************************************************
 *
 *	U N A R Y _ F U N C _ I N T E R P R E T (node *n, int op)
 *
 * Interpret a built-in function with one argument.
 *
 * We need to save the contents of arglist[] into local variables (arg1
 * and/or arg2) as in the call to interpret() an argument may call another
 * function which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
unary_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val;
	node *arg1;

	arg1 = arglist[0];
	if (isexc(val = nargs_should_be(1)))
	    return val;
	else if (isexc(val = interpret(arg1)))
	    return val;
	else
	    return (*(function_part(n->val))) (val);
    } else
	return noval;
}

/******************************************************************************
 *
 *	B I N A R Y _ F U N C _ I N T E R P R E T (node *n, int op)
 *
 * interpret a built-in function with two arguments .
 *
 * We need to save the contents of arglist[] into local variables (arg1
 * and/or arg2) as the call to interpret() an argument may call another
 * function which could destroy the the argument array contents
 *
 ******************************************************************************
 */
value
binary_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val, vals1, vals2;
	node *arg1, *arg2;

	arg1 = arglist[0];
	arg2 = arglist[1];
	if (isexc(val = nargs_should_be(2)))
	    return val;
	else if (isexc(vals1 = interpret(arg1)))
	    return vals1;
	else if (isexc(vals2 = interpret(arg2)))
	    return vals2;
	else
	    return (*(function_part(n->val))) (vals1, vals2);
    } else
	return noval;
}

/******************************************************************************
 *
 *	U N A R Y _ I N T E G E R _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with one integer argument and an integer
 * result which cannot generate an exception.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
unary_integer_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val;
	node *arg1;

	arg1 = arglist[0];
	if (isexc(val = nargs_should_be(1)))
	    return val;
	else if (isexc(val = interpret_to_integer(arg1)))
	    return val;
	else
	    return (call_unary_integer_func(val, ifunction_part(n->val)));
    } else
	return noval;
}

/******************************************************************************
 *
 *	B I N A R Y _ I N T E G E R _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with two integer arguments and an integer
 * result which cannot generate an exception.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
binary_integer_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val, vals[2];
	node *arg2, *arg1;

	arg1 = arglist[0];
	arg2 = arglist[1];
	if (isexc(val = nargs_should_be(2)))
	    return val;
	else if (isexc(vals[0] = interpret_to_integer(arg1)))
	    return vals[0];
	else if (isexc(vals[1] = interpret_to_integer(arg2)))
	    return vals[1];
	else
	    return (call_binary_integer_func(vals[0], vals[1],
	    (ifunction_part(n->val))));
    } else
	return noval;
}

/******************************************************************************
 *
 *	U N A R Y _ N U M E R I C _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with one numeric argument.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
unary_numeric_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val;
	node *arg1;

	arg1 = arglist[0];
	if (isexc(val = nargs_should_be(1)))
	    return val;
	else if (isexc(val = interpret_to_numeric(arg1)))
	    return (exception("INVARGMAT Invalid argument to math function"));
	else
	    return (*(function_part(n->val))) (val);
    } else
	return noval;
}

/******************************************************************************
 *
 *	B I N A R Y _ N U M E R I C _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with two numeric arguments.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
binary_numeric_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val, vals[2];
	node *arg2, *arg1;

	arg1 = arglist[0];
	arg2 = arglist[1];
	if (isexc(val = nargs_should_be(2)))
	    return val;
	else if (isexc(vals[0] = interpret_to_numeric(arg1)))
	    return (exception("INVARGMAT Invalid argument to math function"));
	else if (isexc(vals[1] = interpret_to_numeric(arg2)))
	    return (exception("INVARGMAT Invalid argument to math function"));
	else
	    return (*(function_part(n->val))) (vals[0], vals[1]);
    } else
	return noval;
}

/******************************************************************************
 *
 *	U N A R Y _ S T R I N G _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with one string argument.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
unary_string_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val;
	node *arg1;

	arg1 = arglist[0];
	if (isexc(val = nargs_should_be(1)))
	    return val;
	else if (isexc(val = interpret_to_string(arg1)))
	    return val;
	else
	    return (*(function_part(n->val))) (string_part(val));
    } else
	return noval;
}

/******************************************************************************
 *
 *	B I N A R Y _ S T R I N G _ F U N C _ I N T E R P R E T
 *						(node *n, int op)
 *
 * Interpret a built-in function with two string arguments.
 *
 * We need to save the contents of arglist[] into local variables (arg1 and/or
 * arg2) as the call to interpret() an argument may call another function
 * which could destroy the the argument array contents.
 *
 ******************************************************************************
 */
value
binary_string_func_interpret(node *n, int op)
{
    if (op == OP_INTERPRET) {
	value val, vals[2];
	node *arg2, *arg1;

	arg1 = arglist[0];
	arg2 = arglist[1];
	if (isexc(val = nargs_should_be(2)))
	    return val;
	else if (isexc(vals[0] = interpret_to_string(arg1)))
	    return vals[0];
	else if (isexc(vals[1] = interpret_to_string(arg2)))
	    return vals[1];
	else
	    return (*(function_part(n->val))) (string_part(vals[0]),
		    string_part(vals[1]));
    } else
	return noval;
}

/******************************************************************************
 *
 *	E X C E P T _ I N T E R P R E T (node *n, int op)
 *
 * Actual exceptions are not often stored in nodes (as distinct from exception
 * handlers) One case where they are is when the parser wants to return an
 * exception because of a syntax error.
 *
 * This allows things like evaluation of DEFSTRINGs and the tratment of strings
 * as expressions to be handled robustly.
 *
 * Exception nodes should never be printed.
 *
 ******************************************************************************
 */
value
except_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return n->val;
      case OP_PRINT:
	outfpstring("EXCEPTION: ");
	outfpstring(string_part(n->val));
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	interpret_fail("except_interpret");
	return noval;	/* for lint */
    }
}
