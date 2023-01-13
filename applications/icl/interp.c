/******************************************************************************
 *
 * 	INTERP.C	ICL routines associated with interpreting nodes.
 *
 * 	History
 * 	Created : S.K.Robinson 7/11/91
 *	Tidied, reformatted :
 *		  B.K. McIlwrath 23/7/92 + 15/11/93
 *      Added concat_args from adam.c
 *                A.J.Chipperfield 13/1/94
 *      Use restore_adamstring in abbut_qstring_interpret
 *                A.J.Chipperfield 28/11/96
 *      Use new-style ems function names
 *                A.J.Chipperfield 21/06/99
 *
 ******************************************************************************
 */
#include <stdio.h>
#include "sae_par.h"
#include "ems.h"
#include "icl.h"
#include "expr.h"
#include "interp.h"
#include "node.h"
#include "symtab.h"
#include "procs.h"
#include "f77.h"
#include "cnf.h"
#include "output.h"
#include "control.h"

int yyparse();

extern char *currentproc(void);					/* main.c */

/******************************************************************************
 * 		T O D O
 *
 * A variable that is set by the parser to point to the parse
 * tree that needs to be evaluated first
 ******************************************************************************
 */
node *todo;

/******************************************************************************
 *
 * The following variables are used by the get_args and install_args to
 * communicate arguments between procedure call actual parameters
 * (proc_call_interpret) and the procedure's formal parameters
 * (procedure_interpret).
 *
 * arglist[] is set by get_args() to hold pointers to the actual argument
 *	nodes, arglist[0] to the first, [1] to the second etc, of a function
 *	or procedure call.
 * nargs     is set by get_args() to a count of the number of args
 * oprargs is a flag to set indicate that the current formal PROC parameter
 *	   (and all subsequent ones) are delared to be optional.
 * all_args  is set to point to the abbut_interpret() list if actual
 *	argument nodes.
 *
 ******************************************************************************
 */
node *arglist[ICL_BUFSIZE];
int nargs;
int optargs;
node *all_args;

/********************************************************************
 * node **arglistp is a pointer to the next argument slot available.
 *
 * Used by get_args to linearise the argument list in the parse
 * tree in handling the call and by install_args in placing
 * parameters in the symbol table with their values just
 * prior to executing the body of the proc.
 *
 ********************************************************************
 */
node **arglistp;

/******************************************************************************
 *
 * 	G E T _ A R G S (node *n)
 *
 * Before get_args() is called, arglistp is set to &arglist[0] and nargs is
 * set to zero.
 *
 * This routine traverses the explist_ or abbut_interpret list of actual
 * arguments to a function or procedure and sets up arglist[0] to point to
 * the first argument, arglist[1] to the  second etc.
 * nargs is set to the total number of actual arguments.
 *
 ******************************************************************************
 */
void
get_args(node *n)
{
    if (n == NODENIL) {
	return;
    } else if (n->interpret == explist_interpret ||
	       n->interpret == abbut_interpret) {
	get_args(n->sub[0]);
	get_args(n->sub[1]);
    } else if (n->interpret == comment_interpret) {
	get_args(n->sub[0]);
    } else {
	*arglistp++ = n;
	++nargs;
    }
    return;
}

/******************************************************************************
 *
 * 	S E T U P _ T A S K _ H D S (node *n, int *hdsused)
 *
 * The parser cannot tell whether a command argument list will be passed to an
 * ADAM task or, for example, an ICL procedure. This function is called when
 * the agument list WILL be passed to an ADAM task and adjusts the precompiled
 * node tree such that, when the argument list is interpreted, a suitable
 * text "value string" is generated to be passed to the task.
 *
 * The adjustments made in the parse tree are:
 *
 *	Quotes are restored for strings passed by value.
 *
 *	When ICL and the task are interlocked (eg OBEYW and interpreting a
 *	DEFINE command) ICL variables are written to an HDS object and a
 *	pointer to the HDS object is passed in the "value string". By
 *	reloading ICL variables after the task has run the task is allowed
 *	to alter these ICL variables. (cf setup_task_nohds() when the task
 *	runs asynchronously from ICL).
 *
 * To restore the quotes in strings passed to ADAM tasks (were removed by
 * fixup_quotes() in lex.c) we replace any calls to string_interpret() to call
 * quoted_string_interpret().
 *
 * The parser generates the following node construct for a variable in a
 * command:
 *	node->interpret == paren_interpret();
 * 	node->sub[0]->interpret = name_interpret();
 *
 * This will generate the current value of the ICL variable. In this one case
 * we replace the call to the name_interpret() function by name_interpret_hds()
 *
 * The function argument 'hdsvars' is returned as a count of the total number
 * of ICL variables passed by reference.
 *
 ******************************************************************************
 */
void
setup_task_hds(node *n, int *hdsvars)
{
    extern value name_interpret_hds(node *n, int op);		/* hds.c */
    extern value assign_helper (node *var, value val);          /* symtab.c */
    value val;

    if (n == NODENIL)
	return;
    else if(n->interpret == explist_interpret ||
	    n->interpret == abbut_interpret ) {
	setup_task_hds(n->sub[0], hdsvars);
	setup_task_hds(n->sub[1], hdsvars);
    }
/*
 * String variables
 */
    else if (n->interpret == string_interpret)
	n->interpret = quoted_string_interpret;
/*
 * HDS variables
 * NB Qualifiers to a command line argument (eg a_file(1:(a))) must be
 * passed by value and are recognised by a the node->value component of
 * abbut_interpret being FALSE.
 */
    else if (n->interpret == comment_interpret)
	setup_task_hds(n->sub[0], hdsvars);
    else if (n->interpret == paren_interpret &&
	     n->sub[0]->interpret == name_interpret) {
/*
 * ICL string type variables starting with '@' are HDS locators and get
 * passed by value to the task.
 */
	if( isexc(val = interpret(n->sub[0])) )
	    if( strncmp(val.u.string, "UNDEFVAR", 8) == 0) {
		val = value_string("");
		assign_helper(n->sub[0], val);
	    } else
		return;
	else
	    if( val.type == TYPE_STRING && val.u.string[0] == '@')
		return;

	/* We will use HDS to pass this variable */
 	n->sub[0]->interpret = name_interpret_hds;
	(*hdsvars)++;
    }
    return;
}

/******************************************************************************
 *
 * Before a function is actually called, its arguments are linearised from
 * the expression tree into arglist[] and nargs is set to the argument
 * count by get_args().
 *
 * The name of the function being called is also stacked by
 * function_call_interpret() calling pushcallstack().
 * The following routines are called if the number of arguments expected is
 * either fixed or lies within a range  - an exception is generated if nargs
 * does not correspond.
 *
 * In most places, these checks are centralised in one of the function
 * node interpreters (func_unary_interpret and friends)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 * 	N A R G S _ S H O U L D _ B E (int n)
 *
 * Checks the argument count for an ICL procedure is correct by checking nargs.
 *
 ******************************************************************************
 */
value
nargs_should_be(int n)
{
    char buf[ICL_BUFSIZE];
    char *curfunc;

    if (nargs == n)
	return noval;
    else {
        strcpy(buf, (nargs < n ? "TOOFEWPARS" : "TOOMANYPARS") );
	if( (curfunc = currentproc() ) != CHARNIL )
	    sprintf(buf+strlen(buf), " function %s given %d args, expects %d",
		    curfunc, nargs, n);
    }
    return exception(buf);
}

/******************************************************************************
 *
 *	N A R G S _ I N _ R A N G E (int low, int high)
 *
 * Checks the argument count for an ICL procedure is within an allowable range.
 *
 ******************************************************************************
 */
value
nargs_in_range(int low, int high)
{
    char buf[ICL_BUFSIZE];

    if (nargs >= low && nargs <= high)
	return noval;
    else
	sprintf(buf, "%s  function %s given %d args, expects %d to %d",
		(nargs < low ? "TOOFEWPARS" : "TOOMANYPARS"),
		currentproc(), nargs, low, high);
    return exception(buf);
}

/******************************************************************************
 *
 *	C O N C A T _ A R G S (int from )
 *
 * Concatenate a proc's evaluated argument list seen as strings into a buffer.
 * The arguments have been stored in arglist[] by get_args(), with nargs
 * set to their number.  As usual all_args points to the node representing
 * the complete argument list.
 * Only part of the argument list is so treated (from the 'from'th one)
 * For efficiency, we can work our way down the argument list held in
 * all_args to the 'from'th one and then rely on the fact that if we
 * interpret an abbut_interpret() node it will do the interpretation
 * and concatenation for us delivering a ICL value (string) result.
 *
 * ** Numbering starts from zero.
 *
 ******************************************************************************
 */
value
concat_args(int from)
{
    node *argp = all_args;
    int i;

    for (i = 0; i < from; i++)
	argp = argp->sub[1];
    return interpret(argp);
}

/******************************************************************************
 *
 * On encountering a proc definition we set up a SYM_PROC/SYM_HIDDEN_PROC
 * entry in the world symbol table.  This entry's value member points to a
 * procedure_interpret() node whose value member is a TYPE_SYMTAB with its
 * symboltable_part() pointing to a symbol table dummy header.
 * It is this table which is used to hold local variables and parameters
 * when the proc is called.
 *
 * The sub[0] member of the procedure_interpret() node points to the formal
 * parameter list which is a list of explist_interpret() nodes linked together
 * using their sub[0] members The sub[1] members of these nodes point to
 * name_interpret nodes recording the identities of the formal parameters in
 * the string_part() member of these nodes.  The identity of the last formal
 * is linked in first, the first formal comes last in the list.  This list
 * may record comment_interpret() nodes which use their sub[0] members
 * to continue the list. They are ignored in interpretation.
 *
 * The sub[1] member points to the proc body nodes.
 * The sub[3] member points to the list of execeptions.
 *
 * When we interpret a proc node (or function node) we must set up the
 * formal parameters in the procedure symbol table with their "values"
 * and then execute the body of the procedure or function.  The actual
 * parameter nodes are linearised into the array arglist[], and the count
 * nargs set to how many there (see get_args()) and then we interpret the
 * procedure node.
 *
 * If a actual argument is of the form (v) (ie pass by reference) the actual
 * parameter is a series of paren_interpret() nodes (linked by sub[0] member)
 * ending in a name_interpret node whose string_part() points to the identity
 * of the parameter.
 *
 * If a actual argument is an expression (exp) the actual is a series of
 * paren_interpret() nodes (linked by sub[0] member) ending in a expression
 * node (something other than name_interpret()).
 *
 * If the argument is a string or open string then the actual is a string node.
 *
 * Install_args(n,procn) installs the formal parameters (the list of which
 * is in n) with their corresponding actual's values in procn's symbol table.
 *
 * We walk the FORMAL parameter list in postorder so we see the first formal
 * first. At the leaves of the formal parameter list we need to install the
 * corresponding actual parameter. The formals go into the called procedure's
 * symbol table (symboltable_part(procn->val)) as SYM_PARAMETER objects, so
 * that they are not visible to VARS and because they require different
 * treatment.
 *
 * A SYM_PARAMETER symbol's table entry can have as its value member,  either
 * a pointer to a  parameter_interpret()  node, whose type is TYPE_SYMTAB,
 * and whose symboltable_part() member points to the symbol table containing
 * the corresponding passed_by_reference variable, and whose sub[0] member
 * points to a name_interpret() node whose string_part() holds the identity
 * the passed_by_reference variable
 *
 * OR
 *
 * a pointer to an evaluated expression node (the value of an actual
 * parameter expression) which is evaluated prior to entering the formal into
 * the symboltable. This is the form when closed or open strings form the
 * actual argument.
 *
 * If the actual parameter is itself a parameter in the calling context, its
 * value is simply copied to the corresponding formal. In this way we avoid
 * chains of pass_by_reference.
 *
 * In evaluating an expression actual argument we could call a function
 * and our argument array arglist[]  would be corrupted.  Hence,
 * function_call_interpret() stacks the array arglist[] and pops it on return.
 *
 * By using these rules, the ICL call-by-reference semantics are preserved.
 *
 * Because formal parameters are different from local variables, assignment to
 * a parameter which is not a variable can be caught.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 * 	I N S T A L L _ A R G S ( node *n, node *procn)
 *
 * Initially,
 *
 * We are passed a pointer to formal parameter list of the procedure to be
 * executed and get_args() has been called so arglist[0] contains a pointer
 * to the first actual argument node, arglist[1] to the second etc.  nargs
 * is a count of the number of actual args and arglistp points to arglist[0]
 *
 * On a recursive call
 *
 * We are passed a pointer to a subpart of a formal parameter list of the
 * proc to be executed nargs has a count of the unprocessed actuals
 * arglistp points to the current unprocessed actual in arglist[]
 *
 * We note that we delete any existing entry from the proc's symbol table for
 * the parameter.  This is because it is possible, with the SET NOCHECKPARS
 * command to switch off the "too few parameters" exception.
 * This means that the parameter references in the proc are taken
 * as VARIABLE when too few parameters are given on a call and if the next
 * call gives a value to that parameter we end up with two entries in the
 * procs symbol table, one for the parameter and one for the variable from
 * the previous call.
 *
 ******************************************************************************
 */
static value
install_args(node *n, node *procn)
{
    value val;
    node *arg;

    if (n == NODENIL) {
	return trueval;
    } else if (n->interpret == explist_interpret) {
	if (isexc(val = install_args(n->sub[0], procn)))
	    return val;
	else
	    return (install_args(n->sub[1], procn));
    } else if (n->interpret == comment_interpret) {
	    return (install_args(n->sub[0], procn));
    } else {
	if (!optargs && (n->interpret == optarg_name_interpret ||
			 n->interpret == optarg_name_interpret_end ))
	    optargs = 1;
	if (nargs == 0)
	    if(!optargs && checkpars)
	      return exception("TOOFEWPARS Not enough parameters for command");
	if (nargs != 0) {
	    --nargs;
	    arg = *arglistp++;
	    while (arg->interpret == paren_interpret)
		arg = arg->sub[0];
	    if (arg->interpret == name_interpret) {
		node *v;
		if( (v = lookup_symbol(string_part(arg->val),
				   SYM_PARAMETER)) != NODENIL &&
		    (v->interpret == parameter_interpret ||
		     v->interpret == string_interpret) ) {
		    arg = v;
		} else {
		    node *nw;
		    if( (nw = node0(name_interpret,
				    value_string(string_part(arg->val))))
			== NODENIL ||
			(arg = node1(parameter_interpret,value_symbols(), nw))
			== NODENIL) {
			return exception(
				"SYSERR  memory exhausted in install_args()");
		    }
		}
	    } else {
		if (isexc(val = interpret(arg)))
		    return val;
		arg = node_value(val);
 	    }
	} else {
	    val.type = TYPE_UNDEFINED;
	    arg = node_value(val);
	}
    }
    if( arg == NODENIL )
	return exception("SYSERR memory exhausted in install_args()");
    else
	return (put_symbol(procn->val, string_part(n->val), SYM_PARAMETER,arg));
}

/******************************************************************************
 *
 * 	I N T E R P R E T (node *n),  P R I N T (node *n) &  D E S T R O Y
 *								(node *n)
 *
 * Interpret, print and destroy are routines which will recursively descend a
 * parse tree, performing one of the named actions.
 *
 * Since there is only one function pointer in a node, a second parameter is
 * used to specify the action required.
 *
 * Each xxxxxx_interpret function is thus a switch statement on this action
 * action type.  Each of the actions may call one of these routines to
 * continue the recursion on any sub-nodes.
 *
 * There is a fourth action OP_FORMAT, but its use is specialised (to expr.c)
 * when handling the format operator.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	I N T E R P R E T (node *n)
 *
 ******************************************************************************
 */
value
interpret(node *n)
{
    if (n != NODENIL &&
	n->interpret != INTERPRETNIL)

	return (*n->interpret) (n, OP_INTERPRET);
    return trueval;
}

/******************************************************************************
 *
 *	D E S T R O Y (node *n)
 *
 ******************************************************************************
 */
void
destroy(node *n)
{
    if (n == NODENIL)
	return;
    else {
	int i;

	if (n->interpret != INTERPRETNIL)
	    (*n->interpret) (n, OP_DESTROY);
	if (n->n_nodes != 0) {
		for (i = 0; i < n->n_nodes; i++)
		    destroy(n->sub[i]);
#ifdef DEBUG
		fprintf(stderr, "free n->sub %x\n", n->sub);
#endif
		free((void *) n->sub);
	}
#ifdef DEBUG
	fprintf(stderr, "free n %x\n", n);
#endif
	free((void *) n);
    }
    return;
}

/******************************************************************************
 *
 *	P R I N T (node *n)
 *
 ******************************************************************************
 */
void
print(node *n)
{
    if (n != NODENIL && n->interpret != INTERPRETNIL) {
	(*n->interpret) (n, OP_PRINT);
	return;
    } else
	return;
}

/******************************************************************************
 *
 *	I N T E R P R E T _ T O _ R E A L (node *n)
 *
 * A node n is interpreted and then converted to a real value (using
 * real_val() in value.c).
 * An exception may occur during this process and this is returned to the
 * caller if this occurs. If n is meaningless then realzeroval (0.0) is
 * returned.
 *
 ******************************************************************************
 */
value
interpret_to_real(node *n)
{
    if (n != NODENIL && n->interpret != INTERPRETNIL)
	return (real_val((*n->interpret) (n, OP_INTERPRET)));
    else
	return realzeroval;
}

/******************************************************************************
 *
 *	I N T E R P R E T _ T O _ N U M E R I C (node *n)
 *
 * A node 'n' is interpreted and then converted to a numeric value.
 * An exception may occur during this process and this is returned to the
 * calle. If n is meaningless then intzeroval (0) is returned.
 *
 ******************************************************************************
 */
value
interpret_to_numeric(node *n)
{
    value val;

    if (n != NODENIL && n->interpret != INTERPRETNIL)
	if (isexc(val = as_nonstring((*n->interpret) (n, OP_INTERPRET))))
	    return val;
	else if (isintegertype(val) || isrealtype(val))
	    return val;
	else
	    return exception(
		"INVARGMAT  A non-numeric value is being used numerically");
    else
	return intzeroval;
}

/******************************************************************************
 *
 *	I N T E R P R E T _ T O _ I N T E G E R (node *n)
 *
 * A node n is interpreted and then converted to a integer value
 * (integer_val() in value.c).  An exception may occur during this process
 * and this is returned to the caller if this occurs. If n is meaningless
 * then intzeroval (0) is returned.
 *
 ******************************************************************************
 */
value
interpret_to_integer(node *n)
{
    if (n != NODENIL && n->interpret != INTERPRETNIL)
	return (integer_val((*n->interpret) (n, OP_INTERPRET)));
    else
	return intzeroval;
}

/******************************************************************************
 *
 *	I N T E R P R E T _ T O _ S T R I N G (node *n)
 *
 * A node n is interpreted and then converted to a string value (string_val()
 * in value.c).  An exception may occur during this process and this is
 * returned to the caller if this occurs. If n is meaningless then
 * nullstringvalval ((char *)0) is returned.
 *
 ******************************************************************************
 */
value
interpret_to_string(node *n)
{
    if (n != NODENIL && n->interpret != INTERPRETNIL)
	return (string_val((*n->interpret) (n, OP_INTERPRET)));
    else
	return nullstringval;
}

/******************************************************************************
 *
 *		E X P L I S T _ I N T E R P R E T (node *n, int op)
 *
 * Explist nodes are generated to form the formal parameter list in a PROC
 * header and to form a list of actual arguments on a function call.
 * Both these are handled specially and should not be directly interpreted.
 *
 ******************************************************************************
 */
value
explist_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	systemfail("explist_interpret: attempt to INTERPRET an explist!\n");
	return noval;	/* Never reached : for lint */
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(", ");
	print(n->sub[1]);
	return noval;
      case OP_FORMAT:
	return noval;
      case OP_DESTROY:
	return noval;
      default:
	systemfail("explist_interpret: unknown action for an explist!\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	A B B U T _ I N T E R P R E T (node *n, int op)
 *
 * An abbut node is used to hold the list of arguments to a procedure call.
 *
 * It is a linked list formed through the the sub[1] member of each node.
 * The sub[0] member points EITHER to
 * 	a string_interpret() node or a open_string_interpret() node
 * OR
 * 	a paren_interpret() node (ie argument was of the form  (...) )
 *
 * The value.logical part of the abbut_interpret node controls the spacing
 * used. TRUE will cause a space to be inserted between the strings while
 * FALSE implies that the strings are concatonated.
 *
 * When interpreted abbut_interpret evaluates any evaluable actual arguments
 * a strings (the paren_interpret() nodes) and then concatenates these with
 * those obtained from string_interpret/open_string_interpret() into a
 * string value.
 *
 ******************************************************************************
 */
value
abbut_interpret(node *n, int op)
{
    value val1, val2;
    switch (op) {
      case OP_INTERPRET:
	if (isexc(val1 = interpret_to_string(n->sub[0])))
	    return val1;
	else if (isexc(val2 = interpret_to_string(n->sub[1])))
	    return val2;
	else {
	    char *w, *w1;

            w = string_part(val1);
	    if(logical_part(n->val))
		w1 = strconcat(w, " ");
	    else
		w1 = w;
	    if( w1 == CHARNIL ||
		(w = strconcat(w1, string_part(val2))) == CHARNIL)
		return exception(
			"SYSERR  memory exhausted in abbut_interpret()");
	    else
		return value_string(w);
	}

      case OP_PRINT:
	print(n->sub[0]);
	if( logical_part(n->val) )
	    outfpstring(" ");
	print(n->sub[1]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("abbut_interpret: unknown action for an abbut!\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	A B B U T _ Q S T R I N G _ I N T E R P R E T  (node *n, int op)
 *
 * This function is similar to abbut_interpret() but, rather than just
 * concatonating strings it will restore the quotes which were stripped by
 * fixup_quotes() in lex.
 *
 * Used to pass "value strings" to ADAM tasks when ICL and the task are
 * non-interlocked. For example SEND <task> OBEY <action> <parameters>
 * (cf setup_task_hds() )
 *
 *
 ******************************************************************************
 */
value
abbut_qstring_interpret(node *n, int op)
{
    value val1, val2;
    char *vs, *v1s, *v2s;

    if( op != OP_INTERPRET)
	return abbut_interpret( n, op);

/* Put the parse structure back to what it was */
    n->interpret = abbut_interpret;

    if( n->sub[0]->interpret == string_interpret )
	v1s = restore_adamstring(string_part(n->sub[0]->val) );
    else {
	if( isexc(val1 = interpret(n->sub[0])))
	    return val1;
	else
	    v1s = string_part(string_val(val1) );
    }
    if( n->sub[1]->interpret == string_interpret )
	v2s = restore_adamstring(string_part(n->sub[1]->val) );
    else {
	if( isexc(val2 = interpret(n->sub[1])))
	    return val2;
	else
	    v2s = string_part(string_val(val2) );
    }
    if ((vs = strconcat(v1s, " ")) == CHARNIL ||
        (vs = strconcat(vs, v2s))  == CHARNIL )
	return exception (
		"SYSERR  memory exhausted in abbut_qstring_interpret()");
    else
	return value_string(vs);
}

/******************************************************************************
 *
 *	P A R A M E T E R _ I N T E R P R E T (node *n, int op)
 *
 * Interpret a formal parameter to a procedure.
 *
 * As we call a procedure we pass the actual arguments to the formals by:
 *
 * Walking the formal parameter list (which is the sub[0] member of the PROC
 * entry) and for each we select the corresponding actual argument.  If the
 * actual argument is a name_interpret() node we are passing by reference and
 * we add the formal to the procedure's symbol table as SYM_PARAMETER with a
 * value node that points to a parameter_interpret() node whose sub[0] member
 * points to the actual's name_interpret() node (whose string_part() member is
 * the actual's identity). The val member is a TYPE_SYMTAB value with its
 * symboltable_part() member pointing to the symbol table of the proc owning
 * the actual.
 *
 * If the corresponding actual was an expression we evaluate it and set the
 * formal's SYM_PARAMETER entry's value member to point to the node
 * representing that value.
 *
 * In both action modes, we should never interpret OR print such a node.
 *
 ******************************************************************************
 */
value
parameter_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	bufstring("attempt to interpret parameter \"");
	bufstring(string_part(n->val));
	bufstring("\"\n");
	flshbuf();
	abort();
	return noval;

      case OP_PRINT:
	outfpstring("<parameter - should never be seen - ");
	print(n->sub[0]);
	outfpstring(">");
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("parameter_interpret: unknown action for a parameter!\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 * 		E X C E P T I O N S
 *
 * When a PROC or HIDDEN_PROC is parsed successfully it sets up an entry in
 * the "world" symbol table.  This entry points to a procedure_interpret()
 * node whose sub[3] member is either NODENIL or points to linked list of
 * exception entries.
 *
 * exception_list_interpret() nodes have the following format:
 *
 * sub[0] points to the next node in the list.
 * sub[1] points to either an exception_interpret() node  directly or a
 *        comment_interpret() node whose sub[0] component points to the
 *        exception_interpret() node.
 *
 * Each exception_interpret node has the following structure:
 *
 * value.string is the name of the exception handled.
 * sub[0] is NODENIL or a comment_interpret() node containing the EXCEPTION
 *        line comment
 * sub[1] is the body of the exception
 * sub[2] is NODENIL or a comment_interpret() node containing the END EXCEPTION
 *        line comment
 * sub[3] is NODENIL or points to a node structure for the comment lines AFTER
 *        the "endexception" but before the next exception (or ENDPROC)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	F I N D _ H A N D L E R (node *n, value exc)
 *
 * This routine searches the exception list for the current procedure for an
 * exception handler which matches the given exception name 'exc'. An
 * exception value is a string, the first characters of which
 * contain the name of the exception terminated by a space.
 *
 * iclidentityequal(s,t,n) (utils.c) is used to compare two strings while
 * ignoring case and comparing only the first n characters.
 *
 ******************************************************************************
 */
static node *
find_handler(node *n, value exc)
{
    node *found;

    if (n == NODENIL)
	return NODENIL;
    else if (string_part(exc) == CHARNIL)
	return (NODENIL);
    else if (n->interpret == exception_list_interpret)
	if ((found = find_handler(n->sub[0], exc)))
	    return found;
	else
	    return find_handler(n->sub[1], exc);
    else if (n->interpret == comment_interpret)
	return (find_handler(n->sub[0], exc));
    else if (n->interpret == exception_interpret)
	if (iclidentityequal(string_part(n->val), string_part(exc),
			     strlen(string_part(n->val))))
	    return n;
	else
	    return NODENIL;
    else
	return NODENIL;
}

/******************************************************************************
 *
 *	E X C E P T I O N _ I N T E R P R E T (node *n, int op)
 *
 * An exception is just a sub-tree. It is executed in the same context as the
 * rest of a procedure's statements, so no special action is necessary.
 *
 * The structure has been described earlier
 *
 ******************************************************************************
 */
value
exception_interpret(node *n, int op)
{
    int status;

    switch (op) {
      case OP_INTERPRET:
	return interpret(n->sub[1]);

      case OP_PRINT:
	indent();
	outfpstring("EXCEPTION ");
	outfpstring(string_part(n->val));
	print(n->sub[0]);		/* EXCEPTION line comment */
	outfpstring("\n");
	if (n->sub[1] != NODENIL) {     /* body of the exception */
	    ++indent_count;
	    indent();
	    print(n->sub[1]);
	    --indent_count;
	    outfpstring("\n");
	}
	indent();
	outfpstring("END EXCEPTION");
	if (n->sub[2] != NODENIL)	/* END EXCEPTION comment */
	    print(n->sub[2]);
        if (n->sub[3] != NODENIL) {	/* trailing comment line(s) */
	    outfpstring("\n");
            indent();
	    print(n->sub[3]);
         }

	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail(
		"exception_interpret(): unknown op code for an exception!");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	E X C E P T I O N _ L I S T _ I N T E R P R E T (node *n, int op)
 *
 * This function ensures that exceptions in a procedure can be LISTed
 * correctly
 *
 ******************************************************************************
 */
value
exception_list_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return trueval;

      case OP_PRINT:
	print(n->sub[0]);
	outfpstring("\n");
	print(n->sub[1]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail(
	   "exception_list_interpret(): unknown op code");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *		P R O C E D U R E   E X E C U T I O N
 *
 * On encountering a PROC definition yacc sets up a SYM_PROC/SYM_HIDDEN_PROC
 * entry in the world symbol table.  This entry points to a
 * procedure_interpret() node with the following structure:
 *
 * The node value is of TYPE_SYMTAB and is created pointing to a
 * symbol table dummy header. This table is used to hold local variables and
 * parameters when the procedure is called.
 *
 * sub[0] points to the formal parameter list. This is a explist_interpret()
 * node structure linked together using their sub[0] entries. The sub[1]
 * entries point to name_interpret() nodes recording the names of the formal
 * parameters in their value.string components. The formal parameters are
 * in reverse order.Any comment_interpret() nodes are ignored during
 * interpretation but reconstruct any PROC line comments on output.
 *
 * The sub[1] entry points to the proc body node structure
 * The sub[2] entry points to a node_comment containing any ENDPROC line
 *     comment
 * The sub[3] entry points to execptions list
 *
 * When we interpret a proc or function node we must set up the formal
 * paramters in the procedures symbol table with their actual values and then
 * execute the body.  On encountering a procedure or function call,
 * the actual parameter nodes are linearised into the array arglist[], and
 * the count 'nargs' is set to the number of arguments (see get_args() and
 * the *_call_interpret() functions in control.c).
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	P R O C E D U R E _ I N T E R P R E T (node *n, int op)
 *
 * This routine interprets a user procedure.
 *
 * The value part of a procedure node points to a permanent symbol table.
 * In this way VARS can interrogate a procedure about values of variables
 * at any time. It also means, however,  that procedures cannot be recursive,
 * since symbol tables would have to be stacked.
 *
 * The actual parameters are entered into the procedure's symbol table by
 * install_args() and the currently active ICL symbol table is saved so we
 * can restore it on completion.
 *
 * We execute the procedure body which, when it returns, may have generated
 * an exception. If so, we look for a handler and execute it.
 * If an exception occurs during the handler, we trap that too.
 *
 * There is a potential here for infinite looping, but as we need to trap
 * CTRLC reliably, it has to be like this for now. Once the exception has been
 * handled, we can unstack the active symbol table and return.
 *
 * Note that we also restore the call stack level before starting
 * an exception handler using the saved value which has been saved on entry
 * by pushcalllevel(). popcalllevel() is finally used to clear the recorded
 * value.
 *
 ******************************************************************************
 */
value
procedure_interpret(node *n, int op)
{
    extern int uface_interrupt(void);				/* uface.c */
    extern value pushcalllevel(void),
		 restorecalllevel(void),
		 popcalllevel(void);				/* main.c */
    value val, val1;
    node *handler;
    char *procname = currentproc();
    int status;

    switch (op)
      case OP_INTERPRET:
    {

	if (isexc(val1 = pushcalllevel()))
	    return (val1);
	arglistp = arglist;
        optargs = 0;
	if (isexc(val = install_args(n->sub[0], n)))
	    return val;
	if (nargs != 0)
	    return exception(
		"TOOMANYPARS  Too many parameters for a procedure or command");
	if (isexc(val1 = savesymboltablecontext(n)))
	    return val1;
	if (trace) {
	    outfpstring("PROC ");
	    outfpstring(procname);
	    if (n->sub[0] != NODENULL) {
		outfpchar(' ');
		if (n->sub[0]->interpret == optarg_name_interpret_end)
		    outfpchar('[');
		print(n->sub[0]);
	    }
	    outfpstring("\n");
	    flshbuf();
	}
/* Execute the procedure - stack any EMS errors */
	emsMark();
	val = interpret(n->sub[1]);

/* If the procedure has generated an exception we output EMS errors
 * UNLESS there is an exception handler in force.
 */
	while (isexc(val) && (handler = find_handler(n->sub[3], val)))
	    if (uface_interrupt())
		val = exception("CTRLC Interrupt during procedure execution");
	    else {
		if (!isexc(val = restorecalllevel())) {
		    emsAnnul(&status);
		    val = interpret(handler);
		}
	    }

/* Output any remaining pending error messages */
	iclems_flush();
	emsRlse();

/* Tidy up the "pass by reference" procedure parameters to avoid problems
 * when next called
 */
	delete_sym_parameters(n->val);

	if (isexc(val1 = restoresymboltablecontext()))
	    return (val1);
	else if (isexc(val1 = popcalllevel()))
	    return (val1);
	if (trace) {
	    outfpstring("ENDPROC ");
	    outfpstring(procname);
	    outfpstring("\n");
	    flshbuf();
	}
	return val;

      case OP_PRINT:
	if (n->sub[0] != NODENULL)
	    if (n->sub[0]->interpret == optarg_name_interpret_end)
		outfpchar('[');
	print(n->sub[0]);
	outfpstring("\n");
	if (n->sub[1] != NODENIL) {
	    ++indent_count;
	    indent();
	    print(n->sub[1]);
	    outfpstring("\n");
	    --indent_count;
	}
	if (n->sub[3] != NODENIL) {
	    ++indent_count;
	    print(n->sub[3]);
	    outfpstring("\n");
	    --indent_count;
	}
	outfpstring("END PROC ");
	outfpstring(n->sub[2] ? string_part(n->sub[2]->val) : "");
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("procedure_interpret(): unknown op code");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	B U I L T I N _ I N T E R P R E T (node *n, int op)
 *
 * Built-in commands are stored in a node as a function pointer.
 * They do all their own argument processing, as built-ins are so diverse.
 *
 ******************************************************************************
 */
value
builtin_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return (*(function_part(n->val))) (all_args);

      case OP_PRINT:
	outfpstring("\n");
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("builtin_interpret(): unknown op code");
	return noval;		/* for lint */
    }
}

/******************************************************************************
 *
 *	D E F S T R I N G _ I N T E R P R E T (node *n, int op)
 *
 * Implements the ICL command "DEFSTRING command equiv_string"
 *
 * This function up a HIDDEN_PROC on execution of proc_defstring() with the
 * name, the evaluated first argument (which must deliver a string) and sub[0]
 * member set pointing to a defstring_interpret() node with a val member
 * set to the value that resulted in evaluating the equivalence string.
 *
 * When the command is subsequently executed, interpreting the hidden
 * proc causes def_string_interpret() to be executed.
 *
 * To execute a DEFSTRING which has previously been created (by proc_defstring)
 * we need to append any arguments to the stored equivalence value (held in
 * n->val) interpreted as a string, and then re-parse * the resulting string.
 *
 * We use the push-down stack of input sources to allow the parser to be
 * re-invoked.
 *
 * Since syntax errors generate an exception now, we can be robust about
 * parsing failures.
 *
 ******************************************************************************
 */
value
defstring_interpret(node *n, int op)
{
    extern int stack_string_input(char *s, int length);		/* input.c */
    extern void unstack_input(void);				/* input.c */

    int i;
    value val;
    char buf[ICL_BUFSIZE];
    node *argv;
    char *s;

    switch (op) {
      case OP_INTERPRET:
	if (all_args != NODENIL) {
	    sprintf(buf, "%s ", string_part(n->val));
	    i = 0;
            do {
		argv = arglist[i++];
		if (argv->interpret == paren_interpret) {
		    if(argv->sub[0]->interpret == name_interpret)
			s = string_part(argv->sub[0]->val);
		    else
			s = string_part(interpret_to_string(argv->sub[0]));
		    sprintf(&buf[strlen(buf)], "(%s) ", s);
		} else if (argv->interpret == openstring_interpret)
		    sprintf(&buf[strlen(buf)], "%s ",string_part(argv->val));
		else if (argv->interpret == string_interpret) {
		    s = restore_iclstring(string_part(argv->val));
		    sprintf(&buf[strlen(buf)], "%s ", s);
		} else
		    systemfail("defstring_interpret() - internal failure\n");
	    } while(--nargs);
	    strcat(buf, "\n");
	} else
	    sprintf(buf, "%s\n", string_part(n->val));
	(void) stack_string_input(buf, (int) strlen(buf));
	todo = NODENIL;
	(void) yyparse();
	unstack_input();
	if (todo == NODENIL)
	    return trueval;
	else if (isexc(val = interpret(todo))) {
	    todo = NODENIL;
	    return val;
	} else {
	    todo = NODENIL;
	    return trueval;
	}
      case OP_PRINT:
	return noval;
      case OP_FORMAT:
	return noval;
      case OP_DESTROY:
	return noval;
      default:
	systemfail("defstring_interpret: unknown action for a defstring!\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	D E F P R O C _ I N T E R P R E T (node *n, int op)
 *
 * Implements the ICL command "DEFPROC command filename [procedure]".
 *
 * When a DEFPROC is encoutered a command_line node is created with
 * interpreter proc_defproc().
 *
 * When this is interpreted it sets up a PROC_HIDDEN symbol table entry (one
 * for each abbreviation) whose value member points to a defproc_interpret()
 * node whose string_part() member contains the filename and whose sub[0]
 * memberpoints to a string node. The  string_part() member of this node
 * records the  procedure name (same as the command name if no procedure name
 * given) and whose sub[1] member points to a string node whose string_part()
 * member records the command name
 *
 * To execute an existing DEFPROC definition, we need to load the named file
 * and interpret the procedure object which may (or may not) be defined by
 * the load.  The arguments provided to 'command' will have been processed by
 * get_args() for command and so are available to the procedure.
 *
 * We are supposed to delete the DEFPROC object and all its associated
 * abbreviations after this, but we don't currently. This is a bug.
 *
 ******************************************************************************
 */
value
defproc_interpret(node *n, int op)
{
    extern value do_load (char *whofor, char *filenametobeloaded); /* input.c */
    extern value pushcallstack(char *), popcallstack(void);	   /* main.c  */

    value val, val1;
    char *filename, *unixfilename, *procedurename;
    node *proc;
    extern char *icl_command;					 /* control.c */

    switch (op) {
      case OP_INTERPRET:
	procedurename = string_part(n->sub[0]->val);
	if ((proc = lookup_proc(procedurename)) == NODENIL) {

/* Need to load file containing procedures */
	    filename = string_part(n->val);
	    if (strcmp(&filename[strlen(filename) - 4], ".icl") )
		filename = strconcat(filename, ".icl");
	    if (!expand_name(&unixfilename, filename))
		return exception2("DEFPROC - cannot expand filename %s - %s",
				  filename, unixfilename);
	    bufstring("Loading procedure file ");
	    bufstring(filename);
	    bufnewline();
	    flshbuf();
	    if (isexc(val = do_load("DEFPROC", unixfilename)))
		return val;
	    else if ((proc = lookup_proc(procedurename)) == NODENIL)
		return exception2(
		    "PROCERR DEFPROC procedure \"%s\" not in file \"%s\"\n",
		     procedurename, filename);
	}
	if (isexc(val1 = pushcallstack(procedurename)))
	    return val1;
	if (isexc(val = interpret(proc)))
	    return val;
	if (isexc(val1 = popcallstack()))
	    return val1;
	return val;

      case OP_PRINT:
	return noval;
      case OP_FORMAT:
	return noval;
      case OP_DESTROY:
	return noval;
      default:
	systemfail("defproc_interpret: unknown action for defproc!\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	H E L P _ I N T E R P R E T (node *n, int op)
 *
 * Implements the ICL command "HELP topic1 topic2 ...
 *
 * A help_interpret node is set up by the DEFHELP command
 * or by the HELP command.
 *
 * Interpreting a help_interpret node results in the required help
 * information being output.
 *
 * The arguments of the HELP command are added to the topic string set
 * in the node.
 *
 ******************************************************************************
 */
value
help_interpret(node *n, int op)
{
    extern F77_SUBROUTINE(uface_pwhlp) (CHARACTER(helplib), CHARACTER(topic),
	INTEGER(libsrch), INTEGER(status) TRAIL(helplib) TRAIL(topic) );
    value val;
    char *library;
    char buf[ICL_BUFSIZE];
    char *space=" ";
    char *topicstart;
    int libsrch;
    DECLARE_CHARACTER( topic_f, 132 );
    DECLARE_CHARACTER( helplb_f, 132 );
    DECLARE_INTEGER( status );

    switch (op) {
      case OP_INTERPRET:
        library = string_part(n->val);
        if ( n->sub[0] == NODENIL )
          topicstart = space;
        else {
          if( isexc( val= interpret(n->sub[0] ) ) ) return val;
          topicstart = string_part(val);
        }

	if (nargs > 1) {
/*        Get the full topic string */
	    if (isexc(val = concat_args(1)))
		return val;
	    sprintf(buf, "%s %s", topicstart,
                    string_part(val));
	} else
	    sprintf(buf, "%s", topicstart );

        cnf_exprt( buf, topic_f, 132 );
        cnf_exprt( library, helplb_f, 132 );
        libsrch = 1;
        status = SAI__OK;
        F77_CALL(uface_pwhlp)( CHARACTER_ARG(topic_f),
                           CHARACTER_ARG(helplb_f),
                           INTEGER_ARG(&libsrch), INTEGER_ARG(&status)
                           TRAIL_ARG(topic_f) TRAIL_ARG(helplb_f) );

        if ( status != SAI__OK )
            return exception("HELPERR");
        else
            return trueval;

      case OP_PRINT:
	return noval;
      case OP_FORMAT:
	return noval;
      case OP_DESTROY:
	return noval;
      default:
	systemfail("help_interpret: unknown action for a help!\n");
	return noval;	/* for lint */
    }
}
