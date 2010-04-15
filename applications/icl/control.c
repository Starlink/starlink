/******************************************************************************
 *
 *	CONTROL.C
 *
 * Routines for interpreting and printing (regenerating) the parse sub-trees
 * which make up ICL control structures.
 *
 *	History
 *	Created :	S.K.Robinson	7/11/91
 *	Edited :	I.R.Jenkins	02/06/92
 *			Added conditional prototyping headers to functions
 *			Added Sccsid for use in version management
 *	Tidied :	B.K.McIlwrath	16/07/93
 *			Tidied, function comments and reformatted by 'indent'
 *      Tidied :        A.J.Chipperfield 21/06/99
 *                      Use new style ems function names
 *
 ******************************************************************************
 */
#include "icl.h"
#include "control.h"
#include "interp.h"
#include "symtab.h"
#include "parse.h"
#include "ems.h"
#include "output.h"
#include "carith.h"
#include "interp.h"

extern int uface_interrupt(void);                            /* uface.c */


char *icl_command;

/*
 * indent_count is used when printing to determine how many levels of nesting
 * we are at
 */
int indent_count = 0;

/******************************************************************************
 *
 *	I N D E N T (void)
 *
 ******************************************************************************
 */
void
indent(void)
{
    int i;

    for (i = 0; i < indent_count; i++)
	outfpstring("  ");
    return;
}

/******************************************************************************
 *
 *	D I S P L A Y _ I N T E R P R E T (node *n, int op)
 *
 * Assignment to an absent variable in ICL (eg. =EXPRESSION) prints the value
 * of the expression on the left. This generates a display_interpret() node
 * whose sub[0] member points to the expression node corresponding to the
 * expression on the right.
 *
 * This is treated as a distinct statement type
 *
 ******************************************************************************
 */
value
display_interpret(node * n, int op)
{
    value val;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val = interpret(n->sub[0])))
	    return val;
	else {
	    value_print(val);	/* in value.c */
	    bufnewline();
	    flshbuf();
	    return trueval;
	}

      case OP_PRINT:
	outfpstring("= ");
	print(n->sub[0]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to display_interpret()\n");
	return noval;		/* Never reached : for lint */
    }
}

/******************************************************************************
 *
 *	A S S I G N _ I N T E R P R E T (node *n, int op)
 *
 * Processes the assignment statement.
 *
 * Expressions of the form VARIABLE = EXPRESSION generate an assign_interpret()
 * node whose sub[0] member points to a name_interpret() node whose string_part
 * points to the identifier of the l.h.s. and whose sub[1] member points to
 * the expression node corresponding to the right hand side.
 *
 ******************************************************************************
 */
value
assign_interpret(node * n, int op)
{
    value val;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val = interpret(n->sub[1])))
	    return val;
	else
	    return (assign_helper(n->sub[0], val));

      case OP_PRINT:
	outfpstring(string_part(n->sub[0]->val));
	outfpstring(" = ");
	print(n->sub[1]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to assign_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	I F _ I N T E R P R E T (node *n, int op)
 *
 * Processes the IF statement.
 *
 * The syntax:
 *
 * if expression comment
 *    line_list
 * else comment
 *    linelist
 * OR
 * if expression comment
 *    linelist
 * else if expression .....
 *
 * generate an if_interpret() node with sub[0] pointing to a comment node
 * whose sub[0] member points to an expression or the if_interpret() node's
 * sub[0] directly points to the expression node.  The if_interpret's
 * sub[1] member points to the if's linelist and the sub[2] member points to
 * the elsepart node which is either an else_interpret() node (string_part
 * being the comment string of the else) with the sub[0] member pointing to
 * the else's linelist.
 *
 * OR, in the case of an elseif, is another if_interpret() node but with
 * a integer_part set to 1.
 *
 * OR is nil when there is no elsepart
 *
 ******************************************************************************
 */
value
if_interpret(node * n, int op)
{
    value val;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val = interpret(n->sub[0])))
	    return val;
	else if (!islogicaltype(val))
	    return exception(
  "IFERR  Expression in IF or ELSE IF does not evaluate to logical value");
	else if (as_logical(val))
	    return interpret(n->sub[1]);
	else
	    return interpret(n->sub[2]);

      case OP_PRINT:
	outfpstring("IF ");
	print(n->sub[0]);
	outfpstring("\n");
	++indent_count;
	indent();
	print(n->sub[1]);
	--indent_count;
	if (n->sub[2] != NODENIL) {
	    outfpstring("\n");
	    indent();
	    outfpstring("ELSE ");
	    print(n->sub[2]);
	}
	if (!integer_part(n->val)) {	/* prevent printing nested if of
					 * elseif as if it had endif */
	    outfpstring("\n");
	    indent();
	    outfpstring("END IF");
	}
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to if_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	E L S E _ I N T E R P R E T (node *n, int op)
 *
 * Processes an ELSE statement.
 *
 * The sub[2] member of an if_interpret() node points to the elsepart node
 * which is either
 *
 * An else_interpret() node (string_part being the comment string of the else)
 * with the sub[0] member pointing to the else's linelist (DEALT with here)
 *
 * OR, in the case of an elseif, is another if_interpret() node but with
 * a integer_part set to 1.
 *
 * OR is nil when there is no elsepart
 *
 ******************************************************************************
 */
value
else_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return interpret(n->sub[0]);

      case OP_PRINT:
	outfpstring(" ");
	outfpstring((string_part(n->val) != CHARNIL ?
			string_part(n->val) : ""));
	outfpstring("\n");
	++indent_count;
	indent();
	print(n->sub[0]);
	--indent_count;
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to else_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	L O O P _ I N T E R P R E T (node *n, int op)
 *
 * Processes the LOOP syntax which is:
 *
 *	LOOP comment
 *	linelist
 *	ENDLOOP
 * or
 *	LOOP WHILE expression comment
 *	linelist
 *	ENDLOOP
 * or
 *	LOOP FOR name = expression TO expression [STEP expression] comment
 * 	linelist
 *	ENDLOOP
 *
 * The first case generates a loop_interpret() node whose sub[0] member points
 * to a while_interpret() node (whose string_part node holds the comment)
 * whose sub[0] member is nil. The loop_interpret's sub[1] member points to
 * the linelist.
 *
 * The second case generates a loop_interpret() node whose sub[0] member points
 * to a while interpret() node (whose string_part node holds the comment)
 * whose sub[0] member points to the while expression node.
 * The loop_interpret's sub[1] member points to the linelist.
 *
 * The third case generates a loop_interpret() node whose sub[1] member points
 * to the linelist and whose sub[0] member points to a for_interpret() node
 * (whose string_part member holds the comment).
 * The  for_interpret() has four sub[] members
 *   sub[0] points to a name_interpret() node (with string_part set to the
 *          control variable's identity)
 *   sub[1] points to the initial_expression node
 *   sub[2] points to the end_expression node
 * and
 *   sub[3] points to the step_interpret() node whose sub[0] member points to
 *	    the step expression (set to the constant 1 if no step part in the
 *	    parser)
 *
 * Checking of the exception type is centralised, as BREAK statements are
 * executed by a special exception.
 *
 * Each kind of loop has to check for keyboard interrupts, as although they all
 * execute line_lists, a empty line list is NULL and thus does not contain a
 * check.
 *
 ******************************************************************************
 */
value
loop_interpret(node *n, int op)
{
    value val;

    switch (op) {
      case OP_INTERPRET:
	val = trueval;
	if (n->sub[0] == NODENIL || n->sub[0]->sub[0] == NODENIL)
	    /* LOOP .... ENDLOOP */
	    while (!uface_interrupt())
		if (isexc(val = interpret(n->sub[1])))
		    break;
		else
		    continue;
	else if (n->sub[0]->interpret == while_interpret)
	    /* LOOP WHILE ....... ENDLOOP */
	    while (!uface_interrupt())
		if (isexc(val = interpret(n->sub[0]->sub[0])))
		    break;
		else if (!islogicaltype(val))
		    return exception(
	"WHILEERR  Expression in WHILE does not evaluate to logical value");
		else if (as_logical(val))
		    if (isexc(val = interpret(n->sub[1])))
			break;
		    else
			continue;
		else
		    break;
	else if (n->sub[0]->interpret == for_interpret) {
	   /* LOOP FOR ....... ENDLOOP */
	    extern value func_nint( value val );	/* carith.c */
	    value valfrom, valto, valstep, vval;
	    node *for_part = n->sub[0];

	    if (isexc(valfrom = as_nonstring(interpret(for_part->sub[1]))))
		return valfrom;
	    if (!isintegertype(valfrom))
		valfrom = func_nint(valfrom);
	    vval = valfrom;
	    if (isexc(val = assign_helper(for_part->sub[0], vval)))
		return val;
	    if (isexc(valto = as_nonstring(interpret(for_part->sub[2]))))
		return valto;
	    if (!isintegertype(valto))
		valto = func_nint(valto);
	    if (isexc(valstep = as_nonstring(interpret(for_part->sub[3]))))
		return valstep;
	    if (!isintegertype(valstep))
		valstep = func_nint(valstep);
	    val = trueval;
	    while (!uface_interrupt()) {
		if (integer_part(valstep) < 0) {
		    if (integer_part(vval) < integer_part(valto))
			break;
		} else {
		    if (integer_part(vval) > integer_part(valto))
			break;
		}
		if (isexc(val = interpret(n->sub[1])))
		    break;
		if (isexc(valto = as_nonstring(interpret(for_part->sub[2])))) {
		    val = valto;
		    break;
		}
		if (!isintegertype(valto))
		    valto = func_nint(valto);
		if (isexc(valstep = as_nonstring(interpret(for_part->
								sub[3])))) {
		    val = valstep;
		    break;
		}
		if (!isintegertype(valstep))
		    valstep = func_nint(valstep);
		if (isexc(vval = do_operator(vval, ADD, valstep))) {
		    val = vval;
		    break;
		}
		if (isexc(val = assign_helper(for_part->sub[0], vval)))
		    break;
	    }
	} else {
	    systemfail("loop with unknown condition type!\n");
	    return noval; /* for lint */
	}
	if (uface_interrupt()) {
	    return exception("CTRLC  Control-C entered on the terminal");
	} else if (isexc(val)) {
	    if (string_part(val) == CHARNIL)
		return trueval;	/* we had a break during loop */
	    else
		return val;
	} else
	    return trueval;

      case OP_PRINT:
	outfpstring("LOOP ");
	print(n->sub[0]);
	outfpstring("\n");
	++indent_count;
	indent();
	print(n->sub[1]);
	outfpstring("\n");
	--indent_count;
	indent();
	outfpstring("END LOOP");
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to loop_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	W H I L E _ I N T E R P R E T (node *n, int op)
 *
 * Processes syntax of the form:
 *	LOOP WHILE expression comment
 *	linelist
 *	ENDLOOP
 *
 * This case generates a loop_interpret() node whose sub[0] member points to
 * a while interpret() node (whose string_part node holds the comment) and
 * whose sub[0] member points to the while expression node.
 *
 * As such the while_interpret() node is never directly executed here (see
 * loop_interpret()) but it can be printed.
 *
 ******************************************************************************
 */
value
while_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	systemfail("System error - attempt to interpret while_interpret()\n");
	return noval;	/* for lint */

      case OP_PRINT:
	if (n->sub[0] != NODENIL) {
	    outfpstring("WHILE ");
	    print(n->sub[0]);
	    return noval;
	} else
	    return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to while_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	S T E P _ I N T E R P R E T (node *n, int op)
 *
 * Processes syntax of the form:
 *	LOOP FOR name = expression TO expression [STEP expression] comment
 *	linelist
 *	ENDLOOP
 *
 * This case generates a loop_interpret() node whose sub[1] member points to
 * the linelist and whose sub[0] member points to a for_interpret() node
 * (whose string_part member holds the comment).
 *
 * The for_interpret() node has four sub[] members
 *   sub[0] points to a name_interpret() node (with string_part set to
 *          the control variable's identity)
 *   sub[1] points to the initial_expression node
 *   sub[2] points to the end_expression node
 * and
 *   sub[3] points to the step_interpret() node
 *          whose sub[0] member points to the step expression
 *          (set to the constant 1 if no step part in the parser)
 *
 ******************************************************************************
 */
value
step_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return interpret(n->sub[0]);

      case OP_PRINT:
	outfpstring(" STEP ");
	print(n->sub[0]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to step_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	F O R _ I N T E R P R E T (node *n, int op)
 *
 * Processes syntax of the form:
 *	LOOP FOR name = expression TO expression [STEP expression] comment
 *	linelist
 *	ENDLOOP
 *
 * This case generates a loop_interpret() node whose sub[1] member points to
 * the linelist and whose sub[0] member points to a for_interpret() node
 * (whose string_part member holds the comment).
 *
 * The  for_interpret() has four sub[] members
 *   sub[0] points to a name_interpret() node (with string_part set to
 *          the control variable's identity)
 *   sub[1] points to the initial_expression node
 *   sub[2] points to the end_expression node
 * and
 *   sub[3] points to the step_interpret() node
 *          whose sub[0] member points to the step expression
 *          (set to the constant 1 if no step part in the parser)
 *
 * As such the for_interpret node is never executed here (see loop_interpret()
 * but is responsible for printing.
 *
 ******************************************************************************
 */
value
for_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	systemfail("attempt to interpret for_interpret()\n");
	return noval;	/* for lint */

      case OP_PRINT:
	outfpstring("FOR ");
	print(n->sub[0]);
	outfpstring(" = ");
	print(n->sub[1]);
	outfpstring(" TO ");
	print(n->sub[2]);
	if (n->sub[3] != NODENIL)
	    print(n->sub[3]);
	if (string_part(n->val) != CHARNIL) {
	    outfpstring(" ");
	    outfpstring(string_part(n->val));
	}
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to for_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	B R E A K _ I N T E R P R E T (node *n, int op)
 *
 * The ICL command "BREAK" generates a break_interpret() node.
 *
 * The BREAK statement is implemented by raising a NULL exception.
 *
 * This cannot happen under any other circumstance and is trapped by the loop
 * interpreter.
 *
 ******************************************************************************
 */
value
break_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return exception(CHARNIL);

      case OP_PRINT:
	outfpstring("BREAK");
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to break_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	P R O C _ C A L L _ I N T E R P R E T (node *n, int op)
 *
 * Interpret a ICL procedure call OR an ICL command (which are treated in
 * a similar manner by this version.
 *
 * First we look to see if the procedure object is defined anywhere. If it
 * is not we assume it is an external command and hand the interpreted call
 * line to the operating system.
 *
 * To call a procedure, we first get the arguments into the global arglist[]
 * array using get_args(), then record the name of the procedure we are
 * calling on the call stack, interpret the procedure object we found and
 * finally pop the name off the call stack.
 *
 * If the procedure failed, we do not pop the call stack. This is so that if
 * the exception is not handled, we can print the call stack at the point of
 * failure when we reach the top level again.
 *
 ******************************************************************************
 */
value
proc_call_interpret(node *n, int op)
{
    extern node **arglistp;
    node *p;

    extern value checkcallstack(char *),
		 pushcallstack(char *),
		 popcallstack(void);			/* main.c  */
    extern char *currentproc(void);			/* main.c  */
    extern int os_command(char *command);		/* unix.c  */
    extern int trace;					/* procs.c */

    icl_command = string_part(n->val);

    switch (op) {
      case OP_INTERPRET:
	if (trace && currentproc() != CHARNIL) {
	    outfpstring(string_part(n->val));
	    outfpstring(" ");
	    print(n->sub[0]);
	    outfpstring("\n");
	    flshbuf();
	}
	if ((p = lookup_proc(icl_command)) != NODENIL) {
	    value val, val1;

	    if (isexc(val1 = checkcallstack(icl_command)))
		return val1;
	    all_args = n->sub[0];
	    arglistp = arglist;
	    nargs = 0;
	    get_args(n->sub[0]);
	    if (isexc(val1 = pushcallstack(icl_command)))
		return val1;
	    if (isexc(val = interpret(p)))
		return val;
	    if (isexc(val1 = popcallstack()))
		return val1;
	    return val;
	} else if ((p = lookup_symbol(icl_command, SYM_DEFINED)) != NODENIL ||
		   (p = lookup_symbol(icl_command, SYM_BUILTIN)) != NODENIL ) {
	    value val;

	    all_args = n->sub[0];
	    arglistp = arglist;
	    nargs = 0;
	    get_args(n->sub[0]);
	    emsMark();
	    val = interpret(p);
	    if (isexc(val)) {
		iclems_flush();
	    }
	    emsRlse();
	    return val;
	} else if( currentproc() != CHARNIL) {
	    return (exception1("PROCERR procedure \"%s\" unknown", icl_command));
        } else {
/*
 * We are running an operating system command
 */
	    value val;
	    char *command;

	    if (n->sub[0] != NODENIL) {
		char *actcom = CHARNIL;
		if (isexc(val = interpret_to_string(n->sub[0])))
		    return val;
		else
		    actcom = string_part(val);
	        if( (command = strconcat(icl_command, " "))   == CHARNIL ||
		    (command = strconcat(command, actcom)) == CHARNIL)
		    return exception(
			"SYSERR memory exhausted in proc_call_interpret()");
		free(actcom);
	    } else
		command = icl_command;

	    if (os_command(command) != 0)
		val = exception("Unix command failed");
	    else
		val = trueval;
	    (void) free(command);
	    return val;
	}

      case OP_PRINT:
	outfpstring(string_part(n->val));
	outfpstring(" ");
	print(n->sub[0]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to proc_call_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	B U I L T I N _ C A L L _ I N T E R P R E T (node *n, int op)
 *
 * This routine is called if a command has a # in front of it.
 *
 * The semantics are slightly different, because we are only prepared to
 * execute SYM_BUILTIN type objects. We do not allow  UNIX commands as being
 * built-ins.
 *
 ******************************************************************************
 */
value
builtin_call_interpret(node *n, int op)
{
    extern node **arglistp;
    node *p;
    extern value checkcallstack(char *),
		 pushcallstack(char *),
		 popcallstack(void);			/* main.c */
    value val, val1;

    icl_command = string_part(n->val);

    switch (op) {
      case OP_INTERPRET:
	if ((p = lookup_symbol(icl_command, SYM_BUILTIN)) == NODENIL)
	    return (exception1(
		"CMDERR built-in command \"%s\" unknown\n", icl_command));
	else {
	    all_args = n->sub[0];
	    arglistp = arglist;
	    nargs = 0;
	    get_args(n->sub[0]);
	}
	if (isexc(val1 = pushcallstack(icl_command)))
	    return val1;
	else if (isexc(val = interpret(p)))
	    return val;
	else if (isexc(val1 = popcallstack()))
	    return val1;
	else
	    return val;

      case OP_PRINT:
	outfpstring("# ");
	outfpstring(icl_command);
	outfpstring(" ");
	print(n->sub[0]);
	return noval;

      case OP_FORMAT:
	return noval;

      case OP_DESTROY:
	return noval;

      default:
	systemfail("Unknown operator code to builtin_call_interpret()\n");
	return noval;	/* for lint */
    }
}

/******************************************************************************
 *
 *	C O M M E N T _ I N T E R P R E T (node *n, int op)
 *
 * Comments have to be stored in the parse tree, as the LIST command does its
 * work by regenerating the source from the internal form. The VMS ICL stored
 * both source and internal forms. The new ICL canonicalises layout, which is
 * no bad thing.
 *
 * The comment_interpret() node either has a nil sub[0] member or the sub[0]
 * member points to the line node that the comment terminated.  The
 * exceptions to this are
 *	(1) Comments terminating an IF expression line when the comment
 * 	    node hangs off the sub[0] of the if_interpet() node and its own
 * 	    sub[0] member points to the expression of the if.
 *	(2) The comment after an ELSE when the commment is the string_part value
 *	    of the else_interpret() node.
 *	(3) The comment on a LOOP, LOOP WHILE or LOOPFOR statement when it
 *	    forms the string_part value of the while_interpret() or
 *	    for_interpret() node which hangs off the sub[0] member of the
 *	    loop_interpret() node.
 *	(4) The comment at the end of a PROC header hangs off the sub[0]
 *	    member of the procedure_interpret() node (pointed to from the
 *	    symbol table entry for the proc) and whose sub[0] member
 *	    points to the formal parameter list.
 *	(5) The comment on the ENDPROC statement which hangs off the sub[2]
 *	    member of the procedure_interpret() node (pointed to from the
 *	    symbol table entry for the procedure).
 *	(6) The comment at the end of an EXCEPTION header has, as its
 *	    sub[0] member a pointer to the exception_interpret() node
 *	(7) The comment of an ENDPROC statement hangs off the sub[0] member
 *	    of the exception_interpret() node
 *
 * Only the non-exceptional cases will be interpreted.
 *
 * Comments are added to statements by arranging that a comment object
 * contains the statement rather than the reverse.
 *
 * This means that we don't need a comment field on every statement node.
 * There is a trivial execution overhead associated with comments, however...
 *
 ******************************************************************************
 */
value
comment_interpret(node *n, int op)
{
    switch (op) {
      case OP_INTERPRET:
	return interpret(n->sub[0]);

      case OP_PRINT:
	print(n->sub[0]);
	outfpstring(" ");
	outfpstring(string_part(n->val));
	return noval;

      case OP_DESTROY:
	free(string_part(n->val));
	return noval;

      case OP_FORMAT:
	return noval;

      default:
	systemfail("Unknown operator code to comment_interpret()\n");
	return noval;	/* 	for lint */
    }
}

/******************************************************************************
 *
 *	L I N E _ L I S T _ I N T E R P R E T (node *n, int op)
 *
 * A list of statements is actually stored as a tree of linelist_interpret()
 * nodes. The left subtree (sub[0]) is the first statement,the right subtree
 * (sub[1]) is the rest of statements.
 *
 * Expression lists are stored the same way.
 *
 * We check for exceptions and keyboard interrupts after every statement
 * executed.
 *
 ******************************************************************************
 */
value
line_list_interpret(node *n, int op)
{
    value val;

    switch (op) {
      case OP_INTERPRET:
	if (isexc(val = interpret(n->sub[0])))
	    return val;
	else if (uface_interrupt())
	    return exception("CTRLC  Interrupted in statement list");
	else if (isexc(val = interpret(n->sub[1])))
	    return val;
	else if (uface_interrupt())
	    return exception("CTRLC  Interrupted in statement list");
	else
	    return val;
      case OP_PRINT:
	print(n->sub[0]);
	outfpstring("\n");
	indent();
	print(n->sub[1]);
	return noval;
      case OP_DESTROY:
	return noval;
      case OP_FORMAT:
	return noval;
      default:
	systemfail("Unknown operator code to line_list_interpret()");
	return noval;	/* for lint */
    }
}
