/******************************************************************************
 *
 *	SYMTAB.C	A series of symbol-table related routines
 *
 *	History
 *	Created :	S.K.Robinson	7/11/91
 *	Reformatted, tidied :
 *			B.K.McIlwrath	27/7/93 + 15/11/93
 *      Add assign_helper1 :
 *                      A.J.Chipperfield 24/1/93
 *
 * These routines implement operations on the ICL symbol tables.
 *
 * A symbol table is a dictionary of (name, type, node) triples.
 * They are represented using a doubley linked list which has a  dummy entry
 * at the top. This dummy entry removes the need for special cases when
 * dealing with the ends of the list and the list is actually a ring. If a
 * symbol is found during lookup, it is moved to the front of the list (if it
 * is not already there).  In this way, symbol tables organise themselves into
 * order of most frequent use.  The need for a more time-efficient data
 * structure is thus much reduced. Lookup is done by text string. For short
 * strings this is surprisingly efficient.
 * Letter case is not significant in lookup, following the VMS convention.
 *
 * The abstract data type symtab has two variables active during execution:
 *
 * world   is a pointer to the global symbol table, which contains all the
 *	   built-ins, user procedures, direct-mode variables and so on.
 *
 * symbols points to the currently active symbol table. This is world in
 *	   direct mode, and a procedure's private symbol table when in a user
 *         procedure. This exists so that things like assignment can be done
 *	   without regard to context.
 *
 * A procedure's local variables and parameters are held in its own symbol
 * table pointed to by that proc's symbol table entry in the world symbol
 * table. During execution of that proc, symbols is set to point to this
 * table. Everything else is held in the global world table.
 *
 * Therefore, when we look-up an identifier during execution, if it is a
 * variable or a parameter we use "symbols" to tell us which list to look
 * up.  If not of this type then we always search the world_list.
 *
 ******************************************************************************
 */

#include <stdio.h>
#include <unistd.h>
#include "icl.h"
#include "symtab.h"
#include "expr.h"
#include "interp.h"
#include "node.h"
#include "output.h"


/******************************************************************************
 * The structure of symbol table entries
 ******************************************************************************
 */
typedef struct _symtab {
    struct _symtab *next, *prev;
    char *name;
    node *value;
    int type;
}   symtab;

/******************************************************************************
 *
 * symboltable_part() is exported by value.c but should only be used here as
 * it returns a pointer to a symboltable and this module is the only one that
 * knows about such things!
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	The definition of a nil symbol table pointer SYMBNIL and
 *      empty tables initialise 'world' and 'symbols.
 *
 ******************************************************************************
 */
static symtab *SYMBNIL = (symtab *) 0;
static symtab world_syms =
    {&world_syms, &world_syms, CHARNULL, NODENULL, SYM_NOTHING};
static symtab *world = &world_syms;
static symtab *symbols = &world_syms;

/******************************************************************************
 *
 * A procedure's local variables and parameters are held in its own symbol
 * table pointed to by that procedure's symbol table entry in the world
 * symbol table.
 * During execution of that procedure, "symbols" is set to point to this table.
 * Everything else is held in the global world table.
 * Therefore, when we look-up an identifier during execution, if it is a
 * variable or a parameter we use "symbols" to tell us which list to look up.
 * If not of this type then we always  search the world_list.  This is
 * implemented by the macro activesymtab(type) which returns a pointer to the
 * table to be searched for an entry of type 'type'.
 *
 ******************************************************************************
 */
#define activesymtab(type)\
	((type == SYM_VARIABLE || type == SYM_PARAMETER)? symbols : world)


/******************************************************************************
 *
 *	V A L U E _ S Y M B O L S (void)
 *
 * Deliver the current value of symbols as an ICL value.
 *
 * Used in defining a parameter_interpret() node in interp.c.
 *
 ******************************************************************************
 */
value
value_symbols(void)
{
    return (value_symtab(symbols));
}

/******************************************************************************
 *
 *	S A V E S Y M B O L T A B L E C O N T E X T (node *procnode)
 *
 * Implement a stack of the value of 'symbols'.
 *
 * The value of 'symbols' is changed just prior to interpreting a procedure's
 * body (in procedure interpret()) to the corresponding proc's symbol table.
 * This is found as the symboltable_part() member of the procedure_interpret()
 * node associated with the PROC's name in the world symbol table.
 *
 * Prior to  entering the PROC we must save the the current value of 'symbols'
 * so that, having interpreted the body of the procedure, we can reset
 * 'symbols' to its old value.
 *
 ******************************************************************************
 */
static symtab *symbolstack[ICL_BUFSIZE];
static int symbolstacklevel = 0;

value
savesymboltablecontext(node *procnode)
{
    if (symbolstacklevel == ICL_BUFSIZE)
	return (exception("STKOVFL ICLs proc stack is full"));
    else {
	symbolstack[symbolstacklevel++] = symbols;
	symbols = symboltable_part(procnode->val);
	return trueval;
    }
}

/******************************************************************************
 *
 *	R E S T O R E S Y M B O L T A B L E C O N T E X T (void)
 *
 ******************************************************************************
 */
value
restoresymboltablecontext(void)
{
    if (symbolstacklevel <= 0)
	return exception("STKUNFL ICLs proc stack has underflowed");
    else {
	symbols = symbolstack[--symbolstacklevel];
	return trueval;
    }
}

/******************************************************************************
 *
 * 		C O N S R U C T O R S
 *
 ******************************************************************************
 */


/******************************************************************************
 *
 *	N E W _ S Y M T A B (char *name, int type, node *n)
 *
 * Create a new symbol table entry.
 *
 * This will also create a dummy symtab header if name = nil, type = 0 and
 * n = nil
 *
 ******************************************************************************
 */
static symtab *
new_symtab(char *name, int type, node * n)
{
    char *w;
    symtab *sym = (symtab *) malloc(sizeof(symtab));

    if (sym == SYMBNIL)
	return (SYMBNIL);
    else if (name == CHARNIL)
	sym->name = CHARNIL;
    else if ((w = strcopy(name)) == CHARNIL)
	return (SYMBNIL);
    else
	sym->name = w;
    sym->type = type;
    sym->value = n;
    sym->next = sym->prev = sym;
    return sym;
}

/******************************************************************************
 *
 *	V A L U E _ E M P T Y S Y M T A B (void)
 *
 * Construct an empty symbol table and return as an ICL value.
 *
 ******************************************************************************
 */
value
value_emptysymtab(void)
{
    return (value_symtab(new_symtab(CHARNIL, 0, NODENULL)));
}

/******************************************************************************
 *
 * 		A C C E S S O R S   (LOOKUP)
 *
 ******************************************************************************
 */

/*****************************************************************************
 *
 *	S Y M B O L _ T O T A L (symtab *sym, int type)
 *
 * Returns the total number of symbols of type 'type' in the given symbol
 * table 'sym'
 *
 *****************************************************************************
 */
static int
symbol_total(symtab *sym, int type)
{
    symtab *p;

    int count = 0;
    for (p = sym->next; p != sym; p = p->next)
	if( p->type == type)
	    count++;
    return count;
}

/******************************************************************************
 *
 *	G E T _ S Y M B O L (symtab *sym, char *name, int type)
 *
 * Look up a symbol of the given type in the given symbol table.
 *
 * If a symbol is found, it is moved to the front of the list (if it is not
 * already there).
 * The found symbol's pointer to its nodevalue is returned, otherwise we return
 * NODENIL.
 *
 ******************************************************************************
 */
static node *
get_symbol(symtab * sym, char *name, int type)
{
    symtab *p;

    for (p = sym->next; p != sym; p = p->next)
	if (iclidentityequal(p->name, name, 15) && p->type == type)
	    if (p != sym->next) {	/* Reorder table to make 'p' first */
		p->next->prev = p->prev;
		p->prev->next = p->next;
		p->next = sym->next;
		p->prev = sym;
		sym->next->prev = p;
		sym->next = p;
		return (p->value);
	    } else
		return (p->value);
	else
	    continue;
    return NODENIL;
}

/******************************************************************************
 *
 *	L O O K U P _ S Y M B O L (char *name, int type)
 *
 * Lookup a symbol of the given type in the active symbol table.
 *
 * If we are searching for variables or parameters then we search the
 * "symbols" symbol table (either a proc's symbol table if we are executing a
 * proc, or the same as world if not).  It we are searching for anything else
 * then we will only find it in the global "world" table.
 *
 ******************************************************************************
 */
node *
lookup_symbol(char *name, int type)
{
    return get_symbol(activesymtab(type), name, type);
}

/******************************************************************************
 *
 *	G E T _ P A S S B Y R E F E R E N C E V A R (node *var)
 *
 * If a variable is a parameter then, on calling the current procedure we
 * could have passed EITHER an expression, which is evaluated and the formal's
 * value member points to that value (whose interpret member will not be
 * parameter_interpret())
 * OR
 * a variable (ie passed by reference). It is this latter case that
 * get_passbyreferencevar() handles.
 *
 * In this case the parameter's value member will point to a node whose type
 * is TYPE_SYMTAB, interpret member is parameter_interpret(),
 * symboltable_part() member points to the symbol table of the corresponding
 * actual variable and whose sub[0] member points to a node that is the
 * corresponding actual variable (ie a name_interpret() node whose value field
 * is an ICL string whose value is the identity of the passed variable).
 *
 * Note that if the actual was a parameter itself that had been passed a
 * varaiable then that parameter's actual variable parameter would have been
 * passed onto this (ie we do not have a chain formal-actuals to follow).
 *
 *
 * This routine will return something that can be assigned to or NULL.
 *
 ******************************************************************************
 */
static node *
get_passbyreferencevar(node * var)
{
    return (get_symbol(symboltable_part(var->val),
	    string_part(var->sub[0]->val),
	    SYM_VARIABLE));
}

/******************************************************************************
 *
 *	L O O K F O R _ V A R I A B L E _ V A L U E (symtab *sym, char * name)
 *
 * This routine looks up a variable in a given symbol_table.
 *
 * If the variable is a parameter then, on calling the current procedure we
 * could have passed EITHER an expression, which is evaluated and the formal
 * node's value member points to that value (whose interpret member will not
 * be parameter_interpret())
 * OR
 * a variable.  In this case the value member points to a node whose type is
 * TYPE_SYMTAB, whose interpret member is parameter_interpret(), whose
 * symboltable_part() member points to the symbol table of the corresponding
 * actual variable and whose sub[0] member points to a node that is the
 * corresponding actual variable (ie a name_interpret() node whose value field
 * is an ICL string whose value is the identity of the passed variable).
 *
 * If a value cannot be found, a suitable exception is returned.
 *
 * RETURNS AN ACTUAL VALUE STRUCTURE - IE A COPY OF ONE IN A
 * SYMBOL TABLE
 *
 ******************************************************************************
 */
value
lookfor_variable_value(symtab * sym, char *name)
{
    node *var, *var2;

    if ((var = get_symbol(sym, name, SYM_VARIABLE)) != NODENIL)
	return var->val;
    else if ((var = get_symbol(sym, name, SYM_PARAMETER)) == NODENIL)
	return exception1("UNDEFVAR variable \"%s\" not defined", name);
/*
 * do we have a parameter that was passed an expression ?
 */
    else if (var->interpret != parameter_interpret)
	return var->val;
    else if ((var2 = get_passbyreferencevar(var)) != NODENIL)
	return var2->val;
    else
	return exception2(
		"UNDEFVAR Parameter \"%s\" is undefined variable \"%s\"",
		 name,
		 string_part(var->sub[0]->val));
}

/******************************************************************************
 *
 *	L O O K U P _ V A R I A B L E _ V A L U E (char *name)
 *
 * Looks up a variable in the active symbol table using
 * lookfor_variable_value(symbols,..).
 *
 ******************************************************************************
 */
value
lookup_variable_value(char *name)
{
    return (lookfor_variable_value(symbols, name));
}

/******************************************************************************
 *
 *	L O O K U P _ P R O C V A R I A B L E _ V A L U E
 *		(node * procnode, char *name)
 *
 * Looks up a variable in an ICL procedure symbol table or, if 'procnode'
 * is NULL in the world table.
 *
 * On entry 'procnode' is either NODENULL or  will point to
 * a procedure_interpret() node the symboltable_part() of which points to the
 * symbol table of that procedure.
 *
 * Uses lookfor_variable_value() to search this table for name.
 *
 ******************************************************************************
 */
value
lookup_procvariable_value(node * procnode, char *name)
{
    if( procnode != NODENULL)
	return lookfor_variable_value(symboltable_part(procnode->val), name);
    else
	return lookfor_variable_value(world, name);
}

/******************************************************************************
 *
 *	L O O K U P _ P R O C (char *name)
 *
 * Looks up a command name in the global symbol table, searching in the
 * required order for procedures of different classes.
 *
 ******************************************************************************
 */
node *
lookup_proc(char *name)
{
    node *res;

    if ((res = get_symbol(world, name, SYM_PROC)) != NODENIL)
	return res;
    else
	return get_symbol(world, name, SYM_HIDDEN_PROC);
}

/******************************************************************************
 *
 *		A C C E S S O R S (Adding entries)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	P U T _ S Y M B O L (value symbval, char *name, int type, node *n)
 *
 * Stores a node under a given name, with a supplied symbol type and with a
 * value (n) in a symbol table (all given). A symboltable ICL value is passed
 * and symboltable_part() is used to get the actual symbol table involved.
 *
 * If the symbol already exists, its node value is re-used (overwritten),
 * so that existing pointers to the symbol table entry will point to the
 * new value. I don't believe anything relies on this feature currently.
 ******************************************************************************
 */
value
put_symbol(value symbval, char *name, int type, node * n)
{
    node *found;
    symtab *symb;

    symb = symboltable_part(symbval);
    if ((found = get_symbol(symb, name, type)) != NODENIL) {
	destroy(found);
	symb->next->value = n;		/* get_symbol has reordered table */
	return (n->val);
    } else {
	symtab *sym;

	sym = new_symtab(name, type, n);
	if (sym == SYMBNIL)
	    return (exception("SYSERR  memory exhausted in put_symbol"));
	else {
	    sym->next = symb->next;
	    sym->prev = symb;
	    symb->next->prev = sym;
	    symb->next = sym;
	    return (n->val);
	}
    }
}

/******************************************************************************
 *
 *	S T O R E _ S Y M B O L (char *name, int type, node *n)
 *
 * Stores a given node under the given name, with the given symbol type
 * in the active symbol table using put_symbol().
 *
 ******************************************************************************
 */
value
store_symbol(char *name, int type, node * n)
{
    if (n == NODENIL)
	return exception("SYSERR  memory exhausted in store_symbol()");
    else
	return (put_symbol(value_symtab(activesymtab(type)), name, type, n));
}

/******************************************************************************
 *
 *	I N S T A L L _ A B B R E V S (char *name, node *n)
 *
 * Install a name as a hidden procedure with all the abbreviations implied by
 * specifications of the form: COM(MAND)
 *
 * With DEFPROC or DEFSTRING one can specify the new command or procedure
 * using COM(MAND) meaning that COM, COMM, COMMA, COMMAN and COMMAND are all
 * suitable forms of the command.  This routine generates all the
 * abbreviations and adds each as a HIDDEN_PROC to the global
 * symbol table with the value "n".
 *
 * WILL FAIL GIVEN COMM() ie empty optional part!!
 *
 ******************************************************************************
 */
value
install_abbrevs(char *name, node * n)
{
    value val;
    node *nw;
    char *p;

    if ((name = strcopy(name)) == CHARNIL)
	return exception("SYSERR  memory exhausted in install_abbrevs()");
    if ((p = strchr(name, '(')) != CHARNIL) {
	do {
	    *p = '\0';
	    if ((nw = nodecopy(n)) == NODENIL)
		return exception(
			       "SYSERR  memory exhausted in install_abbrevs()");
	    if (isexc(val = store_symbol(name, SYM_DEFINED, nw)))
		return (val);
	    else {
		*p = p[1];
		p++;
	    }
	}
	while (*p && *p != ')');
	return trueval;
    } else
	return (store_symbol(name, SYM_DEFINED, n));
}

/******************************************************************************
 *
 *		A C C E S S O R S (modifying entries)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	PUT_VALUE (symtab *symb, char *name, int type, value val)
 *
 * Stores a given value under the given name, with the given symbol type in
 * the given symbol table.
 *
 * If the symbol already exists, the same node object is used,  so that
 * pointers to the symbol table entry will point at the new value. I don't
 * believe anything relies on this currently.
 * put_value() exists because it is common to change a value in a variable.
 * This avoids creating and freeing a node object on every assignment.
 * If a node value is changed, the interpreter function is changed so that,
 * if the node is interpreted, the correct thing happens.
 *
 * Returns the value assigned (or exception if failed to assign).
 *
 ******************************************************************************
 */
static value
put_value(symtab * symb, char *name, int type, value val)
{
    node *found;
    char buf[512];

    if ((found = get_symbol(symb, name, type)) != NODENIL) {
	found->val = val;
	switch (thetypeof(val)) {
	  case TYPE_INTEGER:
	    found->interpret = integer_interpret;
	    return (val);
	  case TYPE_REAL:
	    found->interpret = real_interpret;
	    return (val);
	  case TYPE_LOGICAL:
	    found->interpret = logical_interpret;
	    return (val);
	  case TYPE_STRING:
	    found->interpret = string_interpret;
	    return (val);
	  case TYPE_FUNCTION:
	    found->interpret = builtin_interpret;
	    return (val);
	  case TYPE_FILE:
	    found->interpret = 0;
	    return (val);
	  default:
	    sprintf(buf,
		"put_value: assigning an unknown value type %d\n",
		thetypeof(found->val));
	    systemfail(buf);
	    return (val); /* for lint */
	}
    } else {
	node *valuenode = node_value(val);
	symtab *sym = new_symtab(name, type, valuenode);

	if (valuenode == NODENIL || sym == SYMBNIL)
	    return (exception("SYSERR  memory exhausted in put_value()"));
	else {
	    sym->next = symb->next;
	    sym->prev = symb;
	    symb->next->prev = sym;
	    symb->next = sym;
	    return (val);
	}
    }
}

/******************************************************************************
 *
 *	A S S I G N _ H E L P E R (node *var, value val)
 *
 * Assign the value 'val' to the variable 'var'.
 * 'Var' is a name_interpret() node whose string_part member contains the
 * identity of the variable.
 *
 * If the variable is a parameter then, on calling the current procedure we
 * could have passed
 * EITHER
 * an expression, which is evaluated and the formal's value member points to
 * that value
 * OR
 * a variable.  In this latter  case the formal's node value will point to a
 * node whose type is TYPE_SYMTAB, whose interpret member is
 * parameter_interpret(), whose symboltable_part() member points to the symbol
 * table of the corresponding actual variable and whose sub[0] member points
 * to a node that is the corresponding actual variable (ie a name_interpret()
 * whose string_part() member points to the identifier).
 *
 * If the variable is not a parameter then we can use
 * put_value(activesymtab(SYM_VARIABLE), var's name, SYM_VARIABLE,val)....) to
 * achieve the storage otherwise we can use put_value(symboltable from formal
 * entry, actual parameter's name,SYM_VARIABLE,val).
 ******************************************************************************
 */
value
assign_helper(node *var, value val)
{
    node *param;

    if ((param = lookup_symbol(string_part(var->val), SYM_PARAMETER))
	    != NODENIL) {
	if (param->interpret == parameter_interpret)
	    return (put_value(symboltable_part(param->val),
		    string_part(param->sub[0]->val), SYM_VARIABLE, val));
	else if( param->interpret == undefined_interpret)
	    put_value(symbols, string_part(var->val), SYM_PARAMETER, val);
	else
	    return exception(
		"ASSNOTVAR Assignment to parameter not passed as a variable");
    } else
	return (put_value(activesymtab(SYM_VARIABLE), string_part(var->val),
		SYM_VARIABLE, val));
}

/*****************************************************************************
 * A S S I G N _ H E L P E R 1 ( node *var, value val )
 *
 * Similare to assign_helper but used by reload_vars_hds.
 * Therefore if the variable is a parameter given as a value, we assume it is
 * not required to be reset.
 ******************************************************************************
 */
value
assign_helper1(node *var, value val)
{
    node *param;

    if ((param = lookup_symbol(string_part(var->val), SYM_PARAMETER))
	    != NODENIL)
	if (param->interpret == parameter_interpret)
	    return (put_value(symboltable_part(param->val),
		    string_part(param->sub[0]->val), SYM_VARIABLE, val));
	else
	    return trueval;
    else
	return (put_value(activesymtab(SYM_VARIABLE), string_part(var->val),
		SYM_VARIABLE, val));
}

/******************************************************************************
 *
 *		D E S T R U C T O R S
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	D E L E T E _ S Y M B O L (char *name, int type)
 *
 * Deletes a symbol from the active symbol table, freeing any associated
 * storage. It uses get_symbol to find the symbol if present already and then
 * relies on the fact that get_symbol will have moved it to the front of the
 * list.
 *
 ******************************************************************************
 */
int
delete_symbol(char *name, int type)
{
    symtab *found, *symb;

    symb = activesymtab(type);
    if (get_symbol(symb, name, type) == NODENIL)
	return 0;
    else {
	found = symb->next;
	found->next->prev = found->prev;
	found->prev->next = found->next;
	destroy(found->value);
	free((void *) (found->name));
	free((void *) found);
	return TRUE;
    }
}

/******************************************************************************
 *
 *     D E L E T E _ S Y M _ P A R A M E T E R S (value symbval)
 *
 * This routine is called after a procedure has been executed to free all
 * storage used to hold procedure parameters (SYM_PARAMETER type variables).
 * Not that we have to be careful about passed-by-reference variables as, if
 * we free the associated memory, we will corrupt the variable in the calling
 * routine
 *
 * It uses get_symbol() to find the symbol if present already and then relies
 * on the fact that get_symbol will have moved it to the front of the list.
 *
 ******************************************************************************
 */
void
delete_sym_parameters(value symbval)
{
    symtab *found, *next, *symb, *cfound, *csymb, *caller_symtab;

    symb = symboltable_part(symbval);
    for(found = symb->next; found != symb; found = next) {
	next = found->next;
	if(found->type == SYM_PARAMETER) {
	    found->next->prev = found->prev;
	    found->prev->next = found->next;
/*
 * The symbol may be local to this procedure, in which case all storage can be
 * freed, or passed by reference from the level above in which case the value
 * part must be left alone.
 */
	    caller_symtab = symbolstack[symbolstacklevel-1];
	    cfound = SYMBNIL;
	    for(csymb = caller_symtab->next;
		csymb != caller_symtab && cfound == SYMBNIL;
		csymb = csymb->next)
		if(csymb->value == found->value)
		    cfound = csymb;
            if(cfound == SYMBNIL)
		destroy(found->value);
/* Finish deletion of found entry */
	    free((void *) (found->name));
	    free((void *) found);
	} /* if */
    } /* for */
    return;
}

/******************************************************************************
 *
 *     F O R A L L _ T Y P E D _ S Y M B O L S
 *		(int type, int (*fn)(char *name, node *valu, int type))
 *
 * This is an iterator. It scans the active symbol table for names of the
 * given type and applies the given function to their value nodes.
 *
 * The function has signature char* X node* -> int and expects a
 * typed_symbol's name and value node.
 * If the function returns zero, the scan is aborted.
 *
 ******************************************************************************
 */
static int
forall_typed_symbols(int type, int (*fn) (char *name, node *valu, int type))
{
    symtab *p, *sym;

    sym = activesymtab(type);
    for (p = sym->prev; p != sym; p = p->prev)
	if (p->type == type && !(*fn) (p->name, p->value, p->type))
	    return 0;
	else
	    continue;	/* applying function */
    return TRUE;
}

/******************************************************************************
 *
 *	P R I N T _ T Y P E D (char *name, node *p, int sym_type)
 *
 * Print a symbol table entry in a form suitable for the VARS and PROCS
 * built-ins.
 *
 ******************************************************************************
 */
static int
print_typed(char *name, node * p, int sym_type)
{
    char buf[512];

    switch (sym_type) {
      case SYM_VARIABLE:
	sprintf(buf, "%20s  %-9s	", name, (maptypetostring(p->val)));
	bufstring(buf);
	print(p);
	bufnewline();
	break;

      case SYM_PROC:
	bufstring(name);
	bufnewline();
	break;

      default:
	break;
    }
    return TRUE;
}

/******************************************************************************
 *
 *	P R I N T _ T Y P E D _ S Y M B O L S (int sym_type)
 *
 * A helper for the PROCS and VARS ICL command processing.
 *
 ******************************************************************************
 */
static int
print_typed_symbols(int sym_type)
{
    return (forall_typed_symbols(sym_type, print_typed));
}

/******************************************************************************
 *
 *	L I S T P R O C (char *name, node *val, int dummy)
 *
 * A routine to list a PROC's body on the terminal.
 *
 * The 'val' argument points to the procedure's body (extracted from the symtab
 * entry). As this routine also used to list all procs one by one it has a
 * dummy 'int' parameter so it can be used with forall_typed_symbols() in
 * symtab.c.
 *
 * print() (in interp.c) calls the interpret function of the node entry for
 * the procedure with the action OP_PRINT. This causes the procedure to list
 * itself.
 *
 ******************************************************************************
 */
static int
listproc(char *name, node *val, int dummy)
{
    outfpstring("PROC ");
    outfpstring(name);
    outfpstring(" ");
    print(val);
    outfpstring("\n");
    return TRUE;
}

/******************************************************************************
 *
 *	L I S T A P R O C (char *name)
 *
 * Routine used by the ICL command "LIST" and lists the procedure with the
 * name "name".
 *
 * Uses lookup_symbol() to find the proc entry in the global symbol table and,
 * if found, use listproc() to list it
 *
 ******************************************************************************
 */
value
listaproc(char *name)
{
    node *proc;

    if ((proc = lookup_symbol(name, SYM_PROC)) != NODENIL) {
	outfpstring("\n");
	listproc(name, proc, 0);
	outfpstring("\n");
	return trueval;
    } else
	return exception(
		"PROCERR  LIST: Unrecognised procedure or command name");
}

/******************************************************************************
 *
 *	L I S T A L L P R O C S (void)
 *
 * Lists the names of all the current procedures using
 * print_typed_symbols(SYM_PROC) (in symtab.c)
 *
 ******************************************************************************
 */
value
listallprocs(void)
{
    bufnewline();
    (void) print_typed_symbols(SYM_PROC);
    bufnewline();
    return trueval;
}

/******************************************************************************
 *
 *	L I S T P R O C S V A R S (char *name)
 *
 * Used in processing the ICL command "VARS".
 * Lists the variables of procedure name with their current types and values
 * OR, when name is CHARNIL, lists the direct mode variables
 *
 * A PROC entry is always found in the global world symbtab.
 * Its symboltable_part() member, if it has been executed at least once,
 * points to its local symbol table.
 *
 * Search the global symtab for a match (lookup_proc() using the string
 * argument name) and if we find it, we use the symboltable_part() entry
 * as the source of the variables.
 * If name is nil, we use the world symtab as the source.
 * We save the current active symbol table in symbols and restore
 * it once done and we use print_typed_symbols() to do the printing
 * (symtab.c).
 *
 ******************************************************************************
 */
value
listprocsvars(char *name)
{
    if (name != CHARNIL) {
	node *proc;

	if ((proc = lookup_proc(name)) != NODENIL) {
	    symtab *savesym = symbols;

	    symbols = symboltable_part(proc->val);
	    (void) print_typed_symbols(SYM_VARIABLE);
	    symbols = savesym;
	    return trueval;
	} else
	    return exception1(
		"PROCERR  VARS: Unrecognised procedure or command name \"%s\" ",
		name);
    } else {
	symtab *savesym = symbols;

	symbols = world;
	(void) print_typed_symbols(SYM_VARIABLE);
	symbols = savesym;
	return trueval;
    }
}

/******************************************************************************
 *
 *	F I L E A P R O C (char *procname,char *filename, char *comm)
 *
 * Saves the procedure 'procname' in the file 'filename' for the command 'comm'.
 *
 * If the procname parameter is CHARNIL then saves all the currently known
 * procedures into the file filename
 * OR
 * if procname is not CHARNIL the procedure is checked to exist (lookup_proc()).
 *
 * The file "filename" is opened for write using iclopenasoutfp()
 * If procname is CHARNIL then forall_typed_symbols() is called with
 * listproc() to print all the procs to the file otherwise listproc() is used
 * once to list the named proc to filename.
 * The file filename is then closed using iclcloseasoutfp().
 *
 ******************************************************************************
 */
value
fileaproc(char *procname, char *filename, char *comm)
{
    extern value iclopenasoutfp (char *whofor, char *filename);	/* output.c */
    extern value iclcloseasoutfp(char *whofor, char *filename);	/* output.c */
    node *proc = NULL;
    value val;

    if (procname != CHARNIL) {				/* Named procedure */
	if( (proc = lookup_proc(procname)) == NODENIL)
	    return exception2(
		"PROCERR  %s: Unrecognised procedure or command name %s",
		comm, procname);
    } else
	if( symbol_total(world, SYM_PROC) == 0)	{	/* No procedures */
	    unlink(filename);
	    return trueval;
	}

    if (isexc(val = iclopenasoutfp(comm, filename)))
	return val;
    if (procname == CHARNIL )				/* All procedures */
	(void) forall_typed_symbols(SYM_PROC, listproc);
    else
	(void) listproc(procname, proc, 0);
    return (iclcloseasoutfp(comm, filename));
}
