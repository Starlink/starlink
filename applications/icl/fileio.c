/*****************************************************************************
 *
 *		F I L E I O . C
 *
 * Procedures associated with ICL files.
 *
 * The convention adopted here is that any name beginning proc_ is an entry
 * point for an ICL built in procedure.
 *
 *	History
 *	Created :	S.K.Robinson	19/11/91
 *	Edited  :	S.K.Robinson, I.R. Jenkins  1/5/92
 *			(1) Corrected bug in proc_append() which was
 *			    using "w+" mode to append to a file rather
 *			    than "a"
 *			(2) Correct bug in proc_write() that was
 *			    printing newline to std_out not the file
 *			(3) Correct bug that create will not complain
 *			    if file already exists (by changing
 *			    create_helper())
 *	Edited  :	S.K. Robinson, I. R. Jenkins 5/5/92
 *			(1) To correct behaviour in read_values()
 *			    when EOF found during READx command
 *	Edited  :	I.R.Jenkins	2/6/92
 *			Prototyped
 *	Tidied, reformatted by indent and improved function comments:
 *			B.K.McIlwrath	22/7/93 + 16/11/93
 *
 *****************************************************************************
 */
#include <stdio.h>
#include "icl.h"
#include "carith.h"
#include "expr.h"
#include "input.h"
#include "interp.h"
#include "symtab.h"
#include "output.h"

/******************************************************************************
 *
 *	P R O C _ P R I N T (node *n)
 *
 * Handles the ICL command PRINT p1 p2 where the parameters are concatenated
 * and printed on the terminal.
 *
 * On execution n is set pointing to an abbut_interpret() linked list whose
 * sub[0] member points to a paren_interpret() node (actual argument is of
 * the form (x)) or a string/open_string_interpret() node whose sub[1] member
 * points to the next abbut_interpret() node.  A feature of interpreting an
 * abbut_interpret() list like this is that the values of the elements in the
 * list, seen as strings, are concatenated as one long string (see
 * abbut_interpret() in interp.c )
 *
 * value_print() forms the concrete syntax of the string resulting from
 * interpreting the argument_list  (value.c)
 *
 ******************************************************************************
 */
static value
proc_print(node * n)
{
    value val;

    if (n != NODENIL) {
	if (isexc(val = interpret(n)))
	    return val;
	value_print(val);
    }
    bufnewline();
    flshbuf();
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ W R I T E (node *n)
 *
 * Handles the ICL command WRITE intname p2 p3 where parameters of write are
 * concatenated and written to the text file of name intname.  This file must
 * have been opened or created with an APPEND or CREATE command (and therefore
 * will exist in the symbol_table of type SYM_FILE whose file_part() value is
 * the associated C file descriptor number)
 *
 * Works in a similar manner to PRINT (above) except that during execution we
 * save the default screen output channel using stackandsetoutfp() (output.c)
 * and the default real precision (precision) in savep.
 *
 * System routines:
 * ferror(FILE *fp) tests the error indicator for the stream
 *                  pointed to by fp and returns nonzero
 *                  if the error_indicator is set
 ******************************************************************************
 */
static value
proc_write(node * n)
{
    extern int stackandsetoutfp(value vfp);		/* output.c */
    extern int restoreoutfp(void);			/* output.c */
    extern int outfpstring(char *mess);			/* output.c */
    extern int precision;				/* procs.c  */
/*
 * Local variables
 */
    value val;
    node *file;
    char *filename;
    int savep = precision;

    if (nargs == 0)
	return exception("TOOFEWPARS  WRITE: internal name required");
    else if(nargs == 1)
	val = interpret_to_string(n);
    else
	val = interpret_to_string(n->sub[0]);
    if (isexc(val))
	return val;
    if (!(file = lookup_symbol(filename = string_part(val), SYM_FILE)))
	return exception1("WRITEERR  WRITE - file %s not open", filename);
    if (stackandsetoutfp(file->val) == 0)
	return exception("WRITERR WRITE - Too many nested files open");
    precision = 15;
    if(nargs > 1)
	val = interpret(n->sub[1]);
    else
	val = value_string("");
    if (!isexc(val))
	if( value_print(val)  < 0 ||
	    outfpstring("\n") < 0 )	/* Add newline to record written */
		return exception("WRITERR - error in writing to file");
    precision = savep;
    if (restoreoutfp() == 0) {
	systemfail("Did not expect restoreoutfp() to fail");
	return trueval;	 /* for lint */
    }
    if (isexc(val))
	return val;
    if (ferror((FILE *) file_part(file->val)))
	return exception1("WRITEERR  during WRITE on file %s", filename);
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ R E A D (node *n)
 *
 * Handles the ICL command READ intname (string)
 *
 * This reads a line from a text file, internal name 'intname', which must
 * have been previously opened with the OPEN command into the variable
 * (string).
 *
 * On execution n is set pointing to an abbut_interpret() linked list whose
 * sub[0] member points to the node corresponding to intname and whose sub[1]
 * member points to the node corresponding to (string).  This node must be a
 * paren_interpret() node whose sub[0] member is a name_interpret() node whose
 * string_part() member is the identity "string".
 * As get_args() has run when we executed builtin_interpret() the (string)
 * node is also in arglist[1].
 *
 * Functions called:
 * Assign_helper(variable, value) (symtab.c) updates the symbol table entry for
 *     variable with the value 'value'.
 *
 * icl_fgets(char*s,int n,value vfp) (input.c) reads at most n-1 characters
 *     from the stream whose (file type) value is vfp into the array s. No
 *     additional characters are read after newline (retained) or eof.
 *     The string will be null terminated.
 *     s is returned or null on error.
 *
 ******************************************************************************
 */
static value
proc_read(node * n)
{
    value val;
    node *file, *arg0, *arg1;
    char *filename;
    char buf[ICL_BUFSIZE];
    value assign_helper();

    if (isexc(nargs_should_be(2)))
	return exception("TOOFEWPARS  READ: file and variable required");
    arg0 = arglist[0];
    arg1 = arglist[1];
    if (isexc(val = interpret_to_string(arg0)))
	return val;
    if (!(file = lookup_symbol(filename = string_part(val), SYM_FILE)))
	return exception1("READERR  READ - file %s not open", filename);
    if (arg1->interpret != paren_interpret)
	return exception(
	    "ASSNOTVAR  READ Assignment to parameter which is not a variable");
    while (arg1->interpret == paren_interpret)
	arg1 = arg1->sub[0];
    if (arg1->interpret != name_interpret)
	return exception(
	    "ASSNOTVAR  READ Assignment to parameter which is not a variable");
    if (isexc(val = icl_fgets(filename, buf, ICL_BUFSIZE, file->val)))
	return val;
    buf[strlen(buf) - 1] = '\0';
    return (assign_helper(arg1, value_string(buf)));
}

/******************************************************************************
 *
 *		READING ROUTINES
 *
 * READI intname (i) (j) (k) ...
 * READL intname (l1) (l2) (l3) ...
 * READR intname (r1) (r2) (r3) ....
 *
 * Read values (I Int, L Logical, R real) from the given file and store them
 * in args[1..nargs-1]
 *
 * the internal name of which is intname which must have been opened with
 * the OPEN command. (xx) are the variables into which the next value will
 * be read.
 *
 * Any arguments which cannot be read successfully become UNDEFINED
 * as we delete their symbol table entry.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	R E A D _ V A L U E S (char *command, int type, char *filename)
 *
 * All the READx command use this general purpose input routine where command
 * is READI, READL, READR, filename is intname and type is TYPE_INTEGER,
 * TYPE_LOGICAL or TYPE_REAL.
 *
 * get_args() will have been called from builtin_interpret() and will have
 * set nargs to the number of actual arguments and will also have placed the
 * the pointers to (xx1) (xx2) ... in arglist[1], arglist[2] etc. Each should
 * be a paren_interpret() node whose sub[0] member points to a name_interpret()
 * node whose string_part() is the identity of the variable to be read into.
 *
 * Routines called:
 * ferror(FILE *fp) tests the error indicator for the stream pointed to by fp
 *     and returns nonzero if the error_indicator is set.
 *
 * Assign_helper(variable, value) (symtab.c) updates the symbol table entry for
 *     variable with the value 'value'.
 *
 * feof(FILE *fp) tests the end-of-file indicator or fp and returns non_zero
 *    if it is set.
 *
 ******************************************************************************
 */
static value
read_values(char *command, int type, char *filename)
{
    int i;
    value val;
    node *arg, *file;
    static char inputbuffer[4096];
    char *inpos;

    if ((file = lookup_symbol(filename, SYM_FILE)) == NODENIL)
	return exception2("READERR  %s - file %s not open", command, filename);
    if (isexc(val = icl_fgets(filename, inputbuffer, 4094, file->val)))
	return val;
    inpos = inputbuffer;
    for (i = 1; i < nargs; i++) {
	arg = arglist[i];
	if (arg->interpret != paren_interpret)
	    return exception1(
	      "ASSNOTVAR  %s: Assignment to parameter which is not a variable",
	      command);
	while (arg->interpret == paren_interpret)
	    arg = arg->sub[0];
	if (arg->interpret != name_interpret)
	    return exception1(
	      "ASSNOTVAR  %s: Assignment to parameter which is not a variable",
	      command);
	switch (type) {
	  case TYPE_LOGICAL:
	    val = stringtoicllogvalue(&inpos);
	    break;
	  case TYPE_INTEGER:
	    val = stringtoiclintvalue(&inpos);
	    break;
	  case TYPE_REAL:
	    val = stringtoiclrealvalue(&inpos);
	    break;
	  default:
	    val = noval;
	    break;
	}
	if (isexc(val)) {
	    (void) clearupafterread(file->val, command, filename);
	    return val;
	} else if (isunknowntype(val)) {
	    (void) delete_symbol(string_part(arg->val), SYM_VARIABLE);
	    (void) clearupafterread(file->val, command, filename);
	    return exception2(
		"READERR  %s: Input not of the correct format for %s",
		 command, string_part(arg->val));
	} else if (isexc(val = assign_helper(arg, val)))
	    return val;
    }
    return clearupafterread(file->val, command, filename);
}

/******************************************************************************
 *
 *	R E A D _ H E L P E R (char *command, int type)
 *
 * All THE READx commands use this general purpose input routine where command
 * is READI,READL,READR, and type is TYPE_INTEGER, TYPE_LOGICAL or TYPE_REAL.
 *
 * get_args() will have been called in builtin_interpret() and this will have
 * set nargs to the number of actual arguments. get_args() will also have
 * placed the node corresponding to intname in arglist[0] and the pointers to
 * (xx1) (xx2) ... in arglist[1], arglist[2] etc.

 * This routine checks the minimum number of args, obtains the internal name
 * of the file to be read by interpereting arglist[0], and checks the file is
 * opened by searching for a TYPE_FILE entry in the activesymboltable of this
 * name.
 * If found it selects the file_part() member of the entry for intname as
 * this contains the C filedescriptor for the opened stream.  It then calls
 * read_values passing the filedescriptor, the command concerned, the
 * type of input (TYPE_INTEGER, TYPE_LOGICAL etc) and the intname in filename.
 *
 ******************************************************************************
 */
static value
read_helper(char *command, int type)
{
    value val;

    if (nargs == 0)
	return exception1("TOOFEWPARS  %s: file required", command);
    if (isexc(val = interpret_to_string(arglist[0])))
	return exception1("READERR  %s: Invalid internal filename", command);
    else
	return read_values(command, type, string_part(val));
}

/******************************************************************************
 *
 *	P R O C _ R E A D R (node *n)
 *
 * Reads a series of REAL values from a text file
 *
 ******************************************************************************
 */
static value
proc_readr(node * n)
{
    return read_helper("READR", TYPE_REAL);
}

/******************************************************************************
 *
 *	P R O C _ R E A D I (node *n)
 *
 * Reads a series of INTEGER values from a text file
 *
 ******************************************************************************
 */
static value
proc_readi(node * n)
{
    return read_helper("READI", TYPE_INTEGER);
}

/******************************************************************************
 *
 *	P R O C _ R E A D L (node *n)
 *
 * Reads a series of LOGICAL values from a text file
 *
 ******************************************************************************
 */
static value
proc_readl(node * n)
{
    return read_helper("READL", TYPE_LOGICAL);
}

/******************************************************************************
 *
 *		INPUT ROUTINES
 *
 * INPUTI PROMPT (i) (j) (k) ...
 * INPUTL PROMPT (l1) (l2) (l3) ...
 * INPUTR PROMPT (r1) (r2) (r3) ....
 *
 * Input a series of values (I Int, L Logical, R real) from the terminal
 * with the PROMPT "PROMPT" into ICL variables.
 * Any arguments which cannot be read successfully become UNDEFINED
 * as we delete their symbol table entry.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	I N P U T _ V A L U E S (char *command, int type, int start)
 *
 * All the INPUTx functions use this general purpose input routine where
 * command is one of INPUTI, INPUTL, INPUTR, filename is intname and type is
 * TYPE_INTEGER, TYPE_LOGICAL or TYPE_REAL
 *
 * As the prompt is optional by the time this routine is called start will b
 * set to 0 (no prompt) or 1 (prompt present)
 *
 * get_args() will have been called in builtin_interpret() and this will have
 * set nargs to the number of actual arguments. get_args() will also have
 * placed the pointers to (xx1) (xx2) ... in arglist[1], arglist[2] etc. Each
 * will be a paren_interpret() node whose sub[0] member points to a
 * name_interpret() node whose string_part() is the identity of the variable
 * to be read into.
 *
 * Functions called:
 *
 * ferror(FILE *fp) tests the error indicator for the stream pointed to by fp
 *    and returns nonzero if the error_indicator is set.
 *
 * Assign_helper(variable, value) (symtab.c) updates the symbol table entry for
 *     variable with the value 'value'.
 *
 * feof(FILE *fp) tests the end-of-file indicator or fp and returns non_zero
 *    if it is set.
 *
 ******************************************************************************
 */
static value
input_values(char *command, int type, int start, char *prompt)
{
    int i;
    value val;
    node *arg;
    static char inputbuffer[4096];
    char *inpos;

    if (isexc(val = icl_gets(0, command, inputbuffer, 4094, prompt, CHARNIL)))
	return val;
    inpos = inputbuffer;
    for (i = start; i < nargs; i++) {
	arg = arglist[i];
	if (arg->interpret != paren_interpret)
	    return exception1(
	      "ASSNOTVAR  %s: Assignment to parameter which is not a variable",
	      command);
	while (arg->interpret == paren_interpret)
	    arg = arg->sub[0];
	if (arg->interpret != name_interpret)
	    return exception1(
	      "ASSNOTVAR  %s: Assignment to parameter which is not a variable",
	      command);
	switch (type) {
	  case TYPE_LOGICAL:
	    val = stringtoicllogvalue(&inpos);
	    break;
	  case TYPE_INTEGER:
	    val = stringtoiclintvalue(&inpos);
	    break;
	  case TYPE_REAL:
	    val = stringtoiclrealvalue(&inpos);
	    break;
	  default:
	    val = noval;
	    break;
	}
	if (isexc(val)) {
	    (void) clearupafterinput(command);
	    return val;
	} else if (isunknowntype(val)) {
	    (void) delete_symbol(string_part(arg->val), SYM_VARIABLE);
	    (void) clearupafterinput(command);
	    return exception2(
		"READERR  %s: Input not of the correct format for %s",
		command, string_part(arg->val));
	} else if (isexc(val = assign_helper(arg, val)))
	    return val;
    }
    return clearupafterinput(command);
}

/******************************************************************************
 *
 *	P R O C _ I N P U T R (node *n)
 *
 * INPUTR PROMPT (r1) (r2) (r3) ....
 * Reads a series of REAL values from the terminal into (r1), (r2)........
 * with the prompt PROMPT
 *
 * get_args() will have been called in builtin_interpret() and this will have
 * set nargs to the number of actual arguments. get_args() will also have
 * placed the node corresponding to PROMPT in arglist[0] and the pointers to
 * (r1), (r22) ... in arglist[1], arglist[2] etc.
 *
 * By interpreting the PROMPT argument and printing it on stderr, we can use
 * read_values () with stdin to do the reading.
 *
 ******************************************************************************
 */
static value
proc_inputr(node * n)
{
    value val;
    char *prompt;

    prompt = CHARNIL;
    if (nargs < 1)
	return exception(
	    "TOOFEWPARS  INPUTR: At least one input variable required");
    if (arglist[0]->interpret != paren_interpret) {
	if (isexc(val = interpret_to_string(arglist[0])))
	    return exception("READERR INPUTR : Invalid prompt string");
	if (nargs < 2)
	    return exception(
		"TOOFEWPARS  INPUTR: At least one input variable required");
	prompt = strconcat(string_part(val), " ");
	return input_values("INPUTR", TYPE_REAL, 1, prompt);
    } else
	return input_values("INPUTR", TYPE_REAL, 0, prompt);
}

/******************************************************************************
 *
 *	P R O C _ I N P U T I (node *n)
 *
 * INPUTI PROMPT (i1) (i2) (i3) ....
 * Reads a series of INTEGER values from the terminal into (i1), (i2)........
 * with the prompt PROMPT
 *
 * Works as INPUTR (above)
 *
 ******************************************************************************
 */
static value
proc_inputi(node * n)
{
    value val;
    char *prompt;

    prompt = CHARNIL;
    if (nargs < 1)
	return exception(
	    "TOOFEWPARS  INPUTI: At least one input variable required");
    if (arglist[0]->interpret != paren_interpret) {
	if (isexc(val = interpret_to_string(arglist[0])))
	    return exception("READERR INPUTI : Invalid prompt string");
	if (nargs < 2)
	    return exception(
		"TOOFEWPARS  INPUTI: At least one input variable required");
	prompt = strconcat(string_part(val), " ");
	return input_values("INPUTI", TYPE_INTEGER, 1, prompt);
    } else
	return input_values("INPUTI", TYPE_INTEGER, 0, prompt);
}

/******************************************************************************
 *
 *	P R O C _ I N P U T L (node *n)
 *
 * INPUTL PROMPT (l1) (l2) (l3) ....
 * Reads a series of LOGICAL values from the terminal into (l1), (l2)........
 * with the prompt PROMPT
 *
 * Works as INPUTR (above)
 *
 ******************************************************************************
 */
static value
proc_inputl(node * n)
{
    value val;
    char *prompt;

    prompt = CHARNIL;
    if (nargs < 1)
	return exception(
	    "TOOFEWPARS  INPUTL: At least one input variable required");
    if (arglist[0]->interpret != paren_interpret) {
	if (isexc(val = interpret_to_string(arglist[0])))
	    return exception("READERR INPUTL : Invalid prompt string");
	if (nargs < 2)
	    return exception(
		   "TOOFEWPARS  INPUTL: At least one input variable required");
	prompt = strconcat(string_part(val), " ");
	return input_values("INPUTL", TYPE_LOGICAL, 1, prompt);
    } else
	return input_values("INPUTL", TYPE_LOGICAL, 0, prompt);
}

/******************************************************************************
 *
 *	PROC_INPUT (node *n)
 *
 * Hnadles the ICL command INPUT [PROMPT] (string) which reads a line from
 * the terminal with the PROMPT "PROMPT" if present. (string) is the variable
 * into which the line will be read.
 *
 * On execution n is set pointing  either to the node corresponding to (string)
 * (PROMPT missing) or to an abbut_interpret() linked list whose sub[0] member
 * points to the node corresponding to [PROMPT] and whose sub[1] member
 * points to the node corresponding to the (string).  The (string) node must
 * be a paren_interpret() node whose sub[0] member is a name_interpret() node
 * with a string_part() member having the identity "string".
 * As get_args() has run when we executed builtin_interpret() the (string)
 * node is also in arglist[1].
 *
 * Functions used:
 * Assign_helper(variable, value) (symtab.c) updates the symbol table entry for
 *     variable with the value 'value'.
 *
 * icl_gets( "INPUT",char*s,int n) reads at most n-1 characters from stdin
 *     into the array s. No additional chars are read after newline
 *     (retained) or eof. The string will be null terminated.
 *     trueval is returned unless error when an exception is generated.
 *
 ******************************************************************************
 */
static value
proc_input(node * n)
{
    extern int uface_interrupt(void);				/* uface.c */
    value val;
    node *vararg;
    char buf[ICL_BUFSIZE];
    char *prompt;
    int i;


    if (n == NODENIL)
	return exception("TOOFEWPARS  INPUT: variable required");
    if (nargs == 1)
	prompt = ": ";
    else {
	prompt = CHARNIL;
	for (i = 0; i < nargs - 1; i++) {
	    if (isexc(val = interpret_to_string(arglist[i])))
		return val;
	    prompt = strconcat(prompt, string_part(val));
	    if (i < nargs - 2)
		prompt = strconcat(prompt, " ");
	}
    }
    vararg = arglist[nargs - 1];
    if (vararg->interpret != paren_interpret)
	return exception(
	   "ASSNOTVAR  INPUT Assignment to parameter which is not a variable");
    while (vararg->interpret == paren_interpret)
	vararg = vararg->sub[0];
    if (vararg->interpret != name_interpret)
	return exception(
	   "ASSNOTVAR  INPUT Assignment to parameter which is not a variable");
    if (isexc(val = icl_gets(0, "INPUT", buf, ICL_BUFSIZE, prompt, CHARNIL)))
	return val;
    if (uface_interrupt())
	return exception("CTRLC  In INPUT");
    buf[strlen(buf) - 1] = '\0';
    return (assign_helper(vararg, value_string(buf)));
}

/******************************************************************************
 *
 *		FILE OPERATIONS
 *
 * CREATE intname [filename] - create a text file and open it for output
 *
 * OPEN intname [filename] - opens an existing file for input
 *
 * APPEND intname [filename] - opens an existing file for output, append
 *
 * In all cases the file name is filename or intname.dat.  intname is the
 * internal name by which the stream is known.
 *
 * An entry of type SYM_FILE will be present in the symbol table for as long
 * as the file is open. This entry records the intname in the name member of
 * the symbol_table entry and the value member points to a node whose
 * file_part() records the C file descriptor (FILE *)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	C R E A T E _ H E L P E R (char *command, char *mode)
 *
 * This is a helper function for CREATE, OPEN and APPEND.
 * The only thing different about the various modes of open is the mode
 * argument given to iclopenfile()
 *
 * Prior to the functions running, get_args() will have been run in
 * builtin_interpret() which will have set nargs to the number of actual
 * arguments, arglist[0] to point to the node corresponding to intname and,
 * if present, arglist[1] to filename.  By interpreting both these we should
 * obtain the strings required.
 *
 * The routine below is called in all cases but by this time we have
 * translated the command to an open mode in mode, "w" for create, 'r" for
 * open, "a" for append"
 *
 * Functions used:
 * store_symbol(char *name, int type, node *value) sets up a new entry in the
 *    active symboltable with the name "name", type "type", and with the
 *    value "value" which in this case will be SYM_FILE and the C filepointer
 *     from iclopenfile() (symtab.c)
 *
 ******************************************************************************
 */
static value
create_helper(char *command, char *mode)
{
    extern node *node_value (value v);		/* node.c   */
    char *name, *filename;
    value val;
    node *arg0, *arg1;

    if (nargs == 0)
	return exception1(
	 "TOOFEWPARS  %s: file handle and option file name required", command);
    arg0 = arglist[0];
    arg1 = arglist[1];
    if (isexc(val = interpret_to_string(arg0)))
	return val;
    name = string_part(val);
    if (lookup_symbol(name, SYM_FILE))
	return exception2("OPENERR  %s: file %s already open", command, name);
    if (nargs == 1) {
	if ((filename = strconcat(name, ".dat")) == CHARNIL)
	    return exception("SYSERR  memory exhausted in create_helper()");
    } else {
	if (isexc(val = interpret_to_string(arg1)))
	    return val;
	filename = string_part(val);
    }
    if (isexc(val = iclopenfile(command, filename, mode)))
	return val;
    else
	return (store_symbol(name, SYM_FILE, node_value(val)));
}

/******************************************************************************
 *
 *	P R O C _ C R E A T E (node *n)
 *
 * CREATE intname [filename] - create a text file and open it for output
 *
 * Prior to this function, get_args() will have been called from
 * builtin_interpret(). This will have set nargs to the number of actual
 * arguments, arglist[0] to point to the node corresponding to intname and,
 * if present, arglist[1] to filename.  By interpreting both these we should
 * obtain the strings required.
 *
 ******************************************************************************
 */
static value
proc_create(node * n)
{
    return create_helper("CREATE", "w");
}

/******************************************************************************
 *
 *	P R O C _ A P P E N D (node *n)
 *
 * APPEND intname [filename] - opens an existing file for output, append
 *
 ******************************************************************************
 */
static value
proc_append(node * n)
{
    return create_helper("APPEND", "a");
}

/******************************************************************************
 *
 *	P R O C _ O P E N (node *n)
 *
 * OPEN intname [filename] - opens an existing file for input
 *
 ******************************************************************************
 */
static value
proc_open(node * n)
{
    return create_helper("OPEN", "r");
}

/******************************************************************************
 *
 *	P R O C _ C L O S E (node *n)
 *
 * CLOSE intname - ICL file close command.
 *
 * This routine closes the text file previously opened with CREATE, OPEN or
 * APPEND. If the file open,create,append was successfull then an entry wil
 * exist in the current activesymtable with the type SYM_FILE with name
 * intname and file_part() set to the C filedescriptor (FILE*)
 *
 * Prior to the function running, get_args() will have been called from
 * builtin_interpret() andwill have set nargs to the number of actual
 * arguments and  arglist[0] to point to the node corresponding to intname.
 * By interpreting this we should get the string intname and by looking it up
 * in the symbol table we can find if the stream is open. If it is, fclose()
 * it and delete the SYM_FILE entry.
 *  n will have been set to point to the intname node by the time of call
 *
 * Functions used:
 * fclose(FILE *fp) flushes the stream (or discards any unread buffered data)
 *    associated with fp and causes associated file to be closed.  The stream
 *    is then disassociated from the file.
 *    returns zero if OK, or EOF if it failed.
 *
 * delete_symbol(char*name, int type) (symtab.c) removes the entry for name
 * of type 'type' from the activesymboltable.
 *
 ******************************************************************************
 */
static value
proc_close(node * n)
{
    value val;
    node *file;
    char *filename;

    if (n == NODENIL)
	return exception("TOOFEWPARS  CLOSE: file required");
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    if (!(file = lookup_symbol(filename = string_part(val), SYM_FILE)))
	return exception1("CLOSEERR  CLOSE - file %s not open", filename);
    if (fclose((FILE *) file_part(file->val)))
	return exception1("CLOSERR  during CLOSE on file %s", filename);
    (void) delete_symbol(filename, SYM_FILE);
    return trueval;
}

/******************************************************************************
 *
 *	I N I T _ F I L E I O (void)
 *
 * Set up all implemented file I/O commands in the world symbol_table and the
 * three default streams stdin, stdout, and stderr.
 *
 ******************************************************************************
 */
value
init_fileio(void)
{
    extern node *node_builtin (value (*fn)() );			/* node.c   */
    value val;

    if ((isexc(val = store_symbol("CREATE", SYM_BUILTIN,
					node_builtin(proc_create))))	||
	(isexc(val = store_symbol("OPEN", SYM_BUILTIN,
					node_builtin(proc_open))))	||
	(isexc(val = store_symbol("APPEND", SYM_BUILTIN,
					node_builtin(proc_append))))	||
	(isexc(val = store_symbol("CLOSE", SYM_BUILTIN,
					node_builtin(proc_close))))	||
	(isexc(val = store_symbol("WRITE", SYM_BUILTIN,
					node_builtin(proc_write))))	||
	(isexc(val = store_symbol("READ", SYM_BUILTIN,
					node_builtin(proc_read))))	||
	(isexc(val = store_symbol("READI", SYM_BUILTIN,
					node_builtin(proc_readi))))	||
	(isexc(val = store_symbol("READL", SYM_BUILTIN,
					node_builtin(proc_readl))))	||
	(isexc(val = store_symbol("READR", SYM_BUILTIN,
					node_builtin(proc_readr))))	||
	(isexc(val = store_symbol("PRINT", SYM_BUILTIN,
					node_builtin(proc_print))))	||
	(isexc(val = store_symbol("INPUT", SYM_BUILTIN,
					node_builtin(proc_input))))	||
	(isexc(val = store_symbol("INPUTI", SYM_BUILTIN,
					node_builtin(proc_inputi))))	||
	(isexc(val = store_symbol("INPUTL", SYM_BUILTIN,
					node_builtin(proc_inputl))))	||
	(isexc(val = store_symbol("INPUTR", SYM_BUILTIN,
					node_builtin(proc_inputr)))))
	return (val);
    else
	return (trueval);
}
