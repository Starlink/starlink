/*****************************************************************************
 *
 *		C O N T R O L . H
 *
 * Interface to control.c
 *
 *		Created :	S.K.Robinson	19/12/92
 *		Edited :	I.R.Jenkins	02/06/92
 *				prototyped
 *
 *
 *****************************************************************************
 */
extern int indent_count;

extern void indent(void);
extern value display_interpret ( node *n, int op );
extern value assign_interpret ( node *n, int op );
extern value if_interpret ( node *n, int op );
extern value else_interpret ( node *n, int op );
extern value loop_interpret ( node *n, int op );
extern value while_interpret ( node *n, int op );
extern value step_interpret ( node *n, int op );
extern value for_interpret ( node *n, int op );
extern value break_interpret ( node *n, int op );
extern value proc_call_interpret ( node *n, int op );
extern value builtin_call_interpret ( node *n, int op );
extern value comment_interpret ( node *n, int op );
extern value line_list_interpret ( node *n, int op );
