/******************************************************************************
 *
 * 		E X P R . H
 *
 * External interface to expr.c - NB routines in this module are invoked
 * from the intrepreter.
 *
 *		Created :	S.K. Robinson 19/12/91
 *		Tidied Up:	S.K.Robinson  7/5/92
 *		Edited:		I.R.Jenkins   2/6/92
 *				Protoyped
 *
 *
 ******************************************************************************
 */
extern value undefined_interpret	  ( node *n, int op );
extern value integer_interpret            ( node *n, int op );
extern value logical_interpret            ( node *n, int op );
extern value real_interpret               ( node *n, int op );
extern value string_interpret             ( node *n, int op );
extern value quoted_string_interpret      ( node *n, int op );
extern value openstring_interpret         ( node *n, int op );
extern value name_interpret		  ( node *n, int op );
extern value optarg_name_interpret	  ( node *n, int op );
extern value optarg_name_interpret_end    ( node *n, int op );
extern value name_interpret_nohds	  ( node *n, int op );
extern value paren_interpret              ( node *n, int op );
extern value concat_interpret             ( node *n, int op );
extern value and_interpret      	  ( node *n, int op );
extern value or_interpret		  ( node *n, int op );
extern value not_interpret		  ( node *n, int op );
extern value unaryarith_interpret         ( node *n, int op );
extern value binaryoperator_interpret     ( node *n, int op );
extern value reloperator_interpret        ( node *n, int op );
extern value format_interpret             ( node *n, int op );
extern value function_call_interpret      ( node *n, int op );
extern value nonary_func_interpret        ( node *n, int op );
extern value func_interpret               ( node *n, int op );
extern value unary_func_interpret         ( node *n, int op );
extern value binary_func_interpret        ( node *n, int op );
extern value unary_integer_func_interpret ( node *n, int op );
extern value binary_integer_func_interpret( node *n, int op );
extern value unary_numeric_func_interpret ( node *n, int op );
extern value binary_numeric_func_interpret( node *n, int op );
extern value unary_string_func_interpret  ( node *n, int op );
extern value binary_string_func_interpret ( node *n, int op );
extern value unary_string_func_interpret  ( node *n, int op );
extern value binary_string_func_interpret ( node *n, int op );
extern value except_interpret             ( node *n, int op );
