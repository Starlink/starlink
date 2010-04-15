/******************************************************************************
 *
 * 		C A R I T H . H
 *
 * This file contains the interface for the ICL module carith.c.
 * This  contains the implementation of ICL arithmetic in C (ie
 * value X value => value)
 *
 *		Created : 	28/11/91
 *		Edited :	30/4/92	To include this header
 *				I.R.Jenkins	02/06/92
 *				Prototyping
 *		Tidied :	B.K. McIlwrath	15/11/93
 *
 ******************************************************************************
 */
extern void  init_arith ( void );
extern value do_operator ( value left, int op, value right );
extern value do_reloperator ( value val1, int op, value val2 );
extern value call_unary_integer_func ( value val, int (*fn)() );
extern value call_binary_integer_func ( value val1, value val2, int (*fn)() );
extern value stringtointvalue ( char *thestring );
extern value basedstringtointvalue ( char *thestring );
extern value func_abs ( value arg );
extern value func_nint ( value arg );
extern value func_int ( value arg );
extern value func_ifix ( value arg );
extern value func_asin ( value arg );
extern value func_acos ( value arg );
extern value func_asind ( value arg );
extern value func_acosd ( value arg );
extern value func_atan2 ( value y, value x );
extern value func_atan2d ( value y, value x );
extern value func_log ( value arg );
extern value func_log10 ( value arg );
extern value func_sqrt ( value arg );
extern value func_real ( value arg );
extern value func_sin ( value arg );
extern value func_cos ( value arg );
extern value func_tan ( value arg );
extern value func_atan ( value arg );
extern value func_exp ( value arg );
extern value func_sinh ( value arg );
extern value func_cosh ( value arg );
extern value func_tanh ( value arg );
extern value func_sind ( value arg );
extern value func_cosd ( value arg );
extern value func_tand ( value arg );
extern value func_atand ( value arg );
extern value func_mod ( value left, value right );
extern value func_dim ( value left, value right );
extern value func_sign ( value left, value right );
extern value func_float ( value arg );
extern value stringtoiclintvalue( char **thestringptr);
extern value stringtoicllogvalue( char **thestringptr);
extern value stringtoiclrealvalue( char **thestringptr);
