/*
 * tixItcl.h --
 *
 *	Compatibility functions and macros that allow Tix to work
 *	under Incr Tcl.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

#include <tix.h>

#ifdef ITCL_2

#ifndef _TCLINT
#include <tclInt.h>
#endif

/*
 * Structure to store ITcl name space information.
 */
typedef struct _TixItclNameSp {
    Interp *iPtr;
    CallFrame *savedVarFramePtr;
    Itcl_ActiveNamespace nsToken;
} TixItclNameSp;

#define DECLARE_ITCL_NAMESP(x,i) \
    TixItclNameSp x; \
    x.iPtr = (Interp*)(i); \
    x.nsToken = NULL;

EXTERN int		TixItclSetGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
EXTERN void		TixItclRestoreGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));

#else

#ifdef TK_8_0_OR_LATER

typedef struct _TixItclNameSp {
    Tcl_CallFrame frame;
} TixItclNameSp;

#define DECLARE_ITCL_NAMESP(x,i) \
    TixItclNameSp x;

EXTERN int		TixItclSetGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
EXTERN void		TixItclRestoreGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));



#else

#define DECLARE_ITCL_NAMESP(x,i)
#define TixItclSetGlobalNameSp(a,b)     (1)
#define TixItclRestoreGlobalNameSp(a,b)

#endif /* TK_8_0_OR_LATER */
#endif /* ITCL_2 */
