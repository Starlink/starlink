#ifndef _LANG
#define _LANG

#define STATIC_BUILD


#ifdef bool
#undef bool
#endif

#include "tkConfig.h"
#define TCL_NO_DEPRECATED

#if !defined(WIN32) && defined(USE_XFT_FONTS)
#ifndef TCL_UTF_MAX
#define TCL_UTF_MAX 13
#endif
#endif

#define USE_TCLALLOC 1
#define TCL_MEM_DEBUG
#define USE_COMPAT_CONST
#include "tcl.h"

#if !defined(__GNUC__) && !defined(_AIX)
#ifdef __STDC__
#ifndef STRINGIFY
#define STRINGIFY(x)        STRINGIFY1(x)
#define STRINGIFY1(x)       #x
#endif	/* STRINGIFY */
#define __FUNCTION__ __FILE__ ":" STRINGIFY(__LINE__)
#else 	/* STDC */
#define __FUNCTION__ ""
#endif	/* STDC */
#endif	/* GNUC or AIX */

EXTERN int	Tcl_DStringLength _ANSI_ARGS_((Tcl_DString *dString));
EXTERN char *	Tcl_DStringValue _ANSI_ARGS_((Tcl_DString *dString));

EXTERN void	Tcl_IncrRefCount _ANSI_ARGS_((Tcl_Obj *objPtr));
EXTERN void	Tcl_DecrRefCount _ANSI_ARGS_((Tcl_Obj *objPtr));
EXTERN int	Tcl_IsShared _ANSI_ARGS_((Tcl_Obj *objPtr));

EXTERN Tcl_ObjType *	TclObjGetType _ANSI_ARGS_((Tcl_Obj *objPtr));
EXTERN int		TclObjLength  _ANSI_ARGS_((Tcl_Obj *objPtr));
EXTERN void		TclObjSetType _ANSI_ARGS_((Tcl_Obj *objPtr,Tcl_ObjType *newType));
EXTERN Tcl_InternalRep *	TclObjInternal _ANSI_ARGS_((Tcl_Obj *objPtr));

#if defined(TCL_EVENT_IMPLEMENT) && !defined(Var)
#include "mTk/tclGeneric/tclInt.h"
#endif
#if !defined(TCL_EVENT_IMPLEMENT) || defined(Var)
#define _TCLINT
typedef void *TclHandle;
extern Tcl_ObjType	tclIntType;
#endif

#ifndef Var
#define Var Tcl_Obj *
#endif
#ifndef LangCallback
#define LangCallback Tcl_Obj
#endif
#define LangStringArg(x) Tcl_NewStringObj(x,-1)

#define XFree_arg_t void

EXTERN void LangSetString _ANSI_ARGS_((Tcl_Obj **,CONST char *));
EXTERN void LangSetDefault _ANSI_ARGS_((Tcl_Obj **,CONST char *));
EXTERN void LangSetInt _ANSI_ARGS_((Tcl_Obj **,int));
EXTERN void LangSetDouble _ANSI_ARGS_((Tcl_Obj **,double));
EXTERN void LangSetObj _ANSI_ARGS_((Tcl_Obj **,Tcl_Obj *));
EXTERN void LangSetVar _ANSI_ARGS_((Tcl_Obj **,Var));

EXTERN int  LangCmpArg  _ANSI_ARGS_((CONST Tcl_Obj *,CONST Tcl_Obj *));
EXTERN int  LangCmpOpt  _ANSI_ARGS_((CONST char *opt,CONST char *arg,size_t length));


/* FIXME:
   Tk will set freeProc as for Tcl e.g. NULL for statics & UIDs
   and to "free" for Tcl_Merge etc.
   Non Tk users *may* be able to use it as a guide,
   but it is more likely that they will have to use
   their own ref counts.
   Perhaps Tcl_Merge should set freeProc and/or Tcl's
   LangSetString() deliberately malloc() a copy of the string so we don't need
   the freeProc
*/
EXTERN void LangFreeArg _ANSI_ARGS_((Tcl_Obj *,Tcl_FreeProc *freeProc));
EXTERN Tcl_Obj *LangCopyArg _ANSI_ARGS_((Tcl_Obj *));

EXTERN int  LangNull _ANSI_ARGS_((Tcl_Obj *));

EXTERN void		TclpGetTime  _ANSI_ARGS_((Tcl_Time *time));
EXTERN void		TclpAsyncMark _ANSI_ARGS_((Tcl_AsyncHandler async));

EXTERN void Lang_SetErrorCode _ANSI_ARGS_((Tcl_Interp *interp,char *code));
EXTERN char *Lang_GetErrorCode _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN char *Lang_GetErrorInfo _ANSI_ARGS_((Tcl_Interp *interp));

/* Old-config handler for variables */
EXTERN int LangSaveVar _ANSI_ARGS_((Tcl_Interp *,Tcl_Obj *,Var *,int type));
EXTERN void LangFreeVar _ANSI_ARGS_((Var));

/* New-config handler for objects, variables and callbacks */
EXTERN int LangConfigObj _ANSI_ARGS_((Tcl_Interp *interp, Tcl_Obj **save,
				      Tcl_Obj *obj, int type));

EXTERN int LangEventHook _ANSI_ARGS_((int flags));
EXTERN void Lang_BuildInImages _ANSI_ARGS_((void));
EXTERN void *	TclCalloc _ANSI_ARGS_((size_t n,size_t s));
EXTERN void LangDebug _ANSI_ARGS_((CONST char *fmt,...));
EXTERN void LangDumpVec _ANSI_ARGS_((CONST char *tag, int argc, Tcl_Obj **vec));

EXTERN void Lang_DeleteObject _ANSI_ARGS_((Tcl_Interp *,Tcl_Command));
EXTERN Tcl_Command	Lang_CreateObject _ANSI_ARGS_((Tcl_Interp *interp,
			    char *cmdName, Tcl_ObjCmdProc *proc,
			    ClientData clientData,
			    Tcl_CmdDeleteProc *deleteProc));

EXTERN int Lang_CallWithArgs _ANSI_ARGS_ ((Tcl_Interp *interp,
					char *sub, int argc, Tcl_Obj *CONST *argv));

EXTERN void		Tcl_IntResults _ANSI_ARGS_((Tcl_Interp *interp,int,int,...));
EXTERN void		Tcl_DoubleResults _ANSI_ARGS_((Tcl_Interp *interp,int,int,...));
EXTERN void		Tcl_SprintfResult _ANSI_ARGS_((Tcl_Interp *,char *,...));



EXTERN int LangDoCallback _ANSI_ARGS_((Tcl_Interp *,LangCallback *,int result,int argc,...));
EXTERN int LangMethodCall _ANSI_ARGS_((Tcl_Interp *,Tcl_Obj *,char *,int result,int argc,...));

EXTERN char *LangLibraryDir _ANSI_ARGS_((void));
EXTERN void Lang_SetBinaryResult _ANSI_ARGS_((Tcl_Interp *interp,
			    char *string, int len, Tcl_FreeProc *freeProc));
EXTERN Tcl_ObjCmdProc *LangOptionCommand;

typedef char *(Lang_VarTraceProc) _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tcl_Obj *part1, CONST char *part2, int flags));

EXTERN Tcl_Encoding	Lang_CreateEncoding _ANSI_ARGS_((
    CONST char *encodingName,
    Tcl_EncodingConvertProc *toUtfProc,
    Tcl_EncodingConvertProc *fromUtfProc,
    Tcl_EncodingFreeProc *freeProc,
    ClientData clientData,
    int nullSize));

EXTERN int		Tcl_AfterObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));


EXTERN int		Lang_TraceVar _ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj *varRef, int flags,
				Lang_VarTraceProc * proc,
				ClientData clientData));

EXTERN void		Lang_UntraceVar _ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * varRef, int flags,
				Lang_VarTraceProc * proc,
				ClientData clientData));

EXTERN int		Tk_PropertyCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj **objv));

struct TkFontAttributes;

EXTERN unsigned int     LangFontRank _ANSI_ARGS_((unsigned int suggested,
			    int ch, CONST char *gotName,
			    CONST char *wantFoundary,
			    CONST struct TkFontAttributes *wantAttrib,
			    CONST char *wantEncoding,
			    CONST char *gotFoundary,
			    CONST struct TkFontAttributes *gotAttrib,
			    CONST char *gotEncoding));

EXTERN void		Lang_FreeRegExp _ANSI_ARGS_((Tcl_RegExp re));

EXTERN long Lang_OSHandle _ANSI_ARGS_((int fd));
#define TK_LIBRARY LangLibraryDir()

#ifdef WIN32
#ifdef __BORLANDC__
#pragma warn -par	/* "parameter 'foo' is never used" */
#pragma warn -aus	/* "'foo' is assigned a value that is never used" */
#pragma warn -use	/* "'foo' is declared but never used" */
#endif

#ifdef _MSC_VER
#pragma warning(disable:4101 4102 4244 4018)
#pragma warning(disable:4133) /* init incompatible for xlib */
#endif
#endif

#ifndef RC_INVOKED
#include "tkEvent.h"
#if !defined(TCL_EVENT_IMPLEMENT)
#include "tkEvent.m"
#endif
#endif

#endif /* _LANG */




