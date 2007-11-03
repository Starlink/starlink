#ifdef _LANG
VVAR(Tcl_ObjCmdProc *,LangOptionCommand,V_LangOptionCommand)
#if !defined(TCL_EVENT_IMPLEMENT) || defined(Var)
VVAR(Tcl_ObjType,tclIntType,V_tclIntType)
#endif /* #if !defined(TCL_EVENT_IMPLEMENT) || defined(Var) */
#ifndef LangCmpArg
VFUNC(int,LangCmpArg,V_LangCmpArg,_ANSI_ARGS_((CONST Tcl_Obj *,CONST Tcl_Obj *)))
#endif /* #ifndef LangCmpArg */

#ifndef LangCmpOpt
VFUNC(int,LangCmpOpt,V_LangCmpOpt,_ANSI_ARGS_((CONST char *opt,CONST char *arg,size_t length)))
#endif /* #ifndef LangCmpOpt */

#ifndef LangConfigObj
VFUNC(int,LangConfigObj,V_LangConfigObj,_ANSI_ARGS_((Tcl_Interp *interp, Tcl_Obj **save,
				      Tcl_Obj *obj, int type)))
#endif /* #ifndef LangConfigObj */

#ifndef LangCopyArg
VFUNC(Tcl_Obj *,LangCopyArg,V_LangCopyArg,_ANSI_ARGS_((Tcl_Obj *)))
#endif /* #ifndef LangCopyArg */

#ifndef LangDoCallback
VFUNC(int,LangDoCallback,V_LangDoCallback,_ANSI_ARGS_((Tcl_Interp *,LangCallback *,int result,int argc,...)))
#endif /* #ifndef LangDoCallback */

#ifndef LangDumpVec
VFUNC(void,LangDumpVec,V_LangDumpVec,_ANSI_ARGS_((CONST char *tag, int argc, Tcl_Obj **vec)))
#endif /* #ifndef LangDumpVec */

#ifndef LangEventHook
VFUNC(int,LangEventHook,V_LangEventHook,_ANSI_ARGS_((int flags)))
#endif /* #ifndef LangEventHook */

#ifndef LangFontRank
VFUNC(unsigned int,LangFontRank,V_LangFontRank,_ANSI_ARGS_((unsigned int suggested,
			    int ch, CONST char *gotName,
			    CONST char *wantFoundary,
			    CONST struct TkFontAttributes *wantAttrib,
			    CONST char *wantEncoding,
			    CONST char *gotFoundary,
			    CONST struct TkFontAttributes *gotAttrib,
			    CONST char *gotEncoding)))
#endif /* #ifndef LangFontRank */

#ifndef LangFreeArg
VFUNC(void,LangFreeArg,V_LangFreeArg,_ANSI_ARGS_((Tcl_Obj *,Tcl_FreeProc *freeProc)))
#endif /* #ifndef LangFreeArg */

#ifndef LangFreeVar
VFUNC(void,LangFreeVar,V_LangFreeVar,_ANSI_ARGS_((Var)))
#endif /* #ifndef LangFreeVar */

#ifndef LangLibraryDir
VFUNC(char *,LangLibraryDir,V_LangLibraryDir,_ANSI_ARGS_((void)))
#endif /* #ifndef LangLibraryDir */

#ifndef LangMethodCall
VFUNC(int,LangMethodCall,V_LangMethodCall,_ANSI_ARGS_((Tcl_Interp *,Tcl_Obj *,char *,int result,int argc,...)))
#endif /* #ifndef LangMethodCall */

#ifndef LangNull
VFUNC(int,LangNull,V_LangNull,_ANSI_ARGS_((Tcl_Obj *)))
#endif /* #ifndef LangNull */

#ifndef LangSaveVar
VFUNC(int,LangSaveVar,V_LangSaveVar,_ANSI_ARGS_((Tcl_Interp *,Tcl_Obj *,Var *,int type)))
#endif /* #ifndef LangSaveVar */

#ifndef LangSetDefault
VFUNC(void,LangSetDefault,V_LangSetDefault,_ANSI_ARGS_((Tcl_Obj **,CONST char *)))
#endif /* #ifndef LangSetDefault */

#ifndef LangSetDouble
VFUNC(void,LangSetDouble,V_LangSetDouble,_ANSI_ARGS_((Tcl_Obj **,double)))
#endif /* #ifndef LangSetDouble */

#ifndef LangSetInt
VFUNC(void,LangSetInt,V_LangSetInt,_ANSI_ARGS_((Tcl_Obj **,int)))
#endif /* #ifndef LangSetInt */

#ifndef LangSetObj
VFUNC(void,LangSetObj,V_LangSetObj,_ANSI_ARGS_((Tcl_Obj **,Tcl_Obj *)))
#endif /* #ifndef LangSetObj */

#ifndef LangSetString
VFUNC(void,LangSetString,V_LangSetString,_ANSI_ARGS_((Tcl_Obj **,CONST char *)))
#endif /* #ifndef LangSetString */

#ifndef LangSetVar
VFUNC(void,LangSetVar,V_LangSetVar,_ANSI_ARGS_((Tcl_Obj **,Var)))
#endif /* #ifndef LangSetVar */

#ifndef Lang_BuildInImages
VFUNC(void,Lang_BuildInImages,V_Lang_BuildInImages,_ANSI_ARGS_((void)))
#endif /* #ifndef Lang_BuildInImages */

#ifndef Lang_CallWithArgs
VFUNC(int,Lang_CallWithArgs,V_Lang_CallWithArgs,_ANSI_ARGS_((Tcl_Interp *interp,
					char *sub, int argc, Tcl_Obj *CONST *argv)))
#endif /* #ifndef Lang_CallWithArgs */

#ifndef Lang_CreateEncoding
VFUNC(Tcl_Encoding,Lang_CreateEncoding,V_Lang_CreateEncoding,_ANSI_ARGS_((
    CONST char *encodingName,
    Tcl_EncodingConvertProc *toUtfProc,
    Tcl_EncodingConvertProc *fromUtfProc,
    Tcl_EncodingFreeProc *freeProc,
    ClientData clientData,
    int nullSize)))
#endif /* #ifndef Lang_CreateEncoding */

#ifndef Lang_CreateObject
VFUNC(Tcl_Command,Lang_CreateObject,V_Lang_CreateObject,_ANSI_ARGS_((Tcl_Interp *interp,
			    char *cmdName, Tcl_ObjCmdProc *proc,
			    ClientData clientData,
			    Tcl_CmdDeleteProc *deleteProc)))
#endif /* #ifndef Lang_CreateObject */

#ifndef Lang_DeleteObject
VFUNC(void,Lang_DeleteObject,V_Lang_DeleteObject,_ANSI_ARGS_((Tcl_Interp *,Tcl_Command)))
#endif /* #ifndef Lang_DeleteObject */

#ifndef Lang_FreeRegExp
VFUNC(void,Lang_FreeRegExp,V_Lang_FreeRegExp,_ANSI_ARGS_((Tcl_RegExp re)))
#endif /* #ifndef Lang_FreeRegExp */

#ifndef Lang_GetErrorCode
VFUNC(char *,Lang_GetErrorCode,V_Lang_GetErrorCode,_ANSI_ARGS_((Tcl_Interp *interp)))
#endif /* #ifndef Lang_GetErrorCode */

#ifndef Lang_GetErrorInfo
VFUNC(char *,Lang_GetErrorInfo,V_Lang_GetErrorInfo,_ANSI_ARGS_((Tcl_Interp *interp)))
#endif /* #ifndef Lang_GetErrorInfo */

#ifndef Lang_SetBinaryResult
VFUNC(void,Lang_SetBinaryResult,V_Lang_SetBinaryResult,_ANSI_ARGS_((Tcl_Interp *interp,
			    char *string, int len, Tcl_FreeProc *freeProc)))
#endif /* #ifndef Lang_SetBinaryResult */

#ifndef Lang_SetErrorCode
VFUNC(void,Lang_SetErrorCode,V_Lang_SetErrorCode,_ANSI_ARGS_((Tcl_Interp *interp,char *code)))
#endif /* #ifndef Lang_SetErrorCode */

#ifndef Lang_TraceVar
VFUNC(int,Lang_TraceVar,V_Lang_TraceVar,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj *varRef, int flags,
				Lang_VarTraceProc * proc,
				ClientData clientData)))
#endif /* #ifndef Lang_TraceVar */

#ifndef Lang_UntraceVar
VFUNC(void,Lang_UntraceVar,V_Lang_UntraceVar,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * varRef, int flags,
				Lang_VarTraceProc * proc,
				ClientData clientData)))
#endif /* #ifndef Lang_UntraceVar */

#ifndef TclObjGetType
VFUNC(Tcl_ObjType *,TclObjGetType,V_TclObjGetType,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef TclObjGetType */

#ifndef TclObjInternal
VFUNC(Tcl_InternalRep *,TclObjInternal,V_TclObjInternal,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef TclObjInternal */

#ifndef TclObjLength
VFUNC(int,TclObjLength,V_TclObjLength,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef TclObjLength */

#ifndef TclObjSetType
VFUNC(void,TclObjSetType,V_TclObjSetType,_ANSI_ARGS_((Tcl_Obj *objPtr,Tcl_ObjType *newType)))
#endif /* #ifndef TclObjSetType */

#ifndef Tcl_AfterObjCmd
VFUNC(int,Tcl_AfterObjCmd,V_Tcl_AfterObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tcl_AfterObjCmd */

#ifndef Tcl_DStringLength
VFUNC(int,Tcl_DStringLength,V_Tcl_DStringLength,_ANSI_ARGS_((Tcl_DString *dString)))
#endif /* #ifndef Tcl_DStringLength */

#ifndef Tcl_DStringValue
VFUNC(char *,Tcl_DStringValue,V_Tcl_DStringValue,_ANSI_ARGS_((Tcl_DString *dString)))
#endif /* #ifndef Tcl_DStringValue */

#ifndef Tcl_DecrRefCount
VFUNC(void,Tcl_DecrRefCount,V_Tcl_DecrRefCount,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef Tcl_DecrRefCount */

#ifndef Tcl_DoubleResults
VFUNC(void,Tcl_DoubleResults,V_Tcl_DoubleResults,_ANSI_ARGS_((Tcl_Interp *interp,int,int,...)))
#endif /* #ifndef Tcl_DoubleResults */

#ifndef Tcl_IncrRefCount
VFUNC(void,Tcl_IncrRefCount,V_Tcl_IncrRefCount,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef Tcl_IncrRefCount */

#ifndef Tcl_IntResults
VFUNC(void,Tcl_IntResults,V_Tcl_IntResults,_ANSI_ARGS_((Tcl_Interp *interp,int,int,...)))
#endif /* #ifndef Tcl_IntResults */

#ifndef Tcl_IsShared
VFUNC(int,Tcl_IsShared,V_Tcl_IsShared,_ANSI_ARGS_((Tcl_Obj *objPtr)))
#endif /* #ifndef Tcl_IsShared */

#ifndef Tcl_SprintfResult
VFUNC(void,Tcl_SprintfResult,V_Tcl_SprintfResult,_ANSI_ARGS_((Tcl_Interp *,char *,...)))
#endif /* #ifndef Tcl_SprintfResult */

#ifndef Tk_PropertyCmd
VFUNC(int,Tk_PropertyCmd,V_Tk_PropertyCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj **objv)))
#endif /* #ifndef Tk_PropertyCmd */

#endif /* _LANG */
