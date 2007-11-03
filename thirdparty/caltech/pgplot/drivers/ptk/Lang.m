#ifndef _LANG_VM
#define _LANG_VM
#include "Lang_f.h"
#ifndef NO_VTABLES
#define LangOptionCommand (*LangVptr->V_LangOptionCommand)
#define tclIntType (*LangVptr->V_tclIntType)
#ifndef LangCmpArg
#  define LangCmpArg (*LangVptr->V_LangCmpArg)
#endif

#ifndef LangCmpOpt
#  define LangCmpOpt (*LangVptr->V_LangCmpOpt)
#endif

#ifndef LangConfigObj
#  define LangConfigObj (*LangVptr->V_LangConfigObj)
#endif

#ifndef LangCopyArg
#  define LangCopyArg (*LangVptr->V_LangCopyArg)
#endif

#ifndef LangDoCallback
#  define LangDoCallback (*LangVptr->V_LangDoCallback)
#endif

#ifndef LangDumpVec
#  define LangDumpVec (*LangVptr->V_LangDumpVec)
#endif

#ifndef LangEventHook
#  define LangEventHook (*LangVptr->V_LangEventHook)
#endif

#ifndef LangFontRank
#  define LangFontRank (*LangVptr->V_LangFontRank)
#endif

#ifndef LangFreeArg
#  define LangFreeArg (*LangVptr->V_LangFreeArg)
#endif

#ifndef LangFreeVar
#  define LangFreeVar (*LangVptr->V_LangFreeVar)
#endif

#ifndef LangLibraryDir
#  define LangLibraryDir (*LangVptr->V_LangLibraryDir)
#endif

#ifndef LangMethodCall
#  define LangMethodCall (*LangVptr->V_LangMethodCall)
#endif

#ifndef LangNull
#  define LangNull (*LangVptr->V_LangNull)
#endif

#ifndef LangSaveVar
#  define LangSaveVar (*LangVptr->V_LangSaveVar)
#endif

#ifndef LangSetDefault
#  define LangSetDefault (*LangVptr->V_LangSetDefault)
#endif

#ifndef LangSetDouble
#  define LangSetDouble (*LangVptr->V_LangSetDouble)
#endif

#ifndef LangSetInt
#  define LangSetInt (*LangVptr->V_LangSetInt)
#endif

#ifndef LangSetObj
#  define LangSetObj (*LangVptr->V_LangSetObj)
#endif

#ifndef LangSetString
#  define LangSetString (*LangVptr->V_LangSetString)
#endif

#ifndef LangSetVar
#  define LangSetVar (*LangVptr->V_LangSetVar)
#endif

#ifndef Lang_BuildInImages
#  define Lang_BuildInImages (*LangVptr->V_Lang_BuildInImages)
#endif

#ifndef Lang_CallWithArgs
#  define Lang_CallWithArgs (*LangVptr->V_Lang_CallWithArgs)
#endif

#ifndef Lang_CreateEncoding
#  define Lang_CreateEncoding (*LangVptr->V_Lang_CreateEncoding)
#endif

#ifndef Lang_CreateObject
#  define Lang_CreateObject (*LangVptr->V_Lang_CreateObject)
#endif

#ifndef Lang_DeleteObject
#  define Lang_DeleteObject (*LangVptr->V_Lang_DeleteObject)
#endif

#ifndef Lang_FreeRegExp
#  define Lang_FreeRegExp (*LangVptr->V_Lang_FreeRegExp)
#endif

#ifndef Lang_GetErrorCode
#  define Lang_GetErrorCode (*LangVptr->V_Lang_GetErrorCode)
#endif

#ifndef Lang_GetErrorInfo
#  define Lang_GetErrorInfo (*LangVptr->V_Lang_GetErrorInfo)
#endif

#ifndef Lang_SetBinaryResult
#  define Lang_SetBinaryResult (*LangVptr->V_Lang_SetBinaryResult)
#endif

#ifndef Lang_SetErrorCode
#  define Lang_SetErrorCode (*LangVptr->V_Lang_SetErrorCode)
#endif

#ifndef Lang_TraceVar
#  define Lang_TraceVar (*LangVptr->V_Lang_TraceVar)
#endif

#ifndef Lang_UntraceVar
#  define Lang_UntraceVar (*LangVptr->V_Lang_UntraceVar)
#endif

#ifndef TclObjGetType
#  define TclObjGetType (*LangVptr->V_TclObjGetType)
#endif

#ifndef TclObjInternal
#  define TclObjInternal (*LangVptr->V_TclObjInternal)
#endif

#ifndef TclObjLength
#  define TclObjLength (*LangVptr->V_TclObjLength)
#endif

#ifndef TclObjSetType
#  define TclObjSetType (*LangVptr->V_TclObjSetType)
#endif

#ifndef Tcl_AfterObjCmd
#  define Tcl_AfterObjCmd (*LangVptr->V_Tcl_AfterObjCmd)
#endif

#ifndef Tcl_DStringLength
#  define Tcl_DStringLength (*LangVptr->V_Tcl_DStringLength)
#endif

#ifndef Tcl_DStringValue
#  define Tcl_DStringValue (*LangVptr->V_Tcl_DStringValue)
#endif

#ifndef Tcl_DecrRefCount
#  define Tcl_DecrRefCount (*LangVptr->V_Tcl_DecrRefCount)
#endif

#ifndef Tcl_DoubleResults
#  define Tcl_DoubleResults (*LangVptr->V_Tcl_DoubleResults)
#endif

#ifndef Tcl_IncrRefCount
#  define Tcl_IncrRefCount (*LangVptr->V_Tcl_IncrRefCount)
#endif

#ifndef Tcl_IntResults
#  define Tcl_IntResults (*LangVptr->V_Tcl_IntResults)
#endif

#ifndef Tcl_IsShared
#  define Tcl_IsShared (*LangVptr->V_Tcl_IsShared)
#endif

#ifndef Tcl_SprintfResult
#  define Tcl_SprintfResult (*LangVptr->V_Tcl_SprintfResult)
#endif

#ifndef Tk_PropertyCmd
#  define Tk_PropertyCmd (*LangVptr->V_Tk_PropertyCmd)
#endif

#endif /* NO_VTABLES */
#endif /* _LANG_VM */
