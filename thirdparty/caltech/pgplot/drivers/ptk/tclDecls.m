#ifndef _TCLDECLS_VM
#define _TCLDECLS_VM
#include "tclDecls_f.h"
#ifndef NO_VTABLES
#ifndef Tcl_AddErrorInfo
#  define Tcl_AddErrorInfo (*TcldeclsVptr->V_Tcl_AddErrorInfo)
#endif

#ifndef Tcl_AllowExceptions
#  define Tcl_AllowExceptions (*TcldeclsVptr->V_Tcl_AllowExceptions)
#endif

#ifndef Tcl_AppendElement
#  define Tcl_AppendElement (*TcldeclsVptr->V_Tcl_AppendElement)
#endif

#ifndef Tcl_AppendObjToObj
#  define Tcl_AppendObjToObj (*TcldeclsVptr->V_Tcl_AppendObjToObj)
#endif

#ifndef Tcl_AppendResult
#  define Tcl_AppendResult (*TcldeclsVptr->V_Tcl_AppendResult)
#endif

#ifndef Tcl_AppendStringsToObj
#  define Tcl_AppendStringsToObj (*TcldeclsVptr->V_Tcl_AppendStringsToObj)
#endif

#ifndef Tcl_AppendToObj
#  define Tcl_AppendToObj (*TcldeclsVptr->V_Tcl_AppendToObj)
#endif

#ifndef Tcl_BackgroundError
#  define Tcl_BackgroundError (*TcldeclsVptr->V_Tcl_BackgroundError)
#endif

#ifndef Tcl_CallWhenDeleted
#  define Tcl_CallWhenDeleted (*TcldeclsVptr->V_Tcl_CallWhenDeleted)
#endif

#ifndef Tcl_CancelIdleCall
#  define Tcl_CancelIdleCall (*TcldeclsVptr->V_Tcl_CancelIdleCall)
#endif

#ifndef Tcl_Close
#  define Tcl_Close (*TcldeclsVptr->V_Tcl_Close)
#endif

#ifndef Tcl_ConcatObj
#  define Tcl_ConcatObj (*TcldeclsVptr->V_Tcl_ConcatObj)
#endif

#ifndef Tcl_CreateEventSource
#  define Tcl_CreateEventSource (*TcldeclsVptr->V_Tcl_CreateEventSource)
#endif

#ifndef Tcl_CreateExitHandler
#  define Tcl_CreateExitHandler (*TcldeclsVptr->V_Tcl_CreateExitHandler)
#endif

#ifndef Tcl_CreateFileHandler
#  define Tcl_CreateFileHandler (*TcldeclsVptr->V_Tcl_CreateFileHandler)
#endif

#ifndef Tcl_CreateInterp
#  define Tcl_CreateInterp (*TcldeclsVptr->V_Tcl_CreateInterp)
#endif

#ifndef Tcl_CreateObjCommand
#  define Tcl_CreateObjCommand (*TcldeclsVptr->V_Tcl_CreateObjCommand)
#endif

#ifndef Tcl_CreateTimerHandler
#  define Tcl_CreateTimerHandler (*TcldeclsVptr->V_Tcl_CreateTimerHandler)
#endif

#ifndef Tcl_DStringAppend
#  define Tcl_DStringAppend (*TcldeclsVptr->V_Tcl_DStringAppend)
#endif

#ifndef Tcl_DStringAppendElement
#  define Tcl_DStringAppendElement (*TcldeclsVptr->V_Tcl_DStringAppendElement)
#endif

#ifndef Tcl_DStringFree
#  define Tcl_DStringFree (*TcldeclsVptr->V_Tcl_DStringFree)
#endif

#ifndef Tcl_DStringInit
#  define Tcl_DStringInit (*TcldeclsVptr->V_Tcl_DStringInit)
#endif

#ifndef Tcl_DStringResult
#  define Tcl_DStringResult (*TcldeclsVptr->V_Tcl_DStringResult)
#endif

#ifndef Tcl_DStringSetLength
#  define Tcl_DStringSetLength (*TcldeclsVptr->V_Tcl_DStringSetLength)
#endif

#ifndef Tcl_DeleteEventSource
#  define Tcl_DeleteEventSource (*TcldeclsVptr->V_Tcl_DeleteEventSource)
#endif

#ifndef Tcl_DeleteFileHandler
#  define Tcl_DeleteFileHandler (*TcldeclsVptr->V_Tcl_DeleteFileHandler)
#endif

#ifndef Tcl_DeleteHashEntry
#  define Tcl_DeleteHashEntry (*TcldeclsVptr->V_Tcl_DeleteHashEntry)
#endif

#ifndef Tcl_DeleteHashTable
#  define Tcl_DeleteHashTable (*TcldeclsVptr->V_Tcl_DeleteHashTable)
#endif

#ifndef Tcl_DeleteInterp
#  define Tcl_DeleteInterp (*TcldeclsVptr->V_Tcl_DeleteInterp)
#endif

#ifndef Tcl_DeleteTimerHandler
#  define Tcl_DeleteTimerHandler (*TcldeclsVptr->V_Tcl_DeleteTimerHandler)
#endif

#ifndef Tcl_DoOneEvent
#  define Tcl_DoOneEvent (*TcldeclsVptr->V_Tcl_DoOneEvent)
#endif

#ifndef Tcl_DoWhenIdle
#  define Tcl_DoWhenIdle (*TcldeclsVptr->V_Tcl_DoWhenIdle)
#endif

#ifndef Tcl_DuplicateObj
#  define Tcl_DuplicateObj (*TcldeclsVptr->V_Tcl_DuplicateObj)
#endif

#ifndef Tcl_Eof
#  define Tcl_Eof (*TcldeclsVptr->V_Tcl_Eof)
#endif

#ifndef Tcl_EvalObjEx
#  define Tcl_EvalObjEx (*TcldeclsVptr->V_Tcl_EvalObjEx)
#endif

#ifndef Tcl_EvalObjv
#  define Tcl_EvalObjv (*TcldeclsVptr->V_Tcl_EvalObjv)
#endif

#ifndef Tcl_EventuallyFree
#  define Tcl_EventuallyFree (*TcldeclsVptr->V_Tcl_EventuallyFree)
#endif

#ifndef Tcl_ExternalToUtf
#  define Tcl_ExternalToUtf (*TcldeclsVptr->V_Tcl_ExternalToUtf)
#endif

#ifndef Tcl_ExternalToUtfDString
#  define Tcl_ExternalToUtfDString (*TcldeclsVptr->V_Tcl_ExternalToUtfDString)
#endif

#ifndef Tcl_FirstHashEntry
#  define Tcl_FirstHashEntry (*TcldeclsVptr->V_Tcl_FirstHashEntry)
#endif

#ifndef Tcl_FreeEncoding
#  define Tcl_FreeEncoding (*TcldeclsVptr->V_Tcl_FreeEncoding)
#endif

#ifndef Tcl_GetAssocData
#  define Tcl_GetAssocData (*TcldeclsVptr->V_Tcl_GetAssocData)
#endif

#ifndef Tcl_GetBooleanFromObj
#  define Tcl_GetBooleanFromObj (*TcldeclsVptr->V_Tcl_GetBooleanFromObj)
#endif

#ifndef Tcl_GetChannel
#  define Tcl_GetChannel (*TcldeclsVptr->V_Tcl_GetChannel)
#endif

#ifndef Tcl_GetCommandInfo
#  define Tcl_GetCommandInfo (*TcldeclsVptr->V_Tcl_GetCommandInfo)
#endif

#ifndef Tcl_GetDouble
#  define Tcl_GetDouble (*TcldeclsVptr->V_Tcl_GetDouble)
#endif

#ifndef Tcl_GetDoubleFromObj
#  define Tcl_GetDoubleFromObj (*TcldeclsVptr->V_Tcl_GetDoubleFromObj)
#endif

#ifndef Tcl_GetEncoding
#  define Tcl_GetEncoding (*TcldeclsVptr->V_Tcl_GetEncoding)
#endif

#ifndef Tcl_GetEncodingName
#  define Tcl_GetEncodingName (*TcldeclsVptr->V_Tcl_GetEncodingName)
#endif

#ifndef Tcl_GetIndexFromObj
#  define Tcl_GetIndexFromObj (*TcldeclsVptr->V_Tcl_GetIndexFromObj)
#endif

#ifndef Tcl_GetInt
#  define Tcl_GetInt (*TcldeclsVptr->V_Tcl_GetInt)
#endif

#ifndef Tcl_GetIntFromObj
#  define Tcl_GetIntFromObj (*TcldeclsVptr->V_Tcl_GetIntFromObj)
#endif

#ifndef Tcl_GetLongFromObj
#  define Tcl_GetLongFromObj (*TcldeclsVptr->V_Tcl_GetLongFromObj)
#endif

#ifndef Tcl_GetObjResult
#  define Tcl_GetObjResult (*TcldeclsVptr->V_Tcl_GetObjResult)
#endif

#ifndef Tcl_GetObjType
#  define Tcl_GetObjType (*TcldeclsVptr->V_Tcl_GetObjType)
#endif

#ifndef Tcl_GetRegExpFromObj
#  define Tcl_GetRegExpFromObj (*TcldeclsVptr->V_Tcl_GetRegExpFromObj)
#endif

#ifndef Tcl_GetStdChannel
#  define Tcl_GetStdChannel (*TcldeclsVptr->V_Tcl_GetStdChannel)
#endif

#ifndef Tcl_GetString
#  define Tcl_GetString (*TcldeclsVptr->V_Tcl_GetString)
#endif

#ifndef Tcl_GetStringFromObj
#  define Tcl_GetStringFromObj (*TcldeclsVptr->V_Tcl_GetStringFromObj)
#endif

#ifndef Tcl_GetStringResult
#  define Tcl_GetStringResult (*TcldeclsVptr->V_Tcl_GetStringResult)
#endif

#ifndef Tcl_GetThreadData
#  define Tcl_GetThreadData (*TcldeclsVptr->V_Tcl_GetThreadData)
#endif

#ifndef Tcl_GetTime
#  define Tcl_GetTime (*TcldeclsVptr->V_Tcl_GetTime)
#endif

#ifndef Tcl_GetVar
#  define Tcl_GetVar (*TcldeclsVptr->V_Tcl_GetVar)
#endif

#ifndef Tcl_GetVar2
#  define Tcl_GetVar2 (*TcldeclsVptr->V_Tcl_GetVar2)
#endif

#ifndef Tcl_GetVar2Ex
#  define Tcl_GetVar2Ex (*TcldeclsVptr->V_Tcl_GetVar2Ex)
#endif

#ifndef Tcl_GlobalEval
#  define Tcl_GlobalEval (*TcldeclsVptr->V_Tcl_GlobalEval)
#endif

#ifndef Tcl_HideCommand
#  define Tcl_HideCommand (*TcldeclsVptr->V_Tcl_HideCommand)
#endif

#ifndef Tcl_InitHashTable
#  define Tcl_InitHashTable (*TcldeclsVptr->V_Tcl_InitHashTable)
#endif

#ifndef Tcl_IsSafe
#  define Tcl_IsSafe (*TcldeclsVptr->V_Tcl_IsSafe)
#endif

#ifndef Tcl_LinkVar
#  define Tcl_LinkVar (*TcldeclsVptr->V_Tcl_LinkVar)
#endif

#ifndef Tcl_ListObjAppendElement
#  define Tcl_ListObjAppendElement (*TcldeclsVptr->V_Tcl_ListObjAppendElement)
#endif

#ifndef Tcl_ListObjGetElements
#  define Tcl_ListObjGetElements (*TcldeclsVptr->V_Tcl_ListObjGetElements)
#endif

#ifndef Tcl_ListObjIndex
#  define Tcl_ListObjIndex (*TcldeclsVptr->V_Tcl_ListObjIndex)
#endif

#ifndef Tcl_ListObjLength
#  define Tcl_ListObjLength (*TcldeclsVptr->V_Tcl_ListObjLength)
#endif

#ifndef Tcl_ListObjReplace
#  define Tcl_ListObjReplace (*TcldeclsVptr->V_Tcl_ListObjReplace)
#endif

#ifndef Tcl_NewBooleanObj
#  define Tcl_NewBooleanObj (*TcldeclsVptr->V_Tcl_NewBooleanObj)
#endif

#ifndef Tcl_NewDoubleObj
#  define Tcl_NewDoubleObj (*TcldeclsVptr->V_Tcl_NewDoubleObj)
#endif

#ifndef Tcl_NewIntObj
#  define Tcl_NewIntObj (*TcldeclsVptr->V_Tcl_NewIntObj)
#endif

#ifndef Tcl_NewListObj
#  define Tcl_NewListObj (*TcldeclsVptr->V_Tcl_NewListObj)
#endif

#ifndef Tcl_NewLongObj
#  define Tcl_NewLongObj (*TcldeclsVptr->V_Tcl_NewLongObj)
#endif

#ifndef Tcl_NewObj
#  define Tcl_NewObj (*TcldeclsVptr->V_Tcl_NewObj)
#endif

#ifndef Tcl_NewStringObj
#  define Tcl_NewStringObj (*TcldeclsVptr->V_Tcl_NewStringObj)
#endif

#ifndef Tcl_NextHashEntry
#  define Tcl_NextHashEntry (*TcldeclsVptr->V_Tcl_NextHashEntry)
#endif

#ifndef Tcl_NumUtfChars
#  define Tcl_NumUtfChars (*TcldeclsVptr->V_Tcl_NumUtfChars)
#endif

#ifndef Tcl_ObjGetVar2
#  define Tcl_ObjGetVar2 (*TcldeclsVptr->V_Tcl_ObjGetVar2)
#endif

#ifndef Tcl_ObjSetVar2
#  define Tcl_ObjSetVar2 (*TcldeclsVptr->V_Tcl_ObjSetVar2)
#endif

#ifndef Tcl_OpenFileChannel
#  define Tcl_OpenFileChannel (*TcldeclsVptr->V_Tcl_OpenFileChannel)
#endif

#ifndef Tcl_Panic
#  define Tcl_Panic (*TcldeclsVptr->V_Tcl_Panic)
#endif

#ifndef Tcl_PosixError
#  define Tcl_PosixError (*TcldeclsVptr->V_Tcl_PosixError)
#endif

#ifndef Tcl_Preserve
#  define Tcl_Preserve (*TcldeclsVptr->V_Tcl_Preserve)
#endif

#ifndef Tcl_Read
#  define Tcl_Read (*TcldeclsVptr->V_Tcl_Read)
#endif

#ifndef Tcl_RegExpExec
#  define Tcl_RegExpExec (*TcldeclsVptr->V_Tcl_RegExpExec)
#endif

#ifndef Tcl_RegExpRange
#  define Tcl_RegExpRange (*TcldeclsVptr->V_Tcl_RegExpRange)
#endif

#ifndef Tcl_Release
#  define Tcl_Release (*TcldeclsVptr->V_Tcl_Release)
#endif

#ifndef Tcl_ResetResult
#  define Tcl_ResetResult (*TcldeclsVptr->V_Tcl_ResetResult)
#endif

#ifndef Tcl_Seek
#  define Tcl_Seek (*TcldeclsVptr->V_Tcl_Seek)
#endif

#ifndef Tcl_ServiceEvent
#  define Tcl_ServiceEvent (*TcldeclsVptr->V_Tcl_ServiceEvent)
#endif

#ifndef Tcl_SetAssocData
#  define Tcl_SetAssocData (*TcldeclsVptr->V_Tcl_SetAssocData)
#endif

#ifndef Tcl_SetBooleanObj
#  define Tcl_SetBooleanObj (*TcldeclsVptr->V_Tcl_SetBooleanObj)
#endif

#ifndef Tcl_SetChannelOption
#  define Tcl_SetChannelOption (*TcldeclsVptr->V_Tcl_SetChannelOption)
#endif

#ifndef Tcl_SetCommandInfo
#  define Tcl_SetCommandInfo (*TcldeclsVptr->V_Tcl_SetCommandInfo)
#endif

#ifndef Tcl_SetDoubleObj
#  define Tcl_SetDoubleObj (*TcldeclsVptr->V_Tcl_SetDoubleObj)
#endif

#ifndef Tcl_SetIntObj
#  define Tcl_SetIntObj (*TcldeclsVptr->V_Tcl_SetIntObj)
#endif

#ifndef Tcl_SetListObj
#  define Tcl_SetListObj (*TcldeclsVptr->V_Tcl_SetListObj)
#endif

#ifndef Tcl_SetLongObj
#  define Tcl_SetLongObj (*TcldeclsVptr->V_Tcl_SetLongObj)
#endif

#ifndef Tcl_SetMaxBlockTime
#  define Tcl_SetMaxBlockTime (*TcldeclsVptr->V_Tcl_SetMaxBlockTime)
#endif

#ifndef Tcl_SetObjErrorCode
#  define Tcl_SetObjErrorCode (*TcldeclsVptr->V_Tcl_SetObjErrorCode)
#endif

#ifndef Tcl_SetObjResult
#  define Tcl_SetObjResult (*TcldeclsVptr->V_Tcl_SetObjResult)
#endif

#ifndef Tcl_SetResult
#  define Tcl_SetResult (*TcldeclsVptr->V_Tcl_SetResult)
#endif

#ifndef Tcl_SetStringObj
#  define Tcl_SetStringObj (*TcldeclsVptr->V_Tcl_SetStringObj)
#endif

#ifndef Tcl_SetVar
#  define Tcl_SetVar (*TcldeclsVptr->V_Tcl_SetVar)
#endif

#ifndef Tcl_Sleep
#  define Tcl_Sleep (*TcldeclsVptr->V_Tcl_Sleep)
#endif

#ifndef Tcl_StringMatch
#  define Tcl_StringMatch (*TcldeclsVptr->V_Tcl_StringMatch)
#endif

#ifndef Tcl_TranslateFileName
#  define Tcl_TranslateFileName (*TcldeclsVptr->V_Tcl_TranslateFileName)
#endif

#ifndef Tcl_UniCharIsAlpha
#  define Tcl_UniCharIsAlpha (*TcldeclsVptr->V_Tcl_UniCharIsAlpha)
#endif

#ifndef Tcl_UniCharIsSpace
#  define Tcl_UniCharIsSpace (*TcldeclsVptr->V_Tcl_UniCharIsSpace)
#endif

#ifndef Tcl_UniCharIsUpper
#  define Tcl_UniCharIsUpper (*TcldeclsVptr->V_Tcl_UniCharIsUpper)
#endif

#ifndef Tcl_UniCharIsWordChar
#  define Tcl_UniCharIsWordChar (*TcldeclsVptr->V_Tcl_UniCharIsWordChar)
#endif

#ifndef Tcl_UniCharToLower
#  define Tcl_UniCharToLower (*TcldeclsVptr->V_Tcl_UniCharToLower)
#endif

#ifndef Tcl_UniCharToUpper
#  define Tcl_UniCharToUpper (*TcldeclsVptr->V_Tcl_UniCharToUpper)
#endif

#ifndef Tcl_UniCharToUtf
#  define Tcl_UniCharToUtf (*TcldeclsVptr->V_Tcl_UniCharToUtf)
#endif

#ifndef Tcl_UnlinkVar
#  define Tcl_UnlinkVar (*TcldeclsVptr->V_Tcl_UnlinkVar)
#endif

#ifndef Tcl_UtfAtIndex
#  define Tcl_UtfAtIndex (*TcldeclsVptr->V_Tcl_UtfAtIndex)
#endif

#ifndef Tcl_UtfCharComplete
#  define Tcl_UtfCharComplete (*TcldeclsVptr->V_Tcl_UtfCharComplete)
#endif

#ifndef Tcl_UtfNext
#  define Tcl_UtfNext (*TcldeclsVptr->V_Tcl_UtfNext)
#endif

#ifndef Tcl_UtfPrev
#  define Tcl_UtfPrev (*TcldeclsVptr->V_Tcl_UtfPrev)
#endif

#ifndef Tcl_UtfToExternal
#  define Tcl_UtfToExternal (*TcldeclsVptr->V_Tcl_UtfToExternal)
#endif

#ifndef Tcl_UtfToExternalDString
#  define Tcl_UtfToExternalDString (*TcldeclsVptr->V_Tcl_UtfToExternalDString)
#endif

#ifndef Tcl_UtfToLower
#  define Tcl_UtfToLower (*TcldeclsVptr->V_Tcl_UtfToLower)
#endif

#ifndef Tcl_UtfToUniChar
#  define Tcl_UtfToUniChar (*TcldeclsVptr->V_Tcl_UtfToUniChar)
#endif

#ifndef Tcl_Write
#  define Tcl_Write (*TcldeclsVptr->V_Tcl_Write)
#endif

#ifndef Tcl_WriteChars
#  define Tcl_WriteChars (*TcldeclsVptr->V_Tcl_WriteChars)
#endif

#ifndef Tcl_WrongNumArgs
#  define Tcl_WrongNumArgs (*TcldeclsVptr->V_Tcl_WrongNumArgs)
#endif

#endif /* NO_VTABLES */
#endif /* _TCLDECLS_VM */
