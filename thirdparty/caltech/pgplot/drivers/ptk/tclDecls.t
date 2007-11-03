#ifdef _TCLDECLS
#ifndef Tcl_AddErrorInfo
VFUNC(void,Tcl_AddErrorInfo,V_Tcl_AddErrorInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * message)))
#endif /* #ifndef Tcl_AddErrorInfo */

#ifndef Tcl_AllowExceptions
VFUNC(void,Tcl_AllowExceptions,V_Tcl_AllowExceptions,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_AllowExceptions */

#ifndef Tcl_AppendElement
VFUNC(void,Tcl_AppendElement,V_Tcl_AppendElement,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * string)))
#endif /* #ifndef Tcl_AppendElement */

#ifndef Tcl_AppendObjToObj
VFUNC(void,Tcl_AppendObjToObj,V_Tcl_AppendObjToObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				Tcl_Obj * appendObjPtr)))
#endif /* #ifndef Tcl_AppendObjToObj */

#ifndef Tcl_AppendResult
VFUNC(void,Tcl_AppendResult,V_Tcl_AppendResult,_ANSI_ARGS_(TCL_VARARGS(Tcl_Interp *,interp)))
#endif /* #ifndef Tcl_AppendResult */

#ifndef Tcl_AppendStringsToObj
VFUNC(void,Tcl_AppendStringsToObj,V_Tcl_AppendStringsToObj,_ANSI_ARGS_(TCL_VARARGS(Tcl_Obj *,objPtr)))
#endif /* #ifndef Tcl_AppendStringsToObj */

#ifndef Tcl_AppendToObj
VFUNC(void,Tcl_AppendToObj,V_Tcl_AppendToObj,_ANSI_ARGS_((Tcl_Obj* objPtr,
				CONST char* bytes, int length)))
#endif /* #ifndef Tcl_AppendToObj */

#ifndef Tcl_BackgroundError
VFUNC(void,Tcl_BackgroundError,V_Tcl_BackgroundError,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_BackgroundError */

#ifndef Tcl_CallWhenDeleted
VFUNC(void,Tcl_CallWhenDeleted,V_Tcl_CallWhenDeleted,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_InterpDeleteProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tcl_CallWhenDeleted */

#ifndef Tcl_CancelIdleCall
VFUNC(void,Tcl_CancelIdleCall,V_Tcl_CancelIdleCall,_ANSI_ARGS_((
				Tcl_IdleProc * idleProc,
				ClientData clientData)))
#endif /* #ifndef Tcl_CancelIdleCall */

#ifndef Tcl_Close
VFUNC(int,Tcl_Close,V_Tcl_Close,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Channel chan)))
#endif /* #ifndef Tcl_Close */

#ifndef Tcl_ConcatObj
VFUNC(Tcl_Obj *,Tcl_ConcatObj,V_Tcl_ConcatObj,_ANSI_ARGS_((int objc,
				Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tcl_ConcatObj */

#ifndef Tcl_CreateEventSource
VFUNC(void,Tcl_CreateEventSource,V_Tcl_CreateEventSource,_ANSI_ARGS_((
				Tcl_EventSetupProc * setupProc,
				Tcl_EventCheckProc * checkProc,
				ClientData clientData)))
#endif /* #ifndef Tcl_CreateEventSource */

#ifndef Tcl_CreateExitHandler
VFUNC(void,Tcl_CreateExitHandler,V_Tcl_CreateExitHandler,_ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData)))
#endif /* #ifndef Tcl_CreateExitHandler */

#ifndef Tcl_CreateFileHandler
#if !defined(__WIN32__) && !defined(MAC_TCL)
VFUNC(void,Tcl_CreateFileHandler,V_Tcl_CreateFileHandler,_ANSI_ARGS_((int fd, int mask,
				Tcl_FileProc * proc, ClientData clientData)))
#endif /* #if !defined(__WIN32__) && !defined(MAC_TCL) */
#endif /* #ifndef Tcl_CreateFileHandler */

#ifndef Tcl_CreateInterp
VFUNC(Tcl_Interp *,Tcl_CreateInterp,V_Tcl_CreateInterp,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_CreateInterp */

#ifndef Tcl_CreateObjCommand
VFUNC(Tcl_Command,Tcl_CreateObjCommand,V_Tcl_CreateObjCommand,_ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * cmdName,
				Tcl_ObjCmdProc * proc, ClientData clientData,
				Tcl_CmdDeleteProc * deleteProc)))
#endif /* #ifndef Tcl_CreateObjCommand */

#ifndef Tcl_CreateTimerHandler
VFUNC(Tcl_TimerToken,Tcl_CreateTimerHandler,V_Tcl_CreateTimerHandler,_ANSI_ARGS_((int milliseconds,
				Tcl_TimerProc * proc, ClientData clientData)))
#endif /* #ifndef Tcl_CreateTimerHandler */

#ifndef Tcl_DStringAppend
VFUNC(char *,Tcl_DStringAppend,V_Tcl_DStringAppend,_ANSI_ARGS_((Tcl_DString * dsPtr,
				CONST char * str, int length)))
#endif /* #ifndef Tcl_DStringAppend */

#ifndef Tcl_DStringAppendElement
VFUNC(char *,Tcl_DStringAppendElement,V_Tcl_DStringAppendElement,_ANSI_ARGS_((
				Tcl_DString * dsPtr, CONST char * string)))
#endif /* #ifndef Tcl_DStringAppendElement */

#ifndef Tcl_DStringFree
VFUNC(void,Tcl_DStringFree,V_Tcl_DStringFree,_ANSI_ARGS_((Tcl_DString * dsPtr)))
#endif /* #ifndef Tcl_DStringFree */

#ifndef Tcl_DStringInit
VFUNC(void,Tcl_DStringInit,V_Tcl_DStringInit,_ANSI_ARGS_((Tcl_DString * dsPtr)))
#endif /* #ifndef Tcl_DStringInit */

#ifndef Tcl_DStringResult
VFUNC(void,Tcl_DStringResult,V_Tcl_DStringResult,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_DString * dsPtr)))
#endif /* #ifndef Tcl_DStringResult */

#ifndef Tcl_DStringSetLength
VFUNC(void,Tcl_DStringSetLength,V_Tcl_DStringSetLength,_ANSI_ARGS_((
				Tcl_DString * dsPtr, int length)))
#endif /* #ifndef Tcl_DStringSetLength */

#ifndef Tcl_DeleteEventSource
VFUNC(void,Tcl_DeleteEventSource,V_Tcl_DeleteEventSource,_ANSI_ARGS_((
				Tcl_EventSetupProc * setupProc,
				Tcl_EventCheckProc * checkProc,
				ClientData clientData)))
#endif /* #ifndef Tcl_DeleteEventSource */

#ifndef Tcl_DeleteFileHandler
#if !defined(__WIN32__) && !defined(MAC_TCL)
VFUNC(void,Tcl_DeleteFileHandler,V_Tcl_DeleteFileHandler,_ANSI_ARGS_((int fd)))
#endif /* #if !defined(__WIN32__) && !defined(MAC_TCL) */
#endif /* #ifndef Tcl_DeleteFileHandler */

#ifndef Tcl_DeleteHashEntry
VFUNC(void,Tcl_DeleteHashEntry,V_Tcl_DeleteHashEntry,_ANSI_ARGS_((
				Tcl_HashEntry * entryPtr)))
#endif /* #ifndef Tcl_DeleteHashEntry */

#ifndef Tcl_DeleteHashTable
VFUNC(void,Tcl_DeleteHashTable,V_Tcl_DeleteHashTable,_ANSI_ARGS_((
				Tcl_HashTable * tablePtr)))
#endif /* #ifndef Tcl_DeleteHashTable */

#ifndef Tcl_DeleteInterp
VFUNC(void,Tcl_DeleteInterp,V_Tcl_DeleteInterp,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_DeleteInterp */

#ifndef Tcl_DeleteTimerHandler
VFUNC(void,Tcl_DeleteTimerHandler,V_Tcl_DeleteTimerHandler,_ANSI_ARGS_((
				Tcl_TimerToken token)))
#endif /* #ifndef Tcl_DeleteTimerHandler */

#ifndef Tcl_DoOneEvent
VFUNC(int,Tcl_DoOneEvent,V_Tcl_DoOneEvent,_ANSI_ARGS_((int flags)))
#endif /* #ifndef Tcl_DoOneEvent */

#ifndef Tcl_DoWhenIdle
VFUNC(void,Tcl_DoWhenIdle,V_Tcl_DoWhenIdle,_ANSI_ARGS_((Tcl_IdleProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tcl_DoWhenIdle */

#ifndef Tcl_DuplicateObj
VFUNC(Tcl_Obj *,Tcl_DuplicateObj,V_Tcl_DuplicateObj,_ANSI_ARGS_((Tcl_Obj * objPtr)))
#endif /* #ifndef Tcl_DuplicateObj */

#ifndef Tcl_Eof
VFUNC(int,Tcl_Eof,V_Tcl_Eof,_ANSI_ARGS_((Tcl_Channel chan)))
#endif /* #ifndef Tcl_Eof */

#ifndef Tcl_EvalObjEx
VFUNC(int,Tcl_EvalObjEx,V_Tcl_EvalObjEx,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, int flags)))
#endif /* #ifndef Tcl_EvalObjEx */

#ifndef Tcl_EvalObjv
VFUNC(int,Tcl_EvalObjv,V_Tcl_EvalObjv,_ANSI_ARGS_((Tcl_Interp * interp,
				int objc, Tcl_Obj *CONST objv[], int flags)))
#endif /* #ifndef Tcl_EvalObjv */

#ifndef Tcl_EventuallyFree
VFUNC(void,Tcl_EventuallyFree,V_Tcl_EventuallyFree,_ANSI_ARGS_((
				ClientData clientData,
				Tcl_FreeProc * freeProc)))
#endif /* #ifndef Tcl_EventuallyFree */

#ifndef Tcl_ExternalToUtf
VFUNC(int,Tcl_ExternalToUtf,V_Tcl_ExternalToUtf,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, int flags,
				Tcl_EncodingState * statePtr, char * dst,
				int dstLen, int * srcReadPtr,
				int * dstWrotePtr, int * dstCharsPtr)))
#endif /* #ifndef Tcl_ExternalToUtf */

#ifndef Tcl_ExternalToUtfDString
VFUNC(char *,Tcl_ExternalToUtfDString,V_Tcl_ExternalToUtfDString,_ANSI_ARGS_((
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, Tcl_DString * dsPtr)))
#endif /* #ifndef Tcl_ExternalToUtfDString */

#ifndef Tcl_FirstHashEntry
VFUNC(Tcl_HashEntry *,Tcl_FirstHashEntry,V_Tcl_FirstHashEntry,_ANSI_ARGS_((
				Tcl_HashTable * tablePtr,
				Tcl_HashSearch * searchPtr)))
#endif /* #ifndef Tcl_FirstHashEntry */

#ifndef Tcl_FreeEncoding
VFUNC(void,Tcl_FreeEncoding,V_Tcl_FreeEncoding,_ANSI_ARGS_((Tcl_Encoding encoding)))
#endif /* #ifndef Tcl_FreeEncoding */

#ifndef Tcl_GetAssocData
VFUNC(ClientData,Tcl_GetAssocData,V_Tcl_GetAssocData,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name,
				Tcl_InterpDeleteProc ** procPtr)))
#endif /* #ifndef Tcl_GetAssocData */

#ifndef Tcl_GetBooleanFromObj
VFUNC(int,Tcl_GetBooleanFromObj,V_Tcl_GetBooleanFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * objPtr,
				int * boolPtr)))
#endif /* #ifndef Tcl_GetBooleanFromObj */

#ifndef Tcl_GetChannel
VFUNC(Tcl_Channel,Tcl_GetChannel,V_Tcl_GetChannel,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * chanName, int * modePtr)))
#endif /* #ifndef Tcl_GetChannel */

#ifndef Tcl_GetCommandInfo
VFUNC(int,Tcl_GetCommandInfo,V_Tcl_GetCommandInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * cmdName, Tcl_CmdInfo * infoPtr)))
#endif /* #ifndef Tcl_GetCommandInfo */

#ifndef Tcl_GetDouble
VFUNC(int,Tcl_GetDouble,V_Tcl_GetDouble,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, double * doublePtr)))
#endif /* #ifndef Tcl_GetDouble */

#ifndef Tcl_GetDoubleFromObj
VFUNC(int,Tcl_GetDoubleFromObj,V_Tcl_GetDoubleFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * objPtr,
				double * doublePtr)))
#endif /* #ifndef Tcl_GetDoubleFromObj */

#ifndef Tcl_GetEncoding
VFUNC(Tcl_Encoding,Tcl_GetEncoding,V_Tcl_GetEncoding,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name)))
#endif /* #ifndef Tcl_GetEncoding */

#ifndef Tcl_GetEncodingName
VFUNC(CONST84_RETURN char *,Tcl_GetEncodingName,V_Tcl_GetEncodingName,_ANSI_ARGS_((
				Tcl_Encoding encoding)))
#endif /* #ifndef Tcl_GetEncodingName */

#ifndef Tcl_GetIndexFromObj
VFUNC(int,Tcl_GetIndexFromObj,V_Tcl_GetIndexFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, CONST char ** tablePtr,
				CONST char * msg, int flags, int * indexPtr)))
#endif /* #ifndef Tcl_GetIndexFromObj */

#ifndef Tcl_GetInt
VFUNC(int,Tcl_GetInt,V_Tcl_GetInt,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, int * intPtr)))
#endif /* #ifndef Tcl_GetInt */

#ifndef Tcl_GetIntFromObj
VFUNC(int,Tcl_GetIntFromObj,V_Tcl_GetIntFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, int * intPtr)))
#endif /* #ifndef Tcl_GetIntFromObj */

#ifndef Tcl_GetLongFromObj
VFUNC(int,Tcl_GetLongFromObj,V_Tcl_GetLongFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, long * longPtr)))
#endif /* #ifndef Tcl_GetLongFromObj */

#ifndef Tcl_GetObjResult
VFUNC(Tcl_Obj *,Tcl_GetObjResult,V_Tcl_GetObjResult,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_GetObjResult */

#ifndef Tcl_GetObjType
VFUNC(Tcl_ObjType *,Tcl_GetObjType,V_Tcl_GetObjType,_ANSI_ARGS_((CONST char * typeName)))
#endif /* #ifndef Tcl_GetObjType */

#ifndef Tcl_GetRegExpFromObj
VFUNC(Tcl_RegExp,Tcl_GetRegExpFromObj,V_Tcl_GetRegExpFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * patObj,
				int flags)))
#endif /* #ifndef Tcl_GetRegExpFromObj */

#ifndef Tcl_GetStdChannel
VFUNC(Tcl_Channel,Tcl_GetStdChannel,V_Tcl_GetStdChannel,_ANSI_ARGS_((int type)))
#endif /* #ifndef Tcl_GetStdChannel */

#ifndef Tcl_GetString
VFUNC(char *,Tcl_GetString,V_Tcl_GetString,_ANSI_ARGS_((Tcl_Obj * objPtr)))
#endif /* #ifndef Tcl_GetString */

#ifndef Tcl_GetStringFromObj
VFUNC(char *,Tcl_GetStringFromObj,V_Tcl_GetStringFromObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				int * lengthPtr)))
#endif /* #ifndef Tcl_GetStringFromObj */

#ifndef Tcl_GetStringResult
VFUNC(CONST84_RETURN char *,Tcl_GetStringResult,V_Tcl_GetStringResult,_ANSI_ARGS_((
				Tcl_Interp * interp)))
#endif /* #ifndef Tcl_GetStringResult */

#ifndef Tcl_GetThreadData
VFUNC(VOID *,Tcl_GetThreadData,V_Tcl_GetThreadData,_ANSI_ARGS_((
				Tcl_ThreadDataKey * keyPtr, int size)))
#endif /* #ifndef Tcl_GetThreadData */

#ifndef Tcl_GetTime
VFUNC(void,Tcl_GetTime,V_Tcl_GetTime,_ANSI_ARGS_((Tcl_Time* timeBuf)))
#endif /* #ifndef Tcl_GetTime */

#ifndef Tcl_GetVar
VFUNC(CONST84_RETURN char *,Tcl_GetVar,V_Tcl_GetVar,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * varName, int flags)))
#endif /* #ifndef Tcl_GetVar */

#ifndef Tcl_GetVar2
VFUNC(CONST84_RETURN char *,Tcl_GetVar2,V_Tcl_GetVar2,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * part1, CONST char * part2,
				int flags)))
#endif /* #ifndef Tcl_GetVar2 */

#ifndef Tcl_GetVar2Ex
VFUNC(Tcl_Obj *,Tcl_GetVar2Ex,V_Tcl_GetVar2Ex,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * part1, CONST char * part2,
				int flags)))
#endif /* #ifndef Tcl_GetVar2Ex */

#ifndef Tcl_GlobalEval
VFUNC(int,Tcl_GlobalEval,V_Tcl_GlobalEval,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * command)))
#endif /* #ifndef Tcl_GlobalEval */

#ifndef Tcl_HideCommand
VFUNC(int,Tcl_HideCommand,V_Tcl_HideCommand,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * cmdName,
				CONST char * hiddenCmdToken)))
#endif /* #ifndef Tcl_HideCommand */

#ifndef Tcl_InitHashTable
VFUNC(void,Tcl_InitHashTable,V_Tcl_InitHashTable,_ANSI_ARGS_((
				Tcl_HashTable * tablePtr, int keyType)))
#endif /* #ifndef Tcl_InitHashTable */

#ifndef Tcl_IsSafe
VFUNC(int,Tcl_IsSafe,V_Tcl_IsSafe,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_IsSafe */

#ifndef Tcl_LinkVar
VFUNC(int,Tcl_LinkVar,V_Tcl_LinkVar,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * varName, char * addr, int type)))
#endif /* #ifndef Tcl_LinkVar */

#ifndef Tcl_ListObjAppendElement
VFUNC(int,Tcl_ListObjAppendElement,V_Tcl_ListObjAppendElement,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * listPtr,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tcl_ListObjAppendElement */

#ifndef Tcl_ListObjGetElements
VFUNC(int,Tcl_ListObjGetElements,V_Tcl_ListObjGetElements,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * listPtr,
				int * objcPtr, Tcl_Obj *** objvPtr)))
#endif /* #ifndef Tcl_ListObjGetElements */

#ifndef Tcl_ListObjIndex
VFUNC(int,Tcl_ListObjIndex,V_Tcl_ListObjIndex,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * listPtr, int index,
				Tcl_Obj ** objPtrPtr)))
#endif /* #ifndef Tcl_ListObjIndex */

#ifndef Tcl_ListObjLength
VFUNC(int,Tcl_ListObjLength,V_Tcl_ListObjLength,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * listPtr, int * lengthPtr)))
#endif /* #ifndef Tcl_ListObjLength */

#ifndef Tcl_ListObjReplace
VFUNC(int,Tcl_ListObjReplace,V_Tcl_ListObjReplace,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * listPtr, int first, int count,
				int objc, Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tcl_ListObjReplace */

#ifndef Tcl_NewBooleanObj
VFUNC(Tcl_Obj *,Tcl_NewBooleanObj,V_Tcl_NewBooleanObj,_ANSI_ARGS_((int boolValue)))
#endif /* #ifndef Tcl_NewBooleanObj */

#ifndef Tcl_NewDoubleObj
VFUNC(Tcl_Obj *,Tcl_NewDoubleObj,V_Tcl_NewDoubleObj,_ANSI_ARGS_((double doubleValue)))
#endif /* #ifndef Tcl_NewDoubleObj */

#ifndef Tcl_NewIntObj
VFUNC(Tcl_Obj *,Tcl_NewIntObj,V_Tcl_NewIntObj,_ANSI_ARGS_((int intValue)))
#endif /* #ifndef Tcl_NewIntObj */

#ifndef Tcl_NewListObj
VFUNC(Tcl_Obj *,Tcl_NewListObj,V_Tcl_NewListObj,_ANSI_ARGS_((int objc,
				Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tcl_NewListObj */

#ifndef Tcl_NewLongObj
VFUNC(Tcl_Obj *,Tcl_NewLongObj,V_Tcl_NewLongObj,_ANSI_ARGS_((long longValue)))
#endif /* #ifndef Tcl_NewLongObj */

#ifndef Tcl_NewObj
VFUNC(Tcl_Obj *,Tcl_NewObj,V_Tcl_NewObj,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_NewObj */

#ifndef Tcl_NewStringObj
VFUNC(Tcl_Obj *,Tcl_NewStringObj,V_Tcl_NewStringObj,_ANSI_ARGS_((CONST char * bytes,
				int length)))
#endif /* #ifndef Tcl_NewStringObj */

#ifndef Tcl_NextHashEntry
VFUNC(Tcl_HashEntry *,Tcl_NextHashEntry,V_Tcl_NextHashEntry,_ANSI_ARGS_((
				Tcl_HashSearch * searchPtr)))
#endif /* #ifndef Tcl_NextHashEntry */

#ifndef Tcl_NumUtfChars
VFUNC(int,Tcl_NumUtfChars,V_Tcl_NumUtfChars,_ANSI_ARGS_((CONST char * src,
				int len)))
#endif /* #ifndef Tcl_NumUtfChars */

#ifndef Tcl_ObjGetVar2
VFUNC(Tcl_Obj *,Tcl_ObjGetVar2,V_Tcl_ObjGetVar2,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * part1Ptr, Tcl_Obj * part2Ptr,
				int flags)))
#endif /* #ifndef Tcl_ObjGetVar2 */

#ifndef Tcl_ObjSetVar2
VFUNC(Tcl_Obj *,Tcl_ObjSetVar2,V_Tcl_ObjSetVar2,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * part1Ptr, Tcl_Obj * part2Ptr,
				Tcl_Obj * newValuePtr, int flags)))
#endif /* #ifndef Tcl_ObjSetVar2 */

#ifndef Tcl_OpenFileChannel
VFUNC(Tcl_Channel,Tcl_OpenFileChannel,V_Tcl_OpenFileChannel,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * fileName,
				CONST char * modeString, int permissions)))
#endif /* #ifndef Tcl_OpenFileChannel */

#ifndef Tcl_Panic
VFUNC(void,Tcl_Panic,V_Tcl_Panic,_ANSI_ARGS_(TCL_VARARGS(CONST char *,format)))
#endif /* #ifndef Tcl_Panic */

#ifndef Tcl_PosixError
VFUNC(CONST84_RETURN char *,Tcl_PosixError,V_Tcl_PosixError,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_PosixError */

#ifndef Tcl_Preserve
VFUNC(void,Tcl_Preserve,V_Tcl_Preserve,_ANSI_ARGS_((ClientData data)))
#endif /* #ifndef Tcl_Preserve */

#ifndef Tcl_Read
VFUNC(int,Tcl_Read,V_Tcl_Read,_ANSI_ARGS_((Tcl_Channel chan,
				char * bufPtr, int toRead)))
#endif /* #ifndef Tcl_Read */

#ifndef Tcl_RegExpExec
VFUNC(int,Tcl_RegExpExec,V_Tcl_RegExpExec,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_RegExp regexp, CONST char * str,
				CONST char * start)))
#endif /* #ifndef Tcl_RegExpExec */

#ifndef Tcl_RegExpRange
VFUNC(void,Tcl_RegExpRange,V_Tcl_RegExpRange,_ANSI_ARGS_((Tcl_RegExp regexp,
				int index, CONST84 char ** startPtr,
				CONST84 char ** endPtr)))
#endif /* #ifndef Tcl_RegExpRange */

#ifndef Tcl_Release
VFUNC(void,Tcl_Release,V_Tcl_Release,_ANSI_ARGS_((ClientData clientData)))
#endif /* #ifndef Tcl_Release */

#ifndef Tcl_ResetResult
VFUNC(void,Tcl_ResetResult,V_Tcl_ResetResult,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tcl_ResetResult */

#ifndef Tcl_Seek
VFUNC(Tcl_WideInt,Tcl_Seek,V_Tcl_Seek,_ANSI_ARGS_((Tcl_Channel chan,
				Tcl_WideInt offset, int mode)))
#endif /* #ifndef Tcl_Seek */

#ifndef Tcl_ServiceEvent
VFUNC(int,Tcl_ServiceEvent,V_Tcl_ServiceEvent,_ANSI_ARGS_((int flags)))
#endif /* #ifndef Tcl_ServiceEvent */

#ifndef Tcl_SetAssocData
VFUNC(void,Tcl_SetAssocData,V_Tcl_SetAssocData,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name,
				Tcl_InterpDeleteProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tcl_SetAssocData */

#ifndef Tcl_SetBooleanObj
VFUNC(void,Tcl_SetBooleanObj,V_Tcl_SetBooleanObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				int boolValue)))
#endif /* #ifndef Tcl_SetBooleanObj */

#ifndef Tcl_SetChannelOption
VFUNC(int,Tcl_SetChannelOption,V_Tcl_SetChannelOption,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Channel chan,
				CONST char * optionName,
				CONST char * newValue)))
#endif /* #ifndef Tcl_SetChannelOption */

#ifndef Tcl_SetCommandInfo
VFUNC(int,Tcl_SetCommandInfo,V_Tcl_SetCommandInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * cmdName,
				CONST Tcl_CmdInfo * infoPtr)))
#endif /* #ifndef Tcl_SetCommandInfo */

#ifndef Tcl_SetDoubleObj
VFUNC(void,Tcl_SetDoubleObj,V_Tcl_SetDoubleObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				double doubleValue)))
#endif /* #ifndef Tcl_SetDoubleObj */

#ifndef Tcl_SetIntObj
VFUNC(void,Tcl_SetIntObj,V_Tcl_SetIntObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				int intValue)))
#endif /* #ifndef Tcl_SetIntObj */

#ifndef Tcl_SetListObj
VFUNC(void,Tcl_SetListObj,V_Tcl_SetListObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				int objc, Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tcl_SetListObj */

#ifndef Tcl_SetLongObj
VFUNC(void,Tcl_SetLongObj,V_Tcl_SetLongObj,_ANSI_ARGS_((Tcl_Obj * objPtr,
				long longValue)))
#endif /* #ifndef Tcl_SetLongObj */

#ifndef Tcl_SetMaxBlockTime
VFUNC(void,Tcl_SetMaxBlockTime,V_Tcl_SetMaxBlockTime,_ANSI_ARGS_((Tcl_Time * timePtr)))
#endif /* #ifndef Tcl_SetMaxBlockTime */

#ifndef Tcl_SetObjErrorCode
VFUNC(void,Tcl_SetObjErrorCode,V_Tcl_SetObjErrorCode,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * errorObjPtr)))
#endif /* #ifndef Tcl_SetObjErrorCode */

#ifndef Tcl_SetObjResult
VFUNC(void,Tcl_SetObjResult,V_Tcl_SetObjResult,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * resultObjPtr)))
#endif /* #ifndef Tcl_SetObjResult */

#ifndef Tcl_SetResult
VFUNC(void,Tcl_SetResult,V_Tcl_SetResult,_ANSI_ARGS_((Tcl_Interp * interp,
				char * str, Tcl_FreeProc * freeProc)))
#endif /* #ifndef Tcl_SetResult */

#ifndef Tcl_SetStringObj
VFUNC(void,Tcl_SetStringObj,V_Tcl_SetStringObj,_ANSI_ARGS_((Tcl_Obj* objPtr,
				CONST char* bytes, int length)))
#endif /* #ifndef Tcl_SetStringObj */

#ifndef Tcl_SetVar
VFUNC(CONST84_RETURN char *,Tcl_SetVar,V_Tcl_SetVar,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * varName, CONST char * newValue,
				int flags)))
#endif /* #ifndef Tcl_SetVar */

#ifndef Tcl_Sleep
VFUNC(void,Tcl_Sleep,V_Tcl_Sleep,_ANSI_ARGS_((int ms)))
#endif /* #ifndef Tcl_Sleep */

#ifndef Tcl_StringMatch
VFUNC(int,Tcl_StringMatch,V_Tcl_StringMatch,_ANSI_ARGS_((CONST char * str,
				CONST char * pattern)))
#endif /* #ifndef Tcl_StringMatch */

#ifndef Tcl_TranslateFileName
VFUNC(char *,Tcl_TranslateFileName,V_Tcl_TranslateFileName,_ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * name,
				Tcl_DString * bufferPtr)))
#endif /* #ifndef Tcl_TranslateFileName */

#ifndef Tcl_UniCharIsAlpha
VFUNC(int,Tcl_UniCharIsAlpha,V_Tcl_UniCharIsAlpha,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharIsAlpha */

#ifndef Tcl_UniCharIsSpace
VFUNC(int,Tcl_UniCharIsSpace,V_Tcl_UniCharIsSpace,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharIsSpace */

#ifndef Tcl_UniCharIsUpper
VFUNC(int,Tcl_UniCharIsUpper,V_Tcl_UniCharIsUpper,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharIsUpper */

#ifndef Tcl_UniCharIsWordChar
VFUNC(int,Tcl_UniCharIsWordChar,V_Tcl_UniCharIsWordChar,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharIsWordChar */

#ifndef Tcl_UniCharToLower
VFUNC(Tcl_UniChar,Tcl_UniCharToLower,V_Tcl_UniCharToLower,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharToLower */

#ifndef Tcl_UniCharToUpper
VFUNC(Tcl_UniChar,Tcl_UniCharToUpper,V_Tcl_UniCharToUpper,_ANSI_ARGS_((int ch)))
#endif /* #ifndef Tcl_UniCharToUpper */

#ifndef Tcl_UniCharToUtf
VFUNC(int,Tcl_UniCharToUtf,V_Tcl_UniCharToUtf,_ANSI_ARGS_((int ch, char * buf)))
#endif /* #ifndef Tcl_UniCharToUtf */

#ifndef Tcl_UnlinkVar
VFUNC(void,Tcl_UnlinkVar,V_Tcl_UnlinkVar,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * varName)))
#endif /* #ifndef Tcl_UnlinkVar */

#ifndef Tcl_UtfAtIndex
VFUNC(CONST84_RETURN char *,Tcl_UtfAtIndex,V_Tcl_UtfAtIndex,_ANSI_ARGS_((CONST char * src,
				int index)))
#endif /* #ifndef Tcl_UtfAtIndex */

#ifndef Tcl_UtfCharComplete
VFUNC(int,Tcl_UtfCharComplete,V_Tcl_UtfCharComplete,_ANSI_ARGS_((CONST char * src,
				int len)))
#endif /* #ifndef Tcl_UtfCharComplete */

#ifndef Tcl_UtfNext
VFUNC(CONST84_RETURN char *,Tcl_UtfNext,V_Tcl_UtfNext,_ANSI_ARGS_((CONST char * src)))
#endif /* #ifndef Tcl_UtfNext */

#ifndef Tcl_UtfPrev
VFUNC(CONST84_RETURN char *,Tcl_UtfPrev,V_Tcl_UtfPrev,_ANSI_ARGS_((CONST char * src,
				CONST char * start)))
#endif /* #ifndef Tcl_UtfPrev */

#ifndef Tcl_UtfToExternal
VFUNC(int,Tcl_UtfToExternal,V_Tcl_UtfToExternal,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, int flags,
				Tcl_EncodingState * statePtr, char * dst,
				int dstLen, int * srcReadPtr,
				int * dstWrotePtr, int * dstCharsPtr)))
#endif /* #ifndef Tcl_UtfToExternal */

#ifndef Tcl_UtfToExternalDString
VFUNC(char *,Tcl_UtfToExternalDString,V_Tcl_UtfToExternalDString,_ANSI_ARGS_((
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, Tcl_DString * dsPtr)))
#endif /* #ifndef Tcl_UtfToExternalDString */

#ifndef Tcl_UtfToLower
VFUNC(int,Tcl_UtfToLower,V_Tcl_UtfToLower,_ANSI_ARGS_((char * src)))
#endif /* #ifndef Tcl_UtfToLower */

#ifndef Tcl_UtfToUniChar
VFUNC(int,Tcl_UtfToUniChar,V_Tcl_UtfToUniChar,_ANSI_ARGS_((CONST char * src,
				Tcl_UniChar * chPtr)))
#endif /* #ifndef Tcl_UtfToUniChar */

#ifndef Tcl_Write
VFUNC(int,Tcl_Write,V_Tcl_Write,_ANSI_ARGS_((Tcl_Channel chan,
				CONST char * s, int slen)))
#endif /* #ifndef Tcl_Write */

#ifndef Tcl_WriteChars
VFUNC(int,Tcl_WriteChars,V_Tcl_WriteChars,_ANSI_ARGS_((Tcl_Channel chan,
				CONST char * src, int srcLen)))
#endif /* #ifndef Tcl_WriteChars */

#ifndef Tcl_WrongNumArgs
VFUNC(void,Tcl_WrongNumArgs,V_Tcl_WrongNumArgs,_ANSI_ARGS_((Tcl_Interp * interp,
				int objc, Tcl_Obj *CONST objv[],
				CONST char * message)))
#endif /* #ifndef Tcl_WrongNumArgs */

#endif /* _TCLDECLS */
