#ifdef _TKEVENT
#ifndef LangAsyncCheck
VFUNC(void,LangAsyncCheck,V_LangAsyncCheck,_ANSI_ARGS_((void)))
#endif /* #ifndef LangAsyncCheck */

#ifndef LangCallCallback
VFUNC(int,LangCallCallback,V_LangCallCallback,_ANSI_ARGS_((LangCallback *cb, int flags)))
#endif /* #ifndef LangCallCallback */

#ifndef LangCallbackObj
VFUNC(Tcl_Obj *,LangCallbackObj,V_LangCallbackObj,_ANSI_ARGS_((LangCallback *)))
#endif /* #ifndef LangCallbackObj */

#ifndef LangCmpCallback
VFUNC(int,LangCmpCallback,V_LangCmpCallback,_ANSI_ARGS_((LangCallback *a,Tcl_Obj * b)))
#endif /* #ifndef LangCmpCallback */

#ifndef LangCopyCallback
VFUNC(LangCallback *,LangCopyCallback,V_LangCopyCallback,_ANSI_ARGS_((LangCallback *)))
#endif /* #ifndef LangCopyCallback */

#ifndef LangDebug
VFUNC(void,LangDebug,V_LangDebug,_ANSI_ARGS_((CONST char *fmt,...)))
#endif /* #ifndef LangDebug */

#ifndef LangFreeCallback
VFUNC(void,LangFreeCallback,V_LangFreeCallback,_ANSI_ARGS_((LangCallback *)))
#endif /* #ifndef LangFreeCallback */

#ifndef LangMakeCallback
VFUNC(LangCallback *,LangMakeCallback,V_LangMakeCallback,_ANSI_ARGS_((Tcl_Obj *)))
#endif /* #ifndef LangMakeCallback */

#ifndef LangOldCallbackArg
VFUNC(Tcl_Obj *,LangOldCallbackArg,V_LangOldCallbackArg,_ANSI_ARGS_((LangCallback *,char *,int)))
#endif /* #ifndef LangOldCallbackArg */

#ifndef LangPushCallbackArgs
VFUNC(void,LangPushCallbackArgs,V_LangPushCallbackArgs,_ANSI_ARGS_((LangCallback **svp)))
#endif /* #ifndef LangPushCallbackArgs */

#ifndef Lang_NoteOwner
VFUNC(void,Lang_NoteOwner,V_Lang_NoteOwner,_ANSI_ARGS_((void *owner,void *packet,
				CONST char *file, int line)))
#endif /* #ifndef Lang_NoteOwner */

#ifndef Lang_OSHandle
VFUNC(long,Lang_OSHandle,V_Lang_OSHandle,_ANSI_ARGS_((int fd)))
#endif /* #ifndef Lang_OSHandle */

#ifndef Tcl_AlertNotifier
VFUNC(void,Tcl_AlertNotifier,V_Tcl_AlertNotifier,_ANSI_ARGS_((ClientData clientData)))
#endif /* #ifndef Tcl_AlertNotifier */

#ifndef Tcl_Alloc
VFUNC(char *,Tcl_Alloc,V_Tcl_Alloc,_ANSI_ARGS_((unsigned int size)))
#endif /* #ifndef Tcl_Alloc */

#ifndef Tcl_AsyncCreate
VFUNC(Tcl_AsyncHandler,Tcl_AsyncCreate,V_Tcl_AsyncCreate,_ANSI_ARGS_((Tcl_AsyncProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tcl_AsyncCreate */

#ifndef Tcl_AsyncDelete
VFUNC(void,Tcl_AsyncDelete,V_Tcl_AsyncDelete,_ANSI_ARGS_((Tcl_AsyncHandler async)))
#endif /* #ifndef Tcl_AsyncDelete */

#ifndef Tcl_AsyncInvoke
VFUNC(int,Tcl_AsyncInvoke,V_Tcl_AsyncInvoke,_ANSI_ARGS_((Tcl_Interp * interp,
				int code)))
#endif /* #ifndef Tcl_AsyncInvoke */

#ifndef Tcl_AsyncMark
VFUNC(void,Tcl_AsyncMark,V_Tcl_AsyncMark,_ANSI_ARGS_((Tcl_AsyncHandler async)))
#endif /* #ifndef Tcl_AsyncMark */

#ifndef Tcl_AsyncReady
VFUNC(int,Tcl_AsyncReady,V_Tcl_AsyncReady,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_AsyncReady */

#ifndef Tcl_AttemptDbCkalloc
VFUNC(char *,Tcl_AttemptDbCkalloc,V_Tcl_AttemptDbCkalloc,_ANSI_ARGS_((unsigned int size,
				CONST char * file, int line)))
#endif /* #ifndef Tcl_AttemptDbCkalloc */

#ifndef Tcl_CancelIdleCall
VFUNC(void,Tcl_CancelIdleCall,V_Tcl_CancelIdleCall,_ANSI_ARGS_((Tcl_IdleProc *idleProc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_CancelIdleCall */

#ifndef Tcl_CreateEventSource
VFUNC(void,Tcl_CreateEventSource,V_Tcl_CreateEventSource,_ANSI_ARGS_((
			    Tcl_EventSetupProc *setupProc,
			    Tcl_EventCheckProc *checkProc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_CreateEventSource */

#ifndef Tcl_CreateExitHandler
VFUNC(void,Tcl_CreateExitHandler,V_Tcl_CreateExitHandler,_ANSI_ARGS_((Tcl_ExitProc *proc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_CreateExitHandler */

#ifndef Tcl_CreateFileHandler
VFUNC(void,Tcl_CreateFileHandler,V_Tcl_CreateFileHandler,_ANSI_ARGS_((
    			    int fd, int mask, Tcl_FileProc *proc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_CreateFileHandler */

#ifndef Tcl_CreateThreadExitHandler
VFUNC(void,Tcl_CreateThreadExitHandler,V_Tcl_CreateThreadExitHandler,_ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData)))
#endif /* #ifndef Tcl_CreateThreadExitHandler */

#ifndef Tcl_CreateTimerHandler
VFUNC(Tcl_TimerToken,Tcl_CreateTimerHandler,V_Tcl_CreateTimerHandler,_ANSI_ARGS_((int milliseconds,
			    Tcl_TimerProc *proc, ClientData clientData)))
#endif /* #ifndef Tcl_CreateTimerHandler */

#ifndef Tcl_DbCkalloc
VFUNC(char *,Tcl_DbCkalloc,V_Tcl_DbCkalloc,_ANSI_ARGS_((unsigned int size,CONST char *file,int line)))
#endif /* #ifndef Tcl_DbCkalloc */

#ifndef Tcl_DbCkfree
VFUNC(int,Tcl_DbCkfree,V_Tcl_DbCkfree,_ANSI_ARGS_((char * ptr, CONST char * file,
				int line)))
#endif /* #ifndef Tcl_DbCkfree */

#ifndef Tcl_DbCkrealloc
VFUNC(char *,Tcl_DbCkrealloc,V_Tcl_DbCkrealloc,_ANSI_ARGS_((char *ptr,
			    unsigned int size, CONST char *file,int line)))
#endif /* #ifndef Tcl_DbCkrealloc */

#ifndef Tcl_DeleteEventSource
VFUNC(void,Tcl_DeleteEventSource,V_Tcl_DeleteEventSource,_ANSI_ARGS_((
			    Tcl_EventSetupProc *setupProc,
			    Tcl_EventCheckProc *checkProc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_DeleteEventSource */

#ifndef Tcl_DeleteEvents
VFUNC(void,Tcl_DeleteEvents,V_Tcl_DeleteEvents,_ANSI_ARGS_((
				Tcl_EventDeleteProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tcl_DeleteEvents */

#ifndef Tcl_DeleteExitHandler
VFUNC(void,Tcl_DeleteExitHandler,V_Tcl_DeleteExitHandler,_ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData)))
#endif /* #ifndef Tcl_DeleteExitHandler */

#ifndef Tcl_DeleteFileHandler
VFUNC(void,Tcl_DeleteFileHandler,V_Tcl_DeleteFileHandler,_ANSI_ARGS_((int fd)))
#endif /* #ifndef Tcl_DeleteFileHandler */

#ifndef Tcl_DeleteThreadExitHandler
VFUNC(void,Tcl_DeleteThreadExitHandler,V_Tcl_DeleteThreadExitHandler,_ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData)))
#endif /* #ifndef Tcl_DeleteThreadExitHandler */

#ifndef Tcl_DeleteTimerHandler
VFUNC(void,Tcl_DeleteTimerHandler,V_Tcl_DeleteTimerHandler,_ANSI_ARGS_((
			    Tcl_TimerToken token)))
#endif /* #ifndef Tcl_DeleteTimerHandler */

#ifndef Tcl_DoOneEvent
VFUNC(int,Tcl_DoOneEvent,V_Tcl_DoOneEvent,_ANSI_ARGS_((int flags)))
#endif /* #ifndef Tcl_DoOneEvent */

#ifndef Tcl_DoWhenIdle
VFUNC(void,Tcl_DoWhenIdle,V_Tcl_DoWhenIdle,_ANSI_ARGS_((Tcl_IdleProc *proc,
			    ClientData clientData)))
#endif /* #ifndef Tcl_DoWhenIdle */

#ifndef Tcl_DumpActiveMemory
VFUNC(int,Tcl_DumpActiveMemory,V_Tcl_DumpActiveMemory,_ANSI_ARGS_((CONST char * fileName)))
#endif /* #ifndef Tcl_DumpActiveMemory */

#ifndef Tcl_Exit
VFUNC(void,Tcl_Exit,V_Tcl_Exit,_ANSI_ARGS_((int status)))
#endif /* #ifndef Tcl_Exit */

#ifndef Tcl_Finalize
VFUNC(void,Tcl_Finalize,V_Tcl_Finalize,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_Finalize */

#ifndef Tcl_FinalizeNotifier
VFUNC(void,Tcl_FinalizeNotifier,V_Tcl_FinalizeNotifier,_ANSI_ARGS_((
				ClientData clientData)))
#endif /* #ifndef Tcl_FinalizeNotifier */

#ifndef Tcl_FinalizeThread
VFUNC(void,Tcl_FinalizeThread,V_Tcl_FinalizeThread,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_FinalizeThread */

#ifndef Tcl_Free
VFUNC(void,Tcl_Free,V_Tcl_Free,_ANSI_ARGS_((char *ptr)))
#endif /* #ifndef Tcl_Free */

#ifndef Tcl_GetCurrentThread
VFUNC(Tcl_ThreadId,Tcl_GetCurrentThread,V_Tcl_GetCurrentThread,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_GetCurrentThread */

#ifndef Tcl_GetServiceMode
VFUNC(int,Tcl_GetServiceMode,V_Tcl_GetServiceMode,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_GetServiceMode */

#ifndef Tcl_GetThreadData
VFUNC(VOID *,Tcl_GetThreadData,V_Tcl_GetThreadData,_ANSI_ARGS_((
				Tcl_ThreadDataKey * keyPtr, int size)))
#endif /* #ifndef Tcl_GetThreadData */

#ifndef Tcl_GetTime
VFUNC(void,Tcl_GetTime,V_Tcl_GetTime,_ANSI_ARGS_((Tcl_Time *time)))
#endif /* #ifndef Tcl_GetTime */

#ifndef Tcl_InitNotifier
VFUNC(ClientData,Tcl_InitNotifier,V_Tcl_InitNotifier,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_InitNotifier */

#ifndef Tcl_Panic
VFUNC(void,Tcl_Panic,V_Tcl_Panic,_ANSI_ARGS_((CONST char *,...)))
#endif /* #ifndef Tcl_Panic */

#ifndef Tcl_QueueEvent
VFUNC(void,Tcl_QueueEvent,V_Tcl_QueueEvent,_ANSI_ARGS_((Tcl_Event *evPtr,
			    Tcl_QueuePosition position)))
#endif /* #ifndef Tcl_QueueEvent */

#ifndef Tcl_QueueProcEvent
VFUNC(void,Tcl_QueueProcEvent,V_Tcl_QueueProcEvent,_ANSI_ARGS_((Tcl_EventProc *proc,
			    Tcl_Event *evPtr,
			    Tcl_QueuePosition position)))
#endif /* #ifndef Tcl_QueueProcEvent */

#ifndef Tcl_Realloc
VFUNC(char *,Tcl_Realloc,V_Tcl_Realloc,_ANSI_ARGS_((char *ptr,
			    unsigned int size)))
#endif /* #ifndef Tcl_Realloc */

#ifndef Tcl_ServiceAll
VFUNC(int,Tcl_ServiceAll,V_Tcl_ServiceAll,_ANSI_ARGS_((void)))
#endif /* #ifndef Tcl_ServiceAll */

#ifndef Tcl_ServiceEvent
VFUNC(int,Tcl_ServiceEvent,V_Tcl_ServiceEvent,_ANSI_ARGS_((int flags)))
#endif /* #ifndef Tcl_ServiceEvent */

#ifndef Tcl_ServiceModeHook
VFUNC(void,Tcl_ServiceModeHook,V_Tcl_ServiceModeHook,_ANSI_ARGS_((int mode)))
#endif /* #ifndef Tcl_ServiceModeHook */

#ifndef Tcl_SetMaxBlockTime
VFUNC(void,Tcl_SetMaxBlockTime,V_Tcl_SetMaxBlockTime,_ANSI_ARGS_((Tcl_Time *timePtr)))
#endif /* #ifndef Tcl_SetMaxBlockTime */

#ifndef Tcl_SetNotifier
VFUNC(void,Tcl_SetNotifier,V_Tcl_SetNotifier,_ANSI_ARGS_((
				Tcl_NotifierProcs * notifierProcPtr)))
#endif /* #ifndef Tcl_SetNotifier */

#ifndef Tcl_SetServiceMode
VFUNC(int,Tcl_SetServiceMode,V_Tcl_SetServiceMode,_ANSI_ARGS_((int mode)))
#endif /* #ifndef Tcl_SetServiceMode */

#ifndef Tcl_SetTimer
VFUNC(void,Tcl_SetTimer,V_Tcl_SetTimer,_ANSI_ARGS_((Tcl_Time * timePtr)))
#endif /* #ifndef Tcl_SetTimer */

#ifndef Tcl_Sleep
VFUNC(void,Tcl_Sleep,V_Tcl_Sleep,_ANSI_ARGS_((int ms)))
#endif /* #ifndef Tcl_Sleep */

#ifndef Tcl_ThreadAlert
VFUNC(void,Tcl_ThreadAlert,V_Tcl_ThreadAlert,_ANSI_ARGS_((Tcl_ThreadId threadId)))
#endif /* #ifndef Tcl_ThreadAlert */

#ifndef Tcl_ThreadQueueEvent
VFUNC(void,Tcl_ThreadQueueEvent,V_Tcl_ThreadQueueEvent,_ANSI_ARGS_((
				Tcl_ThreadId threadId, Tcl_Event* evPtr,
				Tcl_QueuePosition position)))
#endif /* #ifndef Tcl_ThreadQueueEvent */

#ifndef Tcl_ValidateAllMemory
VFUNC(void,Tcl_ValidateAllMemory,V_Tcl_ValidateAllMemory,_ANSI_ARGS_((CONST char * file,
				int line)))
#endif /* #ifndef Tcl_ValidateAllMemory */

#ifndef Tcl_WaitForEvent
VFUNC(int,Tcl_WaitForEvent,V_Tcl_WaitForEvent,_ANSI_ARGS_((Tcl_Time * timePtr)))
#endif /* #ifndef Tcl_WaitForEvent */

#ifndef TclpAsyncMark
VFUNC(void,TclpAsyncMark,V_TclpAsyncMark,_ANSI_ARGS_((Tcl_AsyncHandler async)))
#endif /* #ifndef TclpAsyncMark */

#ifndef TclpExit
VFUNC(void,TclpExit,V_TclpExit,_ANSI_ARGS_((int status)))
#endif /* #ifndef TclpExit */

#ifndef TkInitTimer
VFUNC(void *,TkInitTimer,V_TkInitTimer,_ANSI_ARGS_((void)))
#endif /* #ifndef TkInitTimer */

#endif /* _TKEVENT */
