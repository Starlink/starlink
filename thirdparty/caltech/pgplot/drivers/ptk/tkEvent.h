#ifndef _TKEVENT
#define _TKEVENT

#define LangCallbackArg(x) LangOldCallbackArg(x,__FILE__,__LINE__)

EXTERN int		LangCallCallback _ANSI_ARGS_((LangCallback *cb, int flags));
EXTERN Tcl_Obj *	LangCallbackObj _ANSI_ARGS_((LangCallback *));
EXTERN int		LangCmpCallback _ANSI_ARGS_((LangCallback *a,Tcl_Obj * b));
EXTERN LangCallback *	LangCopyCallback _ANSI_ARGS_((LangCallback *));
EXTERN void		LangDebug _ANSI_ARGS_((CONST char *fmt,...));
EXTERN void		LangAsyncCheck _ANSI_ARGS_((void));
EXTERN void		LangFreeCallback _ANSI_ARGS_((LangCallback *));
EXTERN LangCallback *	LangMakeCallback _ANSI_ARGS_((Tcl_Obj *));
EXTERN Tcl_Obj *		LangOldCallbackArg _ANSI_ARGS_((LangCallback *,char *,int));
EXTERN void		LangPushCallbackArgs _ANSI_ARGS_((LangCallback **svp));
EXTERN long Lang_OSHandle _ANSI_ARGS_((int fd));
EXTERN void		Tcl_AlertNotifier _ANSI_ARGS_((ClientData clientData));
EXTERN char *		Tcl_Alloc _ANSI_ARGS_((unsigned int size));
EXTERN Tcl_AsyncHandler	 Tcl_AsyncCreate _ANSI_ARGS_((Tcl_AsyncProc * proc,
				ClientData clientData));
EXTERN void		Tcl_AsyncDelete _ANSI_ARGS_((Tcl_AsyncHandler async));
EXTERN int		Tcl_AsyncInvoke _ANSI_ARGS_((Tcl_Interp * interp,
				int code));
EXTERN void		Tcl_AsyncMark _ANSI_ARGS_((Tcl_AsyncHandler async));
EXTERN int		Tcl_AsyncReady _ANSI_ARGS_((void));
EXTERN void		Tcl_CancelIdleCall _ANSI_ARGS_((Tcl_IdleProc *idleProc,
			    ClientData clientData));
EXTERN void		Tcl_CreateEventSource _ANSI_ARGS_((
			    Tcl_EventSetupProc *setupProc,
			    Tcl_EventCheckProc *checkProc,
			    ClientData clientData));
EXTERN void		Tcl_CreateExitHandler _ANSI_ARGS_((Tcl_ExitProc *proc,
			    ClientData clientData));
EXTERN void		Tcl_CreateFileHandler _ANSI_ARGS_((
    			    int fd, int mask, Tcl_FileProc *proc,
			    ClientData clientData));
EXTERN void		Tcl_CreateThreadExitHandler _ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData));
EXTERN Tcl_TimerToken	Tcl_CreateTimerHandler _ANSI_ARGS_((int milliseconds,
			    Tcl_TimerProc *proc, ClientData clientData));
EXTERN char *		Tcl_DbCkalloc _ANSI_ARGS_((unsigned int size,CONST char *file,int line));
EXTERN int		Tcl_DbCkfree _ANSI_ARGS_((char * ptr, CONST char * file,
				int line));
EXTERN char *		Tcl_DbCkrealloc _ANSI_ARGS_((char *ptr,
			    unsigned int size, CONST char *file,int line));
EXTERN void		Tcl_DeleteEventSource _ANSI_ARGS_((
			    Tcl_EventSetupProc *setupProc,
			    Tcl_EventCheckProc *checkProc,
			    ClientData clientData));
EXTERN void		Tcl_DeleteEvents _ANSI_ARGS_((
				Tcl_EventDeleteProc * proc,
				ClientData clientData));
EXTERN void		Tcl_DeleteExitHandler _ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData));
EXTERN void		Tcl_DeleteFileHandler _ANSI_ARGS_((int fd));
EXTERN void		Tcl_DeleteThreadExitHandler _ANSI_ARGS_((
				Tcl_ExitProc * proc, ClientData clientData));
EXTERN void		Tcl_DeleteTimerHandler _ANSI_ARGS_((
			    Tcl_TimerToken token));
EXTERN int		Tcl_DoOneEvent _ANSI_ARGS_((int flags));
EXTERN void		Tcl_DoWhenIdle _ANSI_ARGS_((Tcl_IdleProc *proc,
			    ClientData clientData));
EXTERN int		Tcl_DumpActiveMemory _ANSI_ARGS_((CONST char * fileName));
EXTERN void		Tcl_Exit _ANSI_ARGS_((int status));
EXTERN void		Tcl_Finalize _ANSI_ARGS_((void));
EXTERN void		Tcl_FinalizeNotifier _ANSI_ARGS_((
				ClientData clientData));
EXTERN void		Tcl_FinalizeThread _ANSI_ARGS_((void));
EXTERN void		Tcl_Free _ANSI_ARGS_((char *ptr));
EXTERN Tcl_ThreadId	Tcl_GetCurrentThread _ANSI_ARGS_((void));
EXTERN int		Tcl_GetServiceMode _ANSI_ARGS_((void));
EXTERN VOID *		Tcl_GetThreadData _ANSI_ARGS_((
				Tcl_ThreadDataKey * keyPtr, int size));
EXTERN ClientData	Tcl_InitNotifier _ANSI_ARGS_((void));
EXTERN void		Tcl_Panic _ANSI_ARGS_((CONST char *,...));
EXTERN void		Tcl_QueueEvent _ANSI_ARGS_((Tcl_Event *evPtr,
			    Tcl_QueuePosition position));
EXTERN void		Tcl_QueueProcEvent _ANSI_ARGS_((Tcl_EventProc *proc,
			    Tcl_Event *evPtr,
			    Tcl_QueuePosition position));
EXTERN char *		Tcl_Realloc _ANSI_ARGS_((char *ptr,
			    unsigned int size));
EXTERN int		Tcl_ServiceAll _ANSI_ARGS_((void));
EXTERN int		Tcl_ServiceEvent _ANSI_ARGS_((int flags));
EXTERN void		Tcl_ServiceModeHook _ANSI_ARGS_((int mode));
EXTERN void		Tcl_SetMaxBlockTime _ANSI_ARGS_((Tcl_Time *timePtr));
EXTERN void		Tcl_SetNotifier _ANSI_ARGS_((
				Tcl_NotifierProcs * notifierProcPtr));
EXTERN int		Tcl_SetServiceMode _ANSI_ARGS_((int mode));
EXTERN void		Tcl_SetTimer _ANSI_ARGS_((Tcl_Time * timePtr));
EXTERN void		Tcl_Sleep _ANSI_ARGS_((int ms));
EXTERN void		Tcl_ThreadAlert _ANSI_ARGS_((Tcl_ThreadId threadId));
EXTERN void		Tcl_ThreadQueueEvent _ANSI_ARGS_((
				Tcl_ThreadId threadId, Tcl_Event* evPtr,
				Tcl_QueuePosition position));
EXTERN void		Tcl_ValidateAllMemory _ANSI_ARGS_((CONST char * file,
				int line));
EXTERN int		Tcl_WaitForEvent _ANSI_ARGS_((Tcl_Time * timePtr));
EXTERN void		TclpAsyncMark _ANSI_ARGS_((Tcl_AsyncHandler async));
EXTERN void		Tcl_GetTime _ANSI_ARGS_((Tcl_Time *time));
EXTERN void *		TkInitTimer _ANSI_ARGS_((void));
EXTERN void 		Lang_NoteOwner _ANSI_ARGS_((void *owner,void *packet,
				CONST char *file, int line));
EXTERN char *		Tcl_AttemptDbCkalloc _ANSI_ARGS_((unsigned int size,
				CONST char * file, int line));
EXTERN void             TclpExit _ANSI_ARGS_((int status));

#ifdef TCL_MEM_DEBUG
#define LangNoteOwner(owner,packet) Lang_NoteOwner(owner,packet,__FILE__,__LINE__)
#else
#define LangNoteOwner(owner,packet)
#endif

#ifndef TCL_TSD_INIT
#define TCL_TSD_INIT(keyPtr)	(ThreadSpecificData *)Tcl_GetThreadData((keyPtr), sizeof(ThreadSpecificData))
#endif


#endif /* _TKEVENT */



