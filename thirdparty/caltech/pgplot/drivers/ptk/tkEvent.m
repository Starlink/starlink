#ifndef _TKEVENT_VM
#define _TKEVENT_VM
#include "tkEvent_f.h"
#ifndef NO_VTABLES
#ifndef LangAsyncCheck
#  define LangAsyncCheck (*TkeventVptr->V_LangAsyncCheck)
#endif

#ifndef LangCallCallback
#  define LangCallCallback (*TkeventVptr->V_LangCallCallback)
#endif

#ifndef LangCallbackObj
#  define LangCallbackObj (*TkeventVptr->V_LangCallbackObj)
#endif

#ifndef LangCmpCallback
#  define LangCmpCallback (*TkeventVptr->V_LangCmpCallback)
#endif

#ifndef LangCopyCallback
#  define LangCopyCallback (*TkeventVptr->V_LangCopyCallback)
#endif

#ifndef LangDebug
#  define LangDebug (*TkeventVptr->V_LangDebug)
#endif

#ifndef LangFreeCallback
#  define LangFreeCallback (*TkeventVptr->V_LangFreeCallback)
#endif

#ifndef LangMakeCallback
#  define LangMakeCallback (*TkeventVptr->V_LangMakeCallback)
#endif

#ifndef LangOldCallbackArg
#  define LangOldCallbackArg (*TkeventVptr->V_LangOldCallbackArg)
#endif

#ifndef LangPushCallbackArgs
#  define LangPushCallbackArgs (*TkeventVptr->V_LangPushCallbackArgs)
#endif

#ifndef Lang_NoteOwner
#  define Lang_NoteOwner (*TkeventVptr->V_Lang_NoteOwner)
#endif

#ifndef Lang_OSHandle
#  define Lang_OSHandle (*TkeventVptr->V_Lang_OSHandle)
#endif

#ifndef Tcl_AlertNotifier
#  define Tcl_AlertNotifier (*TkeventVptr->V_Tcl_AlertNotifier)
#endif

#ifndef Tcl_Alloc
#  define Tcl_Alloc (*TkeventVptr->V_Tcl_Alloc)
#endif

#ifndef Tcl_AsyncCreate
#  define Tcl_AsyncCreate (*TkeventVptr->V_Tcl_AsyncCreate)
#endif

#ifndef Tcl_AsyncDelete
#  define Tcl_AsyncDelete (*TkeventVptr->V_Tcl_AsyncDelete)
#endif

#ifndef Tcl_AsyncInvoke
#  define Tcl_AsyncInvoke (*TkeventVptr->V_Tcl_AsyncInvoke)
#endif

#ifndef Tcl_AsyncMark
#  define Tcl_AsyncMark (*TkeventVptr->V_Tcl_AsyncMark)
#endif

#ifndef Tcl_AsyncReady
#  define Tcl_AsyncReady (*TkeventVptr->V_Tcl_AsyncReady)
#endif

#ifndef Tcl_AttemptDbCkalloc
#  define Tcl_AttemptDbCkalloc (*TkeventVptr->V_Tcl_AttemptDbCkalloc)
#endif

#ifndef Tcl_CancelIdleCall
#  define Tcl_CancelIdleCall (*TkeventVptr->V_Tcl_CancelIdleCall)
#endif

#ifndef Tcl_CreateEventSource
#  define Tcl_CreateEventSource (*TkeventVptr->V_Tcl_CreateEventSource)
#endif

#ifndef Tcl_CreateExitHandler
#  define Tcl_CreateExitHandler (*TkeventVptr->V_Tcl_CreateExitHandler)
#endif

#ifndef Tcl_CreateFileHandler
#  define Tcl_CreateFileHandler (*TkeventVptr->V_Tcl_CreateFileHandler)
#endif

#ifndef Tcl_CreateThreadExitHandler
#  define Tcl_CreateThreadExitHandler (*TkeventVptr->V_Tcl_CreateThreadExitHandler)
#endif

#ifndef Tcl_CreateTimerHandler
#  define Tcl_CreateTimerHandler (*TkeventVptr->V_Tcl_CreateTimerHandler)
#endif

#ifndef Tcl_DbCkalloc
#  define Tcl_DbCkalloc (*TkeventVptr->V_Tcl_DbCkalloc)
#endif

#ifndef Tcl_DbCkfree
#  define Tcl_DbCkfree (*TkeventVptr->V_Tcl_DbCkfree)
#endif

#ifndef Tcl_DbCkrealloc
#  define Tcl_DbCkrealloc (*TkeventVptr->V_Tcl_DbCkrealloc)
#endif

#ifndef Tcl_DeleteEventSource
#  define Tcl_DeleteEventSource (*TkeventVptr->V_Tcl_DeleteEventSource)
#endif

#ifndef Tcl_DeleteEvents
#  define Tcl_DeleteEvents (*TkeventVptr->V_Tcl_DeleteEvents)
#endif

#ifndef Tcl_DeleteExitHandler
#  define Tcl_DeleteExitHandler (*TkeventVptr->V_Tcl_DeleteExitHandler)
#endif

#ifndef Tcl_DeleteFileHandler
#  define Tcl_DeleteFileHandler (*TkeventVptr->V_Tcl_DeleteFileHandler)
#endif

#ifndef Tcl_DeleteThreadExitHandler
#  define Tcl_DeleteThreadExitHandler (*TkeventVptr->V_Tcl_DeleteThreadExitHandler)
#endif

#ifndef Tcl_DeleteTimerHandler
#  define Tcl_DeleteTimerHandler (*TkeventVptr->V_Tcl_DeleteTimerHandler)
#endif

#ifndef Tcl_DoOneEvent
#  define Tcl_DoOneEvent (*TkeventVptr->V_Tcl_DoOneEvent)
#endif

#ifndef Tcl_DoWhenIdle
#  define Tcl_DoWhenIdle (*TkeventVptr->V_Tcl_DoWhenIdle)
#endif

#ifndef Tcl_DumpActiveMemory
#  define Tcl_DumpActiveMemory (*TkeventVptr->V_Tcl_DumpActiveMemory)
#endif

#ifndef Tcl_Exit
#  define Tcl_Exit (*TkeventVptr->V_Tcl_Exit)
#endif

#ifndef Tcl_Finalize
#  define Tcl_Finalize (*TkeventVptr->V_Tcl_Finalize)
#endif

#ifndef Tcl_FinalizeNotifier
#  define Tcl_FinalizeNotifier (*TkeventVptr->V_Tcl_FinalizeNotifier)
#endif

#ifndef Tcl_FinalizeThread
#  define Tcl_FinalizeThread (*TkeventVptr->V_Tcl_FinalizeThread)
#endif

#ifndef Tcl_Free
#  define Tcl_Free (*TkeventVptr->V_Tcl_Free)
#endif

#ifndef Tcl_GetCurrentThread
#  define Tcl_GetCurrentThread (*TkeventVptr->V_Tcl_GetCurrentThread)
#endif

#ifndef Tcl_GetServiceMode
#  define Tcl_GetServiceMode (*TkeventVptr->V_Tcl_GetServiceMode)
#endif

#ifndef Tcl_GetThreadData
#  define Tcl_GetThreadData (*TkeventVptr->V_Tcl_GetThreadData)
#endif

#ifndef Tcl_GetTime
#  define Tcl_GetTime (*TkeventVptr->V_Tcl_GetTime)
#endif

#ifndef Tcl_InitNotifier
#  define Tcl_InitNotifier (*TkeventVptr->V_Tcl_InitNotifier)
#endif

#ifndef Tcl_Panic
#  define Tcl_Panic (*TkeventVptr->V_Tcl_Panic)
#endif

#ifndef Tcl_QueueEvent
#  define Tcl_QueueEvent (*TkeventVptr->V_Tcl_QueueEvent)
#endif

#ifndef Tcl_QueueProcEvent
#  define Tcl_QueueProcEvent (*TkeventVptr->V_Tcl_QueueProcEvent)
#endif

#ifndef Tcl_Realloc
#  define Tcl_Realloc (*TkeventVptr->V_Tcl_Realloc)
#endif

#ifndef Tcl_ServiceAll
#  define Tcl_ServiceAll (*TkeventVptr->V_Tcl_ServiceAll)
#endif

#ifndef Tcl_ServiceEvent
#  define Tcl_ServiceEvent (*TkeventVptr->V_Tcl_ServiceEvent)
#endif

#ifndef Tcl_ServiceModeHook
#  define Tcl_ServiceModeHook (*TkeventVptr->V_Tcl_ServiceModeHook)
#endif

#ifndef Tcl_SetMaxBlockTime
#  define Tcl_SetMaxBlockTime (*TkeventVptr->V_Tcl_SetMaxBlockTime)
#endif

#ifndef Tcl_SetNotifier
#  define Tcl_SetNotifier (*TkeventVptr->V_Tcl_SetNotifier)
#endif

#ifndef Tcl_SetServiceMode
#  define Tcl_SetServiceMode (*TkeventVptr->V_Tcl_SetServiceMode)
#endif

#ifndef Tcl_SetTimer
#  define Tcl_SetTimer (*TkeventVptr->V_Tcl_SetTimer)
#endif

#ifndef Tcl_Sleep
#  define Tcl_Sleep (*TkeventVptr->V_Tcl_Sleep)
#endif

#ifndef Tcl_ThreadAlert
#  define Tcl_ThreadAlert (*TkeventVptr->V_Tcl_ThreadAlert)
#endif

#ifndef Tcl_ThreadQueueEvent
#  define Tcl_ThreadQueueEvent (*TkeventVptr->V_Tcl_ThreadQueueEvent)
#endif

#ifndef Tcl_ValidateAllMemory
#  define Tcl_ValidateAllMemory (*TkeventVptr->V_Tcl_ValidateAllMemory)
#endif

#ifndef Tcl_WaitForEvent
#  define Tcl_WaitForEvent (*TkeventVptr->V_Tcl_WaitForEvent)
#endif

#ifndef TclpAsyncMark
#  define TclpAsyncMark (*TkeventVptr->V_TclpAsyncMark)
#endif

#ifndef TclpExit
#  define TclpExit (*TkeventVptr->V_TclpExit)
#endif

#ifndef TkInitTimer
#  define TkInitTimer (*TkeventVptr->V_TkInitTimer)
#endif

#endif /* NO_VTABLES */
#endif /* _TKEVENT_VM */
