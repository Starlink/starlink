#ifndef _TK_VM
#define _TK_VM
#include "tk_f.h"
#ifndef NO_VTABLES
#ifndef LangCheckDC
#  define LangCheckDC (*TkVptr->V_LangCheckDC)
#endif

#ifndef LangEventCallback
#  define LangEventCallback (*TkVptr->V_LangEventCallback)
#endif

#ifndef LangFindVar
#  define LangFindVar (*TkVptr->V_LangFindVar)
#endif

#ifndef LangFontObj
#  define LangFontObj (*TkVptr->V_LangFontObj)
#endif

#ifndef LangObjectObj
#  define LangObjectObj (*TkVptr->V_LangObjectObj)
#endif

#ifndef LangSelectHook
#  define LangSelectHook (*TkVptr->V_LangSelectHook)
#endif

#ifndef LangWidgetObj
#  define LangWidgetObj (*TkVptr->V_LangWidgetObj)
#endif

#ifndef Lang_CreateImage
#  define Lang_CreateImage (*TkVptr->V_Lang_CreateImage)
#endif

#ifndef Lang_CreateWidget
#  define Lang_CreateWidget (*TkVptr->V_Lang_CreateWidget)
#endif

#ifndef Lang_DeleteWidget
#  define Lang_DeleteWidget (*TkVptr->V_Lang_DeleteWidget)
#endif

#ifndef Tk_ChangeScreen
#  define Tk_ChangeScreen (*TkVptr->V_Tk_ChangeScreen)
#endif

#ifndef Tk_CreateOldImageType
#  define Tk_CreateOldImageType (*TkVptr->V_Tk_CreateOldImageType)
#endif

#ifndef Tk_EventInfo
#  define Tk_EventInfo (*TkVptr->V_Tk_EventInfo)
#endif

#ifndef Tk_EventWindow
#  define Tk_EventWindow (*TkVptr->V_Tk_EventWindow)
#endif

#ifndef Tk_FreeTile
#  define Tk_FreeTile (*TkVptr->V_Tk_FreeTile)
#endif

#ifndef Tk_GetTile
#  define Tk_GetTile (*TkVptr->V_Tk_GetTile)
#endif

#ifndef Tk_GetXSelection
#  define Tk_GetXSelection (*TkVptr->V_Tk_GetXSelection)
#endif

#ifndef Tk_InitImageArgs
#  define Tk_InitImageArgs (*TkVptr->V_Tk_InitImageArgs)
#endif

#ifndef Tk_NameOfTile
#  define Tk_NameOfTile (*TkVptr->V_Tk_NameOfTile)
#endif

#ifndef Tk_PixmapOfTile
#  define Tk_PixmapOfTile (*TkVptr->V_Tk_PixmapOfTile)
#endif

#ifndef Tk_SetTileChangedProc
#  define Tk_SetTileChangedProc (*TkVptr->V_Tk_SetTileChangedProc)
#endif

#ifndef Tk_SizeOfTile
#  define Tk_SizeOfTile (*TkVptr->V_Tk_SizeOfTile)
#endif

#endif /* NO_VTABLES */
#endif /* _TK_VM */
