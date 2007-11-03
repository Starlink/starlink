#ifndef _TKINT_VM
#define _TKINT_VM
#include "tkInt_f.h"
#ifndef NO_VTABLES
#define tkBitmapObjType (*TkintVptr->V_tkBitmapObjType)
#define tkBorderObjType (*TkintVptr->V_tkBorderObjType)
#define tkColorObjType (*TkintVptr->V_tkColorObjType)
#define tkCursorObjType (*TkintVptr->V_tkCursorObjType)
#define tkFontObjType (*TkintVptr->V_tkFontObjType)
#define tkOptionObjType (*TkintVptr->V_tkOptionObjType)
#define tkStateKeyObjType (*TkintVptr->V_tkStateKeyObjType)
#ifndef TkCanvPostscriptCmd
#  define TkCanvPostscriptCmd (*TkintVptr->V_TkCanvPostscriptCmd)
#endif

#ifndef TkCreateMenuCmd
#  define TkCreateMenuCmd (*TkintVptr->V_TkCreateMenuCmd)
#endif

#ifndef TkEventInit
#  define TkEventInit (*TkintVptr->V_TkEventInit)
#endif

#ifndef TkGetDoublePixels
#  define TkGetDoublePixels (*TkintVptr->V_TkGetDoublePixels)
#endif

#ifndef TkOffsetParseProc
#  define TkOffsetParseProc (*TkintVptr->V_TkOffsetParseProc)
#endif

#ifndef TkOffsetPrintProc
#  define TkOffsetPrintProc (*TkintVptr->V_TkOffsetPrintProc)
#endif

#ifndef TkOrientParseProc
#  define TkOrientParseProc (*TkintVptr->V_TkOrientParseProc)
#endif

#ifndef TkOrientPrintProc
#  define TkOrientPrintProc (*TkintVptr->V_TkOrientPrintProc)
#endif

#ifndef TkPixelParseProc
#  define TkPixelParseProc (*TkintVptr->V_TkPixelParseProc)
#endif

#ifndef TkPixelPrintProc
#  define TkPixelPrintProc (*TkintVptr->V_TkPixelPrintProc)
#endif

#ifndef TkPostscriptImage
#  define TkPostscriptImage (*TkintVptr->V_TkPostscriptImage)
#endif

#ifndef TkRegisterObjTypes
#  define TkRegisterObjTypes (*TkintVptr->V_TkRegisterObjTypes)
#endif

#ifndef TkTileParseProc
#  define TkTileParseProc (*TkintVptr->V_TkTileParseProc)
#endif

#ifndef TkTilePrintProc
#  define TkTilePrintProc (*TkintVptr->V_TkTilePrintProc)
#endif

#ifndef Tk_BindObjCmd
#  define Tk_BindObjCmd (*TkintVptr->V_Tk_BindObjCmd)
#endif

#ifndef Tk_BindtagsObjCmd
#  define Tk_BindtagsObjCmd (*TkintVptr->V_Tk_BindtagsObjCmd)
#endif

#ifndef Tk_ClipboardObjCmd
#  define Tk_ClipboardObjCmd (*TkintVptr->V_Tk_ClipboardObjCmd)
#endif

#ifndef Tk_GrabObjCmd
#  define Tk_GrabObjCmd (*TkintVptr->V_Tk_GrabObjCmd)
#endif

#ifndef Tk_GridObjCmd
#  define Tk_GridObjCmd (*TkintVptr->V_Tk_GridObjCmd)
#endif

#ifndef Tk_LabelframeObjCmd
#  define Tk_LabelframeObjCmd (*TkintVptr->V_Tk_LabelframeObjCmd)
#endif

#ifndef Tk_PackObjCmd
#  define Tk_PackObjCmd (*TkintVptr->V_Tk_PackObjCmd)
#endif

#ifndef Tk_PanedWindowObjCmd
#  define Tk_PanedWindowObjCmd (*TkintVptr->V_Tk_PanedWindowObjCmd)
#endif

#ifndef Tk_PlaceObjCmd
#  define Tk_PlaceObjCmd (*TkintVptr->V_Tk_PlaceObjCmd)
#endif

#ifndef Tk_SelectionObjCmd
#  define Tk_SelectionObjCmd (*TkintVptr->V_Tk_SelectionObjCmd)
#endif

#ifndef Tk_StateParseProc
#  define Tk_StateParseProc (*TkintVptr->V_Tk_StateParseProc)
#endif

#ifndef Tk_StatePrintProc
#  define Tk_StatePrintProc (*TkintVptr->V_Tk_StatePrintProc)
#endif

#ifndef Tk_TkwaitObjCmd
#  define Tk_TkwaitObjCmd (*TkintVptr->V_Tk_TkwaitObjCmd)
#endif

#ifndef Tk_WmObjCmd
#  define Tk_WmObjCmd (*TkintVptr->V_Tk_WmObjCmd)
#endif

#endif /* NO_VTABLES */
#endif /* _TKINT_VM */
