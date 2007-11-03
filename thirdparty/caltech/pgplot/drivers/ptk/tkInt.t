#ifdef _TKINT
VVAR(Tcl_ObjType,tkBitmapObjType,V_tkBitmapObjType)
VVAR(Tcl_ObjType,tkBorderObjType,V_tkBorderObjType)
VVAR(Tcl_ObjType,tkColorObjType,V_tkColorObjType)
VVAR(Tcl_ObjType,tkCursorObjType,V_tkCursorObjType)
VVAR(Tcl_ObjType,tkFontObjType,V_tkFontObjType)
VVAR(Tcl_ObjType,tkOptionObjType,V_tkOptionObjType)
VVAR(Tcl_ObjType,tkStateKeyObjType,V_tkStateKeyObjType)
#ifndef TkCanvPostscriptCmd
VFUNC(int,TkCanvPostscriptCmd,V_TkCanvPostscriptCmd,_ANSI_ARGS_((struct TkCanvas *canvasPtr,
			    Tcl_Interp *interp, int argc, CONST84 Tcl_Obj *CONST *objv)))
#endif /* #ifndef TkCanvPostscriptCmd */

#ifndef TkCreateMenuCmd
VFUNC(int,TkCreateMenuCmd,V_TkCreateMenuCmd,_ANSI_ARGS_((Tcl_Interp *interp)))
#endif /* #ifndef TkCreateMenuCmd */

#ifndef TkEventInit
VFUNC(void,TkEventInit,V_TkEventInit,_ANSI_ARGS_((void)))
#endif /* #ifndef TkEventInit */

#ifndef TkGetDoublePixels
VFUNC(int,TkGetDoublePixels,V_TkGetDoublePixels,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, CONST char *string,
			    double *doublePtr)))
#endif /* #ifndef TkGetDoublePixels */

#ifndef TkOffsetParseProc
VFUNC(int,TkOffsetParseProc,V_TkOffsetParseProc,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, Tcl_Obj *value, char *widgRec,
			    int offset)))
#endif /* #ifndef TkOffsetParseProc */

#ifndef TkOffsetPrintProc
VFUNC(Tcl_Obj *,TkOffsetPrintProc,V_TkOffsetPrintProc,_ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TkOffsetPrintProc */

#ifndef TkOrientParseProc
VFUNC(int,TkOrientParseProc,V_TkOrientParseProc,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, Tcl_Obj *value,
			    char *widgRec, int offset)))
#endif /* #ifndef TkOrientParseProc */

#ifndef TkOrientPrintProc
VFUNC(Tcl_Obj *,TkOrientPrintProc,V_TkOrientPrintProc,_ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TkOrientPrintProc */

#ifndef TkPixelParseProc
VFUNC(int,TkPixelParseProc,V_TkPixelParseProc,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, Tcl_Obj *value, char *widgRec,
			    int offset)))
#endif /* #ifndef TkPixelParseProc */

#ifndef TkPixelPrintProc
VFUNC(Tcl_Obj *,TkPixelPrintProc,V_TkPixelPrintProc,_ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TkPixelPrintProc */

#ifndef TkPostscriptImage
VFUNC(int,TkPostscriptImage,V_TkPostscriptImage,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, Tk_PostscriptInfo psInfo,
			    XImage *ximage, int x, int y, int width,
			    int height)))
#endif /* #ifndef TkPostscriptImage */

#ifndef TkRegisterObjTypes
VFUNC(void,TkRegisterObjTypes,V_TkRegisterObjTypes,_ANSI_ARGS_((void)))
#endif /* #ifndef TkRegisterObjTypes */

#ifndef TkTileParseProc
VFUNC(int,TkTileParseProc,V_TkTileParseProc,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, Tcl_Obj *value, char *widgRec,
			    int offset)))
#endif /* #ifndef TkTileParseProc */

#ifndef TkTilePrintProc
VFUNC(Tcl_Obj *,TkTilePrintProc,V_TkTilePrintProc,_ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TkTilePrintProc */

#ifndef Tk_BindObjCmd
VFUNC(int,Tk_BindObjCmd,V_Tk_BindObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_BindObjCmd */

#ifndef Tk_BindtagsObjCmd
VFUNC(int,Tk_BindtagsObjCmd,V_Tk_BindtagsObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_BindtagsObjCmd */

#ifndef Tk_ClipboardObjCmd
VFUNC(int,Tk_ClipboardObjCmd,V_Tk_ClipboardObjCmd,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    int objc, Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_ClipboardObjCmd */

#ifndef Tk_GrabObjCmd
VFUNC(int,Tk_GrabObjCmd,V_Tk_GrabObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_GrabObjCmd */

#ifndef Tk_GridObjCmd
VFUNC(int,Tk_GridObjCmd,V_Tk_GridObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_GridObjCmd */

#ifndef Tk_LabelframeObjCmd
VFUNC(int,Tk_LabelframeObjCmd,V_Tk_LabelframeObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_LabelframeObjCmd */

#ifndef Tk_PackObjCmd
VFUNC(int,Tk_PackObjCmd,V_Tk_PackObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_PackObjCmd */

#ifndef Tk_PanedWindowObjCmd
VFUNC(int,Tk_PanedWindowObjCmd,V_Tk_PanedWindowObjCmd,_ANSI_ARGS_((
			    ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_PanedWindowObjCmd */

#ifndef Tk_PlaceObjCmd
VFUNC(int,Tk_PlaceObjCmd,V_Tk_PlaceObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_PlaceObjCmd */

#ifndef Tk_SelectionObjCmd
VFUNC(int,Tk_SelectionObjCmd,V_Tk_SelectionObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_SelectionObjCmd */

#ifndef Tk_StateParseProc
VFUNC(int,Tk_StateParseProc,V_Tk_StateParseProc,_ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, Tcl_Obj *value,
			    char *widgRec, int offset)))
#endif /* #ifndef Tk_StateParseProc */

#ifndef Tk_StatePrintProc
VFUNC(Tcl_Obj *,Tk_StatePrintProc,V_Tk_StatePrintProc,_ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef Tk_StatePrintProc */

#ifndef Tk_TkwaitObjCmd
VFUNC(int,Tk_TkwaitObjCmd,V_Tk_TkwaitObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_TkwaitObjCmd */

#ifndef Tk_WmObjCmd
VFUNC(int,Tk_WmObjCmd,V_Tk_WmObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[])))
#endif /* #ifndef Tk_WmObjCmd */

#endif /* _TKINT */
