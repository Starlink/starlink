#ifdef _TK
#ifndef LangCheckDC
#ifndef RC_INVOKED
VFUNC(void,LangCheckDC,V_LangCheckDC,_ANSI_ARGS_((const char *file, int line)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangCheckDC */

#ifndef LangEventCallback
#ifndef RC_INVOKED
VFUNC(int,LangEventCallback,V_LangEventCallback,_ANSI_ARGS_((ClientData, Tcl_Interp *,XEvent *,Tk_Window,KeySym)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangEventCallback */

#ifndef LangFindVar
#ifndef RC_INVOKED
VFUNC(Var,LangFindVar,V_LangFindVar,_ANSI_ARGS_((Tcl_Interp * interp, Tk_Window, CONST char *name)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangFindVar */

#ifndef LangFontObj
#ifndef RC_INVOKED
VFUNC(Tcl_Obj *,LangFontObj,V_LangFontObj,_ANSI_ARGS_((Tcl_Interp *interp, Tk_Font font, char *name)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangFontObj */

#ifndef LangObjectObj
#ifndef RC_INVOKED
VFUNC(Tcl_Obj *,LangObjectObj,V_LangObjectObj,_ANSI_ARGS_((Tcl_Interp *interp, char *)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangObjectObj */

#ifndef LangSelectHook
#ifndef NO_EXTERN
VFUNC(void,LangSelectHook,V_LangSelectHook,_ANSI_ARGS_((CONST char *what, Tk_Window tkwin,
                                                   Atom selection, Atom target, Atom type)))
#endif /* #ifndef NO_EXTERN */
#endif /* #ifndef LangSelectHook */

#ifndef LangWidgetObj
#ifndef RC_INVOKED
VFUNC(Tcl_Obj *,LangWidgetObj,V_LangWidgetObj,_ANSI_ARGS_((Tcl_Interp *interp, Tk_Window)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef LangWidgetObj */

#ifndef Lang_CreateImage
#ifndef RC_INVOKED
VFUNC(Tcl_Command,Lang_CreateImage,V_Lang_CreateImage,_ANSI_ARGS_((Tcl_Interp *interp,
			    char *cmdName, Tcl_ObjCmdProc *proc,
			    ClientData clientData,
			    Tcl_CmdDeleteProc *deleteProc,
			    Tk_ImageType *typePtr)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Lang_CreateImage */

#ifndef Lang_CreateWidget
#ifndef RC_INVOKED
VFUNC(Tcl_Command,Lang_CreateWidget,V_Lang_CreateWidget,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window, Tcl_ObjCmdProc *proc,
			    ClientData clientData,
			    Tcl_CmdDeleteProc *deleteProc)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Lang_CreateWidget */

#ifndef Lang_DeleteWidget
#ifndef RC_INVOKED
VFUNC(void,Lang_DeleteWidget,V_Lang_DeleteWidget,_ANSI_ARGS_((Tcl_Interp *interp, Tcl_Command cmd)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Lang_DeleteWidget */

#ifndef Tk_ChangeScreen
#ifndef RC_INVOKED
VFUNC(void,Tk_ChangeScreen,V_Tk_ChangeScreen,_ANSI_ARGS_((Tcl_Interp *interp,
			    char *dispName, int screenIndex)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_ChangeScreen */

#ifndef Tk_CreateOldImageType
#ifndef RC_INVOKED
VFUNC(void,Tk_CreateOldImageType,V_Tk_CreateOldImageType,_ANSI_ARGS_((
				Tk_ImageType *typePtr)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_CreateOldImageType */

#ifndef Tk_EventInfo
#ifndef RC_INVOKED
VFUNC(char *,Tk_EventInfo,V_Tk_EventInfo,_ANSI_ARGS_((int letter, Tk_Window tkwin, XEvent *eventPtr,
			    KeySym keySym, int *numPtr, int *isNum, int *type,
			    int num_size, char *numStorage)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_EventInfo */

#ifndef Tk_EventWindow
#ifndef RC_INVOKED
VFUNC(Tk_Window,Tk_EventWindow,V_Tk_EventWindow,_ANSI_ARGS_((XEvent *eventPtr)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_EventWindow */

#ifndef Tk_FreeTile
#ifndef RC_INVOKED
VFUNC(void,Tk_FreeTile,V_Tk_FreeTile,_ANSI_ARGS_((Tk_Tile tile)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_FreeTile */

#ifndef Tk_GetTile
#ifndef RC_INVOKED
VFUNC(Tk_Tile,Tk_GetTile,V_Tk_GetTile,_ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
		    CONST char *imageName)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_GetTile */

#ifndef Tk_GetXSelection
#ifndef RC_INVOKED
VFUNC(int,Tk_GetXSelection,V_Tk_GetXSelection,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, Atom selection, Atom target,
			    Tk_GetXSelProc *proc, ClientData clientData)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_GetXSelection */

#ifndef Tk_InitImageArgs
#ifndef RC_INVOKED
VFUNC(void,Tk_InitImageArgs,V_Tk_InitImageArgs,_ANSI_ARGS_((Tcl_Interp *interp, int argc, char ***objv)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_InitImageArgs */

#ifndef Tk_NameOfTile
#ifndef RC_INVOKED
VFUNC(CONST char *,Tk_NameOfTile,V_Tk_NameOfTile,_ANSI_ARGS_((Tk_Tile tile)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_NameOfTile */

#ifndef Tk_PixmapOfTile
#ifndef RC_INVOKED
VFUNC(Pixmap,Tk_PixmapOfTile,V_Tk_PixmapOfTile,_ANSI_ARGS_((Tk_Tile tile)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_PixmapOfTile */

#ifndef Tk_SetTileChangedProc
#ifndef RC_INVOKED
VFUNC(void,Tk_SetTileChangedProc,V_Tk_SetTileChangedProc,_ANSI_ARGS_((Tk_Tile tile,
		    Tk_TileChangedProc * changeProc, ClientData clientData)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_SetTileChangedProc */

#ifndef Tk_SizeOfTile
#ifndef RC_INVOKED
VFUNC(void,Tk_SizeOfTile,V_Tk_SizeOfTile,_ANSI_ARGS_((Tk_Tile tile, int *widthPtr,
		    int *heightPtr)))
#endif /* #ifndef RC_INVOKED */
#endif /* #ifndef Tk_SizeOfTile */

#endif /* _TK */
