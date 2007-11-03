#ifdef _TKINTDECLS
#ifndef TkAllocWindow
VFUNC(TkWindow *,TkAllocWindow,V_TkAllocWindow,_ANSI_ARGS_((TkDisplay * dispPtr,
				int screenNum, TkWindow * parentPtr)))
#endif /* #ifndef TkAllocWindow */

#ifndef TkBindDeadWindow
VFUNC(void,TkBindDeadWindow,V_TkBindDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkBindDeadWindow */

#ifndef TkBindEventProc
VFUNC(void,TkBindEventProc,V_TkBindEventProc,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * eventPtr)))
#endif /* #ifndef TkBindEventProc */

#ifndef TkBindFree
VFUNC(void,TkBindFree,V_TkBindFree,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkBindFree */

#ifndef TkBindInit
VFUNC(void,TkBindInit,V_TkBindInit,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkBindInit */

#ifndef TkChangeEventWindow
VFUNC(void,TkChangeEventWindow,V_TkChangeEventWindow,_ANSI_ARGS_((XEvent * eventPtr,
				TkWindow * winPtr)))
#endif /* #ifndef TkChangeEventWindow */

#ifndef TkClipBox
#ifdef MAC_OSX_TK
VFUNC(void,TkClipBox,V_TkClipBox,_ANSI_ARGS_((TkRegion rgn,
				XRectangle* rect_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkClipBox,V_TkClipBox,_ANSI_ARGS_((TkRegion rgn,
				XRectangle* rect_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkClipBox,V_TkClipBox,_ANSI_ARGS_((TkRegion rgn,
				XRectangle* rect_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkClipBox */

#ifndef TkClipCleanup
VFUNC(void,TkClipCleanup,V_TkClipCleanup,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #ifndef TkClipCleanup */

#ifndef TkClipInit
VFUNC(int,TkClipInit,V_TkClipInit,_ANSI_ARGS_((Tcl_Interp * interp,
				TkDisplay * dispPtr)))
#endif /* #ifndef TkClipInit */

#ifndef TkComputeAnchor
VFUNC(void,TkComputeAnchor,V_TkComputeAnchor,_ANSI_ARGS_((Tk_Anchor anchor,
				Tk_Window tkwin, int padX, int padY,
				int innerWidth, int innerHeight, int * xPtr,
				int * yPtr)))
#endif /* #ifndef TkComputeAnchor */

#ifndef TkCopyAndGlobalEval
VFUNC(int,TkCopyAndGlobalEval,V_TkCopyAndGlobalEval,_ANSI_ARGS_((Tcl_Interp * interp,
				char * script)))
#endif /* #ifndef TkCopyAndGlobalEval */

#ifndef TkCreateBindingProcedure
VFUNC(unsigned long,TkCreateBindingProcedure,V_TkCreateBindingProcedure,_ANSI_ARGS_((
				Tcl_Interp * interp,
				Tk_BindingTable bindingTable,
				ClientData object, CONST char * eventString,
				TkBindEvalProc * evalProc,
				TkBindFreeProc * freeProc,
				ClientData clientData)))
#endif /* #ifndef TkCreateBindingProcedure */

#ifndef TkCreateCursorFromData
VFUNC(TkCursor *,TkCreateCursorFromData,V_TkCreateCursorFromData,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * source, CONST char * mask,
				int width, int height, int xHot, int yHot,
				XColor fg, XColor bg)))
#endif /* #ifndef TkCreateCursorFromData */

#ifndef TkCreateFrame
VFUNC(int,TkCreateFrame,V_TkCreateFrame,_ANSI_ARGS_((ClientData clientData,
				Tcl_Interp * interp, int argc, Tcl_Obj *CONST *objv,
				int toplevel, char * appName)))
#endif /* #ifndef TkCreateFrame */

#ifndef TkCreateMainWindow
VFUNC(Tk_Window,TkCreateMainWindow,V_TkCreateMainWindow,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * screenName, char * baseName)))
#endif /* #ifndef TkCreateMainWindow */

#ifndef TkCreateRegion
#ifdef MAC_OSX_TK
VFUNC(TkRegion,TkCreateRegion,V_TkCreateRegion,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(TkRegion,TkCreateRegion,V_TkCreateRegion,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(TkRegion,TkCreateRegion,V_TkCreateRegion,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkCreateRegion */

#ifndef TkCurrentTime
VFUNC(Time,TkCurrentTime,V_TkCurrentTime,_ANSI_ARGS_((TkDisplay * dispPtr,
				int fallbackCurrent)))
#endif /* #ifndef TkCurrentTime */

#ifndef TkDebugBitmap
VFUNC(Tcl_Obj *,TkDebugBitmap,V_TkDebugBitmap,_ANSI_ARGS_((Tk_Window tkwin,
				char * name)))
#endif /* #ifndef TkDebugBitmap */

#ifndef TkDebugBorder
VFUNC(Tcl_Obj *,TkDebugBorder,V_TkDebugBorder,_ANSI_ARGS_((Tk_Window tkwin,
				char * name)))
#endif /* #ifndef TkDebugBorder */

#ifndef TkDebugColor
VFUNC(Tcl_Obj *,TkDebugColor,V_TkDebugColor,_ANSI_ARGS_((Tk_Window tkwin,
				char * name)))
#endif /* #ifndef TkDebugColor */

#ifndef TkDebugConfig
VFUNC(Tcl_Obj *,TkDebugConfig,V_TkDebugConfig,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_OptionTable table)))
#endif /* #ifndef TkDebugConfig */

#ifndef TkDebugCursor
VFUNC(Tcl_Obj *,TkDebugCursor,V_TkDebugCursor,_ANSI_ARGS_((Tk_Window tkwin,
				char * name)))
#endif /* #ifndef TkDebugCursor */

#ifndef TkDebugFont
VFUNC(Tcl_Obj *,TkDebugFont,V_TkDebugFont,_ANSI_ARGS_((Tk_Window tkwin,
				char * name)))
#endif /* #ifndef TkDebugFont */

#ifndef TkDeleteAllImages
VFUNC(void,TkDeleteAllImages,V_TkDeleteAllImages,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkDeleteAllImages */

#ifndef TkDestroyRegion
#ifdef MAC_OSX_TK
VFUNC(void,TkDestroyRegion,V_TkDestroyRegion,_ANSI_ARGS_((TkRegion rgn)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkDestroyRegion,V_TkDestroyRegion,_ANSI_ARGS_((TkRegion rgn)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkDestroyRegion,V_TkDestroyRegion,_ANSI_ARGS_((TkRegion rgn)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkDestroyRegion */

#ifndef TkDoConfigureNotify
VFUNC(void,TkDoConfigureNotify,V_TkDoConfigureNotify,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkDoConfigureNotify */

#ifndef TkDrawInsetFocusHighlight
VFUNC(void,TkDrawInsetFocusHighlight,V_TkDrawInsetFocusHighlight,_ANSI_ARGS_((
				Tk_Window tkwin, GC gc, int width,
				Drawable drawable, int padding)))
#endif /* #ifndef TkDrawInsetFocusHighlight */

#ifndef TkEventDeadWindow
VFUNC(void,TkEventDeadWindow,V_TkEventDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkEventDeadWindow */

#ifndef TkFindStateNum
VFUNC(int,TkFindStateNum,V_TkFindStateNum,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * option,
				CONST TkStateMap * mapPtr,
				CONST char * strKey)))
#endif /* #ifndef TkFindStateNum */

#ifndef TkFindStateNumObj
VFUNC(int,TkFindStateNumObj,V_TkFindStateNumObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * optionPtr,
				CONST TkStateMap * mapPtr, Tcl_Obj * keyPtr)))
#endif /* #ifndef TkFindStateNumObj */

#ifndef TkFindStateString
VFUNC(char *,TkFindStateString,V_TkFindStateString,_ANSI_ARGS_((
				CONST TkStateMap * mapPtr, int numKey)))
#endif /* #ifndef TkFindStateString */

#ifndef TkFocusDeadWindow
VFUNC(void,TkFocusDeadWindow,V_TkFocusDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkFocusDeadWindow */

#ifndef TkFocusFilterEvent
VFUNC(int,TkFocusFilterEvent,V_TkFocusFilterEvent,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * eventPtr)))
#endif /* #ifndef TkFocusFilterEvent */

#ifndef TkFocusFree
VFUNC(void,TkFocusFree,V_TkFocusFree,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkFocusFree */

#ifndef TkFocusKeyEvent
VFUNC(TkWindow *,TkFocusKeyEvent,V_TkFocusKeyEvent,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * eventPtr)))
#endif /* #ifndef TkFocusKeyEvent */

#ifndef TkFontPkgFree
VFUNC(void,TkFontPkgFree,V_TkFontPkgFree,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkFontPkgFree */

#ifndef TkFontPkgInit
VFUNC(void,TkFontPkgInit,V_TkFontPkgInit,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkFontPkgInit */

#ifndef TkFreeBindingTags
VFUNC(void,TkFreeBindingTags,V_TkFreeBindingTags,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkFreeBindingTags */

#ifndef TkGCCleanup
VFUNC(void,TkGCCleanup,V_TkGCCleanup,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #ifndef TkGCCleanup */

#ifndef TkGetBitmapData
VFUNC(char *,TkGetBitmapData,V_TkGetBitmapData,_ANSI_ARGS_((Tcl_Interp * interp,
				char * string, char * fileName,
				int * widthPtr, int * heightPtr,
				int * hotXPtr, int * hotYPtr)))
#endif /* #ifndef TkGetBitmapData */

#ifndef TkGetBitmapPredefTable
VFUNC(Tcl_HashTable *,TkGetBitmapPredefTable,V_TkGetBitmapPredefTable,_ANSI_ARGS_((void)))
#endif /* #ifndef TkGetBitmapPredefTable */

#ifndef TkGetCursorByName
VFUNC(TkCursor *,TkGetCursorByName,V_TkGetCursorByName,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_Uid string)))
#endif /* #ifndef TkGetCursorByName */

#ifndef TkGetDefaultScreenName
VFUNC(CONST84_RETURN char *,TkGetDefaultScreenName,V_TkGetDefaultScreenName,_ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * screenName)))
#endif /* #ifndef TkGetDefaultScreenName */

#ifndef TkGetDisplay
VFUNC(TkDisplay *,TkGetDisplay,V_TkGetDisplay,_ANSI_ARGS_((Display * display)))
#endif /* #ifndef TkGetDisplay */

#ifndef TkGetDisplayList
VFUNC(TkDisplay *,TkGetDisplayList,V_TkGetDisplayList,_ANSI_ARGS_((void)))
#endif /* #ifndef TkGetDisplayList */

#ifndef TkGetDisplayOf
VFUNC(int,TkGetDisplayOf,V_TkGetDisplayOf,_ANSI_ARGS_((Tcl_Interp * interp,
				int objc, Tcl_Obj *CONST objv[],
				Tk_Window * tkwinPtr)))
#endif /* #ifndef TkGetDisplayOf */

#ifndef TkGetFocusWin
VFUNC(TkWindow *,TkGetFocusWin,V_TkGetFocusWin,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkGetFocusWin */

#ifndef TkGetInterpNames
VFUNC(int,TkGetInterpNames,V_TkGetInterpNames,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin)))
#endif /* #ifndef TkGetInterpNames */

#ifndef TkGetMainInfoList
VFUNC(TkMainInfo *,TkGetMainInfoList,V_TkGetMainInfoList,_ANSI_ARGS_((void)))
#endif /* #ifndef TkGetMainInfoList */

#ifndef TkGetOptionSpec
VFUNC(CONST Tk_OptionSpec *,TkGetOptionSpec,V_TkGetOptionSpec,_ANSI_ARGS_((CONST char * name,
				Tk_OptionTable optionTable)))
#endif /* #ifndef TkGetOptionSpec */

#ifndef TkGetPointerCoords
VFUNC(void,TkGetPointerCoords,V_TkGetPointerCoords,_ANSI_ARGS_((Tk_Window tkwin,
				int * xPtr, int * yPtr)))
#endif /* #ifndef TkGetPointerCoords */

#ifndef TkGetServerInfo
VFUNC(void,TkGetServerInfo,V_TkGetServerInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin)))
#endif /* #ifndef TkGetServerInfo */

#ifndef TkGetWindowFromObj
VFUNC(int,TkGetWindowFromObj,V_TkGetWindowFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj * objPtr,
				Tk_Window * windowPtr)))
#endif /* #ifndef TkGetWindowFromObj */

#ifndef TkGrabDeadWindow
VFUNC(void,TkGrabDeadWindow,V_TkGrabDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkGrabDeadWindow */

#ifndef TkGrabState
VFUNC(int,TkGrabState,V_TkGrabState,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkGrabState */

#ifndef TkInOutEvents
VFUNC(void,TkInOutEvents,V_TkInOutEvents,_ANSI_ARGS_((XEvent * eventPtr,
				TkWindow * sourcePtr, TkWindow * destPtr,
				int leaveType, int enterType,
				Tcl_QueuePosition position)))
#endif /* #ifndef TkInOutEvents */

#ifndef TkInstallFrameMenu
VFUNC(void,TkInstallFrameMenu,V_TkInstallFrameMenu,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef TkInstallFrameMenu */

#ifndef TkIntersectRegion
#ifdef MAC_OSX_TK
VFUNC(void,TkIntersectRegion,V_TkIntersectRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkIntersectRegion,V_TkIntersectRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkIntersectRegion,V_TkIntersectRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkIntersectRegion */

#ifndef TkKeysymToString
VFUNC(char *,TkKeysymToString,V_TkKeysymToString,_ANSI_ARGS_((KeySym keysym)))
#endif /* #ifndef TkKeysymToString */

#ifndef TkPhotoGetValidRegion
VFUNC(TkRegion,TkPhotoGetValidRegion,V_TkPhotoGetValidRegion,_ANSI_ARGS_((
				Tk_PhotoHandle handle)))
#endif /* #ifndef TkPhotoGetValidRegion */

#ifndef TkPointerEvent
VFUNC(int,TkPointerEvent,V_TkPointerEvent,_ANSI_ARGS_((XEvent * eventPtr,
				TkWindow * winPtr)))
#endif /* #ifndef TkPointerEvent */

#ifndef TkPositionInTree
VFUNC(int,TkPositionInTree,V_TkPositionInTree,_ANSI_ARGS_((TkWindow * winPtr,
				TkWindow * treePtr)))
#endif /* #ifndef TkPositionInTree */

#ifndef TkQueueEventForAllChildren
VFUNC(void,TkQueueEventForAllChildren,V_TkQueueEventForAllChildren,_ANSI_ARGS_((
				TkWindow * winPtr, XEvent * eventPtr)))
#endif /* #ifndef TkQueueEventForAllChildren */

#ifndef TkReadBitmapFile
VFUNC(int,TkReadBitmapFile,V_TkReadBitmapFile,_ANSI_ARGS_((Tcl_Interp * interp,
				Display* display,
				Drawable d, CONST char* filename,
				unsigned int* width_return,
				unsigned int* height_return,
				Pixmap* bitmap_return, int* x_hot_return,
				int* y_hot_return)))
#endif /* #ifndef TkReadBitmapFile */

#ifndef TkRectInRegion
#ifdef MAC_OSX_TK
VFUNC(int,TkRectInRegion,V_TkRectInRegion,_ANSI_ARGS_((TkRegion rgn, int x,
				int y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,TkRectInRegion,V_TkRectInRegion,_ANSI_ARGS_((TkRegion rgn, int x,
				int y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(int,TkRectInRegion,V_TkRectInRegion,_ANSI_ARGS_((TkRegion rgn, int x,
				int y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkRectInRegion */

#ifndef TkScrollWindow
VFUNC(int,TkScrollWindow,V_TkScrollWindow,_ANSI_ARGS_((Tk_Window tkwin, GC gc,
				int x, int y, int width, int height, int dx,
				int dy, TkRegion damageRgn)))
#endif /* #ifndef TkScrollWindow */

#ifndef TkSelDeadWindow
VFUNC(void,TkSelDeadWindow,V_TkSelDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkSelDeadWindow */

#ifndef TkSelEventProc
VFUNC(void,TkSelEventProc,V_TkSelEventProc,_ANSI_ARGS_((Tk_Window tkwin,
				XEvent * eventPtr)))
#endif /* #ifndef TkSelEventProc */

#ifndef TkSelInit
VFUNC(void,TkSelInit,V_TkSelInit,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef TkSelInit */

#ifndef TkSelPropProc
VFUNC(void,TkSelPropProc,V_TkSelPropProc,_ANSI_ARGS_((XEvent * eventPtr)))
#endif /* #ifndef TkSelPropProc */

#ifndef TkSetFocusWin
VFUNC(void,TkSetFocusWin,V_TkSetFocusWin,_ANSI_ARGS_((TkWindow * winPtr,
				int force)))
#endif /* #ifndef TkSetFocusWin */

#ifndef TkSetRegion
#ifdef MAC_OSX_TK
VFUNC(void,TkSetRegion,V_TkSetRegion,_ANSI_ARGS_((Display* display, GC gc,
				TkRegion rgn)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkSetRegion,V_TkSetRegion,_ANSI_ARGS_((Display* display, GC gc,
				TkRegion rgn)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkSetRegion,V_TkSetRegion,_ANSI_ARGS_((Display* display, GC gc,
				TkRegion rgn)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkSetRegion */

#ifndef TkSetWindowMenuBar
VFUNC(void,TkSetWindowMenuBar,V_TkSetWindowMenuBar,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj *oldMenuName,
				Tcl_Obj *menuName)))
#endif /* #ifndef TkSetWindowMenuBar */

#ifndef TkStringToKeysym
VFUNC(KeySym,TkStringToKeysym,V_TkStringToKeysym,_ANSI_ARGS_((char * name)))
#endif /* #ifndef TkStringToKeysym */

#ifndef TkStylePkgFree
VFUNC(void,TkStylePkgFree,V_TkStylePkgFree,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkStylePkgFree */

#ifndef TkStylePkgInit
VFUNC(void,TkStylePkgInit,V_TkStylePkgInit,_ANSI_ARGS_((TkMainInfo * mainPtr)))
#endif /* #ifndef TkStylePkgInit */

#ifndef TkSubtractRegion
#ifdef MAC_OSX_TK
VFUNC(void,TkSubtractRegion,V_TkSubtractRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkSubtractRegion,V_TkSubtractRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkSubtractRegion,V_TkSubtractRegion,_ANSI_ARGS_((TkRegion sra,
				TkRegion srcb, TkRegion dr_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkSubtractRegion */

#ifndef TkToplevelWindowForCommand
VFUNC(Tk_Window,TkToplevelWindowForCommand,V_TkToplevelWindowForCommand,_ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * cmdName)))
#endif /* #ifndef TkToplevelWindowForCommand */

#ifndef TkUnionRectWithRegion
#ifdef MAC_OSX_TK
VFUNC(void,TkUnionRectWithRegion,V_TkUnionRectWithRegion,_ANSI_ARGS_((XRectangle* rect,
				TkRegion src, TkRegion dr_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkUnionRectWithRegion,V_TkUnionRectWithRegion,_ANSI_ARGS_((XRectangle* rect,
				TkRegion src, TkRegion dr_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkUnionRectWithRegion,V_TkUnionRectWithRegion,_ANSI_ARGS_((XRectangle* rect,
				TkRegion src, TkRegion dr_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkUnionRectWithRegion */

#ifndef TkWmAddToColormapWindows
VFUNC(void,TkWmAddToColormapWindows,V_TkWmAddToColormapWindows,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifndef TkWmAddToColormapWindows */

#ifndef TkWmDeadWindow
VFUNC(void,TkWmDeadWindow,V_TkWmDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmDeadWindow */

#ifndef TkWmFocusToplevel
VFUNC(TkWindow *,TkWmFocusToplevel,V_TkWmFocusToplevel,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmFocusToplevel */

#ifndef TkWmMapWindow
VFUNC(void,TkWmMapWindow,V_TkWmMapWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmMapWindow */

#ifndef TkWmNewWindow
VFUNC(void,TkWmNewWindow,V_TkWmNewWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmNewWindow */

#ifndef TkWmProtocolEventProc
VFUNC(void,TkWmProtocolEventProc,V_TkWmProtocolEventProc,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * evenvPtr)))
#endif /* #ifndef TkWmProtocolEventProc */

#ifndef TkWmRemoveFromColormapWindows
VFUNC(void,TkWmRemoveFromColormapWindows,V_TkWmRemoveFromColormapWindows,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifndef TkWmRemoveFromColormapWindows */

#ifndef TkWmRestackToplevel
VFUNC(void,TkWmRestackToplevel,V_TkWmRestackToplevel,_ANSI_ARGS_((TkWindow * winPtr,
				int aboveBelow, TkWindow * otherPtr)))
#endif /* #ifndef TkWmRestackToplevel */

#ifndef TkWmSetClass
VFUNC(void,TkWmSetClass,V_TkWmSetClass,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmSetClass */

#ifndef TkWmStackorderToplevel
VFUNC(TkWindow **,TkWmStackorderToplevel,V_TkWmStackorderToplevel,_ANSI_ARGS_((
				TkWindow * parentPtr)))
#endif /* #ifndef TkWmStackorderToplevel */

#ifndef TkWmUnmapWindow
VFUNC(void,TkWmUnmapWindow,V_TkWmUnmapWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkWmUnmapWindow */

#ifndef TkpChangeFocus
VFUNC(int,TkpChangeFocus,V_TkpChangeFocus,_ANSI_ARGS_((TkWindow * winPtr,
				int force)))
#endif /* #ifndef TkpChangeFocus */

#ifndef TkpClaimFocus
VFUNC(void,TkpClaimFocus,V_TkpClaimFocus,_ANSI_ARGS_((TkWindow * topLevelPtr,
				int force)))
#endif /* #ifndef TkpClaimFocus */

#ifndef TkpCloseDisplay
VFUNC(void,TkpCloseDisplay,V_TkpCloseDisplay,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #ifndef TkpCloseDisplay */

#ifndef TkpDisplayWarning
VFUNC(void,TkpDisplayWarning,V_TkpDisplayWarning,_ANSI_ARGS_((CONST char * msg,
				CONST char * title)))
#endif /* #ifndef TkpDisplayWarning */

#ifndef TkpDrawHighlightBorder
VFUNC(void,TkpDrawHighlightBorder,V_TkpDrawHighlightBorder,_ANSI_ARGS_((Tk_Window tkwin,
				GC fgGC, GC bgGC, int highlightWidth,
				Drawable drawable)))
#endif /* #ifndef TkpDrawHighlightBorder */

#ifndef TkpFreeCursor
VFUNC(void,TkpFreeCursor,V_TkpFreeCursor,_ANSI_ARGS_((TkCursor * cursorPtr)))
#endif /* #ifndef TkpFreeCursor */

#ifndef TkpGetKeySym
VFUNC(KeySym,TkpGetKeySym,V_TkpGetKeySym,_ANSI_ARGS_((TkDisplay * dispPtr,
				XEvent * eventPtr)))
#endif /* #ifndef TkpGetKeySym */

#ifndef TkpGetOtherWindow
VFUNC(TkWindow *,TkpGetOtherWindow,V_TkpGetOtherWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkpGetOtherWindow */

#ifndef TkpGetString
VFUNC(char *,TkpGetString,V_TkpGetString,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * eventPtr, Tcl_DString * dsPtr)))
#endif /* #ifndef TkpGetString */

#ifndef TkpGetSubFonts
VFUNC(void,TkpGetSubFonts,V_TkpGetSubFonts,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Font tkfont)))
#endif /* #ifndef TkpGetSubFonts */

#ifndef TkpGetSystemDefault
VFUNC(Tcl_Obj *,TkpGetSystemDefault,V_TkpGetSystemDefault,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * dbName, CONST char * className)))
#endif /* #ifndef TkpGetSystemDefault */

#ifndef TkpGetWrapperWindow
VFUNC(TkWindow *,TkpGetWrapperWindow,V_TkpGetWrapperWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifndef TkpGetWrapperWindow */

#ifndef TkpInitKeymapInfo
VFUNC(void,TkpInitKeymapInfo,V_TkpInitKeymapInfo,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #ifndef TkpInitKeymapInfo */

#ifndef TkpInitializeMenuBindings
VFUNC(void,TkpInitializeMenuBindings,V_TkpInitializeMenuBindings,_ANSI_ARGS_((
				Tcl_Interp * interp,
				Tk_BindingTable bindingTable)))
#endif /* #ifndef TkpInitializeMenuBindings */

#ifndef TkpMakeContainer
VFUNC(void,TkpMakeContainer,V_TkpMakeContainer,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef TkpMakeContainer */

#ifndef TkpMakeMenuWindow
VFUNC(void,TkpMakeMenuWindow,V_TkpMakeMenuWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int transient)))
#endif /* #ifndef TkpMakeMenuWindow */

#ifndef TkpMakeWindow
VFUNC(Window,TkpMakeWindow,V_TkpMakeWindow,_ANSI_ARGS_((TkWindow * winPtr,
				Window parent)))
#endif /* #ifndef TkpMakeWindow */

#ifndef TkpMenuNotifyToplevelCreate
VFUNC(void,TkpMenuNotifyToplevelCreate,V_TkpMenuNotifyToplevelCreate,_ANSI_ARGS_((
				Tcl_Interp * interp1, char * menuName)))
#endif /* #ifndef TkpMenuNotifyToplevelCreate */

#ifndef TkpMenuThreadInit
VFUNC(void,TkpMenuThreadInit,V_TkpMenuThreadInit,_ANSI_ARGS_((void)))
#endif /* #ifndef TkpMenuThreadInit */

#ifndef TkpOpenDisplay
VFUNC(TkDisplay *,TkpOpenDisplay,V_TkpOpenDisplay,_ANSI_ARGS_((
				CONST char * display_name)))
#endif /* #ifndef TkpOpenDisplay */

#ifndef TkpRedirectKeyEvent
VFUNC(void,TkpRedirectKeyEvent,V_TkpRedirectKeyEvent,_ANSI_ARGS_((TkWindow * winPtr,
				XEvent * eventPtr)))
#endif /* #ifndef TkpRedirectKeyEvent */

#ifndef TkpSetKeycodeAndState
VFUNC(void,TkpSetKeycodeAndState,V_TkpSetKeycodeAndState,_ANSI_ARGS_((Tk_Window tkwin,
				KeySym keySym, XEvent * eventPtr)))
#endif /* #ifndef TkpSetKeycodeAndState */

#ifndef TkpSetMainMenubar
VFUNC(void,TkpSetMainMenubar,V_TkpSetMainMenubar,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, char * menuName)))
#endif /* #ifndef TkpSetMainMenubar */

#ifndef TkpUseWindow
VFUNC(int,TkpUseWindow,V_TkpUseWindow,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj *string)))
#endif /* #ifndef TkpUseWindow */

#ifndef TkpWindowWasRecentlyDeleted
VFUNC(int,TkpWindowWasRecentlyDeleted,V_TkpWindowWasRecentlyDeleted,_ANSI_ARGS_((Window win,
				TkDisplay * dispPtr)))
#endif /* #ifndef TkpWindowWasRecentlyDeleted */

#endif /* _TKINTDECLS */
