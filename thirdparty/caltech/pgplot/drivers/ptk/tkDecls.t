#ifdef _TKDECLS
#ifndef Tk_3DBorderColor
VFUNC(XColor *,Tk_3DBorderColor,V_Tk_3DBorderColor,_ANSI_ARGS_((Tk_3DBorder border)))
#endif /* #ifndef Tk_3DBorderColor */

#ifndef Tk_3DBorderGC
VFUNC(GC,Tk_3DBorderGC,V_Tk_3DBorderGC,_ANSI_ARGS_((Tk_Window tkwin,
				Tk_3DBorder border, int which)))
#endif /* #ifndef Tk_3DBorderGC */

#ifndef Tk_3DHorizontalBevel
VFUNC(void,Tk_3DHorizontalBevel,V_Tk_3DHorizontalBevel,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border, int x,
				int y, int width, int height, int leftIn,
				int rightIn, int topBevel, int relief)))
#endif /* #ifndef Tk_3DHorizontalBevel */

#ifndef Tk_3DVerticalBevel
VFUNC(void,Tk_3DVerticalBevel,V_Tk_3DVerticalBevel,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border, int x,
				int y, int width, int height, int leftBevel,
				int relief)))
#endif /* #ifndef Tk_3DVerticalBevel */

#ifndef Tk_Alloc3DBorderFromObj
VFUNC(Tk_3DBorder,Tk_Alloc3DBorderFromObj,V_Tk_Alloc3DBorderFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_Alloc3DBorderFromObj */

#ifndef Tk_AllocBitmapFromObj
VFUNC(Pixmap,Tk_AllocBitmapFromObj,V_Tk_AllocBitmapFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_AllocBitmapFromObj */

#ifndef Tk_AllocColorFromObj
VFUNC(XColor *,Tk_AllocColorFromObj,V_Tk_AllocColorFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_AllocColorFromObj */

#ifndef Tk_AllocCursorFromObj
VFUNC(Tk_Cursor,Tk_AllocCursorFromObj,V_Tk_AllocCursorFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_AllocCursorFromObj */

#ifndef Tk_AllocFontFromObj
VFUNC(Tk_Font,Tk_AllocFontFromObj,V_Tk_AllocFontFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_AllocFontFromObj */

#ifndef Tk_AllocStyleFromObj
VFUNC(Tk_Style,Tk_AllocStyleFromObj,V_Tk_AllocStyleFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_AllocStyleFromObj */

#ifndef Tk_BindEvent
VFUNC(void,Tk_BindEvent,V_Tk_BindEvent,_ANSI_ARGS_((
				Tk_BindingTable bindingTable,
				XEvent * eventPtr, Tk_Window tkwin,
				int numObjects, ClientData * objectPtr)))
#endif /* #ifndef Tk_BindEvent */

#ifndef Tk_ChangeWindowAttributes
VFUNC(void,Tk_ChangeWindowAttributes,V_Tk_ChangeWindowAttributes,_ANSI_ARGS_((
				Tk_Window tkwin, unsigned long valueMask,
				XSetWindowAttributes * attsPtr)))
#endif /* #ifndef Tk_ChangeWindowAttributes */

#ifndef Tk_CharBbox
VFUNC(int,Tk_CharBbox,V_Tk_CharBbox,_ANSI_ARGS_((Tk_TextLayout layout,
				int index, int * xPtr, int * yPtr,
				int * widthPtr, int * heightPtr)))
#endif /* #ifndef Tk_CharBbox */

#ifndef Tk_ClearSelection
VFUNC(void,Tk_ClearSelection,V_Tk_ClearSelection,_ANSI_ARGS_((Tk_Window tkwin,
				Atom selection)))
#endif /* #ifndef Tk_ClearSelection */

#ifndef Tk_ClipboardAppend
VFUNC(int,Tk_ClipboardAppend,V_Tk_ClipboardAppend,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Atom target, Atom format,
				char* buffer)))
#endif /* #ifndef Tk_ClipboardAppend */

#ifndef Tk_ClipboardClear
VFUNC(int,Tk_ClipboardClear,V_Tk_ClipboardClear,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin)))
#endif /* #ifndef Tk_ClipboardClear */

#ifndef Tk_CollapseMotionEvents
VFUNC(int,Tk_CollapseMotionEvents,V_Tk_CollapseMotionEvents,_ANSI_ARGS_((
				Display * display, int collapse)))
#endif /* #ifndef Tk_CollapseMotionEvents */

#ifndef Tk_ComputeTextLayout
VFUNC(Tk_TextLayout,Tk_ComputeTextLayout,V_Tk_ComputeTextLayout,_ANSI_ARGS_((Tk_Font font,
				CONST char * str, int numChars,
				int wrapLength, Tk_Justify justify,
				int flags, int * widthPtr, int * heightPtr)))
#endif /* #ifndef Tk_ComputeTextLayout */

#ifndef Tk_ConfigureInfo
VFUNC(int,Tk_ConfigureInfo,V_Tk_ConfigureInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_ConfigSpec * specs,
				char * widgRec, CONST char * argvName,
				int flags)))
#endif /* #ifndef Tk_ConfigureInfo */

#ifndef Tk_ConfigureValue
VFUNC(int,Tk_ConfigureValue,V_Tk_ConfigureValue,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_ConfigSpec * specs,
				char * widgRec, CONST char * argvName,
				int flags)))
#endif /* #ifndef Tk_ConfigureValue */

#ifndef Tk_ConfigureWidget
VFUNC(int,Tk_ConfigureWidget,V_Tk_ConfigureWidget,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_ConfigSpec * specs,
				int argc, CONST84 Tcl_Obj *CONST *objv,
				char * widgRec, int flags)))
#endif /* #ifndef Tk_ConfigureWidget */

#ifndef Tk_ConfigureWindow
VFUNC(void,Tk_ConfigureWindow,V_Tk_ConfigureWindow,_ANSI_ARGS_((Tk_Window tkwin,
				unsigned int valueMask,
				XWindowChanges * valuePtr)))
#endif /* #ifndef Tk_ConfigureWindow */

#ifndef Tk_CoordsToWindow
VFUNC(Tk_Window,Tk_CoordsToWindow,V_Tk_CoordsToWindow,_ANSI_ARGS_((int rootX, int rootY,
				Tk_Window tkwin)))
#endif /* #ifndef Tk_CoordsToWindow */

#ifndef Tk_CreateAnonymousWindow
VFUNC(Tk_Window,Tk_CreateAnonymousWindow,V_Tk_CreateAnonymousWindow,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window parent,
				CONST char * screenName)))
#endif /* #ifndef Tk_CreateAnonymousWindow */

#ifndef Tk_CreateBinding
VFUNC(unsigned long,Tk_CreateBinding,V_Tk_CreateBinding,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_BindingTable bindingTable,
				ClientData object, CONST char * eventStr,
				Tcl_Obj *command, int append)))
#endif /* #ifndef Tk_CreateBinding */

#ifndef Tk_CreateBindingTable
VFUNC(Tk_BindingTable,Tk_CreateBindingTable,V_Tk_CreateBindingTable,_ANSI_ARGS_((
				Tcl_Interp * interp)))
#endif /* #ifndef Tk_CreateBindingTable */

#ifndef Tk_CreateClientMessageHandler
VFUNC(void,Tk_CreateClientMessageHandler,V_Tk_CreateClientMessageHandler,_ANSI_ARGS_((
				Tk_ClientMessageProc * proc)))
#endif /* #ifndef Tk_CreateClientMessageHandler */

#ifndef Tk_CreateErrorHandler
VFUNC(Tk_ErrorHandler,Tk_CreateErrorHandler,V_Tk_CreateErrorHandler,_ANSI_ARGS_((Display * display,
				int errNum, int request, int minorCode,
				Tk_ErrorProc * errorProc,
				ClientData clientData)))
#endif /* #ifndef Tk_CreateErrorHandler */

#ifndef Tk_CreateEventHandler
VFUNC(void,Tk_CreateEventHandler,V_Tk_CreateEventHandler,_ANSI_ARGS_((Tk_Window token,
				unsigned long mask, Tk_EventProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tk_CreateEventHandler */

#ifndef Tk_CreateGenericHandler
VFUNC(void,Tk_CreateGenericHandler,V_Tk_CreateGenericHandler,_ANSI_ARGS_((
				Tk_GenericProc * proc, ClientData clientData)))
#endif /* #ifndef Tk_CreateGenericHandler */

#ifndef Tk_CreateImageType
VFUNC(void,Tk_CreateImageType,V_Tk_CreateImageType,_ANSI_ARGS_((
				Tk_ImageType * typePtr)))
#endif /* #ifndef Tk_CreateImageType */

#ifndef Tk_CreateOptionTable
VFUNC(Tk_OptionTable,Tk_CreateOptionTable,V_Tk_CreateOptionTable,_ANSI_ARGS_((
				Tcl_Interp * interp,
				CONST Tk_OptionSpec * templatePtr)))
#endif /* #ifndef Tk_CreateOptionTable */

#ifndef Tk_CreateSelHandler
VFUNC(void,Tk_CreateSelHandler,V_Tk_CreateSelHandler,_ANSI_ARGS_((Tk_Window tkwin,
				Atom selection, Atom target,
				Tk_SelectionProc * proc,
				ClientData clientData, Atom format)))
#endif /* #ifndef Tk_CreateSelHandler */

#ifndef Tk_CreateStyle
VFUNC(Tk_Style,Tk_CreateStyle,V_Tk_CreateStyle,_ANSI_ARGS_((CONST char * name,
				Tk_StyleEngine engine, ClientData clientData)))
#endif /* #ifndef Tk_CreateStyle */

#ifndef Tk_CreateWindow
VFUNC(Tk_Window,Tk_CreateWindow,V_Tk_CreateWindow,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window parent, CONST char * name,
				CONST char * screenName)))
#endif /* #ifndef Tk_CreateWindow */

#ifndef Tk_CreateWindowFromPath
VFUNC(Tk_Window,Tk_CreateWindowFromPath,V_Tk_CreateWindowFromPath,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				CONST char * pathName,
				CONST char * screenName)))
#endif /* #ifndef Tk_CreateWindowFromPath */

#ifndef Tk_DefineBitmap
VFUNC(int,Tk_DefineBitmap,V_Tk_DefineBitmap,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name, CONST char * source,
				int width, int height)))
#endif /* #ifndef Tk_DefineBitmap */

#ifndef Tk_DefineCursor
VFUNC(void,Tk_DefineCursor,V_Tk_DefineCursor,_ANSI_ARGS_((Tk_Window window,
				Tk_Cursor cursor)))
#endif /* #ifndef Tk_DefineCursor */

#ifndef Tk_DeleteAllBindings
VFUNC(void,Tk_DeleteAllBindings,V_Tk_DeleteAllBindings,_ANSI_ARGS_((
				Tk_BindingTable bindingTable,
				ClientData object)))
#endif /* #ifndef Tk_DeleteAllBindings */

#ifndef Tk_DeleteBinding
VFUNC(int,Tk_DeleteBinding,V_Tk_DeleteBinding,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_BindingTable bindingTable,
				ClientData object, CONST char * eventStr)))
#endif /* #ifndef Tk_DeleteBinding */

#ifndef Tk_DeleteBindingTable
VFUNC(void,Tk_DeleteBindingTable,V_Tk_DeleteBindingTable,_ANSI_ARGS_((
				Tk_BindingTable bindingTable)))
#endif /* #ifndef Tk_DeleteBindingTable */

#ifndef Tk_DeleteClientMessageHandler
VFUNC(void,Tk_DeleteClientMessageHandler,V_Tk_DeleteClientMessageHandler,_ANSI_ARGS_((
				Tk_ClientMessageProc * proc)))
#endif /* #ifndef Tk_DeleteClientMessageHandler */

#ifndef Tk_DeleteErrorHandler
VFUNC(void,Tk_DeleteErrorHandler,V_Tk_DeleteErrorHandler,_ANSI_ARGS_((
				Tk_ErrorHandler handler)))
#endif /* #ifndef Tk_DeleteErrorHandler */

#ifndef Tk_DeleteEventHandler
VFUNC(void,Tk_DeleteEventHandler,V_Tk_DeleteEventHandler,_ANSI_ARGS_((Tk_Window token,
				unsigned long mask, Tk_EventProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tk_DeleteEventHandler */

#ifndef Tk_DeleteGenericHandler
VFUNC(void,Tk_DeleteGenericHandler,V_Tk_DeleteGenericHandler,_ANSI_ARGS_((
				Tk_GenericProc * proc, ClientData clientData)))
#endif /* #ifndef Tk_DeleteGenericHandler */

#ifndef Tk_DeleteImage
VFUNC(void,Tk_DeleteImage,V_Tk_DeleteImage,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name)))
#endif /* #ifndef Tk_DeleteImage */

#ifndef Tk_DeleteOptionTable
VFUNC(void,Tk_DeleteOptionTable,V_Tk_DeleteOptionTable,_ANSI_ARGS_((
				Tk_OptionTable optionTable)))
#endif /* #ifndef Tk_DeleteOptionTable */

#ifndef Tk_DeleteSelHandler
VFUNC(void,Tk_DeleteSelHandler,V_Tk_DeleteSelHandler,_ANSI_ARGS_((Tk_Window tkwin,
				Atom selection, Atom target)))
#endif /* #ifndef Tk_DeleteSelHandler */

#ifndef Tk_DestroyWindow
VFUNC(void,Tk_DestroyWindow,V_Tk_DestroyWindow,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_DestroyWindow */

#ifndef Tk_DisplayName
VFUNC(CONST84_RETURN char *,Tk_DisplayName,V_Tk_DisplayName,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_DisplayName */

#ifndef Tk_DistanceToTextLayout
VFUNC(int,Tk_DistanceToTextLayout,V_Tk_DistanceToTextLayout,_ANSI_ARGS_((
				Tk_TextLayout layout, int x, int y)))
#endif /* #ifndef Tk_DistanceToTextLayout */

#ifndef Tk_Draw3DPolygon
VFUNC(void,Tk_Draw3DPolygon,V_Tk_Draw3DPolygon,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border,
				XPoint * pointPtr, int numPoints,
				int borderWidth, int leftRelief)))
#endif /* #ifndef Tk_Draw3DPolygon */

#ifndef Tk_Draw3DRectangle
VFUNC(void,Tk_Draw3DRectangle,V_Tk_Draw3DRectangle,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border, int x,
				int y, int width, int height,
				int borderWidth, int relief)))
#endif /* #ifndef Tk_Draw3DRectangle */

#ifndef Tk_DrawChars
VFUNC(void,Tk_DrawChars,V_Tk_DrawChars,_ANSI_ARGS_((Display * display,
				Drawable drawable, GC gc, Tk_Font tkfont,
				CONST char * source, int numBytes, int x,
				int y)))
#endif /* #ifndef Tk_DrawChars */

#ifndef Tk_DrawElement
VFUNC(void,Tk_DrawElement,V_Tk_DrawElement,_ANSI_ARGS_((Tk_Style style,
				Tk_StyledElement element, char * recordPtr,
				Tk_Window tkwin, Drawable d, int x, int y,
				int width, int height, int state)))
#endif /* #ifndef Tk_DrawElement */

#ifndef Tk_DrawFocusHighlight
VFUNC(void,Tk_DrawFocusHighlight,V_Tk_DrawFocusHighlight,_ANSI_ARGS_((Tk_Window tkwin,
				GC gc, int width, Drawable drawable)))
#endif /* #ifndef Tk_DrawFocusHighlight */

#ifndef Tk_DrawTextLayout
VFUNC(void,Tk_DrawTextLayout,V_Tk_DrawTextLayout,_ANSI_ARGS_((Display * display,
				Drawable drawable, GC gc,
				Tk_TextLayout layout, int x, int y,
				int firstChar, int lastChar)))
#endif /* #ifndef Tk_DrawTextLayout */

#ifndef Tk_Fill3DPolygon
VFUNC(void,Tk_Fill3DPolygon,V_Tk_Fill3DPolygon,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border,
				XPoint * pointPtr, int numPoints,
				int borderWidth, int leftRelief)))
#endif /* #ifndef Tk_Fill3DPolygon */

#ifndef Tk_Fill3DRectangle
VFUNC(void,Tk_Fill3DRectangle,V_Tk_Fill3DRectangle,_ANSI_ARGS_((Tk_Window tkwin,
				Drawable drawable, Tk_3DBorder border, int x,
				int y, int width, int height,
				int borderWidth, int relief)))
#endif /* #ifndef Tk_Fill3DRectangle */

#ifndef Tk_FontId
VFUNC(Font,Tk_FontId,V_Tk_FontId,_ANSI_ARGS_((Tk_Font font)))
#endif /* #ifndef Tk_FontId */

#ifndef Tk_Free3DBorder
VFUNC(void,Tk_Free3DBorder,V_Tk_Free3DBorder,_ANSI_ARGS_((Tk_3DBorder border)))
#endif /* #ifndef Tk_Free3DBorder */

#ifndef Tk_Free3DBorderFromObj
VFUNC(void,Tk_Free3DBorderFromObj,V_Tk_Free3DBorderFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_Free3DBorderFromObj */

#ifndef Tk_FreeBitmap
VFUNC(void,Tk_FreeBitmap,V_Tk_FreeBitmap,_ANSI_ARGS_((Display * display,
				Pixmap bitmap)))
#endif /* #ifndef Tk_FreeBitmap */

#ifndef Tk_FreeBitmapFromObj
VFUNC(void,Tk_FreeBitmapFromObj,V_Tk_FreeBitmapFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_FreeBitmapFromObj */

#ifndef Tk_FreeColor
VFUNC(void,Tk_FreeColor,V_Tk_FreeColor,_ANSI_ARGS_((XColor * colorPtr)))
#endif /* #ifndef Tk_FreeColor */

#ifndef Tk_FreeColorFromObj
VFUNC(void,Tk_FreeColorFromObj,V_Tk_FreeColorFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_FreeColorFromObj */

#ifndef Tk_FreeColormap
VFUNC(void,Tk_FreeColormap,V_Tk_FreeColormap,_ANSI_ARGS_((Display * display,
				Colormap colormap)))
#endif /* #ifndef Tk_FreeColormap */

#ifndef Tk_FreeConfigOptions
VFUNC(void,Tk_FreeConfigOptions,V_Tk_FreeConfigOptions,_ANSI_ARGS_((char * recordPtr,
				Tk_OptionTable optionToken, Tk_Window tkwin)))
#endif /* #ifndef Tk_FreeConfigOptions */

#ifndef Tk_FreeCursor
VFUNC(void,Tk_FreeCursor,V_Tk_FreeCursor,_ANSI_ARGS_((Display * display,
				Tk_Cursor cursor)))
#endif /* #ifndef Tk_FreeCursor */

#ifndef Tk_FreeCursorFromObj
VFUNC(void,Tk_FreeCursorFromObj,V_Tk_FreeCursorFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_FreeCursorFromObj */

#ifndef Tk_FreeFont
VFUNC(void,Tk_FreeFont,V_Tk_FreeFont,_ANSI_ARGS_((Tk_Font f)))
#endif /* #ifndef Tk_FreeFont */

#ifndef Tk_FreeFontFromObj
VFUNC(void,Tk_FreeFontFromObj,V_Tk_FreeFontFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_FreeFontFromObj */

#ifndef Tk_FreeGC
VFUNC(void,Tk_FreeGC,V_Tk_FreeGC,_ANSI_ARGS_((Display * display, GC gc)))
#endif /* #ifndef Tk_FreeGC */

#ifndef Tk_FreeImage
VFUNC(void,Tk_FreeImage,V_Tk_FreeImage,_ANSI_ARGS_((Tk_Image image)))
#endif /* #ifndef Tk_FreeImage */

#ifndef Tk_FreeOptions
VFUNC(void,Tk_FreeOptions,V_Tk_FreeOptions,_ANSI_ARGS_((Tk_ConfigSpec * specs,
				char * widgRec, Display * display,
				int needFlags)))
#endif /* #ifndef Tk_FreeOptions */

#ifndef Tk_FreePixmap
VFUNC(void,Tk_FreePixmap,V_Tk_FreePixmap,_ANSI_ARGS_((Display * display,
				Pixmap pixmap)))
#endif /* #ifndef Tk_FreePixmap */

#ifndef Tk_FreeSavedOptions
VFUNC(void,Tk_FreeSavedOptions,V_Tk_FreeSavedOptions,_ANSI_ARGS_((
				Tk_SavedOptions * savePtr)))
#endif /* #ifndef Tk_FreeSavedOptions */

#ifndef Tk_FreeStyle
VFUNC(void,Tk_FreeStyle,V_Tk_FreeStyle,_ANSI_ARGS_((Tk_Style style)))
#endif /* #ifndef Tk_FreeStyle */

#ifndef Tk_FreeStyleFromObj
VFUNC(void,Tk_FreeStyleFromObj,V_Tk_FreeStyleFromObj,_ANSI_ARGS_((Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_FreeStyleFromObj */

#ifndef Tk_FreeTextLayout
VFUNC(void,Tk_FreeTextLayout,V_Tk_FreeTextLayout,_ANSI_ARGS_((
				Tk_TextLayout textLayout)))
#endif /* #ifndef Tk_FreeTextLayout */

#ifndef Tk_FreeXId
VFUNC(void,Tk_FreeXId,V_Tk_FreeXId,_ANSI_ARGS_((Display * display, XID xid)))
#endif /* #ifndef Tk_FreeXId */

#ifndef Tk_GCForColor
VFUNC(GC,Tk_GCForColor,V_Tk_GCForColor,_ANSI_ARGS_((XColor * colorPtr,
				Drawable drawable)))
#endif /* #ifndef Tk_GCForColor */

#ifndef Tk_GeometryRequest
VFUNC(void,Tk_GeometryRequest,V_Tk_GeometryRequest,_ANSI_ARGS_((Tk_Window tkwin,
				int reqWidth, int reqHeight)))
#endif /* #ifndef Tk_GeometryRequest */

#ifndef Tk_Get3DBorder
VFUNC(Tk_3DBorder,Tk_Get3DBorder,V_Tk_Get3DBorder,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_Uid colorName)))
#endif /* #ifndef Tk_Get3DBorder */

#ifndef Tk_Get3DBorderFromObj
VFUNC(Tk_3DBorder,Tk_Get3DBorderFromObj,V_Tk_Get3DBorderFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_Get3DBorderFromObj */

#ifndef Tk_GetAllBindings
VFUNC(void,Tk_GetAllBindings,V_Tk_GetAllBindings,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_BindingTable bindingTable,
				ClientData object)))
#endif /* #ifndef Tk_GetAllBindings */

#ifndef Tk_GetAnchor
VFUNC(int,Tk_GetAnchor,V_Tk_GetAnchor,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, Tk_Anchor * anchorPtr)))
#endif /* #ifndef Tk_GetAnchor */

#ifndef Tk_GetAnchorFromObj
VFUNC(int,Tk_GetAnchorFromObj,V_Tk_GetAnchorFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, Tk_Anchor * anchorPtr)))
#endif /* #ifndef Tk_GetAnchorFromObj */

#ifndef Tk_GetAtomName
VFUNC(CONST84_RETURN char *,Tk_GetAtomName,V_Tk_GetAtomName,_ANSI_ARGS_((Tk_Window tkwin,
				Atom atom)))
#endif /* #ifndef Tk_GetAtomName */

#ifndef Tk_GetBinding
VFUNC(Tcl_Obj *,Tk_GetBinding,V_Tk_GetBinding,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_BindingTable bindingTable,
				ClientData object, CONST char * eventStr)))
#endif /* #ifndef Tk_GetBinding */

#ifndef Tk_GetBitmap
VFUNC(Pixmap,Tk_GetBitmap,V_Tk_GetBitmap,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * str)))
#endif /* #ifndef Tk_GetBitmap */

#ifndef Tk_GetBitmapFromData
VFUNC(Pixmap,Tk_GetBitmapFromData,V_Tk_GetBitmapFromData,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				CONST char * source, int width, int height)))
#endif /* #ifndef Tk_GetBitmapFromData */

#ifndef Tk_GetBitmapFromObj
VFUNC(Pixmap,Tk_GetBitmapFromObj,V_Tk_GetBitmapFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_GetBitmapFromObj */

#ifndef Tk_GetCapStyle
VFUNC(int,Tk_GetCapStyle,V_Tk_GetCapStyle,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, int * capPtr)))
#endif /* #ifndef Tk_GetCapStyle */

#ifndef Tk_GetColor
VFUNC(XColor *,Tk_GetColor,V_Tk_GetColor,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_Uid name)))
#endif /* #ifndef Tk_GetColor */

#ifndef Tk_GetColorByValue
VFUNC(XColor *,Tk_GetColorByValue,V_Tk_GetColorByValue,_ANSI_ARGS_((Tk_Window tkwin,
				XColor * colorPtr)))
#endif /* #ifndef Tk_GetColorByValue */

#ifndef Tk_GetColorFromObj
VFUNC(XColor *,Tk_GetColorFromObj,V_Tk_GetColorFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_GetColorFromObj */

#ifndef Tk_GetColormap
VFUNC(Colormap,Tk_GetColormap,V_Tk_GetColormap,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * str)))
#endif /* #ifndef Tk_GetColormap */

#ifndef Tk_GetCursor
VFUNC(Tk_Cursor,Tk_GetCursor,V_Tk_GetCursor,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_Uid str)))
#endif /* #ifndef Tk_GetCursor */

#ifndef Tk_GetCursorFromData
VFUNC(Tk_Cursor,Tk_GetCursorFromData,V_Tk_GetCursorFromData,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				CONST char * source, CONST char * mask,
				int width, int height, int xHot, int yHot,
				Tk_Uid fg, Tk_Uid bg)))
#endif /* #ifndef Tk_GetCursorFromData */

#ifndef Tk_GetCursorFromObj
VFUNC(Tk_Cursor,Tk_GetCursorFromObj,V_Tk_GetCursorFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_GetCursorFromObj */

#ifndef Tk_GetElementBorderWidth
VFUNC(int,Tk_GetElementBorderWidth,V_Tk_GetElementBorderWidth,_ANSI_ARGS_((Tk_Style style,
				Tk_StyledElement element, char * recordPtr,
				Tk_Window tkwin)))
#endif /* #ifndef Tk_GetElementBorderWidth */

#ifndef Tk_GetElementBox
VFUNC(void,Tk_GetElementBox,V_Tk_GetElementBox,_ANSI_ARGS_((Tk_Style style,
				Tk_StyledElement element, char * recordPtr,
				Tk_Window tkwin, int x, int y, int width,
				int height, int inner, int * xPtr,
				int * yPtr, int * widthPtr, int * heightPtr)))
#endif /* #ifndef Tk_GetElementBox */

#ifndef Tk_GetElementId
VFUNC(int,Tk_GetElementId,V_Tk_GetElementId,_ANSI_ARGS_((CONST char * name)))
#endif /* #ifndef Tk_GetElementId */

#ifndef Tk_GetElementSize
VFUNC(void,Tk_GetElementSize,V_Tk_GetElementSize,_ANSI_ARGS_((Tk_Style style,
				Tk_StyledElement element, char * recordPtr,
				Tk_Window tkwin, int width, int height,
				int inner, int * widthPtr, int * heightPtr)))
#endif /* #ifndef Tk_GetElementSize */

#ifndef Tk_GetFont
VFUNC(Tk_Font,Tk_GetFont,V_Tk_GetFont,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * str)))
#endif /* #ifndef Tk_GetFont */

#ifndef Tk_GetFontFromObj
VFUNC(Tk_Font,Tk_GetFontFromObj,V_Tk_GetFontFromObj,_ANSI_ARGS_((Tk_Window tkwin,
				Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_GetFontFromObj */

#ifndef Tk_GetFontMetrics
VFUNC(void,Tk_GetFontMetrics,V_Tk_GetFontMetrics,_ANSI_ARGS_((Tk_Font font,
				Tk_FontMetrics * fmPtr)))
#endif /* #ifndef Tk_GetFontMetrics */

#ifndef Tk_GetGC
VFUNC(GC,Tk_GetGC,V_Tk_GetGC,_ANSI_ARGS_((Tk_Window tkwin,
				unsigned long valueMask,
				XGCValues * valuePtr)))
#endif /* #ifndef Tk_GetGC */

#ifndef Tk_GetImage
VFUNC(Tk_Image,Tk_GetImage,V_Tk_GetImage,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * name,
				Tk_ImageChangedProc * changeProc,
				ClientData clientData)))
#endif /* #ifndef Tk_GetImage */

#ifndef Tk_GetImageMasterData
VFUNC(ClientData,Tk_GetImageMasterData,V_Tk_GetImageMasterData,_ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * name,
				Tk_ImageType ** typePtrPtr)))
#endif /* #ifndef Tk_GetImageMasterData */

#ifndef Tk_GetJoinStyle
VFUNC(int,Tk_GetJoinStyle,V_Tk_GetJoinStyle,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, int * joinPtr)))
#endif /* #ifndef Tk_GetJoinStyle */

#ifndef Tk_GetJustify
VFUNC(int,Tk_GetJustify,V_Tk_GetJustify,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * str, Tk_Justify * justifyPtr)))
#endif /* #ifndef Tk_GetJustify */

#ifndef Tk_GetJustifyFromObj
VFUNC(int,Tk_GetJustifyFromObj,V_Tk_GetJustifyFromObj,_ANSI_ARGS_((
				Tcl_Interp * interp, Tcl_Obj * objPtr,
				Tk_Justify * justifyPtr)))
#endif /* #ifndef Tk_GetJustifyFromObj */

#ifndef Tk_GetMMFromObj
VFUNC(int,Tk_GetMMFromObj,V_Tk_GetMMFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj * objPtr,
				double * doublePtr)))
#endif /* #ifndef Tk_GetMMFromObj */

#ifndef Tk_GetNumMainWindows
VFUNC(int,Tk_GetNumMainWindows,V_Tk_GetNumMainWindows,_ANSI_ARGS_((void)))
#endif /* #ifndef Tk_GetNumMainWindows */

#ifndef Tk_GetOptionInfo
VFUNC(Tcl_Obj *,Tk_GetOptionInfo,V_Tk_GetOptionInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				char * recordPtr, Tk_OptionTable optionTable,
				Tcl_Obj * namePtr, Tk_Window tkwin)))
#endif /* #ifndef Tk_GetOptionInfo */

#ifndef Tk_GetOptionValue
VFUNC(Tcl_Obj *,Tk_GetOptionValue,V_Tk_GetOptionValue,_ANSI_ARGS_((Tcl_Interp * interp,
				char * recordPtr, Tk_OptionTable optionTable,
				Tcl_Obj * namePtr, Tk_Window tkwin)))
#endif /* #ifndef Tk_GetOptionValue */

#ifndef Tk_GetPixels
VFUNC(int,Tk_GetPixels,V_Tk_GetPixels,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * str,
				int * intPtr)))
#endif /* #ifndef Tk_GetPixels */

#ifndef Tk_GetPixelsFromObj
VFUNC(int,Tk_GetPixelsFromObj,V_Tk_GetPixelsFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj * objPtr,
				int * intPtr)))
#endif /* #ifndef Tk_GetPixelsFromObj */

#ifndef Tk_GetPixmap
VFUNC(Pixmap,Tk_GetPixmap,V_Tk_GetPixmap,_ANSI_ARGS_((Display * display,
				Drawable d, int width, int height, int depth)))
#endif /* #ifndef Tk_GetPixmap */

#ifndef Tk_GetRelief
VFUNC(int,Tk_GetRelief,V_Tk_GetRelief,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name, int * reliefPtr)))
#endif /* #ifndef Tk_GetRelief */

#ifndef Tk_GetReliefFromObj
VFUNC(int,Tk_GetReliefFromObj,V_Tk_GetReliefFromObj,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * objPtr, int * resultPtr)))
#endif /* #ifndef Tk_GetReliefFromObj */

#ifndef Tk_GetRootCoords
VFUNC(void,Tk_GetRootCoords,V_Tk_GetRootCoords,_ANSI_ARGS_((Tk_Window tkwin,
				int * xPtr, int * yPtr)))
#endif /* #ifndef Tk_GetRootCoords */

#ifndef Tk_GetScreenMM
VFUNC(int,Tk_GetScreenMM,V_Tk_GetScreenMM,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, CONST char * str,
				double * doublePtr)))
#endif /* #ifndef Tk_GetScreenMM */

#ifndef Tk_GetScrollInfo
VFUNC(int,Tk_GetScrollInfo,V_Tk_GetScrollInfo,_ANSI_ARGS_((Tcl_Interp * interp,
				int argc, CONST84 Tcl_Obj *CONST *objv,
				double * dblPtr, int * intPtr)))
#endif /* #ifndef Tk_GetScrollInfo */

#ifndef Tk_GetScrollInfoObj
VFUNC(int,Tk_GetScrollInfoObj,V_Tk_GetScrollInfoObj,_ANSI_ARGS_((Tcl_Interp * interp,
				int objc, Tcl_Obj *CONST objv[],
				double * dblPtr, int * intPtr)))
#endif /* #ifndef Tk_GetScrollInfoObj */

#ifndef Tk_GetSelection
VFUNC(int,Tk_GetSelection,V_Tk_GetSelection,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Atom selection, Atom target,
				Tk_GetSelProc * proc, ClientData clientData)))
#endif /* #ifndef Tk_GetSelection */

#ifndef Tk_GetStyle
VFUNC(Tk_Style,Tk_GetStyle,V_Tk_GetStyle,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * name)))
#endif /* #ifndef Tk_GetStyle */

#ifndef Tk_GetStyleEngine
VFUNC(Tk_StyleEngine,Tk_GetStyleEngine,V_Tk_GetStyleEngine,_ANSI_ARGS_((CONST char * name)))
#endif /* #ifndef Tk_GetStyleEngine */

#ifndef Tk_GetStyleFromObj
VFUNC(Tk_Style,Tk_GetStyleFromObj,V_Tk_GetStyleFromObj,_ANSI_ARGS_((Tcl_Obj * objPtr)))
#endif /* #ifndef Tk_GetStyleFromObj */

#ifndef Tk_GetStyledElement
VFUNC(Tk_StyledElement,Tk_GetStyledElement,V_Tk_GetStyledElement,_ANSI_ARGS_((Tk_Style style,
				int elementId, Tk_OptionTable optionTable)))
#endif /* #ifndef Tk_GetStyledElement */

#ifndef Tk_GetUid
VFUNC(Tk_Uid,Tk_GetUid,V_Tk_GetUid,_ANSI_ARGS_((CONST char * str)))
#endif /* #ifndef Tk_GetUid */

#ifndef Tk_GetVRootGeometry
VFUNC(void,Tk_GetVRootGeometry,V_Tk_GetVRootGeometry,_ANSI_ARGS_((Tk_Window tkwin,
				int * xPtr, int * yPtr, int * widthPtr,
				int * heightPtr)))
#endif /* #ifndef Tk_GetVRootGeometry */

#ifndef Tk_GetVisual
VFUNC(Visual *,Tk_GetVisual,V_Tk_GetVisual,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tcl_Obj *str,
			        int * depthPtr,	Colormap * colormapPtr)))
#endif /* #ifndef Tk_GetVisual */

#ifndef Tk_Grab
VFUNC(int,Tk_Grab,V_Tk_Grab,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, int grabGlobal)))
#endif /* #ifndef Tk_Grab */

#ifndef Tk_HandleEvent
VFUNC(void,Tk_HandleEvent,V_Tk_HandleEvent,_ANSI_ARGS_((XEvent * eventPtr)))
#endif /* #ifndef Tk_HandleEvent */

#ifndef Tk_IdToWindow
VFUNC(Tk_Window,Tk_IdToWindow,V_Tk_IdToWindow,_ANSI_ARGS_((Display * display,
				Window window)))
#endif /* #ifndef Tk_IdToWindow */

#ifndef Tk_ImageChanged
VFUNC(void,Tk_ImageChanged,V_Tk_ImageChanged,_ANSI_ARGS_((Tk_ImageMaster master,
				int x, int y, int width, int height,
				int imageWidth, int imageHeight)))
#endif /* #ifndef Tk_ImageChanged */

#ifndef Tk_InitOptions
VFUNC(int,Tk_InitOptions,V_Tk_InitOptions,_ANSI_ARGS_((Tcl_Interp * interp,
				char * recordPtr, Tk_OptionTable optionToken,
				Tk_Window tkwin)))
#endif /* #ifndef Tk_InitOptions */

#ifndef Tk_InternAtom
VFUNC(Atom,Tk_InternAtom,V_Tk_InternAtom,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * name)))
#endif /* #ifndef Tk_InternAtom */

#ifndef Tk_IntersectTextLayout
VFUNC(int,Tk_IntersectTextLayout,V_Tk_IntersectTextLayout,_ANSI_ARGS_((
				Tk_TextLayout layout, int x, int y,
				int width, int height)))
#endif /* #ifndef Tk_IntersectTextLayout */

#ifndef Tk_MainLoop
VFUNC(void,Tk_MainLoop,V_Tk_MainLoop,_ANSI_ARGS_((void)))
#endif /* #ifndef Tk_MainLoop */

#ifndef Tk_MainWindow
VFUNC(Tk_Window,Tk_MainWindow,V_Tk_MainWindow,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifndef Tk_MainWindow */

#ifndef Tk_MaintainGeometry
VFUNC(void,Tk_MaintainGeometry,V_Tk_MaintainGeometry,_ANSI_ARGS_((Tk_Window slave,
				Tk_Window master, int x, int y, int width,
				int height)))
#endif /* #ifndef Tk_MaintainGeometry */

#ifndef Tk_MakeWindowExist
VFUNC(void,Tk_MakeWindowExist,V_Tk_MakeWindowExist,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_MakeWindowExist */

#ifndef Tk_ManageGeometry
VFUNC(void,Tk_ManageGeometry,V_Tk_ManageGeometry,_ANSI_ARGS_((Tk_Window tkwin,
				Tk_GeomMgr * mgrPtr, ClientData clientData)))
#endif /* #ifndef Tk_ManageGeometry */

#ifndef Tk_MapWindow
VFUNC(void,Tk_MapWindow,V_Tk_MapWindow,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_MapWindow */

#ifndef Tk_MeasureChars
VFUNC(int,Tk_MeasureChars,V_Tk_MeasureChars,_ANSI_ARGS_((Tk_Font tkfont,
				CONST char * source, int numBytes,
				int maxPixels, int flags, int * lengthPtr)))
#endif /* #ifndef Tk_MeasureChars */

#ifndef Tk_MoveResizeWindow
VFUNC(void,Tk_MoveResizeWindow,V_Tk_MoveResizeWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int x, int y, int width, int height)))
#endif /* #ifndef Tk_MoveResizeWindow */

#ifndef Tk_MoveToplevelWindow
VFUNC(void,Tk_MoveToplevelWindow,V_Tk_MoveToplevelWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int x, int y)))
#endif /* #ifndef Tk_MoveToplevelWindow */

#ifndef Tk_MoveWindow
VFUNC(void,Tk_MoveWindow,V_Tk_MoveWindow,_ANSI_ARGS_((Tk_Window tkwin, int x,
				int y)))
#endif /* #ifndef Tk_MoveWindow */

#ifndef Tk_NameOf3DBorder
VFUNC(CONST84_RETURN char *,Tk_NameOf3DBorder,V_Tk_NameOf3DBorder,_ANSI_ARGS_((
				Tk_3DBorder border)))
#endif /* #ifndef Tk_NameOf3DBorder */

#ifndef Tk_NameOfAnchor
VFUNC(CONST84_RETURN char *,Tk_NameOfAnchor,V_Tk_NameOfAnchor,_ANSI_ARGS_((Tk_Anchor anchor)))
#endif /* #ifndef Tk_NameOfAnchor */

#ifndef Tk_NameOfBitmap
VFUNC(CONST84_RETURN char *,Tk_NameOfBitmap,V_Tk_NameOfBitmap,_ANSI_ARGS_((Display * display,
				Pixmap bitmap)))
#endif /* #ifndef Tk_NameOfBitmap */

#ifndef Tk_NameOfCapStyle
VFUNC(CONST84_RETURN char *,Tk_NameOfCapStyle,V_Tk_NameOfCapStyle,_ANSI_ARGS_((int cap)))
#endif /* #ifndef Tk_NameOfCapStyle */

#ifndef Tk_NameOfColor
VFUNC(CONST84_RETURN char *,Tk_NameOfColor,V_Tk_NameOfColor,_ANSI_ARGS_((XColor * colorPtr)))
#endif /* #ifndef Tk_NameOfColor */

#ifndef Tk_NameOfCursor
VFUNC(CONST84_RETURN char *,Tk_NameOfCursor,V_Tk_NameOfCursor,_ANSI_ARGS_((Display * display,
				Tk_Cursor cursor)))
#endif /* #ifndef Tk_NameOfCursor */

#ifndef Tk_NameOfFont
VFUNC(CONST84_RETURN char *,Tk_NameOfFont,V_Tk_NameOfFont,_ANSI_ARGS_((Tk_Font font)))
#endif /* #ifndef Tk_NameOfFont */

#ifndef Tk_NameOfImage
VFUNC(CONST84_RETURN char *,Tk_NameOfImage,V_Tk_NameOfImage,_ANSI_ARGS_((
				Tk_ImageMaster imageMaster)))
#endif /* #ifndef Tk_NameOfImage */

#ifndef Tk_NameOfJoinStyle
VFUNC(CONST84_RETURN char *,Tk_NameOfJoinStyle,V_Tk_NameOfJoinStyle,_ANSI_ARGS_((int join)))
#endif /* #ifndef Tk_NameOfJoinStyle */

#ifndef Tk_NameOfJustify
VFUNC(CONST84_RETURN char *,Tk_NameOfJustify,V_Tk_NameOfJustify,_ANSI_ARGS_((
				Tk_Justify justify)))
#endif /* #ifndef Tk_NameOfJustify */

#ifndef Tk_NameOfRelief
VFUNC(CONST84_RETURN char *,Tk_NameOfRelief,V_Tk_NameOfRelief,_ANSI_ARGS_((int relief)))
#endif /* #ifndef Tk_NameOfRelief */

#ifndef Tk_NameOfStyle
VFUNC(CONST char *,Tk_NameOfStyle,V_Tk_NameOfStyle,_ANSI_ARGS_((Tk_Style style)))
#endif /* #ifndef Tk_NameOfStyle */

#ifndef Tk_NameToWindow
VFUNC(Tk_Window,Tk_NameToWindow,V_Tk_NameToWindow,_ANSI_ARGS_((Tcl_Interp * interp,
				CONST char * pathName, Tk_Window tkwin)))
#endif /* #ifndef Tk_NameToWindow */

#ifndef Tk_OwnSelection
VFUNC(void,Tk_OwnSelection,V_Tk_OwnSelection,_ANSI_ARGS_((Tk_Window tkwin,
				Atom selection, Tk_LostSelProc * proc,
				ClientData clientData)))
#endif /* #ifndef Tk_OwnSelection */

#ifndef Tk_PointToChar
VFUNC(int,Tk_PointToChar,V_Tk_PointToChar,_ANSI_ARGS_((Tk_TextLayout layout,
				int x, int y)))
#endif /* #ifndef Tk_PointToChar */

#ifndef Tk_PostscriptBitmap
VFUNC(int,Tk_PostscriptBitmap,V_Tk_PostscriptBitmap,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_Window tkwin, Tk_PostscriptInfo psInfo,
				Pixmap bitmap, int startX, int startY,
				int width, int height)))
#endif /* #ifndef Tk_PostscriptBitmap */

#ifndef Tk_PostscriptColor
VFUNC(int,Tk_PostscriptColor,V_Tk_PostscriptColor,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_PostscriptInfo psInfo, XColor * colorPtr)))
#endif /* #ifndef Tk_PostscriptColor */

#ifndef Tk_PostscriptFont
VFUNC(int,Tk_PostscriptFont,V_Tk_PostscriptFont,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_PostscriptInfo psInfo, Tk_Font font)))
#endif /* #ifndef Tk_PostscriptFont */

#ifndef Tk_PostscriptFontName
VFUNC(int,Tk_PostscriptFontName,V_Tk_PostscriptFontName,_ANSI_ARGS_((Tk_Font tkfont,
				Tcl_DString * dsPtr)))
#endif /* #ifndef Tk_PostscriptFontName */

#ifndef Tk_PostscriptImage
VFUNC(int,Tk_PostscriptImage,V_Tk_PostscriptImage,_ANSI_ARGS_((Tk_Image image,
				Tcl_Interp * interp, Tk_Window tkwin,
				Tk_PostscriptInfo psinfo, int x, int y,
				int width, int height, int prepass)))
#endif /* #ifndef Tk_PostscriptImage */

#ifndef Tk_PostscriptPath
VFUNC(void,Tk_PostscriptPath,V_Tk_PostscriptPath,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_PostscriptInfo psInfo, double * coordPtr,
				int numPoints)))
#endif /* #ifndef Tk_PostscriptPath */

#ifndef Tk_PostscriptPhoto
VFUNC(int,Tk_PostscriptPhoto,V_Tk_PostscriptPhoto,_ANSI_ARGS_((Tcl_Interp * interp,
				Tk_PhotoImageBlock * blockPtr,
				Tk_PostscriptInfo psInfo, int width,
				int height)))
#endif /* #ifndef Tk_PostscriptPhoto */

#ifndef Tk_PostscriptStipple
VFUNC(int,Tk_PostscriptStipple,V_Tk_PostscriptStipple,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_Window tkwin,
				Tk_PostscriptInfo psInfo, Pixmap bitmap)))
#endif /* #ifndef Tk_PostscriptStipple */

#ifndef Tk_PostscriptY
VFUNC(double,Tk_PostscriptY,V_Tk_PostscriptY,_ANSI_ARGS_((double y,
				Tk_PostscriptInfo psInfo)))
#endif /* #ifndef Tk_PostscriptY */

#ifndef Tk_PreserveColormap
VFUNC(void,Tk_PreserveColormap,V_Tk_PreserveColormap,_ANSI_ARGS_((Display * display,
				Colormap colormap)))
#endif /* #ifndef Tk_PreserveColormap */

#ifndef Tk_QueueWindowEvent
VFUNC(void,Tk_QueueWindowEvent,V_Tk_QueueWindowEvent,_ANSI_ARGS_((XEvent * eventPtr,
				Tcl_QueuePosition position)))
#endif /* #ifndef Tk_QueueWindowEvent */

#ifndef Tk_RedrawImage
VFUNC(void,Tk_RedrawImage,V_Tk_RedrawImage,_ANSI_ARGS_((Tk_Image image,
				int imageX, int imageY, int width,
				int height, Drawable drawable, int drawableX,
				int drawableY)))
#endif /* #ifndef Tk_RedrawImage */

#ifndef Tk_RegisterStyleEngine
VFUNC(Tk_StyleEngine,Tk_RegisterStyleEngine,V_Tk_RegisterStyleEngine,_ANSI_ARGS_((
				CONST char * name, Tk_StyleEngine parent)))
#endif /* #ifndef Tk_RegisterStyleEngine */

#ifndef Tk_RegisterStyledElement
VFUNC(int,Tk_RegisterStyledElement,V_Tk_RegisterStyledElement,_ANSI_ARGS_((
				Tk_StyleEngine engine,
				Tk_ElementSpec * templatePtr)))
#endif /* #ifndef Tk_RegisterStyledElement */

#ifndef Tk_ResizeWindow
VFUNC(void,Tk_ResizeWindow,V_Tk_ResizeWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int width, int height)))
#endif /* #ifndef Tk_ResizeWindow */

#ifndef Tk_RestackWindow
VFUNC(int,Tk_RestackWindow,V_Tk_RestackWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int aboveBelow, Tk_Window other)))
#endif /* #ifndef Tk_RestackWindow */

#ifndef Tk_RestoreSavedOptions
VFUNC(void,Tk_RestoreSavedOptions,V_Tk_RestoreSavedOptions,_ANSI_ARGS_((
				Tk_SavedOptions * savePtr)))
#endif /* #ifndef Tk_RestoreSavedOptions */

#ifndef Tk_RestrictEvents
VFUNC(Tk_RestrictProc *,Tk_RestrictEvents,V_Tk_RestrictEvents,_ANSI_ARGS_((
				Tk_RestrictProc * proc, ClientData arg,
				ClientData * prevArgPtr)))
#endif /* #ifndef Tk_RestrictEvents */

#ifndef Tk_SetAppName
VFUNC(CONST char *,Tk_SetAppName,V_Tk_SetAppName,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * name)))
#endif /* #ifndef Tk_SetAppName */

#ifndef Tk_SetBackgroundFromBorder
VFUNC(void,Tk_SetBackgroundFromBorder,V_Tk_SetBackgroundFromBorder,_ANSI_ARGS_((
				Tk_Window tkwin, Tk_3DBorder border)))
#endif /* #ifndef Tk_SetBackgroundFromBorder */

#ifndef Tk_SetCaretPos
VFUNC(void,Tk_SetCaretPos,V_Tk_SetCaretPos,_ANSI_ARGS_((Tk_Window tkwin, int x,
				int y, int height)))
#endif /* #ifndef Tk_SetCaretPos */

#ifndef Tk_SetClass
VFUNC(void,Tk_SetClass,V_Tk_SetClass,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * className)))
#endif /* #ifndef Tk_SetClass */

#ifndef Tk_SetClassProcs
VFUNC(void,Tk_SetClassProcs,V_Tk_SetClassProcs,_ANSI_ARGS_((Tk_Window tkwin,
				Tk_ClassProcs * procs,
				ClientData instanceData)))
#endif /* #ifndef Tk_SetClassProcs */

#ifndef Tk_SetGrid
VFUNC(void,Tk_SetGrid,V_Tk_SetGrid,_ANSI_ARGS_((Tk_Window tkwin,
				int reqWidth, int reqHeight, int gridWidth,
				int gridHeight)))
#endif /* #ifndef Tk_SetGrid */

#ifndef Tk_SetInternalBorder
VFUNC(void,Tk_SetInternalBorder,V_Tk_SetInternalBorder,_ANSI_ARGS_((Tk_Window tkwin,
				int width)))
#endif /* #ifndef Tk_SetInternalBorder */

#ifndef Tk_SetInternalBorderEx
VFUNC(void,Tk_SetInternalBorderEx,V_Tk_SetInternalBorderEx,_ANSI_ARGS_((Tk_Window tkwin,
				int left, int right, int top, int bottom)))
#endif /* #ifndef Tk_SetInternalBorderEx */

#ifndef Tk_SetMinimumRequestSize
VFUNC(void,Tk_SetMinimumRequestSize,V_Tk_SetMinimumRequestSize,_ANSI_ARGS_((
				Tk_Window tkwin, int minWidth, int minHeight)))
#endif /* #ifndef Tk_SetMinimumRequestSize */

#ifndef Tk_SetOptions
VFUNC(int,Tk_SetOptions,V_Tk_SetOptions,_ANSI_ARGS_((Tcl_Interp * interp,
				char * recordPtr, Tk_OptionTable optionTable,
				int objc, Tcl_Obj *CONST objv[],
				Tk_Window tkwin, Tk_SavedOptions * savePtr,
				int * maskPtr)))
#endif /* #ifndef Tk_SetOptions */

#ifndef Tk_SetTSOrigin
VFUNC(void,Tk_SetTSOrigin,V_Tk_SetTSOrigin,_ANSI_ARGS_((Tk_Window tkwin, GC gc,
				int x, int y)))
#endif /* #ifndef Tk_SetTSOrigin */

#ifndef Tk_SetWindowBackground
VFUNC(void,Tk_SetWindowBackground,V_Tk_SetWindowBackground,_ANSI_ARGS_((Tk_Window tkwin,
				unsigned long pixel)))
#endif /* #ifndef Tk_SetWindowBackground */

#ifndef Tk_SetWindowBackgroundPixmap
VFUNC(void,Tk_SetWindowBackgroundPixmap,V_Tk_SetWindowBackgroundPixmap,_ANSI_ARGS_((
				Tk_Window tkwin, Pixmap pixmap)))
#endif /* #ifndef Tk_SetWindowBackgroundPixmap */

#ifndef Tk_SetWindowBorder
VFUNC(void,Tk_SetWindowBorder,V_Tk_SetWindowBorder,_ANSI_ARGS_((Tk_Window tkwin,
				unsigned long pixel)))
#endif /* #ifndef Tk_SetWindowBorder */

#ifndef Tk_SetWindowBorderPixmap
VFUNC(void,Tk_SetWindowBorderPixmap,V_Tk_SetWindowBorderPixmap,_ANSI_ARGS_((
				Tk_Window tkwin, Pixmap pixmap)))
#endif /* #ifndef Tk_SetWindowBorderPixmap */

#ifndef Tk_SetWindowBorderWidth
VFUNC(void,Tk_SetWindowBorderWidth,V_Tk_SetWindowBorderWidth,_ANSI_ARGS_((Tk_Window tkwin,
				int width)))
#endif /* #ifndef Tk_SetWindowBorderWidth */

#ifndef Tk_SetWindowColormap
VFUNC(void,Tk_SetWindowColormap,V_Tk_SetWindowColormap,_ANSI_ARGS_((Tk_Window tkwin,
				Colormap colormap)))
#endif /* #ifndef Tk_SetWindowColormap */

#ifndef Tk_SetWindowVisual
VFUNC(int,Tk_SetWindowVisual,V_Tk_SetWindowVisual,_ANSI_ARGS_((Tk_Window tkwin,
				Visual * visual, int depth,
				Colormap colormap)))
#endif /* #ifndef Tk_SetWindowVisual */

#ifndef Tk_SizeOfBitmap
VFUNC(void,Tk_SizeOfBitmap,V_Tk_SizeOfBitmap,_ANSI_ARGS_((Display * display,
				Pixmap bitmap, int * widthPtr,
				int * heightPtr)))
#endif /* #ifndef Tk_SizeOfBitmap */

#ifndef Tk_SizeOfImage
VFUNC(void,Tk_SizeOfImage,V_Tk_SizeOfImage,_ANSI_ARGS_((Tk_Image image,
				int * widthPtr, int * heightPtr)))
#endif /* #ifndef Tk_SizeOfImage */

#ifndef Tk_StrictMotif
VFUNC(int,Tk_StrictMotif,V_Tk_StrictMotif,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_StrictMotif */

#ifndef Tk_TextLayoutToPostscript
VFUNC(void,Tk_TextLayoutToPostscript,V_Tk_TextLayoutToPostscript,_ANSI_ARGS_((
				Tcl_Interp * interp, Tk_TextLayout layout)))
#endif /* #ifndef Tk_TextLayoutToPostscript */

#ifndef Tk_TextWidth
VFUNC(int,Tk_TextWidth,V_Tk_TextWidth,_ANSI_ARGS_((Tk_Font font,
				CONST char * str, int numBytes)))
#endif /* #ifndef Tk_TextWidth */

#ifndef Tk_UndefineCursor
VFUNC(void,Tk_UndefineCursor,V_Tk_UndefineCursor,_ANSI_ARGS_((Tk_Window window)))
#endif /* #ifndef Tk_UndefineCursor */

#ifndef Tk_UnderlineChars
VFUNC(void,Tk_UnderlineChars,V_Tk_UnderlineChars,_ANSI_ARGS_((Display * display,
				Drawable drawable, GC gc, Tk_Font tkfont,
				CONST char * source, int x, int y,
				int firstByte, int lastByte)))
#endif /* #ifndef Tk_UnderlineChars */

#ifndef Tk_UnderlineTextLayout
VFUNC(void,Tk_UnderlineTextLayout,V_Tk_UnderlineTextLayout,_ANSI_ARGS_((
				Display * display, Drawable drawable, GC gc,
				Tk_TextLayout layout, int x, int y,
				int underline)))
#endif /* #ifndef Tk_UnderlineTextLayout */

#ifndef Tk_Ungrab
VFUNC(void,Tk_Ungrab,V_Tk_Ungrab,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_Ungrab */

#ifndef Tk_UnmaintainGeometry
VFUNC(void,Tk_UnmaintainGeometry,V_Tk_UnmaintainGeometry,_ANSI_ARGS_((Tk_Window slave,
				Tk_Window master)))
#endif /* #ifndef Tk_UnmaintainGeometry */

#ifndef Tk_UnmapWindow
VFUNC(void,Tk_UnmapWindow,V_Tk_UnmapWindow,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_UnmapWindow */

#ifndef Tk_UnsetGrid
VFUNC(void,Tk_UnsetGrid,V_Tk_UnsetGrid,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifndef Tk_UnsetGrid */

#ifndef Tk_UpdatePointer
VFUNC(void,Tk_UpdatePointer,V_Tk_UpdatePointer,_ANSI_ARGS_((Tk_Window tkwin, int x,
				int y, int state)))
#endif /* #ifndef Tk_UpdatePointer */

#endif /* _TKDECLS */
