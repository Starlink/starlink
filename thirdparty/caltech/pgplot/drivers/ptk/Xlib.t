#ifdef _XLIB
#if (defined(__WIN32__) || defined(__PM__)) && !defined(DO_X_EXCLUDE)
#  define DO_X_EXCLUDE
#endif
#ifndef DO_X_EXCLUDE
#ifndef XAllocClassHint
VFUNC(XClassHint *,XAllocClassHint,V_XAllocClassHint,_ANSI_ARGS_((void)))
#endif /* #ifndef XAllocClassHint */
#endif /* !DO_X_EXCLUDE */

#ifndef XAllocColor
VFUNC(int,XAllocColor,V_XAllocColor,_ANSI_ARGS_((Display *, Colormap, XColor *)))
#endif /* #ifndef XAllocColor */

#ifndef DO_X_EXCLUDE
#ifndef XAllocNamedColor
VFUNC(int,XAllocNamedColor,V_XAllocNamedColor,_ANSI_ARGS_((Display *, Colormap, const char *, XColor *, XColor *)))
#endif /* #ifndef XAllocNamedColor */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XAllocSizeHints
VFUNC(XSizeHints *,XAllocSizeHints,V_XAllocSizeHints,_ANSI_ARGS_((void)))
#endif /* #ifndef XAllocSizeHints */
#endif /* !DO_X_EXCLUDE */

#ifndef XBell
VFUNC(int,XBell,V_XBell,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XBell */

#ifndef XChangeGC
VFUNC(int,XChangeGC,V_XChangeGC,_ANSI_ARGS_(( Display*, GC, unsigned long, XGCValues *)))
#endif /* #ifndef XChangeGC */

#ifndef XChangeProperty
VFUNC(int,XChangeProperty,V_XChangeProperty,_ANSI_ARGS_((Display *, Window, Atom, Atom, int, int, const unsigned char *, int)))
#endif /* #ifndef XChangeProperty */

#ifndef XChangeWindowAttributes
VFUNC(int,XChangeWindowAttributes,V_XChangeWindowAttributes,_ANSI_ARGS_((Display *, Window, long unsigned int, XSetWindowAttributes *)))
#endif /* #ifndef XChangeWindowAttributes */

#ifndef DO_X_EXCLUDE
#ifndef XCheckIfEvent
VFUNC(int,XCheckIfEvent,V_XCheckIfEvent,_ANSI_ARGS_((Display *, XEvent *, int (*) (Display *, XEvent *, char *), char *)))
#endif /* #ifndef XCheckIfEvent */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XCheckWindowEvent
VFUNC(int,XCheckWindowEvent,V_XCheckWindowEvent,_ANSI_ARGS_((Display *, Window, long int, XEvent *)))
#endif /* #ifndef XCheckWindowEvent */
#endif /* !DO_X_EXCLUDE */

#ifndef XClearWindow
VFUNC(int,XClearWindow,V_XClearWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XClearWindow */

#ifndef DO_X_EXCLUDE
#ifndef XClipBox
VFUNC(int,XClipBox,V_XClipBox,_ANSI_ARGS_((Region, XRectangle *)))
#endif /* #ifndef XClipBox */
#endif /* !DO_X_EXCLUDE */

#ifndef XConfigureWindow
VFUNC(int,XConfigureWindow,V_XConfigureWindow,_ANSI_ARGS_((Display *, Window, unsigned int, XWindowChanges *)))
#endif /* #ifndef XConfigureWindow */

#ifndef DO_X_EXCLUDE
#ifndef XConvertSelection
VFUNC(int,XConvertSelection,V_XConvertSelection,_ANSI_ARGS_((Display *, Atom, Atom, Atom, Window, Time)))
#endif /* #ifndef XConvertSelection */
#endif /* !DO_X_EXCLUDE */

#ifndef XCopyArea
VFUNC(int,XCopyArea,V_XCopyArea,_ANSI_ARGS_((Display *, Drawable, Drawable, GC, int, int, unsigned int, unsigned int, int, int)))
#endif /* #ifndef XCopyArea */

#ifndef XCopyPlane
VFUNC(int,XCopyPlane,V_XCopyPlane,_ANSI_ARGS_((Display *, Drawable, Drawable, GC, int, int, unsigned int, unsigned int, int, int, long unsigned int)))
#endif /* #ifndef XCopyPlane */

#ifndef XCreateBitmapFromData
VFUNC(Pixmap,XCreateBitmapFromData,V_XCreateBitmapFromData,_ANSI_ARGS_((Display *, Drawable, const char *, unsigned int, unsigned int)))
#endif /* #ifndef XCreateBitmapFromData */

#ifndef XCreateColormap
VFUNC(Colormap,XCreateColormap,V_XCreateColormap,_ANSI_ARGS_((Display *, Window, Visual *, int)))
#endif /* #ifndef XCreateColormap */

#ifndef XCreateGC
VFUNC(GC,XCreateGC,V_XCreateGC,_ANSI_ARGS_((Display *, Drawable, long unsigned int, XGCValues *)))
#endif /* #ifndef XCreateGC */

#ifndef XCreateGlyphCursor
VFUNC(Cursor,XCreateGlyphCursor,V_XCreateGlyphCursor,_ANSI_ARGS_((Display *, Font, Font, unsigned int, unsigned int, XColor const *, XColor const *)))
#endif /* #ifndef XCreateGlyphCursor */

#ifndef XCreateImage
VFUNC(XImage *,XCreateImage,V_XCreateImage,_ANSI_ARGS_((Display *, Visual *, unsigned int, int, int, char *, unsigned int, unsigned int, int, int)))
#endif /* #ifndef XCreateImage */

#ifndef XCreatePixmapCursor
VFUNC(Cursor,XCreatePixmapCursor,V_XCreatePixmapCursor,_ANSI_ARGS_((Display *, Pixmap, Pixmap, XColor *, XColor *, unsigned int, unsigned int)))
#endif /* #ifndef XCreatePixmapCursor */

#ifndef DO_X_EXCLUDE
#ifndef XCreateRegion
VFUNC(Region,XCreateRegion,V_XCreateRegion,_ANSI_ARGS_((void)))
#endif /* #ifndef XCreateRegion */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XCreateWindow
VFUNC(Window,XCreateWindow,V_XCreateWindow,_ANSI_ARGS_((Display *, Window, int, int, unsigned int, unsigned int, unsigned int, int, unsigned int, Visual *, long unsigned int, XSetWindowAttributes *)))
#endif /* #ifndef XCreateWindow */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultColormap
VFUNC(Colormap,XDefaultColormap,V_XDefaultColormap,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XDefaultColormap */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultDepth
VFUNC(int,XDefaultDepth,V_XDefaultDepth,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XDefaultDepth */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultScreen
VFUNC(int,XDefaultScreen,V_XDefaultScreen,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XDefaultScreen */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultVisual
VFUNC(Visual *,XDefaultVisual,V_XDefaultVisual,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XDefaultVisual */
#endif /* !DO_X_EXCLUDE */

#ifndef XDefineCursor
VFUNC(int,XDefineCursor,V_XDefineCursor,_ANSI_ARGS_((Display *, Window, Cursor)))
#endif /* #ifndef XDefineCursor */

#ifndef XDeleteProperty
VFUNC(int,XDeleteProperty,V_XDeleteProperty,_ANSI_ARGS_((Display *, Window, Atom)))
#endif /* #ifndef XDeleteProperty */

#ifndef DO_X_EXCLUDE
#ifndef XDestroyRegion
VFUNC(int,XDestroyRegion,V_XDestroyRegion,_ANSI_ARGS_((Region)))
#endif /* #ifndef XDestroyRegion */
#endif /* !DO_X_EXCLUDE */

#ifndef XDestroyWindow
VFUNC(int,XDestroyWindow,V_XDestroyWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XDestroyWindow */

#ifndef XDrawArc
VFUNC(int,XDrawArc,V_XDrawArc,_ANSI_ARGS_((Display *, Drawable, GC, int, int, unsigned int, unsigned int, int, int)))
#endif /* #ifndef XDrawArc */

#ifndef DO_X_EXCLUDE
#ifndef XDrawImageString
VFUNC(int,XDrawImageString,V_XDrawImageString,_ANSI_ARGS_((Display *, Drawable, GC, int, int, const char *, int)))
#endif /* #ifndef XDrawImageString */
#endif /* !DO_X_EXCLUDE */

#ifndef XDrawLine
VFUNC(int,XDrawLine,V_XDrawLine,_ANSI_ARGS_((Display *, Drawable, GC, int, int, int, int)))
#endif /* #ifndef XDrawLine */

#ifndef XDrawLines
VFUNC(int,XDrawLines,V_XDrawLines,_ANSI_ARGS_((Display *, Drawable, GC, XPoint *, int, int)))
#endif /* #ifndef XDrawLines */

#ifndef XDrawPoints
VFUNC(int,XDrawPoints,V_XDrawPoints,_ANSI_ARGS_(( Display*, Drawable, GC, XPoint*, int, int)))
#endif /* #ifndef XDrawPoints */

#ifndef XDrawRectangle
VFUNC(int,XDrawRectangle,V_XDrawRectangle,_ANSI_ARGS_((Display *, Drawable, GC, int, int, unsigned int, unsigned int)))
#endif /* #ifndef XDrawRectangle */

#ifndef DO_X_EXCLUDE
#ifndef XDrawString
VFUNC(int,XDrawString,V_XDrawString,_ANSI_ARGS_((Display *, Drawable, GC, int, int, const char *, int)))
#endif /* #ifndef XDrawString */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XEventsQueued
VFUNC(int,XEventsQueued,V_XEventsQueued,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XEventsQueued */
#endif /* !DO_X_EXCLUDE */

#ifndef XFillArc
VFUNC(int,XFillArc,V_XFillArc,_ANSI_ARGS_((Display *, Drawable, GC, int, int, unsigned int, unsigned int, int, int)))
#endif /* #ifndef XFillArc */

#ifndef XFillPolygon
VFUNC(int,XFillPolygon,V_XFillPolygon,_ANSI_ARGS_((Display *, Drawable, GC, XPoint *, int, int, int)))
#endif /* #ifndef XFillPolygon */

#ifndef XFillRectangle
VFUNC(int,XFillRectangle,V_XFillRectangle,_ANSI_ARGS_((Display *, Drawable, GC, int, int, unsigned int, unsigned int)))
#endif /* #ifndef XFillRectangle */

#ifndef XFillRectangles
VFUNC(int,XFillRectangles,V_XFillRectangles,_ANSI_ARGS_((Display *, Drawable, GC, XRectangle *, int)))
#endif /* #ifndef XFillRectangles */

#ifndef XFlush
VFUNC(int,XFlush,V_XFlush,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XFlush */

#ifndef XFree
VFUNC(int,XFree,V_XFree,_ANSI_ARGS_((XFree_arg_t *)))
#endif /* #ifndef XFree */

#ifndef XFreeColormap
VFUNC(int,XFreeColormap,V_XFreeColormap,_ANSI_ARGS_((Display *, Colormap)))
#endif /* #ifndef XFreeColormap */

#ifndef XFreeColors
VFUNC(int,XFreeColors,V_XFreeColors,_ANSI_ARGS_((Display *, Colormap, long unsigned int *, int, long unsigned int)))
#endif /* #ifndef XFreeColors */

#ifndef XFreeCursor
VFUNC(int,XFreeCursor,V_XFreeCursor,_ANSI_ARGS_((Display *, Cursor)))
#endif /* #ifndef XFreeCursor */

#ifndef DO_X_EXCLUDE
#ifndef XFreeFont
VFUNC(int,XFreeFont,V_XFreeFont,_ANSI_ARGS_((Display *, XFontStruct *)))
#endif /* #ifndef XFreeFont */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XFreeFontNames
VFUNC(int,XFreeFontNames,V_XFreeFontNames,_ANSI_ARGS_((char **)))
#endif /* #ifndef XFreeFontNames */
#endif /* !DO_X_EXCLUDE */

#ifndef XFreeGC
VFUNC(int,XFreeGC,V_XFreeGC,_ANSI_ARGS_((Display *, GC)))
#endif /* #ifndef XFreeGC */

#ifndef XFreeModifiermap
VFUNC(int,XFreeModifiermap,V_XFreeModifiermap,_ANSI_ARGS_((XModifierKeymap *)))
#endif /* #ifndef XFreeModifiermap */

#ifndef XGContextFromGC
VFUNC(GContext,XGContextFromGC,V_XGContextFromGC,_ANSI_ARGS_((GC)))
#endif /* #ifndef XGContextFromGC */

#ifndef XGetAtomName
VFUNC(char *,XGetAtomName,V_XGetAtomName,_ANSI_ARGS_((Display *, Atom)))
#endif /* #ifndef XGetAtomName */

#ifndef DO_X_EXCLUDE
#ifndef XGetFontProperty
VFUNC(int,XGetFontProperty,V_XGetFontProperty,_ANSI_ARGS_((XFontStruct *, Atom, long unsigned int *)))
#endif /* #ifndef XGetFontProperty */
#endif /* !DO_X_EXCLUDE */

#ifndef XGetGeometry
VFUNC(int,XGetGeometry,V_XGetGeometry,_ANSI_ARGS_((Display *, Drawable, Window *, int *, int *, unsigned int *, unsigned int *, unsigned int *, unsigned int *)))
#endif /* #ifndef XGetGeometry */

#ifndef XGetImage
VFUNC(XImage *,XGetImage,V_XGetImage,_ANSI_ARGS_((Display *, Drawable, int, int, unsigned int, unsigned int, long unsigned int, int)))
#endif /* #ifndef XGetImage */

#ifndef XGetInputFocus
VFUNC(int,XGetInputFocus,V_XGetInputFocus,_ANSI_ARGS_((Display *, Window *, int *)))
#endif /* #ifndef XGetInputFocus */

#ifndef XGetModifierMapping
VFUNC(XModifierKeymap *,XGetModifierMapping,V_XGetModifierMapping,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XGetModifierMapping */

#ifndef DO_X_EXCLUDE
#ifndef XGetSelectionOwner
VFUNC(Window,XGetSelectionOwner,V_XGetSelectionOwner,_ANSI_ARGS_((Display *, Atom)))
#endif /* #ifndef XGetSelectionOwner */
#endif /* !DO_X_EXCLUDE */

#ifndef XGetVisualInfo
VFUNC(XVisualInfo *,XGetVisualInfo,V_XGetVisualInfo,_ANSI_ARGS_((Display *, long int, XVisualInfo *, int *)))
#endif /* #ifndef XGetVisualInfo */

#ifndef XGetWMColormapWindows
VFUNC(int,XGetWMColormapWindows,V_XGetWMColormapWindows,_ANSI_ARGS_((Display *, Window, Window **, int *)))
#endif /* #ifndef XGetWMColormapWindows */

#ifndef XGetWindowAttributes
VFUNC(int,XGetWindowAttributes,V_XGetWindowAttributes,_ANSI_ARGS_((Display *, Window, XWindowAttributes *)))
#endif /* #ifndef XGetWindowAttributes */

#ifndef XGetWindowProperty
VFUNC(int,XGetWindowProperty,V_XGetWindowProperty,_ANSI_ARGS_((Display *, Window, Atom, long int, long int, int, Atom, Atom *, int *, long unsigned int *, long unsigned int *, unsigned char **)))
#endif /* #ifndef XGetWindowProperty */

#ifndef XGrabKeyboard
VFUNC(int,XGrabKeyboard,V_XGrabKeyboard,_ANSI_ARGS_((Display *, Window, int, int, int, Time)))
#endif /* #ifndef XGrabKeyboard */

#ifndef XGrabPointer
VFUNC(int,XGrabPointer,V_XGrabPointer,_ANSI_ARGS_((Display *, Window, int, unsigned int, int, int, Window, Cursor, Time)))
#endif /* #ifndef XGrabPointer */

#ifndef XGrabServer
VFUNC(int,XGrabServer,V_XGrabServer,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XGrabServer */

#ifndef XIconifyWindow
VFUNC(int,XIconifyWindow,V_XIconifyWindow,_ANSI_ARGS_((Display *, Window, int)))
#endif /* #ifndef XIconifyWindow */

#ifndef XInternAtom
VFUNC(Atom,XInternAtom,V_XInternAtom,_ANSI_ARGS_((Display *, const char *, int)))
#endif /* #ifndef XInternAtom */

#ifndef DO_X_EXCLUDE
#ifndef XIntersectRegion
VFUNC(int,XIntersectRegion,V_XIntersectRegion,_ANSI_ARGS_((Region, Region, Region)))
#endif /* #ifndef XIntersectRegion */
#endif /* !DO_X_EXCLUDE */

#ifndef XKeycodeToKeysym
VFUNC(KeySym,XKeycodeToKeysym,V_XKeycodeToKeysym,_ANSI_ARGS_((Display *, unsigned int, int)))
#endif /* #ifndef XKeycodeToKeysym */

#ifndef XKeysymToString
VFUNC(char *,XKeysymToString,V_XKeysymToString,_ANSI_ARGS_((KeySym)))
#endif /* #ifndef XKeysymToString */

#ifndef DO_X_EXCLUDE
#ifndef XListFonts
VFUNC(char **,XListFonts,V_XListFonts,_ANSI_ARGS_(( Display*, const char *, int, int *)))
#endif /* #ifndef XListFonts */
#endif /* !DO_X_EXCLUDE */

#ifndef XListHosts
VFUNC(XHostAddress *,XListHosts,V_XListHosts,_ANSI_ARGS_((Display *, int *, int *)))
#endif /* #ifndef XListHosts */

#ifndef DO_X_EXCLUDE
#ifndef XListProperties
VFUNC(Atom *,XListProperties,V_XListProperties,_ANSI_ARGS_((Display *, Window, int *)))
#endif /* #ifndef XListProperties */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XLoadFont
VFUNC(Font,XLoadFont,V_XLoadFont,_ANSI_ARGS_((Display *, const char *)))
#endif /* #ifndef XLoadFont */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XLoadQueryFont
VFUNC(XFontStruct *,XLoadQueryFont,V_XLoadQueryFont,_ANSI_ARGS_((Display *, const char *)))
#endif /* #ifndef XLoadQueryFont */
#endif /* !DO_X_EXCLUDE */

#ifndef XLookupColor
VFUNC(int,XLookupColor,V_XLookupColor,_ANSI_ARGS_((Display *, Colormap, const char *, XColor *, XColor *)))
#endif /* #ifndef XLookupColor */

#ifndef XLookupString
VFUNC(int,XLookupString,V_XLookupString,_ANSI_ARGS_((XKeyEvent *, char *, int, KeySym *, XComposeStatus *)))
#endif /* #ifndef XLookupString */

#ifndef XLowerWindow
VFUNC(int,XLowerWindow,V_XLowerWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XLowerWindow */

#ifndef XMapWindow
VFUNC(int,XMapWindow,V_XMapWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XMapWindow */

#ifndef XMoveResizeWindow
VFUNC(int,XMoveResizeWindow,V_XMoveResizeWindow,_ANSI_ARGS_((Display *, Window, int, int, unsigned int, unsigned int)))
#endif /* #ifndef XMoveResizeWindow */

#ifndef XMoveWindow
VFUNC(int,XMoveWindow,V_XMoveWindow,_ANSI_ARGS_((Display *, Window, int, int)))
#endif /* #ifndef XMoveWindow */

#ifndef XNextEvent
VFUNC(int,XNextEvent,V_XNextEvent,_ANSI_ARGS_((Display *, XEvent *)))
#endif /* #ifndef XNextEvent */

#ifndef XNoOp
VFUNC(int,XNoOp,V_XNoOp,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XNoOp */

#ifndef DO_X_EXCLUDE
#ifndef XOpenDisplay
VFUNC(Display *,XOpenDisplay,V_XOpenDisplay,_ANSI_ARGS_((const char *)))
#endif /* #ifndef XOpenDisplay */
#endif /* !DO_X_EXCLUDE */

#ifndef XParseColor
VFUNC(int,XParseColor,V_XParseColor,_ANSI_ARGS_((Display *, Colormap, const char *, XColor *)))
#endif /* #ifndef XParseColor */

#ifndef XPutBackEvent
VFUNC(int,XPutBackEvent,V_XPutBackEvent,_ANSI_ARGS_((Display *, XEvent *)))
#endif /* #ifndef XPutBackEvent */

#ifndef DO_X_EXCLUDE
#ifndef XPutImage
VFUNC(int,XPutImage,V_XPutImage,_ANSI_ARGS_((Display *, Drawable, GC, XImage *, int, int, int, int, unsigned int, unsigned int)))
#endif /* #ifndef XPutImage */
#endif /* !DO_X_EXCLUDE */

#ifndef XQueryColors
VFUNC(int,XQueryColors,V_XQueryColors,_ANSI_ARGS_((Display *, Colormap, XColor *, int)))
#endif /* #ifndef XQueryColors */

#ifndef XQueryPointer
VFUNC(int,XQueryPointer,V_XQueryPointer,_ANSI_ARGS_((Display *, Window, Window *, Window *, int *, int *, int *, int *, unsigned int *)))
#endif /* #ifndef XQueryPointer */

#ifndef XQueryTree
VFUNC(int,XQueryTree,V_XQueryTree,_ANSI_ARGS_((Display *, Window, Window *, Window *, Window **, unsigned int *)))
#endif /* #ifndef XQueryTree */

#ifndef XRaiseWindow
VFUNC(int,XRaiseWindow,V_XRaiseWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XRaiseWindow */

#ifndef DO_X_EXCLUDE
#ifndef XReadBitmapFile
VFUNC(int,XReadBitmapFile,V_XReadBitmapFile,_ANSI_ARGS_((Display *, Drawable, const char *, unsigned int *, unsigned int *, Pixmap *, int *, int *)))
#endif /* #ifndef XReadBitmapFile */
#endif /* !DO_X_EXCLUDE */

#ifndef XRectInRegion
VFUNC(int,XRectInRegion,V_XRectInRegion,_ANSI_ARGS_((Region,int,int,unsigned,unsigned)))
#endif /* #ifndef XRectInRegion */

#ifndef XRefreshKeyboardMapping
VFUNC(int,XRefreshKeyboardMapping,V_XRefreshKeyboardMapping,_ANSI_ARGS_((XMappingEvent *)))
#endif /* #ifndef XRefreshKeyboardMapping */

#ifndef XResizeWindow
VFUNC(int,XResizeWindow,V_XResizeWindow,_ANSI_ARGS_((Display *, Window, unsigned int, unsigned int)))
#endif /* #ifndef XResizeWindow */

#ifndef XRootWindow
VFUNC(Window,XRootWindow,V_XRootWindow,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XRootWindow */

#ifndef XSelectInput
VFUNC(int,XSelectInput,V_XSelectInput,_ANSI_ARGS_((Display *, Window, long int)))
#endif /* #ifndef XSelectInput */

#ifndef XSendEvent
VFUNC(int,XSendEvent,V_XSendEvent,_ANSI_ARGS_((Display *, Window, int, long int, XEvent *)))
#endif /* #ifndef XSendEvent */

#ifndef XSetBackground
VFUNC(int,XSetBackground,V_XSetBackground,_ANSI_ARGS_((Display *, GC, unsigned long)))
#endif /* #ifndef XSetBackground */

#ifndef DO_X_EXCLUDE
#ifndef XSetClassHint
VFUNC(int,XSetClassHint,V_XSetClassHint,_ANSI_ARGS_((Display *, Window, XClassHint *)))
#endif /* #ifndef XSetClassHint */
#endif /* !DO_X_EXCLUDE */

#ifndef XSetClipMask
VFUNC(int,XSetClipMask,V_XSetClipMask,_ANSI_ARGS_((Display *, GC, Pixmap)))
#endif /* #ifndef XSetClipMask */

#ifndef XSetClipOrigin
VFUNC(int,XSetClipOrigin,V_XSetClipOrigin,_ANSI_ARGS_((Display *, GC, int, int)))
#endif /* #ifndef XSetClipOrigin */

#ifndef XSetCommand
VFUNC(int,XSetCommand,V_XSetCommand,_ANSI_ARGS_((Display *, Window, char **, int)))
#endif /* #ifndef XSetCommand */

#ifndef XSetDashes
VFUNC(int,XSetDashes,V_XSetDashes,_ANSI_ARGS_((Display *, GC, int, const char *, int)))
#endif /* #ifndef XSetDashes */

#ifndef XSetErrorHandler
VFUNC(XErrorHandler,XSetErrorHandler,V_XSetErrorHandler,_ANSI_ARGS_((XErrorHandler)))
#endif /* #ifndef XSetErrorHandler */

#ifndef XSetForeground
VFUNC(int,XSetForeground,V_XSetForeground,_ANSI_ARGS_((Display *, GC, long unsigned int)))
#endif /* #ifndef XSetForeground */

#ifndef XSetIconName
VFUNC(int,XSetIconName,V_XSetIconName,_ANSI_ARGS_((Display *, Window, const char *)))
#endif /* #ifndef XSetIconName */

#ifndef XSetInputFocus
VFUNC(int,XSetInputFocus,V_XSetInputFocus,_ANSI_ARGS_((Display *, Window, int, Time)))
#endif /* #ifndef XSetInputFocus */

#ifndef DO_X_EXCLUDE
#ifndef XSetRegion
VFUNC(int,XSetRegion,V_XSetRegion,_ANSI_ARGS_((Display *, GC, Region)))
#endif /* #ifndef XSetRegion */
#endif /* !DO_X_EXCLUDE */

#ifndef XSetSelectionOwner
VFUNC(int,XSetSelectionOwner,V_XSetSelectionOwner,_ANSI_ARGS_((Display *, Atom, Window, Time)))
#endif /* #ifndef XSetSelectionOwner */

#ifndef XSetTSOrigin
VFUNC(int,XSetTSOrigin,V_XSetTSOrigin,_ANSI_ARGS_((Display *, GC, int, int)))
#endif /* #ifndef XSetTSOrigin */

#ifndef DO_X_EXCLUDE
#ifndef XSetTransientForHint
VFUNC(int,XSetTransientForHint,V_XSetTransientForHint,_ANSI_ARGS_((Display *, Window, Window)))
#endif /* #ifndef XSetTransientForHint */
#endif /* !DO_X_EXCLUDE */

#ifndef XSetWMClientMachine
VFUNC(void,XSetWMClientMachine,V_XSetWMClientMachine,_ANSI_ARGS_((Display *, Window, XTextProperty *)))
#endif /* #ifndef XSetWMClientMachine */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMColormapWindows
VFUNC(int,XSetWMColormapWindows,V_XSetWMColormapWindows,_ANSI_ARGS_((Display *, Window, Window *, int)))
#endif /* #ifndef XSetWMColormapWindows */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMHints
VFUNC(int,XSetWMHints,V_XSetWMHints,_ANSI_ARGS_((Display *, Window, XWMHints *)))
#endif /* #ifndef XSetWMHints */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMName
VFUNC(void,XSetWMName,V_XSetWMName,_ANSI_ARGS_((Display *, Window, XTextProperty *)))
#endif /* #ifndef XSetWMName */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMNormalHints
VFUNC(void,XSetWMNormalHints,V_XSetWMNormalHints,_ANSI_ARGS_((Display *, Window, XSizeHints *)))
#endif /* #ifndef XSetWMNormalHints */
#endif /* !DO_X_EXCLUDE */

#ifndef XSetWindowBackground
VFUNC(int,XSetWindowBackground,V_XSetWindowBackground,_ANSI_ARGS_((Display *, Window, long unsigned int)))
#endif /* #ifndef XSetWindowBackground */

#ifndef XSetWindowBackgroundPixmap
VFUNC(int,XSetWindowBackgroundPixmap,V_XSetWindowBackgroundPixmap,_ANSI_ARGS_((Display *, Window, Pixmap)))
#endif /* #ifndef XSetWindowBackgroundPixmap */

#ifndef XSetWindowBorder
VFUNC(int,XSetWindowBorder,V_XSetWindowBorder,_ANSI_ARGS_((Display *, Window, long unsigned int)))
#endif /* #ifndef XSetWindowBorder */

#ifndef XSetWindowBorderPixmap
VFUNC(int,XSetWindowBorderPixmap,V_XSetWindowBorderPixmap,_ANSI_ARGS_((Display *, Window, Pixmap)))
#endif /* #ifndef XSetWindowBorderPixmap */

#ifndef XSetWindowBorderWidth
VFUNC(int,XSetWindowBorderWidth,V_XSetWindowBorderWidth,_ANSI_ARGS_((Display *, Window, unsigned int)))
#endif /* #ifndef XSetWindowBorderWidth */

#ifndef XSetWindowColormap
VFUNC(int,XSetWindowColormap,V_XSetWindowColormap,_ANSI_ARGS_((Display *, Window, Colormap)))
#endif /* #ifndef XSetWindowColormap */

#ifndef XStringListToTextProperty
VFUNC(int,XStringListToTextProperty,V_XStringListToTextProperty,_ANSI_ARGS_((char **, int, XTextProperty *)))
#endif /* #ifndef XStringListToTextProperty */

#ifndef XStringToKeysym
VFUNC(KeySym,XStringToKeysym,V_XStringToKeysym,_ANSI_ARGS_((const char *)))
#endif /* #ifndef XStringToKeysym */

#ifndef XSubtractRegion
VFUNC(int,XSubtractRegion,V_XSubtractRegion,_ANSI_ARGS_((Region, Region, Region)))
#endif /* #ifndef XSubtractRegion */

#ifndef XSync
VFUNC(int,XSync,V_XSync,_ANSI_ARGS_((Display *, int)))
#endif /* #ifndef XSync */

#ifndef DO_X_EXCLUDE
#ifndef XTextExtents
VFUNC(int,XTextExtents,V_XTextExtents,_ANSI_ARGS_((XFontStruct *, const char *, int, int *, int *, int *, XCharStruct *)))
#endif /* #ifndef XTextExtents */
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XTextWidth
VFUNC(int,XTextWidth,V_XTextWidth,_ANSI_ARGS_((XFontStruct *, const char *, int)))
#endif /* #ifndef XTextWidth */
#endif /* !DO_X_EXCLUDE */

#ifndef XTranslateCoordinates
VFUNC(int,XTranslateCoordinates,V_XTranslateCoordinates,_ANSI_ARGS_((Display *, Window, Window, int, int, int *, int *, Window *)))
#endif /* #ifndef XTranslateCoordinates */

#ifndef XUngrabKeyboard
VFUNC(int,XUngrabKeyboard,V_XUngrabKeyboard,_ANSI_ARGS_((Display *, Time)))
#endif /* #ifndef XUngrabKeyboard */

#ifndef XUngrabPointer
VFUNC(int,XUngrabPointer,V_XUngrabPointer,_ANSI_ARGS_((Display *, Time)))
#endif /* #ifndef XUngrabPointer */

#ifndef XUngrabServer
VFUNC(int,XUngrabServer,V_XUngrabServer,_ANSI_ARGS_((Display *)))
#endif /* #ifndef XUngrabServer */

#ifndef DO_X_EXCLUDE
#ifndef XUnionRectWithRegion
VFUNC(int,XUnionRectWithRegion,V_XUnionRectWithRegion,_ANSI_ARGS_((XRectangle *, Region, Region)))
#endif /* #ifndef XUnionRectWithRegion */
#endif /* !DO_X_EXCLUDE */

#ifndef XUnmapWindow
VFUNC(int,XUnmapWindow,V_XUnmapWindow,_ANSI_ARGS_((Display *, Window)))
#endif /* #ifndef XUnmapWindow */

#ifndef XVisualIDFromVisual
VFUNC(VisualID,XVisualIDFromVisual,V_XVisualIDFromVisual,_ANSI_ARGS_((Visual *)))
#endif /* #ifndef XVisualIDFromVisual */

#ifndef DO_X_EXCLUDE
#ifndef XWarpPointer
VFUNC(int,XWarpPointer,V_XWarpPointer,_ANSI_ARGS_(( Display *, Window, Window, int, int, unsigned int, unsigned int, int, int )))
#endif /* #ifndef XWarpPointer */
#endif /* !DO_X_EXCLUDE */

#ifndef XWindowEvent
VFUNC(int,XWindowEvent,V_XWindowEvent,_ANSI_ARGS_((Display *, Window, long int, XEvent *)))
#endif /* #ifndef XWindowEvent */

#ifndef XWithdrawWindow
VFUNC(int,XWithdrawWindow,V_XWithdrawWindow,_ANSI_ARGS_((Display *, Window, int)))
#endif /* #ifndef XWithdrawWindow */

#ifndef _XInitImageFuncPtrs
VFUNC(int,_XInitImageFuncPtrs,V__XInitImageFuncPtrs,_ANSI_ARGS_((XImage *image)))
#endif /* #ifndef _XInitImageFuncPtrs */

#endif /* _XLIB */
