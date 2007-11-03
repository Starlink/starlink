#ifndef _XLIB_VM
#define _XLIB_VM
#include "Xlib_f.h"
#ifndef NO_VTABLES
#if (defined(__WIN32__) || defined(__PM__)) && !defined(DO_X_EXCLUDE)
#  define DO_X_EXCLUDE
#endif
#ifndef DO_X_EXCLUDE
#ifndef XAllocClassHint
#  define XAllocClassHint (*XlibVptr->V_XAllocClassHint)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XAllocColor
#  define XAllocColor (*XlibVptr->V_XAllocColor)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XAllocNamedColor
#  define XAllocNamedColor (*XlibVptr->V_XAllocNamedColor)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XAllocSizeHints
#  define XAllocSizeHints (*XlibVptr->V_XAllocSizeHints)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XBell
#  define XBell (*XlibVptr->V_XBell)
#endif

#ifndef XChangeGC
#  define XChangeGC (*XlibVptr->V_XChangeGC)
#endif

#ifndef XChangeProperty
#  define XChangeProperty (*XlibVptr->V_XChangeProperty)
#endif

#ifndef XChangeWindowAttributes
#  define XChangeWindowAttributes (*XlibVptr->V_XChangeWindowAttributes)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XCheckIfEvent
#  define XCheckIfEvent (*XlibVptr->V_XCheckIfEvent)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XCheckWindowEvent
#  define XCheckWindowEvent (*XlibVptr->V_XCheckWindowEvent)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XClearWindow
#  define XClearWindow (*XlibVptr->V_XClearWindow)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XClipBox
#  define XClipBox (*XlibVptr->V_XClipBox)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XConfigureWindow
#  define XConfigureWindow (*XlibVptr->V_XConfigureWindow)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XConvertSelection
#  define XConvertSelection (*XlibVptr->V_XConvertSelection)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XCopyArea
#  define XCopyArea (*XlibVptr->V_XCopyArea)
#endif

#ifndef XCopyPlane
#  define XCopyPlane (*XlibVptr->V_XCopyPlane)
#endif

#ifndef XCreateBitmapFromData
#  define XCreateBitmapFromData (*XlibVptr->V_XCreateBitmapFromData)
#endif

#ifndef XCreateColormap
#  define XCreateColormap (*XlibVptr->V_XCreateColormap)
#endif

#ifndef XCreateGC
#  define XCreateGC (*XlibVptr->V_XCreateGC)
#endif

#ifndef XCreateGlyphCursor
#  define XCreateGlyphCursor (*XlibVptr->V_XCreateGlyphCursor)
#endif

#ifndef XCreateImage
#  define XCreateImage (*XlibVptr->V_XCreateImage)
#endif

#ifndef XCreatePixmapCursor
#  define XCreatePixmapCursor (*XlibVptr->V_XCreatePixmapCursor)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XCreateRegion
#  define XCreateRegion (*XlibVptr->V_XCreateRegion)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XCreateWindow
#  define XCreateWindow (*XlibVptr->V_XCreateWindow)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultColormap
#  define XDefaultColormap (*XlibVptr->V_XDefaultColormap)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultDepth
#  define XDefaultDepth (*XlibVptr->V_XDefaultDepth)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultScreen
#  define XDefaultScreen (*XlibVptr->V_XDefaultScreen)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XDefaultVisual
#  define XDefaultVisual (*XlibVptr->V_XDefaultVisual)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XDefineCursor
#  define XDefineCursor (*XlibVptr->V_XDefineCursor)
#endif

#ifndef XDeleteProperty
#  define XDeleteProperty (*XlibVptr->V_XDeleteProperty)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XDestroyRegion
#  define XDestroyRegion (*XlibVptr->V_XDestroyRegion)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XDestroyWindow
#  define XDestroyWindow (*XlibVptr->V_XDestroyWindow)
#endif

#ifndef XDrawArc
#  define XDrawArc (*XlibVptr->V_XDrawArc)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XDrawImageString
#  define XDrawImageString (*XlibVptr->V_XDrawImageString)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XDrawLine
#  define XDrawLine (*XlibVptr->V_XDrawLine)
#endif

#ifndef XDrawLines
#  define XDrawLines (*XlibVptr->V_XDrawLines)
#endif

#ifndef XDrawPoints
#  define XDrawPoints (*XlibVptr->V_XDrawPoints)
#endif

#ifndef XDrawRectangle
#  define XDrawRectangle (*XlibVptr->V_XDrawRectangle)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XDrawString
#  define XDrawString (*XlibVptr->V_XDrawString)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XEventsQueued
#  define XEventsQueued (*XlibVptr->V_XEventsQueued)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XFillArc
#  define XFillArc (*XlibVptr->V_XFillArc)
#endif

#ifndef XFillPolygon
#  define XFillPolygon (*XlibVptr->V_XFillPolygon)
#endif

#ifndef XFillRectangle
#  define XFillRectangle (*XlibVptr->V_XFillRectangle)
#endif

#ifndef XFillRectangles
#  define XFillRectangles (*XlibVptr->V_XFillRectangles)
#endif

#ifndef XFlush
#  define XFlush (*XlibVptr->V_XFlush)
#endif

#ifndef XFree
#  define XFree (*XlibVptr->V_XFree)
#endif

#ifndef XFreeColormap
#  define XFreeColormap (*XlibVptr->V_XFreeColormap)
#endif

#ifndef XFreeColors
#  define XFreeColors (*XlibVptr->V_XFreeColors)
#endif

#ifndef XFreeCursor
#  define XFreeCursor (*XlibVptr->V_XFreeCursor)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XFreeFont
#  define XFreeFont (*XlibVptr->V_XFreeFont)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XFreeFontNames
#  define XFreeFontNames (*XlibVptr->V_XFreeFontNames)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XFreeGC
#  define XFreeGC (*XlibVptr->V_XFreeGC)
#endif

#ifndef XFreeModifiermap
#  define XFreeModifiermap (*XlibVptr->V_XFreeModifiermap)
#endif

#ifndef XGContextFromGC
#  define XGContextFromGC (*XlibVptr->V_XGContextFromGC)
#endif

#ifndef XGetAtomName
#  define XGetAtomName (*XlibVptr->V_XGetAtomName)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XGetFontProperty
#  define XGetFontProperty (*XlibVptr->V_XGetFontProperty)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XGetGeometry
#  define XGetGeometry (*XlibVptr->V_XGetGeometry)
#endif

#ifndef XGetImage
#  define XGetImage (*XlibVptr->V_XGetImage)
#endif

#ifndef XGetInputFocus
#  define XGetInputFocus (*XlibVptr->V_XGetInputFocus)
#endif

#ifndef XGetModifierMapping
#  define XGetModifierMapping (*XlibVptr->V_XGetModifierMapping)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XGetSelectionOwner
#  define XGetSelectionOwner (*XlibVptr->V_XGetSelectionOwner)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XGetVisualInfo
#  define XGetVisualInfo (*XlibVptr->V_XGetVisualInfo)
#endif

#ifndef XGetWMColormapWindows
#  define XGetWMColormapWindows (*XlibVptr->V_XGetWMColormapWindows)
#endif

#ifndef XGetWindowAttributes
#  define XGetWindowAttributes (*XlibVptr->V_XGetWindowAttributes)
#endif

#ifndef XGetWindowProperty
#  define XGetWindowProperty (*XlibVptr->V_XGetWindowProperty)
#endif

#ifndef XGrabKeyboard
#  define XGrabKeyboard (*XlibVptr->V_XGrabKeyboard)
#endif

#ifndef XGrabPointer
#  define XGrabPointer (*XlibVptr->V_XGrabPointer)
#endif

#ifndef XGrabServer
#  define XGrabServer (*XlibVptr->V_XGrabServer)
#endif

#ifndef XIconifyWindow
#  define XIconifyWindow (*XlibVptr->V_XIconifyWindow)
#endif

#ifndef XInternAtom
#  define XInternAtom (*XlibVptr->V_XInternAtom)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XIntersectRegion
#  define XIntersectRegion (*XlibVptr->V_XIntersectRegion)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XKeycodeToKeysym
#  define XKeycodeToKeysym (*XlibVptr->V_XKeycodeToKeysym)
#endif

#ifndef XKeysymToString
#  define XKeysymToString (*XlibVptr->V_XKeysymToString)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XListFonts
#  define XListFonts (*XlibVptr->V_XListFonts)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XListHosts
#  define XListHosts (*XlibVptr->V_XListHosts)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XListProperties
#  define XListProperties (*XlibVptr->V_XListProperties)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XLoadFont
#  define XLoadFont (*XlibVptr->V_XLoadFont)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XLoadQueryFont
#  define XLoadQueryFont (*XlibVptr->V_XLoadQueryFont)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XLookupColor
#  define XLookupColor (*XlibVptr->V_XLookupColor)
#endif

#ifndef XLookupString
#  define XLookupString (*XlibVptr->V_XLookupString)
#endif

#ifndef XLowerWindow
#  define XLowerWindow (*XlibVptr->V_XLowerWindow)
#endif

#ifndef XMapWindow
#  define XMapWindow (*XlibVptr->V_XMapWindow)
#endif

#ifndef XMoveResizeWindow
#  define XMoveResizeWindow (*XlibVptr->V_XMoveResizeWindow)
#endif

#ifndef XMoveWindow
#  define XMoveWindow (*XlibVptr->V_XMoveWindow)
#endif

#ifndef XNextEvent
#  define XNextEvent (*XlibVptr->V_XNextEvent)
#endif

#ifndef XNoOp
#  define XNoOp (*XlibVptr->V_XNoOp)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XOpenDisplay
#  define XOpenDisplay (*XlibVptr->V_XOpenDisplay)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XParseColor
#  define XParseColor (*XlibVptr->V_XParseColor)
#endif

#ifndef XPutBackEvent
#  define XPutBackEvent (*XlibVptr->V_XPutBackEvent)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XPutImage
#  define XPutImage (*XlibVptr->V_XPutImage)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XQueryColors
#  define XQueryColors (*XlibVptr->V_XQueryColors)
#endif

#ifndef XQueryPointer
#  define XQueryPointer (*XlibVptr->V_XQueryPointer)
#endif

#ifndef XQueryTree
#  define XQueryTree (*XlibVptr->V_XQueryTree)
#endif

#ifndef XRaiseWindow
#  define XRaiseWindow (*XlibVptr->V_XRaiseWindow)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XReadBitmapFile
#  define XReadBitmapFile (*XlibVptr->V_XReadBitmapFile)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XRectInRegion
#  define XRectInRegion (*XlibVptr->V_XRectInRegion)
#endif

#ifndef XRefreshKeyboardMapping
#  define XRefreshKeyboardMapping (*XlibVptr->V_XRefreshKeyboardMapping)
#endif

#ifndef XResizeWindow
#  define XResizeWindow (*XlibVptr->V_XResizeWindow)
#endif

#ifndef XRootWindow
#  define XRootWindow (*XlibVptr->V_XRootWindow)
#endif

#ifndef XSelectInput
#  define XSelectInput (*XlibVptr->V_XSelectInput)
#endif

#ifndef XSendEvent
#  define XSendEvent (*XlibVptr->V_XSendEvent)
#endif

#ifndef XSetBackground
#  define XSetBackground (*XlibVptr->V_XSetBackground)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XSetClassHint
#  define XSetClassHint (*XlibVptr->V_XSetClassHint)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XSetClipMask
#  define XSetClipMask (*XlibVptr->V_XSetClipMask)
#endif

#ifndef XSetClipOrigin
#  define XSetClipOrigin (*XlibVptr->V_XSetClipOrigin)
#endif

#ifndef XSetCommand
#  define XSetCommand (*XlibVptr->V_XSetCommand)
#endif

#ifndef XSetDashes
#  define XSetDashes (*XlibVptr->V_XSetDashes)
#endif

#ifndef XSetErrorHandler
#  define XSetErrorHandler (*XlibVptr->V_XSetErrorHandler)
#endif

#ifndef XSetForeground
#  define XSetForeground (*XlibVptr->V_XSetForeground)
#endif

#ifndef XSetIconName
#  define XSetIconName (*XlibVptr->V_XSetIconName)
#endif

#ifndef XSetInputFocus
#  define XSetInputFocus (*XlibVptr->V_XSetInputFocus)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XSetRegion
#  define XSetRegion (*XlibVptr->V_XSetRegion)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XSetSelectionOwner
#  define XSetSelectionOwner (*XlibVptr->V_XSetSelectionOwner)
#endif

#ifndef XSetTSOrigin
#  define XSetTSOrigin (*XlibVptr->V_XSetTSOrigin)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XSetTransientForHint
#  define XSetTransientForHint (*XlibVptr->V_XSetTransientForHint)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XSetWMClientMachine
#  define XSetWMClientMachine (*XlibVptr->V_XSetWMClientMachine)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XSetWMColormapWindows
#  define XSetWMColormapWindows (*XlibVptr->V_XSetWMColormapWindows)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMHints
#  define XSetWMHints (*XlibVptr->V_XSetWMHints)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMName
#  define XSetWMName (*XlibVptr->V_XSetWMName)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XSetWMNormalHints
#  define XSetWMNormalHints (*XlibVptr->V_XSetWMNormalHints)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XSetWindowBackground
#  define XSetWindowBackground (*XlibVptr->V_XSetWindowBackground)
#endif

#ifndef XSetWindowBackgroundPixmap
#  define XSetWindowBackgroundPixmap (*XlibVptr->V_XSetWindowBackgroundPixmap)
#endif

#ifndef XSetWindowBorder
#  define XSetWindowBorder (*XlibVptr->V_XSetWindowBorder)
#endif

#ifndef XSetWindowBorderPixmap
#  define XSetWindowBorderPixmap (*XlibVptr->V_XSetWindowBorderPixmap)
#endif

#ifndef XSetWindowBorderWidth
#  define XSetWindowBorderWidth (*XlibVptr->V_XSetWindowBorderWidth)
#endif

#ifndef XSetWindowColormap
#  define XSetWindowColormap (*XlibVptr->V_XSetWindowColormap)
#endif

#ifndef XStringListToTextProperty
#  define XStringListToTextProperty (*XlibVptr->V_XStringListToTextProperty)
#endif

#ifndef XStringToKeysym
#  define XStringToKeysym (*XlibVptr->V_XStringToKeysym)
#endif

#ifndef XSubtractRegion
#  define XSubtractRegion (*XlibVptr->V_XSubtractRegion)
#endif

#ifndef XSync
#  define XSync (*XlibVptr->V_XSync)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XTextExtents
#  define XTextExtents (*XlibVptr->V_XTextExtents)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef DO_X_EXCLUDE
#ifndef XTextWidth
#  define XTextWidth (*XlibVptr->V_XTextWidth)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XTranslateCoordinates
#  define XTranslateCoordinates (*XlibVptr->V_XTranslateCoordinates)
#endif

#ifndef XUngrabKeyboard
#  define XUngrabKeyboard (*XlibVptr->V_XUngrabKeyboard)
#endif

#ifndef XUngrabPointer
#  define XUngrabPointer (*XlibVptr->V_XUngrabPointer)
#endif

#ifndef XUngrabServer
#  define XUngrabServer (*XlibVptr->V_XUngrabServer)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XUnionRectWithRegion
#  define XUnionRectWithRegion (*XlibVptr->V_XUnionRectWithRegion)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XUnmapWindow
#  define XUnmapWindow (*XlibVptr->V_XUnmapWindow)
#endif

#ifndef XVisualIDFromVisual
#  define XVisualIDFromVisual (*XlibVptr->V_XVisualIDFromVisual)
#endif

#ifndef DO_X_EXCLUDE
#ifndef XWarpPointer
#  define XWarpPointer (*XlibVptr->V_XWarpPointer)
#endif
#endif /* !DO_X_EXCLUDE */

#ifndef XWindowEvent
#  define XWindowEvent (*XlibVptr->V_XWindowEvent)
#endif

#ifndef XWithdrawWindow
#  define XWithdrawWindow (*XlibVptr->V_XWithdrawWindow)
#endif

#ifndef _XInitImageFuncPtrs
#  define _XInitImageFuncPtrs (*XlibVptr->V__XInitImageFuncPtrs)
#endif

#endif /* NO_VTABLES */
#endif /* _XLIB_VM */
