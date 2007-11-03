#ifndef _TKINTDECLS_VM
#define _TKINTDECLS_VM
#include "tkIntDecls_f.h"
#ifndef NO_VTABLES
#ifndef TkAllocWindow
#  define TkAllocWindow (*TkintdeclsVptr->V_TkAllocWindow)
#endif

#ifndef TkBindDeadWindow
#  define TkBindDeadWindow (*TkintdeclsVptr->V_TkBindDeadWindow)
#endif

#ifndef TkBindEventProc
#  define TkBindEventProc (*TkintdeclsVptr->V_TkBindEventProc)
#endif

#ifndef TkBindFree
#  define TkBindFree (*TkintdeclsVptr->V_TkBindFree)
#endif

#ifndef TkBindInit
#  define TkBindInit (*TkintdeclsVptr->V_TkBindInit)
#endif

#ifndef TkChangeEventWindow
#  define TkChangeEventWindow (*TkintdeclsVptr->V_TkChangeEventWindow)
#endif

#ifndef TkClipBox
#  define TkClipBox (*TkintdeclsVptr->V_TkClipBox)
#endif

#ifndef TkClipCleanup
#  define TkClipCleanup (*TkintdeclsVptr->V_TkClipCleanup)
#endif

#ifndef TkClipInit
#  define TkClipInit (*TkintdeclsVptr->V_TkClipInit)
#endif

#ifndef TkComputeAnchor
#  define TkComputeAnchor (*TkintdeclsVptr->V_TkComputeAnchor)
#endif

#ifndef TkCopyAndGlobalEval
#  define TkCopyAndGlobalEval (*TkintdeclsVptr->V_TkCopyAndGlobalEval)
#endif

#ifndef TkCreateBindingProcedure
#  define TkCreateBindingProcedure (*TkintdeclsVptr->V_TkCreateBindingProcedure)
#endif

#ifndef TkCreateCursorFromData
#  define TkCreateCursorFromData (*TkintdeclsVptr->V_TkCreateCursorFromData)
#endif

#ifndef TkCreateFrame
#  define TkCreateFrame (*TkintdeclsVptr->V_TkCreateFrame)
#endif

#ifndef TkCreateMainWindow
#  define TkCreateMainWindow (*TkintdeclsVptr->V_TkCreateMainWindow)
#endif

#ifndef TkCreateRegion
#  define TkCreateRegion (*TkintdeclsVptr->V_TkCreateRegion)
#endif

#ifndef TkCurrentTime
#  define TkCurrentTime (*TkintdeclsVptr->V_TkCurrentTime)
#endif

#ifndef TkDebugBitmap
#  define TkDebugBitmap (*TkintdeclsVptr->V_TkDebugBitmap)
#endif

#ifndef TkDebugBorder
#  define TkDebugBorder (*TkintdeclsVptr->V_TkDebugBorder)
#endif

#ifndef TkDebugColor
#  define TkDebugColor (*TkintdeclsVptr->V_TkDebugColor)
#endif

#ifndef TkDebugConfig
#  define TkDebugConfig (*TkintdeclsVptr->V_TkDebugConfig)
#endif

#ifndef TkDebugCursor
#  define TkDebugCursor (*TkintdeclsVptr->V_TkDebugCursor)
#endif

#ifndef TkDebugFont
#  define TkDebugFont (*TkintdeclsVptr->V_TkDebugFont)
#endif

#ifndef TkDeleteAllImages
#  define TkDeleteAllImages (*TkintdeclsVptr->V_TkDeleteAllImages)
#endif

#ifndef TkDestroyRegion
#  define TkDestroyRegion (*TkintdeclsVptr->V_TkDestroyRegion)
#endif

#ifndef TkDoConfigureNotify
#  define TkDoConfigureNotify (*TkintdeclsVptr->V_TkDoConfigureNotify)
#endif

#ifndef TkDrawInsetFocusHighlight
#  define TkDrawInsetFocusHighlight (*TkintdeclsVptr->V_TkDrawInsetFocusHighlight)
#endif

#ifndef TkEventDeadWindow
#  define TkEventDeadWindow (*TkintdeclsVptr->V_TkEventDeadWindow)
#endif

#ifndef TkFindStateNum
#  define TkFindStateNum (*TkintdeclsVptr->V_TkFindStateNum)
#endif

#ifndef TkFindStateNumObj
#  define TkFindStateNumObj (*TkintdeclsVptr->V_TkFindStateNumObj)
#endif

#ifndef TkFindStateString
#  define TkFindStateString (*TkintdeclsVptr->V_TkFindStateString)
#endif

#ifndef TkFocusDeadWindow
#  define TkFocusDeadWindow (*TkintdeclsVptr->V_TkFocusDeadWindow)
#endif

#ifndef TkFocusFilterEvent
#  define TkFocusFilterEvent (*TkintdeclsVptr->V_TkFocusFilterEvent)
#endif

#ifndef TkFocusFree
#  define TkFocusFree (*TkintdeclsVptr->V_TkFocusFree)
#endif

#ifndef TkFocusKeyEvent
#  define TkFocusKeyEvent (*TkintdeclsVptr->V_TkFocusKeyEvent)
#endif

#ifndef TkFontPkgFree
#  define TkFontPkgFree (*TkintdeclsVptr->V_TkFontPkgFree)
#endif

#ifndef TkFontPkgInit
#  define TkFontPkgInit (*TkintdeclsVptr->V_TkFontPkgInit)
#endif

#ifndef TkFreeBindingTags
#  define TkFreeBindingTags (*TkintdeclsVptr->V_TkFreeBindingTags)
#endif

#ifndef TkGCCleanup
#  define TkGCCleanup (*TkintdeclsVptr->V_TkGCCleanup)
#endif

#ifndef TkGetBitmapData
#  define TkGetBitmapData (*TkintdeclsVptr->V_TkGetBitmapData)
#endif

#ifndef TkGetBitmapPredefTable
#  define TkGetBitmapPredefTable (*TkintdeclsVptr->V_TkGetBitmapPredefTable)
#endif

#ifndef TkGetCursorByName
#  define TkGetCursorByName (*TkintdeclsVptr->V_TkGetCursorByName)
#endif

#ifndef TkGetDefaultScreenName
#  define TkGetDefaultScreenName (*TkintdeclsVptr->V_TkGetDefaultScreenName)
#endif

#ifndef TkGetDisplay
#  define TkGetDisplay (*TkintdeclsVptr->V_TkGetDisplay)
#endif

#ifndef TkGetDisplayList
#  define TkGetDisplayList (*TkintdeclsVptr->V_TkGetDisplayList)
#endif

#ifndef TkGetDisplayOf
#  define TkGetDisplayOf (*TkintdeclsVptr->V_TkGetDisplayOf)
#endif

#ifndef TkGetFocusWin
#  define TkGetFocusWin (*TkintdeclsVptr->V_TkGetFocusWin)
#endif

#ifndef TkGetInterpNames
#  define TkGetInterpNames (*TkintdeclsVptr->V_TkGetInterpNames)
#endif

#ifndef TkGetMainInfoList
#  define TkGetMainInfoList (*TkintdeclsVptr->V_TkGetMainInfoList)
#endif

#ifndef TkGetOptionSpec
#  define TkGetOptionSpec (*TkintdeclsVptr->V_TkGetOptionSpec)
#endif

#ifndef TkGetPointerCoords
#  define TkGetPointerCoords (*TkintdeclsVptr->V_TkGetPointerCoords)
#endif

#ifndef TkGetServerInfo
#  define TkGetServerInfo (*TkintdeclsVptr->V_TkGetServerInfo)
#endif

#ifndef TkGetWindowFromObj
#  define TkGetWindowFromObj (*TkintdeclsVptr->V_TkGetWindowFromObj)
#endif

#ifndef TkGrabDeadWindow
#  define TkGrabDeadWindow (*TkintdeclsVptr->V_TkGrabDeadWindow)
#endif

#ifndef TkGrabState
#  define TkGrabState (*TkintdeclsVptr->V_TkGrabState)
#endif

#ifndef TkInOutEvents
#  define TkInOutEvents (*TkintdeclsVptr->V_TkInOutEvents)
#endif

#ifndef TkInstallFrameMenu
#  define TkInstallFrameMenu (*TkintdeclsVptr->V_TkInstallFrameMenu)
#endif

#ifndef TkIntersectRegion
#  define TkIntersectRegion (*TkintdeclsVptr->V_TkIntersectRegion)
#endif

#ifndef TkKeysymToString
#  define TkKeysymToString (*TkintdeclsVptr->V_TkKeysymToString)
#endif

#ifndef TkPhotoGetValidRegion
#  define TkPhotoGetValidRegion (*TkintdeclsVptr->V_TkPhotoGetValidRegion)
#endif

#ifndef TkPointerEvent
#  define TkPointerEvent (*TkintdeclsVptr->V_TkPointerEvent)
#endif

#ifndef TkPositionInTree
#  define TkPositionInTree (*TkintdeclsVptr->V_TkPositionInTree)
#endif

#ifndef TkQueueEventForAllChildren
#  define TkQueueEventForAllChildren (*TkintdeclsVptr->V_TkQueueEventForAllChildren)
#endif

#ifndef TkReadBitmapFile
#  define TkReadBitmapFile (*TkintdeclsVptr->V_TkReadBitmapFile)
#endif

#ifndef TkRectInRegion
#  define TkRectInRegion (*TkintdeclsVptr->V_TkRectInRegion)
#endif

#ifndef TkScrollWindow
#  define TkScrollWindow (*TkintdeclsVptr->V_TkScrollWindow)
#endif

#ifndef TkSelDeadWindow
#  define TkSelDeadWindow (*TkintdeclsVptr->V_TkSelDeadWindow)
#endif

#ifndef TkSelEventProc
#  define TkSelEventProc (*TkintdeclsVptr->V_TkSelEventProc)
#endif

#ifndef TkSelInit
#  define TkSelInit (*TkintdeclsVptr->V_TkSelInit)
#endif

#ifndef TkSelPropProc
#  define TkSelPropProc (*TkintdeclsVptr->V_TkSelPropProc)
#endif

#ifndef TkSetFocusWin
#  define TkSetFocusWin (*TkintdeclsVptr->V_TkSetFocusWin)
#endif

#ifndef TkSetRegion
#  define TkSetRegion (*TkintdeclsVptr->V_TkSetRegion)
#endif

#ifndef TkSetWindowMenuBar
#  define TkSetWindowMenuBar (*TkintdeclsVptr->V_TkSetWindowMenuBar)
#endif

#ifndef TkStringToKeysym
#  define TkStringToKeysym (*TkintdeclsVptr->V_TkStringToKeysym)
#endif

#ifndef TkStylePkgFree
#  define TkStylePkgFree (*TkintdeclsVptr->V_TkStylePkgFree)
#endif

#ifndef TkStylePkgInit
#  define TkStylePkgInit (*TkintdeclsVptr->V_TkStylePkgInit)
#endif

#ifndef TkSubtractRegion
#  define TkSubtractRegion (*TkintdeclsVptr->V_TkSubtractRegion)
#endif

#ifndef TkToplevelWindowForCommand
#  define TkToplevelWindowForCommand (*TkintdeclsVptr->V_TkToplevelWindowForCommand)
#endif

#ifndef TkUnionRectWithRegion
#  define TkUnionRectWithRegion (*TkintdeclsVptr->V_TkUnionRectWithRegion)
#endif

#ifndef TkWmAddToColormapWindows
#  define TkWmAddToColormapWindows (*TkintdeclsVptr->V_TkWmAddToColormapWindows)
#endif

#ifndef TkWmDeadWindow
#  define TkWmDeadWindow (*TkintdeclsVptr->V_TkWmDeadWindow)
#endif

#ifndef TkWmFocusToplevel
#  define TkWmFocusToplevel (*TkintdeclsVptr->V_TkWmFocusToplevel)
#endif

#ifndef TkWmMapWindow
#  define TkWmMapWindow (*TkintdeclsVptr->V_TkWmMapWindow)
#endif

#ifndef TkWmNewWindow
#  define TkWmNewWindow (*TkintdeclsVptr->V_TkWmNewWindow)
#endif

#ifndef TkWmProtocolEventProc
#  define TkWmProtocolEventProc (*TkintdeclsVptr->V_TkWmProtocolEventProc)
#endif

#ifndef TkWmRemoveFromColormapWindows
#  define TkWmRemoveFromColormapWindows (*TkintdeclsVptr->V_TkWmRemoveFromColormapWindows)
#endif

#ifndef TkWmRestackToplevel
#  define TkWmRestackToplevel (*TkintdeclsVptr->V_TkWmRestackToplevel)
#endif

#ifndef TkWmSetClass
#  define TkWmSetClass (*TkintdeclsVptr->V_TkWmSetClass)
#endif

#ifndef TkWmStackorderToplevel
#  define TkWmStackorderToplevel (*TkintdeclsVptr->V_TkWmStackorderToplevel)
#endif

#ifndef TkWmUnmapWindow
#  define TkWmUnmapWindow (*TkintdeclsVptr->V_TkWmUnmapWindow)
#endif

#ifndef TkpChangeFocus
#  define TkpChangeFocus (*TkintdeclsVptr->V_TkpChangeFocus)
#endif

#ifndef TkpClaimFocus
#  define TkpClaimFocus (*TkintdeclsVptr->V_TkpClaimFocus)
#endif

#ifndef TkpCloseDisplay
#  define TkpCloseDisplay (*TkintdeclsVptr->V_TkpCloseDisplay)
#endif

#ifndef TkpDisplayWarning
#  define TkpDisplayWarning (*TkintdeclsVptr->V_TkpDisplayWarning)
#endif

#ifndef TkpDrawHighlightBorder
#  define TkpDrawHighlightBorder (*TkintdeclsVptr->V_TkpDrawHighlightBorder)
#endif

#ifndef TkpFreeCursor
#  define TkpFreeCursor (*TkintdeclsVptr->V_TkpFreeCursor)
#endif

#ifndef TkpGetKeySym
#  define TkpGetKeySym (*TkintdeclsVptr->V_TkpGetKeySym)
#endif

#ifndef TkpGetOtherWindow
#  define TkpGetOtherWindow (*TkintdeclsVptr->V_TkpGetOtherWindow)
#endif

#ifndef TkpGetString
#  define TkpGetString (*TkintdeclsVptr->V_TkpGetString)
#endif

#ifndef TkpGetSubFonts
#  define TkpGetSubFonts (*TkintdeclsVptr->V_TkpGetSubFonts)
#endif

#ifndef TkpGetSystemDefault
#  define TkpGetSystemDefault (*TkintdeclsVptr->V_TkpGetSystemDefault)
#endif

#ifndef TkpGetWrapperWindow
#  define TkpGetWrapperWindow (*TkintdeclsVptr->V_TkpGetWrapperWindow)
#endif

#ifndef TkpInitKeymapInfo
#  define TkpInitKeymapInfo (*TkintdeclsVptr->V_TkpInitKeymapInfo)
#endif

#ifndef TkpInitializeMenuBindings
#  define TkpInitializeMenuBindings (*TkintdeclsVptr->V_TkpInitializeMenuBindings)
#endif

#ifndef TkpMakeContainer
#  define TkpMakeContainer (*TkintdeclsVptr->V_TkpMakeContainer)
#endif

#ifndef TkpMakeMenuWindow
#  define TkpMakeMenuWindow (*TkintdeclsVptr->V_TkpMakeMenuWindow)
#endif

#ifndef TkpMakeWindow
#  define TkpMakeWindow (*TkintdeclsVptr->V_TkpMakeWindow)
#endif

#ifndef TkpMenuNotifyToplevelCreate
#  define TkpMenuNotifyToplevelCreate (*TkintdeclsVptr->V_TkpMenuNotifyToplevelCreate)
#endif

#ifndef TkpMenuThreadInit
#  define TkpMenuThreadInit (*TkintdeclsVptr->V_TkpMenuThreadInit)
#endif

#ifndef TkpOpenDisplay
#  define TkpOpenDisplay (*TkintdeclsVptr->V_TkpOpenDisplay)
#endif

#ifndef TkpRedirectKeyEvent
#  define TkpRedirectKeyEvent (*TkintdeclsVptr->V_TkpRedirectKeyEvent)
#endif

#ifndef TkpSetKeycodeAndState
#  define TkpSetKeycodeAndState (*TkintdeclsVptr->V_TkpSetKeycodeAndState)
#endif

#ifndef TkpSetMainMenubar
#  define TkpSetMainMenubar (*TkintdeclsVptr->V_TkpSetMainMenubar)
#endif

#ifndef TkpUseWindow
#  define TkpUseWindow (*TkintdeclsVptr->V_TkpUseWindow)
#endif

#ifndef TkpWindowWasRecentlyDeleted
#  define TkpWindowWasRecentlyDeleted (*TkintdeclsVptr->V_TkpWindowWasRecentlyDeleted)
#endif

#endif /* NO_VTABLES */
#endif /* _TKINTDECLS_VM */
