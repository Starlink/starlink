#ifndef _TKDECLS_VM
#define _TKDECLS_VM
#include "tkDecls_f.h"
#ifndef NO_VTABLES
#ifndef Tk_3DBorderColor
#  define Tk_3DBorderColor (*TkdeclsVptr->V_Tk_3DBorderColor)
#endif

#ifndef Tk_3DBorderGC
#  define Tk_3DBorderGC (*TkdeclsVptr->V_Tk_3DBorderGC)
#endif

#ifndef Tk_3DHorizontalBevel
#  define Tk_3DHorizontalBevel (*TkdeclsVptr->V_Tk_3DHorizontalBevel)
#endif

#ifndef Tk_3DVerticalBevel
#  define Tk_3DVerticalBevel (*TkdeclsVptr->V_Tk_3DVerticalBevel)
#endif

#ifndef Tk_Alloc3DBorderFromObj
#  define Tk_Alloc3DBorderFromObj (*TkdeclsVptr->V_Tk_Alloc3DBorderFromObj)
#endif

#ifndef Tk_AllocBitmapFromObj
#  define Tk_AllocBitmapFromObj (*TkdeclsVptr->V_Tk_AllocBitmapFromObj)
#endif

#ifndef Tk_AllocColorFromObj
#  define Tk_AllocColorFromObj (*TkdeclsVptr->V_Tk_AllocColorFromObj)
#endif

#ifndef Tk_AllocCursorFromObj
#  define Tk_AllocCursorFromObj (*TkdeclsVptr->V_Tk_AllocCursorFromObj)
#endif

#ifndef Tk_AllocFontFromObj
#  define Tk_AllocFontFromObj (*TkdeclsVptr->V_Tk_AllocFontFromObj)
#endif

#ifndef Tk_AllocStyleFromObj
#  define Tk_AllocStyleFromObj (*TkdeclsVptr->V_Tk_AllocStyleFromObj)
#endif

#ifndef Tk_BindEvent
#  define Tk_BindEvent (*TkdeclsVptr->V_Tk_BindEvent)
#endif

#ifndef Tk_ChangeWindowAttributes
#  define Tk_ChangeWindowAttributes (*TkdeclsVptr->V_Tk_ChangeWindowAttributes)
#endif

#ifndef Tk_CharBbox
#  define Tk_CharBbox (*TkdeclsVptr->V_Tk_CharBbox)
#endif

#ifndef Tk_ClearSelection
#  define Tk_ClearSelection (*TkdeclsVptr->V_Tk_ClearSelection)
#endif

#ifndef Tk_ClipboardAppend
#  define Tk_ClipboardAppend (*TkdeclsVptr->V_Tk_ClipboardAppend)
#endif

#ifndef Tk_ClipboardClear
#  define Tk_ClipboardClear (*TkdeclsVptr->V_Tk_ClipboardClear)
#endif

#ifndef Tk_CollapseMotionEvents
#  define Tk_CollapseMotionEvents (*TkdeclsVptr->V_Tk_CollapseMotionEvents)
#endif

#ifndef Tk_ComputeTextLayout
#  define Tk_ComputeTextLayout (*TkdeclsVptr->V_Tk_ComputeTextLayout)
#endif

#ifndef Tk_ConfigureInfo
#  define Tk_ConfigureInfo (*TkdeclsVptr->V_Tk_ConfigureInfo)
#endif

#ifndef Tk_ConfigureValue
#  define Tk_ConfigureValue (*TkdeclsVptr->V_Tk_ConfigureValue)
#endif

#ifndef Tk_ConfigureWidget
#  define Tk_ConfigureWidget (*TkdeclsVptr->V_Tk_ConfigureWidget)
#endif

#ifndef Tk_ConfigureWindow
#  define Tk_ConfigureWindow (*TkdeclsVptr->V_Tk_ConfigureWindow)
#endif

#ifndef Tk_CoordsToWindow
#  define Tk_CoordsToWindow (*TkdeclsVptr->V_Tk_CoordsToWindow)
#endif

#ifndef Tk_CreateAnonymousWindow
#  define Tk_CreateAnonymousWindow (*TkdeclsVptr->V_Tk_CreateAnonymousWindow)
#endif

#ifndef Tk_CreateBinding
#  define Tk_CreateBinding (*TkdeclsVptr->V_Tk_CreateBinding)
#endif

#ifndef Tk_CreateBindingTable
#  define Tk_CreateBindingTable (*TkdeclsVptr->V_Tk_CreateBindingTable)
#endif

#ifndef Tk_CreateClientMessageHandler
#  define Tk_CreateClientMessageHandler (*TkdeclsVptr->V_Tk_CreateClientMessageHandler)
#endif

#ifndef Tk_CreateErrorHandler
#  define Tk_CreateErrorHandler (*TkdeclsVptr->V_Tk_CreateErrorHandler)
#endif

#ifndef Tk_CreateEventHandler
#  define Tk_CreateEventHandler (*TkdeclsVptr->V_Tk_CreateEventHandler)
#endif

#ifndef Tk_CreateGenericHandler
#  define Tk_CreateGenericHandler (*TkdeclsVptr->V_Tk_CreateGenericHandler)
#endif

#ifndef Tk_CreateImageType
#  define Tk_CreateImageType (*TkdeclsVptr->V_Tk_CreateImageType)
#endif

#ifndef Tk_CreateOptionTable
#  define Tk_CreateOptionTable (*TkdeclsVptr->V_Tk_CreateOptionTable)
#endif

#ifndef Tk_CreateSelHandler
#  define Tk_CreateSelHandler (*TkdeclsVptr->V_Tk_CreateSelHandler)
#endif

#ifndef Tk_CreateStyle
#  define Tk_CreateStyle (*TkdeclsVptr->V_Tk_CreateStyle)
#endif

#ifndef Tk_CreateWindow
#  define Tk_CreateWindow (*TkdeclsVptr->V_Tk_CreateWindow)
#endif

#ifndef Tk_CreateWindowFromPath
#  define Tk_CreateWindowFromPath (*TkdeclsVptr->V_Tk_CreateWindowFromPath)
#endif

#ifndef Tk_DefineBitmap
#  define Tk_DefineBitmap (*TkdeclsVptr->V_Tk_DefineBitmap)
#endif

#ifndef Tk_DefineCursor
#  define Tk_DefineCursor (*TkdeclsVptr->V_Tk_DefineCursor)
#endif

#ifndef Tk_DeleteAllBindings
#  define Tk_DeleteAllBindings (*TkdeclsVptr->V_Tk_DeleteAllBindings)
#endif

#ifndef Tk_DeleteBinding
#  define Tk_DeleteBinding (*TkdeclsVptr->V_Tk_DeleteBinding)
#endif

#ifndef Tk_DeleteBindingTable
#  define Tk_DeleteBindingTable (*TkdeclsVptr->V_Tk_DeleteBindingTable)
#endif

#ifndef Tk_DeleteClientMessageHandler
#  define Tk_DeleteClientMessageHandler (*TkdeclsVptr->V_Tk_DeleteClientMessageHandler)
#endif

#ifndef Tk_DeleteErrorHandler
#  define Tk_DeleteErrorHandler (*TkdeclsVptr->V_Tk_DeleteErrorHandler)
#endif

#ifndef Tk_DeleteEventHandler
#  define Tk_DeleteEventHandler (*TkdeclsVptr->V_Tk_DeleteEventHandler)
#endif

#ifndef Tk_DeleteGenericHandler
#  define Tk_DeleteGenericHandler (*TkdeclsVptr->V_Tk_DeleteGenericHandler)
#endif

#ifndef Tk_DeleteImage
#  define Tk_DeleteImage (*TkdeclsVptr->V_Tk_DeleteImage)
#endif

#ifndef Tk_DeleteOptionTable
#  define Tk_DeleteOptionTable (*TkdeclsVptr->V_Tk_DeleteOptionTable)
#endif

#ifndef Tk_DeleteSelHandler
#  define Tk_DeleteSelHandler (*TkdeclsVptr->V_Tk_DeleteSelHandler)
#endif

#ifndef Tk_DestroyWindow
#  define Tk_DestroyWindow (*TkdeclsVptr->V_Tk_DestroyWindow)
#endif

#ifndef Tk_DisplayName
#  define Tk_DisplayName (*TkdeclsVptr->V_Tk_DisplayName)
#endif

#ifndef Tk_DistanceToTextLayout
#  define Tk_DistanceToTextLayout (*TkdeclsVptr->V_Tk_DistanceToTextLayout)
#endif

#ifndef Tk_Draw3DPolygon
#  define Tk_Draw3DPolygon (*TkdeclsVptr->V_Tk_Draw3DPolygon)
#endif

#ifndef Tk_Draw3DRectangle
#  define Tk_Draw3DRectangle (*TkdeclsVptr->V_Tk_Draw3DRectangle)
#endif

#ifndef Tk_DrawChars
#  define Tk_DrawChars (*TkdeclsVptr->V_Tk_DrawChars)
#endif

#ifndef Tk_DrawElement
#  define Tk_DrawElement (*TkdeclsVptr->V_Tk_DrawElement)
#endif

#ifndef Tk_DrawFocusHighlight
#  define Tk_DrawFocusHighlight (*TkdeclsVptr->V_Tk_DrawFocusHighlight)
#endif

#ifndef Tk_DrawTextLayout
#  define Tk_DrawTextLayout (*TkdeclsVptr->V_Tk_DrawTextLayout)
#endif

#ifndef Tk_Fill3DPolygon
#  define Tk_Fill3DPolygon (*TkdeclsVptr->V_Tk_Fill3DPolygon)
#endif

#ifndef Tk_Fill3DRectangle
#  define Tk_Fill3DRectangle (*TkdeclsVptr->V_Tk_Fill3DRectangle)
#endif

#ifndef Tk_FontId
#  define Tk_FontId (*TkdeclsVptr->V_Tk_FontId)
#endif

#ifndef Tk_Free3DBorder
#  define Tk_Free3DBorder (*TkdeclsVptr->V_Tk_Free3DBorder)
#endif

#ifndef Tk_Free3DBorderFromObj
#  define Tk_Free3DBorderFromObj (*TkdeclsVptr->V_Tk_Free3DBorderFromObj)
#endif

#ifndef Tk_FreeBitmap
#  define Tk_FreeBitmap (*TkdeclsVptr->V_Tk_FreeBitmap)
#endif

#ifndef Tk_FreeBitmapFromObj
#  define Tk_FreeBitmapFromObj (*TkdeclsVptr->V_Tk_FreeBitmapFromObj)
#endif

#ifndef Tk_FreeColor
#  define Tk_FreeColor (*TkdeclsVptr->V_Tk_FreeColor)
#endif

#ifndef Tk_FreeColorFromObj
#  define Tk_FreeColorFromObj (*TkdeclsVptr->V_Tk_FreeColorFromObj)
#endif

#ifndef Tk_FreeColormap
#  define Tk_FreeColormap (*TkdeclsVptr->V_Tk_FreeColormap)
#endif

#ifndef Tk_FreeConfigOptions
#  define Tk_FreeConfigOptions (*TkdeclsVptr->V_Tk_FreeConfigOptions)
#endif

#ifndef Tk_FreeCursor
#  define Tk_FreeCursor (*TkdeclsVptr->V_Tk_FreeCursor)
#endif

#ifndef Tk_FreeCursorFromObj
#  define Tk_FreeCursorFromObj (*TkdeclsVptr->V_Tk_FreeCursorFromObj)
#endif

#ifndef Tk_FreeFont
#  define Tk_FreeFont (*TkdeclsVptr->V_Tk_FreeFont)
#endif

#ifndef Tk_FreeFontFromObj
#  define Tk_FreeFontFromObj (*TkdeclsVptr->V_Tk_FreeFontFromObj)
#endif

#ifndef Tk_FreeGC
#  define Tk_FreeGC (*TkdeclsVptr->V_Tk_FreeGC)
#endif

#ifndef Tk_FreeImage
#  define Tk_FreeImage (*TkdeclsVptr->V_Tk_FreeImage)
#endif

#ifndef Tk_FreeOptions
#  define Tk_FreeOptions (*TkdeclsVptr->V_Tk_FreeOptions)
#endif

#ifndef Tk_FreePixmap
#  define Tk_FreePixmap (*TkdeclsVptr->V_Tk_FreePixmap)
#endif

#ifndef Tk_FreeSavedOptions
#  define Tk_FreeSavedOptions (*TkdeclsVptr->V_Tk_FreeSavedOptions)
#endif

#ifndef Tk_FreeStyle
#  define Tk_FreeStyle (*TkdeclsVptr->V_Tk_FreeStyle)
#endif

#ifndef Tk_FreeStyleFromObj
#  define Tk_FreeStyleFromObj (*TkdeclsVptr->V_Tk_FreeStyleFromObj)
#endif

#ifndef Tk_FreeTextLayout
#  define Tk_FreeTextLayout (*TkdeclsVptr->V_Tk_FreeTextLayout)
#endif

#ifndef Tk_FreeXId
#  define Tk_FreeXId (*TkdeclsVptr->V_Tk_FreeXId)
#endif

#ifndef Tk_GCForColor
#  define Tk_GCForColor (*TkdeclsVptr->V_Tk_GCForColor)
#endif

#ifndef Tk_GeometryRequest
#  define Tk_GeometryRequest (*TkdeclsVptr->V_Tk_GeometryRequest)
#endif

#ifndef Tk_Get3DBorder
#  define Tk_Get3DBorder (*TkdeclsVptr->V_Tk_Get3DBorder)
#endif

#ifndef Tk_Get3DBorderFromObj
#  define Tk_Get3DBorderFromObj (*TkdeclsVptr->V_Tk_Get3DBorderFromObj)
#endif

#ifndef Tk_GetAllBindings
#  define Tk_GetAllBindings (*TkdeclsVptr->V_Tk_GetAllBindings)
#endif

#ifndef Tk_GetAnchor
#  define Tk_GetAnchor (*TkdeclsVptr->V_Tk_GetAnchor)
#endif

#ifndef Tk_GetAnchorFromObj
#  define Tk_GetAnchorFromObj (*TkdeclsVptr->V_Tk_GetAnchorFromObj)
#endif

#ifndef Tk_GetAtomName
#  define Tk_GetAtomName (*TkdeclsVptr->V_Tk_GetAtomName)
#endif

#ifndef Tk_GetBinding
#  define Tk_GetBinding (*TkdeclsVptr->V_Tk_GetBinding)
#endif

#ifndef Tk_GetBitmap
#  define Tk_GetBitmap (*TkdeclsVptr->V_Tk_GetBitmap)
#endif

#ifndef Tk_GetBitmapFromData
#  define Tk_GetBitmapFromData (*TkdeclsVptr->V_Tk_GetBitmapFromData)
#endif

#ifndef Tk_GetBitmapFromObj
#  define Tk_GetBitmapFromObj (*TkdeclsVptr->V_Tk_GetBitmapFromObj)
#endif

#ifndef Tk_GetCapStyle
#  define Tk_GetCapStyle (*TkdeclsVptr->V_Tk_GetCapStyle)
#endif

#ifndef Tk_GetColor
#  define Tk_GetColor (*TkdeclsVptr->V_Tk_GetColor)
#endif

#ifndef Tk_GetColorByValue
#  define Tk_GetColorByValue (*TkdeclsVptr->V_Tk_GetColorByValue)
#endif

#ifndef Tk_GetColorFromObj
#  define Tk_GetColorFromObj (*TkdeclsVptr->V_Tk_GetColorFromObj)
#endif

#ifndef Tk_GetColormap
#  define Tk_GetColormap (*TkdeclsVptr->V_Tk_GetColormap)
#endif

#ifndef Tk_GetCursor
#  define Tk_GetCursor (*TkdeclsVptr->V_Tk_GetCursor)
#endif

#ifndef Tk_GetCursorFromData
#  define Tk_GetCursorFromData (*TkdeclsVptr->V_Tk_GetCursorFromData)
#endif

#ifndef Tk_GetCursorFromObj
#  define Tk_GetCursorFromObj (*TkdeclsVptr->V_Tk_GetCursorFromObj)
#endif

#ifndef Tk_GetElementBorderWidth
#  define Tk_GetElementBorderWidth (*TkdeclsVptr->V_Tk_GetElementBorderWidth)
#endif

#ifndef Tk_GetElementBox
#  define Tk_GetElementBox (*TkdeclsVptr->V_Tk_GetElementBox)
#endif

#ifndef Tk_GetElementId
#  define Tk_GetElementId (*TkdeclsVptr->V_Tk_GetElementId)
#endif

#ifndef Tk_GetElementSize
#  define Tk_GetElementSize (*TkdeclsVptr->V_Tk_GetElementSize)
#endif

#ifndef Tk_GetFont
#  define Tk_GetFont (*TkdeclsVptr->V_Tk_GetFont)
#endif

#ifndef Tk_GetFontFromObj
#  define Tk_GetFontFromObj (*TkdeclsVptr->V_Tk_GetFontFromObj)
#endif

#ifndef Tk_GetFontMetrics
#  define Tk_GetFontMetrics (*TkdeclsVptr->V_Tk_GetFontMetrics)
#endif

#ifndef Tk_GetGC
#  define Tk_GetGC (*TkdeclsVptr->V_Tk_GetGC)
#endif

#ifndef Tk_GetImage
#  define Tk_GetImage (*TkdeclsVptr->V_Tk_GetImage)
#endif

#ifndef Tk_GetImageMasterData
#  define Tk_GetImageMasterData (*TkdeclsVptr->V_Tk_GetImageMasterData)
#endif

#ifndef Tk_GetJoinStyle
#  define Tk_GetJoinStyle (*TkdeclsVptr->V_Tk_GetJoinStyle)
#endif

#ifndef Tk_GetJustify
#  define Tk_GetJustify (*TkdeclsVptr->V_Tk_GetJustify)
#endif

#ifndef Tk_GetJustifyFromObj
#  define Tk_GetJustifyFromObj (*TkdeclsVptr->V_Tk_GetJustifyFromObj)
#endif

#ifndef Tk_GetMMFromObj
#  define Tk_GetMMFromObj (*TkdeclsVptr->V_Tk_GetMMFromObj)
#endif

#ifndef Tk_GetNumMainWindows
#  define Tk_GetNumMainWindows (*TkdeclsVptr->V_Tk_GetNumMainWindows)
#endif

#ifndef Tk_GetOptionInfo
#  define Tk_GetOptionInfo (*TkdeclsVptr->V_Tk_GetOptionInfo)
#endif

#ifndef Tk_GetOptionValue
#  define Tk_GetOptionValue (*TkdeclsVptr->V_Tk_GetOptionValue)
#endif

#ifndef Tk_GetPixels
#  define Tk_GetPixels (*TkdeclsVptr->V_Tk_GetPixels)
#endif

#ifndef Tk_GetPixelsFromObj
#  define Tk_GetPixelsFromObj (*TkdeclsVptr->V_Tk_GetPixelsFromObj)
#endif

#ifndef Tk_GetPixmap
#  define Tk_GetPixmap (*TkdeclsVptr->V_Tk_GetPixmap)
#endif

#ifndef Tk_GetRelief
#  define Tk_GetRelief (*TkdeclsVptr->V_Tk_GetRelief)
#endif

#ifndef Tk_GetReliefFromObj
#  define Tk_GetReliefFromObj (*TkdeclsVptr->V_Tk_GetReliefFromObj)
#endif

#ifndef Tk_GetRootCoords
#  define Tk_GetRootCoords (*TkdeclsVptr->V_Tk_GetRootCoords)
#endif

#ifndef Tk_GetScreenMM
#  define Tk_GetScreenMM (*TkdeclsVptr->V_Tk_GetScreenMM)
#endif

#ifndef Tk_GetScrollInfo
#  define Tk_GetScrollInfo (*TkdeclsVptr->V_Tk_GetScrollInfo)
#endif

#ifndef Tk_GetScrollInfoObj
#  define Tk_GetScrollInfoObj (*TkdeclsVptr->V_Tk_GetScrollInfoObj)
#endif

#ifndef Tk_GetSelection
#  define Tk_GetSelection (*TkdeclsVptr->V_Tk_GetSelection)
#endif

#ifndef Tk_GetStyle
#  define Tk_GetStyle (*TkdeclsVptr->V_Tk_GetStyle)
#endif

#ifndef Tk_GetStyleEngine
#  define Tk_GetStyleEngine (*TkdeclsVptr->V_Tk_GetStyleEngine)
#endif

#ifndef Tk_GetStyleFromObj
#  define Tk_GetStyleFromObj (*TkdeclsVptr->V_Tk_GetStyleFromObj)
#endif

#ifndef Tk_GetStyledElement
#  define Tk_GetStyledElement (*TkdeclsVptr->V_Tk_GetStyledElement)
#endif

#ifndef Tk_GetUid
#  define Tk_GetUid (*TkdeclsVptr->V_Tk_GetUid)
#endif

#ifndef Tk_GetVRootGeometry
#  define Tk_GetVRootGeometry (*TkdeclsVptr->V_Tk_GetVRootGeometry)
#endif

#ifndef Tk_GetVisual
#  define Tk_GetVisual (*TkdeclsVptr->V_Tk_GetVisual)
#endif

#ifndef Tk_Grab
#  define Tk_Grab (*TkdeclsVptr->V_Tk_Grab)
#endif

#ifndef Tk_HandleEvent
#  define Tk_HandleEvent (*TkdeclsVptr->V_Tk_HandleEvent)
#endif

#ifndef Tk_IdToWindow
#  define Tk_IdToWindow (*TkdeclsVptr->V_Tk_IdToWindow)
#endif

#ifndef Tk_ImageChanged
#  define Tk_ImageChanged (*TkdeclsVptr->V_Tk_ImageChanged)
#endif

#ifndef Tk_InitOptions
#  define Tk_InitOptions (*TkdeclsVptr->V_Tk_InitOptions)
#endif

#ifndef Tk_InternAtom
#  define Tk_InternAtom (*TkdeclsVptr->V_Tk_InternAtom)
#endif

#ifndef Tk_IntersectTextLayout
#  define Tk_IntersectTextLayout (*TkdeclsVptr->V_Tk_IntersectTextLayout)
#endif

#ifndef Tk_MainLoop
#  define Tk_MainLoop (*TkdeclsVptr->V_Tk_MainLoop)
#endif

#ifndef Tk_MainWindow
#  define Tk_MainWindow (*TkdeclsVptr->V_Tk_MainWindow)
#endif

#ifndef Tk_MaintainGeometry
#  define Tk_MaintainGeometry (*TkdeclsVptr->V_Tk_MaintainGeometry)
#endif

#ifndef Tk_MakeWindowExist
#  define Tk_MakeWindowExist (*TkdeclsVptr->V_Tk_MakeWindowExist)
#endif

#ifndef Tk_ManageGeometry
#  define Tk_ManageGeometry (*TkdeclsVptr->V_Tk_ManageGeometry)
#endif

#ifndef Tk_MapWindow
#  define Tk_MapWindow (*TkdeclsVptr->V_Tk_MapWindow)
#endif

#ifndef Tk_MeasureChars
#  define Tk_MeasureChars (*TkdeclsVptr->V_Tk_MeasureChars)
#endif

#ifndef Tk_MoveResizeWindow
#  define Tk_MoveResizeWindow (*TkdeclsVptr->V_Tk_MoveResizeWindow)
#endif

#ifndef Tk_MoveToplevelWindow
#  define Tk_MoveToplevelWindow (*TkdeclsVptr->V_Tk_MoveToplevelWindow)
#endif

#ifndef Tk_MoveWindow
#  define Tk_MoveWindow (*TkdeclsVptr->V_Tk_MoveWindow)
#endif

#ifndef Tk_NameOf3DBorder
#  define Tk_NameOf3DBorder (*TkdeclsVptr->V_Tk_NameOf3DBorder)
#endif

#ifndef Tk_NameOfAnchor
#  define Tk_NameOfAnchor (*TkdeclsVptr->V_Tk_NameOfAnchor)
#endif

#ifndef Tk_NameOfBitmap
#  define Tk_NameOfBitmap (*TkdeclsVptr->V_Tk_NameOfBitmap)
#endif

#ifndef Tk_NameOfCapStyle
#  define Tk_NameOfCapStyle (*TkdeclsVptr->V_Tk_NameOfCapStyle)
#endif

#ifndef Tk_NameOfColor
#  define Tk_NameOfColor (*TkdeclsVptr->V_Tk_NameOfColor)
#endif

#ifndef Tk_NameOfCursor
#  define Tk_NameOfCursor (*TkdeclsVptr->V_Tk_NameOfCursor)
#endif

#ifndef Tk_NameOfFont
#  define Tk_NameOfFont (*TkdeclsVptr->V_Tk_NameOfFont)
#endif

#ifndef Tk_NameOfImage
#  define Tk_NameOfImage (*TkdeclsVptr->V_Tk_NameOfImage)
#endif

#ifndef Tk_NameOfJoinStyle
#  define Tk_NameOfJoinStyle (*TkdeclsVptr->V_Tk_NameOfJoinStyle)
#endif

#ifndef Tk_NameOfJustify
#  define Tk_NameOfJustify (*TkdeclsVptr->V_Tk_NameOfJustify)
#endif

#ifndef Tk_NameOfRelief
#  define Tk_NameOfRelief (*TkdeclsVptr->V_Tk_NameOfRelief)
#endif

#ifndef Tk_NameOfStyle
#  define Tk_NameOfStyle (*TkdeclsVptr->V_Tk_NameOfStyle)
#endif

#ifndef Tk_NameToWindow
#  define Tk_NameToWindow (*TkdeclsVptr->V_Tk_NameToWindow)
#endif

#ifndef Tk_OwnSelection
#  define Tk_OwnSelection (*TkdeclsVptr->V_Tk_OwnSelection)
#endif

#ifndef Tk_PointToChar
#  define Tk_PointToChar (*TkdeclsVptr->V_Tk_PointToChar)
#endif

#ifndef Tk_PostscriptBitmap
#  define Tk_PostscriptBitmap (*TkdeclsVptr->V_Tk_PostscriptBitmap)
#endif

#ifndef Tk_PostscriptColor
#  define Tk_PostscriptColor (*TkdeclsVptr->V_Tk_PostscriptColor)
#endif

#ifndef Tk_PostscriptFont
#  define Tk_PostscriptFont (*TkdeclsVptr->V_Tk_PostscriptFont)
#endif

#ifndef Tk_PostscriptFontName
#  define Tk_PostscriptFontName (*TkdeclsVptr->V_Tk_PostscriptFontName)
#endif

#ifndef Tk_PostscriptImage
#  define Tk_PostscriptImage (*TkdeclsVptr->V_Tk_PostscriptImage)
#endif

#ifndef Tk_PostscriptPath
#  define Tk_PostscriptPath (*TkdeclsVptr->V_Tk_PostscriptPath)
#endif

#ifndef Tk_PostscriptPhoto
#  define Tk_PostscriptPhoto (*TkdeclsVptr->V_Tk_PostscriptPhoto)
#endif

#ifndef Tk_PostscriptStipple
#  define Tk_PostscriptStipple (*TkdeclsVptr->V_Tk_PostscriptStipple)
#endif

#ifndef Tk_PostscriptY
#  define Tk_PostscriptY (*TkdeclsVptr->V_Tk_PostscriptY)
#endif

#ifndef Tk_PreserveColormap
#  define Tk_PreserveColormap (*TkdeclsVptr->V_Tk_PreserveColormap)
#endif

#ifndef Tk_QueueWindowEvent
#  define Tk_QueueWindowEvent (*TkdeclsVptr->V_Tk_QueueWindowEvent)
#endif

#ifndef Tk_RedrawImage
#  define Tk_RedrawImage (*TkdeclsVptr->V_Tk_RedrawImage)
#endif

#ifndef Tk_RegisterStyleEngine
#  define Tk_RegisterStyleEngine (*TkdeclsVptr->V_Tk_RegisterStyleEngine)
#endif

#ifndef Tk_RegisterStyledElement
#  define Tk_RegisterStyledElement (*TkdeclsVptr->V_Tk_RegisterStyledElement)
#endif

#ifndef Tk_ResizeWindow
#  define Tk_ResizeWindow (*TkdeclsVptr->V_Tk_ResizeWindow)
#endif

#ifndef Tk_RestackWindow
#  define Tk_RestackWindow (*TkdeclsVptr->V_Tk_RestackWindow)
#endif

#ifndef Tk_RestoreSavedOptions
#  define Tk_RestoreSavedOptions (*TkdeclsVptr->V_Tk_RestoreSavedOptions)
#endif

#ifndef Tk_RestrictEvents
#  define Tk_RestrictEvents (*TkdeclsVptr->V_Tk_RestrictEvents)
#endif

#ifndef Tk_SetAppName
#  define Tk_SetAppName (*TkdeclsVptr->V_Tk_SetAppName)
#endif

#ifndef Tk_SetBackgroundFromBorder
#  define Tk_SetBackgroundFromBorder (*TkdeclsVptr->V_Tk_SetBackgroundFromBorder)
#endif

#ifndef Tk_SetCaretPos
#  define Tk_SetCaretPos (*TkdeclsVptr->V_Tk_SetCaretPos)
#endif

#ifndef Tk_SetClass
#  define Tk_SetClass (*TkdeclsVptr->V_Tk_SetClass)
#endif

#ifndef Tk_SetClassProcs
#  define Tk_SetClassProcs (*TkdeclsVptr->V_Tk_SetClassProcs)
#endif

#ifndef Tk_SetGrid
#  define Tk_SetGrid (*TkdeclsVptr->V_Tk_SetGrid)
#endif

#ifndef Tk_SetInternalBorder
#  define Tk_SetInternalBorder (*TkdeclsVptr->V_Tk_SetInternalBorder)
#endif

#ifndef Tk_SetInternalBorderEx
#  define Tk_SetInternalBorderEx (*TkdeclsVptr->V_Tk_SetInternalBorderEx)
#endif

#ifndef Tk_SetMinimumRequestSize
#  define Tk_SetMinimumRequestSize (*TkdeclsVptr->V_Tk_SetMinimumRequestSize)
#endif

#ifndef Tk_SetOptions
#  define Tk_SetOptions (*TkdeclsVptr->V_Tk_SetOptions)
#endif

#ifndef Tk_SetTSOrigin
#  define Tk_SetTSOrigin (*TkdeclsVptr->V_Tk_SetTSOrigin)
#endif

#ifndef Tk_SetWindowBackground
#  define Tk_SetWindowBackground (*TkdeclsVptr->V_Tk_SetWindowBackground)
#endif

#ifndef Tk_SetWindowBackgroundPixmap
#  define Tk_SetWindowBackgroundPixmap (*TkdeclsVptr->V_Tk_SetWindowBackgroundPixmap)
#endif

#ifndef Tk_SetWindowBorder
#  define Tk_SetWindowBorder (*TkdeclsVptr->V_Tk_SetWindowBorder)
#endif

#ifndef Tk_SetWindowBorderPixmap
#  define Tk_SetWindowBorderPixmap (*TkdeclsVptr->V_Tk_SetWindowBorderPixmap)
#endif

#ifndef Tk_SetWindowBorderWidth
#  define Tk_SetWindowBorderWidth (*TkdeclsVptr->V_Tk_SetWindowBorderWidth)
#endif

#ifndef Tk_SetWindowColormap
#  define Tk_SetWindowColormap (*TkdeclsVptr->V_Tk_SetWindowColormap)
#endif

#ifndef Tk_SetWindowVisual
#  define Tk_SetWindowVisual (*TkdeclsVptr->V_Tk_SetWindowVisual)
#endif

#ifndef Tk_SizeOfBitmap
#  define Tk_SizeOfBitmap (*TkdeclsVptr->V_Tk_SizeOfBitmap)
#endif

#ifndef Tk_SizeOfImage
#  define Tk_SizeOfImage (*TkdeclsVptr->V_Tk_SizeOfImage)
#endif

#ifndef Tk_StrictMotif
#  define Tk_StrictMotif (*TkdeclsVptr->V_Tk_StrictMotif)
#endif

#ifndef Tk_TextLayoutToPostscript
#  define Tk_TextLayoutToPostscript (*TkdeclsVptr->V_Tk_TextLayoutToPostscript)
#endif

#ifndef Tk_TextWidth
#  define Tk_TextWidth (*TkdeclsVptr->V_Tk_TextWidth)
#endif

#ifndef Tk_UndefineCursor
#  define Tk_UndefineCursor (*TkdeclsVptr->V_Tk_UndefineCursor)
#endif

#ifndef Tk_UnderlineChars
#  define Tk_UnderlineChars (*TkdeclsVptr->V_Tk_UnderlineChars)
#endif

#ifndef Tk_UnderlineTextLayout
#  define Tk_UnderlineTextLayout (*TkdeclsVptr->V_Tk_UnderlineTextLayout)
#endif

#ifndef Tk_Ungrab
#  define Tk_Ungrab (*TkdeclsVptr->V_Tk_Ungrab)
#endif

#ifndef Tk_UnmaintainGeometry
#  define Tk_UnmaintainGeometry (*TkdeclsVptr->V_Tk_UnmaintainGeometry)
#endif

#ifndef Tk_UnmapWindow
#  define Tk_UnmapWindow (*TkdeclsVptr->V_Tk_UnmapWindow)
#endif

#ifndef Tk_UnsetGrid
#  define Tk_UnsetGrid (*TkdeclsVptr->V_Tk_UnsetGrid)
#endif

#ifndef Tk_UpdatePointer
#  define Tk_UpdatePointer (*TkdeclsVptr->V_Tk_UpdatePointer)
#endif

#endif /* NO_VTABLES */
#endif /* _TKDECLS_VM */
