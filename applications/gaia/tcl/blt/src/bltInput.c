/*
 * bltInput.c --
 *
 *	This module implements InputOnly class windows for the BLT toolkit.
 *
 * Copyright 1991-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 */

#include "bltInt.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#ifdef XNQueryInputStyle
#define TK_USE_INPUT_METHODS
#endif

struct TkWindow;
struct TkIdStack;
struct TkErrorHandler;
struct TkSelectionInfo;
struct TkSelHandler;
struct TkMainInfo;
struct TkClipboardTarget;
struct TkWmInfo;
typedef struct _TkMainInfo TkMainInfo;
typedef struct _TkGrabEvent TkGrabEvent;
typedef struct _TkColormap TkColormap;
typedef struct _TkStressedCmap TkStressedCmap;
typedef struct _TkEventHandler TkEventHandler;

/*
 * One of the following structures is maintained for each display
 * containing a window managed by Tk:
 */

typedef struct TkDisplay {
    Display *display;		/* Xlib's info about display. */
    struct TkDisplay *nextPtr;	/* Next in list of all displays. */
    char *name;			/* Name of display (with any screen
				 * identifier removed).  Malloc-ed. */
    Time lastEventTime;		/* Time of last event received for this
				 * display. */

    /*
     * Information used primarily by tkBind.c:
     */

    int bindInfoStale;		/* Non-zero means the variables in this
				 * part of the structure are potentially
				 * incorrect and should be recomputed. */
    unsigned int modeModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to "mode shift".  If no
				 * such modifier, than this is zero. */
    unsigned int metaModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to the "Meta" key.  If no
				 * such modifier, then this is zero. */
    unsigned int altModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to the "Meta" key.  If no
				 * such modifier, then this is zero. */
    enum {
	LU_IGNORE, LU_CAPS, LU_SHIFT
    } lockUsage;
    /* Indicates how to interpret lock modifier. */
    int numModKeyCodes;		/* Number of entries in modKeyCodes array
				 * below. */
    KeyCode *modKeyCodes;	/* Pointer to an array giving keycodes for
				 * all of the keys that have modifiers
				 * associated with them.  Malloc'ed, but
				 * may be NULL. */

    /*
     * Information used by tkError.c only:
     */

    struct TkErrorHandler *errorPtr;
    /* First in list of error handlers
				 * for this display.  NULL means
				 * no handlers exist at present. */
    int deleteCount;		/* Counts # of handlers deleted since
				 * last time inactive handlers were
				 * garbage-collected.  When this number
				 * gets big, handlers get cleaned up. */

    /*
     * Information used by tkSend.c only:
     */

    Tk_Window commTkwin;	/* Window used for communication
				 * between interpreters during "send"
				 * commands.  NULL means send info hasn't
				 * been initialized yet. */
    Atom commProperty;		/* X's name for comm property. */
    Atom registryProperty;	/* X's name for property containing
				 * registry of interpreter names. */
    Atom appNameProperty;	/* X's name for property used to hold the
				 * application name on each comm window. */

    /*
     * Information used by tkSelect.c and tkClipboard.c only:
     */

    struct TkSelectionInfo *selectionInfoPtr;
    /* First in list of selection information
				 * records.  Each entry contains information
				 * about the current owner of a particular
				 * selection on this display. */
    Atom multipleAtom;		/* Atom for MULTIPLE.  None means
				 * selection stuff isn't initialized. */
    Atom incrAtom;		/* Atom for INCR. */
    Atom targetsAtom;		/* Atom for TARGETS. */
    Atom timestampAtom;		/* Atom for TIMESTAMP. */
    Atom textAtom;		/* Atom for TEXT. */
    Atom compoundTextAtom;	/* Atom for COMPOUND_TEXT. */
    Atom applicationAtom;	/* Atom for TK_APPLICATION. */
    Atom windowAtom;		/* Atom for TK_WINDOW. */
    Atom clipboardAtom;		/* Atom for CLIPBOARD. */

    Tk_Window clipWindow;	/* Window used for clipboard ownership and to
				 * retrieve selections between processes. NULL
				 * means clipboard info hasn't been
				 * initialized. */
    int clipboardActive;	/* 1 means we currently own the clipboard
				 * selection, 0 means we don't. */
    struct TkMainInfo *clipboardAppPtr;
    /* Last application that owned clipboard. */
    struct TkClipboardTarget *clipTargetPtr;
    /* First in list of clipboard type information
				 * records.  Each entry contains information
				 * about the buffers for a given selection
				 * target. */

    /*
     * Information used by tkAtom.c only:
     */

    int atomInit;		/* 0 means stuff below hasn't been
				 * initialized yet. */
    Tcl_HashTable nameTable;	/* Maps from names to Atom's. */
    Tcl_HashTable atomTable;	/* Maps from Atom's back to names. */

    /*
     * Information used by tkCursor.c only:
     */

    Font cursorFont;		/* Font to use for standard cursors.
				 * None means font not loaded yet. */

    /*
     * Information used by tkGrab.c only:
     */

    struct TkWindow *grabWinPtr;
    /* Window in which the pointer is currently
				 * grabbed, or NULL if none. */
    struct TkWindow *eventualGrabWinPtr;
    /* Value that grabWinPtr will have once the
				 * grab event queue (below) has been
				 * completely emptied. */
    struct TkWindow *buttonWinPtr;
    /* Window in which first mouse button was
				 * pressed while grab was in effect, or NULL
				 * if no such press in effect. */
    struct TkWindow *serverWinPtr;
    /* If no application contains the pointer then
				 * this is NULL.  Otherwise it contains the
				 * last window for which we've gotten an
				 * Enter or Leave event from the server (i.e.
				 * the last window known to have contained
				 * the pointer).  Doesn't reflect events
				 * that were synthesized in tkGrab.c. */
    TkGrabEvent *firstGrabEventPtr;
    /* First in list of enter/leave events
				 * synthesized by grab code.  These events
				 * must be processed in order before any other
				 * events are processed.  NULL means no such
				 * events. */
    TkGrabEvent *lastGrabEventPtr;
    /* Last in list of synthesized events, or NULL
				 * if list is empty. */
    int grabFlags;		/* Miscellaneous flag values.  See definitions
				 * in tkGrab.c. */

    /*
     * Information used by tkXId.c only:
     */

    struct TkIdStack *idStackPtr;
    /* First in list of chunks of free resource
				 * identifiers, or NULL if there are no free
				 * resources. */
              XID(*defaultAllocProc) _ANSI_ARGS_((Display *display));
    /* Default resource allocator for display. */
    struct TkIdStack *windowStackPtr;
    /* First in list of chunks of window
				 * identifers that can't be reused right
				 * now. */
    int idCleanupScheduled;	/* 1 means a call to WindowIdCleanup has
				 * already been scheduled, 0 means it
				 * hasn't. */

    /*
     * Information maintained by tkWindow.c for use later on by tkXId.c:
     */


    int destroyCount;		/* Number of Tk_DestroyWindow operations
				 * in progress. */
    unsigned long lastDestroyRequest;
    /* Id of most recent XDestroyWindow request;
				 * can re-use ids in windowStackPtr when
				 * server has seen this request and event
				 * queue is empty. */

    /*
     * Information used by tkVisual.c only:
     */

    TkColormap *cmapPtr;	/* First in list of all non-default colormaps
				 * allocated for this display. */

    /*
     * Information used by tkFocus.c only:
     */
#if (TK_MAJOR_VERSION < 8)

    struct TkWindow *focusWinPtr;
    /* Window that currently has the focus for
				 * this display, or NULL if none. */
    struct TkWindow *implicitWinPtr;
    /* If the focus arrived at a toplevel window
				 * implicitly via an Enter event (rather
				 * than via a FocusIn event), this points
				 * to the toplevel window.  Otherwise it is
				 * NULL. */
    struct TkWindow *focusOnMapPtr;
    /* This points to a toplevel window that is
				 * supposed to receive the X input focus as
				 * soon as it is mapped (needed to handle the
				 * fact that X won't allow the focus on an
				 * unmapped window).  NULL means no delayed
				 * focus op in progress. */
    int forceFocus;		/* Associated with focusOnMapPtr:  non-zero
				 * means claim the focus even if some other
				 * application currently has it. */
#else
    struct TkWindow *implicitWinPtr;
    /* If the focus arrived at a toplevel window
				 * implicitly via an Enter event (rather
				 * than via a FocusIn event), this points
				 * to the toplevel window.  Otherwise it is
				 * NULL. */
#if (TK_MAJOR_VERSION > 8) || (TK_RELEASE_SERIAL > 0)
    struct TkWindow *focusPtr;	/* Points to the window on this display that
				 * should be receiving keyboard events.  When
				 * multiple applications on the display have
				 * the focus, this will refer to the
				 * innermost window in the innermost
				 * application.  This information isn't used
				 * under Unix or Windows, but it's needed on
				 * the Macintosh. */
#endif /* (TK_MAJOR_VERSION > 8) || (TK_RELEASE_SERIAL > 0) */
#endif /* TK_MAJOR_VERSION < 8 */

    /*
     * Used by tkColor.c only:
     */

    TkStressedCmap *stressPtr;	/* First in list of colormaps that have
				 * filled up, so we have to pick an
				 * approximate color. */

#if !((TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION == 0))
    /*
     * Used by tkEvent.c only:
     */

    struct TkWindowEvent *delayedMotionPtr;
    /* Points to a malloc-ed motion event
				 * whose processing has been delayed in
				 * the hopes that another motion event
				 * will come along right away and we can
				 * merge the two of them together.  NULL
				 * means that there is no delayed motion
				 * event. */
#endif /* !TK_MAJOR_VERSION  == 4 && TK_MINOR_VERSION == 0 */
    /*
     * Miscellaneous information:
     */

#ifdef TK_USE_INPUT_METHODS
    XIM inputMethod;		/* Input method for this display */
#endif /* TK_USE_INPUT_METHODS */
    Tcl_HashTable winTable;	/* Maps from X window ids to TkWindow ptrs. */
#if (TK_MAJOR_VERSION >= 8)
    int refCount;		/* Reference count of how many Tk applications
                                 * are using this display. Used to clean up
                                 * the display when we no longer have any
                                 * Tk applications using it.
                                 */
#endif /* TK_MAJOR_VERSION >= 8 */

} TkDisplay;

/*
 *----------------------------------------------------------------------
 *
 * DoConfigureNotify --
 *
 *	Generate a ConfigureNotify event describing the current
 *	configuration of a window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	An event is generated and processed by Tk_HandleEvent.
 *
 *----------------------------------------------------------------------
 */
static void
DoConfigureNotify(winPtr)
    Tk_FakeWin *winPtr;		/* Window whose configuration was just changed. */
{
    XEvent event;

    event.type = ConfigureNotify;
    event.xconfigure.serial = LastKnownRequestProcessed(winPtr->display);
    event.xconfigure.send_event = False;
    event.xconfigure.display = winPtr->display;
    event.xconfigure.event = winPtr->window;
    event.xconfigure.window = winPtr->window;
    event.xconfigure.x = winPtr->changes.x;
    event.xconfigure.y = winPtr->changes.y;
    event.xconfigure.width = winPtr->changes.width;
    event.xconfigure.height = winPtr->changes.height;
    event.xconfigure.border_width = winPtr->changes.border_width;
    if (winPtr->changes.stack_mode == Above) {
	event.xconfigure.above = winPtr->changes.sibling;
    } else {
	event.xconfigure.above = None;
    }
    event.xconfigure.override_redirect = winPtr->atts.override_redirect;
    Tk_HandleEvent(&event);
}

/*
 *--------------------------------------------------------------
 *
 * Blt_MakeInputOnlyWindowExist --
 *
 *	Like Tk_MakeWindowExist but instead creates a transparent
 *	window to block for user events from sibling windows.
 *
 *	Differences with Tk_MakeWindowExist.
 *
 *	1. This is always a "busy" window. There's never a
 *	   platform-specific class procedure to execute instead.
 *	2. The window is transparent and never will contain children,
 *	   so colormap information is irrelevant.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the procedure returns, the internal window associated
 *	with tkwin is guaranteed to exist.  This may require the
 *	window's ancestors to be created too.
 *
 *--------------------------------------------------------------
 */
void
Blt_MakeInputOnlyWindowExist(tkwin)
    Tk_Window tkwin;		/* Token for window. */
{
    Tk_FakeWin *winPtr = (Tk_FakeWin *) tkwin;
    Tk_FakeWin *winPtr2;
    Window parent;
    Tcl_HashEntry *hPtr;
    int notUsed;
    TkDisplay *dispPtr;
#ifdef WIN32
    HWND parentHWND;
    int style;
    DWORD exStyle;
    HWND hwnd;
#endif /* WIN32 */

#define nextPtr		dummy4
#define dirtyChanges 	dummy6
#define dirtyAtts 	dummy7
#define inputContext 	dummy9

    if (winPtr->window != None) {
	return;
    }
    if ((winPtr->parentPtr == NULL) || (winPtr->flags & TK_TOP_LEVEL)) {
	parent = XRootWindow(winPtr->display, winPtr->screenNum);
    } else {
	if (Tk_WindowId(winPtr->parentPtr) == None) {
	    Tk_MakeWindowExist(winPtr->parentPtr);
	}
	parent = Tk_WindowId(winPtr->parentPtr);
    }

    /*
     * ---------------------------------------------------------
     *
     * Create the window, insuring that it is at the top of the
     * stacking order.
     *
     * ---------------------------------------------------------
     */
#ifdef WIN32
    parentHWND = Tk_GetHWND(parent);
    style = (WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS);
    exStyle = (WS_EX_TRANSPARENT | WS_EX_TOPMOST);
#define TK_WIN_CHILD_CLASS_NAME "TkChild"
    hwnd = CreateWindowEx(exStyle, TK_WIN_CHILD_CLASS_NAME, NULL, style,
	Tk_X(tkwin), Tk_Y(tkwin), Tk_Width(tkwin), Tk_Height(tkwin),
	parentHWND, NULL, Tk_GetHINSTANCE(), NULL);
    SetWindowPos(hwnd, HWND_TOP, 0, 0, 0, 0,
	(SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE));
    winPtr->window = Tk_AttachHWND(tkwin, hwnd);
#else
    /*
     * Ignore the important events while the window is mapped.
     */
    winPtr->atts.do_not_propagate_mask = (KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask);
    winPtr->atts.event_mask = (KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask);
    winPtr->changes.border_width = 0;
    winPtr->depth = 0;
    winPtr->window = XCreateWindow(winPtr->display, parent,
	winPtr->changes.x, winPtr->changes.y,
	(unsigned)winPtr->changes.width,	/* width */
	(unsigned)winPtr->changes.height,	/* height */
	0,			/* border_width */
	0,			/* depth */
	InputOnly,		/* class */
	CopyFromParent,		/* visual */
	(CWDontPropagate | CWEventMask),	/* valuemask */
	&(winPtr->atts) /* attributes */ );
#endif /* WIN32 */

    dispPtr = (TkDisplay *)winPtr->dummy1;
    hPtr = Tcl_CreateHashEntry(&(dispPtr->winTable), (char *)winPtr->window,
	&notUsed);
    Tcl_SetHashValue(hPtr, winPtr);

    winPtr->dirtyAtts = 0;
    winPtr->dirtyChanges = 0;
#ifdef TK_USE_INPUT_METHODS
    winPtr->inputContext = NULL;
#endif /* TK_USE_INPUT_METHODS */

    if (!(winPtr->flags & TK_TOP_LEVEL)) {
	/*
	 * If any siblings higher up in the stacking order have already
	 * been created then move this window to its rightful position
	 * in the stacking order.
	 *
	 * NOTE: this code ignores any changes anyone might have made
	 * to the sibling and stack_mode field of the window's attributes,
	 * so it really isn't safe for these to be manipulated except
	 * by calling Tk_RestackWindow.
	 */

	for (winPtr2 = (Tk_FakeWin *) winPtr->nextPtr; winPtr2 != NULL;
	    winPtr2 = (Tk_FakeWin *) winPtr2->nextPtr) {
	    if ((winPtr2->window != None) && !(winPtr2->flags & TK_TOP_LEVEL)) {
		XWindowChanges changes;
		changes.sibling = winPtr2->window;
		changes.stack_mode = Below;
		XConfigureWindow(winPtr->display, winPtr->window,
		    CWSibling | CWStackMode, &changes);
		break;
	    }
	}

#ifdef notdef
	/* Colormap information is irrelevant for an InputOnly window */

	/*
	 * If this window has a different colormap than its parent, add
	 * the window to the WM_COLORMAP_WINDOWS property for its top-level.
	 */

	if ((winPtr->parentPtr != NULL) &&
	    (winPtr->atts.colormap != winPtr->parentPtr->atts.colormap)) {
	    TkWmAddToColormapWindows(winPtr);
	}
#endif
    }
    /*
     * Issue a ConfigureNotify event if there were deferred configuration
     * changes (but skip it if the window is being deleted;  the
     * ConfigureNotify event could cause problems if we're being called
     * from Tk_DestroyWindow under some conditions).
     */

    if ((winPtr->flags & TK_NEED_CONFIG_NOTIFY)
	&& !(winPtr->flags & TK_ALREADY_DEAD)) {
	winPtr->flags &= ~TK_NEED_CONFIG_NOTIFY;
	DoConfigureNotify((Tk_FakeWin *) tkwin);
    }
}
