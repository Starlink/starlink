/*
 * bltTile.c --
 *
 *	This module manages images for tiled backgrounds for the BLT toolkit.
 *
 * Copyright 1995-1998 Lucent Technologies, Inc.
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
#include "bltImage.h"
#include <X11/Xutil.h>

#define TILE_MAGIC ((unsigned int) 0x46170277)

static Tcl_HashTable tileTable;
static int initialized = 0;

typedef struct {
    Tk_Uid nameId;		/* Identifier of image from which the
				 * tile was generated. */
    Display *display;		/* Display where pixmap was created */
    int flags;			/* See definitions below */
    Tk_Window tkwin;
    Tcl_Interp *interp;
    Tcl_HashEntry *hashPtr;	/* Pointer to hash table location */

    Pixmap pixmap;		/* Pixmap generated from image */
    Tk_Image tkImage;		/* Token of image */
    Blt_List clients;		/* List of clients sharing this tile */

} TileServer;

typedef struct {
    Tk_Uid nameId;		/* Identifier of image from which the
				 * tile was generated. */
    Display *display;
} TileKey;

#define NOTIFY_PENDING	1	/* If set, indicates that the image
				 * associated with the tile has been
				 * updated or deleted.  The tile pixmap
				 * will be changed and the clients of the
				 * tile will be notified (if they supplied
				 * a TileChangedProc routine. */
typedef struct {
    unsigned int magic;
    Blt_TileChangedProc *changeProc;
    /* If non-NULL, routine to
				 * call to when tile image changes. */
    ClientData clientData;	/* Data to pass to when calling the above
				 * routine */
    TileServer *serverPtr;	/* Pointer to actual tile information */
    Blt_ListItem item;		/* Pointer to client entry in the server's
				 * client list.  Used to delete the client */
} Tile;

static int StringToTile _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int flags));
static char *TileToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltTileOption =
{
    StringToTile, TileToString, (ClientData)0
};

#ifdef __STDC__
static Tcl_IdleProc RedrawTile;
static Tk_ImageChangedProc ImageChangedProc;
#endif

/*
 *----------------------------------------------------------------------
 *
 * RedrawTile
 *
 *	It would be better if Tk checked for NULL proc pointers.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
RedrawTile(clientData)
    ClientData clientData;
{
    TileServer *serverPtr = (TileServer *) clientData;
    Tile *tilePtr;
    Blt_ListItem item;

    serverPtr->flags &= ~NOTIFY_PENDING;
    if (Blt_TkImageDeleted(serverPtr->tkImage)) {
	if (serverPtr->pixmap != None) {
	    Tk_FreePixmap(serverPtr->display, serverPtr->pixmap);
	}
	serverPtr->pixmap = None;
    } else {
	int width, height;
	Pixmap pixmap;
	Window root;

	root = RootWindow(serverPtr->display,
	    Tk_ScreenNumber(serverPtr->tkwin));
	/*
	 * Create the new pixmap *before* destroying the old one.  I
	 * don't why this happens, but if you delete the old pixmap
	 * first, the old pixmap sometimes gets used in the client's
	 * GCs.  I suspect it has something to do with the way Tk
	 * reallocates X resource identifiers.
	 */
	Tk_SizeOfImage(serverPtr->tkImage, &width, &height);
	pixmap = Tk_GetPixmap(serverPtr->display, root, width, height,
	    Tk_Depth(serverPtr->tkwin));
	if (serverPtr->pixmap != None) {
	    Tk_FreePixmap(serverPtr->display, serverPtr->pixmap);
	}
	serverPtr->pixmap = pixmap;
	Tk_RedrawImage(serverPtr->tkImage, 0, 0, width, height,
	    (Drawable)serverPtr->pixmap, 0, 0);
    }
    /*
     * Now call back each of the tile clients to notify them that the
     * pixmap has changed.
     */
    for (item = Blt_ListFirstItem(&(serverPtr->clients)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tilePtr = (Tile *)Blt_ListGetKey(item);
	if (tilePtr->changeProc != NULL) {
	    (*tilePtr->changeProc) (tilePtr->clientData, (Blt_Tile)tilePtr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImageChangedProc
 *
 *	The Tk image has changed or been deleted, redraw the pixmap
 *	tile.
 *
 *	Note:	As of Tk 4.2, if you redraw Tk images from a
 *		Tk_ImageChangedProc you'll get a coredump.  As a
 *		workaround, we have to simulate how the Tk widgets
 *		use images and redraw within an idle event.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
ImageChangedProc(clientData, x, y, width, height, imageWidth, imageHeight)
    ClientData clientData;
    int x, y, width, height;	/* Not used */
    int imageWidth, imageHeight;/* Not used */
{
    TileServer *serverPtr = (TileServer *) clientData;

    if (!(serverPtr->flags & NOTIFY_PENDING)) {
	Tk_DoWhenIdle(RedrawTile, (ClientData)serverPtr);
	serverPtr->flags |= NOTIFY_PENDING;
    }
}

static void
InitTables()
{
    Tcl_InitHashTable(&tileTable, sizeof(TileKey) / sizeof(int));
    initialized = 1;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyServer --
 *
 *	Deletes the tile server structure, including the pixmap
 *	representing the tile.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyServer(serverPtr)
    TileServer *serverPtr;
{
    if (serverPtr->flags & NOTIFY_PENDING) {
	Tk_CancelIdleCall(RedrawTile, (ClientData)serverPtr);
    }
    if (serverPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(serverPtr->hashPtr);
    }
    if (serverPtr->pixmap != None) {
	Tk_FreePixmap(serverPtr->display, serverPtr->pixmap);
    }
    Tk_FreeImage(serverPtr->tkImage);
    if (serverPtr->nameId != NULL) {
	Blt_FreeUid(serverPtr->nameId);
    }
    free((char *)serverPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * FindServer --
 *
 *	Performs a hash table search for a tile server keyed by the
 *	image name and display.
 *
 * Results:
 *	A pointer to the tile server if found. NULL otherwise.
 *
 *----------------------------------------------------------------------
 */
static TileServer *
FindServer(tkwin, name)
    Tk_Window tkwin;
    char *name;
{
    TileKey key;
    Tcl_HashEntry *hPtr;

    key.display = Tk_Display(tkwin);
    key.nameId = Blt_FindUid(name);
    if (key.nameId == NULL) {
	return NULL;
    }
    hPtr = Tcl_FindHashEntry(&tileTable, (char *)&key);
    if (hPtr == NULL) {
	return NULL;
    }
    return (TileServer *) Tcl_GetHashValue(hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateServer --
 *
 *	Creates a tile server.  A tile server manages a single pixmap
 *	which it draws into whenever the image changes.  Clients use
 *	this pixmap and are notified by the server (if requested)
 *	whenever they need to reuse the pixmap.
 *
 * Results:
 *	A pointer to the new tile server.  If the image name does not
 *	represent a Tk image, NULL instead.
 *
 *----------------------------------------------------------------------
 */
static TileServer *
CreateServer(interp, tkwin, imageName)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *imageName;
{
    TileServer *serverPtr;
    Tk_Image tkImage;
    int width, height;
    int dummy;
    Pixmap pixmap;
    Window root;
    TileKey key;

    serverPtr = (TileServer *) calloc(1, sizeof(TileServer));
    assert(serverPtr);

    /*
     * Initialize the (server) bookkeeping on the tile.
     */
    serverPtr->tkwin = Tk_MainWindow(interp);
    serverPtr->display = Tk_Display(tkwin);
    serverPtr->interp = interp;
    /*
     * Get the image. Funnel all change notifications to a single routine.
     */
    tkImage = Tk_GetImage(interp, tkwin, imageName, ImageChangedProc,
	(ClientData)serverPtr);
    if (tkImage == NULL) {
	free((char *)serverPtr);
	return NULL;
    }
    /*
     * Create a pixmap the same size and draw the image into it.
     */
    Tk_SizeOfImage(tkImage, &width, &height);
    root = RootWindow(serverPtr->display, Tk_ScreenNumber(tkwin));
    pixmap = Tk_GetPixmap(serverPtr->display, root, width, height,
	Tk_Depth(serverPtr->tkwin));
    Tk_RedrawImage(tkImage, 0, 0, width, height, pixmap, 0, 0);

    serverPtr->pixmap = pixmap;
    serverPtr->tkImage = tkImage;
    serverPtr->nameId = Blt_GetUid(imageName);
    Blt_InitList(&(serverPtr->clients), TCL_ONE_WORD_KEYS);

    key.display = Tk_Display(tkwin);
    key.nameId = Blt_FindUid(imageName);
    serverPtr->hashPtr = Tcl_CreateHashEntry(&tileTable, (char *)&key, &dummy);
    Tcl_SetHashValue(serverPtr->hashPtr, (ClientData)serverPtr);
    return serverPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyClient --
 *
 *	Deletes bookkeeping for a client of a tile.  The client is
 *	removed from the server's list of clients and memory if
 *	released.
 *
 *	If the server has no more clients, the server is also deleted.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyClient(tilePtr)
    Tile *tilePtr;
{
    TileServer *serverPtr;
    serverPtr = tilePtr->serverPtr;

    /* Remove the client from the server tile's list */
    Blt_ListDeleteItem(tilePtr->item);

    if (Blt_ListGetLength(&(serverPtr->clients)) == 0) {
	/*
	 * If there are no more clients of the tile, then remove the
	 * pixmap, image, and the server record.
	 */
	DestroyServer(serverPtr);
    }
    free((char *)tilePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateClient --
 *
 *	Creates bookkeeping for a client of a tile.  The tile it
 *	uses is keyed by the display and image name.   The client
 *	is added to the server's list of clients.  It will be notified
 *	(if requested) whenever the tile changes. If no server exists
 *	already, one is created.
 *
 * Results:
 *	A pointer to the newly created client (i.e. tile).
 *
 *----------------------------------------------------------------------
 */
static Tile *
CreateClient(interp, tkwin, name)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *name;
{
    Tile *tilePtr;
    TileServer *serverPtr;

    tilePtr = (Tile *)calloc(1, sizeof(Tile));
    assert(tilePtr);

    /* Initialize client information (Remember to set the item) */
    tilePtr->magic = TILE_MAGIC;
    serverPtr = FindServer(tkwin, name);
    if (serverPtr == NULL) {
	serverPtr = CreateServer(interp, tkwin, name);
    }
    if (serverPtr == NULL) {
	return NULL;
    }
    tilePtr->item = Blt_ListAppend(&(serverPtr->clients), (char *)tilePtr,
	(ClientData)serverPtr);
    tilePtr->serverPtr = serverPtr;
    return tilePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetTile
 *
 *	Convert the named image into a tile.
 *
 * Results:
 *	If the image is valid, a new tile is returned.  If the name
 *	does not represent a proper image, an error message is left in
 *	interp->result.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
Blt_Tile
Blt_GetTile(interp, tkwin, imageName)
    Tcl_Interp *interp;		/* Interpreter to report results back to */
    Tk_Window tkwin;		/* Window on the same display as tile */
    char *imageName;		/* Name of image */
{
    if (!initialized) {
	InitTables();
    }
    return (Blt_Tile) CreateClient(interp, tkwin, imageName);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FreeTile
 *
 *	Release the resources associated with the tile.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Memory and X resources are freed.  Bookkeeping information
 *	about the tile (i.e. width, height, and name) is discarded.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
void
Blt_FreeTile(tile)
    Blt_Tile tile;		/* Tile to be deleted */
{
    Tile *tilePtr = (Tile *)tile;

    if (!initialized) {
	InitTables();
    }
    if ((tilePtr == NULL) || (tilePtr->magic != TILE_MAGIC)) {
	return;			/* No tile */
    }
    DestroyClient(tilePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NameOfTile
 *
 *	Returns the name of the image from which the tile was
 *	generated.
 *
 * Results:
 *	The name of the image is returned.  The name is not unique.
 *	Many tiles may use the same image.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
char *
Blt_NameOfTile(tile)
    Blt_Tile tile;		/* Tile to query */
{
    Tile *tilePtr = (Tile *)tile;

    if (tilePtr == NULL) {
	return "";
    }
    if (tilePtr->magic != TILE_MAGIC) {
	return "not a tile";
    }
    return (tilePtr->serverPtr->nameId);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_PixmapOfTile
 *
 *	Returns the pixmap of the tile.
 *
 * Results:
 *	The X pixmap used as the tile is returned.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
Pixmap
Blt_PixmapOfTile(tile)
    Blt_Tile tile;		/* Tile to query */
{
    Tile *tilePtr = (Tile *)tile;

    if ((tilePtr == NULL) || (tilePtr->magic != TILE_MAGIC)) {
	return None;
    }
    return (tilePtr->serverPtr->pixmap);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_SizeOfTile
 *
 *	Returns the width and height of the tile.
 *
 * Results:
 *	The width and height of the tile are returned.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
void
Blt_SizeOfTile(tile, widthPtr, heightPtr)
    Blt_Tile tile;		/* Tile to query */
    int *widthPtr, *heightPtr;	/* Returned dimensions of the tile (out) */
{
    Tile *tilePtr = (Tile *)tile;

    if ((tilePtr == NULL) || (tilePtr->magic != TILE_MAGIC)) {
	*widthPtr = *heightPtr = 0;
	return;			/* No tile given */
    }
    Tk_SizeOfImage(tilePtr->serverPtr->tkImage, widthPtr, heightPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_SetTileChangedProc
 *
 *	Sets the routine to called when an image changes.  If
 *	*changeProc* is NULL, no callback will be performed.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The designated routine will be called the next time the
 *	image associated with the tile changes.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
void
Blt_SetTileChangedProc(tile, changeProc, clientData)
    Blt_Tile tile;		/* Tile to query */
    Blt_TileChangedProc *changeProc;
    ClientData clientData;
{
    Tile *tilePtr = (Tile *)tile;

    if ((tilePtr != NULL) && (tilePtr->magic == TILE_MAGIC)) {
	tilePtr->changeProc = changeProc;
	tilePtr->clientData = clientData;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToTile --
 *
 *	Converts the name of an image into a tile.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToTile(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window on same display as tile */
    char *string;		/* Name of image */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of tile in record */
{
    Tile **tilePtrPtr = (Tile **)(widgRec + offset);
    Blt_Tile tile, lastTile;

    lastTile = (Blt_Tile)*tilePtrPtr;
    tile = NULL;
    if ((string != NULL) && (*string != '\0')) {
	tile = Blt_GetTile(interp, tkwin, string);
	if (tile == NULL) {
	    return TCL_ERROR;
	}
    }
    if (lastTile != NULL) {
	Blt_FreeTile(lastTile);
    }
    *tilePtrPtr = (Tile *)tile;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TileToString --
 *
 *	Returns the name of the tile.
 *
 * Results:
 *	The name of the tile is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
TileToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of tile in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Blt_Tile tile = *(Blt_Tile *)(widgRec + offset);

    return (Blt_NameOfTile(tile));
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_SetTileOrigin --
 *
 *	Set the pattern origin of the tile to a common point (i.e. the
 *	origin (0,0) of the top level window) so that tiles from two
 *	different widgets will match up.  This done by setting the
 *	GCTileStipOrigin field is set to the translated origin of the
 *	toplevel window in the hierarchy.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The GCTileStipOrigin is reset in the GC.  This will cause the
 *	tile origin to change when the GC is used for drawing.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
void
Blt_SetTileOrigin(tkwin, gc, x, y)
    Tk_Window tkwin;
    GC gc;
    int x, y;
{
    while (!Tk_IsTopLevel(tkwin)) {
	x += Tk_X(tkwin) + Tk_Changes(tkwin)->border_width;
	y += Tk_Y(tkwin) + Tk_Changes(tkwin)->border_width;
	tkwin = Tk_Parent(tkwin);
    }
    XSetTSOrigin(Tk_Display(tkwin), gc, -x, -y);
}
