/*
 * bltList.c --
 *
 *	The module implements generic linked lists.
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

static struct Blt_ListItem *
FindString(listPtr, key)
    Blt_List *listPtr;		/* List to search */
    char *key;			/* Key to match */
{
    register struct Blt_ListItem *iPtr;
    char c;

    c = key[0];
    for (iPtr = listPtr->headPtr; iPtr != NULL; iPtr = iPtr->nextPtr) {
	if ((c == iPtr->key.string[0]) &&
	    (strcmp(key, iPtr->key.string) == 0)) {
	    return iPtr;
	}
    }
    return NULL;
}

static struct Blt_ListItem *
FindOneWord(listPtr, key)
    Blt_List *listPtr;		/* List to search */
    char *key;			/* Key to match */
{
    register struct Blt_ListItem *iPtr;

    for (iPtr = listPtr->headPtr; iPtr != NULL; iPtr = iPtr->nextPtr) {
	if (key == iPtr->key.oneWordValue) {
	    return iPtr;
	}
    }
    return NULL;
}

static struct Blt_ListItem *
FindArray(listPtr, key)
    Blt_List *listPtr;		/* List to search */
    char *key;			/* Key to match */
{
    register struct Blt_ListItem *iPtr;
    int numBytes;

    numBytes = sizeof(int) * listPtr->type;
    for (iPtr = listPtr->headPtr; iPtr != NULL; iPtr = iPtr->nextPtr) {
	if (memcmp(key, iPtr->key.words, numBytes) == 0) {
	    return iPtr;
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateList --
 *
 *	Creates a new linked list structure and initializes its pointers
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
Blt_List *
Blt_CreateList(type)
    int type;
{
    Blt_List *listPtr;

    listPtr = (Blt_List *)malloc(sizeof(Blt_List));
    if (listPtr != NULL) {
	Blt_InitList(listPtr, type);
    }
    return (listPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListNewItem --
 *
 *	Creates a list entry holder.  This routine does not insert
 *	the entry into the list, nor does it no attempt to maintain
 *	consistency of the keys.  For example, more than one entry
 *	may use the same key.
 *
 * Results:
 *	The return value is the pointer to the newly created entry.
 *
 * Side Effects:
 *	The key is not copied, only the Uid is kept.  It is assumed
 *	this key will not change in the life of the entry.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem
Blt_ListNewItem(listPtr, key)
    Blt_List *listPtr;
    char *key;			/* Unique key to reference object */
{
    register struct Blt_ListItem *iPtr;
    int keySize;

    if (listPtr->type == TCL_STRING_KEYS) {
	keySize = strlen(key) + 1;
    } else {
	keySize = sizeof(int) * listPtr->type;
    }
    iPtr = (struct Blt_ListItem *)
	calloc(1, sizeof(struct Blt_ListItem) + keySize - 4);
    assert(iPtr);
    iPtr->clientData = (ClientData)NULL;
    iPtr->nextPtr = iPtr->prevPtr = NULL;
    iPtr->listPtr = listPtr;
    switch (listPtr->type) {
    case TCL_STRING_KEYS:
	strcpy(iPtr->key.string, key);
	break;
    case TCL_ONE_WORD_KEYS:
	iPtr->key.oneWordValue = key;
	break;
    default:
	memcpy(iPtr->key.words, key, keySize);
	break;
    }
    return iPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * FreeItem --
 *
 *	Free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
FreeItem(iPtr)
    struct Blt_ListItem *iPtr;
{
    free((char *)iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListReset --
 *
 *	Removes all the entries from a list, removing pointers to the
 *	objects and keys (not the objects or keys themselves).  The
 *	entry counter is reset to zero.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListReset(listPtr)
    Blt_List *listPtr;		/* List to clear */
{
    if (listPtr != NULL) {
	register struct Blt_ListItem *oldPtr;
	register struct Blt_ListItem *iPtr = listPtr->headPtr;

	while (iPtr != NULL) {
	    oldPtr = iPtr;
	    iPtr = iPtr->nextPtr;
	    FreeItem(oldPtr);
	    Blt_InitList(listPtr, listPtr->type);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListDestroy
 *
 *     Frees all list structures
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListDestroy(listPtr)
    Blt_List *listPtr;
{
    if (listPtr != NULL) {
	Blt_ListReset(listPtr);
	free((char *)listPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitList --
 *
 *	Initializes a linked list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_InitList(listPtr, type)
    Blt_List *listPtr;
    int type;
{
    listPtr->numEntries = 0;
    listPtr->headPtr = listPtr->tailPtr = NULL;
    listPtr->type = type;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListLinkAfter --
 *
 *	Inserts an entry following a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListLinkAfter(listPtr, iPtr, afterPtr)
    Blt_List *listPtr;
    struct Blt_ListItem *iPtr;
    struct Blt_ListItem *afterPtr;
{
    if (listPtr->headPtr == NULL) {
	listPtr->tailPtr = listPtr->headPtr = iPtr;
    } else {
	if (afterPtr == NULL) {
	    /* Prepend to the front of the list */
	    iPtr->nextPtr = listPtr->headPtr;
	    iPtr->prevPtr = NULL;
	    listPtr->headPtr->prevPtr = iPtr;
	    listPtr->headPtr = iPtr;
	} else {
	    iPtr->nextPtr = afterPtr->nextPtr;
	    iPtr->prevPtr = afterPtr;
	    if (afterPtr == listPtr->tailPtr) {
		listPtr->tailPtr = iPtr;
	    } else {
		afterPtr->nextPtr->prevPtr = iPtr;
	    }
	    afterPtr->nextPtr = iPtr;
	}
    }
    iPtr->listPtr = listPtr;
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListLinkBefore --
 *
 *	Inserts an entry preceding a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListLinkBefore(listPtr, iPtr, beforePtr)
    Blt_List *listPtr;		/* List to contain new entry */
    struct Blt_ListItem *iPtr;	/* New entry to be inserted */
    struct Blt_ListItem *beforePtr;	/* Entry to link before */
{
    if (listPtr->headPtr == NULL) {
	listPtr->tailPtr = listPtr->headPtr = iPtr;
    } else {
	if (beforePtr == NULL) {
	    /* Append onto the end of the list */
	    iPtr->nextPtr = NULL;
	    iPtr->prevPtr = listPtr->tailPtr;
	    listPtr->tailPtr->nextPtr = iPtr;
	    listPtr->tailPtr = iPtr;
	} else {
	    iPtr->prevPtr = beforePtr->prevPtr;
	    iPtr->nextPtr = beforePtr;
	    if (beforePtr == listPtr->headPtr) {
		listPtr->headPtr = iPtr;
	    } else {
		beforePtr->prevPtr->nextPtr = iPtr;
	    }
	    beforePtr->prevPtr = iPtr;
	}
    }
    iPtr->listPtr = listPtr;
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListUnlinkItem --
 *
 *	Unlinks an entry from the given list. The entry itself is
 *	not deallocated, but only removed from the list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListUnlinkItem(iPtr)
    struct Blt_ListItem *iPtr;
{
    Blt_List *listPtr;

    listPtr = iPtr->listPtr;
    if (listPtr != NULL) {
	if (listPtr->headPtr == iPtr) {
	    listPtr->headPtr = iPtr->nextPtr;
	}
	if (listPtr->tailPtr == iPtr) {
	    listPtr->tailPtr = iPtr->prevPtr;
	}
	if (iPtr->nextPtr != NULL) {
	    iPtr->nextPtr->prevPtr = iPtr->prevPtr;
	}
	if (iPtr->prevPtr != NULL) {
	    iPtr->prevPtr->nextPtr = iPtr->nextPtr;
	}
	iPtr->listPtr = NULL;
	listPtr->numEntries--;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListFind --
 *
 *	Find the first entry matching the key given.
 *
 * Results:
 *	Returns the pointer to the entry.  If no entry matching
 *	the key given is found, then NULL is returned.
 *
 *----------------------------------------------------------------------
 */

Blt_ListItem
Blt_ListFind(listPtr, key)
    Blt_List *listPtr;		/* List to search */
    char *key;			/* Key to match */
{
    if (listPtr != NULL) {
	switch (listPtr->type) {
	case TCL_STRING_KEYS:
	    return FindString(listPtr, key);
	case TCL_ONE_WORD_KEYS:
	    return FindOneWord(listPtr, key);
	default:
	    return FindArray(listPtr, key);
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListDeleteItem --
 *
 *	Unlinks and deletes the given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListDeleteItem(iPtr)
    struct Blt_ListItem *iPtr;
{
    Blt_ListUnlinkItem(iPtr);
    FreeItem(iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListDelete --
 *
 *	Find the entry and free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListDelete(listPtr, key)
    Blt_List *listPtr;
    char *key;
{
    struct Blt_ListItem *iPtr;

    iPtr = Blt_ListFind(listPtr, key);
    if (iPtr != NULL) {
	Blt_ListDeleteItem(iPtr);
    }
}

Blt_ListItem
Blt_ListAppend(listPtr, key, clientData)
    Blt_List *listPtr;
    char *key;
    ClientData clientData;
{
    struct Blt_ListItem *iPtr;

    iPtr = Blt_ListNewItem(listPtr, key);
    Blt_ListSetValue(iPtr, clientData);
    Blt_ListAppendItem(listPtr, iPtr);
    return iPtr;
}

Blt_ListItem
Blt_ListPrepend(listPtr, key, clientData)
    Blt_List *listPtr;
    char *key;
    ClientData clientData;
{
    struct Blt_ListItem *iPtr;

    iPtr = Blt_ListNewItem(listPtr, key);
    Blt_ListSetValue(iPtr, clientData);
    Blt_ListPrependItem(listPtr, iPtr);
    return iPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListFindNthItem --
 *
 *	Find the entry based upon a given position in list.
 *
 * Results:
 *	Returns the pointer to the item, if that numbered element
 *	exists. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem
Blt_ListFindNthItem(listPtr, position, direction)
    Blt_List *listPtr;		/* List to traverse */
    int position;		/* Index of item to select from front
				 * or back of the list. */
    int direction;
{
    register struct Blt_ListItem *iPtr;

    if (listPtr != NULL) {
	if (direction > 0) {
	    for (iPtr = listPtr->headPtr; iPtr != NULL; iPtr = iPtr->nextPtr) {
		if (position == 0) {
		    return iPtr;
		}
		position--;
	    }
	} else {
	    for (iPtr = listPtr->tailPtr; iPtr != NULL; iPtr = iPtr->prevPtr) {
		if (position == 0) {
		    return iPtr;
		}
		position--;
	    }
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListSort --
 *
 *	Find the entry based upon a given position in list.
 *
 * Results:
 *	Returns the pointer to the item, if that numbered element
 *	exists. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ListSort(listPtr, proc)
    Blt_List *listPtr;		/* List to traverse */
    Blt_ListCompareProc *proc;
{
    struct Blt_ListItem **itemArr;
    register struct Blt_ListItem *iPtr;
    register int i;

    if (listPtr->numEntries < 2) {
	return;
    }
    itemArr = (struct Blt_ListItem **)
	malloc(sizeof(struct Blt_ListItem *) * (listPtr->numEntries + 1));
    if (itemArr == NULL) {
	return;			/* Out of memory. */
    }
    i = 0;
    for (iPtr = listPtr->headPtr; iPtr != NULL; iPtr = iPtr->nextPtr) {
	itemArr[i++] = iPtr;
    }
    qsort((char *)itemArr, listPtr->numEntries, sizeof(struct Blt_ListItem *),
	             (QSortCompareProc *)proc);

    /* Rethread the list. */
    iPtr = itemArr[0];
    listPtr->headPtr = iPtr;
    iPtr->prevPtr = NULL;
    for (i = 1; i < listPtr->numEntries; i++) {
	iPtr->nextPtr = itemArr[i];
	iPtr->nextPtr->prevPtr = iPtr;
	iPtr = iPtr->nextPtr;
    }
    listPtr->tailPtr = iPtr;
    iPtr->nextPtr = NULL;
    free((char *)itemArr);
}
