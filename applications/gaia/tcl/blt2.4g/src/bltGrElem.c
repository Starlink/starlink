
/*
 * bltGrElem.c --
 *
 *	This module implements generic elements for the BLT graph widget.
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
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

#include "bltGraph.h"
#include <ctype.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

static int StringToData _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *DataToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltDataOption =
{
    StringToData, DataToString, (ClientData)0
};

static int StringToDataPairs _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *DataPairsToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltDataPairsOption =
{
    StringToDataPairs, DataPairsToString, (ClientData)0
};

#include "bltGrElem.h"

extern Element *Blt_BarElement();
extern Element *Blt_LineElement();

#ifdef __STDC__
static Blt_VectorChangedProc VectorChangedProc;
#endif /* __STDC__ */

extern Tk_CustomOption bltLengthOption;


/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */
static void
SyncElemVector(vPtr)
    ElemVector *vPtr;
{
    vPtr->numValues = Blt_VecLength(vPtr->vecPtr);
    vPtr->valueArr = Blt_VecData(vPtr->vecPtr);
    vPtr->min = Blt_VecMin(vPtr->vecPtr);
    vPtr->max = Blt_VecMax(vPtr->vecPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * FindLimits --
 *
 *	Find the minimum, positive minimum, and maximum values in a
 *	given vector and store the results in the vector structure.
 *
 * Results:
 *     	None.
 *
 * Side Effects:
 *	Minimum, positive minimum, and maximum values are stored in
 *	the vector.
 *
 *----------------------------------------------------------------------
 */
static void
FindLimits(vPtr)
    ElemVector *vPtr;
{
    register int i;
    register double *x;
    register double min, max;

    if ((vPtr->numValues < 1) || (vPtr->valueArr == NULL)) {
	return;			/* This shouldn't ever happen */
    }
    x = vPtr->valueArr;

    /*  Initialize values to track the vector limits */
    min = max = x[0];
    for (i = 1; i < vPtr->numValues; i++) {
	if (x[i] < min) {
	    min = x[i];
	} else if (x[i] > max) {
	    max = x[i];
	}
    }
    vPtr->min = min, vPtr->max = max;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FindElemVectorMinimum --
 *
 *	Find the minimum, positive minimum, and maximum values in a
 *	given vector and store the results in the vector structure.
 *
 * Results:
 *     	None.
 *
 * Side Effects:
 *	Minimum, positive minimum, and maximum values are stored in
 *	the vector.
 *
 *----------------------------------------------------------------------
 */
double
Blt_FindElemVectorMinimum(vPtr, minLimit)
    ElemVector *vPtr;
    double minLimit;
{
    register int i;
    register double *x;
    register double min;

    min = bltPosInfinity;
    x = vPtr->valueArr;
    for (i = 0; i <= vPtr->numValues; i++) {
	if ((x[i] > minLimit) && (min > x[i])) {
	    min = x[i];
	}
    }
    if (min == bltPosInfinity) {
	min = minLimit;
    }
    return min;
}

static void
FreeDataVector(vPtr)
    ElemVector *vPtr;
{
    if (vPtr->clientId != NULL) {
	Blt_FreeVectorId(vPtr->clientId);	/* Free the old vector */
	vPtr->clientId = NULL;
    } else if (vPtr->valueArr != NULL) {
	free((char *)vPtr->valueArr);
    }
    vPtr->valueArr = NULL;
    vPtr->numValues = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * VectorChangedProc --
 *
 *
 * Results:
 *     	None.
 *
 * Side Effects:
 *	Graph is redrawn.
 *
 *----------------------------------------------------------------------
 */
static void
VectorChangedProc(interp, clientData, notify)
    Tcl_Interp *interp;
    ClientData clientData;
    Blt_VectorNotify notify;
{
    ElemVector *vPtr = (ElemVector *)clientData;
    Element *elemPtr = vPtr->elemPtr;
    Graph *graphPtr = elemPtr->graphPtr;

    switch (notify) {
    case BLT_VECTOR_NOTIFY_DESTROY:
	vPtr->clientId = NULL;
	vPtr->valueArr = NULL;
	vPtr->numValues = 0;
	break;

    case BLT_VECTOR_NOTIFY_UPDATE:
    default:
	Blt_GetVectorById(interp, vPtr->clientId, &(vPtr->vecPtr));
	SyncElemVector(vPtr);
	break;

    }
    graphPtr->flags |= RESET_AXES;
    elemPtr->flags |= COORDS_NEEDED;
    if (!elemPtr->hidden) {
	graphPtr->flags |= REDRAW_BACKING_STORE;
	Blt_EventuallyRedrawGraph(graphPtr);
    }
}

static int
EvalExprList(interp, list, numElemPtr, dataPtrPtr)
    Tcl_Interp *interp;
    char *list;
    int *numElemPtr;
    double **dataPtrPtr;
{
    int numElem;
    char **elemArr;
    double *dataArr;
    int result;

    result = TCL_ERROR;
    elemArr = NULL;
    if (Tcl_SplitList(interp, list, &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    dataArr = NULL;
    if (numElem > 0) {
	register double *dataPtr;
	register int i;

	dataArr = (double *)malloc(sizeof(double) * numElem);
	if (dataArr == NULL) {
	    Tcl_AppendResult(interp, "can't allocate new vector", (char *)NULL);
	    goto badList;
	}
	dataPtr = dataArr;
	for (i = 0; i < numElem; i++) {
	    if (Tcl_ExprDouble(interp, elemArr[i], dataPtr) != TCL_OK) {
		goto badList;
	    }
	    dataPtr++;
	}
    }
    result = TCL_OK;

  badList:
    free((char *)elemArr);
    *dataPtrPtr = dataArr;
    *numElemPtr = numElem;
    if (result != TCL_OK) {
	free((char *)dataArr);
    }
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToData --
 *
 *	Given a Tcl list of numeric expression representing the element
 *	values, convert into an array of double precision values. In
 *	addition, the minimum and maximum values are saved.  Since
 *	elastic values are allow (values which translate to the
 *	min/max of the graph), we must try to get the non-elastic
 *	minimum and maximum.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vector is passed
 *	back via the vPtr.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToData(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Type of axis vector to fill */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Tcl list of expressions */
    char *widgRec;		/* Element record */
    int offset;			/* Offset of vector in Element record */
{
    Element *elemPtr = (Element *)(widgRec);
    ElemVector *vPtr = (ElemVector *)(widgRec + offset);

    FreeDataVector(vPtr);
    if (Blt_VectorExists(interp, string)) {
	Blt_VectorId clientId;

	clientId = Blt_AllocVectorId(interp, string);
	if (Blt_GetVectorById(interp, clientId, &(vPtr->vecPtr)) != TCL_OK) {
	    return TCL_ERROR;
	}
	Blt_SetVectorChangedProc(clientId, VectorChangedProc, (ClientData)vPtr);
	vPtr->elemPtr = elemPtr;
	vPtr->clientId = clientId;
	SyncElemVector(vPtr);
    } else {
	double *newArr;
	int numValues;

	if (EvalExprList(interp, string, &numValues, &newArr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (numValues > 0) {
	    vPtr->valueArr = newArr;
	}
	vPtr->numValues = numValues;
	FindLimits(vPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DataToString --
 *
 *	Convert the vector of floating point values into a Tcl list.
 *
 * Results:
 *	The string representation of the vector is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
DataToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Type of axis vector to print */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element record */
    int offset;			/* Offset of vector in Element record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Element *elemPtr = (Element *)(widgRec);
    ElemVector *vPtr = (ElemVector *)(widgRec + offset);
    Tcl_DString dStr;
    char string[TCL_DOUBLE_SPACE + 1];
    char *result;
    register int i;

    if (vPtr->clientId != NULL) {
	return Blt_NameOfVectorId(vPtr->clientId);
    }
    if (vPtr->numValues == 0) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    for (i = 0; i < vPtr->numValues; i++) {
	Tcl_PrintDouble(elemPtr->graphPtr->interp, vPtr->valueArr[i], string);
	Tcl_DStringAppendElement(&dStr, string);
    }
    result = Tcl_DStringValue(&dStr);

    /*
     * If memory wasn't allocated for the dynamic string, do it here (it's
     * currently on the stack), so that Tcl can free it normally.
     */
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToDataPairs --
 *
 *	This procedure is like StringToData except that it
 *	interprets the list of numeric expressions as X Y coordinate
 *	pairs.  The minimum and maximum for both the X and Y vectors are
 *	determined.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vectors are passed
 *	back via the widget record (elemPtr).
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToDataPairs(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Tcl list of numeric expressions */
    char *widgRec;		/* Element record */
    int offset;			/* not used */
{
    Element *elemPtr = (Element *)widgRec;
    int numElem, newSize;
    double *newArr;

    if (EvalExprList(interp, string, &numElem, &newArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem & 1) {
	Tcl_AppendResult(interp, "odd number of data points", (char *)NULL);
	free((char *)newArr);
	return TCL_ERROR;
    }
    numElem /= 2;
    newSize = numElem * sizeof(double);

    FreeDataVector(&(elemPtr->x));
    FreeDataVector(&(elemPtr->y));

    elemPtr->x.valueArr = (double *)malloc(newSize);
    elemPtr->y.valueArr = (double *)malloc(newSize);
    assert(elemPtr->x.valueArr && elemPtr->y.valueArr);
    elemPtr->x.numValues = elemPtr->y.numValues = numElem;

    if (newSize > 0) {
	register double *dataPtr;
	register int i;

	for (dataPtr = newArr, i = 0; i < numElem; i++) {
	    elemPtr->x.valueArr[i] = *dataPtr++;
	    elemPtr->y.valueArr[i] = *dataPtr++;
	}
	free((char *)newArr);
	FindLimits(&(elemPtr->x));
	FindLimits(&(elemPtr->y));
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DataPairsToString --
 *
 *	Convert pairs of floating point values in the X and Y arrays
 *	into a Tcl list.
 *
 * Results:
 *	The return value is a string (Tcl list).
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
DataPairsToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Element *elemPtr = (Element *)widgRec;
    Tcl_Interp *interp = elemPtr->graphPtr->interp;
    int i;
    int length;
    char *result;
    char string[TCL_DOUBLE_SPACE + 1];
    Tcl_DString dStr;

    length = NumberOfPoints(elemPtr);
    if (length < 1) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    for (i = 0; i < length; i++) {
	Tcl_PrintDouble(interp, elemPtr->x.valueArr[i], string);
	Tcl_DStringAppendElement(&dStr, string);
	Tcl_PrintDouble(interp, elemPtr->y.valueArr[i], string);
	Tcl_DStringAppendElement(&dStr, string);
    }
    result = Tcl_DStringValue(&dStr);

    /*
     * If memory wasn't allocated for the dynamic string, do it here (it's
     * currently on the stack), so that Tcl can free it normally.
     */
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/*
 * Generic element routines:
 */

ObjectType
Blt_GetElementType(string)
    char *string;
{
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'l') && (strncmp(string, "line", length) == 0)) {
	return TYPE_ELEM_LINE;
    } else if ((c == 's') && (strncmp(string, "strip", length) == 0)) {
	return TYPE_ELEM_STRIP;
    } else if ((c == 'b') && (strncmp(string, "bar", length) == 0)) {
	return TYPE_ELEM_BAR;
    } else {
	return TYPE_UNKNOWN;
    }
}

char *
Blt_NameOfElementType(type)
    ObjectType type;
{
    switch (type) {
    case TYPE_ELEM_LINE:
	return "line";
    case TYPE_ELEM_STRIP:
	return "strip";
    case TYPE_ELEM_BAR:
	return "bar";
    default:
	return "unknown element type";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetIndex --
 *
 *	Given a string representing the index of a pair of x,y
 *	coordinates, return the numeric index.
 *
 * Results:
 *     	A standard TCL result.
 *
 *----------------------------------------------------------------------
 */
static int
GetIndex(interp, elemPtr, string, indexPtr)
    Tcl_Interp *interp;
    Element *elemPtr;
    char *string;
    int *indexPtr;
{
    long elemIndex;
    int last;

    last = NumberOfPoints(elemPtr) - 1;
    if ((*string == 'e') && (strcmp("end", string) == 0)) {
	elemIndex = last;
    } else if (Tcl_ExprLong(interp, string, &elemIndex) != TCL_OK) {
	return TCL_ERROR;
    }
#ifdef notdef
    if ((elemIndex < 0) || (elemIndex > last)) {
	Tcl_AppendResult(interp, "index \"", string,
	    "\" is out of range for element", (char *)NULL);
	return TCL_ERROR;
    }
#endif
    *indexPtr = (int)elemIndex;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NameToElement --
 *
 *	Find the element represented the given name,  returning
 *	a pointer to its data structure in *elemPtrPtr*.
 *
 * Results:
 *     	A standard TCL result.
 *
 *----------------------------------------------------------------------
 */
int
Blt_NameToElement(graphPtr, name, elemPtrPtr)
    Graph *graphPtr;
    char *name;
    Element **elemPtrPtr;
{
    Tcl_HashEntry *hPtr;

    if (name == NULL) {
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), name);
    if (hPtr == NULL) {
	Tcl_AppendResult(graphPtr->interp, "can't find element \"", name,
	    "\" in \"", Tk_PathName(graphPtr->tkwin), "\"", (char *)NULL);
	return TCL_ERROR;
    }
    *elemPtrPtr = (Element *)Tcl_GetHashValue(hPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyElement --
 *
 *	Add a new element to the graph.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyElement(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Blt_ListItem item;

    Tk_FreeOptions(elemPtr->infoPtr->configSpecs, (char *)elemPtr,
	graphPtr->display, GraphType(elemPtr));
    /*
     * Call the element's own destructor to release the memory and
     * resources allocated for it.
     */
    Blt_DeleteAllBindings(graphPtr->bindTable, (ClientData)elemPtr);
    (*elemPtr->infoPtr->destroyProc) (graphPtr, elemPtr);

    /* Remove it also from the element display list */
    item = Blt_ListFind(&(graphPtr->elemList), elemPtr->name);
    if (item != NULL) {
	Blt_ListDeleteItem(item);
	graphPtr->flags |= SET_ALL_FLAGS;
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    /* Remove the element for the graph's hash table of elements */
    if (elemPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(elemPtr->hashPtr);
    }
    if (elemPtr->name != NULL) {
	free((char *)elemPtr->name);
    }
    free((char *)elemPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateElement --
 *
 *	Add a new element to the graph.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
CreateElement(graphPtr, interp, argc, argv, type)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
    ObjectType type;
{
    Element *elemPtr;
    Tcl_HashEntry *hPtr;
    int isNew;

    hPtr = Tcl_CreateHashEntry(&(graphPtr->elemTable), argv[3], &isNew);
    if (!isNew) {
	Tcl_AppendResult(interp, "element \"", argv[3],
	    "\" already exists in \"", argv[0], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (type == TYPE_ELEM_BAR) {
	elemPtr = Blt_BarElement();
    } else {
	/* Stripcharts are line graphs with some options enabled */
	elemPtr = Blt_LineElement();
    }
    elemPtr->hashPtr = hPtr;

    /* By default the element's name and it's label are the same. */
    elemPtr->label = strdup(argv[3]);
    elemPtr->name = strdup(argv[3]);
    elemPtr->graphPtr = graphPtr;
    elemPtr->hidden = 0;
    elemPtr->type = type;
    Tcl_SetHashValue(hPtr, (ClientData)elemPtr);

    if (Blt_ConfigureWidgetComponent(interp, graphPtr->tkwin, elemPtr->name,
	    "Element", elemPtr->infoPtr->configSpecs, argc - 4, argv + 4,
	    (char *)elemPtr, GraphType(elemPtr)) != TCL_OK) {
	DestroyElement(graphPtr, elemPtr);
	return TCL_ERROR;
    }
    (*elemPtr->infoPtr->configProc) (graphPtr, elemPtr);

    if (!elemPtr->hidden) {
	/*
	 * If the element hasn't been configured as hidden, add the new element
	 * to the display list and redraw the graph.
	 */
	Blt_ListAppend(&(graphPtr->elemList), elemPtr->name,
	    (ClientData)elemPtr);
	graphPtr->flags |= REDRAW_BACKING_STORE;
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    elemPtr->flags |= COORDS_NEEDED;
    graphPtr->flags |= RESET_AXES;
    Tcl_SetResult(interp, elemPtr->name, TCL_STATIC);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * RebuildDisplayList --
 *
 *	Given a Tcl list of element names, this procedure rebuilds the
 *	display list, ignoring invalid element names. This list describes
 *	not only only which elements to draw, but in what order.  This is
 *	only important for bar and pie charts.
 *
 * Results:
 *	The return value is a standard Tcl result.  Only if the Tcl list
 *	cannot be split, a TCL_ERROR is returned and interp->result contains
 *	an error message.
 *
 * Side effects:
 *	The graph is eventually redrawn using the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
RebuildDisplayList(graphPtr, newList)
    Graph *graphPtr;		/* Graph widget record */
    char *newList;		/* Tcl list of element names */
{
    int numNames;		/* Number of names found in Tcl name list */
    char **nameArr;		/* Broken out array of element names */
    Tcl_HashSearch cursor;
    register int i;
    register Tcl_HashEntry *hPtr;
    Element *elemPtr;		/* Element information record */

    if (Tcl_SplitList(graphPtr->interp, newList, &numNames,
	    &nameArr) != TCL_OK) {
	Tcl_AppendResult(graphPtr->interp, "can't split name list \"", newList,
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * Clear the display list and mark all elements as hidden.
     */
    Blt_ListReset(&(graphPtr->elemList));
    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	elemPtr->hidden = TRUE;
    }

    /*
     * Rebuild the display list, checking that each name it exists
     * (currently ignoring invalid element names).
     */
    for (i = 0; i < numNames; i++) {
	if (Blt_NameToElement(graphPtr, nameArr[i], &elemPtr) == TCL_OK) {
	    elemPtr->hidden = FALSE;
	    Blt_ListAppend(&(graphPtr->elemList), elemPtr->name,
		(ClientData)elemPtr);
	}
    }
    free((char *)nameArr);
    graphPtr->flags |= SET_ALL_FLAGS;
    Blt_EventuallyRedrawGraph(graphPtr);
    Tcl_ResetResult(graphPtr->interp);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DestroyElements --
 *
 *	Removes all the graph's elements. This routine is called when
 *	the graph is destroyed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory allocated for the graph's elements is freed.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DestroyElements(graphPtr)
    Graph *graphPtr;
{
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    Element *elemPtr;

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	elemPtr->hashPtr = NULL;
	DestroyElement(graphPtr, elemPtr);
    }
    Tcl_DeleteHashTable(&(graphPtr->elemTable));
    Tcl_DeleteHashTable(&(graphPtr->elemTagTable));
    Blt_ListReset(&(graphPtr->elemList));
}

void
Blt_TransformElements(graphPtr)
    Graph *graphPtr;
{
    Element *elemPtr;
    register Blt_ListItem item;

    if (graphPtr->mode != MODE_NORMAL) {
	Blt_ResetStacks(graphPtr);
    }
    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if ((graphPtr->flags & COORDS_ALL_PARTS) ||
	    (elemPtr->flags & COORDS_NEEDED)) {
	    (*elemPtr->infoPtr->transformProc) (graphPtr, elemPtr);
	    elemPtr->flags &= ~COORDS_NEEDED;
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_DrawElements --
 *
 *	Calls the individual element drawing routines for each
 *	element.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Elements are drawn into the drawable (pixmap) which will
 *	eventually be displayed in the graph window.
 *
 * -----------------------------------------------------------------
 */
void
Blt_DrawElements(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    register Blt_ListItem item;
    register Element *elemPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	(*elemPtr->infoPtr->drawNormalProc) (graphPtr, drawable, elemPtr);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_DrawActiveElements --
 *
 *	Calls the individual element drawing routines to display
 *	the active colors for each element.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Elements are drawn into the drawable (pixmap) which will
 *	eventually be displayed in the graph window.
 *
 * -----------------------------------------------------------------
 */
void
Blt_DrawActiveElements(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    register Blt_ListItem item;
    register Element *elemPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (elemPtr->flags & ELEM_ACTIVE) {
	    (*elemPtr->infoPtr->drawActiveProc) (graphPtr, drawable, elemPtr);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_PrintElements --
 *
 *	Generates PostScript output for each graph element in the
 *	element display list.
 *
 * -----------------------------------------------------------------
 */
void
Blt_PrintElements(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    register Blt_ListItem item;
    Element *elemPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);

	/* Comment the PostScript to indicate the start of the element */
	Blt_PrintFormat(printable, "\n%% Element \"%s\"\n\n", elemPtr->name);
	(*elemPtr->infoPtr->printNormalProc) (graphPtr, printable, elemPtr);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_PrintActiveElements --
 *
 * -----------------------------------------------------------------
 */
void
Blt_PrintActiveElements(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    register Blt_ListItem item;
    register Element *elemPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if ((!elemPtr->hidden) && (elemPtr->flags & ELEM_ACTIVE)) {
	    Blt_PrintFormat(printable, "\n%% Active Element \"%s\"\n\n",
		elemPtr->name);
	    (*elemPtr->infoPtr->printActiveProc) (graphPtr, printable, elemPtr);
	}
    }
}


int
Blt_GraphUpdateNeeded(graphPtr)
    Graph *graphPtr;
{
    register Blt_ListItem item;
    register Element *elemPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	/* Check if the x or y vectors have notifications pending */
	if (((elemPtr->x.clientId != NULL) &&
		(Blt_VectorNotifyPending(elemPtr->x.clientId))) ||
	    ((elemPtr->y.clientId != NULL) &&
		(Blt_VectorNotifyPending(elemPtr->y.clientId)))) {
	    return 1;
	}
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * ActivateOp --
 *
 *	Marks data points of elements (given by their index) as active.
 *
 * Results:
 *	Returns TCL_OK if no errors occurred.
 *
 *----------------------------------------------------------------------
 */
static int
ActivateOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    register int i;
    int *activeArr;
    int reqNumActive;

    if (argc == 3) {
	register Tcl_HashEntry *hPtr;
	Tcl_HashSearch cursor;

	/* List all the currently active elements */
	for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	    elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	    if (elemPtr->flags & ELEM_ACTIVE) {
		Tcl_AppendElement(graphPtr->interp, elemPtr->name);
	    }
	}
	return TCL_OK;
    }
    if (Blt_NameToElement(graphPtr, argv[3], &elemPtr) != TCL_OK) {
	return TCL_ERROR;	/* Can't find named element */
    }
    elemPtr->flags |= ELEM_ACTIVE | ELEM_UPDATE_ACTIVE;

    activeArr = NULL;
    reqNumActive = -1;
    if (argc > 4) {
	register int *activePtr;

	reqNumActive = argc - 4;
	activePtr = activeArr = (int *)malloc(sizeof(int) * reqNumActive);
	assert(activeArr);
	for (i = 4; i < argc; i++) {
	    if (GetIndex(interp, elemPtr, argv[i], activePtr) != TCL_OK) {
		return TCL_ERROR;
	    }
	    activePtr++;
	}
    }
    if (elemPtr->reqActiveArr != NULL) {
	free((char *)elemPtr->reqActiveArr);
    }
    elemPtr->reqNumActive = reqNumActive;
    elemPtr->reqActiveArr = activeArr;
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

ClientData
Blt_MakeElementTag(graphPtr, tagName)
    Graph *graphPtr;
    char *tagName;
{
    Tcl_HashEntry *hPtr;
    int isNew;

    hPtr = Tcl_CreateHashEntry(&(graphPtr->elemTagTable), tagName, &isNew);
    return (ClientData) Tcl_GetHashKey(&(graphPtr->elemTagTable), hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * BindOp --
 *
 *	.g element bind elemName sequence command
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BindOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return Blt_ConfigureBindings(interp, graphPtr->bindTable,
	Blt_MakeElementTag(graphPtr, argv[3]), argc - 4, argv + 4);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateOp --
 *
 *	Add a new element to the graph (using the default type of the
 *	graph).
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
CreateOp(graphPtr, interp, argc, argv, type)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
    ObjectType type;
{
    return CreateElement(graphPtr, interp, argc, argv, type);
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Element *elemPtr;

    if (Blt_NameToElement(graphPtr, argv[3], &elemPtr) != TCL_OK) {
	return TCL_ERROR;	/* Can't find named element */
    }
    if (Tk_ConfigureValue(interp, graphPtr->tkwin, elemPtr->infoPtr->configSpecs,
	    (char *)elemPtr, argv[4], GraphType(elemPtr)) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestOp --
 *
 *	Find the element closest to the window coordinates specified.
 *
 * Results:
 *	Returns TCL_OK if no errors occurred. The result field of the
 *	interpreter may contain a list containing the element name,
 *	the index of the closest point, and the x and y graph coordinates
 *	of the point is stored.  If an error occurred, returns TCL_ERROR
 *	and an error message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */

static Tk_ConfigSpec closestSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-halo", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(ClosestSearch, halo), 0, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-interpolate", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(ClosestSearch, interpolate), 0},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

static int
ClosestOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    ClosestSearch info;
    int i;
    int flags = TCL_LEAVE_ERR_MSG;

    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    if (Tk_GetPixels(interp, graphPtr->tkwin, argv[3], &(info.x)) != TCL_OK) {
	Tcl_AppendResult(interp, ": bad window x-coordinate", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tk_GetPixels(interp, graphPtr->tkwin, argv[4], &(info.y)) != TCL_OK) {
	Tcl_AppendResult(interp, ": bad window y-coordinate", (char *)NULL);
	return TCL_ERROR;
    }
    if (graphPtr->inverted) {
	int hold;

	hold = info.x, info.x = info.y, info.y = hold;
    }
    for (i = 6; i < argc; i += 2) {	/* Count switches-value pairs */
	if ((argv[i][0] != '-') ||
	    ((argv[i][1] == '-') && (argv[i][2] == '\0'))) {
	    break;
	}
    }
    if (i > argc) {
	i = argc;
    }
    info.interpolate = 0;
    info.halo = graphPtr->halo;
    info.index = -1;

    if (Tk_ConfigureWidget(interp, graphPtr->tkwin, closestSpecs, i - 6,
	    argv + 6, (char *)&info, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;	/* Error processing option */
    }
    if ((i < argc) && (argv[i][0] == '-')) {
	i++;			/* Skip "--" */
    }
    info.dist = (double)(info.halo + 1);

    if (i < argc) {
	for ( /* empty */ ; i < argc; i++) {
	    if (Blt_NameToElement(graphPtr, argv[i], &elemPtr) != TCL_OK) {
		return TCL_ERROR;	/* Can't find named element */
	    }
	    if (elemPtr->hidden) {
		Tcl_AppendResult(interp, "element \"", argv[i], "\" is hidden",
		    (char *)NULL);
		return TCL_ERROR;	/* Element isn't visible */
	    }
	    (*elemPtr->infoPtr->closestProc) (graphPtr, elemPtr, &info);
	}
    } else {
	register Blt_ListItem item;
	/*
	 * Find the closest point from the set of displayed elements,
	 * searching the display list from back to front.  That way, if
	 * the points from two different elements overlay each other exactly,
	 * the one that's on top (visible) is picked.
	 */
	for (item = Blt_ListLastItem(&(graphPtr->elemList)); item != NULL;
	    item = Blt_ListPrevItem(item)) {
	    elemPtr = (Element *)Blt_ListGetValue(item);
	    if (!elemPtr->hidden) {
		(*elemPtr->infoPtr->closestProc) (graphPtr, elemPtr, &info);
	    }
	}

    }
    if (info.dist < (double)info.halo) {
	char string[200];
	/*
	 *  Return an array of 5 elements
	 */
	if (Tcl_SetVar2(interp, argv[5], "name",
		info.elemPtr->name, flags) == NULL) {
	    return TCL_ERROR;
	}
	sprintf(string, "%d", info.index);
	if (Tcl_SetVar2(interp, argv[5], "index", string, flags) == NULL) {
	    return TCL_ERROR;
	}
	Tcl_PrintDouble(interp, info.point.x, string);
	if (Tcl_SetVar2(interp, argv[5], "x", string, flags) == NULL) {
	    return TCL_ERROR;
	}
	Tcl_PrintDouble(interp, info.point.y, string);
	if (Tcl_SetVar2(interp, argv[5], "y", string, flags) == NULL) {
	    return TCL_ERROR;
	}
	Tcl_PrintDouble(interp, info.dist, string);
	if (Tcl_SetVar2(interp, argv[5], "dist", string, flags) == NULL) {
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, "1", TCL_STATIC);
    } else {
	if (Tcl_SetVar2(interp, argv[5], "name", "", flags) == NULL) {
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, "0", TCL_STATIC);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 *	Sets the element specifications by the given the command line
 *	arguments and calls the element specification configuration
 *	routine. If zero or one command line options are given, only
 *	information about the option(s) is returned in interp->result.
 *      If the element configuration has changed and the element is
 *	currently displayed, the axis limits are updated and recomputed.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Element *elemPtr;
    int flags;
    int numNames, numOpts;
    char **options;
    register int i;
    Tk_ConfigSpec *specsPtr;

    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] == '-') {
	    break;
	}
	if (Blt_NameToElement(graphPtr, argv[i], &elemPtr) != TCL_OK) {
	    return TCL_ERROR;	/* Can't find named element */
	}
    }
    numNames = i;		/* Number of element names specified */
    numOpts = argc - i;		/* Number of options specified */
    options = argv + numNames;	/* Start of options in argv  */

    flags = TK_CONFIG_ARGV_ONLY | GraphType(elemPtr);
    for (i = 0; i < numNames; i++) {
	Blt_NameToElement(graphPtr, argv[i], &elemPtr);
	specsPtr = elemPtr->infoPtr->configSpecs;
	if (numOpts == 0) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin, specsPtr,
		    (char *)elemPtr, (char *)NULL, flags));
	} else if (numOpts == 1) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin, specsPtr,
		    (char *)elemPtr, options[0], flags));
	}
	if (Tk_ConfigureWidget(interp, graphPtr->tkwin, specsPtr, numOpts,
		options, (char *)elemPtr, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((*elemPtr->infoPtr->configProc) (graphPtr, elemPtr) != TCL_OK) {
	    return TCL_ERROR;	/* Failed to configure element */
	}
	if (Blt_ConfigModified(specsPtr, "-hide", (char *)NULL)) {
	    Blt_ListItem item;

	    item = Blt_ListFind(&(graphPtr->elemList), elemPtr->name);
	    if ((elemPtr->hidden) != (item == NULL)) {
		/*
		 * The element's "hidden" variable is out of sync with the
		 * display list. [That's what you get for having two ways
		 * to do the same thing.] Update the display list by
		 * either by adding or removing the element.
		 */
		if (item == NULL) {
		    Blt_ListAppend(&(graphPtr->elemList), elemPtr->name,
			(ClientData)elemPtr);
		} else {
		    Blt_ListDeleteItem(item);
		}
	    }
	    graphPtr->flags |= RESET_AXES;
	    elemPtr->flags |= COORDS_NEEDED;
	}
	/*
	 * If data points or axes have changed, reset the axes (may affect
	 * autoscaling) and recalculate the screen points of the element.
	 */
	if (Blt_ConfigModified(specsPtr, "-*data", "-map*", (char *)NULL)) {
	    graphPtr->flags |= RESET_AXES;
	    elemPtr->flags |= COORDS_NEEDED;
	}
	/* The new label may change the size of the legend */
	if (Blt_ConfigModified(specsPtr, "-label", (char *)NULL)) {
	    graphPtr->flags |= (COORDS_WORLD | REDRAW_WORLD);
	}
    }
    /* Update the pixmap if any configuration option changed */
    graphPtr->flags |= REDRAW_BACKING_STORE | REDRAW_MARGINS;
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeactivateOp --
 *
 *	Clears the active bit for the named elements.
 *
 * Results:
 *	Returns TCL_OK if no errors occurred.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeactivateOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;		/* not used */
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    register int i;

    for (i = 3; i < argc; i++) {
	if (Blt_NameToElement(graphPtr, argv[i], &elemPtr) != TCL_OK) {
	    return TCL_ERROR;	/* Can't find named element */
	}
	elemPtr->flags &= ~ELEM_ACTIVE;
	if (elemPtr->reqActiveArr != NULL) {
	    free((char *)elemPtr->reqActiveArr);
	    elemPtr->reqActiveArr = NULL;
	}
	elemPtr->reqNumActive = 0;
    }
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteOp --
 *
 *	Delete the named elements from the graph.
 *
 * Results:
 *	TCL_ERROR is returned if any of the named elements can not be found.
 *	Otherwise TCL_OK is returned;
 *
 * Side Effects:
 *	If the element is currently displayed, the plotting area of the
 *	graph is redrawn. Memory and resources allocated by the elements
 *	are released.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;		/* not used */
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    register int i;

    for (i = 3; i < argc; i++) {
	if (Blt_NameToElement(graphPtr, argv[i], &elemPtr) != TCL_OK) {
	    return TCL_ERROR;	/* Can't find named element */
	}
	DestroyElement(graphPtr, elemPtr);
    }
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExistsOp --
 *
 *	Runs through the given list of element entries and builds a
 *	Tcl list of element names.  This procedure is used in the
 *	"names" and "show" commands.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *	interp->result contains the list of element names.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
ExistsOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* unused */
    char **argv;
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
    Tcl_SetResult(interp, (hPtr == NULL) ? "0" : "1", TCL_STATIC);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOp --
 *
 * 	Find the legend entry from the given argument.  The argument
 *	can be either a screen position "@x,y" or the name of an
 *	element.
 *
 *	I don't know how useful it is to test with the name of an
 *	element.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
GetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char *argv[];
{
    register Element *elemPtr;

    if ((argv[3][0] == 'c') && (strcmp(argv[3], "current") == 0)) {
	elemPtr = (Element *)Blt_GetCurrentItem(graphPtr->bindTable);
	/* Report only on elements. */
	if ((elemPtr != NULL) && (elemPtr->type > TYPE_UNKNOWN) &&
	    (elemPtr->type < TYPE_MARKER_BITMAP)) {
	    Tcl_SetResult(interp, elemPtr->name, TCL_STATIC);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NamesOp --
 *
 *	Runs through the given list of element entries and builds a
 *	Tcl list of element names.  This procedure is used in the
 *	"names" and "show" commands.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *	interp->result contains the list of element names.
 *
 *----------------------------------------------------------------------
 */
static int
NamesOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Element *elemPtr;
    Tcl_HashSearch cursor;
    register Tcl_HashEntry *hPtr;
    register int i;

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	if (argc == 3) {
	    Tcl_AppendElement(graphPtr->interp, elemPtr->name);
	    continue;
	}
	for (i = 3; i < argc; i++) {
	    if (Tcl_StringMatch(elemPtr->name, argv[i])) {
		Tcl_AppendElement(interp, elemPtr->name);
		break;
	    }
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ShowOp --
 *
 *	Displays or rebuilds the element display list.
 *
 * Results:
 *	The return value is a standard Tcl result. interp->result
 *	will contain the new display list of element names.
 *
 *----------------------------------------------------------------------
 */
static int
ShowOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Element *elemPtr;
    register Blt_ListItem item;

    if (argc == 4) {
	if (RebuildDisplayList(graphPtr, argv[3]) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	Tcl_AppendElement(interp, elemPtr->name);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TypeOp --
 *
 *	Returns the name of the type of the element given by some
 *	element name.
 *
 * Results:
 *	A standard Tcl result. Returns the type of the element in
 *	interp->result. If the identifier given doesn't represent
 *	an element, then an error message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TypeOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;		/* Element name */
{
    Element *elemPtr;

    if (Blt_NameToElement(graphPtr, argv[3], &elemPtr) != TCL_OK) {
	return TCL_ERROR;	/* Can't find named element */
    }
    Tcl_SetResult(interp, Blt_NameOfElementType(elemPtr->type), TCL_STATIC);
    return TCL_OK;
}

/*
 * Global routines:
 */

static Blt_OpSpec elemOps[] =
{
    {"activate", 1, (Blt_Operation)ActivateOp, 3, 0, "?elemName? ?index...?",},
    {"bind", 1, (Blt_Operation)BindOp, 4, 6, "elemName sequence command",},
    {"cget", 2, (Blt_Operation)CgetOp, 5, 5, "elemName option",},
    {"closest", 2, (Blt_Operation)ClosestOp, 6, 0,
	"x y varName ?option value?... ?elemName?...",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 4, 0,
	"elemName ?elemName?... ?option value?...",},
    {"create", 2, (Blt_Operation)CreateOp, 4, 0, "elemName ?option value?...",},
    {"deactivate", 3, (Blt_Operation)DeactivateOp, 3, 0, "?elemName?...",},
    {"delete", 3, (Blt_Operation)DeleteOp, 3, 0, "?elemName?...",},
    {"exists", 1, (Blt_Operation)ExistsOp, 4, 4, "elemName",},
    {"get", 1, (Blt_Operation)GetOp, 4, 4, "name",},
    {"names", 1, (Blt_Operation)NamesOp, 3, 0, "?pattern?...",},
    {"show", 1, (Blt_Operation)ShowOp, 3, 4, "?elemList?",},
    {"type", 1, (Blt_Operation)TypeOp, 4, 4, "elemName",},
};
static int numElemOps = sizeof(elemOps) / sizeof(Blt_OpSpec);


/*
 *--------------------------------------------------------------
 *
 * Blt_ElementOp --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
int
Blt_ElementOp(graphPtr, interp, argc, argv, type)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* # arguments */
    char **argv;		/* Argument list */
    ObjectType type;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numElemOps, elemOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    if (proc == CreateOp) {
	result = CreateOp(graphPtr, interp, argc, argv, type);
    } else {
	result = (*proc) (graphPtr, interp, argc, argv);
    }
    return (result);
}
