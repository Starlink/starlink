/*
 * bltGrElem.h --
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

#ifndef _ELEMENT_H
#define _ELEMENT_H

/*
 * The data structure below contains information pertaining to a line vector.
 * It consists of an array of floating point data values and for convenience,
 * the number and minimum/maximum values.
 */

typedef struct ElemVector {
    Blt_Vector *vecPtr;
    double *valueArr;
    int numValues;
    int arraySize;
    double min, max;

    Blt_VectorId clientId;	/* If non-NULL, a client token identifying the
				 * external vector. */
    Element *elemPtr;		/* Element vector is associated with */

} ElemVector;


/*
 * An element has one or more vectors plus several attributes, such as line
 * style, thickness, color, and symbol type.  It has an identifier which
 * distinguishes it among the list of all elements.
 */
typedef struct ClosestSearch ClosestSearch;

typedef void (ElemDrawProc) _ANSI_ARGS_((Graph *graphPtr, Drawable drawable,
	Element *elemPtr));
typedef void (ElemPrintProc) _ANSI_ARGS_((Graph *graphPtr, Printable printable,
	Element *elemPtr));
typedef void (ElemDestroyProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr));
typedef int (ElemConfigProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr));
typedef void (ElemTransformProc) _ANSI_ARGS_((Graph *graphPtr,
	Element *elemPtr));
typedef void (ElemExtentsProc) _ANSI_ARGS_((Element *elemPtr, Extents2D * extsPtr));
typedef void (ElemClosestProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr,
	ClosestSearch * searchPtr));
typedef void (ElemDrawSymbolProc) _ANSI_ARGS_((Graph *graphPtr, 
	Drawable drawable, Element *elemPtr, int x, int y, int symbolSize));
typedef void (ElemPrintSymbolProc) _ANSI_ARGS_((Graph *graphPtr, 
	Printable printable, Element *elemPtr, int x, int y, int symbolSize));

typedef struct ElemClassInfo {
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */

    ElemClosestProc *closestProc;
    ElemConfigProc *configProc;
    ElemDestroyProc *destroyProc;
    ElemDrawProc *drawActiveProc;
    ElemDrawProc *drawNormalProc;
    ElemDrawSymbolProc *drawSymbolProc;
    ElemExtentsProc *extentsProc;
    ElemPrintProc *printActiveProc;
    ElemPrintProc *printNormalProc;
    ElemPrintSymbolProc *printSymbolProc;
    ElemTransformProc *transformProc;

} ElemClassInfo;

struct Element {
    char *name;			/* Identifier to refer the element. Used in the
				 * "insert", "delete", or "show", commands. */
    ObjectType type;		/* Type of element; either TYPE_ELEM_BAR, 
				 * TYPE_ELEM_LINE, or TYPE_ELEM_STRIP */
    Graph *graphPtr;		/* Graph widget of element*/
    unsigned int flags;		/* Indicates if the entire element is active, or
				 * if coordinates need to be calculated */
    char **tags;
    int hidden;			/* If non-zero, don't display the element. */

    Tcl_HashEntry *hashPtr;
    char *label;		/* Label displayed in legend */
    int labelRelief;		/* Relief of label in legend. */

    Axis2D axes;		/* X-axis and Y-axis mapping the element */
    ElemVector x, y, w;		/* Contains array of floating point graph
				 * coordinate values. Also holds min/max and
				 * the number of coordinates */
    int *reqActiveArr;		/* Array of indices (malloc-ed) which indicate
				 * which data points are active (drawn with
				 * "active" colors). */
    int reqNumActive;		/* Number of active data points. Special case:
				 * if reqNumActive < 0 and the active bit is
				 * set in "flags", then all data points are
				 * drawn active. */
    ElemClassInfo *infoPtr;

};

#define	COORDS_NEEDED 	(1<<0)	/* Indicates that the element's configuration
				 * has changed such that its layout of the
				 * element (i.e. its position in the graph
				 * window) needs to be recalculated. */

#define	ELEM_ACTIVE	(1<<8)	/* Non-zero indicates that the element should be
				 * drawn in its active foreground and background
				 * colors. */
#define	ELEM_UPDATE_ACTIVE	(1<<7)

#define	LABEL_ACTIVE 	(1<<9)	/* Non-zero indicates that the element's entry
				 * in the legend should be drawn in its active
				 * foreground and background colors. */

struct ClosestSearch {
    int halo;			/* Maximal distance a candidate point can be
				 * from the sample window coordinate */
    int interpolate;		/* If non-zero, find the closest point on the
				 * entire line segment, instead of the closest
				 * end point. */
    int x, y;			/* Screen coordinates of test point */

    double dist;		/* Distance in screen coordinates */

    /* Output */
    Element *elemPtr;		/* Name of the closest element */
    Point2D point;		/* Graph coordinates of closest point */
    int index;			/* Index of closest data point */
};

typedef struct PenStyle {

    Pen *penPtr;		/* Pen to draw */
    Limits weight;		/* Range of weights */

} PenStyle;

extern double Blt_FindElemVectorMinimum _ANSI_ARGS_((ElemVector *vecPtr,
	double minLimit));
extern void Blt_ResizeStatusArray _ANSI_ARGS_((Element *elemPtr,
	int numPoints));
extern Point2D Blt_GetProjection _ANSI_ARGS_((int x, int y, int px, int py, 
	int qx, int qy));
extern int Blt_GetPenStyle _ANSI_ARGS_((Graph *graphPtr, char *name,  
	ObjectType type, PenStyle *stylePtr));
extern int Blt_NameToElement _ANSI_ARGS_((Graph *graphPtr, char *name,  
	Element **elemPtrPtr));

#define NumberOfPoints(e)	MIN((e)->x.numValues, (e)->y.numValues)

#endif /* _ELEMENT_H */

