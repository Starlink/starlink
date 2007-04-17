/*
 * E.S.O. - VLT project / ESO Archive
 *
 * Blt_GraphElement.c - replacement for the cancelled blt convenience function
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * PBi             21/02/97  Created
 */

#include "tcl.h"

/* 
 * Hack: Rather than have to worry about finding out where blt.h is, 
 * just include the required definitions here (I don't expect this
 * package to change much anymore). -- Allan: 03.01.06
 */
#if 0
#include "blt.h"
#else

typedef struct {
    double *valueArr;		/* Array of values (possibly malloc-ed) */
    int numValues;		/* Number of values in the array */
    int arraySize;		/* Size of the allocated space */
    double min, max;		/* Minimum and maximum values in the vector */
    int dirty;			/* Indicates if the vector has been updated */
    int reserved;		/* Reserved for future use */

} Blt_Vector;
#endif


/* Replacement for the cancelled blt convenience function */
int Blt_GraphElement(
    Tcl_Interp *interp,         /* Interpreter of the graph widget */
    char *pathName,             /* Path name of the graph widget */
    char *elemName,             /* Name of the element to reset */
    int numValues,              /* Number of values in array */
    double *valueArr,           /* Array of x,y coordinate pairs */
    char *xVecName,             /* Name of x array */
    char *yVecName)             /* Name of y array */

{ 
    register int i;
    int num = numValues/2;
    int nbytes = sizeof(double) * num;

    /* Note: Blt_Vector::arraySize is the number of bytes bytes! */
    Blt_Vector *xVecPtr, *yVecPtr;
    double *xArray, *yArray;

    if (Blt_GetVector(interp, xVecName, &xVecPtr) != 0
	|| Blt_GetVector(interp, yVecName, &yVecPtr) != 0)
	return TCL_ERROR;

    /* Allocate space for the new vectors, if needed */
    if (xVecPtr->arraySize < nbytes) {
	xArray = (double *)Tcl_Alloc(nbytes);
	yArray = (double *)Tcl_Alloc(nbytes);
	if (xArray == NULL || yArray == NULL) {
	    fprintf(stderr, "malloc: out of memory\n");
	    return TCL_ERROR;
	}
    } 
    else {
	/* reuse existing arrays */
	xArray = xVecPtr->valueArr;
	yArray = yVecPtr->valueArr;
	nbytes = xVecPtr->arraySize;
    }

    /* Write the data points into the vectors */
    for (i = 0; i < num; i++) {
        xArray[i] = *valueArr++;
        yArray[i] = *valueArr++;
    }

    if ((Blt_ResetVector(xVecPtr, xArray, num, nbytes, TCL_DYNAMIC) != TCL_OK) ||
        (Blt_ResetVector(yVecPtr, yArray, num, nbytes, TCL_DYNAMIC) != TCL_OK)) {
        return TCL_ERROR;
    }

    return TCL_OK;
}
