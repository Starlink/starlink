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
#include "blt.h"

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

    /* There were a lot of changes between tcl7.6/BLT2.1 and tcl8.0/BLT2.4... (allan) */
#if (TCL_MAJOR_VERSION >= 8)

    /* Note: Blt_Vector::arraySize is the number of bytes bytes! */
    Blt_Vector *xVecPtr, *yVecPtr;
    double *xArray, *yArray;

    if (Blt_GetVector(interp, xVecName, &xVecPtr) != 0
	|| Blt_GetVector(interp, yVecName, &yVecPtr) != 0)
	return TCL_ERROR;

    /* Allocate space for the new vectors, if needed */
    if (xVecPtr->arraySize < nbytes) {
	xArray = (double *)malloc(nbytes);
	yArray = (double *)malloc(nbytes);
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

#else  /* BLT2.1/Tcl7.6 */

    /* Note: Blt_Vector::arraySize is the number of doubles! */
    Blt_Vector xVec, yVec; 

    xVec.numValues = yVec.numValues = xVec.arraySize = yVec.arraySize = num;

    /* Allocate space for the new vectors */
    xVec.valueArr = (double *)malloc(nbytes);
    yVec.valueArr = (double *)malloc(nbytes);
    if (xVec.valueArr == NULL || yVec.valueArr == NULL) {
	fprintf(stderr, "malloc: out of memory\n");
        return TCL_ERROR;
    }

    /* Write the data points into the vectors */
    for (i = 0; i < num; i++) {
        xVec.valueArr[i] = *valueArr++;
        yVec.valueArr[i] = *valueArr++;
    }

    if ((Blt_ResetVector(interp, xVecName, &xVec, TCL_DYNAMIC) != TCL_OK) ||
        (Blt_ResetVector(interp, yVecName, &yVec, TCL_DYNAMIC) != TCL_OK)) {
        return TCL_ERROR;
    }
#endif 

    return TCL_OK;
}
