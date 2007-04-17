#ifndef _astroCatalog_h_
#define _astroCatalog_h_

/*
 * E.S.O. - VLT project 
 * $Id: astro_catalog.h,v 1.1.1.1 2006/01/12 16:36:39 abrighto Exp $
 *
 * astro_catalog.h - C interface to C++ class AstroCatalog
 *
 * (Note: C applications must have at least a dummy C++ main and link
 * with C++)
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include "world_coords.h"


/* handle for an open catalog */
typedef void* AcHandle; 

/* handle for accessing the result of a query */
typedef void* AcResult;


/* 
 * open the named catalog and return a handle for it or NULL if there 
 * were errors
 */
AcHandle acOpen(char* name);

/* close the catalog and free its resources */
void acClose(AcHandle);

/* 
 * return true if there would have been more than "nrows" available
 * in the last query 
 */
int acMore(AcHandle);

/* return the number of colums in the catalog */
int acNumCols(AcHandle);

/* return the name of the given column in the catalog */
char* acColName(AcHandle, int col);

/* return a pointer to an array with the names of the columns in the catalog*/
char** acColNames(AcHandle);

/* return the index for the given column name the catalog */
int acColIndex(AcHandle handle, char* name);

/* 
 * Get the number of columns, the column names
 * for the given catalog and return 0 if all is OK
 */
int acGetDescription(
    AcHandle cat,		/* in  - catalog handle */
    int* numCols,		/* out - number of result columns */
    char*** colNames);		/* out - ptr to array of column names */
   
/*
 * Get the values for the specified columns for the object given by "id"
 * in the given catalog and return 0 if all is OK
 */
int acGetObject(
    AcHandle cat,		/* in  - catalog handle */
    char* id,			/* in  - object id in catalog */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - array[numCols] of column names to read */
    AcResult* result);		/* out - handle for accessing query results (see below) */
    

/*
 * Get the values for all objects in the specified world coordinates area
 */
int acGetArea(
    AcHandle cat,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - array[numCols] of column names to read */
    double ra0,			/* in  - coordinates of area */
    double dec0,		
    double ra1,		
    double dec1,
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    int maxRows,		/* in  - max number of rows to return */
    char* filename,		/* in  - if not null, write results to this file */
    int* numFound,		/* out - number of objects found  */
    AcResult* result);		/* out - handle for accessing query results (see below) */


/* 
 * return a pointer to the most recent error message
 */
char* acGetError();


/* 
 * return the error code (from <sys/errno.h>) for the most recent error
 */
int acGetErrorCode();


/*
 * Get the values for all objects in the specified circle/ring.
 */
int acCircularSearch(
    AcHandle cat,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - array[numCols] of column names to read */
    double ra,			/* in  - center position in world coordinates */
    double dec,		
    double radius0,		/* in  - min radius */
    double radius1,		/* in  - max radius */
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    int maxRows,		/* in  - max number of rows to return */
    char* filename,		/* in  - if not null, write results to this file */
    int* numFound,		/* out - number of objects found */
    AcResult* result);		/* out - handle for accessing query results (see below) */


/* 
 * search for the star closest to the given position, with the magnitude in 
 * the given range and return (via the last 2 args) the columns requested
 * by "colNames"
 */
int acSearchClosestStar(
    AcHandle cat,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - array[numCols] of column names to read */
    double ra,			/* in  - center position in world coordinates */
    double dec,		
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    AcResult* result);		/* out - handle for accessing query results (see below) */


/* 
 * search for the stars fulfilling the specified criteria
 */
int acCatalogSearch(
        AcHandle cat,		/* in  - catalog handle */
	int numCols,		/* in  - number of columns to get */
	char** colNames,	/* in  - array[numCols] of column names to read  */
	int numSearchCols,	/* in  - number of search columns */
	char** searchCols,	/* in  - array of column names to search  */
	char** minVals,		/* in  - optional array of min values  */
	char** maxVals,		/* in  - optional array of max values  */
	int maxRows,		/* in  - max number of rows to return  */
	const char* filename,	/* in  - if not null, write results to this file  */
	int* numFound,		/* out - number of objects found */
	AcResult* result);	/* out - handle for accessing query results (see below) */

/*
 * pass a request to the catalog and return the name of a FITS file
 * containing the resulting image, or NULL if not found
 *
 * Args:
 *
 * handle - handle returned from ac_open()
 *
 * ra, dec - world coordinates position
 *
 * width, height - dimensions of image to return.
 *
 * The return filename is the name of a temporary file that will
 * be reused on the next call to this routine.
 */
char* acGetImage(AcHandle handle, double ra, double dec, 
		 double width, double height);

/*
 *  --- routines for accessing the query results ---
 */

/* return number of result rows */
int acrNumRows(AcResult);

/* return number of result columns */
int acrNumCols(AcResult);

/* return a pointer to an array of result column names */
char** acrColNames(AcResult);

/*
 * Get result values:
 *
 * Note: there are various versions for different data types and parameters.
 * All return 0 for success and set the last argument value or return
 * 1 for error.
 */

/* get result values by row and column index */
int acrGetString(AcResult, int row, int col, char** value);
int acrGetInt(AcResult, int row, int col, int* value);
int acrGetDouble(AcResult, int row, int col, double* value);
int acrGetFloat(AcResult, int row, int col, float* value);
int acrGetShort(AcResult,int row, int col, short* value);
int acrGetChar(AcResult,int row, int col, char* value);


/* get result values by row and column name */
int acrGetNString(AcResult,int row, const char* colName, char** value);
int acrGetNInt(AcResult,int row, const char* colName, int* value);
int acrGetNDouble(AcResult,int row, const char* colName, double* value);
int acrGetNFloat(AcResult,int row, const char* colName, float* value);
int acrGetNShort(AcResult,int row, const char* colName, short* value);
int acrGetNChar(AcResult,int row, const char* colName, char* value);


/*
 * if the result contains a wcs position (fields ra and dec),
 * get it and return success (0)
 */
int acrGetWC(AcResult, int row, WC* pos);


/*
 * return the result column index for the given result column name
 */
int acrColIndex(AcResult,const char* colName);



/*
 * delete the result object (free the memory)
 */
void acrDelete(AcResult);


#endif /* __astroCatalog_h_ */
