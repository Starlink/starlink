/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: astro_catalog.C,v 1.1.1.1 2006/01/12 16:36:39 abrighto Exp $
 *
 * astroCatalog.C - C interface implementation for C++ class AstroCatalog
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Oct 95  Created
 */
static const char* const rcsId="@(#) $Id: astro_catalog.C,v 1.1.1.1 2006/01/12 16:36:39 abrighto Exp $";


using namespace std;
#include <iostream>
#include <errno.h>
#include "error.h"

// include the C++ and C interfaces
#include "AstroCatalog.h"
extern "C" {
#include "astro_catalog.h"
}


/* 
 * check that the given catalog handle is not null and return its
 * error status
 */
static int acCheckHandle(AcHandle handle)
{
    if (handle)
	return ((AstroCatalog*)handle)->status();
    return error("internal error: ", "bad catalog handle", EINVAL);
}


/* 
 * check that the given query result handle is not null and return its
 * error status
 */
static int acCheckResult(AcResult handle)
{
    if (handle)
	return ((QueryResult*)handle)->status();
    return error("internal error: ", "bad query result handle", EINVAL);
}


/* 
 * open the named catalog and return a handle for it or NULL if there 
 * were errors
 */
AcHandle acOpen(char* name)
{
    return (AcHandle)AstroCatalog::open(name);
}


/* close the catalog and free its resources */
void acClose(AcHandle handle)
{
    if (handle)
	delete (AstroCatalog*)handle;
}


/* 
 * return true if there would have been more than "maxRows" available
 * in the last query
 */
int acMore(AcHandle handle)
{
    if (acCheckHandle(handle) != OK)
	return ERROR;
    return ((AstroCatalog*)handle)->more();
}


/* return the number of colums in the catalog */
int acNumCols(AcHandle handle)
{
    if (acCheckHandle(handle) != OK)
	return ERROR;
    return ((AstroCatalog*)handle)->numCols();
}


/* return the name of the given column in the catalog */
char* acColName(AcHandle handle, int col)
{
    if (acCheckHandle(handle) != OK)
	return NULL;
    return (char*)((AstroCatalog*)handle)->colName(col);
}


/* return a pointer to an array with the names of the columns in the catalog*/
char** acColNames(AcHandle handle)
{
    if (acCheckHandle(handle) != OK)
	return NULL;
    return (char**)((AstroCatalog*)handle)->colNames();
}


/* return the index for the given column name the catalog */
int acColIndex(AcHandle handle, char* name)
{
    if (acCheckHandle(handle) != OK)
	return ERROR;
    return ((AstroCatalog*)handle)->colIndex(name);
}


/*
 * Get the number of columns, the column names and types ("char", "int",
 * "float") for the given catalog and return 0 if all is OK
 */
int acGetDescription(
    AcHandle handle,		/* in  - catalog handle*/
    int* numCols,		/* out - number of result columns */
    char*** colNames)		/* out - ptr to array of column names */
{
    if (acCheckHandle(handle) != OK)
	return ERROR;
    return ((AstroCatalog*)handle)->getDescription(
	*numCols, 
	*colNames);  // we don't enforce the const in C
}

    
/*
 * Get the values for the specified columns for the object given by "id"
 * in the named catalog and return 0 if all is OK
 */
int acGetObject(
    AcHandle handle,		/* in  - catalog handle */
    char* id,			/* in  - object id in catalog */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - null terminated array of column names to read */
    AcResult* result)	        /* out - handle for accessing query results (see below) */
{
    if (acCheckHandle(handle) != OK)
	return ERROR;

    QueryResult* qr = new QueryResult;

    int status = ((AstroCatalog*)handle)->getObject(
	id,
	numCols, 
	colNames, 
	*qr);

    if (status == 0)
	*result = (AcResult)qr;
    return status;
}
    

/*
 * Get the values for all objects in the specified world coordinates area
 */
int acGetArea(
    AcHandle handle,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - null terminated array of column names to read */
    double ra0,			/* in  - coordinates of area */
    double dec0,		
    double ra1,		
    double dec1,
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    int maxRows,		/* in  - max number of rows to return */
    char* filename,		/* in  - if not null, write results to this file */
    int* numFound,		/* out - number of objects found */
    AcResult* result)	        /* out - handle for accessing query results (see below) */
{
    if (acCheckHandle(handle) != OK)
	return ERROR;

    QueryResult* qr = new QueryResult;

    int status = ((AstroCatalog*)handle)->getArea(
	numCols,
	colNames, 
	WorldCoords(ra0, dec0), 
	WorldCoords(ra1, dec1), 
	mag0, 
	mag1, 
	maxRows, 
	filename, 
	*numFound, 
	*qr);

    if (status == 0)
	*result = (AcResult)qr;
    return status;
}



/*
 * pass a request to the image server and return the name of a FITS file
 * containing the resulting image, or NULL if not found
 *
 * Args:
 *
 * handle - handle returned from ac_open()
 *
 * ra, dec - world coordinates position (in deg)
 *
 * width, height - dimensions of image to return (in arcmin).
 *
 * The return filename is the name of a temporary file that will
 * be reused on the next call to this routine.
 *
 * XXX note: this routine should probably return the status instead
 * of the filename (the C++ method returns the status).
 */
char* acGetImage(AcHandle handle, double ra, double dec, 
		  double width, double height)
{
    if (acCheckHandle(handle) != OK)
	return NULL;

    AstroQuery q;
    q.pos(WorldCoords(ra, dec));
    q.width(width);
    q.height(height);
    if (((AstroCatalog*)handle)->getImage(q) != 0)
	return NULL;
    return (char*)(((AstroCatalog*)handle)->tmpfile());
}


/* return the error message for the most recent error */
char* acGetError()
{
    return last_error();
}


/* 
 * return the error code for the most recent error 
 * (see errno.h for the posible error codes)
 */
int acGetErrorCode()
{
    return last_error_code();
}


/*
 * Get the values for all objects in the specified circle/ring.
 */
int acCircularSearch(
    AcHandle handle,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - null terminated array of column names to read */
    double ra,			/* in  - center position in world coordinates */
    double dec,		
    double radius0,		/* in  - min radius */
    double radius1,		/* in  - max radius */
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    int maxRows,		/* in  - max number of rows to return */
    char* filename,		/* in  - if not null, write results to this file */
    int* numFound,		/* out - number of objects found */
    AcResult* result)	        /* out - handle for accessing query results (see below) */
    
{
    if (acCheckHandle(handle) != OK)
	return ERROR;

    QueryResult* qr = new QueryResult;

    int status = ((AstroCatalog*)handle)->circularSearch(
	numCols, 
	colNames, 
	WorldCoords(ra, dec),
	radius0, 
	radius1, 
	mag0, 
	mag1, 
	maxRows, 
	filename, 
	*numFound, 
	*qr);

    if (status == 0)
	*result = (AcResult)qr;
    return status;
}


/* 
 * search for the star closest to the given position, with the magnitude in 
 * the given range and return (via the last 2 args) the columns requested
 * by "colNames"
 */
int acSearchClosestStar(
    AcHandle handle,		/* in  - catalog handle */
    int numCols,		/* in  - number of columns to get (size of colNames) */
    char** colNames,		/* in  - null terminated array of column names to read */
    double ra,			/* in  - center position in world coordinates */
    double dec,		
    double mag0,		/* in  - min magnitude */
    double mag1,		/* in  - max magnitude */
    AcResult* result)	        /* out - handle for accessing query results (see below) */
{
    if (acCheckHandle(handle) != OK)
	return ERROR;

    QueryResult* qr = new QueryResult;

    int status = ((AstroCatalog*)handle)->searchClosestStar(
	numCols, 
	colNames, 
	WorldCoords(ra, dec),
	mag0, 
	mag1, 
	*qr);

    if (status == 0)
	*result = (AcResult)qr;
    return status;
}


/* 
 * search for the stars fulfilling the specified criteria
 */
int acCatalogSearch(
        AcHandle handle,	     /* in  - catalog handle */
	int numCols,		     /* in  - number of columns to get */
	char** colNames,             /* in  - array of column names to read  */
	int numSearchCols,	     /* in  - number of search columns */
	char** searchCols,           /* in  - array of column names to search  */
	char** minVals,              /* in  - optional array of min values  */
	char** maxVals,              /* in  - optional array of max values  */
	int maxRows,		     /* in  - max number of rows to return  */
	const char* filename,	     /* in  - if not null, write results to this file  */
	int* numFound,		     /* out - number of objects found */
	AcResult* result)	     /* out - handle for accessing query results (see below) */
{
    if (acCheckHandle(handle) != OK)
	return ERROR;

    QueryResult* qr = new QueryResult;

    int status = ((AstroCatalog*)handle)->CatalogSearch(
	numCols, 
	colNames, 
	numSearchCols, 
	searchCols, 
	minVals,
	maxVals,
	maxRows, 
	filename, 
	*numFound, 
	*qr);

    if (status == 0)
	*result = (AcResult)qr;
    return status;
}



/*
 *  --- routines for accessing the query results ---
 */


/* return number of result rows */
int acrNumRows(AcResult handle) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->numRows();
}

/* return number of result columns */
int acrNumCols(AcResult handle) {
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->numCols();
}

/* return a pointer to an array of result column names */
char** acrColNames(AcResult handle) { 
    if (acCheckResult(handle) != OK)
	return NULL;
    return (char**)((QueryResult*)handle)->colNames();
}

/*
 * Note: there are various versions for different data types and parameters.
 * All return 0 for success and set the last argument value or return
 * 1 for error.
 */

/* get result values by row and column index */
int acrGetString(AcResult handle, int row, int col, char** value) {
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}
int acrGetInt(AcResult handle, int row, int col, int* value) {
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}
int acrGetDouble(AcResult handle, int row, int col, double* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}
int acrGetFloat(AcResult handle, int row, int col, float* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}
int acrGetShort(AcResult handle,int row, int col, short* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}
int acrGetChar(AcResult handle,int row, int col, char* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, col, *value);
}


/* get result values row and column name */
int acrGetNString(AcResult handle, int row, const char* colName, char** value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}
int acrGetNInt(AcResult handle, int row, const char* colName, int* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}
int acrGetNDouble(AcResult handle, int row, const char* colName, double* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}
int acrGetNFloat(AcResult handle, int row, const char* colName, float* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}
int acrGetNShort(AcResult handle, int row, const char* colName, short* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}
int acrGetNChar(AcResult handle, int row, const char* colName, char* value) { 
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->get(row, colName, *value);
}


/*
 * if the result contains a wcs position (fields ra and dec),
 * get it and return success (0), otherwise return an error.
 *
 * The last 2 args should normally be "ra", "dec".
 */
int acrGetWC(AcResult handle, int row, WC* pos) {

    if (acCheckResult(handle) != OK)
	return ERROR;

    WorldOrImageCoords w;
    int status = ((QueryResult*)handle)->getPos(row, w);
    if (status == 0) {
	pos->ra.hours = w.ra().hours();
	pos->ra.min = w.ra().min();
	pos->ra.sec = w.ra().sec();
	pos->ra.val = w.ra().val();

	pos->dec.hours = w.dec().hours();
	pos->dec.min = w.dec().min();
	pos->dec.sec = w.dec().sec();
	pos->dec.val = w.dec().val();
	return 0;
    }
    return ERROR;
}


/*
 * return the result column index for the given result column name
 */
int acrColIndex(AcResult handle, const char* colName) {
    if (acCheckResult(handle) != OK)
	return ERROR;
    return ((QueryResult*)handle)->colIndex(colName);
}


/*
 * delete the result object (free the memory)
 */
void acrDelete(AcResult handle)
{
    if (acCheckResult(handle) != OK)
	return;
    QueryResult* r = ((QueryResult*)handle);
    if (r)
	delete r;
}
