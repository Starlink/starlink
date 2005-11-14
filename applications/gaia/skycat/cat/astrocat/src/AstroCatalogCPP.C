/*
 * E.S.O. - VLT project/ESO Archive
 * $Id$
 *
 * AstroCatalog.C - method definitions for class AstroCatalog
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * Peter W. Draper 01 May 03  Added "ws" and "hs" to get width and
 *                            height in arcsecs (2MASS image servers)
 * Peter W. Draper 10 Dec 03  Moved "delete cat" in nameToWorldCoords
 *                            so that it is performed after the last
 *                            reference to "cat" (->equinox()). 
 *                            Started crashing query subprocess.
 */
static const char* const rcsId="@(#) $Id$";


#include <sys/types.h>
#include <sys/stat.h>
#include <cstdlib>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include "error.h"
#include "Compress.hxx"
#include "WorldOrImageCoords.h"
#include "HTTP.h"
#include "Mem.h"
#include "AstroCatalog.h"
#include "LocalCatalog.h"


/*
 * swap 2 values
 */
static inline void swap(double& x1, double& x2) {
    double tmp = x2;
    x2 = x1;
    x1 = tmp;
}


/*
 * constructor - used internally only, public interface uses "open(name)" 
 * "e" is the catalog config entry object for this catalog 
 * (see CatalogInfo class)
 */
AstroCatalog::AstroCatalog(CatalogInfoEntry* e) 
    : entry_(e),
      status_(OK),
      more_(0),
      tmpfile_(NULL)
{
    newTempFile();
}


/*
 * destructor - close catalog and free any resources
 */
AstroCatalog::~AstroCatalog() 
{
    if (tmpfile_) {
	unlink(tmpfile_);
	free(tmpfile_);
    }
}


/*
 * copy constructor
 */
AstroCatalog::AstroCatalog(const AstroCatalog& a) 
{
    tmpfile_ = a.tmpfile_ ? strdup(a.tmpfile_) : (char*)NULL;
}


/*
 * Return true if the given entry is for a catalog (not an image server or dir)
 */
int AstroCatalog::isCatalog(CatalogInfoEntry* e)
{
    const char* s = e->servType();
    return (strcmp(s, "catalog") == 0
	    || strcmp(s, "local") == 0
	    || strcmp(s, "archive") == 0
	    || strcmp(s, "namesvr") == 0);
}


/*
 * Return true if the given entry is for an image server
 */
int AstroCatalog::isImageServer(CatalogInfoEntry* e)
{
    return (strcmp(e->servType(), "imagesvr") == 0);
}


/*
 * Return true if the given entry is for a local catalog
 */
int AstroCatalog::isLocalCatalog(CatalogInfoEntry* e)
{
    return (strcmp(e->servType(), "local") == 0);
}

/*
 * Generate an error message indicating that the wrong type of
 * catalog was used.
 */
int AstroCatalog::wrongServType(CatalogInfoEntry* e)
{
    return error("This operation is not allowed for catalogs of type ", e->servType());
}


/*
 * Do a dummy query to get the column names for this catalog and set the
 * member variable info_ to the result. The return value is 0 if the
 * query was successful.
 *
 * XXX need a standard query syntax for this ?
 */
int AstroCatalog::getInfo()
{
    if (! isCatalog(entry_))
	return ERROR;

    int more = more_;		// don't want to overwrite this flag here
    AstroQuery q;
    if (entry_->isWcs()) 
	q.pos(WorldCoords(0., 0.)); // dummy world coords
    else if (entry_->isPix())
	q.pos(ImageCoords(0., 0.)); // dummy image coords
    q.maxRows(1);
    int nrows = query(q, NULL, info_);
    more_ = more;		// restore value of this flag
    if (nrows >= 0) 
	return OK;
    return ERROR;
}


/*
 * If we don't have the header info for this catalog, get it and 
 * return the status. 
 */
int AstroCatalog::checkInfo()
{
    if (info_.numCols() > 0)
	return 0;
    return getInfo();
}


/*
 * return the number of columns in the catalog
 */
int AstroCatalog::numCols() 
{
    return ((checkInfo() == OK) ? info_.numCols() : -1);
}


/*
 * return a ptr to an array of catalog column names
 */
char** AstroCatalog::colNames() 
{
    return (char**)((checkInfo() == OK) ? info_.colNames() : NULL);
}


/*
 * return the name of the given column
 */
const char* AstroCatalog::colName(int col) 
{
    return ((checkInfo() == OK) ? info_.colName(col) : (char*)NULL);
}


/*
 *  return the column index for the given column name
 */
int AstroCatalog::colIndex(const char* colName) 
{
    return ((checkInfo() == OK) ? info_.colIndex(colName) : -1);
}


/*
 * This static method is called to open the named catalog and return a
 * pointer to an AstroCatalog object for it, or NULL if errors occur.
 * This is the main entry point to the class.
 */
AstroCatalog* AstroCatalog::open(const char* name) 
{
    // get the entry for this catalog type
    CatalogInfoEntry* e = CatalogInfo::lookup(name);
    if (!e) 
	return NULL;		// error - no config entry

    AstroCatalog* result = NULL;
    if (isLocalCatalog(e)) {
	result = new LocalCatalog(e); // derived class handles local catalog files
    }
    else if (isCatalog(e) || isImageServer(e)) {
	result = new AstroCatalog(e); // this class handles catalogs and image servers
    }
    else {
	fmt_error("'%s' is of type '%s', not a catalog, archive or image server", 
		  name, e->servType());
	return NULL;
    }

    if (result->status() != 0) {
	delete result;
	return NULL;		// error making catalog 
    }
    return result;		// normal return
}


/*
 * static method to use a name server catalog (simbad_ns@eso or ned_ns@eso) 
 * to get the coordinates from the object name. If feedback is not NULL, 
 * status info is written to the given open file.
 * 
 * On success, sets the position arg and returns 0, else 1.
 */
int AstroCatalog::nameToWorldCoords(
	const char* objName, 
	WorldOrImageCoords& pos,
	const char* nameServer, 
	FILE* feedback)
{
    double ra, dec;
    QueryResult result;
    AstroCatalog* cat = AstroCatalog::open(nameServer);

    if (cat == NULL)
	return ERROR;

    if (cat->getObject(objName, 0, NULL, result)) {
	delete cat;
	return ERROR;
    }

    if (result.get(0, cat->ra_col(), ra) 
	|| result.get(0, cat->dec_col(), dec)) {
	delete cat;
	return ERROR;
    }

    pos = WorldCoords(ra, dec, cat->equinox());
    delete cat;
    return 0;
}


/*
 * Pass a query to the catalog and return the number of objects found.
 * Only the given columns are retrieved.
 *
 * Args:
 *     q -        (in)   object describing the query
 *
 *     filename - (in)   filename to hold results, or null
 *
 *     result -   (out)  reference to object used to access the results
 *
 * The return value is the number of rows found, or 0 if none were found.
 * A return value of -1 indicates an error.
 */
int AstroCatalog::query(const AstroQuery& q, const char* filename, QueryResult& result)
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    // generate the URL for a standard query in buf (using ostringstream)
    char* result_buf =  NULL;
    int nlines = 0;

    // if the first URL doesn't work, try the others, if specified
    const char* urls[3];
    urls[0] = entry_->url();
    urls[1] = entry_->backup1();
    urls[2] = entry_->backup2();
    char url[1024];

    char* ctype = "";
    for (int i = 0; i < 3 && urls[i]; i++) {
	if (genHttpQuery(url, sizeof(url), q, urls[i]) != 0) 
	    return -1;

	// send the query
	result_buf =  http_.get(url, nlines);

	ctype = http_.content_type();
	if (!ctype)
	    ctype = "";
	if (result_buf != NULL && strcmp(ctype, "text/html") != 0) 
	    break;
	// don't go to backup URL if it was a request for authorization
	if (http_.authorizationRequired())
	    break;
    }
    if (result_buf == NULL) 
	return -1;		// error in http get

    // check the Content-type of the return data
    if (strcmp(ctype, "text/html") == 0) {
	// most likely an error message
	http_.html_error(result_buf);
	return -1;
    }

    // note the catalog config entry in the results 
    // (This contains important info, such as the location of the id, a and dec cols)
    result.entry(entry_, result_buf);

    if (result.init(result_buf) != 0)
	return -1;		// error

    // sort result ?
    // note: do this before truncating to maxRows to get correct results
    if (q.numSortCols()) 
	result.sort(q.numSortCols(), q.sortCols(), q.sortOrder()); 

    if (q.maxRows() && result.numRows() > q.maxRows()) {
	more_ = 1;
	result.numRows(q.maxRows());
    } else {
	more_ = 0;
    }

    // if we didn't already, note the catalog's column heading info
    if (info_.numCols() <= 0 
	&& info_.init(result.numCols(), result.colNames(), "", 1) != 0) 
	return -1;

    if (filename && result.save(filename) != 0)
	return -1;

    return result.numRows();
}



/*
 * Request an image from the image server based on the given query description
 * and return 0 if all is ok. (Only valid for image servers).
 * The name of a FITS file containing the resulting image can be accessed as
 * this->tmpfile().
 *
 * The catalog config file defines the URL used to get the image. 
 */
int AstroCatalog::getImage(const AstroQuery& q)
{
    if (! isImageServer(entry_))
	return wrongServType(entry_);

    // if the first URL doesn't work, try the others, if specified
    const char* urls[3];
    urls[0] = entry_->url();
    urls[1] = entry_->backup1();
    urls[2] = entry_->backup2();
    char url[1024];

    // for each url, backup-url, etc...
    for (int i = 0; i < 3 && urls[i]; i++) {
	if (genHttpQuery(url, sizeof(url), q, urls[i]) != 0) 
	    return 1;		// error

	if (getImage(url) == 0)
	    return 0;		// normal return

	// don't go to backup URL if it was a request for authorization
	if (http_.authorizationRequired())
	    return 1;		// error
    }
    return 1;			// error, none of the URLs worked
}


/*
 * Given a URL for an image, request the image from the image server and
 * return 0 if all is ok. The name of the FITS file containing the
 * resulting image can be accessed with the method "this->tmpfile()"
 */
int AstroCatalog::getImage(const char* url)
{
    char* ctype = "";
    if (getPreview(url, ctype) == 0 && strcmp(ctype, "image/x-fits") == 0)
	return 0;		// ok
    return 1;			// error
}

 
/*
 * generate the HTTP catalog query string from the AstroQuery object and URL string
 * and write it to the given buffer (buf) of size bufsz.
 *
 * Note: if the query specifies only an object Id, we need to handle a
 *       a search for the specific ID 
 *
 * Note: if 2 positions or a width and height were specified in the query,
 *       we need to return the objects in the "area" 
 *
 * Note: if 1 pos (+ optional mag0, mag1) are specified in the query and 
 *       maxRows is 1, we are looking for the "closest" object 
 *
 * The following substitutions are then performed on the given URL:
 *
 *   %ra, %dec        - world coordinates of center point (for catalogs based in wcs)
 *
 *   %x, %y           - image coordinates of center point (for pixel based catalogs)
 *
 *   %w, %h           - width and height of area in arcmin (area query)
 *
 *   %ws, %hs         - width and height of area in arcsec (area query)
 *
 *   %r1, %r2         - min and max radius (for circular query)
 *
 *   %m1, %m2         - min and max magnitude
 *
 *   %n               - max number of rows to return
 *
 *   %cols            - comma sep. list of columns to return: col1,col2,...coln
 *
 *   %id              - ID field of item to return (if supported)
 *
 *   %mime-type       - value for http mime-type field  
 *
 *   %sort            - insert list of sort columns: col1,col2,...
 *
 *   %sortorder       - insert string: increasing or decreasing
 *
 *   %cond            - insert search condition, if any, in the format
 *                      col1=minVal,maxVal&col2=minVal,maxVal,...
 */
int AstroCatalog::genHttpQuery(char* buf, int bufsz, const AstroQuery& q, const char* url)
{
    if (q.pos().status() != 0)
	return ERROR;

    std::ostringstream os;
    int i;
    int url_has_id = 0, 
	url_has_radec = 0, 
	url_has_xy = 0; 

    // XXX temp - until gsc-server is fixed
    const char* shortName = entry_->shortName();
    if (shortName && strncmp(shortName, "gsc", 3) == 0 
	&& q.mag1() == 0.0 && q.mag2() == 0.0) {
	((AstroQuery&)q).mag(0., 15.);
    }

    // expand the variables in the catalog server command
    while(*url) {
	if (*url == '%') {
	    url++;
	    if (*url == '%') {	// make "%%" expand to "%"
		os << '%';
		url ++;
	    }
	    else if (strncmp(url, "id", 2) == 0) {
		os << q.id();
		url += 2;
		url_has_id++;
	    }
	    else if (strncmp(url, "ra", 2) == 0) {
		os << q.pos().ra();
		url += 2;
		url_has_radec++;
	    }
	    else if (strncmp(url, "dec", 3) == 0) {
		os << q.pos().dec(); 
		url += 3;
		url_has_radec++;
	    }
	    else if (*url == 'x') {
		os << q.pos().x();
		url++;
		url_has_xy++;
	    }
	    else if (*url == 'y') {
		os << q.pos().y();
		url++;
		url_has_xy++;
	    }
	    else if (strncmp(url, "r1", 2) == 0) {
		if (q.radius1() != 0.0 || q.radius2() != 0.0)
		    os << q.radius1();
		url += 2;
	    }
	    else if (strncmp(url, "r2", 2) == 0) {
		if (q.radius1() != 0.0 || q.radius2() != 0.0)
		    os << q.radius2();
		url += 2;
	    }
	    else if (strncmp(url, "m1", 2) == 0) {
		if (q.mag1() != 0.0 || q.mag2() != 0.0)
		    os << q.mag1();
		url += 2;
	    }
	    else if (strncmp(url, "m2", 2) == 0) {
		if (q.mag1() != 0.0 || q.mag2() != 0.0)
		    os << q.mag2();
		url += 2;
	    }
	    else if (*url == 'n') {
		// request one more row than maxRows, 
		// so we can determine if that was all...
		if (q.maxRows() > 0)
		    os << q.maxRows()+1;
		url++;
	    }
	    else if (strncmp(url, "ws", 2) == 0) {
		if (q.width() != 0.0 || q.height() != 0.0)
		    os << q.width() * 60.0;
		url++;
	    }
	    else if (strncmp(url, "hs", 2) == 0) {
		if (q.width() != 0.0 || q.height() != 0.0)
		    os << q.height() * 60.0;
		url++;
	    }
	    else if (strncmp(url, "w", 1) == 0) {
		if (q.width() != 0.0 || q.height() != 0.0)
		    os << q.width();
		url++;
	    }
	    else if (strncmp(url, "h", 1) == 0) {
		if (q.width() != 0.0 || q.height() != 0.0)
		    os << q.height();
		url++;
	    }
	    else if (strncmp(url, "cols", 4) == 0) {
		// insert a list of column names
		for (i = 0; i < q.numCols(); i++) {
		    os << q.colName(i);
		    if (q.numCols() - i > 1)
			os << ',';
		}
		url += 4;
	    }
	    else if (strncmp(url, "sortorder", 9) == 0) {
		os << ((q.sortOrder() < 0) ? "decreasing" : "increasing");
		url += 9;
	    }
	    else if (strncmp(url, "sort", 4) == 0) {
		// insert a list of sort column names
		// XXX note: not all servers may accept the list...
		int n = q.numSortCols();
		if (n > 0) {
		    char** ar = q.sortCols();
		    for (i = 0; i < n; i++) {
			os << ar[i];
			if (n - i > 1)
			    os << ',';
		    }
		}
		url += 4;
	    } 
	    else if (strncmp(url, "cond", 4) == 0) {
		// insert a list of condition column names and min/max values
		int n = q.numSearchCols();
		if (n > 0) {
		    char** arcols = q.searchCols();
		    char** armin = q.minValues();
		    char** armax = q.maxValues();
		    for (i = 0; i < n; i++) {
			// if min and max are the same or max is empty, assume a
			// single value, otherwise a range of values
			if (strcmp(armin[i], armax[i]) == 0 || strlen(armax[i]) == 0)
			    os << arcols[i] << "=" << armin[i];
			else 
			    os << arcols[i] << "=" << armin[i] << "," << armax[i];
			if (n - i > 1)
			    os << '&';
		    }
		}
		url += 4;
	    } 
	    else if (strncmp(url, "mime-type", 9) == 0) {
		os << "application/x-fits"; // should be hard coded in the config file?
		url += 9;
	    }
	    else {
		// if it is not recognized, ignore, don't substitue...
	    }
	}
	else {
	    os << *url++;
	}
    }
    strncpy(buf, os.str().c_str(), bufsz);
    
    // report an error if the caller specified an id, but there is none in the URL
    if (strlen(q.id()) && ! url_has_id) 
	return fmt_error("%s does not suppport search by id", name());

    // report an error if the caller supplied a position, but there is none in the URL 
    if (!q.pos().isNull()) {
	if (q.pos().isWcs() && !url_has_radec) 
	    return fmt_error("%s does not suppport search by World Coordinates", name());

	if (!q.pos().isWcs() && !url_has_xy) 
	    return fmt_error("%s does not suppport search by image coordinates", name());
    }

    return 0;
}


/*
 * Get the number of columns and the column names
 * for the this catalog and return 0 if all is OK.
 *
 * Args:
 *
 *   numCols,	out - number of result columns 
 *   colNames,	out - reference to array of column names 
 *   colTypes,	out - reference to array of column type names 
 */
int AstroCatalog::getDescription(int& numCols, char**& colNames)
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    if (checkInfo() == OK) {
	numCols = info_.numCols();   
	colNames  = (char**)info_.colNames();
	return 0;
    }
    return error("couldn't get catalog info");
}

    
/*
 * Get the values for the specified columns for the object given by "id"
 * in the catalog and return 0 if all is OK.
 *
 * Args:
 *
 *   id		in  - object id in catalog 
 *   numCols	in  - number of columns to get
 *   colNames	in  - array of column names to read 
 *   result	out - reference to object managing result rows 
 */
int AstroCatalog::getObject(
    const char* id, 
    int numCols, 
    char** colNames, 
    QueryResult& result)	
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    AstroQuery q;
    q.id(id);
    q.colNames(numCols, colNames);
    q.maxRows(1);
    
    int nrows = query(q, NULL, result);
    if (nrows < 0) 
	return 1;		// error return
    
    return 0;
}
    

/*
 * Get the values for all objects in the specified world coordinates area
 * Note: This routines returns the number of rows found (in numFound).
 *
 * Args:
 *
 *   numCols 	 in  - number of columns to get
 *   colNames	 in  - array of column names to read 
 *   pos0	 in  - coordinates of area - first point
 *   pos1	 in  - second point of area
 *   mag0	 in  - min magnitude 
 *   mag1	 in  - max magnitude 
 *   maxRows	 in  - max number of rows to return 
 *   filename	 in  - if not null, write results to this file 
 *   result	 out - reference to object managing result rows 
 */
int AstroCatalog::getArea(
    int numCols, 
    char** colNames, 
    const WorldOrImageCoords& pos0, 
    const WorldOrImageCoords& pos1, 
    double mag0, 
    double mag1, 
    int maxRows, 
    const char* filename,
    int& numFound, 
    QueryResult& result)	
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    AstroQuery q;
    q.pos(pos0, pos1);
    q.colNames(numCols, colNames);
    q.maxRows(maxRows);

    numFound = query(q, filename, result);

    if (numFound < 0) 
	return 1;		// error

    return 0;
}


/*
 * Get the values for all objects in the specified circle/ring.
 * Note: This routines returns the number of rows found (in numFound). 
 *
 * Args:
 *
 *  numCols	 in  - number of columns to get
 *  colNames	 in  - array of column names to read 
 *  pos		 in  - center position in world coordinates
 *  radius0	 in  - min radius 
 *  radius1	 in  - max radius 
 *  mag0	 in  - min magnitude 
 *  mag1	 in  - max magnitude 
 *  maxRows	 in  - max number of rows to return 
 *  filename	 in  - if not null, write results to this file 
 *  numFound	 out - number of objects found
 *  result	 out - reference to object managing result rows 
 */
int AstroCatalog::circularSearch(
    int numCols, 
    char** colNames, 
    const WorldOrImageCoords& pos,
    double radius0, 
    double radius1,
    double mag0, 
    double mag1, 
    int maxRows,
    const char* filename, 
    int& numFound,
    QueryResult& result)	
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    AstroQuery q;
    q.pos(pos);
    q.radius(radius0, radius1);
    q.mag(mag0, mag1);
    q.colNames(numCols, colNames);
    q.maxRows(maxRows);
    
    numFound = query(q, filename, result);

    if (numFound < 0) 
	return 1;		// error

    return 0;
}


/*
 * search for the star closest to the given position, with the magnitude in 
 * the given range and return (via the last 2 args) the columns requested
 * by "colNames"
 *
 * Args:
 *
 *  numCols	 in  - number of columns to get
 *  colNames	 in  - array of column names to read 
 *  pos		 in  - center position in world coordinates
 *  mag0	 in  - min magnitude 
 *  mag1	 in  - max magnitude 
 *  result	 out - reference to object managing result rows 
 */
int AstroCatalog::searchClosestStar(
    int numCols, 
    char** colNames, 
    const WorldOrImageCoords& pos,
    double mag0, 
    double mag1, 
    QueryResult& result)	
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    AstroQuery q;
    q.pos(pos);
    q.mag(mag0, mag1);
    q.colNames(numCols, colNames);
    q.maxRows(1);

    int nrows = query(q, NULL, result);

    if (nrows < 0) 
	return 1;		// error return
    
    return 0;
}


/*
 * search for the stars fulfilling the specified criteria
 *
 * Args:
 *
 *  numCols  	   in  - number of columns to get
 *  colNames	   in  - array of column names to get 
 *
 *  numSearchCols  in  - number of search columns (for to minVals, maxVals)
 *  searchCols	   in  - array of search column names for conditions 
 *                      (for minVals, maxVals below)
 *  minVals	   in  - optional array of min values for searchCols
 *  maxVals	   in  - optional array of max values for searchCols
 *
 *  maxRows	   in  - max number of rows to return 
 *  filename	   in  - if not null, write results to this file 
 *  numFound	   out - number of objects found
 *  result	   out - reference to object managing result rows 
 */
int AstroCatalog::CatalogSearch(
    int numCols, 
    char** colNames,
    int numSearchCols, 
    char** searchCols, 
    char** minVals,
    char** maxVals, 
    int maxRows, const char* filename,
    int& numFound, 
    QueryResult& result)	
{
    if (! isCatalog(entry_))
	return wrongServType(entry_);

    AstroQuery q;
    q.colNames(numCols, colNames);
    q.condition(numSearchCols, searchCols, minVals, maxVals);
    q.maxRows(maxRows);
    
    numFound = query(q, filename, result);

    if (numFound < 0) 
	return 1;		// error

    return 0;
}


/*
 * Generate a new temp file name for holding images or preview data. (We
 * could reuse it, but if it is still open and/or mmap'ed, it will cause
 * an error). We also check the name to see if the user may have set the
 * tmpfile name, for backward compatibility.
 */
void AstroCatalog::newTempFile()
{
    static int count = 0;		// used to make a unique file name

    if (!tmpfile_ || strncmp(tmpfile_, "/tmp/cat", 8) == 0) {
	// set default temp file for holding preview data
	char buf[80];
	sprintf(buf, "/tmp/cat%d%d.fits", (int)getpid(), count++);
	tmpfile(buf);
    } 
}

 
/*
 * set the name of the temp file used to hold preview data fetched
 * via http and remove the previous temp file, if it exists.
 */
void AstroCatalog::tmpfile(const char* name) 
{
    if (tmpfile_) {
	unlink(tmpfile_);
	free(tmpfile_);  
    }
    tmpfile_ = strdup(name);
}


/*
 * Given a URL pointing to preview data (FITS image or tab table data),
 * request the data from the server and return 0 if all is OK.  On
 * return, if there were no errors, the "ctype" argument is set to the
 * Content-type of the result to indicate the type of data. The data is
 * automatically decompressed if needed (if the content-type is
 * recognized).  The "tmpfile()" method gives the name of the file
 * containing the results on success.
 */
int AstroCatalog::getPreview(const char* url, char*& ctype)
{
    // we need to use a new file, since the old one may be still in use
    // (even after it was deleted, since it may still be open and/or mmapped)
    newTempFile();

    // open the tmp file
    std::ofstream f(tmpfile_);
    if (!f) 
	return sys_error("could not open file for writing: ", tmpfile_);
	
    if (http_.get(url, f) != 0) {
	unlink(tmpfile_);
	return ERROR;
    }
    f.close();

    // check the Content-type of the return data to determine whether it
    // needs to be decompressed and if so, how...
    ctype = http_.content_type();
    if (!ctype)
	ctype = "";
    
    if (strcmp(ctype, "text/html") == 0) {
	// most likely an HTML formatted server error message
	std::ifstream is(tmpfile_);
	unlink(tmpfile_);
	return http_.html_error(is);
    }

    // for now, assume uncompressed table if the Content-type is not recognized
    char* t = ctype;
    int is_image = 0;
    if (strncmp(ctype, "image/", 6) == 0) {
	t = ctype+6;
	is_image++;
    }
    else if (strncmp(ctype, "text/", 5) == 0) {
	t = ctype+5;
    }
    else {
	// unknown content type, check if it might be a FITS file
	Mem m(tmpfile_);
	if (m.status() == 0 
	    && m.size() >= 2880 
	    && strncmp((const char*)m.ptr(), "SIMPLE", 6) == 0) {
	    ctype = "image/x-fits";   // assume FITS file
	    is_image++;
	} else {
	    ctype = "text/x-starbase"; // assume catalog data
	}
	return 0;
    }
    
    // In some cases the Content-type only gives the general type and
    // we need to check the Content-Encoding also. For example "file.fits.gz"
    // might have a Content-type of image/x-fits and Content-Encoding of
    // x-gzip
    char* ce = http_.content_encoding();
    if (is_image && strcmp(t, "x-fits") == 0 && ce != NULL) {
	if (strcmp(ce, "x-gzip") == 0) {
	    ctype = "image/x-gfits";
	    t = ctype+6;
	}
 	else if (strcmp(ce, "x-compress") == 0) {
	    ctype = "image/x-cfits";
	    t = ctype+6;
	}
    }

    // pure FITS or starbase table ?
    if (strcmp(t, "x-fits") == 0 
	|| strcmp(t, "x-starbase") == 0 
	|| strcmp(t, "plain") == 0 
	|| strcmp(t, "tab-separated-values") == 0) { 
	return 0;	// not compressed, just return filename
    }

    Compress::CompressType type = Compress::NO_COMPRESS;
    if (strcmp(t, "x-hfits") == 0) {
	type = Compress::H_COMPRESS; // Hcompressed FITS file
    }
    else if (strcmp(t, "x-gfits") == 0 || strcmp(t, "x-gstarbase") == 0) {
	type = Compress::GZIP_COMPRESS; // GZIPed FITS or tab table 
    }
    else if (strcmp(t, "x-cfits") == 0 || strcmp(t, "x-cstarbase") == 0) {
	type = Compress::UNIX_COMPRESS; // UNIX Compressed FITS ir tab table
    }
    else if (strcmp(t, "x-sfits") == 0) { // Compressed FITS file (Stark)
	// type = Compress::S_COMPRESS;
	unlink(tmpfile_);
	return error("x-sfits compression (Stark) not supported");
    }
    else {
	unlink(tmpfile_);
	return error("unknown preview data Content-type: ", ctype);
    }

    // do the decompression
    FILE* feedback = http_.feedback();
    if (feedback) {
	fprintf(feedback, "decompressing data...\n");
	fflush(feedback);
    }
    
    Compress c;
    if (c.decompress(tmpfile_, type) != 0) {
	unlink(tmpfile_);
	return ERROR;
    }

    // correct Content-type after decompression
    ctype = (char*)(is_image ? "image/x-fits" : "text/x-starbase");

    // if we got here, then we have the FITS file, so return the file name
    return 0;
}


/*
 * return the text of the previous error message
 */
const char* AstroCatalog::getError() 
{
    return last_error();	// from error.h
}


/*
 * reset the error message buffer to empty
 */
void AstroCatalog::clearError() 
{
    clear_error();		// from error.h
}
