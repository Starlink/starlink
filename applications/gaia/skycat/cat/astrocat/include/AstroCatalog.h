// -*-c++-*-
#ifndef _AstroCatalog_h_
#define _AstroCatalog_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: AstroCatalog.h,v 1.15 1998/09/23 19:12:40 abrighto Exp $
 *
 * AstroCatalog.h - class definitions for accessing astronomical
 *                  catalogs
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */


#include <iostream.h>
#include "CatalogInfo.h"
#include "AstroQuery.h"
#include "QueryResult.h"
#include "HTTP.h"

class WorldCoords;
class WorldOrImageCoords;


/*
 * Class AstroCatalog
 *
 * This class manages access to astronomical catalogs via a collection
 * HTTP catalog servers.  The main entry point is the "open" method,
 * which returns a pointer to a class object representing the catalog.
 */
class AstroCatalog {
protected:
    HTTP http_;			// http server handle
    char* tmpfile_;		// temp file to hold fits image
    int status_;		// status after constructor
    int more_;			// true if more rows would have been available
				// but were not fetched (due to query limit)
    QueryResult info_;		// column name info for this catalog

    CatalogInfoEntry* entry_; // ptr to the config entry for this catalog

    // query server for catalog column names and put result in info_
    virtual int getInfo();
    virtual int checkInfo();

    // generate the query string from the given url and query info
    virtual int genHttpQuery (char* buf, int bufsz, const AstroQuery& q, const char* url);

    // generate a new temp filename to hold an image or preview data
    void newTempFile();

    // constructor - create catalog class instance
    // note: public interface uses AstroCatalog::open().
    // The argument represents the entry in the catalog config file for this catalog
    AstroCatalog(CatalogInfoEntry*);

public:
    // destructor - close catalog and free any resources
    virtual ~AstroCatalog();

    // copy constructor
    AstroCatalog(const AstroCatalog&);

    // open the named catalog and return a pointer to a new 
    // AstroCatalog object created for it or NULL if errors occur
    static AstroCatalog* open(const char* name);

    // use a name server catalog (like simbad_ns@eso or ned_ns@eso) 
    // to get the coordinates from the object name. If feedback is not NULL, 
    // status info is written to the given open file.
    static int nameToWorldCoords(
	const char* objName, 
	WorldOrImageCoords& pos,
	const char* nameServer = "simbad_ns@eso", 
	FILE* feedback = NULL);

    // Return true if the given entry is for a catalog, archive or name server
    static int isCatalog(CatalogInfoEntry*);

    // Return true if the given entry is for an image server
    static int isImageServer(CatalogInfoEntry*);

    // Return true if the given entry is for a local catalog
    static int isLocalCatalog(CatalogInfoEntry*);

    // Generate an error message indicating that the wrong type of catalog was used.
    static int wrongServType(CatalogInfoEntry*);

    // Pass a query to the catalog and return the number of objects found.
    virtual int query(const AstroQuery& q, const char* filename, QueryResult& result);

    // return the symbol entry for this catalog (symbol info for plotting)
    const char* symbol() {return entry_->symbol();}

    // return the search_cols entry for this catalog
    const char* searchCols() {return entry_->searchCols();}

    // return the sort_cols entry for this catalog
    const char* sortCols() {return entry_->sortCols();}

    // return the sort_order entry for this catalog
    const char* sortOrder() {return entry_->sortOrder();}

    // return the show_cols entry for this catalog
    const char* showCols() {return entry_->showCols();}

    // return the copyright field
    const char* copyright() {return entry_->copyright();}

    // return the help field
    const char* help() {return entry_->help();}

    // return the column index for standard fields, may be set in config file
    // or in header or result
    int id_col() {return entry_->id_col();}
    int ra_col() {return entry_->ra_col();}
    int dec_col() {return entry_->dec_col();}
    int x_col() {return entry_->x_col();}
    int y_col() {return entry_->y_col();}
    int is_tcs() {return entry_->is_tcs();}

    // return true if the catalog uses world coordinates
    int isWcs() {return entry_->isWcs();}
    
    // return true if the catalog uses image pixel coords
    int isPix() {return entry_->isPix();}
    
    // return the equinox of the catalog (default: 2000.)
    double equinox() {return entry_->equinox();}

    // set the file ptr to use for http feedback during transfers
    void feedback(FILE* f) {http_.feedback(f);}

    // member access:

    // return status (after constructor) for error checking
    int status() {return status_;}

    // return the name of this catalog
    const char* name() {return entry_->longName();}
    const char* longName() {return entry_->longName();}
    const char* shortName() {return entry_->shortName();}

    // return the serv_type entry from the config file ("catalog", "local", ...)
    const char* servType() {return entry_->servType();}

    // return the url field from the config file
    const char* url() {return entry_->url();}

    // return the number of columns in the catalog
    virtual int numCols() ;

    // return the column names
    virtual char** colNames() ;
    virtual const char* colName(int col);

    // return the column index for the given column name
    virtual int colIndex(const char* colName);

    // return true if the catalog contains the given column 
    int hasCol(const char* name) {return (colIndex(name) >= 0);}

    // return true if more than "maxRows" rows would have been available 
    // to the last call to query()
    int more() {return more_;}

    // set/get the temp file to use for getting preview data via http
    void tmpfile(const char* name);
    const char* tmpfile() {return tmpfile_;}

    // Request an image from the image server based on the given query description
    int getImage(const AstroQuery& q);

    // Given a URL for an image, request the image from the image server
    int getImage(const char* url);

    // fetch a preview image or plot data using the given url
    int getPreview(const char* url, char*& content_type);

    // return the handle for the HTTP object used to do the GET
    // (can be used to determine header values, or check if a 
    //  username and password are needed)
    HTTP& http() {return http_;}

    // get the catalog config entry for this catalog
    CatalogInfoEntry* entry() {return entry_;};

    // For the VLT Catalog interface 


    // Get the number of columns and the column names
    // for the this catalog and return 0 if all is OK
    virtual int getDescription(
	int& numCols,		       // out - number of result columns 
	char**& colNames);             // out - reference to array of column names 
   
    // Get the values for the specified columns for the object given by "id"
    // in the catalog and return 0 if all is OK
    virtual int getObject(
	const char* id,		      // in  - object id in catalog 
	int numCols,		      // in  - number of columns to get
	char** colNames,              // in  - array of column names to read 
	QueryResult& result);	      // out - ref to object managing result 
    
    // Get the values for all objects in the specified world coordinates area
    // Note: This routines returns the number of rows found (in numFound). 
    virtual int getArea(
	int numCols,		     // in  - number of columns to get
	char** colNames,             // in  - array of column names to read 
	const WorldOrImageCoords& pos0,     // in  - coordinates of area - first point
	const WorldOrImageCoords& pos1,     // in  - second point of area
	double mag0,		     // in  - min magnitude 
	double mag1,		     // in  - max magnitude 
	int maxRows,		     // in  - max number of rows to return 
	const char* filename,	     // in  - if not null, write results to this file 
	int& numFound,		     // out - number of objects found 
	QueryResult& result);	     // out - ref to object managing result 

    // Get the values for all objects in the specified circle/ring.
    // Note: This routines returns the number of rows found (in numFound). 
    virtual int circularSearch(
	int numCols,		     // in  - number of columns to get
	char** colNames,             // in  - array of column names to read 
	const WorldOrImageCoords& pos,	     // in  - center position in world coordinates
	double radius0,		     // in  - min radius 
	double radius1,		     // in  - max radius 
	double mag0,		     // in  - min magnitude 
	double mag1,		     // in  - max magnitude 
	int maxRows,		     // in  - max number of rows to return 
	const char* filename,	     // in  - if not null, write results to this file 
	int& numFound,		     // out - number of objects found
	QueryResult& result);	     // out - ref to object managing result 


    // search for the star closest to the given position, with the magnitude in 
    // the given range and return (via the last 2 args) the columns requested
    // by "colNames"
    virtual int searchClosestStar(
	int numCols,		     // in  - number of columns to get
	char** colNames,             // in  - array of column names to read 
	const WorldOrImageCoords& pos,	     // in  - center position in world coordinates
	double mag0,		     // in  - min magnitude 
	double mag1,		     // in  - max magnitude 
	QueryResult& result);	     // out - ref to object managing result 


    // search for the stars fulfilling the specified criteria
    virtual int CatalogSearch(
	int numCols,		       // in  - number of columns to get
	char** colNames,	       // in  - array of column names to read 
	int numSearchCols,	       // in  - number of search cond. columns
	char** searchCols,	       // in  - array of search cond.column names
	char** minVals,		       // in  - optional array of min search values 
	char** maxVals,		       // in  - optional array of max search values 
	int maxRows,		       // in  - max number of rows to return 
	const char* filename,	       // in  - if not null, write results to this file 
	int& numFound,		       // out - number of objects found
	QueryResult& result);	       // out - ref to object managing result 

    // return the text of the previous error message
    static const char* getError();

    // reset the error message buffer to empty
    static void clearError();
};



#endif /* _AstroCatalog_h_ */
