/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsCatalog.C,v 1.1.1.1 2006/01/12 16:36:30 abrighto Exp $
 *
 * TcsCatalog.C - method definitions for class TcsCatalog
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */
static const char* const rcsId="@(#) $Id: TcsCatalog.C,v 1.1.1.1 2006/01/12 16:36:30 abrighto Exp $";


#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
#include "error.h"
#include "TcsLocalCatalog.h"
#include "TcsCatalog.h"


/*
 * open the named catalog and return a pointer to a TcsCatalog
 * object for that catalog or NULL if errors occur
 */
TcsCatalog* TcsCatalog::open(const char* name) 
{
    // get the entry for this catalog type
    CatalogInfoEntry* e = CatalogInfo::lookup(name);
    if (!e) 
	return NULL;		// error - no config entry

    TcsCatalog* result = NULL;
    if (strcmp(e->servType(), "local") == 0) 
	result = new TcsLocalCatalog(e); // derived class for local catalogs
    else
	result = new TcsCatalog(e); // class for (remote) catalogs

    if (result->status() != 0) {
	delete result;
	return NULL;		// error making catalog 
    }
    return result;		// normal return
}

    
/*
 * Get the values for the specified columns for the object given by "id"
 * in the catalog and return 0 if all is OK.
 *
 * Args:
 *
 *   id		in  - object id in catalog 
 *   obj	out - catalog info for row, if found 
 */
int TcsCatalog::getObject(
    const char* id, 
    TcsCatalogObject& obj)	
{
    AstroQuery q;
    q.id(id);
    q.maxRows(1);
    
    TcsQueryResult result;
    int nrows = query(q, NULL, result);
    if (nrows < 0) 
	return 1;		// error return
    if (nrows == 0) {
	return error("object not found: ", id);		// error, not found
    }
    
    int status = result.getObj(0, obj);
    return status;
}
    

/*
 * search for the star closest to the given position, with the magnitude in 
 * the given range 
 *
 * Args:
 *
 *  numCols	 in  - number of columns to get
 *  colNames	 in  - array of column names to read 
 *  pos		 in  - center position in world coordinates
 *  mag0	 in  - min magnitude 
 *  mag1	 in  - max magnitude 
 *  obj 	 out - object containing data for row, if found 
 */
int TcsCatalog::searchClosestStar(
    const WorldCoords& pos,
    double mag0, 
    double mag1, 
    TcsCatalogObject& obj)	
{
    AstroQuery q;
    q.pos(pos);
    q.mag(mag0, mag1);
    q.maxRows(1);

    TcsQueryResult result;
    int nrows = query(q, NULL, result);

    if (nrows < 0) 
	return 1;		// error return
    if (nrows == 0) {
	return error("no objects found"); 
    }
    
    int status = result.getObj(0, obj);
    return 0;
}

