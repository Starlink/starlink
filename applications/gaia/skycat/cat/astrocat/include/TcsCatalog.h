// -*-c++-*-
#ifndef _TcsCatalog_h_
#define _TcsCatalog_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsCatalog.h,v 1.3 1997/09/30 15:54:43 abrighto Exp $
 *
 * TcsCatalog.h - class specialized for accessing GSC, PPM or similar catalogs 
 *                for use by the TCS (Telescope Control Software).
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */

#include "AstroCatalog.h"
#include "TcsQueryResult.h"

/*
 * Class TcsCatalog
 *
 * This class is like class AstroCatalog, except that it assumes a
 * catalog has fixed columns, such as those that are found in the GSC or
 * PPM catalogs.  This class restricts itself to these fixed columns and
 * ignores the rest.  Each row of a TcsCatalog can be represented by a
 * TcsCatalogObject.  Any missing column values are set to the appropriate 
 * null value.
 */
class TcsCatalog : public AstroCatalog {
private:

protected:

public:
    // constructor - create catalog class instance
    // note: public interface uses TcsCatalog::open().
    // The argument represents the entry in the catalog config file for this catalog
    TcsCatalog(CatalogInfoEntry* e)
	: AstroCatalog(e) {}

    // destructor - close catalog and free any resources
    virtual ~TcsCatalog() {}

    // open the named catalog and return a pointer to a new 
    // TcsCatalog object created for it or NULL if errors occur
    static TcsCatalog* open(const char* name);

    // return the number of columns in the catalog
    int numCols() {return TcsCatalogObject::numCols();}

    // return the column names
    char** colNames() {return TcsCatalogObject::colNames();}
    const char* colName(int col) {return TcsCatalogObject::colName(col);}

    // return the column index for the given column name
    int colIndex(const char* colName) {return TcsCatalogObject::colIndex(colName);}

    // return true if the catalog contains the given column 
    int hasCol(const char* name) {return (colIndex(name) >= 0);}

    // -- the interface for the next 2 methods is different for TCS --

    // Get the object given by "id" in the catalog and return 0 if all is OK
    int getObject(
	const char* id,		      // in  - object id in catalog 
	TcsCatalogObject& obj);	      // out - object for row, if found
    
    // search for the star closest to the given position, with the magnitude in 
    // the given range 
    int searchClosestStar(
	const WorldCoords& pos,	     // in  - center position in world coordinates
	double mag0,		     // in  - min magnitude 
	double mag1,		     // in  - max magnitude 
	TcsCatalogObject& obj);	     // out - object for row, if found

};

#endif /* _TcsCatalog_h_ */
