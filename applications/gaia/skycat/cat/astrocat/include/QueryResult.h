// -*-c++-*-
#ifndef _QueryResult_h_
#define _QueryResult_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: QueryResult.h,v 1.15 1998/05/12 21:55:15 abrighto Exp $
 *
 * QueryResult.h - class definitions for accessing results of a catalog
 * 		   query
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  7 Nov 95  Created
 */


#include "WorldOrImageCoords.h"
#include "CatalogInfo.h"
#include "AstroQuery.h"
#include "TabTable.h"


/*
 * Class QueryResult
 *
 * This class manages the result of an AstroCatalog::query. The basic
 * result is a char buffer that contains one row per line where each
 * column is separated by a tab character.
 *
 * This class provides transparent access to the result based on a
 * row,column index and allows for type conversion from string to the
 * desired type.
 */
class QueryResult : public TabTable {
protected:
    WorldOrImageCoords centerPos_; // saved ra,dec or x,y pos from previous query

    CatalogInfoEntry* entry_;      // catalog config file entry pointer,
				   // used if not null when saving to a file 

    // given a tab table (with columns ra and dec)and a row, return 0 if ra and dec 
    // are within the given radius (in arcmin) and mag is in the given magnitude range.
    virtual int circularCompareRow(const TabTable& table, int row, const AstroQuery& q, 
			   int mag_col, int* search_cols); 

    // Search the given tab table for all objects in the specified world
    // coordinate circle/ring and fill "*this" table with the results. 
    virtual int circularSearch(const TabTable& table, const AstroQuery& q, int maxRows);

    // print table title and othe info...
    virtual void printTableTop(ostream& os, const char* title = NULL);

public:
    // constructor: initialize empty table
    QueryResult();

    // constructor: init from query result buffer
    QueryResult(const char* result);

    // constructor: initialize from data buffer without headings
    QueryResult(int numCols, char** colNames, const char* result);

    // destructor: free any allocated memory
    virtual ~QueryResult() {}

    // get the position from the given row as world or image coords
    virtual int getPos(int row, WorldOrImageCoords& pos) const;

    // get the position from the given row as world coords or report
    // an error if the catalog is not using world coords
    virtual int getPos(int row, WorldCoords& pos) const;

    // Query the given tab table using the condition described by the
    // given AstroQuery object.
    virtual int query(const AstroQuery& q, const TabTable& table, const char* outfile, 
	      int& more);

    // member access
    
    // Set the catalog config entry for this object. This is included in the
    // file when this object is saved as a local catalog. The optional result
    // arg may be a pointer to the result of a catalog query, which may
    // contain config configuration information. If specified, it is scanned
    // to update the entry with the new information.
    virtual void entry(CatalogInfoEntry* e, const char* result = NULL);

    // access config info that may have been included in the header
    // of the query result
    virtual const char* symbol() const    {return entry_->symbol();}
    virtual const char* copyright() const {return entry_->copyright();}
    virtual const char* help() const      {return entry_->help();}
    virtual const char* shortName() const {return entry_->shortName();}
    virtual const char* longName() const  {return entry_->longName();}
    virtual int id_col() const            {return entry_->id_col();}
    virtual int ra_col() const            {return entry_->ra_col();}
    virtual int dec_col() const           {return entry_->dec_col();}
    virtual int x_col() const             {return entry_->x_col();}
    virtual int y_col() const             {return entry_->y_col();}
    virtual double equinox() const        {return entry_->equinox();}
    
    // return true if the catalog uses world coords
    virtual int isWcs() const             {return entry_->isWcs();}

    // return true if the catalog uses image pixel coords
    virtual int isPix() const             {return entry_->isPix();}
};


#endif /* _QueryResult_h_ */
