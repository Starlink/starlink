/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: QueryResult.C,v 1.4 2003/01/20 15:52:21 brighton Exp $
 *
 * QueryResult.C - method definitions for class QueryResult
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  07 Nov 95  Created
 */
static const char* const rcsId="@(#) $Id: QueryResult.C,v 1.4 2003/01/20 15:52:21 brighton Exp $";


#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <cstring>
#include "error.h"
#include "QueryResult.h"
#include "WorldOrImageCoords.h"
#include "AstroQuery.h"


// This is a dummy null catalog entry, used as a default
static CatalogInfoEntry* defaultEntry_ = new CatalogInfoEntry;


/*
 * constructor: initialize empty table
 */
QueryResult::QueryResult()
    : TabTable(), entry_(defaultEntry_) {}


/*
 * constructor: init from query result buffer
 */
QueryResult::QueryResult(const char* result) 
	: TabTable(result), entry_(defaultEntry_) {}


/*
 * constructor: initialize from data buffer without headings
 */
QueryResult::QueryResult(int numCols, char** colNames, const char* result) 
    : TabTable(numCols, colNames, result), entry_(defaultEntry_) {}


/*
 * if the result row contains a position (ra, dec) or (x, y),
 * get it and return success (0).
 *
 * Note that by convention the first 3 fields are id, ra and dec, in J2000,
 * unless otherwise defined in the catalog config entry. For example, if the
 * config entry specifies: "x_col: 1" and "y_col: 2", then we look for image
 * coordinates x and y in columns 1 and 2.
 */
int QueryResult::getPos(int row, WorldOrImageCoords& pos) const
{
    if (entry_->isWcs()) {
	// use world coords
	char* ra;		// get ra and dec as strings 
	char* dec;		// so we can accept H:M:S or d.ddd
	if (get(row, entry_->ra_col(), ra) || get(row, entry_->dec_col(), dec))
	    return 1;		// error
    
	pos = WorldCoords(ra, dec, entry_->equinox(), 1);
	if (pos.status() == 0) 
	    return 0;		// success
	return 1;		// error
    }
    else if (entry_->isPix()) {
    
	// use image coords
	double x, y;
	if (get(row, entry_->x_col(), x) || get(row, entry_->y_col(), y))
	    return 1;		// error
    
	pos = ImageCoords(x, y);
	if (pos.status() == 0) 
	    return 0;	// success
	return 1;	// error
    }
    return error("This catalog does not have coordinates");
}

/*
 * get the position from the given row as world coords or report
 * an error if the catalog is not using world coords
 */
int QueryResult::getPos(int row, WorldCoords& pos) const
{
    if (! entry_->isWcs()) 
	return error("catalog does not support world coordinates");
    WorldOrImageCoords p;
    if (getPos(row, p) != 0)
	return 1;
    pos = p.wc();
    return 0;
}


/*
 * Search the given tab table for all objects in the specified world or
 * image coordinate circle/ring and fill "*this" table with the results.
 * The return value is 0 if all is OK.
 *
 * Args:
 *
 *  table	 in  - tab table to search
 *  q  i  	 in  - object holding query conditions
 *  maxRows	 in  - max number of rows to find
 */
int QueryResult::circularSearch(
    const TabTable& table, 
    const AstroQuery& q, 
    int maxRows)	
{
    int tcols = table.numCols(), trows = table.numRows();
    
    // copy table header for the result
    if (init(tcols, table.colNames(), "", 0) != 0)
	return ERROR;
    
    if (maxRows <= 0)
	return 0;

    // search rows and put matching rows in "os"
    std::ostringstream os;
    int n = 0;
    int i = 0;
    
    // get col index for mag
    int mag_col = inputColIndex("mag");

    // get array of col indexed for search columns
    const int maxcols = 255;
    if ((n = q.numSearchCols()) > maxcols)
	return error("too many search columns");
    int search_cols[maxcols];
    for(i = 0; i < n; i++) 
	search_cols[i] = inputColIndex(q.searchCols()[i]);
    
    n = 0;
    for(i = 0; i < trows; i++) {
	if (circularCompareRow(table, i, q, mag_col, search_cols) == 0) {
	    table.printRow(os, i);
	    if (++n >= maxRows)
		break;
	}
    }

    int status = init(numCols_, colNames_, os.str().c_str(), maxRows);
    return status;
    
    return 0;
}


/*
 * Given a tab table and a row number, return 0 if the query position
 * (q.pos()) is within the given radius range (q.radius1(), q.radius2())
 * and mag (if applicable) is in the given magnitude range (q.mag1(),
 * q.mag2()) and all of the other conditions given by q are met.
 *
 * mag_col is the column for "mag" or -1 if there is no mag column in the
 * table, in which case the mag range is ignored.
 *
 * Another way of specifying a mag range or a range for any other column
 * is to use the general purpose condition fields in the AstroQuery object:
 * q.searchCols(), q.minValues(), q.maxValues(). These are used if set.
 * (Note that for backward compat., the hard coded values for mag1,mag2 are
 * still supported, although they could be handled more generally using
 * q.searchCols(), ...)
 *
 * The search_cols parameter is used to save time by passing an array of
 * column indexes for the search columns (if any).
 *
 * The positions are taken either from the columns for ra and dec, if we
 * are using world coords, or the columns for x and y if we are using
 * image coords)
 *
 * The radius is assumed to be in arcmin for world coords, or pixel for
 * image coords.
 */
int QueryResult::circularCompareRow(const TabTable& table, int row, 
				    const AstroQuery& q, int mag_col, 
				    int* search_cols) 
{
    // get value for mag, if there is one
    if (mag_col != -1 && (q.mag1() != 0.0 || q.mag2() != 0.0)) {
	double mag;
	if (table.get(row, mag_col, mag) != 0 || mag < q.mag1() || mag > q.mag2())
	    return 1;
    }

    if (entry_->isWcs() || entry_->isPix()) {
	if (q.radius1() || q.radius2()) {
	    // get ra,dec point
	    WorldOrImageCoords p;
	    if (entry_->isWcs()) {
		char* ra;
		char* dec;
		if (table.get(row, entry_->ra_col(), ra) != 0 
		    || table.get(row, entry_->dec_col(), dec) != 0)
		    return 1;
		p = WorldCoords(ra, dec, entry_->equinox(), 1);
	    }
	    else if (entry_->isPix()) {
		// get x,y
		double x, y;
		if (table.get(row, entry_->x_col(), x) != 0 
		    || table.get(row, entry_->y_col(), y) != 0)
		    return 1;
		p = ImageCoords(x, y);
	    }
	    if (p.status() != 0)
		return ERROR;

	    // see if point is in radius
	    double dist = q.pos().dist(p);
	    if (dist < q.radius1() || dist > q.radius2())
		return 1;		// position for row not in range
	}
    }

    // check any other conditions for column values
    int n = q.numSearchCols();
    if (n > 0) {
	char** minValues = q.minValues();
	char** maxValues = q.maxValues();
	char* tableValue;
	for(int i = 0; i < n; i++) {
	    if (table.get(row, search_cols[i], tableValue) != 0)
		return 1;
	    // since we don't know the type of the column, try double, then int, then string
	    double d, d1, d2;
	    int j, j1, j2;
	    if (sscanf(tableValue, "%lf", &d) == 1
		&& sscanf(minValues[i], "%lf", &d1) == 1 
		&& sscanf(maxValues[i], "%lf", &d2) == 1) {
		// compare as double
		if (d < d1 || d > d2)
		    return 1;  // no match
	    }
	    else if (sscanf(tableValue, "%d", &j) == 1
		&& sscanf(minValues[i], "%d", &j1) == 1 
		&& sscanf(maxValues[i], "%d", &j2) == 1) {
		// compare as int
		if (j < j1 || j > j2)
		    return 1;  // no match
	    }
	    else {
		// compare as string
		if (strcmp(tableValue, minValues[i]) < 0
		    || strcmp(tableValue, maxValues[i]) > 0)
		    return 1;  // no match
	    }
	}
    }
    
    return 0;			// a match
}

/*
 * Query the given tab table using the condition described by the
 * given AstroQuery object.
 *
 * Args:
 *     q -        (in)   object describing the query
 *
 *     table   -  (in)   table to search
 *     outfile  - (in)   optional filename to hold results, or null
 *
 *     more   -   (out)  set to 1 if more objects would be available but were
 *                       not returned because q.maxRows was set lower
 *
 * The return value 0 if all is OK. The number found is available as this->numRows().
 */
int QueryResult::query(const AstroQuery& q, const TabTable& table, const char* outfile, int& more)
{
    // Note: if we have to sort, we don't want to loose data by discarding rows before sorting.
    // Otherwise, if not sorting, we can save space and time and discard unwanted rows earlier.
    // In any case, add at least 1 to maxRows, so we can see if there would be more rows available...
    int maxRows = q.maxRows()+1;
    if (q.numSortCols() > 0 || q.maxRows() == 0) 
	maxRows = table.numRows();
    
    if (strlen(q.id()) != 0) {	// search for id only
	centerPos_.setNull();
	if (search(table, entry_->id_col(), q.id(), maxRows) != 0) 
	    return ERROR;
    }
    else {
	centerPos_ = q.pos();
	if (circularSearch(table, q, maxRows) != 0)
	    return ERROR;
    } 

    // sort result ?
    // note: do this before truncating to maxRows to get correct results
    if (q.numSortCols()) 
	sort(q.numSortCols(), q.sortCols(), q.sortOrder()); 

    if (q.maxRows() && numRows_ > q.maxRows()) {
	more = 1;
	numRows(q.maxRows());
    } else {
	more = 0;
    }

    if (outfile && save(outfile) != 0)
	return ERROR;

    return 0;
}


/*
 * print the table title (and any other info preceding the column headings)
 * (may be redefined in a derived class to add more info)
 */
void QueryResult::printTableTop(std::ostream& os, const char* title) 
{
    if (! title)
	title = "QueryResult";
    TabTable::printTableTop(os, title);
    
    // add some info from the config file entry, if known, so that we know later
    // what plot symbol to use when loading the file as a local catalog
    if (entry_ && entry_->servType()) {
	os << "\n# Config entry for original catalog server:\n" 
	   << *entry_;
	os << "# End config entry\n\n";
    }
}


/*
 * Set the catalog config entry for this object. This is included in the
 * file when this object is saved as a local catalog. The optional result
 * arg may be a pointer to the result of a catalog query, which may
 * contain config configuration information. If specified, it is scanned
 * to update the entry with the new information.
 */
void QueryResult::entry(CatalogInfoEntry* e, const char* result)
{
    entry_ = e;

    if (result) {
	std::istringstream is(result);
	CatalogInfo::updateConfigEntry(is, e);
    }
}

