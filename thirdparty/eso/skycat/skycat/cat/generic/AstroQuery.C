/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: AstroQuery.C,v 1.1.1.1 2006/01/12 16:36:28 abrighto Exp $
 *
 * AstroQuery.C - method definitions for class AstroQuery
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created 
 */
static const char* const rcsId="@(#) $Id: AstroQuery.C,v 1.1.1.1 2006/01/12 16:36:28 abrighto Exp $";


#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <cstring>
#include <sys/types.h>
#include "error.h"
#include "util.h"
#include "AstroQuery.h"


/*
 * constructor - initialize fields to null values.
 */
AstroQuery::AstroQuery() 
    : id_(NULL),
      pos_(),
      mag1_(0.0),
      mag2_(0.0),
      radius1_(0.0), 
      radius2_(0.0), 
      width_(0.0),
      height_(0.0),
      numCols_(0),
      colNames_(NULL),
      maxRows_(0),
      numSortCols_(0),
      sortCols_(NULL),
      numSearchCols_(0),
      searchCols_(NULL),
      minValues_(NULL),
      maxValues_(NULL)
{
}


/*
 * copy constructor
 */
AstroQuery::AstroQuery(const AstroQuery& a) 
    : id_(a.id_ ? strdup(a.id_) : (char*)NULL),
      pos_(a.pos_),
      mag1_(a.mag1_),
      mag2_(a.mag2_),
      radius1_(a.radius1_), 
      radius2_(a.radius2_), 
      width_(a.width_),
      height_(a.height_),
      numCols_(a.numCols_),
      colNames_(copyArray(a.numCols_, a.colNames_)),
      maxRows_(a.maxRows_),
      numSortCols_(a.numSortCols_),
      sortCols_(copyArray(a.numSortCols_, a.sortCols_)),
      numSearchCols_(a.numSearchCols_),
      searchCols_(copyArray(a.numSearchCols_, a.searchCols_)),
      minValues_(copyArray(a.numSearchCols_, a.minValues_)),
      maxValues_(copyArray(a.numSearchCols_, a.maxValues_))
{
}

/*
 * destructor
 */
AstroQuery::~AstroQuery() 
{
    if (id_)
	free(id_);
// PWD: let these leak. In fact these can be new or malloc memory
//      depending on various freeflag values... Need to track which.
//     if (colNames_)
// 	delete[] colNames_;
//     if (sortCols_)
// 	delete[] sortCols_;
//     if (searchCols_)
// 	delete[] searchCols_ ;
//     if (minValues_)
// 	delete[] minValues_;
//     if (maxValues_)
// 	delete[] maxValues_;

}


/*
 * check that the given column name array is valid
 * returns 0 if the args were OK. 
 */ 
static int check(int n, char** ar) 
{
    if ((n && !ar) || (ar && !n))
	return error("invalid column name arguments", "", EINVAL);
    if (n)
	for (int i = 0; i < n; i++)
	    if (!ar[i])
		return error("incomplete column name array", "", EINVAL);
    return 0;
}


/*
 * set center pos, radius, width and height by setting 2 positions
 * The positions may be in World or Image coordinates (the units of
 * the radius, width and height are based on the type of coords - 
 * arcmin or pixel).
 */
int AstroQuery::pos(const WorldOrImageCoords& p1, const WorldOrImageCoords& p2)
{
    if (p1.status() || p2.status())
	return error("invalid position argument", last_error(), EINVAL);
    
    radius1_ = 0.;
    pos_ = WorldOrImageCoords::center(p1, p2, radius2_, width_, height_);
    return pos_.status();
}


/*
 * set the values of the min and max magnitude
 */
int AstroQuery::mag(double m1, double m2) {
    if (m1 < m2) {
	mag1_ = m1;
	mag2_ = m2;
    }
    else {
	mag1_ = m2;
	mag2_ = m1;
    }

    return 0;
}


/*
 * set the values of the min and max radius
 */
int AstroQuery::radius(double r1, double r2) {
    if (r1 < 0.0 || r2 < 0.0)
	return error("negative radius argument", "", EINVAL);

    if (r1 < r2) {
	radius1_ = r1;
	radius2_ = r2;
    }
    else {
	radius1_ = r2;
	radius2_ = r1;
    }

    // if (pos_.isWcs() && (r1 > 300.0 || r2 > 300.0))
    //	return error("radius too large (max 300 arcmin)", "", EINVAL);
    return 0;
}


/* 
 * set the max radius (with 0 min radius)
 */
int AstroQuery::radius(double r) 
{
    if (r < 0.0)
	return error("negative radius", "", EINVAL);
    if (pos_.isNull())
	return error("radius for catalog query set with no center position");
    //if (pos_.isWcs() && r > 300.)
    //	return error("radius too large (max 300 arcmin)", "", EINVAL);
    radius1_ = 0.0; 
    radius2_ = r; 
    return 0;
}


/*
 * set the number of columns and column names for the query to get
 * and check that the array is valid (0 and NULL is ok, use default
 * which is all columns).
 * If freeFlag is true, the memory for the array is used and freed when
 * no longer needed, otherwise a copy is made.
 *
 * returns 0 if the args were OK. 
 */ 
int AstroQuery::colNames(int numCols, char** colNames, int freeFlag) 
{
    if (check(numCols, colNames) != 0)
	return ERROR;
    
    if (! freeFlag)
	colNames = copyArray(numCols, colNames);

    numCols_ = numCols; 
    colNames_ = colNames;
    return 0;
}


/*
 * set the number and names of the columns to sort by.
 * returns 0 if the args were OK. 
 * If freeFlag is true, the memory for the array is used and freed when
 * no longer needed, otherwise a copy is made.
 */ 
int AstroQuery::sort(int numSortCols, char** sortCols, int freeFlag)
{
    if (numSortCols && check(numSortCols, sortCols) != 0)
	return ERROR;

    if (! freeFlag)
	sortCols = copyArray(numSortCols, sortCols);

    numSortCols_ = numSortCols; 
    sortCols_ = sortCols;
    return 0;
}


/*
 * Set the condition for the query. This is defined as an array of column
 * names to compare (and the number of columns), an array of min values
 * and an array of max values (all char* values).
 * If freeFlag is true, the memory for the arrays is used and freed when
 * no longer needed, otherwise a copy is made.
 *
 * For backward compatibility, if the column name is "mag", the mag1 and
 * mag2 fields are set.
 *
 * returns 0 if the args were OK. 
 */ 
int AstroQuery::condition(int n, char** cols, char** low, char** high, int freeFlag) 
{
    if ((n && (!cols || (!low && !high))) || ((cols || low || high) && !n))
	return error("invalid search condition arguments", "", EINVAL);
    if (n) {
	for (int i = 0; i < n; i++) {
	    if (!cols[i] || (high && !high[i]) || (low && !low[i]))
		return error("incomplete search condition arguments", "", EINVAL);
	    // for backward compat, set mag1, mag2 if col name is mag
	    if (strcasecmp(cols[i], "mag") == 0) {
		double m1, m2;
		if (sscanf(low[i], "%lf", &m1) == 1 
		    && sscanf(high[i], "%lf", &m2) == 1) {
		    mag(m1, m2);
		}
	    }
	}
    }

    if (! freeFlag) {
	cols = copyArray(n, searchCols_);
	low = copyArray(n, minValues_);
	high = copyArray(n, maxValues_);
    }

    numSearchCols_ = n; 
    searchCols_ = cols;
    minValues_ = low;
    maxValues_ = high;
    return 0;
}


/*
 * set the dimensions for an area query
 */
int AstroQuery::dim(double w, double h) 
{
    if (w < 0.0 || h < 0.0)
	return error("negative width or height for query", "",  EINVAL);

    width_ = w; 
    height_ = h; 
    return 0;
}


/* 
 * set the max magnitude (with 0 min mag)
 */
int AstroQuery::mag(double m) 
{
    // if (m < 0.0)
    //	return error("negative magnitude");
    mag1_ = 0.0; 
    mag2_ = m; 
    return 0;
}


/*
 * set the max number of rows to return
 */
int AstroQuery::maxRows(int n) 
{
    if (n < 0)
	return error("negative value set for max number of rows");
    maxRows_ = n; 
    return 0;
}


/* 
 * return the name of the given column or NULL if out of range
 */
const char* AstroQuery::colName(int col) const 
{
    if (col >= 0 && col < numCols_)
	return colNames_[col];
    return NULL;
}
