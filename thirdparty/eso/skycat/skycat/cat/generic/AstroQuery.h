// -*-c++-*-
#ifndef _AstroQuery_h_
#define _AstroQuery_h_

/*
 * E.S.O. - VLT project 
 * $Id: AstroQuery.h,v 1.8 1998/09/08 19:42:35 abrighto Exp $
 *
 * AstroQuery.h - class describing a query to search an astronomical catalog.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  27 Sep 95  Created
 */


#include <string.h>
#include "WorldOrImageCoords.h"


/*
 * Class AstroQuery
 *
 * This class is used in star catalog queries to specify which object(s)
 * to search for.
 *
 * The class attributes specify the conditions for the search, such as
 * id, name, position, radius, etc.  All of the fields are
 * optional. Fields are set to the appropriate null value, if they are
 * not being used.
 *
 */
class AstroQuery {
protected:
    char* id_;			// object Id
    WorldOrImageCoords pos_;	// center position as right ascension, declination
    double mag1_, mag2_;	// min, max magnitude of object (most, least bright)
    double radius1_, radius2_;	// min, max radius in arcmin from center 
    double width_, height_;	// width, height in arcmin from center

    int numCols_;		// number of columns corresp. to colNames_ below
    char** colNames_;           // ptr to array of column names to get (default all)
    int maxRows_;		// max number of rows to get

    int numSortCols_;		// number of columns corresp. to sortCols_ below
    char** sortCols_;		// array of column names to sort by
    int sortOrder_;		// >=0 means increasing, <0 means decreasing
    
    int numSearchCols_;		// number of columns corresp. to searchCols_ below
    char** searchCols_;         // ptr to array of column names to compare
    char** minValues_;          // ptr to array of min column values or NULL
    char** maxValues_;          // ptr to array of max column values or NULL
   
    // copy constructor (don't use)
    AstroQuery(const AstroQuery&);

public:
    // constructors:

    // search by object Id (for a specific object)
    // The Id must be gotten by a previous search...
    AstroQuery();
    ~AstroQuery();


    // member access (set and get member values)
    //
    // note: methods with args set the member values and return the error status.
    // methods with no args return the value. 

    const char* id() const {return (id_ ? id_ : "");}
    int id(const char* s) {id_ = strdup(s); return 0;}

    const WorldOrImageCoords& pos() const {return pos_;}
    int pos(const WorldOrImageCoords& p) {pos_ = p; return p.status();}

    // set center, width and height by setting 2 positions
    int pos(const WorldOrImageCoords& p1, const WorldOrImageCoords& p2);

    double width() const {return width_;}
    void width(double w) {width_ = w;}
    double height() const {return height_;}
    void height(double h) {height_ = h;}
    int dim(double w, double h);

    double mag1() const {return mag1_;}
    double mag2() const {return mag2_;}
    int mag(double m);
    int mag(double m1, double m2); // set min/max mag with check

    double radius1() const {return radius1_;}
    double radius2() const {return radius2_;}
    int radius(double r);
    int radius(double r1, double r2); // set min/max radius with check

    char** colNames() const {return colNames_;}
    const char* colName(int col) const;
    int numCols() const {return numCols_;}
    int colNames(int n, char** ar, int freeFlag = 0);

    int numSortCols() const {return numSortCols_;}
    char** sortCols() const {return sortCols_;}
    int sort(int numSortCols, char** sortCols, int freeFlag = 0);
    int sortOrder() const {return sortOrder_;}
    void sortOrder(int i) {sortOrder_ = i;}

    int maxRows() const {return maxRows_;}
    int maxRows(int n);

    // set the search conditions (min and max values for given columns)
    int numSearchCols() const {return numSearchCols_;}
    char** searchCols() const {return searchCols_;}
    char** minValues() const {return minValues_;}
    char** maxValues() const {return maxValues_;}
    int condition(int numSearchCols, char** searchCols, 
		  char**minVals, char**maxVals, int freeFlag = 0);
    
};



#endif /* _AstroQuery_h_ */
