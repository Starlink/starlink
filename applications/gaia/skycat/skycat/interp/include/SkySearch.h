// -*-c++-*-
#ifndef _SkySearch_h_
#define _SkySearch_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: SkySearch.h,v 1.2 1998/04/02 21:16:54 abrighto Exp $
 *
 * SkySearch.h - C++ class to extend the "astrocat" Tcl command
 *               (class TclAstroCat) with image plotting capabilities.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  10 Feb 98  Created
 */


#include "TclAstroCat.h"

// forward ref
class Skycat;


/*
 * This class declares the methods used to implement additional Tcl
 * "astrocat" subcommands by extending the base class TclAstroCat.
 */
class SkySearch : public virtual TclAstroCat {
protected:
    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    // Plot the given query results in the given skycat image.
    virtual int plot(Skycat* image, const QueryResult& r);

    // called to plot objects for one symbol in a list of symbols.
    virtual int plot_objects(Skycat* image, const QueryResult& r, 
			    const char* cols, const char* symbol, 
			    const char* expr);

    // Parse the given symbol info and set the values of the last 7 args
    virtual int parse_symbol(const QueryResult& r, int nsymb, char** symb, 
		     char*& shape, char*& fg, char*& bg, char*& ratio, 
		     char*& angle, char*& label, char*& cond);
    
    // set tcl variables for column names used in expressions
    virtual int set_column_variables(const QueryResult& r, int rownum, 
			     int numCols, char** colNames,
			     const int* colIndexes);

    // Plot the symbol for the given row 
    virtual int plot_row(Skycat* image, const QueryResult& r, 
		 int rownum, const char* id, 
		 double x, double y, const char* xy_units, 
		 int numCols, char** colNames, const int* colIndexes,
		 const char* shape, const char* bg, const char* fg, 
		 const char* ratio, const char* angle, 
		 const char* label, const char* cond, 
		 const char* radius, const char* radius_units);

    // Plot a symbol in the given skycat image with the given shape,...
    virtual int plot_symbol(Skycat* image, const char* shape, 
		    const char* id, int rownum, 
		    double x, double y, const char* xy_units, 
		    double radius, const char* radius_units, 
		    const char* bg, const char* fg, 
		    double ratio, double angle, const char* label);

public:
    // constructor
    SkySearch(Tcl_Interp* interp, const char* cmdname, const char* instname)
	: TclAstroCat(interp, cmdname, instname) {}

    // entry point from Tcl
    static int astroCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[]);

    // -- additional tcl subcommands --
    virtual int imgplotCmd(int argc, char* argv[]);
};

#endif /* _SkySearch_h_ */

