/*
 * E.S.O. - VLT project/Archive
 * $Id: TclTcsCat.C,v 1.8 1998/07/29 21:15:08 abrighto Exp $
 *
 * TclTcsCat.C - method definitions for class TclTcsCat
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */
static const char* const rcsId="@(#) $Id: TclTcsCat.C,v 1.8 1998/07/29 21:15:08 abrighto Exp $";


#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <strstream.h>
#include "TcsCatalog.h"
#include "TcsLocalCatalog.h"
#include "TclQueryUtil.h"
#include "TclTcsCat.h"


/*
 * A call to this function can be made from the tkAppInit file at startup
 * to install the tcscat command
 */
extern "C" 
int TclTcsCat_Init(Tcl_Interp* interp)  
{
    // install the tcscat command 
    Tcl_CreateCommand(interp, "tcscat", TclTcsCat::tcsCatCmd, NULL, NULL);
    return TCL_OK;
}

/*
 * Implementation of the tcl extended command "tcscat" -
 * usage: see man page for more details
 */
int TclTcsCat::tcsCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName\"", NULL);
	return TCL_ERROR;
    }

    TclTcsCat* cmd = new TclTcsCat(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Constructor -
 *
 * Create a "tcscat" object in tcl for accessing the contents of TCS
 * catalogs.
 *
 * Note that the tcl command for this object is created in the
 * base class constructor.
 */
TclTcsCat::TclTcsCat(Tcl_Interp* interp, const char* cmdname, const char* instname)
    : TclAstroCat(interp, cmdname, instname)
{
}


/*
 * desctructor
 */
TclTcsCat::~TclTcsCat()
{
}


/*
 * Save the given query results with the given columns to the given 
 * filename. If iflag is true, insert in the existing file, otherwise create
 * a new file. "equinox" specifies the equinox of the data (the first 3 columns
 * are assumed to be the object id, ra and dec).
 * (This method is redefined here to do the save in TCS catalog format)
 */
int TclTcsCat::saveQueryResult(const char* filename, int numCols, char** colNames, 
			       char* info, int iflag, double equinox)
{
    // create a QueryResult object from the headings and data and 
    // save (or append) it to the file
    TcsQueryResult r;
    if (getQueryResult(numCols, colNames, info, equinox, r) != TCL_OK)
	return TCL_ERROR;

    int id_col = 0;  // catalog's id column index
    if (cat_) 
	id_col = cat_->entry()->id_col();
    
    return (iflag ? r.insert(filename, id_col) : r.save(filename));
}



/*
 * Remove the query results with the given columns and values from the given 
 * filename. "equinox" specifies the equinox of the data (the first 3 columns
 * are assumed to be the object id, ra and dec).
 * (redefined here to use TCS catalog format)
 */
int TclTcsCat::removeQueryResult(const char* filename, int numCols, char** colNames, 
				 char* info, double equinox)
{
    // create a QueryResult object from the headings and data and 
    // remove rows matching it from the file
    TcsQueryResult r;
    // if (cat_)
    //	r.entry(cat_->entry());
    if (getQueryResult(numCols, colNames, info, equinox, r) != TCL_OK)
	return TCL_ERROR;
    return r.remove(filename, 0);
}


/*
 * Open the given astromonical catalog and refer to it in future
 * queries.
 */
int TclTcsCat::openCmd(int argc, char* argv[])
{
    if (cat_)
	delete cat_;
    cat_ = TcsCatalog::open(argv[0]);
    if (!cat_)
	return TCL_ERROR;

    // set up feedback, if requested
    if (feedback_) 
	cat_->feedback(feedback_);

    return TCL_OK;
}


/*
 * Check that the given filename is a valid local TCS catalog
 * (tab table format with standard TCS columns).
 */
int TclTcsCat::checkCmd(int argc, char* argv[])
{
    return TcsLocalCatalog::check_table(argv[0]);
}



/*
 * pass a query to the current catalog and return the result as a list of
 * rows.
 *
 * usage: $cat query -option value ...
 *
 * Most options correspond to the AstroQuery class members and methods:
 *
 * 	-id     - catalog id of object, (as returned from a previous
 *                   query). If this is specified, -pos, -name, -mag and
 *                   -radius should not be specified and will be ignored.
 *
 * 	-pos    - World coordinates pos of center {ra dec}, or list 
 *                   {ra1 dec1 ra2 dec2} of 2 points for an area. 
 * 	          Position is given as {H:M:S[+-]D:M:S} in J2000
 *
 * 	-width    - dimensions of rectangle with pos at center (alternative
 * 	-height     to specifying 2 positions) in arcmin 
 *
 * 	-equinox - equinox for position (default 2000)
 *  
 * 	-mag    - max or list {min max} magnitude of object
 *
 * 	-radius - max or list (min max} radius from position
 *
 * 	-nameserver - name of nameserver catalog to use (simbad@eso, ned@eso,...)
 *
 * 	-name   - can be used instead of -pos. The name will be resolved
 *                   using the value of -nameserver (default: SIMBAD)
 *
 * 	-sort {name if column to sort by}
 *
 * 	-nrows {max number of rows to return}
 *
 * Each option has one value, however, for a range or area query, some
 * values can be a list, such as -radius "$rad1 $rad2" to give a radius
 * range or -pos "$pos1 $pos2" to give an area.
 *
 * Note that not all catalogs will support sorting by all fields.
 */
int TclTcsCat::queryCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is currently open");

    // generate the query from the command args
    AstroQuery q;
    if (genAstroQuery(interp_, argc, argv, q, pos1_, pos2_, 
		      equinox_, feedback_, cat_->entry()) != TCL_OK)
	return TCL_ERROR;
    
    // XXX make error msg if -columns was specified ?

    // make new QueryResult object, or reuse previous one
    if (result_)
	result_->clear();
    else
	result_ = new TcsQueryResult;

    int nrows = cat_->query(q, NULL, *result_);

    // format results as a tcl list of rows
    char* s;
    int i = 0, j = 0;
    int errs = 0;
    WorldCoords pos;
    char buf[1024];

    if (nrows > 0) {
	for (i = 0; i < nrows; i++) {
	    TcsCatalogObject obj;
	    if (((TcsQueryResult*)result_)->getObj(i, obj) != 0) 
		break;

	    obj.print(buf, sizeof(buf));
	    Tcl_AppendElement(interp_, buf) ;
	}
	
	// see if an error occured in the above loop (causing a break)
	if (i != nrows)
	    return TCL_ERROR;
    }

    if (nrows < 0) 
	return TCL_ERROR;	// an query error occured (and was reported)
    
    return TCL_OK;
}

