/*
 * E.S.O. - VLT project/Archive
 * $Id: TclAstroCat.C,v 1.48 1999/03/15 12:30:49 abrighto Exp $
 *
 * TclAstroCat.C - method definitions for class TclAstroCat
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * Peter W. Draper 24 Jul 00  Added (!cat_) test to symbolCmd. If argc 
 *                            was zero this test wasn't being applied.
 */
static const char* const rcsId="@(#) $Id: TclAstroCat.C,v 1.48 1999/03/15 12:30:49 abrighto Exp $";

#include "config.h"  //  From skycat util

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <fstream.h>

//  strstream will be in std:: namespace in cannot use the .h form.
#if HAVE_STRSTREAM_H
#include <strstream.h>
#define STRSTD
#else
#include <strstream>
#define STRSTD std
#endif

#include "TabTable.h"
#include "Mem.h"
#include "error.h"
#include "util.h"
#include "LocalCatalog.h"
#include "TclAstroCat.h"
#include "TclQueryUtil.h"

// from BLT package (now locally because this routine was deleted with blt2.1)
extern "C" int Blt_GraphElement(
    Tcl_Interp *interp,         /* Interpreter of the graph widget */
    char *pathName,             /* Path name of the graph widget */
    char *elemName,             /* Name of the element to reset */
    int numValues,              /* Number of values in array */
    double *valueArr,           /* Array of x,y coordinate pairs */
    char *xVector,              /* Name of x array */
    char *yVector);             /* Name of y array */

// initialize these dependent packages here for backward compat
extern "C" int Tclutil_Init(Tcl_Interp *interp);
extern "C" int Astrotcl_Init(Tcl_Interp *interp);

// generated code for bitmaps used in tcl scripts
void defineCatBitmaps(Tcl_Interp*);

// temp: initialize local copy of Tix widget set
extern "C" int Tixsam_Init(Tcl_Interp *interp);

/* 
 * Declare a table of tcl subcommands.
 *
 * NOTE: keep this table sorted, so we can use a binary search on it !
 * (select lines in emacs and use M-x sort-lines)
 */
static class TclAstroCatSubCmds {
public:
    char* name;      // method name
    int (TclAstroCat::*fptr)(int argc, char* argv[]); 
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"authorize",   &TclAstroCat::authorizeCmd,    0,  4},
    {"check",       &TclAstroCat::checkCmd,        1,  1},
    {"checkrow",    &TclAstroCat::checkrowCmd,     1,  1},
    {"close",       &TclAstroCat::closeCmd,        0,  0},
    {"copyright",   &TclAstroCat::copyrightCmd,    0,  0},
    {"dec_col",     &TclAstroCat::dec_colCmd,      0,  0},
    {"entry",       &TclAstroCat::entryCmd,        1,  4},
    {"feedback",    &TclAstroCat::feedbackCmd,     1,  1},
    {"getcol",      &TclAstroCat::getcolCmd,       2,  2},
    {"getidpos",    &TclAstroCat::getidposCmd,     1,  1},
    {"getimage",    &TclAstroCat::getimageCmd,     0,  99},
    {"getpreview",  &TclAstroCat::getpreviewCmd,   2,  4},
    {"hascol",      &TclAstroCat::hascolCmd,       1,  1},
    {"headings",    &TclAstroCat::headingsCmd,     0,  0},
    {"help",        &TclAstroCat::helpCmd,         0,  0},
    {"id_col",      &TclAstroCat::id_colCmd,       0,  0},
    {"info",        &TclAstroCat::infoCmd,         1,  2},
    {"is_tcs",      &TclAstroCat::is_tcsCmd,       0,  2},
    {"ispix",       &TclAstroCat::ispixCmd,        0,  0},
    {"iswcs",       &TclAstroCat::iswcsCmd,        0,  0},
    {"load",        &TclAstroCat::loadCmd,         1,  2},
    {"longname",    &TclAstroCat::longnameCmd,     0,  1},
    {"more",        &TclAstroCat::moreCmd,         0,  0},
    {"open",        &TclAstroCat::openCmd,         1,  1},
    {"plot",        &TclAstroCat::plotCmd,         5,  5},
    {"query",       &TclAstroCat::queryCmd,        0,  99},
    {"querypos",    &TclAstroCat::queryposCmd,     0,  0},
    {"ra_col",      &TclAstroCat::ra_colCmd,       0,  0},
    {"reload",      &TclAstroCat::reloadCmd,       0,  0},
    {"remove",      &TclAstroCat::removeCmd,       1,  4},
    {"root",        &TclAstroCat::rootCmd,         0,  0},
    {"save",        &TclAstroCat::saveCmd,         1,  5},
    {"searchcols",  &TclAstroCat::searchcolsCmd,   0,  1},
    {"servtype",    &TclAstroCat::servtypeCmd,     0,  1},
    {"shortname",   &TclAstroCat::shortnameCmd,    0,  1},
    {"showcols",    &TclAstroCat::showcolsCmd,     0,  1},
    {"sortcols",    &TclAstroCat::sortcolsCmd,     0,  1},
    {"sortorder",   &TclAstroCat::sortorderCmd,    0,  1},
    {"symbol",      &TclAstroCat::symbolCmd,       0,  1},
    {"url",         &TclAstroCat::urlCmd,          0,  1},
    {"x_col",       &TclAstroCat::x_colCmd,        0,  0},
    {"y_col",       &TclAstroCat::y_colCmd,        0,  0},
};


/*
 * Call the given method in this class with the given arguments
 */
int TclAstroCat::call(const char* name, int len, int argc, char* argv[])
{
    // since this tcl command has a lot of subcommands, 
    // we do a binary search on the method table
    int low = 0, 
	high = sizeof(subcmds_)/sizeof(*subcmds_) - 1,
	mid, 
	cond;

    while (low <= high) {
	mid = (low + high) / 2;
	if ((cond = strcmp(name, subcmds_[mid].name)) < 0) 
	    high = mid - 1;
	else if (cond > 0)
	    low = mid + 1;
	else {
	    TclAstroCatSubCmds& t = subcmds_[mid];
	    if (check_args(name, argc, t.min_args, t.max_args) != TCL_OK)
		return TCL_ERROR;
	    return (this->*t.fptr)(argc, argv);
	}
    }
    
    // not found ? extend search to parent class
    return TclCommand::call(name, len, argc, argv);
}


/*
 * A call to this function can be made from the tkAppInit file at startup
 * to install the Tcl astrocat and astroimage and relaxted commands
 *
 * (note: for convenience, we also install related commands here)
 */
extern "C" int TclAstroImage_Init(Tcl_Interp *interp);
extern "C" int TclWorldCoords_Init(Tcl_Interp *interp);
extern "C" int TclTcsCat_Init(Tcl_Interp *interp);

extern "C" 
int Cat_Init(Tcl_Interp* interp)  
{
    // For backward compatibility, initialize local packages that rtd
    // depends on:

    // initialize the tclutil package 
    if (Tclutil_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    // Tcl_StaticPackage (interp, "Tclutil", Tclutil_Init, (Tcl_PackageInitProc *) NULL);

    // initialize the astrotcl package 
    if (Astrotcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    // Tcl_StaticPackage (interp, "Astrotcl", Astrotcl_Init, (Tcl_PackageInitProc *) NULL);

    // initialize a local copy of the Tix widget set
    // (required by the catalog browser widget)
    if (Tixsam_Init(interp) != TCL_OK) 
	return TCL_ERROR;
    // Tcl_StaticPackage (interp, "Tix", Tixsam_Init, (Tcl_PackageInitProc *) NULL);


    // set up Cat Tcl package
    if (Tcl_PkgProvide (interp, "Cat", CAT_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineCatBitmaps(interp);

    // install the astroimage command 
    if (TclAstroImage_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    // install the wcs command
    if (TclWorldCoords_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    // install the tcscat command
    if (TclTcsCat_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    // install the astrocat command 
    Tcl_CreateCommand(interp, "astrocat", TclAstroCat::astroCatCmd, NULL, NULL);

    // The cat_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of CAT_LIBRARY
    char* libDir = Tcl_GetVar(interp, "cat_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "CAT_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = CAT_LIBRARY;
    }

    // Set the global Tcl variables cat_library and cat_version 
    // and add cat_library to the auto_path.
    Tcl_SetVar(interp, "cat_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "cat_version", CAT_VERSION, TCL_GLOBAL_ONLY);

    char cmd[1048];
    sprintf(cmd, "lappend auto_path %s", libDir);
    if (Tcl_Eval(interp, cmd) != TCL_OK)
	return TCL_ERROR;

    // set up the namespaces used by the itcl/itk classes
    if (Tcl_Eval(interp, 
#if (TCL_MAJOR_VERSION >= 8)
		 "namespace eval cat {namespace export *};"
		 "namespace import -force cat::*;"
#else
		 "namespace ::cat {};"
		 "import add cat;"
#endif
	) != TCL_OK)
	return TCL_ERROR;

    return TCL_OK; 
}


/* 
 * for backward compat.
 */
extern "C"
int TclAstroCat_Init(Tcl_Interp* interp)  
{
    return Cat_Init(interp);
}


/*
 * Implementation of the tcl extended command "astrocat" -
 * usage: see man page for more details
 */
int TclAstroCat::astroCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName\"", NULL);
	return TCL_ERROR;
    }

    TclAstroCat* cmd = new TclAstroCat(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Constructor -
 *
 * Create an "astrocat" object in tcl for accessing the contents of star
 * catalogs.
 *
 * Note that the tcl command for this object is created in the
 * parent class constructor.
 */
TclAstroCat::TclAstroCat(Tcl_Interp* interp, const char* cmdname, const char* instname)
: TclCommand(interp, cmdname, instname),
  cat_(NULL),
  equinox_(2000.0),
  feedback_(NULL),
  result_(NULL)
{
}


/*
 * desctructor
 */
TclAstroCat::~TclAstroCat()
{
    if (cat_) 
	delete cat_;
    if (result_)
	delete result_;
}


/*
 * Open the given astromonical catalog and refer to it in future
 * queries.
 */
int TclAstroCat::openCmd(int argc, char* argv[])
{
    if (cat_)
	delete cat_;
    cat_ = AstroCatalog::open(argv[0]);
    if (!cat_)
	return TCL_ERROR;

    // set up feedback, if requested
    if (feedback_) 
	cat_->feedback(feedback_);

    return TCL_OK;
}


/*
 * close the current catalog, if one was open
 */
int TclAstroCat::closeCmd(int argc, char* argv[])
{
    if (cat_)
	delete cat_;
    cat_ = NULL;
    return TCL_OK;
}


/*
 * save subcommand:
 *
 * usage: $cat save $filename ?$iflag? ?$data? ?$equinox? ?$headings?
 *
 * With 1 arg, save the results of the previous query to the given file.
 *
 * If $iflag is true, the data is inserted in the file if it already
 * exists and the file is sorted and duplicates removed.
 *
 * If $data is specified, it should be a Tcl list of rows to be saved, in
 * the format returned by the query command.
 *
 * If $equinox is specified, it is the equinox of the ra and dec columns in
 * the data (the first 3 columns are assumed to be id, ra and dec, unless
 * otherwise defined in the catalog config entry or header).
 *
 * If $headings is given, it is used as a Tcl list of column headings.
 * Otherwise the catalog headings are used, if there is a current
 * catalog.
 *
 * The data is saved to a file in the form of a local catalog (tab table)
 * that can be loaded again or processed by starbase utilities.
 */
int TclAstroCat::saveCmd(int argc, char* argv[])
{
    int iflag = 0;
    char* filename = argv[0];
    if (argc >= 2) {
	if (Tcl_GetBoolean(interp_, argv[1], &iflag) != TCL_OK)
	    return TCL_ERROR;
    }

    if (argc <= 2) {
	if (!result_) 
	    return error("no previous data to save");

	int id_col = 0;  // catalog's id column index
	if (cat_) 
	    id_col = cat_->entry()->id_col();
	
	return (iflag ? result_->insert(filename, id_col) : result_->save(filename));
    }

    int numCols = 0;;
    char** colNames = NULL;
    int freeColNames = 0;
    double equinox = 2000.;
    
    // get the equinox, if specified
    if (argc >= 4 && strlen(argv[3]) != 0) {
	if (strcmp(argv[3], "B1950") == 0)
	    equinox = 1950;
	else if (isdigit(*argv[3]) && Tcl_GetDouble(interp_, argv[3], &equinox) != TCL_OK)
	    return TCL_ERROR;
    }

    // get the column names
    if (argc <= 4) {		// use current catalog
	if (!cat_) 
	    return error("no catalog is currently open");
	numCols = cat_->numCols();
	colNames = cat_->colNames();
    } 
    else if (argc == 5) {	// use headings list
	if (Tcl_SplitList(interp_, argv[4], &numCols, &colNames) != TCL_OK) 
	    return TCL_ERROR;
	freeColNames++;		// delete memory later
    } 
    else {
	return error("wrong # of args for save"); // should not get here ...
    }

    // save the query results
    int status = saveQueryResult(filename, numCols, colNames, argv[2], iflag, equinox);

    // clean up
    if (freeColNames && colNames)
	free(colNames);		// free split list of column headings

    return status;
}


/*
 * Save the query results with the given columns and values to the given 
 * filename. If iflag is true, insert in the existing file, otherwise create
 * a new file. "equinox" specifies the equinox of the data (the first 3 columns
 * are assumed to be the object id, ra and dec, unless specified otherwise in
 * the config entry).
 */
int TclAstroCat::saveQueryResult(const char* filename, int numCols, char** colNames, 
				 char* info, int iflag, double equinox)
{
    // create a QueryResult object from the headings and data and 
    // save (or append) it to the file
    QueryResult r;
    int id_col = 0;  // catalog's id column index
    if (cat_) {
	r.entry(cat_->entry());
	id_col = cat_->entry()->id_col();
    }
    if (getQueryResult(numCols, colNames, info, equinox, r) != TCL_OK)
	return TCL_ERROR;
    return (iflag ? r.insert(filename, id_col) : r.save(filename));
}


/*
 * Remove the query results with the given columns and values from the given 
 * filename. 
 */
int TclAstroCat::removeQueryResult(const char* filename, int numCols, char** colNames, 
				   char* info, double equinox)
{
    // create a QueryResult object from the headings and data and 
    // remove rows matching it from the file
    QueryResult r;
    if (cat_)
	r.entry(cat_->entry());
    if (getQueryResult(numCols, colNames, info, equinox, r) != TCL_OK)
	return TCL_ERROR;
    return r.remove(filename, 0);
}


/*
 * remove subcommand:
 *
 * usage: $cat remove $filename ?$data? ?$equinox? ?$headings?
 *
 * With 1 arg, remove the results of the previous query from the given file.
 *
 * If $data is specified, it should be a Tcl list of rows to be removed, in
 * the format returned by the query command.
 *
 * If $equinox is specified, it is the equinox of ra and dec in $data (so that
 * ra,dec can be converted to J2000 for comparison).
 *
 * If $headings is given, it is used as a Tcl list of column headings.
 * Otherwise the catalog headings are used, if there is a current
 * catalog.
 *
 * The data rows are removed from the file, which should be in the form of a 
 * local catalog (tab table), such as that used by the starbase utilities.
 */
int TclAstroCat::removeCmd(int argc, char* argv[])
{
    if (argc <= 1) {
	if (!result_) 
	    return error("no previous data for remove");
    
	return result_->remove(argv[0], 0);
    }

    int numCols = 0;;
    char** colNames = NULL;
    int freeColNames = 0;
    double equinox = 2000.;
    
    // get the equinox, if specified
    if (argc >= 3 && strlen(argv[2]) != 0) {
	if (strcmp(argv[2], "B1950") == 0)
	    equinox = 1950;
	else if (isdigit(*argv[2]) && Tcl_GetDouble(interp_, argv[2], &equinox) != TCL_OK)
	    return TCL_ERROR;
    }

    // get the column names
    if (argc <= 3) {		// use current catalog
	if (!cat_) 
	    return error("no catalog is currently open");
	numCols = cat_->numCols();
	colNames = cat_->colNames();
    } else if (argc == 4) {	// use headings list
	if (Tcl_SplitList(interp_, argv[3], &numCols, &colNames) != TCL_OK) 
	    return TCL_ERROR;
	freeColNames++;		// delete memory later
    } else {
	return error("wrong # of args for remove"); // should not get here ...
    }

    // create a QueryResult object from the headings and data and 
    // remove rows matching it from the file
    int status = removeQueryResult(argv[0], numCols, colNames, argv[1], equinox);
    
    // clean up
    if (freeColNames && colNames)
	free(colNames);		// free split list of column headings

    return status;
}


/*
 * Check that the given filename is a valid local catalog
 * (tab table format).
 */
int TclAstroCat::checkCmd(int argc, char* argv[])
{
    return LocalCatalog::check_table(argv[0]);
}


/* 
 * Tcl subcommand: "iswcs" returns true if the catalog is based on world
 * coordinates
 */
int TclAstroCat::iswcsCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is open");

    return set_result(cat_->isWcs());
}


/* 
 * Tcl subcommand: "ispix" returns true if the catalog is based on image
 * pixel coordinates
 */
int TclAstroCat::ispixCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is open");

    return set_result(cat_->isPix());
}


/*
 * Check that the given row (tcl list of columns) is valid for a local
 * catalog. The row should contain valid values for ra and dec, if we are
 * using world coords, or x and y, if we are using image coords. The
 * default columns are "id ra dec ...", but can be overridden in the
 * catalog config file or header.
 *
 * usage: $cat checkrow $data
 *
 * generates an error if the position (ra, dec) or (x, y) is not found or
 * is not in range
 */
int TclAstroCat::checkrowCmd(int argc, char* argv[])
{
    int numValues;
    char** values;
    if (Tcl_SplitList(interp_, argv[0], &numValues, &values) != TCL_OK) 
	return TCL_ERROR;

    // if (numValues < 3) {
    //    free(values);
    //    return error("expected at least 3 columns (id, ra, dec or id, x, y)");
    // }

    if (! cat_)
	return error("no catalog is currently selected");
    WorldOrImageCoords pos;
    if (cat_->isWcs()) 
	pos = WorldCoords(values[cat_->ra_col()], values[cat_->dec_col()]);
    else if (cat_->isPix())
	pos = ImageCoords(values[cat_->x_col()], values[cat_->y_col()]);
    free(values);
    return pos.status();
}


/*
 * feedback subcommand: 
 *
 * specifies a Tcl file descriptor to use to write feedback info during
 * HTTP transfer of catalog data.  1 Arg: file descriptor.
 */
int TclAstroCat::feedbackCmd(int argc, char* argv[])
{
    if (strlen(argv[0]) != 0) {
	if (Tcl_GetOpenFile(interp_, argv[0], 1, 1, (ClientData*)&feedback_) != TCL_OK)
	    return TCL_ERROR;
    }
    else {
	feedback_ = NULL;
    }
    if (cat_)
	cat_->feedback(feedback_);
    return TCL_OK;
}


/*
 * pass a query to the current catalog and return the result as a list of
 * rows.
 *
 * usage: $cat query -option value ...
 *
 * See comments in genAstroQuery in TclQueryUtil.C for details on the
 * options.
 *
 * The result is a tcl list of query result rows, where each row is a tcl list
 * of column values.
 */
int TclAstroCat::queryCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is currently open");

    // generate the query from the command args
    AstroQuery q;
    if (genAstroQuery(interp_, argc, argv, q, pos1_, pos2_, 
		      equinox_, feedback_, cat_->entry()) != TCL_OK)
	return TCL_ERROR;

    // make new QueryResult object, or reuse previous one
    if (result_)
	result_->clear();
    else
	result_ = new QueryResult;

    // do the query
    int nrows = cat_->query(q, NULL, *result_);

    // get the results
    int ncols = result_->numCols();
    char* s;
    int i = 0, j = 0;
    int errs = 0;

    if (nrows >= 0) {
	Tcl_ResetResult(interp_);

	for (i = 0; i < nrows; i++) {
	    
	    // start a row
	    Tcl_AppendResult(interp_, " {", NULL);
	    
	    if (cat_->isWcs()) { // include formatted world coords
		WorldCoords pos;
		if (result_->getPos(i, pos) != 0) 
		    break;
		// format the ra,dec position arguments in H:M:S...
		char ra_buf[32], dec_buf[32];
		int ra_col = result_->ra_col(), dec_col = result_->dec_col();
		pos.print(ra_buf, dec_buf, equinox_);
		// put the column values in a list
		for (j = 0; j < ncols; j++) {
		    if (result_->get(i, j, s) != 0) 
			break;
		    if (j == ra_col)
			Tcl_AppendElement(interp_, ra_buf) ;
		    else if (j == dec_col)
			Tcl_AppendElement(interp_, dec_buf) ;
		    else
			Tcl_AppendElement(interp_, s) ;
		}
	    }
	    else {  // image coords - no special formatting needed
		// put the column values in a list
		for (j = 0; j < ncols; j++) {
		    if (result_->get(i, j, s) != 0) 
			break;
		    Tcl_AppendElement(interp_, s) ;
		}
	    }
	    if (j != ncols)
		break;

	    // end a row
	    Tcl_AppendResult(interp_, "}", NULL);
	}
	
	// see if an error occured in the above loop (causing a break)
	if (i != nrows)
	    return TCL_ERROR;

	return TCL_OK;
    }
    return TCL_ERROR;	// an query error occured (and was reported)
}


/*
 * Request an image from the current image server and return (in Tcl) 
 * the name of the FITS file holding the image.
 *
 * usage: $cat getimage -option value ...
 *
 * See comments in genAstroQuery in TclQueryUtil.C for details on the
 * options.
 */
int TclAstroCat::getimageCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is currently open");

    // generate the query from the command args
    AstroQuery q;
    if (genAstroQuery(interp_, argc, argv, q, pos1_, pos2_, 
		      equinox_, feedback_, cat_->entry()) != TCL_OK)
	return TCL_ERROR;

    // do the query
    if (cat_->getImage(q) != 0)
	return TCL_ERROR;

    return set_result(cat_->tmpfile());
}


/*
 * querypos subcommand:
 *
 * usage: $cat querypos 
 *
 * Return the world or image coordinate position arguments from the most
 * recent query, posibly expanded by a name server such as SIMBAD. The
 * result is a list of the form {ra dec equinox} for world coordinates
 * or just {x y} for image coords.
 *
 */
int TclAstroCat::queryposCmd(int argc, char* argv[])
{
    char buf[126];
    STRSTD::ostrstream os(buf, sizeof(buf));

    if (! pos1_.isNull()) {
	pos1_.print(os, equinox_);	// print coords in given equinox
	if (pos1_.isWcs())
	    os << " " << equinox_;
	os << ends;
	return set_result(buf);
    }
    return TCL_OK;
}


/*
 * catalog "info" subcommand:
 *
 * usage: $cat info $serv_type ?$directory?
 *
 * This command returns a list of catalogs from the catalog config file.
 * The "serv_type" argument determines which catalogs are listed (one of:
 * catalog, namesvr, imagesvr, directory, local...).
 *
 * If $serv_type is an empty string, all entries are returned.
 *
 * If $directory is specified, the return list will be from the given
 * catalog directory entry, retrieved via HTTP if needed. The default, if
 * no $directory is given, is the top level list read from the default
 * config file at startup.
 */
int TclAstroCat::infoCmd(int argc, char* argv[])
{
    Tcl_ResetResult(interp_);

    CatalogInfoEntry* e;
    if (argc == 2) {
	e = CatalogInfo::lookup(argv[1]);
	if (!e)
	    return TCL_ERROR;
	if (strcmp(e->servType(), "directory") != 0)
	    return error("expected a catalog directory entry");
	if (! e->link()) {
	    if (CatalogInfo::load(e) != 0)  // follow catalog dir link
		return TCL_ERROR;
	}
    }
    else {
	e = CatalogInfo::root();
    }
	
    if (!e || !e->link()) 
	return error("can't find catalog info");
    e = e->link();  // get pointer to first catalog in directory
    
    // get the serv_type
    Tcl_ResetResult(interp_);
    int n = strlen(argv[0]);
    for (; e != NULL; e = e->next()) {
	if (strncmp(argv[0], e->servType(), n) == 0) {
	    // ignore local catalogs in /tmp, since they will be deleted later
	    if (! (strcmp(e->servType(), "local") == 0 
		   && strncmp(e->url(), "/tmp/", 5) == 0)) {
		Tcl_AppendElement(interp_, (char*)e->longName());
	    }
	}
    }
    return TCL_OK;
}


/*
 * catalog "root" subcommand:
 *
 * usage: $cat root
 *
 * This command returns the name of the root catalog directory
 *
 */
int TclAstroCat::rootCmd(int argc, char* argv[])
{
    Tcl_ResetResult(interp_);

    const CatalogInfoEntry* e = CatalogInfo::root();
    if (!e) 
	return error("no catalogs are loaded");
    
    return set_result(e->longName());
}


/*
 * append the given keyword/value pair to the Tcl result
 */
void TclAstroCat::appendKeyVal(const char* keyword, const char* value)
{
    if (! value)
	return;

    Tcl_AppendResult(interp_, " {", NULL);
    Tcl_AppendElement(interp_, (char*)keyword);
    Tcl_AppendElement(interp_, (char*)value);
    Tcl_AppendResult(interp_, "}", NULL);
}


/*
 * append the given list value to the Tcl result. The value is assumed
 * to already be a a colon separated list of space separated lists. 
 * "a b c : d e f". Here we make sure that the lists are proper Tcl lists,
 * with special chars handled correctly.
 */
int TclAstroCat::appendListVal(const char* value)
{
    if (! value)
	return TCL_OK;

    char* v = strdup(value);
    char* p;
    char* q = v;

    do {
	Tcl_AppendResult(interp_, " {", NULL);
    
	p = strchr(q, ':');
	if (p) 
	    *p++ = '\0';

	int numValues;
	char** values;
	if (Tcl_SplitList(interp_, q, &numValues, &values) != TCL_OK) {
	    free(v);
	    Tcl_ResetResult(interp_);
	    return Tcl_SplitList(interp_, q, &numValues, &values);
	}
	q = p;
	for(int i = 0; i < numValues; i++) {
	    Tcl_AppendElement(interp_, values[i]);
	}
	free(values);

	Tcl_AppendResult(interp_, "}", NULL);
    } while(p);

    return TCL_OK;
}


/*
 * append the given keyword/value pair to the Tcl result and make a
 * proper tcl list of lists out of the value, which should have the
 * format "a b c : d e f : ...", where ':' is the sep char for the main
 * lists, and space is the sep char for the sublists.
 */
int TclAstroCat::appendKeyListVal(const char* keyword, const char* value)
{
    if (! value || ! strlen(value))
	return TCL_OK;

    Tcl_AppendResult(interp_, " {", NULL);
    Tcl_AppendElement(interp_, (char*)keyword);

    Tcl_AppendResult(interp_, " {", NULL);
    if (appendListVal(value) != TCL_OK)
	return TCL_ERROR;
    Tcl_AppendResult(interp_, "}", NULL);

    Tcl_AppendResult(interp_, "}", NULL);
    return TCL_OK;
}


/*
 * This method is used to write the value of entries that have the
 * format: "a b c : d e f" in the config file, but are treated as
 * tcl lists of lists in Tcl.
 *
 * Write the config file format entry value part to the stream.
 */
int TclAstroCat::tclListToConfigStreamValue(const char* tclList, ostream& os)
{
    int numValues = 0;
    char** values = NULL;

    // split into {keyword value} lists
    if (Tcl_SplitList(interp_, (char*)tclList, &numValues, &values) != TCL_OK) 
	return TCL_ERROR;
    
    for (int i = 0; i < numValues; i++) {
	os << values[i];
	if (i < (numValues-1))
	    os << " : ";
    }

    free(values);
    return TCL_OK;
}


/*
 * write the given catalog config entry line (tcl list format) to the
 * given stream in the format:
 *
 * keyword; value
 */
int TclAstroCat::tclListToConfigStreamLine(const char* tclList, ostream& os)
{
    int numValues = 0;
    char** values = NULL;

    // split into {keyword value} lists
    if (Tcl_SplitList(interp_, (char*)tclList, &numValues, &values) != TCL_OK) 
	return TCL_ERROR;

    if (numValues != 2) {
	free(values);
	return error("astrocat: expected {keyword value} list, not: ", tclList);
    }
    char* keyword = values[0];
    char* value = values[1];

    // append to an inline config entry to pass to the entry update method
    if (strcmp(keyword, "symbol") == 0 || strcmp(keyword, "search_cols") == 0) {
	os << keyword << ": ";
	if (tclListToConfigStreamValue(value, os) != TCL_OK) {
	    free(values);
	    return TCL_ERROR;
	}
	os << endl;
    }
    else {
	os << keyword << ": " << value << endl;
    }
    free(values);
    return TCL_OK;
}


/*
 * write the given catalog config entry (tcl list format) to the
 * given stream in the format:
 *
 * keyword; value
 * keyword: valuue
 */
int TclAstroCat::tclListToConfigStream(const char* tclList, ostream& os)
{
    int numValues = 0;
    char** values = NULL;

    // split into {keyword value} lists
    if (Tcl_SplitList(interp_, (char*)tclList, &numValues, &values) != TCL_OK) 
	return TCL_ERROR;

    for (int i = 0; i < numValues; i++) {
	if (tclListToConfigStreamLine(values[i], os) != TCL_OK) {
	    free(values);
	    return TCL_ERROR;
	}
    }
    free(values);
    return TCL_OK;
}


/*
 * catalog "entry" subcommand:
 *
 * usage: $cat entry get ?name? ?directory?
 *    or: $cat entry set $info ?name? ?directory?
 *    or: $cat entry update $info ?name? ?directory?
 *    or: $cat entry add $info ?directory?
 *    or: $cat entry remove $name
 *
 * "entry get" returns the catalog config entry for the currently open
 * catalog or for the given catalog if one is specified.
 *
 * The format of the return value is a tcl list of {keyword value} pairs:
 *  
 *  {{key value} {key value} ...}
 *
 * for example:
 *
 * {{serv_type catalog} 
 *  {long_name "Guide Star Catalog at ESO"}
 *  {short_name gsc@eso}
 *  {url http://archive.eso.org/...}
 *  {symbol ...}
 *  {id_col 0}
 *  {ra_col 1}
 *  {dec_col 2}
 * ...}
 *
 * If the "directory" argument is specified, the entry is searched for
 * starting in the given catalog directory, otherwise it is searched for
 * starting at the top level directory (the catalog entries are linked
 * in a hierarchical list or directory structure).
 *
 * "entry update" allows you to update fields in the catalog's config
 * entry.  Some fields, such as the catalog name and URL can not be
 * changed in this way, however the column positions id_col, ra_col,
 * dec_col, etc... may be updated.  The format for the update info is the
 * same as that returned by "entry get". The directory argument has the
 * same meaning as with the "entry get" subcommand.
 *
 * Note: The "entry update" command is needed, for example, when a
 * catalog query was run in a subprocess. The results of the query may
 * include config info, such as the default plot symbol and the columns
 * for id, ra and dec. This subcommand can be used to get this
 * information back into the parent process catalog entry.
 *
 * "entry add" adds a new catalog entry to the internal list. The format
 * of the argument is the same as that for update. The entry is added to the
 * given directory or the top level if none is specified.
 *
 * "entry remove" removes the entry for the named catalog from the internal 
 * default catalog list. 
 *
 */
int TclAstroCat::entryCmd(int argc, char* argv[])
{
    CatalogInfoEntry* e;
    CatalogInfoEntry* dir = CatalogInfo::root();
    if (! dir)
	return TCL_ERROR;

    Tcl_ResetResult(interp_);
    if (strcmp(argv[0], "get") == 0) {
	char buf[80];
	
	if (argc == 1) {
	    // get entry from current catalog
	    if (! cat_) 
		return error("no catalog is open");
	    e = cat_->entry();
	}
	else {
	    if (argc > 2) {
		// use given catalog directory
		dir = CatalogInfo::lookup(argv[2]);
		if (!dir)
		    return TCL_ERROR;
	    } 
	    // get entry from named catalog
	    e = CatalogInfo::lookup(dir, argv[1]);
	    if (!e) 
		return error("can't find catalog entry for: ", argv[1]);
	}

	// return tcl list with config entry info
	appendKeyVal("serv_type", (char*)e->servType());
	appendKeyVal("long_name", (char*)e->longName());
	appendKeyVal("short_name", (char*)e->shortName());
	appendKeyVal("url", (char*)e->url());
	appendKeyVal("backup1", (char*)e->backup1());
	appendKeyVal("backup2", (char*)e->backup2());

	if (appendKeyListVal("symbol", (char*)e->symbol()) != TCL_OK)
	    return TCL_ERROR;

	if (appendKeyListVal("search_cols", (char*)e->searchCols()) != TCL_OK)
	    return TCL_ERROR;

	appendKeyVal("sort_cols", (char*)e->sortCols());
	appendKeyVal("sort_order", (char*)e->sortOrder());
	appendKeyVal("show_cols", (char*)e->showCols());

	appendKeyVal("copyright", (char*)e->copyright());
	appendKeyVal("help", (char*)e->help());

	// don't include these values if they are already defaults

	if (e->equinox() != 2000.) {
	    sprintf(buf, "%lg", e->equinox());
	    appendKeyVal("equinox", buf);
	}

	if (e->id_col() != 0) {
	    sprintf(buf, "%d", e->id_col());
	    appendKeyVal("id_col", buf);
	}

	if (e->ra_col() != 1) {
	    sprintf(buf, "%d", e->ra_col());
	    appendKeyVal("ra_col", buf);
	}

	if (e->dec_col() != 2) {
	    sprintf(buf, "%d", e->dec_col());
	    appendKeyVal("dec_col", buf);
	}

	if (e->x_col() != -1) {
	    sprintf(buf, "%d", e->x_col());
	    appendKeyVal("x_col", buf);
	}

	if (e->y_col() != -1) {
	    sprintf(buf, "%d", e->y_col());
	    appendKeyVal("y_col", buf);
	}

	if (e->is_tcs()) {
	    sprintf(buf, "%d", e->is_tcs());
	    appendKeyVal("is_tcs", buf);
	}
    }
    else if (strcmp(argv[0], "remove") == 0) {
	e = CatalogInfo::lookup(argv[1]);
	if (!e)
	    return TCL_ERROR;
	return CatalogInfo::remove(e);
    }
    else {
	int update = 0, set = 0;
	if (strcmp(argv[0], "update") == 0) {
	    update++;
	    if (argc == 4 && !(dir = CatalogInfo::lookup(argv[3])))
		return TCL_ERROR;
	}
	else if (strcmp(argv[0], "set") == 0) {
	    set++;
	    if (argc == 4 && !(dir = CatalogInfo::lookup(argv[3])))
		return TCL_ERROR;
	}
	else if (strcmp(argv[0], "add") == 0) {
	    if (argc == 3 && !(dir = CatalogInfo::lookup(argv[2])))
		return TCL_ERROR;
	}
	else {
	    return error("unknown astrocat entry subcommand: ", argv[0]);
	}

	// update the entry with new info
	if (argc < 2)
	    return error("missing catalog entry argument");

	char buf[10*1024];
	STRSTD::ostrstream os(buf, sizeof(buf));

	// convert tcl list entry to config file format
	if (tclListToConfigStream(argv[1], os) != TCL_OK)
	    return TCL_ERROR;

	// read the new config file entry
	os << ends;
	if (! os)
	    return error("internal error writing config entry");

	STRSTD::istrstream is(buf);
	
	if (update || set) {
	    if (argc == 2) {
		// use current catalog entry
		if (! cat_) 
		    return error("no catalog is open");
		e = cat_->entry();
	    }
	    else {
		// update named catalog entry (in named dir)
		e = CatalogInfo::lookup(dir, argv[2]);
		if (!e) 
		    return error("can't find catalog entry for: ", argv[2]);
	    }
	    if (update) {
		/* entry update */
		CatalogInfo::updateConfigEntry(is, e);
	    }
	    else { /* entry set */
		CatalogInfoEntry* ne = CatalogInfo::load(is);
		if (! ne)
		    return TCL_ERROR;
		*e = *ne;
	    }
	}
	else {
	    /* entry add */
	    e = CatalogInfo::load(is);
	    if (! e)
		return TCL_ERROR;
	    // add to end of main entry list
	    return CatalogInfo::append(e);
	}
    }
    return TCL_OK;
}


/*
 * load subcommand: 
 * 
 * usage: $cat load $filename ?longName?
 *
 * This command loads the named catalog config file, making the catalogs
 * in it available to the application under the given name
 * (default: last component of file name).
 */
int TclAstroCat::loadCmd(int argc, char* argv[])
{
    ifstream is(argv[0]);
    if (!is)
	return sys_error("can't open file: ", argv[0]);

    CatalogInfoEntry* e = CatalogInfo::load(is, argv[0]);
    if (!e)
	return TCL_ERROR;

    // make a directory entry for the file and add it to the catalog tree
    CatalogInfoEntry* dir = new CatalogInfoEntry;
    dir->servType("directory");

    char url[1024+5];
    sprintf(url, "file:%s", argv[0]);
    dir->url(url);

    const char* s = fileBasename(argv[0]);
    dir->shortName(s);
    if (argc > 1)
	s = argv[1];
    dir->longName(s); // name to be displayed
    dir->link(e);

    return CatalogInfo::append(dir);
}


/*
 * reload subcommand: 
 * 
 * usage: $cat reload
 *
 * This command reloads the default catalog config file and updates the
 * internal catalog entries from it. This is meant to be used after
 * the config file has been edited by hand to make the new data available
 * to the application.
 */
int TclAstroCat::reloadCmd(int argc, char* argv[])
{
    return CatalogInfo::reload();
}


/*
 * more subcommand: return true if there were more rows
 * to fetch on the last query (i.e.: not all rows that were
 * found were returned due to limits)
 */
int TclAstroCat::moreCmd(int argc, char* argv[])
{
    return set_result(cat_ ? cat_->more() : 0);
}


/*
 * headings subcommand: return a Tcl list of the column headings
 */
int TclAstroCat::headingsCmd(int argc, char* argv[])
{
    if (cat_) {
	int n = cat_->numCols();
	if (n < 0)
	    return TCL_ERROR;
	for(int i = 0; i < n; i++)
	    Tcl_AppendElement(interp_, (char*)cat_->colName(i));
    }
    return TCL_OK;
}


/*
 * column subcommands: id_col, ra_col, dec_col, x_col, y_col
 * Just return the column index from the catalog config entry.
 */
int TclAstroCat::id_colCmd(int argc, char* argv[])
{
    if (cat_) 
	return set_result(cat_->id_col());
    return TCL_OK;
}
int TclAstroCat::ra_colCmd(int argc, char* argv[])
{
    if (cat_) 
	return set_result(cat_->ra_col());
    return TCL_OK;
}
int TclAstroCat::dec_colCmd(int argc, char* argv[])
{
    if (cat_) 
	return set_result(cat_->dec_col());
    return TCL_OK;
}
int TclAstroCat::x_colCmd(int argc, char* argv[])
{
    if (cat_) 
	return set_result(cat_->x_col());
    return TCL_OK;
}
int TclAstroCat::y_colCmd(int argc, char* argv[])
{
    if (cat_) 
	return set_result(cat_->y_col());
    return TCL_OK;
}


/*
 * is_tcs command: 
 *
 * usage:  $cat is_tcs ?catalog? ?boolValue?
 *
 * With 0 or 1 arg, returns true if this catalog (or the given one) 
 * is a TCS catalog.
 *
 * If a value is specified, it sets the tcs flag for the given catalog.
 * 
 */
int TclAstroCat::is_tcsCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->is_tcs());
	return set_result(0);
    }
    CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (!e) 
	return TCL_ERROR;

    if (argc == 1)
	return set_result(e->is_tcs());
    
    int is_tcs = 0;
    if (Tcl_GetBoolean(interp_, argv[1], &is_tcs) != TCL_OK)
	return TCL_ERROR;
    e->is_tcs(is_tcs);

    return set_result(0);
}

/*
 * symbol subcommand: 
 *
 * usage:  $cat symbol ?value?
 *
 * With no args, return the "symbol" entry for this catalog.
 * With one arg, specify a new value for the symbol entry,
 * which should be a list of the form: 
 *
 *    {{colNames symbolExpr sizeExpr} ...}
 * 
 * where colNames is a list of column names used, symbolExpr is one of
 * the accepted symbol expressions, such as "circle ?color?" and
 * sizeExpr is an expression (or a list of {expr ?units?}) giving the
 * radius of an object.
 */
int TclAstroCat::symbolCmd(int argc, char* argv[])
{
    //  PWD: test cat_ before continuing.
    if ( ! cat_ ) {
        return error( "no catalog is open" );
    }
    if (argc == 0) {
        return appendListVal(cat_->symbol());
    }
    cat_->entry()->symbol(argv[0]);
    return TCL_OK;
}

/*
 * searchcols subcommand: 
 *
 * usage:  $cat searchcols ?value?
 *
 * With no args, return the "search_cols" entry for this catalog.
 * With one arg, specify a new value for the search_cols entry,
 * which should be a list of the form: 
 *
 *    {{colName minLabel maxLabel} ...}
 *
 */
int TclAstroCat::searchcolsCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return appendListVal(cat_->searchCols());
    }
    else if (! cat_)
	return error("no catalog is open");
    cat_->entry()->searchCols(argv[0]);
    return TCL_OK;
}


/*
 * sortcols subcommand: 
 *
 * usage:  $cat sortcols ?value?
 *
 * With no args, return the "sort_cols" entry for this catalog.
 * With one arg, specify a new value for the sort_cols entry,
 * which should be a list of the form: 
 *
 *    {col1 col2 ...}
 *
 */
int TclAstroCat::sortcolsCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->sortCols());
    }
    else if (! cat_)
	return error("no catalog is open");
    cat_->entry()->sortCols(argv[0]);
    return TCL_OK;
}


/*
 * sortorder subcommand: 
 *
 * usage:  $cat sortorder ?value?
 *
 * With no args, return the "sort_order" entry for this catalog.
 * With one arg, specify a new value for the sort_order entry,
 * which should be a list of the form: 
 *
 *    {col1 col2 ...}
 *
 */
int TclAstroCat::sortorderCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->sortOrder());
    }
    else if (! cat_)
	return error("no catalog is open");
    cat_->entry()->sortOrder(argv[0]);
    return TCL_OK;
}


/*
 * showcols subcommand: 
 *
 * usage:  $cat showcols ?value?
 *
 * With no args, return the "show_cols" entry for this catalog.
 * With one arg, specify a new value for the show_cols entry,
 * which should be a list of the form: 
 *
 *    {col1 col2 ...}
 *
 */
int TclAstroCat::showcolsCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->showCols());
    }
    else if (! cat_)
	return error("no catalog is open");
    cat_->entry()->showCols(argv[0]);
    return TCL_OK;
}


/*
 * copyright subcommand: return the copyright info for this catalog
 * from the catalog config file.
 */
int TclAstroCat::copyrightCmd(int argc, char* argv[])
{
    if (cat_) {
	return set_result(cat_->copyright());
    }
    return TCL_OK;
}


/*
 * help subcommand: return the help info for this catalog
 * from the catalog config file.
 */
int TclAstroCat::helpCmd(int argc, char* argv[])
{
    if (cat_) {
	return set_result(cat_->help());
    }
    return TCL_OK;
}


/*
 * servtype subcommand: return the servtype for this catalog or
 * for the given catalog.
 * 
 * usage: $cat servtype ?catalogLongName?
 */
int TclAstroCat::servtypeCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) {
	    return set_result(cat_->servType());
	}
	return TCL_OK;
    }

    // return the servtype for the given catalog name
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->servType());
    return TCL_OK;
}


/*
 * url subcommand: return the url for this catalog or
 * for the given catalog.
 * 
 * usage: $cat url ?catalogLongName?
 */
int TclAstroCat::urlCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) {
	    return set_result(cat_->url());
	}
	return TCL_OK;
    }

    // return the url for the given catalog name
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->url());
    return TCL_OK;
}


/*
 * hascol subcommand: return true if the catalog contains the
 * given column name (in the config entry).
 */
int TclAstroCat::hascolCmd(int argc, char* argv[])
{
    if (cat_) {
	return set_result(cat_->hasCol(argv[0]));
    }
    return set_result(0);
}


/*
 * getcol subcommand: given a row of output from a query,
 * return the value for the named column in Tcl
 *
 * example: set preview [$cat getcol preview $row]
 */
int TclAstroCat::getcolCmd(int argc, char* argv[])
{
    int numValues;
    char** values;
    int index = -1;

    if (cat_) 
	index = cat_->colIndex(argv[0]);
    
    if (index >= 0) {
	if (Tcl_SplitList(interp_, argv[1], &numValues, &values) != TCL_OK) 
	    return TCL_ERROR;
	if (index < numValues) {
	    set_result(values[index]);
	}
	free(values);
    } 
    else {
	return error("no such column: ", argv[0]);
    }
    
    return TCL_OK;
}


/*
 * getidpos subcommand: given a row of output from a query, return a list
 * {id ra dec} (or {id x y} if we're not using wcs) for the given
 * row. The positions of the 3 columns default to the first 3 columns:
 * id, ra and dec, but might be specified differently in the catalog
 * config entry or result header.
 *
 * example: lassign [$cat getidpos $row] id ra dec
 */
int TclAstroCat::getidposCmd(int argc, char* argv[])
{
    // get column indexes from catalog info
    int id_col = 0, ra_col = 1, dec_col = 2;
    if (cat_) { 
	id_col = cat_->id_col();
	ra_col = cat_->ra_col();
	dec_col = cat_->dec_col();
    }
    
    int numValues;
    char** values;
    if (Tcl_SplitList(interp_, argv[0], &numValues, &values) != TCL_OK) 
	    return TCL_ERROR;

    Tcl_ResetResult(interp_);
    if (id_col >= 0 && id_col < numValues
	&& ra_col >= 0 && ra_col < numValues
	&& dec_col >= 0 && dec_col < numValues) {
	Tcl_AppendElement(interp_, values[id_col]);
	Tcl_AppendElement(interp_, values[ra_col]);
	Tcl_AppendElement(interp_, values[dec_col]);
    }

    free(values);
    return TCL_OK;
}


/*
 * getpreview subcommand: given a URL from a query output line, get the
 * preview data and put it in a temp file.  The return value in Tcl is a
 * list of {filename type}, where filename is the name of the file
 * containing the preview data and type is either "image" for a
 * decompressed FITS image or "table" for a tab table with data to be
 * plotted in a graph.
 *
 * The temp file is re-used and deleted in the destructor.
 *
 * example: set preview [$cat getpreview -url "http://...." ?-tmpfile "/tmp/img..."?]
 *
 * -url specifies the URL to use
 *
 * -tmpfile can be used to specify the image file name to use
 *
 */
int TclAstroCat::getpreviewCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is open");

    char* url = NULL;
    for (int i = 0; i < argc; i += 2) {
	char* option = argv[i];
	char* value = argv[i+1];
	if (strcmp(option, "-url") == 0) {
	    url = value;
	}
	else if (strcmp(option, "-tmpfile") == 0) {
	    if (value) {
		unlink(cat_->tmpfile());
		cat_->tmpfile(value);
	    }
	}
    }
    if (!url) 
	return error("missing -url option");
    
    char* type;
    if (cat_->getPreview(url, type) != 0)
	return TCL_ERROR;

    Tcl_ResetResult(interp_);
    Tcl_AppendElement(interp_, (char*)cat_->tmpfile());
    Tcl_AppendElement(interp_, type);

    return TCL_OK;
}

/*
 * authorize subcommand: If the previous HTTP GET returned an
 * authorization error: (The HTML error text returned in Tcl contained 
 * the string: "Authorization Required"), the application can
 * ask the user to enter a username and password to use to access the
 * URL and then retry the GET after using this subcommand to set the
 * authorization information to use.
 *
 * usage: 
 *
 *     $cat authorize
 *     $cat authorize username passwd ?realm server?
 *
 * With no arguments, this command returns a list of the form
 *
 *   {needpasswd realm server}
 *
 * where:
 *    needpasswd is nonzero if a password is required for the URL
 *    realm is the string taken from the HTTP header (Basic realm=...).
 *    server is the name of the target server that wants the password.
 *
 * If arguments are specified, they should be the username and password
 * and optionally the realm and server hostname. If the last 2 args are
 * specified, the information is stored in a file for later lookup 
 * (see tclutil/util/src/HTTP.C).
 */
int TclAstroCat::authorizeCmd(int argc, char* argv[])
{
    if (!cat_) 
	return error("no catalog is open");
    
    if (argc == 0) {
	HTTP& http = cat_->http();
	char buf[1024];
	STRSTD::ostrstream os(buf, sizeof(buf));
	os << http.authorizationRequired() 
	   << " " << http.www_auth_realm()
	   << " " << http.hostname() << ends;
	return set_result(buf);
    }

    if (argc == 2) 
	HTTP::authorize(argv[0], argv[1]);
    else if (argc == 4) 
	HTTP::authorize(argv[0], argv[1], argv[2], argv[3]);
    else
	return error("expected: astrocat authorize ?username passwd realm server?");

    return TCL_OK;
}


/*
 * longname subcommand: 
 *
 * usage: $cat longname ?name?
 *
 * returns the long_name field from the catalog config file
 * for the given long or short name, or for the current catalog
 */
int TclAstroCat::longnameCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->longName());
	return TCL_OK;
    }
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->longName());
    return TCL_OK;
}


/*
 * plot subcommand: plot the contents of the given tab table file in the given 
 * BLT graph widget).
 * 
 * usage: $cat plot $graph $element $filename xVector yVector
 *
 *      element    is the BLT graph element name
 * 
 *      filename   is the local catalog file
 *
 *      xVector    is the name of the BLT x vector
 *
 *      yVector    is the name of the BLT y vector
 *
 * The data for the given element in the given graph will be set
 * directly from here without going through tcl.
 *
 * The return value in Tcl is the number of rows (for the graph's x-axis)
 *
 */
int TclAstroCat::plotCmd(int argc, char* argv[])
{
    char* graph = argv[0];
    char* elem = argv[1];
    char* filename = argv[2];

    // mmap the file to get the data in memory
    Mem m(filename);
    if (m.status() != 0)
	return TCL_ERROR;
    
    // make a tab table out of the file
    TabTable tab((char*)m.ptr());
    if (tab.status() != 0)
	return TCL_ERROR;

    // plot each row under the element name of the column heading
    int ncols = tab.numCols();
    if (ncols < 2)
	return error("expected at least 2 table columns to plot"); 
    // XXX how to interp 3 or more columns ?

    int nrows = tab.numRows();
    int nvalues = nrows*2;
    char** colNames = tab.colNames();
    double* xyvalues = new double[nvalues];
    int n;

    for (int row = 0; row < nrows; row++) {
	if (tab.get(row, 0, xyvalues[n=row*2]) != 0 
	    || tab.get(row, 1, xyvalues[++n]) != 0) {
	    delete xyvalues;
	    return TCL_ERROR;
	}
    }

    if (Blt_GraphElement(interp_, graph, elem, nvalues, xyvalues, 
			 argv[3], argv[4]) != TCL_OK) {
	delete xyvalues;
	return TCL_ERROR;
    }
    delete xyvalues;

    return set_result(nrows);
}


/*
 * shortname subcommand: 
 *
 * usage: $cat shortname ?name?
 *
 * returns the short_name field from the catalog config file
 * for the given long name, or for the current catalog.
 */
int TclAstroCat::shortnameCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (cat_) 
	    return set_result(cat_->shortName());
	return TCL_OK;
    }
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->shortName());
    return TCL_OK;
}


/*
 * Convert the given list from a Tcl list to a QueryResult (tab table),
 * given the number of columns and the column headings.
 *
 * Note: this is needed to make a clean interface between the Tcl and C++
 * classes. The Tcl part needs this because of the way that queries are
 * done in the background, so that the original results are lost when the
 * child process ends. That is why we have to convert back from Tcl list
 * to tab table here. It has the added advantage of making a flexible Tcl
 * command interface where you can specify catalog data as Tcl lists.
 *
 * Note that the data is assumed to have at least 3 columns: id, ra and
 * dec (in the given equinox) (or id, x and y, if the catalog doesn't support wcs). 
 *
 * Normally the first 3 columns are id, ra and dec, but we can't rely on
 * this, so we take the column info from the QueryResult object (the
 * r.entry() method should be called to set the catalog entry before calling
 * this method).
 */
int TclAstroCat::getQueryResult(int numCols, char** colNames, const char* list, 
				double equinox, QueryResult& r)
{
    // if (numCols < 3)
    // return error("error in tcl tab table: expected at least 3 columns: id, ra, dec");

    STRSTD::ostrstream os;
    int numRows = 0;
    char** rows = NULL;
    char raStr[32], decStr[32];

    int status = Tcl_SplitList(interp_, (char*)list, &numRows, &rows);
    if (status == TCL_OK) { 
	for (int row = 0; row < numRows; row++) {
	    int ncols = 0;
	    char** cols = NULL;
	    if (Tcl_SplitList(interp_, rows[row], &ncols, &cols) != TCL_OK
		|| ncols != numCols) {
		status = error("wrong number of columns: ", rows[row]);
		break;
	    }
	    int n = ncols-1;
	    if (r.isWcs()) {  // if catalog uses world coords...
		// convert ra,dec equinox to the catalog's equinox
		int ra_col = r.ra_col();
		int dec_col = r.dec_col();
		char* raPtr = cols[ra_col];
		char* decPtr = cols[dec_col]; 
		WorldCoords pos(raPtr, decPtr, equinox);  // converts to J2000 internally
		if (pos.status() != 0) {
		    status = TCL_ERROR;
		    break;
		}
		// print in catalog's equinox
		pos.print(raStr, decStr, r.equinox()); 

		// output the columns
		for (int col = 0; col < ncols; col++) {
		    if (col == ra_col)
			os << raStr; // ra and dec are formatted specially
		    else if (col == dec_col)
			os << decStr;
		    else
			os << cols[col]; 
		    if (col < n)
			os << '\t';
		}
	    } 
	    else {
		// output the columns
		for (int col = 0; col < ncols; col++) {
		    os << cols[col]; 
		    if (col < n)
			os << '\t';
		}
	    }
	    os << '\n';
	    free(cols);
	}
	os << ends;
    }

    // create a QueryResult object from the headings and data and 
    // save (or append) it to the file
    if (status == 0) 
	status = r.init(numCols, colNames, os.str());

    if (rows)
	free(rows);		// free split list of rows
    delete os.str();		// free generated tab table buffer

    return status;
}
