/*
 * E.S.O. - VLT project/Archive
 * $Id: SkySearch.C,v 1.2 2006/03/26 13:22:33 abrighto Exp $
 *
 * SkySearch.C - method definitions for class SkySearch
 *
 * This class adds Tcl subcommands to the "astrocat" command by
 * extending the TclAstroCat C++ class. Here we add support for
 * plotting objects in an image, done in C++ to improve performace
 * on large data sets.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  10 Feb 98  Created
 * Peter W. Draper 13 Jan 09  Change imgplotCmd to transform the catalogue
 *                            from the given equinox (presumably that of the
 *                            catalogue celestial coordinates) to the image
 *                            equinox before plotting.
 */
static const char* const rcsId="@(#) $Id: SkySearch.C,v 1.2 2006/03/26 13:22:33 abrighto Exp $";


#include <cstring>
#include <cctype>
#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <fstream>
#include <sstream>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "TabTable.h"
#include "Mem.h"
#include "error.h"
#include "util.h"
#include "Skycat.h"
#include "SkySearch.h"

/*
 * Extend the existing Tcl command "astrocat" by deriving a C++ subclass
 * of the class implementing it (this class).
 */
int SkySearch::astroCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName\"", NULL);
	return TCL_ERROR;
    }

    SkySearch* cmd = new SkySearch(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Call the given method in this class with the given arguments
 */
int SkySearch::call(const char* name, int len, int argc, char* argv[])
{
    if (strncmp(name, "imgplot", len) == 0) {
	return imgplotCmd(argc, argv);
    }
    return TclAstroCat::call(name, len, argc, argv);
}


/*
 * Parse the given symbol info and set the values of the last 7 args from
 * it. The symbol info is a variable length list of
 *  {shape color ratio angle label cond}
 * Any missing values are not set, so they should be initialized to a valid
 * default.
 */
int SkySearch::parse_symbol(const QueryResult& r, int argc, char** argv,
			    char*& shape, char*& fg, char*& bg, char*& ratio,
			    char*& angle, char*& label, char*& cond)
{
    static char* symbols[] = {
	"circle",
	"square",
	"plus",
	"cross",
	"triangle",
	"diamond",
	"ellipse",
	"compass",
	"line",
	"arrow"
    };
    static int nsymbols = sizeof(symbols)/sizeof(char*);
    int found = 0;

    if (argc < 1)
	return error("empty plot symbol");

    // symbol shape
    shape = argv[0];
    for (int i = 0; i < nsymbols; i++) {
	if (strcmp(shape, symbols[i]) == 0) {
	    found++;
	    break;
	}
    }
    if (!found)
	return error("invalid plot symbol");

    // color
    if (argc >= 2) {
	if (strlen(argv[1])) {
	    fg = bg = argv[1];
	}
    }

    // ratio
    if (argc >= 3) {
	if (strlen(argv[2])) {
	    ratio = argv[2];
	}
    }

    // angle
    if (argc >= 4) {
	if (strlen(argv[3])) {
	    angle = argv[3];
	}
    }

    // label
    if (argc >= 5) {
	if (strlen(argv[4])) {
	    label = argv[4];
	}
    }

    // cond
    if (argc >= 6) {
	if (strlen(argv[5])) {
	    cond = argv[5];
	}
    }

    return TCL_OK;
}


/*
 * Set Tcl variables with the names of the given column headings and the values
 * from the given query result row, so that the column heading names may be used
 * as variables in Tcl expressions.
 *
 * The values are taken from the given rownum of the query result r.
 * numCols is the number of columns used as variables.
 * colNames is an array of the column names used and
 * colIndexes are the integer column indexes for the column names in the query
 * result.
 */
int SkySearch::set_column_variables(const QueryResult& r, int rownum,
				    int numCols, char** colNames,
				    const int* colIndexes)
{
    for (int i = 0; i < numCols; i++) {
	char* value = NULL;
	if (r.get(rownum, colIndexes[i], value) != TCL_OK)
	    return TCL_ERROR;
	Tcl_SetVar(interp_, (char*)colNames[i], value, 0);
    }
    return TCL_OK;
}


/*
 * Plot a symbol in the given skycat image with the given shape, id,
 * catalog name, pos (x, y, xy_units), radius (in radius_units),
 * bg and fg colors, x/y size ratio, rotation angle and label.
 */
int SkySearch::plot_symbol(Skycat* image, const char* shape,
			   const char* id, int rownum,
			   double x, double y, const char* xy_units,
			   double radius, const char* radius_units,
			   const char* bg, const char* fg,
			   double ratio, double angle, const char* label)
{
    // get the list of tags to use for the symbol.
    // The list includes tags for identifying the symbol as "cat$id" or
    // all symbols for this instance by the instance name. The row number
    // is coded as row#$rownum. The general tag "objects" is also included.
    std::ostringstream symbol_os;
    symbol_os << "{cat" << id << "} "
	      << this->instname()
	      << ' ' << this->instname() << ".objects"
	      << " row#" << rownum
	      << " objects";

    // get the list of tags to use for the label
    // The list includes tags for identifying the label as "label$id" or
    // all labels for this instance by the instance name. The row number
    // is coded as row#$rownum. The general tag "objects" is also included.
    std::ostringstream label_os;
    if (label && strlen(label)) {
	label_os
	    << "{label" << id << "} "
	    << this->instname()
	    << ' ' << this->instname() << ".labels"
	    << " row#" << rownum
	    << " objects";
    }

    // draw the symbol and label
    return image->draw_symbol(shape, x, y, xy_units, radius, radius_units, bg, fg,
			      symbol_os.str().c_str(), ratio, angle, label, label_os.str().c_str());
}


/*
 * Plot the object for the given row in the given image.
 * The row data is taken from the QueryResult object r and plotted in the
 * given image.
 *
 * rownum is the row number.
 *
 * id is a unique id for the row.
 *
 * x and y are the coords of the center of the object in the given
 * units (pos_units).
 *
 * numCols is the number of column variables specified in the array colNames[], which
 * is a list of column names for columns whose values are used in the expressions.
 *
 * shape is the plot symbol (circle, ellipse, etc...)
 *
 * bg and fg are the background and foreground colors for drawing. If they are
 * different, 2 lines are drawn, otherwise 1.
 *
 * ratio and angle are optional arguments to the plot proc (for ellipse, plus).
 *
 * label is an expr to use to label the object.
 *
 * cond should evaluate to a boolean expr. If true, plot the object, otherwise not.
 *
 * size should be an expr and evaluate to the radius on the object in the
 * given units (size_units), using the column names as tcl variables.
 *
 * name is the short name of the catalog, used to identify all catalog objects
 * for this catalog.
 *
 */
int SkySearch::plot_row(Skycat* image, const QueryResult& r,
			int rownum, const char* id,
			double x, double y, const char* xy_units,
			int numCols, char** colNames, const int* colIndexes,
			const char* shape,
			const char* bg, const char* fg,
			const char* ratio, const char* angle,
			const char* label, const char* cond,
			const char* radius, const char* radius_units)
{
    // set local tcl variables for column values used
    if (set_column_variables(r, rownum, numCols, colNames, colIndexes) != TCL_OK)
	return TCL_ERROR;

    // eval expr to get condition
    int condVal = 1;
    if (strcmp(cond, "1") != 0) {
	if (Tcl_ExprBoolean(interp_, (char*)cond, &condVal) != TCL_OK)
	    return fmt_error("error in plot symbol condition: '%s': %s",
			     cond, interp_->result);
    }

    // if condition is not true, don't plot this row
    if (!condVal)
	return TCL_OK;

    // eval expr to get radius
    double radiusVal = 0;
    if (Tcl_ExprDouble(interp_, (char*)radius,  &radiusVal) != TCL_OK)
	return fmt_error("error in plot symbol expression: '%s': %s",
			 radius, interp_->result);
    if (radiusVal < 0.) {
	// don't want a neg radius
	radiusVal = 0.;
    }

    // ratio may be an expression with column name variables
    double ratioVal = 1;
    if (strcmp(ratio, "1") != 0) {
	if (Tcl_ExprDouble(interp_, (char*)ratio, &ratioVal) != TCL_OK)
	    return fmt_error("error in plot symbol ratio expression: '%s': %s",
			     ratio, interp_->result);
    }

    // angle may be an expression with column name variables
    double angleVal = 0;
    if (strcmp(angle, "0") != 0) {
	if (Tcl_ExprDouble(interp_, (char*)angle, &angleVal) != TCL_OK)
	    return fmt_error("error in plot symbol angle expression: '%s': %s",
			     angle, interp_->result);
    }

    // label may also contain col name vars, but might not be numeric
    char labelVal[256];
    labelVal[0] = '\0';
    if (label && strlen(label)) {
	char buf[1024];
        sprintf(buf, "subst %s", label);
	if (Tcl_Eval(interp_, buf) != TCL_OK)
	    return fmt_error("error in plot symbol label: '%s': %s",
			     label, interp_->result);
	if (strlen(interp_->result))
	    strncpy(labelVal, interp_->result, sizeof(labelVal)-1);
    }

    if (plot_symbol(image, shape, id, rownum, x, y, xy_units,
		    radiusVal, radius_units, bg, fg, ratioVal,
		    angleVal, labelVal) != TCL_OK)
	return TCL_ERROR;

    // XXX add bindings here XXX

    return TCL_OK;
}


/*
 * Plot the current objects (in the query result r) using the given plot
 * info (cols, symbol, expr) from the catalog config file "symbol" entry.
 *
 * cols should be a list of column names that may be used as Tcl
 * variables in "symbol" or "expr".
 *
 * "symbol" may be a simple symbol name, such as "circle" or a list such
 * as {ellipse color ratio angle label condition}.
 *
 * The last 4 elements in the symbol description may be expressions using
 * the column names given.  ratio gives the ratio of x/y, angle is the
 * rotation angle, label is an optional text expr and condition may
 * evaluate to 1 or 0 to indicate whether or not the symbol should be
 * displayed for an object.
 *
 * "expr" should be an expression that evaluates to the size of the radius
 * of the object in pixels, or a list {expr units} where units is one of
 * {image deg ...}  as supported by rtd.
 *
 */
int SkySearch::plot_objects(Skycat* image, const QueryResult& r,
			    const char* cols, const char* symbol,
			    const char* expr)
{
    int status = 0;
    int numCols = 0;
    char** colNames = NULL;
    int* colIndexes = NULL;
    int nsymb = 0;
    char** symb = NULL;
    int nexpr = 0;
    char** exprList = NULL;

    // this loop executes only once and is used for error handling/cleanup via "break"
    int once = 1;
    while (once-- > 0) {
	// check that plot columns are valid and also save the column indexes
	// for accessing row values as variables later
	if ((status = Tcl_SplitList(interp_, (char*)cols, &numCols, &colNames)) != TCL_OK)
	    break;
	colIndexes = new int[numCols];
	for (int i = 0; i < numCols; i++) {
	    if ((colIndexes[i] = r.colIndex(colNames[i])) < 0) {
		status = error("invalid plot column: ", colNames[i]);
		break;
	    }
	}

	// parse symbol info, a variable length list of
	// {shape color ratio angle label cond}
	if ((status = Tcl_SplitList(interp_, (char*)symbol, &nsymb, &symb)) != TCL_OK)
	    break;

	// default values
	char* shape = "";
	char* fg = "white"; // if no color is specified, use 2: b&w
	char* bg = "black";
	char* ratio = "1";  // these may be Tcl expressions
	char* angle = "0";
	char* label = "";
	char* cond = "1";
	if ((status = parse_symbol(r, nsymb, (char**)symb, shape, fg, bg, ratio,
				   angle, label, cond)) != TCL_OK)
	    break;

	// parse the size expr list: {size units}
	if ((status = Tcl_SplitList(interp_, (char*)expr, &nexpr, &exprList)) != TCL_OK)
	    break;
	if (nexpr == 0 || strlen(exprList[0]) == 0) {
	    status = error("invalid symbol expression: ", expr);
	    break;
	}
	char* size = (char*)exprList[0];
	char* units = "image";
	if (nexpr > 1 && strlen(exprList[1]))
	    units = (char*)exprList[1];

	// for each row in the catalog, eval the expressions and plot the symbols
	int nrows = r.numRows();
	int id_col = r.id_col();
	for (int rownum = 0; rownum < nrows; rownum++) {
	    char* id;
	    if ((status = r.get(rownum, id_col, id)) != 0)
		break;
	    WorldOrImageCoords pos;
	    if (r.getPos(rownum, pos) != 0)
		continue;	// coordinates might be missing - just ignore
	    double x, y;
	    char xy_units[32];
	    if (r.isPix()) {
		x = pos.x();
		y = pos.y();
		strcpy(xy_units, "image");
	    }
	    else if (r.isWcs()) {
		x = pos.ra_deg();
		y = pos.dec_deg();
		strcpy(xy_units, "deg");
	    }
	    else {
		status = error("no wcs or image coordinates to plot");
		break;
	    }
	    if ((status = plot_row(image, r, rownum, id, x, y, xy_units,
				   numCols, (char**)colNames, colIndexes, shape, bg, fg, ratio,
				   angle, label, cond, size, units)) != TCL_OK)
		break;
	}
    }

    // free memory allocated for split Tcl lists and return the status
    if (colNames)
	Tcl_Free((char *)colNames);
    if (colIndexes)
	delete colIndexes;
    if (symb)
	Tcl_Free((char *)symb);
    if (exprList)
	Tcl_Free((char *)exprList);

    return status;
}


/*
 * Plot the given query results in the given skycat image.
 */
int SkySearch::plot(Skycat* image, const QueryResult& r)
{
    if (!r.isWcs() && !r.isPix())
	return TCL_OK;		// no coordinate columns to plot

    if (!r.symbol() || strlen(r.symbol()) == 0)
	return TCL_OK;		// no symbols to plot

    // Get the list of symbols to plot
    // Note: r.symbol() returns a list of symbol descriptions, separated by ":".
    char* symbols = strdup(r.symbol());
    char* sym = symbols;
    char* p = NULL;

    // for each symbol to plot...
    int status = 0;
    int argc = 0;
    char** argv = NULL;
    do {
	p = strchr(sym, ':');
	if (p)
	    *p = '\0';
	// split each symbol entry into {cols symbol expr}
	// where "cols" is a list of column names to use as variables,
	// "symbol" is a list describing the symbol shape, color, ratio, angle,
	// and "expr" is a list describing the size and units of the symbol.
	status = Tcl_SplitList(interp_, sym, &argc, &argv);
	if (status != TCL_OK)
	    break;

	if (argc < 3) {
	    if (argc == 0)
		continue;
	    status = error("invalid symbol entry in config file: ", sym);
	    break;
	}

	// plot the objects for this symbol
	if ((status = plot_objects(image, r, argv[0], argv[1], argv[2])) != TCL_OK)
	    break;

	if (argv) {
	    Tcl_Free((char *)argv);
	    argv = NULL;
	}

	// move to next ':' separated symbol
	sym = p + 1;
    } while(p);

    if (argv)
	Tcl_Free((char *)argv);
    if (symbols)
	free(symbols);

    return status;

}


/*
 * imgplot subcommand:
 *
 * usage: $instName imgplot $image ?$data? ?$equinox? ?$headings?
 *
 * This subcommand is used to plot catalog objects on the skycat image
 * and was reimplemented here in C++ code to improve performance for
 * large complicated catalogs (note: the "plot" subcommand in the base
 * class is used to plot a BLT graph of the data, so we use "imgplot"
 * here).
 *
 * $image is the name of the image object ("rtdimage" object, implemented
 * by the RtdImage C++ class and extended by the Skycat C++ class).
 *
 * If $data is specified, it should be a Tcl list of rows to be plotted, in
 * the format returned by the query command.
 *
 * If $equinox is specified, it is the equinox of the image being displayed.
 * (The ra,dec columns in the table data are assumed to be J2000 unless
 * specified otherwise, with the equinox keyword in the table config entry).
 *
 * If $headings is given, it is used as a Tcl list of column headings.
 * Otherwise the catalog headings are used, if there is a current
 * catalog.
 *
 * Note: normally you will need to specify all the arguments, since the
 * querries are done in the background (See cat/cat::AstroCat and
 * util::Batch). The information for the previous query is lost when the
 * background process exits. This might change if querries were done
 * using threads or if the background/interrupt handling were done in the
 * C++ code rather than in the Tcl code, as it is done now.
 */
int SkySearch::imgplotCmd(int argc, char* argv[])
{
    // can't plot without a catalog, since no plot symbols would be defined
    if (!cat_)
	return error("no catalog is currently open");

    if (argc <= 0 || argc > 4)
	return error("wrong number of args for astrocat imgplot subcommand");

    // Get a pointer to the C++ class implementing the extended rtdimage object.
    // We will need this to access the image to plot the catalog symbols.
    Skycat* image = Skycat::getInstance(argv[0]);
    if (! image)
	return TCL_ERROR;

    if (argc == 1) {
	if (!result_)
	    return error("no previous data to plot");

	return plot(image, *result_);
    }

    char* equinoxStr = NULL;
    int numCols = 0;;
    char** colNames = NULL;
    int freeColNames = 0;

    // get the equinox, if specified
    if (argc >= 3) {
	equinoxStr = argv[2];
    }

    // get the column names
    if (argc < 4) {		// use current catalog
	numCols = cat_->numCols();
	colNames = cat_->colNames();
    }
    else {			// use headings list
	if (Tcl_SplitList(interp_, argv[3], &numCols, &colNames) != TCL_OK)
	    return TCL_ERROR;
	freeColNames++;		// delete memory later
    }

    // get query results from arguments
    // PWD: set equinox of result to that of image, so catalog will be transformed.
    QueryResult r;
    r.entry(cat_->entry());
    double equinox = 2000.0;
    if ( image->isWcs() ) {
        equinox = r.equinox();        //  Record catalog equinox.
        r.getInfo()->equinox( image->image()->wcs().equinox() );
    }
    int status = getQueryResult(numCols, (char**)colNames, argv[1], equinoxStr, r);
    if (status == TCL_OK)
	status = plot(image, r);

    // catalog equinox back to original
    if ( image->isWcs() ) {
        r.getInfo()->equinox( equinox );
    }

    // clean up
    if (freeColNames && colNames)
	Tcl_Free((char *)colNames);	// free split list of column headings

    return status;
}

