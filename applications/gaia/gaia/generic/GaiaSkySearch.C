/**
 *  Name:
 *     GaiaSkySearch

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the GaiaSkySearch class.

 *  Description:
 *     This class extends the SkySearch class (and thereby TclAstroCat) to use
 *     an external filter scheme to convert catalogues into "tab table"
 *     format. Keeping the names consistent and disposing of the intermediary
 *     files etc. These foreign catalogues are controlled by the
 *     GaiaLocalCatalog class.
 *
 *     Methods related to plotting overlay are also overridden to
 *     resolve problems with catalogues that have both WCS and X-Y
 *     coordinates and to align catalogues to images using the facilities
 *     of AST.
 *
 *     A "namesvr" command is provided so that coordinates can be
 *     retrieved for objects, without doing a full catalogue or image query.

 *  Authors:
 *     PWD: P.W. Draper (JAC, Durham University)

 *  Copyright:
 *     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008-2009 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  History:
 *     25-SEP-1998 (PWD):
 *        Original version.
 *     21-AUG-2000 (PWD):
 *        Added changes to support NDF origins.
 *     22-AUG-2000 (PWD):
 *        Override "info" command to stop foreign files from being
 *        ignored (all files in /tmp not saved).
 *     {enter_new_authors_here}
 *
 *-
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <iostream>
#include <sstream>

extern "C" {
#include "ast.h"
}

#include "error.h"
#include "util.h"
#include "AstroCatalog.h"
#include "LocalCatalog.h"
#include "GaiaLocalCatalog.h"
#include "Skycat.h"
#include "SkySearch.h"
#include "GaiaSkySearch.h"
#include "StarWCS.h"

//
// Declare a table of tcl subcommands.
//
// NOTE: keep this table sorted, so we can use a binary search on it !
// (select lines in emacs and use M-x sort-lines)
//
static class GaiaSkySearchSubCmds {
public:
    const char* name;      // method name
    int (GaiaSkySearch::*fptr)(int argc, char* argv[]);
    int min_args;          // minimum number of args
    int max_args;          // maximum number of args
} subcmds_[] = {
    {"check",      &GaiaSkySearch::checkCmd,        1,  1},
    {"content",    &GaiaSkySearch::contentCmd,      0,  0},
    {"csize",      &GaiaSkySearch::csizeCmd,        1,  1},
    {"entry",      &GaiaSkySearch::entryCmd,        1,  4},
    {"imgplot",    &GaiaSkySearch::imgplotCmd,      1,  4},
    {"info",       &GaiaSkySearch::infoCmd,         1,  2},
    {"namesvr",    &GaiaSkySearch::namesvrCmd,      2,  2},
    {"open",       &GaiaSkySearch::openCmd,         1,  2},
    {"origin",     &GaiaSkySearch::originCmd,       0,  2},
    {"save",       &GaiaSkySearch::saveCmd,         1,  5}
};

/**
 *  Call the given method in this class with the given arguments
 */
int GaiaSkySearch::call(const char* name, int len, int argc, char* argv[])
{
    //  Since this tcl command may have a lot of subcommands, we do a
    //  binary search on the method table.
    int low = 0,
        high = sizeof(subcmds_)/sizeof(*subcmds_) - 1,
        mid,
        cond;

    while ( low <= high ) {
        mid = (low + high) / 2;
        if ( ( cond = strcmp( name, subcmds_[mid].name ) ) < 0 ) {
            high = mid - 1;
        }
        else if ( cond > 0 ) {
            low = mid + 1;
        }
        else {
            GaiaSkySearchSubCmds& t = subcmds_[mid];
            if ( check_args( name, argc, t.min_args, t.max_args ) != TCL_OK ) {
                return TCL_ERROR;
            }
            return (this->*t.fptr)( argc, argv );
        }
    }

    //  Not found, so extend search to parent class.
    return SkySearch::call(name, len, argc, argv);
}

//
//  A call to this function can be made from the tkAppInit file at
//  startup to install the Tcl extended "astrocat" command.
//
extern "C" {
    int GaiaCat_Init(Tcl_Interp* interp)
    {
        // Install the astrocat command.
        Tcl_CreateCommand(interp, "astrocat", GaiaSkySearch::astroCatCmd, NULL, NULL);
        return TCL_OK;
    }
}

/**
 *  Implementation of the tcl extended command "astrocat". This creates
 *  a new instance of the astrocat command.
 *
 */
int GaiaSkySearch::astroCatCmd( ClientData, Tcl_Interp *interp,
                                int argc, char* argv[] )
{
    if (argc != 2) {
        Tcl_AppendResult( interp, "wrong # args:  should be \"",
                          argv[0], " instanceName\"", NULL );
        return TCL_ERROR;
    }

    GaiaSkySearch *cmd = new GaiaSkySearch( interp, argv[0], argv[1] );
    return cmd->status();
}

/**
 *  Constructor -
 *
 *  Create an astrocat object in tcl for accessing the contents of
 *  local and remote catalogues, including CAT catalogues.
 *
 *  Note that the tcl command for this object is created in the
 *  parent class constructor.
 */
GaiaSkySearch::GaiaSkySearch( Tcl_Interp *interp,
                              const char *cmdname,
                              const char *instname )
    : TclAstroCat( interp, cmdname, instname ), //  Needed as SkySearch
                                                //  inherits TclAstroCat
                                                // as "public virtual"!
      SkySearch( interp, cmdname, instname ),
      xOrigin_(0.0),
      yOrigin_(0.0)
{
    // Do nothing.
}

/**
 *  Destructor -
 */
GaiaSkySearch::~GaiaSkySearch()
{
    // Do nothing.
}

/**
 *  Open the given astronomical catalogue.
 */
int GaiaSkySearch::openCmd(int argc, char* argv[])
{
    if ( cat_ ) {
        delete cat_;
        cat_ = NULL;
    }

    CatalogInfoEntry *e = NULL;
    if ( argc == 2 && ( strlen(argv[1]) != 0 ) ) {

        // Open given catalog in given directory path.
        e = lookupCatalogDirectoryEntry( argv[1] );
        if ( !e ) {
            return TCL_ERROR;
        }
        e = CatalogInfo::lookup( e, argv[0] );
        if ( !e ) {
            return fmt_error( "catalog entry for '%s' not found under '%s' ",
                              argv[0], argv[1]);
        }
    }
    else {

        //  Get the entry for this catalogue type.
        e = CatalogInfo::lookup( argv[0] );
        if ( !e ) {
            return TCL_ERROR;
        }
    }

    //  Now open the catalog, using the appropriate type.
    if ( strcmp( e->servType(), "local" ) == 0 ) {

        //  Local catalogues are either tab tables or a format
        //  recognised by GaiaLocalCatalog.
        if ( GaiaLocalCatalog::is_foreign( argv[0] ) ) {
            cat_ = new GaiaLocalCatalog( e, interp_ );
        }
        else {
            cat_ = new LocalCatalog( e );
        }
    }
    else {
        cat_ = new AstroCatalog( e );   //  Class for remote catalogues.
    }

    if ( !cat_ || cat_->status() != 0 ) {
        if ( cat_->status() != 0 ) delete cat_;
        cat_ = NULL;
        return error( "Failed to open catalog:", argv[0] );
    }

    //  Set up feedback, if requested.
    if ( feedback_ ) {
        cat_->feedback( feedback_ );
    }

    //  Reset origins.
    xOrigin_ = 0.0;
    yOrigin_ = 0.0;

    return TCL_OK;
}

/**
 *  Check that the given filename is a valid local catalog.
 */
int GaiaSkySearch::checkCmd(int argc, char* argv[])
{
    if ( GaiaLocalCatalog::is_foreign( argv[0] ) ) {
        return GaiaLocalCatalog::check_table( argv[0] );
    }
    else {
        return LocalCatalog::check_table( argv[0] );
    }
}

/**
 *  Override the "entry" command to make sure url and longname are
 *  the same for foreign catalogues when written into the skycat.cfg
 *  file.
 */
int GaiaSkySearch::entryCmd( int argc, char* argv[] )
{
    CatalogInfoEntry* e;
    CatalogInfoEntry* dir = CatalogInfo::root();
    char *url = NULL;
    if ( !dir ) {
        return TCL_ERROR;
    }

    //  Trap "get" command and modify url so that it appears the same as
    //  longname for foreign catalogues.
    if ( strcmp( argv[0], "get" ) == 0 ) {
        if ( argc == 1 ) {

            //  Get entry from current catalog.
            if ( !cat_ ) {
                return error( "no catalog is open" );
            }
            e = cat_->entry();
        }
        else {
            if ( argc > 2 ) {

                //  Use given catalog directory.
                dir = lookupCatalogDirectoryEntry( argv[2] );
                if ( !dir ) {
                    return TCL_ERROR;
                }
            }

            //  Get entry from named catalog.
            e = CatalogInfo::lookup( dir, argv[1] );
            if ( !e ) {
                return error( "can't find catalog entry for: ", argv[1] );
            }
        }

        //  Reset the url to be longname and pass on to TclAstroCat command.
        url = strdup( e->url() );
        if ( strcmp( e->servType(), "local" ) == 0 ) {
            e->url( e->longName() );
        }
    }
    int ret = TclAstroCat::entryCmd( argc, argv );

    //  Restore dynamic url, if needed.
    if ( url != NULL ) {
        e->url( url );
        free( url );

        //  If "get" then override report of various columns so that
        //  default configuration is reported (GAIA is more fussy
        //  about having X and Y columns -v- RA/Dec, so needs to know this).
        if ( e->id_col() == 0 ) {
            appendKeyVal( "id_col", "0" );
        }
        if ( e->ra_col() == 1 ) {
            appendKeyVal( "ra_col", "1" );
        }
        if ( e->dec_col() == 2 ) {
            appendKeyVal( "dec_col", "2" );
        }
        if ( e->x_col() == -1 ) {
            appendKeyVal( "x_col", "-1" );
        }
        if ( e->y_col() == -1 ) {
            appendKeyVal( "y_col", "-1" );
        }
    }
    return ret;
}

/**
 *  Save command. Override so we can get foreign catalogues to
 *  do their conversion correctly.
 */
int GaiaSkySearch::saveCmd( int argc, char* argv[] )
{

    //  See if this is an attempt to add, or just create.
    int iflag = 0;
    if (argc >= 2) {
        if (Tcl_GetBoolean(interp_, argv[1], &iflag) != TCL_OK)
            return TCL_ERROR;
    }

    //  Check if destination catalogue is a known foreign type.
    char *filename = NULL;
    int ret = TCL_OK;
    AstroCatalog *tmp = NULL;
    if ( GaiaLocalCatalog::is_foreign( argv[0] ) ) {

        //  If appending then the temporary file should already exist, so
        //  save the contents first. To do this we need to convert the
        //  file, if it hasn't been converted already.
        filename = argv[0];
        if ( iflag ) {
            CatalogInfoEntry *e = CatalogInfo::lookup( argv[0] );
            if ( ! e ) {
                return TCL_ERROR;
            }
            //  May be an existing file, check for existence of URL, plus it
            //  cannot be itself.
            struct stat buf;
            if ( stat( e->url(), &buf ) != 0 ||
                 strcmp( e->url(), e->longName() ) == 0) {

                //  Temporary file for this file doesn't exist, so we need to
                //  create it.
                tmp = new GaiaLocalCatalog( e, interp_ );
            }
            argv[0] = (char *) e->url();

        }
        else {

            //  Create a temporary name to store the results.
            argv[0] = tempnam( (char *) NULL, "gaia" );
        }
        argv[0] = strdup( argv[0] );
    }

    //  Now do the real save.
    ret = TclAstroCat::saveCmd( argc, argv );
    if ( filename != NULL && ret == TCL_OK ) {

        //  Result stored in temporary file. Need to convert to
        //  appropriate foreign format. Create a pseudo CatalogInfoEntry
        //  so we can use a proper object to control the conversion.
        CatalogInfoEntry f;
        f.servType( "local" );
        f.longName( filename );
        f.shortName( filename );
        f.url( filename );
        ret = GaiaLocalCatalog::save( &f, interp_, argv[0], filename );
        free( argv[0] );
        argv[0] = filename;
    }
    return ret;
}

/**
 *  Determine the maximum size of character strings needed to store
 *  the columns of a tab-table. The tab-table is passed as a Tcl list
 *  of lists and the return is a list of the number of characters
 *  needed. This is implemented in C++ for speed reasons only.
 */
int GaiaSkySearch::csizeCmd( int argc, char *argv[] )
{
    char **mainArgv;
    int mainArgc = 0;
    char **rowArgv;
    int rowArgc = 0;
    int *sizes = NULL;
    int ncolumn = 0;
    int len = 0;
    int i;
    int j;

    //  Split the list into a list of rows.
    if ( Tcl_SplitList( interp_, argv[0], &mainArgc, &mainArgv ) != TCL_OK ) {
        return error( "not a list" );
    }
    for ( i = 0; i < mainArgc; i++ ) {
        if ( Tcl_SplitList( interp_, mainArgv[i], &rowArgc, &rowArgv ) != TCL_OK ) {
            if ( mainArgc > 0 ) {
                Tcl_Free( (char *) mainArgv );
            }
            return error( "not a sub list" );
        }
        else {
            //  Allocate memory for columns sizes.
            if ( ncolumn == 0  ) {
                sizes = new int[rowArgc];
                ncolumn = rowArgc;
                for ( j = 0; j < ncolumn; j++ ) {
                    sizes[j] = 0;
                }
            }

            //  Check list if same length as before.
            if ( rowArgc != ncolumn ) {
                if ( mainArgc > 0 ) {
                    Tcl_Free( (char *) mainArgv );
                }
                if ( rowArgc > 0 ) {
                    Tcl_Free( (char *) rowArgv );
                }
                return error( "lists are different lengths" );
            }

            //  Compare lengths of these strings with previous ones.
            for ( j = 0; j < ncolumn; j++ ) {
                len = strlen( rowArgv[j] );
                if ( len > sizes[j] ) {
                    sizes[j] = len;
                }
            }

            //  Free the memory.
            if ( rowArgc > 0 ) {
                Tcl_Free( (char *) rowArgv );
            }
        }
    }

    //  Now construct the list to return.
    if ( ncolumn > 0 ) {
        ostringstream os;
        for ( i = 0; i < ncolumn; i++ ) {
            os << sizes[i] << " ";
        }
        set_result( os.str().c_str() );
        delete [] sizes;
    }
    if ( mainArgc > 0 ) {
        Tcl_Free( (char *) mainArgv );
    }
    return TCL_OK;
}


/**
 *  Override plot_objects to sort out problems with plotting when
 *  have both pixel coordinates and sky coordinates. Also now we
 *  are using AST transformations we need to make sure that further
 *  transformations are not applied (the equinox is now the same as the
 *  image).
 */
int GaiaSkySearch::plot_objects( Skycat* image, const QueryResult& r,
                                 const char* cols, const char* symbol,
                                 const char* expr )
{
    int status = 0;
    int numCols = 0;
    char** colNames = NULL;
    int* colIndexes = NULL;
    int nsymb = 0;
    char** symb = NULL;
    int nexpr = 0;
    char** exprList = NULL;

    //  This loop executes only once and is used for error
    //  handling/cleanup via "break".
    int once = 1;
    while (once-- > 0) {
        //  Check that plot columns are valid and also save the column
        //  indexes for accessing row values as variables later.
        if ((status = Tcl_SplitList(interp_, (char*)cols,
                                    &numCols, &colNames)) != TCL_OK) {
            break;
        }
        colIndexes = new int[numCols];
        for (int i = 0; i < numCols; i++) {
            if ( (colIndexes[i] = r.colIndex(colNames[i])) < 0 ) {
                status = error("unrecognised column in plot expression: ", colNames[i]);
                break;
            }
        }
        if ( status != TCL_OK ) {
            break;
        }

        //  Parse symbol info, a variable length list of
        //  {shape color ratio angle label cond}
        if ((status = Tcl_SplitList(interp_, (char*)symbol, &nsymb, &symb)) != TCL_OK)
            break;

        //  Default values
        char* shape = "";
        char* fg = "white"; // if no color is specified, use 2: b&w
        char* bg = "black";
        char* ratio = "1";  // these may be Tcl expressions
        char* angle = "0";
        char* label = "";
        char* cond = "1";
        if ((status = parse_symbol(r, nsymb, symb, shape, fg, bg, ratio,
                                   angle, label, cond)) != TCL_OK)
            break;

        // parse the size expr list: {size units}
        if ((status = Tcl_SplitList(interp_, (char*)expr, &nexpr, &exprList)) != TCL_OK)
            break;
        if (nexpr == 0 || strlen(exprList[0]) == 0) {
            status = error("invalid symbol expression: ", expr);
            break;
        }
        char* size = exprList[0];
        char* units = "image";
        if (nexpr > 1 && strlen(exprList[1]))
            units = exprList[1];

        // for each row in the catalog, eval the expressions and plot the symbols
        int nrows = r.numRows();
        int id_col = r.id_col();
        for (int rownum = 0; rownum < nrows; rownum++) {
            char* id;
            if ((status = r.get(rownum, id_col, id)) != 0)
                break;
            WorldOrImageCoords pos;
            if ((status = r.getPos(rownum, pos)) != 0)
                break;
            double x, y;
            char xy_units[32];
            if (r.isPix() && ! r.isWcs() ) { // PWD: modify here
                x = pos.x();
                y = pos.y();
                strcpy(xy_units, "image");

                //  Subtract the origins.
                x -= xOrigin_;
                y -= yOrigin_;
            }
            else if (r.isWcs()) {
                x = pos.ra_deg();
                y = pos.dec_deg();
                
                //  Degrees in the same units as image, so same equinox
                //  in skycat speak.
                const char *equinox = image->image()->wcs().equinoxStr();
                strcpy(xy_units, "deg ");
                strcat(xy_units, equinox);
            }
            else {
                status = error("no wcs or image coordinates to plot");
                break;
            }
            if ((status = plot_row(image, r, rownum, id, x, y, xy_units,
                                   numCols, colNames, colIndexes, shape, bg, fg, ratio,
                                   angle, label, cond, size, units)) != TCL_OK)
                break;
        }
    }

    // free memory allocated for split Tcl lists and return the status
    if (colNames)
        ckfree((char *)colNames);
    if (colIndexes)
        delete[] colIndexes;
    if (symb)
        ckfree((char *)symb);
    if (exprList)
        ckfree((char *)exprList);

    return status;
}


/*
 * Parse the given symbol info and set the values of the last 7 args from
 * it. Overridden to support GAIA symbols. Note it's a copy because the types
 * are hardcoded.
 */
int GaiaSkySearch::parse_symbol( const QueryResult& r, int argc, char** argv,
                                 char*& shape, char*& fg, char*& bg,
                                 char*& ratio, char*& angle, char*& label,
                                 char*& cond )
{
    static const char* symbols[] = {
        "arrow",
        "arc",
        "circle",
        "compass",
        "cross",
        "diamond",
        "ellipse",
        "line",
        "plus",
        "square",
        "triangle",
        "rotbox",
        "xrange",
        "yrange"
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

/**
 *  Set or get the values used to offset image coordinates when
 *  plotted.  These are the NDF origin values and are used to
 *  transform between image coordinate and pixel coordinates.
 *
 *  Arguments are none, in which case the current origins are returned
 *  as the result, or two doubles, which are added to any image
 *  coordinates before plotting (note if any 0.5 corrections are
 *  required then you must add these here, the origin supplied are
 *  subtracted from the X and Y coordinates).
 */
int GaiaSkySearch::originCmd( int argc, char *argv[] )
{
    if ( argc < 2 ) {

        // Return the current origin.
        Tcl_ResetResult( interp_ );
        char buf[80];
        sprintf( buf, "%f %f", xOrigin_, yOrigin_ );
        set_result( buf );
    }
    else {
        double xo;
        double yo;
        if ( Tcl_GetDouble( interp_, argv[0], &xo ) != TCL_OK ) {
            return error( argv[0], " is not a floating point value" );
        }
        else {
            if ( Tcl_GetDouble( interp_, argv[1], &yo ) != TCL_OK ) {
                return error( argv[1], " is not a floating point value" );
            }
        }
        xOrigin_ = xo;
        yOrigin_ = yo;
    }
    return TCL_OK;
}

/**
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
 *
 * This method overrides the TclAstroCat version to make sure that our
 * temporary files are not excluded from the configuration file.
 */
int GaiaSkySearch::infoCmd(int argc, char* argv[])
{
    Tcl_ResetResult(interp_);

    CatalogInfoEntry* e;
    if (argc == 2) {
        e = lookupCatalogDirectoryEntry(argv[1]);
        if (!e)
            return TCL_ERROR;
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
            // ignore local catalogs in /tmp, since they will be
            // deleted later, unless they are foreign catalogues, that
            // are not stored in a FITS extension!
            int save = 1;
            if ( strcmp(e->servType(), "local") == 0 ) {
                if ( strncmp(e->url(), "/tmp/", 5) == 0 ) {
                    if ( GaiaLocalCatalog::is_foreign( e->longName() ) ) {
                        if ( strstr( e->longName(), "}" ) ) {
                            save = 0;
                        }
                    }
                    else {
                        save = 0;
                    }
                }
            }
            if ( save ) {
                Tcl_AppendElement(interp_, (char*)e->longName());
            }
        }
    }
    return TCL_OK;
}

/**
 *  Get the content of the current query (whole catalogue if local, or
 *  last query if remote) as a Tcl list.
 */
int GaiaSkySearch::contentCmd( int argc, char *argv[] )
{
    if ( !cat_ ) {
        return error( "no catalog is currently open" );
    }

    //  If a local catalogue use whole.
    LocalCatalog *lc = dynamic_cast<LocalCatalog *>( cat_ );
    QueryResult *qr;
    if ( ! lc ) {
        if ( ! result_ ) {
            return error( "no query is available" );
        }
        qr = result_;
    }
    else {
        qr = &lc->getQuery();
    }

    int ncols = qr->numCols();
    int nrows = qr->numRows();
    char* s;

    Tcl_ResetResult( interp_ );
    if ( cat_->isWcs() ) {
        WorldCoords pos;
        char dec_buf[32];
        char ra_buf[32];
        int dec_col = qr->dec_col();
        int ra_col = qr->ra_col();

        for ( int i = 0; i < nrows; i++ ) {

            // Row includes formatted RA & Dec, do that first.
            if ( qr->getPos( i, pos ) != 0 ) {
                return error( "Failed getting world coordinates" );
            }
            pos.print( ra_buf, dec_buf, equinoxStr_ );

            //  Put the column values in a list.
            Tcl_AppendResult( interp_, " {", NULL );
            for ( int j = 0; j < ncols; j++ ) {
                if ( j == ra_col ) {
                    Tcl_AppendElement( interp_, ra_buf );
                }
                else if ( j == dec_col ) {
                    Tcl_AppendElement( interp_, dec_buf );
                }
                else {
                    if ( qr->get( i, j, s ) == 0 ) {
                        Tcl_AppendElement( interp_, s );
                    }
                    else {
                        Tcl_AppendElement( interp_, "" );
                    }
                }
            }
            Tcl_AppendResult(interp_, "}", NULL);
        }
    }
    else {
        // Image coords or no coords - no special formatting needed.
        for ( int i = 0; i < nrows; i++ ) {
            Tcl_AppendResult( interp_, " {", NULL );
            for ( int j = 0; j < ncols; j++) {
                if ( qr->get( i, j, s ) == 0 ) {
                    Tcl_AppendElement( interp_, s );
                }
                else {
                    Tcl_AppendElement( interp_, "" );
                }
            }
            Tcl_AppendResult(interp_, "}", NULL);
        }
    }
    return TCL_OK;
}

/**
 *  Return the RA and Dec for a named object using the given nameserver.
 */
int GaiaSkySearch::namesvrCmd( int argc, char *argv[] )
{
    AstroCatalog* cat = AstroCatalog::open( argv[0] );

    if ( cat == NULL ) {
        return TCL_ERROR;
    }

    QueryResult result;
    if ( cat->getObject( argv[1], 0, NULL, result ) ) {
        delete cat;
        return TCL_ERROR;
    }

    WorldCoords pos;
    if ( result.getPos( 0, pos ) != 0 ) {
        delete cat;
        return TCL_ERROR;
    }

    char ra_buf[32];
    char dec_buf[32];
    pos.print( ra_buf, dec_buf, cat->equinox() );

    char buf[80];
    sprintf( buf, "%s %s", ra_buf, dec_buf );

    Tcl_SetResult( interp_, buf, NULL );
    delete cat;
    return TCL_OK;
}

/*
 * Convert the given list from a Tcl list to a QueryResult (tab table),
 * given the number of columns and the column headings.
 *
 * See TclAstroCat::getQueryResult for further description.
 *
 * Overridden so that we can apply a coordinate transformation from the
 * catalog WCS to that of the image using an AstFrameSet (Skycat version uses
 * an equinox) as the mapping. Note the mapping must transform from image
 * world coordinates to catalogue world coordinates (usually both SkyFrames)
 * and must define an inverse mapping.
 */
int GaiaSkySearch::getQueryResult( int numCols, char **colNames, 
                                   const char *list, AstFrameSet *frmset,
                                   QueryResult &r )
{
    cout << "Using GaiaSkySearch::getQueryResult" << endl;

    ostringstream os;
    int numRows = 0;
    char **rows = NULL;
    char raStr[32], decStr[32];

    int status = Tcl_SplitList( interp_, (char*)list, &numRows, &rows );
    if ( status == TCL_OK ) {

        //  If catalog has world coordinates identified we need to transform
        //  into the world coordinates of the image. That is defined by the
        //  inverse transformation of the given FrameSet, so that must also be
        //  defined.
        if ( r.isWcs() && frmset != NULL ) {

            //  Locate the RA and Dec axes in the given FrameSet (the current
            //  coordinate system must be a SkyFrame and should represent the
            //  catalogue coordinates).
            if ( !astOK ) {
                astClearStatus;
            }
            int base = astGetI( frmset, "Base" );
            int current = astGetI( frmset, "Current" );

            //  Base frame.
            astSetI( frmset, "Current", base );
            int astime2 = astGetI( frmset, "astime(2)" );
            int ra1_index = 1;
            if ( astime2 ) {
                ra1_index  = 2;
            }

            //  Current frame.
            astSetI( frmset, "Current", current );
            astime2 = astGetI( frmset, "astime(2)" );
            int ra2_index = 1;
            int dec2_index = 2;
            if ( astime2 ) {
                ra2_index  = 2;
                dec2_index = 1;
            }

            for ( int row = 0; row < numRows; row++ ) {

                //  Always try again.
                if ( ! astOK ) {
                    astClearStatus;
                }

                int ncols = 0;
                char** cols = NULL;
                if ( Tcl_SplitList(interp_, rows[row], &ncols, &cols) != TCL_OK
                     || ncols != numCols ) {
                    status = error( "wrong number of columns: ", rows[row] );
                    break;
                }
                int n = ncols - 1;
                int ra_col = r.ra_col();
                int dec_col = r.dec_col();
                const char *raPtr = cols[ra_col];
                const char *decPtr = cols[dec_col];

                //  Convert values to double precision.
                double xin[1];
                double yin[1];
                double xout[1];
                double yout[1];
                astUnformat( frmset, ra2_index, raPtr, &xin[0] );
                astUnformat( frmset, dec2_index, decPtr, &yin[0] );

                //  RA and Dec could be swapped.
                if ( ra2_index == 1 ) {
                    astTran2( frmset, 1, xin, yin, 0, xout, yout );
                }
                else {
                    astTran2( frmset, 1, yin, xin, 0, xout, yout );
                }
                if ( ! astOK ) {
                    astClearStatus;
                    raStr[0] = decStr[0] = '\0';
                }
                else {
                    //  Format the values, in the base frame.
                    if ( ra1_index == 1 ) {
                        strcpy( raStr, astFormat( frmset, 1, xout[0] ) );
                        strcpy( decStr, astFormat( frmset, 2, yout[0] ) );
                    }
                    else {
                        strcpy( decStr, astFormat( frmset, 1, xout[0] ) );
                        strcpy( raStr, astFormat( frmset, 2, yout[0] ) );
                    }
                }

                //  Output the columns
                for ( int col = 0; col < ncols; col++ ) {
                    //  RA and Dec are formatted already.
                    if ( col == ra_col ) {
                        os << raStr;
                    }
                    else if ( col == dec_col ) {
                        os << decStr;
                    }
                    else {
                        os << cols[col];
                    }
                    if ( col < n ) {
                        os << '\t';
                    }
                }
                os << '\n';
                Tcl_Free( (char *)cols );
            }
        }
        else {
            //  No WCS so just output the columns
            for ( int row = 0; row < numRows; row++ ) {
                int ncols = 0;
                char** cols = NULL;
                if ( Tcl_SplitList(interp_, rows[row], &ncols, &cols) != TCL_OK
                     || ncols != numCols ) {
                    status = error( "wrong number of columns: ", rows[row] );
                    break;
                }
                int n = ncols - 1;
                for ( int col = 0; col < ncols; col++ ) {
                    os << cols[col];
                    if ( col < n ) {
                        os << '\t';
                    }
                }
                os << '\n';
                Tcl_Free( (char *)cols );
            }
        }
    }

    //  Create a QueryResult object from the headings and data and
    //  save (or append) it to the file
    if ( status == TCL_OK ) {
        status = r.init( numCols, colNames, os.str().c_str() );
    }

    if ( rows ) {
        Tcl_Free( (char *)rows );
    }

    return status;
}


/*
 *  imgplot subcommand:
 *
 *  usage: $instName imgplot $image ?$data? ?$equinox||AST_reference? ?$headings?
 *
 *  See SkySearch::imgplot description.
 *
 *  Overridden to use AST to transform the catalogue positions onto the image.
 *  The catalogue WCS must be passed as a reference to an AST FrameSet. See
 *  gaiautils:: to create on of these.
 * 
 */
int GaiaSkySearch::imgplotCmd( int argc, char* argv[] )
{
    cout << "GaiaSkySearch::imgplotCmd" << endl;

    //  Can't plot without a catalog, since no plot symbols would be defined.
    if ( !cat_ ) {
	return error( "no catalog is currently open" );
    }

    //  Get a pointer to the C++ class implementing the extended rtdimage object.
    //  We will need this to access the image to plot the catalog symbols.
    Skycat *image = Skycat::getInstance( argv[0] );
    if ( ! image ) {
	return TCL_ERROR;
    }

    if ( argc == 1 ) {
	if ( !result_ ) {
	    return error( "no previous data to plot" );
        }

        //  Plot data from last QueryResult.
	return plot( image, *result_ );
    }

    AstFrameSet *frmset = NULL;
    int numCols = 0;;
    char **colNames = NULL;
    int freeColNames = 0;

    //  Get an AST reference to a FrameSet describing the catalogue
    //  coordinates, but only if catalogue and image both support sky
    //  coordinates.
    if ( argc >= 3 && cat_->isWcs() && image->isWcs() ) {
        long adr;
        if ( Tcl_ExprLong( interp_, argv[2], &adr ) != TCL_OK ) {
            return TCL_ERROR;
        }
        if ( adr != 0 ) {
            AstFrameSet *catwcs = (AstFrameSet *) adr;

            //  Connect this to the coordinates of the image. Note this is now
            //  assumes we have a StarRtdImage instance not Skycat.
            StarWCS *wcs = (StarWCS *) image->image()->wcs().rep();
            AstFrameSet *imagewcs = (AstFrameSet *) wcs->astWCSCopy();
            frmset = (AstFrameSet *) astConvert( imagewcs, catwcs, "SKY" );
            if ( ! astOK ) {
                astClearStatus;
                return error( "Failed to connect image and catalogue"
                              " coordinates" );
            }
        }
    }

    //  Get the column names
    if ( argc < 4 ) {		
        //  Use current catalogue.
	numCols = cat_->numCols();
	colNames = cat_->colNames();
    }
    else {			
        //  Use headings list.
	if ( Tcl_SplitList(interp_, argv[3], &numCols, &colNames) != TCL_OK ) {
            return TCL_ERROR;
        }
	freeColNames++;
    }

    //  Get query results from arguments
    QueryResult r;
    r.entry( cat_->entry() );
    int status = getQueryResult( numCols, (char**)colNames, argv[1], 
                                 frmset, r );
    if ( status == TCL_OK ) {
	status = plot( image, r );
    }

    //  Clean up
    if ( freeColNames && colNames ) {
	Tcl_Free( (char *)colNames );
    }
    return status;
}

