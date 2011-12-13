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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

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
#include "TclQueryUtil.h"
#include "GaiaWorldCoords.h"
#include "GaiaQueryResult.h"
#include "StarRtdImage.h"

//  Trig conversion factors.
static const double pi_ = 3.14159265358979323846;
static const double r2d_ = 57.295779513082323;   // (180.0/pi_)
static const double d2r_ = 0.017453292519943295; // (pi_/180.0);

static int gaiaGenAstroQuery( Tcl_Interp* interp, int argc, char* argv[],
                              AstroQuery& q, WorldOrImageCoords& pos1,
                              WorldOrImageCoords& pos2,
                              char* equinoxStr, FILE* feedback,
                              CatalogInfoEntry* entry );

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
    {"hms",        &GaiaSkySearch::hmsCmd,          1,  1},
    {"imgplot",    &GaiaSkySearch::imgplotCmd,      1,  4},
    {"info",       &GaiaSkySearch::infoCmd,         1,  2},
    {"namesvr",    &GaiaSkySearch::namesvrCmd,      2,  2},
    {"open",       &GaiaSkySearch::openCmd,         1,  2},
    {"origin",     &GaiaSkySearch::originCmd,       0,  2},
    {"save",       &GaiaSkySearch::saveCmd,         1,  5},
    {"setequinox", &GaiaSkySearch::setequinoxCmd,   1,  1}
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
      yOrigin_(0.0),
      hms_(1)
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

            //  Invalidate the URL of the entry to avoid premature
            //  loading (when a known local catalogue).
            e->url( " " );
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

    //  Reset display format.
    hms_ = 1;

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

/*
 * In GAIA %% is a pseudonym for "::". Needed as ":" separates multiple
 * symbols and "::" allows global variable access.
 */
static void replace_percents( char *str )
{
    char *p = str;
    while ( ( p = strstr( p, "%%" ) ) ) {
        *p++ = ':';
        *p++ = ':';
    }
}

/**
 *  Override plot_objects to sort out problems with plotting when
 *  have both pixel coordinates and sky coordinates. Also now we
 *  are using AST transformations we need to make sure that further
 *  transformations are not applied (the equinox is now the same as the
 *  image) and support STC regions.
 */
int GaiaSkySearch::plot_objects( Skycat* image, const QueryResult& r,
                                 const char* cols, const char* symbol,
                                 const char* expr )
{
    char *glexpr;
    char *glsymbol;
    char** colNames = NULL;
    char** exprList = NULL;
    char** symb = NULL;
    int nexpr = 0;
    int nsymb = 0;
    int numCols = 0;
    int status = 0;
    int* colIndexes = NULL;

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

        //  In GAIA %% is a pseudonym for "::".
        glsymbol = strdup( symbol );
        replace_percents( glsymbol );

        //  Parse symbol info, a variable length list of
        //  {shape color ratio angle label cond}
        if ((status = Tcl_SplitList(interp_, glsymbol,
                                    &nsymb, &symb)) != TCL_OK) {
            free( glsymbol );
            break;
        }
        free( glsymbol );

        //  Default values
        char* shape = (char *) "";
        char* fg = (char *) "white"; // if no color is specified, use 2: b&w
        char* bg = (char *) "black";
        char* ratio = (char *) "1";  // these may be Tcl expressions
        char* angle = (char *) "0";
        char* label = (char *) "";
        char* cond = (char *) "1";
        if ((status = parse_symbol(r, nsymb, symb, shape, fg, bg, ratio,
                                   angle, label, cond)) != TCL_OK)
            break;

        // parse the size expr list: {size units}
        glexpr = strdup( expr );
        replace_percents( glexpr );
        if ((status = Tcl_SplitList(interp_, (char*)glexpr,
                                    &nexpr, &exprList)) != TCL_OK) {
            free( glexpr );
            break;
        }
        free( glexpr );
        if (nexpr == 0 || strlen(exprList[0]) == 0) {
            status = error("invalid symbol expression: ", expr);
            break;
        }
        char* size = exprList[0];
        char* units = (char *) "image";
        if (nexpr > 1 && strlen(exprList[1]))
            units = exprList[1];

        //  Is this an STC shape?
        int stc_shape = 0;
        if ( strcmp( shape, "stcshape" ) == 0 && r.stc_col() != -1 ) {
            stc_shape = 1;
        }

        //  xy_units are re-used to carry the shape, need to handle
        //  storage for that.
        char xy_storage[32];
        char *xy_units = xy_storage;

        // for each row in the catalog, eval the expressions and plot the symbols
        int nrows = r.numRows();
        int id_col = r.id_col();
        int stc_col = r.stc_col();
        for (int rownum = 0; rownum < nrows; rownum++) {
            char* id;
            if ((status = r.get(rownum, id_col, id)) != 0)
                break;
            WorldOrImageCoords pos;
            if ((status = r.getPos(rownum, pos)) != 0)
                break;
            double x, y;
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

                if ( stc_shape ) {
                    //  xy_units are the shape.
                    if ((status = r.get(rownum, stc_col, xy_units)) != 0)
                        break;
                }
                else {
                    //  Degrees in the same units as image, so same equinox
                    //  in skycat speak.
                    const char *equinox = image->image()->wcs().equinoxStr();
                    strcpy(xy_units, "deg ");
                    strcat(xy_units, equinox);
                }
            }
            else {
                status = error("no wcs or image coordinates to plot");
                break;
            }
            if ((status = plot_row(image, r, rownum, id, x, y, xy_units,
                                   numCols, colNames, colIndexes, shape,
                                   bg, fg, ratio, angle, label, cond, size,
                                   units)) != TCL_OK)
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
        "stcshape",
        "triangle",
        "rectangle",
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
    Tcl_Obj *result = Tcl_GetObjResult( interp_ );
    if ( cat_->isWcs() ) {
        GaiaWorldCoords pos;
        char dec_buf[32];
        char ra_buf[32];
        int dec_col = qr->dec_col();
        int ra_col = qr->ra_col();

        for ( int i = 0; i < nrows; i++ ) {

            // Row includes formatted RA & Dec, do that first.
            if ( qr->getPos( i, pos ) != 0 ) {
                return error( "Failed getting world coordinates" );
            }
            pos.format( ra_buf, dec_buf, equinoxStr_ );

            //  Put the column values in a list.
            Tcl_Obj *list = Tcl_NewListObj( 0, NULL );
            for ( int j = 0; j < ncols; j++ ) {
                if ( j == ra_col ) {
                    Tcl_ListObjAppendElement( interp_, list,
                                              Tcl_NewStringObj(ra_buf, -1) );
                }
                else if ( j == dec_col ) {
                    Tcl_ListObjAppendElement( interp_, list,
                                              Tcl_NewStringObj(dec_buf, -1) );
                }
                else {
                    if ( qr->get( i, j, s ) == 0 ) {
                        Tcl_ListObjAppendElement( interp_, list,
                                                  Tcl_NewStringObj( s, -1 ) );
                    }
                    else {
                        Tcl_ListObjAppendElement( interp_, list,
                                                  Tcl_NewStringObj( "", -1 ) );
                    }
                }
            }
            Tcl_ListObjAppendElement( interp_, result, list );
        }
    }
    else {
        // Image coords or no coords - no special formatting needed.
        for ( int i = 0; i < nrows; i++ ) {
            Tcl_Obj *list = Tcl_NewListObj( 0, NULL );
            for ( int j = 0; j < ncols; j++) {
                if ( qr->get( i, j, s ) == 0 ) {
                    Tcl_ListObjAppendElement( interp_, list,
                                              Tcl_NewStringObj( s, -1 ) );
                }
                else {
                    Tcl_ListObjAppendElement( interp_, list,
                                              Tcl_NewStringObj( "", -1 ) );
                }
            }
            Tcl_ListObjAppendElement( interp_, result, list );
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
                                   GaiaQueryResult &r )
{
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

            //  Reset any AST errors.
            if ( !astOK ) {
                astClearStatus;
            }

            //  Locate the RA and Dec axes in the given FrameSet (the current
            //  coordinate system must be a SkyFrame and should represent the
            //  catalogue coordinates).
            int astime1 = astGetI( frmset, "astime(1)" );
            int astime2 = astGetI( frmset, "astime(2)" );
            int ra_index = 1;
            if ( astime2 ) {
                ra_index  = 2;
            }

            //  If neither axis of catalogue is time then this is some other
            //  coordinate system like AZEL. These need to be displayed as
            //  degrees and not converted into hours.
            int range_check = 1;
            if ( !astime1 && !astime2 ) {
                range_check = 0;
                r.setAssumeDegrees( 1 );
            }

            //  Slight wart is if the image doesn't show RA and Dec. In that
            //  case the catalogue positions will not interpreted as hours.
            //  Also it may have RA and Dec swapped. Sky requires RA and Dec
            //  in that order.
            int current = astGetI( frmset, "Current" );
            int base = astGetI( frmset, "Base" );
            astSetI( frmset, "Current", base );

            astime1 = astGetI( frmset, "astime(1)" );
            astime2 = astGetI( frmset, "astime(2)" );

            int image_ra_index = 1;
            if ( astime2 ) {
                image_ra_index  = 2;
            }

            int image_radec = 1;
            if ( astime1 == 0 && astime2 == 0 ) {
                image_radec = 0;
            }

            astSetI( frmset, "Current", current );

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

                //  Convert values to double precision, use Skycat decoding as
                //  that's what was used to encode (rather than astUnformat).
                double xin[1];
                double yin[1];
                double xout[1];
                double yout[1];
                GaiaWorldCoords pos( raPtr, decPtr, range_check, 2000.0,
                                     !range_check );
                if ( pos.status() != 0 ) {
                    raStr[0] = decStr[0] = '\0';
                }
                else {
                    xin[0] = pos.ra_deg() * d2r_;
                    yin[0] = pos.dec_deg() * d2r_;

                    //  The catalogue RA and Dec could be swapped (not for
                    //  normal Skycat) as could the image RA and Dec. We
                    //  need these back in RA/Dec order when done.
                    if ( ra_index == 1 ) {
                        if ( image_ra_index == 1 ) {
                            astTran2( frmset, 1, xin, yin, 0, xout, yout );
                        }
                        else {
                            astTran2( frmset, 1, xin, yin, 0, yout, xout );
                        }
                    }
                    else {
                        if ( image_ra_index == 1 ) {
                            astTran2( frmset, 1, yin, xin, 0, xout, yout );
                        }
                        else {
                            astTran2( frmset, 1, yin, xin, 0, yout, xout );
                        }
                    }

                    if ( ! astOK ) {
                        astClearStatus;
                        raStr[0] = decStr[0] = '\0';
                    }
                    else {
                        //  Format value as decimal degrees for result.
                        //  AST has returned in radians, so we convert for
                        //  that. Depending on the coordinates of the image
                        //  and catalogue differing interpretations will be
                        //  applied to the "RA" values, so these need to be
                        //  scaled to and from hours.
                        xout[0] *= r2d_;
                        yout[0] *= r2d_;
                        if ( ! image_radec ){
                            if ( range_check ) {
                                //  Image in degrees, catalogue in RA/Dec.
                                //  Values will be interpreted as hours.
                                if ( ra_index == 1 ) {
                                    xout[0] *= 15.0;
                                }
                                else {
                                    yout[0] *= 15.0;
                                }
                            }
                        }
                        else {
                            if ( ! range_check ) {
                                //  Image in RA/Dec, catalogue in degrees.
                                //  Values will be scaled as if in hours.
                                if ( ra_index == 1 ) {
                                    xout[0] /= 15.0;
                                }
                                else {
                                    yout[0] /= 15.0;
                                }
                            }
                        }
                        if ( ra_index == 1 ) {
                            sprintf( raStr, "%.17g", xout[0] );
                            sprintf( decStr, "%.17g", yout[0] );
                        }
                        else {
                            sprintf( raStr, "%.17g", yout[0] );
                            sprintf( decStr, "%.17g", xout[0] );
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
                }
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
 *  gaiautils:: to create one of these.
 *
 */
int GaiaSkySearch::imgplotCmd( int argc, char* argv[] )
{
    //  Can't plot without a catalog, since no plot symbols would be defined.
    if ( !cat_ ) {
        return error( "no catalog is currently open" );
    }

    //  Get a pointer to the C++ class implementing the extended rtdimage
    //  object. We will need this to access the image to plot the catalog
    //  symbols.
    StarRtdImage *image = (StarRtdImage *) Skycat::getInstance( argv[0] );
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

            //  Slight wart. When connecting these the current frame will
            //  actually be the last one tested, usually that doesn't make
            //  sense as we are going from SKY to SKY. Short circuit this (and
            //  coincidently miss out all those other SKY based systems that
            //  tend to be incorrect, like initial pointings).
            AstFrame *current = (AstFrame *) astGetFrame( imagewcs,
                                                          AST__CURRENT );
            if ( astIsASkyFrame( current ) ) {
                frmset = (AstFrameSet *) astConvert( current, catwcs, "SKY" );
            }
            else {
                //  Not a SkyFrame so search FrameSet for one.
                frmset = (AstFrameSet *) astConvert( imagewcs, catwcs, "SKY" );
            }
            if ( imagewcs != NULL ) {
                imagewcs = (AstFrameSet *) astAnnul( imagewcs );
            }
            if ( current != NULL ) {
                current = (AstFrame *) astAnnul( current );
            }
            if ( ! astOK ) {
                if ( frmset != NULL ) {
                    frmset = (AstFrameSet *) astAnnul( frmset );
                }
                astClearStatus;
                return error( "Failed to connect image and catalogue"
                              " coordinates" );
            }
        }
        else {
            cout << "Warning: invalid FrameSet, cannot connect image and " <<
                "catalogue coordinates" << endl;
        }
    }

    //  For STC shapes we need to reset the cache.
    image->clearStcMapping();

    //  Get the column names
    if ( argc < 4 ) {
        //  Use current catalogue.
        numCols = cat_->numCols();
        colNames = cat_->colNames();
    }
    else {
        //  Use headings list.
        if ( Tcl_SplitList(interp_, argv[3], &numCols, &colNames) != TCL_OK ) {
            if ( frmset != NULL ) {
                frmset = (AstFrameSet *) astAnnul( frmset );
            }
            return TCL_ERROR;
        }
        freeColNames++;
    }

    //  Get query results from arguments
    GaiaQueryResult r;
    r.entry( cat_->entry() );
    int status = getQueryResult( numCols, (char**)colNames, argv[1],
                                 frmset, r );
    if ( frmset != NULL ) {
        frmset = (AstFrameSet *) astAnnul( frmset );
    }
    if ( status == TCL_OK ) {
        status = plot( image, r );
    }

    //  Clean up
    if ( freeColNames && colNames ) {
        Tcl_Free( (char *)colNames );
    }
    return status;
}


/*
 * pass a query to the current catalog and return the result as a list of
 * rows.
 *
 * See TclAstroCat::queryCmd
 *
 * Overridden so we can use a special version of WorldCoords that doesn't
 * necessarily re-format celestial latitude and longitude axis as sexagesimal
 * RA and Dec.
 */
int GaiaSkySearch::queryCmd(int argc, char* argv[])
{
    if (!cat_)
        return error("no catalog is currently open");

    // generate the query from the command args
    AstroQuery q;
    if (gaiaGenAstroQuery(interp_, argc, argv, q, pos1_, pos2_,
                          equinoxStr_, feedback_, cat_->entry()) != TCL_OK)
        return TCL_ERROR;

    // make new GaiaQueryResult object, or reuse previous one
    if (result_)
        result_->clear();
    else
        result_ = new GaiaQueryResult;

    // do the query
    int nrows = cat_->query(q, NULL, *result_);

    // get the results
    int ncols = result_->numCols();
    char* s;
    int i = 0, j = 0;

    if (nrows >= 0) {
        Tcl_ResetResult(interp_);
        Tcl_Obj *result = Tcl_GetObjResult( interp_ );

        for (i = 0; i < nrows; i++) {

            // start a row
            Tcl_Obj *list = Tcl_NewListObj( 0, NULL );

            if (cat_->isWcs()) { // include formatted world coords
                GaiaWorldCoords pos;
                if (result_->getPos(i, pos) != 0)
                    return TCL_ERROR;

                // format the ra,dec position arguments in H:M:S...
                char ra_buf[32], dec_buf[32];
                int ra_col = result_->ra_col(), dec_col = result_->dec_col();
                pos.format(ra_buf, dec_buf, equinoxStr_, hms_);

                // put the column values in a list
                for (j = 0; j < ncols; j++) {
                    if (result_->get(i, j, s) != 0)
                        s = (char *) "";
                    if (j == ra_col)
                        Tcl_ListObjAppendElement
                            ( interp_, list, Tcl_NewStringObj(ra_buf, -1) );
                    else if (j == dec_col)
                        Tcl_ListObjAppendElement
                            ( interp_, list, Tcl_NewStringObj(dec_buf, -1) );
                    else
                        Tcl_ListObjAppendElement
                            ( interp_, list, Tcl_NewStringObj(s, -1) );
                }
            }
            else {  // image coords - no special formatting needed
                // put the column values in a list
                for (j = 0; j < ncols; j++) {
                    if (result_->get(i, j, s) != 0)
                        s = (char *) "";
                    Tcl_ListObjAppendElement
                        ( interp_, list, Tcl_NewStringObj( s, -1 ) );
                }
            }

            // end a row
            Tcl_ListObjAppendElement( interp_, result, list );
        }

        return TCL_OK;
    }
    return TCL_ERROR;   // an query error occured (and was reported)
}

/**
 *  Set the format used to display the world coordinates of the
 *  catalogue. When true sexagesimal will be used, other the
 *  values will be shown in degrees.
 */
int GaiaSkySearch::hmsCmd( int argc, char *argv[] )
{
    return Tcl_GetBoolean( interp_, argv[0], &hms_ );
}

/*
 * This utility routine generates an AstroQuery object, given the command
 * line arguments for a Tcl query command. In addition, the world coordinate
 * position and equinox arguments of the query are returned.
 *
 * See genAstroQuery in TclQueryUtil for the description and original
 * code. Re-write to use GaiaWorldCoords class.
 */
int gaiaGenAstroQuery(Tcl_Interp* interp, int argc, char* argv[],
                      AstroQuery& q, WorldOrImageCoords& pos1, WorldOrImageCoords& pos2,
                      char* equinoxStr, FILE* feedback, CatalogInfoEntry* entry)
{
    // set defaults
    int status = 0;
    pos1.setNull(); pos2.setNull();
    int isWcs = entry->isWcs();
    int isPix = entry->isPix();
    strcpy(equinoxStr, "2000");
    double radius1 = 0.0, radius2 = 0.0;
    double mag1 = 0.0, mag2 = 0.0;
    double width = 0.0, height = 0.0;
    const char* id = "";
    const char* nameServer = "simbad@eso";

    // for sorting
    int numSortCols = 0;
    char** sortCols = NULL;
    const char* sortOrder = "increasing";
    int nrows = 0;  // no default limit...

    // column selection
    int numCols = 0;
    char** colNames = NULL;

    // for searching by colName, minValue, maxValue
    int numSearchCols = 0;
    char** searchCols = NULL;
    char** minValues = NULL;
    char** maxValues = NULL;

    // misc
    int got_pos = 0;		// flag: true if we read the position arg
    char** values = NULL;
    int numValues = 0;

    // parse options
    for (int i = 0; i < argc; i += 2) {
	char* option = argv[i];
	char* value = argv[i+1];

	// first handle options with only one value
	if (strcmp(option, "-id") == 0) {
	    id = value;
	}
	else if (strcmp(option, "-nameserver") == 0) {
	    nameServer = value;
	}
	else if (strcmp(option, "-sortorder") == 0) {
	    sortOrder = value;
	    if (strlen(value) == 0)
		sortOrder = "increasing";
	    else if (strcmp(sortOrder, "increasing") != 0 && strcmp(sortOrder, "decreasing") != 0)
		return error("expected -sortorder increasing (or decreasing), not: ", sortOrder);
	}
	else if (strcmp(option, "-name") == 0) {
	    if (AstroCatalog::nameToWorldCoords(value, pos1, nameServer, feedback) != 0)
		return TCL_ERROR;
	}
	else if (strcmp(option, "-equinox") == 0) {
	    if (got_pos)
		return error("-equinox should precede the -pos argument");
	    strcpy(equinoxStr, value);
	}
	else if (strcmp(option, "-nrows") == 0) {
	    if (Tcl_GetInt(interp, value, &nrows) != TCL_OK)
		return error("bad value for max number of rows: ",
                             Tcl_GetStringResult( interp ) );;
	}
	else if (strcmp(option, "-width") == 0) {
	    if (Tcl_GetDouble(interp, value, &width) != TCL_OK)
		return error("bad -width value: ",
                             Tcl_GetStringResult( interp ) );
	}
	else if (strcmp(option, "-height") == 0) {
	    if (Tcl_GetDouble(interp, value, &height) != TCL_OK)
		return error("bad -height value: ",
                             Tcl_GetStringResult( interp ) );
	}
	else {
	    // handle options whic<h may have a tcl list of values
	    if (Tcl_SplitList(interp, value, &numValues, &values) != TCL_OK)
		return TCL_ERROR;

	    if (numValues < 1) {
		status = fmt_error("expected a list of values for %s option", option);
		break;
	    }

	    if (strcmp(option, "-pos") == 0) {
		if (!isWcs && !isPix) {
		    status = error("This catalog does not have coordinates");
		    break;
		}
		got_pos++;
		if (numValues != 2 && numValues != 4) {
		    if (isWcs)
			status = error("expected -pos {ra dec} or {ra1 dec1 ra2 dec2} (WCS positions)");
		    else if (isPix)
			status = error("expected -pos {x y} or {x1 y1 x2 y2} (in image coordinates)");
		    break;
		}
		if (numValues == 4) {
		    if (isWcs)
			pos2 = GaiaWorldCoords(values[2], values[3], 0, equinoxStr, 1);
		    else if (isPix)
			pos2 = ImageCoords(values[2], values[3]);
		    if (pos2.status()) {
			status = TCL_ERROR;
			break;
		    }
		}
		if (isWcs)
		    pos1 = GaiaWorldCoords(values[0], values[1], 0, equinoxStr, 1);
		else if (isPix)
		    pos1 = ImageCoords(values[0], values[1]);
		if (pos1.status()) {
		    status = TCL_ERROR;
		}
	    }
	    else if (strcmp(option, "-radius") == 0) {
		if (numValues > 2) {
		    status = error("expected 1 or 2 values for -radius option");
		    break;
		}
		if (numValues == 2 && Tcl_GetDouble(interp, values[1], &radius2) != TCL_OK) {
		    status = error("bad max radius value: ",
                                   Tcl_GetStringResult( interp ) );
		    break;
		}
		if (Tcl_GetDouble(interp, values[0], &radius1) != TCL_OK) {
		    status = error("bad min radius value: ",
                                   Tcl_GetStringResult( interp ) );
		    break;
		}
	    }
	    else if (strcmp(option, "-mag") == 0) {
		if (numValues > 2) {
		    status = error("expected 1 or 2 values for -mag");
		    break;
		}
		if (numValues == 2 && Tcl_GetDouble(interp, values[1], &mag2) != TCL_OK) {
		    status = error("bad max magnitude value: ",
                                   Tcl_GetStringResult( interp ) );
		    break;
		}
		if (Tcl_GetDouble(interp, values[0], &mag1) != TCL_OK) {
		    status = error("bad min magnitude value: ",
                                   Tcl_GetStringResult( interp ) );
		    break;
		}
	    }
	    else if (strcmp(option, "-columns") == 0) {
		numCols = numValues;
		colNames = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-sort") == 0) {
		numSortCols = numValues;
		sortCols = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-searchcols") == 0) {
		numSearchCols = numValues;
		searchCols = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-minvalues") == 0) {
		if (numValues != numSearchCols) {
		    status = error("number of items for -minvalues not the same as for -searchcols");
		    break;
		}
		minValues = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-maxvalues") == 0) {
		if (numValues != numSearchCols) {
		    status = error("number of items for -maxvalues not the same as for -searchcols");
		    break;
		}
		maxValues = values;
		values = NULL;	// don't free
	    }
	}
    }

    if (values)
	Tcl_Free((char *)values);
    if (status != TCL_OK)
	return TCL_ERROR;

    // setup the query object and return an error if the arguments are invalid
    // (args are checked by AstroQuery class)
    if (strlen(id) && q.id(id))
	return TCL_ERROR;

    if (pos2.isNull()) {
	if (! pos1.isNull())
	    if (q.pos(pos1))
		return TCL_ERROR;
    }
    else {
	if (q.pos(pos1, pos2))
	    return ERROR;
    }

    if (radius2) {
	if (q.radius(radius1, radius2))
	    return TCL_ERROR;
    }
    else if (radius1) {
	if (q.radius(radius1))
	    return TCL_ERROR;
    }

    if (mag2) {
	if (q.mag(mag1, mag2))
	    return TCL_ERROR;
    }
    else if (mag1) {
	if (q.mag(mag1))
	    return TCL_ERROR;
    }

    if (width && height)
	if (q.dim(width, height))
	    return TCL_ERROR;

    if (numCols && colNames)
	if (q.colNames(numCols, (char**)colNames, 1))
	    return TCL_ERROR;

    if (nrows && q.maxRows(nrows))
	return TCL_ERROR;

    if (numSortCols && sortCols) {
	if (q.sort(numSortCols, (char**)sortCols, 1))
	    return TCL_ERROR;
	q.sortOrder(*sortOrder == 'i' ? 1 : -1);
    }

    if (numSearchCols && searchCols) {
	if (q.condition(numSearchCols, (char**)searchCols, (char**)minValues, (char**)maxValues, 1))
	    return TCL_ERROR;
    }

    return TCL_OK;
}


/**
 *  Set the catalogue equinox. Returns the current value as the result.
 */
int GaiaSkySearch::setequinoxCmd( int argc, char *argv[] )
{
    if ( !cat_ ) {
        return error( "no catalog is currently open" );
    }
    char buf[32];
    const char *prefix = cat_->equinoxprefix();
    if ( prefix != NULL && prefix[0] != '\0' ) {
        sprintf( buf, "%c%g", prefix[0], cat_->equinox() );
    }
    else {
        sprintf( buf, "%g", cat_->equinox() );
    }
    set_result( buf );

    //  Need to handle prefix and conversion to double.
    double d;
    const char *p = argv[0];
    if ( p[0] == 'j' || p[0] == 'J' ) {
        cat_->entry()->equinoxprefix( "J" );
        p++;
    }
    else if ( p[0] == 'b' || p[0] == 'B' ) {
        cat_->entry()->equinoxprefix( "B" );
        p++;
    }
    else {
        cat_->entry()->equinoxprefix( "" );
    }
    if ( sscanf( p, "%lf", &d ) == 1 ) {
        cat_->entry()->equinox( d );
    }
    return TCL_OK;
}
