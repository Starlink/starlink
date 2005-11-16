/**
 *  Name:
 *     GaiaSkySearch
 *
 *  Language:
 *     C++
 *
 *  Purpose:
 *     Defines the members of the GaiaSkySearch class.
 *
 *  Description:
 *     This class extends the SkySearch class (and thereby
 *     TclAstroCat) to use an external filter scheme to convert
 *     catalogues into "tab table" format. Keeping the names
 *     consistent and disposing of the intermediary files etc. These
 *     foreign catalogues are controlled by the GaiaLocalCatalog
 *     class.
 *
 *     A special override of the plot_objects function is added to
 *     sort out problems with catalogues that have both WCS and X-Y
 *     coordinates.
 *
 *  Authors:
 *     P.W. Draper (PWD)
 *
 *  Copyright:
 *     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
 *
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
 */

#include <config.h>  //  From skycat util

#include <cstdlib>
#include <iostream>
#include <sstream>

#include "error.h"
#include "util.h"
#include "AstroCatalog.h"
#include "LocalCatalog.h"
#include "GaiaLocalCatalog.h"
#include "SkySearch.h"
#include "GaiaSkySearch.h"

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
    {"check",  &GaiaSkySearch::checkCmd,        1,  1},
    {"csize",  &GaiaSkySearch::csizeCmd,        1,  1},
    {"entry",  &GaiaSkySearch::entryCmd,        1,  4},
    {"info",   &GaiaSkySearch::infoCmd,         1,  2},
    {"open",   &GaiaSkySearch::openCmd,         1,  1},
    {"origin", &GaiaSkySearch::originCmd,       0,  2},
    {"save",   &GaiaSkySearch::saveCmd,         1,  5}
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
    
    //  XXX g++ bug (egcs 1.0.3a), it seems that using static references
    //  to members functions doesn't work when these are virtual
    //  functions inherited from a "public virtual" base class
    //  (SkySearch inherits TclAstroCat this way). So use this following
    //  code, which seems to fix things. Recheck when newer version of
    //  egcs are produced.
    
    if ( strcmp( name, "info" ) == 0 ) {
        if ( check_args( name, argc, 1, 2 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return infoCmd( argc, argv );
    }
    
    if ( strcmp( name, "check" ) == 0 ) {
        if ( check_args( name, argc, 1, 1 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return checkCmd( argc, argv );
    }
    
    if ( strcmp( name, "csize" ) == 0 ) {
        if ( check_args( name, argc, 1, 1 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return csizeCmd( argc, argv );
    }
    
    if ( strcmp( name, "entry" ) == 0 ) {
        if ( check_args( name, argc, 1, 4 ) != TCL_OK ) {
            return TCL_ERROR;
         }
        return entryCmd( argc, argv );
    }
    
    if ( strcmp( name, "open" ) == 0 ) {
        if ( check_args( name, argc, 1, 1 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return openCmd( argc, argv );
    }
    
    if ( strcmp( name, "save" ) == 0 ) {
        if ( check_args( name, argc, 1, 5 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return saveCmd( argc, argv );
    }
    
    if ( strcmp( name, "origin" ) == 0 ) {
        if ( check_args( name, argc, 0, 2 ) != TCL_OK ) {
            return TCL_ERROR;
        }
        return originCmd( argc, argv );
    }
    
    while (low <= high) {
        mid = (low + high) / 2;
        if ((cond = strcmp(name, subcmds_[mid].name)) < 0)
            high = mid - 1;
        else if (cond > 0)
            low = mid + 1;
        else {
            GaiaSkySearchSubCmds& t = subcmds_[mid];
            if (check_args(name, argc, t.min_args, t.max_args) != TCL_OK)
                return TCL_ERROR;
            return (this->*t.fptr)(argc, argv);
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
    : xOrigin_(0.0),
      yOrigin_(0.0),
      SkySearch( interp, cmdname, instname ),
      TclAstroCat(interp, cmdname, instname) //  Needed as SkySearch
                                             //  inherits TclAstroCat
                                             // as "public virtual"!
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
    }
    
    //  Get the entry for this catalogue type.
    CatalogInfoEntry *e = CatalogInfo::lookup( argv[0] );
    if ( !e ) {
        return TCL_ERROR;
    }
    
    //  Now open the catalog, using the appropriate type.
    if ( strcmp( e->servType(), "local" ) == 0 ) {
        
        //  Local catalogues are either tab tables or a format
        //  recognised by GaiaLocalCatalog.
        if ( GaiaLocalCatalog::is_foreign( argv[0] ) ) {
            cat_ = new GaiaLocalCatalog( e, interp_ );
        } else {
            cat_ = new LocalCatalog( e );
        }
    } else {
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
    } else {
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
        } else {
            if ( argc > 2 ) {
                
                //  Use given catalog directory.
                dir = CatalogInfo::lookup( argv[2] );
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
            
        } else {
            
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
        } else {
            
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
        os << ends;
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
 *  have both pixel coordinates and sky coordinates.
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
		strcpy(xy_units, "deg");
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
	free(colNames);
    if (colIndexes)
	delete colIndexes;
    if (symb)
	free(symb);
    if (exprList)
	free(exprList);
    
    return status;
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
    } else {

        double xo;
        double yo;
        if ( Tcl_GetDouble( interp_, argv[0], &xo ) != TCL_OK ) {
            return error( argv[0], " is not a floating point value");
        } else {
            if ( Tcl_GetDouble( interp_, argv[1], &yo ) != TCL_OK ) {
                return error( argv[1], " is not a floating point value");
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
                    } else {
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
