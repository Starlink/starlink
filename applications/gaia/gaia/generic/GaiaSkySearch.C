//+
//  Name:
//     GaiaSkySearch
//
//  Language:
//     C++
//
//  Purpose:
//     Defines the members of the GaiaSkySearch class.
//
//  Description:
//     This class extends the SkySearch class (and thereby
//     TclAstroCat) to use an external filter scheme to convert
//     catalogues into "tab table" format. Keeping the names
//     consistent and disposing of the intermediary files etc. These
//     foreign catalogues are controlled by the GaiaLocalCatalog
//     class.
//
//  Authors:
//     P.W. Draper (PWD)
//
//  Copyright:
//     Copyright (C) 1998 Central Laboratory of the Research Councils
//
//  History:
//     25-SEP-1998 (PWD):
//        Original version.
//     {enter_new_authors_here}
//-

#include <stdlib.h>
#include <iostream.h>
#include <strstream.h>
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
    char* name;      // method name
    int (GaiaSkySearch::*fptr)(int argc, char* argv[]);
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = {
  {"check",  &GaiaSkySearch::checkCmd,        1,  1},
  {"csize",  &GaiaSkySearch::csizeCmd,        1,  1},
  {"entry",  &GaiaSkySearch::entryCmd,        1,  4},
  {"open",   &GaiaSkySearch::openCmd,         1,  1},
  {"save",   &GaiaSkySearch::saveCmd,         1,  5},
};

//
//  Call the given method in this class with the given arguments
//
int GaiaSkySearch::call(const char* name, int len, int argc, char* argv[])
{
  //  Since this tcl command may have a lot of subcommands, we do a
  //  binary search on the method table.
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

//
//  Implementation of the tcl extended command "astrocat". This creates
//  a new instance of the astrocat command.
//
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

//
//  Constructor -
//
//  Create an astrocat object in tcl for accessing the contents of
//  local and remote catalogues, including CAT catalogues.
//
//  Note that the tcl command for this object is created in the
//  parent class constructor.
//
GaiaSkySearch::GaiaSkySearch( Tcl_Interp *interp,
                              const char *cmdname,
                              const char *instname )
  : SkySearch( interp, cmdname, instname ),
    TclAstroCat(interp, cmdname, instname) //  Needed as SkySearch
                                           //  inherits TclAstroCat 
                                           // as "public virtual"!
{
}

//
//  Destructor -
//
//
GaiaSkySearch::~GaiaSkySearch()
{
}

//
//  Open the given astronomical catalogue.
//
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
    cat_ = NULL;
    return error( "Failed to open catalog:", argv[0] );
  }

  //  Set up feedback, if requested.
  if ( feedback_ ) {
    cat_->feedback( feedback_ );
  }
  
  return TCL_OK;
}

//  Check that the given filename is a valid local catalog.
//
int GaiaSkySearch::checkCmd(int argc, char* argv[])
{
  if ( GaiaLocalCatalog::is_foreign( argv[0] ) ) {
    return GaiaLocalCatalog::check_table( argv[0] );
  } else {
    return LocalCatalog::check_table( argv[0] );
  }
}

//
//  Override the "entry" command to make sure url and longname are
//  the same for foreign catalogues when written into the skycat.cfg
//  file.
//
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
  }
  return ret;
}

//
//  Save command. Override so we can get foreign catalogues to
//  do their conversion correctly.
//
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

//
//  Determine the maximum size of character strings needed to store
//  the columns of a tab-table. The tab-table is passed as a Tcl list
//  of lists and the return is a list of the number of characters
//  needed. This is implemented in C++ for speed reasons only.
//
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
    ostrstream os;
    for ( i = 0; i < ncolumn; i++ ) {
      os << sizes[i] << " ";
    }
    os << ends;
    set_result(os.str());
    delete os.str();
  }
  if ( mainArgc > 0 ) {
    Tcl_Free( (char *) mainArgv );
  }
  return TCL_OK;
}
