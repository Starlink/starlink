//+
//   Name:
//      CatLocalCatalog

//  Purpose:
//     Defines a class for controlling access to a CAT catalogue
//     stored in a local file.

//  Language:
//     C++

//  Copyright:
//     Copyright (C) 1998 Central Laboratory of the Research Councils

//  Inherits:
//     LocalCatalog

//  Authors:
//     Peter W. Draper (PDRAPER):
//     {enter_new_authors_here}

//  History:
//     28-JUN-1996 (PDRAPER):
//        Original version, based on LocalCatalog.
//     {enter_changes_here}

//-

#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include "CatLocalCatalog.h"
#include "gaiacat.h"

//
//  Constructor - used internally only, public interface uses
//  "open(name)" "e" is the catalog config entry object for this
//  catalog.
//
CatLocalCatalog::CatLocalCatalog( CatalogInfoEntry *e ) 
  : LocalCatalog( e ),
  tabData_( NULL ),
  catId_( 0 )
{
}

//
//  Static method that checks the validity of the catalogue. This is
//  performed by getting CAT to open it.
//
int CatLocalCatalog::check_table( const char* filename )
{
  char *error_mess;
  int catId;
  if ( gaiaAccessCat( filename, &catId, &error_mess ) ) {
    gaiaFreeCat( catId );
    return 1;
  } else {
    free( error_mess );
    return 0;
  }
}

//
//  Read the local catalog to get the column names and to convert the
//  data into "tab table" format. This is kept in memory to make later
//  searches faster.  The return value is 0 for success. The info_
//  member holds the column info and the local catalog data for
//  searching. It must be updated if the data changes.
//
int CatLocalCatalog::getInfo()
{
  //  Note update time of file, so we know if it has been modified...
  struct stat buf;
  if ( stat(filename_, &buf) != 0 ) {
    return sys_error( "cannot access file: ", filename_ );
  }
  timestamp_ = buf.st_mtime;

  //  Access the CAT catalogue and convert it to a "tab data area" and
  //  a "tab header".
  if ( openCat( filename_ ) ) {

    //  Pass the tab data table to a QueryResult object to
    //  extract tab table data. Note we release the tab data when read.
    if ( info_.init( tabData_ ) != 0 ) {
      freeCat();
      return 1;
    }
    //  Now update any configuration paramters.
    info_.entry( entry_, tabData_ );
    freeCat();
  } else {
    return 1;
  }
  return 0;
}

//
//  Method to open a named CAT catalogue and read in the data and
//  "header" into memory. The data and headers are stored as in tab
//  table format. The headers as an array of strings in the form:
//  
//     parameter_name: value
//
//  up to the first line that starts with "-". The columns, which are
//  separated by tabs, follows from this point.
//
int CatLocalCatalog::openCat( const char *filename )
{

  // Open the catalogue.
  char *error_mess;
  if ( gaiaAccessCat( filename, &catId_, &error_mess ) ) {

    // And read it into the various parts we require.
    if ( gaiaReadCat( catId_, &tabData_, &error_mess ) ) {
      return 1;
    } else {
      error( error_mess );
      free( error_mess );
    }
  } else {
    error( error_mess );
    free( error_mess );
  }
  return 0;
}

//
//  Free all resources obtained via gaiaReadCat.
//
void CatLocalCatalog::freeCat() {
  if ( tabData_ != NULL ) {
    free( tabData_ );
    tabData_ = NULL;
  }
  gaiaFreeCat( catId_ );
  catId_ = 0;
}
