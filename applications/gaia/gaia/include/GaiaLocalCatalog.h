#ifndef _GaiaLocalCatalog_h_
#define _GaiaLocalCatalog_h_
//+
//   Name:
//      GaiaLocalCatalog

//  Purpose:
//     Defines the GaiaLocalCatalog class.

//  Language:
//     C++

//  Description:
//     This module defines the members of the GaiaLocalCatalog
//     class. This class implements methods for accessing foreign
//     catalogues (i.e. CAT) as if they were tab tables.

//  Copyright:
//    Copyright (C) 1998 Central Laboratory of the Research Councils

//  History:
//     25-SEP-1998 (PDRAPER):
//        Original version.
//     {enter_changes_here}

//-

#include <sys/stat.h>
#include <unistd.h>
#include "LocalCatalog.h"
#include "tcl.h"

class GaiaLocalCatalog : public LocalCatalog {

protected:

  //  The realfile name of the catalogue. 
  char *realname_;

  //  Last time temporary catalogue was mapped.
  time_t tempstamp_;

  //  Whether the temporary file has been modified.
  int modified_;

  //  Name of the Tcl interpreter to run commands in.
  Tcl_Interp *interp_;

  //  Free and dispose of a catalogue.
  virtual int freeCat();

  //  Length and name of conversion control object.
  enum {NAMELEN = 32};
  char convertTable_[NAMELEN];
  
  //  Create the conversion control object.
  virtual int startConvert();

  //  Read (and convert) catalogue.
  virtual int getInfo();

  //  Convert catalogue to tab-table format.
  virtual int convertTo( int now = 0 );

  //  Convert tab-table back to original format.
  virtual int convertFrom( int now = 0 );

  //  Get the modification date of a file.
  time_t modDate( const char *filename );

  //  Delete the temporary file.
  void dispose();

  //  Map and read the temporary file.
  int readTemp();

  //  Check that the catalogue is up to date.
  int checkInfo();

public:

  //  Constructor.
  GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp);

  //  Constructor for existing tab-tables to foreign conversion.
  GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp, 
		    const char *in, const char *out );

  //  Destructor.
  ~GaiaLocalCatalog();

  //  Check the validity of a catalogue.
  static int check_table( const char *file );

  //  Check if a catalogue is of a known type.
  static int is_foreign( const char *file );

  //  On-off conversion of tab-table to foreign format.
  static int save( CatalogInfoEntry *e, Tcl_Interp *interp, 
		   const char *in, const char *out );
};

#endif // _GaiaLocalCatalog_
