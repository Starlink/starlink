#ifndef _CatLocalCatalog_h_
#define _CatLocalCatalog_h_
//+
//   Name:
//      CatLocalCatalog

//  Purpose:
//     Defines the CatLocalCatalog class.

//  Language:
//     C++

//  Description:
//     This module defines the members of the CatLocalCatalog
//     class. This class implements methods for accessing CAT
//     catalogues as if they were tab tables.

//  Copyright:
//    Copyright (C) 1998 Central Laboratory of the Research Councils

//  History:
//     21-SEP-1998 (PDRAPER):
//        Original version.
//     {enter_changes_here}

//-

#include "LocalCatalog.h"

class CatLocalCatalog : public LocalCatalog {

protected:

  //  Data members.
  char *tabData_;    // Catalogue columns converted to tab format.
  int catId_;        // Identifier of CAT catalogue.

  //  Open and read a CAT catalogue.
  virtual int openCat( const char *filename );

  //  Free CAT catalogue.
  virtual void freeCat();

public:

  //  Constructor.
  CatLocalCatalog( CatalogInfoEntry *e);

  //  Destructor.
  ~CatLocalCatalog() {};

  //  Check the validity of a catalogue.
  static int check_table(const char* file);

  //  Query server for catalog column names and put result in info_
  virtual int getInfo();
};

#endif // _CatLocalCatalog_
