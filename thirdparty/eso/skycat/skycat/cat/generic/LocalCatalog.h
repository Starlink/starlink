// -*-c++-*-
#ifndef _LocalCatalog_h_
#define _LocalCatalog_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: LocalCatalog.h,v 1.1.1.1 2002/04/04 20:11:45 brighton Exp $
 *
 * LocalCatalog.h - class definitions for accessing local
 *                  catalogs stored as starbase format tab tables.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  11 Jun 96  Created
 * Peter W. Draper 21 Sep 98  Modified private data members to be
 *                            protected. Need these for derived
 *                            classes. 
 */

#include "AstroCatalog.h"

/*
 * Class LocalCatalog
 *
 * This class is used to search a local catalog stored as a file
 * in tab table format. The format of the table is something like:
 *
 * TableName
 *
 * VAR=Value
 *
 * A	B	C
 * -	-	-
 * 0	1	3
 * 3	2	4
 * ...
 *
 * where the table name and variable assignments are optional. 
 */
class LocalCatalog : public AstroCatalog {

protected:  // PWD: change here
    char* filename_;		// file name for local catalog
    time_t timestamp_;		// last update time of file, for caching

public:
    // constructor - create local catalog class instance
    // note: public interface uses AstroCatalog::open() with the name of the
    // file containing the local catalog.
    // The argument represents the entry in the catalog config file for this catalog
    // (made automatially, if not already present).
    LocalCatalog(CatalogInfoEntry*);

    // destructor
    ~LocalCatalog();

    // Run a query on the catalog and return the number of objects found.
    // (redefined here to work with local catalogs)
    virtual int query(const AstroQuery& q, const char* filename, QueryResult& result);

    // check the validity of a tab table file
    static int check_table(const char* file);

    // query server for catalog column names and put result in info_
    virtual int getInfo();
    virtual int checkInfo();
};

#endif /* _LocalCatalog_h_ */
