// -*-c++-*-
#ifndef _TcsLocalCatalog_h_
#define _TcsLocalCatalog_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsLocalCatalog.h,v 1.1.1.1 2002/04/04 20:11:45 brighton Exp $
 *
 * TcsLocalCatalog.h - class definitions for accessing local
 *                     TCS catalogs stored as starbase format tab tables.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Jun 96  Created
 */

#include "TcsCatalog.h"

/*
 * Class TcsLocalCatalog
 *
 * This class is like LocalCatalog, except that the table columns are fixed
 * to be a subset of the GSC/PPM columns.
 */
class TcsLocalCatalog : public TcsCatalog {
private:
    char* filename_;	// file name for local catalog
    time_t timestamp_;		// last update time of file, for caching

protected:
public:
    // constructor - create local TCS catalog class instance
    // note: public interface uses TcsCatalog::open() with the name of the
    // file containing the local TCS catalog.
    // The argument represents the entry in the catalog config file for this catalog
    // (made automatially, if not already present).
    TcsLocalCatalog(CatalogInfoEntry* e);

    // destructor
    ~TcsLocalCatalog();

    // Run a query on the catalog and return the number of objects found.
    // (redefined here to work with local catalogs)
    virtual int query(const AstroQuery& q, const char* filename, QueryResult& result);

    // check the validity of a tab table file
    static int check_table(const char* file);

    // query server for catalog column names and put result in info_
    virtual int getInfo();
    virtual int checkInfo();
};

#endif /* _TcsLocalCatalog_h_ */
