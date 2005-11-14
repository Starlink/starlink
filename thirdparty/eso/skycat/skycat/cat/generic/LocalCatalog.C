/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: LocalCatalog.C,v 1.3 2003/01/20 15:52:21 brighton Exp $
 *
 * LocalCatalog.C - method definitions for class LocalCatalog
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  11 Jun 96  Created
 */
static const char* const rcsId="@(#) $Id: LocalCatalog.C,v 1.3 2003/01/20 15:52:21 brighton Exp $";


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
#include "error.h"
#include "Mem.h"
#include "LocalCatalog.h"


/* 
 * static method to check the validity of a tab table file.
 * Returns 0 if OK.
 */
int LocalCatalog::check_table(const char* file)
{
    TabTable t;
    return TabTable::head(file, t);
}


/*
 * constructor - used internally only, public interface uses "open(name)" 
 * "e" is the catalog config entry object for this catalog.
 * (see CatalogInfo class)
 *
 * In this case, the catalog config entry may have been created automatically.
 * The name of the file containing the local catalog (tab table) is stored in
 * "e->url()", which is normally used to store the URL for HTTP access, but is
 * used here for the filename.
 *
 */
LocalCatalog::LocalCatalog(CatalogInfoEntry* e) 
    : AstroCatalog(e),
      filename_(strdup(e->url()))
{
    status_ = getInfo();
}


/*
 * destructor
 */
LocalCatalog::~LocalCatalog() 
{
    if (filename_)
	free(filename_);
}


/*
 * Run a query on the local catalog and return the number of objects found.
 *
 * Args:
 *     q -        (in)   object describing the query
 *
 *     filename - (in)   filename to hold results, or null
 *
 *     result -   (out)  reference to object to manage the results. 
 *
 * The return value is the number of rows found, or 0 if none were found.
 * A return value of -1 indicates an error.
 *
 * (Redefined from parent class to work with local catalogs)
 */
int LocalCatalog::query(const AstroQuery& q, const char* filename, QueryResult& result)
{
    if (checkInfo() != 0)
	return -1;

    // note the catalog config entry in the results
    result.entry(entry_);

    if (result.query(q, info_, filename, more_) != 0)
	return -1;

    return result.numRows();
}


/*
 * If we don't have the info for this catalog, get it and 
 * return the status. Here we also check if the file has been modified,
 * (by an insert or remove operation) and reload it if needed.
 */
int LocalCatalog::checkInfo()
{
    if (info_.numCols() > 0) {
	struct stat buf;
	if (stat(filename_, &buf) != 0)
	    return sys_error("can't access file: ", filename_);
	if (buf.st_mtime == timestamp_)
	    return 0;
    }
    return getInfo();
}


/*
 * Read the local catalog to get the column names and also read in the
 * data to make later searches faster.  The return value is 0 for
 * success. The info_ member holds the column info and the local catalog
 * data for searching. It must be updated if the data changes.
 */
int LocalCatalog::getInfo()
{
    // note update time of file, so we know if it has been modified...
    struct stat buf;
    if (stat(filename_, &buf) != 0)
	return sys_error("can't access file: ", filename_);
    timestamp_ = buf.st_mtime;

    // mmap the file and put it in a TabTable
    Mem m(filename_);
    if (m.status() != 0)
	return 1;
    
    // make a null terminated copy, which will be managed by info_
    size_t size = m.size() + 1;
    char* data = (char*)malloc(size);
    if (!data)
	return fmt_error("can't allocate %d bytes for %s", size, filename_);
    strncpy(data, (char*)m.ptr(), size-1);
    data[size-1] = '\0';

    if (info_.init(data, 0, 1) != 0)
	return 1;

    // this will extract any catalog config info from the file's header
    info_.entry(entry_, data);
    return 0;
}

