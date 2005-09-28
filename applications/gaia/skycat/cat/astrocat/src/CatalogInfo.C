/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: CatalogInfo.C,v 1.33 1999/03/15 12:30:04 abrighto Exp $
 *
 * CatalogInfo.C - method definitions for class CatalogInfo, CatalogInfoEntry
 *
 * This class is used to load, edit, access and save catalog config files
 * either locally or via HTTP from a remote host.
 *
 * A config file contains the information necessary to access catalogs. 
 * The syntax for each catalog entry is:
 *
 *   serv_type:   service type, one of: catalog, namesvr, imagesvr
 *                directory, local ... (see Service Types below)
 *
 *   long_name:   long name of service for displaying
 *   short_name:  short name of service
 *   url:         URL used to access catalog, %ra,%dec, etc. expanded (see below)
 *
 *   symbol:      the symbol to use to plot the given column value
 *                (see Plotting below)
 *
 *   copyright:   optional copyright notice to display in user interface
 *
 *   help:        optional URL pointing to help for the catalog
 *
 *   search_cols: optional list of columns that can be searched by in the format
 *                col1 "col1 min label" "col1 max label" : ...
 *                example:
 *                   search_cols:    mag "Brightest (min)" "Faintest (max)"
 *
 *   sort_cols:   optional list of columns to sort by {col1 col2 ...}
 *   sort_order:  optional: set to "increasing" or "decreasing"
 *
 *   show_cols:   optional list of columns to display (default: all)
 *
 *   id_col:      column containing id field
 *   ra_col:
 *   dec_col:     columns containing ra and dec (for catalogs supporting WCS)
 *  
 *   x_col:
 *   y_col:       columns containing x,y coords (for catalogs not supporting WCS)
 *
 *   is_tcs:      flag: true if using TCS columns
 *
 *  Service Types
 * ---------------
 *
 *  The currently known service types are:
 *
 *   catalog  - server returns a tab separated table of row/col values
 *
 *   namesvr  - server returns a single line with id, ra and dec to resolve
 *              the given object name
 *
 *   imagesvr - server returns an image file
 *
 *   directory - the URL is a pointer to another catalog config file
 *
 *   local - a local catalog
 *
 *
 *  Syntax for "url" field:
 *  --------------------------
 *
 *  The url field is used to build a URL to get the results via HTTP.
 *  The syntax is like this:
 *
 *       http://host:port/cgi-bin/server?arg1&arg2&...argn
 *
 *  (if ":port" is missing, it defaults to 80.)  
 *
 *  Substitutions are performed on the URL as follows:
 *
 *  For catalogs:
 *
 *   %ra, %dec        - coordinates of center point
 *
 *   %w, %h           - width and height in arcmin
 *
 *   %r1, %r2         - min and max radius (for circular query)
 *   %r               - use when server only accepts single radius value
 *
 *   %m1, %m2         - min and max magnitude
 *   %m               - use when server only accepts single magnitude
 *
 *   %n               - max number of rows to return
 *
 *   %cols            - list of columns to return (col1,col2,...coln)
 *
 *   %id              - ID field of item to return (if supported)
 *
 *   %mime-type       - value for http mime-type field  
 *
 *  Name servers only need the %id field, which is set to the object name.
 *
 *  Plotting column values
 *  ----------------------
 *
 *  The syntax for the "symbol:" field is as follows:
 *
 *     symbol:  colnames symbol expr : colnames symbol expr : ...
 *
 *  where 
 *    colnames - is a list of column names used (in symbol or expr)
 *
 *    symbol   - is the symbol to use, one of: square, circle, triangle, cross, 
 *               plus, diamond, ellipse. 
 *               The symbol may also be a list such as {circle yellow} and some 
 *               symbols take extra args for ratio and angle (ellipse).
 *
 *    expr     - is an expression in terms of colnames above, used to determine the 
 *               size of the symbol. It may also be a list {expr units}, where units
 *               is one of {image "deg $equinox"... } (default: image) (see rtd for
 *               coordinate syntax). The column names (colnames) can be used as variables
 *               in the expression using "$" (following Tcl syntax).
 *
 *  example: 
 *     symbol: mag circle 15-$mag : xyz square (1-$xyz)*2.5
 *     symbol: {a/b pa mag} {ellipse yellow ${a/b} $pa} {15-$mag}
 *     symbol: "a/b pa mag" "ellipse yellow ${a/b} $pa" "15-$mag"
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  29 Oct 95  Created
 * Peter W. Draper 25 Sep 03  Modified to output ra_col and dec_col 
 *                            even when at default values. Can cause
 *                            problems when x_col and y_col are also
 *                            set (after ra_col and dec_col).
 */
static const char* const rcsId="@(#) $Id: CatalogInfo.C,v 1.33 1999/03/15 12:30:04 abrighto Exp $";

#include "config.h"  //  From skycat util

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <iostream.h>
#include <fstream.h>

//  strstream will be in std:: namespace in cannot use the .h form.
#if HAVE_STRSTREAM_H
#include <strstream.h>
#define STRSTD
#else
#include <strstream>
#define STRSTD std
#endif

#include "error.h"
#include "util.h"
#include "HTTP.h"
#include "CatalogInfo.h"


// define the default URL for the catalog config info
const char* catlib_config_url_ = "http://archive.eso.org/skycat/skycat2.0.cfg";
   
// config info used if the http config file can't be found
static const char* config_info_ = 
    "serv_type:   catalog\n"
    "long_name:   Guide Star Catalog at ESO\n"
    "short_name:  gsc@eso\n"
    "url:         http://archive.eso.org/skycat/servers/gsc-server?%ra%dec&obj=%id&r=%r1,%r2&m=%m1,%m2&n=%n&f=8&s=R&F=*\n"
    "symbol:      mag circle 15-$mag\n"
    "search_cols: mag \"Brightest (min)\" \"Faintest (max)\""
    "\n"
    "serv_type:   imagesvr\n"
    "long_name:   Digitized Sky Server at ESO\n"
    "short_name:  dss@eso\n"
    "url:         http://archive.eso.org/cgi-bin/dss?ra=%ra&dec=%dec&mime-type=%mime-type&x=%w&y=%h\n"
    "\n"
    "serv_type:    namesvr\n"
    "long_name:    SIMBAD Names\n"
    "short_name:   simbad_ns@eso\n"
    "url:          http://archive.eso.org/cgi-bin/sim-server?&o=%id\n"
    "\n"
    "serv_type:    directory\n"
    "long_name:    ESO Catalogs\n"
    "short_name:   catalogs@eso\n"
    "url:          http://archive.eso.org/skycat/skycat2.0.cfg\n"
    ;

// linked list of catalog entries
CatalogInfoEntry* CatalogInfo::entries_ = NULL;

// initial undefined column index value
static const int undef_col_ = -99;

/*
 * strip leading and trailing white space from s and
 * return the result (s is modified)
 */
static char* strip(char* s)
{
    char* p = s;
    while(isspace(*p)) 
	p++;		
    s = p;
    p += (strlen(p) - 1);
    while(p >= s && isspace(*p)) 
	*p-- = '\0'; 
    return s;
}


/* 
 * split buf on the ':' char and strip leading and trailing
 * white space on both keyword and value
 */
int split(char* buf, char*& keyword, char*& value)
{
    // split keyword : value
    char* p = strchr(buf, ':');
    if (!p) 
	return 1;
    *p++ = '\0';
    keyword = strip(buf);
    value = strip(p);
    return 0;
}


/*
 * read a line from the stream into buf and return the stream. 
 * Lines ending with backslash are continued on the next line
 */ 
istream& CatalogInfo::getline(istream& f, char* buf, int size)
{
    if (f.getline(buf, size)) {
	char* p = buf;
	int i = strlen(p);
        if ( i > 0 ) {
            i -= 1;
            while(f && p[i] == '\\') {
                size -= i;
                p = p + i;
                if (f.getline(p, size)) {
                    i = strlen(p);
                    if ( i == 0 ) break;
                    i -= 1;
                }
            }
        }
    }
    return f;
}


/*
 * load a catalog config file from the given istream, construct a
 * linked list of entries from the file and return a pointer to the 
 * first entry in the list, or NULL if there were errors.
 *
 * The arguments are:
 *
 *  f - istream open for reading the config file (or buffer)
 *  filename - name of file (or URL) for error reporting
 */
CatalogInfoEntry* CatalogInfo::load(istream& f, const char* filename)
{
    int line = 0;		// line number in config file
    int n;
    char buf[10*1024];		// contents of a line
    char* keyword;		// left of ':'
    char* value;		// right of ':'
    CatalogInfoEntry* entry = NULL; // for list of catalog entries
    CatalogInfoEntry* first = NULL;

    while(getline(f, buf, sizeof(buf))) {
	line++;
	
	// skip comments and empty lines
	if (buf[0] == '#' || strlen(buf) == 0)
	    continue;

	// split keyword : value
	if (split(buf, keyword, value) != 0) {
	    cfg_error(filename, line, "missing ':'");
	    delete first;
	    return NULL;
	}

	if (strcmp(keyword, "serv_type") == 0) {
	    // check that the current entry is complete and start a new one
	    if (entry) {
		char* s;
		if ((s = entry->check()) != NULL) {
		    cfg_error(filename, line, s);
		    delete first;
		    return NULL;
		}
		if (first != entry) {
		    if (first->append(entry) != 0) {
			delete first;
			return NULL;
		    }
		}
		entry = new CatalogInfoEntry;
	    }
	    else {
		// start the first entry
		first = entry = new CatalogInfoEntry;
	    }
	} 
	else if (! entry) {
	    cfg_error(filename, line, "missing 'serv_type:' keyword");
	    delete first;
	    return NULL;
	}
	set_entry_value(entry, keyword, value, 0);
    }

    // check last entry 
    if (entry) {
	char* s;
	if ((s = entry->check()) != NULL) {
	    cfg_error(filename, line, s);
	    delete first;
	    return NULL;
	}
	if (first != entry) {
	    if (first->append(entry) != 0) {
		delete first;
		return NULL;
	    }
	}
    }
    else {
	error("no entries in config file: ", filename);
    }
    return first;
}


/*
 * Set the value for the given keyword in the given entry.  If updateFlag
 * is 1, only set fields that may be updated after an entry is
 * created. Fields such as the catalog name and URL may not be modified,
 * but the symbol and search_col info may be.
 *
 * We want to avoid overwriting changes that a user may have made interactively
 * in an application. Where it makes sense, string fields are only updated if 
 * they were not already set.
 */
int CatalogInfo::set_entry_value(CatalogInfoEntry* entry, 
				 const char* keyword, 
				 const char* value,
				 int updateFlag)
{
    if (!updateFlag) {
	if (strcmp(keyword, "serv_type") == 0) {
	    entry->servType(value);
	} 
	else if (strcmp(keyword, "long_name") == 0) {
	    entry->longName(value);
	} 
	else if (strcmp(keyword, "short_name") == 0) {
	    entry->shortName(value);
	} 
	else if (strcmp(keyword, "url") == 0) {
	    entry->url(value);
	} 
	else if (strcmp(keyword, "backup1") == 0) {
	    entry->backup1(value);
	} 
	else if (strcmp(keyword, "backup2") == 0) {
	    entry->backup2(value);
	} 
    }

    if (strcmp(keyword, "symbol") == 0) {
	if (! entry->symbol())
	    entry->symbol(value);
    } 
    else if (strcmp(keyword, "search_cols") == 0) {
	if (! entry->searchCols())
	    entry->searchCols(value);
    } 
    else if (strcmp(keyword, "sort_cols") == 0) {
	if (! entry->sortCols())
	    entry->sortCols(value);
    } 
    else if (strcmp(keyword, "sort_order") == 0) {
	if (! entry->sortOrder())
	    entry->sortOrder(value);
    } 
    else if (strcmp(keyword, "show_cols") == 0) {
	if (! entry->showCols())
	    entry->showCols(value);
    }
    else if (strcmp(keyword, "copyright") == 0) {
	entry->copyright(value);
    } 
    else if (strcmp(keyword, "help") == 0) {
	entry->help(value);
    } 
    else if (strcmp(keyword, "id_col") == 0) {
	int id_col = undef_col_;
	if (sscanf(value, "%d", &id_col) == 1 && id_col != undef_col_)
	    entry->id_col(id_col);
    }
    else if (strcmp(keyword, "ra_col") == 0) {
	int ra_col = undef_col_;
	if (sscanf(value, "%d", &ra_col) == 1 && ra_col != undef_col_) 
	    entry->ra_col(ra_col);
    }
    else if (strcmp(keyword, "dec_col") == 0) {
	int dec_col = undef_col_;
	if (sscanf(value, "%d", &dec_col) == 1 && dec_col != undef_col_) 
	    entry->dec_col(dec_col);
    }
    else if (strcmp(keyword, "x_col") == 0) {
	int x_col = undef_col_;
	if (sscanf(value, "%d", &x_col) == 1 && x_col != undef_col_)
	    entry->x_col(x_col);
    }
    else if (strcmp(keyword, "y_col") == 0) {
	int y_col = undef_col_;
	if (sscanf(value, "%d", &y_col) == 1 && y_col != undef_col_) 
	    entry->y_col(y_col);
    }
    else if (strcmp(keyword, "is_tcs") == 0) {
	int is_tcs = 0;
	if (sscanf(value, "%d", &is_tcs) == 1)
	    entry->is_tcs(is_tcs);
    }
    if (strcmp(keyword, "equinox") == 0) {
	double d;
	if (sscanf(value, "%lf", &d) == 1)
	    entry->equinox(d);
    }
    return 0;
}


/*
 * report the error with the filename and line number included
 */
int CatalogInfo::cfg_error(const char* filename, int line, const char* msg1, const char* msg2)
{
    char buf[1024];
    STRSTD::ostrstream os(buf, sizeof(buf));
    os << "error in catalog config file: " 
       << filename << ": line " << line << ": " << msg1 << msg2 << ends;
    return error(buf);
}


/*
 * This static method is used to load the root catalog config info 
 * the first time through and return the error status (0 is OK, 
 * 1 for error). 
 */
int CatalogInfo::load() 
{
    if (entries_)
	delete entries_;

    entries_ = loadRootConfig();
    if (!entries_)
	return 1;		// error
    
    // add a link to the main ESO config URL, if it is not already there
    const char* longName = "ESO Catalogs";
    const char* shortName = "catalogs@eso";
    if (strcmp(entries_->url(), catlib_config_url_) != 0
	&& ! lookup(entries_, longName) 
	&& ! lookup(entries_, shortName)) {
	CatalogInfoEntry* e = new CatalogInfoEntry;
	e->servType("directory");
	e->url(catlib_config_url_);
	e->longName(longName);
	e->shortName(shortName);
	if (append(e) != 0)
	    return 1;		// error
    }
    return 0;
}


/*
 * This static method is used to reload the root catalog config info 
 * after it has been edited by hand, to make the new data available
 * in the application. Since pointers to the current entries may still
 * be in use, we need to update the existing entries.
 */
int CatalogInfo::reload() 
{
    // get default config
    CatalogInfoEntry* root = loadRootConfig();
    if (!root)
	return 1;		// error

    int status = reload(first(), root->link());
    delete root;
    return status;
}


/*
 * This static method is used to reload catalog config info after it has
 * been edited by hand, to make the new data available in the
 * application. Since pointers to the old entries may still be in use, we
 * need to update the old entries with the data from the new
 * entries.
 */
int CatalogInfo::reload(CatalogInfoEntry* oldEntry, CatalogInfoEntry* newEntry) 
{
    // merge into existing entries
    for (CatalogInfoEntry* ne = newEntry; ne != NULL; ne = ne->next()) {
	// see if we have this entry already, and if so, update it
	int found = 0;
	for (CatalogInfoEntry* e = oldEntry; e != NULL; e = e->next()) {
	    if (strcmp(e->longName(), ne->longName()) == 0 
		|| strcmp(e->shortName(), ne->shortName()) == 0) {
		if (e->link()) {  // follow opened catalog directory links
		    if (strcmp(ne->servType(), "directory") == 0) {
			if (load(ne) != 0 || reload(e->link(), ne->link()) != 0)
			    return 1; // error
		    }
		}
		// copy the data from the new entry to update the old one
		// and restore the catalog directory link, if any, since 
		// the update is recursive and follows directory links.
		CatalogInfoEntry* link = e->link();
		CatalogInfoEntry* next = e->next();
		*e = *ne;  // (see CatalogInfoEntry::operator=, no links copied)
		e->link(link);
		e->next(next);
		found++;
		break;
	    }
	}
	if (!found) {
	    // if this is a new entry, append a copy of it
	    // (see copy constructor: no links copied)
	    CatalogInfoEntry* copy = new CatalogInfoEntry(*ne);
	    oldEntry->append(copy);
	}
    }

    // Remove any "dead" entries (entries that were deleted from the config file).
    CatalogInfoEntry* e = oldEntry;
    while (e != NULL) {
	int found = 0;
	for (CatalogInfoEntry* ne = newEntry; ne != NULL; ne = ne->next()) {
	    if (strcmp(e->longName(), ne->longName()) == 0 
		|| strcmp(e->shortName(), ne->shortName()) == 0) {
		found++;
		break;
	    }
	}
	if (!found) {
	    // Entry not found in the new list, remove it
	    CatalogInfoEntry* next = e->next();
	    remove(e);
	    e = next;
	}
	else {
	    e = e->next();
	}
    }

    return 0;
}


/*
 * This static method is used to load the root catalog config info the
 * first time through and return a pointer to the root entry, or NULL
 * if there was an error.
 *
 * The root catalog config file is searched for in the following URL
 * locations:
 *
 *   $CATLIB_CONFIG
 *   $SKYCAT_CONFIG (for backward compat)
 *   ESO default URL
 *   hard coded default
 */
CatalogInfoEntry* CatalogInfo::loadRootConfig() 
{
    // look for a catalog config file in the standard places and make the
    // first one found the root of the catalog server tree

    CatalogInfoEntry* e = new CatalogInfoEntry;
    e->servType("directory");
    e->longName("Default Catalog List");
    e->shortName("default");

    // check the CATLIB_CONFIG environment variable
    char* url = getenv("CATLIB_CONFIG");
    if (url) {
	e->url(url);
	if (load(e) == 0)
	    return e;
    }

    // check the SKYCAT_CONFIG environment variable
    url = getenv("SKYCAT_CONFIG");
    if (url) {
	e->url(url);
	if (load(e) == 0)
	    return e;
    }

    // try the default URL 
    e->url(catlib_config_url_);
    if (load(e) == 0)
	return e;

    // if all else fails, use this hard coded config info
    e->url("default");
    STRSTD::istrstream is(config_info_);
    e->link(load(is));
    if (! e->link()) {
	delete e;
	return NULL; // error
    }

    return e;	// normal return
}


/*
 * This static method is used to load catalog config info and return the
 * error status (0 is OK, 1 for error).
 *
 * The argument should be a catalog entry of type "directory", where the
 * URL points to the catalog config file. There may be multiple such
 * entries in the form of a catalog server tree.
 */
int CatalogInfo::load(CatalogInfoEntry* e) {
    // loop through the url list until success
    HTTP http;
    int nlines = 0;
    char * s = http.get(e->url(), nlines);
    if (!s) 
	return 1; // http error

    const char* ctype = (http.content_type() ? http.content_type() : "");
    if (strcmp(ctype, "text/html") == 0) {
	// most likely an error message
	return http.html_error(s);
    }

    STRSTD::istrstream is(s);
    e->link(load(is, e->url()));
    if (! e->link()) 
	return 1;		// input error

    // if it is a local config file, allow URL commands
    if (strncmp(e->url(), "file:", 5) == 0)
	HTTP::allowUrlExec(1);
	    
    return 0;	// normal return
}


/*
 * return a pointer to the first config file entry
 * Note that entries_ points to the root of a hierarchical list
 * of catalog entries. Here we return the first item under the root.
 */
CatalogInfoEntry* CatalogInfo::first() {
    // load the config file the first time through
    if (!entries_)
	if (load() != 0)
	    return NULL;
    return entries_->link(); 
}


/*
 * return a pointer to the root config file entry.
 */
CatalogInfoEntry* CatalogInfo::root() {
    // load the config file the first time through
    if (!entries_)
	if (load() != 0)
	    return NULL;
    return entries_; 
}


/*
 * This static method returns a pointer to the catalog config entry for
 * the given catalog
 */
CatalogInfoEntry* CatalogInfo::lookup(const char* name)
{
    // load the config file the first time through
    if (!entries_)
	if (load() != 0)
	    return NULL;
    
    CatalogInfoEntry* e = lookup(entries_, name);
    if (e)
	return e;

    // if "name" is not a known catalog, it might be a local catalog in a file
    // in tab table format. 
    if (access(name, R_OK) == 0) 
	return lookupFile(name);
    
    error("unknown catalog name: ", name);
    return NULL;
}

/*
 * This static method returns a pointer to the catalog config entry for
 * the given catalog, starting the search with the given entry. 
 */
CatalogInfoEntry* CatalogInfo::lookup(CatalogInfoEntry* entry, const char* name)
{
    // search the top level list first, in case there are 2 catalogs with the
    // same name at different levels.
    CatalogInfoEntry* e;
    for (e = entry; e != NULL; e = e->next()) {
	if (strcmp(e->longName(), name) == 0 || strcmp(e->shortName(), name) == 0)
	    return e;
    }

    // check the sublists
    for (e = entry; e != NULL; e = e->next()) {
	if (e->link()) {
	     CatalogInfoEntry* p = lookup(e->link(), name);
	     if (p)
		 return p;
	}
    }

    return NULL;
}


/*
 * Make a catalog config entry for a local catalog.
 *
 * See if the given file has a catalog config entry in the header part
 * (we add this to the header of a local catalog in
 * QueryResult::printTableTop).  If no config info can be found in the
 * header, make a default catalog entry for it.  
 *
 * The return value is a pointer to the new entry or NULL for errors.
 */
CatalogInfoEntry* CatalogInfo::lookupFile(const char* name)
{
    ifstream is(name);
    if (!is) {
	sys_error("can't open file: ", name);
	return NULL;
    }

    CatalogInfoEntry* entry = new CatalogInfoEntry; 

    // add fields from the file header
    updateConfigEntry(is, entry);

    // add fixed entries
    entry->servType("local");
    entry->longName(name);
    entry->shortName(fileBasename(name));
    entry->url(name);

    if (append(entry) != 0) {
	delete entry;
	return NULL;
    }

    return (entry);
}


/* 
 * Append the given entry to the end of the top level catalog directory 
 * list
 */
int CatalogInfo::append(CatalogInfoEntry* e)
{
    // add to end of top level entry list, don't allow duplicates
    for (CatalogInfoEntry* p = first(); p != NULL; p = p->next()) {
	if (strcmp(p->longName(), e->longName()) == 0
	    || strcmp(p->shortName(), e->shortName()) == 0) {
	    // fprintf(stderr, "warning: duplicate entry in catalog list: %s (%s)\n", 
	    //	    e->longName(), e->shortName()); 
	    return 0;
	}
	if (!p->next()) {
	    p->next(e);
	    break;
	}
    }
    return 0;
}


/*
 * Remove the given entry from the catalog directory list.  It is removed
 * from the list but not deleted, as it might still be referenced
 * somewhere.
 */
int CatalogInfo::remove(CatalogInfoEntry* e)
{
    remove(e, entries_);
    return 0;
}


/*
 * Remove the given entry from the given catalog directory list. It is
 * removed from the list but not deleted, as it might still be referenced
 * somewhere.
 */
void CatalogInfo::remove(CatalogInfoEntry* e, CatalogInfoEntry* dir)
{
    if (e) {
	if (e == dir->link()) {  // first item in sublist
	    dir->link(e->next());
	    e->next(NULL);
	}
	else {
	    for (CatalogInfoEntry* p = dir->link(); p != NULL; p = p->next()) {
		if (p->next() == e) {
		    p->next(e->next());
		    e->next(NULL);
		    break;
		}
		if (p->link()) 
		    remove(e, p);
	    }
	}
    }
}


/*
 * Read config keyword entries from the given stream and update the given
 * entry values. 
 */
void CatalogInfo::updateConfigEntry(istream& is, CatalogInfoEntry* entry)
{
    if (! entry)
	return;

    char buf[1024];
    char* keyword;		// left of ':'
    char* value;		// right of ':'
    while (getline(is, buf, sizeof(buf))) {
	if (buf[0] == '-')
	    break;		// end of header

	// skip comments and empty lines
	if (buf[0] == '#' || strlen(buf) == 0)
	    continue;

	// split keyword : value
	if (split(buf, keyword, value) != 0) {
	    continue;
	}

	set_entry_value(entry, keyword, value, 1);
    }
}


/*
 * default constructor - initialize null keyword values amd set default
 * column values.
 */
CatalogInfoEntry::CatalogInfoEntry()
    : id_col_(undef_col_),
      ra_col_(undef_col_),
      dec_col_(undef_col_),
      x_col_(undef_col_),
      y_col_(undef_col_),
      is_tcs_(0),
      equinox_(2000.),
      link_(NULL),
      next_(NULL)
{
    for (int i = 0; i < NUM_KEY_STRINGS_; i++)
	val_[i] = NULL;
}


/* 
 * copy constructor
 */
CatalogInfoEntry::CatalogInfoEntry(const CatalogInfoEntry& e)
    : id_col_(e.id_col_),
      ra_col_(e.ra_col_),
      dec_col_(e.dec_col_),
      x_col_(e.x_col_),
      y_col_(e.y_col_),
      is_tcs_(e.is_tcs_),
      equinox_(e.equinox_),
      link_(NULL),  // no links or marks copied
      next_(NULL)
{
    for (int i = 0; i < NUM_KEY_STRINGS_; i++)
	val_[i] = e.val_[i] ? strdup(e.val_[i]) : (char*)NULL;
}


/* 
 * assignment operator 
 */
CatalogInfoEntry& CatalogInfoEntry::operator=(const CatalogInfoEntry& e)
{
    id_col_ = e.id_col_;
    ra_col_ = e.ra_col_;
    dec_col_ = e.dec_col_;
    x_col_ = e.x_col_;
    y_col_ = e.y_col_;
    is_tcs_ = e.is_tcs_;
    equinox_ = e.equinox_;
    // don't copy the links or marks

    for (int i = 0; i < NUM_KEY_STRINGS_; i++)
	val_[i] = e.val_[i] ? strdup(e.val_[i]) : (char*)NULL;
    
    return *this;
}


/*
 * destructor
 */
CatalogInfoEntry::~CatalogInfoEntry()
{
    if (link_)
	delete link_;

    if (next_)
	delete next_;

    for (int i = 0; i < NUM_KEY_STRINGS_; i++) {
	if (val_[i]) {
	    free(val_[i]);
	    val_[i] = NULL;
	}
    }
}


/*
 * check that all required fields have been set and return an error message
 * if any field is missing, otherwise NULL if there were no errors
 */
char* CatalogInfoEntry::check()
{
    if (longName() == NULL) 
	return "missing long_name";
    if (shortName() == NULL) 
	return "missing short_name";
    if (url() == NULL) 
	return "missing url";

    return NULL;			// OK
}


/*
 * Return the column number for the id, default to 0, or -1 if there
 * is no id column
 */
int CatalogInfoEntry::id_col() const 
{
    if (id_col_ == undef_col_) 
	return 0;
    return id_col_;
}


/*
 * Return the column number for the X coord, default to -1 (no X column)
 */
int CatalogInfoEntry::x_col() const 
{
    if (x_col_ == undef_col_) 
	return -1;
    return x_col_;
}


/*
 * Return the column number for the Y coord, default to -1 (no Y column)
 */
int CatalogInfoEntry::y_col() const 
{
    if (y_col_ == undef_col_) 
	return -1;
    return y_col_;
}

/*
 * Return the column number for ra, default to 1 if there is no
 * X column defined, or -1 if there is no ra column.
 */
int CatalogInfoEntry::ra_col() const 
{
    if (ra_col_ == undef_col_) {
	if (x_col_ == undef_col_) 
	    return 1;
	return -1;
    }
    return ra_col_;
}


/*
 * Return the column number for dec, default to 2 if there is no
 * Y column defined, or -1 if there is no dec column.
 */
int CatalogInfoEntry::dec_col() const 
{
    if (dec_col_ == undef_col_) {
	if (y_col_ == undef_col_) 
	    return 2;
	return -1;
    }
    return dec_col_;
}


/*
 * set the value for a config keyword
 */
void CatalogInfoEntry::setVal_(KeyStrings keyword, const char* s)
{
    if (val_[keyword]) {
	free(val_[keyword]);
	val_[keyword] = NULL;
    }

    if (s && strlen(s)) {
	val_[keyword] = strdup(s);
    }
}
    

/*
 * append the given entry to the end of the list and return an error
 * if there was already an entryt with the same name there.
 */
int CatalogInfoEntry::append(CatalogInfoEntry* e)
{
    // add to end of top level entry list
    for (CatalogInfoEntry* p = this; p != NULL; p = p->next_) {
	if (strcmp(p->longName(), e->longName()) == 0
	    || strcmp(p->shortName(), e->shortName()) == 0) {
	    // fprintf(stderr, "warning: duplicate entry in catalog list: %s (%s)\n", 
	    //	    e->longName(), e->shortName()); 
	    return 0;
	}
	if (!p->next_) {
	    p->next_ = e;
	    break;
	}
    }
    return 0;
}


/*
 * output operator: format similar to config file input:
 * keyword: value ...
 */
ostream& operator<<(ostream& os, const CatalogInfoEntry& e)
{
    if (e.servType())
	os << "serv_type: " << e.servType() << endl;

    if (e.longName())
	os << "long_name: " << e.longName() << endl;


    if (e.shortName())
	os << "short_name: " << e.shortName() << endl;

    if (e.url())
	os << "url: " << e.url() << endl;

    if (e.backup1())
	os << "backup1: " << e.backup1() << endl;

    if (e.backup2())
	os << "backup2: " << e.backup1() << endl;
    
    if (e.symbol())
	os << "symbol: " << e.symbol() << endl;
    
    if (e.searchCols())
	os << "search_cols: " << e.searchCols() << endl;
    
    if (e.sortCols())
	os << "sort_cols: " << e.sortCols() << endl;
    
    if (e.showCols())
	os << "show_cols: " << e.showCols() << endl;
    
    if (e.copyright())
	os << "copyright: " << e.copyright() << endl;

    if (e.help())
	os << "help: " << e.help() << endl;
    
    if (e.equinox() != 2000.)
	os << "equinox: " << e.equinox() << endl;

    if (e.id_col() > 0)
	os << "id_col: " << e.id_col() << endl;

    // PWD: do need to output default order of: id, ra, dec, x and y.
    if (e.ra_col() >= 0 )  
	os << "ra_col: " << e.ra_col() << endl;
    
    if (e.dec_col() >= 0 )
	os << "dec_col: " << e.dec_col() << endl;

    if (e.x_col() >= 0)
	os << "x_col: " << e.x_col() << endl;

    if (e.y_col() >= 0)
	os << "y_col: " << e.y_col() << endl;

    if (e.is_tcs())
	os << "is_tcs: " << e.is_tcs() << endl;

    return os;
}
