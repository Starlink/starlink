// -*-c++-*-
#ifndef _CatalogInfo_h_
#define _CatalogInfo_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: CatalogInfo.h,v 1.21 1998/07/28 21:22:37 abrighto Exp $
 *
 * CatalogInfo.h - class holding catalog config information
 *                 from the Catalog.cfg file
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  29 Sep 95  Created
 */


#include <iostream.h>
#include <stdio.h>


// forward ref
class CatalogInfoEntry;

/* 
 * This class manages the catalog config file info. Many of the methods
 * are static, since the information needs to be cached and shared by
 * other classes.
 */
class CatalogInfo {
private:
     // handle for catalog config info for static access
    static CatalogInfo* catInfo_;
    
    // hierarchical list of all catalog entries
    static CatalogInfoEntry* entries_;
    
    // for error reporting
    static int cfg_error(const char* filename, int line, const char* msg1, 
			 const char* msg2 = "");

    // load the root config file
    static CatalogInfoEntry* loadRootConfig();

    // Set the value for the given keyword in the given entry
    static int set_entry_value(CatalogInfoEntry* entry, 
			       const char* keyword, 
			       const char* value,
			       int updateFlag);
 
    // read a line from the stream into buf and return the stream. 
    // Lines ending with backslash are continued on the next line
    static istream& getline(istream& f, char* buf, int size);

    // update the old catalog entry with the info from the new one (recursive)
    static int reload(CatalogInfoEntry* oldEntry, CatalogInfoEntry* newEntry);

    // Remove the given entry from the given catalog directory list. 
    static void remove(CatalogInfoEntry* e, CatalogInfoEntry* dir);

public:
    // constructor
    CatalogInfo() {}

    // load the default catalog config file
    static int load();

    // reload the default catalog config file after it has been edited by hand
    // and update recursively any already opened catalog directories
    static int reload();

    // load config file info from the given stream (filename for error reporting)
    static CatalogInfoEntry* load(istream&, const char* filename = "internal");

    // load a catalog config file
    static int load(CatalogInfoEntry*);

    // return a pointer to the catalog config file entry for the given catalog
    static CatalogInfoEntry* lookup(const char* catalogName);

    // as above, but starting the search with the given entry rather than at the root 
    static CatalogInfoEntry* lookup(CatalogInfoEntry* entry, const char* name);

    // get config entry for a local catalog from the header
    static CatalogInfoEntry* lookupFile(const char* catalogFileName);

    // Read config keyword entries from the given stream and update the given
    // entry values  
    static void updateConfigEntry(istream& is, CatalogInfoEntry* entry);

    // Append the given entry to the end of the main catalog list
    static int append(CatalogInfoEntry* e);

    // Remove the given entry from the catalog list
    static int remove(CatalogInfoEntry* e);

    // return a pointer to the first config file entry under the root entry
    static CatalogInfoEntry* first();

    // return a pointer to the root config file entry
    static CatalogInfoEntry* root();
};


/* 
 * one of these is kept in a list for each catalog entry
 * in the config file
 */
class CatalogInfoEntry {
friend class CatalogInfo;
private:
    // This enum defines one keyword for each of the config keyword entries
    // that have string values and is used to index an array of strings 
    // representing the keyword values.
    enum KeyStrings {
	SERVTYPE_,		// service type (catalog, namesvr, imagesvr, ...)
	LONGNAME_,		// long name for display
	SHORTNAME_,		// short catalog name
	URL_,			// http url to use, with wildcards %ra, %dec, ...
	BACKUP1_,		// backup URL (1) 
	BACKUP2_,		// backup URL (2) 
	SYMBOL_,		// plot symbol info
	SEARCH_COLS,		// list of searchable column info: colname minLabel maxLabel, ...
	SORT_COLS,		// list of columns to sort by
	SORT_ORDER,		// "increasing" or "decreasing"
	SHOW_COLS,		// list of columns to display (default: all)
	COPYRIGHT_,		// copyright notice for server
	HELP_,		        // URL pointing to help page fpr catalog

	NUM_KEY_STRINGS_	// dummy last entry, number of keywords
    };
    // array of values for config file keywords, indexed by above enum
    char* val_[NUM_KEY_STRINGS_];

    // integer keyword values
    int id_col_;		// column containing object id
    int ra_col_;		// RA column
    int dec_col_;		// DEC
    int x_col_;			// instead of RA, can use pixel coords X,Y
    int y_col_;
    int is_tcs_;		// flag: true if using TCS columns

    // double keyword values
    double equinox_;		// equinox of wcs coords (default: J2000)

    CatalogInfoEntry* link_;	// If the url is a catalog config file or URL
                                // this points to the first entry in that list.
				// (i.e.: used for a directory entry, if loaded). 

    CatalogInfoEntry* next_;	// next pointer for linked list of entries

    // set the value for a config keyword
    void setVal_(KeyStrings keyword, const char* s);
    
public:
    // default constructor
    CatalogInfoEntry();

    // copy constructor
    CatalogInfoEntry(const CatalogInfoEntry&);
    
    // assignment
    CatalogInfoEntry& operator=(const CatalogInfoEntry&);

    // destructor
    ~CatalogInfoEntry();
    
    // check that all fields have been set and return 0 if all ok
    char* check();

    // set string keyword values
    void servType(const char* s)  {setVal_(SERVTYPE_, s);}
    void longName(const char* s)  {setVal_(LONGNAME_, s);}
    void shortName(const char* s) {setVal_(SHORTNAME_, s);}
    void url(const char* s)       {setVal_(URL_, s);}
    void backup1(const char* s)   {setVal_(BACKUP1_, s);}
    void backup2(const char* s)   {setVal_(BACKUP2_, s);}
    void symbol(const char* s)    {setVal_(SYMBOL_, s);}
    void searchCols(const char* s){setVal_(SEARCH_COLS, s);}
    void sortCols(const char* s)  {setVal_(SORT_COLS, s);}
    void sortOrder(const char* s)  {setVal_(SORT_ORDER, s);}
    void showCols(const char* s)  {setVal_(SHOW_COLS, s);}
    void copyright(const char* s) {setVal_(COPYRIGHT_, s);}
    void help(const char* s)      {setVal_(HELP_, s);}

    // set int keyword values
    void id_col(int i)  {id_col_  = i;}
    void ra_col(int i)  {ra_col_  = i;}
    void dec_col(int i) {dec_col_ = i;}
    void x_col(int i)   {x_col_   = i;}
    void y_col(int i)   {y_col_   = i;}
    void is_tcs(int i)  {is_tcs_  = i;}

    // set double keyword values
    void equinox(double d)  {equinox_  = d;}

    // get string keyword values
    const char* servType() const  {return val_[SERVTYPE_];}
    const char* longName() const  {return val_[LONGNAME_];}
    const char* shortName() const {return val_[SHORTNAME_];}
    const char* url() const       {return val_[URL_];}
    const char* backup1() const   {return val_[BACKUP1_];}
    const char* backup2() const   {return val_[BACKUP2_];}
    const char* symbol() const    {return val_[SYMBOL_];}
    const char* searchCols() const {return val_[SEARCH_COLS];}
    const char* sortCols() const  {return val_[SORT_COLS];}
    const char* sortOrder() const {return val_[SORT_ORDER];}
    const char* showCols() const  {return val_[SHOW_COLS];}
    const char* copyright() const {return val_[COPYRIGHT_];}
    const char* help() const      {return val_[HELP_];}

    // get int keyword values
    int id_col() const;
    int ra_col()  const;
    int dec_col() const;
    int x_col() const;
    int y_col() const;
    int is_tcs() const {return is_tcs_;}

    // get double keyword values
    double equinox() const {return equinox_;}
    
    // return true if the catalog uses word coordinates
    int isWcs() {return ra_col() >= 0 && dec_col() >= 0;}

    // return true if the catalog uses image pixel coordinates
    int isPix() {return x_col() >= 0 && y_col() >= 0;}

    // set/get pointer to link entry
    CatalogInfoEntry* link() const {return link_;}
    void link(CatalogInfoEntry*e) {link_ = e;}

    // append the given entry to the end of the list
    int append(CatalogInfoEntry* e);

    // set/get pointer to next entry
    CatalogInfoEntry* next() const {return next_;}
    void next(CatalogInfoEntry*e) {next_ = e;}

    // output operator (output in format similar to input config file)
    friend ostream& operator<<(ostream&, const CatalogInfoEntry&);
};


#endif /* _CatalogInfo_h_ */
