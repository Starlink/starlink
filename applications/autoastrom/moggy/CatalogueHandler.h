/* Part of moggy
 * Copyright 2001 Council for the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 *
 * $Id$
 */


#ifndef CATALOGUEHANDLER_H_LOADED
#define CATALOGUEHANDLER_H_LOADED 1

#include <vector>
#include <string>

#include <cat/WorldCoords.h>
#include <cat/AstroQuery.h>
#include <cat/QueryResult.h>
#include <cat/AstroCatalog.h>

#include "moggy.h"		/* For MoggyException */

class CatalogueHandler {
 public:
    CatalogueHandler ();
    ~CatalogueHandler ();

    /* All the set... methods return true if the operation completed
       successfully */
    /* In the case of setCatname, the operation should also check that 
       the catalogue is a valid one */
    bool setCatname (string name);
    bool setPos (int num,
		 int rah,  int ramin,  double rasec,
		 int degh, int degmin, double degsec,
		 double equinox=2000.0);
    bool setRadius (double radius);
    bool setSearchtype (string type);
    bool setNrows (int n);
    bool setConfig (string);
    /* setResultCols: set which columns are to be returned.  The
       default is "SIMPLE", which returns the standard id,ra,dec, and
       the only current alternative is "ALL", which returns all the
       columns available.  In future, I might allow this to be a
       space-separated list of column names (but what if some of the
       columns don't exist?).  Perhaps this call could simultaneously
       set the column separator?  Return false if there was a problem. */
    bool setResultCols (string cols = "SIMPLE");

    /* Do the search.  Returns negative if not all required
       information was present.  If all the information is present, it 
       does the query and returns the number of rows obtained.
       Should I distinguish a successful search which returns zero
       rows from a search which has some other problem? */
    int doSearch () throw (MoggyException);

    void printStatus (ostream& o, string lineend);

    /* typedef string* iterator; */
    class const_iterator {
    public:
	/* idx_==-1 indicates end-of-list */
	const_iterator ()	/* for end() */
	    : idx_(-1), iterSeparator_(" ") { }
	const_iterator (CatalogueHandler* par)
	    : idx_(0), parent_(par), iterSeparator_(" ") { }
	const_iterator& operator++() throw (MoggyException);
	string& operator*() throw (MoggyException);
	bool operator==(const const_iterator& it) const
	    { return idx_==it.idx_; }
	bool operator!=(const const_iterator& it) const
	    { return idx_!=it.idx_; }
    private:
	int idx_;
	//const QueryResult& res_;
	const CatalogueHandler* parent_;
	string iterSeparator_;
	string iterLine_;
    };
    const_iterator begin() throw (MoggyException);
    const_iterator end() const { return const_iterator(); }
    void checkQueryStatus_ () const throw (MoggyException);
    friend class const_iterator;


    /* Public inspector methods */
    vector<string> getColnames () const;

    /* The following inspector methods possibly aren't needed, and
       they pretty definitely don't need to be public. */
 private:
    /* string get methods format results for readability, not
       parseability, and will indicate that parameters are unset by
       generating a suggestive text rather than failing. */
    string getPos (int num) const throw (MoggyException);
    double getRadius () const { return (isValid_(RADIUS) ? radius_ : 0.0); }
    string getSearchtype () const;
    string getCatname () const {
	return (isValid_ (CATNAME) ? catname_ : "<UNSET>");
    }
    int getNrows () const { return (isValid_ (NROWS) ? nrows_ : 0); }
    string getConfig () const;


    enum
    {
	UNSPECIFIED, BOXSEARCH, RADIUSSEARCH, RADIUSPOINTSEARCH
    } searchType_;
    string catname_;
    WorldCoords pos_[2];
    double radius_;
    int nrows_;
    double equinox_;
    enum fieldflags { CATNAME=1, /* catalogue name, catname_ and cat_ */
		      POS1=2,	/* first position, pos_[0] */
		      POS2=4,	/* second position, pos_[1] */
		      RADIUS=8,	/* search radius, radius_ */
		      SEARCHTYPE=16, /* search type, searchType_ */
		      NROWS=32, /* number of rows to return, nrows_ */
		      RESULT=64, /* Query result, queryResult_ */
    };
    fieldflags valid_;

    AstroCatalog* cat_;
    QueryResult queryResult_;

    /* returnCols_ is either 0 or an array of integer column numbers,
       terminated by a negative number.  If 0, then return all
       columns, otherwise, return only the listed column numbers. */
    int *returnCols_;

    inline bool isValid_ (fieldflags flag) const { return valid_ & flag; }
    inline void setValid_ (fieldflags flag) { valid_ = fieldflags( valid_|flag); }
    /* The following one produces a -Wall warning from g++ */
    /* inline void setValid_ (fieldflags flag) { valid_ |= flag; } */
};

#endif /* CATALOGUEHANDLER_H_LOADED */
