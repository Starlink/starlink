/*
 *  This file is part of moggy.
 *
 *  Copyright 2001, Council for the Central Laboratory of the Research Councils
 *
 *  This program is part of the Starlink Software Distribution: see
 *  http://www.starlink.ac.uk
 *
 *  moggy is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  moggy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with moggy; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  The General Public License is distributed along with this
 *  program in the file LICENCE.
 *
 *  Author: Norman Gray <norman@astro.gla.ac.uk>
 *  $Id$
 */


#ifndef CATALOGUEHANDLER_H_LOADED
#define CATALOGUEHANDLER_H_LOADED 1

#include <vector>
#include <string>
#include <assert.h>

#if STD_IN_STD_NAMESPACE
using std::vector;
#endif

#include <cat/WorldCoords.h>
#include <cat/AstroQuery.h>
#include <cat/QueryResult.h>
#include <cat/AstroCatalog.h>

#include "moggy.h"		/* For MoggyException */
#include "verbosity.h"

class CatalogueHandler {
 public:
    CatalogueHandler ();
    ~CatalogueHandler ();

    /* All the set... methods return true if the operation completed
       successfully */
    /* In the case of setCatname, the operation should also check that
       the catalogue is a valid one */
    bool setCatname (string name);

    /* Set position 1 or 2 (given by argument num) either in
       H:M:S,D:M:S or in decimal degrees */
    bool setPos (int num,
		 int rah,  int ramin,  double rasec,
		 int degh, int degmin, double degsec,
		 double equinox=2000.0);
    bool setPos (int num,
		 double radeg,
		 double decdeg,
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

    static void verbosity (const verbosities level) { verbosity_ = level; }

    class CatalogueRow {
    public:
	CatalogueRow (const CatalogueHandler& parent, const int rownum)
	    : parent_(parent), num_(rownum) { }
	string id()  const throw (MoggyException);
	double ra()  const throw (MoggyException);
	double dec() const throw (MoggyException);
	double mag() const throw (MoggyException);
	/* Send all the columns of the row to the given stream: used by
	   operator<<(ostream&,CatalogueRow&).  There's no way of
	   selecting what columns to list, here.  If you want that,
	   then you have to use the ra(), dec(), etc methods from the
	   caller. */
	ostream& put (ostream&) const;
    private:
	const CatalogueHandler& parent_;
	const int num_;

	/* Class variable to set column separator.  There's currently
	   no method to set this. */
	static string sep_;
    };
    friend class CatalogueRow;	/* so we can look at parent_'s private
				   variables */
    CatalogueRow* getrow (int n) { return new CatalogueRow (*this, n); }
    /* Return the list of column names in the retrieved catalogue */
    vector<string> getColnames () const;

    /* typedef string* iterator; */
    class const_iterator {
    public:
	/* idx_==-1 indicates end-of-list */
	const_iterator ()	/* for end() */
	    : idx_(-1) { }
	const_iterator (CatalogueHandler* par)
	    : idx_(0), parent_(par) { }
	const_iterator& operator++() throw (MoggyException);
	/* string& operator*() throw (MoggyException); */
	const CatalogueRow operator*() throw (MoggyException);
	bool operator==(const const_iterator& it) const
	    { return idx_==it.idx_; }
	bool operator!=(const const_iterator& it) const
	    { return idx_!=it.idx_; }
    private:
	int idx_;
	const CatalogueHandler* parent_;
	//string iterLine_;
    };
    const_iterator begin() throw (MoggyException);
    const_iterator end() const { return const_iterator(); }
    void checkQueryStatus_ () const throw (MoggyException);
    friend class const_iterator;

    /* Functions to return the columns corresponding to the `common'
       fields requested.  The fields themselves can be returned using
       the ra(), dec() fields of CatalogueRow; these methods therefore
       aren't terribly useful except via the has_ra(), has_dec()
       methods.  These differ from the QueryResult and AstroCatalog
       *_col() methods since they raise an exception if they are
       called before the catalogue is defined, since this is the
       result of calling things out of order.  Given that the
       catalogue _is_ defined, they return <0 if there is no such
       column in the catalogue. */
    int id_col() const {
	if (!isValid_(CATNAME)) throw MoggyException ("id_col() out of order");
	return cat_->id_col();
    }
    int ra_col() const {
	if (!isValid_(CATNAME)) throw MoggyException ("ra_col() out of order");
	return cat_->ra_col();
    }
    int dec_col() const {
	if (!isValid_(CATNAME)) throw MoggyException ("dec_col() out of order");
	return cat_->dec_col();
    }
    int mag_col() const throw (MoggyException);
    vector<int>* mag_cols () const throw (MoggyException);
    bool has_id() const { return id_col() >= 0; }
    bool has_ra() const { return ra_col() >= 0; }
    bool has_dec() const { return dec_col() >= 0; }
    bool has_mag() const { return mag_col() >= 0; }


 private:

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
		      RESULT=64 /* Query result, queryResult_ */
    };
    fieldflags valid_;

    AstroCatalog* cat_;
    QueryResult queryResult_;

    inline bool isValid_ (fieldflags flag)
	const { return (valid_ & flag) == flag; }
    inline void setValid_ (fieldflags flag) { valid_ = fieldflags( valid_|flag); }
    /* The following one produces a -Wall warning from g++ */
    /* inline void setValid_ (fieldflags flag) { valid_ |= flag; } */

    static verbosities verbosity_;
};

ostream& operator<< (ostream& o, const CatalogueHandler::CatalogueRow& t);


#endif /* CATALOGUEHANDLER_H_LOADED */
