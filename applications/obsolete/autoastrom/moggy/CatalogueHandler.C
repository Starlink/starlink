//  This file is part of moggy.
//
//  Copyright 2001, Council for the Central Laboratory of the Research Councils
//
//  This program is part of the Starlink Software Distribution: see
//  http://www.starlink.ac.uk
//
//  moggy is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  moggy is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with moggy; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  The General Public License is distributed along with this
//  program in the file LICENCE.
//
//  Author: Norman Gray <norman@astro.gla.ac.uk>
//  $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if HAVE_CSTD_INCLUDE
#include <cstdlib>		// for setenv
#else
#include <stdlib.h>
#endif /* HAVE_CSTD_INCLUDE */

#if HAVE_SETENV
#if !DECLARE_SETENV  /* function present, but not declared in stdlib.h */
extern "C" int setenv(const char *name, const char *value, int overwrite);
#endif /* !DECLARE_SETENV */
#endif /* HAVE_SETENV */

#include "stringstream.h"

#include "cat/fitshead.h"

#include "CatalogueHandler.h"
#include "util.h"

#include <cat/error.h>		// for set_error_handler
void skycatErrorHandler_ (const char* msg) throw (MoggyException);

// Class variables
verbosities CatalogueHandler::verbosity_ = normal;
string CatalogueHandler::CatalogueRow::sep_ = " ";


CatalogueHandler::CatalogueHandler ()
        : searchType_(UNSPECIFIED), nrows_(0), equinox_(2000.0),
      valid_(fieldflags(0))
{
    // Set the skycat error handler
    set_error_handler (&skycatErrorHandler_);

    nrows_ = 50;		// Guess at a suitable default
    setValid_(NROWS);
}

CatalogueHandler::~CatalogueHandler ()
{
}

// Do the search.  Returns negative if not all required
// information was present.  If all the information is present, it
// does the query and returns the number of rows obtained.
// Should I distinguish a successful search which returns zero
// rows from a search which has some other problem?
int CatalogueHandler::doSearch ()
    throw (MoggyException)
{
    if (! isValid_(fieldflags(SEARCHTYPE | CATNAME | POS1 | NROWS)))
	return -1;

    assert (cat_ != 0);

    int rowsreturned = 0;

    // Set the sort column(s).
    vector<int>* magcols = mag_cols();
    const char **sortcols = 0;	// zero means don't sort
    if (magcols != 0)
    {
	// I'm rather uncertain about this -- the documentation and the
	// AstroQuery.h header seem divergent.  Where they differ,
	// this follows the header, and comments there.
	int nmagcols = magcols->size();
	sortcols = new const char*[nmagcols];
	char **colnames = cat_->colNames();
	for (int i=0; i<nmagcols; i++)
	    sortcols[i] = colnames[(*magcols)[i]];

	if (verbosity_ > normal)
	{
	    Util::logstream() << "CatalogueHandler::doSearch: sortcols("
		 << nmagcols << ") =";
	    for (int i=0; i<nmagcols; i++)
		Util::logstream() << ' ' << (*magcols)[i] << '=' << sortcols[i];
	    Util::logstream() << endl;
	}
    }

    if (searchType_ == RADIUSSEARCH || searchType_ == RADIUSPOINTSEARCH)
    {
	AstroQuery q;
	Util::logstream() << "doSearch: radial search" << endl;

	if (searchType_ == RADIUSSEARCH)
	    if (isValid_(RADIUS))
		q.radius(0, radius_);
	    else
		return -1;
	else if (searchType_ == RADIUSPOINTSEARCH)
	    if (isValid_(POS2))
	    {
		radius_ = pos_[0].dist(pos_[1]);
		q.radius(0,radius_);
	    }
	    else
		return -1;
	else
	    throw MoggyException ("unexpected searchType");

	q.pos(pos_[0]);
	q.maxRows(nrows_);

	// Set the sort column.
	if (sortcols != 0)
	{
	    q.sort (magcols->size(), const_cast<char **>(sortcols));
	    q.sortOrder (+1);
	}

	rowsreturned = cat_->query(q, 0, queryResult_);
	setValid_(RESULT);
    }
    else if (searchType_ == BOXSEARCH)
    {
	if (! isValid_(POS2))
	    return -1;

	// Search the contents of a box, using the standard query
	// method, rather than getArea(), which doesn't allow one to
	// sort by magnitude

	AstroQuery q;

	q.pos(pos_[0], pos_[1]);
	q.maxRows(nrows_);

	// Set the sort column.
	if (sortcols != 0)
	{
	    q.sort (magcols->size(), const_cast<char **>(sortcols));
	    q.sortOrder (+1);
	}
	rowsreturned = cat_->query(q, 0, queryResult_);
	setValid_(RESULT);
    }
    else
	// can't do those searches, yet
	return -1;

    // Tidy away the sortcols and magcols arrays
    if (sortcols != 0)
	delete[] (sortcols);
    if (magcols != 0)
	delete (magcols);

    return rowsreturned;
}

CatalogueHandler::const_iterator CatalogueHandler::begin()
    throw (MoggyException)
{
    checkQueryStatus_();

    if (queryResult_.numRows() == 0)
	// nothing to return
	return end();
    else
	return const_iterator(this);
}

const CatalogueHandler::CatalogueRow
	CatalogueHandler::const_iterator::operator*()
    throw (MoggyException)
{
    parent_->checkQueryStatus_();
    if (idx_ < 0 || idx_ >= parent_->queryResult_.numRows())
    {
	SSTREAM sb;
	sb << "iterator out of bounds in *: " << idx_ << '/'
	   << parent_->queryResult_.numRows();
	throw MoggyException (SS_STRING(sb));
    }

    if (verbosity_ > normal)
	Util::logstream() << "CatalogueHandler::* idx_=" << idx_
	     << '/' << parent_->queryResult_.numRows() << endl;

    return CatalogueRow (*parent_, idx_);
}


// const_iterator::operator++ : increment the iterator.  When we run
// out of rows, set the idx_ to -1, as this compares equal to end().
CatalogueHandler::const_iterator&
	CatalogueHandler::const_iterator::operator++()
    throw (MoggyException)
{
    parent_->checkQueryStatus_();
    if (idx_ < 0 || idx_ >= parent_->queryResult_.numRows())
    {
	SSTREAM sb;
	sb << "iterator out of bounds in ++: " << idx_ << '/'
	   << parent_->queryResult_.numRows();
	throw MoggyException (SS_STRING(sb));
    }

    ++idx_;

    // Are we at the end yet?  If so, set the idx_ to be -1, which
    // matches the state of the iterator which is returned by end(),
    // as tested in operator==.
    if (idx_ >= parent_->queryResult_.numRows())
	idx_ = -1;


    return *this;
}

// Helper function to check the status of the query, and throw a
// MoggyException if they've been violated somehow.  Doesn't check the
// iterator's precise status (because it's not a member of the
// iterator class).
//
// This can signal errors _only_ by throwing an exception, since this
// check will typically be done while the caller is working its way
// through the iterator, so will rather inevitably cause some
// confusion unless the exception is caught and handled cunningly.
// This will, in any case, throw an exception only as a result of
// a coding bug, and not (!) as a result of any user action.
void CatalogueHandler::checkQueryStatus_()
    const
    throw (MoggyException)
{
    if (! isValid_(RESULT))
	throw MoggyException ("iterator called before query result");

    // Note that having zero columns is an error, but having zero rows
    // isn't (from our point of view).
    if (queryResult_.numCols() <= 0 || queryResult_.numRows() < 0)
    {
	SSTREAM sb;
	sb << "Odd: query apparently successful, but has rows/cols="
	   << queryResult_.numRows()
	   << '/'
	   << queryResult_.numCols();
	throw MoggyException (SS_STRING(sb));
    }

    return;
}

bool CatalogueHandler::setPos (int num,
			       double radeg,
			       double decdeg,
			       double equinox)
{
    if (num < 1 || num > 2)
	return false;

    int posindex = num-1;
    equinox_ = equinox;
    pos_[posindex] = WorldCoords (radeg, decdeg, equinox_);
    // Status is 0 for OK
    if (pos_[posindex].status()==0)
    {
	if (verbosity_ > normal)
	    Util::logstream() << "CatalogueHandler::setPos(" << num << "): ("
		 << radeg << ", " << decdeg << " [" << equinox_ << "]) -->"
		 << endl
		 << "    = ("
		 << pos_[posindex].ra().val()*15
		 << " , "
		 << pos_[posindex].dec().val()
		 << ')' << endl;

	setValid_(num==1 ? POS1 : POS2);
	return true;
    }
    else
    {
	if (verbosity_ > normal)
	    Util::logstream() << "CatalogueHandler::setPos(dec): (" << num
		 << ") failed" << endl;
	return false;
    }
}

bool CatalogueHandler::setPos (int num,
			       int rah,  int ramin,  double rasec,
			       int degh, int degmin, double degsec,
			       double equinox)
{
    if (num < 1 || num > 2)
	return false;

    int posindex = num-1;
    equinox_ = equinox;
    pos_[posindex] = WorldCoords (rah,  ramin,  rasec,
				  degh, degmin, degsec,
				  equinox_);
    // Status is 0 for OK
    if (pos_[posindex].status()==0)
    {
	if (verbosity_ > normal)
	    Util::logstream() << "CatalogueHandler::setPos(" << num << "): ("
		 << rah << ',' << ramin << ',' << rasec << ", "
		 << degh << ',' << degmin << ',' << degsec
		 << " [" << equinox_ << "]) --> " << endl
		 << "    = ("
		 << pos_[posindex].ra().val()*15
		 << " , "
		 << pos_[posindex].dec().val()
		 << ')' << endl;

	setValid_(num==1 ? POS1 : POS2);
	return true;
    }
    else
    {
	if (verbosity_ > normal)
	    Util::logstream() << "CatalogueHandler::setPos(hmsdms): (" << num
		 << ") failed" << endl;
	return false;
    }
}

bool CatalogueHandler::setRadius (double radius)
{
    if (radius <= 0.0)
	return false;
    else
    {
	radius_ = radius;
	setValid_ (RADIUS);
	return true;
    }
}

bool CatalogueHandler::setNrows (int n)
{
    if (n <= 0)
	return false;
    else
    {
	nrows_ = n;
	setValid_ (NROWS);
	return true;
    }
}

bool CatalogueHandler::setSearchtype (string type)
{
    Util::uppercaseString (type);

    if (type.compare("BOX") == 0) searchType_ = BOXSEARCH;
    else if (type.compare("RADIUS") == 0) searchType_ = RADIUSSEARCH;
    else if (type.compare("RADIUS2") == 0) searchType_ = RADIUSPOINTSEARCH;
    else return false;

    setValid_(SEARCHTYPE);

    return true;
}

bool CatalogueHandler::setConfig (string URL)
{
#if HAVE_SETENV
    return (setenv ("CATLIB_CONFIG", URL.c_str(), 1) == 0);
#elif HAVE_PUTENV
    // Without setenv(), we have to resort to putenv, which is a mess.
    // The string passed to putenv is retained, so it can't be an
    // automatic variable.  Allocate the correct amount of space.  If
    // this static pointer is already allocated, then it's because
    // we've been here before; so deallocate it.
    static char *putenvarg = 0;
    const char *urlstr = URL.c_str();
    if (putenvarg != 0)		// been here before
    {
	free (putenvarg);
	// There's the _possibility_ of a race condition here, until
	// we call putenv(), but it's absurdly slim....
    }
    putenvarg = static_cast<char *>(malloc(strlen("CATLIB_CONFIG=")
					   +strlen(urlstr)
					   +1));
    if (putenvarg == 0)
	return false;

    sprintf (putenvarg, "CATLIB_CONFIG=%s", urlstr);
    return (putenv (putenvarg) == 0);
#else
#error "This is ridiculous -- we don't have either setenv() or putenv()"
#endif
}


bool CatalogueHandler::setCatname (string name)
{
    // Must also check here, that the catalogue name is a valid one.
    catname_ = name;
    try
    {
	cat_ = AstroCatalog::open (name.c_str());
	if (cat_)
	{
	    setValid_(CATNAME);

	    return true;
	}
	else
	    return false;
    }
    catch (MoggyException& e)
    {
	// This error will be `unknown catalog', raised by the error handler
	if (verbosity_ > normal)
	    // log the error to stderr
	    Util::logstream() << e.msg << endl;
	return false;
    }
}

vector<string> CatalogueHandler::getColnames () const
{
    vector<string> colnames;

    if (isValid_(CATNAME))
	for (int i=0; i<cat_->numCols(); i++)
	    colnames.push_back(cat_->colName(i));
    // if RESULT isn't valid, then a vector of length zero will be returned

    return colnames;
}

vector<int>* CatalogueHandler::mag_cols()
    const
    throw (MoggyException)
{
    if (! isValid_(CATNAME))
	throw MoggyException ("mag_col() out of order");

    vector<int>* colnums = new vector<int>(0);
    // Start with zero elements, so that push_back, below, is the only
    // thing adding elements to the vector.

    // Try to find out which columns are magnitude columns.  There can
    // be more than one of these, since there can be, for example, a
    // r_mag and a b_mag column.
    //
    // There's no standardisation mentioned in the documentation, so
    // the best we can do, I think, is search through the list of
    // column titles finding those which include a substring `MAG'.
    // PWD: but not "IMAGE" (which is often an x/y coordinate).
    for (int col=0; col<cat_->numCols(); col++)
    {
	string ucol(cat_->colName(col));
	Util::uppercaseString(ucol);
	if (ucol.find("MAG") != string::npos &&
            ucol.find("IMAGE") == string::npos)
	    colnums->push_back(col);
    }
    if (verbosity_ > normal)
    {
	Util::logstream() << "CatalogueHandler::mag_cols: "
	     << colnums->size() << " mag cols=";
	for (vector<int>::const_iterator p = colnums->begin();
	     p != colnums->end();
	     p++)
	    Util::logstream() << ' ' << *p;
	Util::logstream() << endl;
    }
    return colnums;
}

int CatalogueHandler::mag_col()
    const
    throw (MoggyException)
{
    static int magcol = -2;	// -2 uninitialised, -1 no mag col

    if (magcol < -1)		// uninitialised
    {
	vector<int>* allmagcols = mag_cols();
	magcol = (*allmagcols)[0];
	delete (allmagcols);
    }
    return magcol;
}

string CatalogueHandler::CatalogueRow::id ()
    const
    throw (MoggyException)
{
     char *value;
     string rval;
     if (parent_.queryResult_.get(num_, parent_.queryResult_.id_col(), value))
	 throw MoggyException ("Can't get id for row");
     rval = value;
     return rval;
}

double CatalogueHandler::CatalogueRow::ra ()
    const
    throw (MoggyException)
{
    if (!parent_.has_ra())
	throw MoggyException ("No RA for row");

    char *value;
    if (parent_.queryResult_.get(num_, parent_.queryResult_.ra_col(), value))
	throw MoggyException ("Can't get RA for row");

    // If string is in sexagesimal convert it to degrees.
    return str2ra( value );
}

double CatalogueHandler::CatalogueRow::dec ()
    const
    throw (MoggyException)
{
    if (!parent_.has_dec())
	throw MoggyException ("No DEC for row");

    char *value;
    if (parent_.queryResult_.get(num_, parent_.queryResult_.dec_col(), value))
	throw MoggyException ("Can't get DEC for row");

    // If string is in sexagesimal convert it to degrees.
    return str2dec( value );
}

double CatalogueHandler::CatalogueRow::mag()
    const
    throw (MoggyException)
{
    double value;
    if (!parent_.has_mag())
	throw MoggyException ("No MAG for row");
    try {
        parent_.queryResult_.get(num_, parent_.mag_col(), value);
    } catch (...) {
        value = 99.0; // Just a bad magnitude, no need to overreact.
    }
    return value;
}

ostream& operator<< (ostream& o, const CatalogueHandler::CatalogueRow& row)
{
    return row.put(o);
}

ostream& CatalogueHandler::CatalogueRow::put (ostream& o)
    const
{
    int ra_col = parent_.queryResult_.ra_col();
    int dec_col = parent_.queryResult_.dec_col();

    for (int i=0; i<parent_.queryResult_.numCols(); i++)
    {
	if (i > 0) {
	    o << sep_;
        }
        char *value;
        if (parent_.queryResult_.get(num_, i, value)) {
            throw MoggyException ("can't get line of query result");
        }

        // RA and Dec maybe in sexagesimal format, if so convert to
        // decimal.
        if ( i == ra_col ) {
            o << str2ra( value );
        } else if ( i == dec_col ) {
            o << str2dec( value );
        } else {
            o << value;
        }
    }

    return o;
}



// Convert skycat errors into exceptions
void skycatErrorHandler_ (const char* msg)
    throw (MoggyException)
{
    string s = "Skycat error: ";
    s.append(msg);
    throw MoggyException (s);
}
