// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "stringstream.h"

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

    int magcol = mag_col();

    if (searchType_ == RADIUSSEARCH || searchType_ == RADIUSPOINTSEARCH)
    {
	AstroQuery q;

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
	if (magcol >= 0)
	{
	    // I'm rather uncertain about this -- the documentation and the
	    // AstroQuery.h header are divergent.  This follows the
	    // header and comments in there.
	    const char *sortcols = cat_->colName(magcol);
	    q.sort(1, const_cast<char **>(&sortcols));
	    // sortOrder: >=0 means increasing -- see AstroQuery.h
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
	if (magcol >= 0)
	{
	    const char *sortcols = cat_->colName(magcol);
	    q.sort(1, const_cast<char **>(&sortcols));
	    q.sortOrder (+1);
	}

	rowsreturned = cat_->query(q, 0, queryResult_);
	setValid_(RESULT);
    }
    else
	// can't do those searches, yet
	return -1;

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

#if 0
string& CatalogueHandler::const_iterator::operator*()
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

    SSTREAM sb;
    if (parent_->returnCols_ == 0)
	for (int i=0; i<parent_->queryResult_.numCols(); i++)
	{
	    if (i > 0)
		sb << iterSeparator_;
	    char *value;
	    if (parent_->queryResult_.get(idx_, i, value))
		throw MoggyException ("can't get line of query result");
	    sb << value;
	}
    else
    {
	for (int i=0; parent_->returnCols_[i] >= 0; i++)
	{
	    if (i > 0)
		sb << iterSeparator_;
	    char *value;
	    if (parent_->queryResult_.get(idx_,
					  parent_->returnCols_[i],
					  value))
	    {
		SSTREAM errmsg;
		errmsg << "can't get column " << i << " of query result"
		       << ends;
		throw MoggyException (SS_STRING(errmsg));
	    }
	    sb << value;
	}
    }
					 
    iterLine_ = SS_STRING(sb);
    return iterLine_;
}
#endif

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
	cerr << "CatalogueHandler::* idx_=" << idx_
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
	    cerr << "CatalogueHandler::setPos(" << num << "): ("
		 << radeg << ", " << decdeg << " [" << equinox_ << "]) -->"
		 << endl
		 << "    "
		 << pos_[posindex] << " = ("
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
	    cerr << "CatalogueHandler::setPos(dec): (" << num
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
	    cerr << "CatalogueHandler::setPos(" << num << "): ("
		 << rah << ',' << ramin << ',' << rasec << ", "
		 << degh << ',' << degmin << ',' << degsec
		 << " [" << equinox_ << "]) --> " << endl
		 << "    "
		 << pos_[posindex] << " = ("
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
	    cerr << "CatalogueHandler::setPos(hmsdms): (" << num
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
    return (setenv ("CATLIB_CONFIG", URL.c_str(), 1) == 0);
}

// setResultCols: this could (fairly easily, with
// Util::tokeniseString) be made more sophisticated, and take a list
// of column names as argument.
//
// This function was deleted (after CatalogueHandler.C,v 1.3) -- if
// you want to select which columns are returned to the user, do it in 
// the caller, using the ra(), dec() functions.
bool CatalogueHandler::setResultCols (string cols)
{
    throw MoggyException ("Can't select columns using setResultCols");
}

string CatalogueHandler::getPos (int num) const throw (MoggyException)
{
    string retval;
    if (num < 1 || num > 2)
	throw MoggyException
	    ("CatalogueHandler::getPos got out-of-range argument");

    if (isValid_(num == 1 ? POS1 : POS2))
    {
	SSTREAM outline;
	outline << pos_[num-1];
	retval = SS_STRING(outline);
    }
    else
	retval = "<UNSPECIFIED>";
    return retval;
}

string CatalogueHandler::getSearchtype () const
{
    string retval;
    if (isValid_(SEARCHTYPE))
	switch (searchType_)
	{
	  case BOXSEARCH:
	    retval = "BOX";
	    break;
	  case RADIUSSEARCH:
	    retval = "RADIUS";
	    break;
	  case RADIUSPOINTSEARCH:
	    retval = "RADIUS2";
	    break;
	  default:
	    retval = "<UNSPECIFIED>";
	    break;
	}
    else
	retval = "<UNSPECIFIED>";
    return retval;
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

	    if (verbosity_ > normal)
	    {
		cerr << "CatalogueHandler::setCatname: "
		     << " longName=" << cat_->longName()
		     << " searchCols=" << cat_->searchCols()
		     << " sortCols=" << cat_->sortCols()
		     << " sortOrder=" << cat_->sortOrder()
		     << " showCols=" << cat_->showCols()
		     << " id_col=" << cat_->id_col()
		     << " ra_col=" << cat_->ra_col()
		     << " dec_col=" << cat_->dec_col()
		     << " x_col=" << cat_->x_col()
		     << " y_col=" << cat_->y_col()
		     << " equinox=" << cat_->equinox()
		     << endl;
 	    }
	    return true;
	}
	else
	    return false;
    }
    catch (MoggyException)
    {
	// This error will be `unknown catalog', raised by the error handler
	return false;
    }
}

string CatalogueHandler::getConfig () const
{
    char *conf = getenv ("CATLIB_CONFIG");
    if (conf == 0)
	return "";
    else
	return conf;
}

vector<string> CatalogueHandler::getColnames () const
{
    vector<string> colnames;

    if (isValid_(CATNAME))
	for (int i=0; i<cat_->numCols(); i++)
	    colnames.push_back(cat_->colName(i));
    // if RESULT isn't valid, then a vecetor of length zero will be returned

    return colnames;
}

void CatalogueHandler::printStatus (ostream& o, string lineend)
{
    if (isValid_(CATNAME))
	o << "NAME " << catname_ << lineend;
    if (isValid_(POS1))
	o << "COORD1 " << getPos(1) << lineend;
    if (isValid_(POS2))
	o << "COORD2 " << getPos(2) << lineend;
    if (isValid_(RADIUS))
	o << "RADIUS " << radius_ << lineend;
    if (isValid_(SEARCHTYPE))
	o << "SEARCH " << getSearchtype() << lineend;
    if (isValid_(NROWS))
	o << "NROW " << nrows_ << lineend;
    string conf = getConfig();
    if (conf.length() > 0)
	o << "CATCONFIG " << conf << lineend;

    return;
}

int CatalogueHandler::mag_col()
    const
    throw (MoggyException)
{
    static int magcol = -2;	// -2 means uninitialised

    if (! isValid_(CATNAME))
	throw MoggyException ("mag_col() out of order");

    if (magcol < -1)		// uninitialised
    {
	// Try to find out which column is the magnitude column.
	// There's no standardisation mentioned in the documentation, so
	// the best we can do, I think, is search through the list of
	// column titles until we find one which include a substring `mag'.
	for (magcol=0; magcol<cat_->numCols(); magcol++)
	{
	    string ucol(cat_->colName(magcol));
	    Util::uppercaseString(ucol);
	    if (ucol.find("MAG") != string::npos)
		break;
	}
	if (magcol >= cat_->numCols())
	    // not found
	    magcol = -1;
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
    double value;
    if (!parent_.has_ra())
	throw MoggyException ("No RA for row");
    if (parent_.queryResult_.get(num_, parent_.queryResult_.ra_col(), value))
	throw MoggyException ("Can't get RA for row");
    return value;
}

double CatalogueHandler::CatalogueRow::dec ()
    const
    throw (MoggyException)
{
    double value;
    if (!parent_.has_dec())
	throw MoggyException ("No DEC for row");
    if (parent_.queryResult_.get(num_, parent_.queryResult_.dec_col(), value))
	throw MoggyException ("Can't get DEC for row");
    return value;
}

double CatalogueHandler::CatalogueRow::mag()
    const
    throw (MoggyException)
{
    double value;
    if (!parent_.has_mag())
	throw MoggyException ("No MAG for row");
    if (parent_.queryResult_.get(num_, parent_.mag_col(), value))
	throw MoggyException ("Can't get MAG for row");
    return value;
}

ostream& operator<< (ostream& o, const CatalogueHandler::CatalogueRow& row)
{
    return row.put(o);
}

ostream& CatalogueHandler::CatalogueRow::put (ostream& o)
    const
{
    for (int i=0; i<parent_.queryResult_.numCols(); i++)
    {
	if (i > 0)
	    o << sep_;
	char *value;
	if (parent_.queryResult_.get(num_, i, value))
	    throw MoggyException ("can't get line of query result");
	o << value;
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
