// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "stringstream.h"

//#include "moggy.h"
#include "CatalogueHandler.h"
#include "util.h"

#include <cat/error.h>		// for set_error_handler
void skycatErrorHandler_ (const char* msg) throw (MoggyException);

CatalogueHandler::CatalogueHandler ()
    : searchType_(UNSPECIFIED), nrows_(0), equinox_(2000.0),
      valid_(fieldflags(0)), returnCols_(0)
{
    // Set the skycat error handler
    set_error_handler (&skycatErrorHandler_);
}

CatalogueHandler::~CatalogueHandler ()
{
    if (returnCols_ != 0)
	delete[] returnCols_;
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

    int rowsreturned = 0;

    // First, try to find out which column is the magnitude column.
    // There's no standardisation mentioned in the documentation, so
    // the best we can do, I think, is search through the list of
    // column titles until we find one which include a substring `mag'.
    int magcol;
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
		cerr << "Pos1=" << pos_[0] << ", Pos2=" << pos_[1]
		     << " : radius=" << radius_ << endl;
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

    return const_iterator(this);
}

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
			       int rah,  int ramin,  double rasec,
			       int degh, int degmin, double degsec,
			       double equinox=2000.0)
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
	setValid_(num==1 ? POS1 : POS2);
	return true;
    }
    else
	return false;
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
    if (setenv ("CATLIB_CONFIG", URL.c_str(), 1) == 0)
	return true;
    else
	return false;
}

// setResultCols: this could (fairly easily, with
// Util::tokeniseString) be made more sophisticated, and take a list
// of column names as argument.
bool CatalogueHandler::setResultCols (string cols)
{
    if (! isValid_(CATNAME))
	return false;

    Util::uppercaseString(cols);

    if (cols == "SIMPLE")
    {
	if (returnCols_ != 0)
	    delete[] returnCols_;
	// allocate space for three column numbers, plus terminator
	returnCols_ = new int[3+1];
	returnCols_[0] = cat_->id_col();
	returnCols_[1] = cat_->ra_col();
	returnCols_[2] = cat_->dec_col();
	returnCols_[3] = -1;

	return true;
    }
    else if (cols == "ALL")
    {
	if (returnCols_ != 0)
	{
	    delete[] returnCols_;
	    returnCols_ = 0;
	}
	return true;
    }
    else			// eh?
	return false;
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
	if (returnCols_ == 0)
	    for (int i=0; i<cat_->numCols(); i++)
		colnames.push_back(cat_->colName(i));
	else
	    for (int i=0; returnCols_[i]>=0; i++)
		colnames.push_back(cat_->colName(returnCols_[i]));
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
    o << "COLUMNS " << (returnCols_ == 0 ? "ALL" : "SIMPLE") << lineend;
    string conf = getConfig();
    if (conf.length() > 0)
	o << "CATCONFIG " << conf << lineend;

    return;
}

// Convert skycat errors into exceptions
void skycatErrorHandler_ (const char* msg)
    throw (MoggyException)
{
    string s = "Skycat error: ";
    s.append(msg);
    throw MoggyException (s);
}
