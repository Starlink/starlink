/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsCatalogObject.C,v 1.6 1996/11/21 15:17:02 abrighto Exp $
 *
 * TcsCatalogObject.C - method definitions for class TcsCatalogObject
 * 
 * See the man page for a complete description.
 * 
 * who             when       whuat
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */
static const char* const rcsId="@(#) $Id: TcsCatalogObject.C,v 1.6 1996/11/21 15:17:02 abrighto Exp $";


#include <string.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>
#include "error.h"
#include "WorldCoords.h"
#include "TcsCatalogObject.h"


// static array of column names
static char* colNames_[] = {
    "id", "ra", "dec", "cooSystem", "epoch", "pma", "pmd", 
    "radvel", "parallax", "cooType", "band", "mag", "more", 
    "preview", "distance", "pa"
};


// constants for column names
enum {
    ID, RA, DEC, COOSYSTEM, EPOCH, PMA, PMD, 
    RADVEL, PARALLAX, COOTYPE, BAND, MAG, MORE, 
    PREVIEW, DISTANCE, PA
};


// number of columns
static const int numCols_ = sizeof(colNames_)/sizeof(char*);


// -- local utils --

// return an allocated copy of the string, or NULL if it is NULL
static inline char* copy(const char* s) {return s ? strdup(s) : (char*)NULL;}

/*
 * check that the given value is in the given range and return 0
 * if it is, otherwise generate an error
 */
static int check(const char* name, double v, double low, double hi)
{
    return (v >= low && v <= hi) ? 0 : error(name, ": column value out of range");
}

/* 
 * check that the given string value has one of the specified values
 */
static int check(const char* name, const char* v, const char* a, const char* b)
{
    return (strcmp(v, a) == 0 || strcmp(v, b) == 0) ? 0 : error(name, ": invalid column value");
}


/*
 * constructor: initialize all fields to default values or null
 */
TcsCatalogObject::TcsCatalogObject() 
    : ra_(TCS_CATALOG_NULL_DOUBLE),
      dec_(TCS_CATALOG_NULL_DOUBLE),
      epoch_(2000.),
      pma_(TCS_CATALOG_NULL_DOUBLE),
      pmd_(TCS_CATALOG_NULL_DOUBLE),
      radvel_(TCS_CATALOG_NULL_DOUBLE),
      parallax_(TCS_CATALOG_NULL_DOUBLE),
      mag_(TCS_CATALOG_NULL_DOUBLE),
      more_(NULL),
      preview_(NULL),
      distance_(TCS_CATALOG_NULL_DOUBLE),
      pa_(TCS_CATALOG_NULL_DOUBLE)
{
      id_[0] = '\0';
      strcpy(cooSystem_, "J2000");
      strcpy(cooType_, "M");
      strcpy(band_, "V");
}

    
/*
 * copy constructor
 */
TcsCatalogObject::TcsCatalogObject(const TcsCatalogObject& t) 
    : ra_(t.ra_),
      dec_(t.dec_),
      epoch_(t.epoch_),
      pma_(t.pma_),
      pmd_(t.pmd_),
      radvel_(t.radvel_),
      parallax_(t.parallax_),
      mag_(t.mag_),
      more_(copy(t.more_)),
      preview_(copy(t.preview_)),
      distance_(t.distance_),
      pa_(t.pa_)
{
      strcpy(id_, t.id_);
      strcpy(cooSystem_, t.cooSystem_);
      strcpy(cooType_, t.cooType_);
      strcpy(band_, t.band_);
}


/*
 * result all fields to default values
 */
void TcsCatalogObject::reset()
{
    *this = TcsCatalogObject();
}


/*
 * assignment operator
 */
TcsCatalogObject& TcsCatalogObject::operator=(const TcsCatalogObject& t)
{
    if (more_)
	free(more_);
    if (preview_)
	free(preview_);

    strcpy(id_, t.id_);
    ra_ = t.ra_;
    dec_ = t.dec_;
    strcpy(cooSystem_, t.cooSystem_);
    epoch_ = t.epoch_;
    pma_ = t.pma_;
    pmd_ = t.pmd_;
    radvel_ = t.radvel_;
    parallax_ = t.parallax_;
    strcpy(cooType_, t.cooType_);
    strcpy(band_, t.band_);
    mag_ = t.mag_;
    more_ = copy(t.more_);
    preview_ = copy(t.preview_);
    distance_ = t.distance_;
    pa_ = t.pa_;
    return *this;
}


/*
 * destructor
 */
TcsCatalogObject::~TcsCatalogObject() 
{
    if (more_)
	free(more_);
    if (preview_)
	free(preview_);
}


/*
 * output operator (Tcl list format)
 */
ostream& operator<<(ostream& os, const TcsCatalogObject& t)
{
    os << '{' << t.id_ << '}';

    if (t.ra_ != TCS_CATALOG_NULL_DOUBLE && t.dec_ != TCS_CATALOG_NULL_DOUBLE) {
	// convert to h:m:s before printing
	WorldCoords pos(t.ra_, t.dec_);
	os << ' ' << pos.ra() << ' ' << pos.dec();
    } 
    else {
	os << " {} {}";
    }

    os << " {" << t.cooSystem_ << "}";
    os << ' ' << t.epoch_;

    if (t.pma_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.pma_;
    else
	os << " {}";
	
    if (t.pmd_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.pmd_;
    else
	os << " {}";

    if (t.radvel_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.radvel_;
    else
	os << " {}";

    if (t.parallax_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.parallax_;
    else
	os << " {}";

    os << " {" << t.cooType_ << "}";
    os << " {" << t.band_ << "}";


    if (t.mag_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.mag_;
    else
	os << " {}";

    os << " {" << (t.more_ ? t.more_ : "") << "}";
    os << " {" << (t.preview_ ? t.preview_ : "") << "}";


    if (t.distance_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.distance_;
    else 
	os << " {}";

    if (t.pa_ != TCS_CATALOG_NULL_DOUBLE)
	os << ' ' << t.pa_;
    else
	os << " {}";

    return os;
}


/*
 * print this object as a tab separated row
 */
int TcsCatalogObject::printTableRow(ostream& os)
{
    char t = '\t';
    
    os << id_ << t
       << ra_ << t
       << dec_ << t
       << cooSystem_ << t
       << epoch_ << t
       << pma_ << t
       << pmd_ << t
       << radvel_ << t
       << parallax_ << t
       << cooType_ << t
       << band_ << t
       << mag_ << t
       << more_ << t
       << preview_ << t
       << distance_ << t
       << pa_
       << endl;
    return 0;
}
 

/*
 * print this object to the given buffer
 */
void TcsCatalogObject::print(char* buf, int bufsize) 
{
    ostrstream os(buf, bufsize);
    os << *this << ends;
}


/*
 * print the headings to the given ostream 
 * to match the output of the above output operator (<<).
 */
void TcsCatalogObject::printHeadings(ostream& os) 
{
    for (int i = 0; i < numCols_; i++) {
	os << colNames_[i];
	if (i < (numCols_-1))
	    os << ' ';
    }
}


/*
 * print the headings to the given buffer, separated by tabs
 * to match the output of the above output operator (<<).
 */
void TcsCatalogObject::printHeadings(char* buf, int bufsize) 
{
    ostrstream os(buf, bufsize);
    printHeadings(os);
    os << ends;
}


/*
 * set fields, with range checking, return 0 if OK
 */

int TcsCatalogObject::id(const char* v) 
{
    if (!v) {
	id_[0] = '\0';
	return error("null string specified for object id");
    }
    strncpy(id_, v, sizeof(id_)-1); 
    return 0;
}

int TcsCatalogObject::ra(double v) 
{
    ra_ = v; 
    return check("ra", v, 0., 360.);
}

int TcsCatalogObject::dec(double v) 
{
    dec_ = v; 
    return check("dec", v, -90., 90.);
}

int TcsCatalogObject::cooSystem(const char* v) 
{
    strncpy(cooSystem_, (v ? v : ""), sizeof(cooSystem_)-1); 
    return check("cooSystem", v, "B1950", "J2000");
}

int TcsCatalogObject::epoch(double v) 
{
    epoch_ = v; 
    return check("epoch", v, -2000., 3000.);
}

int TcsCatalogObject::pma(double v) 
{
    pma_ = v; 
    return check("pma", v, -10., 10.);
}

int TcsCatalogObject::pmd(double v) 
{
    pmd_ = v; 
    return check("pmd", v, -10., 10.);
}

int TcsCatalogObject::radvel(double v) 
{
    radvel_ = v; 
    return check("radvel", v, -200000., 200000.);
}

int TcsCatalogObject::parallax(double v) 
{ 
    parallax_ = v; 
    return check("parallax", v, -10000., 10000.);
}

int TcsCatalogObject::cooType(const char* v) 
{
    strncpy(cooType_, (v ? v : ""), sizeof(cooType_)-1); 
    return check("cooType", v, "M", "A");
}

int TcsCatalogObject::band(const char* v) 
{
    strncpy(band_, (v ? v : ""), sizeof(band_)-1); 
    return 0;
}

int TcsCatalogObject::mag(double v) 
{
    mag_ = v; 
    return 0;
}

int TcsCatalogObject::more(const char* v) 
{
    if (more_)
	free(more_);
    more_ = copy(v); 
    return 0;
}

int TcsCatalogObject::preview(const char* v) 
{
    if (preview_)
	free(preview_);
    preview_ = copy(v); 
    return 0;
}

int TcsCatalogObject::distance(double v) 
{
    distance_ = v; 
    return 0;
}

int TcsCatalogObject::pa(double v) 
{
    pa_ = v; 
    return 0;
}


// -- for compat with AstroCatalog: --


/*
 * return the number of columns in the catalog
 */
int TcsCatalogObject::numCols() 
{
    return numCols_;
}


/*
 * return a ptr to an array of catalog column names
 */
char** TcsCatalogObject::colNames() 
{
    return colNames_;
}


/*
 * return the name of the given column
 */
const char* TcsCatalogObject::colName(int col) 
{
    if (col >= 0 && col < numCols_)
	return colNames_[col];
    error("invalid column index");
    return NULL;
}


/*
 *  return the column index for the given column name
 */
int TcsCatalogObject::colIndex(const char* colName) 
{
    for (int i = 0; i < numCols_; i++) {
	if (strcmp(colName, colNames_[i]) == 0)
	    return i;
    }
    return -1;
}


/* 
 * Compare the given column (member) of this object with the given object.
 * The column index corresponds to the column heading array at the top of this
 * file (it is more efficient to use the index than the col name...).
 * Return <. = or > 0, as by strcmp.
 */
int TcsCatalogObject::compare(const TcsCatalogObject& obj, int colIndex)
{
    double d1 = 0., d2 = 0.;
    const char* s1 = "", *s2 = "";
    int numeric = 1;

    switch(colIndex) {
    case ID:
	s1 = id_; s2 = obj.id_; numeric--;
	break;
    case RA:
	d1 = ra_; d2 = obj.ra_;
	break;
    case DEC: 
	d1 = dec_; d2 = obj.dec_;
	break;
    case COOSYSTEM: 
	s1 = cooSystem_; s2 = obj.cooSystem_; numeric--;
	break;
    case EPOCH: 
	d1 = epoch_; d2 = obj.epoch_;
	break;
    case PMA: 
	d1 = pma_; d2 = obj.pma_;
	break;
    case PMD: 
	d1 = pmd_; d2 = obj.pmd_;
	break;
    case RADVEL: 
	d1 = radvel_; d2 = obj.radvel_;
	break;
    case PARALLAX: 
	d1 = parallax_; d2 = obj.parallax_;
	break;
    case COOTYPE: 
	s1 = cooType_; s2 = obj.cooType_; numeric--;
	break;
    case BAND: 
	s1 = band_; s2 = obj.band_; numeric--;
	break;
    case MAG: 
	d1 = mag_; d2 = obj.mag_;
	break;
    case MORE: 
	s1 = more_; s2 = obj.more_; numeric--;
	break;
    case PREVIEW: 
	s1 = preview_; s2 = obj.preview_; numeric--;
	break;
    case DISTANCE: 
	d1 = distance_; d2 = obj.distance_;
	break;
    case PA:
	d1 = pa_; d2 = obj.pa_;
	break;
    default:
	return error("invalid TCS column index");
    }

    if (numeric) {
	if (d1 > d2)
	    return 1;
	if (d1 < d2)
	    return -1;
	return 0;
    }
    else {
	return strcmp((s1 ? s1 : ""), (s2 ? s2 : ""));
    }
}


/* 
 * Compare the given column (member) of this object with the given object.
 * The column name corresponds to the column heading array at the top of this
 * file.
 * Return <. = or > 0, as by strcmp.
 */
int TcsCatalogObject::compare(const TcsCatalogObject& obj, char* colName)
{
    int i = colIndex(colName);
    if (i >= 0)
	return compare(obj, i);
    return 0;
}

