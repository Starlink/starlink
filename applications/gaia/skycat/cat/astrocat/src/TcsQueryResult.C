/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsQueryResult.C,v 1.4 2003/01/20 15:52:21 brighton Exp $
 *
 * TcsQueryResult.C - method definitions for class TcsQueryResult
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */
static const char* const rcsId="@(#) $Id: TcsQueryResult.C,v 1.4 2003/01/20 15:52:21 brighton Exp $";

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstring>
#include "error.h"
#include "WorldCoords.hxx"
#include "TcsQueryResult.h"


/*
 * Fill the table from the given buffer in tab table format.
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 * (redefined from parent class to add init of objects_ array)
 */
int TcsQueryResult::init(const char* buf, int maxRows, int owner)
{
    if (TabTable::init(buf, maxRows, owner) != 0)
	return ERROR;
    return make_objects();
}


/*
 * Initialize the table from the data buffer (without heading lines).
 * The first two args specify the number column headings and their names.
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 * (redefined from parent class to add init of objects_ array)
 */
int TcsQueryResult::init(int numCols, char** colNames, const char* buf, int maxRows, int owner)
{
    if (TabTable::init(numCols, colNames, buf, maxRows, owner) != 0)
	return ERROR;
    return make_objects();
}


/*
 * make the table empty and free any resources used
 * (redefined from parent class to add cleanup of objects_ array)
 */
int TcsQueryResult::clear()
{
    if (TabTable::clear() != 0)
	return ERROR;
    if (objects_ != NULL) {
	delete [] objects_;
	objects_ = NULL;
    }
    return 0;
}


/*
 * make the objects_ array to hold the data for the rows
 */
int TcsQueryResult::make_objects()
{
    if (objects_ != NULL) {
	delete [] objects_;
	objects_ = NULL;
    }
    
    if (numRows_ <= 0)
	return 0;

    objects_ = new TcsCatalogObject[numRows_];
    if (!objects_) 
	return error("no enough memory");

    for (int row = 0; row < numRows_; row++) {
	if (getObjFromTable(row, objects_[row]) != 0) {
	    delete [] objects_;
	    objects_ = NULL;
	    return ERROR;
	}
    }
    return 0;
}


/*
 * get the value at the given row,column as a double
 * and allow missing columns to be set to the null value
 */
int TcsQueryResult::getDouble(int row, int col, double& value)
{
    char* p;
    if (get(row, col, p) != 0)
	return 1;

    if (strlen(p) == 0) 
	value = TCS_CATALOG_NULL_DOUBLE;
    else if (sscanf(p, "%lf", &value) != 1)
	return tab_error(row, col, "double", p);
    return 0;
}



/*
 * Access a TcsCatalog (GSC/PPM) result row and fill out the given TcsCatalogObject.
 * (For compat. with earlier version: it is more efficient to use getObj(int row))
 */
int TcsQueryResult::getObj(int row, TcsCatalogObject& t) const
{
    if (checkTableIndex(row) != 0) 
	return ERROR;
    
    if (!objects_) 
	return error("empty TCS result");

    t = objects_[index_[row]];
    return 0;
}


/*
 * Return a pointer to an object for the given row or NULL if there is
 * an error. The memory belongs to this class and should not deleted.
 */
TcsCatalogObject* TcsQueryResult::getObj(int row) const
{
    if (checkTableIndex(row) != 0) 
	return NULL;
    
    if (!objects_) {
	error("empty TCS result");
	return NULL;
    }

    return &objects_[index_[row]];
}


/*
 * Access a TcsCatalog (GSC/PPM) result row and fill out the given TcsCatalogObject.
 * Any missing fields are left at the default or null value.
 */
int TcsQueryResult::getObjFromTable(int row, TcsCatalogObject& t)
{
    int i;
    char* s;
    double d;
    WorldCoords pos;	// ra and dec

    t.reset();
    
    if (!entry_->isWcs()) 
	return error("catalog does not support World Coordinates");

    // set the id, ra and dec
    if (get(row, id_col(), s) != 0
	|| t.id(s) != 0
	|| getPos(row, pos) != 0 
	|| t.ra(pos.ra_deg()) != 0 
	|| t.dec(pos.dec_deg()) != 0)
	return ERROR;

    // look for GSC or PPM (http catalog server) names and set the rest

    // epoch
    if ((i = inputColIndex("epoch")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.epoch(d) != 0)
	    return ERROR;
    }

    // pma
    if ((i = inputColIndex("pma")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.pma(d) != 0)
	    return ERROR;
    }

    // pmd
    if ((i = inputColIndex("pmd")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.pmd(d) != 0)
	    return ERROR;
    }

    // radvel
    if ((i = inputColIndex("radvel")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.radvel(d) != 0)
	    return ERROR;
    }

    // parallax
    if ((i = inputColIndex("parallax")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.parallax(d) != 0)
	    return ERROR;
    }

    // mag
    if ((i = inputColIndex("mag")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.mag(d) != 0)
	    return ERROR;
    }

    // more
    if ((i = inputColIndex("more")) >= 0) {
	if (get(row, i, s) != 0 || t.more(s) != 0)
	    return ERROR;
    }

    // preview
    if ((i = inputColIndex("preview")) >= 0) {
	if (get(row, i, s) != 0 || t.preview(s) != 0)
	    return ERROR;
    }

    // distance
    if ((i = inputColIndex("distance")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.distance(d) != 0)
	    return ERROR;
    } 

    // pa
    if ((i = inputColIndex("pa")) >= 0) {
	if (getDouble(row, i, d) != 0 || t.pa(d) != 0)
	    return ERROR;
    }

    // cooSystem
    if ((i = inputColIndex("cooSystem")) >= 0) {
	if (get(row, i, s) != 0 || t.cooSystem(s) != 0)
	    return ERROR;
    }

    // cooType
    if ((i = inputColIndex("cooType")) >= 0) {
	if (get(row, i, s) != 0 || t.cooType(s) != 0)
	    return ERROR;
    }

    // band
    if ((i = inputColIndex("band")) >= 0) {
	if (get(row, i, s) != 0 || t.band(s) != 0)
	    return ERROR;
    }

    // calculate some missing values if needed (distance to center and pa)
    if (TcsCatalogObject::isNull(t.distance()) || TcsCatalogObject::isNull(t.pa())) {
	if (!centerPos_.isNull()) {
	    if (t.distance(centerPos_.wc().dist(pos, d)) != 0 || t.pa(d) != 0)
		return ERROR;
	}
    }

    return 0;
}


/*
 * return column index in the original input for the given TCS column name
 * (might be different than the output index)
 */
int TcsQueryResult::inputColIndex(const char* colName) const
{
    int i = -1;
    if ((i = TabTable::colIndex(colName)) >= 0)
	return i;
    
    // insert any name changes here

    if (strcmp(colName, "distance") == 0)
	return TabTable::colIndex("d'");
    
    return -1;
}


/*
 * print the given table row to the given stream
 */
int TcsQueryResult::printRow(std::ostream& os, int row) const
{
    // output the rows
    TcsCatalogObject* obj = getObj(row);
    if (obj == NULL)
	return ERROR;
    obj->printTableRow(os);
    return 0;
}


/*
 * print the table title (and any other info preceding the column headings)
 * (redefined here from parent class to add TCS column info in table header)
 */
void TcsQueryResult::printTableTop(std::ostream& os, const char* title) 
{
    if (! title)
	title = "TcsQueryResult";
    QueryResult::printTableTop(os, title);

    // add TCS header

    // comment
    os << "\n" 
       << "# This file contains catalog information in TCS tab table format\n"
       << "\n";
    
    // TCS column info
    os << "# Column descriptions:\n"
       << "id_desc= Object ID\n"
       << "id_type= string # Object ID\n"
       << "\n"
       << "ra_desc= Alpha coordinate for the target in decimal degrees\n"
       << "ra_units= deg\n"
       << "ra_type= real\n"
       << "ra_range= 0.,360\n"
       << "\n"
       << "dec_desc= Delta coordinate for the target in decimal degrees\n"
       << "dec_unit= deg\n"
       << "dec_type= real\n"
       << "dec_range= 0.,360.\n"
       << "\n"
       << "cooSystem_desc= Equinox system and equinox (only 1950 or 2000 are accepted)\n"
       << "cooSystem_type= string\n"
       << "cooSystem_range= enum B1950, J2000\n"
       << "cooSystem_def_val= \"J2000\"\n"
       << "\n"
       << "epoch_desc= Epoch expressed as decimal year.\n"
       << "epoch_type= real\n"
       << "epoch_range= -2000.,3000.\n"
       << "epoch_def_val= 2000.\n"
       << "\n"
       << "pma_desc= Proper motion alpha in radians/year (-10.0 to 10.0)\n"
       << "pma_unit= arcsecs/year\n"
       << "pma_type= real\n"
       << "pma_range= -10.,10.\n"
       << "pma_def_val= 0.0\n"
       << "\n"
       << "pmd_desc= Proper motion delta in radians/year (-10.0 to 10.0) \n"
       << "pmd_unit= arcsecs/year\n"
       << "pmd_type= real\n"
       << "pmd_range= -10.,10.\n"
       << "pmd_def_val= 0.0\n"
       << "\n"
       << "radvel_desc= Radial velocity in km/sec (-200000 to 200000)\n"
       << "radvel_unit= km/sec\n"
       << "radvel_type= real\n"
       << "radvel_range= -200000.,200000.\n"
       << "radvel_def_val= 0.\n"
       << "\n"
       << "parallax_desc= Parallax in arcseconds (-10000 to 10000)\n"
       << "parallax_unit= arcseconds\n"
       << "parallax_type= real\n"
       << "parallax_range= -10000.0,10000.0\n"
       << "parallax_def_val= 0.0\n"
       << "\n"
       << "cooType_desc= Coordinate type as \"m\" for mean or \"a\" for apparent character\n"
       << "cooType_type= string\n"
       << "cooType_range= enum \"m\",\"a\"\n"
       << "cooType_def_val= \"m\"\n"
       << "\n"
       << "band_desc= Magnitude wavelength band\n"
       << "band_type= string\n"
       << "band_def_val= \"v\"\n"
       << "\n"
       << "mag_desc= Object's magnitude in given band\n"
       << "mag_unit= magnitude\n"
       << "mag_type= real\n"
       << "mag_def_val= 0.0\n"
       << "\n"
       << "more_desc= An HTTP URL pointing to more info on the object\n"
       << "more_unit= http url\n"
       << "more_type=string\n"
       << "more_def_val= \"\"\n"
       << "\n"
       << "preview_desc= An HTTP URL pointing to an image of the object\n"
       << "preview_unit= http url\n"
       << "preview_type= string\n"
       << "preview_def_val= \"\"\n"
       << "\n"
       << "distance_desc= Object distance to field center\n"
       << "distance_unit= arcmin\n"
       << "distance_type= real\n"
       << "\n"
       << "pa_desc= Object position angle to field center (east of north)\n"
       << "pa_unit= deg\n"
       << "pa_type= real\n"
       << "\n"
       << "# NULL values\n"
       << "string_null= \"\" # empty string\n"
       << "real_null= 1.e-300\n"
       << "int_null= 4294967294 # (2^32 - 1)\n"
       << "\n";
}

/*
 * compare the given rows and return <. = or > 0, as by strcmp.
 * (redefined from parent class to allow compare of columns that
 * have been renamed or calculated (d', pa))
 */
int TcsQueryResult::compareRows(int row1, int row2)
{
    if (row1 < 0 || row1 >= numRows_ || row2 < 0 || row2 >= numRows_) 
	return (sortStatus_ = error("sort row index out of range"));

    // don't use index_[row] here since we are sorting it
    int ret = 0;
    for (int i = 0; i < numSortCols_; i++) {
	if ((ret = objects_[row1].compare(objects_[row2], sortColIndexes_[i])) != 0)
	    break;
    }
    return ret * sortOrder_;
}
