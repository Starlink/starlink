// -*-c++-*-
#ifndef _TcsQueryResult_h_
#define _TcsQueryResult_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsQueryResult.h,v 1.8 1997/10/30 21:46:48 abrighto Exp $
 *
 * TcsQueryResult.h - class definitions for accessing results of a 
 *                    TCS catalog query
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */


#include "QueryResult.h"
#include "TcsCatalogObject.h"


/*
 * Class TcsQueryResult
 *
 * This class manages the result of an TcsCatalog::query. Internally
 * a QueryResult object manages the "tab table" query results (this could
 * be changed later if performance is an issue).
 *
 * This class provides transparent access to the result based on a
 * row,column index and, unlike the QueryResult class, is specialized for 
 * accessing the GSC and PPM catalogs. There are special methods to return
 * GSC and PPM fields in common units and to determine if a field is present
 * in the catalog. 
 */
class TcsQueryResult : public QueryResult {
private:

    // copy constructor (not defined)
    TcsQueryResult(TcsQueryResult&); 

protected:
    // array of objects created from the rows (better for sorting)
    TcsCatalogObject* objects_;

    // get the value at the given row,column as a double
    // and allow missing columns to be set to the null value
    virtual int getDouble(int row, int col, double& value);

    // return column index in the original input for the given TCS column name
    // (might be different than the output index)
    virtual int inputColIndex(const char* colName) const;
    
    // compare 2 rows (redefined from parent class to compare TCS objects)
    virtual int compareRows(int row1, int row2);

    // make array of Tcs objects
    virtual int make_objects();

    // access a TcsCatalog (GSC/PPM) result row: fill out the given TcsCatalogObject
    virtual int getObjFromTable(int row, TcsCatalogObject&);

    // print table title and other info
    virtual void printTableTop(ostream& os, const char* title = NULL);

public:
    // constructor: initialize empty table
    TcsQueryResult()
	: QueryResult(), objects_(NULL) {}

    // constructor: init from query result buffer
    TcsQueryResult(const char* result) 
	: QueryResult(result), objects_(NULL) {}

    // destructor: free any allocated memory
    virtual ~TcsQueryResult() {}

    // make the table empty and free any resources used
    virtual int clear();

    // fill the table from the given buffer in tab table format
    virtual int init(const char* buf, int maxRows = 0);

    // fill the table from the given buffer in tab table format, with headings
    // specified separately
    virtual int init(int numCols, char** colNames, const char* buf, int maxRows = 0);

    // access a TcsCatalog (GSC/PPM) result row: fill out the given TcsCatalogObject
    virtual int getObj(int row, TcsCatalogObject&) const;

    // Return a pointer to an object for the given row or NULL if there is
    // an error. The memory belongs to this class and should not deleted.
    TcsCatalogObject* getObj(int row) const;

    // print the given table row to the given stream
    virtual int printRow(ostream& os, int row) const; 

    // -- redefine these to deal with TCS columns --

    // get array of TCS column names, number of columns
    virtual char** colNames() const {return TcsCatalogObject::colNames();}
    virtual int numCols() const {return TcsCatalogObject::numCols();}
    
    // return the TCS column name for the given TCS column index
    virtual const char* colName(int col) const {return TcsCatalogObject::colName(col);}

    // return the TCS column index for the given TCS column name
    virtual int colIndex(const char* colName) const {return TcsCatalogObject::colIndex(colName);} 

    // redefine these here from the base class, since the columns are fixed
    virtual int id_col() const            {return 0;}
    virtual int ra_col() const            {return 1;}
    virtual int dec_col() const           {return 2;}
    virtual int x_col() const             {return -1;}
    virtual int y_col() const             {return -1;}
};


#endif /* _TcsQueryResult_h_ */
