// -*-c++-*-
#ifndef _TabTable_h_
#define _TabTable_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TabTable.h,v 1.15 1999/03/22 21:41:06 abrighto Exp $
 *
 * TabTable.h - class definitions for accessing values from a char buffer
 * 	        in the format of table, such as the result of a database query,
 *              where the rows are separated by newlines and the columns by
 *              tabs (or other given char).
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  9 Jan 96  Created
 */


#include <iostream.h>

/*
 * Class TabTable
 *
 * This class manages a buffer containing a table where the rows are
 * separated by newlines and the columns by tabs (or other char).  The
 * class provides transparent access to the table based on a row,column
 * index and allows for type conversion from string to the desired type.
 */
class TabTable {
protected:
    int numRows_;		// number of rows
    int numCols_;		// number of columns
    char** colNames_;		// array of column names

    char* buf_;			// saved copy of input buffer (memory for table)
    char** table_;		// array of row/col values
    int* index_;		// array of row indexes for sorting

    char sep_;			// separator char (default: \t)
    int status_;		// status after constructor (0 if OK)

    // these are set and used only during a table sort for use by compare methods
    static int numSortCols_;	// number of columns to sort by
    static char** sortCols_;	// array of sort column names
    static int* sortColIndexes_;// array of sort column indexes
    static int sortOrder_;	// sort order (increasing if >= 0)
    static int sortStatus_;	// status after sort (0 is ok)

    // scan the table to set colNames, numRows, numCols and set the start of data ptr
    virtual int scanTable(int maxRows, char*& start);

    // util to split a line into string array
    virtual int splitList(char* line, char** colValues);

    // report errors in input
    virtual int tab_error(int row, int col, char* expected, char* value) const;
   
    // scan the given buffer and return the number of lines
    virtual int getNumLines(char* buf, int maxRows);

    // create and fill the internal table from the given buffer in tab table format 
    virtual int fillTable(char* buf);

    // internal search util: assumes stream is positioned at first row
    virtual int search(istream& is, int numSearchCols, char** searchCols, 
	       char** minValues, char** maxValues, int maxRows);

    // given a row with columns for this table, compare the row with the
    // given column min and max values and return 0 if there is a match.
    virtual int compareRow(char* buf, int numSearchCols, char** searchCols, 
		   char** minValues, char** maxValues);

    // same as above, but with the row split into a string array
    virtual int compareRow(char** colValues, int numSearchmCols, char** searchCols, 
		   char** minValues, char** maxValues);

    int compareRow(const TabTable& table, int row, int numSearchCols, 
			 char** searchCols, char** minValues, char** maxValues);

    // return 0 if the given value is in the given range, doing a numeric comparison if
    // possible.
    virtual int compareCol(const char* value, const char* minValue, const char* maxValue);

    // return column index in input for the given column name
    // (may be redefined by a derived class taht changes column names or positions)
    virtual int inputColIndex(const char* colName) const {return colIndex(colName);}

    // check that row and column are in range
    virtual int checkTableIndex(int row, int col=0) const;

    // print table title and other info
    virtual void printTableTop(ostream& os, const char* title = NULL);

    // copy constructor (not defined)
    // TabTable(const TabTable&);

public:
    // constants 
    enum {MAX_ROW_SIZE=8*1024};	 // max size of a tab table row
    enum {MAX_HEADER_SIZE=1024}; // max size of a tab table heading
    enum {MAX_COLUMNS=255};	 // max number of table columns

public:
    // constructor: initialize empty table
    TabTable(char sep = '\t');

    // constructor: initialize table from buffer in tab table format
    TabTable(const char* buf, int maxRows = 0, char sep = '\t');

    // constructor: initialize table from data buffer without headings
    TabTable(int numCols, char** colNames, const char* buf, int maxRows = 0, char sep = '\t');

    // destructor: free any allocated memory
    virtual ~TabTable();

    // make the table empty and free any resources used
    virtual int clear();

    // fill the table from the given buffer in tab table format
    virtual int init(const char* buf, int maxRows = 0, int owner = 0);

    // fill the table from the given buffer in tab table format, with headings
    // specified separately
    virtual int init(int numCols, char** colNames, const char* buf, 
		     int maxRows = 0, int owner = 0);

    // access to row,column values
    // set the parameter value and return 0 on success
    virtual int get(int row, int col, char*& value) const;
    virtual int get(int row, int col, int& value) const;
    virtual int get(int row, int col, double& value) const;
    virtual int get(int row, int col, float& value) const;
    virtual int get(int row, int col, short& value) const;
    virtual int get(int row, int col, char& value) const;

    // get table values by column name
    virtual int get(int row, const char* colName, char*& value) const;
    virtual int get(int row, const char* colName, int& value) const;
    virtual int get(int row, const char* colName, double& value) const;
    virtual int get(int row, const char* colName, float& value) const;
    virtual int get(int row, const char* colName, short& value) const;
    virtual int get(int row, const char* colName, char& value) const;

    // return the table column index for the given table column name
    virtual int colIndex(const char* colName)  const;

    // return the column name for the given column index
    virtual const char* colName(int col)  const;

    // return true if the table contains the given column 
    virtual int hasCol(const char* name)  const {return (colIndex(name) >= 0);}

    // read the heading info from the given stream and return object for it
    static int head(const char* filename, TabTable&);
    static int head(istream&, TabTable&);
 
    // compare headings in this table and the given one
    virtual int compareHeadings(const TabTable& t);

    // compare the given rows
    virtual int compareRows(int row1, int row2);

    // save the contents of this object as a tab table
    virtual int save(const char* filename);
    virtual int save(ostream&);
    
    // append the contents of this object to the given tab table file
    virtual int append(const char* filename);
 
    // insert (or update) the contents of this object to the given tab table file
    virtual int insert(const char* filename, int col = 0);

    // remove rows in the tab table file that match the given col in this object
    virtual int remove(const char* filename, int col);
 
    // find a row in this object matching the col in the given tab separated table row
    virtual int findRow(const char* tableRow, int col);
    
    // sort the contents of this tab table by the given columns
    virtual int sort(int numSortCols, char** sortCols, int sortOrder = 0); 

    // Search the given tab table for upto maxRows rows with columns
    // values matching the given arguments and fill this table with the
    // resulting rows.
    virtual int search(const TabTable& table, int numSearchCols, char** searchCols, 
		       char** minValues, char** maxValues, int maxRows); 

    // search as above, but for a single column name and value
    virtual int search(const TabTable& table, const char* searchCol, const char* value, int maxRows);

    // search as above, but for a single column index and value
    virtual int search(const TabTable& table, int searchCol, const char* value, int maxRows); 

    // Search the given tab table file for upto maxRows rows with columns
    // values matching the given arguments and fill this table with the
    // resulting rows.
    virtual int search(const char* filename, int numSearchCols, char** searchCols, 
		       char** minValues, char** maxValues, int maxRows); 

    // search as above, but for a single column name and value
    virtual int search(const char* filename, const char* searchCol, const char* value, int maxRows);

    // search as above, but for a single column index and value
    virtual int search(const char* filename, int searchCol, const char* value, int maxRows); 

    // print the table rows to the given stream
    virtual int printRows(ostream& os) const;
    
    // print the given table row to the given stream
    virtual int printRow(ostream& os, int row) const; 

    // output operator
    friend ostream& operator<<(ostream& os, TabTable& t) {
	t.save(os); return os;
    }

    // member access
    virtual int numRows() const {return numRows_;}
    virtual void numRows(int n) {if (n < numRows_ && n >= 0) numRows_ = n;}
    virtual int numCols() const {return numCols_;}
    virtual char** colNames() const {return colNames_;}
    virtual int status() const {return status_;}
};



#endif /* _TabTable_h_ */
