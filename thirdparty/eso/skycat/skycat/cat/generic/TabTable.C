/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: TabTable.C,v 1.19 1999/03/22 21:41:12 abrighto Exp $
 *
 * TabTable.C - method definitions for class TabTable
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  08 Jan 96  Created
 */
static const char* const rcsId="@(#) $Id: TabTable.C,v 1.19 1999/03/22 21:41:12 abrighto Exp $";


#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>
#include <unistd.h>
#include "error.h"
#include "util.h"
#include "TabTable.h"

// these are needed only during a table sort 
static TabTable* thisPtr_;
int TabTable::numSortCols_ = 0;
char** TabTable::sortCols_ = NULL;
int* TabTable::sortColIndexes_ = NULL;
int TabTable::sortOrder_ = 0;
int TabTable::sortStatus_ = 0;

/*
 * constructor: initialize empty table
 */
TabTable::TabTable(char sep)
    : numRows_(0),
      numCols_(0),
      colNames_(NULL),
      buf_(NULL),
      table_(NULL),
      index_(NULL),
      sep_(sep),
      status_(0)
{
}


/*
 * constructor: initialize table from buffer in tab table format:
 *
 * VAR	VALUE	VALUE	...
 * ...
 * COL1	COL2	COL3	...	COLN
 * ---		----	----	----
 * data	data	data		data
 * ...
 *
 * Currently, anything up to the column headings is ignored (This may change
 * later). The format is the same as the tab table format used by starbase.
 * A line beginning with a "-" separates the column names from the data
 * and all column headings and data are separated by tabs (or sep char).
 *
 * The part preceding the column headings may be used later to set certain
 * variables pertaining to the table. In this case, a variable may have
 * one or more values, separated by tabs (sep char).
 *
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 */
TabTable::TabTable(const char* buf, int maxRows, char sep)
    : numRows_(0),
      numCols_(0),
      colNames_(NULL),
      buf_(NULL),
      table_(NULL),
      index_(NULL),
      sep_(sep),
      status_(0)
{
    status_ = init(buf, maxRows);
}


/*
 * constructor: initialize table from data buffer without headings.
 * The first two args specify the number column headings and their names.
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 */
TabTable::TabTable(int numCols, char** colNames, const char* buf, 
		   int maxRows, char sep)
    : numRows_(0),
      numCols_(0),
      colNames_(NULL),
      buf_(NULL),
      table_(NULL),
      index_(NULL),
      sep_(sep),
      status_(0)
{
    status_ = init(numCols, colNames, buf, maxRows);
}


/*
 * destructor: free any allocated memory
 */
TabTable::~TabTable()
{
    clear();
}


/*
 * Fill the table from the given buffer in tab table format.
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 * If owner is nonzero, this class will take control of the memory
 * for buf, otherwise it makes a copy. The data string in buf should be 
 * null terminated.
 */
int TabTable::init(const char* buf, int maxRows, int owner)
{
    clear();			// erase any existing rows

    if (owner)
	buf_ = (char*)buf;
    else
	buf_ = strdup(buf);

    // scan the table to set colNames, numRows, numCols and get start of data
    char* line = NULL;
    if (scanTable(maxRows, line) != 0)
	return ERROR;
    
    return fillTable(line);
}


/*
 * Initialize the table from the data buffer (without heading lines).
 * The first two args specify the number column headings and their names.
 * If maxRows is nonzero, only upto that many rows are taken from buf.
 * If owner is nonzero, this class will take control of the memory
 * for buf, otherwise it makes a copy. The data string in buf should be 
 * null terminated.
 */
int TabTable::init(int numCols, char** colNames, const char* buf, 
		   int maxRows, int owner)
{
    // count the data rows
    char* nbuf = (owner ? (char*)buf : strdup(buf));

    // (watch out for this: t.init(t.colNames()) (don't delete before using...))
    char** cnames = copyArray(numCols, colNames);
    clear();			// erase any existing rows
    buf_ = nbuf;
    numRows_ = getNumLines(buf_, maxRows);
    numCols_ = numCols;
    colNames_ = cnames;

    // fill the table rows from the buffer
    return fillTable(buf_);
}


/*
 * make the table empty and free any resources used
 */
int TabTable::clear()
{
    if (table_) {
	delete table_;
	table_ = NULL;
    }
    if (index_) {
	delete index_;
	index_ = NULL;
    }
    if (colNames_) {
	delete colNames_;
	colNames_ = NULL;
    }
    numCols_ = 0;
    numRows_ = 0;

    if (buf_) {
	free(buf_);
	buf_ = NULL;
    }
    return 0;
}


/* 
 * util: trim white space from string and return value
 */
static char* trim(char* s)
{
    if (strlen(s)) {
	while(isspace(*s))
	    s++;
	char* p = s + strlen(s) - 1;
	while(isspace(*p) && p > s)
	    *p-- = '\0';
    }
    return s;
}

  

/*
 * scan the table to set colNames, numRows, numCols and set the "start" pointer
 * to point to the start of the data rows.
 * If maxRows is nonzero, the input is truncated to that many rows.
 * Returns 0 if OK.
 */
int TabTable::scanTable(int maxRows, char*& start)
{
    char* line = buf_;		// start of current line
    char* head = NULL;		// start of header line with column names
    char* prev_line = NULL;	// ptr to previous line
    char* p;
    int i;

    start = NULL;		// points to start of data rows, if any
    for (p = strchr(line, '\n'); p; p = strchr(line = p+1, '\n')) {
	if (*line == '-') {
	    line = start = p + 1; // data starts after "---" line
	    head = prev_line;	  // header is line before "---"
	    break;
	} 
	prev_line = line;
	*p = '\0';
    }
    if (! head) {
	// status_ = error("bad tab table format, no '---' line found");
	// allow missing headers
	return 0;
    }

    // get the column names (and strip any leading and trailing white space)
    char* colNames[MAX_COLUMNS];
    for (p = strchr(head, sep_); p; p = strchr(head = p+1, sep_)) {
	*p = '\0';
	colNames[numCols_++] = head;
    }
    colNames[numCols_++] = head;
    colNames_ = new char*[numCols_];
    for (i = 0; i < numCols_; i++)
	colNames_[i] = trim(colNames[i]);

    // count the data rows
    numRows_ = getNumLines(line, maxRows);

    return 0;
}


/*
 * create and fill the internal table from the given buffer in tab table format 
 * and return 0 if all is OK
 */
int TabTable::fillTable(char* buf)
{
    if (numRows_ == 0 || numCols_ == 0) 
	return 0;  // empty table without a header

    table_ = new char*[numRows_*numCols_];
    index_ = new int[numRows_];
    if (!table_ || !index_) {
	return error("could not allocate enough memory for TabTable");
    }

    // fill the table with row,col values
    for (int i = 0; i < numRows_; i++) {
	index_[i] = i;		// default order: no sorting yet
	char* p = strchr(buf, '\n');
	if (!p) {
	    char msg[255];
	    sprintf(msg, "expected %d rows, but found %d", numRows_, i);
	    return error(msg);
	}
	*p++ = '\0';
	if (splitList(buf, table_+(i*numCols_)) != 0)
	    return ERROR;

	buf = p;		// set for next line
    }
    return 0;
}


/*
 * scan the given buffer and return the number of lines.
 * If maxRows is nonzero, truncate the buffer after that many lines.
 */
int TabTable::getNumLines(char* buf, int maxRows) {
    int n = 0;
    for (char* p = strchr(buf, '\n'); p; p = strchr(buf = p+1, '\n')) {
	if (maxRows > 0 && n >= maxRows) {
	    *++p = '\0';
	    return maxRows;
	}
	if (strncmp(buf, "[EOD]", 5) == 0) {  // XXX special case (should be in HTTP.C ?)
	    *buf = '\0';
	    break;
	}
	n++;
    }
    return n;
}


/* 
 * split the given line into numCols_ strings and put them in the given array,
 * which is assumed to be large enough (at least numCols_ long).
 * Returns 0 on success, otherwise 1 and an error message.
 */
int TabTable::splitList(char* line, char** colValues)
{
    char* val = line;
    int col;
    for (col = 0; col < numCols_; col++) {
	char* p = strchr(val, sep_);
	if (!p) {
	    colValues[col] = trim(val);
	    val = "";
	    continue;		// fill any missing cols with empty values
	}
	*p++ = '\0';
	colValues[col] = trim(val);
	val = p;		// set for next column
    }

    return 0;
}


/*
 * report an error in the tab table input at the given row and col,
 * that the given type of value was expected and the given string value
 * was found.
 */
int TabTable::tab_error(int row, int col, char* expected, char* value) const
{
    char msg[255];
    ostrstream os(msg, sizeof(msg));
    os << "error in tab table input: row " << (row+1) << ", col "
	<< (col+1) << ", expected " << expected << ", but found '"
	<< value << "'" << ends;
    return error(msg);
}


/*
 * return the result column index for the given result column name
 */
int TabTable::colIndex(const char* colName) const
{
    for (int i = 0; i < numCols_; i++)
	if (strcasecmp(colName, colNames_[i]) == 0)
	    return i;
    return -1;
}



/*
 * return the column name for the given column index
 */
const char* TabTable::colName(int col) const
{
    if (col >= 0 && col < numCols_ && numCols_ > 0)
	return colNames_[col];
    return NULL;
}


/*
 * save the contents of this object as a tab table file and return 0 if 
 * all is OK.
 */
int TabTable::save(const char* filename) 
{
    ofstream os(filename);
    if (!os) 
	return sys_error("can't open file: ", filename);
    return save(os);
}


/*
 * save the contents of this object as a tab table to the
 * given stream  and return 0 if all is OK.
 */
int TabTable::save(ostream& os) 
{
    if (numCols() == 0)
	return error("no data to save");

    int i, row, col;

    // print the title (and any other info preceding the column headings)
    printTableTop(os);

    // headings
    int ncols = numCols(), n = ncols-1;
    for (col = 0; col < ncols; col++) {
	os << colName(col);
	if (col < n)
	    os << '\t';
    }
    os << endl;

    // dashed line
    for (col = 0; col < ncols; col++) {
	int l = strlen(colName(col));
	for(i = 0; i < l; i++)
	    os << '-';
	if (col < n)
	    os << '\t';
    }
    os << endl;
    
    // rows and columns
    return printRows(os);
}


/*
 * print the table title (and any other info preceding the column headings)
 * (may be redefined in a derived class to add more info)
 */
void TabTable::printTableTop(ostream& os, const char* title) 
{
    if (! title)
	title = "TabTable";
    os << title << endl;
}


/*
 * append the contents of this object to a tab table file and return 0 if 
 * all is OK.
 */
int TabTable::append(const char* filename) 
{
    if (numRows() == 0 || numCols() == 0)
	return error("no data to append");

    TabTable t;
    if (head(filename, t) != 0)	// get table header in t
	return ERROR;

    if (compareHeadings(t) != 0) // compare with this table
	return error("tables have different columns");

    ofstream os(filename, ios::app);
    if (! os)
	return sys_error("can't append to file: ", filename);
        
    return printRows(os);
}


/*
 * Insert the contents of this object (0 or more rows) in a tab table
 * file and return 0 if all is OK. 
 *
 * Each row is assumed here to have a unique id column. If a row already
 * exists with the same id as one that we want to insert, the row is
 * replaced with the new one.  Otherwise, the new row is added at the end
 * of the file.
 *
 * filename is the name of the file to insert in.  
 *
 * col is the column index of the id column, used to check for
 * duplicates.
 */
int TabTable::insert(const char* filename, int col) 
{
    if (numRows() == 0 || numCols() == 0)
	return error("no data to insert");

    // make sure we have a valid id column
    if (col < 0)
	col = 0;

    if (checkTableIndex(0, col) != 0) 
	return ERROR;

    TabTable t;
    if (head(filename, t) != 0)	// get table header in t
	return ERROR;

    if (compareHeadings(t) != 0) // compare with this table
	return error("tables have different columns");

    ifstream is(filename);
    if (! is)
    	return sys_error("can't open file: ", filename);
        
    char tmpfile[1024];
    sprintf(tmpfile, "%s.TMP", filename);
    ofstream os(tmpfile);
    if (! os)
	return sys_error("can't open file: ", tmpfile);

    // skip heading
    char buf[MAX_ROW_SIZE];
    while (is.getline(buf, sizeof(buf))) {
	os << buf << endl;
	if (buf[0] == '-') 
	    break;
    }
    
    // make an array of flags, insertedRow[i] is 1 if row i was inserted in the file
    int* insertedRow = new int[numRows_]; 
    int row;
    for (row = 0; row < numRows_; row++)
	insertedRow[row] = 0;

    while (is.getline(buf, sizeof(buf))) {
	if ((row = findRow(buf, col)) < 0) {
	    // if row is not found in this object, add it unchanged to the output file
	    os << buf << endl;	
	}
	else {
	    // found matching id, add new row to the file to replace the old one
	    printRow(os, row);
	    insertedRow[row] = 1;
	}
    }

    // append any rows that were not inserted in the file
    for (row = 0; row < numRows_; row++)
	if (!insertedRow[row])
	    printRow(os, row);
    delete insertedRow;
        
    // make a backup file and rename the tmpfile to the original
    char bakfile[1024];
    sprintf(bakfile, "%s.BAK", filename);
    if (rename(filename, bakfile) != 0) 
	return sys_error("can't rename file to file.BAK for: ", filename);
    if (rename(tmpfile, filename) != 0) 
	return sys_error("can't rename file.TMP to file for: ", filename);

    return 0;
}


/*
 * remove all rows in the given tab table file where the value in the
 * given column matches the value in the same column in any row of this
 * object.
 *
 * For example: if col 0 is the id column and you want to remove any row
 * from the file foo.tab containing ids from this object, you would call:
 *
 *   table.remove("foo.tab", 0);
 *
 */
int TabTable::remove(const char* filename, int col)
{
    // make sure we have a valid id column
    if (col < 0)
	col = 0;

    if (numRows() == 0 || numCols() == 0)
	return error("no data rows to remove");

    if (checkTableIndex(0, col) != 0) 
	return ERROR;

    TabTable t;
    if (head(filename, t) != 0)	// get table header in t
	return ERROR;

    if (compareHeadings(t) != 0) // compare with this table
	return error("tables have different columns");

    // read from filename and write to $filename.tmp (rename later)
    ifstream is(filename);
    if (!is)
	return sys_error("can't open file: ", filename);

    char tmpfile[1024];
    sprintf(tmpfile, "%s.TMP", filename);
    ofstream os(tmpfile);
    if (! os)
	return sys_error("can't open file: ", tmpfile);

    char buf[MAX_ROW_SIZE];
    // skip heading
    while (is.getline(buf, sizeof(buf))) {
	os << buf << endl;
	if (buf[0] == '-') 
	    break;
    }
    while (is.getline(buf, sizeof(buf))) {
	if (findRow(buf, col) < 0)	// if row is not found
	    os << buf << endl;	// then add to output file
    }
        
    char bakfile[1024];
    sprintf(bakfile, "%s.BAK", filename);
    if (rename(filename, bakfile) != 0) 
	return sys_error("can't rename file to file.BAK for: ", filename);
    if (rename(tmpfile, filename) != 0) 
	return sys_error("can't rename file.TMP to file for: ", filename);

    return 0;
}


/*
 * If there is a row in this object matching the given column in the the given
 * row (in buf, a tab separated table row), then return the row number,
 * otherwise -1.
 *
 */
int TabTable::findRow(const char* tableRow, int col) 
{
    int i, row, n = numCols_-1;
    char* colValues[MAX_COLUMNS];
    char buf[MAX_ROW_SIZE];
    strncpy(buf, tableRow, sizeof(buf)-1);	// make a copy, since nulls are inserted by splitList
    splitList(buf, colValues);
    for (row = 0; row < numRows_; row++) {
	if (strcmp(colValues[col], table_[row*numCols_+col]) == 0)
	    return row;
    }
    return -1;
}


/*
 * local compare function for qsort (method sort), pass on to member method
 */
static int compareQsort(const void* e1, const void* e2)
{
    return thisPtr_->compareRows(*(int*)e1, *(int*)e2);
}


/* 
 * local compare function: try to compare numerically, if possible and return
 * <. = or > 0, as by strcmp.
 */
static int compareValues(const char* s1, const char* s2)
{
    // try numeric comparison first
    double d1, d2;
    int numeric = 2;
    if (!s1 || sscanf(s1, "%lf", &d1) != 1) 
	numeric--;
    if (!s2 || sscanf(s2, "%lf", &d2) != 1)
	numeric--;

    if (numeric) {
	if (d1 > d2)
	    return 1;
	if (d1 < d2)
	    return -1;
	return 0;
    }
    else return strcmp(s1, s2);
}


/*
 * compare the given rows and return <. = or > 0, as by strcmp.
 */
int TabTable::compareRows(int row1, int row2)
{
    int ret = 0;
    for (int i = 0; i < numSortCols_; i++) {
	char* item1 = table_[row1*numCols_+sortColIndexes_[i]]; 
	char* item2 = table_[row2*numCols_+sortColIndexes_[i]]; 
	if ((ret = compareValues(item1, item2)) != 0)
	    break;
    }
    return ret * sortOrder_;
}


/*
 * sort the contents of this tab table by the given columns
 * numSortCols -  the number of columns in the array
 * sortCols -     the array of columns to sort by
 * sortOrder -    >=0 means increasing, <0 means decreasing
 */
int TabTable::sort(int numSortCols, char** sortCols, int sortOrder)
{
    // since the compare function is a static member, it needs to know these
    thisPtr_ = this;
    numSortCols_ = numSortCols;
    sortOrder_ = (sortOrder >= 0 ? 1 : -1);
    sortCols_ = sortCols;
    sortStatus_ = 0;

    // use integer index instead of column name
    int colIndexes[MAX_COLUMNS];
    for(int i = 0; i < numSortCols; i++) {
	colIndexes[i] = colIndex(sortCols[i]);
	if (colIndexes[i] < 0)
	    colIndexes[i] = 0;
    }
    sortColIndexes_ = colIndexes;

    qsort((char*)index_, numRows_, sizeof(int), compareQsort);
    return sortStatus_;
}
 

/*
 * Search the given tab table file for upto maxRows rows where the given
 * column has the given value and fill this table with the resulting
 * rows.
 *
 * Args:
 *
 * filename         - tab table file to search
 *
 * searchCol        - index of the column to compare (0..n)
 *
 * value            - value for comparison
 *
 * maxRows          - max number of rows to find
 */
int TabTable::search(const char* filename, int searchCol, 
		     const char* value, int maxRows) 
{
    // open file to search
    ifstream is(filename);
    if (!is) 
	return sys_error("can't open file: ", filename);

    // get table header from stream into result
    if (head(is, *this) != 0)
	return ERROR;
    
    if (maxRows <= 0)
	return 0;

    if (numCols_ < 1)
	return error("no id column");

    return search(is, 1, &colNames_[searchCol], (char**)&value, (char**)&value, maxRows);
}
   

/*
 * Search the given tab table for upto maxRows rows where the given
 * column has the given value and fill this table with the resulting
 * rows.
 *
 * Args:
 *
 * table            - tab table to search
 *
 * searchCol        - index of the column to compare (0..n)
 *
 * value            - value for comparison
 *
 * maxRows          - max number of rows to find
 */
int TabTable::search(const TabTable& table, int searchCol, 
		     const char* value, int maxRows) 
{
    int tcols = table.numCols();
    if (tcols < 1)
	return error("table contains no columns");

    // copy table header for the result
    if (init(tcols, table.colNames(), "", 0) != 0)
	return ERROR;

    if (maxRows <= 0)
	return 0;
    
    return search(table, 1, &colNames_[searchCol], (char**)&value, (char**)&value, maxRows);
}
   
    
/*
 * Internal version: assumes header has already been read.  Search the
 * given stream for upto maxRows rows with columns values matching the
 * given arguments and fill this table with the resulting rows.
 *
 * Args:
 *
 * is                - stream positioned at first row after header
 *
 * numSearchCols     - number of columns in argument arrays
 *
 * searchCols        - names of the columns to compare
 *
 * minValues         - min/max values for comparison
 * maxValues
 *
 * maxRows           - max number of rows to find
 */
int TabTable::search(istream& is, int numSearchCols, char** searchCols, 
		     char** minValues, char** maxValues, int maxRows) 
{
    // read and search rows, put matching rows in "os"
    char buf[MAX_ROW_SIZE];
    ostrstream os;
    int n = 0;
    while (is.getline(buf, sizeof(buf))) {
	if (compareRow(buf, numSearchCols, searchCols, minValues, maxValues) == 0) {
	    os << buf << endl;
	    if (++n >= maxRows)
		break;
	}
    }

    os << ends;
    int status = init(numCols_, colNames_, os.str(), maxRows);
    delete os.str();
    return status;
}

    
/*
 * Internal version: assumes header has already been read.  Search the
 * given tab table for upto maxRows rows with columns values matching the
 * given arguments and fill this table with the resulting rows.
 *
 * Args:
 *
 * table             - tab table to search
 *
 * numSearchCols     - number of columns in argument arrays
 *
 * searchCols        - names of the columns to compare
 *
 * minValues         - min/max values for comparison
 * maxValues
 *
 * maxRows           - max number of rows to find
 */
int TabTable::search(const TabTable& table, int numSearchCols, char** searchCols, 
		     char** minValues, char** maxValues, int maxRows) 
{
    // read and search rows, put matching rows in "os"
    int trows = table.numRows();
    ostrstream os;
    int n = 0;
    for(int i = 0; i < trows; i++) {
	if (compareRow(table, i, numSearchCols, searchCols, minValues, maxValues) == 0) {
	    table.printRow(os, i);
	    if (++n >= maxRows)
		break;
	}
    }

    os << ends;
    int status = init(numCols_, colNames_, os.str(), maxRows);
    delete os.str();
    return status;
}


/*
 * Search the given tab table file for upto maxRows rows where the given
 * column has the given value and fill this table with the resulting
 * rows.
 *
 * Args:
 *
 * filename         - tab table file to search
 *
 * searchCol        - name of the column to compare
 *
 * value            - value for comparison
 *
 * maxRows          - max number of rows to find
 */
int TabTable::search(const char* filename, const char* searchCol, 
		     const char* value, int maxRows) 
{
    return search(filename, 1, (char**)&searchCol, (char**)&value, 
		  (char**)&value, maxRows);
}


/*
 * Search the given tab table for upto maxRows rows where the given
 * column has the given value and fill this table with the resulting
 * rows.
 *
 * Args:
 *
 * table            - tab table to search
 *
 * searchCol        - name of the column to compare
 *
 * value            - value for comparison
 *
 * maxRows          - max number of rows to find
 */
int TabTable::search(const TabTable& table, const char* searchCol, 
		     const char* value, int maxRows) 
{
    return search(table, 1, (char**)&searchCol, (char**)&value, 
		  (char**)&value, maxRows);
}


/*
 * Search the given tab table file for upto maxRows rows with columns
 * values matching the given arguments and fill this table with the
 * resulting rows.
 *
 * Args:
 *
 * filename          - tab table file to search
 * numSearchCols     - number of columns in argument arrays
 *
 * searchCols        - names of the columns to compare
 *
 * minValues         - min/max values for comparison
 * maxValues
 *
 * maxRows           - max number of rows to find
 */
int TabTable::search(const char* filename, int numSearchCols, char** searchCols, 
		     char** minValues, char** maxValues, int maxRows) 
{
    // open file to search
    ifstream is(filename);
    if (!is) 
	return sys_error("can't open file: ", filename);

    // get table header from stream into result
    if (head(is, *this) != 0)
	return ERROR;
    
    if (maxRows <= 0)
	return 0;

    if (numCols_ < 1)
	return 0;

    return search(is, numSearchCols, searchCols, minValues, maxValues, maxRows);
}


/*
 * given a buffer containing a row for this table, compare the row with
 * the given column min and max values and return 0 if there is a match.
 *
 * (This method simply splits buf into columns and calls the overeloaded
 * method below, see the comments there)
 */
int TabTable::compareRow(char* buf, int numSearchCols, char** searchCols, 
			 char** minValues, char** maxValues) 
{
    // need to make a copy of the row, since splitList replaces tabs with null chars
    char buf2[MAX_ROW_SIZE];
    strcpy(buf2, buf);

    char* colValues[MAX_COLUMNS];
    if (splitList(buf2, colValues) != 0)
	return ERROR;
    
    return compareRow(colValues, numSearchCols, searchCols, minValues, maxValues);
}



/*
 * given a row with columns for this table, compare the row with the
 * given column min and max values and return 0 if there is a match.
 *
 * The first argument represents the row data to be searched. The rest of
 * the arguments describe the search condition such that
 *
 * 	minValue[i] <= colValues[i] <= maxValue[i]
 *
 * Either minValues or maxValues, or any of thier elements may be NULL,
 * to indicate + or - infinity.
 *
 * A numeric comparison is done, if possible.
 *
 * If all of the specified conditions are met, 0 is returned.
 */
int TabTable::compareRow(char** colValues, int numSearchmCols, char** searchCols, 
			 char** minValues, char** maxValues) 
{
    // for each search column specified...
    for (int col = 0; col < numSearchmCols; col++) {
	// see if this table has a column by that name...
	int i = inputColIndex(searchCols[col]);
	if (i < 0) 
	    return 1;		// can't compare non-existant column

	if (compareCol(colValues[i], 
		       (minValues ? minValues[col] : (char*)NULL), 
		       (maxValues ? maxValues[col] : (char*)NULL)) != 0) 
	    return 1;		// column value failed condition
    }

    return 0;			// condition passed
}

/*
 * given a tab table and a row number, compare the row with
 * the given column min and max values and return 0 if there is a match.
 */
int TabTable::compareRow(const TabTable& table, int row, int numSearchCols, 
			 char** searchCols, char** minValues, char** maxValues) 
{
    // for each search column specified...
    for (int col = 0; col < numSearchCols; col++) {
	// see if this table has a column by that name...
	int i = inputColIndex(searchCols[col]);
	if (i < 0) 
	    return 1;		// can't compare non-existant column

	char* s;		// get column value in table
	if (table.get(row, i, s) != 0)
	    return 1;

	if (compareCol(s, 
		       (minValues ? minValues[col] : (char*)NULL), 
		       (maxValues ? maxValues[col] : (char*)NULL)) != 0) 
	    return 1;		// column value failed condition
    }

    return 0;			// condition passed
}


/*
 * return 0 if the given value is in the given range, doing a numeric comparison if
 * possible.
 */
int TabTable::compareCol(const char* value, const char* minValue, const char* maxValue)
{
    // try numeric comparison first
    double d, dmin, dmax;
    int numeric = 2;
    if (!minValue || sscanf(minValue, "%lf", &dmin) != 1) {
	dmin = -HUGE_VAL;
	numeric--;
    }
    if (!maxValue || sscanf(maxValue, "%lf", &dmax) != 1) {
	dmax = HUGE_VAL;
	numeric--;
    }

    if (numeric && sscanf(value, "%lf", &d) == 1) {
	if (d < dmin || d > dmax)
	    return 1;	// condition failed
    }
    else {
	// try string comparison
	if ((minValue && strcmp(minValue, value) > 0) ||  
	    (maxValue && strcmp(maxValue, value) < 0))
	    return 1;		// condition failed
    }
    return 0;			// condition passed
}


/*
 * print the table rows to the given stream
 */
int TabTable::printRows(ostream& os) const 
{
    int row;
    for (row = 0; row < numRows_; row++) {
	printRow(os, row);
    }
    return 0;
}


/*
 * print the given table row to the given stream
 */
int TabTable::printRow(ostream& os, int row) const
{
    int n = numCols_-1, r = index_[row];
    
    for (int col = 0; col < numCols_; col++) {
	os << table_[r*numCols_+col];
	if (col < n)
	    os << '\t';
    }
    os << endl;
    return 0;
}


/*
 * static method: 
 * read the heading info from the given file and put it in the given object.
 */
int TabTable::head(const char* filename, TabTable& t)
{
    ifstream is(filename);
    if (!is) {
	return sys_error("can't open file: ", filename);
    }
    return head(is, t);
}


/*
 * static method: 
 * read the heading info from the given stream and put it in the given object
 */
int TabTable::head(istream& is, TabTable& t)
{
    ostrstream os;
    char buf[MAX_HEADER_SIZE];
    while (is.getline(buf, sizeof(buf))) {
	os << buf << endl;
	if (buf[0] == '-') 
	    break;
    }
    os << ends;
    int status = t.init(os.str());
    delete os.str();
    return status;
}


/*
 * compare headings in this table and the given one and return 0
 * if they are the same.
 */
int TabTable::compareHeadings(const TabTable& t)
{
    int ncols = numCols();
    if (t.numCols() != ncols)
	return 1;
    for (int i = 0; i < ncols; i++)
	if (strcmp(colName(i), t.colName(i)) != 0)
	    return 1;
    return 0;
}


/*
 * check the given row and col index and generate an error if out of range
 */
int TabTable::checkTableIndex(int row, int col) const
{
    if (row < 0 || row >= numRows_) {
	char msg[80];
	sprintf(msg, "row index %d out of range (max %d)", row, numRows_-1);
	return error(msg);
    }

    if (col < 0 || col >= numCols_) {
	char msg[80];
	sprintf(msg, "column index %d out of range (max %d)", col, numCols_-1);
	return error(msg);
    }
    return 0;
}


/*
 * get the value at the given row,column as a char string
 */
int TabTable::get(int row, int col, char*& value) const
{
    if (checkTableIndex(row, col) != 0) 
	return ERROR;
    
    value = table_[index_[row]*numCols_+col]; 
    return 0;
}


/*
 * get the value at the given row,column as an int
 */
int TabTable::get(int row, int col, int& value) const
{ 
    char* p;
    if (get(row, col, p) != 0)
	return 1;		// error
    if (sscanf(p, "%d", &value) != 1)
	return tab_error(row, col, "int", p);
    return 0;
}


/*
 * get the value at the given row,column as a double
 */
int TabTable::get(int row, int col, double& value) const
{
    char* p;
    if (get(row, col, p) != 0)
	return 1;
    if (sscanf(p, "%lf", &value) != 1)
	return tab_error(row, col, "double", p);
    return 0;
}


/*
 * get the value at the given row,column as a float
 */
int TabTable::get(int row, int col, float& value) const
{
    char* p;
    double d;
    if (get(row, col, p) != 0)
	return 1;		// error
    if (sscanf(p, "%f", &value) != 1)
	return tab_error(row, col, "float", p);
    return 0;
}


/*
 * get the value at the given row,column as a short int
 */
int TabTable::get(int row, int col, short& value) const
{
    char* p;
    int i;
    if (get(row, col, p) != 0)
	return 1;		// error
    if (sscanf(p, "%d", &i) != 1)
	return tab_error(row, col, "short", p);
    value = i;
    return 0;
}


/*
 * get the value at the given row,column as a char
 */
int TabTable::get(int row, int col, char& value) const
{
    char* p;
    if (get(row, col, p) != 0)
	return 1;		// error
    value = *p;
    return 0;
}


/*
 * get the value at the given row,column
 * (just convert col name to index and call the inherited method)
 * - use a macro, since the code is always the same - just different 
 * types.
 */

#define TGET(T) \
int TabTable::get(int row, const char* s, T& value) const \
{ \
    int col = inputColIndex(s); \
    if (col < 0) \
	return error("invalid result column: ", s); \
    return TabTable::get(row, col, value); \
}

TGET(char*)
TGET(int)
TGET(short)
TGET(double)
TGET(float)
TGET(char)

