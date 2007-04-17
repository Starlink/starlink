// -*-c++-*-
#ifndef _TcsCatalogObject_h_
#define _TcsCatalogObject_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsCatalogObject.h,v 1.1.1.1 2006/01/12 16:36:27 abrighto Exp $
 *
 * TcsCatalogObject.h - class representing one row of results from a 
 *                      TcsCatalog query.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */

using namespace std;

// NULL values
//#define TCS_CATALOG_NULL_INT 4294967294      /* (2^32 - 1) */
#define TCS_CATALOG_NULL_DOUBLE 1.E-300


/* 
 * This object represents the contents of one row of GSC/PPM results.
 * Missing values are set to the appropriate null value (see above).
 */
class TcsCatalogObject {
private:
    char id_[64];		// object catalog id
    double ra_;			// Alpha coordinate for the target in decimal degrees
    double dec_;		// Delta coordinate for the target in decimal degrees
    char cooSystem_[8];		// Equinox system and equinox ("B1950" or "J2000")
    double epoch_;		// Epoch expressed as decimal year
    double pma_;		// Proper Motion Alpha in radians/year (-10.0 to 10.0)
    double pmd_;		// Proper Motion Delta in radians/year (-10.0 to 10.0)
    double radvel_;		// radial velocity in km/sec (-200000 to 200000)
    double parallax_;		// Parallax in arcseconds (-10000 to 10000)
    char cooType_[4];		// Coordinate type as "M" for mean or "A" for apparent character
    char band_[4];		// Magnitude wavelength band ("V")
    double mag_;		// Object's magnitude in given band
    char* more_;		// An HTTP URL pointing to more info on the object
    char* preview_;		// An HTTP URL pointing to an image of the object
    double distance_;		// distance to center of the field 
    double pa_;			// position angle based on center of the field 
    
public:
    // constructor: initialize all fields to null
    TcsCatalogObject();
    
    // copy constructor
    TcsCatalogObject(const TcsCatalogObject&);

    // destructor
    ~TcsCatalogObject();

    // assignment
    TcsCatalogObject& operator=(const TcsCatalogObject &);

    // output operator
    friend ostream& operator<<(ostream&, const TcsCatalogObject&);

    // print this object to the given buffer
    void print(char* buf, int bufsize);

    // print the headings to match the output of '<<' above 
    static void printHeadings(ostream& os);
    static void printHeadings(char* buf, int bufsize);

    // print this object as a tab separated row
    int printTableRow(ostream&);
 
    // result all fields to default values
    void reset();

    // return true if the given value is null (by convention)
    //static int isNull(int v) {return (v == int(TCS_CATALOG_NULL_INT));}
    static int isNull(double v) {return (v == TCS_CATALOG_NULL_DOUBLE);}
    static int isNull(const char* v) {return (!v || !*v);}

    // set fields, with range checking, return 0 if OK
    int id(const char*);
    int ra(double);
    int dec(double);
    int cooSystem(const char*);
    int epoch(double);
    int pma(double);
    int pmd(double);
    int radvel(double);
    int parallax(double);
    int cooType(const char*);
    int band(const char*);
    int mag(double);
    int more(const char*);
    int preview(const char*);
    int distance(double);
    int pa(double);

    // member access: return member value
    const char* id() {return id_;}
    double ra() {return ra_;}
    double dec() {return dec_;}
    const char* cooSystem() {return cooSystem_;}
    double epoch() {return epoch_;}
    double pma() {return pma_;}
    double pmd() {return pmd_;}
    double radvel() {return radvel_;}
    double parallax() {return parallax_;}
    const char* cooType() {return cooType_;}
    const char* band() {return band_;}
    double mag() {return mag_;}
    const char* more() {return more_ ? more_ : "";}
    const char* preview() {return preview_ ? preview_ : "";}
    double distance() {return distance_;}
    double pa() {return pa_;}

    // for compat. with AstroCatalog

    // return the number of columns in the catalog
    static int numCols();

    // return the column names
    static char** colNames();
    static const char* colName(int col);

    // return the column index for the given column name
    static int colIndex(const char* colName);

    // return true if the catalog contains the given column 
    static int hasCol(const char* name) {return (colIndex(name) >= 0);}

    // Compare the given column (member) of this object with the given object.
    int compare(const TcsCatalogObject& obj, int colIndex);
    int compare(const TcsCatalogObject& obj, char* colName);
};

#endif /* _TcsCatalogObject_h_ */
