/*** File libwcs/wcscat.h
 *** September 2, 2004
 *** By Doug Mink, dmink@cfa.harvard.edu
 *** Copyright (C) 1998-2004
 *** Smithsonian Astrophysical Observatory, Cambridge, MA, USA

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Correspondence concerning WCSTools should be addressed as follows:
           Internet email: dmink@cfa.harvard.edu
           Postal address: Doug Mink
                           Smithsonian Astrophysical Observatory
                           60 Garden St.
                           Cambridge, MA 02138 USA
 */

/* Source catalog flags and subroutines */

/* Source catalog flags returned from CatCode */
#define GSC		1	/* HST Guide Star Catalog */
#define UJC		2	/* USNO UJ Star Catalog */
#define UAC		3	/* USNO A Star Catalog */
#define USAC		4	/* USNO SA Star Catalog */
#define SAO		5	/* SAO Star Catalog */
#define IRAS		6	/* IRAS Point Source Catalog */
#define PPM		7	/* PPM Star Catalog */
#define TYCHO		8	/* Tycho Star Catalog */
#define UA1		9	/* USNO A-1.0 Star Catalog */
#define UA2		10	/* USNO A-2.0 Star Catalog */
#define USA1		11	/* USNO SA-1.0 Star Catalog */
#define USA2		12	/* USNO SA-2.0 Star Catalog */
#define HIP		13	/* Hipparcos Star Catalog */
#define ACT		14	/* USNO ACT Star Catalog */
#define BSC		15	/* Yale Bright Star Catalog */
#define TYCHO2		16	/* Tycho-2 Star Catalog */
#define USNO		17	/* USNO-format plate catalog */
#define TMPSC		18	/* 2MASS All-Sky Point Source Catalog */
#define GSCACT		19	/* GSC-ACT revised Guide Star Catalog */
#define GSC2		20	/* GSC II version 2.2 */
#define UB1		21	/* USNO B-1.0 Star Catalog */
#define UCAC1		22	/* USNO CCD Astrograph Catalog 1.0 */
#define UCAC2		23	/* USNO CCD Astrograph Catalog 2.0 */
#define TMIDR2		24	/* 2MASS IDR2 Point Source Catalog */
#define YB6		25	/* USNO YB6 Catalog */
#define SDSS		26	/* Sloan Digital Sky Survey Catalog */
#define TMXSC		27	/* 2MASS Extended Source Catalog */
#define TABCAT		-1	/* StarBase tab table catalog */
#define BINCAT		-2	/* TDC binary catalog */
#define TXTCAT		-3	/* TDC ASCII catalog */
#define WEBCAT		-4	/* Tab catalog via the web */
#define NUMCAT		27	/* Number of predefined catalogs */

/* Subroutines for dealing with catalogs */
int CatCode();		/* Return catalog type code */
int RefCat();		/* Return catalog type code, title, coord. system */
char *CatName();	/* Return catalog name given catalog type code */
char *CatSource();	/* Return catalog source description given catalog type code */
char *ProgCat();	/* Return catalog name given program name used */
char *ProgName();	/* Return program name given program path used */
char *CatName();	/* Return catalog name given catalog type code */
void CatID();		/* Return catalog ID keyword given catalog type code */
void CatNum();		/* Return formatted source number */
int CatNumLen();	/* Return length of source numbers */
int CatNdec();		/* Return number of decimal places in source numbers */
void CatMagName();	/* Return name of specified magnitude */
int CatMagNum();	/* Returns number of magnitude specified by letter as int */

int NumNdec();		/* Return number of decimal places in a number */
int StrNdec();		/* Return number of decimal places in numeric string */
void SearchLim();	/* Compute limiting RA and Dec */
void RefLim();		/* Compute limiting RA and Dec in new system */
int isacat();		/* Return 1 if string is name of ASCII catalog file */
int ageti4();		/* Extract int value from keyword= value in string */
int agetr8();		/* Extract double value from keyword= value in string */
int agets();		/* Extract value from keyword= value in string */
void bv2sp();		/* Approximate main sequence spectral type from B - V */

/* Subroutines for extracting sources from catalogs by sky region */
int gscread();		/* Read sources from HST Guide Star Catalog */
int gsc2read();		/* Read sources from GSC II Catalog */
int sdssread();		/* Read sources from SDSS Catalog */
int tmcread();		/* Read sources from 2MASS Point Source Catalog */
int uacread();		/* Read sources from USNO A or SA Catalog */
int ubcread();		/* Read sources from USNO B Catalog */
int ucacread();		/* Read sources from USNO UCAC 1 Catalog */
int ujcread();		/* Read sources from USNO J Catalog */
int tabread();		/* Read sources from tab table catalog */
int binread();		/* Read sources from SAO TDC binary format catalog */
int ctgread();		/* Read sources from SAO TDC ASCII format catalog */
int actread();		/* Read sources from USNO ACT Catalog */
int ty2read();		/* Read sources from Tycho 2 Catalog */
int webread();		/* Read sources from catalog on the World Wide Web */
int daoread();		/* Read image source positions from x y mag file */
int daoopen();		/* Open image source position x y mag file */
char *daoline();	/* Read line from image source position x y mag file */

/* Subroutines for extracting sources from catalogs by ID number */
int gscrnum();		/* Read sources from HST Guide Star Catalog */
int tmcrnum();		/* Read sources from 2MASS Point Source Catalog */
int uacrnum();		/* Read sources from USNO A or SA Catalog */
int ubcrnum();		/* Read sources from USNO B Catalog */
int ucacrnum();		/* Read sources from USNO UCAC 1 Catalog */
int ujcrnum();		/* Read sources from USNO J Catalog */
int tabrnum();		/* Read sources from tab table catalog */
int binrnum();		/* Read sources from SAO TDC binary format catalog */
int ctgrnum();		/* Read sources from SAO TDC ASCII format catalog */
int actrnum();		/* Read sources from USNO ACT Catalog */
int ty2rnum();		/* Read sources from Tycho 2 Catalog */
int webrnum();		/* Read sources from catalog on the World Wide Web */

/* Subroutines for extracting sources from catalogs by date range */
int ctgrdate();		/* Read sources from SAO TDC ASCII format catalog */

/* Subroutines for putting sources from catalogs into FITS WCS images */
int gscbin();		/* Read sources from HST Guide Star Catalog */
int tmcbin();		/* Read sources from 2MASS Point Source Catalog */
int uacbin();		/* Read sources from USNO A or SA Catalog */
int ubcbin();		/* Read sources from USNO B Catalog */
int ucacbin();		/* Read sources from USNO UCAC 1 Catalog */
int ujcbin();		/* Read sources from USNO J Catalog */
int tabbin();		/* Read sources from tab table catalog */
int binbin();		/* Read sources from SAO TDC binary format catalog */
int ctgbin();		/* Read sources from SAO TDC ASCII format catalog */
int actbin();		/* Read sources from USNO ACT Catalog */
int ty2bin();		/* Read sources from Tycho 2 Catalog */

char *sdssc2t();	/* Convert SDSS buffer from comma- to tab-separated */
void setgsclass();	/* Set GSC object class */
void setuplate();	/* Set USNO catalog plate number to search */
int getuplate();	/* Get USNO catalog plate number to search */
void settabkey();	/* Set tab table keyword to read for object */
int ctgstar();		/* Read one star entry from ASCII catalog, 0 if OK */
int binstar();		/* Read one star entry from binary catalog, 0 if OK */
int tabstar();		/* Read one star entry from tab table catalog, 0 if OK */
struct TabTable *webopen();	/* Open tab table across the web */
char *webbuff();	/* Read URL into buffer across the web */
void setlimdeg();	/* Limit output in degrees (1) or hh:mm:ss dd:mm:ss (0) */

/* Subroutines for sorting tables of star positions and magnitudes */
#define SORT_UNSET	-1	/* Catalog sort flag not set yet */
#define SORT_NONE	0	/* Do not sort catalog output */
#define SORT_MAG	1	/* Sort output by magnitude */
#define SORT_DIST	2	/* Sort output by distance from center */
#define SORT_RA		3	/* Sort output by right ascension */
#define SORT_DEC	4	/* Sort output by declination */
#define SORT_X		5	/* Sort output by image X coordinate */
#define SORT_Y		6	/* Sort output by image Y coordinate */
#define SORT_MERGE	7	/* Merge close catalog objects */
void XSortStars();
void YSortStars();
void RASortStars();
void DecSortStars();
void MagSortStars();
void FluxSortStars();
int MergeStars();

/* Data structure for SAO TDC ASCII and binary star catalog entries */
struct Star {
    float rdum;
    float xno;		/* Catalog number */
    double ra;		/* Right Ascension (degrees) */
    double dec;		/* Declination (degrees) */
    char isp[24];	/* Spectral type or other 2-char identifier */
    short mag[11];	/* Up to 10 Magnitudes * 100 */
    double rapm;	/* RA proper motion (degrees per year) */
    double decpm;	/* Dec proper motion (degrees per year) */
    double xmag[11];	/* Up to 10 Magnitudes */
    double num;		/* Actual star number */
    int coorsys;	/* Coordinate system (WCS_J2000, WCS_B1950,...) */
    double equinox;	/* Equinox of coordinate system as fractional year */
    double epoch;	/* Epoch of position as fractional year */
    double parallax;	/* Parallax in arcseconds */
    double pxerror;	/* Parallax error in arcseconds */
    double radvel;	/* Radial velocity in km/sec, positive away */
    double dist;	/* Distance from search center in arcseconds */
    double size;	/* Semi-major axis in arcseconds */
    char *entry;	/* Line copied from input catalog */
    char objname[80];	/* Object name */
    int peak;		/* Peak flux per pixel in star image */
};

/* Catalog proper motion units */
#define PM_MASYR		1	/* milliarcseconds per year */
#define PM_ARCSECYR		2	/* arcseconds per year */
#define PM_DEGYR		3	/* degrees per year */
#define PM_RADYR		4	/* radians per year */
#define PM_TSECYR		5	/* seconds of time (RA) per century */
#define PM_ARCSECCEN		6	/* arcseconds per year */
#define PM_TSECCEN		7	/* seconds of time (RA) per century */
#define PM_MTSYR		8	/* milliseconds of time (RA) per year */

/* Data structure for SAO TDC ASCII and binary star catalogs */
struct StarCat {
    int star0;		/* Subtract from star number for file sequence number */
    int star1;		/* First star number in file */
    int nstars;		/* Number of stars in file */
    int stnum;		/* Star number format in catalog file:
			  <0: -stnum-character name at end instead of number
			   0:  no star i.d. numbers
			   1: Real*4 star i.d. numbers
			   2: Integer*4 <region><nnnn>
			   3: Integer*4 <region><nnnnn>
			   4: Integer*4 <nnnnnnnnn>
			   5: Character ID instead of number in ASCII files */
    int mprop;		/* 1 if proper motion is included */
			/* 2 if radial velocity is included */
    int nmag;		/* Number of magnitudes present
			   Negative for J2000 catalog */
    int nbent;		/* Number of bytes per star entry */
    int	rasorted;	/* 1 if RA-sorted, else 0 */
    int	ignore;		/* 1 if ignoring info after position and magnitude */
    FILE *ifcat;	/* File descriptor for catalog file */
    char isfil[24];	/* Star catalog file name */
    char isname[64];	/* Star catalog description */
    int  byteswapped;	/* 1 if catalog is byte-reversed from CPU */
    int  refcat;	/* Code for type of catalog (TXTCAT, BINCAT, etc.) */
    int  coorsys;	/* Coordinate system
			   B1950 J2000 Galactic Ecliptic */
    double epoch;	/* Epoch of catalog coordinates in years */
    double equinox;	/* Equinox of catalog coordinates in years */
    char inform;	/* Coordinate format
			   (B>inary D>egrees H>MS T>able U>SNO) */
    char incdir[128];	/* Catalog directory pathname */
    char incfile[32];	/* Catalog file name */
    int ncobj;		/* Length of object name in binary star entry */
    int nnfld;		/* Length of star number  */
    int nndec;		/* Number of decimal places in star number */
    int nepoch;		/* 1 if epoch of coordinates is present */
    int sptype;		/* 1 if spectral type is present in catalog */
    int plate;		/* 1 if plate or field number is present in catalog */
    char *catbuff;	/* Pointer to start of catalog */
    char *catdata;	/* Pointer to first entry in catalog */
    char *catline;	/* Pointer to current entry in catalog */
    char *catlast;	/* Pointer to one past end of last entry in catalog */
    int  istar;		/* Number of current catalog entry */
    struct TabTable *startab;	/* Structure for tab table catalog */
    int entid;		/* Entry number for ID */
    int entra;		/* Entry number for right ascension */
    int entdec;		/* Entry number for declination */
    int entmag[10];	/* Entry numbers for up to 10 magnitudes */
    int entpeak;	/* Entry number for peak counts */
    int entepoch;	/* Entry number for epoch of observation */
    int entdate;	/* Entry number for FITS-format date of observation */
    int entname;	/* Entry number for object name */
    int entadd;		/* Entry number for additional keyword */
    int entrpm;		/* Entry number for proper motion in right ascension */
    int entdpm;		/* Entry number for proper motion in declination */
    int entpx;		/* Entry number for parallax */
    int entpxe;		/* Entry number for parallax error */
    int entrv;		/* Entry number for radial velocity */
    int enttype;	/* Entry number for spectral type */
    int entsize;	/* Entry number for size of object */
    int rpmunit;	/* Units for RA proper motion (PM_x) */
    int dpmunit;	/* Units for DEC proper motion (PM_x) */
    char *caturl;	/* set if web search, else NULL */
    char keyid[16];	/* Entry name for ID */
    char keyra[16];	/* Entry name for right ascension */
    char keydec[16];	/* Entry name for declination */
    char keymag[10][16]; /* Entry name for up to 10 magnitudes */
    char keyrpm[16];	/* Entry name for right ascension proper motion */
    char keydpm[16];	/* Entry name for declination proper motion */
    char keypeak[16];	/* Entry name for integer code */
    char keytype[16];	/* Entry name for spectral type */
    char keyrv[16];	/* Entry name for radial velocity */
    char keyadd[16];	/* Entry name for additional keyword */
    char keyepoch[16];	/* Entry name for epoch */
};

/* Subroutines for reading headers of TDC binary and ASCII catalogs */
int isbin();
struct StarCat *binopen();
void binclose();
struct StarCat *ctgopen();
void ctgclose();

/* Data structure for tab table files */
struct TabTable {
    char *filename;	/* Name of tab table file */
    int nlines;		/* Number of entries in table */
    char *tabname;	/* Name of this table or NULL */
    char *tabbuff;	/* Pointer to start of saved tab table in memory */
    char *tabheader;	/* Pointer to start of line containing table header */
    char *tabhead;	/* Pointer to start of line containing column heading */
    char *tabdash;	/* Pointer to start of line with dashes after column headings */
    char *tabdata;	/* Pointer to start of first line of table data */
    int lhead;		/* Number of bytes before first data line */
    int iline;		/* Number of current line (1=first) */
    int lline;		/* Length in bytes of line buffer */
    char *tabline;	/* Pointer to start of current line */
    FILE *tcat;		/* File descriptor for tab table file */
    int ncols;		/* Number of columns per table entry */
    char **colname;	/* Column names */
    int *lcol;		/* Lengths of column header names */
    int *lcfld;		/* Number of columns in field (hyphens) */
    int lbuff;		/* Number of bytes in entire tab table */
};

/* Subroutines for extracting tab table information */
struct TabTable *tabopen();	/* Open tab table file */
struct StarCat *tabcatopen();	/* Open tab table catalog */
void tabcatclose();	/* Close tab table catalog */
int tabxyread();	/* Read x, y, and magnitude from tab table star list */
char *gettabline();	/* Find a specified line in a tab table */
int tabrkey();		/* Keyword values from tab table catalogs */
int tabcol();		/* Find column for name */
int tabgetk();		/* Get tab table entries for named column */
int tabgetc();		/* Get tab table entry for named column */
int tabgeti4();		/* Return 4-byte integer from tab table line */
int tabparse();		/* Aeturn column names and positions in tabtable */
double tabgetra();	/* Return right ascension in degrees from tab table*/
double tabgetdec();	/* Return declination in degrees from tab table*/
double tabgetpm();	/* Return RA or Dec p.m. in degrees from tab table*/
double tabgetr8();	/* Return double number from tab table line */
void tabclose();	/* Free all arrays left open by tab table structure */
char *gettaberr();	/* Return most recent tab table error message */
int istab();
int gettabndec();	/* Return number of decimal places in tab catalog ids */

#define MAXRANGE 20

/* Structure for dealing with ranges */
struct Range {
    double first;	/* Current minimum value */
    double last;	/* Current maximum value */
    double step;	/* Current step in value */
    double value;	/* Current value */
    double ranges[MAXRANGE*3];	/* nranges sets of first, last, step */
    int nvalues;	/* Total number of values in all ranges */
    int nranges;	/* Number of ranges */
    int irange;		/* Index of current range */
};

/* Subroutines for dealing with ranges */
struct Range *RangeInit();	/* Initialize range structure from string */
int isrange();		/* Return 1 if string is a range of numbers, else 0 */
int rgetn();		/* Return number of values in all ranges */
int rgeti4();		/* Return next number in range as integer */
double rgetr8();	/* Return next number in range as double */
void rstart();		/* Restart range */

/* Shapes for SAOimage region file output */
#define WCS_CIRCLE 1	/* circle shape for SAOimage plotting */
#define WCS_SQUARE 2	/* square shape for SAOimage plotting */
#define WCS_DIAMOND 3	/* diamond shape for SAOimage plotting */
#define WCS_CROSS 4	/* cross shape for SAOimage plotting */
#define WCS_EX 5	/* x shape for SAOimage plotting */
#define WCS_VAR 6	/* variable (+ and x) shape for HSTGSC plotting */
#define WCS_PCIRCLE 11	/* pixel circle shape for SAOimage plotting */
#define WCS_PSQUARE 12	/* pixel square shape for SAOimage plotting */
#define WCS_PDIAMOND 13	/* pixel diamond shape for SAOimage plotting */
#define WCS_PCROSS 14	/* pixel cross shape for SAOimage plotting */
#define WCS_PEX 15	/* pixel ex shape for SAOimage plotting */
#define WCS_PVAR 16	/* pixel variable (+ and x) shape for HSTGSC plotting */

/* Structure and subroutines for access to tokens within a string */
#define MAXTOKENS 1000    /* Maximum number of tokens to parse */
#define MAXWHITE 20     /* Maximum number of different whitespace characters */
struct Tokens {
    char *line;         /* Line which has been parsed */
    int lline;          /* Number of characters in line */
    int ntok;           /* Number of tokens on line */
    int nwhite;         /* Number of whitespace characters */
    char white[MAXWHITE];       /* Whitespace (separator) characters */
    char *tok1[MAXTOKENS];      /* Pointers to start of tokens */
    int ltok[MAXTOKENS];        /* Lengths of tokens */
    int itok;           /* Current token number */
};
int setoken();		/* Tokenize a string for easy decoding */
int nextoken();		/* Get next token from tokenized string */
int getoken();		/* Get specified token from tokenized string */

/* Subroutines for fitting and evaluating polynomials */
void polfit();		/* Fit polynomial coefficients */
double polcomp();	/* Evaluate polynomial from polfit coefficients */

/* Subroutines for VOTable output */
void vothead();		/* Print heading for VOTable SCAT output */
void vottail();		/* Terminate VOTable SCAT output */

/* Subroutines for version/date string */
void setrevmsg();	/* Set version/date string */
char *getrevmsg();	/* Return version/date string */

/* Sep 22 1998  New header file (star.h)
 * Oct 16 1998  Add more options for ASCII catalogs
 * Oct 20 1998  Add object name to binary files
 * Oct 21 1998	New file (wcscat.h)
 * Oct 26 1998	Combined wcscat.h and star.h
 * Oct 27 1998	Add SAOimage region shapes
 * Nov  9 1998	Add rasorted flag to catalog structure
 * Nov 20 1998	Add support for USNO A-2.0 and SA-2.0 catalogs
 * Dec  8 1998	Add support for the Hipparcos and ACT catalogs
 *
 * Jan 25 1999	Add declarations for tab table access
 * Jan 25 1999	Add declarations for dealing with ranges of numbers
 * Feb  2 1999	Add number of decimal places in star number to StarCat
 * Feb 11 1999	Add coordinate system info to star structure
 * Feb 11 1999	Change starcat.insys to starcat.coorsys for consistency
 * May 14 1999	Update Star and StarCat structure to cover tab tables
 * May 19 1999	Update StarCat structure to include epoch from catalog
 * June 4 1999	Add CatNumLen()
 * Jun 14 1999	Add SearchLim()
 * Jun 30 1999	Add isrange()
 * Jul  1 1999	Add declarations for date/time conversions in dateutil.c
 * Jul  2 1999	Add rstart()
 * Jul 26 1999	Add Yale Bright Star Catalog
 * Aug 16 1999	Add RefLim() to get converted search coordinates right
 * Aug 25 1999	Add ACT catalog
 * Sep 10 1999	Move special case setting from argument list to subroutines
 * Sep 13 1999	Add subroutines to access data structure for single stars
 * Oct  1 1999	Add structure and subroutines for tokenized strings
 * Oct 22 1999	Change cat*() to ctg*() to avoid system conflict
 * Oct 29 1999	Add tabget() subroutines
 * Nov  1 1999	Increase maximum number of tokens on a line from 20 to 100
 * Nov  2 1999	Move date utilities to fitsfile.h
 *
 * Jan 10 2000	Add column names to catalog data structure
 * Jan 11 2000	Add gettabndec()
 * Feb  9 2000	Add proper motion entry information to star data structure
 * Feb 16 2000	Add gettaberr() to return tab table error message
 * Mar  1 2000	Add isfile() and agets() to help with ASCII files
 * Mar  8 2000	Add ProgCat() to return catalog name from program name used
 * Mar  8 2000	Add ProgName() to extract program name from path used
 * Mar 10 2000	Add PropCat() to tell whether a catalog has proper motions
 * Mar 27 2000	Add tabxyread()
 * Apr  3 2000	Add option in catalog structure to ignore extra info
 * May 22 2000	Add Tycho 2 support, bv2sp()
 * May 26 2000	Add separate pointer to header in tab table structure
 * May 26 2000	Add separate pointer to table name in tab table structure
 * Jul 12 2000	Add catalog type code to ctalog data structure
 * Sep 20 2000	Add isacat() to detect ASCII catalog files
 * Sep 25 2000	Add starcat.sptype to flag spectral type in catalog
 * Oct 23 2000	Add USNO plate catalog to catalog type table
 * Oct 26 2000	Add proper motion flags for seconds and arcseconds per century
 * Oct 31 2000	Add proper motion flags for milliseconds per year
 * Nov  2 2000	Add parallax and radial velocity to star structure
 * Nov 21 2000	Add WEBCAT catalog type for tab ctalogs returned from the Web
 * Nov 22 2000	Add webread() and webrnum()
 * Nov 28 2000	Add tabparse()
 * Nov 30 2000	Add spectral type to catalog header; make star->isp 4 char.
 * Dec 13 2000	Add StrNdec() to get number of decimal places in number strings
 * Dec 15 2000	Add CatNdec() to get number of decimal places in source numbers
 * Dec 18 2000	Drop PropCat(), a cludgy proper motion flag
 *
 * Mar 22 2001	Add web search flag in catalog data structure
 * Mar 27 2001	Add shapes in pixels to SAOimage region options
 * May 14 2001	Add 2MASS Point Source Catalog flags
 * May 22 2001	Add declination sorting
 * May 24 2001	Add 2MASS Point Source Catalog subroutines
 * May 29 2001	Add length of star number to catalog structure
 * May 30 2001	Add third magnitude for tab tables to catalog structure
 * Jun 15 2001	Add CatName() and CatID()
 * Jun 19 2001	Add parallax error to catalog and star structures
 * Jun 20 2001	Add webopen(), GSC2, fourth magnitude to star and starcat
 * Jul 12 2001	Add separate web access subroutine, webbuff()
 * Jul 23 2001	Add ageti4() and agetr8()
 * Jul 24 2001	Add polfit() and polcomp()
 * Aug  8 2001	Add keyrv and option to set mprop to 2 to include rv/cz
 * Sep 10 2001	Add entry line and distance from search center to Star
 * Sep 13 2001	Add YSortStars() and SORT_Y
 * Sep 14 2001	Add lbuff to TabTable structure
 * Sep 20 2001	Add CatMagName()
 * Sep 25 2001	Move isfile() to fitsfile.h
 * Oct 16 2001	Add tabdash pointer to tabtable data structure
 *
 * Apr  9 2002	Fix typo in gettaberr() declaration
 * Apr 10 2002	Add CatMagNum()
 * May  6 2002	Increase object name length from 31 to 79 characters
 * May 13 2002	Add NumNdec(), gsc2read(), and gsc2rnum()
 * Aug  6 2002	Make magnitude entries and positions vectors of 10
 * Oct 30 2002	Add epoch keyword and FITS date to StarCat data structure
 *
 * Jan 16 2003	Add USNO-B1.0 catalog
 * Mar 24 2003	Add CatCde() to get only catalog code
 * Apr  3 2003	Add ubcread(), ubcrnum(), and FluxSortStars()
 * Apr  3 2003	Drop gsc2rnum()
 * Apr 14 2003	Add setrevmsg() and getrevmsg()
 * Apr 24 2003	Add UCAC1 and UCAC2, ucacread() and ucacrnum()
 * May 20 2003	Add TMIDR2 for 2MASS PSC Interim Data Release 2
 * Sep 16 2003	Add SORT_MERGE for scat
 * Sep 25 2003	Add *bin() subroutines for catalog binning
 * Dec  3 2003	Add USNO YB6 catalog
 *
 * Jan  5 2004	Add SDSS catalog
 * Jan 12 2004	Add 2MASS Extended Source catalog and size to star structure
 * Jan 14 2004	Add CatSource() subroutine to simplify help message creation
 * Jan 22 2004	Add setlimdeg() to print limit coordinates in degrees
 * Mar 16 2004	Add MergeStars()
 * Apr 23 2004	Add ctgrdate()
 * Aug 31 2004	Increase MAXTOKENS from 100 to 200
 * Sep  2 2004	Increase MAXTOKENS from 200 to 1000
 */
