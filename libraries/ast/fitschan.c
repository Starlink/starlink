/*
*class++
*  Name:
*     FitsChan

*  Purpose:
*     I/O Channel using FITS header cards to represent Objects.

*  Constructor Function:
c     astFitsChan
f     AST_FITSCHAN

*  Description:
*     A FitsChan is a specialised form of Channel which supports I/O
*     operations involving the use of FITS (Flexible Image Transport
*     System) header cards. Writing an Object to a FitsChan (using
c     astWrite) will, if the Object is suitable, generate a
f     AST_WRITE) will, if the Object is suitable, generate a
*     description of that Object composed of FITS header cards, and
*     reading from a FitsChan will create a new Object from its FITS
*     header card description.
*
*     While a FitsChan is active, it represents a buffer which may
*     contain zero or more 80-character "header cards" conforming to
*     FITS conventions. Any sequence of FITS-conforming header cards
*     may be stored, apart from the "END" card whose existence is
*     merely implied.  The cards may be accessed in any order by using
*     the FitsChan's integer Card attribute, which identifies a "current"
*     card, to which subsequent operations apply. Searches
c     based on keyword may be performed (using astFindFits), new
c     cards may be inserted (astPutFits) and existing ones may be
c     deleted (astDelFits).
f     based on keyword may be performed (using AST_FINDFITS), new
f     cards may be inserted (AST_PUTFITS) and existing ones may be
f     deleted (AST_DELFITS).
*
*     When you create a FitsChan, you have the option of specifying
*     "source" and "sink" functions which connect it to external data
*     stores by reading and writing FITS header cards. If you provide
*     a source function, it is used to fill the FitsChan with header
*     cards when it is created. If you do not provide a source
*     function, the FitsChan remains empty until you explicitly enter
c     data into it (e.g. using astPutFits or astWrite). If you
f     data into it (e.g. using AST_PUTFITS or AST_WRITE). If you
*     provide a sink function, it is used to deliver any remaining
*     contents of a FitsChan to an external data store when the
*     FitsChan is deleted. If you do not provide a sink function, any
*     header cards remaining when the FitsChan is deleted will be
*     lost, so you should arrange to extract them first if necessary
c     (e.g. using astFindFits or astRead).
f     (e.g. using AST_FINDFITS or AST_READ).
*     
*     Coordinate system information may be described using FITS header
*     cards using several different conventions, termed
*     "encodings". When an AST Object is written to (or read from) a
*     FitsChan, the value of the FitsChan's Encoding attribute
*     determines how the Object is converted to (or from) a
*     description involving FITS header cards. In general, different
*     encodings will result in different sets of header cards to
*     describe the same Object. Examples of encodings include the DSS
*     encoding (based on conventions used by the STScI Digitised Sky
*     Survey data), the FITS-WCS encoding (based on a proposed FITS
*     standard) and the NATIVE encoding (a near loss-less way of
*     storing AST Objects in FITS headers).
*
*     The available encodings differ in the range of Objects they can
*     represent, in the number of Object descriptions that can coexist
*     in the same FitsChan, and in their accessibility to other
*     (external) astronomy applications (see the Encoding attribute
*     for details). Encodings are not necessarily mutually exclusive
*     and it may sometimes be possible to describe the same Object in
*     several ways within a particular set of FITS header cards by
*     using several different encodings.
*
c     The detailed behaviour of astRead and astWrite, when used with
f     The detailed behaviour of AST_READ and AST_WRITE, when used with
*     a FitsChan, depends on the encoding in use. In general, however,
c     all use of astRead is destructive, so that FITS header cards
f     all use of AST_READ is destructive, so that FITS header cards
*     are consumed in the process of reading an Object, and are
*     removed from the FitsChan.
*
*     If the encoding in use allows only a single Object description
*     to be stored in a FitsChan (e.g. the DSS, FITS-WCS and FITS-IRAF
c     encodings), then write operations using astWrite will
f     encodings), then write operations using AST_WRITE will
*     over-write any existing Object description using that
*     encoding. Otherwise (e.g. the NATIVE encoding), multiple Object
*     descriptions are written sequentially and may later be read
*     back in the same sequence.

*  Inheritance:
*     The FitsChan class inherits from the Channel class.

*  Attributes:
*     In addition to those attributes common to all Channels, every
*     FitsChan also has the following attributes:
*
*     - Card: Index of current FITS card in a FitsChan
*     - Encoding: System for encoding Objects as FITS headers
*     - FitsDigits: Digits of precision for floating-point FITS values
*     - Ncard: Number of FITS header cards in a FitsChan

*  Functions:
c     In addition to those functions applicable to all Channels, the
c     following functions may also be applied to all FitsChans:
f     In addition to those routines applicable to all Channels, the
f     following routines may also be applied to all FitsChans:
*
c     - astDelFits: Delete the current FITS card in a FitsChan
f     - AST_DELFITS: Delete the current FITS card in a FitsChan
c     - astFindFits: Find a FITS card in a FitsChan by keyword
f     - AST_FINDFITS: Find a FITS card in a FitsChan by keyword
c     - astPutFits: Store a FITS header card in a FitsChan
f     - AST_PUTFITS: Store a FITS header card in a FitsChan

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink, RAL)

*  History:
*     11-DEC-1996 (DSB):
*        Original version.
*     20-MAR-1997 (DSB):
*        Made keyword setting and getting functions protected instead of
*        public. Renamed public methods. Added Ncard attribute.
*     20-MAY-1997 (RFWS):
*        Tidied public prologues.
*     30-JUN-1997 (DSB):
*        Added support for reading post-2000 DATE-OBS strings. Reading DSS
*        or FITS-WCS objects now returns NULL unless the FitsChan is
*        positioned at the start-of-file prior to the read. Bug fixed
*        which caused Ncard to be returned too large by one. Removed
*        dependancy on hard-wired header and footer text in Native
*        FitsChans.
*     18-AUG-1997 (DSB):
*        Bug fixed in WcsNative which caused incorrect CRVAL values
*        to be used if the axes needed permuting. Values assigned to the
*        Projection attribute fo the SkyFrames created by astRead.
*     2-SEP-1997 (DSB):
*        Added the IRAF convention that EPOCH=0.0 really means EPOCH=1950.0
*        (the EPOCH keyword is deprecated in the new FITS-WCS conventions
*        and is taken always as a Besselian epoch).
*     19-SEP-1997 (DSB):
*        Corrected interpretation of the FITS CD matrix. 
*     25-SEP-1997 (DSB):
*        o  Fix bug in LinearMap which caused it always to detect a linear
*        mapping. For instance, this allowed DssMaps to be erroneously 
*        written out using FITS-WCS encoding with a CAR projection.
*        o  Assign a full textual description to SkyFrame's Projection
*        attribute instead of a 3 letter acronym.
*        o  If DATE-OBS >= 1999.0 then DATE-OBS is now written in new 
*        Y2000 format. For DATE-OBS < 1999.0, the old format is written.
*        o  Add new attribute CDMatrix to determine whether PC or CD
*        matrices should be used when writing objects using FITS-WCS
*        encoding.
*        o  Modified the way floating point values are formatted to omit
*        unnecessary leading zeros from the exponent (i.e. E-5 instead of
*        E-05).
*        o  New-line characters at the end of supplied header cards are now 
*        ignored.
*        o  Cater for EQUINOX specified as a string prefixed by B or J
*        rather than as a floating point value (some HST data does this).
*        o  Corrected SetValue so that it always inserts comment cards 
*        rather than over-write existing comment cards. Previously,
*        writing a FrameSet to a DSS encoded FitsChan resulted in all
*        comments cards being stripped except for the last one.
*        o  Reading a FrameSet from a DSS-encoded FrameSet now only
*        removes the keywords actually required to construct the FrameSet.
*        Previously, all keywords were removed.
*        o  The EPOCH and EQUINOX keywords created when a FrameSet is
*        written to a DSS-encoded FitsChan are now determined from the 
*        epoch and equinox of the current Frame, instead of from a copy
*        of the original FitsChan stored within the DssMap.
*        o  The Encoding and CDMatrix attributes, and keyword types are 
*        now stored as strings externally instead of integers.
*     11-NOV-1997 (DSB):
*        o  Assume default of j2000 for DSS EQUINOX value.
*        o  Check for null object pointers in the interfaces for 
*        virtual functions which execute even if an error has previously
*        occurred. Otherwise, a segmentation violation can occur when 
*        trying to find the member function pointer.
*        o  Trailing spaces ignored in Encoding attribute.
*        o  Bugs fixed in FindWcs and SetValue which resulted in WCS cards
*        being written at the wrong place if the supplied FitsChan does not 
*        contain any WCS keywords.
*        o  Default for CDMatrix (if no axis rotation keywords can be found) 
*        changed to 2 (i.e. use "CDi_j" form keywords).
*        o  Write now leaves the current card unchanged if nothing is 
*        written to the FitsChan.
*     17-NOV-1997 (RFWS):
*        Disabled use of CDmatrix. Fixed initialisation problems in
*        astLoadFitsChan.
*     24-NOV-1997 (DSB):
*        Replace references to error code AST__OPT with AST__RDERR.
*     28-NOV-1997 (DSB):
*        o  Function WcsValues modified to prevent it from changing the 
*        current card. Previously, this could cause new cards to be 
*        written to the wrong place in a FITS-WCS encoded FitsChan.
*        o  Description of argument "value" corrected in prologue of
*        function FitsSet.
*        o  Argument "lastkey" removed from function SetValue since it
*        was never used (it was a relic from a previous method of
*        determining where to store new cards). Corresponding changes 
*        have been made to all the functions which create "lastkey" values 
*        or pass them on to SetValue (i.e DescWcs, WcsPrimary, WcsSecondary, 
*        WriteWcs and WriteDss).
*     10-DEC-1997 (DSB):
*        Bug fixed which caused the initial character designating the system 
*        within CTYPE value (eg E in ELON, G in GLON, etc) to be omitted.
*     1-JUN-1998 (DSB):
*        CDELT values of zero are now replaced by a small non-zero value
*        when creating the "pixel-to-relative physical" transformation
*        matrix. Previously, zero CDELT values could cause the matrix to
*        be non-invertable.
*     4-SEP-1998 (DSB):
*        - Indicate that SphMaps created by this class when using FITS-WCS
*        encoding all operate on the unit sphere. This aids simplification.
*        - Fix a bug in StoreFits which caused CD matrices to be indexed 
*        incorrectly (sometimes causing floating exceptions) if they do not
*        describe a celestial longitude/latitude system.
*        - Changed astFindFits to ignore trailing spaces in the keyword 
*        template.
*        - astSplit changed so that an error is not reported if a textual
*        keyword value ends before column 20.
*     7-OCT-1998 (DSB):
*        - Corrected test for linearity in LinearMap to include a factor
*        of the test vector length. Also LinearMap now uses a simplified 
*        Mapping.
*     5-NOV-1998 (DSB):
*        Added FITS-IRAF encoding.
*     9-NOV-1998 (DSB):
*        - Corrected values of macros DSS_ENCODING and MAX_ENCODING.
*        - Corrected erroneous success indication in IrafStore.
*        - Included checks for bad values in function LinearMap.
*     17-NOV-1998 (DSB):
*        The Domain name GRID is now given to the Base Frame in any FrameSets
*        created by astRead when using FitsChans with DSS, FITS-WCS or
*        FITS-IRAF encodings.
*     18-DEC-1998 (DSB):
*        Check for "D" exponents in floating point keyword strings.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS FitsChan

/* A macro which tests a character to see if it can be used within a FITS 
   keyword. We include lower case letters here, but they are considered
   as equivalent to upper case letter. */
#define isFits(a) ( islower(a) || isupper(a) || isdigit(a) || (a)=='-' || (a)=='_' )

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro which returns the nearest integer to a given floating point 
   value. */
#define NINT(x) (int)((x)+((x)>0.0)?0.5:-0.5)

/* Set of characters used to encode a "sequence number" at the end of
   FITS keywords in an attempt to make them unique.. */
#define SEQ_CHARS "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"

/* A general tolerance for equality between floating point values. */
#define TOL1 10.0*DBL_EPSILON

/* A tolerance for equality between angular values in radians. */
#define TOL2 1.0E-10

/* Macros to check for equality of floating point values. We cannot
compare bad values directory because of the danger of floating point
exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))


/* Constants: */
#define UNKNOWN_ENCODING  -1
#define NATIVE_ENCODING    0
#define FITSWCS_ENCODING   1
#define FITSIRAF_ENCODING  2
#define DSS_ENCODING       3
#define MAX_ENCODING       3
#define UNKNOWN_STRING     "UNKNOWN"
#define NATIVE_STRING      "NATIVE"
#define FITSWCS_STRING     "FITS-WCS"
#define FITSWCS_STRING2    "FITS_WCS"
#define FITSIRAF_STRING    "FITS-IRAF"
#define FITSIRAF_STRING2   "FITS_IRAF"
#define DSS_STRING         "DSS"
#define INDENT_INC         3
#define PREVIOUS           0
#define NEXT               1
#define HEADER_TEXT        "Beginning of AST data for "
#define FOOTER_TEXT        "End of AST data for "
#define FITSNAMLEN         8
#define FITSSTCOL          20
#define FITSRLCOL          30
#define FITSIMCOL          50
#define FITSCOMCOL         32
#define FITSCARDLEN        80
#define NORADEC            0
#define FK4                1
#define FK4NOE             2
#define FK5                3
#define GAPPT              4
#define NOCEL              0
#define RADEC              1
#define ECLIP              2
#define GALAC              3
#define HELIO              4
#define SUPER              5
#define CELSY              6
#define LONAX             -1
#define NONAX              0
#define LATAX              1
#define NDESC              9

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"    
#include "memory.h"   
#include "object.h"   
#include "channel.h"  
#include "pointset.h" 
#include "unitmap.h"  
#include "frame.h" 
#include "skyframe.h" 
#include "cmpframe.h" 
#include "frameset.h" 
#include "wcsmap.h"   
#include "dssmap.h"   
#include "winmap.h"
#include "matrixmap.h"
#include "sphmap.h"
#include "permmap.h"
#include "cmpmap.h"
#include "fitschan.h" 
#include "slalib.h" 

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Type Definitions */
/* ================ */
/* This structure contains information describing a single FITS header card
   in a circular list of such structures. */

typedef struct FitsCard {
   char name[ FITSNAMLEN + 1 ];/* Keyword name (plus terminating null). */
   int type;                  /* Data type. */
   void *data;                /* Pointer to the keyword's data value. */
   char *comment;             /* Pointer to a comment for the keyword. */
   int del;                   /* Has this card been read into an object? */
   size_t size;               /* Size of data value */
   struct FitsCard *next;     /* Pointer to next structure in list. */
   struct FitsCard *prev;     /* Pointer to previous structure in list. */
} FitsCard;   


typedef struct FitsKeySeq {   /* Associate a keyword with a sequence no. */
   char *key;                 /* Pointer to basic FITS keyword string */
   int seq;                   /* Sequence number last used */
   struct FitsKeySeq *next;   /* Pointer to next list element */
} FitsKeySeq;


/* Structure used to store information derived from the FITS WCS keyword values 
   in a form more convenient to further processing. */
typedef struct FitsStore {
   int *prj;                    /* Pointer to array of WCS projections */
   char **cdelt_name;           /* Pointers to CDELT keyword names */
   char **crota_name;           /* Pointers to CROTA keyword names */
   char **crpix_name;           /* Pointers to CRPIX keyword names */
   char **crval_name;           /* Pointers to CRVAL keyword names */
   char **ctype;                /* Pointer to array of CTYPE values */
   char **ctype_name;           /* Pointers to CTYPE keyword names */
   char **ctype_com;            /* Pointers to CTYPE keyword comments */
   char **cunit;                /* Pointer to array of CUNIT values */
   double *cdelt;               /* Pointer to array of CDELT values */
   double *crota;               /* Pointer to array of CROTA values */
   double *crpix;               /* Pointer to array of CRPIX values */
   double *crval;               /* Pointer to array of CRVAL values */
   double *pc;                  /* Pointer to the PC matrix. */
   double equinox;              /* Epoch of reference equinox */
   double latpole;              /* LATPOLE value in radians */
   double longpole;             /* LONGPOLE value in radians */
   double mjdobs;               /* Modified Julian Date of observation */
   double projp[ AST__WCSMX ];  /* Projection parameters */
   int *ialt;                   /* Pointer to array of axis description counts */
   int axlat;                   /* Index of latitude axis */
   int axlon;                   /* Index of longitude axis */
   int julian;                  /* Is equinox value Julian? */
   int noncel;                  /* No. of non-celestial axes */
   int npar;                    /* No. of supplied projection parameters */
   int naxis;                   /* No. of axes */
   int radecsys;                /* Flag identifying frame of reference */
   int sys;                     /* Flag identifying celestial coord. sys.*/
} FitsStore;



/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstFitsChanVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Strings to decribe each data type. These should be in the order implied
   by the corresponding macros (eg AST__FLOAT, etc). */
static const char *type_names[] = {"comment", "integer", "floating point",
                                   "string", "complex floating point",
                                   "complex integer", "logical" };

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_getfull)( AstChannel * );
static int (* parent_getskip)( AstChannel * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );
static int (* parent_write)( AstChannel *, AstObject * );
static AstObject *(* parent_read)( AstChannel * );

/* A flag indicating if the private functions which navigate through the
  circular linked list of FitsCards structures should skip over cards
  which have been read into an AST object. */
static int Skipping;

/* Number of output items written since the last "Begin" or "IsA"
   output item, and level of Object nesting during recursive
   invocation of the astWrite method. */
static int items_written = 0;
static int write_nest = -1;

/* Indentation level for indented comments when writing Objects to a
   FitsChan. */
static int current_indent;

/* Text values used to represent Encoding values externally. */
static const char *xencod[4] = { NATIVE_STRING, FITSWCS_STRING,
                                 DSS_STRING, FITSIRAF_STRING };

#if 0
/* Text values used to represent CDMatrix values externally. */
static const char *xcdmat[3] = { "PCiiijjj", "CDiiijjj", "CDi_j" };
#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstFitsChan *astFitsChanForId_( const char *(*)( void ), 
                           char *(*)( const char *(*)( void ) ), 
                           void (*)( const char * ), 
                           void (*)( void (*)( const char * ), const char * ),
                           const char *, ... );
AstFitsChan *astFitsChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static void ClearCard( AstFitsChan * );
static int GetCard( AstFitsChan * );
static int TestCard( AstFitsChan * );
static void SetCard( AstFitsChan *, int );

static void ClearEncoding( AstFitsChan * );
static int GetEncoding( AstFitsChan * );
static int TestEncoding( AstFitsChan * );
static void SetEncoding( AstFitsChan *, int );

static void ClearFitsDigits( AstFitsChan * );
static int GetFitsDigits( AstFitsChan * );
static int TestFitsDigits( AstFitsChan * );
static void SetFitsDigits( AstFitsChan *, int );

static void ClearCDMatrix( AstFitsChan * );
static int GetCDMatrix( AstFitsChan * );
static int TestCDMatrix( AstFitsChan * );
static void SetCDMatrix( AstFitsChan *, int );

static int GetNcard( AstFitsChan * );

static AstCmpMap *WcsNative( FitsStore *, AstWcsMap *, const char *, const char * );
static AstFrame *WcsFrame( FitsStore *, const char *, const char * );
static AstFrameSet *ReadDSS( AstFitsChan * );
static AstFrameSet *ReadWcs( AstFitsChan * );
static AstMapping *WcsMapping( FitsStore *, const char *, const char * );
static AstMatrixMap *WcsMatrix( FitsStore *, const char *, const char * );
static AstObject *Read( AstChannel * );
static AstSkyFrame *DSSFrame( AstFitsChan *, const char *, const char * );
static AstWcsMap *WcsDeproj( FitsStore *, const char *, const char * );
static AstWinMap *WcsAddRef( FitsStore *, int * );
static AstWinMap *WcsShift( FitsStore * );
static FitsCard *GetLink( FitsCard *, int, const char *, const char * );
static char *CardComm( AstFitsChan * );
static char *CardName( AstFitsChan * );
static char *SourceWrap( const char *(*)( void ) );
static char *UnPreQuote( const char * );
static const char *FindIraf( AstFitsChan * );
static const char *FindWcs( AstFitsChan * );
static const char *GetAttrib( AstObject *, const char * );
static double DateObs( const char * );
static int CardDel( AstFitsChan * );
static int CardType( AstFitsChan * );
static int CheckFitsName( const char *, const char *, const char * );
static int ChrLen( const char * );
static int ComBlock( AstFitsChan *, int, const char *, const char * );
static int CountFields( const char *, char, const char *, const char * );
static int DescIraf( AstFitsChan *, AstFrameSet *, int, int, int, FitsStore * );
static int DescWcs( AstFitsChan *, AstFrameSet *, int, int, int, int, FitsStore * );
static int EncodeFloat( char *, int, int, int, double );
static int EncodeValue( AstFitsChan *, char *, int, int, const char * );
static int KeyFields( AstFitsChan *, const char *, int, int *, int * );
static int FindKeyCard( AstFitsChan *, const char *, const char *, const char * );
static int FitsEof( AstFitsChan * );
static int FitsGetCF( AstFitsChan *, const char *, double * );
static int FitsGetCI( AstFitsChan *, const char *, int * );
static int FitsGetCom( AstFitsChan *, const char *, char ** );
static int FitsGetF( AstFitsChan *, const char *, double * );
static int FitsGetI( AstFitsChan *, const char *, int * );
static int FitsGetL( AstFitsChan *, const char *, int * );
static int FitsGetS( AstFitsChan *, const char *, char ** );
static int FitsSet( AstFitsChan *, const char *, void *, int, const char *, int );
static int FullForm( const char *, const char * );
static int GetDesc( AstFitsChan *, int, int *, int *, int * );
static int GetFull( AstChannel * );
static int GetNaxis( AstFitsChan *, int );
static int GetSkip( AstChannel * );
static int GetWcsValue( AstFitsChan *, char *, int, void *, int, const char *, const char *  );
static int IrafStore( AstFitsChan *, FitsStore * );
static int LinearMap( AstMapping *, int, int, double *, double *, double );
static int Match( const char *, const char *, int, int *, int *, const char *, const char * );
static int MatchChar( char, char, const char *, const char *, const char * );
static int MatchFront( const char *, const char *, char *, int *, int *, int *, const char *, const char *, const char * );
static int MoveCard( AstFitsChan *, int, const char *, const char * );
static int SearchCard( AstFitsChan *, const char *, const char *, const char *);
static int SkySys( int, AstFrame *, int, FitsStore *, int, int );
static int SplitMap( int, AstMapping *, AstMapping **, AstMapping **, AstMapping **, const char * );
static int SplitMat( int, double *, double * );
static int StoreFits( AstFitsChan *, int, int, int *, FitsStore *, const char *, const char *  );
static int TestAttrib( AstObject *, const char * );
static int Use( AstFitsChan *, int, int );
static int Ustrcmp( const char *, const char * );
static int Ustrncmp( const char *, const char *, size_t );
static int WcsNatPole( AstWcsMap *, double, double, double, double *, double *, double * );
static int WcsNoWcs( AstFitsChan *, AstMapping *, AstFrame *, int, int, double *, FitsStore * );
static int WcsPrimary( AstFitsChan *, FitsStore *, int );
static int WcsSecondary( AstFitsChan *, FitsStore * );
static int WcsValues( AstFitsChan *, AstFrameSet *, int, int, int, int, FitsStore * );
static int WcsWithWcs( AstFitsChan *, AstMapping *, AstMapping *, AstMapping *, AstFrame *, int, int, FitsStore * );
static int Write( AstChannel *, AstObject * );
static int WriteDSS( AstChannel *, AstObject * );
static int WriteIraf( AstChannel *, AstObject * );
static int WriteWcs( AstChannel *, AstObject * );
static int astSplit_( const char *, char **, char **, char **, const char *, const char * );
static void *CardData( AstFitsChan *, size_t * );
static void CheckZero( char *, double );
static void CleanFits( FitsStore * );
static void ClearAttrib( AstObject *, const char * );
static int CnvValue( AstFitsChan *, int , void *, const char *);
static int CnvType( int, void *, int, int, void *, const char *, const char *, const char * );
static void Copy( const AstObject *, AstObject * );
static void CreateKeyword( AstFitsChan *, const char *, char [ FITSNAMLEN + 1 ] );
static void Crota( int, FitsStore *, int, int, int );
static void DelFits( AstFitsChan * );
static void Delete( AstObject * );
static void DeleteCard( AstFitsChan *, const char *, const char * );
static void Dump( AstObject *, AstChannel * );
static void Empty( AstFitsChan * );
static void FitsSetCF( AstFitsChan *, const char *, double *, const char *, int );
static void FitsSetCI( AstFitsChan *, const char *, int *, const char *, int );
static void FitsSetCom( AstFitsChan *, const char *, const char *, int );
static void FitsSetF( AstFitsChan *, const char *, double, const char *, int );
static void FitsSetI( AstFitsChan *, const char *, int, const char *, int );
static void FitsSetL( AstFitsChan *, const char *, int, const char *, int );
static void FitsSetS( AstFitsChan *, const char *, const char *, const char *, int );
static void FormatCard( AstFitsChan *, char *, const char * );
static void GetNextData( AstChannel *, int, char **, char ** );
static void InitVtab( AstFitsChanVtab * );
static void InsCard( AstFitsChan *, int, const char *, int, void *, const char *, const char *, const char * );
static void MakeBanner( const char *, const char *, const char *, char [ FITSCARDLEN - FITSNAMLEN + 1 ] );
static void MakeIndentedComment( int, char, const char *, const char *, char [ FITSCARDLEN - FITSNAMLEN + 1] );
static void MakeIntoComment( AstFitsChan *, const char *, const char * );
static void MarkCard( AstFitsChan * );
static void NewCard( AstFitsChan *, const char *, int, const void *, const char *, int );
static void PreQuote( const char *, char [ FITSCARDLEN - FITSNAMLEN - 3 ] );
static void PutFits( AstFitsChan *, const char [ FITSCARDLEN + 1 ], int );
static void ReadFromSource( AstFitsChan * );
static void SetAttrib( AstObject *, const char * );
static void SetValue( AstFitsChan *, char *, void *, int, char * );
static void SinkWrap( void (*)( const char * ), const char * );
static void WriteBegin( AstChannel *, const char *, const char * );
static void WriteDouble( AstChannel *, const char *, int, int, double, const char * );
static void WriteEnd( AstChannel *, const char * );
static void WriteInt( AstChannel *, const char *, int, int, int, const char * );
static void WriteIsA( AstChannel *, const char *, const char * );
static void WriteObject( AstChannel *, const char *, int, int, AstObject *, const char * );
static void WriteString( AstChannel *, const char *, int, int, const char *, const char * );
static void WriteToSink( AstFitsChan * );
static int FindString( int, const char *[], const char *, const char *, const char *, const char * );

/* Member functions. */
/* ================= */

static char *CardComm( AstFitsChan *this ){
/*
*  Name:
*     CardComm

*  Purpose:
*     Return the keyword comment from the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     char *CardComm( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns a pointer to a string holding the keyword comment from the
*     current card.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to the keyword comment, or NULL if the FitsChan is at
*     end-of-file, or does not have a comment.

*  Notes:
*     -  The current card is not changed by this function.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   char *ret;

/* Check the supplied object. */
   if( !this ) return NULL;

/* If the current card is defined, store a pointer to its keyword comment. */
   if( this->card ){
      ret = ( (FitsCard *) this->card )->comment;

/* Otherwise store a NULL pointer. */
   } else {
      ret =  NULL;
   }

/* Return the answer. */
   return ret;

}

static void *CardData( AstFitsChan *this, size_t *size ){
/*
*  Name:
*     CardData

*  Purpose:
*     Return a pointer to the keyword data value for the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void *CardData( AstFitsChan *this, size_t *size )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns a pointer to keyword data value from the current card.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     size
*        A pointer to a location at which to return the number of bytes
*        occupied by the data value.

*  Returned Value:
*     A pointer to the keyword data, or NULL if the FitsChan is at
*     end-of-file, or if the keyword does not have any data.

*  Notes:
*     -  For text data, the returned value for "size" includes the
*     terminating null character.
*     -  The current card is not changed by this function.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   void *ret;

/* Check the supplied object. */
   if( !this ) return NULL;

/* If the current card is defined, store a pointer to its keyword data. */
   if( this->card ){
      ret = ( (FitsCard *) this->card )->data;
      if( size ) *size = ( (FitsCard *) this->card )->size;

/* Otherwise store a NULL pointer. */
   } else {
      ret =  NULL;
      if( size ) *size = 0;
   }

/* Return the answer. */
   return ret;

}

static int CardDel( AstFitsChan *this ){
/*
*  Name:
*     CardDel

*  Purpose:
*     Return the deletion flag for the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CardDel( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns the deletion flag from the current card. This indicates if
*     the card is marker for deletion.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     Zero if the card is not marker for deletion, 1 if it is.

*  Notes:
*     -  The current card is not changed by this function.
*     -  Zero is returned if the current card is not defined.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   int ret;

/* Check the supplied object. */
   if( !this ) return 0;

/* If the current card is defined, store its deletion flag. */
   if( this->card ){
      ret = ( (FitsCard *) this->card )->del ? 1 : 0;

/* Otherwise store zero. */
   } else {
      ret =  0;
   }

/* Return the answer. */
   return ret;

}

static char *CardName( AstFitsChan *this ){
/*
*  Name:
*     CardName

*  Purpose:
*     Return the keyword name from the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     char *CardName( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns a pointer to a string holding the keyword name from the
*     current card.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to the keyword name, or NULL if the FitsChan is at
*     end-of-file.

*  Notes:
*     -  The current card is not changed by this function.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   char *ret;

/* Check the supplied object. */
   if( !this ) return NULL;

/* If the current card is defined, store a pointer to its keyword name. */
   if( this->card ){
      ret = ( (FitsCard *) this->card )->name;

/* Otherwise store a NULL pointer. */
   } else {
      ret =  NULL;
   }

/* Return the answer. */
   return ret;

}

static int CardType( AstFitsChan *this ){
/*
*  Name:
*     CardType

*  Purpose:
*     Return the keyword type from the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CardType( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns the keyword type from the current card.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The keyword type.

*  Notes:
*     -  The current card is not changed by this function.
*     -  AST__NOTYPE is returned if the current card is not defined.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   int ret;

/* Check the supplied object. */
   if( !this ) return AST__NOTYPE;

/* If the current card is defined, store the keyword type. */
   if( this->card ){
      ret = ( (FitsCard *) this->card )->type;

/* Otherwise store AST__NOTYPE. */
   } else {
      ret =  AST__NOTYPE;
   }

/* Return the answer. */
   return ret;

}

static int CheckFitsName( const char *name, const char *method, 
                          const char *class ){
/*
*  Name:
*     CheckFitsName

*  Purpose:
*     Check a keyword name conforms to FITS standards.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CheckFitsName( const char *name, const char *method, 
*                        const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     FITS keywords must contain between 1 and 8 characters, and each
*     character must be an upper-case Latin alphabetic character, a digit,
*     an underscore, or a hyphen. Leading, trailing or embedded white space
*     is not allowed, with the exception of totally blank or null keyword 
*     names.

*  Parameters:
*     name
*        Pointer to a string holding the name to check. 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 0 is returned if the supplied name was blank. A value of 1
*     is returned otherwise.

*  Notes:
*     -  An error is reported if the supplied keyword name does not
*     conform to FITS requirements, and zero is returned.

*/

/* Local Variables: */
   const char *c;     /* Pointer to next character in name */
   size_t n;          /* No. of characters in supplied name */
   int ret;           /* Returned value */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned value to indicate that the supplied name was
   blank. */
   ret = 0;

/* Check that the supplied pointer is not NULL. */
   if( name ){

/* Get the number of characters in the name. */
      n = strlen( name );

/* Report an error if the name has too many characters in it. */
      if( n > FITSNAMLEN ){
         astError( AST__BDFTS, "%s(%s): The supplied FITS keyword name ('%s') "
                   "has %d characters. FITS only allows up to %d.", method, 
                   class, name, n, FITSNAMLEN );                

/* If the name has no characters in it, then assume it is a legal blank
   keyword name. Otherwise, check that no illegal characters occur in the 
   name. */
      } else if( n != 0 ) {

/* Whitespace is only allowed in the special case of a name consisting
   entirely of whitespace. Such keywords are used to indicate that the rest 
   of the card is a comment. Find the first non-whitespace character in the 
   name. */
         c = name;
         while( isspace( ( int ) *(c++) ) );

/* If the name is filled entirely with whitespace, then the name is acceptable 
   as the special case. Otherwise, we need to do more checks. */
         if( c - name - 1 < n ){

/* Indicate that the supplied name is not blank. */
            ret = 1;

/* Loop round every character checking that it is one of the legal characters. 
   Report an error if any illegal characters are found. */
            c = name;
            while( *c ){

               if( !isFits( (int) *c ) ){

                  if( *c == '=' ){
                     astError( AST__BDFTS, "%s(%s): An equals sign ('=') was found "
                               "before column %d within a FITS keyword name or header "
                               "card.", method, class, FITSNAMLEN + 1 );                
                  } else {   
                     astError( AST__BDFTS, "%s(%s): The supplied FITS keyword "
                               "name ('%s') contains an illegal character ('%c').",
                               method, class, name, *c );                
                  }
                  break;
               }
               c++;
            }         
         }
   
      }
      
/* Report an error if no pointer was supplied. */
   } else {
      astError( AST__BDFTS, "%s(%s): A NULL pointer was supplied for "    
                "the keyword name. ", method, class );                
   }

/* If an error has occurred, return 0. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static void CheckZero( char *text, double value ){
/*
*  Name:
*     CheckZero

*  Purpose:
*     Ensure that the formatted value zero has no minus sign.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void CheckZero( char *text, double value )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     There is sometimes a problem (perhaps only on DEC UNIX) when formatting 
*     the floating-point value 0.0 using C. Sometimes it gives the string 
*     "-0". This function fixed this by checking the first character of
*     the supplied string (if the supplied value is zero), and shunting the
*     remaining text one character to the right if it is a minus sign. It
*     returns without action if the supplied value is not zero.

*  Parameters:
*     text
*        The formatted value.
*     value
*        The floating value which was formatted.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   char *c;

/* If the floating point value is not zero, or if no text was supplied,
   return. */
   if( value != 0.0 || !text ) return;

/* Find the first non-space character. */
   c = text;
   while( *c && isspace( (int) *c ) ) c++;

/* If the first non-space character is a minus sign, shunt the remaining
   text to the left by one character to overwrite it. */
   if( *c == '-' ) {
      while( *c ) {
         *c = *( c + 1 );
         c++;
      }
   }
}

static int ChrLen( const char *string ){
/*
*  Name:
*     ChrLen

*  Purpose:
*     Return the length of a string excluding any trailing white space.

*  Type:
*     Private function.

*  Synopsis:
*     int ChrLen( const char *string )

*  Class Membership:
*     FitsChan

*  Description:
*     This function returns the length of a string excluding any trailing
*     white space, or non-printable characters.

*  Parameters:
*     string
*        Pointer to the string.

*  Returned Value:
*     The length of a string excluding any trailing white space and
*     non-printable characters.

*  Notes:
*     -  A value of zero is returned if a NULL pointer is supplied, or if an
*     error has already occurred.

*/

/* Local Variables: */
   const char *c;      /* Pointer to the next character to check */
   int ret;            /* The returned string length */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned string length. */
   ret = 0;

/* Check a string has been supplied. */
   if( string ){

/* Check each character in turn, starting with the last one. */
      ret = strlen( string );
      c = string + ret - 1;
      while( ret ){
         if( isprint( (int) *c ) && !isspace( (int) *c ) ) break;
         c--;
         ret--;
      }
   }

/* Return the answer. */
   return ret;

}

static void CleanFits( FitsStore *store ){
/*
*  Name:
*     CleanFits

*  Purpose:
*     Free dynamic arrays stored in a FitsStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     void CleanFits( FitsStore *store )

*  Class Membership:
*     FitsChan

*  Description:
*     This function frees all dynamically allocated arrays stored in the
*     supplied FitsStore structure. See functions StoreFits, and ReadWcs.

*  Parameters:
*     store
*        Pointer to the structure to clean.

*  Notes:
*     - This function attempts to execute even if an error exists on entry.

*/

/* Local Variables: */
   int i;                    /* Axis count */

/* Free the memory used to hold individual strings. */
   for( i = 0; i < store->naxis; i++ ) {
      if( store->ctype ) (void) astFree( (void *) store->ctype[ i ] );
      if( store->cunit ) (void) astFree( (void *) store->cunit[ i ] );
      if( store->ctype_name ) (void) astFree( (void *) store->ctype_name[ i ] );
      if( store->ctype_com ) (void) astFree( (void *) store->ctype_com[ i ] );
      if( store->crval_name ) (void) astFree( (void *) store->crval_name[ i ] );
      if( store->cdelt_name ) (void) astFree( (void *) store->cdelt_name[ i ] );
      if( store->crota_name ) (void) astFree( (void *) store->crota_name[ i ] );
      if( store->crpix_name ) (void) astFree( (void *) store->crpix_name[ i ] );
   }

/* Now free the arrays. */         
   store->pc = (double *) astFree( (void *) store->pc );
   store->crval = (double *) astFree( (void *) store->crval );
   store->cdelt = (double *) astFree( (void *) store->cdelt );
   store->crota = (double *) astFree( (void *) store->crota );
   store->crpix = (double *) astFree( (void *) store->crpix );
   store->ctype = (char **) astFree( (void *) store->ctype );
   store->cunit = (char **) astFree( (void *) store->cunit );
   store->crval_name = (char **) astFree( (void *) store->crval_name );
   store->cdelt_name = (char **) astFree( (void *) store->cdelt_name );
   store->crota_name = (char **) astFree( (void *) store->crota_name );
   store->crpix_name = (char **) astFree( (void *) store->crpix_name );
   store->ctype_name = (char **) astFree( (void *) store->ctype_name );
   store->ctype_com = (char **) astFree( (void *) store->ctype_com );
   store->prj = (int *) astFree( (void *) store->prj );
   store->ialt = (int *) astFree( (void *) store->ialt );

   return;
}

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FitsChan member function (over-rides the astClearAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function clears the value of a specified attribute for a
*     FitsChan, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* Card. */
/* ----- */
   if ( !strcmp( attrib, "card" ) ) {
      astClearCard( this );

/* Encoding. */
/* --------- */
   } else if ( !strcmp( attrib, "encoding" ) ) {
      astClearEncoding( this );

/* FitsDigits. */
/* ----------- */
   } else if ( !strcmp( attrib, "fitsdigits" ) ) {
      astClearFitsDigits( this );

#if 0
/* CDMatrix. */
/* ----------- */
   } else if ( !strcmp( attrib, "cdmatrix" ) ) {
      astClearCDMatrix( this );
#endif

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( !strcmp( attrib, "ncard" ) ){
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static void ClearCard( AstFitsChan *this ){
/*
*+
*  Name:
*     astClearCard

*  Purpose:
*     Clear the Card attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     void astClearCard( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function clears the Card attribute for the supplied FitsChan by 
*     setting it to 1. This causes the next read operation performed on the 
*     FitsChan to read the first card in the FistChan. Thus, it is
*     equivalent to "rewinding" the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.
*-
*/

/* Check the supplied FitsChan. If its is empty, return. */
   if ( !this || !(this->head) ) return;

/* Set the pointer to the current card so that it points to the card at
   the head of the list. */
   this->card = this->head;

/* If the current card has been read into an AST object, move on to the 
   first card which has not, unless we are not skipping such cards. */
   if( Skipping && ( (FitsCard *) this->card )->del ){
      MoveCard( this, 1, "astClearCard", astGetClass( this ) );
   }

}

static int CnvValue( AstFitsChan *this, int type, void *buff, 
                      const char *method ){
/*
*
*  Name:
*     CnvValue

*  Purpose:
*     Convert a data value into a given FITS data type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CnvValue( AstFitsChan *this, int type, void *buff, 
*                   const char *method )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function produces a copy of the data value for the current card 
*     converted from its stored data type to the supplied data type.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     type
*        The FITS data type in which to return the data value of the
*        current card.
*     buf
*        A pointer to a buffer to recieve the converted value. It is the
*        responsibility of the caller to ensure that a suitable buffer is
*        supplied.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.

*  Returned Value:
*     Zero if the conversion was not possible (in which case NO error is
*     reported), one otherwise.

*  Notes: 
*     -  When converting from floating point to integer, the  floating
*     point value is truncated using a C cast.
*     -  Non-zero numerical values are considered TRUE, and zero
*     numerical values are considered FALSE. Any string starting with a
*     'T' or a 'Y' (upper or lower case) is considered TRUE, and anything 
*     starting with an 'F' or an 'N' (upper or lower case) is considered
*     FALSE. In addition, a dot ('.') may be placed in front of a 'T' or an
*     'F'.
*     -  A logical TRUE value is represented as a real numerical value of
*     one and the character string "Y". A logical FALSE value is represented 
*     by a real numerical value of zero and the character string "N".
*     -  When converting from a string to any numerical value, zero is
*     returned if the string is not a formatted value which can be converted 
*     into the corresponding type using sscanf.
*     - Real and imaginary parts of a complex value should be separated by 
*     spaces within strings. If a string does contains only a single numerical 
*     value, it is assumed to be the real part, and the imaginary part is 
*     assumed to be zero.
*     -  When converting a complex numerical type to a non-complex numerical 
*     type, the returned value is derived from the real part only, the 
*     imaginary part is ignored.
*     -  Zero is returned if an error has occurred, or if this function
*     should fail for any reason.

*/

/* Local Variables: */
   int otype;               /* Stored data type */
   size_t osize;            /* Size of stored data */
   void *odata;             /* Pointer to stored data */

/* Check the global error status, and the supplied buffer. */
   if ( !astOK || !buff ) return 0; 

/* Get the type in which the data value is stored. */
   otype = CardType( this );

/* Get a pointer to the stored data value, and its size. */
   odata = CardData( this, &osize );

/* Do the conversion. */
   return CnvType( otype, odata, osize, type, buff, CardName( this ), 
                   method, astGetClass( this ) );
}

static int CnvType( int otype, void *odata, int osize, int type, 
                     void *buff, const char *name, const char *method, 
                     const char *class ){
/*
*
*  Name:
*     CnvType

*  Purpose:
*     Convert a data value into a given FITS data type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CnvType( int otype, void *odata, int osize, int type, 
*                   void *buff, const char *name, const char *method, 
*                   const char *class )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function produces a copy of the data value for the current card 
*     converted from its stored data type to the supplied data type.

*  Parameters:
*     otype
*        The type of the supplied data value.
*     odata
*        Pointer to a buffer holding the supplied data value.
*     osize
*        The size of the data value (in bytes - strings include the
*        terminating null).
*     type
*        The FITS data type in which to return the data value of the
*        current card.
*     buf
*        A pointer to a buffer to recieve the converted value. It is the
*        responsibility of the caller to ensure that a suitable buffer is
*        supplied.
*     name
*        A pointer to a string holding a keyword name to include in error
*        messages.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class
*        Pointer to a string holding the name of the object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     Zero if the conversion was not possible (in which case NO error is
*     reported), one otherwise.

*  Notes: 
*     -  When converting from floating point to integer, the  floating
*     point value is truncated using a C cast.
*     -  Non-zero numerical values are considered TRUE, and zero
*     numerical values are considered FALSE. Any string starting with a
*     'T' or a 'Y' (upper or lower case) is considered TRUE, and anything 
*     starting with an 'F' or an 'N' (upper or lower case) is considered
*     FALSE. In addition, a dot ('.') may be placed in front of a 'T' or an
*     'F'.
*     -  A logical TRUE value is represented as a real numerical value of
*     one and the character string "Y". A logical FALSE value is represented 
*     by a real numerical value of zero and the character string "N".
*     -  When converting from a string to any numerical value, zero is
*     returned if the string isn not a formatted value which can be converted 
*     into the corresponding type using sscanf.
*     - Real and imaginary parts of a complex value should be separated by 
*     spaces within strings. If a string does contains only a single numerical 
*     value, it is assumed to be the real part, and the imaginary part is 
*     assumed to be zero.
*     -  When converting a complex numerical type to a non-complex numerical 
*     type, the returned value is derived from the real part only, the 
*     imaginary part is ignored.
*     -  Zero is returned if an error has occurred, or if this function
*     should fail for any reason.

*/

/* Local Variables: */
   const char *c;           /* Pointer to next character */
   const char *ostring;     /* String data value */
   double odouble;          /* Double data value */
   int oint;                /* Integer data value */
   int ival;                /* Integer value read from string */
   int len;                 /* Length of character string */
   int nc;                  /* No. of characetsr used */
   int ret;                 /* Returned success flag */
   static char text[ FITSCARDLEN + 1 ]; /* Buffer for returned text string */
   static char text0[ FITSCARDLEN + 1 ]; /* Buffer for real value */
   static char text1[ FITSCARDLEN + 1 ]; /* Buffer for imaginary value */

/* Check the global error status, and the supplied buffer. */
   if ( !astOK || !buff ) return 0; 

/* Assume success. */
   ret = 1;

/* If there is no data value, or if this is a COMMENT keyword, leave the 
   supplied buffer unchanged. */
   if( odata && type != AST__COMMENT ){

/* Do each possible combination of supplied and stored data types... */

/* Convert a AST__FLOAT data value to ... */
      if( otype == AST__FLOAT ){
         odouble = *( (double *) odata );

         if( type == AST__FLOAT ){
            (void) memcpy( buff, odata, osize );
   
         } else if( type == AST__STRING   ){
            (void) sprintf( text, "%.*g", DBL_DIG, odouble );
            CheckZero( text, odouble );
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            *( (int *) buff ) = (int) odouble;

         } else if( type == AST__LOGICAL  ){
            *( (int *) buff ) = ( odouble == 0.0 ) ? 0 : 1;

         } else if( type == AST__COMPLEXF ){
            ( (double *) buff )[ 0 ] = odouble;
            ( (double *) buff )[ 1 ] = 0.0;

         } else if( type == AST__COMPLEXI ){
            ( (int *) buff )[ 0 ] = (int) odouble;
            ( (int *) buff )[ 1 ] = 0;

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__STRING data value to ... */
      } else if( otype == AST__STRING   ){
         ostring = (char *) odata;
         len = (int) strlen( ostring );

         if( type == AST__FLOAT ){
            if( nc = 0, 
                     ( 1 != sscanf( ostring, "%lf %n", (double *) buff, &nc ) )
                  || (nc < len ) ){
               ret = 0;
            }

         } else if( type == AST__STRING   ){
            strncpy( text, (char *) odata, FITSCARDLEN );
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            if( nc = 0, 
                     ( 1 != sscanf( ostring, "%d %n", (int *) buff, &nc ) )
                  || (nc < len ) ){
               ret = 0;
            }

         } else if( type == AST__LOGICAL  ){
            if( nc = 0, 
                     ( 1 == sscanf( ostring, "%d %n", &ival, &nc ) )
                  && (nc >= len ) ){
               *( (int *) buff ) = ival ? 1 : 0;               

            } else {
               c = ostring;
               while( *c && isspace( (int) *c ) ) c++;

               if( *c == 'y' || *c == 'Y' || *c == 't' || *c == 'T' ||
                   ( *c == '.' && ( c[1] == 't' || c[1] == 'T' ) ) ){
                  *( (int *) buff ) = 1;

               } else if( *c == 'n' || *c == 'N' || *c == 'f' || *c == 'F' ||
                   ( *c == '.' && ( c[1] == 'f' || c[1] == 'F' ) ) ){
                  *( (int *) buff ) = 0;
               } else {
                  ret = 0;
               }
            } 

         } else if( type == AST__COMPLEXF ){
            if( nc = 0, 
                     ( 1 != sscanf( ostring, "%lf %lf %n", (double *) buff, 
                                    (double *) buff + 1, &nc ) )
                  || (nc < len ) ){

               if( nc = 0, 
                        ( 1 != sscanf( ostring, "%lf %n", (double *) buff, 
                                       &nc ) )
                     || (nc < len ) ){
                  ret = 0;
               } else {
                  ( (double *) buff )[ 1 ] = 0.0;
               }

            }

         } else if( type == AST__COMPLEXI ){
            if( nc = 0, 
                    ( 1 != sscanf( ostring, "%d %d %n", (int *) buff, 
                                   (int *) buff + 1, &nc ) )
                   || (nc < len ) ){

               if( nc = 0, 
                        ( 1 != sscanf( ostring, "%d %n", (int *) buff, &nc ) )
                     || (nc < len ) ){

                  ret = 0;
               } else {
                  ( (int *) buff )[ 1 ] = 0;
               }

            }

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert an AST__INT data value to ... */
      } else if( otype == AST__INT      ){
         oint = *( (int *) odata );

         if( type == AST__FLOAT ){
            *( (double *) buff ) = (double) oint;

         } else if( type == AST__STRING   ){
            (void) sprintf( text, "%d", oint );
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            (void) memcpy( buff, odata, osize );

         } else if( type == AST__LOGICAL  ){
            *( (int *) buff ) = oint ? 1 : 0;

         } else if( type == AST__COMPLEXF ){
            ( (double *) buff )[ 0 ] = (double) oint;
            ( (double *) buff )[ 1 ] = 0.0;

         } else if( type == AST__COMPLEXI ){
            ( (int *) buff )[ 0 ] = oint;
            ( (int *) buff )[ 1 ] = 0;

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a LOGICAL data value to ... */
      } else if( otype == AST__LOGICAL  ){
         oint = *( (int *) odata );
         
         if( type == AST__FLOAT ){
            *( (double *) buff ) = oint ? 1.0 : 0.0;

         } else if( type == AST__STRING   ){
            if( oint ){
               strcpy( text, "Y" );
            } else {
               strcpy( text, "N" );
            }
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            *( (int *) buff ) = oint;

         } else if( type == AST__LOGICAL  ){
            (void) memcpy( buff, odata, osize );

         } else if( type == AST__COMPLEXF ){
            ( (double *) buff )[ 0 ] = oint ? 1.0 : 0.0;
            ( (double *) buff )[ 1 ] = 0.0;

         } else if( type == AST__COMPLEXI ){
            ( (int *) buff )[ 0 ] = oint ? 1 : 0;
            ( (int *) buff )[ 1 ] = 0;

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__COMPLEXF data value to ... */
      } else if( otype == AST__COMPLEXF ){
         odouble = ( (double *) odata )[ 0 ];

         if( type == AST__FLOAT ){
            *( (double *) buff ) = odouble;
   
         } else if( type == AST__STRING   ){
            (void) sprintf( text0, "%.*g", DBL_DIG, ( (double *) odata )[ 0 ] );
            CheckZero( text0, ( (double *) odata )[ 0 ] );
            (void) sprintf( text1, "%.*g", DBL_DIG, ( (double *) odata )[ 1 ] );
            CheckZero( text1, ( (double *) odata )[ 1 ] );
            (void) sprintf( text, "%s %s", text0, text1 );
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            *( (int *) buff ) = (int) odouble;

         } else if( type == AST__LOGICAL  ){
            *( (int *) buff ) = ( odouble == 0.0 ) ? 0 : 1;

         } else if( type == AST__COMPLEXF ){
            (void) memcpy( buff, odata, osize );

         } else if( type == AST__COMPLEXI ){
            ( (int *) buff )[ 0 ] = (int) odouble;
            ( (int *) buff )[ 1 ] = (int) ( (double *) odata )[ 1 ];

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__COMPLEXI data value to ... */
      } else if( otype == AST__COMPLEXI ){
         oint = ( (int *) odata )[ 0 ];

         if( type == AST__FLOAT ){
            *( (double *) buff ) = (double) oint;

         } else if( type == AST__STRING   ){
            (void) sprintf( text, "%d %d", ( (int *) odata )[ 0 ],
                                           ( (int *) odata )[ 1 ] );
            *( (char **) buff ) = text;

         } else if( type == AST__INT      ){
            *( (int *) buff ) = oint;

         } else if( type == AST__LOGICAL  ){
            *( (int *) buff ) = oint ? 1 : 0;

         } else if( type == AST__COMPLEXF ){
            ( (double *) buff )[ 0 ] = (double) oint;
            ( (double *) buff )[ 1 ] = (double) ( (int *) odata )[ 1 ];

         } else if( type == AST__COMPLEXI ){
            (void) memcpy( buff, odata, osize );

         } else {
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

      } else {
         ret = 0;
         astError( AST__INTER, "CnvType: AST internal programming error - "
                   "FITS data-type no. %d not yet supported.", type );
      }

   }

   return ret;

}

static int ComBlock( AstFitsChan *this, int incr, const char *method,
                     const char *class ){
/*
*  Name:
*     ComBlock

*  Purpose:
*     Delete a AST comment block in a Native-encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int ComBlock( AstFitsChan *this, int incr, const char *method,
*                   const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function looks for a block of comment cards as defined below,
*     and deletes all the cards in the block, if a suitable block is found.
*
*     Comment blocks consist of a contiguous sequence of COMMENT cards. The 
*     text of each card should start and end with the 3 characters "AST".
*     The block is delimited above by a card containing all +'s (except
*     for the two "AST" strings), and below by a card containing all -'s.
*     
*     The block is assumed to start on the card which is adjacent to the
*     current card on entry.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     incr
*        This should be either +1 or -1, and is the increment between
*        adjacent cards in the comment block. A value of +1 means
*        that the card following the current card is taken as the first in 
*        the block, and subsequent cards are checked. The block must then
*        end with a line of -'s. If -1 is supplied, then the card
*        preceding the current card is taken as the first in the block,
*        and preceding cards are checked. The block must then end with
*        a row of +'s.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     1 if a block was found and deleted, 0 otherwise.

*  Notes:
*     -  The pointer to the current card is returned unchanged.
*/

/* Local Variables: */
   FitsCard *card0;              /* Pointer to current FitsCard on entry */
   char *text;                   /* Pointer to the comment text */
   char del;                     /* The character making up the delimiter line */
   int i;                        /* Card index within the block */
   int ncard;                    /* No. of cards in the block */
   int ret;                      /* The returned flag */   
   size_t len;                   /* Length of the comment text */

/* Check the global status. */
   if( !astOK ) return 0;

/* Save the pointer to the current card. */
   card0 = this->card;

/* Initialise the returned flag to indicate that we have not found a
   comment block. */
   ret = 0;

/* Move on to the first card in the block. If this is not possible (due to 
   us already being at the start or end of the FitsChan), then return. */
   if( MoveCard( this, incr, method, class ) == 1 ) {

/* Store the character which is used in the delimiter line for the
   comment block. */
      del = ( incr == 1 ) ? '-' : '+';

/* Initialise the number of cards in the comment block to zero. */
      ncard = 0;

/* Loop round until the end (or start) of the comment block is found.
   Leave the loop if an error occurs.  */
      while( astOK ) {

/* Is this card a comment card? If not, then we have failed to find a 
   complete comment block. Break out of the loop. */
         if( CardType( this ) != AST__COMMENT ) break;

/* Get the text of the comment, and its length. */
         text = CardComm( this );
         len = strlen( text );

/* Check the first 3 characters. Break out of the loop if they are not
   "AST". */
         if( strncmp( "AST", text, 3 ) ) break;

/* Check the last 3 characters. Break out of the loop if they are not
   "AST". */
         if( strcmp( "AST", text + len - 3 ) ) break;

/* Increment the number of cards in the comment block. */
         ncard++;

/* If the comment is the appropriate block delimiter (a line of +'s or
   -'s depending on the direction), then set the flag to indicate that we
   have a complete comment block and leave the loop. Allow spaces to be
   included. Exclude the "AST" strings at begining and end from the check. */
         ret = 1;
         for( i = 3; i < len - 3; i++ ) {
            if( text[ i ] != del && text[ i ] != ' ' ) {
               ret = 0;
               break;
            }
         }

         if( ret ) break;

/* Move on to the next card. If this is not possible (due to us already
   being at the start or end of the FitsChan), then break out of the loop. */
         if( MoveCard( this, incr, method, class ) == 0 ) break;

      }

/* Re-instate the original current card. */
      this->card = card0;

/* If we found a complete comment block, delete it, and then re-instate
   the original current card. */
      if( ret && astOK ) {

         for( i = 0; i < ncard; i++ ) {
            MoveCard( this, incr, method, class );
            MarkCard( this );
         }

         this->card = card0;

      }
   }

/* If an error occurred, indicate that coment block has been deleted. */
   if( !astOK ) ret = 0;

   return ret;

}

static int CountFields( const char *temp, char type, const char *method, 
                        const char *class ){
/*
*  Name:
*     CountFields

*  Purpose:
*     Count the number of field specifiers in a template string. 

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int CountFields( const char *temp, char type, const char *method, 
*                      const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function returns the number of fields which include the
*     specified character type in the supplied string.

*  Parameters:
*     temp
*        Pointer to a null terminated string holding the template.
*     type
*        A single character giving the field type to be counted (e.g.
*        'd', 'c' or 'f').
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     The number of fields.

*  Notes:
*     -  No error is reported if the parameter "type" is not a valid
*     field type specifier, but zero will be returned.
*     -  An error is reported if the template has any invalid field 
*     specifiers in it.
*     -  A value of zero is returned if an error has already occurred,
*     or if this function should fail for any reason.

*/
/* Local Variables: */
   const char *b;         /* Pointer to next template character */
   int nf;                /* No. of fields found so far */

/* Check global status. */
   if( !astOK ) return 0;

/* Initialise a pointer to the start of the template string. */
   b = temp;

/* Initialise the number of fields found so far. */
   nf = 0;

/* Go through the string. */
   while( *b ){

/* If the current character is a '%', a field is starting. */   
      if( *b == '%' ){

/* Skip over the field width (if supplied). */
         if( isdigit( (int) *(++b) ) ) b++;

/* Report an error if the end of the string occurs within the field. */
         if( !*b ) {
            astError( AST__BDFMT, "%s(%s): Incomplete field specifier found "
                      "at end of filter template '%s'.", method, class, 
                      temp );
            break;

/* Report an error if the field type is illegal. */
         } else if( *b != 'd' && *b != 'c' && *b != 'f' ) {
            astError( AST__BDFMT, "%s(%s): Illegal field type or width "
                      "specifier '%c' found in filter template '%s'.", 
                      method, class, *b, temp );
            break;
         }

/* Compare the field type with the supplied type, and increment the 
   number of fields found if it is the correct type. */
         if( *b == type ) nf++;

      }

/* Move on to the next character. */
      b++;   

   }

/* If an error has occurred, return 0. */
   if( !astOK ) nf = 0;

/* Return the answer. */
   return nf;

}

static void CreateKeyword( AstFitsChan *this, const char *name,
                           char keyword[ FITSNAMLEN + 1 ] ){
/*
*  Name:
*     CreateKeyword

*  Purpose:
*     Create a unique un-used keyword for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void CreateKeyword( AstFitsChan *this, const char *name,
*                         char keyword[ FITSNAMLEN + 1 ] )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function takes a name which forms the basis of a FITS
*     keyword and appends a sequence number (encoded as a pair of
*     legal FITS keyword characters) so as to generate a unique FITS
*     keyword which has not previously been used in the FitsChan
*     supplied.
*
*     It is intended for use when several keywords with the same name
*     must be stored in a FitsChan, since to comply strictly with the
*     FITS standard keywords should normally be unique (otherwise
*     external software which processes the keywords might omit one or
*     other of the values).
*
*     An attempt is also made to generate keywords in a form that is
*     unlikely to clash with those from other sources (in as far as
*     this is possible with FITS). In any event, a keyword that
*     already appears in the FitsChan will not be re-used.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name on which the new keyword should be based. This should be
*        a legal FITS keyword in itself, except that it should be at
*        least two characters shorter than the maximum length, in
*        order to accommodate the sequence number characters.
*
*        If this string is too long, it will be silently
*        truncated. Mixed case is permitted, as all characters
*        supplied are converted to upper case before use.
*     keyword
*        A character array in which the generated unique keyword will
*        be returned, null terminated.
*/

/* Local Variables: */
   const char *class;            /* Object clas */
   FitsKeySeq *keyseq;           /* Pointer to sequence number list entry */
   char *seq_chars = SEQ_CHARS;  /* Pointer to characters used for encoding */
   int found;                    /* Keyword entry found in list? */
   int icard;                    /* Index of current card on entry */
   int limit;                    /* Sequence number has reached limit? */
   int nc;                       /* Number of basic keyword characters */
   static int seq_nchars = -1;   /* Number of characters used for encoding */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the object class. */
   class = astGetClass( this );

/* Remember the index of the current card. */
   icard = astGetCard( this );

/* On the first invocation only, determine the number of characters
   being used to encode sequence number information and save this
   value. */
   if ( seq_nchars < 0 ) seq_nchars = (int) strlen( seq_chars );

/* Copy the name supplied into the output array, converting to upper
   case. Leave space for two characters to encode a sequence
   number. Terminate the resulting string. */
   for ( nc = 0; name[ nc ] && ( nc < ( FITSNAMLEN - 2 ) ); nc++ ) {
      keyword[ nc ] = toupper( name[ nc ] );
   }
   keyword[ nc ] = '\0';

/* We now search the list of sequence numbers already allocated to
   find the next one to use for this keyword. Obtain a pointer to the
   start of this list and loop until the end of the list is reached,
   or the keyword is found. */
   found = 0;
   keyseq = (FitsKeySeq *) this->keyseq;
   while ( keyseq && !found ) {

/* Compare each entry with the keyword we want to match. */
      if ( !strcmp( keyseq->key, keyword ) ) {
         found = 1;

/* Test the next list entry if the current one doesn't match. */
      } else {
         keyseq = keyseq->next;
      }
   }

/* If the keyword was not found in the list, create a new list entry
   to describe it. */
   if ( !found ) {
      keyseq = astMalloc( sizeof( FitsKeySeq ) );

/* If OK, store a copy of the keyword and initialise its sequence
   number (note that sequence number zero is not actually used for
   cosmetic reasons). */
      if ( astOK ) {
         keyseq->key = astString( keyword, nc );
         keyseq->seq = 0;

/* Prefix the new entry to the list. */
         if ( astOK ) {
            keyseq->next = (FitsKeySeq *) this->keyseq;
            this->keyseq = (void *) keyseq;

/* If an error occurred, clean up by freeing the memory allocated for
   the new list entry. */
         } else {
            keyseq = astFree( keyseq );
         }
      }
   }

/* If OK, loop to find a new sequence number which results in a FITS
   keyword that hasn't already been used to store data in the
   FitsChan. */
   if ( astOK ) {
      while ( 1 ) {

/* Determine if the sequence number just obtained has reached the
   upper limit. This is unlikely to happen in practice, but if it
   does, we simply re-use this maximum value. Otherwise, we increment
   the sequence number last used for this keyword to obtain a new
   one. */
         limit = ( keyseq->seq >= ( seq_nchars * seq_nchars - 1 ) );
         if ( !limit ) keyseq->seq++;

/* Encode the sequence number into two characters and append them to
   the original keyword (with a terminating null). */
         keyword[ nc ] = seq_chars[ keyseq->seq / seq_nchars ];
         keyword[ nc + 1 ] = seq_chars[ keyseq->seq % seq_nchars ];
         keyword[ nc + 2 ] = '\0';

/* If the upper sequence number limit has not been reached, try to
   look up the resulting keyword in the FitsChan to see if it has
   already been used. Quit searching when a suitable keyword is
   found. */
         if ( limit || !SearchCard( this, keyword, "astWrite", class ) ) break;
      }
   }

/* Reinstate the original current card. */
   astSetCard( this, icard );

}

static void Crota( int prim, FitsStore *store, int naxis, int axlon, 
                   int axlat ){
/*
*  Name:
*     Crota

*  Purpose:
*     Calculate and store a value for the FITS-WCS or FITS-IRAF CROTA keyword.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void Crota( int prim, FitsStore *store, int naxis, int axlon, int axlat )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Stores a value for the deprecated CROTA keyword for the benefit of 
*     older FITS readers. This is not possible if the PC matrix has any 
*     significant skew. Equation 133 from the Greisen & Calabretta paper 
*     describing FITS-WCS is used. Unused elements of the crota array in
*     "store" are filled with bad values.

*  Parameters:
*     prim
*        Are primary axis descriptions being created? If not, the CROTA
*        keywords are assigned bad values since CROTA is only meaningful
*        in the context of primary axis descriptions.
*     store
*        Pointer to the FitsStore in which to store the CROTA value.
*        It must already contain PC and CDELT values.
*     naxis
*        The number of axes in the pixel Frame.
*     axlon
*        The index of the longitude axis.
*     axlat
*        The index of the latitude axis.

*/

/* Local Variables: */
   double pcii;             /* PC matrix element (i,i) */
   double pcij;             /* PC matrix element (i,j) */
   double pcji;             /* PC matrix element (j,i) */
   double pcjj;             /* PC matrix element (j,j) */
   double roi;              /* First estimate of CROTA */
   double roj;              /* Second estimate of CROTA */
   double S;                /* Ratio of pixel sizes */
   int i;                   /* Loop count */

/* Check the global status. */
   if( !astOK ) return;

   for( i = 0; i < naxis; i++ ) store->crota[ i ] = AST__BAD;
   
   if( store->cdelt[ axlon ] != 0.0 && 
       store->cdelt[ axlat ] != 0.0 && prim ){

      if( store->pc ){
         pcii = store->pc[ axlon*naxis + axlon ];
         pcji = store->pc[ axlat*naxis + axlon ];
         pcij = store->pc[ axlon*naxis + axlat ];
         pcjj = store->pc[ axlat*naxis + axlat ];
         if( pcii == AST__BAD ) pcii = 1.0;
         if( pcij == AST__BAD ) pcij = 0.0;
         if( pcji == AST__BAD ) pcji = 0.0;
         if( pcjj == AST__BAD ) pcjj = 1.0;

         S = store->cdelt[ axlat ]/store->cdelt[ axlon ];
         roi = atan2( pcji*S, pcii );
         roj = atan2( -pcij/S, pcjj );

         if( fabs( slaDrange( roi - roj ) ) < 1.0E-3 ){
            store->crota[ axlat ] = 0.5*( slaDranrm( roi ) + slaDranrm( roj ) );
         }

      } else {
         store->crota[ axlat ] = 0.0;
      }

   }

}

static double DateObs( const char *dateobs ) {
/*
*  Name:
*     DateObs

*  Purpose:
*     Convert a FITS DATE-OBS keyword value to a MJD.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     double DateObs( const char *dateobs )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Extracts the date and time fields from the supplied string and converts 
*     them into a modified Julian Date. Supports both old "dd/mm/yy"
*     format, and the new "ccyy-mm-ddThh:mm:ss[.sss...]Z" format.

*  Parameters:
*     dateobs
*        Pointer to the DATE-OBS string.

*  Returned Value:
*     The Modified Julian Date corresponding to the supplied DATE-OBS
*     string.

*  Notes:
*     -  The value AST__BAD is returned (without error) if the supplied 
*     string does not conform to the requirements of a FITS DATE-OBS value,
*     or if an error has already occurred.
*/

/* Local Variables: */
   double days;               /* The hours, mins and secs as a fraction of a day */
   double ret;                /* The returned MJD value */
   double secs;               /* The total value of the two seconds fields */
   int dd;                    /* The day field from the supplied string */
   int fsc;                   /* The fractional seconds field from the supplied string */
   int hr;                    /* The hour field from the supplied string */
   int j;                     /* SLALIB status */
   int len;                   /* The length of the supplied string */
   int mm;                    /* The month field from the supplied string */
   int mn;                    /* The minute field from the supplied string */
   int nc;                    /* Number of characters used */
   int ok;                    /* Was the string of a legal format? */
   int rem;                   /* The least significant digit in fsc */
   int sc;                    /* The whole seconds field from the supplied string */
   int yy;                    /* The year field from the supplied string */


/* Check the global status. */
   if( !astOK ) return AST__BAD;

/* Initialise the returned value. */
   ret = AST__BAD;

/* Save the length of the supplied string. */
   len = (int) strlen( dateobs );

/* Extract the year, month, day, hour, minute, second and fractional
   seconds fields from the supplied string. Assume initially that the 
   string does not match any format. */
   ok = 0;   

/* First check for the old "dd/mm/yy" format. */
   if( nc = 0,
        ( sscanf( dateobs, " %2d/%2d/%d %n", &dd, &mm, &yy, &nc ) == 3 ) &&
        ( nc >= len )  ){
      ok = 1;
      hr = 0;
      mn = 0;
      sc = 0;
      fsc = 0;

/* Otherwise, check for the new short format "ccyy-mm-dd". */
   } else if( nc = 0,
        ( sscanf( dateobs, " %4d-%2d-%2d %n", &yy, &mm, &dd, &nc ) == 3 ) &&
        ( nc >= len )  ){
      ok = 1;
      hr = 0;
      mn = 0;
      sc = 0;
      fsc = 0;

/* Otherwise, check for the new format "ccyy-mm-ddThh:mm:ss" without a 
   fractional seconds field or the trailing Z. */
   } else if( nc = 0,
        ( sscanf( dateobs, " %4d-%2d-%2dT%2d:%2d:%2d %n", &yy, &mm, &dd,
                  &hr, &mn, &sc, &nc ) == 6 ) && ( nc >= len )  ){
      ok = 1;
      fsc = 0;

/* Otherwise, check for the new format "ccyy-mm-ddThh:mm:ss.sss" with a 
   fractional seconds field but without the trailing Z. */
   } else if( nc = 0,
        ( sscanf( dateobs, " %4d-%2d-%2dT%2d:%2d:%2d.%d %n", &yy, &mm, &dd,
                  &hr, &mn, &sc, &fsc, &nc ) == 7 ) && ( nc >= len )  ){
      ok = 1;

/* Otherwise, check for the new format "ccyy-mm-ddThh:mm:ssZ" without a 
   fractional seconds field but with the trailing Z. */
   } else if( nc = 0,
        ( sscanf( dateobs, " %4d-%2d-%2dT%2d:%2d:%2dZ %n", &yy, &mm, &dd,
                  &hr, &mn, &sc, &nc ) == 6 ) && ( nc >= len )  ){
      ok = 1;
      fsc = 0;

/* Otherwise, check for the new format "ccyy-mm-ddThh:mm:ss.sssZ" with a 
   fractional seconds field and the trailing Z. */
   } else if( nc = 0,
        ( sscanf( dateobs, " %4d-%2d-%2dT%2d:%2d:%2d.%dZ %n", &yy, &mm, &dd,
                  &hr, &mn, &sc, &fsc, &nc ) == 7 ) && ( nc >= len )  ){
      ok = 1;
   }

/* If the supplied string was legal, create a MJD from the separate fields. */
   if( ok ) { 

/* Get the MJD at the start of the day. */
      slaCaldj( yy, mm, dd, &ret, &j );

/* If succesful, convert the hours, minutes and seconds to a fraction of
    a day, and add it onto the MJD found above. */
      if( j == 0 ) {

/* Obtain a floating point representation of the fractional seconds
   field. */
         secs = 0.0;
         while ( fsc > 0 ) {
             rem = ( fsc % 10  );
             fsc /= 10;
             secs = 0.1 * ( secs + (double) rem );
         }

/* Add on the whole seconds field. */
         secs += (double) sc;

/*Convert the hours, minutes and seconds to a fractional day. */
         slaDtf2d( hr, mn, secs, &days, &j );

/* If succesful, add this onto the returned MJD. */
         if( j == 0 ) {
            ret = ret + days;         

/* If the conversion to MJD failed, return AST__BAD. */
         } else {
            ret = AST__BAD;
         }

      } else {
         ret = AST__BAD;
      }
   } 

/* Return the result. */
   return ret;   

}

static void DeleteCard( AstFitsChan *this, const char *method, 
                        const char *class ){
/*
*  Name:
*     DeleteCard

*  Purpose:
*     Delete the current card from a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void DeleteCard( AstFitsChan *this, const char *method,
*                      const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The current card is removed from the circular linked list of structures 
*     stored in the supplied FitsChan, and the memory used to store the 
*     structure is then freed.

*  Parameters:
*     this
*        Pointer to the FitsChan containing the list.
*     method
*        Name of calling method.
*     class
*        Object class.

*  Notes:
*     -  This function returns without action if the FitsChan is
*     currently at "end-of-file".
*     -  The next card becomes the current card.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   FitsCard *card;            /* Pointer to the current card */
   FitsCard *next;            /* Pointer to next card in list */
   FitsCard *prev;            /* Pointer to previous card in list */

/* Return if the supplied object or current card is NULL. */
   if( !this || !this->card ) return;

/* Get a pointer to the card to be deleted (the current card). */
   card = (FitsCard *) this->card;

/* Move the current card on to the next card. */
   MoveCard( this, 1, method, class );

/* Save pointers to the previous and next cards in the list. */
   prev = GetLink( card, PREVIOUS, method, class );
   next = GetLink( card, NEXT, method, class );

/* If the backwards link points back to the supplied card, then it must
   be the only one left on the list. */
   if( prev == card ) prev = NULL;
   if( next == card ) next = NULL;

/* If the list head is to be deleted, store a value for the new list
   head. */
   if( this->head == (void *) card ) this->head = (void *) next;

/* Free the memory used to hold the data value. */
   (void) astFree( card->data );

/* Free the memory used to hold any comment. */
   if( card->comment ) (void) astFree( (void *) card->comment );

/* Free the memory used to hold the whole structure. */
   (void) astFree( (void *) card );

/* Fix up the links between the two adjacent cards in the list, unless the 
   supplied card was the last one in the list. */
   if( prev && next ){
      next->prev = prev;
      prev->next = next;

   } else {
      this->head = NULL;
      this->card = NULL;
   }

/* Return. */
   return;

}

static void DelFits( AstFitsChan *this ){
/*
*++
*  Name:
c     astDelFits
f     AST_DELFITS

*  Purpose:
*     Delete the current FITS card in a FitsChan.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "fitschan.h"
c     void astDelFits( AstFitsChan *this )
f     CALL AST_DELFITS( THIS, STATUS )

*  Class Membership:
*     FitsChan method.

*  Description:
c     This function deletes the current FITS card from a FitsChan. The
f     This routine deletes the current FITS card from a FitsChan. The
*     current card may be selected using the Card attribute (if its index
c     is known) or by using astFindFits (if only the FITS keyword is
f     is known) or by using AST_FINDFITS (if only the FITS keyword is
*     known).
*
*     After deletion, the following card becomes the current card.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsChan.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - This function returns without action if the FitsChan is
*     initially positioned at the "end-of-file" (i.e. if the Card
*     attribute exceeds the number of cards in the FitsChan).
*     - If there are no subsequent cards in the FitsChan, then the
*     Card attribute is left pointing at the "end-of-file" after
*     deletion (i.e. is set to one more than the number of cards in
*     the FitsChan).
*--
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Delete the current card. The next card will be made the current card. */
   DeleteCard( this, "astDelFits", astGetClass( this ) );

}

static int DescIraf( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
                    int iphyfrm, int naxis, FitsStore *store ){
/*
*  Name:
*     DescIraf

*  Purpose:
*     Create FITS-IRAF encoded axis descriptions based on a specified physical 
*     coordinate Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int DescIraf( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
*                  int iphyfrm, int naxis, FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates FITS-IRAF header cards describing the
*     relationship between the specified pixel and physical coordinate 
*     Frames, and stores them in the supplied FitsChan. Keywords
*     describing every axis in the physical Frame are created and stored, 
*     together with the required global keywords which relate to all axes 
*     (eg RADECSYS, LONGPOLE, CDiiijjj, etc). 

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     fset
*        Pointer to the FrameSet.
*     ipixfrm
*        The index of the pixel coordinate frame within "fset".
*     iphyfrm
*        The index of the physical coordinate frame within "fset".
*     naxis
*        The number of axes in the pixel coordinate Frame.
*     store
*        Pointer to a structure to use as a temporary store for
*        IRAF keyword values before putting them into the FitsChan.

*  Returned Value:
*     One if any keywords were stored in the FitsChan. Zero otherwise.

*  Notes:
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   int ret;                 /* Were any cards added to the FitsChan? */
   int i;                   /* Loop count */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that nothing has been added
   to the FitsChan. */
   ret = 0;

/* Initialise the store structure. This is the same type of structure used
   to store FITS-WCS keywords, and so it contains components which will not
   actually be used. Even so, we need to initialise them properly so that 
   they can be freed safely when the structure is no longer needed. */
   store->prj = NULL;         /* Pointer to array of WCS projections */
   store->cdelt_name = NULL;  /* Pointers to CDELT keyword names */
   store->crota_name = NULL;  /* Pointers to CROTA keyword names */
   store->crpix_name = NULL;  /* Pointers to CRPIX keyword names */
   store->crval_name = NULL;  /* Pointers to CRVAL keyword names */
   store->ctype = NULL;       /* Pointer to array of CTYPE values */
   store->ctype_name = NULL;  /* Pointers to CTYPE keyword names */
   store->ctype_com = NULL;   /* Pointers to CTYPE keyword comments */
   store->cunit = NULL;       /* Pointer to array of CUNIT values */
   store->cdelt = NULL;       /* Pointer to array of CDELT values */
   store->crota = NULL;       /* Pointer to array of CROTA values */
   store->crpix = NULL;       /* Pointer to array of CRPIX values */
   store->crval = NULL;       /* Pointer to array of CRVAL values */
   store->pc = NULL;          /* Pointer to the PC matrix. */
   store->ialt = NULL;        /* Pointer to array of axis description counts */
   store->axlat = -1;         /* Index of latitude axis */
   store->axlon = -1;         /* Index of longitude axis */
   store->npar = 0;           /* No. of supplied projection parameters */
   store->naxis = 0;           /* No. of axes */
   store->equinox = 0.0;      /* Epoch of reference equinox */
   store->latpole = 0.0;      /* LATPOLE value in radians */
   store->longpole = 0.0;     /* LONGPOLE value in radians */
   store->mjdobs = 0.0;       /* Modified Julian Date of observation */
   store->julian = 1;         /* Is equinox value Beselian or Julian? */
   store->noncel = 0;         /* No. of non-celestial axes */
   store->radecsys = NORADEC; /* Flag identifying frame of reference */
   store->sys = NOCEL;        /* Flag identifying celestial coord. sys.*/

/* Allocate storage for the required arrays. */
   store->ctype = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
   store->ctype_com = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
   store->cunit = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
   store->cdelt = (double *) astMalloc( sizeof( double )*(size_t)naxis );
   store->crota = (double *) astMalloc( sizeof( double )*(size_t)naxis );
   store->crpix = (double *) astMalloc( sizeof( double )*(size_t)naxis );
   store->crval = (double *) astMalloc( sizeof( double )*(size_t)naxis );
   store->pc = (double *) astMalloc( sizeof( double )*(size_t)(naxis*naxis) );

/* Initialise the string pointers. */
   for( i = 0; i < naxis; i++ ){
      store->ctype[ i ] = NULL;
      store->ctype_com[ i ] = NULL;
      store->cunit[ i ] = NULL;
   }

/* Store the number of axes. */
   store->naxis = naxis;

/* Get the keyword values describing the axes in the specified physical
   coordinate Frame, plus the other global keyword values. The FITS-WCS
   values are created initially, and then a check is performed (in
   IrafStore) to ensure that they are consistent with the FITS-IRAF 
   encoding. */
   if( WcsValues( this, fset, ipixfrm, iphyfrm, naxis, 1, store ) ){

/* Store all the keyword values as header cards in the FitsChan, if
   possible. */
      ret = IrafStore( this, store );

   }

/* If an error has occurred, indicate that nothing has been added to the 
   FitsChan. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static int DescWcs( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
                    int iphyfrm, int naxis, int prim, FitsStore *store ){
/*
*  Name:
*     DescWcs

*  Purpose:
*     Create WCS-encoded axis descriptions based on a specified physical 
*     coordinate Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int DescWcs( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
*                  int iphyfrm, int naxis, int prim, FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates FITS-WCS header cards describing the
*     relationship between the specified pixel and physical coordinate 
*     Frames, and stores them in the supplied FitsChan. This function
*     should be called first to create the primary axis descriptions 
*     (encoded in keywords CRVALi, CRPIXi, etc), in which case keywords 
*     describing every axis in the physical Frame are created and stored, 
*     together with the required global keywords which relate to all axes 
*     (eg RADECSYS, LONGPOLE, PCiiijjj, etc). This function may then be 
*     called again to create secondary axis descriptions (encoded in 
*     keywords CmVALi, CmPIXi, etc), in which case keywords are only 
*     stored for axes which differ from all other axes previously 
*     stored in the FitsChan. In addition, secondary axis descriptions
*     are only stored if the values derived for the global keywords match
*     those stored with the primary axis descriptions.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     fset
*        Pointer to the FrameSet.
*     ipixfrm
*        The index of the pixel coordinate frame within "fset".
*     iphyfrm
*        The index of the physical coordinate frame within "fset".
*     naxis
*        The number of axes in the pixel coordinate Frame.
*     prim
*        Are the primary axis descriptions to be produced?
*     store
*        Pointer to a structure to use as a temporary store for
*        FITS-WCS keyword values before putting them into the FitsChan.

*  Returned Value:
*     One if any keywords were stored in the FitsChan. Zero otherwise.

*  Notes:
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   int ret;                 /* Were any cards added to the FitsChan? */
   int i;                   /* Loop count */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that nothing has been added
   to the FitsChan. */
   ret = 0;

/* If the primary axis descriptions are being created (i.e. if this is
   the first entry into this function), initialise the store structure. */
   if( prim ){
      store->prj = NULL;         /* Pointer to array of WCS projections */
      store->cdelt_name = NULL;  /* Pointers to CDELT keyword names */
      store->crota_name = NULL;  /* Pointers to CROTA keyword names */
      store->crpix_name = NULL;  /* Pointers to CRPIX keyword names */
      store->crval_name = NULL;  /* Pointers to CRVAL keyword names */
      store->ctype = NULL;       /* Pointer to array of CTYPE values */
      store->ctype_name = NULL;  /* Pointers to CTYPE keyword names */
      store->ctype_com = NULL;   /* Pointers to CTYPE keyword comments */
      store->cunit = NULL;       /* Pointer to array of CUNIT values */
      store->cdelt = NULL;       /* Pointer to array of CDELT values */
      store->crota = NULL;       /* Pointer to array of CROTA values */
      store->crpix = NULL;       /* Pointer to array of CRPIX values */
      store->crval = NULL;       /* Pointer to array of CRVAL values */
      store->pc = NULL;          /* Pointer to the PC matrix. */
      store->ialt = NULL;        /* Pointer to array of axis description counts */
      store->axlat = -1;         /* Index of latitude axis */
      store->axlon = -1;         /* Index of longitude axis */
      store->npar = 0;           /* No. of supplied projection parameters */
      store->naxis = 0;           /* No. of axes */
      store->equinox = 0.0;      /* Epoch of reference equinox */
      store->latpole = 0.0;      /* LATPOLE value in radians */
      store->longpole = 0.0;     /* LONGPOLE value in radians */
      store->mjdobs = 0.0;       /* Modified Julian Date of observation */
      store->julian = 1;         /* Is equinox value Beselian or Julian? */
      store->noncel = 0;         /* No. of non-celestial axes */
      store->radecsys = NORADEC; /* Flag identifying frame of reference */
      store->sys = NOCEL;        /* Flag identifying celestial coord. sys.*/

/* Allocate storage for the required arrays. */
      store->ctype = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
      store->ctype_com = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
      store->cunit = (char **) astMalloc( sizeof( char *)*(size_t)naxis );
      store->cdelt = (double *) astMalloc( sizeof( double )*(size_t)naxis );
      store->crota = (double *) astMalloc( sizeof( double )*(size_t)naxis );
      store->crpix = (double *) astMalloc( sizeof( double )*(size_t)naxis );
      store->crval = (double *) astMalloc( sizeof( double )*(size_t)naxis );
      store->pc = (double *) astMalloc( sizeof( double )*(size_t)(naxis*naxis) );

/* Initialise the string pointers. */
      for( i = 0; i < naxis; i++ ){
         store->ctype[ i ] = NULL;
         store->ctype_com[ i ] = NULL;
         store->cunit[ i ] = NULL;
      }

/* Store the number of axes. */
      store->naxis = naxis;
   }

/* Get the keyword values describing the axes in the specified physical
   coordinate Frame, plus the other global keyword values. */
   if( WcsValues( this, fset, ipixfrm, iphyfrm, naxis, prim, store ) ){

/* If we are creating primary axis descriptions, store all the keyword
   values as header cards in the FitsChan. */
      if( prim ){
         ret = WcsPrimary( this, store, 0 );

/* If we are creating secondary axis descriptions, check to see if there
   are any suitable new axis descriptions in the physical Frame, and store 
   them in the FitsChan if there are. */
      } else {
         ret = WcsSecondary( this, store );
      }

   }

/* If an error has occurred, indicate that nothing has been added to the 
   FitsChan. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static AstSkyFrame *DSSFrame( AstFitsChan *this, const char *method, 
                           const char *class  ){
/*
*  Name:
*     DSSFrame

*  Purpose:
*     Create a Frame to describe a Digitised Sky Survey celestial coordinate 
*     system.

*  Type:
*     Private function.

*  Synopsis:
*     AstSkyFrame *DSSFrame( AstFitsChan *this, const char *method, 
*                            const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A SkyFrame is returned describing the celestial coordinate system of
*     a DSS-encoded FitsChan. 

*  Parameters:
*     this
*        A pointer to the FitsChan containing the DSS keywords.
*     method
*        The calling method. Used only in error messages.
*     class 
*        The object class. Used only in error messages.

*  Returned Value:
*     A pointer to the SkyFrame.

*  Notes:
*     -  A NULL pointer is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   AstSkyFrame *ret;              /* Returned SkyFrame */
   double ep;                     /* Epoch of observation */
   double eq;                     /* Epoch of reference equinox */
   int gotep;                     /* Was a value obtained for EPOCH? */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned SkyFrame pointer. */
   ret = NULL;

/* Get the EQUINOX and EPOCH values. EPOCH defaults to EQUINOX if it is
   not supplied. Assume a default EQUINOX of 2000.0 */
   eq = 2000.0;
   GetWcsValue( this, "EQUINOX", AST__FLOAT, (void *) &eq, 0, method, 
                class );
   ep = eq;
   gotep = GetWcsValue( this, "EPOCH", AST__FLOAT, (void *) &ep, 0, method, 
                           class );

/* If the equinox is 1950 (or there-abouts), assume FK4. For any other 
   equinox assume FK5. This behaviour is copied from the "wcsinit"
   function in the SAOimage file "wcs.c". */
   if( NINT( eq ) == 1950 ){
      ret = astSkyFrame( "System=FK4" );
      astSet( ret, "Equinox= B %f", eq );
      astSet( ret, "Epoch= B %f", ep );
   } else {
      ret = astSkyFrame( "System=FK5" );
      astSet( ret, "Equinox= J %f", eq );
      astSet( ret, "Epoch= J %f", ep );
   }

/* Store a description of the projection. */
   astSetProjection( ret, "digitised sky survey" );

/* If an error has occurred, annul the SkyFrame. */
   if( !astOK ) ret = astAnnul( ret );
   
/* Return the SkyFrame. */
   return ret;

}

static void Empty( AstFitsChan *this ){
/*
*+
*  Name:
*     astEmpty

*  Purpose:
*     Remove all cards and related data from a FitsChan. 

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     void astEmpty( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function removes all cards and associated information from the 
*     supplied FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.

*-
*/

/* Local Variables: */
   const char *class;         /* Pointer to string holding object class */
   const char *method;        /* Pointer to string holding calling method */
   FitsKeySeq *tmp;           /* Temporary pointer to list element */
   FitsKeySeq *keyseq;        /* Temporary pointer to list head */
   int old_skipping;          /* Original setting of external Skipping variable */

/* Store the method and class strings. */
   method = "astEmpty";
   class = astGetClass( this );

/* Delete all cards from the circular linked list stored in the FitsChan,
   starting with the card at the head of the list. */
   old_skipping = Skipping;
   Skipping = 0;
   astClearCard( this );
   while( !astFitsEof( this ) ) DeleteCard( this, method, class );   
   Skipping = old_skipping;

/* Empty the list which holds keywords and the latest sequence number
   used by each of them, by repeatedly removing the first element. */

   keyseq = (FitsKeySeq *) this->keyseq;
   while ( keyseq ) {

/* Free the keyword string. */
      keyseq->key = astFree( keyseq->key );

/* Remove the first element from the list, retaining a pointer to
   it. */
      tmp = keyseq;
      keyseq = tmp->next;

/* Free the memory used by the first element. */
      tmp = astFree( tmp );
   }

/* Store a NULL pointer in the FitsChan. */
   this->keyseq = NULL;
}

static int EncodeFloat( char *buf, int digits, int width, int maxwidth,
                        double value ){
/*
*
*  Name:
*     EncodeFloat

*  Purpose:
*     Formats a floating point value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int EncodeFloat( char *buf, int digits, int width, int maxwidth,
*                      double value )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function formats the value using a G format specified in order
*     to use the minimum field width (trailing zeros are not printed). 
*     However, the G specifier does not include a decimal point unless it
*     is necessary. FITS requires that floating point values always include
*     a decimal point, so this function inserts one, if necessary. 

*  Parameters:
*     buf
*        A character string into which the value is written. 
*     digits
*        The number of digits after the decimal point. If the supplied value
*        is negative, the number of digits actually used may be reduced if 
*        the string would otherwise extend beyond the number of columns 
*        allowed by the FITS standard. If the value is positive, the 
*        specified number of digits are always produced, even if it means 
*        breaking the FITS standard.
*     width
*        The minimum field width to use. The value is right justified in 
*        this field width.
*     maxwidth
*        The maximum field width to use. A value of zero is returned if 
*        the maximum field width is exceeded.
*     value
*        The value to format.

*  Returned Value:
*     The field width actually used, or zero if the value could not be
*     formatted. This does not include the trailing null character.

*  Notes:
*     -  If there is room, a trailing zero is also added following the 
*     inserted decimal point. 

*/

/* Local Variables: */
   char *w, *r;
   char *c;
   int i;
   int ldigits;
   int ret;
   int n;

/* Check the global error status. */
   if ( !astOK ) return 0; 

/* The supplied value of "digits" may be negative. Obtain the positive
   value giving the initial number of decimal digits to use. */   
   ldigits = ( digits > 0 ) ? digits : -digits;

/* Loop until a suitably encoded value has been obtained. */
   while( 1 ){

/* Write the value into the buffer. */
      (void) sprintf( buf, "%*.*G", width, ldigits, value );
      CheckZero( buf, value );

/* If the formatted value includes an exponent, it will have 2 digits.
   If the exponent includes a leading zero, remove it. */
      if( ( w = strstr( buf, "E-0" ) ) ) {
         w += 2;
      } else if( ( w = strstr( buf, "E+0" ) ) ){
         w += 2;
      } else if( ( w = strstr( buf, "E0" ) ) ){
         w += 1;
      } 

/* If a leading zero was found, shuffle everything down from the start of
   the string by one character, over-writing the redundant zero, and insert
   a space at the start of the string. */
      if( w ) {
         r = w - 1 ;
         while( w != buf ) *(w--) = *(r--);
         *w = ' ';
      }

/* If the used field width was too large, reduce it and try again, so
   long as we are allowed to change the number of digits being used. */
      ret = strlen( buf );
      if( ret > width && digits < 0 ){
         ldigits -= ( ret - width );
       
/* Otherwise leave the loop. Return zero field width if the maximum field 
   width was exceeded. */
      } else {
         if( ret > maxwidth ) ret = 0;
         break;
      }

   }

/* If a formatted value was obtained, we need to ensure that the it includes 
   a decimal point. */
   if( ret ){

/* Get a pointer to the first digit in the buffer. */
      c = strpbrk( buf, "0123456789" );

/* Something funny is going on if there are no digits in the buffer,
   so return a zero field width. */
      if( !c ){
         ret = 0;

/* Otherwise... */
      } else {

/* Find the number of digits following and including the first digit. */
         n = strspn( c, "0123456789" );

/* If the first non-digit character is a decimal point, do nothing. */
         if( c[ n ] != '.' ){

/* If there are two or more leading spaces, move the start of the string
   two character to the left, and insert ".0" in the gap created. This
   keeps the field right justified within the desired field width. */
            if( buf[ 0 ] == ' ' && buf[ 1 ] == ' ' ){
               for( i = 2; i < c - buf + n; i++ ) buf[ i - 2 ] = buf[ i ];
               c[ n - 2 ] = '.';
               c[ n - 1 ] = '0';

/* If there is just one leading space, move the start of the string
   one character to the left, and insert "." in the gap created. This
   keeps the field right justified within the desired field width. */
            } else if( buf[ 0 ] == ' ' ){
               for( i = 0; i < n; i++ ) c[ i - 1 ] = c[ i ];
               c[ n - 1 ] = '.';
  
/* If there are no leading spaces we need to move the end of the string
   to the right. This will result in the string no longer being right 
   justified in the required field width. Return zero if there is 
   insufficient room for an extra character. */
            } else {
               ret++;
               if( ret > maxwidth ){
                  ret = 0;

/* Otherwise, more the end of the string one place to the right and insert 
   the decimal point. */
               } else {
                  for( i = strlen( c ); i >= n; i-- ) c[ i + 1 ] = c[ i ];
                  c[ n ] = '.';
               }
            }
         }
      }
   }

/* Return the field width. */
   return ret;

}

static int EncodeValue( AstFitsChan *this, char *buf, int col, int digits,
                        const char *method ){
/*
*  Name:
*     EncodeValue

*  Purpose:
*     Encode the current card's keyword value into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int EncodeValue( AstFitsChan *this, char *buf, int col, int digits,
*                      const char *method )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function encodes the keyword value defined in the current card
*     of the supplied FitsChan and stores it at the start of the supplied 
*     buffer. The number of characters placed in the buffer is returned
*     (not including a terminating null).

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     buf
*        The buffer to receive the formatted value. This should be at least
*        70 characters long.
*     col
*        The column number within the FITS header card corresponding to the
*        start of "buf".
*     digits
*        The number of digits to use when formatting floating point
*        values. If the supplied value is negative, the number of digits
*        actually used may be reduced if the string would otherwise extend
*        beyond the number of columns allowed by the FITS standard. If the
*        value is positive, the specified number of digits are always
*        produced, even if it means breaking the FITS standard.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.

*  Returned Value:
*     The number of columns used by the encoded value.

*  Notes:
*     -  The function returns 0 if an error has already occurred
*     or if an error occurs for any reason within this function.
*/

/* Local Variables: */
   char *c;         /* Pointer to next character */
   char *name;      /* Pointer to the keyword name */
   double dval;     /* Keyword value */
   void *data;      /* Pointer to keyword value */
   int i;           /* Loop count */
   int ilen;        /* Length of imaginary part */
   int len;         /* Returned length */
   int quote;       /* Quote character found? */
   int rlen;        /* Length of real part */
   int type;        /* Data type for keyword in current card */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise returned length. */
   len = 0;

/* Return if there is no value associated with the keyword in the current
   card. */
   data = CardData( this, NULL );
   if( data ) {

/* Get the data type and name of the keyword. */
      type = CardType( this );
      name = CardName( this );

/* Go through each supported data type (roughly in the order of
   decreasing usage)... */

/* AST__FLOAT - stored internally in a variable of type "double".  Right justified
   to column 30 in the header card. */
      if( type == AST__FLOAT ){
         dval = *( (double *) data );
   
         len = EncodeFloat( buf, digits, FITSRLCOL - FITSNAMLEN - 2, 
                            FITSCARDLEN - col + 1, dval );
   
         if( len <= 0 ) {
            astError( AST__BDFTS, "%s(%s): Cannot encode floating point value "
                      "%g into a FITS header card for keyword '%s'.", method,
                      astGetClass( this ), dval, name );
         }

/* AST__STRING - stored internally in a null terminated array of type
   "char".  The encoded string is enclosed in single quotes, starting
   at FITS column 11 and ending in at least column 20. Single quotes
   in the string are replaced by two adjacent single quotes. */
      } else if( type == AST__STRING ){
         c = (char *) data;

/* Enter the opening quote. */
         len = 0;
         buf[ len++ ] = '\'';

/* Inspect each character, looking for quotes. */
         for ( i = 0; c[ i ]; ) {
            quote = ( c[ i ] == '\'' );

/* If it will not fit into the header card (allowing for doubled
   quotes), give up here. */
            if ( len + ( quote ? 2 : 1 ) > FITSCARDLEN - col ) break;

/* Otherwise, copy it into the output buffer and double any quotes. */
            buf[ len++ ] = c[ i ];
            if ( quote ) buf[ len++ ] = '\'';

/* Look at the next character. */
            i++;
         }

/* Pad the string out to the required minimum length with blanks and
   add the final quote. */
         while( len < FITSSTCOL - col ) buf[ len++ ] = ' ';
         buf[ len++ ] = '\'';

/* Inspect any characters that weren't used. If any are non-blank,
   report an error. */
         for ( ; c[ i ]; i++ ) {
            if ( !isspace( c[ i ] ) ) {
               astError( AST__BDFTS,
                         "%s(%s): Cannot encode string '%s' into a FITS "
                         "header card for keyword '%s'.", method, astGetClass( this ), 
                         (char *) data, name );
               break;
            }
         }

/* INTEGER - stored internally in a variable of type "int". Right justified
   to column 30 in the header card. */
      } else if( type == AST__INT ){
         len = sprintf(  buf, "%*d", FITSRLCOL - col + 1, 
                         *( (int *) data ) );
         if( len < 0 || len > FITSCARDLEN - col ) {
            astError( AST__BDFTS, "%s(%s): Cannot encode integer value %d into a "
                      "FITS header card for keyword '%s'.", method, astGetClass( this ), 
                      *( (int *) data ), name );
         }

/* LOGICAL - stored internally in a variable of type "int". Represented by
   a "T" or "F" in column 30 of the FITS header card. */
      } else if( type == AST__LOGICAL ){
         for( i = 0; i < FITSRLCOL - col; i++ ) buf[ i ] = ' ';
         if( *( (int *) data ) ){
            buf[ FITSRLCOL - col ] = 'T';
         } else {
            buf[ FITSRLCOL - col ] = 'F';
         }
         len = FITSRLCOL - col + 1;

/* AST__COMPLEXF - stored internally in an array of two "doubles". The real
   part is right justified to FITS column 30. The imaginary part is right
   justified to FITS column 50. */
      } else if( type == AST__COMPLEXF ){
         dval = ( (double *) data )[ 0 ];
   
         rlen = EncodeFloat( buf, digits, FITSRLCOL - FITSNAMLEN - 2, 
                             FITSCARDLEN - col + 1, dval );
         if( rlen <= 0 || rlen > FITSCARDLEN - col ) {
            astError( AST__BDFTS, "%s(%s): Cannot encode real part of a complex "
                      "floating point value [%g,%g] into a FITS header card "
                      "for keyword '%s'.", method, astGetClass( this ), dval, 
                      ( (double *) data )[ 1 ], name );
         } else {
   
            dval = ( (double *) data )[ 1 ];
            ilen = EncodeFloat( buf + rlen, digits, 
                                FITSIMCOL - FITSRLCOL, 
                                FITSCARDLEN - col - rlen, dval );
   
            if( ilen <= 0 ) {
               astError( AST__BDFTS, "%s(%s): Cannot encode imaginary part of a "
                         "complex floating point value [%g,%g] into a FITS header "
                         "card for keyword '%s'.", method, astGetClass( this ),
                         ( (double *) data )[ 0 ], dval, name );
            } else {
               len = ilen + rlen;   
            }
   
         }      

/* AST__COMPLEXI - stored internally in a an array of two "ints". */
      } else if( type == AST__COMPLEXI ){
         rlen = sprintf(  buf, "%*d", FITSRLCOL - col + 1, 
                          ( (int *) data )[ 0 ] );
         if( rlen < 0 || rlen > FITSCARDLEN - col ) {
            astError( AST__BDFTS, "%s(%s): Cannot encode real part of a complex "
                      "integer value [%d,%d] into a FITS header card "
                      "for keyword '%s'.", method, astGetClass( this ), 
                      ( (int *) data )[ 0 ], 
                      ( (int *) data )[ 1 ], name );
         } else {
   
            ilen = sprintf(  buf + rlen, "%*d",  FITSIMCOL - FITSRLCOL + 1,
                             ( (int *) data )[ 1 ] );
            if( ilen < 0 || ilen > FITSCARDLEN - col - rlen ) {
               astError( AST__BDFTS, "%s(%s): Cannot encode imaginary part of a "
                         "complex integer value [%d,%d] into a FITS header card "
                         "for keyword '%s'.", method, astGetClass( this ), 
                         ( (int *) data )[ 0 ],
                         ( (int *) data )[ 1 ], name );
            } else {
               len = ilen + rlen;   
   
            }
   
         }
   
/* Report an internal (ast) programming error if the keyword is of none of the
   above types. */
      } else {
         astError( AST__INTER, "EncodeValue: AST internal programming error - "
                   "FITS %s data-type not yet supported.", 
                   type_names[ type ] );
      }

   }

/* If an error has occurred, return zero length. */
   if( !astOK ) len = 0;

/* Return the answer. */
   return len;
}


static int FindString( int n, const char *list[], const char *test, 
                       const char *text, const char *method, 
                       const char *class ){
/*
*  Name:
*     FindString

*  Purpose:
*     Find a given string within an array of character strings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int FindString( int n, const char *list[], const char *test, 
*                     const char *text, const char *method, const char *class )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function identifies a supplied string within a supplied
*     array of valid strings, and returns the index of the string within
*     the array. The test option may not be abbreviated, but case is
*     insignificant.

*  Parameters:
*     n
*        The number of strings in the array pointed to be "list".
*     list
*        A pointer to an array of legal character strings.
*     test
*        A candidate string.
*     text
*        A string giving a description of the object, parameter,
*        attribute, etc, to which the test value refers.
*        This is only for use in constructing error messages. It should
*        start with a lower case letter.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     The index of the identified string within the supplied array, starting
*     at zero.

*  Notes:
*     -  A value of -1 is returned if an error has already occurred, or
*     if this function should fail for any reason (for instance if the
*     supplied option is not specified in the supplied list). 

*/

/* Local Variables: */
   int ret;                /* The returned index */

/* Check global status. */
   if( !astOK ) return -1;

/* Compare the test string with each element of the supplied list. Leave
   the loop when a match is found. */
   for( ret = 0; ret < n; ret++ ) {
      if( !Ustrcmp( test, list[ ret ] ) ) break;
   }

/* Report an error if the supplied test string does not match any element
   in the supplied list. */
   if( ret >= n ) {
      astError( AST__RDERR, "%s(%s): Illegal value '%s' supplied for %s.",
                method, class, test, text );
      ret = -1;
   }

/* Return the answer. */
   return ret;
}

static int GetCDMatrix( AstFitsChan *this ){
/*
*  Name:
*     GetCDMatrix

*  Purpose:
*     Get the value of the CDMatrix attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetCDMatrix( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     If the CDMatrix attribute has been set, then its value is returned. 
*     Otherwise, an attempt is made to determine the matrix to use by 
*     looking for selected keywords within the FitsChan. If the FitsChan 
*     contains any keywords of the form "PCiiijjj" then a value of zero is 
*     returned. Otherwise, if there are any keywords of the form "CDiiijjj"
*     in the FitsChan then a value of 1 is returned. Otherwise, if there are 
*     any keywords of the form "CDi_j" in the FitsChan then a value of 2 is 
*     returned. If none of these keywords are found, then 2 is returned
*     (CDi_j).

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The CDMatrix value to use.

*  Notes:
*     -  The function returns 2 if an error has already occurred
*     or if an error occurs for any reason within this function.
*/

/* Local Variables... */
   int ret;            /* Returned value */
   int icard;          /* Index of current card on entry */
   int old_skipping;   /* Original value of external variable Skipping */

/* Check the global status. */
   if( !astOK ) return 2;

/* If a value has been supplied for the CDMatrix attribute, use it. */
   if( astTestCDMatrix( this ) ) {
      ret = this->cdmatrix;

/* Otherwise, check for the existence of certain critical keywords... */
   } else {

/* Indicate that cards marked as having been read should not be skipped
   over. */
      old_skipping = Skipping;
      Skipping = 0;

/* Save the current card index, and rewind the FitsChan. */
      icard = astGetCard( this );
      astClearCard( this );

/* If the FitsChan contains any keywords of the form "PCiiijjj", then return
   0. */
      if( astKeyFields( this, "PC%3d%3d", 0, NULL, NULL ) ){
         ret = 0;

/* Otherwise, if the FitsChan contains any "CDiiijjj" keywords, then return 
   1. */
      } else if( astKeyFields( this, "CD%3d%3d", 0, NULL, NULL ) ){
         ret = 1;

/* Otherwise, if the FitsChan contains any "CDi_j" keywords, return 2. */
      } else if( astKeyFields( this, "CD%1d_%1d", 0, NULL, NULL ) ){
         ret = 2;

/* If none of these conditions is met, assume CDi_j form. */
      } else {
         ret = 2;
      }

/* Reinstate the original current card index. */
      astSetCard( this, icard );

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
      Skipping = old_skipping;

   }

/* Return  the matrix type. */
   return astOK ? ret : 2;
}

static int GetEncoding( AstFitsChan *this ){
/*
*  Name:
*     GetEncoding

*  Purpose:
*     Get the value of the Encoding attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetEncoding( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     If the Encoding attribute has been set, then its value is returned. 
*     Otherwise, an attempt is made to determine the encoding scheme by 
*     looking for selected keywords within the FitsChan. If the FitsChan 
*     contains any keywords starting with "BEGAST" then Native encoding is 
*     assumed. Otherwise, if a keyword of the form CDi_j or CDiiijjj is 
*     found in the FitsChan, FITS-IRAF encoding is assumed. Otherwise, if 
*     the CRVAL1 keyword is found in the FitsChan, FITS-WCS encoding is 
*     assumed. Otherwise, if the PLTRAH keyword is found in the FitsChan, 
*     DSS encoding is assumed. If none of these keywords are found, then 
*     Native encoding is assumed.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The encoding scheme identifier.

*  Notes:
*     -  The function returns UNKNOWN_ENCODING if an error has already occurred
*     or if an error occurs for any reason within this function.
*/

/* Local Variables... */
   int ret;            /* Returned value */
   int icard;          /* Index of current card on entry */

/* Check the global status. */
   if( !astOK ) return UNKNOWN_ENCODING;

/* If a value has been supplied for the Encoding attribute, use it. */
   if( astTestEncoding( this ) ) {
      ret = this->encoding;

/* Otherwise, check for the existence of certain critcal keywords... */
   } else {

/* Save the current card index, and rewind the FitsChan. */
      icard = astGetCard( this );
      astClearCard( this );

/* If the FitsChan contains any keywords starting with "BEGAST", then return
   "Native" encoding. */
      if( astKeyFields( this, "BEGAST%2f", 0, NULL, NULL ) ){
         ret = NATIVE_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format "CDi_j"
   or "CDiiijjj", then return "FITS-IRAF" encoding. */
      } else if( astKeyFields( this, "CD%1d_%1d", 0, NULL, NULL ) ||
                 astKeyFields( this, "CD%3d%3d", 0, NULL, NULL ) ){
         ret = FITSIRAF_ENCODING;

/* Otherwise, if the FitsChan contains the "CRVAL1" keywords, then return 
   "FITS-WCS" encoding. */
      } else if( astKeyFields( this, "CRVAL1", 0, NULL, NULL ) ){
         ret = FITSWCS_ENCODING;

/* Otherwise, if the FitsChan contains the "PLTRAH" keywords, use "DSS" 
   encoding. */
      } else if( astKeyFields( this, "PLTRAH", 0, NULL, NULL ) ){
         ret = DSS_ENCODING;

/* If none of these conditions is met, assume Native encoding. */
      } else {
         ret = NATIVE_ENCODING;
      }

/* Reinstate the original current card index. */
      astSetCard( this, icard );

   }

/* Return  the encoding scheme. */
   return astOK ? ret : UNKNOWN_ENCODING;
}

static int KeyFields( AstFitsChan *this, const char *filter, int maxfld, 
                    int *ubnd, int *lbnd ){
/*
*+
*  Name:
*     astKeyFields

*  Purpose:
*     Find the ranges taken by integer fields within the keyword names 
*     in a FitsChan.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astKeyFields( AstFitsChan *this, const char *filter, int maxfld, 
*                       int *ubnd, int *lbnd )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function returns the number of cards within a FitsChan which
*     refer to keywords which match the supplied filter template. If the
*     filter contains any integer field specifiers (e.g. "%d", "%3d", etc),
*     it also returns the upper and lower bounds found for the integer 
*     fields.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     filter
*        The filter string. 
*     maxfld
*        The size of the "ubnd" and "lbnd" arrays.
*     lbnd
*        A pointer to an integer array in which to return the
*        lower bound found for each integer field in the filter.
*        They are stored in the order in which they occur in the filter.
*        If the filter contains too many fields to fit in the supplied
*        array, the excess trailing fields are ignored.
*     ubnd
*        A pointer to an integer array in which to return the
*        upper bound found for each integer field in the filter.

*  Returned Value:
*     astKeyFields()
*        The total number of cards matching the supplied filter in the
*        FitsChan.

*  Filter Syntax:
*     -  The criteria for a keyword name to match a filter template are
*     as follows:
*     -  All characters in the template other than "%" (and the field width
*     and type specifiers which follow a "%") must be matched by an 
*     identical character in the test string. 
      -  If a "%" occurs in the template, then the next character in the 
*     template should be a single digit specifying a field width. If it is 
*     zero, then the test string may contain zero or more matching characters. 
*     Otherwise, the test string must contain exactly the specified number 
*     of matching characters (i.e. 1 to 9). The field width digit may be 
*     omitted, in which case the test string must contain one or more matching 
*     characters. The next character in the template specifies the type of 
*     matching characters and must be one of "d", "c" or "f". Decimal digits 
*     are matched by "d", all upper (but not lower) case alphabetical 
*     characters are matched by "c", and all characters which may legally be 
*     found within a FITS keyword name are matched by "f".

*  Examples:
*     -  The filter "CRVAL1" accepts the single keyword CRVAL1.
*     -  The filter "CRVAL%1d" accepts the single keyword CRVAL0, CRVAL1,
*     CRVAL2, up to CRVAL9.
*     -  The filter "CRVAL%d" accepts any keyword consisting of the string
*     "CRVAL" followed by any integer value.
*     -  The filter "CR%0s1" accepts any keyword starting with the string "CR"
*     and ending with the character "1" (including CR1).

*  Notes:
*     -  The entire FitsChan is searched, irrespective of the setting of
*     the Card attribute.
*     -  If "maxfld" is supplied as zero, "ubnd" and "lbnd" are ignored,
*     but the number of matching cards is still returned as the function value.
*     -  If no matching cards are found in the FitsChan, or if there are no 
*     integer fields in the filter, then the lower and upper bounds are 
*     returned as zero and -1 (i.e. reversed). 
*     -  If an error has already occured, or if this function should fail 
*     for any reason, a value of zero is returned for the function value,
*     and the lower and upper bounds are set to zero and -1.

*-
*/

/* Local Variables: */
   const char *class;     /* Object class */
   const char *method;    /* Method name */
   int *fields;           /* Pointer to array of field values */
   int i;                 /* Field index */
   int icard;             /* Index of current card on entry */
   int nmatch;            /* No. of matching cards */
   int nf;                /* No. of integer fields in the filter */
   int nfld;              /* No. of integer fields in current keyword name */

/* Initialise the returned values. */
   nmatch = 0;
   for( i = 0; i < maxfld; i++ ){
      lbnd[ i ] = 0;
      ubnd[ i ] = -1;
   }
   nf = 0;

/* Check the global error status. */
   if ( !astOK || !filter ) return nf;

/* Store the method name and object class for use in error messages. */
   method = "astKeyFields";
   class = astGetClass( this );

/* Count the number of integer fields in the filter string. */
   nf = CountFields( filter, 'd', method, class );

/* If this is larger than the supplied arrays, use the size of the arrays 
   instead. */
   if( nf > maxfld ) nf = maxfld;

/* Allocate memory to hold the integer field values extracted from 
   each matching keyword. */
   fields = (int *) astMalloc( sizeof( int )*(size_t) nf );

/* Save the current card index, and rewind the FitsChan. */
   icard = astGetCard( this );
   astClearCard( this );

/* Check that the FitsChan is not empty and the pointer can be used. */
   if( !astFitsEof( this ) && astOK ){

/* Initialise the returned bounds. Any excess elements in the array are left
   at the previously initialised values. */
      for( i = 0; i < nf; i++ ){
         lbnd[ i ] = INT_MAX;
         ubnd[ i ] = -INT_MAX;
      }

/* Initialise the number of matching keywords. */
      nmatch = 0;
      
/* Loop round all the cards in the FitsChan. */
      while( !astFitsEof( this ) && astOK ){

/* If the current keyword name matches the filter, update the returned
   bounds and increment the number of matches. */
         if( Match( CardName( this ), filter, nf, fields, &nfld, 
                    method, class ) ){

            for( i = 0; i < nf; i++ ){
               if( fields[ i ] > ubnd[ i ] ) ubnd[ i ] = fields[ i ];
               if( fields[ i ] < lbnd[ i ] ) lbnd[ i ] = fields[ i ];
            }

            nmatch++;
                    
         }
                    
/* Move on to the next card. */
         MoveCard( this, 1, method, class );
         
      }

/* If bounds were not found, returned 0 and -1. */
      for( i = 0; i < nf; i++ ){
         if( lbnd[ i ] == INT_MAX ){
            lbnd[ i ] = 0;
            ubnd[ i ] = -1;
         }
      }

   }

/* Reinstate the original current card index. */
   astSetCard( this, icard );

/* Free the memory used to hold the integer field values extracted from 
   each matching keyword. */
   fields = (int *) astFree( (void *) fields );

/* If an error has occurred, returned no matches and reversed bounds. */
   if( !astOK ){
      nmatch = 0;
      for( i = 0; i < maxfld; i++ ){
         lbnd[ i ] = 0;
         ubnd[ i ] = -1;
      }
   }

/* Returned the answer. */
   return nmatch;

}

static int FindFits( AstFitsChan *this, const char *name, 
                     char card[ FITSCARDLEN + 1 ], int inc ){
/*
*++
*  Name:
c     astFindFits
f     AST_FINDFITS

*  Purpose:
*     Find a FITS card in a FitsChan by keyword.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "fitschan.h"
c     int astFindFits( AstFitsChan *this, const char *name, char card[ 81 ],
c                      int inc )
f     RESULT = AST_FINDFITS( THIS, NAME, CARD, INC, STATUS )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function searches for a card in a FitsChan by keyword. The
*     search commences at the current card (identified by the Card
*     attribute) and ends when a card is found whose FITS keyword
*     matches the template supplied, or when the last card in the
*     FitsChan has been searched.
*
*     If the search is successful (i.e. a card is found which matches
c     the template), the contents of the card are (optionally)
f     the template), the contents of the card are
*     returned and the Card attribute is adjusted to identify the card
*     found or, if required, the one following it. If the search is
c     not successful, the function returns zero and the Card attribute
f     not successful, the function returns .FALSE. and the Card attribute
*     is set to the "end-of-file".

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsChan.
c     name
f     NAME = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing a
f        A character string containing a
*        template for the keyword to be found. In the simplest case,
*        this should simply be the keyword name (the search is case
*        insensitive and trailing spaces are ignored). However, this
*        template may also contain "field specifiers" which are
*        capable of matching a range of characters (see the "Keyword
*        Templates" section for details). In this case, the first card
*        with a keyword which matches the template will be found. To
*        find the next FITS card regardless of its keyword, you should
*        use the template "%f".
c     card
f     CARD = CHARACTER * ( 80 ) (Returned)
c        An array of at least 81 characters (to allow room for a
c        terminating null)
f        A character variable with at least 80 characters
*        in which the FITS card which is found will be returned.  If
c        the search is not successful (or a NULL pointer is given), a
f        the search is not successful, a
*        card will not be returned.
c     inc
f     INC = LOGICAL (Given)
c        If this value is zero (and the search is successful), the
f        If this value is .FALSE. (and the search is successful), the
*        FitsChan's Card attribute will be set to the index of the card
c        that was found. If it is non-zero, however, the Card
f        that was found. If it is .TRUE., however, the Card
*        attribute will be incremented to identify the card which
*        follows the one found.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFindFits()
f     AST_FINDFITS = LOGICAL
c        One if the search was successful, otherwise zero.
f        .TRUE. if the search was successful, otherwise .FALSE..

*  Notes:
*     - The search always starts with the current card, as identified
*     by the Card attribute. To ensure you search the entire contents
*     of a FitsChan, you should first clear the Card attribute (using
c     astClear). This effectively "rewinds" the FitsChan.
f     AST_CLEAR). This effectively "rewinds" the FitsChan.
*     - If a search is unsuccessful, the Card attribute is set to the
*     "end-of-file" (i.e. to one more than the number of cards in the
*     FitsChan). No error occurs.
c     - A value of zero will be returned if this function is invoked
f     - A value of .FALSE. will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.

*  Examples:
c     result = astFindFits( fitschan, "%f", card, 1 );
f     RESULT = AST_FINDFITS( FITSCHAN, '%f', CARD, .TRUE., STATUS )
*        Returns the current card in a FitsChan and advances the Card
*        attribute to identify the card that follows (the "%f"
*        template matches any keyword).
c     result = astFindFits( fitschan, "BITPIX", card, 1 );
f     RESULT = AST_FINDFITS( FITSCHAN, 'BITPIX', CARD, .TRUE., STATUS )
*        Searches a FitsChan for a FITS card with the "BITPIX" keyword
*        and returns that card. The Card attribute is then incremented
*        to identify the card that follows it.
c     result = astFindFits( fitschan, "COMMENT", NULL, 0 );
f     RESULT = AST_FINDFITS( FITSCHAN, 'COMMENT', CARD, .FALSE., STATUS )
*        Sets the Card attribute of a FitsChan to identify the next
c        COMMENT card (if any). The card itself is not returned.
f        COMMENT card (if any) and returns that card.
c     result = astFindFits( fitschan, "CRVAL%1d", card, 1 );
f     RESULT = AST_FINDFITS( FITSCHAN, 'CRVAL%1d', CARD, .TRUE., STATUS )
*        Searches a FitsChan for the next card with a keyword of the
*        form "CRVALi" (for example, any of the keywords "CRVAL1",
*        "CRVAL2" or "CRVAL3" would be matched). The card found (if
*        any) is returned, and the Card attribute is then incremented
*        to identify the following card (ready to search for another
*        keyword with the same form, perhaps).

*  Keyword Templates:
*     The templates used to match FITS keywords are normally composed
*     of literal characters, which must match the keyword exactly
*     (apart from case). However, a template may also contain "field
*     specifiers" which can match a range of possible characters. This
*     allows you to search for keywords that contain (for example)
*     numbers, where the digits comprising the number are not known in
*     advance.
*
*     A field specifier starts with a "%" character. This is followed
*     by an optional single digit (0 to 9) specifying a field
*     width. Finally, there is a single character which specifies the
*     type of character to be matched, as follows:
*
*     - "c": matches all upper case letters,
*     - "d": matches all decimal digits,
*     - "f": matches all characters which are permitted within a FITS
*     keyword (upper case letters, digits, underscores and hyphens).
*
*     If the field width is omitted, the field specifier matches one
*     or more characters. If the field width is zero, it matches zero
*     or more characters. Otherwise, it matches exactly the number of
*     characters specified. In addition to this:
*
*     - The template "%f" will match a blank FITS keyword consisting
*     of 8 spaces (as well as matching all other keywords).
*     - A template consisting of 8 spaces will match a blank keyword
*     (only).
*
*     For example:
*
*     - The template "BitPix" will match the keyword "BITPIX" only.
*     - The template "crpix%1d" will match keywords consisting of
*     "CRPIX" followed by one decimal digit.
*     - The template "P%c" will match any keyword starting with "P"
*     and followed by one or more letters.
*     - The template "E%0f" will match any keyword beginning with "E".
*     - The template "%f" will match any keyword at all (including a
*     blank one).
*--
*/

/* Local Variables: */
   char *c;               /* Pointer to next character to check */
   char *lname;           /* Pointer to copy of name without trailing spaces */
   const char *class;     /* Object class */ 
   const char *method;    /* Calling method */ 
   int ret;               /* Was a card found? */

/* Check the global status, and supplied keyword name. */
   if( !astOK ) return 0;

/* Store the calling method and object class. */ 
   method = "astFindFits";
   class = astGetClass( this ); 

/* Get a local copy of the keyword template. */
   lname = (char *) astStore( NULL, (void *) name, strlen(name) + 1 );

/* Terminate it to exclude trailing spaces. */
   c = lname + strlen(lname) - 1;
   while( *c == ' ' && c >= lname ) *(c--) = 0;

/* Use the private FindKeyCard function to find the card and make it the
   current card. Always use the supplied current card (if any) if the 
   template is "%f" or "%0f". */
   if ( !strcmp( lname, "%f" ) || !strcmp( lname, "%0f" ) ){ 
      ret = astFitsEof( this ) ? 0 : 1;
   } else {
      ret = FindKeyCard( this, lname, method, class );
   }

/* Only proceed if the card was found. */
   if( ret && astOK ){

/* Format the current card if a destination string was supplied. */
      if( card ) FormatCard( this, card, method );

/* Increment the current card pointer if required. */
      if( inc ) MoveCard( this, 1, method, class );

/* Indicate that a card has been formatted. */
      ret = 1;

   }

/* Free the memory holding the local copy of the keyword template. */
   lname = (char *) astFree( (void *) lname );

/* If an errror has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static int FindKeyCard( AstFitsChan *this, const char *name, 
                        const char *method, const char *class ){
/*
*  Name:
*     FindKeyCard

*  Purpose:
*     Find the next card refering to given keyword.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int FindKeyCard( AstFitsChan *this, const char *name, 
*                      const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Finds the next card which refers to the supplied keyword and makes
*     it the current card. The search starts with the current card and ends 
*     when it reaches the last card.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a string holding the keyword template (using the
*        syntax expected by the Match function).
*     method
*        Pointer to string holding name of calling method.

*  Returned Value:
*     A value of 1 is returned if a card was found refering to the given
*     keyword. Otherwise zero is returned.

*  Notes:
*     -  If a NULL pointer is supplied for "name" then the current card
*     is left unchanged.
*     -  The current card is set to NULL (end-of-file) if no card can be
*     found for the supplied keyword.

*/

/* Local Variables: */
   int nfld;             /* Number of fields in keyword template */
   int ret;              /* Was a card found? */

/* Check the global status, and supplied keyword name. */
   if( !astOK || !name ) return 0;

/* Indicate that no card has been found yet. */
   ret = 0;

/* Search forward through the list until all cards have been checked. */
   while( !astFitsEof( this ) && astOK ){

/* Break out of the loop if the keyword name from the current card matches 
   the supplied keyword name. */
      if( Match( CardName( this ), name, 0, NULL, &nfld, method, class ) ){
         ret = 1;
         break;

/* Otherwise, move the current card on to the next card. */
      } else {
         MoveCard( this, 1, method, class );
      }

   }

/* Return. */
   return ret;

}

static const char *FindIraf( AstFitsChan *this ){
/*
*  Name:
*     FindIraf

*  Purpose:
*     Find the last FITS-IRAF keyword card in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     const char *FindIraf( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A search is made through the FitsChan for the last card which
*     relates to a FITS-IRAF keyword. The next card becomes the current 
*     card. Cards marked as having been read are included.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to a static string holding the name of the last IRAF keyword
*     found in the FitsChan. If no such keywords are found, a NULL pointer 
*     is returned.

*  Notes:
*     -  The current card is left unchanged if no FITS-IRAF keyword cards 
*     are found in the FitsChan.

*-
*/

/* Local Variables: */
   const char *class;       /* Pointer to string holding class of object */
   const char *method;      /* Pointer to string holding calling method name */
   const char *keyname;     /* Keyword name from current card */
   const char *ret;         /* Pointer to the last IRAF keyword name */
   int icard;               /* Index of original current card */
   int nfld;                /* Number of fields in keyword template */
   int old_skipping;        /* Original value of external variable Skipping */

/* Check the global status. */
   if( !astOK ) return NULL;

/* Store the method and class strings. */
   method = "astWrite";
   class = astGetClass( this );

/* Initialise */
   ret = NULL;

/* Indicate that cards marked as having been read should not be skipped
   over. */
   old_skipping = Skipping;
   Skipping = 0;

/* Save the current card index. */
   icard = astGetCard( this );

/* Set the FitsChan to end-of-file. */
   astSetCard( this, INT_MAX );

/* Check each card moving backwards from the end to the start, until an
   IRAF keyword is found, or the start of the FitsChan is reached. */
   while( astOK ){   

/* Get the keyword name from the current card. */
      keyname = CardName( this );

/* Save a pointer to the keyword if it is the first non-null, non-comment
   card. */
      if( keyname ) { 

/* If it matches any of the IRAF keywords, move forward one card 
   and break out of the loop. */
         if( Match( keyname, "CRVAL%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CRPIX%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CDELT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CROTA%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CTYPE%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CUNIT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%3d%3d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%1d_%1d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EPOCH", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EQUINOX", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "MJD-OBS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "DATE-OBS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "RADECSYS", 0, NULL, &nfld, method, class ) ){

            ret = keyname;
            MoveCard( this, 1, method, class );
            break;
         }
      }

/* Restore the original current card and break out of the loop if we are 
   now at the start of the FitsChan, otherwise step backwards by one card
   to check the previous card. */
      if( astGetCard( this ) <= 1 ) {
         astSetCard( this, icard );
         break;
      } else {
         MoveCard( this, -1, method, class );
      }

   }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
   Skipping = old_skipping;

/* Return NULL if an error has occurred. */
   if( !astOK ) ret = NULL;

/* Return the answer. */
   return ret;
}

static const char *FindWcs( AstFitsChan *this ){
/*
*  Name:
*     FindWcs

*  Purpose:
*     Find the last FITS-WCS keyword card in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     const char *FindWcs( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A search is made through the FitsChan for the last card which
*     relates to a FITS-WCS keyword. The next card becomes the current 
*     card. Cards marked as having been read are included.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to a static string holding the name of the last WCS keyword
*     found in the FitsChan. If no such keywords are found, a NULL pointer 
*     is returned.

*  Notes:
*     -  The current card is left unchanged if no FITS-WCS keyword cards 
*     are found in the FitsChan.

*-
*/

/* Local Variables: */
   const char *class;       /* Pointer to string holding class of object */
   const char *method;      /* Pointer to string holding calling method name */
   const char *keyname;     /* Keyword name from current card */
   const char *ret;         /* Pointer to the last WCS keyword name */
   int icard;               /* Index of original current card */
   int nfld;                /* Number of fields in keyword template */
   int old_skipping;        /* Original value of external variable Skipping */

/* Check the global status. */
   if( !astOK ) return NULL;

/* Store the method and class strings. */
   method = "astWrite";
   class = astGetClass( this );

/* Initialise */
   ret = NULL;

/* Indicate that cards marked as having been read should not be skipped
   over. */
   old_skipping = Skipping;
   Skipping = 0;

/* Save the current card index. */
   icard = astGetCard( this );

/* Set the FitsChan to end-of-file. */
   astSetCard( this, INT_MAX );

/* Check each card moving backwards from the end to the start, until a WCS 
   keyword is found, or the start of the FitsChan is reached. */
   while( astOK ){   

/* Get the keyword name from the current card. */
      keyname = CardName( this );

/* Save a pointer to the keyword if it is the first non-null, non-comment
   card. */
      if( keyname ) { 

/* If it matches any of the WCS keywords, move forward one card 
   and break out of the loop. */
         if( Match( keyname, "CRVAL%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CRPIX%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CDELT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CROTA%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CTYPE%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CUNIT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PC%3d%3d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%3d%3d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%1d_%1d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "LONGPOLE", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "LATPOLE", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PROJP%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EPOCH", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EQUINOX", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "MJD-OBS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "DATE-OBS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "RADECSYS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dVAL%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dPIX%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dELT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dYPE%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dNIT%d", 0, NULL, &nfld, method, class ) ){

            ret = keyname;
            MoveCard( this, 1, method, class );
            break;
         }
      }

/* Restore the original current card and break out of the loop if we are 
   now at the start of the FitsChan, otherwise step backwards by one card
   to check the previous card. */
      if( astGetCard( this ) <= 1 ) {
         astSetCard( this, icard );
         break;
      } else {
         MoveCard( this, -1, method, class );
      }

   }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
   Skipping = old_skipping;

/* Return NULL if an error has occurred. */
   if( !astOK ) ret = NULL;

/* Return the answer. */
   return ret;
}

static int FitsEof( AstFitsChan *this ){
/*
*+
*  Name:
*     astFitsEof

*  Purpose:
*     See if the FitsChan is at "end-of-file".

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astFitsEof( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     A value of zero is returned if any more cards remain to be read from the
*     FitsChan. Otherwise a value of 1 is returned. Thus, it is
*     equivalent to testing the FitsChan for an "end-of-file" condition.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     One if no more cards remain to be read, otherwise zero.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*-
*/

/* Check the supplied object. */
   if( !this ) return 1;

/* If no more cards remain to be read, the current card pointer in the
   FitsChan will be NULL. Return an appropriate integer value. */
   return  this->card ? 0 : 1;

}

/*
*+
*  Name:
*     astFitsGet<X>

*  Purpose:
*     Get a named keyword value from a FitsChan.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astFitsGet<X>( AstFitsChan *this, const char *name, <X>type *value )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This is a family of functions which gets a value for a named keyword 
*     from a FitsChan using one of several different data types. The data 
*     type of the returned value is selected by replacing <X> in the function 
*     name by one of the following strings representing the recognised FITS 
*     data types:
*
*     CF - Complex floating point values.
*     CI - Complex integer values.
*     F  - Floating point values.
*     I  - Integer values.
*     L  - Logical (i.e. boolean) values.
*     S  - String values.
*
*     The "value" parameter should be a pointer with a data type depending on 
*     <X> as follows:
*
*     CF - "double *" (the pointer should point to a 2 element array to
*          receive the real and imaginary parts of the complex value).
*     CI - "int *" (the pointer should point to a 2 element array to
*          receive the real and imaginary parts of the complex value).
*     F  - "double *".
*     I  - "int *".
*     L  - "int *".
*     S  - "char **" (a pointer to a static "char" array is returned at the
*          location given by the "value" parameter, Note, the stored string
*          may change on subsequent invocations of astFitsGetS so a
*          permanent copy should be taken of the string if necessary). 

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        A pointer to a string holding the keyword name. This may be a 
*        complete FITS header card, in which case the keyword to use is 
*        extracted from it. No more than 80 characters are read from this 
*        string.
*     value
*        A pointer to a location at which to return the keyword value.
*        the data type of this parameter depends on <X> as described above.
*        The supplied value is left unchanged if the keyword is not found.

*  Returned Value:
*     astFitsGet<X>()
*        A value of zero is returned if the keyword was not found
*        (no error is reported). 
*        Otherwise, a value of one is returned. 

*  Notes:
*     -  The card following the current card is checked first. If this is
*     not the required card, then the rest of the FitsChan is searched,
*     starting with the first card added to the FitsChan. Therefore cards
*     should be accessed in the order they are stored in the FitsChan (if
*     possible) as this will minimise the time spent searching for cards. 
*     -  If the requested card is found, it becomes the current card, 
*     otherwise the current card is left pointing at the "end-of-file".
*     -  If the stored keyword value is not of the requested type, it is
*     converted into the requested type.
*     -  An error will be reported if the keyword name does not conform
*     to FITS requirements.
*     -  Zero is returned as the function value 
*     if an error has already occurred, or if this function should fail for 
*     any reason.
*-
*/

/* Define a macro which expands to the implementation of the astFitsGet<X>
   routine for a given data type. */

#define MAKE_FGET(code,ctype,ftype) \
static int FitsGet##code( AstFitsChan *this, const char *name, ctype value ){ \
\
/* Local Variables: */ \
   const char *class;     /* Object class */ \
   const char *method;    /* Calling method */ \
   char *lcom;            /* Supplied keyword comment */ \
   char *lname;           /* Supplied keyword name */ \
   char *lvalue;          /* Supplied keyword value */ \
   int ret;               /* The returned value */ \
   size_t sz;                /* Data size */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Store the calling method and object class. */ \
   method = "astFitsGet"#code; \
   class = astGetClass( this ); \
\
/* Initialise the returned value. */ \
   ret = 0; \
\
/* Extract the keyword name from the supplied string. */ \
   (void) astSplit( name, &lname, &lvalue, &lcom, method, class ); \
\
/* Attempt to find a card in the FitsChan refering to this keyword, \
   and make it the current card. Only proceed if a card was found. */ \
   if( SearchCard( this, lname, method, class ) ){ \
\
/* Convert the stored data value to the requested type, and store it in \
   the supplied buffer. */ \
      if( !CnvValue( this, ftype, value, method ) && astOK ) { \
         astError( AST__FTCNV, "%s(%s): Cannot convert FITS keyword " \
                   "'%s' (value '%s') to %s.", method, class, \
                   lname, CardData( this, &sz ), type_names[ ftype ] ); \
      } \
\
/* Indicate that a value is available. */ \
      if( astOK ) ret = 1; \
\
   } \
\
/* Release the memory used to hold keyword name, value and comment strings. */ \
   lname = (char *) astFree( (void *) lname ); \
   lvalue = (char *) astFree( (void *) lvalue ); \
   lcom = (char *) astFree( (void *) lcom ); \
\
/* Return the answer. */ \
   return ret; \
\
}

/* Use the above macro to give defintions for the astFitsGet<X> method
   for each FITS data type. */

MAKE_FGET(CF,double *,AST__COMPLEXF)
MAKE_FGET(CI,int *,AST__COMPLEXI)
MAKE_FGET(F,double *,AST__FLOAT)
MAKE_FGET(I,int *,AST__INT)
MAKE_FGET(L,int *,AST__LOGICAL)
MAKE_FGET(S,char **,AST__STRING)

#undef MAKE_FGET

static int FitsGetCom( AstFitsChan *this, const char *name,  
                       char **comment ){
/*
*+
*  Name:
*     astFitsGetCom

*  Purpose:
*     Get a keyword comment from a FitsChan.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astFitsGetCom( AstFitsChan *this, const char *name, 
*                        char **comment )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function gets the comment associated with the next occurrence of 
*     a named keyword in a FitsChan. 

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        A pointer to a 
*        string holding the keyword name. This may be a complete FITS
*        header card, in which case the keyword to use is extracted from 
*        it. No more than 80 characters are read from this string.
*     comment
*        A pointer to a location at which to return a pointer to a string
*        holding the keyword comment. Note, the stored string will change on 
*        subsequent invocations of astFitsGetCom so a permanent copy 
*        should be taken of the string if necessary. 

*  Returned Value:
*     astFitsGetCom()
*        A value of zero is returned if the keyword was not found before
*        the end of the FitsChan was reached (no error is reported). 
*        Otherwise, a value of one is returned. 

*  Notes:
*     -  If a NULL pointer is supplied for "name" then the comment from
*     the current card is returned.
*     -  The returned value is obtained from the next card refering to 
*     the required keyword, starting the search with the current card.
*     Any cards occuring before the current card are not seached. If
*     the entire contents of the FitsChan must be searched, then ensure
*     the current card is the first card in the FitsChan by clearing the Card
*     attribute. This effectively "rewinds" the FitsChan.
*     -  The current card is updated to become the card following the one 
*     read by this function. If the card read by this function is the
*     last one in the FitsChan, then the current card is left pointing at the
*     "end-of-file".
*     -  An error will be reported if the keyword name does not conform
*     to FITS requirements.
*     -  A NULL pointer is returned for the comment string if the keyword
*     has no comment.
*     -  Zero is returned as the function value if an error has already 
*     occurred, or if this function should fail for any reason.
*-
*/

/* Local Variables: */ 
   const char *method;    /* Calling method */
   const char *class;     /* Object class */
   char *lcom;            /* Supplied keyword comment */ 
   char *lname;           /* Supplied keyword name */ 
   char *lvalue;          /* Supplied keyword value */ 
   int ret;               /* The returned value */ 
   static char sval[ FITSCARDLEN + 1 ]; /* Static text buffer */ 

/* Check the global error status. */ 
   if ( !astOK ) return 0; 

/* Initialise the returned value. */ 
   ret = 0; 

/* Store the method name and object class. */
   method = "astFitsGetCom";
   class = astGetClass( this );

/* Extract the keyword name from the supplied string (if supplied). */ 
   if( name ){
      (void) astSplit( name, &lname, &lvalue, &lcom, method, class );
   } else {
      lname = NULL;
      lcom = NULL;
      lvalue = NULL;
   }

/* Find the next card in the FitsChan refering to this keyword. This will
   be the current card if no keyword name was supplied. The matching card
   is made the current card. Only proceed if a card was found. */ 
   if( FindKeyCard( this, lname, method, class ) ){ 

/* Copy the comment into a static buffer, and return a pointer to it. */ 
      if( CardComm( this ) ){
         (void) strncpy( sval, CardComm( this ), FITSCARDLEN ); 
         sval[ FITSCARDLEN ] = 0; 
         if( comment ) *comment = sval;
      } else {
         if( comment ) *comment = NULL;
      }        

/* Move on to the next card. */
      MoveCard( this, 1, method, class );

/* Indicate that a value is available. */ 
      if( astOK ) ret = 1; 

   } 

/* Release the memory used to hold keyword name, value and comment strings. */ 
   lname = (char *) astFree( (void *) lname ); 
   lvalue = (char *) astFree( (void *) lvalue ); 
   lcom = (char *) astFree( (void *) lcom ); 

/* Return the answer. */ 
   return ret; 

}

static int FitsSet( AstFitsChan *this, const char *keyname, void *value, 
                    int type, const char *comment, int overwrite ){
/*
*  Name:
*     FitsSet

*  Purpose:
*     Store a keyword value of any type in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int FitsSet( AstFitsChan *this, const char *keyname, void *value, 
*                  int type, const char *comment, int overwrite )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function stores the supplied value for the supplied keyword 
*     in the supplied FitsChan, assuming it is of the supplied data type.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        A pointer to a string holding the keyword name. 
*     value
*        A pointer to a buffer holding the keyword value. For strings,
*        the buffer should hold the address of a pointer to the character 
*        string.
*     type 
*        The keyword type.
*     comment
*        A pointer to a string holding a comment to associated with the 
*        keyword. If a NULL pointer or a blank string is supplied, then
*        any comment included in the string supplied for the "name" parameter 
*        is used instead. If "name" contains no comment, then any existing 
*        comment in the card being over-written is retained, or a NULL
*        pointer is stored if a new card is being inserted.
*     overwrite
*        If non-zero, the new card formed from the supplied keyword name,
*        value and comment string over-writes the current card, and the 
*        current card is incremented to refer to the next card. If zero, the 
*        new card is inserted in front of the current card and the current 
*        card is left unchanged. In either case, if the current card on 
*        entry points to the "end-of-file", the new card is appended to the 
*        end of the list. 

*  Returned Value:
*     A value of 0 is returned if the value could not be stored for any
*     reason. A value of 1 is returned otherwise.

*  Notes:
*     -  Nothing is stored in the FitsChan and a value of zero is returned 
*     (but no error is reported) if an AST__FLOAT value is supplied equal 
*     to AST__BAD.

*/

/* Local Variables: */
   const char *cval; 
   double dval;
   int ival;
   int ret;           /* Returned value */

/* Check the global status, and the supplied pointer. */
   if( !astOK || !value ) return 0;

/* Initialise the returned value to indicate that the supplied name was
   stored. */
   ret = 1;

/* Check each data type in turn. */
   if( type == AST__FLOAT ){
      dval = *( (double *) value );
      if( dval != AST__BAD ) {
         astFitsSetF( this, keyname, dval, comment, overwrite );
      } else {
         ret = 0;
      }

   } else if( type == AST__STRING ){
      cval = *( (char **) value);
      if( cval ){      
         astFitsSetS( this, keyname, cval, comment, overwrite );
      } else {
         ret = 0;
      }

   } else if( type == AST__COMMENT ){
      astFitsSetCom( this, keyname, comment, overwrite );

   } else if( type == AST__INT ){
      ival = *( (int *) value );
      astFitsSetI( this, keyname, ival, comment, overwrite );

   } else if( type == AST__COMPLEXF ){
      if( ( (double *) value )[0] != AST__BAD &&
          ( (double *) value )[1] != AST__BAD ) {
         astFitsSetCF( this, keyname, (double *) value, comment, overwrite );
      } else {
         ret = 0;
      }

   } else if( type == AST__COMPLEXI ){
      astFitsSetCI( this, keyname, (int *) value, comment, overwrite );

   } else if( type == AST__LOGICAL ){
      ival = ( *( (int *) value ) != 0 );
      astFitsSetL( this, keyname, ival, comment, overwrite );

   } 

   return ret;
}

/*
*+
*  Name:
*     astFitsSet<X>

*  Purpose:
*     Store a keyword value in a FitsChan.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     void astFitsSet<X>( AstFitsChan *this, const char *name, <X>type value, 
*                         const char *comment, int overwrite )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This is a family of functions which store values for named keywords
*     within a FitsChan at the current card position. The supplied keyword 
*     value can either over-write an existing keyword value, or can be 
*     inserted as a new header card into the FitsChan.
*
*     The keyword data type is selected by replacing <X> in the function name 
*     by one of the following strings representing the recognised FITS data 
*     types:
*
*     CF - Complex floating point values.
*     CI - Complex integer values.
*     F  - Floating point values.
*     I  - Integer values.
*     L  - Logical (i.e. boolean) values.
*     S  - String values.
*
*     The data type of the "value" parameter depends on <X> as follows:
*
*     CF - "double *" (a pointer to a 2 element array holding the real and
*          imaginary parts of the complex value).
*     CI - "int *" (a pointer to a 2 element array holding the real and
*          imaginary parts of the complex value).
*     F  - "double".
*     I  - "int".
*     L  - "int".
*     S  - "const char *".

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        A pointer to a 
*        string holding the keyword name. This may be a complete FITS
*        header card, in which case the keyword to use is extracted from 
*        it. No more than 80 characters are read from this string.
*     value
*        The keyword value to store with the named keyword. The data type
*        of this parameter depends on <X> as described above.
*     comment
*        A pointer to a string holding a comment to associated with the 
*        keyword. If a NULL pointer or a blank string is supplied, then
*        any comment included in the string supplied for the "name" parameter 
*        is used instead. If "name" contains no comment, then any existing 
*        comment in the card being over-written is retained, or a NULL
*        pointer is stored if a new card is being inserted.
*     overwrite
*        If non-zero, the new card formed from the supplied keyword name,
*        value and comment string over-writes the current card, and the 
*        current card is incremented to refer to the next card. If zero, the 
*        new card is inserted in front of the current card and the current 
*        card is left unchanged. In either case, if the current card on 
*        entry points to the "end-of-file", the new card is appended to the 
*        end of the list. 

*  Notes:
*     -  To assign a new value for an existing keyword within a FitsChan,
*     first find the card describing the keyword using astFindFits, and
*     then use one of the astFitsSet<X> family to over-write the old value.
*     -  If, on exit, there are no cards following the card written by
*     this function, then the current card is left pointing at the 
*     "end-of-file".
*     -  An error will be reported if the keyword name does not conform
*     to FITS requirements.
*-
*/

/* Define a macro which expands to the implementation of the astFitsSet<X>
   routine for a given data type. */

#define MAKE_FSET(code,ctype,ftype,valexp) \
static void FitsSet##code( AstFitsChan *this, const char *name, ctype value, const char *comment, int overwrite ) { \
\
/* Local variables: */ \
   const char *class;     /* Object class */ \
   const char *method;    /* Calling method */ \
   const char *com;       /* Comment to use */ \
   char *lcom;            /* Supplied keyword comment */ \
   char *lname;           /* Supplied keyword name */ \
   char *lvalue;          /* Supplied keyword value */ \
   int free_com;          /* Should com be freed before returned? */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Store the object clas and calling method. */ \
   class = astGetClass( this ); \
   method = "astFitsSet"#code; \
\
/* Extract the keyword name from the supplied string. */ \
   (void) astSplit( name, &lname, &lvalue, &lcom, method, class ); \
\
/* Initialise a pointer to the comment to be stored. If the supplied \
   comment is blank, use the comment given with "name". */ \
   com = ChrLen( comment ) ? comment : lcom; \
\
/* If the comment is still blank, use the existing comment if we are \
   over-writing, or a NULL pointer otherwise. */ \
   free_com = 0; \
   if( !ChrLen( com ) ) { \
      com = NULL; \
      if( overwrite ) { \
         if( CardComm( this ) ){ \
            com = (const char *) astStore( NULL, (void *) CardComm( this), \
                                           strlen( CardComm( this ) ) + 1 ); \
            free_com = 1; \
         } \
      } \
   } \
\
/* Insert the new card. */ \
   InsCard( this, overwrite, lname, ftype, valexp, com, method, class ); \
\
/* Release the memory used to hold keyword name, value and comment strings. */ \
   lname = (char *) astFree( (void *) lname ); \
   lvalue = (char *) astFree( (void *) lvalue ); \
   lcom = (char *) astFree( (void *) lcom ); \
\
/* Release the memory holding the stored comment string, so long as it was \
   allocated within this function. */ \
   if( free_com ) com = (const char *) astFree( (void *) com ); \
\
}

/* Use the above macro to give defintions for the astFitsSet<X> method
   for each FITS data type. */

MAKE_FSET(I,int,AST__INT,(void *)&value)
MAKE_FSET(F,double,AST__FLOAT,(void *)&value)
MAKE_FSET(S,const char *,AST__STRING,(void *)value)
MAKE_FSET(CF,double *,AST__COMPLEXF,(void *)value)
MAKE_FSET(CI,int *,AST__COMPLEXI,(void *)value)
MAKE_FSET(L,int,AST__LOGICAL,(void *)&value)

#undef MAKE_FSET


static void FitsSetCom( AstFitsChan *this, const char *name, 
                        const char *comment, int overwrite ){
/*
*+
*  Name:
*     astFitsSetCom

*  Purpose:
*     Store a comment for a keyword in a FitsChan.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     void astFitsSetCom( AstFitsChan *this, const char *name, 
*                         const char *comment, int overwrite ) 

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function replaces the comment within an existing card, or 
*     stores a new comment card within a FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        A pointer to a 
*        string holding the keyword name. This may be a complete FITS
*        header card, in which case the keyword to use is extracted from 
*        it. No more than 80 characters are read from this string.
*     comment
*        A pointer to a 
*        string holding a comment to associated with the keyword.
*        If a NULL or
*        blank string is supplied, any existing comment associated with
*        the keyword is removed.
*     overwrite
*        If non-zero, the new comment replaces the comment in the current 
*        card, and the current card is then incremented to refer to the next 
*        card. If zero, a new comment card is inserted in front of the current 
*        card and the current card is left unchanged. In either case, if the 
*        current card on entry points to the "end-of-file", the new card is 
*        appended to the end of the list. 

*  Notes:
*     -  When replacing an existing comment, any existing keyword value is 
*     retained only if the supplied keyword name is the same as the keyword 
*     name in the current card. If the keyword names are different, then
*     the new name replaces the old name, and any existing keyword data value 
*     is deleted. The card thus becomes a comment card with the supplied 
*     keyword name and comment, but no data value.
*     -  If, on exit, there are no cards following the card written by
*     this function, then the current card is left pointing at the 
*     "end-of-file".
*     -  The current card can be set explicitly before calling this function
*     either by assigning a value to the Card attribute (if the index of the 
*     required card is already known), or using astFindFits (if only the 
*     keyword name is known).
*     -  An error will be reported if the keyword name does not conform
*     to FITS requirements.
*-
*/

/* Local variables: */ 
   const char *class;     /* Pointer to object class string */
   const char *method;    /* Pointer to calling method string */
   const char *cname;     /* The existing keyword name */ 
   const char *com;       /* The comment to use */ 
   char *lcom;            /* Supplied keyword comment */ 
   char *lname;           /* Supplied keyword name */ 
   char *lvalue;          /* Supplied keyword value */ 
   void *old_data;        /* Pointer to the old data value */
   void *data;            /* Pointer to data value to be stored */
   size_t size;           /* The size of the data value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the calling method and object class. */
   method = "astFitsSetCom";
   class = astGetClass( this );

/* Extract the keyword name, etc, from the supplied string. */ 
   (void) astSplit( name, &lname, &lvalue, &lcom, method, class );

/* If a blank comment has been supplied, use NULL instead. */
   com = ChrLen( comment )? comment : NULL;

/* If we are inserting a new card, or over-writing an old card with a
   different name, create and store a comment card with the given keyword
   name and comment, but no data value. */
   cname = CardName( this );
   if( !overwrite || !cname || strcmp( lname, cname ) ){
      InsCard( this, overwrite, lname, AST__COMMENT, NULL, com, method, class );

/* If we are overwriting an existing keyword comment, use the data type
   and value from the existing current card. Note, we have to take a copy
   of the old data value because InsCard over-writes by deleting the old 
   card and then inserting a new one. */
   } else {
      old_data = CardData( this, &size );
      data = astStore( NULL, old_data, size );
      InsCard( this, 1, lname, CardType( this ), data, com, method, class );
      data = astFree( data );
   }

/* Release the memory used to hold keyword name, value and comment strings. */
   lname = (char *) astFree( (void *) lname );
   lvalue = (char *) astFree( (void *) lvalue );
   lcom = (char *) astFree( (void *) lcom );

}

static void FormatCard( AstFitsChan *this, char *buf, const char *method ){
/*
*
*  Name:
*     FormatCard

*  Purpose:
*     Formats the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FormatCard( AstFitsChan *this, char *buf, const char *method )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function write the current card into the supplied character 
*     buffer as a complete FITS header card.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     buf
*        A character string into which the header card is written. This 
*        should be at least 81 characters long. The returned string is 
*        padded with spaces upto column 80. A terminating null character 
*        is added. 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.

*  Notes:
*     -  An error is reported if the requested header card does not conform to
*     FITS standards.
*
*/

/* Local Variables: */
   const char *com;            /* Pointer to comment string to use */
   int comlen;                 /* Length of comment string */
   int comstart;               /* Column in which to start comment */
   int i;                      /* Loop counter for characters */
   int len;                    /* Output string length */
   int digits;                 /* No. of digits to use when formatting floating point values */

/* Check the global error status, and check the current card is defined. */
   if ( !astOK || astFitsEof( this ) ) return; 

/* Get a pointer to the comment to use and determine its length. */
   com = CardComm( this );
   comlen = ChrLen( com );

/* Copy the keyword name to the start of the output buffer, and store
   its length. */
   len = (int) strlen( strcpy( buf, CardName( this ) ) );

/* Pad the name with spaces up to column 8. */
   while ( len < FITSNAMLEN ) buf[ len++ ] = ' ';

/* If the card contains a keyword value... */
   if( CardType( this ) != AST__COMMENT ){

/* Get the number of digits to use when formatting floating point values. */
      digits = astGetFitsDigits( this );

/* Put an equals sign in column 9, followed by a space in column 10. */
      buf[ len++ ] = '=';
      buf[ len++ ] = ' ';

/* Format and store the keyword value, starting at column 11 and update the
   output string length. */
      len += EncodeValue( this, buf + len, FITSNAMLEN + 3, digits, 
                          method );

/* If there is a comment, determine which column it should start in so that
   it ends in column 80. */
      if( com ){
         comstart = FITSCARDLEN - ( comlen - 2 ) + 1;

/* Adjust the starting column to 32 if possible, avoiding over-writing
   the value, or running off the end of the card unless this is
   unavoidable. */
         if ( comstart > FITSCOMCOL ) comstart = FITSCOMCOL;
         if ( comstart < len + 1 ) comstart = len + 1;

/* Pad the output buffer with spaces up to the start of the comment. */
         while ( len < comstart - 1 ) buf[ len++ ] = ' ';

/* Then append "/ " to introduce the comment, truncating if the card
   length forces this. */
         for ( i = 0; ( i < 2 ) && ( len < FITSCARDLEN ); i++ ) {
            buf[ len++ ] = "/ "[ i ];
         }
      }
   }

/* Append any comment, truncating it if the card length forces
   this. */
   if ( com ) {
      for ( i = 0; com[ i ] && ( len < FITSCARDLEN ); i++ ) {
         buf[ len++ ] = com[ i ];
      }
   }

/* Pad with spaces up to the end of the card. */      
   while ( len < FITSCARDLEN ) buf[ len++ ] = ' ';

/* Terminate it. */
   buf[ FITSCARDLEN ] = 0;

}

static int FullForm( const char *list, const char *test ){
/*
*  Name:
*     FullForm

*  Purpose:
*     Identify the full form of an option string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int FullForm( const char *list, const char *test )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function identifies a supplied test option within a supplied
*     list of valid options, and returns the index of the option within
*     the list. The test option may be abbreviated, and case is
*     insignificant.

*  Parameters:
*     list
*        A list of space separated option strings.
*     test
*        A candidate option string.

*  Returned Value:
*     The index of the identified option within the supplied list, starting
*     at zero. -1 is returned if the option is not recognised, and -2 if
*     the option is ambiguous (no errors are reported in these cases).

*  Notes:
*     -  A value of -1 is returned if an error has already occurred, or
*     if this function should fail for any reason.

*/

/* Local Variables: */
   char *llist;            /* Pointer to a local copy of the options list */
   char *option;           /* Pointer to the start of the next option */
   int i;                  /* Current option index */
   int len;                /* Length of supplied option */
   int nmatch;             /* Number of matching options */
   int ret;                /* The returned index */

/* Initialise the answer to indicate that the option has not been
   identified. */
   ret = -1;

/* Check global status. */
   if( !astOK ) return ret;

/* Take a local copy of the supplied options list. This is necessary since
   "strtok" modified the string by inserting null characters. */
   llist = (char *) astStore( NULL, (void *) list, strlen(list) + 1 );
   if( astOK ){

/* Save the number of characters in the supplied test option (excluding
   trailing spaces). */
      len = ChrLen( test );

/* Compare the supplied test option against each of the known options in 
   turn. Count the number of matches. */
      nmatch = 0;
      option = strtok( llist, " " );
      i = 0;
      while( option ){
      
/* If every character in the supplied label matches the corresponding
   character in the current test label we have a match. Increment the 
   number of matches and save the current item index. */
         if( !Ustrncmp( test, option, len ) ) {
            nmatch++;
            ret = i;
         }

/* Get a pointer to the next option. */
         option = strtok( NULL, " " );
         i++;
      }

/* Return -1 if no match was found. */
      if( !nmatch ){
         ret = -1;

/* Return -2 if the option was ambiguous. */
      } else if( nmatch > 1 ){
         ret = -2;
      }

/* Free the local copy of the options list. */
      llist = (char *) astFree( (void *) llist );
   }

/* Return the answer. */
   return ret;
}

const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FitsChan member function (over-rides the protected astGetAttrib
*     method inherited from the Channel class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a FitsChan, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the FitsChan, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the FitsChan. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   const char *result;           /* Pointer value to return */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of attrib string */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Card. */
/* ----- */
   if ( !strcmp( attrib, "card" ) ) {
      ival = astGetCard( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* Encoding. */
/* --------- */
   } else if ( !strcmp( attrib, "encoding" ) ) {
      ival = astGetEncoding( this );
      if ( astOK ) {
         if( ival == NATIVE_ENCODING ){
            result = NATIVE_STRING;

         } else if( ival == FITSWCS_ENCODING ){
            result = FITSWCS_STRING;

         } else if( ival == FITSIRAF_ENCODING ){
            result = FITSIRAF_STRING;

         } else if( ival == DSS_ENCODING ){
            result = DSS_STRING;

         } else {
            result = UNKNOWN_STRING;
         }
      }

/* FitsDigits. */
/* ----------- */
   } else if ( !strcmp( attrib, "fitsdigits" ) ) {
      ival = astGetFitsDigits( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

#if 0
/* CDMatrix. */
/* --------- */
   } else if ( !strcmp( attrib, "cdmatrix" ) ) {
      ival = astGetCDMatrix( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }
#endif

/* Ncard. */
/* ------ */
   } else if ( !strcmp( attrib, "ncard" ) ) {
      ival = astGetNcard( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib );
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static int GetCard( AstFitsChan *this ){
/*
*+
*  Name:
*     astGetCard

*  Purpose:
*     Get the value of the Card attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astGetCard( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function returns the value of the Card attribute for the supplied 
*     FitsChan. This is the index of the next card to be read from the
*     FitsChan. The index of the first card is 1. If there are no more
*     cards to be read, a value one greater than the number of cards in the
*     FitsChan is returned. 

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The index of the next card to be read.

*  Notes:
*     - A value of zero will be returned if the current card is not defined.
*     - This function attempts to execute even if an error has occurred.

*-
*/

/* Local Variables: */
   const char *class;      /* Pointer to class string */
   const char *method;     /* Pointer to method string */
   FitsCard *card0;        /* Pointer to current FitsCard */
   int index;              /* Index of next FitsCard */

/* Return if no FitsChan was supplied, or if the FitsChan is empty. */
   if ( !this || !this->head ) return 0;

/* Store the method and object class. */
   method = "astGetCard";
   class = astGetClass( this );

/* Save a pointer to the current card, and the reset the current card to
   be the first card. */
   card0 = this->card;
   astClearCard( this );

/* Count through the list of FitsCards in the FitsChan until the original
   current card is reached. */
   index = 1;
   while( this->card != card0 ){

/* Increment the card count and move on to the next card. */
      index++;
      MoveCard( this, 1, method, class );

   }

/* Return the card index. */
   return index;

}

static int GetDesc( AstFitsChan *this, int naxis, int *desc, int *mxdesc, 
                    int *prim ){
/*
*  Name:
*     GetDesc

*  Purpose:
*     Get the next set of axis descriptions indices for a WCS-encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetDesc( AstFitsChan *this, int naxis, int *desc, int *mxdesc, 
*                  int *prim )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function steps through all the possible combinations of axis
*     descriptions on repeated invocations. On the first call, the maximum
*     description index for each axis is found and stored in "mxdesc". This
*     is determined by checking the existence of FITS keywords CmVALi, CmPIXi, 
*     CmELTi, CmNITi and CmYPEi in the supplied FitsChan, where "m" is the 
*     description index and "i" is the axis index. The first call sets
*     every element of "desc" to 1, indicating that the primary axis
*     descriptions should be used for all axis. On subsequent
*     invocations, the values in "desc" are incremented until all possible
*     combinations have been returned. When this has been done, a
*     function value of zero is returned.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     naxis
*        the number of axes.
*     desc
*        Pointer to an array of "naxis" values in which to store the
*        index of the description currently being used for each axis. 
*        The contents of this array should not be changed by the calling
*        function. Element zero of this array should be set to -1 prior
*        to calling this function for the first time.
*     mxdesc
*        Pointer to an array of "naxis" values in which to store the
*        maximum description index available for each axis. The contents of 
*        this array should not be changed by the calling function.
*     prim
*        Pointer to a flag indicating if the primary axis descriptions
*        have been returned.

*  Returned Value:
*     Zero is returned if all possible combinations have already been
*     done. Otherwise one is returned.

*  Notes:
*     -  No checks are made to make sure that all the keywords for a
*     particular axis description exist.

*/

/* Local Variables: */
   char key[ FITSNAMLEN + 10 ];  /* Keyword name */
   int *mx;                      /* Pointer to next upper description limit */
   int axis;                     /* Axis index */
   int hi;                       /* Highest field value */
   int lo;                       /* Lowest field value */
   int ret;                      /* Are usable values being returned? */

/* Check the global error status. */   
   if ( !astOK ) return 0;

/* Indicate that the returned axis descriptions include some secondary
   axis descriptions. */
   *prim = 0;

/* If the arrays have not yet been initialised... */
   if( desc[ 0 ] == -1 ){

/* Loop round every axis, finding the maximum description index for the
   axis. This is done by checking for the existence of keywords with names
   CjVALi, CjPIXi, CjELTi, CjNITi, CjYPEi in the FitsChan, where "j" is
   the description index and "i" is the axis index. */
      mx = mxdesc;
      for( axis = 0; axis < naxis; axis++ ){
         *mx = 1;

         sprintf( key, "C%%1dVAL%d", axis + 1 );         
         if( astKeyFields( this, key, 1, &hi, &lo ) ) *mx = MAX( *mx, hi );

         sprintf( key, "C%%1dPIX%d", axis + 1 );         
         if( astKeyFields( this, key, 1, &hi, &lo ) ) *mx = MAX( *mx, hi );

         sprintf( key, "C%%1dYPE%d", axis + 1 );         
         if( astKeyFields( this, key, 1, &hi, &lo ) ) *mx = MAX( *mx, hi );

         sprintf( key, "C%%1dNIT%d", axis + 1 );         
         if( astKeyFields( this, key, 1, &hi, &lo ) ) *mx = MAX( *mx, hi );

         sprintf( key, "C%%1dELT%d", axis + 1 );         
         if( astKeyFields( this, key, 1, &hi, &lo ) ) *mx = MAX( *mx, hi );

         mx++;

/* Set the current axis description index to 1 (i.e. the primary
   description) for the axis, and set the returned flag. */
         desc[ axis ] = 1;
         *prim = 1;
      }

/* Set the current axis description index of the first axis to zero. This
   will be increment by the main body of this function so that it becomes
   1 (the primary description index). */
      desc[ 0 ] = 0;

   }

/* Find the next set of axis description indicies. */
   ret = 0;
   axis = 0;
   while( axis < naxis ){   

/* Increment the axis description index for the current axis. */
      desc[ axis ]++;
   
/* If this is now larger than the maximum axis description index for this
   axis, then reset it to 1, and increment the axis description index for
   the next axis. */
      if( desc[ axis ] > mxdesc[ axis ] ){
         desc[ axis ] = 1;
         axis++;

/* Otherwise, we have now got the next set of axis description indicies,
   so leave the loop. */
      } else {
         ret = 1;
         break;
      }       

   }

/* Return zero if an error has occurred. */
   if( !astOK ) ret = 0;

/* If we have done all combinations of description indicies, set the current 
   description index for the first axis to -1 so that this function will 
   start again if called again. */
   if( !ret ) desc[ 0 ] = -1;

/* Return the answer. */
   return ret;

}

static int GetFull( AstChannel *this_channel ) {
/*
*  Name:
*     GetFull

*  Purpose:
*     Obtain the value of the Full attribute for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetFull( AstChannel *this )

*  Class Membership:
*     FitsChan member function (over-rides the protected astGetFull
*     method inherited from the Channel class).

*  Description:
*     This function return the integer value of the Full attribute for
*     a FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The Full attribute value.

*  Notes:
*     - This function modifies the default Full value from 0 to -1 for
*     the benefit of the FitsChan class. This prevents non-essential
*     information being written by the astWrite method unless it is
*     requested by explicitlt setting a Full value.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   int result;                   /* Result value to return */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* If the Full attribute us set, obtain its value using the parent class
   method. */
   if ( astTestFull( this ) ) {
      result = (* parent_getfull)( this_channel );

/* Otherwise, supply a default value of -1. */
   } else {
      result = -1;
   }

/* Return the result. */
   return result;
}

static FitsCard *GetLink( FitsCard *card, int next, const char *method, 
                          const char *class ){
/*
*  Name:
*     GetLink

*  Purpose:
*     Get a pointer to the next or previous card in the list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     FitsCard *GetLink( FitsCard *card, int next, const char *method, 
*                        const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns the a pointer to either the next or previous FitsCard
*     structure in the circular linked list of such structures stored in a
*     FitsChan. A check is performed to ensure that the forward and 
*     backward links from the supplied card are consistent and an error
*     is reported if they are not (so long as no previous error has been
*     reported). Memory corruption can result in inconsistent links
*     which can result in infinite loops if an attempt is made to scan the
*     list.

*  Parameters:
*     card
*        The current card.
*     next
*        If non-zero, a pointer to the "next" card is returned. Otherwise
*        a pointer to the "previous" card is returned.
*     method
*        Pointer to string holding the name of the calling method.
*     class
*        Pointer to string holding the object class.

*  Returned Value:
*     A pointer to the required card, or NULL if an error occurs.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   FitsCard *ret;               /* Pointer to the returned card */

/* Check that the "next" link from the previous card points back to
   the current card, and that the "prev" link from the next card points
   back to the current card. */
   if( card && ( card->prev->next != card || 
                 card->next->prev != card ) ){

/* Report an error so long as no previous error has been reported, and
   return a NULL pointer. */
      if( astOK ){
         astError( AST__FCRPT, "%s(%s): A corrupted %s object has been "
                   "supplied.", method, class, class );
      }
      ret = NULL;

/* If the links are good, return a pointer to the required card. */
   } else {
      ret = next ? card->next : card->prev;
   }

/* Return the result. */
   return ret;

}

static int GetNaxis( AstFitsChan *this, int encoding ){
/*
*  Name:
*     GetNaxis

*  Purpose:
*     Return the number of axes described by a FITS header.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetNaxis( AstFitsChan *this, int encoding )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function returns the number of axes described by the FITS
*     header using the supplied encoding scheme.

*  Parameters:
*     this
*        Pointer to the FitsChan holding the header.
*     encoding
*        The encoding system in use.

*  Returned Value:
*     The number of axes.

*  Notes:
*     -  A value of zero is always returned for Native encoding.
*     -  A value of zero is returned if an error has already occurred,
*     or if this function should fail for any reason.

*/
/* Local Variables: */
   char card[ FITSCARDLEN + 1 ]; /* Current card */
   int axis;              /* Axis index extracted from a keyword name */
   int desc;              /* Description index extracted from a keyword name */
   int i;                 /* Axis index extracted from a keyword name */
   int icard;             /* Current card index on entry */
   int j;                 /* Axis index extracted from a keyword name */
   int ret;               /* No. of axes */

/* Check global status. */
   if( !astOK ) return 0;

/* Initialise */
   ret = 0;

/* First deal with FITS-WCS and FITS-IRAF encodings. The number of axis is 
   taken as the largest axis index in the keywords CRVAL, CRPIX, CDELT, CROTA, 
   CTYPE, CUNIT, CmVAL, CmPIX, CmELT, CmNIT, CmYPE, PC and CD. */
   if( encoding == FITSWCS_ENCODING || encoding == FITSIRAF_ENCODING ){

/* Save the current card index, and rewind the FitsChan. */
      icard = astGetCard( this );
      astClearCard( this );

/* Loop round all cards in the FitsChan. */
      while( astFindFits( this, "%f", card, 1 ) ){

/* Check the keyword name in this card against each of the keyword names
   which incorporate an axis index. If it matches, extract the axis index
   and update the maximum axis index. */
         if( sscanf( card, "CRVAL%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "CRPIX%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "CDELT%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "CROTA%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "CUNIT%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "CTYPE%d", &axis ) ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "C%dPIX%d", &desc, &axis ) == 2 ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "C%dVAL%d", &desc, &axis ) == 2 ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "C%dYPE%d", &desc, &axis ) == 2 ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "C%dELT%d", &desc, &axis ) == 2 ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "C%dNIT%d", &desc, &axis ) == 2 ){
            ret = MAX( ret, axis );

         } else if( sscanf( card, "PC%3d%3d", &i, &j ) == 2 ){
            ret = MAX( ret, i );
            ret = MAX( ret, j );

         } else if( sscanf( card, "CD%3d%3d", &i, &j ) == 2 ){
            ret = MAX( ret, i );
            ret = MAX( ret, j );

         } else if( sscanf( card, "CD%1d_%1d", &i, &j ) == 2 ){
            ret = MAX( ret, i );
            ret = MAX( ret, j );
         }

      }

/* Reinstate the original current card index. */
      astSetCard( this, icard );

/* Now deal with DSS encoding. DSS encoding always has 2 axes. */
   } else if( encoding == DSS_ENCODING ){
      ret = 2;

   }

/* Return the answer. */
   return ret;

}

static int GetNcard( AstFitsChan *this ){
/*
*+
*  Name:
*     astGetNcard

*  Purpose:
*     Get the value of the Ncard attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astGetNcard( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function returns the value of the Ncard attribute for the supplied 
*     FitsChan. This is the number of cards currently in the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The number of cards currently in the FitsChan.

*  Notes:
*     - A value of zero will be returned if an error has already
*     occurred, or if this function should fail for any reason.

*-
*/

/* Local Variables: */
   const char *class;      /* Pointer to class string */
   const char *method;     /* Pointer to method string */
   FitsCard *card0;        /* Pointer to current card on entry */
   int ncard;              /* Number of cards so far */

/* Return zero if an error has already occurred, or no FitsChan was supplied, 
   or the FitsChan is empty. */
   if ( !astOK || !this || !this->head ) return 0;

/* Store the method and object class. */
   method = "astGetNcard";
   class = astGetClass( this );

/* Save a pointer to the current card, and then reset the current card to
   be the first card. */
   card0 = this->card;
   astClearCard( this );

/* Count through the cards in the FitsChan until the end of file is reached. */
   ncard = 0;
   while( astOK && this->card ){

/* Increment the card count and move on to the next card. */
      ncard++;
      MoveCard( this, 1, method, class );

   }

/* Reset the current card to be the original current card. */
   this->card = card0;

/* Return the result. */
   return astOK ? ncard : 0;

}

static void GetNextData( AstChannel *this_channel, int skip, char **name,
                         char **val ) {
/*
*  Name:
*     GetNextData

*  Purpose:
*     Read the next item of data from a data source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void GetNextData( AstChannel *this, int skip, char **name, char **val )

*  Class Membership:
*     FitsChan member function (over-rides the protected
*     astGetNextData method inherited from the Channel class).

*  Description:
*     This function reads the next item of input data from a data
*     source associated with a FitsChan and returns the result.  It
*     decodes the data item and returns name/value pairs ready for
*     use.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     skip
*        A non-zero value indicates that a new Object is to be read,
*        and that all input data up to the next "Begin" item are to be
*        skipped in order to locate it. This is useful if the data
*        source contains AST objects interspersed with other data (but
*        note that these other data cannot appear inside AST Objects,
*        only between them).
*
*        A zero value indicates that all input data are significant
*        and the next item will therefore be read and an attempt made
*        to interpret it whatever it contains. Any other data
*        inter-mixed with AST Objects will then result in an error.
*     name
*        An address at which to store a pointer to a null-terminated
*        dynamically allocated string containing the name of the next
*        item in the input data stream. This name will be in lower
*        case with no surrounding white space.  It is the callers
*        responsibilty to free the memory holding this string (using
*        astFree) when it is no longer required.
*
*        A NULL pointer value will be returned (without error) to
*        indicate when there are no further input data items to be
*        read.
*     val
*        An address at which to store a pointer to a null-terminated
*        dynamically allocated string containing the value associated
*        with the next item in the input data stream. No case
*        conversion is performed on this string and all white space is
*        potentially significant.  It is the callers responsibilty to
*        free the memory holding this string (using astFree) when it
*        is no longer required.
*
*        The returned pointer will be NULL if an Object data item is
*        read (see the "Data Representation" section).

*  Data Representation:
*     The returned data items fall into the following categories:
*
*     - Begin: Identified by the name string "begin", this indicates
*     the start of an Object definition. The associated value string
*     gives the class name of the Object being defined.
*
*     - IsA: Identified by the name string "isa", this indicates the
*     end of the data associated with a particular class structure
*     within the definiton of a larger Object. The associated value
*     string gives the name of the class whose data have just been
*     read.
*
*     - End: Identified by the name string "end", this indicates the
*     end of the data associated with a complete Object
*     definition. The associated value string gives the class name of
*     the Object whose definition is being ended.
*
*     - Non-Object: Identified by any other name string plus a
*     non-NULL "val" pointer, this gives the value of a non-Object
*     structure component (instance variable). The name identifies
*     which instance variable it is (within the context of the class
*     whose data are being read) and the value is encoded as a string.
*
*     - Object: Identified by any other name string plus a NULL "val"
*     pointer, this identifies the value of an Object structure
*     component (instance variable).  The name identifies which
*     instance variable it is (within the context of the class whose
*     data are being read) and the value is given by subsequent data
*     items (so the next item should be a "Begin" item).

*  Notes:
*     - NULL pointer values will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 100             /* Length of formatting buffer */

/* Local Variables: */
   const char *class;            /* Pointer to object class */
   const char *method;           /* Pointer to method name */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   char *keyword;                /* Pointer to current keyword string */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatting values */
   int done;                     /* Data item found? */
   int i;                        /* Loop counter for keyword characters */
   int len;                      /* Length of current keyword */
   int nc;                       /* Number of characters read by "sscanf" */
   int old_skipping;             /* Original value of external variable Skipping */
   int type;                     /* Data type code */
   void *data;                   /* Pointer to current data value */

/* Initialise the returned pointer values. */
   *name = NULL;
   *val = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Store the method name and object class. */
   method = "astRead";
   class = astGetClass( this );

/* We skip over cards which have already been read. */
   old_skipping = Skipping;
   Skipping = 1;

/* Loop to consider successive cards stored in the FitsChan (starting
   at the "current" card) until a valid data item is read or "end of
   file" is reached. Also quit the loop if an error occurs. */
   done = 0;
   while ( !done && !astFitsEof( this ) && astOK ){

/* Obtain the keyword string, data type code and data value pointer
   from the current card. */
      keyword = CardName( this );
      type = CardType( this );
      data = CardData( this, NULL );

/* Mark all cards for deletion unless we are skipping over cards which
   may not be related to AST. */
      if( !skip ) MarkCard( this );

/* Ignore comment cards. */
      if ( type != AST__COMMENT ) {

/* Obtain the keyword length and test the card to identify the type of
   AST data item (if any) that it represents. */
         len = (int) strlen( keyword );

/* "Begin" item. */
/* ------------- */
/* This is identified by a string value and a keyword of the form
   "BEGASTxx", where "xx" are characters encoding a sequence
   number. */
         if ( ( type == AST__STRING ) &&
              ( nc = 0,
                ( 0 == sscanf( keyword, "BEGAST"
                                        "%*1[" SEQ_CHARS "]"
                                        "%*1[" SEQ_CHARS "]%n", &nc ) )
                && ( nc >= len ) ) ) {

/* Note we have found a data item. */
            done = 1;

/* Set the returned name to "begin" and extract the associated class
   name from the string value. Store both of these in dynamically
   allocated strings. */
            *name = astString( "begin", 5 );
            *val = UnPreQuote( (const char *) data );

/* Indicate that the current card has been used by marking it for deletion. */
            MarkCard( this );

/* The "begin" item will be preceded by a header of COMMENT cards. Mark
   them as read. */
            Skipping = 0;
            ComBlock( this, -1, method, class );
            Skipping = 1;

/* "IsA" item. */
/* ----------- */
/* This is identified by a string value and a keyword of the form
   "ISAxx", where "xx" are characters encoding a sequence
   number. Don't accept the item if we are skipping over cards looking
   for a "Begin" item. */
         } else if ( !skip &&
                     ( type == AST__STRING ) &&
                     ( nc = 0,
                       ( 0 == sscanf( keyword,
                                      "ISA"
                                      "%*1[" SEQ_CHARS "]"
                                      "%*1[" SEQ_CHARS "]%n", &nc ) )
                       && ( nc >= len ) ) ) {

/* Note we have found a data item. */
            done = 1;

/* Set the returned name to "isa" and extract the associated class
   name from the string value. Store both of these in dynamically
   allocated strings. */
            *name = astString( "isa", 3 );
            *val = UnPreQuote( (const char *) data );

/* "End" item. */
/* ----------- */
/* This is identified by a string value and a keyword of the form
   "ENDASTxx", where "xx" are characters encoding a sequence
   number. Don't accept the item if we are skipping over cards looking
   for a "Begin" item. */
         } else if ( !skip &&
                     ( type == AST__STRING ) &&
                     ( nc = 0,
                       ( 0 == sscanf( keyword,
                                      "ENDAST"
                                      "%*1[" SEQ_CHARS "]"
                                      "%*1[" SEQ_CHARS "]%n", &nc ) )
                       && ( nc >= len ) ) ) {

/* Note we have found a data item. */
            done = 1;

/* Set the returned name to "end" and extract the associated class
   name from the string value. Store both of these in dynamically
   allocated strings. */
            *name = astString( "end", 3 );
            *val = UnPreQuote( (const char *) data );

/* The "end" item eill be followed by a footer of COMMENT cards. Mark
   these cards as read. */
            Skipping = 0;
            ComBlock( this, 1, method, class );
            Skipping = 1;

/* Object or data item. */
/* -------------------- */
/* These are identified by a string, int, or double value, and a
   keyword ending in two characters encoding a sequence number. Don't
   accept the item if we are skipping over cards looking for a "Begin"
   item. */
         } else if ( !skip &&
                     ( ( type == AST__STRING ) ||
                       ( type == AST__INT ) ||
                       ( type == AST__FLOAT ) ) &&
                     ( len > 2 ) &&
                     strchr( SEQ_CHARS, keyword[ len - 1 ] ) &&
                     strchr( SEQ_CHARS, keyword[ len - 2 ] ) ) {

/* Note we have found a data item. */
            done = 1;

/* Set the returned name by removing the last two characters from the
   keyword and converting to lower case. Store this in a dynamically
   allocated string. */
            *name = astString( keyword, len - 2 );
            for ( i = 0; ( *name )[ i ]; i++ ) {
               ( *name )[ i ] = tolower( ( *name )[ i ] );
            }

/* Classify the data type. */
            switch ( type ) {

/* If the value is a string, test if it is zero-length. If so, this
   "null" value indicates an Object data item (whose definition
   follows), so leave the returned value pointer as NULL. Otherwise,
   we have a string data item, so extract its value and store it in a
   dynamically allocated string. */
            case AST__STRING:
               if ( *( (char *) data ) ) {
                  *val = UnPreQuote( (const char *) data );
               }
               break;

/* If the value is an int, format it and store the result in a
   dynamically allocated string. */
            case AST__INT:
               (void) sprintf( buff, "%d", *( (int *) data ) );
               *val = astString( buff, (int) strlen( buff ) );
               break;

/* If the value is a double, format it and store the result in a
   dynamically allocated string. */
            case AST__FLOAT:
               (void) sprintf( buff, "%.*g", DBL_DIG, *( (double *) data ) );
               CheckZero( buff,  *( (double *) data ) );
               *val = astString( buff, (int) strlen( buff ) );
               break;
            }

/* Anything else. */
/* -------------- */
/* If the input line didn't match any of the above and the "skip" flag
   is not set, then report an error.. */
         } else if ( !skip ) {
            astError( AST__BADIN,
                      "%s(%s): Cannot interpret the input data given by "
                      "FITS keyword \"%s\".", method, class, keyword );
         }
      }

/* Increment the current card. */
      MoveCard( this, 1, method, class );
   }

/* If an error occurred, ensure that any allocated memory is freed and
   that NULL pointer values are returned. */
   if ( !astOK ) {
      *name = astFree( *name );
      *val = astFree( *val );
   }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
   Skipping = old_skipping;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static int GetSkip( AstChannel *this_channel ) {
/*
*  Name:
*     GetSkip

*  Purpose:
*     Obtain the value of the Skip attribute for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetSkip( AstChannel *this )

*  Class Membership:
*     FitsChan member function (over-rides the protected astGetSkip
*     method inherited from the Channel class).

*  Description:
*     This function return the (boolean) integer value of the Skip
*     attribute for a FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The Skip attribute value.

*  Notes:
*     - This function modifies the default Skip value from 0 to 1 for
*     the benefit of the FitsChan class. This default value allows the
*     astRead method to skip over unrelated FITS keywords when
*     searching for the next Object to read.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   int result;                   /* Result value to return */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* If the Skip attribute us set, obtain its value using the parent class
   method. */
   if ( astTestSkip( this ) ) {
      result = (* parent_getskip)( this_channel );

/* Otherwise, supply a default value of 1. */
   } else {
      result = 1;
   }

/* Return the result. */
   return result;
}

static int GetWcsValue( AstFitsChan *this, char *keyname, int type, 
                        void *value, int report, const char *method, 
                        const char *class ){
/*
*  Name:
*     GetWcsValue

*  Purpose:
*     Obtain a FITS WCS keyword value.

*  Type:
*     Private function.

*  Synopsis:
*     int GetWcsValue( AstFitsChan *this, char *keyname, int type, void *value, 
*                      int report, const char *method, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     This function gets a value for the specified keyword from the
*     supplied FitsChan, and stores it in the supplied buffer. The keyword
*     is marked as having been read into an AST object so that it is not
*     written out when the FitsChan is deleted.

*  Parameters:
*     this
*        A pointer to the FitsChan containing the keyword values to be
*        read.
*     keyname
*        A pointer to a string holding the keyword name.
*     type
*        The FITS data type in which to return the keyword value. If the
*        stored value is not of the requested type, it is converted if
*        possible. 
*     value
*        A pointer to a buffer of suitable size to receive the keyword
*        value. The supplied value is left unchanged if the keyword is
*        not found.
*     report
*        Should an error be reported if the keyword cannot be found, or
*        cannot be converted to the requested type?
*     method
*        A string holding the name of the calling method.
*     class
*        A string holding the object class.

*  Returned Value:
*     Zero if the keyword does not exist in "this", or cannot be
*     converted to the requested type. One is returned otherwise.

*  Notes:
*     -  A value of zero is returned if an error has already occurred,
*     or if an error occurs within this function.

*/

/* Local Variables: */
   int ret;                           /* Returned value */
   size_t sz;                         /* Data size in bytes */

/* Check the status */
   if( !astOK ) return 0;

/* Attempt to find the supplied keyword. */
   ret = SearchCard( this, keyname, method, class );

/* If the keyword was found, convert the current card's data value and copy 
   it to the supplied buffer. */
   if( ret ){
      if( CnvValue( this, type, value, method ) ) {

/* Mark it as having been read into an AST object. */
         MarkCard( this );
    
/* If the value could not be converted to the requested data, type report
   an error if reporting is enabled. */
      } else {
         ret = 0;
         if( report && astOK ){
            astError( AST__FTCNV, "%s(%s): Cannot convert FITS keyword " \
                      "'%s' (value '%s') to %s.", method, class, \
                      keyname, CardData( this, &sz ), type_names[ type ] ); \
         }
      }

/* If the keyword was not found, report an error if "report" is non-zero. */
   } else if( report && astOK ){
      astError( AST__BDFTS, "%s(%s): Unable to find a value for FITS "
                "keyword \"%s\".", method, class, keyname );
   }

/* If an error has occurred, return 0. */
   if( !astOK ) ret = 0;

/* Return the result. */
   return ret;

}

static int IrafStore( AstFitsChan *this, FitsStore *store ){
/*
*  Name:
*     IrafStore

*  Purpose:
*     Stores FITS-IRAF keywords in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int IrafStore( AstFitsChan *this, FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates a FITS-IRAF header within a supplied FitsChan 
*     holding the keyword values supplied in the given FitsStore structure. 

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the IRAF keywords.

*  Returned Value:
*     One if any keywords were stored in the FitsChan. Zero otherwise.

*  Notes:
*     -  Angular keyword values should be supplied in units of degrees.
*     -  AST__FLOAT keyword values should be supplied as AST__BAD if they take
*     their default values. Such values are not written out.
*     -  AST__STRING keyword value pointers should be supplied as NULL if they 
*     are undefined. Such values are not written out.
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   double p1;               /* Projection parameter PROJP1 */
   double p2;               /* Projection parameter PROJP2 */
   int axlat;               /* Latitude axis index */
   int axlon;               /* Longitude axis index */
   int ok;                  /* Can the physical Frame be encoded? */
   int prj;                 /* Projection identifier */
   int ret;                 /* Were any cards added to the FitsChan? */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that nothing has been added
   to the FitsChan. */
   ret = 0;

/* Check that the keyword values supplied in the FitsStore structure can
   be represented by FITS-IRAF keywords
   ===================================================================== */

/* Get the indices of any celestial longitude and latitude axes. */
   axlat = store->axlat;
   axlon = store->axlon;

/* If both are present in the physical frame... */
   if( axlon >= 0 && axlat >= 0 ) {

/* Extract the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. */
      prj = astWcsPrjType( store->ctype[ 0 ] + 4 );

/* Check the projection type is OK. Assume not initially. */
      ok = 0;
      if( prj == AST__TAN ||
          prj == AST__ARC ||
          prj == AST__STG ||
          prj == AST__MER ||
          prj == AST__AIT ||
          prj == AST__GLS ) {
   
         ok = 1;

/* SIN projections are only acceptable if the associated projection
   parameters (PROJP1 and PROJP2) are both zero, or if PROJP1 is zero
   and PROJP2 = cot( reference point latitude )  (the latter case is
   equivalent to the old NCP projection). */
      } else if( prj == AST__SIN ){
         p1 = ( store->projp )[ 1 ];
         p2 = ( store->projp )[ 2 ];
   
         if( p1 == 0.0 ) {
            if( p2 == 0.0 ) {
               ok = 1;
   
            } else if( fabs( p2 ) >= 1.0E14 && store->crval[ axlat ] == 0.0 ){
               ok = 1;
               (void) strcpy( store->ctype[ axlon ] + 4, "NCP-" );
               (void) strcpy( store->ctype[ axlat ] + 4, "NCP-" );
   
            } else if( fabs( p2*tan( AST__DD2R*store->crval[ axlat ] ) - 1.0 ) 
                       < 0.01 ){
               ok = 1;
               (void) strcpy( store->ctype[ axlon ] + 4, "NCP-" );
               (void) strcpy( store->ctype[ axlat ] + 4, "NCP-" );
            }
         }
      }

/* Identify the celestial coordinate system from the first 4 characters of the
   longitude CTYPE value. Only RA, galactic longitude, and ecliptic
   longitude can be stored using FITS-IRAF. */
      if( strncmp( store->ctype[ axlon ], "RA--", 4 ) &&
          strncmp( store->ctype[ axlon ], "GLON", 4 ) &&
          strncmp( store->ctype[ axlon ], "ELON", 4 ) ) ok = 0;

/* If the physical Frame requires a LONGPOLE or LATPOLE keyword, it cannot
   be encoded using FITS-IRAF. */
      if( store->latpole != AST__BAD || store->longpole != AST__BAD ) ok = 0;

/* If there are no clestial axes, the physical Frame can be written out
   using FITS-IRAF. */
   } else {
      ok = 1;
   }

/* If possible, format and store the keyword values in the FitsChan. A CD 
   matrix is used instead of a PC matrix. */
   if( ok ) {
      WcsPrimary( this, store, 1 );
      ret = 1;
   }

/* If an error has occurred, indicate that nothing has been added to the 
   FitsChan. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static void InitVtab( AstFitsChanVtab *vtab ) {
/*
*  Name:
*     InitVtab

*  Purpose:
*     Initialise a virtual function table for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void InitVtab( AstFitsChanVtab *vtab )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the FitsChan class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes should already have been initialised.
*/

/* Local Variables: */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstChannelVtab *channel;      /* Pointer to Channel component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAFitsChan) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->PutFits = PutFits;   
   vtab->DelFits = DelFits;   
   vtab->FindFits = FindFits;   
   vtab->KeyFields = KeyFields;
   vtab->Empty = Empty;
   vtab->FitsEof = FitsEof;
   vtab->FitsGetCF = FitsGetCF;
   vtab->FitsGetCI = FitsGetCI;
   vtab->FitsGetF = FitsGetF;
   vtab->FitsGetI = FitsGetI;
   vtab->FitsGetL = FitsGetL;
   vtab->FitsGetS = FitsGetS;
   vtab->FitsGetCom = FitsGetCom;
   vtab->FitsSetCom = FitsSetCom;
   vtab->FitsSetCF = FitsSetCF;
   vtab->FitsSetCI = FitsSetCI;
   vtab->FitsSetF = FitsSetF;
   vtab->FitsSetI = FitsSetI;
   vtab->FitsSetL = FitsSetL;
   vtab->FitsSetS = FitsSetS;
   vtab->ClearCard = ClearCard;
   vtab->TestCard = TestCard;
   vtab->SetCard = SetCard;
   vtab->GetCard = GetCard;
   vtab->ClearFitsDigits = ClearFitsDigits;
   vtab->TestFitsDigits = TestFitsDigits;
   vtab->SetFitsDigits = SetFitsDigits;
   vtab->GetFitsDigits = GetFitsDigits;
   vtab->ClearCDMatrix = ClearCDMatrix;
   vtab->TestCDMatrix = TestCDMatrix;
   vtab->SetCDMatrix = SetCDMatrix;
   vtab->GetCDMatrix = GetCDMatrix;
   vtab->GetNcard = GetNcard;
   vtab->ClearEncoding = ClearEncoding;
   vtab->TestEncoding = TestEncoding;
   vtab->SetEncoding = SetEncoding;
   vtab->GetEncoding = GetEncoding;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   channel = (AstChannelVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;
 
   parent_write = channel->Write;
   channel->Write = Write;
   parent_read = channel->Read;
   channel->Read = Read;
   parent_getskip = channel->GetSkip;
   channel->GetSkip = GetSkip;
   parent_getfull = channel->GetFull;
   channel->GetFull = GetFull;

   channel->WriteBegin = WriteBegin;
   channel->WriteIsA = WriteIsA;
   channel->WriteEnd = WriteEnd;
   channel->WriteInt = WriteInt;
   channel->WriteDouble = WriteDouble;
   channel->WriteString = WriteString;
   channel->WriteObject = WriteObject;
   channel->GetNextData = GetNextData;

/* Declare the class dump, copy and delete functions.*/
   astSetDump( vtab, Dump, "FitsChan", "I/O channels to FITS files" );
   astSetCopy( (AstObjectVtab *) vtab, Copy );
   astSetDelete( (AstObjectVtab *) vtab, Delete );

/* Indicate that the private functions which navigate the circular linked
   list of FitsCard structures should not skip over cards which have been 
   read into an AST object. */
   Skipping = 1;

}

static void InsCard( AstFitsChan *this, int overwrite, const char *name, 
                     int type, void *data, const char *comment, 
                     const char *method, const char *class ){
/*
*  Name:
*     InsCard

*  Purpose:
*     Inserts a card into a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void InsCard( AstFitsChan *this, int overwrite, const char *name, 
*                   int type, void *data, const char *comment, 
*                   const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Either appends a new card to a FitsChan, or over-writes an existing 
*     card, holding the supplied keyword name, value and comment. 

*  Parameters:
*     this
*        Pointer to the FitsChan containing the filters to apply to the
*        keyword name. If a NULL pointer is supplied, no filtering is applied.
*     overwrite
*        If non-zero, the new card over-writes the current card given by 
*        the "Card" attribute, and the current card is incremented so
*        that it refers to the next card. Otherwise, the new card is 
*        inserted in front of the current card and the current card is
*        left unchanged.
*     name
*        Pointer to a string holding the keyword name of the new card.
*     type
*        An integer value representing the data type of the keyword.
*     data
*        Pointer to the data associated with the keyword. 
*     comment
*        Pointer to a null-terminated string holding a comment.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Notes:
*     -  An error is reported if an attempt is made to change the data type 
*     of an existing card.
*     -  If a type of AST__COMMENT is supplied, then any data value (of any 
*     type) associated with an existing card is left unchanged.

*/

/* Check the global status. */
   if( !astOK ) return;

/* If the current card is to be over-written, delete the current card (the 
   next card in the list, if any, will become the new current card). */
   if( overwrite ) DeleteCard( this, method, class );

/* Insert the new card into the list, just before the current card. */
   NewCard( this, name, type, data, comment, 0 );

}

static int LinearMap( AstMapping *map, int nin, int nout, double *matrix,
                      double *test, double maxerr ){
/*
*  Name:
*     LinearMap

*  Purpose:
*     See if a mapping is linear and return a matrix describing it.

*  Type:
*     Private function.

*  Synopsis:
*     int LinearMap( AstMapping *map, int nin, int nout, double *matrix,
*                    double *test, double maxerr )

*  Class Membership:
*     FitsChan

*  Description:
*     This function checks to see if the supplied Mapping is linear,
*     except for an optional shift of origin.
*   
*     Unit vectors along each of the axes in the Mapping's input coordinate 
*     system are transformed using the Mapping. If the Mapping is linear, 
*     these transformed vectors form the columns of the required matrix. To 
*     test for linearity, the supplied test point is transformed using the 
*     supplied Mapping, and is then also transformed using the candidate 
*     matrix formed by the above process. If the two resulting positions are 
*     equal to within the supplied error, then the Mapping is considered to 
*     be linear. 
*
*     To account for a possible shift of origin, the origin of the Mappings
*     input Frame is transformed using the supplied Mapping, and this
*     position is subtracted from all other transformed positions.

*  Parameters:
*     map
*        A pointer to the Mapping to be checked.
*     nin
*        The number of input coordinates. This is the number of columns
*        in the returned matrix.
*     nout
*        The number of output coordinates. This is the number of rows
*        in the returned matrix.
*     matrix
*        A pointer to an array of nin*nout elements to receive the matrix.
*        The elements are stored row by row. If the Mapping is not linear
*        the supplied values are left unchanged.
*     test
*        A pointer to an array holding the test point to use, in the input
*        frame of the Mapping. This point should be well away form the
*        origin to ensure that he linearity test is applied over a sizable 
*        distance. The default value of 1000 is used in place of any bad 
*        values in this array.
*     maxerr
*        For two positions in the output coordinate system of the Mapping
*        to be considered equal, the square of the distance between them
*        must not be greater than "maxerr".

*  Returned Value:
*     Zero if the Mapping is not linear, and one otherwise.

*  Notes:
*     -  A value of zero is returned if an error has already occurred,
*     or if an error occurs within this function.

*/

/* Local Variables: */
   AstPointSet *pset1;                /* Pointer to the input PointSet */
   AstPointSet *pset2;                /* Pointer to the output PointSet */
   double **ptr1;                     /* Pointer to the input positions */
   double **ptr2;                     /* Pointer to the output positions */
   double *p;                         /* Pointer to next local axis value */
   double *q;                         /* Pointer to next returned axis value */
   double err;                        /* Absolute difference between axis values */
   double orig;                       /* Constant term on this axis */
   double sumerr2;                    /* Squared distance between 2 points */
   double sum;                        /* Sum of axis values */
   double tvl;                        /* Squared length of test vector */
   int i;                             /* Loop count */
   int j;                             /* Loop count */
   int ret;                           /* Returned value */

/* Check the status */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that the Mapping is not
   linear. */
   ret = 0;

/* Get a PointSet to hold a unit vector along every input axis, plus two
   extra points. */
   pset1 = astPointSet( nin + 2, nin, "" );
   ptr1 = astGetPoints( pset1 );
   if( astOK ){

/* We now store unit vectors along each of the input axes in the PointSet. 
   The first extra point is the supplied test point. The second extra point 
   is the origin. At the same time form the squared length of the test
   vector. */
      tvl = 0.0;
      for( i = 0; i < nin; i++ ){
         p = ptr1[ i ];
         for( j = 0; j < nin; j++ ) *(p++) = 0.0;
         if( test[ i ] != AST__BAD ) {
            *(p++) = test[ i ];
            tvl += test[ i ]*test[ i ];
         } else {
            *(p++) = 1000.0;
            tvl += 1000000.0;
         }
         *p = 0.0;
         ptr1[ i ][ i ] = 1.0;
      }

/* Transform these vectors using the supplied Mapping, and get pointers
   to the transformed data. If the Mapping is linear, the transformed data
   will define the required matrix, with two extra columns holding the
   transformed test point and origin. */
      pset2 = astTransform( map, pset1, 1, NULL );
      ptr2 = astGetPoints( pset2 );
      if( astOK ){

/* Subtract the transformed origin (the second extra position in the
   PointSet) from each of the other positions in the PointSet. */
         for( i = 0; i < nin; i++ ){
            p = ptr2[ i ];
            orig = ptr2[ i ][ nin + 1 ];
            if( orig != AST__BAD ) {
               for( j = 0; j < nin + 1; j++ ) {
                  if( *p != AST__BAD ) *p -= orig;
                  p++;
               }               
            } else {
               for( j = 0; j < nin + 1; j++ ) *(p++) = AST__BAD;
            }
         }

/* We now test the Mapping for linearity. The test point is mapped using
   the candidate matrix, and the squared distance between the resulting
   point and the point found using the supplied Mapping is found. */
         sumerr2 = 0.0;
         for( i = 0; i < nin; i++ ){
            p = ptr2[ i ];
            sum = 0.0;
            for( j = 0; j < nin; j++ ) {
               if( *p != AST__BAD ) {
                  sum += *(p++)*( ( test[ j ] != AST__BAD ) ? test[ i ] : 1000.0 );
               } else {
                  sum = AST__BAD;
                  break;
               }
            }
            if( sum != AST__BAD && *p != AST__BAD ) {
               err = *p - sum;
               sumerr2 += err*err;
            } else {
               sumerr2 = AST__BAD;
               break;
            }
         }

/* If the squared distance is less than the supplied maximum error times
   the squared length of the test vector, the Mapping is considered to be 
   linear. Copy it to the returned array, row by row. */
         if( sumerr2 != AST__BAD && sumerr2 < tvl*maxerr ){
            ret = 1;
            q = matrix;
            for( i = 0; i < nin; i++ ){
               p = ptr2[ i ];
               for( j = 0; j < nin; j++ ) *(q++) = *(p++);
            }
         }
      }

/* Annul the output PointSet. */
      pset2 = astAnnul( pset2 );

   }


/* Annul the input PointSet. */
   pset1 = astAnnul( pset1 );

/* If an error has occurred, return 0. */
   if( !astOK ) ret = 0;

/* Return the result. */
   return ret;

}

static void LinearSky( int prim, AstFrame *phyfrm, int naxis, 
                       FitsStore *store ){
/*
*  Name:
*     LinearSky

*  Purpose:
*     See if a non-WCS Frame can be described by a linear WCS projection.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void LinearSky( int prim, AstFrame *phyfrm, int naxis, 
*                     FitsStore *store )

*  Class Membership:
*     FitsChan

*  Description:
*     This function checks to see if the supplied Frame is (or contains) a
*     SkyFrame. If so, the longitude and latitude axes are found, and the
*     FITS-WCS keywords CTYPE, EQUINOX, MJDOBS, RADECSYS and CROTA are
*     set up to describe the SkyFrame. The keyword values are stored in
*     "store".

*  Parameters:
*     prim
*        Are primary axis descriptions being produced?
*     phyfrm
*        Pointer to the Frame.
*     naxis
*        Number of axes in the Frame.
*     store
*        Pointer to the Fitstore structure in whioch to store the new
*        keyword values.

*/

/* Local Variables: */
   AstSkyFrame *skyfrm;   /* Pointer to the first SkyFrame to be found */   
   AstFrame *frame;       /* Pointer to the Frame containing the current axis */   
   int axis;              /* Axis index within "frame" */
   int axlat;             /* Latitude axis within "phyfrm" */
   int axlon;             /* Longitude axis within "phyfrm" */
   int i;                 /* Axis index within "phyfrm" */
   
/* Check the status */
   if( !astOK ) return;

/* Loop round each axis of the supplied Frame looking for axes which
   belong to a SkyFrame. */
   skyfrm = NULL;
   for( i = 0; i < naxis; i++ ){

/* Get a pointer to the Frame containing the current axis. Also get the
   index of the current axis within that Frame. */
      astPrimaryFrame( phyfrm, i, &frame, &axis );

/* If the current axis belongs to a SkyFrame, we need to check that it is
   the same SkyFrame as any previously found axis. */
      if( astIsASkyFrame( frame ) && 
          ( !skyfrm || ( (AstFrame *) skyfrm ) == frame ) ){

/* Record the pointer to the SkyFrame so that we can check that any
   future candidate axis belongs to the same SkyFrame. */
         skyfrm = (AstSkyFrame *) frame;

/* If we have found the longitude axis, record its index within the
   supplied Frame. Likewise, for the latitude axis. */
         if( axis == 0 ) {
            axlon = i;
         } else {
            axlat = i;
         }
      }
   }

/* If we found a SkyFrame, use it to set up FITS-WCS keywords describing 
   a linear projection. */
   if( skyfrm ){
      if( SkySys( prim, frame, AST__CAR, store, axlon, axlat ) ){

/* If we are producing primary axis descriptions, store a value for the
   deprecated CROTA keyword for the benefit of older FITS readers. */
         Crota( prim, store, naxis, axlon, axlat );

/* The CRVAL, CDELT and CROTA values need to be converted from radians to
   degrees. */
         if( store->crval[ axlon ] != AST__BAD ) store->crval[ axlon ] *= AST__DR2D;
         if( store->crval[ axlat ] != AST__BAD ) store->crval[ axlat ] *= AST__DR2D;
         if( store->cdelt[ axlon ] != AST__BAD ) store->cdelt[ axlon ] *= AST__DR2D;
         if( store->cdelt[ axlat ] != AST__BAD ) store->cdelt[ axlat ] *= AST__DR2D;
         if( store->crota[ axlon ] != AST__BAD ) store->crota[ axlon ] *= AST__DR2D;
         if( store->crota[ axlat ] != AST__BAD ) store->crota[ axlat ] *= AST__DR2D;
      }
   }
}

static void MakeBanner( const char *prefix, const char *middle,
                        const char *suffix,
                        char banner[ FITSCARDLEN -
                                     FITSNAMLEN + 1 ] ) {
/*
*  Name:
*     MakeBanner

*  Purpose:
*     Create a string containing a banner comment.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void MakeBanner( const char *prefix, const char *middle,
*                      const char *suffix,
*                      char banner[ FITSCARDLEN - FITSNAMLEN + 1 ] )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates a string which can be written as a FITS
*     comment card to produce a banner heading (or tail) for an AST
*     Object when it is written to a FitsChan. The banner will occupy
*     the maximum permitted width for text in a FITS comment card.

*  Parameters:
*     prefix
*        A pointer to a constant null-terminated string containing the
*        first part of the text to appear in the banner.
*     middle
*        A pointer to a constant null-terminated string containing the
*        second part of the text to appear in the banner.
*     suffix
*        A pointer to a constant null-terminated string containing the
*        third part of the text to appear in the banner.
*     banner
*        A character array to receive the null-terminated result string.

*  Notes:
*     - The text to appear in the banner is constructed by
*     concatenating the three input strings supplied.
*/

/* Local Variables: */
   char token[] = "AST";         /* Identifying token */
   int i;                        /* Loop counter for input characters */
   int len;                      /* Number of output characters */
   int ltok;                     /* Length of token string */
   int mxlen;                    /* Maximum permitted output characters */
   int start;                    /* Column number where text starts */

/* Check the global error status. */
   if ( !astOK ) return;

/* Calculate the maximum number of characters that the output banner
   can hold and the length of the token string. */
   mxlen = FITSCARDLEN - FITSNAMLEN;
   ltok = (int) strlen( token );

/* Calculate the column in which to start the text, so that it is
   centred in the banner (with 4 non-text characters on each side). */
   start = ltok + 2 + ( mxlen - ltok - 1 -
                        (int) ( strlen( prefix ) +
                                strlen( middle ) +
                                strlen( suffix ) ) - 1 - ltok ) / 2;
   if ( start < ltok + 2 ) start = ltok + 2;

/* Start building the banner with the token string. */
   len = 0;
   for ( i = 0; token[ i ] && ( len < mxlen ); i++ ) {
      banner[ len++ ] = token[ i ];
   }

/* Then pad with spaces up to the start of the text. */
   while ( len < start - 1 ) banner[ len++ ] = ' ';

/* Insert the prefix data, truncating it if it is too long. */
   for ( i = 0; prefix[ i ] && ( len < mxlen - ltok - 1 ); i++ ) {
      banner[ len++ ] = prefix[ i ];
   }

/* Insert the middle data, truncating it if it is too long. */
   for ( i = 0; middle[ i ] && ( len < mxlen - ltok - 1 ); i++ ) {
      banner[ len++ ] = middle[ i ];
   }

/* Insert the suffix data, truncating it if it is too long. */
   for ( i = 0; suffix[ i ] && ( len < mxlen - ltok - 1 ); i++ ) {
      banner[ len++ ] = suffix[ i ];
   }

/* Pad the end of the text with spaces. */
   while ( len < mxlen - ltok ) banner[ len++ ] = ' ';

/* Finish the banner with the token string. */
   for ( i = 0; token[ i ] && ( len < mxlen ); i++ ) {
      banner[ len++ ] = token[ i ];
   }

/* Terminate the output string. */
   banner[ len ] = '\0';
}

static void MakeIndentedComment( int indent, char token,
                                 const char *comment, const char *data,
                                 char string[ FITSCARDLEN -
                                              FITSNAMLEN + 1 ] ) {
/*
*  Name:
*     MakeIndentedComment

*  Purpose:
*     Create a comment string containing an indentation bar.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void MakeIndentedComment( int indent, char token,
*                               const char *comment, const char *data,
*                               char string[ FITSCARDLEN -
*                                            FITSNAMLEN + 1 ] )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates a string that may be used as text in a
*     FITS comment card. The string contains a textual comment
*     preceded by a bar (a line of characters) whose length can be
*     used to indicate a level of indentation (in the absence of any
*     way of indenting FITS keywords).

*  Parameters:
*     indent
*        The level of indentation, in characters.
*     token
*        The character used to form the indentation bar.
*     comment
*        A pointer to a constant null-terminated string containing the text
*        of the comment to be included.
*     data
*        A pointer to a constant null-terminated string containing any
*        textual data to be appended to the comment.
*     string
*        A character array to receive the output string.

*  Notes:
*    - The comment text that appears in the output string is formed by
*   concatenating the "comment" and "data" strings.
*/

/* Local Variables: */
   int i;                        /* Loop counter for input characters */
   int len;                      /* Number of output characters */
   int mxlen;                    /* Maximum length of output string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Calculate the maximum number of characters that the output string
   can accommodate. */
   mxlen = FITSCARDLEN - FITSNAMLEN;

/* Start the string with "indent" copies of the token character, but
   without exceeding the output string length. */
   len = 0;
   while ( ( len < indent ) && ( len < mxlen ) ) string[ len++ ] = token;

/* Pad with spaces up to the start of the comment, if necessary. */
   while ( len < ( FITSCOMCOL - FITSNAMLEN - 1 ) ) {
      string[ len++ ] = ' ';
   }

/* Add "/ " to introduce the comment (strictly not necessary as the
   whole card will be a comment, but it matches the other non-comment
   cards). Truncate if necessary. */
   for ( i = 0; ( i < 2 ) && ( len < mxlen ); i++ ) {
      string[ len++ ] = "/ "[ i ];
   }

/* Append the comment string, truncating it if it is too long. */
   for ( i = 0; comment[ i ] && ( len < mxlen ); i++ ) {
      string[ len++ ] = comment[ i ];
   }

/* Append the data string, again truncating if too long. */
   for ( i = 0; data[ i ] && ( len < mxlen ); i++ ) {
      string[ len++ ] = data[ i ];
   }

/* Terminate the output string. */
   string[ len ] = '\0';
}

static void MakeIntoComment( AstFitsChan *this, const char *method, 
                             const char *class ){
/*
*  Name:
*     MakeIntoComment

*  Purpose:
*     Convert a card into a FITS COMMENT card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void MakeIntoComment( AstFitsChan *this, const char *method,
*                           const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function formats the card stored just prior to the current card, 
*     and re-stores it as a COMMENT card. It is used (when writing an Object 
*     to a FitsChan) to output values that are not "set" and which are 
*     therefore provided for information only, and should not be read back. 
*     the COMMENT card has the effect of "commenting out" the value.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     method
*        Calling method.
*     class
*        Object class.

*/

/* Local Variables: */
   char card[ FITSCARDLEN + 1 ]; /* Character buffer for FITS card data */

/* Check the global error status. */
   if ( !astOK ) return;

/* Move the current card backwards by one card. */
   MoveCard( this, -1, method, class );    

/* Format the new current card. */
   FormatCard( this, card, method );

/* Write the resulting string to the FitsChan as the contents of a COMMENT 
   card, overwriting the existing card. The current card is incremented
   by this call so that it refers to the same card as on entry. */
   astFitsSetCom( this, "COMMENT", card, 1 );

}

static int Match( const char *test, const char *temp, int maxfld, int *fields,
                  int *nfld, const char *method, const char *class ){
/*
*  Name:
*     Match

*  Purpose:
*     Sees if a test keyword name matches a template.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int Match( const char *test, const char *temp, int maxfld, int *fields,
*                int *nfld, const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     All characters in the template other than "%" (and the field width
*     and type specifiers which follow a "%") must be matched by an 
*     identical character (ignoring case) in the test string. If a "%" occurs 
*     in the template, then the next character in the template should be a
*     single digit specifying a field width. If it is zero, then the test 
*     string may contain zero or more matching characters. Otherwise,
*     the test string must contain exactly the specified number of matching 
*     characters (i.e. 1 to 9). The field width digit may be omitted, in 
*     which case the test string must contain one or more matching 
*     characters. The next character in the template specifies the type of 
*     matching characters and must be one of "d", "c" or "f". Decimal digits 
*     are matched by "d", all upper (but not lower) case alphabetical 
*     characters are matched by "c", and all characters which are legal within 
*     a FITS keyword (i.e. upper case letters, digits, underscores and 
*     hyphens) are matched by "f".

*  Parameters:
*     test
*        Pointer to a null terminated string holding the keyword name to
*        be tested.
*     temp
*        Pointer to a null terminated string holding the template.
*     maxfld
*        The maximum number of integer field values which should be
*        returned in "fields".
*     fields
*        A pointer to an array of at least "maxfld" integers. This is
*        returned holding the values of any integer fields specified
*        in the template. The values are extracted from the test string,
*        and stored in the order they appear in the template string.
*     nfld
*        Pointer to a location at which is returned the total number of 
*        integer fields in the test string. This may be more than the 
*        number returned in "fields" if "maxfld" is smaller than "*nfld".
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     Zero is returned if the test string does not match the template
*     string, and one is returned if it does.

*/

/* Local Variables: */
   char type;             /* Field type specifier */
   const char *a;         /* Pointer to next test character */
   const char *b;         /* Pointer to next template character */
   int extend;            /* Can the width of the first field be extended? */
   int i;                 /* Field index */
   int match;             /* Does "test" match "temp"? */ 
   int nfret;             /* No. of fields returned */
   int tmp;               /* Field value */
   static char fmt[ 10 ]; /* Format specifier for reading an integer field */
   static const char *template; /* Pointer to start of template */
   static int *pa;        /* Pointer to first returned field value */
   static int *pb;        /* Pointer to last returned field value */
   static int na;         /* No. of characters read from the test string */
   static int nb;         /* No. of characters read from the template string */
   static int nentry = 0; /* Number of recursive entries into Match */

/* Check global status. */
   if( !astOK ) return 0;

/* On the first entry to this function, indicate that no integer fields 
   have yet been returned, and save a pointer to the start of the template
   string. */
   if( !nentry ) {
      *nfld = 0;
      template = temp;
   }

/* Increment the number of entries into this function. */
   nentry++;

/* Initialise pointers to the start of each string. */
   a = test;
   b = temp;

/* Initialise the returned flag to indicate that the two strings do not
   match. */
   match = 0;

/* Check that the initial part of the test string can match the first 
   field in the template. */
   if( MatchFront( a, b, &type, &extend, &na, &nb, method, class, template ) ){

/* If it does, increment the pointers to skip over the characters 
   used up in the comparison. */
      a += na;
      b += nb;

/* If the ends of both strings have been reached, they match. */
      if( *a == 0 && *b == 0 ){
         match = 1;

/* Otherwise, if the end of the template has been reached but there are 
   still characters to be read from the test string, we could still have 
   a match if all the remaining test characters match an extandable field. */
      } else if( *b == 0 && *a != 0 && extend ){

/* Loop until all the matching characters have been read from the end of
   the test string. */
         while( *a != 0 && MatchChar( *a, type, method, class, template ) ) a++;

/* If we reached the end of the test string, we have a match. */
         if( *a == 0 ) match = 1;

/* Otherwise, if the end of neither string has been reached, we need to carry
   on checking the remaining fields. */
      } else if( *a != 0 && *b != 0 ){

/* Call this function recursively to see if the remainder of the
   strings match. */
         if( Match( a, b, maxfld, fields, nfld, method, class ) ){
            match = 1;

/* If the remainder of the strings do not match, we may be able to make 
   them match by using up some extra test characters on the first field.
   This can only be done if the first field has an unspecified field width,
   and if the next test character if of a type which matches the first
   field in the template. */
         } else if( extend ){

/* Loop until all the suitable characters have been read from the 
   test string. Break out of the loop early if we find a field width
   which results in the whole string matching. */
            while( MatchChar( *a, type, method, class, template ) ){
               a++;

               if( Match( a, b, maxfld, fields, nfld, method, class ) ){
                  match = 1;
                  break;
               }

            }
         
         }
      
      }
   
   }

/* If the strings match and the leading field is an integer, decode
   the field and store it in the supplied array (if there is room). */
   if( match && type == 'd' && a > test ){
      if( *nfld < maxfld ){
         sprintf( fmt, "%%%dd", a - test );
         sscanf( test, fmt, fields + *nfld );
      }
      (*nfld)++;
   }

/* Decrement the number of entries into this function. */
   nentry--;

/* If we are leaving this function for the last time, reverse the
   order of the returned integer fields so that they are returned
   in the same order that they occur in the template. */
   if( !nentry ){
      nfret = ( *nfld < maxfld ) ? (*nfld) : maxfld;
      pa = fields;
      pb = fields + nfret - 1;
      for( i = 0; i < nfret/2; i++ ){
         tmp = *pa;
         *(pa++) = *pb;
         *(pb--) = tmp;
      }
   }

/* Return the result. */
   return match;

}

static int MatchChar( char test, char type, const char *method, 
                      const char *class, const char *template ){
/*
*  Name:
*     MatchChar

*  Purpose:
*     See if a given character is of a specified type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int MatchChar( char test, char type, const char *method, 
*                    const char *class, const char *template )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function checks that the supplied test character belongs
*     to the set of characters specified by the parameter "type".

*  Parameters:
*     test
*        The character to test.
*     type
*        The character specifying the set of acceptable characters. This 
*        should be one of the field type characters accepted by function 
*        Match (e.g. "d", "c" or "f").
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.
*     template
*        Pointer to the start of the whole template string, for use in error
*        messages.

*  Returned Value:
*     Zero is returned if the test character does not belongs to the 
*     specified character set, and one is returned if it does.

*  Notes:
*     -  An error is reported if the type specifier is not legal.
*     -  Zero is returned if an error has already occurred, or if ths
*     function fails for any reason.

*/

/* Local Variables: */
   int ret;            /* Returned flag */

/* Check global status. */
   if( !astOK ) return 0;

/* Check for "d" specifiers (digits). */
   if( type == 'd' ){
      ret = isdigit( (int) test );

/* Check for "c" specifiers (upper case letters). */
   } else if( type == 'c' ){
      ret = isupper( (int) test );

/* Check for "s" specifiers (any legal FITS keyword character). */
   } else if( type == 'f' ){
      ret = isFits( (int) test );

/* Report an error for any other specifier. */      
   } else {
      ret = 0;
      astError( AST__BDFMT, "%s(%s): Illegal field type or width "
                "specifier '%c' found in filter template '%s'.", 
                method, class, type, template );
   }

/* Return the answer. */
   return ret;

}

static int MatchFront( const char *test, const char *temp, char *type, 
                       int *extend, int *ntest, int *ntemp, 
                       const char *method, const char *class,
                       const char *template ){
/*
*  Name:
*     MatchFront

*  Purpose:
*     Sees if the start of a test string matches the start of a template.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int MatchFront( const char *test, const char *temp, char *type, 
*                     int *extend, int *ntest, int *ntemp, 
*                     const char *method, const char *class,
*                     const char *template )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function looks for a match between the first field in the 
*     template string and the string at the start of the test string,
*     using the syntax described in function Match.

*  Parameters:
*     test
*        Pointer to a null terminated string holding the keyword name to
*        be tested.
*     temp
*        Pointer to a null terminated string holding the template.
*     type
*        Pointer to a location at which to return a character specifying the
*        sort of field that was matched. This will be one of the legal field
*        types accepted by Match (e.g. "d", "c" or "f"), or null (zero) if
*        the first field in the template string was a literal character (i.e. 
*        did not start with a "%").
*     extend
*        Pointer to a location at which to return a flag which will be non-zero 
*        if the further test characters could be matched by the first field in 
*        the template. This will be the case if the template field only 
*        specifies a minimum number of matching characters (i.e. if the field 
*        width can be extended). For instance, "%d" can be extended, but "%1d" 
*        cannot.
*     ntest
*        Pointer to a location at which to return the number of characters 
*        matched in the test string. This will be the minimum number allowed 
*        by the template field.
*     ntemp
*        Pointer to a location at which to return the number of characters 
*        read from the template string (i.e. the number of characters in the 
*        field specification).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.
*     template
*        Pointer to the start of the whole template string, for use in error
*        messages.

*  Returned Value:
*     Zero is returned if the test string starts with fewer than the 
*     minimum number of characters matching the template string, and one 
*     is returned if it does.

*  Notes:
*     -  Zero is returned if an error has already occurred, or if this
*     function fails for any reason.

*/

/* Local Variables: */
   const char *a;     /* Pointer to next test character */
   const char *b;     /* Pointer to next template character */
   int i;             /* Character index */
   int match;         /* Does "test" match "temp"? */

/* Check global status. */
   if( !astOK ) return 0;

/* Initialise pointers to the start of each string. */
   a = test;
   b = temp;

/* Initialise the returned value to indicate that the strings match. */
   match = 1;
   
/* If the current character in the template is not a % sign, it must 
   match the current character in the test string (except for case). */
   if( *b != '%' ){
      if( toupper( (int) *b ) != toupper( (int) *a ) ) {
         match = 0;

/* If the characters match, return all the required information. */
      } else {
         *type = 0;
         *extend = 0;
         *ntest = 1;
         *ntemp = 1;
      }

/* If the current character of the template is a %, we need to match
   a field. */
   } else {
      *ntemp = 3;

/* The next character in the template string determines the field width. 
   Get the lowest number of characters which must match in the test string,
   and set a flag indicating if this lowest limit can be extended. */
      b++;         
      if( *b == '0' ){
         *ntest = 0;
         *extend = 1;

      } else if( *b == '1' ){
         *ntest = 1;
         *extend = 0;

      } else if( *b == '2' ){
         *ntest = 2;
         *extend = 0;

      } else if( *b == '3' ){
         *ntest = 3;
         *extend = 0;

      } else if( *b == '4' ){
         *ntest = 4;
         *extend = 0;

      } else if( *b == '5' ){
         *ntest = 5;
         *extend = 0;

      } else if( *b == '6' ){
         *ntest = 6;
         *extend = 0;

      } else if( *b == '7' ){
         *ntest = 7;
         *extend = 0;

      } else if( *b == '8' ){
         *ntest = 8;
         *extend = 0;

      } else if( *b == '9' ){
         *ntest = 9;
         *extend = 0;

/* If no field width was given, one or more test characters are matched.
   Step back a character so that the current character will be re-used as
   the type specifier. */
      } else {
         *ntest = 1;
         *extend = 1;      
         b--;                
         (*ntemp)--;
      }

/* The next template character gives the type of character which should 
   be matched. */
      b++;
      *type = *b;

/* Report an error if the template string ended within the field 
   specifier. */
      if( !*b ){
         match = 0;
         astError( AST__BDFMT, "%s(%s): Incomplete field specifier found "
                   "at end of filter template '%s'.", method, class, 
                   template );

/* Otherwise, check that the test string starts with the minimum allowed 
   number of characters matching the specified type. */
      } else {

         for( i = 0; i < *ntest; i++ ){
            if( !MatchChar( *a, *type, method, class, template ) ){
               match = 0;
               break;
            }
            a++;
         }

      }

   }

/* Return the answer. */
   return match;

}


static void MarkCard( AstFitsChan *this ){
/*
*  Name:
*     MarkCard

*  Purpose:
*     Mark the current card as having been read into an AST object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void MarkCard( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The current card is marked as having been read into an AST object. This
*     means that it will not be written out by astWrite.

*  Parameters:
*     this
*        Pointer to the FitsChan containing the list of cards.

*  Notes:
*     -  The card remains the current card even though it is now marked
*     as having been read.

*/

/* Return if the global error status has been set, or the current card
   is not defined. */
   if( !astOK || !this->card ) return;

/* Set the deletion flag in the current card. */
   ( (FitsCard *) this->card )->del = 1;

}

static int MoveCard( AstFitsChan *this, int move, const char *method,
                      const char *class ){
/*
*  Name:
*     MoveCard

*  Purpose:
*     Move the current card a given number of cards forward or backwards.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int MoveCard( AstFitsChan *this, int move, const char *method,
*                    const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The current card is increment by the given number of cards, ignoring
*     cards which have been read into an AST object if the external Skipping 
*     flag is set non-zero.

*  Parameters:
*     this
*        Pointer to the FitsChan containing the list of cards.
*     move
*        The number of cards by which to move the current card. Positive
*        values move towards the end-of-file. Negative values move
*        towards the start of the file (i.e. the list head).
*     method
*        Pointer to string holding name of calling method.
*     class
*        Pointer to string holding object class.

*  Returned Value:
*     The number of cards actually moved. This may not always be equal to
*     the requested number (for instance, if the end or start of the
*     FitsChan is encountered first).

*  Notes:
*     -  If the end-of-file is reached before the required number of
*     cards have been skipped, the current card is set NULL, to indicate
*     an end-of-file condition.
*     -  If the start of the file is reached before the required number of
*     cards have been skipped, the current card is left pointing to the
*     first usable card.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   FitsCard *card;         /* The current card */
   FitsCard *card0;        /* The previous non-deleted card */
   int moved;              /* The number of cards moved by so far */

/* Return if the supplied object is NULL or the FitsChan is
   empty, or zero movement is requested. */
   if( !this || !this->head || !move ) return 0;

/* Get a pointer to the current card. */
   card = (FitsCard *) this->card;

/* Initialise the number of cards moved so far. */
   moved = 0;

/* First deal with positive movements (towards the end-of-file). */
   if( move > 0 ){

/* Loop round moving on to the next card until the correct number of
   moves have been made, or the end-of-file is reached. */
      while( moved < move && card ){

/* Get a pointer to the next card in the list, reporting an error if the
   links are inconsistent. */
         card = GetLink( card, NEXT, method, class );

/* If we have moved past the last card and are now pointing back at the
   list head, then indicate that we are at end-of-file by setting the
   card pointer NULL. */
         if( (void *) card == this->head ){
            card = NULL;

/* Otherwise, increment the number of cards moved. We ignore cards which
   have been read into an AST object if the external "Skipping" flag is set. */
         } else if( card ){
            if( !card->del || !Skipping ) moved++;
         }

      }

/* Now deal with negative movements (towards the list head), so long as
   we are not currently at the list head. */
   } else if( (void *) card != this->head ){

/* If we are currently at end-of-file, replace the NULL pointer for the
   current card with a pointer to the list head. The first step backwards
   will make the last card the current card. */
      if( !card ) card = (FitsCard *) this->head;

/* Loop round until the correct number of cards have been moved. */
      while( moved < -move && card ){

/* If cards which have been read into an AST object are to be included in the 
   count of moved cards, get a pointer to the previous card in the list, 
   reporting an error if the links are inconsistent. */
         if( !Skipping ){
            card = GetLink( card, PREVIOUS, method, class );

/* If cards which have been read into an AST object are to be skipped over... */
         } else {

/* We need to find the previous card which has not been read into an AST
   object. We do not search beyond the start of the list. */
            card0 = GetLink( card, PREVIOUS, method, class );
            while( card0 && card0->del && (void *) card0 != this->head ){
               card0 = GetLink( card0, PREVIOUS, method, class );
            }

/* If no such card was found we leave the card where it is. */
            if( card0 && card0->del ) {
               break;

/* Otherwise, move back to card found above. */
            } else {
               card = card0;
            }

         }

/* Increment the number of cards moved. */
         moved++;

/* If the current card is the list head, break out of the loop. */
         if( (void *) card == this->head ) break;

      }

   }

/* Store the new current card. */
   this->card = (void *) card;

/* Return the answer. */
   return moved;

}

static void NewCard( AstFitsChan *this, const char *name, int type, 
                     const void *data, const char *comment, int del ){
/*
*  Name:
*     NewCard

*  Purpose:
*     Insert a new card in front of the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void NewCard( AstFitsChan *this, const char *name, int type, 
*                   const void *data, const char *comment, int del )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The supplied keyword name, data type and value, and comment are
*     stored in a new FitsCard structure, and this structure is
*     inserted into the circular linked list stored in the supplied
*     FitsChan. It is inserted in front of the current card.

*  Parameters:
*     this
*        Pointer to the FitsChan containing the list of cards.
*     name
*        Pointer to a string holding the keyword name of the new card.
*     type
*        An integer value representing the data type of the keyword.
*     data
*        Pointer to the data associated with the keyword. 
*     comment
*        Pointer to a null-terminated string holding a comment. 
*     del
*        Should card be marked as having been read immediately?

*  Notes:
*     -  The new card is inserted into the list in front of the current card,
*     so that the "next" link from the new card points to the current card. 
*     If the FitsChan is currently at end-of-file (indicated by a NULL
*     pointer being stored for the current card), then the card is appended
*     to the end of the list. The pointer to the current card is left 
*     unchanged.
*     -  Keyword names are converted to upper case before being stored.
*     -  Any trailing white space in a string value is not stored.
*     -  Logical values are converted to zero or one before being stored.
*     -  The "comment" and/or "data" pointers may be supplied as NULL.

*/

/* Local Variables: */
   FitsCard *new;             /* Pointer to the new card */
   FitsCard *prev;            /* Pointer to the previous card in the list */
   char *b;                   /* Pointer to next stored character */
   const char *a;             /* Pointer to next supplied character */
   int lval;                  /* Logical data value restricted to 0 or 1 */
   int nc;                    /* No. of characters to store */

/* Check the global status. */
   if( !astOK ) return;

/* Get memory to hold the new FitsCard structure. */
   new = (FitsCard *) astMalloc( sizeof( FitsCard ) );

/* Check the pointer can be used. */
   if( astOK ){
     
/* Copy the keyword name, converting to upper case. */
      a = name;
      b = new->name;
      while( *a ) *(b++) = (char) toupper( (int) *(a++) );
      *b = 0;

/* Copy the data type. */
      new->type = type;

/* Copy any data. */
      if( data ){

/* Logical values are converted to zero or one before being stored. */
         if( type == AST__LOGICAL ){
            lval = *( (int *) data ) ? 1 : 0;
            new->size = sizeof( int );
            new->data = astStore( NULL, (void *) &lval, sizeof( int ) );

/* String values have trailing white space removed before being stored. */
         } else if( type == AST__STRING ){

/* Find the number of characters upto and including the final non-space
   character. */
            nc = ChrLen( data );

/* Store the truncated string, reserving room for a terminating null. */
            new->size = (size_t)( nc + 1 );
            new->data = astStore( NULL, (void *) data, (size_t)( nc + 1 ) );

/* Terminate it. */
            ( (char *) new->data)[ nc ] = 0;

/* Other types are stored as supplied. */
         } else if( type == AST__INT ){
            new->size = sizeof( int );
            new->data = astStore( NULL, (void *) data, sizeof( int ) );

         } else if( type == AST__FLOAT ){
            new->size = sizeof( double );
            new->data = astStore( NULL, (void *) data, sizeof( double ) );

         } else if( type == AST__COMPLEXF ){
            new->size = 2*sizeof( double );
            new->data = astStore( NULL, (void *) data, 2*sizeof( double ) );

         } else if( type == AST__COMPLEXI ){
            new->size = 2*sizeof( int );
            new->data = astStore( NULL, (void *) data, 2*sizeof( int ) );

         } else {
            new->size = 0;
            new->data = NULL;
         }

      } else {
         new->size = 0;
         new->data = NULL;
      }

/* Copy any comment (excluding trailing white space). */
      if( comment && strlen( comment ) > 0){
         nc = ChrLen( comment );
         new->comment = astStore( NULL, (void *) comment, (size_t)( nc + 1 ) );
         ( (char *) new->comment)[ nc ] = 0;
      } else {
         new->comment = NULL;
      }
      
/* Set the deltion flag. */
      new->del = del;

/* Insert the copied card into the list, in front of the current card. If
   the current card is the list head, make the new card the list head. */
      if( this->card ){
         prev = ( ( FitsCard *) this->card )->prev;
         ( ( FitsCard *) this->card )->prev = new;
         new->prev = prev;

         prev->next = new;
         new->next = (FitsCard *) this->card;

         if( this->card == this->head ) this->head = (void *) new;

/* If the FitsChan is at end-of-file, append the new card to the end of
   the list (i.e. insert it just before the list head). */
      } else {

         if( this->head ){
            prev = ( (FitsCard *) this->head )->prev;
            ( (FitsCard *) this->head )->prev = new;
            new->prev = prev;

            prev->next = new;
            new->next = (FitsCard *) this->head;

/* If there are no cards in the list, start a new list. */
         } else {
            new->prev = new;
            new->next = new;
            this->head = (void *) new;
            this->card = NULL;
         }

      }

   }

/* Return. */
   return;

}

static void PreQuote( const char *value,
                      char string[ FITSCARDLEN - FITSNAMLEN - 3 ] ) {
/*
*  Name:
*     PreQuote

*  Purpose:
*     Pre-quote FITS character data.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void PreQuote( const char *value,
*                    char string[ FITSCARDLEN - FITSNAMLEN - 3 ] )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function processes a string value in such a way that it can
*     be stored as a FITS character value (associated with a keyword)
*     and later retrieved unchanged, except for possible truncation.
*
*     This pre-processing is necessary because FITS does not regard
*     trailing white space as significant, so it is lost. This
*     function adds double quote (") characters around the string if
*     it is necessary in order to prevent this loss. These quotes are
*     also added to zero-length strings and to strings that are
*     already quoted (so that the original quotes are not lost when
*     they are later un-quoted).
*
*     This function will silently truncate any string that is too long
*     to be stored as a FITS character value, but will ensure that the
*     maximum number of characters are retained, taking account of any
*     quoting required.

*  Parameters:
*     value
*        Pointer to a constant null-terminated string containing the
*        input character data to be quoted. All white space is
*        significant.
*     string
*        A character array into which the result string will be
*        written, with a terminating null. The maximum number of
*        characters from the input string that can be accommodated in
*        this is (FITSCARDLEN - FITSNAMLEN - 4), but this
*        will be reduced if quoting is necessary.

*  Notes:
*     - The UnPreQuote function should be used to reverse the effect
*     of this function on a string (apart from any truncation).
*/
   
/* Local Variables: */
   int dq;                       /* Number of double quotes needed */
   int dquotes;                  /* Final number of double quotes */
   int i;                        /* Loop counter for input characters */
   int j;                        /* Counter for output characters */
   int nc;                       /* Number of characters to be accommodated */
   int sq;                       /* Number of single quotes needed */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise, setting the default number of double quotes (which
   applies to a zero-length string) to 2. */
   dquotes = 2;
   nc = 0;
   sq = 0;

/* Loop to consider each input character to see if it will fit into
   the result string. */
   for ( i = 0; value[ i ]; i++ ) {

/* If a single quote character is to be included, count it. When the
   string is encoded as FITS character data, these quotes will be
   doubled, so will increase the overall string length by one. */
      if ( value[ i ] == '\'' ) sq++;

/* See how many double quotes are needed around the string (0 or
   2). These are needed if there is trailing white space that needs
   protecting (this is not significant in FITS and will be removed),
   or if the string already has quotes at either end (in which case an
   extra set is needed to prevent the original ones being removed when
   it is later un-quoted). Note we do not need to double existing
   double quote characters within the string, because the position of
   the ends of the string are known (from the quoting supplied by
   FITS) so only the first and last characters need be inspected when
   un-quoting the string.

   In assessing the number of double quotes, assume the string will be
   truncated after the current character. */
      dq = ( isspace( value[ i ] ) ||
             ( ( value[ 0 ] == '"' ) && ( value[ i ] == '"' ) ) ) ? 2 : 0;

/* See if the length of the resulting string, including the current
   character and all necessary quotes, is too long. If so, give up
   here. */
      if ( ( nc + 1 + dq + sq ) >
           ( FITSCARDLEN - FITSNAMLEN - 4 ) ) break;

/* If the string is not too long, accept the character and note the
   number of double quotes needed. */
      nc = i + 1;
      dquotes = dq;
   }

/* If double quotes are needed, insert the opening quote into the
   output string. */
   j = 0;
   if ( dquotes ) string[ j++ ] = '"';

/* Follow this with the maximum number of input string characters that
   can be accommodated. */
   for ( i = 0; i < nc; i++ ) string[ j++ ] = value[ i ];

/* Append the closing quote if necessary and terminate the output
   string. */
   if ( dquotes ) string[ j++ ] = '"';
   string[ j ] = '\0';
}

static void PutFits( AstFitsChan *this, const char card[ FITSCARDLEN + 1 ], 
                     int overwrite ){
/*
*++
*  Name:
c     astPutFits
f     AST_PUTFITS

*  Purpose:
*     Store a FITS header card in a FitsChan.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "fitschan.h"
c     void astPutFits( AstFitsChan *this, const char card[ 80 ],
c                      int overwrite )
f     CALL AST_PUTFITS( THIS, CARD, OVERWRITE, STATUS )

*  Class Membership:
*     FitsChan method.

*  Description:
c     This function stores a FITS header card in a FitsChan. The card
f     This routine stores a FITS header card in a FitsChan. The card
*     is either inserted before the current card (identified by the
*     Card attribute), or over-writes the current card, as required.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsChan.
c     card
f     CARD = CHARACTER * ( 80 ) (Given)
c        Pointer to a possibly null-terminated character string
c        containing the FITS card to be stored. No more than 80
c        characters will be used from this string (or fewer if a null
c        occurs earlier).
f        A character string string containing the FITS card to be
f        stored. No more than 80 characters will be used from this
f        string.
c     overwrite
f     OVERWRITE = LOGICAL (Given)
c        If this value is zero, the new card is inserted in front of
f        If this value is .FALSE., the new card is inserted in front of
*        the current card in the FitsChan (as identified by the
c        initial value of the Card attribute). If it is non-zero, the
f        initial value of the Card attribute). If it is .TRUE., the
*        new card replaces the current card. In either case, the Card
*        attribute is then incremented by one so that it subsequently
*        identifies the card following the one stored.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the Card attribute initially points at the "end-of-file"
*     (i.e. exceeds the number of cards in the FitsChan), then the new
*     card is appended as the last card in the FitsChan.
*     - An error will result if the supplied string cannot be interpreted
*     as a FITS header card.
*--
*/
/* Local Variables: */
   char *comment;         /* The keyword comment */
   char *name;            /* The keyword name */
   char *value;           /* The keyword value */
   const char *class;     /* Object class */
   const char *method;    /* Current method */
   double cfval[2];       /* Complex floating point keyword value */
   double fval;           /* floating point keyword value */
   int cival[2];          /* Complex integer keyword value */
   int ival;              /* Integer keyword value */
   int len;               /* No. of characters to read from the value string */
   int nc;                /* No. of characters read from value string */
   int type;              /* Keyword data type */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the current method, and the class of the supplied object for use 
   in error messages.*/
   method = "astPutFits";
   class = astGetClass( this );

/* Split the supplied card up into name, value and commment strings, and
   get pointers to local copies of them. The data type associated with the
   keyword is returned. */
   type = astSplit( card, &name, &value, &comment, method, class );

/* Check that the pointers can be used. */
   if( astOK ){

/* Initialise the number of characters read from the value string. */
      nc = 0;

/* Store the number of characters in the value string. */
      len = strlen( value );

/* Read and store floating point values from the value string. NB, this
   list is roughly in the order of descreasing frequency of use (i.e.
   most FITS keywords are simple floating point values, the next most
   common are strings, etc). */
      if( type == AST__FLOAT ){
         if( 1 == sscanf( value, " %lf %n", &fval, &nc ) && nc >= len ){
            astFitsSetF( this, name, fval, comment, overwrite );
         } else {
            astError( AST__BDFTS, "%s(%s): Unable to read a floating point "
                      "FITS keyword value.", method, class );
         }

/* Read and store string values from the value string. */
      } else if( type == AST__STRING ){
         astFitsSetS( this, name, value, comment, overwrite );

/* Store comment card. */
      } else if( type == AST__COMMENT ){
         astFitsSetCom( this, name, comment, overwrite );

/* Read and store integer values from the value string. */
      } else if( type == AST__INT ){
         if( 1 == sscanf( value, " %d %n", &ival, &nc ) && nc >= len ){
            astFitsSetI( this, name, ival, comment, overwrite );
         } else {
            astError( AST__BDFTS, "%s(%s): Unable to read an integer FITS "
                      "keyword value.", method, class );
         }

/* Read and store logical values from the value string. */
      } else if( type == AST__LOGICAL ){
         astFitsSetL( this, name, (*value == 'T'), comment, overwrite );

/* Read and store complex floating point values from the value string. */
      } else if( type == AST__COMPLEXF ){
         if( 2 == sscanf( value, " %lf %lf %n", cfval, cfval + 1, &nc ) && 
             nc >= len ){
            astFitsSetCF( this, name, cfval, comment, overwrite );
         } else {
            astError( AST__BDFTS, "%s(%s): Unable to read a complex pair "
                      "of floating point FITS keyword values.", method, class );
         }

/* Read and store complex integer values from the value string. */
      } else if( type == AST__COMPLEXI ){
         if( 2 == sscanf( value, " %d %d %n", cival, cival + 1, &nc ) && 
             nc >= len ){
            astFitsSetCI( this, name, cival, comment, overwrite );
         } else {
            astError( AST__BDFTS, "%s(%s): Unable to read a complex pair "
                      "of integer FITS keyword values.", method, class );
         }

/* Report an error for any other type. */
      } else {
         astError( AST__INTER, "%s: AST internal programming error - "
                   "FITS data-type '%d' not yet supported.", method, type );
      }

/* Give a context message if an error occurred. */
      if( !astOK ){
         astError( astStatus, "%s(%s): Unable to store the following FITS "
                   "header card:\n%s\n", method, class, card );
      }

   }

/* Free the memory used to hold the keyword name, comment and value 
   strings. */
   (void) astFree( (void *) name );
   (void) astFree( (void *) comment );
   (void) astFree( (void *) value );

}

static AstObject *Read( AstChannel *this_channel ) {
/*
*  Name:
*     Read

*  Purpose:
*     Read an Object from a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     AstObject *Read( AstChannel *this_channel ) 

*  Class Membership:
*     FitsChan member function (over-rides the astRead method
*     inherited from the Channel class).

*  Description:
*     This function reads an Object from a FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to the new Object. The class to which this will
*     belong is determined by the input data, so is not known in
*     advance.

*/

/* Local Variables: */
   AstObject *new;               /* Pointer to returned Object */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   int encode;                   /* The encoding scheme */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Get the encoding scheme used by the FitsChan. */
   encode = astGetEncoding( this );

/* If we are reading from a FitsChan in which AST objects are encoded using
   native AST-specific keywords, use the Read method inherited from the
   Channel class. */
   if( encode == NATIVE_ENCODING ){
      new = (*parent_read)( this_channel );

/* If we are reading from a FitsChan in which AST objects are encoded using
   FITS World Coordinate System (WCS) keywords (or the IRAF equivalent
   which uses a CD matrix instead of a PC matrix), use the ReadWcs private
   function to read a FrameSet (the only class of Object which can be
   stored using FITS-WCS encoding). */
   } else if( encode == FITSWCS_ENCODING || encode == FITSIRAF_ENCODING ){
      new = (AstObject *) ReadWcs( this );
   
/* If we are reading from a FitsChan in which AST objects are encoded using
   STScI DSS keywords, use the ReadDSS private function to read a FrameSet 
   (the only class of Object which can be stored using DSS encoding). */
   } else if( encode == DSS_ENCODING ){
      new = (AstObject *) ReadDSS( this );
   
/* Report an error if the encoding system is not known. */
   } else if( astOK ){
      astError( AST__INTER, "astRead(%s): AST internal programming error - "
                "FITS encoding scheme %d not yet supported by the FitsChan "
                "class.", astGetClass( this ) );
   }

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* Return the pointer to the new Object. */
   return new;
}

static void ReadFromSource( AstFitsChan *this ){
/*
*  Name:
*     ReadFromSource

*  Purpose:
*     Fill the FitsChan by reading cards from the source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void ReadFromSource( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The source function specified when the FitsChan was created is
*     called repeatedly until it returns a NULL pointer. The string
*     returned by each such call is assumed to be a FITS header card,
*     and is stored in the FitsChan using astPutFits.
*
*     If no source function was provided, the FitsChan is left as supplied.
*     This is different to a standard Channel, which tries to read data
*     from standard input if no source function is provided.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Notes:
*     -  The new cards are appended to the end of the FitsChan.
*     -  The first of the new cards is made the current card on exit. If no
*     source function is supplied, the current card is left unchanged.

*/

/* Local Variables: */
   const char *card;            /* Pointer to externally-read header card */
   int icard;                   /* Current card index on entry */

/* Check the global status. */
   if( !astOK ) return;

/* Only proceed if source function and wrapper were supplied when the FitsChan 
   was created. */
   if( this->source && this->source_wrap ){

/* Ensure the FitsChan is at end-of-file. This will result in the 
   new cards being appended to the end of the FitsChan. */
      astSetCard( this, INT_MAX );

/* Store the current card index. */
      icard = astGetCard( this );

/* Obtain the first header card from the source function. */
      card = ( *this->source_wrap )( this->source );

/* Loop until a NULL pointer is returned by the source function, or an
   error occurs. */
      while( card && astOK ){

/* Store the card in the FitsChan. */
         astPutFits( this, card, 0 );

/* Free the memory holding the header card. */
         card = (char *) astFree( (void *) card );

/* Obtain the next header card. */
         card = ( *this->source_wrap )( this->source );

      }

/* Set the current card index so that the first of the new cards will be the 
   next card to be read from the FitsChan. */
      astSetCard( this, icard );

   }

}

static AstFrameSet *ReadDSS( AstFitsChan *this ){
/*
*  Name:
*     ReadDSS

*  Purpose:
*     Read a FrameSet from a DSS encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFrameSet *ReadDSS( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function constructs a FrameSet on the basis of the STScI DSS
*     keywords contained in the supplied FitsChan. The FrameSet includes 
*     2 Frames; a simple Frame describing pixel coordinates, and a 
*     SkyFrame describing equatorial coordinates.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to the new FrameSet. 

*  Notes:
*     -  This function returns without action unless the FitsChan is at the
*     start-of-file.
*     -  The pixel Frame is given a title of "Pixel Coordinates", and
*     each axis in the pixel Frame is given a label of the form "Pixel
*     axis <n>", where <n> is the axis index (starting at one).
*     -  On exit, the pixel Frame is the base Frame, and the equatorial
*     SkyFrame is the current Frame.
*     -  The FitsChan is left at end-of-file on exit.

*/

/* Local Variables: */
   AstFrame *pframe;             /* Pixel Frame pointer */
   AstSkyFrame *cframe;          /* Celestial coordinate Frame pointer */
   AstFitsChan *fits;            /* FitsChan containing DssMap keywords */
   AstFrameSet *new;             /* Pointer to returned FrameSet */
   AstMapping *mapping;          /* Mapping pointer */
   char buff[ 20 ];              /* Buffer for axis label */
   const char *method;           /* Pointer to string holding calling method */
   const char *class;            /* Pointer to string holding object class */
   const char *name;             /* Keyword name */
   int axis;                     /* Axis index */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Store the calling method, and object class. */
   method = "astRead";
   class = astGetClass( this );

/* Only proceed if the FitsChan is at start-of-file, and if it contains
   the mandatory PLTRAH keyword. */
   if( !astTestCard( this ) && astKeyFields( this, "PLTRAH", 0, NULL, NULL ) ){

/* Create a Frame describing the pixel coordinate system. Give it the
   Domain name GRID. */
      pframe = astFrame( 2, "Title=Pixel Coordinates,Domain=GRID" );

/* Store labels for each pixel axis. */
      if( astOK ){
         for( axis = 1; axis <= 2; axis++ ){
            sprintf( buff, "Pixel axis %d", axis );
            astSetLabel( pframe, axis - 1, buff );
         }
      }
   
/* Create a Mapping conecting the pixel and celestial Frames. */
      mapping = (AstMapping *) astDssMap( this, "" );

/* Create a SkyFrame describing the celestial coordinate system. */
      cframe = DSSFrame( this, method, class );

/* Create the FrameSet initially holding just the pixel coordinate frame
   (this becomes the base Frame). */
      new = astFrameSet( pframe, "" );

/* Add the SkyFrame into the FrameSet (it becomes the current Frame). */
      astAddFrame( new, astGetBase( new ), mapping, cframe );

/* Get a FitsChan describing the DssMap. */
      fits = astDssFits( mapping );

/* Mark cards in the supplied FitsChan as having been read if they are
   present in the FitsChan produced by astDssFits. */
      astClearCard( fits );
      astClearCard( this );
      while( !astFitsEof( fits ) ){
         name = CardName( fits );
         if( SearchCard( this, name, method, class ) ) MarkCard( this );
         MoveCard( fits, 1, method, class );
      }      

/* Annul the FitsChan created by astDssFits. */
      fits = astAnnul( fits );

/* Annul the Frames and Mappings. */
      pframe = astAnnul( pframe );
      cframe = astAnnul( cframe );
      mapping = astAnnul( mapping );

/* Set the supplied FitsChan to end-of-file. */
      astSetCard( this, INT_MAX );

/* If an error occurred, clean up by deleting the new FrameSet and
   return a NULL pointer. */
      if ( !astOK ) new = astDelete( new );

   }

/* Return the pointer to the new Object. */
   return new;
}

static AstFrameSet *ReadWcs( AstFitsChan *this ){
/*
*  Name:
*     ReadWcs

*  Purpose:
*     Read a FrameSet from a FITS-WCS encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFrameSet *ReadWcs( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function constructs a FrameSet on the basis of the FITS WCS
*     keywords contained in the supplied FitsChan, describing the system of
*     coordinate Frames defined by the FITS world Coordinate System.  

*     The FrameSet includes a Frame describing "actual pixel coordinates", 
*     and a set of Frames describing physical coordinates (one FRAME for 
*     every possible combination of axis descriptions in the FITS header).
*     Celestial longitude and latitude axes will be stored in SkyFrames,
*     and all other axes will be stored in simple Frames. If the FITS
*     header describes a combination of celestial and non-celestial axes,
*     then the physical coordinate system will be described by a CmpFrame,
*     holding a SkyFrame for the celestial axes, and a simple Frame for
*     the others. Non-celestial physical axes will refer to absolute values,
*     formed by adding the reference point value onto the "Relative Physical 
*     Coordinates" value.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to the new FrameSet. 

*  Deficiencies:
*     -  The phsyical coordinate system may not contain more than one pair 
*     of celestial longitude/latitude axes.

*  Notes:
*     -  This function returns without action unless the FitsChan is at the
*     start-of-file.
*     -  The pixel Frame is given a title of "Pixel Coordinates", and
*     each axis in the pixel Frame is given a label of the form "Pixel
*     axis <n>", where <n> is the axis index (starting at one).
*     -  The FITS CTYPE keyword values are used to set the labels for any
*     non-celestial axes in the physical coordinate Frames, and the FITS 
*     CUNIT keywords are used to set the corresponding units strings.
*     -  On exit, the pixel Frame is the base Frame, and the physical
*     Frame derived from the primary axis descriptions is the current Frame.
*     -  The FitsChan is left at end-of-file on exit.
*/

/* Local Variables: */
   AstFrame *frame;              /* Frame pointer */
   AstFrameSet *new;             /* Pointer to returned FrameSet */
   AstMapping *mapping;          /* Mapping pointer */
   FitsStore store;              /* Storage for intermediate values */
   char buff[ 20 ];              /* Buffer for axis label */
   const char *method;           /* Pointer to string holding calling method */
   const char *class;            /* Pointer to string holding object class */
   int *desc;                    /* Pointer to current descriptions */
   int *mxdesc;                  /* Pointer to upper description limits */
   int axis;                     /* Axis index */
   int naxis;                    /* No. of axes */
   int old_skipping;             /* Original value of external variable Skipping */
   int physical;                 /* Index of primary physical Frame within FrameSet */
   int pixel;                    /* Index of actual pixel Frame within FrameSet */
   int prim;                     /* Are primary axis descriptions being used? */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Store the calling method, and object class. */
   method = "astRead";
   class = astGetClass( this );

/* Find the number of axes described by the FITS header. Report an error
   if no axes are found. */
   naxis = GetNaxis( this, FITSWCS_ENCODING );

/* Only proceed if the FitsChan is at start-of-file, and there are some
   axes. */
   if( naxis && !astTestCard( this ) && astOK ){

/* We do not skip over cards which have already been read. This is because 
   some keywords such as EQUINOX, RADECSYS, etc, are read several times by the 
   StoreFits function (once for each set of axis descriptions), and if we
   did not switch skipping off, then the keywords would not be found on
   the second and subsequent readings. We have just checked that the
   CRVAL1 keyword is available (i.e. has not yet been used), so we can be
   sure we are not re-reading an Object which has already been read. */
      old_skipping = Skipping;
      Skipping = 0;

/* Create a Frame describing the pixel coordinate system. This is known as 
   the "actual pixel coordinate system" in FITS WCS documentation. Give it
   the Domain GRID. */
      frame = astFrame( naxis, "Title=Pixel Coordinates,Domain=GRID" );

/* Store labels for each pixel axis. */
      if( astOK ){
         for( axis = 1; axis <= naxis; axis++ ){
            sprintf( buff, "Pixel axis %d", axis );
            astSetLabel( frame, axis - 1, buff );
         }
      }
   
/* Create the FrameSet initially holding just the pixel coordinate frame
   (this becomes the base Frame). */
      new = astFrameSet( frame, "" );

/* Annul the pointer to the pixel coordinate Frame. */
      frame = astAnnul( frame );

/* Get the index of the "actual pixel" Frame in the FrameSet. */
      pixel = astGetCurrent( new );

/* Allocate memory to store the maximum and current description indices
   for each axis. */
      mxdesc = (int *) astMalloc( sizeof(int)*naxis );
      desc = (int *) astMalloc( sizeof(int)*naxis );

/* Set the current axis description index of the first axis to -1 to indicate
   to function GetDesc that the arrays need initialising. */
      if( desc ) desc[ 0 ] = -1;

/* Loop round setting up Frames to describe all possible sets of axis
   descriptions. */
      physical = AST__NOFRAME;
      while( astOK ){

/* Get the axis description indicies for the next combination of axis 
   descriptions. Break out of the loop when all have been done. This call
   skips over any combinations which involve descriptions for which the
   relevant keywords don't exist within the FitsChan. */
         if( !GetDesc( this, naxis, desc, mxdesc, &prim ) ) break;

/* Identify and store the required FITS keyword values. The results of this 
   preprocesing are stored in "store". Zero is returned if the set of axis 
   descriptions could not be used. */
         if( StoreFits( this, prim, naxis, desc, &store, method, class ) ){

/* Get a Mapping between pixel coordinates and physical coordinates, using the 
   current set of axis descriptions. */
            mapping = WcsMapping( &store, method, class );

/* Get a Frame describing the physical coordinate system. */
            frame = WcsFrame( &store, method, class );

/* Add the Frame into the FrameSet, and annul the mapping and frame. */
            astAddFrame( new, pixel, mapping, frame );
            frame = astAnnul( frame );
            mapping = astAnnul( mapping );

         }

/* Clean up the structure used to store intermediate values. */
         CleanFits( &store );

/* If we have just done the primary axis description, record the index of
   the physical coordinate Frame within the FrameSet. */
         if( physical == AST__NOFRAME ) physical = astGetCurrent( new );
      }

/* Ensure the pixel Frame is the base Frame. */
      astSetBase( new, pixel );

/* Make the primary physical coordinate Frame the current Frame. */
      if( physical != AST__NOFRAME ) astSetCurrent( new, physical );

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
      Skipping = old_skipping;

/* Ensure the FitsChan is now at end-of-file. */
      astSetCard( this, INT_MAX );

/* Release the memory allocated earlier. */
      mxdesc = (int *) astFree( (void *) mxdesc );
      desc = (int *) astFree( (void *) desc );

/* If an error occurred, clean up by deleting the new FrameSet and
   return a NULL pointer. */
      if ( !astOK ) new = astDelete( new );

   }

/* Return the pointer to the new Object. */
   return new;
}

static int SearchCard( AstFitsChan *this, const char *name, 
                       const char *method, const char *class ){
/*
*  Name:
*     SearchCard

*  Purpose:
*     Search the whole FitsChan for a card refering to given keyword.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int SearchCard( AstFitsChan *this, const char *name, 
*                     const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Searches the whole FitsChan for a card refering to the supplied keyword, 
*     and makes it the current card. The card following the current card is
*     checked first. If this is not the required card, then a search is
*     performed starting with the first keyword in the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a string holding the keyword name.
*     method
*        Pointer to string holding name of calling method.

*  Returned Value:
*     A value of 1 is returned if a card was found refering to the given
*     keyword. Otherwise zero is returned.

*  Notes:
*     -  If a NULL pointer is supplied for "name" then the current card
*     is left unchanged.
*     -  The current card is set to NULL (end-of-file) if no card can be
*     found for the supplied keyword.

*/

/* Local Variables: */
   int ret;              /* Was a card found? */

/* Check the global status, and supplied keyword name. */
   if( !astOK || !name ) return 0;

/* Indicate that no card has been found yet. */
   ret = 0;

/* The required card is very often the next card in the FitsChan, so check the 
   next card, and only search the entire FitsChan if the check fails. */
   MoveCard( this, 1, method, class );
   if( !astFitsEof( this ) && 
       !Ustrncmp( CardName( this ), name, FITSNAMLEN ) ){
      ret = 1;

/* If the next card is not the required card, rewind the FitsChan back to 
   the first card. */
   } else {
      astClearCard( this );

/* Attempt to find the supplied keyword, searching from the first card. */
      ret = FindKeyCard( this, name, method, class );
   } 

/* Return. */
   return ret;

}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     astSetAttrib

*  Purpose:
*     Set an attribute value for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     FitsChan member function (over-rides the astSetAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function assigns an attribute value for a FitsChan, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   const char *class;            /* Object class */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by sscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Obtain the object class. */
   class = astGetClass( this );

/* Card. */
/* ----- */
   if ( nc = 0,
        ( 1 == sscanf( setting, "card= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetCard( this, ival );

/* Encoding. */
/* --------- */
   } else if( nc = 0,
        ( 0 == sscanf( setting, "encoding=%n%*[^\n]%n", &ival, &nc ) )
        && ( nc >= len ) ) {

      nc = ChrLen( setting + ival );

      if( !Ustrncmp( setting + ival, NATIVE_STRING, nc ) ){
         astSetEncoding( this, NATIVE_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSWCS_STRING, nc ) ){
         astSetEncoding( this, FITSWCS_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSWCS_STRING2, nc ) ){
         astSetEncoding( this, FITSWCS_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSIRAF_STRING, nc ) ){
         astSetEncoding( this, FITSIRAF_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSIRAF_STRING2, nc ) ){
         astSetEncoding( this, FITSIRAF_ENCODING );

      } else if( !Ustrncmp( setting + ival, DSS_STRING, nc ) ){
         astSetEncoding( this, DSS_ENCODING );

      } else {
         astError( AST__BADAT, "astSet(%s): Unknown encoding system '%s' "
                   "requested for a %s.", class, setting + ival, class );
      }

/* FitsDigits. */
/* ----------- */
   } else if ( nc = 0,
        ( 1 == sscanf( setting, "fitsdigits= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetFitsDigits( this, ival );

#if 0
/* CDMatrix. */
/* --------- */
   } else if ( nc = 0,
        ( 1 == sscanf( setting, "cdmatrix= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetCDMatrix( this, ival );
#endif

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == sscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "ncard" ) ){
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.",
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }

}

static void SetCard( AstFitsChan *this, int icard ){
/*
*+
*  Name:
*     astSetCard

*  Purpose:
*     Set the value of the Card attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     void astSetCard( AstFitsChan *this, int icard )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function sets the value of the Card attribute for the supplied 
*     FitsChan. This is the index of the next card to be read from the
*     FitsChan. If a value of 1 or less is supplied, the first card in
*     the FitsChan will be read next. If a value greater than the number
*     of cards in the FitsChan is supplied, the FitsChan will be left in an
*     "end-of-file" condition, in which no further read operations can be
*     performed.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     icard
*        The index of the next card to read.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.

*-
*/

/* Check the supplied object. */
   if ( !this ) return;

/* Rewind the FitsChan. */
   astClearCard( this );

/* Move forward the requested number of cards. */
   MoveCard( this, icard - 1, "astSetCard", astGetClass( this ) );

/* Return. */
   return;
}

static void SetValue( AstFitsChan *this, char *keyname, void *value, 
                      int type, char *comment ){
/*
*  Name:
*     SetValue

*  Purpose:
*     Save a FITS keyword value, over-writing any existing keyword value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void SetValue( AstFitsChan *this, char *keyname, void *value, 
*                    int type, char *comment )

*  Class Membership:
*     FitsChan

*  Description:
*     This function saves a keyword value as a card in the supplied
*     FitsChan. Comment cards are always inserted in-front of the current
*     card. If the keyword is not a comment card, any existing value 
*     for the keyword is over-written with the new value (even if it is 
*     marked as having been read). Otherwise, (i.e. if it is not a comment
*     card, and no previous value exists) it is inserted in front 
*     of the current card. The current card is unchanged on exit.

*  Parameters:
*     this
*        A pointer to the FitsChan.
*     keyname
*        A pointer to a string holding the keyword name.
*     value
*        A pointer to a buffer holding the keyword value. For strings,
*        the buffer should hold a pointer to the character string.
*     type
*        The FITS data type of the supplied keyword value. 
*     comment
*        A comment to store with the keyword.

*   Notes:
*     -  Nothing is stored if a NULL pointer is supplied for "value".
*     -  If the keyword has a value of AST__BAD then nothing is stored.
*     An error is only reported in this case if the supplied keyword is 
*     a required FITS_WCS keyword.

*/

/* Local Variables: */
   FitsCard *card;        /* Pointer to original current card */
   const char *class;     /* Class name to include in error messages */
   const char *method;    /* Method name to include in error messages */
   double dval;           /* AST__FLOAT keyword value */
   int newcard;           /* Has the original current card been deleted? */
   int nfld;              /* Number of fields in keyword template */
   int old_skipping;      /* Original value of external variable Skipping */
   int stored;            /* Has the keyword been stored? */

/* Check the status and supplied value pointer. */
   if( !astOK || !value ) return;

/* Set up the method and class names for inclusion in error mesages. */
   method = "astWrite";
   class = astGetClass( this );

/* Comment card are always inserted in-front of the current card. */
   if ( type == AST__COMMENT ) {
      FitsSet( this, keyname, value, type, comment, 0 );
  
/* Otherwise... */
   } else {

/* Check that the required FITS keywords all have defined values. Report
   an error if any do not. */
      if( type == AST__FLOAT ){
         dval = *( (double *) value );
         if( dval == AST__BAD ) {
            if( Match( keyname, "CRVAL%d", 0, NULL, &nfld, method, class ) ||
                Match( keyname, "CRPIX%d", 0, NULL, &nfld, method, class ) ||
                Match( keyname, "CDELT%d", 0, NULL, &nfld, method, class ) ||
                Match( keyname, "C%1dVAL%d", 0, NULL, &nfld, method, class ) ||
                Match( keyname, "C%1dPIX%d", 0, NULL, &nfld, method, class ) ||
                Match( keyname, "C%1dELT%d", 0, NULL, &nfld, method, class ) ) {
   
               if( astOK ) {
                  astError( AST__BDFTS, "%s(%s): The required FITS keyword "
                            "\"%s\" is indeterminate.", method, class, keyname );
               }
            } 
         }
      }

/* Save a pointer to the current card. */
      card = (FitsCard *) this->card;

/* Indicate that we should not skip over cards marked as having been
   read. */
      old_skipping = Skipping;
      Skipping = 0;

/* Indicate that we have not yet stored the keyword value. */
      stored = 0;

/* Attempt to find a card refering to the supplied keyword. If one is
   found, it becomes the current card. */
      if( SearchCard( this, keyname, "astWrite", astGetClass( this ) ) ){

/* If the card which was current on entry to this function will be
   over-written, we will need to take account of this when re-instating the
   original current card. Make a note of this. */
         newcard = ( card == (FitsCard *) this->card );

/* Replace the current card with a card holding the supplied information. */
         FitsSet( this, keyname, value, type, comment, 1 );
         stored = 1;

/* If we have just replaced the original current card, make the new
   card the current card, by moving back one card. */
         if( newcard ) {
            MoveCard( this, -1, "astWrite", astGetClass( this ) );

/* If the original current card is still present in the FitsChan,
   re-instate it as the current card. */
         } else {
            this->card = (void *) card;
         }
      }

/* If the keyword has not yet been stored (i.e. if it did not exist in the 
   FitsChan), insert it in-front of the original current card. */
      if( !stored ) {
         this->card = (void *) card;
         FitsSet( this, keyname, value, type, comment, 0 );
      }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
      Skipping = old_skipping;
   }

}

static void SinkWrap( void (* sink)( const char * ), const char *line ) {
/*
*  Name:
*     SinkWrap

*  Purpose:
*     Wrapper function to invoke a C FitsChan sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     static void SinkWrap( void (* sink)( const char * ), const char *line )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function, whose single parameter is a
*        pointer to a const, null-terminated string containing the
*        text to be written, and which returns void. This is the form
*        of FitsChan sink function employed by the C language interface
*        to the AST library.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the sink function. */
   ( *sink )( line );
}

static char *SourceWrap( const char *(* source)( void ) ) {
/*
*  Name:
*     SourceWrap

*  Purpose:
*     Wrapper function to invoke a C FitsChan source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     static char *SourceWrap( const char *(* source)( void ) )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function, with no parameters, that
*        returns a pointer to a const, null-terminated string
*        containing the text that it read. This is the form of FitsChan
*        source function employed by the C language interface to the
*        AST library.

*  Returned Value:
*     A pointer to a dynamically allocated, null terminated string
*     containing a copy of the text that was read. This string must be
*     freed by the caller (using astFree) when no longer required.
*
*     A NULL pointer will be returned if there is no more input text
*     to read.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   const char *line;             /* Pointer to input line */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the source function to read the next input line and return a
   pointer to the resulting string. */
   line = ( *source )();

/* If a string was obtained, make a dynamic copy of it and save the
   resulting pointer. */
   if ( line ) result = astString( line, (int) strlen( line ) );

/* Return the result. */
   return result;
}

static int SkySys( int prim, AstFrame *skyfrm, int wcstype, FitsStore *store,
                   int axlon, int axlat ){
/*
*  Name:
*     SkySys

*  Purpose:
*     Return FITS-WCS values describing a sky coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int SkySys( int prim, AstFrame *skyfrm, int wcstype, FitsStore *store,
*                 int axlon, int axlat )

*  Class Membership:
*     FitsChan

*  Description:
*     This function sets values for the following FITS-WCS keywords
*     within the supplied FitsStore structure: CTYPE, RADECSYS, EQUINOX,
*     MJDOBS, CUNIT. The values are derived from the supplied SkyFrame
*     and WcsMap.

*  Parameters:
*     prim
*        Are primary axis descriptions being produced?
*     skyfrm
*        A pointer to the SkyFrame to be described.
*     wcstype
*        An identifier for the type of WCS projection to use.
*     store
*        A pointer to the FitsStore structure in which to store the
*        results.
*     axlon
*        The index of the longitude values within the arrays in "store".
*     axlat
*        The index of the latitude values within the arrays in "store".

*  Returned Value:
*     Are the keywords values in the FitsStore usable?

*/

/* Local Variables: */
   const char *prj_name;    /* Pointer to projection name string */
   const char *sys;         /* Celestal coordinate system */
   double ep;               /* Epoch of observation */
   double eq;               /* Epoch of reference equinox */
   int isys;                /* Celestial coordinate system */
   int radecsys;            /* RA/DEC reference frame */
   int ret;                 /* Returned flag */

/* Check the status. */
   if( !astOK ) return 0;

/* Initialise */
   ret = 0;

/* Get the equinox, epoch of observation, and system of the SkyFrame. */
   eq = astGetEquinox( skyfrm );               
   ep = astTestEpoch( skyfrm ) ? astGetEpoch( skyfrm ) : AST__BAD;
   sys = astGetC( skyfrm, "system" );

/* Convert the equinox to a Julian or Besselian epoch. Also get the
   reference frame and standard system. */
   if( !Ustrcmp( sys, "FK4") ){
      eq = slaEpb( eq );
      radecsys = FK4;
      isys = RADEC;

   } else if( !Ustrcmp( sys, "FK4_NO_E") ){
      eq = slaEpb( eq );
      radecsys = FK4NOE;
      isys = RADEC;

   } else if( !Ustrcmp( sys, "FK5" ) ){
      eq = slaEpj( eq );
      radecsys = FK5;
      isys = RADEC;

   } else if( !Ustrcmp( sys, "GAPPT" ) ||
              !Ustrcmp( sys, "Apparent" ) ||
              !Ustrcmp( sys, "Geocentric" ) ){
      eq = AST__BAD;
      radecsys = GAPPT;
      isys = RADEC;

   } else if( !Ustrcmp( sys, "Ecliptic" ) ){
      eq = ( eq < slaEpj2d( 1984.0 ) ) ? slaEpb( eq ) : slaEpj( eq );
      radecsys = NORADEC;
      isys = ECLIP;

   } else if( !Ustrcmp( sys, "Galactic" ) ){
      eq = AST__BAD;
      radecsys = NORADEC;
      isys = GALAC;

   } else if( !Ustrcmp( sys, "Supergalactic" ) ){
      eq = AST__BAD;
      radecsys = NORADEC;
      isys = SUPER;

   } else {
      eq = AST__BAD;
      radecsys = NORADEC;
      isys = NOCEL;
   }

/* Store these values. If we are producing secondary axes, then the
   values found above must match the values already stored in the "store"
   to be usable.  */
   if( prim ){
      ret = 1;
      store->equinox = eq;
      store->radecsys = radecsys;
      store->mjdobs = ep;

   } else {
      if( EQUAL( store->equinox, eq ) &&
                 store->radecsys == eq &&
          EQUAL( store->mjdobs, ep ) ){
         ret = 1;
      }
   }

/* Only proceed if we have usable values */
   if( ret ) {

/* The first 4 characters in CTYPE are determined by the celestial coordinate 
   system and the second 4 by the projection type. First ensure memory is
   allocated to hold the CTYPE strings. */
      store->ctype[ axlat ] = (char *) astGrow( (void *) store->ctype[ axlat ], 1, 9 );
      store->ctype[ axlon ] = (char *) astGrow( (void *) store->ctype[ axlon ], 1, 9 );

/* Store the longitude and latitude strings. */
      if( astOK ){
         if( isys == RADEC ){
            strcpy( store->ctype[ axlon ], "RA--" );
            strcpy( store->ctype[ axlat ], "DEC-" );
   
         } else if( isys == ECLIP ){
            strcpy( store->ctype[ axlon ], "ELON-" );
            strcpy( store->ctype[ axlat ], "ELAT-" );
   
         } else if( isys == GALAC ){
            strcpy( store->ctype[ axlon ], "GLON-" );
            strcpy( store->ctype[ axlat ], "GLAT-" );
   
         } else if( isys == SUPER ){
            strcpy( store->ctype[ axlon ], "SLON-" );
            strcpy( store->ctype[ axlat ], "SLAT-" );
   
         } else {
            strcpy( store->ctype[ axlon ], "LON--" );
            strcpy( store->ctype[ axlat ], "LAT--" );
         }                  

/* Store the projection strings. */
         prj_name = astWcsPrjName( wcstype );
         strcpy( store->ctype[ axlon ] + 4, prj_name );
         strcpy( store->ctype[ axlat ] + 4, prj_name );

/* Nullify any CUNITS strings for the longitude and latitude axes (they
   always take the default value of degrees). */
         store->cunit[ axlon ] = (char *) astFree( (void *) store->cunit[ axlon ] );
         store->cunit[ axlat ] = (char *) astFree( (void *) store->cunit[ axlat ] );
      }
   }

   if( !astOK ) ret = 0;
   return ret;
}

int astSplit_( const char *card, char **name, char **value, 
               char **comment, const char *method, const char *class ){
/*
*+
*  Name:
*     astSplit

*  Purpose:
*     Extract the keyword name, value and comment from a FITS header card.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitschan.h"
*     int astSplit( const char *card, char **name, char **value, 
*                   char **comment, const char *method, const char *class  )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The name, value and comment (if present) are extracted from the
*     supplied card text and returned.

*  Parameters:
*     card
*        Pointer to a string holding the FITS header card.
*     name
*        Pointer to a location at which to return the pointer to a string 
*        holding the keyword name.
*     value
*        Pointer to a location at which to return the pointer to a string 
*        holding the keyword value. 
*     comment
*        Pointer to a location at which to return the pointer to a string 
*        holding the keyword comment.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned value:
*     -  An integer identifying the data type of the keyword value. This
*     will be one of the values AST__COMMENT, AST__INT, AST__STRING, 
*     AST__FLOAT, AST__COMPLEXI or AST__COMPLEXF defined in fitschan.h.

*  Notes:
*     -  If the keyword value is a string, then the returned value does not
*     include the delimiting quotes, and pairs of adjacent quotes within the
*     string are replaced by single quotes.
*     -  A maximum of 80 characters are read from the supplied card, so the
*     string does not need to be null terminated unless less than 80 
*     characters are to be read.
*     -  The memory holding the three strings "name", "value" and "comment" 
*     should be released when no longer needed using astFree.
*     -  NULL pointers and a data type of AST__COMMENT are returned if an 
*     error has already occurred, or if this function fails for any reason.
*-
*/

/* Local Variables: */
   char *c;                   /* Pointer to returned comment string */
   char *dd;                  /* Pointer to intermediate character */
   char *slash;               /* Pointer to comment character */
   char *v;                   /* Pointer to returned value string */
   const char *d;             /* Pointer to first comment character */
   const char *v0;            /* Pointer to first non-blank value character */
   double fi, fr;             /* Values read from value string */
   int blank_name;            /* Is keyword name blank? */
   int i;                     /* Character index */
   int ii, ir;                /* Values read from value string */
   int iopt;                  /* Index of option within list */
   int lq;                    /* Was previous character an escaping quote? */
   int len;                   /* Used length of value string */
   int nch;                   /* No. of characters used */
   int type;                  /* Keyword data type */
   size_t nc;                 /* Number of character in the supplied card */
   size_t ncc;                /* No. of characters in the comment string */
   size_t ncv;                /* No. of characters in the value string */

/* Initialise the returned pointers. */
   *name = NULL;
   *value = NULL;
   *comment = NULL;
   type = AST__COMMENT;
   
/* Check the global status. */
   if( !astOK ) return type;

/* Store the number of characters to be read from the supplied card. This
   is not allowed to be more than the length of a FITS header card.
   Trailing white space and non-printing characters such as new-line are 
   ignored. */
   nc = ChrLen( card );
   if( nc > FITSCARDLEN ) nc = FITSCARDLEN;

/* Allocate memory for a copy of the keyword name plus a terminating 
   null character. */
   *name = (char *) astMalloc( ( 1 + FITSNAMLEN )*sizeof(char) );

/* Check the pointer can be used. */
   if( astOK ){

/* Initialise the name string by filling it with spaces, and terminating it. */
      for( i = 0; i < FITSNAMLEN; i++ ) (*name)[ i ] = ' ';
      (*name)[ FITSNAMLEN ] = 0;

/* Copy the the keyword name, ensuring that no more than FITSNAMLEN (8)
   characters are copied. */
      strncpy( *name, card, ( nc > FITSNAMLEN ) ? FITSNAMLEN : nc );

/* If there is no keyword name, flag that we have a blank name which will
   be treated as a comment card. */
      if( strspn( *name, " " ) == strlen( *name ) ){
         blank_name = 1;

/* If the card contains a keyword name, replace any trailing blanks with
   nulls. */
      } else {
         blank_name = 0;
         dd = *name + strlen( *name ) - 1;
         while( *dd == ' ' ) *(dd--) = 0;
      }
      
/* Check the keyword name is legal. */
      CheckFitsName( *name, method, class );

/* Allocate memory to hold the keyword value and comment strings. */
      *value = (char *) astMalloc( sizeof(char)*( 2 + nc ) );
      *comment = (char *) astMalloc( sizeof(char)*( 1 + nc ) );

/* Check the pointers can be used. */
      if( astOK ){

/* If column 9 does not contain an equals sign, or if the keyword is
   "HISTORY", "COMMENT" or blank, then columns 9 to the end are
   comment characters, and the value string is null. */
         if( nc <= FITSNAMLEN || card[ FITSNAMLEN ] != '='
                              || !Ustrcmp( *name, "HISTORY" )
                              || !Ustrcmp( *name, "COMMENT" )
                              || blank_name ){
            (*value)[ 0 ] = 0;
            if( nc > FITSNAMLEN ){
               (void) strncpy( *comment, card + FITSNAMLEN, 
                               nc - FITSNAMLEN );
               (*comment)[ nc - FITSNAMLEN ] = 0;
            } else {
               (*comment)[ 0 ] = 0;
            }

/* Otherwise there is a value field. */
         } else {

/* Find the first non-blank character in the value string. */
            v0 = card + FITSNAMLEN + 1;
            while( (size_t)(v0 - card) < nc && 
                   isspace( (int) *v0 ) ) v0++;

/* Store pointers to the start of the returned value and comment strings. */
            v = *value;
            c = *comment;

/* If the first character in the value string is a single quote, the value is 
   a string. In this case the value ends at the first non-escaped single 
   quote. */
            if( *v0 == '\''){
               type = AST__STRING;

/* We want to copy the string value, without the delimiting quotes, to the
   returned value string. Single quotes within the string are represented
   by two adjacent quotes, so we also need to check for these and replace
   them by one quote in the returned string. First initialise a pointer
   to the first character after the opening quote, and set a flag 
   indicating that (for the purposes of identifying pairs of adjacent 
   quotes within the string) the previous character was not a quote. */
               d = v0 + 1;
               lq = 0;

/* Loop round each remaining character in the supplied card. */
               while( (size_t)(d - card) < nc ){

/* If the current character is a single quote... */
                  if( *d == '\'' ){

/* If the previous character was also a single quote then the quote does
   not mark the end of the string, but is a quote to be included literally
   in the value. Copy the quote to the returned string and clear the flag
   to indicate that the pair of adjacent quotes is now complete. */
                    if( lq ){
                       *(v++) = '\'';
                       lq = 0;

/* If the last character was not a quote, then set the flag for the next
   pass through the loop, but do not copy the quote to the returned string 
   since it will either be a quote escaping a following adjacent quote, or
   a quote to mark the end of the string. */
                    } else {
                       lq = 1;
                    }
                  
/* If the current character is not a quote... */
                  } else {

/* If the previous character was a quote, then we have found a single
   isolated quote which therefore marks the end of the string value. 
   The pointer "d" is left pointing to the first character
   after the terminating quote. */
                     if( lq ){
                        if( (size_t)( d - card ) < FITSSTCOL - 1 ) break;

/* If the last character was not a quote, copy it to the returned string. */
                     } else {
                        *(v++) = *d;
                     }
                  }
                  d++;
               }

/* Terminate the returned value string. */
               *v = 0;
               
/* Now deal with logical and numerical values. */
            } else {

/* The end of the value field is marked by the first "/". Find the number
   of characters in the value field. Pointer "d" is left pointing to the 
   first character in the comment (if any). */
               d = strchr( card, '/' );
               if( !d ){
                  ncv = nc - FITSNAMLEN - 1;
               } else {
                  ncv = (size_t)( d - card ) - FITSNAMLEN - 1;
               }

/* Copy the value string to the returned string. */
               if( ncv == 0 ){
                  *v = 0;               
               } else {
                  strncpy( v, card + FITSNAMLEN + 1, ncv );
                  v[ ncv ] = ' ';
                  v[ ncv + 1 ] = 0;
               }

/* Find the first non-blank character in the value string. */
               v0 = v;
               while( *v0 && isspace( (int) *v0 ) ) v0++;

/* See if the value string is one of the following strings (optionally
   abbreviated and case insensitive): YES, NO, TRUE, FALSE. */
               iopt = FullForm( "YES NO TRUE FALSE", v0 );

/* Return the single character "T" or "F" at the start of the value string
   if the value matches one of the above strings. */
               if( iopt == 0 || iopt == 2 ) {
                  type = AST__LOGICAL;
                  strcpy ( v, "T" );

               } else if( iopt == 1 || iopt == 3 ) {
                  type = AST__LOGICAL;
                  strcpy ( v, "F" );

/* If it does not match, see if the value is numerical. */
               } else {

/* Save the length of the value string excluding trailing blanks. */
                  len = ChrLen( v );

/* If there are no dots (decimal points) in the value... */
                  if( !strchr( v, '.' ) ){

/* First attempt to read two integers from the string (separated by white
   space). */
                     if( nch = 0, 
                         ( 2 == sscanf( v, " %d %d%n", &ir, &ii, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__COMPLEXI;

/* If that failed, attempt to read a single integer from the string. */
                     } else if( nch = 0, 
                         ( 1 == sscanf( v, " %d%n", &ir, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__INT;
                     }

/* If there are dots (decimal points) in the value... */
                  } else {

/* First attempt to read two doubles from the string (separated by white
   space). */
                     if( nch = 0, 
                         ( 2 == sscanf( v, " %lf %lf%n", &fr, &fi, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__COMPLEXF;

/* If that failed, attempt to read a single double from the string. */
                     } else if( nch = 0, 
                         ( 1 == sscanf( v, " %lf%n", &fr, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__FLOAT;
                     }

/* If both the above failed, it could be because the string contains a
   "D" exponent (which is probably valid FITS) instead of an "E" exponent.
   Replace any "D" in the string with "e" and try again. */
                     if( type == AST__COMMENT && astOK ) {

/* Replace "d" and "D" by "e" (if this doesn't produce a readable floating
   point value then the value string will not be used, so it is safe to
   do the replacement in situ). */
                        for( i = 0; i < len; i++ ) {
                           if( v[ i ] == 'd' || v[ i ] == 'D' ) v[ i ] = 'e';
                        }

/* Attempt to read two doubles from the edited string (separated by white
   space). */
                        if( nch = 0, 
                          ( 2 == sscanf( v, " %lf %lf%n", &fr, &fi, &nch ) ) &&
                          ( nch >= len ) ) {
                           type = AST__COMPLEXF;

/* If that failed, attempt to read a single double from the edited string. */
                        } else if( nch = 0, 
                            ( 1 == sscanf( v, " %lf%n", &fr, &nch ) ) &&
                            ( nch >= len ) ) {
                           type = AST__FLOAT;
                        }
                     }
                  }
               }

/* If the value type could not be determined report an error. */
               if( type == AST__COMMENT && astOK ) {
                  astError( AST__BDFTS, "%s(%s): Illegal keyword value "
                            "supplied.", method, class );
               }
            }

/* Find the number of characters in the comment. Pointer "d" should point to
   the first character following the value string. */
            if( d ){
               ncc = nc - (size_t)( d - card );
            } else {
               ncc = 0;
            }
            
/* Copy the remainder of the card to the returned comment string. */
            if( astOK && ncc > 0 ){
               strncpy( c, d, ncc );
               c[ ncc ] = 0;

/* Find the start of the comment (indicated by the first "/" after the 
   value string). */
               slash = strchr( c, '/' );

/* Temporarily terminate the string at the slash. */
               if( slash ) *slash = 0;

/* Shuffle the characters following the slash down to the
   start of the returned string. */
               if( slash ){
                  ncc -= (size_t)( slash - c ) + 1;
                  d = slash + 1;
                  for( i = 0; i < 1 + (int) ncc; i++ ) *(c++) = *(d++);
               }
               
/* If there is no comment string, return a null string. */
            } else {
               *c = 0;
            }
         }
      }
   }

/* Truncate the returned string to avoid wasting space. */
   if( *name ) *name = (char *) astRealloc( (void *) *name, strlen( *name ) + 1 );
   if( *comment ) *comment = (char *) astRealloc( (void *) *comment, strlen( *comment ) + 1 );
   if( *value ) *value = (char *) astRealloc( (void *) *value, strlen( *value ) + 1 );

/* If an error occurred, free the returned strings and issue a context
   message. */
   if( !astOK ){
      *name = (char *) astFree( (void *) *name );   
      *value = (char *) astFree( (void *) *value );   
      *comment = (char *) astFree( (void *) *comment );   
      type = AST__COMMENT;

      astError( astStatus, "%s(%s): Unable to store the following FITS "
                "header card:\n%s\n", method, class, card );
   }

/* Return the data type. */
   return type;
   
}

static int SplitMap( int naxis, AstMapping *map, AstMapping **map1, AstMapping **map2, 
                     AstMapping **map3, const char *class ){
/*
*  Name:
*     SplitMap

*  Purpose:
*     Locate a WCS projection within a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     int SplitMap( int naxis, AstMapping *map, AstMapping **map1, AstMapping **map2, 
*                   AstMapping **map3, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     The supplied Mapping is decomposed into a list of component
*     mappings to be compounded in series. This list is searched for 
*     a WcsMap. All the mappings before the first WcsMap are compounded
*     together and returned as "map1". The inverse of the WcsMap itself is 
*     returned as "map2", and any remaining Mappings are compounded together 
*     and returned as "map3". 

*  Parameters:
*     naxis
*        WcsMap's are considered if they have "naxis" axes.
*     map
*        A pointer to the Mapping from pixel to physical coordinates.
*     map1
*        A location at which to return a pointer to the Mapping from pixel 
*        to relative physical coordinates. 
*     map2
*        A location at which to return a pointer to the Mapping from relative 
*        physical coordinates to native spherical coordinates. This will
*        be an inverted WcsMap.
*     map3
*        A location at which to return a pointer to the Mapping from 
*        native spherical coordinates to physical coordinates. 
*     class
*        The object class, for inclusion in error messages.

*  Returned Value:
*     One if a WcsMap was found, zero otherwise.

*  Notes:
*     -  The returned Mappings contain independant copies of the relevant
*     components of the supplied Mapping and can be modified without
*     changing the supplied Mapping.
*     -  NULL pointers will be returned for all Mappings if no WcsMap 
*     can be found in the supplied Mapping. 
*     -  A pointer to a UnitMap will be returned for map1 if no mappings
*     exist before the WcsMap.
*     -  A pointer to a UnitMap will be returned for map3 if no mappings
*     exist after the WcsMap.
*     -  NULL pointers will be returned for all Mappings and a function
*     value of zero will be returned if an error has occurred, or if this 
*     function should fail for any reason.

*/

/* Local Variables: */
   AstMapping **map_list;  /* Mapping array pointer */
   AstMapping *temp1;      /* Intermediate Mapping */
   AstMapping *temp2;      /* Intermediate Mapping */
   int i;                  /* Index of current Mapping in list */
   int *invert_list;       /* Invert array pointer */
   int nmap;               /* Number of Mappings in the list */
   int ret;                /* Was a WcsMap found? */

/* Initialise the returned pointers. */
   *map1 = NULL;
   *map2 = NULL;
   *map3 = NULL;

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that no WcsMap can be found. */
   ret = 0;

/* Initialise dynamic arrays of Mapping pointers and associated Invert
   flags. */
   nmap = 0;
   map_list = NULL;
   invert_list = NULL;

/* Decompose the Maping into a sequence of Mappings to be applied in
   series and an associated list of Invert flags. */
   astMapList( map, 1, astGetInvert( map ), &nmap, &map_list,
               &invert_list );

/* Search the list for a WcsMap. */
   for( i = 0; i < nmap; i++ ){

/* If the current Mapping is a WcsMap with the correct number of axes, store 
   a copy of it and give it the correct Invert flag, then leave the loop. */
      temp1 = map_list[ i ];
      if( !strcmp( astGetClass( temp1 ), "WcsMap" ) &&
           astGetNin( temp1 ) == naxis ){
         *map2 = astCopy( temp1 );
         astSetInvert( *map2, invert_list[ i ] );
         break;

/* If this Mappng is not a WcsMap, add it into the returned "map1"
   mapping. */
      } else {

/* If this is the first Mapping to be looked at, take a copy of it, and
   set the invert flag. A copy is taken rather than a clone in order to leave
   the original Mapping unchanged. */
         if( !(*map1) ){
            *map1 = astCopy( map_list[ i ] );
            astSetInvert( *map1, invert_list[ i ] );

/* If a Mapping has already been assigned to "map1", then add the current
   Mapping onto the end of it, again taking a copy rather than a clone. */
         } else {
            temp1 = astCopy( map_list[ i ] );
            astSetInvert( temp1, invert_list[ i ] );

            temp2 = (AstMapping *) astCmpMap( *map1, temp1, 1, "" );
            *map1 = astAnnul( *map1 );
            temp1 = astAnnul( temp1 );
            *map1 = temp2;
           
         }

      }

   }

/* If a WcsMap was found... */
   if( *map2 ){

/* Check it's Invert flag is set (i.e it goes from relative physical to
   native spherical). */
      if( !astGetInvert( *map2 ) && astOK ) {
         astError( AST__BDFTS, "astWrite(%s): The WcsMap in the supplied "
                   "FrameSet needs to be inverted.", class );
      }

/* If no Mappings were found before the WcsMap, return a UnitMap for
   "map1". */
      if( !(*map1) ){
         *map1 = (AstMapping *) astUnitMap( astGetNin( *map2 ), "" );
      }

/* Compound all the Mappings which follow the WcsMap. */
      for( i++; i < nmap && astOK; i++ ){

         if( !(*map3) ){
            *map3 = astCopy( map_list[ i ] );
            astSetInvert( *map3, invert_list[ i ] );

         } else {
            temp1 = astCopy( map_list[ i ] );
            astSetInvert( temp1, invert_list[ i ] );

            temp2 = (AstMapping *) astCmpMap( *map3, temp1, 1, "" );
            *map3 = astAnnul( *map3 );
            temp1 = astAnnul( temp1 );
            *map3 = temp2;
           
         }

      }

/* If no Mappings were found after the WcsMap, return a UnitMap for
   "map3". */
      if( !(*map3) ){
         *map3 = (AstMapping *) astUnitMap( astGetNout( *map2 ), "" );
      }

/* Indicate that a WcsMap was found. */
      ret = 1;

   }

/* Clean up. */
/* ========= */
/* Loop to annul all the Mapping pointers in the list. */
   for ( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );

/* Free the dynamic arrays. */
   map_list = astFree( map_list );
   invert_list = astFree( invert_list );

/* If an error has occurred, attempt to annul the returned Mappings, and
   set the returned flag to indicate that no WcsMap was found. */
   if( !astOK ){
      *map1 = astAnnul( *map1 );
      *map2 = astAnnul( *map2 );
      *map3 = astAnnul( *map3 );
      ret = 0;
   }

/* Return the answer. */
   return ret;
}

static int SplitMat( int naxis, double *matrix, double *cdelt ){
/*
*  Name:
*     SplitMat

*  Purpose:
*     Factorises a single "CD"-style matrix into a diagonal CDELT matrix
*     and a "PC"-style matrix.

*  Type:
*     Private function.

*  Synopsis:
*     static int SplitMat( int naxis, double *matrix, double *cdelt )

*  Class Membership:
*     FitsChan

*  Description:
*     This function splits up the supplied CD matrix (e.g. as used in IRAF
*     FITS headers) into separate PC and CDELT matrices. The product of the 
*     returned matrices (CDELT.PC) equals the supplied CD matrix. The CDELT 
*     values are chosen so that the corresponding row of the PC matrix 
*     represents a unit vector. The signs of the CDELT values are chosen
*     so that the diagonal terms of the PC matrix are all positive.
*   
*  Parameters:
*     naxis
*        The number of axes.
*     matrix
*        A pointer to an array of naxis*naxis elements. On entry this holds
*        the "CD" matrix. On exit, it is modified to represent the "PC" 
*        matrix.
*     cdelt 
*        A pointer to an array of naxis elements. On exit this holds the CDELT
*        values for each axis (i.e. the diagonal terms of the CDELT matrix).

* Returned Value:
*     Zero is returned if any bad values are found in the supplied
*     matrix, or if an error has already occurred. One is returned otherwise.

*/

/* Local Variables: */
   int i;
   int j;
   int ok;
   double *a;
   int dineg;
   double s2;
   double cdlt;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* Assume success. */
   ok = 1;

/* Loop round every row in the matrix. Get a pointer to the first element
   in the row. */
   for( j = 0; j < naxis; j++ ){
      a = matrix + j*naxis;

/* Note the sign of the diagonal term (i.e. the j'th element) of this row. */
      dineg = ( a[ j ] < 0.0 );

/* Get the magnitude of the vector represented by this row. This is the 
   CDELT value for the row. BAD values cause the whole function to return. */
      s2 = 0.0;
      for( i = 0; i < naxis; i++ ){
         if( *a == AST__BAD )  {
            ok = 0;
            break;
         }
         s2 += (*a)*(*a);
         a++;         
      }

      if( !ok ) break;
      cdlt = sqrt( MAX( 0.0, s2 ) );

/* If the diagonal term for this row of the matrix is negative, make
   the CDELT value negative instead. This means that the diagonal term in
   the final PC matrix will be positive. */
      if( dineg ) cdlt = -cdlt;

/* Store the CDELT value. */
      cdelt[ j ] = cdlt;

/* The row of the PC matrix is obtained by dividing the original row by 
   the CDELT value. */
      a = matrix + j*naxis;
      for( i = 0; i < naxis; i++ ) {

         if( cdlt != 0.0 ){
            *a /= cdlt;
         } else {
            *a = 0.0;
         }

         a++;
      }
   }

   return ok;
 
}

static int StoreFits( AstFitsChan *this, int rep, int naxis, int *ialt,
                      FitsStore *store, const char *method, const char *class ){
/*
*  Name:
*     StoreFits

*  Purpose:
*     Stores FITS-related information in a form which is more convenient 
*     for subsequent usage.

*  Type:
*     Private function.

*  Synopsis:
*     int StoreFits( AstFitsChan *this, int rep, int naxis, int *ialt,
*                    FitsStore *store, const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This functions transfers the values of FITS keywords from the supplied
*     FitsChan object to the FitsStore structure. Only the values which 
*     are needed to create the FrameSet are stored. Errors are reported if the
*     necessary keywords have not been supplied. Angular values are converted
*     from degrees to radians. Only values for the requested axis descriptions 
*     are stored (see parameter "ialt").

*  Parameters:
*     this
*        A pointer to a FitsChan object containing values for FITS keywords.
*     rep
*        Should an error be reported if an inconsistent set of axis
*        descriptions is requested (e.g. if a celestial longitude axis is
*        found without a corresponding latitude axis, etc)? The returned
*        function value will be zero in either case.
*     naxis
*        The number of axes.
*     ialt
*        A pointer to an array of integers identifying which axis
*        description is to be used for each axis. The first value in the 
*        array relates to axis 1, the second to axis 2, etc. A value of 1
*        causes the primary axis description to be used (eg for axis 2,
*        this would be given by keywords CRVAL2, CRPIX2, etc). A value of
*        'm' (where m is larger than 1) causes the m'th axis description 
*        to be used (eg for axis 2 and m=3, this would be given by keywords 
*        C3VAL2, C3PIX2, etc). An error is reported if any of the requested 
*        axis descriptions do not exist. Any surplus values in this array 
*        are ignored.
*     store
*        A pointer to the structure in which the axis descriptions are to be 
*        stored.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if the set of axis descriptions specified
*     by "ialt" were usable. A value of zero is returned otherwise.

*  Notes:
*     -  If the CD matrix was supplied, the equivalent PC matrix is stored in
*     the output structure. The primary CDELT values are used to do the
*     conversion, irrespective of the supplied values for "ialt".
*     -  Angular values for longitude and latitude axes are converted to 
*     radians before being stored.
*     -  Longitude and latitude axes are identified. If either cannot be found,
*     an axis index of -1 is returned in store->axlat or store->axlon.
*     -  The projection and celestial coordinate system in use are identified.
*     -  An error is reported if the requested axis descriptions indicate
*     more than one longitude or latitude axis. 
*     -  An error is reported if the longitude and latitude axes refer to
*     different coordinate systems, or have different projections.
*     -  An error is reported if any of the mandatory keywords forming the 
*     axis description are missing.
*     -  A value of zero is returned if an error occurs.

*/

/* Local Variables: */
   char *ckeyval;         /* Character keyword value */
   char *matfmt2;         /* Format specifier for matrix keywords (write) */
   char *matfmt;          /* Format specifier for matrix keywords (read) */
   char *cdkey;           /* Description of CD matrix keywords being used */
   char keyname[ FITSNAMLEN + 1 ]; /* General Keyword name */
   char keyzero[ FITSNAMLEN + 1 ]; /* CmVAL Keyword name */
   char lattype;          /* Character identifying latitude system */
   char lontype;          /* Character identifying longitude system */
   char type;             /* Character identifying either system */
   double *next;          /* pointer to the next CD value */
   double keyval;         /* Keyword value */
   int cd;                /* Was a CD matrix supplied? */
   int cdeltrep;          /* Should absence of CDELT be reported? */
   int eq_j;              /* Was Julian value specified by string EQUINOX? */
   int eq_type;           /* Type of equinox keyword */
   int hi[ 2 ];           /* Largest field values */
   int iaxis;             /* Axis index */
   int idesc;             /* Description index */
   int iprj;              /* Projection parameter index */
   int jaxis;             /* Pixel axis index */
   int keylen;            /* Length of keyword name string */
   int lo[ 2 ];           /* Smallest field values */
   int newwcs;            /* Are standard FITS-WCS conventions being used? */
   int prj;               /* Projection type */
   int ret;               /* Were all axis descriptions found? */
   size_t size;           /* Required amount of memory */
   
/* Initialise the store structure. */
   store->prj = NULL;         /* Pointer to array of WCS projections */
   store->cdelt_name = NULL;  /* Pointers to CDELT keyword names */
   store->crota_name = NULL;  /* Pointers to CROTA keyword names */
   store->crpix_name = NULL;  /* Pointers to CRPIX keyword names */
   store->crval_name = NULL;  /* Pointers to CRVAL keyword names */
   store->ctype = NULL;       /* Pointer to array of CTYPE values */
   store->ctype_name = NULL;  /* Pointers to CTYPE keyword names */
   store->ctype_com = NULL;   /* Pointers to CTYPE keyword comments */
   store->cunit = NULL;       /* Pointer to array of CUNIT values */
   store->cdelt = NULL;       /* Pointer to array of CDELT values */
   store->crota = NULL;       /* Pointer to array of CROTA values */
   store->crpix = NULL;       /* Pointer to array of CRPIX values */
   store->crval = NULL;       /* Pointer to array of CRVAL values */
   store->pc = NULL;          /* Pointer to the PC matrix. */
   store->ialt = NULL;        /* Pointer to array of axis description counts */
   store->axlat = -1;         /* Index of latitude axis */
   store->axlon = -1;         /* Index of longitude axis */
   store->npar = 0;           /* No. of supplied projection parameters */
   store->naxis = 0;          /* No. of axes */
   store->equinox = 0.0;      /* Epoch of reference equinox */
   store->latpole = 0.0;      /* LATPOLE value in radians */
   store->longpole = 0.0;     /* LONGPOLE value in radians */
   store->mjdobs = 0.0;       /* Modified Julian Date of observation */
   store->julian = 1;         /* Is equinox value Beselian or Julian? */
   store->noncel = 0;         /* No. of non-celestial axes */
   store->radecsys = NORADEC; /* Flag identifying frame of reference */
   store->sys = NOCEL;   /* Flag identifying celestial coord. sys.*/

/* Check the global status. */
   if ( !astOK ) return 0;

/* Initalise the returned flag. */
   ret = 0;

/* Check a results structure has been supplied. */
   if( !store ){
      astError( AST__INTER, "StoreFits: AST internal programming error "
                "- NULL pointer supplied for results structure.");
   }

/* Check that the FITS header information has been supplied. */
   if( !this ){
      astError( AST__INTER, "StoreFits: AST internal programming error "
                "- NULL pointer supplied for FITS header structure.");
   }

/* Assume for the moment that we are using the old (AIPS) wcs
   conventions. This flag will be changed if any of the keywords introduced
   by the new conventions are found. */
   newwcs = 0;

/* See if a CD or PC matrix has been supplied. Store a template for the matrix 
   keywords to be used. */
   if( matfmt = "PC%3d%3d", astKeyFields( this, matfmt, 2, hi, lo ) ) {
      matfmt2 = "PC%.3d%.3d";
      cd = 0;
      cdeltrep = 1;
   } else if( matfmt = "CD%3d%3d", astKeyFields( this, matfmt, 2, hi, lo ) ) {
      matfmt2 = "CD%.3d%.3d";
      cd = 1;
      cdeltrep = 0;
      cdkey = "CDiiijjj";
   } else if( matfmt = "CD%1d_%1d", astKeyFields( this, matfmt, 2, hi, lo ) ) {
      matfmt2 = "CD%.1d_%.1d";
      cd = 1; 
      cdeltrep = 0;
      cdkey = "CDi_j";
   } else {
      matfmt2 = NULL;
      matfmt = NULL;
      cdeltrep = 1;
   }

/* Allocate storage for the arrays which store the description. */
   size = sizeof(double)*(size_t)naxis;
   store->crval = (double *) astMalloc( size );
   store->cdelt = (double *) astMalloc( size );
   store->crota = (double *) astMalloc( size );
   store->crpix = (double *) astMalloc( size );

   size = sizeof(char *)*(size_t)naxis;
   store->ctype = (char **) astMalloc( size );
   store->cunit = (char **) astMalloc( size );
   store->ctype_name = (char **) astMalloc( size );
   store->ctype_com = (char **) astMalloc( size );
   store->crval_name = (char **) astMalloc( size );
   store->cdelt_name = (char **) astMalloc( size );
   store->crota_name = (char **) astMalloc( size );
   store->crpix_name = (char **) astMalloc( size );

   size = sizeof(int)*(size_t)naxis;
   store->prj = (int *) astMalloc( size );
   store->ialt = (int *) astMalloc( size );

/* Check that the memory has been obtained succesfully. */
   if( astOK ){

/* Initialise all pointers to character strings so that they hold NULL. */
      for( iaxis = 0; iaxis < naxis; iaxis++ ){
         (store->ctype)[ iaxis ] = NULL;
         (store->cunit)[ iaxis ] = NULL;
         (store->ctype_com)[ iaxis ] = NULL;
         (store->ctype_name)[ iaxis ] = NULL;
         (store->crval_name)[ iaxis ] = NULL;
         (store->cdelt_name)[ iaxis ] = NULL;
         (store->crota_name)[ iaxis ] = NULL;
         (store->crpix_name)[ iaxis ] = NULL;
      }

/* Store the total number of axes. */
      store->naxis = naxis;

/* Initialise the number of non-celestial axes found to the total number 
   of axes. */
      store->noncel = naxis;
      
/* Initialise the indices of the latitude and longitude axes to indicate
   that they have not yet been found. */
      store->axlat = -1;
      store->axlon = -1;

/* Indicate that no celestial coordinate system has yet been found. */
      store->sys = NOCEL;

/* Loop round each axis (physical or pixel). */
      for( iaxis = 0; iaxis < naxis; iaxis++ ){

/* Get the axis description index requested for this axis. By default, the
   primary axis description are used. Note, "idesc" is zero-based, where-as
   the values supplied in "ialt" are not. */
         if( ialt && iaxis < naxis ){
            idesc = ialt[ iaxis ] - 1;
         } else {
            idesc = 0;
         }

/* First deal with primary axis descriptions. The corresponding keywords
   are mandatory.*/
         if( idesc == 0 ){
            ret = 1;

/* Store the CRVAL keyword value and name. Leave the loop error if it was not 
   supplied. */
            keylen = sprintf( keyname, "CRVAL%d", iaxis + 1 ) + 1;
            if( !GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 1, 
                method, class ) ) break;
            (store->crval)[ iaxis ] = keyval;
            (store->crval_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                              keylen );

/* Do the same for the CRPIX keyword. */
            keylen = sprintf( keyname, "CRPIX%d", iaxis + 1 ) + 1;
            if( !GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 1, 
                 method, class ) ) break;
            (store->crpix)[ iaxis ] = keyval;
            (store->crpix_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                              keylen );

/* Do the same for the CDELT keyword. If CDELT is not given in the
   header, only report an error if no CD matrix is given. */
            keylen = sprintf( keyname, "CDELT%d", iaxis + 1 ) + 1;
            if( cdeltrep ) {
               if( !GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval,
                                 1, method, class ) ) break;
            } else {
               if( !GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval,
                                 0, method, class ) ) {
                  keyval = 0.0;
                  keylen = sprintf( keyname, "%s", cdkey ) + 1;
               }
            }
            (store->cdelt)[ iaxis ] = keyval;
            (store->cdelt_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                              keylen );

/* Do the same for the CTYPE keyword. */
            keylen = sprintf( keyname, "CTYPE%d", iaxis + 1 ) + 1;
            if( !GetWcsValue( this, keyname, AST__STRING, (void *) &ckeyval, 1, 
                               method, class ) ) break;
            (store->ctype)[ iaxis ] = (char *) astStore( NULL, (void *) ckeyval,
                                                         strlen( ckeyval ) + 1 );
            (store->ctype_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                              keylen );

/* Store the comment for the CTYPE keyword (this may be used as the axis
   Label). */
            if( astFitsGetCom( this, NULL,  &ckeyval ) && ckeyval && ckeyval[ 0 ]){
               (store->ctype_com)[ iaxis ] = (char *) astStore( NULL, 
                                                       (void *) ckeyval,
                                                       strlen( ckeyval ) + 1 );
            }

/* Store the radians equivalent of the CROTA keyword value, and its name. 
   CROTA values are not mandatory, so do not report an error if it was not 
   supplied. Note, CROTA is always in units of degrees, whatever the axis
   type. CRVAL (etc) values are not converted to radians until the axis
   type has  been checked to see that it is a celestial coordinate axis. */
            keylen = sprintf( keyname, "CROTA%d", iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 0,
                              method, class ) ){
               (store->crota)[ iaxis ] = AST__DD2R*keyval;
            } else {
               (store->crota)[ iaxis ] = AST__BAD;
            }
            (store->crota_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                              keylen );

/* Store the CUNIT keyword value. Do not report an error if it was not 
   supplied as it is optional. */
            keylen = sprintf( keyname, "CUNIT%d", iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__STRING, (void *) &ckeyval, 0, 
                              method, class ) ){
               (store->cunit)[ iaxis ] = (char *) astStore( NULL, (void *) ckeyval,
                                                             strlen( ckeyval ) + 1 );
            } else {
               (store->cunit)[ iaxis ] = NULL;
            }

/* Now deal with the optional secondary axis descriptions. */
         } else {

/* Store the CmVAL keyword value and name. Set the returned flag to indicate 
   if it was supplied. Continue the loop even if it was not supplied so
   that we can check that none of the other keywords related to this
   description were supplied. It is not an error for NO keywords to be
   supplied for a particular axis description, but it is an error if some
   are supplied and some are not supplied.  */
            keylen = sprintf( keyzero, "C%dVAL%d", idesc + 1, iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyzero, AST__FLOAT, (void *) &keyval, 0, 
                              method, class ) ){
               newwcs = 1;
               ret = 1;
               (store->crval)[ iaxis ] = keyval;
               (store->crval_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                                  keylen );
            } else {
               ret = 0;
            }

/* See if a value has been supplied for the CmPIX keyword. */
            keylen = sprintf( keyname, "C%dPIX%d", idesc + 1, iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 0, 
                              method, class ) ){
               newwcs = 1;

/* If it was found, but the CmVAL keyword was not found, report an error
   saying that the CmVAL keyword was missing. */
               if( !ret ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyzero );
                  break;
               }

/* Store the keyword value and name. */
               (store->crpix)[ iaxis ] = keyval;
               (store->crpix_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                                  keylen );

/* If CmPIX was not found, report an error if and only if CmVAL was
   found. Otherwise, continue to check that none of the other keywords
   have been supplied. */
            } else {
               if( ret && astOK ){
                  astError( AST__NOFTS, "%(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyname );
                  break;
               }
            }

/* Do the same for the CmELT keyword. */
            keylen = sprintf( keyname, "C%dELT%d", idesc + 1, iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 0, 
                              method, class ) ){
               newwcs = 1;

               if( !ret ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyzero );
                  break;
               }

               (store->cdelt)[ iaxis ] = keyval;
               (store->cdelt_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                                  keylen );
            } else {
               if( ret && astOK ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyname );
                  break;
               }
            }

/* There is no equivalent to the CROTA keyword in the secondary axis 
   descriptions. */
            (store->crota)[ iaxis ] = AST__BAD;

/* Do the same for the CmYPE keyword. */
            keylen = sprintf( keyname, "C%dYPE%d", idesc + 1, iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__STRING, (void *) &ckeyval, 0, 
                              method, class ) ){
               newwcs = 1;

               if( !ret ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyzero );
                  break;
               }

               (store->ctype)[ iaxis ] = (char *) astStore( NULL, (void *) ckeyval,
                                                             strlen( ckeyval ) + 1 );
               (store->ctype_name)[ iaxis ] = (char *) astStore( NULL, keyname,
                                                                  keylen );

/* Store the comment for the CmYPE keyword (this may be used as the axis
   Label). */
               if( astFitsGetCom( this, NULL,  &ckeyval ) && ckeyval && ckeyval[ 0 ]){
                  (store->ctype_com)[ iaxis ] = (char *) astStore( NULL, 
                                                          (void *) ckeyval,
                                                          strlen( ckeyval ) + 1 );
               }

            } else {
               if( ret && astOK ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyname );
                  break;
               }
            }

/* Store the CmNIT keyword value. Do not report an error if it was not 
   supplied but CmVAL was supplied, as CmNIT is optional. */
            keylen = sprintf( keyname, "C%1dNIT%d", idesc + 1, iaxis + 1 ) + 1;
            if( GetWcsValue( this, keyname, AST__STRING, (void *) &ckeyval, 0, 
                              method, class ) ){
               newwcs = 1;

               if( !ret ){
                  astError( AST__NOFTS, "%s(%s): FITS keyword '%s' has not "
                            "been supplied.", method, class, keyzero );
                  break;
               }

               (store->cunit)[ iaxis ] = (char *) astStore( NULL, (void *) ckeyval,
                                                             strlen( ckeyval ) + 1 );
            } else {
               (store->cunit)[ iaxis ] = NULL;
            }

         }

/* Break out of the for-loop if the axis description was not found or an 
   error has occurred. */
         if( !ret || !astOK ) break;

/* Find and store the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. */
         prj = astWcsPrjType( store->ctype[ iaxis ] + 4 );
         (store->prj)[ iaxis ] = prj;

/* Identify celestial coordinate axes from the first 4 characters of the
   CTYPE value. In order to be a celestial coordinate axis, the CTYPE keyword 
   must include a recognised WCS projection code. */
         if( prj != AST__WCSBAD ){

/* See if this is a longitude axis (Eg if the first 4 character of CTYPE are 
   "RA--" or "xLON"). If so, store the value of "x" (or char(1) for equatorial 
   coordinates) to indicate which coordinate system is being used.  Also store 
   the index of the longitude axis. If another longitude axis has already been 
   found, report an error. Also, decrement the number of non-celestial axes.*/
             type = 0;
             if( !strncmp( store->ctype[ iaxis ], "RA--", 4 ) ){
                type = 1;
             } else if( !strncmp( store->ctype[ iaxis ] + 1, "LON", 3 ) ){
                type = *( store->ctype[ iaxis ] );
             }

             if( type ){
                (store->noncel)--;

                if( store->axlon == -1 ){
                   store->axlon = iaxis;
                   lontype = type;
                } else {
                   if( rep ) astError( AST__BDFTS, "%s(%s): FITS keywords '%s' (='%s') "
                     "and '%s' (='%s') both describe celestial longitude axes.",
                      method, class, store->ctype_name[ store->axlon ],
                      store->ctype[ store->axlon ], 
                      store->ctype_name[ iaxis ], store->ctype[ iaxis ] );
                   ret = 0;
                   break;
                }
             }

/* Do the same for the latitude axis, checking for "DEC-" and "xLAT". */
             type = 0;
             if( !strncmp( store->ctype[ iaxis ], "DEC-", 4 ) ){
                type = 1;
             } else if( !strncmp( store->ctype[ iaxis ] + 1, "LAT", 3 ) ){
                type = *( store->ctype[ iaxis ] );
             }

             if( type ){
                (store->noncel)--;

                if( store->axlat == -1 ){
                   store->axlat = iaxis;
                   lattype = type;
                } else {
                   if( rep ) astError( AST__BDFTS, "%s(%s): FITS keywords '%s' (='%s') "
                      "and '%s' (='%s') both describe celestial latitude axes.",
                      method, class, store->ctype_name[ store->axlat ],
                      store->ctype[ store->axlat ],
                      store->ctype_name[ iaxis ], store->ctype[ iaxis ] );
                   ret = 0;
                   break;
                }
             }
         }
         
/* Store the axis description count being used for this axis. */         
         (store->ialt)[ iaxis ] = idesc + 1;

      }

/* All axis descriptions have now been checked. The following sections are 
   only done if all axis descriptions were found and the above went OK. */
      if( ret && astOK ){

/* If both longitude and latitude axes were found... */
         if( store->axlat != -1 && store->axlon != -1 ){

/* Report an error if they refer to different celestial coordinate systems. */
            if( lattype != lontype ){
               if( rep ) astError( AST__BDFTS, "%s(%s): FITS keywords '%s' and '%s' indicate "
                         "different celestial coordinate systems ('%s' and '%s').",
                         method, class, store->ctype_name[ store->axlat ],
                         store->ctype_name[ store->axlon ],
                         store->ctype[ store->axlat ],
                         store->ctype[ store->axlon ] );
               ret = 0;

/* Otherwise report an error if longitude and latitude axes use different 
   projections. */
            } else if( store->prj[ store->axlon ] != store->prj[ store->axlat ] ){
               if( rep ) astError( AST__BDFTS, "%s(%s): FITS keywords '%s' and '%s' indicate "
                         "different projections ('%s' and '%s').", method, class, 
                         store->ctype_name[ store->axlat ],
                         store->ctype_name[ store->axlon ],
                         store->ctype[ store->axlat ],
                         store->ctype[ store->axlon ] );
               ret = 0;

/* Otherwise, store a flag identifying the celestial coordinate system,
   and convert the CRVAL and CDELT values for the longitude and latitude axes 
   from degrees to radians. */
            } else {

               if( lattype == 0 ){
                  store->sys = NOCEL;
               } else if( lattype == 1 ){
                  store->sys = RADEC;
               } else if( lattype == 'E' ){
                  store->sys = ECLIP;
               } else if( lattype == 'G' ){
                  store->sys = GALAC;
               } else if( lattype == 'H' ){
                  store->sys = HELIO;
               } else if( lattype == 'S' ){
                  store->sys = SUPER;
               } else {
                  store->sys = CELSY;
               }

               store->crval[ store->axlat ] *= AST__DD2R;
               store->crval[ store->axlon ] *= AST__DD2R;
               store->cdelt[ store->axlat ] *= AST__DD2R;
               store->cdelt[ store->axlon ] *= AST__DD2R;
            }

/* If only one axis has been provided without the other (e.g. longitude but no 
   latitude), report an error. */
         } else if( store->axlat != -1 ){
            if( rep ) astError( AST__BDFTS, "%s(%s): A latitude axis (FITS type '%s') "
                      "was found without a corresponding longitude axis.",
                       method, class, store->ctype[ store->axlat ] );
            ret = 0;

         } else if( store->axlon != -1 ){
            if( rep ) astError( AST__BDFTS, "%s(%s): A longitude axis (FITS type '%s') "
                      "was found without a corresponding latitude axis.",
                       method, class, store->ctype[ store->axlon ] );
            ret = 0;
         }

/* If a PC or CD matrix was supplied, reserve room to hold it. */
         if( matfmt ){
            size = sizeof(double)*(size_t)( naxis * naxis );
            store->pc = (double *) astMalloc( size );
            if( astOK ){
               newwcs = 1;

/* Attempt to get each PC or CD keyword from the header and store the value in
   the matrix array. Elements from a unit matrix are used for any missing 
   keywords. */
               next = store->pc;
               for( iaxis = 1; iaxis <= naxis; iaxis++ ){
                  for( jaxis = 1; jaxis <= naxis; jaxis++ ){
                     (void) sprintf( keyname, matfmt2, iaxis, jaxis );
                     if( !GetWcsValue( this, keyname, AST__FLOAT, 
                                       (void *) next, 0, method, class ) ){
                        if( iaxis == jaxis ) {
                           *next = 1.0;
                        } else {
                           *next = 0.0;
                        }
                     }
                     next++;
                  }
               }
                
/* If a CD matrix was supplied (i.e. the product of the PC matrix and 
   the diagonal matrix of primary CDELT values), convert it to the PC
   matrix. The CDELT values implied by the CD matrix are used in preference 
   to any CDELT keywords which have been supplied. They are converted from 
   degrees to radians if they correspond to celestial longitude or
   latitude axes. */
               if( cd ) {
                  newwcs = 0;
                  SplitMat( naxis, store->pc, store->cdelt );
                  if( store->axlat != -1 && store->axlon != -1 ) {
                     store->cdelt[ store->axlat ] *= AST__DD2R;
                     store->cdelt[ store->axlon ] *= AST__DD2R;
                  }
               }
            }

/* If no PC or CD matrix was supplied, store a NULL pointer. */
         } else { 
            store->pc = NULL;
         }

/* Store the supplied projection parameters. Store AST__BAD for any unsupplied
   projection parameters, and note the number of significant parameters. */
         store->npar = 0;
         for( iprj = 0; iprj < AST__WCSMX; iprj++ ){
            (void) sprintf( keyname, "PROJP%d", iprj );
            if( GetWcsValue( this, keyname, AST__FLOAT, (void *) &keyval, 0,
                              method, class ) ){
               newwcs = 1;
               store->projp[ iprj ] = keyval;
               store->npar = iprj + 1;
            } else {
               store->projp[ iprj ] = AST__BAD;
            }
         }

/* Copy the LONGPOLE and LATPOLE keyword values, converting to radians if
   supplied. */
         if( GetWcsValue( this, "LONGPOLE", AST__FLOAT, (void *) &keyval, 0, 
                           method, class ) ){
            newwcs = 1;
            store->longpole = AST__DD2R*keyval;
         } else {
            store->longpole = AST__BAD;
         }

         if( GetWcsValue( this, "LATPOLE", AST__FLOAT, (void *) &keyval, 0,
                           method, class ) ){
            newwcs = 1;
            store->latpole = AST__DD2R*keyval;
         } else {
            store->latpole = AST__BAD;
         }

/* Get the RADECSYS keyword from the header, and identify the value. Store 
   a integer value identifying the system. Report an error if an 
   unrecognised system is supplied. Store NORADEC if the keyword was 
   not supplied. */
         if( GetWcsValue( this, "RADECSYS", AST__STRING, (void *) &ckeyval, 0, 
                           method, class ) ){
            newwcs = 1;
            if( !strncmp( ckeyval, "FK4 ", 4 ) || 
                !strcmp( ckeyval, "FK4" ) ){
               store->radecsys = FK4;

            } else if( !strncmp( ckeyval, "FK4-NO-E", 8 ) ){
               store->radecsys = FK4NOE;

            } else if( !strncmp( ckeyval, "FK5 ", 4 ) || 
                       !strcmp( ckeyval, "FK5" ) ){
               store->radecsys = FK5;

            } else if( !strncmp( ckeyval, "GAPPT ", 6 ) ||
                       !strcmp( ckeyval, "GAPPT" ) ){
               store->radecsys = GAPPT;

            } else if( astOK ){
               astError( AST__BDFTS, "%s(%s): FITS keyword 'RADECSYS' has the "
                         "unrecognised value '%s'.", method, class, ckeyval );
            }

         } else {
            store->radecsys = NORADEC;
         }

/* Get the value of the EQUINOX keyword from the header. First attempt to
   get it as a floating point value, then as a string, possibly prefixed by
   B or J. If it is not available, use the old EPOCH keyword, instead (in 
   which case implement the IRAF convention that "0.0" means "1950.0" ). */
         if( GetWcsValue( this, "EQUINOX", AST__FLOAT, (void *) &(store->equinox), 
                          0,  method, class ) ){
            newwcs = 1;
            eq_type = 1;

         } else if( GetWcsValue( this, "EQUINOX", AST__STRING, 
                                 (void *) &ckeyval, 0,  method, class ) &&
                    ( eq_j = sscanf( ckeyval, "J%lf", &(store->equinox) ) ||
                             sscanf( ckeyval, "B%lf", &(store->equinox) ) ) ){
            eq_type = 3;

         } else if( GetWcsValue( this, "EPOCH", AST__FLOAT,
                       (void *) &(store->equinox), 0,  method, class ) ){
            eq_type = 2;
            if( store->equinox == 0.0 ) store->equinox = 1950.0;

         } else {
            store->equinox = 0.0;
            eq_type = 0;
         }

/* Set up a default equinox value if none has been supplied, depending
   on the frame of reference. For FK4-based systems, assume B1950. */
         if( store->radecsys == FK4 || 
             store->radecsys == FK4NOE ){
            store->julian = 0;
            if( !eq_type ) store->equinox = 1950.0;

/* For FK5-based systems, assume J2000. */
         } else if( store->radecsys == FK5 ){
            store->julian = 1;
            if( !eq_type ) store->equinox = 2000.0;

/* For other systems... */
         } else {

/* If an equinox was provided by the floating point EQUINOX keyword, use the 
   IAU "1984 rule". */
            if( eq_type == 1 ){
               if( store->equinox < 1984.0 ){
                  store->julian = 0;
               } else {
                  store->julian = 1;
               }

/* If an equinox was provided by the string EQUINOX keyword, use the 
   keyword prefix. */
            } else if( eq_type == 3 ){
               store->julian = eq_j;

/* If an equinox was provided by the old EPOCH keyword, always assume FK4. */
            } else if( eq_type == 2 ){
               store->julian = 0;

/* If no equinox is available, assume B1950 unless any keywords not used 
   in the old pre-WCS system have been supplied, in which case use J2000. 
   */
            } else if( newwcs ){
               store->julian = 1;
               store->equinox = 2000.0;
            } else {
               store->julian = 0;
               store->equinox = 1950.0;
            }

         }
         
/* If the frame of reference is unknown, choose a default frame based on
   equinox type determined above.. */
         if( store->radecsys == NORADEC ){
            if( store->julian ){
               store->radecsys = FK5;
            } else {
               store->radecsys = FK4;
            }
         }

/* Get the MJD-OBS keyword from the header. If it is missing (or of zero
   length), get the DATE-OBS keyword and convert it to an MJD. If DATE-OBS 
   is missing, use the equinox. */    
         if( !GetWcsValue( this, "MJD-OBS", AST__FLOAT, (void *) &(store->mjdobs), 
                           0,  method, class ) ){

            if( GetWcsValue( this, "DATE-OBS", AST__STRING, (void *) &ckeyval, 0, 
                              method, class ) && ChrLen( ckeyval ) > 0 ){
               store->mjdobs = DateObs( ckeyval );
               if( store->mjdobs == AST__BAD && astOK ){
                  astError( AST__BDFTS, "%s(%s): FITS keyword 'DATE-OBS' has "
                            "unusable value '%s'.", method, class, ckeyval );
               }

/* If no epoch is available, use the equinox value. Convert it to a 
   Modified Julian Date. */
            } else {
               if( store->julian == 'J' ){
                  store->mjdobs = slaEpj2d( store->equinox );
               } else {
                  store->mjdobs = slaEpb2d( store->equinox );
               }                  
            }
         }
      }
   }

/* If an axis description was missing or an error has occurred, clean up the 
   results structure, and ensure the returned flag is cleared. */
   if( !ret || !astOK ) {
      CleanFits( store );
      ret = 0;
   }

/* Return the result. */
   return ret;

}


static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FitsChan member function (over-rides the astTestAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a FitsChan's attributes.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   int result;                   /* Result value to return */
   int len;                      /* Length of attrib string */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Card. */
/* ----- */
   if ( !strcmp( attrib, "card" ) ) {
      result = astTestCard( this );

/* Encoding. */
/* --------- */
   } else if ( !strcmp( attrib, "encoding" ) ) {
      result = astTestEncoding( this );

/* FitsDigits. */
/* ----------- */
   } else if ( !strcmp( attrib, "fitsdigits" ) ) {
      result = astTestFitsDigits( this );

#if 0
/* CDMatrix. */
/* --------- */
   } else if ( !strcmp( attrib, "cdmatrix" ) ) {
      result = astTestCDMatrix( this );
#endif

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "ncard" ) ){
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }
   
/* Return the result, */
   return result;
}

static int TestCard( AstFitsChan *this ){
/*
*+
*  Name:
*     astTestCard

*  Purpose:
*     Test the Card attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     int astTestCard( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function tests the Card attribute for the supplied FitsChan. 

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     If the Card attribute has its "cleared" value (i.e. if the first card 
*     in the FitsChan will be the next one to be read), then zero is returned,
*     otherwise 1 is returned.

*-
*/

/* Local Variables: */
   int card;               /* The original value of Card */
   int ret;                /* The returned flag */

/* Get the current value of Card. */
   card = astGetCard( this );

/* Temporarily clear Card. */
   astClearCard( this );

/* See if the original Card is equal to the cleared card, and set the
   returned flag appropriately. Re-instate the original value of card is
   required.*/
   if( astGetCard( this ) == card ) {
      ret = 0;
   } else {
      astSetCard( this, card );
      ret = 1;
   }

/* Return the flag. */
   return ret;

}

static char *UnPreQuote( const char *string ) {
/*
*  Name:
*     UnPreQuote

*  Purpose:
*     Reverse the pre-quoting of FITS character data.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     char *UnPreQuote( const char *string )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function reverses the effect of the PreQuote function on a
*     string (apart from any loss of data due to truncation). It
*     should be used to recover the original character data from the
*     pre-quoted version of a string retrieved from a FITS character
*     value associated with a keyword.

*  Parameters:
*     string
*        Pointer to a constant null-terminated string containing the
*        pre-quoted character data.

*  Returned Value:
*     Pointer to a dynamically allocated null-terminated string
*     containing the un-quoted character data. The memory holding this
*     string should be freed by the caller (using astFree) when no
*     longer required.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked wth the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   int i1;                       /* Offset of first useful character */
   int i2;                       /* Offest of last useful character */

/* Check the global error status. */
   if ( !astOK ) return NULL;
   
/* Initialise to use the first and last characters in the input
   string. */
   i1 = 0;
   i2 = strlen( string ) - 1;

/* If the string contains at least 2 characters, check if the first
   and last characters are double quotes ("). If so, adjust the
   offsets to exclude them. */
   if ( ( i2 > i1 ) &&
        ( string[ i1 ] == '"' ) && ( string[ i2 ] == '"' ) ) {
      i1++;
      i2--;
   }

/* Make a dynamically allocated copy of the useful part of the
   string. */
   result = astString( string + i1, i2 - i1 + 1 );

/* Return the answer. */
   return result; 

}

static int Use( AstFitsChan *this, int set, int helpful ) {
/*
*  Name:
*     Use

*  Purpose:
*     Decide whether to write a value to a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int Use( AstFitsChan *this, int set, int helpful )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function decides whether a value supplied by a class "Dump"
*     function, via a call to one of the astWrite... protected
*     methods, should actually be written to a FitsChan.
*
*     This decision is based on the settings of the "set" and
*     "helpful" flags supplied to the astWrite... method, plus the
*     attribute settings of the FitsChan.

*  Parameters:
*     this
*        A pointer to the FitsChan.
*     set
*        The "set" flag supplied.
*     helpful
*        The "helpful" value supplied.

*  Returned Value:
*     One if the value should be written out, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int full;                     /* Full attribute value */
   int result;                   /* Result value to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If "set" is non-zero, then so is the result ("set" values must
   always be written out). */
   result = ( set != 0 );

/* Otherwise, obtain the value of the FitsChan's Full attribute. */
   if ( !set ) {
      full = astGetFull( this );

/* If Full is positive, display all values, if zero, display only
   "helpful" values, if negative, display no (un-"set") values. */
      if ( astOK ) result = ( ( helpful && ( full > -1 ) ) || ( full > 0 ) );
   }

/* Return the result. */
   return result;
}

static int Ustrcmp( const char *a, const char *b ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     static int Ustrcmp( const char *a, const char *b )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1 
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strcmp" does.
*     -  This function attempts to execute even if an error has occurred. 

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Loop round each character. */
   while( 1 ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

static int Ustrncmp( const char *a, const char *b, size_t n ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strncmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     static int Ustrncmp( const char *a, const char *b, size_t n )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns 0 if there are no differences between the first "n"
*     characters of the two strings, and 1 otherwise. Comparisons are
*     case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     n
*        The maximum number of characters to compare.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strncmp" does.
*     -  This function attempts to execute even if an error has occurred. 

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int i;                  /* Character index */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Compare up to "n" characters. */
   for( i = 0; i < (int) n; i++ ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

static AstWcsMap *WcsDeproj( FitsStore *store, const char *method,
                             const char *class ){
/*
*  Name:
*     WcsDeproj

*  Purpose:
*     Create a WcsMap which transforms Relative Physical Coords to
*     Native Spherical Coords.

*  Type:
*     Private function.

*  Synopsis:
*     AstWcsMap *WcsDeproj( FitsStore *store, const char *method, 
*                           const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A WcsMap is created which applies the inverse of one of the projections
*     included in the FITS WCS library (i.e. a DEprojection). It thus maps
*     Relative Physical Coords into Native Spherical Coords. The mapping
*     only affects the latitude and longitude axes. The mapping passes on
*     all other axes unchanged.

*  Parameters:
*     store 
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created WcsMap or a NULL pointer if an error occurred.

*  Notes:
*     -  The obsolete NCP projections is implemented as a slant orthographic
*     (SIN) projection, and is undefined for a reference latitude of 90 
*     degrees.

*/

/* Local Variables: */
   AstWcsMap *new;                 /* The created WcsMap */
   int prj;                        /* Projection type */
   double sinncp;                  /* Sine of the NCP reference latitude */
   int axlat;                      /* Index of latitude physical axis */
   int axlon;                      /* Index of longitude physical axis */
   int i;                          /* Loop count */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned WcsMap pointer. */
   new = NULL;

/* First dela with cases in which both longitude and latitude axes are
   available. */
   axlat = store->axlat;
   axlon = store->axlon;
   if( axlon >= 0 && axlat >= 0 ){

/* Check for the obsolete NCP projection which is treated as a slant 
   orthographic projection. */
      prj = store->prj[ axlat ];
      if( prj == AST__NCP ){

/* If this is an NCP projection check that it is not singular... */
         sinncp = sin( store->crval[ axlat ] );
         if( sinncp == 0.0 ){
            astError( AST__BDFTS, "%s(%s): FITS NCP projection is undefined "
                      "because the sine of '%s' is zero.", method, class,
                      store->crval_name[ axlat ] );

/* If this NCP projection is not singular, use a SIN (orthographic) projection 
   instead, and set up the correct projection parameters. */
         } else {
            prj = AST__SIN;
            store->npar = 3;
            (store->projp)[ 0 ] = AST__BAD;
            (store->projp)[ 1 ] = 0.0;
            (store->projp)[ 2 ] = cos( store->crval[ axlat ] )/sinncp;
         }
      } 

/* Replace any unsupplied projection parameters with zero. */
      for( i = 0; i < AST__WCSMX; i++ ){
         if( store->projp[ i ] == AST__BAD ) store->projp[ i ] = 0.0;
      }

/* Create the WcsMap, and invert it to get a DEprojection. The WcsMap is
   equivalent to a unit mapping for all axes other than "axlat" and "axlon". */
      new = astWcsMap( store->naxis, prj, axlon + 1, axlat + 1, "" );

      for( i = 0; i < store->npar; i++ ){
         if( store->projp[ i ] != AST__BAD ) astSetProjP( new, i, store->projp[ i ] );
      }

      astInvert( new );

/* Now deal with cases where no longitude/latitude axis pair is
   available. */
   } else {

/* Create the WcsMap with projection type AST__WCSBAD. This causes all
   axes to be passed on unchanged. */
      new = astWcsMap( store->naxis, AST__WCSBAD, 0, 0, "" );

   }

/* If an error has occurred, attempt to annul the new WcsMap. */
   if( !astOK ) new = astAnnul( new );
   
/* Return the WcsMap. */
   return new;

}

static AstFrame *WcsFrame( FitsStore *store, const char *method, 
                           const char *class  ){
/*
*  Name:
*     WcsFrame

*  Purpose:
*     Create a Frame to describe a WCS physical coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     AstFrame *WcsFrame( FitsStore *store, const char *method, 
*                         const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A Frame is returned describingf the physical coordinate system of
*     a WCS-encoded FitsChan. This will be a SkyFrame if the physical
*     coordinate system consists of a pair of recognised celestial
*     longitude and latitude axes.  If there are any other axes, then a
*     CmpFrame will be returned holding a SkyFrame for the celestial
*     axes, and a simple Frame for the other axes. If there are no
*     celestial axes in the FitsChan, then a simple Frame will be returned.

*  Parameters:
*     store
*        A pointer to a structure holding a digested version of the WCS
*        keywords, as created by function StoreFits.
*     method
*        The calling method. Used only in error messages.
*     class 
*        The object class. Used only in error messages.

*  Returned Value:
*     A pointer to the Frame.

*  Notes:
*     -  The FITS CTYPE keyword values are used to set the labels for any
*     non-celestial axes in the returned Frame, and the FITS CUNIT keywords 
*     are used to set the corresponding units strings.
*     -  A NULL pointer is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   AstFrame *phyframe;            /* Non-celestial coords Frame */
   AstFrame *ret;                 /* Returned Frame */
   AstSkyFrame *skyframe;         /* Celestial coordinates Frame */
   int *perm;                     /* Permuted axis indices */
   int axis;                      /* Axis index */
   int celsys;                    /* Celestial coordinate system type */
   int inon;                      /* Index of next non-celestial axis */
   int noncel;                    /* Number of non-celestial axes */
   int naxis;                     /* No. of axes */
   int radecsys;                  /* Celestial reference frame */
   int skperm[2];                 /* Axis permutation for SkyFrames */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned Frame pointer. */
   ret = NULL;

/* Get the number of axes. */
   naxis = store->naxis;

/* If the axes defined by the FITS header cards include a celestial 
   longitude/latitude pair, create a SkyFrame for the specified system. */
   celsys = store->sys;
   skyframe = NULL;
   if( celsys != NOCEL ){

      if( celsys == ECLIP ){
         skyframe = astSkyFrame( "System=Ecliptic" );

      } else if( celsys == GALAC ){
         skyframe = astSkyFrame( "System=Galactic" );

      } else if( celsys == SUPER ){
         skyframe = astSkyFrame( "System=Supergalactic" );

      } else if( celsys == RADEC ){
         radecsys = store->radecsys;

         if( radecsys == FK4 ){
            skyframe = astSkyFrame( "System=FK4" );

         } else if( radecsys == FK4NOE ){
            skyframe = astSkyFrame( "System=FK4_NO_E" );

         } else if( radecsys == FK5 ){
            skyframe = astSkyFrame( "System=FK5" );

         } else if( radecsys == GAPPT ){
            skyframe = astSkyFrame( "System=GAPPT" );

         } else if( astOK ){
            astError( AST__INTER, "%s(%s): Internal AST programming "
                      "error - FITS equatorial coordinate system type %d "
                      "not yet supported in WcsFrame.", method, class, radecsys );
         }

      } else if( astOK ){
         astError( AST__INTER, "%s(%s): Internal AST programming "
                   "error - FITS celestial coordinate system type %d "
                   "not yet supported in WcsFrame.", method, class, celsys );
      }

/* Store the projection description. */
      astSetProjection( skyframe, astWcsPrjDesc( (store->prj)[ store->axlon ] )  );

/* Get the epoch of the observation and store it in the SkyFrame. */
      astSet( skyframe, "Epoch= MJD %f", store->mjdobs );

/* Get the epoch of the reference equinox and store it in the SkyFrame. */
      astSet( skyframe, "Equinox= %c %f", (store->julian)?'J':'B',
              store->equinox );

/* If there are more than 2 axes, then there must also be some non-celestial 
   axes. */
      noncel = naxis - 2;

/* If there are no celestial axes, then there must be some non-celestial
   axes. */
   } else {
      noncel = naxis;
   }

/* If there are any non-celestial axes, we need to create a Frame to hold
   them. */
   if( astOK && noncel > 0 ){  
      phyframe = astFrame( noncel, "" );
   
/* If there are no celestial axes, the returned Frame is just this Frame. */
      if( !skyframe ){
         ret = phyframe;

/* Otherwise, there are both celestial and non-celestial axes, so we need
   to make a CmpFrame to contain all axes. */
      } else {
         ret = (AstFrame *) astCmpFrame( skyframe, phyframe, "" );

/* Annul the constituent Frames. */
         skyframe = astAnnul( skyframe );
         phyframe = astAnnul( phyframe );

/* Allocate memory to hold the permuted axis indices. */
         perm = (int *) astMalloc( sizeof( int )*(size_t) naxis );

/* The axis numbering in the CmpFrame needs to be modified to be the same
   as the axis numbering in the FITS header. */
         inon = 2;
         for( axis = 0; axis < naxis && astOK; axis++ ){

/* The longitude axis is axis zero in the CmpFrame. */
            if( axis == store->axlon ){
               perm[ axis ] = 0;

/* The latitude axis is axis one in the CmpFrame. */
            } else if( axis == store->axlat ){
               perm[ axis ] = 1;

/* Use non-celestial axes to fill up. */
            } else {
               perm[ axis ] = inon++;
            }
         }

/* Rarrange the axes. */
         astPermAxes( ret, perm );

/* Free the memory holding the permuted axis indices. */
         perm = (int *) astFree( (void *) perm );

      }

/* We now assign Unit, Label and Symbol attributes to all non-celestial axes. 
   The celestial axes retain the defaults established by the SkyFrame class. 
   Store copies of the FITS CTYPE keyword values as the axis symbols, and
   the FITS CUNIT keyword values as the axis units. Leave these Frame
   attributes unset if the relevant keywords were not supplied. If the axis
   values are relative rather than absolute, include the reference value
   in the axis label. Any comment with the CTYPE keyword is used as the
   axis label. */
      for( axis = 0; axis < naxis; axis++ ){

/* Leave celestial axes unchanged. */
         if( axis != store->axlon && axis != store->axlat ){

            if( store->ctype[ axis ] ) astSetSymbol( ret, axis, 
                                                   store->ctype[ axis ] );
            if( store->ctype_com[ axis ] ) astSetLabel( ret, axis, 
                                                   store->ctype_com[ axis ] );
            if( store->cunit[ axis ] ) astSetUnit( ret, axis, 
                                                   store->cunit[ axis ] );
         }            

      }

/* If there are no non-celestial axes, the returned Frame is just the
   SkyFrame. Permute the axes to ensure that long./lat. axes correspond
   to the FITS headers. */
   } else {
      ret = (AstFrame *) skyframe;
      if( store->axlon != 0 ) {
         skperm[ store->axlon ] = 0;
         skperm[ store->axlat ] = 1;
         astPermAxes( ret, skperm );
      }
   }   

/* If an error has occurred, annul the Frame. */
   if( !astOK ) ret = astAnnul( ret );
   
/* Return the Frame. */
   return ret;

}

static AstMapping *WcsMapping( FitsStore *store, const char *method, 
                               const char *class ){
/*
*  Name:
*     WcsMapping

*  Purpose:
*     Create a CmpMap for the WCS transformations described in a FITS header.

*  Type:
*     Private function.

*  Synopsis:
*     AstMapping *WcsMapping( FitsStore *store, const char *method, 
*                             const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     This function interprets the contents of the supplied FitsStore
*     structure, and creates a Mapping which describes the transformation 
*     from pixel coordinates to absolute physical coordinates, using 
*     the FITS World Coordinate System conventions. A pointer to this CmpMap 
*     is returned.

*  Parameters:
*     store
*        A structure containing information about the requested axis 
*        descriptions derived from a FITS header.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the Mapping.

*  Notes:
*     -  If a "PC" matrix is specified in the header then it will be used.
*     Otherwise, any "CD" matrix present in the header will be used. If 
*     neither a "PC" or "CD" matrix has been supplied, then the deprecated
*     CROTA keywords will be used to construct a "PC" matrix representing 
*     2-dimensional rotation (the corresponding axes must represent some
*     system of celestial longitude and latitude). If no CROTA keywords can 
*     be found, then a unit "PC" matrix is assumed.

*/

/* Local Variables: */
   AstMapping *map1;          /* A Mapping */
   AstMapping *map2;          /* Another Mapping */
   AstMapping *ret;           /* The returned Mapping */
   AstCmpMap *cmpmap;         /* A CmpMap mapping */
   AstWcsMap *wcsmap;         /* A WcsMap mapping */

/* Initialise the pointer to the returned Mapping. */
   ret = NULL;

/* Check the global status. */
   if ( !astOK ) return ret;

/* Now try to create a WinMap which translates the pixel coordinates so
   that they are refered to an origin at the reference pixel. This
   subtracts the value of CRPIXi (or CmPIXi) from axis i. */
   map1 = (AstMapping *) WcsShift( store );
   
/* Now try to create a MatrixMap describing the transformation from "Ideal 
   Pixel Coords" to "Relative Physical Coords" (excluding the shift of 
   origin described by the WinMap just created). This includes both the
   PC matrix and the CDELT values. */
   map2 = (AstMapping *) WcsMatrix( store, method, class );

/* Combine the two Mappings in series, and then annul the individual
   Mappings. Store the new Mapping as "map1". */
   cmpmap = astCmpMap( map1, map2, 1, "" );
   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );
   map1 = (AstMapping *) cmpmap;

/* Now try to create a WcsMap which describes the deprojection for celestial
   axes from "Relative Physical Coords" to "Native Spherical Coords".
   Non-celestial axes are just copied without change. */
   wcsmap = WcsDeproj( store, method, class );

/* Add the WcsMap onto the total Mapping. Note, the wcsmap is needed
   later and so is not annulled yet. */
   cmpmap = astCmpMap( map1, wcsmap, 1, "" );
   map1 = astAnnul( map1 );
   map1 = (AstMapping *) cmpmap;

/* Now produce a Mapping which converts the axes holding "Native Spherical 
   Coords" into "Celestial Coords", leaving all other axes unchanged. This
   is a multi-stage mapping which is stored as a CmpMap. Non-celestial
   axes are converted into absolute physical coordinates by adding on the
   reference value. The WcsMap can then be annulled as it is no longer 
   needed. */
   map2 = (AstMapping *) WcsNative( store, wcsmap, method, class );
   wcsmap = astAnnul( wcsmap );

/* Combine the two mappings in series, and then annul them. */
   ret = (AstMapping *) astCmpMap( map1, map2, 1, "" );
   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );

/* Return. */
   return ret;

}

static AstMatrixMap *WcsMatrix( FitsStore *store, const char *method, 
                                const char *class ){
/*
*  Name:
*     WcsMatrix

*  Purpose:
*     Create a MatrixMap for the transformation from "Ideal Pixel Coords" 
*     to "Relative Physical Coords".

*  Type:
*     Private function.

*  Synopsis:
*     AstMatrixMap *WcsMatrix( FitsStore *store, const char *method,
*                              const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A MatrixMap describing the conversion from "Ideal Pixel Coordinates"
*     relative to the reference pixel, to "Relative Physical Coordinates"
*     is returned. This assumes that the positions being mapped have already
*     been shifted so that they refer to an origin at the reference pixel (as
*     given by keywords CRPIX1, CRPIX2, etc). The returned mapping includes 
*     the scalings implied by keywords CDELT1, CDELT2, etc. If no PC matrix
*     is available, any CROTAi keywords will be used to create a matrix using
*     the old AIPS conventions for the storage of world coordinates. 

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created MatrixMap or a NULL pointer if an 
*     error occurred.

*  Notes:
*     -  The produced mapping assumed that the input pixel coordinates have 
*     already been shifted so that their origin is at the reference pixel.
*     -  CROTA keywords are used to define the matrix if neither a PC or a CD
*     matrix has been supplied. In this case, the CROTA value corresponding to
*     the latitude axes is used if it is defined, otherwise the longitude axis
*     value is used. If neither is defined, then a unit MatrixMap is returned.
*     It is assumed that there is a one-to-one correspondance between physical 
*     and pixel axes (i.e. if the longitude and latitude physical axes have
*     indices i and j, then pixel axes i and j will be rotated to form the
*     longitude and latitude axes).
*/

/* Local Variables: */
   AstMatrixMap *new;       /* The created MatrixMap */
   double *dvals;           /* Pointer to pixel size array */
   double *mat;             /* Pointer to matrix rotation array */
   double *pc;              /* Pointer to next pc value */
   double cdelt;            /* Pixel size for this row */
   double coscr;            /* Cosine of CROTA value */
   double crota;            /* CROTA keyword value to use */
   double sincr;            /* Sine of CROTA value */
   int ax;                  /* Index of axis */
   int axlat;               /* Index of latitude axis */
   int axlon;               /* Index of longitude axis */
   int col;                 /* Column index */
   int i;                   /* Axis count */
   int naxis;               /* No. of axes */
   int row;                 /* Row index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned MatrixMap pointer. */
   new = NULL;

/* Store the indices of any longitude and latitude axes. */
   axlat = store->axlat;
   axlon = store->axlon;

/* Store the number of axes. */
   naxis = store->naxis;   

/* Take a copy of the CDELT values. */
   dvals = (double *) astStore( NULL, store->cdelt, 
                                sizeof( double )*(size_t)naxis );

/* If any CDELT values are zero, use one hundredth of the corresponding
   CRVAL value instead, or 1.0 if CRVAL is zero. Otherwise, the zeros
   could cause the matrix to be non-invertable. The Mapping could then not
   be simplified or used by a Plot. CDELT values of zero are usually used
   to indicate "redundant" axes. For instance, a 2D image may be stored
   as a 3D cube with a single plane with the "redundant" 3rd axis used 
   to specify the wavelength of the filter. The actual value used for CDELT
   shouldn't matter since the axis only spans a single pixel anyway. */
   for( i = 1; i < naxis; i++ ){
      if( dvals[ i ] == 0.0 ) {
         dvals[ i ] = 0.01*(store->crval)[ i ];
         if( dvals[ i ] == 0.0 ) dvals[ i ] = 1.0;
      }
   }

/* First deal with cases where a "PC" matrix is available. */
   if( store->pc ){

/* Multiply the pc values by the appropriate CDELT value. */
      pc = store->pc;
      for( row = 0; row < naxis; row++ ){
         cdelt = dvals[ row ];
         for( col = 0; col < naxis; col++ ) *(pc++) *= cdelt;
      }

/* Create the matrix. */
      new = astMatrixMap( naxis, naxis, 0, store->pc, "" );

/* If no matrix was supplied, then we will need to use the deprecated CROTAi
   keywords to create a PC matrix. The resulting matrix can only rotate the 
   longitude and latitude axes. Therefore, check that longitude and latitude 
   axes are available. It is assumed that the latitude and longitude physical 
   axes are formed by rotating the two pixel axes with the same indices (i.e. 
   pixel axes axlon and axlat are rotated onto physical axes axlon and
   axlat). */
   } else if( axlat >= 0 && axlon >= 0 ){

/* Check that a CROTA keyword has been supplied for one of these two axes 
   (the latitude axis CROTA value is used if both have been supplied). */
      if( store->crota[ axlat ] != AST__BAD ){
         crota = store->crota[ axlat ];
      } else {
         crota = store->crota[ axlon ];
      }
      if( crota != AST__BAD ){

/* Report an error if the pixel size on either of the axes is zero. */
         ax = axlat;
         for( i = 0; i < 2; i++ ){
            if( dvals[ ax ] == 0.0 ){
               astError( AST__BDFTS, "%s(%s): Zero pixel size specified by "
                         "FITS keyword '%s'.",  method, class, 
                         store->cdelt_name[ ax ] );
            }
            ax = axlon;
         }

/* Create an array storing a matrix which contains the CDELT values on
   the non-celestial axes, and the value 1.0 on the celestial axes. */
         mat = (double *) astMalloc( sizeof(double)*(size_t)( naxis*naxis ) );
         if( astOK ){
            for( i = 0; i < naxis*naxis; i++ ) mat[ i ] = 0.0;
            for( i = 0; i < naxis; i++ ) {
               if( i == axlat || i == axlon ){
                  mat[ i*( naxis + 1 ) ] = 1.0;
               } else {
                  mat[ i*( naxis + 1 ) ] = dvals[ i ];
               }
            }            

/* Store the required sin and cos terms to rotate the longitude and latitude
   axes by the required amount. Incorporate the CDELT values into the
   celestial axes in this matrix. */
            sincr = sin( crota );
            coscr = cos( crota );
            mat[ axlon*naxis + axlon ] = dvals[ axlon ]*coscr;
            mat[ axlon*naxis + axlat ] = -dvals[ axlat ]*sincr;
            mat[ axlat*naxis + axlon ] = dvals[ axlon ]*sincr;
            mat[ axlat*naxis + axlat ] = dvals[ axlat ]*coscr;

/* Create the MatrixMap. */               
            new = astMatrixMap( naxis, naxis, 0, mat, "" );

/* Free the matrix array. */
            mat = (double *) astFree( (void *) mat );

         }

/* If there is no CROTA keyword, create a diagonal MatrixMap holding the
   CDELT values. */
      } else {
         new = astMatrixMap( naxis, naxis, 1, dvals, "" );
      }

/* If no PC matrix was supplied, and there are no celestial axes, create a 
   diagonal MatrixMap holding the CDELT values. */
   } else {
      new = astMatrixMap( naxis, naxis, 1, dvals, "" );
   }      

/* Release the memory used to hold the CDELT values. */
   dvals = (double *) astFree( (void *) dvals );

/* If an error has occurred, attempt to annul the returned MatrixMap. */
   if( !astOK ) new = astAnnul( new );

/* Return the MatrixMap. */
   return new;

}

static AstCmpMap *WcsNative( FitsStore *store, AstWcsMap *wcsmap, 
                             const char *method, const char *class ){
/*
*  Name:
*     WcsNative

*  Purpose:
*     Create a CmpMap which transforms Native Spherical Coords to
*     Celestial Coords.

*  Type:
*     Private function.

*  Synopsis:
*     AstCmpMap *WcsNative( FitsStore *store, AstWcsMap *wcsmap, 
*                           const char *method, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A CmpMap is created which rotates the supplied Native Spherical Coords
*     into Celestial Coords in the standard system specified by the CTYPE or
*     CmYPE FITS keywords. The reference values are added on to any
*     non-celestial axes to produce absolute physical coordinates.
*
*     At the highest level, the returned CmpMap is made up of the following 
*     Mappings in series (if celestial long/lat axes are present):
*        1 - A PermMap which rearranges the axes so that the longitude axis is
*            axis 0, the latitude axis is axis 1, and all other axes are
*            stored at higher indices, starting at axis 2.
*        2 - A CmpMap which converts the values on axes 0 and 1 from Native
*            Spherical to Celestial coordinates, leaving all other axes 
*            unchanged.
*        3 - A PermMap which rearranges the axes to put the longitude and 
*            latitude axes back in their original places. This is just the 
*            inverse of the PermMap used at stage 1 above.
*
*     The CmpMap used at stage 2 above, is made up of two Mappings in 
*     parallel:
*         4 - A CmpMap which maps axes 0 and 1 from Native Spherical to
*             Celestial coordinates.
*         5 - A WinMap which adds on the reference values to axes 2, 3, etc.
*
*     The CmpMap used at stage 4 above, is made up of the following Mappings
*     in series:
*         6 - A SphMap which converts the supplied spherical coordinates into
*             Cartesian Coordinates.
*         7 - A MatrixMap which rotates the Cartesian coordinates from the 
*             Native to the Celestial system.
*         8 - A SphMap which converts the resulting Cartesian coordinates back
*             to spherical coordinates.

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     wcsmap 
*        A mapping describing the deprojection which is being used. This is 
*        needed in order to be able to locate the reference point within the
*        Native Speherical Coordinate system, since it varies from projection
*        to projection.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created CmpMap or a NULL pointer if an error occurred.

*  Notes:
*     -  The local variable names correspond to the notation in the paper
*     by Greisen & Calabretta describing the FITS WCS system.

*/

/* Local Variables: */
   AstCmpMap *cmpmap;         /* A CmpMap */
   AstCmpMap *new;            /* The returned CmpMap */
   AstMatrixMap *matmap;      /* A MatrixMap */
   AstMatrixMap *matmap2;     /* Another MatrixMap */
   AstPermMap *permmap;       /* A PermMap */
   AstWinMap *winmap;         /* A WinMap */
   AstSphMap *sphmap;         /* A SphMap */
   double alpha0;             /* Long. of ref. point in standard system */
   double alphap;             /* Long. of native nth pole in standard system */
   double axis[3];            /* Vector giving the axis of rotation */
   double delta0;             /* Lat. of ref. point in standard system */
   double deltap;             /* Lat. of native nth pole in standard system */
   double phip;               /* Long. of standard nth pole in native system */
   double latpole;            /* Lat. of native nth pole in standard system if deltap undefined */
   int axlat;                 /* Index of latitude physical axis */
   int axlon;                 /* Index of longitude physical axis */
   int i;                     /* Loop count */
   int *inperm;               /* Pointer to array of output axis indices */
   int nax_rem;               /* No. of non-astrometric axes */
   int naxis;                 /* No. of axes. */
   int new_axlat;             /* Index of lat. physical axis after perming */
   int *outperm;              /* Pointer to array of input axis indices */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned CmpMap pointer. */
   new = NULL;

/* Store the number of axes in a local variable. */
   naxis = store->naxis;   

/* If there is no longitude or latitude axis, just add on the reference
   value to all axes. */
   axlat = store->axlat;
   axlon = store->axlon;
   if( axlon < 0 || axlat < 0 ){
      new = (AstCmpMap *) WcsAddRef( store, NULL );

/* If there is a lon/lat axis pair, create the inperm and outper arrays
   which will be needed later to create ther PermMap which reorganises
   the axes so that axis zeri is the longitude axis and axis 1 is the
   latitude axis. */
   } else {

/* Get storage for the two arrays. */
      inperm = (int *) astMalloc( sizeof( int )*(size_t)naxis );
      outperm = (int *) astMalloc( sizeof( int )*(size_t)naxis );
      if( astOK ){

/* Initialise an array holding the indices of the input axes which are copied 
   to each output axis. Initially assume that there is no re-arranging of
   the axes. */
         for( i = 0; i < naxis; i++ ) outperm[ i ] = i;

/* Swap the longitude axis and axis 0. */
         i = outperm[ axlon ];
         outperm[ axlon ] = outperm[ 0 ];
         outperm[ 0 ] = i; 

/* If axis 0 was originally the latitude axis, the latitude axis will now
   be where the longitude axis was originally (because of the above axis
   swap). */
         if( axlat == 0 ) {
            new_axlat = axlon;         
         } else {
            new_axlat = axlat;
         }

/* Swap the latitude axis and axis 1. */
         i = outperm[ new_axlat ];
         outperm[ new_axlat ] = outperm[ 1 ];
         outperm[ 1 ] = i;

/* Create the array holding the output axis index corresponding to 
   each input axis. */
         for( i = 0; i < naxis; i++ ) inperm[ outperm[ i ] ] = i;

      }

/* Store the latitude and longitude (in the standard system) of the reference
   point. */
      delta0 = store->crval[ axlat ];   
      alpha0 = store->crval[ axlon ];   

/* If the latitude is outside the range +/- PI/2, convert them to the 
   latitude and longitude on the opposite meridian. */
      alphap = slaDrange( delta0 );  
      delta0 = alphap;
      if ( delta0 > AST__DPIBY2 ){
         delta0 = AST__DPI - delta0;
         alpha0 = slaDrange( AST__DPI + alpha0 );
      } else if ( delta0 < -AST__DPIBY2 ){
         delta0 = -AST__DPI - delta0;
         alpha0 = slaDrange( AST__DPI + alpha0 );
      }
   
/* Store the values of the FITS keywords LONGPOLE and LATPOLE. */
      phip = store->longpole;
      latpole = store->latpole;

/* Find the standard Celestial Coordinates of the north pole of the Native
   Spherical Coordinate system. Report an error if the position was not
   defined. */
      if( !WcsNatPole( wcsmap, alpha0, delta0, latpole, &phip, &alphap,
                       &deltap ) ){
         astError( AST__BDFTS, "%s(%s): Conversion from FITS WCS native "
                   "coordinates to celestial coordinates is ill-conditioned.",
                   method, class );
      }

/* Create the SphMap which converts spherical coordinates to Cartesian
   coordinates (stage 6 in the prologue). This asumes that axis 0 is the 
   longitude axis, and axis 1 is the latitude axis. This will be ensured
   by a PermMap created later. Indicate that the SphMap will only be used 
   to transform points on a unit sphere. This enables a forward SphMap
   to be combined with an inverse SphMap into a UnitMap, and thus aids
   simplification. */
      sphmap = astSphMap( "UnitRadius=1" );
      astInvert( sphmap );

/* Create a unit MatrixMap to be the basis of the MatrixMap which rotates
   Native Spherical Coords to Celestial Coords (stage 7 in the prologue). */
      matmap = astMatrixMap( 3, 3, 2, NULL, "" );

/* Modify the above MatrixMap so that it rotates the Cartesian position vectors
   by -phip (i.e. LONGPOLE) about the Z axis. This puts the north pole of the 
   standard system at zero longitude in the rotated system. Then annul the 
   original MatrixMap and use the new one instead. */
      axis[ 0 ] = 0;
      axis[ 1 ] = 0;
      axis[ 2 ] = 1;
      matmap2 = astMtrRot( matmap, -phip, axis );
      matmap = astAnnul( matmap );
      matmap = matmap2;

/* Now modify the above MatrixMap so that it rotates the Cartesian position
   vectors by -(PI/2-deltap) about the Y axis. This puts the north pole of 
   the standard system as 90 degrees latitude in the rotated system. Then annul
   the original MatrixMap and use the new one instead. */
      axis[ 0 ] = 0;
      axis[ 1 ] = 1;
      axis[ 2 ] = 0;
      matmap2 = astMtrRot( matmap, deltap - AST__DPIBY2, axis );
      matmap = astAnnul( matmap );
      matmap = matmap2;

/* Finally modify the above MatrixMap so that it rotates the Cartesian position
   vectors (PI+alphap) about the Z axis. This puts the primary meridian of the 
   standard system at zero longitude in the rotated system. This results in the 
   rotated system being coincident with the standard system. */
      axis[ 0 ] = 0;
      axis[ 1 ] = 0;
      axis[ 2 ] = 1;
      matmap2 = astMtrRot( matmap, AST__DPI + alphap, axis );
      matmap = astAnnul( matmap );
      matmap = matmap2;

/* Combine the SphMap (stage 6) and MatrixMap (stage 7) in series. */
      cmpmap = astCmpMap( sphmap, matmap, 1, "" );

/* Now invert the SphMap so that it transforms Cartesian to Speherical 
   coordinates (this is the mapping described as stage 8 in the prologue). */
      astInvert( sphmap );
   
/* Add it to the compound mapping. The CmpMap then corresponds to stage 4
   in the prologue. Annul the constituent mappings. */
   new = astCmpMap( cmpmap, sphmap, 1, "" );
      cmpmap = astAnnul( cmpmap );   
      sphmap = astAnnul( sphmap );   
      matmap = astAnnul( matmap );   

/* If there are any remaining axes (i.e. axes which do not describe a 
   Celestial coordinate system), create a WinMap which adds on the
   reference values to these remaining axes (stage 5 in the prologue), 
   and add it the CmpMap, putting it in parallel with the earlier mappings. 
   The resulting CmpMap then corresponds to stage 2 in the prologue. Note,
   the axis numbering used by this WinMap needs to take account of the
   fact that it is only applied to the non-celestial axes. The axes are 
   re-ordered by the PermMap described at stage 1 in the prologue. */
      nax_rem = naxis - 2;
      if( nax_rem > 0 ){
         winmap = WcsAddRef( store, outperm );
         cmpmap = astCmpMap( new, winmap, 0, "" );
         new = astAnnul( new );
         winmap = astAnnul( winmap );
         new = cmpmap;   
      }

/* Now we need to ensure that axes 0 and 1 correspond to longitude and 
   latitude. If this is already the case, then the CmpMap can be returned
   as it is. Otherwise, a PermMap needs to be created to rearrange the
   axes. */
      if( axlon != 0 || axlat != 1 ){

/* Create the PermMap using the inperm and outperm arrays created earlier. 
   This is the mapping described as stage 1 in the prologue. */
         permmap = astPermMap( naxis, inperm, naxis, outperm, NULL, "" );

/* Compound this PermMap and the CmpMap corresponding to stage 2 (created
   earlier) in series. */
         cmpmap = astCmpMap( permmap, new, 1, "" );         
         new = astAnnul( new );
         new = cmpmap; 

/* Now invert the PermMap, so that it re-arranges the axes back into
   their original order. This is the mapping described as stage 3 in
   the prologue. */
         astInvert( permmap );

/* And finally.... add this inverted PermMap onto the end of the CmpMap. */         
         cmpmap = astCmpMap( new, permmap, 1, "" );
         permmap = astAnnul( permmap );
         new = astAnnul( new );
         new = cmpmap; 
      }
      
/* Free the temporary arrays. */
      inperm = (int *) astFree( (void *) inperm );
      outperm = (int *) astFree( (void *) outperm );
   }
   
/* If an error has occurred, attempt to annul the new CmpMap. */
   if( !astOK ) new = astAnnul( new );
   
/* Return the CmpMap. */
   return new;

}

static int WcsNatPole( AstWcsMap *wcsmap, double alpha0, double delta0, 
                        double latpole, double *phip, double *alphap,
                        double *deltap ){
/*
*  Name:
*     WcsNatPole

*  Purpose:
*     Find the position of the north pole of the Native Spherical
*     Coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     int WcsNatPole( AstWcsMap *wcsmap, double alpha0, double delta0, 
*                      double latpole, double *phip, double *alphap,
*                      double *deltap )

*  Class Membership:
*     FitsChan

*  Description:
*     The supplied WcsMap converts projected positions given in
*     "Relative Physical Coords" to positions in the "Native Spherical 
*     Coordinate" system. This function finds the pole of this spherical
*     coordinate system in terms of the standard Celestial Coordinate 
*     system to which the CRVALi, LONGPOLE and LATPOLE keywords refer (this
*     system should be identified by the last 4 characters of the CTYPEi 
*     keywords). It also supplies a default value for LONGPOLE if no value
*     has been supplied explicitly in the FITS header.
*
*     This function implements equations 7, 8 and 9 from the WCS paper by
*     Greisen & Calabretta (plus the associated treatment of special cases).
*     The paper provides more detailed documentation for the mathematics
*     implemented by this function.

*  Parameters:
*     wcsmap 
*        A mapping describing the deprojection being used (i.e. the
*        mapping from Relative Physical Coords to Native Spherical Coords).
*     alpha0
*        The longitude of the reference point in the standard celestial
*        coordinate frame (in radians). 
*     delta0
*        The latitude of the reference point in the standard celestial
*        coordinate frame (in radians).
*     latpole
*        The value of FITS keyword LATPOLE, converted to radians, or the 
*        symbolic constant AST__BAD if the keyword was not supplied. 
*     phip
*        Pointer to a location at which is stored the longitude of the north
*        pole of the standard Celestial coordinate system, as measured in
*        the Native Spherical Coordinate system, in radians. This should be
*        supplied holding the radian equivalent of the value of the FITS 
*        keyword LONGPOLE, or the symbolic constant AST__BAD if the keyword was 
*        not supplied (in which case a default value will be returned at the
*        given location).
*     alphap
*        Pointer to a location at which to store the calculated longitude
*        of the Native North Pole, in radians.
*     deltap
*        Pointer to a location at which to store the calculated latitude
*        of the Native North Pole, in radians.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A status: non-zero for success, zero if the position of the native 
*     north pole is undefined.

*  Notes:
*     -  Certain combinations of keyword values result in the latitude of
*     the Native North Pole being undefined. In these cases, a value of 
*     0 is returned for the function value, but no error is reported.
*     -  All angular values used by this function are in radians.
*     -  A value of 0 is returned if an error has already occurred.

*/

/* Local Variables: */
   double cos_theta0;              /* Cosine of theta0 */
   double cos_phip;                /* Cosine of phip */
   double cos_delta0;              /* Cosine of delta0 */
   double cos_deltap;              /* Cosine of deltap */
   double deltap_1;                /* First possible value for deltap */
   double deltap_2;                /* Second possible value for deltap */
   double sin_theta0;              /* Sine of theta0 */
   double sin_phip;                /* Sine of phip */
   double sin_delta0;              /* Sine of delta0 */
   double sin_deltap;              /* Sine of deltap */
   double t0, t1, t2, t3, t4;      /* Intermediate values */
   double theta0;                  /* Native lat. of ref. point */

/* Check the global status. */
   if ( !astOK ) return 0;

/* Get the latitude (in Native Spherical Coords) of the reference point
   of the projection, in radians. Return if an error occurs. */
   theta0 = astGetNatLat( wcsmap );
   if( !astOK ) return 0;

/* If no value was supplied for the FITS keyword LONGPOLE, set up a default 
   value such that the celestial latitude increases in the same direction
   as the native latitude at the reference point. */
   if( *phip == AST__BAD ){
      if( delta0 > theta0 ){
         *phip = 0.0;
      } else {
         *phip = AST__DPI;
      }
   }

/* If the reference point is concident with the Native North Pole, then the
   Native North Pole must have the same coordinates as the Reference
   Point. Tests for equality include some tolerance to allow for rounding
   errors. */
   sin_theta0 = sin( theta0 );
   if( EQUAL( sin_theta0, 1.0 ) ){
      *alphap = alpha0;
      *deltap = delta0;

/* If the reference point is concident with the Native South Pole, then the
   Native North Pole must have the coordinates of the point diametrically 
   opposite the Reference Point. */
   } else if( EQUAL( sin_theta0, -1.0 ) ){
      *alphap = alpha0 + AST__DPI;
      *deltap = -delta0;

/* For all other cases, go through the procedure described in the WCS paper by 
   Greisen & Calabretta, to find the position of the Native North Pole.
   First store some useful values. */
   } else {
      cos_theta0 = cos( theta0 );
      cos_delta0 = cos( delta0 );
      cos_phip = cos( *phip );
      sin_delta0 = sin( delta0 );
      sin_phip = sin( *phip );

/* Next, find the two possible values for the latitude of the Native 
   North Pole (deltap). If any stage of this transformation is
   indeterminate, return zero (except for the single special case noted 
   in item 6 para. 2 of the WCS paper, for which LATPOLE specifies the
   values to be used). */
      t0 = cos_theta0*cos_phip;
      if( fabs( t0 ) < TOL2 && fabs( sin_theta0 ) < TOL2 ){
         if( latpole != AST__BAD ) {
            *deltap = latpole;
         } else {
            return 0;
         }

      } else {
         t1 = atan2( sin_theta0, t0 );
         t2 = cos_theta0*cos_phip;
         t2 *= t2;
         t2 += sin_theta0*sin_theta0;
         if( t2 <= DBL_MIN ){
            return 0;

         } else {
            t3 = sin_delta0/sqrt( t2 );
            if( fabs( t3 ) > 1.0 + TOL1 ){
               return 0;

            } else {
               if( t3 < -1.0 ){
                  t4 = AST__DPI;
               } else if( t3 > 1.0 ){
                  t4 = 0.0;
               } else {
                  t4 = acos( t3 );
               }
               deltap_1 = slaDrange( t1 + t4 );
               deltap_2 = slaDrange( t1 - t4 );

/* Select which of these two values of deltap to use. Values outside the
   range +/- PI/2 cannot be used. If both values are within this range
   use the value which is closest to the supplied value of latpole (or
   use the northern most value if the LATPOLE keyword was not supplied. */
               if( fabs( deltap_1 ) > AST__DPIBY2 + TOL2 ){
                  *deltap = deltap_2;

               } else if( fabs( deltap_2 ) > AST__DPIBY2 + TOL2 ){
                  *deltap = deltap_1;

               } else {
                  if( latpole != AST__BAD ){
                     if( fabs( deltap_1 - latpole ) < 
                         fabs( deltap_2 - latpole ) ){
                        *deltap = deltap_1;
                     } else {               
                        *deltap = deltap_2;
                     }
                  } else {
                     if( deltap_1 > deltap_2 ){
                        *deltap = deltap_1;
                     } else {
                        *deltap = deltap_2;
                     }
                  }
               }
               if( fabs( *deltap  ) > AST__DPIBY2 + TOL2 ) {
                  return 0;
               } else if( *deltap < -AST__DPIBY2 ){
                  *deltap = -AST__DPIBY2;
               } else if( *deltap > AST__DPIBY2 ){
                  *deltap = AST__DPIBY2;
               }
            }
         }
      }

/* If a valid value for the latitude (deltap) has been found, find the 
   longitude of the Native North Pole. */
      if( *deltap != AST__BAD ) {
         if( fabs( cos_delta0) > TOL2 ){
            cos_deltap = cos( *deltap );
            sin_deltap = sin( *deltap );
            if( fabs( cos_deltap ) > TOL2 ){
               t1 = sin_phip*cos_theta0/cos_delta0;
               t2 = ( sin_theta0 - sin_deltap*sin_delta0 )
                    /( cos_delta0*cos_deltap );
               if( fabs( t1 > TOL2 ) || fabs( t2 > TOL2 ) ){
                  *alphap = alpha0 - atan2( t1, t2 );
               } else {
                  *alphap = alpha0;
               }

            } else if( sin_deltap > 0.0 ){
               *alphap = alpha0 + (*phip) - AST__DPI;

            } else {
               *alphap = alpha0 - (*phip);
            }                         

         } else {
            *alphap = alpha0;
         }

      } else {
         *alphap = AST__BAD;
      }
   }

/* Return a success status if valid latitude and longitude values were
   found. */
   return (*deltap) != AST__BAD && (*alphap) != AST__BAD ;

}


static AstWinMap *WcsAddRef( FitsStore *store, int *outperm ){
/*
*  Name:
*     WcsAddRef

*  Purpose:
*     Create a WinMap which converts non-celestial axes into absolute
*     physical coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     AstWinMap *WcsAddRef( FitsStore *store, int *outperm )

*  Class Membership:
*     FitsChan

*  Description:
*     A WinMap is created which implements a shift of origin by adding
*     the reference absolute physical coordinates (CRVALi) onto the input 
*     relative physical coordinates. The Mapping only applies to the
*     non-celestial axes. It is assumed that the longitude and latitude
*     axes, if present, will have been sshiffled down to axes 0 and 1
*     respectively. The returned Mapping only applies to the remaining
*     axes.

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     outperm
*        Pointer to an array describing the correspondance between
*        axes in the returned Mapping and the crval values stored
*        in "store". Axis "i" of the Mapping will have reference value
*        crval[ outperm[ 2 + i ] ]. outperm[ 0 ] gives the index of the 
*        longitude axis in "store", and outperm[ 1 ] gives the index of 
*        the latitude axis in "store". 

*  Returned Value:
*     A pointer to the created WinMap or a NULL pointer if an 
*     error occurred.

*  Notes:
*     -  If there are no celestial axes then outperm is ignored, and it
*     is assumed that the reference value for axis "i" is in crval[ i ].
*     -  If an error occurs, a NULL pointer is returned.

*/

/* Local Variables: */
   AstWinMap *new;                 /* The created WinMap */
   int i;                          /* Axis index */
   double *c1_in;                  /* Input corner 1 */
   double *c2_in;                  /* Input corner 1 */
   double *c1_out;                 /* Input corner 1 */
   double *c2_out;                 /* Input corner 1 */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned WinMap pointer. */
   new = NULL;

/* Allocate memory to hold the two corners, in both input and output
   coordinates. */
   c1_in = (double *) astMalloc( sizeof(double)*(size_t)store->noncel);
   c1_out = (double *) astMalloc( sizeof(double)*(size_t)store->noncel);
   c2_in = (double *) astMalloc( sizeof(double)*(size_t)store->noncel);
   c2_out = (double *) astMalloc( sizeof(double)*(size_t)store->noncel);

/* Check these pointers can be used. */
   if( astOK ){

/* Set up two arbitrary corners in the input coordinate system, and the
   corresponding values with the CRVAL values added onto the
   non-celestial axes. */
      if( store->noncel < store->naxis ){
         for( i = 0; i < store->noncel; i++ ){
            c1_in[ i ] = 0.0;
            c2_in[ i ] = 1.0;
            c1_out[ i ] = store->crval[ outperm[ 2 + i ] ];
            c2_out[ i ] = 1.0 + store->crval[ outperm[ 2 + i ] ];
         }

      } else {
         for( i = 0; i < store->noncel; i++ ){
            c1_in[ i ] = 0.0;
            c2_in[ i ] = 1.0;
            c1_out[ i ] = store->crval[ i ];
            c2_out[ i ] = 1.0 + store->crval[ i ];
         }
      }

/* Create the WinMap. */
      new = astWinMap( store->noncel, c1_in, c2_in, c1_out, c2_out, "" );

/* If an error has occurred, attempt to annul the new WinMap. */
      if( !astOK ) new = astAnnul( new );
   
   }

/* Free the memory holding the corners. */
   c1_in = (double *) astFree( (void *) c1_in );
   c1_out = (double *) astFree( (void *) c1_out );
   c2_in = (double *) astFree( (void *) c2_in );
   c2_out = (double *) astFree( (void *) c2_out );

/* Return the WinMap. */
   return new;

}

static int WcsNoWcs( AstFitsChan *this, AstMapping *map, AstFrame *phyfrm, 
                     int naxis, int prim, double *pixref, FitsStore *store ){
/*
*  Name:
*     WcsNoWcs

*  Purpose:
*     Calculate FITS-WCS keyword values describing non-celestial physical
*     coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WcsNoWcs( AstFitsChan *this, AstMapping *map, AstFrame *phyfrm, 
*                   int naxis, int prim, double *pixref, FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates FITS-WCS keyword values describing the
*     relationship between pixel coordinates and a set of simple linear 
*     axes representing physical coordinates. No WCS projection is
*     involved. Secondary axis descriptions are only usable if they can
*     be represented using the same PC matrix as the primary axis 
*     descriptions.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     map
*        Pointer to the Mapping from pixel coordinates to physical
*        coordinates. This Mapping should have equal numbers of input and
*        output coordinates.
*     phyfrm
*        Pointer to physical coordinate Frame.
*     naxis
*        The number of input and output coordinates for "map".
*     prim
*        Are primary axis descriptions being created? Secondary axis
*        descriptions are only stored if the PC matrix is the same as for the
*        primary axis descriptions.
*     pixref
*        Pointer to an array holding the pixel coordinates of the
*        reference point. If these are not known, a NULL pointer can be
*        supplied in which case the reference position is put at the
*        centre of the image (if the image dimensions are stored in the
*        FitsChan), or at the origin of pixel coordinates.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords. If "prim" is zero (i.e. if secondary axis
*        (descriptions are being created) then the "pc" component of the 
*        structure on entry should hold the PC matrix formed when the 
*        primary axis descriptions were created.

*  Returned Value:
*     One if the Mapping could be described by FITS-WCS, zero otherwise.

*  Notes:
*     -  Values which would take the correct default value if omitted
*     from the FITS header are given a bad value in "store".

*/

/* Local Variables: */
   AstPointSet *pset1;      /* PointSet holding pixel coordinates */
   AstPointSet *pset2;      /* PointSet holding physical coordinates */
   char comm[ FITSCARDLEN + 1 ];   /* FITS keyword comment */
   char keyname[ FITSNAMLEN + 1 ]; /* FITS keyword name */
   const char *text;        /* Pointer to textual attribute value */
   double **ptr1;           /* Pointer to pixel coordinate data */
   double **ptr2;           /* Pointer to physical coordinate data */
   double *a;               /* Pointer to next matrix element */
   double *b;               /* Pointer to next matrix element */
   double *dim;             /* Pointer to array holding image dimensions */
   double *matrix;          /* Pointer to matrix array */
   double cdelt;            /* CDELT value for this row */
   double delta;            /* Increment between positions on current axis */
   double maxerr;           /* Max. allowed squared distance between positions */
   double pcval;            /* An element of the PC matrix. */
   double s2;               /* Sum of squared values */
   int i,j;                 /* Loop counts */
   int ok;                  /* Can the PC matrix be used? */
   int ret;                 /* Can the Mapping be described by FITS-WCS? */
   int unit;                /* Is it a unit matrix? */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that the Mapping cannot be
   described by FITS-WCS. */
   ret = 0;

/* Create a PointSet to hold two points. */
   pset1 = astPointSet( 2, naxis, "" );
   ptr1 = astGetPoints( pset1 );

/* Allocate memory to hold the image dimensions, in pixels. */
   dim = (double *) astMalloc( sizeof( double )*(size_t) naxis );

/* Check the pointers can be used. */
   if( astOK ){

/* Get the image dimensions, in pixels. Store a bad value if the image
   size is not known. */
      for( i = 0; i < naxis; i++ ){
         sprintf( keyname, "NAXIS%d", i + 1 );
         if( !astFitsGetF( this, keyname, dim + i ) ) dim[ i ] = AST__BAD;
      }

/* Choose the reference pixel. Since the Mapping is assumed to be
   linear, the choice of reference pixel is not critical. If the "pixref"
   array has been supplied use the supplied cooridnates. Otherwise, if the 
   FitsChan contains NAXIS keywords giving the size of the array, place the
   reference pixel at the middle of the array. Otherwise, put it at the
   origin. Store the reference pixel in the PointSet. */
      if( pixref ){
         for( i = 0; i < naxis; i++ ) ptr1[ i ][ 0 ] = pixref[ i ];

      } else {

         for( i = 0; i < naxis; i++ ){
            if( dim[ i ] != AST__BAD ){
               ptr1[ i ][ 0 ] = 0.5*dim[ i ];
            } else {
               break;
            }
         }   
   
         if( i < naxis ){
            for( i = 0; i < naxis; i++ ) ptr1[ i ][ 0 ] = 0.0;
         }
      }

/* The second point in the PointSet is offset by 1 pixel along every axis
   from the reference pixel. */
      for( i = 0; i < naxis; i++ ) ptr1[ i ][ 1 ] = ptr1[ i ][ 0 ] + 1.0;
    
   }

/* Transform these positions to get the physical coordinates at the
   reference point, and at the neighbouring pixel. */
   pset2 = astTransform( map, pset1, 1, NULL );
   ptr2 = astGetPoints( pset2 );

/* Check the results array can be used. */
   if( astOK ){

/* Copy the reference pixel, and reference point physical coordinates to
   "store". Also find the squared distance between the two points in
   physical coordinates. */
      s2 = 0.0;
      for( i = 0; i < naxis; i++ ){
         if( ptr1[ i ][ 0 ] != AST__BAD && 
             ptr2[ i ][ 0 ] != AST__BAD && 
             ptr2[ i ][ 1 ] != AST__BAD ){

            store->crpix[ i ] = ptr1[ i ][ 0 ];
            store->crval[ i ] = ptr2[ i ][ 0 ];
            delta = ptr2[ i ][ 0 ] - ptr2[ i ][ 1 ];
            s2 += delta*delta;

         } else {
            s2 = AST__BAD;
            break;
         }
      }

/* The criterion used in LinearMap for two physical positions to be
   co-incident is that the squared distance between them should be no
   greater than a supplied limit. Set up the limit as one tenth of a
   pixel (one hundredth when squared). */
      maxerr = ( s2 == AST__BAD )? AST__BAD : 0.01*s2;

   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Allocate memory to hold the matrix elements representing the Mapping. 
   This memory is freed when the supplied FitsStore structure "store" is
   cleaned. */
   matrix = (double *) astMalloc( sizeof(double)*(size_t)(naxis*naxis) );

/* See if the Mapping is linear. If it is, a matrix describing it
   is returned. */
   if( maxerr != AST__BAD && LinearMap( map, naxis, naxis, matrix, dim, maxerr ) ){

/* The returned matrix is now split into two; a diagonal matrix
   containing the CDELT values for each axis, and the PC matrix. The
   product of these two matrices is equal to the matrix returned by
   LinearMap. For the primary axis descriptions, the CDELT values are 
   chosen so that the corresponding row of the PC matrix represents a 
   unit vector. For secondary axis descriptions, the CDELT values are
   chosen to give the same PC matrix as the primary axis descriptions.
   If this is not possible, then the secondary axis descriptions in the
   given Frame cannot be used since the PC matrix applied to all axis
   descriptions. First deal with primary axis descriptions. */
      ok = 1;
      if( prim ){
         if( SplitMat( naxis, matrix, store->cdelt ) ){

/* If any elements now have their default values, store bad values instead.
   Otherwise, note that this is not a unit matrix. */
            unit = 1;
            for( j = 0; j < naxis; j++ ) { 
               a = matrix + j*naxis;
               for( i = 0; i < naxis; i++ ) {
   
                  if( ( i == j && EQUAL( *a, 1.0 ) )||
                      ( i != j && EQUAL( *a, 0.0 ) ) ) {
                     *a = AST__BAD;
                  } else {
                     unit = 0;
                  }
                  a++;
               }
            }

/* If we have a unit vector, store a NULL pointer for the PC matrix in
   "store" to indicate that no PC keywords need be written to the FitsChan.
   In this case we can free the memory holding the matrix. */
            if( unit ){
               store->pc = NULL;
               matrix = (double *) astFree( (void *) matrix );         

/* Otherwise, store a pointer to the matrix in "store". The memory  will
   be released when "store" is no longer needed. */
            } else {
               store->pc = matrix;
            }

/* If we could not create a PC matrix, store a NULL pointer for the PC 
   matrix in "store". */
         } else {
            ok = 0;
            store->pc = NULL;
            matrix = (double *) astFree( (void *) matrix );         
         }

/* Now do secondary axis descriptions. We can only use the secondary axis
   descriptions if they use the same PC matrix as the primary axis
   descriptions. Therefore we need to attempt to choose the CDELT values
   in such a way as to get the required PC matrix. Every element of the 
   matrix returned by LinearMap is divided by the corresponding PC matrix 
   element. These factors give the required CDELT values for each row. They
   can only be used, though, if all the CDELT estimates for reach row
   agree. */
      } else {

/* Store pointers to the first element in the matrix returned by
   LinearMap (i.e. the product of the PC and CDELT matrices), and to the
   first element of the PC matrix. Assume that it is OK to use the
   matrix. */
         a = matrix;
         b = store->pc;
         ok = 1;

/* Check every element of the matrix. */
         for( i = 0; i < naxis*naxis; i++ ){

/* Get the value of the PC matrix element. Bad values are used to
   represent elements of a unit matrix. A NULL pointer to the PC matrix
   means that the whole matrix is a unit matrix. */
            if( b && *b != AST__BAD ){
               pcval = *b;
            } else if( !( i % ( naxis + 1 ) ) ){
               pcval = 1.0;
            } else {
               pcval = 0.0;
            }

/* If the denominator (i.e. the PC element) is zero, the numerator 
   must also be zero, otherwise the 2 matrices are not equivalent. */
            if( pcval == 0.0 ){
               if( *a != 0.0 ){
                  ok = 0;
                  break;
               } else {
                  *a = AST__BAD;
               }

/* If just the numerator is bad, then the 2 matrices are not equivalent. */
            } else if( *a == AST__BAD ){
               ok = 0;
               break;

/* Otherwise, store the factor. This is an estimate of CDELT for the row 
   containing the current element. */
            } else {
               *a /= pcval;
            }

/* Move on to the next element. */
            a++;
            if( b ) b++;
         }

/* Now check that all the estimates of CDELT for each row are equal. If
   they are not, the two matrices are not equivalent. */
         for( j = 0; j < naxis && ok; j++ ){
            a = matrix + j*naxis;
            cdelt = AST__BAD;
            for( i = 0; i < naxis; i++ ){
               if( *a != AST__BAD ){
                  if( cdelt == AST__BAD ){
                     cdelt = *a;
                     store->cdelt[ j ] = cdelt;
                  } else if( !EQUAL( cdelt, *a ) ){
                     ok = 0;
                     break;
                  }
               }
               a++;
            }         
         }
      }

/* If the PC matrix is OK, store values for the other axis-specific 
   keywords. */
      if( ok ){
         for( i = 0; i < naxis; i++ ) {

/* The pre-WCS (AIPS) conventions covering the use of the deprecated CROTA
   keywords required that only celestial axes were allowed to rotate.
   Since this function caters for non-celestial axes, no CROTA keyword
   is required. Therefore store bad values for the CROTA values in
   "store". */
            store->crota[ i ] = AST__BAD;

/* The axis symbols are taken as the CTYPE values. */
            text = astGetSymbol( phyfrm, i );
            store->ctype[ i ] = (char *) astStore( NULL, (void *) text,
                                                strlen( text ) + 1 );

/* The axis labels are taken as the comment for the CTYPE keywords (but only 
   if a label has been set). */
            if( astTestLabel( phyfrm, i ) ){
               text = astGetLabel( phyfrm, i );
               store->ctype_com[ i ] = (char *) astStore( NULL, (void *) text,
                                                      strlen( text ) + 1 );
            } else {
               if( prim ){            
                  sprintf( comm, "Quantity represented by axis %d", i + 1 );
               } else {
                  sprintf( comm, "Secondary description of axis %d", i + 1 );
               }
               store->ctype_com[ i ] = (char *) astStore( NULL, (void *) comm,
                                                      strlen( comm ) + 1 );
            }

/* If a value has been set for the axis units, use it as CUNIT. */
            if( astTestUnit( phyfrm, i ) ){
               text = astGetUnit( phyfrm, i );
               store->cunit[ i ] = (char *) astStore( NULL, (void *) text,
                                                      strlen( text ) + 1 );
            } else {
               store->cunit[ i ] = NULL;
            }
   
         }

/* Store bad values for all the global WCS keywords which only relate to
   celestial coordinate axes. This will prevent the corresponding
   keywords from being written to the FitsChan. */
         store->equinox = AST__BAD;
         store->latpole = AST__BAD;
         store->longpole = AST__BAD;
         store->mjdobs = AST__BAD;
         for( i = 0; i < AST__WCSMX; i++ ) store->projp[ i ] = AST__BAD;

/* Set CRVAL values which are very small compared to the pixel size ot
   zero. */
         for( i = 0; i < naxis; i++ ) {
            if( fabs((store->crval)[i]) < 
                sqrt(DBL_EPSILON)*fabs( (store->cdelt)[i] ) ){
               (store->crval)[ i ] = 0.0;
            }
         }

/* Indicate that the FrameSet has been succesfully represented using
   FITS-WCS. */
         ret = 1;
      }
   }

/* Release the memory holding the point used in the linearity test. */
   dim = (double *) astFree( (void *) dim );

/* If an error has occurred, indicate that the Mapping cannot be
   described by FITS-WCS. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static int WcsPrimary( AstFitsChan *this, FitsStore *store, int cdmat ){
/*
*  Name:
*     WcsPrimary

*  Purpose:
*     Create FITS-WCS keywords holding primary axis descriptions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WcsPrimary( AstFitsChan *this, FitsStore *store, int cdmat )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates a FITS-WCS header holding the keyword values
*     supplied in the given FitsStore structure. The header does not
*     include any secondary axis descriptions.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords.
*     cdmat
*        Should a CDi_j matrix be included in the header? Otherwise, a
*        PCiiijjj matrix is included.

*  Returned Value:
*     One if any keywords were stored in the FitsChan. Zero otherwise.

*  Notes:
*     -  Angular keyword values should be supplied in units of degrees.
*     -  AST__FLOAT keyword values should be supplied as AST__BAD if they take
*     their default values. Such values are not written out.
*     -  AST__STRING keyword value pointers should be supplied as NULL if they 
*     are undefined. Such values are not written out.
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   char *text;              /* General text string */
   char date[ 30 ];         /* Formated date of observation */
   char comment[ FITSCARDLEN + 1 ];/* Comment string */
   char keyname[ FITSNAMLEN + 1 ]; /* Keyword name */
   char sign[2];            /* Fraction's sign character */
   double *pc;              /* Pointer to next PC matrix element value */
   double *cdelt;           /* Pointer to next CDELT value */
   double fd;               /* Fraction of a day */
   static double mjd99=AST__BAD; /* MJD at start of 1999 */
   double val;              /* Next CD element value */
   int axis;                /* Axis index */
   int i;                   /* Axis index */
   int ihmsf[ 4 ];          /* Hour, minute, second, fractional second */
   int iymdf[ 4 ];          /* Year, month, date, fractional day */
   int j;                   /* Axis index */
   int jj;                  /* SlaLib status */
   int ret;                 /* Were any cards added to the FitsChan? */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that nothing has been added
   to the FitsChan. */
   ret = 0;

/* Produce axis-specific keywords for each axis. */
   for( axis = 0; axis < store->naxis; axis++ ){

      sprintf( keyname, "CRVAL%d", axis + 1 );
      sprintf( comment, "Axis %d reference value", axis + 1 );
      SetValue( this, keyname, (void *)( store->crval + axis ), AST__FLOAT, 
                   comment );

      sprintf( keyname, "CRPIX%d", axis + 1 );
      sprintf( comment, "Axis %d pixel value", axis + 1 );
      SetValue( this, keyname, (void *)( store->crpix + axis ), AST__FLOAT, 
                   comment );

      if( !cdmat ) {
         sprintf( keyname, "CDELT%d", axis + 1 );
         sprintf( comment, "Increment per pixel on axis %d", axis + 1 );
         SetValue( this, keyname, (void *)( store->cdelt + axis ), AST__FLOAT, 
                      comment );
      }

      sprintf( keyname, "CROTA%d", axis + 1 );
      sprintf( comment, "DEPRECATED - Rotation of axis %d", axis + 1 );
      SetValue( this, keyname, (void *) (store->crota + axis ), AST__FLOAT, 
                   comment );

      sprintf( keyname, "CTYPE%d", axis + 1 );
      SetValue( this, keyname, (void *) &(store->ctype[ axis ]), AST__STRING,
                   store->ctype_com[ axis ] );

      sprintf( keyname, "CUNIT%d", axis + 1 );
      sprintf( comment, "Units for axis %d", axis + 1 );
      SetValue( this, keyname, (void *) &(store->cunit[ axis ]), AST__STRING,
                   comment );
   }

/* Now store all the global keyword values which refer to all axes. */
/* The rotation matrix... First deal with cases where a CD matrix is
   required. */ 
   if( cdmat ) {

/* Get pointers to the first CDELT and PC value. */
      pc = store->pc;
      cdelt = store->cdelt;

/* Loop round every row of the matrix. */
      for( i = 0; i < store->naxis; i++ ){

/* Loop round every element of this row. */          
         for( j = 0; j < store->naxis; j++ ){

/* Format the keyword name and comment. */
            sprintf( keyname, "CD%.1d_%.1d", i + 1, j + 1 );
            sprintf( comment, "Axis rotation and scaling matrix" );

/* Get the CD element value, replacing missing PC values with their 
   defaults (1.0 for diagonal elements, 0.0 for others). */
            if( !pc || *pc == AST__BAD ) {
               val = ( i == j ) ? (*cdelt) : 0.0;
            } else {
               val = (*pc)*(*cdelt);
               pc++;
            }

/* Store the keyword in the FitsChan. */
            SetValue( this, keyname, (void *) &val, AST__FLOAT, comment );
         }

/* Move on to the next CDELT value. */
         cdelt++;
      }

/* Now write out the matrix as a PC matrix if required. */
   } else {
      if( store->pc ) {
         pc = store->pc;
         for( i = 0; i < store->naxis; i++ ){
            for( j = 0; j < store->naxis; j++ ){
               sprintf( keyname, "PC%.3d%.3d", i + 1, j + 1 );
               sprintf( comment, "Axis rotation matrix" );
               SetValue( this, keyname, (void *) (pc++), AST__FLOAT, comment );
            }
         }
      }
   }

/* Write out the other global keywords. */
   SetValue( this, "LONGPOLE", (void *) &(store->longpole), AST__FLOAT, 
                "Native long. at celestial north pole" );

   SetValue( this, "LATPOLE", (void *) &(store->latpole), AST__FLOAT, 
                "Native lat. at celestial north pole" );

   for( i = 0; i < AST__WCSMX; i++ ){   
      sprintf( keyname, "PROJP%d", i + 1 );
      sprintf( comment, "Projection parameter %d", i + 1 );
      SetValue( this, keyname, (void *)( store->projp + i ), AST__FLOAT, 
                   comment );
   }

   SetValue( this, "EQUINOX", (void *) &(store->equinox), AST__FLOAT, 
                "Epoch of the reference equinox" );

   SetValue( this, "EPOCH", (void *) &(store->equinox), AST__FLOAT, 
                "DEPRECATED - Epoch of the reference equinox" );

   SetValue( this, "MJD-OBS", (void *) &(store->mjdobs), AST__FLOAT, 
                "Modified Julian Date of observation" );

   if( store->mjdobs != AST__BAD ){

/* The format used for the DATE-OBS keyword depends on the value of the
   keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
   Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format. */
      if( mjd99 == AST__BAD ) slaCaldj( 99, 1, 1, &mjd99, &jj );

      if( store->mjdobs < mjd99 ) {

         slaDjcal( 0, store->mjdobs, iymdf, &jj );
         sprintf( date, "%2.2d/%2.2d/%2.2d", iymdf[ 2 ], iymdf[ 1 ], 
                  iymdf[ 0 ] - ( ( iymdf[ 0 ] > 1999 ) ? 2000 : 1900 ) ); 

      } else {

         slaDjcl( store->mjdobs, iymdf, iymdf+1, iymdf+2, &fd, &jj );
         slaDd2tf( 3, fd, sign, ihmsf );
         sprintf( date, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3dZ",
                  iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
                  ihmsf[2], ihmsf[3] ); 
      }

/* Now store the formatted string in the FitsChan. */
      text = date;
      SetValue( this, "DATE-OBS", (void *) &text, AST__STRING,
                   "Date of observation" );
   }

   if( store->radecsys == FK4 ){
      text = "FK4";
   } else if( store->radecsys == FK4NOE ){
      text = "FK4-NO-E";
   } else if( store->radecsys == FK5 ){
      text = "FK5";
   } else if( store->radecsys == GAPPT ){
      text = "GAPPT";
   } else {
      text = NULL;
   }
   SetValue( this, "RADECSYS", (void *) &text, AST__STRING,
                "Reference frame for RA/DEC" );

/* If an error has occurred, indicate that nothing has been added to the 
   FitsChan. */
   if( !astOK ) {
      ret = 0;
   } else {
      ret = 1;
   }

/* Return the answer. */
   return ret;

}

static int WcsValues( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
                      int iphyfrm, int naxis, int prim, FitsStore *store ){
/*
*  Name:
*     WcsValues

*  Purpose:
*     Find FITS-WCS keyword values describing the relationship between
*     two supplied Frames in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WcsValues( AstFitsChan *this, AstFrameSet *fset, int ipixfrm, 
*                    int iphyfrm, int naxis, int prim, FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Gets the values necessary to create a set of FITS-WCS keywords
*     describing a FrameSet. These values are returned in the FitsStore
*     pointer to by "store". They are not written to the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan in which the FITS-WCS keywords will be
*        stored.
*     fset
*        A pointer to the FrameSet.
*     ipixfrm
*        The index of the pixel Frame within the FrameSet.
*     iphyfrm
*        The index of the physical coordinates Frame within the FrameSet.
*     naxis
*        The number of axes in the pixel Frame.
*     prim
*        Is the supplied physical Frame the one from which primary axis
*        descriptions will be derived?
*     store
*        Pointer to a FitsSTore structure in which to store the FITS-WCS
*        keyword values describing the relationship between ipixfrm and
*        iphyfrm.

*  Returned Value:
*     One if the FrameSet could be expressed in terms of FITS_WCS. Zero
*     if not.

*  Notes:
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   AstFrame *phyfrm;        /* Physical coordinate Frame */
   AstMapping *map;         /* Mapping from pixel to abs. physical coords */
   AstMapping *map1;        /* Mapping from pixel to rel. physical coords */
   AstMapping *map2;        /* Mapping from rel. physical to Nat. Sph. coords */
   AstMapping *map3;        /* Mapping from Nat. Sph. to abs. physical coords */
   int ret;                 /* The returned flag */
   void *card;              /* Pointer to initial current card */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initally assume that the FrameSet could not be expressed in terms of
   FITS-WCS. */
   ret = 0;

/* We need to ensure that the current card is not changed by this function
   because it determines the place in the FitsChan at which new keywords
   can be stored. Save a pointer to the current card which will be
   re-instated at the end of this function. */
   card = this->card;

/* Get a pointer to the Mapping from pixel coordinates to physical
   coordinates. */
   map = astGetMapping( fset, ipixfrm, iphyfrm );

/* Check that the number of axes in the physical Frame is the same as the
   number in the pixel Frame. */
   if( astGetNout( map ) == naxis ){

/* Get a pointer to the physical Frame. */
      phyfrm = astGetFrame( fset, iphyfrm );

/* Split the mapping up into a list of serial component mappings, and
   locate the first WcsMap in this list. The first Mapping returned by
   this call is the result of compounding all the Mappings up to (but not
   including) the WcsMap, the second returned Mapping is the (inverted) 
   WcsMap, and the third returned Mapping is anything following the WcsMap. 
   Only proceed if a WcsMap is found. */
      if( SplitMap( naxis, map, &map1, &map2, &map3, astGetClass( this ) ) ){
         ret = WcsWithWcs( this, map1, map2, map3, phyfrm, naxis, prim, store );

/* If no WcsMap was found, we can only encode the FrameSet into FITS-WCS if
   the Mapping from pixel to physical coordinates is linear on every
   axis. */
      } else {
         ret = WcsNoWcs( this, map, phyfrm, naxis, prim, NULL, store );

/* If the physical frame is a SkyFrame, we may be able to desribe it using 
   the linear CAR projection. */
         if( ret ) LinearSky( prim, phyfrm, naxis, store );

      }

   }

/* Annul the Mapping from pixel coordinates to physical coordinates. */
   map = astAnnul( map );

/* Re-instate the original current card. */
   this->card = card;

/* If an error has occurred, indicate that the FrameSet could not be 
   expressed in terms of FITS-WCS. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static int WcsSecondary( AstFitsChan *this, FitsStore *store ){
/*
*  Name:
*     WcsSecondary

*  Purpose:
*     Create FITS-WCS keywords holding secondary axis descriptions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WcsSecondary( AstFitsChan *this, FitsStore *store, )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function adds keywords for any new secondary axis descriptions
*     into the given FitsStore structure. The axis descriptions are only
*     stored if the values supplied in "store" for the global keywords 
*     (e.g. PCiiijjj, EQUINOX, RADECSYS, etc) are the same as those already 
*     in the FitsChan. Duplicate axis descriptions will not be stored (that
*     is, at least one of the keyword values desribing the axis must differ
*     from all the other axis descriptions already stored in the FitsChan).

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords.

*  Returned Value:
*     One if any keywords were stored in the FitsChan. Zero otherwise.

*  Notes:
*     -  Angular keyword values should be supplied in units of degrees.
*     -  AST__FLOAT keyword values should be supplied as AST__BAD if they take
*     their default values. Such values are not written out.
*     -  AST__STRING keyword value pointers should be supplied as NULL if they 
*     are undefined. Such values are not written out.
*     -  A value of zero is returned if an error has already occurred, or
*     if this function should fail for any reason.
*/

/* Local Variables: */
   char keyname[ FITSNAMLEN + 5 ]; /* Keyword name */
   char comment[ FITSCARDLEN + 1 ];/* Comment string */
   char *cval;              /* Value of a AST__STRING keyword */
   char *text;              /* General text string */
   double *pc;              /* Pointer to next PC matrix element value */
   double dval;             /* Value of a AST__FLOAT keyword */
   int ret;                 /* Were any cards added to the FitsChan? */
   int axis;                /* Axis index */
   int hi;                  /* Largest secondary axis description index */
   int lo;                  /* Smallest secondary axis description index */
   int i;                   /* Loop count */
   int j;                   /* Loop count */
   int save;                /* Should the axis description be saved? */

/* Check the global status. */
   if( !astOK ) return 0;

/* Indicate that nothing has yet been added to the FitsChan. */
   ret = 0;

/* Assume to begin with that all the global keywords in "store" are 
   identical to those in "this". */
   save = 1;

/* Check the PC matrix. If "store" contains a PC matrix then "this"
   should also contain a "PC" matrix. */
   if( save ){
      if( store->pc ){
         pc = store->pc;

/* Loop round all the rows and columns of the matrix. */
         for( i = 0; i < store->naxis; i++ ){
            for( j = 0; j < store->naxis; j++ ){

/* Get the name of the keyword for the current matrix element. */
               sprintf( keyname, "PC%.3d%.3d", i + 1, j + 1 );

/* See if this keyword has a value in "this". If not, "dval" retains its
   bad value. */
               dval = AST__BAD;
               astFitsGetF( this, keyname, &dval );

/* If the element value from "this" is not the same as that in "store",
   the matrix is different. Therefore the current axis cannot be used as 
   a secondary axis description. Break out of the loop in this case. */
               if( !EQUAL( dval, *pc ) ){
                  save = 0;
                  break;
               }

/* Move on to the next matrix element in "store". */
               pc++;
            }
            if( !save ) break;
         }

/* If there is no matrix in "store", check that there are no matrix
   keywords in "this". */
      } else {
         if( astKeyFields( this, "PC%3d%3d", 0, NULL, NULL ) ) save = 0;
      }
   }

/* Check the LONGPOLE keyword. */
   if( save ){
      dval = AST__BAD;
      astFitsGetF( this, "LONGPOLE", &dval );
      if( !EQUAL( dval, store->longpole ) ) save = 0;
   }
   
/* Check the LATPOLE keyword. */
   if( save ){
      dval = AST__BAD;
      astFitsGetF( this, "LATPOLE", &dval );
      if( !EQUAL( dval, store->longpole ) ) save = 0;
   }
   
/* Check the PROJPi keywords. */
   for( i = 0; i < AST__WCSMX && save; i++ ){   
      sprintf( keyname, "PROJP%d", i + 1 );
      dval = AST__BAD;
      astFitsGetF( this, keyname, &dval );
      if( !EQUAL( dval, store->projp[ i ] ) ) save = 0;
   }

/* Check the EQUINOX keyword. */
   if( save ){
      dval = AST__BAD;
      astFitsGetF( this, "EQUINOX", &dval );
      if( !EQUAL( dval, store->equinox ) ) save = 0;
   }

/* Check the MJD-OBS keyword. */
   if( save ){
      dval = AST__BAD;
      astFitsGetF( this, "MJD-OBS", &dval );
      if( !EQUAL( dval, store->mjdobs ) ) save = 0;
   }

/* Check the RADECSYS keyword. */
   cval = NULL;
   astFitsGetS( this, "RADECSYS", &cval );
   if( store->radecsys == FK4 ){
      text = "FK4";
   } else if( store->radecsys == FK4NOE ){
      text = "FK4-NO-E";
   } else if( store->radecsys == FK5 ){
      text = "FK5";
   } else if( store->radecsys == GAPPT ){
      text = "GAPPT";
   } else {
      text = NULL;
   }

   if( cval && text ){
      if( strcmp( text, cval ) ) save = 0;
   } else {
      if( cval != text ) save = 0;
   }

/* If the global keyword values supplied in "store" are the same as those
   already in the FitsChan, then go on to check the axis-specific
   keywords. */
   if( save && astOK ){

/* Loop round each each axis. */
      for( axis = 0; axis < store->naxis; axis++ ){

/* The axis description cannot be saved if any of the necessary keyword
   values are missing in "store". Check that all required values are
   available. */
         if( store->crval[ axis ] == AST__BAD ||
             store->crpix[ axis ] == AST__BAD ||
             store->cdelt[ axis ] == AST__BAD ||
             !store->ctype[ axis ] ){
            save = 0;
            break;
         }

/* Check each of the axis descriptions stored in the FitsChan. We break
   out of this loop early if an axis description is found which is identical 
   to the axis description supplied in "store". */
         for( i = 1; i < 10 && astOK; i++ ){

/* Assume initially that the axis description supplied in "store" is the
   same as the axis description in the FitsChan. This is assumed to be the 
   case until a keyword is found which has different values in the FitsChan 
   and in "store". The axis description is only saved if it is different to 
   all others currently in the FitsChan. */
            save = 0;

/* Get the value of the CRVAL keyword for this axis. */
            if( i == 1 ){
               sprintf( keyname, "CRVAL%d", axis + 1 );
            } else {
               sprintf( keyname, "C%.1dVAL%d", i, axis + 1 );
            }
            dval = AST__BAD;
            astFitsGetF( this, keyname, &dval );

/* If it is not the same as the crval value in "store", then the axis
   descriptions differ. */
            if( !EQUAL( dval, store->crval[ axis ] ) ) save = 1;

/* If the axis descriptions are still the same, check the CRPIX keyword
   in the same way. */
            if( !save ){
               if( i == 1 ){
                  sprintf( keyname, "CRPIX%d", axis + 1 );
               } else {
                  sprintf( keyname, "C%.1dPIX%d", i, axis + 1 );
               }
               dval = AST__BAD;
               astFitsGetF( this, keyname, &dval );
               if( !EQUAL( dval, store->crpix[ axis ] ) ) save = 1;
            }               

/* If the axis descriptions are still the same, check the CDELT keyword
   in the same way. */
            if( !save ){
               if( i == 1 ){
                  sprintf( keyname, "CDELT%d", axis + 1 );
               } else {
                  sprintf( keyname, "C%.1dELT%d", i, axis + 1 );
               }
               dval = AST__BAD;
               astFitsGetF( this, keyname, &dval );
               if( !EQUAL( dval, store->cdelt[ axis ] ) ) save = 1;
            }               

/* If the axis descriptions are still the same, check the CTYPE keyword
   in the same way. */
            if( !save ){
               if( i == 1 ){
                  sprintf( keyname, "CTYPE%d", axis + 1 );
               } else {
                  sprintf( keyname, "C%.1dYPE%d", i, axis + 1 );
               }
               cval = NULL;
               astFitsGetS( this, keyname, &cval );
               if( ( cval && !store->ctype[ axis ] ) ||
                   ( !cval && store->ctype[ axis ] ) ||
                   ( cval && store->ctype[ axis ] &&
                     strcmp( cval, store->ctype[ axis ] ) ) ) save = 1;
            }               

/* If the axis descriptions are still the same, check the CUNIT keyword
   in the same way. */
            if( !save ){
               if( i == 1 ){
                  sprintf( keyname, "CUNIT%d", axis + 1 );
               } else {
                  sprintf( keyname, "C%.1dNIT%d", i, axis + 1 );
               }
               cval = NULL;
               astFitsGetS( this, keyname, &cval );
               if( ( cval && !store->cunit[ axis ] ) ||
                   ( !cval && store->cunit[ axis ] ) ||
                   ( cval && store->cunit[ axis ] &&
                     strcmp( cval, store->cunit[ axis ] ) ) ) save = 1;
            }               

/* We have now checked all the keywords defining the secondary axis
   description. If all the keywords had identical values in "store" and 
   FitsChan, then the axis description already exists in the FitsChan, and 
   so does not need to be stored. Break of of the description loop in this 
   case. */
            if( !save ) break;

         }

/* If this axis description does not already exist in the FitsChan, store
   it now. */
         if( save && astOK ){

/* We need to decide on the index to use for this secondary axis
   description (i.e. the value of "m" in "CmVALi", etc). We use a value
   which is one greater than the largest index already in use for this 
   axis, subject to the limit that the largest allowed index is 9. First
   create a template string which will match all secondary axis
   description keywords for this axis (e.g. for the first axis the template
   will be "C%1d%3c1", i.e. "C" followed by a single digit, followed by
   3 upper case letters, followed by "1"). */
            sprintf( keyname, "C%%1d%%3c%d", axis + 1 );

/* Now get the highest value found in the FitsChan for the first integer 
   field in this template. This is the largest index currently in use for
   the current axis. If there are no secondary descriptions yet for this
   axis, use a value of 1 (which will get increment to 2, which is the
   smallest allowed secondary axis index). */
            if( !astKeyFields( this, keyname, 1, &hi, &lo ) ) hi = 1;

/* Move on to use the next index, and check it is not too high. */
            hi++;
            if( hi < 10 ){
               ret = 1;

/* Now create the keyword name and comment for the CRVAL value. */
               sprintf( keyname, "C%.1dVAL%d", hi, axis + 1 );
               sprintf( comment, "Secondary ref. value on axis %d ",
                        axis + 1 );

/* Store these, together with the CRVAL value, in the FitsChan. */
               SetValue( this, keyname, (void *)( store->crval + axis ), 
                            AST__FLOAT, comment );

/* Store the CRPIX value in the same way. */   
               sprintf( keyname, "C%.1dPIX%d", hi, axis + 1 );
               sprintf( comment, "Secondary ref. pixel on axis %d",
                        axis + 1 );
               SetValue( this, keyname, (void *)( store->crpix + axis ),
                            AST__FLOAT, comment );
   
/* Store the CDELT value in the same way. */   
               sprintf( keyname, "C%.1dELT%d", hi, axis + 1 );
               sprintf( comment, "Secondary pixel size on axis %d",
                        axis + 1 );
               SetValue( this, keyname, (void *)( store->cdelt + axis ),
                            AST__FLOAT, comment );
   
/* Store the CTYPE value in the same way. */   
               sprintf( keyname, "C%.1dYPE%d", hi, axis + 1 );
               SetValue( this, keyname, (void *) &(store->ctype[ axis ]), AST__STRING,
                            store->ctype_com[ axis ] );
   
/* Store the CUNIT value in the same way. */   
               sprintf( keyname, "C%.1dNIT%d", hi, axis + 1 );
               sprintf( comment, "Secondary units for axis %d", axis + 1 );
               SetValue( this, keyname, (void *) &(store->cunit[ axis ]), AST__STRING,
                            comment );
            }
         }
      }
   }

/* If an error has occurred, indicate that nothing has been added to the 
   FitsChan. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static AstWinMap *WcsShift( FitsStore *store ){
/*
*  Name:
*     WcsShift

*  Purpose:
*     Create a WinMap which shifts pixels coordinates so that their origin
*     is at the reference pixel.

*  Type:
*     Private function.

*  Synopsis:
*     AstWinMap *WcsShift( FitsStore *store )

*  Class Membership:
*     FitsChan

*  Description:
*     A WinMap is created which implements a shift of origin by subtracting
*     the reference pixel coordinates (CRPIXi) from the input pixel
*     coordinates. 

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.

*  Returned Value:
*     A pointer to the created WinMap or a NULL pointer if an 
*     error occurred.

*  Notes:
*     -  If an error occurs, a NULL pointer is returned.

*/

/* Local Variables: */
   AstWinMap *new;                 /* The created WinMap */
   int i;                          /* Axis index */
   double *c1_in;                  /* Input corner 1 */
   double *c2_in;                  /* Input corner 1 */
   double *c1_out;                 /* Input corner 1 */
   double *c2_out;                 /* Input corner 1 */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned WinMap pointer. */
   new = NULL;

/* Allocate memory to hold the two corners, in both input and oputput
   coordinates. */
   c1_in = (double *) astMalloc( sizeof(double)*(size_t)store->naxis);
   c1_out = (double *) astMalloc( sizeof(double)*(size_t)store->naxis);
   c2_in = (double *) astMalloc( sizeof(double)*(size_t)store->naxis);
   c2_out = (double *) astMalloc( sizeof(double)*(size_t)store->naxis);

/* Check these pointers can be used. */
   if( astOK ){

/* Set up two arbitrary corners in the input coordinate system, and the
   correspinding values with the CRPIX values subtracted off. */
      for( i = 0; i < store->naxis; i++ ){
         c1_in[ i ] = 0.0;
         c2_in[ i ] = 1.0;
         c1_out[ i ] = -store->crpix[ i ];
         c2_out[ i ] = 1.0 - store->crpix[ i ];
      }

/* Create the WinMap. */
      new = astWinMap( store->naxis, c1_in, c2_in, c1_out, c2_out, "" );

/* If an error has occurred, attempt to annul the new WinMap. */
      if( !astOK ) new = astAnnul( new );
   
   }

/* Free the memory holding the corners. */
   c1_in = (double *) astFree( (void *) c1_in );
   c1_out = (double *) astFree( (void *) c1_out );
   c2_in = (double *) astFree( (void *) c2_in );
   c2_out = (double *) astFree( (void *) c2_out );

/* Return the WinMap. */
   return new;

}

static int WcsWithWcs( AstFitsChan *this, AstMapping *map1, AstMapping *map2, 
                       AstMapping *map3, AstFrame *phyfrm, int naxis, 
                       int prim,  FitsStore *store ){
/*
*  Name:
*     WcsWithWcs

*  Purpose:
*     Calculate FITS-WCS keyword values describing celestial physical 
*     coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WcsWithWcs( AstFitsChan *this, AstMapping *map1, AstMapping *map2, 
*                     AstMapping *map3, AstFrame *phyfrm, int naxis, int prim, 
*                     FitsStore *store )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates FITS-WCS keyword values describing the
*     relationship between pixel coordinates and a set of simple axes 
*     including a pair of celestial longitude/latitude axes, together with 
*     an arbitrary number of simple linear axies representing other physical 
*     coordinates. Secondary axis descriptions are only usable if they can
*     be represented using the same PC matrix as the primary axis 
*     descriptions.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     map1
*        Pointer to the Mapping from pixel coordinates to relative physical
*        coordinates. 
*     map2
*        Pointer to the Mapping from relative physical coordinates to
*        native spherical coordinates (any non-celestial axes should be simply
*        copied). This should be a WcsMap.
*     map3
*        Pointer to the Mapping from native spherical coordinates to
*        celestial coordinates (any non-celestial axes should be incremented 
*        by the corresponding reference value). 
*     phyfrm
*        Pointer to physical coordinate Frame.
*     naxis
*        The number of input and output coordinates for each of the
*        supplied Mappings. All the three Mappings should have the same
*        number of input and output coordinates.
*     prim
*        Are primary axis descriptions being created? Secondary axis
*        descriptions are only stored if the PC matrix is the same as for the
*        primary axis descriptions.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords. If "prim" is zero (i.e. if secondary axis
*        (descriptions are being created) then the "pc" component of the 
*        structure on entry should hold the PC matrix formed when the 
*        primary axis descriptions were created.

*  Returned Value:
*     One if the Mapping could be described by FITS-WCS, zero otherwise.

*  Notes:
*     -  Values which would take the correct default value if omitted
*     from the FITS header are given a bad value in "store".

*/

/* Local Variables: */
   AstPointSet *pset1;      /* PointSet holding coordinates */
   AstPointSet *pset2;      /* PointSet holding coordinates */
   AstSkyFrame *skyfrm;     /* Pointer to celestial coordinate Frame */
   double **ptr1;           /* Pointer to coordinate data */
   double **ptr2;           /* Pointer to coordinate data */
   double alphap;           /* Celestial longitude of native north pole */
   double error;            /* Difference between values */
   double latpole;          /* Native latitude of celestial north pole */
   double longpole;         /* Native longitude of celestial north pole */
   double s2;               /* Sum of squared values */
   double sky_lat;          /* Normalised latitude at reference point */
   double sky_long;         /* Normalised longitude at reference point */
   int axis;                /* Axis index within skyframe */
   int axlat;               /* Zero-based index of latitude axis */
   int axlon;               /* Zero-based index of longitude axis */
   int celsys;              /* Is there a celestial coordinate pair? */
   int i;                   /* Loop count */
   int ok;                  /* Can the Mapping be used? */
   int ret;                 /* Can the Mapping be described by FITS-WCS? */

/* Check the global status. */
   if( !astOK ) return 0;

/* Initialise the returned flag to indicate that the Mapping cannot be
   described by FITS-WCS. */
   ret = 0;

/* Create two PointSets to hold two points each. */
   pset1 = astPointSet( 2, naxis, "" );
   ptr1 = astGetPoints( pset1 );
   pset2 = astPointSet( 2, naxis, "" );
   ptr2 = astGetPoints( pset2 );

/* Check the projection parameters and pointers can be used. */
   if( astOK ){

/* The reference point is the origin in the relative physical coordinate
   system produced by "map1". Store the coordinates of the origin, and
   then transform them using the inverse of the "map1" Mapping to get the
   pixel coordinates at the reference point. Store these in "store". */
      for( i = 0; i < naxis; i++ ) {
         ptr1[ i ][ 0 ] = 0.0;
         ptr1[ i ][ 1 ] = 0.0;
      }
      astTransform( map1, pset1, 0, pset2 );
      for( i = 0; i < naxis; i++ ) store->crpix[ i ] = ptr2[ i ][ 0 ];
   }

/* Attempt to calculate and store the keyword values relating to the 
   conversion from pixel coordinates to relative physical coordinates.
   This conversion is defined by "map1" and must be linear. The pixel 
   coordinates of the reference point are fixed at the values found above. */
   if( WcsNoWcs( this, map1, phyfrm, naxis, prim, store->crpix, store ) ){

/* The physical coordinates of the reference point are found by
   transforming the origin of relative physical coordinates into absolute
   physical coordinates using map2 and map3. */
      for( i = 0; i < naxis; i++ ) ptr1[ i ][ 0 ] = 0.0;
      astTransform( map2, pset1, 1, pset2 );
      astTransform( map3, pset2, 1, pset1 );
      for( i = 0; i < naxis; i++ ) store->crval[ i ] = ptr1[ i ][ 0 ];

/* Obtain the indices of the longitude and latitude axes. */
      axlon = astGetWcsAxis( map2, 0 );
      axlat = astGetWcsAxis( map2, 1 );

/* The following only needs to be done if the WcsMap includes a pair of
   longitude/latitude axes. */
      celsys = ( axlon != -1 && axlat != -1 );
      if( celsys ){

/* Normalise the latitude and longitude values at the reference point. */
         sky_long = store->crval[ axlon ];
         sky_lat =  store->crval[ axlat ];

         sky_long = slaDranrm( sky_long );
         sky_lat = slaDrange( sky_lat );

         if ( sky_lat > AST__DPIBY2 ) {
            sky_long += ( sky_long < AST__DPI ) ? AST__DPI : -AST__DPI ;
            sky_lat = AST__DPI - sky_lat;

         } else if ( sky_lat < -AST__DPIBY2 ) {
            sky_long += ( sky_long < AST__DPI ) ? AST__DPI : -AST__DPI;
            sky_lat = -AST__DPI - sky_lat;
         }

         store->crval[ axlon ] = sky_long;
         store->crval[ axlat ] = sky_lat;

/* Store the WCS projection parameters. */
         for( i = 0; i < AST__WCSMX; i++ ){
            if( astTestProjP( map2, i ) ){
               store->projp[ i ] = astGetProjP( map2, i );
            } else {
               store->projp[ i ] = AST__BAD;
            } 
         }

/* If we are producing primary axis descriptions, store a value for the
   deprecated CROTA keyword for the benefit of older FITS readers. */
         Crota( prim, store, naxis, axlon, axlat );
      }

/* Store the squared arc-size of a pixel in celestial coordinates. These are 
   used to determine the accuracy required for the Mappings. */
      s2 = celsys ? store->cdelt[ axlon ]*store->cdelt[ axlon ] +
                    store->cdelt[ axlat ]*store->cdelt[ axlat ] : 0.0;
     
/* We now need to check that the Mapping (map3) which comes after the WCS
   projection (map2) is of the correct form to be represented by FITSWCS. 
   For the celestial axes, map3 must represent a 3D spherical rotation. To
   check this, two native spherical positions separated by a known 
   arc-distance are transformed. If the arc-distance between the transformed 
   points is the same, then the projection is assumed to be acceptable. The 
   positions chosen are the north pole and origin of the native spherical 
   coordinate system which are separated by 90 degrees.
   
   For any non-celestial axes, map3 must be a simple shift of origin. To 
   check this, two positions separated by a known vector are transformed.
   If the vector between the transformed positions is the same, then the
   projection is assumed to be acceptable. 

   Set up the two positions and transform them into physical coordinates. */
      for( i = 0; i < naxis; i++ ) {
         ptr1[ i ][ 0 ] = 0.0;
         ptr1[ i ][ 1 ] = (double) i;
      }
      if( celsys ) ptr1[ axlat ][ 1 ] = AST__DPIBY2;
      astTransform( map3, pset1, 1, pset2 );         

/* Check that the arc distance between the two transformed celestial positions 
   is 90 degrees. The allowed error is 10th of a pixel. */
      error = celsys ? slaDsep( ptr2[ axlon ][ 0 ], ptr2[ axlat ][ 0 ],
                  ptr2[ axlon ][ 1 ], ptr2[ axlat ][ 1 ] ) - AST__DPIBY2 : 0.0;

      if( astOK && error*error <= 0.01*s2 ){

/* Now check that the displacement between the two points is unchanged on
   any non-celestial axes. The error must be less than a tenth of a pixel
   on every axis to be acceptable. */
         ok = 1;
         for( i = 0; i < naxis; i++ ){
            if( i != axlon && i != axlat ){
               error = ( ptr2[ i ][ 1 ] - ptr2[ i ][ 0 ] ) - (double) i;
               if( fabs( error ) >= 0.1*fabs( store->cdelt[ i ] ) ){
                  ok = 0;
                  break;
               }
            }
         }

/* If map3 is an acceptable projection, indicate that we have the returned
   information can be used. */
         if( ok ){
            ret = 1;

/* The other values in "store" need only be assigned values if there is a
   pair of celestial axes. */
            if( celsys ){

/* We now calculate the longitude and latitude of the celestial north pole 
   in native spherical coordinates (using the inverse of map3). These values 
   correspond to the LONGPOLE and LATPOLE keywords. */
               for( i = 0; i < naxis; i++ ){
                  ptr2[ i ][ 0 ] = 0.0;
                  ptr2[ i ][ 1 ] = 0.0;
               }
               ptr2[ axlat ][ 0 ] = AST__DPIBY2;
               astTransform( map3, pset2, 0, pset1 );

/* The default value of the LATPOLE is defined by equation 7 of the
   Fits-WCS paper by Greisen & Calabretta (taking the +ve signs). Find
   this value. */
               if( WcsNatPole( (AstWcsMap *) map2, store->crval[ axlon ], 
                               store->crval[ axlat ], 999.0, ptr1[ axlon ],
                               &alphap, &latpole ) ){

/* If the default value is defined, compare it to the latitude of the 
   north pole found above. If they are equal use a bad value instead to
   prevent an explicit keyword from being added to the FitsChan. */
                  if( EQUAL( ptr1[ axlat ][ 0 ], latpole ) ) {
                     latpole = AST__BAD;
                  } else {
                     latpole = ptr1[ axlat ][ 0 ];
                  }

/* If the default value is not defined, always store an explicit LATPOLE 
   value. */
               } else {
                  latpole = ptr1[ axlat ][ 0 ];
               }

/* The default LONGPOLE value is zero if the celestial latitude at the 
   reference point is greater than the native latitude at the reference
   point. Otherwise, the default is (+ or -) 180 degrees. If LONGPOLE takes 
   the default value, replace it with AST__BAD to prevent an explicit keyword
   being stored in the FitsChan. */
               if( store->crval[ axlat ] > astGetNatLat( map2 ) ){
                  if( EQUAL( ptr1[ axlon ][ 0 ], 0.0 ) ){
                     longpole = AST__BAD;
                  } else {
                     longpole = ptr1[ axlon ][ 0 ];
                  } 
   
               } else {
                  if( EQUAL( ptr1[ axlon ][ 0 ], AST__DPI ) ||
                      EQUAL( ptr1[ axlon ][ 0 ], -AST__DPI ) ){
                     longpole = AST__BAD;
                  } else {
                     longpole = ptr1[ axlon ][ 0 ];
                  } 
               }

/* Store these values. */
               store->longpole = longpole;
               store->latpole = latpole;

/* Get a pointer to the SkyFrame defining the celestial axes. Report an
   error if the Frame is not a SkyFrame. */
               astPrimaryFrame( phyfrm, axlon, (AstFrame **) &skyfrm, &axis );

/* If the current axis belongs to a SkyFrame, we need to check that it is
   the same SkyFrame as any previously found axis. */
               if( !astIsASkyFrame( skyfrm ) && astOK ) {
                  astError( AST__BDFTS, "astWrite(%s): The current Frame in"
                            " the supplied FrameSet is not a SkyFrame.",
                            astGetClass( this ) );
               }

/* Store the CTYPE, EQUINOX, MJDOBS, and RADECSYS values. */
               SkySys( prim, (AstFrame *) skyfrm, astGetWcsType( map2 ), store, axlon,
                       axlat );

/* Convert angular values from radians to degrees. */
               if( store->crval[ axlat ] != AST__BAD ) store->crval[ axlat ] *= AST__DR2D;
               if( store->crval[ axlon ] != AST__BAD ) store->crval[ axlon ] *= AST__DR2D;
               if( store->cdelt[ axlat ] != AST__BAD ) store->cdelt[ axlat ] *= AST__DR2D;
               if( store->cdelt[ axlon ] != AST__BAD ) store->cdelt[ axlon ] *= AST__DR2D;
               if( store->crota[ axlat ] != AST__BAD ) store->crota[ axlat ] *= AST__DR2D;
               if( store->crota[ axlon ] != AST__BAD ) store->crota[ axlon ] *= AST__DR2D;
               if( store->longpole != AST__BAD ) store->longpole *= AST__DR2D;
               if( store->latpole != AST__BAD ) store->latpole *= AST__DR2D;

/* Save the celestial co-ordinate axis indices. */
               store->axlat = axlat;
               store->axlon = axlon;

/* Annul the SkyFrame. */
               skyfrm = astAnnul( skyfrm );
            }
         }
      }
   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* If an error has occurred, indicate that the Mapping cannot be
   described by FITS-WCS. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static int Write( AstChannel *this_channel, AstObject *object ) {
/*
*  Name:
*     Write

*  Purpose:
*     Write an Object to a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int Write( AstChannel *this, AstObject *object )

*  Class Membership:
*     FitsChan member function (over-rides the astWrite method
*     inherited from the Channel class).

*  Description:
*     This function writes an Object to a FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     object
*        Pointer to the Object which is to be written.

*  Returned Value:
*     The number of Objects written to the FitsChan by this invocation of 
*     astWrite.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.

*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   char banner[ FITSCARDLEN - FITSNAMLEN + 1 ];
                                 /* Buffer for begin/end banner */
   int comm;                     /* Value of Comm attribute */
   int encode;                   /* FITS encoding scheme to use */
   int ret;                      /* Number of objects read */
   void *card0;                  /* Pointer to original current card */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the number of objects read by this invocation. */
   ret = 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* The original current card is re-instated at the end if no object
   is written. Save a pointer to the current card. */
   card0 = this->card;

/* Get the encoding scheme used by the FitsChan. */
   encode = astGetEncoding( this );

/* First deal with cases where we are writing to a FitsChan in which AST 
   objects are encoded using native AST-specific keywords... */
   if( encode == NATIVE_ENCODING ){

/* Increment the nesting level which keeps track of recursive
   invocations of this function. */
      write_nest++;

/* Initialise the current indentation level for top-level objects. */
      if ( !write_nest ) current_indent = 0;

/* Obtain the value of the Comm attribute. */
      comm = astGetComment( this );

/* If this is the top-level invocation (i.e. we are about to write out
   a new top-level Object), then prefix it with a blank FITS line and
   an appropriate banner of FITS comments, unless comments have been
   suppressed. */
      if ( !write_nest && comm ) {
         astFitsSetCom( this, "        ", "", 0 );
         MakeBanner( 
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                     "", "", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
         MakeBanner( HEADER_TEXT, astGetClass( object ), " object", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
         MakeBanner(
"................................................................",
                     "", "", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
      }

/* Invoke the parent astWrite method to write out the Object data. */
      (*parent_write)( this_channel, object );

/* Append a banner of FITS comments to the object data, as above, if
   necessary. */
      if ( !write_nest && comm ) {
         MakeBanner(
"................................................................",
                     "", "", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
         MakeBanner( FOOTER_TEXT, astGetClass( object ), " object", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
         MakeBanner(
"----------------------------------------------------------------",
                     "", "", banner );
         astFitsSetCom( this, "COMMENT", banner, 0 );
      }

/* Return the nesting level to its previous value. */
      write_nest--;

/* Indicate that an object has been written. */
      ret = 1;

/* Now deal with cases where we are writing to a FitsChan in which AST 
   objects are encoded using FITS World Coordinate System (WCS) keywords... */
   } else if( encode == FITSWCS_ENCODING ){
      ret = WriteWcs( this_channel, object );
   
/* Now deal with cases where we are writing to a FitsChan in which AST 
   objects are encoded using IRAF FITS World Coordinate System keywords... */
   } else if( encode == FITSIRAF_ENCODING ){
      ret = WriteIraf( this_channel, object );
   
/* Now deal with cases where we are writing to a FitsChan in which AST 
   objects are encoded using STScI DSS keywords... */
   } else if( encode == DSS_ENCODING ){
      ret = WriteDSS( this_channel, object );
   
/* Report an error if the encoding system is not known. */
   } else if( astOK ){
      astError( AST__INTER, "astWrite(%s): AST internal programming error - "
                "FITS encoding scheme %d not yet supported by the FitsChan "
                "class.", astGetClass( this ) );
   }

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* If no object was written, re-instate the original current card. */
   if( !ret ) this->card = card0;

/* Return the answer. */
   return ret;
}

static void WriteBegin( AstChannel *this_channel, const char *class,
                        const char *comment ) {
/*
*  Name:
*     WriteBegin

*  Purpose:
*     Write a "Begin" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteBegin( AstChannel *this, const char *class,
*                      const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected astWriteBegin
*     method inherited from the Channel class).

*  Description:
*     This function writes a "Begin" data item to the data sink
*     associated with a FitsChan, so as to begin the output of a new
*     Object definition.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class to which the Object belongs.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "Begin"
*        item. Normally, this will describe the purpose of the Object.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the FitsChan supplied.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char buff[ FITSCARDLEN - FITSNAMLEN + 1 ];
                                 /* Character buffer */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Increment the indentation level for comments. */
   current_indent += INDENT_INC;

/* If we are not beginning a top-level Object definition, and helpful
   information has not been suppressed, generate an indented comment
   to mark the "Begin" item and write it to the FitsChan as a comment
   card with a blank keyword. */
   if ( write_nest && ( astGetFull( this ) >= 0 ) ) {
      MakeIndentedComment( current_indent, '+', "Beginning of ", class, buff );
      astFitsSetCom( this, "        ", buff, 0 );
   }

/* Create a unique FITS keyword for this "Begin" item, basing it on
   "BEGAST". */
   CreateKeyword( this, "BEGAST", keyword );

/* Generate a pre-quoted version of the class name. */
   PreQuote( class, buff );

/* Write the "Begin" item to the FitsChan as a keyword and string
   value. */
   astFitsSetS( this, keyword, buff,
                     astGetComment( this ) ? comment : NULL, 0 );

/* Clear the count of items written. */
   items_written = 0;
}

static void WriteDouble( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         double value, const char *comment ) {
/*
*  Name:
*     WriteDouble

*  Purpose:
*     Write a double value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteDouble( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       double value, const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected
*     astWriteDouble method inherited from the Channel class).

*  Description:
*     This function writes a named double value, representing the
*     value of a class instance variable, to the data sink associated
*     with a FitsChan. It is intended for use by class "Dump"
*     functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        FitsChan's Full attribute is set - either to permit all
*        values to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the FitsChan supplied and the setting of its
*        Comm attribute.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Use the "set" and "helpful" flags, along with the FitsChan's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful ) ) {

/* Create a unique FITS keyword from the name supplied. */
      CreateKeyword( this, name, keyword );

/* Write the value to the FitsChan as a keyword and value */
      astFitsSetF( this, keyword, value,
                       astGetComment( this ) ? comment : NULL, 0 );

/* If the value is not "set", replace the card just written by a COMMENT
   card containing the text of the card as the comment. */
      if( !set ) MakeIntoComment( this, "astWrite", astGetClass( this ) );

/* Increment the count of items written. */
      items_written++;
   }
}

static void WriteEnd( AstChannel *this_channel, const char *class ) {
/*
*  Name:
*     WriteEnd

*  Purpose:
*     Write an "End" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteEnd( AstChannel *this, const char *class )

*  Class Membership:
*     FitsChan member function (over-rides the protected astWriteEnd
*     method inherited from the Channel class).

*  Description:
*     This function writes an "End" data item to the data sink
*     associated with a FitsChan. This item delimits the end of an
*     Object definition.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     class
*        Pointer to a constant null-terminated string containing the
*        class name of the Object whose definition is being terminated
*        by the "End" item.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char buff[ FITSCARDLEN - FITSNAMLEN + 1 ];
                                 /* Character buffer */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Create a unique FITS keyword for this "End" item, basing it on
   "ENDAST". */
   CreateKeyword( this, "ENDAST", keyword );

/* Generate a pre-quoted version of the class name. */
   PreQuote( class, buff );

/* Write the "End" item to the FitsChan as a keyword and string
   value. */
   astFitsSetS( this, keyword, buff,
                     astGetComment( this ) ? "End of object definition" : NULL,
                     0 );

/* If we are not ending a top-level Object definition, and helpful
   information has not been suppressed, generate an indented comment
   to mark the "End" item and write it to the FitsChan as a comment
   card with a blank keyword. */
   if ( write_nest && ( astGetFull( this ) >= 0 ) ) {
      MakeIndentedComment( current_indent, '-', "End of ", class, buff );
      astFitsSetCom( this, "        ", buff, 0 );
   }

/* Decrement the indentation level for comments. */
   current_indent -= INDENT_INC;
}

static void WriteInt( AstChannel *this_channel, const char *name,
                      int set, int helpful,
                      int value, const char *comment ) {
/*
*  Name:
*     WriteInt

*  Purpose:
*     Write an int value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteInt( AstChannel *this, const char *name,
*                    int set, int helpful,
*                    int value, const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected
*     astWriteInt method inherited from the Channel class).

*  Description:
*     This function writes a named int value, representing the
*     value of a class instance variable, to the data sink associated
*     with a FitsChan. It is intended for use by class "Dump"
*     functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        FitsChan's Full attribute is set - either to permit all
*        values to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the FitsChan supplied and the setting of its
*        Comm attribute.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Use the "set" and "helpful" flags, along with the FitsChan's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful ) ) {

/* Create a unique FITS keyword from the name supplied. */
      CreateKeyword( this, name, keyword );

/* Write the value to the FitsChan as a keyword and value */
      astFitsSetI( this, keyword, value,
                   astGetComment( this ) ? comment : NULL, 0 );

/* If the value is not "set", replace the card just written by a COMMENT
   card containing the text of the card as the comment. */
      if( !set ) MakeIntoComment( this, "astWrite", astGetClass( this ) );

/* Increment the count of items written. */
      items_written++;
   }
}

static void WriteIsA( AstChannel *this_channel, const char *class,
                      const char *comment ) {
/*
*  Name:
*     WriteIsA

*  Purpose:
*     Write an "IsA" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteIsA( AstChannel *this, const char *class,
*                    const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected astWriteIsA
*     method inherited from the Channel class).

*  Description:
*     This function writes an "IsA" data item to the data sink
*     associated with a FitsChan. This item delimits the end of the
*     data associated with the instance variables of a class, as part
*     of an overall Object definition.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class whose data are terminated by the "IsA"
*        item.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "IsA"
*        item. Normally, this will describe the purpose of the class
*        whose data are being terminated.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the FitsChan supplied.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char buff[ FITSCARDLEN - FITSNAMLEN + 1 ];
                                 /* Character buffer */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Output an "IsA" item only if there has been at least one item
   written since the last "Begin" or "IsA" item, or if the Full
   attribute for the Channel is greater than zero (requesting maximum
   information). */
   if ( items_written || astGetFull( this ) > 0 ) {

/* Create a unique FITS keyword for this "IsA" item, basing it on
   "ISA". */
      CreateKeyword( this, "ISA", keyword );

/* Generate a pre-quoted version of the class name. */
      PreQuote( class, buff );

/* Write the "IsA" item to the FitsChan as a keyword and string
   value. */
      astFitsSetS( this, keyword, buff,
                        astGetComment( this ) ? comment : NULL, 0 );

/* If helpful information has not been suppressed, generate an
   indented comment to mark the "IsA" item and write it to the
   FitsChan as a comment card with a blank keyword. */
      if ( astGetFull( this ) >= 0 ) {
         MakeIndentedComment( current_indent, '.', "Class boundary", "",
                              buff );
         astFitsSetCom( this, "        ", buff, 0 );
      }
   }

/* Clear the count of items written. */
   items_written = 0;
}

static void WriteObject( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         AstObject *value, const char *comment ) {
/*
*  Name:
*     WriteObject

*  Purpose:
*     Write an Object value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteObject( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       AstObject *value, const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected
*     astWriteObject method inherited from the Channel class).

*  Description:
*     This function writes a named Object value, representing the
*     value of a class instance variable, to the data sink associated
*     with a FitsChan. It is intended for use by class "Dump"
*     functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        FitsChan's Full attribute is set - either to permit all
*        values to be shown, or to suppress non-essential information
*        entirely.
*     value
*        A pointer to the Object to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the FitsChan supplied and the setting of its
*        Comm attribute.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Use the "set" and "helpful" flags, along with the FitsChan's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful ) ) {

/* Create a unique FITS keyword from the name supplied. */
      CreateKeyword( this, name, keyword );

/* Write the value to the FitsChan as a keyword and a blank string value, 
   not pre-quoted (this "null" value indicates that an Object description 
   follows). */
      astFitsSetS( this, keyword, "",
                        astGetComment( this ) ? comment : NULL, 0 );

/* If the value is "set", write out the Object description. */
      if ( set ) {
         astWrite( this, value );

/* If the value is not set, replace the card just written to the FitsChan
   by COMENT card containing the keyword and blank string value (do not
   write out the Object description). */
      } else {
         MakeIntoComment( this, "astWrite", astGetClass( this ) );
      }

/* Increment the count of items written. */
      items_written++;
   }
}

static void WriteToSink( AstFitsChan *this ){
/*
*  Name:
*     WriteToSink

*  Purpose:
*     Write the contents of the FitsChan out through the sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteToSink( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Each card in the FitsChan is passed in turn to the sink function
*     specified when the FitsChan was created. If no sink function was 
*     provided, the cards are not written out. Cards marked as having been 
*     read into an AST object are not written out.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Notes:
*     -  The current card is left unchanged.

*/

/* Local Variables: */
   char card[ FITSCARDLEN + 1]; /* Buffer for header card */
   int icard;                   /* Current card index on entry */
   int old_skipping;            /* Original value of external variable Skipping */

/* Check the global status. */
   if( !astOK ) return;

/* Only proceed if a sink function and wrapper were supplied. */
   if( this->sink && this->sink_wrap ){

/* Store the current card index. */
      icard = astGetCard( this );

/* Indicate that cards which have been read into an AST object should skipped 
   over by the functions which navigate the linked list of cards. */
   old_skipping = Skipping;
      Skipping = 1;

/* Ensure that the first card in the FitsChan will be the next one to be
   read. */
      astSetCard( this, 1 );

/* Loop round obtaining and writing out each card, until all cards have been 
   processed. */
      while( !astFitsEof( this ) && astOK ){

/* Get the current card, and write it out through the sink function.
   The call to astFindFits increments the current card. */
         if( astFindFits( this, "%f", card, 1 ) ) {
            ( *this->sink_wrap )( *this->sink, card );
         }

      }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
      Skipping = old_skipping;

/* Set the current card index back to what it was on entry. */
      astSetCard( this, icard );
   }
}

static void WriteString( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         const char *value, const char *comment ) {
/*
*  Name:
*     WriteString

*  Purpose:
*     Write a string value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WriteString( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       const char *value, const char *comment )

*  Class Membership:
*     FitsChan member function (over-rides the protected
*     astWriteString method inherited from the Channel class).

*  Description:
*     This function writes a named string value, representing the
*     value of a class instance variable, to the data sink associated
*     with a FitsChan. It is intended for use by class "Dump"
*     functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        FitsChan's Full attribute is set - either to permit all
*        values to be shown, or to suppress non-essential information
*        entirely.
*     value
*        Pointer to a constant null-terminated string containing the
*        value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the FitsChan supplied and the setting of its
*        Comm attribute.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure. */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */
   char string[ FITSCARDLEN - FITSNAMLEN - 3 ];
                                 /* Buffer for pre-quoted string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Use the "set" and "helpful" flags, along with the FitsChan's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful ) ) {

/* Create a unique FITS keyword from the name supplied. */
      CreateKeyword( this, name, keyword );

/* Pre-quote the string value to protect any trailing spaces, etc. if
   necessary. */
      PreQuote( value, string );

/* Write the value to the FitsChan as a keyword and value */
      astFitsSetS( this, keyword, string,
                   astGetComment( this ) ? comment : NULL, 0 );

/* If the value is not "set", replace the card just written by a COMMENT
   card containing the text of the card as the comment. */
      if( !set ) MakeIntoComment( this, "astWrite", astGetClass( this ) );

/* Increment the count of items written. */
      items_written++;
   }
}

static int WriteDSS( AstChannel *this_channel, AstObject *object ) {
/*
*  Name:
*     WriteDSS

*  Purpose:
*     Write an Object to a DSS-encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WriteDSS( AstChannel *this, AstObject *object )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function writes an Object to a DSS encoded FitsChan.
*     The Object must be a FrameSet; the base Frame is assumed to 
*     be the pixel Frame, and the current Frame is assumed to be the
*     celestial coordinate Frame. The current Frame must therefore be a
*     SkyFrame with a value of FK4 or FK5 for it's "system" attribute.
*     The Mapping between base and current Frames must be single DssMap, 
*     optionally preceded by a WinMap (to magnify or move the pixel 
*     coordinate system). If any of these conditions are not met, nothing
*     is written to the FitsChan (no error is reported).

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     object
*        Pointer to the Object which is to be written.

*  Returned Value:
*     The number of Objects written to the FitsChan by this invocation.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.

*/

/* Local Variables: */
   AstFitsChan *fits_full;       /* Pointer to the FitsChan holding DSS cards */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   AstFrame *frm;                /* The current Frame in the FrameSet */
   AstFrameSet *fset;            /* The FrameSet to be written out */
   AstMapping *mapping;          /* The FrameSet mapping */
   AstMapping *smap;             /* The simplified FrameSet mapping */
   char *comment;                /* The keyword comment */
   char *name;                   /* The keyword name */
   char *value;                  /* The keyword value */
   char buffer[ FITSCARDLEN + 1 ]; /* A buffer for the keyword value */
   char card[ FITSCARDLEN + 1 ]; /* The current card */
   const char *class;            /* Pointer to class string */
   const char *system;           /* The coordinate system in the current Frame */
   double ep;                    /* Epoch from current Frame */
   double eq;                    /* Equinox from current Frame */
   int ret;                      /* Number of objects read */
   int type;                     /* Keyword data type */
   void *buff;                   /* Address of the buffer */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the number of objects read by this invocation. */
   ret = 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Only proceed if the supplied object is a FrameSet. */
   if( astIsAFrameSet( object ) ){

/* Get a pointer to the FrameSet. */
      fset = (AstFrameSet *) object;

/* Get the Mapping between base (i.e. pixel) and current (i.e. equatorial)
   Frames. */
      mapping = astGetMapping( fset, AST__BASE, AST__CURRENT  );

/* Attempt to simplify the Mapping. */
      smap = astSimplify( mapping );

/* The Mapping may only be used if it simplifies to a simple DssMap. */
      if( astIsADssMap( smap ) ){

/* Get the class string (for inclusion in error messages). */
         class = astGetClass( this );

/* Get the FitsChan describing the DssMap. */
         fits_full = astDssFits( smap );

/* We now copy the cards from this FitsChan to the supplied Fitshan. */
         buff = (void *) buffer;
         astClearCard( fits_full );
         while( astFindFits( fits_full, "%f", card, 1 ) ){

/* Get the keyword name from the card just read from the DssMap. */
            type = astSplit( card, &name, &value, &comment, "astWrite", class );

/* Convert the string value to its binary representation. */
            CnvType( AST__STRING, (void *) value, strlen( value ) + 1, 
                     type, buff, name, "astWrite", class );

/* Store the keyword value. Any existing value for this keyword is
   over-written unless the card is a comment card. */
            SetValue( this, name, (void *) buff, type, comment );
         }

/* Get a pointer to the current Frame in the FrameSet. */
         frm = astGetFrame( fset, AST__CURRENT );

/* If it is a SkyFrame, get the Equinox and Epoch values as MJD's, and
   then convert them to a Julian or Beseelian epoch depending on whether
   the system is FK4 or FK5. Write these values out to the FitsChan. */
         if( astIsASkyFrame( frm ) ) {
            system = astGetC( frm, "system" );
            ep = astGetEpoch( frm );
            eq = astGetEquinox( frm );

            if( !strcmp( system, "FK4" ) ||
                !strcmp( system, "FK4-NO-E" ) ||
                !strcmp( system, "FK4_NO_E" ) ) {
               ep = slaEpb( ep );
               eq = slaEpb( eq );
               SetValue( this, "EQUINOX", (void *) &eq, AST__FLOAT,
                         "Besselian reference frame equinox" );
               SetValue( this, "EPOCH", (void *) &ep, AST__FLOAT,
                         "Besselian epoch of the plate" );

            } else if( !strcmp( system, "FK5" ) ||
                       !strcmp( system, "EQUATORIAL" ) ) {
               ep = slaEpj( ep );
               eq = slaEpj( eq );
               SetValue( this, "EQUINOX", (void *) &eq, AST__FLOAT,
                         "Julian reference frame equinox" );
               SetValue( this, "EPOCH", (void *) &ep, AST__FLOAT,
                         "Julian epoch of the plate" );
            }
         }

/* Annul the pointer to the Frame. */
         frm = astAnnul( frm );

/* Indicate that an object has been written to the FitsChan. */
         ret = 1;

/* Annull things. */
         fits_full = astAnnul( fits_full );

      }

      smap = astAnnul( smap );
      mapping = astAnnul( mapping );

   }

/* If the Object was written to the FitsChan, set the current card to
   end-of-file. */
   if( ret ) astSetCard( this, INT_MAX );

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;
}

static int WriteIraf( AstChannel *this_channel, AstObject *object ) {
/*
*  Name:
*     WriteIraf

*  Purpose:
*     Write an Object to a FITS-IRAF encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WriteIraf( AstChannel *this, AstObject *object )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function writes an Object to a FITS-IRAF encoded FitsChan.
*     This is like FITS-WCS except:
*        o  No secondary axis descriptions are created
*        o  A CD matrix is produced instead of a PC matrix
*        o  No CDELT keywords are created
*        o  A smaller number of sky projections can be used
*        o  No projection parameter keywords are created
*        o  Only equatorial, galactic and ecliptic sky coords can be used

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     object
*        Pointer to the Object which is to be written.

*  Returned Value:
*     The number of Objects written to the FitsChan by this invocation.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.

*/

/* Local Variables: */
   AstFrame *pixfrm;             /* The pixel coordinate Frame */
   AstFrameSet *fset;            /* The FrameSet to be written out */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   FitsStore *store;             /* Pointer to structure holding keyword values */
   int iphyfrm;                  /* Index of the physical coordinate Frame */
   int ipixfrm;                  /* Index of the pixel coordinate Frame */
   int naxis;                    /* No. of axes in the pixel Frame */
   int ret;                      /* Number of objects read */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the number of objects read by this invocation. */
   ret = 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Only proceed if the supplied object is a FrameSet. */
   if( astIsAFrameSet( object ) ){

/* Get a pointer to the FrameSet. */
      fset = (AstFrameSet *) object;

/* Obtain the index of the Frame to use as the pixel frame (given by 
   the FrameSet attribute Base),  and get a pointer to it. */
      ipixfrm = astGetBase( fset );
      pixfrm = astGetFrame( fset, ipixfrm );

/* Get the number of axes in the pixel frame. */
      naxis = astGetNin( pixfrm );

/* Obtain the index of the Frame to use as the physical coordinate frame. */
      iphyfrm = astGetCurrent( fset );

/* Set the current card in the FitsChan to the card following the last IRAF
   card. The current card is unchanged if there are no IRAF keywords in the
   FitsChan. New cards will be inserted in front of the current card unless 
   the FitsChan already contains a card for the same keyword. */
      FindIraf( this );

/* Allocate memory to hold the IRAF keyword values. */
      store = (FitsStore *) astMalloc( sizeof( FitsStore ) );

/* Create the axis descriptions, based on the specified physical Frame. */
      ret = DescIraf( this, fset, ipixfrm, iphyfrm, naxis, store );

/* Clean and free the memory holding the IRAF keyword values. */
      CleanFits( store );
      store = (FitsStore *) astFree( (void *) store );

/* Annul the pointer to the pixel Frame. */
      pixfrm = astAnnul( pixfrm );

   }

/* If the Object was written to the FitsChan, set the current card to
   end-of-file. */
   if( ret ) astSetCard( this, INT_MAX );

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;
}

static int WriteWcs( AstChannel *this_channel, AstObject *object ) {
/*
*  Name:
*     WriteWcs

*  Purpose:
*     Write an Object to a FITS-WCS encoded FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int WriteWcs( AstChannel *this, AstObject *object )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function writes an Object to a FITS-WCS encoded FitsChan.
*     Except for the pixel Frame (specified by attribute Base),
*     every Frame in the FrameSet is assumed to give an alternative 
*     set of physical coordinates which may be used to describe the 
*     pixel array (any other Frames should be removed from the FrameSet
*     before calling astWrite). The primary axis descriptions are produced 
*     from the Frame specified by attribute Current. Secondary axis 
*     descriptions are created form any other Frames in the FrameSet. 
*     Duplicate axis descriptions are not stored in the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     object
*        Pointer to the Object which is to be written.

*  Returned Value:
*     The number of Objects written to the FitsChan by this invocation.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.

*/

/* Local Variables: */
   AstFrame *pixfrm;             /* The pixel coordinate Frame */
   AstFrameSet *fset;            /* The FrameSet to be written out */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   FitsStore *store;             /* Pointer to structure holding keyword values */
   int i;                        /* Index of next physical Frame */
   int iphyfrm;                  /* Index of the physical coordinate Frame */
   int ipixfrm;                  /* Index of the pixel coordinate Frame */
   int naxis;                    /* No. of axes in the pixel Frame */
   int ret;                      /* Number of objects read */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the number of objects read by this invocation. */
   ret = 0;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Only proceed if the supplied object is a FrameSet. */
   if( astIsAFrameSet( object ) ){

/* Get a pointer to the FrameSet. */
      fset = (AstFrameSet *) object;

/* Obtain the index of the Frame to use as the pixel frame (given by 
   the FrameSet attribute Base),  and get a pointer to it. */
      ipixfrm = astGetBase( fset );
      pixfrm = astGetFrame( fset, ipixfrm );

/* Get the number of axes in the pixel frame. */
      naxis = astGetNin( pixfrm );

/* Obtain the index of the Frame to use as the primary physical coordinate 
   frame. */
      iphyfrm = astGetCurrent( fset );

/* Set the current card in the FitsChan to the card following the last WCS
   card. The current card is unchanged if there are no WCS keywords in the
   FitsChan. New cards will be inserted in front of the current card unless 
   the FitsChan already contains a card for the same keyword. */
      FindWcs( this );

/* Allocate memory to hold the WCS-FITS keyword values. */
      store = (FitsStore *) astMalloc( sizeof( FitsStore ) );

/* Create the primary axis descriptions, based on the specified physical
   Frame. Only continue if the primary axis descriptions were produced
   succesfully. */
      if( DescWcs( this, fset, ipixfrm, iphyfrm, naxis, 1, store ) ){

/* Indicate that something has been written to the FitsChan. */
         ret = 1;

/* Now go through any other Frames in the FrameSet, producing secondary
   axis descriptions. Do not do this if a CD matrix was created as part
   of the primary description. */
#if 0
         if( !astGetCDMatrix( this ) ) {
#else
         if ( 1 ) {
#endif
            for( i = 1; i <= astGetNframe( fset ); i++ ){

/* Ignore the pixel Frame and the primary physical coordinate Frame. */
               if( i != ipixfrm && i != iphyfrm ){

/* Create any secondary axis descriptions required to describe the
   Frame with index "i". */
                  (void) DescWcs( this, fset, ipixfrm, i, naxis, 0, store );
               }
            }
         }
      }

/* Clean and free the memory holding the WCS-FITS keyword values. */
      CleanFits( store );
      store = (FitsStore *) astFree( (void *) store );

/* Annul the pointer to the pixel Frame. */
      pixfrm = astAnnul( pixfrm );

   }

/* If the Object was written to the FitsChan, set the current card to
   end-of-file. */
   if( ret ) astSetCard( this, INT_MAX );

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* Card. */
/* ===== */
/*
*att++
*  Name:
*     Card

*  Purpose:
*     Index of current FITS card in a FitsChan.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute gives the index of the "current" FITS header card
*     within a FitsChan, the first card having an index of 1. The
c     choice of current card affects the behaviour of functions that
f     choice of current card affects the behaviour of routines that
c     access the contents of the FitsChan, such as astDelFits,
c     astFindFits and astPutFits.
f     access the contents of the FitsChan, such as AST_DELFITS,
f     AST_FINDFITS and AST_PUTFITS.
*
*     A value assigned to Card will position the FitsChan at any
*     desired point, so that a particular card within it can be
*     accessed. Alternatively, the value of Card may be enquired in
*     order to determine the current position of a FitsChan.
*
*     The default value of Card is 1. This means that clearing
c     this attribute (using astClear) effectively "rewinds" the
f     this attribute (using AST_CLEAR) effectively "rewinds" the
*     FitsChan, so that the first card is accessed next.  If Card is
*     set to a value which exceeds the total number of cards in the
*     FitsChan (as given by its Ncards attribute), it is regarded as
*     pointing at the "end-of-file". In this case, the value returned
*     in response to an enquiry is always one more than the number of
*     cards in the FitsChan.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/

/* Encoding. */
/* ========= */
/*
*att++
*  Name:
*     Encoding

*  Purpose:
*     System for encoding Objects as FITS headers.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the encoding system to use when AST
*     Objects are stored as FITS header cards in a FitsChan. It
c     affects the behaviour of the astWrite and astRead functions when
f     affects the behaviour of the AST_WRITE and AST_READ routines when
*     they are used to transfer any AST Object to or from an external
*     representation consisting of FITS header cards (i.e. whenever a
*     write or read operation is performed using a FitsChan as the I/O
*     Channel).
*
*     There are several ways (conventions) by which coordinate system
*     information may be represented in the form of FITS headers and
*     the Encoding attribute is used to specify which of these should
*     be used. The encoding options available are outlined in the
*     "Encodings Available" section below, and in more detail in the
*     sections which follow.
*
*     Encoding systems differ in the range of possible Objects
*     (e.g. classes) they can represent, in the restrictions they
*     place on these Objects (e.g. compatibility with some
*     externally-defined coordinate system model) and in the number of
*     Objects that can be stored together in any particular set of
*     FITS header cards (e.g. multiple Objects, or only a single
*     Object). The choice of encoding also affects the range of
*     external applications which can potentially read and interpret
*     the FITS header cards produced.
*
*     The encoding options available are not necessarily mutually
*     exclusive, and it may sometimes be possible to store multiple
*     Objects (or the same Object several times) using different
*     encodings within the same set of FITS header cards. This
*     possibility increases the likelihood of other applications being
*     able to read and interpret the information.
*
*     By default, a FitsChan will attempt to determine which encoding
*     system is already in use, and will set the default Encoding
*     value accordingly (so that subsequent I/O operations adopt the
*     same conventions). It does this by looking for certain critical
*     FITS keywords which only occur in particular encodings. For
*     details of how this works, see the "Choice of Default Encoding"
*     section below. If you wish to ensure that a particular encoding
*     system is used, independently of any FITS cards already present,
*     you should set an explicit Encoding value yourself.

*  Encodings Available:
*     The Encoding attribute can take any of the following (case
*     insensitive) string values to select the corresponding encoding
*     system:
*
*     - "DSS": Encodes coordinate system information in FITS header
*     cards using the convention developed at the Space Telescope
*     Science Institute (STScI) for the Digitised Sky Survey (DSS)
*     astrometric plate calibrations. The main advantages of this
*     encoding are that FITS images which use it are widely available
*     and it is understood by a number of important and
*     well-established astronomy applications. For further details,
*     see the section "The DSS Encoding" below.
*
*     - "FITS-WCS": Encodes coordinate system information in FITS
*     header cards using the conventions described in the (draft) FITS
*     world coordinate system (FITS-WCS) paper by E.W. Greisen and
*     M. Calabretta (A & A, in preparation). The main advantages of
*     this encoding are that it should be understood by any FITS-WCS
*     compliant application and is likely to be adopted widely for
*     FITS data in future. At present, however, it suffers from the
*     disadvantage that the FITS-WCS standard is only a draft (and is
*     not stable), so it cannot yet be recommended for regular
*     use. For further details, see the section "The FITS-WCS
*     Encoding" below.
*
*     - "FITS-IRAF": Encodes coordinate system information in FITS
*     header cards using the conventions described in the document
*     "World Coordinate Systems Representations Within the FITS
*     Format" by R.J. Hanisch and D.G. Wells, 1988.  This encoding is
*     currently employed by the IRAF data analysis facility, so its
*     use will facilitate data exchange with IRAF. Its main advantages
*     are that it is a stable convention which approximates to a
*     subset of the propsed FITS-WCS encoding (above). This makes it
*     suitable as an interim method for storing coordinate system
*     information in FITS headers until the FITS-WCS encoding becomes
*     stable. Since many datasets currently use the FITS-IRAF
*     encoding, conversion of data from FITS-IRAF to the final form of
*     FITS-WCS is likely to be well supported.
*
*     - "NATIVE": Encodes AST Objects in FITS header cards using a
*     convention which is private to the AST library (but adheres to
*     the general FITS standard) and which uses FITS keywords that
*     will not clash with other encoding systems. The main advantages
*     of this are that any class of AST Object may be encoded, and any
*     (reasonable) number of Objects may be stored sequentially in the
*     same FITS header. This makes FITS headers an almost loss-less
*     communication path for passing AST Objects between applications
*     (although all such applications must, of course, make use of the
*     AST library to interpret the information). For further details,
*     see the section "The NATIVE Encoding" below.

*  Choice of Default Encoding:
*     If the Encoding attribute of a FitsChan is not set, the default
*     value it takes is determined by the presence of certain critical
*     FITS keywords within the FitsChan. The sequence of decisions
*     used to arrive at the default value is as follows:
*
*     - If the FitsChan contains any keywords beginning with the
*     string "BEGAST", then NATIVE encoding is used,
*     - Otherwise, if the FitsChan contains a keyword of the form
*     "CDi_j" or "CDiiijjj", where "i" and "j" are single digits, then
*     FITS-IRAF encoding is used,
*     - Otherwise, if the FitsChan contains the "CRVAL1" keyword, then
*     FITS-WCS encoding is used,
*     - Otherwise, if the FitsChan contains the "PLTRAH" keyword, then
*     DSS encoding is used,
*     - Otherwise, if none of these conditions is met (as would be the
*     case when using an empty FitsChan), then NATIVE encoding is
*     used.
*
*     Setting an explicit value for the Encoding attribute always
*     over-rides this default behaviour.
*
*     Note that when writing information to a FitsChan, the choice of
*     encoding will depend greatly on the type of application you
*     expect to be reading the information in future. If you do not
*     know this, there may sometimes be an advantage in writing the
*     information several times, using a different encoding on each
*     occasion.

*  The DSS Encoding:
*     The DSS encoding uses FITS header cards to store a multi-term
*     polynomial which relates pixel positions on a digitised
*     photographic plate to celestial coordinates (right ascension and
*     declination). This encoding may only be used to store a single
*     AST Object in any set of FITS header cards, and that Object must
*     be a FrameSet which conforms to the STScI/DSS coordinate system
*     model (this means that its base and current Frames must be
*     related by a DssMap).
*
c     When reading a DSS encoded Object (using astRead), the FitsChan
f     When reading a DSS encoded Object (using AST_READ), the FitsChan
*     concerned must initially be positioned at the first card (its
*     Card attribute must equal 1) and the result of the read, if
*     successful, will always be a pointer to a FrameSet. The base
*     Frame of this FrameSet represents DSS pixel coordinates, and the
*     current Frame represents DSS celestial coordinates. Such a read
*     is always destructive and causes the FITS header cards required
*     for the construction of the FrameSet to be removed from the
*     FitsChan, which is then left positioned at the "end-of-file". A
*     subsequent read using the same encoding will therefore not
*     return another FrameSet, even if the FitsChan is rewound.
*
c     When astWrite is used to store a FrameSet using DSS encoding,
f     When AST_WRITE is used to store a FrameSet using DSS encoding,
*     an attempt is first made to simplify the FrameSet to see if it
*     conforms to the DSS model.  Typically, for this simplification
*     process to succeed, the FrameSet must initially have been read
c     from a FitsChan (using astRead) where it was already stored
f     from a FitsChan (using AST_READ) where it was already stored
*     using the DSS encoding scheme.  This is because any intervening
*     modification to the FrameSet (such as re-mapping its Frames -
c     see astRemapFrame) is likely to cause it to depart from the very
f     see AST_REMAPFRAME) is likely to cause it to depart from the very
*     simple DSS polynomial model.
*
*     Some very limited modification is possible,
*     however. Specifically, the DssMap (which relates the pixel and
*     celestial coordinate Frames) may be prefixed with a linear
*     Mapping which produces a shift of pixel origin and/or a change
*     in pixel scale. However, this is only permitted if the DSS pixel
*     origin values remain as integers. Changes to the Frames within
*     the FrameSet are also permitted, but only the Epoch and Equinox
*     values will be used when any new DSS encoding of the FrameSet is
*     made.
*
*     If the simplification process succeeds, a description of the
*     FrameSet is written to the FitsChan using appropriate DSS FITS
*     header cards. The base Frame of the FrameSet is used to form the
*     DSS pixel coordinate system and the current Frame gives the DSS
*     celestial coordinate system.  A successful write operation will
*     over-write any existing DSS encoded data in the FitsChan, but
*     will not affect other (non-DSS) header cards. If a destructive
*     read of a DSS encoded Object has previously occurred, then an
*     attempt will be made to store the FITS header cards back in
*     their original locations.
*
*     If an attempt to simplify a FrameSet to conform to the DSS model
*     fails (or if the Object supplied is not a FrameSet), then no
c     data will be written to the FitsChan and astWrite will return
f     data will be written to the FitsChan and AST_WRITE will return
*     zero. No error will result.

*  The FITS-WCS Encoding:
*     The FITS-WCS convention uses FITS header cards to describe the
*     relationship between pixels in an image (not necessarily
*     2-dimensional) and a related "world coordinate system" (often,
*     although not necessarily, a celestial coordinate system) in
*     terms of a sequence of transformations which convert between
*     various intermediate coordinate systems. One of the
*     transformations involved is a "sky projection" (e.g. as
*     implemented by a WcsMap).  The FITS-WCS encoding may only be
*     used to store a single AST Object in any set of FITS header
*     cards, and that Object must be a FrameSet which conforms to the
*     FITS-WCS model.
*
c     When reading a FITS-WCS encoded Object (using astRead), the FitsChan
f     When reading a FITS-WCS encoded Object (using AST_READ), the FitsChan
*     concerned must initially be positioned at the first card (its
*     Card attribute must equal 1) and the result of the read, if
*     successful, will always be a pointer to a FrameSet. The base
*     Frame of this FrameSet represents FITS-WCS pixel coordinates,
*     and the current Frame represents the physical coordinate system
*     described by the FITS-WCS primary axis descriptions. If
*     secondary axis descriptions are also present, then the FrameSet
*     may contain additional (non-current) Frames which represent
*     these.  Such a read is always destructive and causes the FITS
*     header cards required for the construction of the FrameSet to be
*     removed from the FitsChan, which is then left positioned at the
*     "end-of-file". A subsequent read using the same encoding will
*     therefore not return another FrameSet, even if the FitsChan is
*     rewound.
*
c     When astWrite is used to store a FrameSet using FITS-WCS
f     When AST_WRITE is used to store a FrameSet using FITS-WCS
*     encoding, an attempt is first made to simplify the FrameSet to
*     see if it conforms to the FITS-WCS model. If this simplification
*     process succeeds (as it often should, as the model is reasonably
*     flexible), a description of the FrameSet is written to the
*     FitsChan using appropriate FITS header cards. The base Frame of
*     the FrameSet is used to form the FITS-WCS pixel coordinate
*     system and the current Frame gives the physical coordinate
*     system to be described by the FITS-WCS primary axis
*     descriptions.  Any additional Frames in the FrameSet may be used
*     to construct secondary axis descriptions, where appropriate.
*
*     A successful write operation will over-write any existing
*     FITS-WCS encoded data in the FitsChan, but will not affect other
*     (non-FITS-WCS) header cards. If a destructive read of a FITS-WCS
*     encoded Object has previously occurred, then an attempt will be
*     made to store the FITS header cards back in their original
*     locations. Otherwise, the new cards will be inserted following
*     any other FITS-WCS related header cards present or, failing
*     that, in front of the current card (as given by the Card
*     attribute).
*
*     If an attempt to simplify a FrameSet to conform to the FITS-WCS
*     model fails (or if the Object supplied is not a FrameSet), then
c     no data will be written to the FitsChan and astWrite will
f     no data will be written to the FitsChan and AST_WRITE will
*     return zero. No error will result.

*  The FITS-IRAF Encoding:
*     The FITS-IRAF encoding can, for most purposes, be considered as
*     a subset of the FITS-WCS encoding (above), although it differs
*     in the details of the FITS keywords used. It is used in exactly
*     the same way and has the same restrictions, but with the
*     addition of the following:
*
*     - The only celestial coordinate systems that may be represented
*     are equatorial, galactic and ecliptic,
*     - The range of sky projections which can be represented is
*     considerably smaller than FITS-WCS,
*     - Secondary axis descriptions are not supported, so when writing
*     a FrameSet to a FitsChan, only information from the base and
*     current Frames will be stored.
*
*     When writing a FrameSet using the FITS-IRAF encoding, axis
*     rotations are specified by a matrix of FITS keywords of the form
*     "CDi_j", where "i" and "j" are single digits. The alternative
*     form "CDiiijjj", which is also in use, is recognised when
*     reading an Object, but is never written.
*
*     You should not normally attempt to mix the FITS-IRAF and
*     FITS-WCS encodings within the same FitsChan, since there is a
*     risk that keyword clashes may occur.

*  The NATIVE Encoding:
*     The NATIVE encoding may be used to store a description of any
*     class of AST Object in the form of FITS header cards, and (for
*     most practical purposes) any number of these Object descriptions
*     may be stored within a single set of FITS cards. If multiple
*     Object descriptions are stored, they are written and read
*     sequentially. The NATIVE encoding makes use of unique FITS
*     keywords which are designed not to clash with keywords that have
*     already been used for other purposes (if a potential clash is
*     detected, an alternative keyword is constructed to avoid the
*     clash).
*
*     When reading a NATIVE encoded object from a FitsChan (using
c     astRead), FITS header cards are read, starting at the current
f     AST_READ), FITS header cards are read, starting at the current
*     card (as determined by the Card attribute), until the start of
*     the next Object description is found. This description is then
*     read and converted into an AST Object, for which a pointer is
*     returned. Such a read is always destructive and causes all the
*     FITS header cards involved in the Object description to be
*     removed from the FitsChan, which is left positioned at the
*     following card.
*
*     The Object returned may be of any class, depending on the
*     description that was read, and other AST routines may be used to
*     validate it (for example, by examining its Class or ID attribute
c     using astGetC). If further NATIVE encoded Object descriptions
f     using AST_GETC). If further NATIVE encoded Object descriptions
c     exist in the FitsChan, subsequent calls to astRead will return
f     exist in the FitsChan, subsequent calls to AST_READ will return
*     the Objects they describe in sequence (and destroy their
*     descriptions) until no more remain between the current card and
*     the "end-of-file".
*
c     When astWrite is used to write an Object using NATIVE encoding,
f     When AST_WRITE is used to write an Object using NATIVE encoding,
*     a description of the Object is inserted immediately before the
*     current card (as determined by the Card attribute).  Multiple
*     Object descriptions may be written in this way and are stored
*     separately (and sequentially if the Card attribute is not
*     modified between the writes). A write operation using the NATIVE
*     encoding does not over-write previously written Object
*     descriptions. Note, however, that subsequent behaviour is
*     undefined if an Object description is written inside a
*     previously-written description, so this should be avoided.
*
*     When an Object is written to a FitsChan using NATIVE encoding,
c     astWrite should (barring errors) always transfer data and
f     AST_WRITE should (barring errors) always transfer data and
*     return a value of 1.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/
astMAKE_CLEAR(FitsChan,Encoding,encoding,UNKNOWN_ENCODING)
astMAKE_SET(FitsChan,Encoding,int,encoding,( 
   value == NATIVE_ENCODING || 
   value == FITSWCS_ENCODING ||
   value == FITSIRAF_ENCODING ||
   value == DSS_ENCODING ? value : 
   (astError( AST__BADAT, "astSetEncoding: Unknown encoding system %d "
              "supplied.", value ), UNKNOWN_ENCODING )))
astMAKE_TEST(FitsChan,Encoding,( this->encoding != UNKNOWN_ENCODING ))

/* FitsDigits. */
/* =========== */
/*
*att++
*  Name:
*     FitsDigits

*  Purpose:
*     Digits of precision for floating point FITS values.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute gives the number of significant decimal digits to
*     use when formatting floating point values for inclusion in the
*     FITS header cards within a FitsChan.
*
*     By default, a positive value is used which results in no loss of
c     information, assuming that the value's precision is double.
f     information, assuming that the value is double precision.
*     Usually, this causes no problems.
*
*     However, to adhere strictly to the recommendations of the FITS
*     standard, the width of the formatted value (including sign,
*     decimal point and exponent) ought not to be more than 20
*     characters. If you are concerned about this, you should set
*     FitsDigits to a negative value, such as -15. In this case, the
*     absolute value (+15) indicates the maximum number of significant
*     digits to use, but the actual number used may be fewer than this
*     to ensure that the FITS recommendations are satisfied. When
*     using this approach, the resulting number of significant digits
*     may depend on the value being formatted and on the presence of
*     any sign, decimal point or exponent.
*
*     The value of this attribute is effective when FITS header cards
*     are output, either using
c     astFindFits or by the action of the FitsChan's sink function
f     AST_FINDFITS or by the action of the FitsChan's sink routine
*     when it is finally deleted.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/
astMAKE_CLEAR(FitsChan,FitsDigits,fitsdigits,DBL_DIG)
astMAKE_GET(FitsChan,FitsDigits,int,DBL_DIG,this->fitsdigits)
astMAKE_SET(FitsChan,FitsDigits,int,fitsdigits,value)
astMAKE_TEST(FitsChan,FitsDigits,( this->fitsdigits != DBL_DIG ))

/* CDMatrix. */
/* ========= */
/*
*att+
*  Name:
*     CDMatrix

*  Purpose:
*     Use a FITS CD matrix to describe axis rotation?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute determines the FITS keywords which will be used to 
*     describe axis rotation when writing out Objects using the FITS-WCS 
*     encoding (both systems are recognised when reading Objects).
*
*     If a zero value is supplied for CDMatrix, then axis
*     rotation will be described using a PC matrix as described in the
*     (draft) FITS world coordinate system (FITS-WCS) paper by E.W.Greisen 
*     and M.Calabretta (A & A, in preparation). Any non-zero value will
*     result in a CD matrix being used instead. A CD matrix is similar to 
*     a PC matrix except that the pixel size on every axis is incorporated 
*     into the matrix rather than being stored independantly in a set of 
*     CDELT keywords. 
*
*     Two diffent conventions for storing a CD matrix are in common use,
*     and the convention used is determined by the particular non-zero
*     value supplied for CDMatrix. If a value of 1 is supplied, then
*     the axis indices are encoded using three digits, resulting in
*     keywords with names like CD001002. If any other non-zero value is 
*     supplied (and there are less than 10 axes), then axis indices are
*     encoded in a single digit using keyword names like CD1_2.
*
*     When using CD matrices, no CDELT keywords will be created, and no
*     secondary axis descriptions will be created.
*
*     By default, a FitsChan will attempt to determine which matrix
*     representation is already in use, and will set the default CDMatrix
*     value accordingly (so that subsequent I/O operations adopt the
*     same conventions). A value of 2 ("CDi_j") is adopted if this cannot be 
*     done, for instance if the FitsChan is empty. If you wish to ensure that a 
*     particular matrix representation is used, independently of any FITS 
*     cards already present, you should set an explicit CDMatrix value 
*     yourself.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att-
*/
/* Has a value of -999 when not set, yielding a default value of 0. */
astMAKE_CLEAR(FitsChan,CDMatrix,cdmatrix,-999)
astMAKE_SET(FitsChan,CDMatrix,int,cdmatrix,MIN(2,abs(value)))
astMAKE_TEST(FitsChan,CDMatrix,( this->cdmatrix != -999 ))

/* Ncard */
/* ===== */
/*
*att++
*  Name:
*     Ncard

*  Purpose:
*     Number of FITS header cards in a FitsChan.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the total number of FITS header cards
*     stored in a FitsChan. It is updated as cards are added or
*     deleted.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for FitsChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for FitsChan objects.

*  Parameters:
*     objin
*        Pointer to the FitsChan to be copied.
*     objout
*        Pointer to the FitsChan being constructed.

*  Notes:
*     - The source and sink functions are not propagated (i.e. the
*     pointers are set NULL in the output FitsChan).
*     - This constructor makes a deep copy, including a copy of the
*     keyword values.
*/


/* Local Variables: */
   const char *class;        /* Pointer to object class */
   AstFitsChan *in;          /* Pointer to input FitsChan */
   AstFitsChan *out;         /* Pointer to output FitsChan */
   FitsKeySeq *keyseq_in;
   FitsKeySeq *keyseq_out;
   int icard;
   int old_skipping;         /* Original value of external variable Skipping */
   
/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output FitsChans. */
   in = (AstFitsChan *) objin;
   out = (AstFitsChan *) objout;

/* Nullify all pointers in the output FitsChan so that the input
   data will not be deleted in the event of an error occurring. */
   out->card = NULL;
   out->head = NULL;
   out->keyseq = NULL;
   out->source = NULL;
   out->source_wrap = NULL;
   out->sink = NULL;
   out->sink_wrap = NULL;

/* Store the object class. */
   class = astGetClass( in );

/* Ensure all cards are copied, including those already read by astRead. */
   old_skipping = Skipping;
   Skipping = 0;

/* Save the current card index in the input FitsChan. */
   icard = astGetCard( in );

/* Rewind the input FitsChan. */
   astClearCard( in );

/* Copy all the FitsCard structures from input to output. */
   while( !astFitsEof( in ) && astOK ){

/* Store a new card in the output, holding the same information as the
   input card. */
      NewCard( out, CardName( in ), CardType( in ), CardData( in, NULL ), 
               CardComm( in ), CardDel( in ) );

/* Move on to the next input card. */
      MoveCard( in, 1, "astCopy", class );

   }

/* Set the current card in both input and output to the current input
   card on entry. */
   astSetCard( in, icard );
   astSetCard( out, icard );

/* Copy the list of keyword sequence numbers used, considering each
   input list element in turn. */
   keyseq_in = (FitsKeySeq *) in->keyseq;
   while ( astOK && keyseq_in ) {

/* Allocate space for the new list element and store a copy of the
   corresponding input element's keyword string. */
      keyseq_out = (FitsKeySeq *) astMalloc( sizeof( FitsKeySeq ) );
      if ( astOK ) {
         keyseq_out->key = astStore( NULL, keyseq_in->key,
                                     strlen( keyseq_in->key ) + (size_t) 1 );

/* If an error occurs, free the list element. */
         if ( !astOK ) {
            keyseq_out = astFree( keyseq_out );

/* Otherwise, copy the input sequence number and link the new element
   into the output list */
         } else {
            keyseq_out->seq = keyseq_in->seq;
            keyseq_out->next = out->keyseq;
            out->keyseq = (void *) keyseq_out;
            
/* Consider the next input element. */
            keyseq_in = keyseq_in->next;
         }
      }
   }

/* Reinstate the original setting of the external Skipping variable. */
   Skipping = old_skipping;

/* If an error occurred, delete the contents of the output Object. */
   if( !astOK ) Delete( objout );
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for FitsChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for FitsChan objects.

*  Parameters:
*     obj
*        Pointer to the FitsChan to be deleted.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to FitsChan */
  
/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) obj;

/* Write out the contents of the FitsChan using the sink function
   provided when it was created. */
   WriteToSink( this );

/* Remove all cards from the FitsChan. */
   Empty( this );

   return;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for FitsChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the FitsChan class to an output Channel.

*  Parameters:
*     this
*        Pointer to the FitsChan whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   const char *class;            /* Object class */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   int ncard;                    /* No. of cards dumped so far */
   int icard;                    /* Index of current card */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */
   int type;                     /* Keyword data type */
   void *data;                   /* Pointer to keyword data value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_object;

/* Store the object class. */
   class = astGetClass( this );

/* Save the index of ht ecurrent card. */
   icard = astGetCard( this );

/* Write out values representing the instance variables for the
   FitsChan class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* Card. */
/* ----- */
   astWriteInt( channel, "Card", 1, 1, icard, "Index of current card" );

/* Encoding. */
/* --------- */
   set = TestEncoding( this );
   ival = set ? GetEncoding( this ) : astGetEncoding( this );
   if( ival > UNKNOWN_ENCODING && ival <= MAX_ENCODING ) {
      astWriteString( channel, "Encod", set, 1, xencod[ival], "Encoding system" );
   } else {
      astWriteString( channel, "Encod", set, 1, UNKNOWN_STRING, "Encoding system" );
   }

/* FitsDigits. */
/* ----------- */
   set = TestFitsDigits( this );
   ival = set ? GetFitsDigits( this ) : astGetFitsDigits( this );
   astWriteInt( channel, "FitsDg", set, 1, ival, "No. of digits for floating point values" );

#if 0
/* CDMatrix. */
/* --------- */
   set = TestCDMatrix( this );
   ival = set ? GetCDMatrix( this ) : astGetCDMatrix( this );
   if( ival > -1 && ival < 3 ) {
      astWriteString( channel, "CDMat", set, 1, xcdmat[ival], "Rotation matrix representation" );
   }
#endif

/* Now do instance variables which are not attributes. */
/* =================================================== */

/* Rewind the FitsChan. */
   astClearCard( this );

/* Dump each card. */
   ncard = 1;
   while( !astFitsEof( this ) && astOK ){

/* Write out the keyword name. */
      if( CardName( this ) ){
         (void) sprintf( buff, "Nm%d", ncard );
         astWriteString( channel, buff, 1, 1, CardName( this ),
                         "FITS keyword name" );
      }

/* Write out the keyword type. */
      type = CardType( this );
      (void) sprintf( buff, "Ty%d", ncard );
      astWriteString( channel, buff, 1, 1, type_names[type],
                      "FITS keyword data type" );

/* Write out the "used" flag if the card has been used. */
      if( CardDel( this ) ){
         (void) sprintf( buff, "Dl%d", ncard );
         astWriteInt( channel, buff, 1, 1, 1, "Destructive read performed" );
      }

/* Write out the data value, using the appropriate data type. */
      data = CardData( this, NULL );
      if( data ){

         if( type == AST__FLOAT ){
            (void) sprintf( buff, "Dt%d", ncard );
            astWriteDouble( channel, buff, 1, 1, *( (double *) data ), 
                            "FITS keyword value" );
   
         } else if( type == AST__STRING ){
            (void) sprintf( buff, "Dt%d", ncard );
            astWriteString( channel, buff, 1, 1, (char *) data, 
                            "FITS keyword value" );
   
         } else if( type == AST__INT ){
            (void) sprintf( buff, "Dt%d", ncard );
            astWriteInt( channel, buff, 1, 1, *( (int *) data ), 
                         "FITS keyword value" );
   
         } else if( type == AST__LOGICAL ){
            (void) sprintf( buff, "Dt%d", ncard );
            astWriteInt( channel, buff, 1, 1, *( (int *) data ), 
                         "FITS keyword value" );
   
         } else if( type == AST__COMPLEXF ){
            (void) sprintf( buff, "Dr%d", ncard );
            astWriteDouble( channel, buff, 1, 1, *( (double *) data ), 
                            "FITS keyword real value" );
   
            (void) sprintf( buff, "Di%d", ncard );
            astWriteDouble( channel, buff, 1, 1, *( ( (double *) data ) + 1 ), 
                            "FITS keyword imaginary value" );
   
         } else if( type == AST__COMPLEXI ){
            (void) sprintf( buff, "Dr%d", ncard );
            astWriteInt( channel, buff, 1, 1, *( (int *) data ), 
                         "FITS keyword real value" );
   
            (void) sprintf( buff, "Di%d", ncard );
            astWriteInt( channel, buff, 1, 1, *( ( (int *) data ) + 1 ), 
                         "FITS keyword imaginary value" );
         }

      }

/* Write out the keyword comment. */
      if( CardComm( this ) ){
         (void) sprintf( buff, "Cm%d", ncard );
         astWriteString( channel, buff, 1, 1, CardComm( this ),
                         "FITS keyword comment" );
      }

/* Move on to the next card. */
      ncard++;
      MoveCard( this, 1, "astDump", class );

   }

/* Reinstate the original current card. */
   astSetCard( this, icard );

#undef KEY_LEN 

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAFitsChan and astCheckFitsChan functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(FitsChan,Channel,check,&class_init)
astMAKE_CHECK(FitsChan)

AstFitsChan *astFitsChan_( const char *(* source)( void ),
                           void (* sink)( const char * ),
                           const char *options, ... ) {
/*
*++
*  Name:
c     astFitsChan
f     AST_FITSCHAN

*  Purpose:
*     Create a FitsChan.

*  Type:
*     Public function.

*  Synopsis:
c     #include "fitschan.h"
c     AstFitsChan *astFitsChan( const char *(* source)( void ),
c                               void (* sink)( const char * ),
c                               const char *options, ... )
f     RESULT = AST_FITSCHAN( SOURCE, SINK, OPTIONS, STATUS )

*  Class Membership:
*     FitsChan constructor.

*  Description:
*     This function creates a new FitsChan and optionally initialises
*     its attributes.
*
*     A FitsChan is a specialised form of Channel which supports I/O
*     operations involving the use of FITS (Flexible Image Transport
*     System) header cards. Writing an Object to a FitsChan (using
c     astWrite) will, if the Object is suitable, generate a
f     AST_WRITE) will, if the Object is suitable, generate a
*     description of that Object composed of FITS header cards, and
*     reading from a FitsChan will create a new Object from its FITS
*     header card description.
*
*     While a FitsChan is active, it represents a buffer which may
*     contain zero or more 80-character "header cards" conforming to
*     FITS conventions. Any sequence of FITS-conforming header cards
*     may be stored, apart from the "END" card whose existence is
*     merely implied.  The cards may be accessed in any order by using
*     the FitsChan's integer Card attribute, which identifies a "current"
*     card, to which subsequent operations apply. Searches
c     based on keyword may be performed (using astFindFits), new
c     cards may be inserted (astPutFits) and existing ones may be
c     deleted (astDelFits).
f     based on keyword may be performed (using AST_FINDFITS), new
f     cards may be inserted (AST_PUTFITS) and existing ones may be
f     deleted (AST_DELFITS).
*
*     When you create a FitsChan, you have the option of specifying
*     "source" and "sink" functions which connect it to external data
*     stores by reading and writing FITS header cards. If you provide
*     a source function, it is used to fill the FitsChan with header
*     cards when it is created. If you do not provide a source
*     function, the FitsChan remains empty until you explicitly enter
c     data into it (e.g. using astPutFits or astWrite). If you
f     data into it (e.g. using AST_PUTFITS or AST_WRITE). If you
*     provide a sink function, it is used to deliver any remaining
*     contents of a FitsChan to an external data store when the
*     FitsChan is deleted. If you do not provide a sink function, any
*     header cards remaining when the FitsChan is deleted will be
*     lost, so you should arrange to extract them first if necessary
c     (e.g. using astFindFits or astRead).
f     (e.g. using AST_FINDFITS or AST_READ).
*     
*     Coordinate system information may be described using FITS header
*     cards using several different conventions, termed
*     "encodings". When an AST Object is written to (or read from) a
*     FitsChan, the value of the FitsChan's Encoding attribute
*     determines how the Object is converted to (or from) a
*     description involving FITS header cards. In general, different
*     encodings will result in different sets of header cards to
*     describe the same Object. Examples of encodings include the DSS
*     encoding (based on conventions used by the STScI Digitised Sky
*     Survey data), the FITS-WCS encoding (based on a proposed FITS
*     standard) and the NATIVE encoding (a near loss-less way of
*     storing AST Objects in FITS headers).
*
*     The available encodings differ in the range of Objects they can
*     represent, in the number of Object descriptions that can coexist
*     in the same FitsChan, and in their accessibility to other
*     (external) astronomy applications (see the Encoding attribute
*     for details). Encodings are not necessarily mutually exclusive
*     and it may sometimes be possible to describe the same Object in
*     several ways within a particular set of FITS header cards by
*     using several different encodings.
*
c     The detailed behaviour of astRead and astWrite, when used with
f     The detailed behaviour of AST_READ and AST_WRITE, when used with
*     a FitsChan, depends on the encoding in use. In general, however,
c     all use of astRead is destructive, so that FITS header cards
f     all use of AST_READ is destructive, so that FITS header cards
*     are consumed in the process of reading an Object, and are
*     removed from the FitsChan.
*
*     If the encoding in use allows only a single Object description
*     to be stored in a FitsChan (e.g. the DSS, FITS-WCS and FITS-IRAF
c     encodings), then write operations using astWrite will
f     encodings), then write operations using AST_WRITE will
*     over-write any existing Object description using that
*     encoding. Otherwise (e.g. the NATIVE encoding), multiple Object
*     descriptions are written sequentially and may later be read
*     back in the same sequence.

*  Parameters:
c     source
f     SOURCE = FUNCTION (Given)
c        Pointer to a source function which takes no arguments and
c        returns a pointer to a null-terminated string. This function
c        will be used by the FitsChan to obtain input FITS header
c        cards. On each invocation, it should read the next input card
c        from some external source (such as a FITS file), and return a
c        pointer to the (null-terminated) contents of the card. It
c        should return a NULL pointer when there are no more cards to
c        be read.
c
c        If "source" is NULL, the FitsChan will remain empty until
c        cards are explicitly stored in it (e.g. using astPutFits).
f        A source routine, which is a function taking two arguments: a
f        character argument of length 80 to contain a FITS card, and an
f        integer error status argument. It should return an integer value.
f        This function will be used by the FitsChan to obtain input
f        FITS header cards. On each invocation, it should read the
f        next input card from some external source (such as a FITS
f        file), and return the contents of the card via its character
f        argument. It should return a function result of one unless
f        there are no more cards to be read, in which case it should
f        return zero. If an error occurs, it should set its error
f        status argument to an error value before returning.
f
f        If the null routine AST_NULL is supplied as the SOURCE value,
f        the FitsChan will remain empty until cards are explicitly
f        stored in it (e.g. using AST_PUTFITS).
c     sink
f     SINK = SUBROUTINE (Given)
c        Pointer to a sink function that takes a pointer to a
c        null-terminated string as an argument and returns void.  This
c        function will be used by the FitsChan to deliver any FITS
c        header cards it contains when it is finally deleted. On 
c        each invocation, it should deliver the contents of the character
c        string passed to it as a FITS header card to some external
c        data store (such as a FITS file).
f        A sink routine, which is a subroutine which takes two
f        arguments: a character argument of length 80 to contain a
f        FITS card, and an integer error status argument. This routine
f        will be used by the FitsChan to deliver any FITS header cards
f        it contains when it is finally deleted. On each invocation,
f        it should deliver the contents of the character string passed
f        to it as a FITS header card to some external data store (such
f        as a FITS file).  If an error occurs, it should set its error
f        status argument to an error value before returning.
*
c        If "sink" is NULL, 
f        If the null routine AST_NULL is supplied as the SINK value,
*        the contents of the FitsChan will not be written out when it
*        is deleted.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new FitsChan. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new FitsChan. The syntax used is identical to that for the
f        AST_SET routine.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFitsChan()
f     AST_FITSCHAN = INTEGER
*        A pointer to the new FitsChan.

*  Notes:
f     - The names of the routines supplied for the SOURCE and SINK
f     arguments should appear in EXTERNAL statements in the Fortran
f     routine which invokes AST_FITSCHAN. However, this is not generally
f     necessary for the null routine AST_NULL (so long as the AST_PAR
f     include file has been used).
c     - No FITS "END" card will be written via the sink function. You
f     - No FITS "END" card will be written via the sink routine. You
*     should add this card yourself after the FitsChan has been
*     deleted.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
f     - Note that the null routine AST_NULL (one underscore) is
f     different to AST__NULL (two underscores), which is the null Object
f     pointer.
*--
*/

/* Local Variables: */
   AstFitsChan *new;             /* Pointer to new FitsChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the FitsChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by other C functions within AST, and uses the standard "wrapper"
   functions included in this class. */
   new = astInitFitsChan( NULL, sizeof( AstFitsChan ), !class_init, 
                          &class_vtab, "FitsChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   FitsChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new FitsChan. */
   return new;
}

AstFitsChan *astFitsChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... ) {
/*
*  Name:
*     astFitsChanId_

*  Purpose:
*     Create a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFitsChan *astFitsChanId_( const char *(* source)( void ),
*                                  void (* sink)( const char * ),
*                                  const char *options, ... )

*  Class Membership:
*     FitsChan constructor.

*  Description:
*     This function implements the external (public) C interface to the
*     astFitsChan constructor function. Another function (astFitsChanForId)
*     should be called to create a FitsChan for use within other languages.
*     Both functions return an ID value (instead of a true C pointer) to 
*     external users, and must be provided because astFitsChan_ has a variable 
*     argument list which cannot be encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astFitsChan_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astFitsChan_.

*  Returned Value:
*     The ID value associated with the new FitsChan.
*/

/* Local Variables: */
   AstFitsChan *new;             /* Pointer to new FitsChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the FitsChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by external C functions and uses the standard "wrapper"
   functions included in this class. */
   new = astInitFitsChan( NULL, sizeof( AstFitsChan ), !class_init, 
                          &class_vtab, "FitsChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   FitsChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new FitsChan. */
   return astMakeId( new );
}

AstFitsChan *astFitsChanForId_( const char *(* source)( void ),
                              char *(* source_wrap)( const char *(*)( void ) ),
                              void (* sink)( const char * ),
                              void (* sink_wrap)( void (*)( const char * ),
                                                  const char * ),
                              const char *options, ... ) {
/*
*+
*  Name:
*     astFitsChanFor

*  Purpose:
*     Initialise a FitsChan from a foreign language interface.

*  Type:
*     Public function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFitsChan *astFitsChanFor( const char *(* source)( void ),
*                                char *(* source_wrap)( const char *(*)
*                                                       ( void ) ),
*                                void (* sink)( const char * ),
*                                void (* sink_wrap)( void (*)( const char * ),
*                                                    const char * ),
*                                const char *options, ... )

*  Class Membership:
*     FitsChan constructor.

*  Description:
*     This function creates a new FitsChan from a foreign language
*     interface and optionally initialises its attributes.
*
*     A FitsChan implements FITS input/output for the AST library.
*     Writing an Object to a FitsChan (using astWrite) will generate a
*     textual representation of that Object in terms of FITS header cards, 
*     and reading from a FitsChan (using astRead) will create a new Object 
*     from its FITS representation.
*
*     Normally, when you use a FitsChan, you should provide "source"
*     and "sink" functions which connect it to an external data store
*     by reading and writing the resulting text. This function also
*     requires you to provide "wrapper" functions which will invoke
*     the source and sink functions. 

*  Parameters:
*     source
*        Pointer to a "source" function which will be used to obtain
*        FITS header cards. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the FitsChan will remain empty until
*        cards are added explicitly (e.g. using astPutFits).
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next FITS header card.
*        The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source" is NULL, the FitsChan will remain empty until
*        cards are added explicitly (e.g. using astPutFits).
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        FITS header cards. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the contents of the FitsChan will not be
*        written out before being deleted.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the contents of the FitsChan will not be
*        written out before being deleted.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new FitsChan. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astFitsChanFor()
*        A pointer to the new FitsChan.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*     - This function is only available through the public interface
*     to the FitsChan class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-

*  Implememtation Notes:
*     - This function behaves exactly like astFitsChanId_, in that it
*     returns ID values and not true C pointers, but it has two
*     additional arguments. These are pointers to the "wrapper
*     functions" which are needed to accommodate foreign language
*     interfaces.
*/

/* Local Variables: */
   AstFitsChan *new;             /* Pointer to new FitsChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the FitsChan, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitFitsChan( NULL, sizeof( AstFitsChan ), !class_init, 
                          &class_vtab, "FitsChan", source, source_wrap, 
                          sink, sink_wrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   FitsChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new FitsChan. */
   return astMakeId( new );
}

AstFitsChan *astInitFitsChan_( void *mem, size_t size, int init,
                               AstFitsChanVtab *vtab, const char *name,
                               const char *(* source)( void ),
                               char *(* source_wrap)( const char *(*)( void ) ),
                               void (* sink)( const char * ),
                               void (* sink_wrap)( void (*)( const char * ),
                                                   const char * ) ) {
/*
*+
*  Name:
*     astInitFitsChan

*  Purpose:
*     Initialise a FitsChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFitsChan *astInitFitsChan_( void *mem, size_t size, int init,
*                            AstFitsChanVtab *vtab, const char *name,
*                            const char *(* source)( void ),
*                            char *(* source_wrap)( const char *(*)( void ) ),
*                            void (* sink)( const char * ),
*                            void (* sink_wrap)( void (*)( const char * ),
*                                                   const char * ) )

*  Class Membership:
*     FitsChan initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new FitsChan object. It allocates memory (if
*     necessary) to accommodate the FitsChan plus any additional data
*     associated with the derived class.  It then initialises a
*     FitsChan structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for a FitsChan at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the FitsChan is to be
*        initialised.  This must be of sufficient size to accommodate
*        the FitsChan data (sizeof(FitsChan)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FitsChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FitsChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A boolean flag indicating if the FitsChan's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FitsChan.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     source
*        Pointer to a "source" function which will be used to obtain
*        FITS header cards. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the FitsChan will remain empty until
*        cards are added explicitly (e.g. using astPutFits).
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next FITS header card.
*        The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source" is NULL, the FitsChan will remain empty until
*        cards are added explicitly (e.g. using astPutFits).
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        FITS header cards. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the contents of the FitsChan will not be
*        written out before being deleted.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the contents of the FitsChan will not be
*        written out before being deleted.

*  Returned Value:
*     A pointer to the new FitsChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstFitsChan *new;              /* Pointer to new FitsChan */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise a Channel structure (the parent class) as the first
   component within the FitsChan structure, allocating memory if
   necessary. */
   new = (AstFitsChan *) astInitChannel( mem, size, init,
                                         (AstChannelVtab *) vtab, name,
                                         NULL, NULL );

/* If necessary, initialise the virtual function table. */
   if ( init ) InitVtab( vtab );
   if ( astOK ) {

/* Initialise the FitsChan data. */
/* ---------------------------- */
      new->head = NULL;
      new->card = NULL;
      new->keyseq = NULL;
      new->fitsdigits = DBL_DIG;
      new->encoding = UNKNOWN_ENCODING;
      new->cdmatrix = -999;

/* Save the pointers to the source and sink functions and the wrapper
   functions that invoke them. */
      new->source = source;
      new->source_wrap = source_wrap;
      new->sink = sink;
      new->sink_wrap = sink_wrap;

/* Fill the FitsChan with header cards read using the source function. */
      ReadFromSource( new );

/* Rewind the FitsChan so that the next read operation will return the
   first card. */
      new->card = new->head;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstFitsChan *astLoadFitsChan_( void *mem, size_t size, int init,
                               AstFitsChanVtab *vtab, const char *name,
                               AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadFitsChan

*  Purpose:
*     Load a FitsChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFitsChan *astLoadFitsChan( void *mem, size_t size, int init,
*                                   AstFitsChanVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     FitsChan loader.

*  Description:
*     This function is provided to load a new FitsChan using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     FitsChan structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a FitsChan at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the FitsChan is to be
*        loaded.  This must be of sufficient size to accommodate the
*        FitsChan data (sizeof(FitsChan)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FitsChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FitsChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstFitsChan) is used instead.
*     init
*        A boolean flag indicating if the FitsChan's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*
*        If the "vtab" parameter is NULL, the "init" value is ignored
*        and the (static) virtual function table initialisation flag
*        for the FitsChan class is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FitsChan. If this is NULL, a pointer
*        to the (static) virtual function table for the FitsChan class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "FitsChan" is used instead.

*  Returned Value:
*     A pointer to the new FitsChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

#define KEY_LEN 50              /* Maximum length of a keyword */

/* Local Variables: */
   AstFitsChan *new;            /* Pointer to the new FitsChan */
   char *comment;               /* Pointer to keyword comment */
   char *keynm;                 /* Keyword name */
   char *text;                  /* Textual version of integer value */
   char buff[ KEY_LEN + 1 ];    /* Buffer for keyword string */
   double dval[2];              /* Double precision data values */
   int ival[2];                 /* Integer data values */
   int ncard;                   /* No. of FitsCards read so far */
   int del;                     /* Keyword deletion flag */
   int type;                    /* Keyword type */
   void *data;                  /* Pointer to keyword data value */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this FitsChan. In this case the
   FitsChan belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstFitsChan );
      init = !class_init;
      vtab = &class_vtab;
      name = "FitsChan";
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built FitsChan. */
   new = astLoadChannel( mem, size, init, (AstChannelVtab *) vtab, name,
                         channel );

/* If required, initialise the part of the virtual function table used
   by this class. */
   if ( init ) InitVtab( vtab );

/* Note if we have successfully initialised the (static) virtual
   function table owned by this class (so that this is done only
   once). */
   if ( astOK ) {
      if ( ( vtab == &class_vtab ) && init ) class_init = 1;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "FitsChan" );

/* Initialise the list of keyword sequence numbers. */
      new->keyseq = NULL;

/* Set the pointers to the source and sink functions, and their
   wrapper functions, to NULL (we cannot restore these since they
   refer to process-specific addresses). */
      new->source = NULL;
      new->source_wrap = NULL;
      new->sink = NULL;
      new->sink_wrap = NULL;

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* Encoding. */
/* --------- */
      text = astReadString( channel, "encod", UNKNOWN_STRING );
      if( strcmp( text, UNKNOWN_STRING ) ) {
         new->encoding = FindString( MAX_ENCODING + 1, xencod, text, 
                                     "the FitsChan component 'Encod'", 
                                     "astRead", astGetClass( channel ) );
      } else {
         new->encoding = UNKNOWN_ENCODING;
      }
      if ( TestEncoding( new ) ) SetEncoding( new, new->encoding );

/* FitsDigits. */
/* ----------- */
      new->fitsdigits = astReadInt( channel, "fitsdg", DBL_DIG );
      if ( TestFitsDigits( new ) ) SetFitsDigits( new, new->fitsdigits );

#if 0
/* CDMatrix. */
/* --------- */
      new->cdmatrix = FindString( 3, xcdmat, 
                                  astReadString( channel, "cdmat", xcdmat[0] ),
                                  "the FitsChan component 'CDMat'", 
                                  "astRead", astGetClass( channel ) );
      if ( TestCDMatrix( new ) ) SetCDMatrix( new, new->cdmatrix );
#endif

/* Card. */
/* ----- */
/* Initialise the index of the card to be read next. */
      ncard = 1;
      new->card = NULL;
      new->head = NULL;

/* Load each card. */
      type = AST__NOTYPE + 1;
      while( type != AST__NOTYPE && astOK ){

/* Get the keyword type. */
         (void) sprintf( buff, "ty%d", ncard );
         text = astReadString( channel, buff, " " );

         if( strcmp( text, " " ) ) {
            type = FindString( 7, type_names, text, 
                               "a FitsChan keyword data type", 
                               "astRead", astGetClass( channel ) );
         } else {
            type = AST__NOTYPE;
         }

/* Only proceed if the keyword type was found. */
         if( type != AST__NOTYPE ){

/* Get the keyword name. Use a default blank name. */
            (void) sprintf( buff, "nm%d", ncard );
            keynm = astReadString( channel, buff, "        " );

/* Get the data value, using the appropriate data type, unless the
   keyword is a comment keyword. */
            if( type == AST__FLOAT ){
               (void) sprintf( buff, "dt%d", ncard );
               dval[ 0 ] = astReadDouble( channel, buff, AST__BAD );
               data = (void *) dval;   

            } else if( type == AST__STRING ){
               (void) sprintf( buff, "dt%d", ncard );
               data = (void *) astReadString( channel, buff, "" );
   
            } else if( type == AST__INT ){
               (void) sprintf( buff, "dt%d", ncard );
               ival[ 0 ] = astReadInt( channel, buff, 0 );
               data = (void *) ival;   
   
            } else if( type == AST__LOGICAL ){
               (void) sprintf( buff, "dt%d", ncard );
               ival[ 0 ] = astReadInt( channel, buff, 0 );
               data = (void *) ival;   
   
            } else if( type == AST__COMPLEXF ){
               (void) sprintf( buff, "dr%d", ncard );
               dval[ 0 ] = astReadDouble( channel, buff, AST__BAD );

               (void) sprintf( buff, "di%d", ncard );
               dval[ 1 ] = astReadDouble( channel, buff, AST__BAD );
               data = (void *) dval;   
   
            } else if( type == AST__COMPLEXI ){
               (void) sprintf( buff, "dr%d", ncard );
               ival[ 0 ] = astReadInt( channel, buff, 0 );

               (void) sprintf( buff, "di%d", ncard );
               ival[ 1 ] = astReadInt( channel, buff, 0 );
               data = (void *) ival;   

            } else {
               data = NULL;
            }

/* Get the keyword deletion flag. */
            (void) sprintf( buff, "dl%d", ncard );
            del = astReadInt( channel, buff, 0 );

/* Get the keyword comment. */
            (void) sprintf( buff, "cm%d", ncard );
            comment = astReadString( channel, buff, NULL );

/* Append a new card to the output FitsChan. */
            NewCard( new, keynm, type, data, comment, del );

/* Free the character strings. */
            comment = (char *) astFree( (void *) comment );
            keynm = (char *) astFree( (void *) keynm );

         }

/* Move on to the next card. */
         ncard++;
      }

/* Set up the current card index. */
      astSetCard( new, astReadInt( channel, "card", 0 ) );
   }

/* If an error occurred, clean up by deleting the new FitsChan. */
   if ( !astOK ) new = astDelete( new );

/* Return the new FitsChan pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */

void astEmpty_( AstFitsChan *this ){
   if( !this ) return;
   (**astMEMBER(this,FitsChan,Empty))(this);
}

void astPutFits_( AstFitsChan *this, const char *card, int overwrite ){
   if( !astOK ) return;
   (**astMEMBER(this,FitsChan,PutFits))(this,card,overwrite);
}

void astDelFits_( AstFitsChan *this ){
   if( !astOK ) return;
   (**astMEMBER(this,FitsChan,DelFits))(this);
}

int astFitsEof_( AstFitsChan *this ){
   if( !this ) return 1;
   return (**astMEMBER(this,FitsChan,FitsEof))( this );
}

void astFitsSetCom_( AstFitsChan *this, const char *name, 
                         const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetCom))( this, name, comment, overwrite );
}

void astFitsSetI_( AstFitsChan *this, const char *name, int value, 
                     const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetI))( this, name, value, comment, overwrite );
}

void astFitsSetF_( AstFitsChan *this, const char *name, double value, 
                       const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetF))( this, name, value, comment, overwrite );
}

void astFitsSetS_( AstFitsChan *this, const char *name, const char *value, 
                        const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetS))( this, name, value, comment, overwrite );
}

void astFitsSetCF_( AstFitsChan *this, const char *name, double *value, 
                          const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetCF))( this, name, value, comment, overwrite );
}

void astFitsSetCI_( AstFitsChan *this, const char *name, int *value, 
                          const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetCI))( this, name, value, comment, overwrite );
}

void astFitsSetL_( AstFitsChan *this, const char *name, int value, 
                         const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetL))( this, name, value, comment, overwrite );
}

void astClearCard_( AstFitsChan *this ){
   if( !this ) return;
   (**astMEMBER(this,FitsChan,ClearCard))( this );
}

void astSetCard_( AstFitsChan *this, int card ){
   if( !this ) return;
   (**astMEMBER(this,FitsChan,SetCard))( this, card );
}

int astTestCard_( AstFitsChan *this ){
   if( !this ) return 0;
   return (**astMEMBER(this,FitsChan,TestCard))( this );
}

int astGetCard_( AstFitsChan *this ){
   if( !this ) return 0;
   return (**astMEMBER(this,FitsChan,GetCard))( this );
}

int astGetNcard_( AstFitsChan *this ){
   if( !this ) return 0;
   return (**astMEMBER(this,FitsChan,GetNcard))( this );
}

int astFitsGetCF_( AstFitsChan *this, const char *name, double *value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetCF))( this, name, value );
}

int astFitsGetCI_( AstFitsChan *this, const char *name, int *value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetCI))( this, name, value );
}

int astFitsGetF_( AstFitsChan *this, const char *name, double *value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetF))( this, name, value );
}

int astFitsGetI_( AstFitsChan *this, const char *name, int *value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetI))( this, name, value );
}

int astFitsGetL_( AstFitsChan *this, const char *name, int *value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetL))( this, name, value );
}

int astFitsGetS_( AstFitsChan *this, const char *name, char **value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetS))( this, name, value );
}

int astFitsGetCom_( AstFitsChan *this, const char *name, char **comment ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetCom))( this, name, comment );
}

int astKeyFields_( AstFitsChan *this, const char *filter, int maxfld, 
                int *ubnd, int *lbnd ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,KeyFields))( this, filter, maxfld,
           ubnd, lbnd );
}

int astFindFits_( AstFitsChan *this, const char *name, char *card, int inc ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FindFits))( this, name, card, inc );
}

int astGetEncoding_( AstFitsChan *this ){
   if( !astOK ) return UNKNOWN_ENCODING;
   return (**astMEMBER(this,FitsChan,GetEncoding))( this );
}

int astGetCDMatrix_( AstFitsChan *this ){
   if( !astOK ) return 2;
   return (**astMEMBER(this,FitsChan,GetCDMatrix))( this );
}

