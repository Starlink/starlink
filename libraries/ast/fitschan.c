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
*     - AllWarnings: A list of the available conditions
*     - DefB1950: Use FK4 B1950 as default equatorial coordinates?
*     - Card: Index of current FITS card in a FitsChan
*     - Encoding: System for encoding Objects as FITS headers
*     - FitsDigits: Digits of precision for floating-point FITS values
*     - Ncard: Number of FITS header cards in a FitsChan
*     - Warnings: Produces warnings about selected conditions

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
*     12-FEB-1998 (DSB):
*        Modified EncodeFloat to avoid exceeding the 20 character FITS 
*        limit wherever possible if FitsDigits is positive.
*     10-MAY-1998 (DSB):
*        Bug fixed in astSplit which caused comments associated with string
*        keywords to be lost when storing the card in a FitsChan.
*     15-JUN-1999 (DSB):
*        Report an error if an unrecognised projection name is supplied.
*     9-DEC-1999 (DSB):
*        - Fixed bug in WcsNatPole which could result in longitude values
*        being out by 180 degrees for cylindrical projections such as CAR.
*        - Only report an "unrecognised projection" error for CTYPE values
*        which look like celestial longitude or latitude axes (i.e. if the 
*        first 4 characters are "RA--", "DEC-", "xLON" or "xLAT", and the
*        fifth character is "-").
*        - Added function SpecTrans to translated keywords related to the 
*        IRAF ZPX projection into keyword for the standard ZPN projection.
*        - Add ICRS as a valid value for the RADECSYS keyword. Since the
*        SkyFrame class does not yet support ICRS, an FK5 SkyFrame is
*        created if RADECSYS=ICRS.
*     16-DEC-1999 (DSB):
*        - Modified SpecTrans so that all keywords used to created a 
*        standard WCS representation from a non-standard one are consumed 
*        by the astRead operation.
*        - Changed the text of ASTWARN cards added to the FitsChan if an
*        IRAF ZPX projection is found to require unsupported corrections.
*        - Simplified the documentation describing the handling of the IRAF 
*        ZPX projection.
*        - Fixed code which assumed that the 10 FITS-WCS projection
*        parameters were PROJP1 -> PROJP10. In fact they are PROJP0 -
*        PROJP9. This could cause projection parameter values to be
*        incorrectly numbered when they are written out upon deletion of 
*        the FitsChan.
*     1-FEB-2000 (DSB):
*        Check that FITS_IRAF encoding is not being used before using a
*        PC matrix when reading WCS information from a header. This is
*        important if the header contains both PC and CD matrices.
*     8-FEB-2000 (DSB):
*        - Header cards are now only consumed by an astRead operation if the
*        operation succeeds (i.e. returns a non-null Object).
*        - The original FITS-WCS encoding has been renamed as FITS-PC (to
*        indicate the use of a PCiiijjj matrix), and a new FITS-WCS
*        encoding has been added.
*        - The disabled CDMatrix attribute has been removed.
*        - Bug in LinearMap corrected which prevented genuinely linear 
*        Mappings from being judged to be linear. This bug was previously
*        fudged (so it now appears) by the introduction of the test vector
*        length factor (see History entry for 7-OCT-1998). This test
*        vector length scale factor has consequently now been removed.
*        - Added FITS-AIPS encoding.
*        - The critical keywords used to select default encoding have been
*        changed.
*        - Support for common flavours of IRAF TNX projections added.
*        - The algorithm used to find a WcsMap in the supplied FrameSet 
*        has been improved so that compound Mappings which contain complex
*        mixtures of parallel and serial Mappings can be translated into
*        FITS-WCS encoding.
*        - Trailing white space in string keyword values is now retained
*        when using foreign encodings to enable correct concatenation where 
*        a string has been split over several keywords. E.g. if 2 string 
*        keywords contain a list of formatted numerical values (e.g. IRAF 
*        WAT... keywords), and the 1st one ends "0.123 " and the next one 
*        begins "1234.5 ", the trailing space at the end of the first keyword 
*        is needed to prevent the two numbers being merged into "0.1231234.5". 
*        Trailing spaces in native encodings is still protected by enclosing 
*        the whole string in double quotes. 
*        - The Channel methods WriteString and GetNextData can now save
*        and restore strings of arbitary length. This is done by storing
*        as much of the string as possible in the usual way, and then
*        storing any remaining characters in subsequent CONTINUE cards,
*        using the FITSIO conventions. This storage and retrieval of long
*        strings is only available for native encodings.
*     19-MAY-2000 (DSB):
*        Added attribute Warnings. Lowered DSS in the priority list
*        of encodings implemented by GetEncoding.
*     6-OCT-2000 (DSB):
*        Increased size of buffers used to store CTYPE values to take
*        account of the possiblity of lots of trailing spaces.
*     5-DEC-2000 (DSB):
*        Add support for the WCSNAME FITS keyword.
*     12-DEC-2000 (DSB):
*        Add a title to each physical, non-celestial coord Frame based on 
*        its Domain name (if any).
*     3-APR-2001 (DSB):
*        -  Use an "unknown" celestial coordinate system, instead of a
*        Cartesian coordinate system, if the CTYPE keywords specify an
*        unknown celestial coordinate system.
*        -  Do not report an error if there are no CTYPE keywords in the 
*        header (assume a unit mapping, like in La Palma FITS files).
*        -  Add a NoCTYPE warning condition.
*        -  Added AllWarnings attribute.
*        -  Ensure multiple copies of identical warnings are not produced.
*        -  Use the Object Ident attribute to store the identifier letter
*        associated with each Frame read from a secondary axis description,
*        so that they can be given the same letter when they are written
*        out to a new FITS file.
*     10-AUG-2001 (DSB):
*        - Corrected function value returned by SkySys to be 1 unless an
*        error occurs. This error resulted in CAR headers being produced
*        by astWrite with CRVAL and CD values till in radians rather than 
*        degrees.
*        - Introduced SplitMap2 in order to guard against producing
*        celestial FITS headers for a Mapping which includes more than
*        one WcsMap.
*     13-AUG-2001 (DSB):
*        - Modified FixNew so that it retains the current card index if possible.
*        This fixed a bug which could cause headers written out using Native 
*        encodings to be non-contiguous.
*        - Corrected ComBlock to correctly remove AST comment blocks in
*        native encoded fitschans.
*     14-AUG-2001 (DSB:
*        - Modified FixUsed so that it it does not set the current card
*        back to the start of file if the last card in the FitsChan is 
*        deleted.
*     16-AUG-2001 (DSB):
*        Modified WcsNative to limit reference point latitude to range
*        +/-90 degs (previously values outside this range were wrapped
*        round onto the opposite meridian). Also added new warning
*        condition "badlat".
*     23-AUG-2001 (DSB):
*        - Re-write LinearMap to use a least squares fit.
*        - Check that CDj_i is not AST__BAD within WcsWithWcs when
*        forming the increments along each physical axis.
*     28-SEP-2001 (DSB):
*        GoodWarns changed so that no error is reported if a blank list
*        of conditions is supplied.
*     12-OCT-2001 (DSB):
*        - Added DefB1950 attribute.
*        - Corrected equations which calculate CROTA when writing 
*        FITS-AIPS encodings.
*        - Corrected equations which turn a CROTA value into a CD matrix.
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

/* Macro which takes a pointer to a FitsCard and returns non-zero if the 
   card has been used and so should be ignored. */
#define CARDUSED(card)  ( \
             ( IgnoreUsed == 2 && \
                ( (FitsCard *) (card) )->flags & PROVISIONALLY_USED ) || \
             ( IgnoreUsed >= 1 && \
                ( (FitsCard *) (card) )->flags & USED ) )

/* Set of characters used to encode a "sequence number" at the end of
   FITS keywords in an attempt to make them unique.. */
#define SEQ_CHARS "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"

/* A general tolerance for equality between floating point values. */
#define TOL1 10.0*DBL_EPSILON

/* A tolerance for equality between angular values in radians. */
#define TOL2 1.0E-10

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Macro to check for equality of floating point angular values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. The smallest
   significant angle is assumed to be 1E-9 radians (0.0002 arc-seconds).*/
#define EQUALANG(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=MAX(1.0E5*(fabs(aa)+fabs(bb))*DBL_EPSILON,1.0E-9))))

/* Macro to compare an angle in radians with zero, allowing some tolerance. */
#define ZEROANG(aa) (fabs(aa)<1.0E-9)

/* Constants: */
#define UNKNOWN_ENCODING  -1
#define NATIVE_ENCODING    0
#define FITSPC_ENCODING    1
#define DSS_ENCODING       2
#define FITSWCS_ENCODING   3
#define FITSIRAF_ENCODING  4
#define FITSAIPS_ENCODING  5
#define MAX_ENCODING       5
#define UNKNOWN_STRING     "UNKNOWN"
#define NATIVE_STRING      "NATIVE"
#define FITSPC_STRING      "FITS-PC"
#define FITSPC_STRING2     "FITS_PC"
#define DSS_STRING         "DSS"
#define FITSWCS_STRING     "FITS-WCS"
#define FITSWCS_STRING2    "FITS_WCS"
#define FITSIRAF_STRING    "FITS-IRAF"
#define FITSIRAF_STRING2   "FITS_IRAF"
#define FITSAIPS_STRING    "FITS-AIPS"
#define FITSAIPS_STRING2   "FITS_AIPS"
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
#define ICRS               5
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
#define MXCTYPELEN        81
#define ALLWARNINGS       " noequinox noradesys nomjd-obs nolonpole nolatpole tnx zpx badcel noctype badlat "
#define NPFIT             10

/* Each card in the fitschan has a set of flags associated with it,
   stored in different bits of the "flags" item within each FitsCard
   structure (note, in AST V1.4 these flags were stored in the "del"
   item... Dump and LoadFitsChan will need to be changed to use a
   correspondingly changed name for the external representation of this
   item). The following flags are currently defined: */

/* "USED" - This flag indicates that the the card has been used in the
   construction of an AST Object returned by astRead. Such cards should
   usually be treated as if they do not exist, i.e. they should not be 
   used again by subsequent calls to astRead, they should not be recognised
   by public FitsChan methods which search the FitsChan for specified
   cards, and they should not be written out when the FitsChan is deleted.
   This flag was the only flag available in AST V1.4, and was called
   "Del" (for "deleted"). Used cards are retained in order to give an
   indication of where abouts within the header new cards should be placed
   when astWrite is called (i.e. new cards should usually be placed at
   the same point within the header as the cards which they replace). */
#define USED 	1

/* "PROVISIONALLY_USED" - This flag indicates that the the card is being
   considered as a candidate for inclusion in the construction of an AST 
   Object. If the Object is constructed succesfully, cards flagged as 
   "provisionally used" will be changed to be flagged as "definitely used"
   (using the USED flag). If the Object fails to be constructed
   succesfully (if some required cards are missing from the FitsChan
   for instance), then "provisionally used" cards will be returned to the
   former state which they had prior to the attempt to construct the
   object. */
#define PROVISIONALLY_USED 2

/* "NEW" - This flag indicates that the the card has just been added to
   the FitsChan and may yet proove to be unrequired. For instance if the
   supplied Object is not of an appropriate flavour to be stored using
   the requested encoding, all "new" cards which were added before the 
   inappropriateness was discovered will be removed from the FitsChan.
   Two different levels of "newness" are available. */
#define NEW1 4
#define NEW2 8

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
   int flags;                 /* Flags for each card */
   size_t size;               /* Size of data value */
   struct FitsCard *next;     /* Pointer to next structure in list. */
   struct FitsCard *prev;     /* Pointer to previous structure in list. */
} FitsCard;   


typedef struct FitsKeySeq {   /* Associate a keyword with a sequence no. */
   char *key;                 /* Pointer to basic FITS keyword string */
   int seq;                   /* Sequence number last used */
   struct FitsKeySeq *next;   /* Pointer to next list element */
} FitsKeySeq;


/* Structure used to store information derived from the FITS WCS keyword 
   values in a form more convenient to further processing. Conventions
   for units, etc, for values in a FitsStore follow FITS-WCS (e.g. angular 
   values are stored in degrees, equinox is B or J depending on RADECSYS, 
   etc). */
typedef struct FitsStore {
   char ***ctype;
   char ***ctype_com;
   char ***cunit;
   char ***radesys;
   char ***wcsname;
   double ***cd;
   double ***crpix;
   double ***crval;
   double ***equinox;
   double ***latpole;
   double ***lonpole;
   double ***mjdobs;
   double ***pv;
   int naxis;
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
                                   "complex integer", "logical",
                                   "continuation string" };

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_getfull)( AstChannel * );
static int (* parent_getskip)( AstChannel * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );
static int (* parent_write)( AstChannel *, AstObject * );
static AstObject *(* parent_read)( AstChannel * );

/* Number of output items written since the last "Begin" or "IsA"
   output item, and level of Object nesting during recursive
   invocation of the astWrite method. */
static int items_written = 0;
static int write_nest = -1;

/* Indentation level for indented comments when writing Objects to a
   FitsChan. */
static int current_indent;

/* Text values used to represent Encoding values externally. */
static const char *xencod[6] = { NATIVE_STRING, FITSPC_STRING,
                                 DSS_STRING, FITSWCS_STRING, 
                                 FITSIRAF_STRING, FITSAIPS_STRING };

/* IgnoreUsed: If 2, then cards which have been marked as either "definitely 
   used" or "provisionally used" (see the USED flag above) will be ignored 
   when searching the FitsChan, etc (i.e. they will be treated as if they 
   have been removed from the FitsChan). If 1, then cards which have been 
   "definitely used" will be skipped over. If zero then no cards will be 
   skipped over. */
int IgnoreUsed;

/* MarkNew: If non-zero, then all cards added to the FitsChan will be
   marked with both the NEW1 and NEW2 flags (see above). If zero then 
   new cards will not be marked with either NEW1 or NEW2. */
int MarkNew;

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

static void ClearDefB1950( AstFitsChan * );
static int GetDefB1950( AstFitsChan * );
static int TestDefB1950( AstFitsChan * );
static void SetDefB1950( AstFitsChan *, int );

static void ClearWarnings( AstFitsChan * );
static const char *GetWarnings( AstFitsChan * );
static int TestWarnings( AstFitsChan * );
static void SetWarnings( AstFitsChan *, const char * );

static int GetNcard( AstFitsChan * );
static const char *GetAllWarnings( AstFitsChan * );

static AstObject *FsetFromStore( AstFitsChan *, FitsStore *, const char *, const char * );
static AstObject *Read( AstChannel * );
static FitsCard *GetLink( FitsCard *, int, const char *, const char * );
static FitsStore *FitsToStore( AstFitsChan *, int, const char *, const char * );
static FitsStore *FsetToStore( AstFrameSet *, int, double *, int *, int *, const char *, const char * );
static char *CardComm( AstFitsChan * );
static char *CardName( AstFitsChan * );
static char *GetItemC( char ****, int, char, char *, const char *method, const char *class );
static char *SourceWrap( const char *(*)( void ) );
static char *UnPreQuote( const char * );
static char *FormatKey( char *, int, int, char );
static char GetMaxS( double ****item );
static const char *GetAttrib( AstObject *, const char * );
static double DateObs( const char * );
static double GetItem( double ****, int, int, char, char *, const char *method, const char *class );
static int *CardFlags( AstFitsChan * );
static int CardType( AstFitsChan * );
static int CheckFitsName( const char *, const char *, const char * );
static int ChrLen( const char * );
static int CnvType( int, void *, int, int, void *, const char *, const char *, const char * );
static int CnvValue( AstFitsChan *, int , void *, const char *);
static int ComBlock( AstFitsChan *, int, const char *, const char * );
static int CountFields( const char *, char, const char *, const char * );
static int EncodeFloat( char *, int, int, int, double );
static int EncodeValue( AstFitsChan *, char *, int, int, const char * );
static int FindKeyCard( AstFitsChan *, const char *, const char *, const char * );
static int FindString( int, const char *[], const char *, const char *, const char *, const char * );
static int FitLinear( AstMapping *, int, double *, double *, double *, double * );
static int FitsEof( AstFitsChan * );
static int FitsFromStore( AstFitsChan *, FitsStore *, int, int, int, const char *, const char * );
static int FitsGetCF( AstFitsChan *, const char *, double * );
static int FitsGetCI( AstFitsChan *, const char *, int * );
static int FitsGetCom( AstFitsChan *, const char *, char ** );
static int FitsGetF( AstFitsChan *, const char *, double * );
static int FitsGetI( AstFitsChan *, const char *, int * );
static int FitsGetL( AstFitsChan *, const char *, int * );
static int FitsGetS( AstFitsChan *, const char *, char ** );
static int FitsGetCN( AstFitsChan *, const char *, char ** );
static int FitsSet( AstFitsChan *, const char *, void *, int, const char *, int );
static int FullForm( const char *, const char *, int );
static int GetFull( AstChannel * );
static int GetMaxIM( double ****item );
static int GetSkip( AstChannel * );
static int GetValue( AstFitsChan *, char *, int, void *, int, const char *, const char * );
static int GoodWarns( const char * );
static int KeyFields( AstFitsChan *, const char *, int, int *, int * );
static int LinearMap( AstMapping *, int, double *, double *, double );
static int Match( const char *, const char *, int, int *, int *, const char *, const char * );
static int MatchChar( char, char, const char *, const char *, const char * );
static int MatchFront( const char *, const char *, char *, int *, int *, int *, const char *, const char *, const char * );
static int MoveCard( AstFitsChan *, int, const char *, const char * );
static int SearchCard( AstFitsChan *, const char *, const char *, const char *);
static int SplitMap( AstMapping *, int, AstMapping **, AstMapping **, AstMapping ** );
static int SplitMap2( AstMapping *, int, AstMapping **, AstMapping **, AstMapping ** );
static int TestAttrib( AstObject *, const char * );
static int Use( AstFitsChan *, int, int );
static int Ustrcmp( const char *, const char * );
static int Ustrncmp( const char *, const char *, size_t );
static int WcsNatPole( AstFitsChan *, AstWcsMap *, double, double, double, double *, double *, double * );
static int Write( AstChannel *, AstObject * );
static int astSplit_( const char *, char **, char **, char **, const char *, const char * );
static void *CardData( AstFitsChan *, size_t * );
static void AddFrame( AstFitsChan *, AstFrameSet *, int, FitsStore *, char, const char *, const char * );  
static int AddVersion( AstFrameSet *, int, int, FitsStore *, int, double *, int *, int *, char, const char *, const char * );  
static void CheckZero( char *, double );
static void ClearAttrib( AstObject *, const char * );
static void Copy( const AstObject *, AstObject * );
static void CreateKeyword( AstFitsChan *, const char *, char [ FITSNAMLEN + 1 ] );
static void DSSToStore( AstFitsChan *, FitsStore *, const char *, const char * );
static void WCSToStore( AstFitsChan *, AstFitsChan *, FitsStore *, const char *, const char * );
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
static void FitsSetCN( AstFitsChan *, const char *, const char *, const char *, int );
static void FixNew( AstFitsChan *, int, int, const char *, const char * );
static void FixUsed( AstFitsChan *, int, int, const char *, const char * );
static void FormatCard( AstFitsChan *, char *, const char * );
static FitsStore *FreeStore( FitsStore * );
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
static void SetItem( double ****, int, int, char, double );
static void SetItemC( char ****, int, char, char * );
static void SetValue( AstFitsChan *, char *, void *, int, char * );
static void SinkWrap( void (*)( const char * ), const char * );
static AstFitsChan *SpecTrans( AstFitsChan *, int, const char *, const char * );
static void Warn( AstFitsChan *, const char *, const char *, const char *, const char * );
static void WriteBegin( AstChannel *, const char *, const char * );
static void WriteDouble( AstChannel *, const char *, int, int, double, const char * );
static void WriteEnd( AstChannel *, const char * );
static void WriteInt( AstChannel *, const char *, int, int, int, const char * );
static void WriteIsA( AstChannel *, const char *, const char * );
static void WriteObject( AstChannel *, const char *, int, int, AstObject *, const char * );
static void WriteString( AstChannel *, const char *, int, int, const char *, const char * );
static void WriteToSink( AstFitsChan * );
static AstWinMap *WcsShift( FitsStore *, char, const char *, const char * );
static AstMapping *WcsMapping( AstFitsChan *, FitsStore *, char, char **, int *, int *, int *, const char *, const char * );
static AstMatrixMap *WcsMatrix( FitsStore *, char, const char *, const char * );
static AstWcsMap *WcsDeproj( AstFitsChan *, FitsStore *, char, char **, const char *, const char * );
static AstWinMap *WcsAddRef( FitsStore *, int, char, int *, const char *, const char *  );
static AstFrame *WcsFrame( AstFitsChan *, FitsStore *, char, int, char *, int, int, const char *, const char * );
static AstCmpMap *WcsNative( AstFitsChan *, FitsStore *, char, AstWcsMap *, const char *, const char * );
static void FreeItem( double **** );
static void FreeItemC( char **** );
static void WCSFcRead( AstFitsChan *, FitsStore *, const char *, const char * );
static int WcsWithWcs( AstFitsChan *, AstMapping *, AstMapping *, AstMapping *, AstFrame *, int, FitsStore *, double *, char c, const char *, const char * );
static int SkySys( AstSkyFrame *, int, FitsStore *, int, int, char c, const char *, const char * );
static int WcsNoWcs( AstMapping *, AstFrame *, int, double *, FitsStore *, double *, char, const char *, const char *);
static void LinearSky( AstFrame *, int, FitsStore *, char, const char *, const char * );
static void FindWcs( AstFitsChan *, const char *, const char * );
static int DSSFromStore( AstFitsChan *, FitsStore *, int, int, const char *, const char * );
static int AIPSFromStore( AstFitsChan *, FitsStore *, int, int, const char *, const char * );
static int PCFromStore( AstFitsChan *, FitsStore *, int, int, const char *, const char * );
static int IRAFFromStore( AstFitsChan *, FitsStore *, int, int, const char *, const char * );
static int WCSFromStore( AstFitsChan *, FitsStore *, int, int, const char *, const char * );
static int SplitMat( int , double *, double * );

/* Member functions. */
/* ================= */
static void AddFrame( AstFitsChan *this, AstFrameSet *fset, int pixel, 
                      FitsStore *store, char s, const char *method, 
                      const char *class ){
/*
*  Name:
*     AddFrame

*  Purpose:
*     Create a Frame describing a set of axes with a given co-ordinate 
*     version, and add it to the supplied FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void AddFrame( AstFitsChan *this, AstFrameSet *fset, int pixel, 
*                    FitsStore *store, char s, const char *method, 
*                    const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A Frame is created describing axis with a specific co-ordinate
*     version character, reading information from the supplied FitsStore. 
*     A suitable Mapping is created to connect the new Frame to the pixel 
*     (GRID) Frame in the supplied FrameSet, and the Frame is added into 
*     the FrameSet using this Mapping.

*  Parameters:
*     this
*        The FitsChan from which the keywords were read. Warning messages
*        are added to this FitsChan if the celestial co-ordinate system is 
*        not recognized. 
*     fset
*        Pointer to the FrameSet to be extended.
*     pixel
*        The index of the pixel (GRID) Frame within fset.
*     store
*        The FitsStore containing the required information extracted from 
*        the FitsChan.
*     s
*        The co-ordinate version character. A space means the primary
*        axis descriptions. Otherwise the supplied character should be 
*        an upper case alphabetical character ('A' to 'Z'). 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.
*/

/* Local Variables: */
   AstFrame *frame;            /* Requested Frame */
   AstMapping *mapping;        /* Mapping from pixel to requested Frame */
   int axlon;                  /* Index of the longitude axis */
   int axlat;                  /* Index of the latitude axis */
   int prj;                    /* Projection code */
   char *sys;                  /* Pointer to celestial co-ord. system code */

/* Check the inherited status. */
   if( !astOK ) return;

/* Get a Mapping between pixel coordinates and physical coordinates, using the
   requested axis descriptions. */
   mapping = WcsMapping( this, store, s, &sys, &prj, &axlon, &axlat, method, class );

/* Get a Frame describing the physical coordinate system. */
   frame = WcsFrame( this, store, s, prj, sys, axlon, axlat, method, class );

/* Add the Frame into the FrameSet, and annul the mapping and frame. */
   astAddFrame( fset, pixel, mapping, frame );

   frame = astAnnul( frame );
   mapping = astAnnul( mapping );

}

static int AddVersion( AstFrameSet *fset, int ipix, 
                       int iphy, FitsStore *store, int naxis, double *dim,
                       int *axlat, int *axlon, char s, 
                       const char *method, const char *class ){
/*
*  Name:
*     AddVersion

*  Purpose:
*     Add values to a FitsStore describing a specified Frame in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int AddVersion( AstFrameSet *fset, int ipix, int iphy,
*                     FitsStore *store, int naxis, double *dim, int *axlat, 
*                     int *axlon, char s, const char *method, 
*                     const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Values are added to the supplied FitsStore describing the specified
*     physical co-ordinate Frame, and its relationship to the specified
*     pixel Frame. These values are based on the standard FITS-WCS 
*     conventions.

*  Parameters:
*     this
*        The FitsChan.
*     fset
*        Pointer to the FrameSet.
*     ipix
*        The index of the pixel (GRID) Frame within fset.
*     iphy
*        The index of the Frame within fset to use as the physical
*        co-ordinate Frame.
*     store
*        The FitsStore in which to store the information extracted from 
*        the FrameSet.
*     naxis
*        The number of axes in the Base Frame of the supplied FrameSet.
*     dim 
*        Pointer to an array of pixel axis dimensions. Individual elements 
*        will be AST__BAD if dimensions are not known.
*     axlat
*        A pointer to a location at which to return the index of the
*        celestial latitude axis. -1 is returned if there is no celestial 
*        latitude axis.
*     axlon
*        A pointer to a location at which to return the index of the
*        celestial longitude axis. -1 is returned if there is no celestial 
*        longitude axis.
*     s
*        The co-ordinate version character. A space means the primary
*        axis descriptions. Otherwise the supplied character should be 
*        an upper case alphabetical character ('A' to 'Z'). 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Retuned Value:
*     A value of 1 is returned if the iphy Frame was siccesfulyl added to
*     the FitsStore. A value of zero is returned otherwise.

*/

/* Local Variables: */
   AstFrame *frame;         /* Requested Frame */
   AstMapping *fmap;        /* Full Mapping before simplification */
   AstMapping *map1;        /* Mapping from pixel to rel. phys. coords */
   AstMapping *map2;        /* Mapping from rel. phys. to Nat. Sph. coords */
   AstMapping *map3;        /* Mapping from Nat. Sph. to abs. phys. coords */
   AstMapping *mapping;     /* Mapping from pixel to requested Frame */
   int ret;                 /* Returned value */

/* Initialise */
   ret = 0;
   if( axlat ) *axlat = -1;
   if( axlon ) *axlon = -1;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Get a pointer to the Mapping from pixel coordinates to physical
   coordinates, and simplify it. */
   fmap = astGetMapping( fset, ipix, iphy );
   mapping = astSimplify( fmap );
   fmap = astAnnul( fmap );

/* Check it has the same number of inputs and outputs. */
   if( naxis == astGetNout( mapping ) ){

/* Get a pointer to the physical Frame. */
      frame = astGetFrame( fset, iphy );

/* Split the mapping up into a list of serial component mappings, and
   locate the first WcsMap in this list. The first Mapping returned by
   this call is the result of compounding all the Mappings up to (but not
   including) the WcsMap, the second returned Mapping is the (inverted) 
   WcsMap, and the third returned Mapping is anything following the WcsMap. 
   Only proceed if one and only one WcsMap is found. */
      if( SplitMap( mapping, astGetInvert( mapping ), &map1, &map2, &map3 ) ){
         ret = WcsWithWcs( NULL, map1, map2, map3, frame, naxis, store, dim, 
                           s, method, class );

/* Return longitude and latitude axes. */
         if( axlon ) *axlon = astGetWcsAxis( (AstWcsMap *) map2,  0 );
         if( axlat ) *axlat = astGetWcsAxis( (AstWcsMap *) map2,  1 );

/* Annul the individual Mappings */
         map1 = (AstMapping *) astAnnul( map1 );
         map2 = (AstMapping *) astAnnul( map2 );
         map3 = (AstMapping *) astAnnul( map3 );

/* If no WcsMap was found, we can only encode the FrameSet into FITS-WCS if
   the Mapping from pixel to physical coordinates is linear on every
   axis. */
      } else {
         ret = WcsNoWcs( mapping, frame, naxis, NULL, store, dim, s, method,
                         class );

/* If the physical frame is a SkyFrame, we may be able to desribe it using 
   the linear CAR projection. */
         if( ret ) LinearSky( frame, naxis, store, s, method, class );
      }

/* Annul the Frame */
      frame = (AstFrame *) astAnnul( frame );

   }

/* Annul the Mapping */
   mapping = (AstMapping *) astAnnul( mapping );

/* If an error has occurred, return zero */
   return astOK ? ret : 0;

}

static int AIPSFromStore( AstFitsChan *this, FitsStore *store, 
                          int axlat, int axlon, const char *method, 
                          const char *class ){
/*
*  Name:
*     AIPSFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan using FITS-AIPS encoding.

*  Type:
*     Private function.

*  Synopsis:
*     int AIPSFromStore( AstFitsChan *this, FitsStore *store, 
*                        int axlat, int axlon, const char *method, 
*                        const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using FITS-AIPS encoding.
*
*     AIPS encoding is like FITS-WCS encoding but with the following
*     restrictions:
*
*     1) The celestial projection must not have any projection parameters
*     which are not set to their default values. The one exception to this 
*     is that SIN projections are acceptable if the associated projection 
*     parameter PV<axlat>_1 is zero and PV<axlat>_2 = cot( reference point 
*     latitude). This is encoded using the string "-NCP". The SFL projection 
*     is encoded using the string "-GLS". Note, the original AIPS WCS
*     system only recognised a small subset of the currently available
*     projections, but some more recent AIPS-like software recognizes some 
*     of the new projections included in the FITS-WCS encoding. The AIT, 
*     GLS and MER can only be written if the CRVAL keywords are zero for 
*     both longitude and latitude axes.
*
*     2) The celestial axes must be RA/DEC, galactic or ecliptic.   
*
*     3) LONPOLE and LATPOLE must take their default values.
*
*     4) Only primary axis descriptions are written out.
*
*     5) EPOCH is written instead of EQUINOX & RADECSYS, and uses the 
*        IAU 1984 rule ( EPOCH < 1984.0 is treated as a Besselian epoch 
*        and implies RADECSYS=FK4,  EPOCH >= 1984.0 is treated as a 
*        Julian epoch and implies RADECSYS=FK5). The RADECSYS & EQUINOX
*        values in the FitsStore must be consistent with this rule.
*
*     6) Any rotation produced by the CD matrix must be restricted to
*        the celestial plane, and must involve no shear. A CROTA keyword
*        with associated CDELT values are produced instead of the CD
*        matrix.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   char *comm;         /* Pointer to comment string */
   char *cval;         /* Pointer to string keyword value */
   char combuf[80];    /* Buffer for FITS card comment */
   char lattype[MXCTYPELEN];/* Latitude axis CTYPE */
   char lontype[MXCTYPELEN];/* Longitude axis CTYPE */
   char s;             /* Co-ordinate version character */
   char sign[2];       /* Fraction's sign character */
   double *cdelt;      /* Pointer to CDELT array */
   double cdlat_lon;   /* Off-diagonal CD element */
   double cdlon_lat;   /* Off-diagonal CD element */
   double coscro;      /* Cos( CROTA ) */
   double crota;       /* CROTA value to use */
   double epoch;       /* Epoch of reference equinox */
   double fd;          /* Fraction of a day */
   double latval;      /* CRVAL for latitude axis */
   double lonval;      /* CRVAL for longitude axis */
   double mjd99;       /* MJD at start of 1999 */
   double p1, p2;      /* Projection parameters */
   double rho_a;       /* First estimate of CROTA */
   double rho_b;       /* Second estimate of CROTA */
   double sincro;      /* Sin( CROTA ) */
   double val;         /* General purpose value */
   int i;              /* Axis index */
   int ihmsf[ 4 ];     /* Hour, minute, second, fractional second */
   int iymdf[ 4 ];     /* Year, month, date, fractional day */
   int j;              /* Axis index */
   int jj;             /* SlaLib status */
   int naxis;          /* No. of axes */
   int ok;             /* Is FitsSTore OK for IRAF encoding? */
   int prj;            /* Projection type */

/* Check the inherited status. */
   if( !astOK ) return 0;

/* First check that the values in the FitsStore conform to the
   requirements of the AIPS encoding. Assume they do to begin with. */
   ok = 1;

/* Just do primary axes. */
   s = ' '; 

/* If both longitude and latitude axes are present ...*/
   if( axlon >= 0 && axlat >= 0 ) {

/* Get the CRVAL values for both axes. */
      latval = GetItem( &( store->crval ), axlat, 0, s, NULL, method, class );
      if( latval == AST__BAD ) ok = 0;
      
      lonval = GetItem( &( store->crval ), axlon, 0, s, NULL, method, class );
      if( lonval == AST__BAD ) ok = 0;
      
/* Get the CTYPE values for both axes. Extract the projection type as 
   specified by the last 4 characters in the latitude CTYPE keyword value. */
      cval = GetItemC( &(store->ctype), axlon, s, NULL, method, class );
      if( !cval ) {
         ok = 0;
      } else {
         strcpy( lontype, cval );
      }

      cval = GetItemC( &(store->ctype), axlat, s, NULL, method, class );
      if( !cval ) {
         ok = 0;
         prj = AST__WCSBAD;
      } else {
         strcpy( lattype, cval );
         prj = astWcsPrjType( cval + 4 );
      }

/* Check the projection type is OK. */
      if( prj != AST__SIN ){
   
/* There must be no projection parameters. */
         if( GetMaxIM( &(store->pv) ) >= 0 ) {
            ok = 0;

/* For AIT, MER and GLS, check that the reference point is the origin of
   the celestial co-ordinate system. */
         } else if( prj == AST__MER ||
                    prj == AST__AIT ||
                    prj == AST__SFL ) {
            if( latval != 0.0 || lonval != 0.0 ){
               ok = 0;     

/* Change the new SFL projection code to to the older equivalent GLS */
            } else if( prj == AST__SFL ){
               (void) strcpy( lontype + 4, "-GLS" );
               (void) strcpy( lattype + 4, "-GLS" );
            }
         }

/* SIN projections are only acceptable if the associated projection
   parameters are both zero, or if the first is zero and the second 
   = cot( reference point latitude )  (the latter case is equivalent to 
   the old NCP projection). */
      } else {
         p1 = GetItem( &( store->pv ), axlat, 1, s, NULL, method, class );
         p2 = GetItem( &( store->pv ), axlat, 2, s, NULL, method, class );
         if( p1 == AST__BAD ) p1 = 0.0;   
         if( p2 == AST__BAD ) p2 = 0.0;   
         ok = 0;

         if( p1 == 0.0 ) {
            if( p2 == 0.0 ) {
               ok = 1;
   
            } else if( fabs( p2 ) >= 1.0E14 && latval == 0.0 ){
               ok = 1;
               (void) strcpy( lontype + 4, "-NCP" );
               (void) strcpy( lattype + 4, "-NCP" );
   
            } else if( fabs( p2*tan( AST__DD2R*latval ) - 1.0 ) 
                       < 0.01 ){
               ok = 1;
               (void) strcpy( lontype + 4, "-NCP" );
               (void) strcpy( lattype + 4, "-NCP" );
            }
         }
      }

/* Identify the celestial coordinate system from the first 4 characters of the
   longitude CTYPE value. Only RA, galactic longitude, and ecliptic
   longitude can be stored using FITS-AIPS. */
      if( ok && strncmp( lontype, "RA--", 4 ) &&
               strncmp( lontype, "GLON", 4 ) &&
               strncmp( lontype, "ELON", 4 ) ) ok = 0;

/* If the physical Frame requires a LONPOLE or LATPOLE keyword, it cannot
   be encoded using FITS-IRAF. */
      if( GetItem( &(store->latpole), 0, 0, s, NULL, method, class )
          != AST__BAD || 
          GetItem( &(store->lonpole), 0, 0, s, NULL, method, class )
          != AST__BAD ) ok = 0;
   }

/* Save the number of pixel axes */
   naxis = GetMaxIM( &(store->crpix) ) + 1;

/* Allocate memory to store the CDELT values */
   if( ok ) {
      cdelt = (double *) astMalloc( sizeof(double)*naxis );
      if( !cdelt ) ok = 0;
   } else {
      cdelt = NULL;
   }

/* Check that rotation is restricted to the celestial plane, and extract
   the CDELT (diagonal) terms, etc. */
   for( j = 0; j < naxis && ok; j++ ){
      for( i = 0; i < naxis && ok; i++ ){
          val = GetItem( &(store->cd), j, i, s, NULL, method, class );
          if( val == AST__BAD ) val = ( i == j ) ? 1.0 : 0.0;

          if( i == j ){
             cdelt[ i ] = val;

          } else if( j == axlon && i == axlat ){
             cdlon_lat = val;

          } else if( j == axlat && i == axlon ){
             cdlat_lon = val;

          } else if( val != 0.0 ){
             ok = 0;
          }
      }
   }

/* Find the CROTA and CDELT values for the celestial axes. */
   if( ok && axlon >= 0 && axlat >= 0 ) {

      if( cdlat_lon > 0.0 ) {
         rho_a = atan2( cdlat_lon, cdelt[ axlon ] );
      } else if( cdlat_lon == 0.0 ) {
         rho_a = 0.0;
      } else {
         rho_a = atan2( -cdlat_lon, -cdelt[ axlon ] );
      }

      if( cdlon_lat > 0.0 ) {
         rho_b = atan2( cdlon_lat, -cdelt[ axlat ] );
      } else if( cdlon_lat == 0.0 ) {
         rho_b = 0.0;
      } else {
         rho_b = atan2( -cdlon_lat, cdelt[ axlat ] );
      }

      if( fabs( slaDrange( rho_a - rho_b ) ) < 1.0E-3 ){
         crota = 0.5*( slaDranrm( rho_a ) + slaDranrm( rho_b ) );
         coscro = cos( crota );

         if( coscro != 0.0 ){
            cdelt[ axlat ] /= coscro;
            cdelt[ axlon ] /= coscro;
            crota *= AST__DR2D;
         } else {
            sincro = sin( crota );
            cdelt[ axlat ] = -cdlon_lat/sincro;
            cdelt[ axlon ] = cdlat_lon/sincro;
         }      

      } else {
         ok = 0;
      }

   } else {
      crota = 0.0;
   }

/* Get RADECSYS and the reference equinox (called EPOCH in FITS-AIPS). */
   cval = GetItemC( &(store->radesys), 0, s, NULL, method, class );
   epoch = GetItem( &(store->equinox), 0, 0, s, NULL, method, class );

/* If RADECSYS was available, but not epoch, set a default epoch. */
   if( cval ){
      if( epoch == AST__BAD ){

         if( !strcmp( "FK4", cval ) ){
            epoch = 1950.0;
         } else if( !strcmp( "FK5", cval ) ){
            epoch = 2000.0;
         } else {
            ok = 0;
         }

/* If an epoch was supplied, check it is consistent with the IAU 1984
   rule. */
      } else {
         if( !strcmp( "FK4", cval ) ){
            if( epoch >= 1984.0 ) ok = 0;
         } else if( !strcmp( "FK5", cval ) ){
            if( epoch < 1984.0 ) ok = 0;
         } else {
            ok = 0;
         }
      }
   }

/* Only create the keywords if the FitsStore conforms to the requirements
   of the FITS-AIPS encoding. */
   if( ok ) {

/* Get and save CRPIX for all pixel axes. These are required, so break
   if they are not available. */
      for( i = 0; i < naxis && ok; i++ ){
         val = GetItem( &(store->crpix), 0, i, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
         } else {
            sprintf( combuf, "Reference pixel on axis %d", i + 1 );
            SetValue( this, FormatKey( "CRPIX", i + 1, -1, s ), &val, 
                      AST__FLOAT, combuf );
         }
      }

/* Get and save CRVAL for all intermediate axes. These are required, so 
   break if they are not available. */
      for( j = 0; j < naxis && ok; j++ ){
         val = GetItem( &(store->crval), j, 0, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
         } else {
            sprintf( combuf, "Value at ref. pixel on axis %d", j + 1 );
            SetValue( this, FormatKey( "CRVAL", j + 1, -1, s ), &val, 
                      AST__FLOAT, combuf );
         }
      }

/* Get and save CTYPE for all intermediate axes. These are required, so 
   break if they are not available. Use the potentially modified versions 
   saved above for the celestial axes. */
      for( j = 0; j < naxis && ok; j++ ){
         if( j == axlat ) {
            cval = lattype;
         } else if( j == axlon ) {
            cval = lontype;
         } else {
            cval = GetItemC( &(store->ctype), j, s, NULL, method, class );
         }
         if( cval ){
            comm = GetItemC( &(store->ctype_com), j, s, NULL, method, class );
            if( !comm ) {            
               sprintf( combuf, "Type of co-ordinate on axis %d", j + 1 );
               comm = combuf;
            }
            SetValue( this, FormatKey( "CTYPE", j + 1, -1, s ), &cval, 
                      AST__STRING, comm );
         } else {
            ok = 0;
         }
      }

/* CDELT values */
      for( j = 0; j < naxis; j++ ){
         SetValue( this, FormatKey( "CDELT", j + 1, -1, s ), cdelt + j, 
                   AST__FLOAT, "Pixel size" );
      }

/* CROTA */
      if( axlat != -1 ){
         SetValue( this, FormatKey( "CROTA", axlat + 1, -1, s ), &crota, 
                   AST__FLOAT, "Axis rotation" );
      } else {
         SetValue( this, "CROTA1", &crota, AST__FLOAT, "Axis rotation" );
      }

/* Reference equinox */
      if( epoch != AST__BAD ) SetValue( this, "EPOCH", &epoch, AST__FLOAT, 
                                        "Epoch of reference equinox" );

/* Date of observation. */
      val = GetItem( &(store->mjdobs), 0, 0, s, NULL, method, class );
      if( val != AST__BAD ) {

/* The format used for the DATE-OBS keyword depends on the value of the
   keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
   Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format. */
         slaCaldj( 99, 1, 1, &mjd99, &jj );
         if( val < mjd99 ) {
            slaDjcal( 0, val, iymdf, &jj );
            sprintf( combuf, "%2.2d/%2.2d/%2.2d", iymdf[ 2 ], iymdf[ 1 ], 
                     iymdf[ 0 ] - ( ( iymdf[ 0 ] > 1999 ) ? 2000 : 1900 ) ); 
         } else {
            slaDjcl( val, iymdf, iymdf+1, iymdf+2, &fd, &jj );
            slaDd2tf( 3, fd, sign, ihmsf );
            sprintf( combuf, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3dZ",
                     iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
                     ihmsf[2], ihmsf[3] ); 
         }

/* Now store the formatted string in the FitsChan. */
         cval = combuf;
         SetValue( this, "DATE-OBS", (void *) &cval, AST__STRING,
                   "Date of observation" );
      }
   }

/* Release CDELT workspace */
   if( cdelt ) cdelt = (double *) astFree( (void *) cdelt );

/* Return zero or ret depending on whether an error has occurred. */
   return astOK ? ok : 0;
}

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
*        occupied by the data value. NULL can be supplied if this
*        information is not required.

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

static int *CardFlags( AstFitsChan *this ){
/*
*  Name:
*     CardFlags

*  Purpose:
*     Return a pointer to the flags mask for the current card.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int *CardFlags( AstFitsChan *this )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Returns a pointer to the flags mask for the current card. 

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     The pointer to the flags mask.

*  Notes:
*     -  The current card is not changed by this function.
*     -  NULL is returned if the current card is not defined.
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   int *ret;

/* Check the supplied object. */
   if( !this ) return NULL;

/* If the current card is defined, store its deletion flag. */
   if( this->card ){
      ret = &( ( (FitsCard *) this->card )->flags );

/* Otherwise store zero. */
   } else {
      ret =  NULL;
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
   } else if( astOK ){
      astError( AST__INTER, "CheckFitsName(fitschan): AST internal "
                "error; a NULL pointer was supplied for the keyword name. ", 
                method, class );                
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

/* DefB1950 */
/* -------- */
   } else if ( !strcmp( attrib, "defb1950" ) ) {
      astClearDefB1950( this );

/* Warnings. */
/* -------- */
   } else if ( !strcmp( attrib, "warnings" ) ) {
      astClearWarnings( this );

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( astOK && ( !strcmp( attrib, "ncard" ) || 
                          !strcmp( attrib, "allwarnings" ) ) ){
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
*     setting it to the index of the first un-used card in the FitsChan. 
*     This causes the next read operation performed on the FitsChan to 
*     read the first card. Thus, it is equivalent to "rewinding" the FitsChan.

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

   if( CARDUSED(this->card) ){
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
   
         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__STRING data value to ... */
      } else if( otype == AST__STRING || type == AST__CONTINUE ){
         ostring = (char *) odata;
         len = (int) strlen( ostring );

         if( type == AST__FLOAT ){
            if( nc = 0, 
                     ( 1 != sscanf( ostring, "%lf %n", (double *) buff, &nc ) )
                  || (nc < len ) ){
               ret = 0;
            }

         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert an AST__INT data value to ... */
      } else if( otype == AST__INT      ){
         oint = *( (int *) odata );

         if( type == AST__FLOAT ){
            *( (double *) buff ) = (double) oint;

         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a LOGICAL data value to ... */
      } else if( otype == AST__LOGICAL  ){
         oint = *( (int *) odata );
         
         if( type == AST__FLOAT ){
            *( (double *) buff ) = oint ? 1.0 : 0.0;

         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__COMPLEXF data value to ... */
      } else if( otype == AST__COMPLEXF ){
         odouble = ( (double *) odata )[ 0 ];

         if( type == AST__FLOAT ){
            *( (double *) buff ) = odouble;
   
         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

/* Convert a AST__COMPLEXI data value to ... */
      } else if( otype == AST__COMPLEXI ){
         oint = ( (int *) odata )[ 0 ];

         if( type == AST__FLOAT ){
            *( (double *) buff ) = (double) oint;

         } else if( type == AST__STRING || type == AST__CONTINUE  ){
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

         } else if( astOK ){
            ret = 0;
            astError( AST__INTER, "CnvType: AST internal programming error - "
                      "FITS data-type no. %d not yet supported.", type );
         }

      } else if( astOK ){
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
   char del;                     /* Delimiter character */
   char *text;                   /* Pointer to the comment text */
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

/* Increment the number of cards in the comment block. */
         ncard++;

/* Get the text of the comment, and its length. */
         text = CardComm( this );
         if( text ){
            len = strlen( text );

/* Check the first 3 characters. Break out of the loop if they are not
   "AST". */
            if( strncmp( "AST", text, 3 ) ) break;

/* Check the last 3 characters. Break out of the loop if they are not
   "AST". */
            if( strcmp( "AST", text + len - 3 ) ) break;

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
         }

         if( ret ) break;

/* Move on to the next card. If this is not possible (due to us already
   being at the start or end of the FitsChan), then break out of the loop. */
         if( MoveCard( this, incr, method, class ) == 0 ) break;

      }

/* Re-instate the original current card. */
      this->card = card0;

/* If we found a complete comment block, mark it (which is equivalent to
   deleting it except that memory of the cards location within the
   FitsChan is preserved for future use), and then re-instate the original 
   current card. */
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
   while( *b && astOK ){

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

static int DSSFromStore( AstFitsChan *this, FitsStore *store, 
                         int axlat, int axlon, const char *method, 
                         const char *class ){
/*
*  Name:
*     DSSFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan using DSS encoding.

*  Type:
*     Private function.

*  Synopsis:
*     int DSSFromStore( AstFitsChan *this, FitsStore *store, 
*                       int axlat, int axlon, const char *method, 
*                       const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using DSS encoding.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   char *comm;         /* Pointer to comment string */
   char *cval;         /* Pointer to string keyword value */
   char *pltdecsn;     /* PLTDECSN keyword value */
   double amdx[20];    /* AMDXi keyword value */
   double amdy[20];    /* AMDYi keyword value */
   double cnpix1;      /* CNPIX1 keyword value */
   double cnpix2;      /* CNPIX2 keyword value */
   double pltdecd;     /* PLTDECD keyword value */
   double pltdecm;     /* PLTDECM keyword value */
   double pltdecs;     /* PLTDECS keyword value */
   double pltrah;      /* PLTRAH keyword value */
   double pltram;      /* PLTRAM keyword value */
   double pltras;      /* PLTRAS keyword value */
   double pltscl;      /* PLTSCL keyword value */
   double ppo1;        /* PPO1 keyword value */
   double ppo2;        /* PPO2 keyword value */
   double ppo3;        /* PPO3 keyword value */
   double ppo4;        /* PPO4 keyword value */
   double ppo5;        /* PPO5 keyword value */
   double ppo6;        /* PPO6 keyword value */
   double pvx[22];     /* X projection parameter values */
   double pvy[22];     /* Y projection parameter values */
   double val;         /* General purpose value */
   double xpixelsz;    /* XPIXELSZ keyword value */
   double ypixelsz;    /* YPIXELSZ keyword value */
   int i;              /* Loop count */
   int ret;            /* Returned value. */

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Check the image is 2 dimensional. */
   if( GetMaxIM( &(store->crpix) ) != 1 ) return ret;

/* Check the first axis is RA with a TAN projection. */
   cval = GetItemC( &(store->ctype), 0, ' ', NULL, method, class );
   if( !cval || strcmp( "RA---TAN", cval ) ) return ret;

/* Check the second axis is DEC with a TAN projection. */
   cval = GetItemC( &(store->ctype), 1, ' ', NULL, method, class );
   if( !cval || strcmp( "DEC--TAN", cval ) ) return ret;

/* Check that LONPOLE is undefined or is 180 degrees. */
   val = GetItem( &(store->lonpole), 0, 0, ' ', NULL, method, class );
   if( val != AST__BAD && val != 180.0 ) return ret;

/* Check that the RA/DEC system is FK5. */
   cval = GetItemC( &(store->radesys), 0, ' ', NULL, method, class );
   if( !cval || strcmp( "FK5", cval ) ) return ret;

/* Check that equinox is not defined or is 2000.0 */
   val = GetItem( &(store->equinox), 0, 0, ' ', NULL, method, class );
   if( val != AST__BAD && val != 2000.0 ) return ret;

/* Get the pixel sizes from the CD matrix. They must be defined and not
   be zero.  */
   xpixelsz = GetItem( &(store->cd), 0, 0, ' ', NULL, method, class );
   ypixelsz = GetItem( &(store->cd), 1, 1, ' ', NULL, method, class );
   if( xpixelsz == AST__BAD || ypixelsz == AST__BAD ||
       xpixelsz == 0.0 || ypixelsz == 0.0 ) return ret;
   xpixelsz *= -1000.0;
   ypixelsz *= 1000.0;

/* Check the off-diagonal terms are zero. DSS does not allow any rotation. */
   val = GetItem( &(store->cd), 0, 1, ' ', NULL, method, class );
   if( val != AST__BAD && val != 0.0 ) return ret;
   
   val = GetItem( &(store->cd), 1, 0, ' ', NULL, method, class );
   if( val != AST__BAD && val != 0.0 ) return ret;
   
/* Get the required projection parameter values from the store. */
   for( i = 0; i < 22; i++ ){
      pvx[ i ] = GetItem( &(store->pv), 0, i, ' ', NULL, method, class );
      if( pvx[ i ] == AST__BAD ) pvx[ i ] = ( i == 1 ) ? 1.0 : 0.0;

      pvy[ i ] = GetItem( &(store->pv), 1, i, ' ', NULL, method, class );
      if( pvy[ i ] == AST__BAD ) pvy[ i ] = ( i == 1 ) ? 1.0 : 0.0;

   }

/* Check that no other projection parameters have been set. */
   if( GetMaxIM( &(store->pv) ) > 21 ) return ret;

/* Check that specific parameters take their required zero value. */
   if( pvx[ 3 ] != 0.0 || pvy[ 3 ] != 0.0 ) return ret;

   for( i = 11; i < 17; i++ ){
      if( pvx[ i ] != 0.0 || pvy[ i ] != 0.0 ) return ret;
   }

   if( pvx[ 18 ] != 0.0 || pvy[ 18 ] != 0.0 ) return ret;
   if( pvx[ 20 ] != 0.0 || pvy[ 20 ] != 0.0 ) return ret;

/* Check that other projection parameters are related correctly. */
   if( !EQUAL( 2*pvx[ 17 ], pvx[ 19 ] ) ) return ret;
   if( !EQUAL( pvx[ 17 ], pvx[ 21 ] ) ) return ret;

   if( !EQUAL( 2*pvy[ 17 ], pvy[ 19 ] ) ) return ret;
   if( !EQUAL( pvy[ 17 ], pvy[ 21 ] ) ) return ret;

/* Initialise all polynomial co-efficients to zero. */
   for( i = 0; i < 20; i++ ){
      amdx[ i ] = 0.0;
      amdy[ i ] = 0.0;
   }

/* Polynomial co-efficients. There is redundancy here too, so we
   arbitrarily choose to leave AMDX/Y7 and AMDX/Y12 set to zero.  */
   amdx[ 0 ] = 3600.0*pvx[ 1 ];
   amdx[ 1 ] = 3600.0*pvx[ 2 ];
   amdx[ 2 ] = 3600.0*pvx[ 0 ];
   amdx[ 3 ] = 3600.0*pvx[ 4 ];
   amdx[ 4 ] = 3600.0*pvx[ 5 ];
   amdx[ 5 ] = 3600.0*pvx[ 6 ];
   amdx[ 7 ] = 3600.0*pvx[ 7 ];
   amdx[ 8 ] = 3600.0*pvx[ 8 ];
   amdx[ 9 ] = 3600.0*pvx[ 9 ];
   amdx[ 10 ] = 3600.0*pvx[ 10 ];
   amdx[ 12 ] = 3600.0*pvx[ 17 ];

   amdy[ 0 ] = 3600.0*pvy[ 1 ];
   amdy[ 1 ] = 3600.0*pvy[ 2 ];
   amdy[ 2 ] = 3600.0*pvy[ 0 ];
   amdy[ 3 ] = 3600.0*pvy[ 4 ];
   amdy[ 4 ] = 3600.0*pvy[ 5 ];
   amdy[ 5 ] = 3600.0*pvy[ 6 ];
   amdy[ 7 ] = 3600.0*pvy[ 7 ];
   amdy[ 8 ] = 3600.0*pvy[ 8 ];
   amdy[ 9 ] = 3600.0*pvy[ 9 ];
   amdy[ 10 ] = 3600.0*pvy[ 10 ];
   amdy[ 12 ] = 3600.0*pvy[ 17 ];

/* The plate scale is the mean of the first X and Y co-efficients. */
   pltscl = 0.5*( amdx[ 0 ] + amdy[ 0 ] );

/* There is redundancy in the DSS encoding. We can choose an arbitrary 
   pixel corner (CNPIX1, CNPIX2) so long as we use the corresponding origin 
   for the cartesian co-ordinate system in which the plate centre is 
   specified (PPO3, PPO6). Arbitrarily set CNPIX1 and CNPIX2 to one. */
   cnpix1 = 1.0;
   cnpix2 = 1.0;

/* Find the corresponding plate centre PPO3 and PPO6 (other co-efficients
   are set to zero). */
   ppo1 = 0.0;
   ppo2 = 0.0;

   val = GetItem( &(store->crpix), 0, 0, ' ', NULL, method, class );
   if( val == AST__BAD ) return ret;
   ppo3 = xpixelsz*( val + cnpix1 - 0.5 );

   ppo4 = 0.0;
   ppo5 = 0.0;

   val = GetItem( &(store->crpix), 0, 1, ' ', NULL, method, class );
   if( val == AST__BAD ) return ret;
   ppo6 = ypixelsz*( val + cnpix2 - 0.5 );

/* The reference RA. Get it in degrees. */
   val = GetItem( &(store->crval), 0, 0, ' ', NULL, method, class );
   if( val == AST__BAD ) return ret;

/* Convert to hours and ensure it is in the range 0 to 24 */
   val /= 15.0;
   while( val < 0 ) val += 24.0;
   while( val >= 24.0 ) val -= 24.0;

/* Split into hours, mins and seconds. */
   pltrah = (int) val;
   val = 60.0*( val - pltrah );
   pltram = (int) val;
   pltras = 60.0*( val - pltram );

/* The reference DEC. Get it in degrees. */
   val = GetItem( &(store->crval), 1, 0, ' ', NULL, method, class );
   if( val == AST__BAD ) return ret;

/* Ensure it is in the range -180 to +180 */
   while( val < -180.0 ) val += 360.0;
   while( val >= 180.0 ) val -= 360.0;

/* Save the sign. */
   if( val > 0.0 ){
      pltdecsn = "+";
   } else {
      pltdecsn = "-";
      val = -val;
   }

/* Split into degrees, mins and seconds. */
   pltdecd = (int) val;
   val = 60.0*( val - pltdecd );
   pltdecm = (int) val;
   pltdecs = 60.0*( val - pltdecm );

/* Store the DSS keywords in the FitsChan. */
   SetValue( this, "CNPIX1", &cnpix1, AST__FLOAT, "X corner (pixels)" );
   SetValue( this, "CNPIX2", &cnpix2, AST__FLOAT, "Y corner (pixels)" );
   SetValue( this, "PPO1", &ppo1, AST__FLOAT, "Orientation co-efficients" );
   SetValue( this, "PPO2", &ppo2, AST__FLOAT, "" );
   SetValue( this, "PPO3", &ppo3, AST__FLOAT, "" );
   SetValue( this, "PPO4", &ppo4, AST__FLOAT, "" );
   SetValue( this, "PPO5", &ppo5, AST__FLOAT, "" );
   SetValue( this, "PPO6", &ppo6, AST__FLOAT, "" );
   SetValue( this, "XPIXELSZ", &xpixelsz, AST__FLOAT, "X pixel size (microns)" );
   SetValue( this, "YPIXELSZ", &ypixelsz, AST__FLOAT, "Y pixel size (microns)" );
   SetValue( this, "PLTRAH", &pltrah, AST__FLOAT, "RA at plate centre" );
   SetValue( this, "PLTRAM", &pltram, AST__FLOAT, "" );
   SetValue( this, "PLTRAS", &pltras, AST__FLOAT, "" );
   SetValue( this, "PLTDECD", &pltdecd, AST__FLOAT, "DEC at plate centre" );
   SetValue( this, "PLTDECM", &pltdecm, AST__FLOAT, "" );
   SetValue( this, "PLTDECS", &pltdecs, AST__FLOAT, "" );
   SetValue( this, "PLTDECSN", &pltdecsn, AST__STRING, "" );
   SetValue( this, "PLTSCALE", &pltscl, AST__FLOAT, "Plate scale (arcsec/mm)" );

   comm = "Plate solution x co-efficients";
   for( i = 0; i < 20; i++ ){
      SetValue( this, FormatKey( "AMDX", i + 1, -1, ' ' ), amdx + i, 
                AST__FLOAT, comm );
      comm = NULL;
   }

   comm = "Plate solution y co-efficients";
   for( i = 0; i < 20; i++ ){
      SetValue( this, FormatKey( "AMDY", i + 1, -1, ' ' ), amdy + i, 
                AST__FLOAT, comm );
      comm = NULL;
   }

/* If no error has occurred, return one. */
   if( astOK ) ret = 1;

/* Return the answer. */
   return ret;

}

static void DSSToStore( AstFitsChan *this, FitsStore *store, 
                        const char *method, const char *class ){
/*
*  Name:
*     DSSToStore

*  Purpose:
*     Extract WCS information from the supplied FItsChan using a DSS
*     encoding, and store it in the supplied FitsStore.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void DSSToStore( AstFitsChan *this, FitsStore *store, 
                       const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function extracts DSS keywords from the supplied FitsChan, and
*     stores the corresponding WCS information in the supplied FitsStore.
*     The conversion from DSS encoding to standard WCS encoding is
*     described in the Calabretta & Greisen paper "Representations of
*     celestial coordinates in FITS" (A&A, in prep.). Here we use
*     "lambda=1" (i.e. plate co-ordinate are measured in mm, not degrees).
*
*     It is assumed that DSS images are 2 dimensional.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore structure.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*/

/* Local Variables: */
   char *pltdecsn;     /* PLTDECSN keyword value */
   char keyname[10];   /* Buffer for keyword name */
   double amdx[20];    /* AMDXi keyword value */
   double amdy[20];    /* AMDYi keyword value */
   double cnpix1;      /* CNPIX1 keyword value */
   double cnpix2;      /* CNPIX2 keyword value */
   double crval2;      /* Equivalent CRVAL2 keyword value */
   double dummy;       /* Unused keyword value */
   double pltdecd;     /* PLTDECD keyword value */
   double pltdecm;     /* PLTDECM keyword value */
   double pltdecs;     /* PLTDECS keyword value */
   double pltrah;      /* PLTRAH keyword value */
   double pltram;      /* PLTRAM keyword value */
   double pltras;      /* PLTRAS keyword value */
   double ppo3;        /* PPO3 keyword value */
   double ppo6;        /* PPO6 keyword value */
   double pv;          /* Projection parameter value */
   double xpixelsz;    /* XPIXELSZ keyword value */
   double ypixelsz;    /* YPIXELSZ keyword value */
   int i;              /* Loop count */

/* Check the inherited status. */
   if( !astOK ) return;

/* Get the required DSS keywords. Report an error if any are missing. */
   GetValue( this, "CNPIX1", AST__FLOAT, &cnpix1, 1, method, class );
   GetValue( this, "CNPIX2", AST__FLOAT, &cnpix2, 1, method, class );
   GetValue( this, "PPO3", AST__FLOAT, &ppo3, 1, method, class );
   GetValue( this, "PPO6", AST__FLOAT, &ppo6, 1, method, class );
   GetValue( this, "XPIXELSZ", AST__FLOAT, &xpixelsz, 1, method, class );
   GetValue( this, "YPIXELSZ", AST__FLOAT, &ypixelsz, 1, method, class );
   GetValue( this, "PLTRAH", AST__FLOAT, &pltrah, 1, method, class );
   GetValue( this, "PLTRAM", AST__FLOAT, &pltram, 1, method, class );
   GetValue( this, "PLTRAS", AST__FLOAT, &pltras, 1, method, class );
   GetValue( this, "PLTDECD", AST__FLOAT, &pltdecd, 1, method, class );
   GetValue( this, "PLTDECM", AST__FLOAT, &pltdecm, 1, method, class );
   GetValue( this, "PLTDECS", AST__FLOAT, &pltdecs, 1, method, class );

   GetValue( this, "PLTDECSN", AST__STRING, &pltdecsn, 1, method, class );

/* Read other related keywords. We do not need these, but we read them
   so that they are not propagated to any output FITS file. */
   GetValue( this, "PLTSCALE", AST__FLOAT, &dummy, 0, method, class );
   GetValue( this, "PPO1", AST__FLOAT, &dummy, 0, method, class );
   GetValue( this, "PPO2", AST__FLOAT, &dummy, 0, method, class );
   GetValue( this, "PPO4", AST__FLOAT, &dummy, 0, method, class );
   GetValue( this, "PPO5", AST__FLOAT, &dummy, 0, method, class );

/* Get the polynomial co-efficients. These can be defaulted if they are 
   missing, so do not report an error. */
   for( i = 0; i < 20; i++ ){
      (void) sprintf( keyname, "AMDX%d", i + 1 );
      amdx[i] = AST__BAD;
      GetValue( this, keyname, AST__FLOAT, amdx + i, 0, method, class );

      (void) sprintf( keyname, "AMDY%d", i + 1 );
      amdy[i] = AST__BAD;
      GetValue( this, keyname, AST__FLOAT, amdy + i, 0, method, class );
   }

/* Check the above went OK. */
   if( astOK ) {

/* Calculate and store the equivalent PV projection parameters. */
      if( amdx[2] != AST__BAD ) {
         pv = amdx[2]/3600.0;
         SetItem( &(store->pv), 0, 0, ' ', pv );
      }
      if( amdx[0] != AST__BAD ) {
         pv = amdx[0]/3600.0;
         SetItem( &(store->pv), 0, 1, ' ', pv );
      }
      if( amdx[1] != AST__BAD ) {
         pv = amdx[1]/3600.0;
         SetItem( &(store->pv), 0, 2, ' ', pv );
      }
      if( amdx[3] != AST__BAD && amdx[6] != AST__BAD ) {
         pv = ( amdx[3] + amdx[6] )/3600.0;
         SetItem( &(store->pv), 0, 4, ' ', pv );
      }
      if( amdx[4] != AST__BAD ) {
         pv = amdx[4]/3600.0;
         SetItem( &(store->pv), 0, 5, ' ', pv );
      }
      if( amdx[5] != AST__BAD && amdx[6] != AST__BAD ) {
         pv = ( amdx[5] + amdx[6] )/3600.0;
         SetItem( &(store->pv), 0, 6, ' ', pv );
      }
      if( amdx[7] != AST__BAD && amdx[11] != AST__BAD ) {
         pv = ( amdx[7] + amdx[11] )/3600.0;
         SetItem( &(store->pv), 0, 7, ' ', pv );
      }
      if( amdx[8] != AST__BAD ) {
         pv = amdx[8]/3600.0;
         SetItem( &(store->pv), 0, 8, ' ', pv );
      }
      if( amdx[9] != AST__BAD && amdx[11] != AST__BAD ) {
         pv = ( amdx[9] + amdx[11] )/3600.0;
         SetItem( &(store->pv), 0, 9, ' ', pv );
      }
      if( amdx[10] != AST__BAD ) {
         pv = amdx[10]/3600.0;
         SetItem( &(store->pv), 0, 10, ' ', pv );
      }
      if( amdx[12] != AST__BAD ) {
         pv = amdx[12]/3600.0;
         SetItem( &(store->pv), 0, 17, ' ', pv );
         SetItem( &(store->pv), 0, 19, ' ', 2*pv );
         SetItem( &(store->pv), 0, 21, ' ', pv );
      }
      
      if( amdy[2] != AST__BAD ) {
         pv = amdy[2]/3600.0;
         SetItem( &(store->pv), 1, 0, ' ', pv );
      }
      if( amdy[0] != AST__BAD ) {
         pv = amdy[0]/3600.0;
         SetItem( &(store->pv), 1, 1, ' ', pv );
      }
      if( amdy[1] != AST__BAD ) {
         pv = amdy[1]/3600.0;
         SetItem( &(store->pv), 1, 2, ' ', pv );
      }
      if( amdy[3] != AST__BAD && amdy[6] != AST__BAD ) {
         pv = ( amdy[3] + amdy[6] )/3600.0;
         SetItem( &(store->pv), 1, 4, ' ', pv );
      }
      if( amdy[4] != AST__BAD ) {
         pv = amdy[4]/3600.0;
         SetItem( &(store->pv), 1, 5, ' ', pv );
      }
      if( amdy[5] != AST__BAD && amdy[6] != AST__BAD ) {
         pv = ( amdy[5] + amdy[6] )/3600.0;
         SetItem( &(store->pv), 1, 6, ' ', pv );
      }
      if( amdy[7] != AST__BAD && amdy[11] != AST__BAD ) {
         pv = ( amdy[7] + amdy[11] )/3600.0;
         SetItem( &(store->pv), 1, 7, ' ', pv );
      }
      if( amdy[8] != AST__BAD ) {
         pv = amdy[8]/3600.0;
         SetItem( &(store->pv), 1, 8, ' ', pv );
      }
      if( amdy[9] != AST__BAD && amdy[11] != AST__BAD ) {
         pv = ( amdy[9] + amdy[11] )/3600.0;
         SetItem( &(store->pv), 1, 9, ' ', pv );
      }
      if( amdy[10] != AST__BAD ) {
         pv = amdy[10]/3600.0;
         SetItem( &(store->pv), 1, 10, ' ', pv );
      }
      if( amdy[12] != AST__BAD ) {
         pv = amdy[12]/3600.0;
         SetItem( &(store->pv), 1, 17, ' ', pv );
         SetItem( &(store->pv), 1, 19, ' ', 2*pv );
         SetItem( &(store->pv), 1, 21, ' ', pv );
      }

/* Calculate and store the equivalent CRPIX values. */
      if( xpixelsz != 0.0 ) {
         SetItem( &(store->crpix), 0, 0, ' ', 
                  ( ppo3/xpixelsz ) - cnpix1 + 0.5 );
      } else if( astOK ){      
         astError( AST__BDFTS, "%s(%s): FITS keyword XPIXELSZ has illegal "
                   "value 0.0", method, class );
      }   

      if( ypixelsz != 0.0 ) {
         SetItem( &(store->crpix), 0, 1, ' ', 
                  ( ppo6/ypixelsz ) - cnpix2 + 0.5 );
      } else if( astOK ){      
         astError( AST__BDFTS, "%s(%s): FITS keyword YPIXELSZ has illegal "
                   "value 0.0", method, class );
      }   

/* Calculate and store the equivalent CRVAL values. */
      SetItem( &(store->crval), 0, 0, ' ', 
               15.0*( pltrah + pltram/60.0 + pltras/3600.0 ) );
      crval2 = pltdecd + pltdecm/60.0 + pltdecs/3600.0;
      if( !strcmp( pltdecsn, "-") ) crval2 = -crval2;
      SetItem( &(store->crval), 1, 0, ' ', crval2 );

/* Calculate and store the equivalent CD matrix. */
      SetItem( &(store->cd), 0, 0, ' ', -0.001*xpixelsz );
      SetItem( &(store->cd), 1, 1, ' ', 0.001*ypixelsz );

/* Store remaining constant items */
      SetItem( &(store->lonpole), 0, 0, ' ', 180.0 );
      SetItem( &(store->equinox), 0, 0, ' ', 2000.0 );
      SetItemC( &(store->radesys), 0, ' ', "FK5" );
      SetItemC( &(store->ctype), 0, ' ', "RA---TAN" );
      SetItemC( &(store->ctype), 1, ' ', "DEC--TAN" );

   }

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
   int old_ignoreused;        /* Original setting of external IgnoreUsed variable */

/* Store the method and class strings. */
   method = "astEmpty";
   class = astGetClass( this );

/* Delete all cards from the circular linked list stored in the FitsChan,
   starting with the card at the head of the list. */
   old_ignoreused = IgnoreUsed;
   IgnoreUsed = 0;
   astClearCard( this );
   while( !astFitsEof( this ) ) DeleteCard( this, method, class );   
   IgnoreUsed = old_ignoreused;

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
   char *c;
   char *w, *r;
   int i;
   int ldigits;
   int n;
   int ret;

/* Check the global error status. */
   if ( !astOK ) return 0; 

/* The supplied value of "digits" may be negative. Obtain the positive
   value giving the initial number of decimal digits to use. */   
   ldigits = ( digits > 0 ) ? digits : -digits;

/* Loop until a suitably encoded value has been obtained. */
   while( 1 ){

/* Write the value into the buffer.  Most are formatted with a G specifier.
   This will result in values between  -0.001 and -0.0001 being formatted
   without an exponent, and thus occupying (ldigits+6) characters. With
   an exponent, these values would be formatted in (ldigits+5) characters
   thus saving one character. This is important because the default value
   of ldigits is 15, resulting in 21 characters being used by the G
   specifier. This is one more than the maximum allowed by the FITS
   standard. Using an exponent instead would result in 20 characters
   being used without any loss of precision, thus staying within the FITS
   limit. Note, the precision used with the E specifier is one less than
   with the G specifier because the digit to the left of the decimal place
   is significant with the E specifier, and so we only need (ldigits-1)
   significant digits to the right of the decimal point. */
      if( value > -0.001 && value < -0.0001 ) {
         (void) sprintf( buf, "%*.*E", width, ldigits - 1, value );
      } else {
         (void) sprintf( buf, "%*.*G", width, ldigits, value );
      }

/* Check that the value zero is not encoded with a minus sign (e.g. "-0."). */
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
   
         if( len <= 0 && astOK ) {
            astError( AST__BDFTS, "%s(%s): Cannot encode floating point value "
                      "%g into a FITS header card for keyword '%s'.", method,
                      astGetClass( this ), dval, name );
         }

/* AST__STRING & AST__CONTINUE - stored internally in a null terminated array of 
   type "char".  The encoded string is enclosed in single quotes, starting
   at FITS column 11 and ending in at least column 20. Single quotes
   in the string are replaced by two adjacent single quotes. */
      } else if( type == AST__STRING || type == AST__CONTINUE ){
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
      } else if( astOK ){
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

static void FindWcs( AstFitsChan *this, const char *method, const char *class ){
/*
*  Name:
*     FindWcs

*  Purpose:
*     Find the last FITS WCS related keyword in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FindWcs( AstFitsChan *this, const char *method, const char *class  )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A search is made through the FitsChan for the last card which
*     relates to a FITS WCS keyword (any encoding). The next card becomes 
*     the current card. Cards marked as having been read are included.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Notes:
*     -  The current card is left unchanged if no FITS-WCS keyword cards 
*     are found in the FitsChan.

*-
*/

/* Local Variables: */
   const char *keyname;     /* Keyword name from current card */
   int icard;               /* Index of original current card */
   int nfld;                /* Number of fields in keyword template */
   int old_ignoreused;      /* Original value of external variable IgnoreUsed */

/* Check the global status. */
   if( !astOK ) return;

/* Indicate that we should not skip over cards marked as having been
   read. */
   old_ignoreused = IgnoreUsed;
   IgnoreUsed = 0;

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
         if( Match( keyname, "NAXIS%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CRVAL%d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CRPIX%d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CDELT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CROTA%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CTYPE%d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CUNIT%d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PC%3d%3d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%3d%3d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CD%1d_%1d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "LONGPOLE", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "LONPOLE%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "LATPOLE%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PROJP%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PV%d_%d%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EPOCH", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "EQUINOX%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "MJD-OBS%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "DATE-OBS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "RADECSYS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "RADESYS%0c", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dVAL%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dPIX%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dELT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dYPE%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "C%1dNIT%d", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CNPIX1", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "CNPIX2", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO3", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO6", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "XPIXELSZ", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "YPIXELSZ", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTRAH", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTRAM", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTRAS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTDECD", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTDECM", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTDECS", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTDECSN", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PLTSCALE", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO1", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO2", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO4", 0, NULL, &nfld, method, class ) ||
             Match( keyname, "PPO5", 0, NULL, &nfld, method, class ) ){

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
   IgnoreUsed = old_ignoreused;

/* Return. */
   return;
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
   if( ret >= n && astOK ) {
      astError( AST__RDERR, "%s(%s): Illegal value '%s' supplied for %s.",
                method, class, test, text );
      ret = -1;
   }

/* Return the answer. */
   return ret;
}

static int FitLinear( AstMapping *map, int ndim, double *dim, double *c,
                      double *d, double *rms ){
/*
*  Name:
*     FitLinear

*  Purpose:
*     Calculate an N-dimensional least-squares linear fit to a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     int FitLinear( AstMapping *map, int ndim, double *dim, double *c,
*                    double *d, double *rms )

*  Class Membership:
*     FitsChan

*  Description:
*     This function calculates a least squares linear fit to the supplied
*     Mapping, if possible. For an n-dimensional position x[], the fit y[] 
*     is given by:
*
*        y[0] = c[0] + d[0]*x[0] + d[1]*x[1] + ...
*        y[1] = c[1] + d[n]*x[0] + d[n+1]*x[1] + ...
*        y[2] = c[2] + d[2*n]*x[0] + d[2*n+1]*x[1] + ...
*        y[3] = ...
*
*     where y[] is an approximation to the result of transforming x[]
*     using the supplied Mapping.
*
*     A square grid of NPFIT x NPFIT test points, equally spaced over the
*     specified array dimensions, is used to calculate the fit.

*  Parameters:
*     map
*        A pointer to the Mapping. A function value of zero is returned
*        (without error) if either the number of inputs or the number of 
*        outputs is not equal to ndim.
*     ndim 
*        The number of axes.
*     dim
*        A pointer to an array holding the ndim dimensions of the data array
*        in pixels (if known). If not know, the array should be filled with
*        AST__BAD values, in which case a step size of 100 pixels will be
*        used for the grid of NPFIT x NPFIT test points.
*     c
*        A pointer to an array in which to store the constant terms. Should
*        have at least ndim elements.
*     d
*        A pointer to an array in which to store the coefficients. Should
*        have at least (ndim*ndim) elements.
*     rms
*        A pointer to a location at which to return the RMS error between
*        the actual test points and the fitted test points.

*  Returned Value:
*     Zero if no fit to the Mapping can be found. One otherwise.

*  Notes:
*     -  A value of zero is returned if an error has already occurred,
*     or if an error occurs within this function.

*/

/* Local Variables: */
   AstPointSet *pset1;            /* Pointer to the grid coords PointSet */
   AstPointSet *pset2;            /* Pointer to the phys coords PointSet */
   double **ptr1;                 /* Pointer to the grid positions */
   double **ptr2;                 /* Pointer to the physical positions */
   double *mat;                   /* Pointer to matrix */
   double *mp;                    /* Pointer to next matrix element */
   double *p1i;                   /* Pointer to first value on axis i*/
   double *p1j;                   /* Pointer to first value on axis j */
   double *p;                     /* Pointer to next position array */
   double *step;                  /* Pointer to step sizes array */
   double *vec;                   /* Pointer to known vector */
   double *x;                     /* Pointer to first axis value */
   double *z;                     /* Pointer to first residual */
   double det;                    /* Determinant of supplied matrix */
   double f;                      /* Normalization factor */
   double res;                    /* The fit residual */
   double s;                      /* Sum of logs of matrix elements */
   double v;                      /* Vector element */
   double zik;                    /* The fit value */
   int *ip;                       /* Pointer to next position index array */
   int *iw;                       /* Pointer to workspace used by slaDmat */
   int allgood;                   /* Are all mapped positions good? */
   int i;                         /* Axis index */
   int j;                         /* Axis index */
   int k;                         /* Test point index */
   int m;                         /* Axis index */
   int n;                         /* No. of values summed in s */
   int np;                        /* Total number of test points */
   int ok;                        /* Returned value */
   int sing;                      /* Zero if matrix is not singular */

/* Initialize the returned value to indsicate that no fit could be
   obtained. */
   ok = 0;

/* Check the status */
   if( !astOK ) return ok;

/* A linear fit to the Mapping can only be made if the number of inputs
   and outputs are equal. */
   if( astGetNin( map ) != ndim || astGetNout( map ) != ndim ) return ok;

/* Set up a PointSet holding a set of test points in grid coordinates. The
   first of these points is the origin of grid coordinates. First find the
   increment between test points on each axis. At the same time initialize
   the current test point, p, to the bottom left corner (1.0, 1.0, 1.0, ...) */
   step = (double *) astMalloc( ndim*sizeof( double ) );
   p = (double *) astMalloc( ndim*sizeof( double ) );
   ip = (int *) astMalloc( ndim*sizeof( int ) );
   np = 1;
   if( astOK ) {
      for( i = 0; i < ndim; i++ ){
         if( dim[ i ] != AST__BAD ) {
            step[ i ] = dim[ i ]/NPFIT;
         } else {
            step[ i ] = 100.0;
         }
         np *= NPFIT;
         p[ i ] = 1.0;
         ip[ i ] = 0;
      }
   }

/* Add one extra for the origin. */
   np++;

/* Now create the PointSet to hold the test grid coordinates. */
   pset1 = astPointSet( np, ndim, "" );
   ptr1 = astGetPoints( pset1 );

/* Check the pointer can be used safely. */
   if( astOK ) {

/* Store the origin. */
      for( j = 0; j < ndim; j++ ){
         ptr1[ j ][ 0 ] = 0.0;
      }

/* Store the test point grid coords in the PointSet. Loop round each
   point. */
      for( k = 1; k < np; k++ ){            

/* Store the current point in the PointSet. */
         for( j = 0; j < ndim; j++ ){
            ptr1[ j ][ k ] = p[ j ];
         }

/* Increment the axis 0 vaue at the current point. */
         j = 0;
         p[ j ] += step[ j ];
         ip[ j ]++;

/* If we have stepped beyond the last test point on this axis, reset the axis 
   value to 1.0, and move on to increment the next higher axis. */
         while( ip[ j ] == NPFIT ){
            p[ j ] = 1.0;
            ip[ j ] = 0;
            if( ++j < ndim ) {
               p[ j ] += step[ j ];
               ip[ j ]++;
            } else {
               break;         
            }
         }
      }

/* Transform the test points using the supplied Mapping. */
      pset2 = astTransform( map, pset1, 1, NULL );
      ptr2 = astGetPoints( pset2 );

/* Check the results array can be used. */
      if( astOK ){

/* The constant terms of the returned linear transformation are just
   equal to the transformed origin. Store them in the returned array and
   subtract them from all the transformed test points. Also check for any
   bad transformed values. */
         allgood = 1;
         for( i = 0; i < ndim; i++ ){
            c[ i ] = ptr2[ i ][ 0 ];
            for( k = 0; k < np; k++ ){
               if( ptr2[ i ][ k ] != AST__BAD ) {
                  ptr2[ i ][ k ] -= c[ i ];
               } else {
                  allgood = 0;
                  break;
               }
            }
         }

/* Create the matrix holding the coefficients of the normal equations. 
   First allocate the memory for the matrix (and also the known vector -
   used later) and check the pointers can be used. Abort if any bad
   values were found above. */
         mat = (double *) astMalloc( ndim*ndim*sizeof(double) );
         vec = (double *) astMalloc( ndim*sizeof(double) );
         if( astOK && allgood ) {

/* Element (Row=i,Column=j) of the matrix holds the sum over all the test
   points, of the product of i'th and j'th grid axis value. The matrix is
   symetric since Xi*Xj=Xj*Xi, so we calculate the upper right half of the 
   matrix explicitly, and copy the values into the lower left half. First,
   initialize the pointer to the next matrix element. */
            mp = mat;

/* Loop round each row of the matrix. */
            for( i = 0; i < ndim; i++ ){

/* Get a pointer to the first test value for this (i.e. the i'th) axis. The
   other values for this axis follow on after the first. */
               p1i = ptr1[ i ];

/* The elements to the left of the diagonal are simply copied from the
   corresponding right-hand elements of earlier rows. */
               for( j = 0; j < i; j++ ){
                  *(mp++) = mat[ i + j*ndim ];
               }

/* The remaining elements of this row are calulated explicitly. Loop
   through each column. */
               for( j = i; j < ndim; j++ ){

/* Get a pointer to the first test value for this (i.e. the j'th) axis. The
   other values for this axis follow on after the first. */
                  p1j = ptr1[ j ];

/* Initialize the current matrix element to zero. */
                  *mp = 0.0;

/* Go round each test point (except the first which is the origin), 
   incrementing the current matrix element by the product of the i'th and 
   j'th coordinate value at the test point. */
                  for( k = 1; k < np; k++ ) {
                     *mp += p1i[ k ]*p1j[ k ];
                  }

/* Move on to the next matrix element. */
                  mp++;
               }
            }

/* The matrix elements and the known vectors are normalized by dividing
   them by the geometric mean of the unnormalized matrix elements. Find
   the normalization factor, and normlize the matrix elements. */
            s = 0.0;
            n = 0;
            for( i = 0; i < ndim*ndim; i++ ) {
               if( mat[ i ] != 0.0 ) {
                  s += log( fabs( mat[ i ] ) );
                  n++;
               }
            }
            if( n > 0 ) {
               f = 1.0/exp( s/( (double) n ) );
               for( i = 0; i < ndim*ndim; i++ ) mat[ i ] *= f;
            } else {
               f = 1.0;
            }

/* For axis 0, we invert the above matrix, finding the required
   transformation coefficients in the process. For subsequent axes, 
   we use the inverted matrix directly. First find the known vector 
   for axis 0, normalizing it using the above normalization factor. */
            for( m = 0; m < ndim; m++ ) {
               z = ptr2[ 0 ];
               x = ptr1[ m ];
               v = 0.0;
               for( k = 1; k < np; k++ ) {
                  v += z[ k ]*x[ k ];
               }
               vec[ m ] = v*f;
            }

/* Invert the normal equations matrix. */
            iw = (int *) astMalloc( sizeof(int)*(size_t) ndim );
            if( astOK ) slaDmat( ndim, mat, vec, &det, &sing, iw );
            iw = (int *) astFree( (void *) iw ); 

/* Check the matrix could be inverted. */
            if( astOK && sing == 0 ) {
               ok = 1;

/* Store the resulting coefficients in the returned array. */
               for( i = 0; i < ndim; i++ ) d[ i ] = vec[ i ]; 

/* Loop round each other axis. */
               for( i = 1; i < ndim; i++ ) {

/* Form the known vector for this axis, normalizing it using the above 
   normalization factor. */
                  for( m = 0; m < ndim; m++ ) {
                     z = ptr2[ i ];
                     x = ptr1[ m ];
                     v = 0.0;
                     for( k = 1; k < np; k++ ) {
                        v += z[ k ]*x[ k ];
                     }
                     vec[ m ] = v*f;
                  }

/* Apply the inverted normal equations matrix to the known vector, and
   store the resulting coefficients in the returned array. */
                  mp = mat;
                  for( j = 0; j < ndim; j++ ) {
                     v = 0.0;
                     for( m = 0; m < ndim; m++ ) {
                        v += ( *(mp++) )*vec[ m ];
                     }
                     d[ i*ndim + j ] = v;
                  }
               }

/* We now have all the fit coefficients. Use them to find the fitted value
   (without the constant term) at each test point. Then find the residuals
   between the fitted value and the actual value, and form the sum of the
   squared residuals, summed over all test points. Return the rms error
   of the fit. */
               *rms = 0.0;
               for( k = 1; k < np; k++ ) {
                  for( j = 0; j < ndim; j++ ) {
                     p[ j ] = ptr1[ j ][ k ];
                  }


                  for( i = 0; i < ndim; i++ ) {

                     zik = 0;
                     for( j = 0; j < ndim; j++ ) {
                        zik += p[ j ]*d[ i*ndim + j ];
                     }

                     step[ i ] = zik;
                     res = ptr2[ i ][ k ] - zik ;
                     *rms += res*res;
                  }
               }

               *rms = sqrt( (*rms) / ( np - 1 ) );

            }
         }

/*  Free the memory holding the matrix and vector. */
         mat = (double *) astFree( (void *) mat );
         vec = (double *) astFree( (void *) vec );

      }
   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Free other memory */
   p = (double *) astFree( (void *) p );
   ip = (int *) astFree( (void *) ip );
   step = (double *) astFree( (void *) step );

/* Return the success flag. */
   return ok;

}

static int FitsFromStore( AstFitsChan *this, FitsStore *store, int encoding, 
                          int axlat, int axlon, const char *method, 
                          const char *class ){
/*
*  Name:
*     FitsFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     int FitsFromStore( AstFitsChan *this, FitsStore *store, int encoding, 
*                        int axlat, int axlon, const char *method, 
*                        const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using a specified encoding.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     encoding
*        The encoding to use.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   int ret;

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Set the current card so that it points to the last WCS-related keyword
   in the FitsChan (whether previously read or not). Any new WCS related
   keywords either over-write pre-existing cards for the same keyword, or
   (if no pre-existing card exists) are inserted after the last WCS related
   keyword. */
   FindWcs( this, method, class );

/* Do each non-standard FITS encoding... */
   if( encoding == DSS_ENCODING ){
      ret = DSSFromStore( this, store, axlat, axlon, method, class );

   } else if( encoding == FITSPC_ENCODING ){
      ret = PCFromStore( this, store, axlat, axlon, method, class );

   } else if( encoding == FITSIRAF_ENCODING ){
      ret = IRAFFromStore( this, store, axlat, axlon, method, class );

   } else if( encoding == FITSAIPS_ENCODING ){
      ret = AIPSFromStore( this, store, axlat, axlon, method, class );

/* Standard FITS-WCS encoding */
   } else {
      ret = WCSFromStore( this, store, axlat, axlon, method, class );

   }

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static FitsStore *FitsToStore( AstFitsChan *this, int encoding,
                               const char *method, const char *class ){
/*
*  Name:
*     FitsToStore

*  Purpose:
*     Return a pointer to a FitsStore structure containin WCS information
*     read from the supplied FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     FitsStore *FitsToStore( AstFitsChan *this, int encoding,
*                             const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function creates a new FitsStore containing WCS information
*     read from the supplied FitsChan using the specified encoding. An
*     error is reported and a null pointer returned if the FitsChan does 
*     not contain usable WCS information with the specified encoding.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     encoding
*        The encoding to use.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A pointer to a new FitsStore, or NULL if an error has occurred. The 
*     FitsStore should be released using FreeStore function when it is no 
*     longer needed.

*/

/* Local Variables: */
   AstFitsChan *trans;
   FitsStore *ret;

/* Initialise */
   ret = NULL;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Allocate memory for the new FitsStore, and store NULL pointers in it. */
   ret = (FitsStore *) astMalloc( sizeof(FitsStore) );
   if( ret ) {
      ret->ctype = NULL;
      ret->ctype_com = NULL;
      ret->cunit = NULL;
      ret->radesys = NULL;
      ret->wcsname = NULL;
      ret->cd = NULL;
      ret->crpix = NULL;
      ret->crval = NULL;
      ret->equinox = NULL;
      ret->latpole = NULL;
      ret->lonpole = NULL;
      ret->mjdobs = NULL;
      ret->pv = NULL;
   }

/* Call the routine apropriate to the encoding. */
   if( encoding == DSS_ENCODING ){
      DSSToStore( this, ret, method, class );

/* All other foreign encodings are treated as variants of FITS-WCS. */
   } else {

/* Create a new FitsChan containing standard translations for any
   non-standard keywords in the supplied FitsChan. The non-standard
   keywords are marked as provisionally read in the supplied FitsChan. */
      trans = SpecTrans( this, encoding, method, class );

/* Copy the required values to the FitsStore, using keywords in "trans"
   in preference to those in "this". */
      WCSToStore( this, trans, ret, method, class );

/* Delete the temporary FitsChan holding translations of non-standard
   keywords. */
      if( trans ) trans = (AstFitsChan *) astDelete( trans );
   }

/* If an error has occurred, free the returned FitsStore, and return a null 
   pointer. */
   if( !astOK ) ret = FreeStore( ret );

/* Return the answer. */
   return ret;

}

static void FreeItem( double ****item ){
/*
*  Name:
*     FreeItem

*  Purpose:
*     Frees all dynamically allocated memory associated with a specified
*     item in a FitsStore.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FreeItem( double ****item );

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Frees all dynamically allocated memory associated with the specified
*     item in a FitsStore. A NULL pointer is stored in the FitsStore.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates an
*        array of axis keyword values. These arrays of keyword values have 
*        one element for every pixel axis (i) or projection parameter (m). 

*  Notes:
*    - This function attempt to execute even if an error has occurred.

*/

/* Local Variables: */
   int si;               /* Integer co-ordinate version index */
   int j;                /* Intermediate co-ordinate axis index */

/* Check the supplied pointer */
   if( item && *item ){

/* Loop round each coordinate version. */
      for( si = 0; si < astSizeOf( (void *) *item )/sizeof(double **);
           si++ ){

/* Check the pointer stored for this co-ordinate version is not null. */
         if( (*item)[si] ) {

/* Loop round the intermediate axes. */
            for( j = 0; j < astSizeOf( (void *) (*item)[si] )/sizeof(double *);
                 j++ ){

/* Free the pixel axis/parameter index pointer. */
               (*item)[si][j] = (double *) astFree( (void *) (*item)[si][j] );
            }

/* Free the intermediate axes pointer */
            (*item)[si] = (double **) astFree( (void *) (*item)[si] );
         }
      }

/* Free the co-ordinate versions pointer */
      *item = (double ***) astFree( (void *) *item );

   }

}

static void FreeItemC( char ****item ){
/*
*  Name:
*     FreeItemC

*  Purpose:
*     Frees all dynamically allocated memory associated with a specified
*     string item in a FitsStore.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FreeItemC( char ****item );

*  Class Membership:
*     FitsChan member function.

*  Description:
*     Frees all dynamically allocated memory associated with the specified
*     string item in a FitsStore. A NULL pointer is stored in the FitsStore.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates a
*        character string. 

*  Notes:
*    - This function attempt to execute even if an error has occurred.

*/

/* Local Variables: */
   int si;               /* Integer co-ordinate version index */
   int j;                /* Intermediate co-ordinate axis index */

/* Check the supplied pointer */
   if( item && *item ){

/* Loop round each coordinate version. */
      for( si = 0; si < astSizeOf( (void *) *item )/sizeof(char **);
           si++ ){

/* Check the pointer stored for this co-ordinate version is not null. */
         if( (*item)[si] ) {

/* Loop round the intermediate axes. */
            for( j = 0; j < astSizeOf( (void *) (*item)[si] )/sizeof(char *);
                 j++ ){

/* Free the character string pointer. */
               (*item)[si][j] = (char *) astFree( (void *) (*item)[si][j] );
            }

/* Free the intermediate axes pointer */
            (*item)[si] = (char **) astFree( (void *) (*item)[si] );
         }
      }

/* Free the co-ordinate versions pointer */
      *item = (char ***) astFree( (void *) *item );

   }

}

static FitsStore *FreeStore( FitsStore *store ){
/*
*  Name:
*     FreeStore

*  Purpose:
*     Free dynamic arrays stored in a FitsStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     FitsStore *FreeStore( FitsStore *store )

*  Class Membership:
*     FitsChan

*  Description:
*     This function frees all dynamically allocated arrays stored in the
*     supplied FitsStore structure, and returns a NULL pointer.

*  Parameters:
*     store
*        Pointer to the structure to clean.

*  Notes:
*     - This function attempts to execute even if an error exists on entry.

*/

/* Return if no FitsStore was supplied. */
   if( !store ) return NULL;

/* Free each of the dynamic arrays stored in the FitsStore. */
   FreeItemC( &(store->ctype) );
   FreeItemC( &(store->ctype_com) );
   FreeItemC( &(store->cunit) );
   FreeItemC( &(store->radesys) );
   FreeItemC( &(store->wcsname) );

   FreeItem( &(store->cd) );
   FreeItem( &(store->crpix) );
   FreeItem( &(store->crval) );
   FreeItem( &(store->equinox) );
   FreeItem( &(store->latpole) );
   FreeItem( &(store->lonpole) );
   FreeItem( &(store->mjdobs) );
   FreeItem( &(store->pv) );

   return (FitsStore *) astFree( (void *) store );
}

static char *FormatKey( char *key, int c1, int c2, char s ){
/*
*  Name:
*     FormatKey

*  Purpose:
*     Format a keyword name with indices and co-ordinate version character.

*  Type:
*     Private function.

*  Synopsis:
*     char *FormatKey( char *key, int c1, int c2, char s )

*  Class Membership:
*     FitsChan

*  Description:
*     This function formats a keyword name by including the supplied
*     axis/parameter indices and co-ordinate version character.

*  Parameters:
*     key
*        The base name of the keyword (e.g. "CD", "CRVAL", etc).
*     c1
*        An integer value to append to the end of the keyword. Ignored if
*        less than zero.
*     c2
*        A second integer value to append to the end of the keyword. Ignored if
*        less than zero. This second integer is preceeded by an underscore.
*     s
*        The co-ordinate version character to append to the end of the
*        final string. Ignored if blank.

*  Returned Value;
*     A pointer to a static character buffer containing the final string.
*     NULL if an error occurs.

*/

/* Local Variables: */
   static char buff[10];
   char *ret;
   int len;
   int nc; 

/* Initialise */
   ret = NULL;

/* Check inherited status */
   if( !astOK ) return ret;   

/* No characters stored yet. A value of -1 is used to indicate that an 
   error has occurred. */
   len = 0;

/* Store the supplied keyword base name. */
   if( len >= 0 && ( nc = sprintf( buff + len, "%s", key ) ) >= 0 ){
      len += nc;
   } else {
      len = -1;
   }

/* If index c1 has been supplied, append it to the end of the string. */
   if( c1 >= 0 ) {
      if( len >= 0 && ( nc = sprintf( buff + len, "%d", c1 ) ) >= 0 ){
         len += nc;
      } else {
         len = -1;
      }

/* If index c2 has been supplied, append it to the end of the string,
   preceeded by an underscore. */
      if( c2 >= 0 ) {
         if( len >= 0 && ( nc = sprintf( buff + len, "_%d", c2 ) ) >= 0 ){
            len += nc;
         } else {
            len = -1;
         }
      }
   }

/* If a co-ordinate version character has been supplied, append it to the end 
   of the string. */
   if( s != ' ' ) {
      if( len >= 0 && ( nc = sprintf( buff + len, "%c", s ) ) >= 0 ){
         len += nc;
      } else {
         len = -1;
      }
   }

/* Report an error if necessary */
   if( len < 0 && astOK ) {
      astError( AST__INTER, "FormatKey(fitschan): AST internal error; failed "
                "to format the keyword %s with indices %d and %d, and "
                "co-ordinate version %c.", key, c1, c2, s );
      ret = NULL;

   } else {
      ret = buff;
   }

   return buff;
}

static AstObject *FsetFromStore( AstFitsChan *this, FitsStore *store, 
                                 const char *method, const char *class ){
/*
*  Name:
*     FsetFromStore

*  Purpose:
*     Create a FrameSet using the the information previously stored in
*     the suppllied FitsStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     AstObject *FsetFromStore( AstFitsChan *this, FitsStore *store, 
*                               const char *method, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function creates a new FrameSet containing WCS information
*     stored in the supplied FitsStore. A null pointer is returned and no
*     error is reported if this is not possible.

*  Parameters:
*     this
*        The FitsChan from which the keywords were read. Warning messages
*        are added to this FitsChan if the celestial co-ordinate system is 
*        not recognized. 
*     store
*        Pointer to the FitsStore.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A pointer to the new FrameSet, or a null pointer if no FrameSet
*     could be constructed.

*  Notes:
*     -  The pixel Frame is given a title of "Pixel Coordinates", and
*     each axis in the pixel Frame is given a label of the form "Pixel
*     axis <n>", where <n> is the axis index (starting at one).
*     -  The FITS CTYPE keyword values are used to set the labels for any
*     non-celestial axes in the physical coordinate Frames, and the FITS 
*     CUNIT keywords are used to set the corresponding units strings.
*     -  On exit, the pixel Frame is the base Frame, and the physical
*     Frame derived from the primary axis descriptions is the current Frame.
*     - Extra Frames are added to hold any secondary axis descriptions. All
*     axes within such a Frame refer to the same coordinate version ('A',
*     'B', etc).

*/

/* Local Variables: */
   AstFrame *frame;   /* Pointer to pixel Frame */
   AstFrameSet *ret;  /* Pointer to returned FrameSet */
   char buff[ 20 ];   /* Buffer for axis label */
   char s;            /* Co-ordinate version character */
   int i;             /* Pixel axis index */
   int physical;      /* Index of primary physical co-ordinate Frame */
   int pixel;         /* Index of pixel Frame in returned FrameSet */
   int use;           /* Has this co-ordinate version been used? */

/* Initialise */
   ret = NULL;

/* Check the inherited status. */
   if( !astOK ) return (AstObject *) ret;

/* Get the number of pixel axes. Only proceed if not zero, */
   store->naxis = GetMaxIM( &(store->crpix) ) + 1;

/* Only proceed if there are some axes. */
   if( store->naxis ) {

/* Create a Frame describing the pixel coordinate system. Give it the Domain 
   GRID. */
      frame = astFrame( store->naxis, "Title=Pixel Coordinates,Domain=GRID" );

/* Store labels for each pixel axis. */
      if( astOK ){
         for( i = 0; i < store->naxis; i++ ){
            sprintf( buff, "Pixel axis %d", i + 1 );
            astSetLabel( frame, i, buff );
         }
      }

/* Create the FrameSet initially holding just the pixel coordinate frame
   (this becomes the base Frame). */
      ret = astFrameSet( frame, "" );

/* Annul the pointer to the pixel coordinate Frame. */
      frame = astAnnul( frame );

/* Get the index of the pixel Frame in the FrameSet. */
      pixel = astGetCurrent( ret );

/* Produce the Frame describing the primary axis descriptions, and add it
   into the FrameSet. */
      AddFrame( this, ret, pixel, store, ' ', method, class );  

/* Get the index of the primary physical co-ordinate Frame in the FrameSet. */
      physical = astGetCurrent( ret );

/* Loop, producing secondary axis Frames for each of the co-ordinate 
   versions stored in the FitsStore. */
      for( s = 'A'; s <= GetMaxS( &(store->crval) ) && astOK; s++ ){      

/* Only use this co-ordnate version character if any of the required
   keywords (for any axis) are stored in the FitsStore. */
         use = 0;
         for( i = 0; i < store->naxis; i++ ){
            if( GetItem( &(store->crval), i, 0, s, NULL, method, class ) != AST__BAD ||
                GetItem( &(store->crpix), 0, i, s, NULL, method, class ) != AST__BAD ||
                GetItemC( &(store->ctype), i, s, NULL, method, class ) != NULL ){
               use = 1;
               break;
            }
         }

/* If this co-ordinate version has been used, add a Frame to the returned
   FrameSet holding this co-ordinate version. */
         if( use ) AddFrame( this, ret, pixel, store, s, method, class );  

      }

/* Ensure the pixel Frame is the Base Frame and the primary physical
   Frame is the Current Frame. */
      astSetBase( ret, pixel );
      astSetCurrent( ret, physical );
   }

/* If an error has occurred, free the returned FrameSet and return a null 
   pointer. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the answer. */
   return (AstObject *) ret;

}

static FitsStore *FsetToStore( AstFrameSet *fset, int naxis, double *dim, 
                               int *axlat, int *axlon, const char *class, 
                               const char *method ){
/*
*  Name:
*     FsetToStore

*  Purpose:
*     Fill a FitsStore structure with a description of the supplied
*     FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     FitsStore *FsetToStore( AstFrameSet *fset, int naxis, double *dim, 
*                             const char *class, const char *method )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function creates a new FitsStore containing WCS information
*     read from the supplied FitsChan using the specified encoding. An
*     error is reported and a null pointer returned if the FitsChan does 
*     not contain usable WCS information with the specified encoding.

*  Parameters:
*     fset
*        Pointer to the FrameSet.
*     naxis
*        The number of axes in the Base Frame of the supplied FrameSet.
*     dim 
*        Pointer to an array of pixel axis dimensions. Individual elements 
*        will be AST__BAD if dimensions are not known.
*     axlat
*        A pointer to a location at which to return the index of the
*        primary celestial latitude axis. -1 is returned if there is no 
*        celestial latitude axis.
*     axlon
*        A pointer to a location at which to return the index of the
*        primary celestial longitude axis. -1 is returned if there is no 
*        celestial longitude axis.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A pointer to a new FitsStore, or NULL if an error has occurred. The 
*     FitsStore should be released using FreeStore function when it is no 
*     longer needed.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.
*     - The Base Frame in the FrameSet is used as the pixel Frame, and
*     the Current Frame is used to create the primary axis descriptions.
*     Attempts are made to create secondary axis descriptions for any 
*     other Frames in the FrameSet (up to a total of 26).
*/

/* Local Variables: */
   AstFrame *frame;     /* A Frame */
   const char *id;      /* Frame Ident string */
   int nfrm;            /* Number of Frames in FrameSet */
   char *sid;           /* Pointer to array of version letters */
   int frms[ 'Z' + 1 ]; /* Array of Frame indices */
   FitsStore *ret;      /* Returned FitsStore */
   char s;              /* Co-ordinate version character */
   int ibase;           /* Base Frame index */
   int icurr;           /* Current Frame index */
   int ifrm;            /* Next Frame index */
   int primok;          /* Primary Frame stored succesfully? */
   int secok;           /* Secondary Frame stored succesfully? */

/* Initialise */
   ret = NULL;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Allocate memory for the new FitsStore, and store NULL pointers in it. */
   ret = (FitsStore *) astMalloc( sizeof(FitsStore) );
   if( astOK ) {
      ret->ctype = NULL;
      ret->ctype_com = NULL;
      ret->cunit = NULL;
      ret->radesys = NULL;
      ret->wcsname = NULL;
      ret->cd = NULL;
      ret->crpix = NULL;
      ret->crval = NULL;
      ret->equinox = NULL;
      ret->latpole = NULL;
      ret->lonpole = NULL;
      ret->mjdobs = NULL;
      ret->pv = NULL;

/* Obtain the index of the Base Frame (i.e. the pixel frame ). */
      ibase = astGetBase( fset );

/* Obtain the index of the Current Frame (i.e. the Frame to use as the 
   primary physical coordinate frame). */
      icurr = astGetCurrent( fset );

/* Add a description of the primary axes to the FitsStore, based on the
   Current Frame in the FrameSet. */
      primok = AddVersion( fset, ibase, icurr, ret, naxis, dim, axlat, 
                           axlon, ' ', method, class );

/* Do not add any alternate axis descriptions if the primary axis
   descriptions could not be produced. */
      if( primok && astOK ) {

/* Get the number of Frames in the FrameSet. */
         nfrm = astGetNframe( fset );

/* We now need to allocate a version letter to each Frame. Allocate
   memory to hold the version letter assigned to each Frame. */
         sid = (char *) astMalloc( ( nfrm + 1 )*sizeof( char ) );

/* The frms array has an entry for each of the 26 possible version
   letters (starting at A and ending at Z). Each entry holds the index of 
   the Frame which has been assigned that version character. Initialise
   this array to indicate that no version letters have yet been assigned. */
         for( s = 'A'; s <= 'Z'; s++ ) {
            frms[ s ] = 0;
         }

/* Loop round all frames (excluding the current and base Frames which do not 
   need version letters). If the Frame has an Ident attribute consisting of a
   single upper case letter, use it as its version letter unless that
   letter has already been given to an earlier frame. */
         for( ifrm = 1; ifrm <= nfrm; ifrm++ ){
            sid[ ifrm ] = 0;
            if( ifrm != icurr && ifrm != ibase ) { 
               frame = astGetFrame( fset, ifrm );
               id = astGetIdent( frame );
               if( strlen( id ) == 1 && isupper( id[ 0 ] ) ) {
                  if( frms[ id[ 0 ] ] == 0 ) {
                     frms[ id[ 0 ] ] = ifrm;
                     sid[ ifrm ] = id[ 0 ];
                  }
               }
               astAnnul( frame );
            }
         }

/* Now go round all the Frames again, looking for Frames which did not
   get a version letter assigned to it on the previous loop. Assign them
   letters now, selected them from the letters not already assigned
   (lowest to highest). */
         s = 'A' - 1;
         for( ifrm = 1; ifrm <= nfrm; ifrm++ ){
            if( sid[ ifrm ] == 0 ){
               if( ifrm != icurr && ifrm != ibase ) { 
                  while( frms[ ++s ] != 0 );
                  if( s <= 'Z' ) { 
                     sid[ ifrm ] = s;
                     frms[ s ] = ifrm;
                  }
               }
            }
         }

/* Now go through all the other Frames in the FrameSet, attempting to
   create alternate axis descriptions for each one. */
         for( ifrm = 1; ifrm <= nfrm; ifrm++ ){
            s = sid[ ifrm ];
            if( s != 0 ) {
               secok = AddVersion( fset, ibase, ifrm, ret, naxis, dim, NULL,
                                   NULL, s, method, class );
            }
         }

/* Free memory holding version letters */
         sid = (char *) astFree( (void *) sid );

      }

/* If an error has occurred, or if the primary Frame could not be cerated, 
   free the returned FitsStore, and return a null pointer. */
      if( !astOK || !primok ) ret = FreeStore( ret );
   }

/* Return the answer. */
   return ret;

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
*     looking for selected keywords within the FitsChan. Checks are made
*     for the following keywords in the order specified, and the
*     corresponding encoding is adopted when the first one is found ( where
*     i, j and m are integers and s is a single upper case character):
*
*     1) Any keywords starting with "BEGAST" = Native encoding 
*     2) Any keywords matching PCiiijjj = FITS-PC encoding
*     3) Any keywords matching CDiiijjj = FITS-IRAF encoding
*     4) Any keywords matching CDi_j, AND at least one of RADECSYS, PROJPi
*        or CmVALi = FITS-IRAF encoding
*     5) Any keywords RADECSYS, PROJPi or CmVALi = FITS-PC encoding
*     6) Any keywords matching CROTAi = FITS-AIPS encoding
*     7) Any keywords matching CDELTi = FITS-PC encoding
*     8) Keywords matching CRVALi = FITS-WCS encoding
*     9) The PLTRAH keyword = DSS encoding
*     10) If none of the above keywords are found, Native encoding is assumed.

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

/* Otherwise, if the FitsChan contains any keywords with the format 
   "PCiiijjj" then return "FITS-PC" encoding. */
      } else if( astKeyFields( this, "PC%3d%3d", 0, NULL, NULL ) ){
         ret = FITSPC_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   "CDiiijjj" then return "FITS-IRAF" encoding. */
      } else if( astKeyFields( this, "CD%3d%3d", 0, NULL, NULL ) ){
         ret = FITSIRAF_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   "CDi_j"  AND there is a RADECSYS. PROJPi or CmVALi keyword, then return 
   "FITS-IRAF" encoding. */
      } else if( astKeyFields( this, "CD%1d_%1d", 0, NULL, NULL ) 
                 && ( astKeyFields( this, "RADECSYS", 0, NULL, NULL ) ||
                      astKeyFields( this, "PROJP%d", 0, NULL, NULL ) ||
                      astKeyFields( this, "C%1dVAL%d", 0, NULL, NULL ) ) ){
         ret = FITSIRAF_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   RADECSYS. PROJPi or CmVALi keyword, then return "FITS-PC" encoding. */
      } else if( astKeyFields( this, "RADECSYS", 0, NULL, NULL ) ||
                 astKeyFields( this, "PROJP%d", 0, NULL, NULL ) ||
                 astKeyFields( this, "C%1dVAL%d", 0, NULL, NULL ) ) {
         ret = FITSPC_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   "CROTAi" then return "FITS-AIPS" encoding. */
      } else if( astKeyFields( this, "CROTA%d", 0, NULL, NULL ) ){
         ret = FITSAIPS_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   "CDELTi" then return "FITS-PC" encoding. */
      } else if( astKeyFields( this, "CDELT%d", 0, NULL, NULL ) ){
         ret = FITSPC_ENCODING;

/* Otherwise, if the FitsChan contains any keywords with the format 
   "CRVALi" then return "FITS-WCS" encoding. */
      } else if( astKeyFields( this, "CRVAL%d", 0, NULL, NULL ) ){
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

static double GetItem( double ****item, int j, int im, char s, char *name,
                       const char *method, const char *class ){
/*
*  Name:
*     GetItem

*  Purpose:
*     Retrieve a value for a axis keyword value from a FitStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     double GetItem( double ****item, int j, int im, char s, char *name,
*                     const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The requested keyword value is retrieved from the specified array,
*     at a position indicated by the axis and co-ordinate version.
*     AST__BAD is returned if the array does not contain the requested
*     value. 

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates an
*        array of axis keyword values. These arrays of keyword values have 
*        one element for every pixel axis (i) or projection parameter (m). 
*     j
*        The zero based intermediate axis index in the range 0 to 98. Set 
*        this to zero for keywords (e.g. CRPIX) which are not indexed by 
*        intermediate axis number.
*     im
*        The zero based pixel axis index (in the range 0 to 98) or parameter 
*        index (in the range 0 to 99). Set this to zero for keywords (e.g. 
*        CRVAL) which are not indexed by either pixel axis or parameter 
*        number.
*     s
*        The co-ordinate version character (A to Z, or space), case
*        insensitive
*     name 
*        A string holding a name for the item of information. A NULL
*        pointer may be supplied, in which case it is ignored. If a
*        non-NULL pointer is supplied, an error is reported if the item
*        of information has not been stored, and the supplied name is
*        used to identify the information within the error message.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.


*  Returned Value:
*     The required keyword value, or AST__BAD if no value has previously
*     been stored for the keyword (or if an error has occurred).

*/

/* Local Variables: */
   double ret;           /* Returned keyword value */
   int si;               /* Integer co-ordinate version index */

/* Initialise */
   ret = AST__BAD;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Convert the character co-ordinate version into an integer index, and
   check it is within range. The primary axis description (s=' ') is
   given index zero. 'A' is 1, 'B' is 2, etc. */
   if( s == ' ' ) {
      si = 0;
   } else if( islower(s) ){
      si = (int) ( s - 'a' ) + 1;
   } else {
      si = (int) ( s - 'A' ) + 1;
   }

   if( si < 0 || si > 26 ) {
      astError( AST__INTER, "GetItem(fitschan): AST internal error; "
                "co-ordinate version '%c' ( char(%d) ) is invalid.", s, s );

/* Check the intermediate axis index is within range. */
   } else if( j < 0 || j > 98 ) {
      astError( AST__INTER, "GetItem(fitschan): AST internal error; "
                "intermediate axis index %d is invalid.", j );

/* Check the pixel axis or parameter index is within range. */
   } else if( im < 0 || im > 99 ) {
      astError( AST__INTER, "GetItem(fitschan): AST internal error; "
                "pixel axis or parameter index %d is invalid.", j );

/* Otherwise, if the array holding the required keyword is not null, 
   proceed... */
   } else if( *item ){

/* Find the number of coordinate versions in the supplied array.
   Only proceed if it encompasses the requested co-ordinate
   version. */
      if( astSizeOf( (void *) *item )/sizeof(double **) > si ){

/* Find the number of intermediate axes in the supplied array.
   Only proceed if it encompasses the requested intermediate axis. */
         if( astSizeOf( (void *) (*item)[si] )/sizeof(double *) > j ){

/* Find the number of pixel axes or parameters in the supplied array.
   Only proceed if it encompasses the requested index. */
            if( astSizeOf( (void *) (*item)[si][j] )/sizeof(double) > im ){

/* Return the required keyword value. */
               ret = (*item)[si][j][im];
            }
         }
      }
   }

/* If required, report an error if the requested item of information has
   not been stored. */
   if( ret == AST__BAD && name && astOK ){
      astError( AST__NOFTS, "%s(%s): No value can be found for %s.",
                method, class, name );
   }

   return ret;

}

static int GetMaxIM( double ****item ){
/*
*  Name:
*     GetMaxIM

*  Purpose:
*     Return the largest pixel axis or parameter index stored for an axis 
*     keyword value in a FitStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GetMaxIM( double ****item )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The number of pixel axis numbers or projection parameters stored for
*     a specified primary axis keyword is found and returned.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates an
*        array of axis keyword values. These arrays of keyword values have 
*        one element for every pixel axis (i) or projection parameter (m). 

*  Returned Value:
*     The maximum pixel axis number or projection parameter index (zero 
*     based).

*/

/* Local Variables: */
   int im;               /* Number of parameters/pixel axes */
   int j;                /* Intermediate axis index */
   int ret;              /* Returned axis index */

/* Initialise */
   ret = -1;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* If the array holding the required keyword is not null, proceed... */
   if( *item ){

/* Check that the pointer to the array of primary intermediate axis values 
   is not null. */
      if( (*item)[0] ){

/* Loop round each used element in this array. */
         for( j = 0; j < astSizeOf( (void *) (*item)[0] )/sizeof(double *);
              j++ ){
            if( (*item)[0][j] ){

/* Get the size of the pixel axis/projection parameter array for the
   current intermediate axis, and subtract 1 to get the largest index. */
               im = astSizeOf( (void *) (*item)[0][j] )/sizeof(double) - 1;

/* Ignore any trailing unused (AST__BAD) values. */
               while( im >= 0 && (*item)[0][j][im] == AST__BAD ) im--;

/* Update the returned value if the current value is larger. */
               if( im > ret ) ret = im;

            }
         }
      }
   }

   return ret;

}

static char GetMaxS( double ****item ){
/*
*  Name:
*     GetMaxS

*  Purpose:
*     Return the largest (i.e. closest to Z) coordinate version character 
*     stored for a axis keyword value in a FitStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     char GetMaxS( double ****item )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The largest (i.e. closest to Z) coordinate version character 
*     stored for a axis keyword value in a FitStore structure is found
*     and returned.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates an
*        array of axis keyword values. These arrays of keyword values have 
*        one element for every pixel axis (i) or projection parameter (m). 

*  Returned Value:
*     The highest coordinate version character.

*/

/* Local Variables: */
   char ret;              /* Returned axis index */
   int si;                /* Integer index into alphabet */

/* Initialise */
   ret = ' ';

/* Check the inherited status. */
   if( !astOK ) return ret;

/* If the array holding the required keyword is not null, proceed... */
   if( *item ){

/* Find the length of this array, and subtract 1 to get the largest index
   in the array. */
      si = astSizeOf( (void *) *item )/sizeof(double **) - 1;

/* Ignore any trailing null (i.e. unused) values. */
      while( si >= 0 && !(*item)[si] ) si--;

/* Store the corresponding character */
      ret = 'A' + si;
   }

   return ret;

}

static char *GetItemC( char ****item, int j, char s, char *name,
                       const char *method, const char *class  ){
/*
*  Name:
*     GetItemC

*  Purpose:
*     Retrieve a string value for a axis keyword value from a FitStore 
*     structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     char *GetItemC( char ****item, int j, char s, char *name,
*                     const char *method, const char *class  )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The requested keyword string value is retrieved from the specified 
*     array, at a position indicated by the axis and co-ordinate version.
*     NULL is returned if the array does not contain the requested
*     value. 

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates a
*        character string. 
*     j
*        The zero based intermediate axis index in the range 0 to 98. Set 
*        this to zero for keywords (e.g. CRPIX) which are not indexed by 
*        intermediate axis number.
*     s
*        The co-ordinate version character (A to Z, or space), case
*        insensitive
*     name 
*        A string holding a name for the item of information. A NULL
*        pointer may be supplied, in which case it is ignored. If a
*        non-NULL pointer is supplied, an error is reported if the item
*        of information has not been stored, and the supplied name is
*        used to identify the information within the error message.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A pointer to the required keyword string value, or NULL if no value 
*     has previously been stored for the keyword (or if an error has 
*     occurred).

*/

/* Local Variables: */
   char *ret;            /* Returned keyword value */
   int si;               /* Integer co-ordinate version index */

/* Initialise */
   ret = NULL;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Convert the character co-ordinate version into an integer index, and
   check it is within range. The primary axis description (s=' ') is
   given index zero. 'A' is 1, 'B' is 2, etc. */
   if( s == ' ' ) {
      si = 0;
   } else if( islower(s) ){
      si = (int) ( s - 'a' ) + 1;
   } else {
      si = (int) ( s - 'A' ) + 1;
   }

   if( si < 0 || si > 26 ) {
      astError( AST__INTER, "GetItemC(fitschan): AST internal error; "
                "co-ordinate version '%c' ( char(%d) ) is invalid.", s, s );

/* Check the intermediate axis index is within range. */
   } else if( j < 0 || j > 98 ) {
      astError( AST__INTER, "GetItemC(fitschan): AST internal error; "
                "intermediate axis index %d is invalid.", j );

/* Otherwise, if the array holding the required keyword is not null, 
   proceed... */
   } else if( *item ){

/* Find the number of coordinate versions in the supplied array.
   Only proceed if it encompasses the requested co-ordinate
   version. */
      if( astSizeOf( (void *) *item )/sizeof(char **) > si ){

/* Find the number of intermediate axes in the supplied array.
   Only proceed if it encompasses the requested intermediate axis. */
         if( astSizeOf( (void *) (*item)[si] )/sizeof(char *) > j ){

/* Return the required keyword value. */
            ret = (*item)[si][j];
         }
      }
   }

/* If required, report an error if the requested item of information has
   not been stored. */
   if( !ret && name && astOK ){
      astError( AST__NOFTS, "%s(%s): No value can be found for %s.",
                method, class, name );
   }

   return ret;

}

static int GoodWarns( const char *value ){
/*
*  Name:
*     GoodWarns

*  Purpose:
*     Checks a string to ensure it is a legal list of warning conditions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int GoodWarns( const char *value )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function checks the supplied string to ensure it contains a space
*     separated list of zero or more recognised warning conditions. An
*     error is reported if it does not.

*  Parameters:
*     value
*        The string to check.

*  Returned Value:
*     Zero is returned if the supplied string is not a legal list of
*     conditions, or if an error has already occurred. One is returned 
*     otherwise.

*/

/* Local Variables: */
   char *b;              /* Pointer to next buffer element */
   const char *c  ;      /* Pointer to next character */
   char buf[100];        /* Buffer for condition name */
   int inword;           /* Are we in a word? */
   int n;                /* Number of conditions supplied */
   int ret;              /* Returned value */

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Report an error and return if the pointer is null. */
   if( !value ){
      astError( AST__ATTIN, "astSetWarnings(fitschan): Null pointer "
                "supplied for the Warnings attribute." );
      return ret;
   }

/* Initialise things */
   inword = 0;
   buf[ 0 ] = ' ';
   b = buf + 1;
   n = 0;
   ret = 1;

/* Loop round each character in the supplied string. */
   for( c = value ; c < value + strlen( value ) + 1; c++ ){

/* Have we found the first space or null following a word? */
      if( ( !(*c) || isspace( *c ) ) && inword ){

/* Add a space to the end of the buffer and terminate it. */
         *(b++) = ' ';
         *b = 0;

/* Check the word is legal by searching for it in the string of all
   conditions, which should be lower case and have spaces at start and end. 
   The word in the buffer is delimited by spaces and so it will not match 
   a substring within a condition. If it is legal increment the number of 
   conditions found. */
         if( strstr( ALLWARNINGS, buf ) ){         
            n++;

/* Otherwise, report an error and break. */
         } else {
            ret = 0;
            *(--b) = 0;
            astError( AST__ATTIN, "astSetWarnings(fitschan): Unknown "
                      "condition '%s' specified when setting the Warnings "
                      "attribute.", buf + 1 );
            break;
         }

/* Reset the pointer to the next character in the buffer, retaining the
   initial space in the buffer. */
         b = buf + 1;

/* Indicate we are no longer in a word. */
         inword = 0;

/* Have we found the first non-space, non-null character following a space? */
      } else if( *c && !isspace( *c ) && !inword ){

/* Note we are now in a word. */
         inword = 1;
         
      }

/* If we are in a word, copy the lowercase character to the buffer. */
      if( inword ) *(b++) = tolower( *c );
      
   }

   return ret;

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
*     CN  - CONTINUE values.
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
*     CN - Like "S".

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
MAKE_FGET(CN,char **,AST__CONTINUE)

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

   } else if( type == AST__CONTINUE ){
      cval = *( (char **) value);
      if( cval ){      
         astFitsSetCN( this, keyname, cval, comment, overwrite );
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
*     CN - A "CONTINUE" value, these are treated like string values, but
*          are encoded without an equals sign.
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
*     CN - "const char *".

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
MAKE_FSET(CN,const char *,AST__CONTINUE,(void *)value)
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

static void FixNew( AstFitsChan *this, int flag, int remove, 
                    const char *method, const char *class ){
/*
*
*  Name:
*     FixNew

*  Purpose:
*     Remove "new" flags from the whole FitsChan, and optionally remove
*     "new" cards.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FixNew( AstFitsChan *this, int flag, int remove, 
*                  const char *method, const char *class )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function searches the entire FitsChan for cards which are
*     marked as new using the supplied flag (NEW1 or NEW2). If "remove"
*     is non-zero, these cards are completely removed from the FitsChan 
*     (not just marked as used). If "remove" is zero, they are retained
*     and the specified flag is cleared.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     flag
*        The flag to use; NEW1 or NEW2.
*     remove
*        Remove flagged cards from the FitsChan?
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*     - If any cards are removed, the current Card is left at "end-of-file" 
*       on exit. If no cards are removed, the original current card is
*       retained.

*-
*/

/* Local Variables: */
   int *flags;             /* Pointer to flags mask for the current card */
   int icard;              /* Index of current card on entry */
   int ndeleted;           /* Number of cards deleted by this call */

/* Return if no FitsChan was supplied, or if the FitsChan is empty. */
   if ( !this || !this->head ) return;

/* Save the current card index, and rewind the FitsChan. */
   icard = astGetCard( this );
   astClearCard( this );

/* Indicate no cards have yet been deleted. */
   ndeleted = 0;

/* Loop through the list of FitsCards in the FitsChan until the final
   card is reached. */
   while( astOK && this->card ){

/* Get a pointer to the flags mask for this card. */
      flags = CardFlags( this );

/* See if the Card has been marked with the requeste new flag. */
      if( flags && ( (*flags) & flag ) ) {

/* If requested, remove the card. This will automatically move the
   current card on to the next card. */
         if( remove ){
            DeleteCard( this, method, class );
            ndeleted++;

/* Otherwise, clear the flag. */
         } else {
            *flags = (*flags) & ~flag;

/* Increment the card count and move on to the next card. */
            MoveCard( this, 1, method, class );

         }

/* Move on to the next card if this card is not marked with the requested 
   new flag. */
      } else {
         MoveCard( this, 1, method, class );
      }
   }

/* If no cards were removed, we can safely re-instate the original
   current card. Otherwise, the current card is left at "end-of-file". */
   if( ndeleted == 0 ) astSetCard( this, icard );

/* Return */
   return;

}

static void FixUsed( AstFitsChan *this, int used, int remove, 
                     const char *method, const char *class ){
/*
*
*  Name:
*     FixUsed

*  Purpose:
*     Remove "provisionally used" flags from the whole FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void FixUsed( AstFitsChan *this, int used, int remove, 
*                   const char *method, const char *class )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function searches the entire FitsChan for cards which are
*     marked as "provisionally used". The "provisionally used" flag is
*     cleared for each such card. In addition, if "used" is non-zero then
*     each such card is flagged as having been "definitely used". If
*     "remove" is non-zero, then all "provisionally used" cards are deleted 
*     from the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     used
*        Have the provisionally used cards definitely been used?
*     remove
*        Should provisionally used cards be deleted?
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Notes:
*     - This function attempts to execute even if an error has occurred.

*-
*/

/* Local Variables: */
   FitsCard *card0;        /* Pointer to current FitsCard */
   int *flags;             /* Pointer to flags mask for the current card */
   int old_ignoreused;     /* Original value of external variable IgnoreUsed */

/* Return if no FitsChan was supplied, or if the FitsChan is empty. */
   if ( !this || !this->head ) return;

/* Indicate that we should not skip over cards marked as having been
   read. */
   old_ignoreused = IgnoreUsed;
   IgnoreUsed = 0;

/* Save a pointer to the current card, and the reset the current card to
   be the first card. */
   card0 = this->card;
   astClearCard( this );

/* Loop through the list of FitsCards in the FitsChan until the final
   card is reached. */
   while( astOK && this->card ){

/* Get a pointer to the flags mask for this card. */
      flags = CardFlags( this );

/* See if the Card has been provisionally used. */
      if( flags && ( (*flags) & PROVISIONALLY_USED ) ) {

/* Clear the provisionally used flag. */
         *flags = (*flags) & ~PROVISIONALLY_USED;

/* If required, set the definitely used flag. */
         if( used ) *flags = (*flags) | USED;

/* If required, delete the card. The next card is made current. If we are
   about to delete the original current card, we need to update the
   pointer to the card to be made current at the end of this function.
   If we end up back at the head of the chain, indicate that we have
   reached the end of file by setting card0 NULL.  */
         if( remove ) {
            if( card0 == this->card && card0 ) {
               card0 = ( (FitsCard *) this->card )->next;
               if( (void *) card0 == this->head ) card0 = NULL;
            }
            DeleteCard( this, method, class );

/* Otherwise, just move on to the next card. */
         } else {
            MoveCard( this, 1, method, class );
         }

/* If this card has not bee provisionally used, move on to the next card. */
      } else {
         MoveCard( this, 1, method, class );
      }

   }

/* Re-instate the original current card. */
   this->card = card0;

/* If this card is now flagged as definitely used, move forward to the
   next un-used card. */
   flags = CardFlags( this );
   if( flags && (*flags & USED ) ) {
      IgnoreUsed = 1;
      MoveCard( this, 1, method, class );
   }

/* Re-instate the original flag indicating if cards marked as having been 
   read should be skipped over. */
   IgnoreUsed = old_ignoreused;

/* Return */
   return;

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
   int type;                   /* Card data type */

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
   type = CardType( this );
   if( type != AST__COMMENT ){

/* Get the number of digits to use when formatting floating point values. */
      digits = astGetFitsDigits( this );

/* Put an equals sign in column 9 (or a space if the keyword is a CONTINUE
   card), followed by a space in column 10. */
      buf[ len++ ] = ( type == AST__CONTINUE ) ? ' ' : '=';
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

static int FullForm( const char *list, const char *test, int abbrev ){
/*
*  Name:
*     FullForm

*  Purpose:
*     Identify the full form of an option string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int FullForm( const char *list, const char *test, int abbrev )

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
*     abbrev
*        1 if abbreviations are to be accepted. Zero otherwise.

*  Returned Value:
*     The index of the identified option within the supplied list, starting
*     at zero. -1 is returned if the option is not recognised, and (if
*     abbrev is 1 ) -2 if the option is ambiguous (no errors are reported 
*     in these cases). If abbrev is zero, the returned index will be the
*     index of the first matching string.
*    

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
   number of matches and save the current item index. If abbreviation is
   not allowed ensure that the lengths of the strings are equal. */
         if( !Ustrncmp( test, option, len ) && ( abbrev ||
             len == ChrLen( option ) ) ) {
            nmatch++;
            ret = i;
            if( !abbrev ) break;
         }

/* Get a pointer to the next option. */
         option = strtok( NULL, " " );
         i++;
      }

/* Return -1 if no match was found. */
      if( !nmatch ){
         ret = -1;

/* Return -2 if the option was ambiguous. */
      } else if( abbrev && nmatch > 1 ){
         ret = -2;
      }

/* Free the local copy of the options list. */
      llist = (char *) astFree( (void *) llist );
   }

/* Return the answer. */
   return ret;
}

static const char *GetAllWarnings( AstFitsChan *this ){
/*
*+
*  Name:
*     astGetAllWarnings

*  Purpose:
*     Return a list of all condition names.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fitschan.h"
*     const char *GetAllWarnings( AstFitsChan *this )

*  Class Membership:
*     FitsChan method.

*  Description:
*     This function returns a space separated lits of the condition names
*     currently recognized by the Warnings attribute.

*  Parameters:
*     this
*        Pointer to the FitsChan.

*  Returned Value:
*     A pointer to a static string holding the condition names.

*  Notes:
*     - This routine does not check the inherited status.

*-
*/

/* Return the result. */
   return ALLWARNINGS;

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

         } else if( ival == FITSPC_ENCODING ){
            result = FITSPC_STRING;

         } else if( ival == FITSIRAF_ENCODING ){
            result = FITSIRAF_STRING;

         } else if( ival == FITSAIPS_ENCODING ){
            result = FITSAIPS_STRING;

         } else if( ival == FITSWCS_ENCODING ){
            result = FITSWCS_STRING;

         } else if( ival == DSS_ENCODING ){
            result = DSS_STRING;

         } else {
            result = UNKNOWN_STRING;
         }
      }

/* DefB1950 */
/* -------- */
   } else if ( !strcmp( attrib, "defb1950" ) ) {
      ival = astGetDefB1950( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* FitsDigits. */
/* ----------- */
   } else if ( !strcmp( attrib, "fitsdigits" ) ) {
      ival = astGetFitsDigits( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* Ncard. */
/* ------ */
   } else if ( !strcmp( attrib, "ncard" ) ) {
      ival = astGetNcard( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* AllWarnings */
/* ----------- */
   } else if ( !strcmp( attrib, "allwarnings" ) ) {
      result = astGetAllWarnings( this );

/* Warnings. */
/* -------- */
   } else if ( !strcmp( attrib, "warnings" ) ) {
      result = astGetWarnings( this );

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
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   char *keyword;                /* Pointer to current keyword string */
   char *newdata;                /* Pointer to stripped string value */
   char *upq;                    /* Pointer to unprequoted string */
   char buff[ BUFF_LEN + 1 ];    /* Buffer for formatting values */
   const char *class;            /* Pointer to object class */
   const char *method;           /* Pointer to method name */
   int cont;                     /* String ends with an ampersand? */
   int done;                     /* Data item found? */
   int freedata;                 /* Should the data pointer be freed? */
   int i;                        /* Loop counter for keyword characters */
   int len;                      /* Length of current keyword */
   int nc;                       /* Number of characters read by "sscanf" */
   int nn;                       /* No. of characters after UnPreQuoting */
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

/* Mark all cards as having been used unless we are skipping over cards which
   may not be related to AST. */
      if( !skip ) MarkCard( this );

/* Ignore comment cards. */
      if ( type != AST__COMMENT ) {

/* Native encoding requires trailing white space to be removed from
   string values (so that null strings can be distinguished from blank
   strings). Do this now. */
         freedata = 0;
         if ( ( type == AST__STRING || type == AST__CONTINUE ) && data ){
            newdata = (char *) astStore( NULL, data, strlen( (char *) data ) + 1 );
            if( newdata ){
               newdata[ ChrLen( data ) ] = 0;            
               data = (void *) newdata;
               freedata = 1;
            }
         }
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

/* Indicate that the current card has been used. */
            MarkCard( this );

/* The "begin" item will be preceded by a header of COMMENT cards. Mark
   them as having been used. */
            ComBlock( this, -1, method, class );

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
   these cards as having been used. */
            ComBlock( this, 1, method, class );

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

/* A long string value may be continued on subsequent CONTINUE cards. See
   if the current string may be continued. This is the case if the final
   non-blank character (before UnPreQuoting) is an ampersand. */
                  cont = ( ((char *) data)[ ChrLen( data ) - 1 ] == '&' );

/* If the string does not end with an ampersand, just UnPreQUote it and
   return a copy. */
                  if( !cont ) {
                     *val = UnPreQuote( (const char *) data );

/* Otherwise, initialise the returned string to hold a copy of the keyword 
   value. */
                  } else {
                     nc = strlen( (const char *) data );
                     *val = astStore( NULL, (const char *) data, nc + 1 );

/* Loop round reading any subsequent CONTINUE cards. Leave the loop when
   the end-of-file is hit, or an error occurs. */
                     while( cont && MoveCard( this, 1, method, class ) &&
                            astOK ){

/* See if this is a CONTINUE card. If so, get its data pointer. */
                        if( CardType( this ) == AST__CONTINUE ){
                           data = CardData( this, NULL );

/* See if the CONTINUE card ends with an ampersand (i.e. if there is
   a possibility of there being any remaining CONTINUE cards). */
                           cont = ( ( (char *) data)[ ChrLen( data ) - 1 ] == '&' );

/* UnPreQUote it. */
                           upq = UnPreQuote( (const char *) data );
                           if( !astOK ) break;

/* Expand the memory for the returned string to hold the new string. */
                           nn = strlen( upq );
                           *val = astRealloc( *val, nc + nn );
                           if( !astOK ) break;
                           
/* Copy the new string into the expanded memory, so that the first
   character of the new string over-writes the trailing ampersand 
   currently in the buffer. */
                           strcpy( *val + nc - 1, upq );

/* Release the memory holding the UnPreQUoted string . */
                           upq = astFree( upq );

/* Update the current length of the returned string. */
                           nc += nn - 1;

/* Mark the current card as having been read. */
                           MarkCard( this );

/* Report an error if this is not a CONTINUE card. */
                        } else {
                           astError( AST__BADIN, "%s(%s): One or more "
                                     "FITS \"CONTINUE\" cards are missing "
                                     "after the card for keyword \"%s\".",
                                     method, class, keyword );
                        }
                     }
                  }
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

/* Free any memory used to hold stripped string data. */
         if( freedata ) newdata = (char *) astFree( (void *) newdata );

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

static int GetValue( AstFitsChan *this, char *keyname, int type, 
                     void *value, int report, const char *method, 
                     const char *class ){
/*
*  Name:
*     GetValue

*  Purpose:
*     Obtain a FITS keyword value.

*  Type:
*     Private function.

*  Synopsis:
*     int GetValue( AstFitsChan *this, char *keyname, int type, void *value, 
*                   int report, const char *method, const char *class )

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
   vtab->FitsGetCN = FitsGetCN;
   vtab->FitsGetCom = FitsGetCom;
   vtab->FitsSetCom = FitsSetCom;
   vtab->FitsSetCF = FitsSetCF;
   vtab->FitsSetCI = FitsSetCI;
   vtab->FitsSetF = FitsSetF;
   vtab->FitsSetI = FitsSetI;
   vtab->FitsSetL = FitsSetL;
   vtab->FitsSetS = FitsSetS;
   vtab->FitsSetCN = FitsSetCN;
   vtab->ClearCard = ClearCard;
   vtab->TestCard = TestCard;
   vtab->SetCard = SetCard;
   vtab->GetCard = GetCard;
   vtab->ClearFitsDigits = ClearFitsDigits;
   vtab->TestFitsDigits = TestFitsDigits;
   vtab->SetFitsDigits = SetFitsDigits;
   vtab->GetFitsDigits = GetFitsDigits;
   vtab->ClearDefB1950 = ClearDefB1950;
   vtab->TestDefB1950 = TestDefB1950;
   vtab->SetDefB1950 = SetDefB1950;
   vtab->GetDefB1950 = GetDefB1950;
   vtab->ClearWarnings = ClearWarnings;
   vtab->TestWarnings = TestWarnings;
   vtab->SetWarnings = SetWarnings;
   vtab->GetWarnings = GetWarnings;
   vtab->GetNcard = GetNcard;
   vtab->GetAllWarnings = GetAllWarnings;
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
   list of FitsCard structures should ignore cards which have been 
   read into an AST object. */
   IgnoreUsed = 1;

/* Indicate that new cards added to the FitsChan should not be marked as
   new. This facility is only used when objects are written to the FitsChan
   using astWrite (so that in appropriately added cards can be identified
   and removed), and is explicitly switched on in astWrite. */
   MarkNew = 0;

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

/* Local Variables: */
   int flags;             /* Flags to assign to new card */

/* Check the global status. */
   if( !astOK ) return;

/* If the current card is to be over-written, delete the current card (the 
   next card in the list, if any, will become the new current card). */
   if( overwrite ) DeleteCard( this, method, class );

/* If requested, set both NEW flags for the new card. */
   flags = ( MarkNew ) ? ( NEW1 | NEW2 ): 0;

/* Insert the new card into the list, just before the current card. */
   NewCard( this, name, type, data, comment, flags );

}

static int IRAFFromStore( AstFitsChan *this, FitsStore *store, 
                          int axlat, int axlon, const char *method, 
                          const char *class ){
/*
*  Name:
*     IRAFFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan using FITS-IRAF encoding.

*  Type:
*     Private function.

*  Synopsis:
*     int IRAFFromStore( AstFitsChan *this, FitsStore *store, 
*                        int axlat, int axlon, const char *method, 
*                        const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using FITS-IRAF encoding.
*
*     IRAF encoding is like FITS-WCS encoding but with the following
*     restrictions:
*
*     1) The celestial projection must not have any projection parameters
*     which are not set to their default values. The one exception to this 
*     is that SIN projections are acceptable if the associated projection 
*     parameter PV<axlat>_1 is zero and PV<axlat>_2 = cot( reference point 
*     latitude). This is encoded using the string "-NCP". The SFL projection 
*     is encoded using the string "-GLS". Note, the original IRAF WCS
*     system only recognised a small subset of the currently available
*     projections, but some more recent IRAF-like software recognizes some 
*     of the new projections included in the FITS-WCS encoding.
*
*     2) The celestial axes must be RA/DEC, galactic or ecliptic.   
*
*     3) LONPOLE and LATPOLE cannot be used. 
*
*     4) Only primary axis descriptions are written out.
*
*     5) RADECSYS is used in place of RADESYS.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   char *comm;         /* Pointer to comment string */
   char *cval;         /* Pointer to string keyword value */
   char combuf[80];    /* Buffer for FITS card comment */
   char lattype[MXCTYPELEN];/* Latitude axis CTYPE */
   char lontype[MXCTYPELEN];/* Longitude axis CTYPE */
   char s;             /* Co-ordinate version character */
   char sign[2];       /* Fraction's sign character */
   double fd;          /* Fraction of a day */
   double mjd99;       /* MJD at start of 1999 */
   double p1, p2;      /* Projection parameters */
   double val;         /* General purpose value */
   int i;              /* Axis index */
   int ihmsf[ 4 ];     /* Hour, minute, second, fractional second */
   int iymdf[ 4 ];     /* Year, month, date, fractional day */
   int j;              /* Axis index */
   int jj;             /* SlaLib status */
   int naxis;          /* No. of axes */
   int ok;             /* Is FitsSTore OK for IRAF encoding? */
   int prj;            /* Projection type */
   int ret;            /* Returned value. */

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* First check that the values in the FitsStore conform to the
   requirements of the IRAF encoding. Assume they do to begin with. */
   ok = 1;

/* Just do primary axes. */
   s = ' '; 

/* If both longitude and latitude axes are present ...*/
   if( axlon >= 0 && axlat >= 0 ) {

/* Get the CTYPE values for both axes. */
      cval = GetItemC( &(store->ctype), axlon, s, NULL, method, class );
      if( !cval ) return ret;
      strcpy( lontype, cval );

      cval = GetItemC( &(store->ctype), axlat, s, NULL, method, class );
      if( !cval ) return ret;
      strcpy( lattype, cval );

/* Extract the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. */
      prj = astWcsPrjType( lattype + 4 );

/* Check the projection type is OK. Assume not initially. */
      ok = 0;
      if( prj != AST__SIN ){
   
/* There must be no projection parameters. */
         if( GetMaxIM( &(store->pv) ) == -1 ) ok = 1;

/* Change the new SFL projection code to to the older equivalent GLS */
         if( prj == AST__SFL ){
            (void) strcpy( lontype + 4, "-GLS" );
            (void) strcpy( lattype + 4, "-GLS" );
         }

/* SIN projections are only acceptable if the associated projection
   parameters are both zero, or if the first is zero and the second 
   = cot( reference point latitude )  (the latter case is equivalent to 
   the old NCP projection). */
      } else {
         p1 = GetItem( &( store->pv ), axlat, 1, s, NULL, method, class );
         p2 = GetItem( &( store->pv ), axlat, 2, s, NULL, method, class );
         if( p1 == AST__BAD ) p1 = 0.0;   
         if( p2 == AST__BAD ) p2 = 0.0;   

         val = GetItem( &( store->crval ), axlat, 0, s, NULL, method, class );
         if( val != AST__BAD ) {
            if( p1 == 0.0 ) {
               if( p2 == 0.0 ) {
                  ok = 1;
      
               } else if( fabs( p2 ) >= 1.0E14 && val == 0.0 ){
                  ok = 1;
                  (void) strcpy( lontype + 4, "-NCP" );
                  (void) strcpy( lattype + 4, "-NCP" );
      
               } else if( fabs( p2*tan( AST__DD2R*val ) - 1.0 ) 
                          < 0.01 ){
                  ok = 1;
                  (void) strcpy( lontype + 4, "-NCP" );
                  (void) strcpy( lattype + 4, "-NCP" );
               }
            }
         }
      }

/* Identify the celestial coordinate system from the first 4 characters of the
   longitude CTYPE value. Only RA, galactic longitude, and ecliptic
   longitude can be stored using FITS-IRAF. */
      if( strncmp( lontype, "RA--", 4 ) &&
          strncmp( lontype, "GLON", 4 ) &&
          strncmp( lontype, "ELON", 4 ) ) ok = 0;

/* If the physical Frame requires a LONPOLE or LATPOLE keyword, it cannot
   be encoded using FITS-IRAF. */
      if( GetItem( &(store->latpole), 0, 0, s, NULL, method, class )
          != AST__BAD || 
          GetItem( &(store->lonpole), 0, 0, s, NULL, method, class )
          != AST__BAD ) ok = 0;

/* If there are no celestial axes, the physical Frame can be written out
   using FITS-IRAF. */
   } else {
      ok = 1;
   }

/* Return if the FitsStore does not conform to IRAF encoding. */
   if( !ok ) return ret;

/* Save the number of pixel axes */
   naxis = GetMaxIM( &(store->crpix) ) + 1;

/* Get and save CRPIX for all pixel axes. These are required, so return
   if they are not available. */
   for( i = 0; i < naxis; i++ ){
      val = GetItem( &(store->crpix), 0, i, s, NULL, method, class );
      if( val == AST__BAD ) return ret;
      sprintf( combuf, "Reference pixel on axis %d", i + 1 );
      SetValue( this, FormatKey( "CRPIX", i + 1, -1, s ), &val, AST__FLOAT, 
                combuf );
   }

/* Get and save CRVAL for all intermediate axes. These are required, so return
   if they are not available. */
   for( j = 0; j < naxis; j++ ){
      val = GetItem( &(store->crval), j, 0, s, NULL, method, class );
      if( val == AST__BAD ) return ret;
      sprintf( combuf, "Value at ref. pixel on axis %d", j + 1 );
      SetValue( this, FormatKey( "CRVAL", j + 1, -1, s ), &val, AST__FLOAT, 
                combuf );
   }

/* Get and save CTYPE for all intermediate axes. These are required, so return
   if they are not available. Use the potentially modified versions saved
   above for the celestial axes. */
   for( j = 0; j < naxis; j++ ){
      if( j == axlat ) {
         cval = lattype;
      } else if( j == axlon ) {
         cval = lontype;
      } else {
         cval = GetItemC( &(store->ctype), j, s, NULL, method, class );
         if( !cval ) return ret;
      }
      comm = GetItemC( &(store->ctype_com), j, s, NULL, method, class );
      if( !comm ) {            
         sprintf( combuf, "Type of co-ordinate on axis %d", j + 1 );
         comm = combuf;
      }
      SetValue( this, FormatKey( "CTYPE", j + 1, -1, s ), &cval, AST__STRING, 
                comm );
   }

/* CD matrix */
   for( j = 0; j < naxis; j++ ){
      for( i = 0; i < naxis; i++ ){
         val = GetItem( &(store->cd), j, i, s, NULL, method, class );
         if( val != AST__BAD ) {
            if( ( i != j && val != 0.0 ) || 
                ( i == j && val != 1.0 ) ){
               SetValue( this, FormatKey( "CD", j + 1, i + 1, s ), &val, 
                         AST__FLOAT, "Transformation matrix element" );
            }
         }
      }
   }

/* Get and save CUNIT for all intermediate axes. These are NOT required, so 
   do not return if they are not available. */
   for( j = 0; j < naxis; j++ ){
      cval = GetItemC( &(store->cunit), j, s, NULL, method, class );
      if( cval ) {
         sprintf( combuf, "Units for axis %d", j + 1 );
         SetValue( this, FormatKey( "CUNIT", j + 1, -1, s ), &cval, AST__STRING, 
                   combuf );
      }
   }

/* Get and save RADECSYS. This is NOT required, so do not return if it is 
   not available. */
   cval = GetItemC( &(store->radesys), 0, s, NULL, method, class );
   if( cval ) SetValue( this, "RADECSYS", &cval, AST__STRING, 
                        "Reference frame for RA/DEC values" );

/* Reference equinox */
   val = GetItem( &(store->equinox), 0, 0, s, NULL, method, class );
   if( val != AST__BAD ) SetValue( this, "EQUINOX", &val, AST__FLOAT, 
                                   "Epoch of reference equinox" );

/* Date of observation */
   val = GetItem( &(store->mjdobs), 0, 0, s, NULL, method, class );
   if( val != AST__BAD ) {

/* The format used for the DATE-OBS keyword depends on the value of the
   keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
   Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format. */
      slaCaldj( 99, 1, 1, &mjd99, &jj );
      if( val < mjd99 ) {

         slaDjcal( 0, val, iymdf, &jj );
         sprintf( combuf, "%2.2d/%2.2d/%2.2d", iymdf[ 2 ], iymdf[ 1 ], 
                  iymdf[ 0 ] - ( ( iymdf[ 0 ] > 1999 ) ? 2000 : 1900 ) ); 

      } else {

         slaDjcl( val, iymdf, iymdf+1, iymdf+2, &fd, &jj );
         slaDd2tf( 3, fd, sign, ihmsf );
         sprintf( combuf, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3dZ",
                  iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
                  ihmsf[2], ihmsf[3] ); 
      }

/* Now store the formatted string in the FitsChan. */
      cval = combuf;
      SetValue( this, "DATE-OBS", (void *) &cval, AST__STRING,
                "Date of observation" );
   }

/* If we get here we have succeeded. */
   ret = 1;

/* Return zero or ret depending on whether an error has occurred. */
   return astOK ? ret : 0;
}

static int LinearMap( AstMapping *map, int ndim, double *dim, 
                      double *matrix, double maxerr ){
/*
*  Name:
*     LinearMap

*  Purpose:
*     See if a mapping is linear and return a matrix describing it.

*  Type:
*     Private function.

*  Synopsis:
*     int LinearMap( AstMapping *map, int ndim, double *dim, 
*                    double *matrix, double maxerr )

*  Class Membership:
*     FitsChan

*  Description:
*     This function checks to see if the supplied Mapping is linear,
*     except for an optional shift of origin. A least squares linear fit 
*     is done to the Mapping. If the mean squared residual in this fit is 
*     less than the suplied value of maxerr, then the fit is accepted. 
*     Otherwise, zero is returned.

*  Parameters:
*     map
*        A pointer to the Mapping to be checked. The Mapping must have
*        equal numbers of input and output coordinates.
*     ndim
*        The number of input and output coordinates for the Mapping.
*     dim
*        The dimensions of the array grid. If not known, supply AST__BAD.
*     matrix
*        A pointer to an array of ndim**2 elements to receive the matrix.
*        The elements are stored row by row. If the Mapping is not linear
*        the supplied values are left unchanged.
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
   double *c;                         /* Pointer to array of constant terms */
   double *m;                         /* Pointer to next matrix element */
   double *mm;                        /* Pointer to first matrix element in row */
   double mx;                         /* Largest absolute element value */
   double rms;                        /* RMS error of fit */
   double v;                          /* Absolute element value */
   int i;                             /* Loop count */
   int j;                             /* Loop count */
   int ret;                           /* Returned value */

 
/* Check the status */
   if( !astOK ) return 0;

/* If the number of input and output axes are equal, attempt to do a least 
   squares linear fit to the Mapping. */
   c = (double *) astMalloc( ndim*sizeof( double ) );
   ret = FitLinear( map, ndim, dim, c, matrix, &rms );
   c = (double *) astFree( (void *) c );

/* If the rms is greater than the maximum allowed error, returns zero. */
   if( ret && rms*rms > maxerr ) {
      ret = 0;

/* Otherwise, search for small non-zero values (typically caused by
   rounding error within the least squares fit) and set them to zero. */
   } else {

/* For each row of the matrix, find the largest absolute value. */
      m = matrix;
      for( i = 0; i < ndim; i++ ){
         mm = m;
         mx = 0.0;
         for( j = 0; j < ndim; j++ ){
            v = fabs( *(m++) );
            if( v > mx ) mx = v;
         }

/* Look for elements with absolute values which are less than 1.0E-5 of
   the largest value found above, and set them to zero. */
         m = mm;
         mx *= 1.0E-5;
         for( j = 0; j < ndim; j++ ){
            if( fabs( *m ) < mx ) *m = 0.0;
            m++;
         }
      }
   }

/* If an error has occurred, return 0. */
   if( !astOK ) ret = 0;

/* Return the result. */
   return ret;

}

static void LinearSky( AstFrame *phyfrm, int naxis, FitsStore *store, char s,
                       const char *method, const char *class ){
/*
*  Name:
*     LinearSky

*  Purpose:
*     See if a non-WCS Frame can be described by a linear WCS projection.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void LinearSky( AstFrame *phyfrm, int naxis, FitsStore *store, char s, 
*                     const char *method, const char *class  )

*  Class Membership:
*     FitsChan

*  Description:
*     This function checks to see if the supplied Frame is (or contains) a
*     SkyFrame. If so, the longitude and latitude axes are found, and 
*     values for FITS-WCS keywords CTYPE, EQUINOX, MJDOBS and RADESYS are
*     set up to describe the SkyFrame. The keyword values are stored in
*     "store".

*  Parameters:
*     phyfrm
*        Pointer to the Frame.
*     naxis
*        Number of axes in the Frame.
*     store
*        Pointer to the Fitstore structure in whioch to store the new
*        keyword values.
*     s
*        The co-ordinate version character. A space means the primary
*        axis descriptions. Otherwise the supplied character should be 
*        an upper case alphabetical character ('A' to 'Z'). 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*/

/* Local Variables: */
   AstSkyFrame *skyfrm;   /* Pointer to the first SkyFrame to be found */   
   AstFrame *frame;       /* Pointer to the Frame containing the current axis */   
   double val;            /* Store item value */
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
      if( SkySys( skyfrm, AST__CAR, store, axlon, axlat, s, method, class ) ){

/* The CRVAL and CD values need to be converted from radians to degrees
   on the latitude and longitude axes. */
         val = GetItem( &(store->crval), axlat, 0, s, NULL, method, class );
         if( val != AST__BAD ) SetItem( &(store->crval), axlat, 0, s, 
                                        val*AST__DR2D );
         for( i = 0; i < naxis; i++ ){
            val = GetItem( &(store->cd), axlat, i, s, NULL, method, class );
            if( val != AST__BAD ) SetItem( &(store->cd), axlat, i, s, 
                                           val*AST__DR2D );
         }

         val = GetItem( &(store->crval), axlon, 0, s, NULL, method, class );
         if( val != AST__BAD ) SetItem( &(store->crval), axlon, 0, s, 
                                        val*AST__DR2D );
         for( i = 0; i < naxis; i++ ){
            val = GetItem( &(store->cd), axlon, i, s, NULL, method, class );
            if( val != AST__BAD ) SetItem( &(store->cd), axlon, i, s, 
                                           val*AST__DR2D );
         }
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

/* Otherwise, we need to carry on checking the remaining fields. */
      } else {

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
   ret = 0;
   if( !astOK ) return ret;

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
   } else if( astOK ){
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
*     The current card is marked as having been "provisionally used" in
*     the construction of an AST object. If the Object is constructed 
*     succesfully, such cards are marked as havign been definitely used,
*     and they are then considered to have been removed from the FitsChan.

*  Parameters:
*     this
*        Pointer to the FitsChan containing the list of cards.

*  Notes:
*     -  The card remains the current card even though it is now marked
*     as having been read.

*/
   int flags;

/* Return if the global error status has been set, or the current card
   is not defined. */
   if( !astOK || !this->card ) return;

/* Set the PROVISIONALLY_USED flag in the current card. */
   flags = ( (FitsCard *) this->card )->flags;
   ( (FitsCard *) this->card )->flags = flags | PROVISIONALLY_USED;

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
*     cards which have been read into an AST object if the external 
*     IgnoreUsed flag is set non-zero.

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
   have been read into an AST object if the external "IgnoreUsed" flag is 
   set. */
         } else if( card ){
            if( !CARDUSED(card) ) moved++;
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
         if( !IgnoreUsed ){
            card = GetLink( card, PREVIOUS, method, class );

/* If cards which have been read into an AST object are to be ignored... */
         } else {

/* We need to find the previous card which has not been read into an AST
   object. We do not search beyond the start of the list. */
            card0 = GetLink( card, PREVIOUS, method, class );
            while( card0 && CARDUSED(card0) && (void *) card0 != this->head ){
               card0 = GetLink( card0, PREVIOUS, method, class );
            }

/* If no such card was found we leave the card where it is. */
            if( card0 && ( card0->flags & USED ) ) {
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
                     const void *data, const char *comment, int flags ){
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
*                   const void *data, const char *comment, int flags )

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
*     flags
*        The flags to assign to the card.

*  Notes:
*     -  The new card is inserted into the list in front of the current card,
*     so that the "next" link from the new card points to the current card. 
*     If the FitsChan is currently at end-of-file (indicated by a NULL
*     pointer being stored for the current card), then the card is appended
*     to the end of the list. The pointer to the current card is left 
*     unchanged.
*     -  Keyword names are converted to upper case before being stored.
*     -  Any trailing white space in a string value is saved as supplied.
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

/* String values... */
         } else if( type == AST__STRING || type == AST__CONTINUE ){

/* Find the number of characters excluding the trailing null character. */
            nc = strlen( data );

/* Store the string, reserving room for a terminating null. */
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

/* Find the first non-blank character in the comment, and find the used
   length of the remaining string. */
      if( comment ){
         a = comment;
         while( isspace( *a ) ) a++;
         nc = ChrLen( a );
      } else {
         nc = 0;
      }

/* Copy any comment (excluding leading and trailing white space). */
      if( nc > 0 ){
         new->comment = astStore( NULL, (void *) a, (size_t)( nc + 1 ) );
         ( (char *) new->comment)[ nc ] = 0;
      } else {
         new->comment = NULL;
      }
      
/* Set the supplied flag values. */
      new->flags = flags;

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

static int PCFromStore( AstFitsChan *this, FitsStore *store, 
                        int axlat, int axlon, const char *method, 
                        const char *class ){
/*
*  Name:
*     PCFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan using FITS-PC encoding.

*  Type:
*     Private function.

*  Synopsis:
*     int PCFromStore( AstFitsChan *this, FitsStore *store, 
*                      int axlat, int axlon, const char *method, 
*                      const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using FITS-PC encoding.
*
*     Zero is returned if the primary axis descriptions cannot be produced.
*     Whether or not secondary axis descriptions can be produced does not
*     effect the returned value (i.e. failure to produce a specific set of 
*     secondary axes does not prevent other axis descriptions from being
*     produced).

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   char *comm;         /* Pointer to comment string */
   char *cval;         /* Pointer to string keyword value */
   char combuf[80];    /* Buffer for FITS card comment */
   char keyname[10];   /* Buffer for keyword name string */
   char primsys[20];   /* Buffer for primnary RADECSYS value */
   char type[MXCTYPELEN];/* Buffer for CTYPE value */
   char s;             /* Co-ordinate version character */
   char sign[2];       /* Fraction's sign character */
   char sup;           /* Upper limit on s */
   double *c;          /* Pointer to next array element */
   double *cdelt;      /* Pointer to Frame CDELT matrix */
   double *d;          /* Pointer to next array element */
   double *matrix;     /* Pointer to Frame PC/CD matrix */
   double *primpc;     /* Pointer to primary PC/CD matrix */
   double fd;          /* Fraction of a day */
   double mjd99;       /* MJD at start of 1999 */
   double primdt;      /* Primary mjd-obs value */
   double primeq;      /* Primary equinox value */
   double primln;      /* Primary lonpole value */
   double primlt;      /* Primary latpole value */
   double primpv[10];  /* Primary projection parameter values */
   double val;         /* General purpose value */
   int i;              /* Axis index */
   int ihmsf[ 4 ];     /* Hour, minute, second, fractional second */
   int is;             /* Co-ordinate version index */
   int iymdf[ 4 ];     /* Year, month, date, fractional day */
   int j;              /* Axis index */
   int jj;             /* SlaLib status */
   int m;              /* Parameter index */
   int maxm;           /* Upper limit on m */
   int naxis;          /* No. of axes */
   int ok;             /* Frame written out succesfully? */
   int prj;            /* Projection type */
   int ret;            /* Returned value. */

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Find the number of co-ordinate versions in the FitsStore. FITS-PC
   can only encode 10 axis descriptions (including primary). */
   sup = GetMaxS( &(store->crval) ); 
   if( sup > 'I' ) return ret;

/* Save the number of pixel axes */
   naxis = GetMaxIM( &(store->crpix) ) + 1;

/* Loop round all co-ordinate versions (0-9) */
   primpc = NULL;
   for( s = ' '; s < sup && astOK; s++ ){      
      is = s - 'A' + 1;

/*  Assume this Frame can be written out succesfully. */
      ok = 1;

/* PC matrix */
/* Allocate memory to hold the CD or PC matrix */
      matrix = (double *) astMalloc( sizeof(double)*naxis*naxis );
      if( matrix ){

/* Fill this array with the values of the matrix elements supplied in the
   FitsStore. */
         c = matrix;
         for( j = 0; j < naxis; j++ ){
            for( i = 0; i < naxis; i++ ){
               *c = GetItem( &(store->cd), j, i, s, NULL, method, class );
               if( *c == AST__BAD ) *c = ( i == j ) ? 1.0 : 0.0;
               c++;
            }
         }

/* Allocate memory to hold the corresponding CDELT values */
         cdelt = (double *) astMalloc( sizeof(double)*naxis );
         if( cdelt ){

/* Split the CD matrix currently in "matrix" into a PC matrix and
   corresponding CDELT values. The CD matrix in "matrix" is replaced by 
   the PC matrix. */
            (void) SplitMat( naxis, matrix, cdelt );

/* If this is the primary axis Frame, save a copy of the PC matrix, and 
   put the PC and CDELT keywords into the FitsChan. A copy of the matrix
   is required so that we can compare PC matrices for subsequent Frames 
   with the primary PC matrix. FITS-PC requires that all primary and 
   secondary Frames use the same PC matrix. */
            if( s == ' ' ) {
               primpc = (double *) astStore(  NULL, (void *) matrix, 
                                              sizeof(double)*naxis*naxis );

/* Store each matrix element in turn. */
               c = matrix;
               for( j = 0; j < naxis; j++ ){
                  for( i = 0; i < naxis; i++ ){

/* Set the element bad if it takes its default value. */
                     val = *(c++);
                     if( j == i ){
                        if( EQUAL( val, 1.0 ) ) val = AST__BAD;
                     } else {
                        if( EQUAL( val, 0.0 ) ) val = AST__BAD;
                     }

/* Only store elements which do not take their default values. */
                     if( val != AST__BAD ){
                        sprintf( keyname, "PC%.3d%.3d", j + 1, i + 1 );
                        SetValue( this, keyname, &val, AST__FLOAT, NULL );
                     }
                  }
               }

/* For secondary axis descriptions, a check is made that the PC values are 
   the same as the primary PC values stored earlier. If not, the current 
   Frame cannot be stored as a secondary axis description so continue on 
   to the next Frame. */
            } else {
               if( primpc ){
                  c = matrix;
                  d = primpc;
                  for( j = 0; j < naxis; j++ ){
                     for( i = 0; i < naxis; i++ ){
                        if( !EQUAL( *c, *d ) ){
                           ok = 0;
                        } else {
                           c++;
                           d++;
                        }
                     }
                  }

/* Continue with the next Frame if the PC matrix for this Frame is different 
   to the primary PC matrix. */
                  if( !ok ) goto next;
               }
            }

/* Now save the CDELT values. */
            c = cdelt;
            for( j = 0; j < naxis; j++ ){
               sprintf( combuf, "Pixel scale on axis %d", j + 1 );
               if( s == ' ' ) {
                  sprintf( keyname, "CDELT%d", j + 1 );
               } else {
                  sprintf( keyname, "C%dELT%d", is, j + 1 );
               }
               SetValue( this, keyname, c++, AST__FLOAT, combuf );
            }

/* Release memory. */
            cdelt = (double *) astFree( (void *) cdelt );
         }
         matrix = (double *) astFree( (void *) matrix );
      }

/* Get and save CRPIX for all pixel axes. These are required, so continue
   if they are not available. */
      for( i = 0; i < naxis; i++ ){
         val = GetItem( &(store->crpix), 0, i, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
            goto next;
         }
         sprintf( combuf, "Reference pixel on axis %d", i + 1 );
         if( s == ' ' ) {
            sprintf( keyname, "CRPIX%d", i + 1 );
         } else {
            sprintf( keyname, "C%dPIX%d", is, i + 1 );
         }
         SetValue( this, keyname, &val, AST__FLOAT, combuf );
      }

/* Get and save CRVAL for all intermediate axes. These are required, so
   continue if they are not available. */
      for( j = 0; j < naxis; j++ ){
         val = GetItem( &(store->crval), j, 0, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
            goto next;
         }
         sprintf( combuf, "Value at ref. pixel on axis %d", j + 1 );
         if( s == ' ' ) {
            sprintf( keyname, "CRVAL%d", j + 1 );
         } else {
            sprintf( keyname, "C%dVAL%d", is, j + 1 );
         }
         SetValue( this, keyname, &val, AST__FLOAT, combuf );
      }

/* Get and save CTYPE for all intermediate axes. These are required, so 
   continue if they are not available. */
      for( j = 0; j < naxis; j++ ){
         cval = GetItemC( &(store->ctype), j, s, NULL, method, class );
         if( !cval ) {
            ok = 0;
            goto next;
         }

         comm = GetItemC( &(store->ctype_com), j, s, NULL, method, class );
         if( !comm ) {            
            sprintf( combuf, "Type of co-ordinate on axis %d", j + 1 );
            comm = combuf;
         }

         if( s == ' ' ) {
            sprintf( keyname, "CTYPE%d", j + 1 );
         } else {
            sprintf( keyname, "C%dYPE%d", is, j + 1 );
         }

/* FITS-PC cannot handle celestial axes of type "xxLT" or "xxLN". */
         if( !strncmp( cval + 2, "LT-", 3 ) ||
             !strncmp( cval + 2, "LN-", 3 ) ){
            ok = 0;
            goto next;
         }

/* Extract the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. This will be AST__WCSBAD for non-celestial
   axes. */
         prj = astWcsPrjType( cval + 4 );

/* Change the new SFL projection code to to the older equivalent GLS */
         if( prj == AST__SFL ) {
            strcpy( type, cval );
            (void) strcpy( type + 4, "-GLS" );
            cval = type;
         }

/* Store the CTYPE value */
         SetValue( this, keyname, &cval, AST__STRING, comm );
      }

/* Get and save CUNIT for all intermediate axes. These are NOT required, so 
   do not pass on if they are not available. */
      for( j = 0; j < naxis; j++ ){
         cval = GetItemC( &(store->cunit), j, s, NULL, method, class );
         if( cval ) {
            sprintf( combuf, "Units for axis %d", j + 1 );
            if( s == ' ' ) {
               sprintf( keyname, "CUNIT%d", j + 1 );
            } else {
               sprintf( keyname, "C%dNIT%d", is, j + 1 );
            }
            SetValue( this, keyname, &cval, AST__STRING, combuf );
         }
      }

/* Get and save RADESYS. This is NOT required, so do not pass on if it is 
   not available. If RADECSYS is provided for a secondary axis, it must
   be the same as the primary axis RADECSYS value. If it is not, pass on to
   the next Frame. */
      cval = GetItemC( &(store->radesys), 0, s, NULL, method, class );
      if( cval ) {
         if( s == ' ' ) {
            strcpy( primsys, cval );
            SetValue( this, "RADECSYS", &cval, AST__STRING, 
                      "Reference frame for RA/DEC values" );
         } else if( strcmp( cval, primsys ) ) {
            ok = 0;
            goto next;
         }
      }

/* Reference equinox. This is NOT required, so do not pass on if it is 
   not available. If equinox is provided for a secondary axis, it must
   be the same as the primary axis equinox value. If it is not, pass on to
   the next Frame. */
      val = GetItem( &(store->equinox), 0, 0, s, NULL, method, class );
      if( s == ' ' ) {
         primeq = val;
         if( val != AST__BAD ) SetValue( this, "EQUINOX", &val, AST__FLOAT, 
                                         "Epoch of reference equinox" );
      } else if( !EQUAL( val, primeq ) ){
         ok = 0;
         goto next;
      }

/* Latitude of native north pole. This is NOT required, so do not pass on 
   if it is not available. If latpole is provided for a secondary axis, it 
   must be the same as the primary axis value. If it is not, pass on to
   the next Frame. */
      val = GetItem( &(store->latpole), 0, 0, s, NULL, method, class );
      if( s == ' ' ) {
         primlt = val;
         if( val != AST__BAD ) SetValue( this, "LATPOLE", &val, AST__FLOAT, 
                                         "Latitude of native north pole" );
      } else if( !EQUALANG( val, primlt ) ){
         ok = 0;
         goto next;
      }

/* Longitude of native north pole. This is NOT required, so do not pass on 
   if it is not available. If lonpole is provided for a secondary axis, it 
   must be the same as the primary axis value. If it is not, pass on to
   the next Frame. */
      val = GetItem( &(store->lonpole), 0, 0, s, NULL, method, class );
      if( s == ' ' ) {
         primln = val;
         if( val != AST__BAD ) SetValue( this, "LONGPOLE", &val, AST__FLOAT, 
                                         "Longitude of native north pole" );
      } else if( !EQUALANG( val, primln ) ){
         ok = 0;
         goto next;
      }

/* Date of observation. This is NOT required, so do not pass on if it is 
   not available. If mjd-obs is provided for a secondary axis, it must be 
   the same as the primary axis value. If it is not, pass on to the next 
   Frame. */
      val = GetItem( &(store->mjdobs), 0, 0, s, NULL, method, class );
      if( s == ' ' ) {
         primdt = val;
         if( val != AST__BAD ) {
            SetValue( this, "MJD-OBS", &val, AST__FLOAT, 
                      "Modified Julian Date of observation" );

/* The format used for the DATE-OBS keyword depends on the value of the
   keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
   Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format. */
            slaCaldj( 99, 1, 1, &mjd99, &jj );
            if( val < mjd99 ) {
               slaDjcal( 0, val, iymdf, &jj );
               sprintf( combuf, "%2.2d/%2.2d/%2.2d", iymdf[ 2 ], iymdf[ 1 ], 
                        iymdf[ 0 ] - ( ( iymdf[ 0 ] > 1999 ) ? 2000 : 1900 ) ); 
      
            } else {
               slaDjcl( val, iymdf, iymdf+1, iymdf+2, &fd, &jj );
               slaDd2tf( 3, fd, sign, ihmsf );
               sprintf( combuf, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3dZ",
                        iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
                        ihmsf[2], ihmsf[3] ); 
            }

/* Now store the formatted string in the FitsChan. */
            cval = combuf;
            SetValue( this, "DATE-OBS", &cval, AST__STRING,
                      "Date of observation" );
         }

      } else if( !EQUAL( val, primdt ) ){
         ok = 0;
         goto next;
      }

/* If both longitude and latitude axes are present ...*/
      if( axlon >= 0 && axlat >= 0 ) {

/* Get the CTYPE values for the latitude axis. */
         cval = GetItemC( &(store->ctype), axlat, s, NULL, method, class );

/* Extract the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. */
         prj = ( cval ) ? astWcsPrjType( cval + 4 ) : AST__WCSBAD;

/* Projection parameters. If provided for a secondary axis, they must be 
   the same as the primary axis value. If it is not, pass on to the next 
   Frame. PC encoding only allows parameters to be associated with the
   latitude axis. Pass on if any other axes have any projection
   parameters. The old PC TAN projection did not have any parameters.
   Pass on if a TAN projection with parameters is found.  The number of
   parameters was limited to 10. Pass on if more than 10 are supplied. */
         maxm = GetMaxIM( &(store->pv) );
         for( j = 0; j < naxis; j++ ){
            for( m = 0; m <= maxm; m++ ){
               val = GetItem( &(store->pv), j, m, s, NULL, method, class );
               if( s == ' ' ){
                  if( val != AST__BAD ) {
                     if( j != axlat || prj == AST__TAN || m >= 10 ){
                        ok = 0;
                        goto next;
                     } else {
                        SetValue( this, FormatKey( "PROJP", m, -1, ' ' ), &val, 
                                  AST__FLOAT, "Projection parameter" );
                     }
                  } 

                  if( j == axlat && m < 10 ) primpv[m] = val;

               } else {
                  if( ( ( j != axlat || m >= 10 ) && val != AST__BAD ) ||
                      ( j == axlat && m < 10 && !EQUAL( val, primpv[m] ) ) ){
                     ok = 0;
                     goto next;
                  }
               }
            }
         }
      }

/* See if a Frame was sucessfully written to the FitsChan. */
next:
      ok = ok && astOK;

/* If so, indicate we have something to return. */
      if( ok ) ret = 1;

/* Clear any error status so we can continue to produce the next Frame.
   Retain the error if the primary axes could not be produced. After the 
   primary axes, do the A axes. */
      if( s != ' ' ) {
         astClearStatus;
      } else {
         s = 'A' - 1;
      }

/* Remove the secondary "new" flags from the FitsChan. This flag is
   associated with cards which have been added to the FitsChan during
   this pass through the main loop in this function. If the Frame was
   written out succesfully, just clear the flags. If anything went wrong
   with this Frame, remove the flagged cards from the FitsChan. */
      FixNew( this, NEW2, !ok, method, class );

/* Set the current card so that it points to the last WCS-related keyword
   in the FitsChan (whether previously read or not). */
      FindWcs( this, method, class );
   }

/* Annul the array holding the primary PC matrix. */
   primpc = (double *) astFree( (void *) primpc );

/* Return zero or ret depending on whether an error has occurred. */
   return astOK ? ret : 0;
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

/* Read and store string values from the value string. */
      } else if( type == AST__CONTINUE ){
         astFitsSetCN( this, name, value, comment, overwrite );

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
*     A pointer to the new Object. This will always be a FrameSet.

*  Notes:
*     -  The pixel Frame is given a title of "Pixel Coordinates", and
*     each axis in the pixel Frame is given a label of the form "Pixel
*     axis <n>", where <n> is the axis index (starting at one).
*     -  The FITS CTYPE keyword values are used to set the labels for any
*     non-celestial axes in the physical coordinate Frames, and the FITS 
*     CUNIT keywords are used to set the corresponding units strings.
*     -  On exit, the pixel Frame is the base Frame, and the physical
*     Frame derived from the primary axis descriptions is the current Frame.
*     -  Extra Frames are added to hold any secondary axis descriptions. All
*     axes within such a Frame refer to the same coordinate version ('A',
*     'B', etc).
*     -  For foreign encodings, the first card in the FitsChan must be 
*     the current card on entry (otherwise a NULL pointer is returned),
*     and the FitsChan is left at end-of-file on exit.
*     -  For the Native encoding, reading commences from the current card 
*     on entry (which need not be the first in the FitsChan), and the 
*     current Card on exit is the first card following the last one read
*     (or end-of-file).

*/

/* Local Variables: */
   AstObject *new;               /* Pointer to returned Object */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   FitsStore *store;             /* Intermediate storage for WCS information */
   const char *method;           /* Pointer to string holding calling method */
   const char *class;            /* Pointer to string holding object class */
   int encoding;                 /* The encoding scheme */
   int remove;                   /* Remove used cards? */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Store the calling method, and object class. */
   method = "astRead";
   class = astGetClass( this );

/* Get the encoding scheme used by the FitsChan. */
   encoding = astGetEncoding( this );

/* If we are reading from a FitsChan in which AST objects are encoded using
   native AST-specific keywords, use the Read method inherited from the
   Channel class. */
   if( encoding == NATIVE_ENCODING ){
      new = (*parent_read)( this_channel );

/* Indicate that used cards should be removed from the FitsChan. */
      remove = 1; 

/* If we are reading from a FitsChan in which AST objects are encoded using
   any of the other supported encodings, the header may only contain a 
   single FrameSet. */
   } else {
      remove = 0;

/* Only proceed if the FitsChan is at start-of-file. */
      if( !astTestCard( this ) && astOK ){ 

/* Extract the required information from the FITS header into a standard
   intermediary structure called a FitsStore. */
         store = FitsToStore( this, encoding, method, class );

/* Now create a FrameSet from this FitsStore. */
         new = FsetFromStore( this, store, method, class );

/* Release the resources used by the FitsStore. */
         store = FreeStore( store );      

/* Indicate that used cards should be retained in the FitsChan. */
         remove = 0;

/* If no object is being returned, rewind the fitschan in order to
   re-instate the original current Card. */
         if( !new ) {
            astClearCard( this );

/*  Otherwise, ensure the current card is at "end-of-file". */
         } else {
            astSetCard( this, INT_MAX );
         }
      }
   }

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* If no object is being returned, clear the "provisionally used" flags 
   associated with cards which were read. */
   if( !new ) {
      FixUsed( this, 0, 0, method, class );

/*  Otherwise, indicate that all the "provisionally used" cards have been 
    "definitely used". If native encoding was used, these cards are
    totally removed from the FitsChan. */
   } else {
      FixUsed( this, 1, remove, method, class );
   }      

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
   int warn;                     /* Offset of Warnings string */

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

      } else if( !Ustrncmp( setting + ival, FITSPC_STRING, nc ) ){
         astSetEncoding( this, FITSPC_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSPC_STRING2, nc ) ){
         astSetEncoding( this, FITSPC_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSWCS_STRING, nc ) ){
         astSetEncoding( this, FITSWCS_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSWCS_STRING2, nc ) ){
         astSetEncoding( this, FITSWCS_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSIRAF_STRING, nc ) ){
         astSetEncoding( this, FITSIRAF_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSIRAF_STRING2, nc ) ){
         astSetEncoding( this, FITSIRAF_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSAIPS_STRING, nc ) ){
         astSetEncoding( this, FITSAIPS_ENCODING );

      } else if( !Ustrncmp( setting + ival, FITSAIPS_STRING2, nc ) ){
         astSetEncoding( this, FITSAIPS_ENCODING );

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

/* DefB1950 */
/* -------- */
   } else if ( nc = 0,
        ( 1 == sscanf( setting, "defb1950= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetDefB1950( this, ival );

/* Warnings. */
/* -------- */
   } else if ( nc = 0,
               ( 0 == sscanf( setting, "warnings=%n%*[^\n]%n", &warn, &nc ) )
               && ( nc >= len ) ) {
      astSetWarnings( this, setting + warn );

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == sscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "ncard" ) ||
               MATCH( "allwarnings" ) ){
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

static void SetItem( double ****item, int j, int im, char s, double val ){
/*
*  Name:
*     SetItem

*  Purpose:
*     Store a value for a axis keyword value in a FitStore structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void SetItem( double ****item, int j, int im, char s, double val )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The supplied keyword value is stored in the specified array,
*     at a position indicated by the axis and co-ordinate version.
*     The array is created or extended as necessary to make room for 
*     the new value. Any old value is over-written.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->crval) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates an
*        array of axis keyword values. These arrays of keyword values have 
*        one element for every pixel axis (i) or projection parameter (m). 
*     j
*        The zero based intermediate axis index in the range 0 to 98. Set 
*        this to zero for keywords (e.g. CRPIX) which are not indexed by 
*        intermediate axis number.
*     im
*        The zero based pixel axis index (in the range 0 to 98) or parameter 
*        index (in the range 0 to 99). Set this to zero for keywords (e.g. 
*        CRVAL) which are not indexed by either pixel axis or parameter 
*        number.
*     val
*        The keyword value to store.

*/

/* Local Variables: */
   int el;               /* Array index */
   int nel;              /* Number of elements in array */
   int si;               /* Integer co-ordinate version index */

/* Check the inherited status. */
   if( !astOK ) return;

/* Convert the character co-ordinate version into an integer index, and
   check it is within range. The primary axis description (s=' ') is
   given index zero. 'A' is 1, 'B' is 2, etc. */
   if( s == ' ' ) {
      si = 0;
   } else if( islower(s) ){
      si = (int) ( s - 'a' ) + 1;
   } else {
      si = (int) ( s - 'A' ) + 1;
   }

   if( si < 0 || si > 26 ) {
      astError( AST__INTER, "SetItem(fitschan): AST internal error; "
                "co-ordinate version '%c' ( char(%d) ) is invalid.", s, s );

/* Check the intermediate axis index is within range. */
   } else if( j < 0 || j > 98 ) {
      astError( AST__INTER, "SetItem(fitschan): AST internal error; "
                "intermediate axis index %d is invalid.", j );

/* Check the pixel axis or parameter index is within range. */
   } else if( im < 0 || im > 99 ) {
      astError( AST__INTER, "SetItem(fitschan): AST internal error; "
                "pixel axis or parameter index %d is invalid.", j );

/* Otherwise proceed... */
   } else {

/* Store the current number of coordinate versions in the supplied array */
      nel = astSizeOf( (void *) *item )/sizeof(double **);

/* If required, extend the array located by the supplied pointer so that
   it is long enough to hold the specified co-ordinate version. */ 
      if( nel < si + 1 ){
         *item = (double ***) astGrow( (void *) *item, si + 1, 
                                      sizeof(double **) ); 

/* Check the pointer can be used. */
         if( astOK ){

/* Initialise the new elements to hold NULL. Note, astGrow may add more
   elements to the array than is actually needed, so use the actual current
   size of the array as implied by astSize rather than the index si. */
            for( el = nel; 
                 el < astSizeOf( (void *) *item )/sizeof(double **);
                 el++ ) (*item)[el] = NULL;
         }
      }

/* If the above went OK... */
      if( astOK ){

/* Store the currrent number of intermediate axes in the supplied array */
         nel = astSizeOf( (void *) (*item)[si] )/sizeof(double *);

/* If required, extend the array so that it is long enough to hold the 
   specified intermediate axis. */ 
         if( nel < j + 1 ){
            (*item)[si] = (double **) astGrow( (void *) (*item)[si], j + 1, 
                                      sizeof(double *) ); 

/* Check the pointer can be used. */
            if( astOK ){

/* Initialise the new elements to hold NULL. */
               for( el = nel; 
                    el < astSizeOf( (void *) (*item)[si] )/sizeof(double *);
                    el++ ) (*item)[si][el] = NULL;
            }
         }

/* If the above went OK... */
         if( astOK ){

/* Store the current number of pixel axis or parameter values in the array. */
            nel = astSizeOf( (void *) (*item)[si][j] )/sizeof(double);

/* If required, extend the array so that it is long enough to hold the 
   specified axis. */ 
            if( nel < im + 1 ){
               (*item)[si][j] = (double *) astGrow( (void *) (*item)[si][j], 
                                                    im + 1, sizeof(double) ); 

/* Check the pointer can be used. */
               if( astOK ){

/* Initialise the new elements to hold AST__BAD. */
                  for( el = nel; 
                       el < astSizeOf( (void *) (*item)[si][j] )/sizeof(double);
                       el++ ) (*item)[si][j][el] = AST__BAD;
               }
            }

/* If the above went OK, store the supplied keyword value. */
            if( astOK ) (*item)[si][j][im] = val;
         }
      }
   }
}

static void SetItemC( char ****item, int j, char s, char *val ){
/*
*  Name:
*     SetItem

*  Purpose:
*     Store a character string for an axis keyword value in a FitStore 
*     structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void SetItemC( char ****item, int j, char s, char *val )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     The supplied keyword string value is stored in the specified array,
*     at a position indicated by the axis and co-ordinate version.
*     The array is created or extended as necessary to make room for 
*     the new value. Any old value is over-written.

*  Parameters:
*     item
*        The address of the pointer within the FitsStore which locates the 
*        arrays of values for the required keyword (eg &(store->ctype) ).
*        The array located by the supplied pointer contains a vector of
*        pointers. Each of these pointers is associated with a particular
*        co-ordinate version (s), and locates an array of pointers for that 
*        co-ordinate version. Each such array of pointers has an element
*        for each intermediate axis number (j), and the pointer locates a
*        character string. 
*     j
*        The zero based intermediate axis index in the range 0 to 98. Set 
*        this to zero for keywords (e.g. RADESYS) which are not indexed by 
*        intermediate axis number.
*     val
*        The keyword string value to store. A copy of the supplied string
*        is taken.

*/

/* Local Variables: */
   int el;               /* Array index */
   int nel;              /* Number of elements in array */
   int si;               /* Integer co-ordinate version index */

/* Check the inherited status. Also return if a null pointer was supplied */
   if( !astOK || !val ) return;

/* Convert the character co-ordinate version into an integer index, and
   check it is within range. The primary axis description (s=' ') is
   given index zero. 'A' is 1, 'B' is 2, etc. */
   if( s == ' ' ) {
      si = 0;
   } else if( islower(s) ){
      si = (int) ( s - 'a' ) + 1;
   } else {
      si = (int) ( s - 'A' ) + 1;
   }

   if( si < 0 || si > 26 ) {
      astError( AST__INTER, "SetItemC(fitschan): AST internal error; "
                "co-ordinate version '%c' ( char(%d) ) is invalid.", s, s );

/* Check the intermediate axis index is within range. */
   } else if( j < 0 || j > 98 ) {
      astError( AST__INTER, "SetItemC(fitschan): AST internal error; "
                "intermediate axis index %d is invalid.", j );

/* Otherwise proceed... */
   } else {

/* Store the current number of coordinate versions in the supplied array */
      nel = astSizeOf( (void *) *item )/sizeof(char **);

/* If required, extend the array located by the supplied pointer so that
   it is long enough to hold the specified co-ordinate version. */ 
      if( nel < si + 1 ){
         *item = (char ***) astGrow( (void *) *item, si + 1, 
                                      sizeof(char **) ); 

/* Check the pointer can be used. */
         if( astOK ){

/* Initialise the new elements to hold NULL. */
            for( el = nel; 
                 el < astSizeOf( (void *) *item )/sizeof(char **);
                 el++ ) (*item)[el] = NULL;
         }
      }

/* If the above went OK... */
      if( astOK ){

/* Store the currrent number of intermediate axes in the supplied array */
         nel = astSizeOf( (void *) (*item)[si] )/sizeof(char *);

/* If required, extend the array so that it is long enough to hold the 
   specified intermediate axis. */ 
         if( nel < j + 1 ){
            (*item)[si] = (char **) astGrow( (void *) (*item)[si], j + 1, 
                                      sizeof(char *) ); 

/* Check the pointer can be used. */
            if( astOK ){

/* Initialise the new elements to hold NULL. */
               for( el = nel; 
                    el < astSizeOf( (void *) (*item)[si] )/sizeof(char *);
                    el++ ) (*item)[si][el] = NULL;
            }
         }

/* If the above went OK... */
         if( astOK ){

/* Store a copy of the supplied string, using any pre-allocated memory. */
            (*item)[si][j] = (char *) astStore( (void *) (*item)[si][j],
                                                (void *) val, 
                                                strlen( val ) + 1 );
         }
      }
   }
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
*     -  If the keyword has a value of AST__BAD then nothing is stored,
*     and an error is reported.

*/

/* Local Variables: */
   FitsCard *card;        /* Pointer to original current card */
   const char *class;     /* Class name to include in error messages */
   const char *method;    /* Method name to include in error messages */
   int newcard;           /* Has the original current card been deleted? */
   int old_ignoreused;    /* Original setting of external IgnoreUsed variable */
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

/* Report an error if a bad value is stored for a keyword. */
      if( type == AST__FLOAT ){
         if( *( (double *) value ) == AST__BAD && astOK ) {
            astError( AST__BDFTS, "%s(%s): The required FITS keyword "
                      "\"%s\" is indeterminate.", method, class, keyname );
         }
      }

/* Save a pointer to the current card. */
      card = (FitsCard *) this->card;

/* Indicate that we should not skip over cards marked as having been
   read. */
      old_ignoreused = IgnoreUsed;
      IgnoreUsed = 0;

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
      IgnoreUsed = old_ignoreused;
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

static int SkySys( AstSkyFrame *skyfrm, int wcstype, FitsStore *store,
                   int axlon, int axlat, char s, const char *method, 
                   const char *class ){
/*
*  Name:
*     SkySys

*  Purpose:
*     Return FITS-WCS values describing a sky coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int SkySys( AstSkyFrame *skyfrm, int wcstype, FitsStore *store,
*                 int axlon, int axlat, char s, const char *method, 
*                 const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     This function sets values for the following FITS-WCS keywords
*     within the supplied FitsStore structure: CTYPE, RADECSYS, EQUINOX,
*     MJDOBS, CUNIT. The values are derived from the supplied SkyFrame
*     and WcsMap.

*  Parameters:
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
*     s
*        Co-ordinate version character.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     Are the keywords values in the FitsStore usable?

*/

/* Local Variables: */
   char lattype[MXCTYPELEN];/* Latitude axis CTYPE value */
   char lontype[MXCTYPELEN];/* Longitude axis CTYPE value */
   const char *prj_name;    /* Pointer to projection name string */
   const char *sys;         /* Celestal coordinate system */
   const char *latsym;      /* SkyFrame latitude axis symbol */
   const char *lonsym;      /* SkyFrame longitude axis symbol */
   double ep;               /* Epoch of observation (MJD) */
   double eq;               /* Epoch of reference equinox (MJD) */
   int defdate;             /* Can the date keywords be defaulted? */
   int i;                   /* Character count */
   int isys;                /* Celestial coordinate system */
   int radesys;             /* RA/DEC reference frame */
   int ret;                 /* Returned flag */
   int latax;               /* Index of latitude axis in SkyFrame */
   int lonax;               /* Index of longitude axis in SkyFrame */

/* Check the status. */
   if( !astOK ) return 0;

/* Check we have a SkyFrame. */
   if( !astIsASkyFrame( skyfrm ) ) return 0;

/* Initialise */
   ret = 1;

/* Get the equinox, epoch of observation, and system of the SkyFrame. */
   eq = astGetEquinox( skyfrm );               
   ep = astTestEpoch( skyfrm ) ? astGetEpoch( skyfrm ) : AST__BAD;
   sys = astGetC( skyfrm, "system" );

/* The MJD-OBS and DATE-OBS keywords default to the epoch of the
   reference equinox if not supplied. Therefore MJD-OBS and DATE-OBS do
   not need to be stored in the FitsChan if the epoch of observation is 
   the same as the epoch of the reference equinox. This can avoid 
   producing FITS headers which say unlikely things like
   DATE-OBS = "01/01/50". Set a flag indicating if MJD-OBS and DATE-OBS
   can be defaulted. */
   defdate = EQUAL( ep, eq );

/* Convert the equinox to a Julian or Besselian epoch. Also get the
   reference frame and standard system. */
   if( !Ustrcmp( sys, "FK4") ){
      eq = slaEpb( eq );
      radesys = FK4;
      isys = RADEC;
      SetItemC( &(store->radesys), 0, s, "FK4" );
      
   } else if( !Ustrcmp( sys, "FK4_NO_E") ){
      eq = slaEpb( eq );
      radesys = FK4NOE;
      isys = RADEC;
      SetItemC( &(store->radesys), 0, s, "FK4_NO_E" );

   } else if( !Ustrcmp( sys, "FK5" ) ){
      eq = slaEpj( eq );
      radesys = FK5;
      isys = RADEC;
      SetItemC( &(store->radesys), 0, s, "FK5" );

   } else if( !Ustrcmp( sys, "ICRS" ) ){
      eq = slaEpj( eq );
      radesys = ICRS;
      isys = RADEC;
      SetItemC( &(store->radesys), 0, s, "ICRS" );

   } else if( !Ustrcmp( sys, "GAPPT" ) ||
              !Ustrcmp( sys, "Apparent" ) ||
              !Ustrcmp( sys, "Geocentric" ) ){
      eq = AST__BAD;
      radesys = GAPPT;
      isys = RADEC;
      SetItemC( &(store->radesys), 0, s, "GAPPT" );

   } else if( !Ustrcmp( sys, "Ecliptic" ) ){
      eq = ( eq < slaEpj2d( 1984.0 ) ) ? slaEpb( eq ) : slaEpj( eq );
      radesys = NORADEC;
      isys = ECLIP;

   } else if( !Ustrcmp( sys, "Galactic" ) ){
      eq = AST__BAD;
      radesys = NORADEC;
      isys = GALAC;

   } else if( !Ustrcmp( sys, "Supergalactic" ) ){
      eq = AST__BAD;
      radesys = NORADEC;
      isys = SUPER;

   } else {
      eq = AST__BAD;
      radesys = NORADEC;
      isys = NOCEL;
   }

/* Store these values. Only store the date if it does not take its
   default value. */
   SetItem( &(store->equinox), 0, 0, s, eq );
   if( !defdate ) SetItem( &(store->mjdobs), 0, 0, s, ep );

/* Only proceed if we have usable values */
   if( astOK ) {

/* The first 4 characters in CTYPE are determined by the celestial coordinate 
   system and the second 4 by the projection type. */
      if( isys == RADEC ){
         strcpy( lontype, "RA--" );
         strcpy( lattype, "DEC-" );

      } else if( isys == ECLIP ){
         strcpy( lontype, "ELON" );
         strcpy( lattype, "ELAT" );

      } else if( isys == GALAC ){
         strcpy( lontype, "GLON" );
         strcpy( lattype, "GLAT" );

      } else if( isys == SUPER ){
         strcpy( lontype, "SLON" );
         strcpy( lattype, "SLAT" );

/* For unknown systems, use the axis symbols within CTYPE */
      } else {

         latax = astGetLatAxis( skyfrm );         
         lonax = 2 - latax;

         latsym = astGetSymbol( skyfrm, latax );
         lonsym = astGetSymbol( skyfrm, lonax );

         if( astOK ) { 
            strncpy( lontype, lonsym, 4 );
            for( i = strlen( lonsym ); i < 4; i++ ) {
               lontype[ i ] = '-';
            }

            strncpy( lattype, latsym, 4 );
            for( i = strlen( latsym ); i < 4; i++ ) {
               lattype[ i ] = '-';
            }
         }
      }                  

/* Store the projection strings. */
      prj_name = astWcsPrjName( wcstype );
      if( astOK ) {
         strcpy( lontype + 4, prj_name );
         strcpy( lattype + 4, prj_name );
      }

/* Store the total CTYPE strings */
      SetItemC( &(store->ctype), axlon, s, lontype );
      SetItemC( &(store->ctype), axlat, s, lattype );

/* Nullify any CUNITS strings for the longitude and latitude axes (they
   always take the default value of degrees). */
      SetItemC( &(store->cunit), axlat, s, NULL );
      SetItemC( &(store->cunit), axlon, s, NULL );

   }

/* Store the Domain name as the WCSNAME keyword (if set). */
   if( astTestDomain( skyfrm ) ) { 
      SetItemC( &(store->wcsname), 0, s, (char *) astGetDomain( skyfrm ) );
   }

   if( !astOK ) ret = 0;
   return ret;
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

static AstFitsChan *SpecTrans( AstFitsChan *this, int encoding, 
                               const char *method, const char *class ){
/*
*  Name:
*     SpecTrans

*  Purpose:
*     Translated non-standard WCS FITS headers into equivalent standard
*     ones.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     AstFitsChan *SpecTrans( AstFitsChan *this, int encoding, 
*                             const char *method, const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function checks the supplied FitsChan for selected
*     non-standard WCS keywords and, if found, stores equivalent 
*     standard keywords in a newly created FitsChan which is returned as
*     the function value. All the original keywords are marked
*     as having been used, so that they are not written out when the 
*     FitsChan is deleted. 
*
*     At the moment, the non-standard keywords checked for are:
*
*     1) RADECSYS is renamed as RADESYS
*
*     2) LONGPOLE is renamed as LONPOLE
*
*     3) CDjjjiii is renamed as CDj_i
*
*     4) PCjjjiii, CDELTj and CROTAj are converted to CDj_i
*
*     5) PROJPi are converted to PV<axlat>_i
*   
*     6) CmVALi are converted to CRVALis (s=A,B,,, for m=1,2...). This
*        is also done for CmPIXi, CmYPEi, and CmNITi. CmELTi is converted
*        to a CDj_is array.
*
*     7) EQUINOX keywords with string values equal to a date preceeded
*        by the leter B or J (eg "B1995.0"). These are converted to the
*        corresponding Julian floating point value without any epoch
*        specifier.
*
*     8) EPOCH values are converted into Julian EQUINOX values (but only
*        if the FitsChan does not already contain an EQUINOX value).
*
*     9) DATE-OBS values are converted into MJD-OBS values (but only
*        if the FitsChan does not already contain an MJD-OBS value).
*
*     10) EQUINOX or EPOCH keywords with value zero  are converted to 
*         B1950. 
*     
*     11) The AIPS NCP projection is converted into an equivalent SIN
*         projection.
*
*     12) The IRAF "ZPX" projection. If the last 4 chacaters of CTYPEi 
*       (i = 1, naxis) are "-ZPX", then:
*	- "ZPX" is replaced by "ZPN" within the CTYPEi value
*       - If the FitsChan contains no PROJP keywords, then projection
*       parameter valus are read from any WATi_nnn keywords, and
*       corresponding PV keywords are added to the FitsChan.
*       - The WATi_nnn keywords may specify corrections to the basic ZPN
*       projection by including "lngcor" or "latcor" terms. There is no
*       direct equivalent in FITS-PC to these terms and so they are
*       ignored (it may be possible to use a pixel correction image but
*       such images are not supported by AST anyway). If these correction
*       terms are found ASTWARN keywords are added to the FitsChan 
*       containing a warning message. The calling application can (if it
*       wants to) check for this keyword, and report its contents to the
*       user.
*
*     13) The IRAF "TNX" projection. If the last 4 chacaters of CTYPEi 
*       (i = 1, naxis) are "-TNX", then:
*	- "TNX" is replaced by "TAN" within the CTYPEi value
*       - If the FitsChan contains no PROJP keywords, then projection
*       parameter valus are read from any WATi_nnn keywords, and
*       corresponding PV keywords are added to the FitsChan.
*       - If the TNX projection cannot be converted exactly into a TAN 
*       projection, ASTWARN keywords are added to the FitsChan 
*       containing a warning message. The calling application can (if it
*       wants to) check for this keyword, and report its contents to the
*       user.
*
*     14) Keywords relating to the IRAF "mini-WCS" system are removed.
*       This is the IRAF equivalent of the AST native encoding. Mini-WCS
*       keywords are removed in order to avoid confusion arising between
*       potentially inconsistent encodings.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     encoding
*        The FitsChan encoding in use.
*     method
*        Pointer to string holding name of calling method.
*     class 
*        Pointer to a string holding the name of the supplied object class.

*  Returned Value:
*     A pointer to the new FitsChan containing the keywords which
*     constitute the standard equivalents to any non-standard keywords in 
*     the supplied FitsChan. A NULL pointer is returned if there are no
*     non-standard keywords in the supplied FitsChan.

*/

/* Local Variables: */
   AstFitsChan *ret;              /* The returned FitsChan */
   char *cval;                    /* Pointer to character string */
   char *start;                   /* Pointer to start of projp term */
   char *wat;                     /* Pointer to a single WAT string */
   char *watmem;                  /* Pointer to total WAT string */
   char bj;                       /* Besselian/Julian indicator */
   char format[ 50 ];             /* scanf format string */
   char keyname[ FITSNAMLEN + 1 ];/* General Keyword name */
   char lattype[MXCTYPELEN];      /* CTYPE value for latitude axis */
   char lontype[MXCTYPELEN];      /* CTYPE value for longitude axis */
   char prj[6];                   /* Projection string */
   char s;                        /* Co-ordinate version character */
   double cdelti;                 /* CDELT for longitude axis */
   double cdeltj;                 /* CDELT for latitude axis */
   double cosrota;                /* Cos( CROTA ) */
   double crota;                  /* CROTA Value */
   double dval;                   /* General floating value */
   double projp;                  /* Projection parameter value */
   double sinrota;                /* Sin( CROTA ) */
   int *mp;                       /* Pointer to next projection parameter index */
   int axlat;                     /* Index of latitude axis */
   int axlon;                     /* Index of longitude axis */
   int i,j;                       /* Indices */
   int iaxis;                     /* Axis index */
   int iproj;                     /* Projection parameter index */
   int lbnd[ 2 ];                 /* Lower index bounds */
   int m;                         /* Co-ordinate version index */
   int naxis;                     /* Number of axes */
   int nch;                       /* No. of characters read */
   int ok;                        /* Can projection be represented in FITS-WCS?*/
   int porder;                    /* Order of polynomial */
   int ubnd[ 2 ];                 /* Upper index bounds */
   int watlen;                    /* Length of total WAT string (inc. term null)*/
   size_t size;                   /* Length of string value */

/* Arrays needed to convert the index of a TNX co-efficient into an index
   of a TAN projection parameter. */
   static int abskip[] = {0,1,4,10,20,35,56,84};
   static int nab[] = {1,3,6,10,15,21,28,36};
   static int a[] = { 
0,  
0,1,2,
0,1,4,2,5,6,
0,1,4,7,2,5,8,6,9,10,
0,1,4,7,12,2,5,8,13,6,9,14,10,15,16,
0,1,4,7,12,17,2,5,8,13,18,6,9,14,19,10,15,20,16,21,22,
0,1,4,7,12,17,24,2,5,8,13,18,25,6,9,14,19,26,10,15,20,27,16,21,28,22,29,30,
0,1,4,7,12,17,24,31,2,5,8,13,18,25,32,6,9,14,19,26,33,10,15,20,27,34,16,21,28,35,22,29,36,30,37,38};

   static int b[] = { 
0, 
0,2,1,
0,2,6,1,5,4,
0,2,6,10,1,5,9,4,8,7,
0,2,6,10,16,1,5,9,15,4,8,14,7,13,12,
0,2,6,10,16,22,1,5,9,15,21,4,8,14,20,7,13,19,12,18,17,
0,2,6,10,16,22,30,1,5,9,15,21,29,4,8,14,20,28,7,13,19,27,12,18,26,17,25,24,
0,2,6,10,16,22,30,38,1,5,9,15,21,29,37,4,8,14,20,28,36,7,13,19,27,35,12,18,26,34,17,25,33,24,32,31};

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Find the number of axes by finding the highest axis number in any 
   CRPIXi keyword name. Return if there are no axes. */
   if( !astKeyFields( this, "CRPIX%d", 1, &naxis, lbnd ) ) return NULL;

/* Create the returned FitsChan. */
   ret = astFitsChan( NULL, NULL, "" );

/* Find the longitude and latitude axes by examining the CTYPE values.
   They are marked as read. Such markings are only provisional, and they
   can be read again any number of times until the current astRead
   operation is completed. Also note the projection type. */
   j = 0;
   axlon = -1;
   axlat = -1;
   while( j < naxis && astOK ){
      if( GetValue( this, FormatKey( "CTYPE", j + 1, -1, ' ' ),
                    AST__STRING, (void *) &cval, 0, method, 
                    class ) ){
         if( !strncmp( cval, "RA--", 4 ) ||
             !strncmp( cval + 1, "LON", 3 ) ||
             !strncmp( cval + 2, "LN", 2 ) ) {
            axlon = j;
            strncpy( prj, cval + 4, 5 );
            strncpy( lontype, cval, 10 );

         } else if( !strncmp( cval, "DEC-", 4 ) ||
             !strncmp( cval + 1, "LAT", 3 ) ||
             !strncmp( cval + 2, "LT", 2 ) ) {
            axlat = j;
            strncpy( prj, cval + 4, 5 );
            strncpy( lattype, cval, 10 );
         }
         j++;
      } else {
         break;
      }
   }

/* RADECSYS keywords 
   ----------------- */
   if( GetValue( this, "RADECSYS", AST__STRING, (void *) &cval, 0, method, 
                 class ) ){
      if( encoding == FITSPC_ENCODING || encoding == FITSIRAF_ENCODING ){
         SetValue( ret, "RADESYS", (void *) &cval, AST__STRING, 
                   CardComm( this ) );
      }
   }

/* LONGPOLE keywords 
   ----------------- */
   if( GetValue( this, "LONGPOLE", AST__FLOAT, (void *) &dval, 0, method, 
                 class ) ){
      if( encoding == FITSPC_ENCODING || encoding == FITSIRAF_ENCODING ){
         SetValue( ret, "LONPOLE", (void *) &dval, AST__FLOAT, 
                   CardComm( this ) );
      }
   }

/* CDjjjiii 
   -------- */
   if( astKeyFields( this, "CD%3d%3d", 0, NULL, NULL ) ){

/* Do each row in the matrix. */
      for( j = 0; j < naxis; j++ ){

/* Do each column in the matrix. */
         for( i = 0; i < naxis; i++ ){

/* Get the CDjjjiii matrix element */
            sprintf( keyname, "CD%.3d%.3d", j + 1, i + 1 );
            if( GetValue( this, keyname, AST__FLOAT, (void *) &dval, 0, 
                          method, class ) ){

/* If found, save it with name CDj_i */
               if( encoding == FITSIRAF_ENCODING ){
                  SetValue( ret, FormatKey( "CD", j + 1, i + 1, ' ' ),
                            (void *) &dval, AST__FLOAT, NULL );
               }
            }
         }
      }
   }

/* PCjjjiii, CROTAi and CDELTj keywords 
   ------------------------------------ */

/* Check there are some CDELT keywords... */
   if( astKeyFields( this, "CDELT%d", 0, NULL, NULL ) ){

/* See if there is a CROTA keyword. Try to read values for both axes
   since they are sometimes both included. This ensures they will not be
   included in the output when the FitsChan is deleted. Read the latitude
   axis second in order to give it priority in cases where both are
   present. */
      crota = AST__BAD;
      GetValue( this, FormatKey( "CROTA", axlon + 1, -1, ' ' ), 
                 AST__FLOAT, (void *) &crota, 0, method, class );
      GetValue( this, FormatKey( "CROTA", axlat + 1, -1, ' ' ), 
                 AST__FLOAT, (void *) &crota, 0, method, class );

/* If there are any PCjjjiii keywords, create a CDj_i matrix from them. 
   Otherwise, if there is no CROTA keyword, we also create a CDj_i matrix 
   from the (default) PCjjjiii values. */
      if( astKeyFields( this, "PC%3d%3d", 0, NULL, NULL ) || 
          crota == AST__BAD ){

/* Do each row in the matrix. */
         for( j = 0; j < naxis; j++ ){

/* Get the CDELT value for this row. Report an error if not present. */
            GetValue( this, FormatKey( "CDELT", j + 1, -1, ' ' ), AST__FLOAT, 
                      (void *) &cdeltj, 1, method, class );

/* If CDELT is zero, use 1.0E-6 of the corresponding CRVAL value 
   instead, or 1.0 if CRVAL is zero. Otherwise, the zeros could cause the 
   matrix to be non-invertable. The Mapping could then not be simplified 
   or used by a Plot. CDELT values of zero are usually used to indicate 
   "redundant" axes. For instance, a 2D image may be stored as a 3D cube  
   with a single plane with the "redundant" 3rd axis used to specify the 
   wavelength of the filter. The actual value used for CDELT shouldn't 
   matter since the axis only spans a single pixel anyway. */
            if( cdeltj == 0.0 ){
               GetValue( this, FormatKey( "CRVAL", j + 1, -1, ' ' ), AST__FLOAT, 
                         (void *) &dval, 1, method, class );
               cdeltj = 1.0E-6*dval;
               if( cdeltj == 0.0 ) cdeltj = 1.0;
            }

/* Do each column in the matrix. */
            for( i = 0; i < naxis; i++ ){

/* Get the PC matrix element */
               sprintf( keyname, "PC%.3d%.3d", j + 1, i + 1 );
               if( GetValue( this, keyname, AST__FLOAT, (void *) &dval, 0, 
                             method, class ) ){
               } else if( i == j ) {
                  dval = 1.0;
               } else {
                  dval = 0.0;
               }

/* Multiply it by CDELT to get the CD matrix element. */
               dval *= cdeltj;

/* If the CD value can be defaulted, set it bad. */
               if( i == j ) {
                  if( dval == 1.0 ) dval = AST__BAD;
               } else {
                  if( dval == 0.0 ) dval = AST__BAD;
               }

/* If the CD value cannot be defaulted, store it. */
               if( dval != AST__BAD && encoding == FITSPC_ENCODING ){
                  SetValue( ret, FormatKey( "CD", j + 1, i + 1, ' ' ),
                            (void *) &dval, AST__FLOAT, NULL );
               }
            }
         }

/* If there is a CROTA value and no PCjjjii keywords, create a CDj_i
   matrix from the CROTA values and CDELT values. We need to have
   latitude and longitude axes for this.  */
      } else if( axlat != -1 && axlon != -1 ){

/* Get the sin and cos of CROTA */
         cosrota = cos( crota*AST__DD2R );
         sinrota = sin( crota*AST__DD2R );

/* Get the CDELT values for the longitude and latitude axes. */
         if( GetValue( this, FormatKey( "CDELT", axlat + 1, -1, ' ' ),
                       AST__FLOAT, (void *) &cdeltj, 1, method, 
                       class ) && 
             GetValue( this, FormatKey( "CDELT", axlon + 1, -1, ' ' ),
                       AST__FLOAT, (void *) &cdelti, 1, method, 
                       class ) ){

/* Save a corresponding set of CDi_j keywords in the FitsChan. First do
   the diagonal terms. */
            for( i = 0; i < naxis; i++ ){
               if( i == axlat ) {
                  dval = cdeltj*cosrota;
               } else if( i == axlon ) {
                  dval = cdelti*cosrota;
               } else {
                  GetValue( this, FormatKey( "CDELT", i + 1, -1, ' ' ),
                            AST__STRING, (void *) &dval, 1, method, 
                            class );

/* If CDELT is zero, use one hundredth of the corresponding CRVAL value 
   instead, or 1.0 if CRVAL is zero. Otherwise, the zeros could cause the 
   matrix to be non-invertable. The Mapping could then not be simplified 
   or used by a Plot. CDELT values of zero are usually used to indicate 
   "redundant" axes. For instance, a 2D image may be stored as a 3D cube  
   with a single plane with the "redundant" 3rd axis used to specify the 
   wavelength of the filter. The actual value used for CDELT shouldn't 
   matter since the axis only spans a single pixel anyway. */
                  if( dval == 0.0 ){
                     GetValue( this, FormatKey( "CRVAL", i + 1, -1, ' ' ), 
                               AST__FLOAT, (void *) &dval, 1, method,
                               class );
                     dval *= 0.01;
                     if( dval == 0.0 ) dval = 1.0;
                  }
               }

               if( encoding == FITSPC_ENCODING || 
                   encoding == FITSAIPS_ENCODING ){
                  SetValue( ret, FormatKey( "CD", i + 1, i + 1, ' ' ),
                            (void *) &dval, AST__FLOAT, NULL );
               }
            }

/* Now do the non-zero off-diagonal terms. */
            if( encoding == FITSPC_ENCODING || 
                encoding == FITSAIPS_ENCODING ){
               dval = cdelti*sinrota;
               SetValue( ret, FormatKey( "CD", axlat + 1, axlon + 1, ' ' ),
                         (void *) &dval, AST__FLOAT, NULL );
 
               dval = -cdeltj*sinrota;
               SetValue( ret, FormatKey( "CD", axlon + 1, axlat + 1, ' ' ),
                         (void *) &dval, AST__FLOAT, NULL );
            }
         }
      }
   }

/* PROJP keywords
   -------------- */
   if( astKeyFields( this, "PROJP%d", 1, ubnd, lbnd ) && axlat != -1 ){
      for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++ ){
         if( GetValue( this, FormatKey( "PROJP", i, -1, ' ' ), 
                       AST__FLOAT, (void *) &dval, 0, method, class ) &&
             ( encoding == FITSPC_ENCODING || 
               encoding == FITSIRAF_ENCODING ) ){
            SetValue( ret, FormatKey( "PV", axlat + 1, i, ' ' ),
                      (void *) &dval, AST__FLOAT, CardComm( this ) );
         }
      }
   }
   
/* CmVALi keywords 
   --------------- */
   if( astKeyFields( this, "C%1dVAL%d", 2, ubnd, lbnd ) ){
      s = 'A';
      for( m = lbnd[ 0 ]; m <= ubnd[ 0 ]; m++ ){
         for( i = lbnd[ 1 ]; i <= ubnd[ 1 ]; i++ ){
            sprintf( keyname, "C%dVAL%d", m, i );
            if( GetValue( this, keyname, AST__FLOAT, (void *) &dval, 0, 
                          method, class ) && 
                ( encoding == FITSPC_ENCODING || 
                  encoding == FITSIRAF_ENCODING ) ){
               sprintf( keyname, "CRVAL%d%c", i, s );
               SetValue( ret, keyname, (void *) &dval, AST__FLOAT,
                         CardComm( this ) );
            }
         }
         s++;
      }
   }

/* CmPIXi keywords 
   --------------- */
   if( astKeyFields( this, "C%1dPIX%d", 2, ubnd, lbnd ) ){
      s = 'A';
      for( m = lbnd[ 0 ]; m <= ubnd[ 0 ]; m++ ){
         for( i = lbnd[ 1 ]; i <= ubnd[ 1 ]; i++ ){
            sprintf( keyname, "C%dPIX%d", m, i );
            if( GetValue( this, keyname, AST__FLOAT, (void *) &dval, 0, 
                          method, class ) && 
                ( encoding == FITSPC_ENCODING || 
                  encoding == FITSIRAF_ENCODING ) ){
               sprintf( keyname, "CRPIX%d%c", i, s );
               SetValue( ret, keyname, (void *) &dval, AST__FLOAT,
                         CardComm( this ) );
            }
         }
         s++;
      }
   }

/* CmYPEi keywords 
   --------------- */
   if( astKeyFields( this, "C%1dYPE%d", 2, ubnd, lbnd ) ){
      s = 'A';
      for( m = lbnd[ 0 ]; m <= ubnd[ 0 ]; m++ ){
         for( i = lbnd[ 1 ]; i <= ubnd[ 1 ]; i++ ){
            sprintf( keyname, "C%dYPE%d", m, i );
            if( GetValue( this, keyname, AST__STRING, (void *) &cval, 0, 
                          method, class ) && 
                ( encoding == FITSPC_ENCODING || 
                  encoding == FITSIRAF_ENCODING ) ){
               sprintf( keyname, "CTYPE%d%c", i, s );
               SetValue( ret, keyname, (void *) &cval, AST__STRING,
                         CardComm( this ) );
            }
         }
         s++;
      }
   }

/* CmNITi keywords 
   --------------- */
   if( astKeyFields( this, "C%1dNIT%d", 2, ubnd, lbnd ) ){
      s = 'A';
      for( m = lbnd[ 0 ]; m <= ubnd[ 0 ]; m++ ){
         for( i = lbnd[ 1 ]; i <= ubnd[ 1 ]; i++ ){
            sprintf( keyname, "C%dNIT%d", m, i );
            if( GetValue( this, keyname, AST__STRING, (void *) &cval, 0, 
                          method, class ) && 
                ( encoding == FITSPC_ENCODING || 
                  encoding == FITSIRAF_ENCODING ) ){
               sprintf( keyname, "CUNIT%d%c", i, s );
               SetValue( ret, keyname, (void *) &cval, AST__STRING,
                         CardComm( this ) );
            }
         }
         s++;
      }
   }


/* CmELTi keywords 
   --------------- */
   if( astKeyFields( this, "C%1dELT%d", 2, ubnd, lbnd ) ){
      s = 'A';
      for( m = lbnd[ 0 ]; m <= ubnd[ 0 ]; m++ ){

/* Create a CDj_is matrix from the CmELTi and PCjjjiii values. */
/* Do each row in the matrix. */
         for( j = 0; j < naxis; j++ ){

/* Get the CDELT value for this row. Report an error if not present. */
            sprintf( keyname, "C%dELT%d", m, j + 1 );
            GetValue( this, keyname, AST__FLOAT, (void *) &cdeltj, 1, 
                      method, class );

/* If CDELT is zero, use one hundredth of the corresponding CRVAL value 
   instead, or 1.0 if CRVAL is zero. Otherwise, the zeros could cause the 
   matrix to be non-invertable. The Mapping could then not be simplified 
   or used by a Plot. CDELT values of zero are usually used to indicate 
   "redundant" axes. For instance, a 2D image may be stored as a 3D cube  
   with a single plane with the "redundant" 3rd axis used to specify the 
   wavelength of the filter. The actual value used for CDELT shouldn't 
   matter since the axis only spans a single pixel anyway. */
            if( cdeltj == 0.0 ){
               GetValue( this, FormatKey( "CRVAL", j + 1, -1, s ), AST__FLOAT, 
                         (void *) &dval, 1, method, class );
               cdeltj = 0.01*dval;
               if( cdeltj == 0.0 ) cdeltj = 1.0;
            }

/* Do each column in the matrix. */
            for( i = 0; i < naxis; i++ ){

/* Get the PC matrix element */
               sprintf( keyname, "PC%.3d%.3d", j + 1, i + 1 );
               if( GetValue( this, keyname, AST__FLOAT, (void *) &dval, 0, 
                             method, class ) ){
               } else if( i == j ) {
                  dval = 1.0;
               } else {
                  dval = 0.0;
               }

/* Multiply it by CDELT to get the CD matrix element. */
               dval *= cdeltj;

/* If the CD value can be defaulted, set it bad. */
               if( i == j ) {
                  if( dval == 1.0 ) dval = AST__BAD;
               } else {
                  if( dval == 0.0 ) dval = AST__BAD;
               }

/* If the CD value cannot be defaulted, store it. */
               if( dval != AST__BAD && encoding == FITSPC_ENCODING ){
                  SetValue( ret, FormatKey( "CD", j + 1, i + 1, s ),
                            (void *) &dval, AST__FLOAT, NULL );
               }
            }
         }
         s++;
      }
   }

/* EPOCH keywords
   ------------ */
/* Get any EPOCH card, marking it as read. */
   if( GetValue( this, "EPOCH", AST__FLOAT, (void *) &dval, 0, method, 
                    class ) ){

/* Convert values of zero to B1950. */
      if( dval == 0.0 ) dval = 1950.0;

/* Save a new EQUINOX card in the FitsChan, so long as there is not
   already one there. */
      if( !GetValue( this, "EQUINOX", AST__STRING, (void *) &cval, 0, 
                      method, class ) ){
         SetValue( ret, "EQUINOX", (void *) &dval, AST__FLOAT, 
                   "Reference equinox" );
      }
   }

/* String EQUINOX values 
   --------------------- 
   If found, EQUINOX will be used in favour of any EPOCH value found
   above. */
   if( GetValue( this, "EQUINOX", AST__STRING, (void *) &cval, 0, method, 
                 class ) ){

/* Note the first character. */
      bj = cval[ 0 ];      

/* If it is "B" or "J", read a floating value from the rest */
      if( bj == 'B' || bj == 'J' ) {
         if( 1 == sscanf( cval + 1, " %lf ", &dval ) ){

/* If it is a Besselian epoch, convert to Julian. */
            if( bj == 'B' ) dval = slaEpj( slaEpb2d( dval ) );

/* Replace the original EQUINOX card. */
            SetValue( ret, "EQUINOX", (void *) &dval, AST__FLOAT, 
                      CardComm( this ) );
         }
      }
   } 

/* EQUINOX = 0.0 keywords
   ---------------------- */
   if( GetValue( this, "EQUINOX", AST__FLOAT, (void *) &dval, 0, method, 
                 class ) ){
      if( dval == 0.0 ){
         dval = 1950.0;
         SetValue( ret, "EQUINOX", (void *) &dval, AST__FLOAT, 
                   CardComm( this ) );
      }
   }

/* DATE-OBS keywords
   ---------------- */
/* Read any DATE-OBS card. This prevents it being written out when the
   FitsChan is deleted.  */
   if( GetValue( this, "DATE-OBS", AST__STRING, (void *) &cval, 0, method, 
                    class ) ){

/* Ignore DATE-OBS values if the header contains an MJD-OBS value */
      if( !GetValue( this, "MJD-OBS", AST__FLOAT, (void *) &dval, 0, 
                     method, class ) ){

/* Get the corresponding mjd-obs value, checking that DATE-OBS is valid. */
         dval = DateObs( cval );
         if( dval != AST__BAD ){
            SetValue( ret, "MJD-OBS", (void *) &dval, AST__FLOAT, 
                      "Date of observation" );
         }
      }
   }

/* AIPS "NCP" projections 
   --------------------- */

/* Compare the projection type with "-NCP" */
   if( !Ustrcmp( prj, "-NCP" ) ) {

/* Get the latitude reference value, and take is cot. */
      GetValue( this, FormatKey( "CRVAL", axlat + 1, -1, ' ' ),
                AST__FLOAT, (void *) &dval, 1, method, class );

      dval = sin( dval*AST__DD2R );
      if( dval != 0.0 ) {
         dval = cos( dval*AST__DD2R )/dval;

/* Replace NCP with SIN in the CTYPE values. */
         strcpy( lontype + 4, "-SIN" );
         cval = lontype;
         SetValue( ret, FormatKey( "CTYPE", axlon + 1, -1, ' ' ),
                   (void *) &cval, AST__STRING, NULL );
         strcpy( lattype + 4, "-SIN" );
         cval = lattype;
         SetValue( ret, FormatKey( "CTYPE", axlat + 1, -1, ' ' ),
                   (void *) &cval, AST__STRING, NULL );

/* Store the new projection parameters using names suitable to the
   encoding. */
         if( encoding == FITSWCS_ENCODING ){
            SetValue( ret, FormatKey( "PV", axlat + 1, 2, ' ' ),
                      (void *) &dval, AST__FLOAT, NULL );
            dval = 0.0;
            SetValue( ret, FormatKey( "PV", axlat + 1, 1, ' ' ),
                      (void *) &dval, AST__FLOAT, NULL );
         } else {
            SetValue( ret, FormatKey( "PROJP", 2, -1, ' ' ),
                      (void *) &dval, AST__FLOAT, NULL );
            dval = 0.0;
            SetValue( ret, FormatKey( "PROJP", 1, -1, ' ' ),
                      (void *) &dval, AST__FLOAT, NULL );
         }
      }
   }


/* IRAF "ZPX" projections 
   --------------------- */
   if( !Ustrcmp( prj, "-ZPX" ) ) {

/* Replace ZPX with ZPN in the CTYPE values. */
      strcpy( lontype + 4, "-ZPN" );
      cval = lontype;
      SetValue( ret, FormatKey( "CTYPE", axlon + 1, -1, ' ' ),
                (void *) &cval, AST__STRING, NULL );

      strcpy( lattype + 4, "-ZPN" );
      cval = lattype;
      SetValue( ret, FormatKey( "CTYPE", axlat + 1, -1, ' ' ),
                (void *) &cval, AST__STRING, NULL );

/* Check latitude then longitude axes */
      for( i = 0; i < 2; i++ ){
         iaxis = i ? axlat : axlon;

/* Rewind the FitsChan. */
         astClearCard( this );

/* Concatenate all the IRAF "WAT" keywords together for this axis. These 
   keywords are marked as having been used, so that they are not written 
   out when the FitsChan is deleted. */
         watmem = NULL;
         watlen = 1;
         j = 1;
         sprintf( keyname, "WAT%d_%.3d", iaxis + 1, j );
         while( FindKeyCard( this, keyname, method, class ) && astOK ) {
            wat = (char *) CardData( this, &size );
            watmem = (char *) astRealloc( (void *) watmem, 
                                          watlen - 1 + size );
            if( watmem ) {
               strcpy( watmem + watlen - 1, wat );
               watlen += size - 1;
               MarkCard( this );
               MoveCard( this, 1, method, class );
               j++;
               sprintf( keyname, "WAT%d_%.3d", iaxis + 1, j );
            } else {
               break;
            }
         }

/* Search the total WAT string for any projp terms. */
         if( watmem ){
            for( iproj = 0; iproj < 10 && astOK; iproj++ ) {
               sprintf( format, "projp%d=", iproj );
               start = strstr( watmem, format );
               if( start ) {
                  sprintf( format, "projp%d=%%lf", iproj );
                  if( sscanf( start, format, &projp ) ){
                     SetValue( ret, FormatKey( "PV", axlat + 1, iproj, ' ' ),
                               (void *) &projp, AST__FLOAT, 
                               "ZPN projection parameter" );
                  }
               }
            }

/* See if the WAT string contains any lngcor or latcor terms. If so, add
   warning keywords to the FitsChan. */
            if( ( strstr( watmem, "lngcor" ) || 
                  strstr( watmem, "lngcor" ) ) ){
               Warn( this, "zpn", "This FITS header includes, or was "
                     "derived from, a ZPN projection which requires "
                     "unsupported IRAF-specific corrections (lngcor "
                     "and/or latcor). The WCS information may therefore "
                     "be incorrect.", method, class );
            }
      
/*  Release the memory used to hold the concatenated WAT keywords. */
            watmem = (char *) astFree( (void *) watmem );
         }
      }

/* IRAF "TNX" projections 
   --------------------- */
   } else if( !Ustrcmp( prj, "-TNX" ) ) {

/* Replace TNX with TAN in the CTYPE values. */
      strcpy( lontype + 4, "-TAN" );
      cval = lontype;
      SetValue( ret, FormatKey( "CTYPE", axlon + 1, -1, ' ' ),
                (void *) &cval, AST__STRING, NULL );

      strcpy( lattype + 4, "-TAN" );
      cval = lattype;
      SetValue( ret, FormatKey( "CTYPE", axlat + 1, -1, ' ' ),
                (void *) &cval, AST__STRING, NULL );

/* Check latitude then longitude axes */
      for( i = 0; i < 2; i++ ){
         iaxis = i ? axlat : axlon;

/* Assume the TNX axis can be represented in FITS-WCS. */
         ok = 1;

/* Rewind the FitsChan. */
         astClearCard( this );

/* Concatenate all the IRAF "WAT" keywords together for this axis. These 
   keywords are marked as having been used, so that they are not written 
   out when the FitsChan is deleted. */
         watmem = NULL;
         watlen = 1;
         j = 1;
         sprintf( keyname, "WAT%d_%.3d", iaxis + 1, j );
         while( FindKeyCard( this, keyname, method, class ) && astOK ) {
            wat = (char *) CardData( this, &size );
            watmem = (char *) astRealloc( (void *) watmem, 
                                          watlen - 1 + size );
            if( watmem ) {
               strcpy( watmem + watlen - 1, wat );
               watlen += size - 1;
               MarkCard( this );
               MoveCard( this, 1, method, class );
               j++;
               sprintf( keyname, "WAT%d_%.3d", iaxis + 1, j );
            } else {
               break;
            }
         }

/* Search the total WAT string for any lngcor or latcor terms. */
         if( watmem ){
            start = strstr( watmem, "cor = \"" );

/* If found, extract the numerical values which follow. */
            if( start ) {
               start = strstr( start, "\"" ) + 1;
               j = 0;
               nch = 0;
               porder = -1;
               while( ok && 1 == sscanf( start, " %lf %n", (double *) &dval, &nch ) ){

/* The first value gives the correction surface type. We can only handle 
   type 3 (simple polynonial). */
                  if( j == 0 ){
                     if( dval != 3.0 ) ok = 0;
                  
/* The second and third numbers gives the orders of the polynomial in X
   and Y. We can only handle cases in which the orders are the same on
   both axes, and greater than 0 and less than 9. Store a pointer to the
   first TAN projection parameter index to use. */
                  } else if( j == 1 ){
                     porder = dval - 1;
                  } else if( j == 2 ){
                     if( dval - 1 != porder || dval < 0 || dval > 7 ) ok = 0;
                     mp = (i?b:a) + abskip[ porder ];

/* The fourth number defines the type of cross-terms. We can only handle
   type 2 (half-cross terms). */
                  } else if( j == 3 ){
                     if( dval != 2.0 ) ok = 0;

/* The next 4 numbers describe the region of validity of the fits in
   xi and eta space, e.g. ximin, ximax, etamin, etamax. We skip over these
   since we have no means of implementing any limit on the region of
   validity. */
                  
/* The remaining terms are the coefficients of the polynomial terms. */
                  } else if( j > 7 ){

/* Find the index of the corresponding PV keyword. */
                     m = *(mp++);

/* TNX polynomials provide a "correction* to be added to the supplied X and 
   Y values. Therefore increase the linear co-efficients by 1 on both
   axes. */
                     if( m == 1 ) dval += 1.0; 

/* Store the PV value */
                     SetValue( ret, FormatKey( "PV", iaxis + 1, m, ' ' ),
                               (void *) &dval, AST__FLOAT, 
                               "TAN projection parameter" );
                  }

                  start += nch;
                  nch = 0;
                  j++;
               }

/* Check that all the required co-efficients were found */
               if( porder == -1 || j != 8 + nab[ porder ] ) ok = 0;
            }

/* If the TNX cannot be represented in FITS-WCS (within our restrictions), add
   warning keywords to the FitsChan. */
            if( !ok ){
               Warn( this, "tnx", "This FITS header includes, or was "
                     "derived from, a TNX projection which requires "
                     "unsupported IRAF-specific corrections. The WCS "
                     "information may therefore be incorrect.", method, class );
            }
   
/*  Release the memory used to hold the concatenated WAT keywords. */
            watmem = (char *) astFree( (void *) watmem );
         }
      }
   }

/* IRAF mini-WCS keywords
   ---------------------- */
/* Rewind the FItsChan to search from the first card. */
   astClearCard( this );

/* Search forward through until all cards have been checked. */
   while( !astFitsEof( this ) && astOK ){

/* Check to see if the keyword name from the current card matches 
   any of the known mini-WCS keywords. If so, mark the card as read. */
      if( Match( CardName( this ), "WAT%d_%d", 0, NULL, &m, method, class ) ||
          Match( CardName( this ), "LTM%d_%d", 0, NULL, &m, method, class ) ||
          Match( CardName( this ), "LTV%d", 0, NULL, &m, method, class ) ||
          Match( CardName( this ), "WSV%d_LEN", 0, NULL, &m, method, class ) ||
          Match( CardName( this ), "WSV%d_%d", 0, NULL, &m, method, class ) ){
         MarkCard( this );
      }

/* Now move the current card on to the next card. */
      MoveCard( this, 1, method, class );

   }

/* Delete the returned FitsChan if it is empty. */
   if( ret && !astGetNcard( ret ) ) ret = (AstFitsChan *) astDelete( ret );

/* Return. */
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
*     AST__CONTINUE, AST__FLOAT, AST__COMPLEXI or AST__COMPLEXF defined in 
*     fitschan.h.

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
   int cont;                  /* Is this a continuation card? */
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

/* Check for CONTINUE cards. These have keyword CONTINUE but have a space
   instead of an equals sign in column 9. They must also have a single quote 
   in column 11. */
         cont = ( !Ustrcmp( *name, "CONTINUE" ) && 
                  nc > FITSNAMLEN + 3 && 
                  card[ FITSNAMLEN ] == ' ' && 
                  card[ FITSNAMLEN + 2 ] == '\'' );

/* If column 9 does not contain an equals sign (but is not a CONTINUE card), or if 
   the keyword is "HISTORY", "COMMENT" or blank, then columns 9 to the end are
   comment characters, and the value string is null. */
         if( ( nc <= FITSNAMLEN || card[ FITSNAMLEN ] != '='
                                || !Ustrcmp( *name, "HISTORY" )
                                || !Ustrcmp( *name, "COMMENT" )
                                || blank_name ) && !cont ){
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
               type = cont ? AST__CONTINUE : AST__STRING;

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
                        break;

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
               iopt = FullForm( "YES NO TRUE FALSE", v0, 1 );

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

static int SplitMap( AstMapping *map, int invert, AstMapping **map1,
                     AstMapping **map2, AstMapping **map3 ){
/*
*  Name:
*     SplitMap

*  Purpose:
*     Locate a WCS projection within a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     int SplitMap( AstMapping *map, int invert, AstMapping **map1, 
*                   AstMapping **map2, AstMapping **map3 )

*  Class Membership:
*     FitsChan

*  Description:
*     If possible, the supplied Mapping is decomposed into three component 
*     mappings to be compounded in series. To be acceptable, the second of 
*     these three Mappings must be an inverted WcsMap, and there must not
*     be a WcsMap in either of the other two Mappings. If it is not
*     possible to produce such a group of three Mappings, then a zero
*     function value is returned, together with three NULL Mapping
*     pointers. All the mappings before the WcsMap are compounded
*     together and returned as "map1". The inverse of the WcsMap itself is 
*     returned as "map2", and any remaining Mappings are compounded together 
*     and returned as "map3".
*
*     The search algorithm allows for an arbitrary combination of series and
*     parallel CmpMaps.

*  Parameters:
*     map
*        A pointer to the Mapping from pixel to physical coordinates.
*     invert
*        The value of the Invert attribute to use with "map" (the value 
*        returned by astGetInvert is not used).
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
*     dep 
*        The address of an integer holding the current depth of recursion
*        into this function.

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

/* Local Variables */
   AstMapping *mapa;       /* Pre-wcs Mapping */
   AstMapping *mapb;       /* WcsMap */
   AstMapping *mapc;       /* Post-wcs Mapping */
   int ret;                /* Was a non-linear Mapping found? */

/* Initialise */
   *map1 = NULL;
   *map2 = NULL;
   *map3 = NULL;
   ret = 0;

/* Check the global status. */
   if( !astOK ) return ret;

/* Call SplitMap2 to do the work. SplitMap2 does not check that the 
   WcsMap is an *inverted* WcsMap, neither does it check that there
   are no WcsMaps in either map1 or map3. */
   if( SplitMap2( map, invert, map1, map2, map3 ) ) {

/* Check that the WcsMap is inverted. */
      if( astGetInvert( *map2 ) ) {

/* Check that map 1 does not contain a WcsMap. */
         if( !SplitMap2( *map1, astGetInvert( *map1 ), &mapa, &mapb, &mapc ) ) {

/* Check that map 3 does not contain a WcsMap. */
            if( !SplitMap2( *map3, astGetInvert( *map3 ), &mapa, &mapb, &mapc ) ) {

/* If so, the three Mappings are OK. */
               ret = 1;

            } else {
               mapa = astAnnul( mapa );
               mapb = astAnnul( mapb );
               mapc = astAnnul( mapc );
            }

         } else {
            mapa = astAnnul( mapa );
            mapb = astAnnul( mapb );
            mapc = astAnnul( mapc );
         }
      }

      if( !ret ) {
         *map1 = astAnnul( *map1 );
         *map2 = astAnnul( *map2 );
         *map3 = astAnnul( *map3 );
      }

   }

   return ret;
}

static int SplitMap2( AstMapping *map, int invert, AstMapping **map1,
                      AstMapping **map2, AstMapping **map3 ){
/*
*  Name:
*     SplitMap2

*  Purpose:
*     Locate a WCS projection within a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     int SplitMap2( AstMapping *map, int invert, AstMapping **map1, 
*                    AstMapping **map2, AstMapping **map3 )

*  Class Membership:
*     FitsChan

*  Description:
*     If possible, the supplied Mapping is decomposed into three component 
*     mappings to be compounded in series. To be acceptable, the second of 
*     these three Mappings must be a WcsMap. If it is not possible to produce 
*     such a group of three Mappings, then a zero function value is returned, 
*     together with three NULL Mapping pointers. All the mappings before the 
*     WcsMap are compounded together and returned as "map1". The WcsMap itself 
*     is returned as "map2", and any remaining Mappings are compounded together
*     and returned as "map3". 
*
*     The search algorithm allows for an arbitrary combination of series and
*     parallel CmpMaps.

*  Parameters:
*     map
*        A pointer to the Mapping from pixel to physical coordinates.
*     invert
*        The value of the Invert attribute to use with "map" (the value 
*        returned by astGetInvert is not used).
*     map1
*        A location at which to return a pointer to the Mapping from pixel 
*        to relative physical coordinates. 
*     map2
*        A location at which to return a pointer to the Mapping from relative 
*        physical coordinates to native spherical coordinates. This will
*        be a WcsMap.
*     map3
*        A location at which to return a pointer to the Mapping from 
*        native spherical coordinates to physical coordinates. 
*     dep 
*        The address of an integer holding the current depth of recursion
*        into this function.

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

/* Local Variables */
   AstMapping **map_list;  /* Mapping array pointer */
   AstMapping *mapa;       /* Pre-wcs Mapping */
   AstMapping *mapb;       /* WcsMap */
   AstMapping *mapc;       /* Post-wcs Mapping */
   AstMapping *temp;       /* Intermediate Mapping */
   const char *class;      /* Pointer to class of supplied Mapping */
   double pv;              /* Projection parameter value */
   int *invert_list;       /* Invert array pointer */
   int axis;               /* No. of axes in whole Mapping */
   int axlat;              /* Index of latitude axis */
   int axlon;              /* Index of longitude axis */
   int haswcs;             /* Was a usable inverted WcsMap found? */
   int i;                  /* Index of current Mapping in list */
   int j;                  /* axis index */
   int m;                  /* Parameter index */
   int nax;                /* No. of axes in Mapping */
   int nmap;               /* Number of Mappings in the list */
   int ret;                /* Was a non-linear Mapping found? */
   int wcsaxis;            /* Index of first WcsMap axis */

/* Initialise */
   *map1 = NULL;
   *map2 = NULL;
   *map3 = NULL;
   ret = 0;

/* Check the global status. */
   if( !astOK ) return ret;

/* Get the class of the Mapping. */   
   class = astGetClass( map );

/* If the supplied Mapping is a CmpMap... */
   if( !strcmp( class, "CmpMap" ) ){

/* Decompose the Mapping into a sequence of Mappings to be applied in
   series and an associated list of Invert flags. */
      map_list = NULL;
      invert_list = NULL;
      nmap = 0;
      astMapList( map, 1, invert, &nmap, &map_list, &invert_list );

/* If there is more than one Mapping, this must be a series CmpMap. */
      if( nmap > 1 && astOK ){

/* Initialise the returned pre-wcs Mapping to be a UnitMap. */
         if( invert == astGetInvert( map ) ){
            *map1 = (AstMapping *) astUnitMap( astGetNin( map ), "" );
         } else {
            *map1 = (AstMapping *) astUnitMap( astGetNout( map ), "" );
         }

/* Indicate we have not yet found  a WcsMap. */
         ret = 0;

/* Process each series Mapping. */
         for( i = 0; i < nmap; i++ ){

/* If we have not yet found a WcsMap... */
            if( !ret ){

/* Search this Mapping for a WcsMap. */
               ret = SplitMap2( map_list[ i ], invert_list[ i ], &mapa, 
                               map2, map3 );

/* If no WcsMap was found, use the whole mapping as part of the 
   pre-wcs Mapping. */
               if( !ret ){
                  mapa = astCopy( map_list[ i ] );
                  astSetInvert( mapa, invert_list[ i ] );
               }

/* Add the pre-wcs mapping to the cumulative pre-wcs CmpMap. */
               temp = (AstMapping *) astCmpMap( *map1, mapa, 1, "" );
               *map1 = astAnnul( *map1 );
               mapa = astAnnul( mapa );
               *map1 = temp;                 

/* If we have previously found a WcsMap, use the whole mapping
   as part of the post-wcs mapping. */
            } else {
               mapc = astCopy( map_list[ i ] );
               astSetInvert( mapc, invert_list[ i ] );

               temp = (AstMapping *) astCmpMap( *map3, mapc, 1, "" );
               *map3 = astAnnul( *map3 );
               mapc = astAnnul( mapc );
               *map3 = temp;                 
            }
         }

/* If there is only one Mapping, this must be a parallel CmpMap. */
      } else {

/* Annul the Mapping pointer in the series list created above, and free the 
   dynamic arrays. */
         map_list[ 0 ] = astAnnul( map_list[ 0 ] );
         map_list = astFree( map_list );
         invert_list = astFree( invert_list );
         nmap = 0;

/* Decompose the Mapping into a sequence of Mappings to be applied in
   parallel and an associated list of Invert flags. */
         astMapList( map, 0, invert, &nmap, &map_list, &invert_list );

/* Process each parallel Mapping. */
         axis = 0;
         for( i = 0; i < nmap && astOK; i++ ){

/* See if this Mapping contains a usable WcsMap. Only do the search 
   if no such WcsMap has already been found, since only the first is usable. */
            if( !ret ) {

/* Search this Mapping for a WcsMap. */
               haswcs = SplitMap2( map_list[ i ], invert_list[ i ], &mapa, 
                                  &mapb, &mapc );

/* Note if we have found a usable WcsMap, and its first axis index. */
               if( haswcs ){
                  ret = 1;
                  wcsaxis = axis;
               }

/* If a WcsMap has already been found, the mapping cannot contain a
   usable WcsMap. */
            } else {
               haswcs = 0;
            }

/* If the Mapping did not contain a usable WcsMap, use the whole mapping as 
   part of the pre-wcs Mapping, and create a UnitMap as part of the post-wcs 
   mapping. */
            if( !haswcs ){
               mapa = astCopy( map_list[ i ] );
               astSetInvert( mapa, invert_list[ i ] );
               nax = astGetNout( mapa );
               mapc = (AstMapping *) astUnitMap( nax, "" );
            } 

/* Increment the index of the first axis in the next Mapping. */
            axis += astGetNout( mapa );

/* Add the pre-wcs mapping in parallel with the cumulative pre-wcs CmpMap. */
            if( *map1 ){
               temp = (AstMapping *) astCmpMap( *map1, mapa, 0, "" );
               *map1 = astAnnul( *map1 );
               mapa = astAnnul( mapa );
               *map1 = temp;                 
            } else {
               *map1 = mapa;
            }

/* Add the post-wcs mapping in parallel with the cumulative post-wcs CmpMap. */
            if( *map3 ){
               temp = (AstMapping *) astCmpMap( *map3, mapc, 0, "" );
               *map3 = astAnnul( *map3 );
               mapc = astAnnul( mapc );
               *map3 = temp;                 
            } else {
               *map3 = mapc;
            }

         }

/* If a usable WcsMap was found, create a new one which has all the same
   properties, but with enough axes to join the pre and post wcs Mappings
   together. Ensure the correct axes are used for longitude and latitude,
   and copy the projection parameters. */
         if( ret ){
            axlat = astGetWcsAxis( mapb, 1 );
            axlon = astGetWcsAxis( mapb, 0 );
            *map2 = (AstMapping *) astWcsMap( axis, astGetWcsType( mapb ), 
                                              axlon + wcsaxis + 1, 
                                              axlat + wcsaxis + 1, "");
            for( j = 0; j < astGetNin( mapb ); j++ ){
               for( m = 0; m < 100; m++ ){
                  pv = astGetPV( mapb, j, m );
                  if( pv != AST__BAD ) astSetPV( *map2, j + wcsaxis, m, pv );
               }
            }
            astInvert( *map2 );
            mapb = astAnnul( mapb );
         }
      }

/* Loop to annul all the Mapping pointers in the list. */
      for ( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );

/* Free the dynamic arrays. */
      map_list = astFree( map_list );
      invert_list = astFree( invert_list );

/* If the supplied Mapping is not a CmpMap, see if it is a WcsMap. If so, 
   take a copy and set its invert attribute correctly. Also create UnitMaps 
   for the pre and post wcs mappings. */
   } else if( !strcmp( class, "WcsMap" ) ){
      ret = 1;
      nax = astGetNin( map );
      *map1 = (AstMapping *) astUnitMap( nax, "" );
      *map2 = astCopy( map );
      astSetInvert( *map2, invert );
      *map3 = (AstMapping *) astUnitMap( nax, "" );
   }

/* If an error has occurred, or if no WcsMap was found, annul any Mappings. */
   if( !astOK || !ret ){
      ret = 0;
      if( *map1 ) *map1 = astAnnul( *map1 );
      if( *map2 ) *map2 = astAnnul( *map2 );
      if( *map3 ) *map3 = astAnnul( *map3 );
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
*     This function splits up the supplied CD matrix into separate PC and 
*     CDELT matrices. The product of the returned matrices (CDELT.PC) 
*     equals the supplied CD matrix. The CDELT values are chosen so that 
*     the corresponding row of the PC matrix represents a unit vector. 
*     The signs of the CDELT values are chosen so that the diagonal terms 
*     of the PC matrix are all positive.
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

/* DefB1950. */
/* --------- */
   } else if ( !strcmp( attrib, "defb1950" ) ) {
      result = astTestDefB1950( this );

/* Warnings. */
/* -------- */
   } else if ( !strcmp( attrib, "warnings" ) ) {
      result = astTestWarnings( this );

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "ncard" ) ||
               !strcmp( attrib, "allwarnings" ) ){
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

static void Warn( AstFitsChan *this, const char *condition, const char *text, 
                  const char*method, const char *class ){
/*
*  Name:
*     Warn

*  Purpose:
*     Store warning cards in a FitsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     int Warn( AstFitsChan *this, const char *condition, const char *text,
*               const char*method, const char *class );

*  Class Membership:
*     FitsChan member function.

*  Description:
*     If the Warnings attribute indicates that occurences of the specified 
*     condition should be reported, the supplied text is split into lines
*     and stored in the FitsChan as a series of ASTWARN cards, in front
*     of the current card. If the specified condition is not being reported, 
*     this function returns without action.

*  Parameters:
*     this
*        The FitsChan.
*     condition
*        Pointer to a string holding a lower case condition name.
*     text
*        Pointer to a string holding the text of the warning.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*/

/* Local Variables: */
   char buff[ FITSCARDLEN + 1 ]; /* Buffer for new card text */
   const char *a;        /* Pointer to 1st character in next card */
   const char *b;        /* Pointer to terminating null character */
   const char *c;        /* Pointer to last character in next card */
   int exists;           /* Has the supplied warning already been issued? */
   int icard;            /* Index of original card */
   int nc;               /* No. of characters in next card */

/* Check the inherited status, warning text and FitsChan. */
   if( !astOK || !text || !text[0] || !this ) return;

/* Look for the supplied condition within the list of conditions to be
   reported (given by the Warnings attribue). */
   if( FullForm( astGetWarnings( this ), condition, 0 ) >= 0 ){

/* If found, save the current card index, and rewind the FitsChan. */
      icard = astGetCard( this );
      astClearCard( this );

/* Break the supplied text into lines and check the FitsChan to see if 
   a block of adjacent ASTWARN cards with these lines already exist
   within the FitsChan. Assume they do until proven otherwise. */
      exists = 1;
      a = text;
      b = a + strlen( text );
      while( a < b ){

/* Each card contains about 60 characters of the text. Get a pointer to
   the nominal last character in the next card. */
         c = a + 60;

/* If this puts the last character beyond the end of the text, use the
   last character before the null as the last character in the card. */
         if( c >= b ) {
            c = b - 1;

/* Otherwise, if the last character is not a space, move the last
   character backwards to the first space. This avoids breaking words 
   across cards. */
         } else {
            while( !isspace( *c ) && c > a ) c--;
         }

/* Copy the text into a null terminated buffer. */
         nc = c - a + 1;
         strncpy( buff, a, nc );
         buff[ nc ] = 0;         

/* If this is the first line, search the entire FitsChan for an ASTWARN card 
   with this text. If not, indiate that the supplied text needs to be
   stored in the FitsChan, and break out of the loop. */
         if( a == text ) {
            exists = 0;

            while( !exists && 
                   FindKeyCard( this, "ASTWARN", method, class ) ) {
               if( !strcmp( (const char *) CardData( this, NULL ), buff ) ) {
                  exists = 1;
               }         
               MoveCard( this, 1, method, class );
            }
            if( !exists ) break;

/* If this is not the first line, see if the next card in the FitsChan is
   an ASTWARN card with this text. If not, indiate that the supplied text 
   needs to be stored in the FitsChan, and break out of the loop. */
         } else {
            if( !strcmp( CardName( this ), "ASTWARN" ) &&
                !strcmp( (const char *) CardData( this, NULL ), buff ) ) {
               MoveCard( this, 1, method, class );
            } else {
               exists = 0;
               break;
            }
         }

/* Set the start of the next bit of the text. */
         a = c + 1;
      }

/* Reinstate the original current card index. */
      astSetCard( this, icard );

/* We only add new cards to the FitsChan if they do not already exist. */
      if( !exists ) {

/* Break the text into lines using the same algorithm as above, and store 
   each line as a new ASTWARN card. Start with a blank ASTWARN card. */
         astFitsSetS( this, "ASTWARN", " ", NULL, 0 );

/* Loop until the entire text has been written out. */
         a = text;
         b = a + strlen( text );
         while( a < b ){

/* Each card contains about 60 characters of the text. Get a pointer to
   the nominal last character in the next card. */
            c = a + 60;

/* If this puts the last character beyond the end of the text, use the
   last character before the null as the last character in the card. */
            if( c >= b ) {
               c = b - 1;

/* Otherwise, if the last character is not a space, move the last
   character backwards to the first space. This avoids breaking words 
   across cards. */
            } else {
               while( !isspace( *c ) && c > a ) c--;
            }

/* Copy the text into a null terminated buffer. */
            nc = c - a + 1;
            strncpy( buff, a, nc );
            buff[ nc ] = 0;         

/* Store the buffer as the next card. */
            astFitsSetS( this, "ASTWARN", buff, NULL, 0 );

/* Set the start of the next bit of the text. */
            a = c + 1;
         }

/* Include a final blank card. */
         astFitsSetS( this, "ASTWARN", " ", NULL, 0 );

      }
   }
}

static AstWinMap *WcsAddRef( FitsStore *store, int noncel, char s, 
                             int *outperm, const char *method, 
                             const char *class   ){
/*
*  Name:
*     WcsAddRef

*  Purpose:
*     Create a WinMap which converts non-celestial axes into absolute
*     physical coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     AstWinMap *WcsAddRef( FitsStore *store, int noncel, char s, 
*                           int *outperm, const char *method, 
*                           const char *class   )

*  Class Membership:
*     FitsChan

*  Description:
*     A WinMap is created which implements a shift of origin by adding
*     the reference absolute physical coordinates (CRVALi) onto the input 
*     relative physical coordinates. The Mapping only applies to the
*     non-celestial axes. It is assumed that the longitude and latitude
*     axes, if present, will have been shuffled down to axes 0 and 1
*     respectively. The returned Mapping only applies to the remaining
*     axes.

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     noncel
*        The number of non-celestial axes.
*     s
*        Co-ordinate version character to use (space means primary axes).
*     outperm
*        Pointer to an array describing the correspondance between
*        axes in the returned Mapping and the crval values stored
*        in "store". Axis "i" of the Mapping will have reference value
*        crval[ outperm[ 2 + i ] ]. outperm[ 0 ] gives the index of the 
*        longitude axis in "store", and outperm[ 1 ] gives the index of 
*        the latitude axis in "store". 
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

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
   double *c1_in;                  /* Input corner 1 */
   double *c1_out;                 /* Input corner 1 */
   double *c2_in;                  /* Input corner 1 */
   double *c2_out;                 /* Input corner 1 */
   double crval;                   /* CRVAL value */
   int i;                          /* Axis index */
   int j;                          /* Axis index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the returned WinMap pointer. */
   new = NULL;

/* Allocate memory to hold the two corners, in both input and output
   coordinates. */
   c1_in = (double *) astMalloc( sizeof(double)*(size_t)noncel);
   c1_out = (double *) astMalloc( sizeof(double)*(size_t)noncel);
   c2_in = (double *) astMalloc( sizeof(double)*(size_t)noncel);
   c2_out = (double *) astMalloc( sizeof(double)*(size_t)noncel);

/* Check these pointers can be used. */
   if( astOK ){

/* Set up two arbitrary corners in the input coordinate system, and the
   corresponding values with the CRVAL values added onto the
   non-celestial axes. */
      if( noncel < store->naxis ){
         for( i = 0; i < noncel; i++ ){

/* Get the CRVAL value for the corresponding axis. */
            j = outperm[ 2 + i ];
            crval = GetItem( &(store->crval), j, 0, s, 
                             FormatKey( "CRVAL", j + 1, -1, s ), method, class );

/* Store the positions of the corners. */
            c1_in[ i ] = 0.0;
            c2_in[ i ] = 1.0;
            c1_out[ i ] = crval;
            c2_out[ i ] = 1.0 + crval;
         }

      } else {
         for( i = 0; i < noncel; i++ ){
            crval = GetItem( &(store->crval), i, 0, s, 
                             FormatKey( "CRVAL", i + 1, -1, s ), method, class );
            c1_in[ i ] = 0.0;
            c2_in[ i ] = 1.0;
            c1_out[ i ] = crval;
            c2_out[ i ] = 1.0 + crval;
         }
      }

/* Create the WinMap. */
      new = astWinMap( noncel, c1_in, c2_in, c1_out, c2_out, "" );

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

static AstWcsMap *WcsDeproj( AstFitsChan *this, FitsStore *store, char s, 
                           char **sys, const char *method, const char *class ){
/*
*  Name:
*     WcsDeproj

*  Purpose:
*     Create a WcsMap which transforms Relative Physical Coords to
*     Native Spherical Coords.

*  Type:
*     Private function.

*  Synopsis:
*     AstWcsMap *WcsDeproj( AstFitsChan *this, FitsStore *store, char s, char **sys, 
*                           const char *method, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A WcsMap is created which applies the inverse of one of the projections
*     included in the FITS WCS library (i.e. a DEprojection). It thus maps
*     Relative Physical Coords into Native Spherical Coords. The mapping
*     only affects the latitude and longitude axes. The mapping passes on
*     all other axes unchanged.

*  Parameters:
*     this
*        The FitsChan.
*     store 
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     s
*        Co-ordinate version character to use (space means primary axes).
*     sys
*        A pointer to a location at which to store a pointer to a static
*        string identifying the celestial co-ordinate system implied by 
*        the CTYPE values in the FitsStore. This will be "EQU" (for
*        equatorial), or a one or two character code extracted from the
*        CTYPE values.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created WcsMap or a NULL pointer if an error occurred.

*/

/* Local Variables: */
   AstWcsMap *new;                 /* The created WcsMap */
   char *ctype;                    /* Pointer to CTYPE string */
   char *keyname;                  /* Pointer to keyword name string */
   char buf[300];                  /* Text buffer */
   char latctype[MXCTYPELEN];      /* Latitude CTYPE keyword value */
   char latkey[10];                /* Latitude CTYPE keyword name */
   char lattype[4];                /* Buffer for celestial system */
   char lonctype[MXCTYPELEN];      /* Longitude CTYPE keyword value */
   char lonkey[10];                /* Longitude CTYPE keyword name */
   char lontype[4];                /* Buffer for celestial system */
   double pv;                      /* Projection parameter value */
   int axlat;                      /* Index of latitude physical axis */
   int axlon;                      /* Index of longitude physical axis */
   int gotax;                      /* Celestial axis found? */
   int j;                          /* Axis index */
   int latprj;                     /* Latitude projection type identifier */
   int lonprj;                     /* Longitude projection type identifier */
   int m;                          /* Parameter index */
   int np;                         /* Max parameter index */
   int prj;                        /* Projection type identifier */
   static char type[4];            /* Buffer for celestial system */

/* Initialise. */
   new = NULL;
   *sys = "";

/* Check the global status. */
   if ( !astOK ) return new;

/* We have not yet found any celestial axes. */
   axlon = -1;
   axlat = -1;

/* First, we need to examine the CTYPE values in the FitsStore to determine
   the type of projection to use. This also tells us which axes are the 
   longitude and latitude axes, and what the celestial co-ordinate system
   is (this is returned via the "sys" argument). Loop round the intermediate 
   axes, getting each CTYPE value. */
   for( j = 0; j < store->naxis && astOK; j++ ){
      keyname =  FormatKey( "CTYPE", j + 1, -1, s );
      ctype = GetItemC( &(store->ctype), j, s, NULL, method, class );

/* Issue a warning if no CTYPE value was found. */
      if( !ctype ) {
         sprintf( buf, "Axis type keywords (CTYPE, etc) were not found "
                  "for one or more axes in the original FITS header. These "
                  "axes will be assumed to be linear." );
         Warn( this, "noctype", buf, method, class );

/* We are looking for celestial axes. Celestial axes must have a "-" as the 
   fifth character in CTYPE. */
      } else if( ctype[4] == '-' ) {

/* Find the projection type as specified by the last 4 characters 
   in the CTYPE keyword value. AST__WCSBAD is stored in "prj" if the
   last 4 characters do not specify a known WCS projection, but no error
   is reported. */
         prj = astWcsPrjType( ctype + 4 );

/* See if this is a longitude axis (e.g. if the first 4 characters of CTYPE 
   are "RA--" or "xLON" or "yzLN" ). If so, store the value of "x" or "yz"
   (or "EQU" for equatorial coordinates) in variable "type" to indicate which 
   coordinate system is being used. */
         gotax = 0;
         if( !strncmp( ctype, "RA--", 4 ) ){
            strcpy( type, "EQU" );
            gotax = 1;

         } else if( !strncmp( ctype + 1, "LON", 3 ) ){
            type[ 0 ] = ctype[ 0 ];
            type[ 1 ] = 0;
            gotax = 1;

         } else if( !strncmp( ctype + 2, "LN", 2 ) ){
            type[ 0 ] = ctype[ 0 ];
            type[ 1 ] = ctype[ 1 ];
            type[ 2 ] = 0;
            gotax = 1;
         }

/* If this is a longitude axis... */
         if( gotax ){

/* Check that this is the first longitude axis to be found. */
            if( axlon == -1 ){

/* Report an error if the projection is unknown. */
               if( prj == AST__WCSBAD ){
                  astError( AST__BDFTS, "%s(%s): FITS keyword '%s' refers to "
                        "an unknown projection type '%s'.", method, class, 
                         keyname, ctype + 4 );
                  break;
               }   

/* Store the index of the longitude axis, type of longitude, etc. */
               axlon = j;
               strcpy( lontype, type );
               strcpy( lonkey, keyname );
               strcpy( lonctype, ctype );
               lonprj = prj;

/* If another longitude axis has already been found, report an error. */
            } else {
               astError( AST__BDFTS, "%s(%s): FITS keywords '%s' (='%s') "
                  "and '%s' (='%s') both describe celestial longitude axes.",
                  method, class, keyname, ctype, lonkey, lonctype );
               break;
            }
         }

/* Do the same for the latitude axis, checking for "DEC-" and "xLAT" and
  "yzLT". */
         gotax = 0;
         if( !strncmp( ctype, "DEC-", 4 ) ){
            strcpy( type, "EQU" );
            gotax = 1;

         } else if( !strncmp( ctype + 1, "LAT", 3 ) ){
            type[ 0 ] = ctype[ 0 ];
            type[ 1 ] = 0;
            gotax = 1;

         } else if( !strncmp( ctype + 2, "LT", 2 ) ){
            type[ 0 ] = ctype[ 0 ];
            type[ 1 ] = ctype[ 1 ];
            type[ 2 ] = 0;
            gotax = 1;
         }

         if( gotax ){
            if( axlat == -1 ){
               if( prj == AST__WCSBAD ){
                  astError( AST__BDFTS, "%s(%s): FITS keyword '%s' refers to "
                        "an unknown projection type '%s'.", method, class, 
                         keyname, ctype + 4 );
                  break;
               }   

               axlat = j;
               strcpy( lattype, type );
               strcpy( latkey, keyname );
               strcpy( latctype, ctype );
               latprj = prj;

            } else {
               astError( AST__BDFTS, "%s(%s): FITS keywords '%s' (='%s') "
                  "and '%s' (='%s') both describe celestial latitude axes.",
                  method, class, keyname, ctype, latkey, latctype );
               break;
            }
         }
      }
   }

/* Check the above went OK */
   if( astOK ){

/* If both longitude and latitude axes were found... */
      if( axlat != -1 && axlon != -1 ){

/* Report an error if they refer to different celestial coordinate systems. */
         if( strcmp( lattype, lontype ) ){
            astError( AST__BDFTS, "%s(%s): FITS keywords '%s' and '%s' "
                      "indicate different celestial coordinate systems "
                      "('%s' and '%s').", method, class, latkey, lonkey, 
                      latctype, lonctype );

/* Otherwise report an error if longitude and latitude axes use different 
   projections. */
         } else if( lonprj != latprj ){
            astError( AST__BDFTS, "%s(%s): FITS keywords '%s' and '%s' "
                      "indicate different projections ('%s' and '%s').", 
                      method, class, latkey, lonkey, latctype, lonctype );

/* Otherwise, return a pointer to the celestial coordinate system string. */
         } else {
            *sys = type;
         }

/* If only one axis has been provided without the other (e.g. longitude but no 
   latitude), report an error. */
      } else if( axlat != -1 ){
         astError( AST__BDFTS, "%s(%s): A latitude axis ('%s') was found "
                   "without a corresponding longitude axis.", method, class, 
                   latctype );

      } else if( axlon != -1 ){
         astError( AST__BDFTS, "%s(%s): A longitude axis ('%s') was found "
                   "without a corresponding latitude axis.", method, class, 
                   lonctype );
      }
   }   

/* Now we can create the WcsMap... First deal with cases in which both 
   longitude and latitude axes are available. */
   if( axlon >= 0 && axlat >= 0 && astOK ){

/* Create the WcsMap, and invert it to get a DEprojection. The WcsMap is
   equivalent to a unit mapping for all axes other than "axlat" and "axlon". */
      new = astWcsMap( store->naxis, latprj, axlon + 1, axlat + 1, "" );
      astInvert( new );

/* Set any projection parameters. */
      np = GetMaxIM( &(store->pv) );
      for( m = 0; m <= np; m++ ){

         pv = GetItem( &(store->pv), axlat, m, s, NULL, method, class );
         if( pv != AST__BAD ) astSetPV( new, axlat, m, pv );

         pv = GetItem( &(store->pv), axlon, m, s, NULL, method, class );
         if( pv != AST__BAD ) astSetPV( new, axlon, m, pv );

      }

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

static void WCSFcRead( AstFitsChan *fc, FitsStore *store, const char *method, 
                        const char *class ){
/*
*  Name:
*     WCSFcRead

*  Purpose:
*     Extract WCS information from a supplied FitsChan using a FITSWCS
*     encoding, and store it in the supplied FitsStore.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WCSFcRead( AstFitsChan *fc, FitsStore *store, const char *method, 
*                      const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function extracts FITSWCS keywords from the supplied FitsChan, 
*     and stores the corresponding WCS information in the supplied FitsStore.

*  Parameters:
*     fc
*        Pointer to the FitsChan containing the cards read from the
*        original FITS header. This should not include any un-used 
*        non-standard keywords.
*     store
*        Pointer to the FitsStore structure.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*/

/* Local Variables: */
   char *cval;        /* String keyword value */
   char *keynam;      /* Pointer to current keyword name */
   char s;            /* Co-ordinate version character */
   double dval;       /* Floating point keyword value */
   int fld[2];        /* Integer field values from keyword name */
   int im;            /* Pixel axis or projection parameter index */
   int j;             /* Intermediate axis index */
   int nfld;          /* Number of integer fields in test string */
   int type;          /* Keyword data type */
   void *item;        /* Pointer to item to get/put */

/* Check the global error status. */
   if ( !astOK ) return;

/* Ensure the FitsChan is re-wound. */
   astClearCard( fc );

/* Loop round all the cards in the FitsChan obtaining the keyword name for 
   each card. Note, the single "=" is correct in the following "while"
   statement. */
   while( (keynam = CardName( fc )) ){
      item = NULL;

/* Is this a primary CRVAL keyword? */
      if( Match( keynam, "CRVAL%d", 1, fld, &nfld, method, class ) ){
         item = &(store->crval);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = 0;
         s = ' ';

/* Is this a secondary CRVAL keyword? */
      } else if( Match( keynam, "CRVAL%d%1c", 1, fld, &nfld, method, class ) ){
         item = &(store->crval);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary CRPIX keyword? */
      } else if( Match( keynam, "CRPIX%d", 1, fld, &nfld, method, class ) ){
         item = &(store->crpix);
         type = AST__FLOAT;
         j = 0;
         im = fld[ 0 ] - 1;
         s = ' ';

/* Is this a secondary CRPIX keyword? */
      } else if( Match( keynam, "CRPIX%d%1c", 1, fld, &nfld, method, class ) ){
         item = &(store->crpix);
         type = AST__FLOAT;
         j = 0;
         im = fld[ 0 ] - 1;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary CTYPE keyword? If so, store the associated comment. */
      } else if( Match( keynam, "CTYPE%d", 1, fld, &nfld, method, class ) ){
         item = &(store->ctype);
         type = AST__STRING;
         j = fld[ 0 ] - 1;
         im = 0;
         s = ' ';
         SetItemC( &(store->ctype_com), j, ' ', CardComm( fc ) );

/* Is this a secondary CTYPE keyword? If so, store the associated comment. */
      } else if( Match( keynam, "CTYPE%d%1c", 1, fld, &nfld, method, class ) ){
         item = &(store->ctype);
         type = AST__STRING;
         j = fld[ 0 ] - 1;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];
         SetItemC( &(store->ctype_com), j, s, CardComm( fc ) );

/* Is this a primary CUNIT keyword? */
      } else if( Match( keynam, "CUNIT%d", 1, fld, &nfld, method, class ) ){
         item = &(store->cunit);
         type = AST__STRING;
         j = fld[ 0 ] - 1;
         im = 0;
         s = ' ';

/* Is this a secondary CUNIT keyword? */
      } else if( Match( keynam, "CUNIT%d%1c", 1, fld, &nfld, method, class ) ){
         item = &(store->cunit);
         type = AST__STRING;
         j = fld[ 0 ] - 1;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary CD keyword? */
      } else if( Match( keynam, "CD%d_%d", 2, fld, &nfld, method, class ) ){
         item = &(store->cd);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = fld[ 1 ] - 1;
         s = ' ';

/* Is this a secondary CD keyword? */
      } else if( Match( keynam, "CD%d_%d%1c", 2, fld, &nfld, method, class ) ){
         item = &(store->cd);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = fld[ 1 ] - 1;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary PV keyword? */
      } else if( Match( keynam, "PV%d_%d", 2, fld, &nfld, method, class ) ){
         item = &(store->pv);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = fld[ 1 ];
         s = ' ';

/* Is this a secondary PV keyword? */
      } else if( Match( keynam, "PV%d_%d%1c", 2, fld, &nfld, method, class ) ){
         item = &(store->pv);
         type = AST__FLOAT;
         j = fld[ 0 ] - 1;
         im = fld[ 1 ];
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary RADESYS keyword? */
      } else if( Match( keynam, "RADESYS", 0, fld, &nfld, method, class ) ){
         item = &(store->radesys);
         type = AST__STRING;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a secondary RADESYS keyword? */
      } else if( Match( keynam, "RADESYS%1c", 0, fld, &nfld, method, class ) ){
         item = &(store->radesys);
         type = AST__STRING;
         j = 0;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary EQUINOX keyword? */
      } else if( Match( keynam, "EQUINOX", 0, fld, &nfld, method, class ) ){
         item = &(store->equinox);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a secondary EQUINOX keyword? */
      } else if( Match( keynam, "EQUINOX%1c", 0, fld, &nfld, method, class ) ){
         item = &(store->equinox);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary LATPOLE keyword? */
      } else if( Match( keynam, "LATPOLE", 0, fld, &nfld, method, class ) ){
         item = &(store->latpole);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a secondary LATPOLE keyword? */
      } else if( Match( keynam, "LATPOLE%1c", 0, fld, &nfld, method, class ) ){
         item = &(store->latpole);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary LONPOLE keyword? */
      } else if( Match( keynam, "LONPOLE", 0, fld, &nfld, method, class ) ){
         item = &(store->lonpole);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a secondary LONPOLE keyword? */
      } else if( Match( keynam, "LONPOLE%1c", 0, fld, &nfld, method, class ) ){
         item = &(store->lonpole);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];

/* Is this a primary MJD-OBS keyword? */
      } else if( Match( keynam, "MJD-OBS", 0, fld, &nfld, method, class ) ){
         item = &(store->mjdobs);
         type = AST__FLOAT;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a primary WCSNAME keyword? */
      } else if( Match( keynam, "WCSNAME", 0, fld, &nfld, method, class ) ){
         item = &(store->wcsname);
         type = AST__STRING;
         j = 0;
         im = 0;
         s = ' ';

/* Is this a secondary WCSNAME keyword? */
      } else if( Match( keynam, "WCSNAME%1c", 0, fld, &nfld, method, class ) ){
         item = &(store->wcsname);
         type = AST__STRING;
         j = 0;
         im = 0;
         s = keynam[ strlen( keynam ) - 1 ];
      }

/* If this keyword was recognized, store it in the FitsStore, and mark it
   as having been read. */
      if( item ){
         if( type == AST__FLOAT ){
            if( CnvValue( fc, AST__FLOAT, &dval, method ) ) {
               SetItem( (double ****) item, j, im, s, dval );
               MarkCard( fc );
            }    
         } else {
            if( CnvValue( fc, AST__STRING, &cval, method ) ) {
               SetItemC( (char ****) item, j, s, cval );
               MarkCard( fc );
            }    
         }
      }   

/* Move on to the next card. */
      MoveCard( fc, 1, method, class );

   }

}

static AstFrame *WcsFrame( AstFitsChan *this, FitsStore *store, char s, int prj,
                           char *sys, int axlon, int axlat, const char *method,
                           const char *class  ){
/*
*  Name:
*     WcsFrame

*  Purpose:
*     Create a Frame to describe a WCS physical coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     AstFrame *WcsFrame( AstFitsChan this, FitsStore *store, char s, int prj,
*                         char *sys, int axlon, int axlat, const char *method, 
*                         const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A Frame is returned describing the physical coordinate system of
*     a WCS system. This will be a SkyFrame if the physical
*     coordinate system consists of a pair of celestial
*     longitude and latitude axes.  If there are any other axes, then a
*     CmpFrame will be returned holding a SkyFrame for the celestial
*     axes, and a simple Frame for the other axes. If there are no
*     celestial axes in the FitsChan, then a simple Frame will 
*     be returned.

*  Parameters:
*     this
*        The FitsChan from which the keywords were read. Warning messages
*        may be added to this FitsChan.
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     s
*        A character identifying the co-ordinate version to use. A space 
*        means use primary axis descriptions. Otherwise, it must be an 
*        upper-case alphabetical characters ('A' to 'Z').
*     prj
*        An integer code for the WCS projection being used.
*     sys
*        A pointer to a string identifying the celestial co-ordinate system 
*        implied by the CTYPE values in the FitsStore. This will be "EQU" (for
*        equatorial), or a one or two character code extracted from the
*        CTYPE values.
*     axlon 
*        Zero based index of the longitude axis in the FITS header.
*     axlat
*        Zero based index of the latitude axis in the FITS header.
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
   char *ckeyval;                 /* Pointer to string item value */
   char *lattype;                 /* Pointer to latitude CTYPE value */
   char *lontype;                 /* Pointer to longitude CTYPE value */
   char bj;                       /* Besselian/Julian selector */
   char buf[300];                 /* Text buffer */
   char id[2];                    /* ID string for returned Frame */
   char sym[10];                  /* Axis symbol */
   double equinox;                /* EQUINOX value */
   double eqmjd;                  /* MJD equivalent of equinox */
   double mjdobs;                 /* MJD-OBS value */
   int *perm;                     /* Permuted axis indices */
   int axis;                      /* Axis index */
   int axis2;                     /* Another axis index */
   int inon;                      /* Index of next non-celestial axis */
   int naxis;                     /* No. of axes */
   int noncel;                    /* Number of non-celestial axes */
   int ok;                        /* No identical labels found yet? */
   int radesys;                   /* RADESYS value */
   int skperm[2];                 /* Axis permutation for SkyFrames */

/* Initialise. */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Get the number of axes. */
   naxis = store->naxis;

/* First deal with cases where the axes defined by the FITS header cards 
   include a celestial longitude/latitude pair. */
   skyframe = NULL;
   if( sys[ 0 ] && axlat != axlon ){

/* Get the RADESYS keyword from the header, and identify the value. Store 
   a integer value identifying the system. Report an error if an 
   unrecognised system is supplied. Store NORADEC if the keyword was 
   not supplied. */
      ckeyval = GetItemC( &(store->radesys), 0, s, NULL, method, class );
      if( ckeyval ){
         if( !strncmp( ckeyval, "FK4 ", 4 ) || 
             !strcmp( ckeyval, "FK4" ) ){
            radesys = FK4;

         } else if( !strncmp( ckeyval, "FK4-NO-E", 8 ) ){
            radesys = FK4NOE;

         } else if( !strncmp( ckeyval, "FK5 ", 4 ) || 
                    !strcmp( ckeyval, "FK5" ) ){
            radesys = FK5;

         } else if( !strncmp( ckeyval, "ICRS ", 5 ) || 
                    !strcmp( ckeyval, "ICRS" ) ){
            radesys = ICRS;

         } else if( !strncmp( ckeyval, "GAPPT ", 6 ) ||
                    !strcmp( ckeyval, "GAPPT" ) ){
            radesys = GAPPT;

         } else if( astOK ){
            astError( AST__BDFTS, "%s(%s): FITS keyword '%s' has the "
                      "unrecognised value '%s'.", method, class,
                      FormatKey( "RADESYS", -1, -1, s ), ckeyval );
         }

      } else {
         radesys = NORADEC;
      }

/* Get the value of the EQUINOX keyword. */
      equinox = GetItem( &(store->equinox), 0, 0, s, NULL, method, class );

/* For FK4 and FK4-NO-E any supplied equinox value is Besselian. For all 
   other systems, the equinox value is Julian. */
      if( equinox != AST__BAD ){
         if( radesys == FK4 || radesys == FK4NOE ){
            bj = 'B';
         } else if( radesys != NORADEC ) {         
            bj = 'J';

/* If no RADESYS was suppied, but an equinox was, use the IAU 1984 rule
   to determine the default RADESYS and equinox type. */
         } else {
            if( equinox < 1984.0 ){
               radesys = FK4;
               bj = 'B';
            } else {
               radesys = FK5;
               bj = 'J';
            }

/* If an equatorial system is being used, give a warning that a default RADESYS 
   value is being used. */
            if( !strcmp( sys, "EQU" ) ){
               sprintf( buf, "The original FITS header did not specify the "
                        "RA/DEC reference frame. A default value of %s was "
                        "assumed.", ( radesys == FK4 ) ? "FK4" : "FK5" );
               Warn( this, "noradesys", buf, method, class );
            }
         }

/* If no equinox was supplied, use a default equinox value depending
   on the frame of reference. For FK4-based systems, use B1950. */
      } else {
         if( radesys == FK4 || radesys == FK4NOE ){
            equinox = 1950.0;
            bj = 'B';

/* For ICRS or FK5-based systems, use J2000. */
         } else if( radesys == FK5 || radesys == ICRS ){
            equinox = 2000.0;
            bj = 'J';

/* If no RADESYS or EQUINOX was supplied, assume either FK4 B1950 or FK5
   J2000 (decided by attribute DefB1950) (GAPPT does not use EQUINOX). */
         } else if( radesys == NORADEC ) {
            if( astGetDefB1950( this ) ) {
               equinox = 1950.0;
               bj = 'B';
               radesys = FK4;
            } else {
               equinox = 2000.0;
               bj = 'J';
               radesys = FK5;
            }
            if( !strcmp( sys, "EQU" ) ){
               sprintf( buf, "The original FITS header did not specify the "
                        "RA/DEC reference frame. A default value of %s was "
                        "assumed.", ( radesys == FK4 ) ? "FK4" : "FK5" );
               Warn( this, "noradesys", buf, method, class );
            }
         }

/* If we have an equatorial or ecliptic system, issue a warning that a default
   equinox has been adopted. */
         if( !strcmp( sys, "EQU" ) || !strcmp( sys, "ECL" ) ){
            sprintf( buf, "The original FITS header did not specify the "
                     "reference equinox. A default value of %c%.8g was "
                     "assumed.", bj, equinox );
            Warn( this, "noequinox", buf, method, class );
         }
      }

/* Convert the equinox to a Modified Julian Date. */
      if( bj == 'B' ) {
         eqmjd = slaEpb2d( equinox );
      } else {
         eqmjd = slaEpj2d( equinox );
      }

/* Get the MJD-OBS value. If it is missing, use the equinox, and issue a
   warning. */    
      mjdobs = GetItem( &(store->mjdobs), 0, 0, s, NULL, method, class );
      if( mjdobs == AST__BAD ) {
         mjdobs = eqmjd;
         sprintf( buf, "The original FITS header did not specify the "
                  "date of observation. A default value of %c%.8g was "
                  "assumed.", bj, equinox );
         Warn( this, "nomjd-obs", buf, method, class );

      }

/* Create a SkyFrame for the specified system. */
      if( !strcmp( sys, "E" ) ){
         skyframe = astSkyFrame( "System=Ecliptic" );

      } else if( !(strcmp( sys, "G" ) ) ){
         skyframe = astSkyFrame( "System=Galactic" );

      } else if( !(strcmp( sys, "S" ) ) ){
         skyframe = astSkyFrame( "System=Supergalactic" );

      } else if( !(strcmp( sys, "EQU" ) ) ){

/* For equatorial systems, the specific system is given by the RADESYS
   value. */
         if( radesys == FK4 ){
            skyframe = astSkyFrame( "System=FK4" );

         } else if( radesys == FK4NOE ){
            skyframe = astSkyFrame( "System=FK4_NO_E" );

         } else if( radesys == FK5 ){
            skyframe = astSkyFrame( "System=FK5" );

         } else if( radesys == ICRS ){
            skyframe = astSkyFrame( "System=FK5" );

         } else if( radesys == GAPPT ){
            skyframe = astSkyFrame( "System=GAPPT" );

         } else if( astOK ){
            astError( AST__INTER, "%s(%s): Internal AST programming "
                      "error - FITS equatorial coordinate system type %d "
                      "not yet supported in WcsFrame.", method, class, radesys );
         }

/* If an unknown celestial co-ordinate system was specified by the CTYPE 
   keywords, add warning messages to the FitsChan and treat the axes as
   a general spherical coordinate system. */
      } else if( astOK ){
         skyframe = astSkyFrame( "System=UNKNOWN" );
         strcpy( sym, sys );
         if( strlen( sys ) == 1 ) {
            strcpy( sym + 1, "LON" );                        
            astSetSymbol( skyframe, 0, sym );
            strcpy( sym + 1, "LAT" );                        
            astSetSymbol( skyframe, 1, sym );
         } else {
            strcpy( sym + 2, "LN" );                        
            astSetSymbol( skyframe, 0, sym );
            strcpy( sym + 2, "LT" );                        
            astSetSymbol( skyframe, 1, sym );
         }

         lontype = GetItemC( &(store->ctype), axlon, s, NULL, method, class );
         lattype = GetItemC( &(store->ctype), axlat, s, NULL, method, class );
         if( lontype && lattype ){
            sprintf( buf, "This FITS header contains references to an unknown "
                     "spherical co-ordinate system specified in the values " 
                     "%s and %s. It may not be possible to convert to "
                     "other standard co-ordinate systems.", lontype, lattype );
            Warn( this, "badcel", buf, method, class );
         }
      }
   }

/* If a skyFrame was created... */
   if( skyframe ){

/* Store the projection description. */
      astSetProjection( skyframe, astWcsPrjDesc( prj )  );

/* Store the epoch of the observation in the SkyFrame. */
      if( mjdobs != AST__BAD ) astSetEpoch( skyframe, mjdobs );

/* Store the epoch of the reference equinox in the SkyFrame. */
      if( equinox != AST__BAD ) astSetEquinox( skyframe, eqmjd );

/* If there are more than 2 axes, then there must also be some non-celestial 
   axes. */
      noncel = naxis - 2;

/* If no SkyFrame was created, all axes are non-celestial. */
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
            if( axis == axlon ){
               perm[ axis ] = 0;

/* The latitude axis is axis one in the CmpFrame. */
            } else if( axis == axlat ){
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
         if( axis != axlon && axis != axlat ){
            ckeyval = GetItemC( &(store->ctype), axis, s, NULL, method, class );
            if( ckeyval ) astSetSymbol( ret, axis, ckeyval );

            ckeyval = GetItemC( &(store->ctype_com), axis, s, NULL, method, class );
            if( ckeyval ) astSetLabel( ret, axis, ckeyval );

            ckeyval = GetItemC( &(store->cunit), axis, s, NULL, method, class );
            if( ckeyval ) astSetUnit( ret, axis, ckeyval );
         }            

      }

/* Now check that no two axis labels are identical. If so we clear all the
   axis labels. */
      ok = 1;
      for( axis = 0; axis < naxis-1 && ok; axis++ ){
         if( axis != axlon && axis != axlat ){
            ckeyval = (char *) astGetLabel( ret, axis );
            for( axis2 = axis+1; axis2 < naxis && ok; axis2++ ){
               if( axis2 != axlon && axis2 != axlat ){
                  if( !strcmp( astGetLabel( ret, axis2 ), ckeyval ) ) {
                     ok = 0;
                  }
               }
            }
         }
      }
               
      if( !ok ){
         for( axis = 0; axis < naxis; axis++ ){
            if( axis != axlon && axis != axlat ){
               astClearLabel( ret, axis );
            }
         }
      }

/* If there are no non-celestial axes, the returned Frame is just the
   SkyFrame. Permute the axes to ensure that long./lat. axes correspond
   to the FITS headers. */
   } else {
      ret = (AstFrame *) skyframe;
      if( axlon != 0 ) {
         skperm[ axlon ] = 0;
         skperm[ axlat ] = 1;
         astPermAxes( ret, skperm );
      }
   }   

/* If the header contained a WCSNAME keyword use it as the Domain name
   for the Frame. */
   ckeyval = GetItemC( &(store->wcsname), 0, s, NULL, method, class );
   if( ckeyval ){
      astSetDomain( ret, ckeyval );
   }

/* If the returned Frame is not a SkyFrame, and no Title value has been
   set, create a Title using the Domain name (if any). */
   if( !astIsASkyFrame( ret ) && !astTestTitle( ret ) && 
        astTestDomain( ret ) ) {
      sprintf( buf, "%s coordinates", astGetDomain( ret ) );
      astSetTitle( ret, buf );
   }

/* Unless this is the primary coordinate system, store the coordinate version 
   character as the Ident attribute for the returned Frame. */
   if ( s != ' ' ) { 
       id[ 0 ] = s;
       id[ 1 ] = 0;
       astSetIdent( ret, id );
   }

/* If an error has occurred, annul the Frame. */
   if( !astOK ) ret = astAnnul( ret );
   
/* Return the Frame. */
   return ret;

}

static int WCSFromStore( AstFitsChan *this, FitsStore *store, 
                         int axlat, int axlon, const char *method, 
                         const char *class ){
/*
*  Name:
*     WCSFromStore

*  Purpose:
*     Store WCS keywords in a FitsChan using FITS-WCS encoding.

*  Type:
*     Private function.

*  Synopsis:
*     int WCSFromStore( AstFitsChan *this, FitsStore *store, 
*                       int axlat, int axlon, const char *method, 
*                       const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function copies the WCS information stored in the supplied 
*     FitsStore into the supplied FitsChan, using FITS-WCS encoding.

*  Parameters:
*     this
*        Pointer to the FitsChan.
*     store
*        Pointer to the FitsStore.
*     axlon 
*        Index of celestial longitude axis (-1 if there is no celestial
*        longitude axis).
*     axlat
*        Index of celestial latitude axis (-1 if there is no celestial
*        longitude axis).
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A value of 1 is returned if succesfull, and zero is returned
*     otherwise.

*/

/* Local Variables: */
   char *comm;         /* Pointer to comment string */
   char *cval;         /* Pointer to string keyword value */
   char combuf[80];    /* Buffer for FITS card comment */
   char s;             /* Co-ordinate version character */
   char sign[2];       /* Fraction's sign character */
   char sup;           /* Upper limit on s */
   double fd;          /* Fraction of a day */
   double mjd99;       /* MJD at start of 1999 */
   double val;         /* General purpose value */
   int i;              /* Axis index */
   int ihmsf[ 4 ];     /* Hour, minute, second, fractional second */
   int iymdf[ 4 ];     /* Year, month, date, fractional day */
   int j;              /* Axis index */
   int jj;             /* SlaLib status */
   int m;              /* Parameter index */
   int maxm;           /* Upper limit on m */
   int naxis;          /* No. of axes */
   int ok;             /* Frame created succesfully? */
   int ret;            /* Returned value */

/* Initialise */
   ret = 0;

/* Check the inherited status. */
   if( !astOK ) return ret;

/* Save the number of pixel axes */
   naxis = GetMaxIM( &(store->crpix) ) + 1;

/* Loop round all co-ordinate versions */
   sup = GetMaxS( &(store->crval) );
   for( s = ' '; s <= sup && astOK; s++ ){      

/* Assume the Frame can be created succesfully. */
      ok = 1;

/* Get and save CRPIX for all pixel axes. These are required, so pass on
   if they are not available. */
      for( i = 0; i < naxis; i++ ){
         val = GetItem( &(store->crpix), 0, i, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
            goto next;
         }
         sprintf( combuf, "Reference pixel on axis %d", i + 1 );
         SetValue( this, FormatKey( "CRPIX", i + 1, -1, s ), &val, AST__FLOAT, 
                   combuf );
      }

/* Get and save CRVAL for all intermediate axes. These are required, so 
   pass on if they are not available. */
      for( j = 0; j < naxis; j++ ){
         val = GetItem( &(store->crval), j, 0, s, NULL, method, class );
         if( val == AST__BAD ) {
            ok = 0;
            goto next;
         }
         sprintf( combuf, "Value at ref. pixel on axis %d", j + 1 );
         SetValue( this, FormatKey( "CRVAL", j + 1, -1, s ), &val, AST__FLOAT, 
                   combuf );
      }

/* Get and save CTYPE for all intermediate axes. These are required, so 
   pass on if they are not available. */
      for( j = 0; j < naxis; j++ ){
         cval = GetItemC( &(store->ctype), j, s, NULL, method, class );
         if( !cval ) {
            ok = 0;
            goto next;
         }
         comm = GetItemC( &(store->ctype_com), j, s, NULL, method, class );
         if( !comm ) {            
            sprintf( combuf, "Type of co-ordinate on axis %d", j + 1 );
            comm = combuf;
         }
         SetValue( this, FormatKey( "CTYPE", j + 1, -1, s ), &cval, AST__STRING, 
                   comm );
      }

/* CD matrix. These are NOT required, so do not pass on if they are not 
   available. */
      for( j = 0; j < naxis; j++ ){
         for( i = 0; i < naxis; i++ ){
            val = GetItem( &(store->cd), j, i, s, NULL, method, class );
            if( val != AST__BAD ) {
               if( ( i != j && val != 0.0 ) || 
                   ( i == j && val != 1.0 ) ){
                  SetValue( this, FormatKey( "CD", j + 1, i + 1, s ), &val, 
                            AST__FLOAT, "Transformation matrix element" );
               }
            }
         }
      }

/* Get and save CUNIT for all intermediate axes. These are NOT required, so 
   do not pass on if they are not available. */
      for( j = 0; j < naxis; j++ ){
         cval = GetItemC( &(store->cunit), j, s, NULL, method, class );
         if( cval ) {
            sprintf( combuf, "Units for axis %d", j + 1 );
            SetValue( this, FormatKey( "CUNIT", j + 1, -1, s ), &cval, AST__STRING, 
                      combuf );
         }
      }

/* Get and save WCSNAME. This is NOT required, so do not return if it is 
   not available. */
      cval = GetItemC( &(store->wcsname), 0, s, NULL, method, class );
      if( cval ) SetValue( this, FormatKey( "WCSNAME", -1, -1, s ), &cval, 
                           AST__STRING, "Reference name for the coord. frame" );

/* Get and save RADESYS. This is NOT required, so do not return if it is 
   not available. */
      cval = GetItemC( &(store->radesys), 0, s, NULL, method, class );
      if( cval ) SetValue( this, FormatKey( "RADESYS", -1, -1, s ), &cval, 
                           AST__STRING, "Reference frame for RA/DEC values" );

/* Reference equinox */
      val = GetItem( &(store->equinox), 0, 0, s, NULL, method, class );
      if( val != AST__BAD ) SetValue( this, FormatKey( "EQUINOX", -1, -1, s ),
                                      &val, AST__FLOAT, 
                                      "Epoch of reference equinox" );

/* Latitude of native north pole */
      val = GetItem( &(store->latpole), 0, 0, s, NULL, method, class );
      if( val != AST__BAD ) SetValue( this, FormatKey( "LATPOLE", -1, -1, s ),
                                      &val, AST__FLOAT, 
                                      "Latitude of native north pole" );
/* Longitude of native north pole */
      val = GetItem( &(store->lonpole), 0, 0, s, NULL, method, class );
      if( val != AST__BAD ) SetValue( this, FormatKey( "LONPOLE", -1, -1, s ),
                                      &val, AST__FLOAT, 
                                      "Longitude of native north pole" );

/* Date of observation */
      val = GetItem( &(store->mjdobs), 0, 0, s, NULL, method, class );
      if( val != AST__BAD ) {
         SetValue( this, FormatKey( "MJD-OBS", -1, -1, s ),
                   &val, AST__FLOAT, "Modified Julian Date of observation" );

/* The format used for the DATE-OBS keyword depends on the value of the
   keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
   Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format. */
         slaCaldj( 99, 1, 1, &mjd99, &jj );
         if( val < mjd99 ) {
   
            slaDjcal( 0, val, iymdf, &jj );
            sprintf( combuf, "%2.2d/%2.2d/%2.2d", iymdf[ 2 ], iymdf[ 1 ], 
                     iymdf[ 0 ] - ( ( iymdf[ 0 ] > 1999 ) ? 2000 : 1900 ) ); 
   
         } else {
   
            slaDjcl( val, iymdf, iymdf+1, iymdf+2, &fd, &jj );
            slaDd2tf( 3, fd, sign, ihmsf );
            sprintf( combuf, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3dZ",
                     iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
                     ihmsf[2], ihmsf[3] ); 
         }

/* Now store the formatted string in the FitsChan. */
         cval = combuf;
         SetValue( this, "DATE-OBS", (void *) &cval, AST__STRING,
                   "Date of observation" );
      }

/* Projection parameters */
      maxm = GetMaxIM( &(store->pv) );
      for( j = 0; j < naxis; j++ ){
         for( m = 0; m <= maxm; m++ ){
            val = GetItem( &(store->pv), j, m, s, NULL, method, class );
            if( val != AST__BAD ) {
               SetValue( this, FormatKey( "PV", j + 1, m, s ), &val, 
                         AST__FLOAT, "Projection parameter" );
            }
         }
      }

/* See if a Frame was sucessfully written to the FitsChan. */
next:
      ok = ok && astOK;

/* If so, indicate we have something to return. */
      if( ok ) ret = 1;

/* If we are producing secondary axes, clear any error status so we can 
   continue to produce the next Frame. Retain the error if the primary axes 
   could not be produced. After the primary axes, do the A axes. */
      if( s != ' ' ) {
         astClearStatus;
      } else {
         s = 'A' - 1;
      }

/* Remove the secondary "new" flags from the FitsChan. This flag is
   associated with cards which have been added to the FitsChan during
   this pass through the main loop in this function. If the Frame was
   written out succesfully, just clear the flags. If anything went wrong
   with this Frame, remove the flagged cards from the FitsChan. */
      FixNew( this, NEW2, !ok, method, class );

/* Set the current card so that it points to the last WCS-related keyword
   in the FitsChan (whether previously read or not). */
      FindWcs( this, method, class );
   }

/* Return zero or ret depending on whether an error has occurred. */
   return astOK ? ret : 0;
}

static AstMapping *WcsMapping( AstFitsChan *this, FitsStore *store, char s, 
                               char **sys, int *prj, int *axlon, int *axlat, 
                               const char *method, const char *class ){
/*
*  Name:
*     WcsMapping

*  Purpose:
*     Create a CmpMap for the WCS transformations described in a FITS header.

*  Type:
*     Private function.

*  Synopsis:
*     AstMapping *WcsMapping( AstFitsChan *this, FitsStore *store, char s, 
*                             char **sys, int *prj, int *axlon, int *axlat, 
*                             const char *method, const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     This function interprets the contents of the supplied FitsStore
*     structure, and creates a Mapping which describes the transformation 
*     from pixel coordinates to absolute physical coordinates, using 
*     the FITS World Coordinate System conventions. A pointer to this CmpMap 
*     is returned.

*  Parameters:
*     this
*        The FitsChan.
*     store
*        A structure containing information about the requested axis 
*        descriptions derived from a FITS header.
*     s
*        A character identifying the co-ordinate version to use. A space 
*        means use primary axis descriptions. Otherwise, it must be an 
*        upper-case alphabetical characters ('A' to 'Z').
*     sys
*        A pointer to a location at which to store a pointer to a static
*        string identifying the celestial co-ordinate system implied by 
*        the CTYPE values in the FitsStore. This will be "EQU" (for
*        equatorial), or a one or two character code extracted from the
*        CTYPE values.
*     prj
*        A pointer to a location at which to store an integer code 
*        identifying the projection implied by the CTYPE values in the 
*        FitsStore. 
*     axlon
*        A pointer to a location at which to store the index of the 
*        longitude axis.
*     axlat
*        A pointer to a location at which to store the index of the 
*        latitude axis.
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the Mapping.

*/

/* Local Variables: */
   AstCmpMap    *map4;       /* Spherical rotation mapping */
   AstCmpMap    *map7;       /* Temporary mapping */
   AstMapping   *ret;        /* The returned Mapping */
   AstMatrixMap *map2;       /* CDj_i scale/rotation mapping (degrees) */
   AstMatrixMap *map5;       /* Degrees to radians mapping */
   AstMatrixMap *map6;       /* CDj_i scale/rotation mapping (radians) */
   AstWcsMap    *map3;       /* Deprojection Mapping */
   AstWinMap    *map1;       /* CRPIX shift mapping */
   double *mat;              /* Pointer to data for deg->rad scaling matrix */
   int i;                    /* Loop count */

/* Initialise the pointer to the returned Mapping. */
   ret = NULL;

/* Check the global status. */
   if ( !astOK ) return ret;

/* Try to create a WinMap which translates the pixel coordinates so
   that they are refered to an origin at the reference pixel. This
   subtracts the value of CRPIXi from axis i. */
   map1 = WcsShift( store, s, method, class );
   
/* Now try to create a MatrixMap describing the transformation from "Ideal 
   Pixel Coords" to "Relative Physical Coords" (excluding the shift of 
   origin described by the WinMap just created). This implements the CD 
   matrix. */
   map2 = WcsMatrix( store, s, method, class );

/* Now try to create a WcsMap which describes the deprojection for celestial
   axes from "Relative Physical Coords" to "Native Spherical Coords".
   Non-celestial axes are just copied without change. */
   map3 = WcsDeproj( this, store, s, sys, method, class );

/* Return the projection type and axis indices. */
   if( map3 ){
      *prj = astGetWcsType( map3 );
      *axlon = astGetWcsAxis( map3, 0 );
      *axlat = astGetWcsAxis( map3, 1 );

/* Scale the longitude and latitude axes from degrees to radians. */
      mat = (double *) astMalloc( sizeof(double)*store->naxis );
      if( mat ){
         for( i = 0; i < store->naxis; i++ ){
            if( i == *axlat || i == *axlon ){
               mat[i] = AST__DD2R;
            } else {
               mat[i] = 1.0;
            }
         }
         map5 = astMatrixMap( store->naxis, store->naxis, 1, mat, "" );
         map6 = astMtrMult( map2, map5 );

         map2 = astAnnul( map2 );         
         map5 = astAnnul( map5 );         
         map2 = map6;

         mat = (double *) astFree( (void *) mat );
      }

   } else {
      *prj = AST__WCSBAD;
      *axlon = -1;
      *axlat = -1;
   }

/* Now produce a Mapping which converts the axes holding "Native Spherical 
   Coords" into "Celestial Coords", leaving all other axes unchanged. This
   is a multi-stage mapping which is stored as a CmpMap. Non-celestial
   axes are converted into absolute physical coordinates by adding on the
   reference value. */
   map4 = WcsNative( this, store, s, map3, method, class );

/* Combine the four mappings in series, and then annul them. */
   ret = (AstMapping *) astCmpMap( map1, map2, 1, "" );
   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );

   if( map3 ) {
      map7 = astCmpMap( ret, map3, 1, "" );
      ret = (AstMapping *) astAnnul( ret );
      map3 = astAnnul( map3 );
      ret = (AstMapping *) map7;
   } 

   map7 = astCmpMap( ret, map4, 1, "" );
   ret = (AstMapping *) astAnnul( ret );
   map4 = astAnnul( map4 );
   ret = (AstMapping *) map7;

/* Return. */
   return ret;

}

static AstMatrixMap *WcsMatrix( FitsStore *store, char s, const char *method, 
                                const char *class ){
/*
*  Name:
*     WcsMatrix

*  Purpose:
*     Create a MatrixMap for the transformation from "Ideal Pixel Coords" 
*     to intermediate co-ordinates ("Relative Physical Coords").

*  Type:
*     Private function.

*  Synopsis:
*     AstMatrixMap *WcsMatrix( FitsStore *store, char s, const char *method,
*                              const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A MatrixMap representing the FITS "CD" matrix is returned.

*  Parameters:
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     s
*        A character s identifying the co-ordinate version to use. A space 
*        means use primary axis descriptions. Otherwise, it must be an 
*        upper-case alphabetical characters ('A' to 'Z').
*     axlat 
*        Zero based index of latitude axis, or -1 if no latitude axis
*        exists.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created MatrixMap or a NULL pointer if an 
*     error occurred.

*/

/* Local Variables: */
   AstMatrixMap *new;       /* The created MatrixMap */
   double *el;              /* Pointer to next matrix element */
   double *mat;             /* Pointer to matrix array */
   int i;                   /* Pixel axis index */
   int j;                   /* Intermediate axis index. */
   int naxis;               /* No. of axes */

/* Initialise/ */
   new = NULL;

/* Check the global status. */
   if ( !astOK ) return new;

/* Store the number of axes. */
   naxis = store->naxis;   

/* Allocate memory for the matrix. */
   mat = (double *) astMalloc( sizeof(double)*naxis*naxis );
   if( astOK ){

/* Fill the matrix with values from the FitsStore. */
      el = mat;
      for( j = 0; j < naxis; j++ ){
         for( i = 0; i < naxis; i++ ){

/* Get the CDj_i value for this axis. Missing terms can be defaulted so
   do not report an error if the required value is not present in the 
   FitsStore. */
            *el = GetItem( &(store->cd), j, i, s, NULL, method, class );

/* Diagonal terms default to 1.0. Off-diagonal terms default to 0.0. */
            if( *el == AST__BAD ){
               if( i == j ) {
                  *el = 1.0;
               } else {
                  *el = 0.0;
               }
            }

/* Move on to the next matrix element. */
            el++;
         }
      }

/* Create the matrix. */
      new = astMatrixMap( naxis, naxis, 0, mat, "" );

/* Release the memory used to hold the matrix. */
      mat = (double *) astFree( (void *) mat );

   }

/* If an error has occurred, attempt to annul the returned MatrixMap. */
   if( !astOK ) new = astAnnul( new );

/* Return the MatrixMap. */
   return new;

}

static AstCmpMap *WcsNative( AstFitsChan *this, FitsStore *store, char s, 
                             AstWcsMap *wcsmap, const char *method, 
                             const char *class ){
/*
*  Name:
*     WcsNative

*  Purpose:
*     Create a CmpMap which transforms Native Spherical Coords to
*     Celestial Coords.

*  Type:
*     Private function.

*  Synopsis:
*     AstCmpMap *WcsNative( AstFitsChan *this, FitsStore *store, char s, 
*                           AstWcsMap *wcsmap, const char *method, 
*                           const char *class )

*  Class Membership:
*     FitsChan

*  Description:
*     A CmpMap is created which rotates the supplied Native Spherical Coords
*     into Celestial Coords in the standard system specified by the CTYPE 
*     keywords. The reference values are added on to any non-celestial axes 
*     to produce absolute physical coordinates.
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
*     this
*        The FitsChan.
*     store
*        A structure containing values for FITS keywords relating to 
*        the World Coordinate System.
*     s
*        Co-ordinate version character to use (space means primary axes).
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
   char buf[150];             /* Message buffer */
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

/* Get the indices of the celestial axes. */
   axlon = astGetWcsAxis( wcsmap, 0 );
   axlat = astGetWcsAxis( wcsmap, 1 );

/* If there is no longitude or latitude axis, just add on the reference
   value to all axes. */
   if( axlon == axlat ){
      new = (AstCmpMap *) WcsAddRef( store, naxis, s, NULL, method, class );

/* If there is a lon/lat axis pair, create the inperm and outperm arrays
   which will be needed later to create the PermMap which reorganises
   the axes so that axis zero is the longitude axis and axis 1 is the
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
   point, in radians. */
      delta0 = AST__DD2R*GetItem( &(store->crval), axlat, 0, s, 
                           FormatKey( "CRVAL", axlat + 1, -1, s ), method, class );
      alpha0 = AST__DD2R*GetItem( &(store->crval), axlon, 0, s, 
                           FormatKey( "CRVAL", axlon + 1, -1, s ), method, class );

/* Limit the latitude to the range +/- PI/2, issuing a warning if the
   supplied CRVAL value is outside this range. */
      alphap = slaDrange( delta0 );  
      delta0 = alphap;
      if ( delta0 > AST__DPIBY2 ){
         delta0 = AST__DPIBY2;
      } else if ( delta0 < -AST__DPIBY2 ){
         delta0 = -AST__DPIBY2;
      }
      if( alphap != delta0 ) {
         sprintf( buf, "The original FITS header specified a reference "
                  "point with latitude %.*g. A value of %.*g is being used "
                  "instead. ", DBL_DIG, alphap*AST__DR2D, DBL_DIG, 
                  delta0*AST__DR2D );
         Warn( this, "badlat", buf, method, class );
      }

/* Store the radian values of the FITS keywords LONPOLE and LATPOLE. Defaults
   will be used if either of these items was not supplied. */
      phip = GetItem( &(store->lonpole), 0, 0, s, NULL, method, class );
      if( phip != AST__BAD ) phip *= AST__DD2R;

      latpole = GetItem( &(store->latpole), 0, 0, s, NULL, method, class );
      if( latpole != AST__BAD ) latpole *= AST__DD2R;

/* Find the standard Celestial Coordinates of the north pole of the Native
   Spherical Coordinate system. Report an error if the position was not
   defined. */
      if( !WcsNatPole( this, wcsmap, alpha0, delta0, latpole, &phip, &alphap,
                       &deltap ) && astOK ){
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
   by -phip (i.e. LONPOLE) about the Z axis. This puts the north pole of the 
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
         winmap = WcsAddRef( store, nax_rem, s, outperm, method, class );
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

static int WcsNatPole( AstFitsChan *this, AstWcsMap *wcsmap, double alpha0, 
                       double delta0, double latpole, double *phip, 
                       double *alphap, double *deltap ){
/*
*  Name:
*     WcsNatPole

*  Purpose:
*     Find the position of the north pole of the Native Spherical
*     Coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     int WcsNatPole( AstFitsChan *this, AstWcsMap *wcsmap, double alpha0, 
*                     double delta0, double latpole, double *phip, 
*                     double *alphap, double *deltap )

*  Class Membership:
*     FitsChan

*  Description:
*     The supplied WcsMap converts projected positions given in
*     "Relative Physical Coords" to positions in the "Native Spherical 
*     Coordinate" system. This function finds the pole of this spherical
*     coordinate system in terms of the standard Celestial Coordinate 
*     system to which the CRVALi, LONPOLE and LATPOLE keywords refer (this
*     system should be identified by the last 4 characters of the CTYPEi 
*     keywords). It also supplies a default value for LONPOLE if no value
*     has been supplied explicitly in the FITS header.
*
*     This function implements equations 8, 9 and 10 from the WCS paper by
*     Calabretta & Greisen (plus the associated treatment of special cases).
*     The paper provides more detailed documentation for the mathematics
*     implemented by this function.

*  Parameters:
*     this
*        The FitsChan in which to store any warning cards.
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
*        keyword LONPOLE, or the symbolic constant AST__BAD if the keyword was 
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
   char buf[150];                  /* Buffer for warning message */
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

/* If no value was supplied for the FITS keyword LONPOLE, set up a default 
   value such that the celestial latitude increases in the same direction
   as the native latitude at the reference point. */
   if( *phip == AST__BAD ){
      if( delta0 > theta0 ){
         *phip = 0.0;
      } else {
         *phip = AST__DPI;
      }

/* Issue a warning that a default lonpole value has been adopted. */
      sprintf( buf, "The original FITS header did not specify the "
               "longitude of the native north pole. A default value "
               "of %.8g degrees was assumed.", (*phip)*AST__DR2D );
      Warn( this, "nolonpole", buf, "astRead", "FitsChan" );

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

/* Issue a warning that a default latpole value has been adopted. */
                     sprintf( buf, "The original FITS header did not specify "
                              "the latitude of the native north pole. A "
                              "default value of %.8g degrees was assumed.",
                              (*deltap)*AST__DR2D );
                     Warn( this, "nolatpole", buf, "astRead", "FitsChan" );

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
               if( ( fabs( t1 ) > TOL2 ) || ( fabs( t2 ) > TOL2 ) ){
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

static int WcsNoWcs( AstMapping *map, AstFrame *phyfrm, int naxis, 
                     double *pixref, FitsStore *store, double *dim,
                     char s, const char *method, const char *class ){
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
*     int WcsNoWcs( AstMapping *map, AstFrame *phyfrm, int naxis, 
*                   double *pixref, FitsStore *store, double *dim,
*                   char s, const char *method, const char *class );

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function stores values describing FITS-WCS keywords within the
*     given FitsStore structure. The values describe the relationship between 
*     pixel coordinates and a set of simple linear axes representing physical 
*     coordinates. No WCS projection is involved. 

*  Parameters:
*     map
*        Pointer to the Mapping from pixel coordinates to physical
*        coordinates. This Mapping should have equal numbers of input and
*        output coordinates.
*     phyfrm
*        Pointer to physical coordinate Frame.
*     naxis
*        The number of input and output coordinates for "map".
*     pixref
*        Pointer to an array holding the pixel coordinates of the
*        reference point. If these are not known, a NULL pointer can be
*        supplied in which case the reference position is put at the
*        centre of the image (if the image dimensions are stored in the
*        FitsChan), or at the origin of pixel coordinates.
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords. 
*     dim 
*        Pointer to an array of pixel axis dimensions. Individual elements 
*        may be AST__BAD if dimensions are not known.
*     s
*        A character identifying the co-ordinate version to use. A space 
*        means use primary axis descriptions. Otherwise, it must be an 
*        upper-case alphabetical characters ('A' to 'Z').
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     One if the Mapping could be described by FITS-WCS, zero otherwise.

*  Notes:
*     -  Values which would take the correct default value if omitted
*     from the FITS header are given a bad value in "store".

*/

/* Local Variables: */
   AstPointSet *pset1;      /* PointSet holding pixel coordinates */
   AstPointSet *pset2;      /* PointSet holding physical coordinates */
   char combuf[80];         /* Buffer for card comment */
   double **ptr1;           /* Pointer to pixel coordinate data */
   double **ptr2;           /* Pointer to physical coordinate data */
   double *a;               /* Pointer to next matrix element */
   double *matrix;          /* Pointer to matrix array */
   double cd;               /* CD value */
   double crval;            /* CRVAL value */
   double cdelt;            /* CDELT value for this row */
   double delta;            /* Increment between positions on current axis */
   double maxerr;           /* Max. allowed squared distance between positions */
   double s2;               /* Sum of squared values */
   int i,j;                 /* Loop counts */
   int ret;                 /* Can the Mapping be described by FITS-WCS? */

/* Initialise */
   ret = 0;

/* Check the global status. */
   if( !astOK ) return ret;

/* Create a PointSet to hold two points and get pointers to the data. */
   pset1 = astPointSet( 2, naxis, "" );
   ptr1 = astGetPoints( pset1 );

/* Check the pointers can be used. */
   if( astOK ){

/* Choose the reference pixel. Since the Mapping is assumed to be
   linear, the choice of reference pixel is not critical. If the "pixref"
   array has been supplied use the supplied coordinates. Otherwise, if the 
   image dimensions have been supplied, place the reference pixel at the 
   middle of the array. Otherwise, put it at the origin. Store the reference 
   pixel in the PointSet. */
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

            SetItem( &(store->crpix), 0, i, s, ptr1[ i ][ 0 ] );
            SetItem( &(store->crval), i, 0, s, ptr2[ i ][ 0 ] );

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
   if( maxerr != AST__BAD && LinearMap( map, naxis, dim, matrix, maxerr ) ){

/* Store any non-default CD matrix elements in store. */
      for( j = 0; j < naxis; j++ ) { 
         a = matrix + j*naxis;
         for( i = 0; i < naxis; i++ ) {
            if( ( i == j && !EQUAL( *a, 1.0 ) )||
                ( i != j && !EQUAL( *a, 0.0 ) ) ) {
               SetItem( &(store->cd), j, i, s, *a );
            }
            a++;
         }
      }

/* Store values for the other axis-specific keywords. */
      for( j = 0; j < naxis; j++ ) {

/* The axis symbols are taken as the CTYPE values. */
         SetItemC( &(store->ctype), j, s, (char *) astGetSymbol( phyfrm, j ) );

/* The axis labels are taken as the comment for the CTYPE keywords (but only 
   if a label has been set). */
         if( astTestLabel( phyfrm, j ) ){
            SetItemC( &(store->ctype_com), j, s, (char *) astGetLabel( phyfrm, j ) );
         } else {
            sprintf( combuf, "Quantity represented by axis %d", j + 1 );
            SetItemC( &(store->ctype_com), j, s, combuf );
         }

/* If a value has been set for the axis units, use it as CUNIT. */
         if( astTestUnit( phyfrm, j ) ){
            SetItemC( &(store->cunit), j, s, (char *) astGetUnit( phyfrm, j ) );
         }
      }

/* Set CRVAL values which are very small compared to the pixel size to
   zero. */
      for( j = 0; j < naxis; j++ ) {
         crval = GetItem( &(store->crval), j, 0, s, NULL, method, class );
         if( crval != AST__BAD ) {

            cdelt = 0.0;
            for( i = 0; i < naxis; i++ ){
               cd = GetItem( &(store->cd), j, i, s, NULL, method, class );
               if( cd == AST__BAD ) cd = ( i == j ) ? 1.0 : 0.0;
               cdelt += cd;
            }

            if( fabs( crval ) < sqrt( DBL_EPSILON )*fabs( cdelt ) ){
               SetItem( &(store->crval), j, 0, s, 0.0 );
            }
         }
      }

/* Store the Domain name as the WCSNAME keyword (if set). */
      if( astTestDomain( phyfrm ) ) { 
         SetItemC( &(store->wcsname), 0, s, (char *) astGetDomain( phyfrm ) );
      }

/* Indicate that the FrameSet has been succesfully represented using
   FITS-WCS. */
      ret = 1;
   }

/* If an error has occurred, indicate that the Mapping cannot be
   described by FITS-WCS. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;

}

static AstWinMap *WcsShift( FitsStore *store, char s, const char *method,
                            const char *class ){
/*
*  Name:
*     WcsShift

*  Purpose:
*     Create a WinMap which shifts pixels coordinates so that their origin
*     is at the reference pixel.

*  Type:
*     Private function.

*  Synopsis:
*     AstWinMap *WcsShift( FitsStore *store, char s, const char *method,
*                          const char *class )

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
*     s
*        A character identifying the co-ordinate version to use. A space 
*        means use primary axis descriptions. Otherwise, it must be an 
*        upper-case alphabetical characters ('A' to 'Z').
*     method
*        A pointer to a string holding the name of the calling method.
*        This is used only in the construction of error messages.
*     class
*        A pointer to a string holding the class of the object being
*        read. This is used only in the construction of error messages.

*  Returned Value:
*     A pointer to the created WinMap or a NULL pointer if an 
*     error occurred.

*  Notes:
*     -  If an error occurs, a NULL pointer is returned.

*/

/* Local Variables: */
   AstWinMap *new;                 /* The created WinMap */
   int i;                          /* Axis index */
   double crpix;                   /* CRPIX keyword value */
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
   corresponding values with the CRPIX values subtracted off. */
      for( i = 0; i < store->naxis; i++ ){

/* Get the CRPIX value for this axis. */
         crpix = GetItem( &(store->crpix), 0, i, s, 
                          FormatKey( "CRPIX", i + 1, -1, s ), method, class );

/* Store the corner co-ordinates. */ 
         c1_in[ i ] = 0.0;
         c2_in[ i ] = 1.0;
         c1_out[ i ] = -crpix;
         c2_out[ i ] = 1.0 - crpix;
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

static void WCSToStore( AstFitsChan *this, AstFitsChan *trans, 
                        FitsStore *store, const char *method, 
                        const char *class ){
/*
*  Name:
*     WCSToStore

*  Purpose:
*     Extract WCS information from the supplied FitsChan using a FITSWCS
*     encoding, and store it in the supplied FitsStore.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitschan.h"
*     void WCSToStore( AstFitsChan *this, AstFitsChan *trans, 
*                      FitsStore *store, const char *method, 
*                      const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     A FitsStore is a structure containing a generalised represention of
*     a FITS WCS FrameSet. Functions exist to convert a FitsStore to and
*     from a set of FITS header cards (using a specified encoding), or
*     an AST FrameSet. In other words, a FitsStore is an encoding-
*     independant intermediary staging post between a FITS header and 
*     an AST FrameSet.
*
*     This function extracts FITSWCS keywords from the supplied FitsChan(s), 
*     and stores the corresponding WCS information in the supplied FitsStore.
*     Keywords will be searched for first in "trans", and then, if they
*     are not found in "trans", they will be searched for in "this".

*  Parameters:
*     this
*        Pointer to the FitsChan containing the cards read from the
*        original FITS header. This may include non-standard keywords.
*     trans
*        Pointer to a FitsChan containing cards representing standard
*        translations of any non-standard keywords in "this". A NULL
*        pointer indicates that "this" contains no non-standard keywords.
*     store
*        Pointer to the FitsStore structure.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Read all usable cards out of the main FitsChan, into the FitsStore. */
   WCSFcRead( this, store, method, class );

/* If a FitsChan containing standard translations was supplied, read all 
   cards out of it, into the FitsStore, potentially over-writing the
   non-standard values stored in the previous call to WCSFcRead. */
   if( trans ) WCSFcRead( trans, store, method, class );

}

static int WcsWithWcs( AstFitsChan *this, AstMapping *map1, AstMapping *map2, 
                       AstMapping *map3, AstFrame *phyfrm, int naxis, 
                       FitsStore *store, double *dim, char s, 
                       const char *method, const char *class ){
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
*                     AstMapping *map3, AstFrame *phyfrm, int naxis, 
*                     FitsStore *store, double *, char s, const char *method, 
*                     const char *class )

*  Class Membership:
*     FitsChan member function.

*  Description:
*     This function creates FITS-WCS keyword values describing the
*     relationship between pixel coordinates and a set of simple axes 
*     including a pair of celestial longitude/latitude axes, together with 
*     an arbitrary number of simple linear axis representing other physical 
*     coordinates. 

*  Parameters:
*     this
*        The FitsChan. Maybe NULL if no FitsChan is available.
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
*     store
*        Pointer to the FitsStore structure holding the values to use for 
*        the WCS keywords. 
*     dim 
*        Pointer to an array of pixel axis dimensions. Individual elements 
*        will be AST__BAD if dimensions are not known.
*     s
*        Co-ordinate version character.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

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
   double *work;            /* Pointer to workspace */
   double alpha0;           /* Long. of ref. point in standard system */
   double alphap;           /* Celestial longitude of native north pole */
   double delta0;           /* Lat. of ref. point in standard system */
   double error;            /* Difference between values */
   double latpole;          /* Native latitude of celestial north pole */
   double lonpole;          /* Native longitude of celestial north pole */
   double pv;               /* Projection parameter value */
   double s2;               /* Sum of squared values */
   double sky_lat;          /* Normalised latitude at reference point */
   double sky_long;         /* Normalised longitude at reference point */
   double val;              /* Keyword value */
   int axis;                /* Axis index within skyframe */
   int axlat;               /* Zero-based index of latitude axis */
   int axlon;               /* Zero-based index of longitude axis */
   int celsys;              /* Is there a celestial coordinate pair? */
   int i,j;                 /* Loop count */
   int ok;                  /* Can the Mapping be used? */
   int ret;                 /* Can the Mapping be described by FITS-WCS? */

/* Initialise */
   ret = 0;

/* Check the global status. */
   if( !astOK ) return ret;

/* Allocate some work space */
   work = (double *) astMalloc( sizeof(double)*naxis );

/* Create two PointSets to hold two points each. */
   pset1 = astPointSet( 2, naxis, "" );
   ptr1 = astGetPoints( pset1 );
   pset2 = astPointSet( 2, naxis, "" );
   ptr2 = astGetPoints( pset2 );

/* Check the projection parameters and pointers can be used. */
   if( astOK ){

/* The reference pixel is mapped onto the origin in the relative physical 
   coordinate system produced by "map1". Store the coordinates of the 
   origin, and then transform them using the inverse of the "map1" Mapping 
   to get the pixel coordinates at the reference point. Store these in 
   "store". */
      for( i = 0; i < naxis; i++ ) {
         ptr1[ i ][ 0 ] = 0.0;
         ptr1[ i ][ 1 ] = 0.0;
      }
      astTransform( map1, pset1, 0, pset2 );
      for( i = 0; i < naxis; i++ ) {
         work[ i ] = ptr2[ i ][ 0 ];
         SetItem( &(store->crpix), 0, i, s, work[ i ] );
      }
   }

/* Attempt to calculate and store the keyword values relating to the 
   conversion from pixel coordinates to relative physical coordinates.
   This conversion is defined by "map1" and must be linear. The pixel 
   coordinates of the reference point are fixed at the values found above. */
   if( WcsNoWcs( map1, phyfrm, naxis, work, store, dim, s, method, class ) ){

/* Obtain the indices of the longitude and latitude axes. */
      axlon = astGetWcsAxis( map2, 0 );
      axlat = astGetWcsAxis( map2, 1 );

/* The reference point in the physical co-ordinate system is found by
   transforming the reference point in native spherical co-ordinates
   into absolute physical coordinates using map3. Note, the reference 
   pixel given by CRPIX does not necessarily map onto physical 
   reference point given by CRVAL. For instance, the two points will be
   different for a TAN projection in which any of the PVi_0 projection
   parameters are not zero. */
      for( i = 0; i < naxis; i++ ) ptr1[ i ][ 0 ] = 0.0;
      ptr1[ axlat ][ 0 ] = astGetNatLat( map2 );
      astTransform( map3, pset1, 1, pset2 );
      for( i = 0; i < naxis; i++ ) {
         SetItem( &(store->crval), i, 0, s, ptr2[ i ][ 0 ] );
      }

/* The following only needs to be done if the WcsMap includes a pair of
   longitude/latitude axes. */
      celsys = ( axlon != -1 && axlat != -1 );
      if( celsys && astOK ){

/* Normalise the latitude and longitude values at the reference point. The
   longitude and latitude values stored above will be in radians, but after
   normalization we convert them to degrees, as expected by other
   functions which handle FitsStores. */
         sky_long = GetItem( &(store->crval), axlon, 0, s, NULL, method, 
                             class );
         sky_lat = GetItem( &(store->crval), axlat, 0, s, NULL, method, 
                             class );

         if( ZEROANG( sky_long ) ) sky_long = 0.0;
         if( ZEROANG( sky_lat  ) ) sky_lat = 0.0;

         sky_long = slaDranrm( sky_long );
         sky_lat = slaDrange( sky_lat );

         if ( sky_lat > AST__DPIBY2 ) {
            sky_long += ( sky_long < AST__DPI ) ? AST__DPI : -AST__DPI ;
            sky_lat = AST__DPI - sky_lat;

         } else if ( sky_lat < -AST__DPIBY2 ) {
            sky_long += ( sky_long < AST__DPI ) ? AST__DPI : -AST__DPI;
            sky_lat = -AST__DPI - sky_lat;
         }

         SetItem( &(store->crval), axlon, 0, s, AST__DR2D*sky_long );
         SetItem( &(store->crval), axlat, 0, s, AST__DR2D*sky_lat );

/* Store the WCS projection parameters. */
         for( i = 0; i < 99; i++ ){
            pv = astGetPV( map2, axlon, i );
            if( pv != AST__BAD ) SetItem( &(store->pv), axlon, i, s, pv );
            pv = astGetPV( map2, axlat, i );
            if( pv != AST__BAD ) SetItem( &(store->pv), axlat, i, s, pv );
         }

/* Scale the CD terms referring to the celestial axis so that they create
   degrees instead of radians. */
         for( i = 0; i < naxis; i++ ){
            val = GetItem( &(store->cd), axlat, i, s, NULL, method, class );
            if( val != AST__BAD ) SetItem( &(store->cd), axlat, i, s, 
                                           val*AST__DR2D );
         }

         for( i = 0; i < naxis; i++ ){
            val = GetItem( &(store->cd), axlon, i, s, NULL, method, class );
            if( val != AST__BAD ) SetItem( &(store->cd), axlon, i, s, 
                                           val*AST__DR2D );
         }

      }

/* Form the increments along each physical axis produced by a movement of
   one pixel along every pixel axis. */
      if( astOK ){
         for( j = 0; j < naxis; j++ ){
            work[ j ] = 0.0;
            for( i = 0; i < naxis; i++ ){
               val = GetItem( &(store->cd), j, i, s, NULL, method, class );
               if( val != AST__BAD ){
                  work[ j ] += val;
               } else if( i == j ){
                  work[ j ] += 1.0;
               }
            }
         }
      }

/* Store the squared arc-size of a pixel in celestial coordinates. These are 
   used to determine the accuracy required for the Mappings. */
      if( celsys && astOK ) {
         s2 = work[ axlat ]*work[ axlat ] + work[ axlon ]*work[ axlon ];
      } else {
         s2 = 0.0;
      }

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
               if( fabs( error ) >= 0.1*fabs( work[ i ] ) ){
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
            if( celsys && astOK ){

/* We now calculate the longitude and latitude of the celestial north pole 
   in native spherical coordinates (using the inverse of map3). These values 
   correspond to the LONPOLE and LATPOLE keywords. */
               for( i = 0; i < naxis; i++ ){
                  ptr2[ i ][ 0 ] = 0.0;
                  ptr2[ i ][ 1 ] = 0.0;
               }
               ptr2[ axlat ][ 0 ] = AST__DPIBY2;
               astTransform( map3, pset2, 0, pset1 );

/* Retrieve the latitude and longitude (in the standard system) of the 
   reference point, in radians. */
               delta0 = AST__DD2R*GetItem( &(store->crval), axlat, 0, s, 
                                           NULL, method, class );
               alpha0 = AST__DD2R*GetItem( &(store->crval), axlon, 0, s, 
                                           NULL, method, class );

/* The default value of the LATPOLE is defined by equation 8 of the
   Fits-WCS paper by Greisen & Calabretta (taking the +ve signs). Find
   this value. */
               if( WcsNatPole( this, (AstWcsMap *) map2, alpha0, delta0, 
                               999.0, ptr1[ axlon ], &alphap, &latpole ) ){

/* If the default value is defined, compare it to the latitude of the 
   north pole found above. If they are equal use a bad value instead to
   prevent an explicit keyword from being added to the FitsChan. */
                  if( EQUALANG( ptr1[ axlat ][ 0 ], latpole ) ) {
                     latpole = AST__BAD;
                  } else {
                     latpole = ptr1[ axlat ][ 0 ];
                  }

/* If the default value is not defined, always store an explicit LATPOLE 
   value. */
               } else {
                  latpole = ptr1[ axlat ][ 0 ];
               }

/* The default LONPOLE value is zero if the celestial latitude at the 
   reference point is greater than the native latitude at the reference
   point. Otherwise, the default is (+ or -) 180 degrees. If LONPOLE takes 
   the default value, replace it with AST__BAD to prevent an explicit keyword
   being stored in the FitsChan. */
               if( delta0 > astGetNatLat( map2 ) ){
                  if( EQUALANG( ptr1[ axlon ][ 0 ], 0.0 ) ){
                     lonpole = AST__BAD;
                  } else {
                     lonpole = ptr1[ axlon ][ 0 ];
                  } 
   
               } else {
                  if( EQUALANG( ptr1[ axlon ][ 0 ], AST__DPI ) ||
                      EQUALANG( ptr1[ axlon ][ 0 ], -AST__DPI ) ){
                     lonpole = AST__BAD;
                  } else {
                     lonpole = ptr1[ axlon ][ 0 ];
                  } 
               }

/* Store these values. */
               if( lonpole != AST__BAD ){
                  SetItem( &(store->lonpole), 0, 0, s, lonpole*AST__DR2D );
               }

               if( latpole != AST__BAD ){
                  SetItem( &(store->latpole), 0, 0, s, latpole*AST__DR2D );
               }

/* Get a pointer to the SkyFrame defining the celestial axes. */
               astPrimaryFrame( phyfrm, axlon, (AstFrame **) &skyfrm, &axis );

/* Store the CTYPE, EQUINOX, MJDOBS, and RADESYS values. */
               SkySys( skyfrm, astGetWcsType( map2 ), store, 
                       axlon, axlat, s, method, class );

/* Annul the SkyFrame. */
               skyfrm = astAnnul( skyfrm );
            }
         }
      }
   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Free the work space */
   work = (double *) astFree( (void *) work );

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
*     - The Base Frame in the FrameSet is used as the pixel Frame, and
*     the Current Frame is used to create the primary axis descriptions.
*     Attempts are made to create secondary axis descriptions for any 
*     other Frames in the FrameSet (up to a total of 26).
*/

/* Local Variables: */
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   FitsStore *store;             /* Intermediate storage for WCS information */
   char banner[ FITSCARDLEN - FITSNAMLEN + 1 ]; /* Buffer for begin/end banner */
   const char *class;            /* Pointer to string holding object class */
   const char *method;           /* Pointer to string holding calling method */
   double *dim;                  /* Pointer to array of axis dimensions */
   int axlat;                    /* Index of latitude axis */
   int axlon;                    /* Index of longitude axis */
   int card0;                    /* Index of original current card */
   int comm;                     /* Value of Comm attribute */
   int encoding;                 /* FITS encoding scheme to use */
   int i;                        /* Axis index */
   int naxis;                    /* No. of pixel axes */
   int ret;                      /* Number of objects read */

/* Initialise. */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Obtain a pointer to the FitsChan structure. */
   this = (AstFitsChan *) this_channel;

/* Store the calling method, and object class. */
   method = "astWrite";
   class = astGetClass( this );

/* The original current card is re-instated at the end if no object
   is written. Save its index. */
   card0 = astGetCard( this );

/* Indicate that all cards added to the FitsCHan by this call should be
   marked as "new". */
   MarkNew = 1;

/* Get the encoding scheme used by the FitsChan. */
   encoding = astGetEncoding( this );

/* First deal with cases where we are writing to a FitsChan in which AST 
   objects are encoded using native AST-specific keywords... */
   if( encoding == NATIVE_ENCODING ){

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
   objects are encoded using any of the supported foreign encodings... */
   } else {

/* Only proceed if the supplied object is a FrameSet. */
      if( astIsAFrameSet( object ) ){

/* Note the number of pixel (i.e. Base Frame) axes, and allocate memory to 
   hold the image dimensions. */
         naxis = astGetNin( (AstFrameSet *) object );
         dim = (double *) astMalloc( sizeof(double)*naxis );
         if( dim ){

/* Note the image dimensions, if known. If not, store AST__BAD values. */
            for( i = 0; i < naxis; i++ ){
               if( !astFitsGetF( this, FormatKey( "NAXIS", i + 1, -1, ' ' ), 
                                 dim + i ) ) dim[ i ] = AST__BAD;
            }

/* Extract the required information from the FrameSet into a standard
   intermediary structure called a FitsStore. The indices of any
   celestial axes are returned. */
            store = FsetToStore( (AstFrameSet *) object, naxis, dim,
                                 &axlat, &axlon, method, class );

/* If the FrameSet cannot be described in terms of any of the supported
   FITS encodings, a null pointer will have been returned. */
            if( store ){

/* Now put header cards describing the contents of the FitsStore into the 
   supplied FitsChan, using the requested encoding. Zero or one is
   returned depending on whether the information could be encoded. */
               ret = FitsFromStore( this, store, encoding, axlat, axlon, 
                                    method, class );

/* Release the resources used by the FitsStore. */
               store = FreeStore( store );      

/* If the Object was written to the FitsChan, set the current card to
   end-of-file. */
               if( ret ) astSetCard( this, INT_MAX );
            }

/* Free workspace holding image dimensions */
            dim = (double *) astFree( (void *) dim );
         }
      }
   }

/* If an error has occurred, return zero and remove any new cards added
   to the FitsCHan by this call. */
   if( !astOK ) ret = 0;

/* Clear the new flag associated with cards which have been added to the 
   FitsChan as a result of this function. If the object was not added 
   succesfully to the FitsChan, remove any cards which were added before 
   the error was discovered. */
   FixNew( this, NEW1, !ret, method, class );
   FixNew( this, NEW2, !ret, method, class );

/* Indicate that all cards added to the FitsChan from now on should not be
   marked as "new". */
   MarkNew = 0;

/* If no object was written, re-instate the original current card. */
   if( !ret ) astSetCard( this, card0 );

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
   int old_ignoreused;          /* Original value of external variable IgnoreUsed */

/* Check the global status. */
   if( !astOK ) return;

/* Only proceed if a sink function and wrapper were supplied. */
   if( this->sink && this->sink_wrap ){

/* Store the current card index. */
      icard = astGetCard( this );

/* Indicate that cards which have been read into an AST object should skipped 
   over by the functions which navigate the linked list of cards. */
      old_ignoreused = IgnoreUsed;
      IgnoreUsed = 1;

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
      IgnoreUsed = old_ignoreused;

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
   char *c;                      /* Pointer to next buffer character */
   char buff1[ FITSCARDLEN - FITSNAMLEN - 3 ]; /* Buffer for a single substring */
   char buff2[ FITSCARDLEN - FITSNAMLEN - 3 ]; /* Buffer for pre-quoted string */
   char cc;                      /* Next character */
   char keyword[ FITSNAMLEN + 1 ]; /* Buffer for FITS keyword */
   const char *start;            /* Pointer to start of substring */
   int first;                    /* Is this the first sub-string? */
   int nc;                       /* No. of available columns remaining */

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

/* Store a pointer to the start of the next sub-string (i.e. the
   beggining of the string), and then loop round until the end of the
   string is reached. */
      start = value;
      first = 1;
      while( *start && astOK ){

/* Store the number of characters available in the 80 column header card 
   for the next substring, leaving room for the "= " string at the start,
   and the delimiting quotes. Also reserve 2 characters to allow for the
   possibility of double quotes being needed to protect trailing white space
   (see function PreQuote). */
         nc = FITSCARDLEN - FITSNAMLEN - 6;

/* If this is the first sub-string reserve room for any comment. */
         if( first ){
            if( comment && comment[0] ) nc -= ChrLen( comment ) + 3;

/* If the first card will be turned into a comment card, we need to leave room
   for the keyword name and equals sign, etc, within the 80 columns. */
            if( !set ) nc -= FITSNAMLEN + 5;
         }

/* We need to check the sub-string for single quotes since these will
   take up 2 characters each instead of 1 when encoded since single quotes
   within a string are doubled. Search through from the starting
   character, copying the sub-string into a buffer, and reducing the number 
   of available characters remaining in the card for each character. */
         c = buff1;
         while( *start && nc > 0 ){
            cc = *(start++);
            *(c++) = cc;

            if( cc == '\'' ) {
               nc -= 2;
            } else {
               nc -= 1;
            }
         }

/* If the last character in the substring was a single quote, there may
   not have been room for the extra quote which is added when the
   sub-string is encoded. In this case we need to back up a character in
   order to remove the single quote frin this substring and move it into 
   the next sub-string. */
         if( nc < 0 ){
            start--;
            c--;
         }
 
/* If the supplied value has not been exhausted, append an ampersand to
   the string. In this case we need to move the last character in the
   substring into the next substring to make room for the ampersand. */
         if( *start ) {
            start--;
            c--;
            *(c++) = '&';
         }

/* Terminate the buffer. */
         *c = 0;         

/* The FITS standard considers trailing white space is be insignificant, 
   and so we need to guard against external applications throwing away
   significant trailing white space. This is done by encosing the string, 
   including trailing white space, in double quotes. */
         PreQuote( buff1, buff2 );

/* On the first pass through this loop, write the value to the FitsChan as 
   a keyword and value */
         if( first ){
            astFitsSetS( this, keyword, buff2,
                         astGetComment( this ) ? comment : NULL, 0 );

/* If the value is not "set", replace the card just written by a COMMENT
   card containing the text of the card as the comment. */
            if( !set ) MakeIntoComment( this, "astWrite", astGetClass( this ) );

/* On subsequent passes through the loop, store the string using a CONTINUE 
   keyword, with type AST__CONTINUE (this type is like AST__STRING but is 
   formatted without an equals sign). */
         } else {
            astFitsSetCN( this, "CONTINUE", buff2, NULL, 0 );
         }

         first = 0;
      }

/* Increment the count of items written. */
      items_written++;
   }
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
*     - "FITS-PC": Encodes coordinate system information in FITS
*     header cards using the conventions described in an earlier draft
*     of the FITS world coordinate system paper by E.W. Greisen and
*     M. Calabretta. This encoding uses a combination of CDELTi and
*     PCiiijjj keywords to describe the scale and rotation of the pixel
*     axes. This encoding is included to support existing data and  
*     software which uses these now superceded conventions. In general,
*     the "FITS-WCS" encoding (which uses CDi_j keywords to describe the
*     scale and rotation) should be used in preference to "FITS-PC".
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
*     - "FITS-AIPS": Encodes coordinate system information in FITS
*     header cards using the conventions originally introduced by the 
*     AIPS data analysis facility. This is base on the use of CDELTi and
*     CROTAi keuwords to desribe the scale and rotation of each axis.
*     These conventions have been superceded but are still widely used.
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
*     "PCiiijjj", where "i" and "j" are single digits, then
*     FITS-PC encoding is used,
*     - Otherwise, if the FitsChan contains a keyword of the form
*     "CDiiijjj", where "i" and "j" are single digits, then
*     FITS-IRAF encoding is used,
*     - Otherwise, if the FitsChan contains a keyword of the form
*     "CDi_j", and at least one of RADECSYS, PROJPi, or CjVALi
*     where "i" and "j" are single digits, then FITS-IRAF encoding is 
*     used.
*     - Otherwise, if the FitsChan contains any keywords of the form
*     PROJPi, CjVALi or RADECSYS, where "i" and "j" are single digits, 
*     then FITS-PC encoding is used.
*     - Otherwise, if the FitsChan contains a keyword of the form
*     CROTAi, where "i" is a single digit, then FITS-AIPS encoding is 
*     used.
*     - Otherwise, if the FitsChan contains a keyword of the form
*     CDELTi, where "i" is a single digit, then FITS-PC encoding is 
*     used.
*     - Otherwise, if the FitsChan contains a keyword of the form
*     CRVALi, where "i" is a single digit, then FITS-WCS encoding is 
*     used.
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
*     model (this means the Mapping which relates its base and current 
*     Frames must include either a DssMap or a WcsMap with type 
*     AST__TAN).
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
*     conforms to the DSS model.  Specifically, the current Frame must 
*     be a default FK5 SkyFrame; the projection must be a tangent plane 
*     (gnomonic) projection with polynomial corrections conforming to 
*     DSS requirements, and north must be parallel to the second base
*     Frame axis.
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
*     2-dimensional) and one or more related "world coordinate systems".
*     Often, although not necessarily, one of these systems will be a 
*     celestial coordinate system, in which case the sequence of 
*     transformations which convert between the various intermediate 
*     coordinate systems will include a "sky projection" (e.g. as
*     implemented by a WcsMap).  The FITS-WCS encoding may only be
*     used to store a single AST Object in any set of FITS header
*     cards, and that Object must be a FrameSet which conforms to the
*     FITS-WCS model (the FrameSet may, however, contain multiple Frames).
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
*     - Sky projections can be represented only if any associated
*     projection parameters are set to their default values.
*     - Secondary axis descriptions are not supported, so when writing
*     a FrameSet to a FitsChan, only information from the base and
*     current Frames will be stored.
*
*     Note that this encoding is provided mainly as an interim measure to
*     provide a more stable alternative to the FITS-WCS encoding until the
*     FITS standard for encoding WCS information is finalised.  The name
*     "FITS-IRAF" indicates the general keyword conventions used and does
*     not imply that this encoding will necessarily support all features of
*     the WCS scheme used by IRAF software. Nevertheless, an attempt has
*     been made to support a few such features where they are known to be
*     used by important sources of data.
*
*     When writing a FrameSet using the FITS-IRAF encoding, axis rotations
*     are specified by a matrix of FITS keywords of the form "CDi_j", where
*     "i" and "j" are single digits. The alternative form "CDiiijjj", which
*     is also in use, is recognised when reading an Object, but is never
*     written.
*
*     In addition, the experimental IRAF "ZPX" and "TNX" sky projections will 
*     be accepted when reading, but will never be written (the corresponding
*     FITS "ZPN" or "TAN" projection being used instead). However, there
*     are restrictions on the use of these experimental projections. For
*     "ZPX", longitude and latitude correction surfaces (appearing as
*     "lngcor" or "latcor" terms in the IRAF-specific "WAT" keywords) are
*     not supported. For "TNX" projections, only cubic surfaces encoded as 
*     simple polynomials with "half cross-terms" are supported. If an
*     un-usable "TNX" or "ZPX" projection is encountered while reading 
*     from a FitsChan, a simpler form of TAN or ZPN projection is used
*     which ignores the unsupported features and may therefore be
*     inaccurate. If this happens, a warning message is added to the 
*     contents of the FitsChan as a set of cards using the keyword "ASTWARN".
*
*     You should not normally attempt to mix the FITS-IRAF, FITS-WCS,
*     FITS-AIPS or FITS-PC encodings within the same FitsChan, since there 
*     is a risk that keyword clashes may occur.

*  The FITS-PC Encoding:
*     The FITS-PC encoding can, for most purposes, be considered as
*     equivalent to the FITS-WCS encoding (above), although it differs
*     in the details of the FITS keywords used. It is used in exactly
*     the same way and has the same restrictions.

*  The FITS-AIPS Encoding:
*     The FITS-AIPS encoding can, for most purposes, be considered as
*     equivalent to the FITS-WCS encoding (above), although it differs
*     in the details of the FITS keywords used. It is used in exactly
*     the same way and has the same restrictions, but with the
*     addition of the following:
*
*     - The only celestial coordinate systems that may be represented
*     are equatorial, galactic and ecliptic,
*     - Sky projections can be represented only if any associated
*     projection parameters are set to their default values.
*     - The AIT, SFL and MER projections can only be written if the CRVAL 
*     keywords are zero for both longitude and latitude axes.
*     - Secondary axis descriptions are not supported, so when writing
*     a FrameSet to a FitsChan, only information from the base and
*     current Frames will be stored.
*     - If there are more than 2 axes in the base and current Frames, any 
*     rotation must be restricted to the celestial plane, and must involve 
*     no shear.

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
   value == FITSPC_ENCODING ||
   value == FITSWCS_ENCODING ||
   value == FITSIRAF_ENCODING ||
   value == FITSAIPS_ENCODING ||
   value == DSS_ENCODING ? value : 
   (astError( AST__BADAT, "astSetEncoding: Unknown encoding system %d "
              "supplied.", value ), UNKNOWN_ENCODING )))
astMAKE_TEST(FitsChan,Encoding,( this->encoding != UNKNOWN_ENCODING ))

/* DefB1950 */
/* ======== */
/*
*att++
*  Name:
*     DefB1950

*  Purpose:
*     Use FK4 B1950 as defaults?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which specifies a default equinox
*     and reference frame to use when reading a FrameSet from a FitsChan
*     with a foreign (i.e. non-native) encoding. It is only used if the FITS 
*     header contains no information about the reference frame or equinox. If
*     this is the case, then values of FK4 and B1950 are assumed if the DefB1950 
*     attribute has a non-zero value, and FK5 J2000 is assumed if DefB1950 is zero.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/
astMAKE_CLEAR(FitsChan,DefB1950,defb1950,-1)
astMAKE_GET(FitsChan,DefB1950,int,1,(this->defb1950 == -1 ? 1 : this->defb1950))
astMAKE_SET(FitsChan,DefB1950,int,defb1950,( value ? 1 : 0 ))
astMAKE_TEST(FitsChan,DefB1950,( this->defb1950 != -1 ))

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

/* Warnings. */
/* ======== */
/*
*att++
*  Name:
*     Warnings

*  Purpose:
*     Controls the issuing of warnings about various conditions.

*  Type:
*     Public attribute.

*  Synopsis:
*     String

*  Description:
*     This attribute controls the issuing of warnings about selected
*     conditions when an Object is read from or written to a FitsChan.
*     The value supplied for the Warnings attribute should consist of a
*     space separated list of condition names (see the AllWarnings
*     attribute for a list of the currently defined names). Each name 
*     indicates a condition which should be reported. The default 
*     value for Warnings is the string "Tnx Zpx BadCel".
*
*     The text of any warning will be stored within the FitsChan in the
*     form of one or more new header cards with keyword ASTWARN. If
*     required, applications can check the FitsChan for ASTWARN cards
c     (using astFindFits) after the call to astRead or astWrite has been
f     (using AST_FINDFITS) after the call to AST_READ or AST_WRITE has been
*     performed, and report the text of any such cards to the user. ASTWARN
*     cards will be propagated to any output header unless they are
c     deleted from the FitsChan using astDelFits.
f     deleted from the FitsChan using astDelFits.

*  Applicability:
*     FitsChan
*        All FitsChans have this attribute.
*att--
*/
/* Clear the Warnings value by freeing the allocated memory and assigning
   a NULL pointer. */
astMAKE_CLEAR(FitsChan,Warnings,warnings,astFree( this->warnings ))

/* If the Warnings value is not set, supply a default in the form of a
   pointer to the constant string "Tnx Zpx BadCel". */
astMAKE_GET(FitsChan,Warnings,const char *,NULL,( this->warnings ? this->warnings :
                                                            "Tnx Zpx BadCel" ))

/* Set a Warnings value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy.
   First check that the list does not contain any unknown conditions. If
   it does, an error is reported by GoodWarns and the current attribute value 
   is retained. */
astMAKE_SET(FitsChan,Warnings,const char *,warnings,( GoodWarns( value ) ? 
                                astStore( this->warnings, value, strlen( value ) + (size_t) 1 ) :
                                this->warnings))

/* The Warnings value is set if the pointer to it is not NULL. */
astMAKE_TEST(FitsChan,Warnings,( this->warnings != NULL ))

/* AllWarnings. */
/* ============ */
/*
*att++
*  Name:
*     AllWarnings

*  Purpose:
*     A list of all currently available condition names.

*  Type:
*     Public attribute.

*  Synopsis:
*     String, read-only

*  Description:
*     This read-only attribute is a space separated list of all the conditions
*     names recognized by the Warnings attribute. The names are listed
*     below.

*  Conditions:
*     The following conditions are currently recognised (all are
*     case-insensitive):
*
*     - "BadCel": This condition arises when reading a FrameSet from a
*     non-Native encoded FitsChan if an unknown celestial co-ordinate 
*     system is specified by the CTYPE keywords.
*
*     - "BadLat": This condition arises when reading a FrameSet from a
*     non-Native encoded FitsChan if the latitude of the reference point
*     has an absolute value greater than 90 degrees. The actual absolute
*     value used is set to exactly 90 degrees in these cases.
*
*     - "NoCTYPE": This condition arises if a default CTYPE value is used 
c     within astRead, due to no value being present in the supplied FitsChan.
f     within AST_READ, due to no value being present in the supplied FitsChan.
*     This condition is only tested for when using non-Native encodings.
*
*     - "NoEquinox": This condition arises if a default equinox value is used 
c     within astRead, due to no value being present in the supplied FitsChan.
f     within AST_READ, due to no value being present in the supplied FitsChan.
*     This condition is only tested for when using non-Native encodings.
*
*     - "NoRadesys": This condition arises if a default reference frame is 
c     used for an equatorial co-ordinate system within astRead, due to no 
f     used for an equatorial co-ordinate system within AST_READ, due to no 
*     value being present in the supplied FitsChan. This condition is only 
*     tested for when using non-Native encodings.
*
*     - "NoLonpole": This condition arises if a default value is used for 
c     the LONPOLE keyword within astRead, due to no value being present 
f     the LONPOLE keyword within AST_READ, due to no value being present 
*     in the supplied FitsChan. This condition is only tested for when 
*     using non-Native encodings.
*
*     - "NoLatpole": This condition arises if a default value is used for 
c     the LATPOLE keyword within astRead, due to no value being present 
f     the LATPOLE keyword within AST_READ, due to no value being present 
*     in the supplied FitsChan. This condition is only tested for when 
*     using non-Native encodings.
*
*     - "NoMjd-obs": This condition arises if a default value is used for 
c     the date of observation within astRead, due to no value being present 
f     the date of observation within AST_READ, due to no value being present 
*     in the supplied FitsChan. This condition is only tested for when using 
*     non-Native encodings.
*
*     - "Tnx": This condition arises if a FrameSet is read from a FITS
*     header containing an IRAF "TNX" projection which includes terms
*     not supproted by AST. Such terms are ignored and so the resulting
*     FrameSet may be inaccurate.
*
*     - "Zpx": This condition arises if a FrameSet is read from a FITS
*     header containing an IRAF "ZPX" projection which includes "lngcor" 
*     or "latcor" correction terms. These terms are not supported by AST
*     and are ignored. The resulting FrameSet may therefore be inaccurate.

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
   int *flags;
   int icard;
   int old_ignoreused;       /* Original value of external variable IgnoreUsed */
   
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
   old_ignoreused = IgnoreUsed;
   IgnoreUsed = 0;

/* Save the current card index in the input FitsChan. */
   icard = astGetCard( in );

/* Rewind the input FitsChan. */
   astClearCard( in );

/* Copy all the FitsCard structures from input to output. */
   while( !astFitsEof( in ) && astOK ){

/* Get a pointer to the flags mask for this card. */
      flags = CardFlags( in );

/* Store a new card in the output, holding the same information as the
   input card. */
      NewCard( out, CardName( in ), CardType( in ), CardData( in, NULL ), 
               CardComm( in ), (flags?(*flags):0) );

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

/* Reinstate the original setting of the external IgnoreUsed variable. */
   IgnoreUsed = old_ignoreused;

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
   AstFitsChan *this;            /* Pointer to the FitsChan structure */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   const char *class;            /* Object class */
   const char *sval;             /* Pointer to string value */
   int flags;                    /* Keyword flags */
   int icard;                    /* Index of current card */
   int ival;                     /* Integer value */
   int ncard;                    /* No. of cards dumped so far */
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

/* DefB1950 */
/* -------- */
   set = TestDefB1950( this );
   ival = set ? GetDefB1950( this ) : astGetDefB1950( this );
   astWriteInt( channel, "DfB1950", set, 1, ival, (ival ? "Default to FK4 B1950": "Default to FK5 J2000") );

/* Warnings. */
/* --------- */
   set = TestWarnings( this );
   sval = set ? GetWarnings( this ) : astGetWarnings( this );
   astWriteString( channel, "Warn", set, 1, sval, "Warnings to be reported" );

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

/* Write out the flag values if any are non-zero. */
      flags = *CardFlags( this );
      if( flags ){
         (void) sprintf( buff, "Fl%d", ncard );
         astWriteInt( channel, buff, 1, 1, flags, "FITS keyword flags" );
      }

/* Write out the data value, using the appropriate data type. */
      data = CardData( this, NULL );
      if( data ){

         if( type == AST__FLOAT ){
            (void) sprintf( buff, "Dt%d", ncard );
            astWriteDouble( channel, buff, 1, 1, *( (double *) data ), 
                            "FITS keyword value" );
   
         } else if( type == AST__STRING || type == AST__CONTINUE ){
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
      new->defb1950 = 1;
      new->fitsdigits = DBL_DIG;
      new->encoding = UNKNOWN_ENCODING;
      new->warnings = NULL;

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
   int flags;                    /* Keyword flags */
   int ival[2];                 /* Integer data values */
   int ncard;                   /* No. of FitsCards read so far */
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

/* DefB1950 */
/* -------- */
      new->defb1950 = astReadInt( channel, "dfb1950", DBL_DIG );
      if ( TestDefB1950( new ) ) SetDefB1950( new, new->defb1950 );

/* Warnings. */
/* --------- */
      new->warnings = astReadString( channel, "warn", NULL );

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

            } else if( type == AST__STRING || type == AST__CONTINUE ){
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

/* Get the keyword flags (only written by versions of AST later than 
   V1.4). These are packed into an int. */
            (void) sprintf( buff, "fl%d", ncard );
            flags = astReadInt( channel, buff, 0 );

/* If the flags were not found, use the keyword deletion flag written by 
   AST V1.4 and earlier. */
            if( !flags ) {
               (void) sprintf( buff, "dl%d", ncard );
               flags = astReadInt( channel, buff, 0 );
            }

/* Get the keyword comment. */
            (void) sprintf( buff, "cm%d", ncard );
            comment = astReadString( channel, buff, NULL );

/* Append a new card to the output FitsChan. */
            NewCard( new, keynm, type, data, comment, flags );

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

void astFitsSetCN_( AstFitsChan *this, const char *name, const char *value, 
                    const char *comment, int overwrite ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsChan,FitsSetCN))( this, name, value, comment, overwrite );
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

const char *astGetAllWarnings_( AstFitsChan *this ){
   if( !this ) return NULL;
   return (**astMEMBER(this,FitsChan,GetAllWarnings))( this );
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

int astFitsGetCN_( AstFitsChan *this, const char *name, char **value ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,FitsChan,FitsGetCN))( this, name, value );
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






