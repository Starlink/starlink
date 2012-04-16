/*
*class++
*  Name:
*     XmlChan

*  Purpose:
*     I/O Channel using XML to represent Objects.

*  Constructor Function:
c     astXmlChan
f     AST_XMLCHAN

*  Description:
*     A XmlChan is a specialised form of Channel which supports XML I/O
*     operations. Writing an Object to an XmlChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     XML description of that Object, and reading from an XmlChan will
*     create a new Object from its XML description.
*
*     Normally, when you use an XmlChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
c     by reading and writing the resulting XML text. These functions
f     and "sink" routines which connect it to an external data store
f     by reading and writing the resulting XML text. These routines
*     should perform any conversions needed between external character
c     encodings and the internal ASCII encoding. If no such functions
f     encodings and the internal ASCII encoding. If no such routines
*     are supplied, a Channel will read from standard input and write
*     to standard output.
*
*     Alternatively, an XmlChan can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.

*  Inheritance:
*     The XmlChan class inherits from the Channel class.

*  Attributes:
*     In addition to those attributes common to all Channels, every
*     XmlChan also has the following attributes:
*
*     - XmlFormat: System for formatting Objects as XML
*     - XmlLength: Controls output buffer length
*     - XmlPrefix: The namespace prefix to use when writing

*  Functions:
c     The XmlChan class does not define any new functions beyond those
f     The XmlChan class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (Starlink)

*  History:
*     10-OCT-2003 (DSB):
*        Original version.
*     6-FEB-2004 (DSB):
*        Added XmlPrefix and XmlFormat attributes.
*     10-FEB-2004 (DSB):
*        - Added debug conditional code to keep track of memory leaks.
*        - Fixed bug which prevented more than 1 object being read from
*        an XmlChan.
*     7-DEC-2005 (DSB):
*        Free memory allocated by calls to astReadString.
*     12-FEB-2010 (DSB):
*        Represent AST__BAD externally using the string "<bad>".
*class--

* Further STC work:
*     - Speed up general STC processing (a lot of time seems to be spent
*     simplifying things)
*     - Document (including a complete description of what is and is not
*     supported in the reference docs for the XmlFormat attribute).
*     - Produce a schema describing the format which can in fact be read by
*     AST.
*     - Look at Jonathan McDowell's mini-STC schema (also STC stuff in
*     spectral data model)
*     - Web services. Read only: test STCs for overlap, test points for
*     inclusion/exclusion, plot a mask over an image, verification (can AST
*     read it & does it generate warnings?). Read/Write: convert FITS to STC,
*     transform STC into a new coord system.
*     - Add support for writing as well as reading
*     - Modify Stc... constructors to check that the supplied Frame is suitable.
*     - What about multiple AstroCoordFrames and AstroCoordAreas in a STC?
*     - Add support for generic CoordFrames
*     - What should be done with pixel coords info within STC?
*     - Extend coverage (e.g. to 3D space frames, etc)

*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS XmlChan

/* The XML element name used to store an AST attribute setting */
#define ATTR "_attribute"

/* The XML element name used for an AST "isa" element */
#define ISA "_isa"

/* The XML attribute name which holds the name of the AST class which
   defines the item contained in the element. */
#define DEFINEDBY "definedby"

/* The XML attribute name which holds the name of the AST attribute */
#define NAME "name"

/* The XML attribute name which holds the value of the AST attribute */
#define VALUE "value"

/* The XML attribute name which indicates if the AST attribute value is a
   default value. */
#define DEFAULT "default"

/* The XML attribute name which indicates if the AST attribute value was
   originally a string value. */
#define QUOTED "quoted"

/* The XML attribute name which holds a description of the AST attribute. */
#define DESC "desc"

/* The XML attribute name which holds the label associated with an AST
   Object (if any). */
#define LABEL "label"

/* A string used to indicate atrue attribute value */
#define TRUE "true"

/* Format identifiers and strings */
#define UNKNOWN_FORMAT  -1
#define NATIVE_FORMAT    0
#define QUOTED_FORMAT    1
#define IVOA_FORMAT      2
#define MAX_FORMAT       2
#define UNKNOWN_STRING   "UNKNOWN"
#define NATIVE_STRING    "NATIVE"
#define QUOTED_STRING    "QUOTED"
#define IVOA_STRING      "IVOA"

/* Values representing message severities. */
#define WARNING 0
#define FAILURE 1
#define RESET 2

/* Known IVOA namespaces. When a new name is added, update the FindIVOAClass
   function. */
#define STC_URI "urn:nvo-stc"

/* Known IVOA Classes and attributes. When a new name is added, it may be
   necessary to update the FindIVOAClass function. */
#define STC_RESOURCE_PROFILE     "STCResourceProfile"
#define SEARCH_LOCATION          "SearchLocation"
#define OBSERVATION_LOCATION     "ObservationLocation"
#define OBSERVATORY_LOCATION     "ObservatoryLocation"
#define CATALOG_ENTRY_LOCATION   "CatalogEntryLocation"
#define OBS_DATA_LOCATION        "ObsDataLocation"
#define ASTRO_COORD_SYSTEM       "AstroCoordSystem"
#define ASTRO_COORD_AREA         "AstroCoordArea"
#define ASTRO_COORDS             "AstroCoords"
#define TIME_FRAME               "TimeFrame"
#define SPACE_FRAME              "SpaceFrame"
#define SPECTRAL_FRAME           "SpectralFrame"
#define REDSHIFT_FRAME           "RedshiftFrame"
#define DOPPLER_DEFINITION       "DopplerDefinition"

/* Returns string "an" or "a" depending on whether the first character of
   the supplied string is a vowel or not. */
#define ANA(t) (t?(strchr("AaEeIiOoUu",t[0])?"an":"a"):"")

/* String used to represent AST__BAD externally. */
#define BAD_STRING "<bad>"

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Coordinate Frames */
#include "timeframe.h"           /* Time coordinate Frames */
#include "cmpframe.h"            /* Coordinate Frames */
#include "skyframe.h"            /* Celestial coordinate Frames */
#include "specframe.h"           /* Spectral coordinate Frames */
#include "region.h"              /* Regions within coordinate Frames */
#include "ellipse.h"             /* Ellipses within coordinate Frames */
#include "pointlist.h"           /* Points within coordinate Frames */
#include "polygon.h"             /* Polygons within coordinate Frames */
#include "circle.h"              /* Circles within coordinate Frames */
#include "keymap.h"              /* Mapping of keys to values */
#include "channel.h"             /* Interface for parent class */
#include "xmlchan.h"             /* Interface definition for this class */
#include "loader.h"              /* Interface to the global loader */
#include "object.h"              /* Base Object class */
#include "wcsmap.h"              /* Angular conversion constants */
#include "xml.h"                 /* AST XML facilities */
#include "sofa.h"                /* IAU SOFA functions */
#include "stcresourceprofile.h"  /* IVOA StcResourceProfile class */
#include "stcsearchlocation.h"   /* IVOA SearchLocation class */
#include "stccatalogentrylocation.h"/* IVOA CatalogEntryLocation class */
#include "stcobsdatalocation.h"  /* IVOA ObsDataLocation class */
#include "nullregion.h"          /* Null regions */
#include "interval.h"            /* Axis intervals */
#include "box.h"                 /* Box regions */
#include "cmpregion.h"           /* Compound regions */
#include "prism.h"               /* Prism regions */
#include "unitmap.h"             /* Unit Mappings */
#include "unit.h"                /* Unit handling utilities */
#include "pal.h"                 /* slalib functions */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Type Definitions */
/* ================ */

/* A type for functions which read an IVOA element and return a
   corresponding AST Object. */
typedef AstObject *(*IVOAReader)( AstXmlChan *, AstXmlElement *, int * );

/* A structure to hold the result of scanning the content of an IVOA
   element.*/
typedef struct IVOAScan {
   int n;           /* Number of element names described by this structure */
   int *count;      /* Array holding number of each element name found */
   AstXmlElement ***el;  /* Array holding pointers to each element found */
} IVOAScan;

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static int (* parent_getfull)( AstChannel *, int * );
static int (* parent_getcomment)( AstChannel *, int * );
static int (* parent_getindent)( AstChannel *, int * );

/* Text values used to represent XmlFormat values externally. These
   should be in the order defined by the associated constants above. */
static const char *xformat[3] = { NATIVE_STRING, QUOTED_STRING, IVOA_STRING };

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->IsUsable_This = NULL; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->GetNextChar_C = NULL; \
   globals->GetNextChar_Buf = NULL;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(XmlChan)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(XmlChan,Class_Init)
#define class_vtab astGLOBAL(XmlChan,Class_Vtab)
#define isusable_this  astGLOBAL(XmlChan,IsUsable_This)
#define getattrib_buff astGLOBAL(XmlChan,GetAttrib_Buff)
#define getnextchar_c astGLOBAL(XmlChan,GetNextChar_C)
#define getnextchar_buf astGLOBAL(XmlChan,GetNextChar_Buf)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* An XmlChan pointer use to communicate with the IsUsable function. */
static AstXmlChan *isusable_this = NULL;

/* Buffer returned by GetAttrib. */
static char getattrib_buff[ 51 ];

/* Variables used in GetNextChar */
static char *getnextchar_c = NULL;    /* Pointer to next character to read */
static char *getnextchar_buf = NULL;  /* Pointer to previously read text */


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstXmlChanVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif


/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstXmlChan *astXmlChanForId_( const char *(*)( void ),
                           char *(*)( const char *(*)( void ), int * ),
                           void (*)( const char * ),
                           void (*)( void (*)( const char * ), const char *, int * ),
                           const char *, ... );
AstXmlChan *astXmlChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstObject *AstroCoordSystemReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *MakeAstFromXml( AstXmlChan *, AstXmlElement *, int * );
static AstObject *ObsDataLocationReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *Read( AstChannel *, int * );
static AstObject *ReadObject( AstChannel *, const char *, AstObject *, int * );
static AstObject *RedshiftFrameReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *SpaceFrameReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *SpectralFrameReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *StcMetadataReader( AstXmlChan *, AstXmlElement *, int * );
static AstObject *TimeFrameReader( AstXmlChan *, AstXmlElement *, int * );
static AstPointList *ObservatoryLocationReader( AstXmlChan *, AstXmlElement *, AstStcObsDataLocation *, int * );
static AstRegion *AllSkyReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *AstroCoordAreaReader( AstXmlChan *, AstXmlElement *, AstFrame *, AstRegion *[4], int, AstKeyMap **, int * );
static AstRegion *BoxReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *CircleReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *ConstraintReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *ConvexReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *Coord2VecIntervalReader( AstXmlChan *, AstXmlElement *, const char *, AstFrame *, int * );
static AstRegion *Coord3VecIntervalReader( AstXmlChan *, AstXmlElement *, const char *, AstFrame *, int * );
static AstRegion *CoordScalarIntervalReader( AstXmlChan *, AstXmlElement *, const char *, AstFrame *, int * );
static AstRegion *EllipseReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *IntersectionReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *NegationReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *PolygonReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *Position2DReader( AstXmlChan *, AstXmlElement *, AstFrame *, double *, AstKeyMap **, int * );
static AstRegion *PositionIntervalReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *RedshiftIntervalReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *RedshiftReader( AstXmlChan *, AstXmlElement *, AstFrame *, AstKeyMap **, int * );
static AstRegion *StcRegionReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *RegionReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *SpectralIntervalReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *SpectralReader( AstXmlChan *, AstXmlElement *, AstFrame *, double *, AstKeyMap **, int * );
static AstRegion *SphereReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstRegion *TimeIntervalReader( AstXmlChan *, AstXmlElement *, AstTimeFrame *, int * );
static AstRegion *TimeReader( AstXmlChan *, AstXmlElement *, AstTimeFrame *, double *, AstKeyMap **, int * );
static AstRegion *UnionReader( AstXmlChan *, AstXmlElement *, AstFrame *, int * );
static AstSystemType RedshiftSys( AstXmlChan *, AstXmlElement *, char **, int, int * );
static AstSystemType SpecSys( AstXmlChan *, AstXmlElement *, const char *, int, int * );
static AstXmlElement *FindAttribute( AstXmlChan *, const char *, int * );
static AstXmlElement *FindElement( AstXmlChan *, AstXmlElement *, const char *, int * );
static AstXmlElement *FindObject( AstXmlChan *, const char *, int * );
static AstXmlElement *MakePos2D( AstXmlChan *, AstXmlElement *, int * );
static AstXmlElement *ReadXmlText( AstXmlChan *, int * );
static AstXmlElement *Remove( AstXmlChan *, AstXmlElement *, int * );
static IVOAReader FindIVOAClass( AstXmlElement *, int *, int * );
static IVOAScan *FreeIVOAScan( IVOAScan *, int * );
static IVOAScan *ScanIVOAElement( AstXmlChan *, AstXmlElement *, int, const char *[], int[], int[], int * );
static char *ReadString( AstChannel *, const char *, const char *, int * );
static char *SourceWrap( const char *(*)( void ), int * );
static char GetNextChar( void *, int * );
static const char *FindNextIsA( AstXmlElement *, int, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetTag( AstXmlObject *, int, int * );
static double  AstronTimeReader( AstXmlChan *, AstXmlElement *, AstTimeFrame *, int * );
static double AttrValueD( AstXmlChan *, AstXmlElement *, const char *, double, int * );
static double ElemValueD( AstXmlChan *, AstXmlElement *, double, int * );
static double Error2PAReader( AstXmlChan *, AstXmlElement *, double *, int * );
static double MakeMJD( AstTimeFrame *, double, int * );
static double PosAngleReader( AstXmlChan *, AstXmlElement *, int * );
static double ReadDouble( AstChannel *, const char *, double, int * );
static int AstroCoordsReader( AstXmlChan *, AstXmlElement *, AstFrame *, AstRegion *[4], AstKeyMap **, int * );
static int AttrValueB( AstXmlChan *, AstXmlElement *, const char *, int, int * );
static int AttrValueI( AstXmlChan *, AstXmlElement *, const char *, int, int * );
static int ElemListD( AstXmlChan *, AstXmlElement *, int, double *, int * );
static int FindString( int, const char *[], const char *, const char *, const char *, const char *, int * );
static int GetComment( AstChannel *, int * );
static int GetFull( AstChannel *, int * );
static int GetIndent( AstChannel *, int * );
static int IsUsable( AstXmlElement *, int * );
static int ReadInt( AstChannel *, const char *, int, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int Use( AstXmlChan *, int, int, int * );
static int Ustrcmp( const char *, const char *, int * );
static int Ustrncmp( const char *, const char *, size_t, int * );
static int VertexReader( AstXmlChan *, AstXmlElement *, double *, double *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void FillAndLims( AstXmlChan *, AstXmlElement *, AstRegion *, int * );
static void OutputText( AstXmlChan *, const char *, int, int * );
static void ReCentreAnc( AstRegion *, int, AstKeyMap **, int * );
static void ReadClassData( AstChannel *, const char *, int * );
static void Report( AstXmlChan *, AstXmlElement *, int, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SinkWrap( void (*)( const char * ), const char *, int * );
static void WriteBegin( AstChannel *, const char *, const char *, int * );
static void WriteDouble( AstChannel *, const char *, int, int, double, const char *, int * );
static void WriteEnd( AstChannel *, const char *, int * );
static void WriteInt( AstChannel *, const char *, int, int, int, const char *, int * );
static void WriteIsA( AstChannel *, const char *, const char *, int * );
static void WriteObject( AstChannel *, const char *, int, int, AstObject *, const char *, int * );
static void WriteString( AstChannel *, const char *, int, int, const char *, const char *, int * );
static AstTimeScaleType TimeScaleReader( AstXmlChan *, AstXmlElement *, int * );

static int TestXmlLength( AstXmlChan *, int * );
static void ClearXmlLength( AstXmlChan *, int * );
static void SetXmlLength( AstXmlChan *, int, int * );
static int GetXmlLength( AstXmlChan *, int * );

static int TestXmlFormat( AstXmlChan *, int * );
static void ClearXmlFormat( AstXmlChan *, int * );
static void SetXmlFormat( AstXmlChan *, int, int * );
static int GetXmlFormat( AstXmlChan *, int * );

static int TestXmlPrefix( AstXmlChan *, int * );
static void ClearXmlPrefix( AstXmlChan *, int * );
static void SetXmlPrefix( AstXmlChan *, const char *, int * );
static const char * GetXmlPrefix( AstXmlChan *, int * );

/* Member functions. */
/* ================= */

static AstRegion *AllSkyReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     AllSkyReader

*  Purpose:
*     Make an AST Region from an IVOA AllSky element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *AllSkyReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     AllSky element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA AllSky element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Create a negated NullRegion (this is a boundless Region which includes
   all points in the Frame). */
   new = (AstRegion *) astNullRegion( frm, NULL, "negated=1", status );

/* Get any fill factor from the element and assign to the returned Region. */
   FillAndLims( this, elem, new, status );

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *AstroCoordAreaReader( AstXmlChan *this, AstXmlElement *elem,
                                        AstFrame *frm, AstRegion *uncs[4],
                                        int nanc, AstKeyMap **ancs, int *status ) {
/*
*  Name:
*     AstroCoordAreaReader

*  Purpose:
*     Make an AST Region from an IVOA AstroCoordArea element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *AstroCoordAreaReader( AstXmlChan *this, AstXmlElement *elem,
*                                      AstFrame *frm, AstRegion *uncs[4],
*                                      int nanc, AstKeyMap **ancs, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     AstroCoordArea element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA AstroCoordArea element. May be NULL, in
*        which case a NullRegion is returned.
*     frm
*        The Frame in which the returned Region is to be defined. If
*        Units or reference values (Epoch, RestFreq, RefRA, etc) are not set
*        for any axes, then they will be set by this function if possible.
*     uncs
*        Array holding pointers to the uncertainty Regions to be associated
*        with each of the four STC domains (space, time, spectral, redshift).
*        NULL should be suppied in any element for which no uncertainty is
*        available.
*     nanc
*        Number of KeyMap pointers stored in "ancs"
*     ancs
*        Pointer to an array of "nanc" elements, each being a pointer to
*        a KeyMap. Each one describes the ancilary information in an
*        AstroCoords element associated with the AstroCoordsArea decribed
*        by "region". Each KeyMap has elements with keys AST__STCERROR,
*        AST__STCRES, AST__STCSIZE, AST__STCPIXSZ, AST__STCVALUE each of
*        which holds a pointer to a Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.
*/

/* Local Variables: */
   AstRegion *r;
   AstFrame *cfrm;
   AstFrame *fr;
   AstFrame *pfrm;
   AstFrame *red_frame;
   AstFrame *space_frame;
   AstFrame *spec_frame;
   AstFrameSet *fs;
   AstMapping *map;
   AstObject *o;
   AstRegion **red_list;
   AstRegion **spec_list;
   AstRegion **space_list;
   AstRegion **time_list;
   AstRegion *new;
   AstRegion *reg;
   AstRegion *rred;
   AstRegion *rspec;
   AstRegion *rspace;
   AstRegion *rtime;
   AstRegion *sum;
   AstRegion *tmp;
   AstTimeFrame *time_frame;
   IVOAScan *scan;
   char *decset;
   char *raset;
   char buff[ DBL_DIG + 30 ];
   char setting[ 100 ];
   const char *dom;
   const char *id;
   const char *names[4];
   const char *name;
   const char *old_units;
   const char *text;
   double decref;
   double lbnd[2];
   double raref;
   double space_val[2];
   double spec_val;
   double time_val;
   double ubnd[2];
   int i;
   int ianc;
   int ired;
   int ispace;
   int ispec;
   int itime;
   int k;
   int l;
   int max[4];
   int min[4];
   int nax;
   int nred;
   int nspace;
   int nspec;
   int ntime;
   int paxis;

   static const char *key[ 5 ] = { AST__STCERROR,
                                   AST__STCRES,
                                   AST__STCSIZE,
                                   AST__STCPIXSZ,
                                   AST__STCVALUE };

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If null AstroCoordArea element has been supplied, return a NullRegion. */
   if( !elem ) {
      new = (AstRegion *) astNullRegion( frm, NULL, "", status );

/* Otherwise, create a Region of suitable class. */
   } else {

/* First identify the individual Frames within the supplied Frame. Current
   implementation for spatial axes is limited to celestial longitude and
   latitude. */
      space_frame = NULL;
      spec_frame = NULL;
      red_frame = NULL;
      time_frame = NULL;

      nax = astGetNaxes( frm );
      for( i = 0; i < nax; i++ ) {
         astPrimaryFrame( frm, i, &pfrm, &paxis );
         dom = astGetDomain( pfrm );
         if( !strcmp( dom, "SKY" ) ) {
            if( !space_frame ) {
               space_frame = astClone( pfrm );
            } else if( pfrm != space_frame) {
               Report( this, elem, FAILURE, "contains more than 2 spatial axes", status );
            }

         } else if( !strcmp( dom, "TIME" ) ) {
            if( !time_frame ) {
               if( astIsATimeFrame( pfrm ) ) {
                  time_frame = (AstTimeFrame *) astClone( pfrm );
               } else if( astOK ) {
                  astError( AST__INTER, "AstroCoordAreaReader(XmlChan): %s "
                            "supplied where TimeFrame expected (internal "
                            "AST programming error).", status, astGetClass( pfrm ) );
               }
            } else {
               Report( this, elem, FAILURE, "contains more than 1 time axis", status );
            }

         } else if( !strcmp( dom, "SPECTRUM" ) ) {
            if( !spec_frame ) {
               spec_frame = astClone( pfrm );
            } else {
               Report( this, elem, FAILURE, "contains more than 1 spectral axis", status );
            }

         } else if( !strcmp( dom, "REDSHIFT" ) ) {
            if( !red_frame ) {
               red_frame = astClone( pfrm );
            } else {
               Report( this, elem, FAILURE, "contains more than 1 redshift axis", status );
            }

         } else {
            Report( this, elem, FAILURE, "contains axes for an unsupported domain", status );
         }
         pfrm = astAnnul( pfrm );
      }

/* Search the supplied element for the required sub-elements. */
      names[ 0 ] = "Sphere|PositionInterval|Region";
      names[ 1 ] = "TimeInterval";
      names[ 2 ] = "SpectralInterval";
      names[ 3 ] = "RedshiftInterval";
      min[ 0 ] = 0;
      min[ 1 ] = 0;
      min[ 2 ] = 0;
      min[ 3 ] = 0;
      max[ 0 ] = INT_MAX;
      max[ 1 ] = INT_MAX;
      max[ 2 ] = INT_MAX;
      max[ 3 ] = INT_MAX;
      scan = ScanIVOAElement( this, elem, 4, names, min, max, status );

/* If succesfull.. */
      if( scan ) {

/* Create Regions for all the SpatialIntervals found in the supplied element. */
         space_val[ 0 ] = AST__BAD;
         space_val[ 1 ] = AST__BAD;
         nspace = scan->count[ 0 ];
         space_list = astMalloc( sizeof(AstRegion *)*(size_t)nspace );
         if( space_list ) {
            for( ispace = 0; ispace < nspace; ispace++ ) {
               name = astXmlGetName( scan->el[ 0 ][ ispace ] );
               if( !strcmp( name, "Sphere" ) ) {
                  space_list[ ispace ] = SphereReader( this,
                                                       scan->el[ 0 ][ ispace ],
                                                       space_frame, status );
               } else if( !strcmp( name, "PositionInterval" ) ) {
                  space_list[ ispace ] = PositionIntervalReader( this,
                                                       scan->el[ 0 ][ ispace ],
                                                       space_frame, status );
               } else if( !strcmp( name, "Region" ) ) {
                  space_list[ ispace ] = StcRegionReader( this,
                                                       scan->el[ 0 ][ ispace ],
                                                       space_frame, status );
               } else if( astOK ) {
                  astError( AST__INTER, "AstroCoordAreaReader(XmlChan): "
                            "SpatialInterval type %s not yet supported "
                            "(AST internal programming error).", status, name );
                  break;
               }

/* Store any uncertainty region.*/
               if( uncs[ 0 ] ) astSetUnc( space_list[ ispace ], uncs[ 0 ] );

            }

/* If the spatial region is a single point we will use the point as the
   reference position for any SpecFrames which are created. If there is
   just one spatial interval, and if it is bounded. and if the bounds are
   equal on both axes, note the mean position. */
            if( nspace == 1 ){
               if( astGetBounded( space_list[ 0 ] ) ) {
                  astGetRegionBounds( space_list[ 0 ], lbnd, ubnd );
                  if( astEQUAL( lbnd[ 0 ], ubnd[ 0 ] ) &&
                      astEQUAL( lbnd[ 1 ], ubnd[ 1 ] ) ) {
                     space_val[ 0 ] = 0.5*( lbnd[ 0 ] + ubnd[ 0 ] );
                     space_val[ 1 ] = 0.5*( lbnd[ 1 ] + ubnd[ 1 ] );
                  }
               }
            }
         }

/* Create Regions for all the TimeIntervals found in the supplied element. */
         time_val = AST__BAD;
         ntime = scan->count[ 1 ];
         time_list = astMalloc( sizeof(AstRegion *)*(size_t)ntime );
         if( time_list ) {
            for( itime = 0; itime < ntime; itime++ ) {
               time_list[ itime ] = TimeIntervalReader( this,
                                                       scan->el[ 1 ][ itime ],
                                                       time_frame, status );

/* Store any uncertainty region. Transfer the System and TimeOrigin
   values from the time region to the time uncertainty, if set. */
               if( uncs[ 1 ] ) {

                  if( astTestSystem( time_frame ) &&
                      astTestTimeOrigin( time_frame ) ) {

                     sprintf( setting, "System=%s",
                              astGetC( time_frame, "System" ) );
                     astRegSetAttrib( uncs[ 1 ], setting, NULL );


                     if( astTestUnit( time_frame, 0 ) ) {
                        old_units = astGetUnit( time_frame, 0 );
                        old_units = astStore( NULL, old_units,
                                              strlen( old_units ) + 1 );
                     } else {
                        old_units = NULL;
                     }

                     astSetUnit( time_frame, 0, astGetUnit( uncs[ 1 ], 0 ) );

                     sprintf( setting, "TimeOrigin=%s",
                              astGetC( time_frame, "TimeOrigin" ) );
                     astRegSetAttrib( uncs[ 1 ], setting, NULL );

                     if( old_units ) {
                        astSetUnit( time_frame, 0, old_units );
                        old_units = astFree( (void *) old_units );
                     } else {
                        astClearUnit( time_frame, 0 );
                     }

                  }

                  astSetUnc( time_list[ itime ], uncs[ 1 ] );
               }
            }

/* Use the mid point as the Epoch for all Frames which are created. If
   either limit is not specified, use the specified limit. */
            if( ntime > 0 ){
               astGetRegionBounds( time_list[ 0 ], lbnd, ubnd );
               if( fabs( lbnd[ 0 ] ) != DBL_MAX && lbnd[ 0 ] != AST__BAD ){
                  if( fabs( ubnd[ 0 ] ) != DBL_MAX && ubnd[ 0 ] != AST__BAD ){
                     time_val = 0.5*( lbnd[ 0 ] + ubnd[ 0 ] );
                  } else {
                     time_val = lbnd[ 0 ];
                  }
               } else if( fabs( ubnd[ 0 ] ) != DBL_MAX && ubnd[ 0 ] != AST__BAD ){
                  time_val = ubnd[ 0 ];
               }
            }
         }

/* Create Regions for all the SpectralIntervals found in the supplied element. */
         spec_val = AST__BAD;
         nspec = scan->count[ 2 ];
         spec_list = astMalloc( sizeof(AstRegion *)*(size_t)nspec );
         if( spec_list ) {
            for( ispec = 0; ispec < nspec; ispec++ ) {
               spec_list[ ispec ] = SpectralIntervalReader( this,
                                                       scan->el[ 2 ][ ispec ],
                                                       spec_frame, status );
/* Store any uncertainty region.*/
               if( uncs[ 2 ] ) astSetUnc( spec_list[ ispec ], uncs[ 2 ] );
            }

/* If the spectral region is a single point we will use the point as the
   rest frequency for all RedShift Frames which are created. If there is just
   one spectral interval, and if it is bounded. and if the bounds are equal,
   note the mean spectral value. */
            if( nspec == 1 ){
               if( astGetBounded( spec_list[ 0 ] ) ) {
                  astGetRegionBounds( spec_list[ 0 ], lbnd, ubnd );
                  if( astEQUAL( lbnd[ 0 ], ubnd[ 0 ] ) ) {
                     spec_val = 0.5*( lbnd[ 0 ] + ubnd[ 0 ] );
                  }
               }
            }
         }

/* Create Regions for all the RedshiftIntervals found in the supplied element. */
         nred = scan->count[ 3 ];
         red_list = astMalloc( sizeof(AstRegion *)*(size_t)nred );
         if( red_list ) {
            for( ired = 0; ired < nred; ired++ ) {
               red_list[ ired ] = RedshiftIntervalReader( this,
                                                       scan->el[ 3 ][ ired ],
                                                       red_frame, status );
/* Store any uncertainty region.*/
               if( uncs[ 3 ] ) astSetUnc( red_list[ ired ], uncs[ 3 ] );
            }
         }

/* Free the can result structure.*/
         scan = FreeIVOAScan( scan, status );

/* If the spatial regions cover only a single point, convert it to FK5
   J2000 and use it as the reference position for any SpecFrames (spectral or
   redshift) unless values were inherited from the supplied Frame. If the
   supplied Frame did not contain set values for these attributes, set them
   now. Use astRegSetAttrib which applies the attribute setting to both
   base and current Frame of the Region's FrameSet, and avoids re-mapping
   the current Frame. */
         if( astOK ) {
            if( space_val[ 0 ] != AST__BAD && space_val[ 1 ] != AST__BAD ) {

/* First need to convert to FK5 J2000 and format into a string for use with
   astRegSetAttrib. Need to ensure that the Format and Digits attributes
   are set to values which will result in no loss of precision in the
   formatting and unformatting steps. */
               fr = astCopy( space_frame );
               astClear( fr, "Format(1),Format(2),Digits(1),Digits(2)" );
               astSet( fr, "digits=%d,system=FK5,equinox=J2000", status, DBL_DIG);
               fs = astConvert( space_frame, fr, "" );
               fr = astAnnul( fr );
               if( fs ) {
                  astTran2( fs, 1, space_val, space_val + 1, 1, &raref, &decref );

                  text = astFormat( fs, raref, 0 );
                  l = text ? strlen( text ) : 0;
                  raset = astMalloc( l + 10 );
                  if( raset ) sprintf( raset, "refra=%s", text );

                  text = astFormat( fs, decref, 1 );
                  l = text ? strlen( text ) : 0;
                  decset = astMalloc( l + 10 );
                  if( decset ) sprintf( decset, "refdec=%s", text );

                  fs = astAnnul( fs );

/* Now set the FK5 J2000 values in the required Frames and Regions. */
                  if( !spec_frame || !astTestRefRA( spec_frame ) ||
                                     !astTestRefDec( spec_frame ) ) {
                     for( ispec = 0; ispec < nspec; ispec++ ) {
                        astRegSetAttrib( spec_list[ ispec ], raset, NULL );
                        astRegSetAttrib( spec_list[ ispec ], decset, NULL );
                     }

                     if( spec_frame ) {
                        astSetRefRA( (AstSpecFrame *) spec_frame, raref );
                        astSetRefDec( (AstSpecFrame *) spec_frame, decref );
                     }
                  }

                  if( !red_frame || !astTestRefRA( red_frame ) ||
                                    !astTestRefDec( red_frame ) ) {
                     for( ired = 0; ired < nred; ired++ ) {
                        astRegSetAttrib( red_list[ ired ], raset, NULL );
                        astRegSetAttrib( red_list[ ired ], decset, NULL );
                     }

                     if( red_frame ) {
                        astSetRefRA( (AstSpecFrame *) red_frame, raref );
                        astSetRefDec( (AstSpecFrame *) red_frame, decref );
                     }
                  }

                  for( ianc = 0; ianc < nanc; ianc++ ) {
                     for( k = 0; k < 5; k++ ) {
                        if( astMapGet0A( ancs[ ianc ], key[ k ], &o ) ) {
                           r = (AstRegion *) o;
                           astRegSetAttrib( r, raset, NULL );
                           astRegSetAttrib( r, decset, NULL );
                           r = astAnnul( r );
                        }
                     }
                  }

/* Free resources. */
                  if( raset ) raset = astFree( raset );
                  if( decset ) decset = astFree( decset );

               } else if( astOK ) {
                  astError( AST__INTER, "AstroCoordAreaReader(XmlChan):"
                            " Cannot convert spatial position to FK5 J2000" , status);
               }
            }

/* If a time region was specified, use a typical value as the epoch for
   all Frames. Call MakeMJD to convert "time_val" from the system of the
   TimeFrame to an MJD (as required by the Frame Epoch attribute). Set
   the value in both the returned Region and the supplied Frame. */
            if( time_val != AST__BAD ) {
               fr = astRegFrame( time_list[ 0 ] );
               if( astIsATimeFrame( fr ) ) {
                  time_val = MakeMJD( (AstTimeFrame *) fr, time_val, status );
               } else if( astOK ) {
                  astError( AST__INTER, "AstroCoordAreaReader(XmlChan): %s "
                            "supplied where TimeFrame expected (internal "
                            "AST programming error).", status, astGetClass( fr ) );
               }
               fr = astAnnul( fr );

               sprintf( buff, "epoch= MJD %.*g", DBL_DIG, time_val );

               if( !space_frame || !astTestEpoch( space_frame ) ) {
                  for( ispace = 0; ispace < nspace; ispace++ ) {
                     astRegSetAttrib( space_list[ ispace ], buff, NULL );
                  }
                  if( space_frame ) astSetEpoch( space_frame, time_val );
               }

               if( !spec_frame || !astTestEpoch( spec_frame ) ) {
                  for( ispec = 0; ispec < nspec; ispec++ ) {
                     astRegSetAttrib( spec_list[ ispec ], buff, NULL );
                  }
                  if( spec_frame ) astSetEpoch( spec_frame, time_val );
               }

               if( !red_frame || !astTestEpoch( red_frame ) ) {
                  for( ired = 0; ired < nred; ired++ ) {
                     astRegSetAttrib( red_list[ ired ], buff, NULL );
                  }
                  if( red_frame ) astSetEpoch( red_frame, time_val );
               }

               for( ianc = 0; ianc < nanc; ianc++ ) {
                  for( k = 0; k < 5; k++ ) {
                     if( astMapGet0A( ancs[ ianc ], key[ k ], &o ) ) {
                        r = (AstRegion *) o;
                        astRegSetAttrib( r, buff, NULL );
                        r = astAnnul( r );
                     }
                  }
               }

            }

/* If the spectral regions cover only a single point, format it with its
   units so that the astSetAttrib function can convert it to Hz and use
   it as the rest frequency for any redshift Frames. */
            if( spec_val != AST__BAD && nred > 0 ) {

               text = astGetUnit( spec_frame, 0 );
               if( text ) sprintf( buff, "restfreq= %.*g %s", DBL_DIG,
                                   spec_val, text );

               if( !red_frame || !astTestRestFreq( red_frame ) ) {
                  for( ired = 0; ired < nred; ired++ ) {
                     astRegSetAttrib( red_list[ ired ], buff, NULL );
                  }
                  if( red_frame ) astSetAttrib( red_frame, buff );
               }

               for( ianc = 0; ianc < nanc; ianc++ ) {
                  for( k = 0; k < 5; k++ ) {
                     if( astMapGet0A( ancs[ ianc ], key[ k ], &o ) ) {
                        r = (AstRegion *) o;
                        astRegSetAttrib( r, buff, NULL );
                        r = astAnnul( r );
                     }
                  }
               }
            }

/* Create Regions corresponding to every possible combination of interval
   on each axis type, and assemble the union of these into a CmpRegion (if
   there is more than one). */
            sum = NULL;

/* Initialise indices of the sub-Frame intervals to use. */
            ispace = 0;
            itime = 0;
            ispec = 0;
            ired = 0;

/* Loop over all possible combinations of time+space+spec+red intervals. */
            while( 1 ) {
               rspace = ( ispace < nspace ) ? space_list[ ispace ] : NULL;
               rtime = ( itime < ntime ) ? time_list[ itime ] : NULL;
               rspec = ( ispec < nspec ) ? spec_list[ ispec ] : NULL;
               rred = ( ired < nred ) ? red_list[ ired ] : NULL;

/* Prism Regions extrude a Region into higher dimensions, and the
   extrusion is defined by an Interval. Spatial Regions are not
   restricted to Intervals and so any spatial Region must be the first
   Region to be included in the Prism (all the other axis types *are*
   restricted to Intervals and so can be used to extrude the spatial
   region). */
               reg = rspace ? astClone( rspace ) : NULL;

/* Now extrude this region (if any) into the time axis. */
               if( rtime ) {
                  if( reg ) {
                     tmp = (AstRegion *) astPrism( reg, rtime, "", status );
                     (void) astAnnul( reg );
                     reg = tmp;
                  } else {
                     reg = astClone( rtime );
                  }
               }

/* Now extrude this region (if any) into the spectral axis. */
               if( rspec ) {
                  if( reg ) {
                     tmp = (AstRegion *) astPrism( reg, rspec, "", status );
                     (void) astAnnul( reg );
                     reg = tmp;
                  } else {
                     reg = astClone( rspec );
                  }
               }

/* Now extrude this region (if any) into the redshift axis. */
               if( rred ) {
                  if( reg ) {
                     tmp = (AstRegion *) astPrism( reg, rred, "", status );
                     (void) astAnnul( reg );
                     reg = tmp;
                  } else {
                     reg = astClone( rred );
                  }
               }


/* If a Prism was created, add it into the CmpRegion which holds the
   running sum of the union of all Prisms created so far. */
               if( reg ) {
                  if( !sum ) {
                     sum = astClone( reg );
                  } else {
                     tmp = (AstRegion *) astCmpRegion( sum, reg, AST__OR, "", status );
                     (void) astAnnul( sum );
                     sum = tmp;
                  }
                  reg = astAnnul( reg );
               }

/* Increment the indices of the next set of sub-Frame Intervals to use.
   Leave the while loop when all combinations have been done. */
               if( ++ired >= nred ) {
                  ired = 0;
                  if( ++ispec >= nspec ) {
                     ispec = 0;
                     if( ++itime >= ntime ) {
                        itime = 0;
                        if( ++ispace >= nspace ) break;
                     }
                  }
               }
            }

/* Simplify the total sum Region. */
            tmp = astSimplify( sum );
            (void) astAnnul( sum );
            sum = tmp;

/* The axes in this sum Region may not be in the correct order or units (i.e
   in the order and units specified in the supplied Frame). So use
   astConvert to get a Mapping from the Frame represented by the sum
   Region to the supplied Frame. */
            fs = astConvert( sum, frm, "" );
            if( fs ) {

/* Unless the Mapping is a UnitMap, remap the sum Region into the
   supplied Frame using this Mapping. */
               map = astGetMapping( fs, AST__BASE, AST__CURRENT );
               if( !astIsAUnitMap( map ) ) {
                  new = astMapRegion( sum, map, frm );
               } else {
                  new = astClone( sum );
               }

               map = astAnnul( map );
               fs = astAnnul( fs );

            } else if( astOK ) {
               astError( AST__INTER, "AstroCoordAreaReader(%s): Cannot "
                         "convert from supplied Frame to internal Frame (AST "
                         "internal programming error).", status, astGetClass( this ) );
            }

/* Transfer selected properties from the supplied Frame to the current Frame
   of the returned Region. */
            cfrm = astRegFrame( new );
            if( astTestIdent( frm ) ) astSetIdent( cfrm, astGetIdent( frm ) );
            if( astTestTitle( frm ) ) astSetTitle( cfrm, astGetTitle( frm ) );

/* Ensure the Epoch is set correctly in the Region */
            if( time_val != AST__BAD ) {
               sprintf( buff, "epoch= MJD %.*g", DBL_DIG, time_val );
               astRegSetAttrib( new, buff, NULL );
            }

/* Free resources. */
            cfrm = astAnnul( cfrm );
            sum = astAnnul( sum );
         }

         if( space_list ) {
            for( i = 0; i < nspace; i++ ) space_list[ i ] = astAnnul( space_list[ i ]  );
            space_list = astFree( space_list );
         }

         if( time_list ) {
            for( i = 0; i < ntime; i++ ) time_list[ i ] = astAnnul( time_list[ i ]  );
            time_list = astFree( time_list );
         }

         if( spec_list ) {
            for( i = 0; i < nspec; i++ ) spec_list[ i ] = astAnnul( spec_list[ i ]  );
            spec_list = astFree( spec_list );
         }

         if( red_list ) {
            for( i = 0; i < nred; i++ ) red_list[ i ] = astAnnul( red_list[ i ]  );
            red_list = astFree( red_list );
         }

      }

      if( space_frame ) space_frame = astAnnul( space_frame );
      if( time_frame ) time_frame = astAnnul( time_frame );
      if( spec_frame ) spec_frame = astAnnul( spec_frame );
      if( red_frame ) red_frame = astAnnul( red_frame );

/* Get the ID attribute from the AstroCoordArea element and store in the
   returned Region. */
      id = astXmlGetAttributeValue( elem, "ID" );
      if( id ) astSetIdent( new, id );

   }

/* If an error has occurred,annul the returned pointer. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static int AstroCoordsReader( AstXmlChan *this, AstXmlElement *elem,
                              AstFrame *frm, AstRegion *uncs[4],
                              AstKeyMap **anc, int *status ) {
/*
*  Name:
*     AstroCoordsReader

*  Purpose:
*     Modify a Frame to take account of an IVOA AstroCoords element, and
*     return an coordinate uncertainties.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int AstroCoordsReader( AstXmlChan *this, AstXmlElement *elem,
*                            AstFrame *frm, AstRegion *uncs[4],
*                            AstKeyMap **anc, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function modifies the supplied Frame object to incorporate the
*     effects of the supplied AstroCoords element. It may also return
*     Regions representing the bounds of the uncertainties in the four
*     component coordinate Frames, depending on the contents of the
*     AstroCoords element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA AstroCoords element.
*     frm
*        The Frame object to modify.
*     uncs
*        Array in which to return pointers to the uncertainty Regions to
*        be associated with each of the four STC domains (space, time,
*        spectral, redshift). NULL is returned in any element for which
*        no uncertainty is specified within the supplied AstroCoords element.
*     anc
*        Address of a location at which to store the pointer to a newly
*        created KeyMap holding ancillary information describing the
*        AstroCoords element in the form required by constructors of AST
*        Stc objects. A NULL pointer is returned if no usable ancillary
*        information is found in the AstroCoords.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if any non-NULL values have been returned in the "uncs"
*     array. Zero otherwise.

*/

/* Local Variables: */
   AstFrame *afrm;               /* Pointer to axis Frame */
   AstFrame *gfrm;                /* Pointer to generic Frame */
   AstFrame *pfrm;               /* Pointer to position Frame */
   AstFrame *rfrm;               /* Pointer to redshift Frame */
   AstFrame *sfrm;               /* Pointer to spectral Frame */
   AstTimeFrame *tfrm;           /* Pointer to time Frame */
   AstKeyMap *panc;              /* KeyMap holding spatial ancillary data */
   AstKeyMap *ranc;              /* KeyMap holding redshift ancillary data */
   AstKeyMap *sanc;              /* KeyMap holding spectral ancillary data */
   AstKeyMap *tanc;              /* KeyMap holding temporal ancillary data */
   AstObject *o;                 /* Pointer to object retrieved from KeyMap */
   AstRegion *r;                 /* Individual ancillary Region */
   AstRegion *t;                 /* Total extruded ancillary Region */
   AstRegion *tt;                /* Temporary Region pointer */
   AstXmlElement *el;            /* Pointer to Position2D element */
   IVOAScan *scan;               /* Structure holding scan results */
   char **anames;                /* Pointer to list of ancillary name pointers */
   const char *dom;              /* Pointer to Domain attribute value */
   const char *nam;              /* Pointer to ancillary Name string */
   const char *names[4];         /* Names of the subelements to be searched for */
   char buff[100];               /* Message buffer */
   double epoch;                 /* Epoch */
   double hi;                    /* High limit for zero-width interval */
   double lo;                    /* Low limit for zero-width interval */
   double pos[2];                /* Reference spatial position */
   double rf;                    /* Rest frequency */
   int axes[2];                  /* Indices of position axes */
   int axis;                     /* Index of next axis to use */
   int empty;                    /* Is returned KeyMap empty? */
   int i;                        /* Loop count */
   int isearth;                  /* Does the SkyFrame represent terrestrial lon/lat? */
   int junk;                     /* Unused integer value */
   int max[4];                   /* Max allowed occurrences of each name */
   int min[4];                   /* Min allowed occurrences of each name */
   int nax;                      /* Number of axes in supplied Frame */
   int unc;                      /* Any uncertainty Regions found? */
   int use;                      /* Use ancillary information? */

   static const char *key[ 5 ] = { AST__STCERROR,
                                   AST__STCRES,
                                   AST__STCSIZE,
                                   AST__STCPIXSZ,
                                   AST__STCVALUE };
/* Initialise */
   unc = 0;
   uncs[ 0 ] = NULL;
   uncs[ 1 ] = NULL;
   uncs[ 2 ] = NULL;
   uncs[ 3 ] = NULL;
   *anc = NULL;

/* Check the global error status. */
   if ( !astOK ) return unc;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Position2D|Position3D";
   names[ 1 ] = "Time";
   names[ 2 ] = "Spectral";
   names[ 3 ] = "Redshift";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   max[ 3 ] = 1;
   scan = ScanIVOAElement( this, elem, 4, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Initialise pointers to component Frames */
      pfrm = NULL;
      tfrm = NULL;
      sfrm = NULL;
      rfrm = NULL;

/* Initialise pointers to KeyMaps holding ancillary data. */
      panc = NULL;
      tanc = NULL;
      sanc = NULL;
      ranc = NULL;

/* Allocate storage for an array of pointers to strings holding the Name
   value for each axis. Initialise them to a null string. */
      nax = astGetNaxes( frm );
      anames = astMalloc( sizeof( char * )*(size_t)nax );
      for( i = 0; i < nax; i++ ) anames[ i ] = NULL;

/* Initialise the index of the next Frame axis to use. */
      axis = 0;

/* Check to see if the next 2 axes describe positions on the sky or earth
   (see SpaceFrameReader). */
      axes[ 0 ] = 0;
      axes[ 1 ] = 1;
      afrm = astPickAxes( frm, 2, axes, NULL );
      dom = astGetDomain( afrm );
      isearth = dom && ( !strcmp( dom, "GEO_D" ) ||
                         !strcmp( dom, "GEO_C" ) );

      if( isearth || ( dom && !strcmp( dom, "SKY" ) ) ){
         astPrimaryFrame( frm, axis, &pfrm, &junk );
         if( scan->count[ 0 ] ) {

/* We currently also use SkyFrames to represent geographical long/lat used to
   describe observatory positions. These may have 3D positions, in which
   case we convert the 3D position to a 2D position by ignoring the 3rd axis
   value (height). See SpaceFrameReader. */
            el = MakePos2D( this, scan->el[ 0 ][ 0 ], status );

/* Use the Position2D to create a Region describing the uncertainty in
   the space axes of the Frame. Also create a KeyMap holding Regions
   describing any ancillary information stored in the Position2D. */
            uncs[ 0 ] = Position2DReader( this, el, pfrm, pos, &panc, status );
            if( uncs[ 0 ] ) unc = 1;
            el = astXmlDelete( el );

/* If ancillary information was returned, extract the Name element, and
   store it twice (once for each axis) in the "names" array. */
            if( panc && astMapGet0C( panc, AST__STCNAME, &nam ) ) {
               anames[ axis ] = astStore( NULL, nam, strlen( nam ) + 1 );
               anames[ axis + 1 ] = astStore( NULL, nam, strlen( nam ) + 1 );
               nam = astFree( (void *) nam );
            }
         }

/* Increment the axis index. */
         axis += 2;

/* If the supplied Frame has no sky frame, but we found a Position2D, then
   report a warning and ignore the Position2D. */
      } else if( scan->count[ 0 ] ) {
         sprintf( buff, "contains a <%s> which is not being used.",
                  astXmlGetName( scan->el[ 0 ][ 0 ] ) );
         Report( this, elem, WARNING, buff, status );
      }
      afrm = astAnnul( afrm );

/* Indicate we do not yet have an epoch to use. */
      epoch = AST__BAD;

/* Check to see if the Frame contains a time frame. It will be the next
   axis if it does. */
      afrm = astPickAxes( frm, 1, &axis, NULL );
      dom = astGetDomain( afrm );
      if( dom && !strcmp( dom, "TIME" ) ){
         astPrimaryFrame( frm, axis, &gfrm, &junk );

/* Report an error if it is not an AST TimeFrame. */
         if( !astIsATimeFrame( gfrm ) && astOK ) {
            astError( AST__INTER, "AstroCoordAreaReader(XmlChan): %s "
                      "supplied where TimeFrame expected (internal "
                      "AST programming error).", status, astGetClass( pfrm ) );
         } else {
            tfrm = (AstTimeFrame *) gfrm;
         }

/* Use any Time element to create a Region describing the uncertainty in the
   time axis of the Frame. Also create a KeyMap holding Regions describing
   any ancillary information stored in the Time element. */
         if( scan->count[ 1 ] ) {
            uncs[ 1 ] = TimeReader( this, scan->el[ 1 ][ 0 ], tfrm, &epoch,
                                    &tanc, status );
            if( uncs[ 1 ] ) unc = 1;

/* If ancillary information was returned, extract the Name element, and
   store it in the "names" array. */
            if( tanc && astMapGet0C( tanc, AST__STCNAME, &nam ) ) {
               anames[ axis ] = astStore( NULL, nam, strlen( nam ) + 1 );
               nam = astFree( (void *) nam );
            }
         }

/* Increment the index of the next axis to use. */
         axis++;

/* If the supplied Frame has no time frame, but we found a Time element, then
   report a warning and ignore the Time element. */
      } else if( scan->count[ 1 ] ) {
         Report( this, elem, WARNING, "contains a <Time> which is not needed", status );
      }
      afrm = astAnnul( afrm );

/* Indicate we do not yet have a rest frequency to use with any redshift
   axis. */
      rf = AST__BAD;

/* Check to see if the Frame contains a spectral frame. It will be the next
   axis if it does. */
      afrm = astPickAxes( frm, 1, &axis, NULL );
      dom = astGetDomain( afrm );
      if( dom && !strcmp( dom, "SPECTRUM" ) ){
         astPrimaryFrame( frm, axis, &sfrm, &junk );

/* Use any Spectral to create a Region describing the uncertainty in the
   spectral axis of the Frame. If the Spectral contains a spectral value, the
   first value will be returned so that it can be used  as the rest frequency
   for any Redshift axis. It will be in units of Hz and will be AST__BAD if
   the Spectral did not contain any spectral values. Also create a KeyMap
   holding Regions describing any ancillary information stored in the
   Spectral element. */
         if( scan->count[ 2 ] ) {
            uncs[ 2 ] = SpectralReader( this, scan->el[ 2 ][ 0 ], sfrm, &rf,
                                        &sanc, status );
            if( uncs[ 2 ] ) unc = 1;

/* If ancillary information was returned, extract the Name element, and
   store it in the "names" array. */
            if( sanc && astMapGet0C( sanc, AST__STCNAME, &nam ) ) {
               anames[ axis ] = astStore( NULL, nam, strlen( nam ) + 1 );
               nam = astFree( (void *) nam );
            }
         }

/* Increment the index of the next axis to use. */
         axis++;

/* If the supplied Frame has no spectral frame, but we found a Spectral
   element, then report a warning and ignore the Spectral element. */
      } else if( scan->count[ 2 ] ) {
         Report( this, elem, WARNING, "contains a <Spectral> which is not needed", status );
      }
      afrm = astAnnul( afrm );

/* Check to see if the Frame contains a redshift frame. It will be the next
   axis if it does. */
      afrm = astPickAxes( frm, 1, &axis, NULL );
      dom = astGetDomain( afrm );
      if( dom && !strcmp( dom, "REDSHIFT" ) ){
         astPrimaryFrame( frm, axis, &rfrm, &junk );

/* Use any Redshift to create a Region describing the uncertainty in the
   redshift axis of the Frame. Also create a KeyMap holding Regions describing
   any ancillary information stored in the Redshift element. */
         if( scan->count[ 3 ] ) {
            uncs[ 3 ] = RedshiftReader( this, scan->el[ 3 ][ 0 ], rfrm,
                                        &ranc, status );
            if( uncs[ 3 ] ) unc = 1;

/* If ancillary information was returned, extract the Name element, and
   store it in the "names" array. */
            if( ranc && astMapGet0C( ranc, AST__STCNAME, &nam ) ) {
               anames[ axis ] = astStore( NULL, nam, strlen( nam ) + 1 );
               nam = astFree( (void *) nam );
            }
         }

/* Increment the index of the next axis to use. */
         axis++;

/* If the supplied Frame has no redshift frame, but we found a Redshift
   element, then report a warning and ignore the Redshift element. */
      } else if( scan->count[ 3 ] ) {
         Report( this, elem, WARNING, "contains a <Redshift> which is not needed", status );
      }
      afrm = astAnnul( afrm );

/* Now assign fixed axis values (Epoch, RestFreq, etc) to the component
   Frames of the supplied Frame. */
      if( epoch != AST__BAD ) {
         if( pfrm ) astSetEpoch( pfrm, epoch );
         if( tfrm ) astSetEpoch( tfrm, epoch );
         if( sfrm ) astSetEpoch( sfrm, epoch );
         if( rfrm ) astSetEpoch( rfrm, epoch );
         astSetEpoch( frm, epoch );
      }

      if( sfrm && pfrm && astIsASpecFrame( sfrm ) && astIsASkyFrame( pfrm ) &&
          !isearth && pos[ 0 ] != AST__BAD && pos[ 1 ] != AST__BAD ) {
         astSetRefPos( sfrm, pfrm, pos[ 0 ], pos[ 1 ] );
      }

      if( rfrm && astIsASpecFrame( rfrm ) && rf != AST__BAD ) {
         astSetRestFreq( rfrm, rf );
         if( pfrm && astIsASkyFrame( pfrm ) && !isearth &&
             pos[ 0 ] != AST__BAD && pos[ 1 ] != AST__BAD ) {
            astSetRefPos( rfrm, pfrm, pos[ 0 ], pos[ 1 ] );
         }
      }

/* Now combine ancillary data for each component Frame into the total
   Frame. */
      *anc = astKeyMap( "", status );
      if( *anc ) {
         empty = 1;

/* Store the Names element if at least one axis has a Name item. */
         for( i = 0; i < nax; i++ ) {
            if( !anames[ i ] ) anames[ i ] = astStore( NULL, "", 1 );
         }

         for( i = 0; i < nax; i++ ) {
            if( empty && strlen( anames[ i ] ) > 0 ) {
               astMapPut1C( *anc, AST__STCNAME, nax, (const char **) anames, NULL );
               empty = 0;
            }
            anames[ i ] = astFree( anames[ i ] );
         }

/* Do each of the other items, all of which are described by a Region. */
         lo = 0.0;
         hi = 0.0;
         for( i = 0; i < 5; i++ ) {

/* Initialise a flag indicating that we have not yet found any non-null
   information to store for this item. */
            use = 0;

/* Initialise a pointer to the Region describing the item extruded into
   all axes. */
            t = NULL;

/* If there is a positional Frame, determine the Region describing the
   intersection of the total Region with the position Frame. If none is
   supplied use a zero width Interval as a flag that no information is
   available. */
            if( pfrm ) {
               if( panc && astMapGet0A( panc, key[ i ], &o ) ) {
                  t = (AstRegion *) o;
                  use = 1;
               } else {
                  t = (AstRegion *) astInterval( pfrm, &lo, &hi, NULL, "", status );
               }
            }

/* If there is a time Frame, determine the Region describing the intersection
   of the total Region with the time Frame. If none is supplied use a zero
   width Interval as a flag that no information is available. */
            if( tfrm ) {
               if( tanc && astMapGet0A( tanc, key[ i ], &o ) ) {
                  r = (AstRegion *) o;
                  use = 1;
               } else {
                  r = (AstRegion *) astInterval( tfrm, &lo, &hi, NULL, "", status );
               }

/* If there were earlier axes, extrude the current total region into the
   time axis, and use the extruded region as the new total region.*/
               if( t ) {
                  tt = (AstRegion *) astPrism( t, r, "", status );
                  r = astAnnul( r );
                  (void) astAnnul( t );
                  t = tt;

/* If this is the first axis, use the region determined for this axis as
   the total Region.*/
               } else {
                  t = r;
               }
            }

/* Do the same for any spectral axis. */
            if( sfrm ) {
               if( sanc && astMapGet0A( sanc, key[ i ], &o ) ) {
                  r = (AstRegion *) o;
                  use = 1;
               } else {
                  r = (AstRegion *) astInterval( sfrm, &lo, &hi, NULL, "", status );
               }

               if( t ) {
                  tt = (AstRegion *) astPrism( t, r, "", status );
                  r = astAnnul( r );
                  (void) astAnnul( t );
                  t = tt;
               } else {
                  t = r;
               }

            }

/* Do the same for any redshift axis. */
            if( rfrm ) {
               if( ranc && astMapGet0A( ranc, key[ i ], &o ) ) {
                  r = (AstRegion *) o;
                  use = 1;
               } else {
                  r = (AstRegion *) astInterval( rfrm, &lo, &hi, NULL, "", status );
               }

               if( t ) {
                  tt = (AstRegion *) astPrism( t, r, "", status );
                  r = astAnnul( r );
                  (void) astAnnul( t );
                  t = tt;
               } else {
                  t = r;
               }
            }

/* If there is some non-null information for this item, replace the
   stored Frame with the Frame which has set Epoch/RefLat/etc, simplify the
   total Region and store it in the returned KeyMap. */
            if( use ) {
               astSetRegFS( t, frm );
               tt = astSimplify( t );
               astMapPut0A( *anc, key[ i ], tt, NULL );
               tt = astAnnul( tt );
               empty = 0;
            }
            if( t ) t = astAnnul( t );
         }

/* Return a NULL KeyMap pointer if the KeyMap is empty. */
         if( empty ) *anc = astAnnul( *anc );
      }

/* Free resources. */
      if( panc ) panc = astAnnul( panc );
      if( tanc ) tanc = astAnnul( tanc );
      if( sanc ) sanc = astAnnul( sanc );
      if( ranc ) ranc = astAnnul( ranc );
      if( pfrm ) pfrm = astAnnul( pfrm );
      if( tfrm ) tfrm = astAnnul( tfrm );
      if( sfrm ) sfrm = astAnnul( sfrm );
      if( rfrm ) rfrm = astAnnul( rfrm );
      scan = FreeIVOAScan( scan, status );
      anames = astFree( anames );
   }

/* Annull any returned Regions if an error occurred.*/
   if( !astOK ) {
      uncs[ 0 ] = astAnnul( uncs[ 0 ] );
      uncs[ 1 ] = astAnnul( uncs[ 1 ] );
      uncs[ 2 ] = astAnnul( uncs[ 2 ] );
      uncs[ 3 ] = astAnnul( uncs[ 3 ] );
      unc = 0;
      *anc = astAnnul( *anc );
   }

/* Return the result. */
   return unc;
}

static AstObject *AstroCoordSystemReader( AstXmlChan *this,
                                          AstXmlElement *elem, int *status ) {
/*
*  Name:
*     AstroCoordSystemReader

*  Purpose:
*     Make an AST Object from an IVOA AstroCoordSystem element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *AstroCoordSystemReader( AstXmlChan *this,
*                                        AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     AstroCoordSystem element. This will be a Frame of some kind.
*     If the AstroCoordSystem element contains only one sub-frame
*     element, then the returned Frame will be of a suitable class
*     to describe that sub-frame (SkyFrame, SpecFrame or TimeFrame).
*     If the AstroCoordSystem element contains more than one sub-frame
*     element, then the returned Frame will be a CmpFrame in which the
*     component Frames are in the order SpaceFrame, TimeFrame,
*     SpectralFrame, RedshiftFrame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA AstroCoordSystem element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.

*  Notes:
*     - GenericCoordFrame sub-elements are currently ignored since it is not
*     clear how they relate to the other sub-elements.

*/

/* Local Variables: */
   AstCmpFrame *tmp;             /* Pointer to intermediate CmpFrame */
   AstFrame *comp[ 4 ];          /* Pointers to component Frames */
   AstObject *new;               /* Pointer to returned Object */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *id;               /* Pointer to ID attribute value */
   const char *names[4];         /* Names of the subelements to be searched for */
   int i;                        /* Index of current content item */
   int j;                        /* Index to store Frame pointer at */
   int max[4];                   /* Max allowed occurrences of each name */
   int min[4];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = SPACE_FRAME;
   names[ 1 ] = TIME_FRAME;
   names[ 2 ] = SPECTRAL_FRAME;
   names[ 3 ] = REDSHIFT_FRAME;
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   max[ 3 ] = 1;
   scan = ScanIVOAElement( this, elem, 4, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create Frames from the found sub-elements */
      comp[ 0 ] = scan->count[0] ? (AstFrame *) SpaceFrameReader( this,
                                                   scan->el[ 0 ][ 0 ], status ) : NULL;
      comp[ 1 ] = scan->count[1] ? (AstFrame *) TimeFrameReader( this,
                                                   scan->el[ 1 ][ 0 ], status ) : NULL;
      comp[ 2 ] = scan->count[2] ? (AstFrame *) SpectralFrameReader( this,
                                                   scan->el[ 2 ][ 0 ], status ) : NULL;
      comp[ 3 ] = scan->count[3] ? (AstFrame *) RedshiftFrameReader( this,
                                                   scan->el[ 3 ][ 0 ], status ) : NULL;

/* If more than one frame was obtained combine them into a CmpFrame. If
   present, the Frames are stored in the order SpaceFrame, TimeFrame,
   SpectralFrame, RedshiftFrame. Shuffle the the higher elements of the
   "comp" array down to fill any NULL elements. */
      j = 0;
      for( i = 0; i < 4; i++ ) {
         if( comp[ i ] ) {
            comp[ j++ ] = comp[ i ];
         }
      }

/* Fill any unused elements at the end with NULL. */
      for( ; j < 4; j++ ) comp[ j ] = NULL;

/* If no Frames were read issue a fatal error. */
      if( !comp[ 0 ] ) {
         Report( this, elem, FAILURE, "contains no usable coordinate axes", status );

/* If only one Frame was read return a clone of its pointer. */
      } else if( !comp[ 1 ] ) {
         new = astClone( comp[ 0 ] );

/* If two or more Frames were read, create a CmpFrame holding the Frames. */
      } else if( !comp[ 2 ] ) {
         new = (AstObject *) astCmpFrame( comp[ 0 ], comp[ 1 ], "", status );

      } else if( !comp[ 3 ] ) {
         tmp = astCmpFrame( comp[ 0 ], comp[ 1 ], "", status );
         new = (AstObject *) astCmpFrame( tmp, comp[ 2 ], "", status );
         tmp = astAnnul( tmp );

      } else {
         tmp = astCmpFrame( comp[ 0 ], comp[ 1 ], "", status );
         (void) astAnnul( comp[ 0 ] );
         comp[ 0 ] = (AstFrame *) tmp;
         tmp = astCmpFrame( comp[ 0 ], comp[ 2 ], "", status );
         new = (AstObject *) astCmpFrame( tmp, comp[ 3 ], "", status );
         tmp = astAnnul( tmp );
      }

/* Get the ID attribute from the AstroCoordSystem element and store in the
   returned Frame. */
      id = astXmlGetAttributeValue( elem, "ID" );
      if( id ) astSetIdent( new, id );

/* Free resources */
      for( i = 0; i < 4; i++ ) {
         if( comp[ i ] ) comp[ i ] = astAnnul( comp[ i ] );
      }
      scan = FreeIVOAScan( scan, status );

   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Object. */
   return new;
}

static double AstronTimeReader( AstXmlChan *this, AstXmlElement *elem,
                                AstTimeFrame *frm, int *status ){
/*
*  Name:
*     AstronTimeReader

*  Purpose:
*     Read a time value from an IVOA AstronTime element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double AstronTimeReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstTimeFrame *frm )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function returns a double representing the time specified by
*     the supplied IVOA AstronTime element, converted into the system
*     represented by the supplied Frame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA AstronTime element.
*     frm
*        Pointer to the TimeFrame in which the returned value should be
*        defined. Relevant attributes which are not set will be set by
*        this function if possible.

*  Returned Value:
*     The time value, in the system described by "frm".

*/

/* Local Variables: */
   AstFrameSet *fs;              /* FrameSet connecting two TimeFrames */
   AstTimeFrame *cfrm;           /* TimeFrame describing XML time system */
   AstTimeScaleType ts;          /* TimeScale */
   IVOAScan *scan;               /* Structure holding scan results */
   char buff[ 200 ];             /* Message buffer */
   const char *iso;              /* Pointer to ISO date string */
   const char *names[3];         /* Names of the subelements to be searched for */
   const char *time_type;        /* Pointer to time type string */
   const char *unit;             /* Pointer to Unit string */
   double fval;                  /* Value converted to supplied TimeFrame */
   double offset;                /* Time offset */
   double result;                /* Time offset converted to required TimeFrame */
   double val;                   /* Value read from element */
   int max[3];                   /* Max allowed occurrences of each name */
   int min[3];                   /* Min allowed occurrences of each name */

/* Initialise. */
   offset = 0.0;

/* Check the global error status. */
   if ( !astOK ) return offset;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "JDTime|MJDTime|ISOTime";
   names[ 1 ] = "TimeOffset";
   names[ 2 ] = "TimeScale|Timescale";
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   scan = ScanIVOAElement( this, elem, 3, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* The supplied TimeFrame describes the system in which the caller wants
   the time values to be returned. This may not be the same as the system
   in which the value is stored in the XML. We create a TimeFrame
   describing the XML system, and later transform time values from the XML
   system to the system required by the caller. Any attributes of the XML
   system which are not specified in the XML are assumed to be equal to
   the values of the corresponding attributes in the supplied TimeFrame.
   If the XML system specifies values for attributes which have not been
   set in the supplied TimeFrame, then the values read fomr the XML are
   assigned to the attributes of the supplied TimeFrame. */
      cfrm = astCopy( frm );

      if( scan->count[2] ) {
         ts = TimeScaleReader( this, scan->el[2][0], status );
         astSetTimeScale( cfrm, ts );
         if( !astTestTimeScale( frm ) ) astSetTimeScale( frm, ts );
      }

/* If a JDTime element was found, get its value and set the TimeFrame System
   values. */
      time_type = astXmlGetName( scan->el[0][0] );
      if( !strcmp( "JDTime", time_type ) ) {
         val = ElemValueD( this, scan->el[0][0], 2400000.5, status );
         astSetSystem( cfrm, AST__JD );
         if( !astTestSystem( frm ) ) astSetSystem( frm, AST__JD );

/* If a ISOTime element was found, get its value and set the TimeFrame
   System attribute to MJD (the choice of AST System for an ISOTime is
   arbitrary - JD or JEPOCH could also have been used). */
      } else if( !strcmp( "ISOTime", time_type ) ) {
         astSetSystem( cfrm, AST__MJD );
         if( !astTestSystem( frm ) ) astSetSystem( frm, AST__MJD );
         iso = astXmlGetValue( scan->el[0][0], 0 );
         astClearTimeOrigin( cfrm );
         if( iso && astUnformat( cfrm, 0, iso, &val ) != strlen( iso ) ) {
            sprintf( buff, "contains unsupported ISO time format \"%s\"",
                     iso );
            Report( this, elem, FAILURE, buff, status );
         }

/* If an MJDTime was found, get its value and set System attributes. */
      } else {
         val = ElemValueD( this, scan->el[0][0], 2400000.5, status );
         astSetSystem( cfrm, AST__MJD );
         if( !astTestSystem( frm ) ) astSetSystem( frm, AST__MJD );
      }

/* Use this value as the TimeFrame's TimeOrigin value. Use the public
   astSetD rather than astSetTimeOrigin since the later requires the
   value to be supplied in the default units for the TimeFrame's System. */
      astSetD( cfrm, "TimeOrigin", val );

/* If the supplied Frame has no set TimeOrigin, also use the value
   obtained above as the TimeOrigin in "frm". Convert it into the supplied
   TimeFrame, and set it. Note zero is used as the axis value in cfrm
   because the relevant epoch is zero distance away from the cfrm
   TimeOrigin (set above). */
      if( !astTestTimeOrigin( frm ) ) {

         fs = astConvert( cfrm, frm, "" );
         if( fs ){
            val = 0.0;
            astTran1( fs, 1, &val, 1, &fval );
            astSetD( frm, "TimeOrigin", fval );
            fs = astAnnul( fs );
         } else if( astOK ) {
            sprintf( buff, "contains inconsistent timescale (%s)",
                     astGetC( cfrm, "timescale" ) );
            Report( this, elem, FAILURE, buff, status );
         }
      }

/* If an TimeOffset element was found, get its value and the value of its
   unit attribute (assume a default of days). Set the units in the
   TimeFrames. */
      if( scan->count[1] ) {
         offset = ElemValueD( this, scan->el[1][0], 0.0, status );
         unit = astXmlGetAttributeValue( scan->el[1][0], "unit" );
         if( !unit ) unit = "d";
         astSetUnit( cfrm, 0, unit );
         if( !astTestUnit( frm, 0 ) ) astSetUnit( frm, 0, unit );

/* If no offset was given, use zero. */
      } else {
         offset = 0.0;
      }

/* Convert the offset from the system in which it is stored in the XML to
   the system required by the caller. */
      fs = astConvert( cfrm, frm, "" );
      if( fs ){
         astTran1( fs, 1, &offset, 1, &result );
         fs = astAnnul( fs );
      } else if( astOK ) {
         sprintf( buff, "contains inconsistent timescale (%s)",
                  astGetC( cfrm, "timescale" ) );
         Report( this, elem, FAILURE, buff, status );
      }

/* Free resources. */
      cfrm = astAnnul( cfrm );
      scan = FreeIVOAScan( scan, status );
   }

/* Return the time value. */
   return result;
}

void astInitXmlChanVtab_(  AstXmlChanVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitXmlChanVtab

*  Purpose:
*     Initialise a virtual function table for an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     void astInitXmlChanVtab( AstXmlChanVtab *vtab, const char *name )

*  Class Membership:
*     XmlChan vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the XmlChan class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstChannelVtab *channel;      /* Pointer to Channel component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitChannelVtab( (AstChannelVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAXmlChan) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstChannelVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

   vtab->SetXmlLength = SetXmlLength;
   vtab->ClearXmlLength = ClearXmlLength;
   vtab->TestXmlLength = TestXmlLength;
   vtab->GetXmlLength = GetXmlLength;

   vtab->SetXmlFormat = SetXmlFormat;
   vtab->ClearXmlFormat = ClearXmlFormat;
   vtab->TestXmlFormat = TestXmlFormat;
   vtab->GetXmlFormat = GetXmlFormat;

   vtab->SetXmlPrefix = SetXmlPrefix;
   vtab->ClearXmlPrefix = ClearXmlPrefix;
   vtab->TestXmlPrefix = TestXmlPrefix;
   vtab->GetXmlPrefix = GetXmlPrefix;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   channel = (AstChannelVtab *) vtab;

   channel->WriteBegin = WriteBegin;
   channel->WriteIsA = WriteIsA;
   channel->WriteEnd = WriteEnd;
   channel->WriteInt = WriteInt;
   channel->WriteDouble = WriteDouble;
   channel->WriteString = WriteString;
   channel->WriteObject = WriteObject;

   channel->Read = Read;
   channel->ReadClassData = ReadClassData;
   channel->ReadDouble = ReadDouble;
   channel->ReadInt = ReadInt;
   channel->ReadObject = ReadObject;
   channel->ReadString = ReadString;

   parent_getindent = channel->GetIndent;
   channel->GetIndent = GetIndent;

   parent_getfull = channel->GetFull;
   channel->GetFull = GetFull;

   parent_getcomment = channel->GetComment;
   channel->GetComment = GetComment;


/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the class dump, copy and delete functions.*/
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "XmlChan", "XML I/O channel" );
   astSetDelete( vtab, Delete );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static double AttrValueD( AstXmlChan *this, AstXmlElement *elem,
                          const char *name, double def, int *status ) {
/*
*  Name:
*     AttrValueD

*  Purpose:
*     Read a floating point XML element attribute value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double AttrValueD( AstXmlChan *this, AstXmlElement *elem,
*                        const char *name, double def, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function returns the value of a named attribute of an XML
*     element as a floating point value. A report is made if the
*     attribute value is not floating point.The supplied default value is
*     returned if the attribute is not present.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required attribute value.
*     def
*        If the supplied element does not have the requried attribute, then
*        this value will be returned instead.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The required attribute value, or the default if the value was not found.

*/

/* Local Variables: */
   char buff[ 200 ];             /* Msg buffer */
   const char *value;            /* Pointer to attribute value */
   double result;                /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */
   int nf;                       /* Number of matching fields */
   int len;                      /* Length of attribute string */

/* Initialise. */
   result = def;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the attribute value as a string. */
   value = astXmlGetAttributeValue( elem, name );

/* If the attribute exists, attempt to decode the string to give a double
   value, checking that the entire string is read. */
   if( value ) {
      nc = 0;
      nf = astSscanf( value, " %lf %n", &result, &nc );
      len = strlen( value );

      if ( nf != 1 || nc < len ) {
         sprintf( buff, "contains a bad <%s> value: \"%s\"", name, value );
         Report( this, elem, WARNING, buff, status );
      }
   }

/* Return the result. */
   return result;
}

static int AttrValueI( AstXmlChan *this, AstXmlElement *elem, const char *name,
                       int def, int *status ) {
/*
*  Name:
*     AttrValueI

*  Purpose:
*     Read an integer XML element attribute value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int AttrValueI( AstXmlChan *this, AstXmlElement *elem, const char *name,
*                     int def )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function returns the value of a named attribute of an XML element
*     as an integer value. A report is made if the attribute value is not
*     integer. The supplied default value is returned if the attribute is not
*     present.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required attribute value.
*     def
*        If the supplied element does not have the requried attribute, then
*        this value will be returned instead.

*  Returned Value:
*     The required attribute value, or the default if the value was not found.

*/

/* Local Variables: */
   char buff[ 200 ];             /* Msg buffer */
   const char *value;            /* Pointer to attribute value */
   int result;                   /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */
   int nf;                       /* Number of matching fields */
   int len;                      /* Length of attribute string */

/* Initialise. */
   result = def;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the attribute value as a string. */
   value = astXmlGetAttributeValue( elem, name );

/* If the attribute exists, attempt to decode the string to give an integer
   value, checking that the entire string is read. */
   if( value ) {
      nc = 0;
      nf = astSscanf( value, " %d %n", &result, &nc );
      len = strlen( value );

      if ( nf != 1 || nc < len ) {
         sprintf( buff, "contains a bad <%s> value: \"%s\"", name, value );
         Report( this, elem, WARNING, buff, status );
      }
   }

/* Return the result. */
   return result;
}

static int AttrValueB( AstXmlChan *this, AstXmlElement *elem, const char *name,
                       int def, int *status ) {
/*
*  Name:
*     AttrValueB

*  Purpose:
*     Read a boolean XML element attribute value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int AttrValueB( AstXmlChan *this, AstXmlElement *elem, const char *name,
*                     int def, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function returns the value of a named attribute of an XML element
*     as a boolean. A report is made if the attribute value is not
*     boolean. The supplied default value is returned if the attribute is not
*     present.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required attribute value.
*     def
*        If the supplied element does not have the requried attribute, then
*        this value will be returned instead.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The required attribute value, or the default if the value was not found.

*/

/* Local Variables: */
   char buff[ 200 ];             /* Msg buffer */
   const char *value;            /* Pointer to attribute value */
   int result;                   /* Value to be returned */
   int i;                        /* Loop count */

/* Define the recognised true and false strings. */
   const char *true[ 5 ] = { "true", "TRUE", "yes", "YES", "1" };
   const char *false[ 5 ] = { "false", "FALSE", "no", "NO", "0" };

/* Initialise. */
   result = def;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the attribute value as a string. */
   value = astXmlGetAttributeValue( elem, name );

/* If the attribute exists, attempt to decode the string to give a boolean
   value. */
   if( value ) {

/* Indicate the result has not yet been determined. */
      result = -1;

/* See if the attribute value is equal to (or an abbreviation of) any of
   the true strings. */
      for( i = 0; i < 5; i++ ) {
         if( strstr( true[ i ], value ) == true[ i ] ) {
            result = 1;
            break;
         }
      }

/* If not, see if the attribute value is equal to (or an abbreviation of) any
   of the false strings. */
      if( result == -1 ) {
         for( i = 0; i < 5; i++ ) {
            if( strstr( false[ i ], value ) == false[ i ] ) {
               result = 0;
               break;
            }
         }
      }

/* If not, report a warning and return the default. */
      if( result == -1 ) {
         result = def;
         sprintf( buff, "contains a bad <%s> value: \"%s\"", name, value );
         Report( this, elem, WARNING, buff, status );
      }
   }

/* Return the result. */
   return result;
}

static AstRegion *BoxReader( AstXmlChan *this, AstXmlElement *elem,
                             AstFrame *frm, int *status ){
/*
*  Name:
*     BoxReader

*  Purpose:
*     Make an AST Region from an IVOA Box element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *BoxReader( AstXmlChan *this, AstXmlElement *elem,
*                           AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Box element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Box element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstMapping *map;              /* Mapping between units */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   const char *unit;             /* Centre and radii unit string */
   double cen[2];                /* Centre */
   double size[2];               /* Axis sizes */
   double pos[8];                /* Polygon vertex axis values */
   double *x;                    /* Pointer to first vertex X axis value */
   double *y;                    /* Pointer to first vertex Y axis value */
   int i;                        /* Axis count */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Center";
   names[ 1 ] = "Size";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the centre. */
      cen[0] = 0.0;
      cen[1] = 0.0;
      ElemListD( this, scan->el[0][0], 2, cen, status );

/* Get the size. */
      size[0] = 0.0;
      size[1] = 0.0;
      ElemListD( this, scan->el[1][0], 2, size, status );

/* Get the units attribute from the supplied element. These are the units
   of the centre and size values. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Since the SkyFrame class does not have active Units we must handle it
   separately. */
      if( astIsASkyFrame( frm ) ) {

/* Create the anti-clockwise list of (x,y) at the four corners of the box. */
         x = pos;
         y = pos+ 4;
         x[ 3 ] = cen[ 0 ] + 0.5*size[ 0 ];
         y[ 3 ] = cen[ 1 ] - 0.5*size[ 1 ];
         x[ 2 ] = cen[ 0 ] + 0.5*size[ 0 ];
         y[ 2 ] = cen[ 1 ] + 0.5*size[ 1 ];
         x[ 1 ] = cen[ 0 ] - 0.5*size[ 0 ];
         y[ 1 ] = cen[ 1 ] + 0.5*size[ 1 ];
         x[ 0 ] = cen[ 0 ] - 0.5*size[ 0 ];
         y[ 0 ] = cen[ 1 ] - 0.5*size[ 1 ];

/* Convert the axis values to radians. */
         map = astUnitMapper( unit, "rad", NULL, NULL );
         if( map ) {
            astTran1( map, 8, pos, 1, pos );
            map = astAnnul( map );
         } else if( astOK ) {
            Report( this, elem, FAILURE, "contains unusable units", status );
         }

/* Create the Polygon. */
         new = (AstRegion *) astPolygon( frm, 4, 4, pos, NULL, "", status );

/* Now handles Frames other than SkyFrames. */
      } else {

/* Create the anti-clockwise list of (x,y) at the four corners of the box. */
         x = pos;
         y = pos+ 4;
         x[ 0 ] = cen[ 0 ] + 0.5*size[ 0 ];
         y[ 0 ] = cen[ 1 ] - 0.5*size[ 1 ];
         x[ 1 ] = cen[ 0 ] + 0.5*size[ 0 ];
         y[ 1 ] = cen[ 1 ] + 0.5*size[ 1 ];
         x[ 2 ] = cen[ 0 ] - 0.5*size[ 0 ];
         y[ 2 ] = cen[ 1 ] + 0.5*size[ 1 ];
         x[ 3 ] = cen[ 0 ] - 0.5*size[ 0 ];
         y[ 3 ] = cen[ 1 ] - 0.5*size[ 1 ];

/* Take a copy of the supplied Frame and set its Units to the value
   obtained from the supplied element. */
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, unit );
         astSetUnit( cfrm, 1, unit );

/* Create a Polygon within this modified Frame. */
         new = (AstRegion *) astPolygon( frm, 4, 4, pos, NULL, "", status );

/* If the Unit of this Region differs from that of the supplied Frame,
   set it to the Unit of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Unit. If the supplied
   Frame had no set Unit, set it to the units obtained from the supplied
   element. */
         for( i = 0; i < 2; i++ ) {
            if( astTestUnit( frm, i ) ) {
               funit = astGetUnit( frm, i );
               if( strcmp( funit, unit ) ) astSetUnit( new, i, funit );
            } else {
               astSetUnit( frm, i, unit );
            }
         }

/* Free resources */
         cfrm = astAnnul( cfrm );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *CircleReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     CircleReader

*  Purpose:
*     Make an AST Region from an IVOA Circle element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *CircleReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Circle element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Circle element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstMapping *map;              /* Mapping between units */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   const char *unit;             /* Centre unit string from supplied element */
   double cen[2];                /* Centre */
   double rad;                   /* Radius */
   int i;                        /* Axis count */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Radius";
   names[ 1 ] = "Center";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the radius. */
      rad = ElemValueD( this, scan->el[0][0], 0.0, status );

/* Get the centre. */
      cen[0] = 0.0;
      cen[1] = 0.0;
      ElemListD( this, scan->el[1][0], 2, cen, status );

/* Get the units attribute from the supplied element. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Since the SkyFrame class does not have active Units we must handle it
   separately. */
      if( astIsASkyFrame( frm ) ) {

/* Convert the axis values and radius to radians. */
         map = astUnitMapper( unit, "rad", NULL, NULL );
         if( map ) {
            astTran1( map, 2, cen, 1, cen );
            astTran1( map, 1, &rad, 1, &rad );
            map = astAnnul( map );
         } else if( astOK ) {
            Report( this, elem, FAILURE, "contains unusable units", status );
         }

/* Create the Circle. */
         new = (AstRegion *) astCircle( frm, 1, cen, &rad, NULL, "", status );

/* Now handles Frames other than SkyFrames. */
      } else {

/* Take a copy of the supplied Frame and set its Units to the value
   obtained from the supplied element. */
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, unit );
         astSetUnit( cfrm, 1, unit );

/* Create a Circle within this modified Frame. */
         new = (AstRegion *) astCircle( cfrm, 1, cen, &rad, NULL, "", status );

/* If the Unit of this Region differs from that of the supplied Frame,
   set it to the Unit of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Unit. If the supplied
   Frame had no set Unit, set it to the units obtained from the supplied
   element. */
         for( i = 0; i < 2; i++ ) {
            if( astTestUnit( frm, i ) ) {
               funit = astGetUnit( frm, i );
               if( strcmp( funit, unit ) ) astSetUnit( new, i, funit );
            } else {
               astSetUnit( frm, i, unit );
            }
         }

/* Free resources */
         cfrm = astAnnul( cfrm );

      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Channel member function (over-rides the astClearAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function clears the value of a specified attribute for a
*     XmlChan so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;              /* Pointer to the XmlChan structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* XmlLength */
/* --------- */
   if ( !strcmp( attrib, "xmllength" ) ) {
      astClearXmlLength( this );

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      astClearXmlFormat( this );

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      astClearXmlPrefix( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static AstRegion *ConstraintReader( AstXmlChan *this, AstXmlElement *elem,
                                    AstFrame *frm, int *status ){
/*
*  Name:
*     ConstraintReader

*  Purpose:
*     Make an AST Region from an IVOA Constraint element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *ConstraintReader( AstXmlChan *this, AstXmlElement *elem,
*                                  AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Constraint element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Constraint element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. The Unit attribute is assumed to be set to "rad".
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[2];         /* Names of the subelements to be searched for */
   double cen[2];                /* Centre long/lat values */
   double vec[3];                /* Cartesian centre vector */
   double rad;                   /* Radius */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Vector";
   names[ 1 ] = "Offset";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the vector and convert from 3D cartesian to a 2D long/lat centre
   position, in radians. */
      vec[0] = 1.0;
      vec[1] = 0.0;
      vec[2] = 0.0;
      ElemListD( this, scan->el[0][0], 3, vec, status );
      palDcc2s( vec, cen, cen + 1 );

/* Get the offset, and convert to a radial distance in radians. */
      rad = acos( ElemValueD( this, scan->el[1][0], 1.0, status ) );

/* Create the Circle. */
      new = (AstRegion *) astCircle( frm, 1, cen, &rad, NULL, "", status );

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *ConvexReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     ConvexReader

*  Purpose:
*     Make an AST Region from an IVOA Convex element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *ConvexReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Convex element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Convex element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame to use  when building Constraints */
   AstRegion *new;               /* Pointer to returned Region */
   AstRegion *reg;               /* Pointer to component Region */
   AstRegion *tmp;               /* Pointer to new Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   const char *unit;             /* Unit attribute in element tag */
   int i;                        /* Loop count */
   int issky;                    /* Is supplied Frame a SkyFrame? */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for a Region sub-element. */
   names[ 0 ] = "Constraint";
   min[ 0 ] = 1;
   max[ 0 ] = INT_MAX;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Convex needs no units since all values are normalised to a unit sphere */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( unit ) {
         Report( this, elem, WARNING, "contains unnecessary unit attribute", status );
      }

/* Unless the supplied Frame is a SkyFrame (which handles the Unit
   attribute unusually), take a copy of the supplied Frame and set its
   units to radians. */
      issky = astIsASkyFrame( frm );
      if( issky ) {
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, "rad" );
         astSetUnit( cfrm, 1, "rad" );
      } else {
         cfrm = astClone( frm );
      }

/* Create Regions from all the component Constraint elements, and combine
   them into nested CmpRegions, using the boolean AND operator to combine
   them. */
      new = ConstraintReader( this, scan->el[0][0], cfrm, status );
      for( i = 1; i < scan->count[0]; i++ ) {
         reg = ConstraintReader( this, scan->el[0][i], cfrm, status );
         tmp = (AstRegion *) astCmpRegion( new, reg, AST__AND, "", status );
         reg = astAnnul( reg );
         (void) astAnnul( new );
         new = tmp;
      }

/* If required, modify the units back to their original values This
   will cause the axis values defining the returned Region to be re-mapped
   into the new units. Do not do this if the supplied Frame is a SkyFrame. */
      if( !issky ) {
         if( astTestUnit( frm, 0 ) ) astSetUnit( new, 0, astGetUnit( frm, 0 ) );
         if( astTestUnit( frm, 1 ) ) astSetUnit( new, 1, astGetUnit( frm, 1 ) );
      }

/* Get any fill factor from the element and assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      cfrm = astAnnul( cfrm );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}


static AstRegion *Coord2VecIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                           const char *unit, AstFrame *frm, int *status ){
/*
*  Name:
*     Coord2VecIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA Coord2VecInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *Coord2VecIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                         const char *unit, AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Coord2VecInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Coord2VecInterval element.
*     unit
*        A string holding the units in which the axis values are stored
*        in the supplied element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstMapping *map;              /* Mapping from supplied units to rads */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   double hilimit[2];            /* Upper limits */
   double lolimit[2];            /* Lower limits */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "LoLimit2Vec";
   names[ 1 ] = "HiLimit2Vec";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      lolimit[0] = AST__BAD;
      lolimit[1] = AST__BAD;
      if( scan->count[0] ) ElemListD( this, scan->el[0][0], 2, lolimit, status );

      hilimit[0] = AST__BAD;
      hilimit[1] = AST__BAD;
      if( scan->count[1] ) ElemListD( this, scan->el[1][0], 2, hilimit, status );

/* Since the SkyFrame class does not have active Units we must handle it
   separately. */
      if( astIsASkyFrame( frm ) ) {

/* Convert the limit values to radians. */
         map = astUnitMapper( unit, "rad", NULL, NULL );
         if( map ) {
            astTran1( map, 2, lolimit, 1, lolimit );
            astTran1( map, 2, hilimit, 1, hilimit );
            map = astAnnul( map );
         } else if( astOK ) {
            Report( this, elem, FAILURE, "contains unusable units", status );
         }

/* If at least one limit was found, create an Interval within the supplied
   Frame. Otherwise create a negated NullRegion. */
         if( lolimit[ 0 ] != AST__BAD || lolimit[ 1 ] != AST__BAD ||
             hilimit[ 0 ] != AST__BAD || hilimit[ 1 ] != AST__BAD ) {
            new = (AstRegion *) astInterval( frm, lolimit, hilimit, NULL, "", status );
         } else {
            new = (AstRegion *) astNullRegion( frm, NULL, "negated=1", status );
         }

/* Now handles Frames other than SkyFrames. */
      } else {

/* Take a copy of the supplied Frame and set its Unit attribute to the
   supplied value. */
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, unit );

/* If at least one limit was found, create an Interval within this
   modified Frame. Otherwise create a negated NullRegion. */
         if( lolimit[ 0 ] != AST__BAD || lolimit[ 1 ] != AST__BAD ||
             hilimit[ 0 ] != AST__BAD || hilimit[ 1 ] != AST__BAD ) {
            new = (AstRegion *) astInterval( cfrm, lolimit, hilimit, NULL, "", status );
         } else {
            new = (AstRegion *) astNullRegion( cfrm, NULL, "negated=1", status );
         }

/* If the supplied units differ from that of the supplied Frame, set the
   units in the Region to those of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the required units. If the supplied
   Frame had no set Units, set it to the supplied units. */
         if( astTestUnit( frm, 0 ) ) {
            funit = astGetUnit( frm, 0 );
            if( strcmp( funit, unit ) ) astSetUnit( new, 0, funit );
         } else {
            astSetUnit( frm, 0, unit );
         }

/* Free resources */
         cfrm = astAnnul( cfrm );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *Coord3VecIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                           const char *unit, AstFrame *frm, int *status ){
/*
*  Name:
*     Coord3VecIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA Coord3VecInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *Coord3VecIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                         const char *unit, AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Coord3VecInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Coord3VecInterval element.
*     unit
*        A string holding the units in which the axis values are stored
*        in the supplied element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   double hilimit[3];            /* Upper limits */
   double lolimit[3];            /* Lower limits */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "LoLimit3Vec";
   names[ 1 ] = "HiLimit3Vec";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      lolimit[0] = AST__BAD;
      lolimit[1] = AST__BAD;
      lolimit[2] = AST__BAD;
      if( scan->count[0] ) ElemListD( this, scan->el[0][0], 3, lolimit, status );

      hilimit[0] = AST__BAD;
      hilimit[1] = AST__BAD;
      hilimit[2] = AST__BAD;
      if( scan->count[1] ) ElemListD( this, scan->el[1][0], 3, hilimit, status );

/* Take a copy of the supplied Frame and set its Unit attribute to the
   supplied value. */
      cfrm = astCopy( frm );
      astSetUnit( cfrm, 0, unit );

/* If at least one limit was found, create an Interval within this
   modified Frame. Otherwise create a negated NullRegion. */
      if( lolimit[ 0 ] != AST__BAD || lolimit[ 1 ] != AST__BAD ||
          lolimit[ 2 ] != AST__BAD ||
          hilimit[ 0 ] != AST__BAD || hilimit[ 1 ] != AST__BAD ||
          hilimit[ 2 ] != AST__BAD ) {
         new = (AstRegion *) astInterval( cfrm, lolimit, hilimit, NULL, "", status );
      } else {
         new = (AstRegion *) astNullRegion( cfrm, NULL, "negated=1", status );
      }

/* If the supplied units differ from that of the supplied Frame, set the
   units in the Region to those of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the required units. If the supplied
   Frame had no set Units, set it to the supplied units. */
      if( astTestUnit( frm, 0 ) ) {
         funit = astGetUnit( frm, 0 );
         if( strcmp( funit, unit ) ) astSetUnit( new, 0, funit );
      } else {
         astSetUnit( frm, 0, unit );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      cfrm = astAnnul( cfrm );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *CoordScalarIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                             const char *unit, AstFrame *frm, int *status ){
/*
*  Name:
*     CoordScalarIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA CoordScalarInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *CoordScalarIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                           const char *unit, AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     CoordScalarInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA CoordScalarInterval element.
*     unit
*        A string holding the units in which the axis values are stored
*        in the supplied element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   double hilimit;               /* Upper limit */
   double lolimit;               /* Lower limit */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "LoLimit";
   names[ 1 ] = "HiLimit";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      lolimit = scan->count[0] ? ElemValueD( this, scan->el[0][0], 0.0, status ) : AST__BAD;
      hilimit = scan->count[1] ? ElemValueD( this, scan->el[1][0], 0.0, status ) : AST__BAD;

/* Take a copy of the supplied Frame and set its Unit attribute to the
   supplied value. */
      cfrm = astCopy( frm );
      astSetUnit( cfrm, 0, unit );

/* If at least one limit was found, create an Interval within this
   modified Frame. Otherwise create a negated NullRegion. */
      if( lolimit != AST__BAD || hilimit != AST__BAD ) {
         new = (AstRegion *) astInterval( cfrm, &lolimit, &hilimit, NULL, "", status );
      } else {
         new = (AstRegion *) astNullRegion( cfrm, NULL, "negated=1", status );
      }

/* If the supplied units differ from that of the supplied Frame, set the
   units in the Region to those of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the required units. If the supplied
   Frame had no set Units, set it to the supplied units. */
      if( astTestUnit( frm, 0 ) ) {
         funit = astGetUnit( frm, 0 );
         if( strcmp( funit, unit ) ) astSetUnit( new, 0, funit );
      } else {
         astSetUnit( frm, 0, unit );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      cfrm = astAnnul( cfrm );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static int ElemListD( AstXmlChan *this, AstXmlElement *elem, int n,
                      double *vals, int *status ) {
/*
*  Name:
*     ElemListD

*  Purpose:
*     Read a floating point XML element value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int ElemListD( AstXmlChan *this, AstXmlElement *elem, int n,
*                    double *vals, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function reads the content of the supplied element, converts its
*     contents to a list of floating point values and returns these
*     values in "values". A report is made if the element value is not a
*     space separated list of floating point values, or if it contains
*     more than "n" values. The number of values stored in "values" is
*     returned as the function value.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement.
*     n
*        The maximum number of floating point values to read from the supplied
*        element.
*     values
*        Pointer to an array to hold the values read. This should have at
*        least "n" elements. Any unused elements are left unchanged.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The number of values stored in "values".

*/

/* Local Variables: */
   AstXmlContentItem *item;  /* Item no. "i" */
   char *text;               /* Pointer to string holding formatted item */
   char buff[200];           /* Message buffer */
   const char *p;            /* Pointer to start of remaining text */
   const char *value;        /* Pointer to element value */
   double dval;              /* Value read from string */
   int i;                    /* Index of current item */
   int l;                    /* Used length of string */
   int nc;                   /* Number of characters read by astSscanf */
   int nitem;                /* Number of items in the element */
   int rep;                  /* Has a warning about excess values been made? */
   int result;               /* Value to be returned */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* No warning has yet been made avbout excess values */
   rep = 0;

/* Loop through all content items within the supplied element. */
   nitem = astXmlGetNitem( elem );
   for( i = 0; i < nitem; i++ ) {
      item = astXmlGetItem( elem, i );

/* If this is non-blank character data, attempt to read values from it. */
      if( astXmlCheckType( item, AST__XMLBLACK ) ) {

/* Get the element value as a string. */
         value = astXmlGetValue( item, 0 );
         if( value ) {

/* Loop round reading floating point values from the text until the
   end of the string is reached. */
            l = astChrLen( value );
            p = value;
            while( p < value + l ){

/* Read a floating point value from the start of the remaining string,
   storing the result in the supplied array. If succesful, increment the
   number of values read, and increment the pointer to the start of the
   remaining string. Abort if too many values are found. */
               if( astSscanf( p, " %lf %n", &dval, &nc ) == 1 ) {
                  if( result < n ) {
                     vals[ result++ ] = dval;
                     p += nc;
                  } else {
                     if( !rep ) {
                        rep = 1;
                        if( n > 1 ) {
                           sprintf( buff, "contains more than %d values - "
                                    "extra values will be ignored", n );
                        } else {
                           sprintf( buff, "contains more than 1 value - "
                                    "extra values will be ignored" );
                        }
                        Report( this, elem, WARNING, buff, status );
                     }
                     break;
                  }

/* If the remaing text is not a floating point value, then issue a report. */
               } else {
                  Report( this, elem, FAILURE, "contains a non-numerical value", status );
                  break;
               }
            }
         }

/* If this is not character data, nor a comment, issue a warning. */
      } else if( !astXmlCheckType( item, AST__XMLWHITE ) &&
                 !astXmlCheckType( item, AST__XMLCOM ) ) {
         text = (char *) astXmlFormat( item );
         if( text ) {
            if( strlen( text ) > 30 ) text[ 30 ] = 0;
            sprintf( buff, "contains the following which is being ignored: \"%s\"",
                     text );
            text = astFree( text );
            Report( this, elem, WARNING, buff, status );
         }
      }
   }

/* Return the result. */
   return result;
}

static double ElemValueD( AstXmlChan *this, AstXmlElement *elem, double def, int *status ) {
/*
*  Name:
*     ElemValueD

*  Purpose:
*     Read a floating point XML element value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double ElemValueD( AstXmlChan *this, AstXmlElement *elem, double def )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function reads the content of the supplied element, converts its
*     contents to a floating point value and returns this value. A report is
*     made if the element value is not floating point. The supplied default
*     value is returned if the element is not present.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement.
*     def
*        If the content of the supplied element is not a flaoting point
*        value, then this value will be returned instead.

*  Returned Value:
*     The required element value, or the default if the value was not found.

*/

/* Local Variables: */
   const char *value;            /* Pointer to element value */
   double result;                /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */
   int ok;                       /* Value read OK? */

/* Initialise. */
   result = def;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Assume the value cannot be read. */
   ok = 0;

/* Get the element value as a string. */
   value = astXmlGetValue( elem, 0 );

/* If succesful, convert the value to floating point. */
   if( value ) {
      nc = 0;
      ok = ( ( 1 == astSscanf( value, " %lf %n", &result, &nc ) )
               && ( nc >= (int) strlen( value ) ) );
   }

/* Give a warning if not OK, and use default value. */
   if( !ok ) {
      Report( this, elem, FAILURE, "does not contain a floating point value", status );
      result = def;
   }

/* Return the result. */
   return result;
}

static AstRegion *EllipseReader( AstXmlChan *this, AstXmlElement *elem,
                                 AstFrame *frm, int *status ){
/*
*  Name:
*     EllipseReader

*  Purpose:
*     Make an AST Region from an IVOA Ellipse element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *EllipseReader( AstXmlChan *this, AstXmlElement *elem,
*                               AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Ellipse element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Ellipse element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstMapping *map;              /* Mapping between units */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[4];         /* Names of the subelements to be searched for */
   const char *unit;             /* Centre and radii unit string */
   double cen[2];                /* Centre */
   double pa;                    /* Major axis position angle */
   double rad[2];                /* Major and minor radii */
   int i;                        /* Axis count */
   int max[4];                   /* Max allowed occurrences of each name */
   int min[4];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Radius";
   names[ 1 ] = "Center";
   names[ 2 ] = "MinorRadius";
   names[ 3 ] = "PosAngle";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   min[ 2 ] = 1;
   min[ 3 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   max[ 3 ] = 1;
   scan = ScanIVOAElement( this, elem, 4, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the major radius */
      rad[ 0 ] = ElemValueD( this, scan->el[0][0], 0.0, status );

/* Get the minor radius. */
      rad[ 1 ] = ElemValueD( this, scan->el[2][0], 0.0, status );

/* Get the centre. */
      cen[0] = 0.0;
      cen[1] = 0.0;
      ElemListD( this, scan->el[1][0], 2, cen, status );

/* Get the position angle. This is returned in the AST convention, i.e.
   measured in radians from from +ve second axis through positive first
   axis. */
      pa = PosAngleReader( this, scan->el[3][0], status );

/* Get the units attribute from the supplied element. These are the units
   of the centre and radii value. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Since the SkyFrame class does not have active Units we must handle it
   separately. */
      if( astIsASkyFrame( frm ) ) {

/* Convert the axis values and radii to radians. */
         map = astUnitMapper( unit, "rad", NULL, NULL );
         if( map ) {
            astTran1( map, 2, cen, 1, cen );
            astTran1( map, 2, rad, 1, rad );
            map = astAnnul( map );
         } else if( astOK ) {
            Report( this, elem, FAILURE, "contains unusable units", status );
         }

/* Create the Ellipse. */
         new = (AstRegion *) astEllipse( frm, 1, cen, rad, &pa, NULL, "", status );

/* Now handles Frames other than SkyFrames. */
      } else {

/* Take a copy of the supplied Frame and set its Units to the value
   obtained from the supplied element. */
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, unit );
         astSetUnit( cfrm, 1, unit );

/* Create a Ellipse within this modified Frame. */
         new = (AstRegion *) astEllipse( cfrm, 1, cen, rad, &pa, NULL, "", status );

/* If the Unit of this Region differs from that of the supplied Frame,
   set it to the Unit of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Unit. If the supplied
   Frame had no set Unit, set it to the units obtained from the supplied
   element. */
         for( i = 0; i < 2; i++ ) {
            if( astTestUnit( frm, i ) ) {
               funit = astGetUnit( frm, i );
               if( strcmp( funit, unit ) ) astSetUnit( new, i, funit );
            } else {
               astSetUnit( frm, i, unit );
            }
         }

/* Free resources */
         cfrm = astAnnul( cfrm );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static double Error2PAReader( AstXmlChan *this, AstXmlElement *elem,
                              double *size, int *status ){
/*
*  Name:
*     Error2PAReader

*  Purpose:
*     Read the contents of an Stc Error2PA element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double Error2PAReader( AstXmlChan *this, AstXmlElement *elem,
*                            double *size, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the contents of an Stc Error2PA element. It can
*     also be used to read Resolution2PA, Size2PAand PixSize2PA which
*     have exactly the same structure as a Error2PA element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Ellipse element.
*     size
*        Pointer to an array to receive the 2 error sizes.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The position angle of the first error size, in radians, from positive
*     second axis to positive first axis.

*/

/* Local Variables: */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[4];         /* Names of the subelements to be searched for */
   double pa;                    /* Major axis position angle */
   int max[4];                   /* Max allowed occurrences of each name */
   int min[4];                   /* Min allowed occurrences of each name */

/* Initialise. */
   pa = 0.0;

/* Check the global error status. */
   if ( !astOK ) return pa;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Size";
   names[ 1 ] = "PosAngle";
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the sizes */
      ElemListD( this, scan->el[0][0], 2, size, status );

/* Get the position angle. This is returned in the AST convention, i.e.
   measured in radians from from +ve second axis through positive first
   axis. */
      pa = PosAngleReader( this, scan->el[1][0], status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Return the position angle. */
   return pa;
}

static void FillAndLims( AstXmlChan *this, AstXmlElement *elem, AstRegion *new, int *status ){
/*
*  Name:
*     FillAndLims

*  Purpose:
*     Get fill factor and limit inclusion flags from IVOA element and
*     assign to an AST Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void FillAndLims( AstXmlChan *this, AstXmlElement *elem, AstRegion *new, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function gets attributes from the supplied element describing
*     fill factor and limit inclusion flags, and assigns suitable values
*     to the supplied Region. Default values are used if the supplied
*     element does not have the required attributes.

*  Parameters:
*     this
*        Pointer to the XmlChan in which to store warnings.
*     elem
*        Pointer to the AstXmlElement to search.
*     new
*        Pointer to the Region in which to store the values.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   const char *text;        /* Attribute text */
   double ff;               /* Fill factor */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get any fill factor attribute from the element and assign to the
   returned Region. */
   ff = AttrValueD( this, elem, "fill_factor", AST__BAD, status );
   if( ff != AST__BAD ) astSetFillFactor( new, ff );

/* Get the flags indicating if the limits are included in the interval.
   If either of the limits is not included in the interval, then make the
   Region open. Assume a default of true ("included") if the attribute is
   missing. */
   text = astXmlGetAttributeValue( elem, "lo_include" );
   if( text && !strcmp( text, "false" ) ) astSetClosed( new, 0 );

   text = astXmlGetAttributeValue( elem, "hi_include" );
   if( text && !strcmp( text, "false" ) ) astSetClosed( new, 0 );

}

static AstXmlElement *FindAttribute( AstXmlChan *this, const char *name, int *status ) {
/*
*  Name:
*     FindAttribute

*  Purpose:
*     Find an XML element representing a named AST attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *FindAttribute( AstXmlChan *this, const char *name, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the current container element
*     of the supplied XmlChan looking for an element which represents a
*     named AST attribute. No error is reported if the attribute is not
*     found. Attributes which represent defaul values are ignored.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     name
*        Pointer to a string holding the required AST attribute name
*        (case-insensitive).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the XmlElement if found, and NULL otherwise.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item no. "i" */
   AstXmlElement *result;   /* Returned pointer */
   const char *def;         /* Value from XML DEFAULT attribute */
   const char *definedby;   /* Name of class which defines the item */
   const char *xmlname;     /* Value from XML NAME attribute */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still in the element */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Report an error if the class being loaded has not been set. */
   if( !this->isa_class ) {
      astError( AST__INTER, "astRead(XmlChan): astReadNextData not called "
                "before reading values for a %s (internal AST programming "
                "error).", status, astXmlGetName( this->container ) );
   }

/* Check we have a container to search. */
   if( !this->container ) {
      astError( AST__INTER, "astRead(XmlChan): No container before reading "
                "values for a %s (internal AST programming error).", status,
                astXmlGetName( this->container ) );
   }

/* Check all is OK. */
   if( astOK ) {

/* Loop round all items in the elements contents. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( this->container, i );

/* Ignore this item if it is not an element. */
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* Ignore this element if its name is not ATTR. */
            if( !astOK ) break;
            if( !strcmp( astXmlGetName( item ), ATTR ) ){

/* Ignore this element if it represents a default value. */
               def = astXmlGetAttributeValue( item, DEFAULT );
               if( !def || strcmp( def, TRUE ) ) {

/* If this ATTR element has an XML attribute called NAME with
   the required value (case-insensitive), we may have found a matching
   element. */
                   xmlname = astXmlGetAttributeValue( item, NAME );
                   if( xmlname && !Ustrcmp( xmlname, name, status ) ) {

/* Ignore the attribute if it does not belong to the correct part of the
   object's class hierarchy. If it does, we have found the required
   attribute. */
                      definedby = astXmlGetAttributeValue( item, DEFINEDBY );
                      if( definedby && !strcmp( definedby, this->isa_class ) ) {
                         result = (AstXmlElement *) item;
                         break;
                      }
                   }
               }
            }
         }
      }
   }

/* Return the pointer. */
   return result;
}

static AstXmlElement *FindElement( AstXmlChan *this, AstXmlElement *elem,
                                   const char *name, int *status ) {
/*
*  Name:
*     FindElement

*  Purpose:
*     Find a named element within a supplied element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *FindElement( AstXmlChan *this, AstXmlElement *elem,
*                                 const char *name, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the supplied element looking for
*     an element with the supplied Name. No error is reported if the element
*     is not found, but a Warning is issued if it found more than once.

*  Parameters:
*     this
*        Pointer to the XmlChan in which to store warnings.
*     elem
*        Pointer to the AstXmlElement to search.
*     name
*        Pointer to a string holding the required element name.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the XmlElement if found, and NULL otherwise.

* Notes:
*     - If the supplied element contains more than one element with the
*     given name, the returned pointer locates the first element
*     encountered with the required name, and a WARNING is issued that the
*     second and subsequent elements will be ignored.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item no. "i" */
   AstXmlElement *result;   /* Returned pointer */
   char buff[ 200 ];        /* Message buffer */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still in the element */
   int warned;              /* Has a warning been issued? */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Loop round all items in the elements contents. */
   warned = 0;
   nitem = astXmlGetNitem( elem );
   for( i = 0; i < nitem; i++ ) {
      item = astXmlGetItem( elem, i );

/* Ignore this item if it is not an element. */
      if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* If this element's name is the given name. */
         if( !strcmp( astXmlGetName( item ), name ) ){

/* If this is the first element with the required name, store its
   pointer. */
            if( !result ) {
               result = (AstXmlElement *) item;

/* Otherwise add a Warning (unles a Warning has already been issued). */
            } else if( !warned ) {
               warned = 1;
               sprintf( buff, "contains more than one %s element. The "
                        "second and subsequent such elements will be "
                        "ignored", name );
               Report( this, elem, WARNING, buff, status );
            }
         }
      }
   }

/* Return the pointer. */
   return result;
}

static IVOAReader FindIVOAClass( AstXmlElement *elem, int *is_ivoa, int *status ) {
/*
*  Name:
*     FindIVOAClass

*  Purpose:
*     Return a pointer to a function which will create an AST Object from
*     an IVOA element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     IVOAReader FindIVOAClass( AstXmlElement *elem, int *is_ivoa, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function checks the namespace of the supplied element to see if
*     it is a known IVOA namespace. If it is, it returns the "is_ivoa"
*     flag set to non-zero (otherwise it is returned as zero). It then
*     checks to see if an AST object can be created from the IVOA
*     element. If so, a pointer to the function which will do the
*     conversion is returned. Otherwise a NULL pointer is returned.

*  Parameters:
*     elem
*        Pointer to the element to check.
*     is_ivoa
*        Pointer to an int in which to return a flag indicating if the
*        supplied element belongs to a known IVOA namespace.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the function (if any) which can produce an AST Object
*     from the supplied element, or NULL if conversion is not possible.

*  Notes:
*     - NULL is returned if this function is invoked with the error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   IVOAReader result;      /* Returned pointer */
   const char *ivoa;       /* Pointer to "ivoa" substring */
   const char *name;       /* Pointer to string holding element name */
   const char *stc;        /* Pointer to "stc" substring */
   const char *uri;        /* Pointer to string holding element namespace URI */

/* Initialise */
   *is_ivoa = 0;
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the element name.*/
   name = astXmlGetName( elem );

/* Get the namespace URI for the element, and see if it contains
   sub-strings "STC" (or "stc") and "IVOA" (or "ivoa"). */
   uri = astXmlGetURI( elem );
   if( uri ) {
      stc = strstr( uri, "STC" );
      if( !stc ) stc = strstr( uri, "stc" );
      ivoa = strstr( uri, "IVOA" );
      if( !ivoa ) ivoa = strstr( uri, "ivoa" );

   } else {
      stc = NULL;
      ivoa = NULL;
   }

/* If it is a known IVOA namespace, proceed. */
   if( name && ( stc || ivoa ) ){
      *is_ivoa = 1;

/* Look for element types which can be converted to AST objects, and
   return a pointer to the corresponding reader function. */
      if( !strcmp( name, STC_RESOURCE_PROFILE ) ) {
         result = StcMetadataReader;

      } else if( !strcmp( name, SEARCH_LOCATION ) ) {
         result = StcMetadataReader;

      } else if( !strcmp( name, CATALOG_ENTRY_LOCATION ) ) {
         result = StcMetadataReader;

      } else if( !strcmp( name, OBSERVATION_LOCATION ) ) {
         result = StcMetadataReader;

      } else if( !strcmp( name, OBS_DATA_LOCATION ) ) {
         result = ObsDataLocationReader;

      } else if( !strcmp( name, ASTRO_COORD_SYSTEM ) ) {
         result = AstroCoordSystemReader;

      } else if( !strcmp( name, TIME_FRAME ) ) {
         result = TimeFrameReader;

      } else if( !strcmp( name, SPACE_FRAME ) ) {
         result = SpaceFrameReader;

      } else if( !strcmp( name, SPECTRAL_FRAME ) ) {
         result = SpectralFrameReader;

      } else if( !strcmp( name, REDSHIFT_FRAME ) ) {
         result = RedshiftFrameReader;

      } else if( !strcmp( name, REDSHIFT_FRAME ) ) {
         result = RedshiftFrameReader;

      }
   }

/* Return null if an error occurred. */
   if( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static const char *FindNextIsA( AstXmlElement *elem, int start, int *status ) {
/*
*  Name:
*     FindNextIsA

*  Purpose:
*     Find the next "isa" element within an XML element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *FindNextIsA( AstXmlElement *elem, int start, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the specified element,
*     starting at the item with the speicfied index, until it finds the
*     next "isa" element. It returns the value of the "class" attribute
*     of the found "isa" element, or the name of the supplied element
*     if no "isa" element is found.

*  Parameters:
*     elem
*        Pointer to the XmlElement (an element describing an AST Object).
*     start
*        The index of the first content item to check.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the class string.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item no. "i" */
   const char *result;      /* Returned string */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still i nthe element */

/* Initialise */
   result = astXmlGetName( elem );

/* Check the global error status. */
   if ( !astOK ) return result;

/* Loop round all items contained in the element, starting at the given
   index. */
   nitem = astXmlGetNitem( elem );
   for( i = start; i < nitem; i++ ) {
      item = astXmlGetItem( elem, i );

/* Check this item is an XmlElement with name ISA. */
      if( astXmlCheckType( item, AST__XMLELEM ) ) {
         if( astOK && !strcmp( astXmlGetName( item ), ISA ) ) {

/* The returned string is the value of the "class" attribute of this
   element. */
            result = astXmlGetAttributeValue( item, "class" );

/* Report an error if the element does not have a class attribute. */
            if( !result && astOK ) {
               astError( AST__BADIN, "astRead(XmlChan): The tag %s "
                         "does not include a \"class\" attribute.", status,
                         GetTag( (AstXmlObject *) item, 1, status ) );
            }

            break;

         }
      }
   }

/* Return the result. */
   return result;
}

static AstXmlElement *FindObject( AstXmlChan *this, const char *name, int *status ) {
/*
*  Name:
*     FindObject

*  Purpose:
*     Find an XML element representing a named AST Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *FindObject( AstXmlChan *this, const char *name, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the current container element
*     of the supplied XmlChan looking for an element which represents a
*     named AST Object. No error is reported if the object is not
*     found. Objects which represent default values are ignored.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     name
*        Pointer to a string holding the required AST object name
*        (case-insensitive).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the XmlElement if found, and NULL otherwise.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item */
   AstXmlElement *result;   /* Returned pointer */
   const char *def;         /* Value from XML DEFAULT attribute */
   const char *definedby;   /* Name of class which defines the item */
   const char *xmlname;     /* Value from XML LABEL attribute */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still i nthe element */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Report an error if the class being loaded has not been set. */
   if( !this->isa_class ) {
      astError( AST__INTER, "astRead(XmlChan): astReadNextData not called "
                "before reading values for a %s (internal AST programming "
                "error).", status, astXmlGetName( this->container ) );
   }

/* Check we have a container to search. */
   if( !this->container ) {
      astError( AST__INTER, "astRead(XmlChan): No container before reading "
                "values for a %s (internal AST programming error).", status,
                astXmlGetName( this->container ) );
   }

/* Check all is OK. */
   if( astOK ) {

/* Loop round all items in the elements contents. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( this->container, i );

/* Ignore this item if it is not an element. */
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* Ignore this element if its name is ATTR. */
            if( astOK && strcmp( astXmlGetName( item ), ATTR ) ){

/* Ignore this element if it represents a default value. */
               def = astXmlGetAttributeValue( item, DEFAULT );
               if( !def || strcmp( def, TRUE ) ) {

/* If this non-ATTR element has an XML attribute called LABEL with
   the required value (case-insensitive), we may have found a matching element. */
                   xmlname = astXmlGetAttributeValue( item, LABEL );
                   if( xmlname && !Ustrcmp( xmlname, name, status ) ) {

/* Ignore the element if it does not belong to the correct part of the
   object's class hierarchy. If it does, we have found the required
   object. */
                      definedby = astXmlGetAttributeValue( item, DEFINEDBY );
                      if( definedby && !strcmp( definedby, this->isa_class ) ) {
                         result = (AstXmlElement *) item;
                         break;
                      }
                   }
               }
            }
         }
      }
   }

/* Return the pointer. */
   return result;
}

static int FindString( int n, const char *list[], const char *test,
                       const char *text, const char *method,
                       const char *class, int *status ){
/*
*  Name:
*     FindString

*  Purpose:
*     Find a given string within an array of character strings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int FindString( int n, const char *list[], const char *test,
*                     const char *text, const char *method, const char *class, int *status )

*  Class Membership:
*     XmlChan method.

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
*     status
*        Pointer to the inherited status variable.

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
      if( !Ustrcmp( test, list[ ret ], status ) ) break;
   }

/* Report an error if the supplied test string does not match any element
   in the supplied list. */
   if( ret >= n && astOK ) {
      astError( AST__RDERR, "%s(%s): Illegal value '%s' supplied for %s.", status,
                method, class, test, text );
      ret = -1;
   }

/* Return the answer. */
   return ret;
}

static IVOAScan *FreeIVOAScan( IVOAScan *in, int *status ){
/*
*  Name:
*     FreeIVOAScan

*  Purpose:
*     Free resources used by an IVOAScan structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     IVOAScan *FreeIVOAScan( IVOAScan *in )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function frees resources used by an IVOAScan structure (such
*     as returned by the ScanIVOAElement function).

*  Parameters:
*     in
*        Pointer to the IVOAScan structure.

*  Returned Value:
*     A NULL pointer.

*/

/* Local Variables: */
   int j;                        /* Index of current name */

/* Check the supplied pointer can be used safely. */
   if( in ) {

/* Free the arrays holding the element pointers. */
      for( j = 0; j < in->n; j++ ) {
         in->count[ j ] = 0;
         in->el[ j ] = astFree( in->el[ j ] );
      }

/* Free the array holding the pointers to the arrays holding the element
   pointers. */
      in->el = astFree( in->el );

/* Free the array holding the number of element pointers stored. */
      in->count = astFree( in->count );

/* For safety, put a zero in the name count. */
      in->n = 0;

/* Free the whole structure. */
      in = astFree( in );

   }

   return NULL;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetAttrib
*     method inherited from the Channel class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a XmlChan, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the XmlChan, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the XmlChan. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Integer attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* XmlLength */
/* --------- */
   if ( !strcmp( attrib, "xmllength" ) ) {
      ival = astGetXmlLength( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      ival = astGetXmlFormat( this );
      if ( astOK ) {
         if( ival == NATIVE_FORMAT ){
            result = NATIVE_STRING;

         } else if( ival == QUOTED_FORMAT ){
            result = QUOTED_STRING;

         } else if( ival == IVOA_FORMAT ){
            result = IVOA_STRING;

         } else {
            result = UNKNOWN_STRING;
         }
      }

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      result = astGetXmlPrefix( this );

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;
}


static int GetComment( AstChannel *this, int *status ) {
/*
*  Name:
*     GetComment

*  Purpose:
*     Get the value of the Comment attribute of a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int GetComment( AstChannel *this, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetComment
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Comment attribute of the XmlChan.
*     It changs the default value from 1 (provided by the parent Channel
*     class) to zero.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The Comment value.
*/

   return astTestComment( this ) ? (*parent_getcomment)( this, status ) : 0;
}

static int GetFull( AstChannel *this, int *status ) {
/*
*  Name:
*     GetFull

*  Purpose:
*     Get the value of the Full attribute of a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int GetFull( AstChannel *this, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetFull
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Full attribute of the XmlChan.
*     It changs the default value from zero (provided by the parent Channel
*     class) to -1.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The Full value.
*/

   return astTestFull( this ) ? (*parent_getfull)( this, status ) : -1;
}

static int GetIndent( AstChannel *this, int *status ) {
/*
*  Name:
*     GetIndent

*  Purpose:
*     Get the value of the Indent attribute for an XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "XmlChan.h"
*     int GetIndent( AstChannel *this, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetIndent
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Indent attribute, supplying
*     a default value appropriate to an XmlChan.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The Indent value to use.

*/

/* If the attribute is set, return its value. Otherwise return a value of
   zero. */
   return astTestIndent( this ) ? (*parent_getindent)( this, status ) : 0;
}


static char GetNextChar( void *data, int *status ) {
/*
*  Name:
*     GetNextChar

*  Purpose:
*     Get the next character from the XML source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char GetNextChar( void *data, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function returns the next character from the XML source,
*     getting a new string if necessary.

*  Parameters:
*     data
*        Pointer to a structure holding data needed to perform the read.
*        This should be a pointer to the XmlChan being read. If NULL is
*        supplied, then any internal resources are freed and a value of
*        zero is returned.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The next source character, or zero if NULL is supplied for "data".

*  Notes:
*     - Zero is returned if there is no more text to read.
*     - Zero is returned if an error has already occurred, or if this
*     function should failed for any reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS        /* Declare the thread specific global data */
   AstXmlChan *this;         /* Pointer to the XmlChan */
   char result;              /* The returned character */

/* Initiialise */
   result = 0;

/* Get a pointer to the XmlChan. */
   this = (AstXmlChan *) data;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* If a NULL pointer is supplied free any memory holding text already
   read from the source, and return zero. */
   if( !data ) {
      getnextchar_buf = astFree( getnextchar_buf );
      return 0;
   }

/* Check the global status */
   if( !astOK ) return result;

/* We read a new line from the source if: 1) the reset flag is set in the
   XmlChan, 2) we have reached the terminating null in the previous line,
   or 3) we do not yet have a line of text. */
   if( this->reset_source || *getnextchar_c == 0 || !getnextchar_buf ) {
      this->reset_source = 0;

/* Free the memory used to hold any previous text. */
      if( getnextchar_buf ) getnextchar_buf = astFree( getnextchar_buf );

/* Read a new line of text from the source. */
      getnextchar_buf = astGetNextText( this );

/* Read a new line if the previous line was empty. */
      while( getnextchar_buf && !getnextchar_buf[ 0 ] ) {
         astFree( getnextchar_buf );
         getnextchar_buf = astGetNextText( this );
      }

/* Reset the pointer to the next character to the start of the new
   string. */
      getnextchar_c = getnextchar_buf;

/* If all has gone OK, return the first character and then increment getnextchar_c to
   point to the next character. */
      if( getnextchar_c && astOK ) result = *(getnextchar_c++);

/* If we are reading a previously read line, return the character located
   by getnextchar_c and increment getnextchar_c. */
   } else {
      result = *(getnextchar_c++);
   }

/* Return the result */
   return result;

}

static const char *GetTag( AstXmlObject *this, int opening, int *status ){
/*
*  Name:
*     GetTag

*  Purpose:
*     Returns a string holding an XML tag describing the given XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *GetTag( AstXmlObject *this, int opening, int *status )

*  Description:
*     This function returns a pointer to static string
*     containing an XML tag describing the given XmlObject. It is a
*     wrapper for the astXmlGetTag function defined in xml.h, but it
*     additionally removes any "definedby" attribute before formating the
*     tag (the "definedby" attribute is added by the ReadClassData
*     function and is not part of the XML text read from the source).

*  Parameters:
*     this
*        Pointer to the XmlObject.
*     opening
*        Indicates which tag is to be returned; the start tag or the end
*        tag. If non-zero the start tag is returned. Otherwise, the
*        end tag is returned. If the supplied XmlObject has no end
*        tag (i.e. if it is an empty element, or if it is not an element),
*        then NULL is returned but no error is reported.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a null terminated static string holding the tag.

*  Notes:
*     - Empty elements are represented as an start tag of the form <.../>,
*     with no corresponding end tag.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   AstXmlElement *elem;      /* Pointer to XML element */
   const char *result;       /* The returned pointer */
   const char *ptr;          /* The value of the "definedby" attribute */
   const char *class;        /* Copy of the value of the "definedby" attribute */

/* Initialise */
   result = NULL;

/* If the object is an element, check for the "definedby" attribute. */
   if( astXmlCheckType( this, AST__XMLELEM ) ) {
      elem = (AstXmlElement *) this;

/* See if the element contains a "definedby" attribute. */
      ptr = astXmlGetAttributeValue( elem, DEFINEDBY );

/* If so, temporarily remove it, format the tag and then put it back. */
      if( ptr ) {
         class = astStore( NULL, ptr, strlen( ptr ) + 1 );
         astXmlRemoveAttr( elem, DEFINEDBY, NULL );
         result = astXmlGetTag( elem, opening );
         astXmlAddAttr( elem, DEFINEDBY, class, NULL );
         class = astFree( (void *) class );

/* If not, just use astXmlGetTag. */
      } else {
         result = astXmlGetTag( this, opening );
      }

/* If the object is not an element, just use astXmlGetTag. */
   } else {
      result = astXmlGetTag( this, opening );
   }

/* Return the result. */
   return result;
}

static AstRegion *IntersectionReader( AstXmlChan *this, AstXmlElement *elem,
                                      AstFrame *frm, int *status ){
/*
*  Name:
*     IntersectionReader

*  Purpose:
*     Make an AST Region from an IVOA Intersection region element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *IntersectionReader( AstXmlChan *this, AstXmlElement *elem,
*                                    AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Intersection region element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Intersection region element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   AstRegion *reg;               /* Pointer to component Region */
   AstRegion *tmp;               /* Pointer to new Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   int i;                        /* Loop count */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for a Region sub-element. */
   names[ 0 ] = "Intersection|Union|Negation|AllSky|Circle|Ellipse|Polygon|"
                "Convex|Box";
   min[ 0 ] = 2;
   max[ 0 ] = INT_MAX;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create Regions from all the component region elements, and combine
   them into nested CmpRegions, using the boolean AND operator to combine
   them. */
      new = RegionReader( this, scan->el[0][0], frm, status );
      for( i = 1; i < scan->count[0]; i++ ) {
         reg = RegionReader( this, scan->el[0][i], frm, status );
         tmp = (AstRegion *) astCmpRegion( new, reg, AST__AND, "", status );
         reg = astAnnul( reg );
         (void) astAnnul( new );
         new = tmp;
      }

/* Get any fill factor from the element and assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static int IsUsable( AstXmlElement *elem, int *status ){
/*
*  Name:
*     IsUsable

*  Purpose:
*     See if an XmlElement could describe an AST object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int IsUsable( AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function checks if an instance of an AST class could be
*     created from the supplied XmlElement.

*  Parameters:
*     elem
*        A pointer to the XmlElement, or NULL.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If an AST Object could be created from the supplied element, +1 is
*     returned. Otherwise, -1 is returned. Zero is returned if the supplied
*     pointer is NULL.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   const char *class;            /* Pointer to element name */
   const char *uri;              /* Pointer to namespace URI */
   IVOAReader reader;            /* Pointer to reader function */
   int is_ivoa;                  /* Element belongs to an IVOA namespace? */
   int oldrep;                   /* Original value of the Reporting flag */
   int result;                   /* Result value to be returned */

/* Check the global error status, and the supplied pointer. */
   if ( !astOK || !elem ) return 0;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Initialise */
   result = -1;

/* See if the element is in a supported IVOA namespace, and has a reader
   function for converting it to an AST object. If so, set the default
   XmlFormat to IVOA, and set the result non-zero. */
   reader = FindIVOAClass( elem, &is_ivoa, status );
   if( is_ivoa ){
      if( reader ) result = 1;
      if( isusable_this ) isusable_this->formatdef = IVOA_FORMAT;
   }

/* If the element is not an IVOA class, only proceed if the URI is not
   defined, or if it the AST URI. */
   uri = astXmlGetURI( elem );
   if( result == -1 && ( !uri || !strcmp( uri, AST__XMLNS ) ) ) {

/* Get the element name. This will be an AST class name if the element
   describes an AST Object. */
      class = astXmlGetName( elem );

/* Attempt to get the loader for a class of this name. If no loader exists an
   error would normally be reported. Therefore we switch off error reporting
   before making this call. After the class we clear any error status and
   switch error reporting back on. If no error occurs whilst getting the
   loader, then the class name must be a valid AST class name and so return
   a non-zero result value. */
      if( astOK ) {
         oldrep = astReporting( 0 );
         astGetLoader( class, status );
         if( astOK ) {
            result = 1;
         } else {
            astClearStatus;
         }
         astReporting( oldrep );
      }

/* If the element is in no namespace, use the AST URI as the default
   namespace for it and its children. */
      if( !uri ) astXmlAddURI( elem, NULL, AST__XMLNS );

   }

/* Return the result. */
   return result;
}

static AstObject *MakeAstFromXml( AstXmlChan *this, AstXmlElement *elem, int *status ) {
/*
*  Name:
*     MakeAstFromXml

*  Purpose:
*     Make an AST Object from an XML element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *MakeAstFromXml( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied XML element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XML element containing a description of the AST
*        object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstLoaderType *loader;        /* Pointer to loader for Object */
   AstObject *new;               /* Pointer to returned Object */
   AstXmlParent *old_container;  /* Element from which items are being read */
   IVOAReader reader;            /* Pointer to reader function */
   const char *class;            /* Pointer to Object class name string */
   int is_ivoa;                  /* Element belongs to an IVOA namespace? */
   int i;                        /* Index of content item */
   int nitem;                    /* No. of items of content within element */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* First deal with IVOA format. */
/* ---------------------------- */
   if( astGetXmlFormat( this ) == IVOA_FORMAT ) {

/* Get a pointer to a function which will produce an AST object from
   elements of the class of the supplied element. */
      reader = FindIVOAClass( elem, &is_ivoa, status );

/* If found, invoke the function to create the new AST object. */
      if( is_ivoa && reader ) {
         new = ( *reader )( this, elem, status );

/* IVOA reader functions do not remove used content as they are read
   from the element (unlike AST native readers). Therefore empty the
   element of all content now to indicate that the element contained no
   unrecognised content. This prevents an error being reported. If there
   was in fact any unrecognised content, then an error will already have
   been reported. */
         nitem = astXmlGetNitem( elem );
         for( i = nitem - 1; i >= 0; i-- ) astXmlDelete( astXmlGetItem( elem, i ) );

/* If not found, report an error. This should not happen since the IsUsable
   function should already have checked that the element is usable. */
      } else if( astOK ){
         astError( AST__INTER, "astRead(XmlChan): MakeAstFromIVOA does not "
                   "support IVOA class \"%s\" (internal AST programming error).", status,
                   astXmlGetName( elem ) );
      }

/* Now deal with other (i.e. NATIVE and QUOTED) format. */
/* ---------------------------------------------------- */
   } else {

/* Get the AST class name. This is the name of the XML element. */
      class = astXmlGetName( elem );

/* Use the associated class name to locate the loader for that
   class. This function will then be used to build the Object. */
      loader = astGetLoader( class, status );

/* If OK, save the pointer to the current container element, and indicate
   that the supplied element is now to be used as the current container.
   The "current container" is the XML element from which values are being
   read. */
      if( astOK ) {
         old_container = this->container;
         this->container = (AstXmlParent *) elem;

/* The "isa_class" item in the XmlChan structure contains a pointer to
   the name of the class whose loader is currently being invoked. It is set
   by the loader itself as a side effect of calling the astReadClassData
   function. Initialise it to NULL to indicate that astReadClassData has
   not yet been called. */
         this->isa_class = NULL;

/* Invoke the loader, which reads the Object definition from the
   current XML container (i.e. the supplied XML element) and builds the
   Object. Supply NULL/zero values to the loader so that it will substitute
   values appropriate to its own class. */
         new = (*loader)( NULL, (size_t) 0, NULL, NULL, (AstChannel *)
                          this, status );

/* Re-instate the original container. */
         this->container = old_container;
      }
   }

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* Return the pointer to the new Object. */
   return new;
}

static double MakeMJD( AstTimeFrame *frm, double time, int *status ) {
/*
*  Name:
*     MakeMJD

*  Purpose:
*     Create an MJD value from a TimeFrame axis value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double MakeMJD( AstTimeFrame *frm, double time, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function converts the supplied time value from the system
*     represented by the supplied TimeFrame into an absolute TBD MJD,
*     in units of days.

*  Parameters:
*     frm
*        Pointer to the TimeFrame defining the system in which "time" is
*        supplied.
*     time
*        The time value to convert.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The equivalent MJD value.

*/

/* Local Variables: */
   AstFrameSet *fs;
   AstTimeFrame *cfrm;
   double result;

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Create a copy of the supplied TimeFrame, and set its attributes to
   describe the required MJD system. */
   cfrm = astCopy( frm );
   astSetSystem( cfrm, AST__MJD );
   astSetUnit( cfrm, 0, "d" );
   astSetTimeScale( cfrm, AST__TDB );
   astSetTimeOrigin( cfrm, 0.0 );

/* Find the Mapping from the supplied TimeFrame to this TimeFrame. Use it to
   transform the supplied time value */
   fs = astConvert( frm, cfrm, "" );
   if( fs ) {
      astTran1( fs, 1, &time, 1, &result );

/* Free resources */
      fs = astAnnul( fs );
   }
   cfrm = astAnnul( cfrm );

/* Result */
   return result;

}

static AstXmlElement *MakePos2D( AstXmlChan *this, AstXmlElement *elem, int *status ){
/*
*  Name:
*     MakePos2D

*  Purpose:
*     Create an STC Position2D element from the supplied Position3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *MakePos2D(  AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function returns a pointer to a Position2D element by throwing
*     away the last axis in the supplied Position3D element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the Position3D element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Position2D element.


*/

/* Local Variables: */
   AstXmlElement *el;            /* Pointer to subelement */
   AstXmlElement *new;           /* Pointer to returned XmlElement */
   IVOAScan *scan;               /* Structure holding scan results */
   char **words;                 /* Array of words read from string */
   char *unit2;                  /* New Unit string */
   char buff[100];               /* Text buffer */
   const char *names[3];         /* Names of the subelements to be searched for */
   const char *unit;             /* Unit string */
   double pos[3];                /* Values read from Position3D */
   int i;                        /* Loop count */
   int l1;                       /* Length of word 1 */
   int l2;                       /* Length of word 2 */
   int max[3];                   /* Max allowed occurrences of each name */
   int min[3];                   /* Min allowed occurrences of each name */
   int n;                        /* Number of words read from string */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If the supplied element is not a Position3D just copy it. */
   if( strcmp( astXmlGetName( elem ), "Position3D" ) ) {
      new = (AstXmlElement *) astXmlCopy( elem );

/* Otherwise, we create a new Position2D and add required content to it. */
   } else {

/* Search the supplied element for the required sub-elements. */
      names[ 0 ] = "Name";
      names[ 1 ] = "Error3";
      names[ 2 ] = "Value3";
      max[ 0 ] = 1;
      max[ 1 ] = 2;
      max[ 2 ] = 1;
      min[ 0 ] = 1;
      min[ 1 ] = 0;
      min[ 2 ] = 0;
      scan = ScanIVOAElement( this, elem, 3, names, min, max, status );

/* If succesfull.. */
      if( scan ) {

/* Create an empty XML element with name "Position2D". */
         new = astXmlAddElement( NULL, "Position2D", NULL );

/* Get the units attribute from the supplied element. These are the units
   of the positional axis values. Copy the first 2 words to the unit
   attribute of the new element. */
         unit = astXmlGetAttributeValue( elem, "unit" );
         if( unit ) {
            words = astChrSplit( unit, &n );
            if( words ) {
               if( n > 2 ) {
                  l1 = strlen( words[ 0 ] );
                  l2 = strlen( words[ 1 ] );
                  unit2 = astMalloc( l1 + l2 + 2 );
                  if( unit2 ) {
                     strcpy( unit2, words[ 0 ] );
                     unit2[ l1 ] = ' ';
                     strcpy( unit2 + l1 + 1, words[ 1 ] );
                     unit2[ l1 + l2 + 1 ] = 0;
                     astXmlAddAttr( new, "unit", unit2, NULL );
                     unit2 = astFree( unit2 );
                  }
               } else {
                  astXmlAddAttr( new, "unit", unit, NULL );
               }

               if( words ) {
                  for( i = 0; i < n; i++ ) words[ i ] = astFree( words[ i ] );
                  words = astFree( words );
               }
            }
         }

/* If this Position3D contains a Name which can be read, obtain it
   and store it in the returned Position2D. */
         if( scan->count[ 0 ] > 0 ) {
            el = astXmlAddElement( new, "Name", NULL );
            astXmlAddCharData( el, 0, astXmlGetValue( scan->el[ 0 ][ 0 ], 0 ) );
         }

/* If this Position3D contains a Value which can be read, obtain it,
   format the first 2 values and store in the returned Position2D. */
         if( scan->count[ 2 ] > 0 ) {
            ElemListD( this, scan->el[ 2 ][ 0 ], 3, pos, status );
            el = astXmlAddElement( new, "Value2", NULL );
            sprintf( buff, "%.*g %.*g", DBL_DIG, pos[0], DBL_DIG, pos[1] );
            astXmlAddCharData( el, 0, buff );
         }

/* If this Position3D contains an Error which can be read, obtain it,
   format the first 2 values and store in the returned Position2D. */
         if( scan->count[ 1 ] > 0 ) {
            ElemListD( this, scan->el[ 1 ][ 0 ], 3, pos, status );
            el = astXmlAddElement( new, "Error2", NULL );
            sprintf( buff, "%.*g %.*g", DBL_DIG, pos[0], DBL_DIG, pos[1] );
            astXmlAddCharData( el, 0, buff );
         }

/* Free resources */
         scan = FreeIVOAScan( scan, status );
      }
   }

/* Return the result.*/
   return new;

}

static AstRegion *NegationReader( AstXmlChan *this, AstXmlElement *elem,
                                  AstFrame *frm, int *status ){
/*
*  Name:
*     NegationReader

*  Purpose:
*     Make an AST Region from an IVOA Negation region element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *NegationReader( AstXmlChan *this, AstXmlElement *elem,
*                                AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Negation region element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Negation region element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for a Region sub-element. */
   names[ 0 ] = "Intersection|Union|Negation|AllSky|Circle|Ellipse|Polygon|"
                "Convex|Box";
   min[ 0 ] = 1;
   max[ 0 ] = 1;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a Region from the component region element, and negate it. */
      new = RegionReader( this, scan->el[0][0], frm, status );
      astNegate( new );

/* Get any fill factor from the element and assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstObject *ObsDataLocationReader( AstXmlChan *this,
                                         AstXmlElement *elem, int *status ) {
/*
*  Name:
*     ObsDataLocationReader

*  Purpose:
*     Make an AST Object from an IVOA ObsDataLocationReader element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *ObsDataLocationReader( AstXmlChan *this,
*                                       AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     ObsDataLocationReader element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA ObsDataLocationReader element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstPointList *obs;            /* PointList defining the observatory position */
   AstStcObsDataLocation *stc;   /* Pointer to returned Object */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[2];         /* Names of the subelements to be searched for */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   stc = NULL;

/* Check the global error status. */
   if ( !astOK ) return (AstObject *) stc;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "ObservatoryLocation";
   names[ 1 ] = "ObservationLocation";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Read the observation location. */
      stc = (AstStcObsDataLocation *) StcMetadataReader( this, scan->el[ 1 ][ 0 ], status );

/* Read the observatory location, returning a Pointlist describing the
   observatory position (if possible), and modifiying the observation
   Region by (if possible) assigning the observatory location to the
   ObsLon and ObsLat attributes of any SpecFrames in the Region, and the
   ObsLon and ObsLat attributes of any TimeFrames in the Region. */
      obs = ObservatoryLocationReader( this, scan->el[ 0 ][ 0 ], stc, status );
      if( obs ) {
         astStcSetObs( stc, obs );
         obs = astAnnul( obs );
      }

/* Free resources. */
      scan = FreeIVOAScan( scan, status );
   }

/* Return the pointer to the new Object. */
   return (AstObject *) stc;
}

static AstPointList *ObservatoryLocationReader( AstXmlChan *this,
                                                AstXmlElement *elem,
                                                AstStcObsDataLocation *obs, int *status ){
/*
*  Name:
*     ObservatoryLocationReader

*  Purpose:
*     Make an AST PointList from an IVOA ObservatoryLocationReader element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstPointList *ObservatoryLocationReader( AstXmlChan *this,
*                                              AstXmlElement *elem,
*                                              AstStcObsDataLocation *obs, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST PointList from the supplied IVOA
*     ObservatoryLocationReader element, and also modifies the supplied
*     StcObsDataLocation so that the ObsLon and ObsLat attributes hold
*     the observatory position (if appropriate).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA ObservatoryLocation element.
*     obs
*        Pointer to the StcObsDataLocation in which to store the
*        observatory position (if terrestrial).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new PointList.

*/

/* Local Constants: */
#define A0  6378140.0            /* Earth equatorial radius (metres) */

/* Local Variables: */
   AstFrame *frm;                /* Pointer to obsvtory lon/lat Frame */
   AstFrame *obs_frm;            /* Pointer to obsvation lon/lat Frame */
   AstFrame *pfrm;               /* Pointer to axis primary Frame */
   AstKeyMap *km;                /* KeyMap holding AstroCoords info */
   AstObject *new;               /* Pointer to returned Region */
   AstObject *o;                 /* Pointer to retrieved Region */
   AstPointSet *ps;              /* Pointer to PointSet holding obs lon/lat */
   AstRegion *err;               /* Pointer to error Region */
   AstStc *stc;                  /* Pointer to Observatory location stc */
   char setting[ 100 ];          /* Attribute setting string */
   const char *dom;              /* Domain string */
   double **ptr;                 /* Pointers to axis values for obs lon/lat */
   double lambda;                /* Geodetic longitude radians */
   double phi;                   /* Geodetic latitude radians */
   double lon;                   /* Geocentric longitude radians */
   double lat;                   /* Geocentric latitude radians */
   int i;                        /* Index of Frame axis */
   int nax;                      /* Number of Frame axes */
   int paxis;                    /* Index of primary Frame axis */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise */
   new = NULL;

/* Read the ObservatoryLocation as an StcMetadata element (it will be
   represented by a NullRegion). */
   stc = (AstStc *) StcMetadataReader( this, elem, status );

/* Extract the first AstroCoords KeyMap from the stc. */
   if( astGetStcNCoord( stc ) == 0 ) {
      Report( this, elem, FAILURE, "contains no observatory position", status );
   } else {
      km = astGetStcCoord( stc, 1 );

/* Extract the PointList holding the axis values from the KeyMap. */
      if( !astMapGet0A( km, AST__STCVALUE, &new ) ){
         Report( this, elem, FAILURE, "contains no observatory position", status );

/* Extract any position uncertainty, and store as the uncertainty in the
   value PointList. */
      } else if( astMapGet0A( km, AST__STCERROR, &o ) ){
         err = (AstRegion *) o;
         astSetUnc( new, err );

/* Free resources */
         err = astAnnul( err );
      }
      km = astAnnul( km );
   }
   stc = astAnnul( stc );

/* Check the Region is a PointList. */
   if( !astIsAPointList( new ) && astOK ) {
      astError( AST__INTER, "ObservatoryLocationReader(XmlChan): The "
                "observatory location is described by a %s rather than "
                "a PointList (internal AST programming error).", status,
                astGetClass( new ) );
   }

/* If possible, we use the observatory location to set the value of the
   ObsLon and ObsLat attributes of any SpecFrames, and the ObsLon and
   ObsLat attributes of any TimeFrames, in the supplied ObsDataLocation.
   For this to be possible, the PointList being returned must represent
   either geodetic or geocentric longitude/latitude. If it is geocentric,
   the values need to be converted to geodetic. */
   ps = astRegTransform( new, NULL, 1, NULL, &frm );
   ptr = astGetPoints( ps );
   if( ptr ){
      nax = astGetNaxes( frm );
      lon = AST__BAD;
      lat = AST__BAD;
      lambda = AST__BAD;
      phi = AST__BAD;
      for( i = 0; i < nax; i++ ) {
         astPrimaryFrame( frm, i, &pfrm, &paxis );
         dom = astGetDomain( pfrm );
         if( dom ) {
            if( !strcmp( dom, "GEO_C" ) ){
               if( lon == AST__BAD ) {
                  lon = ptr[i][0];
                  astSetLabel( pfrm, 0, "Geodetic longitude" );
               } else {
                  lat = ptr[i][0];
                  astSetLabel( pfrm, 1, "Geodetic latitude" );
                  astSetDomain( pfrm, "GEO_D" );
               }

            } else if( !strcmp( dom, "GEO_D" ) ){
               if( lambda == AST__BAD ) {
                  lambda = ptr[i][0];
               } else {
                  phi = ptr[i][0];
               }
            }
         }
         pfrm = astAnnul( pfrm );
      }

/* If required, convert from geocentric lon/lat to geodetic lon/lat. */
      if( lon != AST__BAD ) {
         double pos[ 3 ], height;

/* Convert the supplied geocentric lon/lat to terrestrial Cartesian
   (x,y,z) values. The (x,y,z) system has origin at the centre of the
   earth, Z axis going through the north pole, X axis at (long,lat)=(0,0),
   and Y axis at (long,lat) = (E90,0). Assume an equatorial sea level
   position. */
         palDcs2c( lon, lat, pos );
         pos[ 0 ] *= A0;
         pos[ 1 ] *= A0;
         pos[ 2 ] *= A0;

/* Get the corresponding geodetic lon/lat. */
         iauGc2gd( 1, pos, &lambda, &phi, &height );
      }

      if( lambda != AST__BAD ) {
         obs_frm = astGetFrame( ((AstRegion *) obs)->frameset, AST__CURRENT );
         nax = astGetNaxes( obs );
         for( i = 0; i < nax; i++ ) {
            astPrimaryFrame( obs_frm, i, &pfrm, &paxis );
            if( astIsASpecFrame( pfrm ) ) {
               sprintf( setting, "ObsLon(%d)=%.*g", i + 1, DBL_DIG, lambda*AST__DR2D );
               astRegSetAttrib( obs, setting, NULL );
               sprintf( setting, "ObsLat(%d)=%.*g", i + 1, DBL_DIG, phi*AST__DR2D );
               astRegSetAttrib( obs, setting, NULL );
            } else if( astIsATimeFrame( pfrm ) ) {
               sprintf( setting, "ObsLon(%d)=%.*g", i + 1, DBL_DIG, lambda*AST__DR2D );
               astRegSetAttrib( obs, setting, NULL );
               sprintf( setting, "ObsLat(%d)=%.*g", i + 1, DBL_DIG, phi*AST__DR2D );
               astRegSetAttrib( obs, setting, NULL );
            }
            pfrm = astAnnul( pfrm );
         }
         obs_frm = astAnnul( obs_frm );
      }
   }

/* Free resources */
   frm = astAnnul( frm );
   ps = astAnnul( ps );

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return (AstPointList *) new;
}
#undef A0

static void OutputText( AstXmlChan *this, const char *text, int mxlen, int *status ) {
/*
*  Name:
*     OutputText

*  Purpose:
*     Write a string to the sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void OutputText( AstXmlChan *this, const char *text, int mxlen, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function writes the supplied text to the output sink,
*     splitting it into lines of no more than "mxlen" characters, if
*     required.

*  Parameters:
*     this
*        A pointer to the XmlChan.
*     text
*        Pointer to the (potentially long) null terminated string to write
*        out to the sink.
*     mxlen
*        The maximum allowed output line length. If zero, no limit is
*        placed on the output line length and the supplied text is always
*        written out as a single string.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   char *breakat;                /* Pointer to terminating character */
   char *c;                      /* Pointer to start of next chunk */
   char *lastend;                /* Pointer to last closing quote */
   char *lastspace;              /* Pointer to last whitespace character */
   char *linestart;              /* Pointer to start of current line */
   char quote;                   /* Opening quote character */
   char tt;                      /* Character temporarily replaced by 0 */
   int len;                      /* Length of current line */

/* Check the global error status. */
   if ( !astOK ) return;

/* If "mxlen" is zero, just output the string as supplied. */
   if( mxlen < 1 ) {
      astPutNextText( this, text );

/* Otherwise, output the string split up into lines */
   } else {
      c = (char *) text - 1;
      quote = 0;
      lastend = NULL;
      lastspace = NULL;
      len = 0;
      linestart = (char *) text;

/* Loop round each character in the text */
      while( *(++c ) ){

/* Note if we are currently inside a quoted string. Remember the quote
   character (" or ') so that we can look out for the corresponding
   closing quote. Note the position of the previous quote end. */
         if( !quote ) {
            if( *c == '\"' || *c == '\'' ) quote = *c;
         } else {
            if( *c == quote ) {
               quote = 0;
               lastend = c;
            }
         }

/* Note the position of hte previous space. */
         if( isspace( *c ) ) lastspace = c;

/* If we have exceeded the maximum allowed line length, split it. If we
   are inside a quote, we split it at the last quote end (if any). If we
   are not in a quote, we split it at the last space, or the last quote
   end (which ever is closest). To cover the case where the end quote is
   first character beyoind the limit, reduce the limit a bit. */
         len++;
         if( len >= mxlen - 2 ) {
            if( !quote || !lastend ) {
               if( lastend && lastspace ){
                  breakat = ( lastend > lastspace ) ? lastend + 1: lastspace;
               } else if( lastend ){
                  breakat = lastend + 1;
               } else if( lastspace ){
                  breakat = lastspace;
               } else {
                  breakat = c;
               }
            } else {
               breakat = lastend + 1;
            }
         } else {
            breakat = NULL;
         }

/* If we have a line break, output the current line. */
         if( breakat ) {

/* Terminate the string, first saving the character which is replaced by the
   terminating zero so that it can be re-instated later. */
            tt = *breakat;
            *breakat = 0;

/* Write out the newly terminated chunk. */
            astPutNextText( this, linestart );

/* Move on to ths start of the next chunk, decrement the number of characters
   remaining, and re-instate the character previously over-written by
   zero. */
            c = breakat;
            linestart = c;
            *c = tt;
            len = 0;
            quote = 0;
         }
      }

/* Write out any remaining text (this will be less than "mxlen"
   characters long)*/
      if( linestart && *linestart ) astPutNextText( this, linestart );
   }
}

static AstRegion *PolygonReader( AstXmlChan *this, AstXmlElement *elem,
                                 AstFrame *frm, int *status ){
/*
*  Name:
*     PolygonReader

*  Purpose:
*     Make an AST Region from an IVOA Polygon element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *PolygonReader( AstXmlChan *this, AstXmlElement *elem,
*                               AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Polygon element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Polygon element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;
   AstMapping *map;
   AstRegion *new;
   IVOAScan *scan;
   const char *names[1];
   const char *unit;
   const char *funit;
   double *pos;
   double *x0;
   double *y0;
   double lbnd[2];
   double ubnd[2];
   int axcon;
   int axlon;
   int i;
   int is_box;
   int is_sky;
   int laxcon;
   int max[1];
   int min[1];
   int nv;
   int small[ 4 ];

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Vertex";
   min[ 0 ] = 1;
   max[ 0 ] = INT_MAX;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* See if the Frame is a SkyFrame, and if so, get the index of the
   longitude axis. */
      is_sky = astIsASkyFrame( frm );
      axlon = is_sky ? astGetLonAxis( (AstSkyFrame *) frm ) : -1;

/* Get the units attribute from the supplied element. These are the units
   of the vertex axis values. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Create an array to hold the axis values at the vertices. */
      nv = scan->count[0];
      pos = astMalloc( sizeof( double )*(size_t) (2*nv) );

/* Read each vertex element in turn. Record whether or not the first 4
   vertices have <SmallCircles>. */
      x0 = pos;
      y0 = x0 + nv;
      for( i = 0; i < nv; i++, x0++, y0++ ) {
         small [ i % 4 ] = VertexReader( this, scan->el[0][i], x0, y0, status );
      }

/* Reset the pointers so that they point to the first x and y values. */
      x0 = pos;
      y0 = x0 + nv;

/* Since the SkyFrame class does not have active Units we must handle it
   separately. Convert the axis values from the supplied units (e.g.
   degrees) to radians. */
      if( is_sky ) {
         map = astUnitMapper( unit, "rad", NULL, NULL );
         if( map ) {
            astTran1( map, nv*2, pos, 1, pos );
            map = astAnnul( map );
         } else if( astOK ) {
            Report( this, elem, FAILURE, "contains unusable units", status );
         }
      }

/* If there are 4 vertices we may be able to create an AST Box (not the
   same as an STC Box) instead of a Polygon.*/
      is_box = 0;
      ubnd[0] = x0[ 0 ];
      lbnd[0] = ubnd[0];
      ubnd[1] = y0[ 0 ];
      lbnd[1] = ubnd[1];

      if( nv == 4 ) {

/* See if the edge which ends at the 1st vertex has a constant value on
   either axis. Is so, note the index of the axis which is held constant. */
         is_box = 1;
         if( is_sky && small[ 0 ] ) {
            laxcon = 1 - axlon;

         } else if( astEQUAL( x0[ 0 ], x0[ 3 ] ) ) {
            laxcon = 0;

         } else if( astEQUAL( y0[ 0 ], y0[ 3 ] ) ) {
            laxcon = 1;

         } else {
            is_box = 0;
         }

/* If the first edge represents a constant axis value, see if the others
   do too (ensuring that the axes which are held constant alternate). Also
   find the bounds of the box.*/
         if( is_box ) {
            for( i = 1; i < 4; i++ ) {
               if( is_sky && small[ i ] ) {
                  axcon = 1 - axlon;

               } else if( astEQUAL( x0[ i ], x0[ i - 1 ] ) ) {
                  axcon = 0;

               } else if( astEQUAL( y0[ i ], y0[ i - 1 ] ) ) {
                  axcon = 1;

               } else {
                  is_box = 0;
                  break;
               }

               if( axcon != 1 - laxcon ) {
                  is_box = 0;
                  break;
               }

               if( x0[ i ] > ubnd[0] ) {
                  ubnd[0] = x0[ i ];

               } else if( x0[ i ] < lbnd[0] ) {
                  lbnd[0] = x0[ i ];
               }

               if( y0[ i ] > ubnd[1] ) {
                  ubnd[1] = y0[ i ];

               } else if( y0[ i ] < lbnd[1] ) {
                  lbnd[1] = y0[ i ];
               }

               laxcon = axcon;
            }
         }
      }

/* Since the SkyFrame class does not have active Units we must handle it
   separately. The axis values have already been converted from the supplied
   units (e.g. degrees) to radians. */
      if( is_sky ) {

/* Create the Polygon or Box within the SkyFrame. */
         if( is_box ) {
            new = (AstRegion *) astBox( frm, 1, lbnd, ubnd, NULL, "", status );
         } else {
            new = (AstRegion *) astPolygon( frm, nv, nv, pos, NULL, "", status );
         }

/* Now handles Polygons in Frames other than SkyFrames. */
      } else {

/* Take a copy of the supplied Frame and set its Units to the value
   obtained from the supplied element. */
         cfrm = astCopy( frm );
         astSetUnit( cfrm, 0, unit );
         astSetUnit( cfrm, 1, unit );

/* Create the Polygon or Box within the SkyFrame. */
         if( is_box ) {
            new = (AstRegion *) astBox( cfrm, 1, lbnd, ubnd, NULL, "", status );
         } else {
            new = (AstRegion *) astPolygon( cfrm, nv, nv, pos, NULL, "", status );
         }

/* If the Unit of this Region differs from that of the supplied Frame,
   set it to the Unit of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Unit. If the supplied
   Frame had no set Unit, set it to the units obtained from the supplied
   element. */
         for( i = 0; i < 2; i++ ) {
            if( astTestUnit( frm, i ) ) {
               funit = astGetUnit( frm, i );
               if( strcmp( funit, unit ) ) astSetUnit( new, i, funit );
            } else {
               astSetUnit( frm, i, unit );
            }
         }

/* Free resources */
         cfrm = astAnnul( cfrm );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      pos = astFree( pos );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static double PosAngleReader( AstXmlChan *this, AstXmlElement *elem, int *status ){
/*
*  Name:
*     PosAngleReader

*  Purpose:
*     Read an AST position angle value from an IVOA PosAngle element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double PosAngleReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the supplied PosANgle element and returns a value
*     representing a position angle in the AST convention (radians from
*     +ve second axis to +ve first axis).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Ellipse element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The position angle.

*/

/* Local Variables: */
   char buff[ 200 ];             /* Message buffer */
   const char *paunit;           /* Position angle unit string */
   const char *ref;              /* Reference axis string */
   double result;                /* Position angle value */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Get the numerical position angle stored in the supplied PosAngle
   element. */
   result = ElemValueD( this, elem, 0.0, status );

/* Get the units attribute from the supplied element. These are the units
   of the above value. Default is degrees. */
   paunit = astXmlGetAttributeValue( elem, "unit" );

/* Convert the position angle to radians. */
   if( !paunit || !strcmp( paunit, "deg" ) ) {
      result *= AST__DD2R;

   } else if( !strcmp( paunit, "h" ) ) {
      result *= 15*AST__DD2R;

   } else if( !strcmp( paunit, "arcmin" ) ) {
      result *= AST__DD2R/60.0;

   } else if( !strcmp( paunit, "arcsec" ) ) {
      result *= AST__DD2R/3600.0;

   } else {
      sprintf( buff, "contains unusable angle units \"%s\"", paunit );
      Report( this, elem, FAILURE, buff, status );
   }

/* Get the reference attribute from the supplied element. This indicates
   the sense and origin of the stored angle value. Convert the result
   to the AST convention, which is the equivalent of "Y" (which is the same
   as "North"). "X" means "from X to Y", "Y" means "from Y to X". Default
   is "X". */
   ref = astXmlGetAttributeValue( elem, "reference" );
   if( !ref || !Ustrcmp( ref, "X", status ) ) {
      result = AST__DPIBY2 - result;

   } else if( Ustrcmp( ref, "Y", status ) && Ustrcmp( ref, "North", status ) ) {
      sprintf( buff, "contains unusable reference attribute \"%s\" "
               "(will assume \"Y\" instead)", ref );
      Report( this, elem, WARNING, buff, status );
   }

/* Return the result. */
   return result;
}

static AstRegion *Position2DReader( AstXmlChan *this, AstXmlElement *elem,
                                    AstFrame *frm, double *pos,
                                    AstKeyMap **anc, int *status ){
/*
*  Name:
*     Position2DReader

*  Purpose:
*     Modify a Frame to take account of an STC <Position2D> element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *Position2DReader( AstXmlChan *this, AstXmlElement *elem,
*                                  AstFrame *frm, double *pos, int axis,
*                                  AstKeyMap **anc, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the supplied STC <Position2D> element, and uses it,
*     if possible, to create the uncertainty associated with the spatial
*     axis in the supplied Frame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Position2D element.
*     frm
*        Pointer to the 2D spatial Frame.
*     pos
*        Pointer to an array in which to return the spatial axis values
*        represented by the supplied Position2D element. This array is
*        returned filled with AST__BAD If the supplied Position2D element
*        does not contain any axis values.
*     anc
*        Address of a location at which to put a pointer to a newly
*        created KeyMap. This KeyMap will contain ancillary information
*        from the Position2D. The keys identify the item of ancillary
*        information (Name, Value, Error, Resolution, Size, Pixel Size).
*        The value associated with the Name key is string containing
*        the Name item from the Position2D. The value
*        associated with each of the other keys is a pointer to a 2D Region
*        within the supplied Frame, corresponding to the value, error,
*        resolution, etc. Keys will not be present in the returned KeyMap
*        if the corresponding item of ancillary information is not present
*        in the Position2D. A NULL pointer is returned if there is no
*        ancillary information at all.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The uncertainty Region, or NULL if the supplied Position2D element
*     does not specify an uncertainty.

*/

/* Local Variables: */
   AstMapping *map1;        /* Mapping from first axis units to radians */
   AstMapping *map2;        /* Mapping from second axis units to radians */
   AstPointSet *pset;       /* Pointset holding Position2D axis values */
   AstRegion *r;            /* Region to store in ancillary KeyMap */
   AstRegion *result;       /* Returned uncertainty Region */
   IVOAScan *scan;          /* Structure holding scan results */
   char **words;            /* Array of words extracted from string */
   const char *name;        /* Pointer to XML element name */
   const char *names[6];    /* Names of the subelements to be searched for */
   const char *unit1;       /* Pointer to axis 1 unit attribute string */
   const char *unit2;       /* Pointer to axis 2 unit attribute string */
   const char *unit;        /* Pointer to Position2D's unit attribute string */
   double **ptr;            /* Arrays holding Position2D axis values */
   double cen[ 2 ];         /* Centre values */
   double hw[ 2 ];          /* Half widths values */
   double pa;               /* Error position angle */
   int i;                   /* Loop count */
   int max[6];              /* Max allowed occurrences of each name */
   int min[6];              /* Min allowed occurrences of each name */
   int nword;               /* Number of words extracted from string */

/* Initialise */
   result = NULL;
   pos[ 0 ] = AST__BAD;
   pos[ 1 ] = AST__BAD;
   *anc = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Name";
   names[ 1 ] = "Error2|Error2PA";
   names[ 2 ] = "Value2";
   names[ 3 ] = "Resolution2|Resolution2PA";
   names[ 4 ] = "Size2|Size2PA";
   names[ 5 ] = "PixSize2|PixSize2PA";
   max[ 0 ] = 1;
   max[ 1 ] = 2;
   max[ 2 ] = 1;
   max[ 3 ] = 2;
   max[ 4 ] = 2;
   max[ 5 ] = 2;
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   min[ 4 ] = 0;
   min[ 5 ] = 0;
   scan = ScanIVOAElement( this, elem, 6, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a KeyMap to return holding ancilary info, and put the Name into
   it. */
      *anc = astKeyMap( "", status );
      if( scan->count[0] > 0 ) astMapPut0C( *anc, AST__STCNAME,
                                  astXmlGetValue( scan->el[0][0], 0 ), NULL );

/* Get the units attribute from the supplied element. These are the units
   of the positional axis values. Split into units for each of the two
   axes. */
      unit1 = "";
      unit2 = "";
      words = NULL;
      unit = astXmlGetAttributeValue( elem, "unit" );

      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );

      } else {
         words = astChrSplit( unit, &nword );
         if( words ) {
            unit1 = words[ 0 ];
            unit2 = nword > 1 ? words[ 1 ] : words[ 0 ];
         }
      }

/* Since the SkyFrame class does not have active Units we must convert the
   axis values from the supplied units (e.g. degrees) to radians. Allow
   for different units on the two axes. */
      map1 = astUnitMapper( unit1, "rad", NULL, NULL );
      if( !map1 ) Report( this, elem, FAILURE, "contains unusable units for axis 1", status );

      if( unit1 && unit2 && strcmp( unit1, unit2 ) ) {
         map2 = astUnitMapper( unit2, "rad", NULL, NULL );
         if( !map2 ) Report( this, elem, FAILURE, "contains unusable units for axis 2", status );
      } else {
         map2 = astClone( map1 );
      }

/* If this Position2D contains a Value which can be read, obtain it. Otherwise,
   issue a warning. */
      if( scan->count[ 2 ] > 0 ) {
         ElemListD( this, scan->el[ 2 ][ 0 ], 2, pos, status );

/* Convert to radians. */
         if( map1 == map2 ) {
            astTran1( map1, 2, pos, 1, pos );
         } else {
            astTran1( map1, 1, pos, 1, pos );
            astTran1( map2, 1, pos + 1, 1, pos + 1 );
         }

/* If this Position2D contains a value which cannot be used, issue a warning. */
         if( pos[ 1 ] == AST__BAD ) {
            Report( this, elem, WARNING, "contains an unreadable <Value>", status );
         }

/* Create a PointList from it and store in the returned ancillary KeyMap. */
         pset = astPointSet( 1, 2, "", status );
         ptr = astGetPoints( pset );
         if( astOK ) {
            ptr[ 0 ][ 0 ] = pos[ 0 ];
            ptr[ 1 ][ 0 ] = pos[ 1 ];
            r = (AstRegion *) astPointList( frm, pset, NULL, "", status );
            astMapPut0A( *anc, AST__STCVALUE, r, NULL );
            r = astAnnul( r );
         }
         pset = astAnnul( pset );
      }

/* Does this Position2D contain any Error? */
      if( scan->count[ 1 ] > 0 && map1 ) {

/* Issue a warning if more than 1 Error value was found. */
         if( scan->count[ 1 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Error>"
                    " element. AST can only use the first", status );
         }

/* If the error has no position angle, just read it as a list of double.
   Otherwise, read the Error2PA structure. */
         name = astXmlGetName( scan->el[ 1 ][ 0 ] );
         if( name ) {
            if( !strcmp( name, "Error2" ) ) {
               ElemListD( this, scan->el[ 1 ][ 0 ], 2, hw, status );
               pa = AST__BAD;
            } else {
               pa = Error2PAReader( this, scan->el[ 1 ][ 0 ], hw, status );
            }

/* Convert to radians, and halve to get the half-width. */
            if( map1 == map2 ) {
               astTran1( map1, 2, hw, 1, hw );
            } else {
               astTran1( map1, 1, hw, 1, hw );
               astTran1( map2, 1, hw + 1, 1, hw + 1 );
            }

            if( hw[ 0 ] != AST__BAD ) hw[ 0 ] *= 0.5;
            if( hw[ 1 ] != AST__BAD ) hw[ 1 ] *= 0.5;

/* Create an Ellipse or Box to describe the error */
            cen[ 0 ] = 0.0;
            cen[ 1 ] = 0.0;
            if( pa != AST__BAD ) {
               result = (AstRegion *) astEllipse( frm, 1, cen, hw, &pa,
                                                  NULL, "", status );
            } else {
               result = (AstRegion *) astBox( frm, 0, cen, hw, NULL, "", status );
            }

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCERROR, result, NULL );
         }
      }

/* Does this Position2D contain any Resolution? */
      if( scan->count[ 3 ] > 0 && map1 ) {

/* Issue a warning if more than 1 Resolution value was found. */
         if( scan->count[ 3 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Resolution>"
                    " element. AST can only use the first", status );
         }

/* If the resolution has no position angle, just read it as a list of double.
   Otherwise, read the Resolution2PA structure (which is exactly the same
   as an Error2PA structure). */
         name = astXmlGetName( scan->el[ 3 ][ 0 ] );
         if( name ) {
            if( !strcmp( name, "Resolution2" ) ) {
               ElemListD( this, scan->el[ 3 ][ 0 ], 2, hw, status );
               pa = AST__BAD;
            } else {
               pa = Error2PAReader( this, scan->el[ 3 ][ 0 ], hw, status );
            }

/* Convert to radians, and halve to get the half-width. */
            if( map1 == map2 ) {
               astTran1( map1, 2, hw, 1, hw );
            } else {
               astTran1( map1, 1, hw, 1, hw );
               astTran1( map2, 1, hw + 1, 1, hw + 1 );
            }

            if( hw[ 0 ] != AST__BAD ) hw[ 0 ] *= 0.5;
            if( hw[ 1 ] != AST__BAD ) hw[ 1 ] *= 0.5;

/* Create an Ellipse or Box to describe the resolution */
            cen[ 0 ] = 0.0;
            cen[ 1 ] = 0.0;
            if( pa != AST__BAD ) {
               r = (AstRegion *) astEllipse( frm, 1, cen, hw, &pa,
                                                  NULL, "", status );
            } else {
               r = (AstRegion *) astBox( frm, 0, cen, hw, NULL, "", status );
            }

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCRES, r, NULL );
            r = astAnnul( r );
         }
      }

/* Does this Position2D contain any Size? */
      if( scan->count[ 4 ] > 0 && map1 ) {

/* Issue a warning if more than 1 Size value was found. */
         if( scan->count[ 4 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Size>"
                    " element. AST can only use the first", status );
         }

/* If the size has no position angle, just read it as a list of double.
   Otherwise, read the Size2PA structure (which is exactly the same
   as an Error2PA structure). */
         name = astXmlGetName( scan->el[ 4 ][ 0 ] );
         if( name ) {
            if( !strcmp( name, "Size2" ) ) {
               ElemListD( this, scan->el[ 4 ][ 0 ], 2, hw, status );
               pa = AST__BAD;
            } else {
               pa = Error2PAReader( this, scan->el[ 4 ][ 0 ], hw, status );
            }

/* Convert to radians, and halve to get the half-width. */
            if( map1 == map2 ) {
               astTran1( map1, 2, hw, 1, hw );
            } else {
               astTran1( map1, 1, hw, 1, hw );
               astTran1( map2, 1, hw + 1, 1, hw + 1 );
            }

            if( hw[ 0 ] != AST__BAD ) hw[ 0 ] *= 0.5;
            if( hw[ 1 ] != AST__BAD ) hw[ 1 ] *= 0.5;

/* Create an Ellipse or Box to describe the size */
            cen[ 0 ] = 0.0;
            cen[ 1 ] = 0.0;
            if( pa != AST__BAD ) {
               r = (AstRegion *) astEllipse( frm, 1, cen, hw, &pa,
                                                  NULL, "", status );
            } else {
               r = (AstRegion *) astBox( frm, 0, cen, hw, NULL, "", status );
            }

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCSIZE, r, NULL );
            r = astAnnul( r );
         }
      }

/* Does this Position2D contain any PixSize? */
      if( scan->count[ 5 ] > 0 && map1 ) {

/* Issue a warning if more than 1 PixSize value was found. */
         if( scan->count[ 5 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <PixSize>"
                    " element. AST can only use the first", status );
         }

/* If the pixsize has no position angle, just read it as a list of double.
   Otherwise, read the PixSize2PA structure (which is exactly the same
   as an Error2PA structure). */
         name = astXmlGetName( scan->el[ 5 ][ 0 ] );
         if( name ) {
            if( !strcmp( name, "PixSize2" ) ) {
               ElemListD( this, scan->el[ 5 ][ 0 ], 2, hw, status );
               pa = AST__BAD;
            } else {
               pa = Error2PAReader( this, scan->el[ 5 ][ 0 ], hw, status );
            }

/* Convert to radians, and halve to get the half-width. */
            if( map1 == map2 ) {
               astTran1( map1, 2, hw, 1, hw );
            } else {
               astTran1( map1, 1, hw, 1, hw );
               astTran1( map2, 1, hw + 1, 1, hw + 1 );
            }

            if( hw[ 0 ] != AST__BAD ) hw[ 0 ] *= 0.5;
            if( hw[ 1 ] != AST__BAD ) hw[ 1 ] *= 0.5;

/* Create an Ellipse or Box to describe the pixsize */
            cen[ 0 ] = 0.0;
            cen[ 1 ] = 0.0;
            if( pa != AST__BAD ) {
               r = (AstRegion *) astEllipse( frm, 1, cen, hw, &pa,
                                             NULL, "", status );
            } else {
               r = (AstRegion *) astBox( frm, 0, cen, hw, NULL, "", status );
            }

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCPIXSZ, r, NULL );
            r = astAnnul( r );
         }
      }

/* Free resources */
      if( map1 ) map1 = astAnnul( map1 );
      if( map2 ) map2 = astAnnul( map2 );
      scan = FreeIVOAScan( scan, status );
      if( words ) {
         for( i = 0; i < nword; i++ ) words[ i ] = astFree( words[ i ] );
         words = astFree( words );
      }

   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;

}

static AstRegion *PositionIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                          AstFrame *frm, int *status ){
/*
*  Name:
*     PositionIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA PositionInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *PositionIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                        AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     PositionInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA PositionInterval element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit or System attribute is not set, this
*        function will decide on the values to be used, and set these
*        values in the supplied Frame before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   const char *unit;             /* Unit string from supplied element */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */
   int ndim;                     /* Number of axes in supplied Frame */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-element. */
   ndim = astGetNaxes( frm );
   if( ndim == 1 ) {
      names[ 0 ] = "CoordScalarInterval";
   } else if( ndim == 2 ) {
      names[ 0 ] = "Coord2VecInterval";
   } else if( ndim == 3 ) {
      names[ 0 ] = "Coord3VecInterval";
   } else if( astOK ) {
      astError( AST__INTER, "PositionIntervalReader(XmlChan): Supplied "
                "Frame has more than 3 axes (internal AST programming error )." , status);
   }
   min[ 0 ] = 1;
   max[ 0 ] = 1;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the units attribute from the supplied element. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Read 1-d intervals */
      if( ndim == 1 ) {
         new = CoordScalarIntervalReader( this, scan->el[0][0], unit, frm, status );

/* Read 2-d intervals */
      } else if( ndim == 2 ) {
         new = Coord2VecIntervalReader( this, scan->el[0][0], unit, frm, status );

/* Read 3-d intervals */
      } else if( ndim == 3 ) {
         new = Coord3VecIntervalReader( this, scan->el[0][0], unit, frm, status );

/* Report error for other dimensionalities */
      } else if( astOK ) {
         astError( AST__INTER, "PositionIntervalReader(XmlChan): Supplied "
                   "Frame has more than 3 axes (internal AST programming error )." , status);
      }

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstObject *Read( AstChannel *this_channel, int *status ) {
/*
*  Name:
*     Read

*  Purpose:
*     Read an Object from a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *Read( AstChannel *this_channel, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the astRead method
*     inherited from the Channel class).

*  Description:
*     This function reads an Object from an XmlChan.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstObject *new;               /* Pointer to returned Object */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *elem;          /* XML element holding AST Object */
   int def_fmt;                  /* Original default format */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Save the current default format, and then reset it to NATIVE */
   def_fmt = this->formatdef;
   this->formatdef = NATIVE_FORMAT;

/* First we construct an in-memory XML representation of the data source,
   by reading text up to the end of the first element encountered from
   which an AST Object could be created. If the Skip attribute is zero, then
   an error is reported if there is any text prior to the start of the first
   usable element. If Skip is non-zero any initial text prior to the start
   of the first usable element is ignored. */
   elem = ReadXmlText( this, status );

/* Check a usable element was found. */
   if( elem ) {

/* The "current container element" is the XML element from which items
   are currently being read. Indicate that we are currently not reading
   any element (not used for IVOA formats). */
      this->container = NULL;

/* Next we create a new AST Object from this in-memory XML representation
   of the source. */
      new = MakeAstFromXml( this, elem, status );

/* Remove the element. This will cause an error to be reported if
   the element contains any items which have not been used. */
      elem = Remove( this, elem, status );
   }

/* If an error has occurred, annul the document. */
   if( !astOK ) this->readcontext = astXmlAnnul( this->readcontext );

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer, and re-instate original default format. */
   if ( !astOK ) {
      new = astDelete( new );
      this->formatdef = def_fmt;
   }

/* Return the pointer to the new Object. */
   return new;
}

static void ReadClassData( AstChannel *this_channel, const char *class, int *status ) {
/*
*  Name:
*     ReadClassData

*  Purpose:
*     Read values from a data source for a class loader.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void ReadClassData( AstChannel *this, const char *class, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the astReadClassData method
*     inherited from the Channel class).

*  Description:
*     This function reads the data for a class from the data source
*     associated with a Channel, so as to provide values for
*     initialising the instance variables of that class as part of
*     building a complete Object. This function should be invoked by
*     the loader for each class immediately before it attempts to read
*     these values.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        A pointer to a constant null-terminated string containing the
*        name of the class whose loader is requesting the data (note
*        this is not usually the same as the class name of the Object
*        being built). This value allows the class structure of the
*        input data to be validated.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlContentItem *item;      /* Pointer to next item of content */
   const char *definedby;        /* Class defining current content items */
   int nitem;                    /* Number of items in container */
   int i;                        /* Loop counter */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Check we have a container, and then store the name of the class being
   loaded. */
   if( !this->container ){
      astError( AST__INTER, "astRead(XmlChan): Invalid attempt to read "
                "%s data - there is currently no container element "
                "(internal AST programming error).", status, class );

   } else {
      this->isa_class = class;

/* Go through all the content elements within the current container and
   give them an extra attribute named "definedby" the value of which is
   the name of the class which defines the associated AST attribute or
   object. This is determined by the the "isa" elements - an element is
   "definedby" the class noted in the next following "isa" element, or by
   the class being loaded if there is no following "isa" element. */

/* Find the first "isa" element and get the value of its "class" attribute.
   If none is found the name of the class being loaded is used. */
      definedby = FindNextIsA( (AstXmlElement *) this->container, 0, status );

/* Loop round all elements within the container. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; astOK && i < nitem; i++ ) {
         item = astXmlGetItem( this->container, i );
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* If this is an "ISA" element, then we have ended the scope of the
   current "isa" class. All subsequent items will be defined by the class
   mentioned in the next following "ISA" element. Find the next ISA
   element and get its class. */
            if( astOK && !strcmp( astXmlGetName( item ), ISA ) ) {
               definedby = FindNextIsA( (AstXmlElement *) this->container, i + 1, status );

/* For other element types, add a "definedby" attribute holding the name
   of the class defined by the current ISA element. */
            } else {
               astXmlAddAttr( item, DEFINEDBY, definedby, NULL );
            }
         }
      }
   }
}

static double ReadDouble( AstChannel *this_channel, const char *name, double def, int *status ) {
/*
*  Name:
*     ReadDouble

*  Purpose:
*     Read a double value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double ReadDouble( AstChannel *this, const char *name, double def, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the astReadDouble method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a double value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a double
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of 0.0 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   const char *value;            /* Pointer to attribute value */
   double result;                /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */

/* Initialise. */
   result = 0.0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be
   found. */
   element = FindAttribute( this, name, status );

/* If an element was found, attempt to decode the string to give a double
   value, checking that the entire string is read (and checking for the
   magic string used to represent bad values). If this fails, then the
   wrong name has probably been given, or the input data are corrupt,
   so report an error. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         nc = 0;
         if ( ( 0 == astSscanf( value, " " BAD_STRING " %n",
                                                   &nc ) )
                 && ( nc >= (int) strlen( value ) ) ) {
            result = AST__BAD;

         } else if ( !( ( 1 == astSscanf( value, " %lf %n", &result, &nc ) )
                 && ( nc >= (int) strlen( value ) ) ) ) {
            astError( AST__BADIN, "astRead(XmlChan): The value \"%s = %s\" "
                      "cannot be read as a double precision floating point "
                      "number.", status, name, value );

/* If the value was succesfully read, remove the ATTR element
   from the container. */
         } else {
            element = Remove( this, element, status );
         }

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", status, name,
                   GetTag( (AstXmlObject *) element, 1, status ) );
      }

/* If no suitable element was found, then use the default value instead. */
   } else {
      result = def;
   }

/* Return the result. */
   return result;
}

static int ReadInt( AstChannel *this_channel, const char *name, int def, int *status ) {
/*
*  Name:
*     ReadInt

*  Purpose:
*     Read a int value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int ReadInt( AstChannel *this, const char *name, int def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadInt method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a int value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   const char *value;            /* Pointer to attribute value */
   int result;                   /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be
   found. */
   element = FindAttribute( this, name, status );

/* If an element was found, attempt to decode the string to give a int
   value, checking that the entire string is read. If this fails, then the
   wrong name has probably been given, or the input data are corrupt,
   so report an error. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         nc = 0;
         if ( !( ( 1 == astSscanf( value, " %d %n", &result, &nc ) )
                 && ( nc >= (int) strlen( value ) ) ) ) {
            astError( AST__BADIN,
                      "astRead(XmlChan): The value \"%s = %s\" cannot "
                      "be read as an integer.", status, name, value );

/* If the value was succesfully read, remove the ATTR element
   from the container. */
         } else {
            element = Remove( this, element, status );
         }

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", status, name,
                   GetTag( (AstXmlObject *) element, 1, status ) );
      }

/* If no suitable element was found, then use the default value instead. */
   } else {
      result = def;
   }

/* Return the result. */
   return result;
}

static AstObject *ReadObject( AstChannel *this_channel, const char *name,
                              AstObject *def, int *status ) {
/*
*  Name:
*     ReadObject

*  Purpose:
*     Read a (sub)Object as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char *ReadObject( AstChannel *this, const char *name, AstObject *def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadObject method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify an Object with a specified name. If such an Object
*     is found, a pointer to it is returned, otherwise a default pointer
*     is returned instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     A pointer to the Object, or a clone of the default pointer if
*     the Object was not found.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   AstObject *result;            /* Value to be returned */
   const char *isa_class;        /* Class currently being loaded */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an element with a name which
   is not ATTR and with the specified LABEL. This call ignores
   elements which represent default values. No error is reported if an
   element with the given label cannot be found. */
   element = FindObject( this, name, status );

/* If an element was found, make an AST object from it. First remember
   the class currently being loaded so that it can be re-instated. */
   if( element ) {
      isa_class = this->isa_class;
      result = MakeAstFromXml( this, element, status );
      this->isa_class = isa_class;

/* Remove the element from the container. */
      element = Remove( this, element, status );

/* If no suitable Value structure was found, clone the default
   pointer, if given. */
   } else if ( def ) {
      result = astClone( def );
   }

/* Return the result. */
   return result;
}

static char *ReadString( AstChannel *this_channel, const char *name, const char *def, int *status ) {
/*
*  Name:
*     ReadString

*  Purpose:
*     Read a string value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char *ReadString( AstChannel *this, const char *name, const char *def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadString method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a string value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     A pointer to a dynamically allocated null-terminated string
*     containing the value required, or to a copy of the default
*     string if the value was not found (or NULL if the "def" pointer
*     was NULL).

*  Notes:
*     - It is the caller's responsibility to arrange for the memory
*     holding the returned string to be freed (using astFree) when it
*     is no longer required.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   char *result;                 /* Value to be returned */
   const char *value;            /* Pointer to attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be
   found. */
   element = FindAttribute( this, name, status );

/* If an element was found, return a copy of the "value" string. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         result = astStore( NULL, value, strlen( value ) + 1 );

/* If the new default for XmlFormat has not yet been set, note if this
   element contained a "quoted" attribute. */
         if( this->formatdef == NATIVE_FORMAT ) {
            if( astXmlGetAttributeValue( element, QUOTED ) ) {
               this->formatdef = QUOTED_FORMAT;
            }
         }

/* Remove the ATTR element from the container. */
         element = Remove( this, element, status );

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", status, name,
                   GetTag( (AstXmlObject *) element, 1, status ) );
      }

/* If no suitable Value structure was found, then make a dynamic copy
   of the default string (if given) and return a pointer to this. */
   } else if ( def ) {
      result = astStore( NULL, def, strlen( def ) + (size_t) 1 );
   }

/* Return the result. */
   return result;
}

static AstXmlElement *ReadXmlText( AstXmlChan *this, int *status ){
/*
*  Name:
*     ReadXmlText

*  Purpose:
*     Create an in-memory XML tree from an XML text source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *ReadXmlText( AstXmlChan *this, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function constructs an in-memory XML representation of the data
*     source by reading text up to the end of the first element encountered
*     from which an AST Object could be constructed. If the Skip attribute is
*     zero, then an error is reported if there is any text prior to the start
*     of the first AST Object. If Skip is non-zero any initial text prior to
*     the start of the first usable element is ignored.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the returned XmlElement. This should be annuled using
*     astXmlAnnul when no longer needed. NULL is returned if the end of
*     the source text is reached without finding a en element from which
*     an AST Object could be read.

*  Notes:
*     - A NULL pointer is returned if an error has already occurred, of
*     if this function should fail for any reason.

*/

/* Local Variables: */
   astDECLARE_GLOBALS       /* Declare the thread specific global data */
   AstXmlElement *result;   /* Returned pointer */
   int skip;                /* Skip over initial irrelevant markup? */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Get the value of the Skip attribute. This indicates if we should skip
   over any irrelevant markup prior to the first element from which an
   AST object could be created. */
   skip = astGetSkip( this );

/* Store a pointer to the XmlChan in a module variable so that the IsUsable function
   can access its properties. */
   isusable_this = this;

/* Read characters from the XML source and return an XmlElement structure
   containing the first usable element encountered. */
   result = astXmlReadDocument( &(this->readcontext), IsUsable, skip,
                               GetNextChar, this );

/* Nullify the module variable for safety. */
   isusable_this = NULL;

/* If no usable element was found, annul the document. */
   if( !result ) this->readcontext = astXmlAnnul( this->readcontext );

/* Delete the returned element if an error has occurred. */
   if( !astOK ) result = astXmlAnnulTree( result );

/* Return the result. */
   return result;

}

static void ReCentreAnc( AstRegion *region, int nanc, AstKeyMap **ancs, int *status ){
/*
*  Name:
*     ReCentreAnc

*  Purpose:
*     Re-centre the Regions describing ancillary information extracted
*     from an AstroCoords elements.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     ReCentreAnc( AstRegion *region, int nanc, AstKeyMap **ancs, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function recentres the Regions which describe ancillary
*     information from an AstroCoords element so that it is centred at
*     the centre of the associated AstroCoordsArea element.

*  Parameters:
*     region
*        Pointer to the Region defining the associated AstroCoordsArea.
*     nanc
*        Number of KeyMap pointers stored in "ancs"
*     ancs
*        Pointer to an array of "nanc" elements, each being a pointer to
*        a KeyMap. Each one describes the ancilary information in an
*        AstroCoords element associated with the AstroCoordsArea decribed
*        by "region". Each KeyMap has elements with keys AST__STCERROR,
*        AST__STCRES, AST__STCSIZE, AST__STCPIXSZ, AST__STCVALUE each of
*        which holds a pointer to a Region. These Regions are modified on
*        exit so that they are centred on a point which inside the supplied
*        Region.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *frm;
   AstFrame *pfrm;
   AstFrameSet *fs;
   AstMapping *map;
   AstMapping *smap;
   AstObject *o;
   AstRegion *r2;
   AstRegion *r;
   char orgatt[ 20 ];
   char orgset[ 80 ];
   char setting[ 80 ];
   char sysatt[ 20 ];
   char sysset[ 80 ];
   const char *old_unit;
   const char *time_unit;
   double *lbnd;
   double *mid;
   double *ubnd;
   double epoch;
   int i;
   int j;
   int k;
   int ndim;
   int paxis;
   int time_axis;

   static const char *key[ 5 ] = { AST__STCERROR,
                                   AST__STCRES,
                                   AST__STCSIZE,
                                   AST__STCPIXSZ,
                                   AST__STCVALUE };

/* Check the inherited status. Also return if no Keymaps supplied. */
   if( !nanc || !astOK ) return;

/* Get the Frame  encapsulated by the suipplied Region. */
   frm = astRegFrame( region );

/* Get the bounds of the supplied Region. */
   ndim = astGetNaxes( frm );
   lbnd = astMalloc( sizeof( double )*(size_t) ndim );
   ubnd = astMalloc( sizeof( double )*(size_t) ndim );
   mid = astMalloc( sizeof( double )*(size_t) ndim );
   if( mid ) {
      astGetRegionBounds( region, lbnd, ubnd );

/* Get a mid point, taking account of unbounded axes. Also find the index of
   the time axis (if any) in the supplied Region, and get the System and
   TimeOrigin values for the time axis. */
      time_axis = -1;
      time_unit = NULL;
      orgatt[ 0 ] = 0;
      sysatt[ 0 ] = 0;
      for( i = 0; i < ndim; i++ ) {
         if( lbnd[ i ] > -0.5*DBL_MAX ) {
            if( ubnd[ i ] < 0.5*DBL_MAX ) {
               mid[ i ] = 0.5*( lbnd[ i ] + ubnd[ i ] );
            } else {
               mid[ i ] = lbnd[ i ];
            }
         } else {
            if( ubnd[ i ] < 0.5*DBL_MAX ) {
               mid[ i ] = ubnd[ i ];
            } else {
               mid[ i ] = 0.0;
            }
         }

/* If we have not found a time axis, see if the current axis is a time axis. */
         if( time_axis == -1 ) {
            astPrimaryFrame( frm, i, &pfrm, &paxis );
            if( astIsATimeFrame( pfrm ) ) {

/* If so, record its index. */
               time_axis = i;

/* If the TimeOrigin attribute is set, save its value. Create strings
   holding the attribute name and appropriate setting string for use with
   the ancillary regions. */
               if( astTestTimeOrigin( (AstTimeFrame *) pfrm ) ) {
                  sprintf( orgatt, "TimeOrigin(%d)", i + 1 );
                  sprintf( orgset, "TimeOrigin(%d)=%s", i + 1,
                           astGetC( pfrm, "TimeOrigin" ) );
               }

/* If the System attribute is set, save its value. Create strings
   holding the attribute name and appropriate setting string for use with
   the ancillary regions. */
               if( astTestSystem( pfrm ) ) {
                  sprintf( sysatt, "System(%d)", i + 1 );
                  sprintf( sysset, "System(%d)=%s", i + 1,
                           astGetC( pfrm, "System" ) );
               }

               time_unit = astGetUnit( pfrm, 0 );
            }
            pfrm = astAnnul( pfrm );
         }

      }

/* Get the Region Epoch. */
      if( astTestEpoch( frm ) ){
         epoch = astGetEpoch( frm );
         sprintf( setting, "Epoch=MJD %.*g", DBL_DIG, epoch );
      } else {
         setting[ 0 ] = 0;
         epoch = AST__BAD;
      }

/* Loop round each KeyMap. */
      for( j = 0; j < nanc; j++ ) {

/* Loop round each of the relevant KeyMap elements (skip the "Value"
   element since this should not be re-centred). */
         for( k = 0; k < 5; k++ ) {
            if( astMapGet0A( ancs[ j ], key[ k ], &o ) ) {
               r = (AstRegion *) o;

/* The System and TimeOrigin attributes of the STC Region are set when the
   AstroCoordArea is read. This occurs after the ancillary Coords Regions are
   created. Consequently, the ancillary Coords Regions may not have set
   System and/or TimeOrigin values.  So, for System and TimeOrigin, if
   the attribute is set in the supplied Region but not set in the ancillary
   Region, transfer the set value to the ancillary Region. */
               if( strlen( sysatt ) && strlen( orgatt ) ) {
                  if( !astTest( r, sysatt ) && !astTest( r, orgatt ) ) {
                     astRegSetAttrib( r, sysset, NULL );

                     old_unit = astGetUnit( r, time_axis );
                     if( old_unit && time_unit &&
                         strcmp( old_unit, time_unit ) ) {
                        if( !astTestUnit( r, time_axis ) ) {
                           old_unit = NULL;
                        } else {
                           old_unit = astStore( NULL, old_unit,
                                                strlen( old_unit ) + 1 );
                        }
                        astSetUnit( r, time_axis, time_unit );
                     }
                     astRegSetAttrib( r, orgset, NULL );
                     if( !old_unit ) {
                        astClearUnit( r, time_axis );
                     } else if( strcmp( old_unit, time_unit ) ) {
                        astSetUnit( r, time_axis, old_unit );
                        old_unit = astFree( (void *) old_unit );
                     }
                  }
               }

/* Re-centre the Regions held in this element of the KeyMap, and set
   its Epoch (do not re-centre the "Value" element). */
               if( strcmp( key[ k ], AST__STCVALUE ) ) {

/* First ensure the ancillary Region refers to the supplied Frame. */
                  fs = astConvert( r, frm, "" );
                  if( fs ) {
                     map = astGetMapping( fs, AST__BASE, AST__CURRENT );
                     smap = astSimplify( map );
                     if( !astIsAUnitMap( smap ) ) {
                        r2 = astMapRegion( r, smap, frm );
                        astMapPut0A( ancs[ j ], key[ k ], r2, NULL );
                        (void) astAnnul( r );
                        r = r2;
                     }
                     map = astAnnul( map );
                     smap = astAnnul( smap );
                     fs = astAnnul( fs );

/* Now set the epoch and re-centre.*/
                     if( epoch != AST__BAD ) astRegSetAttrib( r, setting, NULL );
                     astRegCentre( r, mid, NULL, 0, AST__CURRENT );
                  }
               }
               r = astAnnul( r );
            }
         }
      }
   }

/* Free resources. */
   lbnd = astFree( lbnd );
   ubnd = astFree( ubnd );
   mid = astFree( mid );
   frm = astAnnul( frm );
}

static AstObject *RedshiftFrameReader( AstXmlChan *this,
                                       AstXmlElement *elem, int *status ) {
/*
*  Name:
*     RedshiftFrameReader

*  Purpose:
*     Make an AST Object from an IVOA RedshiftFrame element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *RedshiftFrameReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     RedshiftFrame element. The returned Object is a SpecFrame in which
*     the Domain is set explicitly to REDSHIFT.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA RedshiftFrame element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.

*/

/* Local Variables: */
   AstSpecFrame *new;            /* Pointer to returned Object */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[3];         /* Names of the subelements to be searched for */
   const char *sor;              /* StdOfRest for returned Frame */
   const char *type;             /* Doppler type (velocity or redshift) */
   const char *sys;              /* Spectral system */
   int max[3];                   /* Max allowed occurrences of each name */
   int min[3];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return (AstObject *) new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "TOPOCENTER|BARYCENTER|HELIOCENTER|GEOCENTER|LSR|"
                "LSRK|GALACTIC_CENTER|LOCAL_GROUP_CENTER|LSRD";
   names[ 1 ] = DOPPLER_DEFINITION;
   names[ 2 ] = "Name";
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   min[ 2 ] = 0;
   scan = ScanIVOAElement( this, elem, 3, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the name of the Element specifying the reference position and find
   the corresponding AST name.*/
      sor = astXmlGetName( scan->el[0][0] );
      if( !strcmp( sor, "TOPOCENTER" ) ) {
         sor = "Topo";
      } else if( !strcmp( sor, "BARYCENTER" ) ){
         sor = "Bary";
      } else if( !strcmp( sor, "GEOCENTER" ) ){
         sor = "Geo";
      } else if( !strcmp( sor, "LSR" ) || !strcmp( sor, "LSRK" ) ) {
         sor = "LSRK";
      } else if( !strcmp( sor, "LSRD" ) ) {
         sor = "LSRD";
      } else if( !strcmp( sor, "GALACTIC_CENTER" ) ) {
         sor = "Galactic";
      } else if( !strcmp( sor, "LOCAL_GROUP_CENTER" ) ) {
         sor = "Local_group";
      } else if( !strcmp( sor, "HELIOCENTER" ) ) {
         sor = "Helio";
      } else if( astOK ){
         astError( AST__INTER, "RedshiftFrameReader(XmlChan): Unknown "
                   "standard of rest %s (internal AST programming error).", status,
                   sor );
      }

/* Issue a warning if the reference position includes an ephemeris. */
      if( FindElement( this, scan->el[0][0], "PlanetaryEphem", status ) ) {
         Report( this, scan->el[0][0], WARNING, "contains a <PlanetaryEphem> "
                 "element which will be ignored", status );
      }

/* Get the value of the value_type attribute from the element. */
      type = astXmlGetAttributeValue( elem, "value_type" );
      if( !type ) type = "VELOCITY";

/* If the type is REDSHIFT, set the system to redshift. Also check that
   any <DopplerDefinition> element is "OPTICAL". */
      if( !strcmp( type, "REDSHIFT" ) ) {
         sys = astXmlGetValue( scan->el[1][0], 0 );
         if( sys && !strcmp( sys, "OPTICAL" ) ) {
            sys = "REDSHIFT";
         } else {
            Report( this, elem, FAILURE, "specifies dimensionless "
                    "redshift (z) but has non-optical <DopplerDefinition>", status );
         }

/* Otherwise, get the value of the Doppler definition element, and translate
   it to an AST value.*/
      } else {
         sys = astXmlGetValue( scan->el[1][0], 0 );
         if( !sys ) {
            Report( this, elem, FAILURE, "contains a <DopplerDefinition> "
                    "element which is not simply character data", status );

         } else if( !strcmp( sys, "OPTICAL" ) ) {
            sys = "VOPT";

         } else if( !strcmp( sys, "RADIO" ) ) {
            sys = "VRAD";

         } else if( !strcmp( sys, "RELATIVISTIC" ) ) {
            sys = "VREL";

         } else {
            Report( this, elem, FAILURE, "contains unsupported Doppler definition", status );
         }
      }

/* Create a suitable SpecFrame. */
      new = astSpecFrame( "Domain=REDSHIFT,System=%s,StdOfRest=%s", status, sys, sor);

/* If the SpectralFrame has a <Name> element use it as the SpecFrame title. */
      if( scan->count[2] ) astSetTitle( new, astXmlGetValue( scan->el[2][0], 0 ) );

/* Free resources */
      scan = FreeIVOAScan( scan, status );

   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Object. */
   return (AstObject *) new;
}

static AstRegion *RedshiftIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                          AstFrame *frm, int *status ){
/*
*  Name:
*     RedshiftIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA RedshiftInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *RedshiftIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                        AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     RedshiftInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA RedshiftInterval element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit or System attribute is not set, this
*        function will decide on the values to be used, and set these
*        values in the supplied Frame before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstRegion *new;               /* Pointer to returned Region */
   AstSystemType sys;            /* Redshift system */
   IVOAScan *scan;               /* Structure holding scan results */
   char *unit;                   /* Unit string from supplied element */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   double hilimit;               /* Upper spectral limit */
   double lolimit;               /* Lower spectral limit */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "LoLimit";
   names[ 1 ] = "HiLimit";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      lolimit = scan->count[0] ? ElemValueD( this, scan->el[0][0], 0.0, status ) : AST__BAD;
      hilimit = scan->count[1] ? ElemValueD( this, scan->el[1][0], 0.0, status ) : AST__BAD;

/* Use any unit and vel_time_unit attributes in the supplied element to
   determine the system and units for the redshift Frame. */
      sys = RedshiftSys( this, elem, &unit, 1, status );

/* If no system has been set in the supplied Frame, set a default system
   now (radio velocity if both units are present, dimensionaless redshift
   otherwise). */
      if( !astTestSystem( frm ) ) {
         astSetSystem( frm, sys );

/* The ReddshiftSys function always returns AST__VRADIO if the velocity
   is not dimensionless. In this case, if the supplied Frame has system
   explicitly set AST__VOPTICAL, we use the supplied Frame preference of
   optical/radio instead of the default returned by RedshiftSys. */
      } else if( sys != AST__REDSHIFT ) {
         sys = astGetSystem( frm );
         if( sys == AST__REDSHIFT ) sys = AST__VRADIO;
      }

/* Take a copy of the supplied Frame and set its Units to the value found
   above. */
      cfrm = astCopy( frm );
      astSetUnit( cfrm, 0, unit );

/* If at least one limit was found, create an Interval within this
   modified Frame. Otherwise create a negated NullRegion. */
      if( lolimit != AST__BAD || hilimit != AST__BAD ) {
         new = (AstRegion *) astInterval( cfrm, &lolimit, &hilimit, NULL, "", status );
      } else {
         new = (AstRegion *) astNullRegion( cfrm, NULL, "negated=1", status );
      }

/* If the Units of this Region differs from that of the supplied Frame,
   set it to the Units of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Units. If the supplied
   Frame had no set Units, set it to the units implied by the supplied
   XML element. */
      if( astTestUnit( frm, 0 ) ) {
         funit = astGetUnit( frm, 0 );
         if( strcmp( funit, unit ) ) astSetUnit( new, 0, funit );
      } else {
         astSetUnit( frm, 0, unit );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      cfrm = astAnnul( cfrm );
      if( unit ) unit = astFree( unit );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *RedshiftReader( AstXmlChan *this, AstXmlElement *elem,
                                  AstFrame *frm, AstKeyMap **anc, int *status ){
/*
*  Name:
*     RedshiftReader

*  Purpose:
*     Modify a Frame to take account of an STC <Redshift> element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *RedshiftReader( AstXmlChan *this, AstXmlElement *elem,
*                                AstFrame *frm, AstKeyMap **anc, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the supplied STC <Redshift> element, and uses it,
*     if possible, to create the uncertainty associated with the redshift
*     axis in the supplied Frame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Redshift element.
*     frm
*        Pointer to the 1D redshift Frame.
*     anc
*        Address of a location at which to put a pointer to a newly
*        created KeyMap. This KeyMap will contain ancillary information
*        from the Redshift. The keys identify the item of ancillary
*        information (Name, Value, Error, Resolution, Size, Pixel Size).
*        The value associated with the Name key is string containing
*        the Name item from the Redshift. The value associated with each of
*        the other keys is a pointer to a 1D Region within the supplied
*        Frame, corresponding to the value, error, resolution, etc. Keys
*        will not be present in the returned KeyMap if the corresponding
*        item of ancillary information is not present in the Redshift. A
*        NULL pointer is returned if there is no ancillary information at all.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The uncertainty Region, or NULL if the supplied Redshift element
*     does not specify an uncertainty.

*/

/* Local Variables: */
   AstFrameSet *fs;         /* FrameSet connecting "sf1" and "sf2" */
   AstMapping *map;         /* Mapping from <Redshift> Frame to supplied Frame */
   AstRegion *r2;           /* Region mapped into returned Frame */
   AstRegion *r3;           /* Simplified Region mapped into returned Frame */
   AstRegion *r;            /* Original Region */
   AstRegion *result;       /* Returned uncertainty Region */
   AstSpecFrame *sf1;       /* SpecFrame describing value element */
   AstSystemType fsys;      /* Redshift system from supplied Stc */
   IVOAScan *scan;          /* Structure holding scan results */
   const char *name;        /* Pointer to XML element name */
   const char *names[6];    /* Names of the subelements to be searched for */
   char *unit;              /* Pointer to Redshift's unit attribute string */
   double lbnd[ 1 ] ;       /* Lower interval bounds */
   double tmp;              /* Temporary storage */
   double ubnd[ 1 ] ;       /* Upper interval bounds */
   double v;                /* Axis value */
   int max[6];              /* Max allowed occurrences of each name */
   int min[6];              /* Min allowed occurrences of each name */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Name";
   names[ 1 ] = "Error";
   names[ 2 ] = "Value";
   names[ 3 ] = "Resolution";
   names[ 4 ] = "Size";
   names[ 5 ] = "PixSize";
   max[ 0 ] = 1;
   max[ 1 ] = 2;
   max[ 2 ] = 1;
   max[ 3 ] = 2;
   max[ 4 ] = 2;
   max[ 5 ] = 2;
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   min[ 4 ] = 0;
   min[ 5 ] = 0;
   scan = ScanIVOAElement( this, elem, 6, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a KeyMap to return holding ancilary info, and put the Name into
   it. */
      *anc = astKeyMap( "", status );
      if( scan->count[0] > 0 ) astMapPut0C( *anc, AST__STCNAME,
                                  astXmlGetValue( scan->el[0][0], 0 ), NULL );

/* Determine the units and system implied by the <Redshift> element.
   The returned system is AST__REDSHIFT if there is no unit attribute in
   the <Redshift> element, and is AST__VRADIO otherwise. */
      fsys = RedshiftSys( this, elem, &unit, 1, status );

/* If no system has been set in the supplied Frame, set it now to the system
   determined above. */
      if( !astTestSystem( frm ) ) {
         astSetSystem( frm, fsys );

/* The ReddshiftSys function above always returns AST__VRADIO if the velocity
   is not dimensionless. However, the supplied Frame may have System set
   explicitly to AST__VOPTICAL. In this case change the "fsys" value to use
   AST__VOPTICAL. */
      } else if( fsys != AST__REDSHIFT ) {
         fsys = astGetSystem( frm );
         if( fsys == AST__REDSHIFT ) fsys = AST__VRADIO;
      }

/* If the supplied Frame has no set units, set them now to the units of
   the Redshift element (if any, and if the redshift is not dimensionless). */
      if( unit && fsys != AST__REDSHIFT &&
          astGetSystem( frm ) != AST__REDSHIFT && !astTestUnit( frm, 0 ) ) {
         astSetUnit( frm, 0, unit );
      }

/* The values represented by the <Redshift> element may not be in the same
   system, units, etc as the supplied SpecFrame. We will need to be able to
   convert from one to the other, so create a SpecFrame describing the
   system and units used by the <Redshift> element. */
      sf1 = astCopy( frm );
      astSetSystem( sf1, fsys );
      if( unit ) {
         astSetUnit( sf1, 0, unit );
         unit = astFree( unit );
      }

/* Find the Mapping from Redshift value to the supplied SpecFrame value */
      fs = astConvert( sf1, frm, "" );
      if( fs ) {
         map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         fs = astAnnul( fs );
      } else {
         map = NULL;
         Report( this, elem, FAILURE, "connot convert AstroCoords "
                 "redshift values to the required redshift system", status );
      }

/* If this Redshift contains a Value which can be read, obtain it. */
      if( scan->count[ 2 ] > 0 ) {
         name = astXmlGetName( scan->el[ 2 ][ 0 ] );
         if( name && !strcmp( name, "Value" ) ) {
            v = ElemValueD( this, scan->el[ 2 ][ 0 ], AST__BAD, status );

/* Convert the value into the supplied SpecFrame system. Create an
   Interval describing it and store it in the returned ancillary keyMap.
   Note we create an Interval rather than a PintList since the Prism
   class can only extrude using Intervals. */
            astTran1( map, 1, &v, 1, &tmp );
            r = (AstRegion *) astInterval( frm, &tmp, &tmp, NULL, "", status ) ;
            astMapPut0A( *anc, AST__STCVALUE, r, NULL );
            r = astAnnul( r );
         }
      }

/* Check for Error values in the Redshift. */
      if( scan->count[ 1 ] > 0 ) {

/* Issue a warning if more than 1 Error value was found. */
         if( scan->count[ 1 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Error>"
                    " element. AST can only use the first", status );
         }

/* Get the first Error value. */
         v = ElemValueD( this, scan->el[1][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an error bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Redshift element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            result = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCERROR, result, NULL );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for Resolution values in the Redshift. */
      if( scan->count[ 3 ] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 3 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Resolution>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[3][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Redshift element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCRES, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for Size values in the Redshift. */
      if( scan->count[ 4 ] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 4 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Size>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[4][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Redshift element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCSIZE, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for PixSize values in the Redshift. */
      if( scan->count[ 5] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 5 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <PixSize>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[5][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Redshift element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCPIXSZ, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Free resources. */
      scan = FreeIVOAScan( scan, status );
      sf1 = astAnnul( sf1 );
      map = astAnnul( map );
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;

}

static AstSystemType RedshiftSys( AstXmlChan *this, AstXmlElement *elem,
                                  char **unit, int report, int *status ){
/*
*  Name:
*     RedshiftSys

*  Purpose:
*     Determine the redshift system described by the attributes in a
*     given element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstSystemType RedshiftSys( AstXmlChan *this, AstXmlElement *elem,
*                                char **unit, int report, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function determines the redshift system described by the unit and
*     vel_time_unit attributes in the supplied element. It optionally reports
*     an error if the units are not recognised.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA element containing the unit attribute to be used.
*     unit
*        Pointer to a location at which to return a pointer to a
*        dynamically allocated string in which is stored the total units string
*        implied by the unit and vel_time_unit attributes. This string
*        should be freed when no longer needed using astFree. A NULL
*        pointer is returned if either of the two attributes (unit and
*        vel_time_unit) is not found in the supplied element, or if an error
*        occurs.
*     report
*        If non-zero, then a failure is reported if the spectral system
*        cannot be determined from the supplied string.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The redshift system (radio velocity if both unit and vel_time_unit
*     attributes are present in the supplied element, or dimensionaless
*     redshift otherwise).

*/

/* Local Variables: */
   const char *punit;            /* Pointer to positional unit string */
   const char *tunit;            /* Pointer to time unit string */
   int pl;                       /* Length of punit string */
   int tl;                       /* Length of tunit string */

/* Initialise. */
   *unit = NULL;

/* Check the global error status. */
   if ( !astOK ) return AST__BADSYSTEM;

/* Get the Unit attribute from the element (this describes units of position) */
   punit = astXmlGetAttributeValue( elem, "unit" );
   if( punit ) {

/* Check it is a linear measure (not angular). */
      if( strstr( "m", punit ) &&
          strstr( "km", punit ) &&
          strstr( "mm", punit ) &&
          strstr( "AU", punit ) &&
          strstr( "kpc", punit ) &&
          strstr( "Mpc", punit ) &&
          strstr( "lyr", punit ) ) {
         if( report ) Report( this, elem, FAILURE, "contains an angular unit attribute", status );
      }
   }

/* Get the vel_time_unit attribute from the element (this describes units of
   time). If OK, construct the total unit string (eg "km/h") . */
   tunit = astXmlGetAttributeValue( elem, "vel_time_unit" );
   if( tunit ) {
      if( !punit ) {
         if( report ) Report( this, elem, FAILURE, "contains time units but not position units - assuming Z", status );
      } else {
         pl = strlen( punit );
         tl = strlen( tunit );
         *unit = astMalloc( (size_t)( pl + tl + 2 ) );
         if( *unit ) {
            strcpy( *unit, punit );
            (*unit)[ pl ] = '/';
            strcpy( *unit + pl + 1, tunit );
         }
      }

   } else if( punit ) {
      if( report ) Report( this, elem, FAILURE, "contains position units but not time units - assuming Z", status );
   }

/* Return a default system (radio velocity if both units are present,
   dimensionless redshift otherwise). */
   return ( punit && tunit ) ? AST__VRADIO : AST__REDSHIFT;
}

static AstRegion *RegionReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     RegionReader

*  Purpose:
*     Make an AST Region from any subclass of IVOA Region element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *RegionReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     element which can be of any subclass of Region.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   const char *name;             /* Region type */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Process each supported type of STC Region. */
   name = astXmlGetName( elem );
   if( !strcmp( name, "Intersection" ) ) {
      new = IntersectionReader( this, elem, frm, status );

   } else if( !strcmp( name, "Union" ) ) {
      new = UnionReader( this, elem, frm, status );

   } else if( !strcmp( name, "Negation" ) ) {
      new = NegationReader( this, elem, frm, status );

   } else if( !strcmp( name, "AllSky" ) ) {
      new = AllSkyReader( this, elem, frm, status );

   } else if( !strcmp( name, "Circle" ) ) {
      new = CircleReader( this, elem, frm, status );

   } else if( !strcmp( name, "Ellipse" ) ) {
      new = EllipseReader( this, elem, frm, status );

   } else if( !strcmp( name, "Polygon" ) ) {
      new = PolygonReader( this, elem, frm, status );

   } else if( !strcmp( name, "Box" ) ) {
      new = BoxReader( this, elem, frm, status );

   } else if( !strcmp( name, "Convex" ) ) {
      new = ConvexReader( this, elem, frm, status );

   } else {
      astError( AST__INTER, "RegionReader(XmlChan): Does not yet "
                "support \"%s\" regions (internal AST programming "
                "error).", status, name );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstXmlElement *Remove( AstXmlChan *this, AstXmlElement *element, int *status ) {
/*
*  Name:
*     Remove

*  Purpose:
*     Remove an element from the current container element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *Remove( AstXmlChan *this, AstXmlElement *element, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function removes the specified element from the current
*     container element, and then annuls the removed element. An error is
*     reported if the element being removed contains anything other than
*     comments, "isa" elements and blank character data (all contents should
*     have been consumed by the process of reading the object).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     element
*        Pointer to the XML element to be removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer is returned.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item */
   const char *def;         /* Pointer to default attribute value */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still in the element */

/* Check the global error status, and the supplied element. */
   if ( !astOK || !element ) return NULL;

/* Check we have a container from which to remove the element. If so,
   check that the container is the elements parent. If so, remove the
   element from its parent container. */
   if( this->container ) {
      if( (AstXmlParent *) this->container != astXmlGetParent( element ) ){
         astError( AST__INTER, "Remove(XmlChan): Supplied element is not "
                   "contained within the current container element (internal "
                   "AST programming error)." , status);
      } else {
         astXmlRemoveItem( element );
      }
   }

/* Check that the element being removed is empty (apart from comments,
   defaulted values and "isa" elements). */
   nitem = astXmlGetNitem( element );
   for( i = 0; i < nitem; i++ ) {
      item = astXmlGetItem( element, i );
      if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* See if this element represents a default value */
         def = astXmlGetAttributeValue( item, DEFAULT );

/* Default values and "isa" elements are OK. */
         if( ( !def || strcmp( def, TRUE ) ) && astOK &&
             strcmp( astXmlGetName( item ), ISA ) ) {

/* Remove any "definedby" attribute (added by ReadClassData) so that it
   does not appear in the error message. */
            if( astXmlGetAttributeValue( item, DEFINEDBY ) ) {
               astXmlRemoveAttr( item, DEFINEDBY, NULL );
            }

/* Report the error. */
            if( astOK ) astError( AST__BADIN, "astRead(XmlChan): The following "
                                  "tag was not recognised as valid input within "
                                  "a %s: %s", status, astXmlGetName( element ),
                                  GetTag( (AstXmlObject *) item, 1, status ) );
            break;
         }

/* Character data is OK so long as it contains only white space */
      } else if( astXmlCheckType( item, AST__XMLBLACK ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following character "
                   "data was not recognised as valid input within a %s: %s", status,
                   astXmlGetName( element ), astXmlGetValue( item, 0 ) );
         break;

      } else if( astXmlCheckType( item, AST__XMLCDATA ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following CDATA section "
                   "data was not recognised as valid input within a %s: %s", status,
                   astXmlGetName( element ), astXmlGetValue( item, 0 ) );
         break;

      } else if( astXmlCheckType( item, AST__XMLPI ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following processing "
                   "instruction was not recognised as valid input within "
                   "a %s: %s", status, astXmlGetName( element ), GetTag( (AstXmlObject *) item, 1, status ) );
         break;
      }
   }

/* Remove the element from its parent and the annul it. */
   astXmlRemoveItem( element );
   astXmlAnnul( element );

/* Return a NULL pointer. */
   return NULL;
}

static void Report( AstXmlChan *this, AstXmlElement *elem, int severity,
                    const char *msg, int *status ){
/*
*  Name:
*     Report

*  Purpose:
*     Handle problems reading supplied XML.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void Report( AstXmlChan *this, AstXmlElement *elem, int severity,
*                  const char *msg, int *status )

*  Class Membership:
*     XmlChan member function

*  Description:
*     This function handles conditions which arise whilst interpreting

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XmlElement which cound not be interpreted.
*     severity
*        WARNING (in which case the message is added to a list of
*        warnings, but execution continues), or FAILURE, in which case
*        an error is reported using astError, or RESET in which case any
*        warnings stored in the XmlChan are removed ("elem" and "msg" are
*        ignored).
*     msg
*        A message describing the condition.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   char *text;                /* Pointer to tformatted element text */
   const char *name;          /* Element name */

   if( severity == RESET ) astAddWarning( this, 0, NULL, NULL, status );

   if( !astOK ) return;

   if( severity == WARNING && !astGetStrict( this ) ) {
      name = astXmlGetName( elem );
      astAddWarning( this, 1, "astRead(%s): Warning whilst reading %s %s "
                     "element: %s", "astRead", status, astGetClass( this ),
                     ANA(name), name, msg );
   } else {
      text = (char *) astXmlGetTag( elem, 1 );
      astError( AST__BADIN, "astRead(%s): Failed to read %s element: %s", status,
                astGetClass( this ), text, msg );
      text = astFree( text );
   }
}

static IVOAScan *ScanIVOAElement( AstXmlChan *this, AstXmlElement *elem, int n,
                                  const char *names[], int min[], int max[], int *status ){
/*
*  Name:
*     ScanIVOAElement

*  Purpose:
*     Identify required sub-elements within an IVOA element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     IVOAScan *ScanIVOAElement( AstXmlChan *this, AstXmlElement *elem, int n,
*                                const char *names[], int min[], int max[], int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the supplied XML element for named sub-elements.
*     A structure is returned containing the number of sub-elements found
*     with each name, and pointers to the sub-elements. This structure
*     should be freed using FreeIVOAScan when no longer needed.
*
*     Reports are made about any content in the supplied element which is
*     not specified in the list of known sub-element names (excepting
*     comments and white space).
*
*     Reports are also made if the number of sub-elements found with each
*     known name is inappropriate (the minimum and maximum allowed
*     occurrences of each name is specified by the caller).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XML element to be searched.
*     n
*        The number of names supplied in "names"
*     names
*        An array holding pointers to strings giving the names of the known
*        sub-elements. Each string may be either a single element name,
*        or a set of element names separated by "|" (the string must
*        also start and end with a "|"). If a set is supplied, then the
*        associated "min" and "max" values specify the minimum and maximum
*        total number of occurrences of all names in the set, and the
*        occurrence count stored in the returned structure gives the total
*        number of occurrences of all names in the set.
*     min
*        An array holding the mimimum number of occrrences of each name within
*        the element being searched. Supplied in the same order as the names
*        themselves.
*     max
*        An array holding the maximum number of occrrences of each name within
*        the element being searched. Supplied in the same order as the names
*        themselves.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the IVOAScan structure holding the results of the
*     scan. A NULL pointer is returned unless all names have at least
*     their minimum number of occurrences in the supplied element. A NULL
*     pointer is returned if an error occurs.

*/

/* Local Variables: */
   AstXmlContentItem *item;      /* Current content item */
   IVOAScan *result;             /* Pointer to returned structure */
   char *text;                   /* Pointer formatted item string */
   char buff[ 200 ];             /* Message buffer */
   const char *name;             /* Pointer to element name string */
   const char *w1;               /* Pointer to word to use in message */
   const char *w2;               /* Pointer to word to use in message */
   const char *p;                /* Pointer to start of name in string */
   int i;                        /* Index of current content item */
   int j;                        /* Index of current name */
   int k;                        /* Index of current occurrence of name */
   int l;                        /* Length of element name */
   int known;                    /* Was content item known? */
   int nitem;                    /* No. of content items in supplied element */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise a structure to hold the results of the scan. */
   result = astMalloc( sizeof(IVOAScan) );
   if( result ) {
      result->n = n;
      result->count = astMalloc( sizeof(int)*(size_t)n);
      result->el = astMalloc( sizeof(AstXmlElement **)*(size_t)n);
      if( result->el ) {
         for( j = 0; j < n; j++ ) {
            result->count[ j ] = 0;
            result->el[ j ] = NULL;
         }
      }
   }

/* Loop round all items in the elements contents. */
   if( astOK ) {
      nitem = astXmlGetNitem( elem );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( elem, i );
         known = 1;

/* If it is not an XML element, it is not known. */
         if( !astXmlCheckType( item, AST__XMLELEM ) ) {
            known = 0;

/* If it is an element, get the name of the element. */
         } else {
            name = astXmlGetName( item );

/* See if this name is in the supplied list of known names. */
            known = 0;
            j = 0;
            if( name ) {
               l = strlen( name );
               for( j = 0; j < n; j++ ) {
                  p = strstr(  names[ j ], name );
                  if( p ){
                     if( p == names[ j ] ) {
                        if( p[ l ] == 0 || p[ l ] == '|' ) {
                           known = 1;
                           break;
                        }
                     } else {
                        if( p[ -1 ] == '|' && ( p[ l ] == 0 || p[ l ] == '|' ) ) {
                           known = 1;
                           break;
                        }
                     }
                  }
               }
            }

/* If it is known, store the element in the results structure */
            if( known ) {
               k = ( result->count[ j ] )++;
               result->el[ j ]= astGrow( result->el[ j ], k + 1,
                                         sizeof( AstXmlElement * ) );
               if(  result->el[ j ] ) {
                  result->el[ j ][ k ] = (AstXmlElement *) item;
               } else {
                  break;
               }
            }
         }

/* If this content item was not known, issue a warning unless it is a comment
   or white space. */
         if( !known && !astXmlCheckType( item, AST__XMLCOM ) &&
                       !astXmlCheckType( item, AST__XMLWHITE ) ) {
            text = (char *) astXmlFormat( item );
            if( text ) {
               if( strlen( text ) > 30 ) text[ 30 ] = 0;
               sprintf( buff, "contains the following which is being ignored: \"%s\"",
                        text );
               text = astFree( text );
               Report( this, elem, WARNING, buff, status );
            }
         }
      }

/* Now check that the number of instances of each element found is OK.
   Report warnings or failures if not. */
      if( astOK ) {
         for( j = 0; j < n; j++ ) {
            if( result->count[ j ] < min[ j ] ) {
               w1 = ( result->count[ j ] == 1 ) ? "element" : "elements";
               w2 = ( min[ j ] == 1 ) ? "is" : "are";
               sprintf( buff, "contains %d <%s> %s but at least %d %s needed",
                        result->count[ j ], names[ j ], w1, min[ j ], w2 );
               Report( this, elem, FAILURE, buff, status );

            } else if ( result->count[ j ] > max[ j ] ) {
               w1 = ( result->count[ j ] == 1 ) ? "element" : "elements";
               w2 = ( max[ j ] == 1 ) ? "is" : "are";
               sprintf( buff, "contains %d <%s> %s but no more than %d %s "
                        "allowed (only the first will be used)",
                        result->count[ j ], names[ j ], w1, max[ j ], w2 );
               Report( this, elem, WARNING, buff, status );
            }
         }
      }
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = FreeIVOAScan( result, status );

/* Return the results structure.*/
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     XmlChan member function (over-rides the astSetAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function assigns an attribute value for a XmlChan, the
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
*        Pointer to the XmlChan.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by "astSscanf" */
   int pr;                       /* Offset to start of string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* XmlLength */
/* ----------*/
   if ( nc = 0,
        ( 1 == astSscanf( setting, "xmllength= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetXmlLength( this, ival );

/* XmlFormat */
/* ----------*/
   } else if( nc = 0,
        ( 0 == astSscanf( setting, "xmlformat=%n%*[^\n]%n", &ival, &nc ) )
        && ( nc >= len ) ) {

      nc = astChrLen( setting + ival );

      if( !Ustrncmp( setting + ival, NATIVE_STRING, nc, status ) ){
         astSetXmlFormat( this, NATIVE_FORMAT );

      } else if( !Ustrncmp( setting + ival, QUOTED_STRING, nc, status ) ){
         astSetXmlFormat( this, QUOTED_FORMAT );

      } else if( !Ustrncmp( setting + ival, IVOA_STRING, nc, status ) ){
         astSetXmlFormat( this, IVOA_FORMAT );

      } else {
         astError( AST__BADAT, "astSet(%s): Unknown XML format '%s' "
                   "requested for a %s.", status, astGetClass( this ), setting + ival,
                   astGetClass( this ) );
      }

/* XmlPrefix */
/* ----------*/
   } else if ( nc = 0, ( 0 == astSscanf( setting, "xmlprefix=%n%*[^\n]%n", &pr, &nc ) )
                && ( nc >= len ) ) {
      astSetXmlPrefix( this, setting + pr );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static void SinkWrap( void (* sink)( const char * ), const char *line, int *status ) {
/*
*  Name:
*     SinkWrap

*  Purpose:
*     Wrapper function to invoke a C XmlChan sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void SinkWrap( void (* sink)( const char * ), const char *line, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function, whose single parameter is a
*        pointer to a const, null-terminated string containing the
*        text to be written, and which returns void. This is the form
*        of XmlChan sink function employed by the C language interface
*        to the AST library.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the sink function. */
   ( *sink )( line );
}

static char *SourceWrap( const char *(* source)( void ), int *status ) {
/*
*  Name:
*     SourceWrap

*  Purpose:
*     Wrapper function to invoke a C XmlChan source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     char *SourceWrap( const char *, int *status(* source)( void ) )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function, with no parameters, that
*        returns a pointer to a const, null-terminated string
*        containing the text that it read. This is the form of XmlChan
*        source function employed by the C language interface to the
*        AST library.
*     status
*        Pointer to the inherited status variable.

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

static AstObject *SpaceFrameReader( AstXmlChan *this,
                                    AstXmlElement *elem, int *status ) {
/*
*  Name:
*     SpaceFrameReader

*  Purpose:
*     Make an AST Object from an IVOA SpaceFrame element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *SpaceFrameReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     SpaceFrame element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA SpaceFrame element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.

*/

/* Local Variables: */
   AstObject *new;               /* Pointer to returned Object */
   AstXmlElement *el;            /* Pointer to sub-element */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *dom;              /* Domain string for returned SkyFrame */
   const char *eq;               /* Equinox string for returned SkyFrame */
   const char *names[4];         /* Names of the subelements to be searched for */
   const char *sys;              /* System for returned Frame */
   int ignore_h;                 /* Ignore 3rd spherical axis? */
   int max[4];                   /* Max allowed occurrences of each name */
   int min[4];                   /* Min allowed occurrences of each name */
   int isgeod;                   /* Is the system geodetic lon/lat? */
   int isgeoc;                   /* Is the system geocentric lon/lat? */
   int need_eq;                  /* Does system need an equinox? */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "ICRS|GALACTIC_II|SUPER_GALACTIC|HEE|FK4|FK5|ECLIPTIC|GEO_C|GEO_D";
   names[ 1 ] = "TOPOCENTER";
   names[ 2 ] = "Name";
   names[ 3 ] = "SPHERICAL|CARTESIAN|UNITSPHERE|POLAR";
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   max[ 2 ] = 1;
   max[ 3 ] = 1;
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   min[ 2 ] = 0;
   min[ 3 ] = 1;
   scan = ScanIVOAElement( this, elem, 4, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the sky coordinate system specified in the element. */
      sys = astXmlGetName( scan->el[0][0] );
      need_eq = 0;
      dom = NULL;

/* If the system is geodetic or geocentric, ignore height information if
   supplied. This is so we can get an approximation to an observatory
   position given in 3D, for use with SpecFrame. */
      ignore_h = 0;
      isgeod = sys && !strcmp( sys, "GEO_D" );
      isgeoc = sys && !strcmp( sys, "GEO_C" );
      if( isgeod || isgeoc ){
         if( AttrValueI( this, scan->el[3][0], "coord_naxes", 2, status ) != 2 ) {
            Report( this, elem, WARNING, "contains 3D spherical spatial "
                    "coords (unsupported by AST - height information will "
                    "be ignored)", status );
            ignore_h = 1;
         }

/* If the system is geodetic ignore any attributes specifying a reference
   spheroid. */
         if( isgeod && astXmlGetNattr( scan->el[0][0] ) > 0 ) {
            Report( this, elem, WARNING, "contains reference spheroid "
                    "(unsupported by AST - default values will be used)", status );
         }
      }

/* Check that the spatial axes are longitude/latitude */
      if( strcmp( "SPHERICAL", astXmlGetName( scan->el[3][0] ) ) ){
         Report( this, elem, FAILURE, "contains non-spherical spatial "
                 "coords (currently unsupported by AST)", status );

      } else if( !ignore_h && AttrValueI( this, scan->el[3][0], "coord_naxes", 2, status ) != 2 ) {
         Report( this, elem, FAILURE, "contains 3D spherical spatial "
                 "coords (currently unsupported by AST)", status );

      } else if( AttrValueB( this, scan->el[3][0], "coord_vel", 0, status ) ) {
         Report( this, elem, FAILURE, "contains velocity coords", status );

/* Now check for the supported sky coordinate systems and translate to the
   equivalent AST value. Note if the system needs an equinox to qualify it. */
      } else if( !strcmp( sys, "GALACTIC_II" ) ){
         sys = "GALACTIC";
         need_eq = 0;

      } else if( !strcmp( sys, "SUPER_GALACTIC" ) ){
         sys = "SUPERGALACTIC";
         need_eq = 0;

      } else if( !strcmp( sys, "HEE" ) ){
         sys = "HELIOECLIPTIC";
         need_eq = 0;

      } else if( !strcmp( sys, "FK4" ) ) {
         sys = "FK4";
         need_eq = 1;

      } else if( !strcmp( sys, "FK5" ) ) {
         sys = "FK5";
         need_eq = 1;

      } else if( !strcmp( sys, "ECLIPTIC" ) ) {
         sys = "ECLIPTIC";
         need_eq = 1;

      } else if( isgeoc ) {
         dom = "GEO_C";
         sys = "UNKNOWN";
         need_eq = 0;

      } else if( isgeod ) {
         dom = "GEO_D";
         sys = "UNKNOWN";
         need_eq = 0;

      } else {
         sys = "ICRS";
         need_eq = 0;
      }

/* Extract the equinox if required. */
      if( need_eq ) {
         el = FindElement( this, scan->el[0][0], "Equinox", status );
         if( el ) {
            eq = astXmlGetValue( el, 0 );
            if( !eq ) Report( this, scan->el[0][0], WARNING, "contains an "
                              "<Equinox> element which is not simply "
                              "character data. The AST default (B1950 "
                              "or J2000) will be used", status );
         } else {
            eq = NULL;
            Report( this, scan->el[0][0], WARNING, "contains no <Equinox> element. "
                              "The AST default (B1950 or J2000) will be used", status );
         }

      } else {
         eq = NULL;
      }

/* Create a suitable SkyFrame. */
      new = (AstObject *) astSkyFrame( "system=%s", status, sys);
      if( eq ) astSetC( new, "Equinox", eq );
      if( dom ) astSetDomain( new, dom );

      if( isgeod ){
         astSetLabel( new, 0, "Geodetic longitude" );
         astSetLabel( new, 1, "Geodetic latitude" );

      } else if( isgeoc ){
         astSetLabel( new, 0, "Geocentric longitude" );
         astSetLabel( new, 1, "Geocentric latitude" );
      }

/* If the SpaceFrame has a <Name> element use it as the Frame title. */
      if( scan->count[2] ) astSetTitle( new, astXmlGetValue( scan->el[2][0], 0 ) );

/* Free resources */
      scan = FreeIVOAScan( scan, status );

   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Object. */
   return (AstObject *) new;
}

static AstSystemType SpecSys( AstXmlChan *this, AstXmlElement *elem,
                              const char *unit, int report, int *status ) {
/*
*  Name:
*     SpecSys

*  Purpose:
*     Determine the spectral system described by a given units string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstSystemType SpecSys( AstXmlChan *this, AstXmlElement *elem,
*                            const char *unit, int report, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function determines the spectral system described by a given units
*     string. It optionally reports an error if the string is not
*     recognised.

*  Parameters:
*     this
*        Pointer to the XmlChan. Only used if "report" is non-zero.
*     elem
*        Pointer to the IVOA element to which the unit relates. Only used
*        if "report" is non-zero.
*     unit
*        Pointer to the units string.
*     report
*        If non-zero, then a failure is reported if the spectral system
*        cannot be determined from the supplied string.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The spectral system, or AST__BADSYSTEM if an error occurs.

*/

/* Local Variables: */
   AstMapping *map;         /* Mapping from supplied unit to default unitl */
   AstSystemType sys;       /* System value corresponding to "unit" */
   char buff[200];          /* Buffer for failure message */

/* Initialise */
   sys = AST__BADSYSTEM;

/* Check inherited status */
   if( !astOK ) return sys;

/* See if a Mapping can be found from the supplied units to "Hz". If
   so, the supplied units are assumed to describe frequency. */
   map = astUnitMapper( unit, "Hz", NULL, NULL );
   if( map ) {
      sys = AST__FREQ;

/* Otherwise, see if a Mapping can be found from the supplied units to
   "m" (metre). If so, the supplied units are assumed to describe wavelength. */
   } else {
      map = astUnitMapper( unit, "m", NULL, NULL );
      if( map ) {
         sys = AST__WAVELEN;

/* Otherwise, see if a Mapping can be found from the supplied units to
   "J" (Joule). If so, the supplied units are assumed to describe energy. */
      } else {
         map = astUnitMapper( unit, "J", NULL, NULL );
         if( map ) {
            sys = AST__ENERGY;

/* Otherwise, see if a Mapping can be found from the supplied units to
   "m^-1" (per metre). If so, the supplied units are assumed to describe
   wave number. */
         } else {
            map = astUnitMapper( unit, "m^-1", NULL, NULL );
            if( map ) {
               sys = AST__WAVENUM;

/* Otherwise, report an error if requested. */
            } else if( report ){
               sprintf( buff, "contains unsupported spectral units \"%s\"", unit );
               Report( this, elem, FAILURE, buff, status );
            }
         }
      }
   }

/* Free resources */
   if( map ) map = astAnnul( map );

/* Return the result. */
   return sys;
}

static AstRegion *SpectralReader( AstXmlChan *this, AstXmlElement *elem,
                                  AstFrame *frm, double *rf,
                                  AstKeyMap **anc, int *status ){
/*
*  Name:
*     SpectralReader

*  Purpose:
*     Modify a Frame to take account of an STC <Spectral> element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *SpectralReader( AstXmlChan *this, AstXmlElement *elem,
*                                AstFrame *frm, double *rf,
*                                AstKeyMap **anc, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the supplied STC <Spectral> element, and uses it,
*     if possible, to create the uncertainty associated with the spectral
*     axis in the supplied Frame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Spectral element.
*     frm
*        Pointer to the 1D spectral Frame.
*     rf
*        Point to double in which to return the rest frequency to be used
*        with any redshift axis. Value is returned in Hz. AST__BAD will
*        be returned if no rest frequency is found.
*     anc
*        Address of a location at which to put a pointer to a newly
*        created KeyMap. This KeyMap will contain ancillary information
*        from the Spectral. The keys identify the item of ancillary
*        information (Name, Value, Error, Resolution, Size, Pixel Size).
*        The value associated with the Name key is string containing
*        the Name item from the Spectral. The value associated with each of
*        the other keys is a pointer to a 1D Region within the supplied
*        Frame, corresponding to the value, error, resolution, etc. Keys
*        will not be present in the returned KeyMap if the corresponding
*        item of ancillary information is not present in the Spectral. A
*        NULL pointer is returned if there is no ancillary information at all.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The uncertainty Region, or NULL if the supplied Spectral element
*     does not specify an uncertainty.

*/

/* Local Variables: */
   AstFrameSet *fs;         /* FrameSet connecting "sf1" and "sf2" */
   AstMapping *map;         /* Mapping from <Spectral> Frame to supplied Frame */
   AstRegion *r2;           /* Region mapped into returned Frame */
   AstRegion *r3;           /* Simplified Region mapped into returned Frame */
   AstRegion *r;            /* Original Region */
   AstRegion *result;       /* Returned uncertainty Region */
   AstSpecFrame *sf1;       /* SpecFrame describing value element */
   AstSpecFrame *sf2;       /* SpecFrame describing returned "rf" value */
   AstSystemType fsys;      /* Spectral system from supplied Stc */
   IVOAScan *scan;          /* Structure holding scan results */
   const char *name;        /* Pointer to XML element name */
   const char *names[6];    /* Names of the subelements to be searched for */
   const char *unit;        /* Pointer to Spectral's unit attribute string */
   double lbnd[ 1 ] ;       /* Lower interval bounds */
   double ubnd[ 1 ] ;       /* Upper interval bounds */
   double tmp;              /* Mapped value */
   double v;                /* Axis value */
   int max[6];              /* Max allowed occurrences of each name */
   int min[6];              /* Min allowed occurrences of each name */

/* Initialise */
   result = NULL;
   *rf = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Name";
   names[ 1 ] = "Error";
   names[ 2 ] = "Value";
   names[ 3 ] = "Resolution";
   names[ 4 ] = "Size";
   names[ 5 ] = "PixSize";
   max[ 0 ] = 1;
   max[ 1 ] = 2;
   max[ 2 ] = 1;
   max[ 3 ] = 2;
   max[ 4 ] = 2;
   max[ 5 ] = 2;
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   min[ 4 ] = 0;
   min[ 5 ] = 0;
   scan = ScanIVOAElement( this, elem, 6, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a KeyMap to return holding ancilary info, and put the Name into
   it. */
      *anc = astKeyMap( "", status );
      if( scan->count[0] > 0 ) astMapPut0C( *anc, AST__STCNAME,
                                  astXmlGetValue( scan->el[0][0], 0 ), NULL );

/* The values represented by the <Spectral> element may not be in the same
   system,units, etc as the supplied SpecFrame. We will need to be able to
   convert from one to the other, so create a SpecFrame describing the
   system and units used by the <Spectral> element. If the element does not
   have a unit attribute, assume the values are in the supplied SpecFrame
   system and units. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( unit ) {
         sf1 = astCopy( frm );
         fsys = SpecSys( this, elem, unit, 1, status );
         astSetSystem( sf1, fsys );
         astSetUnit( sf1, 0, unit );

/* If the supplied Frame did not have any set System, use the values from
   the <Spectral> Frame. */
         if( !astTestSystem( frm ) ) {
            astSetSystem( frm, fsys );
            astSetUnit( frm, 0, unit );
         } else if( astGetSystem( frm ) == fsys && !astTestUnit( frm, 0 ) ) {
            astSetUnit( frm, 0, unit );
         }

      } else {
         sf1 = astClone( frm );
      }

/* Find the Mapping from Spectral value to the supplied SpecFrame value */
      fs = astConvert( sf1, frm, "" );
      if( fs ) {
         map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         fs = astAnnul( fs );
      } else {
         map = NULL;
         Report( this, elem, FAILURE, "connot convert AstroCoords "
                 "spectral values to the required spectral system", status );
      }

/* If this Spectral contains a frequency Value which can be read, obtain
   it. We will use the value to calculate the returned rest frequency. */
      if( scan->count[ 2 ] > 0 ) {
         name = astXmlGetName( scan->el[ 2 ][ 0 ] );
         if( name && !strcmp( name, "Value" ) ) {
            v = ElemValueD( this, scan->el[ 2 ][ 0 ], AST__BAD, status );

/* Convert the value into the supplied SpecFrame system. Create an
   Interval describing it and store it in the returned ancillary keyMap.
   Use an Interval rather than a PointList since an Interval can be used
   within a Prism to extrude another Region, but a PointList cannot. */
            astTran1( map, 1, &v, 1, &tmp );
            r = (AstRegion *) astInterval( frm, &tmp, &tmp, NULL, "", status ) ;
            astMapPut0A( *anc, AST__STCVALUE, r, NULL );
            r = astAnnul( r );

/* We also want the rest frequency in Hz. Create a SpecFrame describing Hz. */
            sf2 = astCopy( sf1 );
            astSet( sf2, "system=freq,unit=Hz", status );

/* Find the Mapping from the supplied value to frequency in Hz. Use it to
   convert the rf value into Hz. */
            fs = astConvert( sf1, sf2, "" );
            if( fs ) {
               astTran1( fs, 1, &v, 1, rf );
               fs = astAnnul( fs );
            } else if( astOK ) {
               Report( this, elem, FAILURE, "Cannot convert spectral value"
                         "to frequency in Hz.", status );
            }
            sf2 = astAnnul( sf2 );
         }
      }

/* Check for Error values in the Spectral. */
      if( scan->count[ 1 ] > 0 ) {

/* Issue a warning if more than 1 Error value was found. */
         if( scan->count[ 1 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Error>"
                    " element. AST can only use the first", status );
         }

/* Get the first Error value. */
         v = ElemValueD( this, scan->el[1][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an error bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Spectral element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            result = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCERROR, result, NULL );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for Resolution values in the Spectral. */
      if( scan->count[ 3 ] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 3 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Resolution>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[3][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Spectral element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCRES, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for Size values in the Spectral. */
      if( scan->count[ 4 ] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 4 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Size>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[4][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Spectral element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCSIZE, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Check for PixSize values in the Spectral. */
      if( scan->count[ 5] > 0 ) {

/* Issue a warning if more than 1 value was found. */
         if( scan->count[ 5 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <PixSize>"
                    " element. AST can only use the first", status );
         }

/* Get the first value. */
         v = ElemValueD( this, scan->el[5][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an interval centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the Frame represented by the Spectral element.
   Map it into the supplied Frame. Simplify it. Store in the returned
   ancillary KeyMap. */
            r = (AstRegion *) astInterval( sf1, lbnd, ubnd, NULL, "", status );
            r2 = astMapRegion( r, map, frm );
            r3 = astSimplify( r2 );
            astMapPut0A( *anc, AST__STCPIXSZ, r3, NULL );
            r3 = astAnnul( r3 );
            r2 = astAnnul( r2 );
            r = astAnnul( r );
         }
      }

/* Free resources. */
      scan = FreeIVOAScan( scan, status );
      sf1 = astAnnul( sf1 );
      map = astAnnul( map );
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;

}

static AstObject *SpectralFrameReader( AstXmlChan *this,
                                       AstXmlElement *elem, int *status ) {
/*
*  Name:
*     SpectralFrameReader

*  Purpose:
*     Make an AST Object from an IVOA SpectralFrame element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *SpectralFrameReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     SpectralFrame element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA SpectralFrame element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.

*/

/* Local Variables: */
   AstSpecFrame *new;            /* Pointer to returned Object */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[2];         /* Names of the subelements to be searched for */
   const char *sor;              /* StdOfRest for returned Frame */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return (AstObject *) new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "TOPOCENTER|BARYCENTER|HELIOCENTER|GEOCENTER|LSR|"
                "LSRK|GALACTIC_CENTER|LOCAL_GROUP|LSRD";
   names[ 1 ] = "Name";
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the name of the Element specifying the reference position and find
   the corresponding AST name.*/
      sor = astXmlGetName( scan->el[0][0] );
      if( !strcmp( sor, "TOPOCENTER" ) ) {
         sor = "Topo";
      } else if( !strcmp( sor, "BARYCENTER" ) ){
         sor = "Bary";
      } else if( !strcmp( sor, "GEOCENTER" ) ){
         sor = "Geo";
      } else if( !strcmp( sor, "LSR" ) || !strcmp( sor, "LSRK" ) ) {
         sor = "LSRK";
      } else if( !strcmp( sor, "LSRD" ) ) {
         sor = "LSRD";
      } else if( !strcmp( sor, "GALACTIC_CENTER" ) ) {
         sor = "Galactic";
      } else if( !strcmp( sor, "LOCAL_GROUP" ) ) {
         sor = "Local_group";
      } else if( !strcmp( sor, "HELIOCENTER" ) ) {
         sor = "Helio";
      } else if( astOK ){
         astError( AST__INTER, "SpectralFrameReader(XmlChan): Unknown "
                   "standard of rest %s (internal AST programming error).", status,
                   sor );
      }

/* Issue a warning if the reference position includes an ephemeris. */
      if( FindElement( this, scan->el[0][0], "PlanetaryEphem", status ) ) {
         Report( this, scan->el[0][0], WARNING, "contains a <PlanetaryEphem> "
                 "element which will be ignored", status );
      }

/* Create a suitable SpecFrame. */
      new = astSpecFrame( "StdOfRest=%s", status, sor);

/* If the SpectralFrame has a <Name> element use it as the SpecFrame title. */
      if( scan->count[1] ) astSetTitle( new, astXmlGetValue( scan->el[1][0], 0 ) );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Object. */
   return (AstObject *) new;
}

static AstRegion *SpectralIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                          AstFrame *frm, int *status ){
/*
*  Name:
*     SpectralIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA SpectralInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *SpectralIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                        AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     SpectralInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA SpectralInterval element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit or System attribute is not set, this
*        function will decide on the values to be used, and set these
*        values in the supplied Frame before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstRegion *new;               /* Pointer to returned Region */
   AstSystemType fsys;           /* System value from supplied Frame */
   AstSystemType sys;            /* System value corresponding to "unit" */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   char *title;                  /* Title string */
   const char *unit;             /* Unit string from supplied element */
   double hilimit;               /* Upper spectral limit */
   double lolimit;               /* Lower spectral limit */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "LoLimit";
   names[ 1 ] = "HiLimit";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      lolimit = scan->count[0] ? ElemValueD( this, scan->el[0][0], 0.0, status ) : AST__BAD;
      hilimit = scan->count[1] ? ElemValueD( this, scan->el[1][0], 0.0, status ) : AST__BAD;

/* Get the Unit attribute from the element. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";

/* Find the spectral system corresponding to these units. */
      } else {
         sys = SpecSys( this, elem, unit, 1, status );

/* Take a copy of the supplied Frame and set its System and Units to
   these values. Ensure the title is preserved. */
         cfrm = astCopy( frm );
         if( astTestTitle( frm ) ) {
            title = (char *) astGetTitle( frm );
            if( title ) title = astStore( NULL, title, strlen( title ) + 1 );
         } else {
            title = NULL;
         }
         astSetSystem( cfrm, sys );
         astSetUnit( cfrm, 0, unit );
         if( title ) astSetTitle( cfrm, title );

/* If at least one limit was found, create an Interval within this
   modified Frame. Otherwise create a negated NullRegion. */
         if( lolimit != AST__BAD || hilimit != AST__BAD ) {
            new = (AstRegion *) astInterval( cfrm, &lolimit, &hilimit, NULL, "", status );
         } else {
            new = (AstRegion *) astNullRegion( cfrm, NULL, "negated=1", status );
         }

/* If the System of this Region differs from that of the supplied Frame,
   set it to the System of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new System. If the supplied
   Frame had no set system, set it to the system implied by th eunits in the
   supplied XML element. */
         if( astTestSystem( frm ) ) {
            fsys = astGetSystem( frm );
            if( fsys != sys ) astSetSystem( new, fsys );
         } else {
            astSetSystem( frm, sys );
         }

/* Do the same with the Units. */
         if( astTestUnit( frm, 0 ) ) {
            funit = astGetUnit( frm, 0 );
            if( strcmp( funit, unit ) ) astSetUnit( new, 0, funit );
         } else {
            astSetUnit( frm, 0, unit );
         }

/* Ensure the original titleis preserved. */
         if( title ) {
            astSetTitle( new, title );
            astSetTitle( frm, title );
         }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
         FillAndLims( this, elem, new, status );

/* Free resources */
         cfrm = astAnnul( cfrm );
         title = astFree( title );
      }

      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *SphereReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     SphereReader

*  Purpose:
*     Make an AST Region from an IVOA Sphere element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *SphereReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Sphere element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Sphere element.
*     frm
*        Pointer to the Frame in which the returned Region should be
*        defined. If the Unit or System attribute is not set, this
*        function will decide on the values to be used, and set these
*        values in the supplied Frame before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstFrame *cfrm;               /* Frame used to define returned Region */
   AstMapping *map;              /* Mapping between units */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   char buff[200];               /* Message buffer */
   const char *funit;            /* Unit string from supplied Frame */
   const char *names[2];         /* Names of the subelements to be searched for */
   const char *runit;            /* Radius unit string from supplied element */
   const char *unit;             /* Centre unit string from supplied element */
   double cen[3];                /* Centre */
   double rad;                   /* Radius */
   double tmp;                   /* New radius value */
   int i;                        /* Axis count */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Check the supplied Frame has the correct number of axes. */
   if( astGetNaxes( frm ) != 3 && astOK ) {
      astError( AST__INTER, "SphereReader(XmlChan): Supplied "
                "Frame does not have 3 axes (internal AST programming error )." , status);
   }

/* Scan the supplied element for the required sub-elements */
   names[ 0 ] = "Radius";
   names[ 1 ] = "Center";
   min[ 0 ] = 1;
   min[ 1 ] = 1;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the radius. */
      rad = ElemValueD( this, scan->el[0][0], 0.0, status );

/* Get the centre. */
      cen[0] = 0.0;
      cen[1] = 0.0;
      cen[2] = 0.0;
      ElemListD( this, scan->el[1][0], 3, cen, status );

/* Get the units attribute from the supplied element. This applies to the
   values describing the centre position. */
      unit = astXmlGetAttributeValue( elem, "unit" );
      if( !unit ) {
         Report( this, elem, FAILURE, "contains no unit attribute", status );
         unit = "";
      }

/* Get the radius units attribute from the supplied element. */
      runit = astXmlGetAttributeValue( elem, "radius_unit" );

/* If necessary, convert the radius to the same units as the centre. */
      if( runit && strcmp( unit, runit ) ) {
         map = astUnitMapper( runit, unit, NULL, NULL );
         if( map ) {
            astTran1( map, 1, &rad, 1, &tmp );
            rad = tmp;
            map = astAnnul( map );

         } else if( astOK ) {
            sprintf( buff, "has inconsistent units attributes \"%s\" and "
                     "\"%s\"", unit, runit );
            Report( this, elem, FAILURE, buff, status );
         }
      }

/* Take a copy of the supplied Frame and set its Units to the value
   obtained from the supplied element. */
      cfrm = astCopy( frm );
      astSetUnit( cfrm, 0, unit );
      astSetUnit( cfrm, 1, unit );
      astSetUnit( cfrm, 2, unit );

/* Create a Circle within this modified Frame. */
      new = (AstRegion *) astCircle( cfrm, 1, cen, &rad, NULL, "", status );

/* If the Unit of this Region differs from that of the supplied Frame,
   set it to the Unit of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new Unit. If the supplied
   Frame had no set Unit, set it to the units obtained from the supplied
   element. */
      for( i = 0; i < 3; i++ ) {
         if( astTestUnit( frm, i ) ) {
            funit = astGetUnit( frm, i );
            if( strcmp( funit, unit ) ) astSetUnit( new, i, funit );
         } else {
            astSetUnit( frm, i, unit );
         }
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      cfrm = astAnnul( cfrm );
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstObject *StcMetadataReader( AstXmlChan *this,
                                     AstXmlElement *elem, int *status ) {
/*
*  Name:
*     StcMetadataReader

*  Purpose:
*     Make an AST Object from an IVOA STCMetadata element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *StcMetadataReader( AstXmlChan *this,
*                                   AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     STCMetadata element. The STCMetadata object can be of any subclass
*     (e.g. STCResourceProfile, SearchLocation, CatalogEntryLocation,
*     ObservationLocation, ObservatoryLocation).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA STCMetadata element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstFrame *frm;                /* Frame representing the STC object */
   AstKeyMap *anc;               /* Map holding AstroCoords ancillary data */
   AstKeyMap **ancs;             /* List of KeyMaps holding ancillary data */
   AstKeyMap *map1;              /* Map holding AstroCoordSystem elements */
   AstKeyMap *map2;              /* Map holding AstroCoordArea elements */
   AstKeyMap *map3;              /* Map holding CoordSpec elements */
   AstKeyMap *map;               /* Map to use */
   AstRegion *region;            /* Region representing the STC object */
   AstRegion *tuncs[ 4 ];        /* Temporary uncertainty Regions */
   AstRegion *uncs[ 4 ];         /* Uncertainty Regions for returned STC */
   AstStc *stc;                  /* Pointer to returned Object (an Stc) */
   AstXmlContentItem *item;      /* Pointer to content item */
   AstXmlElement *aca;           /* Pointer to AstroCoordArea element to use */
   AstXmlElement *aco;           /* Pointer to AstroCoords element to use */
   AstXmlElement *acs;           /* Pointer to AstroCoordSystem element to use */
   char *text;                   /* Formatted item text */
   char buff[ 200 ];             /* Message buffer */
   const char *id;               /* Value of ID attribute */
   const char *ido;              /* Value of ID attribute */
   const char *name;             /* Element name */
   const char *stc_class;        /* STC subclass name */
   int gotunc;                   /* Have any uncertainty Regions been obtained? */
   int i;                        /* Index of content item within element */
   int j;                        /* Index into list of map keys */
   int narea;                    /* Number of AstroCoordArea elements found */
   int ncoord;                   /* Number of CoordSpec elements found */
   int nanc;                     /* No.of KeyMaps in "ancs" array */
   int nitem;                    /* No. of items of content in element */
   int nsys;                     /* Number of AstroCoordSystem elements found */
   int reported;                 /* Have multiple uncertainies been reported? */
   int used;                     /* Was the content item used? */

/* Initialise. */
   stc = NULL;

/* Check the global error status. */
   if ( !astOK ) return (AstObject *) stc;

/* Avoid compiler warnings. */
   id = "";

/* Get name of the the STCMetadata subclass represented by the supplied
   element. */
   stc_class = astXmlGetName( elem );

/* Create KeyMaps to hold the required sub-elements. We will store the
   integer indices of the requried elements in these keymaps, using the
   associated Xml ID attribute values as the keys. */
   map1 = astKeyMap( "", status );
   map2 = astKeyMap( "", status );
   map3 = astKeyMap( "", status );

/* Loop round all items in the elements contents. */
   nitem = astXmlGetNitem( elem );
   for( i = 0; i < nitem; i++ ) {
      item = astXmlGetItem( elem, i );
      used = 1;

/* Ignore this item if it is not an element. */
      if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* Choose the KeyMap in which to save this item. */
         name = astXmlGetName( item );
         if( !strcmp( name, ASTRO_COORD_SYSTEM ) ){
            map = map1;

         } else if( !strcmp( name, ASTRO_COORD_AREA ) ){
            map = map2;

         } else if( !strcmp( name, ASTRO_COORDS ) ){
            map = map3;

         } else {
            map = NULL;
            used = 0;
         }

/* If we are going to save the item, get the value of the ID attribute
   and check it. */
         if( map ) {
            id = astXmlGetAttributeValue( (AstXmlElement *) item, "ID" );
            if( !id ) {
               id = "";
               if( map != map3 ) {
                  Report( this, elem, WARNING, "has no ID attribute. Assuming"
                          "a null ID value", status );
               }
            }

/* If the KeyMap already contains an object with this ID, issue a
   warning and skip the element. */
            if( astMapHasKey( map, id ) ) {
               if( map != map3 ) {
                  sprintf( buff, "contains two or more %s elements with the "
                           "same ID (\"%s\"). Only the first one will be used",
                           name, id );
                  Report( this, elem, WARNING, buff, status );
               } else {
                  Report( this, elem, WARNING, "contains two or more AstroCoords "
                          "elements. Only the first one will be used", status );
               }

/* Otherwise, save the index of the item in the KeyMap, using the ID as the
   key. */
            } else {
               astMapPut0I( map, id, i, "" );
            }
         }

      } else {
         used = 0;
      }

/* If this content item was not used, issue a warning unless it is a comment
   or white space. */
      if( !used && !astXmlCheckType( item, AST__XMLCOM ) &&
                   !astXmlCheckType( item, AST__XMLWHITE ) ) {
         text = (char *) astXmlFormat( item );
         if( strlen( text ) > 30 ) text[ 30 ] = 0;
         sprintf( buff, "contains the following which is being ignored: \"%s\"",
                  text );
         text = astFree( text );
         Report( this, elem, WARNING, buff, status );
      }

   }

/* Note the number of each type of element found with unique ID values. */
   nsys = astMapSize( map1 );
   narea = astMapSize( map2 );
   ncoord = astMapSize( map3 );

/* If any CoordArea elements were found, find the first one for which the
   coordesponding AstroCoordSystem is available. */
   acs = NULL;
   aca = NULL;
   for( j = 0; j < narea; j++ ) {

/* Get the j'th key from "map2" (the ID associated with the j'th
   AstroCoordArea found in the supplied element) and retrieve the value
   associated with this key (the index "i" into the content of the
   supplied element at which the j'th AstroCoordArea is stored). */
      astMapGet0I( map2, astMapKey( map2, j ), &i );

/* Get the i'th element in the supplied element. This will be the j'th
   AstroCoordArea. */
      aca = (AstXmlElement *) astXmlGetItem( elem, i );

/* Get the "coord_system_id" attribute from this AstroCoordArea. Use null
   if not available. */
      id = astXmlGetAttributeValue( aca, "coord_system_id" );
      if( !id ) {
         id = "";
         Report( this, aca, WARNING, "has no coord_system_id attribute. "
                 "Assuming a null coord_system_id value", status );
      }

/* Get the index within the supplied element of the AstroCoordSystem with this
   ID. Jump forward if no AstroCoordSystem with this id is available. */
      if( astMapGet0I( map1, id, &i ) ) {

/* Get a pointer to the AstroCoordSystem element with the required ID. */
         acs = (AstXmlElement *) astXmlGetItem( elem, i );

/* Leave the AstroCoordArea loop. */
         break;

/* Report a warning if no AstroCoordSystem with this id is available. */
      } else {
         sprintf( buff, "refers to an AstroCoordSystem with "
                 "ID \"%s\", but no such AstroCoordSystem is available "
                 "within the parent %s", id, stc_class );
         Report( this, aca, WARNING, buff, status );
      }
   }

/* If we did not find a corresponding pair of AstroCoordSystem and
   AstroCoordArea, we either report a failure (if there were any
   AstroCoordAreas), or get a pointer the AstroCoordSystem referred to by
   the first AstroCoords element (we will create a Frame from this later). */
   if( !acs ) {
      aca = NULL;

/* Report a warning if there were some AstroCoordArea tags but no matching
   AstroCoordSystem was found. */
      if( narea > 0 ) {
         Report( this, elem, WARNING, "does not contain a pair of "
                 "matching AstroCoordArea and AstroCoordSystem tags", status );

/* If there are no AstroCoordAreas in the supplied element, look for a
   pair of matching AstroCoords and AstroCoordSystem. The returned Region
   will represent a NullRegion within this system. */
      } else if( ncoord > 0 ) {

/* Get the 1st key from "map3" (the ID associated with the 1st
   AstroCoords found in the supplied element) and retrieve the value
   associated with this key (the index "i" into the content of the
   supplied element at which the 1st AstroCoords is stored). */
         astMapGet0I( map3, astMapKey( map3, 0 ), &i );

/* Get the i'th element in the supplied element. This will be the 1st
   AstroCoord. */
         aco = (AstXmlElement *) astXmlGetItem( elem, i );

/* Get the "coord_system_id" attribute from this AstroCoords. Use null
   if not available. */
         id = astXmlGetAttributeValue( aco, "coord_system_id" );
         if( !id ) {
            id = "";
            Report( this, aco, WARNING, "has no coord_system_id attribute. "
                    "Assuming a null coord_system_id value", status );
         }

/* Get the index within the supplied element of the AstroCoordSystem with this
   ID. Jump forward if no AstroCoordSystem with this id is available. */
         if( astMapGet0I( map1, id, &i ) ) {

/* Get a pointer to the AstroCoordSystem element with the required ID. */
            acs = (AstXmlElement *) astXmlGetItem( elem, i );

         } else {
            Report( this, aco, FAILURE, "no corresponding AstroCoordSystem found", status );
         }

/* If there are no AstroCoords in the supplied element we create a
   NullRegion within the first supplied AstroCoordSystem. */
      } else if( nsys > 0 ) {
         if( astMapGet0I( map1, astMapKey( map1, 0 ), &i ) ) {
            acs = (AstXmlElement *) astXmlGetItem( elem, i );
         }

      } else {
         Report( this, elem, FAILURE, "no usable content found", status );
      }
   }

/* Report failure if we still have no AstroCoordSystem. */
   if( !acs ) {
      Report( this, elem, FAILURE, "does not contain a usable AstroCoordSystem", status );

/* Issue a warning if more than one AstroCoordArea was found. */
   } else {
      if( narea > 1 ) Report( this, elem, WARNING, "contains more than one "
                             "AstroCoordArea. Only one will be used", status );

/* Create a Frame from the ASTRO_COORD_SYSTEM. */
      frm = (AstFrame *) AstroCoordSystemReader( this, acs, status );

/* Loop round all AstroCoords elements in the supplied element. */
      gotunc = 0;
      reported = 0;
      uncs[ 0 ] = NULL;
      uncs[ 1 ] = NULL;
      uncs[ 2 ] = NULL;
      uncs[ 3 ] = NULL;
      nanc = 0;
      ancs = NULL;
      for( j = 0; j < ncoord; j++ ) {

/* Get the j'th key from "map3" (the ID associated with the j'th
   AstroCoords found in the supplied element) and retrieve the value
   associated with this key (the index "i" into the content of the
   supplied element at which the j'th AstroCoords is stored). */
         astMapGet0I( map3, astMapKey( map3, j ), &i );

/* Get the i'th element in the supplied element. This will be the j'th
   AstroCoords. */
         aco = (AstXmlElement *) astXmlGetItem( elem, i );

/* Get the "coord_system_id" attribute from this AstroCoords and compare it
   with the ID of the AstrocCoordSys being used. If they match, incorporate
   the effects of the AstroCoords into the "frm" Frame and get a set of 4
   Regions representing the uncertainty within each of the 4 STC domains
   (space, time, spectral, redshift). */
         ido = astXmlGetAttributeValue( aco, "coord_system_id" );
         if( ido && !strcmp( id, ido ) ) {
            if( AstroCoordsReader( this, aco, frm, tuncs, &anc, status ) ) {
               if( !gotunc ) {
                  uncs[ 0 ] = tuncs[ 0 ];
                  uncs[ 1 ] = tuncs[ 1 ];
                  uncs[ 2 ] = tuncs[ 2 ];
                  uncs[ 3 ] = tuncs[ 3 ];
                  gotunc = 1;
               } else {
                  if( tuncs[ 0 ] ) tuncs[ 0 ] = astAnnul( tuncs[ 0 ] );
                  if( tuncs[ 1 ] ) tuncs[ 1 ] = astAnnul( tuncs[ 1 ] );
                  if( tuncs[ 2 ] ) tuncs[ 2 ] = astAnnul( tuncs[ 2 ] );
                  if( tuncs[ 3 ] ) tuncs[ 3 ] = astAnnul( tuncs[ 3 ] );
                  if( !reported ) {
                     Report( this, elem, WARNING, "contains more than one "
                             "specification of the coordinate uncertainties. "
                             "Only the first will be used", status );
                     reported= 1;
                  }
               }
            }

/* If any ancillary information was read from the AstroCoords, add it to
   the list of ancillary information to be stored in the Stc structure. */
            if( anc ) {
               ancs = astGrow( ancs, nanc + 1, sizeof( AstKeyMap * ) );
               if( ancs ) ancs[ nanc++ ] = anc;
            }
         }
      }

/* Now create a Region from this Frame and the ASTRO_COORD_AREA. Note,
   "aca" may be NULL in which case the returned Region will be NullRegion. */
      region = AstroCoordAreaReader( this, aca, frm, uncs, nanc, ancs, status );

/* Re-centre the Regions describing ancillary information extracted from
   the AstroCoords elements. */
      ReCentreAnc( region, nanc, ancs, status );

/* Now create a Stc object of the appropriate sub-class. */
      if( !strcmp( stc_class, STC_RESOURCE_PROFILE ) ) {
         stc = (AstStc *) astStcResourceProfile( region, nanc, ancs, "", status );

      } else if( !strcmp( stc_class, SEARCH_LOCATION ) ) {
         stc = (AstStc *) astStcSearchLocation( region, nanc, ancs, "", status );

      } else if( !strcmp( stc_class, CATALOG_ENTRY_LOCATION ) ) {
         stc = (AstStc *) astStcCatalogEntryLocation( region, nanc, ancs, "", status );

      } else if( !strcmp( stc_class, OBSERVATION_LOCATION ) ||
                 !strcmp( stc_class, OBSERVATORY_LOCATION ) ) {
         stc = (AstStc *) astStcObsDataLocation( region, nanc, ancs, "", status );

      } else if( astOK ){
         astError( AST__INTER, "astRead(XmlChan): StcMetadataReader knows "
                   "nothing about the %s class (internal AST programming "
                   "error).", status, stc_class );
      }

/* Get the ID attribute from the supplied element and store in the
   returned Object. */
      id = astXmlGetAttributeValue( elem, "ID" );
      if( id ) astSetIdent( stc, id );

/* Free resources. */
      if( uncs[ 0 ] ) uncs[ 0 ] = astAnnul( uncs[ 0 ] );
      if( uncs[ 1 ] ) uncs[ 1 ] = astAnnul( uncs[ 1 ] );
      if( uncs[ 2 ] ) uncs[ 2 ] = astAnnul( uncs[ 2 ] );
      if( uncs[ 3 ] ) uncs[ 3 ] = astAnnul( uncs[ 3 ] );
      frm = astAnnul( frm );
      region = astAnnul( region );
      if( ancs ) {
         for( i = 0; i < nanc; i++ ) ancs[ i ] = astAnnul( ancs[ i ] );
         ancs = astFree( ancs );
      }
   }

   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );
   map3 = astAnnul( map3 );

/* Return the pointer to the new Object. */
   return (AstObject *) stc;
}

static AstRegion *StcRegionReader( AstXmlChan *this, AstXmlElement *elem,
                                AstFrame *frm, int *status ){
/*
*  Name:
*     StcRegionReader

*  Purpose:
*     Make an AST Region from an IVOA Region element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *StcRegionReader( AstXmlChan *this, AstXmlElement *elem,
*                              AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Region element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Region element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for a Region sub-element. */
   names[ 0 ] = "Intersection|Union|Negation|AllSky|Circle|Ellipse|Polygon|"
                "Convex|Box";
   min[ 0 ] = 1;
   max[ 0 ] = 1;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create the Region */
      new = RegionReader( this, scan->el[0][0], frm, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the astTestAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a XmlChan's attributes.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* XmlLength */
/* --------- */
   if ( !strcmp( attrib, "xmllength" ) ) {
      result = astTestXmlLength( this );

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      result = astTestXmlFormat( this );

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      result = astTestXmlPrefix( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstObject *TimeFrameReader( AstXmlChan *this,
                                   AstXmlElement *elem, int *status ) {
/*
*  Name:
*     TimeFrameReader

*  Purpose:
*     Make an AST Object from an IVOA TimeFrame element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *TimeFrameReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied IVOA
*     TimeFrame element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA TimeFrame element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.

*/

/* Local Variables: */
   AstTimeFrame *new;         /* Pointer to returned Frame */
   IVOAScan *scan;            /* Structure holding scan results */
   const char *names[3];      /* Names of the subelements to be searched for */
   const char *text;          /* Pointer to Name value */
   int max[3];                /* Max allowed occurrences of each name */
   int min[3];                /* Min allowed occurrences of each name */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return (AstObject *) new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Name";
   names[ 1 ] = "TOPOCENTER";
   names[ 2 ] = "TimeScale|Timescale";
   min[ 0 ] = 0;
   max[ 0 ] = 1;
   min[ 1 ] = 0;
   max[ 1 ] = 1;
   min[ 2 ] = 1;
   max[ 2 ] = 1;
   scan = ScanIVOAElement( this, elem, 3, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a suitable TimeFrame. Set the timescale, but leave the other
   attributes unset since they will not be known until an AstronTimeType
   is read. Except for unit. We set unit to "d" (day) because all the
   time form,ats supported by STC have "d" as the default unit. This
   avoids bad publicity which arises from presentin (say) MJD values in
   units of "s" - which people will think is wrong until they have it
   explained. */
      new = astTimeFrame( "unit=d", status );
      astSetTimeScale( new, TimeScaleReader( this, scan->el[ 2 ][ 0 ], status ) );

/* If the STC TimeFrame has a <Name> element use it as the AST TimeFrame title. */
      if( scan->count[ 0 ] > 0 ) {
         text = astXmlGetValue( scan->el[ 0 ][ 0 ], 0 );
         if( text ) astSetTitle( new, text );
      }

/* Free resources. */
      scan = FreeIVOAScan( scan, status );

   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new= astAnnul( new );

/* Return the pointer to the new Object. */
   return (AstObject *) new;
}

static AstRegion *TimeIntervalReader( AstXmlChan *this, AstXmlElement *elem,
                                      AstTimeFrame *frm, int *status ){
/*
*  Name:
*     TimeIntervalReader

*  Purpose:
*     Make an AST Region from an IVOA TimeInterval element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *TimeIntervalReader( AstXmlChan *this, AstXmlElement *elem,
*                                    AstTimeFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     TimeInterval element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA TimeInterval element.
*     frm
*        Pointer to the TimeFrame in which the returned Region should be
*        defined.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[2];         /* Names of the subelements to be searched for */
   double start;                 /* Start time */
   double stop;                  /* Stop time */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "StartTime";
   names[ 1 ] = "StopTime";
   min[ 0 ] = 0;
   min[ 1 ] = 0;
   max[ 0 ] = 1;
   max[ 1 ] = 1;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the limits. */
      start = scan->count[0] ? AstronTimeReader( this, scan->el[0][0], frm, status ) : AST__BAD;
      stop = scan->count[1] ? AstronTimeReader( this, scan->el[1][0], frm, status ) : AST__BAD;

/* If at least one limit was found, create an Interval. Otherwise create
   a negated NullRegion. */
      if( start != AST__BAD || stop != AST__BAD ) {

/* Use the stop or start time (converted to an MJD) as the Epoch within the
   Frame. */
         if( start != AST__BAD ) {
            astSetEpoch( frm, MakeMJD( frm, start, status ) );
         } else if( stop != AST__BAD ) {
            astSetEpoch( frm, MakeMJD( frm, stop, status ) );
         }
         new = (AstRegion *) astInterval( frm, &start, &stop, NULL, "", status );
      } else {
         new = (AstRegion *) astNullRegion( frm, NULL, "negated=1", status );
      }

/* Get any fill factor and lo/hi_include attributes from the element and
   assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources. */
      scan = FreeIVOAScan( scan, status );

   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static AstRegion *TimeReader( AstXmlChan *this, AstXmlElement *elem,
                              AstTimeFrame *frm, double *epoch,
                              AstKeyMap **anc, int *status ){
/*
*  Name:
*     TimeReader

*  Purpose:
*     Modify a Frame to take account of an STC <Time> element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *TimeReader( AstXmlChan *this, AstXmlElement *elem,
*                            AstTimeFrame *frm, double *epoch,
*                            AstKeyMap **anc, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads the supplied STC <Time> element, and uses it,
*     if possible, to create the uncertainty associated with the time
*     axis in the supplied Frame.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Time element.
*     frm
*        Pointer to the TimeFrame.
*     epoch
*        Pointer to double in which to return the epoch to be used
*        with other axes. Value is returned as an Modified Julian Date
*        in the barycentric dynamical timescale (TDB). AST__BAD will
*        be returned if the supplied Time element has no value.
*     anc
*        Address of a location at which to put a pointer to a newly
*        created KeyMap. This KeyMap will contain ancillary information
*        from the Time. The keys identify the item of ancillary
*        information (Name, Value, Error, Resolution, Size, Pixel Size).
*        The value associated with the Name key is string containing
*        the Name item from the Time. The value
*        associated with each of the other keys is a pointer to a 1D Region
*        within the supplied Frame, corresponding to the value, error,
*        resolution, etc. Keys will not be present in the returned KeyMap
*        if the corresponding item of ancillary information is not present
*        in the Time. A NULL pointer is returned if there is no
*        ancillary information at all.
*     status
*        Pointer to the inherited status variable.

*  Returned:
*     The uncertainty Region, or NULL if the supplied Time element
*     does not specify an uncertainty.

*/

/* Local Variables: */
   AstTimeFrame *cfrm;      /* Pointer to copy of time axis */
   AstRegion *result;       /* Returned uncertainty Region */
   AstRegion *r;            /* Ancillary Region */
   IVOAScan *scan;          /* Structure holding scan results */
   const char *funit;       /* Pointer to Frame's unit attribute string */
   const char *names[6];    /* Names of the subelements to be searched for */
   const char *title;       /* Pointer to Frame title string */
   const char *unit;        /* Pointer to Time's unit attribute string */
   double lbnd[ 1 ] ;       /* Lower interval bounds */
   double ubnd[ 1 ] ;       /* Upper interval bounds */
   double value;            /* Time value */
   double v;                /* Ancillary value */
   int max[6];              /* Max allowed occurrences of each name */
   int min[6];              /* Min allowed occurrences of each name */

/* Initialise */
   result = NULL;
   *epoch = AST__BAD;
   *anc = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Name";
   names[ 1 ] = "Error";
   names[ 2 ] = "TimeInstant";
   names[ 3 ] = "Resolution";
   names[ 4 ] = "Size";
   names[ 5 ] = "PixSize";
   max[ 0 ] = 1;
   max[ 1 ] = 2;
   max[ 2 ] = 1;
   max[ 3 ] = 2;
   max[ 4 ] = 2;
   max[ 5 ] = 2;
   min[ 0 ] = 1;
   min[ 1 ] = 0;
   min[ 2 ] = 0;
   min[ 3 ] = 0;
   min[ 4 ] = 0;
   min[ 5 ] = 0;
   scan = ScanIVOAElement( this, elem, 6, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create a KeyMap to return holding ancilary info, and put the Name into
   it. */
      *anc = astKeyMap( "", status );
      if( scan->count[0] > 0 ) astMapPut0C( *anc, AST__STCNAME,
                                  astXmlGetValue( scan->el[0][0], 0 ), NULL );

/* Get any Unit attribute from the Time element. */
      unit = astXmlGetAttributeValue( elem, "unit" );

/* We need to ensure the returned regions are mapped into units of "funit".
   If this is NULL it means that the returned regions are already in the
   required units. */
      funit = NULL;

/* If the Time element has a unit attribute, we use it in preference to any
   units values in the supplied Frame. Take a copy of the time Frame and set
   its Units to this values. Ensure the title is preserved. */
      if( unit && astChrLen( unit ) ) {
         cfrm = astCopy( frm );
         if( astTestTitle( frm ) ) {
            title = (char *) astGetTitle( frm );
            if( title ) title = astStore( NULL, title, strlen( title ) + 1 );
         } else {
            title = NULL;
         }
         astSetUnit( cfrm, 0, unit );
         if( title ) astSetTitle( cfrm, title );

         if( astTestUnit( frm, 0 ) ) {
            funit = astGetUnit( frm, 0 );
            if( !strcmp( funit, unit ) ) {
               funit = NULL;
            } else {
               funit = astStore( NULL, funit, strlen( funit ) + 1 );
            }
         } else {
            astSetUnit( frm, 0, unit );
         }

      } else {
         cfrm = astClone( frm );
         title = NULL;
      }

/* If this Time contains a Value which can be read, obtain it. Otherwise,
   issue a warning. We will use the value to calculate the returned epoch. */
      if( scan->count[ 2 ] > 0 ) {
         value = AstronTimeReader( this, scan->el[ 2 ][ 0 ], cfrm, status );
         *epoch = MakeMJD( cfrm, value, status );

/* Ensure any relevant attribute values which were set by AstronTimeReader
   within "cfrm" are transferred to "frm". */
         if( astTestTimeScale( cfrm ) ) astSetTimeScale( frm, astGetTimeScale( cfrm ) );
         if( astTestSystem( cfrm ) ) astSetSystem( frm, astGetSystem( cfrm ) );
         if( astTestUnit( cfrm, 0 ) ) astSetUnit( frm, 0, astGetUnit( cfrm, 0 ) );
         if( astTestTimeOrigin( cfrm ) ) astSetTimeOrigin( frm, astGetTimeOrigin( cfrm ) );

/* Create a Interval from it and store in the returned ancillary KeyMap. If
   the units of this Frame differs from that of the supplied Frame, set it
   to the units of the supplied Frame. This will cause the encapsulated
   limits to be mapped into the new units. Ensure the original title is
   preserved. Use an Interval rather than a PointList since an Interval
   can be used within a Prism to extrude another Region, but a PointList
   cannot. */
         r = (AstRegion *) astInterval( cfrm, &value, &value, NULL, "", status ) ;
         if( funit ) astSetUnit( r, 0, funit );
         if( title ) astSetTitle( r, title );
         astMapPut0A( *anc, AST__STCVALUE, r, NULL );
         r = astAnnul( r );
      }

/* Does this Time contain any Error? */
      if( scan->count[ 1 ] > 0 ) {

/* Issue a warning if more than 1 Error value was found. */
         if( scan->count[ 1 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Error>"
                    " element. AST can only use the first", status );
         }

/* Get the first Error value. */
         v = ElemValueD( this, scan->el[1][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of an error bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the time Frame. */
            result = (AstRegion *) astInterval( cfrm, lbnd, ubnd, NULL, "", status );

/* If the units of this Frame differs from that of the supplied Frame,
   set it to the units of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new units. */
            if( funit ) astSetUnit( result, 0, funit );

/* Ensure the original title is preserved. */
            if( title ) astSetTitle( result, title );

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCERROR, result, NULL );

         }
      }

/* Does this Time contain any Resolution? */
      if( scan->count[ 3 ] > 0 ) {

/* Issue a warning if more than 1 Resolution value was found. */
         if( scan->count[ 3 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Resolution>"
                    " element. AST can only use the first", status );
         }

/* Get the first Resolution value. */
         v = ElemValueD( this, scan->el[3][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of a bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the time Frame. */
            r = (AstRegion *) astInterval( cfrm, lbnd, ubnd, NULL, "", status );

/* If the units of this Frame differs from that of the supplied Frame,
   set it to the units of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new units. */
            if( funit ) astSetUnit( r, 0, funit );

/* Ensure the original title is preserved. */
            if( title ) astSetTitle( r, title );

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCRES, r, NULL );
            r = astAnnul( r );
         }
      }

/* Does this Time contain any Size? */
      if( scan->count[ 4 ] > 0 ) {

/* Issue a warning if more than 1 Size value was found. */
         if( scan->count[ 4 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <Size>"
                    " element. AST can only use the first", status );
         }

/* Get the first Size value. */
         v = ElemValueD( this, scan->el[4][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of a bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the time Frame. */
            r = (AstRegion *) astInterval( cfrm, lbnd, ubnd, NULL, "", status );

/* If the units of this Frame differs from that of the supplied Frame,
   set it to the units of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new units. */
            if( funit ) astSetUnit( r, 0, funit );

/* Ensure the original title is preserved. */
            if( title ) astSetTitle( r, title );

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCSIZE, r, NULL );
            r = astAnnul( r );
         }
      }

/* Does this Time contain any PixSize? */
      if( scan->count[ 5 ] > 0 ) {

/* Issue a warning if more than 1 PixSize value was found. */
         if( scan->count[ 5 ] > 1 ) {
            Report( this, elem, WARNING, "contains more than one <PixSize>"
                    " element. AST can only use the first", status );
         }

/* Get the first PixSize value. */
         v = ElemValueD( this, scan->el[5][0], AST__BAD, status );
         if( v != AST__BAD ) {

/* Create the upper and lower limits of a bar centred on zero. */
            ubnd[ 0 ] = 0.5*fabs( v );
            lbnd[ 0 ] = -ubnd[ 0 ];

/* Create an Interval within the time Frame. */
            r = (AstRegion *) astInterval( cfrm, lbnd, ubnd, NULL, "", status );

/* If the units of this Frame differs from that of the supplied Frame,
   set it to the units of the supplied Frame. This will cause the
   encapsulated limits to be mapped into the new units. */
            if( funit ) astSetUnit( r, 0, funit );

/* Ensure the original title is preserved. */
            if( title ) astSetTitle( r, title );

/* Store in the returned ancillary KeyMap. */
            astMapPut0A( *anc, AST__STCPIXSZ, r, NULL );
            r = astAnnul( r );
         }
      }

/* Free resources */
      if( funit ) funit = astFree( (void *) funit );
      cfrm = astAnnul( cfrm );
      if( title ) title = astFree( (void *) title );
      scan = FreeIVOAScan( scan, status );
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;

}

static AstTimeScaleType TimeScaleReader( AstXmlChan *this, AstXmlElement *elem, int *status ){
/*
*  Name:
*     TimeScaleReader

*  Purpose:
*     Read a time value from an IVOA TimeScale element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstTimeScaleType TimeScaleReader( AstXmlChan *this, AstXmlElement *elem, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function returns a value representing the timescale specified by
*     the supplied IVOA TimeScale element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA TimeScale element.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The timescale value (values are defined in timeframe.h).

*/

/* Local Variables: */
   AstTimeScaleType result;
   char buff[ 80 ];
   const char *tstxt;

/* Initialise */
   result = AST__BADTS;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the timescale string from the element, and find the corresponding
   AST timescale value (if any). */
   tstxt = astXmlGetValue( elem, 0 );
   if( tstxt ) {

      if( !strcmp( tstxt, "TT" ) ) {
         result = AST__TT;

      } else if( !strcmp( tstxt, "TDT" ) ) {
         result = AST__TT;

      } else if( !strcmp( tstxt, "ET" ) ) {
         Report( this, elem, WARNING, "TT will be used in place of ET", status );
         result = AST__TT;

      } else if( !strcmp( tstxt, "TDB" ) ) {
         result = AST__TDB;

      } else if( !strcmp( tstxt, "TCG" ) ) {
         result = AST__TCG;

      } else if( !strcmp( tstxt, "TCB" ) ) {
         result = AST__TCB;

      } else if( !strcmp( tstxt, "TAI" ) ) {
         result = AST__TAI;

      } else if( !strcmp( tstxt, "IAT" ) ) {
         result = AST__TAI;

      } else if( !strcmp( tstxt, "UTC" ) ) {
         result = AST__UTC;

      } else if( !strcmp( tstxt, "LST" ) ) {
         result = AST__LMST;

      } else {
         sprintf( buff, "contains unsupported timescale %s", tstxt );
         Report( this, elem, FAILURE, buff, status );
         result = AST__BADTS;
      }
   }

/* Return the time value. */
   return result;
}

static AstRegion *UnionReader( AstXmlChan *this, AstXmlElement *elem,
                               AstFrame *frm, int *status ){
/*
*  Name:
*     UnionReader

*  Purpose:
*     Make an AST Region from an IVOA Union region element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstRegion *UnionReader( AstXmlChan *this, AstXmlElement *elem,
*                             AstFrame *frm, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Region from the supplied IVOA
*     Union region element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Union region element.
*     frm
*        Pointer to the 2D Frame in which the returned Region should be
*        defined. If the Unit attribute is not set, this function will
*        set it to the value supplied in "unit" before returning.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Region.

*/

/* Local Variables: */
   AstRegion *new;               /* Pointer to returned Region */
   AstRegion *reg;               /* Pointer to component Region */
   AstRegion *tmp;               /* Pointer to new Region */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[1];         /* Names of the subelements to be searched for */
   int i;                        /* Loop count */
   int max[1];                   /* Max allowed occurrences of each name */
   int min[1];                   /* Min allowed occurrences of each name */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Search the supplied element for a Region sub-element. */
   names[ 0 ] = "Intersection|Union|Negation|AllSky|Circle|Ellipse|Polygon|"
                "Convex|Box";
   min[ 0 ] = 2;
   max[ 0 ] = INT_MAX;
   scan = ScanIVOAElement( this, elem, 1, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Create Regions from all the component region elements, and combine
   them into nested CmpRegions, using the boolean OR operator to combine
   them. */
      new = RegionReader( this, scan->el[0][0], frm, status );
      for( i = 1; i < scan->count[0]; i++ ) {
         reg = RegionReader( this, scan->el[0][i], frm, status );
         tmp = (AstRegion *) astCmpRegion( new, reg, AST__OR, "", status );
         reg = astAnnul( reg );
         (void) astAnnul( new );
         new = tmp;
      }

/* Get any fill factor from the element and assign to the returned Region. */
      FillAndLims( this, elem, new, status );

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

/* Annul any returned Frame if an error has occurred. */
   if( !astOK ) new = astAnnul( new );

/* Return the pointer to the new Region. */
   return new;
}

static int Use( AstXmlChan *this, int set, int helpful, int *status ) {
/*
*  Name:
*     Use

*  Purpose:
*     Decide whether to write a value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int Use( AstXmlChan *this, int set, int helpful, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function decides whether a value supplied by a class "Dump"
*     function, via a call to one of the astWrite... protected
*     methods, should actually be written to the data sink associated
*     with a XmlChan.
*
*     This decision is based on the settings of the "set" and
*     "helpful" flags supplied to the astWrite... method, plus the
*     attribute settings of the XmlChan.

*  Parameters:
*     this
*        A pointer to the XmlChan.
*     set
*        The "set" flag supplied.
*     helpful
*        The "helpful" value supplied.
*     status
*        Pointer to the inherited status variable.

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

/* Otherwise, obtain the value of the XmlChan's Full attribute. */
   if ( !set ) {
      full = astGetFull( this );

/* If Full is positive, display all values, if zero, display only
   "helpful" values, if negative, display no (un-"set") values. */
      if ( astOK ) result = ( ( helpful && ( full > -1 ) ) || ( full > 0 ) );
   }

/* Return the result. */
   return result;
}

static int Ustrcmp( const char *a, const char *b, int *status ){
/*
*  Name:
*     Ustrcmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int Ustrcmp( const char *a, const char *b, int *status )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     status
*        Pointer to the inherited status variable.

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

static int Ustrncmp( const char *a, const char *b, size_t n, int *status ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strncmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int Ustrncmp( const char *a, const char *b, size_t n, int *status )

*  Class Membership:
*     XmlChan member function.

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
*     status
*        Pointer to the inherited status variable.

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

static int VertexReader( AstXmlChan *this, AstXmlElement *elem, double *x,
                         double *y, int *status ){
/*
*  Name:
*     VertexReader

*  Purpose:
*     Read a position from an IVOA Vertex element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int VertexReader( AstXmlChan *this, AstXmlElement *elem, double *x,
*                       double *y )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function reads a 2D position from the supplied IVOA Vertex
*     element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the IVOA Vertex element.
*     x
*        Pointer to the double in which to put the returned X value.
*     y
*        Pointer to the double in which to put the returned Y value.

*  Returned Value:
*     Non-zero if the <Vertex> contains a <pole> tag.

*/

/* Local Variables: */
   IVOAScan *scan;               /* Structure holding scan results */
   const char *names[2];         /* Names of the subelements to be searched for */
   double xy[ 2 ];               /* Axis values read from Position */
   int max[2];                   /* Max allowed occurrences of each name */
   int min[2];                   /* Min allowed occurrences of each name */
   int result;                   /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   result = 0;
   *x = AST__BAD;
   *y = AST__BAD;

/* Search the supplied element for the required sub-elements. */
   names[ 0 ] = "Position";
   max[ 0 ] = 1;
   min[ 0 ] = 1;
   names[ 1 ] = "SmallCircle";
   max[ 1 ] = 1;
   min[ 1 ] = 0;
   scan = ScanIVOAElement( this, elem, 2, names, min, max, status );

/* If succesfull.. */
   if( scan ) {

/* Get the axis values from the Position element. */
      xy[ 0 ] = AST__BAD;
      xy[ 1 ] = AST__BAD;
      ElemListD( this, scan->el[0][0], 2, xy, status );
      *x = xy[ 0 ];
      *y = xy[ 1 ];

/* Get any SmallCircle element. If it has a Pole issue a warning. */
      result = scan->count[ 1 ];
      if( result ) {
         if( FindElement( this, scan->el[1][0], "Pole", status ) ) {
            Report( this, scan->el[1][0], WARNING, "contains a <Pole> "
                    "tag (poles are not supported by AST)", status );
         }
      }

/* Free resources */
      scan = FreeIVOAScan( scan, status );
   }

   return result;
}

static void WriteBegin( AstChannel *this_channel, const char *class,
                        const char *comment, int *status ) {
/*
*  Name:
*     WriteBegin

*  Purpose:
*     Write a "Begin" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteBegin( AstChannel *this_channel, const char *class,
*                      const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteBegin method inherited from the Channel class).

*  Description:
*     This function writes a "Begin" data item to the data sink
*     associated with a Channel, so as to begin the output of a new
*     Object definition.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class to which the Object belongs.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "Begin"
*        item. Normally, this will describe the purpose of the Object.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*/

/* Local Variables: */
   AstXmlChan *this;         /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;      /* The XML element to hodl the new AST object */
   const char *pref;         /* XML namespace prefix to use */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If this is a top level object (i.e. if there is no container element),
   reset all the other values in the XmlChan for safety. */
   if( !this->container ) {
      this->objectname = NULL;
      this->objectset = 1;
      this->objectcomment = NULL;
   }

/* Initialise a flag to indicate that the next "IsA" item should not be
   written. This flag will be changed if and when an item is added which
   related to the class described by the "IsA" item. Save the old value
   first. */
   this->write_isa = 0;

/* Store the namespace prefix. */
   pref = astGetXmlPrefix( this );

/* Create a new XmlElement with a name equal to the AST class name of the
   object being dumped (and no namespace prefix), and add it into the
   current container (i.e. parent) element. */
   elem = astXmlAddElement( this->container, class, pref );

/* If this is a top level container, store the namespace URI for
   the element, either default or named depending on the value of
   XmlPrefix. */
   if( !this->container ) astXmlAddURI( elem, pref, AST__XMLNS );

/* If non-blank, append a "Label" atttribute to the element holding the
   name of the object (stored in the XmlChan structure). */
   if( this->objectname ) astXmlAddAttr( elem, LABEL, this->objectname, NULL );

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
   if( !this->objectset ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Add commments if required. */
   if( astGetComment( this_channel ) ) {

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. This comment describes the class function as a whole, not
   the specific usage of this instance of the class (this is given by the
   comment in this->objectcomment). */
      if( comment && *comment ) astXmlAddComment( elem, 0, comment );

/* If the object has a usage comment, add it to the content of the
   element if required. */
      if( this->objectcomment ) astXmlAddAttr( elem, DESC, this->objectcomment, NULL );
   }

/* Make the new element the current container. */
   this->container = (AstXmlParent *) elem;

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteDouble( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         double value, const char *comment, int *status ) {
/*
*  Name:
*     WriteDouble

*  Purpose:
*     Write a double value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteDouble( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       double value, const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteDouble method inherited from the Channel class).

*  Description:
*     This function writes a named double value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
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
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define BUFF_LEN 100             /* Size of local formatting buffer */

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful, status ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* Format the value as a string and store it as the VALUE attribute.
   Make sure "-0" isn't produced. Use a magic string to represent bad
   values. */
         if( value != AST__BAD ) {
            (void) sprintf( buff, "%.*g", DBL_DIG, value );
            if ( !strcmp( buff, "-0" ) ) {
               buff[ 0 ] = '0';
               buff[ 1 ] = '\0';
            }
         } else {
            strcpy( buff, BAD_STRING );
         }
         astXmlAddAttr( elem, VALUE, buff, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void WriteEnd( AstChannel *this_channel, const char *class, int *status ) {
/*
*  Name:
*     WriteEnd

*  Purpose:
*     Write an "End" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteEnd( AstChannel *this, const char *class, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteEnd method inherited from the Channel class).

*  Description:
*     This function writes an "End" data item to the data sink
*     associated with a Channel. This item delimits the end of an
*     Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        class name of the Object whose definition is being terminated
*        by the "End" item.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlParent *parent;         /* Pointer to parent element */
   char *d;                      /* Pointer to end of next sub-string */
   char *c;                      /* Pointer to start of next sub-string */
   char *text;                   /* Pointer to complete string */
   int mxlen;                    /* Max allowed length of text */

#ifdef DEBUG
   int nobj;                     /* No. of XmlObjects in existence */
#endif

/* Check the global error status. */
   if ( !astOK ) return;

#ifdef DEBUG
/* Save the number of XmlObjects currently in existenece. */
   nobj = astXmlTrace(3);
#endif

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Get the parent of the current container element. */
   if( this->container ) {
      parent = astXmlGetParent( this->container );

/* If the current container element has no parent, we have completed the
   construction of the in-memory XML representation of the AST object being
   written out. In this case, we convert the in-memory representation
   into a set of strings and write them out using the supplied sink
   function. */
      if( !parent ) {

/* First get a single string holding the complete formatted XML
   representation of the AST object. */
         if( astGetIndent( this ) ) {
            text = (char *) astXmlShow( this->container );
         } else {
            text = (char *) astXmlFormat( this->container );
         }

/* Now, if we have any text, split it into separate lines. The end of a line
   is indicated by a "\n" character in the text returned by astXmlFormat. */
         if( text ) {

/* Get the maximum allowed line length. */
            mxlen = astGetXmlLength( this );

/* Loop round locating each '\n' character in the string. Replace the
   '\n' character by 0, so that the previous part of the string is then
   null terminated, and write it out using the astPutNextText method
   (splitting the text up into lines no longer than "mxlen"). */
            c = text;
            d = strchr( c, '\n' );
            while( d ) {
               *d = 0;
               OutputText( this, c, mxlen, status );
               c = d + 1;
               d = strchr( c, '\n' );
            }

/* Write out any text following the last '\n' character. */
            if( *c ) OutputText( this, c, mxlen, status );

/* Free the memory holding the text and in-memory representations of the AST
   Object. */
            text = astFree( (void *) text );
            astXmlRemoveItem( this->container );
            this->container = astXmlAnnul( this->container );

#ifdef DEBUG
/* Report an error if there is a memory leak. */
            if( astXmlTrace(3) > nobj && astOK ) {
               astError( AST__INTER, "astWriteEnd(XmlChan): %d XmlObjects "
                         "remain in existence - should be %d (internal AST "
                         "programming error).", status, astXmlTrace(3), nobj );
            }
#endif

         }
      }

/* Reset the current container element to be the parent found above. */
      if( !parent || astXmlCheckType( parent, AST__XMLELEM ) ) {
         this->container = parent;
      } else if( astOK ) {
         astError( AST__INTER, "astWriteEnd(XmlChan): Cannot update "
                   "container: parent is not an XmlElement (internal "
                   "AST programming error)." , status);
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteInt( AstChannel *this_channel, const char *name, int set, int helpful,
                      int value, const char *comment, int *status ) {
/*
*  Name:
*     WriteInt

*  Purpose:
*     Write an integer value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteInt( AstChannel *this, const char *name, int set, int helpful,
*                    int value, const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteInt method inherited from the Channel class).

*  Description:
*     This function writes a named integer value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
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
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define BUFF_LEN 50             /* Size of local formatting buffer */

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful, status ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* Format the value as a decimal string and add it to the element as the
   VALUE attribute. */
         (void) sprintf( buff, "%d", value );
         astXmlAddAttr( elem, VALUE, buff, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void WriteIsA( AstChannel *this_channel, const char *class,
                      const char *comment, int *status ) {
/*
*  Name:
*     WriteIsA

*  Purpose:
*     Write an "IsA" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteIsA( AstChannel *this, const char *class,
*                    const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteIsA method inherited from the Channel class).

*  Description:
*     This function writes an "IsA" data item to the data sink
*     associated with a Channel. This item delimits the end of the
*     data associated with the instance variables of a class, as part
*     of an overall Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class whose data are terminated by the "IsA"
*        item.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "IsA"
*        item. Normally, this will describe the purpose of the class
*        whose data are being terminated.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*/

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Output an "IsA" item only if there has been at least one item
   written since the last "Begin" or "IsA" item, or if the Full
   attribute for the Channel is greater than zero (requesting maximum
   information). */
      if ( this->write_isa || astGetFull( this ) > 0 ) {

/* Create a new XmlElement with a name of "_isa" (and no namespace prefix),
   and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ISA,
                                  astGetXmlPrefix( this ) );

/* Add a "class" attribute to this element containing the class name. */
         astXmlAddAttr( elem, "class", class, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. This comment describes the class function as a whole, not
   the specific usage of this instance of the class. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }
      }
   }

/* Initialise a flag to indicate that the next "IsA" item should not be
   written. This flag will be changed if and when an item is added which
   related to the class described by the "IsA" item. */
   this->write_isa = 0;

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );
}

static void WriteObject( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         AstObject *value, const char *comment, int *status ) {
/*
*  Name:
*     WriteObject

*  Purpose:
*     Write an Object as a value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteObject( AstChannel *this_channel, const char *name,
*                       int set, int helpful,
*                       AstObject *value, const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteObject method inherited from the Channel class).

*  Description:
*     This function writes an Object as a named value, representing
*     the value of a class instance variable, to the data sink
*     associated with an XmlChan. It is intended for use by class
*     "Dump" functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the XmlChan.
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
*        representations.
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
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        A Pointer to the Object to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;         /* A pointer to the XmlChan structure. */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out the object. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if ( Use( this, set, helpful, status ) ) {

/* Save the supplied name associated with the object being written so
   that it is available for use within the following invocation of the
   WriteBegin method. The name is stored within the XmlChan structure
   (NULL is used to indicate "no name supplied"). */
         this->objectname = ( name && strlen( name ) ) ? name : NULL;

/* Also save the supplied comment and a flag indicating if the object is
   set. These will be used by the WriteBegin method. They are stored within
   the XmlChan structure. */
         this->objectset = set;
         this->objectcomment = comment;

/* Write the object to the XmlChan. */
         (void) astWrite( this, value );

/* Nullify the components of the XmlChan set above. */
         this->objectname = NULL;
         this->objectset = 1;
         this->objectcomment = NULL;

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteString( AstChannel *this_channel, const char *name, int set,
                         int helpful, const char *value, const char *comment, int *status ){
/*
*  Name:
*     WriteString

*  Purpose:
*     Write a string value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteString( AstChannel *this, const char *name, int set, int helpful,
*                       const char *value, const char *comment, int *status )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteString method inherited from the Channel class).

*  Description:
*     This function writes a named string value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
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
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        Pointer to a constant null-terminated string containing the
*        value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful, status ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* If we are using QUOTED format, add an attribute to indicate that this is a
   string value (mainly included for compatibility with JNIAST). */
         if( astGetXmlFormat( this ) == QUOTED_FORMAT ) {
            astXmlAddAttr( elem, QUOTED, TRUE, NULL );
         }

/* Add it the value to the element as the VALUE attribute. */
         astXmlAddAttr( elem, VALUE, value, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}


/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* XmlFormat */
/* ========= */
/*
*att++
*  Name:
*     XmlFormat

*  Purpose:
*     System for formatting Objects as XML.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the formatting system to use when AST
*     Objects are written out as XML through an XmlChan. It
c     affects the behaviour of the astWrite function when
f     affects the behaviour of the AST_WRITE routine  when
*     they are used to transfer any AST Object to or from an external
*     XML representation.
*
*     The XmlChan class allows AST objects to be represented in the form
*     of XML in several ways (conventions) and the XmlFormat attribute is
*     used to specify which of these should be used. The formatting options
*     available are outlined in the "Formats Available" section below.
*
*     By default, an XmlChan will attempt to determine which format system
*     is already in use, and will set the default XmlFormat value
*     accordingly (so that subsequent I/O operations adopt the same
*     conventions). It does this by looking for certain critical items
*     which only occur in particular formats. For details of how this
*     works, see the "Choice of Default Format" section below. If you wish
*     to ensure that a particular format system is used, independently of
*     any XML already read, you should set an explicit XmlFormat value
*     yourself.
*
*  Formats Available:
*     The XmlFormat attribute can take any of the following (case
*     insensitive) string values to select the corresponding formatting
*     system:
*
*     - "NATIVE": This is a direct conversion to XML of the heirarchical
*     format used by a standard XML channel (and also by the NATIVE
*     encoding of a FitsChan).
*
*     - "QUOTED": This is the same as NATIVE format except that extra
*     information is included which allows client code to convert the
*     XML into a form which can be read by a standard AST Channel. This
*     extra information indicates which AST attribute values should be
*     enclosed in quotes before being passed to a Channel.
*
*     - "IVOA": This is a format that uses an early draft of the STC-X schema
*     developed by the International Virtual Observatory Alliance (IVOA -
*     see "http://www.ivoa.net/") to describe coordinate systems, regions,
*     mappings, etc. Support is limited to V1.20 described at
*     "http://www.ivoa.net/Documents/WD/STC/STC-20050225.html". Since the
*     version of STC-X finally adopted by the IVOA differs in several
*     significant respects from V1.20, this format is now mainly of
*     historical interest. Note, the alternative "STC-S" format (a
*     simpler non-XML encoding of the STC metadata) is supported by the
*     StcsChan class.

*  Choice of Default Format;
*     If the XmlFormat attribute of an XmlChan is not set, the default
*     value it takes is determined by the presence of certain critical
*     items within the document most recently read using
c     astRead.
f     AST_READ.
*     The sequence of decision used to arrive at the default value is as
*     follows:
*
*     - If the previous document read contained any elements in any of the STC
*     namespaces ("urn:nvo-stc", "urn:nvo-coords" or "urn:nvo-region"), then
*     the default value is IVOA.
*     - If the previous document read contained any elements in the AST
*     namespace which had an associated XML attribute called "quoted", then
*     the default value is QUOTED.
*     - Otherwise, if none of these conditions is met (as would be the
*     case if no document had yet been read), then NATIVE format is
*     used.
*
*     Setting an explicit value for the XmlFormat attribute always
*     over-rides this default behaviour.

*  The IVOA Format:
*     The IVOA support caters only for certain parts of V1.20 of the
*     draft Space-Time Coordinate (STC) schema (see
*     http://www.ivoa.net/Documents/WD/STC/STC-20050225.html). Note, this
*     draft has now been superceded by an officially adopted version that
*     differs in several significant respects from V1.20. Consequently,
*     the "IVOA" XmlChan format is of historical interest only.
*
*     The following points should be noted when using an XmlChan to read
*     or write STC information (note, this list is currently incomplete):
*
*     - Objects can currently only be read using this format, not written.
*     - The AST object generated by reading an <STCMetadata> element will
*     be an instance of one of the AST "Stc" classes: StcResourceProfile,
*     StcSearchLocation, StcCatalogEntryLocation, StcObsDataLocation.
*     - When reading an <STCMetadata> element, the axes in the returned
*     AST Object will be in the order space, time, spectral, redshift,
*     irrespective of the order in which the axes occur in the <STCMetadata>
*     element. If the supplied <STCMetadata> element does not contain all of
*     these axes, the returned AST Object will also omit them, but the
*     ordering of those axes which are present will be as stated above. If
*     the spatial frame represents a celestial coordinate system the
*     spatial axes will be in the order (longitude, latitude).
*     - Until such time as the AST TimeFrame is complete, a simple
*     1-dimensional Frame (with Domain set to TIME) will be used to
*     represent the STC <TimeFrame> element. Consequently, most of the
*     information within a <TimeFrame> element is currently ignored.
*     - <SpaceFrame> elements can only be read if they describe a celestial
*     longitude and latitude axes supported by the AST SkyFrame class. The
*     space axes will be returned in the order (longitude, latitude).
*     - Velocities associated with SpaceFrames cannot be read.
*     - Any <GenericCoordFrame> elements within an <AstroCoordSystem> element
*     are currently ignored.
*     - Any second or subsequent <AstroCoordSystem> found within an
*     STCMetaData element is ignored.
*     - Any second or subsequent <AstroCoordArea> found within an
*     STCMetaData element is ignored.
*     - Any <OffsetCenter> found within a <SpaceFrame> is ignored.
*     - Any CoordFlavor element found within a <SpaceFrame> is ignored.
*     - <SpaceFrame> elements can only be read if they refer to
*     one of the following space reference frames: ICRS, GALACTIC_II,
*     SUPER_GALACTIC, HEE, FK4, FK5, ECLIPTIC.
*     - <SpaceFrame> elements can only be read if the reference
*     position is TOPOCENTER. Also, any planetary ephemeris is ignored.
*     - Regions: there is currently no support for STC regions of type
*     Sector, ConvexHull or SkyIndex.
*     - The AST Region read from a CoordInterval element is considered to
*     be open if either the lo_include or the hi_include attribute is
*     set to false.
*     - <RegionFile> elements are not supported.
*     - Vertices within <Polygon> elements are always considered to be
*     joined using great circles (that is, <SmallCircle> elements are
*     ignored).

*  Applicability:
*     XmlChan
*        All XmlChans have this attribute.
*att--
*/
astMAKE_CLEAR(XmlChan,XmlFormat,xmlformat,UNKNOWN_FORMAT)
astMAKE_SET(XmlChan,XmlFormat,int,xmlformat,(
   value == NATIVE_FORMAT ||
   value == IVOA_FORMAT ||
   value == QUOTED_FORMAT ? value :
   (astError( AST__BADAT, "astSetXmlFormat: Unknown XML formatting system %d "
              "supplied.", status, value ), UNKNOWN_FORMAT )))
astMAKE_TEST(XmlChan,XmlFormat,( this->xmlformat != UNKNOWN_FORMAT ))
astMAKE_GET(XmlChan,XmlFormat,int,0,(this->xmlformat == UNKNOWN_FORMAT ?
                                this->formatdef : this->xmlformat))

/*
*att++
*  Name:
*     XmlLength

*  Purpose:
*     Controls output buffer length.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute specifies the maximum length to use when writing out
*     text through the sink function supplied when the XmlChan was created.
*
*     The number of characters in each string written out through the sink
*     function will not be greater than the value of this attribute (but
*     may be less). A value of zero (the default) means there is no limit -
*     each string can be of any length.
*
f     Note, the default value of zero is unlikely to be appropriate when
f     an XmlChan is used within Fortran code. In this case, XmlLength
f     should usually be set to the size of the CHARACTER variable used to
f     receive the text returned by AST_GETLINE within the sink function.
f     This avoids the possibility of long lines being truncated invisibly
f     within AST_GETLINE.

*  Applicability:
*     XmlChan
*        All XmlChans have this attribute.
*att--
*/
astMAKE_CLEAR(XmlChan,XmlLength,xmllength,-INT_MAX)
astMAKE_GET(XmlChan,XmlLength,int,0,( ( this->xmllength != -INT_MAX ) ? this->xmllength : 0 ))
astMAKE_SET(XmlChan,XmlLength,int,xmllength,(value<0?0:value))
astMAKE_TEST(XmlChan,XmlLength,( this->xmllength != -INT_MAX ))

/*
*att++
*  Name:
*     XmlPrefix

*  Purpose:
*     The namespace prefix to use when writing.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute is a string which is to be used as the namespace
*     prefix for all XML elements created as a result of writing an AST
*     Object out through an XmlChan. The URI associated with the namespace
*     prefix is given by the symbolic constant AST__XMLNS defined in
f     AST_PAR.
c     ast.h.
*     A definition of the namespace prefix is included in each top-level
*     element produced by the XmlChan.
*
*     The default value is a blank string which causes no prefix to be
*     used. In this case each top-level element will set the default
*     namespace to be the value of AST__XMLNS.

*  Applicability:
*     Object
*        All Objects have this attribute.

*att--
*/
astMAKE_CLEAR(XmlChan,XmlPrefix,xmlprefix,astFree( this->xmlprefix ))
astMAKE_GET(XmlChan,XmlPrefix,const char *,NULL,( this->xmlprefix ? this->xmlprefix : "" ))
astMAKE_SET(XmlChan,XmlPrefix,const char *,xmlprefix,astStore( this->xmlprefix, value,
                                                strlen( value ) + (size_t) 1 ))
astMAKE_TEST(XmlChan,XmlPrefix,( this->xmlprefix != NULL ))


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for XmlChan objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstXmlChan *in;               /* Pointer to input XmlChan */
   AstXmlChan *out;              /* Pointer to output XmlChan */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output XmlChans. */
   in = (AstXmlChan *) objin;
   out = (AstXmlChan *) objout;

/* Clear the non-persistent values in the new XmlChan. */
   out->objectname = NULL;   /* Name of object being written */
   out->objectset = 1;       /* Is object being written set?*/
   out->objectcomment = NULL;/* Comment for object class being written */
   out->readcontext = NULL;  /* XmlElement giving context for current read */
   out->container = NULL;    /* XmlElement to which content will be added */
   out->write_isa = 0;       /* Write out the next "IsA" item? */
   out->reset_source = 1;    /* A new line should be read from the source */
   out->isa_class = NULL;    /* Class being loaded */

/* Store a copy of the prefix string.*/
   if ( in->xmlprefix ) out->xmlprefix = astStore( NULL, in->xmlprefix,
                                           strlen( in->xmlprefix ) + (size_t) 1 );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for XmlChan objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to XmlChan */

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) obj;

/* Free any unread part of the document. */
   this->readcontext = astXmlAnnul( this->readcontext );

/* Free the memory used for the XmlPrefix string if necessary. */
   this->xmlprefix = astFree( this->xmlprefix );

/* Free any memory used to store text read from the source */
   GetNextChar( NULL, status );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the XmlChan class to an output Channel.

*  Parameters:
*     this
*        Pointer to the XmlChan whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlChan *this;            /* Pointer to the XmlChan structure */
   const char *class;           /* Class name */
   const char *sval;            /* String attribute value */
   int ival;                    /* Integer attribute value */
   int set;                     /* Has the attribute got a set value? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Store the object class. */
   class = astGetClass( this );

/* Write out values representing the instance variables for the
   XmlChan class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/


/* Now do instance variables which are not attributes. */
/* =================================================== */

/* XmlLength */
/* --------- */
      set = TestXmlLength( this, status );
      ival = set ? GetXmlLength( this, status ) : astGetXmlLength( this );
      astWriteInt( channel, "XmlLen", set, 0, ival, "XML buffer length" );

/* XmlFormat. */
/* --------- */
   set = TestXmlFormat( this, status );
   ival = set ? GetXmlFormat( this, status ) : astGetXmlFormat( this );
   if( ival > UNKNOWN_FORMAT && ival <= MAX_FORMAT ) {
      astWriteString( channel, "XmlFmt", set, 1, xformat[ival], "Formatting system" );
   } else {
      astWriteString( channel, "XmlFmt", set, 1, UNKNOWN_STRING, "Formatting system" );
   }

/* XmlPrefix */
/* --------- */
      set = TestXmlPrefix( this, status );
      sval = set ? GetXmlPrefix( this, status ) : astGetXmlPrefix( this );
      astWriteString( channel, "XmlPrf", set, 1, sval,
                      "Namespace prefix" );
}


/* Standard class functions. */
/* ========================= */
/* Implement the astIsAXmlChan and astCheckXmlChan functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(XmlChan,Channel)
astMAKE_CHECK(XmlChan)

AstXmlChan *astXmlChan_( const char *(* source)( void ),
                         void (* sink)( const char * ),
                         const char *options, int *status, ...) {
/*
*++
*  Name:
c     astXmlChan
f     AST_XMLCHAN

*  Purpose:
*     Create an XmlChan.

*  Type:
*     Public function.

*  Synopsis:
c     #include "xmlchan.h"
c     AstXmlChan *astXmlChan( const char *(* source)( void ),
c                             void (* sink)( const char * ),
c                             const char *options, ... )
f     RESULT = AST_XMLCHAN( SOURCE, SINK, OPTIONS, STATUS )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function creates a new XmlChan and optionally initialises
*     its attributes.
*
*     A XmlChan is a specialised form of Channel which supports XML I/O
*     operations. Writing an Object to an XmlChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     XML description of that Object, and reading from an XmlChan will
*     create a new Object from its XML description.
*
*     Normally, when you use an XmlChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
f     and "sink" routines which connect it to an external data store
*     by reading and writing the resulting XML text. By default, however,
*     an XmlChan will read from standard input and write to standard
*     output.
*
*     Alternatively, an XmlChan can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.

*  Parameters:
c     source
f     SOURCE = SUBROUTINE (Given)
c        Pointer to a source function that takes no arguments and
c        returns a pointer to a null-terminated string.  If no value
c        has been set for the SourceFile attribute, this function
c        will be used by the XmlChan to obtain lines of input text. On
c        each invocation, it should return a pointer to the next input
c        line read from some external data store, and a NULL pointer
c        when there are no more lines to read.
c
c        If "source" is NULL and no value has been set for the SourceFile
c        attribute, the XmlChan will read from standard input instead.
f        A source routine, which is a subroutine which takes a single
f        integer error status argument.   If no value has been set
f        for the SourceFile attribute, this routine will be used by
f        the XmlChan to obtain lines of input text. On each
f        invocation, it should read the next input line from some
f        external data store, and then return the resulting text to
f        the AST library by calling AST_PUTLINE. It should supply a
f        negative line length when there are no more lines to read.
f        If an error occurs, it should set its own error status
f        argument to an error value before returning.
f
f        If the null routine AST_NULL is suppied as the SOURCE value,
f        and no value has been set for the SourceFile attribute,
f        the XmlChan will read from standard input instead.
c     sink
f     SINK = SUBROUTINE (Given)
c        Pointer to a sink function that takes a pointer to a
c        null-terminated string as an argument and returns void.
c        If no value has been set for the SinkFile attribute, this
c        function will be used by the XmlChan to deliver lines of
c        output text. On each invocation, it should deliver the
c        contents of the string supplied to some external data store.
c
c        If "sink" is NULL, and no value has been set for the SinkFile
c        attribute, the XmlChan will write to standard output instead.
f        A sink routine, which is a subroutine which takes a single
f        integer error status argument.  If no value has been set
f        for the SinkFile attribute, this routine will be used by
f        the XmlChan to deliver lines of output text. On each
f        invocation, it should obtain the next output line from the
f        AST library by calling AST_GETLINE, and then deliver the
f        resulting text to some external data store.  If an error
f        occurs, it should set its own error status argument to an
f        error value before returning.
f
f        If the null routine AST_NULL is suppied as the SINK value,
f        and no value has been set for the SinkFile attribute,
f        the XmlChan will write to standard output instead.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new XmlChan. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new XmlChan. The syntax used is identical to that for the
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
c     astXmlChan()
f     AST_XMLCHAN = INTEGER
*        A pointer to the new XmlChan.

*  Notes:
f     - The names of the routines supplied for the SOURCE and SINK
f     arguments should appear in EXTERNAL statements in the Fortran
f     routine which invokes AST_XMLCHAN. However, this is not generally
f     necessary for the null routine AST_NULL (so long as the AST_PAR
f     include file has been used).
*     - If the external data source or sink uses a character encoding
*     other than ASCII, the supplied source and sink functions should
*     translate between the external character encoding and the internal
*     ASCII encoding used by AST.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
f     - Note that the null routine AST_NULL (one underscore) is
f     different to AST__NULL (two underscores), which is the null Object
f     pointer.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstXmlChan *new;             /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by other C functions within AST, and uses the standard "wrapper"
   functions included in this class. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init,
                          &class_vtab, "XmlChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new XmlChan. */
   return new;
}

AstXmlChan *astXmlChanId_( const char *(* source)( void ),
                           void (* sink)( const char * ),
                           const char *options, ... ) {
/*
*  Name:
*     astXmlChanId_

*  Purpose:
*     Create an XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astXmlChanId_( const char *(* source)( void ),
*                                void (* sink)( const char * ),
*                                const char *options, ... )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function implements the external (public) C interface to the
*     astXmlChan constructor function. Another function (astXmlChanForId)
*     should be called to create an XmlChan for use within other languages.
*     Both functions return an ID value (instead of a true C pointer) to
*     external users, and must be provided because astXmlChan_ has a variable
*     argument list which cannot be encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astXmlChan_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astXmlChan_.

*  Returned Value:
*     The ID value associated with the new XmlChan.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstXmlChan *new;             /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by external C functions and uses the standard "wrapper"
   functions included in this class. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init,
                         &class_vtab, "XmlChan", source, SourceWrap,
                         sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new XmlChan. */
   return astMakeId( new );
}

AstXmlChan *astXmlChanForId_( const char *(* source)( void ),
                              char *(* source_wrap)( const char *(*)( void ), int * ),
                              void (* sink)( const char * ),
                              void (* sink_wrap)( void (*)( const char * ),
                                                  const char *, int * ),
                              const char *options, ... ) {
/*
*+
*  Name:
*     astXmlChanFor

*  Purpose:
*     Initialise an XmlChan from a foreign language interface.

*  Type:
*     Public function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astXmlChanFor( const char *(* source)( void ),
*                                char *(* source_wrap)( const char *(*)
*                                                       ( void ), int * ),
*                                void (* sink)( const char * ),
*                                void (* sink_wrap)( void (*)( const char * ),
*                                                    const char *, int * ),
*                                const char *options, ... )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function creates a new XmlChan from a foreign language
*     interface and optionally initialises its attributes.
*
*     A XmlChan implements low-level XML input/output for the AST library.
*     Writing an Object to an XmlChan (using astWrite) will generate a
*     XML representation of that Object, and reading from a
*     XmlChan (using astRead) will create a new Object from its
*     XML representation.
*
*     Normally, when you use an XmlChan, you should provide "source"
*     and "sink" functions which connect it to an external data store
*     by reading and writing the resulting text. This function also
*     requires you to provide "wrapper" functions which will invoke
*     the source and sink functions. By default, however, an XmlChan
*     will read from standard input and write to standard output.

*  Parameters:
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of input text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the XmlChan will read from standard
*        input instead.
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
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the XmlChan will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of output text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the XmlChan will write to standard output
*        instead.
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
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new XmlChan. The syntax used is identical to
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
*     astXmlChanFor()
*        A pointer to the new XmlChan.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*     - This function is only available through the public interface
*     to the XmlChan class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-

*  Implememtation Notes:
*     - This function behaves exactly like astXmlChanId_, in that it
*     returns ID values and not true C pointers, but it has two
*     additional arguments. These are pointers to the "wrapper
*     functions" which are needed to accommodate foreign language
*     interfaces.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstXmlChan *new;              /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init,
                         &class_vtab, "XmlChan", source, source_wrap,
                         sink, sink_wrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new XmlChan. */
   return astMakeId( new );
}

AstXmlChan *astInitXmlChan_( void *mem, size_t size, int init,
                             AstXmlChanVtab *vtab, const char *name,
                             const char *(* source)( void ),
                             char *(* source_wrap)( const char *(*)( void ), int * ),
                             void (* sink)( const char * ),
                             void (* sink_wrap)( void (*)( const char * ),
                                                 const char *, int * ), int *status ) {
/*
*+
*  Name:
*     astInitXmlChan

*  Purpose:
*     Initialise an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astInitXmlChan( void *mem, size_t size, int init,
*                                 AstXmlChanVtab *vtab, const char *name,
*                                 const char *(* source)( void ),
*                                 char *(* source_wrap)( const char *(*)( void ), int * ),
*                                 void (* sink)( const char * ),
*                                 void (* sink_wrap)( void (*)( const char * ),
*                                                     const char *, int * ) )

*  Class Membership:
*     XmlChan initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new XmlChan object. It allocates memory (if
*     necessary) to accommodate the XmlChan plus any additional data
*     associated with the derived class.  It then initialises a
*     XmlChan structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for an XmlChan at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the XmlChan is to be
*        initialised.  This must be of sufficient size to accommodate
*        the XmlChan data (sizeof(XmlChan)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the XmlChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the XmlChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A boolean flag indicating if the XmlChan's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new XmlChan.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
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
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the contents of the XmlChan will not be
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
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.

*  Returned Value:
*     A pointer to the new XmlChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *new;              /* Pointer to new XmlChan */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitXmlChanVtab( vtab, name );

/* Initialise a Channel structure (the parent class) as the first
   component within the XmlChan structure, allocating memory if
   necessary. */
   new = (AstXmlChan *) astInitChannel( mem, size, 0,
                                       (AstChannelVtab *) vtab, name,
                                        source, source_wrap, sink,
                                        sink_wrap );

   if ( astOK ) {

/* Initialise the XmlChan data. */
/* ---------------------------- */
      new->objectname = NULL;   /* Name of object being written */
      new->objectset = 1;       /* Is object being written set?*/
      new->objectcomment = NULL;/* Comment for object class being written */
      new->container = NULL;    /* XmlElement to which content will be added */
      new->readcontext = NULL;  /* XmlElement giving context for current read */
      new->write_isa = 0;       /* Write out the next "IsA" item? */
      new->xmllength = -INT_MAX;/* Buffer length */
      new->xmlprefix = NULL;    /* Xml prefix */
      new->xmlformat = UNKNOWN_FORMAT; /* Xml format */
      new->formatdef = NATIVE_FORMAT;  /* Default Xml format */
      new->reset_source = 1;    /* A new line should be read from the source */
      new->isa_class = NULL;    /* Class being loaded */

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstXmlChan *astLoadXmlChan_( void *mem, size_t size,
                             AstXmlChanVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadXmlChan

*  Purpose:
*     Load an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astLoadXmlChan( void *mem, size_t size,
*                                 AstXmlChanVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     XmlChan loader.

*  Description:
*     This function is provided to load a new XmlChan using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     XmlChan structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for an XmlChan at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the XmlChan is to be
*        loaded.  This must be of sufficient size to accommodate the
*        XmlChan data (sizeof(XmlChan)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the XmlChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the XmlChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstXmlChan) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new XmlChan. If this is NULL, a pointer
*        to the (static) virtual function table for the XmlChan class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "XmlChan" is used instead.

*  Returned Value:
*     A pointer to the new XmlChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstXmlChan *new;            /* Pointer to the new XmlChan */
   char *text;                 /* Textual version of integer value */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this XmlChan. In this case the
   XmlChan belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstXmlChan );
      vtab = &class_vtab;
      name = "XmlChan";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitXmlChanVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built XmlChan. */
   new = astLoadChannel( mem, size, (AstChannelVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "XmlChan" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* Ensure other items in the XmlChan structure are initialised properly. */
      new->objectname = NULL;   /* Name of object being written */
      new->objectset = 1;       /* Is object being written set?*/
      new->objectcomment = NULL;/* Comment for object class being written */
      new->container = NULL;    /* XmlElement to which content will be added */
      new->readcontext = NULL;  /* XmlElement giving context for current read */
      new->write_isa = 0;       /* Write out the next "IsA" item? */
      new->xmllength = -INT_MAX;/* Buffer length */
      new->xmlprefix = NULL;    /* Xml prefix */
      new->reset_source = 1;    /* A new line should be read from the source */
      new->isa_class = NULL;    /* Class being loaded */
      new->formatdef = NATIVE_FORMAT;  /* Default Xml format */

/* Now restore presistent values. */

/* XmlLength */
/* --------- */
      new->xmllength = astReadInt( channel, "xmllen", -INT_MAX );

/* XmlPrefix */
/* --------- */
      new->xmlprefix = astReadString( channel, "xmlprf", NULL );

/* XmlFormat. */
/* --------- */
      text = astReadString( channel, "xmlfmt", UNKNOWN_STRING );
      if( strcmp( text, UNKNOWN_STRING ) ) {
         new->xmlformat = FindString( MAX_FORMAT + 1, xformat, text,
                                     "the XmlChan component 'XmlFmt'",
                                     "astRead", astGetClass( channel ), status );
      } else {
         new->xmlformat = UNKNOWN_FORMAT;
      }
      if ( TestXmlFormat( new, status ) ) SetXmlFormat( new, new->xmlformat, status );
      text = astFree( text );
   }

/* If an error occurred, clean up by deleting the new XmlChan. */
   if ( !astOK ) new = astDelete( new );

/* Return the new XmlChan pointer. */
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













