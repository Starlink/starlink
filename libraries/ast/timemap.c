/*
*class++
*  Name:
*     TimeMap

*  Purpose:
*     Sequence of time coordinate conversions.

*  Constructor Function:
c     astTimeMap (also see astTimeAdd)
f     AST_TIMEMAP (also see AST_TIMEADD)

*  Description:
*     A TimeMap is a specialised form of 1-dimensional Mapping which can be
*     used to represent a sequence of conversions between standard time
*     coordinate systems.
*
*     When a TimeMap is first created, it simply performs a unit
c     (null) Mapping. Using the astTimeAdd
f     (null) Mapping. Using the AST_TIMEADD
c     function, a series of coordinate conversion steps may then be
f     routine, a series of coordinate conversion steps may then be
*     added. This allows multi-step conversions between a variety of
*     time coordinate systems to be assembled out of a set of building
*     blocks.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astTimeAdd function.
f     see the description of the AST_TIMEADD routine.

*  Inheritance:
*     The TimeMap class inherits from the Mapping class.

*  Attributes:
*     The TimeMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     In addition to those functions applicable to all Mappings, the
c     following function may also be applied to all TimeMaps:
f     In addition to those routines applicable to all Mappings, the
f     following routine may also be applied to all TimeMaps:
*
c     - astTimeAdd: Add a time coordinate conversion to an TimeMap
f     - AST_TIMEADD: Add a time coordinate conversion to an TimeMap

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     NG: Norman Gray (Starlink)
*     DSB: David Berry (Starlink)

*  History:
*     5-Sep-2003 (NG):
*        Original version (drawing heavily on specmap.c)
*     25-MAY-2005 (DSB):
*        Extensive modifications to make it more AST-like.
*     10-AUG-2005 (DSB):
*        Add 2006 leap second.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*     15-OCT-2006 (DSB):
*        Add conversions between UT1 and UTC (UTTOUTC and UTCTOUT).
*     3-APR-2008 (DSB):
*        Only call memcpy if the source and destination pointers are
*        different.
*     15-APR-2008 (DSB):
*        Add missing "break;" statement to "case AST__LMSTTOGMST:"
*        in Transform.
*     20-MAY-2008 (DSB):
*        Add conversions between Local Time and UTC (LTTOUTC and UTCTOLT).
*     18-JUN-2009 (DSB):
*        Add OBSALT to argument list for TTTOTDB and TDBTOTT. Change
*        CLOCKLAT/LON to OBSLAT/LON for consistency with other classes.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS TimeMap

/* Codes to identify time coordinate conversions. */
#define AST__TIME_NULL   0       /* Null value */
#define AST__MJDTOMJD    1       /* MJD to MJD */
#define AST__MJDTOJD     2       /* MJD to JD */
#define AST__JDTOMJD     3       /* JD to MJD */
#define AST__MJDTOBEP    4       /* MJD to Besselian epoch */
#define AST__BEPTOMJD    5       /* Besselian epoch to MJD */
#define AST__MJDTOJEP    6       /* MJD to Julian epoch */
#define AST__JEPTOMJD    7       /* Julian epoch to MJD */
#define AST__TAITOUTC    8       /* TAI to UTC */
#define AST__UTCTOTAI    9       /* UTC to TAI */
#define AST__TTTOTAI    10       /* TT to TAI */
#define AST__TAITOTT    11       /* TAI to TT */
#define AST__TDBTOTT    12       /* TDB to TT */
#define AST__TTTOTDB    13       /* TT to TDB */
#define AST__TCGTOTT    14       /* TCG to TT */
#define AST__TTTOTCG    15       /* TT to TCG */
#define AST__TCBTOTDB   16       /* TCB to TDB */
#define AST__TDBTOTCB   17       /* TDB to TCB */
#define AST__UTTOGMST   18       /* UT to GMST */
#define AST__GMSTTOUT   19       /* GMST to UT1 */
#define AST__GMSTTOLMST 20       /* GMST to LMST */
#define AST__LMSTTOGMST 21       /* LMST to GMST */
#define AST__LASTTOLMST 22       /* LAST to LMST */
#define AST__LMSTTOLAST 23       /* LMST to LAST */
#define AST__UTTOUTC    24       /* UT1 to UTC */
#define AST__UTCTOUT    25       /* UTC to UT1 */
#define AST__LTTOUTC    26       /* Local Time to UTC */
#define AST__UTCTOLT    27       /* UTC to Local Time */

/* Maximum number of arguments required by a conversion. */
#define MAX_ARGS 6

/* The alphabet (used for generating keywords for arguments). */
#define ALPHABET "abcdefghijklmnopqrstuvwxyz"

/* Angle conversion */
#define PI 3.1415926535897932384626433832795028841971693993751
#define D2PI (2*PI)
#define PIBY2 (PI/2.0)
#define D2R (PI/180.0)
#define R2D (180.0/PI)

/* Other constants */
#define SPD 86400
#define LG 6.969290134E-10
#define LB 1.55051976772E-8
#define P0 6.55E-5
#define TTOFF 32.184

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "pal.h"              /* SLALIB interface */
#include "slamap.h"              /* Spatial sla mappings */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "unitmap.h"             /* Unit (null) Mappings */
#include "timemap.h"             /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double (* parent_rate)( AstMapping *, double *, int, int, int * );



#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(TimeMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(TimeMap,Class_Init)
#define class_vtab astGLOBAL(TimeMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstTimeMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstTimeMap *astTimeMapId_( int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *CvtString( int, const char **, int *, int *, const char *[ MAX_ARGS ], int * );
static double Gmsta( double, double, int, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static double Rcc( double, double, double, double, double, int * );
static int Equal( AstObject *, AstObject *, int * );
static int CvtCode( const char *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void AddArgs( int, double *, int * );
static void AddTimeCvt( AstTimeMap *, int, const double *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void TimeAdd( AstTimeMap *, const char *, const double[], int * );

static int GetObjSize( AstObject *, int * );
/* Member functions. */
/* ================= */

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two TimeMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     TimeMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two TimeMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a TimeMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the TimeMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTimeMap *that;
   AstTimeMap *this;
   const char *argdesc[ MAX_ARGS ];
   const char *comment;
   int i, j;
   int nargs;
   int nin;
   int nout;
   int result;
   int szargs;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two TimeMap structures. */
   this = (AstTimeMap *) this_object;
   that = (AstTimeMap *) that_object;

/* Check the second object is a TimeMap. We know the first is a
   TimeMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsATimeMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two TimeMaps differ, it may still be possible
   for them to be equivalent. First compare the TimeMaps if their Invert
   flags are the same. In this case all the attributes of the two TimeMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {
            if( this->ncvt == that->ncvt ) {
               result = 1;
               for( i = 0; i < this->ncvt && result; i++ ) {
                  if( this->cvttype[ i ] != that->cvttype[ i ] ) {
                     result = 0;
                  } else {
                     CvtString( this->cvttype[ i ], &comment, &nargs, &szargs, argdesc, status );
                     for( j = 0; j < nargs; j++ ) {
                        if( !astEQUAL( this->cvtargs[ i ][ j ],
                                       that->cvtargs[ i ][ j ] ) ){
                           result = 0;
                           break;
                        }
                     }
                  }
               }
            }

/* If the Invert flags for the two TimeMaps differ, the attributes of the two
   TimeMaps must be inversely related to each other. */
         } else {

/* In the specific case of a TimeMap, Invert flags must be equal. */
            result = 0;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     TimeMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied TimeMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the TimeMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTimeMap *this;         /* Pointer to TimeMap structure */
   int result;               /* Result value to return */
   int cvt;                  /* Loop counter for coordinate conversions */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the TimeMap structure. */
   this = (AstTimeMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   for ( cvt = 0; cvt < this->ncvt; cvt++ ) {
      result += astTSizeOf( this->cvtargs[ cvt ] );
   }

   result += astTSizeOf( this->cvtargs );
   result += astTSizeOf( this->cvttype );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}


static void AddArgs( int cvttype, double *cvtargs, int *status ) {
/*
*  Name:
*     AddArgs

*  Purpose:
*     Set values for addition conversion arguments.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     void AddArgs( int cvttype, double *cvtargs, int *status )

*  Class Membership:
*     TimeMap member function.

*  Description:
*     This function stores value for additional conversion arguments,
*     based on the values supplied for the user arguments.

*  Parameters:
*     cvttype
*        The conversion type.
*     cvtargs
*        The arguments for the conversion.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double r;                     /* Distance from Earth axis (AU) */
   double z;                     /* Distance from plane of Earth equator (AU) */

/* Check the global error status. */
   if ( !astOK ) return;

/* Test for each valid code value in turn and assign the appropriate
   extra values. */
   switch ( cvttype ) {

   case AST__MJDTOMJD:
      cvtargs[ 2 ] = cvtargs[ 0 ] - cvtargs[ 1 ];
      break;

   case AST__MJDTOJD:
      cvtargs[ 2 ] = cvtargs[ 0 ] - cvtargs[ 1 ] + 2400000.5;
      break;

   case AST__JDTOMJD:
      cvtargs[ 2 ] = cvtargs[ 0 ] - cvtargs[ 1 ] - 2400000.5;
      break;

   case AST__MJDTOBEP:
      cvtargs[ 2 ] = palEpb( cvtargs[ 0 ] ) - palEpb( 0.0 ) - cvtargs[ 1 ];
      cvtargs[ 3 ] = palEpb2d( cvtargs[ 1 ] ) - palEpb2d( 0.0 ) - cvtargs[ 0 ];
      break;

   case AST__BEPTOMJD:
      cvtargs[ 2 ] = palEpb2d( cvtargs[ 0 ] ) - palEpb2d( 0.0 ) - cvtargs[ 1 ];
      cvtargs[ 3 ] = palEpb( cvtargs[ 1 ] ) - palEpb( 0.0 ) - cvtargs[ 0 ];
      break;

   case AST__MJDTOJEP:
      cvtargs[ 2 ] = palEpj( cvtargs[ 0 ] ) - palEpj( 0.0 ) - cvtargs[ 1 ];
      cvtargs[ 3 ] = palEpj2d( cvtargs[ 1 ] ) - palEpj2d( 0.0 ) - cvtargs[ 0 ];
      break;

   case AST__JEPTOMJD:
      cvtargs[ 2 ] = palEpj2d( cvtargs[ 0 ] ) - palEpj2d( 0.0 ) - cvtargs[ 1 ];
      cvtargs[ 3 ] = palEpj( cvtargs[ 1 ] ) - palEpj( 0.0 ) - cvtargs[ 0 ];
      break;

   case AST__TTTOTDB:
      palGeoc( cvtargs[ 2 ], cvtargs[ 3 ], &r, &z );
      cvtargs[ 4 ] = 0.001*r*AST__AU;
      cvtargs[ 5 ] = 0.001*z*AST__AU;
      break;

   case AST__TDBTOTT:
      palGeoc( cvtargs[ 2 ], cvtargs[ 3 ], &r, &z );
      cvtargs[ 4 ] = 0.001*r*AST__AU;
      cvtargs[ 5 ] = 0.001*z*AST__AU;
      break;

   case AST__TDBTOTCB:
      cvtargs[ 1 ] = LB*( cvtargs[ 0 ] - (TTOFF/SPD)
                                        - 43144.0 ) + P0/SPD;
      break;

   case AST__TCBTOTDB:
      cvtargs[ 1 ] = LB*( cvtargs[ 0 ] - (TTOFF/SPD)
                                        - 43144.0 ) + P0/SPD;
      break;

   case AST__TTTOTCG:
      cvtargs[ 1 ] = LG*( cvtargs[ 0 ] - (TTOFF/SPD) - 43144.0 );
      break;

   case AST__TCGTOTT:
      cvtargs[ 1 ] = LG*( cvtargs[ 0 ] - (TTOFF/SPD) - 43144.0 );
      break;

   }
}

static void AddTimeCvt( AstTimeMap *this, int cvttype, const double *args, int *status ) {
/*
*  Name:
*     AddTimeCvt

*  Purpose:
*     Add a coordinate conversion step to an TimeMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     void AddTimeCvt( AstTimeMap *this, int cvttype, const double *args )

*  Class Membership:
*     TimeMap member function.

*  Description:
*     This function allows one of the supported time coordinate
*     conversions to be appended to a TimeMap. When a TimeMap is first
*     created (using astTimeMap), it simply performs a unit mapping. By
*     using AddTimeCvt repeatedly, a series of coordinate conversions may
*     then be specified which the TimeMap will subsequently perform in
*     sequence. This allows a complex coordinate conversion to be
*     assembled out of the basic building blocks. The TimeMap will also
*     perform the inverse coordinate conversion (applying the individual
*     conversion steps in reverse) if required.

*  Parameters:
*     this
*        Pointer to the TimeMap.
*     cvttype
*        A code to identify which time coordinate conversion is to be
*        appended.  See the "Coordinate Conversions" section for details
*        of those available.
*     args
*        Pointer to an array of double containing the argument values
*        required to fully specify the required coordinate
*        conversion. The number of arguments depends on the conversion
*        (see the "Coordinate Conversions" section for details). This
*        value is ignored and may be NULL if no arguments are required.

*  Returned Value:
*     void.

*  Coordinate Conversions:
*     The following values may be supplied for the "cvttype" parameter
*     in order to specify the coordinate conversion to be performed.
*     The argument(s) required to fully specify each conversion are
*     indicated in parentheses after each value, and described at the end
*     of the list. Values for these should be given in the array pointed
*     at by "args".
*
*        AST__MJDTOMJD( MJDOFF1, MJDOFF2 )
*           Convert Modified Julian Date from one offset to another.
*        AST__MJDTOJD( MJDOFF, JDOFF )
*           Convert Modified Julian Date to Julian Date.
*        AST__JDTOMJD( JDOFF, MJDOFF )
*           Convert Julian Date to Modified Julian Date.
*        AST__MJDTOBEP( MJDOFF, BEPOFF )
*           Convert Modified Julian Date to Besselian epoch.
*        AST__BEPTOMJD( BEPOFF, MJDOFF )
*           Convert Besselian epoch to Modified Julian Date.
*        AST__MJDTOJEP( MJDOFF, JEPOFF )
*           Convert Modified Julian Date to Julian epoch.
*        AST__JEPTOMJD( JEPOFF, MJDOFF )
*           Convert Julian epoch to Modified Julian Date.
*        AST__TAITOUTC( MJDOFF )
*           Convert a TAI MJD to a UTC MJD.
*        AST__UTCTOTAI( MJDOFF )
*           Convert a UTC MJD to a TAI MJD.
*        AST__TAITOTT( MJDOFF )
*           Convert a TAI MJD to a TT MJD.
*        AST__TTTOTAI( MJDOFF )
*           Convert a TT MJD to a TAI MJD.
*        AST__TTTOTDB( MJDOFF, OBSLON, OBSLAT, OBSALT )
*           Convert a TT MJD to a TDB MJD.
*        AST__TDBTOTT( MJDOFF, OBSLON, OBSLAT, OBSALT )
*           Convert a TDB MJD to a TT MJD.
*        AST__TTTOTCG( MJDOFF )
*           Convert a TT MJD to a TCG MJD.
*        AST__TCGTOTT( MJDOFF )
*           Convert a TCG MJD to a TT MJD.
*        AST__TDBTOTCB( MJDOFF)
*           Convert a TAI MJD to a TCB MJD.
*        AST__TCBTOTDB( MJDOFF)
*           Convert a TCB MJD to a TDB MJD.
*        AST__UTTOGMST( MJDOFF )
*           Convert a UT MJD to a GMST MJD.
*        AST__GMSTTOUT( MJDOFF )
*           Convert a GMST MJD to a UT MJD.
*        AST__GMSTTOLMST( MJDOFF, OBSLON, OBSLAT )
*           Convert a GMST MJD to a LMST MJD.
*        AST__LMSTTOGMST( MJDOFF, OBSLON, OBSLAT )
*           Convert a LMST MJD to a GMST MJD.
*        AST__LASTTOLMST( MJDOFF, OBSLON, OBSLAT )
*           Convert a LAST MJD to a LMST MJD.
*        AST__LMSTTOLAST( MJDOFF, OBSLON, OBSLAT )
*           Convert a LMST MJD to a LAST MJD.
*        AST__UTTOUTC( DUT1 )
*           Convert a UT1 MJD to a UTC MJD.
*        AST__UTCTOUT( DUT1 )
*           Convert a UTC MJD to a UT1 MJD.
*        AST__LTTOUTC( LTOFF )
*           Convert a local time MJD to a UTC MJD.
*        AST__UTCTOLT( LTOFF )
*           Convert a UTC MJD to a local time MJD.
*
*     The units for the values processed by the above conversions are as
*     follows:
*
*     - MJD, MJDOFF, JD, JDOFF: days
*     - Julian epochs, BEPOFF: Tropical years
*     - Besselian epochs, JEPOFF: Julian years
*
*     The arguments used in the above conversions are as follows:
*
*     - MJDOFF: Offset to be added to each MJD value
*     - JDOFF: Offset to be added to each JD value
*     - JEPOFF: Offset to be added to each Julian epoch value
*     - BEPOFF: Offset to be added to each Besselian epoch value
*     - OBSLON: Observer's longitude in radians (+ve westwards)
*     - OBSLAT: Observer's geodetic latitude in radians (+ve northwards)
*     - OBSALT: Observer's geodetic altitude in metres.
*     - DUT1: The value of UT1-UTC
*     - LTOFF: The offset between Local Time and UTC (in hours, positive
*     for time zones east of Greenwich).

*  Notes:
*     - The specified conversion is appended only if the TimeMap's
*     Invert attribute is zero. If it is non-zero, this function
*     effectively prefixes the inverse of the conversion specified
*     instead.
*/

/* Local Variables: */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *cvt_string;       /* Pointer to conversion type string */
   int i;                        /* Argument index */
   int nargs;                    /* Number of user-supplied arguments */
   int ncvt;                     /* Number of coordinate conversions */
   int szargs;                   /* Size of arguments array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the coordinate conversion type and obtain the number of
   required user-supplied arguments, and the size of the array in which
   to put the user-supplied arguments (the array may leave room after
   the user-supplied arguments for various useful pre-calculated values). */
   cvt_string = CvtString( cvttype, &comment, &nargs, &szargs, argdesc, status );

/* If the coordinate conversion type was not valid, then report an
   error. */
   if ( astOK && !cvt_string ) {
      astError( AST__TIMIN, "AddTimeCvt(%s): Invalid time coordinate "
                "conversion type (%d).", status, astGetClass( this ),
                (int) cvttype );
   }

/* Note the number of coordinate conversions already stored in the TimeMap. */
   if ( astOK ) {
      ncvt = this->ncvt;

/* Extend the array of conversion types and the array of pointers to
   their argument lists to accommodate the new one. */
      this->cvttype = (int *) astGrow( this->cvttype, ncvt + 1,
                                       sizeof( int ) );
      this->cvtargs = (double **) astGrow( this->cvtargs, ncvt + 1,
                                           sizeof( double * ) );

/* Allocate memory for the argument list, putting a pointer to it into
   the TimeMap. */
      this->cvtargs[ ncvt ] = astMalloc( sizeof( double ) * (size_t) szargs );

/* Store the conversion type and increment the conversion count. Also
   copy the supplied arguments into the memory allocated above and put
   suitable values in any elements of the argument array which are beyond
   the end of the user-supplied arguments. These are intermediate values
   calculated on the basis of the user-supplied arguments. */
      if ( astOK ) {
         this->cvttype[ ncvt ] = cvttype;
         for( i = 0; i < nargs; i++ ) this->cvtargs[ ncvt ][ i ] = args[ i ];
         for( i = nargs; i < szargs; i++ ) this->cvtargs[ ncvt ][ i ] = AST__BAD;
         this->ncvt++;

/* Test for each valid code value in turn and assign the appropriate extra values. */
         AddArgs( cvttype, this->cvtargs[ ncvt ], status );
      }
   }
}

static int CvtCode( const char *cvt_string, int *status ) {
/*
*  Name:
*     CvtCode

*  Purpose:
*     Convert a conversion type from a string representation to a code value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     int CvtCode( const char *cvt_string, int *status )

*  Class Membership:
*     TimeMap member function.

*  Description:
*     This function accepts a string used to repersent one of the
*     TimeMap coordinate conversions and converts it into a code
*     value for internal use.

*  Parameters:
*     cvt_string
*        Pointer to a constant null-terminated string representing a
*        time coordinate conversion. This is case sensitive and should
*        contain no unnecessary white space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The equivalent conversion code. If the string was not
*     recognised, the code AST__TIME_NULL is returned, without error.

*  Notes:
*     - A value of AST__TIME_NULL will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   int result;                   /* Result value to return */

/* Initialise. */
   result = AST__TIME_NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Test the string against each recognised value in turn and assign
   the result. */
   if ( astChrMatch( cvt_string, "MJDTOJD" ) ) {
      result = AST__MJDTOJD;

   } else if ( astChrMatch( cvt_string, "MJDTOMJD" ) ) {
      result = AST__MJDTOMJD;

   } else if ( astChrMatch( cvt_string, "JDTOMJD" ) ) {
      result = AST__JDTOMJD;

   } else if ( astChrMatch( cvt_string, "JDTOMJD" ) ) {
      result = AST__JDTOMJD;

   } else if ( astChrMatch( cvt_string, "MJDTOBEP" ) ) {
      result = AST__MJDTOBEP;

   } else if ( astChrMatch( cvt_string, "BEPTOMJD" ) ) {
      result = AST__BEPTOMJD;

   } else if ( astChrMatch( cvt_string, "MJDTOJEP" ) ) {
      result = AST__MJDTOJEP;

   } else if ( astChrMatch( cvt_string, "JEPTOMJD" ) ) {
      result = AST__JEPTOMJD;

   } else if ( astChrMatch( cvt_string, "TAITOUTC" ) ) {
      result = AST__TAITOUTC;

   } else if ( astChrMatch( cvt_string, "UTCTOTAI" ) ) {
      result = AST__UTCTOTAI;

   } else if ( astChrMatch( cvt_string, "TAITOTT" ) ) {
      result = AST__TAITOTT;

   } else if ( astChrMatch( cvt_string, "TTTOTAI" ) ) {
      result = AST__TTTOTAI;

   } else if ( astChrMatch( cvt_string, "TTTOTDB" ) ) {
      result = AST__TTTOTDB;

   } else if ( astChrMatch( cvt_string, "TDBTOTT" ) ) {
      result = AST__TDBTOTT;

   } else if ( astChrMatch( cvt_string, "TTTOTCG" ) ) {
      result = AST__TTTOTCG;

   } else if ( astChrMatch( cvt_string, "TCGTOTT" ) ) {
      result = AST__TCGTOTT;

   } else if ( astChrMatch( cvt_string, "TDBTOTCB" ) ) {
      result = AST__TDBTOTCB;

   } else if ( astChrMatch( cvt_string, "TCBTOTDB" ) ) {
      result = AST__TCBTOTDB;

   } else if ( astChrMatch( cvt_string, "UTTOGMST" ) ) {
      result = AST__UTTOGMST;

   } else if ( astChrMatch( cvt_string, "GMSTTOUT" ) ) {
      result = AST__GMSTTOUT;

   } else if ( astChrMatch( cvt_string, "GMSTTOLMST" ) ) {
      result = AST__GMSTTOLMST;

   } else if ( astChrMatch( cvt_string, "LMSTTOGMST" ) ) {
      result = AST__LMSTTOGMST;

   } else if ( astChrMatch( cvt_string, "LASTTOLMST" ) ) {
      result = AST__LASTTOLMST;

   } else if ( astChrMatch( cvt_string, "LMSTTOLAST" ) ) {
      result = AST__LMSTTOLAST;

   } else if ( astChrMatch( cvt_string, "UTTOUTC" ) ) {
      result = AST__UTTOUTC;

   } else if ( astChrMatch( cvt_string, "UTCTOUT" ) ) {
      result = AST__UTCTOUT;

   } else if ( astChrMatch( cvt_string, "LTTOUTC" ) ) {
      result = AST__LTTOUTC;

   } else if ( astChrMatch( cvt_string, "UTCTOLT" ) ) {
      result = AST__UTCTOLT;
   }

/* Return the result. */
   return result;
}

static const char *CvtString( int cvt_code, const char **comment,
                              int *nargs, int *szargs,
                              const char *arg[ MAX_ARGS ], int *status ) {
/*
*  Name:
*     CvtString

*  Purpose:
*     Convert a conversion type from a code value to a string representation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     const char *CvtString( int cvt_code, const char **comment, int *nargs,
*                            int *szargs, const char *arg[ MAX_ARGS ], int *status )

*  Class Membership:
*     TimeMap member function.

*  Description:
*     This function accepts a code value used to represent one of the
*     TimeMap coordinate conversions and converts it into an
*     equivalent string representation. It also returns a descriptive
*     comment and information about the arguments required in order to
*     perform the conversion.

*  Parameters:
*     cvt_code
*        The conversion code.
*     comment
*        Address of a location to return a pointer to a constant
*        null-terminated string containing a description of the
*        conversion.
*     nargs
*        Address of an int in which to return the number of arguments
*        required from the user in order to perform the conversion (may
*        be zero).
*     szargs
*        Address of an int in which to return the number of arguments
*        associated with the conversion. This may be bigger than "nargs"
*        if the conversion can pre-calculate useful values on the basis
*        of the user-supplied values. Such precalculated values are
*        stored after the last user-supplied argument.
*     arg
*        An array in which to return a pointer to a constant
*        null-terminated string for each argument (above) containing a
*        description of what each argument represents. This includes both
*        user-supplied arguments and pre-calculated values.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a constant null-terminated string representation of
*     the conversion code value supplied. If the code supplied is not
*     valid, a NULL pointer will be returned, without error.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;          /* Result pointer to return */

/* Initialise the returned values. */
   *comment = NULL;
   *nargs = 0;
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Test for each valid code value in turn and assign the appropriate
   return values. */
   switch ( cvt_code ) {

   case AST__MJDTOMJD:
      *comment = "Convert MJD between offsets";
      result = "MJDTOMJD";
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "Input MJD offset";
      arg[ 1 ] = "Output MJD offset";
      arg[ 2 ] = "Combined offset";
      break;

   case AST__MJDTOJD:
      *comment = "Convert MJD to JD";
      result = "MJDTOJD";
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "JD offset";
      arg[ 2 ] = "Combined offset";
      break;

   case AST__JDTOMJD:
      *comment = "Convert JD to MJD";
      result = "JDTOMJD";
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "JD offset";
      arg[ 1 ] = "MJD offset";
      arg[ 2 ] = "Combined offset";
      break;

   case AST__MJDTOBEP:
      *comment = "Convert MJD to Besselian epoch";
      result = "MJDTOBEP";
      *nargs = 2;
      *szargs = 4;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Besselian epoch offset";
      arg[ 2 ] = "Combined forward offset";
      arg[ 3 ] = "Combined inverse offset";
      break;

   case AST__BEPTOMJD:
      *comment = "Convert Besselian epoch to MJD";
      result = "BEPTOMJD";
      *nargs = 2;
      *szargs = 4;
      arg[ 0 ] = "Besselian epoch offset";
      arg[ 1 ] = "MJD offset";
      arg[ 2 ] = "Combined forward offset";
      arg[ 3 ] = "Combined inverse offset";
      break;

   case AST__MJDTOJEP:
      *comment = "Convert MJD to Julian epoch";
      result = "MJDTOJEP";
      *nargs = 2;
      *szargs = 4;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Julian epoch offset";
      arg[ 2 ] = "Combined forward offset";
      arg[ 3 ] = "Combined inverse offset";
      break;

   case AST__JEPTOMJD:
      *comment = "Convert Julian epoch to MJD";
      result = "JEPTOMJD";
      *nargs = 2;
      *szargs = 4;
      arg[ 0 ] = "Julian epoch offset";
      arg[ 1 ] = "MJD offset";
      arg[ 2 ] = "Combined forward offset";
      arg[ 3 ] = "Combined inverse offset";
      break;

   case AST__TAITOUTC:
      *comment = "Convert TAI to UTC";
      result = "TAITOUTC";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__UTCTOTAI:
      *comment = "Convert UTC to TAI";
      result = "UTCTOTAI";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__TAITOTT:
      *comment = "Convert TAI to TT";
      result = "TAITOTT";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__TTTOTAI:
      *comment = "Convert TT to TAI";
      result = "TTTOTAI";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__TTTOTDB:
      *comment = "Convert TT to TDB";
      result = "TTTOTDB";
      *nargs = 4;
      *szargs = 6;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      arg[ 3 ] = "Observer altitude";
      arg[ 4 ] = "Distance from earth spin axis";
      arg[ 5 ] = "Distance north of equatorial plane";
      break;

   case AST__TDBTOTT:
      *comment = "Convert TDB to TT";
      result = "TDBTOTT";
      *nargs = 4;
      *szargs = 6;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      arg[ 3 ] = "Observer altitude";
      arg[ 4 ] = "Distance from earth spin axis";
      arg[ 5 ] = "Distance north of equatorial plane";
      break;

   case AST__TTTOTCG:
      *comment = "Convert TT to TCG";
      result = "TTTOTCG";
      *nargs = 1;
      *szargs = 2;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "TCG offset";
      break;

   case AST__TCGTOTT:
      *comment = "Convert TCG to TT";
      result = "TCGTOTT";
      *nargs = 1;
      *szargs = 2;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "TCG offset";
      break;

   case AST__TDBTOTCB:
      *comment = "Convert TDB to TCB";
      result = "TDBTOTCB";
      *nargs = 1;
      *szargs = 2;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "TCB offset";
      break;

   case AST__TCBTOTDB:
      *comment = "Convert TCB to TDB";
      result = "TCBTOTDB";
      *nargs = 1;
      *szargs = 2;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "TCB offset";
      break;

   case AST__UTTOGMST:
      *comment = "Convert UT to GMST";
      result = "UTTOGMST";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__GMSTTOUT:
      *comment = "Convert GMST to UT";
      result = "GMSTTOUT";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "MJD offset";
      break;

   case AST__GMSTTOLMST:
      *comment = "Convert GMST to LMST";
      result = "GMSTTOLMST";
      *nargs = 3;
      *szargs = 3;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      break;

   case AST__LMSTTOGMST:
      *comment = "Convert LMST to GMST";
      result = "LMSTTOGMST";
      *nargs = 3;
      *szargs = 3;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      break;

   case AST__LASTTOLMST:
      *comment = "Convert LAST to LMST";
      result = "LASTTOLMST";
      *nargs = 3;
      *szargs = 3;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      break;

   case AST__LMSTTOLAST:
      *comment = "Convert LMST to LAST";
      result = "LMSTTOLAST";
      *nargs = 3;
      *szargs = 3;
      arg[ 0 ] = "MJD offset";
      arg[ 1 ] = "Observer longitude";
      arg[ 2 ] = "Observer latitude";
      break;

   case AST__UTTOUTC:
      *comment = "Convert UT1 to UTC";
      result = "UTTOUTC";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "DUT1";
      break;

   case AST__UTCTOUT:
      *comment = "Convert UTC to UT1";
      result = "UTCTOUT";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "DUT1";
      break;

   case AST__LTTOUTC:
      *comment = "Convert Local Time to UTC";
      result = "LTTOUTC";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "LTOFF";
      break;

   case AST__UTCTOLT:
      *comment = "Convert UTC to Local Time";
      result = "UTCTOLT";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "LTOFF";
      break;
   }

/* Return the result. */
   return result;
}

double astDat_( double in, int forward, int *status ){
/*
*+
*  Name:
*     Dat

*  Purpose:
*     Convert between UTC and TAI.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "timemap.h"
*     double astDat( double in, int forward )

*  Class Membership:
*     TimeMap member function

*  Description:
*     This function returns the difference between Coordinated Universal Time
*     (UTC) and International Atomic Time (TAI), at a given epoch.

*  Parameters:
*     in
*        UTC date or TAI time (as selected by "forward"), as an absolute
*        MJD.
*     forward
*        If non-zero, "in" should be a UTC value, and the returned value
*        is TAI-UTC. If zero, "in" should be a TAI value, and the returned
*        value is UTC-TAI.

*  Returned Value:
*     Either UTC-TAI or TAI-UTC (as indicated by "forward") in units of
*     seconds.

*  Notes:
*     - The UTC is specified to be a date rather than a time to indicate
*     that care needs to be taken not to specify an instant which lies
*     within a leap second.  Though in most cases UTC can include the
*     fractional part, correct behaviour on the day of a leap second
*     can only be guaranteed up to the end of the second 23:59:59.
*     - For epochs from 1961 January 1 onwards, the expressions from the
*     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
*     - The 5ms time step at 1961 January 1 is taken from 2.58.1 (p87) of
*     the 1992 Explanatory Supplement.
*     - UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
*     to call the routine with an earlier epoch.  However, if this
*     is attempted, the TAI-UTC expression for the year 1960 is used.

*  Implementation Details:
*     - This function is based on SLA_DAT by P.T.Wallace.
*     - This routine must be updated on each occasion that a leap second is
*     announced
*     - Latest leap second:  2015 July 1

*-
*/

/* Local Variables: */
   double result;

/* Initialise the returned value. */
   if( in == AST__BAD ) return AST__BAD;

/* First do TAI-UTC at a given UTC
   ------------------------------- */
   if( forward ) {

/* 2015 July 1 */
      if ( in >= 57204.0 ) {
         result = 36.0;

/* 2012 July 1 */
      } else if ( in >= 56109.0 ) {
         result = 35.0;

/* 2009 January 1 */
      } else if ( in >= 54832.0 ) {
         result = 34.0;

/* 2006 January 1 */
      } else if( in >= 53736.0 ) {
         result = 33.0;

/* 1999 January 1 */
      } else if( in >= 51179.0 ){
         result = 32.0;

/* 1997 July 1 */
      } else if( in >= 50630.0 ){
         result = 31.0;

/* 1996 January 1 */
      } else if( in >= 50083.0 ){
         result = 30.0;

/* 1994 July 1 */
      } else if( in >= 49534.0 ){
         result = 29.0;

/* 1993 July 1 */
      } else if( in >= 49169.0 ){
         result = 28.0;

/* 1992 July 1 */
      } else if( in >= 48804.0 ){
         result = 27.0;

/* 1991 January 1 */
      } else if( in >= 48257.0 ){
         result = 26.0;

/* 1990 January 1 */
      } else if( in >= 47892.0 ){
         result = 25.0;

/* 1988 January 1 */
      } else if( in >= 47161.0 ){
         result = 24.0;

/* 1985 July 1 */
      } else if( in >= 46247.0 ){
         result = 23.0;

/* 1983 July 1 */
      } else if( in >= 45516.0 ){
         result = 22.0;

/* 1982 July 1 */
      } else if( in >= 45151.0 ){
         result = 21.0;

/* 1981 July 1 */
      } else if( in >= 44786.0 ){
         result = 20.0;

/* 1980 January 1 */
      } else if( in >= 44239.0 ){
         result = 19.0;

/* 1979 January 1 */
      } else if( in >= 43874.0 ){
         result = 18.0;

/* 1978 January 1 */
      } else if( in >= 43509.0 ){
         result = 17.0;

/* 1977 January 1 */
      } else if( in >= 43144.0 ){
         result = 16.0;

/* 1976 January 1 */
      } else if( in >= 42778.0 ){
         result = 15.0;

/* 1975 January 1 */
      } else if( in >= 42413.0 ){
         result = 14.0;

/* 1974 January 1 */
      } else if( in >= 42048.0 ){
         result = 13.0;

/* 1973 January 1 */
      } else if( in >= 41683.0 ){
         result = 12.0;

/* 1972 July 1 */
      } else if( in >= 41499.0 ){
         result = 11.0;

/* 1972 January 1 */
      } else if( in >= 41317.0 ){
         result = 10.0;

/* 1968 February 1 */
      } else if( in >= 39887.0 ){
         result = 4.2131700 + ( in - 39126.0 )*0.002592;

/* 1966 January 1 */
      } else if( in >= 39126.0 ){
         result = 4.3131700 + ( in - 39126.0 )*0.002592;

/* 1965 September 1 */
      } else if( in >= 39004.0 ){
         result = 3.8401300 + ( in - 38761.0 )*0.001296;

/* 1965 July 1 */
      } else if( in >= 38942.0 ){
         result = 3.7401300 + ( in - 38761.0 )*0.001296;

/* 1965 March 1 */
      } else if( in >= 38820.0 ){
         result = 3.6401300 + ( in - 38761.0 )*0.001296;

/* 1965 January 1 */
      } else if( in >= 38761.0 ){
         result = 3.5401300 + ( in - 38761.0 )*0.001296;

/* 1964 September 1 */
      } else if( in >= 38639.0 ){
         result = 3.4401300 + ( in - 38761.0 )*0.001296;

/* 1964 April 1 */
      } else if( in >= 38486.0 ){
         result = 3.3401300 + ( in - 38761.0 )*0.001296;

/* 1964 January 1 */
      } else if( in >= 38395.0 ){
         result = 3.2401300 + ( in - 38761.0 )*0.001296;

/* 1963 November 1 */
      } else if( in >= 38334.0 ){
         result = 1.9458580 + ( in - 37665.0 )*0.0011232;

/* 1962 January 1 */
      } else if( in >= 37665.0 ){
         result = 1.8458580 + ( in - 37665.0 )*0.0011232;

/* 1961 August 1 */
      } else if( in >= 37512.0 ){
         result = 1.3728180 + ( in - 37300.0 )*0.001296;

/* 1961 January 1 */
      } else if( in >= 37300.0 ){
         result = 1.4228180 + ( in - 37300.0 )*0.001296;

/* Before that */
      } else {
         result = 1.4178180 + ( in - 37300.0 )*0.001296;
      }

/* Now do UTC-TAI at a given TAI.
   ------------------------------  */
   } else {


/* 2015 July 1 */
      if ( in >= 57204.0 + 36.0/SPD ) {
         result = -36.0;

/* 2012 July 1 */
      } else if( in >= 56109.0 + 35.0/SPD ) {
         result = -35.0;

/* 2009 January 1 */
      } else if( in >= 54832.0 + 34.0/SPD ) {
         result = -34.0;

/* 2006 January 1 */
      } else if( in >= 53736.0 + 33.0/SPD ){
         result = -33.0;

/* 1999 January 1 */
      } else if( in >= 51179.0 + 32.0/SPD ){
         result = -32.0;

/* 1997 July 1 */
      } else if( in >= 50630.0 + 31.0/SPD ){
         result = -31.0;

/* 1996 January 1 */
      } else if( in >= 50083.0 + 30.0/SPD ){
         result = -30.0;

/* 1994 July 1 */
      } else if( in >= 49534.0 + 29.0/SPD ){
         result = -29.0;

/* 1993 July 1 */
      } else if( in >= 49169.0 + 28.0/SPD ){
         result = -28.0;

/* 1992 July 1 */
      } else if( in >= 48804.0 + 27.0/SPD ){
         result = -27.0;

/* 1991 January 1 */
      } else if( in >= 48257.0 + 26.0/SPD ){
         result = -26.0;

/* 1990 January 1 */
      } else if( in >= 47892.0 + 25.0/SPD ){
         result = -25.0;

/* 1988 January 1 */
      } else if( in >= 47161.0 + 24.0/SPD ){
         result = -24.0;

/* 1985 July 1 */
      } else if( in >= 46247.0 + 23.0/SPD ){
         result = -23.0;

/* 1983 July 1 */
      } else if( in >= 45516.0 + 22.0/SPD ){
         result = -22.0;

/* 1982 July 1 */
      } else if( in >= 45151.0 + 21.0/SPD ){
         result = -21.0;

/* 1981 July 1 */
      } else if( in >= 44786.0 + 20.0/SPD ){
         result = -20.0;

/* 1980 January 1 */
      } else if( in >= 44239.0 + 19.0/SPD ){
         result = -19.0;

/* 1979 January 1 */
      } else if( in >= 43874.0 + 18.0/SPD ){
         result = -18.0;

/* 1978 January 1 */
      } else if( in >= 43509.0 + 17.0/SPD ){
         result = -17.0;

/* 1977 January 1 */
      } else if( in >= 43144.0 + 16.0/SPD ){
         result = -16.0;

/* 1976 January 1 */
      } else if( in >= 42778.0 + 15.0/SPD ){
         result = -15.0;

/* 1975 January 1 */
      } else if( in >= 42413.0 + 14.0/SPD ){
         result = -14.0;

/* 1974 January 1 */
      } else if( in >= 42048.0 + 13.0/SPD ){
         result = -13.0;

/* 1973 January 1 */
      } else if( in >= 41683.0 + 12.0/SPD ){
         result = -12.0;

/* 1972 July 1 */
      } else if( in >= 41499.0 + 11.0/SPD ){
         result = -11.0;

/* 1972 January 1 */
      } else if( in >= 41317.0 + 10.0/SPD ){
         result = -10.0;

/* 1968 February 1 */
      } else if( in >= 39887.0 + ( 4.2131700
                                  + ( 39887.0 - 39126.0 )*0.002592 )/SPD ){
         result = -( 4.2131700 + ( in - 39126.0 )*0.002592 )/1.02592;

/* 1966 January 1 */
      } else if( in >= 39126.0 + ( 4.3131700
                                  + ( 39126.0 - 39126.0 )*0.002592 )/SPD ){
         result = -( 4.2131700 + ( in - 39126.0 )*0.002592 )/1.02592;

/* 1965 September 1 */
      } else if( in >= 39004.0 + ( 3.8401300
                                  + ( 39004.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.8401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1965 July 1 */
      } else if( in >= 38942.0 + ( 3.7401300
                                  + ( 38942.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.7401300 + ( in - 38761.0 )*0.001296 )/1.01296;

/* 1965 March 1 */
      } else if( in >= 38820.0 + ( 3.6401300
                                  + ( 38820.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.6401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1965 January 1 */
      } else if( in >= 38761.0 + ( 3.5401300
                                   + ( 38761.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.5401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1964 September 1 */
      } else if( in >= 38639.0 + ( 3.4401300
                                   + ( 38639.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.4401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1964 April 1 */
      } else if( in >= 38486.0 + ( 3.3401300
                                   + ( 38486.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.3401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1964 January 1 */
      } else if( in >= 38395.0 + ( 3.2401300
                                   + ( 38395.0 - 38761.0 )*0.001296 )/SPD ){
         result = -( 3.2401300 + ( in - 38761.0 )*0.001296 )/1.001296;

/* 1963 November 1 */
      } else if( in >= 38334.0 + ( 1.9458580
                                   + ( 38334.0 - 37665.0 )*0.0011232 )/SPD ){
         result = -( 1.9458580 + ( in - 37665.0 )*0.0011232 )/1.0011232;

/* 1962 January 1 */
      } else if( in >= 37665.0 + ( 1.8458580
                                   + ( 37665.0 - 37665.0 )*0.0011232 )/SPD ){
         result = -( 1.8458580 + ( in - 37665.0 )*0.0011232 )/1.0011232;

/* 1961 August 1 */
      } else if( in >= 37512.0 + ( 1.3728180
                                   + ( 37512.0 - 37300.0 )*0.001296 )/SPD ){
         result = -( 1.3728180 + ( in - 37300.0 )*0.001296 )/1.001296;

/* 1961 January 1 */
      } else if( in >= 37300.0 + ( 1.4228180
                                   + ( 37300.0 - 37300.0 )*0.001296 )/SPD ){
         result = -( 1.4228180 + ( in - 37300.0 )*0.001296 )/1.001296;

/* Before that */
      } else {
         result = -( 1.4178180 + ( in - 37300.0 )*0.001296 )/1.001296;
      }
   }

/* Return the result */
   return result;
}

static double Gmsta( double in, double off, int forward, int *status ){
/*
*  Name:
*     Gmsta

*  Purpose:
*     Convert between  Universal Time (UT) and Greenwich Mean Sidereal Time (GMST).

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     double Gmsta( double in, double off, int forward, int *status ){

*  Class Membership:
*     TimeMap member function

*  Description:
*     This functions converts between UT and GMST. Both timescales are
*     represented by an MJD, rather than as an angle (as is done by SLALIB)
*     in order to facilitate conversions from GMST to UT1. This means
*     that whole days are retained.

*  Parameters:
*     in
*        The time to convert, represented as an offset in days from the MJD
*        zero-point specified by "off". The time is either UT1 or GMST, as
*        selected by "forward").
*     off
*        The MJD value corresponding to a value of 0.0 for "in".
*     forward
*        If non-zero, "in" should be a UT1 value, and the returned value
*        is GMST. If zero, "in" should be a GMST value, and the returned
*        value is UT1.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     An offset in days from the MJD given by "off". When the returned
*     value is added to "off" the sum is a GMST MJD (if "forward" is
*     non-zero), or a UT1 MJD (if "forward" is zero),

*  Notes:
*     - This function is based on SLA_GMST by P.T.Wallace.

*/

/* Local Variables: */
   double dgdu;
   double g;
   double result;
   double t;
   double utl;
   int nit;

/* Initialise the returned value. */
   if( in == AST__BAD || off == AST__BAD ) return AST__BAD;

/* First deal with UT1 -> GMST
   --------------------------- */
   if( forward ) {

/* Julian centuries since J2000. */
      t = ( off + in - 51544.5 )/36525.0;

/* GMST at this UT1. */
      result = in + ( 24110.54841 + ( 8640184.812866 + ( 0.093104 -
                      6.2E-6*t )*t )*t )/86400.0;

/* Now deal with GMST -> UT1
   ----------------------- */
   } else {

/* Form an initial guess at the UT1 value using the inverse of a linear
   approximation to the UT1->GMST equation. */
      result = 0.996997348638869*in + 154.49194372222 - 0.00300265136113098*off;

/* Loop round improving the guess, until the guess stops changing, or 10
   iterations have been performed. */
      utl = AST__BAD;
      nit = 0;
      while( result != utl && nit++ < 10 ){

/* Calculate the GMST at the current UT1 guess. */
         t = ( off + result - 51544.5 )/36525.0;
         g = result + ( 24110.54841 + ( 8640184.812866 + ( 0.093104 -
                      6.2E-6*t )*t )*t )/86400.0;

/* Calculate the rate of change of GMST with respect to UT1 at the current
   UT1 guess. */
         dgdu = 1.0 + ( 8640184.812866 +
                  ( 0.186208 - 12.4E-6*t )*t)/(36525.0*86400.0);

/* Improve the UT1 guess. */
         utl = result;
         result = result - ( g - in )/dgdu;
      }
   }

/* Return the result */
   return result;
}

void astInitTimeMapVtab_(  AstTimeMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitTimeMapVtab

*  Purpose:
*     Initialise a virtual function table for a TimeMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "timemap.h"
*     void astInitTimeMapVtab( AstTimeMapVtab *vtab, const char *name )

*  Class Membership:
*     TimeMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the TimeMap class.

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
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsATimeMap) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->TimeAdd = TimeAdd;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_rate = mapping->Rate;
   mapping->Rate = Rate;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "TimeMap",
               "Conversion between time coordinate systems" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a TimeMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     TimeMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated TimeMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated TimeMap with one which it
*     considers simpler, or to merge it with the Mappings which
*     immediately precede it or follow it in the sequence (both will
*     normally be considered). This is sufficient to ensure the
*     eventual simplification of most Mapping sequences by repeated
*     application of this function.
*
*     In some cases, the function may attempt more elaborate
*     simplification, involving any number of other Mappings in the
*     sequence. It is not restricted in the type or scope of
*     simplification it may perform, but will normally only attempt
*     elaborate simplification in cases where a more straightforward
*     approach is not adequate.

*  Parameters:
*     this
*        Pointer to the nominated TimeMap which is to be merged with
*        its neighbours. This should be a cloned copy of the TimeMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        TimeMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated TimeMap resides.
*     series
*        A non-zero value indicates that the sequence of Mappings to
*        be simplified will be applied in series (i.e. one after the
*        other), whereas a zero value indicates that they will be
*        applied in parallel (i.e. on successive sub-sets of the
*        input/output coordinates).
*     nmap
*        Address of an int which counts the number of Mappings in the
*        sequence. On entry this should be set to the initial number
*        of Mappings. On exit it will be updated to record the number
*        of Mappings remaining after simplification.
*     map_list
*        Address of a pointer to a dynamically allocated array of
*        Mapping pointers (produced, for example, by the astMapList
*        method) which identifies the sequence of Mappings. On entry,
*        the initial sequence of Mappings to be simplified should be
*        supplied.
*
*        On exit, the contents of this array will be modified to
*        reflect any simplification carried out. Any form of
*        simplification may be performed. This may involve any of: (a)
*        removing Mappings by annulling any of the pointers supplied,
*        (b) replacing them with pointers to new Mappings, (c)
*        inserting additional Mappings and (d) changing their order.
*
*        The intention is to reduce the number of Mappings in the
*        sequence, if possible, and any reduction will be reflected in
*        the value of "*nmap" returned. However, simplifications which
*        do not reduce the length of the sequence (but improve its
*        execution time, for example) may also be performed, and the
*        sequence might conceivably increase in length (but normally
*        only in order to split up a Mapping into pieces that can be
*        more easily merged with their neighbours on subsequent
*        invocations of this function).
*
*        If Mappings are removed from the sequence, any gaps that
*        remain will be closed up, by moving subsequent Mapping
*        pointers along in the array, so that vacated elements occur
*        at the end. If the sequence increases in length, the array
*        will be extended (and its pointer updated) if necessary to
*        accommodate any new elements.
*
*        Note that any (or all) of the Mapping pointers supplied in
*        this array may be annulled by this function, but the Mappings
*        to which they refer are not modified in any way (although
*        they may, of course, be deleted if the annulled pointer is
*        the final one).
*     invert_list
*        Address of a pointer to a dynamically allocated array which,
*        on entry, should contain values to be assigned to the Invert
*        attributes of the Mappings identified in the "*map_list"
*        array before they are applied (this array might have been
*        produced, for example, by the astMapList method). These
*        values will be used by this function instead of the actual
*        Invert attributes of the Mappings supplied, which are
*        ignored.
*
*        On exit, the contents of this array will be updated to
*        correspond with the possibly modified contents of the
*        "*map_list" array.  If the Mapping sequence increases in
*        length, the "*invert_list" array will be extended (and its
*        pointer updated) if necessary to accommodate any new
*        elements.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If simplification was possible, the function returns the index
*     in the "map_list" array of the first element which was
*     modified. Otherwise, it returns -1 (and makes no changes to the
*     arrays supplied).

*  Notes:
*     - A value of -1 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapping *new;              /* Pointer to replacement Mapping */
   AstTimeMap *timemap;            /* Pointer to TimeMap */
   const char *argdesc[ MAX_ARGS ]; /* Argument descriptions (junk) */
   const char *class;            /* Pointer to Mapping class string */
   const char *comment;          /* Pointer to comment string (junk) */
   double (*cvtargs)[ MAX_ARGS ]; /* Pointer to argument arrays */
   double tmp;                   /* Temporary storage */
   int *cvttype;                 /* Pointer to transformation type codes */
   int *szarg;                   /* Pointer to argument count array */
   int done;                     /* Finished (no further simplification)? */
   int iarg;                     /* Loop counter for arguments */
   int icvt1;                    /* Loop initial value */
   int icvt2;                    /* Loop final value */
   int icvt;                     /* Loop counter for transformation steps */
   int ikeep;                    /* Index to store step being kept */
   int imap1;                    /* Index of first TimeMap to merge */
   int imap2;                    /* Index of last TimeMap to merge */
   int imap;                     /* Loop counter for Mappings */
   int inc;                      /* Increment for transformation step loop */
   int invert;                   /* TimeMap applied in inverse direction? */
   int istep;                    /* Loop counter for transformation steps */
   int keep;                     /* Keep transformation step? */
   int narg;                     /* Number of user-supplied arguments */
   int ngone;                    /* Number of Mappings eliminated */
   int nstep0;                   /* Original number of transformation steps */
   int nstep;                    /* Total number of transformation steps */
   int result;                   /* Result value to return */
   int simpler;                  /* Simplification possible? */
   int unit;                     /* Replacement Mapping is a UnitMap? */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* TimeMaps can only be merged if they are in series (or if there is
   only one Mapping present, in which case it makes no difference), so
   do nothing if they are not. */
   if ( series || ( *nmap == 1 ) ) {

/* Initialise the number of transformation steps to be merged to equal
   the number in the nominated TimeMap. */
      nstep = ( (AstTimeMap *) ( *map_list )[ where ] )->ncvt;

/* Search adjacent lower-numbered Mappings until one is found which is
   not a TimeMap. Accumulate the number of transformation steps involved in
   any TimeMaps found. */
      imap1 = where;
      while ( ( imap1 - 1 >= 0 ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap1 - 1 ] );
         if ( !astOK || strcmp( class, "TimeMap" ) ) break;
         nstep += ( (AstTimeMap *) ( *map_list )[ imap1 - 1 ] )->ncvt;
         imap1--;
      }

/* Similarly search adjacent higher-numbered Mappings. */
      imap2 = where;
      while ( ( imap2 + 1 < *nmap ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap2 + 1 ] );
         if ( !astOK || strcmp( class, "TimeMap" ) ) break;
         nstep += ( (AstTimeMap *) ( *map_list )[ imap2 + 1 ] )->ncvt;
         imap2++;
      }

/* Remember the initial number of transformation steps. */
      nstep0 = nstep;

/* Allocate memory for accumulating a list of all the transformation
   steps involved in all the TimeMaps found. */
      cvttype = astMalloc( sizeof( int ) * (size_t) nstep );
      cvtargs = astMalloc( sizeof( double[ MAX_ARGS ] ) * (size_t) nstep );
      szarg = astMalloc( sizeof( int ) * (size_t) nstep );

/* Loop to obtain the transformation data for each TimeMap being merged. */
      nstep = 0;
      for ( imap = imap1; astOK && ( imap <= imap2 ); imap++ ) {

/* Obtain a pointer to the TimeMap and note if it is being applied in
   its inverse direction. */
         timemap = (AstTimeMap *) ( *map_list )[ imap ];
         invert = ( *invert_list )[ imap ];

/* Set up loop limits and an increment to scan the transformation
   steps in each TimeMap in either the forward or reverse direction, as
   dictated by the associated "invert" value. */
         icvt1 = invert ? timemap->ncvt - 1 : 0;
         icvt2 = invert ? -1 : timemap->ncvt;
         inc = invert ? -1 : 1;

/* Loop through each transformation step in the TimeMap. */
         for ( icvt = icvt1; icvt != icvt2; icvt += inc ) {

/* Store the transformation type code and use "CvtString" to determine
   the associated number of arguments. Then store these arguments. */
            cvttype[ nstep ] = timemap->cvttype[ icvt ];
            (void) CvtString( cvttype[ nstep ], &comment,
                              &narg, szarg + nstep, argdesc, status );
            if ( !astOK ) break;
            for ( iarg = 0; iarg < szarg[ nstep ]; iarg++ ) {
               cvtargs[ nstep ][ iarg ] = timemap->cvtargs[ icvt ][ iarg ];
            }

/* If the TimeMap is inverted, we must not only accumulate its
   transformation steps in reverse, but also apply them in
   reverse. For some steps this means changing arguments, for some it
   means changing the transformation type code to a complementary
   value, and for others it means both.  Define macros to perform each
   of the required changes. */

/* Macro to exchange a transformation type code for its inverse (and
   vice versa). */
#define SWAP_CODES( code1, code2 ) \
            if ( cvttype[ nstep ] == code1 ) { \
               cvttype[ nstep ] = code2; \
               AddArgs(  code2, cvtargs[ nstep ], status ); \
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
               AddArgs(  code1, cvtargs[ nstep ], status ); \
            }

/* Macro to exchange a transformation type code for its inverse (and
   vice versa), and swap the order of its 2 arguments. */
#define SWAP_CODES2( code1, code2 ) \
            if ( cvttype[ nstep ] == code1 ) { \
               cvttype[ nstep ] = code2; \
               tmp = cvtargs[ nstep ][ 0 ]; \
               cvtargs[ nstep ][ 0 ] = cvtargs[ nstep ][ 1 ]; \
               cvtargs[ nstep ][ 1 ] = tmp; \
               AddArgs(  cvttype[ nstep ], cvtargs[ nstep ], status ); \
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
               tmp = cvtargs[ nstep ][ 0 ]; \
               cvtargs[ nstep ][ 0 ] = cvtargs[ nstep ][ 1 ]; \
               cvtargs[ nstep ][ 1 ] = tmp; \
               AddArgs(  cvttype[ nstep ], cvtargs[ nstep ], status ); \
            }

/* Use these macros to apply the changes where needed. */
            if ( invert ) {

/* Exchange transformation codes for their inverses. */
               SWAP_CODES( AST__TAITOUTC, AST__UTCTOTAI )
               SWAP_CODES( AST__TAITOTT, AST__TTTOTAI )
               SWAP_CODES( AST__TTTOTDB, AST__TDBTOTT )
               SWAP_CODES( AST__TDBTOTCB, AST__TCBTOTDB )
               SWAP_CODES( AST__TTTOTCG, AST__TCGTOTT )
               SWAP_CODES( AST__UTTOGMST, AST__GMSTTOUT )
               SWAP_CODES( AST__GMSTTOLMST, AST__LMSTTOGMST )
               SWAP_CODES( AST__LASTTOLMST, AST__LMSTTOLAST )
               SWAP_CODES( AST__UTTOUTC, AST__UTCTOUT )
               SWAP_CODES( AST__LTTOUTC, AST__UTCTOLT )

/* Exchange transformation codes for their inverses, and swap the offset
   values. */
               SWAP_CODES2( AST__MJDTOMJD, AST__MJDTOMJD )
               SWAP_CODES2( AST__MJDTOJD, AST__JDTOMJD )
               SWAP_CODES2( AST__MJDTOBEP, AST__BEPTOMJD )
               SWAP_CODES2( AST__MJDTOJEP, AST__JEPTOMJD )

            }

/* Undefine the local macros. */
#undef SWAP_CODES
#undef SWAP_CODES2

/* Count the transformation steps. */
            nstep++;
         }
      }

/* Loop to simplify the sequence of transformation steps until no
   further improvement is possible. */
      done = 0;
      while ( astOK && !done ) {

/* Examine each remaining transformation step in turn.  */
         ikeep = -1;
         for ( istep = 0; istep < nstep; istep++ ) {

/* Initially assume we will retain the current step. */
            keep = 1;

/* We can eliminate changes of system which have no effect. */
            if( ( cvttype[ istep ] == AST__MJDTOMJD ||
                  cvttype[ istep ] == AST__MJDTOJD ||
                  cvttype[ istep ] == AST__JDTOMJD ) &&
                cvtargs[ istep ][ 2 ] == 0.0 ) {
               keep = 0;

/* The only simplifications for the conversions currently in this class act
   to combine adjacent transformation steps, so only apply them while there
   are at least 2 steps left. */
            } else if ( istep < ( nstep - 1 ) ) {

/* Define a macro to test if two adjacent transformation type codes
   have specified values. */
#define PAIR_CVT( code1, code2 ) \
               ( ( cvttype[ istep ] == code1 ) && \
                 ( cvttype[ istep + 1 ] == code2 ) )

/* Define a macro to test if two adjacent transformation type codes
   have specified values, either way round. */
#define PAIR_CVT2( code1, code2 ) \
               ( ( PAIR_CVT( code1, code2 ) ) || \
                 ( PAIR_CVT( code2, code1 ) ) )

/* If a correction is followed by its inverse, and the user-supplied argument
   values are unchanged (we do not need to test values stored in the
   argument array which were not supplied by the user), we can eliminate them.
   First check for conversions which have a single user-supplied argument. */
               if( ( PAIR_CVT2( AST__TAITOUTC, AST__UTCTOTAI ) ||
                     PAIR_CVT2( AST__TAITOTT, AST__TTTOTAI ) ||
                     PAIR_CVT2( AST__UTTOGMST, AST__GMSTTOUT ) ||
                     PAIR_CVT2( AST__TTTOTCG, AST__TCGTOTT ) ||
                     PAIR_CVT2( AST__TTTOTCG, AST__TCGTOTT ) ||
                     PAIR_CVT2( AST__UTTOUTC, AST__UTCTOUT ) ||
                     PAIR_CVT2( AST__LTTOUTC, AST__UTCTOLT ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ],
                                 cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have two user-supplied arguments
   (test they are swapped). */
               } else if( ( PAIR_CVT2( AST__MJDTOJD, AST__JDTOMJD ) ||
                            PAIR_CVT2( AST__MJDTOMJD, AST__MJDTOMJD ) ||
                            PAIR_CVT2( AST__MJDTOBEP, AST__BEPTOMJD ) ||
                            PAIR_CVT2( AST__MJDTOJEP, AST__JEPTOMJD ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ],
                                 cvtargs[ istep + 1 ][ 1 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ],
                                 cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have three user-supplied arguments. */
               } else if( ( PAIR_CVT2( AST__TDBTOTCB, AST__TCBTOTDB ) ||
                            PAIR_CVT2( AST__GMSTTOLMST, AST__LMSTTOGMST ) ||
                            PAIR_CVT2( AST__LASTTOLMST, AST__LMSTTOLAST ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ],
                                 cvtargs[ istep + 1 ][ 0 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ],
                                 cvtargs[ istep + 1 ][ 1 ] ) &&
                          EQUAL( cvtargs[ istep ][ 2 ],
                                 cvtargs[ istep + 1 ][ 2 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have four user-supplied arguments. */
               } else if( ( PAIR_CVT2( AST__TTTOTDB, AST__TDBTOTT ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ],
                                 cvtargs[ istep + 1 ][ 0 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ],
                                 cvtargs[ istep + 1 ][ 1 ] ) &&
                          EQUAL( cvtargs[ istep ][ 2 ],
                                 cvtargs[ istep + 1 ][ 2 ] ) &&
                          EQUAL( cvtargs[ istep ][ 3 ],
                                 cvtargs[ istep + 1 ][ 3 ] ) ) {
                  istep++;
                  keep = 0;
               }

/* Undefine the local macros. */
#undef PAIR_CVT
#undef PAIR_CVT2
            }

/* If the current transformation (possibly modified above) is being
   kept, then increment the index that identifies its new location in
   the list of transformation steps. */
            if ( keep ) {
               ikeep++;

/* If the new location is different to its current location, copy the
   transformation data into the new location. */
               if ( ikeep != istep ) {
                  cvttype[ ikeep ] = cvttype[ istep ];
                  for ( iarg = 0; iarg < szarg[ istep ]; iarg++ ) {
                     cvtargs[ ikeep ][ iarg ] = cvtargs[ istep ][ iarg ];
                  }
                  szarg[ ikeep ] = szarg[ istep ];
               }
            }
         }

/* Note if no simplification was achieved on this iteration (i.e. the
   number of transformation steps was not reduced). This is the signal
   to quit. */
         done = ( ( ikeep + 1 ) >= nstep );

/* Note how many transformation steps now remain. */
         nstep = ikeep + 1;
      }

/* Determine how many Mappings can be eliminated by condensing all
   those considered above into a single Mapping. */
      if ( astOK ) {
         ngone = imap2 - imap1;

/* Determine if the replacement Mapping can be a UnitMap (a null
   Mapping). This will only be the case if all the transformation
   steps were eliminated above. */
         unit = ( nstep == 0 );

/* Determine if simplification is possible. This will be the case if
   (a) Mappings were eliminated ("ngone" is non-zero), or (b) the
   number of transformation steps was reduced, or (c) the TimeMap(s)
   can be replaced by a UnitMap, or (d) if there was initially only
   one TimeMap present, its invert flag was set (this flag will always
   be cleared in the replacement Mapping). */
         simpler = ngone || ( nstep < nstep0 ) || unit ||
                   ( *invert_list )[ where ];

/* Do nothing more unless simplification is possible. */
         if ( simpler ) {

/* If the replacement Mapping is a UnitMap, then create it. */
            if ( unit ) {
               new = (AstMapping *)
                        astUnitMap( astGetNin( ( *map_list )[ where ] ), "", status );

/* Otherwise, create a replacement TimeMap and add each of the
   remaining transformation steps to it. */
            } else {
               new = (AstMapping *) astTimeMap( 0, "", status );
               for ( istep = 0; istep < nstep; istep++ ) {
                  AddTimeCvt( (AstTimeMap *) new, cvttype[ istep ],
                             cvtargs[ istep ], status );
               }
            }

/* Annul the pointers to the Mappings being eliminated. */
            if ( astOK ) {
               for ( imap = imap1; imap <= imap2; imap++ ) {
                  ( *map_list )[ imap ] = astAnnul( ( *map_list )[ imap ] );
               }

/* Insert the pointer and invert value for the new Mapping. */
               ( *map_list )[ imap1 ] = new;
               ( *invert_list )[ imap1 ] = 0;

/* Move any subsequent Mapping information down to close the gap. */
               for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
                  ( *map_list )[ imap - ngone ] = ( *map_list )[ imap ];
                  ( *invert_list )[ imap - ngone ] = ( *invert_list )[ imap ];
               }

/* Blank out any information remaining at the end of the arrays. */
               for ( imap = ( *nmap - ngone ); imap < *nmap; imap++ ) {
                  ( *map_list )[ imap ] = NULL;
                  ( *invert_list )[ imap ] = 0;
               }

/* Decrement the Mapping count and return the index of the first
   Mapping which was eliminated. */
               ( *nmap ) -= ngone;
               result = imap1;

/* If an error occurred, annul the new Mapping pointer. */
            } else {
               new = astAnnul( new );
            }
         }
      }

/* Free the memory used for the transformation steps. */
      cvttype = astFree( cvttype );
      cvtargs = astFree( cvtargs );
      szarg = astFree( szarg );
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static double Rate( AstMapping *this, double *at, int ax1, int ax2, int *status ){
/*
*  Name:
*     Rate

*  Purpose:
*     Calculate the rate of change of a Mapping output.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     TimeMap member function (overrides the astRate method inherited
*     from the Mapping class ).

*  Description:
*     This function returns the rate of change of a specified output of
*     the supplied Mapping with respect to a specified input, at a
*     specified input position.

*  Parameters:
*     this
*        Pointer to the Mapping to be applied.
*     at
*        The address of an array holding the axis values at the position
*        at which the rate of change is to be evaluated. The number of
*        elements in this array should equal the number of inputs to the
*        Mapping.
*     ax1
*        The index of the Mapping output for which the rate of change is to
*        be found (output numbering starts at 0 for the first output).
*     ax2
*        The index of the Mapping input which is to be varied in order to
*        find the rate of change (input numbering starts at 0 for the first
*        input).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The rate of change of Mapping output "ax1" with respect to input
*     "ax2", evaluated at "at", or AST__BAD if the value cannot be
*     calculated.

*  Implementation Deficiencies:
*     The initial version of this implementation only deals with
*     frequency->wavelength conversions. This is because the slowness of
*     the numerical differentiation implemented by the astRate method in
*     the parent Mapping class is cripples conversion between SpecFluxFrames.
*     Such conversions only rely on rate of change of wavelength with
*     respect to frequency. This implementation should be extended when
*     needed.

*/

/* Local Variables: */
   AstTimeMap *map;
   double result;
   int cvt;
   int i;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the TimeMap structure. */
   map = (AstTimeMap *) this;

/* Initialise the returned value. */
   result = 1.0;

/* Loop round each conversion. */
   for( i = 0; i < map->ncvt; i++ ) {

/* Store the type of the current conversion.*/
      cvt = map->cvttype[ i ];

/* Many of the time conversions are linear. If this is the case, multiply
   the total rate of change by the rate of change for this step. */
      if( cvt == AST__MJDTOBEP ) {
         result *= 1.0/365.242198781;

      } else if( cvt == AST__BEPTOMJD ) {
         result *= 365.242198781;

      } else if( cvt == AST__MJDTOJEP ) {
         result *= 1.0/365.25;

      } else if( cvt == AST__JEPTOMJD ) {
         result *= 365.25;

/* The GMST scales is not linear, so break if we encounter it, and use the
   (numerical) parent astRate method. The other time scale conversions are
   assumed to have a slope of unity. In fact the slope will be ever so
   slightly different to unity. */
      } else if( cvt == AST__UTTOGMST || cvt == AST__GMSTTOUT ) {
         result = AST__BAD;
         break;
      }
   }

/* If this is non-linear TimeMap, use the astRate method inherited from the
   parent Mapping class. */
   if( result == AST__BAD ) result = (*parent_rate)( this, at, ax1, ax2, status );

/* Return the result. */
   return result;
}

static double Rcc( double tdb, double ut1, double wl, double u, double v, int *status ){
/*
*  Name:
*     Rcc

*  Purpose:
*     Find difference between TDB and TT.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     double Rcc( double tdb, double ut1, double wl, double u, double v, int *status )

*  Class Membership:
*     TimeMap member function

*  Description:
*     Relativistic clock correction:  the difference between proper time at
*     a point on the surface of the Earth and coordinate time in the Solar
*     System barycentric space-time frame of reference.
*
*     The proper time is terrestrial time, TT;  the coordinate time is an
*     implementation of barycentric dynamical time, TDB.

*  Parameters:
*     tdb
*        TDB as an MJD.
*     ut1
*        Universal time (only the fraction of the day is relevant)
*     wl
*        Observer longitude (radians west)
*     u
*        Observer distance from Earth spin axis (km)
*     v
*        Observer distance north of Earth equatorial plane (km)
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The clock correction, TDB-TT, in seconds. TDB is coordinate time in the
*     solar system barycentre frame of reference, in units chosen to eliminate
*     the scale difference with respect to terrestrial time. TT is the proper
*     time for clocks at mean sea level on the Earth.

*  Notes:
*     - This function is a translation of the fortran routine SLA_RCC
*     written by by P.T.Wallace.
*
*     - The argument TDB is, strictly, the barycentric coordinate time;
*     however, the terrestrial time TT can in practice be used without
*     any significant loss of accuracy.
*
*     - The result returned by Rcc comprises a main (annual)
*     sinusoidal term of amplitude approximately 0.00166 seconds, plus
*     planetary and lunar terms up to about 20 microseconds, and diurnal
*     terms up to 2 microseconds.  The variation arises from the
*     transverse Doppler effect and the gravitational red-shift as the
*     observer varies in speed and moves through different gravitational
*     potentials.
*
*     - The geocentric model is that of Fairhead & Bretagnon (1990), in
*     its full form.  It was supplied by Fairhead (private
*     communication) as a FORTRAN subroutine.  The original Fairhead
*     routine used explicit formulae, in such large numbers that
*     problems were experienced with certain compilers (Microsoft
*     Fortran on PC aborted with stack overflow, Convex compiled
*     successfully but extremely slowly).  The present implementation is
*     a complete recoding, with the original Fairhead coefficients held
*     in a table.  To optimise arithmetic precision, the terms are
*     accumulated in reverse order, smallest first.  A number of other
*     coding changes were made, in order to match the calling sequence
*     of previous versions of the present routine, and to comply with
*     Starlink programming standards.  The numerical results compared
*     with those from the Fairhead form are essentially unaffected by
*     the changes, the differences being at the 10^-20 sec level.
*
*     - The topocentric part of the model is from Moyer (1981) and
*     Murray (1983).  It is an approximation to the expression
*     ( v / c ) . ( r / c ), where v is the barycentric velocity of
*     the Earth, r is the geocentric position of the observer and
*     c is the speed of light.
*
*     - During the interval 1950-2050, the absolute accuracy of is better
*     than +/- 3 nanoseconds relative to direct numerical integrations
*     using the JPL DE200/LE200 solar system ephemeris.
*
*     - The IAU definition of TDB was that it must differ from TT only by
*     periodic terms.  Though practical, this is an imprecise definition
*     which ignores the existence of very long-period and secular
*     effects in the dynamics of the solar system.  As a consequence,
*     different implementations of TDB will, in general, differ in zero-
*     point and will drift linearly relative to one other.
*
*     - TDB was, in principle, superseded by new coordinate timescales
*     which the IAU introduced in 1991:  geocentric coordinate time,
*     TCG, and barycentric coordinate time, TCB.  However, Rcc
*     can be used to implement the periodic part of TCB-TCG.

*  References:
*     - Fairhead, L., & Bretagnon, P., Astron.Astrophys., 229, 240-247
*     (1990).
*
*     - Moyer, T.D., Cel.Mech., 23, 33 (1981).
*
*     - Murray, C.A., Vectorial Astrometry, Adam Hilger (1983).
*
*     - Seidelmann, P.K. et al, Explanatory Supplement to the
*     Astronomical Almanac, Chapter 2, University Science Books
*     (1992).
*
*     - Simon J.L., Bretagnon P., Chapront J., Chapront-Touze M.,
*     Francou G. & Laskar J., Astron.Astrophys., 282, 663-683 (1994).
*/





/* -----------------------------------------------------------------------
*
*  Fairhead and Bretagnon canonical coefficients
*
*  787 sets of three coefficients.
*
*  Each set is amplitude (microseconds)
*              frequency (radians per Julian millennium since J2000),
*              phase (radians).
*
*  Sets   0-473 are the T**0 terms,
*   "   474-678  "   "  T**1   "
*   "   679-763  "   "  T**2   "
*   "   764-783  "   "  T**3   "
*   "   784-786  "   "  T**4   "  .
*/
   static double fairhd[ 787 ][ 3 ] = {

     { 1656.674564E-6,    6283.075849991, 6.240054195},
     {   22.417471E-6,    5753.384884897, 4.296977442},
     {   13.839792E-6,   12566.151699983, 6.196904410},
     {    4.770086E-6,     529.690965095, 0.444401603},
     {    4.676740E-6,    6069.776754553, 4.021195093},
     {    2.256707E-6,     213.299095438, 5.543113262},
     {    1.694205E-6,      -3.523118349, 5.025132748},
     {    1.554905E-6,   77713.771467920, 5.198467090},
     {    1.276839E-6,    7860.419392439, 5.988822341},
     {    1.193379E-6,    5223.693919802, 3.649823730},
     {    1.115322E-6,    3930.209696220, 1.422745069},
     {    0.794185E-6,   11506.769769794, 2.322313077},
     {    0.447061E-6,      26.298319800, 3.615796498},
     {    0.435206E-6,    -398.149003408, 4.349338347},
     {    0.600309E-6,    1577.343542448, 2.678271909},
     {    0.496817E-6,    6208.294251424, 5.696701824},
     {    0.486306E-6,    5884.926846583, 0.520007179},
     {    0.432392E-6,      74.781598567, 2.435898309},
     {    0.468597E-6,    6244.942814354, 5.866398759},
     {    0.375510E-6,    5507.553238667, 4.103476804},
     {    0.243085E-6,    -775.522611324, 3.651837925},
     {    0.173435E-6,   18849.227549974, 6.153743485},
     {    0.230685E-6,    5856.477659115, 4.773852582},
     {    0.203747E-6,   12036.460734888, 4.333987818},
     {    0.143935E-6,    -796.298006816, 5.957517795},
     {    0.159080E-6,   10977.078804699, 1.890075226},
     {    0.119979E-6,      38.133035638, 4.551585768},
     {    0.118971E-6,    5486.777843175, 1.914547226},
     {    0.116120E-6,    1059.381930189, 0.873504123},
     {    0.137927E-6,   11790.629088659, 1.135934669},
     {    0.098358E-6,    2544.314419883, 0.092793886},
     {    0.101868E-6,   -5573.142801634, 5.984503847},
     {    0.080164E-6,     206.185548437, 2.095377709},
     {    0.079645E-6,    4694.002954708, 2.949233637},
     {    0.062617E-6,      20.775395492, 2.654394814},
     {    0.075019E-6,    2942.463423292, 4.980931759},
     {    0.064397E-6,    5746.271337896, 1.280308748},
     {    0.063814E-6,    5760.498431898, 4.167901731},
     {    0.048042E-6,    2146.165416475, 1.495846011},
     {    0.048373E-6,     155.420399434, 2.251573730},
     {    0.058844E-6,     426.598190876, 4.839650148},
     {    0.046551E-6,      -0.980321068, 0.921573539},
     {    0.054139E-6,   17260.154654690, 3.411091093},
     {    0.042411E-6,    6275.962302991, 2.869567043},
     {    0.040184E-6,      -7.113547001, 3.565975565},
     {    0.036564E-6,    5088.628839767, 3.324679049},
     {    0.040759E-6,   12352.852604545, 3.981496998},
     {    0.036507E-6,     801.820931124, 6.248866009},
     {    0.036955E-6,    3154.687084896, 5.071801441},
     {    0.042732E-6,     632.783739313, 5.720622217},
     {    0.042560E-6,  161000.685737473, 1.270837679},
     {    0.040480E-6,   15720.838784878, 2.546610123},
     {    0.028244E-6,   -6286.598968340, 5.069663519},
     {    0.033477E-6,    6062.663207553, 4.144987272},
     {    0.034867E-6,     522.577418094, 5.210064075},
     {    0.032438E-6,    6076.890301554, 0.749317412},
     {    0.030215E-6,    7084.896781115, 3.389610345},
     {    0.029247E-6,  -71430.695617928, 4.183178762},
     {    0.033529E-6,    9437.762934887, 2.404714239},
     {    0.032423E-6,    8827.390269875, 5.541473556},
     {    0.027567E-6,    6279.552731642, 5.040846034},
     {    0.029862E-6,   12139.553509107, 1.770181024},
     {    0.022509E-6,   10447.387839604, 1.460726241},
     {    0.020937E-6,    8429.241266467, 0.652303414},
     {    0.020322E-6,     419.484643875, 3.735430632},
     {    0.024816E-6,   -1194.447010225, 1.087136918},
     {    0.025196E-6,    1748.016413067, 2.901883301},
     {    0.021691E-6,   14143.495242431, 5.952658009},
     {    0.017673E-6,    6812.766815086, 3.186129845},
     {    0.022567E-6,    6133.512652857, 3.307984806},
     {    0.016155E-6,   10213.285546211, 1.331103168},
     {    0.014751E-6,    1349.867409659, 4.308933301},
     {    0.015949E-6,    -220.412642439, 4.005298270},
     {    0.015974E-6,   -2352.866153772, 6.145309371},
     {    0.014223E-6,   17789.845619785, 2.104551349},
     {    0.017806E-6,      73.297125859, 3.475975097},
     {    0.013671E-6,    -536.804512095, 5.971672571},
     {    0.011942E-6,    8031.092263058, 2.053414715},
     {    0.014318E-6,   16730.463689596, 3.016058075},
     {    0.012462E-6,     103.092774219, 1.737438797},
     {    0.010962E-6,       3.590428652, 2.196567739},
     {    0.015078E-6,   19651.048481098, 3.969480770},
     {    0.010396E-6,     951.718406251, 5.717799605},
     {    0.011707E-6,   -4705.732307544, 2.654125618},
     {    0.010453E-6,    5863.591206116, 1.913704550},
     {    0.012420E-6,    4690.479836359, 4.734090399},
     {    0.011847E-6,    5643.178563677, 5.489005403},
     {    0.008610E-6,    3340.612426700, 3.661698944},
     {    0.011622E-6,    5120.601145584, 4.863931876},
     {    0.010825E-6,     553.569402842, 0.842715011},
     {    0.008666E-6,    -135.065080035, 3.293406547},
     {    0.009963E-6,     149.563197135, 4.870690598},
     {    0.009858E-6,    6309.374169791, 1.061816410},
     {    0.007959E-6,     316.391869657, 2.465042647},
     {    0.010099E-6,     283.859318865, 1.942176992},
     {    0.007147E-6,    -242.728603974, 3.661486981},
     {    0.007505E-6,    5230.807466803, 4.920937029},
     {    0.008323E-6,   11769.853693166, 1.229392026},
     {    0.007490E-6,   -6256.777530192, 3.658444681},
     {    0.009370E-6,  149854.400134205, 0.673880395},
     {    0.007117E-6,      38.027672636, 5.294249518},
     {    0.007857E-6,   12168.002696575, 0.525733528},
     {    0.007019E-6,    6206.809778716, 0.837688810},
     {    0.006056E-6,     955.599741609, 4.194535082},
     {    0.008107E-6,   13367.972631107, 3.793235253},
     {    0.006731E-6,    5650.292110678, 5.639906583},
     {    0.007332E-6,      36.648562930, 0.114858677},
     {    0.006366E-6,    4164.311989613, 2.262081818},
     {    0.006858E-6,    5216.580372801, 0.642063318},
     {    0.006919E-6,    6681.224853400, 6.018501522},
     {    0.006826E-6,    7632.943259650, 3.458654112},
     {    0.005308E-6,   -1592.596013633, 2.500382359},
     {    0.005096E-6,   11371.704689758, 2.547107806},
     {    0.004841E-6,    5333.900241022, 0.437078094},
     {    0.005582E-6,    5966.683980335, 2.246174308},
     {    0.006304E-6,   11926.254413669, 2.512929171},
     {    0.006603E-6,   23581.258177318, 5.393136889},
     {    0.005123E-6,      -1.484472708, 2.999641028},
     {    0.004648E-6,    1589.072895284, 1.275847090},
     {    0.005119E-6,    6438.496249426, 1.486539246},
     {    0.004521E-6,    4292.330832950, 6.140635794},
     {    0.005680E-6,   23013.539539587, 4.557814849},
     {    0.005488E-6,      -3.455808046, 0.090675389},
     {    0.004193E-6,    7234.794256242, 4.869091389},
     {    0.003742E-6,    7238.675591600, 4.691976180},
     {    0.004148E-6,    -110.206321219, 3.016173439},
     {    0.004553E-6,   11499.656222793, 5.554998314},
     {    0.004892E-6,    5436.993015240, 1.475415597},
     {    0.004044E-6,    4732.030627343, 1.398784824},
     {    0.004164E-6,   12491.370101415, 5.650931916},
     {    0.004349E-6,   11513.883316794, 2.181745369},
     {    0.003919E-6,   12528.018664345, 5.823319737},
     {    0.003129E-6,    6836.645252834, 0.003844094},
     {    0.004080E-6,   -7058.598461315, 3.690360123},
     {    0.003270E-6,      76.266071276, 1.517189902},
     {    0.002954E-6,    6283.143160294, 4.447203799},
     {    0.002872E-6,      28.449187468, 1.158692983},
     {    0.002881E-6,     735.876513532, 0.349250250},
     {    0.003279E-6,    5849.364112115, 4.893384368},
     {    0.003625E-6,    6209.778724132, 1.473760578},
     {    0.003074E-6,     949.175608970, 5.185878737},
     {    0.002775E-6,    9917.696874510, 1.030026325},
     {    0.002646E-6,   10973.555686350, 3.918259169},
     {    0.002575E-6,   25132.303399966, 6.109659023},
     {    0.003500E-6,     263.083923373, 1.892100742},
     {    0.002740E-6,   18319.536584880, 4.320519510},
     {    0.002464E-6,     202.253395174, 4.698203059},
     {    0.002409E-6,       2.542797281, 5.325009315},
     {    0.003354E-6,  -90955.551694697, 1.942656623},
     {    0.002296E-6,    6496.374945429, 5.061810696},
     {    0.003002E-6,    6172.869528772, 2.797822767},
     {    0.003202E-6,   27511.467873537, 0.531673101},
     {    0.002954E-6,   -6283.008539689, 4.533471191},
     {    0.002353E-6,     639.897286314, 3.734548088},
     {    0.002401E-6,   16200.772724501, 2.605547070},
     {    0.003053E-6,  233141.314403759, 3.029030662},
     {    0.003024E-6,   83286.914269554, 2.355556099},
     {    0.002863E-6,   17298.182327326, 5.240963796},
     {    0.002103E-6,   -7079.373856808, 5.756641637},
     {    0.002303E-6,   83996.847317911, 2.013686814},
     {    0.002303E-6,   18073.704938650, 1.089100410},
     {    0.002381E-6,      63.735898303, 0.759188178},
     {    0.002493E-6,    6386.168624210, 0.645026535},
     {    0.002366E-6,       3.932153263, 6.215885448},
     {    0.002169E-6,   11015.106477335, 4.845297676},
     {    0.002397E-6,    6243.458341645, 3.809290043},
     {    0.002183E-6,    1162.474704408, 6.179611691},
     {    0.002353E-6,    6246.427287062, 4.781719760},
     {    0.002199E-6,    -245.831646229, 5.956152284},
     {    0.001729E-6,    3894.181829542, 1.264976635},
     {    0.001896E-6,   -3128.388765096, 4.914231596},
     {    0.002085E-6,      35.164090221, 1.405158503},
     {    0.002024E-6,   14712.317116458, 2.752035928},
     {    0.001737E-6,    6290.189396992, 5.280820144},
     {    0.002229E-6,     491.557929457, 1.571007057},
     {    0.001602E-6,   14314.168113050, 4.203664806},
     {    0.002186E-6,     454.909366527, 1.402101526},
     {    0.001897E-6,   22483.848574493, 4.167932508},
     {    0.001825E-6,   -3738.761430108, 0.545828785},
     {    0.001894E-6,    1052.268383188, 5.817167450},
     {    0.001421E-6,      20.355319399, 2.419886601},
     {    0.001408E-6,   10984.192351700, 2.732084787},
     {    0.001847E-6,   10873.986030480, 2.903477885},
     {    0.001391E-6,   -8635.942003763, 0.593891500},
     {    0.001388E-6,      -7.046236698, 1.166145902},
     {    0.001810E-6,  -88860.057071188, 0.487355242},
     {    0.001288E-6,   -1990.745017041, 3.913022880},
     {    0.001297E-6,   23543.230504682, 3.063805171},
     {    0.001335E-6,    -266.607041722, 3.995764039},
     {    0.001376E-6,   10969.965257698, 5.152914309},
     {    0.001745E-6,  244287.600007027, 3.626395673},
     {    0.001649E-6,   31441.677569757, 1.952049260},
     {    0.001416E-6,    9225.539273283, 4.996408389},
     {    0.001238E-6,    4804.209275927, 5.503379738},
     {    0.001472E-6,    4590.910180489, 4.164913291},
     {    0.001169E-6,    6040.347246017, 5.841719038},
     {    0.001039E-6,    5540.085789459, 2.769753519},
     {    0.001004E-6,    -170.672870619, 0.755008103},
     {    0.001284E-6,   10575.406682942, 5.306538209},
     {    0.001278E-6,      71.812653151, 4.713486491},
     {    0.001321E-6,   18209.330263660, 2.624866359},
     {    0.001297E-6,   21228.392023546, 0.382603541},
     {    0.000954E-6,    6282.095528923, 0.882213514},
     {    0.001145E-6,    6058.731054289, 1.169483931},
     {    0.000979E-6,    5547.199336460, 5.448375984},
     {    0.000987E-6,   -6262.300454499, 2.656486959},
     {    0.001070E-6, -154717.609887482, 1.827624012},
     {    0.000991E-6,    4701.116501708, 4.387001801},
     {    0.001155E-6,     -14.227094002, 3.042700750},
     {    0.001176E-6,     277.034993741, 3.335519004},
     {    0.000890E-6,   13916.019109642, 5.601498297},
     {    0.000884E-6,   -1551.045222648, 1.088831705},
     {    0.000876E-6,    5017.508371365, 3.969902609},
     {    0.000806E-6,   15110.466119866, 5.142876744},
     {    0.000773E-6,   -4136.910433516, 0.022067765},
     {    0.001077E-6,     175.166059800, 1.844913056},
     {    0.000954E-6,   -6284.056171060, 0.968480906},
     {    0.000737E-6,    5326.786694021, 4.923831588},
     {    0.000845E-6,    -433.711737877, 4.749245231},
     {    0.000819E-6,    8662.240323563, 5.991247817},
     {    0.000852E-6,     199.072001436, 2.189604979},
     {    0.000723E-6,   17256.631536341, 6.068719637},
     {    0.000940E-6,    6037.244203762, 6.197428148},
     {    0.000885E-6,   11712.955318231, 3.280414875},
     {    0.000706E-6,   12559.038152982, 2.824848947},
     {    0.000732E-6,    2379.164473572, 2.501813417},
     {    0.000764E-6,   -6127.655450557, 2.236346329},
     {    0.000908E-6,     131.541961686, 2.521257490},
     {    0.000907E-6,   35371.887265976, 3.370195967},
     {    0.000673E-6,    1066.495477190, 3.876512374},
     {    0.000814E-6,   17654.780539750, 4.627122566},
     {    0.000630E-6,      36.027866677, 0.156368499},
     {    0.000798E-6,     515.463871093, 5.151962502},
     {    0.000798E-6,     148.078724426, 5.909225055},
     {    0.000806E-6,     309.278322656, 6.054064447},
     {    0.000607E-6,     -39.617508346, 2.839021623},
     {    0.000601E-6,     412.371096874, 3.984225404},
     {    0.000646E-6,   11403.676995575, 3.852959484},
     {    0.000704E-6,   13521.751441591, 2.300991267},
     {    0.000603E-6,  -65147.619767937, 4.140083146},
     {    0.000609E-6,   10177.257679534, 0.437122327},
     {    0.000631E-6,    5767.611978898, 4.026532329},
     {    0.000576E-6,   11087.285125918, 4.760293101},
     {    0.000674E-6,   14945.316173554, 6.270510511},
     {    0.000726E-6,    5429.879468239, 6.039606892},
     {    0.000710E-6,   28766.924424484, 5.672617711},
     {    0.000647E-6,   11856.218651625, 3.397132627},
     {    0.000678E-6,   -5481.254918868, 6.249666675},
     {    0.000618E-6,   22003.914634870, 2.466427018},
     {    0.000738E-6,    6134.997125565, 2.242668890},
     {    0.000660E-6,     625.670192312, 5.864091907},
     {    0.000694E-6,    3496.032826134, 2.668309141},
     {    0.000531E-6,    6489.261398429, 1.681888780},
     {    0.000611E-6, -143571.324284214, 2.424978312},
     {    0.000575E-6,   12043.574281889, 4.216492400},
     {    0.000553E-6,   12416.588502848, 4.772158039},
     {    0.000689E-6,    4686.889407707, 6.224271088},
     {    0.000495E-6,    7342.457780181, 3.817285811},
     {    0.000567E-6,    3634.621024518, 1.649264690},
     {    0.000515E-6,   18635.928454536, 3.945345892},
     {    0.000486E-6,    -323.505416657, 4.061673868},
     {    0.000662E-6,   25158.601719765, 1.794058369},
     {    0.000509E-6,     846.082834751, 3.053874588},
     {    0.000472E-6,  -12569.674818332, 5.112133338},
     {    0.000461E-6,    6179.983075773, 0.513669325},
     {    0.000641E-6,   83467.156352816, 3.210727723},
     {    0.000520E-6,   10344.295065386, 2.445597761},
     {    0.000493E-6,   18422.629359098, 1.676939306},
     {    0.000478E-6,    1265.567478626, 5.487314569},
     {    0.000472E-6,     -18.159247265, 1.999707589},
     {    0.000559E-6,   11190.377900137, 5.783236356},
     {    0.000494E-6,    9623.688276691, 3.022645053},
     {    0.000463E-6,    5739.157790895, 1.411223013},
     {    0.000432E-6,   16858.482532933, 1.179256434},
     {    0.000574E-6,   72140.628666286, 1.758191830},
     {    0.000484E-6,   17267.268201691, 3.290589143},
     {    0.000550E-6,    4907.302050146, 0.864024298},
     {    0.000399E-6,      14.977853527, 2.094441910},
     {    0.000491E-6,     224.344795702, 0.878372791},
     {    0.000432E-6,   20426.571092422, 6.003829241},
     {    0.000481E-6,    5749.452731634, 4.309591964},
     {    0.000480E-6,    5757.317038160, 1.142348571},
     {    0.000485E-6,    6702.560493867, 0.210580917},
     {    0.000426E-6,    6055.549660552, 4.274476529},
     {    0.000480E-6,    5959.570433334, 5.031351030},
     {    0.000466E-6,   12562.628581634, 4.959581597},
     {    0.000520E-6,   39302.096962196, 4.788002889},
     {    0.000458E-6,   12132.439962106, 1.880103788},
     {    0.000470E-6,   12029.347187887, 1.405611197},
     {    0.000416E-6,   -7477.522860216, 1.082356330},
     {    0.000449E-6,   11609.862544012, 4.179989585},
     {    0.000465E-6,   17253.041107690, 0.353496295},
     {    0.000362E-6,   -4535.059436924, 1.583849576},
     {    0.000383E-6,   21954.157609398, 3.747376371},
     {    0.000389E-6,      17.252277143, 1.395753179},
     {    0.000331E-6,   18052.929543158, 0.566790582},
     {    0.000430E-6,   13517.870106233, 0.685827538},
     {    0.000368E-6,   -5756.908003246, 0.731374317},
     {    0.000330E-6,   10557.594160824, 3.710043680},
     {    0.000332E-6,   20199.094959633, 1.652901407},
     {    0.000384E-6,   11933.367960670, 5.827781531},
     {    0.000387E-6,   10454.501386605, 2.541182564},
     {    0.000325E-6,   15671.081759407, 2.178850542},
     {    0.000318E-6,     138.517496871, 2.253253037},
     {    0.000305E-6,    9388.005909415, 0.578340206},
     {    0.000352E-6,    5749.861766548, 3.000297967},
     {    0.000311E-6,    6915.859589305, 1.693574249},
     {    0.000297E-6,   24072.921469776, 1.997249392},
     {    0.000363E-6,    -640.877607382, 5.071820966},
     {    0.000323E-6,   12592.450019783, 1.072262823},
     {    0.000341E-6,   12146.667056108, 4.700657997},
     {    0.000290E-6,    9779.108676125, 1.812320441},
     {    0.000342E-6,    6132.028180148, 4.322238614},
     {    0.000329E-6,    6268.848755990, 3.033827743},
     {    0.000374E-6,   17996.031168222, 3.388716544},
     {    0.000285E-6,    -533.214083444, 4.687313233},
     {    0.000338E-6,    6065.844601290, 0.877776108},
     {    0.000276E-6,      24.298513841, 0.770299429},
     {    0.000336E-6,   -2388.894020449, 5.353796034},
     {    0.000290E-6,    3097.883822726, 4.075291557},
     {    0.000318E-6,     709.933048357, 5.941207518},
     {    0.000271E-6,   13095.842665077, 3.208912203},
     {    0.000331E-6,    6073.708907816, 4.007881169},
     {    0.000292E-6,     742.990060533, 2.714333592},
     {    0.000362E-6,   29088.811415985, 3.215977013},
     {    0.000280E-6,   12359.966151546, 0.710872502},
     {    0.000267E-6,   10440.274292604, 4.730108488},
     {    0.000262E-6,     838.969287750, 1.327720272},
     {    0.000250E-6,   16496.361396202, 0.898769761},
     {    0.000325E-6,   20597.243963041, 0.180044365},
     {    0.000268E-6,    6148.010769956, 5.152666276},
     {    0.000284E-6,    5636.065016677, 5.655385808},
     {    0.000301E-6,    6080.822454817, 2.135396205},
     {    0.000294E-6,    -377.373607916, 3.708784168},
     {    0.000236E-6,    2118.763860378, 1.733578756},
     {    0.000234E-6,    5867.523359379, 5.575209112},
     {    0.000268E-6, -226858.238553767, 0.069432392},
     {    0.000265E-6,  167283.761587465, 4.369302826},
     {    0.000280E-6,   28237.233459389, 5.304829118},
     {    0.000292E-6,   12345.739057544, 4.096094132},
     {    0.000223E-6,   19800.945956225, 3.069327406},
     {    0.000301E-6,   43232.306658416, 6.205311188},
     {    0.000264E-6,   18875.525869774, 1.417263408},
     {    0.000304E-6,   -1823.175188677, 3.409035232},
     {    0.000301E-6,     109.945688789, 0.510922054},
     {    0.000260E-6,     813.550283960, 2.389438934},
     {    0.000299E-6,  316428.228673312, 5.384595078},
     {    0.000211E-6,    5756.566278634, 3.789392838},
     {    0.000209E-6,    5750.203491159, 1.661943545},
     {    0.000240E-6,   12489.885628707, 5.684549045},
     {    0.000216E-6,    6303.851245484, 3.862942261},
     {    0.000203E-6,    1581.959348283, 5.549853589},
     {    0.000200E-6,    5642.198242609, 1.016115785},
     {    0.000197E-6,     -70.849445304, 4.690702525},
     {    0.000227E-6,    6287.008003254, 2.911891613},
     {    0.000197E-6,     533.623118358, 1.048982898},
     {    0.000205E-6,   -6279.485421340, 1.829362730},
     {    0.000209E-6,  -10988.808157535, 2.636140084},
     {    0.000208E-6,    -227.526189440, 4.127883842},
     {    0.000191E-6,     415.552490612, 4.401165650},
     {    0.000190E-6,   29296.615389579, 4.175658539},
     {    0.000264E-6,   66567.485864652, 4.601102551},
     {    0.000256E-6,   -3646.350377354, 0.506364778},
     {    0.000188E-6,   13119.721102825, 2.032195842},
     {    0.000185E-6,    -209.366942175, 4.694756586},
     {    0.000198E-6,   25934.124331089, 3.832703118},
     {    0.000195E-6,    4061.219215394, 3.308463427},
     {    0.000234E-6,    5113.487598583, 1.716090661},
     {    0.000188E-6,    1478.866574064, 5.686865780},
     {    0.000222E-6,   11823.161639450, 1.942386641},
     {    0.000181E-6,   10770.893256262, 1.999482059},
     {    0.000171E-6,    6546.159773364, 1.182807992},
     {    0.000206E-6,      70.328180442, 5.934076062},
     {    0.000169E-6,   20995.392966449, 2.169080622},
     {    0.000191E-6,   10660.686935042, 5.405515999},
     {    0.000228E-6,   33019.021112205, 4.656985514},
     {    0.000184E-6,   -4933.208440333, 3.327476868},
     {    0.000220E-6,    -135.625325010, 1.765430262},
     {    0.000166E-6,   23141.558382925, 3.454132746},
     {    0.000191E-6,    6144.558353121, 5.020393445},
     {    0.000180E-6,    6084.003848555, 0.602182191},
     {    0.000163E-6,   17782.732072784, 4.960593133},
     {    0.000225E-6,   16460.333529525, 2.596451817},
     {    0.000222E-6,    5905.702242076, 3.731990323},
     {    0.000204E-6,     227.476132789, 5.636192701},
     {    0.000159E-6,   16737.577236597, 3.600691544},
     {    0.000200E-6,    6805.653268085, 0.868220961},
     {    0.000187E-6,   11919.140866668, 2.629456641},
     {    0.000161E-6,     127.471796607, 2.862574720},
     {    0.000205E-6,    6286.666278643, 1.742882331},
     {    0.000189E-6,     153.778810485, 4.812372643},
     {    0.000168E-6,   16723.350142595, 0.027860588},
     {    0.000149E-6,   11720.068865232, 0.659721876},
     {    0.000189E-6,    5237.921013804, 5.245313000},
     {    0.000143E-6,    6709.674040867, 4.317625647},
     {    0.000146E-6,    4487.817406270, 4.815297007},
     {    0.000144E-6,    -664.756045130, 5.381366880},
     {    0.000175E-6,    5127.714692584, 4.728443327},
     {    0.000162E-6,    6254.626662524, 1.435132069},
     {    0.000187E-6,   47162.516354635, 1.354371923},
     {    0.000146E-6,   11080.171578918, 3.369695406},
     {    0.000180E-6,    -348.924420448, 2.490902145},
     {    0.000148E-6,     151.047669843, 3.799109588},
     {    0.000157E-6,    6197.248551160, 1.284375887},
     {    0.000167E-6,     146.594251718, 0.759969109},
     {    0.000133E-6,   -5331.357443741, 5.409701889},
     {    0.000154E-6,      95.979227218, 3.366890614},
     {    0.000148E-6,   -6418.140930027, 3.384104996},
     {    0.000128E-6,   -6525.804453965, 3.803419985},
     {    0.000130E-6,   11293.470674356, 0.939039445},
     {    0.000152E-6,   -5729.506447149, 0.734117523},
     {    0.000138E-6,     210.117701700, 2.564216078},
     {    0.000123E-6,    6066.595360816, 4.517099537},
     {    0.000140E-6,   18451.078546566, 0.642049130},
     {    0.000126E-6,   11300.584221356, 3.485280663},
     {    0.000119E-6,   10027.903195729, 3.217431161},
     {    0.000151E-6,    4274.518310832, 4.404359108},
     {    0.000117E-6,    6072.958148291, 0.366324650},
     {    0.000165E-6,   -7668.637425143, 4.298212528},
     {    0.000117E-6,   -6245.048177356, 5.379518958},
     {    0.000130E-6,   -5888.449964932, 4.527681115},
     {    0.000121E-6,    -543.918059096, 6.109429504},
     {    0.000162E-6,    9683.594581116, 5.720092446},
     {    0.000141E-6,    6219.339951688, 0.679068671},
     {    0.000118E-6,   22743.409379516, 4.881123092},
     {    0.000129E-6,    1692.165669502, 0.351407289},
     {    0.000126E-6,    5657.405657679, 5.146592349},
     {    0.000114E-6,     728.762966531, 0.520791814},
     {    0.000120E-6,      52.596639600, 0.948516300},
     {    0.000115E-6,      65.220371012, 3.504914846},
     {    0.000126E-6,    5881.403728234, 5.577502482},
     {    0.000158E-6,  163096.180360983, 2.957128968},
     {    0.000134E-6,   12341.806904281, 2.598576764},
     {    0.000151E-6,   16627.370915377, 3.985702050},
     {    0.000109E-6,    1368.660252845, 0.014730471},
     {    0.000131E-6,    6211.263196841, 0.085077024},
     {    0.000146E-6,    5792.741760812, 0.708426604},
     {    0.000146E-6,     -77.750543984, 3.121576600},
     {    0.000107E-6,    5341.013788022, 0.288231904},
     {    0.000138E-6,    6281.591377283, 2.797450317},
     {    0.000113E-6,   -6277.552925684, 2.788904128},
     {    0.000115E-6,    -525.758811831, 5.895222200},
     {    0.000138E-6,    6016.468808270, 6.096188999},
     {    0.000139E-6,   23539.707386333, 2.028195445},
     {    0.000146E-6,   -4176.041342449, 4.660008502},
     {    0.000107E-6,   16062.184526117, 4.066520001},
     {    0.000142E-6,   83783.548222473, 2.936315115},
     {    0.000128E-6,    9380.959672717, 3.223844306},
     {    0.000135E-6,    6205.325306007, 1.638054048},
     {    0.000101E-6,    2699.734819318, 5.481603249},
     {    0.000104E-6,    -568.821874027, 2.205734493},
     {    0.000103E-6,    6321.103522627, 2.440421099},
     {    0.000119E-6,    6321.208885629, 2.547496264},
     {    0.000138E-6,    1975.492545856, 2.314608466},
     {    0.000121E-6,     137.033024162, 4.539108237},
     {    0.000123E-6,   19402.796952817, 4.538074405},
     {    0.000119E-6,   22805.735565994, 2.869040566},
     {    0.000133E-6,   64471.991241142, 6.056405489},
     {    0.000129E-6,     -85.827298831, 2.540635083},
     {    0.000131E-6,   13613.804277336, 4.005732868},
     {    0.000104E-6,    9814.604100291, 1.959967212},
     {    0.000112E-6,   16097.679950283, 3.589026260},
     {    0.000123E-6,    2107.034507542, 1.728627253},
     {    0.000121E-6,   36949.230808424, 6.072332087},
     {    0.000108E-6,  -12539.853380183, 3.716133846},
     {    0.000113E-6,   -7875.671863624, 2.725771122},
     {    0.000109E-6,    4171.425536614, 4.033338079},
     {    0.000101E-6,    6247.911759770, 3.441347021},
     {    0.000113E-6,    7330.728427345, 0.656372122},
     {    0.000113E-6,   51092.726050855, 2.791483066},
     {    0.000106E-6,    5621.842923210, 1.815323326},
     {    0.000101E-6,     111.430161497, 5.711033677},
     {    0.000103E-6,     909.818733055, 2.812745443},
     {    0.000101E-6,    1790.642637886, 1.965746028},
     {  102.156724E-6,    6283.075849991, 4.249032005},
     {    1.706807E-6,   12566.151699983, 4.205904248},
     {    0.269668E-6,     213.299095438, 3.400290479},
     {    0.265919E-6,     529.690965095, 5.836047367},
     {    0.210568E-6,      -3.523118349, 6.262738348},
     {    0.077996E-6,    5223.693919802, 4.670344204},
     {    0.054764E-6,    1577.343542448, 4.534800170},
     {    0.059146E-6,      26.298319800, 1.083044735},
     {    0.034420E-6,    -398.149003408, 5.980077351},
     {    0.032088E-6,   18849.227549974, 4.162913471},
     {    0.033595E-6,    5507.553238667, 5.980162321},
     {    0.029198E-6,    5856.477659115, 0.623811863},
     {    0.027764E-6,     155.420399434, 3.745318113},
     {    0.025190E-6,    5746.271337896, 2.980330535},
     {    0.022997E-6,    -796.298006816, 1.174411803},
     {    0.024976E-6,    5760.498431898, 2.467913690},
     {    0.021774E-6,     206.185548437, 3.854787540},
     {    0.017925E-6,    -775.522611324, 1.092065955},
     {    0.013794E-6,     426.598190876, 2.699831988},
     {    0.013276E-6,    6062.663207553, 5.845801920},
     {    0.011774E-6,   12036.460734888, 2.292832062},
     {    0.012869E-6,    6076.890301554, 5.333425680},
     {    0.012152E-6,    1059.381930189, 6.222874454},
     {    0.011081E-6,      -7.113547001, 5.154724984},
     {    0.010143E-6,    4694.002954708, 4.044013795},
     {    0.009357E-6,    5486.777843175, 3.416081409},
     {    0.010084E-6,     522.577418094, 0.749320262},
     {    0.008587E-6,   10977.078804699, 2.777152598},
     {    0.008628E-6,    6275.962302991, 4.562060226},
     {    0.008158E-6,    -220.412642439, 5.806891533},
     {    0.007746E-6,    2544.314419883, 1.603197066},
     {    0.007670E-6,    2146.165416475, 3.000200440},
     {    0.007098E-6,      74.781598567, 0.443725817},
     {    0.006180E-6,    -536.804512095, 1.302642751},
     {    0.005818E-6,    5088.628839767, 4.827723531},
     {    0.004945E-6,   -6286.598968340, 0.268305170},
     {    0.004774E-6,    1349.867409659, 5.808636673},
     {    0.004687E-6,    -242.728603974, 5.154890570},
     {    0.006089E-6,    1748.016413067, 4.403765209},
     {    0.005975E-6,   -1194.447010225, 2.583472591},
     {    0.004229E-6,     951.718406251, 0.931172179},
     {    0.005264E-6,     553.569402842, 2.336107252},
     {    0.003049E-6,    5643.178563677, 1.362634430},
     {    0.002974E-6,    6812.766815086, 1.583012668},
     {    0.003403E-6,   -2352.866153772, 2.552189886},
     {    0.003030E-6,     419.484643875, 5.286473844},
     {    0.003210E-6,      -7.046236698, 1.863796539},
     {    0.003058E-6,    9437.762934887, 4.226420633},
     {    0.002589E-6,   12352.852604545, 1.991935820},
     {    0.002927E-6,    5216.580372801, 2.319951253},
     {    0.002425E-6,    5230.807466803, 3.084752833},
     {    0.002656E-6,    3154.687084896, 2.487447866},
     {    0.002445E-6,   10447.387839604, 2.347139160},
     {    0.002990E-6,    4690.479836359, 6.235872050},
     {    0.002890E-6,    5863.591206116, 0.095197563},
     {    0.002498E-6,    6438.496249426, 2.994779800},
     {    0.001889E-6,    8031.092263058, 3.569003717},
     {    0.002567E-6,     801.820931124, 3.425611498},
     {    0.001803E-6,  -71430.695617928, 2.192295512},
     {    0.001782E-6,       3.932153263, 5.180433689},
     {    0.001694E-6,   -4705.732307544, 4.641779174},
     {    0.001704E-6,   -1592.596013633, 3.997097652},
     {    0.001735E-6,    5849.364112115, 0.417558428},
     {    0.001643E-6,    8429.241266467, 2.180619584},
     {    0.001680E-6,      38.133035638, 4.164529426},
     {    0.002045E-6,    7084.896781115, 0.526323854},
     {    0.001458E-6,    4292.330832950, 1.356098141},
     {    0.001437E-6,      20.355319399, 3.895439360},
     {    0.001738E-6,    6279.552731642, 0.087484036},
     {    0.001367E-6,   14143.495242431, 3.987576591},
     {    0.001344E-6,    7234.794256242, 0.090454338},
     {    0.001438E-6,   11499.656222793, 0.974387904},
     {    0.001257E-6,    6836.645252834, 1.509069366},
     {    0.001358E-6,   11513.883316794, 0.495572260},
     {    0.001628E-6,    7632.943259650, 4.968445721},
     {    0.001169E-6,     103.092774219, 2.838496795},
     {    0.001162E-6,    4164.311989613, 3.408387778},
     {    0.001092E-6,    6069.776754553, 3.617942651},
     {    0.001008E-6,   17789.845619785, 0.286350174},
     {    0.001008E-6,     639.897286314, 1.610762073},
     {    0.000918E-6,   10213.285546211, 5.532798067},
     {    0.001011E-6,   -6256.777530192, 0.661826484},
     {    0.000753E-6,   16730.463689596, 3.905030235},
     {    0.000737E-6,   11926.254413669, 4.641956361},
     {    0.000694E-6,    3340.612426700, 2.111120332},
     {    0.000701E-6,    3894.181829542, 2.760823491},
     {    0.000689E-6,    -135.065080035, 4.768800780},
     {    0.000700E-6,   13367.972631107, 5.760439898},
     {    0.000664E-6,    6040.347246017, 1.051215840},
     {    0.000654E-6,    5650.292110678, 4.911332503},
     {    0.000788E-6,    6681.224853400, 4.699648011},
     {    0.000628E-6,    5333.900241022, 5.024608847},
     {    0.000755E-6,    -110.206321219, 4.370971253},
     {    0.000628E-6,    6290.189396992, 3.660478857},
     {    0.000635E-6,   25132.303399966, 4.121051532},
     {    0.000534E-6,    5966.683980335, 1.173284524},
     {    0.000543E-6,    -433.711737877, 0.345585464},
     {    0.000517E-6,   -1990.745017041, 5.414571768},
     {    0.000504E-6,    5767.611978898, 2.328281115},
     {    0.000485E-6,    5753.384884897, 1.685874771},
     {    0.000463E-6,    7860.419392439, 5.297703006},
     {    0.000604E-6,     515.463871093, 0.591998446},
     {    0.000443E-6,   12168.002696575, 4.830881244},
     {    0.000570E-6,     199.072001436, 3.899190272},
     {    0.000465E-6,   10969.965257698, 0.476681802},
     {    0.000424E-6,   -7079.373856808, 1.112242763},
     {    0.000427E-6,     735.876513532, 1.994214480},
     {    0.000478E-6,   -6127.655450557, 3.778025483},
     {    0.000414E-6,   10973.555686350, 5.441088327},
     {    0.000512E-6,    1589.072895284, 0.107123853},
     {    0.000378E-6,   10984.192351700, 0.915087231},
     {    0.000402E-6,   11371.704689758, 4.107281715},
     {    0.000453E-6,    9917.696874510, 1.917490952},
     {    0.000395E-6,     149.563197135, 2.763124165},
     {    0.000371E-6,    5739.157790895, 3.112111866},
     {    0.000350E-6,   11790.629088659, 0.440639857},
     {    0.000356E-6,    6133.512652857, 5.444568842},
     {    0.000344E-6,     412.371096874, 5.676832684},
     {    0.000383E-6,     955.599741609, 5.559734846},
     {    0.000333E-6,    6496.374945429, 0.261537984},
     {    0.000340E-6,    6055.549660552, 5.975534987},
     {    0.000334E-6,    1066.495477190, 2.335063907},
     {    0.000399E-6,   11506.769769794, 5.321230910},
     {    0.000314E-6,   18319.536584880, 2.313312404},
     {    0.000424E-6,    1052.268383188, 1.211961766},
     {    0.000307E-6,      63.735898303, 3.169551388},
     {    0.000329E-6,      29.821438149, 6.106912080},
     {    0.000357E-6,    6309.374169791, 4.223760346},
     {    0.000312E-6,   -3738.761430108, 2.180556645},
     {    0.000301E-6,     309.278322656, 1.499984572},
     {    0.000268E-6,   12043.574281889, 2.447520648},
     {    0.000257E-6,   12491.370101415, 3.662331761},
     {    0.000290E-6,     625.670192312, 1.272834584},
     {    0.000256E-6,    5429.879468239, 1.913426912},
     {    0.000339E-6,    3496.032826134, 4.165930011},
     {    0.000283E-6,    3930.209696220, 4.325565754},
     {    0.000241E-6,   12528.018664345, 3.832324536},
     {    0.000304E-6,    4686.889407707, 1.612348468},
     {    0.000259E-6,   16200.772724501, 3.470173146},
     {    0.000238E-6,   12139.553509107, 1.147977842},
     {    0.000236E-6,    6172.869528772, 3.776271728},
     {    0.000296E-6,   -7058.598461315, 0.460368852},
     {    0.000306E-6,   10575.406682942, 0.554749016},
     {    0.000251E-6,   17298.182327326, 0.834332510},
     {    0.000290E-6,    4732.030627343, 4.759564091},
     {    0.000261E-6,    5884.926846583, 0.298259862},
     {    0.000249E-6,    5547.199336460, 3.749366406},
     {    0.000213E-6,   11712.955318231, 5.415666119},
     {    0.000223E-6,    4701.116501708, 2.703203558},
     {    0.000268E-6,    -640.877607382, 0.283670793},
     {    0.000209E-6,    5636.065016677, 1.238477199},
     {    0.000193E-6,   10177.257679534, 1.943251340},
     {    0.000182E-6,    6283.143160294, 2.456157599},
     {    0.000184E-6,    -227.526189440, 5.888038582},
     {    0.000182E-6,   -6283.008539689, 0.241332086},
     {    0.000228E-6,   -6284.056171060, 2.657323816},
     {    0.000166E-6,    7238.675591600, 5.930629110},
     {    0.000167E-6,    3097.883822726, 5.570955333},
     {    0.000159E-6,    -323.505416657, 5.786670700},
     {    0.000154E-6,   -4136.910433516, 1.517805532},
     {    0.000176E-6,   12029.347187887, 3.139266834},
     {    0.000167E-6,   12132.439962106, 3.556352289},
     {    0.000153E-6,     202.253395174, 1.463313961},
     {    0.000157E-6,   17267.268201691, 1.586837396},
     {    0.000142E-6,   83996.847317911, 0.022670115},
     {    0.000152E-6,   17260.154654690, 0.708528947},
     {    0.000144E-6,    6084.003848555, 5.187075177},
     {    0.000135E-6,    5756.566278634, 1.993229262},
     {    0.000134E-6,    5750.203491159, 3.457197134},
     {    0.000144E-6,    5326.786694021, 6.066193291},
     {    0.000160E-6,   11015.106477335, 1.710431974},
     {    0.000133E-6,    3634.621024518, 2.836451652},
     {    0.000134E-6,   18073.704938650, 5.453106665},
     {    0.000134E-6,    1162.474704408, 5.326898811},
     {    0.000128E-6,    5642.198242609, 2.511652591},
     {    0.000160E-6,     632.783739313, 5.628785365},
     {    0.000132E-6,   13916.019109642, 0.819294053},
     {    0.000122E-6,   14314.168113050, 5.677408071},
     {    0.000125E-6,   12359.966151546, 5.251984735},
     {    0.000121E-6,    5749.452731634, 2.210924603},
     {    0.000136E-6,    -245.831646229, 1.646502367},
     {    0.000120E-6,    5757.317038160, 3.240883049},
     {    0.000134E-6,   12146.667056108, 3.059480037},
     {    0.000137E-6,    6206.809778716, 1.867105418},
     {    0.000141E-6,   17253.041107690, 2.069217456},
     {    0.000129E-6,   -7477.522860216, 2.781469314},
     {    0.000116E-6,    5540.085789459, 4.281176991},
     {    0.000116E-6,    9779.108676125, 3.320925381},
     {    0.000129E-6,    5237.921013804, 3.497704076},
     {    0.000113E-6,    5959.570433334, 0.983210840},
     {    0.000122E-6,    6282.095528923, 2.674938860},
     {    0.000140E-6,     -11.045700264, 4.957936982},
     {    0.000108E-6,   23543.230504682, 1.390113589},
     {    0.000106E-6,  -12569.674818332, 0.429631317},
     {    0.000110E-6,    -266.607041722, 5.501340197},
     {    0.000115E-6,   12559.038152982, 4.691456618},
     {    0.000134E-6,   -2388.894020449, 0.577313584},
     {    0.000109E-6,   10440.274292604, 6.218148717},
     {    0.000102E-6,    -543.918059096, 1.477842615},
     {    0.000108E-6,   21228.392023546, 2.237753948},
     {    0.000101E-6,   -4535.059436924, 3.100492232},
     {    0.000103E-6,      76.266071276, 5.594294322},
     {    0.000104E-6,     949.175608970, 5.674287810},
     {    0.000101E-6,   13517.870106233, 2.196632348},
     {    0.000100E-6,   11933.367960670, 4.056084160},
     {    4.322990E-6,    6283.075849991, 2.642893748},
     {    0.406495E-6,       0.000000000, 4.712388980},
     {    0.122605E-6,   12566.151699983, 2.438140634},
     {    0.019476E-6,     213.299095438, 1.642186981},
     {    0.016916E-6,     529.690965095, 4.510959344},
     {    0.013374E-6,      -3.523118349, 1.502210314},
     {    0.008042E-6,      26.298319800, 0.478549024},
     {    0.007824E-6,     155.420399434, 5.254710405},
     {    0.004894E-6,    5746.271337896, 4.683210850},
     {    0.004875E-6,    5760.498431898, 0.759507698},
     {    0.004416E-6,    5223.693919802, 6.028853166},
     {    0.004088E-6,      -7.113547001, 0.060926389},
     {    0.004433E-6,   77713.771467920, 3.627734103},
     {    0.003277E-6,   18849.227549974, 2.327912542},
     {    0.002703E-6,    6062.663207553, 1.271941729},
     {    0.003435E-6,    -775.522611324, 0.747446224},
     {    0.002618E-6,    6076.890301554, 3.633715689},
     {    0.003146E-6,     206.185548437, 5.647874613},
     {    0.002544E-6,    1577.343542448, 6.232904270},
     {    0.002218E-6,    -220.412642439, 1.309509946},
     {    0.002197E-6,    5856.477659115, 2.407212349},
     {    0.002897E-6,    5753.384884897, 5.863842246},
     {    0.001766E-6,     426.598190876, 0.754113147},
     {    0.001738E-6,    -796.298006816, 2.714942671},
     {    0.001695E-6,     522.577418094, 2.629369842},
     {    0.001584E-6,    5507.553238667, 1.341138229},
     {    0.001503E-6,    -242.728603974, 0.377699736},
     {    0.001552E-6,    -536.804512095, 2.904684667},
     {    0.001370E-6,    -398.149003408, 1.265599125},
     {    0.001889E-6,   -5573.142801634, 4.413514859},
     {    0.001722E-6,    6069.776754553, 2.445966339},
     {    0.001124E-6,    1059.381930189, 5.041799657},
     {    0.001258E-6,     553.569402842, 3.849557278},
     {    0.000831E-6,     951.718406251, 2.471094709},
     {    0.000767E-6,    4694.002954708, 5.363125422},
     {    0.000756E-6,    1349.867409659, 1.046195744},
     {    0.000775E-6,     -11.045700264, 0.245548001},
     {    0.000597E-6,    2146.165416475, 4.543268798},
     {    0.000568E-6,    5216.580372801, 4.178853144},
     {    0.000711E-6,    1748.016413067, 5.934271972},
     {    0.000499E-6,   12036.460734888, 0.624434410},
     {    0.000671E-6,   -1194.447010225, 4.136047594},
     {    0.000488E-6,    5849.364112115, 2.209679987},
     {    0.000621E-6,    6438.496249426, 4.518860804},
     {    0.000495E-6,   -6286.598968340, 1.868201275},
     {    0.000456E-6,    5230.807466803, 1.271231591},
     {    0.000451E-6,    5088.628839767, 0.084060889},
     {    0.000435E-6,    5643.178563677, 3.324456609},
     {    0.000387E-6,   10977.078804699, 4.052488477},
     {    0.000547E-6,  161000.685737473, 2.841633844},
     {    0.000522E-6,    3154.687084896, 2.171979966},
     {    0.000375E-6,    5486.777843175, 4.983027306},
     {    0.000421E-6,    5863.591206116, 4.546432249},
     {    0.000439E-6,    7084.896781115, 0.522967921},
     {    0.000309E-6,    2544.314419883, 3.172606705},
     {    0.000347E-6,    4690.479836359, 1.479586566},
     {    0.000317E-6,     801.820931124, 3.553088096},
     {    0.000262E-6,     419.484643875, 0.606635550},
     {    0.000248E-6,    6836.645252834, 3.014082064},
     {    0.000245E-6,   -1592.596013633, 5.519526220},
     {    0.000225E-6,    4292.330832950, 2.877956536},
     {    0.000214E-6,    7234.794256242, 1.605227587},
     {    0.000205E-6,    5767.611978898, 0.625804796},
     {    0.000180E-6,   10447.387839604, 3.499954526},
     {    0.000229E-6,     199.072001436, 5.632304604},
     {    0.000214E-6,     639.897286314, 5.960227667},
     {    0.000175E-6,    -433.711737877, 2.162417992},
     {    0.000209E-6,     515.463871093, 2.322150893},
     {    0.000173E-6,    6040.347246017, 2.556183691},
     {    0.000184E-6,    6309.374169791, 4.732296790},
     {    0.000227E-6,  149854.400134205, 5.385812217},
     {    0.000154E-6,    8031.092263058, 5.120720920},
     {    0.000151E-6,    5739.157790895, 4.815000443},
     {    0.000197E-6,    7632.943259650, 0.222827271},
     {    0.000197E-6,      74.781598567, 3.910456770},
     {    0.000138E-6,    6055.549660552, 1.397484253},
     {    0.000149E-6,   -6127.655450557, 5.333727496},
     {    0.000137E-6,    3894.181829542, 4.281749907},
     {    0.000135E-6,    9437.762934887, 5.979971885},
     {    0.000139E-6,   -2352.866153772, 4.715630782},
     {    0.000142E-6,    6812.766815086, 0.513330157},
     {    0.000120E-6,   -4705.732307544, 0.194160689},
     {    0.000131E-6,  -71430.695617928, 0.000379226},
     {    0.000124E-6,    6279.552731642, 2.122264908},
     {    0.000108E-6,   -6256.777530192, 0.883445696},
     {    0.143388E-6,    6283.075849991, 1.131453581},
     {    0.006671E-6,   12566.151699983, 0.775148887},
     {    0.001480E-6,     155.420399434, 0.480016880},
     {    0.000934E-6,     213.299095438, 6.144453084},
     {    0.000795E-6,     529.690965095, 2.941595619},
     {    0.000673E-6,    5746.271337896, 0.120415406},
     {    0.000672E-6,    5760.498431898, 5.317009738},
     {    0.000389E-6,    -220.412642439, 3.090323467},
     {    0.000373E-6,    6062.663207553, 3.003551964},
     {    0.000360E-6,    6076.890301554, 1.918913041},
     {    0.000316E-6,     -21.340641002, 5.545798121},
     {    0.000315E-6,    -242.728603974, 1.884932563},
     {    0.000278E-6,     206.185548437, 1.266254859},
     {    0.000238E-6,    -536.804512095, 4.532664830},
     {    0.000185E-6,     522.577418094, 4.578313856},
     {    0.000245E-6,   18849.227549974, 0.587467082},
     {    0.000180E-6,     426.598190876, 5.151178553},
     {    0.000200E-6,     553.569402842, 5.355983739},
     {    0.000141E-6,    5223.693919802, 1.336556009},
     {    0.000104E-6,    5856.477659115, 4.239842759},
     {    0.003826E-6,    6283.075849991, 5.705257275},
     {    0.000303E-6,   12566.151699983, 5.407132842},
     {    0.000209E-6,     155.420399434, 1.989815753}
   };

/* -------------------------------------------------------------------- */

/* Local Variables: */
   double t, tsol, w, elsun, emsun, d, elj, els, wt, w0, w1, w2, w3, w4,
          wf, wj;
   int i;


/* Time since J2000.0 in Julian millennia. */
   t = ( tdb - 51544.5 )/365250;



/* -------------------- Topocentric terms ----------------------------- */

/* Convert UT1 to local solar time in radians. */
   tsol = fmod( ut1, 1.0 )*D2PI - wl;

/* FUNDAMENTAL ARGUMENTS:  Simon et al 1994 */

/* Combine time argument (millennia ) with deg/arcsec factor. */
   w = t / 3600.0;

/* Sun Mean Longitude. */
   elsun = fmod( 280.46645683 + 1296027711.03429*w, 360.0 )*D2R;

/* Sun Mean Anomaly. */
   emsun = fmod( 357.52910918 + 1295965810.481*w, 360.0 )*D2R;

/* Mean Elongation of Moon from Sun. */
   d = fmod( 297.85019547 + 16029616012.090*w, 360.0 )*D2R;

/* Mean Longitude of Jupiter. */
   elj = fmod( 34.35151874 + 109306899.89453*w, 360.0 )*D2R;

/* Mean Longitude of Saturn. */
   els = fmod( 50.07744430 + 44046398.47038*w, 360.0 )*D2R;

/* TOPOCENTRIC TERMS:  Moyer 1981 and Murray 1983. */
   wt =   + 0.00029E-10*u*sin( tsol + elsun - els )
          + 0.00100E-10*u*sin( tsol - 2*emsun )
          + 0.00133E-10*u*sin( tsol - d )
          + 0.00133E-10*u*sin( tsol + elsun - elj )
          - 0.00229E-10*u*sin( tsol + 2*elsun + emsun )
          - 0.0220E-10*v*cos( elsun + emsun )
          + 0.05312E-10*u*sin( tsol - emsun )
          - 0.13677E-10*u*sin( tsol + 2*elsun )
          - 1.3184E-10*v*cos( elsun )
          + 3.17679E-10*u*sin( tsol );



/* --------------- Fairhead model --------------------------------------- */

/* t**0 */
   w0 = 0;
   for( i = 473; i >= 0; i-- ) {
      w0 = w0 + fairhd[ i ][ 0 ]*sin( fairhd[ i ][ 1 ]*t + fairhd[ i ][ 2 ] );
   }

/* t**1 */
   w1 = 0;
   for( i = 678; i >= 474; i-- ) {
      w1 = w1 + fairhd[ i ][ 0 ]*sin( fairhd[ i ][ 1 ]*t + fairhd[ i ][ 2 ] );
   }

/* t**2 */
   w2 = 0;
   for( i = 763; i >= 679; i-- ) {
      w2 = w2 + fairhd[ i ][ 0 ]*sin( fairhd[ i ][ 1 ]*t + fairhd[ i ][ 2 ] );
   }

/* t**3 */
   w3 = 0;
   for( i = 783; i >= 764; i-- ) {
      w3 = w3 + fairhd[ i ][ 0 ]*sin( fairhd[ i ][ 1 ]*t + fairhd[ i ][ 2 ] );
   }

/* t**4 */
   w4 = 0;
   for( i = 786; i >= 784; i-- ) {
      w4 = w4 + fairhd[ i ][ 0 ]*sin( fairhd[ i ][ 1 ]*t + fairhd[ i ][ 2 ] );
   }

/* Multiply by powers of T and combine. */
   wf = t*( t*( t*( t*w4 + w3 ) + w2 ) + w1 ) + w0;

/* Adjustments to use JPL planetary masses instead of IAU. */
   wj =    0.00065E-6  * sin(  6069.776754  *t  +  4.021194  )  +
           0.00033E-6  * sin(   213.299095  *t  +  5.543132  )  +
        ( -0.00196E-6  * sin( 6208.294251   *t  +  5.696701  ) ) +
        ( -0.00173E-6  * sin(   74.781599   *t  +  2.435900  ) ) +
           0.03638E-6*t*t;



/* -------------------------------------------------------------------- */

/* Final result:  TDB-TT in seconds. */
   return wt + wf + wj;

}

static void TimeAdd( AstTimeMap *this, const char *cvt, const double args[], int *status ) {
/*
*++
*  Name:
c     astTimeAdd
f     AST_TIMEADD

*  Purpose:
*     Add a time coordinate conversion to a TimeMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "timemap.h"
c     void astTimeAdd( AstTimeMap *this, const char *cvt, const double args[] )
f     CALL AST_TIMEADD( THIS, CVT, ARGS, STATUS )

*  Class Membership:
*     TimeMap method.

*  Description:
c     This function adds one of the standard time coordinate
f     This routine adds one of the standard time coordinate
*     system conversions listed below to an existing TimeMap.
*
c     When a TimeMap is first created (using astTimeMap), it simply
f     When a TimeMap is first created (using AST_TIMEMAP), it simply
c     performs a unit (null) Mapping. By using astTimeAdd (repeatedly
f     performs a unit (null) Mapping. By using AST_TIMEADD (repeatedly
*     if necessary), one or more coordinate conversion steps may then
*     be added, which the TimeMap will perform in sequence. This allows
*     multi-step conversions between a variety of time coordinate
*     systems to be assembled out of the building blocks provided by
*     this class.
*
*     Normally, if a TimeMap's Invert attribute is zero (the default),
*     then its forward transformation is performed by carrying out
*     each of the individual coordinate conversions specified by
c     astTimeAdd in the order given (i.e. with the most recently added
f     AST_TIMEADD in the order given (i.e. with the most recently added
*     conversion applied last).
*
*     This order is reversed if the TimeMap's Invert attribute is
*     non-zero (or if the inverse transformation is requested by any
*     other means) and each individual coordinate conversion is also
*     replaced by its own inverse. This process inverts the overall
*     effect of the TimeMap. In this case, the first conversion to be
*     applied would be the inverse of the one most recently added.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the TimeMap.
c     cvt
f     CVT = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string which identifies the
f        A character string which identifies the
*        time coordinate conversion to be added to the
*        TimeMap. See the "Available Conversions" section for details of
*        those available.
c     args
f     ARGS( * ) = DOUBLE PRECISION (Given)
*        An array containing argument values for the time
*        coordinate conversion. The number of arguments required, and
*        hence the number of array elements used, depends on the
*        conversion specified (see the "Available Conversions"
*        section). This array is ignored
c        and a NULL pointer may be supplied
*        if no arguments are needed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - When assembling a multi-stage conversion, it can sometimes be
*     difficult to determine the most economical conversion path. A solution
*     to this is to include all the steps which are (logically) necessary,
*     but then to use
c     astSimplify to simplify the resulting
f     AST_SIMPLIFY to simplify the resulting
*     TimeMap. The simplification process will eliminate any steps
*     which turn out not to be needed.
c     - This function does not check to ensure that the sequence of
f     - This routine does not check to ensure that the sequence of
*     coordinate conversions added to a TimeMap is physically
*     meaningful.

*  Available Conversions:
*     The following strings (which are case-insensitive) may be supplied
c     via the "cvt" parameter to indicate which time coordinate
f     via the CVT argument to indicate which time coordinate
*     conversion is to be added to the TimeMap. Where arguments are needed by
*     the conversion, they are listed in parentheses. Values for
c     these arguments should be given, via the "args" array, in the
f     these arguments should be given, via the ARGS array, in the
*     order indicated. Units and argument names are described at the end of
*     the list of conversions, and "MJD" means Modified Julian Date.
*
*     - "MJDTOMJD"  (MJDOFF1,MJDOFF2): Convert MJD from one offset to another.
*     - "MJDTOJD"  (MJDOFF,JDOFF): Convert MJD to Julian Date.
*     - "JDTOMJD"  (JDOFF,MJDOFF): Convert Julian Date to MJD.
*     - "MJDTOBEP" (MJDOFF,BEPOFF): Convert MJD to Besselian epoch.
*     - "BEPTOMJD" (BEPOFF,MJDOFF): Convert Besselian epoch to MJD.
*     - "MJDTOJEP" (MJDOFF,JEPOFF): Convert MJD to Julian epoch.
*     - "JEPTOMJD" (JEPOFF,MJDOFF): Convert Julian epoch to MJD.
*     - "TAITOUTC" (MJDOFF): Convert a TAI MJD to a UTC MJD.
*     - "UTCTOTAI" (MJDOFF): Convert a UTC MJD to a TAI MJD.
*     - "TAITOTT"  (MJDOFF): Convert a TAI MJD to a TT MJD.
*     - "TTTOTAI"  (MJDOFF): Convert a TT MJD to a TAI MJD.
*     - "TTTOTDB"  (MJDOFF, OBSLON, OBSLAT, OBSALT): Convert a TT MJD to a TDB MJD.
*     - "TDBTOTT"  (MJDOFF, OBSLON, OBSLAT, OBSALT): Convert a TDB MJD to a TT MJD.
*     - "TTTOTCG"  (MJDOFF): Convert a TT MJD to a TCG MJD.
*     - "TCGTOTT"  (MJDOFF): Convert a TCG MJD to a TT MJD.
*     - "TDBTOTCB" (MJDOFF): Convert a TDB MJD to a TCB MJD.
*     - "TCBTOTDB" (MJDOFF): Convert a TCB MJD to a TDB MJD.
*     - "UTTOGMST" (MJDOFF): Convert a UT MJD to a GMST MJD.
*     - "GMSTTOUT" (MJDOFF): Convert a GMST MJD to a UT MJD.
*     - "GMSTTOLMST" (MJDOFF, OBSLON, OBSLAT): Convert a GMST MJD to a LMST MJD.
*     - "LMSTTOGMST" (MJDOFF, OBSLON, OBSLAT): Convert a LMST MJD to a GMST MJD.
*     - "LASTTOLMST" (MJDOFF, OBSLON, OBSLAT): Convert a GMST MJD to a LMST MJD.
*     - "LMSTTOLAST" (MJDOFF, OBSLON, OBSLAT): Convert a LMST MJD to a GMST MJD.
*     - "UTTOUTC" (DUT1): Convert a UT1 MJD to a UTC MJD.
*     - "UTCTOUT" (DUT1): Convert a UTC MJD to a UT1 MJD.
*     - "LTTOUTC" (LTOFF): Convert a Local Time MJD to a UTC MJD.
*     - "UTCTOLT" (LTOFF): Convert a UTC MJD to a Local Time MJD.
*
*     The units for the values processed by the above conversions are as
*     follows:
*
*     - Julian epochs and offsets: Julian years
*     - Besselian epochs and offsets: Tropical years
*     - Modified Julian Dates and offsets: days
*     - Julian Dates and offsets: days
*
*     The arguments used in the above conversions are the zero-points
*     used by the
c     astTransform function.
f     AST_TRANSFORM routine.
*     The axis values supplied and returned by
c     astTransform
f     AST_TRANSFORM
*     are offsets away from these zero-points:
*
*     - MJDOFF: The zero-point being used with MJD values.
*     - JDOFF: The zero-point being used with Julian Date values.
*     - BEPOFF: The zero-point being used with Besselian epoch values.
*     - JEPOFF: The zero-point being used with Julian epoch values.
*     - OBSLON: Observer longitude in radians (+ve westwards).
*     - OBSLAT: Observer geodetic latitude (IAU 1975) in radians (+ve northwards).
*     - OBSALT: Observer geodetic altitude (IAU 1975) in metres.
*     - DUT1: The UT1-UTC value to use.
*     - LTOFF: The offset between Local Time and UTC (in hours, positive
*     for time zones east of Greenwich).
*--
*/

/* Local Variables: */
   int cvttype;                  /* Conversion type code */

/* Check the inherited status. */
   if ( !astOK ) return;

/* Validate the type string supplied and obtain the equivalent
   conversion type code. */
   cvttype = CvtCode( cvt, status );

/* If the string was not recognised, then report an error. */
   if ( astOK && ( cvttype == AST__TIME_NULL ) ) {
      astError( AST__TIMIN,
                "%s(%s): Invalid TimeMap time coordinate "
                "conversion type \"%s\".", status, "astAddTime", astGetClass( this ), cvt );
   }

/* Add the new conversion to the TimeMap. */
   AddTimeCvt( this, cvttype, args, status );
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a TimeMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     TimeMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a TimeMap and a set of points encapsulated
*     in a PointSet and transforms the points so as to perform the
*     sequence of time coordinate conversions specified by
*     previous invocations of astTimeAdd.

*  Parameters:
*     this
*        Pointer to the TimeMap.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the TimeMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstTimeMap *map;              /* Pointer to TimeMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *args;                 /* Pointer to argument list for conversion */
   double *time;                 /* Pointer to output time axis value array */
   double gmstx;                 /* GMST offset (in days) */
   double tai;                   /* Absolute TAI value (in days) */
   double tdb;                   /* Absolute TDB value (in days) */
   double tt;                    /* Absolute TT value (in days) */
   double utc;                   /* Absolute UTC value (in days) */
   int ct;                       /* Conversion type */
   int cvt;                      /* Loop counter for conversions */
   int end;                      /* Termination index for conversion loop */
   int inc;                      /* Increment for conversion loop */
   int ncoord_in;                /* Number of coordinates per input point */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */
   int start;                    /* Starting index for conversion loop */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the TimeMap. */
   map = (AstTimeMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   coordinate conversions needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   PointSet and obtain pointers for accessing the input and output coordinate
   values. */
   ncoord_in = astGetNcoord( in );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse transformation, according
   to the direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( this ) ) forward = !forward;

/* Transform the coordinate values. */
/* -------------------------------- */
/* Use "time" as a synonym for the array of time axis values stored in
   the output PointSet. */
   if ( astOK ) {
      time = ptr_out[ 0 ];

/* Initialise the output coordinate values by copying the input ones. */
      if( time != ptr_in[ 0 ] ) {
         (void) memcpy( time, ptr_in[ 0 ], sizeof( double ) * (size_t) npoint );
      }

/* We will loop to apply each time coordinate conversion in turn to the
   (time) array. However, if the inverse transformation was requested,
   we must loop through these transformations in reverse order, so set up
   appropriate limits and an increment to control this loop. */
      start = forward ? 0 : map->ncvt - 1;
      end = forward ? map->ncvt : -1;
      inc = forward ? 1 : -1;

/* Loop through the coordinate conversions in the required order and obtain a
   pointer to the argument list for the current conversion. */
      for ( cvt = start; cvt != end; cvt += inc ) {
         args = map->cvtargs[ cvt ];

/* Classify the SLALIB sky coordinate conversion to be applied. */
         ct = map->cvttype[ cvt ];
         switch ( ct ) {

/* MJD to MJD. */
/* ---------- */
            case AST__MJDTOMJD:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 2 ];
                     }
                  }
               }
               break;

/* MJD to JD. */
/* ---------- */
            case AST__MJDTOJD:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 2 ];
                     }
                  }
               }
               break;

/* JD to MJD. */
/* ---------- */
            case AST__JDTOMJD:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 2 ];
                     }
                  }
               }
               break;

/* MJD to Besselian epoch. */
/* ----------------------- */
            case AST__MJDTOBEP:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpb( time[ point ] ) + args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpb2d( time[ point ] ) + args[ 3 ];
                     }
                  }
               }
               break;

/* Besselian epoch to MJD. */
/* ----------------------- */
            case AST__BEPTOMJD:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpb2d( time[ point ] ) + args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpb( time[ point ] ) + args[ 3 ];
                     }
                  }
               }
               break;

/* MJD to Julian epoch. */
/* -------------------- */
            case AST__MJDTOJEP:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpj( time[ point ] ) + args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpj2d( time[ point ] ) + args[ 3 ];
                     }
                  }
               }
               break;

/* Julian epoch to MJD. */
/* -------------------- */
            case AST__JEPTOMJD:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpj2d( time[ point ] ) + args[ 2 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = palEpj( time[ point ] ) + args[ 3 ];
                     }
                  }
               }
               break;

/* TAI to UTC. */
/* ----------- */
            case AST__TAITOUTC:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += astDat( time[ point ] +
                                              args[ 0 ], 0 )/SPD;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += astDat( time[ point ] +
                                              args[ 0 ], 1 )/SPD;
                     }
                  }
               }
               break;

/* UTC to TAI. */
/* ----------- */
            case AST__UTCTOTAI:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += astDat( time[ point ] +
                                              args[ 0 ], 1 )/SPD;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += astDat( time[ point ] +
                                              args[ 0 ], 0 )/SPD;
                     }
                  }
               }
               break;

/* TAI to TT. */
/* ---------- */
            case AST__TAITOTT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += (TTOFF/SPD);
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= (TTOFF/SPD);
                     }
                  }
               }
               break;

/* TT to TAI. */
/* ---------- */
            case AST__TTTOTAI:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= (TTOFF/SPD);
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += (TTOFF/SPD);
                     }
                  }
               }
               break;

/* TT to TDB. */
/* ---------- */
/* For the purpose of estimating TDB-TT, we assume UTC is a good approximation
   to UT1, and that TT is a good approximation to TDB. */
            case AST__TTTOTDB:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        tt = time[ point ] + args[ 0 ];
                        tai = tt - (TTOFF/SPD);
                        utc = tai + astDat( tai, 0 )/SPD;
                        time[ point ] += Rcc( tt, utc, args[ 1 ], args[ 4 ],
                                              args[ 5 ], status )/SPD;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        tdb = time[ point ] + args[ 0 ];
                        tai = tdb - (TTOFF/SPD);
                        utc = tai + astDat( tai, 0 )/SPD;
                        time[ point ] -= Rcc( tdb, utc, args[ 1 ], args[ 4 ],
                                                args[ 5 ], status )/SPD;
                     }
                  }
               }
               break;

/* TDB to TT. */
/* ---------- */
/* For the purpose of estimating TDB-TT, we assume UTC is a good approximation
   to UT1, and that TT is a good approximation to TDB. */
            case AST__TDBTOTT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        tdb = time[ point ] + args[ 0 ];
                        tai = tdb - (TTOFF/SPD);
                        utc = tai + astDat( tai, 0 )/SPD;
                        time[ point ] -= Rcc( tdb, utc, args[ 1 ], args[ 4 ],
                                                args[ 5 ], status )/SPD;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        tt = time[ point ] + args[ 0 ];
                        tai = tt - (TTOFF/SPD);
                        utc = tai + astDat( tai, 0 )/SPD;
                        time[ point ] += Rcc( tt, utc, args[ 1 ], args[ 4 ],
                                              args[ 5 ], status )/SPD;
                     }
                  }
               }
               break;

/* TT to TCG. */
/* ---------- */
            case AST__TTTOTCG:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += time[ point ]*LG + args[ 1 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = ( time[ point ] - args[ 1 ] ) /
                                         ( 1.0 + LG );
                     }
                  }
               }
               break;

/* TCG to TT. */
/* ---------- */
            case AST__TCGTOTT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = ( time[ point ] - args[ 1 ] ) /
                                         ( 1.0 + LG );
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += time[ point ]*LG + args[ 1 ];
                     }
                  }
               }
               break;

/* TDB to TCB. */
/* ----------- */
/* For the purpose of estimating TDB-TT, we assume UTC is a good approximation
   to UT1, and that TT is a good approximation to both TDB and TCB. */
            case AST__TDBTOTCB:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += time[ point ]*LB + args[ 1 ];
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = ( time[ point ] - args[ 1 ] ) /
                                         ( 1.0 + LB );
                     }
                  }
               }
               break;

/* TCB to TDB. */
/* ----------- */
/* For the purpose of estimating TDB-TT, we assume UTC is a good approximation
   to UT1, and that TT is a good approximation to TDB. */
            case AST__TCBTOTDB:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = ( time[ point ] - args[ 1 ] ) /
                                         ( 1.0 + LB );
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += time[ point ]*LB + args[ 1 ];
                     }
                  }
               }
               break;

/* UT to GMST . */
/* ------------ */
            case AST__UTTOGMST:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = Gmsta( time[ point ], args[ 0 ], 1, status );
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = Gmsta( time[ point ], args[ 0 ], 0, status );
                     }
                  }
               }
               break;

/* GMST to UT. */
/* ----------- */
            case AST__GMSTTOUT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = Gmsta( time[ point ], args[ 0 ], 0, status );
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] = Gmsta( time[ point ], args[ 0 ], 1, status );
                     }
                  }
               }
               break;

/* GMST to LMST. */
/* ------------- */
            case AST__GMSTTOLMST:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 1 ]/D2PI;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 1 ]/D2PI;
                     }
                  }
               }
               break;

/* LMST to GMST. */
/* ------------- */
            case AST__LMSTTOGMST:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 1 ]/D2PI;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 1 ]/D2PI;
                     }
                  }
               }
               break;

/* UT1 to UTC. */
/* ------------- */
            case AST__UTTOUTC:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 0 ]/86400.0;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 0 ]/86400.0;
                     }
                  }
               }
               break;


/* UTC to UT1. */
/* ------------- */
            case AST__UTCTOUT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 0 ]/86400.0;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 0 ]/86400.0;
                     }
                  }
               }
               break;

/* LT to UTC. */
/* ---------- */
            case AST__LTTOUTC:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 0 ]/24.0;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 0 ]/24.0;
                     }
                  }
               }
               break;


/* UTC to LT. */
/* ---------- */
            case AST__UTCTOLT:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] += args[ 0 ]/24.0;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        time[ point ] -= args[ 0 ]/24.0;
                     }
                  }
               }
               break;

/* LMST to LAST. */
/* ------------- */
/* Calculating the equation of the equinoxes required TDB. So we need to
   convert the given LMST to TDB. We first convert LMST to UT1. UT1 is
   equal to UTC to within 1 second. We then add on 32 seconds to get TAI
   (this value is correct since 1999 - for earlier epochs an error of the
   order of a minute will be introduced in the TAI value). We then add on
   TTOFF seconds to get TT. This TT is then used as an approximation to
   TDB. The total error in TDB is of the order of a few minutes, which
   corresponds to an error of a few tens of microseconds in the equation of
   the equinoxes. The sla precession-nutation model is accurate to around 3
   mas = 200 us, so the error in TDB will be insignificant. */
            case AST__LMSTTOLAST:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        gmstx = time[ point ] + args[ 1 ]/D2PI;
                        tdb = Gmsta( gmstx, args[ 0 ], 0, status )
                              + args[ 0 ] + (32 + TTOFF)/SPD;
                        time[ point ] += palEqeqx( tdb )/D2PI;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        gmstx = time[ point ] + args[ 1 ]/D2PI;
                        tdb = Gmsta( gmstx, args[ 0 ], 0, status )
                              + args[ 0 ] + (32+TTOFF)/SPD;
                        time[ point ] -= palEqeqx( tdb )/D2PI;
                     }
                  }
               }
               break;

/* LAST to LMST. */
/* ------------- */
            case AST__LASTTOLMST:
               if ( forward ) {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        gmstx = time[ point ] + args[ 1 ]/D2PI;
                        tdb = Gmsta( gmstx, args[ 0 ], 0, status )
                              + args[ 0 ] + (32+TTOFF)/SPD;
                        time[ point ] -= palEqeqx( tdb )/D2PI;
                     }
                  }
               } else {
                  for ( point = 0; point < npoint; point++ ) {
                     if ( time[ point ] != AST__BAD ) {
                        gmstx = time[ point ] + args[ 1 ]/D2PI;
                        tdb = Gmsta( gmstx, args[ 0 ], 0, status )
                              + args[ 0 ] + (32 + TTOFF)/SPD;
                        time[ point ] += palEqeqx( tdb )/D2PI;
                     }
                  }
               }

         }
      }
   }

/* If an error has occurred and a new PointSet may have been created, then
   clean up by annulling it. In any case, ensure that a NULL result is
   returned.*/
   if ( !astOK ) {
      if ( !out ) result = astAnnul( result );
      result = NULL;
   }

/* Return a pointer to the output PointSet. */
   return result;
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for TimeMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for TimeMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstTimeMap *in;                /* Pointer to input TimeMap */
   AstTimeMap *out;               /* Pointer to output TimeMap */
   int cvt;                       /* Loop counter for coordinate conversions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output TimeMap structures. */
   in = (AstTimeMap *) objin;
   out = (AstTimeMap *) objout;

/* For safety, first clear any references to the input memory from the output
   TimeMap. */
   out->cvtargs = NULL;
   out->cvttype = NULL;

/* Allocate memory for the output array of argument list pointers. */
   out->cvtargs = astMalloc( sizeof( double * ) * (size_t) in->ncvt );

/* If necessary, allocate memory and make a copy of the input array of
   coordinate conversion codes. */
   if ( in->cvttype ) out->cvttype = astStore( NULL, in->cvttype,
                                               sizeof( int )
                                               * (size_t) in->ncvt );

/* If OK, loop through each conversion in the input TimeMap and make a copy of
   its argument list, storing the new pointer in the output argument list
   array. */
   if ( astOK ) {
      for ( cvt = 0; cvt < in->ncvt; cvt++ ) {
         out->cvtargs[ cvt ] = astStore( NULL, in->cvtargs[ cvt ],
                                         astSizeOf( in->cvtargs[ cvt ] ) );
      }

/* If an error occurred while copying the argument lists, loop through the
   conversions again and clean up by ensuring that the new memory allocated for
   each argument list is freed. */
      if ( !astOK ) {
         for ( cvt = 0; cvt < in->ncvt; cvt++ ) {
            out->cvtargs[ cvt ] = astFree( out->cvtargs[ cvt ] );
	 }
      }
   }

/* If an error occurred, free all other memory allocated above. */
   if ( !astOK ) {
      out->cvtargs = astFree( out->cvtargs );
      out->cvttype = astFree( out->cvttype );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for TimeMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for TimeMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstTimeMap *this;              /* Pointer to TimeMap */
   int cvt;                       /* Loop counter for coordinate conversions */

/* Obtain a pointer to the TimeMap structure. */
   this = (AstTimeMap *) obj;

/* Loop to free the memory containing the argument list for each coordinate
   conversion. */
   for ( cvt = 0; cvt < this->ncvt; cvt++ ) {
      this->cvtargs[ cvt ] = astFree( this->cvtargs[ cvt ] );
   }

/* Free the memory holding the array of conversion types and the array of
   argument list pointers. */
   this->cvtargs = astFree( this->cvtargs );
   this->cvttype = astFree( this->cvttype );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for TimeMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the TimeMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the TimeMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstTimeMap *this;             /* Pointer to the TimeMap structure */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *sval;             /* Pointer to string value */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int ival;                     /* Integer value */
   int nargs;                    /* Number of user-supplied arguments */
   int szargs;                   /* Number of stored arguments */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TimeMap structure. */
   this = (AstTimeMap *) this_object;

/* Write out values representing the instance variables for the TimeMap
   class.  Accompany these with appropriate comment strings, possibly
   depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* Number of conversion steps. */
/* --------------------------- */
/* Regard this as "set" if it is non-zero. */
   ival = this->ncvt;
   set = ( ival != 0 );
   astWriteInt( channel, "Ntime", set, 0, ival, "Number of conversion steps" );

/* Write out data for each conversion step... */
   for ( icvt = 0; icvt < this->ncvt; icvt++ ) {

/* Conversion type. */
/* ---------------- */
/* Change each conversion type code into an equivalent string and
   obtain associated descriptive information. If the conversion code
   was not recognised, report an error and give up. */
      if ( astOK ) {
         sval = CvtString( this->cvttype[ icvt ], &comment,
                           &nargs, &szargs, argdesc, status );
         if ( astOK && !sval ) {
            astError( AST__TIMIN,
                      "astWrite(%s): Corrupt %s contains invalid TimeMap "
                      "time coordinate conversion code (%d).", status,
                      astGetClass( channel ), astGetClass( this ),
                      (int) this->cvttype[ icvt ] );
            break;
         }

/* Create an appropriate keyword and write out the conversion code
   information. */
         (void) sprintf( key, "Time%d", icvt + 1 );
         astWriteString( channel, key, 1, 1, sval, comment );

/* Write out data for each conversion argument... */
         for ( iarg = 0; iarg < szargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and write out the argument value,
   accompanied by the descriptive comment obtained above. */
            if( this->cvtargs[ icvt ][ iarg ] != AST__BAD ) {
               (void) sprintf( key, "Time%d%c", icvt + 1, ALPHABET[ iarg ] );
               astWriteDouble( channel, key, 1, 1, this->cvtargs[ icvt ][ iarg ],
                               argdesc[ iarg ] );
            }
         }

/* Quit looping if an error occurs. */
         if ( !astOK ) break;
      }
   }

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsATimeMap and astCheckTimeMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(TimeMap,Mapping)
astMAKE_CHECK(TimeMap)

AstTimeMap *astTimeMap_( int flags, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astTimeMap
f     AST_TIMEMAP

*  Purpose:
*     Create a TimeMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "timemap.h"
c     AstTimeMap *astTimeMap( int flags, const char *options, ... )
f     RESULT = AST_TIMEMAP( FLAGS, OPTIONS, STATUS )

*  Class Membership:
*     TimeMap constructor.

*  Description:
*     This function creates a new TimeMap and optionally initialises
*     its attributes.
*
*     A TimeMap is a specialised form of 1-dimensional Mapping which can be
*     used to represent a sequence of conversions between standard time
*     coordinate systems.
*
*     When a TimeMap is first created, it simply performs a unit
c     (null) Mapping. Using the astTimeAdd
f     (null) Mapping. Using the AST_TIMEADD
c     function, a series of coordinate conversion steps may then be
f     routine, a series of coordinate conversion steps may then be
*     added. This allows multi-step conversions between a variety of
*     time coordinate systems to be assembled out of a set of building
*     blocks.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astTimeAdd function.
f     see the description of the AST_TIMEADD routine.

*  Parameters:
c     flags
f     FLAGS = INTEGER (Given)
c        This parameter is reserved for future use and should currently
f        This argument is reserved for future use and should currently
*        always be set to zero.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new TimeMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new TimeMap. The syntax used is identical to that for the
f        AST_SET routine. If no initialisation is required, a blank
f        value may be supplied.
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
c     astTimeMap()
f     AST_TIMEMAP = INTEGER
*        A pointer to the new TimeMap.

*  Notes:
*     - The nature and units of the coordinate values supplied for the
*     first input (i.e. the time input) of a TimeMap must be appropriate
*     to the first conversion step applied by the TimeMap. For instance, if
*     the first conversion step is "MJDTOBEP" (Modified Julian Date to
*     Besselian epoch) then the coordinate values for the first input should
*     be date in units of days. Similarly, the nature and units of the
*     coordinate values returned by a TimeMap will be determined by the
*     last conversion step applied by the TimeMap.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTimeMap *new;              /* Pointer to the new TimeMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the TimeMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitTimeMap( NULL, sizeof( AstTimeMap ), !class_init, &class_vtab,
                        "TimeMap", flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new TimeMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new TimeMap. */
   return new;
}

AstTimeMap *astTimeMapId_( int flags, const char *options, ... ) {
/*
*  Name:
*     astTimeMapId_

*  Purpose:
*     Create a TimeMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "timemap.h"
*     AstTimeMap *astTimeMapId_( int flags, const char *options, ... )

*  Class Membership:
*     TimeMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astTimeMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astTimeMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astTimeMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astTimeMap_.

*  Returned Value:
*     The ID value associated with the new TimeMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTimeMap *new;              /* Pointer to the new TimeMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the TimeMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitTimeMap( NULL, sizeof( AstTimeMap ), !class_init, &class_vtab,
                        "TimeMap", flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new TimeMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new TimeMap. */
   return astMakeId( new );
}

AstTimeMap *astInitTimeMap_( void *mem, size_t size, int init,
                             AstTimeMapVtab *vtab, const char *name,
                             int flags, int *status ) {
/*
*+
*  Name:
*     astInitTimeMap

*  Purpose:
*     Initialise a TimeMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "timemap.h"
*     AstTimeMap *astInitTimeMap( void *mem, size_t size, int init,
*                               AstTimeMapVtab *vtab, const char *name,
*                               int flags )

*  Class Membership:
*     TimeMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new TimeMap object. It allocates memory (if necessary) to accommodate
*     the TimeMap plus any additional data associated with the derived class.
*     It then initialises a TimeMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a TimeMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the TimeMap is to be initialised.
*        This must be of sufficient size to accommodate the TimeMap data
*        (sizeof(TimeMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the TimeMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the TimeMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the TimeMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new TimeMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astClass
*        method).
*     flags
*        This parameter is reserved for future use. It is currently ignored.

*  Returned Value:
*     A pointer to the new TimeMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstTimeMap *new;               /* Pointer to the new TimeMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitTimeMapVtab( vtab, name );

/* Initialise a 1D Mapping structure (the parent class) as the first component
   within the TimeMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstTimeMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       1, 1, 1, 1 );

   if ( astOK ) {

/* Initialise the TimeMap data. */
/* --------------------------- */
/* The initial state is with no conversions set, in which condition the
   TimeMap simply implements a unit mapping. */
      new->ncvt = 0;
      new->cvtargs = NULL;
      new->cvttype = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstTimeMap *astLoadTimeMap_( void *mem, size_t size,
                           AstTimeMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadTimeMap

*  Purpose:
*     Load a TimeMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "timemap.h"
*     AstTimeMap *astLoadTimeMap( void *mem, size_t size,
*                               AstTimeMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     TimeMap loader.

*  Description:
*     This function is provided to load a new TimeMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     TimeMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a TimeMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the TimeMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        TimeMap data (sizeof(TimeMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the TimeMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the TimeMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstTimeMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new TimeMap. If this is NULL, a pointer to
*        the (static) virtual function table for the TimeMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "TimeMap" is used instead.

*  Returned Value:
*     A pointer to the new TimeMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstTimeMap *new;              /* Pointer to the new TimeMap */
   char *sval;                   /* Pointer to string value */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int nargs;                    /* Number of user-supplied arguments */
   int szargs;                   /* Number of stored arguments */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this TimeMap. In this case the
   TimeMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstTimeMap );
      vtab = &class_vtab;
      name = "TimeMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitTimeMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built TimeMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "TimeMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Number of conversion steps. */
/* --------------------------- */
/* Read the number of conversion steps and allocate memory to hold
   data for each step. */
      new->ncvt = astReadInt( channel, "ntime", 0 );
      if ( new->ncvt < 0 ) new->ncvt = 0;
      new->cvttype = astMalloc( sizeof( int ) * (size_t) new->ncvt );
      new->cvtargs = astMalloc( sizeof( double * ) * (size_t) new->ncvt );

/* If an error occurred, ensure that all allocated memory is freed. */
      if ( !astOK ) {
         new->cvttype = astFree( new->cvttype );
         new->cvtargs = astFree( new->cvtargs );

/* Otherwise, initialise the argument pointer array. */
      } else {
         for ( icvt = 0; icvt < new->ncvt; icvt++ ) {
            new->cvtargs[ icvt ] = NULL;
         }

/* Read in data for each conversion step... */
         for ( icvt = 0; icvt < new->ncvt; icvt++ ) {

/* Conversion type. */
/* ---------------- */
/* Create an appropriate keyword and read the string representation of
   the conversion type. */
            (void) sprintf( key, "time%d", icvt + 1 );
            sval = astReadString( channel, key, NULL );

/* If no value was read, report an error. */
            if ( astOK ) {
               if ( !sval ) {
                  astError( AST__BADIN,
                            "astRead(%s): A time coordinate conversion "
                            "type is missing from the input TimeMap data.", status,
                            astGetClass( channel ) );

/* Otherwise, convert the string representation into the required
   conversion type code. */
               } else {
                  new->cvttype[ icvt ] = CvtCode( sval, status );

/* If the string was not recognised, report an error. */
                  if ( new->cvttype[ icvt ] == AST__TIME_NULL ) {
                     astError( AST__BADIN,
                              "astRead(%s): Invalid time conversion "
                              "type \"%s\" in TimeMap data.", status,
                              astGetClass( channel ), sval );
                  }
               }

/* Free the memory holding the string value. */
               sval = astFree( sval );
            }

/* Obtain the number of arguments associated with the conversion and
   allocate memory to hold them. */
            (void) CvtString( new->cvttype[ icvt ], &comment,
                              &nargs, &szargs, argdesc, status );
            new->cvtargs[ icvt ] = astMalloc( sizeof( double ) *
                                              (size_t) szargs );

/* Read in data for each argument... */
            if ( astOK ) {
               for ( iarg = 0; iarg < szargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and read each argument value. */
                  (void) sprintf( key, "time%d%c", icvt + 1, ALPHABET[ iarg ] );
                  new->cvtargs[ icvt ][ iarg ] = astReadDouble( channel, key,
                                                                AST__BAD );
               }
            }

/* Quit looping if an error occurs. */
            if ( !astOK ) break;
         }
      }

/* If an error occurred, clean up by deleting the new TimeMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new TimeMap pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
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
void astTimeAdd_( AstTimeMap *this, const char *cvt, const double args[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,TimeMap,TimeAdd))( this, cvt, args, status );
}




