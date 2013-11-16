/*
*class++
*  Name:
*     SlaMap

*  Purpose:
*     Sequence of celestial coordinate conversions.

*  Constructor Function:
c     astSlaMap (also see astSlaAdd)
f     AST_SLAMAP (also see AST_SLAADD)

*  Description:
*     An SlaMap is a specialised form of Mapping which can be used to
*     represent a sequence of conversions between standard celestial
*     (longitude, latitude) coordinate systems.
*
*     When an SlaMap is first created, it simply performs a unit
c     (null) Mapping on a pair of coordinates. Using the astSlaAdd
f     (null) Mapping on a pair of coordinates. Using the AST_SLAADD
c     function, a series of coordinate conversion steps may then be
f     routine, a series of coordinate conversion steps may then be
*     added, selected from those provided by the SLALIB Positional
*     Astronomy Library (Starlink User Note SUN/67). This allows
*     multi-step conversions between a variety of celestial coordinate
*     systems to be assembled out of the building blocks provided by
*     SLALIB.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astSlaAdd function.
f     see the description of the AST_SLAADD routine.

*  Inheritance:
*     The SlaMap class inherits from the Mapping class.

*  Attributes:
*     The SlaMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     In addition to those functions applicable to all Mappings, the
c     following function may also be applied to all SlaMaps:
f     In addition to those routines applicable to all Mappings, the
f     following routine may also be applied to all SlaMaps:
*
c     - astSlaAdd: Add a celestial coordinate conversion to an SlaMap
f     - AST_SLAADD: Add a celestial coordinate conversion to an SlaMap

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     25-APR-1996 (RFWS):
*        Original version.
*     28-MAY-1996 (RFWS):
*        Fixed bug in argument order to palMappa for AST__SLA_AMP case.
*     26-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     23-MAY-1997 (RFWS):
*        Over-ride the astMapMerge method.
*     28-MAY-1997 (RFWS):
*        Use strings to specify conversions for the public interface
*        and convert to macros (from an enumerated type) for the
*        internal representation. Tidy the public prologues.
*     8-JAN-2003 (DSB):
*        - Changed private InitVtab method to protected astInitSlaMapVtab
*        method.
*        - Included STP conversion functions.
*     11-JUN-2003 (DSB):
*        - Added HFK5Z and FK5HZ conversion functions.
*     28-SEP-2003 (DSB):
*        - Added HEEQ and EQHE conversion functions.
*     2-DEC-2004 (DSB):
*        - Added J2000H and HJ2000 conversion functions.
*     15-AUG-2005 (DSB):
*        - Added H2E and E2H conversion functions.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     22-FEB-2006 (DSB):
*        Cache results returned by palMappa in order to increase speed.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*     31-AUG-2007 (DSB):
*        - Modify H2E and E2H conversion functions so that they convert to
*        and from apparent (HA,Dec) rather than topocentric (HA,Dec) (i.e.
*        include a correction for diurnal aberration). This requires an
*        extra conversion argument holding the magnitude of the diurnal
*        aberration vector.
*        - Correct bug in the simplification of adjacent AMP and MAP
*        conversions.
*     15-NOV-2013 (DSB):
*        Fix bug in merging of adjacent AMP and MAP conversions (MapMerge
*        did not take account of the fact that the arguments for these
*        two conversions are stored in swapped order).

*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SlaMap

/* Codes to identify SLALIB sky coordinate conversions. */
#define AST__SLA_NULL    0       /* Null value */
#define AST__SLA_ADDET   1       /* Add E-terms of aberration */
#define AST__SLA_SUBET   2       /* Subtract E-terms of aberration */
#define AST__SLA_PREBN   3       /* Bessel-Newcomb (FK4) precession */
#define AST__SLA_PREC    4       /* Apply IAU 1975 (FK5) precession model */
#define AST__SLA_FK45Z   5       /* FK4 to FK5, no proper motion or parallax */
#define AST__SLA_FK54Z   6       /* FK5 to FK4, no proper motion or parallax */
#define AST__SLA_AMP     7       /* Geocentric apparent to mean place */
#define AST__SLA_MAP     8       /* Mean place to geocentric apparent */
#define AST__SLA_ECLEQ   9       /* Ecliptic to J2000.0 equatorial */
#define AST__SLA_EQECL  10       /* Equatorial J2000.0 to ecliptic */
#define AST__SLA_GALEQ  11       /* Galactic to J2000.0 equatorial */
#define AST__SLA_EQGAL  12       /* J2000.0 equatorial to galactic */
#define AST__SLA_GALSUP 13       /* Galactic to supergalactic */
#define AST__SLA_SUPGAL 14       /* Supergalactic to galactic */
#define AST__HPCEQ      15       /* Helioprojective-Cartesian to J2000.0 equatorial */
#define AST__EQHPC      16       /* J2000.0 equatorial to Helioprojective-Cartesian */
#define AST__HPREQ      17       /* Helioprojective-Radial to J2000.0 equatorial */
#define AST__EQHPR      18       /* J2000.0 equatorial to Helioprojective-Radial */
#define AST__SLA_HFK5Z  19       /* ICRS to FK5 J2000.0, no pm or parallax */
#define AST__SLA_FK5HZ  20       /* FK5 J2000.0 to ICRS, no pm or parallax */
#define AST__HEEQ       21       /* Helio-ecliptic to equatorial */
#define AST__EQHE       22       /* Equatorial to helio-ecliptic */
#define AST__J2000H     23       /* Dynamical J2000 to ICRS */
#define AST__HJ2000     24       /* ICRS to dynamical J2000 */
#define AST__SLA_DH2E   25       /* Horizon to equatorial coordinates */
#define AST__SLA_DE2H   26       /* Equatorial coordinates to horizon */
#define AST__R2H        27       /* RA to hour angle */
#define AST__H2R        28       /* Hour to RA angle */

/* Maximum number of arguments required by an SLALIB conversion. */
#define MAX_SLA_ARGS 4

/* The alphabet (used for generating keywords for arguments). */
#define ALPHABET "abcdefghijklmnopqrstuvwxyz"

/* Angle conversion (PI is from the SLALIB slamac.h file) */
#define PI 3.1415926535897932384626433832795028841971693993751
#define PIBY2 (PI/2.0)
#define D2R (PI/180.0)
#define R2D (180.0/PI)
#define AS2R (PI/648000.0)

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

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "wcsmap.h"              /* Required for AST__DPI */
#include "unitmap.h"             /* Unit (null) Mappings */
#include "slamap.h"              /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );


/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->Eq_Cache = AST__BAD; \
   globals->Ep_Cache = AST__BAD; \

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(SlaMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(SlaMap,Class_Init)
#define class_vtab astGLOBAL(SlaMap,Class_Vtab)
#define eq_cache astGLOBAL(SlaMap,Eq_Cache)
#define ep_cache astGLOBAL(SlaMap,Ep_Cache)
#define amprms_cache astGLOBAL(SlaMap,Amprms_Cache)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* A cache used to store the most recent results from palMappa in order
   to avoid continuously recalculating the same values. */
static double eq_cache = AST__BAD;
static double ep_cache = AST__BAD;
static double amprms_cache[ 21 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSlaMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSlaMap *astSlaMapId_( int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *CvtString( int, const char **, int *, const char *[ MAX_SLA_ARGS ], int * );
static int CvtCode( const char *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void AddSlaCvt( AstSlaMap *, int, const double *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void De2h( double, double, double, double, double *, double *, int * );
static void Dh2e( double, double, double, double, double *, double *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void Earth( double, double[3], int * );
static void SlaAdd( AstSlaMap *, const char *, const double[], int * );
static void SolarPole( double, double[3], int * );
static void Hpcc( double, double[3], double[3][3], double[3], int * );
static void Hprc( double, double[3], double[3][3], double[3], int * );
static void Hgc( double, double[3][3], double[3], int * );
static void Haec( double, double[3][3], double[3], int * );
static void Haqc( double, double[3][3], double[3], int * );
static void Gsec( double, double[3][3], double[3], int * );
static void STPConv( double, int, int, int, double[3], double *[3], int, double[3], double *[3], int * );
static void J2000H( int, int, double *, double *, int * );

static int GetObjSize( AstObject *, int * );

/* Member functions. */
/* ================= */

static void De2h( double ha, double dec, double phi, double diurab,
                  double *az, double *el, int *status ){

/* Not quite like slaDe2h since it converts from apparent (ha,dec) to
   topocentric (az,el). This includes a correction for diurnal
   aberration. The magnitude of the diurnal aberration vector should be
   supplied in parameter "diurab". The extra code is taken from the
   Fortran routine SLA_AOPQK. */

/* Local Variables: */
   double a;
   double cd;
   double ch;
   double cp;
   double f;
   double r;
   double sd;
   double sh;
   double sp;
   double x;
   double xhd;
   double xhdt;
   double y;
   double yhd;
   double yhdt;
   double z;
   double zhd;
   double zhdt;

/* Check inherited status */
   if( !astOK ) return;

/* Pre-compute common values */
   sh = sin( ha );
   ch = cos( ha );
   sd = sin( dec );
   cd = cos( dec );
   sp = sin( phi );
   cp = cos( phi );

/* Components of cartesian (-ha,dec) vector. */
   xhd = ch*cd;
   yhd = -sh*cd;
   zhd = sd;

/* Modify the above vector to apply diurnal aberration. */
   f = ( 1.0 - diurab*yhd );
   xhdt = f*xhd;
   yhdt = f*( yhd + diurab );
   zhdt = f*zhd;

/* Convert to cartesian (az,el). */
   x = -xhdt*sp + zhdt*cp;
   y = yhdt;
   z = xhdt*cp + zhdt*sp;

/* Convert to spherical (az,el). */
   r = sqrt( x*x + y*y );
   if( r == 0.0 ) {
      a = 0.0;
   } else {
      a = atan2( y, x );
   }

   while( a < 0.0 ) a += 2*AST__DPI;

   *az = a;
   *el = atan2( z, r );
}

static void Dh2e( double az, double el, double phi, double diurab, double *ha,
                  double *dec, int *status ){

/* Not quite like slaDh2e since it converts from topocentric (az,el) to
   apparent (ha,dec). This includes a correction for diurnal aberration.
   The magnitude of the diurnal aberration vector should be supplied in
   parameter "diurab". The extra code is taken from the Fortran routine
   SLA_OAPQK. */

/* Local Variables: */
   double ca;
   double ce;
   double cp;
   double f;
   double r;
   double sa;
   double se;
   double sp;
   double x;
   double xmhda;
   double y;
   double ymhda;
   double z;
   double zmhda;

/* Check inherited status */
   if( !astOK ) return;

/* Pre-compute common values. */
   sa = sin( az );
   ca = cos( az );
   se = sin( el );
   ce = cos( el );
   sp = sin( phi );
   cp = cos( phi );

/* Cartesian (az,el) to Cartesian (ha,dec) - note, +ha, not -ha. */
   xmhda = -ca*ce*sp + se*cp;
   ymhda = -sa*ce;
   zmhda = ca*ce*cp + se*sp;

/* Correct this vector for diurnal aberration. Since the above
   expressions produce +ha rather than -ha, we do not negate "diurab"
   before using it. Compare this to SLA_AOPQK. */
   f = ( 1 - diurab*ymhda );
   x = f*xmhda;
   y = f*( ymhda + diurab );
   z = f*zmhda;

/* Cartesian (ha,dec) to spherical (ha,dec). */
   r = sqrt( x*x + y*y );
   if( r == 0.0 ) {
      *ha = 0.0;
   } else {
      *ha = atan2( y, x );
   }
   *dec = atan2( z, r );
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two SlaMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     SlaMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two SlaMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a SlaMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the SlaMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSlaMap *that;
   AstSlaMap *this;
   const char *argdesc[ MAX_SLA_ARGS ];
   const char *comment;
   int i, j;
   int nargs;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two SlaMap structures. */
   this = (AstSlaMap *) this_object;
   that = (AstSlaMap *) that_object;

/* Check the second object is a SlaMap. We know the first is a
   SlaMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsASlaMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two SlaMaps differ, it may still be possible
   for them to be equivalent. First compare the SlaMaps if their Invert
   flags are the same. In this case all the attributes of the two SlaMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {
            if( this->ncvt == that->ncvt ) {
               result = 1;
               for( i = 0; i < this->ncvt && result; i++ ) {
                  if( this->cvttype[ i ] != that->cvttype[ i ] ) {
                     result = 0;
                  } else {
                     CvtString( this->cvttype[ i ], &comment, &nargs,
                                argdesc, status );
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

/* If the Invert flags for the two SlaMaps differ, the attributes of the two
   SlaMaps must be inversely related to each other. */
         } else {

/* In the specific case of a SlaMap, Invert flags must be equal. */
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
*     #include "slamap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     SlaMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied SlaMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the SlaMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSlaMap *this;         /* Pointer to SlaMap structure */
   int result;              /* Result value to return */
   int cvt;                 /* Loop counter for coordinate conversions */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the SlaMap structure. */
   this = (AstSlaMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   for ( cvt = 0; cvt < this->ncvt; cvt++ ) {
      result += astTSizeOf( this->cvtargs[ cvt ] );
      result += astTSizeOf( this->cvtextra[ cvt ] );
   }

   result += astTSizeOf( this->cvtargs );
   result += astTSizeOf( this->cvtextra );
   result += astTSizeOf( this->cvttype );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void AddSlaCvt( AstSlaMap *this, int cvttype, const double *args, int *status ) {
/*
*  Name:
*     AddSlaCvt

*  Purpose:
*     Add a coordinate conversion step to an SlaMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     void AddSlaCvt( AstSlaMap *this, int cvttype, const double *args )

*  Class Membership:
*     SlaMap member function.

*  Description:
*     This function allows one of the sky coordinate conversions
*     supported by SLALIB to be appended to an SlaMap. When an SlaMap
*     is first created (using astSlaMap), it simply performs a unit
*     mapping. By using AddSlaCvt repeatedly, a series of sky
*     coordinate conversions may then be specified which the SlaMap
*     will subsequently perform in sequence. This allows a complex
*     coordinate conversion to be assembled out of the basic building
*     blocks provided by SLALIB. The SlaMap will also perform the
*     inverse coordinate conversion (applying the individual
*     conversion steps in reverse) if required.

*  Parameters:
*     this
*        Pointer to the SlaMap.
*     cvttype
*        A code to identify which sky coordinate conversion is to be
*        appended.  See the "SLALIB Coordinate Conversions" section
*        for details of those available.
*     args
*        Pointer to an array of double containing the argument values
*        required to fully specify the required coordinate
*        conversion. The number of arguments depends on the conversion
*        (see the "SLALIB Coordinate Conversions" section for
*        details). This value is ignored and may be NULL if no
*        arguments are required.

*  Returned Value:
*     void.

*  SLALIB Coordinate Conversions:
*     The following values may be supplied for the "cvttype" parameter
*     in order to specify the sky coordinate conversion to be
*     performed. In each case the value is named after the SLALIB
*     routine that performs the conversion, and the relevant SLALIB
*     documentation should be consulted for full details.
*
*     The argument(s) required to fully specify each conversion are
*     indicated in parentheses after each value. Values for these
*     should be given in the array pointed at by "args". The argument
*     names given match the corresponding SLALIB function arguments
*     (in the Fortran 77 documentation - SUN/67) and their values
*     should be given using the same units, time scale, calendar,
*     etc. as in SLALIB.
*
*        AST__SLA_ADDET( EQ )
*           Add E-terms of aberration.
*        AST__SLA_SUBET( EQ )
*           Subtract E-terms of aberration.
*        AST__SLA_PREBN( BEP0, BEP1 )
*           Apply Bessel-Newcomb pre-IAU 1976 (FK4) precession model.
*        AST__SLA_PREC( EP0, EP1 )
*           Apply IAU 1975 (FK5) precession model.
*        AST__SLA_FK45Z( BEPOCH )
*           Convert FK4 to FK5 (no proper motion or parallax).
*        AST__SLA_FK54Z( BEPOCH )
*           Convert FK5 to FK4 (no proper motion or parallax).
*        AST__SLA_AMP( DATE, EQ )
*           Convert geocentric apparent to mean place.
*        AST__SLA_MAP( EQ, DATE )
*           Convert mean place to geocentric apparent.
*        AST__SLA_ECLEQ( DATE )
*           Convert ecliptic coordinates to J2000.0 equatorial.
*        AST__SLA_EQECL( DATE )
*           Convert equatorial J2000.0 to ecliptic coordinates.
*        AST__SLA_GALEQ( )
*           Convert galactic coordinates to J2000.0 equatorial.
*        AST__SLA_EQGAL( )
*           Convert J2000.0 equatorial to galactic coordinates.
*        AST__SLA_HFK5Z( JEPOCH )
*           Convert ICRS coordinates to J2000.0 equatorial (no proper
*           motion or parallax).
*        AST__SLA_FK5HZ( JEPOCH )
*           Convert J2000.0 equatorial to ICRS coordinates (no proper
*           motion or parallax).
*        AST__SLA_GALSUP( )
*           Convert galactic to supergalactic coordinates.
*        AST__SLA_SUPGAL( )
*           Convert supergalactic coordinates to galactic.
*        AST__HPCEQ( DATE, OBSX, OBSY, OBSZ )
*           Convert Helioprojective-Cartesian coordinates to J2000.0
*           equatorial. This is not a native SLALIB conversion, but is
*           implemented by functions within this module. The DATE argument
*           is the MJD defining the HPC coordinate system. The OBSX, OBSY
*           and OBSZ arguments are the AST__HAEC coordinates of the observer.
*        AST__EQHPC( DATE, OBSX, OBSY, OBSZ )
*           Convert J2000.0 equatorial coordinates to Helioprojective-Cartesian.
*        AST__HPREQ( DATE, OBSX, OBSY, OBSZ )
*           Convert Helioprojective-Radial coordinates to J2000.0 equatorial.
*        AST__EQHPR( DATE, OBSX, OBSY, OBSZ )
*           Convert J2000.0 equatorial coordinates to Helioprojective-Radial.
*        AST__HEEQ( DATE )
*           Convert helio-ecliptic to ecliptic coordinates.
*        AST__EQHE( DATE )
*           Convert ecliptic to helio-ecliptic coordinates.
*        AST__J2000H( )
*           Convert dynamical J2000 to ICRS.
*        AST__HJ2000( )
*           Convert ICRS to dynamical J2000.
*        AST__SLA_DH2E( LAT, DIURAB )
*           Convert horizon to equatorial coordinates
*        AST__SLA_DE2H( LAT, DIURAB )
*           Convert equatorial to horizon coordinates
*        AST__R2H( LAST )
*           Convert RA to Hour Angle.
*        AST__H2R( LAST )
*           Convert Hour Angle to RA.

*  Notes:
*     - The specified conversion is appended only if the SlaMap's
*     Invert attribute is zero. If it is non-zero, this function
*     effectively prefixes the inverse of the conversion specified
*     instead.
*     - Sky coordinate values are in radians (as for SLALIB) and all
*     conversions are performed using double arithmetic.
*/

/* Local Variables: */
   const char *argdesc[ MAX_SLA_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *cvt_string;       /* Pointer to conversion type string */
   int nargs;                    /* Number of arguments */
   int ncvt;                     /* Number of coordinate conversions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the coordinate conversion type and obtain the number of
   required arguments. */
   cvt_string = CvtString( cvttype, &comment, &nargs, argdesc, status );

/* If the sky coordinate conversion type was not valid, then report an
   error. */
   if ( astOK && !cvt_string ) {
      astError( AST__SLAIN, "AddSlaCvt(%s): Invalid SLALIB sky coordinate "
                "conversion type (%d).", status, astGetClass( this ),
                (int) cvttype );
   }

/* Note the number of coordinate conversions already stored in the SlaMap. */
   if ( astOK ) {
      ncvt = this->ncvt;

/* Extend the array of conversion types and the array of pointers to
   their argument lists to accommodate the new one. */
      this->cvttype = (int *) astGrow( this->cvttype, ncvt + 1,
                                       sizeof( int ) );
      this->cvtargs = (double **) astGrow( this->cvtargs, ncvt + 1,
                                           sizeof( double * ) );
      this->cvtextra = (double **) astGrow( this->cvtextra, ncvt + 1,
                                           sizeof( double * ) );

/* If OK, allocate memory and store a copy of the argument list,
   putting a pointer to the copy into the SlaMap. */
      if ( astOK ) {
         this->cvtargs[ ncvt ] = astStore( NULL, args,
                                           sizeof( double ) * (size_t) nargs );
         this->cvtextra[ ncvt ] = NULL;
      }

/* Store the conversion type and increment the conversion count. */
      if ( astOK ) {
         this->cvttype[ ncvt ] = cvttype;
         this->ncvt++;
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
*     #include "slamap.h"
*     int CvtCode( const char *cvt_string, int *status )

*  Class Membership:
*     SlaMap member function.

*  Description:
*     This function accepts a string used to repersent one of the
*     SLALIB sky coordinate conversions and converts it into a code
*     value for internal use.

*  Parameters:
*     cvt_string
*        Pointer to a constant null-terminated string representing a
*        sky coordinate conversion. This is case sensitive and should
*        contain no unnecessary white space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The equivalent conversion code. If the string was not
*     recognised, the code AST__SLA_NULL is returned, without error.

*  Notes:
*     - A value of AST__SLA_NULL will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   int result;                   /* Result value to return */

/* Initialise. */
   result = AST__SLA_NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Test the string against each recognised value in turn and assign
   the result. */
   if ( astChrMatch( cvt_string, "ADDET" ) ) {
      result = AST__SLA_ADDET;

   } else if ( astChrMatch( cvt_string, "SUBET" ) ) {
      result = AST__SLA_SUBET;

   } else if ( astChrMatch( cvt_string, "PREBN" ) ) {
      result = AST__SLA_PREBN;

   } else if ( astChrMatch( cvt_string, "PREC" ) ) {
      result = AST__SLA_PREC;

   } else if ( astChrMatch( cvt_string, "FK45Z" ) ) {
      result = AST__SLA_FK45Z;

   } else if ( astChrMatch( cvt_string, "FK54Z" ) ) {
      result = AST__SLA_FK54Z;

   } else if ( astChrMatch( cvt_string, "AMP" ) ) {
      result = AST__SLA_AMP;

   } else if ( astChrMatch( cvt_string, "MAP" ) ) {
      result = AST__SLA_MAP;

   } else if ( astChrMatch( cvt_string, "ECLEQ" ) ) {
      result = AST__SLA_ECLEQ;

   } else if ( astChrMatch( cvt_string, "EQECL" ) ) {
      result = AST__SLA_EQECL;

   } else if ( astChrMatch( cvt_string, "GALEQ" ) ) {
      result = AST__SLA_GALEQ;

   } else if ( astChrMatch( cvt_string, "EQGAL" ) ) {
      result = AST__SLA_EQGAL;

   } else if ( astChrMatch( cvt_string, "FK5HZ" ) ) {
      result = AST__SLA_FK5HZ;

   } else if ( astChrMatch( cvt_string, "HFK5Z" ) ) {
      result = AST__SLA_HFK5Z;

   } else if ( astChrMatch( cvt_string, "GALSUP" ) ) {
      result = AST__SLA_GALSUP;

   } else if ( astChrMatch( cvt_string, "SUPGAL" ) ) {
      result = AST__SLA_SUPGAL;

   } else if ( astChrMatch( cvt_string, "HPCEQ" ) ) {
      result = AST__HPCEQ;

   } else if ( astChrMatch( cvt_string, "EQHPC" ) ) {
      result = AST__EQHPC;

   } else if ( astChrMatch( cvt_string, "HPREQ" ) ) {
      result = AST__HPREQ;

   } else if ( astChrMatch( cvt_string, "EQHPR" ) ) {
      result = AST__EQHPR;

   } else if ( astChrMatch( cvt_string, "HEEQ" ) ) {
      result = AST__HEEQ;

   } else if ( astChrMatch( cvt_string, "EQHE" ) ) {
      result = AST__EQHE;

   } else if ( astChrMatch( cvt_string, "J2000H" ) ) {
      result = AST__J2000H;

   } else if ( astChrMatch( cvt_string, "HJ2000" ) ) {
      result = AST__HJ2000;

   } else if ( astChrMatch( cvt_string, "H2E" ) ) {
      result = AST__SLA_DH2E;

   } else if ( astChrMatch( cvt_string, "E2H" ) ) {
      result = AST__SLA_DE2H;

   } else if ( astChrMatch( cvt_string, "R2H" ) ) {
      result = AST__R2H;

   } else if ( astChrMatch( cvt_string, "H2R" ) ) {
      result = AST__H2R;

   }

/* Return the result. */
   return result;
}

static const char *CvtString( int cvt_code, const char **comment,
                              int *nargs, const char *arg[ MAX_SLA_ARGS ], int *status ) {
/*
*  Name:
*     CvtString

*  Purpose:
*     Convert a conversion type from a code value to a string representation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     const char *CvtString( int cvt_code, const char **comment,
*                            int *nargs, const char *arg[ MAX_SLA_ARGS ], int *status )

*  Class Membership:
*     SlaMap member function.

*  Description:
*     This function accepts a code value used to represent one of the
*     SLALIB sky coordinate conversions and converts it into an
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
*        required in order to perform the conversion (may be zero).
*     arg
*        An array in which to return a pointer to a constant
*        null-terminated string for each argument (above) containing a
*        description of what each argument represents.
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
   const char *result;           /* Result pointer to return */

/* Initialise the returned values. */
   *comment = NULL;
   *nargs = 0;
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Test for each valid code value in turn and assign the appropriate
   return values. */
   switch ( cvt_code ) {

   case AST__SLA_ADDET:
      result = "ADDET";
      *comment = "Add E-terms of aberration";
      *nargs = 1;
      arg[ 0 ] = "Besselian epoch of mean equinox (FK4)";
      break;

   case AST__SLA_SUBET:
      result = "SUBET";
      *comment = "Subtract E-terms of aberration";
      *nargs = 1;
      arg[ 0 ] = "Besselian epoch of mean equinox (FK4)";
      break;

   case AST__SLA_PREBN:
      result = "PREBN";
      *comment = "Apply Bessel-Newcomb (FK4) precession";
      *nargs = 2;
      arg[ 0 ] = "From Besselian epoch";
      arg[ 1 ] = "To Besselian epoch";
      break;

   case AST__SLA_PREC:
      result = "PREC";
      *comment = "Apply IAU 1975 (FK5) precession";
      *nargs = 2;
      arg[ 0 ] = "From Julian epoch";
      arg[ 1 ] = "To Julian epoch";
      break;

   case AST__SLA_FK45Z:
      result = "FK45Z";
      *comment = "FK4 to FK5 J2000.0 (no PM or parallax)";
      arg[ 0 ] = "Besselian epoch of FK4 coordinates";
      *nargs = 1;
      break;

   case AST__SLA_FK54Z:
      result = "FK54Z";
      *comment = "FK5 J2000.0 to FK4 (no PM or parallax)";
      *nargs = 1;
      arg[ 0 ] = "Besselian epoch of FK4 system";
      break;

   case AST__SLA_AMP:
      result = "AMP";
      *comment = "Geocentric apparent to mean place (FK5)";
      *nargs = 2;
      arg[ 0 ] = "TDB of apparent place (as MJD)";
      arg[ 1 ] = "Julian epoch of mean equinox (FK5)";
      break;

   case AST__SLA_MAP:
      result = "MAP";
      *comment = "Mean place (FK5) to geocentric apparent";
      *nargs = 2;
      arg[ 0 ] = "Julian epoch of mean equinox (FK5)";
      arg[ 1 ] = "TDB of apparent place (as MJD)";
      break;

   case AST__SLA_ECLEQ:
      result = "ECLEQ";
      *comment = "Ecliptic (IAU 1980) to J2000.0 equatorial (FK5)";
      *nargs = 1;
      arg[ 0 ] = "TDB of mean ecliptic (as MJD)";
      break;

   case AST__SLA_EQECL:
      result = "EQECL";
      *comment = "Equatorial J2000.0 (FK5) to ecliptic (IAU 1980)";
      *nargs = 1;
      arg[ 0 ] = "TDB of mean ecliptic (as MJD)";
      break;

   case AST__SLA_GALEQ:
      result = "GALEQ";
      *comment = "Galactic (IAU 1958) to J2000.0 equatorial (FK5)";
      *nargs = 0;
      break;

   case AST__SLA_EQGAL:
      result = "EQGAL";
      *comment = "J2000.0 equatorial (FK5) to galactic (IAU 1958)";
      *nargs = 0;
      break;

   case AST__SLA_FK5HZ:
      result = "FK5HZ";
      *comment = "J2000.0 FK5 to ICRS (no PM or parallax)";
      arg[ 0 ] = "Julian epoch of FK5 coordinates";
      *nargs = 1;
      break;

   case AST__SLA_HFK5Z:
      result = "HFK5Z";
      *comment = "ICRS to J2000.0 FK5 (no PM or parallax)";
      arg[ 0 ] = "Julian epoch of FK5 coordinates";
      *nargs = 1;
      break;

   case AST__SLA_GALSUP:
      result = "GALSUP";
      *comment = "Galactic (IAU 1958) to supergalactic";
      *nargs = 0;
      break;

   case AST__SLA_SUPGAL:
      result = "SUPGAL";
      *comment = "Supergalactic to galactic (IAU 1958)";
      *nargs = 0;
      break;

   case AST__HPCEQ:
      result = "HPCEQ";
      *comment = "Helioprojective-Cartesian to J2000.0 equatorial (FK5)";
      *nargs = 4;
      arg[ 0 ] = "Modified Julian Date of observation";
      arg[ 1 ] = "Heliocentric-Aries-Ecliptic X value at observer";
      arg[ 2 ] = "Heliocentric-Aries-Ecliptic Y value at observer";
      arg[ 3 ] = "Heliocentric-Aries-Ecliptic Z value at observer";
      break;

   case AST__EQHPC:
      result = "EQHPC";
      *comment = "J2000.0 equatorial (FK5) to Helioprojective-Cartesian";
      *nargs = 4;
      arg[ 0 ] = "Modified Julian Date of observation";
      arg[ 1 ] = "Heliocentric-Aries-Ecliptic X value at observer";
      arg[ 2 ] = "Heliocentric-Aries-Ecliptic Y value at observer";
      arg[ 3 ] = "Heliocentric-Aries-Ecliptic Z value at observer";
      break;

   case AST__HPREQ:
      result = "HPREQ";
      *comment = "Helioprojective-Radial to J2000.0 equatorial (FK5)";
      *nargs = 4;
      arg[ 0 ] = "Modified Julian Date of observation";
      arg[ 1 ] = "Heliocentric-Aries-Ecliptic X value at observer";
      arg[ 2 ] = "Heliocentric-Aries-Ecliptic Y value at observer";
      arg[ 3 ] = "Heliocentric-Aries-Ecliptic Z value at observer";
      break;

   case AST__EQHPR:
      result = "EQHPR";
      *comment = "J2000.0 equatorial (FK5) to Helioprojective-Radial";
      *nargs = 4;
      arg[ 0 ] = "Modified Julian Date of observation";
      arg[ 1 ] = "Heliocentric-Aries-Ecliptic X value at observer";
      arg[ 2 ] = "Heliocentric-Aries-Ecliptic Y value at observer";
      arg[ 3 ] = "Heliocentric-Aries-Ecliptic Z value at observer";
      break;

   case AST__HEEQ:
      result = "HEEQ";
      *comment = "Helio-ecliptic to equatorial";
      *nargs = 1;
      arg[ 0 ] = "Modified Julian Date of observation";
      break;

   case AST__EQHE:
      result = "EQHE";
      *comment = "Equatorial to helio-ecliptic";
      *nargs = 1;
      arg[ 0 ] = "Modified Julian Date of observation";
      break;

   case AST__J2000H:
      result = "J2000H";
      *comment = "J2000 equatorial (dynamical) to ICRS";
      *nargs = 0;
      break;

   case AST__HJ2000:
      result = "HJ2000";
      *comment = "ICRS to J2000 equatorial (dynamical)";
      *nargs = 0;
      break;

   case AST__SLA_DH2E:
      result = "H2E";
      *comment = "Horizon to equatorial";
      *nargs = 2;
      arg[ 0 ] = "Geodetic latitude of observer";
      arg[ 1 ] = "Magnitude of diurnal aberration vector";
      break;

   case AST__SLA_DE2H:
      result = "E2H";
      *comment = "Equatorial to horizon";
      *nargs = 2;
      arg[ 0 ] = "Geodetic latitude of observer";
      arg[ 1 ] = "Magnitude of diurnal aberration vector";
      break;

   case AST__R2H:
      result = "R2H";
      *comment = "RA to Hour Angle";
      *nargs = 1;
      arg[ 0 ] = "Local apparent sidereal time (radians)";
      break;

   case AST__H2R:
      result = "H2R";
      *comment = "Hour Angle to RA";
      *nargs = 1;
      arg[ 0 ] = "Local apparent sidereal time (radians)";
      break;

   }

/* Return the result. */
   return result;
}

static void Earth( double mjd, double earth[3], int *status ) {
/*
*+
*  Name:
*     Earth

*  Purpose:
*     Returns the AST__HAEC position of the earth at the specified time.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Earth( double mjd, double earth[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns the AST__HAEC position of the earth at the
*     specified time. See astSTPConv for a description of the AST__HAEC
*     coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date.
*     earth
*        The AST__HAEC position of the earth at the given date.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   double dpb[3];     /* Earth position (barycentric) */
   double dph[3];     /* Earth position (heliocentric) */
   double dvb[3];     /* Earth velocity (barycentric) */
   double dvh[3];     /* Earth velocity (heliocentric, AST__HAQC) */
   double ecmat[3][3];/* Equatorial to ecliptic matrix */
   int i;             /* Loop count */

/* Initialize. */
   for( i = 0; i < 3; i++ ) earth[ i ] = 0.0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the position of the earth at the given date in the AST__HAQC coord
   system (dph). */
   palEvp( mjd, 2000.0, dvb, dpb, dvh, dph );

/* Now rotate the earths position vector into AST__HAEC coords. */
   palEcmat( palEpj2d( 2000.0 ), ecmat );
   palDmxv( ecmat, dph, earth );

/* Convert from AU to metres. */
   earth[0] *= AST__AU;
   earth[1] *= AST__AU;
   earth[2] *= AST__AU;

}

static void Hgc( double mjd, double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Hgc

*  Purpose:
*     Returns matrix and offset for converting AST__HGC positions to AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Hgc( double mjd, double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__HGC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__HGC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     mat
*        Matrix which rotates from AST__HGC to AST__HAEC.
*     offset
*        The origin of the AST__HGC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   double earth[3];   /* Earth position (heliocentric, AST__HAEC) */
   double len;        /* Vector length */
   double xhg[3];     /* Unix X vector of AST__HGC system in AST__HAEC */
   double yhg[3];     /* Unix Y vector of AST__HGC system in AST__HAEC */
   double ytemp[3];   /* Un-normalized Y vector */
   double zhg[3];     /* Unix Z vector of AST__HGC system in AST__HAEC */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Initialize. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[ i ] = 0.0;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a unit vector parallel to the solar north pole at the given date.
   This vector is expressed in AST__HAEC coords. This is the Z axis of the
   AST__HGC system. */
   SolarPole( mjd, zhg, status );

/* Get the position of the earth at the given date in the AST__HAEC coord
   system. */
   Earth( mjd, earth, status );

/* The HG Y axis is perpendicular to both the polar axis and the
   sun-earth line. Obtain a Y vector by taking the cross product of the
   two vectors, and then normalize it into a unit vector. */
   palDvxv( zhg, earth, ytemp );
   palDvn( ytemp, yhg, &len );

/* The HG X axis is perpendicular to both Z and Y, */
   palDvxv( yhg, zhg, xhg );

/* The HG X, Y and Z unit vectors form the columns of the required matrix.
   The origins of the two systems are co-incident, so return the zero offset
   vector initialised earlier. */
   for( i = 0; i < 3; i++ ) {
      mat[ i ][ 0 ] = xhg[ i ];
      mat[ i ][ 1 ] = yhg[ i ];
      mat[ i ][ 2 ] = zhg[ i ];
   }

}

static void Gsec( double mjd, double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Gsec

*  Purpose:
*     Returns matrix and offset for converting AST__GSEC positions to AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Gsec( double mjd, double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__GSEC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__GSEC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     mat
*        Matrix which rotates from AST__GSEC to AST__HAEC.
*     offset
*        The origin of the AST__GSEC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   double earth[3];   /* Earth position (heliocentric, AST__HAEC) */
   double pole[3];    /* Solar pole (AST__HAEC) */
   double len;        /* Vector length */
   double xgs[3];     /* Unix X vector of AST__GSEC system in AST__HAEC */
   double ygs[3];     /* Unix Y vector of AST__GSEC system in AST__HAEC */
   double ytemp[3];   /* Un-normalized Y vector */
   double zgs[3];     /* Unix Z vector of AST__GSEC system in AST__HAEC */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Initialize. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[ i ] = 0.0;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the position of the earth at the given date in the AST__HAEC coord
   system. */
   Earth( mjd, earth, status );

/* We need to find unit vectors parallel to the GSEC (X,Y,Z) axes, expressed
   in terms of the AST__HAEC (X,Y,Z) axes. The GSEC X axis starts at the
   earth and passes through the centre of the sun. This is just the
   normalized opposite of the earth's position vector. */
   palDvn( earth, xgs, &len );
   xgs[0] *= -1.0;
   xgs[1] *= -1.0;
   xgs[2] *= -1.0;

/* The GSEC Y axis is perpendicular to both the X axis and the ecliptic north
   pole vector. So find the ecliptic north pole vector in AST__HAEC coords. */
   pole[ 0 ] = 0.0;
   pole[ 1 ] = 0.0;
   pole[ 2 ] = 1.0;

/* Find the GSEC Y axis by taking the vector product of the X axis and
   the ecliptic north pole vector, and then normalize it into a unit
   vector. */
   palDvxv( pole, xgs, ytemp );
   palDvn( ytemp, ygs, &len );

/* The GSEC Z axis is perpendicular to both X and Y axis, and forms a
   right-handed system. The resulting vector will be of unit length
   since the x and y vectors are both of unit length, and are
   perpendicular to each other. It therefore does not need to be
   normalized.*/
   palDvxv( xgs, ygs, zgs );

/* The GSEC X, Y and Z unit vectors form the columns of the required matrix. */
   for( i = 0; i < 3; i++ ) {
      mat[ i ][ 0 ] = xgs[ i ];
      mat[ i ][ 1 ] = ygs[ i ];
      mat[ i ][ 2 ] = zgs[ i ];
      offset[i] = earth[ i ];
   }

}

static void Haec( double mjd, double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Haec

*  Purpose:
*     Returns matrix and offset for converting AST__HAEC positions to AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Haec( double mjd, double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__HAEC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__HAEC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     mat
*        Matrix which rotates from AST__HAEC to AST__HAEC.
*     offset
*        The origin of the AST__HAEC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Return an identity matrix and a zero offset vector. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[ i ] = 0.0;
   }

}

static void Haqc( double mjd, double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Haqc

*  Purpose:
*     Returns matrix and offset for converting AST__HAQC positions to AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Haqc( double mjd, double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__HAQC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__HAQC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     mat
*        Matrix which rotates from AST__HAQC to AST__HAEC.
*     offset
*        The origin of the AST__HAQC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Initialise an identity matrix and a zero offset vector. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[ i ] = 0.0;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* Return the required matrix. */
   palEcmat( palEpj2d( 2000.0 ), mat );
   return;
}

static void Hpcc( double mjd, double obs[3], double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Hpcc

*  Purpose:
*     Returns matrix and offset for converting AST__HPCC positions to
*     AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Hpcc( double mjd, double obs[3], double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__HPCC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__HPCC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     obs
*        The observers position, in AST__HAEC, or NULL if the observer is
*        at the centre of the earth.
*     mat
*        Matrix which rotates from AST__HPCC to AST__HAEC.
*     offset
*        The origin of the AST__HPCC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   double earth[3];   /* Earth position (heliocentric, AST__HAEC) */
   double pole[3];    /* Solar pole vector (AST__HAEC) */
   double len;        /* Vector length */
   double xhpc[3];    /* Unix X vector of AST__HPCC system in AST__HAEC */
   double yhpc[3];    /* Unix Y vector of AST__HPCC system in AST__HAEC */
   double ytemp[3];   /* Un-normalized Y vector */
   double zhpc[3];    /* Unix Z vector of AST__HPCC system in AST__HAEC */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Initialize. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[i] = 0.0;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* If no observers position was supplied, use the position of the earth
   at the specified date in AST__HAEC coords. */
   if( !obs ) {
      Earth( mjd, earth, status );
      obs = earth;
   }

/* We need to find unit vectors parallel to the HPCC (X,Y,Z) axes, expressed
   in terms of the AST__HAEC (X,Y,Z) axes. The HPCC X axis starts at the
   observer and passes through the centre of the sun. This is just the
   normalized opposite of the supplied observer's position vector. */
   palDvn( obs, xhpc, &len );
   xhpc[0] *= -1.0;
   xhpc[1] *= -1.0;
   xhpc[2] *= -1.0;

/* The HPC Y axis is perpendicular to both the X axis and the solar north
   pole vector. So find the solar north pole vector in AST__HAEC coords. */
   SolarPole( mjd, pole, status );

/* Find the HPC Y axis by taking the vector product of the X axis and
   the solar north pole vector, and then normalize it into a unit vector.
   Note, HPC (X,Y,Z) axes form a left-handed system! */
   palDvxv( xhpc, pole, ytemp );
   palDvn( ytemp, yhpc, &len );

/* The HPC Z axis is perpendicular to both X and Y axis, and forms a
   left-handed system. The resulting vector will be of unit length
   since the x and y vectors are both of unit length, and are
   perpendicular to each other. It therefore does not need to be
   normalized.*/
   palDvxv( yhpc, xhpc, zhpc );

/* The HPC X, Y and Z unit vectors form the columns of the required matrix. */
   for( i = 0; i < 3; i++ ) {
      mat[ i ][ 0 ] = xhpc[ i ];
      mat[ i ][ 1 ] = yhpc[ i ];
      mat[ i ][ 2 ] = zhpc[ i ];
      offset[i] = obs[ i ];
   }

}

static void Hprc( double mjd, double obs[3], double mat[3][3], double offset[3], int *status ) {
/*
*+
*  Name:
*     Hprc

*  Purpose:
*     Returns matrix and offset for converting AST__HPRC positions to
*     AST__HAEC.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void Hprc( double mjd, double obs[3], double mat[3][3], double offset[3], int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function returns a 3x3 matrix which rotates direction vectors
*     given in the AST__HPRC system to the AST__HAEC system at the
*     specified date. It also returns the position of the origin of the
*     AST__HPRC system as an AST__HAEC position. See astSTPConv for a
*     description of these coordinate systems.

*  Parameters:
*     mjd
*        Modified Julian date defining the coordinate systems.
*     obs
*        The observers position, in AST__HAEC, or NULL if the observer is
*        at the centre of the earth.
*     mat
*        Matrix which rotates from AST__HPRC to AST__HAEC.
*     offset
*        The origin of the AST__HPRC system within the AST__HAEC system.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   double pole[3];    /* Solar pole (AST__HAEC) */
   double earth[3];   /* Earth position (heliocentric, AST__HAEC) */
   double len;        /* Vector length */
   double xhpr[3];    /* Unix X vector of AST__HPRC system in AST__HAEC */
   double yhpr[3];    /* Unix Y vector of AST__HPRC system in AST__HAEC */
   double ytemp[3];   /* Un-normalized Y vector */
   double zhpr[3];    /* Unix Z vector of AST__HPRC system in AST__HAEC */
   int i;             /* Loop count */
   int j;             /* Loop count */

/* Initialize. */
   for( i = 0; i < 3; i++ ) {
      for( j = 0; j < 3; j++ ) {
         mat[i][j] = (i==j)?1.0:0.0;
      }
      offset[i] = 0.0;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* If no observers position was supplied, use the position of the earth
   at the specified date in AST__HAEC coords. */
   if( !obs ) {
      Earth( mjd, earth, status );
      obs = earth;
   }

/* We need to find unit vectors parallel to the HPRC (X,Y,Z) axes, expressed
   in terms of the AST__HAEC (X,Y,Z) axes. The HPRC Z axis starts at the
   observer and passes through the centre of the sun. This is just the
   normalized opposite of the supplied observer's position vector. */
   palDvn( obs, zhpr, &len );
   zhpr[0] *= -1.0;
   zhpr[1] *= -1.0;
   zhpr[2] *= -1.0;

/* The HPR Y axis is perpendicular to both the Z axis and the solar north
   pole vector. So find the solar north pole vector in AST__HAEC coords. */
   SolarPole( mjd, pole, status );

/* Find the HPR Y axis by taking the vector product of the Z axis and
   the solar north pole vector, and then normalize it into a unit vector.
   Note, HPR (X,Y,Z) axes form a left-handed system! */
   palDvxv( pole, zhpr, ytemp );
   palDvn( ytemp, yhpr, &len );

/* The HPRC X axis is perpendicular to both Y and Z axis, and forms a
   left-handed system. The resulting vector will be of unit length
   since the y and z vectors are both of unit length, and are
   perpendicular to each other. It therefore does not need to be
   normalized.*/
   palDvxv( zhpr, yhpr, xhpr );

/* The HPRC X, Y and Z unit vectors form the columns of the required matrix. */
   for( i = 0; i < 3; i++ ) {
      mat[ i ][ 0 ] = xhpr[ i ];
      mat[ i ][ 1 ] = yhpr[ i ];
      mat[ i ][ 2 ] = zhpr[ i ];
      offset[ i ] = obs[ i ];
   }
}

static void J2000H( int forward, int npoint, double *alpha, double *delta, int *status ){
/*
*  Name:
*     J2000H

*  Purpose:
*     Convert dynamical J2000 equatorial coords to ICRS.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void J2000H( int forward, int npoint, double *alpha, double *delta, int *status )

*  Class Membership:
*     SlaMap method.

*  Description:
*     This function converts the supplied dynamical J2000 equatorial coords
*     to ICRS (or vice-versa).

*  Parameters:
*     forward
*        Do forward transformation?
*     npoint
*        Number of points to transform.
*     alpha
*        Pointer to longitude values.
*     delta
*        Pointer to latitude values.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int i;                  /* Loop count */
   double rmat[3][3];      /* J2000 -> ICRS rotation matrix */
   double v1[3];           /* J2000 vector */
   double v2[3];           /* ICRS vector */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the J2000 to ICRS rotation matrix (supplied by P.T. Wallace) */
   palDeuler( "XYZ", -0.0068192*AS2R, 0.0166172*AS2R, 0.0146000*AS2R,
              rmat );

/* Loop round all points. */
   for( i = 0; i < npoint; i++ ) {

/* Convert from (alpha,delta) to 3-vector */
      palDcs2c( alpha[ i ], delta[ i ], v1 );

/* Rotate the 3-vector */
      if( forward ) {
         palDmxv( rmat, v1, v2 );
      } else {
         palDimxv( rmat, v1, v2 );
      }

/* Convert from 3-vector to (alpha,delta) */
      palDcc2s( v2, alpha + i, delta + i );
   }
}

void astSTPConv1_( double mjd, int in_sys, double in_obs[3], double in[3],
                   int out_sys, double out_obs[3], double out[3], int *status ){
/*
*+
*  Name:
*     astSTPConv1

*  Purpose:
*     Converts a 3D solar system position between specified STP coordinate
*     systems.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "slamap.h"
*     void astSTPConv1( double mjd, int in_sys, double in_obs[3],
*                       double in[3], int out_sys, double out_obs[3],
*                       double out[3] )

*  Class Membership:
*     Frame method.

*  Description:
*     This function converts a single 3D solar-system position from the
*     specified input coordinate system to the specified output coordinate
*     system. See astSTPConv for a list of supported coordinate systems.

*  Parameters:
*     mjd
*        The Modified Julian Date to which the coordinate systems refer.
*     in_sys
*        The coordinate system in which the input positions are supplied.
*     in_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the input system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     in
*        A 3-element array holding the input position.
*     out_sys
*        The coordinate system in which the input positions are supplied.
*     out_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the output system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     out
*        A 3-element array holding the output position.

*  Notes:
*     - The "in" and "out" arrays may safely point to the same memory.
*     - Output longitude values are always in the range 0 - 2.PI.

*-
*/

/* Local Variables: */
   double *ins[ 3 ];      /* The input position */
   double *outs[ 3 ];     /* The output position */

/* Store pointers to the supplied arrays suitable for passing to STPConv. */
   ins[ 0 ] = in;
   ins[ 1 ] = in + 1;
   ins[ 2 ] = in + 2;
   outs[ 0 ] = out;
   outs[ 1 ] = out + 1;
   outs[ 2 ] = out + 2;

/* Convert the position. */
   STPConv( mjd, 0, 1, in_sys, in_obs, ins, out_sys, out_obs, outs, status );

}

void astSTPConv_( double mjd, int n, int in_sys, double in_obs[3],
                  double *in[3], int out_sys, double out_obs[3],
                  double *out[3], int *status ){
/*
*+
*  Name:
*     astSTPConv

*  Purpose:
*     Converts a set of 3D solar system positions between specified STP
*     coordinate systems.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "slamap.h"
*     void astSTPConv( double mjd, int n, int in_sys, double in_obs[3],
*                      double *in[3], int out_sys, double out_obs[3],
*                      double *out[3] )

*  Class Membership:
*     Frame method.

*  Description:
*     This function converts a set of 3D solar-system positions from
*     the specified input coordinate system to the specified output
*     coordinate system.

*  Parameters:
*     mjd
*        The Modified Julian Date to which the coordinate systems refer.
*     in_sys
*        The coordinate system in which the input positions are supplied
*        (see below).
*     in_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the input system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     in
*        A 3-element array holding the input positions. Each of the 3
*        elements should point to an array of "n" axis values. For spherical
*        input systems, in[3] can be supplied as NULL, in which case a
*        constant value of 1 AU will be used.
*     out_sys
*        The coordinate system in which the input positions are supplied
*        (see below).
*     out_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the output system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     out
*        A 3-element array holding the output positions. Each of the 3
*        elements should point to an array of "n" axis values. If in[3] is
*        NULL, no values will be assigned to out[3].

*  Notes:
*     - The "in" and "out" arrays may safely point to the same memory.
*     - Output longitude values are always in the range 0 - 2.PI.

*  Supported Coordinate Systems:
*     Coordinate systems are either spherical or Cartesian, and are right
*     handed (unless otherwise indicated). Spherical systems use axis 0 for
*     longitude, axis 1 for latitude, and axis 2 for radius. Cartesian systems
*     use 3 mutually perpendicular axes; X is axis 0 and points towards the
*     intersection of the equator and the zero longitude meridian of the
*     corresponding spherical system, Y is axis 1 and points towards longitude
*     of +90 degrees, Z is axis 2 and points twowards the north pole. All
*     angles are in radians and all distances are in metres. The following
*     systems are supported:
*
*     - AST__HAE: Heliocentric-aries-ecliptic spherical coordinates. Centred
*     at the centre of the sun. The north pole points towards the J2000
*     ecliptic north pole, and meridian of zero longitude includes the
*     J2000 equinox.
*
*     - AST__HAEC: Heliocentric-aries-ecliptic cartesian coordinates. Origin
*     at the centre of the sun. The Z axis points towards the J2000 ecliptic
*     north pole, and the X axis points towards the J2000 equinox.
*
*     - AST__HAQ: Heliocentric-aries-equatorial spherical coordinates. Centred
*     at the centre of the sun. The north pole points towards the FK5 J2000
*     equatorial north pole, and meridian of zero longitude includes the
*     FK5 J2000 equinox.
*
*     - AST__HAQC: Heliocentric-aries-equatorial cartesian coordinates. Origin
*     at the centre of the sun. The Z axis points towards the FK5 J2000
*     equatorial north pole, and the X axis points towards the FK5 J2000
*     equinox.
*
*     - AST__HG: Heliographic spherical coordinates. Centred at the centre of
*     the sun. North pole points towards the solar north pole at the given
*     date. The meridian of zero longitude includes the sun-earth line at
*     the given date.
*
*     - AST__HGC: Heliographic cartesian coordinates. Origin at the centre of
*     the sun. The Z axis points towards the solar north pole at the given
*     date. The X axis is in the plane spanned by the Z axis, and the
*     sun-earth line at the given date.
*
*     - AST__HPC: Helioprojective-cartesian spherical coordinates. A
*     left-handed system (that is, longitude increases westwards), centred
*     at the specified observer position. The intersection of the
*     zero-longitude meridian and the equator coincides with the centre of
*     the sun as seen from the observers position. The zero longitude
*     meridian includes the solar north pole at the specified date.
*
*     - AST__HPCC: Helioprojective-cartesian cartesian coordinates. A
*     left-handed system with origin at the specified observer position. The
*     X axis points towards the centre of the sun as seen from the observers
*     position. The X-Z plane includes the solar north pole at the specified
*     date.
*
*     - AST__HPR: Helioprojective-radial spherical coordinates. A left-handed
*     system (that is, longitude increases westwards), centred at the
*     specified observer position. The north pole points towards the centre
*     of the sun as seen from the observers position. The zero longitude
*     meridian includes the solar north pole at the specified date.
*
*     - AST__HPRC: Helioprojective-radial cartesian coordinates. A left-handed
*     system with origin at the specified observer position. The Z axis points
*     towards the centre of the sun as seen from the observers position. The
*     X-Z plane includes the solar north pole at the specified date.
*
*     - AST__GSE: Geocentric-solar-ecliptic spherical coordinates. Centred at
*     the centre of the earth at the given date. The north pole points towards
*     the  J2000 ecliptic north pole, and the meridian of zero longitude
*     includes the Sun.
*
*     - AST__GSEC: Geocentric-solar-ecliptic cartesian coordinates. Origin at
*     the centre of the earth at the given date. The X axis points towards the
*     centre of sun, and the X-Z plane contains the J2000 ecliptic north
*     pole. Since the earth may not be exactly in the mean ecliptic of
*     J2000, the Z axis will not in general correspond exactly to the
*     ecliptic north pole.
*-
*/
   STPConv( mjd, 0, n, in_sys, in_obs, in, out_sys, out_obs, out, status );
}

static void STPConv( double mjd, int ignore_origins, int n, int in_sys,
                     double in_obs[3], double *in[3], int out_sys,
                     double out_obs[3], double *out[3], int *status ){
/*
*  Name:
*     STPConv

*  Purpose:
*     Convert a set of 3D solar system positions between specified STP
*     coordinate systems.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "slamap.h"
*     void STPConv( double mjd, int ignore_origins, int n, int in_sys,
*                   double in_obs[3], double *in[3], int out_sys,
*                   double out_obs[3], double *out[3], int *status ){

*  Class Membership:
*     Frame method.

*  Description:
*     This function converts a set of 3D solar-system positions from
*     the specified input coordinate system to the specified output
*     coordinate system. See astSTPConv for a list of the available
*     coordinate systems.

*  Parameters:
*     mjd
*        The Modified Julian Date to which the coordinate systems refer.
*     ignore_origins
*        If non-zero, then the coordinate system definitions are modified so
*        that all cartesian systems have the origin at the centre of the
*        Sun. If zero, the correct origins are used for each individual
*        system.
*     n
*        The number of positions to transform.
*     in_sys
*        The coordinate system in which the input positions are supplied
*     in_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the input system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     in
*        A 3-element array holding the input positions. Each of the 3
*        elements should point to an array of "n" axis values. For spherical
*        input systems, in[3] can be supplied as NULL, in which case a
*        constant value of 1 AU will be used.
*     out_sys
*        The coordinate system in which the input positions are supplied
*        (see "Supported Coordinate Systems" below).
*     out_obs
*        The position of the observer in AST__HAEC coordinates. This is only
*        needed if the output system is an observer-centric system. If this
*        is not the case, a NULL pointer can be supplied. A NULL pointer
*        can also be supplied to indicate that he observer is at the centre of
*        the earth at the specified date.
*     out
*        A 3-element array holding the input positions. Each of the 3
*        elements should point to an array of "n" axis values. For spherical
*        output coordinates, out[2] may be NULL, in which case the output
*        radius values are thrown away.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Output longitude values are always in the range 0 - 2.PI.
*     - The "in" and "out" arrays may safely point to the same memory.
*     - The contents of the output array is left unchanged if an error
*     has already occurred.
*/

/* Local Variables: */
   double *out2;      /* Pointer to output third axis values */
   double *px;        /* Pointer to next X axis value */
   double *py;        /* Pointer to next Y axis value */
   double *pz;        /* Pointer to next Z axis value */
   double lat;        /* Latitude value */
   double lng;        /* Longitude value */
   double mat1[3][3]; /* Input->HAEC rotation matrix */
   double mat2[3][3]; /* Output->HAEC rotation matrix */
   double mat3[3][3]; /* HAEC->output rotation matrix */
   double mat4[3][3]; /* Input->output rotation matrix */
   double off1[3];    /* Origin of input system in HAEC coords */
   double off2[3];    /* Origin of output system in HAEC coords */
   double off3[3];    /* HAEC vector from output origin to input origin */
   double off4[3];    /* Position of input origin within output system */
   double p[3];       /* Current position */
   double q[3];       /* New position */
   double radius;     /* Radius value */
   int cur_sys;       /* Current system for output values */
   int i;             /* Loop count */
   int j;             /* Loop count */
   int inCsys;        /* Input cartesian system */
   int outCsys;       /* Output cartesian system */
   size_t nbyte;      /* Amount of memory to copy */

/* Check the global error status. */
   if ( !astOK ) return;

/* If out[2] was supplied as null, allocate memory to hold third axis
   values. Otherwise, use the supplied array. */
   nbyte = n*sizeof( double );
   if( !out[2] ) {
      out2 = (double *) astMalloc( nbyte );
   } else {
      out2 = out[2];
   }

/* Copy the input data to the output data and note that the output values
   are currently in the same system as the input values. */
   memcpy ( out[ 0 ], in[ 0 ], nbyte );
   memcpy ( out[ 1 ], in[ 1 ], nbyte );
   if( in[2] ) {
      memcpy ( out2, in[ 2 ], nbyte );
   } else {
      for( i = 0; i < n; i++ ) out2[ i ] = AST__AU;
   }
   cur_sys = in_sys;

/* Skip the next bit if the output values are now in the required system. */
   if( cur_sys != out_sys ) {

/* If the current system is spherical note the corresponding cartesian
   system. If the current system is cartesian, use it. */
      if( cur_sys == AST__HG ){
         inCsys = AST__HGC;
      } else if( cur_sys == AST__HAQ ){
         inCsys = AST__HAQC;
      } else if( cur_sys == AST__HAE ){
         inCsys = AST__HAEC;
      } else if( cur_sys == AST__GSE ){
         inCsys = AST__GSEC;
      } else if( cur_sys == AST__HPC ){
         inCsys = AST__HPCC;
      } else if( cur_sys == AST__HPR ){
         inCsys = AST__HPRC;
      } else {
         inCsys = cur_sys;
      }

/* Convert input spherical positions into the corresponding cartesian system,
   putting the results in the "out" arrays. Modify the input system
   accordingly. */
      if( cur_sys != inCsys ) {
         px = out[ 0 ];
         py = out[ 1 ];
         pz = out2;
         for( i = 0; i < n; i++ ) {
            p[ 0 ] = *px;
            p[ 1 ] = *py;
            p[ 2 ] = *pz;
            if( p[ 0 ] != AST__BAD &&
                p[ 1 ] != AST__BAD &&
                p[ 2 ] != AST__BAD ) {
                palDcs2c( p[ 0 ], p[ 1 ], q );
                *(px++) = q[ 0 ]*p[ 2 ];
                *(py++) = q[ 1 ]*p[ 2 ];
                *(pz++) = q[ 2 ]*p[ 2 ];
            } else {
               *(px++) = AST__BAD;
               *(py++) = AST__BAD;
               *(pz++) = AST__BAD;
            }
         }

         cur_sys = inCsys;

      }
   }

/* Skip the next bit if the output values are now in the required system. */
   if( cur_sys != out_sys ) {

/* If the required output system is spherical, note the corresponding
   cartesian system. If the required output system is cartesian, use it.*/
      if( out_sys == AST__HG ){
         outCsys = AST__HGC;
      } else if( out_sys == AST__HAQ ){
         outCsys = AST__HAQC;
      } else if( out_sys == AST__HAE ){
         outCsys = AST__HAEC;
      } else if( out_sys == AST__GSE ){
         outCsys = AST__GSEC;
      } else if( out_sys == AST__HPC ){
         outCsys = AST__HPCC;
      } else if( out_sys == AST__HPR ){
         outCsys = AST__HPRC;
      } else {
         outCsys = out_sys;
      }

/* Skip the next bit if the output values are already in the required
   output cartesian system. */
      if( cur_sys != outCsys ) {

/* Obtain an offset vector and a rotation matrix which moves positions from
   the current (Cartesian) system to the AST__HAEC system. The offset vector
   returned by these functions is the AST__HAEC coordinates of the origin of
   the current system. The matrix rotates direction vectors from the current
   system to the AST__HAEC system. */
         if( cur_sys == AST__HGC ) {
            Hgc( mjd, mat1, off1, status );

         } else if( cur_sys == AST__HAEC ) {
            Haec( mjd, mat1, off1, status );

         } else if( cur_sys == AST__HAQC ) {
            Haqc( mjd, mat1, off1, status );

         } else if( cur_sys == AST__GSEC ) {
            Gsec( mjd, mat1, off1, status );

         } else if( cur_sys == AST__HPCC ) {
            Hpcc( mjd, in_obs, mat1, off1, status );

         } else if( cur_sys == AST__HPRC ) {
            Hprc( mjd, in_obs, mat1, off1, status );

         } else {
            astError( AST__INTER, "astSTPConv(SlaMap): Unsupported input "
                      "cartesian coordinate system type %d (internal AST "
                      "programming error).", status, cur_sys );
         }

/* Obtain an offset vector and a rotation matrix which moves positions from
   the required output Cartesian system to the AST__HAEC system. */
         if( outCsys == AST__HGC ) {
            Hgc( mjd, mat2, off2, status );

         } else if( outCsys == AST__HAEC ) {
            Haec( mjd, mat2, off2, status );

         } else if( outCsys == AST__HAQC ) {
            Haqc( mjd, mat2, off2, status );

         } else if( outCsys == AST__GSEC ) {
            Gsec( mjd, mat2, off2, status );

         } else if( outCsys == AST__HPCC ) {
            Hpcc( mjd, out_obs, mat2, off2, status );

         } else if( outCsys == AST__HPRC ) {
            Hprc( mjd, out_obs, mat2, off2, status );

         } else {
            astError( AST__INTER, "astSTPConv(SlaMap): Unsupported output "
                      "cartesian coordinate system type %d (internal AST "
                      "programming error).", status, outCsys );
         }

/* Invert the second matrix to get the matrix which rotates AST__HAEC coords
   to the output cartesian system. This an be done simply by transposing it
   since all the matrices are 3D rotations. */
         for( i = 0; i < 3; i++ ) {
            for( j = 0; j < 3; j++ ) mat3[ i ][ j ] = mat2[ j ][ i ];

/* Find the offset in AST__HAEC coords from the origin of the output
   cartesian system to the origin of the current system. */
            off3[ i ] = off1[ i ] - off2[ i ];
         }

/* Unless the origins are being ignored, use the above matrix to rotate the
   above AST__HAEC offset into the output cartesian system. If origins are
   being ignored, use an offset of zero. */
         if( ignore_origins ) {
            off4[ 0 ] = 0.0;
            off4[ 1 ] = 0.0;
            off4[ 2 ] = 0.0;
         } else {
            palDmxv( mat3, off3, off4 );
         }

/* Concatentate the two matrices to get the matrix which rotates from the
   current system to the output cartesian system. */
         palDmxm( mat3, mat1, mat4 );

/* Use the matrix and offset to convert current positions to output
   cartesian positions. */
         px = out[ 0 ];
         py = out[ 1 ];
         pz = out2;

         for( i = 0; i < n; i++ ) {
            p[ 0 ] = *px;
            p[ 1 ] = *py;
            p[ 2 ] = *pz;

            if( p[ 0 ] != AST__BAD &&
                p[ 1 ] != AST__BAD &&
                p[ 2 ] != AST__BAD ) {
               palDmxv( mat4, p, q );
               *(px++) = q[ 0 ] + off4[ 0 ];
               *(py++) = q[ 1 ] + off4[ 1 ];
               *(pz++) = q[ 2 ] + off4[ 2 ];
            } else {
               *(px++) = AST__BAD;
               *(py++) = AST__BAD;
               *(pz++) = AST__BAD;
            }
         }

/* Indicate that the output values are now in the required output
   cartesian system. */
         cur_sys = outCsys;

      }
   }

/* Skip the next bit if the output values are now in the required system. */
   if( cur_sys != out_sys ) {

/* The only reason why the output values may not be in the required output
   system is because the output system is spherical. Convert output Cartesian
   positions to output spherical positions. */
      px = out[ 0 ];
      py = out[ 1 ];
      pz = out2;
      for( i = 0; i < n; i++ ) {
         p[ 0 ] = *px;
         p[ 1 ] = *py;
         p[ 2 ] = *pz;
         if( p[ 0 ] != AST__BAD &&
             p[ 1 ] != AST__BAD &&
             p[ 2 ] != AST__BAD ) {
             palDvn( p, q, &radius );
             palDcc2s( q, &lng, &lat );
             *(px++) = palDranrm( lng );
             *(py++) = lat;
             *(pz++) = radius;
         } else {
            *(px++) = AST__BAD;
            *(py++) = AST__BAD;
            *(pz++) = AST__BAD;
         }
      }
   }

/* If out[2] was supplied as null, free the memory used to hold third axis
   values. */
   if( !out[2] ) out2 = (double *) astFree( (void *) out2 );
}

void astInitSlaMapVtab_(  AstSlaMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSlaMapVtab

*  Purpose:
*     Initialise a virtual function table for a SlaMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "slamap.h"
*     void astInitSlaMapVtab( AstSlaMapVtab *vtab, const char *name )

*  Class Membership:
*     SlaMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SlaMap class.

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
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsASlaMap) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->SlaAdd = SlaAdd;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "SlaMap",
               "Conversion between sky coordinate systems" );

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
*     Simplify a sequence of Mappings containing an SlaMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     SlaMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated SlaMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated SlaMap with one which it
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
*        Pointer to the nominated SlaMap which is to be merged with
*        its neighbours. This should be a cloned copy of the SlaMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        SlaMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated SlaMap resides.
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
   AstSlaMap *slamap;            /* Pointer to SlaMap */
   const char *argdesc[ MAX_SLA_ARGS ]; /* Argument descriptions (junk) */
   const char *class;            /* Pointer to Mapping class string */
   const char *comment;          /* Pointer to comment string (junk) */
   double (*cvtargs)[ MAX_SLA_ARGS ]; /* Pointer to argument arrays */
   int *cvttype;                 /* Pointer to transformation type codes */
   int *narg;                    /* Pointer to argument count array */
   int done;                     /* Finished (no further simplification)? */
   int iarg;                     /* Loop counter for arguments */
   int icvt1;                    /* Loop initial value */
   int icvt2;                    /* Loop final value */
   int icvt;                     /* Loop counter for transformation steps */
   int ikeep;                    /* Index to store step being kept */
   int imap1;                    /* Index of first SlaMap to merge */
   int imap2;                    /* Index of last SlaMap to merge */
   int imap;                     /* Loop counter for Mappings */
   int inc;                      /* Increment for transformation step loop */
   int invert;                   /* SlaMap applied in inverse direction? */
   int istep;                    /* Loop counter for transformation steps */
   int keep;                     /* Keep transformation step? */
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

/* SlaMaps can only be merged if they are in series (or if there is
   only one Mapping present, in which case it makes no difference), so
   do nothing if they are not. */
   if ( series || ( *nmap == 1 ) ) {

/* Initialise the number of transformation steps to be merged to equal
   the number in the nominated SlaMap. */
      nstep = ( (AstSlaMap *) ( *map_list )[ where ] )->ncvt;

/* Search adjacent lower-numbered Mappings until one is found which is
   not an SlaMap. Accumulate the number of transformation steps
   involved in any SlaMaps found. */
      imap1 = where;
      while ( ( imap1 - 1 >= 0 ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap1 - 1 ] );
         if ( !astOK || strcmp( class, "SlaMap" ) ) break;
         nstep += ( (AstSlaMap *) ( *map_list )[ imap1 - 1 ] )->ncvt;
         imap1--;
      }

/* Similarly search adjacent higher-numbered Mappings. */
      imap2 = where;
      while ( ( imap2 + 1 < *nmap ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap2 + 1 ] );
         if ( !astOK || strcmp( class, "SlaMap" ) ) break;
         nstep += ( (AstSlaMap *) ( *map_list )[ imap2 + 1 ] )->ncvt;
         imap2++;
      }

/* Remember the initial number of transformation steps. */
      nstep0 = nstep;

/* Allocate memory for accumulating a list of all the transformation
   steps involved in all the SlaMaps found. */
      cvttype = astMalloc( sizeof( int ) * (size_t) nstep );
      cvtargs = astMalloc( sizeof( double[ MAX_SLA_ARGS ] ) * (size_t) nstep );
      narg = astMalloc( sizeof( int ) * (size_t) nstep );

/* Loop to obtain the transformation data for each SlaMap being merged. */
      nstep = 0;
      for ( imap = imap1; astOK && ( imap <= imap2 ); imap++ ) {

/* Obtain a pointer to the SlaMap and note if it is being applied in
   its inverse direction. */
         slamap = (AstSlaMap *) ( *map_list )[ imap ];
         invert = ( *invert_list )[ imap ];

/* Set up loop limits and an increment to scan the transformation
   steps in each SlaMap in either the forward or reverse direction, as
   dictated by the associated "invert" value. */
         icvt1 = invert ? slamap->ncvt - 1 : 0;
         icvt2 = invert ? -1 : slamap->ncvt;
         inc = invert ? -1 : 1;

/* Loop through each transformation step in the SlaMap. */
         for ( icvt = icvt1; icvt != icvt2; icvt += inc ) {

/* For simplicity, free any extra information stored with the conversion
   step (it will be recreated as and when necessary). */
            slamap->cvtextra[ icvt ] = astFree( slamap->cvtextra[ icvt ] );

/* Store the transformation type code and use "CvtString" to determine
   the associated number of arguments. Then store these arguments. */
            cvttype[ nstep ] = slamap->cvttype[ icvt ];
            (void) CvtString( cvttype[ nstep ], &comment, narg + nstep,
                              argdesc, status );
            if ( !astOK ) break;
            for ( iarg = 0; iarg < narg[ nstep ]; iarg++ ) {
               cvtargs[ nstep ][ iarg ] = slamap->cvtargs[ icvt ][ iarg ];
            }

/* If the SlaMap is inverted, we must not only accumulate its
   transformation steps in reverse, but also apply them in
   reverse. For some steps this means swapping arguments, for some it
   means changing the transformation type code to a complementary
   value, and for others it means both.  Define macros to perform each
   of these changes. */

/* Macro to swap the values of two nominated arguments if the
   transformation type code matches "code". */
#define SWAP_ARGS( code, arg1, arg2 ) \
            if ( cvttype[ nstep ] == code ) { \
               double tmp = cvtargs[ nstep ][ arg1 ]; \
               cvtargs[ nstep ][ arg1 ] = cvtargs[ nstep ][ arg2 ]; \
               cvtargs[ nstep ][ arg2 ] = tmp; \
            }

/* Macro to exchange a transformation type code for its inverse (and
   vice versa). */
#define SWAP_CODES( code1, code2 ) \
            if ( cvttype[ nstep ] == code1 ) { \
               cvttype[ nstep ] = code2; \
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
            }

/* Use these macros to apply the changes where needed. */
            if ( invert ) {

/* E-terms of aberration. */
/* ---------------------- */
/* Exchange addition and subtraction of E-terms. */
               SWAP_CODES( AST__SLA_ADDET, AST__SLA_SUBET )

/* Bessel-Newcomb pre-IAU 1976 (FK4) precession model. */
/* --------------------------------------------------- */
/* Exchange the starting and ending Besselian epochs. */
               SWAP_ARGS( AST__SLA_PREBN, 0, 1 )

/* IAU 1975 (FK5) precession model. */
/* -------------------------------- */
/* Exchange the starting and ending epochs. */
               SWAP_ARGS( AST__SLA_PREC, 0, 1 )

/* FK4 to FK5 (no proper motion or parallax). */
/* ------------------------------------------ */
/* Exchange FK5 to FK4 conversion for its inverse, and vice versa. */
               SWAP_CODES( AST__SLA_FK54Z, AST__SLA_FK45Z )

/* Geocentric apparent to mean place. */
/* ---------------------------------- */
/* Exchange the transformation code for its inverse and also exchange
   the order of the date and equinox arguments. */
               SWAP_CODES( AST__SLA_AMP, AST__SLA_MAP )
               SWAP_ARGS( AST__SLA_AMP, 0, 1 )
               SWAP_ARGS( AST__SLA_MAP, 0, 1 )

/* Ecliptic coordinates to FK5 J2000.0 equatorial. */
/* ------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__SLA_ECLEQ, AST__SLA_EQECL )

/* Horizon to equatorial. */
/* ---------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__SLA_DH2E, AST__SLA_DE2H )

/* Galactic coordinates to FK5 J2000.0 equatorial. */
/* ------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__SLA_GALEQ, AST__SLA_EQGAL )

/* ICRS coordinates to FK5 J2000.0 equatorial. */
/* ------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__SLA_HFK5Z, AST__SLA_FK5HZ )

/* Galactic to supergalactic coordinates. */
/* -------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__SLA_GALSUP, AST__SLA_SUPGAL )

/* FK5 J2000 equatorial coordinates to Helioprojective-Cartesian. */
/* -------------------------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__EQHPC, AST__HPCEQ )

/* FK5 J2000 equatorial coordinates to Helioprojective-Radial. */
/* ----------------------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__EQHPR, AST__HPREQ )

/* FK5 J2000 equatorial coordinates to Helio-ecliptic. */
/* --------------------------------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__EQHE, AST__HEEQ )

/* Dynamical J2000.0 to ICRS. */
/* -------------------------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__J2000H, AST__HJ2000 )

/* HA to RA */
/* -------- */
/* Exchange the transformation code for its inverse. */
               SWAP_CODES( AST__H2R, AST__R2H )

            }

/* Undefine the local macros. */
#undef SWAP_ARGS
#undef SWAP_CODES

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

/* Eliminate redundant precession corrections. */
/* ------------------------------------------- */
/* First check if this is a redundant precession transformation
   (i.e. the starting and ending epochs are the same). If so, then
   note that it should not be kept. */
            if ( ( ( cvttype[ istep ] == AST__SLA_PREBN ) ||
                   ( cvttype[ istep ] == AST__SLA_PREC ) ) &&
                 EQUAL( cvtargs[ istep ][ 0 ], cvtargs[ istep ][ 1 ] ) ) {
               keep = 0;

/* The remaining simplifications act to combine adjacent
   transformation steps, so only apply them while there are at least 2
   steps left. */
            } else if ( istep < ( nstep - 1 ) ) {

/* Define a macro to test if two adjacent transformation type codes
   have specified values. */
#define PAIR_CVT( code1, code2 ) \
               ( ( cvttype[ istep ] == code1 ) && \
                 ( cvttype[ istep + 1 ] == code2 ) )

/* Combine adjacent precession corrections. */
/* ---------------------------------------- */
/* If two precession corrections are adjacent, and have an equinox
   value in common, then they may be combined into a single correction
   by eliminating the common equinox. */
               if ( ( PAIR_CVT( AST__SLA_PREBN, AST__SLA_PREBN ) ||
                      PAIR_CVT( AST__SLA_PREC, AST__SLA_PREC ) ) &&
                    EQUAL( cvtargs[ istep ][ 1 ], cvtargs[ istep + 1 ][ 0 ] ) ) {

/* Retain the second correction, changing its first argument, and
   eliminate the first correction. */
                  cvtargs[ istep + 1 ][ 0 ] = cvtargs[ istep ][ 0 ];
                  istep++;

/* Eliminate redundant E-term handling. */
/* ------------------------------------ */
/* Check if adjacent steps implement a matching pair of corrections
   for the E-terms of aberration with the same argument value. If so,
   they will cancel, so eliminate them both. */
               } else if ( ( PAIR_CVT( AST__SLA_SUBET, AST__SLA_ADDET ) ||
                             PAIR_CVT( AST__SLA_ADDET, AST__SLA_SUBET ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant FK4/FK5 conversions. */
/* ---------------------------------------- */
/* Similarly, check for a matching pair of FK4/FK5 conversions with
   the same argument value and eliminate them both if possible. */
               } else if ( ( PAIR_CVT( AST__SLA_FK45Z, AST__SLA_FK54Z ) ||
                             PAIR_CVT( AST__SLA_FK54Z, AST__SLA_FK45Z ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant ICRS/FK5 conversions. */
/* ----------------------------------------- */
/* Similarly, check for a matching pair of ICRS/FK5 conversions with
   the same argument value and eliminate them both if possible. */
               } else if ( ( PAIR_CVT( AST__SLA_HFK5Z, AST__SLA_FK5HZ ) ||
                             PAIR_CVT( AST__SLA_FK5HZ, AST__SLA_HFK5Z ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant geocentric apparent conversions. */
/* ---------------------------------------------------- */
/* As above, check for a matching pair of conversions with matching
   argument values (note the argument order reverses for the two
   directions) and eliminate them if possible. */
               } else if ( ( PAIR_CVT( AST__SLA_AMP, AST__SLA_MAP ) ||
                             PAIR_CVT( AST__SLA_MAP, AST__SLA_AMP ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 1 ] ) &&
                           EQUAL( cvtargs[ istep ][ 1 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant ecliptic coordinate conversions. */
/* ---------------------------------------------------- */
/* This is handled in the same way as the FK4/FK5 case. */
               } else if ( ( PAIR_CVT( AST__SLA_ECLEQ, AST__SLA_EQECL ) ||
                             PAIR_CVT( AST__SLA_EQECL, AST__SLA_ECLEQ ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant AzEl coordinate conversions. */
/* ------------------------------------------------ */
               } else if ( ( PAIR_CVT( AST__SLA_DH2E, AST__SLA_DE2H ) ||
                             PAIR_CVT( AST__SLA_DE2H, AST__SLA_DH2E ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) &&
                           EQUAL( cvtargs[ istep ][ 1 ],
                                  cvtargs[ istep + 1 ][ 1 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant galactic coordinate conversions. */
/* ---------------------------------------------------- */
/* This is handled as above, except that there are no arguments to
   check. */
               } else if ( PAIR_CVT( AST__SLA_GALEQ, AST__SLA_EQGAL ) ||
                           PAIR_CVT( AST__SLA_EQGAL, AST__SLA_GALEQ ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant supergalactic coordinate conversions. */
/* --------------------------------------------------------- */
/* This is handled as above. */
               } else if ( PAIR_CVT( AST__SLA_GALSUP, AST__SLA_SUPGAL ) ||
                           PAIR_CVT( AST__SLA_SUPGAL, AST__SLA_GALSUP ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant helioprojective-Cartesian coordinate conversions. */
/* --------------------------------------------------------------------- */
               } else if ( ( PAIR_CVT( AST__HPCEQ, AST__EQHPC ) ||
                             PAIR_CVT( AST__EQHPC, AST__HPCEQ ) ) &&
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

/* Eliminate redundant helioprojective-Radial coordinate conversions. */
/* --------------------------------------------------------------------- */
               } else if ( ( PAIR_CVT( AST__HPREQ, AST__EQHPR ) ||
                             PAIR_CVT( AST__EQHPR, AST__HPREQ ) ) &&
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

/* Eliminate redundant helio-ecliptic coordinate conversions. */
/* ---------------------------------------------------------- */
               } else if ( ( PAIR_CVT( AST__EQHE, AST__HEEQ ) ||
                             PAIR_CVT( AST__HEEQ, AST__EQHE ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant dynamical J2000 coordinate conversions. */
/* ----------------------------------------------------------- */
               } else if ( PAIR_CVT( AST__J2000H, AST__HJ2000 ) ||
                           PAIR_CVT( AST__HJ2000, AST__J2000H ) ) {
                  istep++;
                  keep = 0;

/* Eliminate redundant Hour Angle conversions. */
/* ------------------------------------------- */
               } else if ( ( PAIR_CVT( AST__R2H, AST__H2R ) ||
                             PAIR_CVT( AST__H2R, AST__R2H ) ) &&
                           EQUAL( cvtargs[ istep ][ 0 ],
                                  cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

               }

/* Undefine the local macro. */
#undef PAIR_CVT
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
                  for ( iarg = 0; iarg < narg[ istep ]; iarg++ ) {
                     cvtargs[ ikeep ][ iarg ] = cvtargs[ istep ][ iarg ];
                  }
                  narg[ ikeep ] = narg[ istep ];
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
   number of transformation steps was reduced, or (c) the SlaMap(s)
   can be replaced by a UnitMap, or (d) if there was initially only
   one SlaMap present, its invert flag was set (this flag will always
   be cleared in the replacement Mapping). */
         simpler = ngone || ( nstep < nstep0 ) || unit ||
                   ( *invert_list )[ where ];

/* Do nothing more unless simplification is possible. */
         if ( simpler ) {

/* If the replacement Mapping is a UnitMap, then create it. */
            if ( unit ) {
               new = (AstMapping *)
                        astUnitMap( astGetNin( ( *map_list )[ where ] ), "", status );

/* Otherwise, create a replacement SlaMap and add each of the
   remaining transformation steps to it. */
            } else {
               new = (AstMapping *) astSlaMap( 0, "", status );
               for ( istep = 0; istep < nstep; istep++ ) {
                  AddSlaCvt( (AstSlaMap *) new, cvttype[ istep ],
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
      narg = astFree( narg );
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static void SlaAdd( AstSlaMap *this, const char *cvt, const double args[], int *status ) {
/*
*++
*  Name:
c     astSlaAdd
f     AST_SLAADD

*  Purpose:
*     Add a celestial coordinate conversion to an SlaMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "slamap.h"
c     void astSlaAdd( AstSlaMap *this, const char *cvt, const double args[] )
f     CALL AST_SLAADD( THIS, CVT, ARGS, STATUS )

*  Class Membership:
*     SlaMap method.

*  Description:
c     This function adds one of the standard celestial coordinate
f     This routine adds one of the standard celestial coordinate
*     system conversions provided by the SLALIB Positional Astronomy
*     Library (Starlink User Note SUN/67) to an existing SlaMap.
*
c     When an SlaMap is first created (using astSlaMap), it simply
f     When an SlaMap is first created (using AST_SLAMAP), it simply
c     performs a unit (null) Mapping. By using astSlaAdd (repeatedly
f     performs a unit (null) Mapping. By using AST_SLAADD (repeatedly
*     if necessary), one or more coordinate conversion steps may then
*     be added, which the SlaMap will perform in sequence. This allows
*     multi-step conversions between a variety of celestial coordinate
*     systems to be assembled out of the building blocks provided by
*     SLALIB.
*
*     Normally, if an SlaMap's Invert attribute is zero (the default),
*     then its forward transformation is performed by carrying out
*     each of the individual coordinate conversions specified by
c     astSlaAdd in the order given (i.e. with the most recently added
f     AST_SLAADD in the order given (i.e. with the most recently added
*     conversion applied last).
*
*     This order is reversed if the SlaMap's Invert attribute is
*     non-zero (or if the inverse transformation is requested by any
*     other means) and each individual coordinate conversion is also
*     replaced by its own inverse. This process inverts the overall
*     effect of the SlaMap. In this case, the first conversion to be
*     applied would be the inverse of the one most recently added.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the SlaMap.
c     cvt
f     CVT = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string which identifies the
f        A character string which identifies the
*        celestial coordinate conversion to be added to the
*        SlaMap. See the "SLALIB Conversions" section for details of
*        those available.
c     args
f     ARGS( * ) = DOUBLE PRECISION (Given)
*        An array containing argument values for the celestial
*        coordinate conversion. The number of arguments required, and
*        hence the number of array elements used, depends on the
*        conversion specified (see the "SLALIB Conversions"
*        section). This array is ignored
c        and a NULL pointer may be supplied
*        if no arguments are needed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - All coordinate values processed by an SlaMap are in
*     radians. The first coordinate is the celestial longitude and the
*     second coordinate is the celestial latitude.
*     - When assembling a multi-stage conversion, it can sometimes be
*     difficult to determine the most economical conversion path. For
*     example, converting to the standard FK5 coordinate system as an
*     intermediate stage is often sensible in formulating the problem,
*     but may introduce unnecessary extra conversion steps. A solution
*     to this is to include all the steps which are (logically)
c     necessary, but then to use astSimplify to simplify the resulting
f     necessary, but then to use AST_SIMPLIFY to simplify the resulting
*     SlaMap. The simplification process will eliminate any steps
*     which turn out not to be needed.
c     - This function does not check to ensure that the sequence of
f     - This routine does not check to ensure that the sequence of
*     coordinate conversions added to an SlaMap is physically
*     meaningful.

*  SLALIB Conversions:
*     The following strings (which are case-insensitive) may be supplied
c     via the "cvt" parameter to indicate which celestial coordinate
f     via the CVT argument to indicate which celestial coordinate
*     conversion is to be added to the SlaMap. Each string is derived
*     from the name of the SLALIB routine that performs the
*     conversion and the relevant documentation (SUN/67) should be
*     consulted for details.  Where arguments are needed by
*     the conversion, they are listed in parentheses. Values for
c     these arguments should be given, via the "args" array, in the
f     these arguments should be given, via the ARGS array, in the
*     order indicated. The argument names match the corresponding
*     SLALIB routine arguments and their values should be given using
*     exactly the same units, time scale, calendar, etc. as described
*     in SUN/67:
*
*     - "ADDET" (EQ): Add E-terms of aberration.
*     - "SUBET" (EQ): Subtract E-terms of aberration.
*     - "PREBN" (BEP0,BEP1): Apply Bessel-Newcomb pre-IAU 1976 (FK4)
*     precession model.
*     - "PREC" (EP0,EP1): Apply IAU 1975 (FK5) precession model.
*     - "FK45Z" (BEPOCH): Convert FK4 to FK5 (no proper motion or parallax).
*     - "FK54Z" (BEPOCH): Convert FK5 to FK4 (no proper motion or parallax).
*     - "AMP" (DATE,EQ): Convert geocentric apparent to mean place.
*     - "MAP" (EQ,DATE): Convert mean place to geocentric apparent.
*     - "ECLEQ" (DATE): Convert ecliptic coordinates to FK5 J2000.0 equatorial.
*     - "EQECL" (DATE): Convert equatorial FK5 J2000.0 to ecliptic coordinates.
*     - "GALEQ": Convert galactic coordinates to FK5 J2000.0 equatorial.
*     - "EQGAL": Convert FK5 J2000.0 equatorial to galactic coordinates.
*     - "HFK5Z" (JEPOCH): Convert ICRS coordinates to FK5 J2000.0 equatorial.
*     - "FK5HZ" (JEPOCH): Convert FK5 J2000.0 equatorial coordinates to ICRS.
*     - "GALSUP": Convert galactic to supergalactic coordinates.
*     - "SUPGAL": Convert supergalactic coordinates to galactic.
*     - "J2000H": Convert dynamical J2000.0 to ICRS.
*     - "HJ2000": Convert ICRS to dynamical J2000.0.
*     - "R2H" (LAST): Convert RA to Hour Angle.
*     - "H2R" (LAST): Convert Hour Angle to RA.
*
*     For example, to use the "ADDET" conversion, which takes a single
*     argument EQ, you should consult the documentation for the SLALIB
*     routine SLA_ADDET. This describes the conversion in detail and
*     shows that EQ is the Besselian epoch of the mean equator and
*     equinox.
c     This value should then be supplied to astSlaAdd in args[0].
f     This value should then be supplied to AST_SLAADD in ARGS(1).
*
*     In addition the following strings may be supplied for more complex
*     conversions which do not correspond to any one single SLALIB routine
*     (DIURAB is the magnitude of the diurnal aberration vector in units
*     of "day/(2.PI)", DATE is the Modified Julian Date of the observation,
*     and (OBSX,OBSY,OBZ) are the Heliocentric-Aries-Ecliptic cartesian
*     coordinates, in metres, of the observer):
*
*     - "HPCEQ" (DATE,OBSX,OBSY,OBSZ): Convert Helioprojective-Cartesian coordinates to J2000.0 equatorial.
*     - "EQHPC" (DATE,OBSX,OBSY,OBSZ): Convert J2000.0 equatorial coordinates to Helioprojective-Cartesian.
*     - "HPREQ" (DATE,OBSX,OBSY,OBSZ): Convert Helioprojective-Radial coordinates to J2000.0 equatorial.
*     - "EQHPR" (DATE,OBSX,OBSY,OBSZ): Convert J2000.0 equatorial coordinates to Helioprojective-Radial.
*     - "HEEQ" (DATE): Convert helio-ecliptic coordinates to J2000.0 equatorial.
*     - "EQHE" (DATE): Convert J2000.0 equatorial coordinates to helio-ecliptic.
*     - "H2E" (LAT,DIRUAB): Convert horizon coordinates to equatorial.
*     - "E2H" (LAT,DIURAB): Convert equatorial coordinates to horizon.
*
*     Note, the "H2E" and "E2H" conversions convert between topocentric
*     horizon coordinates (azimuth,elevation), and apparent local equatorial
*     coordinates (hour angle,declination). Thus, the effects of diurnal
*     aberration are taken into account in the conversions but the effects
*     of atmospheric refraction are not.

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
   if ( astOK && ( cvttype == AST__SLA_NULL ) ) {
      astError( AST__SLAIN,
                "astSlaAdd(%s): Invalid SLALIB sky coordinate conversion "
                "type \"%s\".", status, astGetClass( this ), cvt );
   }

/* Add the new conversion to the SlaMap. */
   AddSlaCvt( this, cvttype, args, status );
}

static void SolarPole( double mjd, double pole[3], int *status ) {
/*
*  Name:
*     SolarPole

*  Purpose:
*     Returns a unit vector along the solar north pole at the given date.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     void SolarPole( double mjd, double pole[3], int *status )

*  Class Membership:
*     SlaMap member function.

*  Description:
*     This function returns a unit vector along the solar north pole at
*     the given date, in the AST__HAEC coordinate system.

*  Parameters:
*     mjd
*        The date at which the solar north pole vector is required.
*     pole
*        An array holding the (X,Y,Z) components of the vector, in the
*        AST__HAEC system.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  AST__BAD will be returned for all components of the vector if this
*     function is invoked with the global error status set, or if it should
*     fail for any reason.
*/

/* Local Variables: */
   double omega;
   double sproj;
   double inc;
   double t1;

/* Initialize. */
   pole[0] = AST__BAD;
   pole[1] = AST__BAD;
   pole[2] = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return;

/* First, we find the ecliptic longitude of the ascending node of the solar
   equator on the ecliptic at the required date. This is based on the
   equation in the "Explanatory Supplement to the Astronomical Alamanac",
   section "Physical Ephemeris of the Sun":

   Omega = 75.76 + 0.01397*T degrees

   Note, the text at the start of the chapter says that "T" is measured in
   centuries since J2000, but the equivalent expression in Table 15.4 is
   only consistent with the above equation if "T" is measured in days since
   J2000. We assume T is in days. The text does not explicitly say so,
   but we assume that this longitude value (Omega) is with respect to the
   mean equinox of J2000.0. */
   omega = 75.76 + 0.01397*( palEpj(mjd) - 2000.0 );

/* Convert this to the ecliptic longitude of the projection of the sun's
   north pole onto the ecliptic, in radians. */
   sproj = ( omega - 90.0 )*D2R;

/* Obtain a unit vector parallel to the sun's north pole, in terms of
   the required ecliptic (X,Y,Z) axes, in which X points towards ecliptic
   longitude/latitude ( 0, 0 ), Y axis points towards ecliptic
   longitude/latitude ( 90, 0 ) degrees, and Z axis points towards the
   ecliptic north pole. The inclination of the solar axis to the ecliptic
   axis (7.25 degrees) is taken from the "Explanatory Supplement" section
   "The Physical Ephemeris of the Sun". */
   inc = 7.25*D2R;
   t1 = sin( inc );
   pole[ 0 ]= t1*cos( sproj );
   pole[ 1 ] = t1*sin( sproj );
   pole[ 2 ] = cos( inc );

}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply an SlaMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     SlaMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes an SlaMap and a set of points encapsulated
*     in a PointSet and transforms the points so as to perform the
*     sequence of SLALIB sky coordinate conversions specified by
*     previous invocations of astSlaAdd.

*  Parameters:
*     this
*        Pointer to the SlaMap.
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
*     match the number of coordinates for the SlaMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstSlaMap *map;               /* Pointer to SlaMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *alpha;                /* Pointer to longitude array */
   double *args;                 /* Pointer to argument list for conversion */
   double *extra;                /* Pointer to intermediate values */
   double *delta;                /* Pointer to latitude array */
   double *p[3];                 /* Pointers to arrays to be transformed */
   double *obs;                  /* Pointer to array holding observers position */
   int cvt;                      /* Loop counter for conversions */
   int ct;                       /* Conversion type */
   int end;                      /* Termination index for conversion loop */
   int inc;                      /* Increment for conversion loop */
   int ncoord_in;                /* Number of coordinates per input point */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */
   int start;                    /* Starting index for conversion loop */
   int sys;                      /* STP coordinate system code */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this);

/* Obtain a pointer to the SlaMap. */
   map = (AstSlaMap *) this;

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
/* Use "alpha" and "delta" as synonyms for the arrays of longitude and latitude
   coordinate values stored in the output PointSet. */
   if ( astOK ) {
      alpha = ptr_out[ 0 ];
      delta = ptr_out[ 1 ];

/* Initialise the output coordinate values by copying the input ones. */
      (void) memcpy( alpha, ptr_in[ 0 ], sizeof( double ) * (size_t) npoint );
      (void) memcpy( delta, ptr_in[ 1 ], sizeof( double ) * (size_t) npoint );

/* We will loop to apply each SLALIB sky coordinate conversion in turn to the
   (alpha,delta) arrays. However, if the inverse transformation was requested,
   we must loop through these transformations in reverse order, so set up
   appropriate limits and an increment to control this loop. */
      start = forward ? 0 : map->ncvt - 1;
      end = forward ? map->ncvt : -1;
      inc = forward ? 1 : -1;

/* Loop through the coordinate conversions in the required order and obtain a
   pointer to the argument list for the current conversion. */
      for ( cvt = start; cvt != end; cvt += inc ) {
         args = map->cvtargs[ cvt ];
         extra = map->cvtextra[ cvt ];

/* Define a local macro as a shorthand to apply the code given as "function"
   (the macro argument) to each element of the (alpha,delta) arrays in turn.
   Before applying this conversion function, each element is first checked for
   "bad" coordinates (indicated by the value AST__BAD) and appropriate "bad"
   result values are assigned if necessary. */
#define TRAN_ARRAY(function) \
        for ( point = 0; point < npoint; point++ ) { \
           if ( ( alpha[ point ] == AST__BAD ) || \
                ( delta[ point ] == AST__BAD ) ) { \
              alpha[ point ] = AST__BAD; \
              delta[ point ] = AST__BAD; \
	   } else { \
              function \
	   } \
        }

/* Classify the SLALIB sky coordinate conversion to be applied. */
         ct = map->cvttype[ cvt ];
         switch ( ct ) {

/* Add E-terms of aberration. */
/* -------------------------- */
/* Add or subtract (for the inverse) the E-terms from each coordinate pair
   in turn, returning the results to the same arrays. */
            case AST__SLA_ADDET:
               if ( forward ) {
                  TRAN_ARRAY(palAddet( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
	       } else {
                  TRAN_ARRAY(palSubet( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
               }
               break;

/* Subtract E-terms of aberration. */
/* ------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
            case AST__SLA_SUBET:
               if ( forward ) {
                  TRAN_ARRAY(palSubet( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
	       } else {
                  TRAN_ARRAY(palAddet( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
	       }
               break;

/* Apply Bessel-Newcomb pre-IAU 1976 (FK4) precession model. */
/* --------------------------------------------------------- */
/* Since we are transforming a sequence of points, first set up the required
   precession matrix, swapping the argument order to get the inverse matrix
   if required. */
            case AST__SLA_PREBN:
               {
                  double epoch1 = forward ? args[ 0 ] : args[ 1 ];
                  double epoch2 = forward ? args[ 1 ] : args[ 0 ];
                  double precess_matrix[ 3 ][ 3 ];
                  double vec1[ 3 ];
                  double vec2[ 3 ];
                  palPrebn( epoch1, epoch2, precess_matrix );

/* For each point in the (alpha,delta) arrays, convert to Cartesian
   coordinates, apply the precession matrix, convert back to polar coordinates
   and then constrain the longitude result to lie in the range 0 to 2*pi
   (palDcc2s doesn't do this itself). */
                  TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ], vec1 );
                             palDmxv( precess_matrix, vec1, vec2 );
                             palDcc2s( vec2, alpha + point, delta + point );
                             alpha[ point ] = palDranrm( alpha[ point ] );)
	       }
               break;

/* Apply IAU 1975 (FK5) precession model. */
/* -------------------------------------- */
/* This is handled in the same way as above, but using the appropriate FK5
   precession matrix. */
            case AST__SLA_PREC:
               {
                  double epoch1 = forward ? args[ 0 ] : args[ 1 ];
                  double epoch2 = forward ? args[ 1 ] : args[ 0 ];
                  double precess_matrix[ 3 ][ 3 ];
                  double vec1[ 3 ];
                  double vec2[ 3 ];
                  palPrec( epoch1, epoch2, precess_matrix );
                  TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ], vec1 );
                             palDmxv( precess_matrix, vec1, vec2 );
                             palDcc2s( vec2, alpha + point, delta + point );
                             alpha[ point ] = palDranrm( alpha[ point ] );)
	       }
               break;

/* Convert FK4 to FK5 (no proper motion or parallax). */
/* -------------------------------------------------- */
/* Apply the conversion to each point. */
	    case AST__SLA_FK45Z:
               if ( forward ) {
                  TRAN_ARRAY(palFk45z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)

/* The inverse transformation is also straightforward, except that we need a
   couple of dummy variables as function arguments. */
	       } else {
                  double dr1950;
                  double dd1950;
                  TRAN_ARRAY(palFk54z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point,
                                       &dr1950, &dd1950 );)
	       }
               break;

/* Convert FK5 to FK4 (no proper motion or parallax). */
/* -------------------------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_FK54Z:
               if ( forward ) {
                  double dr1950;
                  double dd1950;
                  TRAN_ARRAY(palFk54z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point,
                                       &dr1950, &dd1950 );)
	       } else {
                  TRAN_ARRAY(palFk45z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
               }
               break;

/* Convert geocentric apparent to mean place. */
/* ------------------------------------------ */
/* Since we are transforming a sequence of points, first set up the required
   parameter array. Than apply this to each point in turn. */
	    case AST__SLA_AMP:
               {

                  if( !extra ) {

                     if( args[ 1 ] != eq_cache ||
                         args[ 0 ] != ep_cache ) {
                        eq_cache = args[ 1 ];
                        ep_cache = args[ 0 ];
                        palMappa( eq_cache, ep_cache, amprms_cache );
                     }

                     extra = astStore( NULL, amprms_cache,
                                       sizeof( double )*21 );
                     map->cvtextra[ cvt ] = extra;
                  }

                  if ( forward ) {
                     TRAN_ARRAY(palAmpqk( alpha[ point ], delta[ point ],
                                          extra,
                                          alpha + point, delta + point );)

/* The inverse uses the same parameter array but converts from mean place
   to geocentric apparent. */
                  } else {
                     TRAN_ARRAY(palMapqkz( alpha[ point ], delta[ point ],
                                           extra,
                                           alpha + point, delta + point );)
		  }
               }
               break;

/* Convert mean place to geocentric apparent. */
/* ------------------------------------------ */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_MAP:
               {
                  if( !extra ) {

                     if( args[ 0 ] != eq_cache ||
                         args[ 1 ] != ep_cache ) {
                        eq_cache = args[ 0 ];
                        ep_cache = args[ 1 ];
                        palMappa( eq_cache, ep_cache, amprms_cache );
                     }

                     extra = astStore( NULL, amprms_cache,
                                       sizeof( double )*21 );
                     map->cvtextra[ cvt ] = extra;
                  }

                  if ( forward ) {
                     TRAN_ARRAY(palMapqkz( alpha[ point ], delta[ point ],
                                           extra,
                                           alpha + point, delta + point );)
                  } else {
                     TRAN_ARRAY(palAmpqk( alpha[ point ], delta[ point ],
                                          extra,
                                          alpha + point, delta + point );)
		  }
               }
               break;

/* Convert ecliptic coordinates to J2000.0 equatorial. */
/* --------------------------------------------------- */
/* Since we are transforming a sequence of points, first set up the required
   conversion matrix (the conversion is a rotation). */
	    case AST__SLA_ECLEQ:
               {
                  double convert_matrix[ 3 ][ 3 ];
                  double precess_matrix[ 3 ][ 3 ];
                  double rotate_matrix[ 3 ][ 3 ];
                  double vec1[ 3 ];
                  double vec2[ 3 ];

/* Obtain the matrix that precesses equatorial coordinates from J2000.0 to the
   required date. Also obtain the rotation matrix that converts from
   equatorial to ecliptic coordinates.  */
                  palPrec( 2000.0, palEpj( args[ 0 ] ), precess_matrix );
                  palEcmat( args[ 0 ], rotate_matrix );

/* Multiply these matrices to give the overall matrix that converts from
   equatorial J2000.0 coordinates to ecliptic coordinates for the required
   date. */
                  palDmxm( rotate_matrix, precess_matrix, convert_matrix );

/* Apply the conversion by transforming from polar to Cartesian coordinates,
   multiplying by the inverse conversion matrix and converting back to polar
   coordinates. Then constrain the longitude result to lie in the range
   0 to 2*pi (palDcc2s doesn't do this itself). */
                  if ( forward ) {
                     TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ],
                                          vec1 );
                                palDimxv( convert_matrix, vec1, vec2 );
                                palDcc2s( vec2, alpha + point, delta + point );
                                alpha[ point ] = palDranrm ( alpha[ point ] );)

/* The inverse conversion is the same except that we multiply by the forward
   conversion matrix (palDmxv instead of palDimxv). */
                  } else {
                     TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ],
                                          vec1 );
                                palDmxv( convert_matrix, vec1, vec2 );
                                palDcc2s( vec2, alpha + point, delta + point );
                                alpha[ point ] = palDranrm ( alpha[ point ] );)
                  }
	       }
               break;

/* Convert equatorial J2000.0 to ecliptic coordinates. */
/* --------------------------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_EQECL:
               {
                  double convert_matrix[ 3 ][ 3 ];
                  double precess_matrix[ 3 ][ 3 ];
                  double rotate_matrix[ 3 ][ 3 ];
                  double vec1[ 3 ];
                  double vec2[ 3 ];

/* Create the conversion matrix. */
                  palPrec( 2000.0, palEpj( args[ 0 ] ), precess_matrix );
                  palEcmat( args[ 0 ], rotate_matrix );
                  palDmxm( rotate_matrix, precess_matrix, convert_matrix );

/* Apply it. */
                  if ( forward ) {
                     TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ],
                                          vec1 );
                                palDmxv( convert_matrix, vec1, vec2 );
                                palDcc2s( vec2, alpha + point, delta + point );
                                alpha[ point ] = palDranrm ( alpha[ point ] );)
                  } else {
                     TRAN_ARRAY(palDcs2c( alpha[ point ], delta[ point ],
                                          vec1 );
                                palDimxv( convert_matrix, vec1, vec2 );
                                palDcc2s( vec2, alpha + point, delta + point );
                                alpha[ point ] = palDranrm ( alpha[ point ] );)
                  }
	       }
               break;

/* Convert ICRS to J2000.0 equatorial. */
/* ----------------------------------- */
/* Apply the conversion to each point. */
	    case AST__SLA_HFK5Z:
               if ( forward ) {
                  double dr5;
                  double dd5;
                  TRAN_ARRAY(palHfk5z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point,
                                       &dr5, &dd5 );)

/* The inverse simply uses the inverse SLALIB function. */
	       } else {
                  TRAN_ARRAY(palFk5hz( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)
	       }
               break;

/* Convert J2000.0 to ICRS equatorial. */
/* ----------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_FK5HZ:
               if ( forward ) {
                  TRAN_ARRAY(palFk5hz( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point );)

/* The inverse simply uses the inverse SLALIB function. */
	       } else {
                  double dr5;
                  double dd5;
                  TRAN_ARRAY(palHfk5z( alpha[ point ], delta[ point ],
                                       args[ 0 ],
                                       alpha + point, delta + point,
                                       &dr5, &dd5 );)
	       }
               break;

/* Convert horizon to equatorial. */
/* ------------------------------ */
/* Apply the conversion to each point. */
	    case AST__SLA_DH2E:
               if ( forward ) {
                  TRAN_ARRAY(Dh2e( alpha[ point ], delta[ point ],
                                      args[ 0 ], args[ 1 ],
                                      alpha + point, delta + point, status );)

/* The inverse simply uses the inverse SLALIB function. */
	       } else {
                  TRAN_ARRAY(De2h( alpha[ point ], delta[ point ],
                                      args[ 0 ], args[ 1 ],
                                      alpha + point, delta + point, status );)
	       }
               break;

/* Convert equatorial to horizon. */
/* ------------------------------ */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_DE2H:
               if ( forward ) {
                  TRAN_ARRAY(De2h( alpha[ point ], delta[ point ],
                                      args[ 0 ], args[ 1 ],
                                      alpha + point, delta + point, status );)

/* The inverse simply uses the inverse SLALIB function. */
	       } else {
                  TRAN_ARRAY(Dh2e( alpha[ point ], delta[ point ],
                                      args[ 0 ], args[ 1 ],
                                      alpha + point, delta + point, status );)
	       }
               break;

/* Convert galactic coordinates to J2000.0 equatorial. */
/* --------------------------------------------------- */
/* Apply the conversion to each point. */
	    case AST__SLA_GALEQ:
               if ( forward ) {
                  TRAN_ARRAY(palGaleq( alpha[ point ], delta[ point ],
                                       alpha + point, delta + point );)

/* The inverse simply uses the inverse SLALIB function. */
	       } else {
                  TRAN_ARRAY(palEqgal( alpha[ point ], delta[ point ],
                                       alpha + point, delta + point );)
	       }
               break;

/* Convert J2000.0 equatorial to galactic coordinates. */
/* --------------------------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_EQGAL:
               if ( forward ) {
                  TRAN_ARRAY(palEqgal( alpha[ point ], delta[ point ],
                                       alpha + point, delta + point );)
	       } else {
                  TRAN_ARRAY(palGaleq( alpha[ point ], delta[ point ],
                                       alpha + point, delta + point );)
               }
               break;

/* Convert galactic to supergalactic coordinates. */
/* ---------------------------------------------- */
/* Apply the conversion to each point. */
	    case AST__SLA_GALSUP:
               if ( forward ) {
                  TRAN_ARRAY(palGalsup( alpha[ point ], delta[ point ],
                                        alpha + point, delta + point );)

/* The inverse simply uses the inverse SLALIB function. */
               } else {
                  TRAN_ARRAY(palSupgal( alpha[ point ], delta[ point ],
                                        alpha + point, delta + point );)
               }
               break;

/* Convert supergalactic coordinates to galactic. */
/* ---------------------------------------------- */
/* This is the same as above, but with the forward and inverse cases
   transposed. */
	    case AST__SLA_SUPGAL:
               if ( forward ) {
                  TRAN_ARRAY(palSupgal( alpha[ point ], delta[ point ],
                                        alpha + point, delta + point );)
               } else {
                  TRAN_ARRAY(palGalsup( alpha[ point ], delta[ point ],
                                        alpha + point, delta + point );)
               }
               break;

/* If the conversion type was not recognised, then report an error
   (this should not happen unless validation in astSlaAdd has failed
   to detect a bad value previously). */
            default:
               astError( AST__SLAIN, "astTransform(%s): Corrupt %s contains "
                         "invalid SLALIB sky coordinate conversion code (%d).", status,
                         astGetClass( this ), astGetClass( this ),
                         (int) ct );
               break;

/* Convert any STP coordinates to J2000 equatorial. */
/* ------------------------------------------------ */
            case AST__HPCEQ:
            case AST__HPREQ:
            case AST__HEEQ:
               {

/* Get the code for the appropriate 3D STP coordinate system to use.
   Also, get a point to the observer position, if needed. */
                  if( ct == AST__HPCEQ ) {
                    sys = AST__HPC;
                    obs = args + 1;

                  } else if( ct == AST__HPREQ ) {
                    sys = AST__HPR;
                    obs = args + 1;

                  } else {
                    sys = AST__GSE;
                    obs = NULL;

                  }

/* Store the 3D positions to be transformed. The supplied arrays are used
   for the longitude and latitude values. No radius values are supplied.
   (a value of 1AU will be used in the transformation). */
                  p[0] = alpha;
                  p[1] = delta;
                  p[2] = NULL;

/* Convert the supplied positions to (or from) AST__HEQ, ignoring the
   distinction between the origin of the input and output systems (which
   is appropriate since we are considering points at an infinite distance
   from the observer). */
                  if( forward ) {
                     STPConv( args[ 0 ], 1, npoint, sys, obs, p,
                              AST__HAQ, NULL, p, status );
                  } else {
                     STPConv( args[ 0 ], 1, npoint, AST__HAQ, NULL, p,
                              sys, obs, p, status );
                  }
	       }
               break;


/* Convert J2000 equatorial to any STP coordinates. */
/* ------------------------------------------------ */
/* Same as above, but with forward and inverse cases transposed. */
            case AST__EQHPC:
            case AST__EQHPR:
            case AST__EQHE:
               {

/* Get the code for the appropriate 3D STP coordinate system to use.
   Also, get a point to the observer position, if needed. */
                  if( ct == AST__EQHPC ) {
                    sys = AST__HPC;
                    obs = args + 1;

                  } else if( ct == AST__EQHPR ) {
                    sys = AST__HPR;
                    obs = args + 1;

                  } else {
                    sys = AST__GSE;
                    obs = NULL;

                  }

/* Store the 3D positions to be transformed. The supplied arrays are used
   for the longitude and latitude values. No radius values are supplied.
   (a value of 1AU will be used in the transformation). */
                  p[0] = alpha;
                  p[1] = delta;
                  p[2] = NULL;

/* Convert the supplied positions from (or to) AST__HEQ, ignoring the
   distinction between the origin of the input and output systems (which
   is appropriate since we are considering points at an infinite distance
   from the observer). */
                  if( forward ) {
                     STPConv( args[ 0 ], 1, npoint, AST__HAQ, NULL, p,
                              sys, obs, p, status );
                  } else {
                     STPConv( args[ 0 ], 1, npoint, sys, obs, p,
                              AST__HAQ, NULL, p, status );
                  }
	       }
               break;

/* Convert dynamical J2000.0 to ICRS. */
/* ---------------------------------- */
/* Apply the conversion to each point. */
	    case AST__J2000H:
               J2000H( forward, npoint, alpha, delta, status );
               break;

/* Convert ICRS to dynamical J2000.0  */
/* ---------------------------------- */
	    case AST__HJ2000:
               J2000H( !(forward), npoint, alpha, delta, status );
               break;

/* Convert HA to RA, or RA to HA */
/* ----------------------------- */
/* The forward and inverse transformations are the same. */
	    case AST__H2R:
	    case AST__R2H:
               TRAN_ARRAY( alpha[ point ] = args[ 0 ] - alpha[ point ]; )
               break;

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

/* Undefine macros local to this function. */
#undef TRAN_ARRAY
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for SlaMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for SlaMap objects.

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
   AstSlaMap *in;                /* Pointer to input SlaMap */
   AstSlaMap *out;               /* Pointer to output SlaMap */
   int cvt;                      /* Loop counter for coordinate conversions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output SlaMap structures. */
   in = (AstSlaMap *) objin;
   out = (AstSlaMap *) objout;

/* For safety, first clear any references to the input memory from the output
   SlaMap. */
   out->cvtargs = NULL;
   out->cvtextra = NULL;
   out->cvttype = NULL;

/* Allocate memory for the output array of argument list pointers. */
   out->cvtargs = astMalloc( sizeof( double * ) * (size_t) in->ncvt );

/* Allocate memory for the output array of extra (intermediate) values. */
   out->cvtextra = astMalloc( sizeof( double * ) * (size_t) in->ncvt );

/* If necessary, allocate memory and make a copy of the input array of sky
   coordinate conversion codes. */
   if ( in->cvttype ) out->cvttype = astStore( NULL, in->cvttype,
                                               sizeof( int )
                                               * (size_t) in->ncvt );

/* If OK, loop through each conversion in the input SlaMap and make a copy of
   its argument list, storing the new pointer in the output argument list
   array. */
   if ( astOK ) {
      for ( cvt = 0; cvt < in->ncvt; cvt++ ) {
         out->cvtargs[ cvt ] = astStore( NULL, in->cvtargs[ cvt ],
                                         astSizeOf( in->cvtargs[ cvt ] ) );
         out->cvtextra[ cvt ] = astStore( NULL, in->cvtextra[ cvt ],
                                         astSizeOf( in->cvtextra[ cvt ] ) );
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
      out->cvtextra = astFree( out->cvtextra );
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
*     Destructor for SlaMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for SlaMap objects.

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
   AstSlaMap *this;              /* Pointer to SlaMap */
   int cvt;                      /* Loop counter for coordinate conversions */

/* Obtain a pointer to the SlaMap structure. */
   this = (AstSlaMap *) obj;

/* Loop to free the memory containing the argument list for each sky coordinate
   conversion. */
   for ( cvt = 0; cvt < this->ncvt; cvt++ ) {
      this->cvtargs[ cvt ] = astFree( this->cvtargs[ cvt ] );
      this->cvtextra[ cvt ] = astFree( this->cvtextra[ cvt ] );
   }

/* Free the memory holding the array of conversion types and the array of
   argument list pointers. */
   this->cvtargs = astFree( this->cvtargs );
   this->cvtextra = astFree( this->cvtextra );
   this->cvttype = astFree( this->cvttype );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SlaMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SlaMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SlaMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstSlaMap *this;              /* Pointer to the SlaMap structure */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_SLA_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *sval;             /* Pointer to string value */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int ival;                     /* Integer value */
   int nargs;                    /* Number of conversion arguments */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SlaMap structure. */
   this = (AstSlaMap *) this_object;

/* Write out values representing the instance variables for the SlaMap
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
   astWriteInt( channel, "Nsla", set, 0, ival, "Number of conversion steps" );

/* Write out data for each conversion step... */
   for ( icvt = 0; icvt < this->ncvt; icvt++ ) {

/* Conversion type. */
/* ---------------- */
/* Change each conversion type code into an equivalent string and
   obtain associated descriptive information. If the conversion code
   was not recognised, report an error and give up. */
      if ( astOK ) {
         sval = CvtString( this->cvttype[ icvt ], &comment, &nargs, argdesc, status );
         if ( astOK && !sval ) {
            astError( AST__SLAIN,
                      "astWrite(%s): Corrupt %s contains invalid SLALIB "
                      "sky coordinate conversion code (%d).", status,
                      astGetClass( channel ), astGetClass( this ),
                      (int) this->cvttype[ icvt ] );
            break;
         }

/* Create an appropriate keyword and write out the conversion code
   information. */
         (void) sprintf( key, "Sla%d", icvt + 1 );
         astWriteString( channel, key, 1, 1, sval, comment );

/* Write out data for each conversion argument... */
         for ( iarg = 0; iarg < nargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and write out the argument value,
   accompanied by the descriptive comment obtained above. */
            (void) sprintf( key, "Sla%d%c", icvt + 1, ALPHABET[ iarg ] );
            astWriteDouble( channel, key, 1, 1, this->cvtargs[ icvt ][ iarg ],
                            argdesc[ iarg ] );
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
/* Implement the astIsASlaMap and astCheckSlaMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SlaMap,Mapping)
astMAKE_CHECK(SlaMap)

AstSlaMap *astSlaMap_( int flags, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astSlaMap
f     AST_SLAMAP

*  Purpose:
*     Create an SlaMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "slamap.h"
c     AstSlaMap *astSlaMap( int flags, const char *options, ... )
f     RESULT = AST_SLAMAP( FLAGS, OPTIONS, STATUS )

*  Class Membership:
*     SlaMap constructor.

*  Description:
*     This function creates a new SlaMap and optionally initialises
*     its attributes.
*
*     An SlaMap is a specialised form of Mapping which can be used to
*     represent a sequence of conversions between standard celestial
*     (longitude, latitude) coordinate systems.
*
*     When an SlaMap is first created, it simply performs a unit
c     (null) Mapping on a pair of coordinates. Using the astSlaAdd
f     (null) Mapping on a pair of coordinates. Using the AST_SLAADD
c     function, a series of coordinate conversion steps may then be
f     routine, a series of coordinate conversion steps may then be
*     added, selected from those provided by the SLALIB Positional
*     Astronomy Library (Starlink User Note SUN/67). This allows
*     multi-step conversions between a variety of celestial coordinate
*     systems to be assembled out of the building blocks provided by
*     SLALIB.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astSlaAdd function.
f     see the description of the AST_SLAADD routine.

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
c        initialising the new SlaMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SlaMap. The syntax used is identical to that for the
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
c     astSlaMap()
f     AST_SLAMAP = INTEGER
*        A pointer to the new SlaMap.

*  Notes:
*     - The Nin and Nout attributes (number of input and output
*     coordinates) for an SlaMap are both equal to 2. The first
*     coordinate is the celestial longitude and the second coordinate
*     is the celestial latitude. All coordinate values are in radians.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSlaMap *new;               /* Pointer to the new SlaMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SlaMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSlaMap( NULL, sizeof( AstSlaMap ), !class_init, &class_vtab,
                        "SlaMap", flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SlaMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new SlaMap. */
   return new;
}

AstSlaMap *astSlaMapId_( int flags, const char *options, ... ) {
/*
*  Name:
*     astSlaMapId_

*  Purpose:
*     Create an SlaMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "slamap.h"
*     AstSlaMap *astSlaMapId_( int flags, const char *options, ... )

*  Class Membership:
*     SlaMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astSlaMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astSlaMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astSlaMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astSlaMap_.

*  Returned Value:
*     The ID value associated with the new SlaMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSlaMap *new;               /* Pointer to the new SlaMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SlaMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSlaMap( NULL, sizeof( AstSlaMap ), !class_init, &class_vtab,
                        "SlaMap", flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SlaMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new SlaMap. */
   return astMakeId( new );
}

AstSlaMap *astInitSlaMap_( void *mem, size_t size, int init,
                           AstSlaMapVtab *vtab, const char *name,
                           int flags, int *status ) {
/*
*+
*  Name:
*     astInitSlaMap

*  Purpose:
*     Initialise an SlaMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "slamap.h"
*     AstSlaMap *astInitSlaMap( void *mem, size_t size, int init,
*                               AstSlaMapVtab *vtab, const char *name,
*                               int flags )

*  Class Membership:
*     SlaMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new SlaMap object. It allocates memory (if necessary) to accommodate
*     the SlaMap plus any additional data associated with the derived class.
*     It then initialises an SlaMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for an SlaMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SlaMap is to be initialised.
*        This must be of sufficient size to accommodate the SlaMap data
*        (sizeof(SlaMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the SlaMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the SlaMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the SlaMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new SlaMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astClass
*        method).
*     flags
*        This parameter is reserved for future use. It is currently ignored.

*  Returned Value:
*     A pointer to the new SlaMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstSlaMap *new;               /* Pointer to the new SlaMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSlaMapVtab( vtab, name );

/* Initialise a Mapping structure (the parent class) as the first component
   within the SlaMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstSlaMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       2, 2, 1, 1 );

   if ( astOK ) {

/* Initialise the SlaMap data. */
/* --------------------------- */
/* The initial state is with no SLALIB conversions set, in which condition the
   SlaMap simply implements a unit mapping. */
      new->ncvt = 0;
      new->cvtargs = NULL;
      new->cvtextra = NULL;
      new->cvttype = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstSlaMap *astLoadSlaMap_( void *mem, size_t size,
                           AstSlaMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadSlaMap

*  Purpose:
*     Load a SlaMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "slamap.h"
*     AstSlaMap *astLoadSlaMap( void *mem, size_t size,
*                               AstSlaMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     SlaMap loader.

*  Description:
*     This function is provided to load a new SlaMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SlaMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a SlaMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the SlaMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SlaMap data (sizeof(SlaMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SlaMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SlaMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSlaMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SlaMap. If this is NULL, a pointer to
*        the (static) virtual function table for the SlaMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SlaMap" is used instead.

*  Returned Value:
*     A pointer to the new SlaMap.

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
   AstSlaMap *new;               /* Pointer to the new SlaMap */
   char *sval;                   /* Pointer to string value */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_SLA_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int nargs;                    /* Number of conversion arguments */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SlaMap. In this case the
   SlaMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSlaMap );
      vtab = &class_vtab;
      name = "SlaMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSlaMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SlaMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SlaMap" );

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
      new->ncvt = astReadInt( channel, "nsla", 0 );
      if ( new->ncvt < 0 ) new->ncvt = 0;
      new->cvttype = astMalloc( sizeof( int ) * (size_t) new->ncvt );
      new->cvtargs = astMalloc( sizeof( double * ) * (size_t) new->ncvt );
      new->cvtextra = astMalloc( sizeof( double * ) * (size_t) new->ncvt );

/* If an error occurred, ensure that all allocated memory is freed. */
      if ( !astOK ) {
         new->cvttype = astFree( new->cvttype );
         new->cvtargs = astFree( new->cvtargs );
         new->cvtextra = astFree( new->cvtextra );

/* Otherwise, initialise the argument pointer array. */
      } else {
         for ( icvt = 0; icvt < new->ncvt; icvt++ ) {
            new->cvtargs[ icvt ] = NULL;
            new->cvtextra[ icvt ] = NULL;
         }

/* Read in data for each conversion step... */
         for ( icvt = 0; icvt < new->ncvt; icvt++ ) {

/* Conversion type. */
/* ---------------- */
/* Create an appropriate keyword and read the string representation of
   the conversion type. */
            (void) sprintf( key, "sla%d", icvt + 1 );
            sval = astReadString( channel, key, NULL );

/* If no value was read, report an error. */
            if ( astOK ) {
               if ( !sval ) {
                  astError( AST__BADIN,
                            "astRead(%s): An SLALIB sky coordinate conversion "
                            "type is missing from the input SlaMap data.", status,
                            astGetClass( channel ) );

/* Otherwise, convert the string representation into the required
   conversion type code. */
               } else {
                  new->cvttype[ icvt ] = CvtCode( sval, status );

/* If the string was not recognised, report an error. */
                  if ( new->cvttype[ icvt ] == AST__SLA_NULL ) {
                     astError( AST__BADIN,
                              "astRead(%s): Invalid SLALIB sky conversion "
                              "type \"%s\" in SlaMap data.", status,
                              astGetClass( channel ), sval );
                  }
               }

/* Free the memory holding the string value. */
               sval = astFree( sval );
            }

/* Obtain the number of arguments associated with the conversion and
   allocate memory to hold them. */
            (void) CvtString( new->cvttype[ icvt ], &comment, &nargs,
                              argdesc, status );
            new->cvtargs[ icvt ] = astMalloc( sizeof( double ) *
                                              (size_t) nargs );

/* Read in data for each argument... */
            if ( astOK ) {
               for ( iarg = 0; iarg < nargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and read each argument value. */
                  (void) sprintf( key, "sla%d%c", icvt + 1, ALPHABET[ iarg ] );
                  new->cvtargs[ icvt ][ iarg ] = astReadDouble( channel, key,
                                                                AST__BAD );
               }
            }

/* Quit looping if an error occurs. */
            if ( !astOK ) break;
         }
      }

/* If an error occurred, clean up by deleting the new SlaMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new SlaMap pointer. */
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
void astSlaAdd_( AstSlaMap *this, const char *cvt, const double args[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,SlaMap,SlaAdd))( this, cvt, args, status );
}





