/*
*class++
*  Name:
*     SpecMap

*  Purpose:
*     Sequence of spectral coordinate conversions.

*  Constructor Function:
c     astSpecMap (also see astSpecAdd)
f     AST_SPECMAP (also see AST_SPECADD)

*  Description:
*     A SpecMap is a specialised form of Mapping which can be used to
*     represent a sequence of conversions between standard spectral
*     coordinate systems.
*
*     When an SpecMap is first created, it simply performs a unit
c     (null) Mapping. Using the astSpecAdd
f     (null) Mapping. Using the AST_SPECADD
c     function, a series of coordinate conversion steps may then be
f     routine, a series of coordinate conversion steps may then be
*     added. This allows multi-step conversions between a variety of 
*     spectral coordinate systems to be assembled out of a set of building 
*     blocks.
*
*     Conversions are available to transform between standards of rest.
*     Such conversions need to know the source position as an RA and DEC.
*     This information can be supplied in the form of parameters for
*     the relevant conversions, in which case the SpecMap is 1-dimensional, 
*     simply transforming the spectral axis values. This means that the
*     same source position will always be used by the SpecMap. However, this 
*     may not be appropriate for an accurate description of a 3-D spectral
*     cube, where changes of spatial position can produce significant
*     changes in the Doppler shift introduced when transforming between
*     standards of rest. For this situation, a 3-dimensional SpecMap can
*     be created in which axes 2 and 3 correspond to the source RA and DEC
*     The SpecMap simply copies values for axes 2 and 3 from input to
*     output), but modifies axis 1 values (the spectral axis) appropriately.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astSpecAdd function.
f     see the description of the AST_SPECADD routine.

*  Inheritance:
*     The SpecMap class inherits from the Mapping class.

*  Attributes:
*     The SpecMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     In addition to those functions applicable to all Mappings, the
c     following function may also be applied to all SpecMaps:
f     In addition to those routines applicable to all Mappings, the
f     following routine may also be applied to all SpecMaps:
*
c     - astSpecAdd: Add a celestial coordinate conversion to an SpecMap
f     - AST_SPECADD: Add a celestial coordinate conversion to an SpecMap

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     6-NOV-2002 (DSB):
*        Original version.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SpecMap

/* Codes to identify spectral coordinate conversions. */
#define AST__SPEC_NULL   0       /* Null value */
#define AST__FRTOVL      1       /* Frequency to relativistic velocity */
#define AST__VLTOFR      2       /* Relativistic velocity to Frequency */
#define AST__ENTOFR      3       /* Energy to frequency */
#define AST__FRTOEN      4       /* Frequency to energy */
#define AST__WNTOFR      5       /* Wave number to frequency */
#define AST__FRTOWN      6       /* Frequency to wave number */
#define AST__WVTOFR      7       /* Wavelength (vacuum) to frequency */
#define AST__FRTOWV      8       /* Frequency to wavelength (vacuum) */
#define AST__AWTOFR      9       /* Wavelength (air) to frequency */
#define AST__FRTOAW     10       /* Frequency to wavelength (air) */
#define AST__VRTOVL     11       /* Radio to relativistic velocity */
#define AST__VLTOVR     12       /* Relativistic to radio velocity */
#define AST__VOTOVL     13       /* Optical to relativistic velocity */
#define AST__VLTOVO     14       /* Relativistic to optical velocity */
#define AST__ZOTOVL     15       /* Redshift to relativistic velocity */
#define AST__VLTOZO     16       /* Relativistic velocity to redshift */
#define AST__BTTOVL     17       /* Beta factor to relativistic velocity */
#define AST__VLTOBT     18       /* Relativistic velocity to beta factor */
#define AST__TPV2HL     19       /* Topocentric to heliocentric velocity */
#define AST__HLV2TP     20       /* Heliocentric to topocentric velocity */
#define AST__GEV2HL     21       /* Geocentric to heliocentric velocity */
#define AST__HLV2GE     22       /* Heliocentric to geocentric velocity */
#define AST__BYV2HL     23       /* Barycentric to heliocentric velocity */
#define AST__HLV2BY     24       /* Heliocentric to barycentric velocity */
#define AST__LKV2HL     25       /* LSRK to heliocentric velocity */
#define AST__HLV2LK     26       /* Heliocentric to LSRK velocity */
#define AST__LDV2HL     27       /* LSRD to heliocentric velocity */
#define AST__HLV2LD     28       /* Heliocentric to LSRD velocity */
#define AST__LGV2HL     29       /* Local group to heliocentric velocity */
#define AST__HLV2LG     30       /* Heliocentric to local group velocity */
#define AST__GLV2HL     31       /* Galactic to heliocentric velocity */
#define AST__HLV2GL     32       /* Heliocentric to galactic velocity */
#define AST__TPF2HL     33       /* Topocentric to heliocentric frequency  */
#define AST__HLF2TP     34       /* Heliocentric to topocentric frequency */
#define AST__GEF2HL     35       /* Geocentric to heliocentric frequency */
#define AST__HLF2GE     36       /* Heliocentric to geocentric frequency */
#define AST__BYF2HL     37       /* Barycentric to heliocentric frequency */
#define AST__HLF2BY     38       /* Heliocentric to barycentric frequency */
#define AST__LKF2HL     39       /* LSRK to heliocentric frequency */
#define AST__HLF2LK     40       /* Heliocentric to LSRK frequency */
#define AST__LDF2HL     41       /* LSRD to heliocentric frequency */
#define AST__HLF2LD     42       /* Heliocentric to LSRD frequency */
#define AST__LGF2HL     43       /* Local group to heliocentric frequency */
#define AST__HLF2LG     44       /* Heliocentric to local group frequency */
#define AST__GLF2HL     45       /* Galactic to heliocentric frequency */
#define AST__HLF2GL     46       /* Heliocentric to galactic frequency */

/* Maximum number of arguments required by a conversion. */
#define MAX_ARGS 6

/* The alphabet (used for generating keywords for arguments). */
#define ALPHABET "abcdefghijklmnopqrstuvwxyz"

/* Angle conversion */
#define PI 3.141592653589793238462643
#define PIBY2 (PI/2.0)
#define D2R (PI/180.0)
#define R2D (180.0/PI)

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
#include "slalib.h"              /* SLALIB interface */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "unitmap.h"             /* Unit (null) Mappings */
#include "specmap.h"             /* Interface definition for this class */

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
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSpecMapVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet * );

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSpecMap *astSpecMapId_( int, int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet * );
static const char *CvtString( int, const char **, int *, int *, int *, int *, const char *[ MAX_ARGS ] );
static double Refrac( double );
static double HelioVel( double, double, double );
static double BaryVel( double, double, double );
static double TopoVel( double, double, double, double, double );
static int CvtCode( const char * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int ** );
static void (*GetCvtFunc( int ))( int, double *, double *, double *, double *, int );
static void AddSpecCvt( AstSpecMap *, int, const double * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void SpecAdd( AstSpecMap *, const char *, const double[] );
static void VelCorr( double *, double );
static void awtofr( int, double *, double *, double *, double *, int ); 
static void bttovl( int, double *, double *, double *, double *, int ); 
static void byf2hl( int, double *, double *, double *, double *, int ); 
static void byv2hl( int, double *, double *, double *, double *, int ); 
static void entofr( int, double *, double *, double *, double *, int ); 
static void frtoaw( int, double *, double *, double *, double *, int ); 
static void frtoen( int, double *, double *, double *, double *, int ); 
static void frtovl( int, double *, double *, double *, double *, int ); 
static void frtown( int, double *, double *, double *, double *, int ); 
static void frtowv( int, double *, double *, double *, double *, int ); 
static void gef2hl( int, double *, double *, double *, double *, int ); 
static void gev2hl( int, double *, double *, double *, double *, int ); 
static void glf2hl( int, double *, double *, double *, double *, int ); 
static void glv2hl( int, double *, double *, double *, double *, int ); 
static void hlf2by( int, double *, double *, double *, double *, int ); 
static void hlf2ge( int, double *, double *, double *, double *, int ); 
static void hlf2gl( int, double *, double *, double *, double *, int ); 
static void hlf2ld( int, double *, double *, double *, double *, int ); 
static void hlf2lg( int, double *, double *, double *, double *, int ); 
static void hlf2lk( int, double *, double *, double *, double *, int ); 
static void hlf2tp( int, double *, double *, double *, double *, int ); 
static void hlv2by( int, double *, double *, double *, double *, int ); 
static void hlv2ge( int, double *, double *, double *, double *, int ); 
static void hlv2gl( int, double *, double *, double *, double *, int ); 
static void hlv2ld( int, double *, double *, double *, double *, int ); 
static void hlv2lg( int, double *, double *, double *, double *, int ); 
static void hlv2lk( int, double *, double *, double *, double *, int ); 
static void hlv2tp( int, double *, double *, double *, double *, int ); 
static void ldf2hl( int, double *, double *, double *, double *, int ); 
static void ldv2hl( int, double *, double *, double *, double *, int ); 
static void lgf2hl( int, double *, double *, double *, double *, int ); 
static void lgv2hl( int, double *, double *, double *, double *, int ); 
static void lkf2hl( int, double *, double *, double *, double *, int ); 
static void lkv2hl( int, double *, double *, double *, double *, int ); 
static void tpf2hl( int, double *, double *, double *, double *, int ); 
static void tpv2hl( int, double *, double *, double *, double *, int ); 
static void vltobt( int, double *, double *, double *, double *, int ); 
static void vltofr( int, double *, double *, double *, double *, int ); 
static void vltovo( int, double *, double *, double *, double *, int ); 
static void vltovr( int, double *, double *, double *, double *, int ); 
static void vltozo( int, double *, double *, double *, double *, int ); 
static void votovl( int, double *, double *, double *, double *, int ); 
static void vrtovl( int, double *, double *, double *, double *, int ); 
static void wntofr( int, double *, double *, double *, double *, int ); 
static void wvtofr( int, double *, double *, double *, double *, int ); 
static void zotovl( int, double *, double *, double *, double *, int ); 



/* Member functions. */
/* ================= */
static void AddSpecCvt( AstSpecMap *this, int cvttype, const double *args ) {
/*
*  Name:
*     AddSpecCvt

*  Purpose:
*     Add a coordinate conversion step to an SpecMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     void AddSpecCvt( AstSpecMap *this, int cvttype, const double *args )

*  Class Membership:
*     SpecMap member function.

*  Description:
*     This function allows one of the supported spectral coordinate
*     conversions to be appended to a SpecMap. When a SpecMap is first
*     created (using astSpecMap), it simply performs a unit mapping. By
*     using AddSpecCvt repeatedly, a series of coordinate conversions may
*     then be specified which the SpecMap will subsequently perform in
*     sequence. This allows a complex coordinate conversion to be
*     assembled out of the basic building blocks. The SpecMap will also
*     perform the inverse coordinate conversion (applying the individual
*     conversion steps in reverse) if required.

*  Parameters:
*     this
*        Pointer to the SpecMap.
*     cvttype
*        A code to identify which spectral coordinate conversion is to be
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
*        AST__FRTOVL( RF )
*           Convert frequency to relativistic velocity.
*        AST__VLTOFR( RF )
*           Convert relativistic velocity to Frequency.
*        AST__ENTOFR      
*           Convert energy to frequency.
*        AST__FRTOEN      
*           Convert frequency to energy.
*        AST__WNTOFR      
*           Convert wave number to frequency.
*        AST__FRTOWN      
*           Convert frequency to wave number.
*        AST__WVTOFR      
*           Convert wavelength (vacuum) to frequency.
*        AST__FRTOWV      
*           Convert frequency to wavelength (vacuum).
*        AST__AWTOFR     
*           Convert wavelength (air) to frequency.
*        AST__FRTOAW     
*           Convert frequency to wavelength (air).
*        AST__VRTOVL     
*           Convert radio to relativistic velocity.
*        AST__VLTOVR     
*           Convert relativistic to radio velocity.
*        AST__VOTOVL     
*           Convert optical to relativistic velocity.
*        AST__VLTOVO     
*           Convert relativistic to optical velocity.
*        AST__ZOTOVL     
*           Convert redshift to relativistic velocity.
*        AST__VLTOZO     
*           Convert relativistic velocity to redshift.
*        AST__BTTOVL     
*           Convert beta factor to relativistic velocity.
*        AST__VLTOBT     
*           Convert relativistic velocity to beta factor.
*        AST__TPV2HL( GLON, GLAT, EPOCH, RA, DEC )
*           Convert from Topocentric to heliocentric relativistic velocity.
*        AST__HLV2TP( GLON, GLAT, EPOCH, RA, DEC )
*           Convert from Heliocentric to topocentric relativistic velocity.
*        AST__GEV2HL( EPOCH, RA, DEC )
*           Convert from Geocentric to heliocentric relativistic velocity.
*        AST__HLV2GE( EPOCH, RA, DEC )
*           Convert from Heliocentric to geocentric relativistic velocity.
*        AST__BYV2HL( EPOCH, RA, DEC )
*           Convert from Barycentric to heliocentric relativistic velocity.
*        AST__HLV2BY( EPOCH, RA, DEC )
*           Convert from Heliocentric to barycentric relativistic velocity.
*        AST__LKV2HL( RA, DEC )
*           Convert from LSRK to heliocentric relativistic velocity.
*        AST__HLV2LK( RA, DEC )
*           Convert from Heliocentric to LSRK relativistic velocity.
*        AST__LDV2HL( RA, DEC )
*           Convert from LSRD to heliocentric relativistic velocity.
*        AST__HLV2LD( RA, DEC )
*           Convert from Heliocentric to LSRD relativistic velocity.
*        AST__LGV2HL( RA, DEC )
*           Convert from Local group to heliocentric relativistic velocity.
*        AST__HLV2LG( RA, DEC )
*           Convert from Heliocentric to local group relativistic velocity.
*        AST__GLV2HL( RA, DEC )
*           Convert from Galactic to heliocentric relativistic velocity.
*        AST__HLV2GL( RA, DEC )
*           Convert from Heliocentric to galactic relativistic velocity.
*        AST__TPF2HL( GLON, GLAT, EPOCH, RA, DEC )
*           Convert from Topocentric to heliocentric frequency
*        AST__HLF2TP( GLON, GLAT, EPOCH, RA, DEC )
*           Convert from Heliocentric to topocentric frequency.
*        AST__GEF2HL( EPOCH, RA, DEC )
*           Convert from Geocentric to heliocentric frequency.
*        AST__HLF2GE( EPOCH, RA, DEC )
*           Convert from Heliocentric to geocentric frequency.
*        AST__BYF2HL( EPOCH, RA, DEC )
*           Convert from Barycentric to heliocentric frequency.
*        AST__HLF2BY( EPOCH, RA, DEC )
*           Convert from Heliocentric to barycentric frequency.
*        AST__LKF2HL( RA, DEC )
*           Convert from LSRK to heliocentric frequency.
*        AST__HLF2LK( RA, DEC )
*           Convert from Heliocentric to LSRK frequency.
*        AST__LDF2HL( RA, DEC )
*           Convert from LSRD to heliocentric frequency.
*        AST__HLF2LD( RA, DEC )
*           Convert from Heliocentric to LSRD frequency.
*        AST__LGF2HL( RA, DEC )
*           Convert from Local group to heliocentric frequency.
*        AST__HLF2LG( RA, DEC )
*           Convert from Heliocentric to local group frequency.
*        AST__GLF2HL( RA, DEC )
*           Convert from Galactic to heliocentric frequency.
*        AST__HLF2GL( RA, DEC )
*           Convert from Heliocentric to galactic frequency.
*
*     The units for the values processed by the above conversions are as
*     follows: 
*
*     - all velocities: metres per second.
*     - frequency: Hertz.
*     - all wavelengths: metres.
*     - energy: Joules.
*     - wave number: cycles per metre.
*
*     The arguments used in the above conversions are as follows:
*
*     - RF: Rest frequency (Hz).
*     - GLAT: Latitude of observer (radians, geodetic).
*     - GLON: Longitude of observer (radians, positive eastwards).
*     - EPOCH: Epoch of observation (UT1 expressed as a Modified Julian Date).
*     - RA: Right Ascension of source (radians, FK5 J2000).
*     - DEC: Declination of source (radians, FK5 J2000).
*
*     The RA and DEC values are ignored if the SpecMap is 3-dimensional
*     (the souce position is instead supplied as values for axes 2 and 3
*     of the SpecMap). However, values must still be supplied even if
*     they are to be ignored (they can be AST__BAD).

*  Notes:
*     - The specified conversion is appended only if the SpecMap's
*     Invert attribute is zero. If it is non-zero, this function
*     effectively prefixes the inverse of the conversion specified
*     instead.
*/

/* Local Variables: */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *cvt_string;       /* Pointer to conversion type string */
   int argdec;                   /* Index of DEC argument */
   int argra;                    /* Index of RA argument */
   int i;                        /* Argument index */
   int nargs;                    /* Number of user-supplied arguments */
   int ncvt;                     /* Number of coordinate conversions */
   int szargs;                   /* Size of arguments array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the coordinate conversion type and obtain the number of
   required user-supplied arguments, and the size of the array in which
   to put the user-supplied arguments (the array meay leave room after
   the user-supplied arguments for various useful pre-calculated values). */
   cvt_string = CvtString( cvttype, &comment, &argra, &argdec, &nargs, 
                           &szargs, argdesc );

/* If the coordinate conversion type was not valid, then report an
   error. */
   if ( astOK && !cvt_string ) {
      astError( AST__SPCIN,
                "Invalid spectral coordinate conversion type (%d).",
                astGetClass( this ), (int) cvttype );
   }

/* Note the number of coordinate conversions already stored in the SpecMap. */
   if ( astOK ) {
      ncvt = this->ncvt;

/* Extend the array of conversion types and the array of pointers to
   their argument lists to accommodate the new one. */
      this->cvttype = (int *) astGrow( this->cvttype, ncvt + 1,
                                       sizeof( int ) );
      this->cvtargs = (double **) astGrow( this->cvtargs, ncvt + 1,
                                           sizeof( double * ) );

/* If OK, allocate memory and store a copy of the argument list,
   putting a pointer to the copy into the SpecMap. */
      if ( astOK ) {
         this->cvtargs[ ncvt ] = astStore( NULL, args,
                                           sizeof( double ) * (size_t) szargs );
      }

/* Store the conversion type and increment the conversion count. Also put
   AST__BAD in any elements of the argument array which are beyond the
   end of the user-supplied arguments. These will be used to hold
   intermediate values calculated on the basis of the user-supplied
   arguments. Also set any source RA and DEC arguments to AST__BAD if the 
   SpecMap is 3-dimensional. */
      if ( astOK ) {
         this->cvttype[ ncvt ] = cvttype;
         this->ncvt++;
         for( i = nargs; i < szargs; i++ ) this->cvtargs[ ncvt ][ i ] = AST__BAD;

         if( astGetNin( this ) == 3 ) {
            if( argra != -1 ) this->cvtargs[ ncvt ][ argra ] = AST__BAD;
            if( argdec != -1 ) this->cvtargs[ ncvt ][ argdec ] = AST__BAD;
         }

      }
   }
}

static double BaryVel( double epoch, double ra, double dec ){
/*
*  Name:
*     BaryVel

*  Purpose:
*     Returns the heliocentric velocity component of the sun-earth barycentre.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     double BaryVel( double epoch, double ra, double dec )

*  Class Membership:
*     SpecMap method.

*  Description:
*     This function returns the component away from the source, of the 
*     velocity of the sun-earth barycentre relative to the sun (in km/s). 

*  Parameters:
*    epoch 
*       Epoch of observation (UT1 expressed as a Modified Julian Date)
*    ra 
*       Right Ascension of source (radians, FK5 J2000)
*    dec
*       Declination of source (radians, FK5 J2000)

*  Returned Value:
*     The required velocity (km/s).

*/

/* Local Variables: */
   double dpb[ 3 ];          /* Barycentric earth position vector */
   double dph[ 3 ];          /* Heliocentric earth position vector */
   double dvb[ 3 ];          /* Barycentric earth velocity vector */
   double dvh[ 3 ];          /* Heliocentric earth velocity vector */
   double v[ 3 ];            /* Source direction vector */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Get the Cartesian vector towards the source, in the Cartesian FK5
   J2000 system. */
   slaDcs2c( ra, dec, v );

/* Get the Earth/Sun velocity and position vectors in the same system. */
   slaEvp( epoch, 2000.0, dvb, dpb, dvh, dph );
  
/* Change the heliocentric velocity of the earth into the heliocentric 
   velocity of the barycentre. */
   dvh[ 0 ] -= dvb[ 0 ];
   dvh[ 1 ] -= dvb[ 1 ];
   dvh[ 2 ] -= dvb[ 2 ];

/* Return the component away from the source, of the velocity of the
   barycentre relative to the sun (in km/s). */
   return -slaDvdv( v, dvh )*149.597870E6;

}

static int CvtCode( const char *cvt_string ) {
/*
*  Name:
*     CvtCode

*  Purpose:
*     Convert a conversion type from a string representation to a code value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     int CvtCode( const char *cvt_string )

*  Class Membership:
*     SpecMap member function.

*  Description:
*     This function accepts a string used to repersent one of the
*     SpecMap coordinate conversions and converts it into a code
*     value for internal use.

*  Parameters:
*     cvt_string
*        Pointer to a constant null-terminated string representing a
*        spectral coordinate conversion. This is case sensitive and should
*        contain no unnecessary white space.

*  Returned Value:
*     The equivalent conversion code. If the string was not
*     recognised, the code AST__SPEC_NULL is returned, without error.

*  Notes:
*     - A value of AST__SPEC_NULL will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   int result;                   /* Result value to return */

/* Initialise. */
   result = AST__SPEC_NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Test the string against each recognised value in turn and assign
   the result. */
   if ( astChrMatch( cvt_string, "FRTOVL" ) ) {
      result = AST__FRTOVL;

   } else if ( astChrMatch( cvt_string, "VLTOFR" ) ) {
      result = AST__VLTOFR;

   } else if ( astChrMatch( cvt_string, "VLTOFR" ) ) { 
      result = AST__VLTOFR; 

   } else if ( astChrMatch( cvt_string, "ENTOFR" ) ) { 
      result = AST__ENTOFR; 

   } else if ( astChrMatch( cvt_string, "FRTOEN" ) ) { 
      result = AST__FRTOEN; 

   } else if ( astChrMatch( cvt_string, "WNTOFR" ) ) { 
      result = AST__WNTOFR; 

   } else if ( astChrMatch( cvt_string, "FRTOWN" ) ) { 
      result = AST__FRTOWN; 

   } else if ( astChrMatch( cvt_string, "WVTOFR" ) ) { 
      result = AST__WVTOFR; 

   } else if ( astChrMatch( cvt_string, "FRTOWV" ) ) { 
      result = AST__FRTOWV; 

   } else if ( astChrMatch( cvt_string, "AWTOFR" ) ) { 
      result = AST__AWTOFR; 

   } else if ( astChrMatch( cvt_string, "FRTOAW" ) ) { 
      result = AST__FRTOAW; 

   } else if ( astChrMatch( cvt_string, "VRTOVL" ) ) { 
      result = AST__VRTOVL; 

   } else if ( astChrMatch( cvt_string, "VLTOVR" ) ) { 
      result = AST__VLTOVR; 

   } else if ( astChrMatch( cvt_string, "VOTOVL" ) ) { 
      result = AST__VOTOVL; 

   } else if ( astChrMatch( cvt_string, "VLTOVO" ) ) { 
      result = AST__VLTOVO; 

   } else if ( astChrMatch( cvt_string, "ZOTOVL" ) ) { 
      result = AST__ZOTOVL; 

   } else if ( astChrMatch( cvt_string, "VLTOZO" ) ) { 
      result = AST__VLTOZO; 

   } else if ( astChrMatch( cvt_string, "BTTOVL" ) ) { 
      result = AST__BTTOVL; 

   } else if ( astChrMatch( cvt_string, "VLTOBT" ) ) { 
      result = AST__VLTOBT; 

   } else if ( astChrMatch( cvt_string, "TPV2HL" ) ) { 
      result = AST__TPV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2TP" ) ) { 
      result = AST__HLV2TP; 

   } else if ( astChrMatch( cvt_string, "GEV2HL" ) ) { 
      result = AST__GEV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2GE" ) ) { 
      result = AST__HLV2GE; 

   } else if ( astChrMatch( cvt_string, "BYV2HL" ) ) { 
      result = AST__BYV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2BY" ) ) { 
      result = AST__HLV2BY; 

   } else if ( astChrMatch( cvt_string, "LKV2HL" ) ) { 
      result = AST__LKV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2LK" ) ) { 
      result = AST__HLV2LK; 

   } else if ( astChrMatch( cvt_string, "LDV2HL" ) ) { 
      result = AST__LDV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2LD" ) ) { 
      result = AST__HLV2LD; 

   } else if ( astChrMatch( cvt_string, "LGV2HL" ) ) { 
      result = AST__LGV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2LG" ) ) { 
      result = AST__HLV2LG; 

   } else if ( astChrMatch( cvt_string, "GLV2HL" ) ) { 
      result = AST__GLV2HL; 

   } else if ( astChrMatch( cvt_string, "HLV2GL" ) ) { 
      result = AST__HLV2GL; 

   } else if ( astChrMatch( cvt_string, "TPF2HL" ) ) { 
      result = AST__TPF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2TP" ) ) { 
      result = AST__HLF2TP; 

   } else if ( astChrMatch( cvt_string, "GEF2HL" ) ) { 
      result = AST__GEF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2GE" ) ) { 
      result = AST__HLF2GE; 

   } else if ( astChrMatch( cvt_string, "BYF2HL" ) ) { 
      result = AST__BYF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2BY" ) ) { 
      result = AST__HLF2BY; 

   } else if ( astChrMatch( cvt_string, "LKF2HL" ) ) { 
      result = AST__LKF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2LK" ) ) { 
      result = AST__HLF2LK; 

   } else if ( astChrMatch( cvt_string, "LDF2HL" ) ) { 
      result = AST__LDF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2LD" ) ) { 
      result = AST__HLF2LD; 

   } else if ( astChrMatch( cvt_string, "LGF2HL" ) ) { 
      result = AST__LGF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2LG" ) ) { 
      result = AST__HLF2LG; 

   } else if ( astChrMatch( cvt_string, "GLF2HL" ) ) { 
      result = AST__GLF2HL; 

   } else if ( astChrMatch( cvt_string, "HLF2GL" ) ) { 
      result = AST__HLF2GL; 

   }

/* Return the result. */
   return result;
}

static const char *CvtString( int cvt_code, const char **comment,
                              int *argra, int *argdec, int *nargs, int *szargs,
                              const char *arg[ MAX_ARGS ] ) {
/*
*  Name:
*     CvtString

*  Purpose:
*     Convert a conversion type from a code value to a string representation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     const char *CvtString( int cvt_code, const char **comment,
*                            int *argra, int *argdec, int *nargs, 
*                            int *szargs, const char *arg[ MAX_ARGS ] )

*  Class Membership:
*     SpecMap member function.

*  Description:
*     This function accepts a code value used to represent one of the
*     SpecMap coordinate conversions and converts it into an
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
*     argra
*        Address of an int in which to return the index of the argument
*        corresponding to the source RA. Returned equal to -1 if the
*        conversion does not have a source RA argument.
*     argdec
*        Address of an int in which to return the index of the argument
*        corresponding to the source DEC. Returned equal to -1 if the
*        conversion does not have a source DEC argument.
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
   *argra = -1;
   *argdec = -1;
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;
      
/* Test for each valid code value in turn and assign the appropriate
   return values. */
   switch ( cvt_code ) {

   case AST__FRTOVL:
      *comment = "Convert frequency to rel. velocity";
      result = "FRTOVL";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "Rest frequency (Hz)";
      break;

   case AST__VLTOFR:
      *comment = "Convert rel. velocity to frequency";
      result = "VLTOFR";
      *nargs = 1;
      *szargs = 1;
      arg[ 0 ] = "Rest frequency (Hz)";
      break;

   case AST__ENTOFR:
      *comment = "Convert energy to frequency";
      result = "ENTOFR";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__FRTOEN:
      *comment = "Convert frequency to energy";
      result = "FRTOEN";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__WNTOFR:
      *comment = "Convert wave number to frequency";
      result = "WNTOFR";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__FRTOWN:
      *comment = "Convert frequency to wave number";
      result = "FRTOWN";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__WVTOFR:
      *comment = "Convert wavelength (vacuum) to frequency";
      result = "WVTOFR";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__FRTOWV:
      *comment = "Convert fFrequency to wavelength (vacuum)";
      result = "FRTOWV";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__AWTOFR:
      *comment = "Convert wavelength (air) to frequency";
      result = "AWTOFR";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__FRTOAW:
      *comment = "Convert frequency to wavelength (air)";
      result = "FRTOAW";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VRTOVL:
      *comment = "Convert radio to rel. velocity";
      result = "VRTOVL";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VLTOVR:
      *comment = "Convert relativistic to radio velocity";
      result = "VLTOVR";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VOTOVL:
      *comment = "Convert optical to rel. velocity";
      result = "VOTOVL";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VLTOVO:
      *comment = "Convert relativistic to optical velocity";
      result = "VLTOVO";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__ZOTOVL:
      *comment = "Convert redshift to rel. velocity";
      result = "ZOTOVL";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VLTOZO:
      *comment = "Convert rel. velocity to redshift";
      result = "VLTOZO";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__BTTOVL:
      *comment = "Convert beta factor to rel. velocity";
      result = "BTTOVL";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__VLTOBT:
      *comment = "Convert rel. velocity to beta factor";
      result = "VLTOBT";
      *nargs = 0;
      *szargs = 0;
      break;

   case AST__TPV2HL:
      *comment = "Convert from Topocentric to heliocentric rel. velocity";
      result = "TPV2HL";
      *nargs = 5;
      *argra = 3;
      *argdec = 4;
      *szargs = 6;
      arg[ 0 ] = "Longitude (positive eastwards, radians)";
      arg[ 1 ] = "Latitude (geodetic, radians)";
      arg[ 2 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 3 ] = "RA of source (FK5 J2000, radians)";
      arg[ 4 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 5 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2TP:
      *comment = "Convert from Heliocentric to topocentric rel. velocity";
      result = "HLV2TP";
      *nargs = 5;
      *argra = 3;
      *argdec = 4;
      *szargs = 6;
      arg[ 0 ] = "Longitude (positive eastwards, radians)";
      arg[ 1 ] = "Latitude (geodetic, radians)";
      arg[ 2 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 3 ] = "RA of source (FK5 J2000, radians)";
      arg[ 4 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 5 ] = "Velocity correction term (m/s)";
      break;

   case AST__GEV2HL:
      *comment = "Convert from Geocentric to heliocentric rel. velocity";
      result = "GEV2HL";
      *nargs = 3;
      *argra = 1;
      *argdec = 2;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2GE:
      *comment = "Convert from Heliocentric to geocentric rel. velocity";
      result = "HLV2GE";
      *nargs = 3;
      *argra = 1;
      *argdec = 2;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Velocity correction term (m/s)";
      break;

   case AST__BYV2HL:
      *comment = "Convert from Barycentric to heliocentric rel. velocity";
      result = "BYV2HL";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2BY:
      *comment = "Convert from Heliocentric to barycentric rel. velocity";
      result = "HLV2BY";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Velocity correction term (m/s)";
      break;

   case AST__LKV2HL:
      *comment = "Convert from LSRK to heliocentric rel. velocity";
      result = "LKV2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2LK:
      *comment = "Convert from Heliocentric to LSRK rel. velocity";
      result = "HLV2LK";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__LDV2HL:
      *comment = "Convert from LSRD to heliocentric rel. velocity";
      result = "LDV2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2LD:
      *comment = "Convert from Heliocentric to LSRD rel. velocity";
      result = "HLV2LD";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__LGV2HL:
      *comment = "Convert from Local group to heliocentric rel. velocity";
      result = "LGV2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2LG:
      *comment = "Convert from Heliocentric to local group rel. velocity";
      result = "HLV2LG";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__GLV2HL:
      *comment = "Convert from Galactic to heliocentric rel. velocity";
      result = "GLV2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__HLV2GL:
      *comment = "Convert from Heliocentric to galactic rel. velocity";
      result = "HLV2GL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Velocity correction term (m/s)";
      break;

   case AST__TPF2HL:
      *comment = "Convert from Topocentric to heliocentric frequency";
      result = "TPF2HL";
      *argra = 3;
      *argdec = 4;
      *nargs = 5;
      *szargs = 6;
      arg[ 0 ] = "Longitude (positive eastwards, radians)";
      arg[ 1 ] = "Latitude (geodetic, radians)";
      arg[ 2 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 3 ] = "RA of source (FK5 J2000, radians)";
      arg[ 4 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 5 ] = "Frequency correction factor";
      break;

   case AST__HLF2TP:
      *comment = "Convert from Heliocentric to topocentric frequency";
      result = "HLF2TP";
      *argra = 3;
      *argdec = 4;
      *nargs = 5;
      *szargs = 6;
      arg[ 0 ] = "Longitude (positive eastwards, radians)";
      arg[ 1 ] = "Latitude (geodetic, radians)";
      arg[ 2 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 3 ] = "RA of source (FK5 J2000, radians)";
      arg[ 4 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 5 ] = "Frequency correction factor";
      break;

   case AST__GEF2HL:
      *comment = "Convert from Geocentric to heliocentric frequency";
      result = "GEF2HL";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Frequency correction factor";
      break;

   case AST__HLF2GE:
      *comment = "Convert from Heliocentric to geocentric frequency";
      result = "HLF2GE";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Frequency correction factor";
      break;

   case AST__BYF2HL:
      *comment = "Convert from Barycentric to heliocentric frequency";
      result = "BYF2HL";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Frequency correction factor";
      break;

   case AST__HLF2BY:
      *comment = "Convert from Heliocentric to barycentric frequency";
      result = "HLF2BY";
      *argra = 1;
      *argdec = 2;
      *nargs = 3;
      *szargs = 4;
      arg[ 0 ] = "UT1 epoch of observaton (Modified Julian Date)";
      arg[ 1 ] = "RA of source (FK5 J2000, radians)";
      arg[ 2 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 3 ] = "Frequency correction factor";
      break;

   case AST__LKF2HL:
      *comment = "Convert from LSRK to heliocentric frequency";
      result = "LKF2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__HLF2LK:
      *comment = "Convert from Heliocentric to LSRK frequency";
      result = "HLF2LK";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__LDF2HL:
      *comment = "Convert from LSRD to heliocentric frequency";
      result = "LDF2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__HLF2LD:
      *comment = "Convert from Heliocentric to LSRD frequency";
      result = "HLF2LD";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__LGF2HL:
      *comment = "Convert from Local group to heliocentric frequency";
      result = "LGF2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__HLF2LG:
      *comment = "Convert from Heliocentric to local group frequency";
      result = "HLF2LG";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__GLF2HL:
      *comment = "Convert from Galactic to heliocentric frequency";
      result = "GLF2HL";
      *argra = 0;
      *argdec = 1;
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   case AST__HLF2GL:
      *comment = "Convert from Heliocentric to galactic frequency";
      *argra = 0;
      *argdec = 1;
      result = "HLF2GL";
      *nargs = 2;
      *szargs = 3;
      arg[ 0 ] = "RA of source (FK5 J2000, radians)";
      arg[ 1 ] = "DEC of source (FK5 J2000, radians)";
      arg[ 2 ] = "Frequency correction factor";
      break;

   }

/* Return the result. */
   return result;
}


static void (*GetCvtFunc( int cvt_code ))( int, double *, double *, double *, 
                                           double *, int ) {
/*
*  Name:
*     GetCvtFunc

*  Purpose:
*     Return a pointer to the function which implements a particular
*     conversion type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     void (*GetCvtFunc( int cvt_code ))( int, double *, double *, double *, 
*                                         double *, int )

*  Class Membership:
*     SpecMap member function.

*  Description:
*     This function accepts a code value used to represent one of the
*     SpecMap coordinate conversions and returns a pointer to a function
*     which will implement that conversion. 

*  Parameters:
*     cvt_code
*        The conversion code.

*  Returned Value:
*     Pointer to a function which implements the conversion. This function
*     will have the following synopsis:
*
*        void CvtFunc( int npoint, double *values, double *ra, double *dec, 
*                      double *args, int forward )
*
*     where "npoint" is the number of axis values to transform, "values"
*     is a pointer to the array of "npoint" axis values, "args" is a
*     pointer to an array of argument values, and "forward" indicates if
*     the conversion should be applied in the forward or invere direction. 
*     The number of arguments required is specified by each particular 
*     conversion. If the conversion code value supplied is not valid, a 
*     NULL pointer will be returned, without error.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   void (* result)( int, double *, double *, double *, double *, int); /* Pointer to return */

/* Initialise the returned pointer, */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;
      
/* Test for each valid code value in turn and assign the appropriate
   return value. */
   switch ( cvt_code ) {

   case AST__FRTOVL:
      result = frtovl;
      break;

   case AST__VLTOFR:
      result = vltofr;
      break;

   case AST__ENTOFR:
      result = entofr;
      break;

   case AST__FRTOEN:
      result = frtoen;
      break;

   case AST__WNTOFR:
      result = wntofr;
      break;

   case AST__FRTOWN:
      result = frtown;
      break;

   case AST__WVTOFR:
      result = wvtofr;
      break;

   case AST__FRTOWV:
      result = frtowv;
      break;

   case AST__AWTOFR:
      result = awtofr;
      break;

   case AST__FRTOAW:
      result = frtoaw;
      break;

   case AST__VRTOVL:
      result = vrtovl;
      break;

   case AST__VLTOVR:
      result = vltovr;
      break;

   case AST__VOTOVL:
      result = votovl;
      break;

   case AST__VLTOVO:
      result = vltovo;
      break;

   case AST__ZOTOVL:
      result = zotovl;
      break;

   case AST__VLTOZO:
      result = vltozo;
      break;

   case AST__BTTOVL:
      result = bttovl;
      break;

   case AST__VLTOBT:
      result = vltobt;
      break;

   case AST__TPV2HL:
      result = tpv2hl;
      break;

   case AST__HLV2TP:
      result = hlv2tp;
      break;

   case AST__GEV2HL:
      result = gev2hl;
      break;

   case AST__HLV2GE:
      result = hlv2ge;
      break;

   case AST__BYV2HL:
      result = byv2hl;
      break;

   case AST__HLV2BY:
      result = hlv2by;
      break;

   case AST__LKV2HL:
      result = lkv2hl;
      break;

   case AST__HLV2LK:
      result = hlv2lk;
      break;

   case AST__LDV2HL:
      result = ldv2hl;
      break;

   case AST__HLV2LD:
      result = hlv2ld;
      break;

   case AST__LGV2HL:
      result = lgv2hl;
      break;

   case AST__HLV2LG:
      result = hlv2lg;
      break;

   case AST__GLV2HL:
      result = glv2hl;
      break;

   case AST__HLV2GL:
      result = hlv2gl;
      break;

   case AST__TPF2HL:
      result = tpf2hl;
      break;

   case AST__HLF2TP:
      result = hlf2tp;
      break;

   case AST__GEF2HL:
      result = gef2hl;
      break;

   case AST__HLF2GE:
      result = hlf2ge;
      break;

   case AST__BYF2HL:
      result = byf2hl;
      break;

   case AST__HLF2BY:
      result = hlf2by;
      break;

   case AST__LKF2HL:
      result = lkf2hl;
      break;

   case AST__HLF2LK:
      result = hlf2lk;
      break;

   case AST__LDF2HL:
      result = ldf2hl;
      break;

   case AST__HLF2LD:
      result = hlf2ld;
      break;

   case AST__LGF2HL:
      result = lgf2hl;
      break;

   case AST__HLF2LG:
      result = hlf2lg;
      break;

   case AST__GLF2HL:
      result = glf2hl;
      break;

   case AST__HLF2GL:
      result = hlf2gl;
      break;


   }

/* Return the result. */
   return result;
}

static double HelioVel( double epoch, double ra, double dec ){
/*
*  Name:
*     HelioVel

*  Purpose:
*     Returns the heliocentric velocity component of the earth.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     double HelioVel( double epoch, double ra, double dec )

*  Class Membership:
*     SpecMap method.

*  Description:
*     This function returns the component away from the source, of the 
*     velocity of the earths centre relative to the sun (in km/s). 

*  Parameters:
*    epoch 
*       Epoch of observation (UT1 expressed as a Modified Julian Date)
*    ra 
*       Right Ascension of source (radians, FK5 J2000)
*    dec
*       Declination of source (radians, FK5 J2000)

*  Returned Value:
*     The required velocity (km/s).

*/

/* Local Variables: */
   double dpb[ 3 ];          /* Barycentric earth position vector */
   double dph[ 3 ];          /* Heliocentric earth position vector */
   double dvb[ 3 ];          /* Barycentric earth velocity vector */
   double dvh[ 3 ];          /* Heliocentric earth velocity vector */
   double v[ 3 ];            /* Source direction vector */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Get the Cartesian vector towards the source, in the Cartesian FK5
   J2000 system. */
   slaDcs2c( ra, dec, v );

/* Get the Earth/Sun velocity and position vectors in the same system. */
   slaEvp( epoch, 2000.0, dvb, dpb, dvh, dph );
  
/* Return the component away from the source, of the velocity of the earths
   centre relative to the sun (in km/s). */
   return -slaDvdv( v, dvh )*149.597870E6;

}

void astInitSpecMapVtab_(  AstSpecMapVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitSpecMapVtab

*  Purpose:
*     Initialise a virtual function table for a SpecMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specmap.h"
*     void astInitSpecMapVtab( AstSpecMapVtab *vtab, const char *name )

*  Class Membership:
*     SpecMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SpecMap class.

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
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsASpecMap) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->SpecAdd = SpecAdd;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   mapping = (AstMappingVtab *) vtab;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "SpecMap",
               "Conversion between spectral coordinate systems" );
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a SpecMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     SpecMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated SpecMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated SpecMap with one which it
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
*        Pointer to the nominated SpecMap which is to be merged with
*        its neighbours. This should be a cloned copy of the SpecMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        SpecMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated SpecMap resides.
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
   AstSpecMap *specmap;            /* Pointer to SpecMap */
   const char *argdesc[ MAX_ARGS ]; /* Argument descriptions (junk) */
   const char *class;            /* Pointer to Mapping class string */
   const char *comment;          /* Pointer to comment string (junk) */
   double (*cvtargs)[ MAX_ARGS ]; /* Pointer to argument arrays */
   double tmp;                   /* Temporary storage */
   int *cvttype;                 /* Pointer to transformation type codes */
   int *szarg;                   /* Pointer to argument count array */
   int argdec;                   /* Index of DEC argument */
   int argra;                    /* Index of RA argument */
   int done;                     /* Finished (no further simplification)? */
   int iarg;                     /* Loop counter for arguments */
   int icvt1;                    /* Loop initial value */
   int icvt2;                    /* Loop final value */
   int icvt;                     /* Loop counter for transformation steps */
   int ikeep;                    /* Index to store step being kept */
   int imap1;                    /* Index of first SpecMap to merge */
   int imap2;                    /* Index of last SpecMap to merge */
   int imap;                     /* Loop counter for Mappings */
   int inc;                      /* Increment for transformation step loop */
   int invert;                   /* SpecMap applied in inverse direction? */
   int istep;                    /* Loop counter for transformation steps */
   int keep;                     /* Keep transformation step? */
   int narg;                     /* Number of user-supplied arguments */
   int ngone;                    /* Number of Mappings eliminated */
   int nin;                      /* Numbr of axes for SpecMaps being merged */
   int nstep0;                   /* Original number of transformation steps */
   int nstep;                    /* Total number of transformation steps */
   int result;                   /* Result value to return */
   int simpler;                  /* Simplification possible? */
   int unit;                     /* Replacement Mapping is a UnitMap? */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* SpecMaps can only be merged if they are in series (or if there is
   only one Mapping present, in which case it makes no difference), so
   do nothing if they are not. */
   if ( series || ( *nmap == 1 ) ) {

/* Save the number of inputs for the SpecMap. */
      nin = astGetNin( this );

/* Initialise the number of transformation steps to be merged to equal
   the number in the nominated SpecMap. */
      nstep = ( (AstSpecMap *) ( *map_list )[ where ] )->ncvt;

/* Search adjacent lower-numbered Mappings until one is found which is
   not a SpecMap, or is a SpecMap with a different number of axes. Accumulate 
   the number of transformation steps involved in any SpecMaps found. */
      imap1 = where;
      while ( ( imap1 - 1 >= 0 ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap1 - 1 ] );
         if ( !astOK || strcmp( class, "SpecMap" ) || 
              astGetNin( ( *map_list )[ imap1 - 1 ] ) != nin ) break;
         nstep += ( (AstSpecMap *) ( *map_list )[ imap1 - 1 ] )->ncvt;
         imap1--;
      }

/* Similarly search adjacent higher-numbered Mappings. */
      imap2 = where;
      while ( ( imap2 + 1 < *nmap ) && astOK ) {
         class = astGetClass( ( *map_list )[ imap2 + 1 ] );
         if ( !astOK || strcmp( class, "SpecMap" ) ||
              astGetNin( ( *map_list )[ imap2 + 1 ] ) != nin ) break;
         nstep += ( (AstSpecMap *) ( *map_list )[ imap2 + 1 ] )->ncvt;
         imap2++;
      }

/* Remember the initial number of transformation steps. */
      nstep0 = nstep;

/* Allocate memory for accumulating a list of all the transformation
   steps involved in all the SpecMaps found. */
      cvttype = astMalloc( sizeof( int ) * (size_t) nstep );
      cvtargs = astMalloc( sizeof( double[ MAX_ARGS ] ) * (size_t) nstep );
      szarg = astMalloc( sizeof( int ) * (size_t) nstep );

/* Loop to obtain the transformation data for each SpecMap being merged. */
      nstep = 0;
      for ( imap = imap1; astOK && ( imap <= imap2 ); imap++ ) {

/* Obtain a pointer to the SpecMap and note if it is being applied in
   its inverse direction. */
         specmap = (AstSpecMap *) ( *map_list )[ imap ];
         invert = ( *invert_list )[ imap ];

/* Set up loop limits and an increment to scan the transformation
   steps in each SpecMap in either the forward or reverse direction, as
   dictated by the associated "invert" value. */
         icvt1 = invert ? specmap->ncvt - 1 : 0;
         icvt2 = invert ? -1 : specmap->ncvt;
         inc = invert ? -1 : 1;

/* Loop through each transformation step in the SpecMap. */
         for ( icvt = icvt1; icvt != icvt2; icvt += inc ) {

/* Store the transformation type code and use "CvtString" to determine
   the associated number of arguments. Then store these arguments. */
            cvttype[ nstep ] = specmap->cvttype[ icvt ];
            (void) CvtString( cvttype[ nstep ], &comment, &argra, &argdec, 
                              &narg, szarg + nstep, argdesc );
            if ( !astOK ) break;
            for ( iarg = 0; iarg < szarg[ nstep ]; iarg++ ) {
               cvtargs[ nstep ][ iarg ] = specmap->cvtargs[ icvt ][ iarg ];
            }

/* If the SpecMap is inverted, we must not only accumulate its
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
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
            }

/* Macro to exchange a transformation type code for its inverse (and
   vice versa), and reciprocate a specified argument. */
#define SWAP_CODES2( code1, code2, jarg ) \
            if ( cvttype[ nstep ] == code1 ) { \
               cvttype[ nstep ] = code2; \
               tmp = cvtargs[ nstep ][ jarg ]; \
               if( tmp != AST__BAD && tmp != 0.0 ) { \
                  cvtargs[ nstep ][ jarg ] = 1.0/tmp; \
               } else { \
                  cvtargs[ nstep ][ jarg ] = AST__BAD; \
               } \
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
               tmp = cvtargs[ nstep ][ jarg ]; \
               if( tmp != AST__BAD && tmp != 0.0 ) { \
                  cvtargs[ nstep ][ jarg ] = 1.0/tmp; \
               } else { \
                  cvtargs[ nstep ][ jarg ] = AST__BAD; \
               } \
            }

/* Macro to exchange a transformation type code for its inverse (and
   vice versa), and negate a specified argument. */
#define SWAP_CODES3( code1, code2, jarg ) \
            if ( cvttype[ nstep ] == code1 ) { \
               cvttype[ nstep ] = code2; \
               tmp = cvtargs[ nstep ][ jarg ]; \
               if( tmp != AST__BAD ) { \
                  cvtargs[ nstep ][ jarg ] = -tmp; \
               } \
            } else if ( cvttype[ nstep ] == code2 ) { \
               cvttype[ nstep ] = code1; \
               tmp = cvtargs[ nstep ][ jarg ]; \
               if( tmp != AST__BAD ) { \
                  cvtargs[ nstep ][ jarg ] = -tmp; \
               } \
            }

/* Use these macros to apply the changes where needed. */
            if ( invert ) {

/* Exchange transformation codes for their inverses. */
               SWAP_CODES( AST__FRTOVL, AST__VLTOFR )
               SWAP_CODES( AST__ENTOFR, AST__FRTOEN ) 
               SWAP_CODES( AST__WNTOFR, AST__FRTOWN ) 
               SWAP_CODES( AST__WVTOFR, AST__FRTOWV ) 
               SWAP_CODES( AST__AWTOFR, AST__FRTOAW ) 
               SWAP_CODES( AST__VRTOVL, AST__VLTOVR ) 
               SWAP_CODES( AST__VOTOVL, AST__VLTOVO ) 
               SWAP_CODES( AST__ZOTOVL, AST__VLTOZO ) 
               SWAP_CODES( AST__BTTOVL, AST__VLTOBT ) 

/* Exchange transformation codes for their inverses, and negate the
   velocity correction term. */
               SWAP_CODES3( AST__TPV2HL, AST__HLV2TP, 5 ) 
               SWAP_CODES3( AST__GEV2HL, AST__HLV2GE, 3 ) 
               SWAP_CODES3( AST__BYV2HL, AST__HLV2BY, 3 ) 
               SWAP_CODES3( AST__LKV2HL, AST__HLV2LK, 2 ) 
               SWAP_CODES3( AST__LDV2HL, AST__HLV2LD, 2 ) 
               SWAP_CODES3( AST__LGV2HL, AST__HLV2LG, 2 ) 
               SWAP_CODES3( AST__GLV2HL, AST__HLV2GL, 2 ) 

/* Exchange transformation codes for their inverses, and reciprocate the
   frequency correction factor. */
               SWAP_CODES2( AST__TPF2HL, AST__HLF2TP, 5 ) 
               SWAP_CODES2( AST__GEF2HL, AST__HLF2GE, 3 ) 
               SWAP_CODES2( AST__BYF2HL, AST__HLF2BY, 3 ) 
               SWAP_CODES2( AST__LKF2HL, AST__HLF2LK, 2 ) 
               SWAP_CODES2( AST__LDF2HL, AST__HLF2LD, 2 ) 
               SWAP_CODES2( AST__LGF2HL, AST__HLF2LG, 2 ) 
               SWAP_CODES2( AST__GLF2HL, AST__HLF2GL, 2 ) 

            }

/* Undefine the local macros. */
#undef SWAP_CODES
#undef SWAP_CODES2
#undef SWAP_CODES3

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

/* The only simplifications for the conversions currently in this class act 
   to combine adjacent transformation steps, so only apply them while there 
   are at least 2 steps left. */
            if ( istep < ( nstep - 1 ) ) {

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
   First check for conversions which have no user-supplied arguments. */
               if ( PAIR_CVT2( AST__ENTOFR, AST__FRTOEN ) ||
                    PAIR_CVT2( AST__WNTOFR, AST__FRTOWN ) ||
                    PAIR_CVT2( AST__WVTOFR, AST__FRTOWV ) ||     
                    PAIR_CVT2( AST__AWTOFR, AST__FRTOAW ) ||
                    PAIR_CVT2( AST__VRTOVL, AST__VLTOVR ) ||
                    PAIR_CVT2( AST__VOTOVL, AST__VLTOVO ) ||
                    PAIR_CVT2( AST__ZOTOVL, AST__VLTOZO ) ||
                    PAIR_CVT2( AST__BTTOVL, AST__VLTOBT ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have a single user-supplied argument. */
               } else if( PAIR_CVT2( AST__FRTOVL, AST__VLTOFR ) &&
                          EQUAL( cvtargs[ istep ][ 0 ], 
                                 cvtargs[ istep + 1 ][ 0 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have two user-supplied arguments. */
               } else if( ( PAIR_CVT2( AST__LKF2HL, AST__HLF2LK ) || 
                            PAIR_CVT2( AST__LDF2HL, AST__HLF2LD ) || 
                            PAIR_CVT2( AST__LGF2HL, AST__HLF2LG ) || 
                            PAIR_CVT2( AST__GLF2HL, AST__HLF2GL ) || 
                            PAIR_CVT2( AST__LKV2HL, AST__HLV2LK ) || 
                            PAIR_CVT2( AST__LDV2HL, AST__HLV2LD ) || 
                            PAIR_CVT2( AST__LGV2HL, AST__HLV2LG ) || 
                            PAIR_CVT2( AST__GLV2HL, AST__HLV2GL ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ], 
                                 cvtargs[ istep + 1 ][ 0 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ], 
                                 cvtargs[ istep + 1 ][ 1 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have three user-supplied arguments. */
               } else if( ( PAIR_CVT2( AST__GEF2HL, AST__HLF2GE ) || 
                            PAIR_CVT2( AST__BYF2HL, AST__HLF2BY ) ||
                            PAIR_CVT2( AST__GEV2HL, AST__HLV2GE ) || 
                            PAIR_CVT2( AST__BYV2HL, AST__HLV2BY ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ], 
                                 cvtargs[ istep + 1 ][ 0 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ], 
                                 cvtargs[ istep + 1 ][ 1 ] ) &&
                          EQUAL( cvtargs[ istep ][ 2 ], 
                                 cvtargs[ istep + 1 ][ 2 ] ) ) {
                  istep++;
                  keep = 0;

/* Now check for conversions which have five user-supplied arguments (currently
   no conversions have four user-supplied arguments). */
               } else if( ( PAIR_CVT2( AST__TPF2HL, AST__HLF2TP ) ||
                            PAIR_CVT2( AST__TPV2HL, AST__HLV2TP ) ) &&
                          EQUAL( cvtargs[ istep ][ 0 ], 
                                 cvtargs[ istep + 1 ][ 0 ] ) &&
                          EQUAL( cvtargs[ istep ][ 1 ], 
                                 cvtargs[ istep + 1 ][ 1 ] ) &&
                          EQUAL( cvtargs[ istep ][ 2 ], 
                                 cvtargs[ istep + 1 ][ 2 ] ) &&
                          EQUAL( cvtargs[ istep ][ 3 ], 
                                 cvtargs[ istep + 1 ][ 3 ] ) &&
                          EQUAL( cvtargs[ istep ][ 4 ], 
                                 cvtargs[ istep + 1 ][ 4 ] ) ) {
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
   number of transformation steps was reduced, or (c) the SpecMap(s)
   can be replaced by a UnitMap, or (d) if there was initially only
   one SpecMap present, its invert flag was set (this flag will always
   be cleared in the replacement Mapping). */
         simpler = ngone || ( nstep < nstep0 ) || unit ||
                   ( *invert_list )[ where ];

/* Do nothing more unless simplification is possible. */
         if ( simpler ) {

/* If the replacement Mapping is a UnitMap, then create it. */
            if ( unit ) {
               new = (AstMapping *)
                        astUnitMap( astGetNin( ( *map_list )[ where ] ), "" );

/* Otherwise, create a replacement SpecMap and add each of the
   remaining transformation steps to it. */
            } else {
               new = (AstMapping *) astSpecMap( nin, 0, "" );
               for ( istep = 0; istep < nstep; istep++ ) {
                  AddSpecCvt( (AstSpecMap *) new, cvttype[ istep ],
                             cvtargs[ istep ] );
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

static double Refrac( double wavelen ){
/*
*  Name:
*     Refrac

*  Purpose:
*     Returns the refractive index of dry air at a given wavelength.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     double Refrac( double wavelen )

*  Class Membership:
*     SpecMap method.

*  Description:
*     This function returns the refractive index of dry air at standard
*     temperature and pressure, at a given wavelength. The formula is
*     taken from the paper "Representation of Spectral Coordinates in FITS"
*     (Greisen et al).

*  Parameters:
*     wavelen
*        The wavelength, in metres. This should be the air wavelength,
*        but supplying the vacuum wavelength will make no significant
*        difference.

*  Returned Value:
*     The refractive index. A value of 1.0 is returned if an error
*     occurs, or has already occurred.

*/

/* Local Variables: */
   double result;            /* The returned refractive index */
   double w2;                /* Wavenumber squared */
   double d;                 /* Intermediate value */

/* Initialise. */
   result = 1.0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Find the squared wave number in units of "(per cm)**2". */
   w2 = 1.0E-4/( wavelen * wavelen );

/* Apply the rest of the algorithm as described in the FITS WCS 
   paper III. */
   result = 1.0;
   result += 6432.8E-8;

   d = 1.46E10 - w2;
   if( d != 0.0 ) {
      result += 2.949810E6/d;
      d = 4.1E9 - w2;
      if( d != 0.0 ) {
         result += 2.5540E4/d;
      } else {
         result = 1.0;
      }
   } else {
      result = 1.0;
   }

   return result;
}

static void SpecAdd( AstSpecMap *this, const char *cvt, const double args[] ) {
/*
*++
*  Name:
c     astSpecAdd
f     AST_SPECADD

*  Purpose:
*     Add a spectral coordinate conversion to a SpecMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "specmap.h"
c     void astSpecAdd( AstSpecMap *this, const char *cvt, const double args[] )
f     CALL AST_SPECADD( THIS, CVT, ARGS, STATUS )

*  Class Membership:
*     SpecMap method.

*  Description:
c     This function adds one of the standard spectral coordinate
f     This routine adds one of the standard spectral coordinate
*     system conversions listed below to an existing SpecMap.
*
c     When a SpecMap is first created (using astSpecMap), it simply
f     When a SpecMap is first created (using AST_SPECMAP), it simply
c     performs a unit (null) Mapping. By using astSpecAdd (repeatedly
f     performs a unit (null) Mapping. By using AST_SPECADD (repeatedly
*     if necessary), one or more coordinate conversion steps may then
*     be added, which the SpecMap will perform in sequence. This allows
*     multi-step conversions between a variety of spectral coordinate
*     systems to be assembled out of the building blocks provided by
*     this class.
*
*     Normally, if a SpecMap's Invert attribute is zero (the default),
*     then its forward transformation is performed by carrying out
*     each of the individual coordinate conversions specified by
c     astSpecAdd in the order given (i.e. with the most recently added
f     AST_SPECADD in the order given (i.e. with the most recently added
*     conversion applied last).
*
*     This order is reversed if the SpecMap's Invert attribute is
*     non-zero (or if the inverse transformation is requested by any
*     other means) and each individual coordinate conversion is also
*     replaced by its own inverse. This process inverts the overall
*     effect of the SpecMap. In this case, the first conversion to be
*     applied would be the inverse of the one most recently added.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the SpecMap.
c     cvt
f     CVT = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string which identifies the
f        A character string which identifies the
*        spectral coordinate conversion to be added to the
*        SpecMap. See the "Available Conversions" section for details of
*        those available.
c     args
f     ARGS( * ) = DOUBLE PRECISION (Given)
*        An array containing argument values for the celestial
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
*     difficult to determine the most economical conversion path. For
*     example, when converting between reference frames, converting first 
*     to the heliographic reference frame as an intermediate stage is often 
*     sensible in formulating the problem, but may introduce unnecessary
*     extra conversion steps. A solution to this is to include all the steps 
*     which are (logically) necessary, but then to use 
c     astSimplify to simplify the resulting
f     AST_SIMPLIFY to simplify the resulting
*     SpecMap. The simplification process will eliminate any steps
*     which turn out not to be needed.
c     - This function does not check to ensure that the sequence of
f     - This routine does not check to ensure that the sequence of
*     coordinate conversions added to a SpecMap is physically
*     meaningful.

*  Available Conversions:
*     The following strings (which are case-insensitive) may be supplied
c     via the "cvt" parameter to indicate which spectral coordinate
f     via the CVT argument to indicate which spectral coordinate
*     conversion is to be added to the SpecMap. Where arguments are needed by
*     the conversion, they are listed in parentheses. Values for
c     these arguments should be given, via the "args" array, in the
f     these arguments should be given, via the ARGS array, in the
*     order indicated. Units and argument names are described at the end of 
*     the list of conversions.

*     - "FRTOVL" (RF): Convert frequency to relativistic velocity.
*     - "VLTOFR" (RF): Convert relativistic velocity to Frequency.
*     - "ENTOFR": Convert energy to frequency.
*     - "FRTOEN": Convert frequency to energy.
*     - "WNTOFR": Convert wave number to frequency.
*     - "FRTOWN": Convert frequency to wave number.
*     - "WVTOFR": Convert wavelength (vacuum) to frequency.
*     - "FRTOWV": Convert fFrequency to wavelength (vacuum).
*     - "AWTOFR": Convert wavelength (air) to frequency.
*     - "FRTOAW": Convert frequency to wavelength (air).
*     - "VRTOVL": Convert radio to relativistic velocity.
*     - "VLTOVR": Convert relativistic to radio velocity.
*     - "VOTOVL": Convert optical to relativistic velocity.
*     - "VLTOVO": Convert relativistic to optical velocity.
*     - "ZOTOVL": Convert redshift to relativistic velocity.
*     - "VLTOZO": Convert relativistic velocity to redshift.
*     - "BTTOVL": Convert beta factor to relativistic velocity.
*     - "VLTOBT": Convert relativistic velocity to beta factor.
*     - "TPV2HL" (GLON,GLAT,EPOCH,RA,DEC): Convert relativistic velocity from 
*     topocentric reference frame to heliocentric.
*     - "HLV2TP" (GLON,GLAT,EPOCH,RA,DEC): Convert relativistic velocity from 
*     heliocentric reference frame to topocentric.
*     - "GEV2HL" (EPOCH,RA,DEC): Convert relativistic velocity from geocentric 
*     reference frame to heliocentric.
*     - "HLV2GE" (EPOCH,RA,DEC): Convert relativistic velocity from 
*     heliocentric reference frame to geocentric.
*     - "BYV2HL" (EPOCH,RA,DEC): Convert relativistic velocity from 
*     barycentric reference frame to heliocentric.
*     - "HLV2BY" (EPOCH,RA,DEC): Convert relativistic velocity from 
*     heliocentric reference frame to barycentric.
*     - "LKV2HL" (RA,DEC): Convert relativistic velocity from kinematic LSR 
*     reference frame to heliocentric.
*     - "HLV2LK" (RA,DEC): Convert relativistic velocity from heliocentric 
*     reference frame to kinematic LSR.
*     - "LDV2HL" (RA,DEC): Convert relativistic velocity from dynamical LSR
*     reference frame to heliocentric.
*     - "HLV2LD" (RA,DEC): Convert relativistic velocity from heliocentric 
*     reference frame to dynamical LSR.
*     - "LGV2HL" (RA,DEC): Convert relativistic velocity from local group 
*     reference frame to heliocentric.
*     - "HLV2LG" (RA,DEC): Convert relativistic velocity from heliocentric 
*     reference frame to local group.
*     - "GLV2HL" (RA,DEC): Convert relativistic velocity from galactic 
*     reference frame to heliocentric.
*     - "HLV2GL" (RA,DEC): Convert relativistic velocity from heliocentric 
*     reference frame to galactic.
*     - "TPF2HL" (GLON,GLAT,EPOCH,RA,DEC): Convert frequency from 
*     topocentric reference frame to heliocentric.
*     - "HLF2TP" (GLON,GLAT,EPOCH,RA,DEC): Convert frequency from 
*     heliocentric reference frame to topocentric.
*     - "GEF2HL" (EPOCH,RA,DEC): Convert frequency from geocentric 
*     reference frame to heliocentric.
*     - "HLF2GE" (EPOCH,RA,DEC): Convert frequency from 
*     heliocentric reference frame to geocentric.
*     - "BYF2HL" (EPOCH,RA,DEC): Convert frequency from 
*     barycentric reference frame to heliocentric.
*     - "HLF2BY" (EPOCH,RA,DEC): Convert frequency from 
*     heliocentric reference frame to barycentric.
*     - "LKF2HL" (RA,DEC): Convert frequency from kinematic LSR 
*     reference frame to heliocentric.
*     - "HLF2LK" (RA,DEC): Convert frequency from heliocentric 
*     reference frame to kinematic LSR.
*     - "LDF2HL" (RA,DEC): Convert frequency from dynamical LSR
*     reference frame to heliocentric.
*     - "HLF2LD" (RA,DEC): Convert frequency from heliocentric 
*     reference frame to dynamical LSR.
*     - "LGF2HL" (RA,DEC): Convert frequency from local group 
*     reference frame to heliocentric.
*     - "HLF2LG" (RA,DEC): Convert frequency from heliocentric 
*     reference frame to local group.
*     - "GLF2HL" (RA,DEC): Convert frequency from galactic 
*     reference frame to heliocentric.
*     - "HLF2GL" (RA,DEC): Convert frequency from heliocentric 
*     reference frame to galactic.

*     The units for the values processed by the above conversions are as
*     follows: 
*
*     - all velocities: metres per second.
*     - frequency: Hertz.
*     - all wavelengths: metres.
*     - energy: Joules.
*     - wave number: cycles per metre.
*
*     The arguments used in the above conversions are as follows:
*
*     - RF: Rest frequency (Hz).
*     - GLAT: Geodetic latitude of observer (radians).
*     - GLON: Geodetic longitude of observer (radians - positive eastwards).
*     - EPOCH: Epoch of observation (UT1 expressed as a Modified Julian Date).
*     - RA: Right Ascension of source (radians, FK5 J2000).
*     - DEC: Declination of source (radians, FK5 J2000).
*
*     The values supplied for the "RA" and "DEC" arguments are ignored if
*     the SpecMap is 3-dimensional (AST__BAD may be supplied). In this case, 
*     the source position is provided by the RA and DEC values supplied to 
*     inputs 2 and 3 of the SpecMap (which are simply copied to outputs 2 and 
*     3).
*
*--
*/

/* Local Variables: */
   int cvttype;                  /* Conversion type code */

/* Check the inherited status. */
   if ( !astOK ) return;

/* Validate the type string supplied and obtain the equivalent
   conversion type code. */
   cvttype = CvtCode( cvt );

/* If the string was not recognised, then report an error. */
   if ( astOK && ( cvttype == AST__SPEC_NULL ) ) {
      astError( AST__SPCIN,
                "%s(%s): Invalid SpecMap spectral coordinate "
                "conversion type \"%s\".", "astAddSpec", astGetClass( this ), cvt );
   }

/* Add the new conversion to the SpecMap. */
   AddSpecCvt( this, cvttype, args );
}

static double TopoVel( double lon, double lat, double epoch, double ra,
                       double dec ){
/*
*  Name:
*     TopoVel

*  Purpose:
*     Returns the topocentric velocity of the observer.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     double TopoVel( double lon, double lat, double epoch, double ra,
*                     double dec )

*  Class Membership:
*     SpecMap method.

*  Description:
*     This function returns the component away from the source, of the 
*     velocity of the observer  relative to the centre of the earth, in 
*     km/s. 

*  Parameters:
*    lon 
*       Geodetic longitude of observer (radians - positive eastwards)
*    lat 
*       Geodetic latitude of observer (radians)
*    epoch 
*       Epoch of observation (UT1 expressed as a Modified Julian Date)
*    ra 
*       Right Ascension of source (radians, FK5 J2000)
*    dec
*       Declination of source (radians, FK5 J2000)

*  Returned Value:
*     The required velocity (km/s).

*/

/* Local Variables: */
   double amprms[21];        /* Mean to apparent parameters */
   double deca;              /* Apparent DEC */
   double last;              /* Local apparent siderial time */
   double raa;               /* Apparent RA */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Convert the supplied mean ra and dec to apparent ra and dec. */
   slaMappa( 2000.0, epoch, amprms );
   slaMapqkz( ra, dec, amprms, &raa, &deca );

/* Get the local apparent siderial time (in radians). */
   last = slaGmst( epoch ) + slaEqeqx( epoch ) + lon;

/* Return the component away from the source, of the velocity of the observer 
   relative to the centre of the earth (in km/s). */
   return (double) slaRverot( (float) lat, (float) raa, (float) deca, 
                              (float) last );
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a SpecMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out )

*  Class Membership:
*     SpecMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a SpecMap and a set of points encapsulated
*     in a PointSet and transforms the points so as to perform the
*     sequence of spectral coordinate conversions specified by
*     previous invocations of astSpecAdd.

*  Parameters:
*     this
*        Pointer to the SpecMap.
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

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the SpecMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstSpecMap *map;              /* Pointer to SpecMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *spec;                 /* Pointer to output spectral axis value array */
   double *alpha;                /* Pointer to output RA axis value array */
   double *beta;                 /* Pointer to output DEC axis value array */
   int cvt;                      /* Loop counter for conversions */
   int end;                      /* Termination index for conversion loop */
   int inc;                      /* Increment for conversion loop */
   int map3d;                    /* Is the SpecMap 3-dimensional? */
   int ncoord_in;                /* Number of coordinates per input point */
   int npoint;                   /* Number of points */
   int start;                    /* Starting index for conversion loop */
   void (* cvtFunc)( int, double *, double *, double *, double *, int ); /* Pointer to conversion function */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the SpecMap. */
   map = (AstSpecMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out );

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
/* Use "spec" as a synonym for the array of spectral axis values stored in 
   the output PointSet. */
   if ( astOK ) {
      spec = ptr_out[ 0 ];

/* If this is a 3D SpecMap use "alpha" as a synonym for the array of RA axis 
   values and "beta" as a synonym for the array of DEC axis values stored 
   in the output PointSet. */
      map3d = ( ncoord_in == 3 );
      if( map3d ) {
         alpha = ptr_out[ 1 ];
         beta = ptr_out[ 2 ];
      } else {
         alpha = NULL;
         beta = NULL;
      }

/* Initialise the output coordinate values by copying the input ones. */
      (void) memcpy( spec, ptr_in[ 0 ], sizeof( double ) * (size_t) npoint );
      if( map3d ) {
         (void) memcpy( alpha, ptr_in[ 1 ], sizeof( double ) * (size_t) npoint );
         (void) memcpy( beta, ptr_in[ 2 ], sizeof( double ) * (size_t) npoint );
      }

/* We will loop to apply each spectral coordinate conversion in turn to the
   (spec) array. However, if the inverse transformation was requested,
   we must loop through these transformations in reverse order, so set up
   appropriate limits and an increment to control this loop. */
      start = forward ? 0 : map->ncvt - 1;
      end = forward ? map->ncvt : -1;
      inc = forward ? 1 : -1;

/* Loop through the coordinate conversions in the required order. */
      for ( cvt = start; cvt != end; cvt += inc ) {

/* Get a pointer to the function to be called to do the transformation. */
         cvtFunc = GetCvtFunc( map->cvttype[ cvt ] );

/* Invoke this function to do the transformation. This over-writes the
   current contents of the "spec" array. */
         if( cvtFunc ) cvtFunc( npoint, spec, alpha, beta, map->cvtargs[ cvt ],
                                forward );
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


static void VelCorr( double *value, double vc ){
/*
*  Name:
*     VelCorr

*  Purpose:
*     Convert a radial velocity to a new reference frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     void VelCorr( double *value, double vc )

*  Class Membership:
*     SpecMap method.

*  Description:
*     This function converts the supplied relativistic radial velocity to 
*     a new reference frame.

*  Parameters:
*     value
*        Pointer to a double holding a velocity value (m/s) measured
*        in the old reference frame. Positive velocity corresponds to the 
*        source receeding from the observer. The supplied value is modified 
*        on return to hold the corresponding converted value.
*     vc 
*        The component of the velocity (m/s) of the origin of the new 
*        reference frame, towards the source, measured in the old
*        reference frame.
*/

/* Local Variables: */
   double f, d;

/* Check the global error status. */
   if ( !astOK ) return;

/* Correct the supplied velocity value, adding the correction term to the 
   velocity relativistically. */
   f = vc/( AST__C * AST__C );
   if( *value != AST__BAD ) {
      d = 1.0 - (*value)*f;
      if( d != 0.0 ) {
         *value = ( (*value) - vc )/d;
      } else {
         *value = AST__BAD;
      }
   }
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for SpecMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for SpecMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstSpecMap *in;                /* Pointer to input SpecMap */
   AstSpecMap *out;               /* Pointer to output SpecMap */
   int cvt;                       /* Loop counter for coordinate conversions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output SpecMap structures. */
   in = (AstSpecMap *) objin;
   out = (AstSpecMap *) objout;

/* For safety, first clear any references to the input memory from the output
   SpecMap. */
   out->cvtargs = NULL;
   out->cvttype = NULL;

/* Allocate memory for the output array of argument list pointers. */
   out->cvtargs = astMalloc( sizeof( double * ) * (size_t) in->ncvt );

/* If necessary, allocate memory and make a copy of the input array of
   coordinate conversion codes. */
   if ( in->cvttype ) out->cvttype = astStore( NULL, in->cvttype,
                                               sizeof( int )
                                               * (size_t) in->ncvt );

/* If OK, loop through each conversion in the input SpecMap and make a copy of
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
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for SpecMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for SpecMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstSpecMap *this;              /* Pointer to SpecMap */
   int cvt;                       /* Loop counter for coordinate conversions */

/* Obtain a pointer to the SpecMap structure. */
   this = (AstSpecMap *) obj;

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
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SpecMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SpecMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SpecMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstSpecMap *this;             /* Pointer to the SpecMap structure */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   const char *sval;             /* Pointer to string value */
   int argdec;                   /* Index of DEC argument */
   int argra;                    /* Index of RA argument */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int ival;                     /* Integer value */
   int nargs;                    /* Number of user-supplied arguments */
   int szargs;                   /* Number of stored arguments */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecMap structure. */
   this = (AstSpecMap *) this_object;

/* Write out values representing the instance variables for the SpecMap
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
   astWriteInt( channel, "Nspec", set, 0, ival, "Number of conversion steps" );

/* Write out data for each conversion step... */
   for ( icvt = 0; icvt < this->ncvt; icvt++ ) {

/* Conversion type. */
/* ---------------- */
/* Change each conversion type code into an equivalent string and
   obtain associated descriptive information. If the conversion code
   was not recognised, report an error and give up. */
      if ( astOK ) {
         sval = CvtString( this->cvttype[ icvt ], &comment, &argra, &argdec, 
                           &nargs, &szargs, argdesc );
         if ( astOK && !sval ) {
            astError( AST__SPCIN,
                      "astWrite(%s): Corrupt %s contains invalid SpecMap "
                      "spectral coordinate conversion code (%d).",
                      astGetClass( channel ), astGetClass( this ),
                      (int) this->cvttype[ icvt ] );
            break;
         }

/* Create an appropriate keyword and write out the conversion code
   information. */
         (void) sprintf( key, "Spec%d", icvt + 1 );
         astWriteString( channel, key, 1, 1, sval, comment );

/* Write out data for each conversion argument... */
         for ( iarg = 0; iarg < szargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and write out the argument value,
   accompanied by the descriptive comment obtained above. */
            if( this->cvtargs[ icvt ][ iarg ] != AST__BAD ) {
               (void) sprintf( key, "Spec%d%c", icvt + 1, ALPHABET[ iarg ] );
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

/* Conversion functions. */
/* ===================== */
/* These functions provide the implementation of each of the supported
   conversions. They all have the same synopsis and argument list:

*  Synopsis:
*     #include "specmap.h"
*     void <name>( int npoint, double *values, double *ra, double *dec, 
*                  double *args )

*  Parameters:
*     npoint
*        The number of axis values to transform.
*     values
*        Pointer to an array of "npoint" spectral axis values. These are 
*        modified on return to hold the corresponding converted values.
*     ra
*        Pointer to an array of "npoint" RA (J2000 FK5) values. These are 
*        unchanged on exit. If a NULL pointer is supplied, then a single
*        RA value is used for all spectral axis values, given by the RA 
*        value in the "args" array. Otherwise, the "RA" value in the
*        "args" array is ignored.
*     dec
*        Pointer to an array of "npoint" DEC (J2000 FK5) values. These are 
*        unchanged on exit. If a NULL pointer is supplied, then a single
*        DEC value is used for all spectral axis values, given by the DEC
*        value in the "args" array. Otherwise, the "DEC" value in the
*        "args" array is ignored.
*     args
*        Pointer to an array holding the conversion arguments. The number
*        of arguments expected depends on the particular conversion being
*        used.
*     forward
*        Should the conversion be applied in the forward or inverse
*        direction? Non-zero for forward, zero for inverse.
*/



/* Frequency (Hz) to relativistic velocity (m/s). "args[0]" is the rest
   frequency (Hz).
   -------------------------------------------------------------------- */
static void frtovl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {

      double *val, *end, rf2, f2, d;
      rf2 = args[ 0 ]*args[ 0 ];
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            f2 = (*val)*(*val);
            d = rf2 + f2;
            if( d > 0.0 ) {
               *val = AST__C*( ( rf2 - f2 )/( rf2 + f2 ) );
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */
   } else {
      vltofr( npoint, values, ra, dec, args, 1 );
   }
}



/* Relativistic velocity (m/s) to frequency (Hz). "args[0]" is the rest
   frequency (Hz).
   ------------------------------------------------------------------- */
static void vltofr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, rf, d;
      rf = args[ 0 ];
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            d = AST__C + *val;
            if( d != 0.0 ) {
               d = (AST__C - *val )/d;
               if( d >= 0.0 ) {
                  *val = rf*sqrt( d );
               } else {
                  *val = AST__BAD;
               }
   
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      frtovl( npoint, values, ra, dec, args, 1 );
   }
}

/* Frequency (Hz) to energy (Joules). No arguments used.
   ---------------------------------------------------- */
static void frtoen( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val *= AST__H;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      entofr( npoint, values, ra, dec, args, 1 );
   }
}



/* Energy (Joules) to frequency (Hz). No arguments used.
   ---------------------------------------------------- */
static void entofr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val /= AST__H;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      frtoen( npoint, values, ra, dec, args, 1 );
   }
}



/* Frequency (Hz) to WaveNumber (cycles per metre). No arguments used.
   ------------------------------------------------------------------- */
static void frtown( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val /= AST__C ;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      wntofr( npoint, values, ra, dec, args, 1 );
   }
}



/* WaveNumber (cycles per metre) to frequency (Hz). No arguments used.
   ------------------------------------------------------------------ */
static void wntofr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val *= AST__C;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      frtown( npoint, values, ra, dec, args, 1 );
   }
}



/* Frequency (Hz) to wavelength (metres). No arguments used.
   --------------------------------------------------------- */
static void frtowv( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            if( *val != 0.0 ) {
               *val = AST__C/(*val);
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      wvtofr( npoint, values, ra, dec, args, 1 );
   }
}



/* Wavelength (metres) to frequency (Hz). No arguments used.
   --------------------------------------------------------- */
static void wvtofr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            if( *val != 0.0 ) {
               *val = AST__C/(*val);
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      frtowv( npoint, values, ra, dec, args, 1 );
   }
}



/* Frequency (Hz) to wavelength (metres) in air. No arguments used.
   --------------------------------------------------------------- */
static void frtoaw( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, wavelen;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            if( *val != 0.0 ) {
               wavelen  = AST__C/(*val);
               *val = wavelen/Refrac( wavelen );
            } else {
               *val = AST__BAD;
            }
         } 
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      awtofr( npoint, values, ra, dec, args, 1 );
   }
}



/* Wavelength (metres) in air to frequency (Hz). No arguments used.
   ---------------------------------------------------------------- */
static void awtofr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            if( *val != 0.0 ) {
               *val = AST__C/( (*val)*Refrac( *val ) );
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      frtoaw( npoint, values, ra, dec, args, 1 );
   }
}



/* Radio velocity to relativistic velocity (m/s). No arguments used.
   ---------------------------------------------------------------- */
static void vrtovl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, rnu;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            rnu = 1.0 - (*val)/AST__C;
            rnu *= rnu;
            *val = AST__C*( 1.0 - rnu )/( 1.0 + rnu );
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      vltovr( npoint, values, ra, dec, args, 1 );
   }
}



/* Relativistic velocity (m/s) to radio velocity. No arguments used.
   ------------------------------------------------------------------- */
static void vltovr( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, d;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
   
            d = AST__C + *val;
            if( d != 0.0 ) {
               d = (AST__C - *val )/d;
               if( d >= 0.0 ) {
                  *val = AST__C*( 1.0 - sqrt( d ) );
               } else {
                  *val = AST__BAD;
               }
   
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      vrtovl( npoint, values, ra, dec, args, 1 );
   }
}



/* Optical velocity to relativistic velocity (m/s). No arguments used.
   ------------------------------------------------------------------ */
static void votovl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, rlam;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            rlam = 1.0 + (*val)/AST__C;
            rlam *= rlam;
            *val = AST__C*( rlam - 1.0 )/( rlam + 1.0 );
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      vltovo( npoint, values, ra, dec, args, 1 );
   }
}



/* Relativistic velocity (m/s) to optical velocity (m/s). No arguments used.
   ----------------------------------------------------------------------- */
static void vltovo( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, d;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
   
            d = AST__C - *val;
            if( d != 0.0 ) {
               d = (AST__C + *val )/d;
               if( d >= 0.0 ) {
                  *val = AST__C*( sqrt( d ) - 1.0 );
               } else {
                  *val = AST__BAD;
               }
   
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      votovl( npoint, values, ra, dec, args, 1 );
   }
}



/* Redshift to relativistic velocity (m/s). No arguments used.
   --------------------------------------------------------- */
static void zotovl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, rlam;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            rlam = 1.0 + (*val);
            rlam *= rlam;
            *val = AST__C*( rlam - 1.0 )/( rlam + 1.0 );
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      vltozo( npoint, values, ra, dec, args, 1 );
   }
}



/* Relativistic velocity (m/s) to redshift. No arguments used.
   ----------------------------------------------------------- */
static void vltozo( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end, d;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            d = AST__C - *val;
            if( d != 0.0 ) {
               d = (AST__C + *val )/d;
               if( d >= 0.0 ) {
                  *val = sqrt( d ) - 1.0;
               } else {
                  *val = AST__BAD;
               }
   
            } else {
               *val = AST__BAD;
            }
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      zotovl( npoint, values, ra, dec, args, 1 );
   }
}



/* Beta factor to relativistic velocity (m/s). No arguments used.
   ------------------------------------------------------------ */
static void bttovl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val *= AST__C;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      vltobt( npoint, values, ra, dec, args, 1 );
   }
}



/* Relativistic velocity (m/s) to beta factor. No arguments used.
   ------------------------------------------------------------------- */
static void vltobt( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {

/* First deal with forward transformations. */
   if( forward ) {
      double *val, *end;
   
      end = values + npoint;
      for( val = values; val < end; val++ ) {
         if( *val != AST__BAD ) {
            *val /= AST__C;
         }
      }

/* If the inverse transformation is to be applied, invoke the appropriate
   inverse function in the forward direction. */ 
   } else {
      bttovl( npoint, values, ra, dec, args, 1 );
   }
}



/* Topocentric to heliocentric frequency correction:
*    args[ 0 ] = Geodetic longitude of observer (radians, positive eastwards)
*    args[ 1 ] = Geodetic latitude of observer (radians)
*    args[ 2 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 3 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 4 ] = Declination of source (radians, FK5 J2000)
*    args[ 5 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void tpf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, s3, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
      used_f = forward ? args[ 5 ] : 1.0/args[ 5 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, or if we are finding a separate correction factor for
   each spatial position, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 3 ];
            dec0 = args[ 4 ];
         }

/* Get the component away from the source, of the velocity of the observer 
   relative to the centre of the earth (in km/s). */
         s1 = TopoVel( args[ 0 ], args[ 1 ], args[ 2 ], ra0, dec0 );

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in km/s). */
         s2 = HelioVel( args[ 2 ], ra0, dec0 );

/* Find the total velocity of the observer away from the source, relative
   to the sun in m/s. This can also be thought of as the velocity of the
   sun towards the source relative to the observer. */
         s3 = 1000.0*( s1 + s2 );

/* Find the factor by which to multiply the supplied topocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the observer.  If s3 is 
   positive (i.e. the sun is moving towards the source relative to the 
   observer), then the frequency as seen on the sun should be higher than the 
   frequency as seen by the observer, so the factor should be greater
   than one. The factor in args[ 5 ] is then the value appropriate to the
   forward correction. For the inverse correction, it should be reciprocated. */
         if( AST__C != s3 ) args[ 5 ] = sqrt( ( AST__C + s3 )/( AST__C - s3 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : 1.0/args[ 5 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to topocentric frequency correction:
*    args[ 0 ] = Geodetic longitude of observer (radians, positive eastwards)
*    args[ 1 ] = Geodetic latitude of observer (radians)
*    args[ 2 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 3 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 4 ] = Declination of source (radians, FK5 J2000)
*    args[ 5 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2tp( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, s3, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
      used_f = forward ? args[ 5 ] : 1.0/args[ 5 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 3 ];
            dec0 = args[ 4 ];
         }

/* Get the component away from the source, of the velocity of the observer 
   relative to the centre of the earth (in km/s). */
         s1 = TopoVel( args[ 0 ], args[ 1 ], args[ 2 ], ra0, dec0 );

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in km/s). */
         s2 = HelioVel( args[ 2 ], ra0, dec0 );

/* Find the total velocity of the observer away from the source, relative
   to the sun in m/s. This can also be thought of as the velocity of the
   sun towards the source relative to the observer. */
         s3 = 1000.0*( s1 + s2 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the observer.  If s3 is 
   positive (i.e. the sun is moving towards the source relative to the 
   observer), then the frequency as seen on the sun should be higher than the 
   frequency as seen by the observer, so the factor should be less than 
   one. */
         if( AST__C != s3 ) args[ 5 ] = sqrt( ( AST__C - s3 )/( AST__C + s3 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : 1.0/args[ 5 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Geocentric to heliocentric frequency correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void gef2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in m/s). This can also be thought of as the 
   velocity of the sun towards the source relative to the earth. */
         s1 = 1000.0*HelioVel( args[ 0 ], ra0, dec0 );

/* Find the factor by which to multiply the supplied geocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the earth.  If s1 is 
   positive (i.e. the sun is moving towards the source relative to the 
   earth), then the frequency as seen on the sun should be higher than the 
   frequency as seen on the earth, so the factor should be greater than one. */
         if( AST__C != s1 ) args[ 3 ] = sqrt( ( AST__C + s1 )/( AST__C - s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to Geocentric frequency correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2ge( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in m/s). This can also be thought of as the 
   velocity of the sun towards the source relative to the earth. */
         s1 = 1000.0*HelioVel( args[ 0 ], ra0, dec0 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the earth.  If s1 is 
   positive (i.e. the sun is moving towards the source relative to the 
   earth), then the frequency as seen on the sun should be higher than the 
   frequency as seen on the earth, so the factor should be less than one. */
         if( AST__C != s1 ) args[ 3 ] = sqrt( ( AST__C - s1 )/( AST__C + s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Barycentric to heliocentric frequency correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void byf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component away from the source, of the velocity of the earth-sun
   barycentre relative to the sun (in m/s). This can also be thought of as the 
   velocity of the sun towards the source relative to the barycentre. */
         s1 = 1000.0*BaryVel( args[ 0 ], ra0, dec0 );

/* Find the factor by which to multiply the supplied barycentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the barycentre.  If s1 
   is positive (i.e. the sun is moving towards the source relative to the 
   barycentre), then the frequency as seen on the sun should be higher than 
   the frequency as seen at the barycentre, so the factor should be greater 
   than one. */
         if( AST__C != s1 ) args[ 3 ] = sqrt( ( AST__C + s1 )/( AST__C - s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to barycentric frequency correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2by( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component away from the source, of the velocity of the earth-sun
   barycentre relative to the sun (in m/s). This can also be thought of as the 
   velocity of the sun towards the source relative to the barycentre. */
         s1 = 1000.0*BaryVel( args[ 0 ], ra0, dec0 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun towards the source relative to the barycentre.  If s1 
   is positive (i.e. the sun is moving towards the source relative to the 
   barycentre), then the frequency as seen on the sun should be higher than 
   the frequency as seen at the barycentre, so the factor should be less than 
   one. */
         if( AST__C != s1 ) args[ 3 ] = sqrt( ( AST__C - s1 )/( AST__C + s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : 1.0/args[ 3 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Kinematic LSR to heliocentric frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void lkf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the kinematic LSR (in m/s). This can also be thought of as the 
   velocity of the LSR towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlsrk( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied LSRK frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the LSRK.  If s1 
   is positive (i.e. the sun is moving away from the source relative to the 
   LSRK), then the frequency as seen on the sun should be lower than the
   frequency as seen in the LSRK, so the factor should be less than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C - s1 )/( AST__C + s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to kinematic LSR frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2lk( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the kinematic LSR (in m/s). This can also be thought of as the 
   velocity of the LSR towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlsrk( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the LSRK.  If s1 
   is positive (i.e. the LSRK is moving towards the source relative to the 
   sun), then the frequency as seen in the LSRK should be higher than 
   the frequency as seen on the sun, so the factor should be greater 
   than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C + s1 )/( AST__C - s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Dynamic LSR to heliocentric frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void ldf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in m/s). This can also be thought of as the 
   velocity of the LSR towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlsrd( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied LSRD frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the LSRD.  If s1 
   is positive (i.e. the sun is moving away from the source relative to the 
   LSRD), then the frequency as seen on the sun should be lower than the
   frequency as seen in the LSRD, so the factor should be less than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C - s1 )/( AST__C + s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to dynamic LSR frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2ld( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in m/s). This can also be thought of as the 
   velocity of the LSR towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlsrd( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the LSRD.  If s1 
   is positive (i.e. the LSRD is moving towards the source relative to the 
   sun), then the frequency as seen in the LSRD should be higher than 
   the frequency as seen on the sun, so the factor should be greater 
   than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C + s1 )/( AST__C - s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Local Group to heliocentric frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void lgf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the local group (in m/s). This can also be thought of as the 
   velocity of the local group towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlg( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied local group frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the local group.  If 
   s1 is positive (i.e. the sun is moving away from the source relative to the 
   local group), then the frequency as seen on the sun should be lower than
   the frequency as seen in the local group, so the factor should be less than 
   one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C - s1 )/( AST__C + s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to Local Group frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2lg( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the local group (in m/s). This can also be thought of as the 
   velocity of the local group towards the source relative to the sun. */
         s1 = (double) 1000.0*slaRvlg( (float) ra0, (float) dec0 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the local group.  If 
   s1 is positive (i.e. the local group is moving towards the source relative 
   to the sun), then the frequency as seen in the local group should be higher 
   than the frequency as seen on the sun, so the factor should be greater 
   than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C + s1 )/( AST__C - s1 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Galactic to heliocentric frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void glf2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, s3, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in km/s). */
         s1 = (double) slaRvlsrd( (float) ra0, dec0 );

/* Get the component away from the source, of the velocity of the
   dynamic LSR relative to the galactic centre (in km/s). */
         s2 = (double) slaRvgalc( (float) ra0, (float) dec0 );

/* Find the total velocity of the sun away from the source, relative
   to the galactic centre, in m/s. This can also be thought of as the 
   velocity of the galactic centre towards the source relative to the sun. */
         s3 = 1000.0*( s1 + s2 );

/* Find the factor by which to multiply the supplied galactic frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the galactic centre.  
   If s3 is positive (i.e. the sun is moving away from the source relative to 
   the galactic centre), then the frequency as seen on the sun should be lower 
   than the frequency as seen at the galactic centre, so the factor should be 
   less than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C - s3 )/( AST__C + s3 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Heliocentric to galactic frequency correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding frequency correction factor, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlf2gl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, s3, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   frequency correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the frequency correction factor for this
   spectral point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in km/s). */
         s1 = (double) slaRvlsrd( (float) ra0, (float) dec0 );

/* Get the component away from the source, of the velocity of the
   dynamic LSR relative to the galactic centre (in km/s). */
         s2 = (double) slaRvgalc( (float) ra0, (float) dec0 );

/* Find the total velocity of the sun away from the source, relative
   to the galactic centre, in m/s. This can also be thought of as the 
   velocity of the galactic centre towards the source relative to the sun. */
         s3 = 1000.0*( s1 + s2 );

/* Find the factor by which to multiply the supplied heliocentric frequency 
   values to account for the Doppler shift introduced as a consequence of the
   movement of the Sun away from the source relative to the galactic centre.  
   If s3 is positive (i.e. the galactic centre is moving towards the source 
   relative to the sun), then the frequency as seen at the galactic centre 
   should be higher than the frequency as seen at the sun, so the factor 
   should be greater than one. */
         if( AST__C != s1 ) args[ 2 ] = sqrt( ( AST__C + s3 )/( AST__C - s3 ) );

/* Reciprocate the frequency correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : 1.0/args[ 2 ];
         }
      }

/* Correct the frequency value. */
      if( values[ i ] != AST__BAD && used_f != AST__BAD ) {
         values[ i ] *= used_f;
      } else {
         values[ i ] = AST__BAD;
      }
   }
}


/* Topocentric to heliocentric velocity correction:
*    args[ 0 ] = Geodetic longitude of observer (radians, positive eastwards)
*    args[ 1 ] = Geodetic latitude of observer (radians)
*    args[ 2 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 3 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 4 ] = Declination of source (radians, FK5 J2000)
*    args[ 5 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void tpv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
      used_f = forward ? args[ 5 ] : -args[ 5 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 3 ];
            dec0 = args[ 4 ];
         }

/* Get the component away from the source, of the velocity of the observer 
   relative to the centre of the earth (in km/s). */
         s1 = TopoVel( args[ 0 ], args[ 1 ], args[ 2 ], ra0, dec0 );

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in km/s). */
         s2 = HelioVel( args[ 2 ], ra0, dec0  );

/* Find the total velocity of the observer away from the source, relative
   to the sun in m/s. This can also be thought of as the velocity of the
   sun towards the source relative to the observer. */
         args[ 5 ] = 1000.0*( s1 + s2 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : -args[ 5 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to topocentric velocity correction:
*    args[ 0 ] = Geodetic longitude of observer (radians, positive eastwards)
*    args[ 1 ] = Geodetic latitude of observer (radians)
*    args[ 2 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 3 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 4 ] = Declination of source (radians, FK5 J2000)
*    args[ 5 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2tp( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, s1, s2, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
      used_f = forward ? args[ 5 ] : -args[ 5 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 3 ];
            dec0 = args[ 4 ];
         }

/* Get the component away from the source, of the velocity of the observer 
   relative to the centre of the earth (in km/s). */
         s1 = TopoVel( args[ 0 ], args[ 1 ], args[ 2 ], ra0, dec0 );

/* Get the component away from the source, of the velocity of the earths
   centre relative to the sun (in km/s). */
         s2 = HelioVel( args[ 2 ], ra0, dec0  );

/* Find the total velocity of the observer towards the source, relative
   to the sun in m/s. */
         args[ 5 ] = -1000.0*( s1 + s2 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : -args[ 5 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Geocentric to heliocentric velocity correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void gev2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : -args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component towards the source, of the velocity of the sun
   relative to the earths centre (in m/s). */
         args[ 3 ] = 1000.0*HelioVel( args[ 0 ], ra0, dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : -args[ 3 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to geocentric velocity correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2ge( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : -args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component towards the source, of the velocity of the earths
   centre relative to the sun (in m/s). */
         args[ 3 ] = -1000.0*HelioVel( args[ 0 ], ra0, dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
            used_f = forward ? args[ 3 ] : -args[ 3 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Barycentric to heliocentric velocity correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void byv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : -args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component towards the source, of the velocity of the sun
   relative to the barycentre (in m/s). */
         args[ 3 ] = 1000.0*BaryVel( args[ 0 ], ra0, dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : -args[ 5 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to barycentric velocity correction:
*    args[ 0 ] = Epoch of observation (UT1 expressed as a Modified Julian Date)
*    args[ 1 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 2 ] = Declination of source (radians, FK5 J2000)
*    args[ 3 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2by( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 3 ] != AST__BAD && args[ 3 ] != 0.0 ) {
      used_f = forward ? args[ 3 ] : -args[ 3 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 1 ];
            dec0 = args[ 2 ];
         }

/* Get the component towards the source, of the velocity of the barycentre
   relative to the sun (in m/s). */
         args[ 3 ] = -1000.0*BaryVel( args[ 0 ], ra0, dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 5 ] != AST__BAD && args[ 5 ] != 0.0 ) {
            used_f = forward ? args[ 5 ] : -args[ 5 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Kinematic LSR to heliocentric velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void lkv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the sun
   relative to the LSRK (in m/s). */
         args[ 2 ] = (double) -1000.0*slaRvlsrk( (float) ra0, (float) dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to kinematic LSR velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2lk( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the LSRK 
   relative to the sun (in m/s). */
         args[ 2 ] = (double) 1000.0*slaRvlsrk( (float) ra0, (float) dec0 );
 
/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}


/* Dynamic LSR to heliocentric velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void ldv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the sun
   relative to the LSRD (in m/s). */
         args[ 2 ] = (double) -1000.0*slaRvlsrd( (float) ra0, (float) dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to dynamic LSR velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2ld( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the LSRD
   relative to the sun (in m/s). */
         args[ 2 ] = (double) 1000.0*slaRvlsrd( (float) ra0, (float) dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Local Group to heliocentric velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void lgv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the sun
   relative to the Local Group (in m/s). */
         args[ 2 ] = (double) -1000.0*slaRvlg( (float) ra0, (float) dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to Local Group velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2lg( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component towards the source, of the velocity of the Local group
   relative to the sun (in m/s). */
         args[ 2 ] = (double) 1000.0*slaRvlg( (float) ra0, (float) dec0 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Galactic to heliocentric velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void glv2hl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0, s1, s2;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in km/s). */
         s1 = (double) slaRvlsrd( (float) ra0, (float) dec0 );

/* Get the component away from the source, of the velocity of the
   dynamic LSR relative to the galactic centre (in km/s). */
         s2 = (double) slaRvgalc( (float) ra0, (float) dec0 );

/* Get the component towards the source, of the velocity of the sun
   relative to the galactic centre (in m/s). */
         args[ 2 ] = -1000.0*( s1 + s2 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Heliocentric to galactic velocity correction:
*    args[ 0 ] = Right Ascension of source (radians, FK5 J2000)
*    args[ 1 ] = Declination of source (radians, FK5 J2000)
*    args[ 2 ] = Corresponding velocity correction term, or AST__BAD.
   ------------------------------------------------------------------- */
static void hlv2gl( int npoint, double *values, double *ra, double *dec, double *args, int forward ) {
   double used_f, ra0, dec0, s1, s2;
   int i;

/* If this is a 1-D SpecMap (i.e. for a single spatial position), get the 
   velocity correction factor to use (which takes account of whether
   we are doing an inverse or forward correction). */
   used_f = AST__BAD;
   if( !ra && args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
      used_f = forward ? args[ 2 ] : -args[ 2 ];
   }

/* Do each spectral point. */
   for( i = 0; i < npoint; i++ ) {

/* If we do not yet know the velocity correction factor for this spectral 
   point, calculate it now. */
      if( used_f == AST__BAD || ra ){

/* Choose the source RA and DEC; either from the supplied arrays or from the
   conversion argument array. */
         if( ra ) {
            ra0 = ra[ i ];
            dec0 = dec[ i ];
         } else {
            ra0 = args[ 0 ];
            dec0 = args[ 1 ];
         }

/* Get the component away from the source, of the velocity of the sun
   relative to the dynamic LSR (in km/s). */
         s1 = (double) slaRvlsrd( (float) ra0, (float) dec0 );

/* Get the component away from the source, of the velocity of the
   dynamic LSR relative to the galactic centre (in km/s). */
         s2 = (double) slaRvgalc( (float) ra0, (float) dec0 );

/* Get the component towards the source, of the velocity of the galactic
   centre relative to the sun (in m/s). */
         args[ 2 ] = 1000.0*( s1 + s2 );

/* Negate the velocity correction factor if the inverse conversion is
   being performed. */
         if( args[ 2 ] != AST__BAD && args[ 2 ] != 0.0 ) {
            used_f = forward ? args[ 2 ] : -args[ 2 ];
         }
      }

/* Correct the supplied velocity value. */
      VelCorr( values + i, used_f );
   }
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsASpecMap and astCheckSpecMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SpecMap,Mapping,check,&class_init)
astMAKE_CHECK(SpecMap)

AstSpecMap *astSpecMap_( int nin, int flags, const char *options, ... ) {
/*
*++
*  Name:
c     astSpecMap
f     AST_SPECMAP

*  Purpose:
*     Create a SpecMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "specmap.h"
c     AstSpecMap *astSpecMap( int nin, int flags, const char *options, ... )
f     RESULT = AST_SPECMAP( NIN, FLAGS, OPTIONS, STATUS )

*  Class Membership:
*     SpecMap constructor.

*  Description:
*     This function creates a new SpecMap and optionally initialises
*     its attributes.
*
*     An SpecMap is a specialised form of Mapping which can be used to
*     represent a sequence of conversions between standard spectral
*     coordinate systems. This includes conversions between frequency,
*     wavelength, and various forms of velocity, as well as conversions
*     between different standards of rest. 
*
*     When a SpecMap is first created, it simply performs a unit
c     (null) Mapping. Using the astSpecAdd function,
f     (null) Mapping. Using the AST_SPECADD routine,
*     a series of coordinate conversion steps may then be added, selected 
*     from the list of supported conversions. This allows multi-step 
*     conversions between a variety of spectral coordinate systems to 
*     be assembled out of the building blocks provided by this class.
*
*     For details of the individual coordinate conversions available,
c     see the description of the astSpecAdd function.
f     see the description of the AST_SPECADD routine.
*
*     Conversions are available to transform between standards of rest.
*     Such conversions need to know the source position as an RA and DEC.
*     This information can be supplied in the form of parameters for
*     the relevant conversions, in which case the SpecMap is 1-dimensional, 
*     simply transforming the spectral axis values. This means that the
*     same source position will always be used by the SpecMap. However, this 
*     may not be appropriate for an accurate description of a 3-D spectral
*     cube, where changes of spatial position can produce significant
*     changes in the Doppler shift introduced when transforming between
*     standards of rest. For this situation, a 3-dimensional SpecMap can
*     be created in which axes 2 and 3 correspond to the source RA and DEC
*     The SpecMap simply copies values for axes 2 and 3 from input to
*     output).

*  Parameters:
c     nin
f     NIN = INTEGER (Given)
*        The number of inputs to the Mapping (this will also equal the 
*        number of outputs). This value must be either 1 or 3. In either
*        case, the first input and output correspoindis the spectral axis.
*        For a 3-axis SpecMap, the second and third axes give the RA and
*        DEC (J2000 FK5) of the source. This positional information is
*        used by conversions which transform between standards of rest,
*        and replaces the "RA" and "DEC" arguments for the individual
*        conversions listed in description of the "SpecAdd" 
c        function.
f        routine.
c     flags
f     FLAGS = INTEGER (Given)
c        This parameter is reserved for future use and should currently
f        This argument is reserved for future use and should currently
*        always be set to zero.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SpecMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SpecMap. The syntax used is identical to that for the
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
c     astSpecMap()
f     AST_SPECMAP = INTEGER
*        A pointer to the new SpecMap.

*  Notes:
*     - The nature and units of the coordinate values supplied for the
*     first input (i.e. the spectral input) of a SpecMap must be appropriate 
*     to the first conversion step applied by the SpecMap. For instance, if 
*     the first conversion step is "FRTOVL" (frequency to relativistic 
*     velocity), then the coordinate values for the first input should
*     be frequency in units of Hz. Similarly, the nature and units of the 
*     coordinate values returned by a SpecMap will be determined by the
*     last conversion step applied by the SpecMap. For instance, if the 
*     last conversion step is "VLTOVO" (relativistic velocity to optical
*     velocity), then the coordinate values for the first output will be optical
*     velocity in units of metres per second. See the description of the
c     astSpecAdd function for the units expected and returned by each
f     AST_SPECADD routine for the units expected and returned by each
*     conversion.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstSpecMap *new;              /* Pointer to the new SpecMap */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SpecMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSpecMap( NULL, sizeof( AstSpecMap ), !class_init, &class_vtab,
                        "SpecMap", nin, flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SpecMap's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new SpecMap. */
   return new;
}

AstSpecMap *astSpecMapId_( int nin, int flags, const char *options, ... ) {
/*
*  Name:
*     astSpecMapId_

*  Purpose:
*     Create a SpecMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specmap.h"
*     AstSpecMap *astSpecMapId_( int nin, int flags, const char *options, ... )

*  Class Membership:
*     SpecMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astSpecMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astSpecMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astSpecMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astSpecMap_.

*  Returned Value:
*     The ID value associated with the new SpecMap.
*/

/* Local Variables: */
   AstSpecMap *new;              /* Pointer to the new SpecMap */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SpecMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSpecMap( NULL, sizeof( AstSpecMap ), !class_init, &class_vtab,
                        "SpecMap", nin, flags );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SpecMap's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new SpecMap. */
   return astMakeId( new );
}

AstSpecMap *astInitSpecMap_( void *mem, size_t size, int init,
                             AstSpecMapVtab *vtab, const char *name,
                             int nin, int flags ) {
/*
*+
*  Name:
*     astInitSpecMap

*  Purpose:
*     Initialise a SpecMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specmap.h"
*     AstSpecMap *astInitSpecMap( void *mem, size_t size, int init,
*                               AstSpecMapVtab *vtab, const char *name,
*                               int nin, int flags )

*  Class Membership:
*     SpecMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new SpecMap object. It allocates memory (if necessary) to accommodate
*     the SpecMap plus any additional data associated with the derived class.
*     It then initialises a SpecMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a SpecMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SpecMap is to be initialised.
*        This must be of sufficient size to accommodate the SpecMap data
*        (sizeof(SpecMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the SpecMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the SpecMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the SpecMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new SpecMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astClass
*        method).
*     nin
*        The number of inputs and outputs for the SpecMap (either 1 or 3).
*     flags
*        This parameter is reserved for future use. It is currently ignored.

*  Returned Value:
*     A pointer to the new SpecMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstSpecMap *new;               /* Pointer to the new SpecMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Check nin is OK (1 or 3). */
   if( nin != 1 && nin != 3 ) {
      astError( AST__BADNI, "astInitSpecMap(SpecMap): Supplied number of "
                "SpecMap axes (%d) is illegal; it should be 1 or 2. ",
                nin );
   }

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSpecMapVtab( vtab, name );

/* Initialise a 1D Mapping structure (the parent class) as the first component
   within the SpecMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstSpecMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       nin, nin, 1, 1 );

   if ( astOK ) {

/* Initialise the SpecMap data. */
/* --------------------------- */
/* The initial state is with no conversions set, in which condition the
   SpecMap simply implements a unit mapping. */
      new->ncvt = 0;
      new->cvtargs = NULL;
      new->cvttype = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstSpecMap *astLoadSpecMap_( void *mem, size_t size,
                           AstSpecMapVtab *vtab, const char *name,
                           AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadSpecMap

*  Purpose:
*     Load a SpecMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specmap.h"
*     AstSpecMap *astLoadSpecMap( void *mem, size_t size,
*                               AstSpecMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     SpecMap loader.

*  Description:
*     This function is provided to load a new SpecMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SpecMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a SpecMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the SpecMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SpecMap data (sizeof(SpecMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SpecMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SpecMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSpecMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SpecMap. If this is NULL, a pointer to
*        the (static) virtual function table for the SpecMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SpecMap" is used instead.

*  Returned Value:
*     A pointer to the new SpecMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstSpecMap *new;              /* Pointer to the new SpecMap */
   char *sval;                   /* Pointer to string value */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   const char *argdesc[ MAX_ARGS ]; /* Pointers to argument descriptions */
   const char *comment;          /* Pointer to comment string */
   int argdec;                   /* Index of DEC argument */
   int argra;                    /* Index of RA argument */
   int iarg;                     /* Loop counter for arguments */
   int icvt;                     /* Loop counter for conversion steps */
   int nargs;                    /* Number of user-supplied arguments */
   int szargs;                   /* Number of stored arguments */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SpecMap. In this case the
   SpecMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSpecMap );
      vtab = &class_vtab;
      name = "SpecMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSpecMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SpecMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SpecMap" );

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
      new->ncvt = astReadInt( channel, "nspec", 0 );
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
            (void) sprintf( key, "spec%d", icvt + 1 );
            sval = astReadString( channel, key, NULL );

/* If no value was read, report an error. */
            if ( astOK ) {
               if ( !sval ) {
                  astError( AST__BADIN,
                            "astRead(%s): A spectral coordinate conversion "
                            "type is missing from the input SpecMap data.",
                            astGetClass( channel ) );

/* Otherwise, convert the string representation into the required
   conversion type code. */
               } else {
                  new->cvttype[ icvt ] = CvtCode( sval );

/* If the string was not recognised, report an error. */
                  if ( new->cvttype[ icvt ] == AST__SPEC_NULL ) {
                     astError( AST__BADIN,
                              "astRead(%s): Invalid spectral conversion "
                              "type \"%s\" in SpecMap data.",
                              astGetClass( channel ), sval );
                  }
               }

/* Free the memory holding the string value. */
               sval = astFree( sval );
            }

/* Obtain the number of arguments associated with the conversion and
   allocate memory to hold them. */
            (void) CvtString( new->cvttype[ icvt ], &comment, &argra,
                              &argdec, &nargs, &szargs, argdesc );
            new->cvtargs[ icvt ] = astMalloc( sizeof( double ) *
                                              (size_t) szargs );

/* Read in data for each argument... */
            if ( astOK ) {
               for ( iarg = 0; iarg < szargs; iarg++ ) {

/* Arguments. */
/* ---------- */
/* Create an appropriate keyword and read each argument value. */
                  (void) sprintf( key, "spec%d%c", icvt + 1, ALPHABET[ iarg ] );
                  new->cvtargs[ icvt ][ iarg ] = astReadDouble( channel, key,
                                                                AST__BAD );
               }
            }

/* Quit looping if an error occurs. */
            if ( !astOK ) break;
         }
      }

/* If an error occurred, clean up by deleting the new SpecMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new SpecMap pointer. */
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
void astSpecAdd_( AstSpecMap *this, const char *cvt, const double args[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,SpecMap,SpecAdd))( this, cvt, args );
}
