/*
*class++
*  Name:
*     SpecFrame

*  Purpose:
*     Spectral coordinate system description.

*  Constructor Function:
c     astSpecFrame
f     AST_SPECFRAME

*  Description:
*     A SpecFrame is a specialised form of one-dimensional Frame which 
*     represents various coordinate systems used to describe positions within 
*     an electro-magnetic spectrum. The particular coordinate system to be 
*     used is specified by setting the SpecFrame's System attribute (the 
*     default is wavelength) qualified, as necessary, by other attributes 
*     such as the rest frequency, the standard of rest, the epoch of 
*     observation, units, etc (see the description of the System attribute
*     for details).

*  Inheritance:
*     The SpecFrame class inherits from the Frame class.

*  Attributes:
*     In addition to those attributes common to all Frames, every
*     SpecFrame also has the following attributes:
*
*     - AlignStdOfRest: Standard of rest in which to align SpecFrames
*     - GeoLat: Geodetic latitude of observer
*     - GeoLon: Geodetic longitude of observer
*     - RefDec: Declination of the source (FK5 J2000)
*     - RefRA: Right ascension of the source (FK5 J2000)
*     - RestFreq: Rest frequency
*     - SourceVel: Source velocity
*     - SourceVRF: Source velocity rest frame
*     - StdOfRest: Standard of rest 
*
*     Several of the Frame attributes inherited by the SpecFrame class 
*     refer to a specific axis of the Frame (for instance Unit(axis), 
*     Label(axis), etc). Since a SpecFrame is strictly one-dimensional,
*     it allows these attributes to be specified without an axis index.
*     So for instance, "Unit" is allowed in place of "Unit(1)".

*  Functions:
c     In addition to those functions applicable to all Frames, the
c     following functions may also be applied to all SpecFrames:
f     In addition to those routines applicable to all Frames, the
f     following routines may also be applied to all SpecFrames:
*
c     - astSetRefPos: Set reference position in any celestial system
f     - AST_SETREFPOS: Set reference position in any celestial system
c     - astGetRefPos: Get reference position in any celestial system
f     - AST_GETREFPOS: Get reference position in any celestial system

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     4-NOV-2002 (DSB):
*        Original version.
*     2-FEB-2005 (DSB):
*        - Avoid using astStore to allocate more storage than is supplied
*        in the "data" pointer. This can cause access violations since 
*        astStore will then read beyond the end of the "data" area.
*     22-MAR-2005 (DSB):
*        - Re-structure MakeSpecMapping in order to avoid unnecessary
*        access to SpecFrame attributes which may not be set, and to
*        check that all required attributes have been set if UseDefs is 
*        zero.
*     23-MAR-2005 (DSB):
*        - Added missing rest frames to SorEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SpecFrame

/* Define the first and last acceptable System values. */
#define FIRST_SYSTEM AST__FREQ
#define LAST_SYSTEM AST__VREL

/* Define the first and last acceptable StdOfRest values. */
#define FIRST_SOR AST__TPSOR
#define LAST_SOR AST__SCSOR

/* The supported spectral coordinate systems fall into two groups;
   "relative", and "absolute". The relative systems define each axis
   value with respect to the rest frequency, whereas the absolute systems
   have axis values which do not depend on the rest frequency. Define a 
   macro which returns one if the specified system is absolute, and zero 
   otherwise. */
#define ABS_SYSTEM(sys) \
      ( ( sys == AST__ENERGY || \
          sys == AST__WAVENUM || \
          sys == AST__WAVELEN || \
          sys == AST__AIRWAVE || \
          sys == AST__FREQ ) ? 1 : 0 )

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "unit.h"                /* Units management facilities */
#include "object.h"              /* Base Object class */
#include "specmap.h"             /* Spectral coordinate Mappings */
#include "frame.h"               /* Parent Frame class */
#include "skyframe.h"            /* Celestial coordinate frames */
#include "specframe.h"           /* Interface definition for this class */
#include "mapping.h"             /* Coordinate Mappings */
#include "cmpmap.h"              /* Compound Mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "slalib.h"              /* SlaLib interface */


/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <math.h>

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag as
   static variables. */
static AstSpecFrameVtab class_vtab; /* Virtual function table */
static int class_init = 0;          /* Virtual function table initialised? */

/* Define a variable to hold a SkyFrame which will be used for formatting
   and unformatting sky positions, etc. */
static AstSkyFrame *skyframe;      

/* Pointers to parent class methods which are used or extended by this
   class. */
static AstSystemType (* parent_getalignsystem)( AstFrame * );
static AstSystemType (* parent_getsystem)( AstFrame * );
static const char *(* parent_getattrib)( AstObject *, const char * );
static const char *(* parent_getdomain)( AstFrame * );
static const char *(* parent_getlabel)( AstFrame *, int );
static const char *(* parent_getsymbol)( AstFrame *, int );
static const char *(* parent_gettitle)( AstFrame * );
static const char *(* parent_getunit)( AstFrame *, int );
static int (* parent_match)( AstFrame *, AstFrame *, int **, int **, AstMapping **, AstFrame ** );
static int (* parent_subframe)( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame ** );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_setunit)( AstFrame *, int, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_overlay)( AstFrame *, const int *, AstFrame * );
static void (* parent_setattrib)( AstObject *, const char * );
static void (* parent_setmaxaxes)( AstFrame *, int );
static void (* parent_setminaxes)( AstFrame *, int );
static void (* parent_setsystem)( AstFrame *, AstSystemType );
static void (* parent_clearsystem)( AstFrame * );
static void (* parent_clearunit)( AstFrame *, int );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstStdOfRestType StdOfRestCode( const char * );
static AstSystemType GetAlignSystem( AstFrame * );
static AstSystemType SystemCode( AstFrame *, const char * );
static AstSystemType ValidateSystem( AstFrame *, AstSystemType, const char * );
static const char *DefUnit( AstSystemType, const char *, const char * );
static const char *GetDomain( AstFrame * );
static const char *GetLabel( AstFrame *, int );
static const char *GetSymbol( AstFrame *, int );
static const char *GetTitle( AstFrame * );
static const char *GetUnit( AstFrame *, int );
static const char *SpecMapUnit( AstSystemType, const char *, const char * );
static const char *StdOfRestString( AstStdOfRestType );
static const char *SystemLabel( AstSystemType );
static const char *SystemString( AstFrame *, AstSystemType );
static double ConvertSourceVel( AstSpecFrame *, AstStdOfRestType );
static int EqualSor( AstSpecFrame *, AstSpecFrame * );
static int GetActiveUnit( AstFrame * );
static int MakeSpecMapping( AstSpecFrame *, AstSpecFrame *, AstSpecFrame *, int, AstMapping ** );
static int Match( AstFrame *, AstFrame *, int **, int **, AstMapping **, AstFrame ** );
static int SorConvert( AstSpecFrame *, AstSpecFrame *, AstSpecMap * );
static int SubFrame( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame ** );
static int TestActiveUnit( AstFrame * );
static void ClearUnit( AstFrame *, int );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void GetRefPos( AstSpecFrame *, AstSkyFrame *, double *, double * );
static void Overlay( AstFrame *, const int *, AstFrame * );
static void SetMaxAxes( AstFrame *, int );
static void SetMinAxes( AstFrame *, int );
static void SetRefPos( AstSpecFrame *, AstSkyFrame *, double, double );
static void SetUnit( AstFrame *, int, const char * );
static void VerifyAttrs( AstSpecFrame *, const char *, const char *, const char * );

static AstSystemType GetSystem( AstFrame * );
static void SetSystem( AstFrame *, AstSystemType );
static void ClearSystem( AstFrame * );

static const char *GetAttrib( AstObject *, const char * );
static int TestAttrib( AstObject *, const char * );
static void ClearAttrib( AstObject *, const char * );
static void SetAttrib( AstObject *, const char * );

static AstStdOfRestType GetAlignStdOfRest( AstSpecFrame * );
static int TestAlignStdOfRest( AstSpecFrame * );
static void ClearAlignStdOfRest( AstSpecFrame * );
static void SetAlignStdOfRest( AstSpecFrame *, AstStdOfRestType );

static AstStdOfRestType GetStdOfRest( AstSpecFrame * );
static int TestStdOfRest( AstSpecFrame * );
static void ClearStdOfRest( AstSpecFrame * );
static void SetStdOfRest( AstSpecFrame *, AstStdOfRestType );

static double GetGeoLat( AstSpecFrame * );
static int TestGeoLat( AstSpecFrame * );
static void ClearGeoLat( AstSpecFrame * );
static void SetGeoLat( AstSpecFrame *, double );

static double GetGeoLon( AstSpecFrame * );
static int TestGeoLon( AstSpecFrame * );
static void ClearGeoLon( AstSpecFrame * );
static void SetGeoLon( AstSpecFrame *, double );

static double GetRestFreq( AstSpecFrame * );
static int TestRestFreq( AstSpecFrame * );
static void ClearRestFreq( AstSpecFrame * );
static void SetRestFreq( AstSpecFrame *, double );

static double GetSourceVel( AstSpecFrame * );
static int TestSourceVel( AstSpecFrame * );
static void ClearSourceVel( AstSpecFrame * );
static void SetSourceVel( AstSpecFrame *, double );

static double GetRefRA( AstSpecFrame * );
static int TestRefRA( AstSpecFrame * );
static void ClearRefRA( AstSpecFrame * );
static void SetRefRA( AstSpecFrame *, double );

static double GetRefDec( AstSpecFrame * );
static int TestRefDec( AstSpecFrame * );
static void ClearRefDec( AstSpecFrame * );
static void SetRefDec( AstSpecFrame *, double );

static AstStdOfRestType GetSourceVRF( AstSpecFrame * );
static int TestSourceVRF( AstSpecFrame * );
static void ClearSourceVRF( AstSpecFrame * );
static void SetSourceVRF( AstSpecFrame *, AstStdOfRestType );

/* Member functions. */
/* ================= */

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     SpecFrame member function (over-rides the astClearAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function clears the value of a specified attribute for a
*     SpecFrame, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   char *new_attrib;             /* Pointer value to new attribute name */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* First look for axis attributes defined by the Frame class. Since a
   SpecFrame has only 1 axis, we allow these attributes to be specified
   without a trailing "(axis)" string. */
   if ( !strcmp( attrib, "direction" ) || 
        !strcmp( attrib, "bottom" ) ||
        !strcmp( attrib, "top" ) ||
        !strcmp( attrib, "format" ) ||
        !strcmp( attrib, "label" ) ||
        !strcmp( attrib, "symbol" ) ||
        !strcmp( attrib, "unit" ) ) {

/* Create a new attribute name from the original by appending the string
   "(1)" and then use the parent ClearAttrib method. */
      new_attrib = astMalloc( len + 4 );
      if( new_attrib ) {
         memcpy( new_attrib, attrib, len );
         memcpy( new_attrib + len, "(1)", 4 ); 
         (*parent_clearattrib)( this_object, new_attrib );
         new_attrib = astFree( new_attrib );
      }

/* AlignStdOfRest. */
/* --------------- */
   } else if ( !strcmp( attrib, "alignstdofrest" ) ) {
      astClearAlignStdOfRest( this );

/* GeoLat. */
/* ------- */
   } else if ( !strcmp( attrib, "geolat" ) ) {
      astClearGeoLat( this );

/* GeoLon. */
/* ------- */
   } else if ( !strcmp( attrib, "geolon" ) ) {
      astClearGeoLon( this );

/* RefDec. */
/* ---------- */
   } else if ( !strcmp( attrib, "refdec" ) ) {
      astClearRefDec( this );

/* RefRA. */
/* --------- */
   } else if ( !strcmp( attrib, "refra" ) ) {
      astClearRefRA( this );

/* RestFreq. */
/* --------- */
   } else if ( !strcmp( attrib, "restfreq" ) ) {
      astClearRestFreq( this );

/* SourceVel. */
/* ---------- */
   } else if ( !strcmp( attrib, "sourcevel" ) ) {
      astClearSourceVel( this );

/* SourceVRF */
/* --------- */
   } else if ( !strcmp( attrib, "sourcevrf" ) ) {
      astClearSourceVRF( this );

/* StdOfRest. */
/* ---------- */
   } else if ( !strcmp( attrib, "stdofrest" ) ) {
      astClearStdOfRest( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static void ClearSystem( AstFrame *this_frame ) {
/*
*  Name:
*     ClearSystem

*  Purpose:
*     Clear the System attribute for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void ClearSystem( AstFrame *this_frame )

*  Class Membership:
*     SpecFrame member function (over-rides the astClearSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function clears the System attribute for a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   AstSystemType newsys;         /* System after clearing */
   AstSystemType oldsys;         /* System before clearing */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* Save the original system */
   oldsys = astGetSystem( this_frame );

/* Use the parent ClearSystem method to clear the System value. */
   (*parent_clearsystem)( this_frame );

/* Get the default System. */
   newsys = astGetSystem( this_frame );

/* If the system has actually changed. */
   if( newsys != oldsys ) {

/* Changing the System value will in general require the Units to change
   as well. If the used has previously specified the units to be used with
   the new system, then re-instate them (they are stored in the "usedunits"
   array in the SpecFrame structure). Otherwise, clear the units so that
   the default units will eb used with the new System. */
      if( (int) newsys < this->nuunits && this->usedunits &&
          this->usedunits[ (int) newsys ] ) {
         astSetUnit( this, 0, this->usedunits[ (int) newsys ] );
      } else {
         astClearUnit( this, 0 );
      }

/* Also, clear all attributes which have system-specific defaults. */
      astClearLabel( this_frame, 0 );
      astClearSymbol( this_frame, 0 );
      astClearTitle( this_frame );
   }

}

static void ClearUnit( AstFrame *this_frame, int axis ) {
/*
*  Name:
*     astClearUnit

*  Purpose:
*     Clear the value of the Unit string for a SpecFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void ClearUnit( AstFrame *this_frame, int axis )

*  Class Membership:
*     SpecFrame member function (over-rides the astClearUnit method inherited
*     from the Frame class).

*  Description:
*     This function clears the Unit string for a specified axis of a 
*     SpecFrame. It also clears the UsedUnit item in the SpecFrame
*     structure corresponding to the current System.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     axis
*        The number of the axis (zero-based).
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   int system;                   /* The SpecFrame's System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astClearUnit" );

/* Clear the UsedUnit item for the current System, if current set. */
   system = (int) astGetSystem( this );
   if( system < this->nuunits && this->usedunits ) {
      this->usedunits[ system ] = astFree( this->usedunits[ system ] );
   }

/* Use the parent method to clear the Unit attribute of the axis. */
   (*parent_clearunit)( this_frame, axis );
}

static double ConvertSourceVel( AstSpecFrame *this, AstStdOfRestType new ) {
/*
*  Name:
*     ConvertSourceVel

*  Purpose:
*     Convert the SourceVel value to a specified rest frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     double ConvertSourceVel( AstSpecFrame *this, AstStdOfRestType new )

*  Class Membership:
*     SpecFrame member function 

*  Description:
*     This function convert the SourceVel value to a specified rest frame
*     and returns the new value.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     new
*        The rest frame in which the source velocity is required.

*  Returned Value:
*     The converted source velocity (m/s).

*  Notes:
*     - This function returns zero if an error occurs.
*/

/* Local Variables: */
   AstSpecFrame *from;     /* Pointer to a source SpecFrame */
   AstSpecFrame *to;       /* Pointer to a destination SpecFrame */
   AstSpecMap *specmap;    /* Pointer to a SpecMap */
   AstStdOfRestType sor;   /* Standard of rest in which SourceVel is defined */
   double ret;             /* The returned value */
   double rf;              /* Rest frequency (Hz) */
   double temp;            /* Temporary storage */

/* Initialise */
   ret = 0.0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Get the value of the SourceVel attribute. */
   ret = astGetSourceVel( this );

/* Check it can be used (depends on whether a value has been set and
   whether the USeDefs attribute is zero). */
   VerifyAttrs( this, "convert source velocity to a new standard of rest", 
                "SourceVel", "astMatch" );

/* Get the rest frame to which value refers. */
   sor = astGetSourceVRF( this );

/* If necessary, convert to the requested rest frame. */
   if( sor != new ) {

/* Verify that usable value is available for the RestFreq attribute. An 
   error is reported if not. */
      VerifyAttrs( this, "convert source velocity to a new standard of rest", 
                   "RestFreq", "astMatch" );

/* Take two copies of the supplied SpecFrame and set their StdOfRest
   attribute to the required values. */
      from = astCopy( this );
      astSetStdOfRest( from, sor );

      to = astCopy( this );
      astSetStdOfRest( to, new );

/* Initialise a new SpecMap to describe the conversion. The new sSpecMap
   initially represents a UnitMap. */
      specmap = astSpecMap( 1, 0, "" );

/* Add a conversion from velocity to frequency since SorConvert converts
   frequencies. */
      rf = astGetRestFreq( this );
      astSpecAdd( specmap, "VLTOFR", &rf );

/* Now add a conversion from frequency in the SourveVRF standard of rest to 
   frequency in the required rest frame. */
      SorConvert( from, to, specmap );

/* Finally, add a conversion from frequency back to velocity. Note, the
   value of the rest frequency does not affect the overall conversion. */
      astSpecAdd( specmap, "FRTOVL", &rf );

/* Use the SpecMap to convert the source velocity in the SourceVRF
   standard of rest to the required rest frame. */
      temp = ret;
      astTran1( specmap, 1, &temp, 1, &ret );

/* Free resources */
      specmap = astAnnul( specmap );
      to = astAnnul( to );
      from = astAnnul( from );
   }

/* Return zero if an error has occurred. */
   if( !astOK ) ret = 0.0;

/* Return the answer. */
   return ret;

}

static const char *DefUnit( AstSystemType system, const char *method,
                            const char *class ){
/*
*  Name:
*     DefUnit

*  Purpose:
*     Return the default units for a spectral coordinate system type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *DefUnit( AstSystemType system, const char *method,
*                          const char *class )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function returns a textual representation of the default 
*     units associated with the specified spectral coordinate system.

*  Parameters:
*     system
*        The spectral coordinate system.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     As tring describing the default units. This string follows the
*     units syntax described in FITS WCS paper I "Representations of world
*     coordinates in FITS" (Greisen & Calabretta).

*  Notes:
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   const char *result;           /* Value to return */

/* Initialize */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get an identifier for the default units. */
   if( system == AST__FREQ ) {
      result = "GHz";
   } else if( system == AST__ENERGY ) {
      result = "J";
   } else if( system == AST__WAVENUM ) {
      result = "1/m";
   } else if( system == AST__WAVELEN ) {
      result = "Angstrom";
   } else if( system == AST__AIRWAVE ) {
      result = "Angstrom";
   } else if( system == AST__VRADIO ) {
      result = "km/s";
   } else if( system == AST__VOPTICAL ) {
      result = "km/s";
   } else if( system == AST__REDSHIFT ) {
      result = "";
   } else if( system == AST__BETA ) {
      result = "";
   } else if( system == AST__VREL ) {
      result = "km/s";

/* Report an error if the coordinate system was not recognised. */
   } else {
      astError( AST__SCSIN, "%s(%s): Corrupt %s contains illegal System "
                "identification code (%d).", method, class, class, 
                (int) system );
   }

/* Return the result. */
   return result;
}

static int EqualSor( AstSpecFrame *this, AstSpecFrame *that ) {
/*
*  Name:
*     EqualSor

*  Purpose:
*     Do two SpecFrames use the same standard of rest?

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int EqualSor( AstSpecFrame *this, AstSpecFrame *that ) 

*  Class Membership:
*     SpecFrame member function 

*  Description:
*    This function returns non-zero if the two supplied SpecFrames use
*    the same standard of rest.

*  Parameters:
*     this
*        Pointer to the first SpecFrame.
*     that
*        Pointer to the second SpecFrame.

*  Returned Value:
*     Non-zero if the two SpecFrames use the same standard of rest. Zero
*     otherwise.

*/

/* Local Variables: */
   AstStdOfRestType sor;             /* Standard of rest */
   int result;                       /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise. */
   result = 1;

/* Compare StdOfRest attributes. */
   sor = astGetStdOfRest( this );
   if( astGetStdOfRest( that ) != sor ) {
      result = 0;

/* If the standards of rest are equal we need to check the the attributes
   which specify the precise rest frame. */
   } else {

/* The reference RA and Dec need to be equal */
      if( !EQUAL( astGetRefRA( this ), astGetRefRA( that ) ) || 
          !EQUAL( astGetRefDec( this ), astGetRefDec( that ) ) ) {
         result = 0;

/* For source rest frame, the source velocities must be equal */
      } else if( sor == AST__SCSOR ){
         if( !EQUAL( astGetSourceVel( this ), astGetSourceVel( that ) ) || 
                     astGetSourceVRF( this ) != astGetSourceVRF( that ) ) {
            result = 0;
         }

/* For geocentric, barycentric and heliocentric rest frames, the epochs must 
   be the same */
      } else if( sor == AST__GESOR || sor == AST__BYSOR || sor == AST__HLSOR ){
         if( !EQUAL( astGetEpoch( this ), astGetEpoch( that ) ) ) result = 0;

/* For topocentric rest frame, the epoch and observer position must be the 
   same */
      } else if( sor == AST__TPSOR ){
         if( !EQUAL( astGetEpoch( this ), astGetEpoch( that ) ) ||
             !EQUAL( astGetGeoLon( this ), astGetGeoLon( that ) ) ||
             !EQUAL( astGetGeoLat( this ), astGetGeoLat( that ) ) ) result = 0;
      
      } else if( sor != AST__LKSOR && sor != AST__LDSOR &&
                 sor != AST__GLSOR && sor != AST__LGSOR && astOK ) {
         astError( AST__INTER, "SorEqual(SpecFrame): Function SorEqual "
                   "does not yet support rest frame %d (AST internal "
                   "programming error)", sor );
      }
   }

/* Return the result */
   return result;
}

static int GetActiveUnit( AstFrame *this_frame ) {
/*
*  Name:
*     GetActiveUnit

*  Purpose:
*     Obtain the value of the ActiveUnit flag for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int GetActiveUnit( AstFrame *this_frame ) 

*  Class Membership:
*     SpecFrame member function (over-rides the astGetActiveUnit protected
*     method inherited from the Frame class).

*  Description:
*    This function returns the value of the ActiveUnit flag for a
*    SpecFrame, which is always 1.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     The value to use for the ActiveUnit flag (1).

*/
   return 1;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     SpecFrame member function (over-rides the protected astGetAttrib
*     method inherited from the Frame class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a SpecFrame, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - The returned string pointer may point at memory allocated
*     within the SpecFrame, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the SpecFrame. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   AstStdOfRestType sor;         /* Standard of rest */
   char *new_attrib;             /* Pointer value to new attribute name */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Attribute value */
   int len;                      /* Length of attrib string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* First look for axis attributes defined by the Frame class. Since a
   SpecFrame has only 1 axis, we allow these attributes to be specified
   without a trailing "(axis)" string. */
   if ( !strcmp( attrib, "direction" ) || 
        !strcmp( attrib, "bottom" ) ||
        !strcmp( attrib, "top" ) ||
        !strcmp( attrib, "format" ) ||
        !strcmp( attrib, "label" ) ||
        !strcmp( attrib, "symbol" ) ||
        !strcmp( attrib, "unit" ) ) {

/* Create a new attribute name from the original by appending the string
   "(1)" and then use the parent GetAttrib method. */
      new_attrib = astMalloc( len + 4 );
      if( new_attrib ) {
         memcpy( new_attrib, attrib, len );
         memcpy( new_attrib + len, "(1)", 4 ); 
         result = (*parent_getattrib)( this_object, new_attrib );
         new_attrib = astFree( new_attrib );
      }

/* AlignStdOfRest. */
/* --------------- */
/* Obtain the AlignStdOfRest code and convert to a string. */
   } else if ( !strcmp( attrib, "alignstdofrest" ) ) {
      sor = astGetAlignStdOfRest( this );
      if ( astOK ) {
         result = StdOfRestString( sor );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid AlignStdOfRest "
                     "identification code (%d).", astGetClass( this ), 
                     astGetClass( this ), (int) sor );
         }
      }

/* GeoLat. */
/* ------- */
   } else if ( !strcmp( attrib, "geolat" ) ) {
      dval = astGetGeoLat( this );
      if ( astOK ) {

/* Display absolute value preceeded by "N" or "S" as appropriate. */
         if( dval < 0 ) {         
            (void) sprintf( buff, "S%s",  astFormat( skyframe, 1, -dval ) );
         } else {
            (void) sprintf( buff, "N%s",  astFormat( skyframe, 1, dval ) );
         }
         result = buff;
      }

/* GeoLon. */
/* ------- */
   } else if ( !strcmp( attrib, "geolon" ) ) {
      dval = astGetGeoLon( this );
      if ( astOK ) {

/* Put into range +/- PI. */
         dval = slaDrange( dval );

/* Temporarily make the SkyFrame use degrees for longitude axis. */
         astSetAsTime( skyframe, 0, 0 );

/* Display absolute value preceeded by "E" or "W" as appropriate. */
         if( dval < 0 ) {         
            (void) sprintf( buff, "W%s",  astFormat( skyframe, 0, -dval ) );
         } else {
            (void) sprintf( buff, "E%s",  astFormat( skyframe, 0, dval ) );
         }
         result = buff;

/* Make the SkyFrame use hours for longitude axis again. */
         astSetAsTime( skyframe, 0, 1 );

      }

/* RefDec. */
/* ------- */
/* Convert to a string using the SkyFrame Format method. */
   } else if ( !strcmp( attrib, "refdec" ) ) {
      dval = astGetRefDec( this );
      if ( astOK ) {
         result = astFormat( skyframe, 1, dval );
      }

/* RefRA. */
/* ------ */
/* Convert to a string using the SkyFrame Format method. */
   } else if ( !strcmp( attrib, "refra" ) ) {
      dval = astGetRefRA( this );
      if ( astOK ) {
         result = astFormat( skyframe, 0, dval );
      }

/* RestFreq. */
/* --------- */
   } else if ( !strcmp( attrib, "restfreq" ) ) {
      dval = astGetRestFreq( this );
      if ( astOK ) {
         (void) sprintf( buff, "%.*g", DBL_DIG, dval*1.0E-9 );
         result = buff;
      }

/* SourceVel */
/* --------- */
   } else if ( !strcmp( attrib, "sourcevel" ) ) {
      dval = astGetSourceVel( this );
      if ( astOK ) {

/* Convert from "m/s" to "km/s" */
         (void) sprintf( buff, "%.*g", DBL_DIG, dval*1.0E-3 );
         result = buff;
      }

/* SourceVRF */
/* ----------*/
   } else if ( !strcmp( attrib, "sourcevrf" ) ) {
      sor = astGetSourceVRF( this );
      if ( astOK ) {
         result = StdOfRestString( sor );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid SourceVRF "
                     "identification code (%d).", astGetClass( this ), 
                     astGetClass( this ), (int) sor );
         }
      }

/* StdOfRest. */
/* ---------- */
/* Obtain the StdOfRest code and convert to a string. */
   } else if ( !strcmp( attrib, "stdofrest" ) ) {
      sor = astGetStdOfRest( this );
      if ( astOK ) {
         result = StdOfRestString( sor );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid StdOfRest "
                     "identification code (%d).", astGetClass( this ), 
                     astGetClass( this ), (int) sor );
         }
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

static const char *GetDomain( AstFrame *this_frame ) {
/*
*  Name:
*     GetDomain

*  Purpose:
*     Obtain a pointer to the Domain attribute string for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetDomain( AstFrame *this )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetDomain protected
*     method inherited from the Frame class).

*  Description:
*    This function returns a pointer to the Domain attribute string
*    for a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     A pointer to a constant null-terminated string containing the
*     Domain value.

*  Notes:
*     - The returned pointer or the string it refers to may become
*     invalid following further invocation of this function or
*     modification of the SpecFrame.
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* If a Domain attribute string has been set, invoke the parent method
   to obtain a pointer to it. */
   if ( astTestDomain( this ) ) {
      result = (*parent_getdomain)( this_frame );

/* Otherwise, provide a pointer to a suitable default string. */
   } else {
      result = "SPECTRUM";
   }

/* Return the result. */
   return result;
}

static const char *GetLabel( AstFrame *this, int axis ) {
/*
*  Name:
*     GetLabel

*  Purpose:
*     Access the Label string for a SpecFrame axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetLabel( AstFrame *this, int axis )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetLabel method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Label string for a specified axis
*     of a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     axis
*        Axis index (zero-based) identifying the axis for which information is
*        required.

*  Returned Value:
*     Pointer to a constant null-terminated character string containing the
*     requested information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstMapping *map;              /* Mapping between units */
   AstSystemType system;         /* Code identifying type of spectral coordinates */
   char *new_lab;                /* Modified label string */
   const char *result;           /* Pointer to label string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetLabel" );

/* Check if a value has been set for the required axis label string. If so,
   invoke the parent astGetLabel method to obtain a pointer to it. */
   if ( astTestLabel( this, axis ) ) {
      result = (*parent_getlabel)( this, axis );

/* Otherwise, identify the spectral coordinate system described by the 
   SpecFrame. */
   } else {
      system = astGetSystem( this );

/* If OK, supply a pointer to a suitable default label string. */
      if ( astOK ) {
         result = strcpy( buff, SystemLabel( system ) );
         buff[ 0 ] = toupper( buff[ 0 ] );

/* Modify this default to take account of the current value of the Unit 
   attribute, if set. */
         if( astTestUnit( this, axis ) ) {

/* Find a Mapping from the default Units for the current System, to the 
   units indicated by the Unit attribute. This Mapping is used to modify
   the existing default label appropriately. For instance, if the default
   units is "Hz" and the actual units is "log(Hz)", then the default label
   of "Frequency" is changed to "log( frequency )". */
            map = astUnitMapper( DefUnit( system, "astGetLabel", 
                                          astGetClass( this ) ),
                                 astGetUnit( this, axis ), result,
                                 &new_lab );
            if( new_lab ) {
               result = strcpy( buff, new_lab );
               new_lab = astFree( new_lab );
            }

/* Annul the unused Mapping. */
            if( map ) map = astAnnul( map );

         }
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN

}

static void GetRefPos( AstSpecFrame *this, AstSkyFrame *frm, double *lon, 
                       double *lat ){
/*
*++
*  Name:
c     astGetRefPos
f     AST_GETREFPOS

*  Purpose:
*     Return the reference position in a specified celestial coordinate system.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "specframe.h"
c     void astGetRefPos( AstSpecFrame *this, AstSkyFrame *frm, double *lon, 
c                        double *lat )
f     CALL AST_GETREFPOS( THIS, FRM, LON, LAT, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function 
f     This routine 
*     returns the reference position (specified by attributes RefRA and
*     RefDec) converted to the celestial coordinate system represented by
*     a supplied SkyFrame. The celestial longitude and latitude values
*     are returned in radians.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the SpecFrame.
c     frm
f     FRM = INTEGER (Given)
*        Pointer to the SkyFrame which defines the required celestial
*        coordinate system. 
c        If NULL 
f        If AST__NULL 
*        is supplied, then the longitude and latitude values are returned 
*        as FK5 J2000 RA and Dec values.
c     lon
f     LON = DOUBLE PRECISION (Returned)
c        A pointer to a double in which to store the
f        The 
*        longitude of the reference point, in the coordinate system
*        represented by the supplied SkyFrame (radians).
c     lat
f     LAT = DOUBLE PRECISION (Returned)
c        A pointer to a double in which to store the
f        The 
*        latitude of the reference point, in the coordinate system
*        represented by the supplied SkyFrame (radians).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - Values of AST__BAD will be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   AstFrameSet *fs;            /* Conversion FrameSet */
   AstFrame *fb;               /* Base Frame */
   AstFrame *fc;               /* Current Frame */
   double xin[ 1 ];            /* Axis 1 values */
   double yin[ 1 ];            /* Axis 2 values */
   double xout[ 1 ];           /* Axis 1 values */
   double yout[ 1 ];           /* Axis 2 values */

/* Initialise. */
   if( lon ) *lon = AST__BAD;
   if( lat ) *lat = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return;

/* If no SkyFrame was supplied, just return the stored RefRA and RefDec 
   values. */
   if( !frm ) {
      if( lon ) *lon = astGetRefRA( this );
      if( lat ) *lat = astGetRefDec( this );

/* Otherwise, convert the stored values to the requested system. */
   } else {

/* Find the Mapping from the SkyFrame which describes the internal format
   in which the RefRA and RefDec attribute values are stored, to the
   supplied Frame. */
      fs = astFindFrame( skyframe, frm, "" );

/* If alignment was possible, use the Mapping to transform the internal
   RefRA and RefDec values. Check for axis permutatuion. */
      if( fs ) {
         fb = astGetFrame( fs, AST__BASE );
         if( astGetLonAxis( fb ) == 0 ) {
            xin[ 0 ] = astGetRefRA( this );
            yin[ 0 ] = astGetRefDec( this );
         } else {
            yin[ 0 ] = astGetRefRA( this );
            xin[ 0 ] = astGetRefDec( this );
         }
         astTran2( fs, 1, xin, yin, 1, xout, yout );

/* Store the returned values, checking to see if the axes of the supplied
   SkyFrame have been permuted. */
         fc = astGetFrame( fs, AST__CURRENT );
         if( astGetLonAxis( fc ) == 0 ) {
            if( lon ) *lon = xout[ 0 ];
            if( lat ) *lat = yout[ 0 ];
         } else {
            if( lon ) *lon = yout[ 0 ];
            if( lat ) *lat = xout[ 0 ];
         }

/* Annul object references. */
         fc = astAnnul( fc );
         fb = astAnnul( fb );
         fs = astAnnul( fs );
      }
   }  
}

static const char *GetSymbol( AstFrame *this, int axis ) {
/*
*  Name:
*     GetSymbol

*  Purpose:
*     Obtain a pointer to the Symbol string for a SpecFrame axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetSymbol( AstFrame *this, int axis )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetSymbol method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Symbol string for a specified axis
*     of a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     axis
*        Axis index (zero-based) identifying the axis for which information is
*        required.

*  Returned Value:
*     Pointer to a constant null-terminated character string containing the
*     requested information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstMapping *map;              /* Mapping between units */
   AstSystemType system;         /* Code identifying type of sky coordinates */
   char *new_sym;                /* Modified symbol string */
   const char *result;           /* Pointer to symbol string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetSymbol" );

/* Check if a value has been set for the required axis symbol string. If so,
   invoke the parent astGetSymbol method to obtain a pointer to it. */
   if ( astTestSymbol( this, axis ) ) {
      result = (*parent_getsymbol)( this, axis );

/* Otherwise, identify the sky coordinate system described by the SpecFrame. */
   } else {
      system = astGetSystem( this );

/* If OK, supply a pointer to a suitable default Symbol string. */
      if ( astOK ) {

         if( system == AST__FREQ ) {
	    result = "FREQ";
         } else if( system == AST__ENERGY ) {
	    result = "ENER";
         } else if( system == AST__WAVENUM ) {
	    result = "WAVN";
         } else if( system == AST__WAVELEN ) {
	    result = "WAVE";
         } else if( system == AST__AIRWAVE ) {
	    result = "AWAV";
         } else if( system == AST__VRADIO ) {
	    result = "VRAD";
         } else if( system == AST__VOPTICAL ) {
	    result = "VOPT";
         } else if( system == AST__REDSHIFT ) {
	    result = "ZOPT";
         } else if( system == AST__BETA ) {
	    result = "BETA";
         } else if( system == AST__VREL ) {
	    result = "VELO";

/* Report an error if the coordinate system was not recognised. */
         } else {
	    astError( AST__SCSIN, "astGetSymbol(%s): Corrupt %s contains "
		      "invalid System identification code (%d).", 
                      astGetClass( this ), astGetClass( this ), (int) system );
         }

/* Modify this default to take account of the current value of the Unit 
   attribute, if set. */
         if( astTestUnit( this, axis ) ) {

/* Find a Mapping from the default Units for the current System, to the 
   units indicated by the Unit attribute. This Mapping is used to modify
   the existing default symbol appropriately. For instance, if the default
   units is "Hz" and the actual units is "log(Hz)", then the default symbol
   of "nu" is changed to "log( nu )". */
            map = astUnitMapper( DefUnit( system, "astGetSymbol", 
                                          astGetClass( this ) ),
                                 astGetUnit( this, axis ), result,
                                 &new_sym );
            if( new_sym ) {
               result = strcpy( buff, new_sym );
               new_sym = astFree( new_sym );
            }

/* Annul the unused Mapping. */
            if( map ) map = astAnnul( map );

         }
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static AstSystemType GetAlignSystem( AstFrame *this_frame ) {
/*
*  Name:
*     GetAlignSystem

*  Purpose:
*     Obtain the AlignSystem attribute for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "Specframe.h"
*     AstSystemType GetAlignSystem( AstFrame *this_frame )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetAlignSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function returns the AlignSystem attribute for a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     The AlignSystem value.

*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   AstSystemType result;         /* Value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* If a AlignSystem attribute has been set, invoke the parent method to obtain 
   it. */
   if ( astTestAlignSystem( this ) ) {
      result = (*parent_getalignsystem)( this_frame );

/* Otherwise, provide a suitable default. */
   } else {
      result = AST__WAVELEN;
   }

/* Return the result. */
   return result;
}

static AstSystemType GetSystem( AstFrame *this_frame ) {
/*
*  Name:
*     GetSystem

*  Purpose:
*     Obtain the System attribute for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     AstSystemType GetSystem( AstFrame *this_frame )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function returns the System attribute for a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     The System value.

*  Notes:
*     - AST__BADSYSTEM is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   AstSystemType result;         /* Value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* If a System attribute has been set, invoke the parent method to obtain 
   it. */
   if ( astTestSystem( this ) ) {
      result = (*parent_getsystem)( this_frame );

/* Otherwise, provide a suitable default. */
   } else {
      result = AST__WAVELEN;
   }

/* Return the result. */
   return result;
}

static const char *GetTitle( AstFrame *this_frame ) {
/*
*  Name:
*     GetTitle

*  Purpose:
*     Obtain a pointer to the Title string for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetTitle( AstFrame *this_frame )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetTitle method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Title string for a SpecFrame.
*     A pointer to a suitable default string is returned if no Title value has
*     previously been set.

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     Pointer to a null-terminated character string containing the requested
*     information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   AstStdOfRestType sor;         /* Code identifying standard of rest */
   AstSystemType system;         /* Code identifying type of coordinates */
   const char *sor_string;       /* Pointer to SOR description */
   const char *result;           /* Pointer to result string */
   double rf;                    /* Rest frequency */
   int nc;                       /* No. of characters added */
   int pos;                      /* Buffer position to enter text */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* See if a Title string has been set. If so, use the parent astGetTitle
   method to obtain a pointer to it. */
   if ( astTestTitle( this ) ) {
      result = (*parent_gettitle)( this_frame );

/* Otherwise, we will generate a default Title string. Obtain the values of the
   SpecFrame's attributes that determine what this string will be. */
   } else {
      system = astGetSystem( this );
      sor = astGetStdOfRest( this );
      sor_string = StdOfRestString( sor );
      rf = astGetRestFreq( this );

/* Classify the coordinate system type and create an appropriate Title
   string.  (Note that when invoking the astFmtDecimalYr function we must
   use a separate sprintf on each occasion so as not to over-write its
   internal buffer before the result string has been used.) */
      if ( astOK ) {
         result = buff;

/* Begin with the system's default label. */
         pos = sprintf( buff, "%s", SystemLabel( system ) );
         buff[ 0 ] = toupper( buff[ 0 ] );

/* Append the standard of rest in parentheses, if set. */
         if( astTestStdOfRest( this ) ) {
            nc = sprintf( buff+pos, " (%s)", sor_string );
            pos += nc;
         }

/* Append the rest frequency if relevant. */
         if( !ABS_SYSTEM(system) && ( astTestRestFreq( this ) ||
                                      astGetUseDefs( this ) ) ) {
            pos += sprintf( buff+pos, ", rest frequency = %g GHz", rf*1.0E-9 );
         }
      }
   }

/* If an error occurred, clear the returned pointer value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static const char *GetUnit( AstFrame *this_frame, int axis ) {
/*
*  Name:
*     astGetUnit

*  Purpose:
*     Obtain a pointer to the Unit string for a SpecFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *GetUnit( AstFrame *this_frame, int axis )

*  Class Membership:
*     SpecFrame member function (over-rides the astGetUnit method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Unit string for a specified axis
*     of a SpecFrame. If the Unit attribute has not been set for the axis, a
*     pointer to a suitable default string is returned instead.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     axis
*        The number of the axis (zero-based) for which information is required.

*  Returned Value:
*     A pointer to a null-terminated string containing the Unit value.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   AstSystemType system;         /* The SpecFrame's System value */
   const char *result;           /* Pointer value to return */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetUnit" );

/* If a value has been set for the Unit attribute, use the parent 
   GetUnit method to return a pointer to the required Unit string. */
   if( astTestUnit( this, axis ) ){
      result = (*parent_getunit)( this_frame, axis );

/* Otherwise, identify the spectral coordinate system described by the 
   SpecFrame. */
   } else {
      system = astGetSystem( this );

/* Return a string describing the default units. */
      result = DefUnit( system, "astGetUnit", astGetClass( this ) );
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

void astInitSpecFrameVtab_(  AstSpecFrameVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitSpecFrameVtab

*  Purpose:
*     Initialise a virtual function table for a SpecFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specframe.h"
*     void astInitSpecFrameVtab( AstSpecFrameVtab *vtab, const char *name )

*  Class Membership:
*     SpecFrame vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SpecFrame class.

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
   AstFrameVtab *frame;          /* Pointer to Frame component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

#ifdef DEBUG
   int pm;     /* See astSetPermMem in memory.c */
#endif

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitFrameVtab( (AstFrameVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsASpecFrame) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->GetRefPos = GetRefPos;
   vtab->SetRefPos = SetRefPos;

   vtab->ClearAlignStdOfRest = ClearAlignStdOfRest;
   vtab->TestAlignStdOfRest = TestAlignStdOfRest;
   vtab->GetAlignStdOfRest = GetAlignStdOfRest;
   vtab->SetAlignStdOfRest = SetAlignStdOfRest;

   vtab->ClearSourceVRF = ClearSourceVRF;
   vtab->TestSourceVRF = TestSourceVRF;
   vtab->GetSourceVRF = GetSourceVRF;
   vtab->SetSourceVRF = SetSourceVRF;

   vtab->ClearGeoLat = ClearGeoLat;
   vtab->TestGeoLat = TestGeoLat;
   vtab->GetGeoLat = GetGeoLat;
   vtab->SetGeoLat = SetGeoLat;

   vtab->ClearGeoLon = ClearGeoLon;
   vtab->TestGeoLon = TestGeoLon;
   vtab->GetGeoLon = GetGeoLon;
   vtab->SetGeoLon = SetGeoLon;

   vtab->ClearRefDec = ClearRefDec;
   vtab->TestRefDec = TestRefDec;
   vtab->GetRefDec = GetRefDec;
   vtab->SetRefDec = SetRefDec;

   vtab->ClearRefRA = ClearRefRA;
   vtab->TestRefRA = TestRefRA;
   vtab->GetRefRA = GetRefRA;
   vtab->SetRefRA = SetRefRA;

   vtab->ClearRestFreq = ClearRestFreq;
   vtab->TestRestFreq = TestRestFreq;
   vtab->GetRestFreq = GetRestFreq;
   vtab->SetRestFreq = SetRestFreq;

   vtab->ClearStdOfRest = ClearStdOfRest;
   vtab->TestStdOfRest = TestStdOfRest;
   vtab->GetStdOfRest = GetStdOfRest;
   vtab->SetStdOfRest = SetStdOfRest;

   vtab->ClearSourceVel = ClearSourceVel;
   vtab->TestSourceVel = TestSourceVel;
   vtab->GetSourceVel = GetSourceVel;
   vtab->SetSourceVel = SetSourceVel;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   frame = (AstFrameVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_getdomain = frame->GetDomain;
   frame->GetDomain = GetDomain;

   parent_getsystem = frame->GetSystem;
   frame->GetSystem = GetSystem;
   parent_setsystem = frame->SetSystem;
   frame->SetSystem = SetSystem;
   parent_clearsystem = frame->ClearSystem;
   frame->ClearSystem = ClearSystem;

   parent_getalignsystem = frame->GetAlignSystem;
   frame->GetAlignSystem = GetAlignSystem;

   parent_getlabel = frame->GetLabel;
   frame->GetLabel = GetLabel;

   parent_getsymbol = frame->GetSymbol;
   frame->GetSymbol = GetSymbol;

   parent_gettitle = frame->GetTitle;
   frame->GetTitle = GetTitle;

   parent_clearunit = frame->ClearUnit;
   frame->ClearUnit = ClearUnit;

   parent_getunit = frame->GetUnit;
   frame->GetUnit = GetUnit;

   parent_setunit = frame->SetUnit;
   frame->SetUnit = SetUnit;

   parent_match = frame->Match;
   frame->Match = Match;

   parent_overlay = frame->Overlay;
   frame->Overlay = Overlay;

   parent_setmaxaxes = frame->SetMaxAxes;
   frame->SetMaxAxes = SetMaxAxes;

   parent_setminaxes = frame->SetMinAxes;
   frame->SetMinAxes = SetMinAxes;

   parent_subframe = frame->SubFrame;
   frame->SubFrame = SubFrame;

/* Store replacement pointers for methods which will be over-ridden by new
   member functions implemented here. */
   frame->GetActiveUnit = GetActiveUnit;
   frame->TestActiveUnit = TestActiveUnit;
   frame->ValidateSystem = ValidateSystem;
   frame->SystemString = SystemString;
   frame->SystemCode = SystemCode;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "SpecFrame",
               "Description of spectral coordinate system" );

/* Create an FK5 J2000 SkyFrame which will be used for formatting and 
   unformatting sky positions, etc. */
#ifdef DEBUG
   pm = astSetPermMem( 1 );
#endif

   skyframe = astSkyFrame( "system=FK5,equinox=J2000" );

#ifdef DEBUG
   astSetPermMem( pm );
#endif


}

static int MakeSpecMapping( AstSpecFrame *target, AstSpecFrame *result,
                            AstSpecFrame *align_frm, int report, 
                            AstMapping **map ) {
/*
*  Name:
*     MakeSpecMapping

*  Purpose:
*     Generate a Mapping between two SpecFrames.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int MakeSpecMapping( AstSpecFrame *target, AstSpecFrame *result,
*                          AstSpecFrame *align_frm, int report, 
*                          AstMapping **map ) {

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function takes two SpecFrames and generates a Mapping that
*     converts between them, taking account of differences in their
*     coordinate systems, rest frequency, standard of rest, etc. 
*
*     In order to cut down the number of transformations to be considered,
*     the scheme works by first converting from the target frame to an
*     "alignment" Frame, using the attributes of the target to define the
*     transformation. A transformation is then found from the alignment
*     frame to the required result Frame,  using the attributes of the
*     result to define the transformation. The alignment Frame is
*     described by the attributes of the "align_frm" SpecFrame.
*
*     Thus, different forms of alignment can be obtained by suitable
*     choice of the attributes of "align_frm". For instance, to compare the
*     radio velocity dispersion of two lines at different rest frequencies,
*     you would set "system=radio velocity" and (probably) "stdofrest=local 
*     group" in "align_frm". On the other hand if you wanted to re-calibrate 
*     an existing radio velocity Frame within a FrameSet to use a different 
*     rest frequency, you would make the SpecFrame the current Frame and then 
*     set the rest frequency attribute for the FrameSet. The "integrity 
*     checking" system in the FrameSet class would then get the Mapping 
*     between the original and the modified SpecFrames. In this case, the 
*     "alignment system" needs to be "frequency" since you want the original 
*     and modified SpecFrames to be aligned in frequency, not radio velocity.

*  Parameters:
*     target
*        Pointer to the first SpecFrame.
*     result
*        Pointer to the second SpecFrame.
*     align_frm
*        A SpecFrame defining the system and standard of rest in which to 
*        align the target and result SpecFrames.
*     report
*        Should errors be reported if no match is possible? These reports
*        will describe why no match was possible.
*     map
*        Pointer to a location which is to receive a pointer to the
*        returned Mapping. The forward transformation of this Mapping
*        will convert from "target" coordinates to "result"
*        coordinates, and the inverse transformation will convert in
*        the opposite direction (all coordinate values in radians).

*  Returned Value:
*     Non-zero if the Mapping could be generated, or zero if the two
*     SpecFrames are sufficiently un-related that no meaningful Mapping
*     can be produced (albeit an "unmeaningful" Mapping will be returned
*     in this case, which will need to be annulled).

*  Notes:
*     A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Constants: */
#define MAX_ARGS 1               /* Max arguments for an SpecMap conversion */

/* Local Variables: */
   AstMapping *map1;             /* Intermediate Mapping */
   AstMapping *map2;             /* Intermediate Mapping */
   AstMapping *umap1;            /* First Units Mapping */
   AstMapping *umap2;            /* Second Units Mapping */
   AstSpecMap *specmap;          /* Pointer to SpecMap */
   AstSystemType serr;           /* Erroneous system */
   AstSystemType align_system;   /* Code to identify alignment system */
   AstSystemType target_system;  /* Code to identify target system */
   AstSystemType result_system;  /* Code to identify result system */
   const char *uerr;             /* Erroneous units */
   const char *ures;             /* Results units */
   const char *utarg;            /* Target units */
   const char *vmess;            /* Text for use in error messages */
   double args[ MAX_ARGS ];      /* Conversion argument array */
   double target_rf;             /* Target rest frequency (Hz) */
   double result_rf;             /* Result rest frequency (Hz) */
   int match;                    /* Mapping can be generated? */
   int step1;                    /* Perform the 1st step in the Mapping? */
   int step2;                    /* Perform the 2nd step in the Mapping? */
   int step3;                    /* Perform the 3rd step in the Mapping? */
   int step4;                    /* Perform the 4th step in the Mapping? */
   int step5;                    /* Perform the 5th step in the Mapping? */
   int step6;                    /* Perform the 6th step in the Mapping? */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the returned values. */
   match = 1;
   *map = NULL;

/* Create an initial (null) SpecMap. This is a 1D Mapping which converts
   spectral axis values between different systems and standard of rest.
   The axis units used by the SpecMap class match the default units used
   by this class. Any discrepancy between units is taken into account at
   the end of this function, once the total SpecMap has been created. */
   specmap = astSpecMap( 1, 0, "" );

/* Define local macros as shorthand for adding spectral coordinate
   conversions to this SpecMap.  Each macro simply stores details of
   the additional arguments in the "args" array and then calls
   astSpecAdd. The macros differ in the number of additional argument
   values. */
#define TRANSFORM_0(cvt) \
        astSpecAdd( specmap, cvt, NULL );

#define TRANSFORM_1(cvt,arg0) \
        args[ 0 ] = arg0; \
        astSpecAdd( specmap, cvt, args );

/* Get all the necessary attributes from the result, target and alignment
   Frames. */
   target_rf = astGetRestFreq( target );
   result_rf = astGetRestFreq( result );

   target_system = astGetSystem( target );
   result_system = astGetSystem( result );
   align_system = astGetSystem( align_frm );

/* Define text for error messages.*/
   vmess = "convert between spectral systems";

/* Verify that values for the standard of rest have been set if required
   (i.e if the UseDefs attribute of either SpecFrame is false). */
   VerifyAttrs( result, vmess, "StdOfRest", "astMatch" );
   VerifyAttrs( target, vmess, "StdOfRest", "astMatch" );

/* The supported spectral coordinate systems fall into two groups;
   "relative", and "absolute". The relative systems define each axis
   value with respect to the rest frequency, whereas the absolute systems
   have axis values which do not depend on the rest frequency. In order
   to convert an axis value from a system in one group to a system in the 
   other group, the rest frequency must be known. However, the rest
   frequency is not necessary in order to convert axis values between two 
   systems belonging to the same group.  Determine if the alignment system 
   is absolute or relative. If absolute, we ignore the system of the supplied 
   "align_frm" and align in frequency, since aligning in any absolute system 
   will automatically ensure that all the other absolute systems are aligned. 
   Similarly, aligning in any relative system will automatically ensure that 
   all the other relative systems are aligned. Doing this cuts down the 
   complexity of the conversion process since we do not need to check every 
   possible alignment system. */
   align_system = ( ABS_SYSTEM( align_system ) ) ? AST__FREQ : AST__VREL;

/* The total Mapping is made up of the following steps in series:

  0) Convert target units to default units for the targets system
  1) Convert from target system in target SOR to frequency in target SOR
  2) Convert from freq in target SOR to freq in alignment SOR
  3) Convert from freq in alignment SOR to alignment system in alignment SOR
  4) Convert from alignment system in alignment SOR to freq in alignment SOR
  5) Convert from freq in alignment SOR to freq in result SOR 
  6) Convert from freq in result SOR to result system in result SOR
  7) Convert default units for the result system to results unit

   Steps 1,2,3 are performed using the attributes of the target (rest
   frequency, reference position, etc), whilst steps 4,5,6 are performed 
   using the attributes of the target (rest frequency, reference position, 
   etc). It is necessary to go from target system to alignment system 
   via frequency because SOR conversion can only be performed in the
   frequency domain.
 
   Some of these steps may not be necessary. Initially assume all steps
   are necessary (we leave steps 0 and 7 out of this process and
   implement them once all other steps have been done). */
   step1 = 1;
   step2 = 1;
   step3 = 1;
   step4 = 1;
   step5 = 1;
   step6 = 1;

/* Step 1 is not necessary if the target system is frequency. */
   if( target_system == AST__FREQ ) step1 = 0;

/* Step 2 is not necessary if the alignment SOR is the same as the target 
   SOR. */
   if( EqualSor( target, align_frm ) ) step2 = 0;

/* Step 5 is not necessary if the alignment SOR is the same as the result
   SOR. */
   if( EqualSor( result, align_frm ) ) step5 = 0;

/* Step 6 is not necessary if the result system is frequency. */
   if( result_system == AST__FREQ ) step6 = 0;

/* Steps 3 and 4 are not necessary if the alignment system is frequency,
   or if the target and result rest frequencies are equal. */
   if( align_system == AST__FREQ || result_rf == target_rf ) step3 = step4 = 0;

/* Steps 2 and 5 are not necessary if steps 3 and 4 are not necessary, and
   the target sor equals the result sor. */
   if( !step3 && !step4 && EqualSor( target, result ) ) step2 = step5 = 0;

/* Steps 1 and 6 are not necessary if steps 2, 3, 4, 5 are not necessary, and
   the target sor equals the result sor, and the target and results systems 
   are equal (if the systems are relative they must also have equal rest 
   frequencies). */
   if( !step2 && !step3 && !step4 && !step5 && EqualSor( target, result ) &&
       target_system == result_system ) {
      if( !ABS_SYSTEM( target_system ) || result_rf == target_rf ) step1 = step6 = 0;
   }


/* Now we know which steps are needed, let's do them (we delay unit
   conversion to the end)... */

/* Step 1: target system in target rest frame to frequency in target rest 
   frame. */
   if( step1 ) {
      if( target_system != AST__FREQ ) {

/* If the target system is absolute, we can convert directly to frequency. */
         if ( target_system == AST__ENERGY ) {
            TRANSFORM_0( "ENTOFR" )

         } else if ( target_system == AST__WAVENUM ) {
            TRANSFORM_0( "WNTOFR" )

         } else if ( target_system == AST__WAVELEN ) {
            TRANSFORM_0( "WVTOFR" )

         } else if ( target_system == AST__AIRWAVE ) {
            TRANSFORM_0( "AWTOFR" )

/* If the target target_system is relative, we first need to convert to 
   apparent radial velocity, and then to frequency using the rest frequency. */
         } else {

            if ( target_system == AST__VRADIO ) {
               TRANSFORM_0( "VRTOVL" )
   
            } else if ( target_system == AST__VOPTICAL ) {
               TRANSFORM_0( "VOTOVL" )
   
            } else if ( target_system == AST__REDSHIFT ) {
               TRANSFORM_0( "ZOTOVL" )
   
            } else if ( target_system == AST__BETA ) {
               TRANSFORM_0( "BTTOVL" )
            }

            VerifyAttrs( target, vmess, "RestFreq", "astMatch" );
            TRANSFORM_1( "VLTOFR", target_rf )
         }
      }
   }

/* Step 2: frequency in target rest frame to frequency in alignment rest
   frame. */
   if( step2 ) match = SorConvert( target, align_frm, specmap );

/* Step 3: frequency in alignment rest frame to alignment system in alignment 
   rest frame. The alignment will be either relativistic velocity or
   frequency. */
   if( step3 ) {
      if( align_system == AST__VREL ) {
         VerifyAttrs( target, vmess, "RestFreq", "astMatch" );
         TRANSFORM_1( "FRTOVL", target_rf )
      }
   }

/* Step 4: Alignment system in alignment rest frame to frequency in alignment 
   rest frame (from now on use the attributes of the result SpecFrame to
   define the conversion parameters). */
   if( step4 ) {
      if( align_system == AST__VREL ) {
         VerifyAttrs( result, vmess, "RestFreq", "astMatch" );
         TRANSFORM_1( "VLTOFR", result_rf )
      }
   }

/* Step 5: frequency in alignment rest frame to frequency in result rest 
   frame. */
   if( step5 ) match = SorConvert( align_frm, result, specmap );

/* Step 6: frequency in result rest frame to result system in result rest
   frame. */
   if( step6 ) {
      if( result_system != AST__FREQ ) {

/* If the results system is absolute, we can convert directly. */
         if ( result_system == AST__ENERGY ) {
            TRANSFORM_0( "FRTOEN" )

         } else if ( result_system == AST__WAVENUM ) {
            TRANSFORM_0( "FRTOWN" )

         } else if ( result_system == AST__WAVELEN ) {
            TRANSFORM_0( "FRTOWV" )

         } else if ( result_system == AST__AIRWAVE ) {
            TRANSFORM_0( "FRTOAW" )

/* If the result system is relative, we first need to convert to apparent 
   radial velocity from frequency using the rest frequency. Report an error 
   if the rest frequency is undefined. */
         } else {
            VerifyAttrs( result, vmess, "RestFreq", "astMatch" );
            TRANSFORM_1( "FRTOVL", result_rf )

/* Now convert from apparent radial velocity to the required result system. */
            if ( result_system == AST__VRADIO ) {
               TRANSFORM_0( "VLTOVR" )
   
            } else if ( result_system == AST__VOPTICAL ) {
               TRANSFORM_0( "VLTOVO" )
   
            } else if ( result_system == AST__REDSHIFT ) {
               TRANSFORM_0( "VLTOZO" )
   
            } else if ( result_system == AST__BETA ) {
               TRANSFORM_0( "VLTOBT" )
            }
         }
      }
   }

/* The SpecMap created above class assumes that the axis values supplied to 
   its Transform method are in units which correspond to the default units 
   for its class (the returned values also use these units). However,
   the Unit attributes of the supplied Frames may have been set to some
   non-default value, and so we need to add Mappings before and after the
   SpecMap which convert to and from the default units. Find the Mapping
   from the target Frame Units to the default Units for the target's system. */
   utarg = astGetUnit( target, 0 );
   umap1 = astUnitMapper( utarg, SpecMapUnit( target_system, "MakeSpecMap", 
                                              "SpecFrame" ), NULL, NULL );

/* Find the Mapping from the default Units for the result's system to the
   Units of the result Frame. */
   ures = astGetUnit( result, 0 );
   umap2 = astUnitMapper( SpecMapUnit( result_system, "MakeSpecMap", 
                                       "SpecFrame" ), ures, NULL, NULL );

/* If both units Mappings were created OK, sandwich the SpecMap between
   them. */
   if( umap1 && umap2 ) {
      map1 = (AstMapping *) astCmpMap( umap1, specmap, 1, "" );
      map2 = (AstMapping *) astCmpMap( map1, umap2, 1, "" );
      map1 = astAnnul( map1 );

/* If the simplified SpecMap is a UnitMap, and the target and result
   units are the same, we do not need to know the mapping between units.
   Otherwise, report an error and indicate that we cannot convert between 
   the Frames. */
   } else {
      map2 = astSimplify( specmap );
      if( !astIsAUnitMap( map2 ) || strcmp( ures, utarg ) ) {
         match = 0;
         if( astOK && report ) {         
            if( !umap1 ) {
               uerr = utarg;
               serr = astGetSystem( target );
            } else {
               uerr = ures;
               serr = astGetSystem( result );
            }

            astError( AST__BADUN, "astMatch(SpecFrame): Inappropriate units (%s) "
                      "specified for a %s axis.", uerr, SystemLabel( serr ) );
         }
      }
   }

/* Return the simplified Mapping. */
   *map = astSimplify( map2 );

/* Annul remaining resources. */
   map2 = astAnnul( map2 );
   specmap = astAnnul( specmap );
   if( umap1 ) umap1 = astAnnul( umap1 );
   if( umap2 ) umap2 = astAnnul( umap2 );

/* If an error occurred, annul the returned Mapping and clear the returned 
   values. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      match = 0;
   }

/* Return the result. */
   return match;

/* Undefine macros local to this function. */
#undef MAX_ARGS
#undef TRANSFORM_0
#undef TRANSFORM_1
}

static int Match( AstFrame *template_frame, AstFrame *target,
                  int **template_axes, int **target_axes, AstMapping **map,
                  AstFrame **result ) {
/*
*  Name:
*     Match

*  Purpose:
*     Determine if conversion is possible between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int Match( AstFrame *template, AstFrame *target,
*                int **template_axes, int **target_axes,
*                AstMapping **map, AstFrame **result )

*  Class Membership:
*     SpecFrame member function (over-rides the protected astMatch method
*     inherited from the Frame class).

*  Description:
*     This function matches a "template" SpecFrame to a "target" Frame and
*     determines whether it is possible to convert coordinates between them.
*     If it is, a mapping that performs the transformation is returned along
*     with a new Frame that describes the coordinate system that results when
*     this mapping is applied to the "target" coordinate system. In addition,
*     information is returned to allow the axes in this "result" Frame to be
*     associated with the corresponding axes in the "target" and "template"
*     Frames from which they are derived.

*  Parameters:
*     template
*        Pointer to the template SpecFrame. This describes the coordinate 
*        system (or set of possible coordinate systems) into which we wish to 
*        convert our coordinates.
*     target
*        Pointer to the target Frame. This describes the coordinate system in
*        which we already have coordinates.
*     template_axes
*        Address of a location where a pointer to int will be returned if the
*        requested coordinate conversion is possible. This pointer will point
*        at a dynamically allocated array of integers with one element for each
*        axis of the "result" Frame (see below). It must be freed by the caller
*        (using astFree) when no longer required.
*
*        For each axis in the result Frame, the corresponding element of this
*        array will return the index of the template SpecFrame axis from 
*        which it is derived. If it is not derived from any template 
*        SpecFrame axis, a value of -1 will be returned instead.
*     target_axes
*        Address of a location where a pointer to int will be returned if the
*        requested coordinate conversion is possible. This pointer will point
*        at a dynamically allocated array of integers with one element for each
*        axis of the "result" Frame (see below). It must be freed by the caller
*        (using astFree) when no longer required.
*
*        For each axis in the result Frame, the corresponding element of this
*        array will return the index of the target Frame axis from which it
*        is derived. If it is not derived from any target Frame axis, a value
*        of -1 will be returned instead.
*     map
*        Address of a location where a pointer to a new Mapping will be
*        returned if the requested coordinate conversion is possible. If
*        returned, the forward transformation of this Mapping may be used to
*        convert coordinates between the "target" Frame and the "result"
*        Frame (see below) and the inverse transformation will convert in the
*        opposite direction.
*     result
*        Address of a location where a pointer to a new Frame will be returned
*        if the requested coordinate conversion is possible. If returned, this
*        Frame describes the coordinate system that results from applying the
*        returned Mapping (above) to the "target" coordinate system. In
*        general, this Frame will combine attributes from (and will therefore
*        be more specific than) both the target and the template Frames. In
*        particular, when the template allows the possibility of transformaing
*        to any one of a set of alternative coordinate systems, the "result"
*        Frame will indicate which of the alternatives was used.

*  Returned Value:
*     A non-zero value is returned if the requested coordinate conversion is
*     possible. Otherwise zero is returned (this will not in itself result in
*     an error condition).

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.

*  Implementation Notes:
*     This implementation addresses the matching of a SpecFrame class 
*     object to any other class of Frame. A SpecFrame will match any class 
*     of SpecFrame (i.e. possibly from a derived class) but will not match 
*     a less specialised class of Frame.
*/

/* Local Variables: */
   AstFrame *frame0;             /* Pointer to Frame underlying axis 0 */
   AstSpecFrame *template;       /* Pointer to template SpecFrame structure */
   int iaxis0;                   /* Axis index underlying axis 0 */
   int match;                    /* Coordinate conversion possible? */

/* Initialise the returned values. */
   *template_axes = NULL;
   *target_axes = NULL;
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the template SpecFrame structure. */
   template = (AstSpecFrame *) template_frame;

/* Obtain pointers to the primary Frame which underlies the target axis. */
   astPrimaryFrame( target, 0, &frame0, &iaxis0 );

/* The first criterion for a match is that this Frame must be a SpecFrame 
   (or from a class derived from SpecFrame). */
   match = astIsASpecFrame( frame0 );

/* Annul the Frame pointers used in the above tests. */
   frame0 = astAnnul( frame0 );

/* The next criterion for a match is that the template matches as a
   Frame class object. This ensures that the number of axes (1) and
   domain, etc. of the target Frame are suitable. Invoke the parent
   "astMatch" method to verify this. */
   if( match ) {
      match = (*parent_match)( template_frame, target,
                               template_axes, target_axes, map, result );
   }

/* If a match was found, annul the returned objects, which are not
   needed, but keep the memory allocated for the axis association
   arrays, which we will re-use. */
   if ( astOK && match ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
   }

/* If the Frames still match, we next set up the axis association
   arrays. */
   if ( astOK && match ) {
      (*template_axes)[ 0 ] = 0;
      (*target_axes)[ 0 ] = 0;

/* Use the target's "astSubFrame" method to create a new Frame (the
   result Frame) with a copy of of the target axis. This process also 
   overlays the template attributes on to the target Frame and returns a 
   Mapping between the target and result Frames which effects the required 
   coordinate conversion. */
      match = astSubFrame( target, template, 1, *target_axes, *template_axes,
                           map, result );

/* If an error occurred, or conversion to the result Frame's coordinate 
   system was not possible, then free all memory, annul the returned 
   objects, and reset the returned value. */
      if ( !astOK || !match ) {
         *template_axes = astFree( *template_axes );
         *target_axes = astFree( *target_axes );
         if( *map ) *map = astAnnul( *map );
         if( *result ) *result = astAnnul( *result );
         match = 0;
      }
   }

/* Return the result. */
   return match;
}

static void Overlay( AstFrame *template, const int *template_axes,
                     AstFrame *result ) {
/*
*  Name:
*     Overlay

*  Purpose:
*     Overlay the attributes of a template SpecFrame on to another Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void Overlay( AstFrame *template, const int *template_axes,
*                   AstFrame *result )

*  Class Membership:
*     SpecFrame member function (over-rides the protected astOverlay method
*     inherited from the Frame class).

*  Description:
*     This function overlays attributes of a SpecFrame (the "template") on to
*     another Frame, so as to over-ride selected attributes of that second
*     Frame. Normally only those attributes which have been specifically set
*     in the template will be transferred. This implements a form of
*     defaulting, in which a Frame acquires attributes from the template, but
*     retains its original attributes (as the default) if new values have not
*     previously been explicitly set in the template.
*
*     Note that if the result Frame is a SpecFrame and a change of spectral
*     coordinate system occurs as a result of overlaying its System
*     attribute, then some of its original attribute values may no
*     longer be appropriate (e.g. the Title, or attributes describing
*     its axes). In this case, these will be cleared before overlaying
*     any new values.

*  Parameters:
*     template
*        Pointer to the template SpecFrame, for which values should have been
*        explicitly set for any attribute which is to be transferred.
*     template_axes
*        Pointer to an array of int, with one element for each axis of the
*        "result" Frame (see below). For each axis in the result frame, the
*        corresponding element of this array should contain the (zero-based)
*        index of the template axis to which it corresponds. This array is used
*        to establish from which template axis any axis-dependent attributes
*        should be obtained.
*
*        If any axis in the result Frame is not associated with a template
*        axis, the corresponding element of this array should be set to -1.
*     result
*        Pointer to the Frame which is to receive the new attribute values.

*  Returned Value:
*     void

*  Notes:
*     -  In general, if the result Frame is not from the same class as the
*     template SpecFrame, or from a class derived from it, then attributes may
*     exist in the template SpecFrame which do not exist in the result Frame. 
*     In this case, these attributes will not be transferred.
*/


/* Local Variables: */
   const char *new_class;        /* Pointer to template class string */
   const char *old_class;        /* Pointer to result class string */
   const char *method;           /* Pointer to method string */
   AstSystemType new_system;     /* Code identifying new cordinates */
   AstSystemType old_system;     /* Code identifying old coordinates */
   int resetSystem;              /* Was the template System value cleared? */
   int specframe;                /* Result Frame is a SpecFrame? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise strings used in error messages. */
   new_class = astGetClass( template );   
   old_class = astGetClass( result );   
   method = "astOverlay";

/* Get the old and new systems. */
   old_system = astGetSystem( result );
   new_system = astGetSystem( template );

/* If the result Frame is a SpecFrame, we must test to see if overlaying its
   System attribute will change the type of coordinate system it describes. 
   Determine the value of this attribute for the result and template 
   SpecFrames. */
   resetSystem = 0;
   specframe = astIsASpecFrame( result );
   if( specframe ) {

/* If the coordinate system will change, any value already set for the result
   SpecFrame's Title will no longer be appropriate, so clear it. */
      if ( new_system != old_system ) {
         astClearTitle( result );

/* If the systems have the same default units, we can retain the current 
   Unit value. */
         if( strcmp( DefUnit( new_system, method, new_class ),
                     DefUnit( old_system, method, old_class ) ) ) { 
            astClearUnit( result, 0 );
         }

/* If necessary, clear inappropriate values for all those axis attributes
   whose access functions are over-ridden by this class (these access functions
   will then provide suitable defaults appropriate to the new coordinate system
   instead). */
         astClearLabel( result, 0 );
         astClearSymbol( result, 0 );
      }

/* If the result Frame is not a SpecFrame, we must temporarily clear the
   System value since the System values used by this class are only
   appropriate to this class. */
   } else {
      if( astTestSystem( template ) ) {
         astClearSystem( template );
         resetSystem = 1;
      }
   }

/* Invoke the parent class astOverlay method to transfer attributes inherited
   from the parent class. */
   (*parent_overlay)( template, template_axes, result );

/* Reset the System value if necessary */
   if( resetSystem ) astSetSystem( template, new_system );

/* Check if the result Frame is a SpecFrame or from a class derived from
   SpecFrame. If not, we cannot transfer SpecFrame attributes to it as it is
   insufficiently specialised. In this case simply omit these attributes. */
   if ( specframe && astOK ) {

/* Define macros that test whether an attribute is set in the template and,
   if so, transfers its value to the result. */
#define OVERLAY(attribute) \
   if ( astTest##attribute( template ) ) { \
      astSet##attribute( result, astGet##attribute( template ) ); \
   }

/* Use the macro to transfer each SpecFrame attribute in turn. Note,
   SourceVRF must be overlayed before SourceVel. Otherwise the stored value 
   for SourceVel would be changed from the default SourceVRF to the specified
   SourceVRF when SourceVRF was overlayed. */
      OVERLAY(AlignStdOfRest)
      OVERLAY(GeoLat)
      OVERLAY(GeoLon)
      OVERLAY(RefDec)
      OVERLAY(RefRA)
      OVERLAY(RestFreq)
      OVERLAY(SourceVRF)
      OVERLAY(SourceVel)
      OVERLAY(StdOfRest)
   }

/* Undefine macros local to this function. */
#undef OVERLAY
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     SpecFrame member function (extends the astSetAttrib method inherited from
*     the Mapping class).

*  Description:
*     This function assigns an attribute value for a SpecFrame, the attribute
*     and its value being specified by means of a string of the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in lower
*     case with no white space present. The value to the right of the "="
*     should be a suitable textual representation of the value to be assigned
*     and this will be interpreted according to the attribute's data type.
*     White space surrounding the value is only significant for string
*     attributes.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.

*  Returned Value:
*     void

*  Notes:
*     This protected method is intended to be invoked by the Object astSet
*     method and makes additional attributes accessible to it.
*/

/* Local Vaiables: */
   AstMapping *umap;             /* Mapping between units */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   AstStdOfRestType sor;         /* Standard of rest type code */
   char *a;                      /* Pointer to next character */
   char *new_setting;            /* Pointer value to new attribute setting */
   double dval;                  /* Double atribute value */
   double dtemp;                 /* Temporary double atribute value */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int ulen;                     /* Used length of setting string */
   int namelen;                  /* Length of attribute name in setting */
   int nc;                       /* Number of characters read by astSscanf */
   int off;                      /* Offset of attribute value */
   int off2;                     /* Modified offset of attribute value */
   int sign;                     /* Sign of longitude value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Obtain the used length of the setting string. */
   ulen = astChrLen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* First look for axis attributes defined by the Frame class. Since a
   SpecFrame has only 1 axis, we allow these attributes to be specified
   without a trailing "(axis)" string. */
   if ( !strncmp( setting, "direction=", 10 ) || 
        !strncmp( setting, "bottom=", 7 ) ||
        !strncmp( setting, "top=", 4 ) ||
        !strncmp( setting, "format=", 7 ) ||
        !strncmp( setting, "label=", 6 ) ||
        !strncmp( setting, "symbol=", 7 ) ||
        !strncmp( setting, "unit=", 5 ) ) {

/* Create a new setting string from the original by appending the string
   "(1)" to the end of the attribute name and then use the parent SetAttrib 
   method. */
      new_setting = astMalloc( len + 4 );
      if( new_setting ) {
         memcpy( new_setting, setting, len + 1 );
         a = strchr( new_setting, '=' );
         namelen = a - new_setting;
         memcpy( a, "(1)", 4 );
         a += 3;
         strcpy( a, setting + namelen );
         (*parent_setattrib)( this_object, new_setting );
         new_setting = astFree( new_setting );
      }

/* AlignStdOfRest. */
/* --------------- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "alignstdofrest=%n%*s %n", &off, &nc ) )
        && ( nc >= len ) ) {

/* Convert the string to a StdOfRest code before use. */
      sor = StdOfRestCode( setting + off );
      if ( sor != AST__BADSOR ) {
         astSetAlignStdOfRest( this, sor );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid standard of rest "
                   "description \"%s\".", astGetClass( this ), setting+off );
      }

/* GeoLat. */
/* ------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "geolat=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {

/* If the first character in the value string is "N" or "S", remember the
   sign of the value and skip over the sign character. Default is north
   (+ve). */
      off2 = off;
      if( setting[ off ] == 'N' || setting[ off ] == 'n' ) {
         off2++;
         sign = +1;
      } else if( setting[ off ] == 'S' || setting[ off ] == 's' ) {
         off2++;
         sign = -1;
      } else {
         sign = +1;
      } 

/* Convert the string to a radians value before use. */
      ival = astUnformat( skyframe, 1, setting + off2, &dval );
      if ( ival == ulen - off2  ) {
         astSetGeoLat( this, dval*sign );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid value for "
                   "GeoLat (observers latitude) \"%s\".", astGetClass( this ), 
                   setting + off );
      }

/* GeoLon. */
/* ------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "geolon=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {

/* If the first character in the value string is "E" or "W", remember the
   sign of the value and skip over the sign character. Default is east
   (+ve). */
      off2 = off;
      if( setting[ off ] == 'E' || setting[ off ] == 'e' ) {
         off2++;
         sign = +1;
      } else if( setting[ off ] == 'W' || setting[ off ] == 'w' ) {
         off2++;
         sign = -1;
      } else {
         sign = +1;
      } 

/* Convert the string to a radians value before use (temporarily make the 
   SkyFrame use degrees for longitude axis). */
      astSetAsTime( skyframe, 0, 0 );
      ival = astUnformat( skyframe, 0, setting + off2, &dval );
      astSetAsTime( skyframe, 0, 1 );
      if ( ival == ulen - off2  ) {
         astSetGeoLon( this, dval*sign );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid value for "
                   "GeoLon (observers longitude) \"%s\".", astGetClass( this ), 
                   setting + off );
      }

/* RefDec. */
/* ------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "refdec=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {

/* Convert the string to a radians value before use. */
      ival = astUnformat( skyframe, 1, setting + off, &dval );
      if ( ival == ulen - off  ) {
         astSetRefDec( this, dval );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid reference "
                   "declination \"%s\".", astGetClass( this ), setting + off );
      }

/* RefRA. */
/* ------ */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "refra=%n%*s %n", &off, &nc ) )
              && ( nc >= 6 ) ) {

/* Convert the string to a radians value before use. */
      ival = astUnformat( skyframe, 0, setting + off, &dval );
      if ( ival == ulen - off  ) {
         astSetRefRA( this, dval );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid reference right "
                   "ascension \"%s\".", astGetClass( this ), setting + off );
      }

/* RestFreq. */
/* --------- */
/* Without any units indication - assume GHz. Convert to Hz for storage. */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "restfreq= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetRestFreq( this, dval*1.0E9 );

/* With units indication. */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "restfreq= %lg %n%*s %n", &dval, &off, &nc ) )
        && ( nc >= len ) ) {

/* Is there a Mapping from the supplied units to Hz? If so, use the
   Mapping to convert the supplied value to Hz. */
      if( ( umap = astUnitMapper( setting + off, "Hz", NULL, NULL ) ) ) {
         astTran1( umap, 1, &dval, 1, &dtemp );
         umap = astAnnul( umap );

/* Otherwise, if there is a Mapping from the supplied units to metre,
   assume the supplied unit is a vacuum wavelength. */
      } else if( ( umap = astUnitMapper( setting + off, "m", NULL, NULL ) ) ) {

/* Convert the supplied wavelength to metres. */
         astTran1( umap, 1, &dval, 1, &dtemp );
         umap = astAnnul( umap );

/* Convert the wavelength (m) to frequency (Hz). */ 
         if( dtemp != AST__BAD && dtemp != 0.0 ) {
            dtemp = AST__C/dtemp;
         } else if( astOK ) {
            astError( AST__ATTIN, "astSetAttrib(%s): Invalid rest wavelength "
                   "\"%g %s\" supplied.", astGetClass( this ), dval, setting + off );
         }

/* Otherwise, if there is a Mapping from the supplied units to Joule,
   assume the supplied unit is an energy. */
      } else if( ( umap = astUnitMapper( setting + off, "J", NULL, NULL ) ) ) {

/* Convert the supplied energy to Joules. */
         astTran1( umap, 1, &dval, 1, &dtemp );
         umap = astAnnul( umap );

/* Convert the energy (J) to frequency (Hz). */ 
         if( dtemp != AST__BAD ) {
            dtemp *= 1.0/AST__H;
         } else if( astOK ) {
            astError( AST__ATTIN, "astSetAttrib(%s): Invalid rest energy "
                   "\"%g %s\" supplied.", astGetClass( this ), dval, setting + off );
         }

/* Otherwise report an error. */
      } else if( astOK ) {
         astError( AST__ATTIN, "astSetAttrib(%s): Rest frequency given in an "
                   "unsupported system of units \"%g %s\".", 
                   astGetClass( this ), dval, setting + off );
      }

/* Set the rest frequency. */
      astSetRestFreq( this, dtemp );

/* SourceVel. */
/* ---------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "sourcevel= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {

/* Convert from km/s to m/s */
      astSetSourceVel( this, dval*1.0E3 );

/* SourceVRF */
/* --------- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "sourcevrf=%n%*s %n", &off, &nc ) )
        && ( nc >= len ) ) {

/* Convert the string to a StdOfRest code before use. */
      sor = StdOfRestCode( setting + off );
      if ( sor != AST__BADSOR ) {
         astSetSourceVRF( this, sor );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid standard of rest "
                   "description \"%s\".", astGetClass( this ), setting+off );
      }

/* StdOfRest. */
/* ---------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "stdofrest=%n%*s %n", &off, &nc ) )
              && ( nc >= len ) ) {

/* Convert the string to a StdOfRest code before use. */
      sor = StdOfRestCode( setting + off );
      if ( sor != AST__BADSOR ) {
         astSetStdOfRest( this, sor );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid standard of rest "
                   "description \"%s\".", astGetClass( this ), setting + off );
      }

/* Pass any unrecognised setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }
}

static void SetMaxAxes( AstFrame *this_frame, int maxaxes ) {
/*
*  Name:
*     SetMaxAxes

*  Purpose:
*     Set a value for the MaxAxes attribute of a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void SetMaxAxes( AstFrame *this, int maxaxes )

*  Class Membership:
*     SpecFrame member function (over-rides the astSetMaxAxes method
*     inherited from the Frame class).

*  Description:
*     This function sets the MaxAxes value for a SpecFrame to 1, which 
*     is the only valid value for a SpecFrame, regardless of the value 
*     supplied.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     maxaxes
*        The new value to be set (ignored).

*  Returned Value:
*     void.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the parent astSetMaxAxes method to set a value of 1. */
   (*parent_setmaxaxes)( this_frame, 1 );
}

static void SetMinAxes( AstFrame *this_frame, int minaxes ) {
/*
*  Name:
*     SetMinAxes

*  Purpose:
*     Set a value for the MinAxes attribute of a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void SetMinAxes( AstFrame *this, int minaxes )

*  Class Membership:
*     SpecFrame member function (over-rides the astSetMinAxes method
*     inherited from the Frame class).

*  Description:
*     This function sets the MinAxes value for a SpecFrame to 1, which is 
*     the only valid value for a SpecFrame, regardless of the value 
*     supplied.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     minaxes
*        The new value to be set (ignored).

*  Returned Value:
*     void.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the parent astSetMinAxes method to set a value of 1. */
   (*parent_setminaxes)( this_frame, 1 );
}

static void SetRefPos( AstSpecFrame *this, AstSkyFrame *frm, double lon, 
                       double lat ){
/*
*++
*  Name:
c     astSetRefPos
f     AST_SETREFPOS

*  Purpose:
*     Set the reference position in a specified celestial coordinate system.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "specframe.h"
c     void astSetRefPos( AstSpecFrame *this, AstSkyFrame *frm, double lon, 
c                        double lat )
f     CALL AST_SETREFPOS( THIS, FRM, LON, LAT, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function 
f     This routine 
*     sets the reference position (see attributes RefRA and RefDec) using 
*     axis values (in radians) supplied within the celestial coordinate 
*     system represented by a supplied SkyFrame. 

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the SpecFrame.
c     frm
f     FRM = INTEGER (Given)
*        Pointer to the SkyFrame which defines the celestial coordinate 
*        system in which the longitude and latitude values are supplied.
c        If NULL 
f        If AST__NULL 
*        is supplied, then the supplied longitude and latitude values are 
*        assumed to be FK5 J2000 RA and Dec values.
c     lon
f     LON = DOUBLE PRECISION (Given)
*        The longitude of the reference point, in the coordinate system
*        represented by the supplied SkyFrame (radians).
c     lat
f     LAT = DOUBLE PRECISION (Given)
*        The latitude of the reference point, in the coordinate system
*        represented by the supplied SkyFrame (radians).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*--
*/

/* Local Variables: */
   AstFrameSet *fs;            /* Conversion FrameSet */
   AstFrame *fb;               /* Base Frame */
   AstFrame *fc;               /* Current Frame */
   double xin[ 1 ];            /* Axis 1 values */
   double yin[ 1 ];            /* Axis 2 values */
   double xout[ 1 ];           /* Axis 1 values */
   double yout[ 1 ];           /* Axis 2 values */

/* Check the global error status. */
   if ( !astOK ) return;

/* If no SkyFrame was supplied, just store the supplied RefRA and RefDec 
   values. */
   if( !frm ) {
      astSetRefRA( this, lon );
      astSetRefDec( this, lat );

/* Otherwise, convert the supplied values from the requested system. */
   } else {

/* Find the Mapping from the supplied SkyFrame, to the SkyFrame which 
   describes the internal format in which the RefRA and RefDec attribute 
   values are stored. */
      fs = astFindFrame( frm, skyframe, "" );

/* If alignment was possible, use the Mapping to transform the supplied
   axis values, checking to see if the axes of the supplied SkyFrame have 
   been permuted. */
      if( fs ) {
         
/* Find the longitude axis in the Base Frame, and store the supplied 
   longitude and latitude values. */
         fb = astGetFrame( fs, AST__BASE );
         if( astGetLonAxis( fb ) == 0 ) {
            xin[ 0 ] = lon;
            yin[ 0 ] = lat;
         } else {
            xin[ 0 ] = lat;
            yin[ 0 ] = lon;
         }
         astTran2( fs, 1, xin, yin, 1, xout, yout );

/* Store the corresponding RefRA and RefDec values. */
         fc = astGetFrame( fs, AST__CURRENT );
         if( astGetLonAxis( fc ) == 0 ) {
            astSetRefRA( this, xout[ 0 ] );
            astSetRefDec( this, yout[ 0 ] );
         } else {
            astSetRefRA( this, yout[ 0 ] );
            astSetRefDec( this, xout[ 0 ] );
         }

/* Annul object references. */
         fc = astAnnul( fc );
         fb = astAnnul( fb );
         fs = astAnnul( fs );
      }
   }  
}

static void SetSystem( AstFrame *this_frame, AstSystemType newsys ) {
/*
*  Name:
*     SetSystem

*  Purpose:
*     Set the System attribute for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void SetSystem( AstFrame *this_frame, AstSystemType newsys )

*  Class Membership:
*     SpecFrame member function (over-rides the astSetSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function sets the System attribute for a SpecFrame.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     newsys
*        The new System value to be stored.

*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to SpecFrame structure */
   AstSystemType oldsys;         /* Original System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* Save the original System value */
   oldsys = astGetSystem( this_frame );

/* Use the parent SetSystem method to store the new System value. */
   (*parent_setsystem)( this_frame, newsys );

/* If the system has changed... */
   if( oldsys != newsys ) {

/* Changing the System value will in general require the Units to change
   as well. If the used has previously specified the units to be used with
   the new system, then re-instate them (they are stored in the "usedunits"
   array in the SpecFrame structure). Otherwise, clear the units so that
   the default units will eb used with the new System. */
      if( (int) newsys < this->nuunits && this->usedunits &&
          this->usedunits[ (int) newsys ] ) {
         astSetUnit( this, 0, this->usedunits[ (int) newsys ] );
      } else {
         astClearUnit( this, 0 );
      }

/* Also, clear all attributes which have system-specific defaults. */
      astClearLabel( this_frame, 0 );
      astClearSymbol( this_frame, 0 );
      astClearTitle( this_frame );
   }
}

static void SetUnit( AstFrame *this_frame, int axis, const char *value ) {
/*
*  Name:
*     astSetUnit

*  Purpose:
*     Set a pointer to the Unit string for a SpecFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void SetUnit( AstFrame *this_frame, int axis, const char *value )

*  Class Membership:
*     SpecFrame member function (over-rides the astSetUnit method inherited
*     from the Frame class).

*  Description:
*     This function stores a pointer to the Unit string for a specified axis
*     of a SpecFrame. It also stores the string in the "usedunits" array
*     in the SpecFrame structure, in the element associated with the
*     current System.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     axis
*        The number of the axis (zero-based) for which information is required.
*     unit
*        The new string to store.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   int i;                        /* Loop counter */
   int system;                   /* The SpecFrame's System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astSetUnit" );

/* Store the supplied value as the UsedUnit for the current System. First
   ensure the array is big enough. Free any previous value stored for the
   current system. */
   system = (int) astGetSystem( this );
   if( system >= this->nuunits ) {
      this->usedunits = astGrow( this->usedunits, system + 1, 
                                 sizeof(char *) );
      if( astOK ) {
         for( i = this->nuunits; i < system + 1; i++ ) this->usedunits[ i ] = NULL;
         this->nuunits = system + 1;
      }
   }

/* Now store a copy of the value, if it is different to the stored string. */
   if( astOK && ( !this->usedunits[ system ] ||
                  strcmp( this->usedunits[ system ], value ) ) ) {
      this->usedunits[ system ] = astStore( this->usedunits[ system ],
                                            value, strlen( value ) + 1 );
   }

/* Now use the parent SetUnit method to store the value in the Axis
   structure */
   (*parent_setunit)( this_frame, axis, value );

}

static int SorConvert( AstSpecFrame *this, AstSpecFrame *that,
                       AstSpecMap *specmap ) {
/*
*  Name:
*     SorConvert

*  Purpose:
*     Add a conversion to a SpecMap which transforms between two
*     standards of rest.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int SorConvert( AstSpecFrame *this, AstSpecFrame *that,
*                     AstSpecMap *specmap )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function adds a conversion to a SpecMap which transforms
*     frequencies between the standards of rest specified by "this" to
*     the standard of rest specified by "that". Note the conversion is
*     always between frequency in the two rest frames no matter what the 
*     System attributes of the two SpecFrames may be (which are ignored).

*  Parameters:
*     this
*        The SpecFrame which defines the input rest frame.
*     that
*        The SpecFrame which defines the output rest frame.
*     specmap
*        The SpecMap to which the conversion is to be added.

*  Returned Value:
*     Zero is returned if the conversion could not be performed. One is 
*     returned otherwise.

*/

/* Local Constants: */
#define MAX_ARGS 5               /* Max arguments for an SpecMap conversion */

/* Local Variables: */
   AstStdOfRestType from;        /* Input standard of rest */
   AstStdOfRestType to;          /* Output standard of rest */
   const char *vmess;            /* Text for use in error messages */
   double args[ MAX_ARGS ];      /* Conversion argument array */
   double dec;                   /* DEC of source (radians, FK5 J2000) */
   double epoch;                 /* Epoch of observation (MJD) */
   double lat;                   /* Observers geodetic latitude (radians) */
   double lon;                   /* Observers geodetic longitude (radians) */
   double ra;                    /* RA of source (radians, FK5 J2000) */
   int result;                   /* Returned value */

/* Initialise */
   result = 1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* No conversion is required if the rest frames are equal. */
   if( !EqualSor( this, that ) ) {

/* Define local macros as shorthand for adding spectral coordinate
   conversions to the SpecMap.  Each macro simply stores details of
   the additional arguments in the "args" array and then calls
   astSpecAdd. The macros differ in the number of additional argument
   values. */
#define TRANSFORM_2(cvt,arg0,arg1) \
        args[ 0 ] = arg0; \
        args[ 1 ] = arg1; \
        astSpecAdd( specmap, cvt, args );

#define TRANSFORM_3(cvt,arg0,arg1,arg2) \
        args[ 0 ] = arg0; \
        args[ 1 ] = arg1; \
        args[ 2 ] = arg2; \
        astSpecAdd( specmap, cvt, args );

#define TRANSFORM_5(cvt,arg0,arg1,arg2,arg3,arg4) \
        args[ 0 ] = arg0; \
        args[ 1 ] = arg1; \
        args[ 2 ] = arg2; \
        args[ 3 ] = arg3; \
        args[ 4 ] = arg4; \
        astSpecAdd( specmap, cvt, args );

/* A string for use in error messages. */
      vmess = "convert between different standards of rest";

/* Get the required values from "this". */
      from = astGetStdOfRest( this );
      ra = astGetRefRA( this );
      dec = astGetRefDec( this );
      lon = astGetGeoLon( this );
      lat = astGetGeoLat( this );
      epoch = astGetEpoch( this );

/* Verify that the reference RA and DEC can be used (they are needed by all 
   the conversions used below). */
      VerifyAttrs( this, vmess, "RefRA RefDec", "astMatch" );

/* Convert from the "this" rest frame to heliographic. */
      if( from == AST__TPSOR ) {
         VerifyAttrs( this, vmess, "GeoLon GeoLat Epoch", "astMatch" );
         TRANSFORM_5( "TPF2HL", lon, lat, epoch, ra, dec )
      
      } else if( from == AST__GESOR ) {
         VerifyAttrs( this, vmess, "Epoch", "astMatch" );
         TRANSFORM_3( "GEF2HL", epoch, ra, dec )

      } else if( from == AST__BYSOR ) {
         VerifyAttrs( this, vmess, "Epoch", "astMatch" );
         TRANSFORM_3( "BYF2HL", epoch, ra, dec )

      } else if( from == AST__LKSOR ) {
         TRANSFORM_2( "LKF2HL", ra, dec )

      } else if( from == AST__LDSOR ) {
         TRANSFORM_2( "LDF2HL", ra, dec )

      } else if( from == AST__LGSOR ) {
         TRANSFORM_2( "LGF2HL", ra, dec )

      } else if( from == AST__GLSOR ) {
         TRANSFORM_2( "GLF2HL", ra, dec )

      } else if( from == AST__SCSOR ) {
         TRANSFORM_3( "USF2HL", ConvertSourceVel( this, AST__HLSOR ), 
                      ra, dec )
      }
   
/* Now go from heliocentric to the "to" frame. */  
      to = astGetStdOfRest( that );
      ra = astGetRefRA( that );
      dec = astGetRefDec( that );
      lon = astGetGeoLon( that );
      lat = astGetGeoLat( that );
      epoch = astGetEpoch( that );
      VerifyAttrs( that, vmess, "RefRA RefDec", "astMatch" );

      if( to == AST__TPSOR ) {
         VerifyAttrs( that, vmess, "GeoLon GeoLat Epoch", "astMatch" );
         TRANSFORM_5( "HLF2TP", lon, lat, epoch, ra, dec )
   
      } else if( to == AST__GESOR ) {
         VerifyAttrs( that, vmess, "Epoch", "astMatch" );
         TRANSFORM_3( "HLF2GE", epoch, ra, dec )

      } else if( to == AST__BYSOR ) {
         VerifyAttrs( that, vmess, "Epoch", "astMatch" );
         TRANSFORM_3( "HLF2BY", epoch, ra, dec )

      } else if( to == AST__LKSOR ) {
         TRANSFORM_2( "HLF2LK", ra, dec )

      } else if( to == AST__LDSOR ) {
         TRANSFORM_2( "HLF2LD", ra, dec )

      } else if( to == AST__LGSOR ) {
         TRANSFORM_2( "HLF2LG", ra, dec )

      } else if( to == AST__GLSOR ) {
         TRANSFORM_2( "HLF2GL", ra, dec )

      } else if( to == AST__SCSOR ) {
         TRANSFORM_3( "HLF2US", ConvertSourceVel( that, AST__HLSOR ), 
                      ra, dec )
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef MAX_ARGS
#undef TRANSFORM_2
#undef TRANSFORM_3
#undef TRANSFORM_5
}

static const char *SpecMapUnit( AstSystemType system, const char *method,
                                const char *class ){
/*
*  Name:
*     SpecMapUnit

*  Purpose:
*     Return the default units for a spectral coordinate system type used
*     by the SpecMap class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *SpecMapUnit( AstSystemType system, const char *method,
*                              const char *class )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function returns a textual representation of the 
*     units used by the SpecMap class for the specified spectral 
*     coordinate system. In general, the SpecMap class uses SI units
*     (m/s, Hz, m, etc), but this class (SpecFrame) has default units 
*     more appropriate to astronomers (km/s, GHz, Angstroms, etc).

*  Parameters:
*     system
*        The spectral coordinate system.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     A string describing the default units. This string follows the
*     units syntax described in FITS WCS paper I "Representations of world
*     coordinates in FITS" (Greisen & Calabretta).

*  Notes:
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   const char *result;           /* Value to return */

/* Initialize */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get an identifier for the default units. */
   if( system == AST__FREQ ) {
      result = "Hz";
   } else if( system == AST__ENERGY ) {
      result = "J";
   } else if( system == AST__WAVENUM ) {
      result = "1/m";
   } else if( system == AST__WAVELEN ) {
      result = "m";
   } else if( system == AST__AIRWAVE ) {
      result = "m";
   } else if( system == AST__VRADIO ) {
      result = "m/s";
   } else if( system == AST__VOPTICAL ) {
      result = "m/s";
   } else if( system == AST__REDSHIFT ) {
      result = "";
   } else if( system == AST__BETA ) {
      result = "";
   } else if( system == AST__VREL ) {
      result = "m/s";

/* Report an error if the coordinate system was not recognised. */
   } else {
      astError( AST__SCSIN, "%s(%s): Corrupt %s contains illegal System "
                "identification code (%d).", method, class, class, 
                (int) system );
   }

/* Return the result. */
   return result;
}

static AstStdOfRestType StdOfRestCode( const char *sor ) {
/*
*  Name:
*     StdOfRestCode

*  Purpose:
*     Convert a string into a standard of rest type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     AstStdOfRestType StdOfRestCode( const char *sor )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function converts a string used for the external description of 
*     a standard of rest into a SpecFrame standard of rest type code 
*     (StdOfRest attribute value). It is the inverse of the 
*     StdOfRestString function.

*  Parameters:
*     sor
*        Pointer to a constant null-terminated string containing the
*        external description of the standard of rest.

*  Returned Value:
*     The StdOfRest type code.

*  Notes:
*     - A value of AST__BADSOR is returned if the standard of rest 
*     description was not recognised. This does not produce an error.
*     - A value of AST__BADSOR is also returned if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstStdOfRestType result;      /* Result value to return */

/* Initialise. */
   result = AST__BADSOR;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "sor" string against each possibility and assign the
   result. */
   if ( astChrMatch( "TOPO", sor ) || astChrMatch( "TOPOCENT", sor ) || astChrMatch( "TOPOCENTRIC", sor ) ) {
      result = AST__TPSOR;

   } else if ( astChrMatch( "GEO", sor ) || astChrMatch( "GEOCENTR", sor ) || astChrMatch( "GEOCENTRIC", sor ) ) {
      result = AST__GESOR;

   } else if ( astChrMatch( "BARY", sor ) || astChrMatch( "BARYCENT", sor ) || astChrMatch( "BARYCENTRIC", sor ) ) {
      result = AST__BYSOR;

   } else if ( astChrMatch( "HELIO", sor ) || astChrMatch( "HELIOCEN", sor ) || astChrMatch( "HELIOCENTRIC", sor ) ) {
      result = AST__HLSOR;

   } else if ( astChrMatch( "LSRK", sor ) || astChrMatch( "LSR", sor ) ) {
      result = AST__LKSOR;

   } else if ( astChrMatch( "LSRD", sor ) ) {
      result = AST__LDSOR;

   } else if ( astChrMatch( "GAL", sor ) || astChrMatch( "GALACTOC", sor ) || astChrMatch( "GALACTIC", sor ) ) {
      result = AST__GLSOR;

   } else if ( astChrMatch( "LG", sor ) || astChrMatch( "LOCALGRP", sor ) || 
               astChrMatch( "LOCAL_GROUP", sor ) || astChrMatch( "LOCAL-GROUP", sor ) ) { 
      result = AST__LGSOR;

   } else if ( astChrMatch( "SOURCE", sor ) || astChrMatch( "SRC", sor ) ) {
      result = AST__SCSOR;

   }

/* Return the result. */
   return result;
}

static const char *StdOfRestString( AstStdOfRestType sor ) {
/*
*  Name:
*     StdOfRestString

*  Purpose:
*     Convert a standard of rest type code into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *StdOfRestString( AstStdOfRestType sor )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function converts a SpecFrame standard of rest type code
*     (StdOfRest attribute value) into a string suitable for use as an
*     external representation of the standard of rest type.

*  Parameters:
*     sor
*        The standard of rest type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the standard of rest
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "sor" value against each possibility and convert to a
   string pointer. (Where possible, return the same string as would be
   used in the FITS WCS representation of the standard of rest). */
   switch ( sor ) {

   case AST__TPSOR:
      result = "Topocentric";
      break;

   case AST__GESOR:
      result = "Geocentric";
      break;

   case AST__BYSOR:
      result = "Barycentric";
      break;

   case AST__HLSOR:
      result = "Heliocentric";
      break;

   case AST__LDSOR:
      result = "LSRD";
      break;

   case AST__LKSOR:
      result = "LSRK";
      break;

   case AST__LGSOR:
      result = "Local_group";
      break;

   case AST__GLSOR:
      result = "Galactic";
      break;

   case AST__SCSOR:
      result = "Source";
      break;

   }

/* Return the result pointer. */
   return result;
}

static int SubFrame( AstFrame *target_frame, AstFrame *template,
                     int result_naxes, const int *target_axes,
                     const int *template_axes, AstMapping **map,
                     AstFrame **result ) {
/*
*  Name:
*     SubFrame

*  Purpose:
*     Select axes from a SpecFrame and convert to the new coordinate 
*     system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int SubFrame( AstFrame *target, AstFrame *template,
*                   int result_naxes, const int *target_axes,
*                   const int *template_axes, AstMapping **map,
*                   AstFrame **result )

*  Class Membership:
*     SpecFrame member function (over-rides the protected astSubFrame 
*     method inherited from the Frame class).

*  Description:
*     This function selects a requested sub-set (or super-set) of the axes
*     from a "target" SpecFrame and creates a new Frame with copies of
*     the selected axes assembled in the requested order. It then
*     optionally overlays the attributes of a "template" Frame on to the
*     result. It returns both the resulting Frame and a Mapping that
*     describes how to convert between the coordinate systems described by
*     the target and result Frames. If necessary, this Mapping takes
*     account of any differences in the Frames' attributes due to the
*     influence of the template.

*  Parameters:
*     target
*        Pointer to the target SpecFrame, from which axes are to be 
*        selected.
*     template
*        Pointer to the template Frame, from which new attributes for the
*        result Frame are to be obtained. Optionally, this may be NULL, in
*        which case no overlaying of template attributes will be performed.
*     result_naxes
*        Number of axes to be selected from the target Frame. This number may
*        be greater than or less than the number of axes in this Frame (or
*        equal).
*     target_axes
*        Pointer to an array of int with result_naxes elements, giving a list
*        of the (zero-based) axis indices of the axes to be selected from the
*        target SpecFrame. The order in which these are given determines
*        the order in which the axes appear in the result Frame. If any of the
*        values in this array is set to -1, the corresponding result axis will
*        not be derived from the target Frame, but will be assigned default
*        attributes instead.
*     template_axes
*        Pointer to an array of int with result_naxes elements. This should
*        contain a list of the template axes (given as zero-based axis indices)
*        with which the axes of the result Frame are to be associated. This
*        array determines which axes are used when overlaying axis-dependent
*        attributes of the template on to the result. If any element of this
*        array is set to -1, the corresponding result axis will not receive any
*        template attributes.
*
*        If the template argument is given as NULL, this array is not used and
*        a NULL pointer may also be supplied here.
*     map
*        Address of a location to receive a pointer to the returned Mapping.
*        The forward transformation of this Mapping will describe how to
*        convert coordinates from the coordinate system described by the target
*        SpecFrame to that described by the result Frame. The inverse
*        transformation will convert in the opposite direction.
*     result
*        Address of a location to receive a pointer to the result Frame.

*  Returned Value:
*     A non-zero value is returned if coordinate conversion is possible
*     between the target and the result Frame. Otherwise zero is returned and
*     *map and *result are returned as NULL (but this will not in itself
*     result in an error condition). In general, coordinate conversion should
*     always be possible if no template Frame is supplied but may not always
*     be possible otherwise.

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.

*  Implementation Notes:
*     -  This implementation addresses the selection of axes from a 
*     SpecFrame object. This results in another object of the same class 
*     only if the single SpecFrame axis is selected exactly once. 
*     Otherwise, the result is a Frame class object which inherits the 
*     SpecFrame's axis information (if appropriate) but none of the other 
*     properties of a SpecFrame.
*     -  In the event that a SpecFrame results, the returned Mapping will 
*     take proper account of the relationship between the target and result 
*     coordinate systems.
*     -  In the event that a Frame class object results, the returned Mapping
*     will only represent a selection/permutation of axes.

*  Implementation Deficiencies:
*     -  Any axis selection is currently permitted. Probably this should be
*     restricted so that each axis can only be selected once. The
*     astValidateAxisSelection method will do this but currently there are bugs
*     in the CmpFrame class that cause axis selections which will not pass this
*     test. Install the validation when these are fixed.
*/

/* Local Variables: */
   AstSpecFrame *target;      /* Pointer to the SpecFrame structure */
   AstSpecFrame *temp;        /* Pointer to copy of target SpecFrame */
   AstSpecFrame *align_frm;   /* Frame in which to align the SpecFrames */
   int match;                 /* Coordinate conversion is possible? */
   int report;                /* Report errors if SpecFrames cannot be aligned? */

/* Initialise the returned values. */
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the target SpecFrame structure. */
   target = (AstSpecFrame *) target_frame;

/* Result is a SpecFrame. */
/* -------------------------- */
/* Check if the result Frame is to have one axis obtained by selecting
   the single target SpecFrame axis. If so, the result will also be 
   a SpecFrame. */
   if ( ( result_naxes == 1 ) && ( target_axes[ 0 ] == 0 ) ) {

/* Form the result from a copy of the target. */
      *result = astCopy( target );

/* Initialise a flag to indicate that MakeSpecMapping should not report
   errors if no Mapping can be created. */
      report = 0;

/* If required, overlay the template attributes on to the result SpecFrame.
   Also get the system and standard of rest in which to align the two 
   SpecFrames. These are the values from the template (if there is a 
   template). */
      if ( template ) {
         astOverlay( template, template_axes, *result );
         if( astIsASpecFrame( template ) ) {
            align_frm = astClone( template );

/* Since we now know that both the template and target are SpecFrames, it 
   should usually be possible to convert betwen them. If conversion is
   *not* possible (fpr instance if no rest frequency is availalbe, etc)
   then the user will probably be interested in knowing the reason why
   conversion is not possible. Therefore, indicate that MakeSpecMapping 
   should report errors if no Mapping can be created. */
            report = 1;

         } else {
            align_frm = astClone( target );
         }

/* If no template was supplied, align in the System and StdOfRest of the
   target. */
      } else {
         VerifyAttrs( target, "convert between different spectral systems", 
                   "StdOfRest", "astMatch" );
         align_frm = astClone( target );
      }

/* Generate a Mapping that takes account of changes in the sky coordinate
   system (equinox, epoch, etc.) between the target SpecFrame and the result
   SpecFrame. If this Mapping can be generated, set "match" to indicate that
   coordinate conversion is possible. If the template is a specframe,
   report errors if a match is not possible. */
      match = ( MakeSpecMapping( target, (AstSpecFrame *) *result, 
                align_frm, report, map ) != 0 );

/* Free resources. */
      align_frm = astAnnul( align_frm );

/* Result is not a SpecFrame. */
/* ------------------------------ */
/* In this case, we select axes as if the target were from the Frame
   class.  However, since the resulting data will then be separated
   from their enclosing SpecFrame, default attribute values may differ
   if the methods for obtaining them were over-ridden by the SpecFrame
   class. To overcome this, we ensure that these values are explicitly
   set for the result Frame (rather than relying on their defaults). */
   } else {

/* Make a temporary copy of the target SpecFrame. We will explicitly
   set the attribute values in this copy so as not to modify the original. */
      temp = astCopy( target );

/* Define a macro to test if an attribute is set. If not, set it
   explicitly to its default value. */
#define SET(attribute) \
   if ( !astTest##attribute( temp ) ) { \
      astSet##attribute( temp, astGet##attribute( temp ) ); \
   }

/* Set attribute values which apply to the Frame as a whole and which
   we want to retain, but whose defaults are over-ridden by the
   SpecFrame class. */
      SET(Domain)
      SET(Title)

/* Define a macro to test if an attribute is set for axis zero (the only
   axis of a SpecFrame). If not, set it explicitly to its default value. */
#define SET_AXIS(attribute) \
   if ( !astTest##attribute( temp, 0 ) ) { \
      astSet##attribute( temp, 0, \
                         astGet##attribute( temp, 0 ) ); \
   }

/* Use this macro to set explicit values for all the axis attributes
   for which the SpecFrame class over-rides the default value. */
      SET_AXIS(Label)
      SET_AXIS(Symbol)
      SET_AXIS(Unit)

/* Clear attributes which have an extended range of values allowed by
   this class. */
      astClearSystem( temp );

/* Invoke the astSubFrame method inherited from the Frame class to
   produce the result Frame by selecting the required set of axes and
   overlaying the template Frame's attributes. */
      match = (*parent_subframe)( (AstFrame *) temp, template,
                                  result_naxes, target_axes, template_axes,
                                  map, result );

/* Delete the temporary copy of the target SpecFrame. */
      temp = astDelete( temp );
   }

/* If an error occurred or no match was found, annul the returned
   objects and reset the returned result. */
   if ( !astOK || !match ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;

/* Undefine macros local to this function. */
#undef SET
#undef SET_AXIS
}

static AstSystemType SystemCode( AstFrame *this, const char *system ) {
/*
*  Name:
*     SystemCode

*  Purpose:
*     Convert a string into a coordinate system type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     AstSystemType SystemCode( AstFrame *this, const char *system )

*  Class Membership:
*     SpecFrame member function (over-rides the astSystemCode method
*     inherited from the Frame class).

*  Description:
*     This function converts a string used for the external
*     description of a coordinate system into a SpecFrame
*     coordinate system type code (System attribute value). It is the
*     inverse of the astSystemString function.

*  Parameters:
*     this
*        The Frame.
*     system
*        Pointer to a constant null-terminated string containing the
*        external description of the sky coordinate system.

*  Returned Value:
*     The System type code.

*  Notes:
*     - A value of AST__BADSYSTEM is returned if the sky coordinate
*     system description was not recognised. This does not produce an
*     error.
*     - A value of AST__BADSYSTEM is also returned if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstSystemType result;      /* Result value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" string against each possibility and assign the
   result. */
   if ( astChrMatch( "FREQ", system ) ) {
      result = AST__FREQ;

   } else if ( astChrMatch( "ENER", system ) || astChrMatch( "ENERGY", system ) ) {
      result = AST__ENERGY;

   } else if ( astChrMatch( "WAVN", system ) || astChrMatch( "WAVENUM", system ) ) {
      result = AST__WAVENUM;

   } else if ( astChrMatch( "WAVE", system ) || astChrMatch( "WAVELEN", system ) ) {
      result = AST__WAVELEN;

   } else if ( astChrMatch( "AWAV", system ) || astChrMatch( "AIRWAVE", system ) ) {
      result = AST__AIRWAVE;

   } else if ( astChrMatch( "VRAD", system ) || astChrMatch( "VRADIO", system ) ) {
      result = AST__VRADIO;

   } else if ( astChrMatch( "VOPT", system ) || astChrMatch( "VOPTICAL", system ) ) {
      result = AST__VOPTICAL;

   } else if ( astChrMatch( "ZOPT", system ) || astChrMatch( "REDSHIFT", system ) ) {
      result = AST__REDSHIFT;

   } else if ( astChrMatch( "BETA", system ) ) {
      result = AST__BETA;

   } else if ( astChrMatch( "VELO", system ) || astChrMatch( "VREL", system ) ) {
      result = AST__VREL;

   }

/* Return the result. */
   return result;
}

static const char *SystemLabel( AstSystemType system ) {
/*
*  Name:
*     SystemLabel

*  Purpose:
*     Return a label for a coordinate system type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *SystemLabel( AstSystemType system )

*  Class Membership:
*     SpecFrame member function.

*  Description:
*     This function converts a SpecFrame coordinate system type code
*     (System attribute value) into a descriptive string for human readers.

*  Parameters:
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the sky coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" value against each possibility and convert to a
   string pointer. */
   switch ( system ) {

   case AST__FREQ:
      result = "frequency";
      break;

   case AST__ENERGY:
      result = "energy";
      break;

   case AST__WAVENUM:
      result = "wave-number";
      break;

   case AST__WAVELEN:
      result = "wavelength";
      break;

   case AST__AIRWAVE:
      result = "wavelength in air";
      break;

   case AST__VRADIO:
      result = "radio velocity";
      break;

   case AST__VOPTICAL:
      result = "optical velocity";
      break;

   case AST__REDSHIFT:
      result = "redshift";
      break;

   case AST__BETA:
      result = "beta factor";
      break;

   case AST__VREL:
      result = "apparent radial velocity";
      break;
   }

/* Return the result pointer. */
   return result;
}

static const char *SystemString( AstFrame *this, AstSystemType system ) {
/*
*  Name:
*     SystemString

*  Purpose:
*     Convert a coordinate system type code into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     const char *SystemString( AstFrame *this, AstSystemType system )

*  Class Membership:
*     SpecFrame member function (over-rides the astSystemString method
*     inherited from the Frame class).

*  Description:
*     This function converts a SpecFrame coordinate system type code
*     (System attribute value) into a string suitable for use as an
*     external representation of the coordinate system type.

*  Parameters:
*     this
*        The Frame.
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the sky coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" value against each possibility and convert to a
   string pointer. (Where possible, return the same string as would be
   used in the FITS WCS representation of the coordinate system). */
   switch ( system ) {

   case AST__FREQ:
      result = "FREQ";
      break;

   case AST__ENERGY:
      result = "ENER";
      break;

   case AST__WAVENUM:
      result = "WAVN";
      break;

   case AST__WAVELEN:
      result = "WAVE";
      break;

   case AST__AIRWAVE:
      result = "AWAV";
      break;

   case AST__VRADIO:
      result = "VRAD";
      break;

   case AST__VOPTICAL:
      result = "VOPT";
      break;

   case AST__REDSHIFT:
      result = "ZOPT";
      break;

   case AST__BETA:
      result = "BETA";
      break;

   case AST__VREL:
      result = "VELO";
      break;
   }

/* Return the result pointer. */
   return result;
}

static int TestActiveUnit( AstFrame *this_frame ) {
/*
*  Name:
*     TestActiveUnit

*  Purpose:
*     Test the ActiveUnit flag for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int TestActiveUnit( AstFrame *this_frame ) 

*  Class Membership:
*     SpecFrame member function (over-rides the astTestActiveUnit protected
*     method inherited from the Frame class).

*  Description:
*    This function test the value of the ActiveUnit flag for a SpecFrame, 
*    which is always "unset". 

*  Parameters:
*     this
*        Pointer to the SpecFrame.

*  Returned Value:
*     The result of the test (0).

*/
   return 0;
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a SpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     SpecFrame member function (over-rides the astTestAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a SpecFrame's attributes.

*  Parameters:
*     this
*        Pointer to the SpecFrame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   char *new_attrib;             /* Pointer value to new attribute name */
   int len;                      /* Length of attrib string */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* First look for axis attributes defined by the Frame class. Since a
   SpecFrame has only 1 axis, we allow these attributes to be specified
   without a trailing "(axis)" string. */
   if ( !strcmp( attrib, "direction" ) || 
        !strcmp( attrib, "bottom" ) ||
        !strcmp( attrib, "top" ) ||
        !strcmp( attrib, "format" ) ||
        !strcmp( attrib, "label" ) ||
        !strcmp( attrib, "symbol" ) ||
        !strcmp( attrib, "unit" ) ) {

/* Create a new attribute name from the original by appending the string
   "(1)" and then use the parent TestAttrib method. */
      new_attrib = astMalloc( len + 4 );
      if( new_attrib ) {
         memcpy( new_attrib, attrib, len );
         memcpy( new_attrib + len, "(1)", 4 ); 
         result = (*parent_testattrib)( this_object, new_attrib );
         new_attrib = astFree( new_attrib );
      }

/* AlignStdOfRest. */
/* --------------- */
   } else if ( !strcmp( attrib, "alignstdofrest" ) ) {
      result = astTestAlignStdOfRest( this );

/* GeoLat. */
/* ------- */
   } else if ( !strcmp( attrib, "geolat" ) ) {
      result = astTestGeoLat( this );

/* GeoLon. */
/* ------- */
   } else if ( !strcmp( attrib, "geolon" ) ) {
      result = astTestGeoLon( this );

/* RefDec. */
/* ------- */
   } else if ( !strcmp( attrib, "refdec" ) ) {
      result = astTestRefDec( this );

/* RefRA. */
/* ------ */
   } else if ( !strcmp( attrib, "refra" ) ) {
      result = astTestRefRA( this );

/* RestFreq. */
/* --------- */
   } else if ( !strcmp( attrib, "restfreq" ) ) {
      result = astTestRestFreq( this );

/* SourceVel. */
/* ---------- */
   } else if ( !strcmp( attrib, "sourcevel" ) ) {
      result = astTestSourceVel( this );

/* SourceVRF */
/* --------- */
   } else if ( !strcmp( attrib, "sourcevrf" ) ) {
      result = astTestSourceVRF( this );

/* StdOfRest. */
/* ---------- */
   } else if ( !strcmp( attrib, "stdofrest" ) ) {
      result = astTestStdOfRest( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

static int ValidateSystem( AstFrame *this, AstSystemType system, const char *method ) {
/*
*
*  Name:
*     ValidateSystem

*  Purpose:
*     Validate a value for a Frame's System attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "specframe.h"
*     int ValidateSystem( AstFrame *this, AstSystemType system, 
*                         const char *method )

*  Class Membership:
*     SpecFrame member function (over-rides the astValidateSystem method
*     inherited from the Frame class).

*  Description:
*     This function checks the validity of the supplied system value.
*     If the value is valid, it is returned unchanged. Otherwise, an
*     error is reported and a value of AST__BADSYSTEM is returned.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        The system value to be checked. 
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis index. This method name is used solely
*        for constructing error messages.

*  Returned Value:
*     The validated system value.

*  Notes:
*     - A value of AST__BADSYSTEM will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstSystemType result;              /* Validated system value */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the value is out of bounds, report an error. */
   if ( system < FIRST_SYSTEM || system > LAST_SYSTEM ) {
         astError( AST__AXIIN, "%s(%s): Bad value (%d) given for the System "
                   "attribute of a %s.", method, astGetClass( this ),
                   (int) system, astGetClass( this ) );

/* Otherwise, return the supplied value. */
   } else {
      result = system;
   }

/* Return the result. */
   return result;
}

static void VerifyAttrs( AstSpecFrame *this, const char *purp, 
                         const char *attrs, const char *method ) {
/*
*  Name:
*     VerifyAttrs

*  Purpose:
*     Verify that usable attribute values are available.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specframe.h"
*     void VerifyAttrs( AstSpecFrame *this, const char *purp, 
*                       const char *attrs, const char *method  )

*  Class Membership:
*     SpecFrame member function 

*  Description:
*     This function tests each attribute listed in "attrs". It returns
*     without action if 1) an explicit value has been set for each attribute
*     or 2) the UseDefs attribute of the supplied SpecFrame is non-zero.
*
*     If UseDefs is zero (indicating that default values should not be
*     used for attributes), and any of the named attributes does not have
*     an explicitly set value, then an error is reported.

*  Parameters:
*     this
*        Pointer to the SpecFrame. 
*     purp
*        Pointer to a text string containing a message which will be
*        included in any error report. This shouldindicate the purpose
*        for which the attribute value is required. 
*     attrs
*        A string holding a space separated list of attribute names.
*     method
*        A string holding the name of the calling method for use in error
*        messages.

*/

/* Local Variables: */
   const char *a;
   const char *desc;
   const char *p;
   int len;
   int set;
   int state;

/* Check inherited status */
   if( !astOK ) return;

/* If the SpecFrame has a non-zero value for its UseDefs attribute, then
   all attributes are assumed to have usable values, since the defaults 
   will be used if no explicit value has been set. So we only need to do
   any checks if UseDefs is zero. */
   if( !astGetUseDefs( this ) ) {   

/* Loop round the "attrs" string identifying the start and length of each
   non-blank word in the string. */
      state = 0;
      p = attrs;
      while( 1 ) {
         if( state == 0 ) {
            if( !isspace( *p ) ) {
               a = p;
               len = 1;
               state = 1;
            }
         } else {
            if( isspace( *p ) || !*p ) {
   
/* The end of a word has just been reached. Compare it to each known
   attribute value. Get a flag indicating if the attribute has a set
   value, and a string describing the attribute.*/
               if( len > 0 ) {

                  if( !strncmp( "GeoLat", a, len ) ) {
                     set = astTestGeoLat( this );
                     desc = "observatory latitude";

                  } else if( !strncmp( "GeoLon", a, len ) ) {
                     set = astTestGeoLon( this );
                     desc = "observatory longitude";

                  } else if( !strncmp( "RefRA", a, len ) ) {
                     set = astTestRefRA( this );
                     desc = "source RA";

                  } else if( !strncmp( "RefDec", a, len ) ) {
                     set = astTestRefDec( this );
                     desc = "source Dec";

                  } else if( !strncmp( "RestFreq", a, len ) ) {
                     set = astTestRestFreq( this );
                     desc = "rest frequency";

                  } else if( !strncmp( "SourceVel", a, len ) ) {
                     set = astTestSourceVel( this );
                     desc = "source velocity";

                  } else if( !strncmp( "StdOfRest", a, len ) ) {
                     set = astTestStdOfRest( this );
                     desc = "spectral standard of rest";

                  } else if( !strncmp( "Epoch", a, len ) ) {
                     set = astTestEpoch( this );
                     desc = "epoch of observation";

                  } else {
                     astError( AST__INTER, "VerifyAttrs(SpecFrame): "
                               "Unknown attribute name \"%.*s\" supplied (AST "
                               "internal programming error).", len, a );
                  }

/* If the attribute does not have a set value, report an error. */
                  if( !set && astOK ) {
                     astError( AST__NOVAL, "%s(%s): Cannot %s.", method,
                               astGetClass( this ), purp );
                     astError( AST__NOVAL, "No value has been set for "
                               "the AST \"%.*s\" attribute (%s).", len, a,
                               desc );
                  }

/* Continue the word search algorithm. */
               }
               len = 0;
               state = 0;
            } else {
               len++;
            }
         }
         if( !*(p++) ) break;
      }
   }
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/*
*att++
*  Name:
*     AlignStdOfRest

*  Purpose:
*     Standard of rest to use when aligning SpecFrames.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute controls how a SpecFrame behaves when it is used (by
c     astFindFrame or astConvert) as a template to match another (target)
f     AST_FINDFRAME or AST_CONVERT) as a template to match another (target)
*     SpecFrame. It identifies the standard of rest in which alignment is 
*     to occur. See the StdOfRest attribute for a desription of the values 
*     which may be assigned to this attribute. The default AlignStdOfRest 
*     value is "Helio" (heliographic).
*
c     When astFindFrame or astConvert is used on two SpecFrames (potentially 
f     When AST_FindFrame or AST_CONVERT is used on two SpecFrames (potentially 
*     describing different spectral coordinate systems), it returns a Mapping 
*     which can be used to transform a position in one SpecFrame into the
*     corresponding position in the other. The Mapping is made up of the
*     following steps in the indicated order:
*
*     - Map values from the system used by the target (wavelength,
*     apparent radial velocity, etc) to the system specified by the
*     AlignSystem attribute, using the target's rest frequency if necessary.
*
*     - Map these values from the target's standard of rest to the standard of
*     rest specified by the AlignStdOfRest attribute, using the Epoch, GeoLat, 
*     GeoLon, RefDec and RefRA attributes of the target to define the two
*     standards of rest.
*
*     - Map these values from the standard of rest specified by the 
*     AlignStdOfRest attribute, to the template's standard of rest, using the 
*     Epoch, GeoLat, GeoLon, RefDec and RefRA attributes of the template to 
*     define the two standards of rest.
*
*     - Map these values from the system specified by the AlignSystem 
*     attribute, to the system used by the template, using the template's 
*     rest frequency if necessary.

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The AlignStdOfRest value has a value of AST__BADSOR when not set yielding 
   a default of AST__HLSOR. */
astMAKE_TEST(SpecFrame,AlignStdOfRest,( this->alignstdofrest != AST__BADSOR ))
astMAKE_CLEAR(SpecFrame,AlignStdOfRest,alignstdofrest,AST__BADSOR)
astMAKE_GET(SpecFrame,AlignStdOfRest,AstStdOfRestType,AST__BADSOR,(
            ( this->alignstdofrest == AST__BADSOR ) ? AST__HLSOR : this->alignstdofrest ) )

/* Validate the AlignStdOfRest value being set and report an error if necessary. */
astMAKE_SET(SpecFrame,AlignStdOfRest,AstStdOfRestType,alignstdofrest,(
            ( ( value >= FIRST_SOR ) && ( value <= LAST_SOR ) ) ?
                 value :
                 ( astError( AST__ATTIN, "%s(%s): Bad value (%d) "
                             "given for AlignStdOfRest attribute.",
                             "astSetAlignStdOfRest", astGetClass( this ), (int) value ),

/* Leave the value unchanged on error. */
                                            this->alignstdofrest ) ) )

/*
*att++
*  Name:
*     GeoLat

*  Purpose:
*     The geodetic latitude of the observer 

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the geodetic latitude of the observer, in
*     degrees. Together with the GeoLon, Epoch, RefRA and RefDec attributes, 
*     it defines the Doppler shift introduced by the observers diurnal 
*     motion around the earths axis, which is needed when converting to 
*     or from the topocentric standard of rest. The maximum velocity
*     error which can be caused by an incorrect value is 0.5 km/s. The 
*     default value for the attribute is zero.

*     The value is stored internally in radians, but is converted to and 
*     from a degrees string for access. Some example input formats are: 
*     "22:19:23.2", "22 19 23.2", "22:19.387", "22.32311", "N22.32311", 
*     "-45.6", "S45.6". As indicated, the sign of the latitude can 
*     optionally be indicated using characters "N" and "S" in place of the 
*     usual "+" and "-". When converting the stored value to a string, the 
*     format "[s]dd:mm:ss" is used, when "[s]" is "N" or "S".

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The geodetic latitude of the observer (radians). Clear the GeoLat value by 
   setting it to AST__BAD, returning zero as the default value. Any value is 
   acceptable. */
astMAKE_CLEAR(SpecFrame,GeoLat,geolat,AST__BAD)
astMAKE_GET(SpecFrame,GeoLat,double,0.0,((this->geolat!=AST__BAD)?this->geolat:0.0))
astMAKE_SET(SpecFrame,GeoLat,double,geolat,value)
astMAKE_TEST(SpecFrame,GeoLat,(this->geolat!=AST__BAD))


/*
*att++
*  Name:
*     GeoLon

*  Purpose:
*     The geodetic longitude of the observer 

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the geodetic (or equivalently, geocentric)
*     longitude of the observer, in degrees, measured positive eastwards. 
*     See also attribute GeoLat. The default value is zero.
*
*     The value is stored internally in radians, but is converted to and 
*     from a degrees string for access. Some example input formats are: 
*     "155:19:23.2", "155 19 23.2", "155:19.387", "155.32311", "E155.32311", 
*     "-204.67689", "W204.67689". As indicated, the sign of the longitude can 
*     optionally be indicated using characters "E" and "W" in place of the 
*     usual "+" and "-". When converting the stored value to a string, the 
*     format "[s]ddd:mm:ss" is used, when "[s]" is "E" or "W" and the 
*     numerical value is chosen to be less than 180 degrees.

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The geodetic longitude of the observer (radians). Clear the GeoLon value by 
   setting it to AST__BAD, returning zero as the default value. Any value is 
   acceptable. */
astMAKE_CLEAR(SpecFrame,GeoLon,geolon,AST__BAD)
astMAKE_GET(SpecFrame,GeoLon,double,0.0,((this->geolon!=AST__BAD)?this->geolon:0.0))
astMAKE_SET(SpecFrame,GeoLon,double,geolon,value)
astMAKE_TEST(SpecFrame,GeoLon,(this->geolon!=AST__BAD))


/*
*att++
*  Name:
*     RefDec

*  Purpose:
*     The declination of the reference point

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the FK5 J2000.0 declination of a reference 
*     point on the sky. See the description of attribute RefRA for details. 
*     The default RefDec is "0:0:0".

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The reference declination (FK5 J2000, radians). Clear the RefDec value by 
   setting it to AST__BAD, which results in a default value of zero. Any
   value is acceptable. */
astMAKE_CLEAR(SpecFrame,RefDec,refdec,AST__BAD)
astMAKE_GET(SpecFrame,RefDec,double,0.0,((this->refdec!=AST__BAD)?this->refdec:0.0))
astMAKE_SET(SpecFrame,RefDec,double,refdec,value)
astMAKE_TEST(SpecFrame,RefDec,( this->refdec != AST__BAD ))

/*
*att++
*  Name:
*     RefRA

*  Purpose:
*     The right ascension of the reference point

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute, together with the RefDec attribute, specifies the FK5 
*     J2000.0 coordinates of a reference point on the sky. For 1-dimensional 
*     spectra, this should normally be the position of the source. For 
*     spectral data with spatial coverage (spectral cubes, etc), this should 
*     be close to centre of the spatial coverage. It is used to define the 
*     correction for Doppler shift to be applied when using the 
c     astFindFrame or astConvert 
f     AST_FINDFRAME or AST_CONVERT 
*     method to convert between different standards of rest. 
*
*     The SpecFrame class assumes this velocity correction is spatially
*     invariant. If a single SpecFrame is used (for instance, as a
*     component of a CmpFrame) to describe spectral values at different
*     points on the sky, then it is assumes that the doppler shift at any
*     spatial position is the same as at the reference position. The
*     maximum velocity error introduced by this assumption is of the order
*     of V*SIN(FOV), where FOV is the angular field of view, and V is the
*     relative velocity of the two standards of rest. As an example, when
*     correcting from the observers rest frame (i.e. the topocentric rest
*     frame) to the kinematic local standard of rest the maximum value of V 
*     is about 20 km/s, so for 5 arc-minute field of view the maximum velocity 
*     error introduced by the correction will be about 0.03 km/s. As another 
*     example, the maximum error when correcting from the observers rest frame 
*     to the local group is about 5 km/s over a 1 degree field of view.
*     
*     The RefRA and RefDec attributes are stored internally in radians, but 
*     are converted to and from a string for access. The format "hh:mm:ss.ss" 
*     is used for RefRA, and "dd:mm:ss.s" is used for RefDec. The methods
c     astSetRefPos and astGetRefPos may be used to access the values of
f     AST_SETREFPOS and AST_GETREFPOS may be used to access the value of
*     these attributes directly as unformatted values in radians.
*
*     The default for RefRA is "0:0:0".

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The reference right ascension (FK5 J2000, radians). Clear the RefRA value 
   by setting it to AST__BAD, which gives a default value of 0.0. Any
   value is acceptable. */
astMAKE_CLEAR(SpecFrame,RefRA,refra,AST__BAD)
astMAKE_GET(SpecFrame,RefRA,double,0.0,((this->refra!=AST__BAD)?this->refra:0.0))
astMAKE_SET(SpecFrame,RefRA,double,refra,value)
astMAKE_TEST(SpecFrame,RefRA,( this->refra != AST__BAD ))


/*
*att++
*  Name:
*     RestFreq

*  Purpose:
*     The rest frequency.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute specifies the frequency corresponding to zero
*     velocity. It is used when converting between between velocity-based 
*     coordinate systems and and other coordinate systems (such as frequency, 
*     wavelength, energy, etc). The default value is 1.0E5 GHz.
*
*     When setting a new value for this attribute, the new value can be 
*     supplied either directly as a frequency, or indirectly as a wavelength 
*     or energy, in which case the supplied value is converted to a frequency 
*     before being stored. The nature of the supplied value is indicated by 
*     appending text to the end of the numerical value indicating the units in 
*     which the value is supplied. If the units are not specified, then the 
*     supplied value is assumed to be a frequency in units of GHz. If the 
*     supplied unit is a unit of frequency, the supplied value is assumed to 
*     be a frequency in the given units. If the supplied unit is a unit of 
*     length, the supplied value is assumed to be a (vacuum) wavelength. If 
*     the supplied unit is a unit of energy, the supplied value is assumed to 
*     be an energy. For instance, the following strings all result in 
*     a rest frequency of around 1.4E14 Hz being used: "1.4E5", "1.4E14 Hz",
*     "1.4E14 s**-1", "1.4E5 GHz", "2.14E-6 m", "21400 Angstrom", "9.28E-20 J",
*     "9.28E-13 erg", "0.58 eV", etc. 
*
*     When getting the value of this attribute, the returned value is
*     always a frequency in units of GHz.

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The rest frequency (Hz). Clear the RestFreq value by setting it to AST__BAD,
   which gives 1.0E14 as the default value. Any value is acceptable. */
astMAKE_CLEAR(SpecFrame,RestFreq,restfreq,AST__BAD)
astMAKE_GET(SpecFrame,RestFreq,double,1.0E14,((this->restfreq!=AST__BAD)?this->restfreq:1.0E14))
astMAKE_SET(SpecFrame,RestFreq,double,restfreq,value)
astMAKE_TEST(SpecFrame,RestFreq,( this->restfreq != AST__BAD ))

/*
*att++
*  Name:
*     SourceVel

*  Purpose:
*     The source velocity.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute (together with SourceVRF, RefRA and RefDec) defines the 
*     "Source" standard of rest (see attribute StdOfRest). This is a rest frame
*     which is moving towards the position given by RefRA and RefDec at a 
*     apparent radial ("relativistic") velocity given by SourceVel (in km/s). 
*     When setting a value for SourceVel, the velocity should be supplied in 
*     the rest frame specified by the SourceVRF attribute. Likewise, when 
*     getting the value of SourceVel, it will be returned in the rest frame 
*     specified by the SourceVRF attribute.
*
*     The default value is zero.

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*  Notes:
*     - It is important to set an appropriate value for SourceVRF before
*     setting a value for SourceVel. If a new value is later set for
*     SourceVRF, the value stored for SourceVel will simultaneously be 
*     changed to the new standard of rest.

*att--
*/
/* The source velocity (stored internally as a speed in m/s). Clear it by 
   setting it to AST__BAD, which returns a default value of zero. Any 
   value is acceptable. */
astMAKE_CLEAR(SpecFrame,SourceVel,sourcevel,AST__BAD)
astMAKE_SET(SpecFrame,SourceVel,double,sourcevel,value)
astMAKE_TEST(SpecFrame,SourceVel,( this->sourcevel != AST__BAD ))
astMAKE_GET(SpecFrame,SourceVel,double,0.0,((this->sourcevel!=AST__BAD)?this->sourcevel:0.0))

/*
*att++
*  Name:
*     SourceVRF

*  Purpose:
*     Rest frame in which the source velocity is stored.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute identifies the rest frame in which the source
*     velocity is stored (the source velocity is accessed using attribute
*     SourceVel). When setting a new value for the SourceVel attribute,
*     the source velocity should be supplied in the rest frame indicated
*     by this attribute. Likewise, when getting the value of the SourceVel
*     attribute, the velocity will be returned in this rest frame.
*
*     If the value of SourceVRF is changed, the value stored for SourceVel 
*     will be converted from the old to the new rest frame.
*
*     The values which can be supplied are the same as for the StdOfRest 
*     attribute (except that SourceVRF cannot be set to "Source"). The 
*     default value is "Helio".

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*att--
*/
/* The SourceVRF value has a value of AST__BADSOR when not set yielding 
   a default of AST__HLSOR. */
astMAKE_TEST(SpecFrame,SourceVRF,( this->sourcevrf != AST__BADSOR ))
astMAKE_GET(SpecFrame,SourceVRF,AstStdOfRestType,AST__BADSOR,(
            ( this->sourcevrf == AST__BADSOR ) ? AST__HLSOR : this->sourcevrf ) )

/* When clearing SourceVRF, convert the SourceVel value to heliocentric
  (but only if set)*/
astMAKE_CLEAR(SpecFrame,SourceVRF,sourcevrf,((astTestSourceVel( this )?
astSetSourceVel( this, ConvertSourceVel( this, AST__HLSOR ) ),NULL:NULL),AST__BADSOR))

/* Validate the SourceVRF value being set and report an error if necessary. 
   If OK, convert the stored SourceVel value into the new rest frame (but
only if set)*/
astMAKE_SET(SpecFrame,SourceVRF,AstStdOfRestType,sourcevrf,(
            ( ( value >= FIRST_SOR ) && ( value <= LAST_SOR ) && value != AST__SCSOR ) ?
                 (astTestSourceVel( this )?
                 astSetSourceVel( this, ConvertSourceVel( this, value )),NULL:NULL), value:
                 ( astError( AST__ATTIN, "%s(%s): Bad value (%d) "
                             "given for SourceVRF attribute.",
                             "astSetSourceVRF", astGetClass( this ), (int) value ),

/* Leave the value unchanged on error. */
                                            this->sourcevrf ) ) )

/*
*att++
*  Name:
*     StdOfRest

*  Purpose:
*     Standard of rest.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute identifies the standard of rest to which the spectral
*     axis values of a SpecFrame refer, and may take any of the values
*     listed in the "Standards of Rest" section (below). 
*
*     The default StdOfRest value is "Helio".

*  Applicability:
*     SpecFrame
*        All SpecFrames have this attribute.

*  Standards of Rest:
*     The SpecFrame class supports the following StdOfRest values (all are
*     case-insensitive):
*
*     - "Topocentric", "Topocent" or "Topo": The observers rest-frame (assumed 
*     to be on the surface of the earth). Spectra recorded in this standard of 
*     rest suffer a Doppler shift which varies over the course of a day
*     because of the rotation of the observer around the axis of the earth.
*     This standard of rest must be qualified using the GeoLat, GeoLon, Epoch, 
*     RefRA and RefDec attributes. 
*
*     - "Geocentric", "Geocentr" or "Geo": The rest-frame of the earth centre. 
*     Spectra recorded in this standard of rest suffer a Doppler shift which 
*     varies over the course of a year because of the rotation of the earth 
*     around the Sun. This standard of rest must be qualified using the Epoch, 
*     RefRA and RefDec attributes. 
*
*     - "Barycentric", "Barycent" or "Bary": The rest-frame of the solar-system
*     barycentre. Spectra recorded in this standard of rest suffer a Doppler 
*     shift which depends both on the velocity of the Sun through the Local 
*     Standard of Rest, and on the movement of the planets through the solar 
*     system. This standard of rest must be qualified using the Epoch, RefRA 
*     and RefDec attributes. 
*
*     - "Heliocentric", "Heliocen" or "Helio": The rest-frame of the Sun. 
*     Spectra recorded in this standard of rest suffer a Doppler shift which 
*     depends on the velocity of the Sun through the Local Standard of Rest. 
*     This standard of rest must be qualified using the RefRA and RefDec 
*     attributes. 
*
*     - "LSRK", "LSR": The rest-frame of the kinematical Local Standard of 
*     Rest. Spectra recorded in this standard of rest suffer a Doppler shift 
*     which depends on the velocity of the kinematical Local Standard of Rest 
*     through the galaxy. This standard of rest must be qualified using the 
*     RefRA and RefDec attributes. 
*
*     - "LSRD": The rest-frame of the dynamical Local Standard of Rest. Spectra
*     recorded in this standard of rest suffer a Doppler shift which depends 
*     on the velocity of the dynamical Local Standard of Rest through the 
*     galaxy.  This standard of rest must be qualified using the RefRA and 
*     RefDec attributes. 
*
*     - "Galactic", "Galactoc" or "Gal": The rest-frame of the galactic centre.
*     Spectra recorded in this standard of rest suffer a Doppler shift which 
*     depends on the velocity of the galactic centre through the local group. 
*     This standard of rest must be qualified using the RefRA and RefDec 
*     attributes.
*
*     - "Local_group", "Localgrp" or "LG": The rest-frame of the local group. 
*     This standard of rest must be qualified using the RefRA and RefDec 
*     attributes.
*
*     - "Source", or "src": The rest-frame of the source. This standard of 
*     rest must be qualified using the RefRA, RefDec and SourceVel attributes.
*
*     Where more than one alternative System value is shown above, the
*     first of these will be returned when an enquiry is made.
*att--
*/
/* The StdOfRest value has a value of AST__BADSOR when not set yielding 
   a default of AST__HLSOR. */
astMAKE_TEST(SpecFrame,StdOfRest,( this->stdofrest != AST__BADSOR ))
astMAKE_CLEAR(SpecFrame,StdOfRest,stdofrest,AST__BADSOR)
astMAKE_GET(SpecFrame,StdOfRest,AstStdOfRestType,AST__BADSOR,(
            ( this->stdofrest == AST__BADSOR ) ? AST__HLSOR : this->stdofrest ) )

/* Validate the StdOfRest value being set and report an error if necessary. */
astMAKE_SET(SpecFrame,StdOfRest,AstStdOfRestType,stdofrest,(
            ( ( value >= FIRST_SOR ) && ( value <= LAST_SOR ) ) ?
                 value :
                 ( astError( AST__ATTIN, "%s(%s): Bad value (%d) "
                             "given for StdOfRest attribute.",
                             "astSetStdOfRest", astGetClass( this ), (int) value ),

/* Leave the value unchanged on error. */
                                            this->stdofrest ) ) )

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for SpecFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for SpecFrame objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstSpecFrame *in;             /* Pointer to input SpecFrame */
   AstSpecFrame *out;            /* Pointer to output SpecFrame */
   char *usedunit;               /* Pointer to an element of usedunits array */
   int i;                        /* Loop count */
   int nused;                    /* Size of "usedunits" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output SpecFrames. */
   in = (AstSpecFrame *) objin;
   out = (AstSpecFrame *) objout;

/* Nullify the pointers stored in the output object since these will
   currently be pointing at the input data (since the output is a simple
   byte-for-byte copy of the input). Otherwise, the input data could be
   freed by accidient if the output object is deleted due to an error
   occuring in this function. */
   out->usedunits = NULL;

/* Store the last used units in the output SpecMap. */
   if( in && in->usedunits ) {
      nused = in->nuunits;
      out->usedunits = astMalloc( nused*sizeof( char * ) );
      if( out->usedunits ) {
         for( i = 0; i < nused; i++ ) {
            usedunit = in->usedunits[ i ];
            if( usedunit ) {
               out->usedunits[ i ] = astStore( NULL, usedunit, 
                                               strlen( usedunit ) + 1 );
            } else {
               out->usedunits[ i ] = NULL;
            }
         }
      }
   }

/* If an error has occurred, free the output resources. */
   if( !astOK ) Delete( (AstObject *) out );

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for SpecFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for SpecFrame objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/
   
/* Local Variables: */
   AstSpecFrame *this;
   int i;

/* Release the memory referred to in the SpecFrame structure. */
   this = (AstSpecFrame *) obj;
   if( this && this->usedunits ) {
      for( i = 0; i < this->nuunits; i++ ) {
         this->usedunits[ i ] = astFree( this->usedunits[ i ] );
      }
      this->usedunits = astFree( this->usedunits );
   }
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SpecFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SpecFrame class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SpecFrame whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstSpecFrame *this;           /* Pointer to the SpecFrame structure */
   AstStdOfRestType sor;         /* StdOfRest attribute value */
   char buff[ 20 ];              /* Buffer for item name */
   char comm[ 50 ];              /* Buffer for comment */
   const char *sval;             /* Pointer to string value */
   double dval;                  /* Double value */
   int i;                        /* Loop count */
   int j;                        /* Loop count */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFrame structure. */
   this = (AstSpecFrame *) this_object;

/* Write out values representing the instance variables for the
   SpecFrame class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

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

/* StdOfRest. */
/* ---------- */
   set = TestStdOfRest( this );
   sor = set ? GetStdOfRest( this ) : astGetStdOfRest( this );

/* If set, convert explicitly to a string for the external
   representation. */
   sval = "";
   if ( set ) {
      if ( astOK ) {
         sval = StdOfRestString( sor );

/* Report an error if the StdOfRest value was not recognised. */
         if ( !sval ) {
            astError( AST__SCSIN,
                     "%s(%s): Corrupt %s contains invalid standard of rest "
                     "identification code (%d).", "astWrite", 
                     astGetClass( channel ), astGetClass( this ), (int) sor );
         }
      }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
   } else {
      sval = astGetAttrib( this_object, "stdofrest" );
   }

/* Write out the value. */
   astWriteString( channel, "SoR", set, 1, sval, "Standard of rest" );

/* AlignStdOfRest. */
/* --------------- */
   set = TestAlignStdOfRest( this );
   sor = set ? GetAlignStdOfRest( this ) : astGetAlignStdOfRest( this );

/* If set, convert explicitly to a string for the external representation. */
   if ( set ) {
      if ( astOK ) {
         sval = StdOfRestString( sor );

/* Report an error if the StdOfRest value was not recognised. */
         if ( !sval ) {
            astError( AST__SCSIN,
                     "%s(%s): Corrupt %s contains invalid alignment standard "
                     "of rest identification code (%d).", "astWrite", 
                     astGetClass( channel ), astGetClass( this ), (int) sor );
         }
      }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
   } else {
      sval = astGetAttrib( this_object, "alignstdofrest" );
   }

/* Write out the value. */
   astWriteString( channel, "AlSoR", set, 0, sval, "Alignment standard of rest" );

/* RefRA. */
/* ------ */
   set = TestRefRA( this );
   dval = set ? GetRefRA( this ) : astGetRefRA( this );
   astWriteDouble( channel, "RefRA", set, 0, dval, "Reference RA (rads, FK5 J2000)" );

/* RefDec. */
/* ------- */
   set = TestRefDec( this );
   dval = set ? GetRefDec( this ) : astGetRefDec( this );
   astWriteDouble( channel, "RefDec", set, 0, dval, "Reference Dec (rads, FK5 J2000)" );

/* GeoLat. */
/* ------- */
   set = TestGeoLat( this );
   dval = set ? GetGeoLat( this ) : astGetGeoLat( this );
   astWriteDouble( channel, "GeoLat", set, 0, dval, "Observers geodetic latitude (rads)" );

/* GeoLon. */
/* ------- */
   set = TestGeoLon( this );
   dval = set ? GetGeoLon( this ) : astGetGeoLon( this );
   astWriteDouble( channel, "GeoLon", set, 0, dval, "Observers geodetic longitude (rads)" );

/* RestFreq. */
/* --------- */
   set = TestRestFreq( this );
   dval = set ? GetRestFreq( this ) : astGetRestFreq( this );
   astWriteDouble( channel, "RstFrq", set, 0, dval, "Rest frequency (Hz)" );

/* SourceVel. */
/* ---------- */
   set = TestSourceVel( this );
   dval = set ? GetSourceVel( this ) : astGetSourceVel( this );
   astWriteDouble( channel, "SrcVel", set, 0, dval, "Source velocity (m/s)" );

/* SourceVRF. */
/* ---------- */
   set = TestSourceVRF( this );
   sor = set ? GetSourceVRF( this ) : astGetSourceVRF( this );

/* If set, convert explicitly to a string for the external representation. */
   if ( set ) {
      if ( astOK ) {
         sval = StdOfRestString( sor );

/* Report an error if the value was not recognised. */
         if ( !sval ) {
            astError( AST__SCSIN,
                     "%s(%s): Corrupt %s contains invalid source velocity "
                     "rest frame identification code (%d).", "astWrite", 
                     astGetClass( channel ), astGetClass( this ), (int) sor );
         }
      }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
   } else {
      sval = astGetAttrib( this_object, "sourcevrf" );
   }

/* Write out the value. */
   astWriteString( channel, "SrcVRF", set, 0, sval, "Source velocity rest frame" );

/* UsedUnits */
/* --------- */
   if( this->usedunits ) {
      for( i = 0; i < this->nuunits; i++ ) {
         if( this->usedunits[ i ] ) {
            sprintf( buff, "U%s", astSystemString( this, (AstSystemType) i ));
            for( j = 2; j < strlen( buff ); j++ ) buff[ j ] = tolower( buff[ j ] );
            sprintf( comm, "Preferred units for %s", SystemLabel( (AstSystemType) i ) );
            astWriteString( channel, buff, 1, 0, this->usedunits[ i ], comm );
         }
      }
   }
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsASpecFrame and astCheckSpecFrame functions using the 
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SpecFrame,Frame,check,&class_init)
astMAKE_CHECK(SpecFrame)

AstSpecFrame *astSpecFrame_( const char *options, ... ) {
/*
*+
*  Name:
*     astSpecFrame

*  Purpose:
*     Create a SpecFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specframe.h"
*     AstSpecFrame *astSpecFrame( const char *options, ... )

*  Class Membership:
*     SpecFrame constructor.

*  Description:
*     This function creates a new SpecFrame and optionally initialises its
*     attributes.

*  Parameters:
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new SpecFrame. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new SpecFrame.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic SpecFrame constructor which
*     is available via the protected interface to the SpecFrame class.
*     A public interface is provided by the astSpecFrameId_ function.
*/

/* Local Variables: */
   AstMapping *um;               /* Mapping from default to actual units */
   AstSpecFrame *new;            /* Pointer to new SpecFrame */
   AstSystemType s;              /* System */
   const char *u;                /* Units string */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SpecFrame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSpecFrame( NULL, sizeof( AstSpecFrame ), !class_init, 
                           &class_vtab, "SpecFrame" );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SpecFrame's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* Check the Units are appropriate for the System. */
      u = astGetUnit( new, 0 );
      s = astGetSystem( new );
      um = astUnitMapper( DefUnit( s, "astSpecFrame", "SpecFrame" ), 
                          u, NULL, NULL );
      if( um ) {
         um = astAnnul( um );
      } else {
         astError( AST__BADUN, "astSpecFrame: Inappropriate units (%s) "
                   "specified for a %s axis.", u, SystemLabel( s ) );
      }      

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new SpecFrame. */
   return new;
}

AstSpecFrame *astInitSpecFrame_( void *mem, size_t size, int init,
                                 AstSpecFrameVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitSpecFrame

*  Purpose:
*     Initialise a SpecFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specframe.h"
*     AstSpecFrame *astInitSpecFrame( void *mem, size_t size, int init,
*                                     AstFrameVtab *vtab, const char *name )

*  Class Membership:
*     SpecFrame initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new SpecFrame object. It allocates memory (if
*     necessary) to accommodate the SpecFrame plus any additional data
*     associated with the derived class. It then initialises a
*     SpecFrame structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual function
*     table for a SpecFrame at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SpecFrame is to be
*	created. This must be of sufficient size to accommodate the
*	SpecFrame data (sizeof(SpecFrame)) plus any data used by
*	the derived class. If a value of NULL is given, this function
*	will allocate the memory itself using the "size" parameter to
*	determine its size.
*     size
*        The amount of memory used by the SpecFrame (plus derived
*	class data). This will be used to allocate memory if a value of
*	NULL is given for the "mem" parameter. This value is also stored
*	in the SpecFrame structure, so a valid value must be supplied
*	even if not required for allocating memory.
*     init
*        A logical flag indicating if the SpecFrame's virtual function
*	table is to be initialised. If this value is non-zero, the
*	virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*	associated with the new SpecFrame.
*     name
*        Pointer to a constant null-terminated character string which
*	contains the name of the class to which the new object belongs
*	(it is this pointer value that will subsequently be returned by
*	the astGetClass method).

*  Returned Value:
*     A pointer to the new SpecFrame.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstSpecFrame *new;        /* Pointer to the new SpecFrame */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSpecFrameVtab( vtab, name );

/* Initialise a 1D Frame structure (the parent class) as the first component
   within the SpecFrame structure, allocating memory if necessary. */
   new = (AstSpecFrame *) astInitFrame( mem, size, 0,
                                        (AstFrameVtab *) vtab, name, 1 );

   if ( astOK ) {

/* Initialise the SpecFrame data. */
/* ----------------------------- */
/* Initialise all attributes to their "undefined" values. */
      new->alignstdofrest = AST__BADSOR;    
      new->geolat = AST__BAD;
      new->geolon = AST__BAD;
      new->refdec = AST__BAD;
      new->refra = AST__BAD;
      new->restfreq = AST__BAD;
      new->sourcevel = AST__BAD;
      new->sourcevrf = AST__BADSOR;    
      new->stdofrest = AST__BADSOR;
      new->nuunits = 0;
      new->usedunits = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );

   }

/* Return a pointer to the new object. */
   return new;
}

AstSpecFrame *astLoadSpecFrame_( void *mem, size_t size,
                                 AstSpecFrameVtab *vtab, 
                                 const char *name, AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadSpecFrame

*  Purpose:
*     Load a SpecFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specframe.h"
*     AstSpecFrame *astLoadSpecFrame( void *mem, size_t size,
*                                      AstSpecFrameVtab *vtab, 
*                                      const char *name, AstChannel *channel )

*  Class Membership:
*     SpecFrame loader.

*  Description:
*     This function is provided to load a new SpecFrame using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SpecFrame structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the SpecFrame is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SpecFrame data (sizeof(SpecFrame)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SpecFrame (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SpecFrame structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSpecFrame) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SpecFrame. If this is NULL, a pointer
*        to the (static) virtual function table for the SpecFrame class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SpecFrame" is used instead.

*  Returned Value:
*     A pointer to the new SpecFrame.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstSpecFrame *new;            /* Pointer to the new SpecFrame */
   char buff[ 20 ];              /* Buffer for item name */
   char *sval;                   /* Pointer to string value */
   int i;                        /* Loop count */
   int j;                        /* Loop count */
   int nc;                       /* String length */
   int sys;                      /* System value */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SpecFrame. In this case the
   SpecFrame belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSpecFrame );
      vtab = &class_vtab;
      name = "SpecFrame";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSpecFrameVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SpecFrame. */
   new = astLoadFrame( mem, size, (AstFrameVtab *) vtab, name,
                       channel );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
       astReadClassData( channel, "SpecFrame" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* StdOfRest. */
/* ---------- */
/* Set the default and read the external representation as a string. */
       new->stdofrest = AST__BADSOR;
       sval = astReadString( channel, "sor", NULL );

/* If a value was read, convert from a string to a StdOfRest code. */
       if ( sval ) {
          if ( astOK ) {
             new->stdofrest = StdOfRestCode( sval );

/* Report an error if the value wasn't recognised. */
             if ( new->stdofrest == AST__BADSOR ) {
                astError( AST__ATTIN,
                          "astRead(%s): Invalid standard of rest description "
                          "\"%s\".", astGetClass( channel ), sval );
             }
          }

/* Free the string value. */
          sval = astFree( sval );
       }

/* AlignStdOfRest. */
/* --------------- */
/* Set the default and read the external representation as a string. */
       new->alignstdofrest = AST__BADSOR;
       sval = astReadString( channel, "alsor", NULL );

/* If a value was read, convert from a string to a StdOfRest code. */
       if ( sval ) {
          if ( astOK ) {
             new->alignstdofrest = StdOfRestCode( sval );

/* Report an error if the value wasn't recognised. */
             if ( new->alignstdofrest == AST__BADSOR ) {
                astError( AST__ATTIN,
                          "astRead(%s): Invalid alignment standard of rest "
                          "description \"%s\".", astGetClass( channel ), sval );
             }
          }

/* Free the string value. */
          sval = astFree( sval );
       }

/* GeoLat. */
/* ------- */
      new->geolat = astReadDouble( channel, "geolat", AST__BAD );
      if ( TestGeoLat( new ) ) SetGeoLat( new, new->geolat );

/* GeoLon. */
/* ------- */
      new->geolon = astReadDouble( channel, "geolon", AST__BAD );
      if ( TestGeoLon( new ) ) SetGeoLon( new, new->geolon );

/* RefRA. */
/* ------ */
      new->refra = astReadDouble( channel, "refra", AST__BAD );
      if ( TestRefRA( new ) ) SetRefRA( new, new->refra );

/* RefDec. */
/* ------- */
      new->refdec = astReadDouble( channel, "refdec", AST__BAD );
      if ( TestRefDec( new ) ) SetRefDec( new, new->refdec );

/* RestFreq. */
/* --------- */
      new->restfreq = astReadDouble( channel, "rstfrq", AST__BAD );
      if ( TestRestFreq( new ) ) SetRestFreq( new, new->restfreq );

/* SourceVel. */
/* ---------- */
      new->sourcevel = astReadDouble( channel, "srcvel", AST__BAD );
      if ( TestSourceVel( new ) ) SetSourceVel( new, new->sourcevel );

/* SourceVRF */
/* --------- */
/* Set the default and read the external representation as a string. */
       new->sourcevrf = AST__BADSOR;
       sval = astReadString( channel, "srcvrf", NULL );

/* If a value was read, convert from a string to a StdOfRest code. */
       if ( sval ) {
          if ( astOK ) {
             new->sourcevrf = StdOfRestCode( sval );

/* Report an error if the value wasn't recognised. */
             if ( new->sourcevrf == AST__BADSOR ) {
                astError( AST__ATTIN,
                          "astRead(%s): Invalid source velocity rest frame "
                          "description \"%s\".", astGetClass( channel ), sval );
             }
          }

/* Free the string value. */
          sval = astFree( sval );
       }

/* UsedUnits */
/* --------- */
      new->nuunits = 0;
      new->usedunits = NULL;
      for( sys = FIRST_SYSTEM; sys <= LAST_SYSTEM; sys++ ) {
         nc = sprintf( buff, "u%s", astSystemString( new, (AstSystemType) sys ));
         for( j = 0; j < nc; j++ ) buff[ j ] = tolower( buff[ j ] );
         sval = astReadString( channel, buff, NULL );
         if( sval ) {
            if( (int) sys >= new->nuunits ) {
               new->usedunits = astGrow( new->usedunits, sys + 1, 
                                          sizeof(char *) );
               if( astOK ) {
                  for( i = new->nuunits; i < sys + 1; i++ ) new->usedunits[ i ] = NULL;
                  new->nuunits = sys + 1;
               }
            } else {
               new->usedunits[ sys ] = astFree( new->usedunits[ sys ] );
            }
            if( astOK ) {
               new->usedunits[ sys ] = astStore( new->usedunits[ sys ],
                                                 sval, strlen( sval ) + 1 );
            }
            sval = astFree( sval);
         }
      }

/* If an error occurred, clean up by deleting the new SpecFrame. */
       if ( !astOK ) new = astDelete( new );
   }

/* Return the new SpecFrame pointer. */
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
void astGetRefPos_( AstSpecFrame *this, AstSkyFrame *frm, double *lon, 
                    double *lat ){
   if ( !astOK ) return;
   (**astMEMBER(this,SpecFrame,GetRefPos))(this,frm,lon,lat);
}
void astSetRefPos_( AstSpecFrame *this, AstSkyFrame *frm, double lon, 
                    double lat ){
   if ( !astOK ) return;
   (**astMEMBER(this,SpecFrame,SetRefPos))(this,frm,lon,lat);
}

/* Special public interface functions. */
/* =================================== */
/* These provide the public interface to certain special functions
   whose public interface cannot be handled using macros (such as
   astINVOKE) alone. In general, they are named after the
   corresponding protected version of the function, but with "Id"
   appended to the name. */

/* Public Interface Function Prototypes. */
/* ------------------------------------- */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSpecFrame *astSpecFrameId_( const char *, ... );

/* Special interface function implementations. */
/* ------------------------------------------- */
AstSpecFrame *astSpecFrameId_( const char *options, ... ) {
/*
*++
*  Name:
c     astSpecFrame
f     AST_SPECFRAME

*  Purpose:
*     Create a SpecFrame.

*  Type:
*     Public function.

*  Synopsis:
c     #include "specframe.h"
c     AstSpecFrame *astSpecFrame( const char *options, ... )
f     RESULT = AST_SPECFRAME( OPTIONS, STATUS )

*  Class Membership:
*     SpecFrame constructor.

*  Description:
*     This function creates a new SpecFrame and optionally initialises
*     its attributes.
*
*     A SpecFrame is a specialised form of one-dimensional Frame which 
*     represents various coordinate systems used to describe positions within 
*     an electro-magnetic spectrum. The particular coordinate system to be 
*     used is specified by setting the SpecFrame's System attribute (the 
*     default is wavelength) qualified, as necessary, by other attributes 
*     such as the rest frequency, the standard of rest, the epoch of 
*     observation, etc (see the description of the System attribute for 
*     details).

*  Parameters:
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SpecFrame. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SpecFrame. The syntax used is identical to that for the
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
c     astSpecFrame()
f     AST_SPECFRAME = INTEGER
*        A pointer to the new SpecFrame.

*  Examples:
c     frame = astSpecFrame( "" );
f     FRAME = AST_SPECFRAME( ' ', STATUS )
*        Creates a SpecFrame to describe the default wavelength spectral
*        coordinate system. The RestFreq attribute (rest frequency) is 
*        unspecified, so it will not be possible to align this SpecFrame 
*        with another SpecFrame on the basis of a velocity-based system. The
*        standard of rest is also unspecified. This means that alignment
*        will be possible with other SpecFrames, but no correction will be
*        made for Doppler shift caused by change of rest frame during the 
*        alignment.
c     frame = astSpecFrame( "System=VELO, RestFreq=1.0E15, StdOfRest=LSRK" );
f     FRAME = AST_SPECFRAME( 'System=VELO, RestFreq=1.0E15, StdOfRest=LSRK', STATUS )
*        Creates a SpecFrame describing a apparent radial velocity ("VELO") axis 
*        with rest frequency 1.0E15 Hz (about 3000 Angstroms), measured
*        in the kinematic Local Standard of Rest ("LSRK"). Since the
*        source position has not been specified (using attributes RefRA and 
*        RefDec), it will only be possible to align this SpecFrame with
*        other SpecFrames which are also measured in the LSRK standard of
*        rest.

*  Notes:
*     - When conversion between two SpecFrames is requested (as when
c     supplying SpecFrames to astConvert),
f     supplying SpecFrames AST_CONVERT),
*     account will be taken of the nature of the spectral coordinate systems 
*     they represent, together with any qualifying rest frequency, standard 
*     of rest, epoch values, etc. The AlignSystem and AlignStdOfRest
*     attributes will also be taken into account. The results will therefore 
*     fully reflect the relationship between positions measured in the two
*     systems. In addition, any difference in the Unit attributes of the two 
*     systems will also be taken into account.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astSpecFrame constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astSpecFrame_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astSpecFrame_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.
*/

/* Local Variables: */
   AstMapping *um;               /* Mapping from default to actual units */
   AstSpecFrame *new;            /* Pointer to new SpecFrame */
   AstSystemType s;              /* System */
   const char *u;                /* Units string */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SpecFrame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitSpecFrame( NULL, sizeof( AstSpecFrame ), !class_init, 
                           &class_vtab, "SpecFrame" );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SpecFrame's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* Check the Units are appropriate for the System. */
      u = astGetUnit( new, 0 );
      s = astGetSystem( new );
      um = astUnitMapper( DefUnit( s, "astSpecFrame", "SpecFrame" ), 
                          u, NULL, NULL );
      if( um ) {
         um = astAnnul( um );
      } else {
         astError( AST__BADUN, "astSpecFrame: Inappropriate units (%s) "
                   "specified for a %s axis.", u, SystemLabel( s ) );
      }      

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new SpecFrame. */
   return astMakeId( new );
}




