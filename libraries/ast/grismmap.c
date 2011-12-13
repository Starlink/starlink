/*
*class++
*  Name:
*     GrismMap

*  Purpose:
*     Transform 1-dimensional coordinates using a grism dispersion equation.

*  Constructor Function:
c     astGrismMap
f     AST_GRISMMAP

*  Description:
*     A GrismMap is a specialised form of Mapping which transforms
*     1-dimensional coordinates using the spectral dispersion equation
*     described in FITS-WCS paper III "Representation of spectral
*     coordinates in FITS". This describes the dispersion produced by
*     gratings, prisms and grisms.
*
*     When initially created, the forward transformation of a GrismMap
*     transforms input "grism parameter" values into output wavelength
*     values. The "grism parameter" is a dimensionless value which is
*     linearly related to position on the detector. It is defined in FITS-WCS
*     paper III as "the offset on the detector from the point of intersection
*     of the camera axis, measured in units of the effective local length".
*     The units in which wavelength values are expected or returned is
*     determined by the values supplied for the GrismWaveR, GrismNRP and
*     GrismG attribute: whatever units are used for these attributes will
*     also be used for the wavelength values.

*  Inheritance:
*     The GrismMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     GrismMap also has the following attributes:
*
*     - GrismNR: The refractive index at the reference wavelength
*     - GrismNRP: Rate of change of refractive index with wavelength
*     - GrismWaveR: The reference wavelength
*     - GrismAlpha: The angle of incidence of the incoming light
*     - GrismG: The grating ruling density
*     - GrismM: The interference order
*     - GrismEps: The angle between the normal and the dispersion plane
*     - GrismTheta: Angle between normal to detector plane and reference ray

*  Functions:
c     The GrismMap class does not define any new functions beyond those
f     The GrismMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

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
*     DSB: David S. Berry (Starlink)

*  History:
*     18-JUN-2003 (DSB):
*        Original version.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS GrismMap


/*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear an attribute value for a GrismMap.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "grismmap.h"
*     MAKE_CLEAR(class,attribute,component,assign)

*  Class Membership:
*     Defined by the GrismMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstGrismMap *this )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstGrismMap *this )
*
*     which implement a method for clearing a specified attribute value for
*     a class. The derived constants stored in the GrismMap structure are
*     updated after the attribute has been cleared.

*  Parameters:
*     class
*        The name (not the type) of the class to which the attribute belongs.
*     attribute
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. Label in "astClearLabel").
*     component
*        The name of the class structure component that holds the attribute
*        value.
*     assign
*        An expression that evaluates to the value to assign to the component
*        to clear its value.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_CLEAR(class,attribute,component,assign) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attribute( Ast##class *this, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Assign the "clear" value. */ \
   this->component = (assign); \
\
/* Update the derived constants. */ \
   UpdateConstants( this, status ); \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attribute##_( Ast##class *this, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,class,Clear##attribute))( this, status ); \
}

/*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set an attribute value for a GrismMap.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "grismmap.h"
*     astMAKE_SET(class,attribute,type,component,assign)

*  Class Membership:
*     Defined by the GrismMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstGrismMap *this, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstGrismMap *this, <Type> value )
*
*     which implement a method for setting a specified attribute value for a
*     GrismMap. The derived constants stored in the GrismMap structure are
*     updated after the attribute has been cleared.

*  Parameters:
*      class
*         The name (not the type) of the class to which the attribute belongs.
*      attribute
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. Label in "astSetLabel").
*      type
*         The C type of the attribute.
*      component
*         The name of the class structure component that holds the attribute
*         value.
*      assign
*         An expression that evaluates to the value to be assigned to the
*         component.

*  Notes:
*     - To avoid problems with some compilers, you should not leave
*     any white space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_SET(class,attribute,type,component,assign) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attribute( Ast##class *this, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Store the new value in the structure component. */ \
   this->component = (assign); \
\
/* Update the derived constants. */ \
   UpdateConstants( this, status ); \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attribute##_( Ast##class *this, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,class,Set##attribute))( this, value, status ); \
}


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory management facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "unitmap.h"             /* Unit Mappings */
#include "channel.h"             /* I/O channels */
#include "zoommap.h"             /* ZoomMap interface */
#include "winmap.h"              /* WinMap interface */
#include "grismmap.h"            /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macros to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(GrismMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(GrismMap,Class_Init)
#define class_vtab astGLOBAL(GrismMap,Class_Vtab)
#define getattrib_buff astGLOBAL(GrismMap,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstGrismMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstGrismMap *astGrismMapId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static AstMapping *CanMerge( AstMapping *, int, AstMapping *, int, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int Equal( AstObject *, AstObject *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void UpdateConstants( AstGrismMap *, int * );

static double GetGrismNR( AstGrismMap *, int * );
static int TestGrismNR( AstGrismMap *, int * );
static void ClearGrismNR( AstGrismMap *, int * );
static void SetGrismNR( AstGrismMap *, double, int * );

static double GetGrismNRP( AstGrismMap *, int * );
static int TestGrismNRP( AstGrismMap *, int * );
static void ClearGrismNRP( AstGrismMap *, int * );
static void SetGrismNRP( AstGrismMap *, double, int * );

static double GetGrismWaveR( AstGrismMap *, int * );
static int TestGrismWaveR( AstGrismMap *, int * );
static void ClearGrismWaveR( AstGrismMap *, int * );
static void SetGrismWaveR( AstGrismMap *, double, int * );

static double GetGrismAlpha( AstGrismMap *, int * );
static int TestGrismAlpha( AstGrismMap *, int * );
static void ClearGrismAlpha( AstGrismMap *, int * );
static void SetGrismAlpha( AstGrismMap *, double, int * );

static double GetGrismG( AstGrismMap *, int * );
static int TestGrismG( AstGrismMap *, int * );
static void ClearGrismG( AstGrismMap *, int * );
static void SetGrismG( AstGrismMap *, double, int * );

static int GetGrismM( AstGrismMap *, int * );
static int TestGrismM( AstGrismMap *, int * );
static void ClearGrismM( AstGrismMap *, int * );
static void SetGrismM( AstGrismMap *, int, int * );

static double GetGrismEps( AstGrismMap *, int * );
static int TestGrismEps( AstGrismMap *, int * );
static void ClearGrismEps( AstGrismMap *, int * );
static void SetGrismEps( AstGrismMap *, double, int * );

static double GetGrismTheta( AstGrismMap *, int * );
static int TestGrismTheta( AstGrismMap *, int * );
static void ClearGrismTheta( AstGrismMap *, int * );
static void SetGrismTheta( AstGrismMap *, double, int * );

/* Member functions. */
/* ================= */
static AstMapping *CanMerge( AstMapping *map1, int inv1, AstMapping *map2,
                             int inv2, int *status ){
/*
*
*  Name:
*     CanMerge

*  Purpose:
*     Checks if two GrismMaps can be merged.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     AstMapping *CanMerge( AstMapping *map1, int inv1, AstMapping *map2,
*                           int inv2, int *status )

*  Class Membership:
*     GrismMap internal utility function.

*  Description:
*     This function checks the two supplied Mappings to see if they can
*     be merged into a single Mapping. One of the two Mappings should be
*     a GrismMap. If they can be merged, the Merged Mapping is returned
*     as the function value. Otherwise NULL is returned.

*  Parameters:
*     map1
*        A pointer to the first mapping.
*     map2
*        A pointer to the second mapping.
*     inv1
*        The invert flag to use with the first mapping.
*     inv2
*        The invert flag to use with the second mapping.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the merged Mapping if the supplied Mappings can be merged,
*     NULL otherwise.

*/

/* Local Variables: */
   AstGrismMap *gmap2;        /* Pointer to second GrismMap */
   AstGrismMap *gmap;         /* Pointer to first GrismMap */
   AstMapping *ret;           /* Returned merged Mapping */
   double g;                  /* The value of the GrismG attribute */
   double nrp;                /* The value of the GrismNRP attribute */
   double waver;              /* The value of the GrismWaveR attribute */
   double z;                  /* Wavelength scaling */
   int invert_result;         /* Is "ret" the inverse of the required Mapping? */

/* Initialise the returned value. */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   gmap = NULL;
   invert_result = 0;

/* Initialise the zoom factor of the adjacent ZoomMap to indicate
   that we have not yet found an adjacent ZoomMap. */
   z = AST__BAD;

/* If the first Mapping is a GrismMap... */
   if( !strcmp( "GrismMap", astGetClass( map1 ) ) ) {
      gmap = (AstGrismMap *) map1;

/* If the second Mapping is also a GrismMap, they can be merged into a
   UnitMap if one GrismMap is the inverse of the other. */
      if( !strcmp( "GrismMap", astGetClass( map2 ) ) ) {
         gmap2 = (AstGrismMap *) map2;

/* Check that the two GrismMaps have the same attribute values. */
         if( EQUAL( astGetGrismNR( gmap ), astGetGrismNR( gmap2 )) &&
             EQUAL( astGetGrismNRP( gmap ), astGetGrismNRP( gmap2 )) &&
             EQUAL( astGetGrismWaveR( gmap ), astGetGrismWaveR( gmap2 )) &&
             EQUAL( astGetGrismAlpha( gmap ), astGetGrismAlpha( gmap2 )) &&
             EQUAL( astGetGrismG( gmap ), astGetGrismG( gmap2 )) &&
             EQUAL( astGetGrismM( gmap ), astGetGrismM( gmap2 )) &&
             EQUAL( astGetGrismEps( gmap ), astGetGrismEps( gmap2 )) &&
             EQUAL( astGetGrismTheta( gmap ), astGetGrismTheta( gmap2 )) ){

/* If so, check that the GrismMaps are applied in opposite senses. If so
   we can cancel the two GrismMaps, so return a UnitMap. */
            if( inv1 != inv2 ) ret = (AstMapping *) astUnitMap( 1, "", status );
         }

/* If the first Mapping is a GrismMap but the second one is not... */
      } else {

/* We can merge the GrismMap with the second Mapping if the GrismMap has
   not been inverted (i.e. if the wavelength output produced by the
   GrismMap is fed as input to the second Mapping), and if the second
   Mapping is a ZoomMap. */
         if( !inv1 ) {

/* Indicate that any merged Mapping to be created later will not need to
   be inverted. */
            invert_result = 0;

/* See if the second Mapping is a ZoomMap, and if so, get the zoom
   factor. If the Invert attribute in the ZoomMap is not set to the
   required value, invert the zoom factor. This gives us the required
   *forward* transformation. */
            if( !strcmp( "ZoomMap", astGetClass( map2 ) ) ) {
               z = astGetZoom( (AstZoomMap *) map2 );
               if( astGetInvert( map2 ) != inv2 && z != 0.0 ) z = 1.0/z;
            }
         }
      }

/* If the first Mapping is not a GrismMap, but the second one is... */
   } else if( !strcmp( "GrismMap", astGetClass( map2 ) ) ) {
      gmap = (AstGrismMap *) map2;

/* We can merge the GrismMap with the first Mapping if the GrismMap has
   been inverted (i.e. if the wavelength output produced by the first
   Mapping is fed as input to the inverted GrismMap), and if the first
   Mapping is a ZoomMap. */
      if( inv2 ) {

/* It is easier to consider pairs of Mappings in which an un-inverted
   GrismMap is followed by a ZoomMap (as in the above case). For this
   reason, we invert the Mappings here, so that the merged Mapping created
   later will be in the inverse of the required Mapping. Indicate that the
   merged Mapping will therefore need to be inverted before being returned. */
         invert_result = 1;

/* See if the first Mapping is a ZoomMap. If so, get the zoom factor. If the
   Invert attribute in the ZoomMap is not set to the opposite of the required
   value, invert the zoom factor. This gives us the required *inverse*
   transformation. */
         if( !strcmp( "ZoomMap", astGetClass( map1 ) ) ) {
            z = astGetZoom( (AstZoomMap *) map1 );
            if( astGetInvert( map1 ) == inv1 && z != 0.0 ) z = 1.0/z;
         }
      }
   }

/* If required, produce the merged Mapping by merging the forward
   GrismMap with the following ZoomMap (and then invert the
   resulting Mapping if it is in the wrong direction). */
   if( !ret && z != AST__BAD && z != 0.0 ) {

/* Ensure we have a forward GrismMap. */
      ret = astCopy( gmap );
      astSetInvert( ret, 0 );

/* Get the required GrismMap attribute values. */
      g = astGetGrismG( ret );
      nrp = astGetGrismNRP( ret );
      waver = astGetGrismWaveR( ret );

/* The above code ensures that z is the zoom factor from the wavelength
   produced by the forward GrismMap to the final (modified) wavelength units.
   Set the new GrismMap attribute values. GrismG, GrismNRP and GrismWaveR have
   units of length and are scaled to represent new length units using the
   zoom factor found above. */
      g /= z;
      nrp /= z;
      waver *= z;

      astSetGrismG( ret, g );
      astSetGrismNRP( ret, nrp );
      astSetGrismWaveR( ret, waver );

/* If required invert this GrismMap. */
      if( invert_result ) astInvert( ret );

   }

/* Return the answer. */
   return ret;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     GrismMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     GrismMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the GrismMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstGrismMap *this;             /* Pointer to the GrismMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the GrismMap structure. */
   this = (AstGrismMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */
   if ( !strcmp( attrib, "grismnr" ) ) {
      astClearGrismNR( this );

   } else if ( !strcmp( attrib, "grismnrp" ) ) {
      astClearGrismNRP( this );

   } else if ( !strcmp( attrib, "grismwaver" ) ) {
      astClearGrismWaveR( this );

   } else if ( !strcmp( attrib, "grismalpha" ) ) {
      astClearGrismAlpha( this );

   } else if ( !strcmp( attrib, "grismg" ) ) {
      astClearGrismG( this );

   } else if ( !strcmp( attrib, "grismm" ) ) {
      astClearGrismM( this );

   } else if ( !strcmp( attrib, "grismeps" ) ) {
      astClearGrismEps( this );

   } else if ( !strcmp( attrib, "grismtheta" ) ) {
      astClearGrismTheta( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two GrismMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     GrismMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two GrismMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a GrismMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the GrismMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstGrismMap *that;
   AstGrismMap *this;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two GrismMap structures. */
   this = (AstGrismMap *) this_object;
   that = (AstGrismMap *) that_object;

/* Check the second object is a GrismMap. We know the first is a
   GrismMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAGrismMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two GrismMaps differ, it may still be possible
   for them to be equivalent. First compare the GrismMaps if their Invert
   flags are the same. In this case all the attributes of the two GrismMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

            if( astEQUAL( this->nr, that->nr ) &&
                astEQUAL( this->nrp, that->nrp ) &&
                astEQUAL( this->waver, that->waver ) &&
                astEQUAL( this->alpha, that->alpha ) &&
                astEQUAL( this->g, that->g ) &&
                astEQUAL( this->m, that->m ) &&
                astEQUAL( this->eps, that->eps ) &&
                astEQUAL( this->theta, that->theta ) &&
                astEQUAL( this->k1, that->k1 ) &&
                astEQUAL( this->k2, that->k2 ) &&
                astEQUAL( this->k3, that->k3 ) ) {
                result = 1;
             }

/* If the Invert flags for the two GrismMaps differ, the attributes of the two
   GrismMaps must be inversely related to each other. */
         } else {

/* In the specific case of a GrismMap, Invert flags must be equal. */
            result = 0;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     GrismMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a GrismMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the GrismMap.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the GrismMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the GrismMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/


/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstGrismMap *this;            /* Pointer to the GrismMap structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the GrismMap structure. */
   this = (AstGrismMap *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

   if ( !strcmp( attrib, "grismnr" ) ) {
      dval = astGetGrismNR( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismnrp" ) ) {
      dval = astGetGrismNRP( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismwaver" ) ) {
      dval = astGetGrismWaveR( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismalpha" ) ) {
      dval = astGetGrismAlpha( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismg" ) ) {
      dval = astGetGrismG( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismm" ) ) {
      dval = astGetGrismM( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismeps" ) ) {
      dval = astGetGrismEps( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

   } else if ( !strcmp( attrib, "grismtheta" ) ) {
      dval = astGetGrismTheta( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;
}

void astInitGrismMapVtab_(  AstGrismMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitGrismMapVtab

*  Purpose:
*     Initialise a virtual function table for a GrismMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "grismmap.h"
*     void astInitGrismMapVtab( AstGrismMapVtab *vtab, const char *name )

*  Class Membership:
*     GrismMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the GrismMap class.

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
   will be used (by astIsAGrismMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->ClearGrismNR = ClearGrismNR;
   vtab->GetGrismNR = GetGrismNR;
   vtab->SetGrismNR = SetGrismNR;
   vtab->TestGrismNR = TestGrismNR;

   vtab->ClearGrismNRP = ClearGrismNRP;
   vtab->GetGrismNRP = GetGrismNRP;
   vtab->SetGrismNRP = SetGrismNRP;
   vtab->TestGrismNRP = TestGrismNRP;

   vtab->ClearGrismWaveR = ClearGrismWaveR;
   vtab->GetGrismWaveR = GetGrismWaveR;
   vtab->SetGrismWaveR = SetGrismWaveR;
   vtab->TestGrismWaveR = TestGrismWaveR;

   vtab->ClearGrismAlpha = ClearGrismAlpha;
   vtab->GetGrismAlpha = GetGrismAlpha;
   vtab->SetGrismAlpha = SetGrismAlpha;
   vtab->TestGrismAlpha = TestGrismAlpha;

   vtab->ClearGrismG = ClearGrismG;
   vtab->GetGrismG = GetGrismG;
   vtab->SetGrismG = SetGrismG;
   vtab->TestGrismG = TestGrismG;

   vtab->ClearGrismM = ClearGrismM;
   vtab->GetGrismM = GetGrismM;
   vtab->SetGrismM = SetGrismM;
   vtab->TestGrismM = TestGrismM;

   vtab->ClearGrismEps = ClearGrismEps;
   vtab->GetGrismEps = GetGrismEps;
   vtab->SetGrismEps = SetGrismEps;
   vtab->TestGrismEps = TestGrismEps;

   vtab->ClearGrismTheta = ClearGrismTheta;
   vtab->GetGrismTheta = GetGrismTheta;
   vtab->SetGrismTheta = SetGrismTheta;
   vtab->TestGrismTheta = TestGrismTheta;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the class dump, copy and delete functions.*/
   astSetDump( vtab, Dump, "GrismMap",
               "Map 1-d coordinates using a spectral disperser" );

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
*     Simplify a sequence of Mappings containing a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     GrismMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated GrismMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated GrismMap with a Mapping which it
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
*        Pointer to the nominated GrismMap which is to be merged with
*        its neighbours. This should be a cloned copy of the GrismMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        GrismMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated GrismMap resides.
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
   AstMapping *merged_map; /* Merger of two Mappings */
   AstMapping *neighbour;  /* Pointer to neighbouring Mapping */
   const char *class1;     /* Pointer to first Mapping class string */
   const char *class2;     /* Pointer to second Mapping class string */
   int i1;                 /* Lower index of the two GrismMaps being merged */
   int i2;                 /* Upper index of the two GrismMaps being merged */
   int i;                  /* Mapping index */
   int result;             /* Result value to return */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   i1 = -1;
   i2 = -1;

/* See if the GrismMap can be merged with the Mappings on either side of it
   in the list. This can only be done in series for a GrismMap. */
/* ===================================================================== */
   if( series ) {

/* Store the classes of the neighbouring Mappings in the list. */
      class1 = ( where > 0 ) ? astGetClass( ( *map_list )[ where - 1 ] ) : NULL;
      class2 = ( where < *nmap - 1 ) ? astGetClass( ( *map_list )[ where + 1 ] ) : NULL;

/* Set a flag indicating that we have not yet found a neighbour with which
   the GrismMap can be merged. */
      merged_map = NULL;

/* First check the lower neighbour (if any). */
      if( where > 0 ) {
         i1 = where - 1;
         i2 = where;
         neighbour = ( *map_list )[ i1 ];
         merged_map = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                                ( *map_list )[ i2 ], (* invert_list)[ i2 ], status );
      }

/* If the GrismMap can not be merged with its lower neighbour, check its
   upper neighbour (if any) in the same way. */
      if( !merged_map && where < *nmap - 1 ) {
         i1 = where;
         i2 = where + 1;
         neighbour = ( *map_list )[ i2 ];
         merged_map = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                                ( *map_list )[ i2 ], (* invert_list)[ i2 ], status );
      }

/* If either neighbour has passed these checks, replace the pair of
   Mappings which have been merged with the single merged Mapping returned
   above. */
      if( merged_map ) {

/* Annul the two Mappings. */
         (void) astAnnul( ( *map_list )[ i1 ] );
         (void) astAnnul( ( *map_list )[ i2 ] );

/* Store a pointer for the merged Mapping in place of the first of the
   two replaced Mappings. */
         ( *map_list )[ i1 ] = merged_map;
         ( *invert_list )[ i1 ] = astGetInvert( merged_map );

/* Shuffle down the remaining Mappings to fill the hole left by the
   second of the replaced Mappings. */
         for ( i = i2 + 1; i < *nmap; i++ ) {
            ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
            ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
         }

/* Clear the vacated element at the end. */
         ( *map_list )[ *nmap - 1 ] = NULL;
         ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
         (*nmap)--;
         result = i1;

      }
   }

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     GrismMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a GrismMap, the
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
*        Pointer to the GrismMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstGrismMap *this;            /* Pointer to the GrismMap structure */
   double dval;                  /* Attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the GrismMap structure. */
   this = (AstGrismMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

   if (  nc = 0, ( 1 == astSscanf( setting, "grismnr= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismNR( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismnrp= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismNRP( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismwaver= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismWaveR( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismalpha= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismAlpha( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismg= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismG( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismm= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismM( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismeps= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismEps( this, dval );

   } else if (  nc = 0, ( 1 == astSscanf( setting, "grismtheta= %lf %n", &dval, &nc ) ) && ( nc >= len ) ) {
      astSetGrismTheta( this, dval );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     GrismMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a GrismMap's attributes.

*  Parameters:
*     this
*        Pointer to the GrismMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
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
   AstGrismMap *this;             /* Pointer to the GrismMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the GrismMap structure. */
   this = (AstGrismMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */
   if ( !strcmp( attrib, "grismnr" ) ) {
      result = astTestGrismNR( this );

   } else if ( !strcmp( attrib, "grismnrp" ) ) {
      result = astTestGrismNRP( this );

   } else if ( !strcmp( attrib, "grismwaver" ) ) {
      result = astTestGrismWaveR( this );

   } else if ( !strcmp( attrib, "grismalpha" ) ) {
      result = astTestGrismAlpha( this );

   } else if ( !strcmp( attrib, "grismg" ) ) {
      result = astTestGrismG( this );

   } else if ( !strcmp( attrib, "grismm" ) ) {
      result = astTestGrismM( this );

   } else if ( !strcmp( attrib, "grismeps" ) ) {
      result = astTestGrismEps( this );

   } else if ( !strcmp( attrib, "grismtheta" ) ) {
      result = astTestGrismTheta( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a GrismMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     GrismMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a GrismMap and a set of points encapsulated
*     in a PointSet and transforms the points so as to apply the
*     forward or inverse dispersal equation.

*  Parameters:
*     this
*        Pointer to the GrismMap.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation should be applied, while a zero value requests
*        the inverse transformation.
*     out
*        Pointer to a PointSet which will hold the transformed
*        (output) coordinate values. A NULL value may also be given,
*        in which case a new PointSet will be created by this
*        function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*     - The number of coordinate values per point in the input
*     PointSet must equal 1.
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points (with 1 coordinate value per point)
*     to accommodate the result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstGrismMap *map;             /* Pointer to GrismMap to be applied */
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double sinbeta;               /* Sin( beta ) (see FITS-WCS paper III) */
   double value_in;              /* Input coordinate value */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the GrismMap. */
   map = (AstGrismMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform
   member function inherited from the parent Mapping class. This
   function validates all arguments and generates an output PointSet
   if necessary, but does not actually transform any coordinate
   values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points from the input PointSet and obtain
   pointers for accessing the input and output coordinate values. */
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping,
   according to the direction specified and whether the mapping has
   been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* If any of the parameters are undefined fill the output with bad
   values (if possible). */
   if( map->k1 == AST__BAD || map->k2 == AST__BAD || map->k3 == AST__BAD ) {
      if( astOK ) {
         for ( point = 0; point < npoint; point++ ) {
            ptr_out[ 0 ][ point ] = AST__BAD;
         }
      }

/* Otherwise... */
   } else {

/* Forward transformation. */
/* ----------------------- */
      if ( forward ) {

/* Loop to transform each input point. */
         for ( point = 0; point < npoint; point++ ) {

/* Extract the input grism parameter value. */
            value_in = ptr_in[ 0 ][ point ];

/* Check for bad input coordinates and generate a bad result if necessary. */
            if( value_in == AST__BAD || map->k2 == 0.0 ) {
               ptr_out[ 0 ][ point ] = AST__BAD;

/* Otherwise, apply the algorithm described in FITS-WCS paper III. */
            } else {
               ptr_out[ 0 ][ point ] = ( map->k1 + sin( atan( value_in ) + map->k3 ) )/map->k2;
            }
         }

/* Inverse transformation. */
/* ----------------------- */
      } else {

/* Loop to transform each input point. */
         for ( point = 0; point < npoint; point++ ) {

/* Extract the input wavelength value. */
            value_in = ptr_in[ 0 ][ point ];

/* Check for bad input coordinates and generate a bad result if necessary. */
            if ( value_in == AST__BAD ) {
               ptr_out[ 0 ][ point ] = AST__BAD;

/* Otherwise, apply the algorithm described in FITS-WCS paper III. */
            } else {
               sinbeta = map->k2*value_in - map->k1;
               if( sinbeta < -1.0 || sinbeta > 1.0 ) {
                  ptr_out[ 0 ][ point ] = AST__BAD;
               } else {
                  ptr_out[ 0 ][ point ] = tan( asin( sinbeta ) - map->k3 );
               }
            }
         }
      }
   }

/* Return a pointer to the output PointSet. */
   return result;
}


static void UpdateConstants( AstGrismMap *this, int *status ){
/*
*  Name:
*     UpdateConstants

*  Purpose:
*     Re-calculate the constants used within the transformation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     void UpdateConstants( AstGrismMap *this, int *status )

*  Class Membership:
*     GrismMap member function

*  Description:
*     This function re-calculates the constants used within the
*     transformation on the basis of the current values of the
*     GrismMap attributes. It should be called whenever a new value is
*     set for an attribute, or an attribute is cleared.

*  Parameters:
*     this
*        Pointer to the GrismMap.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double alpha;        /* The current vaue of the GrismAlpha attribute */
   double coseps;       /* cos( eps ) */
   double sinalpha;     /* sin( alpha ) */
   double eps;          /* The current vaue of the GrismEps attribute */
   double g;            /* The current vaue of the GrismG attribute */
   double nr;           /* The current vaue of the GrismNR attribute */
   double nrp;          /* The current vaue of the GrismNRP attribute */
   double sinbeta_r;    /* sin( beta_r ) */
   double theta;        /* The current vaue of the GrismTheta attribute */
   double wave_r;       /* The current vaue of the GrismWaveR attribute */
   int m;               /* The current vaue of the GrismM attribute */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the current attribute values. */
   nr = astGetGrismNR( this );
   nrp = astGetGrismNRP( this );
   wave_r = astGetGrismWaveR( this );
   alpha = astGetGrismAlpha( this );
   g = astGetGrismG( this );
   m = astGetGrismM( this );
   eps = astGetGrismEps( this );
   theta = astGetGrismTheta( this );

/* Re-calculate the constants. */
   coseps = cos( eps );
   sinalpha = sin( alpha );

   this->k1 = sinalpha*( nr - nrp*wave_r );

   if( coseps != 0.0 ) {
      this->k2 = ( g*m/coseps ) - nrp*sinalpha;

      sinbeta_r = g*m*wave_r/coseps - nr*sinalpha;
      if( sinbeta_r < -1.0 || sinbeta_r > 1.0 ) {
         this->k3 = AST__BAD;
      } else {
         this->k3 = asin( sinbeta_r ) + theta;
      }

   } else {
      this->k2 = AST__BAD;
      this->k3 = AST__BAD;
   }

}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/*
*att++
*  Name:
*     GrismNR

*  Purpose:
*     The refractive index at the reference wavelength.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds refractive index of the grism material at the
*     reference wavelength (given by attribute GrismWaveR). The default
*     value is 1.0.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismNR,nr,(AST__BAD))
astMAKE_GET(GrismMap,GrismNR,double,1.0,( ( this->nr == AST__BAD ) ?
                                      1.0 : this->nr ))
MAKE_SET(GrismMap,GrismNR,double,nr,(value) )
astMAKE_TEST(GrismMap,GrismNR,( this->nr != AST__BAD ))

/*
*att++
*  Name:
*     GrismNRP

*  Purpose:
*     The rate of change of refractive index with wavelength.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the rate of change of the refractive index of the
*     grism material with respect to wavelength at the reference wavelength
*     (given by attribute GrismWaveR). The default value is 0.0 (the
*     appropriate value for a pure grating disperser with no prism). The
*     units of this attribute should be consistent with those of attributes
*     GrismWaveR and GrismG.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismNRP,nrp,(AST__BAD))
astMAKE_GET(GrismMap,GrismNRP,double,0.0,( ( this->nrp == AST__BAD ) ?
                                      0.0 : this->nrp ))
MAKE_SET(GrismMap,GrismNRP,double,nrp,(value) )
astMAKE_TEST(GrismMap,GrismNRP,( this->nrp != AST__BAD ))

/*
*att++
*  Name:
*     GrismWaveR

*  Purpose:
*     The reference wavelength.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds reference wavelength. The default value is
*     5000 (Angstrom). The units of this attribute should be consistent with
*     those of attributes GrismNRP and GrismG.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismWaveR,waver,(AST__BAD))
astMAKE_GET(GrismMap,GrismWaveR,double,5000.0,( ( this->waver == AST__BAD ) ?
                                      5000.0 : this->waver ))
MAKE_SET(GrismMap,GrismWaveR,double,waver,(value) )
astMAKE_TEST(GrismMap,GrismWaveR,( this->waver != AST__BAD ))

/*
*att++
*  Name:
*     GrismAlpha

*  Purpose:
*     The angle of incidence of the incoming light on the grating surface.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the angle between the incoming light and the
*     normal to the grating surface, in radians. The default value is 0.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismAlpha,alpha,(AST__BAD))
astMAKE_GET(GrismMap,GrismAlpha,double,0.0,( ( this->alpha == AST__BAD ) ?
                                       0.0 : this->alpha ))
MAKE_SET(GrismMap,GrismAlpha,double,alpha,(value) )
astMAKE_TEST(GrismMap,GrismAlpha,( this->alpha != AST__BAD ))

/*
*att++
*  Name:
*     GrismG

*  Purpose:
*     The grating ruling density.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the number of grating rulings per unit length.
*     The unit of length used should be consistent with the units used
*     for attributes GrismWaveR and GrismNRP. The default value is 0.0.
*     (the appropriate value for a pure prism disperser with no grating).

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismG,g,(AST__BAD))
astMAKE_GET(GrismMap,GrismG,double,0.0,( ( this->g == AST__BAD ) ?
                                      0.0 : this->g ))
MAKE_SET(GrismMap,GrismG,double,g,(value) )
astMAKE_TEST(GrismMap,GrismG,( this->g != AST__BAD ))

/*
*att++
*  Name:
*     GrismM

*  Purpose:
*     The interference order

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute holds the interference order being considered.
*     The default value is 0.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismM,m,(INT_MAX))
astMAKE_GET(GrismMap,GrismM,int,0,( ( this->m == INT_MAX ) ?
                                      0 : this->m ))
MAKE_SET(GrismMap,GrismM,int,m,(value) )
astMAKE_TEST(GrismMap,GrismM,( this->m != INT_MAX ))

/*
*att++
*  Name:
*     GrismEps

*  Purpose:
*      The angle between the normal and the dispersion plane.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the angle (in radians) between the normal to
*     the grating or exit prism face, and the dispersion plane. The
*     dispersion plane is the plane spanned by the incoming and outgoing
*     ray. The default value is 0.0.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismEps,eps,(AST__BAD))
astMAKE_GET(GrismMap,GrismEps,double,0.0,( ( this->eps == AST__BAD ) ?
                                      0.0 : this->eps ))
MAKE_SET(GrismMap,GrismEps,double,eps,(value) )
astMAKE_TEST(GrismMap,GrismEps,( this->eps != AST__BAD ))

/*
*att++
*  Name:
*     GrismTheta

*  Purpose:
*     Angle between normal to detector plane and reference ray.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute gives the angle of incidence of light of the
*     reference wavelength (given by attribute GrismWaveR) onto the
*     detector. Specifically, it holds the angle (in radians) between
*     the normal to the detector plane and an incident ray at the reference
*     wavelength. The default value is 0.0.

*  Applicability:
*     GrismMap
*        All GrismMaps have this attribute.

*att--
*/
MAKE_CLEAR(GrismMap,GrismTheta,theta,(AST__BAD))
astMAKE_GET(GrismMap,GrismTheta,double,0.0,( ( this->theta == AST__BAD ) ?
                                      0.0 : this->theta ))
MAKE_SET(GrismMap,GrismTheta,double,theta,(value) )
astMAKE_TEST(GrismMap,GrismTheta,( this->theta != AST__BAD ))

/* Copy constructor. */
/* ----------------- */
/* No copy constructor is needed, as a byte-by-byte copy suffices. */

/* Destructor. */
/* ----------- */
/* No destructor is needed as no memory, etc. needs freeing. */

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for GrismMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the GrismMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the GrismMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstGrismMap *this;            /* Pointer to the GrismMap structure */
   double dval;                  /* Double value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the GrismMap structure. */
   this = (AstGrismMap *) this_object;

/* Write out values representing the instance variables for the
   GrismMap class.  Accompany these with appropriate comment strings,
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

   set = TestGrismNR( this, status );
   dval = set ? GetGrismNR( this, status ) : astGetGrismNR( this );
   astWriteDouble( channel, "GrmNR", set, 1, dval, "Refractive index at the ref. wavelength" );

   set = TestGrismNRP( this, status );
   dval = set ? GetGrismNRP( this, status ) : astGetGrismNRP( this );
   astWriteDouble( channel, "GrmNRP", set, 1, dval, "Rate of change of refractive index" );

   set = TestGrismWaveR( this, status );
   dval = set ? GetGrismWaveR( this, status ) : astGetGrismWaveR( this );
   astWriteDouble( channel, "GrmWR", set, 1, dval, "Ref. wavelength" );

   set = TestGrismAlpha( this, status );
   dval = set ? GetGrismAlpha( this, status ) : astGetGrismAlpha( this );
   astWriteDouble( channel, "GrmAlp", set, 1, dval, "Angle of incidence of incoming light" );

   set = TestGrismG( this, status );
   dval = set ? GetGrismG( this, status ) : astGetGrismG( this );
   astWriteDouble( channel, "GrmG", set, 1, dval, "Grating ruling density" );

   set = TestGrismM( this, status );
   dval = set ? GetGrismM( this, status ) : astGetGrismM( this );
   astWriteDouble( channel, "GrmM", set, 1, dval, "The interference order" );

   set = TestGrismEps( this, status );
   dval = set ? GetGrismEps( this, status ) : astGetGrismEps( this );
   astWriteDouble( channel, "GrmEps", set, 1, dval, "Angle between grating normal and dispersion plane" );

   set = TestGrismTheta( this, status );
   dval = set ? GetGrismTheta( this, status ) : astGetGrismTheta( this );
   astWriteDouble( channel, "GrmTh", set, 1, dval, "Angle between detector normal and reference ray" );

}


/* Standard class functions. */
/* ========================= */
/* Implement the astIsAGrismMap and astCheckGrismMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(GrismMap,Mapping)
astMAKE_CHECK(GrismMap)

AstGrismMap *astGrismMap_( const char *options, int *status, ...) {
/*
*++
*  Name:
c     astGrismMap
f     AST_GRISMMAP

*  Purpose:
*     Create a GrismMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "grismmap.h"
c     AstGrismMap *astGrismMap( const char *options, ... )
f     RESULT = AST_GRISMMAP( OPTIONS, STATUS )

*  Class Membership:
*     GrismMap constructor.

*  Description:
*     This function creates a new GrismMap and optionally initialises
*     its attributes.
*
*     A GrismMap is a specialised form of Mapping which transforms
*     1-dimensional coordinates using the spectral dispersion equation
*     described in FITS-WCS paper III "Representation of spectral
*     coordinates in FITS". This describes the dispersion produced by
*     gratings, prisms and grisms.
*
*     When initially created, the forward transformation of a GrismMap
*     transforms input "grism parameter" values into output wavelength
*     values. The "grism parameter" is a dimensionless value which is
*     linearly related to position on the detector. It is defined in FITS-WCS
*     paper III as "the offset on the detector from the point of intersection
*     of the camera axis, measured in units of the effective local length".
*     The units in which wavelength values are expected or returned is
*     determined by the values supplied for the GrismWaveR, GrismNRP and
*     GrismG attribute: whatever units are used for these attributes will
*     also be used for the wavelength values.

*  Parameters:
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new GrismMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new GrismMap. The syntax used is identical to that for the
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
c     astGrismMap()
f     AST_GRISMMAP = INTEGER
*        A pointer to the new GrismMap.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstGrismMap *new;             /* Pointer to new GrismMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the GrismMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitGrismMap( NULL, sizeof( AstGrismMap ), !class_init,
                          &class_vtab, "GrismMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   GrismMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new GrismMap. */
   return new;
}

AstGrismMap *astGrismMapId_( const char *options, ... ) {
/*
*  Name:
*     astGrismMapId_

*  Purpose:
*     Create a GrismMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "grismmap.h"
*     AstGrismMap *astGrismMapId( const char *options, int *status, ... )

*  Class Membership:
*     GrismMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astGrismMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astGrismMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astGrismMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astGrismMap_.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The ID value associated with the new GrismMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstGrismMap *new;             /* Pointer to new GrismMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the GrismMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitGrismMap( NULL, sizeof( AstGrismMap ), !class_init,
                          &class_vtab, "GrismMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new GrismMap's
   attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new GrismMap. */
   return astMakeId( new );
}

AstGrismMap *astInitGrismMap_( void *mem, size_t size, int init,
                               AstGrismMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitGrismMap

*  Purpose:
*     Initialise a GrismMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "grismmap.h"
*     AstGrismMap *astInitGrismMap( void *mem, size_t size, int init,
*                                   AstGrismMapVtab *vtab, const char *name )

*  Class Membership:
*     GrismMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new GrismMap object. It allocates memory (if necessary) to accommodate
*     the GrismMap plus any additional data associated with the derived class.
*     It then initialises a GrismMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a GrismMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the GrismMap is to be initialised.
*        This must be of sufficient size to accommodate the GrismMap data
*        (sizeof(GrismMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the GrismMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the GrismMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the GrismMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new GrismMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).

*  Returned Value:
*     A pointer to the new GrismMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstGrismMap *new;              /* Pointer to new GrismMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitGrismMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the GrismMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstGrismMap *) astInitMapping( mem, size, 0,
                                         (AstMappingVtab *) vtab, name,
                                         1, 1, 1, 1 );
   if ( astOK ) {

/* Initialise the GrismMap data. */
/* ---------------------------- */
      new->nr    = AST__BAD;
      new->nrp   = AST__BAD;
      new->waver = AST__BAD;
      new->alpha = AST__BAD;
      new->g     = AST__BAD;
      new->m     = INT_MAX;
      new->eps   = AST__BAD;
      new->theta = AST__BAD;

/* Set up the other required derived constants. */
      UpdateConstants( new, status );

/* If an error occurred, clean up by deleting the new GrismMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new GrismMap. */
   return new;
}

AstGrismMap *astLoadGrismMap_( void *mem, size_t size,
                           AstGrismMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadGrismMap

*  Purpose:
*     Load a GrismMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "grismmap.h"
*     AstGrismMap *astLoadGrismMap( void *mem, size_t size,
*                               AstGrismMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     GrismMap loader.

*  Description:
*     This function is provided to load a new GrismMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     GrismMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a GrismMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the GrismMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        GrismMap data (sizeof(GrismMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the GrismMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the GrismMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstGrismMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new GrismMap. If this is NULL, a pointer
*        to the (static) virtual function table for the GrismMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "GrismMap" is used instead.

*  Returned Value:
*     A pointer to the new GrismMap.

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
   AstGrismMap *new;             /* Pointer to the new GrismMap */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this GrismMap. In this case the
   GrismMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstGrismMap );
      vtab = &class_vtab;
      name = "GrismMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitGrismMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built GrismMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "GrismMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

      new->nr = astReadDouble( channel, "grmnr", AST__BAD );
      if ( TestGrismNR( new, status ) ) SetGrismNR( new, new->nr, status );

      new->nrp = astReadDouble( channel, "grmnrp", AST__BAD );
      if ( TestGrismNRP( new, status ) ) SetGrismNRP( new, new->nrp, status );

      new->waver = astReadDouble( channel, "grmwr", AST__BAD );
      if ( TestGrismWaveR( new, status ) ) SetGrismWaveR( new, new->waver, status );

      new->alpha = astReadDouble( channel, "grmalp", AST__BAD );
      if ( TestGrismAlpha( new, status ) ) SetGrismAlpha( new, new->alpha, status );

      new->g = astReadDouble( channel, "grmg", AST__BAD );
      if ( TestGrismG( new, status ) ) SetGrismG( new, new->g, status );

      new->m = astReadInt( channel, "grmm", INT_MAX );
      if ( TestGrismM( new, status ) ) SetGrismM( new, new->m, status );

      new->eps = astReadDouble( channel, "grmeps", AST__BAD );
      if ( TestGrismEps( new, status ) ) SetGrismEps( new, new->eps, status );

      new->theta = astReadDouble( channel, "grmth", AST__BAD );
      if ( TestGrismTheta( new, status ) ) SetGrismTheta( new, new->theta, status );

/* Set up the other required derived constants. */
      UpdateConstants( new, status );
   }

/* If an error occurred, clean up by deleting the new GrismMap. */
   if ( !astOK ) new = astDelete( new );

/* Return the new GrismMap pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions
   defined by this class. Each simply checks the global error status
   and then locates and executes the appropriate member function,
   using the function pointer stored in the object's virtual function
   table (this pointer is located using the astMEMBER macro defined in
   "object.h").

   Note that the member function may not be the one defined here, as
   it may have been over-ridden by a derived class. However, it should
   still have the same interface. */





