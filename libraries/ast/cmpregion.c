/*
*class++
*  Name:
*     CmpRegion

*  Purpose:
*     A combination of two regions within a single Frame

*  Constructor Function:
c     astCmpRegion
f     AST_CMPREGION

*  Description:
*     A CmpRegion is a Region which allows two component
*     Regions (of any class) to be combined to form a more complex
*     Region. This combination may be performed a boolean AND, OR
*     or XOR (exclusive OR) operator. If the AND operator is
*     used, then a position is inside the CmpRegion only if it is
*     inside both of its two component Regions. If the OR operator is
*     used, then a position is inside the CmpRegion if it is inside
*     either (or both) of its two component Regions. If the XOR operator
*     is used, then a position is inside the CmpRegion if it is inside
*     one but not both of its two component Regions. Other operators can
*     be formed by negating one or both component Regions before using
*     them to construct a new CmpRegion.
*
*     The two component Region need not refer to the same coordinate
*     Frame, but it must be possible for the
c     astConvert
f     AST_CONVERT
*     function to determine a Mapping between them (an error will be
*     reported otherwise when the CmpRegion is created). For instance,
*     a CmpRegion may combine a Region defined within an ICRS SkyFrame
*     with a Region defined within a Galactic SkyFrame. This is
*     acceptable because the SkyFrame class knows how to convert between
*     these two systems, and consequently the
c     astConvert
f     AST_CONVERT
*     function will also be able to convert between them. In such cases,
*     the second component Region will be mapped into the coordinate Frame
*     of the first component Region, and the Frame represented by the
*     CmpRegion as a whole will be the Frame of the first component Region.
*
*     Since a CmpRegion is itself a Region, it can be used as a
*     component in forming further CmpRegions. Regions of arbitrary
*     complexity may be built from simple individual Regions in this
*     way.

*  Inheritance:
*     The CmpRegion class inherits from the Region class.

*  Attributes:
*     The CmpRegion class does not define any new attributes beyond those
*     which are applicable to all Regions.

*  Functions:
c     The CmpRegion class does not define any new functions beyond those
f     The CmpRegion class does not define any new routines beyond those
*     which are applicable to all Regions.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     7-OCT-2004 (DSB):
*        Original version.
*     28-MAY-2007 (DSB):
*        - Corrected RegBaseMesh.
*        - In RegBaseBox, if the CmpRegion is bounded find the box by
*        finding the extreme position sin a mesh covering the boundary.
*     20-JAN-2009 (DSB):
*        Over-ride astRegBasePick.
*     19-MAR-2009 (DSB):
*        Over-ride the astDecompose method.
*     8-SEP-2009 (DSB):
*        Fix logic in RegTrace.
*     9-SEP-2009 (DSB):
*        - Added astCmpRegionList
*        - Added support for XOR
*        - Override astGetObjSize.
*     27-APR-2012 (DSB):
*        - Cache the bounded property.
*        - Speed up plotting of CmpRegions by using the cached negation
*        of a Region instead of setting the Regions's Negated flag (which
*        causes the Region's cache to be cleared).
*     30-APR-2012 (DSB):
*        Use geodesic distance to measure distances around the two component
*        Regions when tracing the border. Previously, a distance normalised
*        from zero to one was used for both component Regions, but this gives
*        greater priority to Regions higher in the CmpRegion nesting order,
*        resulting in a high chance that lower Regions will not be seen.
*     7-JUN-2012 (DSB):
*        Override astRegSplit method.
*     21-NOV-2012 (DSB):
*        Map the regions returned by RegSplit into the current Frame of the
*        CmpRegion.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS CmpRegion

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "region.h"              /* Regions (parent class) */
#include "channel.h"             /* I/O channels */
#include "nullregion.h"          /* Boundless Regions */
#include "cmpregion.h"           /* Interface definition for this class */
#include "unitmap.h"             /* Unit Mapings */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *(* parent_getdefunc)( AstRegion *, int * );
static void (* parent_setregfs)( AstRegion *, AstFrame *, int * );
static AstMapping *(* parent_simplify)( AstMapping *, int * );
static int (* parent_equal)( AstObject *, AstObject *, int * );
static void (* parent_setclosed)( AstRegion *, int, int * );
static void (* parent_setmeshsize)( AstRegion *, int, int * );
static void (* parent_clearclosed)( AstRegion *, int * );
static void (* parent_clearmeshsize)( AstRegion *, int * );
static double (*parent_getfillfactor)( AstRegion *, int * );
static void (*parent_regsetattrib)( AstRegion *, const char *, char **, int * );
static void (*parent_regclearattrib)( AstRegion *, const char *, char **, int * );
static void (* parent_resetcache)( AstRegion *, int * );
static int (* parent_getobjsize)( AstObject *, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(CmpRegion)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(CmpRegion,Class_Init)
#define class_vtab astGLOBAL(CmpRegion,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstCmpRegionVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstCmpRegion *astCmpRegionId_( void *, void *, int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *GetDefUnc( AstRegion *, int * );
static AstRegion *MatchRegion( AstRegion *, int, AstRegion *, const char *, int * );
static AstRegion *RegBasePick( AstRegion *this, int, const int *, int * );
static AstRegion **RegSplit( AstRegion *, int *, int * );
static double GetFillFactor( AstRegion *, int * );
static int CmpRegionList( AstCmpRegion *, int *, AstRegion ***, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetBounded( AstRegion *, int * );
static int GetObjSize( AstObject *, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static int RegTrace( AstRegion *, int, double *, double **, int * );
static void ClearClosed( AstRegion *, int * );
static void ClearMeshSize( AstRegion *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Decompose( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void GetRegions( AstCmpRegion *, AstRegion **, AstRegion **, int *, int *, int *, int * );
static void RegBaseBox( AstRegion *, double *, double *, int * );
static void RegBaseBox2( AstRegion *, double *, double *, int * );
static void RegClearAttrib( AstRegion *, const char *, char **, int * );
static void RegSetAttrib( AstRegion *, const char *, char **, int * );
static void ResetCache( AstRegion *this, int * );
static void SetBreakInfo( AstCmpRegion *, int, int * );
static void SetClosed( AstRegion *, int, int * );
static void SetMeshSize( AstRegion *, int, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );
static void XORCheck( AstCmpRegion *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif


/* Member functions. */
/* ================= */
int CmpRegionList( AstCmpRegion *this, int *nreg, AstRegion ***reg_list,
                   int *status ) {
/*
*+
*  Name:
*     astCmpRegionList

*  Purpose:
*     Decompose a CmpRegion into a sequence of simpler Regions.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "cmpregion.h"
*     int astCmpRegionList( AstCmpRegion *this, int *nreg,
*                           AstRegion ***reg_list, int *status )

*  Class Membership:
*     CmpRegion method.

*  Description:
*     This function decomposes a CmpRegion into a sequence of simpler
*     Regions which may be applied in sequence to achieve the same
*     effect.

*  Parameters:
*     this
*        Pointer to the CmpRegion to be decomposed (the CmpRegion is not
*        actually modified by this function).
*     nreg
*        The address of an int which holds a count of the number of
*        individual Regions in the decomposition. On entry, this
*        should count the number of Regions already in the
*        "*reg_list" array (below). On exit, it is updated to include
*        any new Regions appended by this function.
*     reg_list
*        Address of a pointer to an array of Region pointers. On
*        entry, this array pointer should either be NULL (if no
*        Regions have yet been obtained) or should point at a
*        dynamically allocated array containing Region pointers
*        ("*nreg" in number) which have been obtained from a previous
*        invocation of this function.
*
*        On exit, the dynamic array will be enlarged to contain any
*        new Region pointers that result from the decomposition
*        requested. These pointers will be appended to any previously
*        present, and the array pointer will be updated as necessary
*        to refer to the enlarged array (any space released by the
*        original array will be freed automatically).
*
*        The new Region pointers returned will identify a sequence of
*        Region which, when applied in order, will represent an area
*        equivalent to that of the original Region.
*
*        All the Region pointers returned by this function should be
*        annulled by the caller, using astAnnul, when no longer
*        required. The dynamic array holding these pointers should
*        also be freed, using astFree.

*  Returned Value:
*     An integer identifying the boolean operation that should be used to
*     combine the Regions returned in "reg_list". This will be AST__AND
*     or AST__OR.

*-
*/

/* Local Variables: */
   AstCmpRegion *cmpreg;
   int add;
   int result;

/* Check the global error status. */
   if ( !astOK ) return AST__AND;

/* Check if this CmpRegion has an equivalent XOR representation. Is so,
   store details of the XOR representation in the CmpRegion. */
   XORCheck( this, status );

/* The CmpRegion class only has full support for AND and OR operators.
   However, it can also represent XOR operators, but it does this by
   an equivalent set of AND and OR operators. When an XOR CmpRegion is
   created, the original supplied argument regions are stored in
   "this->xor1" and "this->xor2", and the component Regions placed in the
   new CmpRegion are actually CmpRegions that implement the equivalent
   of an XOR operation, using AND and OR operators. We want to hide this
   to the outside world, so if the supplied CmpRegion represents an XOR
   operation, add the XOR regions to the returned list, and return an
   XOR operator. */
   if( this->xor1 ) {
      *reg_list = astGrow( *reg_list, *nreg + 2, sizeof( AstRegion * ) );
      if( astOK ) {
         ( *reg_list )[ (*nreg)++ ] = astClone( this->xor1 );
         ( *reg_list )[ (*nreg)++ ] = astClone( this->xor2 );
      }
      result = AST__XOR;

/* For AND and OR operators, we deal with the component Regions directly. */
   } else {

/* If the first component of the supplied CmpRegion is itself a CmpRegion
   that uses the same boolean operator as "this", call this function
   recursively to add its component Regions to the returned list. */
      add = 1;
      if( astIsACmpRegion( this->region1 ) ) {
         cmpreg = (AstCmpRegion *) this->region1;
         if( cmpreg->oper == this->oper ) {
            (void) CmpRegionList( cmpreg, nreg, reg_list, status );
            add = 0;
         }
      }

/* Otherwise, add the component Region directly into the returned list of
   Regions. */
      if( add ) {
         *reg_list = astGrow( *reg_list, *nreg + 1, sizeof( AstRegion * ) );
         if( astOK ) {
            ( *reg_list )[ *nreg ] = astClone( this->region1 );
            ( *nreg )++;
         }
      }

/* Do the same for the second component region */
      add = 1;
      if( astIsACmpRegion( this->region2 ) ) {
         cmpreg = (AstCmpRegion *) this->region2;
         if( cmpreg->oper == this->oper ) {
            (void) CmpRegionList( cmpreg, nreg, reg_list, status );
            add = 0;
         }
      }

      if( add ) {
         *reg_list = astGrow( *reg_list, *nreg + 1, sizeof( AstRegion * ) );
         if( astOK ) {
            ( *reg_list )[ *nreg ] = astClone( this->region2 );
            ( *nreg )++;
         }
      }

      result = this->oper;
   }

/* Return the boolean operator used to combine the regions in the
   returned array. */
   return result;
}

static void Decompose( AstMapping *this_mapping, AstMapping **map1,
                       AstMapping **map2, int *series, int *invert1,
                       int *invert2, int *status ) {
/*
*
*  Name:
*     Decompose

*  Purpose:
*     Decompose a CmpRegion into two component Regions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void Decompose( AstMapping *this, AstMapping **map1,
*                     AstMapping **map2, int *series,
*                     int *invert1, int *invert2, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the protected astDecompose
*     method inherited from the Mapping class).

*  Description:
*     This function returns pointers to two Mappings which, when applied
*     either in series or parallel, are equivalent to the supplied Mapping.
*
*     Since the Frame class inherits from the Mapping class, Frames can
*     be considered as special types of Mappings and so this method can
*     be used to decompose either CmpMaps, CmpFrames, CmpRegions or Prisms.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     map1
*        Address of a location to receive a pointer to first component
*        Mapping.
*     map2
*        Address of a location to receive a pointer to second component
*        Mapping.
*     series
*        Address of a location to receive a value indicating if the
*        component Mappings are applied in series or parallel. A non-zero
*        value means that the supplied Mapping is equivalent to applying map1
*        followed by map2 in series. A zero value means that the supplied
*        Mapping is equivalent to applying map1 to the lower numbered axes
*        and map2 to the higher numbered axes, in parallel.
*     invert1
*        The value of the Invert attribute to be used with map1.
*     invert2
*        The value of the Invert attribute to be used with map2.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Any changes made to the component rames using the returned
*     pointers will be reflected in the supplied CmpFrame.

*-
*/


/* Local Variables: */
   AstCmpRegion *this;              /* Pointer to CmpRegion structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpRegion *) this_mapping;

/* The components Frames of a CmpRegion are considered to be series
   Mappings. */
   if( series ) *series = 1;

/* The Frames are returned in their original order whether or not the
   CmpRegion has been inverted. */
   if( map1 ) *map1 = astClone( this->region1 );
   if( map2 ) *map2 = astClone( this->region2 );

/* The invert flags dont mean anything for a Region, but we return them
   anyway. If the CmpRegion has been inverted, return inverted Invert flags. */
   if( astGetInvert( this ) ) {
      if( invert1 ) *invert1 = astGetInvert( this->region1 ) ? 0 : 1;
      if( invert2 ) *invert2 = astGetInvert( this->region2 ) ? 0 : 1;

/* If the CmpRegion has not been inverted, return the current Invert flags. */
   } else {
      if( invert1 ) *invert1 = astGetInvert( this->region1 );
      if( invert2 ) *invert2 = astGetInvert( this->region2 );
   }
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two Objects are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     int Equal( AstObject *this_object, AstObject *that_object, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astEqual protected
*     method inherited from the Region class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two CmpRegions are equivalent.

*  Parameters:
*     this
*        Pointer to the first CmpRegion.
*     that
*        Pointer to the second CmpRegion.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the CmpRegions are equivalent, zero otherwise.

*  Notes:
*     - The CmpRegions are equivalent if their component Regions are
*     equivalent and if they have the same boolean operation, negation
*     and closed flags.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpRegion *that;
   AstCmpRegion *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the Equal method inherited from the parent Region class. This checks
   that the Objects are both of the same class, and have the same Negated
   and Closed flags (amongst other things). */
   if( (*parent_equal)( this_object, that_object, status ) ) {

/* Obtain pointers to the two CmpRegion structures. */
      this = (AstCmpRegion *) this_object;
      that = (AstCmpRegion *) that_object;

/* Test their first component Regions for equality. */
      if( astEqual( this->region1, that->region1 ) ) {

/* Test their second component Regions for equality. */
         if( astEqual( this->region2, that->region2 ) ) {

/* Test their boolean operator for equality. */
            if( this->oper == that->oper ) result = 1;
         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

/*
*  Name:
*     MAKE_SET

*  Purpose:
*     Define a function to set an attribute value for a CmpRegion.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "cmpregion.h"
*     MAKE_SET(attribute,lattribute,type)

*  Class Membership:
*     Defined by the CmpRegion class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Set<Attribute>( AstRegion *this, <Type> value )
*
*     that sets the value of a specified Region attribute in the parent
*     Region structure and also in the component Regions.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     lattribute
*        Name of the attribute, all in lower case.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_SET(attribute,lattribute,type) \
static void Set##attribute( AstRegion *this_region, type value, int *status ) { \
\
/* Local Variables: */ \
   AstCmpRegion *this;         /* Pointer to the CmpRegion structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Use the parent method to set the value in the parent Region structure. */ \
   (*parent_set##lattribute)( this_region, value, status ); \
\
/* Also set the value in the two component Regions. */ \
   this = (AstCmpRegion *) this_region; \
   astSet##attribute( this->region1, value ); \
   astSet##attribute( this->region2, value ); \
}

/* Use the above macro to create accessors for the MeshSize and Closed attributes. */
MAKE_SET(MeshSize,meshsize,int)
MAKE_SET(Closed,closed,int)

/* Undefine the macro. */
#undef MAKE_SET

/*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Define a function to clear an attribute value for a CmpRegion.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "cmpregion.h"
*     MAKE_CLEAR(attribute,lattribute)

*  Class Membership:
*     Defined by the CmpRegion class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Clear<Attribute>( AstRegion *this )
*
*     that sets the value of a specified Region attribute in the parent
*     Region structure and also in the component Regions.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     lattribute
*        Name of the attribute, all in lower case.
*/

/* Define the macro. */
#define MAKE_CLEAR(attribute,lattribute) \
static void Clear##attribute( AstRegion *this_region, int *status ) { \
\
/* Local Variables: */ \
   AstCmpRegion *this;         /* Pointer to the CmpRegion structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Use the parent method to clear the value in the parent Region structure. */ \
   (*parent_clear##lattribute)( this_region, status ); \
\
/* Also clear the value in the two component Regions. */ \
   this = (AstCmpRegion *) this_region; \
   astClear##attribute( this->region1 ); \
   astClear##attribute( this->region2 ); \
}

/* Use the above macro to create accessors for the MeshSize and Closed attributes. */
MAKE_CLEAR(MeshSize,meshsize)
MAKE_CLEAR(Closed,closed)

/* Undefine the macro. */
#undef MAKE_CLEAR

static int GetBounded( AstRegion *this_region, int *status ) {
/*
*  Name:
*     GetBounded

*  Purpose:
*     Is the Region bounded?

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     int GetBounded( AstRegion *this, int *status )

*  Class Membership:
*     CmpRegion method (over-rides the astGetBounded method inherited from
*     the Region class).

*  Description:
*     This function returns a flag indicating if the Region is bounded.
*     The implementation provided by the base Region class is suitable
*     for Region sub-classes representing the inside of a single closed
*     curve (e.g. Circle, Ellipse, Box, etc). Other sub-classes (such as
*     CmpRegion, PointList, etc ) may need to provide their own
*     implementations.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the Region is bounded. Zero otherwise.

*/

/* Local Variables: */
   AstCmpRegion *this;        /* Pointer to CmpRegion structure */
   AstRegion *reg1;           /* Pointer to first component Region */
   AstRegion *reg2;           /* Pointer to second component Region */
   int neg1;                  /* Negated flag to use with first component */
   int neg2;                  /* Negated flag to use with second component */
   int oper;                  /* Combination operator */
   int overlap;               /* Nature of overlap between components */
   int reg1b;                 /* Is the first component Region bounded?*/
   int reg2b;                 /* Is the second component Region bounded?*/
   int result;                /* Returned result */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Only calculated a new value if there is no cached value in the Region. */
   if( this->bounded == -INT_MAX ) {

/* Get the component Regions, how they should be combined, and the
   Negated values which should be used with them. The returned values
   take account of whether the supplied CmpRegion has itself been Negated
   or not. The returned Regions represent regions within the base Frame
   of the FrameSet encapsulated by the parent Region structure. */
      GetRegions( this, &reg1, &reg2, &oper, &neg1, &neg2, status );

/* If the first component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg1" in place of "reg1"
   itself. */
      if( neg1 != astGetNegated( reg1 ) ) {
         AstRegion *tmp = astGetNegation( reg1 );
         (void) astAnnul( reg1 );
         reg1 = tmp;
      }

/* If the second component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg2" in place of "reg2"
   itself. */
      if( neg2 != astGetNegated( reg2 ) ) {
         AstRegion *tmp = astGetNegation( reg2 );
         (void) astAnnul( reg2 );
         reg2 = tmp;
      }

/* See if either of the component Regions is bounded. */
      reg1b = astGetBounded( reg1 );
      reg2b = astGetBounded( reg2 );

/* If the regions are ANDed... */
      if( oper == AST__AND ) {

/* If either one of the two components are bounded, then the AND region is
   bounded. */
         if( reg1b || reg2b ) {
            result = 1;

/* If neither of the two components is bounded, then the AND region is
   unbounded if there is partial or no overlap between them and is bounded
   otherwise. */
         } else {
            overlap = astOverlap( reg1, reg2 );
            if( overlap == 1 || overlap == 4 || overlap == 6 ) {
               result = 0;
            } else {
               result = 1;
            }
         }

/* If the regions are ORed... */
      } else {

/* If either one of the two components is unbounded, then the OR region is
   unbounded. */
         if( !reg1b || !reg2b ) {
            result = 0;

/* If both of the two components are bounded, then the OR region is also
   bounded. */
         } else {
            result = 1;
         }
      }

/* Free resources. */
      reg1 = astAnnul( reg1 );
      reg2 = astAnnul( reg2 );

/* Cache the value in the CmpRegion. */
      this->bounded = astOK ? result : -INT_MAX;
   }

/* Return zero if an error occurred. Otherwise, return the cached value. */
   if( astOK ) {
      result = ( this->bounded == -INT_MAX ) ? 0 : this->bounded;
   } else {
      result = 0;
   }

/* Return the required pointer. */
   return result;
}

static double GetFillFactor( AstRegion *this_region, int *status ) {
/*
*  Name:
*     GetFillFactor

*  Purpose:
*     Obtain the value of the FillFactor attribute for a CmpRegion.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     double GetFillFactor( AstRegion *this, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astGetFillFactor method inherited
*     from the Region class).

*  Description:
*     This function returns the value of the FillFactor attribute for a
*     CmpRegion.  A suitable default value is returned if no value has
*     previously been set.

*  Parameters:
*     this
*        Pointer to the CmpRegion.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The FillFactor value to use.

*/

/* Local Variables: */
   AstCmpRegion *this;
   double result;

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Initialise. */
   result = AST__BAD;

/* Obtain a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* See if a FillFactor value has been set. If so, use the parent
   astGetFillFactor  method to obtain it. */
   if ( astTestFillFactor( this ) ) {
      result = (*parent_getfillfactor)( this_region, status );

/* Otherwise, we will generate a default value equal to the FillFactor values
   of the first component Region. */
   } else {
      result = astGetFillFactor( this->region1 );
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
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
*     #include "cmpregion.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied CmpRegion,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the CmpRegion.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpRegion *this;        /* Pointer to CmpRegion structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the CmpRegion structure. */
   this = (AstCmpRegion *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astGetObjSize( this->region1 );
   result += astGetObjSize( this->region2 );
   if( this->xor1 ) result += astGetObjSize( this->xor1 );
   if( this->xor2 ) result += astGetObjSize( this->xor2 );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void GetRegions( AstCmpRegion *this, AstRegion **reg1, AstRegion **reg2,
                        int *oper, int *neg1, int *neg2, int *status ) {
/*
*
*  Name:
*     GetRegions

*  Purpose:
*     Get the component Regions of a CmpRegion.

*  Type:
*     Private function.

*  Synopsis:
*     #include "region.h"
*     void GetRegions( AstCmpRegion *this, AstRegion **reg1, AstRegion **reg2,
*                      int *oper, int *neg1, int *neg2, int *status )

*  Class Membership:
*     CmpRegion member function

*  Description:
*     This function returns pointers to two Regions which, when applied
*     using the returned boolean operator, are equivalent to the supplied
*     Region. If the CmpRegion has been negated, then the returned operator
*     and "negated" flags will be set such that they represent the
*     negated CmpRegion.
*
*     The current Frames in both the returned component Regions will be
*     equivalent to the base Frame in the FrameSet encapsulated by the
*     parent Region structure.

*  Parameters:
*     this
*        Pointer to the CmpRegion.
*     reg1
*        Address of a location to receive a pointer to first component
*        Region. The current Frame in this region will be equivalent to
*        the base Frame in the FrameSet
*     reg2
*        Address of a location to receive a pointer to second component
*        Region.
*     oper
*        Address of a location to receive a value indicating how the
*        component Regions are combined together. This will be one of
*        AST__AND or AST__OR
*     neg1
*        The value of the Negated attribute to be used with reg1.
*     neg2
*        The value of the Negated attribute to be used with reg2.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Any changes made to the component Regions using the returned
*     pointers will be reflected in the supplied CmpRegion.

*-
*/

/* Initialise */
   if( reg1 ) *reg1 = NULL;
   if( reg2 ) *reg2 = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Return the component Region pointers. */
   if( reg1 ) *reg1 = astClone( this->region1 );
   if( reg2 ) *reg2 = astClone( this->region2 );

/* Initialise the other returned items. Note, the CmpRegion initialiser
   stored a deep copy of the supplied component Regions, and so we do not
   need to worry about attributes of the components having been changed
   after the creation of the CmpRegion. This is different to the CmpMap
   class which merely clones its supplied component pointers and so has
   to save copies of the original Invert settings within the CmpMap
   structure. */
   if( oper ) *oper = this->oper;
   if( neg1 ) *neg1 = astGetNegated( this->region1 );
   if( neg2 ) *neg2 = astGetNegated( this->region2 );

/* If the CmpRegion has been inverted, we modify the boolean operator and
   negation flags so that they reflect the inverted CmpRegion. */
   if( astGetNegated( this ) ) {

/* If the component Regions are combined using AND, then the negated
   CmpRegion combines its negated components using  OR. */
      if( this->oper == AST__AND ){
         if( oper ) *oper = AST__OR;
         if( neg1 ) *neg1 = *neg1 ? 0 : 1;
         if( neg2 ) *neg2 = *neg2 ? 0 : 1;

/* If the component Regions are combined using OR, then the negated CmpRegion
   combines its negated components using  AND. */
      } else if( this->oper == AST__OR ){
         if( oper ) *oper = AST__AND;
         if( neg1 ) *neg1 = *neg1 ? 0 : 1;
         if( neg2 ) *neg2 = *neg2 ? 0 : 1;

      } else if( astOK ) {
         astError( AST__INTER, "GetRegions(%s): The %s refers to an unknown "
                   "boolean operator with identifier %d (internal AST "
                   "programming error).", status, astGetClass( this ),
                   astGetClass( this ), this->oper );
      }
   }
}

static AstRegion *GetDefUnc( AstRegion *this_region, int *status ) {
/*
*  Name:
*     GetDefUnc

*  Purpose:
*     Obtain a pointer to the default uncertainty Region for a given Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstRegion *GetDefUnc( AstRegion *this )

*  Class Membership:
*     CmpRegion method (over-rides the astGetDefUnc method inherited from
*     the Region class).

*     This function returns a pointer to a Region which represents the
*     default uncertainty associated with a position on the boundary of the
*     given  Region. The returned Region refers to the base Frame within the
*     FrameSet encapsulated by the supplied Region.

*  Parameters:
*     this
*        Pointer to the Region.

*  Returned Value:
*     A pointer to the Region. This should be annulled (using astAnnul)
*     when no longer needed.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstCmpRegion *this;        /* Pointer to CmpRegion structure */
   AstRegion *result;         /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* If the first component region has non-default uncertainty, use it as
   the default uncertainty for the CmpRegion. Note, the current Frame of
   an uncertainty Region is assumed to be the same as the base Frame in the
   CmpRegion. */
   if( astTestUnc( this->region1 ) ) {
      result = astGetUncFrm( this->region1, AST__CURRENT );

/* Otherwise, if the second component region has non-default uncertainty,
   use it as the default uncertainty for the CmpRegion. */
   } else if( astTestUnc( this->region2 ) ) {
      result = astGetUncFrm( this->region2, AST__CURRENT );

/* Otherwise, use the parent method to determine the default uncertainty. */
   } else {
      result = (* parent_getdefunc)( this_region, status );
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the required pointer. */
   return result;
}

void astInitCmpRegionVtab_(  AstCmpRegionVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitCmpRegionVtab

*  Purpose:
*     Initialise a virtual function table for a CmpRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpregion.h"
*     void astInitCmpRegionVtab( AstCmpRegionVtab *vtab, const char *name )

*  Class Membership:
*     CmpRegion vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the CmpRegion class.

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
   AstRegionVtab *region;        /* Pointer to Region component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;


/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitRegionVtab( (AstRegionVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsACmpRegion) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */

   vtab->CmpRegionList = CmpRegionList;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   region = (AstRegionVtab *) vtab;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_simplify = mapping->Simplify;
   mapping->Simplify = Simplify;

   parent_getdefunc = region->GetDefUnc;
   region->GetDefUnc = GetDefUnc;

   parent_setregfs = region->SetRegFS;
   region->SetRegFS = SetRegFS;

   parent_resetcache = region->ResetCache;
   region->ResetCache = ResetCache;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

   parent_clearclosed = region->ClearClosed;
   region->ClearClosed = ClearClosed;

   parent_clearmeshsize = region->ClearMeshSize;
   region->ClearMeshSize = ClearMeshSize;

   parent_setclosed = region->SetClosed;
   region->SetClosed = SetClosed;

   parent_setmeshsize = region->SetMeshSize;
   region->SetMeshSize = SetMeshSize;

   parent_getfillfactor = region->GetFillFactor;
   region->GetFillFactor = GetFillFactor;

   parent_regsetattrib = region->RegSetAttrib;
   region->RegSetAttrib = RegSetAttrib;

   parent_regclearattrib = region->RegClearAttrib;
   region->RegClearAttrib = RegClearAttrib;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->Decompose = Decompose;
   region->RegBaseBox = RegBaseBox;
   region->RegBaseBox2 = RegBaseBox2;
   region->RegBaseMesh = RegBaseMesh;
   region->RegSplit = RegSplit;
   region->RegPins = RegPins;
   region->RegTrace = RegTrace;
   region->GetBounded = GetBounded;
   region->RegBasePick = RegBasePick;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "CmpRegion", "Combination of two Regions" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstCmpRegion *this;       /* Pointer to CmpRegion structure */
   int result;               /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the CmpRegion structure. */
   this = (AstCmpRegion *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->region1, mode, extra, fail );
   if( !result ) result = astManageLock( this->region2, mode, extra, fail );

   return result;

}
#endif

static AstRegion *MatchRegion( AstRegion *this, int ifrm, AstRegion *that,
                               const char *method, int *status ) {
/*
*  Name:
*     MatchRegion

*  Purpose:
*     Map a Region into the Frame of another Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstRegion *MatchRegion( AstRegion *this, int ifrm, AstRegion *that,
*                             const char *method, int *status )

*  Class Membership:
*     CmpRegion method.

*  Description:
*     This function returns a pointer to a new Region which is a copy of
*     "that" mapped into either the base or current Frame of "this".

*  Parameters:
*     this
*        Pointer to a Region defining the Frame of the returned Region.
*     ifrm
*        The index of a Frame within the FrameSet encapsulated by "this".
*        The returned Region will refer to the requested Frame. It should
*        be either AST__CURRENT or AST__BASE.
*     that
*        Pointer to a Region defining the shape and extent of the
*        returned Region.
*     method
*        Pointer to a string holding the calling method.This is only used
*        in error messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a new Region. This should be annulled (using astAnnul)
*     when no longer needed.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *frm;             /* Current Frame from "fs" */
   AstFrameSet *fs;           /* FrameSet connecting that to this */
   AstMapping *map;           /* Base->Current Mapping from "fs" */
   AstRegion *result;         /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. Also return NULL if no Regions were
   supplied. */
   if ( !astOK || !this || !that ) return result;

/* Temporarily invert "this" if we are matching its base Frame (since the
   astConvert method matches current Frames). */
   if( ifrm == AST__BASE ) astInvert( this );

/* Find a FrameSet connecting the current Frames of the two Regions */
   fs = astConvert( that, this, "" );

/* Re-instate the original Frame indices in "this" if required. */
   if( ifrm == AST__BASE ) astInvert( this );

/* Check a conversion path was found. */
   if( fs ) {

/* Get the Frame and Mapping form the FrameSet. */
      frm = astGetFrame( fs, AST__CURRENT );
      map = astGetMapping( fs, AST__BASE, AST__CURRENT );

/* Re-map the Region. */
      result = astMapRegion( that, map, frm );

/* Free resources. */
      frm = astAnnul( frm );
      map = astAnnul( map );
      fs = astAnnul( fs );

/* Report an error if there is no conversion between the two Frames. */
   } else {
      astError( AST__INTER, "%s(%s): MatchRegion cannot convert between "
                "the two supplied coordinate Frames (internal AST "
                "programming error).", status, method, astGetClass( this ) );
   }

/* Annul the returned pointer if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static void RegBaseBox( AstRegion *this_region, double *lbnd, double *ubnd, int *status ){
/*
*  Name:
*     RegBaseBox

*  Purpose:
*     Returns the bounding box of an un-negated Region in the base Frame of
*     the encapsulated FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegBaseBox protected
*     method inherited from the Region class).

*  Description:
*     This function returns the upper and lower axis bounds of a Region in
*     the base Frame of the encapsulated FrameSet, assuming the Region
*     has not been negated. That is, the value of the Negated attribute
*     is ignored.

*  Parameters:
*     this
*        Pointer to the Region.
*     lbnd
*        Pointer to an array in which to return the lower axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     ubnd
*        Pointer to an array in which to return the upper axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstCmpRegion *this;          /* Pointer to CmpRegion structure */
   AstPointSet *ps;             /* Mesh pointset */
   AstRegion *reg1;             /* Pointer to first component Region */
   AstRegion *reg2;             /* Pointer to second component Region */
   double **ptr;                /* Pointer to mesh data */
   double *clbnd1;              /* Point to 1st comp lower bounds array */
   double *clbnd2;              /* Point to 2nd comp lower bounds array */
   double *cubnd1;              /* Point to 1st comp upper bounds array */
   double *cubnd2;              /* Point to 2nd comp upper bounds array */
   double *p;                   /* Pointer to next coordinate value */
   double lb;                   /* Lower limit */
   double ub;                   /* Upper limit */
   int i;                       /* Axis index */
   int icoord;                  /* Coordinate index */
   int inc1;                    /* First component interval is included? */
   int inc2;                    /* Second component interval is included? */
   int ipoint;                  /* Point index */
   int nax;                     /* Number of axes in Frame */
   int ncoord;                  /* Number of coords */
   int neg1;                    /* First component negated? */
   int neg2;                    /* Second component negated? */
   int npoint;                  /* Number of points */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the CmpRegion structure */
   this = (AstCmpRegion *) this_region;

/* If the CmpRegion is bounded, we find the bounding box using a mesh of
   points spread evenly over the boundary of the CmpRegion. */
   if( astGetBounded( this ) ) {
      ps = astRegBaseMesh( this_region );
      ptr = astGetPoints( ps );
      ncoord = astGetNcoord( ps );
      npoint = astGetNpoint( ps );

      if( astOK ) {
         for( icoord = 0; icoord < ncoord; icoord++ ) {
            lbnd[ icoord ] = DBL_MAX;
            ubnd[ icoord ] = -DBL_MAX;
            p = ptr[ icoord ];
            for( ipoint = 0; ipoint < npoint; ipoint++, p++ ) {
               if( *p != AST__BAD ) {
                  if( *p < lbnd[ icoord ] )  lbnd[ icoord ] = *p;
                  if( *p > ubnd[ icoord ] )  ubnd[ icoord ] = *p;
               }
            }
         }
      }
      ps = astAnnul( ps );

/* If the CmpRegion is not bounded we look at each axis individually. */
   } else {

/* Get pointers to the component Regions. */
      reg1 = this->region1;
      reg2 = this->region2;

/* Get their negated flags */
      neg1 = astGetNegated( reg1 );
      neg2 = astGetNegated( reg2 );

/* The base Frame of the parent Region structure is the current Frame of
   the component Regions. Get the no. of axes in this Frame. */
      nax = astGetNaxes( reg1 );

/* Get the bounding boxes of the component Regions in this Frame. */
      clbnd1 = astMalloc( sizeof( double )*(size_t) nax );
      cubnd1 = astMalloc( sizeof( double )*(size_t) nax );
      clbnd2 = astMalloc( sizeof( double )*(size_t) nax );
      cubnd2 = astMalloc( sizeof( double )*(size_t) nax );
      if( astOK ) {
         astGetRegionBounds( reg1, clbnd1, cubnd1 );
         astGetRegionBounds( reg2, clbnd2, cubnd2 );

/* Loop round every axis. */
         for( i = 0; i < nax; i++ ) {

/* If the first component Region has been negated, the lower and upper
   bounds from the first component are the bounds of an *excluded* axis
   interval, not an included interval. If either of the bounds are
   infinite, we can swap it to an included interval. If both bounds are
   finite, we cannot convert to an included interval. In this case, we
   assume that the gap will be filled at some point on another axis, if
   there is more than 1 axis, and convert it to an unbouded included
   interval. */
            inc1 = 1;
            if( neg1 ) {
               lb = clbnd1[ i ];
               ub = cubnd1[ i ];
               if( lb == -DBL_MAX ) clbnd1[ i ] = ub;
               if( ub == DBL_MAX ) cubnd1[ i ] = lb;
               if( lb != -DBL_MAX && ub != DBL_MAX ) {
                  if( nax == 1 ) {
                     inc1 = 0;
                  } else {
                     clbnd1[ i ] = -DBL_MAX;
                     cubnd1[ i ] = DBL_MAX;
                  }
               }
            }

/* Likewise attempt to convert an excluded interval into an included
   interval for the second component Region. */
            inc2 = 1;
            if( neg2 ) {
               lb = clbnd2[ i ];
               ub = cubnd2[ i ];
               if( lb == -DBL_MAX ) clbnd2[ i ] = ub;
               if( ub == DBL_MAX ) cubnd2[ i ] = lb;
               if( lb != -DBL_MAX && ub != DBL_MAX ) {
                  if( nax == 1 ) {
                     inc2 = 0;
                  } else {
                     clbnd2[ i ] = -DBL_MAX;
                     cubnd2[ i ] = DBL_MAX;
                  }
               }
            }

/* If the component Regions are combined using AND, find the overlap of
   the axis intervals. This depends on whether the intervals are included
   or excluded. */
            if( this->oper == AST__AND ) {

               if( inc1 ) {
                  if( inc2 ) {
                     lbnd[ i ] = astMAX( clbnd1[ i ], clbnd2[ i ] );
                     ubnd[ i ] = astMIN( cubnd1[ i ], cubnd2[ i ] );
                  } else {
                     lbnd[ i ] = clbnd1[ i ] < clbnd2[ i ] ? clbnd1[ i ] : cubnd2[ i ];
                     ubnd[ i ] = cubnd1[ i ] > cubnd2[ i ] ? cubnd1[ i ] : clbnd2[ i ];
                  }
               } else {
                  if( inc2 ) {
                     lbnd[ i ] = clbnd2[ i ] < clbnd1[ i ] ? clbnd2[ i ] : cubnd1[ i ];
                     ubnd[ i ] = cubnd2[ i ] > cubnd1[ i ] ? cubnd2[ i ] : clbnd1[ i ];
                  } else {
                     lbnd[ i ] = clbnd1[ i ] < clbnd2[ i ] ? clbnd1[ i ] : cubnd2[ i ];
                     ubnd[ i ] = cubnd1[ i ] > cubnd2[ i ] ? cubnd1[ i ] : clbnd2[ i ];
                  }
               }

/* If the component Regions are not combined using AND, find the union of
   the axis intervals. */
            } else {
               if( inc1 && inc2 ) {
                  lbnd[ i ] = astMIN( clbnd1[ i ], clbnd2[ i ] );
                  ubnd[ i ] = astMAX( cubnd1[ i ], cubnd2[ i ] );
               } else {
                  lbnd[ i ] = -DBL_MAX;
                  ubnd[ i ] = DBL_MAX;
               }
            }
         }
      }

/* Free resources. */
      clbnd1 = astFree( clbnd1 );
      cubnd1 = astFree( cubnd1 );
      clbnd2 = astFree( clbnd2 );
      cubnd2 = astFree( cubnd2 );
   }
}

static void RegBaseBox2( AstRegion *this_region, double *lbnd, double *ubnd, int *status ){
/*
*  Name:
*     RegBaseBox2

*  Purpose:
*     Returns the bounding box of an un-negated Region in the base Frame of
*     the encapsulated FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void RegBaseBox2( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegBaseBox2 protected
*     method inherited from the Region class).

*  Description:
*     This function is similar to astRegBaseBox in that it returns the
*     upper and lower axis bounds of a Region in the base Frame of the
*     encapsulated FrameSet. But, in addition to assuming that the
*     supplied Region has not been negated, it also assumes that any
*     component Regions contained within the supplied Region have not been
*     negated.

*  Parameters:
*     this
*        Pointer to the Region.
*     lbnd
*        Pointer to an array in which to return the lower axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     ubnd
*        Pointer to an array in which to return the upper axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstCmpRegion *this;          /* Pointer to CmpRegion structure */
   AstRegion *reg1;             /* Pointer to first component Region */
   AstRegion *reg2;             /* Pointer to second component Region */
   double *clbnd1;              /* Point to 1st comp lower bounds array */
   double *clbnd2;              /* Point to 2nd comp lower bounds array */
   double *cubnd1;              /* Point to 1st comp upper bounds array */
   double *cubnd2;              /* Point to 2nd comp upper bounds array */
   int i;                       /* Axis index */
   int nax;                     /* Number of axes in Frame */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the CmpRegion structure */
   this = (AstCmpRegion *) this_region;

/* Get pointers to the component Regions. */
   reg1 = this->region1;
   reg2 = this->region2;

/* The base Frame of the parent Region structure is the current Frame of
   the component Regions. Get the no. of axes in this Frame. */
   nax = astGetNaxes( reg1 );

/* Get the bounding boxes of the component Regions in this Frame. */
   clbnd1 = astMalloc( sizeof( double )*(size_t) nax );
   cubnd1 = astMalloc( sizeof( double )*(size_t) nax );
   clbnd2 = astMalloc( sizeof( double )*(size_t) nax );
   cubnd2 = astMalloc( sizeof( double )*(size_t) nax );
   if( astOK ) {
      astGetRegionBounds2( reg1, clbnd1, cubnd1 );
      astGetRegionBounds2( reg2, clbnd2, cubnd2 );

/* How we combine the two bounding boxes depends on the boolean operator
   associated with this CmpRegion.  For AND find the overlap of the two
   bounding boxes. For other operators find the union. */
      if( this->oper == AST__AND ) {
         for( i = 0; i < nax; i++ ) {
            lbnd[ i ]= astMAX( clbnd1[ i ], clbnd2[ i ] );
            ubnd[ i ]= astMIN( cubnd1[ i ], cubnd2[ i ] );
         }

      } else {
         for( i = 0; i < nax; i++ ) {
            lbnd[ i ]= astMIN( clbnd1[ i ], clbnd2[ i ] );
            ubnd[ i ]= astMAX( cubnd1[ i ], cubnd2[ i ] );
         }
      }
   }

/* Free resources. */
   clbnd1 = astFree( clbnd1 );
   cubnd1 = astFree( cubnd1 );
   clbnd2 = astFree( clbnd2 );
   cubnd2 = astFree( cubnd2 );

}

static AstPointSet *RegBaseMesh( AstRegion *this_region, int *status ){
/*
*  Name:
*     RegBaseMesh

*  Purpose:
*     Return a PointSet containing a mesh of points on the boundary of a
*     Region in its base Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegBaseMesh protected
*     method inherited from the Region class).

*  Description:
*     This function returns a PointSet containing a mesh of points on the
*     boundary of the Region. The points refer to the base Frame of
*     the encapsulated FrameSet.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the PointSet. Annul the pointer using astAnnul when it
*     is no longer needed.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*/


/* Local Variables: */
   AstCmpRegion *this;            /* The CmpRegion structure */
   AstPointSet *mesh1;            /* PointSet holding mesh for 1st component */
   AstPointSet *mesh1b;           /* Mesh for 1st component mapped by 2nd comp. */
   AstPointSet *mesh2;            /* PointSet holding mesh for 2nd component */
   AstPointSet *mesh2b;           /* Mesh for 2nd component mapped by 1st comp. */
   AstPointSet *result;           /* Returned pointer */
   AstRegion *reg1;               /* Pointer to first component Region */
   AstRegion *reg2;               /* Pointer to second component Region */
   double **ptr1;                 /* Pointer to array of mesh1b axis value pointers */
   double **ptr2;                 /* Pointer to array of mesh2b axis value pointers */
   double **ptr;                  /* Pointer to array of total axis value pointers */
   double *lbnd;                  /* Pointer to array of bounding box lower bounds */
   double *ubnd;                  /* Pointer to array of bounding box upper bounds */
   double v;                      /* Axis value */
   int hasMesh1;                  /* Does 1st component Region have a mesh? */
   int hasMesh2;                  /* Does 2nd component Region have a mesh? */
   int ic;                        /* Axis index */
   int ip;                        /* Input point index */
   int jp;                        /* Output point index */
   int nc;                        /* No. of axis values per point */
   int np1;                       /* No. of points in mesh1b */
   int np2;                       /* No. of points in mesh2b */
   int np;                        /* No. of points in returned PointSet */
   int ok;                        /* Were all axis values good at this point? */

/* Initialise */
   result= NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* If the Region structure contains a pointer to a PointSet holding
   a previously created mesh, return it. */
   if( this_region->basemesh ) {
      result = astClone( this_region->basemesh );

/* Otherwise, create a new mesh. */
   } else {

/* Get pointers to the component regions. */
      reg1 = this->region1;
      reg2 = this->region2;

/* A mesh can only be produced for a Region if it is bounded when either
   negated or un-negated. See if meshes can be produced for the component
   Regions. */
      hasMesh1 = astGetBounded( reg1 );
      if( !hasMesh1 ){
         astNegate( reg1 );
         hasMesh1 = astGetBounded( reg1 );
         astNegate( reg1 );
      }

      hasMesh2 = astGetBounded( reg2 );
      if( !hasMesh2 ){
         astNegate( reg2 );
         hasMesh2 = astGetBounded( reg2 );
         astNegate( reg2 );
      }

/* If neither Region has a mesh we cannot produce a mesh. */
      if( !hasMesh1 && !hasMesh2 && astOK ) {
         astError( AST__INTER, "astRegBaseMesh(%s): No mesh can be "
                   "produced for the %s bacause neither of its component "
                   "Regions has a mesh (internal AST programming error).", status,
                   astGetClass( this ), astGetClass( this ) );

/* If only one Region has a mesh, we can produce a mesh so long as the
    boolean operator is not OR. */
      } else if( ( !hasMesh1 || !hasMesh2 ) && this->oper == AST__OR && astOK ) {
         astError( AST__INTER, "astRegBaseMesh(%s): No mesh can be produced "
                   "for the %s bacause one its component Regions has no "
                   "mesh and the union of the Regions is required (internal "
                   "AST programming error).", status, astGetClass( this ), astGetClass( this ) );
      }

/* Allocate memory to hold a bounding box in the base Frame of the CmpRegion. */
      nc = astGetNin( this_region->frameset );
      lbnd = astMalloc( sizeof( double )*(size_t) nc );
      ubnd = astMalloc( sizeof( double )*(size_t) nc );

/* Get current Frame meshes covering the two component Regions (the current
   Frame of the component Regions is the same as the base Frame of the parent
   Region). We now know that at least one Region has a mesh. If the other
   one does not have a mesh we may be able to create a mesh by taking the
   intersection of the Region with the bounding box of the bounded Region. */
      if( hasMesh1 ) {
         mesh1 = astRegMesh( reg1 );
         if( hasMesh2 ) {
            mesh2 = astRegMesh( reg2 );
         } else {
            astGetRegionBounds( reg1, lbnd, ubnd );
            mesh2 = astBndMesh( reg2, lbnd, ubnd );
         }

      } else {
         mesh2 = astRegMesh( reg2 );
         astGetRegionBounds( reg2, lbnd, ubnd );
         mesh1 = astBndMesh( reg1, lbnd, ubnd );
      }

/* If the CmpRegion represents the intersection of the two component Regions
   (AND operator), the total mesh is the sum of the component mesh points
   which are inside the other component region. If the CmpRegion represents
   the union of the two component Regions (OR operator), the total mesh is
   the sum of the component mesh points which are outside the other component
   region. So temporarily negate the component Regions if they are
   combined using OR. */
      if( this->oper == AST__OR ) {
         astNegate( reg1 );
         astNegate( reg2 );
      }

/* Transform the mesh for the first component using the second component
   as a Mapping. Mesh points outside (or inside if "oper" is OR) the bounds
   of the second component will be set bad. */
      mesh1b = astTransform( reg2, mesh1, 1, NULL );

/* Transform the mesh for the second component using the first component
   as a Mapping. Mesh points outside (or inside if "oper" is OR) the bounds
   of the first component will be set bad. */
      mesh2b = astTransform( reg1, mesh2, 1, NULL );

/* If required, negate them again to bring them back to their original state.*/
      if( this->oper == AST__OR ) {
         astNegate( reg1 );
         astNegate( reg2 );
      }

/* The required mesh contains all the good points form both mesh1b and
   mesh2b (i.e. all boundary points which are inside -or inside if "oper"
   is OR- the other component Region). Create a PointSet assuming that all
   points are good. First allocate an array to hold pointers to the arrays
   holding coordinate values for each axis. */
      nc = astGetNcoord( mesh1b );
      np1 = astGetNpoint( mesh1b );
      np2 = astGetNpoint( mesh2b );
      np = np1 + np2;
      result = astPointSet( np, nc, "", status );
      ptr = astGetPoints( result );

/* Get points to the axis values of the mapped meshes. */
      ptr1 = astGetPoints( mesh1b );
      ptr2 = astGetPoints( mesh2b );

/* Check pointers can be used safely. */
      if( astOK ) {

/* Initialise the index of the next point in the total mesh. */
         jp = 0;

/* Loop round all the points in the transformed mesh for the first
   component. */
         for( ip = 0; ip < np1; ip++ ) {

/* Assume this point has good axis values */
            ok = 1;

/* Copy the axis values into the total mesh. Break if a bad axis value is
   found. */
            for( ic = 0; ic < nc; ic++ ) {
               v = ptr1[ ic ][ ip ];
               if( v != AST__BAD ) {
                  ptr[ ic ][ jp ] = v;
               } else {
                  ok = 0;
                  break;
               }
            }

/* If no bad axis values were found, increment the index of the next
   point in the total mesh. */
            if( ok ) jp++;
         }

/* Now similarly copy the good values from the second transformed mesh onto
   the end of the total mesh array. */
         for( ip = 0; ip < np2; ip++ ) {
            ok = 1;
            for( ic = 0; ic < nc; ic++ ) {
               v = ptr2[ ic ][ ip ];
               if( v != AST__BAD ) {
                  ptr[ ic ][ jp ] = v;
               } else {
                  ok = 0;
                  break;
               }
            }
            if( ok ) jp++;
         }

/* If the total mesh contains no good points, we will create a PointSet
   holding a single bad position. */
         if( jp == 0 ) {
            np = 1;
            for( ic = 0; ic < nc; ic++ ) ptr[ ic ][ 0 ] = AST__BAD;
         } else {
            np = jp;
         }

/* Adjust the size of the returned PointSet to exclude the extra space
   caused by any axis values being bad in the transformed meshes. */
         astSetNpoint( result, np );

      }

/* Free resources. */
      mesh1 = astAnnul( mesh1 );
      mesh2 = astAnnul( mesh2 );
      mesh1b = astAnnul( mesh1b );
      mesh2b = astAnnul( mesh2b );
      lbnd = astFree( lbnd );
      ubnd = astFree( ubnd );

/* Save the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
      if( astOK && result ) this_region->basemesh = astClone( result );
   }

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;
}

static AstRegion *RegBasePick( AstRegion *this_region, int naxes,
                               const int *axes, int *status ){
/*
*  Name:
*     RegBasePick

*  Purpose:
*     Return a Region formed by picking selected base Frame axes from the
*     supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstRegion *RegBasePick( AstRegion *this, int naxes, const int *axes,
*                             int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegBasePick protected
*     method inherited from the Region class).

*  Description:
*     This function attempts to return a Region that is spanned by selected
*     axes from the base Frame of the encapsulated FrameSet of the supplied
*     Region. This may or may not be possible, depending on the class of
*     Region. If it is not possible a NULL pointer is returned.

*  Parameters:
*     this
*        Pointer to the Region.
*     naxes
*        The number of base Frame axes to select.
*     axes
*        An array holding the zero-based indices of the base Frame axes
*        that are to be selected.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the Region, or NULL if no region can be formed.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.
*/

/* Local Variables: */
   AstCmpRegion *this;     /* Pointer to CmpRegion structure */
   AstFrame *frm1;         /* Axes picked from the 1st encapsulated Region */
   AstFrame *frm2;         /* Axes picked from the 2nd encapsulated Region */
   AstRegion *result;      /* Returned Region */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpRegion information. */
   this = (AstCmpRegion *) this_region;

/* Both encapsulated regions refer to the same Frame (the base Frame of
   the parent Region), so attempt to pick the requested axs from them.
   If the resulting Frames are not Regions, we cannot pick the requested
   axes so return the NULL Frame pointer initialised above. */
   frm1 = astPickAxes( this->region1, naxes, axes, NULL );
   if( astIsARegion( frm1 ) ) {
      frm2 = astPickAxes( this->region2, naxes, axes, NULL );
      if( astIsARegion( frm2 ) ) {

/* Create the new CmpRegion. */
         result = (AstRegion *) astCmpRegion( (AstRegion *) frm1,
                                              (AstRegion *) frm2,
                                              this->oper, "", status );
      }

/* Free resources */
      frm2 = astAnnul( frm2 );
   }
   frm1 = astAnnul( frm1 );

/* Return a NULL pointer if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int RegPins( AstRegion *this_region, AstPointSet *pset, AstRegion *unc,
                    int **mask, int *status ){
/*
*  Name:
*     RegPins

*  Purpose:
*     Check if a set of points fall on the boundary of a given CmpRegion.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given CmpRegion.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied CmpRegion "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the CmpRegion.
*     pset
*        Pointer to the PointSet. The points are assumed to refer to the
*        base Frame of the FrameSet encapsulated by "this".
*     unc
*        Pointer to a Region representing the uncertainties in the points
*        given by "pset". The Region is assumed to represent the base Frame
*        of the FrameSet encapsulated by "this". Zero uncertainity is assumed
*        if NULL is supplied.
*     mask
*        Pointer to location at which to return a pointer to a newly
*        allocated dynamic array of ints. The number of elements in this
*        array is equal to the value of the Npoint attribute of "pset".
*        Each element in the returned array is set to 1 if the
*        corresponding position in "pset" is on the boundary of the Region
*        and is set to zero otherwise. A NULL value may be supplied
*        in which case no array is created. If created, the array should
*        be freed using astFree when no longer needed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the points all fall on the boundary of the given
*     Region, to within the tolerance specified. Zero otherwise.

*/

/* Local variables: */
   AstCmpRegion *this;          /* Pointer to the CmpRegion structure. */
   AstPointSet *pset1;          /* Points masked by 1st component Region */
   AstPointSet *pset2;          /* Points masked by 2nd component Region */
   AstPointSet *psetb1;         /* Points in base Frame of 1st component Region */
   AstPointSet *psetb2;         /* Points in base Frame of 2nd component Region */
   AstRegion *reg1;             /* Pointer to first component Region */
   AstRegion *reg2;             /* Pointer to second component Region */
   AstRegion *unc1;             /* Base Frame uncertainty in 1st component Region */
   AstRegion *unc2;             /* Base Frame uncertainty in 2nd component Region */
   double **ptr1;               /* Pointer to axis values in "pset1" */
   double **ptr2;               /* Pointer to axis values in "pset2" */
   double *p1;                  /* Pointer to next axis zero value for pset1 */
   double *p2;                  /* Pointer to next axis zero value for pset2 */
   int *mask1;                  /* Mask for first component boundary */
   int *mask2;                  /* Mask for second component boundary */
   int ip;                      /* Point index */
   int np;                      /* Number of points */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Get pointers to the two component Regions. */
   reg1 = this->region1;
   reg2 = this->region2;

/* Get a mask which indicates if each supplied point is on or off the
   boundary of the first component Region. astRegPins expects its "pset"
   argument to contain positions in the base Frame of the Region, so
   we must first transform the supplied points into the base Frame of
   "reg1". We must also map the uncertainty into the base Frame of the
   component Region. */
   psetb1 = astRegTransform( reg1, pset, 0, NULL, NULL );
   unc1 = MatchRegion( reg1, AST__BASE, unc, "astRegPins", status );
   astRegPins( reg1, psetb1, unc1, &mask1 );

/* Likewise, get a mask which indicates if each supplied point is on or off
   the boundary of the second component Region. */
   psetb2 = astRegTransform( reg2, pset, 0, NULL, NULL );
   unc2 = MatchRegion( reg2, AST__BASE, unc, "astRegPins", status );
   astRegPins( reg2, psetb2, unc2, &mask2 );

/* The criteria for a point to be on the boundary of the CmpRegion depend
   on the boolean operator being used. If component regions A and B are
   ANDed together, then a point is on the boundary of the CmpRegion if
   either 1) it is on the boundary of A and inside B, or 2) it is on the
   boundary of B and inside A. If the component regions are ORed together,
   then a point is on the boundary of the CmpRegion if either 1) it is on
   the boundary of A and outside B, or 2) it is on the boundary of B and
   outside A. Either we need to transform the supplied PointSet using each
   component Region as a Mapping. But if using OR we temporarily negate
   the Regions. */
   if( this->oper == AST__OR ) {
      astNegate( reg1 );
      astNegate( reg2 );
   }
   pset1 = astTransform( reg1, pset, 1, NULL );
   pset2 = astTransform( reg2, pset, 1, NULL );
   if( this->oper == AST__OR ) {
      astNegate( reg1 );
      astNegate( reg2 );
   }

/* Get pointers to the axis values in these PointSets */
   ptr1 = astGetPoints( pset1 );
   ptr2 = astGetPoints( pset2 );

/* If required, create an output mask array */
   np = astGetNpoint( pset );
   if( mask ) *mask = astMalloc( sizeof(int)*(size_t) np );

/* Check pointers can be used safely. */
   if( astOK ) {

/* We can use the values for the first axis to indicate if a point is
   inside or outside a Region. So store pointers to the first axis arrays. */
      p1 = ptr1[ 0 ];
      p2 = ptr2[ 0 ];

/* Assume all points are on the boundary of the CmpRegion. */
      result = 1;

/* If we are creating an output mask, we must check every point. Otherwise
   we can stop checking when we find the first point which is not on the
   boundary of the CmpRegion. */
      if( mask ) {

         for( ip = 0; ip < np; ip++ ) {
            if( ( mask1[ ip ] && p2[ ip ] != AST__BAD ) ||
                ( mask2[ ip ] && p1[ ip ] != AST__BAD ) ){
               (*mask)[ ip ] = 1;
            } else {
               (*mask)[ ip ] = 0;
               result = 0;
            }
         }

      } else {

         for( ip = 0; ip < np; ip++ ) {
            if( ( !mask1[ ip ] || p2[ ip ] == AST__BAD ) &&
                ( !mask2[ ip ] || p1[ ip ] == AST__BAD ) ){
               result = 0;
               break;
            }
         }
      }
   }

/* Free resources */
   mask1 = astFree( mask1 );
   mask2 = astFree( mask2 );
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );
   psetb1 = astAnnul( psetb1 );
   psetb2 = astAnnul( psetb2 );
   if( unc1 ) unc1 = astAnnul( unc1 );
   if( unc2 ) unc2 = astAnnul( unc2 );

/* If an error has occurred, return zero. */
   if( !astOK ) {
      result = 0;
      if( mask ) *mask = astAnnul( *mask );
   }

/* Return the result. */
   return result;
}

static void RegSetAttrib( AstRegion *this_region, const char *setting,
                          char **base_setting, int *status ) {
/*
*  Name:
*     RegSetAttrib

*  Purpose:
*     Set an attribute value for a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void RegSetAttrib( AstRegion *this, const char *setting,
*                        char **base_setting, int *status )

*  Class Membership:
*     CmpRegion method (over-rides the astRegSetAttrib method inherited from
*     the Region class).

*  Description:
*     This function assigns an attribute value to both the base and
*     current Frame in the FrameSet encapsulated within a Region, without
*     remapping either Frame.
*
*     No error is reported if the attribute is not recognised by the base
*     Frame.

*  Parameters:
*     this
*        Pointer to the Region.
*     setting
*        Pointer to a null terminated attribute setting string. NOTE, IT
*        SHOULD BE ENTIRELY LOWER CASE. The supplied string will be
*        interpreted using the public interpretation implemented by
*        astSetAttrib. This can be different to the interpretation of the
*        protected accessor functions. For instance, the public
*        interpretation of an unqualified floating point value for the
*        Epoch attribute is to interpet the value as a gregorian year,
*        but the protected interpretation is to interpret the value as an
*        MJD.
*     base_setting
*        Address of a location at which to return a pointer to the null
*        terminated attribute setting string which was applied to the
*        base Frame of the encapsulated FrameSet. This may differ from
*        the supplied setting if the supplied setting contains an axis
*        index and the current->base Mapping in the FrameSet produces an
*        axis permutation. The returned pointer should be freed using
*        astFree when no longer needed. A NULL pointer may be supplied in
*        which case no pointer is returned.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstCmpRegion *this;
   char *bset;
   int rep;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Use the RegSetAttrib method inherited from the parent class to apply the
   setting to the current and base Frames in the FrameSet encapsulated by the
   parent Region structure. */
   (*parent_regsetattrib)( this_region, setting, &bset, status );

/* Now apply the base Frame setting to the component Regions (the current
   Frame within the component Regions is equivalent to the base Frame in the
   parent Region structure). Annul any "attribute unknown" error that results
   from attempting to do this. */
   if( astOK ) {
      rep = astReporting( 0 );
      astRegSetAttrib( this->region1, bset, NULL );
      astRegSetAttrib( this->region2, bset, NULL );
      if( astStatus == AST__BADAT ) astClearStatus;
      astReporting( rep );
   }

/* If required, return the base Frame setting string, otherwise free it. */
   if( base_setting ) {
      *base_setting = bset;
   } else {
      bset = astFree( bset );
   }
}

static AstRegion **RegSplit( AstRegion *this_region, int *nlist, int *status ){
/*
*+
*  Name:
*     RegSplit

*  Purpose:
*     Split a Region into a list of disjoint component Regions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "region.h"
*     AstRegion **astRegSplit( AstRegion *this, int *nlist )

*  Class Membership:
*     CmpRegion member function (overrides the astRegSplit method
*     inherited from the parent Region class).

*  Description:
*     This function splits the supplied Region into a set of disjoint
*     component Regions. If the Region cannot be split, then the returned
*     array contains only one pointer - a clone of the supplied Region
*     pointer.

*  Parameters:
*     this
*        Pointer to the Region.
*     nlist
*        Pointer to an int in which to return the number of elements in
*        the returned array.

*  Returned Value:
*     Pointer to dynamically alloctaed memory holding an array of Region
*     pointers. The length of this array is given by the value returned
*     in "*nlist". The pointers in the returned array should be annulled
*     using astAnnul when no longer needed, and the memory used to hold
*     the array should be freed using astFree.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.
*-
*/

/* Local Variables; */
   AstCmpRegion *new;
   AstCmpRegion *this;
   AstFrame *frm;
   AstFrameSet *fs;
   AstMapping *map;
   AstRegion **cmplist;
   AstRegion **result;
   AstRegion *cmpreg;
   AstRegion *new_reg;
   int icomp;
   int ifirst;
   int ilist;
   int iw;
   int jcomp;
   int ncomp;
   int nn;
   int unbounded;

/* Initialise. */
   result = NULL;
   *nlist = 0;

/* Check the local error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Indicate we have not yet found any unbounded component regions. */
   unbounded = 0;

/* Can only split non-inverted CmpRegions that combine their components
   using the OR operator. */
   if( this->oper == AST__OR && !astGetNegated( this->region1 ) &&
                                !astGetNegated( this->region2 ) ) {

/* Process each of the two component Regions in turn. */
      for( icomp = 0; icomp < 2 && !unbounded; icomp++ ) {
         cmpreg = icomp ? this->region2 : this->region1;

/* Create a set of disjoint Regions that are equivalent to the current
   component Region, and loop round them. */
         cmplist = astRegSplit( cmpreg, &ncomp );
         for( jcomp = 0; jcomp < ncomp; jcomp++ ) {

/* If any of the components are unbounds, we cannot split the supplied
   Region. */
            unbounded = unbounded || !astGetBounded( cmplist[ jcomp ] );
            if( ! unbounded ) {

/* Initialise the index within the returned list of the first Region that
   overlaps the current disjoint component Region. */
               ifirst = -1;

/* Loop round all the Regions currently in the returned list. */
               for( ilist = 0; ilist < *nlist; ilist++ ) {
                  if( result[ ilist ] ) {

/* See if the current disjoint component overlaps the current entry in
   the returned list. */
                     if( astOverlap( cmplist[ jcomp ], result[ ilist ] ) > 1 ) {

/* If this is the first overlap found for the current disjoint component,
   form a CmpRegion that combines the two overlapping Regions, and use it
   to replace the current entry in the returned list. */
                        if( ifirst == -1 ) {
                           new = astCmpRegion( cmplist[ jcomp ], result[ ilist ],
                                               AST__OR, " ", status );
                           (void) astAnnul( result[ ilist ] );
                           result[ ilist ] = (AstRegion *) new;

/* Note the index within the returned list of the first Region that overlaps
   the current disjoint component Region. */
                           ifirst = ilist;

/* If this is the second or later overlap, add the overlapping returned Region
   into the CmpRegion that it is stored at index "ifirsT" in the returned
   list. */
                        } else {
                           new = astCmpRegion( result[ ilist ], result[ ifirst ],
                                               AST__OR, " ", status );
                           result[ ilist ] = astAnnul( result[ ilist ] );
                           (void) astAnnul( result[ ifirst ] );
                           result[ ifirst ] = (AstRegion *) new;
                        }
                     }
                  }
               }

/* If the current disjoint component does not overlap any of the Regions
   already in the returned list, append the current disjoint component to
   the end of the returned list. */
               if( ifirst == -1 ) {
                  ilist = (*nlist)++;
                  result = astGrow( result, *nlist, sizeof( *result ) );
                  if( astOK ) result[ ilist ] = astClone( cmplist[ jcomp ] );
               }
            }

/* Annul the pointer to the disjoint component Region. */
            cmplist[ jcomp ] = astAnnul( cmplist[ jcomp ] );
         }

/* Free the mnemory holding the list of disjoint components. */
         cmplist = astFree( cmplist );
      }
   }

/* If any unbounded components were found, ensure the returned list is
   empty. */
   if( unbounded && result ) {
      for( ilist = 0; ilist < *nlist; ilist++ ) {
         if( result[ ilist ] ) result[ ilist ] = astAnnul( result[ ilist ] );
      }
      result = astFree( result );
      *nlist = 0;

/* Otherwise, shuffle later entries down to fill any NULL slots in the returned
   list. */
   } else if( result ){
      nn = *nlist;
      iw = 0;
      for( ilist = 0; ilist < nn; ilist++ ) {
         if( result[ ilist ] ) result[ iw++ ] = result[ ilist ];
      }
      *nlist = iw;
   }

/* If this CmpRegion cannot be split, the returned list just holds a
   clone of the Region pointer. */
   if( !result ) {
      result = astMalloc( sizeof( *result ) );
      if( astOK ) {
         result[ 0 ] = astClone( this );
         *nlist = 1;
      }
   }

/* Remap any returned Regions so that they are defined within the same
   coordinate system as the supplied Region. */
   if( result && *nlist > 0 )  {
      fs = this_region->frameset;
      map = astGetMapping( fs, AST__BASE, AST__CURRENT );
      frm = astGetFrame( fs, AST__CURRENT );
      for( ilist = 0; ilist < *nlist; ilist++ ) {
         new_reg = astMapRegion( result[ ilist ], map, frm );
         (void) astAnnul( result[ ilist ] );
         result[ ilist ] = new_reg;
      }
      map = astAnnul( map );
      frm = astAnnul( frm );
   }

/* Free all returned pointers if an error has occurred. */
   if( !astOK && result ) {
      for( ilist = 0; ilist < *nlist; ilist++ ) {
         result[ ilist ] = astAnnul( result[ ilist ] );
      }
      result = astFree( result );
      *nlist = 0;
   }

/* Return the result. */
   return result;
}

static int RegTrace( AstRegion *this_region, int n, double *dist, double **ptr,
                     int *status ){
/*
*+
*  Name:
*     RegTrace

*  Purpose:
*     Return requested positions on the boundary of a 2D Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     int astRegTrace( AstRegion *this, int n, double *dist, double **ptr );

*  Class Membership:
*     CmpRegion member function (overrides the astRegTrace method
*     inherited from the parent Region class).

*  Description:
*     This function returns positions on the boundary of the supplied
*     Region, if possible. The required positions are indicated by a
*     supplied list of scalar parameter values in the range zero to one.
*     Zero corresponds to some arbitrary starting point on the boundary,
*     and one corresponds to the end (which for a closed region will be
*     the same place as the start).

*  Parameters:
*     this
*        Pointer to the Region.
*     n
*        The number of positions to return. If this is zero, the function
*        returns without action (but the returned function value still
*        indicates if the method is supported or not).
*     dist
*        Pointer to an array of "n" scalar parameter values in the range
*        0 to 1.0.
*     ptr
*        A pointer to an array of pointers. The number of elements in
*        this array should equal tthe number of axes in the Frame spanned
*        by the Region. Each element of the array should be a pointer to
*        an array of "n" doubles, in which to return the "n" values for
*        the corresponding axis. The contents of the arrays are unchanged
*        if the supplied Region belongs to a class that does not
*        implement this method.

*  Returned Value:
*     Non-zero if the astRegTrace method is implemented by the class
*     of Region supplied, and zero if not.

*  Notes:
*    - The current algorithm results in the boundary of the CmpRegion
*    being dis-contiguous - supplied distance values from zero up to some
*    mid-value correspond to positions on the first component Region, and
*    higher distance (up to 1.0) correspond to points on the second
*    component Region.

*-
*/

/* Local Variables; */
   AstCmpRegion *this;
   AstFrame *frm;
   AstMapping *map;
   AstPointSet *bpset;
   AstPointSet *cpset;
   AstRegion *ureg1;
   AstRegion *ureg2;
   double **bptr;
   int i;
   int j;
   int ncur;
   int result;
   double *rval;
   double *off;
   double *r1d;
   double *r2d;
   double *r1ptr[ 2 ];
   double *r2ptr[ 2 ];
   double **r1ptrb;
   double **r2ptrb;
   double dbreak;
   double dtot;
   double x;
   double x0;
   int r1n;
   int r2n;
   AstPointSet *r1pset;
   AstPointSet *r2pset;
   AstPointSet *r1psetb;
   AstPointSet *r2psetb;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( ! astOK ) return result;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
   frm = astGetFrame( this_region->frameset, AST__BASE );

/* Check it is 2-dimensional. */
   result = 1;
   if( astGetNaxes( frm ) != 2 ) result = 0;

/* Check the component Regions can be traced. */
   if( !astRegTrace( this->region1, 0, NULL, NULL ) ||
       !astRegTrace( this->region1, 0, NULL, NULL ) ) result = 0;

/* Check we have some points to find. */
   if( result && n > 0 ) {

/* We first determine the required positions in the base Frame of the
   Region, and then transform them into the current Frame. Get the
   base->current Mapping, and the number of current Frame axes. */
      map = astGetMapping( this_region->frameset, AST__BASE, AST__CURRENT );

/* If it's a UnitMap we do not need to do the transformation, so put the
   base Frame positions directly into the supplied arrays. */
      if( astIsAUnitMap( map ) ) {
         bpset = NULL;
         bptr = ptr;
         ncur = 2;

/* Otherwise, create a PointSet to hold the base Frame positions. */
      } else {
         bpset = astPointSet( n, 2, " ", status );
         bptr = astGetPoints( bpset );
         ncur = astGetNout( map );
      }

      r1d = astMalloc( sizeof( double )*n );
      r2d = astMalloc( sizeof( double )*n );

/* Ensure information about the breaks in the boundary of each component
   region is available within the CmpRegion structure. These breaks are
   the points at which the two boundaries cross. */
      SetBreakInfo( this, 0, status );
      SetBreakInfo( this, 1, status );

/* Get the constants needed to convert the supplied distances (normalised
   so that the border of the entire CmpRegion has a length of 1.0), into
   geodesic distances around the border of each component Region. */
      dtot = this->d0[ 0 ] + this->d0[ 1 ];
      dbreak = this->d0[ 0 ]/dtot;

/* Initialise here to avoid compiler warnings. */
      r1n = 0;
      r2n = 0;

/* Check the pointers can be used safely. */
      if( astOK ) {

/* Loop round all supplied distances, determining if they represent a
   position on the first or second component Region. */
         for( i = 0; i < n; i++ ) {

/* If the current distance represents a point in the second component
   Region... */
            if( dist[ i ] > dbreak ) {

/* Find the correspond distance around the used sections of the second
   component region (normalised so that the entire border of the
   component region has a length of "this->d0[1]"). */
               x0 = ( dist[ i ] - dbreak )*dtot;
               x = x0;

/* Convert this into the correspond distance around the entire border of
   the second component region (normalised so that the entire border of the
   component region has unit length). */
               rval = this->rvals[ 1 ];
               off = this->offs[ 1 ];

               for( j = 0; j < this->nbreak[ 1 ]; j++,rval++,off++ ) {
                  if( *rval >= x0 ) break;
                  x += *off;
               }

/* Store this as the next distance to move around the second component
   Region, normalising it to the range 0 to 1 as required by astRegTrace. */
               r2d[ r2n++ ] = x/this->dtot[ 1 ];

/* Now we do the same if the current distance corresponds to a position
   in the first component Region. */
            } else {

               x0 = dist[ i ]*dtot;
               x = x0;

               rval = this->rvals[ 0 ];
               off = this->offs[ 0 ];

               for( j = 0; j < this->nbreak[ 0 ]; j++,rval++,off++ ) {
                  if( *rval >= x0 ) break;
                  x += *off;
               }

               r1d[ r1n++ ] = x/this->dtot[ 0 ];

            }

         }
      }

/* Allocate memory to hold the axis values at the corresponding positions
   in the first component Region. */
      r1ptr[ 0 ] = astMalloc( sizeof( double )*r1n );
      r1ptr[ 1 ] = astMalloc( sizeof( double )*r1n );

/* Allocate memory to hold the axis values at the corresponding positions
   in the second component Region. */
      r2ptr[ 0 ] = astMalloc( sizeof( double )*r2n );
      r2ptr[ 1 ] = astMalloc( sizeof( double )*r2n );

/* Check the pointers can be used safely. */
      if( astOK ) {

/* Find the axis values at each of the required positions that fall in
   the first component Region. Negate it first if needed to ensure the
   Region is bounded (not guaranteed, but likely). */
         if( astGetBounded( this->region1 ) ) {
            (void) astRegTrace( this->region1, r1n, r1d, r1ptr );
         } else {
            AstRegion *negation = astGetNegation( this->region1 );
            (void) astRegTrace( negation, r1n, r1d, r1ptr );
            negation = astAnnul( negation );
         }

/* Do the same for the second component Region. */
         if( astGetBounded( this->region2 ) ) {
            (void) astRegTrace( this->region2, r2n, r2d, r2ptr );
         } else {
            AstRegion *negation = astGetNegation( this->region2 );
            (void) astRegTrace( negation, r2n, r2d, r2ptr );
            negation = astAnnul( negation );
         }

/* The arrays of positions returned by the above calls to astRegTrace may
   include points that should not be there (e.g. points on the boundary
   of one component region that should have been blanked due to being inside
   the second component region - if the regions are ORed together). This
   is a consequence of the relatively low value of the "NP" local constant
   in function SetBreakInfo. So we now refine the positions to exclude
   any such unwanted positions.

   If the two component Regions are ANDed together, we want to remove the
   positions from the boundary of the required component Region that fall
   outside the other region. We can do this by simply using the other Region
   as a Mapping. If the two component Regions are ORed together, we want to
   remove the position that fall within (rather than outside) the other
   Region. To do this we need to negate the other region  first. */
         if( this->oper == AST__OR ) {
            ureg1 = astGetNegation( this->region1 );
            ureg2 = astGetNegation( this->region2 );
         } else {
            ureg1 = astClone( this->region1 );
            ureg2 = astClone( this->region2 );
         }

/* Now transform the points on the boundary of the first Region in order
   to set invalid those positions which are not on the boundary of the
   supplied CmpRegion. */
         if( r1n > 0 ) {
            r1pset = astPointSet( r1n, 2, " ", status );
            astSetPoints( r1pset, r1ptr );
            r1psetb = astTransform( ureg2, r1pset, 1, NULL );
            r1ptrb = astGetPoints( r1psetb );
         } else {
            r1pset = NULL;
            r1psetb = NULL;
            r1ptrb = NULL;
         }

/* Now transform the points on the boundary of the second Region in order
   to set invalid those positions which are not on the boundary of the
   supplied CmpRegion. */
         if( r2n > 0 ) {
            r2pset = astPointSet( r2n, 2, " ", status );
            astSetPoints( r2pset, r2ptr );
            r2psetb = astTransform( ureg1, r2pset, 1, NULL );
            r2ptrb = astGetPoints( r2psetb );
         } else {
            r2pset = NULL;
            r2psetb = NULL;
            r2ptrb = NULL;
         }

/* Free the begation pointers. */
         ureg1 = astAnnul( ureg1 );
         ureg2 = astAnnul( ureg2 );

/* Check pointer can be used safely. */
         if( astOK ) {

/* Copy the boundary positions from each component Region into a single
   PointSet. These positions are in the base Frame of the CmpRegion. */
            r1n = 0;
            r2n = 0;
            for( i = 0; i < n; i++ ) {
               if( dist[ i ] > dbreak ) {
                  bptr[ 0 ][ i ] = r2ptrb[ 0 ][ r2n ];
                  bptr[ 1 ][ i ] = r2ptrb[ 1 ][ r2n++ ];
               } else {
                  bptr[ 0 ][ i ] = r1ptrb[ 0 ][ r1n ];
                  bptr[ 1 ][ i ] = r1ptrb[ 1 ][ r1n++ ];
               }
            }

         }

/* Free resources. */
         if( r1pset ) r1pset = astAnnul( r1pset );
         if( r2pset ) r2pset = astAnnul( r2pset );
         if( r1psetb ) r1psetb = astAnnul( r1psetb );
         if( r2psetb ) r2psetb = astAnnul( r2psetb );

      }

/* If required, transform the base frame positions into the current
   Frame of the CmpRegion, storing them in the supplied array. Then
   free resources. */
      if( bpset ) {
         cpset = astPointSet( n, ncur, " ", status );
         astSetPoints( cpset, ptr );

         (void) astTransform( map, bpset, 1, cpset );

         cpset = astAnnul( cpset );
         bpset = astAnnul( bpset );
      }

/* Free remaining resources. */
      map = astAnnul( map );
   }
   frm = astAnnul( frm );

/* Return the result. */
   return result;
}

static void RegClearAttrib( AstRegion *this_region, const char *attrib,
                            char **base_attrib, int *status ) {
/*
*  Name:
*     RegClearAttrib

*  Purpose:
*     Clear an attribute value for a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void RegClearAttrib( AstRegion *this, const char *attrib,
*                          char **base_attrib, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astRegClearAttrib method
*     inherited from the Region class).

*  Description:
*     This function clears the value of a named attribute in both the base
*     and current Frame in the FrameSet encapsulated within a Region, without
*     remapping either Frame.
*
*     No error is reported if the attribute is not recognised by the base
*     Frame.

*  Parameters:
*     this
*        Pointer to the Region.
*     attrib
*        Pointer to a null terminated string holding the attribute name.
*        NOTE, IT SHOULD BE ENTIRELY LOWER CASE.
*     base_attrib
*        Address of a location at which to return a pointer to the null
*        terminated string holding the attribute name which was cleared in
*        the base Frame of the encapsulated FrameSet. This may differ from
*        the supplied attribute if the supplied attribute contains an axis
*        index and the current->base Mapping in the FrameSet produces an
*        axis permutation. The returned pointer should be freed using
*        astFree when no longer needed. A NULL pointer may be supplied in
*        which case no pointer is returned.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstCmpRegion *this;
   char *batt;
   int rep;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_region;

/* Use the RegClearAttrib method inherited from the parent class to clear the
   attribute in the current and base Frames in the FrameSet encapsulated by
   the parent Region structure. */
   (*parent_regclearattrib)( this_region, attrib, &batt, status );

/* Now clear the base Frame attribute to the component Regions (the current
   Frame within the component Regions is equivalent to the base Frame in the
   parent Region structure). Annul any "attribute unknown" error that results
   from attempting to do this. */
   if( astOK ) {
      rep = astReporting( 0 );
      astRegClearAttrib( this->region1, batt, NULL );
      astRegClearAttrib( this->region2, batt, NULL );
      if( astStatus == AST__BADAT ) astClearStatus;
      astReporting( rep );
   }

/* If required, return the base Frame attribute name, otherwise free it. */
   if( base_attrib ) {
      *base_attrib = batt;
   } else {
      batt = astFree( batt );
   }
}

static void ResetCache( AstRegion *this_region, int *status ){
/*
*  Name:
*     ResetCache

*  Purpose:
*     Clear cached information within the supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void ResetCache( AstRegion *this, int *status )

*  Class Membership:
*     Region member function (overrides the astResetCache method
*     inherited from the parent Region class).

*  Description:
*     This function clears cached information from the supplied Region
*     structure.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables *: */
   AstCmpRegion *this;
   int i;

/* Check a Region was supplied. */
   if( this_region ) {

/* Get a pointer to the CmpRegion structure. */
      this = (AstCmpRegion *) this_region;

/* Clear information cached in the CmpRegion structure. */
      for( i = 0; i < 2; i++ ) {
         this->rvals[ i ] = astFree(  this->rvals[ i ] );
         this->offs[ i ] = astFree(  this->offs[ i ] );
         this->nbreak[ i ] = 0;
         this->d0[ i ] = AST__BAD;
         this->dtot[ i ] = AST__BAD;
      }

      this->bounded = -INT_MAX;

/* Clear information cached in the component regions. */
      if( this->region1 ) astResetCache( this->region1 );
      if( this->region2 ) astResetCache( this->region2 );

/* Clear information cached in the parent Region structure. */
      (*parent_resetcache)( this_region, status );
   }
}

static void SetBreakInfo( AstCmpRegion *this, int comp, int *status ){
/*
*  Name:
*     SetBreakInfo

*  Purpose:
*     Ensure that a CmpRegion has information about the breaks in the
*     boundaries of one of the two component Regions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void SetBreakInfo( AstCmpRegion *this, int comp, int *status )

*  Class Membership:
*     CmpRegion method.

*  Description:
*     This function returns without action if the supplied CmpRegion
*     already contains break information for the specified component Region.
*     Otherwise, it creates the required information and stores it in the
*     CmpRegion.
*
*     Each component Region in the CmpRegion has a boundary. But in
*     general only part of the boundary of a component Region will also
*     be included in the CmpRegion boundary. Thus the component Region
*     boundary can be broken up into sections; sections that form part
*     of the CmpRegion boundary, and sections that do not. This function
*     stores information about the breaks between these sections.
*
*     The complete boundary of a component Region is parameterised by a
*     geodesic distance that goes from 0.0 to the value found by this
*     function and stored in this->dtot (the total geodesic distance
*     around the border). This function find the ranges of this parameter
*     that correspond to the sections of the boundary that are also on the
*     CmpRegion boundary, and thus finds the total length that the component
*     boundary contributes to the CmpRegion boundary. This length is stored
*     in "this->d0" (a two element array, one for each component Region).
*
*     It also find two arrays "this->rvals" and "this->offs" that allow a
*     distance value in the range 0.0 to "this->d0" (i.e. a distance
*     measured by skipping over the parts of the component boundary that
*     are not on the CmpRegion boundary), to be converted into the
*     corresponding distance value in the range 0.0 to "this->dtot" (i.e. a
*     distance measured round the complete component boundary, including the
*     parts not on the CmpRegion boundary).

*  Parameters:
*     this
*        Pointer to a CmpRegion.
*     comp
*        Zero or one, indicating which component Region is to be checked.
*     status
*        Pointer to the inherited status variable.

*/

/* The number of points to be spread evenly over the entire boundary of the
   component Region. */
#define NP 101

/* Local Variables: */
   AstFrame *frm;
   AstPointSet *pset1;
   AstPointSet *pset2;
   AstRegion *other;
   AstRegion *reg;
   AstRegion *uother;
   double **ptr2;
   double **ptr1;
   double *d;
   double *offs;
   double *p0;
   double *p1;
   double *p;
   double *q;
   double *rvals;
   double delta;
   double dist;
   double pnt1[ 2 ];
   double pnt2[ 2 ];
   double rbad;
   double rval;
   double totdist;
   int i;
   int j;
   int nn;
   int prevgood;

/* Check inherited status */
   if( !astOK ) return;

/* If the information describing breaks in the component boundary has not
   yet been set up, do so now. */
   if( this->d0[ comp ] == AST__BAD ) {

/* Get a pointer to the component Region for which break information is
   required. */
      reg = comp ? this->region2 : this->region1;

/* Check the component class implements the astRegTrace method. */
      if( astRegTrace( reg, 0, NULL, NULL ) ) {

/* Create a pointSet to hold axis values at evenly spaced positions along
   the entire boundary of the selected component region. */
         pset1 = astPointSet( NP, 2, " ", status );
         ptr1 = astGetPoints( pset1 );

/* Allocate memory to hold an array of corresponding scalar distances around
   the boundary. */
         d = astMalloc( NP*sizeof( double ) );

/* Check pointers can be used safely. */
         if( astOK ) {

/* Get the distance increment between points (at this point the distances
   are normalised so that the entire boundary has unit length, as
   required by astRegTrace). */
            delta = 1.0/( NP - 1 );

/* Set up the array of evenly spaced distances around the boundary of the
   component region. */
            for( i = 0; i < NP; i++ ) d[ i ] = i*delta;

/* Get the corresponding Frame positions. If the Region is unbounded
   (e.g. a negated circle, etc), then negate it first in the hope that
   this may produced a bounded Region. */
            if( astGetBounded( reg ) ) {
               (void) astRegTrace( reg, NP, d, ptr1 );
            } else {
               AstRegion *negation = astGetNegation( reg );
               (void) astRegTrace( negation, NP, d, ptr1 );
               negation = astAnnul( negation );
            }

/* Get a pointer to the other component Region. */
            other = comp ? this->region1 : this->region2;

/* If the two component Regions are ANDed together, we want to remove the
   positions from the boundary of the required component Region that fall
   outside the other region. We can do this by simply using the other Region
   as a Mapping. If the two component Regions are ORed together, we want to
   remove the position that fall within (rather than outside) the other
   Region. To do this we need to negate the other region  first. */
            if( this->oper == AST__OR ) {
               uother = astGetNegation( other );
            } else {
               uother = astClone( other );
            }

/* Now transform the points on the boundary of the selected Region in
   order to set invalid those positions which are not on the boundary of
   the supplied CmpRegion. */
            pset2 = astTransform( uother, pset1, 1, NULL );

/* Annul the negation pointer */
            uother = astAnnul( uother );

/* Modify the distance array by setting invalid each element that is not
   on the boundary of the CmpRegion. */
            ptr2 = astGetPoints( pset2 );
            if( astOK ) {
               p = ptr2[ 0 ];
               q = ptr2[ 1 ];
               for( i = 0; i < NP; i++,p++,q++ ) {
                  if( *p == AST__BAD || *q == AST__BAD ) d[ i ] = AST__BAD;
               }

/* At each good/bad junction in this list, extend the good section by one
   point. This ensures that the good sections of the curve do in fact
   touch each other (they may in fact overlap a little but that does not
   matter). */
               prevgood = ( d[ 0 ] != AST__BAD );
               for( i = 1; i < NP; i++,p++,q++ ) {
                  if( d[ i ] == AST__BAD ) {
                     if( prevgood ) d[ i ] = i*delta;
                     prevgood = 0;

                  } else {
                     if( !prevgood ) d[ i - 1 ] = ( i - 1 )*delta;
                     prevgood = 1;
                  }
               }

/* Find the total geodesic distance around the border. This is only an
   approximation but it is only used to give a relative weight to this
   component within the CmpFrame, and so does not need to be very accurate. */
               frm = astGetFrame( reg->frameset, AST__CURRENT );
               p0 = ptr1[ 0 ];
               p1 = ptr1[ 1 ];
               totdist = 0;
               pnt1[ 0 ] = *(p0++);
               pnt1[ 1 ] = *(p1++);
               for( i = 1; i < NP; i++ ) {
                  pnt2[ 0 ] = *(p0++);
                  pnt2[ 1 ] = *(p1++);
                  dist = astDistance( frm, pnt1, pnt2 );
                  if( dist != AST__BAD ) totdist += dist;
                  pnt1[ 0 ] = pnt2[ 0 ];
                  pnt1[ 1 ] = pnt2[ 1 ];
               }

/* Change delta so that it represents a geodesic distance, rather than a
   normalised distance in the range zero to one. Working in geodesic distance
   (e.g. Radians on a SkyFrame) prevents Regions higher up in a complex nested
   CmpRegion being given higher priority than a lower Region. */
               delta *= totdist;

/* Now create two arrays - "rvals" holds the distance travelled around
   the used parts of the border at which breaks occur, "offs" holds the jump
   in distance around the complete border at each break. The distance
   around the complete border is normalised to the range [0.0,1.0].
   Therefore the total distance around the used parts of the border will in
   general be less than 1.0 */
               if( d[ 0 ] == AST__BAD ) {
                  nn = 1;
                  j = 0;
                  rvals = astMalloc( sizeof( double ) );
                  offs = astMalloc( sizeof( double ) );
                  if( astOK ) rvals[ 0 ] = -0.5*delta;
                  rbad = 0.5;
                  prevgood = 0;
                  rval = -0.5*delta;

               } else {
                  nn = 0;
                  rvals = NULL;
                  offs = NULL;
                  prevgood = 1;
                  rbad = 0.0;
                  rval = 0.0;
               }

               for( i = 1; i < NP; i++,p++,q++ ) {

                  if( d[ i ] == AST__BAD ) {
                     if( prevgood ) {
                        j = nn++;
                        rvals = astGrow( rvals, nn, sizeof( double ) );
                        offs = astGrow( offs, nn, sizeof( double ) );
                        if( astOK ) {
                           rvals[ j ] = rval + 0.5*delta;
                           rbad = 0.0;
                        } else {
                           break;
                        }
                        prevgood = 0;
                     }

                     rbad += 1.0;

                  } else {
                     if( !prevgood ) {
                        offs[ j ] = rbad*delta;
                        prevgood = 1;
                     }
                     rval += delta;
                  }
               }

               if( !prevgood ) {
                  rval += 0.5*delta;
                  offs[ j ] = rbad*delta;
               }

/* Record the information in the CmpRegion structure. */
               this->rvals[ comp ] = rvals;
               this->offs[ comp ] = offs;
               this->nbreak[ comp ] = nn;
               this->d0[ comp ] = rval;
               this->dtot[ comp ] = totdist;
            }

/* Free resources. */
            pset2 = astAnnul( pset2 );
         }

         pset1 = astAnnul( pset1 );
         d = astFree( d );

      }
   }
}

#undef NP

static void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status ) {
/*
*  Name:
*     SetRegFS

*  Purpose:
*     Stores a new FrameSet in a Region

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     CmpRegion method (over-rides the astSetRegFS method inherited from
*     the Region class).

*  Description:
*     This function creates a new FrameSet and stores it in the supplied
*     Region. The new FrameSet contains two copies of the supplied
*     Frame, connected by a UnitMap.

*  Parameters:
*     this
*        Pointer to the Region.
*     frm
*        The Frame to use.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstRegion *creg;        /* Pointer to component Region structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the parent method to store the FrameSet in the parent Region
   structure. */
   (* parent_setregfs)( this_region, frm, status );

/* If either component Region has a dummy FrameSet use this method
   recursively to give them the same FrameSet. */
   creg = ((AstCmpRegion *) this_region )->region1;
   if( creg && !astGetRegionFS( creg ) ) astSetRegFS( creg, frm );

   creg = ((AstCmpRegion *) this_region )->region2;
   if( creg && !astGetRegionFS( creg ) ) astSetRegFS( creg, frm );

}

static AstMapping *Simplify( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     Simplify

*  Purpose:
*     Simplify a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "region.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     CmpRegion method (over-rides the astSimplify method inherited from
*     the Region class).

*  Description:
*     This function simplifies a CmpRegion to eliminate redundant
*     computational steps, or to merge separate steps which can be
*     performed more efficiently in a single operation.

*  Parameters:
*     this
*        Pointer to the original Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A new pointer to the (possibly simplified) Region.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.

*  Deficiencies:
*     - Currently, this function does not attempt to map the component
*     Regions into the current Frame of the parent Region structure.
*     Both components should be mapped into the current Frame, and if the
*     resulting base->current Mappings in *both* remapped component Regions are
*     UnitMaps, then a new CmpRegion should be created from the re-mapped
*     Regions.
*/

/* Local Variables: */
   AstCmpRegion *newb;           /* New CmpRegion defined in base Frame */
   AstCmpRegion *newc;           /* New CmpRegion defined in current Frame */
   AstFrame *frm;                /* Current Frame */
   AstMapping *map;              /* Base->current Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstRegion *csreg1;            /* Copy of simplified first component Region */
   AstRegion *csreg2;            /* Copy of simplified second component Region */
   AstRegion *nullreg;           /* Null or infinfite Region */
   AstRegion *othereg;           /* Non-Null and non-infinfite Region */
   AstRegion *reg1;              /* First component Region */
   AstRegion *reg2;              /* Second component Region */
   AstRegion *sreg1;             /* Simplified first component Region */
   AstRegion *sreg2;             /* Simplified second component Region */
   int neg1;                     /* Negated flag to use with first component */
   int neg2;                     /* Negated flag to use with second component */
   int oper;                     /* Boolean operator used to combine components */
   int overlap;                  /* Nature of overlap between components */
   int simpler;                  /* Has any simplification taken place? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the parent Simplify method inherited from the Region class. This
   will simplify the encapsulated FrameSet and uncertainty Region. The
   returned pointer identifies a region within the current Frame of the
   FrameSet encapsulated by the parent Region structure. Note this by
   storing the pointer in the "newc" ("c" for "current") variable. */
   newc = (AstCmpRegion *) (*parent_simplify)( this_mapping, status );

/* Note if any simplification took place. This is assumed to be the case
   if the pointer returned by the above call is different to the supplied
   pointer. */
   simpler = ( (AstMapping *) newc != this_mapping );

/* Below we may create a new simplified region which identifies a region
   within the base Frame of the FrameSet encapsulated by the parent Region
   structure. Such a result will need to be mapped into the current Frame
   before being returned. The "newb" variable ("b" for "base") will be
   used to store a pointer to such a result. Initialise this variable to
   indicate that we do not yet have a base Frame result. */
   newb = NULL;

/* Get the component Regions, how they should be combined, and the
   Negated values which should be used with them. The returned values
   take account of whether the supplied CmpRegion has itself been Negated
   or not. The returned Regions represent regions within the base Frame
   of the FrameSet encapsulated by the parent Region structure. */
   GetRegions( newc, &reg1, &reg2, &oper, &neg1, &neg2, status );

/* If the first component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg1" in place of "reg1"
   itself. */
   if( neg1 != astGetNegated( reg1 ) ) {
      AstRegion *tmp = astGetNegation( reg1 );
      (void) astAnnul( reg1 );
      reg1 = tmp;
   }

/* If the second component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg2" in place of "reg2"
   itself. */
   if( neg2 != astGetNegated( reg2 ) ) {
      AstRegion *tmp = astGetNegation( reg2 );
      (void) astAnnul( reg2 );
      reg2 = tmp;
   }

/* Simplify each of the two components. */
   sreg1 = astSimplify( reg1 );
   sreg2 = astSimplify( reg2 );

/* Note if any simplification took place. */
   simpler = simpler || ( sreg1 != reg1 || sreg2 != reg2 );

/* If either component is null or infinite we can exclude it from the
   returned Region. */
   if( astIsANullRegion( sreg1 ) || astIsANullRegion( sreg2 ) ) {

/* Get a pointer to the non-null Region. The following is still valid
   even if both regions are null or infinite. */
      if( astIsANullRegion( sreg1 ) ){
         nullreg = sreg1;
         othereg = sreg2;
      } else {
         nullreg = sreg2;
         othereg = sreg1;
      }

/* If null.. */
      if( !astGetNegated( nullreg ) ){
         if( oper == AST__AND ) {
            newb = (AstCmpRegion *) astNullRegion( othereg,
                                             astGetUnc( othereg, 0 ), "", status );

         } else if( oper == AST__OR ) {
            newb = astCopy( othereg );

         } else {
            astError( AST__INTER, "astSimplify(%s): The %s refers to an "
                      "unknown boolean operator with identifier %d (internal "
                      "AST programming error).", status, astGetClass( newc ),
                      astGetClass( newc ), oper );
         }

/* If infinite.. */
      } else {
         if( oper == AST__AND ) {
            newb = astCopy( othereg );

         } else if( oper == AST__OR ) {
            newb = (AstCmpRegion *) astNullRegion( othereg,
                                      astGetUnc( othereg, 0 ), "negated=1", status );

         } else {
            astError( AST__INTER, "astSimplify(%s): The %s refers to an "
                      "unknown boolean operator with identifier %d (internal "
                      "AST programming error).", status, astGetClass( newc ),
                      astGetClass( newc ), oper );
         }
      }

/* Flag that we have done some simplication.*/
      simpler = 1;

/* If neither component is null or infinite, see if it is possible to
   remove one or both of the components on the basis of the overlap
   between them. */
   } else {
      overlap = astOverlap( sreg1, sreg2 );

/* If the components have no overlap, and they are combined using AND, then
   the CmpRegion is null. */
      if( ( overlap == 1 || overlap == 6 ) && oper == AST__AND ) {
         newb = (AstCmpRegion *) astNullRegion( sreg1, astGetUnc( sreg1, 0 ),
                                                "", status );
         simpler = 1;

/* If one component is the negation of the other component, and they are
   combined using OR, then the CmpRegion is infinite. This is represented
   by a negated null region.*/
      } else if( overlap == 6 && oper == AST__OR ) {
         newb = (AstCmpRegion  *) astNullRegion( sreg1, astGetUnc( sreg1, 0 ),
                                                 "negated=1", status );
         simpler = 1;

/* If the two components are identical... */
      } else if( overlap == 5 ) {
         simpler = 1;

/* If combined with AND or OR, the CmpRegion can be replaced by the first
   (or second) component Region. */
         if( oper == AST__AND || oper == AST__OR ) {
            newb = astCopy( sreg1 );
         } else {
            astError( AST__INTER, "astSimplify(%s): The %s refers to an "
                      "unknown boolean operator with identifier %d (internal "
                      "AST programming error).", status, astGetClass( newc ),
                      astGetClass( newc ), oper );
         }

/* If the first component is entirely contained within the second
   component, and they are combined using AND or OR, then the CmpRegion
   can be replaced by the first or second component. */
      } else if( overlap == 2 && ( oper == AST__AND || oper == AST__OR ) ){
         newb = astCopy( ( oper == AST__AND ) ? sreg1 : sreg2 );
         simpler = 1;

/* If the second component is entirely contained within the first
   component, and they are combined using AND or OR, then the CmpRegion
   can be replaced by the second or first component. */
      } else if( overlap == 3 && ( oper == AST__AND || oper == AST__OR ) ){
         newb = astCopy( ( oper == AST__AND ) ? sreg2 : sreg1 );
         simpler = 1;

/* Otherwise, no further simplication is possible, so either create a new
   CmpRegion or leave the "newb" pointer NULL (which will cause "newc" to
   be used), depending on whether the components were simplified. */
      } else if( simpler ){
         csreg1 = astCopy( sreg1 );
         csreg2 = astCopy( sreg2 );
         newb = astCmpRegion( csreg1, csreg2, oper, "", status );
         csreg1 = astAnnul( csreg1 );
         csreg2 = astAnnul( csreg2 );

      }
   }

/* If any simplification took place, decide whether to use the "newc" or
   "newb" pointer for the returned Mapping. If "newb" is non-NULL we use
   it, otherwise we use "newc". If "newb" is used we must first map the
   result Region from the base Frame of the FrameSet encapsulated
   by the parent Region structure, to the current Frame. */
   if( simpler ) {
      if( newb ){
         frm = astGetFrame( ((AstRegion *) newc)->frameset, AST__CURRENT );
         map = astGetMapping( ((AstRegion *) newc)->frameset, AST__BASE, AST__CURRENT );
         result = astMapRegion( newb, map, frm );
         frm = astAnnul( frm );
         map = astAnnul( map );
         newb = astAnnul( newb );
      } else {
         result = astClone( newc );
      }

/* If no simplification took place, return a clone of the supplied pointer. */
   } else {
      result = astClone( this_mapping );
   }

/* Free resources. */
   reg1 = astAnnul( reg1 );
   reg2 = astAnnul( reg2 );
   sreg1 = astAnnul( sreg1 );
   sreg2 = astAnnul( sreg2 );
   newc = astAnnul( newc );

/* If an error occurred, annul the returned Mapping. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a CmpRegion to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     CmpRegion member function (over-rides the astTransform method inherited
*     from the Region class).

*  Description:
*     This function takes a CmpRegion and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Region.
*     This implies applying each of the CmpRegion's component Regions in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the CmpRegion.
*     in
*        Pointer to the PointSet associated with the input coordinate values.
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
*     match the number of coordinates for the CmpRegion being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstCmpRegion *this;           /* Pointer to the CmpRegion structure */
   AstPointSet *ps1;             /* Pointer to PointSet for first component */
   AstPointSet *ps2;             /* Pointer to PointSet for second component */
   AstPointSet *pset_tmp;        /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   AstRegion *reg1;              /* Pointer to first component Region */
   AstRegion *reg2;              /* Pointer to second component Region */
   double **ptr1;                /* Pointer to first component axis values */
   double **ptr2;                /* Pointer to second component axis values */
   double **ptr_out;             /* Pointer to output coordinate data */
   int coord;                    /* Zero-based index for coordinates */
   int good;                     /* Is the point inside the CmpRegion? */
   int ncoord_out;               /* No. of coordinates per output point */
   int ncoord_tmp;               /* No. of coordinates per base Frame point */
   int neg1;                     /* Negated value for first component Region */
   int neg2;                     /* Negated value for second component Region */
   int npoint;                   /* No. of points */
   int oper;                     /* Boolean operator to use */
   int point;                    /* Loop counter for points */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a Pointer to the CmpRegion structure */
   this = (AstCmpRegion *) this_mapping;

/* Get the component Regions, how they should be combined, and the
   Negated values which should be used with them. The returned values
   take account of whether the supplied CmpRegion has itself been Negated
   or not. The returned Regions represent regions within the base Frame
   of the FrameSet encapsulated by the parent Region structure. */
   GetRegions( this, &reg1, &reg2, &oper, &neg1, &neg2, status );

/* If the first component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg1" in place of "reg1"
   itself. */
   if( neg1 != astGetNegated( reg1 ) ) {
      AstRegion *tmp = astGetNegation( reg1 );
      (void) astAnnul( reg1 );
      reg1 = tmp;
   }

/* If the second component Region does not have the required value for
   its "Negated" attribute, use the negation of "reg2" in place of "reg2"
   itself. */
   if( neg2 != astGetNegated( reg2 ) ) {
      AstRegion *tmp = astGetNegation( reg2 );
      (void) astAnnul( reg2 );
      reg2 = tmp;
   }

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary, containing
   a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet in the parent Region structure to
   transform the supplied positions from the current Frame in the
   encapsulated FrameSet (the Frame represented by the CmpRegion), to the
   base Frame (the Frame in which the component Regions are defined). Note,
   the returned pointer may be a clone of the "in" pointer, and so we
   must be carefull not to modify the contents of the returned PointSet. */
   pset_tmp = astRegTransform( this, in, 0, NULL, NULL );

/* Now transform this PointSet using each of the two component Regions in
   turn. */
   ps1 = astTransform( reg1, pset_tmp, 0, NULL );
   ps2 = astTransform( reg2, pset_tmp, 0, NULL );

/* Determine the numbers of points and coordinates per point for these base
   Frame PointSets and obtain pointers for accessing the base Frame and output
   coordinate values. */
   npoint = astGetNpoint( pset_tmp );
   ncoord_tmp = astGetNcoord( pset_tmp );
   ptr1 = astGetPoints( ps1 );
   ptr2 = astGetPoints( ps2 );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* First deal with ANDed Regions */
      if( oper == AST__AND ) {
         for ( point = 0; point < npoint; point++ ) {
            good = 0;

            for ( coord = 0; coord < ncoord_tmp; coord++ ) {
               if( ptr1[ coord ][ point ] != AST__BAD &&
                   ptr2[ coord ][ point ] != AST__BAD ) {
                  good = 1;
                  break;
               }
            }

            if( !good ) {
               for ( coord = 0; coord < ncoord_out; coord++ ) {
                  ptr_out[ coord ][ point ] = AST__BAD;
               }
            }
         }

/* Now deal with ORed Regions */
      } else if( oper == AST__OR ) {
         for ( point = 0; point < npoint; point++ ) {
            good = 0;

            for ( coord = 0; coord < ncoord_tmp; coord++ ) {
               if( ptr1[ coord ][ point ] != AST__BAD ||
                   ptr2[ coord ][ point ] != AST__BAD ) {
                  good = 1;
                  break;
               }
            }

            if( !good ) {
               for ( coord = 0; coord < ncoord_out; coord++ ) {
                  ptr_out[ coord ][ point ] = AST__BAD;
               }
            }
         }

/* Report error for any unknown operator. */
      } else if( astOK ) {
         astError( AST__INTER, "astTransform(%s): The %s refers to an unknown "
                   "boolean operator with identifier %d (internal AST "
                   "programming error).", status, astGetClass( this ),
                    astGetClass( this ), oper );
      }
   }

/* Free resources. */
   reg1 = astAnnul( reg1 );
   reg2 = astAnnul( reg2 );
   ps1 = astAnnul( ps1 );
   ps2 = astAnnul( ps2 );
   pset_tmp = astAnnul( pset_tmp );

/* If an error occurred, clean up by deleting the output PointSet (if
   allocated by this function) and setting a NULL result pointer. */
   if ( !astOK ) {
      if ( !out ) result = astDelete( result );
      result = NULL;
   }

/* Return a pointer to the output PointSet. */
   return result;
}

static void XORCheck( AstCmpRegion *this, int *status ) {
/*
*  Name:
*     XORCheck

*  Purpose:
*     Check if the supplied CmpRegion represents an XOR operation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpregion.h"
*      void XORCheck( AstCmpRegion *this, int *status )

*  Class Membership:
*     CmpRegion method

*  Decription:
*     This function analyses the component Regions within the supplied
*     CmpRegion to see if the CmpRegion is equivalent to an XOR operation
*     on two other Regions. If it is, teh Regions that are XORed are
*     stored in the supplied CmpRegion.

*  Parameters:
*     this
*        Pointer to the CmpRegion.

*/

/* Local Variables: */
   AstCmpRegion *cmpreg1;
   AstCmpRegion *cmpreg2;
   int xor;

/* Check the global error status. */
   if ( !astOK ) return;

/* If the CmpRegion is already known to be an XOR operation, return
   without action. */
   if( this->xor1 ) return;

/* To be equivalent to an XOR operation, the supplied CmpRegion must be an
   OR operation and each component Region must be a CmpRegion. */
   if( this->oper == AST__OR && astIsACmpRegion( this->region1 )
                             && astIsACmpRegion( this->region2 ) ) {
      cmpreg1 = (AstCmpRegion *) this->region1;
      cmpreg2 = (AstCmpRegion *) this->region2;

/* Each component CmpRegion must be an AND operation. */
      if( cmpreg1->oper == AST__AND && cmpreg2->oper == AST__AND ) {

/* Temporarily negate the first component of the first CmpRegion. */
         astNegate( cmpreg1->region1 );

/* Initially, assume the supplied CmpRegion is not equivalent to an XOR
   operation. */
         xor = 0;

/* This negated region must be equal to one of the two component Regions
   in the second component CmpRegion. Check the first. */
         if( astEqual( cmpreg1->region1, cmpreg2->region1 ) ) {

/* We now check that the other two Regions are equal (after negating the
   first). If so, set "xor" non-zero. */
            astNegate( cmpreg1->region2 );
            if( astEqual( cmpreg1->region2, cmpreg2->region2 ) ) xor = 1;
            astNegate( cmpreg1->region2 );

/* Do equiovalent checks the other way round. */
         } else if( astEqual( cmpreg1->region1, cmpreg2->region2 ) ) {
            astNegate( cmpreg1->region2 );
            if( astEqual( cmpreg1->region2, cmpreg2->region1 ) ) xor = 1;
            astNegate( cmpreg1->region2 );
         }

/* Re-instate the original state of the Negated attribute in the first
   component of the first CmpRegion. */
         astNegate( cmpreg1->region1 );

/* If the supplied CmpRegion is equivalent to an XOR operation, store
   copies of the components in the supplied CmpRegion. */
         if( xor ) {
            this->xor1 = astCopy( cmpreg1->region1 );
            this->xor2 = astCopy( cmpreg1->region2 );

/* We need to negate one of these two Region (it doesn't matter which),
   and we choose to negate which ever of them is already negated (so that
   it becomes un-negated). */
            if( astGetNegated( this->xor1 ) ) {
               astNegate( this->xor1 );
            } else {
               astNegate( this->xor2 );
            }
         }
      }
   }
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for CmpRegion objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for CmpRegion objects.

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
*     -  This constructor makes a deep copy, including a copy of the component
*     Regions within the CmpRegion.
*/

/* Local Variables: */
   AstCmpRegion *in;                /* Pointer to input CmpRegion */
   AstCmpRegion *out;               /* Pointer to output CmpRegion */
   int i;                           /* Loop count */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output CmpRegions. */
   in = (AstCmpRegion *) objin;
   out = (AstCmpRegion *) objout;

/* For safety, start by clearing any memory references in the output
   Region that were copied from the input Region. */
   out->region1 = NULL;
   out->region2 = NULL;
   out->xor1 = NULL;
   out->xor2 = NULL;

   for( i = 0; i < 2; i++ ) {
      out->rvals[ i ] = NULL;
      out->offs[ i ] = NULL;
   }

/* Make copies of these Regions and store pointers to them in the output
   CmpRegion structure. */
   out->region1 = astCopy( in->region1 );
   out->region2 = astCopy( in->region2 );
   if( in->xor1 ) out->xor1 = astCopy( in->xor1 );
   if( in->xor2 ) out->xor2 = astCopy( in->xor2 );

/* Copy cached arrays. */
   for( i = 0; i < 2; i++ ) {
      out->rvals[ i ] = astStore( NULL, in->rvals[ i ], in->nbreak[ i ]*sizeof( **in->rvals ) );
      out->offs[ i ] = astStore( NULL, in->offs[ i ], in->nbreak[ i ]*sizeof( **in->offs ) );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for CmpRegion objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for CmpRegion objects.

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
   AstCmpRegion *this;              /* Pointer to CmpRegion */
   int i;

/* Obtain a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) obj;

/* Free arrays holding cached information. */
   for( i = 0; i < 2; i++ ) {
      this->rvals[ i ] = astFree( this->rvals[ i ] );
      this->offs[ i ] = astFree( this->offs[ i ] );
   }

/* Annul the pointers to the component Regions. */
   this->region1 = astAnnul( this->region1 );
   this->region2 = astAnnul( this->region2 );
   if( this->xor1 ) this->xor1 = astAnnul( this->xor1 );
   if( this->xor2 ) this->xor2 = astAnnul( this->xor2 );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for CmpRegion objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the CmpRegion class to an output Channel.

*  Parameters:
*     this
*        Pointer to the CmpRegion whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstRegion *reg1;              /* First Region to include in dump */
   AstRegion *reg2;              /* Second Region to include in dump */
   AstCmpRegion *this;           /* Pointer to the CmpRegion structure */
   const char *comment;          /* Pointer to comment string */
   int ival;                     /* Integer value */
   int oper;                     /* The operator to include in the dump */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the CmpRegion structure. */
   this = (AstCmpRegion *) this_object;

/* Check if this CmpRegion has an equivalent XOR representation. Is so,
   store details of the XOR representation in the CmpRegion. */
   XORCheck( this, status );

/* Choose the operator and component regions to include in the dump. If
   the CmpRegion originally used an XOR operator, then save the XORed
   regions. Otherwise, store the real component Regions. */
   if( this->xor1 ) {
      oper = AST__XOR;
      reg1 = this->xor1;
      reg2 = this->xor2;
   } else {
      oper = this->oper;
      reg1 = this->region1;
      reg2 = this->region2;
   }

/* Write out values representing the instance variables for the CmpRegion
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

/* Oper */
/* ------- */
   ival = oper;
   if( ival == AST__AND ) {
      comment = "Regions combined using Boolean AND";
   } else if( ival == AST__OR ) {
      comment = "Regions combined using Boolean OR";
   } else if( ival == AST__XOR ) {
      comment = "Regions combined using Boolean XOR";
   } else {
      comment = "Regions combined using unknown operator";
   }
   astWriteInt( channel, "Operator", 1, 0, ival, comment );

/* First Region. */
/* -------------- */
   astWriteObject( channel, "RegionA", 1, 1, reg1,
                   "First component Region" );

/* Second Region. */
/* --------------- */
   astWriteObject( channel, "RegionB", 1, 1, reg2,
                   "Second component Region" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsACmpRegion and astCheckCmpRegion functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(CmpRegion,Region)
astMAKE_CHECK(CmpRegion)

AstCmpRegion *astCmpRegion_( void *region1_void, void *region2_void, int oper,
                             const char *options, int *status, ...) {
/*
*+
*  Name:
*     astCmpRegion

*  Purpose:
*     Create a CmpRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstCmpRegion *astCmpRegion( AstRegion *region1, AstRegion *region2,
*                                 int oper, const char *options, ..., int *status )

*  Class Membership:
*     CmpRegion constructor.

*  Description:
*     This function creates a new CmpRegion and optionally initialises its
*     attributes.

*  Parameters:
*     region1
*        Pointer to the first Region.
*     region2
*        Pointer to the second Region.
*     oper
*        The boolean operator with which to combine the two Regions. Either
*        AST__AND or AST__OR.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new CmpRegion. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new CmpRegion.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic CmpRegion constructor which is
*     available via the protected interface to the CmpRegion class.  A
*     public interface is provided by the astCmpRegionId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "region1" and "region2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpRegion *new;              /* Pointer to new CmpRegion */
   AstRegion *region1;             /* Pointer to first Region structure */
   AstRegion *region2;             /* Pointer to second Region structure */
   va_list args;                   /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain and validate pointers to the Region structures provided. */
   region1 = astCheckRegion( region1_void );
   region2 = astCheckRegion( region2_void );
   if ( astOK ) {

/* Initialise the CmpRegion, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitCmpRegion( NULL, sizeof( AstCmpRegion ), !class_init,
                              &class_vtab, "CmpRegion", region1, region2,
                              oper );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new CmpRegion's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new CmpRegion. */
   return new;
}

AstCmpRegion *astCmpRegionId_( void *region1_void, void *region2_void,
                               int oper, const char *options, ... ) {
/*
*++
*  Name:
c     astCmpRegion
f     AST_CMPREGION

*  Purpose:
*     Create a CmpRegion.

*  Type:
*     Public function.

*  Synopsis:
c     #include "cmpregion.h"
c     AstCmpRegion *astCmpRegion( AstRegion *region1, AstRegion *region2,
c                                 int oper, const char *options, ... )
f     RESULT = AST_CMPREGION( REGION1, REGION2, OPER, OPTIONS, STATUS )

*  Class Membership:
*     CmpRegion constructor.

*  Description:
*     This function creates a new CmpRegion and optionally initialises
*     its attributes.
*
*     A CmpRegion is a Region which allows two component
*     Regions (of any class) to be combined to form a more complex
*     Region. This combination may be performed a boolean AND, OR
*     or XOR (exclusive OR) operator. If the AND operator is
*     used, then a position is inside the CmpRegion only if it is
*     inside both of its two component Regions. If the OR operator is
*     used, then a position is inside the CmpRegion if it is inside
*     either (or both) of its two component Regions. If the XOR operator
*     is used, then a position is inside the CmpRegion if it is inside
*     one but not both of its two component Regions. Other operators can
*     be formed by negating one or both component Regions before using
*     them to construct a new CmpRegion.
*
*     The two component Region need not refer to the same coordinate
*     Frame, but it must be possible for the
c     astConvert
f     AST_CONVERT
*     function to determine a Mapping between them (an error will be
*     reported otherwise when the CmpRegion is created). For instance,
*     a CmpRegion may combine a Region defined within an ICRS SkyFrame
*     with a Region defined within a Galactic SkyFrame. This is
*     acceptable because the SkyFrame class knows how to convert between
*     these two systems, and consequently the
c     astConvert
f     AST_CONVERT
*     function will also be able to convert between them. In such cases,
*     the second component Region will be mapped into the coordinate Frame
*     of the first component Region, and the Frame represented by the
*     CmpRegion as a whole will be the Frame of the first component Region.
*
*     Since a CmpRegion is itself a Region, it can be used as a
*     component in forming further CmpRegions. Regions of arbitrary
*     complexity may be built from simple individual Regions in this
*     way.

*  Parameters:
c     region1
f     REGION1 = INTEGER (Given)
*        Pointer to the first component Region.
c     region2
f     REGION2 = INTEGER (Given)
*        Pointer to the second component Region. This Region will be
*        transformed into the coordinate Frame of the first region before
*        use. An error will be reported if this is not possible.
c     oper
f     OPER = INTEGER (Given)
*        The boolean operator with which to combine the two Regions. This
*        must be one of the symbolic constants AST__AND, AST__OR or AST__XOR.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new CmpRegion. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new CmpRegion. The syntax used is identical to that for the
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
c     astCmpRegion()
f     AST_CMPREGION = INTEGER
*        A pointer to the new CmpRegion.

*  Notes:
*     - If one of the supplied Regions has an associated uncertainty,
*     that uncertainty will also be used for the returned CmpRegion.
*     If both supplied Regions have associated uncertainties, the
*     uncertainty associated with the first Region will be used for the
*     returned CmpRegion.
*     - Deep copies are taken of the supplied Regions. This means that
*     any subsequent changes made to the component Regions using the
*     supplied pointers will have no effect on the CmpRegion.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astCmpRegion constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astCmpRegion_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "region1" and "region2" parameters
*     are of type (void *) and are converted from an ID value to a
*     pointer and validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astCmpRegion_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpRegion *new;              /* Pointer to new CmpRegion */
   AstRegion *region1;             /* Pointer to first Region structure */
   AstRegion *region2;             /* Pointer to second Region structure */
   va_list args;                   /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise. */
   new = NULL;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain the Region pointers from the ID's supplied and validate the
   pointers to ensure they identify valid Regions. */
   region1 = astVerifyRegion( astMakePointer( region1_void ) );
   region2 = astVerifyRegion( astMakePointer( region2_void ) );
   if ( astOK ) {

/* Initialise the CmpRegion, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitCmpRegion( NULL, sizeof( AstCmpRegion ), !class_init,
                              &class_vtab, "CmpRegion", region1, region2,
                              oper );

/* If successful, note that the virtual function table has been initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new CmpRegion's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new CmpRegion. */
   return astMakeId( new );
}

AstCmpRegion *astInitCmpRegion_( void *mem, size_t size, int init,
                                 AstCmpRegionVtab *vtab, const char *name,
                                 AstRegion *region1, AstRegion *region2,
                                 int oper, int *status ) {
/*
*+
*  Name:
*     astInitCmpRegion

*  Purpose:
*     Initialise a CmpRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstCmpRegion *astInitCmpRegion_( void *mem, size_t size, int init,
*                                      AstCmpRegionVtab *vtab, const char *name,
*                                      AstRegion *region1, AstRegion *region2,
*                                      int oper )

*  Class Membership:
*     CmpRegion initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new CmpRegion object. It allocates memory (if necessary) to
*     accommodate the CmpRegion plus any additional data associated with the
*     derived class. It then initialises a CmpRegion structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a CmpRegion at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the CmpRegion is to be initialised.
*        This must be of sufficient size to accommodate the CmpRegion data
*        (sizeof(CmpRegion)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the CmpRegion (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        CmpRegion structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the CmpRegion's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new CmpRegion.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     region1
*        Pointer to the first Region.
*     region2
*        Pointer to the second Region.
*     oper
*        The boolean operator to use. Must be one of AST__AND, AST__OR or
*        AST__XOR.

*  Returned Value:
*     A pointer to the new CmpRegion.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstCmpRegion *new;            /* Pointer to new CmpRegion */
   AstFrame *frm;                /* Frame encapsulated by first Region */
   AstFrameSet *fs;              /* FrameSet connecting supplied Regions */
   AstMapping *map;              /* Mapping between two supplied Regions */
   AstMapping *smap;             /* Simplified Mapping between two supplied Regions */
   AstRegion *new_reg1;          /* Replacement for first region */
   AstRegion *new_reg2;          /* Replacement for second region */
   AstRegion *reg1;              /* First Region to store in the CmpRegion */
   AstRegion *reg2;              /* Second Region to store in the CmpRegion */
   AstRegion *xor1;              /* Copy of first supplied Region or NULL */
   AstRegion *xor2;              /* Copy of second supplied Region or NULL */
   int i;                        /* Loop count */
   int used_oper;                /* The boolean operation actually used */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitCmpRegionVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the supplied oper value. */
   if( oper != AST__AND && oper != AST__OR && oper != AST__XOR && astOK ) {
      astError( AST__INTRD, "astInitCmpRegion(%s): Illegal "
                "boolean operator value (%d) supplied.", status, name, oper );
   }

/* Take copies of the supplied Regions. */
   reg1 = astCopy( region1 );
   reg2 = astCopy( region2 );

/* Get the Mapping from the second to the first Region. */
   fs = astConvert( reg2, reg1, "" );

/* Report an error if not possible. */
   if( fs == NULL ) {
      frm = NULL;
      if( astOK ) astError( AST__INTRD, "astInitCmpRegion(%s): No Mapping can "
                            "be found between the two supplied Regions.", status, name );

/* Otherwise, map the second Region into the Frame of the first (unless
   they are already in the same Frame). This results in both component
   Frames having the same current Frame. This current Frame is used as the
   encapsulated Frame within the parent Region structure. */
   } else {
      frm = astGetFrame( fs, AST__CURRENT );
      map = astGetMapping( fs, AST__BASE, AST__CURRENT );
      smap = astSimplify( map );
      if( !astIsAUnitMap( smap ) ) {
         new_reg2 = astMapRegion( reg2, smap, frm );
         (void) astAnnul( reg2 );
         reg2 = new_reg2;
      }
      smap = astAnnul( smap );
      map = astAnnul( map );
      fs = astAnnul( fs );
   }

/* The CmpRegion class does not implement XOR directly (as it does for
   AND and OR). Instead, when requested to create an XOR CmpRegion, it
   creates a CmpRegion that uses AND and OR to simulate XOR. The top
   level XOR CmpRegion actually uses AST__OR and the two component
   regions within it are CmpRegions formed by combing the two supplied
   Regions (one being negated first) using AND. Create the required
   component Regions. */
   if( oper == AST__XOR ) {
      astNegate( reg1 );
      new_reg1 = (AstRegion *) astCmpRegion( reg1, reg2, AST__AND, " ",
                                             status );
      astNegate( reg1 );

      astNegate( reg2 );
      new_reg2 = (AstRegion *) astCmpRegion( reg1, reg2, AST__AND, " ",
                                             status );
      astNegate( reg2 );

      xor1 = reg1;
      xor2 = reg2;

      reg1 = new_reg1;
      reg2 = new_reg2;

      used_oper = AST__OR;

/* For AND and OR, use the supplied operator. */
   } else {
      xor1 = NULL;
      xor2 = NULL;
      used_oper = oper;
   }

/* Initialise a Region structure (the parent class) as the first component
   within the CmpRegion structure, allocating memory if necessary. A NULL
   PointSet is suppled as the two component Regions will perform the function
   of defining the Region shape. The base Frame of the FrameSet in the
   parent Region structure will be the same as the current Frames of the
   FrameSets in the two component Regions. */
   if ( astOK ) {
      new = (AstCmpRegion *) astInitRegion( mem, size, 0,
                                          (AstRegionVtab *) vtab, name,
                                          frm, NULL, NULL );

/* Initialise the CmpRegion data. */
/* --------------------------- */
/* Store pointers to the component Regions. */
      new->region1 = astClone( reg1 );
      new->region2 = astClone( reg2 );

/* Note the operator used to combine the somponent Regions. */
      new->oper = used_oper;

/* If we are creating an XOR CmpRegion, save copies of the supplied
   Regions (i.e. the supplied Regions which are XORed). These will not
   be the same as "reg1" and "reg2" since each of those two regions will
   be CmpRegions that combine the supplied Regions using AST__AND. */
      if( oper == AST__XOR ) {
         new->xor1 = xor1;
         new->xor2 = xor2;
      } else {
         new->xor1 = NULL;
         new->xor2 = NULL;
      }

/* Initialised cached values to show they have not yet been found. */
      for( i = 0; i < 2; i++ ) {
         new->rvals[ i ] = NULL;
         new->offs[ i ] = NULL;
         new->nbreak[ i ] = 0;
         new->d0[ i ] = AST__BAD;
         new->dtot[ i ] = AST__BAD;
      }
      new->bounded = -INT_MAX;

/* If the base->current Mapping in the FrameSet within each component Region
   is a UnitMap, then the FrameSet does not need to be included in the
   Dump of the new CmpRegion. Set the RegionFS attribute of the component
   Region to zero to flag this. */
      map = astGetMapping( reg1->frameset, AST__BASE, AST__CURRENT );
      if( astIsAUnitMap( map ) ) astSetRegionFS( reg1, 0 );
      map = astAnnul( map );

      map = astGetMapping( reg2->frameset, AST__BASE, AST__CURRENT );
      if( astIsAUnitMap( map ) ) astSetRegionFS( reg2, 0 );
      map = astAnnul( map );

/* Copy attribute values from the first component Region to the parent
   Region. */
      if( astTestMeshSize( new->region1 ) ) {
         astSetMeshSize( new,  astGetMeshSize( new->region1 ) );
      }
      if( astTestClosed( new->region1 ) ) {
         astSetClosed( new,  astGetClosed( new->region1 ) );
      }

/* If an error occurred, clean up by annulling the Region pointers and
   deleting the new object. */
      if ( !astOK ) {
         new->region1 = astAnnul( new->region1 );
         new->region2 = astAnnul( new->region2 );
         new = astDelete( new );
      }
   }

/* Free resources */
   reg1 = astAnnul( reg1 );
   reg2 = astAnnul( reg2 );
   if( frm ) frm = astAnnul( frm );

/* Return a pointer to the new object. */
   return new;
}

AstCmpRegion *astLoadCmpRegion_( void *mem, size_t size,
                                 AstCmpRegionVtab *vtab, const char *name,
                                 AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadCmpRegion

*  Purpose:
*     Load a CmpRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpregion.h"
*     AstCmpRegion *astLoadCmpRegion( void *mem, size_t size,
*                                     AstCmpRegionVtab *vtab, const char *name,
*                                     AstChannel *channel )

*  Class Membership:
*     CmpRegion loader.

*  Description:
*     This function is provided to load a new CmpRegion using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     CmpRegion structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a CmpRegion at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the CmpRegion is to be
*        loaded.  This must be of sufficient size to accommodate the
*        CmpRegion data (sizeof(CmpRegion)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the CmpRegion (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the CmpRegion structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstCmpRegion) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new CmpRegion. If this is NULL, a pointer to
*        the (static) virtual function table for the CmpRegion class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "CmpRegion" is used instead.

*  Returned Value:
*     A pointer to the new CmpRegion.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstCmpRegion *new;            /* Pointer to the new CmpRegion */
   AstRegion *reg1;              /* First Region read from dump */
   AstRegion *reg2;              /* Second Region read from dump */
   AstFrame *f1;                 /* Base Frame in parent Region */
   AstRegion *creg;              /* Pointer to component Region */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   int i;                        /* Loop count */
   int oper;                     /* The operator to include in the dump */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this CmpRegion. In this case the
   CmpRegion belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstCmpRegion );
      vtab = &class_vtab;
      name = "CmpRegion";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitCmpRegionVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built CmpRegion. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "CmpRegion" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Operator */
/* -------- */
      oper = astReadInt( channel, "operator", AST__AND );

/* First Region. */
/* -------------- */
      reg1 = astReadObject( channel, "regiona", NULL );

/* Second Region. */
/* --------------- */
      reg2 = astReadObject( channel, "regionb", NULL );

/* Initialised cached values to show they have not yet been found. */
      for( i = 0; i < 2; i++ ) {
         new->rvals[ i ] = NULL;
         new->offs[ i ] = NULL;
         new->nbreak[ i ] = 0;
         new->d0[ i ] = AST__BAD;
         new->dtot[ i ] = AST__BAD;
      }
      new->bounded = -INT_MAX;

/* The CmpRegion class does not implement XOR directly (as it does for
   AND and OR). Instead, when requested to create an XOR CmpRegion, it
   creates a CmpRegion that uses AND and OR to simulate XOR. The top
   level XOR CmpRegion actually uses AST__OR and the two component
   regions within it are CmpRegions formed by combing the two supplied
   Regions (one being negated first) using AND. Create the required
   component Regions. */
      if( oper == AST__XOR ) {
         astNegate( reg1 );
         new->region1 = (AstRegion *) astCmpRegion( reg1, reg2, AST__AND,
                                                    " ", status );
         astNegate( reg1 );

         astNegate( reg2 );
         new->region2 = (AstRegion *) astCmpRegion( reg1, reg2, AST__AND,
                                                    " ", status );
         astNegate( reg2 );

         new->xor1 = reg1;
         new->xor2 = reg2;

         new->oper = AST__OR;

/* For AND and OR, use the supplied Regions and operator. */
      } else {
         new->region1 = reg1;
         new->region2 = reg2;
         new->xor1 = NULL;
         new->xor2 = NULL;
         new->oper = oper;
      }

/* If either component Region has a dummy FrameSet rather than the correct
   FrameSet, the correct FrameSet will have copies of the base Frame of the
   new CmpRegion as both its current and base Frames, connected by a UnitMap
   (this is equivalent to a FrameSet containing a single Frame). However if
   the new CmpRegion being loaded has itself got a dummy FrameSet, then we do
   not do this since we do not yet know what the correct FrameSet is. In this
   case we wait until the parent Region invokes the astSetRegFS method on the
   new CmpRegion. */
      if( !astRegDummyFS( new ) ) {
         f1 = astGetFrame( ((AstRegion *) new)->frameset, AST__BASE );
         creg = new->region1;
         if( astRegDummyFS( creg ) ) astSetRegFS( creg, f1 );
         creg = new->region2;
         if( astRegDummyFS( creg ) ) astSetRegFS( creg, f1 );
         f1 = astAnnul( f1 );
      }

/* If an error occurred, clean up by deleting the new CmpRegion. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new CmpRegion pointer. */
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

int astCmpRegionList_( AstCmpRegion *this, int *nreg, AstRegion ***reg_list,
                       int *status ) {
   if ( !astOK ) return AST__AND;
   return (**astMEMBER(this,CmpRegion,CmpRegionList))( this, nreg, reg_list,
                                                       status );
}






