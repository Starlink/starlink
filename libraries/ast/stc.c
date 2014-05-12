/*
*class++
*  Name:
*     Stc

*  Purpose:
*     Represents an instance of the IVOA STC class.

*  Constructor Function:
c     astStc
f     AST_STC

*  Description:
*     The Stc class is an implementation of the IVOA STC class which forms
*     part of the IVOA Space-Time Coordinate Metadata system. See:
*
*     http://hea-www.harvard.edu/~arots/nvometa/STC.html
*
*     The Stc class does not have a constructor function of its own, as it
*     is simply a container class for a family of specialised sub-classes
*     including StcCatalogEntryLocation, StcResourceProfile, StcSearchLocation
*     and StcObsDataLocation.

*  Inheritance:
*     The Stc class inherits from the Region class.

*  Attributes:
*     In addition to those attributes common to all Regions, every
*     Stc also has the following attributes:
*
*     - RegionClass: The class name of the encapsulated Region.

*  Functions:
c     In addition to those functions applicable to all Regions, the
c     following functions may also be applied to all Stc's:
f     In addition to those routines applicable to all Regions, the
f     following routines may also be applied to all Stc's:
*
c     - astGetStcRegion: Get a pointer to the encapsulated Region
f     - AST_GETSTCREGION: Get a pointer to the encapsulated Region
c     - astGetStcCoord: Get information about an AstroCoords element
f     - AST_GETSTCCOORD: Get information about an AstroCoords element
c     - astGetStcNCoord: Returns the number of AstroCoords elements in an Stc
f     - AST_GETSTCNCOORD: Returns the number of AstroCoords elements in an Stc

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     23-NOV-2004 (DSB):
*        Original version.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     13-MAR-2009 (DSB):
*        Over-ride astRegBasePick.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Stc

/* The number of components in an AstroCoords element which are described
   using a Region within a KeyMap. */
#define NREG 5

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "unitmap.h"             /* Unit Mappings */
#include "region.h"              /* Regions (parent class) */
#include "channel.h"             /* I/O channels */
#include "stc.h"                 /* Interface definition for this class */
#include "keymap.h"              /* Lists of value/key pairs */
#include "pointlist.h"           /* Individual points in a Frame */
#include "ellipse.h"             /* Ellipses within a Frame */
#include "interval.h"            /* Axis intervals within a Frame */
#include "prism.h"               /* Extrusions into higher dimensions */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstMapping *(* parent_simplify)( AstMapping *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *(* parent_getdefunc)( AstRegion *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_equal)( AstObject *, AstObject *, int * );
static int (* parent_getobjsize)( AstObject *, int * );
static int (* parent_getusedefs)( AstObject *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static void (* parent_setregfs)( AstRegion *, AstFrame *, int * );
static void (*parent_regclearattrib)( AstRegion *, const char *, char **, int * );
static void (*parent_regsetattrib)( AstRegion *, const char *, char **, int * );

static void (* parent_clearnegated)( AstRegion *, int * );
static void (* parent_clearclosed)( AstRegion *, int * );
static void (* parent_clearfillfactor)( AstRegion *, int * );
static void (* parent_clearmeshsize)( AstRegion *, int * );

static void (* parent_setclosed)( AstRegion *, int, int * );
static void (* parent_setfillfactor)( AstRegion *, double, int * );
static void (* parent_setmeshsize)( AstRegion *, int, int * );
static void (* parent_setnegated)( AstRegion *, int, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif

/* The keys associated with each component of an AstroCoords element
   within KeyMap */
static const char *regkey[ NREG ] = { AST__STCERROR,
                                      AST__STCRES,
                                      AST__STCSIZE,
                                      AST__STCPIXSZ,
                                      AST__STCVALUE };

/* The comments associated with each component of an AstroCoords element
   within KeyMap */
static const char *regcom[ NREG ] = { "AstroCoords error region",
                                      "AstroCoords resolution region",
                                      "AstroCoords size region",
                                      "AstroCoords pixel size region",
                                      "AstroCoords value region" };


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Stc)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Stc,Class_Init)
#define class_vtab astGLOBAL(Stc,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstStcVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstKeyMap *GetStcCoord( AstStc *, int, int * );
static AstKeyMap *MakeAstroCoordsKeyMap( AstRegion *, AstKeyMap *, const char *, int * );
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *GetDefUnc( AstRegion *, int * );
static AstRegion *GetStcRegion( AstStc *, int * );
static AstRegion *RegBasePick( AstRegion *this, int, const int *, int * );
static const char *GetRegionClass( AstStc *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetBounded( AstRegion *, int * );
static int GetObjSize( AstObject *, int * );
static int GetStcNCoord( AstStc *, int * );
static int GetUseDefs( AstObject *, int * );
static int Overlap( AstRegion *, AstRegion *, int * );
static int OverlapX( AstRegion *, AstRegion *, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void GetRegion( AstStc *, AstRegion **, int *, int * );
static void RegBaseBox( AstRegion *, double *, double *, int * );
static void RegClearAttrib( AstRegion *, const char *, char **, int * );
static void RegSetAttrib( AstRegion *, const char *, char **, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );

static void ClearAttrib( AstObject *, const char *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );

static void ClearClosed( AstRegion *, int * );
static int GetClosed( AstRegion *, int * );
static void SetClosed( AstRegion *, int, int * );
static int TestClosed( AstRegion *, int * );

static void ClearMeshSize( AstRegion *, int * );
static int GetMeshSize( AstRegion *, int * );
static void SetMeshSize( AstRegion *, int, int * );
static int TestMeshSize( AstRegion *, int * );

static void ClearFillFactor( AstRegion *, int * );
static double GetFillFactor( AstRegion *, int * );
static void SetFillFactor( AstRegion *, double, int * );
static int TestFillFactor( AstRegion *, int * );

static void ClearNegated( AstRegion *, int * );
static int GetNegated( AstRegion *, int * );
static void SetNegated( AstRegion *, int, int * );
static int TestNegated( AstRegion *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */
static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Stc member function (over-rides the astClearAttrib protected
*     method inherited from the Region class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Stc, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Stc.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* (none as yet) */
/* ------------- */

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute name matches any of the read-only attributes
   of this class. If it does, then report an error. */
   if ( !strcmp( attrib, "regionclass" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Not recognised. */
/* --------------- */
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
*     Test if two Objects are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int Equal( AstObject *this_object, AstObject *that_object, int *status )

*  Class Membership:
*     Stc member function (over-rides the astEqual protected
*     method inherited from the Region class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two Stcs are equivalent.

*  Parameters:
*     this
*        Pointer to the first Stc.
*     that
*        Pointer to the second Stc.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the Stcs are equivalent, zero otherwise.

*  Notes:
*     - The Stcs are equivalent if their encapsulated Region are
*     equivalent, and if they have the same boolean operation, negation
*     and closed flags.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstStc *that;
   AstStc *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the Equal method inherited from the parent Region class. This checks
   that the Objects are both of the same class, and have the same Negated
   and Closed flags (amongst other things). */
   if( (*parent_equal)( this_object, that_object, status ) ) {

/* Obtain pointers to the two Stc structures. */
      this = (AstStc *) this_object;
      that = (AstStc *) that_object;

/* Test their encapsulated Region for equality. */
      result = astEqual( this->region, that->region );
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
*     Define a function to set an attribute value for a Stc.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "stc.h"
*     MAKE_SET(attribute,lattribute,type)

*  Class Membership:
*     Defined by the Stc class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Set<Attribute>( AstRegion *this, <Type> value )
*
*     that sets the value of a specified Region attribute in the parent
*     Region structure and also in the encapsulated Region.

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
   AstStc *this;         /* Pointer to the Stc structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Use the parent method to set the value in the parent Region structure. */ \
   (*parent_set##lattribute)( this_region, value, status ); \
\
/* Also set the value in the encapsulated Region. */ \
   this = (AstStc *) this_region; \
   astSet##attribute( this->region, value ); \
}

/* Use the above macro to create accessors for the MeshSize, Closed and
   FillFactor attributes. */
MAKE_SET(FillFactor,fillfactor,double)
MAKE_SET(MeshSize,meshsize,int)
MAKE_SET(Closed,closed,int)
MAKE_SET(Negated,negated,int)

/* Undefine the macro. */
#undef MAKE_SET

/*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Define a function to clear an attribute value for a Stc.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "stc.h"
*     MAKE_CLEAR(attribute,lattribute)

*  Class Membership:
*     Defined by the Stc class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Clear<Attribute>( AstRegion *this )
*
*     that sets the value of a specified Region attribute in the parent
*     Region structure and also in the encapsulated Region.

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
   AstStc *this;         /* Pointer to the Stc structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Use the parent method to clear the value in the parent Region structure. */ \
   (*parent_clear##lattribute)( this_region, status ); \
\
/* Also clear the value in the encapsulated Region. */ \
   this = (AstStc *) this_region; \
   astClear##attribute( this->region ); \
}

/* Use the above macro to create accessors for the MeshSize, Closed and
   FillFactor attributes. */
MAKE_CLEAR(FillFactor,fillfactor)
MAKE_CLEAR(MeshSize,meshsize)
MAKE_CLEAR(Closed,closed)
MAKE_CLEAR(Negated,negated)

/* Undefine the macro. */
#undef MAKE_CLEAR


/*
*  Name:
*     MAKE_GET

*  Purpose:
*     Define a function to get an attribute value for a Stc.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "stc.h"
*     MAKE_GET(attribute,type,bad)

*  Class Membership:
*     Defined by the Stc class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static <Type> Get<Attribute>( AstRegion *this )
*
*     that gets the value of a specified Region attribute from the encapsulated
*     Region.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*     bad
*        Value to return in caseof error.
*/

/* Define the macro. */
#define MAKE_GET(attribute,type,bad) \
static type Get##attribute( AstRegion *this_region, int *status ) { \
\
/* Local Variables: */ \
   AstStc *this;         /* Pointer to the Stc structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad); \
\
/* Get the value from the encapsulated Region. */ \
   this = (AstStc *) this_region; \
   return astGet##attribute( this->region ); \
}

/* Use the above macro to create accessors for the MeshSize, Closed and
   FillFactor attributes. */
MAKE_GET(FillFactor,double,AST__BAD)
MAKE_GET(MeshSize,int,100)
MAKE_GET(Closed,int,1)
MAKE_GET(Negated,int,0)

/* Undefine the macro. */
#undef MAKE_GET

/*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Define a function to test an attribute value for a Stc.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "stc.h"
*     MAKE_TEST(attribute)

*  Class Membership:
*     Defined by the Stc class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static int Test<Attribute>( AstRegion *this )
*
*     that test the value of a specified Region attribute from the encapsulated
*     Region.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_TEST(attribute) \
static int Test##attribute( AstRegion *this_region, int *status ) { \
\
/* Local Variables: */ \
   AstStc *this;         /* Pointer to the Stc structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Test the value from the encapsulated Region. */ \
   this = (AstStc *) this_region; \
   return astTest##attribute( this->region ); \
}

/* Use the above macro to create accessors for the MeshSize, Closed and
   FillFactor attributes. */
MAKE_TEST(FillFactor)
MAKE_TEST(MeshSize)
MAKE_TEST(Closed)
MAKE_TEST(Negated)

/* Undefine the macro. */
#undef MAKE_TEST




static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     Stc member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied Stc,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Stc.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstStc *this;         /* Pointer to Stc structure */
   int result;           /* Result value to return */
   int i;                /* AstroCoords index */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the Stc structure. */
   this = (AstStc *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astGetObjSize( this->region );

   if( this->coord ) {
      for( i = 0; i < this->ncoord; i++ ) {
         result += astGetObjSize( this->coord[ i ] );
      }
      result += astTSizeOf( this->coord );
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
*     Get the value of a specified attribute for a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Stc member function (over-rides the protected astGetAttrib
*     method inherited from the Region class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Stc, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Stc.
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
*     within the Stc, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Stc. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   const char *result;           /* Pointer value to return */
   int len;                      /* Length of attrib string */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* RegionClass. */
/* ------------ */
   if ( !strcmp( attrib, "regionclass" ) ) {
      result = astGetClass( this->region );

/* Not recognised. */
/* --------------- */
/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;
}

static int GetBounded( AstRegion *this_region, int *status ) {
/*
*  Name:
*     GetBounded

*  Purpose:
*     Is the Region bounded?

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int GetBounded( AstRegion *this, int *status )

*  Class Membership:
*     Stc method (over-rides the astGetBounded method inherited from
*     the Region class).

*  Description:
*     This function returns a flag indicating if the Region is bounded.
*     The implementation provided by the base Region class is suitable
*     for Region sub-classes representing the inside of a single closed
*     curve (e.g. Circle, Ellipse, Box, etc). Other sub-classes (such as
*     Stc, PointList, etc ) may need to provide their own implementations.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the Region is bounded. Zero otherwise.

*/

/* Local Variables: */
   AstStc *this;              /* Pointer to Stc structure */
   AstRegion *reg;            /* Pointer to the encapsulated Region */
   int neg;                   /* Negated flag to use */
   int neg_old;               /* Original Negated flag */
   int result;                /* Returned result */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Stc structure. */
   this = (AstStc *) this_region;

/* Get the encapsulated Region, and the Negated value which should be used
   with it. The returned values take account of whether the supplied Stc has
   itself been Negated or not. The returned Region represent a region within
   the base Frame of the FrameSet encapsulated by the parent Region
   structure. */
   GetRegion( this, &reg, &neg, status );

/* Temporarily set the Negated attribute to the required value.*/
   neg_old = astGetNegated( reg );
   astSetNegated( reg, neg );

/* See if the encapsulated Region is bounded. */
   result = astGetBounded( reg );

/* Re-instate the original value for the Negated attribute of the
   encapsulated Region. */
   if( reg ) astSetNegated( reg, neg_old );

/* Free resources. */
   reg = astAnnul( reg );

/* Return zero if an error occurred. */
   if( !astOK ) result = 0;

/* Return the required pointer. */
   return result;
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
*     #include "stc.h"
*     AstRegion *GetDefUnc( AstRegion *this )

*  Class Membership:
*     Stc method (over-rides the astGetDefUnc method inherited from
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
   AstStc *this;              /* Pointer to the Stc structure */
   AstRegion *result;         /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Stc structure. */
   this = (AstStc *) this_region;

/* If the encapsulated region has non-default uncertainty, use it as
   the default uncertainty for the Cmpregion. Note, the current Frame of
   an uncertainty Region is assumed to be the same as the base Frame in the
   Stc. */
   if( astTestUnc( this->region ) ) {
      result = astGetUncFrm( this->region, AST__CURRENT );

/* Otherwise, use the parent method to determine the default uncertainty. */
   } else {
      result = (* parent_getdefunc)( this_region, status );
   }

/* Return NULL if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the required pointer. */
   return result;
}

static void GetRegion( AstStc *this, AstRegion **reg, int *neg, int *status ) {
/*
*
*  Name:
*     GetRegion

*  Purpose:
*     Get the encapsulated Region of a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "region.h"
*     void GetRegion( AstStc *this, AstRegion **reg, int *neg, int *status )

*  Class Membership:
*     Stc member function

*  Description:
*     This function returns a pointer to a Region which is equivalent to
*     the supplied Stc. If the Stc has been negated, then the returned
*     "negated" flag will be set such that it represents the negated Stc.
*
*     The current Frames in returned encapsulated Region will be equivalent
*     to the base Frame in the FrameSet encapsulated by the parent Region
*     structure.

*  Parameters:
*     this
*        Pointer to the Stc.
*     reg
*        Address of a location to receive a pointer to the encapsulated
*        Region. The current Frame in this region will be equivalent to
*        the base Frame in the FrameSet
*     neg
*        The value of the Negated attribute to be used with reg.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Any changes made to the encapsulated Region using the returned
*     pointer will be reflected in the supplied Stc.

*/

/* Initialise */
   if( reg ) *reg = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Return the component Region pointers. */
   if( reg ) *reg = astClone( this->region );

/* Initialise the other returned items. Note, the Stc initialiser stored a
   deep copy of the supplied encapsulated Region, and so we do not
   need to worry about attributes of the Region having been changed
   after the creation of the Stc. This is different to the CmpMap
   class which merely clones its supplied component pointers and so has
   to save copies of the original Invert settings within the CmpMap
   structure. */
   if( neg ) *neg = astGetNegated( this->region );

/* If the Stc has been inverted, we modify the boolean operator and
   negation flags so that they reflect the inverted Stc. */
   if( astGetNegated( this ) && neg ) *neg = *neg ? 0 : 1;
}

static const char *GetRegionClass( AstStc *this, int *status ){
/*
*+
*  Name:
*     astGetRegionClass

*  Purpose:
*     Get the value of a RegionClass attribute for a Stc.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stc.h"
*     const char *astGetRegionClass( AstStc *this )

*  Class Membership:
*     Stc virtual function

*  Description:
*     This function returns a pointer to the value of the RegionClass
*     attribute for a Stc.

*  Parameters:
*     this
*        Pointer to the Stc.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Stc, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Stc. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain and return the class of the encapsulated Region. */
   return astGetClass( ((AstStc *) this)->region );
}


static AstKeyMap *GetStcCoord( AstStc *this, int icoord, int *status ){
/*
*++
*  Name:
c     astGetStcCoord
f     AST_GETSTCCOORD

*  Purpose:
*     Return information about an AstroCoords element stored in an Stc.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "specframe.h"
c     AstKeyMap *astGetStcCoord( AstStc *this, int icoord )
f     RESULT = AST_GETSTCCOORD( THIS, ICOORD, STATUS )

*  Class Membership:
*     Stc method.

*  Description:
*     When any sub-class of Stc is created, the constructor function
*     allows one or more AstroCoords elements to be stored within the Stc.
*     This function allows any one of these AstroCoords elements to be
*     retrieved. The format of the returned information is the same as
*     that used to pass the original information to the Stc constructor.
*     That is, the information is returned in a KeyMap structure
*     containing elements with one or more of the keys given by symbolic
*     constants AST__STCNAME, AST__STCVALUE, AST__STCERROR, AST__STCRES,
*     AST__STCSIZE and AST__STCPIXSZ.
*
*     If the coordinate system represented by the Stc has been changed
*     since it was created (for instance, by changing its System
*     attribute), then the sizes and positions in the returned KeyMap
*     will reflect the change in coordinate system.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Stc.
c     icoord
f     ICOORD = INTEGER (Given)
*        The index of the AstroCoords element required. The first has index
*        one. The number of AstroCoords elements in the Stc can be found using
c        function astGetStcNcoord.
f        function AST_GETSTCNCOORD.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetStcCoord()
f     AST_GETSTCCOORD = INTEGER
*        A pointer to a new KeyMap containing the required information.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstFrame *frm;
   AstKeyMap *result;
   AstMapping *map;
   AstMapping *smap;
   AstObject *obj;
   AstRegion *reg;
   AstRegion *rereg;
   AstRegion *srereg;
   int ikey;
   int nc;

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the supplied index. */
   nc = astGetStcNCoord( this );
   if( icoord < 1 || icoord > nc ) {
      astError( AST__STCIND, "astGetStcCoord(%s): Supplied AstroCoords "
                "index (%d) is invalid.", status, astGetClass( this ), icoord );

      if( icoord < 1 ) {
         astError( AST__STCIND, "The index of the first AstroCoord "
                   "element is one, not zero." , status);
      } else if( nc == 0 ) {
         astError( AST__STCIND, "There are no AstroCoords elements in "
                   "the supplied %s.", status, astGetClass( this ) );
      } else if( nc == 1 ) {
         astError( AST__STCIND, "There is 1 AstroCoords element in "
                   "the supplied %s.", status, astGetClass( this ) );
      } else {
         astError( AST__STCIND, "There are %d AstroCoords elements in "
                   "the supplied %s.", status, nc, astGetClass( this ) );
      }

/* If the index is OK, initialise the returned KeyMap to be a copy of the
   KeyMap holding information about the required AstroCoords element.*/
   } else {
      result = astCopy( this->coord[ icoord - 1 ] );

/* The Regions stored within this KeyMap describe regions within the base
   Frame of the parent Region structure. If the Mapping from base to current
   Frame in the parent Region structure is not a UnitMap, we need to
   change these to represent regions within the current Frame of the
   parent Region structure. */
      map = astGetMapping( ((AstRegion *)this)->frameset,
                           AST__BASE, AST__CURRENT );
      smap = astSimplify( map );
      frm = astGetFrame( ((AstRegion *)this)->frameset, AST__CURRENT );

/* If the Frame represented by the Region has changed, erase the Names
   element since they may no longer be correct. */
      if( !astIsAUnitMap( smap ) ) astMapRemove( result, AST__STCNAME );

/* Loop round keys for which a Region may be stored in the KeyMap. */
      for( ikey = 0; ikey < NREG; ikey++ ) {

/* If the KeyMap contains a Region for this key, get a pointer to it. */
         if( astMapGet0A( result, regkey[ ikey ], &obj ) ){
            reg = (AstRegion *) obj;

/* Sets its RegionFS attribute so that the encapsulated FrameSet will be
   included in any dump of the Region. This is needed since the returned
   Region pointer will have no parent Region from which the FrameSet can
   be determined. */
            astSetRegionFS( reg, 1 );

/* If necessary, remap the Region into the current Frame, and simplify. */
            if( !astIsAUnitMap( smap ) ) {
               rereg = astMapRegion( reg, smap, frm );
               srereg = astSimplify( rereg );
               rereg = astAnnul( rereg );
            } else {
               srereg = astClone( reg );
            }

/* Replace the Region in the KeyMap with the remapped Region. */
            astMapPut0A( result, regkey[ ikey ], srereg, NULL );

/* Free resources */
            reg = astAnnul( reg );
            srereg = astAnnul( srereg );
         }
      }

      frm = astAnnul( frm );
      map = astAnnul( map );
      smap = astAnnul( smap );

/* Annul the returned KeyMap if an error has occurred. */
      if( !astOK ) result = astAnnul( result );
   }

/* Return the pointer */
   return result;

}

static int GetStcNCoord( AstStc *this, int *status ){
/*
*++
*  Name:
c     astGetStcNCoord
f     AST_GETSTCNCOORD

*  Purpose:
*     Return the number of AstroCoords elements stored in an Stc.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "stc.h"
c     int astGetStcNCoord( AstStc *this )
f     RESULT = AST_GETSTCNCOORD( THIS, STATUS )

*  Class Membership:
*     Stc method.

*  Description:
*     This function returns the number of AstroCoords elements stored in
*     an Stc.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Stc.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetStcNCoord()
f     AST_GETSTCNCOORD = INTEGER
*        The number of  AstroCoords elements stored in the Stc.

*  Notes:
*     - Zero will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Return the required value. */
   return astOK ? this->ncoord : 0;

}

static AstRegion *GetStcRegion( AstStc *this, int *status ) {
/*
*++
*  Name:
c     astGetStcRegion
f     AST_GETSTCREGION

*  Purpose:
*     Obtain a copy of the encapsulated Region within a Stc.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "stc.h"
c     AstRegion *astGetStcRegion( AstStc *this )
f     RESULT = AST_GETSTCREGION( THIS, STATUS )

*  Class Membership:
*     Region method.

*  Description:
*     This function returns a pointer to a deep copy of the Region
*     supplied when the Stc was created.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Stc.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetStcRegion()
f     AST_GETSTCREGION = INTEGER
*        A pointer to a deep copy of the Region encapsulated within the
*        supplied Stc.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Return a pointer to a copy of the encapsulated Region. */
   return astCopy( this->region );
}

static int GetUseDefs( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetUseDefs

*  Purpose:
*     Get the value of the UseDefs attribute for a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int GetUseDefs( AstObject *this_object, int *status ) {

*  Class Membership:
*     Stc member function (over-rides the protected astGetUseDefs
*     method inherited from the Region class).

*  Description:
*     This function returns the value of the UseDefs attribute for a
*     Stc, supplying a suitable default.

*  Parameters:
*     this
*        Pointer to the Stc.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The USeDefs value.
*/

/* Local Variables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   int result;                   /* Value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* If the UseDefs value for the Stc has been set explicitly, use the
   Get method inherited from the parent Region class to get its value. */
   if( astTestUseDefs( this ) ) {
      result = (*parent_getusedefs)( this_object, status );

/* Otherwise, supply a default value equal to the UseDefs value of the
   encapsulated Region. */
   } else {
      result = astGetUseDefs( this->region );
   }

/* Return the result. */
   return result;
}

void astInitStcVtab_(  AstStcVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitStcVtab

*  Purpose:
*     Initialise a virtual function table for a Stc.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stc.h"
*     void astInitStcVtab( AstStcVtab *vtab, const char *name )

*  Class Membership:
*     Stc vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Stc class.

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
   AstRegionVtab *region;        /* Pointer to Region component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitRegionVtab( (AstRegionVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAStc) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */

   vtab->GetRegionClass = GetRegionClass;
   vtab->GetStcRegion = GetStcRegion;
   vtab->GetStcCoord = GetStcCoord;
   vtab->GetStcNCoord = GetStcNCoord;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   region = (AstRegionVtab *) vtab;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

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

   parent_simplify = mapping->Simplify;
   mapping->Simplify = Simplify;

   parent_setregfs = region->SetRegFS;
   region->SetRegFS = SetRegFS;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_clearclosed = region->ClearClosed;
   region->ClearClosed = ClearClosed;

   parent_setclosed = region->SetClosed;
   region->SetClosed = SetClosed;

   region->TestClosed = TestClosed;
   region->GetClosed = GetClosed;

   parent_regsetattrib = region->RegSetAttrib;
   region->RegSetAttrib = RegSetAttrib;

   parent_regclearattrib = region->RegClearAttrib;
   region->RegClearAttrib = RegClearAttrib;

   parent_clearnegated = region->ClearNegated;
   region->ClearNegated = ClearNegated;

   parent_setnegated = region->SetNegated;
   region->SetNegated = SetNegated;

   region->TestNegated = TestNegated;
   region->GetNegated = GetNegated;

   parent_setmeshsize = region->SetMeshSize;
   region->SetMeshSize = SetMeshSize;

   parent_clearmeshsize = region->ClearMeshSize;
   region->ClearMeshSize = ClearMeshSize;

   region->TestMeshSize = TestMeshSize;
   region->GetMeshSize = GetMeshSize;

   parent_setfillfactor = region->SetFillFactor;
   region->SetFillFactor = SetFillFactor;

   parent_clearfillfactor = region->ClearFillFactor;
   region->ClearFillFactor = ClearFillFactor;

   region->TestFillFactor = TestFillFactor;
   region->GetFillFactor = GetFillFactor;

   parent_getusedefs = object->GetUseDefs;
   object->GetUseDefs = GetUseDefs;

   parent_getdefunc = region->GetDefUnc;
   region->GetDefUnc = GetDefUnc;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   region->Overlap = Overlap;
   region->OverlapX = OverlapX;
   region->RegBaseBox = RegBaseBox;
   region->RegBaseMesh = RegBaseMesh;
   region->RegBasePick = RegBasePick;
   region->RegPins = RegPins;
   region->GetBounded = GetBounded;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "Stc", "An IVOA Space-Time-Coords object" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static AstKeyMap *MakeAstroCoordsKeyMap( AstRegion *reg, AstKeyMap *coord,
                                         const char *class, int *status ){
/*
*  Name:
*     MakeAstroCoordsKeyMap

*  Purpose:
*     Create a new KeyMap holding Regions describing a supplied
*     AstroCoords element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     AstKeyMap *MakeAstroCoordsKeyMap( AstRegion *reg, AstKeyMap *coord,
*                                       const char *class, int *status )

*  Class Membership:
*     Stc member function

*  Description:
*     This function returns a pointer to a new KeyMap containing elements
*     which correspond to the components of an STC AstroCoords element.
*     The element with key AST__STCNAME holds a vector of character
*     strings containing the names associated with each of the axies.
*     The other elements of the returned KeyMap such as AST__STCERROR,
*     AST__STCRES, etc, hold pointers to Regions describing the error
*     box, resolution, etc, in the Frame of the supplied Region "reg".

*  Parameters:
*     reg
*        Pointer to the Region in which the AstroCoords is defined.
*     coordId
*        An ID (not a pointer) to a KeyMap defining a single <AstroCoords>
*        element, having elements with keys given by constants AST__STCNAME,
*        AST__STCVALUE, AST__STCERROR, AST__STCRES, AST__STCSIZE,
*        AST__STCPIXSZ. Any of these elements may be omitted, but no other
*        elements should be included. If supplied, the AST__STCNAME element
*        should be a vector of character string pointers holding the "Name"
*        item for each axis. Any other supplied elements should be scalar
*        elements, each  holding a pointer to a Region describing the
*        associated item of ancillary information (error, resolution, size,
*        pixel size or value). These Regions should refer to the coordinate
*        system represented by "region".
*     class
*        Pointer to a string holding the STC class name.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to the new KeyMap.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *frm;                /* Pointer to current Frame */
   AstFrameSet *fs;              /* Pointer to conversion FrameSet */
   AstKeyMap *result;            /* Pointer value to return */
   AstMapping *map;              /* Pointer to conversion Mapping */
   AstObject *obj;               /* Pointer to Object stored in supplied KeyMap */
   AstRegion *areg;              /* Pointer to remapped Region */
   AstRegion *sareg;             /* Pointer to simplified remapped Region */
   const char *key;              /* Current key */
   int j;                        /* Index of key within KeyMap */
   int naxes;                    /* Number of axes in region */
   int nkey;                     /* Number of keys in supplied KeyMap */
   int nv;                       /* Number of values in KeyMap element */
   int type;                     /* Data type of entry */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Confirm it is a genuine KeyMap pointer. */
   if( !astIsAKeyMap( coord ) && astOK ) {
      astError( AST__STCKEY, "astInitStc(%s): Supplied pointer is for "
                "a %s, not a KeyMap.", status, class, astGetClass( coord ) );
   }

/* Initialise the new KeyMap to be a copy of the supplied KeyMap. */
   result = astCopy( coord );

/* Check the supplied KeyMap is usable. */
   naxes = astGetNaxes( reg );
   nkey = astMapSize( result );
   for( j = 0; j < nkey; j++ ) {
      key = astMapKey( result, j );
      if( key ) {
         nv = astMapLength( result, key );
         type = astMapType( result, key );

/* Check no unknown keys are present in the KeyMap. */
         if( strcmp( key, AST__STCNAME ) &&
             strcmp( key, AST__STCVALUE ) &&
             strcmp( key, AST__STCERROR ) &&
             strcmp( key, AST__STCRES ) &&
             strcmp( key, AST__STCSIZE ) &&
             strcmp( key, AST__STCPIXSZ ) ) {
            astError( AST__STCKEY, "astInitStc(%s): Unknown key "
                      "\"%s\" supplied in an AstroCoords list.", status,
                      class, key );
            break;

/* Check that the "Name" element is a vector of "naxes" strings. */
         } else if( !strcmp( key, AST__STCNAME ) ) {
            if( nv != naxes ) {
               astError( AST__STCKEY, "astInitStc(%s): %d \"%s\" "
                         "values supplied in an AstroCoords list, but "
                         "the Stc has %d axes. ", status, class, nv, key,
                         naxes );
               break;

            } else if( type != AST__STRINGTYPE ) {
               astError( AST__STCKEY, "astInitStc(%s): The \"%s\" "
                         "values supplied in an AstroCoords list are "
                         "not character strings. ", status, class, key );
               break;
            }

/* Check that all other elements are scalar. */
         } else if( nv != 1 ) {
            astError( AST__STCKEY, "astInitStc(%s): %d \"%s\" "
                      "values supplied in an AstroCoords list, but "
                      "only one is allowed. ", status, class, nv, key );
            break;

/* Check that all other elements are AST Object pointers. */
         } else if( type != AST__OBJECTTYPE ) {
            astError( AST__STCKEY, "astInitStc(%s): The \"%s\" "
                      "value supplied in an AstroCoords list is "
                      "not an AST Object pointer. ", status, class, key );
            break;

/* Check that the Object pointers are not NULL. */
         } else {
            astMapGet0A( result, key, &obj );
            if( astOK ) {
               if( !obj ) {
                  astError( AST__STCKEY, "astInitStc(%s): The \"%s\" "
                            "value supplied in an AstroCoords list is "
                            "a NULL pointer. ", status, class, key );
                  break;

/* Check that the Object pointers are Region pointers. */
               } else if( !astIsARegion( obj ) ){
                  astError( AST__STCKEY, "astInitStc(%s): The \"%s\" "
                            "value supplied in an AstroCoords list is "
                            "a %s, not a Region. ", status, class, key,
                            astGetClass(obj) );
                  obj = astAnnul( obj );
                  break;

/* Check that the Region pointers can be converted to the coordinate
   system represented by the supplied Region. */
               } else {
                  fs = astConvert( obj, reg, "" );
                  if( !fs ) {
                     obj = astAnnul( obj );
                     astError( AST__STCKEY, "astInitStc(%s): The \"%s\" "
                               "value supplied in an AstroCoords list "
                               "cannot be converted to the coordinate "
                               "system of its parent Stc object.", status, class,
                               key );
                     break;

/* If necessary, map the Region into the same frame as the supplied
   Region, and replace the Region in the returned KeyMap with the
   remapped Region. Also set the RegionFS attribute to indicate that the
   FrameSet in the Region does not need to be dumped if it contains a
   UnitMap. */
                  } else {
                     map = astGetMapping( fs, AST__BASE, AST__CURRENT );
                     if( !astIsAUnitMap( map ) ) {
                        frm = astGetFrame( fs, AST__CURRENT );
                        areg = astMapRegion( (AstRegion *) obj, map, frm );
                        sareg = astSimplify( areg );
                        astSetRegionFS( sareg, 0 );
                        astMapPut0A( result, key, sareg, NULL );
                        areg = astAnnul( areg );
                        sareg = astAnnul( sareg );
                        frm = astAnnul( frm );
                     } else {
                        astSetRegionFS( (AstRegion *) obj, 0 );
                     }
                     map = astAnnul( map );
                     fs = astAnnul( fs );

                  }
                  obj = astAnnul( obj );
               }
            }
         }
      }
   }

/* Free the returned KeyMap if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;

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
*     Stc member function (over-rides the astManageLock protected
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
   AstStc *this;       /* Pointer to STC structure */
   int i;              /* Loop count */
   int result;         /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the STC structure. */
   this = (AstStc *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->region, mode, extra, fail );
   for( i = 0; i < this->ncoord; i++ ) {
      if( !result ) result = astManageLock( this->coord[ i ], mode,
                                            extra, fail );
   }

   return result;

}
#endif

static int Overlap( AstRegion *this, AstRegion *that, int *status ){
/*
*  Name:
*     Overlap

*  Purpose:
*     Test if two regions overlap each other.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int Overlap( AstRegion *this, AstRegion *that, int *status )

*  Class Membership:
*     Stc member function (over-rides the astOverlap method inherited
*     from the Region class).

*  Description:
*     This function returns an integer value indicating if the two
*     supplied Regions overlap. The two Regions are converted to a commnon
*     coordinate system before performing the check. If this conversion is
*     not possible (for instance because the two Regions represent areas in
*     different domains), then the check cannot be performed and a zero value
*     is returned to indicate this.

*  Parameters:
*     this
*        Pointer to the first Region.
*     that
*        Pointer to the second Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     astOverlap()
*        A value indicating if there is any overlap between the two Regions.
*        Possible values are:
*
*        0 - The check could not be performed because the second Region
*            could not be mapped into the coordinate system of the first
*            Region.
*
*        1 - There is no overlap between the two Regions.
*
*        2 - The first Region is completely inside the second Region.
*
*        3 - The second Region is completely inside the first Region.
*
*        4 - There is partial overlap between the two Regions.
*
*        5 - The Regions are identical.
*
*        6 - The second Region is the negation of the first Region.

*  Notes:
*     - The returned values 5 and 6 do not check the value of the Closed
*     attribute in the two Regions.
*     - A value of zero will be returned if this function is invoked with the
*     AST error status set, or if it should fail for any reason.

*/

/* Check the inherited status. */
   if ( !astOK ) return 0;

/* Invoke the "astOverlap" method on the encapsulated Region. */
   return astOverlap( ((AstStc *)this)->region, that );
}

static int OverlapX( AstRegion *that, AstRegion *this, int *status ){
/*
*  Name:
*     OverlapX

*  Purpose:
*     Test if two regions overlap each other.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int OverlapX( AstRegion *that, AstRegion *this )

*  Class Membership:
*     Stc member function (over-rides the astOverlapX method inherited
*     from the Region class).

*  Description:
*     This function performs the processing for the public astOverlap
*     method and has exactly the same interface except that the order
*     of the two arguments is swapped. This is a trick to allow
*     the astOverlap method to be over-ridden by derived classes on
*     the basis of the class of either of its two arguments.
*
*     See the astOverlap method for details of the interface.

*/

/* Local Variables: */
   int result;

/* Check the inherited status. */
   if ( !astOK ) return 0;

/* Invoke the "astOverlapX" method on the encapsulated Region. */
   result = astOverlap( ((AstStc *)that)->region, this );

/* Swap the returned values 2 and 3 to take account of the swapping of
   the regions.*/
   if( result == 2 ) {
      result = 3;
   } else if( result == 3 ) {
      result = 2;
   }

/* Return the result. */
   return result;
}

static void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status ){
/*
*  Name:
*     RegBaseBox

*  Purpose:
*     Returns the bounding box of an un-negated Region in the base Frame of
*     the encapsulated FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     Stc member function (over-rides the astRegBaseBox protected
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

/* Check the global error status. */
   if( !astOK ) return;

/* Invoke the method on the encapsulated Region. */
   astRegBaseBox( ((AstStc *)this)->region, lbnd, ubnd );
}

static AstPointSet *RegBaseMesh( AstRegion *this, int *status ){
/*
*  Name:
*     RegBaseMesh

*  Purpose:
*     Create a new PointSet containing a mesh of points on the boundary of a
*     Region in its base Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     Stc member function (over-rides the astRegBaseMesh protected
*     method inherited from the Region class).

*  Description:
*     This function creates a new PointSet containing a mesh of points on the
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

/* Check the global error status. */
   if( !astOK ) return NULL;

/* Invoke the astRegMesh method on the encapsulated Region. This returns
   a mesh in the current Frame of the encapsulated Region which is the same
   as the base Frame of the Stc Region. */
   return astRegMesh( ((AstStc *)this)->region );
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
*     #include "stc.h"
*     AstRegion *RegBasePick( AstRegion *this, int naxes, const int *axes,
*                             int *status )

*  Class Membership:
*     Stc member function (over-rides the astRegBasePick protected
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

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Invoke the astRegBaePick method on the encapsulated Region. */
   return astRegBasePick( ((AstStc *)this_region)->region, naxes, axes );
}

static void RegClearAttrib( AstRegion *this_region, const char *attrib,
                            char **base_attrib, int *status ) {
/*
*  Name:
*     RegClearAttrib

*  Purpose:
*     Clear an attribute value for a Region.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stc.h"
*     void RegClearAttrib( AstRegion *this, const char *attrib,
*                          char **base_attrib, int *status )

*  Class Membership:
*     Stc method (over-rides the astRegClearAttrib method inherited from
*     the Region class).

*  Description:
*     This function clears the value of an attribute in both the base and
*     current Frame in the FrameSet encapsulated within a Region, without
*     remapping either Frame.
*
*     No error is reported if the attribute is not recognised by the base
*     Frame.

*  Parameters:
*     this
*        Pointer to the Region.
*     attrib
*        Pointer to a null terminated string containing an attribute name.
*        NOTE, IT SHOULD BE ENTIRELY LOWER CASE.
*     base_attrib
*        Address of a location at which to return a pointer to the null
*        terminated string holding the name of the attribute which was
*        cleared in the base Frame of the encapsulated FrameSet. This may
*        differ from the supplied name if the supplied name contains an axis
*        index and the current->base Mapping in the FrameSet produces an
*        axis permutation. The returned pointer should be freed using
*        astFree when no longer needed. A NULL pointer may be supplied in
*        which case no pointer is returned.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstStc *this;
   char *batt;
   int rep;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Stc  structure. */
   this = (AstStc *) this_region;

/* Use the RegClearAttrib method inherited from the parent class to clear
   the attribute in the current and base Frames in the FrameSet encapsulated
   by the parent Region structure. */
   (*parent_regclearattrib)( this_region, attrib, &batt, status );

/* Now clear the base Frame attribute in the encapsulated Region (the current
   Frame within the encapsulated Region is equivalent to the base Frame in the
   parent Region structure). Annul any "attribute unknown" error that results
   from attempting to do this. */
   if( astOK ) {
      rep = astReporting( 0 );
      astRegClearAttrib( this->region, batt, NULL );
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

static int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
                    int **mask, int *status ){
/*
*  Name:
*     RegPins

*  Purpose:
*     Check if a set of points fall on the boundary of a given Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     Stc member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given Stc.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied Stc "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the Stc.
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

/* Check the global error status. */
   if( !astOK ) return 0;

/* Invoke the method on the encapsulated Region. */
   return astRegPins( ((AstStc *)this)->region, pset, unc, mask );
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
*     #include "stc.h"
*     void RegSetAttrib( AstRegion *this, const char *setting,
*                        char **base_setting, int *status )

*  Class Membership:
*     Stc method (over-rides the astRegSetAttrib method inherited from
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
   AstKeyMap *keymap;
   AstObject *obj;
   AstRegion *reg;
   AstStc *this;
   char *bset;
   int i;
   int ikey;
   int rep;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Stc structure. */
   this = (AstStc *) this_region;

/* Use the RegSetAttrib method inherited from the parent class to apply the
   setting to the current and base Frames in the FrameSet encapsulated by the
   parent Region structure. */
   (*parent_regsetattrib)( this_region, setting, &bset, status );

/* Now apply the base Frame setting to the encapsulated Region (the current
   Frame within the encapsulated Region is equivalent to the base Frame in the
   parent Region structure). Annul any "attribute unknown" error that results
   from attempting to do this. Also do any AstroCoords in the Stc. */
   if( astOK ) {
      rep = astReporting( 0 );
      astRegSetAttrib( this->region, bset, NULL );
      if( astStatus == AST__BADAT ) astClearStatus;

/* Loop round all AstroCoords elements. */
      for( i = 0; i < this->ncoord; i++ ) {

/* Get a pointer to the KeyMap holding a description of the current
   AstroCoords element. */
         keymap = this->coord[ i ];

/* Loop round all the elements of this KeyMap which may hold a Region
   pointer. */
         for( ikey = 0; ikey < NREG; ikey++ ) {

/* If the KeyMap contains a Region for this key, get a pointer to it. */
            if( astMapGet0A( keymap, regkey[ ikey ], &obj ) ){
               reg = (AstRegion *) obj;

/* Modify it by applying the attribute setting. */
               astRegSetAttrib( reg, bset, NULL );
               if( astStatus == AST__BADAT ) astClearStatus;

/* Annul the pointer. */
               reg = astAnnul( reg );
            }
         }
      }

      astReporting( rep );
   }

/* If required, return the base Frame setting string, otherwise free it. */
   if( base_setting ) {
      *base_setting = bset;
   } else {
      bset = astFree( bset );
   }
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     Stc member function (over-rides the astSetAttrib method inherited
*     from the Region class).

*  Description:
*     This function assigns an attribute value for a Stc, the
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
*        Pointer to the Stc.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Vaiables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* (none as yet) */

/* Read-only attributes. */
/* --------------------- */
/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   if ( MATCH( "regionclass" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status ) {
/*
*  Name:
*     SetRegFS

*  Purpose:
*     Stores a new FrameSet in a Region

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     Stc method (over-rides the astSetRegFS method inherited from
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
   AstRegion *creg;        /* Pointer to encapsulated Region structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the parent method to store the FrameSet in the parent Region
   structure. */
   (* parent_setregfs)( this_region, frm, status );

/* If the encapsulated Region has a dummy FrameSet use this method
   recursively to give it the same FrameSet. */
   creg = ((AstStc *) this_region )->region;
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
*     Stc method (over-rides the astSimplify method inherited from
*     the Region class).

*  Description:
*     This function simplifies a Stc to eliminate redundant
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
*/

/* Local Variables: */
   AstFrame *frm;                /* Current Frame */
   AstKeyMap *keymap;            /* KeyMap holding stroCoords element */
   AstMapping *map;              /* Base->current Mapping */
   AstObject *obj;               /* Pointer to object retrieved from keymap */
   AstRegion *newreg;            /* New encapsulated Region */
   AstRegion *reg;               /* AstroCoords Region pointer */
   AstRegion *treg;              /* Temporary Region pointer */
   AstStc *stc;                  /* Returned Stc Structure. */
   AstStc *temp;                 /* Temporary Stc pointer */
   int i;                        /* Index of current AstroCoords element */
   int ikey;                     /* Index of key to be tested */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Invoke the Simplify method of the parent Region class. This simplifies
   the FrameSet and uncertainty Region in the parent Region structure. */
   stc = (AstStc *) (AstRegion *) (* parent_simplify)( this_mapping, status );

/* If the Stc is negated, we can perform a simplication by transferring
   the negated state from the Stc itself to the encapsulated Region. */
   if( astGetNegated( stc ) ) {

/* Ensure that modifying "stc" will not modify the supplied Stc, by
   creating a copy of the supplied Stc, if this has not already been done. */
      if( stc == (AstStc *) this_mapping ) {
         temp = (AstStc *) astCopy( stc );
         (void) astAnnul( stc );
         stc = temp;
      }

/* Modify "temp" by negating both the Stc structure and its encapsulated
   Region. */
      astNegate( stc );
      astNegate( stc->region );

   }

/* Get the base->current Mapping from the parent Region structure, and
   the current Frame. */
   map = astGetMapping( ((AstRegion *) stc)->frameset, AST__BASE, AST__CURRENT );
   frm = astGetFrame( ((AstRegion *) stc)->frameset, AST__CURRENT );

/* We may be able to perform some more simplication on the encapsulated
   Region itself. If the above mapping is not a unit map, remap the
   encapsulated Region into the current Frame of the parent Region structure
   and simplify it. This transfers complication from the Mapping in the
   parent Region structure to the encapsulated Region. */
   if( !astIsAUnitMap( map ) ) {
      treg = astMapRegion( stc->region, map, frm );
      newreg = astSimplify( treg );
      treg = astAnnul( treg );

/* If the base->current Mapping in the parent Region structure is a unit
   map, simplification of the whole Stc is possible if the encapsulated
   Region (without any remapping) can be simplied. */
   } else {
      newreg = astSimplify( stc->region );
   }

/* If the encapsulated Region has been changed, store it in the returned
   Stc. */
   if( newreg != stc->region ) {

/* Ensure that modifying "stc" will not modify the supplied Stc, by
   creating a copy of the supplied Stc, if this has not already been done. */
      if( stc == (AstStc *) this_mapping ) {
         temp = (AstStc *) astCopy( stc );
         (void) astAnnul( stc );
         stc = temp;
      }

/* Store the new region in "stc", annulling the existing Region. */
      if( stc ) {
         (void) astAnnul( stc->region );
         stc->region = astClone( newreg );
      }

/* The encapsulated Region now represents an area in the current Frame
   represented by the supplied Stc. Since the encapsulated Region is
   defined as being in the base Frame of the FrameSet in the parent
   Region structure, the parent FrameSet should just be a UnitMap. Modify
   it appropriately (if it not already a UnitMap). */
      if( !astIsAUnitMap( map ) ) astSetRegFS( stc, frm );
   }

/* Free resources */
   newreg = astAnnul( newreg );

/* Now we do a similar process on any Regions held within an AstroCoords
   elements. Loop round all AstroCoords elements. */
   if( stc ) {
      for( i = 0; i < stc->ncoord; i++ ) {

/* Get a pointewr to the KeyMap holding a description of the current
   AstroCoords element. */
         keymap = stc->coord[ i ];

/* Loop round all the elements of this KeyMap which may hold a Region
   pointer. */
         for( ikey = 0; ikey < NREG; ikey++ ) {

/* If the KeyMap contains a Region for this key, get a pointer to it. */
            if( astMapGet0A( keymap, regkey[ ikey ], &obj ) ){
               reg = (AstRegion *) obj;

/* We have two tasks now, firstly to ensure that this AstroCoords Region
   describes an area in the base Frame of the FrameSet in the parent
   Region structure (which may have been changed by the earlier
   simplications performed by this function), and secondly, to attempt to
   simplify the Region.

   The Stc structure addressed by the "stc" pointer will have a current
   Frame given by "frm". This will also be its base Frame, and the
   base->current Mapping will consequently be a UnitMap. The Mapping from
   the original base Frame to the new base Frame is given by "map". Unless
   this is a UnitMap, we need to remap the Region.*/
               if( !astIsAUnitMap( map ) ) {
                  treg = astMapRegion( reg, map, frm );
               } else {
                  treg = astClone( reg );
               }

/* Now attempt to simplify the Region.*/
               newreg = astSimplify( treg );

/* If the Region has been changed by either of these steps, we need to
   store the modified Region back in the "stc" structure which is being
   returned. But we need to be careful we do not modify the supplied Stc
   structure. */
               if( newreg != reg ) {

                  if( stc == (AstStc *) this_mapping ) {
                     temp = astCopy( stc );
                     (void) astAnnul( stc );
                     stc = temp;
                     keymap = temp->coord[ i ];
                  }

                  astMapPut0A( keymap, regkey[ ikey ], newreg, regcom[ ikey ] );

               }

/* Free resources */
               reg = astAnnul( reg );
               treg = astAnnul( treg );
               newreg = astAnnul( newreg );

            }
         }
      }
   }

/* Free resources */
   map = astAnnul( map );
   frm = astAnnul( frm );

/* If an error occurred, annul the returned Mapping. */
   if ( !astOK ) stc = astAnnul( stc );

/* Return the result. */
   return (AstMapping *) stc;
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Stc.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Stc member function (over-rides the astTestAttrib protected
*     method inherited from the Region class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Stc's attributes.

*  Parameters:
*     this
*        Pointer to the Stc.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   int len;                      /* Length of attrib string */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute name matches any of the read-only attributes
   of this class. If it does, then return zero. */
   if ( !strcmp( attrib, "regionclass" ) ) {
      result = 0;

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a Stc to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stc.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Stc member function (over-rides the astTransform method inherited
*     from the Region class).

*  Description:
*     This function takes a Stc and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Region.
*     This implies applying each of the Stc's encapsulated Region in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the Stc.
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
*     match the number of coordinates for the Stc being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *ps;              /* Pointer to PointSet */
   AstPointSet *pset_tmp;        /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   AstRegion *reg;               /* Pointer to encapsulated Region */
   AstStc *this;                 /* Pointer to the Stc structure */
   double **ptr;                 /* Pointer to axis values */
   double **ptr_out;             /* Pointer to output coordinate data */
   int coord;                    /* Zero-based index for coordinates */
   int good;                     /* Is the point inside the Stc? */
   int ncoord_out;               /* No. of coordinates per output point */
   int ncoord_tmp;               /* No. of coordinates per base Frame point */
   int neg;                      /* Negated value for encapsulated Region */
   int neg_old;                  /* Original Negated flag */
   int npoint;                   /* No. of points */
   int point;                    /* Loop counter for points */
   int rep;                      /* Original error reporting status */
   int status_value;                   /* AST status value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a Pointer to the Stc structure */
   this = (AstStc *) this_mapping;

/* Get the encapsulated Region, and the Negated value which should be used
   with it. The returned values take account of whether the supplied Stc has
   itself been Negated or not. The returned Region represent a region within
   the base Frame of the FrameSet encapsulated by the parent Region
   structure. */
   GetRegion( this, &reg, &neg, status );

/* Temporarily set the Negated attribute to the required value.*/
   neg_old = astGetNegated( reg );
   astSetNegated( reg, neg );

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary, containing
   a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet in the parent Region structure to
   transform the supplied positions from the current Frame in the
   encapsulated FrameSet (the Frame represented by the Stc), to the
   base Frame (the Frame in which the encapsulated Region are defined). Note,
   the returned pointer may be a clone of the "in" pointer, and so we
   must be carefull not to modify the contents of the returned PointSet. */
   pset_tmp = astRegTransform( this, in, 0, NULL, NULL );

/* Now transform this PointSet using the encapsulated Region. */
   ps = astTransform( reg, pset_tmp, 0, NULL );

/* Determine the numbers of points and coordinates per point for these base
   Frame PointSets and obtain pointers for accessing the base Frame and output
   coordinate values. */
   npoint = astGetNpoint( pset_tmp );
   ncoord_tmp = astGetNcoord( pset_tmp );
   ptr = astGetPoints( ps );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

      for ( point = 0; point < npoint; point++ ) {
         good = 0;

         for ( coord = 0; coord < ncoord_tmp; coord++ ) {
            if( ptr[ coord ][ point ] != AST__BAD ) {
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
   }

/* Re-instate the original value for the Negated attribute of the
   encapsulated Region. Do this even if an error has occurred. */
   status_value = astStatus;
   astClearStatus;
   rep = astReporting( 0 );
   if( reg ) astSetNegated( reg, neg_old );
   astReporting( rep );
   astSetStatus( status_value );

/* Free resources. */
   reg = astAnnul( reg );
   ps = astAnnul( ps );
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


/* Stc Attributes: */
/* =============== */

/*
*att++
*  Name:
*     RegionClass

*  Purpose:
*     The AST class name of the Region encapsulated within an Stc

*  Type:
*     Public attribute.

*  Synopsis:
*     String, read-only.

*  Description:
*     This is a read-only attribute giving the AST class name of the
*     Region encapsulated within an Stc (that is, the class of the Region
*     which was supplied when the Stc was created).

*  Applicability:
*     Stc
*        All Stc objects this attribute.
*att--
*/

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Stc objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Stc objects.

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
*     Regions within the Stc.
*/

/* Local Variables: */
   AstStc *in;                /* Pointer to input Stc */
   AstStc *out;               /* Pointer to output Stc */
   int i;                     /* AstroCoords index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Stcs. */
   in = (AstStc *) objin;
   out = (AstStc *) objout;

/* For safety, start by clearing any references to the input component
   Regions, etc,  from the output Stc. */
   out->region = NULL;
   out->coord = NULL;
   out->ncoord = 0;

/* Make a copy of the Region and store a pointer to it in the output Stc
   structure. */
   out->region = astCopy( in->region );

/* Copy any memory holding AstroCoords values */
   if( in->coord && in->ncoord ) {
      out->ncoord = in->ncoord;
      out->coord = astMalloc( sizeof(AstKeyMap *) * (size_t)in->ncoord );
      if( out->coord ) {
         for( i = 0; i < in->ncoord; i++ ) {
            out->coord[ i ] = astCopy( in->coord[ i ] );
         }
      }
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Stc objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Stc objects.

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
   AstStc *this;              /* Pointer to Stc */
   int i;                     /* AstroCoords index */

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) obj;

/* Annul the pointer to the encapsulated Region. */
   this->region = astAnnul( this->region );

/* Free any memory holding AstroCoords values */
   if( this->coord ) {
      for( i = 0; i < this->ncoord; i++ ) {
         this->coord[ i ] = astAnnul( this->coord[ i ] );
      }
      this->coord = astFree( this->coord );
   }
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Stc objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Stc class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Stc whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 150          /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstStc *this;                 /* Pointer to the Stc structure */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment string */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   int ico;                      /* Loop counter for KeyMaps */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Stc structure. */
   this = (AstStc *) this_object;

/* Write out values representing the instance variables for the Stc
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

/* Encapsulated Region. */
/* -------------------- */
   astWriteObject( channel, "Region", 1, 1, this->region,
                   "STC Region" );

/* AstroCoords info */
/* ---------------- */
   astWriteInt( channel, "Ncoord", ( this->ncoord != 0 ), 0, this->ncoord,
                "Number of AstroCoords elements" );

   for ( ico = 1; ico <= this->ncoord; ico++ ) {
      (void) sprintf( key, "Coord%d", ico );
      (void) sprintf( comment, "AstroCoords number %d", ico );
      astWriteObject( channel, key, 1, 1, this->coord[ ico - 1 ],
                      comment );
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAStc and astCheckStc functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Stc,Region)
astMAKE_CHECK(Stc)

AstStc *astInitStc_( void *mem, size_t size, int init, AstStcVtab *vtab,
                     const char *name, AstRegion *region, int ncoords,
                     AstKeyMap **coords, int *status ) {
/*
*+
*  Name:
*     astInitStc

*  Purpose:
*     Initialise a Stc.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stc.h"
*      AstStc *astInitStc( void *mem, size_t size, int init, AstStcVtab *vtab,
*                          const char *name, AstRegion *region, int ncoords,
*                          AstKeyMap **coords )

*  Class Membership:
*     Stc initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Stc object. It allocates memory (if necessary) to
*     accommodate the Stc plus any additional data associated with the
*     derived class. It then initialises a Stc structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a Stc at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Stc is to be initialised.
*        This must be of sufficient size to accommodate the Stc data
*        (sizeof(Stc)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Stc (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        Stc structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the Stc's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Stc.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     region
*        Pointer to the Region represented by the Stc.
*     ncoords
*        Number of KeyMap pointers supplied in "coords". Can be zero.
*        Ignored if "coords" is NULL.
*     coords
*        Pointer to an array of "ncoords" KeyMap pointers, or NULL if
*        "ncoords" is zero. Each KeyMap defines defines a single <AstroCoords>
*        element, and should have elements with keys given by constants
*        AST__STCNAME, AST__STCVALUE, AST__STCERROR, AST__STCRES, AST__STCSIZE,
*        AST__STCPIXSZ. Any of these elements may be omitted, but no other
*        elements should be included. If supplied, the AST__STCNAME element
*        should be a vector of character string pointers holding the "Name"
*        item for each axis. Any other supplied elements should be scalar
*        elements, each  holding a pointer to a Region describing the
*        associated item of ancillary information (error, resolution, size,
*        pixel size or value). These Regions should describe a volume within
*        the coordinate system represented by "region".

*  Returned Value:
*     A pointer to the new Stc.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstMapping *frm;              /* Current Frame in supplied Stc */
   AstMapping *map;              /* Base -> Current Mapping in supplied Stc */
   AstRegion *reg;               /* Copy of supplied Region */
   AstStc *new;                  /* Pointer to new Stc */
   int i;                        /* AstroCoords index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitStcVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* If the supplied Region is an Stc, create a new Region by mapping the
   encapsulated Region within the supplied Stc into the current Frame of the
   Stc. */
   if( astIsAStc( region ) ) {
      map = astGetMapping( region->frameset, AST__BASE, AST__CURRENT );
      frm = astGetFrame( region->frameset, AST__CURRENT );
      reg = astMapRegion( ((AstStc *) region)->region, map, frm );
      frm = astAnnul( frm );
      map = astAnnul( map );

/* Otherwise, just take a copy of the supplied Region. */
   } else {
      reg = astCopy( region );
   }

/* Initialise a Region structure (the parent class) as the first component
   within the Stc structure, allocating memory if necessary. A NULL
   PointSet is suppled as the encapsulated Region will perform the function
   of defining the Region shape. The base Frame of the FrameSet in the
   parent Region structure will be the same as the current Frames of the
   FrameSets in the two encapsulated Region. */
   if ( astOK ) {
      new = (AstStc *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                      name, reg, NULL, NULL );

/* Initialise the Stc data. */
/* --------------------------- */
/* Store a pointer to the encapsulated Region. */
      new->region = astClone( reg );

/* No AstroCoords info as yet. */
      new->ncoord = 0;
      new->coord = NULL;

/* Transfer attributes from the encapsulated region to the parent region. */
     astRegOverlay( new, reg, 1 );
     if( astTestIdent( reg ) ) astSetIdent( new, astGetIdent( reg ) );

/* If the base->current Mapping in the FrameSet within the encapsulated Region
   is a UnitMap, then the FrameSet does not need to be included in the
   Dump of the new Stc. Set the RegionFS attribute of the encapsulated
   Region to zero to flag this. Note, we do this after the previous class
   to astRegOverlay because we do not want this zero value for RegionFS to
   be copied into the new Stc object. */
      astSetRegionFS( reg, 0 );

/* For each supplied AstroCoords, create a new KeyMap holding Regions
   representing the various elements of the AstroCoords, and store the
   new KeyMap in the Stc structure. */
      if( coords && ncoords > 0 ) {
         new->ncoord = ncoords;
         new->coord = astMalloc( sizeof( AstKeyMap *)*(size_t) ncoords );
         if( new->coord ) {
            for( i = 0; i < ncoords; i++ ) {
               new->coord[ i ] = MakeAstroCoordsKeyMap( reg, coords[ i ],
                                                        name, status );
            }
         }
      }

/* If an error occurred, clean up deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Free resources */
   reg = astAnnul( reg );

/* Return a pointer to the new object. */
   return new;
}

AstStc *astLoadStc_( void *mem, size_t size, AstStcVtab *vtab,
                     const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadStc

*  Purpose:
*     Load a Stc.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stc.h"
*     AstStc *astLoadStc( void *mem, size_t size, AstStcVtab *vtab,
*                         const char *name, AstChannel *channel )

*  Class Membership:
*     Stc loader.

*  Description:
*     This function is provided to load a new Stc using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Stc structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Stc at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Stc is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Stc data (sizeof(Stc)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Stc (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Stc structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstStc) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Stc. If this is NULL, a pointer to
*        the (static) virtual function table for the Stc class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Stc" is used instead.

*  Returned Value:
*     A pointer to the new Stc.

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
   AstFrame *f1;                 /* Base Frame in parent Region */
   AstObject *obj;               /* Pointer to Object retrieved from KeyMap */
   AstRegion *creg;              /* Pointer to encapsulated Region */
   AstStc *new;                  /* Pointer to the new Stc */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   int ico;                      /* Loop counter for AstroCoords */
   int ikey;                     /* Index of KeyMap */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Stc. In this case the
   Stc belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstStc );
      vtab = &class_vtab;
      name = "Stc";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitStcVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Stc. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Stc" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Encapsulated Region. */
/* -------------------- */
      new->region = astReadObject( channel, "region", NULL );

/* Get a pointer to the base Frame in the FrameSet encapsulated by the
   parent Region structure. */
      f1 = astGetFrame( ((AstRegion *) new)->frameset, AST__BASE );

/* If the encapsulated Region has a dummy FrameSet rather than the correct
   FrameSet, the correct FrameSet will have copies of the base Frame of the
   new Stc as both its current and base Frames, connected by a UnitMap (this
   is equivalent to a FrameSet containing a single Frame). However if the new
   Stc being loaded has itself got a dummy FrameSet, then we do not do this
   since we do not yet know what the correct FrameSet is. In this case we
   wait until the parent Region invokes the astSetRegFS method on the new
   Stc. */
      if( !astRegDummyFS( new ) ) {
         creg = new->region;
         if( astRegDummyFS( creg ) ) astSetRegFS( creg, f1 );
      }

/* AstroCoords info */
/* ---------------- */
/* The number of AstroCoords described in the new Stc. */
      new->ncoord = astReadInt( channel, "ncoord", 0 );
      if( new->ncoord < 0 ) new->ncoord = 0;

/* Read back each KeyMap describing these AstroCoords. */
      new->coord = astMalloc( sizeof( AstKeyMap *) * (size_t) new->ncoord );
      for( ico = 1; ico <= new->ncoord; ico++ ) {
         (void) sprintf( key, "coord%d", ico );
         new->coord[ ico - 1 ] = astReadObject( channel, key, NULL );

/* Ensure the Regions within the KeyMap do not have dummy FrameSets. */
         if( new->coord[ ico - 1 ] && !astRegDummyFS( new ) ) {
            for( ikey = 0; ikey < NREG; ikey++ ) {
               if( astMapGet0A( new->coord[ ico - 1 ], regkey[ ikey ], &obj ) ){
                  creg = (AstRegion *) obj;
                  if( astRegDummyFS( creg ) ) {
                     astSetRegFS( creg, f1 );
                     astMapPut0A( new->coord[ ico - 1 ], regkey[ ikey ], creg,
                                  regcom[ ikey ] );
                  }
                  creg = astAnnul( creg );
               }
            }
         }
      }

/* Free resources */
      f1 = astAnnul( f1 );

/* If an error occurred, clean up by deleting the new Stc. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Stc pointer. */
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

const char *astGetRegionClass_( AstStc *this, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Stc,GetRegionClass))( this, status );
}

AstRegion *astGetStcRegion_( AstStc *this, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Stc,GetStcRegion))( this, status );
}

AstKeyMap *astGetStcCoord_( AstStc *this, int icoord, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Stc,GetStcCoord))( this, icoord, status );
}

int astGetStcNCoord_( AstStc *this, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Stc,GetStcNCoord))( this, status );
}













