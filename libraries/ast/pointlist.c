/*
*class++
*  Name:
*     PointList

*  Purpose:
*     A collection of points in a Frame.

*  Constructor Function:
c     astPointList
f     AST_POINTLIST

*  Description:
*     The PointList class implements a Region which represents a collection
*     of points in a Frame.

*  Inheritance:
*     The PointList class inherits from the Region class.

*  Attributes:
*     In addition to those attributes common to all Regions, every
*     PointList also has the following attributes:
*
*     - ListSize: The number of positions stored in the PointList


*  Functions:
c     The PointList class does not define any new functions beyond those
f     The PointList class does not define any new routines beyond those
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
*     22-MAR-2004 (DSB):
*        Original version.
*     20-JAN-2009 (DSB):
*        Over-ride astRegBasePick.
*     21-JAN-2009 (DSB):
*        - Add methods astGetEnclosure, astSetEnclosure and astPoints, and
*        attribute ListSize.
*        - Override astGetObjSize and astEqual.
*     26-JAN-2009 (DSB):
*        Change protected constructor to accept a PointSet rather than an
*        array of doubles.
*     6-FEB-2009 (DSB):
*        Over-ride astMapMerge.
*     9-FEB-2009 (DSB):
*        Move methods astGetEnclosure and astSetEnclosure to Region class.
*     8-JUL-2009 (DSB):
*        In Transform, use "ptr2", not "ptr", if we are creating a mask.
*class--

*  Implementation Deficiencies:
*     - Use of simple arrays to hold lists  of points is probably not
*     efficient for large numbers of points. For instance, use of k-tree
*     structures instead of arrays could result in a much  more efficient
*     implementation of the Transform function. Maybe the PointSet class
*     should be extended to provide a k-tree representation as well as a
*     simple array.

*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS PointList

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "region.h"              /* Coordinate regions (parent class) */
#include "channel.h"             /* I/O channels */
#include "pointlist.h"           /* Interface definition for this class */
#include "mapping.h"             /* Position mappings */
#include "unitmap.h"             /* Unit Mapping */
#include "frame.h"               /* Coordinate systems */
#include "cmpframe.h"            /* Compound Frames */
#include "cmpmap.h"              /* Compound Mappings */
#include "prism.h"               /* Extruded Regions */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstMapping *(* parent_simplify)( AstMapping *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_getobjsize)( AstObject *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(PointList)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(PointList,Class_Init)
#define class_vtab astGLOBAL(PointList,Class_Vtab)
#define getattrib_buff astGLOBAL(PointList,GetAttrib_Buff)


#include <pthread.h>


#else

static char getattrib_buff[ 101 ];

/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPointListVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPointList *astPointListId_( void *, int, int, int, const double *, void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
static int MaskLD( AstRegion *, AstMapping *, int, int, const int[], const int ubnd[], long double [], long double, int * );
#endif
static int MaskB( AstRegion *, AstMapping *, int, int, const int[], const int[], signed char[], signed char, int * );
static int MaskD( AstRegion *, AstMapping *, int, int, const int[], const int[], double[], double, int * );
static int MaskF( AstRegion *, AstMapping *, int, int, const int[], const int[], float[], float, int * );
static int MaskI( AstRegion *, AstMapping *, int, int, const int[], const int[], int[], int, int * );
static int MaskL( AstRegion *, AstMapping *, int, int, const int[], const int[], long int[], long int, int * );
static int MaskS( AstRegion *, AstMapping *, int, int, const int[], const int[], short int[], short int, int * );
static int MaskUB( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned char[], unsigned char, int * );
static int MaskUI( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned int[], unsigned int, int * );
static int MaskUL( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned long int[], unsigned long int, int * );
static int MaskUS( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned short int[], unsigned short int, int * );

static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *RegBasePick( AstRegion *, int, const int *, int * );
static int GetClosed( AstRegion *, int * );
static int GetListSize( AstPointList *, int * );
static int GetObjSize( AstObject *, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void PointListPoints( AstPointList *, AstPointSet **, int *);
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void RegBaseBox( AstRegion *, double *, double *, int * );
static AstRegion *MergePointList( AstPointList *, AstRegion *, int, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );

static const char *GetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );


/* Member functions. */
/* ================= */

static void ClearAttrib( AstObject *this_object, const char *attrib,
                         int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointList member function (over-rides the astClearAttrib
*     protected method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for a
*     PointList, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the PointList.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPointList *this;            /* Pointer to the PointList structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then report an error. */
   if ( !strcmp( attrib, "listsize" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static const char *GetAttrib( AstObject *this_object, const char *attrib,
                              int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointList member function (over-rides the protected astGetAttrib
*     method inherited from the Region class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a PointList, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the PointList.
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
*     within the PointList, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the PointList. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPointList *this;           /* Pointer to the PointList structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Integer attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an
   appropriate format.  Set "result" to point at the result string. */

/* ListSize. */
/* -------- */
   if ( !strcmp( attrib, "listsize" ) ) {
      ival = astGetListSize( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
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

static int GetClosed( AstRegion *this, int *status ) {
/*
*  Name:
*     GetClosed

*  Purpose:
*     Get the value of the CLosed attribute for a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     int GetClosed( AstRegion *this, int *status )

*  Class Membership:
*     PointList member function (over-rides the astGetClosed method
*     inherited from the Region class).

*  Description:
*     This function returns the value of the Closed attribute for a
*     PointList. Since points have zero volume they consist entirely of
*     boundary. Therefore the Region is always considered to be closed
*     unless it has been negated, in which case it is always assumed to
*     be open.

*  Parameters:
*     this
*        Pointer to the PointList.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The value to use for the Closed attribute.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* The value to be used for the Closed attribute is always the opposite
   of the Negated attribute. */
   return ( astGetNegated( this ) == 0 );
}

static int GetListSize( AstPointList *this, int *status ) {
/*
*+
*  Name:
*     astGetListSize

*  Purpose:
*     Determine how many points there are in a PointList.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointlist.h"
*     int astGetListSize( AstPointList *this )

*  Class Membership:
*     PointList method.

*  Description:
*     This function returns the number of points stored in a Point|List.

*  Parameters:
*     this
*        Pointer to the PointList.

*  Returned Value:
*     The number of points in the PointList.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of points by querying the PointSet that holds the
   points. */
   return astGetNpoint( ((AstRegion *) this)->points );
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
*     #include "pointlist.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     PointList member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied PointList,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the PointList.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPointList *this;         /* Pointer to PointList structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the PointList structure. */
   this = (AstPointList *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astGetObjSize( this->lbnd );
   result += astGetObjSize( this->ubnd );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitPointListVtab_(  AstPointListVtab *vtab, const char *name,
                             int *status ) {
/*
*+
*  Name:
*     astInitPointListVtab

*  Purpose:
*     Initialise a virtual function table for a PointList.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointlist.h"
*     void astInitPointListVtab( AstPointListVtab *vtab, const char *name )

*  Class Membership:
*     PointList vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the PointList class.

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
   will be used (by astIsAPointList) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->GetListSize = GetListSize;
   vtab->PointListPoints = PointListPoints;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   region = (AstRegionVtab *) vtab;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

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

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->MapMerge = MapMerge;

   region->RegBaseMesh = RegBaseMesh;
   region->RegBaseBox = RegBaseBox;
   region->RegBasePick = RegBasePick;
   region->RegPins = RegPins;
   region->GetClosed = GetClosed;
   region->MaskB = MaskB;
   region->MaskD = MaskD;
   region->MaskF = MaskF;
   region->MaskI = MaskI;
   region->MaskL = MaskL;
   region->MaskS = MaskS;
   region->MaskUB = MaskUB;
   region->MaskUI = MaskUI;
   region->MaskUL = MaskUL;
   region->MaskUS = MaskUS;
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
   region->MaskLD = MaskLD;
#endif

/* Declare the class dump function. There is no copy constructor or
   destructor. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "PointList", "Collection of points" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

/*
*  Name:
*     Mask<X>

*  Purpose:
*     Mask a region of a data grid.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     int Mask<X>( AstRegion *this, AstMapping *map, int inside, int ndim,
*                  const int lbnd[], const int ubnd[], <Xtype> in[],
*                  <Xtype> val )

*  Class Membership:
*     PointList function method (replaces the astMask<X> methods
*     inherited from the parent Region class).

*  Description:
*     This is a set of functions for masking out regions within gridded data
*     (e.g. an image). The functions modifies a given data grid by
*     assigning a specified value to all samples which are inside (or outside
*     if "inside" is zero) the specified Region.
*
*     You should use a masking function which matches the numerical
*     type of the data you are processing by replacing <X> in
*     the generic function name astMask<X> by an appropriate 1- or
*     2-character type code. For example, if you are masking data
*     with type "float", you should use the function astMaskF (see
*     the "Data Type Codes" section below for the codes appropriate to
*     other numerical types).

*  Parameters:
*     this
*        Pointer to a Region.
*     map
*        Pointer to a Mapping. The forward transformation should map
*        positions in the coordinate system of the supplied Region
*        into pixel coordinates as defined by the "lbnd" and "ubnd"
*        parameters. A NULL pointer can be supplied if the coordinate
*        system of the supplied Region corresponds to pixel coordinates.
*        This is equivalent to supplying a UnitMap.
*
*        The number of inputs for this Mapping (as given by its Nin attribute)
*        should match the number of axes in the supplied Region (as given
*        by the Naxes attribute of the Region). The number of outputs for the
*        Mapping (as given by its Nout attribute) should match the number of
*        grid dimensions given by the value of "ndim" below.
*     inside
*        A boolean value which indicates which pixel are to be masked. If
*        a non-zero value is supplied, then all grid pixels which are inside
*        the supplied Region are assigned the value given by "val",
*        and all other pixels are left unchanged. If zero is supplied, then
*        all grid pixels which are not inside the supplied Region are
*        assigned the value given by "val", and all other pixels are left
*        unchanged. Note, the Negated attribute of the Region is used to
*        determine which pixel are inside the Region and which are outside.
*        So the inside of a Region which has not been negated is the same as
*        the outside of the corresponding negated Region.
*     ndim
*        The number of dimensions in the input grid. This should be at
*        least one.
*     lbnd
*        Pointer to an array of integers, with "ndim" elements,
*        containing the coordinates of the centre of the first pixel
*        in the input grid along each dimension.
*     ubnd
*        Pointer to an array of integers, with "ndim" elements,
*        containing the coordinates of the centre of the last pixel in
*        the input grid along each dimension.
*
*        Note that "lbnd" and "ubnd" together define the shape
*        and size of the input grid, its extent along a particular
*        (j'th) dimension being ubnd[j]-lbnd[j]+1 (assuming the
*        index "j" to be zero-based). They also define
*        the input grid's coordinate system, each pixel having unit
*        extent along each dimension with integral coordinate values
*        at its centre.
*     in
*        Pointer to an array, with one element for each pixel in the
*        input grid, containing the data to be masked.  The
*        numerical type of this array should match the 1- or
*        2-character type code appended to the function name (e.g. if
*        you are using astMaskF, the type of each array element
*        should be "float").
*
*        The storage order of data within this array should be such
*        that the index of the first grid dimension varies most
*        rapidly and that of the final dimension least rapidly
*        (i.e. Fortran array indexing is used).
*
*        On exit, the samples specified by "inside" are set to the value
*        of "val". All other samples are left unchanged.
*     val
*        This argument should have the same type as the elements of
*        the "in" array. It specifies the value used to flag the
*        masked data (see "inside").

*  Returned Value:
*     The number of pixels to which a value of "badval" has been assigned.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.

*  Data Type Codes:
*     To select the appropriate masking function, you should
*     replace <X> in the generic function name astMask<X> with a
*     1- or 2-character data type code, so as to match the numerical
*     type <Xtype> of the data you are processing, as follows:
*     - D: double
*     - F: float
*     - L: long int
*     - UL: unsigned long int
*     - I: int
*     - UI: unsigned int
*     - S: short int
*     - US: unsigned short int
*     - B: byte (signed char)
*     - UB: unsigned byte (unsigned char)
*
*     For example, astMaskD would be used to process "double"
*     data, while astMaskS would be used to process "short int"
*     data, etc.
*/
/* Define a macro to implement the function for a specific data
   type. */
#define MAKE_MASK(X,Xtype) \
static int Mask##X( AstRegion *this, AstMapping *map, int inside, int ndim, \
                    const int lbnd[], const int ubnd[], \
                    Xtype in[], Xtype val, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *grid_frame;         /* Pointer to Frame describing grid coords */ \
   AstPointSet *pset1;           /* Pointer to base Frame positions */ \
   AstPointSet *pset2;           /* Pointer to current Frame positions */ \
   AstRegion *used_region;       /* Pointer to Region to be used by astResample */ \
   Xtype *temp;                  /* Pointer to temp storage for retained points */ \
   double **ptr2;                /* Pointer to pset2 data values */ \
   int *iv;                      /* Pointer to index array */ \
   int i;                        /* Point index */ \
   int idim;                     /* Loop counter for coordinate dimensions */ \
   int ii;                       /* Vectorised point index */ \
   int j;                        /* Axis index */ \
   int nax;                      /* Number of Region axes */ \
   int negated;                  /* Has Region been negated? */ \
   int nin;                      /* Number of Mapping input coordinates */ \
   int nout;                     /* Number of Mapping output coordinates */ \
   int npnt;                     /* Number of points in PointList */ \
   int result;                   /* Result value to return */ \
   int vlen;                     /* Length of vectorised array */ \
\
/* Initialise. */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Obtain value for the Naxes attribute of the Region. */ \
   nax = astGetNaxes( this ); \
\
/* If supplied, obtain values for the Nin and Nout attributes of the Mapping. */ \
   if( map ) { \
      nin = astGetNin( map ); \
      nout = astGetNout( map ); \
\
/* If OK, check that the number of mapping inputs matches the \
   number of axes in the Region. Report an error if necessary. */ \
      if ( astOK && ( nax != nin ) ) { \
         astError( AST__NGDIN, "astMask"#X"(%s): Bad number of mapping " \
                   "inputs (%d).", status, astGetClass( this ), nin ); \
         astError( AST__NGDIN, "The %s given requires %d coordinate value%s " \
                   "to specify a position.", status, \
                   astGetClass( this ), nax, ( nax == 1 ) ? "" : "s" ); \
      } \
\
/* If OK, check that the number of mapping outputs matches the \
   number of grid dimensions. Report an error if necessary. */ \
      if ( astOK && ( ndim != nout ) ) { \
         astError( AST__NGDIN, "astMask"#X"(%s): Bad number of mapping " \
                   "outputs (%d).", status, astGetClass( this ), nout ); \
         astError( AST__NGDIN, "The pixel grid requires %d coordinate value%s " \
                   "to specify a position.", status, \
                   ndim, ( ndim == 1 ) ? "" : "s" ); \
      } \
\
/* Create a new Region by mapping the supplied Region with the supplied \
   Mapping.*/ \
      grid_frame = astFrame( ndim, "Domain=grid", status ); \
      used_region = astMapRegion( this, map, grid_frame ); \
      grid_frame = astAnnul( grid_frame ); \
\
/* If no Mapping was supplied check that the number of grid dimensions \
   matches the number of axes in the Region.*/ \
   } else if ( astOK && ( ( ndim != nax ) || ( ndim < 1 ) ) ) { \
      used_region = NULL; \
      astError( AST__NGDIN, "astMask"#X"(%s): Bad number of input grid " \
                "dimensions (%d).", status, astGetClass( this ), ndim ); \
      if ( ndim != nax ) { \
         astError( AST__NGDIN, "The %s given requires %d coordinate value%s " \
                   "to specify an input position.", status, \
                   astGetClass( this ), nax, ( nax == 1 ) ? "" : "s" ); \
      } \
\
/* If no Mapping was supplied and the parameters look OK, clone the \
   supplied Region pointer for use later on. */  \
   } else { \
      used_region = astClone( this );  \
   }  \
\
/* Check that the lower and upper bounds of the input grid are \
   consistent. Report an error if any pair is not. */ \
   if ( astOK ) { \
      for ( idim = 0; idim < ndim; idim++ ) { \
         if ( lbnd[ idim ] > ubnd[ idim ] ) { \
            astError( AST__GBDIN, "astMask"#X"(%s): Lower bound of " \
                      "input grid (%d) exceeds corresponding upper bound " \
                      "(%d).", status, astGetClass( this ), \
                      lbnd[ idim ], ubnd[ idim ] ); \
            astError( AST__GBDIN, "Error in input dimension %d.", status, \
                      idim + 1 ); \
            break; \
         } \
      } \
   } \
\
/* Get the PointSet in the base Frame of the Region's FrameSet, and \
   transform to the current (GRID) Frame. */ \
   pset1 = used_region->points; \
   pset2 = astRegTransform( used_region, pset1, 1, NULL, NULL ); \
   ptr2 =astGetPoints( pset2 ); \
\
/* Allocate memory to hold the corresponding vector indices. */ \
   npnt = astGetNpoint( pset2 ); \
   iv = astMalloc( sizeof(int)*(size_t) npnt ); \
   if( astOK ) { \
\
/* Convert the transformed GRID positions into integer indices into the \
   vectorised data array. Also form the total size of the data array. */ \
      vlen = 0; \
      for( i = 0; i < npnt; i++ ) { \
         vlen = 1; \
         ii = 0; \
         for( j = 0; j < ndim; j++ ) { \
            ii += vlen*( (int)( ptr2[ j ][ i ] + 0.5 ) - lbnd[ j ] ); \
            vlen *= ubnd[ i ] - lbnd[ i ] + 1; \
         }  \
         iv[ i ] = ii; \
      } \
\
/* See if the Region is negated. */ \
      negated = astGetNegated( used_region ); \
\
/* If necessary, set the transformed pixel coords to the supplied value. */ \
      if( ( inside && !negated ) || ( !inside && negated ) ) { \
         for( i = 0; i < npnt; i++ ) in[ iv[ i ] ] = val; \
         result = npnt; \
\
/* If necessary, set all except the transformed pixel coords to the supplied  \
   value. */ \
      } else { \
         temp = astMalloc( sizeof( Xtype )*(size_t)npnt ); \
         if( astOK ) { \
            for( i = 0; i < npnt; i++ ) temp[ i ] = in[ iv[ i ] ]; \
            for( i = 0; i < vlen; i++ ) in[ i ] = val; \
            for( i = 0; i < npnt; i++ ) in[ iv[ i ] ] = temp[ i ]; \
            result = vlen - npnt; \
         } \
         temp = astFree( temp ); \
      } \
   } \
\
/* Free resources */ \
   iv = astFree( iv ); \
   pset2 = astAnnul( pset2 ); \
   used_region = astAnnul( used_region ); \
\
/* If an error occurred, clear the returned result. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
MAKE_MASK(LD,long double)
#endif
MAKE_MASK(D,double)
MAKE_MASK(F,float)
MAKE_MASK(L,long int)
MAKE_MASK(UL,unsigned long int)
MAKE_MASK(I,int)
MAKE_MASK(UI,unsigned int)
MAKE_MASK(S,short int)
MAKE_MASK(US,unsigned short int)
MAKE_MASK(B,signed char)
MAKE_MASK(UB,unsigned char)

/* Undefine the macro. */
#undef MAKE_MASK

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     PointList method (over-rides the protected astMapMerge method
*     inherited from the Region class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated PointList in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated PointList with a Mapping which it
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
*        Pointer to the nominated PointList which is to be merged with
*        its neighbours. This should be a cloned copy of the PointList
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        PointList it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated PointList resides.
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
   AstPointList *oldint;  /* Pointer to supplied PointList */
   AstMapping *map;      /* Pointer to adjacent Mapping */
   AstMapping *new;      /* Simplified or merged Region */
   int i1;               /* Index of first Mapping merged */
   int i;                /* Loop counter */
   int result;           /* Result value to return */

/* Initialise. */
   result = -1;
   i1 = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the PointList. */
   oldint = (AstPointList *) this;

/* First of all, see if the PointList can be replaced by a simpler Region,
   without reference to the neighbouring Regions in the list.           */
/* =====================================================================*/

/* Try to simplify the PointList. If the pointer value has changed, we assume
   some simplification took place. */
   new = astSimplify( oldint );
   if( new != (AstMapping *) oldint ) {

/* Annul the PointList pointer in the list and replace it with the new Region
   pointer, and indicate that the forward transformation of the returned
   Region should be used (not really needed but keeps things clean). */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = new;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the PointList itself could not be simplified, see if it can be merged
   with the Regions on either side of it in the list. We can only merge
   in parallel. */
/* =====================================================================*/
   } else if( ! series ){
      new = astAnnul( new );

/* Attempt to merge the PointList with its lower neighbour (if any). */
      if( where > 0 ) {
         i1 = where - 1;
         map = ( *map_list )[ where - 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergePointList( oldint, (AstRegion *) map,
                                                  0, status );
         }
      }

/* If this did not produced a merged Region, attempt to merge the PointList
   with its upper neighbour (if any). */
      if( !new && where < *nmap - 1 ) {
         i1 = where;
         map = ( *map_list )[ where + 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergePointList( oldint, (AstRegion *) map,
                                                  1, status );
         }
      }

/* If succesfull... */
      if( new ){

/* Annul the first of the two Mappings, and replace it with the merged
   Region. Also clear the invert flag. */
         (void) astAnnul( ( *map_list )[ i1 ] );
         ( *map_list )[ i1 ] = new;
         ( *invert_list )[ i1 ] = 0;

/* Annul the second of the two Mappings, and shuffle down the rest of the
   list to fill the gap. */
         (void) astAnnul( ( *map_list )[ i1 + 1 ] );
         for ( i = i1 + 2; i < *nmap; i++ ) {
            ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
            ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
         }

/* Clear the vacated element at the end. */
         ( *map_list )[ *nmap - 1 ] = NULL;
         ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
         ( *nmap )--;
         result = i1;
      }

   } else {
      new = astAnnul( new );
   }

/* Return the result. */
   return result;
}

static AstRegion *MergePointList( AstPointList *this, AstRegion *reg,
                                   int plsfirst, int *status ) {
/*
*  Name:
*     MergePointList

*  Purpose:
*     Attempt to merge a PointList with another Region to form a Region of
*     higher dimensionality.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     AstRegion *MergePointList( AstPointList *this, AstRegion *reg,
*                                 int plsfirst, int *status )

*  Class Membership:
*     PointList member function.

*  Description:
*     This function attempts to combine the supplied Regions together
*     into a Region of higher dimensionality.

*  Parameters:
*     this
*        Pointer to a PointList.
*     reg
*        Pointer to another Region.
*     plsfirst
*        If non-zero, then the PointList axes are put first in the new Region.
*        Otherwise, the other Region's axes are put first.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to a new region, or NULL if the supplied Regions could
*     not be merged.
*/

/* Local Variables: */
   AstFrame *bfrm;           /* Pointer to base Frame for "result" */
   AstFrame *cfrm;           /* Pointer to current Frame for "result" */
   AstFrame *frm_reg;        /* Pointer to Frame from "reg" */
   AstFrame *frm_this;       /* Pointer to Frame from "this" */
   AstMapping *bcmap;        /* Base->current Mapping for "result" */
   AstMapping *map_reg;      /* Base->current Mapping from "reg" */
   AstMapping *map_this;     /* Base->current Mapping from "this" */
   AstMapping *sbunc;        /* Simplified uncertainty */
   AstPointSet *pset_new;    /* PointSet for new PointList */
   AstPointSet *pset_reg;    /* PointSet from reg */
   AstPointSet *pset_this;   /* PointSet from this */
   AstRegion *bunc;          /* Base Frame uncertainty Region */
   AstRegion *new;           /* Pointer to new PointList in base Frame */
   AstRegion *result;        /* Pointer to returned PointList in current Frame */
   AstRegion *unc_reg;       /* Current Frame uncertainty Region from "reg" */
   AstRegion *unc_this;      /* Current Frame uncertainty Region from "this" */
   double **ptr_new;         /* PointSet data pointers for new PointList */
   double **ptr_reg;         /* PointSet data pointers for reg */
   double **ptr_this;        /* PointSet data pointers for this */
   double fac_reg;           /* Ratio of used to default MeshSize for "reg" */
   double fac_this;          /* Ratio of used to default MeshSize for "this" */
   int i;                    /* Axis index */
   int msz_reg;              /* Original MeshSize for "reg" */
   int msz_reg_set;          /* Was MeshSize originally set for "reg"? */
   int msz_this;             /* Original MeshSize for "this" */
   int msz_this_set;         /* Was MeshSize originally set for "this"? */
   int nax;                  /* Number of axes in "result" */
   int nax_reg;              /* Number of axes in "reg" */
   int nax_this;             /* Number of axes in "this" */
   int neg_reg;              /* Negated attribute value for other supplied Region */
   int neg_this;             /* Negated attribute value for supplied PointList  */

/* Initialise */
   result = NULL;

/* Check the local error status. */
   if ( !astOK ) return result;

/* Get the Closed attributes of the two Regions. They must be the same in
   each Region if we are to merge the Regions. In addition, in order to
   merge, either both Regions must have a defined uncertainty, or neither
   Region must have a defined Uncertainty. */
   if( astGetClosed( this ) == astGetClosed( reg ) &&
       astTestUnc( this ) == astTestUnc( reg ) ) {

/* Get the Nagated attributes of the two Regions. */
      neg_this = astGetNegated( this );
      neg_reg = astGetNegated( reg );

/* We only check for merging with another PointList (other classes such
   as Box and Interval check for merging of PointLists with other classes).
   The result will be an PointList. Both Regions must have the same value
   for the Negated flag, and can contain only a single point. */
      if( astIsAPointList( reg ) && neg_this == neg_reg &&
          astGetListSize( this ) == 1 &&
          astGetListSize( (AstPointList *) reg ) == 1 ) {

/* Get the number of axes in the two supplied Regions. */
         nax_reg = astGetNaxes( reg );
         nax_this = astGetNaxes( this );

/* Get the number of axes the combination will have. */
         nax = nax_reg + nax_this;

/* Get the base Frames from the two Region FrameSets, and combine them
   into a single CmpFrame that will be used to create the new Region. */
         frm_this = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );
         frm_reg = astGetFrame( reg->frameset, AST__BASE );

         if( plsfirst ) {
            bfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
         } else {
            bfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
         }

         frm_this = astAnnul( frm_this );
         frm_reg = astAnnul( frm_reg );

/* Get pointers to the coordinate data in the two PointLists */
         pset_this = ((AstRegion *) this)->points;
         ptr_this = astGetPoints( pset_this );
         pset_reg = reg->points;
         ptr_reg = astGetPoints( pset_reg );

/* Create a PointSet to hold the points for the returned PointList. */
         pset_new = astPointSet( 1, nax, "", status );

/* Get pointers to its data. */
         ptr_new = astGetPoints( pset_new );

/* Check pointers can be used safely. */
         if( astOK ) {

/* Copy the point positions fon the selected axes into the arrays allocated
   above, in the requested order. */
            if( plsfirst ) {
               for( i = 0; i < nax_this; i++ ) {
                  ptr_new[ i ][ 0 ] = ptr_this[ i ][ 0 ];
               }
               for( ; i < nax; i++ ) {
                  ptr_new[ i ][ 0 ] = ptr_reg[ i - nax_this ][ 0 ];
               }

            } else {
               for( i = 0; i < nax_reg; i++ ) {
                  ptr_new[ i ][ 0 ] = ptr_reg[ i ][ 0 ];
               }
               for( ; i < nax; i++ ) {
                  ptr_new[ i ][ 0 ] = ptr_this[ i - nax_reg ][ 0 ];
               }
            }

/* Create the new PointList. */
            new = (AstRegion *) astPointList( bfrm, pset_new, NULL, "",
                                              status );

/* Propagate remaining attributes of the supplied Region to it. */
            astRegOverlay( new, this, 1 );

/* Ensure the Negated flag is set correctly in the returned PointList. */
            if( neg_this ) {
               astSetNegated( new, neg_this );
            } else {
               astClearNegated( new );
            }

/* If both the supplied Regions have uncertainty, assign the new Region an
   uncertainty. */
            if( astTestUnc( this ) && astTestUnc( reg ) ) {

/* Get the uncertainties from the two supplied Regions. */
               unc_this = astGetUncFrm( this, AST__BASE );
               unc_reg = astGetUncFrm( reg, AST__BASE );

/* Combine them into a single Region (a Prism), in the correct order. */
               if( plsfirst ) {
                  bunc = (AstRegion *) astPrism( unc_this, unc_reg, "", status );
               } else {
                  bunc = (AstRegion *) astPrism( unc_reg, unc_this, "", status );
               }

/* Attempt to simplify the Prism. */
               sbunc = astSimplify( bunc );

/* Use the simplified Prism as the uncertainty for the returned Region. */
               astSetUnc( new, sbunc );

/* Free resources. */
               sbunc = astAnnul( sbunc );
               bunc = astAnnul( bunc );
               unc_reg = astAnnul( unc_reg );
               unc_this = astAnnul( unc_this );
            }

/* Get the current Frames from the two Region FrameSets, and combine them
   into a single CmpFrame. */
            frm_this = astGetFrame( ((AstRegion *) this)->frameset, AST__CURRENT );
            frm_reg = astGetFrame( reg->frameset, AST__CURRENT );

            if( plsfirst ) {
               cfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
            } else {
               cfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
            }

/* Get the base -> current Mappings from the two Region FrameSets, and
   combine them into a single parallel CmpMap that connects bfrm and cfrm. */
            map_this = astGetMapping( ((AstRegion *) this)->frameset, AST__BASE,
                                      AST__CURRENT );
            map_reg = astGetMapping( reg->frameset, AST__BASE, AST__CURRENT );

            if( plsfirst ) {
               bcmap = (AstMapping *) astCmpMap( map_this, map_reg, 0, "",
                                                 status );
            } else {
               bcmap = (AstMapping *) astCmpMap( map_reg, map_this, 0, "",
                                                 status );
            }

/* Map the new Region into the new current Frame. */
            result = astMapRegion( new, bcmap, cfrm );

/* The filling factor in the returned is the product of the filling
   factors for the two supplied Regions. */
            if( astTestFillFactor( reg ) || astTestFillFactor( this ) ) {
               astSetFillFactor( result, astGetFillFactor( reg )*
                                         astGetFillFactor( this ) );
            }

/* If the MeshSize value is set in either supplied Region, set a value
   for the returned Region which scales the default value by the
   product of the scaling factors for the two supplied Regions. First see
   if either MeshSize value is set. */
            msz_this_set = astTestMeshSize( this );
            msz_reg_set = astTestMeshSize( reg );
            if( msz_this_set || msz_reg_set ) {

/* If so, get the two MeshSize values (one of which may be a default
   value), and then clear them so that the default value will be returned
   in future. */
               msz_this = astGetMeshSize( this );
               msz_reg = astGetMeshSize( reg );
               astClearMeshSize( this );
               astClearMeshSize( reg );

/* Get the ratio of the used MeshSize to the default MeshSize for both
   Regions. */
               fac_this = (double)msz_this/(double)astGetMeshSize( this );
               fac_reg = (double)msz_reg/(double)astGetMeshSize( reg );

/* The MeshSize of the returned Returned is the default value scaled by
   the product of the two ratios found above. */
               astSetMeshSize( result, fac_this*fac_reg*astGetMeshSize( result ) );

/* Re-instate the original MeshSize values for the supplied Regions (if
   set) */
               if( msz_this_set ) astSetMeshSize( this, msz_this );
               if( msz_reg_set ) astSetMeshSize( reg, msz_reg );
            }

/* Free remaining resources */
            frm_this = astAnnul( frm_this );
            frm_reg = astAnnul( frm_reg );
            map_this = astAnnul( map_this );
            map_reg = astAnnul( map_reg );
            bcmap = astAnnul( bcmap );
            cfrm = astAnnul( cfrm );
            new = astAnnul( new );
         }
         bfrm = astAnnul( bfrm );
         pset_new = astAnnul( pset_new );

      }
   }

/* If an error has occurred, annul the returned pointer. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

void PointListPoints( AstPointList *this, AstPointSet **pset, int *status) {
/*
*+
*  Name:
*     astPointListPoints

*  Purpose:
*     Return the defining points of a PointList.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointlist.h"
*     astPointListPoints( AstPointList *this, AstPointSet **pset )

*  Class Membership:
*     Region virtual function.

*  Description:
*     This function returns the axis values at the points defining the
*     supplied PointList.

*  Parameters:
*     this
*        Pointer to the PointList.
*     pset
*        Address of a location at which to return a pointer to a PointSet
*        holding the points in the PointList, in the base Frame of the
*        encapsulated FrameSet. The returned Pointer should be annulled
*        when no longer needed.

*-
*/

/* Check the inherited status. */
   if( !astOK ) return;

/* Return a clone of the PointSet holding the points defining the PointList. */
   *pset = astClone( ((AstRegion *) this)->points );

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
*     #include "pointlist.h"
*     void astRegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     PointList member function (over-rides the astRegBaseBox protected
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
   AstFrame *frm;                /* Pointer to encapsulated Frame */
   AstPointList *this;           /* Pointer to PointList structure */
   AstPointSet *pset;            /* Pointer to PointSet defining the Region */
   double **ptr;                 /* Pointer to PointSet data */
   double *p;                    /* Pointer to next axis value */
   double *lb;                   /* Pointer to lower limit array */
   double *ub;                   /* Pointer to upper limit array */
   double d;                     /* Axis offset from refernce value */
   double p0;                    /* Reference axis value */
   int ic;                       /* Axis index */
   int ip;                       /* Point index */
   int nb;                       /* Number of bytes to be copied */
   int nc;                       /* No. of axes in base Frame */
   int np;                       /* No. of points in PointSet */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the PointList structure. */
   this = (AstPointList *) this_region;

/* Calculate the number of bytes in each array. */
   nb = sizeof( double )*(size_t) astGetNaxes( this );

/* If the base Frame bounding box has not yet been found, find it now and
   store it in the PointList structure. */
   if( !this->lbnd || !this->ubnd ) {

/* Allocate memory to store the bounding box in the PointList structure. */
      lb = astMalloc( nb );
      ub = astMalloc( nb );

/* Get the axis values for the PointSet which defines the location and
   extent of the region in the base Frame of the encapsulated FrameSet. */
      pset = this_region->points;
      ptr = astGetPoints( pset );
      nc = astGetNcoord( pset );
      np = astGetNpoint( pset );

/* Get a pointer to the base Frame in the encaposulated FrameSet. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Check pointers can be used safely. */
      if( astOK ) {

/* Find the bounds on each axis in turn. */
         for( ic = 0; ic < nc; ic++ ) {

/* We first find the max and min axis offsets from the first point. We
   used astAxDistance to cater for the possbility that the Frame may be a
   SkyFrame and thus have circular redundancy. */
            p = ptr[ ic ] + 1;
            p0 = p[ -1 ];
            lb[ ic ] = 0.0;
            ub[ ic ] = 0.0;
            for( ip = 1; ip < np; ip++, p++ ) {
               d = astAxDistance( frm, ic + 1, p0, *p );
               if( d < lb[ ic ] ) lb[ ic ] = d;
               if( d > ub[ ic ] ) ub[ ic ] = d;
            }

/* Now convert these offsets to actual axis values. */
            lb[ ic ] = astAxOffset( frm, ic + 1, p0, lb[ ic ] );
            ub[ ic ] = astAxOffset( frm, ic + 1, p0, ub[ ic ] );

         }
      }

/* Free resources */
      frm = astAnnul( frm );

/* Store the pointers in the PointList structure. */
      if( astOK ) {
         this->lbnd = lb;
         this->ubnd = ub;
      } else {
         this->lbnd = astFree( this->lbnd );
         this->ubnd = astFree( this->ubnd );
      }
   }

/* If the bounding box has been found succesfully, copy it into the supplied
   arrays. */
   if( astOK ) {
      memcpy( lbnd, this->lbnd, nb );
      memcpy( ubnd, this->ubnd, nb );
   }

}

static AstPointSet *RegBaseMesh( AstRegion *this, int *status ){
/*
*  Name:
*     RegBaseMesh

*  Purpose:
*     Return a PointSet containing a mesh of points on the boundary of a
*     Region in its base Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     PointList member function (over-rides the astRegBaseMesh protected
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
*     Pointer to the PointSet. The axis values in this PointSet will have
*     associated accuracies derived from the accuracies which were
*     supplied when the Region was created.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*/

/* Local Variables: */
   AstPointSet *result;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* If the Region structure contains a pointer to a PointSet holding
   a previously created mesh, return it. */
   if( this->basemesh ) {
      result = astClone( this->basemesh );

/* Otherwise, create a new mesh. */
   } else {

/* It is just a copy of the encapsulated PointSet. */
      result = astCopy( this->points );

/* Same the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
      if( astOK && result ) this->basemesh = astClone( result );
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
*     #include "pointlist.h"
*     AstRegion *RegBasePick( AstRegion *this, int naxes, const int *axes,
*                             int *status )

*  Class Membership:
*     PointList member function (over-rides the astRegBasePick protected
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
   AstFrame *bfrm;         /* The base Frame in the supplied Region */
   AstFrame *frm;          /* The base Frame in the returned Region */
   AstPointSet *pset;      /* Holds axis values defining the supplied Region */
   AstPointSet *pset_new;  /* Holds axis values defining the returned Region */
   AstRegion *bunc;        /* The uncertainty in the supplied Region */
   AstRegion *result;      /* Returned Region */
   AstRegion *unc;         /* The uncertainty in the returned Region */
   double **ptr;           /* Holds axis values defining the supplied Region */
   double **ptr_new;       /* Holds axis values defining the returned Region */
   double *p;              /* Pointer to next input axis value */
   double *q;              /* Pointer to next output axis value */
   int i;                  /* Index of axis within returned Region */
   int j;                  /* Point index */
   int npnt;               /* Number of points in PointList */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the base Frame of the encapsulated FrameSet. */
   bfrm = astGetFrame( this_region->frameset, AST__BASE );

/* Create a Frame by picking the selected axes from the base Frame of the
   encapsulated FrameSet. */
   frm = astPickAxes( bfrm, naxes, axes, NULL );

/* Get the uncertainty Region (if any) within the base Frame of the supplied
   Region, and select the required axes from it. If the resulting Object
   is not a Region, annul it so that the returned Region will have no
   uncertainty. */
   if( astTestUnc( this_region ) ) {
      bunc = astGetUncFrm( this_region, AST__BASE );
      unc = astPickAxes( bunc, naxes, axes, NULL );
      bunc = astAnnul( bunc );

      if( ! astIsARegion( unc ) ) unc = astAnnul( unc );

   } else {
      unc = NULL;
   }

/* Get pointers to the coordinate data in the parent Region structure. */
   pset = this_region->points;
   ptr = astGetPoints( pset );
   npnt = astGetNpoint( pset );

/* Create a PointSet to hold the points for the returned PointList. */
   pset_new = astPointSet( npnt, naxes, "", status );

/* Get pointers to its data. */
   ptr_new = astGetPoints( pset_new );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Copy the point positions on the selected axes into the arrays allocated
   above. */
      for( i = 0; i < naxes; i++ ) {
         p = ptr[ axes[ i ] ];
         q = ptr_new[ i ];
         for( j = 0; j < npnt; j++ ) *(q++) = *(p++);
      }

/* Create the new PointList. */
      result = (AstRegion *) astPointList( frm, pset_new, unc, "", status );
   }

/* Free resources */
   frm = astAnnul( frm );
   bfrm = astAnnul( bfrm );
   if( unc ) unc = astAnnul( unc );
   pset_new = astAnnul( pset_new );

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
*     Check if a set of points fall on the boundary of a given PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     PointList member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given PointList.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied PointList "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the PointList.
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
*        array is equal to the value of the Npoint attribute of "this".
*        Each element in the returned array is set to 1 if the
*        corresponding position in "this" is on the boundary of the Region
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
   AstPointList *pl;            /* Pointer to PointList holding supplied points */
   AstPointList *this;          /* Pointer to the PointList structure. */
   AstPointSet *pset2;          /* Supplied points masked by this PointList */
   AstPointSet *pset3;          /* This PointList masked by supplied points */
   double **ptr2;               /* Pointer to axis values in "pset2" */
   double **ptr3;               /* Pointer to axis values in "pset3" */
   double **ptr;                /* Pointer to axis values in "this" */
   double *p;                   /* Pointer to next axis value to read */
   int ic;                      /* Axis index */
   int icurr;                   /* Index of original current Frame in "this" */
   int ip;                      /* Point index */
   int nc;                      /* No. of axes in Box base frame */
   int neg_old;                 /* Original Negated flag for "this" */
   int np;                      /* No. of supplied points */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the Box structure. */
   this = (AstPointList *) this_region;

/* Temporarily ensure that the current Frame in "this" is the same as the
   base Frame. We need to do this since the supplied points are in the
   base Frame of "this", but the astTransform method below assumes
   that it is transforming points in the current Frame of the Region. */
   icurr = astGetCurrent( this_region->frameset );
   astSetCurrent( this_region->frameset, AST__BASE );

/* Get pointer to the supplied axis values, the number of points and the
   number of axis values per point. */
   ptr = astGetPoints( pset );
   np = astGetNpoint( pset );
   nc = astGetNcoord( pset );

/* All the supplied points should be within the supplied PointsList region
   (given that "within" implies some tolerance). Transform the positions
   using this PointList and check if any of the resulting points fell
   outside this PointList. We need to ensure that the PointList is not
   negated first. */
   neg_old = astGetNegated( this );
   astSetNegated( this, 0 );
   pset2 = astTransform( this, pset, 1, NULL );
   ptr2 = astGetPoints( pset2 );

/* Check pointers can be used. */
   if( astOK ) {

/* Check there are no bad points (i.e. check that none of the points are
   outside the supplied PointList). The algorithm used to do this depends
   on whether we need to create an output mask. */
      result = 1;
      if( mask ) {

/* Create the returned mask array. */
         *mask = astMalloc( np );
         if( astOK ) {

/* Initialise the mask elements on the basis of the first axis values */
            result = 1;
            p = ptr2[ 0 ];
            for( ip = 0; ip < np; ip++ ) {
               if( *(p++) == AST__BAD ) {
                  result = 0;
                  (*mask)[ ip ] = 0;
               } else {
                  (*mask)[ ip ] = 1;
               }
            }

/* Now check for bad values on other axes. */
            for( ic = 1; ic < nc; ic++ ) {
               p = ptr2[ ic ];
               for( ip = 0; ip < np; ip++ ) {
                  if( *(p++) == AST__BAD ) {
                     result = 0;
                     (*mask)[ ip ] = 0;
                  }
               }
            }
         }

/* If no output mask is to be made, we can break out of the check as soon
   as the first bad value is found. */
      } else {
         for( ic = 0; ic < nc && result; ic++ ){
            p = ptr2[ ic ];
            for( ip = 0; ip < np; ip++,p++ ){
               if( *p == AST__BAD ) {
                  result = 0;
                  break;
               }
            }
         }
      }

/* If this check was passed, we perform a similar check in the opposite
   direction: we create a new PointList from the supplied list of points,
   and then we transform the points associated with the supplied PointList
   using the new PointList. This checks that all the points in the
   supplied PointList are close to the supplied points. Create the new
   PointList holding the supplied points. */
      if( result ) {
         pl = astPointList( unc, pset, unc, "", status );

/* Transform the points in "this" PointList using the new PointList as a
   Mapping. */
         pset3 = astTransform( pl, this_region->points, 1, NULL );
         ptr3 = astGetPoints( pset3 );

/* Check pointers can be used. */
         if( astOK ) {
            for( ic = 0; ic < nc && result; ic++ ){
               p = ptr3[ ic ];
               for( ip = 0; ip < np; ip++,p++ ){
                  if( *p == AST__BAD ) {
                     result = 0;
                     break;
                  }
               }
            }
         }

/* Free resources. */
         pl = astAnnul( pl );
         pset3 = astAnnul( pset3 );

      }
   }

   pset2 = astAnnul( pset2 );

/* Re-instate the original current Frame in "this". */
   astSetCurrent( this_region->frameset, icurr );

/* Re-instate the original Negated flag for "this". */
   astSetNegated( this, neg_old );

/* If an error has occurred, return zero. */
   if( !astOK ) {
      result = 0;
      if( mask ) *mask = astAnnul( *mask );
   }

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting,
                       int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     PointList member function (over-rides the astSetAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for a PointList, the
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
*        Pointer to the PointList.
*     setting
*        Pointer to a null-terminated string specifying the new
*        attribute value.
*/

/* Local Variables: */
   AstPointList *this;            /* Pointer to the PointList structure */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   if ( MATCH( "listsize" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.",
                status, setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static AstMapping *Simplify( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     Simplify

*  Purpose:
*     Simplify the Mapping represented by a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     PointList method (over-rides the astSimplify method inherited
*     from the Region class).

*  Description:
*     This function invokes the parent Region Simplify method, and then
*     performs any further region-specific simplification.
*
*     If the Mapping from base to current Frame is not a UnitMap, this
*     will include attempting to fit a new Region to the boundary defined
*     in the current Frame.

*  Parameters:
*     this
*        Pointer to the original Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the simplified Region. A cloned pointer to the
*     supplied Region will be returned if no simplication could be
*     performed.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to encapsulated Frame */
   AstMapping *map;              /* Pointer to frameset Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstPointList *new2;           /* Pointer to simplified Region */
   AstPointSet *pset1;           /* Original base Frame positions */
   AstPointSet *pset2;           /* Current Frame Frame positions */
   AstRegion *new;               /* Pointer to simplified Region */
   AstRegion *this;              /* Pointer to original Region structure */
   AstRegion *unc;               /* Pointer to new uncertainty Region */
   double **ptr2;                /* Pointer to current Frame points */
   int simpler;                  /* Has some simplication taken place? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Region structure. */
   this = (AstRegion *) this_mapping;

/* Invoke the parent Simplify method inherited from the Region class. This
   will simplify the encapsulated FrameSet and uncertainty Region. */
   new = (AstRegion *) (*parent_simplify)( this_mapping, status );

/* Note if any simplification took place. This is assumed to be the case
   if the pointer returned by the above call is different to the supplied
   pointer. */
   simpler = ( new != this );

/* Get the Mapping from base to current Frame. We can modify the PointList so
   that a UnitMap can be used. This is good because it means that the
   serialised PointList is simpler since the Dump function only needs to
   record one Frame instead of the whole FrameSet. */
   map = astGetMapping( new->frameset, AST__BASE, AST__CURRENT );
   if( !astIsAUnitMap( map ) ){

/* Get a pointer to the current Region Frame */
      fr = astGetFrame( this->frameset, AST__CURRENT );

/* Get the PointSet which holds the base Frame positions defining this
   PointList. */
      pset1 = this->points;

/* Transform the PointSet using this Mapping. */
      pset2 = astTransform( map, pset1, 1, NULL );
      ptr2 = astGetPoints( pset2 );

/* Get the Region describing the positional uncertainty within the
   supplied PointList, in its current Frame. */
      unc = astGetUncFrm( new, AST__CURRENT );

/* Create a new PointList, and use it in place of the original. */
      new2 = astPointList( fr, pset2, unc, "", status );
      (void) astAnnul( new );
      new = (AstRegion *) new2;
      simpler = 1;

/* Free resources. */
      fr = astAnnul( fr );
      pset2 = astAnnul( pset2 );
      unc = astAnnul( unc );
   }

/* Free resources. */
   map = astAnnul( map );

/* If any simplification could be performed, copy Region attributes from
   the supplied Region to the returned Region, and return a pointer to it.
   If the supplied Region had no uncertainty, ensure the returned Region
   has no uncertainty. Otherwise, return a clone of the supplied pointer. */
   if( simpler ){
      astRegOverlay( new, this, 1 );
      result = (AstMapping *) new;

   } else {
      new = astAnnul( new );
      result = astClone( this );
   }

/* If an error occurred, annul the returned pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int TestAttrib( AstObject *this_object, const char *attrib,
                       int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a PointList.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointList member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate
*     whether a value has been set for one of a PointList's attributes.

*  Parameters:
*     this
*        Pointer to the PointList.
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
   AstPointList *this;            /* Pointer to the PointList structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then return zero. */
   if ( !strcmp( attrib, "listsize" ) ){
      result = 0;

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
*     Apply a PointList to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointlist.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     PointList member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a PointList and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region covered by the
*     PointList. PointList inside the region are copied unchanged from input
*     to output.

*  Parameters:
*     this
*        Pointer to the PointList.
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
*     -  The forward and inverse transformations are identical for a
*     Region.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of axes in the Frame represented by the PointList.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *in_base;         /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *ps1;             /* Pointer to accumulation PointSet */
   AstPointSet *ps2;             /* Pointer to accumulation PointSet */
   AstPointSet *ps3;             /* Pointer for swapping PointSet pointers */
   AstPointSet *pset_base;       /* PointList positions in "unc" base Frame */
   AstPointSet *pset_reg;        /* Pointer to Region PointSet */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstRegion *this;              /* Pointer to the Region structure */
   AstRegion *unc;               /* Pointer to uncertainty Region */
   double **ptr1;                /* Pointer to mask pointer array */
   double **ptr_base;            /* Pointer to axis values for "pset_base" */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *cen_orig;             /* Pointer to array holding original centre coords */
   double *mask;                 /* Pointer to mask axis values */
   int coord;                    /* Zero-based index for coordinates */
   int ncoord_base;              /* No. of coordinates per base Frame point */
   int ncoord_out;               /* No. of coordinates per output point */
   int npoint;                   /* No. of supplied input test points */
   int nrp;                      /* No. of points in Region PointSet */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Avoid -Wall compiler warnings. */
   ps1 = NULL;
   ps2 = NULL;

/* Obtain a pointer to the Region structure. */
   this = (AstRegion *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet to transform the supplied positions
   from the current Frame in the encapsulated FrameSet (the Frame
   represented by the Region), to the base Frame (the Frame in which the
   Region is defined). */
   in_base = astRegTransform( this, in, 0, NULL, NULL );

/* The PointSet pointer returned above may be a clone of the "in"
   pointer. If so take a copy of the PointSet so we can change it safely. */
   if( in_base == in ) {
      ps3 = astCopy( in_base );
      (void) astAnnul( in_base );
      in_base = ps3;
      ps3 = NULL;
   }

/* Determine the numbers of points and coordinates per point from the base
   Frame PointSet and obtain pointers for accessing the base Frame and output
   coordinate values. */
   npoint = astGetNpoint( in_base );
   ncoord_base = astGetNcoord( in_base );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* Get the axis values for the PointSet which defines the location and
   extent of the region in the base Frame, and check them. */
   pset_reg = this->points;
   nrp = astGetNpoint( pset_reg );
   if( astGetNcoord( pset_reg ) != ncoord_base && astOK ) {
      astError( AST__INTER, "astTransform(PointList): Illegal number of "
                "coords (%d) in the Region - should be %d "
                "(internal AST programming error).", status, astGetNcoord( pset_reg ),
                ncoord_base );
   }

/* Get the base Frame uncertainty Region. Temporarily set its negated flag. */
   unc = astGetUncFrm( this, AST__BASE );
   astSetNegated( unc, 1 );

/* Transform the PointList PointSet into the base Frame of the uncertainty
   Region, and get pointers to the corresponding axis value. */
   pset_base = astRegTransform( unc, pset_reg, 0, NULL, NULL );
   ptr_base = astGetPoints( pset_base );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* Save the original base Frame centre coords of the uncertainty Region. */
      cen_orig = astRegCentre( unc, NULL, NULL, 0, AST__BASE );

/* We use the PointSet created above as the initial input to astTransform
   below. Also indicate we currently have no output PointSet. This will
   cause a new PointSet to be created on the first pass through the loop
   below. */
      ps1 = astClone( in_base );
      ps2 = NULL;

/* Loop round all the points in the PointList. */
      for ( point = 0; point < nrp; point++ ) {

/* Centre the uncertainty Region at this PointList position. Note, the
   base Frame of the PointList should be the same as the current Frame
   of the uncertainty Region. */
         astRegCentre( unc, NULL, ptr_base, point, AST__BASE );

/* Use the uncertainty Region to transform the supplied PointSet. This
   will set supplied points bad if they are within the uncertainty Region
   (since the uncertainty Region has been negated above). */
         ps2 = astTransform( unc, ps1, 0, ps2 );

/* Use the output PointSet created above as the input for the next
   position. This causes bad points to be accumulated in the output
   PointSet. */
         ps3 = ps2;
         ps2 = ps1;
         ps1 = ps3;

      }

/* Re-instate the original centre coords of the uncertainty Region. */
      astRegCentre( unc, cen_orig, NULL, 0, AST__BASE );
      cen_orig = astFree( cen_orig );

/* The ps1 PointSet will now be a copy of the supplied PointSet but with
   positions set to bad if they are inside any of the re-centred uncertainty
   Regions.  If this PointList has been negated, this is what we want so
   we just transfer this bad position mask to the result PointSet. If this
   PointList has not been negated we need to invert the bad position
   mask. Get a pointer to the first axis of the resulting PointSet. */
      ptr1 = astGetPoints( ps1 );
      if( astOK ) {
         mask = ptr1[ 0 ];

/* Apply the mask to the returned PointSet, inverted if necessary. */
         if( astGetNegated( this ) ) {
            for ( point = 0; point < npoint; point++, mask++ ) {
               if( *mask == AST__BAD ) {
                  for( coord = 0; coord < ncoord_out; coord++ ) {
                     ptr_out[ coord ][ point ] = AST__BAD;
                  }
               }
            }

         } else {
            for ( point = 0; point < npoint; point++, mask++ ) {
               if( *mask != AST__BAD ) {
                  for( coord = 0; coord < ncoord_out; coord++ ) {
                     ptr_out[ coord ][ point ] = AST__BAD;
                  }
               }
            }
         }
      }
   }

/* Clear the negated flag for the uncertainty Region. */
   astClearNegated( unc );

/* Free resources */
   in_base = astAnnul( in_base );
   pset_base = astAnnul( pset_base );
   unc = astAnnul( unc );
   if( ps2 ) ps2 = astAnnul( ps2 );
   if( ps1 ) ps1 = astAnnul( ps1 );

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;
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
*     ListSize

*  Purpose:
*     Number of points in a PointList.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This is a read-only attribute giving the number of points in a
*     PointList. This value is determined when the PointList is created.

*  Applicability:
*     PointList
*        All PointLists have this attribute.
*att--
*/


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for PointList objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for PointList objects.

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
   AstPointList *in;             /* Pointer to input PointList */
   AstPointList *out;            /* Pointer to output PointList */
   int nb;                       /* Number of bytes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output PointLists. */
   in = (AstPointList *) objin;
   out = (AstPointList *) objout;

/* For safety, first clear any references to the input memory from
   the output PointList. */
   out->lbnd = NULL;
   out->ubnd = NULL;

/* Copy dynamic memory contents */
   if( in->lbnd && in->ubnd ) {
      nb = sizeof( double )*astGetNaxes( in );
      out->lbnd = astStore( NULL, in->lbnd, nb );
      out->ubnd = astStore( NULL, in->ubnd, nb );
   }
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for PointList objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for PointList objects.

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
   AstPointList *this;                 /* Pointer to PointList */

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) obj;

/* Annul all resources. */
   this->lbnd = astFree( this->lbnd );
   this->ubnd = astFree( this->ubnd );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for PointList objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the PointList class to an output Channel.

*  Parameters:
*     this
*        Pointer to the PointList whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPointList *this;                 /* Pointer to the PointList structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointList structure. */
   this = (AstPointList *) this_object;

/* Write out values representing the instance variables for the
   PointList class.  Accompany these with appropriate comment strings,
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

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPointList and astCheckPointList functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(PointList,Region)
astMAKE_CHECK(PointList)

AstPointList *astPointList_( void *frame_void, AstPointSet *points,
                             AstRegion *unc, const char *options,
                             int *status, ...) {
/*
*+
*  Name:
*     astPointList

*  Purpose:
*     Create a PointList.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointlist.h"
*     AstPointList *astPointList( AstFrame *frame, AstPointSet *points,
*                                 AstRegion *unc, const char *options,
*                                 int *status, ...) {

*  Class Membership:
*     PointList constructor.

*  Description:
*     This function implements the protected interface to the astPointList
*     constructor function, returning a true C pointer. The parameter list
*     differs from the public constructor, in that the positions are
*     defined by a PointSet rather than an array of doubles.

*  Parameters:
*     frame
*        A pointer to the Frame in which the region is defined. A deep
*        copy is taken of the supplied Frame. This means that any
*        subsequent changes made to the Frame using the supplied pointer
*        will have no effect the Region.
*     points
*        A PointSet holding the physical coordinates of the points.
*     unc
*        An optional pointer to an existing Region which specifies the
*        uncertainties associated with each point in the PointList being
*        created. The uncertainty at any point in the PointList is found by
*        shifting the supplied "uncertainty" Region so that it is centred at
*        the point being considered. The area covered by the shifted
*        uncertainty Region then represents the uncertainty in the position.
*        The uncertainty is assumed to be the same for all points.
*
*        If supplied, the uncertainty Region must be of a class for which
*        all instances are centro-symetric (e.g. Box, Circle, Ellipse, etc.)
*        or be a Prism containing centro-symetric component Regions. A deep
*        copy of the supplied Region will be taken, so subsequent changes to
*        the uncertainty Region using the supplied pointer will have no
*        effect on the created Box. Alternatively, a NULL Object pointer
*        may be supplied, in which case a default uncertainty is used
*        equivalent to a box 1.0E-6 of the size of the bounding box of the
*        PointList being created.
*
*        The uncertainty Region has two uses: 1) when the astOverlap
*        function compares two Regions for equality the uncertainty Region
*        is used to determine the tolerance on the comparison, and 2)
*        when a Region is mapped into a different coordinate system and
*        subsequently simplified (using astSimplify), the uncertainties are
*        used to determine if the transformed boundary can be accurately
*        represented by a specific shape of Region.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new PointList. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status value.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new PointList.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPointList *new;            /* Pointer to new PointList */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the PointList, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointList( NULL, sizeof( AstPointList ), !class_init,
                           &class_vtab, "PointList", frame, points, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PointList's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PointList. */
   return new;
}

AstPointList *astPointListId_( void *frame_void, int npnt, int ncoord, int dim,
                               const double *points, void *unc_void,
                               const char *options, ... ) {
/*
*++
*  Name:
c     astPointList
f     AST_POINTLIST

*  Purpose:
*     Create a PointList.

*  Type:
*     Public function.

*  Synopsis:
c     #include "pointlist.h"
c     AstPointList *astPointList( AstFrame *frame, int npnt, int ncoord, int dim,
c                                 const double *points, AstRegion *unc,
c                                 const char *options, ... )
f     RESULT = AST_POINTLIST( FRAME, NPNT, COORD, DIM, POINTS, UNC, OPTIONS, STATUS )

*  Class Membership:
*     PointList constructor.

*  Description:
*     This function creates a new PointList object and optionally initialises
*     its attributes.
*
*     A PointList object is a specialised type of Region which represents a
*     collection of points in a coordinate Frame.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. A deep
*        copy is taken of the supplied Frame. This means that any
*        subsequent changes made to the Frame using the supplied pointer
*        will have no effect the Region.
c     npnt
f     NPNT = INTEGER (Given)
*        The number of points in the Region.
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of coordinates being supplied for each point. This
*        must equal the number of axes in the supplied Frame, given by
*        its Naxes attribute.
c     dim
f     DIM = INTEGER (Given)
c        The number of elements along the second dimension of the "points"
f        The number of elements along the first dimension of the POINTS
*        array (which contains the point coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
c        given should not be less than "npnt".
f        given should not be less than NPNT.
c     points
f     POINTS( DIM, NCOORD ) = DOUBLE PRECISION (Given)
c        The address of the first element of a 2-dimensional array of
c        shape "[ncoord][dim]" giving the physical coordinates of the
c        points. These should be stored such that the value of coordinate
c        number "coord" for point number "pnt" is found in element
c        "in[coord][pnt]".
f        A 2-dimensional array giving the physical coordinates of the
f        points. These should be stored such that the value of coordinate
f        number COORD for point number PNT is found in element IN(PNT,COORD).
c     unc
f     UNC = INTEGER (Given)
*        An optional pointer to an existing Region which specifies the uncertainties
*        associated with each point in the PointList being created. The
*        uncertainty at any point in the PointList is found by shifting the
*        supplied "uncertainty" Region so that it is centred at the point
*        being considered. The area covered by the shifted uncertainty Region
*        then represents the uncertainty in the position. The uncertainty is
*        assumed to be the same for all points.
*
*        If supplied, the uncertainty Region must be of a class for which
*        all instances are centro-symetric (e.g. Box, Circle, Ellipse, etc.)
*        or be a Prism containing centro-symetric component Regions. A deep
*        copy of the supplied Region will be taken, so subsequent changes to
*        the uncertainty Region using the supplied pointer will have no
*        effect on the created Box. Alternatively,
f        a null Object pointer (AST__NULL)
c        a NULL Object pointer
*        may be supplied, in which case a default uncertainty is used
*        equivalent to a box 1.0E-6 of the size of the bounding box of the
*        PointList being created.
*
*        The uncertainty Region has two uses: 1) when the
c        astOverlap
f        AST_OVERLAP
*        function compares two Regions for equality the uncertainty
*        Region is used to determine the tolerance on the comparison, and 2)
*        when a Region is mapped into a different coordinate system and
*        subsequently simplified (using
c        astSimplify),
f        AST_SIMPLIFY),
*        the uncertainties are used to determine if the transformed boundary
*        can be accurately represented by a specific shape of Region.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new PointList. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new PointList. The syntax used is identical to that for the
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
c     astPointList()
f     AST_POINTLIST = INTEGER
*        A pointer to the new PointList.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   AstFrame *frame;          /* Pointer to Frame structure */
   AstPointList *new;        /* Pointer to new PointList */
   AstPointSet *pset;        /* Pointer to PointSet holding points */
   AstRegion *unc;           /* Pointer to Region structure */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */
   const double *q;          /* Pointer to next supplied axis value */
   double **ptr;             /* Pointer to data in pset */
   double *p;                /* Pointer to next PointSet axis value */
   int *status;              /* Pointer to inherited status value */
   int i;                    /* Axis index */
   int j;                    /* Point index */
   va_list args;             /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain a Frame pointer from the supplied ID and validate the
   pointer to ensure it identifies a valid Frame. */
   frame = astVerifyFrame( astMakePointer( frame_void ) );

/* Create a PointSet and store the supplied points in it. */
   pset = astPointSet( npnt, ncoord , "", status );
   ptr = astGetPoints( pset );
   if( astOK ) {
      for( i = 0; i < ncoord; i++ ) {
         p = ptr[ i ];
         q = points + i*dim;
         for( j = 0; j < npnt; j++ ) *(p++) = *(q++);
      }
   }

/* Obtain a Region pointer from the supplied "unc" ID and validate the
   pointer to ensure it identifies a valid Region . */
   unc = unc_void ? astCheckRegion( astMakePointer( unc_void ) ) : NULL;

/* Initialise the PointList, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointList( NULL, sizeof( AstPointList ), !class_init,
                           &class_vtab, "PointList", frame, pset, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PointList's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Free resources. */
   pset = astAnnul( pset );

/* Return an ID value for the new PointList. */
   return astMakeId( new );
}

AstPointList *astInitPointList_( void *mem, size_t size, int init,
                                 AstPointListVtab *vtab, const char *name,
                                 AstFrame *frame, AstPointSet *points,
                                 AstRegion *unc, int *status ) {
/*
*+
*  Name:
*     astInitPointList

*  Purpose:
*     Initialise a PointList.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointlist.h"
*     AstPointList *astInitPointList( void *mem, size_t size, int init,
*                                     AstPointListVtab *vtab, const char *name,
*                                     AstFrame *frame, AstPointSet *points,
*                                     AstRegion *unc, int *status )

*  Class Membership:
*     PointList initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new PointList object. It allocates memory (if necessary) to accommodate
*     the PointList plus any additional data associated with the derived class.
*     It then initialises a PointList structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a PointList at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the PointList is to be initialised.
*        This must be of sufficient size to accommodate the PointList data
*        (sizeof(PointList)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the PointList (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the PointList
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the PointList's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new PointList.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     frame
*        A pointer to the Frame in which the region is defined.
*     points
*        A PointSet containing the Points for the PointList.
*     unc
*        A pointer to a Region which specifies the uncertainty in the
*        supplied positions (all points in the new PointList being
*        initialised are assumed to have the same uncertainty). A NULL
*        pointer can be supplied, in which case default uncertainties equal
*        to 1.0E-6 of the dimensions of the new PointList's bounding box are
*        used. If an uncertainty Region is supplied, it must be either a Box,
*        a Circle or an Ellipse, and its encapsulated Frame must be related
*        to the Frame supplied for parameter "frame" (i.e. astConvert
*        should be able to find a Mapping between them). Two positions
*        the "frame" Frame are considered to be co-incident if their
*        uncertainty Regions overlap. The centre of the supplied
*        uncertainty Region is immaterial since it will be re-centred on the
*        point being tested before use. A deep copy is taken of the supplied
*        Region.

*  Returned Value:
*     A pointer to the new PointList.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPointList *new;        /* Pointer to new PointList */
   int ncoord;               /* No. of axes in PointSet */
   int nin;                  /* No. of axes in Frame */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPointListVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the number of axis values per position is correct. */
   nin = astGetNaxes( frame );
   ncoord = astGetNcoord( points );
   if( nin != ncoord ) {
      astError( AST__NCPIN, "astInitPointList(): Bad number of coordinate "
                "values (%d).", status, ncoord );
      astError( AST__NCPIN, "The %s given requires %d coordinate value(s) for "
                "each input point.", status, astGetClass( frame ), nin );
   }

/* Initialise a Region structure (the parent class) as the first component
   within the PointList structure, allocating memory if necessary. */
   if( astOK ) {
      new = (AstPointList *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                            name, frame, points, unc );
      if ( astOK ) {

/* Initialise the PointList data. */
/* ------------------------------ */
         new->lbnd = NULL;
         new->ubnd = NULL;

/* If an error occurred, clean up by deleting the new PointList. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new PointList. */
   return new;
}

AstPointList *astLoadPointList_( void *mem, size_t size, AstPointListVtab *vtab,
                                 const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPointList

*  Purpose:
*     Load a PointList.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointlist.h"
*     AstPointList *astLoadPointList( void *mem, size_t size, AstPointListVtab *vtab,
*                                     const char *name, AstChannel *channel )

*  Class Membership:
*     PointList loader.

*  Description:
*     This function is provided to load a new PointList using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     PointList structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a PointList at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the PointList is to be
*        loaded.  This must be of sufficient size to accommodate the
*        PointList data (sizeof(PointList)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the PointList (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the PointList structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPointList) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new PointList. If this is NULL, a pointer
*        to the (static) virtual function table for the PointList class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "PointList" is used instead.

*  Returned Value:
*     A pointer to the new PointList.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPointList *new;              /* Pointer to the new PointList */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this PointList. In this case the
   PointList belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPointList );
      vtab = &class_vtab;
      name = "PointList";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPointListVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built PointList. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "PointList" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* If an error occurred, clean up by deleting the new PointList. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new PointList pointer. */
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

int astGetListSize_( AstPointList *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointList,GetListSize))( this, status );
}
void astPointListPoints_( AstPointList *this, AstPointSet **pset, int *status) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointList,PointListPoints))( this, pset, status );
   return;
}

