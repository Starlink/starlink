/*
*  Name:
*     pointset.c

*  Purpose:
*     Implement the PointSet class.

*  Description:
*     This file implements the PointSet class. For a description of
*     the class and its interface, see the .h file of the same name.

*  Inheritance:
*     The PointSet class inherits from the Object class.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     1-FEB-1996 (RFWS):
*        Original version.
*     27-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS PointSet

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */
/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPointSetVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPointSet *astPointSetId_( int, int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static const char *GetAttrib( AstObject *, const char * );
static double **GetPoints( AstPointSet * );
static int GetNcoord( const AstPointSet * );
static int GetNpoint( const AstPointSet * );
static int TestAttrib( AstObject *, const char * );
static void ClearAttrib( AstObject *, const char * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void InitVtab( AstPointSetVtab * );
static void SetAttrib( AstObject *, const char * );
static void SetPoints( AstPointSet *, double ** );
static void SetSubPoints( AstPointSet *, int, int, AstPointSet * );

/* Member functions. */
/* ================= */
static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     PointSet member function (over-rides the astClearAttrib
*     protected method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for a
*     PointSet, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then report an error. */
   if ( !strcmp( attrib, "ncoord" ) ||
        !strcmp( attrib, "npoint" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     PointSet member function (over-rides the protected astGetAttrib
*     method inherited from the Object class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a PointSet, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the PointSet, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the PointSet. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   const char *result;           /* Pointer value to return */
   int ncoord;                   /* Ncoord attribute value */
   int npoint;                   /* Npoint attribute value */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Ncoord. */
/* ------- */
   if ( !strcmp( attrib, "ncoord" ) ) {
      ncoord = astGetNcoord( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ncoord );
         result = buff;
      }

/* Npoint. */
/* ------- */
   } else if ( !strcmp( attrib, "npoint" ) ) {
      npoint = astGetNpoint( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", npoint );
         result = buff;
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

static int GetNcoord( const AstPointSet *this ) {
/*
*+
*  Name:
*     astGetNcoord

*  Purpose:
*     Get the number of coordinate values per point from a PointSet.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "pointset.h"
*     int astGetNcoord( const AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns the number of coordinate values per point (1 or
*     more) for a PointSet.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     The number of coordinate values per point.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of coordinate values. */
   return this->ncoord;
}

static int GetNpoint( const AstPointSet *this ) {
/*
*+
*  Name:
*     astGetNpoint

*  Purpose:
*     Get the number of points in a PointSet.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "pointset.h"
*     int astGetNpoint( const AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns the number of points (1 or more) in a PointSet.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     The number of points.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of points. */
   return this->npoint;
}

static double **GetPoints( AstPointSet *this ) {
/*
*+
*  Name:
*     astGetPoints

*  Purpose:
*     Get a pointer for the coordinate values associated with a PointSet.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "pointset.h"
*     double **astGetPoints( AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns a pointer which grants access to the coordinate
*     values associated with a PointSet. If the PointSet has previously had
*     coordinate values associated with it, this pointer will identify these
*     values. Otherwise, it will point at a newly-allocated region of memory
*     (associated with the PointSet) in which new coordinate values may be
*     stored.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     A pointer to an array of type double* with ncoord elements (where ncoord
*     is the number of coordinate values per point). Each element of this array
*     points at an array of double, of size npoint (where npoint is the number
*     of points in the PointSet), containing the values of that coordinate for
*     each point in the set. Hence, the value of the i'th coordinate for the
*     j'th point (where i and j are counted from zero) is given by ptr[i][j]
*     where ptr is the returned pointer value.

*  Notes:
*     -  The returned pointer points at an array of pointers allocated
*     internally within the PointSet. The values in this array may be changed
*     by the caller, who is reponsible for ensuring that they continue to
*     point at valid arrays of coordinate values.
*     -  No attempt should be made to de-allocate memory allocated by a
*     PointSet to store coordinate values or pointers to them. This memory
*     will be freed when the PointSet is deleted.
*     -  No count is kept of the number of pointers issued for the PointSet
*     coordinate values. The caller must keep track of these.
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int i;                        /* Loop counter for coordinates */
   int nval;                     /* Number of values to be stored */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* If the PointSet has an existing array of pointers (which point at coordinate
   values), we will simply return a pointer to it. Otherwise, we must allocate
   space to hold new coordinate values. */
   if( !this->ptr ) {

/* Determine the number of coordinate values to be stored and allocate memory
   to hold them, storing the pointer to this values array in the PointSet
   structure. */
      nval = this->npoint * this->ncoord;
      this->values = (double *) astMalloc( sizeof( double ) * (size_t) nval );

/* If OK, also allocate memory for the array of pointers into this values
   array, storing a pointer to this pointer array in the PointSet structure. */
      if ( astOK ) {
         this->ptr = (double **) astMalloc( sizeof( double * )
                                            * (size_t) this->ncoord );

/* If OK, initialise the pointer array to point into the values array. */
         if ( astOK ) {
            for ( i = 0; i < this->ncoord; i++ ) {
               this->ptr[ i ] = this->values + ( i * this->npoint );
            }

/* If we failed to allocate the pointer array, then free the values array. */
         } else {
            this->values = (double *) astFree( (void *) this->values );
         }
      }
   }

/* Return the required pointer. */
   return this->ptr;
}

static void InitVtab( AstPointSetVtab *vtab ) {
/*
*  Name:
*     InitVtab

*  Purpose:
*     Initialise a virtual function table for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     void InitVtab( AstPointSetVtab *vtab )

*  Class Membership:
*     PointSet member function.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the PointSet class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes should already have been initialised.
*/

/* Local Variables: */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAPointSet) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->GetNcoord = GetNcoord;
   vtab->GetNpoint = GetNpoint;
   vtab->GetPoints = GetPoints;
   vtab->SetPoints = SetPoints;
   vtab->SetSubPoints = SetSubPoints;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "PointSet", "Container for a set of points" );
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     astSetAttrib

*  Purpose:
*     Set an attribute value for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     PointSet member function (over-rides the astSetAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for a PointSet, the
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
*        Pointer to the PointSet.
*     setting
*        Pointer to a null-terminated string specifying the new
*        attribute value.
*/

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by sscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "sscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == sscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   if ( MATCH( "ncoord" ) ||
        MATCH( "npoint" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.",
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetPoints( AstPointSet *this, double **ptr ) {
/*
*+
*  Name:
*     astSetPoints

*  Purpose:
*     Associate coordinate values with a PointSet.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astSetPoints( AstPointSet *this, double **ptr )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function associates coordinate values with a PointSet by storing an
*     array of pointers to the values within the PointSet object. A pointer to
*     this pointer array will later be returned when astGetPoints is used to
*     locate the coordinate values. If values are already associated with the
*     PointSet, the array of pointers to them is over-written by the new values
*     (any internally allocated memory holding the actual coordinate values
*     first being freed).

*  Parameters:
*     this
*        Pointer to the PointSet.
*     ptr
*        Pointer to an array of type double* with ncoord elements (where ncoord
*        is the number of coordinate values per point in the PointSet). Each
*        element of this array should point at an array of double with npoint
*        elements (where npoint is the number of points in the PointSet),
*        containing the values of that coordinate for each point in the set.
*        Hence, the value of the i'th coordinate for the j'th point (where i
*        and j are counted from zero) should be given by ptr[i][j].

*  Returned Value:
*     void

*  Notes:
*     -  It is the caller's responsibility to ensure that the pointer supplied
*     points at a valid array of pointers that point at arrays of coordinate
*     values. This is only superficially validated by this function, which then
*     simply stores a copy of the supplied array of pointers for later use.
*     The caller must also manage any allocation (and freeing) of memory for
*     these coordinate values.
*     -  This functon makes a copy of the array of pointers supplied, but does
*     not copy the coordinate values they point at. If a PointSet containing a
*     copy of the coordinate values is required, internal memory should be
*     allocated within the PointSet by calling astGetPoints before storing any
*     pointer, and then copying the values into this memory. Alternatively,
*     using astCopy to produce a deep copy of a PointSet will also copy the
*     coordinate values.
*     -  A NULL pointer may be supplied as the "ptr" argument, in which case
*     any previously stored array of pointers will be cancelled (and internal
*     memory freed if necessary) and subsequent use of astGetPoints will then
*     cause memory to be allocated internally by the PointSet to hold new
*     values.
*-
*/

/* Local Variables: */
   int i;                        /* Loop counter for coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* If the pointer supplied is not NULL, inspect each pointer in the array it
   points at to check that none of these are NULL. This validates (to some
   extent) the caller's data structure. Report an error and quit checking if a
   NULL pointer is found. */
   if ( ptr ) {
      for ( i = 0; i < this->ncoord; i++ ) {
         if ( !ptr[ i ] ) {
            astError( AST__PDSIN, "astSetPoints(%s): Invalid NULL pointer in "
                      "element %d of array of pointers to coordinate values.",
                      astGetClass( this ), i );
            break;
         }
      }
   }

/* Do not carry on if the data structure is obviously invalid. */
   if ( astOK ) {

/* Free any memory previously allocated to store coordinate values. */
      this->values = (double *) astFree( (void *) this->values );

/* If a new array of pointers has been provided, (re)allocate memory and store
   a copy of the array in it, saving a pointer to this copy in the PointSet
   structure. */
      if ( ptr ) {
         this->ptr = (double **) astStore( (void *) this->ptr,
                                           (const void *) ptr,
                                           sizeof( double * )
                                           * (size_t) this->ncoord );

/* If no pointer array was provided, free the previous one (if any). */
      } else {
         this->ptr = (double **) astFree( (void *) this->ptr );
      }
   }
}

static void SetSubPoints( AstPointSet *point1, int point, int coord,
                          AstPointSet *point2 ) {
/*
*+
*  Name:
*     astSetSubPoints

*  Purpose:
*     Associate a subset of one PointSet with another PointSet.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astSetSubPoints( AstPointSet *point1, int point, int coord,
*                           AstPointSet *point2 )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function selects a subset of the coordinate values associated with
*     one PointSet and associates them with another PointSet. The second
*     PointSet may then be used to access the subset. Any previous coordinate
*     value association with the second PointSet is replaced.

*  Parameters:
*     point1
*        Pointer to the first PointSet, from which a subset is to be selected.
*     point
*        The index of the first point (counting from zero) which is to appear
*        in the subset (the number of points is determined by the size of the
*        second PointSet).
*     coord
*        The index of the first coordinate (counting from zero) which is to
*        appear in the subset (the number of coordinates is determined by the
*        size of the second PointSet).
*     point2
*        Second PointSet, with which the subset of coordinate values is to be
*        associated.

*  Returned Value:
*     void

*  Notes:
*     -  The range of points and coordinates selected must lie entirely within
*     the first PointSet.
*     -  This function does not make a copy of the coordinate values, but
*     merely stores pointers to the required subset of values associated with
*     the first PointSet. If a PointSet containing a copy of the subset's
*     coordinate values is required, then astCopy should be used to make a
*     deep copy from the second PointSet.
*     -  If the first PointSet does not yet have coordinate values associated
*     with it, then space will be allocated within it to hold values (so that
*     the second PointSet has somewhere to point at).
*-
*/

/* Local Variables: */
   double ** ptr2;               /* Pointer to new pointer array */
   double **ptr1;                /* Pointer to original pointer array */
   int i;                        /* Loop counter for coordinates */
   int ncoord1;                  /* Number of coordinates in first PointSet */
   int ncoord2;                  /* Number of coordinates in second PointSet */
   int npoint1;                  /* Number of points in first PointSet */
   int npoint2;                  /* Number of points in second PointSet */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the sizes of both PointSets. */
   npoint1 = astGetNpoint( point1 );
   npoint2 = astGetNpoint( point2 );
   ncoord1 = astGetNcoord( point1 );
   ncoord2 = astGetNcoord( point2 );

/* Check if the range of points required lies within the first PointSet and
   report an error if it does not. */
   if ( astOK ) {
      if ( ( point < 0 ) || ( point + npoint2 > npoint1 ) ) {
         astError( AST__PTRNG, "astSetSubPoints(%s): Range of points in "
                   "output %s (%d to %d) lies outside the input %s extent "
                   "(0 to %d).",
                   astGetClass( point1 ), astGetClass( point2 ), point,
                   point + npoint2, astGetClass( point1 ), npoint1 );

/* Similarly check that the range of coordinates is valid. */
      } else if ( ( coord < 0 ) || ( coord + ncoord2 > ncoord1 ) ) {
         astError( AST__CORNG, "astSetSubPoints(%s): Range of coordinates in "
                   "output %s (%d to %d) lies outside the input %s extent "
                   "(0 to %d).",
                   astGetClass( point1 ), astGetClass( point2 ), coord,
                   coord + ncoord2, astGetClass( point1 ), ncoord1 );

/* Obtain a pointer for the coordinate values associated with the first
   PointSet (this will cause internal memory to be allocated if it is not
   yet associated with coordinate values). */
      } else {
         ptr1 = astGetPoints( point1 );

/* Allocate a temporary array to hold new pointer values. */
         ptr2 = (double **) astMalloc( sizeof( double * ) * (size_t) ncoord2 );

/* Initialise this pointer array to point at the required subset of coordinate
   values. */
         if ( astOK ) {
            for ( i = 0; i < ncoord2; i++ ) {
               ptr2[ i ] = ptr1[ i + coord ] + point;
            }

/* Associate the second PointSet with this new pointer array. This will free
   any internally allocated memory and replace any existing coordinate value
   association. */
            astSetPoints( point2, ptr2 );
	 }

/* Free the temporary pointer arry. */
         ptr2 = (double **) astFree( (void * ) ptr2 );
      }
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     PointSet member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate
*     whether a value has been set for one of a PointSet's attributes.

*  Parameters:
*     this
*        Pointer to the PointSet.
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
   AstPointSet *this;            /* Pointer to the PointSet structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then return zero. */
   if ( !strcmp( attrib, "ncoord" ) ||
        !strcmp( attrib, "npoint" ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for PointSet objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy, including a copy of the coordinate
*     values (if any) associated with the input PointSet.
*/

/* Local Variables: */
   AstPointSet *in;              /* Pointer to input PointSet */
   AstPointSet *out;             /* Pointer to output PointSet */
   int i;                        /* Loop counter for coordinates */
   int nval;                     /* Number of values to store */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output PointSets. */
   in = (AstPointSet *) objin;
   out = (AstPointSet *) objout;

/* For safety, first clear any references to the input coordinate values from
   the output PointSet. */
   out->ptr = NULL;
   out->values = NULL;

/* If the input PointSet is associated with coordinate values, we must
   allocate memory in the output PointSet to hold a copy of them. */
   if ( in->ptr ) {

/* Determine the number of coordinate values to be stored and allocate memory
   to hold them, storing a pointer to this memory in the output PointSet. */
      nval = in->npoint * in->ncoord;
      out->values = (double *) astMalloc( sizeof( double ) * (size_t) nval );

/* If OK, also allocate memory for the array of pointers into this values
   array, storing a pointer to this pointer array in the output PointSet. */
      if ( astOK ) {
         out->ptr = (double **) astMalloc( sizeof( double * )
                                           * (size_t) in->ncoord );

/* If OK, initialise the new pointer array. */
         if ( astOK ) {
            for ( i = 0; i < in->ncoord; i++ ) {
               out->ptr[ i ] = out->values + ( i * in->npoint );
            }

/* If we failed to allocate the pointer array, then free the values array. */
         } else {
            out->values = (double *) astFree( (void *) out->values );
         }
      }

/* Copy the values for each coordinate from the input to the output. Use a
   memory copy to avoid floating point errors if the data are
   un-initialised. */
      if ( astOK ) {
         for ( i = 0; i < in->ncoord; i++ ) {
            (void) memcpy( (void *) out->ptr[ i ],
                           (const void *) in->ptr[ i ],
                           sizeof( double ) * (size_t) in->npoint );
         }
      }
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for PointSet objects.

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
   AstPointSet *this;            /* Pointer to PointSet */

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) obj;

/* Free any pointer array and associated coordinate values array, */
   this->ptr = (double **) astFree( (void *) this->ptr );
   this->values = (double *) astFree( (void *) this->values );

/* Clear the remaining PointSet variables. */
   this->npoint = 0;
   this->ncoord = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the PointSet class to an output Channel.

*  Parameters:
*     this
*        Pointer to the PointSet whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.

*  Notes:
*     - It is not recommended that PointSets containing large numbers
*     of points be written out, as the coordinate data will be
*     formatted as text and this will not be very efficient.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   int coord;                    /* Loop counter for coordinates */
   int i;                        /* Counter for coordinate values */
   int ival;                     /* Integer value */
   int point;                    /* Loop counter for points */
   int set;                      /* Attribute value set? */
   
/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Write out values representing the instance variables for the
   PointSet class.  Accompany these with appropriate comment strings,
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

/* Npoint. */
/* ------- */
   astWriteInt( channel, "Npoint", 1, 1, this->npoint,
                "Number of points" );

/* Ncoord. */
/* ------- */
   astWriteInt( channel, "Ncoord", 1, 1, this->ncoord,
                "Number of coordinates per point" );

/* Coordinate data. */
/* ---------------- */
/* Write an "Empty" value to indicate whether or not the PointSet
   contains data. */
   ival = ( this->ptr == NULL );
   set = ( ival != 0 );
   astWriteInt( channel, "Empty", set, 0, ival,
                ival ? "PointSet is empty" :
                       "PointSet contains data" );

/* If it contains data, create a suitable keyword for each coordinate
   value in turn. */
   if ( this->ptr ) {
      i = 0;
      for ( point = 0; point < this->npoint; point++ ) {
         for ( coord = 0; coord < this->ncoord; coord++ ) {
            i++;
            (void) sprintf( key, "X%d", i );

/* Write the value out. Only supply a comment for the first value. */
            astWriteDouble( channel, key, 1, 1, this->ptr[ coord ][ point ],
                            ( i == 1 ) ? "Coordinate values..." : "" );
         }
      }
   }

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPointSet and astCheckPointSet functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(PointSet,Object,check,&class_init)
astMAKE_CHECK(PointSet)

AstPointSet *astPointSet_( int npoint, int ncoord, const char *options, ... ) {
/*
*+
*  Name:
*     astPointSet

*  Purpose:
*     Create a PointSet.

*  Type:
*     Public function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astPointSet( int npoint, int ncoord,
*                               const char *options, ... )

*  Class Membership:
*     PointSet constructor.

*  Description:
*     This function creates a new PointSet and optionally initialises its
*     attributes.

*  Parameters:
*     npoint
*        The number of points to be stored in the PointSet (must be at
*        least 1).
*     ncoord
*        The number of coordinate values associated with each point
*        (must be at least 1).
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new PointSet. The syntax used is the same as
*        for the astSet method and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of arguments may follow it in order to
*        supply values to be substituted for these specifiers. The
*        rules for supplying these are identical to those for the
*        astSet method (and for the C "printf" function).

*  Returned Value:
*     A pointer to the new PointSet.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstPointSet *new;             /* Pointer to new PointSet */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PointSet, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointSet( NULL, sizeof( AstPointSet ), !class_init,
                          &class_vtab, "PointSet", npoint, ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   PointSet's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PointSet. */
   return new;
}

AstPointSet *astPointSetId_( int npoint, int ncoord,
                             const char *options, ... ) {
/*
*  Name:
*     astPointSetId_

*  Purpose:
*     Create a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astPointSetId_( int npoint, int ncoord,
*                                  const char *options, ... )

*  Class Membership:
*     PointSet constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astPointSet constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astPointSet_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*
*     The variable argument list also prevents this function from
*     invoking astPointSet_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.

*  Parameters:
*     As for astPointSet_.

*  Returned Value:
*     The ID value associated with the new PointSet.
*/

/* Local Variables: */
   AstPointSet *new;             /* Pointer to new PointSet */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PointSet, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointSet( NULL, sizeof( AstPointSet ), !class_init,
                          &class_vtab, "PointSet", npoint, ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   PointSet's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new PointSet. */
   return astMakeId( new );
}

AstPointSet *astInitPointSet_( void *mem, size_t size, int init,
                               AstPointSetVtab *vtab, const char *name,
                               int npoint, int ncoord ) {
/*
*+
*  Name:
*     astInitPointSet

*  Purpose:
*     Initialise a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astInitPointSet( void *mem, size_t size, int init,
*                                   AstPointSetVtab *vtab, const char *name,
*                                   int npoint, int ncoord )

*  Class Membership:
*     PointSet initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new PointSet object. It allocates memory (if necessary) to accommodate
*     the PointSet plus any additional data associated with the derived class.
*     It then initialises a PointSet structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a PointSet at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the PointSet is to be created. This
*        must be of sufficient size to accommodate the PointSet data
*        (sizeof(PointSet)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the PointSet (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the PointSet
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the PointSet's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new PointSet.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     npoint
*        The number of points in the PointSet (must be at least 1).
*     ncoord
*        The number of coordinate values associated with each point (must be
*        at least 1).

*  Returned Value:
*     A pointer to the new PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPointSet *new;             /* Pointer to new PointSet */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   new = NULL;

/* Check the initialisation values for validity, reporting an error if
   necessary. */
   if ( npoint < 1 ) {
      astError( AST__NPTIN, "astInitPointSet(%s): Number of points (%d) is "
                "not valid.", name, npoint );
   } else if ( ncoord < 1 ) {
      astError( AST__NCOIN, "astInitPointSet(%s): Number of coordinates per "
                "point (%d) is not valid.", name, ncoord );
   }

/* Initialise an Object structure (the parent class) as the first component
   within the PointSet structure, allocating memory if necessary. */
   new = (AstPointSet *) astInitObject( mem, size, init,
                                        (AstObjectVtab *) vtab, name );

/* If necessary, initialise the virtual function table. */
/* ---------------------------------------------------- */
   if ( init ) InitVtab( vtab );
   if ( astOK ) {

/* Initialise the PointSet data. */
/* ----------------------------- */
/* Store the number of points and number of coordinate values per point. */
      new->npoint = npoint;
      new->ncoord = ncoord;

/* Initialise pointers to the pointer array and associated coordinate
   values array. */
      new->ptr = NULL;
      new->values = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstPointSet *astLoadPointSet_( void *mem, size_t size, int init,
                               AstPointSetVtab *vtab, const char *name,
                               AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadPointSet

*  Purpose:
*     Load a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astLoadPointSet( void *mem, size_t size, int init,
*                                   AstPointSetVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     PointSet loader.

*  Description:
*     This function is provided to load a new PointSet using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     PointSet structure in this memory, using data read from the
*     input Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a PointSet at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the PointSet is to be
*        loaded.  This must be of sufficient size to accommodate the
*        PointSet data (sizeof(PointSet)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the PointSet (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the PointSet structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPointSet) is used instead.
*     init
*        A boolean flag indicating if the PointSet's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*
*        If the "vtab" parameter is NULL, the "init" value is ignored
*        and the (static) virtual function table initialisation flag
*        for the PointSet class is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new PointSet. If this is NULL, a pointer
*        to the (static) virtual function table for the PointSet class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "PointSet" is used instead.

*  Returned Value:
* A pointer to the new PointSet.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstPointSet *new;             /* Pointer to the new PointSet */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   int coord;                    /* Loop counter for coordinates */
   int empty;                    /* PointSet empty? */
   int i;                        /* Counter for coordinate values */
   int point;                    /* Loop counter for points */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this PointSet. In this case the
   PointSet belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPointSet );
      init = !class_init;
      vtab = &class_vtab;
      name = "PointSet";
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built PointSet. */
   new = astLoadObject( mem, size, init, (AstObjectVtab *) vtab, name,
                        channel );

/* If required, initialise the part of the virtual function table used
   by this class. */
   if ( init && !class_init ) InitVtab( vtab );

/* Note if we have successfully initialised the (static) virtual
   function table owned by this class (so that this is done only
   once). */
   if ( astOK ) {
      if ( ( vtab == &class_vtab ) && init ) class_init = 1;

/* Initialise the PointSet's data pointers. */
      new->ptr = NULL;
      new->values = NULL;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "PointSet" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Npoint. */
/* ------- */
      new->npoint = astReadInt( channel, "npoint", 1 );
      if ( new->npoint < 1 ) new->npoint = 1;

/* Ncoord. */
/* ------- */
      new->ncoord = astReadInt( channel, "ncoord", 1 );
      if ( new->ncoord < 1 ) new->ncoord = 1;

/* Coordinate data. */
/* ---------------- */
/* Read a value for the "Empty" keyword to see whether the PointSet
   contains data. */
      empty = astReadInt( channel, "empty", 0 );

/* If it does, allocate memory to hold the coordinate data and
   pointers. */
      if ( astOK && !empty ) {
         new->ptr = astMalloc( sizeof( double * ) * (size_t) new->npoint );
         new->values = astMalloc( sizeof( double ) *
                                  (size_t) ( new->npoint * new->ncoord ) );
         if ( astOK ) {

/* Initialise the array of pointers into the main data array. */
            for ( coord = 0; coord < new->ncoord; coord++ ) {
               new->ptr[ coord ] = new->values + ( coord * new->npoint );
            }

/* Create a keyword for each coordinate value to be read. */
            i = 0;
            for ( point = 0; point < new->npoint; point++ ) {
               for ( coord = 0; coord < new->ncoord; coord++ ) {
                  i++;
                  (void) sprintf( key, "x%d", i );

/* Read and assign the values. */
                  new->ptr[ coord ][ point ] =
                     astReadDouble( channel, key, AST__BAD );
               }
            }
         }

/* If an error occurred, clean up by freeing the memory allocated
   above, thus emptying the PointSet. */
         if ( !astOK ) {
            new->ptr = astFree( new->ptr );
            new->values = astFree( new->values );
         }
      }

/* If an error occurred, clean up by deleting the new PointSet. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new PointSet pointer. */
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
int astGetNpoint_( const AstPointSet *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointSet,GetNpoint))( this );
}
int astGetNcoord_( const AstPointSet *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointSet,GetNcoord))( this );
}
double **astGetPoints_( AstPointSet *this ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,PointSet,GetPoints))( this );
}
void astSetPoints_( AstPointSet *this, double **ptr ) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointSet,SetPoints))( this, ptr );
}
void astSetSubPoints_( AstPointSet *point1, int point, int coord,
                       AstPointSet *point2) {
   if ( !astOK ) return;
   (**astMEMBER(point1,PointSet,SetSubPoints))( point1, point, coord, point2 );
}
