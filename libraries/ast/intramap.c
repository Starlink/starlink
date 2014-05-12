/*
*class++
*  Name:
*     IntraMap

*  Purpose:
c     Map points using a private transformation function.
f     Map points using a private transformation routine.

*  Constructor Function:
c     astIntraMap (also see astIntraReg)
f     AST_INTRAMAP (also see AST_INTRAREG)

*  Description:
c     The IntraMap class provides a specialised form of Mapping which
c     encapsulates a privately-defined coordinate transformation
c     other AST Mapping. This allows you to create Mappings that
c     perform any conceivable coordinate transformation.
f     The IntraMap class provides a specialised form of Mapping which
f     encapsulates a privately-defined coordinate transformation
f     routine (e.g. written in Fortran) so that it may be used like
f     any other AST Mapping. This allows you to create Mappings that
f     perform any conceivable coordinate transformation.
*
*     However, an IntraMap is intended for use within a single program
*     or a private suite of software, where all programs have access
*     to the same coordinate transformation functions (i.e. can be
*     linked against them). IntraMaps should not normally be stored in
*     datasets which may be exported for processing by other software,
*     since that software will not have the necessary transformation
*     functions available, resulting in an error.
*
c     You must register any coordinate transformation functions to be
c     used using astIntraReg before creating an IntraMap.
f     You must register any coordinate transformation functions to be
f     used using AST_INTRAREG before creating an IntraMap.

*  Inheritance:
*     The IntraMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     IntraMap also has the following attributes:
*
*     - IntraFlag: IntraMap identification string

*  Functions:
c     The IntraMap class does not define any new functions beyond those
f     The IntraMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

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
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     16-MAR-1998 (RFWS):
*        Original version.
*     15-SEP-1999 (RFWS):
*        Added a "this" pointer to the external transformation function
*        used by an IntraMap.
*     20-JUN-2001 (DSB):
*        Add an "astClone" call to prevent the pointer for "this" being
*        annulled at the end of the Transform method.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitIntraMapVtab
*        method.
*     7-DEC-2005 (DSB):
*        Free memory allocated by calls to astReadString.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS IntraMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "unitmap.h"             /* Unit (identity) Mappings */
#include "intramap.h"            /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Pointer to array of transformation function data. */
static AstIntraMapTranData *tran_data = NULL;

/* Number of transformation functions registered. */
static int tran_nfun = 0;

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are used or extended by this
   class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_getnin)( AstMapping *, int * );
static int (* parent_getnout)( AstMapping *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(IntraMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(IntraMap,Class_Init)
#define class_vtab astGLOBAL(IntraMap,Class_Vtab)


/* A mutex used to serialise invocations of the IntraReg function (the
   only function allowed to modify the contents of the static tran_data
   array). */
static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX1 pthread_mutex_lock( &mutex1 );
#define UNLOCK_MUTEX1 pthread_mutex_unlock( &mutex1 );

/* A mutex used to serialise invocations of extrnal transformation
   functions (which may not be thread-safe). */
static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstIntraMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#define LOCK_MUTEX1
#define UNLOCK_MUTEX1

#define LOCK_MUTEX2
#define UNLOCK_MUTEX2

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstIntraMap *astIntraMapId_( const char *, int, int, const char *, ... );
void astIntraRegFor_( const char *, int, int, void (* tran)( AstMapping *, int, int, const double *[], int, int, double *[] ), void (* tran_wrap)( void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), AstMapping *, int, int, const double *[], int, int, double *[], int * ), unsigned int, const char *, const char *, const char *, int * );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static char *CleanName( const char *, const char *, int * );
static int GetObjSize( AstObject *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetIntraFlag( AstIntraMap *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestIntraFlag( AstIntraMap *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearIntraFlag( AstIntraMap *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int Equal( AstObject *, AstObject *, int * );
static void IntraReg( const char *, int, int, void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), void (*)( void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), AstMapping *, int, int, const double *[], int, int, double *[], int * ), unsigned int, const char *, const char *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetIntraFlag( AstIntraMap *, const char *, int * );
static void TranWrap( void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), AstMapping *, int, int, const double *[], int, int, double *[], int * );

/* Member functions. */
/* ================= */
static char *CleanName( const char *name, const char *caller, int *status ) {
/*
*  Name:
*     CleanName

*  Purpose:
*     Clean (and validate) a transformation function name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     char *CleanName( const char *name, const char *caller, int *status )

*  Class Membership:
*     IntraMap member function.

*  Description:
*     This function cleans a transformation function name by removing
*     all white space. It returns a copy of the cleaned name held in
*     dynamically allocated memory. If the name is entirely blank, an
*     error is reported.

*  Parameters:
*     name
*        Pointer to a null-terminated string containing the name to be
*        cleaned.
*     caller
*        Pointer to a null-terminated string containing the name of
*        the calling function. This is only used to construct error
*        messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a dynamically-allocated null-terminated string
*     containing the cleaned name. A NULL pointer is returned if the
*     name was entirely blank.

*  Notes:
*     - The memory holding the returned string should be deallocated
*     (using astFree) when no longer required.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer to result string */
   int i;                        /* Loop counter for input characters */
   int len;                      /* Length of name */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine the number of non-blank characters in the name supplied. */
   len = 0;
   for ( i = 0; name[ i ]; i++ ) if ( !isspace( (int) name[ i ] ) ) len++;

/* If the name is entirely blank, then report an error. */
   if ( !len ) {
      astError( AST__ITFNI, "%s: Invalid blank transformation function name "
                            "given.", status, caller );

/* Otherwise, allocate memory to hold the cleaned name. */
   } else {
      result = astMalloc( (size_t) ( len + 1 ) );

/* If OK, make a copy of the name with white space removed. */
      if ( astOK ) {
         len = 0;
         for ( i = 0; name[ i ]; i++ ) {
            if ( !isspace( (int) name[ i ] ) ) result[ len++ ] = name[ i ];
         }

/* Terminate the result string. */
         result[ len ] = '\0';
      }
   }

/* Return the result pointer. */
   return result;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for an
*     IntraMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the IntraMap.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstIntraMap *this;            /* Pointer to the IntraMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* IntraFlag. */
/* ---------- */
   if ( !strcmp( attrib, "intraflag" ) ) {
      astClearIntraFlag( this );

/* Not recognised. */
/* --------------- */
/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two IntraMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two IntraMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a IntraMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the IntraMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstIntraMap *that;
   AstIntraMap *this;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two IntraMap structures. */
   this = (AstIntraMap *) this_object;
   that = (AstIntraMap *) that_object;

/* Check the second object is a IntraMap. We know the first is a
   IntraMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAIntraMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two IntraMaps differ, it may still be possible
   for them to be equivalent. First compare the IntraMaps if their Invert
   flags are the same. In this case all the attributes of the two IntraMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

            if( this->ifun == that->ifun &&
                this->intraflag && that->intraflag &&
                !strcmp( this->intraflag, that->intraflag ) ) {
               result = 1;
            }

/* If the Invert flags for the two IntraMaps differ, the attributes of the two
   IntraMaps must be inversely related to each other. */
         } else {

/* In the specific case of a IntraMap, Invert flags must be equal. */
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
*     #include "intramap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied IntraMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the IntraMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstIntraMap *this;         /* Pointer to IntraMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astTSizeOf( this->intraflag );

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
*     Get the value of a specified attribute for an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a IntraMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the IntraMap.
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
*     within the IntraMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the IntraMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstIntraMap *this;            /* Pointer to the IntraMap structure */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* IntraFlag. */
/* ---------- */
   if ( !strcmp( attrib, "intraflag" ) ) {
      result = astGetIntraFlag( this );

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

void astInitIntraMapVtab_(  AstIntraMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitIntraMapVtab

*  Purpose:
*     Initialise a virtual function table for an IntraMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "intramap.h"
*     void astInitIntraMapVtab( AstIntraMapVtab *vtab, const char *name )

*  Class Membership:
*     IntraMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the IntraMap class.

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

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAIntraMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearIntraFlag = ClearIntraFlag;
   vtab->GetIntraFlag = GetIntraFlag;
   vtab->SetIntraFlag = SetIntraFlag;
   vtab->TestIntraFlag = TestIntraFlag;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
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

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Store pointers to inherited methods that will be invoked explicitly
   by this class. */
   parent_getnin = mapping->GetNin;
   parent_getnout = mapping->GetNout;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "IntraMap",
               "Map points using a private transformation function" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void IntraReg( const char *name, int nin, int nout,
                      void (* tran)( AstMapping *, int, int, const double *[],
                                     int, int, double *[] ),
                      void (* tran_wrap)( void (*)( AstMapping *, int, int,
                                                    const double *[], int, int,
                                                    double *[] ),
                                          AstMapping *, int, int,
                                          const double *[], int, int,
                                          double *[], int * ),
                      unsigned int flags,
                      const char *purpose, const char *author,
                      const char *contact, int *status ) {
/*
*  Name:
*     IntraReg

*  Purpose:
*     Register a transformation function for use by an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     void IntraReg( const char *name, int nin, int nout,
*                    void (* tran)( AstMapping *, int, int, const double *[],
*                                   int, int, double *[] ),
*                    void (* tran_wrap)( void (*)( AstMapping *, int, int,
*                                                  const double *[], int, int,
*                                                  double *[] ),
*                                        AstMapping *, int, int,
*                                        const double *[], int, int,
*                                        double *[], int * ),
*                    unsigned int flags,
*                    const char *purpose, const char *author,
*                    const char *contact, int *status )

*  Class Membership:
*     IntraMap member function.

*  Description:
*     This function registers a transformation function which will
*     later be used by an IntraMap and associates it with a name. It
*     also stores related information which will be required by the
*     IntraMap.

*  Parameters:
*     name
*        Pointer to a null-terminated string containing the name to be
*        used to identify the transformation function. This string is
*        case sensitive. All white space is removed before use.
*     nin
*        The number of input coordinates per point (or AST__ANY if any
*        number are allowed).
*     nout
*        The number of output coordinates per point (or AST__ANY if
*        any number are allowed).
*     tran
*        Pointer to the transformation function to be registered.
*        This may have any form of interface, which need not be known
*        to the implementation of the IntraMap class. Instead, the
*        method of invoking the transformation function should be
*        encapsulated in the "tran_wrap" function (below).
*     tran_wrap
*        Pointer to a wrapper function appropriate to the transformation
*        function (above). This wrapper function should have the same
*        interface as astTranP (from the Mapping class), except that it takes
*        a pointer to a function like "tran" as an additional first argument.
*        The purpose of this wrapper is to invoke the transformation function
*        via the pointer supplied, to pass it the necessary information
*        derived from the remainder of its arguments, and then to return the
*        results.
*     flags
*        This argument may be used to supply a set of flags which
*        control the behaviour of any IntraMap which uses the
*        registered transformation function. See the public interface
*        for astIntraReg for details.
*     purpose
*        Pointer to a null-terminated string containing a short (one
*        line) textual comment to describe the purpose of the
*        transformation function.
*     author
*        Pointer to a null-terminated string containing the name of
*        the author of the transformation function.
*     contact
*        Pointer to a null-terminated string containing contact
*        details for the author of the function (e.g. an e-mail or WWW
*        address).
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   char *clname;                 /* Pointer to cleaned name string */
   int found;                    /* Transformation function found? */
   int ifun;                     /* Loop counter for function information */

/* Check the global error status. */
   if ( !astOK ) return;

/* This function modifies the global static tran_data array, so we use a
   mutex to ensure that only one thread can run this function at any one
   time. */
   LOCK_MUTEX1;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Indicate that subsequent memory allocations may never be freed (other
   than by any AST exit handler). */
   astBeginPM;

/* Clean (and validate) the name supplied. */
   clname = CleanName( name, "astIntraReg", status );

/* If OK, also validate the numbers of input and output
   coordinates. Report an error if necessary. */
   if ( astOK ) {
      if ( ( nin < 0 ) && ( nin != AST__ANY ) ) {
         astError( AST__BADNI, "astIntraReg(%s): Bad number of input "
                               "coordinates (%d).", status, clname, nin );
         astError( AST__BADNI, "This number should be zero or more (or "
                               "AST__ANY)." , status);

      } else if ( ( nout < 0 ) && ( nout != AST__ANY ) ) {
         astError( AST__BADNO, "astIntraReg(%s): Bad number of output "
                               "coordinates (%d).", status, clname, nout );
         astError( AST__BADNO, "This number should be zero or more (or "
                               "AST__ANY)." , status);
      }
   }

/* Search the array of transformation function data to see if a
   function with the same name has already been registered. */
   if ( astOK ) {
      found = 0;
      for ( ifun = 0; ifun < tran_nfun; ifun++ ) {
         if ( ( found = !strcmp( clname, tran_data[ ifun ].name ) ) ) break;
      }

/* If so, then check that the information supplied this time is
   identical to that supplied before. If any discrepancy is found,
   report an error. */
      if ( found ) {
         if ( ( nin != tran_data[ ifun ].nin ) ||
              ( nout != tran_data[ ifun ].nout ) ||
              ( tran != tran_data[ ifun ].tran ) ||
              ( tran_wrap != tran_data[ ifun ].tran_wrap ) ||
              ( flags != tran_data[ ifun ].flags ) ||
              strcmp( purpose, tran_data[ ifun ].purpose ) ||
              strcmp( author, tran_data[ ifun ].author ) ||
              strcmp( contact, tran_data[ ifun ].contact ) ) {
            astError( AST__MRITF, "astIntraReg: Invalid attempt to register "
                                  "the transformation function name \"%s\" "
                                  "multiple times.", status, clname );
         }

/* If this is a new function, extend the array of transformation
   function data to accommodate it. */
      } else {

         tran_data = astGrow( tran_data, tran_nfun + 1, sizeof( AstIntraMapTranData ) );

/* Store the information supplied. */
         if ( astOK ) {
            tran_data[ tran_nfun ].name = clname;
            tran_data[ tran_nfun ].nin = nin;
            tran_data[ tran_nfun ].nout = nout;
            tran_data[ tran_nfun ].tran = tran;
            tran_data[ tran_nfun ].tran_wrap = tran_wrap;
            tran_data[ tran_nfun ].flags = flags;
            tran_data[ tran_nfun ].purpose =
               astStore( NULL, purpose, strlen( purpose ) + (size_t) 1 );
            tran_data[ tran_nfun ].author =
               astStore( NULL, author, strlen( author ) + (size_t) 1 );
            tran_data[ tran_nfun ].contact =
               astStore( NULL, contact, strlen( contact ) + (size_t) 1 );

/* If successful, increment the count of transformation functions
   registered. */
            if ( astOK ) {
               tran_nfun++;

/* If an error occurred, free any memory that was allocated. */
            } else {
               tran_data[ tran_nfun ].name = NULL;
               tran_data[ tran_nfun ].purpose =
                  astFree( tran_data[ tran_nfun ].purpose );
               tran_data[ tran_nfun ].author =
                  astFree( tran_data[ tran_nfun ].author );
               tran_data[ tran_nfun ].contact =
                  astFree( tran_data[ tran_nfun ].contact );
            }
         }
      }
   }

/* If an error occurred, free the memory holding the cleaned function
   name. */
   if ( !astOK ) clname = astFree( clname );

/* Mark the end of the section in which memory allocations may never be
   freed (other than by any AST exit handler). */
   astEndPM;

/* Unlock the mutex that ensures that only one thread can run this function
   at any one time. */
   UNLOCK_MUTEX1;

}

void astIntraReg_( const char *name, int nin, int nout,
                   void (* tran)( AstMapping *, int, int, const double *[],
                                  int, int, double *[] ),
                   unsigned int flags, const char *purpose, const char *author,
                   const char *contact, int *status ) {
/*
*++
*  Name:
c     astIntraReg
f     AST_INTRAREG

*  Purpose:
c     Register a transformation function for use by an IntraMap.
f     Register a transformation routine for use by an IntraMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "intramap.h"
c     astIntraReg( const char *name, int nin, int nout,
c                  void (* tran)( AstMapping *, int, int, const double *[],
c                                 int, int, double *[] ),
c                  unsigned int flags, const char *purpose, const char *author,
c                  const char *contact )
f     CALL AST_INTRAREG( NAME, NIN, NOUT, TRAN, FLAGS, PURPOSE, AUTHOR,
f                        CONTACT, STATUS )

*  Class Membership:
*     IntraMap member function.

*  Description:
c     This function registers a privately-defined coordinate
c     transformation function written in C so that it may be used to
c     create an IntraMap. An IntraMap is a specialised form of Mapping
c     which encapsulates the C function so that it may be used like
c     any other AST Mapping. This allows you to create Mappings that
c     perform any conceivable coordinate transformation.
f     This function registers a privately-defined coordinate
f     transformation routine written in Fortran so that it may be used
f     to create an IntraMap. An IntraMap is a specialised form of
f     Mapping which encapsulates the Fortran routine so that it may be
f     used like any other AST Mapping. This allows you to create
f     Mappings that perform any conceivable coordinate transformation.
*
c     Registration of relevant transformation functions is required
c     before using the astIntraMap constructor function to create an
c     IntraMap or reading an external representation of an IntraMap
c     from a Channel.
f     Registration of relevant transformation routines is required
f     before using the AST_INTRAMAP constructor function to create an
f     IntraMap or reading an external representation of an IntraMap
f     from a Channel.

*  Parameters:
c     name
f     NAME = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing a unique name
c        to be associated with the transformation function in order to
c        identify it. This name is case sensitive. All white space
c        will be removed before use.
f        A character string containing a unique name to be associated
f        with the transformation routine in order to identify it. This
f        name is case sensitive. All white space will be removed
f        before use.
c     nin
f     NIN = INTEGER (Given)
c        The number of input coordinates accepted by the
c        transformation function (i.e. the number of dimensions of the
c        space in which the input points reside). A value of AST__ANY
c        may be given if the function is able to accommodate a
c        variable number of input coordinates.
f        The number of input coordinates accepted by the
f        transformation routine (i.e. the number of dimensions of the
f        space in which the input points reside). A value of AST__ANY
f        may be given if the routine is able to accommodate a variable
f        number of input coordinates.
c     nout
f     NOUT = INTEGER (Given)
c        The number of output coordinates produced by the
c        transformation function (i.e. the number of dimensions of the
c        space in which the output points reside). A value of AST__ANY
c        may be given if the function is able to produce a variable
c        number of output coordinates.
f        The number of output coordinates produced by the
f        transformation routine (i.e. the number of dimensions of the
f        space in which the output points reside). A value of AST__ANY
f        may be given if the routine is able to produce a variable
f        number of output coordinates.
c     tran
f     TRAN = SUBROUTINE (Given)
c        Pointer to the transformation function to be registered.
c        This function should perform whatever coordinate
c        transformations are required and should have an interface
c        like astTranP (q.v.).
f        The transformation routine to be registered.  This routine
f        should perform whatever coordinate transformations are
f        required and should have an interface like AST_TRANN (q.v.).
f
f        This transformation routine must also appear in an EXTERNAL
f        statement in the routine which calls AST_INTRAREG.
c     flags
f     FLAGS = INTEGER (Given)
c        This value may be used to supply a set of flags which
c        describe the transformation function and which may affect the
c        behaviour of any IntraMap which uses it.  Often, a value of
c        zero will be given here, but you may also supply the bitwise
c        OR of a set of flags as described in the "Transformation
c        Flags" section (below).
f        This value may be used to supply a set of flags which
f        describe the transformation routine and which may affect the
f        behaviour of any IntraMap which uses it.  Often, a value of
f        zero will be given here, but you may also supply the sum of a
f        set of flags as described in the "Transformation Flags"
f        section (below).
c     purpose
f     PURPOSE = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing a short (one
c        line) textual comment to describe the purpose of the
c        transformation function.
f        A character string containing a short (one line) textual
f        comment to describe the purpose of the transformation
f        routine.
c     author
f     AUTHOR = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing the name of
c        the author of the transformation function.
f        A character string containing the name of the author of the
f        transformation routine.
c     contact
f     CONTACT = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing contact
c        details for the author of the transformation function
c        (e.g. an e-mail or WWW address). If any IntraMap which uses
c        this transformation function is exported as part of a dataset
c        to an external user who does not have access to the function,
c        then these contact details should allow them to obtain the
c        necessary code.
f        A character string containing contact details for the author
f        of the transformation routine (e.g. an e-mail or WWW
f        address). If any IntraMap which uses this transformation
f        routine is exported as part of a dataset to an external user
f        who does not have access to the routine, then these contact
f        details should allow them to obtain the necessary code.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
c     - Beware that an external representation of an IntraMap (created
c     by writing it to a Channel) will not include the coordinate
c     transformation function which it uses, so will only refer to the
c     function by its name (as assigned using astIntraReg).
c     Consequently, the external representation cannot be utilised by
c     another program unless that program has also registered the same
c     transformation function with the same name using an identical
c     invocation of astIntraReg. If no such registration has been
c     performed, then attempting to read the external representation
c     will result in an error.
f     - Beware that an external representation of an IntraMap (created
f     by writing it to a Channel) will not include the coordinate
f     transformation routine which it uses, so will only refer to the
f     routine by its name (as assigned using AST_INTRAREG).
f     Consequently, the external representation cannot be utilised by
f     another program unless that program has also registered the same
f     transformation routine with the same name using an identical
f     invocation of AST_INTRAREG. If no such registration has been
f     performed, then attempting to read the external representation
f     will result in an error.
c     - You may use astIntraReg to register a transformation function
c     with the same name more than once, but only if the arguments
c     supplied are identical on each occasion (i.e there is no way of
c     changing things once a function has been successfully registered
c     under a given name, and attempting to do so will result in an
c     error). This feature simply allows registration to be performed
c     independently, but consistently, at several places within your
c     program, without having to check whether it has already been
c     done.
f     - You may use AST_INTRAREG to register a transformation routine
f     with the same name more than once, but only if the arguments
f     supplied are identical on each occasion (i.e there is no way of
f     changing things once a routine has been successfully registered
f     under a given name, and attempting to do so will result in an
f     error). This feature simply allows registration to be performed
f     independently, but consistently, at several places within your
f     program, without having to check whether it has already been
f     done.
c     - If an error occurs in the transformation function, this may be
c     indicated by setting the AST error status to an error value
c     (using astSetStatus) before it returns.  This will immediately
c     terminate the current AST operation.  The error value AST__ITFER
c     is available for this purpose, but other values may also be used
c     (e.g. if you wish to distinguish different types of error).
f     - If an error occurs in the transformation routine, this may be
f     indicated by setting its STATUS argument to an error value
f     before it returns.  This will immediately terminate the current
f     AST operation.  The error value AST__ITFER is available for this
f     purpose, but other values may also be used (e.g. if you wish to
f     distinguish different types of error). The AST__ITFER error
f     value is defined in the AST_ERR include file.

*  Transformation Flags:
c     The following flags are defined in the ``ast.h'' header file and
c     allow you to provide further information about the nature of the
c     transformation function. Having selected the set of flags which
c     apply, you should supply the bitwise OR of their values as the
c     ``flags'' argument to astIntraReg.
f     The following flags are defined in the AST_PAR include file and
f     allow you to provide further information about the nature of the
f     transformation routine. Having selected the set of flags which
f     apply, you should supply the sum of their values as the FLAGS
f     argument to AST_INTRAREG.

c     - AST__NOFWD: If this flag is set, it indicates that the
c     transformation function does not implement a forward coordinate
c     transformation. In this case, any IntraMap which uses it will
c     have a TranForward attribute value of zero and the
c     transformation function itself will not be invoked with its
c     ``forward'' argument set to a non-zero value.  By default, it is
c     assumed that a forward transformation is provided.
f     - AST__NOFWD: If this flag is set, it indicates that the
f     transformation routine does not implement a forward coordinate
f     transformation. In this case, any IntraMap which uses it will
f     have a TranForward attribute value of zero and the
f     transformation routine itself will not be called with its
f     FORWARD argument set to .TRUE.. By default, it is assumed that a
f     forward transformation is provided.
c     - AST__NOINV: If this flag is set, it indicates that the
c     transformation function does not implement an inverse coordinate
c     transformation. In this case, any IntraMap which uses it will
c     have a TranInverse attribute value of zero and the
c     transformation function itself will not be invoked with its
c     ``forward'' argument set to zero.  By default, it is assumed
c     that an inverse transformation is provided.
f     - AST__NOINV: If this flag is set, it indicates that the
f     transformation routine does not implement an inverse coordinate
f     transformation. In this case, any IntraMap which uses it will
f     have a TranInverse attribute value of zero and the
f     transformation routine itself will not be called with its
f     FORWARD argument set to .FALSE.. By default, it is assumed that
f     an inverse transformation is provided.
c     - AST__SIMPFI: You may set this flag if applying the
c     transformation function's forward coordinate transformation,
c     followed immediately by the matching inverse transformation,
c     should always restore the original set of coordinates. It
c     indicates that AST may replace such a sequence of operations by
c     an identity Mapping (a UnitMap) if it is encountered while
c     simplifying a compound Mapping (e.g. using astSimplify).  It is
c     not necessary that both transformations have actually been
c     implemented.
f     - AST__SIMPFI: You may set this flag if applying the
f     transformation routine's forward coordinate transformation,
f     followed immediately by the matching inverse transformation,
f     should always restore the original set of coordinates. It
f     indicates that AST may replace such a sequence of operations by
f     an identity Mapping (a UnitMap) if it is encountered while
f     simplifying a compound Mapping (e.g. using AST_SIMPLIFY).  It is
f     not necessary that both transformations have actually been
f     implemented.
c     - AST__SIMPIF: You may set this flag if applying the
c     transformation function's inverse coordinate transformation,
c     followed immediately by the matching forward transformation,
c     should always restore the original set of coordinates. It
c     indicates that AST may replace such a sequence of operations by
c     an identity Mapping (a UnitMap) if it is encountered while
c     simplifying a compound Mapping (e.g. using astSimplify).  It is
c     not necessary that both transformations have actually been
c     implemented.
f     - AST__SIMPIF: You may set this flag if applying the
f     transformation routine's inverse coordinate transformation,
f     followed immediately by the matching forward transformation,
f     should always restore the original set of coordinates. It
f     indicates that AST may replace such a sequence of operations by
f     an identity Mapping (a UnitMap) if it is encountered while
f     simplifying a compound Mapping (e.g. using AST_SIMPLIFY).  It is
f     not necessary that both transformations have actually been
f     implemented.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Register the transformation function together with the appropriate
   wrapper function for the C language. */
   IntraReg( name, nin, nout, tran, TranWrap, flags, purpose, author,
             contact, status );
}

void astIntraRegFor_( const char *name, int nin, int nout,
                      void (* tran)( AstMapping *, int, int, const double *[],
                                     int, int, double *[] ),
                      void (* tran_wrap)( void (*)( AstMapping *, int, int,
                                                    const double *[], int, int,
                                                    double *[] ),
                                          AstMapping *, int, int,
                                          const double *[], int, int,
                                          double *[], int * ),
                      unsigned int flags, const char *purpose,
                      const char *author, const char *contact, int *status ) {
/*
*+
*  Name:
*     astIntraRegFor

*  Purpose:
*     Register a foreign language transformation function for an IntraMap.

*  Type:
*     Public function.

*  Synopsis:
*     #include "intramap.h"
*     void astIntraRegFor( const char *name, int nin, int nout,
*                          void (* tran)( AstMapping *, int, int,
*                                         const double *[], int, int,
*                                         double *[] ),
*                          void (* tran_wrap)( void (*)( AstMapping *, int,
*                                                        int, const double *[],
*                                                        int, int,
*                                                        double *[] ),
*                                              AstMapping *, int, int,
*                                              const double *[], int, int,
*                                              double *[], int * ),
*                          unsigned int flags, const char *purpose,
*                          const char *author, const char *contact )

*  Class Membership:
*     IntraMap member function.

*  Description:
*     This function registers a transformation function provided by a
*     foreign language interface which will later be used by an
*     IntraMap, and associates it with a name. It also stores related
*     information which will be required by the IntraMap.

*  Parameters:
*     name
*        Pointer to a null-terminated string containing the name to be
*        used to identify the transformation function. This string is
*        case sensitive. All white space is removed before use.
*     nin
*        The number of input coordinates per point (or AST__ANY if any
*        number are allowed).
*     nout
*        The number of output coordinates per point (or AST__ANY if
*        any number are allowed).
*     tran
*        Pointer to the foreign language transformation function to be
*        registered. This may have any form of interface, which need
*        not be known to the implementation of the IntraMap
*        class. Instead, the method of invoking the transformation
*        function should be encapsulated in the "tran_wrap" function
*        (below).
*     tran_wrap
*        Pointer to a wrapper function appropriate to the foreign
*        language interface. This wrapper function should have the
*        same interface as astTranP (from the Mapping class), except
*        that it takes a pointer to a function like "tran" as an additional
*        first argument. The purpose of this wrapper is to invoke the
*        transformation function via the pointer supplied, to pass it the
*        necessary information derived from the remainder of its arguments,
*        and then to return the results.
*     flags
*        This argument may be used to supply a set of flags which
*        control the behaviour of any IntraMap which uses the
*        registered transformation function. See the description of
*        astIntraReg for details.
*     purpose
*        Pointer to a null-terminated string containing a short (one
*        line) textual comment to describe the purpose of the
*        transformation function.
*     author
*        Pointer to a null-terminated string containing the name of
*        the author of the transformation function.
*     contact
*        Pointer to a null-terminated string containing contact
*        details for the author of the transformation function
*        (e.g. an e-mail address or URL). If any IntraMap using this
*        transformation function is exported as part of a dataset to
*        an external user who does not have access to the function,
*        then these contact details should allow them to obtain the
*        necessary code.

*  Notes:
*     - This function is only available through the public interface
*     to the IntraMap class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Register the transformation function together with the appropriate
   wrapper function for the foreign language interface. */
   IntraReg( name, nin, nout, tran, tran_wrap, flags, purpose, author,
             contact, status );
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     IntraMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated IntraMap in the sequence with its
*     neighbours, so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated IntraMap with one which it
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
*        Pointer to the nominated IntraMap which is to be merged with
*        its neighbours. This should be a cloned copy of the IntraMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        IntraMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated IntraMap resides.
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
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstIntraMap *intramap1;       /* Pointer to first IntraMap */
   AstIntraMap *intramap2;       /* Pointer to second IntraMap */
   AstMapping *new;              /* Pointer to replacement Mapping */
   const char *class;            /* Pointer to Mapping class string */
   int imap1;                    /* Index of first IntraMap */
   int imap2;                    /* Index of second IntraMap */
   int imap;                     /* Loop counter for Mappings */
   int invert1;                  /* Invert flag value (1st IntraMap) */
   int invert2;                  /* Invert flag value (2nd IntraMap) */
   int nin1;                     /* No. input coordinates (1st IntraMap) */
   int nout2;                    /* No. output coordinates (2nd IntraMap) */
   int result;                   /* Result value to return */
   int simpler;                  /* Mappings simplified? */

/* Initialise the returned result. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this);

/* Further initialisation. */
   new = NULL;
   simpler = 0;
   nin1 = -1;

/* We will only handle the case of IntraMaps in series and will
   consider merging the nominated IntraMap with the Mapping which
   follows it. Check that there is such a Mapping. */
   if ( series && ( ( where + 1 ) < *nmap ) ) {

/* Obtain the indices of the two potential IntraMaps to be merged and
   a pointer to the first one. */
      imap1 = where;
      imap2 = where + 1;
      intramap1 = (AstIntraMap *) ( *map_list )[ imap1 ];

/* Obtain the Class string of the second Mapping and determine if it
   is an IntraMap. */
      class = astGetClass( ( *map_list )[ imap2 ] );
      if ( astOK && !strcmp( class, "IntraMap" ) ) {

/* Obtain a pointer to the second IntraMap. */
         intramap2 = (AstIntraMap *) ( *map_list )[ imap2 ];

/* Check that the two IntraMaps use the same transformation function
   and have the same IntraFlag string (if set). */
         if ( ( intramap1->ifun == intramap2->ifun ) &&
              !strcmp( intramap1->intraflag ? intramap1->intraflag : "",
                       intramap2->intraflag ? intramap2->intraflag : "" ) ) {

/* Determine the number of input coordinates that the first IntraMap
   would have if its Invert attribute were set to the value of the
   associated invert flag. Take account of the current Invert
   attribute in obtaining this value. */
            invert1 = ( *invert_list )[ imap1 ];
            if ( astGetInvert( intramap1 ) ) {
               nin1 = invert1 ? astGetNin( intramap1 ) :
                                astGetNout( intramap1 );
            } else {
               nin1 = invert1 ? astGetNout( intramap1 ) :
                                astGetNin( intramap1 );
            }

/* Similarly, determine the number of output coordinates that the
   second IntraMap would have. */
            invert2 = ( *invert_list )[ imap2 ];
            if ( astGetInvert( intramap2 ) ) {
               nout2 = invert2 ? astGetNout( intramap2 ) :
                                 astGetNin( intramap2 );
            } else {
               nout2 = invert2 ? astGetNin( intramap2 ) :
                                 astGetNout( intramap2 );
            }

/* Check that the effect of applying the two IntraMaps will be to
   preserve the number of coordinates. */
            if ( astOK && ( nin1 == nout2 ) ) {

/* If so, check if the first transformation function is applied in the
   forward direction and the second in the inverse direction. If so,
   note if this configuration can be simplified. */
               if ( !invert1 && invert2 ) {
                  simpler = tran_data[ intramap1->ifun ].flags & AST__SIMPFI;

/* Similarly, if the first transformation function is applied in the
   inverse direction and the second in the forward direction, then
   note if this configuration can be simplified. */
               } else if ( invert1 && !invert2 ) {
                  simpler = tran_data[ intramap1->ifun ].flags & AST__SIMPIF;
               }
            }
         }
      }

/* If the two IntraMaps can be simplified, create a UnitMap to replace
   them. */
      if ( simpler ) {
         new = (AstMapping *) astUnitMap( nin1, "", status );

/* Annul the pointers to the IntraMaps. */
         if ( astOK ) {
            ( *map_list )[ imap1 ] = astAnnul( ( *map_list )[ imap1 ] );
            ( *map_list )[ imap2 ] = astAnnul( ( *map_list )[ imap2 ] );

/* Insert the pointer to the replacement Mapping and initialise its
   invert flag. */
            ( *map_list )[ imap1 ] = new;
            ( *invert_list )[ imap1 ] = 0;

/* Loop to close the resulting gap by moving subsequent elements down
   in the arrays. */
            for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
               ( *map_list )[ imap - 1 ] = ( *map_list )[ imap ];
               ( *invert_list )[ imap - 1 ] = ( *invert_list )[ imap ];
            }

/* Clear the vacated elements at the end. */
            ( *map_list )[ *nmap - 1 ] = NULL;
            ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
            ( *nmap )--;
            result = imap1;
         }
      }
   }

/* If an error occurred, clear the returned result. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astSetAttrib method inherited
*     from the Mapping class).

*  Description:
*     This function assigns an attribute value for a IntraMap, the
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
*        Pointer to the IntraMap.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Vaiables: */
   AstIntraMap *this;            /* Pointer to the IntraMap structure */
   int intraflag;                /* Offset of IntraFlag value in string */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* IntraFlag. */
/* ---------- */
   if ( nc = 0,
        ( 0 == astSscanf( setting, "intraflag=%n%*[^\n]%n", &intraflag, &nc ) )
        && ( nc >= len ) ) {
      astSetIntraFlag( this, setting + intraflag );

/* Not recognised. */
/* --------------- */
/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a IntraMap's attributes.

*  Parameters:
*     this
*        Pointer to the IntraMap.
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
   AstIntraMap *this;            /* Pointer to the IntraMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* IntraFlag. */
/* ---------- */
   if ( !strcmp( attrib, "intraflag" ) ) {
      result = astTestIntraFlag( this );

/* Not recognised. */
/* --------------- */
/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
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
*     Apply an IntraMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     IntraMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a IntraMap and a set of points encapsulated
*     in a PointSet and transforms the points using the transformation
*     function associated with the IntraMap.

*  Parameters:
*     this
*        Pointer to the IntraMap.
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
*     PointSet must match the number of coordinates for the IntraMap
*     being applied.
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points and coordinate values per point to
*     accommodate the result. Any excess space will be ignored.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstIntraMap *this;            /* Pointer to IntraMap structure */
   AstMapping *id;               /* Public ID for the IntraMap supplied */
   AstPointSet *result;          /* Pointer to output PointSet */
   const double **ptr_in;        /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   int ncoord_in;                /* Number of coordinates per input point */
   int ncoord_out;               /* Number of coordinates per output point */
   int npoint;                   /* Number of points */
   int ok;                       /* AST status OK? */
   int status_value;             /* AST status value */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_mapping);

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform
   member function inherited from the parent Mapping class. This
   function validates all arguments and generates an output PointSet
   if necessary, but does not actually transform any coordinate
   values. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the
   input and output PointSets and obtain pointers for accessing the
   input and output coordinate values. */
   npoint = astGetNpoint( in );
   ncoord_in = astGetNcoord( in );
   ncoord_out = astGetNcoord( result );
   ptr_in = (const double **) astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse transformation,
   according to the direction specified and whether the Mapping has
   been inverted. */
   if ( astGetInvert( this ) ) forward = !forward;

/* Obtain a public (external) ID for the IntraMap. This will be
   required (instead of a true C pointer) by the transformation function,
   since it is user-written. Clone the IntraMap pointer so that the call
   to astAnnulID later on does not annul the IntraMap pointer. */
   id = (AstMapping *) astMakeId( astClone( this ) );

/* Locate the transformation function data associated with the
   IntraMap and use the wrapper function to invoke the transformation
   function itself. */
   if ( ( ok = astOK ) ) {
      LOCK_MUTEX2;
      ( *tran_data[ this->ifun ].tran_wrap )( tran_data[ this->ifun ].tran,
                                              id, npoint, ncoord_in, ptr_in,
                                              forward, ncoord_out, ptr_out,
                                              status );
      UNLOCK_MUTEX2;

/* If an error occurred, report a contextual error message. To ensure
   that the location of the error appears in the message, we first clear
   the global status (which makes the error system think this is the
   first report). */
      if ( !( ok = astOK ) ) {
         status_value = astStatus;
         astClearStatus;
         astError( status_value,
                   "astTransform(%s): Error signalled by \"%s\" "
                   "transformation function.", status,
                   astGetClass( this ), tran_data[ this->ifun ].name );
      }
   }

/* Annul the external identifier. */
   id = astMakeId( astAnnulId( id ) );

/* If an error occurred here, but earlier steps were successful, then
   something has happened to the external ID, so report a contextual
   error message. */
   if ( !astOK && ok ) {
      astError( astStatus,
                "astTransform(%s): %s pointer corrupted by \"%s\" "
                "transformation function.", status,
                astGetClass( this ), astGetClass( this ),
                tran_data[ this->ifun ].name );
   }

/* If an error occurred, clear the returned pointer. If a new output
   PointSet has been created, then delete it. */
   if ( !astOK ) {
      result = ( result == out ) ? NULL : astDelete( result );
   }

/* Return a pointer to the output PointSet. */
   return result;
}

static void TranWrap( void (* tran)( AstMapping *, int, int, const double *[],
                                     int, int, double *[] ),
                      AstMapping *this, int npoint, int ncoord_in,
                      const double *ptr_in[], int forward, int ncoord_out,
                      double *ptr_out[], int *status ) {
/*
*  Name:
*     TranWrap

*  Purpose:
*     Wrapper function to invoke a C transformation function.

*  Type:
*     Private function.

*  Synopsis:
*     void TranWrap( void (* tran)( AstMapping *, int, int, const double *[],
*                                   int, int, double *[] ),
*                    AstMapping *this, int npoint, int ncoord_in,
*                    const double *ptr_in[], int forward, int ncoord_out,
*                    double *ptr_out[], int *status )

*  Class Membership:
*     IntraMap member function.

*  Description:
*     This function invokes a C implementation of a transformation
*     function (which resembles the astTranP function from the Mapping
*     class).
*
*     This wrapper is essentially a dummy function for the C language.
*     It may be replaced by alternative versions for foreign language
*     interfaces, thus allowing transformation functions supplied by
*     those languages to be invoked without knowledge of their
*     interfaces.

*  Parameters:
*     tran
*        Pointer to the transformation function to be invoked. This
*        should resemble astTranP (but with the first argument
*        omitted).
*     this
*        An external Mapping ID associated with the internal (true C) pointer
*        for the IntraMap whose transformation is being evaluated.
*     npoint
*        The number of points to be transformed.
*     ncoord_in
*        The number of coordinates being supplied for each input point
*        (i.e. the number of dimensions of the space in which the
*        input points reside).
*     ptr_in
*        An array of pointers to double, with "ncoord_in"
*        elements. Element "ptr_in[coord]" should point at the first
*        element of an array of double (with "npoint" elements) which
*        contain the values of coordinate number "coord" for each
*        input (untransformed) point. The value of coordinate number
*        "coord" for input point number "point" is therefore given by
*        "ptr_in[coord][point]".
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation is to be applied, while a zero value indicates
*        that the inverse transformation should be used.
*     ncoord_out
*        The number of coordinates being generated for each output
*        point (i.e. the number of dimensions of the space in which
*        the output points reside). This need not be the same as
*        "ncoord_in".
*     ptr_out
*        An array of pointers to double, with "ncoord_out"
*        elements. Element "ptr_out[coord]" should point at the first
*        element of an array of double (with "npoint" elements) into
*        which the values of coordinate number "coord" for each output
*        (transformed) point will be written.  The value of coordinate
*        number "coord" for output point number "point" will therefore
*        be found in "ptr_out[coord][point]".
*     status
*        Pointer to the inherited status value.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the transformation function. */
   ( *tran )( this, npoint, ncoord_in, ptr_in, forward, ncoord_out, ptr_out );
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
*     IntraFlag

*  Purpose:
*     IntraMap identification string.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
c     This attribute allows an IntraMap to be flagged so that it is
c     distinguishable from other IntraMaps. The transformation function
c     associated with the IntraMap may then enquire the value of this
c     attribute and adapt the transformation it provides according to the
c     particular IntraMap involved.
f     This attribute allows an IntraMap to be flagged so that it is
f     distinguishable from other IntraMaps. The transformation routine
f     associated with the IntraMap may then enquire the value of this
f     attribute and adapt the transformation it provides according to the
f     particular IntraMap involved.
*
c     Although this is a string attribute, it may often be useful to store
c     numerical values here, encoded as a character string, and to use these
c     as data within the transformation function. Note, however, that this
c     mechanism is not suitable for transferring large amounts of data (more
c     than about 1000 characters) to an IntraMap. For that purpose, global
c     variables are recommended, although the IntraFlag value can be used to
c     supplement this approach. The default IntraFlag value is an empty
c     string.
f     Although this is a string attribute, it may often be useful to store
f     numerical values here, encoded as a character string, and to use these
f     as data within the transformation routine. Note, however, that this
f     mechanism is not suitable for transferring large amounts of data (more
f     than about 1000 characters) to an IntraMap. For that purpose, global
f     variables are recommended, although the IntraFlag value can be used to
f     supplement this approach. The default IntraFlag value is an empty
f     string.

*  Applicability:
*     IntraMap
*        All IntraMaps have this attribute.

*  Notes:
c     - A pair of IntraMaps whose transformations may potentially cancel
c     cannot be simplified to produce a UnitMap (e.g. using astSimplify)
c     unless they have the same IntraFlag values. The test for equality is
c     case-sensitive.
f     - A pair of IntraMaps whose transformations may potentially cancel
f     cannot be simplified to produce a UnitMap (e.g. using AST_SIMPLIFY)
f     unless they have the same IntraFlag values. The test for equality is
f     case-sensitive.
*att--
*/

/* Clear the IntraFlag value by freeing the allocated memory and
   assigning a NULL pointer. */
astMAKE_CLEAR(IntraMap,IntraFlag,intraflag,astFree( this->intraflag ))

/* Return a pointer to the IntraFlag value. */
astMAKE_GET(IntraMap,IntraFlag,const char *,NULL,this->intraflag)

/* Set a IntraFlag value by freeing any previously allocated memory,
   allocating new memory, storing the string and saving the pointer to
   the copy. */
astMAKE_SET(IntraMap,IntraFlag,const char *,intraflag,astStore(
                                                      this->intraflag, value,
                                                      strlen( value ) +
                                                      (size_t) 1 ))

/* The IntraFlag value is set if the pointer to it is not NULL. */
astMAKE_TEST(IntraMap,IntraFlag,( this->intraflag != NULL ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for IntraMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for IntraMap objects.

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
   AstIntraMap *in;              /* Pointer to input IntraMap */
   AstIntraMap *out;             /* Pointer to output IntraMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output IntraMaps. */
   in = (AstIntraMap *) objin;
   out = (AstIntraMap *) objout;

/* For safety, first clear any references to the input memory from
   the output IntraMap. */
   out->intraflag = NULL;

/* If necessary, allocate memory in the output IntraMap and store a
   copy of the input IntraFlag string. */
   if ( in->intraflag ) out->intraflag = astStore( NULL, in->intraflag,
                                                   strlen( in->intraflag ) +
                                                   (size_t) 1 );

/* If an error occurred, free any allocated memory. */
   if ( !astOK ) {
      out->intraflag = astFree( out->intraflag );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for IntraMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for IntraMap objects.

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
   AstIntraMap *this;            /* Pointer to IntraMap */

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) obj;

/* Free the memory used for the IntraFlag string if necessary. */
   this->intraflag = astFree( this->intraflag );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for IntraMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the IntraMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the IntraMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS             /* Pointer to thread-specific global data */
   AstIntraMap *this;             /* Pointer to the IntraMap structure */
   const char *sval;              /* Pointer to string value */
   int set;                       /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the IntraMap structure. */
   this = (AstIntraMap *) this_object;

/* Write out values representing the instance variables for the
   IntraMap class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* Transformation function name. */
/* ----------------------------- */
   astWriteString( channel, "Fname", 1, 1, tran_data[ this->ifun ].name,
                   "Name of transformation function" );

/* IntraFlag string. */
/* ----------------- */
   set = TestIntraFlag( this, status );
   sval = set ? GetIntraFlag( this, status ) : astGetIntraFlag( this );
   astWriteString( channel, "Iflag", set, 0, sval,
                   "IntraMap identification string" );

/* Purpose string. */
/* --------------- */
   astWriteString( channel, "Purp", 1, 1, tran_data[ this->ifun ].purpose,
                   "Purpose of function" );

/* Author's name. */
/* -------------- */
   astWriteString( channel, "Auth", 1, 1, tran_data[ this->ifun ].author,
                   "Author's name" );

/* Contact details. */
/* ---------------- */
   astWriteString( channel, "Cntact", 1, 1, tran_data[ this->ifun ].contact,
                   "Contact address" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAIntraMap and astCheckIntraMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(IntraMap,Mapping)
astMAKE_CHECK(IntraMap)

AstIntraMap *astIntraMap_( const char *name, int nin, int nout,
                           const char *options, int *status, ...) {
/*
*++
*  Name:
c     astIntraMap
f     AST_INTRAMAP

*  Purpose:
*     Create an IntraMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "intramap.h"
c     AstIntraMap *astIntraMap( const char *name, int nin, int nout,
c                               const char *options, ... )
f     RESULT = AST_INTRAMAP( NAME, NIN, NOUT, OPTIONS, STATUS )

*  Class Membership:
*     IntraMap constructor.

*  Description:
*     This function creates a new IntraMap and optionally initialises
*     its attributes.
*
c     An IntraMap is a specialised form of Mapping which encapsulates
c     a privately-defined coordinate transformation function
c     (e.g. written in C) so that it may be used like any other AST
c     Mapping. This allows you to create Mappings that perform any
c     conceivable coordinate transformation.
f     An IntraMap is a specialised form of Mapping which encapsulates
f     a privately-defined coordinate transformation routine
f     (e.g. written in Fortran) so that it may be used like any other
f     AST Mapping. This allows you to create Mappings that perform any
f     conceivable coordinate transformation.
*
*     However, an IntraMap is intended for use within a single program
*     or a private suite of software, where all programs have access
*     to the same coordinate transformation functions (i.e. can be
*     linked against them). IntraMaps should not normally be stored in
*     datasets which may be exported for processing by other software,
*     since that software will not have the necessary transformation
*     functions available, resulting in an error.
*
c     You must register any coordinate transformation functions to be
c     used using astIntraReg before creating an IntraMap.
f     You must register any coordinate transformation functions to be
f     used using AST_INTRAREG before creating an IntraMap.

*  Parameters:
c     name
f     NAME = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing the name of
c        the transformation function to use (which should previously
c        have been registered using astIntraReg). This name is case
c        sensitive. All white space will be removed before use.
f        A character string containing the name of the transformation
f        routine to use (which should previously have been registered
f        using AST_INTRAREG). This name is case sensitive. All white
f        space will be removed before use.
c     nin
f     NIN = INTEGER (Given)
c        The number of input coordinates. This must be compatible with
c        the number of input coordinates accepted by the
c        transformation function (as specified when this function was
c        registered using astIntraReg).
f        The number of input coordinates. This must be compatible with
f        the number of input coordinates accepted by the
f        transformation routine (as specified when this routine was
f        registered using AST_INTRAREG).
c     nout
f     NOUT = INTEGER (Given)
c        The number of output coordinates. This must be compatible
c        with the number of output coordinates produced by the
c        transformation function (as specified when this function was
c        registered using astIntraReg).
f        The number of output coordinates. This must be compatible
f        with the number of output coordinates produced by the
f        transformation routine (as specified when this routine was
f        registered using AST_INTRAREG).
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new IntraMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new IntraMap. The syntax used is identical to that for the
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
c     astIntraMap()
f     AST_INTRAMAP = INTEGER
*        A pointer to the new IntraMap.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstIntraMap *new;             /* Pointer to new IntraMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the IntraMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitIntraMap( NULL, sizeof( AstIntraMap ), !class_init,
                          &class_vtab, "IntraMap", name, nin, nout );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   IntraMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new IntraMap. */
   return new;
}

AstIntraMap *astIntraMapId_( const char *name, int nin, int nout,
                             const char *options, ... ) {
/*
*  Name:
*     astIntraMapId_

*  Purpose:
*     Create an IntraMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "intramap.h"
*     AstIntraMap *astIntraMapId_( const char *name, int nin, int nout,
*                                  const char *options, ... )

*  Class Membership:
*     IntraMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astIntraMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astIntraMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*
*     The variable argument list also prevents this function from
*     invoking astIntraMap_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.

*  Parameters:
*     As for astIntraMap_.

*  Returned Value:
*     The ID value associated with the new IntraMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstIntraMap *new;             /* Pointer to new IntraMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the IntraMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitIntraMap( NULL, sizeof( AstIntraMap ), !class_init,
                          &class_vtab, "IntraMap", name, nin, nout );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   IntraMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new IntraMap. */
   return astMakeId( new );
}


AstIntraMap *astInitIntraMap_( void *mem, size_t size, int init,
                               AstIntraMapVtab *vtab, const char *name,
                               const char *fname, int nin, int nout, int *status ) {
/*
*+
*  Name:
*     astInitIntraMap

*  Purpose:
*     Initialise an IntraMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "intramap.h"
*     AstIntraMap *astInitIntraMap( void *mem, size_t size, int init,
*                                   AstIntraMapVtab *vtab, const char *name,
*                                   const char *fname, int nin, int nout )

*  Class Membership:
*     IntraMap initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new IntraMap object. It allocates memory (if
*     necessary) to accommodate the IntraMap plus any additional data
*     associated with the derived class.  It then initialises a
*     IntraMap structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for a IntraMap at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the IntraMap is to be
*        initialised.  This must be of sufficient size to accommodate
*        the IntraMap data (sizeof(IntraMap)) plus any data used by
*        the derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the IntraMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the IntraMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A logical flag indicating if the IntraMap's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new IntraMap.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     fname
*        Pointer to a null-terminated string containing the name of
*        the transformation function to be used, as previously
*        registered using astIntraReg.
*     nin
*        The number of input coordinates.
*     nout
*        The number of output coordinates.

*  Returned Value:
*     A pointer to the new IntraMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS             /* Pointer to thread-specific global data */
   AstIntraMap *new;              /* Pointer to new IntraMap */
   char *clname;                  /* Cleaned transformation function name */
   int found;                     /* Transformation function name found? */
   int ifun;                      /* Loop counter for registered functions */

/* Initialise. */
   new = NULL;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   found = 0;
   ifun = 0;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitIntraMapVtab( vtab, name );

/* Clean (and validate) the transformation function name supplied. */
   clname = CleanName( fname, "astIntraMap", status );

/* Search for a registered transformation function name which matches. */
   if ( astOK ) {
      found = 0;
      for ( ifun = 0; ifun < tran_nfun; ifun++ ) {
         if ( ( found = !strcmp( clname, tran_data[ ifun ].name ) ) ) break;
      }
   }

/* Free the memory containing the cleaned name string. */
   clname = astFree( clname );

/* If no match was found, then report an error. */
   if ( astOK ) {
      if ( !found ) {
         astError( AST__URITF, "astInitIntraMap(%s): The transformation "
                               "function \"%s\" has not been registered using "
                               "astIntraReg.", status, name, clname );

/* Check that the number of input coordinates is compatible with the
   number used by the transformation function (as specified when it
   was registered). Report an error if necessary. */
      } else {
         if ( ( nin != tran_data[ ifun ].nin ) &&
              ( tran_data[ ifun ].nin != AST__ANY ) ) {
            astError( AST__BADNI, "astInitIntraMap(%s): Number of input "
                                  "coordinates (%d) does not match the number "
                                  "used by the \"%s\" transformation function "
                                  "(%d).", status, name, nin, tran_data[ ifun ].name,
                                  tran_data[ ifun ].nin  );

/* Similarly check the number of output coordinates. */
         } else if ( ( nout != tran_data[ ifun ].nout ) &&
                     ( tran_data[ ifun ].nout != AST__ANY ) ) {
            astError( AST__BADNO, "astInitIntraMap(%s): Number of output "
                                  "coordinates (%d) does not match the number "
                                  "used by the \"%s\" transformation function "
                                  "(%d).", status, name, nout, tran_data[ ifun ].name,
                                  tran_data[ ifun ].nout  );

/* If OK, initialise a Mapping structure (the parent class) as the
   first component within the IntraMap structure, allocating memory if
   necessary (note that this also provides further checks on the
   validity of "nin" and "nout"). Specify whether the forward and
   inverse transformations are defined. */
         } else {
            new = (AstIntraMap *) astInitMapping( mem, size, 0,
                     (AstMappingVtab *) vtab, name, nin, nout,
                     ( ( tran_data[ ifun ].flags & AST__NOFWD ) == 0 ),
                     ( ( tran_data[ ifun ].flags & AST__NOINV ) == 0 ) );

            if ( astOK ) {

/* Initialise the IntraMap data. */
/* ---------------------------- */
/* Initialise the IntraFlag string pointer. */
               new->intraflag = NULL;

/* Store the index used to access the transformation function data. */
               new->ifun = ifun;

/* If an error occurred, clean up by deleting the new IntraMap. */
               if ( !astOK ) new = astDelete( new );
            }
         }
      }
   }

/* Return a pointer to the new IntraMap. */
   return new;
}

AstIntraMap *astLoadIntraMap_( void *mem, size_t size,
                               AstIntraMapVtab *vtab, const char *name,
                               AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadIntraMap

*  Purpose:
*     Load an IntraMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "intramap.h"
*     AstIntraMap *astLoadIntraMap( void *mem, size_t size,
*                                    AstIntraMapVtab *vtab, const char *name,
*                                    AstChannel *channel )

*  Class Membership:
*     IntraMap loader.

*  Description:
*     This function is provided to load a new IntraMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     IntraMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for an IntraMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the IntraMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        IntraMap data (sizeof(IntraMap)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the IntraMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the IntraMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstIntraMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new IntraMap. If this is NULL, a pointer
*        to the (static) virtual function table for the IntraMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "IntraMap" is used instead.

*  Returned Value:
*     A pointer to the new IntraMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS             /* Pointer to thread-specific global data */
   AstIntraMap *new;              /* Pointer to the new IntraMap */
   char *author;                  /* Pointer to author's name string */
   char *contact;                 /* Pointer to contact details string */
   char *fname;                   /* Pointer to transformation function name */
   char *purpose;                 /* Pointer to purpose comment string */
   int found;                     /* Function name found? */
   int ifun;                      /* Loop counter for registered functions */
   int nin;                       /* Number of IntraMap input coordinates */
   int nout;                      /* Number of IntraMap output coordinates */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this IntraMap. In this case the
   IntraMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstIntraMap );
      vtab = &class_vtab;
      name = "IntraMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitIntraMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built IntraMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "IntraMap" );

/* Now read each individual data item from this list. */

/* Transformation function name. */
/* ----------------------------- */
      fname = astReadString( channel, "fname", "" );

/* IntraFlag string. */
/* ----------------- */
      new->intraflag = astReadString( channel, "iflag", NULL );

/* Purpose string. */
/* --------------- */
      purpose = astReadString( channel, "purp", "" );

/* Author's name. */
/* -------------- */
      author = astReadString( channel, "auth", "" );

/* Contact details. */
/* ---------------- */
      contact = astReadString( channel, "cntact", "" );

/* If OK, search the array of transformation function data to see if
   the required transformation function has been registered. */
      if ( astOK ) {
         found = 0;
         for ( ifun = 0; ifun < tran_nfun; ifun++ ) {
            if ( ( found = !strcmp( fname, tran_data[ ifun ].name ) ) ) break;
         }

/* If the transformation function has not been registered, report an
   error explaining how to obtain it from the author and register
   it. */
         if ( !found ) {
            astError( AST__URITF, "astLoadIntraMap(%s): An IntraMap was read "
                                  "which uses an unknown transformation "
                                  "function.", status, astGetClass( channel ) );
            astError( AST__URITF, "This is a private extension to the AST "
                                  "library: to handle it, you must obtain the "
                                  "source code from its author." , status);
            astError( AST__URITF, "You can then register it with AST in your "
                                  "software by calling astIntraReg (see "
                                  "SUN/211)." , status);
            astError( AST__URITF, " " , status);
            astError( AST__URITF, "   Function name:   \"%s\".", status, fname );
            astError( AST__URITF, "   Purpose:         \"%s\".", status, purpose );
            astError( AST__URITF, "   Author:          \"%s\".", status, author );
            astError( AST__URITF, "   Contact address: \"%s\".", status, contact );
            astError( AST__URITF, " " , status);

/* Obtain the numbers of input and output coordinates for the
   IntraMap. Use parent methods for this, since if any derived class
   has overridden these methods it may depend on data that have not
   yet been loaded. */
         } else {
            nin = ( *parent_getnin )( (AstMapping *) new, status );
            nout = ( *parent_getnout )( (AstMapping *) new, status );
            if ( astOK ) {

/* Check that the numbers of coordinates are compatible with the
   numbers used by the transformation function, as specified when it
   was registered. */
               if ( ( nin != tran_data[ ifun ].nin ) &&
                    ( tran_data[ ifun ].nin != AST__ANY ) ) {
                  astError( AST__BADNI, "astLoadIntraMap(%s): The number of "
                                        "input coordinates for the IntraMap "
                                        "read (%d) does not match the number "
                                        "used by the registered \"%s\" "
                                        "transformation function (%d).", status,
                                        astGetClass( channel ), nin,
                                        tran_data[ ifun ].name,
                                        tran_data[ ifun ].nin  );
               } else if ( ( nout != tran_data[ ifun ].nout ) &&
                           ( tran_data[ ifun ].nout != AST__ANY ) ) {
                  astError( AST__BADNO, "astLoadIntraMap(%s): The number of "
                                        "output coordinates for the IntraMap "
                                        "read (%d) does not match the number "
                                        "used by the registered \"%s\" "
                                        "transformation function (%d).", status,
                                        astGetClass( channel ), nout,
                                        tran_data[ ifun ].name,
                                        tran_data[ ifun ].nout  );

/* If OK, store the index used to access the transformation function
   data. */
               } else {
                  new->ifun = ifun;
               }
            }
         }
      }

/* Free strings allocated by astReadString. */
      fname = astFree( fname );
      purpose = astFree( purpose );
      author = astFree( author );
      contact = astFree( contact );

/* If an error occurred, clean up by deleting the new IntraMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new IntraMap pointer. */
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





