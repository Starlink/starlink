/*
*class++
*  Name:
*     PermMap

*  Purpose:
*     Coordinate permutation Mapping.

*  Constructor Function:
c     astPermMap
f     AST_PERMMAP

*  Description:
*     A PermMap is a Mapping which permutes the order of coordinates,
*     and possibly also changes the number of coordinates, between its
*     input and output.
*
*     In addition to permuting the coordinate order, a PermMap may
*     also assign constant values to coordinates. This is useful when
*     the number of coordinates is being increased as it allows fixed
*     values to be assigned to any new ones.

*  Inheritance:
*     The PermMap class inherits from the Mapping class.

*  Attributes:
*     The PermMap class does not define any new attributes beyond
*     those which are applicable to all Mappings.

*  Functions:
c     The PermMap class does not define any new functions beyond those
f     The PermMap class does not define any new routines beyond those
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
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     29-FEB-1996 (RFWS):
*        Original version.
*     26-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     3-JUN-1997 (RFWS):
*        Over-ride the MapMerge method.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitPermMapVtab
*        method.
*     11-SEP-2003 (DSB):
*        Added methods astGetInPerm and astGetOutPerm.
*     2-NOV-2004 (DSB):
*        Added method astGetConstants.
*     14-MAR-2006 (DSB):
*        Override astEqual.
*     2-MAY-2007 (DSB):
*        Change MapSplit so that it does not try to use the
*        implementation from the parent Mapping class, since this
*        class can do a better job.
*     11-SEP-2007 (DSB):
*        In MapSplit, check that the permuted axis index is less than the
*        number of axes available. Use AST__BAD otherwise.
*     10-JAN-2011 (DSB):
*        Add protected PermSplit attribute.
*     11-FEB-2011 (DSB):
*        Do not allow MapSplit to return a Mapping with zero outputs.
*     22-NOV-2012 (DSB):
*        When using a default inperm array (as indicated by a NULL pointer
*        for inperm), ensure the array is padded with "-1" values if the
*        number of inputs exceeds the number of outputs. Also do the 
*        equivalent for default outperm arrays.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS PermMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "unitmap.h"             /* Unit Mappings */
#include "permmap.h"             /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

/* Module Macros */
/* ============= */
/* A macro that returns the inperm or outperm value to use for a given
   index, taking account of the possibility that the inperm or outperm may
   be NULL (implying a unit permutation), and that he numbers of inputs
   and outputs may not be equal. "perms" is a pointer to the integer
   permutation array (inperm or outperm), "i" is the index of the required
   element of the permutation array, and "maxperm" is one more than the
   maximum value allowed in the permutation array (i.e. the number of
   PermMap outputs if "perms" is inperm, or PermMap inputs if "perms" is
   outperm). */
#define PERMVAL( perms, i, maxperm ) ( perms ? perms[ i ] : (  i < maxperm ? i : -1 ))

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(PermMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(PermMap,Class_Init)
#define class_vtab astGLOBAL(PermMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPermMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPermMap *astPermMapId_( int, const int [], int, const int [], const double [], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double *GetConstants( AstPermMap *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int Equal( AstObject *, AstObject *, int * );
static int *GetInPerm( AstPermMap *, int * );
static int *GetOutPerm( AstPermMap *, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int NullPerm( AstPermMap *, int, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );

static void SetPermSplit( AstPermMap *, int, int * );
static void ClearPermSplit( AstPermMap *, int * );
static int TestPermSplit( AstPermMap *, int * );
static int GetPermSplit( AstPermMap *, int * );


/* Member functions. */
/* ================= */

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two PermMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "permmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     PermMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two PermMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a PermMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the PermMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPermMap *that;
   AstPermMap *this;
   int *that_inp;
   int *that_outp;
   int *this_inp;
   int *this_outp;
   int i;
   int nin;
   int nin_that;
   int nout;
   int nout_that;
   int p1;
   int p2;
   int result;
   int that_inp_len;
   int that_outp_len;
   int this_inp_len;
   int this_outp_len;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two PermMap structures. */
   this = (AstPermMap *) this_object;
   that = (AstPermMap *) that_object;

/* Check the second object is a PermMap. We know the first is a
   PermMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAPermMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNout( that ) == nout && astGetNin( that ) == nin ) {

/* Assume the PermMaps are equivalent. */
         result = 1;

/* Get the number of inputs and outputs in the second PermMap. */
         nin_that = astGetNin( that );
         nout_that = astGetNout( that );

/* Get pointers to the effective inperm and outperm array for each PermMap.
   If the Invert flags of the two PermMaps are not equal, we swap the
   arrays for the second PermMap in order to take account of the relative
   inversion of the second PermMap. */
         this_inp = this->inperm;
         this_outp = this->outperm;

         if(  astGetInvert( this ) ) {
            this_inp_len =  nout;
            this_outp_len =  nin;
         } else {
            this_inp_len =  nin;
            this_outp_len =  nout;
         }

         if( astGetInvert( this ) != astGetInvert( that ) ) {
            that_inp = that->outperm;
            that_outp = that->inperm;

            if(  astGetInvert( that ) ) {
               that_inp_len =  nin_that;
               that_outp_len =  nout_that;
            } else {
               that_inp_len =  nout_that;
               that_outp_len =  nin_that;
            }

         } else {
            that_inp = that->inperm;
            that_outp = that->outperm;

            if(  astGetInvert( that ) ) {
               that_inp_len =  nout_that;
               that_outp_len =  nin_that;
            } else {
               that_inp_len =  nin_that;
               that_outp_len =  nout_that;
            }
         }

/* Loop round every PermMap input. */
         for( i = 0; i < nin; i++ ) {

/* See what is fed to this input by the inverse transformation. A zero or
   positive integer "p" value indicates that the input is fed from the
   output with the corresponding index. A negative integer "p" value means
   the input is fed a constant value stored at index (-p-1) in the
   associated constants array. */
            p1 = PERMVAL( this_inp, i, this_outp_len );
            p2 = PERMVAL( that_inp, i, that_outp_len );

/* If the "p" values differ, we may have evidence that the PermMaps are
   not equivalent. */
            if( p1 != p2 ) {

/* If either "p" value is zero or positive, then the PermMaps are
   definitely different since input "i" is fed from differing outputs, or
   one is fed from an input and the other is fed a constant. */
               if( p1 >= 0 || p2 >= 0 ) {
                  result = 0;
                  break;

/* If both "p" values are negative, then both inputs are fed a constant
   value. The PermMaps differ if these constants differ. */
               } else if( this->constant[ -p1 - 1 ] !=
                          that->constant[ -p2 - 1 ] ) {
                  result = 0;
                  break;
               }
            }
         }

/* If we have not yet discovered any evidence that the PermMaps differ,
   go on to check each output in the same way that we have just checked the
   inputs. */
         if( result ) {
            for( i = 0; i < nout; i++ ) {
               p1 = PERMVAL( this_outp, i, this_inp_len );
               p2 = PERMVAL( that_outp, i, that_inp_len );

               if( p1 != p2 ) {
                  if( p1 >= 0 || p2 >= 0 ) {
                     result = 0;
                     break;
                  } else if( this->constant[ -p1 - 1 ] !=
                             that->constant[ -p2 - 1 ] ) {
                     result = 0;
                     break;
                  }
               }
            }
         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static double *GetConstants( AstPermMap *this, int *status ){
/*
*+
*  Name:
*     astGetConstants

*  Purpose:
*     Return a pointer to the constants array of a PermMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "permmap.h"
*     double *astGetConstants( AstPermMap *this )

*  Class Membership:
*     PermMap method

*  Description:
*     This function returns a pointer to a dynamically allocated array
*     holding a copy of the constants array supplied when the PermMap was
*     created.
*
*     Negative values in the arrays returned by the astGetInPerm and
*     astGetOutPerm methods can be used as indices into the constants
*     array returned by this method, if they are first negated and then
*     decrement by one. Thus an inperm value of -3 refers to element 2 of
*     the constants array.

*  Parameters:
*     this
*        Pointer to the PermMap.

*  Returned Value:
*     A pointer to a dynamically allocated array holding a copy of the
*     constants array. The pointer should be freed using astFree when it is
*     no longer needed. NULL will be returned if the PermMap has no
*     constants.

*  Notes:
*     - A value of NULL will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   double *result;                /* Pointer to the returned array */

/* Initialise the returned result. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Allocate memory and put a copy of the InPerm array in it. */
   result = (double *) astStore( NULL, this->constant, astSizeOf( this->constant ) );

/* Return the result. */
   return result;
}

static int *GetInPerm( AstPermMap *this, int *status ){
/*
*  Name:
*     GetInPerm

*  Purpose:
*     Return a pointer to the InPerm array of a PermMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "permmap.h"
*     int *astGetInPerm( AstPermMap *this, int *status )

*  Class Membership:
*     PermMap method

*  Description:
*     This function returns a pointer to a dynamically allocated array
*     holding a copy of the InPerm array supplied when thre PermMap was
*     created.

*  Parameters:
*     this
*        Pointer to the PermMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array holding a copy of the
*     InPerm array. The pointer should be freed using astFree when it is
*     no longer needed. The number of elements in the array will be given
*     by the value of the Nin attribute. The value in element "i" is the
*     zero-based index of the output axis which provides values for input
*     "i" when the inverse transformation is used. If the value in element
*     "i" is less than zero, then input "i" is fed a constant value. This
*     constant value is stored in the "constants" array (see astGetConstants)
*     at an index equal to the absolute value of inperm[i], minus 1. Thus
*     if element 3 of the array returned by this function has value -2,
*     then input axis 3 is fed the value held in constants[1]. If the
*     value of element "i" of the returned inperm array is greater than
*     or equal to the number of output axes, then input "i" will be fed
*     the constant AST__BAD.

*  Notes:
*     - A value of NULL will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int i;                      /* Loop count */
   int nin;                    /* Number of inputs. */
   int *result;                /* Pointer to the returned array */

/* Initialise the returned result. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If no inperm array is stored, every input is derived from the
   corresponding output. Therefore, return an array holding 0 to Nin-1. */
   if( !this->inperm ) {
      nin = astGetNin( this );
      result = (int *) astMalloc( sizeof( int ) * (size_t) nin );
      if( astOK ) for( i = 0; i < nin; i++ ) result[ i ] = i;

/* Otherwise, allocate memoy and put a copy of the InPerm array in it. */
   } else {
      result = (int *) astStore( NULL, this->inperm,
                              sizeof( int ) * (size_t) astGetNin( this ) );
   }

/* Return the result. */
   return result;
}

static int *GetOutPerm( AstPermMap *this, int *status ){
/*
*  Name:
*     GetOutPerm

*  Purpose:
*     Return a pointer to the OutPerm array of a PermMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "permmap.h"
*     int *astGetOutPerm( AstPermMap *this, int *status )

*  Class Membership:
*     PermMap method

*  Description:
*     This function returns a pointer to a dynamically allocated array
*     holding a copy of the OutPerm array supplied when thre PermMap was
*     created.

*  Parameters:
*     this
*        Pointer to the PermMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array holding a copy of the
*     OutPerm array. The pointer should be freed using astFree when it is
*     no longer needed. The number of elements in the array will be given
*     by the value of the Nout attribute. The value in element "i" is the
*     zero-based index of the input axis which provides values for output
*     "i" when the forward transformation is used. If the value in element
*     "i" is less than zero, then output "i" is fed a constant value. This
*     constant value is stored in the "constants" array (see astGetConstants)
*     at an index equal to the absolute value of outperm[i], minus 1. Thus
*     if element 3 of the array returned by this function has value -2,
*     then output axis 3 is fed the value held in constants[1]. If the
*     value of element "i" of the returned outperm array is greater than
*     or equal to the number of input axes, then output "i" will be fed
*     the constant AST__BAD.

*  Notes:
*     - A value of NULL will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int i;                      /* Loop count */
   int nout;                   /* Number of outputs. */
   int *result;                /* Pointer to the returned array */

/* Initialise the returned result. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If no outperm array is stored, every output is derived from the
   corresponding input. Therefore, return an array holding 0 to Nout-1. */
   if( !this->outperm ) {
      nout = astGetNout( this );
      result = (int *) astMalloc( sizeof( int ) * (size_t) nout );
      if( astOK ) for( i = 0; i < nout; i++ ) result[ i ] = i;

/* Otherwise, allocate memory and put a copy of the OutPerm array in it. */
   } else {
      result = (int *) astStore( NULL, this->outperm,
                              sizeof( int ) * (size_t) astGetNout( this ) );
   }

/* Return the result. */
   return result;
}

void astInitPermMapVtab_(  AstPermMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitPermMapVtab

*  Purpose:
*     Initialise a virtual function table for a PermMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "permmap.h"
*     void astInitPermMapVtab( AstPermMapVtab *vtab, const char *name )

*  Class Membership:
*     PermMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the PermMap class.

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
   will be used (by astIsAPermMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->GetConstants = GetConstants;
   vtab->GetInPerm = GetInPerm;
   vtab->GetOutPerm = GetOutPerm;
   vtab->ClearPermSplit = ClearPermSplit;
   vtab->GetPermSplit = GetPermSplit;
   vtab->SetPermSplit = SetPermSplit;
   vtab->TestPermSplit = TestPermSplit;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   mapping->MapSplit = MapSplit;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;
   mapping->Rate = Rate;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "PermMap", "Coordinate permutation" );

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
*     Simplify a sequence of Mappings containing a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     PermMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated PermMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated PermMap with one which it
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
*        Pointer to the nominated PermMap which is to be merged with
*        its neighbours. This should be a cloned copy of the PermMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        PermMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated PermMap resides.
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
   AstMapping *map;              /* Pointer to Mapping */
   AstMapping *new;              /* Pointer to replacement Mapping */
   AstPermMap *permmap;          /* Pointer to PermMap */
   const char *class;            /* Pointer to Mapping class string */
   double *con;                  /* Pointer to constants array */
   double constant;              /* Constant value */
   int *inperm;                  /* Pointer to "inperm" permutation array */
   int *newperm;                 /* Pointer to new permutation array */
   int *outperm;                 /* Pointer to "outperm" permutation array */
   int *perm;                    /* Pointer to individual permutation array */
   int back;                     /* Considering inverse transformation? */
   int coord;                    /* Loop counter for coordinates */
   int icon;                     /* Loop counter for constants */
   int iend;                     /* Loop ending value */
   int imap1;                    /* Index of first Mapping */
   int imap2;                    /* Index of last Mapping */
   int imap;                     /* Loop counter for Mappings */
   int inc;                      /* Loop increment */
   int invert;                   /* Invert attribute value */
   int istart;                   /* Loop starting value */
   int maxperm;                  /* Max value (+1) allowed in permutation array */
   int ncon;                     /* Number of constants */
   int ncoord_in;                /* Effective number of input coordinates */
   int ncoord_out;               /* Effective number of output coordinates */
   int ngone;                    /* Number of Mappings eliminated */
   int nin;                      /* Total number of input coordinates */
   int ninsum;                   /* Accumulated count of input coordinates */
   int nout;                     /* Total number of output coordinates */
   int noutsum;                  /* Accumulated count of output coordinates */
   int nperm;                    /* Number of permutation array elements */
   int p;                        /* Permuted coordinate index */
   int result;                   /* Result value to return */
   int simpler;                  /* Mapping(s) simplified? */
   int store_in;                 /* Need to store "inperm" array contents? */
   int store_out;                /* Need to store "outperm" array contents? */
   int unit;                     /* Replacement Mapping is a UnitMap? */

/* Initialise the returned result. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Further initialisation. */
   con = NULL;
   inperm = outperm = NULL;
   ncon = 0;
   permmap = NULL;

/* In series. */
/* ---------- */
/* Handle the case where the Mappings are connected in series. */
   if ( series ) {

/* Search adjacent lower-numbered Mappings until one is found which is
   not a PermMap or a UnitMap. */
      imap1 = where;
      while ( ( imap1 - 1 ) >= 0 ) {
         class = astGetClass( ( *map_list )[ imap1 - 1 ] );
         if ( astOK ) {
            if ( strcmp( class, "PermMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;
            imap1--;
         }
      }

/* Similarly search adjacent higher-numbered Mappings. */
      imap2 = where;
      while ( ( imap2 + 1 ) < *nmap ) {
         class = astGetClass( ( *map_list )[ imap2 + 1 ] );
         if ( astOK ) {
            if ( strcmp( class, "PermMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;
            imap2++;
         }
      }

/* Obtain a pointer to the first Mapping found and determine if it is
   to be applied with its Invert attribute set. */
      map = ( *map_list )[ imap1 ];
      invert = ( *invert_list )[ imap1 ];

/* Use this first Mapping (allowing for how its Invert attribute is
   currently set) to determine the number of input coordinates that
   the simplified Mapping should have. */
      if ( astGetInvert( map ) ) {
         nin = invert ? astGetNin( map ) : astGetNout( map );
      } else {
         nin = invert ? astGetNout( map ) : astGetNin( map );
      }

/* Repeat this process for the last Mapping found, to determine the
   number of output coordinates for the simplified Mapping. */
      map = ( *map_list )[ imap2 ];
      invert = ( *invert_list )[ imap2 ];
      if ( astGetInvert( map ) ) {
         nout = invert ? astGetNout( map ) : astGetNin( map );
      } else {
         nout = invert ? astGetNin( map ) : astGetNout( map );
      }

/* Allocate memory to hold input and output permutation arrays for the
   simplified Mapping, together with a list of constants. */
      inperm = astMalloc( sizeof( int ) * (size_t) nin );
      outperm = astMalloc( sizeof( int ) * (size_t) nout );
      con = astMalloc( sizeof( double ) * (size_t) ( nin + nout ) );
      if ( astOK ) {

/* Initialise the number of constants. */
         ncon = 0;

/* Loop twice, to calculate the forward and inverse (backward)
   simplified permutation arrays in turn. */
         for ( back = 0; back <= 1; back++ ) {

/* Obtain a pointer to the appropriate (forward/inverse) permutation
   array that we wish to fill, and obtain the number of elements it
   will contain. Initialise the array contents to represent a null
   permutation.*/
            newperm = back ? outperm : inperm;
            nperm = back ? nout : nin;
            for ( coord = 0; coord < nperm; coord++ ) newperm[ coord ] = coord;

/* Set up limits to scan through the list of Mappings being merged in
   either the forward or reverse order, as required. */
            istart = back ? imap2 : imap1;
            iend = back ? imap1 - 1 : imap2 + 1;
            inc = back ? -1 : 1;

/* Loop through the Mappings, obtaining a pointer to each, together
   with the value to be used for its Invert attribute. Invert this
   attribute value if calculating the overall inverse (backward)
   permutation array. */
            for ( imap = istart; imap != iend; imap += inc ) {
               map = ( *map_list )[ imap ];
               invert = ( *invert_list )[ imap ];
               if ( back ) invert = !invert;

/* Determine the class to which the Mapping belongs. */
               class = astGetClass( map );
               if ( astOK ) {

/* If it is a PermMap, obtain a pointer to the PermMap structure and
   hence to the relevant permutation array.  Otherwise (if it is a
   UnitMap), leave the permutation array pointer NULL, which indicates
   a null permutation. */
                  perm = NULL;
                  maxperm = astGetNout( map );
                  if ( !strcmp( class, "PermMap" ) ) {
                     permmap = (AstPermMap *) map;
                     perm = invert ? permmap->outperm : permmap->inperm;
                  }

/* Obtain the effective number of output coordinates associated with
   this individual Mapping (when transforming points in the direction
   to which this permutation array applies). */
                  if ( astGetInvert( map ) ) {
                     ncoord_out = invert ? astGetNout( map ) :
                                           astGetNin( map );
                  } else {
                     ncoord_out = invert ? astGetNin( map ) :
                                           astGetNout( map );
                  }

/* Loop through the elements of the simplified permutation array to
   accumulate the effects of the current individual Mapping. */
                  if ( astOK ) {
                     for ( coord = 0; coord < nperm; coord++ ) {

/* Find the effective input coordinate for the current Mapping from
   the permutation accumulated so far, and check this is not
   negative. If it is, the accumulated permutation refers to a "bad"
   coordinate value or a constant, so the current Mapping makes no
   further difference. */
                        p = newperm[ coord ];
                        if ( p >= 0 ) {

/* Otherwise, obtain the permuting effect of the current Mapping,
   allowing for the possibility of its permutation array being NULL
   (implying a null permutation). */
                           p = PERMVAL( perm, p, maxperm );

/* If the permuted index refers to a valid (effective) output
   coordinate for the individual Mapping, then accumulate its effect
   in the overall permutation array. */
                           if ( ( p >= 0 ) && ( p < ncoord_out ) ) {
                              newperm[ coord ] = p;

/* Otherwise (this can only occur if the individual Mapping is a
   PermMap), determine whether it refers to a "bad" coordinate value
   or a constant. If the former, extract the constant's value,
   otherwise use a constant value of AST__BAD. */
                           } else {
                              if ( ( p < 0 ) && permmap->constant ) {
                                 constant = permmap->constant[ (-p) - 1 ];
                              } else {
                                 constant = AST__BAD;
                              }

/* If the result (however reached) is a coordinate value of AST__BAD,
   then mark the accumulated permutation array with a value of -1 to
   indicate this. */
                              if ( constant == AST__BAD ) {
                                 newperm[ coord ] = -1;

/* Otherwise, search the array of constants to see if this one has
   been encountered before. If not, append the new constant to the
   list. */
                              } else {
                                 for ( icon = 0; icon < ncon; icon++ ) {
                                    if ( con[ icon ] == constant ) break;
                                 }
                                 if ( icon == ncon ) con[ ncon++ ] = constant;

/* Store a (negative) reference to the new constant in the accumulated
   permutation array (note we use an extra offset of -1 here in
   forming these references, so that the value -1 itself can be used
   to indicate a "bad" coordinate value without an entry in the
   constants array). */
                                 newperm[ coord ] = (-icon) - 2;
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }

/* In parallel. */
/* ------------ */
/* Handle the case where the Mappings are connected in parallel. */
   } else {

/* Obtain a pointer to the nominated Mapping (which is a PermMap) and
   determine if it is to be applied with its Invert attribute set. */
      map = ( *map_list )[ where ];
      invert = ( *invert_list )[ where ];

/* Use this nominated Mapping to initialise the counts of input and
   output coordinates for the simplified Mapping (allowing for how its
   Invert attribute is currently set). */
      if ( astGetInvert( map ) ) {
         nin = invert ? astGetNin( map ) : astGetNout( map );
         nout = invert ? astGetNout( map ) : astGetNin( map );
      } else {
         nin = invert ? astGetNout( map ) : astGetNin( map );
         nout = invert ? astGetNin( map ) : astGetNout( map );
      }

/* Search adjacent lower-numbered Mappings until one is found which is
   not a PermMap or a UnitMap. */
      imap1 = where;
      while ( astOK && ( ( imap1 - 1 ) >= 0 ) ) {
         map = ( *map_list )[ imap1 - 1 ];
         class = astGetClass( map );
         if ( astOK ) {
            if ( strcmp( class, "PermMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;

/* For each Mapping found, obtain the effective numbers of input and
   output coordinates (allowing for all the direction flags, as above)
   and accumulate the total count of input and output coordinates for
   the overall simplified Mapping. */
            invert = ( *invert_list )[ imap1 - 1 ];
            if ( astGetInvert( map ) ) {
               nin += ( invert ? astGetNin( map ) : astGetNout( map ) );
               nout += ( invert ? astGetNout( map ) : astGetNin( map ) );
            } else {
               nin += ( invert ? astGetNout( map ) : astGetNin( map ) );
               nout += ( invert ? astGetNin( map ) : astGetNout( map ) );
            }
            imap1--;
         }
      }

/* Similarly search higher-numbered Mappings and accumulate their
   coordinate counts. */
      imap2 = where;
      while ( astOK && ( ( imap2 + 1 ) < *nmap ) ) {
         map = ( *map_list )[ imap2 + 1 ];
         class = astGetClass( map );
         if ( astOK ) {
            if ( strcmp( class, "PermMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;
            invert = ( *invert_list )[ imap2 + 1 ];
            if ( astGetInvert( map ) ) {
               nin += ( invert ? astGetNin( map ) : astGetNout( map ) );
               nout += ( invert ? astGetNout( map ) : astGetNin( map ) );
            } else {
               nin += ( invert ? astGetNout( map ) : astGetNin( map ) );
               nout += ( invert ? astGetNin( map ) : astGetNout( map ) );
            }
            imap2++;
         }
      }

/* Allocate memory to hold input and output permutation arrays for the
   simplified Mapping, together with a list of constants. */
      inperm = astMalloc( sizeof( int ) * (size_t) nin );
      outperm = astMalloc( sizeof( int ) * (size_t) nout );
      con = astMalloc( sizeof( double ) * (size_t) ( nin + nout ) );
      if ( astOK ) {

/* Initialise the number of constants. */
         ncon = 0;

/* Loop twice, to calculate the forward and inverse (backward)
   simplified permutation arrays in turn. */
         for ( back = 0; back <= 1; back++ ) {

/* Obtain a pointer to the appropriate (forward/inverse) permutation
   array that we wish to fill, and obtain the number of elements it
   will contain. */
            newperm = back ? outperm : inperm;
            nperm = back ? nout : nin;

/* Initialise counts of (effective) input and output coordinates. */
            ninsum = noutsum = 0;

/* Loop through the Mappings, obtaining a pointer to each, together
   with the value to be used for its Invert attribute.  Invert this
   attribute value if calculating the overall inverse (backward)
   permutation array. */
            for ( imap = imap1; imap <= imap2; imap++ ) {
               map = ( *map_list )[ imap ];
               invert = ( *invert_list )[ imap ];
               if ( back ) invert = !invert;

/* Determine the class to which the Mapping belongs. */
               class = astGetClass( map );
               if ( astOK ) {

/* If it is a PermMap, obtain a pointer to the PermMap structure and
   hence to the relevant permutation array.  Otherwise (if it is a
   UnitMap), leave the permutation array pointer NULL, which indicates
   a null permutation. */
                  perm = NULL;
                  maxperm = astGetNout( map );
                  if ( !strcmp( class, "PermMap" ) ) {
                     permmap = (AstPermMap *) map;
                     perm = invert ? permmap->outperm : permmap->inperm;
                  }

/* Obtain the effective number of input and output coordinates
   associated with this individual Mapping (when transforming points
   in the direction to which this permutation array applies). */
                  if ( astGetInvert( map ) ) {
                     ncoord_in = invert ? astGetNin( map ) :
                                          astGetNout( map );
                     ncoord_out = invert ? astGetNout( map ) :
                                           astGetNin( map );
                  } else {
                     ncoord_in = invert ? astGetNout( map ) :
                                          astGetNin( map );
                     ncoord_out = invert ? astGetNin( map ) :
                                           astGetNout( map );
                  }

/* Loop through the (effective) input coordinates of the current
   individual Mapping to accumulate their effect on the overall
   permutation array. */
                  if ( astOK ) {
                     for ( coord = 0; coord < ncoord_in; coord++ ) {

/* Obtain the permuting effect of the current Mapping, allowing for
   the possibility of its permutation array being NULL. */
                        p = PERMVAL( perm, coord, maxperm );

/* If the permuted index refers to a valid (effective) output
   coordinate for the individual Mapping, then accumulate its effect
   on the overall permutation array, allowing for the coordinate
   numbering offset produced by any Mappings already accumulated. */
                        if ( ( p >= 0 ) && ( p < ncoord_out ) ) {
                           newperm[ coord + ninsum ] = p + noutsum;

/* Otherwise (this can only occur if the individual Mapping is a
   PermMap), determine whether it refers to a "bad" coordinate value
   or a constant. If the former, extract the constant's value,
   otherwise use a constant value of AST__BAD. */
                        } else {
                           if ( ( p < 0 ) && permmap->constant ) {
                              constant = permmap->constant[ (-p) - 1 ];
                           } else {
                              constant = AST__BAD;
                           }

/* If the result (however reached) is a coordinate value of AST__BAD,
   then mark the accumulated permutation array with a value of -1 to
   indicate this. */
                           if ( constant == AST__BAD ) {
                              newperm[ coord + ninsum ] = -1;

/* Otherwise, search the array of constants to see if this one has
   been encountered before. If not, append the new constant to the
   list. */
                           } else {
                              int icon;
                              for ( icon = 0; icon < ncon; icon++ ) {
                                 if ( con[ icon ] == constant ) break;
                              }
                              if ( icon == ncon ) con[ ncon++ ] = constant;

/* Store a (negative) reference to the new constant in the accumulated
   permutation array (note we use an extra offset of -1 here in
   forming these references, so that the value -1 itself can be used
   to indicate a "bad" coordinate value without an entry in the
   constants array). */
                              newperm[ coord + ninsum ] = (-icon) - 2;
                           }
                        }
                     }
                  }

/* Accumulate the counts of (effective) input and output coordinates
   for each individual Mapping. */
                  ninsum += ncoord_in;
                  noutsum += ncoord_out;
               }
            }
         }
      }
   }

/* Inspect each element of the accumulated "inperm" array to determine
   if it needs to be stored by the replacement PermMap. */
   if ( astOK ) {
      store_in = 0;
      for ( coord = 0; coord < nin; coord++ ) {

/* It need not be stored if it produces a null permutation, where each
   input coordinate takes its value from the corresponding output
   coordinate (or where a "bad" value results if there is no
   corresponding output coordinate). Note any deviation from this
   pattern. */
         if ( coord < nout ) {
            store_in = store_in || ( inperm[ coord ] != coord );
         } else {
            store_in = store_in || ( inperm[ coord ] != -1 );
         }

/* Also convert permutation array values of -1 into non-existent
   positive coordinate indices (indicating "bad" coordinate values)
   and adjust (negative) references to constants by +1 to eliminate
   the extra offset of -1 used temporarily above. This returns the
   permutation array values to normal. */
         if ( inperm[ coord ] < 0 ) {
            if ( !++inperm[ coord ] ) inperm[ coord ] = nout;
         }
      }

/* Similarly inspect the "outperm" array and return its values to
   normal. */
      store_out = 0;
      for ( coord = 0; coord < nout; coord++ ) {
         if ( coord < nin ) {
            store_out = store_out || ( outperm[ coord ] != coord );
         } else {
            store_out = store_out || ( outperm[ coord ] != -1 );
         }
         if ( outperm[ coord ] < 0 ) {
            if ( !++outperm[ coord ] ) outperm[ coord ] = nin;
         }
      }

/* Determine how many adjacent Mappings can be eliminated by merging
   them. */
      ngone = imap2 - imap1;

/* Determine if the resultant PermMap can be simplified still further
   to become a UnitMap (a null Mapping). This will be the case if both
   the forward and inverse coordinate permutations it produces are
   null, and if the number of input and output coordinates are
   equal. */
      unit = !store_in && !store_out && ( nin == nout );

/* We must now determine whether we have actually produced any
   simplification. This is important, because if we indicate a
   simplification when none has, in fact, been achieved, then this
   function may get called over and over again without end. */

/* Simplification is clearly evident if (a) Mappings have been
   eliminated ("ngone" is non-zero), or (b) a PermMap has been reduced
   to a UnitMap, or (c) where there was originally only one PermMap to
   simplify, its invert flag was set (the replacement Mapping will
   always have this flag cleared). */
      simpler = ngone || unit || ( *invert_list )[ where ];

/* If the above tests do not indicate simplification, then we can only
   be considering the case where there was a single initial
   PermMap. In this case we have also achieved simplification if
   either the "inperm" or "outperm" array no longer needs storing
   whereas previously it was stored. */
      permmap = (AstPermMap *) ( *map_list )[ where ];
      if ( !simpler ) {
         simpler = ( !store_in && !NullPerm( permmap, 0, status ) ) ||
                   ( !store_out && !NullPerm( permmap, 1, status ) );
      }

/* If we still haven't detected any simplification, then compare the
   original and replacement "inperm" arrays (if present) in detail for
   equality.  We declare simplification to have occurred if they
   differ. */
      if ( !simpler && store_in ) {
         for ( coord = 0; coord < nin; coord++ ) {
            simpler = ( inperm[ coord ] != permmap->inperm[ coord ] );
            if ( simpler ) break;
         }
      }

/* Similarly, if necessary, compare the original and replacement
   "outperm" arrays. */
      if ( !simpler && store_out ) {
         for ( coord = 0; coord < nout; coord++ ) {
            simpler = ( outperm[ coord ] != permmap->outperm[ coord ] );
            if ( simpler ) break;
         }
      }

/* Do nothing more unless there has been some simplification. */
      if ( simpler ) {

/* If the PermMaps (and UnitMaps) can be replaced by a UnitMap, then
   create the replacement. */
         if ( unit ) {
            new = (AstMapping *) astUnitMap( nin, "", status );

/* Otherwise, create a replacement PermMap, setting as many arguments
   to NULL in the constructor function as can be achieved without
   affecting the result. */
         } else {
            new = (AstMapping *) astPermMap( nin, store_in ? inperm : NULL,
                                             nout, store_out ? outperm : NULL,
                                             ncon ? con : NULL, "", status );
         }

/* Annul the pointers to all the Mappings that are being replaced. */
         if ( astOK ) {
            for ( imap = imap1; imap <= imap2; imap++ ) {
               ( *map_list )[ imap ] = astAnnul( ( *map_list )[ imap ] );
            }

/* Insert the new pointer and the associated invert flag. */
            ( *map_list )[ imap1 ] = new;
            ( *invert_list )[ imap1 ] = 0;

/* Loop to close the resulting gap by moving subsequent elements down
   in the arrays. */
            for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
               ( *map_list )[ imap - ngone ] = ( *map_list )[ imap ];
               ( *invert_list )[ imap - ngone ] = ( *invert_list )[ imap ];
            }

/* Clear the vacated elements at the end. */
            for ( imap = *nmap - ngone; imap < *nmap; imap++ ) {
               ( *map_list )[ imap ] = NULL;
               ( *invert_list )[ imap ] = 0;
            }

/* Decrement the Mapping count and return the index of the first
   modified element. */
            ( *nmap ) -= ngone;
            result = imap1;
         }
      }
   }

/* Free the workspace arrays. */
   inperm = astFree( inperm );
   outperm = astFree( outperm );
   con = astFree( con );

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static int *MapSplit( AstMapping *this_map, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "permmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     PermMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing PermMap. This is only possible if the specified inputs
*     correspond to some subset of the PermMap outputs. That is, there
*     must exist a subset of the PermMap outputs for which each output
*     depends only on the selected PermMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied PermMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the PermMap to be split (the PermMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied PermMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied PermMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied PermMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstPermMap *this;          /* Pointer to PermMap structure */
   double *con;               /* Pointer to constants array */
   int *inp;                  /* Input perm array to use with supplied PermMap */
   int *inpm;                 /* Input perm array to use with new PermMap */
   int *outp;                 /* Output perm array to use with supplied PermMap */
   int *outpm;                /* Output perm array to use with new PermMap */
   int *result;               /* Pointer to returned array */
   int i;                     /* Loop count */
   int iin;                   /* Mapping input index */
   int iout;                  /* Output index */
   int j;                     /* Loop count */
   int nout;                  /* No. of outputs in the new PermMap */
   int npin;                  /* No. of inputs in the supplied Mapping */
   int npout;                 /* No. of outputs in the supplied Mapping */
   int ok;                    /* Are input indices OK? */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;


/* Get a pointer to the PermMap structure. */
   this = (AstPermMap *) this_map;

/* Get the number of inputs and outputs in the supplied PermMap. */
   npin = astGetNin( this );
   npout = astGetNout( this );

/* Check all input axis indices are valid. */
   ok = 1;
   for( i = 0; i < nin; i++ ) {
      if( in[ i ] < 0 || in[ i ] >= npin ) {
         ok = 0;
         break;
      }
   }

/* Get pointers to the input and output permutation arrays and constant
   array taking account of whether the PermMap has been inverted. */
   if( astGetInvert( this ) ) {
      outp = this->inperm;
      inp = this->outperm;
   } else {
      outp = this->outperm;
      inp = this->inperm;
   }
   con = this->constant;

/* The "normal" method, as described in the prologue. */
   if( astGetPermSplit( this ) == 0 ) {

/* Allocate memory for the returned array of output indices. */
      result = astMalloc( sizeof( int )*(size_t) npout );

/* Allocate memory for the inperm and outperm arrays of the returned
   PermMap. Make these the largest they could possible need to be. */
      inpm = astMalloc( sizeof( int )*(size_t) npin );
      outpm = astMalloc( sizeof( int )*(size_t) npout );
      if( astOK ) {

/* Initialise number of outputs in returned PermMap. */
         nout = 0;

/* Loop round each output of the supplied PermMap. */
         for( iout = 0; iout < npout; iout++ ) {

/* Is this output fed by one of the selected inputs? If so store the input
   index of the returned Mapping, which feeds this output and add this
   output index to the list of returned outputs. */
            iin = PERMVAL( outp, iout, npin );
            if( iin >= 0 && iin < npin ) {
               for( i = 0; i < nin; i++ ) {
                  if( in[ i ] == iin ) {
                     outpm[ nout ] = i;
                     result[ nout ] = iout;
                     nout++;
                     break;
                  }
               }
            }
         }

/* We now need to set up the inperm array for the returned PermMap. This
   ensures that the inverse transformation in the returned Mapping provides
   values for the selected inputs. Loop round all the selected inputs. */
         for( i = 0; i < nin; i++ ) {
            iin = in[ i ];

/* Is this input constant or fed by one of the selected outputs? If so store
   the output or constant index in the returned Mapping which feeds this
   input. */
            ok = 0;
            iout = PERMVAL( inp, iin, npout );
            if( iout >= 0 && iout < npout ) {
               for( j = 0; j < nout; j++ ) {
                  if( result[ j ] == iout ) {
                     ok = 1;
                     inpm[ i ] = j;
                     break;
                  }
               }
            } else {
               inpm[ i ] = iout;
               ok = 1;
            }

/* If this input is fed by an output which has not been selected, then we
   cannot produce the required Mapping. */
            if( !ok ) break;
         }

/* If possible produce the returned PermMap. Otherwise, free the returned
   array. */
         if( ok && nout > 0 ) {
            *map = (AstMapping *) astPermMap( nin, inpm, nout, outpm, con, "", status );
         } else {
            result = astFree( result );
         }

/* Free other resources. */
         inpm = astFree( inpm );
         outpm = astFree( outpm );
      }

/* The "alternative" method. Only the inperm array is used - the outperm
   array is assumed to be an exact inverse of the inperm array. In other
   words, only the inverse transformation is used, and the forward
   transformation is assumed to be the exact opposite. */
   } else {

/* The returned array of output indices holds the "inperm" values for the
   selected inputs. */
      result = astMalloc( sizeof( int )*(size_t) nin );
      if( astOK ) {
         for( i = 0; i < nin; i++ ) {
            result[ i ] = PERMVAL( inp, in[ i ], npout );

/* Check the input is not fed by a constant. */
            if( result[ i ] < 0 ) {
               result = astFree( result );
               break;

/* Check that the the output has not already been used. */
            } else {
               for( j = 0; j < i; j++ ) {
                  if( result[ j ] == result[ i ] ) {
                     result = astFree( result );
                     break;
                  }
               }
            }
         }

/* If the split was possible, the returned Mapping is a UnitMap. */
         if( result ) *map = (AstMapping *) astUnitMap( nin, " ", status );
      }
   }

/* If the returned Mapping has no outputs, do not return it. */
   if( !result && *map ) {
      *map = astAnnul( *map );
   }

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int NullPerm( AstPermMap *this, int forward, int *status ){
/*
*  Name:
*     NullPerm

*  Purpose:
*     See if a PermMap transformation represents a null axis permutation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "permmap.h"
*     int NullPerm( AstPermMap *this, int forward, int *status )

*  Class Membership:
*     PermMap method

*  Description:
*     This function returns a logical value indicating if the specified
*     transformation of the supplied PermMap is a null (i.e. unit)
*     transformation.

*  Parameters:
*     this
*        Pointer to the PermMap.
*     forward
*        Check the forward transformation? Otherise, check the inverse
*        transformation.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the specified transformation is a null axis permutation.
*     Zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int i;                     /* Coordinate index */
   int nin;                   /* Number of Mapping inputs */
   int nout;                  /* Number of Mapping outputs */
   int result;                /* Returned value */

/* Initialise the returned result. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* First check the forward transformation, given by the outperm array. */
   if( forward ) {

/* If no outperm array is stored, every output is derived from the
   corresponding input. Therefore, return 1 indicating a null axis
   permutation. */
      if( !this->outperm ) {
         result = 1;

/* Otherwise, check that every element in the outperm array indicates
   that the output is derived from the input with the saem index. */
      } else {
         result = 1;
         nout = astGetNout( this );
         for( i = 0; i < nout; i++ ) {
            if( this->outperm[ i ] != i ) {
               result = 0;
               break;
            }
         }
      }

/* Now check the inverse transformation, given by the inperm array. */
   } else {

/* If no inperm array is stored, every input is derived from the
   corresponding output. Therefore, return 1 indicating a null axis
   permutation. */
      if( !this->inperm ) {
         result = 1;

/* Otherwise, check that every element in the inperm array indicates
   that the input is derived from the output with the same index. */
      } else {
         result = 1;
         nin = astGetNin( this );
         for( i = 0; i < nin; i++ ) {
            if( this->inperm[ i ] != i ) {
               result = 0;
               break;
            }
         }
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

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
*     #include "permmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     PermMap member function (overrides the astRate method inherited
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

*/

/* Local Variables: */
   AstPermMap *map;
   int *outperm;
   int result;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the PermMap structure. */
   map = (AstPermMap *) this;

/* Obtain a pointer to the appropriate output coordinate permutation array,
   according to whether the PermMap has been inverted. If the specified
   output is derived from the specified input then the rate is unity.
   Otherwise it is zero. */
   outperm = astGetInvert( this ) ? map->inperm : map->outperm;
   if( outperm ) {
      result = ( ax2 == outperm[ ax1 ] ) ? 1.0 : 0.0;
   } else {
      result = ( ax2 == ax1 ) ? 1.0 : 0.0;
   }

   return result;
}

static AstPointSet *Transform( AstMapping *map, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a PermMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "permmap.h"
*     AstPointSet *Transform( AstMapping *map, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     PermMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a PermMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required coordinate
*     permutation.

*  Parameters:
*     map
*        Pointer to the PermMap.
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
*     match the number of coordinates for the PermMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPermMap *this;             /* Pointer to PermMap to be applied */
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double constant;              /* Constant coordinate value */
   int *perm;                    /* Pointer to permutation array */
   int coord;                    /* Loop counter for coordinates */
   int maxperm;                  /* Max value in permutation array */
   int ncoord_in;                /* Number of coordinates per input point */
   int ncoord_out;               /* Number of coordinates per output point */
   int npoint;                   /* Number of points */
   int p;                        /* Permuted coordinate index */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the PermMap. */
   this = (AstPermMap *) map;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( map, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   permutation needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   and output PointSets and obtain pointers for accessing the input and output
   coordinate values. */
   ncoord_in = astGetNcoord( in );
   ncoord_out = astGetNcoord( result );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Obtain a pointer to the appropriate coordinate permutation array, according
   to the direction of transformation required and whether the PermMap has
   been inverted. Also get the maximum allowed value in the permutation array. */
   if ( ( astGetInvert( this ) != 0 ) == ( forward != 0 ) ) {
      perm = this->inperm;
      maxperm = ncoord_out;
   } else {
      perm = this->outperm;
      maxperm = ncoord_in;
   }

/* Perform coordinate permutation. */
/* ------------------------------- */
   if ( astOK ) {

/* Loop to generate values for each output coordinate. */
      for ( coord = 0; coord < ncoord_out; coord++ ) {

/* If the permutation array is not NULL, use it to look up which input
   coordinate to use. Otherwise, use the corresponding input coordinate. */
         p = PERMVAL( perm, coord, maxperm );

/* If a valid input coordinate has been identified, simply copy the required
   coordinate values from input to output. */
         if ( ( p >= 0 ) && ( p < ncoord_in ) ) {
            (void) memcpy( ptr_out[ coord ], ptr_in[ p ],
                           sizeof( double ) * (size_t) npoint );

/* If the permuted coordinate index is negative, use it to index the "constant"
   array to obtain a constant value to assign. If this array is NULL, use
   AST__BAD as the constant. */
	 } else if ( p < 0 ) {
            constant = this->constant ? this->constant[ (-p) - 1 ] : AST__BAD;

/* Assign the constant value to the output coordinate for all points. */
            for ( point = 0; point < npoint; point++ ) {
               ptr_out[ coord ][ point ] = constant;
	    }

/* In all other cases, simply assign the value AST__BAD to the output
   coordinate for all points. */
	 } else {
            for ( point = 0; point < npoint; point++ ) {
               ptr_out[ coord ][ point ] = AST__BAD;
	    }
	 }
      }
   }

/* Return a pointer to the output PointSet. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/*
*att+
*  Name:
*     PermSplit

*  Purpose:
*     The method to use when splitting a PermMap using astMapSplit.

*  Type:
*     Protected attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute controls the behaviour of the implementation of the
*     astMapSplit method provided by the PermMap class. If set to zero (the
*     default), astMapSplit will split PermMaps according to the public
*     documentation for the method. If set non-zero, the forward transformation
*     of the PermMap defined by the "outperm" array will be ignored.
*     Instead, the forward transformation is assumed to be the exact
*     inverse of the inverse transformation. The Mapping returned will
*     then be a UnitMap with Nin equal to the number of picked inputs,
*     and the returned array of output indices will hold the "inperm"
*     values for the picked inputs. Note, if any of these "inperm" values
*     are negative (indicating that the inverse transformation supplies a
*     constant value for the input), or if more than one of the selected
*     inputs are fed (by the inverse transformation) by the same output,
*     then the PermMap cannot be split.

*  Applicability:
*     PermMap
*        All PermMaps have this attribute.
*att-
*/
astMAKE_CLEAR(PermMap,PermSplit,permsplit,-INT_MAX)
astMAKE_GET(PermMap,PermSplit,int,0,( this->permsplit != -INT_MAX ?
                                      this->permsplit : 0 ))
astMAKE_SET(PermMap,PermSplit,int,permsplit,( value != 0 ))
astMAKE_TEST(PermMap,PermSplit,( this->permsplit != -INT_MAX ))


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for PermMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for PermMap objects.

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
   AstPermMap *in;               /* Pointer to input PermMap */
   AstPermMap *out;              /* Pointer to output PermMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output PermMaps. */
   in = (AstPermMap *) objin;
   out = (AstPermMap *) objout;

/* For safety, first clear any references to the input memory from
   the output PermMap. */
   out->inperm = NULL;
   out->outperm = NULL;
   out->constant = NULL;

/* For each input array which is not NULL, make a copy in allocated memory,
   storing a pointer to it in the output PermMap structure. */
   if ( in->inperm ) out->inperm = astStore( NULL, in->inperm,
                                             astSizeOf( in->inperm ) );
   if ( in->outperm ) out->outperm = astStore( NULL, in->outperm,
                                               astSizeOf( in->outperm ) );
   if ( in->constant ) out->constant = astStore( NULL, in->constant,
                                                 astSizeOf( in->constant ) );

/* If an error occurred, clean up by freeing all memory allocated above. */
   if ( !astOK ) {
      out->inperm = astFree( out->inperm );
      out->outperm = astFree( out->outperm );
      out->constant = astFree( out->constant );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for PermMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for PermMap objects.

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
   AstPermMap *this;             /* Pointer to PermMap */

/* Obtain a pointer to the PermMap structure. */
   this = (AstPermMap *) obj;

/* Free all memory allocated by the PermMap. */
   this->inperm = astFree( this->inperm );
   this->outperm = astFree( this->outperm );
   this->constant = astFree( this->constant );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for PermMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the PermMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the PermMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 150          /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstPermMap *this;             /* Pointer to the PermMap structure */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment strings */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword strings */
   int coord;                    /* Loop counter for coordinates */
   int iconst;                   /* Loop counter for constants */
   int invert;                   /* Invert attribute value */
   int ival;                     /* Integer value */
   int nconst;                   /* Number of constants */
   int nin;                      /* Number of input coordinates */
   int nout;                     /* Number of output coordinates */
   int set;                      /* Value is "set"? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PermMap structure. */
   this = (AstPermMap *) this_object;

/* Determine if the PermMap is inverted and obtain the "true" number
   of input and output coordinates by un-doing the effects of any
   inversion. */
   invert = astGetInvert( this );
   nin = !invert ? astGetNin( this ) : astGetNout( this );
   nout = !invert ? astGetNout( this ) : astGetNin( this );

/* Initialise the count of constants in use. */
   nconst = 0;

/* Write out values representing the instance variables for the
   PermMap class.  Accompany these with appropriate comment strings,
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

/* PermSplit */
/* --------- */
   set = TestPermSplit( this, status );
   ival = set ? GetPermSplit( this, status ) : astGetPermSplit( this );
   astWriteInt( channel, "pmsplt", set, 0, ival,
                ival ? "Use alternative astMapSplit implementation" :
                       "Use normal astMapSplit implementation" );

/* "outperm" array contents. */
/* ------------------------- */
/* Write the boolean "OutCpy" value to indicate if output coordinates
   are obtained simply by copying corresponding input
   coordinates. This will be the case if "this->outperm" is NULL. */
   ival = this->outperm ? 0 : 1;
   set = ( ival != 0 );
   astWriteInt( channel, "OutCpy", set, 0, ival,
                ival ? "Output coordinates = input coordinates" :
                       "Output coordinates specified individually" );

/* If output coordinates are specified individually, create a keyword
   for each element of the "outperm" array. */
   if ( this->outperm ) {
      for ( coord = 0; coord < nout; coord++ ) {
         (void) sprintf( key, "Out%d", coord + 1 );

/* Obtain the array value. If it refers to a coordinate that does not
   exist, change the value to zero (indicating a "bad" value for this
   coordinate). Create an appropriate comment. */
         ival = this->outperm[ coord ];
         if ( ival >= nin ) {
            ival = 0;
            (void) sprintf( comment, "Output coordinate %d is \"bad\"",
                            coord + 1 );

/* If the coordinate reference is valid, convert to 1-based coordinate
   numbering and create an appropriate comment. */
         } else if ( ival >= 0 ) {
            ival++;
            (void) sprintf( comment,
                            "Output coordinate %d = input coordinate %d",
                            coord + 1, ival );

/* If the reference is to a constant, create an appropriate comment
   (which depends on whether there are any constants). */
         } else {
            if ( this->constant ) {
               (void) sprintf( comment,
                               "Output coordinate %d = constant no. %d",
                               coord + 1, -ival );
            } else {
               (void) sprintf( comment, "Output coordinate %d is \"bad\"",
                               coord + 1 );
            }

/* Update the top constant number referenced. */
            if ( nconst < -ival ) nconst = -ival;
         }

/* Write out the array value with accompanying comment. */
         astWriteInt( channel, key, 1, 1, ival, comment );
      }
   }

/* "inperm" array contents. */
/* ------------------------ */
/* Write the boolean "InCpy" value to indicate if input coordinates
   are obtained simply by copying corresponding output
   coordinates. This will be the case if "this->inperm" is NULL. */
   ival = this->inperm ? 0 : 1;
   set = ( ival != 0 );
   astWriteInt( channel, "InCpy", set, 0, ival,
                ival ? "Input coordinates = output coordinates" :
                       "Input coordinates specified individually" );

/* If input coordinates are specified individually, create a keyword
   for each element of the "inperm" array. */
   if ( this->inperm ) {
      for ( coord = 0; coord < nin; coord++ ) {
         (void) sprintf( key, "In%d", coord + 1 );

/* Obtain the array value. If it refers to a coordinate that does not
   exist, change the value to zero (indicating a "bad" value for this
   coordinate). Create an appropriate comment. */
         ival = this->inperm[ coord ];
         if ( ival >= nout ) {
            ival = 0;
            (void) sprintf( comment, "Input coordinate %d is \"bad\"",
                            coord + 1 );

/* If the coordinate reference is valid, convert to 1-based coordinate
   numbering and create an appropriate comment. */
         } else if ( ival >= 0 ) {
            ival++;
            (void) sprintf( comment,
                            "Input coordinate %d = output coordinate %d",
                            coord + 1, ival );

/* If the reference is to a constant, create an appropriate comment
   (which depends on whether there are any constants). */
         } else {
            if ( this->constant ) {
               (void) sprintf( comment,
                               "Input coordinate %d = constant no. %d",
                               coord + 1, -ival );
            } else {
               (void) sprintf( comment, "Input coordinate %d is \"bad\"",
                               coord + 1 );
            }

/* Update the top constant number referenced. */
            if ( nconst < -ival ) nconst = -ival;
         }

/* Write out the array value with accompanying comment. */
         astWriteInt( channel, key, 1, 1, ival, comment );
      }
   }

/* Number of constants. */
/* -------------------- */
/* First check if there are any constants, then write out how many
   there are. */
   if ( !this->constant ) nconst = 0;
   set = ( nconst != 0 );
   astWriteInt( channel, "Nconst", set, 0, nconst, "Number of constants" );

/* Constants. */
/* ---------- */
/* Loop to create a keyword and comment for each constant. */
   for ( iconst = 0; iconst < nconst; iconst++ ) {
      (void) sprintf( key, "Con%d", iconst + 1 );
      (void) sprintf( comment, "Constant number %d", iconst + 1 );

/* Write out each constant value and comment. */
      set = ( this->constant[ iconst ] != AST__BAD );
      if ( set ) {
         astWriteDouble( channel, key, 1, 1, this->constant[ iconst ],
                         comment );
      } else {
         astWriteString( channel, key, 0, 1, "<bad>", comment );
      }
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPermMap and astCheckPermMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(PermMap,Mapping)
astMAKE_CHECK(PermMap)

AstPermMap *astPermMap_( int nin, const int inperm[], int nout,
                         const int outperm[], const double constant[],
                         const char *options, int *status, ...) {
/*
*++
*  Name:
c     astPermMap
f     AST_PERMMAP

*  Purpose:
*     Create a PermMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "permmap.h"
c     AstPermMap *astPermMap( int nin, const int inperm[], int nout,
c                             const int outperm[], double constant[],
c                             const char *options, ... )
f     RESULT = AST_PERMMAP( NIN, INPERM, NOUT, OUTPERM, CONSTANT, OPTIONS,
f                           STATUS )

*  Class Membership:
*     PermMap constructor.

*  Description:
*     This function creates a new PermMap and optionally initialises its
*     attributes.
*
*     A PermMap is a Mapping which permutes the order of coordinates,
*     and possibly also changes the number of coordinates, between its
*     input and output.
*
*     In addition to permuting the coordinate order, a PermMap may
*     also assign constant values to coordinates. This is useful when
*     the number of coordinates is being increased as it allows fixed
*     values to be assigned to any new ones.

*  Parameters:
c     nin
f     NIN = INTEGER (Given)
*        The number of input coordinates.
c     inperm
f     INPERM = INTEGER( NIN ) (Given)
c        An optional array with "nin" elements which, for each input
f        An array which, for each input
*        coordinate, should contain the number of the output
*        coordinate whose value is to be used (note that this array
*        therefore defines the inverse coordinate transformation).
*        Coordinates are numbered starting from 1.
*
*        For details of additional special values that may be used in
c        this array, see the description of the "constant" parameter.
f        this array, see the description of the CONSTANT argument.
c
c        If a NULL pointer is supplied instead of an array, each input
c        coordinate will obtain its value from the corresponding
c        output coordinate (or will be assigned the value AST__BAD if
c        there is no corresponding output coordinate).
c     nout
f     NOUT = INTEGER (Given)
*        The number of output coordinates.
c     outperm
f     OUTPERM = INTEGER( NOUT ) (Given)
c        An optional array with "nout" elements which, for each output
f        An array which, for each output
*        coordinate, should contain the number of the input coordinate
*        whose value is to be used (note that this array therefore
*        defines the forward coordinate transformation).  Coordinates
*        are numbered starting from 1.
*
*        For details of additional special values that may be used in
c        this array, see the description of the "constant" parameter.
f        this array, see the description of the CONSTANT argument.
c
c        If a NULL pointer is supplied instead of an array, each output
c        coordinate will obtain its value from the corresponding
c        input coordinate (or will be assigned the value AST__BAD if
c        there is no corresponding input coordinate).
c     constant
f     CONSTANT = DOUBLE PRECISION( * ) (Given)
c        An optional array containing values which may be assigned to
f        An array containing values which may be assigned to
*        input and/or output coordinates instead of deriving them
c        from other coordinate values. If either of the "inperm" or
f        from other coordinate values. If either of the INPERM or
c        "outperm" arrays contains a negative value, it is used to
f        OUTPERM arrays contains a negative value, it is used to
c        address this "constant" array (such that -1 addresses the
f        address this CONSTANT array (such that -1 addresses the
*        first element, -2 addresses the second element, etc.) and the
*        value obtained is used as the corresponding coordinate value.
*
*        Care should be taken to ensure that locations lying outside
*        the extent of this array are not accidentally addressed. The
c        array is not used if the "inperm" and "outperm" arrays do not
f        array is not used if the INPERM and OUTPERM arrays do not
*        contain negative values.
c
c        If a NULL pointer is supplied instead of an array, the
c        behaviour is as if the array were of infinite length and
c        filled with the value AST__BAD.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new PermMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new PermMap. The syntax used is identical to that for the
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
c     astPermMap()
f     AST_PERMMAP = INTEGER
*        A pointer to the new PermMap.

*  Notes:
c     - If either of the "inperm" or "outperm" arrays contains a
f     - If either of the INPERM or OUTPERM arrays contains a
*     zero value (or a positive value which does not identify a valid
*     output/input coordinate, as appropriate), then the value
*     AST__BAD is assigned as the new coordinate value.
*     - This function does not attempt to ensure that the forward and
*     inverse transformations performed by the PermMap are
*     self-consistent in any way. You are therefore free to supply
*     coordinate permutation arrays that achieve whatever effect is
*     desired.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPermMap *new;              /* Pointer to new PermMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PermMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPermMap( NULL, sizeof( AstPermMap ), !class_init, &class_vtab,
                         "PermMap", nin, inperm, nout, outperm, constant );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PermMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PermMap. */
   return new;
}

AstPermMap *astPermMapId_( int nin, const int inperm[], int nout,
                           const int outperm[], const double constant[],
                           const char *options, ... ) {
/*
*  Name:
*     astPermMapId_

*  Purpose:
*     Create a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "permmap.h"
*     AstPermMap *astPermMapId_( int nin, const int inperm[], int nout,
*                                const int outperm[], const double constant[],
*                                const char *options, ... )

*  Class Membership:
*     PermMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astPermMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astPermMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     This function also converts the coordinate numbering in the
*     permutation arrays from 1-based (used externally) to zero-based
*     (used internally).
*
*     The variable argument list also prevents this function from
*     invoking astPermMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astPermMap_.

*  Returned Value:
*     The ID value associated with the new PermMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPermMap *new;              /* Pointer to new PermMap */
   int *inperm1;                 /* Pointer to temporary copy of "inperm" */
   int *outperm1;                /* Pointer to temporary copy of "outperm" */
   int coord;                    /* Loop counter for coordinates */

/* Variable argument list */
   va_list args;                 /* Get a pointer to the thread specific global data structure. */

   int *status;                  /* Pointer to inherited status value */

   astGET_GLOBALS(NULL);

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If the "nin" and "nout" values are acceptable, allocate memory to
   hold temporary copies of the "inperm" and "outperm" arrays (but
   only if these arrays are not NULL). */
   inperm1 = NULL;
   outperm1 = NULL;
   if ( ( nin >= 0 ) && ( nout >= 0 ) ) {
      if ( inperm ) inperm1 = astMalloc( sizeof( int ) * (size_t) nin );
      if ( outperm ) outperm1 = astMalloc( sizeof( int ) * (size_t) nout );
      if ( astOK ) {

/* If necessary, make a copy of the "inperm" array, converting any
   zero values into (zero-based) coordinate numbers that do not exist,
   indicating a "bad" coordinate value. */
         if ( inperm ) {
            for ( coord = 0; coord < nin; coord++ ) {
               if ( inperm[ coord ] < 0 ) {
                  inperm1[ coord ] = inperm[ coord ];
               } else if ( inperm[ coord ] == 0 ) {
                  inperm1[ coord ] = nout;

/* Convert valid coordinate references from 1-based (used externally)
   to zero-based (used internally). */
               } else {
                  inperm1[ coord ] = inperm[ coord ] - 1;
               }
            }
         }

/* Repeat this process on the "outperm" array. */
         if ( outperm ) {
            for ( coord = 0; coord < nout; coord++ ) {
               if ( outperm[ coord ] < 0 ) {
                  outperm1[ coord ] = outperm[ coord ];
               } else if ( outperm[ coord ] == 0 ) {
                  outperm1[ coord ] = nin;
               } else {
                  outperm1[ coord ] = outperm[ coord ] - 1;
               }
            }
         }
      }
   }

/* Initialise the PermMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPermMap( NULL, sizeof( AstPermMap ), !class_init, &class_vtab,
                         "PermMap", nin, inperm1, nout, outperm1, constant );

/* If necessary, free the temporary arrays allocated above. */
   if ( ( nin >= 0 ) && ( nout >= 0 ) ) {
      if ( inperm ) inperm1 = astFree( inperm1 );
      if ( outperm ) outperm1 = astFree( outperm1 );
   }

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PermMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new PermMap. */
   return astMakeId( new );
}

AstPermMap *astInitPermMap_( void *mem, size_t size, int init,
                             AstPermMapVtab *vtab, const char *name,
                             int nin, const int inperm[],
                             int nout, const int outperm[],
                             const double constant[], int *status ) {
/*
*+
*  Name:
*     astInitPermMap

*  Purpose:
*     Initialise a PermMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "permmap.h"
*     AstPermMap *astInitPermMap( void *mem, size_t size, int init,
*                                 AstPermMapVtab *vtab, const char *name,
*                                 int nin, const int inperm[],
*                                 int nout, const int outperm[],
*                                 const double constant[] )

*  Class Membership:
*     PermMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new PermMap object. It allocates memory (if necessary) to accommodate
*     the PermMap plus any additional data associated with the derived class.
*     It then initialises a PermMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a PermMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the PermMap is to be initialised.
*        This must be of sufficient size to accommodate the PermMap data
*        (sizeof(PermMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the PermMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the PermMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the PermMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new PermMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     nin
*        The number of input coordinate values per point.
*     inperm
*        Pointer to an array of int, with nin elements. For each input
*        coordinate, the corresponding element of this array should contain the
*        (zero-based) index of the output coordinate whose value is to be used.
*        (Note that this array therefore defines the inverse coordinate
*        transformation.) If a NULL value is supplied, the corresponding output
*        coordinate value is used (or AST__BAD if there is no corresponding
*        output coordinate).
*
*        For details of additional special values that may be used in this
*        array, see the description of the "constant" parameter.
*     nout
*        The number of output coordinate values per point.
*     outperm
*        Pointer to an array of int, with nout elements. For each output
*        coordinate, the corresponding element of this array should contain the
*        (zero-based) index of the input coordinate whose value is to be used.
*        (Note that this array therefore defines the forward coordinate
*        transformation.) If a NULL value is supplied, the corresponding input
*        coordinate value is used (or AST__BAD if there is no corresponding
*        input coordinate).
*
*        For details of additional special values that may be used in this
*        array, see the description of the "constant" parameter.
*     constant
*        Pointer to an array of double, which contains optional values which
*        may be assigned to input and/or output coordinate values (instead of
*        deriving them from other coordinate values). If either of the
*        "inperm" or "outperm" arrays contains a negative value, it is used to
*        address this "constant" array (such that -1 addresses the first
*        element, -2 addresses the second element, etc.) and the value obtained
*        is used as the corresponding coordinate value. Care should be taken
*        to ensure that locations lying outside the extent of this array are
*        not accidentally addressed.
*
*        If a NULL value is supplied for this parameter, the behaviour is as
*        if the constant array were of infinite length and filled with the
*        value AST__BAD.

*  Returned Value:
*     A pointer to the new PermMap.

*  Notes:
*     -  This function does not attempt to ensure that the forward and inverse
*     transformations performed by the resulting PermMap are consistent in any
*     way. The caller is therefore free to define the permutation arrays to
*     achieve whatever effect is desired.
*     -  If either of the "inperm" or "outperm" arrays contains a positive
*     value which does not identify a valid output/input coordinate (as
*     appropriate), then the value AST__BAD is assigned as the new coordinate
*     value.
*     -  This function makes a copy of the contents of the arrays supplied.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPermMap *new;              /* Pointer to new PermMap */
   int i;                        /* Loop counter for coordinates */
   int neg;                      /* Most negative permutation index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPermMapVtab( vtab, name );

/* Initialise a Mapping structure (the parent class) as the first component
   within the PermMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstPermMap *) astInitMapping( mem, size, 0,
                                        (AstMappingVtab *) vtab, name,
                                        nin, nout, 1, 1 );

   if ( astOK ) {

/* Initialise the PermMap data. */
/* ---------------------------- */
      new->permsplit = -INT_MAX;

/* Initialise the array pointers. */
      new->inperm = NULL;
      new->outperm = NULL;
      new->constant = NULL;

/* If an "inperm" and/or "outperm" array has been supplied, allocate memory
   and store a copy. */
      if ( inperm ) new->inperm = astStore( NULL, inperm, sizeof( int ) *
                                                          (size_t) nin );
      if ( outperm ) new->outperm = astStore( NULL, outperm, sizeof( int ) *
                                                             (size_t) nout );

/* If a "constant" array has been supplied, we must also store a copy of it,
   but must first determine how many of its elements we need. */
      if ( constant ) {

/* Loop through the "inperm" array (if supplied) to find the most negative
   value it contains. This corresponds with the maximum index into the
   constant array. */
         neg = 0;
         if ( inperm ) {
            for ( i = 0; i < nin; i++ ) {
               if ( inperm[ i ] < neg ) neg = inperm[ i ];
	    }
	 }

/* Also perform this process on the "outperm" array (if supplied). */
         if ( outperm ) {
            for ( i = 0; i < nout; i++ ) {
               if ( outperm[ i ] < neg ) neg = outperm[ i ];
	    }
	 }

/* If a negative value was found, use its size to determine how many elements
   of the "constant" array to store in allocated memory. */
         if ( neg < 0 ) {
            new->constant = astStore( NULL, constant, sizeof( double ) *
                                                      (size_t) (-neg) );
	 }
      }

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) {
         new = astDelete( new );
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstPermMap *astLoadPermMap_( void *mem, size_t size,
                             AstPermMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPermMap

*  Purpose:
*     Load a PermMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "permmap.h"
*     AstPermMap *astLoadPermMap( void *mem, size_t size,
*                                 AstPermMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     PermMap loader.

*  Description:
*     This function is provided to load a new PermMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     PermMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a PermMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the PermMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        PermMap data (sizeof(PermMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the PermMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the PermMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPermMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new PermMap. If this is NULL, a pointer
*        to the (static) virtual function table for the PermMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "PermMap" is used instead.

*  Returned Value:
*     A pointer to the new PermMap.

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
   AstPermMap *new;              /* Pointer to the new PermMap */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword strings */
   int coord;                    /* Loop counter for coordinates */
   int iconst;                   /* Loop counter for constants */
   int in_cpy;                   /* Input coordinates obtained by copying? */
   int invert;                   /* Invert attribute value */
   int ival;                     /* Integer value */
   int nconst;                   /* Number of constants */
   int nin;                      /* Number of input coordinates */
   int nout;                     /* Number of output coordinates */
   int out_cpy;                  /* Output coordinates obtained by copying? */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this PermMap. In this case the
   PermMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPermMap );
      vtab = &class_vtab;
      name = "PermMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPermMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built PermMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "PermMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Initialise the PermMap's pointers. */
      new->inperm = NULL;
      new->outperm = NULL;
      new->constant = NULL;

/* Determine if the PermMap is inverted and obtain the "true" number
   of input and output coordinates by un-doing the effects of any
   inversion. */
      invert = astGetInvert( new );
      nin = !invert ? astGetNin( new ) : astGetNout( new );
      nout = !invert ? astGetNout( new ) : astGetNin( new );

/* PermSplit. */
/* ---------- */
      new->permsplit = astReadInt( channel, "pmsplt", -INT_MAX );
      if ( TestPermSplit( new, status ) ) SetPermSplit( new, new->permsplit, status );

/* InCpy and OutCpy. */
/* ----------------- */
/* Obtain boolean values via the "InCpy" and "OutCpy" keywords which
   indicate if input/output coordinates should be obtained simply by
   copying the corresponding output/input coordinates. */
      in_cpy = astReadInt( channel, "incpy", 0 );
      out_cpy = astReadInt( channel, "outcpy", 0 );

/* If coordinates are specified individually (not simply copied), then
   allocate memory for the coordinate permutation arrays. */
      if ( !in_cpy ) new->inperm = astMalloc( sizeof( int ) * (size_t) nin );
      if ( !out_cpy ) new->outperm = astMalloc( sizeof( int ) *
                                                (size_t) nout );

/* If an error occurred, ensure that all allocated memory is freed. */
      if ( !astOK ) {
         if ( !in_cpy ) new->inperm = astFree( new->inperm );
         if ( !out_cpy ) new->outperm = astFree( new->outperm );

/* Otherwise read data into these arrays... */
      } else {

/* "inperm" array contents. */
/* ------------------------ */
/* If required, create a keyword for each element of the "inperm"
   array and read the element's value. */
         if ( !in_cpy ) {
            for ( coord = 0; coord < nin; coord++ ) {
               (void) sprintf( key, "in%d", coord + 1 );
               ival = astReadInt( channel, key, 0 );

/* If the value is zero (indicating a "bad" coordinate), convert it to
   a (zero-based) coordinate number that doesn't exist. */
               if ( ival == 0 ) {
                  ival = nout;

/* If the coordinate reference is valid, convert to zero-based
   coordinate numbering for internal use. */
               } else if ( ival > 0 ) {
                  ival--;
               }

/* Store the value. */
               new->inperm[ coord ] = ival;
            }
         }

/* "outperm" array contents. */
/* ------------------------- */
/* If required, create a keyword for each element of the "outperm"
   array and read the element's value. */
         if ( !out_cpy ) {
            for ( coord = 0; coord < nout; coord++ ) {
               (void) sprintf( key, "out%d", coord + 1 );
               ival = astReadInt( channel, key, 0 );

/* If the value is zero (indicating a "bad" coordinate), convert it to
   a (zero-based) coordinate number that doesn't exist. */
               if ( ival == 0 ) {
                  ival = nin;

/* If the coordinate reference is valid, convert to zero-based
   coordinate numbering for internal use. */
               } else if ( ival > 0 ) {
                  ival--;
               }

/* Store the value. */
               new->outperm[ coord ] = ival;
            }
         }

/* Number of constants. */
/* -------------------- */
/* Determine the number of constants and allocate memory to hold
   them. */
         nconst = astReadInt( channel, "nconst", 0 );
         if ( nconst < 0 ) nconst = 0;
         new->constant = astMalloc( sizeof( double ) * (size_t) nconst );
         if ( astOK ) {

/* Constants. */
/* ---------- */
/* Create a keyword for each constant and read its value. */
            for ( iconst = 0; iconst < nconst; iconst++ ) {
               (void) sprintf( key, "con%d", iconst + 1 );
               new->constant[ iconst ] =
                  astReadDouble( channel, key, AST__BAD );
            }
         }
      }

/* If an error occurred, clean up by deleting the new PermMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new PermMap pointer. */
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

double *astGetConstants_( AstPermMap *this, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,PermMap,GetConstants))( this, status );
}

int *astGetInPerm_( AstPermMap *this, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,PermMap,GetInPerm))( this, status );
}

int *astGetOutPerm_( AstPermMap *this, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,PermMap,GetOutPerm))( this, status );
}






