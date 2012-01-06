/*
*class++

*  Name:
*     Object

*  Purpose:
*     Base class for all AST Objects.

*  Constructor Function:
*     None.

*  Description:
*     This class is the base class from which all other classes in the
*     AST library are derived. It provides all the basic Object
*     behaviour and Object manipulation facilities required throughout
*     the library. There is no Object constructor, however, as Objects
*     on their own are not useful.

*  Inheritance:
*     The Object base class does not inherit from any other class.

*  Attributes:
*     All Objects have the following attributes:
*
*     - Class: Object class name
*     - ID: Object identification string
*     - Ident: Permanent Object identification string
*     - Nobject: Number of Objects in class
*     - ObjSize: The in-memory size of the Object in bytes
*     - RefCount: Count of active Object pointers
*     - UseDefs: Allow use of default values for Object attributes?

*  Functions:
c     The following functions may be applied to all Objects:
f     The following routines may be applied to all Objects:
*
c     - astAnnul: Annul a pointer to an Object
c     - astBegin: Begin a new AST context
c     - astClear: Clear attribute values for an Object
c     - astClone: Clone a pointer to an Object
c     - astCopy: Copy an Object
c     - astDelete: Delete an Object
c     - astEnd: End an AST context
c     - astEscapes: Control whether graphical escape sequences are removed
c     - astExempt: Exempt an Object pointer from AST context handling
c     - astExport: Export an Object pointer to an outer context
c     - astGet<X>: Get an attribute value for an Object
c     - astHasAttribute: Test if an Object has a named attribute
c     - astImport: Import an Object pointer to the current context
c     - astIsA<Class>: Test class membership
c     - astLock: Lock an Object for use by the calling thread
c     - astToString: Create an in-memory serialisation of an Object
c     - astSame: Do two AST pointers refer to the same Object?
c     - astSet: Set attribute values for an Object
c     - astSet<X>: Set an attribute value for an Object
c     - astShow: Display a textual representation of an Object on standard
c     output
c     - astTest: Test if an attribute value is set for an Object
c     - astTune: Set or get an integer AST tuning parameter
c     - astTuneC: Set or get a character AST tuning parameter
c     - astUnlock: Unlock an Object for use by other threads
c     - astFromString: Re-create an Object from an in-memory serialisation
c     - astVersion: Return the verson of the AST library being used.
f     - AST_ANNUL: Annul a pointer to an Object
f     - AST_BEGIN: Begin a new AST context
f     - AST_CLEAR: Clear attribute values for an Object
f     - AST_CLONE: Clone a pointer to an Object
f     - AST_COPY: Copy an Object
f     - AST_DELETE: Delete an Object
f     - AST_END: End an AST context
f     - AST_ESCAPES: Control whether graphical escape sequences are removed
f     - AST_EXEMPT: Exempt an Object pointer from AST context handling
f     - AST_EXPORT: Export an Object pointer to an outer context
f     - AST_GET<X>: Get an attribute value for an Object
f     - AST_HASATTRIBUTE: Test if an Object has a named attribute
f     - AST_IMPORT: Import an Object pointer to the current context
f     - AST_ISA<CLASS>: Test class membership
f     - AST_SAME: Do two AST pointers refer to the same Object?
f     - AST_SET: Set attribute values for an Object
f     - AST_SET<X>: Set an attribute value for an Object
f     - AST_SHOW: Display a textual representation of an Object on standard
f     output
f     - AST_TEST: Test if an attribute value is set for an Object
f     - AST_TUNE: Set or get an integer AST tuning parameter
f     - AST_TUNEC: Set or get a character AST tuning parameter
f     - AST_VERSION: Return the verson of the AST library being used.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     1-FEB-1996 (RFWS):
*        Original version.
*     22-APR-1996 (RFWS):
*        Added attribute setting functions.
*     2-JUL-1996 (RFWS):
*        Added functions to support the external interface (using
*        identfiers).
*     10-SEP-1996 (RFWS):
*        Added I/O facilities.
*     30-MAY-1997 (RFWS):
*        Add ID attribute.
*     14-JUL-1997 (RFWS):
*        Add astExempt function.
*     14-OCT-1997 (RFWS):
*        Fixed uninitialised use of "dynamic" in astCopy_.
*     14-NOV-1997 (RFWS):
*        Remove the subversive C "strtok" function.
*     20-JAN-1998 (RFWS):
*        Make the astClear and astRVSet methods virtual.
*     29-APR-1998 (RFWS):
*        Fixed bug in algorithm for encoding Object IDs.
*     15-SEP-1999 (RFWS)
*        Made astAnnulId accessible from protected code.
*     12-APR-2000 (DSB):
*        Zero all memory allocated for a new Object in InitObject before
*        storing any new values in the memory.
*     3-APR-2001 (DSB):
*        Added the Ident attribute.
*     28-SEP-2001 (DSB):
*        Modified VSet to ensure a non-null string always follows the equal
*        sign in the attribute setting passed to SetAttrib.
*     27-NOV-2002 (DSB):
*        Modified astShow to use astWrite instead of astDump, so that
*        invocations of astShow will be included in the count of the
*        number of invocations of astWrite returned by astWriteInvocations.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitObjectVtab
*        method.
*     8-FEB-2004 (DSB):
*        Added astEscapes.
*     10-FEB-2004 (DSB):
*        Added debug conditional code to keep track of memory leaks.
*     22-AUG-2004 (DSB):
*        Added astEqual
*     27-JAN-2005 (DSB):
*        Correct use of ->ident pointers, and added further DEBUG blocks.
*     11-MAR-2005 (DSB):
*        Added attribute UseDefs.
*     14-FEB-2006 (DSB):
*        Added attribute ObjSize.
*     23-FEB-2006 (DSB):
*        Added MemoryCaching tuning parameter.
*     27-FEB-2006 (DSB):
*        Include Objects returned by astCopy in the ObjectCaching system.
*     28-FEB-2006 (DSB):
*        Use astOK to protect against errors within astGrow.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     26-MAY-2006 (DSB):
*        Correct handling of commas within the attribute value supplied
*        to astSetC.
*     30-MAY-2006 (DSB):
*        Correct the correction made to handle commas within attribute
*     6-JUN-2007 (DSB):
*        Fix harmless compiler warnings.
*     21-JUN-2007 (DSB):
*        In astSet<X>, ignore trailing spaces in the attribute name.
*     22-JUN-2007 (DSB):
*        - Make astVSet return a pointer to dynamic memory holding the
*        expanded setting string.
*        - Add astSetVtab, and astCast.
*     27-JUN-2007 (DSB):
*        Modify astInitObject so that it ignores the supplied "size" value
*        if some memory is supplied.
*     2-JULY-2007 (DSB):
*        Fix memory access problem in VSet.
*     20-SEP-2007 (DSB):
*        In astDelete, ensure the error status is reset before calling
*        astGrow to extend the vtab free list.
*     22-APR-2008 (DSB):
*        Added astSame.
*     24-OCT-2008 (DSB):
*        Prevent a mutex deadlock that could occur when annulling an
*        Object ID.
*     28-JAN-2008 (DSB):
*        Allow unlocked objects to be annulled using astAnnul.
*     14-OCT-2009 (DSB):
*        Modify astCast to make it a virtual function and add type
*        checking.
*     7-APR-2010 (DSB):
*        Added method astHasAttribute.
*     24-AUG-2010 (DSB):
*        Allow commas to be included in attribute values supplied to
*        astSet or astVSet by putting quotes around the attribute value.
*     16-JUN-2011 (DSB):
*        Added component "iref" to the Object structure. This is an
*        integer identifier for each object that is unique within the
*        class of the object. Useful for debugging.
*     22-JUL-2011 (DSB):
*        Add methods astSetProxy and astGetProxy.
*     2-SEP-2011 (DSB):
*        Add functions astToString and astFromString
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Object

#define INVALID_CONTEXT -1   /* Context value for handles that have no
                                associated Object */
#define UNOWNED_CONTEXT -2   /* Context value for handles for objects
                                that are not locked by any thread */


/* Include files. */
/* ============== */

/* Configuration information */
/* ------------------------ */
#include "version.h"             /* Version numbers */
#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "channel.h"             /* I/O channels */
#include "object.h"              /* Interface definition for this class */
#include "plot.h"                /* Plot class (for astStripEscapes) */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

/* Type Definitions */
/* ================ */
/* Structure used to pass data between astToString/FromString and the
   corresponding Channel source and sink functions. */
typedef struct StringData {
   char *ptr;      /* Pointer to serialisation text */
   char *buff;     /* Pointer to a buffer for a single line of text */
   int len;        /* Current length of serialisation text */
} StringData;

/* Module Variables. */
/* ================= */

/* The following globals have the same values in all threads and so do
   not need to be in thread-specific data. */
/* ------------------------------------------------------------------ */

/* Character-valued tuning parameters. */
#define MAXLEN_TUNEC 200
static char hrdel[ MAXLEN_TUNEC ] = "%-%^50+%s70+h%+";
static char mndel[ MAXLEN_TUNEC ] = "%-%^50+%s70+m%+";
static char scdel[ MAXLEN_TUNEC ] = "%-%^50+%s70+s%+";
static char dgdel[ MAXLEN_TUNEC ] = "%-%^53+%s60+o%+";
static char amdel[ MAXLEN_TUNEC ] = "%-%^20+%s85+'%+";
static char asdel[ MAXLEN_TUNEC ] = "%-%^20+%s85+\"%+";
static char exdel[ MAXLEN_TUNEC ] = "10%-%^50+%s70+";

/* A pointer full of zeros. */
static AstObject *zero_ptr;

/* A flag which indicates what should happen when an AST Object is
   deleted. If this flag is non-zero, the memory used by the Object is
   not freed, but a pointer to it is placed on the end of a list of free
   memory chunk pointers so that the memory can be re-used if necessary
   avoiding the need to re-allocate memory with malloc (which is slow).
   A separate list of free memory chunks is kept for each class because
   each class object will require chunks of a different size. Pointers
   to these lists are stored in the virtual function table associated
   with each class. All memory on these lists is freed when object
   caching is switched off via the astTune function. */
static int object_caching = 0;

/* Set up global data access, mutexes, etc, needed for thread safety. */
#ifdef THREAD_SAFE

/* Define the initial values for the global data for this module. */
#define GLOBAL_inits \
   globals->Retain_Esc = 0; \
   globals->Context_Level = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->AstGetC_Init = 0; \
   globals->AstGetC_Istr = 0; \
   globals->Active_Handles = NULL; \
   globals->Class_Init = 0; \
   globals->Nvtab = 0; \
   globals->Known_Vtabs = NULL;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Object)

/* Define macros for accessing each item of thread specific global data. */
#define retain_esc  astGLOBAL(Object,Retain_Esc)
#define context_level  astGLOBAL(Object,Context_Level)
#define active_handles  astGLOBAL(Object,Active_Handles)
#define getattrib_buff  astGLOBAL(Object,GetAttrib_Buff)
#define astgetc_strings  astGLOBAL(Object,AstGetC_Strings)
#define astgetc_istr  astGLOBAL(Object,AstGetC_Istr)
#define astgetc_init  astGLOBAL(Object,AstGetC_Init)
#define class_init astGLOBAL(Object,Class_Init)
#define class_vtab astGLOBAL(Object,Class_Vtab)
#define nvtab astGLOBAL(Object,Nvtab)
#define known_vtabs astGLOBAL(Object,Known_Vtabs)

/* mutex1 is used to prevent tuning parameters being accessed by more
   than one thread at any one time.  */
static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX1 pthread_mutex_lock( &mutex1 );
#define UNLOCK_MUTEX1 pthread_mutex_unlock( &mutex1 );

/* mutex2 is used to prevent the global lists of object handles being
   accessed by more than one thread at any one time.  */
static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

/* Each Object contains two mutexes. The primary mutex (mutex1) is used
   to guard access to all aspects of the Object except for the "locker"
   and "ref_count" items. The secondary mutex (mutex2) is used to guard
   access to these two remaining items. We need this secondary mutex
   since the "locker" and "ref_count" items need to be accessable within
   a thread even if that thread has not locked the Object using astLock.
   Define macros for accessing these two mutexes. */
#define LOCK_PMUTEX(this) (pthread_mutex_lock(&((this)->mutex1)))
#define UNLOCK_PMUTEX(this) (pthread_mutex_unlock(&((this)->mutex1)))
#define LOCK_SMUTEX(this) (pthread_mutex_lock(&((this)->mutex2)))
#define UNLOCK_SMUTEX(this) (pthread_mutex_unlock(&((this)->mutex2)))





/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Define the class virtual function table and its initialisation flag as
   static variables. */
static int class_init = 0;       /* Virtual function table initialised? */
static AstObjectVtab class_vtab; /* Virtual function table */

/* A list of pointers to all the known class virtual function tables. */
static int nvtab = 0;
static AstObjectVtab **known_vtabs = NULL;

/* A flag which indicates if AST functions which return text strings
   should retain any graphical escape sequences (as interpreted by the
   Plot class). */
static int retain_esc = 0;

/* Context level (Begin/End/Exempt/Export) */
static int context_level = 0;

/* Array of list heads for each context (each list is a list of Handle
   structures). */
static int *active_handles = NULL;

/* String returned by GetAttrib. */
static char getattrib_buff[ AST__GETATTRIB_BUFF_LEN + 1 ] = "";

/* Pointers to string buffers returned by astGetC. */
static char *astgetc_strings[ AST__ASTGETC_MAX_STRINGS ];

/* Offset of next string in "AstGetC_Strings" */
static int astgetc_istr = 0;

/* "AstGetC_Strings" array initialised? */
static int astgetc_init = 0;

/* Null macros for mutex locking and unlocking */
#define LOCK_MUTEX1
#define UNLOCK_MUTEX1
#define LOCK_MUTEX2
#define UNLOCK_MUTEX2
#define LOCK_PMUTEX(this)
#define LOCK_SMUTEX(this)
#define UNLOCK_PMUTEX(this)
#define UNLOCK_SMUTEX(this)

#endif


/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstObject *Cast( AstObject *, AstObject *, int * );
static const char *GetID( AstObject *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetIdent( AstObject *, int * );
static const char *Get( AstObject *, const char *, int * );
static const char *FromStringSource( void );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static int HasAttribute( AstObject *, const char *, int * );
static int Same( AstObject *, AstObject *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestID( AstObject *, int * );
static int TestIdent( AstObject *, int * );
static unsigned long Magic( const AstObject *, size_t, int * );
static void CleanAttribs( AstObject *, int * );
static void Clear( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearIdent( AstObject *, int * );
static void ClearID( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void EmptyObjectCache( int * );
static void ToStringSink( const char * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetID( AstObject *, const char *, int * );
static void SetIdent( AstObject *, const char *, int * );
static void Show( AstObject *, int * );
static void VSet( AstObject *, const char *, char **, va_list, int * );

static int GetUseDefs( AstObject *, int * );
static int TestUseDefs( AstObject *, int * );
static void ClearUseDefs( AstObject *, int * );
static void SetUseDefs( AstObject *, int, int * );

#if defined(THREAD_SAFE)
static void ChangeThreadVtab( AstObject *, int * );
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */
AstObject *astAnnul_( AstObject *this, int *status ) {
/*
*++
*  Name:
c     astAnnul
f     AST_ANNUL

*  Purpose:
*     Annul a pointer to an Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     AstObject *astAnnul( AstObject *this )
f     CALL AST_ANNUL( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function annuls a pointer to an Object so that it is no
f     This routine annuls a pointer to an Object so that it is no
*     longer recognised as a valid pointer by the AST library. Any
*     resources associated with the pointer are released and made
*     available for re-use.
*
c     This function also decrements the Object's RefCount attribute by
f     This routine also decrements the Object's RefCount attribute by
*     one. If this attribute reaches zero (which happens when the last
*     pointer to the Object is annulled), then the Object is deleted.

*  Parameters:
c     this
c        The Object pointer to be annulled.
f     THIS = INTEGER (Given and Returned)
f        The Object pointer to be annulled. A null pointer value (AST__NULL)
f        is always returned.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

c  Returned Value:
c     astAnnul()
c        A null Object pointer (AST__NULL) is always returned.
c
*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - This function will attempt to annul the pointer even if the
c     Object is not currently locked by the calling thread (see astLock).
c     - This function attempts to execute even if the AST error
c     status is set
f     - This routine attempts to execute even if STATUS is set to an
f     error value
*     on entry, although no further error report will be
*     made if it subsequently fails under these circumstances. In
*     particular, it will fail if the pointer suppled is not valid,
*     but this will only be reported if the error status is clear on
*     entry.
*--
*/

/* Check the pointer to ensure it identifies a valid Object (this
   generates an error if it doesn't). */
   if ( !astIsAObject( this ) ) return NULL;

/* Get a lock on the object's secondary mutex. This mutex guards access
   to the "ref_count" and "locker" components of the AstObject structure. */
   LOCK_SMUTEX(this);

#ifdef MEM_DEBUG
   {   int rc;
       char buf[100];
       rc = this->ref_count;
       sprintf(buf,"annulled (refcnt: %d -> %d)", rc, rc-1 );
       astMemoryUse( this, buf );
   }
#endif

/* Decrement the Object's reference count. */
   --(this->ref_count);

/* Unlock the object's secondary mutex. */
   UNLOCK_SMUTEX(this);

/* Decrement the Object's reference count and delete the Object if
   necessary. */
   if ( !this->ref_count ) (void) astDelete( this );

/* Always return NULL. */
   return NULL;
}

static AstObject *Cast( AstObject *this, AstObject *obj, int *status ) {
/*
*+
*  Name:
*     astCast

*  Purpose:
*     Cast an Object into an instance of a sub-class.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astCast( AstObject *this, AstObject *obj )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a deep copy of an ancestral component of the
*     supplied object. The required class of the ancestral component is
*     specified by another object. Specifically, if "this" and "new" are
*     of the same class, a copy of "this" is returned. If "this" is an
*     instance of a subclass of "obj", then a copy of the component
*     of "this" that matches the class of "obj" is returned. Otherwise,
*     a NULL pointer is returned without error.

*  Parameters:
*     this
*        Pointer to the Object to be cast.
*     obj
*        Pointer to an Object that defines the class of the returned Object.
*        The returned Object will be of the same class as "obj".

*  Returned Value:
*     A pointer to the new Object. NULL if "this" is not a sub-class of
*     "obj", or if an error occurs.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstObject *new;
   int generation_gap;

/* Initialise */
   new = NULL;

/* Check inherited status */
   if( !astOK ) return new;

/* Check pointer have been supplied. */
   if( this && obj ) {

/* See how many steps up the class inheritance ladder it is from "this" to
   "obj". A positive value is returned if "this" is a sub-class of "obj".
   A negative value is returned if "obj" is a sub-class of "this". Zero
   is returned if they are of the same class. AST__COUSIN is returned if
   they share a common ancestor but are not on the same line of descent. */
      generation_gap = astClassCompare( astVTAB( this ), astVTAB( obj ) );

/* If the two objects are of the same class, just return a copy of
   "this". */
      if( generation_gap == 0 ) {
         new = astCopy( this );

/* If "this" is a subclass of "obj", return a deep copy of "this" cast
   into the class of "obj". */
      } else if( generation_gap != AST__COUSIN && generation_gap > 0 ) {
         new = astCastCopy( this, obj );

      }
   }

/* Return the new pointer. */
   return new;
}

AstObject *astCastCopy_( AstObject *this, AstObject *obj, int *status ) {
/*
*+
*  Name:
*     astCastCopy

*  Purpose:
*     Cast an Object into an instance of a sub-class, without type-checking.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astCastCopy( AstObject *this, AstObject *obj )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a deep copy of an ancestral component of the
*     supplied object. The required class of the ancestral component is
*     specified by another object. No checks are performed that "this" is
*     a sub-class of "obj".
*
*     It works by temporarily changing the vtab in "this" to be the same
*     as in "obj", and then doing a deep copy, and then re-instating the
*     original vtab.

*  Parameters:
*     this
*        Pointer to the Object to be cast.
*     obj
*        Pointer to an Object that defines the class of the returned Object.
*        The returned Object will be of the same class as "obj".

*  Returned Value:
*     A pointer to the new Object.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstObject *new;
   AstObjectVtab *this_vtab;
   size_t this_size;

/* Initialise */
   new = NULL;

/* Check inherited status */
   if( !astOK ) return new;

/* Check pointer have been supplied. */
   if( this && obj ) {

/* Save a pointer to the original virtual function tables for "this". */
      this_vtab = astVTAB( this );

/* Temporarily change the vtab of "this" to that of "obJ". */
      this->vtab = astVTAB( obj );

/* Temporarily change the size of "this" to be the size of "obj". */
      this_size = this->size;
      this->size = obj->size;

/* Now take a copy of the object (now considered to be an instance of the
   class specified by "obj"). */
      new = astCopy( this );

/* Re-instate the original Object vtab and size. */
      this->vtab = this_vtab;
      this->size = this_size;

/* The sub-clas to which "this" originally belonged may have extended the
   range of values allowed for one or more of the attributes inherited from
   the "obj" class. This means that the current attribute values stored
   in the returned object may be inappropriate for the class of "obj". An
   example is the System attribute defined by the Frame class, and extended
   by sub-classes of Frame. So we now call astCleanAttribs to ensure that
   any inappropriate attribute values are cleared in the returned object. */
      astCleanAttribs( new );
   }

/* Return the new pointer. */
   return new;
}

#if defined(THREAD_SAFE)
static void ChangeThreadVtab( AstObject *this, int *status ){
/*
*  Name:
*     ChangeThreadVtab

*  Purpose:
*     Modify an Object structure so that it refers to a vtab created by
*     the currently executing thread.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void ChangeThreadVtab( AstObject *this, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     Each Object structure contains a pointer to a virtual function
*     table (vtab) that identifies information about the class to
*     which the Object belongs (function pointers, Object caches,
*     etc). In order to avoid use of mutexes (which can slow down AST
*     applications enormously), each thread has its own set of vtab
*     structures (one for each AST class) stored in thread-specific
*     data. Each time an Object is locked by the currently executing
*     thread, this function should be called to change the vtab pointer
*     in the Object to refer to the vtab relevant to the currently
*     executing thread.

*  Parameters:
*     this
*        Pointer to the Object.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   astDECLARE_GLOBALS
   const char *class;
   int i;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific data for the currently executing thread. */
   astGET_GLOBALS(this);

/* Get the class name for the supplied Object. This uses the existing
   vtab pointer in the Object structure to locate the required GetClass
   method and the class name. This vtab pointer may be for a vtab created
   by a different thread to the one currently executing, but this shouldn't
   matter since we are not modifying the vtab contents. */
   class = astGetClass( this );

/* Check a class name was obtained */
   if( class ) {

/* Loop round the vtab structures created by the currently executing thread. */
      for( i = 0; i < nvtab; i++ ) {

/* If the current vtab is for a class that matches the class of the
   supplied Object, then store a pointer to the vtab in the Object
   structure, and exit. */
         if( !strcmp( class, known_vtabs[ i ]->class ) ) {
            this->vtab = known_vtabs[ i ];
            break;
         }
      }
   }
}
#endif

AstObject *astCheckLock_( AstObject *this, int *status ) {
/*
*+
*  Name:
*     astCheckLock

*  Purpose:
*     Check that supplied Object is locked by the calling thread.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astCheckLock( AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function reports an error if the supplied object has not
*     previously been locked (using astLock) by the calling thread.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     A copy of the supplied pointer ("this") is returned. The Object
*     reference count is not changed.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*-
*/

/* This function does nothing in the non-threads version of libast. */
#if defined(THREAD_SAFE)

/* Local Variables; */
   AstObject *fail;

/* Check the supplied pointer. */
   if( this ) {

/* First use the private ManageLock function rather than the virtual
   astManageLock method to check the top level Object is locked for use
   by the current thread. This saves time and allows a more appropriate
   error message to be issued. */
      if( ManageLock( this, AST__CHECKLOCK, 0, NULL, status ) ) {
         if( astOK ) {
            astError( AST__LCKERR, "astCheckLock(%s): The supplied %s cannot "
                      "be used since it is not locked for use by the current "
                      "thread (programming error).", status, astGetClass( this ),
                      astGetClass( this ) );
         }

/* If the top level Object is locked, now use the virtual astManageLock
   method to check any objects contained within the top level Object. */
      } else if( astManageLock( this, AST__CHECKLOCK, 0, &fail ) ) {
         if( astOK ) {
            astError( AST__LCKERR, "astCheckLock(%s): The supplied %s cannot "
                      "be used since a %s contained within the %s is not "
                      "locked for use by the current thread (programming "
                      "error).", status, astGetClass( this ),
                       astGetClass( this ), astGetClass( fail ),
                       astGetClass( this ) );
         }
      }
   }
#endif

/* Return the supploed pointer. */
   return this;

}

int astClassCompare_( AstObjectVtab *class1, AstObjectVtab *class2,
                      int *status ) {
/*
*+
*  Name:
*     astClassCompare

*  Purpose:
*     Determine the relationship between two AST classes.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     int astClassCompare( AstObjectVtab *class1, AstObjectVtab *class2 )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns the number of steps up the class inheritance
*     ladder from the class specified by "class1" to the class specified
*     by "class2".

*  Parameters:
*     class1
*        Pointer to a virtual function table describing the first AST class.
*     class2
*        Pointer to a virtual function table describing the second AST class.

*  Returned Value:
*     The generation gap between "class1" and "class2". The result will be
*     positive if "class1" is a subclass of "class2", negative if "class2"
*     is a subclass of "class1", zero if they are of the same class (or
*     an error occurs), or AST__COUSIN if they are not on the same line
*     of descent.

*-
*/

/* Local Variables: */
   AstClassIdentifier *class1_id;
   AstClassIdentifier *class2_id;
   AstClassIdentifier *id;
   int *class1_check;
   int *class2_check;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status */
   if( !astOK ) return result;

/* Check pointer have been supplied. */
   if( class1 && class2 ) {

/* Get pointers to the AstClassIdentifier that identifies the top-level
   class of each vtab. */
      class1_id = class1->top_id;
      class2_id = class2->top_id;

/* Class membership is specified by the "check" value in each class
   identifier. Get the check values for both vtabs. */
      class1_check = class1_id->check;
      class2_check = class2_id->check;

/* Try walking up the class heirarchy of "class1" until the class of
   "class2" is reached. The top-level AstObject class has a NULL "parent"
   pointer in its class identifier structure. */
      id = class1_id;
      while( id && ( id->check != class2_check ) ) {
         id = id->parent;
         result++;
      }

/* If "class1" is not a subclass of "class2", try walking up the class
   heirarchy of "class2" until the class of "class1" is reached. */
      if( !id ) {
         result = 0;
         id = class2_id;
         while( id && ( id->check != class1_check ) ) {
            id = id->parent;
            result--;
         }

/* If "class2" is not a subclass of "class1", return AST__COUSIN. */
         if( !id ) result = AST__COUSIN;
      }
   }

/* Return the generation gap. */
   return result;
}

static void CleanAttribs( AstObject *this_object, int *status ) {
/*
*+
*  Name:
*     astCleanAttribs

*  Purpose:
*     Clear any invalid set attribute values.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     void astCleanAttribs( AstObject *this, int *status )

*  Class Membership:
*     Object method.

*  Description:
*     This function clears any attributes that are currently set to
*     invalid values in the supplied object. This can happen for instance
*     when an object is cast into an instance of a parent class using
*     astCast, since sub-classes can extend the range of valid values
*     an attribute can take.

*  Parameters:
*     this
*        Pointer to the Object to be cleaned.
*-
*/

/* The base Object class has no attributes that need cleaning. */

}

static void Clear( AstObject *this, const char *attrib, int *status ) {
/*
*++
*  Name:
c     astClear
f     AST_CLEAR

*  Purpose:
*     Clear attribute values for an Object.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "object.h"
c     void astClear( AstObject *this, const char *attrib )
f     CALL AST_CLEAR( THIS, ATTRIB, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function clears the values of a specified set of attributes
f     This routine clears the values of a specified set of attributes
*     for an Object. Clearing an attribute cancels any value that has
*     previously been explicitly set for it, so that the standard
*     default attribute value will subsequently be used instead. This
c     also causes the astTest function to return the value zero for
f     also causes the AST_TEST function to return the value .FALSE. for
*     the attribute, indicating that no value has been set.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
c     attrib
f     ATTRIB = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing a
c        comma-separated list of the names of the attributes to be cleared.
f        A character string containing a comma-separated list of the
f        names of the attributes to be cleared.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
*     - Attribute names are not case sensitive and may be surrounded
*     by white space.
*     - It does no harm to clear an attribute whose value has not been
*     set.
*     - An error will result if an attempt is made to clear the value
*     of a read-only attribute.
*--
*/

/* Local Variables: */
   char *buff;                   /* Pointer to character buffer */
   char *name;                   /* Pointer to individual attribute name */
   char *name_end;               /* Pointer to null at end of name */
   int i;                        /* Loop counter for characters */
   int j;                        /* Non-blank character count */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the length of the attrib string. */
   len = (int) strlen( attrib );
   if ( len != 0 ) {

/* Allocate memory and store a copy of the string. */
      buff = astStore( NULL, attrib, (size_t) ( len + 1 ) );
      if ( astOK ) {

/* Loop to process each element in the comma-separated list. */
         name = buff;
         while ( name ) {

/* Change the comma at the end of each element to a null to terminate
   the name. */
            if ( ( name_end = strchr( name, ',' ) ) ) *name_end = '\0';

/* Remove white space and upper case characters from the attribute
   name. */
            for ( i = j = 0; name[ i ]; i++ ) {
               if ( !isspace( name[ i ] ) ) name[ j++ ] = tolower( name[ i ] );
            }

/* Terminate the attribute name and pass it to astClearAttrib to clear
   the attribute (unless it is all blank, in which case we ignore
   it). */
            name[ j ] = '\0';
            if ( j ) astClearAttrib( this, name );

/* Check for errors and abort if any clear operation fails. Otherwise,
   process the next attribute. */
            if ( !astOK ) break;
            name = name_end ? name_end + 1 : NULL;
         }
      }

/* Free the memory allocated for the string buffer. */
      buff = astFree( buff );
   }
}

static void ClearAttrib( AstObject *this, const char *attrib, int *status ) {
/*
*+
*  Name:
*     astClearAttrib

*  Purpose:
*     Clear an attribute value for an Object.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     void astClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Object method.

*  Description:
*     This function clears the value of a specified attribute for an
*     Object, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Object.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Notes:
*     - The Object class does not have any writable attributes, so
*     this function merely reports an error. It is intended to be
*     extended by other class definitions.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Check the attribute name and clear the appropriate attribute. */

/* ID. */
/* --- */
   if ( !strcmp( attrib, "id" ) ) {
      astClearID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      astClearIdent( this );

/* UseDefs. */
/* -------- */
   } else if ( !strcmp( attrib, "usedefs" ) ) {
      astClearUseDefs( this );

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute string matches any of the read-only
   attributes of this class. If it does, then report an error. */
   } else if ( !strcmp( attrib, "class" ) ||
               !strcmp( attrib, "nobject" ) ||
               !strcmp( attrib, "objsize" ) ||
               !strcmp( attrib, "refcount" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Since no writable attributes are defined for the Object class, any
   attempt to clear a value for anything else is also an error. */
   } else {
      astError( AST__BADAT, "astClear: The attribute name \"%s\" is invalid "
               "for a %s.", status, attrib, astGetClass( this ) );
   }
}

AstObject *astClone_( AstObject *this, int *status ) {
/*
*++
*  Name:
c     astClone
f     AST_CLONE

*  Purpose:
*     Clone (duplicate) an Object pointer.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     AstObject *astClone( AstObject *this )
f     RESULT = AST_CLONE( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a duplicate pointer to an existing
*     Object. It also increments the Object's RefCount attribute to
*     keep track of how many pointers have been issued.
*
*     Note that this function is NOT equivalent to an assignment
*     statement, as in general the two pointers will not have the same
*     value.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Original pointer to the Object.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astClone()
f     AST_CLONE = INTEGER
*        A duplicate pointer to the same Object.

*  Applicability:
*     Object
*        This function applies to all Objects.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a lock on the object's secondary mutex. This mutex guards access
   to the "ref_count" and "locker" components of the AstObject structure. */
   LOCK_SMUTEX(this);

#ifdef MEM_DEBUG
   {   int rc;
       char buf[100];
       rc = this->ref_count;
       sprintf(buf,"cloned (refcnt: %d -> %d)", rc, rc+1 );
       astMemoryUse( this, buf );
   }
#endif

/* Increment the Object's reference count. */
   this->ref_count++;

/* Unlock the object's secondary mutex. */
   UNLOCK_SMUTEX(this);

/* Return a new pointer to the Object. */
   return this;
}

AstObject *astCopy_( const AstObject *this, int *status ) {
/*
*++
*  Name:
c     astCopy
f     AST_COPY

*  Purpose:
*     Copy an Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     AstObject *astCopy( const AstObject *this )
f     RESULT = AST_COPY( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
*     This function creates a copy of an Object and returns a pointer
*     to the resulting new Object. It makes a "deep" copy, which
*     contains no references to any other Object (i.e. if the original
*     Object contains references to other Objects, then the actual
*     data are copied, not simply the references). This means that
*     modifications may safely be made to the copy without indirectly
*     affecting any other Object.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object to be copied.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astCopy()
f     AST_COPY = INTEGER
*        Pointer to the new Object.

*  Applicability:
*     Object
*        This function applies to all Objects.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstObject *new;               /* Pointer to new object */
   AstObjectVtab *vtab;          /* Pointer to object vtab */
   int i;                        /* Loop counter for copy constructors */

/* Initiallise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Re-use cached memory, or allocate new memory using the size of the input
   object, to store the output Object. */

   vtab = this->vtab;
   if( object_caching ){

      if( vtab->nfree > 0 ) {
         new = vtab->free_list[ --(vtab->nfree) ];
         vtab->free_list[ vtab->nfree ] = NULL;
      } else {
         new = astMalloc( this->size );
      }

   } else {
      new = astMalloc( this->size );
   }

   if ( astOK ) {

/* Perform an initial byte-by-byte copy of the entire object
   structure. */
      (void) memcpy( (void *) new, (const void *) this, this->size );

/* Initialise any components of the new Object structure that need to
   differ from the input. */
      new->check = Magic( new, new->size, status );
      new->dynamic = 1;
      new->ref_count = 1;
      new->id = NULL;   /* ID attribute is not copied (but Ident is copied) */
      new->proxy = NULL;

/* Copy the persistent identifier string. */
      if( this->ident ) {
         new->ident = astStore( NULL, this->ident, strlen( this->ident ) + 1 );
      }

/* Create a new mutex for the new Object, and lock it for use by the
   current thread. */
#ifdef THREAD_SAFE
      if( pthread_mutex_init( &(new->mutex1), NULL ) != 0 && astOK ) {
         astError( AST__INTER, "astInitObject(%s): Failed to "
                   "initialise POSIX mutex1 for the new Object.", status,
                   vtab->class );
      }
      if( pthread_mutex_init( &(new->mutex2), NULL ) != 0 && astOK ) {
         astError( AST__INTER, "astInitObject(%s): Failed to "
                   "initialise POSIX mutex2 for the new Object.", status,
                   vtab->class );
      }
      new->locker = -1;
      new->globals = NULL;
      (void) ManageLock( new, AST__LOCK, 0, NULL, status );
#endif

/* Loop to execute any copy constructors declared by derived classes. */
      for ( i = 0; i < vtab->ncopy; i++ ) {

/* Invoke each copy constructor in turn. */
         (*vtab->copy[ i ])( this, new, status );

/* If any copy constructor fails, work backwards through the
   corresponding destructor functions, invoking each in turn to undo
   the copy operations that have been completed so far. */
         if ( !astOK ) {
            for ( ; i >= 0; i-- ) {
               (*vtab->delete[ i ])( new, status );
            }

/* Zero the entire new Object structure (to prevent accidental re-use
   of any of its values after deletion). */
            (void) memset( new, 0, new->size );

/* Free the Object's memory and ensure that a NULL pointer will be
   returned. */
            new = astFree( new );

/* Quit trying to copy the Object. */
            break;
         }
      }
   }

/* If OK, increment the count of active objects. */
   if ( astOK ) vtab->nobject++;

/* Return a pointer to the new Object. */
   return new;
}

AstObject *astDelete_( AstObject *this, int *status ) {
/*
*++
*  Name:
c     astDelete
f     AST_DELETE

*  Purpose:
*     Delete an Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     AstObject *astDelete( AstObject *this )
f     CALL AST_DELETE( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function deletes an Object, freeing all resources
f     This routine deletes an Object, freeing all resources
*     associated with it and rendering any remaining pointers to the
*     Object invalid.
*
*     Note that deletion is unconditional, regardless of whether other
*     pointers to the Object are still in use (possibly within other
*     Objects). A safer approach is to defer deletion, until all
c     references to an Object have expired, by using astBegin/astEnd
c     (together with astClone and astAnnul if necessary).
f     references to an Object have expired, by using AST_BEGIN/AST_END
f     (together with AST_CLONE and AST_ANNUL if necessary).

*  Parameters:
c     this
c        Pointer to the Object to be deleted.
f     THIS = INTEGER (Given and Returned)
f        Pointer to the Object to be deleted. A null pointer value
f        (AST__NULL) is always returned.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

c  Returned Value:
c     astDelete()
c        A null Object pointer (AST__NULL) is always returned.
c
*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - This function attempts to execute even if the AST error status
c     is set
f     - This routine attempts to execute even if STATUS is set to an error
f     value
*     on entry, although no further error report will be
*     made if it subsequently fails under these circumstances.
*--
*/

/* Local Variables: */
   AstObjectVtab *vtab;          /* Pointer to virtual function table */
   int dynamic;                  /* Was memory allocated dynamically? */
   int i;                        /* Loop counter for destructors */
   int ifree;                    /* Index of next slot on free list */
   int status_value;             /* AST error status value */
   size_t size;                  /* Object size */

/* Check the pointer to ensure it identifies a valid Object (this
   generates an error if it doesn't). */
   if ( !astIsAObject( this ) ) return NULL;

/* Loop through all the destructors associated with the Object by derived
   classes (working up the class hierarchy). */
   for ( i = this->vtab->ndelete - 1; i >= 0; i-- ) {

/* Invoke each destructor in turn. Attempt to continue even if destructors
   fail. */
      ( *this->vtab->delete[ i ] )( this, status );
   }

/* Free the ID strings. */
   this->id = astFree( this->id );
   this->ident = astFree( this->ident );

/* Attempt to unlock the Object and destroy its mutexes. */
#if defined(THREAD_SAFE)
   (void) ManageLock( this, AST__UNLOCK, 0, NULL, status );
   pthread_mutex_destroy( &(this->mutex1) );
   pthread_mutex_destroy( &(this->mutex2) );
#endif

/* Save the virtual function table address and note if the Object's
   memory was allocated dynamically. Also note its size. */
   vtab = this->vtab;
   dynamic = this->dynamic;
   size = this->size;

/* Zero the entire Object structure (to prevent accidental re-use of
   any of its values after deletion). */
   (void) memset( this, 0, size );

/* If necessary, free the Object's memory. If object caching is switched
   on, the memory is not in fact freed; it is merely placed onto the end
   of the list of free memory blocks included in the virtual function table
   of the AST class concerned. astGrow returns immediately if an error
   has already occurred, so we need to reset the error status explicitly
   before calling astGrow. */
   if ( dynamic ) {
      if( object_caching ) {
         ifree = (vtab->nfree)++;

         status_value = astStatus;
         astClearStatus;
         vtab->free_list = astGrow( vtab->free_list, vtab->nfree,
                                    sizeof(AstObject *) );
         astSetStatus( status_value );

         if( vtab->free_list ) vtab->free_list[ ifree ] = this;
      } else {
         (void) astFree( this );
      }
   }

/* Decrement the count of active Objects. */
   vtab->nobject--;

/* Always return NULL. */
   return NULL;
}

static void Dump( AstObject *this, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astDump

*  Purpose:
*     Write an Object to a Channel.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astDump( AstObject *this, AstChannel *channel )

*  Class Membership:
*     Object method.

*  Description:
*     This function writes an Object to a Channel, appending it to any
*     previous Objects written to that Channel.

*  Parameters:
*     this
*        Pointer to the Object to be written.
*     channel
*        Pointer to the output Channel.
*-
*/

/* Local Variables: */
   AstObjectVtab *vtab;          /* Pointer to virtual function table */
   const char *sval;             /* Pointer to string value */
   int helpful;                  /* Helpful to show value even if not set? */
   int idump;                    /* Loop counter for dump functions */
   int ival;                     /* Attribute value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Write an initial "Begin" item, giving the class name of the Object
   being written. Also supply a pointer to the comment associated with
   the most recently-declared dump function in the Object's virtual
   function table. This should describe the class to which the Object
   belongs (assuming it has correctly declared its dump function). */
   astWriteBegin( channel, astGetClass( this ),
                  this->vtab->dump_comment[ this->vtab->ndump - 1 ] );

/* Write out instance variable information for the base Object
   class. Accompany these with appropriate comment strings, possibly
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

/* ID. */
/* --- */
   set = TestID( this, status );
   sval = set ? GetID( this, status ) : astGetID( this );

/* Don't show an un-set ID value if it is blank. */
   helpful = ( sval && *sval );
   astWriteString( channel, "ID", set, helpful, sval,
                   "Object identification string" );

/* Ident. */
/* --- */
   set = TestIdent( this, status );
   sval = set ? GetIdent( this, status ) : astGetIdent( this );

/* Don't show an un-set Ident value if it is blank. */
   helpful = ( sval && *sval );
   astWriteString( channel, "Ident", set, helpful, sval,
                   "Permanent Object identification string" );

/* UseDefs */
/* ------- */
   set = TestUseDefs( this, status );
   ival = set ? GetUseDefs( this, status ) : astGetUseDefs( this );
   astWriteInt( channel, "UseDfs", set, 0, ival,
                ival ? "Default attribute values can be used" :
                       "Default values cannot be used" );

/* RefCnt. */
/* ------- */
   LOCK_SMUTEX(this);
   ival = this->ref_count;
   UNLOCK_SMUTEX(this);

   astWriteInt( channel, "RefCnt", 0, 0, ival,
                "Count of active Object pointers" );


/* Nobj. */
/* ----- */
   vtab = this->vtab;
   astWriteInt( channel, "Nobj", 0, 0, vtab->nobject,
                "Count of active Objects in same class" );

/* Terminate the information above with an "IsA" item for the base
   Object class. */
   astWriteIsA( channel, "Object", "AST Object" );

/* Now loop to perform the same operation for each additional class
   from which the Object inherits (the Object class itself does not
   declare a dump function). Invoke the dump function for each class
   in turn, working down the class hierarchy, to write out instance
   variable information for that class. */
   for ( idump = 0; idump < this->vtab->ndump; idump++ ) {
      ( *this->vtab->dump[ idump ] )( this, channel, status );

/* Terminate the output from all except the final dump function with
   an appropriate "IsA" item describing the class whose data have just
   been written. */
      if ( idump != ( this->vtab->ndump - 1 ) ) {
         astWriteIsA( channel, this->vtab->dump_class[ idump ],
                      this->vtab->dump_comment[ idump ] );
      }

/* Quit looping if an error occurs. */
      if ( !astOK ) break;
   }

/* Terminate the output from the final dump function with an "End"
   item to match the initial "Begin" item. */
   astWriteEnd( channel, astGetClass( this ) );
}

static void EmptyObjectCache( int *status ){
/*
*  Name:
*     EmptyObjectCache

*  Purpose:
*     Free all memory blocks currently on the free list of any class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     EmptyObjectCache( int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function empties the cache of Object memory by freeing all
*     memory blocks on the free_list of all classes.

*  Parameters:
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This function attempts to execute even if an error has occurred.
*/

/* Local Variables: */
   astDECLARE_GLOBALS    /* Thread-specific global data */
   int iblock;           /* Index of next entry in free list */
   int itab;             /* Index of next virtual function table */
   AstObjectVtab *vtab;  /* Pointer to next virtual function table */

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Loop round all the virtual function tables which are known about. */
   for( itab = 0; itab < nvtab; itab++ ) {
      vtab = known_vtabs[ itab ];

/* Free all memory blocks stored on the free list for this class. */
      for( iblock = 0; iblock < vtab->nfree; iblock++ ) {
         (vtab->free_list)[ iblock ] = astFree( (vtab->free_list)[ iblock ] );
      }

/* Free the memory used to hold the free list, and indicate it has zero
   length. */
      vtab->free_list = astFree( vtab->free_list );
      vtab->nfree = 0;
   }
}

static int Equal( AstObject *this, AstObject *that, int *status ){
/*
*+
*  Name:
*     astEqual

*  Purpose:
*     Check equality of two AST Objects.

*  Type:
*     Public (but undocumented) function.

*  Synopsis:
*     #include "object.h"
*     int astEqual( AstObject *this, AstObject *this )

*  Class Membership:
*     Object virtual function.

*  Description:
*     This function returns non-zero if the two pointers identify
*     equivalent objects.

*  Parameters:
*     this
*        Pointer to the first Object.
*     that
*        Pointer to the second Object.

*  Returned Value:
*     Non-zero if the objects are equivalent.

*  Notes:
*    - This function is available in the public interface even though it is
*    documented as protected. This is because it is difficult to document
*    precisely which aspects of two Objects must be equal in order for this
*    function to return a non-zero value. Each class of Object supplies
*    its own Equal method that tests which-ever attributes the class
*    considers to be significiant.
*    - The implementation of this function provided by the base Object
*    class simply compares the class names and the structure size.
*    Sub-classes should override this method to provide more appropriate tests.
*    - Zero is returned if an error has already occurred, or if
*    this function should fail for any reason.

*-
*/

/* Local Variables: */
   int result;

/* Check inherited status */
   if( !astOK ) return 0;

/* Objects are equivalent if they are the same object. */
   if( this == that ) {
      result = 1;

/* Otherwise, check the structure size and class names */
   } else {
      result = ( this->size == that->size &&
                 !strcmp( astGetClass( this ), astGetClass( that ) ) );
   }

   return result;
}

static const char *Get( AstObject *this, const char *attrib, int *status ) {
/*
*  Name:
*     Get

*  Purpose:
*     Get the value of a specified attribute for an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     const char *Get( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for an Object, formatted as a character string. It is
*     mainly a wrap-up used internally for invoking the astGetAttrib
*     method. It converts the attribute name to lower case and removes
*     white space before invoking the method. This saves derived
*     classes that over-ride the astGetAttrib method from having to do
*     this themselves.

*  Parameters:
*     this
*        Pointer to the Object.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This may contain mixed
*        case and white space, but should not be composed entirely of
*        white space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Object, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Object. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   char *buff;                   /* Pointer to local string buffer */
   const char *result;           /* Pointer value to return */
   int i;                        /* Loop counter for characters */
   int j;                        /* Non-blank character count */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Allocate a local buffer long enough to hold the attribute name
   string. */
   buff = astMalloc( strlen( attrib ) + (size_t) 1 );
   if ( astOK ) {

/* Copy the attribute name characters into the buffer, omitting all
   white space and converting to lower case. */
      for ( i = j = 0; attrib[ i ]; i++ ) {
         if ( !isspace( attrib[ i ] ) ) buff[ j++ ] = tolower( attrib[ i ] );
      }

/* Terminate the copied string. */
      buff[ j ] = '\0';

/* If no characters were copied, the attribute name was blank, so
   report an error. */
      if ( !j ) {
         if( astOK ) astError( AST__BADAT, "astGet(%s): A blank attribute "
                               "name was given.", status, astGetClass( this ) );

/* Of OK, invoke astGetAttrib to obtain a pointer to the attribute
   value formatted as a character string. */
      } else {
         result = astGetAttrib( this, buff );

/* If required, strip out graphical escape sequences. */
         if( !astEscapes( -1 ) ) result = astStripEscapes( result );
      }
   }

/* Free the local string buffer. */
   buff = astFree( buff );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static const char *GetAttrib( AstObject *this, const char *attrib, int *status ) {
/*
*+
*  Name:
*     astGetAttrib

*  Purpose:
*     Get the value of a specified attribute for an Object.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     const char *astGetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for an Object, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Object.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Object, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Object. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   const char *result;           /* Pointer value to return */
   int nobject;                  /* Nobject attribute value */
   int objsize;                  /* ObjSize attribute value */
   int ref_count;                /* RefCount attribute value */
   int usedefs;                  /* UseDefs attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(this);

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an
   appropriate format.  Set "result" to point at the result string. */

/* Class. */
/* ------ */
   if ( !strcmp( attrib, "class" ) ) {
      result = astGetClass( this );

/* ID. */
/* --- */
   } else if ( !strcmp( attrib, "id" ) ) {
      result = astGetID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      result = astGetIdent( this );

/* UseDefs */
/* ------- */
   } else if ( !strcmp( attrib, "usedefs" ) ) {
      usedefs = astGetUseDefs( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", usedefs );
         result = getattrib_buff;
      }

/* Nobject. */
/* -------- */
   } else if ( !strcmp( attrib, "nobject" ) ) {
      nobject = astGetNobject( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", nobject );
         result = getattrib_buff;
      }

/* ObjSize */
/* ------- */
   } else if ( !strcmp( attrib, "objsize" ) ) {
      objsize = astGetObjSize( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", objsize );
         result = getattrib_buff;
      }

/* RefCount. */
/* --------- */
   } else if ( !strcmp( attrib, "refcount" ) ) {
      ref_count = astGetRefCount( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ref_count );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, then report an error. */
   } else if( astOK ){
      astError( AST__BADAT, "astGet: The %s given does not have an attribute "
                "called \"%s\".", status, astGetClass( this ), attrib );
   }

/* Return the result. */
   return result;
}

const char *astGetClass_( const AstObject *this, int *status ) {
/*
*+
*  Name:
*     astGetClass

*  Purpose:
*     Obtain the value of the Class attribute for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     const char *astGetClass( const AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a pointer to the Class string for an
*     Object. This contains the name of the class which created the
*     Object.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     Pointer to a string containing the class name.

*  Notes:
*     - This function does not check the global error status before
*     executing.  This is to allow it to be used to obtain class names
*     for inclusion in error messages.
*     - A pointer to an explanatory string will be returned if this
*     function is given a pointer which does not identify an Object.
*-
*/

/* Local Variables: */
   const char *name;             /* Pointer to returned string */

/* First check if the Object pointer supplied is NULL, and set the
   returned pointer accordingly. */
   if ( !this ) {
      name = "<NULL>";

/* Also check if the supposed Object has the correct "magic number" in
   its check field. If not, it is not an Object. */
   } else if ( this->check != Magic( this, this->size, status ) ) {
      name = "<unknown>";

/* If OK, obtain a pointer to the class name from the Object's virtual
   function table. */
   } else {
      name = this->vtab->class;
   }

/* Return the result. */
   return name;
}

int astGetNobject_( const AstObject *this, int *status ) {
/*
*+
*  Name:
*     astGetNobject

*  Purpose:
*     Obtain the value of the Nobject attribute for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     int astGetNobject( const AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns the value of the Nobject attribute for an
*     Object. This is a count of the number of active Objects in the
*     same class as the Object supplied. This count does not include
*     Objects in derived classes.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     The number of active Objects.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the active object count. */
   return this->vtab->nobject;
}

static int GetObjSize( AstObject *this, int *status ) {
/*
*+
*  Name:
*     astGetObjSize

*  Purpose:
*     Determine the in-memory size of the Object.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     int astGetObjSize( AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns the in-memory size of an Object.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the object size. */
   return this->size;
}

void *astGetProxy_( AstObject *this, int *status ) {
/*
*+
*  Name:
*     astGetProxy

*  Purpose:
*     Get a pointer to the foreign language proxy used to represent a
*     given AST Object.

*  Type:
*     Undocumented public function.

*  Synopsis:
*     #include "object.h"
*     void *astGetProxy( AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns any pointer stored previously in the AST
*     Object using astSetProxy. If no such pointer has been stored, a
*     NULL pointer is returned.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     Pointer to the proxy object, or NULL.

*  Notes:
*     - This function is public, but is currently undocumented since it
*     is only of interest to people writing AST interfaces for other
*     languages.
*     - This function attempts to execute even if the AST error status
*     is set on entry, although no further error report will be made
*     if it subsequently fails under these circumstances.
*-
*/
   return this ? this->proxy : NULL;
}

int astGetRefCount_( AstObject *this, int *status ) {
/*
*+
*  Name:
*     astGetRefCount

*  Purpose:
*     Obtain the value of the RefCount attribute for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     int astGetRefCount( const AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns the value of the read-only RefCount
*     attribute for an Object. This is a "reference count" of the
*     number of active pointers to it, as accounted for by astClone
*     and astAnnul (plus the pointer issued when it was created).  If
*     the reference count for an Object falls to zero when astAnnul is
*     invoked, the object will be deleted.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     The reference count.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables; */
   int result;          /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Get a lock on the object's secondary mutex. This mutex guards access
   to the "ref_count" and "locker" components of the AstObject structure. */
   LOCK_SMUTEX(this);

/* Get the reference count. */
   result = this->ref_count;

/* Unlock the object's secondary mutex. */
   UNLOCK_SMUTEX(this);

/* Return the result. */
   return result;
}

/*
*++
*  Name:
c     astGet<X>
f     AST_GET<X>

*  Purpose:
*     Get an attribute value for an Object.

*  Type:
*     Public functions.

*  Synopsis:
c     #include "object.h"
c     <X>type astGet<X>( AstObject *this, const char *attrib )
f     RESULT = AST_GET<X>( THIS, ATTRIB, STATUS )

*  Class Membership:
*     Object methods.

*  Description:
*     This is a family of functions which return a specified attribute
*     value for an Object using one of several different data
*     types. The type is selected by replacing <X> in the function name
c     by C, D, F, I or L, to obtain a result in const char* (i.e. string),
c     double, float, int, or long format, respectively.
f     by C, D, I, L or R, to obtain a result in Character, Double
f     precision, Integer, Logical or Real format, respectively.
*
*     If possible, the attribute value is converted to the type you
*     request. If conversion is not possible, an error will result.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
c     attrib
f     ATTRIB = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing the name of
c        the attribute whose value is required.
f        A character string containing the name of the attribute whose
f        value is required.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGet<X>()
f     AST_GET<X> = <X>type
c        The attribute value, in the data type corresponding to <X> (or,
c        in the case of astGetC, a pointer to a constant null-terminated
c        character string containing this value).
f        The attribute value, in the data type corresponding to <X>.

*  Applicability:
*     Object
*        These functions apply to all Objects.

*  Examples:
c     printf( "RefCount = %d\n", astGetI( z, "RefCount" ) );
c        Prints the RefCount attribute value for Object "z" as an int.
c     title = astGetC( axis, "Title" );
c        Obtains a pointer to a null-terminated character string containing
c        the Title attribute of Object "axis".
f     WRITE( *, '('' RefCount = '', A10 )' ) AST_GETC( Z, 'RefCount', STATUS )
f        Prints the RefCount attribute value for Object Z as a character
f        string.
f     NAXES = AST_GETI( FRAME, 'Naxes', STATUS )
f        Obtains the value of the Naxes attribute for Object FRAME as an
f        integer.

*  Notes:
*     - Attribute names are not case sensitive and may be surrounded
*     by white space.
*     - An appropriate "null" value will be returned if this function
c     is invoked with the AST error status set, or if it should
f     is invoked with STATUS set to an error value, or if it should
*     fail for any reason. This null value is zero for numeric
c     values and NULL for pointer values.
f     values, .FALSE. for logical values, and blank for character values.
f     - Numerical attribute values of zero translate to logical value
f     .FALSE. and all other numerical values translate to .TRUE..
c     - The pointer returned by astGetC is guaranteed to remain valid
c     and the string to which it points will not be over-written for a
c     total of 50 successive invocations of this function. After this,
c     the memory containing the string may be re-used, so a copy of
c     the string should be made if it is needed for longer than this.
*--
*/

/* Define a macro that expands to implement the astGetX_ member
   functions required. The arguments to this macro are:

      code
         The character that appears at the end of the function name.
      type
         The C type of the function return value.
      format
         A quoted string containing a astSscanf format specifier that
         will read the attribute value into a variable of the required
         data type. This format should transfer 1 astSscanf value.
*/
#define MAKE_GETX(code,type,format) \
type astGet##code##_( AstObject *this, const char *attrib, int *status ) { \
\
/* Local Variables: */ \
   const char *str;              /* Pointer to string attribute value */ \
   int nc;                       /* Number of characters read from string */ \
   int nval;                     /* Number of values read from string */ \
   type result;                  /* Value to return */ \
   type value;                   /* Converted value */ \
\
/* Initialise. */ \
   result = (type) 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Obtain the attribute value as a string. */ \
   str = Get( this, attrib, status ); \
   if ( astOK ) { \
\
/* Read the value from the string, ignoring surrounding white \
   space. */ \
      nc = 0; \
      nval = astSscanf( str, " " format " %n", &value, &nc ); \
\
/* Check that the number of values read was 1 and that all the \
   string's characters were consumed. If so, use the result. */ \
      if ( ( nval == 1 ) && ( nc >= (int) strlen( str ) ) ) { \
         result = value; \
\
/* If the read was unsuccessful, report an error. */ \
      } else if( astOK ) { \
         astError( AST__ATGER, "astGet" #code "(%s): The attribute " \
                   "value \"%s=%s\" cannot be read using the requested data " \
                   "type.", status,astGetClass( this ), attrib, str ); \
      } \
   } \
\
/* Return the result. */ \
   return result; \
}

/* Use this macro to create all the GetX_ private member functions,
   except SetC (which is handled separately). */
MAKE_GETX(D,double,"%lf")
MAKE_GETX(F,float,"%f")
MAKE_GETX(I,int,"%d")
MAKE_GETX(L,long,"%ld")

/* Handle GetC separately because memory must be allocated to hold the
   returned character values. */
const char *astGetC_( AstObject *this, const char *attrib, int *status ) {

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   const char *result;           /* Pointer value to return */
   const char *value;            /* Pointer to attribute value */
   int i;                        /* Loop count */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(this);

/* If the "strings" array has not been initialised, fill it with
   NULL pointers. */
   if ( !astgetc_init ) {
      astgetc_init = 1;
      for ( i = 0; i < AST__ASTGETC_MAX_STRINGS; i++ ) astgetc_strings[ i ] = NULL;
   }

/* Obtain a pointer to the required attribute value, formatted as a
   character string. */
   value = Get( this, attrib, status );

/* If OK, store a copy of the resulting string in dynamically
   allocated memory, putting a pointer to the copy into the next
   element of the "astgetc_strings" array.  (This process also de-allocates
   any previously allocated memory pointed at by this "strings"
   element, so the earlier string is effectively replaced by the new
   one.) */
   if ( astOK ) {

      astBeginPM;
      astgetc_strings[ astgetc_istr ] = astStore( astgetc_strings[ astgetc_istr ],
                                             value, strlen( value ) + (size_t) 1 );
      astEndPM;

/* If OK, return a pointer to the copy and increment "astgetc_istr" to use the
   next element of "astgetc_strings" on the next invocation. Recycle
   "astgetc_istr" to zero when all elements have been used. */
      if ( astOK ) {
         result = astgetc_strings[ astgetc_istr++ ];
         if ( astgetc_istr == ( AST__ASTGETC_MAX_STRINGS - 1 ) ) astgetc_istr = 0;
      }
   }

/* Return the result. */
   return result;

}

static int HasAttribute( AstObject *this, const char *attrib, int *status ) {
/*
*++
*  Name:
c     astHasAttribute
f     AST_HASATTRIBUTE

*  Purpose:
*     Test if an Object has a named attribute.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astHasAttribute( AstObject *this, const char *attrib )
f     RESULT = AST_HASATTRIBUTE( THIS, ATTRIB, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function returns a boolean result (0 or 1) to indicate
f     This function returns a logical result to indicate
*     whether the supplied Object has an attribute with the supplied name.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the first Object.
c     attrib
f     ATTRIB = INTEGER (Given)
c        Pointer to a string holding the
f        The
*        name of the attribute to be tested.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astHasAttribute()
c        One if the Object has the named attribute, otherwise zero.
f     AST_SAME = LOGICAL
f        .TRUE. if the Object has the named attribute, otherwise
f        .FALSE.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - A value of zero will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any reason.
f     - A value of .FALSE. will be returned if this function is invoked
f     with STATUS set to an error value, or if it should fail for any reason.
*--
*/

/* Local Variables: */
   int oldrep;           /* Original AST error reporting flag */
   int result;           /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Temporarily switch off error reporting. */
   oldrep = astReporting( 0 );

/* Attempt to get a value for the specified attribute. */
   (void) Get( this, attrib, status );

/* An error will have been reported if the object does not have the
   requested attribute. Set the result and clear the error status. */
   if( !astOK ) {
      result = 0;
      astClearStatus;
   } else {
      result = 1;
   }

/* Re-instate the original error reporting flag. */
   (void) astReporting( oldrep );

/* Return the result. */
   return result;
}

static unsigned long Magic( const AstObject *this, size_t size, int *status ) {
/*
*  Name:
*     Magic

*  Purpose:
*     Generate a "magic number" for an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     unsigned long Magic( const AstObject *this, size_t size, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function generates a "magic number" which is a function of an Object
*     pointer (address) and an Object size. This number may be stored in an
*     Object to allow it to be recognised as a valid Object by other routines
*     and to provide security against argument passing errors, etc.

*  Parameters:
*     this
*        Pointer to an Object.
*     size
*        The Object size.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The magic number.

*  Notes:
*     -  This function does not perform any error checking.
*/

/* Form the bit-wise exclusive OR between the Object address and the Object
   size, then add 2 and invert the bits. Return the result as an unsigned
   long integer. */
   return ~( ( ( (unsigned long) this ) ^ ( (unsigned long) size ) ) +
             ( (unsigned long) 2 ) );
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*+
*  Name:
*     astManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     int astManageLock( AstObject *this, int mode, int extra,
*                        AstObject **fail )

*  Class Membership:
*     Object method.

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
*        calling thread.
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.

*  Returned Value:
*     A status value:
*        0 - Success.
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.
*        5 - Check failed - object is locked by a different thread
*        6 - Check failed - object is unlocked
*

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int result;                   /* Returned value */

/* Initialise */
   result = 0;
   if( fail ) *fail = NULL;

/* Check the supplied point is not NULL. */
   if( ! this ) return result;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Get a lock on the object's secondary mutex. This gives us exclusive
   access to the "locker" (and "ref_count") component in the AstObject
   structure. All other components in the structure are guarded by the
   primary mutex (this->mutex1). */
   if( LOCK_SMUTEX(this) ) {
      result = 2;

/* If the secondary mutex was locked succesfully, first deal with cases
   where the caller wants to lock the Object for exclusive use by the
   calling thread. */
   } else if( mode == AST__LOCK ) {

/* If the Object is not currently locked, lock the Object primary mutex
   and record the identity of the calling thread in the Object. */
      if( this->locker == -1 ) {
         if( LOCK_PMUTEX(this) ) result = 2;
         this->locker = AST__THREAD_ID;
         this->globals = AST__GLOBALS;
         ChangeThreadVtab( this, status );

/* If the Object is already locked by the calling thread, do nothing. */
      } else if( this->locker == AST__THREAD_ID ) {

/* If the object is locked by a different thread, and the caller is
   willing to wait, attempt to lock the Object primary mutex. This will
   cause the calling thread to block until the Object is release by the
   thread that currently has it locked. Then store the identity of the
   calling thread (the new lock owner). We first need to release the
   secondary mutex so that the other thread can modify the "locker"
   component in the AstObject structure when it releases the Object
   (using this function). We then re-lock the secondary mutex so this
   thread can change the "locker" component safely. */
      } else if( extra ) {
         if( UNLOCK_SMUTEX(this) ) {
            result = 3;
         } else if( LOCK_PMUTEX(this) ) {
            result = 2;
         } else if( LOCK_SMUTEX(this) ) {
            result = 2;
         }
         this->locker = AST__THREAD_ID;
         this->globals = AST__GLOBALS;
         ChangeThreadVtab( this, status );

/* If the caller does not want to wait until the Object is available,
   return a status of 1. */
      } else {
         result = 1;
      }

/* Unlock the Object for use by other threads. */
   } else if( mode == AST__UNLOCK ) {

/* Do nothing if the Object is currently unlocked. */
      if( this->locker == -1 ) {

/* If the object is currently locked by the calling thread, clear the
   identity of the thread that owns the lock and unlock the primary
   mutex. */
      } else if( this->locker == AST__THREAD_ID ) {
         this->locker = -1;
         this->globals = NULL;
         if( UNLOCK_PMUTEX(this) ) result = 3;

/* Return an error status value if the Object is locked by another
   thread. */
      } else {
         result = 1;
      }

/* Check the Object is locked by the calling thread. Return a status of 1 if
   not. */
   } else if( mode == AST__CHECKLOCK ) {
      if( this->locker == -1 ) {
         result = 6;
      } else if( this->locker != AST__THREAD_ID ) {
         result = 5;
      }

/* Return a status of 4 for any other modes. */
   } else {
      result = 4;
   }

/* Unlock the secondary mutex so that other threads can access the "locker"
   component in the Object to see if it is locked. */
   if( UNLOCK_SMUTEX(this) ) result = 3;

/* If the operation failed, return a pointer to the failed object. */
   if( result && fail ) *fail = this;

/* Return the status value */
   return result;
}
#endif

char *astToString_( AstObject *this, int *status ) {
/*
c++
*  Name:
*     astToString

*  Purpose:
*     Create an in-memory serialisation of an Object

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     char *astToString( AstObject *this )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a string holding a minimal textual
*     serialisation of the supplied AST Object. The Object can re
*     re-created from the serialisation using astFromString.

*  Parameters:
*     this
*        Pointer to the Object to be serialised.

*  Returned Value:
*     astToString()
*        Pointer to dynamically allocated memory holding the
*        serialisation, or NULL if an error occurs. The pointer
*        should be freed when no longer needed using astFree.

c--
*/

/* Local Variables: */
   StringData data;              /* Data passed to the sink function */
   AstChannel *channel;          /* Pointer to output Channel */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Create a Channel which will write to an expanding dynamically
   allocated memory buffer. Set Channel attributes to exclude all
   non-essential characters. */
   channel = astChannel( NULL, ToStringSink, "Comment=0,Full=-1,Indent=0",
                         status );

/* Initialise the data structure used to communicate with the sink
   function, and store a pointer to it in the Channel. */
   data.ptr = NULL;
   data.buff = NULL;
   data.len = 0;
   astPutChannelData( channel, &data );

/* Write the Object to the Channel. */
   astWrite( channel, this );

/* Annul the Channel pointer. */
   channel = astAnnul( channel );

/* Free the returned string if an error has occurred. */
   if( !astOK ) data.ptr = astFree( data.ptr );

/* Return the pointer. */
   return data.ptr;
}

static void ToStringSink( const char *text ){
/*
*  Name:
*     ToStringSink

*  Purpose:
*     A Channel sink function for use by the astToString method.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*      ToStringSink( const char *text )

*  Class Membership:
*     Object member function.

*  Description:
*     This function appends the supplied line of text to the end of a
*     dynamically growing memory block.

*  Parameters:
*     text
*        Pointer to the null-terminated line of text to be stored.

*/

/* Local Variables: */
   StringData *data;     /* Data passed to the sink function */
   int *status;          /* Pointer to local status value */
   int status_value;     /* Local status value */

/* Set up the local status */
   status_value = 0;
   status = &status_value;

/* Get a pointer to the structure holding the current memory pointer and
   the length of the currently allocated memory. */
   data = astChannelData;

/* Append the supplied text to the end of the string, and update the
   string length. */
   data->ptr = astAppendString( data->ptr, &(data->len), text );

/* Append a newline character to the end of the string, and update the
   string length. */
   data->ptr = astAppendString( data->ptr, &(data->len), "\n" );
}

void astSet_( void *this_void, const char *settings, int *status, ... ) {
/*
*++
*  Name:
c     astSet
f     AST_SET

*  Purpose:
*     Set attribute values for an Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astSet( AstObject *this, const char *settings, ... )
f     CALL AST_SET( THIS, SETTINGS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function assigns a set of attribute values to an Object,
f     This routine assigns a set of attribute values to an Object,
*     over-riding any previous values. The attributes and their new
*     values are specified via a character string, which should
*     contain a comma-separated list of the form:
*
*        "attribute_1 = value_1, attribute_2 = value_2, ... "
*
*     where "attribute_n" specifies an attribute name, and the value
*     to the right of each "=" sign should be a suitable textual
*     representation of the value to be assigned. This value will be
*     interpreted according to the attribute's data type.
c
c     The string supplied may also contain "printf"-style format
c     specifiers, identified by "%" signs in the usual way. If
c     present, these will be substituted by values supplied as
c     additional optional arguments (using the normal "printf" rules)
c     before the string is used.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
c     settings
f     SETTINGS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing a
c        comma-separated list of attribute settings in the form described
c        above.
f        A character string containing a comma-separated list of
f        attribute settings in the form described above.
c     ...
c        Optional additional arguments which supply values to be
c        substituted for any "printf"-style format specifiers that
c        appear in the "settings" string.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Examples:
c     astSet( map, "Report = 1, Zoom = 25.0" );
c        Sets the Report attribute for Object "map" to the value 1 and
c        the Zoom attribute to 25.0.
c     astSet( frame, "Label( %d ) =Offset along axis %d", axis, axis );
c        Sets the Label(axis) attribute for Object "frame" to a
c        suitable string, where the axis number is obtained from
c        "axis", a variable of type int.
c     astSet( frame, "Title =%s", mystring );
c        Sets the Title attribute for Object "frame" to the contents of
c        the string "mystring".
f     CALL AST_SET( MAP, 'Report = 1, Zoom = 25.0', STATUS )
f        Sets the Report attribute for Object MAP to the value 1 and
f        the Zoom attribute to 25.0.
f     CALL AST_SET( FRAME, 'Label( 1 ) =Offset from cluster axis', STATUS )
f        Sets the Label(1) attribute for Object FRAME to a suitable
f        string.

*  Notes:
*     - Attribute names are not case sensitive and may be surrounded
*     by white space.
*     - White space may also surround attribute values, where it will
*     generally be ignored (except for string-valued attributes where
*     it is significant and forms part of the value to be assigned).
*     - To include a literal comma in the value assigned to an attribute,
*     the whole attribute value should be enclosed in quotation markes.
c     Alternatively, you can use "%s" format and supply the value as a
c     separate additional argument to astSet (or use the astSetC
c     function instead).
c     - The same procedure may be adopted if "%" signs are to be included
c     and are not to be interpreted as format specifiers (alternatively,
c     the "printf" convention of writing "%%" may be used).
*     - An error will result if an attempt is made to set a value for
*     a read-only attribute.
*--

*  Implementation Notes:
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     Object identifier is of type (void *) and is converted and
*     validated within the function itself.
*     - This implementation of astSet is designed to be used within AST,
*     and has an explicit status parameter. From outside AST, the astSet
*     macro will invoke the astSetId_ function which does not have an
*     status parameter.

*--
*/

/* Local Variables: */
   AstObject *this;              /* Pointer to the Object structure */
   va_list args;                 /* Variable argument list */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain and validate a pointer to the Object structure. */
   this = astCheckObject( this_void );
   if ( astOK ) {

/* Obtain the variable argument list and pass all arguments to the
   astVSet method for interpretation. */
      va_start( args, status );
      astVSet( this, settings, NULL, args );
      va_end( args );
   }
}

static void SetAttrib( AstObject *this, const char *setting, int *status ) {
/*
*+
*  Name:
*     astSetAttrib

*  Purpose:
*     Set an attribute value for an Object.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     void astSetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     Object method.

*  Description:
*     This function assigns an attribute value for an Object, the attribute and
*     its value being specified by means of a string of the form:
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
*        Pointer to the Object.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.

*  Notes:
*     - The Object class does not have any writable attributes, so
*     this function merely reports an error. It is intended to be
*     extended by other class definitions.
*-
*/

/* Local Variables: */
   int id;                       /* Offset of ID string */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* ID. */
/* --- */
   if ( nc = 0, ( 0 == astSscanf( setting, "id=%n%*[^\n]%n", &id, &nc ) )
                && ( nc >= len ) ) {
      astSetID( this, setting + id );

/* Ident. */
/* ------ */
   } else if ( nc = 0, ( 0 == astSscanf( setting, "ident=%n%*[^\n]%n", &id, &nc ) )
                && ( nc >= len ) ) {
      astSetIdent( this, setting + id );

/* UseDefs */
/* ------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "usedefs= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetUseDefs( this, ival );

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class and use this to report an error
   if it does. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

   } else if ( MATCH( "class" ) ||
               MATCH( "nobject" ) ||
               MATCH( "objsize" ) ||
               MATCH( "refcount" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Since no writable attributes are defined for the Object class, any
   attempt to set a value for anything else is also an error. */
   } else {
      astError( AST__BADAT, "astSet: The attribute setting \"%s\" is invalid "
               "for a %s.", status, setting, astGetClass( this ) );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

void astSetCopy_( AstObjectVtab *vtab,
                  void (* copy)( const AstObject *, AstObject *, int * ), int *status ) {
/*
*+
*  Name:
*     astSetCopy

*  Purpose:
*     Declare a copy constructor for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astSetCopy( AstObjectVtab *vtab,
*                      void (* copy)( const AstObject *, AstObject * ) )

*  Class Membership:
*     Object method.

*  Description:
*     This function is provided so that class definitions can declare a copy
*     constructor to be associated with an Object that is being constructed.
*     When a copy is later performed on the Object, the copy constructor of
*     each class to which the Object belongs will be invoked in turn (working
*     down the class hierarchy). The copy constructor is passed pointers to the
*     source and destination Objects. It should implement the copy and return
*     void.

*  Parameters:
*     vtab
*        Pointer to the Object's virtual function table, in which the copy
*        constructor's pointer is to be stored for future use.
*     copy
*        Pointer to the copy constructor function.

*  Notes:
*     -  When an Object is copied, a byte-by-byte copy of its structure is
*     automatically made before any copy constructors are invoked. A copy
*     constructor need only be provided if this does not suffice (e.g. if the
*     structure contains pointers to other data).
*     - If a copy constructor is declared for a class, then a
*     destructor for that class must also be declared (using
*     astSetDelete) so that there is a one-to-one correspondence
*     between copy constructors and their associated destructors.
*     -  Copy constructors should check the global error status in the normal
*     way and should set it (and report an error) if they fail.
*-
*/


/* Check the global status. */
   if ( !astOK ) return;

/* Indicate that subsequent memory allocations may never be freed (other
   than by any AST exit handler). */
   astBeginPM;

/* Expand the array of copy constructor pointers in the virtual function table
   (if necessary) to accommodate the new one. */
   vtab->copy = astGrow( vtab->copy, vtab->ncopy + 1,
                        sizeof( void (*)( const AstObject *, AstObject * ) ) );

/* If OK, store the new function pointer and increment the count of copy
   constructors. */
   if ( astOK ) {
      vtab->copy[ vtab->ncopy++ ] = copy;
   }

/* Mark the end of the section in which memory allocations may never be freed
   (other than by any AST exit handler). */
   astEndPM;

}

void astSetDelete_( AstObjectVtab *vtab, void (* delete)( AstObject *, int * ), int *status ) {
/*
*+
*  Name:
*     astSetDelete

*  Purpose:
*     Declare a destructor for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astSetDelete( AstObjectVtab *vtab, void (* delete)( AstObject * ) )

*  Class Membership:
*     Object method.

*  Description:
*     This function is provided so that class definitions can declare a
*     destructor to be associated with an Object. When the Object is later
*     deleted, the destructor declared by each class to which the Object
*     belongs will be invoked in turn (working up the class hierarchy). The
*     destructor is passed a pointer to the Object. It should free any
*     resources (e.g. memory) associated with it and return void. It should
*     not free the memory containing the Object itself.

*  Parameters:
*     vtab
*        Pointer to the Object's virtual function table, in which the
*        destructor's pointer is to be stored for future use.
*     delete
*        Pointer to the destructor function.

*  Notes:
*     - A destructor need not be declared for a class if there are no
*     resources to free.
*     - If a destructor is declared for a class, then a copy
*     constructor for that class must also be declared (using
*     astSetCopy) so that there is a one-to-one correspondence between
*     copy constructors and their associated destructors.
*     - A destructor function should generally attempt to execute even
*     if the global error status is set on entry, but should not
*     report further errors in that case (errors should be reported
*     normally if status is not set on entry).
*-
*/


/* Check the global status. */
   if ( !astOK ) return;

/* Indicate that subsequent memory allocations may never be freed (other
   than by any AST exit handler). */
   astBeginPM;

/* Expand the array of destructor pointers in the virtual function table (if
   necessary) to accommodate the new one. */
   vtab->delete = astGrow( vtab->delete, vtab->ndelete + 1,
                           sizeof( void (*)( AstObject * ) ) );

/* If OK, store the new function pointer and increment the count of
   destructors. */
   if ( astOK ) {
      vtab->delete[ vtab->ndelete++ ] = delete;
   }

/* Mark the end of the section in which memory allocations may never be freed
   (other than by any AST exit handler). */
   astEndPM;

}

void astSetDump_( AstObjectVtab *vtab,
                  void (* dump)( AstObject *, AstChannel *, int * ),
                  const char *class, const char *comment, int *status ) {
/*
*+
*  Name:
*     astSetDump

*  Purpose:
*     Declare a dump function for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astSetDump( AstObjectVtab *vtab,
*                      void (* dump)( AstObject *, AstChannel * ),
*                      const char *class, const char *comment )

*  Class Membership:
*     Object method.

*  Description:
*     This function is provided so that class definitions can declare
*     a dump function to be associated with an Object that is being
*     constructed.  When the astWrite (or astShow or astToString) method
*     is later used to write the Object to a Channel, the dump function
*     of each class to which the Object belongs will be invoked in turn
*     (working down the class hierarchy). The dump function is passed
*     pointers to the Object and the output Channel. It should write
*     out any internal values (e.g. instance variables) for its class
*     that are to be kept (using the protected astWrite... methods of
*     the Channel) and return void.

*  Parameters:
*     vtab
*        Pointer to the Object's virtual function table, in which the
*        dump function's pointer is to be stored for future use.
*     dump
*        Pointer to the dump function.
*     class
*        Pointer to a constant null-terminated string (residing in
*        static memory) containing the name of the class that is
*        declaring the dump function.
*     comment
*        Pointer to a constant null-terminated string (residing in
*        static memory) containing a comment to associate with the
*        dump function.  This should normally describe the purpose of
*        the class that is declaring the dump function.

*  Notes:
*     - Dump functions should check the global error status in the
*     normal way and should set it (and report an error) if they fail.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Indicate that subsequent memory allocations may never be freed (other
   than by any AST exit handler). */
   astBeginPM;

/* Expand the arrays of pointers to dump functions and related data in
   the virtual function table (if necessary) to accommodate the new
   one. */
   vtab->dump = astGrow( vtab->dump, vtab->ndump + 1,
                         sizeof( void (*)( AstObject *, AstChannel * ) ) );
   vtab->dump_class = astGrow( vtab->dump_class, vtab->ndump + 1,
                               sizeof( char * ) );
   vtab->dump_comment = astGrow( vtab->dump_comment, vtab->ndump + 1,
                                 sizeof( char * ) );

/* If OK, store the new pointers (to the dump function, class name and
   comment) and increment the count of dump functions. */
   if ( astOK ) {
      vtab->dump[ vtab->ndump ] = dump;
      vtab->dump_class[ vtab->ndump ] = class;
      vtab->dump_comment[ vtab->ndump ] = comment;
      vtab->ndump++;
   }

/* Mark the end of the section in which memory allocations may never be
   freed (other than by any AST exit handler). */
   astEndPM;
}

void astSetProxy_( AstObject *this, void *proxy, int *status ) {
/*
*+
*  Name:
*     astSetProxy

*  Purpose:
*     Store a pointer to the foreign language proxy used to represent a
*     given AST Object.

*  Type:
*     Undocumented public function.

*  Synopsis:
*     #include "object.h"
*     void astSetProxy( AstObject *this, void *proxy )

*  Class Membership:
*     Object method.

*  Description:
*     This function stores the supplied pointer in the AST Object so that
*     it can be retrieved later using astGetProxy.
*
*     The supplied pointer should point to a structure that is used
*     to represent the AST Object within some external system. It is
*     expected that the external system will check each object reference
*     returned by AST to see if it has an associated proxy object. If not
*     (i.e. if astGetProxy returns NULL), a new external object will be
*     created to represent the AST Object, and a pointer to it will be
*     stored in the AST Object using astSetProxy. If the AST Object
*     already has a proxy, the AST reference is annulled and the existing
*     proxy object is used by the external system.

*  Parameters:
*     this
*        Pointer to the Object.
*     proxy
*        Pointer to the proxy object, or NULL.

*  Notes:
*     - The suppied pointer is not used within AST itself, other than to
*     be returned by the astGetProxy method.
*     - This function is public, but is currently undocumented since it
*     is only of interest to people writing AST interfaces for other
*     languages.
*-
*/
   if( !astOK ) return;
   this->proxy = proxy;
}

void astSetVtab_( AstObject *this, AstObjectVtab *vtab, int *status ) {
/*
*+
*  Name:
*     astSetVtab

*  Purpose:
*     Change the virtual function table associated with an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astSetVtab( AstObject *this, AstObjectVtab *vtab )

*  Class Membership:
*     Object method.

*  Description:
*     This function changes the virtual function table associated with an
*     Object. This may be needed, for instance, if a super-class
*     initialises a parent class structure with a NULL vtab, causing the
*     vtab of the parent class to be used instead of the super-class.
*     Whilst the super-class object is being constructed its inherited methods
*     will be determined by the parent class. Once the super-class object
*     has been constructed, it can invoke this fuction in order to
*     set the vtab to the super-class vtab, thus causing the method
*     implementations provided by the super-cvlass to be used.

*  Parameters:
*     this
*        Pointer to the Object to be modified.
*     vtab
*        Pointer to the virtual function table to store in the Object.
*-
*/
   if( this ) this->vtab = vtab;
}

static int Same( AstObject *this, AstObject *that, int *status ) {
/*
*++
*  Name:
c     astSame
f     AST_SAME

*  Purpose:
*     Test if two AST pointers refer to the same Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astSame( AstObject *this,  AstObject *that )
f     RESULT = AST_SAME( THIS, THAT, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function returns a boolean result (0 or 1) to indicate
f     This function returns a logical result to indicate
*     whether two pointers refer to the same Object.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the first Object.
c     that
f     THAT = INTEGER (Given)
*        Pointer to the second Object.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astSame()
c        One if the two pointers refer to the same Object, otherwise zero.
f     AST_SAME = LOGICAL
f        .TRUE. if the two pointers refer to the same Object, otherwise
f        .FALSE.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
*     - Two independent Objects that happen to be identical are not
*     considered to be the same Object by this function.
c     - A value of zero will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any reason.
f     - A value of .FALSE. will be returned if this function is invoked
f     with STATUS set to an error value, or if it should fail for any reason.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the result. */
   return ( this == that ) ? 1 : 0;
}

/*
*++
*  Name:
c     astSet<X>
f     AST_SET<X>

*  Purpose:
*     Set an attribute value for an Object.

*  Type:
*     Public functions.

*  Synopsis:
c     #include "object.h"
c     void astSet<X>( AstObject *this, const char *attrib, <X>type value )
f     CALL AST_SET<X>( THIS, ATTRIB, VALUE, STATUS )

*  Class Membership:
*     Object methods.

*  Description:
c     This is a family of functions which set a specified attribute
f     This is a family of routines which set a specified attribute
*     value for an Object using one of several different data
c     types. The type is selected by replacing <X> in the function name
f     types. The type is selected by replacing <X> in the routine name
c     by C, D, F, I or L, to supply a value in const char* (i.e. string),
c     double, float, int, or long format, respectively.
f     by C, D, I, L or R, to supply a value in Character, Double
f     precision, Integer, Logical or Real format, respectively.
*
*     If possible, the value you supply is converted to the type of
*     the attribute. If conversion is not possible, an error will
*     result.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
c     attrib
f     ATTRIB = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing the
c        name of the attribute whose value is to be set.
f        A character string containing the name of the attribute whose
f        value is to be set.
c     value
f     VALUE = <X>type (Given)
c        The value to be set for the attribute, in the data type corresponding
c        to <X> (or, in the case of astSetC, a pointer to a null-terminated
c        character string containing this value).
f        The value to be set for the attribute, in the data type corresponding
f        to <X>.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        These functions apply to all Objects.
f        These routines apply to all Objects.

*  Examples:
c     astSetI( frame, "Preserve", 1 );
c        Sets the Preserve attribute value for Object "frame" to 1.
c     astSetC( plot, "Format(1)", "%.2g" );
c        Sets the Format(1) attribute value for Object "plot" to the
c        character string "%.2g".
f     CALL AST_SETC( PLOT, 'Title', CVALUE, STATUS )
f        Sets the Title attribute value for Object PLOT to the contents
f        of the character variable CVALUE.
f     CALL AST_SETL( FRAME, 'Preserve', .TRUE., STATUS );
f        Sets the Preserve attribute value for Object FRAME to 1 (true).

*  Notes:
*     - Attribute names are not case sensitive and may be surrounded
*     by white space.
f     - The logical value .FALSE. will translate to a numerical attribute
f     value of zero and logical .TRUE. will translate to one.
*     - An error will result if an attempt is made to set a value for
*     a read-only attribute.
*--
*/

/* Define a macro that expands to implement the astSetX_ member
   functions required. The arguments to this macro are:

      code
         The character that appears at the end of the function name.
      type
         The C type of the function "value" parameter.
      format
         A quoted string containing a sprintf format specifier that will
         format the supplied value as a character string. This format should
         consume 2 sprintf arguments: a field width and the value to be
         formatted.
      fmtlen
         The number of characters in the format specifier (above).
      fieldsz
         The value of the field width to be used by the format specifier.
*/
#define MAKE_SETX(code,type,format,fmtlen,fieldsz) \
void astSet##code##_( AstObject *this, const char *attrib, type value, int *status ) { \
\
/* Local Variables: */ \
   char *setting;                /* Pointer to attribute setting string */ \
   int len;                      /* Length of attribute name */ \
\
/* Check the global status. */ \
   if ( !astOK ) return; \
\
/* Obtain the length of the attribute name and allocate memory to hold \
   this name plus the format specifier to be appended to it. */ \
   len = (int) astChrLen( attrib ); \
   setting = astMalloc( (size_t) ( len + fmtlen + 2 ) ); \
\
/* Make a copy of the attribute name in the allocated memory. */ \
   if ( astOK ) { \
      (void) memcpy( setting, attrib, (size_t) len ); \
      setting[ len ] = 0; \
\
/* Append "=", followed by the format specifier, to construct a \
   suitable "setting" string for use by astSet. */ \
      (void) strcat( setting, "=" format ); \
\
/* Invoke astSet to set the attribute value. */ \
      astSet( this, setting, status, fieldsz, value ); \
   } \
\
/* Free the allocated memory. */ \
   setting = astFree( setting ); \
}

/* Use this macro to create all the SetX_ private member functions. */
MAKE_SETX(D,double,"%.*g",4,DBL_DIG)
MAKE_SETX(F,float,"%.*g",4,FLT_DIG)
MAKE_SETX(I,int,"%.*d",4,1)
MAKE_SETX(L,long,"%.*ld",5,1)


/* The astSetC_ function is implemented separately so that commas can be
   handled. Since astSetC can only be used to set a single attribute
   value, we know that any commas in the supplied value are included
   within the attribuite value, rather than being used as delimiters
   between adjacent attribute settings. To avoid VSet using them as
   delimiters, they are replaced here by '\r' before calling astSet, and
   VSet then converts them back to commas. */

void astSetC_( AstObject *this, const char *attrib, const char *value, int *status ) {

/* Local Variables: */
   char *d;                      /* Pointer to next setting character */
   char *newv;                   /* Pointer to new attribute value string */
   char *setting;                /* Pointer to attribute setting string */
   const char *c;                /* Pointer to next value character */
   int len;                      /* Length of attribute name */

/* Check the global status. */
   if ( !astOK ) return;

/* Produce a copy of the supplied attribute value in which any commas
   are replaced by carriage returns ("\r"). */
   newv = astMalloc( (size_t)( strlen( value ) + 1 ) );
   if( newv ) {
      d = newv;
      c = value;
      while( *c ) {
         if( *c == ',' ) {
            *d = '\r';
         } else {
            *d = *c;
         }
         c++;
         d++;
      }
      *d = 0;

/* Obtain the length of the attribute name and allocate memory to hold
   this name plus the format specifier to be appended to it. */
      len = (int) astChrLen( attrib );
      setting = astMalloc( (size_t) ( len + 5 ) );

/* Make a copy of the attribute name in the allocated memory. */
      if ( astOK ) {
         (void) memcpy( setting, attrib, (size_t) len );
         setting[ len ] = 0;

/* Append "=", followed by the format specifier, to construct a
   suitable "setting" string for use by astSet. */
         (void) strcat( setting, "=%*s" );

/* Invoke astSet to set the attribute value. */
         astSet( this, setting, status, 0, newv );
      }

/* Free the allocated memory. */
      setting = astFree( setting );
   }
   newv = astFree( newv );
}

static void Show( AstObject *this, int *status ) {
/*
*++
*  Name:
c     astShow
f     AST_SHOW

*  Purpose:
*     Display a textual representation of an Object on standard output.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astShow( AstObject *this )
f     CALL AST_SHOW( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function displays a textual description of any AST Object
f     This routine displays a textual description of any AST Object
*     on standard output. It is provided primarily as an aid to
*     debugging.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object to be displayed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.
*--
*/

/* Local Variables: */
   AstChannel *channel;          /* Pointer to output Channel */

/* Check the global error status. */
   if ( !astOK ) return;

/* Create a Channel which will write to standard output. */
   channel = astChannel( NULL, NULL, "", status );

/* Write the Object to the Channel. */
   astWrite( channel, this );

/* Annul the Channel pointer. */
   channel = astAnnul( channel );
}

int astTest_( AstObject *this, const char *attrib, int *status ) {
/*
*++
*  Name:
c     astTest
f     AST_TEST

*  Purpose:
*     Test if an Object attribute value is set.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astTest( AstObject *this, const char *attrib )
f     RESULT = AST_TEST( THIS, ATTRIB, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function returns a boolean result (0 or 1) to indicate
f     This function returns a logical result to indicate
*     whether a value has been explicitly set for one of an Object's
*     attributes.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
c     attrib
f     ATTRIB = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing
c        the name of the attribute to be tested.
f        A character string containing the name of the attribute to be
f        tested.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astTest()
c        One if a value has previously been explicitly set for the attribute
c        (and hasn't been cleared), otherwise zero.
f     AST_TEST = LOGICAL
f        .TRUE. if a value has previously been explicitly set for the
f        attribute (and hasn't been cleared), otherwise .FALSE..

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
*     - Attribute names are not case sensitive and may be surrounded
*     by white space.
c     - A value of zero will be returned if this function is invoked
f     - A value of .FALSE. will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any reason.
f     with STATUS set to an error value, or if it should fail for any reason.
c     - A value of zero will also be returned if this function is used
f     - A value of .FALSE. will also be returned if this function is used
*     to test a read-only attribute, although no error will result.
*--
*/

/* Local Variables: */
   char *buff;                   /* Pointer to character buffer */
   int i;                        /* Loop counter for characters */
   int j;                        /* Non-blank character count */
   int len;                      /* Length of attrib string */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain the length of the attrib string. */
   len = (int) strlen( attrib );

/* Allocate memory and store a copy of the string. */
   buff = astStore( NULL, attrib, (size_t) ( len + 1 ) );
   if ( astOK ) {

/* Remove white space and upper case characters. */
      for ( i = j = 0; buff[ i ]; i++ ) {
         if ( !isspace( buff[ i ] ) ) buff[ j++ ] = tolower( buff[ i ] );
      }

/* Terminate the attribute name and pass it to astTestAttrib to test
   the attribute. */
      buff[ j ] = '\0';
      result = astTestAttrib( this, buff );
   }

/* Free the memory allocated for the string buffer. */
   buff = astFree( buff );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int TestAttrib( AstObject *this, const char *attrib, int *status ) {
/*
*+
*  Name:
*     astTestAttrib

*  Purpose:
*     Test if a specified attribute value is set for an Object.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     int astTestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of an Object's attributes.

*  Parameters:
*     this
*        Pointer to the Object.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check the attribute name and test the appropriate attribute. */

/* ID. */
/* --- */
   if ( !strcmp( attrib, "id" ) ) {
      result = astTestID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      result = astTestIdent( this );

/* UseDefs */
/* ------- */
   } else if ( !strcmp( attrib, "usedefs" ) ) {
      result = astTestUseDefs( this );

/* Test if the attribute string matches any of the read-only
   attributes of this class. If it does, then return zero. */
   } else if ( !strcmp( attrib, "class" ) ||
               !strcmp( attrib, "nobject" ) ||
               !strcmp( attrib, "objsize" ) ||
               !strcmp( attrib, "refcount" ) ) {
      result = 0;

/* Any attempt to test any other attribute is an error. */
   } else if( astOK ){
      astError( AST__BADAT, "astTest: The attribute name \"%s\" is invalid "
               "for a %s.", status, attrib, astGetClass( this ) );
   }

/* Return the result, */
   return result;
}

int astTune_( const char *name, int value, int *status ) {
/*
*++
*  Name:
c     astTune
f     AST_TUNE

*  Purpose:
*     Set or get an integer-valued AST global tuning parameter.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astTune( const char *name, int value )
f     RESULT = AST_TUNE( NAME, VALUE, STATUS )

*  Class Membership:
*     Object function.

*  Description:
*     This function returns the current value of an integer-valued AST
*     global tuning parameter, optionally storing a new value for the
*     parameter. For character-valued tuning parameters, see
c     astTuneC.
f     AST_TUNEC.

*  Parameters:
c     name
f     NAME = CHARACTER * ( * ) (Given)
*        The name of the tuning parameter (case-insensitive).
c     value
f     VALUE = INTEGER (Given)
*        The new value for the tuning parameter. If this is AST__TUNULL,
*        the existing current value will be retained.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astTune()
f     AST_TUNE = INTEGER
c        The original value of the tuning parameter. A default value will
*        be returned if no value has been set for the parameter.

*  Tuning Parameters:
*     ObjectCaching
*        A boolean flag which indicates what should happen
*        to the memory occupied by an AST Object when the Object is deleted
*        (i.e. when its reference count falls to zero or it is deleted using
c        astDelete).
f        AST_DELETE).
*        If this is zero, the memory is simply freed using the systems "free"
*        function. If it is non-zero, the memory is not freed. Instead a
*        pointer to it is stored in a pool of such pointers, all of which
*        refer to allocated but currently unused blocks of memory. This allows
*        AST to speed up subsequent Object creation by re-using previously
*        allocated memory blocks rather than allocating new memory using the
*        systems malloc function. The default value for this parameter is
*        zero. Setting it to a non-zero value will result in Object memory
*        being cached in future. Setting it back to zero causes any memory
*        blocks currently in the pool to be freed. Note, this tuning parameter
*        only controls the caching of memory used to store AST Objects. To
*        cache other memory blocks allocated by AST, use MemoryCaching.
*     MemoryCaching
*        A boolean flag similar to ObjectCaching except
*        that it controls caching of all memory blocks of less than 300 bytes
*        allocated by AST (whether for internal or external use), not just
*        memory used to store AST Objects.

*  Notes:
c     - This function attempts to execute even if the AST error
c     status is set
f     - This routine attempts to execute even if STATUS is set to an
f     error value
*     on entry, although no further error report will be
*     made if it subsequently fails under these circumstances.
*     - All threads in a process share the same AST tuning parameters
*     values.
*--
*/

   int result = AST__TUNULL;

   if( name ) {

      LOCK_MUTEX1;

      if( astChrMatch( name, "ObjectCaching" ) ) {
         result = object_caching;
         if( value != AST__TUNULL ) {
            object_caching = value;
            if( !object_caching ) EmptyObjectCache( status );
         }

      } else if( astChrMatch( name, "MemoryCaching" ) ) {
         result = astMemCaching( value );

      } else if( astOK ) {
         astError( AST__TUNAM, "astTune: Unknown AST tuning parameter "
                   "specified \"%s\".", status, name );
      }

      UNLOCK_MUTEX1;

   }

   return result;
}

void astTuneC_( const char *name, const char *value, char *buff,
                int bufflen, int *status ) {
/*
*++
*  Name:
c     astTuneC
f     AST_TUNEC

*  Purpose:
*     Set or get a character-valued AST global tuning parameter.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astTuneC( const char *name, const char *value, char *buff,
c                    int bufflen )
f     CALL AST_TUNEC( NAME, VALUE, BUFF, STATUS )

*  Class Membership:
*     Object function.

*  Description:
*     This function returns the current value of a character-valued
*     AST global tuning parameter, optionally storing a new value
*     for the parameter. For integer-valued tuning parameters, see
c     astTune.
f     AST_TUNE.

*  Parameters:
c     name
f     NAME = CHARACTER * ( * ) (Given)
*        The name of the tuning parameter (case-insensitive).
c     value
f     VALUE = CHARACTER * ( ) (Given)
*        The new value for the tuning parameter. If this is
f        AST__TUNULLC,
c        NULL,
*        the existing current value will be retained.
c     buff
f     BUFF = CHARACTER * ( ) (Given)
*        A character string in which to return the original value of
*        the tuning parameter. An error will be reported if the buffer
*        is too small to hold the value.
c        NULL may be supplied if the old value is not required.
c     bufflen
c        The size of the supplied "buff" array. Ignored if "buff" is NULL.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Tuning Parameters:
*     HRDel
*        A string to be drawn following the hours field in a formatted
*        sky axis value when "g" format is in use (see the Format
*        attribute). This string may include escape sequences to produce
*        super-scripts, etc. (see the Escapes attribute for details
*        of the escape sequences allowed). The default value is
*        "%-%^50+%s70+h%+" which produces a super-script "h".
*     MNDel
*        A string to be drawn following the minutes field in a formatted
*        sky axis value when "g" format is in use. The default value is
*        "%-%^50+%s70+m%+" which produces a super-script "m".
*     SCDel
*        A string to be drawn following the seconds field in a formatted
*        sky axis value when "g" format is in use. The default value is
*        "%-%^50+%s70+s%+" which produces a super-script "s".
*     DGDel
*        A string to be drawn following the degrees field in a formatted
*        sky axis value when "g" format is in use. The default value is
*        "%-%^53+%s60+o%+" which produces a super-script "o".
*     AMDel
*        A string to be drawn following the arc-minutes field in a formatted
*        sky axis value when "g" format is in use. The default value is
*        "%-%^20+%s85+'%+" which produces a super-script "'" (single quote).
*     ASDel
*        A string to be drawn following the arc-seconds field in a formatted
*        sky axis value when "g" format is in use. The default value is
*        "%-%^20+%s85+\"%+" which produces a super-script """ (double quote).
*     EXDel
*        A string to be drawn to introduce the exponent in a value when "g"
*        format is in use. The default value is "10%-%^50+%s70+" which
*        produces "10" followed by the exponent as a super-script.

*  Notes:
c     - This function attempts to execute even if the AST error
c     status is set
f     - This routine attempts to execute even if STATUS is set to an
f     error value
*     on entry, although no further error report will be
*     made if it subsequently fails under these circumstances.
*     - All threads in a process share the same AST tuning parameters
*     values.
*--
*/

/* Local Variables: */
   char *p;
   int len;

/* Check the name of a tuning parameter was supplied. */
   if( name ) {

/* Serialise access to the tuning parameters since they are common to all
   threads. */
      LOCK_MUTEX1;

/* Get a pointer to the buffer that holds the value of the requested
   tuning parameter. */
      if( astChrMatch( name, "hrdel" ) ) {
         p = hrdel;
      } else if( astChrMatch( name, "mndel" ) ) {
         p = mndel;
      } else if( astChrMatch( name, "scdel" ) ) {
         p = scdel;
      } else if( astChrMatch( name, "dgdel" ) ) {
         p = dgdel;
      } else if( astChrMatch( name, "amdel" ) ) {
         p = amdel;
      } else if( astChrMatch( name, "asdel" ) ) {
         p = asdel;
      } else if( astChrMatch( name, "exdel" ) ) {
         p = exdel;

/* Report an error if an the tuning parameter name is unknown. */
      } else if( astOK ) {
         p = NULL;
         astError( AST__TUNAM, "astTuneC: Unknown AST tuning parameter "
                   "specified \"%s\".", status, name );
      }

/* If the existing value was found. */
      if( p ) {

/* And is to be returned in the supplied buffer... */
         if( buff ) {

/* Check that the buffer is long enough. If so, copy the current value
   into the buffer, otherwise report an error. */
            len = strlen( p ) ;
            if( len < bufflen ) {
               strcpy( buff, p );
            } else {
               astError( AST__TUNAM, "astTuneC: Supplied string variable "
                         "is too small - the current '%s' value (%s) has "
                         "%d characters.", status, name, p, len );
            }
         }

/* If a new value is to be stored.... */
         if( value ) {

/* Report an error if it is too long to fit in the static buffer. */
            len = strlen( value ) ;
            if( len >= MAXLEN_TUNEC ) {
               astError( AST__TUNAM, "astTuneC: Supplied value for '%s' "
                         "(%s) is too long - must not be longer than %d "
                         "characters.", status, name, value, MAXLEN_TUNEC );

/* Otherwise, copy the new value into the static buffer. */
            } else {
               strcpy( p, value );
            }
         }
      }

      UNLOCK_MUTEX1;
   }
}

AstObject *astFromString_( const char *string, int *status ) {
/*
c++
*  Name:
*     astFromString

*  Purpose:
*     Re-create an Object from an in-memory serialisation

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astFromString( const char *string )

*  Class Membership:
*     Object method.

*  Description:
*     This function returns a pointer to a new Object created from the
*     supplied text string, which should have been created by astToString.

*  Parameters:
*     string
*        Pointer to a text string holding an Object serialisation created
*        previously by astToString.

*  Returned Value:
*     astFromString()
*        Pointer to a new Object created from the supplied serialisation,
*        or NULL if the serialisation was invalid, or an error occurred.

c--
*/

/* Local Variables: */
   StringData data;              /* Data passed to the source function */
   AstChannel *channel;          /* Pointer to output Channel */
   AstObject *result;            /* Pointer to returned Object */

/* Check the global error status and supplied serialisation. */
   if ( !astOK || !string ) return NULL;

/* Create a Channel which will read from the supplied serialisation. */
   channel = astChannel( FromStringSource, NULL, "", status );

/* Initialise the data structure used to communicate with the source
   function, and store a pointer to it in the Channel. */
   data.ptr = (char *) string;
   data.buff = NULL;
   data.len = 0;
   astPutChannelData( channel, &data );

/* Read an Object from the Channel. */
   result = astRead( channel );

/* Annul the Channel pointer. */
   channel = astAnnul( channel );

/* Free the line buffer. */
   data.buff = astFree( data.buff );

/* Annul the returned Object if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the Object pointer. */
   return result;
}

static const char *FromStringSource( void ){
/*
*  Name:
*     FromStringSource

*  Purpose:
*     A Channel source function for use by the astFromString method.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     result = FromStringSource( void )

*  Class Membership:
*     Object member function.

*  Description:
*     This function reads the next line of text from a serialisation and
*     returns a pointer to it, or NULL if no lines remain.

*  Returned Value:
*     Pointer to the null terminated line of text or NULL if no lines
*     remain.
*/

/* Local Variables: */
   StringData *data;     /* Data passed to the sink function */
   char *nl;             /* Pointer to next newline character */
   int *status;          /* Pointer to local status value */
   int nc;               /* Number of characters to read from serialisation */
   int status_value;     /* Local status value */

/* Set up the local status */
   status_value = 0;
   status = &status_value;

/* Get a pointer to the structure holding a pointer to the next line, and
   to the buffer to return. */
   data = astChannelData;

/* Return NULL if no text remains to be read. */
   if( !data->ptr || (data->ptr)[0] == 0 ) return NULL;

/* Find the next newline (if any) in the serialisation. */
   nl = strchr( data->ptr, '\n' );

/* Get the number of characters to copy. */
   nc = nl ? nl - data->ptr : strlen( data->ptr );

/* Copy them into the returned buffer, including an extra character for
   the terminating null. */
   data->buff = astStore( data->buff, data->ptr, nc + 1 );

/* Store the terminating null. */
   (data->buff)[ nc ] = 0;

/* Update the pointer to the next character to read from the
   serialisation. */
   data->ptr = nl ? nl + 1 : NULL;

/* Return the buffer. */
   return data->buff;
}

static void VSet( AstObject *this, const char *settings, char **text,
                  va_list args, int *status ) {
/*
*+
*  Name:
*     astVSet

*  Purpose:
*     Set values for an Object's attributes.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "object.h"
*     void astVSet( AstObject *this, const char *settings, char **text,
*                   va_list args )

*  Class Membership:
*     Object method.

*  Description:
*     This function assigns a set of attribute values for an Object,
*     the attributes and their values being specified by means of a
*     string containing a comma-separated list of the form:
*
*        "attribute1 = value1, attribute2 = value2, ... "
*
*     Here, "attribute" specifies an attribute name and the value to
*     the right of each "=" sign should be a suitable textual
*     representation of the value to be assigned to that
*     attribute. This will be interpreted according to the attribute's
*     data type.
*
*     The string supplied may also contain "printf"-style format
*     specifiers identified by a "%" sign in the usual way. If
*     present, these will be substituted by values supplied as
*     optional arguments (as a va_list variable argument list), using
*     the normal "printf" rules, before the string is used.

*  Parameters:
*     this
*        Pointer to the Object.
*     settings
*        Pointer to a null-terminated string containing a
*        comma-separated list of attribute settings.
*     text
*        Pointer to a location at which to return a pointer to dynamic
*        memory holding a copy of the expanded setting string. This memory
*        should be freed using astFree when no longer needed. If a NULL
*        pointer is supplied, no string is created.
*     args
*        The variable argument list which contains values to be
*        substituted for any "printf"-style format specifiers that
*        appear in the "settings" string.

*  Notes:
*     - Attribute names are not case sensitive.
*     - White space may surround attribute names and will be ignored.
*     - White space may also surround attribute values where it will
*     be ignored (except for string-valued attributes where it is
*     significant and forms part of the value to be assigned).
*     - After this function has substituted values for "printf"-style
*     format specifiers it splits the "settings" string into
*     individual attribute settings which it passes one at a time to
*     the protected astSetAttrib method (after removal of white space
*     and conversion of attribute names to lower case). The
*     astSetAttrib method should therefore be extended by derived
*     classes which define new attributes, and this will allow the
*     astVSet (and astSet) methods to have access to those attributes.
*     - This function provides the same functionality as the astSet
*     public method but accepts a va_list variable argument list
*     instead of a variable number of arguments. It is provided for
*     use by functions in other class implementations which accept a
*     variable number of arguments and must therefore pass their
*     argument list to this method in va_list form.
*-
*/

/* Local Constants: */
   const int min_buff_len = 1024; /* Minimum size of formatting buffer */
#define ERRBUF_LEN 80

/* Local Variables: */
   char errbuf[ ERRBUF_LEN ];    /* Buffer for system error message */
   char *errstat;                /* Pointer to error message */
   char *assign;                 /* Pointer to assigment substring */
   char *assign_end;             /* Pointer to null at end of assignment */
   char *buff1;                  /* Pointer to temporary string buffer */
   char *buff2;                  /* Pointer to temporary string buffer */
   char *buff3;                  /* Pointer to temporary string buffer */
   char *eq1;                    /* Pointer to 1st equals sign */
   int buff_len;                 /* Length of temporary buffer */
   int i;                        /* Loop counter for characters */
   int j;                        /* Offset for revised assignment character */
   int len;                      /* Length of settings string */
   int lo;                       /* Convert next character to lower case? */
   int nc;                       /* Number of vsprintf output characters */
   int quoted;                   /* Are we in a quoted string? */
   int stat;                     /* Value of errno after an error */
   int tq;                       /* Test if the next non-space is a quote? */

/* Initialise */
   if( text ) *text = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the length of the "settings" string and test it is not
   zero. If it is, there is nothing more to do. */
   len = (int) strlen( settings );
   if ( len != 0 ) {

/* Allocate memory and store a copy of the string. */
      buff1 = astStore( NULL, settings, (size_t) ( len + 1 ) );
      if ( astOK ) {

/* Convert each comma in the string into '\n'. This is to distinguish
   commas initially present from those introduced by the formatting to
   be performed below. We only do this if there is more than one equals
   sign in the setting string, since otherwise any commas are probably
   characters contained within a string attribute value. Ignore commas
   that occur within quoted strings. */
         eq1 = strchr( buff1, '=' );
         if( eq1 && strchr( eq1 + 1, '=' ) ) {
            quoted = 0;
            for ( i = 0; i < len; i++ ) {
               if( !quoted ) {
                  if ( buff1[ i ] == ',' ) {
                     buff1[ i ] = '\n';
                  } else if( buff1[ i ] == '"' ) {
                     quoted = 1;
                  }
               } else if( buff1[ i ] == '"' ){
                  quoted = 0;
               }
            }
         }

/* Calculate a size for a further buffer twice the size of the first
   one.  Ensure it is not less than a minimum size and then allocate
   this buffer. */
         buff_len = 2 * len;
         if ( buff_len < min_buff_len ) buff_len = min_buff_len;
         buff2 = astMalloc( (size_t) ( buff_len + 1 ) );
         if ( astOK ) {

/* Use "vsprintf" to substitute values for any format specifiers in
   the "settings" string, writing the resulting string into the second
   buffer. */
            errno = 0;
            nc = vsprintf( buff2, buff1, args );

/* Get a copy of the expanded string to return as the function value and
   convert newlines back to commas. */
            if( text ) {
               *text = astStore( NULL, buff2, nc + 1 );
               if( *text ) {
                  for ( i = 0; i <= nc; i++ ) {
                     if ( (*text)[ i ] == '\n' ) (*text)[ i ] = ',';
                  }
               }
            }

/* The possibilities for error detection are limited here, but check
   if an error value was returned and report an error. Include
   information from errno if it was set. */
            if ( nc < 0 ) {
               if( astOK ) {
                  stat = errno;

                  if( stat ) {
#if HAVE_STRERROR_R
                     strerror_r( stat, errbuf, ERRBUF_LEN );
                     errstat = errbuf;
#else
                     errstat = strerror( stat );
#endif
                  } else {
                     *errbuf = 0;
                     errstat = errbuf;
                  }

                  astError( AST__ATSER, "astVSet(%s): Error formatting an "
                            "attribute setting%s%s.", status, astGetClass( this ),
                            stat? " - " : "", errstat );
                  astError( AST__ATSER, "The setting string was \"%s\".", status,
                            settings );
               }

/* Also check that the result buffer did not overflow. If it did,
   memory will probably have been corrupted but this cannot be
   prevented with "vsprintf" (although we try and make the buffer
   large enough). Report the error and abort. */
            } else if ( nc > buff_len ) {
               if( astOK ) {
                  astError( AST__ATSER, "astVSet(%s): Internal buffer overflow "
                            "while formatting an attribute setting - the result "
                            "exceeds %d characters.", status, astGetClass( this ),
                            buff_len );
                  astError( AST__ATSER, "The setting string was \"%s\".", status,
                            settings );
               }

/* If all is OK, loop to process each formatted attribute assignment
   (these are now separated by '\n' characters). */
	    } else {
               assign = buff2;
               while ( assign ) {

/* Change the '\n' at the end of each assignment to a null to
   terminate it. */
                  if ( ( assign_end = strchr( assign, '\n' ) ) ) {
                     *assign_end = '\0';
                  }

/* Before making the assignment, loop to remove white space and upper
   case characters from the attribute name. */
                  lo = 1;
                  tq = -1;
                  quoted = 0;
                  for ( i = j = 0; assign[ i ]; i++ ) {

/* Note when an '=' sign is encountered (this signals the end of the
   attribute name). */
                     if ( assign[ i ] == '=' ) lo = 0;

/* Before the '=' sign, convert all characters to lower case and move
   everything to the left to eliminate white space. Afer the '=' sign,
   copy all characters to their new location unchanged, except for any
   delimiting quotes, which are removed. astSetC replaces commas in the
   attribute value by '\r' characters. Reverse this now. */
                     if ( !lo || !isspace( assign[ i ] ) ) {
                        if( assign[ i ] == '\r' ) {
                           assign[ j++ ] = ',';

                        } else if( lo ) {
                           assign[ j++ ] = tolower( assign[ i ] );

                        } else {
                           assign[ j++ ] = assign[ i ];

                           if( tq > 0 && !isspace( assign[ i ] ) ) {
                              if( assign[ i ] == '"' ) {
                                 quoted = 1;
                                 j--;
                              }
                              tq = 0;
                           }

                        }
                     }

/* If the current character is the initial '=' sign, set "tq" positive,
   meaning "check if the next non-space character is a quote". */
                     if ( assign[ i ] == '=' && tq == -1 ) tq = 1;
                  }

/* if the value was quoted. remove the trailing quote. */
                  if( quoted ) {
                     j--;
                     while( isspace( assign[ j ] ) ) j--;
                     if( assign[ j ] == '"' ) j--;
                     j++;
                  }

/* Terminate the revised assignment string and pass it to astSetAttrib
   to make the assignment (unless the string was all blank, in which
   case we ignore it). */
                  assign[ j ] = '\0';
                  if ( j ) {

/* If there are no characters to the right of the equals sign append a
   space after the equals sign. Without this, a string such as "Title="
   would not be succesfully matched against the attribute name "Title"
   within SetAttrib. */
                     if( assign[ j - 1 ] == '=' ) {
                        buff3 = astStore( NULL, assign,
                                          (size_t) j + 2 );
                        if ( astOK ) {
                           buff3[ j ] = ' ';
                           buff3[ j + 1 ] = '\0';
                           astSetAttrib( this, buff3 );
                        }
                        buff3 = astFree( buff3 );

                     } else {
                        astSetAttrib( this, assign );
                     }
                  }

/* Check for errors and abort if any assignment fails. Otherwise,
   process the next assignment substring. */
                  if ( !astOK ) break;
                  assign = assign_end ? assign_end + 1 : NULL;
               }
	    }
         }

/* Free the memory allocated for string buffers. */
         buff2 = astFree( buff2 );
      }
      buff1 = astFree( buff1 );
   }
}
#undef ERRBUF_LEN

/* Attribute access functions. */
/* --------------------------- */
/*
*att++
*  Name:
*     Class

*  Purpose:
*     Object class name.

*  Type:
*     Public attribute.

*  Synopsis:
*     Character string, read-only.

*  Description:
*     This attribute gives the name of the class to which an Object
*     belongs.

*  Applicability:
*     Object
*        All Objects have this attribute.
*att--
*/

/*
*att++
*  Name:
*     ID

*  Purpose:
*     Object identification string.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute contains a string which may be used to identify
*     the Object to which it is attached. There is no restriction on
*     the contents of this string, which is not used internally by the
*     AST library, and is simply returned without change when
*     required. The default value is an empty string.
*
*     An identification string can be valuable when, for example,
c     several Objects have been stored in a file (using astWrite) and
f     several Objects have been stored in a file (using AST_WRITE) and
c     are later retrieved (using astRead). Consistent use of the ID
f     are later retrieved (using AST_READ). Consistent use of the ID
*     attribute allows the retrieved Objects to be identified without
*     depending simply on the order in which they were stored.
*
*     This attribute may also be useful during debugging, to
c     distinguish similar Objects when using astShow to display them.
f     distinguish similar Objects when using AST_SHOW to display them.

*  Applicability:
*     Object
*        All Objects have this attribute.

*  Notes:
*     - Unlike most other attributes, the value of the ID attribute is
*     not transferred when an Object is copied. Instead, its value is
*     undefined (and therefore defaults to an empty string) in any
*     copy. However, it is retained in any external representation of
c     an Object produced by the astWrite function.
f     an Object produced by the AST_WRITE routine.
*att--
*/
/* Clear the ID value by freeing the allocated memory and assigning a
   NULL pointer. */
astMAKE_CLEAR(Object,ID,id,astFree( this->id ))

/* If the ID value is not set, supply a default in the form of a
   pointer to the constant string "". */
astMAKE_GET(Object,ID,const char *,NULL,( this->id ? this->id : "" ))

/* Set an ID value by freeing any previously allocated memory,
   allocating new memory and storing the string. */
astMAKE_SET(Object,ID,const char *,id,astStore( this->id, value,
                                                strlen( value ) + (size_t) 1 ))

/* The ID value is set if the pointer to it is not NULL. */
astMAKE_TEST(Object,ID,( this->id != NULL ))

/*
*att++
*  Name:
*     Ident

*  Purpose:
*     Permanent Object identification string.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute is like the ID attribute, in that it contains a
*     string which may be used to identify the Object to which it is
*     attached. The only difference between ID and Ident is that Ident
*     is transferred when an Object is copied, but ID is not.

*  Applicability:
*     Object
*        All Objects have this attribute.

*att--
*/
/* Clear the Ident value by freeing the allocated memory and assigning a
   NULL pointer. */
astMAKE_CLEAR(Object,Ident,ident,astFree( this->ident ))

/* If the Ident value is not set, supply a default in the form of a
   pointer to the constant string "". */
astMAKE_GET(Object,Ident,const char *,NULL,( this->ident ? this->ident : "" ))

/* Set an Ident value by freeing any previously allocated memory,
   allocating new memory and storing the string. */
astMAKE_SET(Object,Ident,const char *,ident,astStore( this->ident, value,
                                                strlen( value ) + (size_t) 1 ))

/* The Ident value is set if the pointer to it is not NULL. */
astMAKE_TEST(Object,Ident,( this->ident != NULL ))

/*
*att++
*  Name:
*     UseDefs

*  Purpose:
*     Use default values for unspecified attributes?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute specifies whether default values should be used
*     internally for object attributes which have not been assigned a
*     value explicitly. If a non-zero value (the default) is supplied for
*     UseDefs, then default values will be used for attributes which have
*     not explicitly been assigned a value. If zero is supplied for UseDefs,
*     then an error will be reported if an attribute for which no explicit
*     value has been supplied is needed internally within AST.
*
*     Many attributes (including the UseDefs attribute itself) are unaffected
*     by the setting of the UseDefs attribute, and default values will always
*     be used without error for such attributes. The "Applicability:" section
*     below lists the attributes which are affected by the setting of UseDefs.

*     Note, UseDefs only affects access to attributes internally within
*     AST. The public accessor functions such as
c     astGetC
f     AST_GETC
*     is unaffected by the UseDefs attribute - default values will always
*     be returned if no value has been set. Application code should use the
c     astTest
f     AST_TEST
*     function if required to determine if a value has been set for an
*     attribute.

*  Applicability:
*     Object
*        All Objects have this attribute, but ignore its setting except
*        as described below for individual classes.
*     FrameSet
*        The default value of UseDefs for a FrameSet is redefined to be
*        the UseDefs value of its current Frame.
*     CmpFrame
*        The default value of UseDefs for a CmpFrame is redefined to be
*        the UseDefs value of its first component Frame.
*     Region
*        The default value of UseDefs for a Region is redefined to be
*        the UseDefs value of its encapsulated Frame.
*     Frame
*        If UseDefs is zero, an error is reported when aligning Frames if the
*        Epoch, ObsLat or ObsLon attribute is required but has not been
*        assigned a value explicitly.
*     SkyFrame
*        If UseDefs is zero, an error is reported when aligning SkyFrames
*        if any of the following attributes are required but have not been
*        assigned a value explicitly: Epoch, Equinox.
*     SpecFrame
*        If UseDefs is zero, an error is reported when aligning SpecFrames
*        if any of the following attributes are required but have not been
*        assigned a value explicitly: Epoch, RefRA, RefDec, RestFreq,
*        SourceVel, StdOfRest.
*     DSBSpecFrame
*        If UseDefs is zero, an error is reported when aligning DSBSpecFrames
*        or when accessing the ImagFreq attribute if any of the following
*        attributes are required but have not been assigned a value explicitly:
*        Epoch, DSBCentre, IF.
*att--
*/
astMAKE_CLEAR(Object,UseDefs,usedefs,CHAR_MAX)
astMAKE_GET(Object,UseDefs,int,1,((this->usedefs!=CHAR_MAX)?this->usedefs:1))
astMAKE_SET(Object,UseDefs,int,usedefs,((value)?1:0))
astMAKE_TEST(Object,UseDefs,(this->usedefs!=CHAR_MAX))

/*
*att++
*  Name:
*     Nobject

*  Purpose:
*     Number of Objects in class.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the total number of Objects currently in
*     existence in the same class as the Object whose attribute value
*     is requested. This count does not include Objects which belong
*     to derived (more specialised) classes.
*
*     This attribute is mainly intended for debugging. It can be used
*     to detect whether Objects which should have been deleted have,
*     in fact, been deleted.

*  Applicability:
*     Object
*        All Objects have this attribute.
*att--
*/

/*
*att++
*  Name:
*     ObjSize

*  Purpose:
*     The in-memory size of the Object.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the total number of bytes of memory used by
*     the Object. This includes any Objects which are encapsulated within
*     the supplied Object.

*  Applicability:
*     Object
*        All Objects have this attribute.
*att--
*/

/*
*att++
*  Name:
*     RefCount

*  Purpose:
*     Count of active Object pointers.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the number of active pointers associated
*     with an Object. It is modified whenever pointers are created or
c     annulled (by astClone, astAnnul or astEnd for example). The count
f     annulled (by AST_CLONE, AST_ANNUL or AST_END for example). The count
*     includes the initial pointer issued when the Object was created.
*
*     If the reference count for an Object falls to zero as the result
*     of annulling a pointer to it, then the Object will be deleted.

*  Applicability:
*     Object
*        All Objects have this attribute.
*att--
*/

/* Standard class functions. */
/* ========================= */
/*
*+
*  Name:
*     astCheck<Class>

*  Purpose:
*     Validate class membership.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "class.h"
*     Ast<Class> *astCheck<Class>( Ast<Class> *this )

*  Class Membership:
*     <Class> class function.

*  Description:
*     This function validates membership of the class called <Class>,
*     or of any class derived from it. If the Object is not a member,
*     or the pointer supplied does not identify a valid Object, an
*     error is reported and the global error status is set to
*     AST__OBJIN.

*  Parameters:
*     this
*        Pointer to the Object.

*  Returned Value:
*     The function always returns a copy of the "this" pointer
*     (whether it finds it valid or not).

*  Notes:
*     - Each class provides a function (astCheck<Class>) of this form,
*     where <Class> and <class> are replaced by the class name (with
*     appropriate capitalisation).
*     - Normal error status checking is performed, so this function
*     will not execute if the global error status is set on entry (the
*     usual function value will be returned, however).
*     - This function is primarily intended for validating Object
*     pointers passed to member functions as part of a class
*     interface.
*-
*/

/* Implement the astCheckObject function using the macro defined for this
   purpose in the "object.h" header file. */
astMAKE_CHECK(Object)

int astIsAObject_( const AstObject *this, int *status ) {
/*
*++
*  Name:
c     astIsA<Class>
f     AST_ISA<CLASS>

*  Purpose:
*     Test membership of a class by an Object.

*  Type:
*     Public function.

*  Synopsis:
c     #include "class.h"
c     int astIsA<Class>( const Ast<Class> *this )
f     RESULT = AST_ISA<CLASS>( THIS, STATUS )

*  Class Membership:
*     Class function.

*  Description:
*     This is a family of functions which test whether an Object is a
c     member of the class called <Class>, or of any class derived from
f     member of the class called <CLASS>, or of any class derived from
*     it.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Object.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astIsA<Class>()
c        One if the Object belongs to the class called <Class> (or to a
c        class derived from it), otherwise zero.
f     AST_ISA<CLASS> = LOGICAL
f        .TRUE. if the Object belongs to the class called <CLASS> (or to
f        a class derived from it), otherwise .FALSE..

*  Applicability:
*     Object
*        These functions apply to all Objects.

*  Examples:
c     member = astIsAFrame( obj );
c        Tests whether Object "obj" is a member of the Frame class, or
c        of any class derived from a Frame.
f     MEMBER = AST_ISAFRAME( OBJ, STATUS );
f        Tests whether Object OBJ is a member of the Frame class, or
f        of any class derived from a Frame.

*  Notes:
c     - Every AST class provides a function (astIsA<Class>) of this
c     form, where <Class> should be replaced by the class name.
f     - Every AST class provides a function (AST_ISA<CLASS>) of this
f     form, where <CLASS> should be replaced by the class name.
c     - This function attempts to execute even if the AST error status
c     is set
f     - This function attempts to execute even if STATUS is set to an
f     error value
*     on entry, although no further error report will be made
*     if it subsequently fails under these circumstances.
c     - A value of zero will be returned if this function should fail
f     - A value of .FALSE. will be returned if this function should fail
*     for any reason. In particular, it will fail if the pointer
*     supplied does not identify an Object of any sort.
*--
*/

/* Local Variables: */
   int valid;                    /* Valid object? */

/* Since this is the base class, the implementation of this function
   differs from that in derived classes (in that it fails and
   potentially reports an error if the returned result is zero). */

/* Initialise. */
   valid = 0;

/* Check if a NULL pointer was supplied (this can never be valid). If
   OK, check if the Object contains the correct "magic number" in its
   check field. */
   if ( !this || ( this->check != Magic( this, this->size, status ) ) ) {

/* If it is not a valid Object, then report an error (but only if the
   global error status has not already been set). */
      if ( astOK ) {
         astError( AST__OBJIN, "astIsAObject(%s): Invalid Object pointer "
                   "given (points at address %p).", status, astGetClass( this ),
                   (void *) this );
      }

/* Otherwise, note that the Object is valid. */
   } else {
      valid = 1;
   }

/* Return the result. */
   return valid;
}

void astInitObjectVtab_(  AstObjectVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitObjectVtab

*  Purpose:
*     Initialise a virtual function table for a Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     void astInitObjectVtab( AstObjectVtab *vtab, const char *name )

*  Class Membership:
*     Object vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Object class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int ivtab;                    /* Index of next entry in known_vtabs */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Initialise the contents of the class identifier. Since this is the
   base class, we assign null values to the fields. */
   vtab->id.check = NULL;
   vtab->id.parent = NULL;

/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->Clear = Clear;
   vtab->ClearAttrib = ClearAttrib;
   vtab->ClearID = ClearID;
   vtab->ClearIdent = ClearIdent;
   vtab->Dump = Dump;
   vtab->Equal = Equal;
   vtab->GetAttrib = GetAttrib;
   vtab->GetID = GetID;
   vtab->GetIdent = GetIdent;
   vtab->HasAttribute = HasAttribute;
   vtab->Same = Same;
   vtab->SetAttrib = SetAttrib;
   vtab->SetID = SetID;
   vtab->SetIdent = SetIdent;
   vtab->Show = Show;
   vtab->TestAttrib = TestAttrib;
   vtab->TestID = TestID;
   vtab->TestIdent = TestIdent;
   vtab->VSet = VSet;
   vtab->Cast = Cast;
   vtab->GetObjSize = GetObjSize;
   vtab->CleanAttribs = CleanAttribs;

   vtab->TestUseDefs = TestUseDefs;
   vtab->SetUseDefs = SetUseDefs;
   vtab->ClearUseDefs = ClearUseDefs;
   vtab->GetUseDefs = GetUseDefs;

#if defined(THREAD_SAFE)
   vtab->ManageLock = ManageLock;
#endif

/* Store the pointer to the class name. */
   vtab->class = name;

/* Initialise the count of active objects and the number of destructors,
   copy constructors and dump functions. */
   vtab->nobject = 0;
   vtab->ndelete = 0;
   vtab->ncopy = 0;
   vtab->ndump = 0;

/* Initialise the arrays of destructor, copy constructor and dump
   function pointers. */
   vtab->delete = NULL;
   vtab->copy = NULL;
   vtab->dump = NULL;
   vtab->dump_class = NULL;
   vtab->dump_comment = NULL;

/* The virtual function table for each class contains a list of pointers
   to memory blocks which have previously been used to store an Object of
   the same class, but which have since been deleted using astDelete.
   These memory blocks are free to be re-used when a new Object of the
   same class is initialised. This saves on the overheads associated with
   continuously allocating small blocks of memory using malloc. */
   vtab->nfree = 0;
   vtab->free_list = NULL;

/* Add the supplied virtual function table pointer to the end of the list
   of known vtabs. */
   ivtab = nvtab++;

   astBeginPM;
   known_vtabs = astGrow( known_vtabs, nvtab, sizeof( AstObjectVtab *) );
   astEndPM;

   if( astOK && known_vtabs ) known_vtabs[ ivtab ] = vtab;

/* Fill a pointer value with zeros (not necessarily the same thing as a
   NULL pointer) for subsequent use. */
   (void) memset( &zero_ptr, 0, sizeof( AstObject * ) );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised. */
   if( vtab == &class_vtab ) class_init = 1;
}

AstObject *astInitObject_( void *mem, size_t size, int init,
                           AstObjectVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitObject

*  Purpose:
*     Initialise an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astInitObject( void *mem, size_t size, int init,
*                               AstObjectVtab *vtab, const char *name )

*  Class Membership:
*     Object initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Object. It allocates memory (if necessary) to accommodate the
*     Object plus any additional data associated with the derived class. It
*     then initialises an Object structure at the start of this memory. If the
*     "init" flag is set, it also initialises the contents of a virtual
*     function table for an Object at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Object is to be initialised.
*        This must be of sufficient size to accommodate the Object data
*        (sizeof(Object)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Object (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Object
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Object's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Object.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).

*  Returned Value:
*     A pointer to the new Object.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstObject *new;               /* Pointer to new Object */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Determine if memory must be allocated dynamically. If so, use the last
   block of memory in the list of previously allocated but currently
   unused blocks identified by the vtab "free_list" array, reducing the
   length of the free list by one, and nullifying the entry in the list
   for safety. If the list is originally empty, allocate memory for a new
   object using astMalloc. */
   if( !mem ) {
      if( object_caching ) {
         if( vtab->nfree > 0 ) {
            mem = vtab->free_list[ --(vtab->nfree) ];
            vtab->free_list[ vtab->nfree ] = NULL;
            if( astSizeOf( mem ) != size && astOK ) {
               astError( AST__INTER, "astInitObject(%s): Free block has size "
                         "%d but the %s requires %d bytes (internal AST "
                         "programming error).", status, vtab->class,
                         (int) astSizeOf( mem ), vtab->class, (int) size );
            }
         } else {
            mem = astMalloc( size );
         }

      } else {
         mem = astMalloc( size );
      }

/* If memory had already been allocated, adjust the "size" value to match
   the size of the allocated memory. */
   } else {
      size = astSizeOf( mem );
   }

/* Obtain a pointer to the new Object. */
   if ( astOK ) {
      new = (AstObject *) mem;

/* Zero the entire new Object structure (to prevent accidental re-use
   of any of its values after deletion). */
      (void) memset( new, 0, size );

/* If necessary, initialise the virtual function table. */
/* ---------------------------------------------------- */
      if ( init ) astInitObjectVtab( vtab, name );
      if( astOK ) {

/* Initialise the Object data. */
/* --------------------------- */
/* Store a unique "magic" value in the Object structure. This will be
   used (e.g. by astIsAObject) to determine if a pointer identifies a
   valid Object. Note that this differs from the practice in derived
   classes, where this number is stored in the virtual function
   table. We take a different approach here so that we need not follow
   a pointer to the virtual function table obtained from a structure
   that hasn't yet been validated as an Object. This minimises the
   risk of a memory access violation. */
         new->check = Magic( new, size, status );

/* Associate the Object with its virtual function table. */
         new->vtab = vtab;

/* Store the Object size and note if its memory was dynamically allocated. */
         new->size = size;
         new->dynamic = astIsDynamic( new );

/* Initialise the reference count (of Object pointers in use). */
         new->ref_count = 1;

/* Initialise the ID strings. */
         new->id = NULL;
         new->ident = NULL;

/* Use default values for unspecified attributes. */
         new->usedefs = CHAR_MAX;

/* Increment the count of active Objects in the virtual function table.
   Use the count as a unique identifier (unique within the class) for
   the Object. */
         new->iref = vtab->nobject++;

/* Initialise the pointer to an external object that acts as a proxy for
   the AST Object within foreign language interfaces. */
         new->proxy = NULL;
      }

/* If an error occurred, clean up by deleting the new Object. Otherwise
   lock the object for use by the currently executing thread. */
      if ( !astOK ) {
         new = astDelete( new );

#ifdef THREAD_SAFE
      } else {
         if( pthread_mutex_init( &(new->mutex1), NULL ) != 0 && astOK ) {
            astError( AST__INTER, "astInitObject(%s): Failed to "
                      "initialise POSIX mutex1 for the new Object.", status,
                      vtab->class );
         }
         if( pthread_mutex_init( &(new->mutex2), NULL ) != 0 && astOK ) {
            astError( AST__INTER, "astInitObject(%s): Failed to "
                      "initialise POSIX mutex2 for the new Object.", status,
                      vtab->class );
         }
         new->locker = -1;
         new->globals = NULL;
         (void) ManageLock( new, AST__LOCK, 0, NULL, status );
         if( !astOK ) new = astDelete( new );
#endif
      }
   }

/* Return a pointer to the new Object. */
   return new;
}

AstObject *astLoadObject_( void *mem, size_t size,
                           AstObjectVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadObject

*  Purpose:
*     Load an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astLoadObject( void *mem, size_t size,
*                               AstObjectVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     Object loader.

*  Description:
*     This function is provided to load a new Object using data read
*     from a Channel, and to allocate memory for it if necessary.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for an Object at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Object is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Object data (sizeof(Object)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Object (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Object structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstObject) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Object. If this is NULL, a pointer to
*        the (static) virtual function table for the Object class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new Object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Object" is used instead.

*  Returned Value:
*     A pointer to the new Object.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   AstObject *new;               /* Pointer to the new Object */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Object. In this case the
   Object belongs to this class, so supply appropriate values for
   initialising it and its virtual function table. */
   if ( !vtab ) {
      size = sizeof( AstObject );
      vtab = &class_vtab;
      name = "Object";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitObjectVtab( vtab, name );
         class_init = 1;
      }
   }

/* There is no parent class to load, so simply initialise a new Object
   structure as if a new Object were being created from scratch. */
   new = astInitObject( mem, size, 0, vtab, name );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Object" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */
      new->id = astReadString( channel, "id", NULL );
      new->ident = astReadString( channel, "ident", NULL );
      new->usedefs = astReadInt( channel, "usedfs", CHAR_MAX );

/* We simply read the values for the read-only attributes (just in
   case they've been un-commented in the external representation) and
   discard them. This prevents any possibility of error due to un-read
   input. */
      (void) astReadInt( channel, "refcnt", 0 );
      (void) astReadInt( channel, "nobj", 0 );

/* Initialise the pointer to an external object that acts as a proxy for
   the AST Object within foreign language interfaces. */
      new->proxy = NULL;

/* If an error occurred, clean up by deleting the new Object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Object pointer. */
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
void astClear_( AstObject *this, const char *attrib, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,Clear))( this, attrib, status );
}
void astClearAttrib_( AstObject *this, const char *attrib, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,ClearAttrib))( this, attrib, status );
}
void astDump_( AstObject *this, AstChannel *channel, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,Dump))( this, channel, status );
}

#if defined(THREAD_SAFE)
int astManageLock_( AstObject *this, int mode, int extra, AstObject **fail,
                    int *status ) {
   if( !this ) return 0;
   return (**astMEMBER(this,Object,ManageLock))( this, mode, extra, fail, status );
}
#endif

int astEqual_( AstObject *this, AstObject *that, int *status ) {
   if ( !astOK ) return 0;
   if( this == that ) return 1;
   return (**astMEMBER(this,Object,Equal))( this, that, status );
}
const char *astGetAttrib_( AstObject *this, const char *attrib, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Object,GetAttrib))( this, attrib, status );
}
void astSetAttrib_( AstObject *this, const char *setting, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,SetAttrib))( this, setting, status );
}
void astShow_( AstObject *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,Show))( this, status );
}
int astTestAttrib_( AstObject *this, const char *attrib, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Object,TestAttrib))( this, attrib, status );
}
void astVSet_( AstObject *this, const char *settings, char **text, va_list args, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,VSet))( this, settings, text, args, status );
}
int astGetObjSize_( AstObject *this, int *status ) {
   if ( !astOK || !this ) return 0;
   return (**astMEMBER(this,Object,GetObjSize))( this, status );
}
void astCleanAttribs_( AstObject *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Object,CleanAttribs))( this, status );
}
AstObject *astCast_( AstObject *this, AstObject *obj, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Object,Cast))( this, obj, status );
}
int astSame_( AstObject *this, AstObject *that, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Object,Same))( this, that, status );
}
int astHasAttribute_( AstObject *this, const char *attrib, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Object,HasAttribute))( this, attrib, status );
}

/* External interface. */
/* =================== */
/* The following relates to the external interface to Objects and not
   specifically to the implementation of the Object class itself
   (although it contains external functions which replace the internal
   versions defined earlier). */


/* Type Definitions. */
/* ----------------- */
/* Define the Handle structure. This is attached to Objects when they
   are accessed via the public (external, user-callable) interface.
   Handles provide a buffer between the Object identifiers issued to
   external users and the naked C pointers used to handle Objects
   internally. They also implement the context levels used by
   astBegin, astEnd, astExempt and astExport (which are only available
   to external users). */
typedef struct Handle {
   AstObject *ptr;               /* C Pointer to the associated Object */
   int context;                  /* Context level for this Object */
   int check;                    /* Check value to ensure validity */

#if defined(THREAD_SAFE)
   int thread;                   /* Identifier for owning thread */
#endif

#if defined(MEM_DEBUG)
   int id;                       /* The id associated with the memory block
                                    holding the Object last associated with
                                    this handle. */
   AstObjectVtab *vtab;          /* Pointer to the firtual function table of
                                    the Object last associated with this
                                    handle. */
#endif

/* Links between Handles are implemented using integer offsets rather
   than through pointers. */
   int flink;                    /* Backward link to previous Handle */
   int blink;                    /* Forward link to next Handle */
} Handle;

/* Define a union with an overlapping int and AstObject*. This is used
   to transfer an integer bit pattern into and out of a pointer
   variable used to store an Object identifier. This avoids any
   implementation dependent aspects of integer-to-pointer
   conversions. */
typedef union IdUnion {
   AstObject *pointer;
   int integer;
} IdUnion;

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int. */
typedef union MixedInts {
   int i;
   unsigned u;
} MixedInts;

/* Static Variables. */
/* ----------------- */
/* The array of Handle structures is a pool of resources available to all
   threads. Each thread has its own conext level and its own "active_handles"
   array to identify the first Handle at each context level. */
static Handle *handles = NULL; /* Pointer to allocated array of Handles */
static int free_handles = -1; /* Offset to head of free Handle list */
static int nhandles = 0; /* Number of Handles in "handles" array */
static unsigned nids = 0; /* Number of IDs issued to external users */

#if defined(THREAD_SAFE)
static int unowned_handles = -1; /* Offset to head of unowned Handle
                                    list. In a single threaded environment,
                                    all handles must be owned by a thread. */
#endif

#ifdef MEM_DEBUG
static int Watched_Handle = -1; /* A handle index to be watched. Activity
                                   on this handle is reported using
                                   astHandleUse and astHandleAlarm. */
#endif


/* External Interface Function Prototypes. */
/* --------------------------------------- */
/* MYSTATIC should normally be set to "static" to make the following
   function have local symbols. But it may be set to blank for debugging
   purposes in order to enable these functions to appear in a backtrace
   such as produced by the astBacktrace function. */
#define MYSTATIC

/* Private functions associated with the external interface. */
MYSTATIC AstObject *AssocId( int, int * );
MYSTATIC int CheckId( AstObject *, int, int * );
MYSTATIC void AnnulHandle( int, int * );
MYSTATIC void InitContext( int * );
MYSTATIC void InsertHandle( int, int *, int * );
MYSTATIC void RemoveHandle( int, int *, int * );

#if defined(MEM_DEBUG)
MYSTATIC void CheckList( int *, int * );
MYSTATIC void CheckInList( int, int *, int, int * );
MYSTATIC int CheckThread( int, int *, int * );
MYSTATIC const char *HandleString( int, char * );
MYSTATIC const char *HeadString( int *, char * );
#endif


/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstObject *astDeleteId_( AstObject *, int * );
void astBegin_( void );
void astEnd_( int * );
void astExemptId_( AstObject *, int * );
void astImportId_( AstObject *, int * );
void astSetId_( void *, const char *, ... );
void astLockId_( AstObject *, int, int * );
void astUnlockId_( AstObject *, int, int * );


/* External Interface Functions. */
/* ----------------------------- */
MYSTATIC void AnnulHandle( int ihandle, int *status ) {
/*
*  Name:
*     AnnulHandle

*  Purpose:
*     Annul a Handle and its associated Object pointer.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void AnnulHandle( int ihandle, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function annuls an active Handle by annulling the Object
*     pointer it is associated with, deactivating the Handle and
*     returning it to the free Handle list for re-use.
*
*     The reference count for the associated Object is decremented by
*     this function and the Object will be deleted if this reference
*     count falls to zero.

*  Parameters:
*     ihandle
*        Array offset that identifies the Handle to be annulled in the
*        "handles" array. This is fully validated by this function.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The Handle supplied should be active, otherwise an error will
*     result and the Handle will not be touched (but no error report
*     will be made if the global error status has already been set).
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   AstObject *ptr;               /* Object pointer */
   int context;                  /* Context level where Handle was issued */

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Check that the handle offset supplied is valid and report an error
   if it is not (but only if the global error status has not already
   been set). */
   if ( ( ihandle < 0 ) || ( ihandle >= nhandles ) ) {
      if ( astOK ) {
         astError( AST__INHAN, "astAnnulHandle: Invalid attempt to annul an "
                   "Object Handle (no. %u).", status, ihandle );
         astError( AST__INHAN, "This Handle number is not valid (possible "
                   "internal programming error)." , status);
      }

/* If OK, obtain the Handle's context level. */
   } else {
      context = handles[ ihandle ].context;

/* If this indicates that the Handle isn't active, then report an
   error (but only if the global error status has not already been
   set). We allow handles that are currently not owned by any thread to
   be annulled. */
      if ( context < 0 && context != UNOWNED_CONTEXT ) {
         if ( astOK ) {
            astError( AST__INHAN, "astAnnulHandle: Invalid attempt to annul "
                      "an Object Handle (no. %u).", status, ihandle );
            astError( AST__INHAN, "This Handle is not active (possible "
                      "internal programming error)." , status);
         }

/* If the Handle is active, annul its Object pointer. The astAnnul
   function may call Delete functions supplied by any class, and these
   Delete functions may involve annulling external Object IDs, which in
   turn requires access to the handles array. For this reason, we release
   the mutex that protects access to the handles arrays so that it can
   potentially be re-aquired within astAnnul without causing deadlock. */
      } else {

#ifdef MEM_DEBUG
         astHandleUse( ihandle, "annulled using check value %d ",
                       handles[ ihandle ].check  );
#endif

         ptr = handles[ ihandle ].ptr;
         UNLOCK_MUTEX2;
         ptr = astAnnul( ptr );
         LOCK_MUTEX2;

/* Remove the Handle from the active list for its context level. */
         if( context == UNOWNED_CONTEXT ) {

#if defined(THREAD_SAFE)
            RemoveHandle( ihandle, &unowned_handles, status );
#else
            if( astOK ) astError( AST__INTER, "AnnulHandle: reference to "
                         "'unowned_handles' in a non-thread-safe context "
                         "(internal AST programming error).", status );
#endif

         } else if( active_handles ) {
            RemoveHandle( ihandle, &active_handles[ context ], status );

         } else if( astOK ){
            astError( AST__INTER, "AnnulHandle: active_handles array has "
                      "not been initialised (internal AST programming error).",
                      status );
         }

/* Reset the Handle's "context" value (making it inactive) and its "check"
   value (so it is no longer associated with an identifier value). */
         handles[ ihandle ].ptr = NULL;
         handles[ ihandle ].context = INVALID_CONTEXT;
         handles[ ihandle ].check = 0;
#if defined(THREAD_SAFE)
         handles[ ihandle ].thread = -1;
#endif

/* Place the modified Handle on the free Handles list ready for re-use. */
         InsertHandle( ihandle, &free_handles, status );

      }
   }
}

AstObject *astAnnulId_( AstObject *this_id, int *status ) {
/*
*+
*  Name:
*     AnnulId

*  Purpose:
*     Annul an external Object identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astAnnulId( AstObject *this )

*  Class Membership:
*     Object member function.

*  Description:
*     This function implements the external (public) interface to the
*     astAnnul method. It accepts an active Object identifier, which
*     it annuls, causing the associated Handle to be deactivated and
*     returned to the free Handles list for re-use. It also causes the
*     reference count of the associated Object to be decremented and
*     the Object to be deleted if this reference count falls to zero.

*  Parameters:
*     this
*        The Object identifier to be annulled.

*  Returned Value:
*     A NULL C pointer is always returned (this should be translated
*     into an identifier value of zero for external use).

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set.
*     - The identifier supplied should be associated with an active
*     Object, otherwise an error will result (but no error report will
*     be made if the global error status has already been set).
*     - This function is invoked via the astAnnul macro for external use.
*     For internal use (from protected code which needs to handle external
*     IDs) it should be invoked via the astAnnulId macro (since astAnnul
*     expects a true C pointer as its argument when used internally).
*-
*/

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object (this generates an
   error if it doesn't). Note, we use "astMakePointer_NoLockCheck",
   rather than the usual "astMakePointer" since a thread should be able
   to renounce interest in an object without needing to own the object.
   If we used "astMakePointer" then a thread could not annul a pointer
   unless it owned the object. But having annulled the pointer it could
   then not unlock the object for use by another thread (since the
   pointer it would need to do this has just been annulled). */
   if ( !astIsAObject( astMakePointer_NoLockCheck( this_id ) ) ) return NULL;

/* Obtain the Handle offset for this Object and annul the Handle and
   its associated Object pointer. */
   LOCK_MUTEX2;
   AnnulHandle( CheckId( this_id, 0, status ), status );
   UNLOCK_MUTEX2;

/* Always return a NULL pointer value. */
   return NULL;
}

MYSTATIC AstObject *AssocId( int ihandle, int *status ) {
/*
*  Name:
*     AssocId

*  Purpose:
*     Associate an identifier value with a Handle.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *AssocId( int ihandle )

*  Class Membership:
*     Object member function.

*  Description:
*     This function takes a zero-based offset into the "handles" array
*     that identifies a Handle associated with an active Object. It
*     encodes this into an identifier value to be issued to an
*     external user to identify that Handle and its Object, and then
*     associates this identifier value with the Handle.

*  Parameters:
*     ihandle
*        Offset in the "handles" array that identifies the Handle,
*        which should be active (i.e. associated with an active
*        Object). This function will modify the "check" field in this
*        Handle to associate it with the identifier value it returns.

*  Returned Value:
*     The resulting identifier value.

*  Notes:
*     - The returned identifier value is, in fact, an int.  This
*     allows the value to be passed easily to other languages
*     (e.g. Fortran) and stored as an integer without loss of
*     information.
*     - The value is stored within C code as type AstObject*.  This
*     means that C code (e.g. function prototypes, etc.) need not be
*     aware that an identifier (as opposed to an Object pointer) is
*     being used, even though the actual bit patterns will be
*     different. The same C code can then be used for both internal
*     and external purposes so long as care is taken to convert
*     between the two representations at appropriate points.
*     - The reverse operation (conversion of an ID back to a handle
*     offset) is performed by CheckId.
*     - A zero identifier value will be returned if this function is
*     invoked with the global status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstObject *result;            /* Pointer value to return */
   MixedInts test;               /* Union for testing encoding */
   MixedInts work;               /* Union for encoding ID value */

/* Initialise. */
   result = astI2P( 0 );

/* Check the global error status. */
   if ( !astOK ) return result;

/* Copy the Handle offset and clear the lowest 8 bits by shifting
   left. */
   work.i = ihandle;
   work.u = work.u << 8U;

/* Make a copy of the result shifted right again. Test if any bits
   have been lost in this process. If so, there are too many Handles
   in use at once to encode them into IDs, so report an error. */
   test.u = work.u >> 8U;
   if ( test.i != ihandle ) {
      astError( AST__XSOBJ, "AssocId(%s): There are too many AST Objects in "
                "use at once.", status, astGetClass( handles[ ihandle ].ptr ) );

/* If OK, scramble the value by exclusive-ORing with the bit pattern
   in AST__FAC (a value unique to this library), also shifted left by
   8 bits. This makes it even less likely that numbers from other
   sources will be accepted in error as valid IDs. */
   } else {
      work.u ^= ( ( (unsigned) AST__FAC ) << 8U );

/* Fill the lowest 8 bits with a count of the total number of IDs
   issued so far (which we increment here). This makes each ID unique,
   so that an old one that identifies a Handle that has been annulled
   and re-used (i.e. associated with a new ID) can be spotted.  We
   only use the lowest 8 bits of this count because this provides
   adequate error detection to reveal programming errors and we do not
   need higher security than this. We also prevent a count of zero
   being used, as this could result in a zero identifier value (this
   being reserved as the "null" value). */
      if ( ++nids > 255U ) nids = 1U;
      work.u |= nids;

/* Store the value as a check count in the Handle. This will be used
   to validate the ID in future. */
      handles[ ihandle ].check = work.i;

/* Pack the value into the pointer to be returned. */
      result = astI2P( work.i );
   }

/* Return the result. */
   return result;
}

void astBegin_( void ) {
/*
*++
*  Name:
c     astBegin
f     AST_BEGIN

*  Purpose:
*     Begin a new AST context.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astBegin
f     CALL AST_BEGIN( STATUS )

*  Class Membership:
*     Object class function.

*  Description:
c     This macro invokes a function to begin a new AST context.
c     Any Object pointers
f     This routine begins a new AST context. Any Object pointers
*     created within this context will be annulled when it is later
c     ended using astEnd (just as if astAnnul had been invoked),
f     ended using AST_END (just as if AST_ANNUL had been invoked),
c     unless they have first been exported using astExport or rendered
c     exempt using astExempt. If
f     unless they have first been exported using AST_EXPORT or rendered
f     exempt using AST_EXEMPT. If
*     annulling a pointer causes an Object's RefCount attribute to
*     fall to zero (which happens when the last pointer to it is
*     annulled), then the Object will be deleted.

f  Parameters:
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This macro applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - astBegin attempts to execute even if the AST error status
c     is set on entry.
f     - This routine attempts to execute even if STATUS is set to an
f     error value.
c     - Contexts delimited by astBegin and astEnd may be nested to any
c     depth.
f     - Contexts delimited by AST_BEGIN and AST_END may be nested to any
f     depth.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int stat;                     /* Copy of global status */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Now that the thread-specific data has been initialised, we can get a
   pointer to the threads inherited status value. */
   status = astGetStatusPtr;

/* Save and clear the global status so that memory allocation can be
   performed within this function even under error conditions. */
   stat = astStatus;
   astClearStatus;

/* Ensure that the active_handles array has been initialised. */
   if ( !active_handles ) InitContext( status );

/* Extend the "active handles" array to accommodate a new context
   level. This array contains integer offsets into the "handles" array
   to identify the handle which is at the head of the list of active
   handles for each context level. */
   astBeginPM;
   active_handles = astGrow( active_handles, context_level + 2,
                             sizeof( int ) );
   astEndPM;

/* Initialise the array element for the new context level to indicate
   an empty Handle list. */
   if ( astOK ) active_handles[ ++context_level ] = -1;

/* Restore the original global status value. */
   astSetStatus( stat );
}

MYSTATIC int CheckId( AstObject *this_id, int lock_check, int *status ) {
/*
*  Name:
*     CheckId

*  Purpose:
*     Check an identifier value and convert it into a Handle offset.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     int CheckId( AstObject *this, int lock_check, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function takes an identifier value encoded by AssocId and
*     validates it to ensure it is associated with an active
*     Handle. If valid, it converts it back into a zero-based offset
*     which may be used to access the Handle in the "handles"
*     array. Otherwise, an error is reported.

*  Parameters:
*     this
*        The identifier value to be decoded.
*     lock_check
*        Should an error be reported if the handle is in an Object
*        context for a different thread?
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The resulting Handle offset, or -1 if the identifier is not
*     valid or any other error occurs.

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set, but no further error report will be made if it
*     fails under these circumstances.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   MixedInts work;               /* Union for decoding ID value */
   int id;                       /* ID value as an int */
   int ihandle;                  /* Result to return */

#ifdef MEM_DEBUG
   int oldok = astOK;
#endif

/* Initialise. */
   ihandle = -1;

/* Get a pointer to thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Retrieve the integer Object identifier value from the pointer
   supplied. */
   id = astP2I( this_id );

/* Check if a value of zero has been supplied and report an error if
   it has. */
   if ( !id ) {
      if ( astOK ) {
         astError( AST__OBJIN, "Invalid Object pointer given (value is "
                   "zero)." , status);
      }

/* If OK, reverse the encoding process performed by AssocId to
   retrieve the Handle offset. */
   } else {
      work.i = id;
      work.u = ( work.u ^ ( ( (unsigned) AST__FAC ) << 8U ) ) >> 8U;

/* Check that the offset obtained doesn't extend beyond the limits of
   the "handles" array. Report an error if it does. */
      if ( ( work.i < 0 ) || ( work.i >= nhandles ) ) {
         if ( astOK ) {
            astError( AST__OBJIN, "Invalid Object pointer given (value is "
                      "%d).", status, id );
         }

/* See if the "check" field matches the ID value and the Handle is
   valid (i.e. is associated with an active Object). If not, the
   Handle has been annulled and possibly re-used, so report an
   error. */
      } else if ( ( handles[ work.i ].check != id ) ||
                  ( handles[ work.i ].context == INVALID_CONTEXT ) ) {
         if ( astOK ) {
            astError( AST__OBJIN, "Invalid Object pointer given (value is "
                      "%d).", status, id  );
            astError( AST__OBJIN, "This pointer has been annulled, or the "
                      "associated Object deleted." , status);
         }
#if defined(THREAD_SAFE)
      } else if(  lock_check && handles[ work.i ].context != UNOWNED_CONTEXT &&
                                handles[ work.i ].thread != AST__THREAD_ID ) {
         if ( astOK ) {
            astError( AST__OBJIN, "Invalid Object pointer given (value is "
                      "%d).", status, id  );
            astError( AST__OBJIN, "This pointer is currently locked by "
                      "another thread." , status);
         }
#endif

/* If OK, set the Handle offset to be returned. */
      } else {
         ihandle = work.i;
      }

#ifdef MEM_DEBUG
      if ( oldok && !astOK && ( work.i >= 0 ) && ( work.i < nhandles ) ) {
         char buf[200];
         astError( astStatus, "Handle properties: %s ", status,
                   HandleString( work.i, buf ) );
      }
#endif

   }

/* Return the result. */
   return ihandle;
}

AstObject *astDeleteId_( AstObject *this_id, int *status ) {
/*
*+
*  Name:
*     astDeleteId

*  Purpose:
*     Delete an Object via an identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astDeleteId_( AstObject *this )

*  Class Membership:
*     Object member function.

*  Description:
*     This function implements the external (public) interface to the
*     astDelete method. It accepts an active Object identifier and
*     deletes the associated Object. Before doing so, it annuls all
*     active identifiers associated with the object, deactivates their
*     Handles and returns these Handles to the free Handles list for
*     re-use.

*  Parameters:
*     this
*        An identifier for the Object to be deleted.

*  Returned Value:
*     A NULL C pointer is always returned (this should be translated
*     into an identifier value of zero for external use).

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set.
*     - The identifier supplied should be associated with an active
*     Object, otherwise an error will result (but no error report will
*     be made if the global error status has already been set).
*     - Although this function is documented as "private" and should
*     not be invoked directly from outside this class, it is not a
*     static function and has a public prototype. This is because it
*     must be invoked via the astDelete macro (defined in the
*     "object.h" include file) as part of the public interface.
*-
*/

/* Local Variables: */
   AstObject *this;              /* Pointer to Object */
   int i;                        /* Loop counter for Handles */
   int ihandle;                  /* Object Handle offset */

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object (this generates an
   error if it doesn't). */
   if ( !astIsAObject( this = astMakePointer( this_id ) ) ) return NULL;

/* Gain exclusive access to the handles array. */
   LOCK_MUTEX2;

/* Obtain the Handle offset for this Object. */
   ihandle = CheckId( this_id, 1, status );
   if ( ihandle != -1 ) {

/* Since the Object is to be deleted, we must annul all identifiers
   that refer to it.  Loop to inspect each currently allocated Handle
   in the "handles" array. */
      for ( i = 0; i < nhandles; i++ ) {

/* Select active handles and test if their Object pointer refers to
   the Object to be deleted. */
         if ( ( handles[ i ].context != INVALID_CONTEXT ) &&
              ( handles[ i ].ptr == this ) ) {

/* If so, explicitly set the reference count for the Object to 2 so
   that it will not be deleted (yet) when we annul the pointer
   associated with the Handle. */
            handles[ i ].ptr->ref_count = 2;

/* Annul the Handle, which frees its resources, decrements the Object
   reference count and makes any ID associated with the Handle become
   invalid. */
            AnnulHandle( i, status );
         }
      }

/* If required, tell the user that the handle's object has been deleted. */
#ifdef MEM_DEBUG
      astHandleUse( ihandle, "object-deleted" );
#endif
   }

   UNLOCK_MUTEX2;

/* When all Handles associated with the Object have been annulled,
   delete the object itself. This over-rides the reference count and
   causes any remaining pointers to the Object (e.g. in internal code
   or within other Objects) to become invalid. */
   this = astDelete( this );

/* Always return a NULL pointer value. */
   return NULL;
}

void astEnd_( int *status ) {
/*
*++
*  Name:
c     astEnd
f     AST_END

*  Purpose:
*     End an AST context.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astEnd
f     CALL AST_END( STATUS )

*  Class Membership:
*     Object class function.

*  Description:
c     This macro invokes a function to end an AST context which was
f     This routine ends an AST context which was
c     begun with a matching invocation of astBegin. Any Object
f     begun with a matching invocation of AST_BEGIN. Any Object
*     pointers created within this context will be annulled (just as
c     if astAnnul had been invoked) and will cease to be valid
f     if AST_ANNUL had been invoked) and will cease to be valid
*     afterwards, unless they have previously been exported using
c     astExport or rendered exempt using astExempt.
f     AST_EXPORT or rendered exempt using AST_EXEMPT.
*     If annulling a pointer causes an Object's RefCount attribute to
*     fall to zero (which happens when the last pointer to it is
*     annulled), then the Object will be deleted.

*  Parameters:
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This macro applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - astEnd attempts to execute even if the AST error status is set.
f     - This routine attempts to execute even if STATUS is set to an
f     error value.
c     - Contexts delimited by astBegin and astEnd may be nested to any
c     depth.
f     - Contexts delimited by AST_BEGIN and AST_END may be nested to any
f     depth.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int ihandle;                  /* Offset of Handle to be annulled */

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Check that the current context level is at least 1, otherwise there
   has been no matching use of astBegin, so report an error (unless
   the global status has already been set). */
   if ( context_level < 1 ) {
      if ( astOK ) {
         astError( AST__ENDIN, "astEnd: Invalid use of astEnd without a "
                   "matching astBegin." , status);
      }

/* If OK, loop while there are still active Handles associated with
   the current context level. First gain exclusive access to the handles
   array. */
   } else if ( active_handles ) {
      LOCK_MUTEX2;
      while ( ( ihandle = active_handles[ context_level ] ) != -1 ) {

/* Annul the Handle at the head of the active Handles list. */
         AnnulHandle( ihandle, status );

/* It is just posible that under error conditions inactive Handles
   might get left in the active_handles list and AnnulHandle would
   then fail. Since this would result in an infinite loop, we check to
   see if the handle we have just annulled is still in the list. If
   so, transfer it to the free Handles list for re-use. */
         if ( ihandle == active_handles[ context_level ] ) {
            RemoveHandle( ihandle, &active_handles[ context_level ], status );
            InsertHandle( ihandle, &free_handles, status );
         }
      }

/* Ensure the context level is decremented unless it was zero to start
   with. */
      context_level--;

/* Relinquish access to the handles array. */
      UNLOCK_MUTEX2;
   }

}

void astExemptId_( AstObject *this_id, int *status ) {
/*
*++
*  Name:
c     astExempt
f     AST_EXEMPT

*  Purpose:
*     Exempt an Object pointer from AST context handling.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astExempt( AstObject *this )
f     CALL AST_EXEMPT( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function exempts an Object pointer from AST context
f     This routine exempts an Object pointer from AST context
c     handling, as implemented by astBegin and astEnd. This means that
f     handling, as implemented by AST_BEGIN and AST_END. This means that
c     the pointer will not be affected when astEnd is invoked and will
f     the pointer will not be affected when AST_END is called and will
*     remain active until the end of the program, or until explicitly
c     annulled using astAnnul.
f     annulled using AST_ANNUL.
*
c     If possible, you should avoid using this function when writing
f     If possible, you should avoid using this routine when writing
*     applications. It is provided mainly for developers of other
*     libraries, who may wish to retain references to AST Objects in
*     internal data structures, and who therefore need to avoid the
c     effects of astBegin and astEnd.
f     effects of AST_BEGIN and AST_END.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Object pointer to be exempted from context handling.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.
*--

*  Implementation Notes:
*     - This function does not exist in the "protected" interface to
*     the Object class and is not available to other class
*     implementations.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int context;                  /* Handle context level */
   int ihandle;                  /* Offset of Handle in "handles" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object. */
   (void) astCheckObject( astMakePointer( this_id ) );
   if ( astOK ) {

/* Gain exclusive access to the handles array. */
      LOCK_MUTEX2;

/* Obtain the Handle offset for this Object. */
      ihandle = CheckId( this_id, 1, status );
      if ( ihandle != -1 ) {

/* Extract the context level at which the Object was created. */
         context = handles[ ihandle ].context;

/* Set the new context level to zero, where it cannot be affected by
   ending any context. */
         handles[ ihandle ].context = 0;

/* Remove the object's Handle from its original active Handles list
   and insert it into the list appropriate to its new context
   level. */

#if defined(THREAD_SAFE)
         if( context == UNOWNED_CONTEXT ) {
            RemoveHandle( ihandle, &unowned_handles, status );
         } else {
            RemoveHandle( ihandle, &active_handles[ context ], status );
         }
#else
         RemoveHandle( ihandle, &active_handles[ context ], status );
#endif

         InsertHandle( ihandle, &active_handles[ 0 ], status );

/* If required, tell the user that the handle has been exempted. */
#ifdef MEM_DEBUG
         astHandleUse( ihandle, "exempted" );
#endif
      }

      UNLOCK_MUTEX2;
   }
}

void astExportId_( AstObject *this_id, int *status ) {
/*
*++
*  Name:
c     astExport
f     AST_EXPORT

*  Purpose:
*     Export an Object pointer to an outer context.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astExport( AstObject *this )
f     CALL AST_EXPORT( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function exports an Object pointer from the current AST context
f     This routine exports an Object pointer from the current AST context
*     into the context that encloses the current one. This means that
*     the pointer will no longer be annulled when the current context
c     is ended (with astEnd), but only when the next outer context (if
f     is ended (with AST_END), but only when the next outer context (if
*     any) ends.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Object pointer to be exported.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
c     - It is only sensible to apply this function to pointers that
f     - It is only sensible to apply this routine to pointers that
*     have been created within (or exported to) the current context
c     and have not been rendered exempt using astExempt.
f     and have not been rendered exempt using AST_EXEMPT.
*     Applying it to an unsuitable Object pointer has no effect.
*--

*  Implementation Notes:
*     - This function does not exist in the "protected" interface to
*     the Object class and is not available to other class
*     implementations.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int context;                  /* Handle context level */
   int ihandle;                  /* Offset of Handle in "handles" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object. */
   (void) astCheckObject( astMakePointer( this_id ) );
   if ( astOK ) {

/* Gain exclusive access to the handles array. */
      LOCK_MUTEX2;

/* Obtain the Handle offset for this Object. */
      ihandle = CheckId( this_id, 1, status );
      if ( ihandle != -1 ) {

/* Check that the current context level is at least 1 and report an
   error if it is not. */
         if ( context_level < 1 ) {
            if( astOK ) astError( AST__EXPIN, "astExport(%s): Attempt to export an Object "
                                  "from context level zero.", status,
                                  astGetClass( handles[ ihandle ].ptr ) );

/* Extract the context level at which the Object was created. */
         } else {
            context = handles[ ihandle ].context;

/* Check that the Object's existing context level is high enough to be
   affected by being exported to the next outer context level. If not,
   do nothing. */
            if ( context > ( context_level - 1 ) ) {

/* Set the new context level. */
               handles[ ihandle ].context = context_level - 1;

/* Remove the object's Handle from its original active Handles list
   and insert it into the list appropriate to its new context
   level. */
               RemoveHandle( ihandle, &active_handles[ context ], status );
               InsertHandle( ihandle, &active_handles[ context_level - 1 ],
                             status );

/* If required, tell the user that the handle has been exempted. */
#ifdef MEM_DEBUG
               astHandleUse( ihandle, "exported from context level %d",
                              context_level );
#endif
            }
         }
      }
      UNLOCK_MUTEX2;
   }
}

void astImportId_( AstObject *this_id, int *status ) {
/*
*++
*  Name:
c     astImport
f     AST_IMPORT

*  Purpose:
*     Import an Object pointer to the current context.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     void astImport( AstObject *this )
f     CALL AST_IMPORT( THIS, STATUS )

*  Class Membership:
*     Object method.

*  Description:
c     This function
f     This routine
*     imports an Object pointer that was created in a higher or lower
*     level context, into the current AST context.
*     This means that the pointer will be annulled when the current context
c     is ended (with astEnd).
f     is ended (with AST_END).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Object pointer to be imported.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Object
c        This function applies to all Objects.
f        This routine applies to all Objects.

*--

*  Implementation Notes:
*     - This function does not exist in the "protected" interface to
*     the Object class and is not available to other class
*     implementations.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   int context;                  /* Handle context level */
   int ihandle;                  /* Offset of Handle in "handles" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object. */
   (void) astCheckObject( astMakePointer( this_id ) );
   if ( astOK ) {

/* Gain exclusive access to the handles array. */
      LOCK_MUTEX2;

/* Obtain the Handle offset for this Object. */
      ihandle = CheckId( this_id, 1, status );
      if ( ihandle != -1 ) {

/* Extract the context level at which the Object was created. */
         context = handles[ ihandle ].context;

/* Do nothing if the Identifier already belongs to the current context. */
         if( context != context_level ) {

/* Set the new context level. */
            handles[ ihandle ].context = context_level;

/* Remove the object's Handle from its original active Handles list
   and insert it into the list appropriate to its new context
   level. */
            RemoveHandle( ihandle, &active_handles[ context ], status );
            InsertHandle( ihandle, &active_handles[ context_level ], status );

/* If required, tell the user that the handle has been imported. */
#ifdef MEM_DEBUG
            astHandleUse( ihandle, "imported into context level %d",
                           context_level );
#endif
         }
      }
      UNLOCK_MUTEX2;
   }
}

void astLockId_( AstObject *this_id, int wait, int *status ) {
/*
c++
*  Name:
*     astLock

*  Purpose:
*     Lock an Object for exclusive use by the calling thread.

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     void astLock( AstObject *this, int wait )

*  Class Membership:
*     Object method.

*  Description:
*     The thread-safe public interface to AST is designed so that an
*     error is reported if any thread attempts to use an Object that it
*     has not previously locked for its own exclusive use using this
*     function. When an Object is created, it is initially locked by the
*     thread that creates it, so newly created objects do not need to be
*     explicitly locked. However, if an Object pointer is passed to
*     another thread, the original thread must first unlock it (using
*     astUnlock) and the new thread must then lock it (using astLock)
*     before the new thread can use the Object.
*
*     The "wait" parameter determines what happens if the supplied Object
*     is curently locked by another thread when this function is invoked.

*  Parameters:
*     this
*        Pointer to the Object to be locked.
*     wait
*        If the Object is curently locked by another thread then this
*        function will either report an error or block. If a non-zero value
*        is supplied for "wait", the calling thread waits until the object
*        is available for it to use. Otherwise, an error is reported and
*        the function returns immediately without locking the Object.

*  Applicability:
*     Object
*        This function applies to all Objects.

*  Notes:
*     - The astAnnul function is exceptional in that it can be used on
*     pointers for Objects that are not currently locked by the calling
*     thread. All other AST functions will report an error.
*     - The Locked object will belong to the current AST context.
*     - This function returns without action if the Object is already
*     locked by the calling thread.
*     - If simultaneous use of the same object is required by two or more
*     threads, astCopy should be used to to produce a deep copy of
*     the Object for each thread. Each copy should then be unlocked by
*     the parent thread (i.e. the thread that created the copy), and then
*     locked by the child thread (i.e. the thread that wants to use the
*     copy).
*     - This function is only available in the C interface.
*     - This function returns without action if the AST library has
*     been built without POSIX thread support (i.e. the "-with-pthreads"
*     option was not specified when running the "configure" script).
c--
*/

/* This function odes nothing if thread support is not enabvled. */
#if defined(THREAD_SAFE)

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   AstObject *fail;              /* Pointer to Object that failed to lock */
   AstObject *this;              /* Pointer to Object */
   int ihandle;                  /* Index of supplied objetc handle */
   int lstat;                    /* Local status value */

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object (this generates an
   error if it doesn't). Note, we use the "astMakePointer_NoLockCheck",
   since the usual "astMakePointer" macro invokes astCheckLock to report
   an error if the Object is not currently locked by the calling thread. */
   if ( !astIsAObject( this = astMakePointer_NoLockCheck( this_id ) ) ) return;

/* Ensure the global data for this class is accessable. Do not use the
   globals pointer stored in "*this" because "*this" may be locked by
   another thread and so we would pick up the wrong globals. */
   astGET_GLOBALS(NULL);

/* Ensure the running thread has sole access to the static handles arrays. */
   LOCK_MUTEX2;

/* Ensure the Handles arrays have been initialised. */
   if ( !active_handles ) InitContext( status );

/* Get the Handle index for the supplied object identifier. No error is
   reported if the handle is not curently associated with a thread.
   However, an error is reported if the Handle is associated with any
   thread other than the running thread. */
   ihandle = CheckId( this_id, 0, status );

/* We've finished with the handles arrays, for the moment. */
   UNLOCK_MUTEX2;

/* Check the object pointer was valid. */
   if( ihandle != -1 ){

/* The protected astManageLock function implements the public functions,
   astLock and astUnlock. */
      lstat = astManageLock( this, AST__LOCK, wait, &fail );
      if( astOK ) {
         if( lstat == 1 ) {
            if( fail == this ) {
               astError( AST__LCKERR, "astLock(%s): Failed to lock the %s because"
                         " it is already locked by another thread (programming "
                         "error).", status, astGetClass( this ),
                         astGetClass( this ) );

            } else {
               astError( AST__LCKERR, "astLock(%s): Failed to lock the %s because"
                         " a %s contained within it is already locked by another "
                         "thread (programming error).", status,
                         astGetClass( this ), astGetClass( this ),
                         astGetClass( fail ) );
            }

         } else if( lstat == 2 ) {
            astError( AST__LCKERR, "astLock(%s): Failed to lock a POSIX mutex.", status,
                      astGetClass( this ) );

/* If the Object is now locked for the running thread... */
         } else {

/* We need access to the handles arrays again. */
            LOCK_MUTEX2;

/* If the supplied handle is not currently assigned to any thread, assign
   it to the running thread. */
            if( handles[ ihandle ].context == UNOWNED_CONTEXT ) {
               RemoveHandle( ihandle, &unowned_handles, status );

#if defined(MEM_DEBUG)
               astHandleUse( ihandle, "locked by thread %d at context level %d",
                             handles[ ihandle ].thread, context_level );
#endif

               handles[ ihandle ].thread = AST__THREAD_ID;
               handles[ ihandle ].context = context_level;
               InsertHandle( ihandle, &active_handles[ context_level ],
                                status );
            }

/* Finished with the handles arrays again. */
            UNLOCK_MUTEX2;
         }
      }
   }
#endif
}

void astUnlockId_( AstObject *this_id, int report, int *status ) {
/*
c++
*  Name:
*     astUnlock

*  Purpose:
*     Unlock an Object for use by other threads.

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     void astUnlock( AstObject *this, int report )

*  Class Membership:
*     Object method.

*  Description:
*     Unlocks an Object previously locked using astLock, so that other
*     threads can use the Object. See astLock for further details.

*  Parameters:
*     this
*        Pointer to the Object to be unlocked.
*     report
*        If non-zero, an error will be reported if the supplied Object,
*        or any Object contained within the supplied Object, is not
*        currently locked by the running thread. If zero, such Objects
*        will be left unchanged, and no error will be reported.

*  Applicability:
*     Object
*        This function applies to all Objects.

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set, but no further error report will be made if it
*     subsequently fails under these circumstances.
*     - All unlocked Objects are excluded from AST context handling until
*     they are re-locked using astLock.
*     - This function is only available in the C interface.
*     - This function returns without action if the Object is not currently
*     locked by any thread. If it is locked by the running thread, it is
*     unlocked. If it is locked by another thread, an error will be reported
*     if "error" is non-zero.
*     - This function returns without action if the AST library has
*     been built without POSIX thread support (i.e. the "-with-pthreads"
*     option was not specified when running the "configure" script).
c--
*/

/* This function odes nothing if thread support is not enabvled. */
#if defined(THREAD_SAFE)

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   AstErrorContext error_context;/* Info about the current error context */
   AstObject *fail;              /* Pointer to Object that failed */
   AstObject *this;              /* Pointer to Object */
   int ihandle;                  /* Index of supplied objetc handle */
   int lstat;                    /* Local status value */

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object (this generates an
   error if it doesn't). Note, we use the "astMakePointer_NoLockCheck",
   since the usual "astMakePointer" macro invokes astCheckLock to report
   an error if the Object is not currently locked by the calling thread. */
   if ( !astIsAObject( this = astMakePointer_NoLockCheck( this_id ) ) ) return;

/* Ensure the global data for this class is accessable. */
   astGET_GLOBALS(this);

/* Start a new error reporting context. This saves any existing error status
   and then clear the status value. It also defer further error reporting. */
   astErrorBegin( &error_context );

/* Ensure the running thread has sole access to the static handles arrays. */
   LOCK_MUTEX2;

/* Ensure the Handles arrays have been initialised. */
   if ( !active_handles ) InitContext( status );

/* Get the Handle index for the supplied object identifier. Report an error
   if the handle is not curently associated with the running thread. */
   ihandle = CheckId( this_id, 1, status );

/* Break the associated of the handle with the current thread so that the
   handle is not assigned to any thread. We do this before unlocking the
   Object structure (using astManageLock) since as soon as astManageLock
   returns, another thread that is waiting for the object to be unlocked
   may start up and modify the handle properties. */
   if( handles[ ihandle ].context >= 0 ) {
      RemoveHandle( ihandle, &active_handles[ handles[ ihandle ].context ],
                    status );
#if defined(MEM_DEBUG)
      astHandleUse( ihandle, "unlocked from thread %d at context "
                    "level %d", handles[ ihandle ].thread,
                    handles[ ihandle ].context );
#endif
      handles[ ihandle ].thread = -1;
      handles[ ihandle ].context = UNOWNED_CONTEXT;
      InsertHandle( ihandle, &unowned_handles, status );
   }

/* We've finished with the handles arrays, for the moment. */
   UNLOCK_MUTEX2;

/* Check the supplied object pointer was valid. */
   if( ihandle != -1 ){

/* The protected astManageLock function implements the public functions,
   astLock and astUnlock. */
      lstat = astManageLock( this, AST__UNLOCK, 0, &fail );
      if( astOK ) {
         if( lstat == 1 ) {
            if( report ) {
               if( fail == this ) {
                  astError( AST__LCKERR, "astUnlock(%s): Failed to unlock the %s "
                            "because it is locked by another thread (programming "
                            "error).", status, astGetClass( this ),
                            astGetClass( this ) );

               } else {
                  astError( AST__LCKERR, "astUnlock(%s): Failed to unlock the %s "
                            "because a %s contained within it is locked by another "
                            "thread (programming error).", status,
                            astGetClass( this ), astGetClass( this ),
                            astGetClass( fail ) );
               }
            }

         } else if( lstat == 3 ) {
            astError( AST__LCKERR, "astUnlock(%s): Failed to unlock a POSIX mutex.", status,
                      astGetClass( this ) );

         }
      }
   }

/* End the error reporting context. If an error has occurred within this
   function, then this will display the deferred error messages so long
   as there was no error condition on entry to this function. If there
   was an error condition on entry, then the original status value will be
   re-instated. */
   astErrorEnd( &error_context );

#endif
}

AstObject *astI2P_( int integer, int *status ) {
/*
*+
*  Name:
*     astI2P

*  Purpose:
*     Pack an integer Object ID into a pointer.

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astI2P( int integer )

*  Class Membership:
*     Object class function.

*  Description:
*     This function accepts an integer (int) value representing an
*     Object identifier and packs its bit pattern into a pointer value
*     (from which it may subsequently be retrieved using astP2I).
*
*     These functions should be used to avoid any dependency on
*     integer-to-pointer conversions (given that the values are not
*     true pointers) which might affect the exchange of Object
*     identifier values with other languages, such as Fortran 77,
*     where they are stored as integers.

*  Parameters:
*     integer
*        The integer value to be stored.

*  Returned Value:
*     The resulting pointer value (which is not usually a valid C pointer).

*  Notes:
*     - This function does not perform error checking and does not
*     generate errors.
*     - This is a protected function in the sense that it is not
*     intended that external users should invoke it directly.  It is
*     accessible from external code, however, in order that public
*     macros invoked from that code can make use of it.
*-
*/

/* Local Variables: */
   IdUnion temp;                 /* Overlapping int and pointer */

/* Clear the pointer value in the "temp" IdUnion and then set the
   integer part of it to the value to be stored. */
   temp.pointer = zero_ptr;
   temp.integer = integer;

/* Return the resulting pointer value. */
   return temp.pointer;
}

MYSTATIC void InitContext( int *status ) {
/*
*  Name:
*     InitContext

*  Purpose:
*     Initialise the first AST context level.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void InitContext( int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function initialises the first AST context level (the level
*     before any call to astBegin has been made). It should be invoked
*     once, before any use is made of context levels.

*  Parameters:
*     status
*        Pointer to the inherited status variable.

*  Parameters:
*     None.

*  Notes:
*     - This function does nothing after the first successful invocation.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Check that the active_handles array hasn't already been allocated. */
   if ( !active_handles ) {

/* Allocate and initialise the "active_handles" array. */

      astBeginPM;
      active_handles = astMalloc( sizeof( int ) );
      astEndPM;

      if ( astOK ) active_handles[ 0 ] = -1;
   }
}

MYSTATIC void InsertHandle( int ihandle, int *head, int *status ) {
/*
*  Name:
*     InsertHandle

*  Purpose:
*     Insert a Handle into a list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void InsertHandle( int ihandle, int *head, int *status )

*  Class Membership:
*     Object member function.

*  Description:
*     This function inserts a Handle structure into a doubly linked
*     list of such structures composed of elements drawn from the
*     "handles" array. The new list member is inserted in front of the
*     member previously occupying the "head of list" position.

*  Parameters:
*     ihandle
*        Offset in the "handles" array that identifies the handle to
*        be added to the list.
*     head
*        Address of an int which holds the offset in the "handles"
*        array of the element at the head of the list (or -1 if the
*        list is empty). This value will be updated to identify the
*        new list member.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function does not perform error chacking and does not
*     generate errors.
*     - The lists generated by this function use integer offsets into
*     the "handles" array for their links, rather than pointers. This
*     is because the array may be re-located in memory when it needs
*     to be extended, so pointers to its element would not remain
*     valid.
*     - The list elements are drawn from the "handles" array in the
*     first place so that they can be addressed by small integers (the
*     offset in the array). This allows references to Handles to be
*     encoded along with security information into an integer that is
*     sufficiently short to be exported to other languages
*     (e.g. Fortran) which might not be able to accommodate
*     full-length C pointers.
*/


#if defined(MEM_DEBUG)
   char buf[80];
   astHandleUse( ihandle, "about to be inserted into %s (%d)",
                 HeadString( head, buf ), head );
   CheckList( head, status );
   CheckInList( ihandle, head, 0, status );
#endif

/* Check a head pointer was supplied (may not be if an error has
   occurred). */
   if( ! head ) return;

/* If the list is empty, the sole new element points at itself. */
   if ( *head == -1 ) {
      handles[ ihandle ].flink = ihandle;
      handles[ ihandle ].blink = ihandle;

/* Otherwise, insert the new element in front of the element at the
   head of the list. */
   } else {
      handles[ ihandle ].flink = *head;
      handles[ ihandle ].blink = handles[ *head ].blink;
      handles[ ( handles[ *head ].blink) ].flink = ihandle;
      handles[ *head ].blink = ihandle;
   }

/* Update the list head to identify the new element. */
   *head = ihandle;

#if defined(MEM_DEBUG)
   CheckList( head, status );
   astHandleUse( ihandle, "has been inserted into %s", buf );
#endif
}

AstObject *astMakeId_( AstObject *this, int *status ) {
/*
*+
*  Name:
*     astMakeId

*  Purpose:
*     Issue an identifier for an Object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astMakeId( AstObject *this )

*  Class Membership:
*     Object member function.

*  Description:
*     This function takes a normal C pointer to an Object and
*     associates an identifier with it.

*  Parameters:
*     this
*        Pointer to an Object. Note that this function copies this
*        value (it is not cloned), so the caller should not annul the
*        pointer afterwards.

*  Returned Value:
*     The resulting identifier value.

*  Notes:
*     - An identifier value of zero will be returned and the supplied
*     Object pointer will be annulled if this function is invoked with
*     the global error status set or if it should fail for any reason.
*     - A zero identifier value will also be returned if a NULL object
*     pointer is supplied, but this will not provoke an error.
*     - This is a protected function in the sense that it is not
*     intended that external users should invoke it directly.  It is
*     accessible from external code, however, in order that public
*     macros invoked from that code can make use of it.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   AstObject *id;                /* ID value to return */
   int ihandle;                  /* Handle offset */

/* Initialise. */
   id = astI2P( 0 );
   ihandle = 0;

/* Check the global error status. */
   if ( astOK ) {

/* Get a pointer to Thread-specific global data. */
      astGET_GLOBALS(this);

/* Gain exclusive access to the handles array. */
      LOCK_MUTEX2;

/* If a non-NULL Object pointer was given, we must obtain a Handle
   structure to associate with it (otherwise a zero identifier value
   is returned without error). */
      if ( this ) {

/* If the free Handles list is not empty, obtain a Handle from it. */
         if ( free_handles != -1 ) {
            ihandle = free_handles;
            RemoveHandle( ihandle, &free_handles, status );

/* Otherwise, allocate a new Handle by extending the "handles" array
   and using the offset of the new element. */
         } else {

            astBeginPM;
            handles = astGrow( handles, nhandles + 1, sizeof( Handle ) );
            astEndPM;

            if ( astOK ) {
               ihandle = nhandles++;

               handles[ ihandle ].ptr = NULL;
               handles[ ihandle ].context = INVALID_CONTEXT;
               handles[ ihandle ].check = 0;
               handles[ ihandle ].flink = -1;
               handles[ ihandle ].blink = -1;
#if defined(THREAD_SAFE)
               handles[ ihandle ].thread = 0;
#endif

#if defined(MEM_DEBUG)
               handles[ ihandle ].id = 0;
               handles[ ihandle ].vtab = NULL;
#endif
            }
         }

/* If the first AST context level has not yet been initialised, invoke
   InitContext to initialise it and allocate memory for the
   "active_handles" array which stores context information. */
         if ( astOK ) {
            if ( !active_handles ) InitContext( status );

/* Store the Object pointer and current context level in the Handle. */
            if ( astOK ) {
               handles[ ihandle ].ptr = this;
               handles[ ihandle ].context = context_level;
#if defined(THREAD_SAFE)
               handles[ ihandle ].thread = AST__THREAD_ID;
#endif

/* Store extra debugging information in the handle if enabled */
#if defined(MEM_DEBUG)
               handles[ ihandle ].id = astMemoryId( this );
               handles[ ihandle ].vtab = this->vtab;
               astHandleUse( ihandle, "associated with a %s (id %d)",
                              astGetClass( this ), astMemoryId( this ));
#endif

/* Insert the Handle into the active Handles list for the current
   context level. */
               InsertHandle( ihandle, &active_handles[ context_level ], status );

/* Associate an identifier value with the Handle. */
               id = AssocId( ihandle, status );

/* If an error occurred, clean up by annulling the Handle. This
   ensures that the Object pointer is annulled and returns the unused
   Handle to the Free Handle list. */
               if ( !astOK ) {
                  AnnulHandle( ihandle, status );
                  this = NULL;
               }

/* If the Handle wasn't used (because of an earlier error), return it
   to the free Handles list. */
            } else {
               InsertHandle( ihandle, &free_handles, status );
            }
         }
      }
      UNLOCK_MUTEX2;
   }

/* If a bad status value was either supplied or generated within this
   function, then annul the supplied pointer (unless it is NULL). */
   if ( !astOK && this ) (void) astAnnul( this );

/* Return the identifier value. */
   return id;
}

AstObject *astMakePointer_( AstObject *this_id, int *status ) {
/*
*+
*  Name:
*     astMakePointer

*  Purpose:
*     Obtain a true C pointer from an Object identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astMakePointer( AstObject *this )

*  Class Membership:
*     Object class function.

*  Description:
*     This function accepts an external Object identifier and returns
*     a true C Object pointer that may be de-referenced to access the
*     Object's data and methods.

*  Parameters:
*     this
*        The identifier value.

*  Returned Value:
*     The true C pointer value.

*  Notes:
*     - In a thread-safe context, an error is reported if the supplied
*     Object has not been locked by the calling thread (using astLock)
*     prior to invoking this function.
*     - The object reference count is not modified by this function,
*     so the returned pointer should not be annulled by the caller.
*     - Typically, this function should be used whenever a public
*     function must accept identifier values directly (rather than
*     having them translated automatically into pointers during
*     argument validation by the astCheck<Class> range of functions).
*     - Note that this function will NOT accept a true C pointer as an
*     argument, even when invoked from internal code (with astCLASS
*     defined). An identifier value MUST be supplied, and an error
*     will result if it is not associated with an active Object.
*     - This function attempts to execute even if the global error
*     status is set, but no further error report will be made if it
*     subsequently fails under these circumstances.
*     - This is a protected function in the sense that it is not
*     intended that external users should invoke it directly.  It is
*     accessible from external code, however, in order that public
*     macros invoked from that code can make use of it.
*-
*/

/* Local Variables: */
   AstObject *ptr;               /* Pointer value to return */
   int ihandle;                  /* Handle offset in "handles" array */

/* Initialise. */
   ptr = NULL;

/* Gain exclusive access to the handles array. */
   LOCK_MUTEX2;

/* Validate the identifier supplied and derive the Handle offset. */
   ihandle = CheckId( this_id, 1, status );

/* If the identifier was valid, extract the Object pointer from the
   Handle. */
   if ( ihandle != -1 ) ptr = handles[ ihandle ].ptr;

   UNLOCK_MUTEX2;

/* Return the result. */
   return ptr;
}

AstObject *astMakePointer_NoLockCheck_( AstObject *this_id, int *status ) {
/*
*+
*  Name:
*     astMakePointer_NoLockCheck

*  Purpose:
*     Obtain a true C pointer from an Object identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "object.h"
*     AstObject *astMakePointer_NoLockCheck( AstObject *this )

*  Class Membership:
*     Object class function.

*  Description:
*     This function accepts an external Object identifier and returns
*     a true C Object pointer that may be de-referenced to access the
*     Object's data and methods.
*
*     It is identicial to astMakePointer except that it does not check
*     that the supplied Object is locked by the running thread.

*  Parameters:
*     this
*        The identifier value.

*  Returned Value:
*     The true C pointer value.

*  Notes:
*     - The object reference count is not modified by this function,
*     so the returned pointer should not be annulled by the caller.
*     - Typically, this function should be used whenever a public
*     function must accept identifier values directly (rather than
*     having them translated automatically into pointers during
*     argument validation by the astCheck<Class> range of functions).
*     - Note that this function will NOT accept a true C pointer as an
*     argument, even when invoked from internal code (with astCLASS
*     defined). An identifier value MUST be supplied, and an error
*     will result if it is not associated with an active Object.
*     - This function attempts to execute even if the global error
*     status is set, but no further error report will be made if it
*     subsequently fails under these circumstances.
*     - This is a protected function in the sense that it is not
*     intended that external users should invoke it directly.  It is
*     accessible from external code, however, in order that public
*     macros invoked from that code can make use of it.
*-
*/

/* Local Variables: */
   AstObject *ptr;               /* Pointer value to return */
   int ihandle;                  /* Handle offset in "handles" array */

/* Initialise. */
   ptr = NULL;

/* Gain exclusive access to the handles array. */
   LOCK_MUTEX2;

/* Validate the identifier supplied and derive the Handle offset. */
   ihandle = CheckId( this_id, 0, status );

/* If the identifier was valid, extract the Object pointer from the
   Handle. */
   if ( ihandle != -1 ) ptr = handles[ ihandle ].ptr;

   UNLOCK_MUTEX2;

/* Return the result. */
   return ptr;
}

int astP2I_( AstObject *pointer, int *status ) {
/*
*+
*  Name:
*     astP2I

*  Purpose:
*     Extract an integer Object ID from a pointer.

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     int astP2I( AstObject *pointer )

*  Class Membership:
*     Object class function.

*  Description:
*     This function retrieves an integer (int) value representing an
*     Object identifier from a pointer value (into which it has
*     previously been packed using astI2P).
*
*     These functions should be used to avoid any dependency on
*     integer-to-pointer conversions (given that the values are not
*     true pointers) which might affect the exchange of Object
*     identifier values with other languages, such as Fortran 77,
*     where they are stored as integers.

*  Parameters:
*     pointer
*       The pointer value into which the ID has previously been packed.

*  Returned Value:
*     The extracted Object identifier value as an integer (int).

*  Notes:
*     - This function does not perform error checking and does not
*     generate errors.
*     - This is a protected function in the sense that it is not
*     intended that external users should invoke it directly.  It is
*     accessible from external code, however, in order that public
*     macros invoked from that code can make use of it.
*-
*/

/* Local Variables: */
   IdUnion temp;                 /* Overlapping int and pointer */

/* Store the pointer value supplied. */
   temp.pointer = pointer;

/* Return the integer part of it. */
   return temp.integer;
}

MYSTATIC void RemoveHandle( int ihandle, int *head, int *status ) {
/*
*  Name:
*     RemoveHandle

*  Purpose:
*     Remove a Handle from a list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void RemoveHandle( int ihandle, int *head )

*  Class Membership:
*     Object member function.

*  Description:
*     This function removes a Handle structure from a doubly linked
*     list of such structures composed of elements drawn from the
*     "handles" array. The "head of list" position is updated if
*     necessary.

*  Parameters:
*     ihandle
*        Offset in the "handles" array that identifies the Handle to
*        be removed from the list (note that the Handle must actually
*        be in the list, although this function does not check this).
*     head
*        Address of an int which holds the offset in the "handles"
*        array of the element at the head of the list. This value will
*        be updated if necessary to identify the new list head (or set
*        to -1 if removing the Handle causes the list to become
*        empty).

*  Notes:
*     - This function does not perform error chacking and does not
*     generate errors.
*     - See the InsertHandle function for details of how and why lists
*     of Handles are constructed.
*/


#if defined(MEM_DEBUG)
   char buf[80];
   astHandleUse( ihandle, "about to be removed from %s", HeadString( head, buf ) );
   CheckList( head, status );
   CheckInList( ihandle, head, 1, status );
#endif

/* Check a head pointer was supplied (may not be if an error has
   occurred). */
   if( ! head ) return;

/* Remove the Handle from the list by re-establishing links between
   the elements on either side of it. */
   handles[ ( handles[ ihandle ].blink ) ].flink = handles[ ihandle ].flink;
   handles[ ( handles[ ihandle ].flink ) ].blink = handles[ ihandle ].blink;

/* If the element removed was at the head of the list, update the head
   of list offset to identify the following element. */
   if ( ihandle == *head ) {
      *head = handles[ ihandle ].flink;

/* If the head of list still identifies the removed element, then note
   that the list is now empty. */
      if ( *head == ihandle ) *head = -1;
   }

/* Make the removed element point at itself. */
   handles[ ihandle ].flink = ihandle;
   handles[ ihandle ].blink = ihandle;

#if defined(MEM_DEBUG)
   astHandleUse( ihandle, "has been removed from %s", buf );
   CheckList( head, status );
#endif
}

void astSetId_( void *this_id_void, const char *settings, ... ) {
/*
*  Name:
*     astSetId_

*  Purpose:
*     Set values for an Object's attributes via an identifier.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     void astSetId_( AstObject *this, const char *settings, ... )

*  Class Membership:
*     Object member function.

*  Description:
*     This function implements the axternal interface to the astSet
*     method (q.v.). It accepts an Object identifier, but otherwise
*     behaves identically to astSet.

*  Parameters:
*     this
*        Object identifier.
*     settings
*        Pointer to a null-terminated string containing a
*        comma-separated list of attribute settings.
*     ...
*        Optional arguments which supply values to be substituted for
*        any "printf"-style format specifiers that appear in the
*        "settings" string.

*  Notes:
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     Object identifier is of type (void *) and is converted and
*     validated within the function itself.
*/

/* Local Variables: */
   AstObject *this;              /* Pointer to the Object structure */
   int *status;                  /* Pointer to inherited status variable */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the integer holding the inherited status value. */
   status = astGetStatusPtr;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the Object pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Object. */
   this = astCheckObject( astMakePointer( this_id_void ) );
   if ( astOK ) {

/* Obtain the variable argument list and pass all arguments to the
   astVSet method for interpretation. */
      va_start( args, settings );
      astVSet( this, settings, NULL, args );
      va_end( args );
   }
}

int astThreadId_( AstObject *this_id, int ptr, int *status ) {
/*
c++
*  Name:
*     astThread

*  Purpose:
*     Determine the thread that owns an Object.

*  Type:
*     Public function.

*  Synopsis:
*     #include "object.h"
*     int astThread( AstObject *this, int ptr )

*  Class Membership:
*     Object method.

*  Description:
*     Returns an integer that indicates whether the supplied Object (or
*     Object pointer) is currently unlocked, or is currently locked by
*     the running thread, or another thread.

*  Parameters:
*     this
*        Pointer to the Object to be checked.
*     ptr
*        If non-zero, returns information about the supplied Object
*        pointer, rather than the Object structure itself. See "Object
*        Pointers and Structures" below.

*  Returned Value:
*     astThread()
*        A value of AST__UNLOCKED is returned if the Object (or pointer)
*        is currently unlocked (i.e. has been unlocked using astUnlock
*        but has not yet been locked using astLock). A value of
*        AST__RUNNING is returned if the Object (or pointer) is currently
*        locked by the running thread. A value of AST__OTHER is returned
*        if the Object (or pointer) is currently locked by the another
*        thread.

*  Object Pointers and Structures:
*     At any one time, an AST Object can have several distinct pointers,
*     any one of which can be used to access the Object structure. For
*     instance, the astClone function will produce a new distinct pointer
*     for a given Object. In fact, an AST "pointer" is not a real pointer
*     at all - it is an identifier for a "handle" structure, encoded to
*     make it look like a pointer. Each handle contains (amongst othere
*     things) a "real" pointer to the Object structure. This allows more
*     than one handle to refer to the same Object structure. So when you
*     call astClone (for instance) you get back an identifier for a new
*     handle that refers to the same Object as the supplied handle.
*
*     In order to use an Object for anything useful, it must be locked
*     for use by the running thread (either implicitly at creation or
*     explicitly using astLock). The identity of the thread is stored in
*     both the Object structure, and in the handle that was passed to
*     astLock (or returned by the constructor function). Thus it is
*     possible for a thread to have active pointers for Objects that are
*     currently locked by another thread. In general, if such a pointer is
*     passed to an AST function an error will be reported indicating that
*     the Object is currently locked by another thread. The two exceptions
*     to this is that astAnnul can be used to annull such a pointer, and
*     this function can be used to return information about the pointer.
*
*     The other practical consequence of this is that when astEnd is
*     called, all active pointers currently owned by the running thread
*     (at the current context level) are annulled. This includes pointers
*     for Objects that are currently locked by other threads.
*
*     If the "ptr" parameter is zero, then the returned value describes
*     the Object structure itself. If "ptr" is non-zero, then the returned
*     value describes the supplied Object pointer (i.e. handle), rather
*     than the Object structure.

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set, but no further error report will be made if it
*     subsequently fails under these circumstances.
*     - This function is only available in the C interface.
*     - This function always returns AST__RUNNING if the AST library has
*     been built without POSIX thread support (i.e. the "-with-pthreads"
*     option was not specified when running the "configure" script).
c--
*/

/* Local Variables: */
   astDECLARE_GLOBALS       /* Thread-specific global data */
   int result;              /* The returned value */

#if defined(THREAD_SAFE)

/* More local Variables: */
   AstObject *this;
   int ihandle;
   int check;

/* Ensure global variables are accessable. */
   astGET_GLOBALS(NULL);
#endif

/* Initialise the returned value */
   result = AST__RUNNING;

/* Nothing more to do if AST was not build with thread support. */
#if defined(THREAD_SAFE)

/* If the ownership of the handle is being queried... */
   if( ptr ) {

/* Lock the mutex that guards access to the handles array */
      LOCK_MUTEX2;

/* Check the supplied object identifier is valid and get the
   corresponding index into the handles array. */
      ihandle = CheckId( this_id, 1, status );
      if( ihandle != -1 ) {

/* Set the returned value on the basis of the threa didentifier stored in
   the handle structure. */
         if( handles[ ihandle ].thread == -1 ) {
            result = AST__UNLOCKED;
         } else if( handles[ ihandle ].thread != AST__THREAD_ID ) {
            result = AST__OTHER;
         }
      }

/* Unlock the mutex that guards access to the handles array */
      UNLOCK_MUTEX2;

/* Otherwise, the ownership of the Object is being queried. Obtain the
   Object pointer from the ID supplied and validate the pointer to ensure
   it identifies a valid Object (this generates an error if it doesn't).
   Note, we use the "astMakePointer_NoLockCheck", since the usual
   "astMakePointer" macro invokes astCheckLock to report an error if the
   Object is not currently locked by the calling thread. */
   } else if( astIsAObject( this = astMakePointer_NoLockCheck( this_id ) ) ) {

/* Determine which thread (if any) has the object locked, and set an
   appropriate return value. */
      check = astManageLock( this, AST__CHECKLOCK, 0, NULL );

      if( check == 5 ) {
         result = AST__OTHER;
      } else if( check == 6 ) {
         result = AST__UNLOCKED;
      }
   }

#endif

/* Return the result. */
   return result;
}

int astVersion_( int *status ) {
/*
*++
*  Name:
c     astVersion
f     AST_VERSION

*  Purpose:
*     Return the version of the AST library being used.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astVersion
f     RESULT = AST_VERSION()

*  Class Membership:
*     Object class function.

*  Description:
c     This macro invokes a function which
f     This function
*     returns an integer representing the version of the AST library
*     being used. The library version is formatted as a string such as
*     "2.0-7" which contains integers representing the "major version" (2),
*     the "minor version" (0) and the "release" (7). The integer returned
*     by this function combines all three integers together into a single
*     integer using the expresion:
*
*     (major version)*1E6 + (minor version)*1E3 + (release)

*  Returned Value:
c     astVersion
f     AST_VERSION = INTEGER
*        The major version, minor version and release numbers for the AST
*        library, encoded as a single integer.

*  Applicability:
*     Object
c        This macro applies to all Objects.
f        This routine applies to all Objects.

*--
*/

   return (int) AST__VMAJOR*1E6 + AST__VMINOR*1E3 + AST__RELEASE;
}

int astEscapes_( int new_value, int *status ) {
/*
*++
*  Name:
c     astEscapes
f     AST_ESCAPES

*  Purpose:
*     Control whether graphical escape sequences are included in strings.

*  Type:
*     Public function.

*  Synopsis:
c     #include "object.h"
c     int astEscapes( int new_value )
f     RESULT = AST_ESCAPES( NEWVAL, STATUS )

*  Class Membership:
*     Object class function.

*  Description:
*     The Plot class defines a set of escape sequences which can be
*     included within a text string in order to control the appearance of
*     sub-strings within the text. See the Escape attribute for a
*     description of these escape sequences. It is usually inappropriate
*     for AST to return strings containing such escape sequences when
*     called by application code. For instance, an application which
*     displays the value of the Title attribute of a Frame usually does
*     not want the displayed string to include potentially long escape
*     sequences which a human read would have difficuly interpreting.
*     Therefore the default behaviour is for AST to strip out such escape
*     sequences when called by application code. This default behaviour
*     can be changed using this function.

*  Parameters:
c     new_value
f     NEWVAL = INTEGER (Given)
*        A flag which indicates if escapes sequences should be included
*        in returned strings. If zero is supplied, escape sequences will
*        be stripped out of all strings returned by any AST function. If
*        a positive value is supplied, then any escape sequences will be
*        retained in the value returned to the caller. If a negative
*        value is supplied, the current value of the flag will be left
*        unchanged.

*  Returned Value:
c     astEscapes
f     AST_ESCAPES = INTEGER
*        The value of the flag on entry to this function.

*  Applicability:
*     Object
c        This macro applies to all Objects.
f        This routine applies to all Objects.

*  Notes:
*     - This function also controls whether the
c     astStripEscapes
f     AST_STRIPESCAPES
*     function removes escape sequences from the supplied string, or
*     returns the supplied string without change.
*     - This function attempts to execute even if an error has already
*     occurred.

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS
   int old_val;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Save the old value. */
   old_val = retain_esc;

/* Set the new value. */
   if( new_value > 0 ) {
      retain_esc = 1;

   } else if( new_value == 0 ) {
      retain_esc = 0;
   }

/* Return the old value. */
   return old_val;
}


#if defined(MEM_DEBUG)

/* Check each handle in a list is uniquely connected to one other handle
   in both the forward and backward directions. */

void CheckList( int *head, int *status ) {
   int ok;
   int ihandle;
   char buf[200];
   astDECLARE_GLOBALS
   if( !astOK ) return;

   astGET_GLOBALS(NULL);

   ok = 1;
   if ( *head != -1 ) {
      ihandle = *head;
      if( handles[ handles[ ihandle ].blink ].flink != ihandle ||
          handles[ handles[ ihandle ].flink ].blink != ihandle ) {
         ok = 0;

      } else {
         if( CheckThread( ihandle, head, status ) ) {
            ihandle= handles[ *head ].blink;
            while( ihandle != *head ) {
               if( handles[ handles[ ihandle ].blink ].flink != ihandle ||
                   handles[ handles[ ihandle ].flink ].blink != ihandle ||
                   CheckThread( ihandle, head, status ) == 0 ) {
                  ok = 0;
                  break;
               }
               ihandle= handles[ ihandle ].blink;
            }
         }
      }
   }

   if( !ok ) {
      printf("CheckList error in %s\n", HeadString( head, buf ) );
      printf("   Central handle: %s\n", HandleString( ihandle, buf ) );

      if( handles[ handles[ ihandle ].blink ].flink != ihandle ) {
         printf("   Central handle->blink: %s\n",
                 HandleString( handles[ ihandle ].blink, buf ) );
         printf("   Central handle->blink->flink: %s\n",
                 HandleString( handles[ handles[ ihandle ].blink ].flink, buf ) );
      }

      if( handles[ handles[ ihandle ].flink ].blink != ihandle ) {
         printf("   Central handle->flink: %s\n",
                 HandleString( handles[ ihandle ].flink, buf ) );
         printf("   Central handle->flink->blink: %s\n",
                 HandleString( handles[ handles[ ihandle ].flink ].blink, buf ) );
      }
   }

}


/* Check if a specified handle is, or is not, in a given list of handles. */

void CheckInList( int ihandle, int *head, int in, int *status ){
   char list[80], buf[200];
   int found = 0;
   if( !astOK ) return;

   if ( *head != -1 ) {
      if( ihandle == *head ) {
         found = 1;
      } else {
         if( CheckThread( ihandle, head, status ) ) {
            int jhandle= handles[ *head ].blink;
            while( jhandle != *head ) {
               if( ihandle == jhandle ) {
                  found = 1;
                  break;
               }
               jhandle= handles[ jhandle ].blink;
            }
         }
      }
   }

   if( in && !found ) {
      printf("Error: Handle %s not in %s\n", HandleString( ihandle, buf ),
              HeadString( head, list ) );
   } else if( !in && found ) {
      printf("Error: Handle %s is in %s\n", HandleString( ihandle, buf ),
              HeadString( head, list ) );
   }

}

/* Check that a handle is owned by the currently executing thread. */

int CheckThread( int ihandle, int *head, int *status ) {
   int result = 1;

#if defined(THREAD_SAFE)

   char buf[200];
   astDECLARE_GLOBALS
   if( !astOK ) return result;

   astGET_GLOBALS(NULL);

   if( *head == unowned_handles  ) {
      if(  handles[ ihandle ].thread != -1 ) {
         printf("Handle %s has wrong thread: is %d, should "
             "be -1 (i.e. unowned)\n", HandleString( ihandle, buf ),
              handles[ ihandle ].thread );

         result = 0;
      }

   } else if( *head == free_handles ) {
      if(  handles[ ihandle ].thread != -1 ) {
         printf("Handle %s has wrong thread: is %d, should "
             "be -1 (i.e. free)\n", HandleString( ihandle, buf ),
              handles[ ihandle ].thread );
         result = 0;
      }

   } else if( handles[ ihandle ].thread != AST__THREAD_ID ) {
      printf("Handle %s has wrong thread: is %d, should "
             "be %d\n", HandleString( ihandle, buf ),
              handles[ ihandle ].thread, AST__THREAD_ID );
      result = 0;
   }

#endif

   return result;
}

void astWatchHandle_( int handle ){
   Watched_Handle = handle;
}

void astHandleUse_( int handle, const char *verb, ... ){
   va_list args;
   if( handle == Watched_Handle ) {
      va_start( args, verb );
      astHandleAlarm( verb, args );
      va_end( args );
   }
}

void astHandleAlarm_( const char *verb, va_list args ){
   char buff[200], hbuf[200];
   astDECLARE_GLOBALS
   astGET_GLOBALS(NULL);

   vsprintf( buff, verb, args );

#if defined(THREAD_SAFE)
   printf( "astHandleAlarm: Handle %s %s (current thread is %d).\n\n",
           HandleString( Watched_Handle, hbuf ), buff, AST__THREAD_ID );
#else
   printf( "astHandleAlarm: Handle %s %s.\n\n",
           HandleString( Watched_Handle, hbuf ), buff );
#endif
}

MYSTATIC const char *HandleString( int ihandle, char *buf ){
#if defined(THREAD_SAFE)
   astDECLARE_GLOBALS
   astGET_GLOBALS(NULL);

   if( ihandle >= 0 ) {
      sprintf( buf, "(index:%d v:%d c:%d t:%d i:%d cl:%s) [cur. thread: %d]",
               ihandle,
               handles[ ihandle ].check,
               handles[ ihandle ].context, handles[ ihandle ].thread,
               handles[ ihandle ].id,
               handles[ ihandle ].vtab ? handles[ ihandle ].vtab->class : "<none>",
               AST__THREAD_ID );
   } else {
      sprintf( buf, "(index:%d <invalid>) [cur. thread: %d]", ihandle,
               AST__THREAD_ID );
   }

#else
   if( ihandle >= 0 ) {
      sprintf( buf, "(index:%d v:%d c:%d i:%d cl:%s)", ihandle,
               handles[ ihandle ].check,
               handles[ ihandle ].context, handles[ ihandle ].id,
               handles[ ihandle ].vtab ? handles[ ihandle ].vtab->class : "<none>" );
   } else {
      sprintf( buf, "(index:%d <invalid>)", ihandle );
   }
#endif
   return buf;
}

MYSTATIC const char *HeadString( int *head, char *list ){
   int i;
   astDECLARE_GLOBALS
   astGET_GLOBALS(NULL);

   if( head == &free_handles ) {
      strcpy( list, "free_handles" );

#if defined(THREAD_SAFE)
   } else if( head == &unowned_handles ) {
      strcpy( list, "unowned_handles" );
#endif

   } else {
      *list = 0;
      for( i = 0; i <= context_level; i++ ) {
         if( *head == active_handles[ i ] ) {
            sprintf( list, "active_handles[%d]", i );
            break;
         }
      }
      if( *list == 0 ) sprintf( list, "unknown handles list with head %d",
                                *head );
   }
   return list;
}

#endif
