/*
*class++
*  Name:
*     StcObsDataLocation

*  Purpose:
*     Correspond to the IVOA ObsDataLocation class.

*  Constructor Function:
c     astStcObsDataLocation
f     AST_STCOBSDATALOCATION

*  Description:
*     The StcObsDataLocation class is a sub-class of Stc used to describe
*     the coordinate space occupied by a particular observational dataset.
*
*     See http://hea-www.harvard.edu/~arots/nvometa/STC.html
*
*     An STC ObsDataLocation element specifies the extent of the
*     observation within a specified coordinate system, and also specifies
*     the observatory location within a second coordinate system.
*
*     The AST StcObsDataLocation class inherits from Stc, and therefore
*     an StcObsDataLocation can be used directly as an Stc. When used
*     in this way, the StcObsDataLocation describes the location of the
*     observation (not the observatory).
*
*     Eventually, this class will have a method for returning an Stc
*     describing the observatory location. However, AST currently does not
*     include any classes of Frame for describing terrestrial or solar
*     system positions. Therefore, the provision for returning observatory
*     location as an Stc is not yet available. However, for terrestrial
*     observations, the position of the observatory can still be recorded
*     using the ObsLon and ObsLat attributes of the Frame encapsulated
*     within the Stc representing the observation location (this assumes
*     the observatory is located at sea level).

*  Inheritance:
*     The StcObsDataLocation class inherits from the Stc class.

*  Attributes:
*     The StcObsDataLocation class does not define any new attributes beyond
*     those which are applicable to all Stcs.

*  Functions:
c     The StcObsDataLocation class does not define any new functions beyond those
f     The StcObsDataLocation class does not define any new routines beyond those
*     which are applicable to all Stcs.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     25-APR-2005 (DSB):
*        Original version.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS StcObsDataLocation

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "stc.h"                 /* Coordinate stcs (parent class) */
#include "channel.h"             /* I/O channels */
#include "region.h"              /* Regions within coordinate systems */
#include "pointlist.h"           /* Points within coordinate systems */
#include "stcobsdatalocation.h"  /* Interface definition for this class */

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

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );



#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(StcObsDataLocation)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(StcObsDataLocation,Class_Init)
#define class_vtab astGLOBAL(StcObsDataLocation,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstStcObsDataLocationVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstStcObsDataLocation *astStcObsDataLocationId_( void *, int, AstKeyMap **, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void StcSetObs( AstStcObsDataLocation *, AstPointList *, int * );

static int GetObjSize( AstObject *, int * );
/* Member functions. */
/* ================= */
static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcobsdatalocation.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     StcObsDataLocation member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied StcObsDataLocation,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the StcObsDataLocation.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstStcObsDataLocation *this;         /* Pointer to StcObsDataLocation structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the StcObsDataLocation structure. */
   this = (AstStcObsDataLocation *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astGetObjSize( this->obs );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}


void astInitStcObsDataLocationVtab_(  AstStcObsDataLocationVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitStcObsDataLocationVtab

*  Purpose:
*     Initialise a virtual function table for a StcObsDataLocation.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcobsdatalocation.h"
*     void astInitStcObsDataLocationVtab( AstStcObsDataLocationVtab *vtab, const char *name )

*  Class Membership:
*     StcObsDataLocation vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the StcObsDataLocation class.

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
   AstStcVtab *stc;        /* Pointer to Stc component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitStcVtab( (AstStcVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAStcObsDataLocation) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstStcVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;
   stc = (AstStcVtab *) vtab;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   vtab->StcSetObs = StcSetObs;

/* Declare the copy constructor, destructor and class dump functions. */
   astSetDump( vtab, Dump, "StcObsDataLocation", "Observation coverage" );
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void StcSetObs( AstStcObsDataLocation *this, AstPointList *obs, int *status ) {
/*
*+
*  Name:
*     astStcSetObs

*  Purpose:
*     Set the observatory position within an StcObsDataLocation.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "region.h"
*     void astStcSetObs( AstStcObsDataLocation *this, AstPointList *obs )

*  Class Membership:
*     StcObsDataLocation virtual function

*  Description:
*     This function stores a clone of the supplied PointList pointer
*     within the supplied StcObsDataLocation, first annulling any
*     pointer already stored in the StcObsDataLocation.

*  Parameters:
*     this
*        Pointer to the StcObsDataLocation.
*     obs
*        Pointer to a PointList defining the observatory position. NULL
*        may be supplied in which case any existing observatory position
*        is removed.

*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Free any existing obseravtory position PointList. */
   if( this->obs ) this->obs = astAnnul( this->obs );

/* Store any supplied pointer. */
   if( obs ) this->obs = astClone( obs );

}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for StcObsDataLocation objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for StcObsDataLocation
*     objects.

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
*     Regions within the StcObsDataLocation.
*/

/* Local Variables: */
   AstStcObsDataLocation *in;   /* Pointer to input StcObsDataLocation */
   AstStcObsDataLocation *out;  /* Pointer to output StcObsDataLocation */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output StcObsDataLocations. */
   in = (AstStcObsDataLocation *) objin;
   out = (AstStcObsDataLocation *) objout;

/* For safety, start by clearing any references to the input component
   Regions, etc,  from the output StcObsDataLocation. */
   out->obs = NULL;

/* Make a copy of the Observatory location */
   if( in->obs ) out->obs = astCopy( in->obs );

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for StcObsDataLocation objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for StcObsDataLocation objects.

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
   AstStcObsDataLocation *this;   /* Pointer to StcObsDataLocation */

/* Obtain a pointer to the StcObsDataLocation structure. */
   this = (AstStcObsDataLocation *) obj;

/* Annul the pointer to the observatory location Region. */
   if( this->obs ) this->obs = astAnnul( this->obs );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for StcObsDataLocation objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the StcObsDataLocation class to an output Channel.

*  Parameters:
*     this
*        Pointer to the StcObsDataLocation whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstStcObsDataLocation *this;                 /* Pointer to the StcObsDataLocation structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the StcObsDataLocation structure. */
   this = (AstStcObsDataLocation *) this_object;

/* Write out values representing the instance variables for the
   StcObsDataLocation class.  Accompany these with appropriate comment strings,
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

/* Observatory position. */
/* --------------------- */
   astWriteObject( channel, "ObsLoc", 1, 1, this->obs, "Observatory position" );

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAStcObsDataLocation and astCheckStcObsDataLocation functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(StcObsDataLocation,Stc)
astMAKE_CHECK(StcObsDataLocation)


AstStcObsDataLocation *astStcObsDataLocation_( void *region_void, int ncoords,
                               AstKeyMap **coords, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astStcObsDataLocation
f     AST_STCOBSDATALOCATION

*  Purpose:
*     Create a StcObsDataLocation.

*  Type:
*     Public function.

*  Synopsis:
c     #include "stcobsdatalocation.h"
c     AstStcObsDataLocation *astStcObsDataLocation( AstRegion *region,
c                  int ncoords, AstKeyMap *coords[], const char *options, ... )
f     RESULT = AST_STCOBSDATALOCATION( REGION, NCOORDS, COORDS, OPTIONS, STATUS )

*  Class Membership:
*     StcObsDataLocation constructor.

*  Description:
*     This function creates a new StcObsDataLocation and optionally initialises its
*     attributes.
*
*     The StcObsDataLocation class is a sub-class of Stc used to describe
*     the coverage of the datasets contained in some VO resource.
*
*     See http://hea-www.harvard.edu/~arots/nvometa/STC.html

*  Parameters:
c     region
f     REGION = INTEGER (Given)
*        Pointer to the encapsulated Region.
c     ncoords
f     NCOORDS = INTEGER (Given)
c        The length of the "coords" array. Supply zero if "coords" is NULL.
f        The length of the COORDS array. Supply zero if COORDS should be
f        ignored.
c     coords
f     COORDS( NCOORDS ) = INTEGER (Given)
c        Pointer to an array holding "ncoords" AstKeyMap pointers (if "ncoords"
f        An array holding NCOORDS AstKeyMap pointers (if NCOORDS
*        is zero, the supplied value is ignored). Each supplied KeyMap
*        describes the contents of a single STC <AstroCoords> element, and
*        should have elements with keys given by constants AST__STCNAME,
*        AST__STCVALUE, AST__STCERROR, AST__STCRES, AST__STCSIZE,
*        AST__STCPIXSZ. Any of these elements may be omitted, but no other
*        elements should be included. If supplied, the AST__STCNAME element
*        should be a vector of character string pointers holding the "Name"
*        item for each axis in the coordinate system represented by
c        "region".
f        REGION.
*        Any other supplied elements should be scalar elements, each  holding
*        a pointer to a Region describing the associated item of ancillary
*        information (error, resolution, size, pixel size or value). These
*        Regions should describe a volume within the coordinate system
c        represented by "region".
f        represented by REGION.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new StcObsDataLocation. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new StcObsDataLocation. The syntax used is identical to that for the
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
c     astStcObsDataLocation()
f     AST_STCOBSDATALOCATION = INTEGER
*        A pointer to the new StcObsDataLocation.

*  Notes:
*     - A deep copy is taken of the supplied Region. This means that
*     any subsequent changes made to the encapsulated Region using the
*     supplied pointer will have no effect on the Stc.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstRegion *region;            /* Pointer to Region structure */
   AstStcObsDataLocation *new;   /* Pointer to new StcObsDataLocation */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the Region structure provided. */
   region = astCheckRegion( region_void );

/* Initialise the StcObsDataLocation, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitStcObsDataLocation( NULL, sizeof( AstStcObsDataLocation ), !class_init,
                                    &class_vtab, "StcObsDataLocation", region,
                                    ncoords, coords );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new StcObsDataLocation's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new StcObsDataLocation. */
   return new;
}

AstStcObsDataLocation *astStcObsDataLocationId_( void *region_void, int ncoords,
                               AstKeyMap **coords, const char *options, ... ) {
/*
*  Name:
*     astStcObsDataLocationId_

*  Purpose:
*     Create a StcObsDataLocation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcobsdatalocation.h"
*     AstStcObsDataLocation *astStcObsDataLocationId( AstRegion *region,
*                  int ncoords, AstKeyMap *coords[], const char *options, ..., int *status )

*  Class Membership:
*     StcObsDataLocation constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astStcObsDataLocation constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astStcObsDataLocation_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astStcObsDataLocation_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astStcObsDataLocation_.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The ID value associated with the new StcObsDataLocation.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstKeyMap **keymaps;            /* Pointer to array of KeyMap pointers */
   AstRegion *region;              /* Pointer to Region structure */
   AstStcObsDataLocation *new;     /* Pointer to new StcObsDataLocation */
   int icoord;                     /* Keymap index */
   va_list args;                   /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain a Region pointer from the supplied ID and validate the
   pointer to ensure it identifies a valid Region. */
   region = astVerifyRegion( astMakePointer( region_void ) );

/* Obtain pointer from the supplied KeyMap ID's and validate the
   pointers to ensure it identifies a valid KeyMap. */
   keymaps = astMalloc( sizeof( AstKeyMap * )*(size_t) ncoords );
   if( keymaps ) {
      for( icoord = 0; icoord < ncoords; icoord++ ) {
         keymaps[ icoord ] = astVerifyKeyMap( astMakePointer( coords[ icoord ] ) );
      }
   }

/* Initialise the StcObsDataLocation, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitStcObsDataLocation( NULL, sizeof( AstStcObsDataLocation ), !class_init,
                                    &class_vtab, "StcObsDataLocation", region,
                                    ncoords, keymaps );

/* Free resources. */
   keymaps = astFree( keymaps );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new StcObsDataLocation's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new StcObsDataLocation. */
   return astMakeId( new );
}

AstStcObsDataLocation *astInitStcObsDataLocation_( void *mem, size_t size,
                                    int init, AstStcObsDataLocationVtab *vtab,
                                    const char *name, AstRegion *region,
                                    int ncoords, AstKeyMap **coords, int *status ) {
/*
*+
*  Name:
*     astInitStcObsDataLocation

*  Purpose:
*     Initialise a StcObsDataLocation.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcobsdatalocation.h"
*     AstStcObsDataLocation *astInitStcObsDataLocation_( void *mem, size_t size,
*                                    int init, AstStcObsDataLocationVtab *vtab,
*                                    const char *name, AstRegion *region,
*                                    int ncoords, AstKeyMap **coords )

*  Class Membership:
*     StcObsDataLocation initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new StcObsDataLocation object. It allocates memory (if necessary) to accommodate
*     the StcObsDataLocation plus any additional data associated with the derived class.
*     It then initialises a StcObsDataLocation structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a StcObsDataLocation at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the StcObsDataLocation is to be initialised.
*        This must be of sufficient size to accommodate the StcObsDataLocation data
*        (sizeof(StcObsDataLocation)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the StcObsDataLocation (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the StcObsDataLocation
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the StcObsDataLocation's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new StcObsDataLocation.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     region
*        A pointer to the Region encapsulated by the StcObsDataLocation.
*     ncoords
*        Number of KeyMap pointers supplied in "coords". Can be zero.
*        Ignored if "coords" is NULL.
*     coords
*        Pointer to an array of "ncoords" KeyMap pointers, or NULL if
*        "ncoords" is zero. Each KeyMap defines defines a single <AstroCoords>
*        element, and should have elements with keys given by constants
*        AST__STCNAME, AST__STCVALUE, AST__STCERROR, AST__STCRES, AST__STCSIZE,
*        AST__STCPIXSZ. These elements hold values for the corresponding
*        components of the STC AstroCoords element. Any of these elements may
*        be omitted, but no other elements should be included. All supplied
*        elements should be vector elements, with vector length less than or
*        equal to the number of axes in the supplied Region. The data type of
*        all elements should be "double", except for AST__STCNAME which should
*        be "character string". If no value is available for a given axis, then
*        AST__BAD (or NULL for the AST__STCNAME element) should be stored in
*        the vector at the index corresponding to the axis (trailing axes
*        can be omitted completely from the KeyMap).

*  Returned Value:
*     A pointer to the new StcObsDataLocation.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstStcObsDataLocation *new;       /* Pointer to new StcObsDataLocation */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitStcObsDataLocationVtab( vtab, name );

/* Initialise a Stc structure (the parent class) as the first component
   within the StcObsDataLocation structure, allocating memory if necessary. */
   new = (AstStcObsDataLocation *) astInitStc( mem, size, 0, (AstStcVtab *) vtab,
                                               name, region, ncoords, coords );

/* If succesful, initialise properties of the StcObsDataLocation. */
   if( new ) {
      new->obs = NULL;
   }

/* If an error occurred, clean up by deleting the new StcObsDataLocation. */
   if ( !astOK ) new = astDelete( new );

/* Return a pointer to the new StcObsDataLocation. */
   return new;
}

AstStcObsDataLocation *astLoadStcObsDataLocation_( void *mem, size_t size, AstStcObsDataLocationVtab *vtab,
                                                   const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadStcObsDataLocation

*  Purpose:
*     Load a StcObsDataLocation.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcobsdatalocation.h"
*     AstStcObsDataLocation *astLoadStcObsDataLocation( void *mem, size_t size, AstStcObsDataLocationVtab *vtab,
*                                       const char *name, AstChannel *channel )

*  Class Membership:
*     StcObsDataLocation loader.

*  Description:
*     This function is provided to load a new StcObsDataLocation using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     StcObsDataLocation structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a StcObsDataLocation at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the StcObsDataLocation is to be
*        loaded.  This must be of sufficient size to accommodate the
*        StcObsDataLocation data (sizeof(StcObsDataLocation)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the StcObsDataLocation (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the StcObsDataLocation structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstStcObsDataLocation) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new StcObsDataLocation. If this is NULL, a pointer
*        to the (static) virtual function table for the StcObsDataLocation class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "StcObsDataLocation" is used instead.

*  Returned Value:
*     A pointer to the new StcObsDataLocation.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstStcObsDataLocation *new;              /* Pointer to the new StcObsDataLocation */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this StcObsDataLocation. In this case the
   StcObsDataLocation belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstStcObsDataLocation );
      vtab = &class_vtab;
      name = "StcObsDataLocation";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitStcObsDataLocationVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built StcObsDataLocation. */
   new = astLoadStc( mem, size, (AstStcVtab *) vtab, name, channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "StcObsDataLocation" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Observatory position. */
/* --------------------- */
      new->obs = astReadObject( channel, "obsloc", NULL );

/* If an error occurred, clean up by deleting the new StcObsDataLocation. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new StcObsDataLocation pointer. */
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

void astStcSetObs_( AstStcObsDataLocation *this, AstPointList *obs, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,StcObsDataLocation,StcSetObs))( this, obs, status );
}




