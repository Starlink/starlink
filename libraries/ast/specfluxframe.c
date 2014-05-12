/*
*class++
*  Name:
*     SpecFluxFrame

*  Purpose:
*     Compound spectrum/flux Frame.

*  Constructor Function:
c     astSpecFluxFrame
f     AST_SPECFLUXFRAME

*  Description:
*     A SpecFluxFrame combines a SpecFrame and a FluxFrame into a single
*     2-dimensional compound Frame. Such a Frame can for instance be used
*     to describe a Plot of a spectrum in which the first axis represents
*     spectral position and the second axis represents flux.

*  Inheritance:
*     The SpecFluxFrame class inherits from the CmpFrame class.

*  Attributes:
*     The SpecFluxFrame class does not define any new attributes beyond
*     those which are applicable to all CmpFrames. However, the attributes
*     of the component Frames can be accessed as if they were attributes
*     of the SpecFluxFrame. For instance, the SpecFluxFrame will recognise
*     the "StdOfRest" attribute and forward access requests to the component
*     SpecFrame. An axis index can optionally be appended to the end of any
*     attribute name, in which case the request to access the attribute will
*     be forwarded to the primary Frame defining the specified axis.

*  Functions:
c     The SpecFluxFrame class does not define any new functions beyond those
f     The SpecFluxFrame class does not define any new routines beyond those
*     which are applicable to all CmpFrames.

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
*     8-DEC-2004 (DSB):
*        Original version.
*     29-APR-2011 (DSB):
*        Prevent astFindFrame from matching a subclass template against a
*        superclass target.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SpecFluxFrame

/* Define the first and last acceptable System values. */
#define FIRST_SYSTEM AST__COMP
#define LAST_SYSTEM AST__COMP

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "mapping.h"             /* Coordinate Mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "permmap.h"             /* Coordinate permutation Mappings */
#include "cmpmap.h"              /* Compound Mappings */
#include "axis.h"                /* Coordinate axes */
#include "cmpframe.h"            /* Parent CmpFrame class */
#include "tranmap.h"             /* Separated transformation Mappings */
#include "mathmap.h"             /* Algebraic Mappings */
#include "ratemap.h"             /* Differential Mappings */
#include "specframe.h"           /* SpecFrame class */
#include "fluxframe.h"           /* FluxFrame class */
#include "specfluxframe.h"       /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_match)( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
static int (* parent_subframe)( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
static const char *(* parent_gettitle)( AstFrame *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetTitle_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(SpecFluxFrame)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(SpecFluxFrame,Class_Init)
#define class_vtab astGLOBAL(SpecFluxFrame,Class_Vtab)
#define gettitle_buff astGLOBAL(SpecFluxFrame,GetTitle_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char gettitle_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSpecFluxFrameVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSpecFluxFrame *astSpecFluxFrameId_( void *, void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstFluxFrame *GetFluxFrame( AstSpecFluxFrame *, int, int * );
static AstMapping *MakeMap2( AstSpecFluxFrame *, int * );
static AstMapping *MakeMap3( AstSpecFluxFrame *, AstSpecFluxFrame *, int * );
static AstMapping *MakeMapF( AstFluxFrame *, AstSpecFrame *, AstFluxFrame *, AstSpecFrame *, int * );
static AstMapping *MakeMapI( AstFluxFrame *, AstSpecFrame *, AstFluxFrame *, AstSpecFrame *, int * );
static AstSpecFrame *GetSpecFrame( AstSpecFluxFrame *, int, int * );
static const char *GetTitle( AstFrame *, int * );
static int MakeSFMapping( AstSpecFluxFrame *, AstSpecFluxFrame *, AstMapping **, int * );
static int Match( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
static int SubFrame( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
static void Dump( AstObject *, AstChannel *, int * );

/* Member functions. */
/* ================= */

static AstFluxFrame *GetFluxFrame( AstSpecFluxFrame *this, int std, int *status ){
/*
*  Name:
*     GetFluxFrame

*  Purpose:
*     Return a pointer to the FluxFrame in a FluxSpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstFluxFrame *GetFluxFrame( AstSpecFluxFrame *this, int std, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     Returns a pointer to the FluxFrame in a SpecFluxFrame.

*  Parameters:
*     this
*        Pointer to the SpecFluxFrame.
*     std
*        If non zero, then the returned FluxFrame is a standardised copy of
*        the FluxFrame in the supplied SpecFluxFrame, in which the System has
*        been set explicitly (rather than potentially being defaulted), and
*        the Units have been cleared to use default units appropriate to
*        the flux System.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the FluxFrame. Should be freed using astAnnul when no
*     longer needed.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables; */
   AstFluxFrame *ff;
   AstFluxFrame *ret;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* The FluxFrame is always the second Frame in the parent CmpFrame. */
   ff = (AstFluxFrame *) ((AstCmpFrame *)this)->frame2;

/* Produce a standardised copy of the FluxFrame if required, or clone the
   above pointer otherwise. */
   if( std ) {
      ret = astCopy( ff );
      astSetSystem( ret, astGetSystem( ff ) );
      astClearUnit( ret, 0 );
   } else {
      ret = astClone( ff );
   }

/* Annul the returned pointer if anything went wrong. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result. */
   return ret;
}

static AstSpecFrame *GetSpecFrame( AstSpecFluxFrame *this, int std, int *status ){
/*
*  Name:
*     GetSpecFrame

*  Purpose:
*     Return a pointer to the SpecFrame in a FluxSpecFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstSpecFrame *GetSpecFrame( AstSpecFluxFrame *this, int std, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     Returns a pointer to the SpecFrame in a SpecFluxFrame.

*  Parameters:
*     this
*        Pointer to the SpecFluxFrame.
*     std
*        If non zero, then the returned SpecFrame is a standardised copy of
*        the SpecFrame in the supplied SpecFluxFrame, in which the System
*        and Units have been set explicitly to the values appropriate to the
*        flux system in use in the FluxFrame in the supplied SpecFluxFrame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the FluxFrame. Should be freed using astAnnul when no
*     longer needed.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables; */
   AstFluxFrame *ff;
   AstSpecFrame *ret;
   AstSpecFrame *sf;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the SpecFrame (the first Frame in the parent CmpFrame). */
   sf = (AstSpecFrame *) ((AstCmpFrame *)this)->frame1;

/* If we want a standardised version of the SpecFrame... */
   if( std ) {

/* The FluxFrame is always the second Frame in the parent CmpFrame. */
      ff = (AstFluxFrame *) ((AstCmpFrame *)this)->frame2;

/* Produce a copy of the SpecFrame and set its System and Units
   appropriate to the flux system (expressed in default units). */
      ret = astCopy( sf );
      astSetSystem( ret, astGetDensitySystem( ff ) );
      astSetUnit( ret, 0, astGetDensityUnit( ff ) );

/* If we are not standardising the SpecFrame, just return a clone of the
   pointer in the parent CmpFrame. */
   } else {
      ret = astClone( sf );
   }

/* Annul the returned pointer if anything went wrong. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result. */
   return ret;
}

static const char *GetTitle( AstFrame *this_frame, int *status ) {
/*
*  Name:
*     GetTitle

*  Purpose:
*     Obtain a pointer to the Title string for a SpecFluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     const char *GetTitle( AstFrame *this_frame, int *status )

*  Class Membership:
*     SpecFluxFrame member function (over-rides the astGetTitle method
*     inherited from the CmpFrame class).

*  Description:
*     This function returns a pointer to the Title string for a SpecFluxFrame.
*     A pointer to a suitable default string is returned if no Title value has
*     previously been set.

*  Parameters:
*     this
*        Pointer to the SpecFluxFrame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a null-terminated character string containing the requested
*     information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS
   AstSpecFluxFrame *this;
   AstSpecFrame *sf;
   AstFluxFrame *ff;
   const char *result;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_frame);

/* Initialise. */
   result = NULL;

/* Obtain a pointer to the SpecFluxFrame structure. */
   this = (AstSpecFluxFrame *) this_frame;

/* See if a Title string has been set. If so, use the parent astGetTitle
   method to obtain a pointer to it. */
   if ( astTestTitle( this ) ) {
      result = (*parent_gettitle)( this_frame, status );

/* Otherwise, we will generate a default Title string. Obtain the values of the
   SpecFrame's attributes that determine what this string will be. */
   } else {
      ff = GetFluxFrame( this, 0, status );
      sf = GetSpecFrame( this, 0, status );

      if( astOK ) {
         sprintf( gettitle_buff, "%s versus %s", astGetLabel( ff, 0 ),
                  astGetLabel( sf, 0 ) );
         gettitle_buff[ 0 ] = toupper( gettitle_buff[ 0 ] );
         result = gettitle_buff;
      }

      ff = astAnnul( ff );
      sf = astAnnul( sf );

   }

/* If an error occurred, clear the returned pointer value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

void astInitSpecFluxFrameVtab_(  AstSpecFluxFrameVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSpecFluxFrameVtab

*  Purpose:
*     Initialise a virtual function table for a SpecFluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specfluxframe.h"
*     void astInitSpecFluxFrameVtab( AstSpecFluxFrameVtab *vtab, const char *name )

*  Class Membership:
*     SpecFluxFrame vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SpecFluxFrame class.

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
   AstFrameVtab *frame;          /* Pointer to Frame component of Vtab */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitCmpFrameVtab( (AstCmpFrameVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsASpecFluxFrame) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstCmpFrameVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   frame = (AstFrameVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

   parent_match = frame->Match;
   frame->Match = Match;

   parent_subframe = frame->SubFrame;
   frame->SubFrame = SubFrame;

   parent_gettitle = frame->GetTitle;
   frame->GetTitle = GetTitle;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetDump( vtab, Dump, "SpecFluxFrame",
               "Compound spectral/flux coordinate system description" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static AstMapping *MakeMap2( AstSpecFluxFrame *this, int *status ){
/*
*  Name:
*     MakeMap2

*  Purpose:
*     Generate the second Mapping required by MakeSFMapping

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstMapping *MakeMap2( AstSpecFluxFrame *this, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     The second Mapping used by MakeSFMapping contains three Mappings in
*     parallel which converts v1 (flux value) and x1 (spectral position) into
*     default units, and passes the third axis (a copy of flux value)
*     unchanged.

*  Parameters:
*     this
*        Pointer to the SpecFluxFrame to use.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the required Mapping, or NULL if the Mapping cannot be
*     created. The Mapping will have 3 inputs and 3 outputs.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstFrame *f1;
   AstFrame *f2;
   AstFrameSet *fs;
   AstMapping *ax1_map;
   AstMapping *ax2_map;
   AstMapping *ax3_map;
   AstMapping *ret;
   AstMapping *tmap;

/* Initialise. */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Input 0 is the supplied FluxFrame value and output 0 is the corresponding
   value in the default units for the FluxFrame system. Take a copy of the
   supplied FluxFrame, and fix its System value (which may be a default value
   based on the Units string), and then clear the Units so that it represents
   default units for the System. */
   f1 = (AstFrame *) GetFluxFrame( this, 0, status );
   f2 = (AstFrame *) GetFluxFrame( this, 1, status );

/* Now, if conversion was possible, get the Mapping from the supplied
   FluxFrame to the default units FluxFrame. */
   fs = astConvert( f1, f2, "" );
   f1 = astAnnul( f1 );
   f2 = astAnnul( f2 );

   if( fs ) {
      ax1_map = astGetMapping( fs, AST__BASE, AST__CURRENT );
      fs = astAnnul( fs );

/* Input 1 is the supplied SpecFrame value and output 1 is the corresponding
   value in the spectral system used by the flux system (wavelength or
   frequency). Take a copy of the supplied SpecFrame, and fix its System
   value to wavelength or frequency (depending on the System value of the
   FluxFrame), and set up units of Hz or Angstrom (these are the spectral
   position units used within the default flux units for a FluxFrame). */
      f1 = (AstFrame *) GetSpecFrame( this, 0, status );
      f2 = (AstFrame *) GetSpecFrame( this, 1, status );

/* Now, if conversion was possible, get the Mapping from the supplied
   SpecFrame to the required SpecFrame. */
      fs = astConvert( f1, f2, "" );
      f1 = astAnnul( f1 );
      f2 = astAnnul( f2 );

      if( fs ) {
         ax2_map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         fs = astAnnul( fs );

/* Create a UnitMap for the 3rd axis. */
         ax3_map = (AstMapping *) astUnitMap( 1, "", status );

/* Create a parallel CmpMap containing the three Mappings. */
         tmap = (AstMapping *) astCmpMap( ax1_map, ax2_map, 0, "", status );
         ret = (AstMapping *) astCmpMap( tmap, ax3_map, 0, "", status );

/* Free remaining resources. */
         tmap = astAnnul( tmap );
         ax2_map = astAnnul( ax2_map );
         ax3_map = astAnnul( ax3_map );

      }
      ax1_map = astAnnul( ax1_map );
   }

/* If an error has occurred, return NULL. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result */
   return ret;
}

static AstMapping *MakeMap3( AstSpecFluxFrame *target, AstSpecFluxFrame *result, int *status ){
/*
*  Name:
*     MakeMap3

*  Purpose:
*     Generate the third Mapping required by MakeSFMapping

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstMapping *MakeMap3( AstSpecFluxFrame *target, AstSpecFluxFrame *result, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     The third Mapping used by MakeSFMapping converts input (v1,x1) in
*     default units to output (v2,x2) in default units. The third axis (x1)
*     in original units is converted to x2 in original units.

*  Parameters:
*     target
*        Pointer to the first SpecFluxFrame.
*     result
*        Pointer to the second SpecFluxFrame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the required Mapping, or NULL if the Mapping cannot be
*     created. The Mapping will have 3 inputs and 3 outputs.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstFluxFrame *ff2;
   AstFluxFrame *ff1;
   AstFrameSet *fs;
   AstMapping *fmap;
   AstMapping *imap;
   AstMapping *mapa;
   AstMapping *mapb;
   AstMapping *ret;
   AstSpecFrame *sf2;
   AstSpecFrame *sf1;

/* Initialise */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* The first two inputs and outputs are related by a TranMap which
   converts between standardised (v1,x1) and standardised (v2,x2). Get
   pointers to the standardised SpecFrames and FluxFrames in the two
   supplied SpecFluxFrames. */
   ff1 = GetFluxFrame( target, 1, status );
   sf1 = GetSpecFrame( target, 1, status );
   ff2 = GetFluxFrame( result, 1, status );
   sf2 = GetSpecFrame( result, 1, status );

/* Create the Mapping which defines the forward transformation of the
   required TranMap. The forward transformation of this Mapping goes from
   (v1,x1) to (v2,x2). */
   fmap = MakeMapF( ff1, sf1, ff2, sf2, status );

/* Create the Mapping which defines the inverse transformation of the
   required TranMap. The inverse transformation of this Mapping goes from
   (v2,x2) to (v1,x1). */
   imap = MakeMapI( ff1, sf1, ff2, sf2, status );

/* Combine these into a TranMap */
   if( fmap && imap ) {
      mapa = (AstMapping *) astTranMap( fmap, imap, "", status );
   } else {
      mapa = NULL;
   }

/* Free resources. */
   ff1 = astAnnul( ff1 );
   sf1 = astAnnul( sf1 );
   ff2 = astAnnul( ff2 );
   sf2 = astAnnul( sf2 );
   if( fmap ) fmap = astAnnul( fmap );
   if( imap ) imap = astAnnul( imap );

/* The third input and output are related by a Mapping which converts
   between supplied (x1) and supplied (x2). Get pointers to the original
   unmodified SpecFrames in the two supplied SpecFluxFrames. */
   sf1 = GetSpecFrame( target, 0, status );
   sf2 = GetSpecFrame( result, 0, status );

/* Find the Mapping from the first to the second. */
   fs = astConvert( sf1, sf2, "" );
   if( fs ) {
      mapb = astGetMapping( fs, AST__BASE, AST__CURRENT );
      fs = astAnnul( fs );
   } else {
      mapb = NULL;
   }

/* Free resources. */
   sf1 = astAnnul( sf1 );
   sf2 = astAnnul( sf2 );

/* Combine the two Mappings in parallel. */
   if( mapa && mapb ) ret = (AstMapping *) astCmpMap( mapa, mapb, 0, "", status );
   if( mapa ) mapa = astAnnul( mapa );
   if( mapb ) mapb = astAnnul( mapb );

/* If an error has occurred, return NULL. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result */
   return ret;
}

static AstMapping *MakeMapF( AstFluxFrame *v1, AstSpecFrame *x1,
                             AstFluxFrame *v2, AstSpecFrame *x2, int *status ){
/*
*  Name:
*     MakeMapF

*  Purpose:
*     Generate the forward part of the third Mapping required by MakeSFMapping

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstMapping *MakeMapF( AstFluxFrame *v1, AstSpecFrame *x1,
*                           AstFluxFrame *v2, AstSpecFrame *x2, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     Theis creates a 2-input 2-output Mapping which transforms
*     input (v1,x1) in default units to output (v2,x2) in default units.

*  Parameters:
*     v1
*        Pointer to the standardised input FluxFrame.
*     x1
*        Pointer to the standardised input SpecFrame.
*     v2
*        Pointer to the standardised output FluxFrame.
*     x2
*        Pointer to the standardised output SpecFrame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the required Mapping, or NULL if the Mapping cannot be
*     created.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpMap *cmap1;
   AstCmpMap *cmap2;
   AstCmpMap *cmap3;
   AstFrameSet *fs;
   AstMapping *m;
   AstMapping *ret;
   AstMathMap *div;
   AstPermMap *perm;
   AstRateMap *rate;
   AstUnitMap *unit;
   const char *fwd[1];
   const char *inv[2];
   int inperm[ 2 ];
   int outperm[ 3 ];

/* Initialise */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* First create the required component Mappings.
   --------------------------------------------- */

/* A Mapping which maps input spectral position (x1) into output spectral
   position (x2). */
   fs = astConvert( x1, x2, "" );
   if( fs ) {
      m = astGetMapping( fs, AST__BASE, AST__CURRENT );

/* A 1-input 1-output Mapping in which the input is spectral position (x1)
   and the output is the rate of change of output spectral position (x2)
   with respect to input spectral position (x1). */
      rate = astRateMap( m, 0, 0, "", status );

/* A MathMap which is used to divide the flux value (v1) by the absolute rate
   of change of x2 wrt x1 */
      fwd[ 0 ] = "out=in0/abs(in1)";
      inv[ 0 ] = "in0";
      inv[ 1 ] = "in1";
      div = astMathMap( 2, 1, 1, fwd, 2, inv, "", status );

/* A 1D UnitMap used to copy v1. */
      unit = astUnitMap( 1, "", status );

/* A PermMap which is used to produce an extra output copy of x1. */
      inperm[ 0 ] = 0;
      inperm[ 1 ] = 2;
      outperm[ 0 ] = 0;
      outperm[ 1 ] = 1;
      outperm[ 2 ] = 1;
      perm = astPermMap( 2, inperm, 3, outperm, NULL, "", status );

/* Now combine these component Mappings together.
   --------------------------------------------- */

/* First put the UnitMap and the RateMap in parallel. This produces a 2-in
   2-out Mapping in which the inputs are (v1,x1) and the outputs are
   (v1,dx2/dx1). */
      cmap1 = astCmpMap( unit, rate, 0, "", status );

/* Now put this in series with the dividing MathMap. This results in a
   2-in, 1-out Mapping in which the inputs are v1 and x1 and the single
   output is v2. */
      cmap2 = astCmpMap( cmap1, div, 1, "", status );

/* Now put this in parallel with the x1->x2 Mapping. This results in a
   3-in, 2-out Mapping in which the inputs are (v1,x1,x1) and the outputs
   are (v2,x2). */
      cmap3 = astCmpMap( cmap2, m, 0, "", status );

/* Finally put this in series with the PermMap. This results in a 2-in,
   2-out Mapping in which the inputs are (v1,x1) and the outputs are
   (v2,x2). */
      ret = (AstMapping *) astCmpMap( perm, cmap3, 1, "", status );

/* Free resources. */
      fs = astAnnul( fs );
      m = astAnnul( m );
      rate = astAnnul( rate );
      div= astAnnul( div );
      unit = astAnnul( unit );
      perm = astAnnul( perm );
      cmap1 = astAnnul( cmap1 );
      cmap2 = astAnnul( cmap2 );
      cmap3 = astAnnul( cmap3 );
   }

/* If an error has occurred, return NULL. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result */
   return ret;
}

static AstMapping *MakeMapI( AstFluxFrame *v1, AstSpecFrame *x1,
                             AstFluxFrame *v2, AstSpecFrame *x2, int *status ){
/*
*  Name:
*     MakeMapI

*  Purpose:
*     Generate the inverse part of the third Mapping required by MakeSFMapping

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstMapping *MakeMapI( AstFluxFrame *v1, AstSpecFrame *x1,
*                           AstFluxFrame *v2, AstSpecFrame *x2 )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     This creates a 2-input 2-output Mapping in which the inverse
*     transformation transforms "outputs" representing (v2,x2) into
*     "inputs" representing (v1,x1).

*  Parameters:
*     v1
*        Pointer to the standardised input FluxFrame.
*     x1
*        Pointer to the standardised input SpecFrame.
*     v2
*        Pointer to the standardised output FluxFrame.
*     x2
*        Pointer to the standardised output SpecFrame.

*  Returned Value:
*     A pointer to the required Mapping, or NULL if the Mapping cannot be
*     created.

*  Notes:
*     NULL is returned if this function is invoked with the global error
*     status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpMap *cmap1;
   AstCmpMap *cmap2;
   AstCmpMap *cmap3;
   AstCmpMap *cmap4;
   AstCmpMap *cmap5;
   AstFrameSet *fs;
   AstMapping *m;
   AstMapping *ret;
   AstMathMap *mult;
   AstPermMap *perm;
   AstRateMap *rate;
   AstUnitMap *unit;
   const char *fwd[1];
   const char *inv[2];
   int inperm[ 2 ];
   int outperm[ 3 ];

/* Initialise */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* We create a CmpMap in which the forward transformation foes from
   (v2,x2) to (v1,x1) and we finally invert this Mapping to get the
   required Mapping in which the *inverse* transformation goes from
   (v2,x2) to (v1,x1).

   First create the required component Mappings.
   --------------------------------------------- */

/* A Mapping which maps spectral position x1 into spectral position x2. */
   fs = astConvert( x1, x2, "" );
   if( fs ) {
      m = astGetMapping( fs, AST__BASE, AST__CURRENT );

/* A 1-input 1-output Mapping in which the input is spectral position x1
   and the output is the rate of change of spectral position x2 with
   respect to spectral position x1. */
      rate = astRateMap( m, 0, 0, "", status );

/* Now invert "m" so that its forward transformation goes from x2 to x1.
   The RateMap created above retains a copy of the original Invert flag
   for "m" and uses it in preference to the current value when transforming
   points. */
      astInvert( m );

/* A MathMap which is used to multiple the flux value v2 by the
   absolute rate of change of x2 wrt x1 */
      fwd[ 0 ] = "out=in0*abs(in1)";
      inv[ 0 ] = "in0";
      inv[ 1 ] = "in1";
      mult = astMathMap( 2, 1, 1, fwd, 2, inv, "", status );

/* A 1D UnitMap used to copy various values. */
      unit = astUnitMap( 1, "", status );

/* A PermMap which is used to produce an extra copy of x1. */
      inperm[ 0 ] = 0;
      inperm[ 1 ] = 2;
      outperm[ 0 ] = 0;
      outperm[ 1 ] = 1;
      outperm[ 2 ] = 1;
      perm = astPermMap( 2, inperm, 3, outperm, NULL, "", status );

/* Now combine these component Mappings together.
   --------------------------------------------- */

/* First put the UnitMap and the RateMap in parallel. This produces a 2-in
   2-out Mapping in which the inputs are (v2,x1) and the outputs are
   (v2,dx2/dx1). */
      cmap1 = astCmpMap( unit, rate, 0, "", status );

/* Now put this in series with the multiplying MathMap. This results in a
   2-in, 1-out Mapping in which the inputs are (v2,x1) and the single
   output is v1. */
      cmap2 = astCmpMap( cmap1, mult, 1, "", status );

/* Now put this in parallel with the UnitMap to get a 3-in, 2-out Mapping
   in which the inputs are (v2,x1,x1) and the outputs are (v1,x1). */
      cmap3 = astCmpMap( cmap2, unit, 0, "", status );

/* Now put this in series with the PermMap to get a 2-in, 2-out Mapping
   in which the inputs are (v2,x1) and the outputs are (v1,x1). */
      cmap4 = astCmpMap( perm, cmap3, 1, "", status );

/* Now put the UnitMap in parallel with the (x2->x1 Mapping to get a
   2-in, 2-out Mapping in which the inputs are (v2,x2) and the outputs are
   (v2,x1). */
      cmap5 = astCmpMap( unit, m, 0, "", status );

/* Finally put this in series with "cmap4" to get a 2-in 2-out Mapping
   from (v2,x2) to (v1,x1). */
      ret = (AstMapping *) astCmpMap( cmap5, cmap4, 1, "", status );

/* Invert this so that the inverse transformation goes from (v2,x2) to
   (v1,x1). */
      astInvert( ret );

/* Free resources. */
      fs = astAnnul( fs );
      m = astAnnul( m );
      rate = astAnnul( rate );
      mult = astAnnul( mult );
      unit = astAnnul( unit );
      perm = astAnnul( perm );
      cmap1 = astAnnul( cmap1 );
      cmap2 = astAnnul( cmap2 );
      cmap3 = astAnnul( cmap3 );
      cmap4 = astAnnul( cmap4 );
      cmap5 = astAnnul( cmap5 );
   }

/* If an error has occurred, return NULL. */
   if( !astOK ) ret = astAnnul( ret );

/* Return the result */
   return ret;
}

static int MakeSFMapping( AstSpecFluxFrame *target, AstSpecFluxFrame *result,
                          AstMapping **map, int *status ){
/*
*  Name:
*     MakeSFMapping

*  Purpose:
*     Generate a Mapping between two SpecFluxFrames.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     int MakeSFMapping( AstSpecFluxFrame *target, AstSpecFluxFrame *result,
*                        AstMapping **map, int *status )

*  Class Membership:
*     SpecFluxFrame member function.

*  Description:
*     This function takes two SpecFluxFrames and generates a Mapping that
*     converts between them, taking account of differences in their
*     coordinate systems, systems, units, etc. (but not allowing for any
*     axis permutations).

*  Parameters:
*     target
*        Pointer to the first SpecFluxFrame.
*     result
*        Pointer to the second SpecFluxFrame.
*     map
*        Pointer to a location which is to receive a pointer to the
*        returned Mapping. The forward transformation of this Mapping
*        will convert from "target" coordinates to "result"
*        coordinates, and the inverse transformation will convert in
*        the opposite direction (all coordinate values in radians).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the Mapping could be generated, or zero if the two
*     SpecFluxFrames are sufficiently un-related that no meaningful Mapping
*     can be produced.

*  Notes:
*     A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstMapping *map1;
   AstMapping *map2;
   AstMapping *map3;
   AstMapping *map4;
   AstMapping *map5;
   AstMapping *tmap1;
   AstMapping *tmap2;
   AstMapping *tmap3;
   AstMapping *tmap4;
   int inperm[2];
   int match;
   int outperm[3];

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the returned values. */
   match = 0;
   *map = NULL;

/* Initialise other things. */
   map1 = NULL;
   map2 = NULL;
   map3 = NULL;
   map4 = NULL;
   map5 = NULL;
   tmap1 = NULL;
   tmap2 = NULL;
   tmap3 = NULL;
   tmap4 = NULL;

/* At the top level, the required Mapping consists of five Mappings in
   series. Inputs 0 and 1 of the total Mapping correspond to the SpecFrame
   and FluxFrame in the target SpecFluxFrame. These are referred to as X1
   and V1. Outputs 0 and 1 of the total Mapping correspond to the SpecFrame
   and FluxFrame in the result SpecFluxFrame. These are referred to as X2
   and V2. */

/* Map1 is a PermMap which copies v1 to its first output and x1 to its
   second and third outputs. The inverse transformation copies v1 from
   its first output and x1 from its third output. */
   inperm[ 0 ] = 2;
   inperm[ 1 ] = 0;
   outperm[ 0 ] = 1;
   outperm[ 1 ] = 0;
   outperm[ 2 ] = 0;
   map1 = (AstMapping *) astPermMap( 2, inperm, 3, outperm, NULL, "", status );

/* Map2 contains three Mappings in parallel which converts v1 and x1 into
   default units, and passes the third axis unchanged. */
   map2 = MakeMap2( target, status );

/* Map3 converts ( v1,x1) in default units to (v2,x2) in default units.
   The third axis (x1) in original units is convert to x2 in original
   units. */
   map3 = map2 ? MakeMap3( target, result, status ) : NULL;

/* Map4 converts (v2,x2) in default units to (v2,x2) in original units
   and passes the third axis unchanged. This is similar to Map2 but based
   on the result ratherthan the target, and in the opposite direction. */
   if( map3 ) {
      map4 = MakeMap2( result, status );
      if( map4 ) astInvert( map4 );
   } else {
      map4 = NULL;
   }

/* Map5 is a PermMap which is the inverse of Map1. */
   map5 = map4 ? astCopy( map1 ) : NULL;
   if( map5 ) astInvert( map5 );

/* Combine all 6 Mappings in series. */
   if( map5 ) {
      tmap1 = (AstMapping *) astCmpMap( map1, map2, 1, "", status );
      tmap2 = (AstMapping *) astCmpMap( tmap1, map3, 1, "", status );
      tmap3 = (AstMapping *) astCmpMap( tmap2, map4, 1, "", status );
      tmap4 = (AstMapping *) astCmpMap( tmap3, map5, 1, "", status );

/* Return the simplified total Mapping. */
      *map = astSimplify( tmap4 );
      match = 1;
   }

/* Free resources. */
   if( map1 ) map1 = astAnnul( map1 );
   if( map2 ) map2 = astAnnul( map2 );
   if( map3 ) map3 = astAnnul( map3 );
   if( map4 ) map4 = astAnnul( map4 );
   if( map5 ) map5 = astAnnul( map5 );
   if( tmap1 ) tmap1 = astAnnul( tmap1 );
   if( tmap2 ) tmap2 = astAnnul( tmap2 );
   if( tmap3 ) tmap3 = astAnnul( tmap3 );
   if( tmap4 ) tmap4 = astAnnul( tmap4 );

/* If an error occurred, annul the returned Mapping and clear the
   returned values. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      match = 0;
   }

/* Return the result. */
   return match;
}

static int Match( AstFrame *template_frame, AstFrame *target, int matchsub,
                  int **template_axes, int **target_axes,
                  AstMapping **map, AstFrame **result, int *status ) {
/*
*  Name:
*     Match

*  Purpose:
*     Determine if conversion is possible between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     int Match( AstFrame *template, AstFrame *target, int matchsub,
*                int **template_axes, int **target_axes,
*                AstMapping **map, AstFrame **result, int *status )

*  Class Membership:
*     SpecFluxFrame member function (over-rides the protected astMatch
*     method inherited from the Frame class).

*  Description:
*     This function matches a "template" SpecFluxFrame to a "target" Frame
*     and determines whether it is possible to convert coordinates
*     between them.  If it is, a Mapping that performs the
*     transformation is returned along with a new Frame that describes
*     the coordinate system that results when this Mapping is applied
*     to the "target" coordinate system. In addition, information is
*     returned to allow the axes in this "result" Frame to be
*     associated with the corresponding axes in the "target" Frame and
*     "template" SpecFluxFrame from which they are derived.

*  Parameters:
*     template
*        Pointer to the template SpecFluxFrame. This describes the
*        coordinate system (or set of possible coordinate systems)
*        into which we wish to convert our coordinates.
*     target
*        Pointer to the target Frame. This describes the coordinate
*        system in which we already have coordinates.
*     matchsub
*        If zero then a match only occurs if the template is of the same
*        class as the target, or of a more specialised class. If non-zero
*        then a match can occur even if this is not the case.
*     template_axes
*        Address of a location where a pointer to int will be returned
*        if the requested coordinate conversion is possible. This
*        pointer will point at a dynamically allocated array of
*        integers with one element for each axis of the "result" Frame
*        (see below). It must be freed by the caller (using astFree)
*        when no longer required.
*
*        For each axis in the result Frame, the corresponding element
*        of this array will return the (zero-based) index of the
*        template SpecFluxFrame axis from which it is derived. If it is not
*        derived from any template axis, a value of -1 will be
*        returned instead.
*     target_axes
*        Address of a location where a pointer to int will be returned
*        if the requested coordinate conversion is possible. This
*        pointer will point at a dynamically allocated array of
*        integers with one element for each axis of the "result" Frame
*        (see below). It must be freed by the caller (using astFree)
*        when no longer required.
*
*        For each axis in the result Frame, the corresponding element
*        of this array will return the (zero-based) index of the
*        target Frame axis from which it is derived. If it is not
*        derived from any target axis, a value of -1 will be returned
*        instead.
*     map
*        Address of a location where a pointer to a new Mapping will
*        be returned if the requested coordinate conversion is
*        possible. If returned, the forward transformation of this
*        Mapping may be used to convert coordinates between the
*        "target" Frame and the "result" Frame (see below) and the
*        inverse transformation will convert in the opposite
*        direction.
*     result
*        Address of a location where a pointer to a new Frame will be
*        returned if the requested coordinate conversion is
*        possible. If returned, this Frame describes the coordinate
*        system that results from applying the returned Mapping
*        (above) to the "target" coordinate system. In general, this
*        Frame will combine attributes from (and will therefore be
*        more specific than) both the target Frame and the template
*        SpecFluxFrame. In particular, when the template allows the
*        possibility of transformaing to any one of a set of
*        alternative coordinate systems, the "result" Frame will
*        indicate which of the alternatives was used.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if the requested coordinate
*     conversion is possible. Otherwise zero is returned (this will
*     not in itself result in an error condition).

*  Notes:
*     - By default, the "result" Frame will have its number of axes
*     and axis order determined by the "template" SpecFluxFrame. However,
*     if the PreserveAxes attribute of the template SpecFluxFrame is
*     non-zero, then the axis count and axis order of the "target"
*     Frame will be used instead.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstSpecFluxFrame *template;   /* Pointer to template SpecFluxFrame structure */
   int match;                    /* Coordinate conversion possible? */
   int swap1;                    /* Template axes swapped? */
   int swap2;                    /* Target axes swapped? */
   int swap;                     /* Additional axis swap needed? */

/* Initialise the returned values. */
   *template_axes = NULL;
   *target_axes = NULL;
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the template SpecFluxFrame structure. */
   template = (AstSpecFluxFrame *) template_frame;

/* If the target is not a SpecFluxFrame, use the results returned by the
   parent Match method inherited from the CmpFrame class. */
   if( !astIsASpecFluxFrame( target ) ) {
      match = (*parent_match)( template_frame, target, matchsub, template_axes,
                               target_axes, map, result, status );


/* If the target is a SpecFluxFrame, see if we can convert between target
   and template */
   } else {

/* We must now decide how the order of the axes in the result Frame relates to
   the order of axes in the target Frame. There are two factors involved. The
   first depends on whether the axis permutation array for the template
   SpecFluxFrame (whose method we are executing) causes an axis
   reversal. Determine this by permuting axis index zero. */
      swap1 = ( astValidateAxis( template, 0, 1, "astMatch" ) != 0 );

/* The second factor depends on whether the axes of the target SpecFluxFrame
   causes an axis reversal. Determine this by permuting axis index zero. */
      swap2 = ( astValidateAxis( target, 0, 1, "astMatch" ) != 0 );

/* Combine these to determine if an additional axis swap will be
   needed. */
      swap = ( swap1 != swap2 );

/* Now check to see if this additional swap is permitted by the template's
   Permute attribute. */
      match = ( !swap || astGetPermute( template ) );

/* Allocate the target and template axes arrays. */
      *template_axes = astMalloc( sizeof(int)*2 );
      *target_axes = astMalloc( sizeof(int)*2 );

/* If the Frames still match, we next set up the axis association
   arrays. */
      if ( astOK && match ) {

/* If the target axis order is to be preserved, then the target axis
   association involves no permutation but the template axis
   association may involve an axis swap. */
         if ( astGetPreserveAxes( template ) ) {
            (*template_axes)[ 0 ] = swap;
            (*template_axes)[ 1 ] = !swap;
            (*target_axes)[ 0 ] = 0;
            (*target_axes)[ 1 ] = 1;

/* Otherwise, any swap applies to the target axis association
   instead. */
         } else {
            (*template_axes)[ 0 ] = 0;
            (*template_axes)[ 1 ] = 1;
            (*target_axes)[ 0 ] = swap;
            (*target_axes)[ 1 ] = !swap;
         }

/* Use the target's "astSubFrame" method to create a new Frame (the
   result Frame) with copies of the target axes in the required
   order. This process also overlays the template attributes on to the
   target Frame and returns a Mapping between the target and result
   Frames which effects the required coordinate conversion. */
         match = astSubFrame( target, template, 2, *target_axes, *template_axes,
                              map, result );

/* If an error occurred, or conversion to the result Frame's
   coordinate system was not possible, then free all memory, annul the
   returned objects, and reset the returned value. */
         if ( !astOK || !match ) {
            *template_axes = astFree( *template_axes );
            *target_axes = astFree( *target_axes );
            if( *map ) *map = astAnnul( *map );
            if( *result ) *result = astAnnul( *result );
            match = 0;
         }
      }
   }

/* Return the result. */
   return match;
}

static int SubFrame( AstFrame *target_frame, AstFrame *template,
                     int result_naxes, const int *target_axes,
                     const int *template_axes, AstMapping **map,
                     AstFrame **result, int *status ) {
/*
*  Name:
*     SubFrame

*  Purpose:
*     Select axes from a SpecFluxFrame and convert to the new coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     int SubFrame( AstFrame *target, AstFrame *template,
*                   int result_naxes, const int *target_axes,
*                   const int *template_axes, AstMapping **map,
*                   AstFrame **result, int *status )

*  Class Membership:
*     SpecFluxFrame member function (over-rides the protected astSubFrame
*     method inherited from the Frame class).

*  Description:
*     This function selects a requested sub-set (or super-set) of the
*     axes from a "target" SpecFluxFrame and creates a new Frame with
*     copies of the selected axes assembled in the requested order. It
*     then optionally overlays the attributes of a "template" Frame on
*     to the result. It returns both the resulting Frame and a Mapping
*     that describes how to convert between the coordinate systems
*     described by the target and result Frames. If necessary, this
*     Mapping takes account of any differences in the Frames'
*     attributes due to the influence of the template.

*  Parameters:
*     target
*        Pointer to the target SpecFluxFrame, from which axes are to be selected.
*     template
*        Pointer to the template Frame, from which new attributes for
*        the result Frame are to be obtained. Optionally, this may be
*        NULL, in which case no overlaying of template attributes will
*        be performed.
*     result_naxes
*        Number of axes to be selected from the target Frame. This
*        number may be greater than or less than the number of axes in
*        this Frame (or equal).
*     target_axes
*        Pointer to an array of int with result_naxes elements, giving
*        a list of the (zero-based) axis indices of the axes to be
*        selected from the target SpecFluxFrame. The order in which these
*        are given determines the order in which the axes appear in
*        the result Frame. If any of the values in this array is set
*        to -1, the corresponding result axis will not be derived from
*        the target Frame, but will be assigned default attributes
*        instead.
*     template_axes
*        Pointer to an array of int with result_naxes elements. This
*        should contain a list of the template axes (given as
*        zero-based axis indices) with which the axes of the result
*        Frame are to be associated. This array determines which axes
*        are used when overlaying axis-dependent attributes of the
*        template on to the result. If any element of this array is
*        set to -1, the corresponding result axis will not receive any
*        template attributes.
*
*        If the template argument is given as NULL, this array is not
*        used and a NULL pointer may also be supplied here.
*     map
*        Address of a location to receive a pointer to the returned
*        Mapping.  The forward transformation of this Mapping will
*        describe how to convert coordinates from the coordinate
*        system described by the target SpecFluxFrame to that described by
*        the result Frame. The inverse transformation will convert in
*        the opposite direction.
*     result
*        Address of a location to receive a pointer to the result Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if coordinate conversion is
*     possible between the target and the result Frame. Otherwise zero
*     is returned and *map and *result are returned as NULL (but this
*     will not in itself result in an error condition). In general,
*     coordinate conversion should always be possible if no template
*     Frame is supplied but may not always be possible otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.

*  Implementation Deficiencies:
*     - It is not clear that the method of handling "extra" axes is
*     the best one, nor is the method of setting the "following" flag
*     necessarily correct.  However, it is also not obvious that this
*     feature will ever be needed, so improvements have been left
*     until the requirement is clearer.
*/

/* Local Variables: */
   AstMapping *tmpmap;           /* Temporary Mapping pointer */
   AstPermMap *permmap;          /* Pointer to PermMap */
   AstSpecFluxFrame *target;     /* Pointer to target SpecFluxFrame structure */
   int match;                    /* Coordinate conversion is possible? */
   int perm[ 2 ];                /* Permutation array for axis swap */
   int result_swap;              /* Swap result SpecFluxFrame coordinates? */
   int target_swap;              /* Swap target SpecFluxFrame coordinates? */

/* Initialise the returned values. */
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* If the template is not a SpecFluxFrame we use the parent SubFrame
   method inherited form the CmpFrame class. */
   if( !template || !astIsASpecFluxFrame( template ) || result_naxes != 2 ) {
      match = (*parent_subframe)( target_frame, template, result_naxes,
                                  target_axes, template_axes, map, result, status );

/* Otherwise... */
   } else {

/* Obtain a pointer to the target SpecFluxFrame structure. */
      target = (AstSpecFluxFrame *) target_frame;

/* Form the result from a copy of the target and then permute its axes
   into the order required. */
      *result = astCopy( target );
      astPermAxes( *result, target_axes );

/* Overlay the template attributes on to the result SpecFrame. */
      astOverlay( template, template_axes, *result );

/* Generate a Mapping that takes account of changes in the coordinate
   system (system, units, etc.) between the target SpecFluxFrame and the
   result SpecFluxFrame. If this Mapping can be generated, set "match" to
   indicate that coordinate conversion is possible. */
      match = MakeSFMapping( target, (AstSpecFluxFrame *) *result, map, status );

/* If a Mapping has been obtained, it will expect coordinate values to be
   supplied in (flux,spec) pairs. Test whether we need to swap the
   order of the target SpecFluxFrame coordinates to conform with this. */
      if ( astOK && match ) {
         target_swap = ( astValidateAxis( target, 0, 1, "astSubFrame" ) != 0 );

/* Coordinates will also be delivered in (flux,spec) pairs, so check
   to see whether the result SpecFluxFrame coordinate order should be
   swapped. */
         result_swap = ( target_swap != ( target_axes[ 0 ] != 0 ) );

/* If either set of coordinates needs swapping, create a PermMap that
   will swap a pair of coordinates. */
         permmap = NULL;
         if ( target_swap || result_swap ) {
            perm[ 0 ] = 1;
            perm[ 1 ] = 0;
            permmap = astPermMap( 2, perm, 2, perm, NULL, "", status );
         }

/* If necessary, prefix this PermMap to the main Mapping. */
         if ( target_swap ) {
            tmpmap = (AstMapping *) astCmpMap( permmap, *map, 1, "", status );
            *map = astAnnul( *map );
            *map = tmpmap;
         }

/* Also, if necessary, append it to the main Mapping. */
         if ( result_swap ) {
            tmpmap = (AstMapping *) astCmpMap( *map, permmap, 1, "", status );
            *map = astAnnul( *map );
            *map = tmpmap;
         }

/* Annul the pointer to the PermMap (if created). */
         if ( permmap ) permmap = astAnnul( permmap );
      }
   }

/* If an error occurred, clean up by annulling the result pointers and
   returning appropriate null values. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   the axes of a SpecFluxFrame using the private macros defined for this
   purpose at the start of this file. */

/* Copy constructor. */
/* ----------------- */

/* Destructor. */
/* ----------- */

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SpecFluxFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SpecFluxFrame class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SpecFluxFrame whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstSpecFluxFrame *this;            /* Pointer to the SpecFluxFrame structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SpecFluxFrame structure. */
   this = (AstSpecFluxFrame *) this_object;

/* Write out values representing the instance variables for the
   SpecFluxFrame class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsASpecFluxFrame and astCheckSpecFluxFrame functions using
   the macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SpecFluxFrame,CmpFrame)
astMAKE_CHECK(SpecFluxFrame)

AstSpecFluxFrame *astSpecFluxFrame_( void *frame1_void, void *frame2_void,
                                     const char *options, int *status, ...) {
/*
*++
*  Name:
c     astSpecFluxFrame
f     AST_SPECFLUXFRAME

*  Purpose:
*     Create a SpecFluxFrame.

*  Type:
*     Public function.

*  Synopsis:
c     #include "specfluxframe.h"
c     AstSpecFluxFrame *astSpecFluxFrame( AstSpecFrame *frame1, AstFluxFrame *frame2,
c                                         const char *options, ... )
f     RESULT = AST_SPECFLUXFRAME( FRAME1, FRAME2, OPTIONS, STATUS )

*  Class Membership:
*     SpecFluxFrame constructor.

*  Description:
*     This function creates a new SpecFluxFrame and optionally initialises
*     its attributes.
*
*     A SpecFluxFrame combines a SpecFrame and a FluxFrame into a single
*     2-dimensional compound Frame. Such a Frame can for instance be used
*     to describe a Plot of a spectrum in which the first axis represents
*     spectral position and the second axis represents flux.

*  Parameters:
c     frame1
f     FRAME1 = INTEGER (Given)
*        Pointer to the SpecFrame. This will form the first axis in the
*        new SpecFluxFrame.
c     frame2
f     FRAME2 = INTEGER (Given)
*        Pointer to the FluxFrame. This will form the second axis in the
*        new SpecFluxFrame. The "SpecVal" attribute of this FluxFrame is
*        not used by the SpecFluxFrame class and so may be set to AST__BAD
*        when the FluxFrame is created.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SpecFluxFrame. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SpecFluxFrame. The syntax used is identical to that for the
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
c     astSpecFluxFrame()
f     AST_SPECFLUXFRAME = INTEGER
*        A pointer to the new SpecFluxFrame.

*  Notes:
*     - The supplied Frame pointers are stored directly, rather than
*     being used to create deep copies of the supplied Frames. This means
*     that any subsequent changes made to the Frames via the supplied
*     pointers will result in equivalent changes being visible in the
*     SpecFluxFrame.
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

*  Implementation Notes:
*     - This function implements the basic SpecFluxFrame constructor which
*     is available via the protected interface to the SpecFluxFrame class.
*     A public interface is provided by the astSpecFluxFrameId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "frame1" and "frame2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSpecFluxFrame *new;        /* Pointer to new SpecFluxFrame */
   AstFluxFrame *frame2;         /* Pointer to FluxFrame structure */
   AstSpecFrame *frame1;         /* Pointer to SpecFrame structure */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   new = NULL;
   if ( !astOK ) return new;

/* Obtain and validate pointers to the Frame structures provided. */
   frame1 = astCheckSpecFrame( frame1_void );
   frame2 = astCheckFluxFrame( frame2_void );
   if ( astOK ) {

/* Initialise the SpecFluxFrame, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSpecFluxFrame( NULL, sizeof( AstSpecFluxFrame ), !class_init,
                                  &class_vtab, "SpecFluxFrame", frame1, frame2 );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   SpecFluxFrame's attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new SpecFluxFrame. */
   return new;
}

AstSpecFluxFrame *astSpecFluxFrameId_( void *frame1_void, void *frame2_void,
                                       const char *options, ... ) {
/*
*  Name:
*     astSpecFluxFrameId_

*  Purpose:
*     Create a SpecFluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstSpecFluxFrame *astSpecFluxFrameId_( void *frame1_void, void *frame2_void,
*                                            const char *options, ... )

*  Class Membership:
*     SpecFluxFrame constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astSpecFluxFrame constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astSpecFluxFrame_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur). For the same reason, the "frame1" and "frame2"
*     parameters are of type (void *) and are converted and validated
*     within the function itself.
*
*     The variable argument list also prevents this function from
*     invoking astSpecFluxFrame_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.

*  Parameters:
*     As for astSpecFluxFrame_.

*  Returned Value:
*     The ID value associated with the new SpecFluxFrame.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSpecFluxFrame *new;        /* Pointer to new SpecFluxFrame */
   AstSpecFrame *frame1;         /* Pointer to first Frame structure */
   AstFluxFrame *frame2;         /* Pointer to second Frame structure */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   new = NULL;
   if ( !astOK ) return new;

/* Obtain the Frame pointers from the ID's supplied and validate the
   pointers to ensure they identify valid Frames. */
   frame1 = astVerifySpecFrame( astMakePointer( frame1_void ) );
   frame2 = astVerifyFluxFrame( astMakePointer( frame2_void ) );
   if ( astOK ) {

/* Initialise the SpecFluxFrame, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSpecFluxFrame( NULL, sizeof( AstSpecFluxFrame ), !class_init,
                                  &class_vtab, "SpecFluxFrame", frame1, frame2 );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   SpecFluxFrame's attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new SpecFluxFrame. */
   return astMakeId( new );
}

AstSpecFluxFrame *astInitSpecFluxFrame_( void *mem, size_t size, int init,
                               AstSpecFluxFrameVtab *vtab, const char *name,
                               AstSpecFrame *frame1, AstFluxFrame *frame2, int *status ) {
/*
*+
*  Name:
*     astInitSpecFluxFrame

*  Purpose:
*     Initialise a SpecFluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstSpecFluxFrame *astInitSpecFluxFrame( void *mem, size_t size, int init,
*                                 AstSpecFluxFrameVtab *vtab, const char *name,
*                                 AstSpecFrame *frame1, AstFluxFrame *frame2 )

*  Class Membership:
*     SpecFluxFrame initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new SpecFluxFrame object. It allocates memory (if
*     necessary) to accommodate the SpecFluxFrame plus any additional data
*     associated with the derived class.  It then initialises a
*     SpecFluxFrame structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for a SpecFluxFrame at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SpecFluxFrame is to be
*        created. This must be of sufficient size to accommodate the
*        SpecFluxFrame data (sizeof(SpecFluxFrame)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SpecFluxFrame (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SpecFluxFrame structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A logical flag indicating if the SpecFluxFrame's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SpecFluxFrame.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the Object astClass function).
*     frame1
*        Pointer to the SpecFrame
*     frame2
*        Pointer to the FluxFrame

*  Returned Value:
*     A pointer to the new SpecFluxFrame.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstSpecFluxFrame *new;        /* Pointer to new SpecFluxFrame */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSpecFluxFrameVtab( vtab, name );

/* Initialise a Frame structure (the parent class) as the first
   component within the SpecFluxFrame structure, allocating memory if
   necessary. Set the number of Frame axes to zero, since all axis
   information is stored within the component Frames. */
   new = astInitCmpFrame( mem, size, 0, (AstCmpFrameVtab *) vtab, name,
                          frame1, frame2 );
   if ( astOK ) {


/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstSpecFluxFrame *astLoadSpecFluxFrame_( void *mem, size_t size,
                                 AstSpecFluxFrameVtab *vtab, const char *name,
                                 AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadSpecFluxFrame

*  Purpose:
*     Load a SpecFluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "specfluxframe.h"
*     AstSpecFluxFrame *astLoadSpecFluxFrame( void *mem, size_t size,
*                                   AstSpecFluxFrameVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     SpecFluxFrame loader.

*  Description:
*     This function is provided to load a new SpecFluxFrame using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SpecFluxFrame structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the SpecFluxFrame is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SpecFluxFrame data (sizeof(SpecFluxFrame)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SpecFluxFrame (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SpecFluxFrame structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSpecFluxFrame) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SpecFluxFrame. If this is NULL, a pointer
*        to the (static) virtual function table for the SpecFluxFrame class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SpecFluxFrame" is used instead.

*  Returned Value:
*     A pointer to the new SpecFluxFrame.

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
   AstSpecFluxFrame *new;        /* Pointer to the new SpecFluxFrame */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SpecFluxFrame. In this case the
   SpecFluxFrame belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSpecFluxFrame );
      vtab = &class_vtab;
      name = "SpecFluxFrame";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSpecFluxFrameVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SpecFluxFrame. */
   new = astLoadCmpFrame( mem, size, (AstCmpFrameVtab *) vtab, name,
                          channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SpecFluxFrame" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */
/*  (none)  */

/* If an error occurred, clean up by deleting the new SpecFluxFrame. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new SpecFluxFrame pointer. */
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





