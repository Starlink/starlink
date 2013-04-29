/*
*+
*  Name:
*     frameset.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the FrameSet class.

*  Invocation:
*     #include "frameset.h"

*  Description:
*     This include file defines the interface to the FrameSet class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     A FrameSet consists of a set of one or more Frames, which are
*     inter-related by Mappings in such a way that it is possible to
*     obtain a Mapping between any pair of the Frames. The Frames are
*     identified by an integer index, with Frames being numbered
*     consecutively from one as they are added to the FrameSet.
*
*     At any time, there is a "base" Frame and a "current" Frame
*     (which are allowed to be the same). Any of the Frames may be
*     nominated to hold these positions, and the choice is determined
*     by the values of the FrameSet's Base and Current attributes
*     which hold the indices of the relevant Frames.  By default, the
*     first Frame added to a FrameSet is its base Frame, and the last
*     one added is its current Frame.
*
*     The base Frame describes the "native" coordinate system of
*     whatever the FrameSet is used to calibrate (e.g. the pixel
*     coordinates of an image) and the current Frame describes the
*     "apparent" coordinate system in which it should be viewed
*     (e.g. displayed, etc.). The other Frames represent alternative
*     coordinate systems which may be selected by making them current.
*
*     When Frame methods are invoked on a FrameSet (e.g. to obtain a
*     Title value or to determine the number of axes), they are
*     applied to the current Frame. Thus, a FrameSet may be used in
*     place of its current Frame in most situations.
*
*     When Mapping methods are invoked on a FrameSet, the Mapping used
*     is the one between its base Frame and its current Frame. Thus, a
*     FrameSet may be used to convert "native" coordinates into
*     "apparent" ones, and vice versa. A FrameSet may also be
*     inverted, which has the effect of interchanging its base and
*     current Frames (and hence of reversing the Mapping between
*     them).
*
*     The FrameSet class also defines methods of its own, which are
*     used to manage the Frames and Mappings that it contains and to
*     convert between coordinate systems described by different
*     FrameSets.

*  Inheritance:
*     The FrameSet class inherits from the Frame class.

*  Attributes Over-Ridden:
*     Digits (integer)
*     Direction(axis) (integer)
*     Domain (string)
*     Format(axis) (string)
*     Label(axis) (string)
*     MatchEnd (integer)
*     MaxAxes (integer)
*     MinAxes (integer)
*     Naxes (integer)
*     Permute (integer)
*     PreserveAxes (integer)
*     Symbol(axis) (string)
*     Title (string)
*     Unit(axis) (string)
*        The FrameSet acquires all of these attributes from its
*        current Frame, so their meanings, values and defaults are
*        determined by this Frame and may change if a different
*        current Frame is selected.
*     Nin (integer)
*     Nout (integer)
*     TranForward (integer)
*     TranInverse (integer)
*        The FrameSet interprets all of these as applying to the
*        Mapping that converts coordinates between its base Frame and
*        its current Frame, so their values may change if a different
*        base or current Frame is selected.
*     Invert (integer)
*     Report (integer)
*        The FrameSet interprets these as applying to the Mapping that
*        converts coordinates between its base Frame and its current
*        Frame, but their values are not affected by selecing a
*        different base or current Frame.

*  New Attributes Defined:
*     Base (integer)
*        The (one-based) index of the Frame which is to be regarded as
*        the base Frame in the FrameSet. By default, this is the first
*        Frame added to the FrameSet (i.e. when it was created),
*        unless the Frameset has been inverted, in which case it is
*        the last Frame added. Inverting a FrameSet interchanges the
*        values of its Base and Current attributes.
*     Current (integer)
*        The (one-based) index of the Frame which is to be regarded as
*        the current Frame in the FrameSet. By default, this is the
*        last Frame added to the FrameSet, unless the Frameset has
*        been inverted, in which case it is the first Frame added
*        (i.e. when the FrameSet was created). Inverting a FrameSet
*        interchanges the values of its Base and Current attributes.
*     Nframe (integer)
*        A read-only value giving the number of Frames in a
*        FrameSet. This value will change as Frames are added or
*        removed.

*  Methods Over-Ridden:
*     Public:
*        astClear
*           Clear attribute values for a FrameSet.
*        astConvert
*           Determine how to convert between two coordinate systems.
*        astDistance
*           Calculate the distance between two points.
*        astFindFrame
*           Find a coordinate system with specified characteristics
*        astFormat
*           Format a coordinate value for a FrameSet axis.
*        astGetAxis
*           Obtain a pointer to a specified Axis from a FrameSet.
*        astGetNaxes
*           Determine how many axes a FrameSet has.
*        astGetNin
*           Get the number of input coordinates for a FrameSet.
*        astGetNout
*           Get the number of output coordinates for a FrameSet.
*        astNorm
*           Normalise a set of FrameSet coordinates.
*        astOffset
*           Calculate an offset along a geodesic curve.
*        astPermAxes
*           Permute the order of a FrameSet's axes.
*        astPickAxes
*           Create a new Frame by picking axes from a FrameSet.
*        astSetAxis
*           Set a new Axis for a FrameSet.
*        astSimplify
*           Simplify the Mappings in a FrameSet.
*        astTransform
*           Transform a set of points.
*        astUnformat
*           Read a formatted coordinate value for a FrameSet axis.
*
*     Protected:
*        astAbbrev
*           Abbreviate a formatted FrameSet axis value by skipping leading
*           fields.
*        astClearDigits
*           Clear the value of the Digits attribute for a FrameSet.
*        astClearDirection
*           Clear the value of the Direction attribute for a FrameSet axis.
*        astClearDomain
*           Clear the value of the Domain attribute for a FrameSet.
*        astClearFormat
*           Clear the value of the Format attribute for a FrameSet axis.
*        astClearLabel
*           Clear the value of the Label attribute for a FrameSet axis.
*        astClearMatchEnd
*           Clear the value of the MatchEnd attribute for a FrameSet.
*        astClearMaxAxes
*           Clear the value of the MaxAxes attribute for a FrameSet.
*        astClearMinAxes
*           Clear the value of the MinAxes attribute for a FrameSet.
*        astClearPermute
*           Clear the value of the Permute attribute for a FrameSet.
*        astClearPreserveAxes
*           Clear the value of the PreserveAxes attribute for a FrameSet.
*        astClearSymbol
*           Clear the value of the Symbol attribute for a FrameSet axis.
*        astClearTitle
*           Clear the value of the Title attribute for a FrameSet.
*        astClearUnit
*           Clear the value of the Unit attribute for a FrameSet axis.
*        astConvertX
*           Determine how to convert between two coordinate systems.
*        astGap
*           Find a "nice" gap for tabulating FrameSet axis values.
*        astGetDigits
*           Get the value of the Digits attribute for a FrameSet.
*        astGetDirection
*           Get the value of the Direction attribute for a FrameSet axis.
*        astGetDomain
*           Get the value of the Domain attribute for a FrameSet.
*        astGetFormat
*           Get the value of the Format attribute for a FrameSet axis.
*        astGetLabel
*           Get the value of the Label attribute for a FrameSet axis.
*        astGetMatchEnd
*           Get the value of the MatchEnd attribute for a FrameSet.
*        astGetMaxAxes
*           Get the value of the MaxAxes attribute for a FrameSet.
*        astGetMinAxes
*           Get the value of the MinAxes attribute for a FrameSet.
*        astGetPerm
*           Access the axis permutation array for the current Frame of
*           a FrameSet.
*        astGetPermute
*           Get the value of the Permute attribute for a FrameSet.
*        astGetPreserveAxes
*           Get the value of the PreserveAxes attribute for a FrameSet.
*        astGetSymbol
*           Get the value of the Symbol attribute for a FrameSet axis.
*        astGetTitle
*           Get the value of the Title attribute for a FrameSet.
*        astGetTranForward
*           Determine if a Mapping can perform a "forward" coordinate
*           transformation.
*        astGetTranInverse
*           Determine if a Mapping can perform an "inverse" coordinate
*           transformation.
*        astGetUnit
*           Get the value of the Unit attribute for a FrameSet axis.
*        astMatch
*           Determine if conversion is possible between two coordinate systems.
*        astOverlay
*           Overlay the attributes of a template FrameSet on to another Frame.
*        astPrimaryFrame
*           Uniquely identify a primary Frame and one of its axes.
*        astReportPoints
*           Report the effect of transforming a set of points using a FrameSet.
*        astSetAttrib
*           Set an attribute value for a FrameSet.
*        astSetDigits
*           Set the value of the Digits attribute for a FrameSet.
*        astSetDirection
*           Set the value of the Direction attribute for a FrameSet axis.
*        astSetDomain
*           Set the value of the Domain attribute for a FrameSet.
*        astSetFormat
*           Set the value of the Format attribute for a FrameSet axis.
*        astSetLabel
*           Set the value of the Label attribute for a FrameSet axis.
*        astSetMatchEnd
*           Set the value of the MatchEnd attribute for a FrameSet.
*        astSetMaxAxes
*           Set the value of the MaxAxes attribute for a FrameSet.
*        astSetMinAxes
*           Set the value of the MinAxes attribute for a FrameSet.
*        astSetPermute
*           Set the value of the Permute attribute for a FrameSet.
*        astSetPreserveAxes
*           Set the value of the PreserveAxes attribute for a FrameSet.
*        astSetSymbol
*           Set the value of the Symbol attribute for a FrameSet axis.
*        astSetTitle
*           Set the value of the Title attribute for a FrameSet.
*        astSetUnit
*           Set the value of the Unit attribute for a FrameSet axis.
*        astSubFrame
*           Select axes from a FrameSet and convert to the new coordinate
*           system.
*        astTestDigits
*           Test if a value has been set for the Digits attribute of a
*           FrameSet.
*        astTestDirection
*           Test if a value has been set for the Direction attribute of a
*           FrameSet axis.
*        astTestDomain
*           Test if a value has been set for the Domain attribute of a
*           FrameSet.
*        astTestFormat
*           Test if a value has been set for the Format attribute of a
*           FrameSet axis.
*        astTestLabel
*           Test if a value has been set for the Label attribute of a
*           FrameSet axis.
*        astTestMatchEnd
*           Test if a value has been set for the MatchEnd attribute of a
*           FrameSet.
*        astTestMaxAxes
*           Test if a value has been set for the MaxAxes attribute of a
*           FrameSet.
*        astTestMinAxes
*           Test if a value has been set for the MinAxes attribute of a
*           FrameSet.
*        astTestPermute
*           Test if a value has been set for the Permute attribute of a
*           FrameSet.
*        astTestPreserveAxes
*           Test if a value has been set for the PreserveAxes attribute of a
*           FrameSet.
*        astTestSymbol
*           Test if a value has been set for the Symbol attribute of a
*           FrameSet axis.
*        astTestTitle
*           Test if a value has been set for the Title attribute of a FrameSet.
*        astTestUnit
*           Test if a value has been set for the Unit attribute of a FrameSet
*           axis.
*        astValidateAxis
*           Validate and permute a FrameSet's axis index.
*        astVSet
*           Set values for a FrameSet's attributes.

*  New Methods Defined:
*     Public:
*        astAddFrame
*           Add a Frame to a FrameSet to define a new coordinate system.
*        astGetFrame
*           Obtain a pointer to a specified Frame in a FrameSet.
*        astGetMapping
*           Obtain a Mapping between two Frames in a FrameSet.
*        astRemapFrame
*           Modify a Frame's relationshp to the other Frames in a FrameSet.
*        astRemoveFrame
*           Remove a Frame from a FrameSet.
*
*     Protected:
*        astClearBase
*           Clear the value of the Base attribute for a FrameSet.
*        astClearCurrent
*           Clear the value of the Current attribute for a FrameSet.
*        astGetBase
*           Obtain the value of the Base attribute for a FrameSet.
*        astGetCurrent
*           Obtain the value of the Current attribute for a FrameSet.
*        astGetNframe
*           Determine the number of Frames in a FrameSet.
*        astSetBase
*           Set the value of the Base attribute for a FrameSet.
*        astSetCurrent
*           Set the value of the Current attribute for a FrameSet.
*        astTestBase
*           Test if a value has been set for the Base attribute of a FrameSet.
*        astTestCurrent
*           Test if a value has been set for the Current attribute of a
*           FrameSet.
*        astValidateFrameIndex
*           Validate a FrameSet Frame index number.

*  Other Class Functions:
*     Public:
*        astFrameSet
*           Create a FrameSet.
*        astIsAFrameSet
*           Test class membership.
*
*     Protected:
*        astCheckFrameSet
*           Validate class membership.
*        astInitFrameSet
*           Initialise a FrameSet.
*        astInitFrameSetVtab
*           Initialise the virtual function table for the FrameSet class.
*        astLoadFrameSet
*           Load a FrameSet.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstFrameSet
*           FrameSet object type.

*     Protected:
*        AstFrameSetVtab
*           FrameSet virtual function table type.

*  Macros:
*     Public:
*        AST__BASE
*           Expands to a constant int that may be used as a Frame index to
*           refer to a FrameSet's base Frame.
*        AST__CURRENT
*           Expands to a constant int that may be used as a Frame index to
*           refer to a FrameSet's current Frame.
*        AST__NOFRAME
*           Expands to a constant int that is guaranteed not to be valid when
*           used as a Frame index for a FrameSet.
*
*     Protected:
*        None.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

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
*     16-FEB-1996 (RFWS):
*        Original version.
*     5-JUN-1996 (RFWS):
*        Tidied up, etc.
*     12-AUG-1996 (RFWS):
*        Added support for the public interface.
*     25-SEP-1996 (RFWS):
*        Added I/O facilities.
*     20-JAN-1998 (RFWS):
*        Implemented preservation of FrameSet integrity when attribute
*        values associated with the current Frame are modified.
*     25-FEB-1998 (RFWS):
*        Over-ride the astUnformat method.
*     8-JAN-2003 (DSB):
*        Added protected astInitFrameSetVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "frame.h"               /* Parent Frame class */

/* Note that the usual setting of the FRAMESET_INCLUDED flag, which
   prevents this file being included more than once, must be deferred
   until after including the "frame.h" file. This is because "frame.h"
   needs to include the present interface definition (as a form of
   "forward reference") in order to have access to FrameSets
   itself. */
#if !defined( FRAMESET_INCLUDED )
#define FRAMESET_INCLUDED

/* Macros. */
/* ======= */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST__BASE (0)            /* Identify base Frame */
#define AST__CURRENT (-1)        /* Identify current Frame */
#define AST__NOFRAME (-99)       /* An invalid Frame index */
#define AST__ALLFRAMES (-199)    /* A value representing all Frames */
#define AST__FRAMESET_GETALLVARIANTS_BUFF_LEN 200 /* Length for AllVariants buffer */
#define AST__FRAMESET_GETATTRIB_BUFF_LEN 200 /* Length for GetAtribb buffer */

/* Type Definitions. */
/* ================= */
/* FrameSet structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstFrameSet {

/* Attributes inherited from the parent class. */
   AstFrame parent;              /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstFrame **frame;             /* Array of Frame pointers */
   AstMapping **map;             /* Array of Mapping pointers */
   int *varfrm;                  /* Array of variants Frames indicies */
   int *invert;                  /* Array of Mapping Invert values */
   int *link;                    /* Parent node index for each node */
   int *node;                    /* Index of node associated with Frame */
   int base;                     /* Index of base Frame */
   int current;                  /* Index of current Frame */
   int nframe;                   /* Number of Frames */
   int nnode;                    /* Number of nodes */
} AstFrameSet;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstFrameSetVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstFrame *(* GetFrame)( AstFrameSet *, int, int * );
   AstMapping *(* GetMapping)( AstFrameSet *, int, int, int * );
   int (* GetBase)( AstFrameSet *, int * );
   int (* GetCurrent)( AstFrameSet *, int * );
   int (* GetNframe)( AstFrameSet *, int * );
   int (* TestBase)( AstFrameSet *, int * );
   int (* TestCurrent)( AstFrameSet *, int * );
   int (* ValidateFrameIndex)( AstFrameSet *, int, const char *, int * );
   void (* AddFrame)( AstFrameSet *, int, AstMapping *, AstFrame *, int * );
   void (* AddVariant)( AstFrameSet *, AstMapping *, const char *, int * );
   void (* MirrorVariants)( AstFrameSet *, int, int * );
   void (* ClearBase)( AstFrameSet *, int * );
   void (* ClearCurrent)( AstFrameSet *, int * );
   void (* RemapFrame)( AstFrameSet *, int, AstMapping *, int * );
   void (* RemoveFrame)( AstFrameSet *, int, int * );
   void (* SetBase)( AstFrameSet *, int, int * );
   void (* SetCurrent)( AstFrameSet *, int, int * );
   void (* ClearVariant)( AstFrameSet *, int * );
   const char *(* GetVariant)( AstFrameSet *, int * );
   void (* SetVariant)( AstFrameSet *, const char *, int * );
   int (* TestVariant)( AstFrameSet *, int * );
   const char *(* GetAllVariants)( AstFrameSet *, int * );
} AstFrameSetVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstFrameSetGlobals {
   AstFrameSetVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__FRAMESET_GETATTRIB_BUFF_LEN + 1 ];
   char GetAllVariants_Buff[ AST__FRAMESET_GETALLVARIANTS_BUFF_LEN + 1 ];
   AstFrame *Integrity_Frame;
   const char *Integrity_Method;
   int Integrity_Lost;
} AstFrameSetGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(FrameSet)         /* Check class membership */
astPROTO_ISA(FrameSet)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstFrameSet *astFrameSet_( void *, const char *, int *, ...);
#else
AstFrameSet *astFrameSetId_( void *, const char *, ... )__attribute__((format(printf,2,3)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFrameSet *astInitFrameSet_( void *, size_t, int, AstFrameSetVtab *,
                               const char *, AstFrame *, int * );

/* Vtab initialiser. */
void astInitFrameSetVtab_( AstFrameSetVtab *, const char *, int * );

/* Loader. */
AstFrameSet *astLoadFrameSet_( void *, size_t, AstFrameSetVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitFrameSetGlobals_( AstFrameSetGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstFrame *astGetFrame_( AstFrameSet *, int, int * );
AstMapping *astGetMapping_( AstFrameSet *, int, int, int * );
void astAddFrame_( AstFrameSet *, int , AstMapping *, AstFrame *, int * );
void astAddVariant_( AstFrameSet *, AstMapping *, const char *, int * );
void astMirrorVariants_( AstFrameSet *, int, int * );
void astRemapFrame_( AstFrameSet *, int, AstMapping *, int * );
void astRemoveFrame_( AstFrameSet *, int, int * );

#if defined(astCLASS)            /* Protected */
const char *astGetAllVariants_( AstFrameSet *, int * );
int astGetBase_( AstFrameSet *, int * );
int astGetCurrent_( AstFrameSet *, int * );
int astGetNframe_( AstFrameSet *, int * );
int astTestBase_( AstFrameSet *, int * );
int astTestCurrent_( AstFrameSet *, int * );
int astValidateFrameIndex_( AstFrameSet *, int, const char *, int * );
void astClearBase_( AstFrameSet *, int * );
void astClearCurrent_( AstFrameSet *, int * );
void astSetBase_( AstFrameSet *, int, int * );
void astSetCurrent_( AstFrameSet *, int, int * );
void astClearVariant_( AstFrameSet *, int * );
const char *astGetVariant_( AstFrameSet *, int * );
void astSetVariant_( AstFrameSet *, const char *, int * );
int astTestVariant_( AstFrameSet *, int * );
#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class to make
   them easier to invoke (e.g. to avoid type mis-matches when passing pointers
   to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them to
   validate their own arguments. We must use a cast when passing object
   pointers (so that they can accept objects from derived classes). */

/* Check class membership. */
#define astCheckFrameSet(this) astINVOKE_CHECK(FrameSet,this,0)
#define astVerifyFrameSet(this) astINVOKE_CHECK(FrameSet,this,1)

/* Test class membership. */
#define astIsAFrameSet(this) astINVOKE_ISA(FrameSet,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astFrameSet astINVOKE(F,astFrameSet_)
#else
#define astFrameSet astINVOKE(F,astFrameSetId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitFrameSet(mem,size,init,vtab,name,frame) \
astINVOKE(O,astInitFrameSet_(mem,size,init,vtab,name,astCheckFrame(frame),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitFrameSetVtab(vtab,name) astINVOKE(V,astInitFrameSetVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadFrameSet(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFrameSet_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckFrameSet to validate FrameSet pointers before
   use. This provides a contextual error report if a pointer to the wrong sort
   of object is supplied. */
#define astAddFrame(this,iframe,map,frame) \
astINVOKE(V,astAddFrame_(astCheckFrameSet(this),iframe,(((iframe)!=AST__ALLFRAMES)?astCheckMapping(map):NULL),astCheckFrame(frame),STATUS_PTR))
#define astAddVariant(this,map,name) \
astINVOKE(V,astAddVariant_(astCheckFrameSet(this),map?astCheckMapping(map):NULL,name,STATUS_PTR))
#define astMirrorVariants(this,iframe) \
astINVOKE(V,astMirrorVariants_(astCheckFrameSet(this),iframe,STATUS_PTR))
#define astGetFrame(this,iframe) \
astINVOKE(O,astGetFrame_(astCheckFrameSet(this),iframe,STATUS_PTR))
#define astGetMapping(this,iframe1,iframe2) \
astINVOKE(O,astGetMapping_(astCheckFrameSet(this),iframe1,iframe2,STATUS_PTR))
#define astRemapFrame(this,iframe,map) \
astINVOKE(V,astRemapFrame_(astCheckFrameSet(this),iframe,astCheckMapping(map),STATUS_PTR))
#define astRemoveFrame(this,iframe) \
astINVOKE(V,astRemoveFrame_(astCheckFrameSet(this),iframe,STATUS_PTR))

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
#if defined(astCLASS)            /* Protected */
#define astClearBase(this) \
astINVOKE(V,astClearBase_(astCheckFrameSet(this),STATUS_PTR))
#define astClearCurrent(this) \
astINVOKE(V,astClearCurrent_(astCheckFrameSet(this),STATUS_PTR))
#define astGetBase(this) \
astINVOKE(V,astGetBase_(astCheckFrameSet(this),STATUS_PTR))
#define astGetCurrent(this) \
astINVOKE(V,astGetCurrent_(astCheckFrameSet(this),STATUS_PTR))
#define astGetNframe(this) \
astINVOKE(V,astGetNframe_(astCheckFrameSet(this),STATUS_PTR))
#define astSetBase(this,ibase) \
astINVOKE(V,astSetBase_(astCheckFrameSet(this),ibase,STATUS_PTR))
#define astSetCurrent(this,icurrent) \
astINVOKE(V,astSetCurrent_(astCheckFrameSet(this),icurrent,STATUS_PTR))
#define astTestBase(this) \
astINVOKE(V,astTestBase_(astCheckFrameSet(this),STATUS_PTR))
#define astTestCurrent(this) \
astINVOKE(V,astTestCurrent_(astCheckFrameSet(this),STATUS_PTR))
#define astValidateFrameIndex(this,iframe,method) \
astINVOKE(V,astValidateFrameIndex_(astCheckFrameSet(this),iframe,method,STATUS_PTR))
#define astClearVariant(this) \
astINVOKE(V,astClearVariant_(astCheckFrameSet(this),STATUS_PTR))
#define astGetVariant(this) \
astINVOKE(V,astGetVariant_(astCheckFrameSet(this),STATUS_PTR))
#define astSetVariant(this,variant) \
astINVOKE(V,astSetVariant_(astCheckFrameSet(this),variant,STATUS_PTR))
#define astTestVariant(this) \
astINVOKE(V,astTestVariant_(astCheckFrameSet(this),STATUS_PTR))
#define astGetAllVariants(this) \
astINVOKE(V,astGetAllVariants_(astCheckFrameSet(this),STATUS_PTR))
#endif
#endif





