#if !defined( CMPFRAME_INCLUDED ) /* Include this file only once */
#define CMPFRAME_INCLUDED
/*
*+
*  Name:
*     cmpframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the CmpFrame class.

*  Invocation:
*     #include "cmpframe.h"

*  Description:
*     This include file defines the interface to the CmpFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.
*
*     A CmpFrame is a compound Frame which allows two component Frames
*     (of any class) to be merged together to form a more complex
*     Frame. The axes of the two component Frames then appear together
*     in the resulting CmpFrame (those of the first Frame, followed by
*     those of the second Frame).
*
*     Since a CmpFrame is itself a Frame, it can be used as a
*     component in forming further CmpFrames. Frames of arbitrary
*     complexity may be built from simple individual Frames in this
*     way.

*  Inheritance:
*     The CmpFrame class inherits from the Frame class.

*  Attributes Over-Ridden:
*     Domain (string)
*        A string which may be used to identify a CmpFrame and used as
*        an additional key when matching a target CmpFrame with a
*        template.  The CmpFrame class re-defines the default value to
*        "CMP".
*     MaxAxes (integer)
*        Specifies the maximum number of axes in a target Frame that
*        can be matched when using the CmpFrame as a template. The
*        CmpFrame class sets this to be the sum of the MaxAxes
*        attribute values for the two component Frames contained by
*        the CmpFrame. Any attempt to alter this value (other than
*        through the component Frames) is simply ignored.
*     MinAxes (integer)
*        Specifies the minimum number of axes in a target Frame that
*        can be matched when using the CmpFrame as a template. The
*        CmpFrame class sets this to be the sum of the MinAxes
*        attribute values for the two component Frames contained by
*        the CmpFrame. Any attempt to alter this value (other than
*        through the component Frames) is simply ignored.
*     Naxes (integer)
*        A read-only attribute that gives the number of axes in a
*        CmpFrame (i.e. the number of dimensions of the space which
*        the CmpFrame describes). This value is determined when the
*        CmpFrame is created and is equal to the sum of the Naxes
*        attributes of the two component Frames.
*     Title (string)
*        Specifies a string to be used as a title on (e.g.) graphs to
*        describe the coordinate system which the CmpFrame
*        represents. The CmpFrame class re-defines the default value
*        to "<n>-D Compound Coordinate System", where <n> is the
*        number of CmpFrame axes.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        astDistance
*           Calculate the distance between two points.
*        astFormat
*           Format a coordinate value for a CmpFrame axis.
*        astNorm
*           Normalise a set of CmpFrame coordinates.
*        astOffset
*           Calculate an offset along a geodesic curve.
*        astPermAxes
*           Permute the order of a CmpFrame's axes.
*        astUnformat
*           Read a formatted coordinate value for a CmpFrame axis.
*
*     Protected:
*        astAbbrev
*           Abbreviate a formatted CmpFrame axis value by skipping leading
*           fields.
*        astClearDirection
*           Clear the value of the Direction attribute for a CmpFrame axis.
*        astClearFormat
*           Clear the value of the Format attribute for a CmpFrame axis.
*        astClearLabel
*           Clear the value of the Label attribute for a CmpFrame axis.
*        astClearMaxAxes
*           Clear the value of the MaxAxes attribute for a CmpFrame.
*        astClearMinAxes
*           Clear the value of the MinAxes attribute for a CmpFrame.
*        astClearSymbol
*           Clear the value of the Symbol attribute for a CmpFrame axis.
*        astClearUnit
*           Clear the value of the Unit attribute for a CmpFrame axis.
*        astGap
*           Find a "nice" gap for tabulating CmpFrame axis values.
*        astGetAxis
*           Obtain a pointer to a specified Axis from a CmpFrame.
*        astGetDirection
*           Obtain the value of the Direction attribute for a CmpFrame axis.
*        astGetDomain
*           Obtain a pointer to the Domain attribute string for a CmpFrame.
*        astGetFormat
*           Obtain the value of the Format attribute for a CmpFrame axis.
*        astGetLabel
*           Obtain the value of the Label attribute for a CmpFrame axis.
*        astGetMaxAxes
*           Obtain the value of the MaxAxes attribute for a CmpFrame.
*        astGetMinAxes
*           Obtain the value of the MinAxes attribute for a CmpFrame.
*        astGetNaxes
*           Obtain the value of the Naxes attribute for a CmpFrame.
*        astGetPerm
*           Access the axis permutation array for a CmpFrame.
*        astGetSymbol
*           Obtain the value of the Symbol attribute for a CmpFrame axis.
*        astGetTitle
*           Obtain a pointer to the Title attribute string for a CmpFrame.
*        astGetUnit
*           Obtain the value of the Unit attribute for a CmpFrame axis.
*        astMatch
*           Determine if conversion is possible between two coordinate
*           systems.
*        astPrimaryFrame
*           Uniquely identify a primary Frame and one of its axes.
*        astSetAxis
*           Set a new Axis for a CmpFrame.
*        astSetDirection
*           Set the value of the Direction attribute for a CmpFrame axis.
*        astSetFormat
*           Set the value of the Format attribute for a CmpFrame axis.
*        astSetLabel
*           Set the value of the Label attribute for a CmpFrame axis.
*        astSetMaxAxes
*           Set a value for the MaxAxes attribute of a CmpFrame.
*        astSetMinAxes
*           Set a value for the MinAxes attribute of a CmpFrame.
*        astSetSymbol
*           Set the value of the Symbol attribute for a CmpFrame axis.
*        astSetUnit
*           Set the value of the Unit attribute for a CmpFrame axis.
*        astSubFrame
*           Select axes from a CmpFrame and convert to the new coordinate
*           system.
*        astTestDirection
*           Test if a Direction attribute value has been set for a CmpFrame
*           axis.
*        astTestFormat
*           Test if a Format attribute value has been set for a CmpFrame axis.
*        astTestLabel
*           Test if a Label attribute value has been set for a CmpFrame axis.
*        astTestMaxAxes
*           Test if a value has been set for the MaxAxes attribute of a
*           CmpFrame.
*        astTestMinAxes
*           Test if a value has been set for the MinAxes attribute of a
*           CmpFrame.
*        astTestSymbol
*           Test if a Symbol attribute value has been set for a CmpFrame axis.
*        astTestUnit
*           Test if a Unit attribute value has been set for a CmpFrame axis.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsACmpFrame
*           Test class membership.
*        astCmpFrame
*           Create a CmpFrame.
*
*     Protected:
*        astCheckCmpFrame
*           Validate class membership.
*        astInitCmpFrame
*           Initialise a CmpFrame.
*        astInitCmpFrameVtab
*           Initialise the virtual function table for the CmpFrame class.
*        astLoadCmpFrame
*           Load a CmpFrame.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstCmpFrame
*           CmpFrame object type.
*
*     Protected:
*        AstCmpFrameVtab
*           CmpFrame virtual function table type.

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
*     6-MAR-1996 (RFWS):
*        Original version.
*     27-FEB-1997 (RFWS):
*        Improved the prologue.
*     25-FEB-1998 (RFWS):
*        Over-ride the astUnformat method.
*     8-JAN-2003 (DSB):
*        Added protected astInitCmpFrameVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Parent Frame class */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#if defined(astCLASS)            /* Protected */

/* The legal System values recognized by this class of Frame. */
#define AST__COMP 0

#endif

/* Type Definitions. */
/* ================= */
/* CmpFrame structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstCmpFrame {

/* Attributes inherited from the parent class. */
   AstFrame frame;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstFrame *frame1;             /* First component frame */
   AstFrame *frame2;             /* Second component Frame */
   int *perm;                    /* Pointer to axis permutation array */
} AstCmpFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstCmpFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */

} AstCmpFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstCmpFrameGlobals {
   AstCmpFrameVtab Class_Vtab;
   int Class_Init;
   int *qsort_axes;
   char Label_Buff[ 101 ];
   char Symbol_Buff[ 51 ];
   char GetDomain_Buff[ 101 ];
   char GetTitle_Buff[ 101 ];
} AstCmpFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(CmpFrame)         /* Check class membership */
astPROTO_ISA(CmpFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstCmpFrame *astCmpFrame_( void *, void *, const char *, int *, ...);
#else
AstCmpFrame *astCmpFrameId_( void *, void *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstCmpFrame *astInitCmpFrame_( void *, size_t, int, AstCmpFrameVtab *,
                               const char *, AstFrame *, AstFrame *, int * );

/* Vtab initialiser. */
void astInitCmpFrameVtab_( AstCmpFrameVtab *, const char *, int * );

/* Loader. */
AstCmpFrame *astLoadCmpFrame_( void *, size_t, AstCmpFrameVtab *,
                               const char *, AstChannel *, int * );
/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitCmpFrameGlobals_( AstCmpFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them
   to validate their own arguments. We must use a cast when passing
   object pointers (so that they can accept objects from derived
   classes). */

/* Check class membership. */
#define astCheckCmpFrame(this) astINVOKE_CHECK(CmpFrame,this,0)
#define astVerifyCmpFrame(this) astINVOKE_CHECK(CmpFrame,this,1)

/* Test class membership. */
#define astIsACmpFrame(this) astINVOKE_ISA(CmpFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astCmpFrame astINVOKE(F,astCmpFrame_)
#else
#define astCmpFrame astINVOKE(F,astCmpFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitCmpFrame(mem,size,init,vtab,name,frame1,frame2) \
astINVOKE(O,astInitCmpFrame_(mem,size,init,vtab,name,astCheckFrame(frame1),astCheckFrame(frame2),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitCmpFrameVtab(vtab,name) astINVOKE(V,astInitCmpFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadCmpFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadCmpFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckCmpFrame to validate CmpFrame pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#endif





