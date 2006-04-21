#if !defined( AXIS_INCLUDED )    /* Include this file only once */
#define AXIS_INCLUDED
/*
*+
*  Name:
*     axis.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Axis class.

*  Invocation:
*     #include "axis.h"

*  Description:
*     This include file defines the interface to the Axis class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Axis class implements the basic behaviour of a coordinate
*     axis, several of which may be assembled to represent a
*     coordinate system.

*  Inheritance:
*     The Axis class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Bottom (double)
*        Lowest legal value for axis.
*     Digits (integer)
*        Specifies how many digits of precision are required by
*        default when a coordinate value for an Axis is formatted
*        (e.g. using the astAxisFormat method). The Digits value acts
*        as a default only and is over-ridden if a Format string is
*        specified explicitly (using the astSetAxisFormat method). The
*        default supplied by the Axis class for the Digits attribute
*        itself is 7.
*     Direction (integer)
*        Specifies how coordinate values for an Axis should be
*        displayed. By default, it has the value one, indicating that
*        they should be shown in the conventional sense
*        (i.e. increasing left to right for an abscissa and bottom to
*        top for an ordinate). If set to zero, this attribute
*        indicates that the direction should be reversed (as would
*        often be done for an astronomical magnitude or a right
*        ascension axis, for example).
*     Format (string)
*        Specifies the format to be used to display coordinate values
*        for an Axis (i.e. to convert them from binary to character
*        form). The interpretation of this string (e.g. by derived
*        classes) is left to the astAxisFormat method, but the Axis
*        class interprets this parameter as a C "printf" format string
*        which should be capable of formatting a single coordinate
*        value stored as a double (e.g.  "%1.7G"). If no Format string
*        is set, the default supplied by the Axis class is based on
*        the value of the Digits attribute.
*     Label (string)
*        Specifies the label to be attached to an Axis when it is
*        represented in (e.g.) a graph. It is intended purely for
*        interpretation by human readers and not by software. The
*        default supplied by the Axis class is the string "Coordinate
*        Axis".
*     Symbol (string)
*        Specifies the symbol to be used to represent coordinate
*        values for an Axis in "short form", such as in algebraic
*        expressions where a full description of the Axis would be
*        inappropriate. Examples include "RA" and "Dec" (for Right
*        Ascension and Declination). The default supplied by the Axis
*        class is the string "x".
*     Top (double)
*        Highest legal value for axis.
*     Unit (string)
*        Describes the units used to represent coordinate values on an
*        Axis.  The default supplied by the Axis class is an empty
*        string "".

*  Methods Over-Ridden:
*     Public:
*        None.

*     Protected:
*        astSetAttrib
*           Set an attribute value for an Axis.

*  New Methods Defined:
*     Public:
*        astAxisFormat
*           Format a coordinate value for an Axis.
*        astAxisNorm
*           Normalise an Axis coordinate value.
*        astAxisUnformat
*           Read a formatted coordinate value for an Axis.

*     Protected:
*        astAxisAbbrev
*           Abbreviate a formatted Axis value by skipping leading fields.
*        astAxisDistance
*           Find the distance between two axis values.
*        astAxisFields
*           Identify the fields within a formatted SkyAxis value.
*        astAxisGap
*           Find a "nice" gap for tabulating Axis values.
*        astAxisOffset
*           Add an increment onto a supplied axis value.
*        astAxisOverlay
*           Overlay the attributes of a template Axis on to another Axis.
*        astClearAxisDigits
*           Clear the Digits attribute for an Axis.
*        astClearAxisDirection
*           Clear the Direction attribute for an Axis.
*        astClearAxisFormat
*           Clear the Format attribute for an Axis.
*        astClearAxisLabel
*           Clear the Label attribute for an Axis.
*        astClearAxisSymbol
*           Clear the Symbol attribute for an Axis.
*        astClearAxisUnit
*           Clear the Unit attribute for an Axis.
*        astGetAxisDigits
*           Get the value of the Digits attribute for an Axis.
*        astGetAxisDirection
*           Get the value of the Direction attribute for an Axis.
*        astGetAxisFormat
*           Get a pointer to the Format attribute for an Axis.
*        astGetAxisLabel
*           Get a pointer to the Label attribute for an Axis.
*        astGetAxisSymbol
*           Get a pointer to the Symbol attribute for an Axis.
*        astGetAxisUnit
*           Get a pointer to the Unit attribute for an Axis.
*        astSetAxisDigits
*           Set the value of the Digits attribute for an Axis.
*        astSetAxisDirection
*           Set the value of the Direction attribute for an Axis.
*        astSetAxisFormat
*           Set the value of the Format attribute for an Axis.
*        astSetAxisLabel
*           Set the value of the Label attribute for an Axis.
*        astSetAxisSymbol
*           Set the value of the Symbol attribute for an Axis.
*        astSetAxisUnit
*           Set the value of the Unit attribute for an Axis.
*        astTestAxisDigits
*           Test whether a value has been set for the Digits attribute of an
*           Axis.
*        astTestAxisDirection
*           Test whether a value has been set for the Direction attribute of an
*           Axis.
*        astTestAxisFormat
*           Test whether a value has been set for the Format attribute of an
*           Axis.
*        astTestAxisLabel
*           Test whether a value has been set for the Label attribute of an
*           Axis.
*        astTestAxisSymbol
*           Test whether a value has been set for the Symbol attribute of an
*           Axis.
*        astTestAxisUnit
*           Test whether a value has been set for the Unit attribute of an
*           Axis.

*  Other Class Functions:
*     Public:
*        astAxis
*           Create an Axis.
*        astIsAAxis
*           Test class membership.

*     Protected:
*        astCheckAxis
*           Validate class membership.
*        astInitAxis
*           Initialise an Axis.
*        astLoadAxis
*           Load an Axis.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstAxis
*           Axis object type.

*     Protected:
*        AstAxisVtab
*           Axis virtual function table type.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: B.S. Berry (Starlink)

*  History:
*     1-MAR-1996 (RFWS):
*        Original version.
*     25-APR-1996 (RFWS):
*        Made all attribute access functions protected.
*     10-SEP-1996 (RFWS):
*        Added I/O facilities.
*     11-SEP-1996 (RFWS):
*        Added astAxisGap (written by DSB).
*     25-FEB-1998 (RFWS):
*        Added astAxisUnformat.
*     29-AUG-2001 (DSB):
*        Added AxisDistance and AxisOffset.
*     10-OCT-2002 (DSB):
*        Added Top and Bottom.
*     8-JAN-2003 (DSB):
*        Added protected astInitAxisVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#if defined(astCLASS)            /* Protected */
#include "channel.h"
#endif

/* Type Definitions. */
/* ================= */
/* Axis structure. */
/* --------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstAxis {

/* Attributes inherited from the parent class. */
   AstObject object;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   char *label;                  /* Pointer to label string */
   char *format;                 /* Pointer to format string */
   char *symbol;                 /* Pointer to symbol string */
   char *unit;                   /* Pointer to unit string */
   int digits;                   /* Default digits of precision */
   int direction;                /* Plot in conventional direction? */
   double top;                   /* Highest legal axis value */
   double bottom;                /* Lowest legal axis value */
} AstAxis;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstAxisVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstObjectVtab object_vtab;    /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   const char *(* AxisAbbrev)( AstAxis *, const char *, const char *, const char * );
   const char *(* AxisFormat)( AstAxis *, double );
   const char *(* GetAxisFormat)( AstAxis * );
   const char *(* GetAxisLabel)( AstAxis * );
   const char *(* GetAxisSymbol)( AstAxis * );
   const char *(* GetAxisUnit)( AstAxis * );
   double (* AxisGap)( AstAxis *, double, int * );
   double (* AxisDistance)( AstAxis *, double, double );
   double (* AxisOffset)( AstAxis *, double, double );
   int (* AxisIn)( AstAxis *, double, double, double, int );
   int (* AxisFields)( AstAxis *, const char *, const char *, int, char **, int *, double * );
   int (* AxisUnformat)( AstAxis *, const char *, double * );
   int (* GetAxisDigits)( AstAxis * );
   int (* GetAxisDirection)( AstAxis * );
   int (* TestAxisDigits)( AstAxis * );
   int (* TestAxisDirection)( AstAxis * );
   int (* TestAxisFormat)( AstAxis * );
   int (* TestAxisLabel)( AstAxis * );
   int (* TestAxisSymbol)( AstAxis * );
   int (* TestAxisUnit)( AstAxis * );
   void (* AxisNorm)( AstAxis *, double * );
   void (* AxisOverlay)( AstAxis *, AstAxis * );
   void (* ClearAxisDigits)( AstAxis * );
   void (* ClearAxisDirection)( AstAxis * );
   void (* ClearAxisFormat)( AstAxis * );
   void (* ClearAxisLabel)( AstAxis * );
   void (* ClearAxisSymbol)( AstAxis * );
   void (* ClearAxisUnit)( AstAxis * );
   void (* SetAxisDigits)( AstAxis *, int );
   void (* SetAxisDirection)( AstAxis *, int );
   void (* SetAxisFormat)( AstAxis *, const char * );
   void (* SetAxisLabel)( AstAxis *, const char * );
   void (* SetAxisSymbol)( AstAxis *, const char * );
   void (* SetAxisUnit)( AstAxis *, const char * );

   double (* GetAxisTop)( AstAxis * );
   int (* TestAxisTop)( AstAxis * );
   void (* ClearAxisTop)( AstAxis * );
   void (* SetAxisTop)( AstAxis *, double );

   double (* GetAxisBottom)( AstAxis * );
   int (* TestAxisBottom)( AstAxis * );
   void (* ClearAxisBottom)( AstAxis * );
   void (* SetAxisBottom)( AstAxis *, double );

} AstAxisVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Axis)             /* Check class membership */
astPROTO_ISA(Axis)               /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstAxis *astAxis_( const char *, ... );
#else
AstAxis *astAxisId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstAxis *astInitAxis_( void *, size_t, int, AstAxisVtab *, const char * );

/* Vtab initialiser. */
void astInitAxisVtab_( AstAxisVtab *, const char * );

/* Loader. */
AstAxis *astLoadAxis_( void *, size_t, AstAxisVtab *, const char *,
                       AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
const char *astAxisFormat_( AstAxis *, double );
int astAxisUnformat_( AstAxis *, const char *, double * );
void astAxisNorm_( AstAxis *, double * );

#if defined(astCLASS)            /* Protected */
const char *astAxisAbbrev_( AstAxis *, const char *, const char *, const char * );
const char *astGetAxisFormat_( AstAxis * );
const char *astGetAxisLabel_( AstAxis * );
const char *astGetAxisSymbol_( AstAxis * );
const char *astGetAxisUnit_( AstAxis * );
double astAxisGap_( AstAxis *, double, int * );
double astAxisDistance_( AstAxis *, double, double );
double astAxisOffset_( AstAxis *, double, double );
int astGetAxisDigits_( AstAxis * );
int astGetAxisDirection_( AstAxis * );
int astTestAxisDigits_( AstAxis * );
int astTestAxisDirection_( AstAxis * );
int astAxisFields_( AstAxis *, const char *, const char *, int, char **, int *, double * );
int astAxisIn_( AstAxis *, double, double, double, int );
int astTestAxisFormat_( AstAxis * );
int astTestAxisLabel_( AstAxis * );
int astTestAxisSymbol_( AstAxis * );
int astTestAxisUnit_( AstAxis * );
void astAxisOverlay_( AstAxis *, AstAxis * );
void astClearAxisDigits_( AstAxis * );
void astClearAxisDirection_( AstAxis * );
void astClearAxisFormat_( AstAxis * );
void astClearAxisLabel_( AstAxis * );
void astClearAxisSymbol_( AstAxis * );
void astClearAxisUnit_( AstAxis * );
void astSetAxisDigits_( AstAxis *, int );
void astSetAxisDirection_( AstAxis *, int );
void astSetAxisFormat_( AstAxis *, const char * );
void astSetAxisLabel_( AstAxis *, const char * );
void astSetAxisSymbol_( AstAxis *, const char * );
void astSetAxisUnit_( AstAxis *, const char * );

double astGetAxisTop_( AstAxis * );
int astTestAxisTop_( AstAxis * );
void astClearAxisTop_( AstAxis * );
void astSetAxisTop_( AstAxis *, double );

double astGetAxisBottom_( AstAxis * );
int astTestAxisBottom_( AstAxis * );
void astClearAxisBottom_( AstAxis * );
void astSetAxisBottom_( AstAxis *, double );

#endif

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
#define astCheckAxis(this) astINVOKE_CHECK(Axis,this)

/* Test class membership. */
#define astIsAAxis(this) astINVOKE_ISA(Axis,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astAxis astINVOKE(F,astAxis_)
#else
#define astAxis astINVOKE(F,astAxisId_)
#endif

#if defined(astCLASS)            /* Protected. */

/* Initialiser. */
#define astInitAxis(mem,size,init,vtab,name) \
astINVOKE(O,astInitAxis_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitAxisVtab(vtab,name) astINVOKE(V,astInitAxisVtab_(vtab,name))
/* Loader. */
#define astLoadAxis(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadAxis_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckAxis to validate Axis pointers before
   use. This provides a contextual error report if a pointer to the
   wrong sort of object is supplied. */
#define astAxisFormat(this,value) \
astINVOKE(V,astAxisFormat_(astCheckAxis(this),value))
#define astAxisNorm(this,value) \
astINVOKE(V,astAxisNorm_(astCheckAxis(this),value))
#define astAxisUnformat(this,string,value) \
astINVOKE(V,astAxisUnformat_(astCheckAxis(this),string,value))

#if defined(astCLASS)            /* Protected */
#define astAxisAbbrev(this,fmt,str1,str2) \
astINVOKE(V,astAxisAbbrev_(astCheckAxis(this),fmt,str1,str2))
#define astAxisGap(this,gap,ntick) \
astINVOKE(V,astAxisGap_(astCheckAxis(this),gap,ntick))
#define astAxisFields(this,fmt,str,maxfld,fields,nc,val) \
astINVOKE(V,astAxisFields_(astCheckAxis(this),fmt,str,maxfld,fields,nc,val))
#define astAxisIn(this,lo,hi,val,closed) \
astINVOKE(V,astAxisIn_(astCheckAxis(this),lo,hi,val,closed))
#define astAxisDistance(this,v1,v2) \
astINVOKE(V,astAxisDistance_(astCheckAxis(this),v1,v2))
#define astAxisOffset(this,v1,dist) \
astINVOKE(V,astAxisOffset_(astCheckAxis(this),v1,dist))
#define astAxisOverlay(template,result) \
astINVOKE(V,astAxisOverlay_(astCheckAxis(template),astCheckAxis(result)))
#define astClearAxisDigits(this) \
astINVOKE(V,astClearAxisDigits_(astCheckAxis(this)))
#define astClearAxisDirection(this) \
astINVOKE(V,astClearAxisDirection_(astCheckAxis(this)))
#define astClearAxisFormat(this) \
astINVOKE(V,astClearAxisFormat_(astCheckAxis(this)))
#define astClearAxisLabel(this) \
astINVOKE(V,astClearAxisLabel_(astCheckAxis(this)))
#define astClearAxisSymbol(this) \
astINVOKE(V,astClearAxisSymbol_(astCheckAxis(this)))
#define astClearAxisUnit(this) \
astINVOKE(V,astClearAxisUnit_(astCheckAxis(this)))
#define astGetAxisDigits(this) \
astINVOKE(V,astGetAxisDigits_(astCheckAxis(this)))
#define astGetAxisDirection(this) \
astINVOKE(V,astGetAxisDirection_(astCheckAxis(this)))
#define astGetAxisFormat(this) \
astINVOKE(V,astGetAxisFormat_(astCheckAxis(this)))
#define astGetAxisLabel(this) \
astINVOKE(V,astGetAxisLabel_(astCheckAxis(this)))
#define astGetAxisSymbol(this) \
astINVOKE(V,astGetAxisSymbol_(astCheckAxis(this)))
#define astGetAxisUnit(this) \
astINVOKE(V,astGetAxisUnit_(astCheckAxis(this)))
#define astSetAxisDigits(this,digits) \
astINVOKE(V,astSetAxisDigits_(astCheckAxis(this),digits))
#define astSetAxisDirection(this,direction) \
astINVOKE(V,astSetAxisDirection_(astCheckAxis(this),direction))
#define astSetAxisFormat(this,format) \
astINVOKE(V,astSetAxisFormat_(astCheckAxis(this),format))
#define astSetAxisLabel(this,label) \
astINVOKE(V,astSetAxisLabel_(astCheckAxis(this),label))
#define astSetAxisSymbol(this,symbol) \
astINVOKE(V,astSetAxisSymbol_(astCheckAxis(this),symbol))
#define astSetAxisUnit(this,unit) \
astINVOKE(V,astSetAxisUnit_(astCheckAxis(this),unit))
#define astTestAxisDigits(this) \
astINVOKE(V,astTestAxisDigits_(astCheckAxis(this)))
#define astTestAxisDirection(this) \
astINVOKE(V,astTestAxisDirection_(astCheckAxis(this)))
#define astTestAxisFormat(this) \
astINVOKE(V,astTestAxisFormat_(astCheckAxis(this)))
#define astTestAxisLabel(this) \
astINVOKE(V,astTestAxisLabel_(astCheckAxis(this)))
#define astTestAxisSymbol(this) \
astINVOKE(V,astTestAxisSymbol_(astCheckAxis(this)))
#define astTestAxisUnit(this) \
astINVOKE(V,astTestAxisUnit_(astCheckAxis(this)))

#define astClearAxisTop(this) \
astINVOKE(V,astClearAxisTop_(astCheckAxis(this)))
#define astGetAxisTop(this) \
astINVOKE(V,astGetAxisTop_(astCheckAxis(this)))
#define astSetAxisTop(this,top) \
astINVOKE(V,astSetAxisTop_(astCheckAxis(this),top))
#define astTestAxisTop(this) \
astINVOKE(V,astTestAxisTop_(astCheckAxis(this)))

#define astClearAxisBottom(this) \
astINVOKE(V,astClearAxisBottom_(astCheckAxis(this)))
#define astGetAxisBottom(this) \
astINVOKE(V,astGetAxisBottom_(astCheckAxis(this)))
#define astSetAxisBottom(this,bottom) \
astINVOKE(V,astSetAxisBottom_(astCheckAxis(this),bottom))
#define astTestAxisBottom(this) \
astINVOKE(V,astTestAxisBottom_(astCheckAxis(this)))

#endif
#endif
