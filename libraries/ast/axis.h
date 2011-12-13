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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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


/* Macros */
/* ====== */
#if defined(astCLASS)
#define AST__AXIS_GETDEFAULTFORMAT_BUFF_LEN 50
#define AST__AXIS_AXISFORMAT_BUFF_LEN 127
#define AST__AXIS_GETAXISNORMUNIT_BUFF_LEN 127
#define AST__AXIS_GETATTRIB_BUFF_LEN 50
#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
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

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   const char *(* AxisAbbrev)( AstAxis *, const char *, const char *, const char *, int * );
   const char *(* AxisFormat)( AstAxis *, double, int * );
   const char *(* GetAxisFormat)( AstAxis *, int * );
   const char *(* GetAxisLabel)( AstAxis *, int * );
   const char *(* GetAxisSymbol)( AstAxis *, int * );
   const char *(* GetAxisUnit)( AstAxis *, int * );
   const char *(* GetAxisNormUnit)( AstAxis *, int * );
   double (* AxisGap)( AstAxis *, double, int *, int * );
   double (* AxisDistance)( AstAxis *, double, double, int * );
   double (* AxisOffset)( AstAxis *, double, double, int * );
   int (* AxisIn)( AstAxis *, double, double, double, int, int * );
   int (* AxisFields)( AstAxis *, const char *, const char *, int, char **, int *, double *, int * );
   int (* AxisUnformat)( AstAxis *, const char *, double *, int * );
   int (* GetAxisDigits)( AstAxis *, int * );
   int (* GetAxisDirection)( AstAxis *, int * );
   int (* TestAxisDigits)( AstAxis *, int * );
   int (* TestAxisDirection)( AstAxis *, int * );
   int (* TestAxisFormat)( AstAxis *, int * );
   int (* TestAxisLabel)( AstAxis *, int * );
   int (* TestAxisSymbol)( AstAxis *, int * );
   int (* TestAxisUnit)( AstAxis *, int * );
   int (* TestAxisNormUnit)( AstAxis *, int * );
   void (* AxisNorm)( AstAxis *, double *, int * );
   void (* AxisOverlay)( AstAxis *, AstAxis *, int * );
   void (* ClearAxisDigits)( AstAxis *, int * );
   void (* ClearAxisDirection)( AstAxis *, int * );
   void (* ClearAxisFormat)( AstAxis *, int * );
   void (* ClearAxisLabel)( AstAxis *, int * );
   void (* ClearAxisSymbol)( AstAxis *, int * );
   void (* ClearAxisUnit)( AstAxis *, int * );
   void (* SetAxisDigits)( AstAxis *, int, int * );
   void (* SetAxisDirection)( AstAxis *, int, int * );
   void (* SetAxisFormat)( AstAxis *, const char *, int * );
   void (* SetAxisLabel)( AstAxis *, const char *, int * );
   void (* SetAxisSymbol)( AstAxis *, const char *, int * );
   void (* SetAxisUnit)( AstAxis *, const char *, int * );

   double (* GetAxisTop)( AstAxis *, int * );
   int (* TestAxisTop)( AstAxis *, int * );
   void (* ClearAxisTop)( AstAxis *, int * );
   void (* SetAxisTop)( AstAxis *, double, int * );

   double (* GetAxisBottom)( AstAxis *, int * );
   int (* TestAxisBottom)( AstAxis *, int * );
   void (* ClearAxisBottom)( AstAxis *, int * );
   void (* SetAxisBottom)( AstAxis *, double, int * );

} AstAxisVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstAxisGlobals {
   AstAxisVtab Class_Vtab;
   int Class_Init;
   char GetDefaultFormat_Buff[ AST__AXIS_GETDEFAULTFORMAT_BUFF_LEN + 1 ];
   char AxisFormat_Buff[ AST__AXIS_AXISFORMAT_BUFF_LEN + 1 ];
   char GetAxisNormUnit_Buff[ AST__AXIS_GETAXISNORMUNIT_BUFF_LEN + 1 ];
   char GetAttrib_Buff[ AST__AXIS_GETATTRIB_BUFF_LEN + 1 ];
} AstAxisGlobals;

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Axis)             /* Check class membership */
astPROTO_ISA(Axis)               /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstAxis *astAxis_( const char *, int *, ...);
#else
AstAxis *astAxisId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstAxis *astInitAxis_( void *, size_t, int, AstAxisVtab *, const char *, int * );

/* Vtab initialiser. */
void astInitAxisVtab_( AstAxisVtab *, const char *, int * );

/* Loader. */
AstAxis *astLoadAxis_( void *, size_t, AstAxisVtab *, const char *,
                       AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitAxisGlobals_( AstAxisGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
const char *astAxisFormat_( AstAxis *, double, int * );
int astAxisUnformat_( AstAxis *, const char *, double *, int * );
void astAxisNorm_( AstAxis *, double *, int * );

#if defined(astCLASS)            /* Protected */
const char *astAxisAbbrev_( AstAxis *, const char *, const char *, const char *, int * );
const char *astGetAxisFormat_( AstAxis *, int * );
const char *astGetAxisLabel_( AstAxis *, int * );
const char *astGetAxisSymbol_( AstAxis *, int * );
const char *astGetAxisUnit_( AstAxis *, int * );
const char *astGetAxisNormUnit_( AstAxis *, int * );
double astAxisGap_( AstAxis *, double, int *, int * );
double astAxisDistance_( AstAxis *, double, double, int * );
double astAxisOffset_( AstAxis *, double, double, int * );
int astGetAxisDigits_( AstAxis *, int * );
int astGetAxisDirection_( AstAxis *, int * );
int astTestAxisDigits_( AstAxis *, int * );
int astTestAxisDirection_( AstAxis *, int * );
int astAxisFields_( AstAxis *, const char *, const char *, int, char **, int *, double *, int * );
int astAxisIn_( AstAxis *, double, double, double, int, int * );
int astTestAxisFormat_( AstAxis *, int * );
int astTestAxisLabel_( AstAxis *, int * );
int astTestAxisSymbol_( AstAxis *, int * );
int astTestAxisUnit_( AstAxis *, int * );
int astTestAxisNormUnit_( AstAxis *, int * );
void astAxisOverlay_( AstAxis *, AstAxis *, int * );
void astClearAxisDigits_( AstAxis *, int * );
void astClearAxisDirection_( AstAxis *, int * );
void astClearAxisFormat_( AstAxis *, int * );
void astClearAxisLabel_( AstAxis *, int * );
void astClearAxisSymbol_( AstAxis *, int * );
void astClearAxisUnit_( AstAxis *, int * );
void astSetAxisDigits_( AstAxis *, int, int * );
void astSetAxisDirection_( AstAxis *, int, int * );
void astSetAxisFormat_( AstAxis *, const char *, int * );
void astSetAxisLabel_( AstAxis *, const char *, int * );
void astSetAxisSymbol_( AstAxis *, const char *, int * );
void astSetAxisUnit_( AstAxis *, const char *, int * );

double astGetAxisTop_( AstAxis *, int * );
int astTestAxisTop_( AstAxis *, int * );
void astClearAxisTop_( AstAxis *, int * );
void astSetAxisTop_( AstAxis *, double, int * );

double astGetAxisBottom_( AstAxis *, int * );
int astTestAxisBottom_( AstAxis *, int * );
void astClearAxisBottom_( AstAxis *, int * );
void astSetAxisBottom_( AstAxis *, double, int * );

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
#define astCheckAxis(this) astINVOKE_CHECK(Axis,this,0)
#define astVerifyAxis(this) astINVOKE_CHECK(Axis,this,1)

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
astINVOKE(O,astInitAxis_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitAxisVtab(vtab,name) astINVOKE(V,astInitAxisVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadAxis(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadAxis_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckAxis to validate Axis pointers before
   use. This provides a contextual error report if a pointer to the
   wrong sort of object is supplied. */
#define astAxisFormat(this,value) \
astINVOKE(V,astAxisFormat_(astCheckAxis(this),value,STATUS_PTR))
#define astAxisNorm(this,value) \
astINVOKE(V,astAxisNorm_(astCheckAxis(this),value,STATUS_PTR))
#define astAxisUnformat(this,string,value) \
astINVOKE(V,astAxisUnformat_(astCheckAxis(this),string,value,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astAxisAbbrev(this,fmt,str1,str2) \
astINVOKE(V,astAxisAbbrev_(astCheckAxis(this),fmt,str1,str2,STATUS_PTR))
#define astAxisGap(this,gap,ntick) \
astINVOKE(V,astAxisGap_(astCheckAxis(this),gap,ntick,STATUS_PTR))
#define astAxisFields(this,fmt,str,maxfld,fields,nc,val) \
astINVOKE(V,astAxisFields_(astCheckAxis(this),fmt,str,maxfld,fields,nc,val,STATUS_PTR))
#define astAxisIn(this,lo,hi,val,closed) \
astINVOKE(V,astAxisIn_(astCheckAxis(this),lo,hi,val,closed,STATUS_PTR))
#define astAxisDistance(this,v1,v2) \
astINVOKE(V,astAxisDistance_(astCheckAxis(this),v1,v2,STATUS_PTR))
#define astAxisOffset(this,v1,dist) \
astINVOKE(V,astAxisOffset_(astCheckAxis(this),v1,dist,STATUS_PTR))
#define astAxisOverlay(template,result) \
astINVOKE(V,astAxisOverlay_(astCheckAxis(template),astCheckAxis(result),STATUS_PTR))
#define astClearAxisDigits(this) \
astINVOKE(V,astClearAxisDigits_(astCheckAxis(this),STATUS_PTR))
#define astClearAxisDirection(this) \
astINVOKE(V,astClearAxisDirection_(astCheckAxis(this),STATUS_PTR))
#define astClearAxisFormat(this) \
astINVOKE(V,astClearAxisFormat_(astCheckAxis(this),STATUS_PTR))
#define astClearAxisLabel(this) \
astINVOKE(V,astClearAxisLabel_(astCheckAxis(this),STATUS_PTR))
#define astClearAxisSymbol(this) \
astINVOKE(V,astClearAxisSymbol_(astCheckAxis(this),STATUS_PTR))
#define astClearAxisUnit(this) \
astINVOKE(V,astClearAxisUnit_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisDigits(this) \
astINVOKE(V,astGetAxisDigits_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisDirection(this) \
astINVOKE(V,astGetAxisDirection_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisFormat(this) \
astINVOKE(V,astGetAxisFormat_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisLabel(this) \
astINVOKE(V,astGetAxisLabel_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisSymbol(this) \
astINVOKE(V,astGetAxisSymbol_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisUnit(this) \
astINVOKE(V,astGetAxisUnit_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisNormUnit(this) \
astINVOKE(V,astGetAxisNormUnit_(astCheckAxis(this),STATUS_PTR))
#define astSetAxisDigits(this,digits) \
astINVOKE(V,astSetAxisDigits_(astCheckAxis(this),digits,STATUS_PTR))
#define astSetAxisDirection(this,direction) \
astINVOKE(V,astSetAxisDirection_(astCheckAxis(this),direction,STATUS_PTR))
#define astSetAxisFormat(this,format) \
astINVOKE(V,astSetAxisFormat_(astCheckAxis(this),format,STATUS_PTR))
#define astSetAxisLabel(this,label) \
astINVOKE(V,astSetAxisLabel_(astCheckAxis(this),label,STATUS_PTR))
#define astSetAxisSymbol(this,symbol) \
astINVOKE(V,astSetAxisSymbol_(astCheckAxis(this),symbol,STATUS_PTR))
#define astSetAxisUnit(this,unit) \
astINVOKE(V,astSetAxisUnit_(astCheckAxis(this),unit,STATUS_PTR))
#define astTestAxisDigits(this) \
astINVOKE(V,astTestAxisDigits_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisDirection(this) \
astINVOKE(V,astTestAxisDirection_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisFormat(this) \
astINVOKE(V,astTestAxisFormat_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisLabel(this) \
astINVOKE(V,astTestAxisLabel_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisSymbol(this) \
astINVOKE(V,astTestAxisSymbol_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisUnit(this) \
astINVOKE(V,astTestAxisUnit_(astCheckAxis(this),STATUS_PTR))
#define astTestAxisNormUnit(this) \
astINVOKE(V,astTestAxisNormUnit_(astCheckAxis(this),STATUS_PTR))

#define astClearAxisTop(this) \
astINVOKE(V,astClearAxisTop_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisTop(this) \
astINVOKE(V,astGetAxisTop_(astCheckAxis(this),STATUS_PTR))
#define astSetAxisTop(this,top) \
astINVOKE(V,astSetAxisTop_(astCheckAxis(this),top,STATUS_PTR))
#define astTestAxisTop(this) \
astINVOKE(V,astTestAxisTop_(astCheckAxis(this),STATUS_PTR))

#define astClearAxisBottom(this) \
astINVOKE(V,astClearAxisBottom_(astCheckAxis(this),STATUS_PTR))
#define astGetAxisBottom(this) \
astINVOKE(V,astGetAxisBottom_(astCheckAxis(this),STATUS_PTR))
#define astSetAxisBottom(this,bottom) \
astINVOKE(V,astSetAxisBottom_(astCheckAxis(this),bottom,STATUS_PTR))
#define astTestAxisBottom(this) \
astINVOKE(V,astTestAxisBottom_(astCheckAxis(this),STATUS_PTR))

#endif
#endif





