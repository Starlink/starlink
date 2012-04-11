#if !defined( FRAME_INCLUDED )   /* Include this file only once */
#define FRAME_INCLUDED
/*
*+
*  Name:
*     frame.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Frame class.

*  Invocation:
*     #include "frame.h"

*  Description:
*     This include file defines the interface to the Frame class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     A Frame object encapsulates information about a coordinate
*     system, including its axes. It may also act as a "template" to
*     be matched against another Frame object. This process determines
*     whether it is possible to perform a coordinate transformation
*     between the two coordinates systems they describe.

*  Inheritance:
*     The Frame class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     Nin (integer, readonly)
*        The Frame class sets this value to be equal to the number of
*        Frame axes.
*     Nout (integer, readonly)
*        The Frame class sets this value to be equal to the number of
*        Frame axes.

*  New Attributes Defined:
*     AlignSystem (string)
*        This attribute takes a value to identify the coordinate system
*        in which the Frame should be aligned with other Frames.
*     Digits [or Digits(axis)] (integer)
*        Specifies how many digits of precision are required by
*        default when a coordinate value for a Frame is formatted
*        (e.g. using the astFormat method). The Digits value acts as a
*        default only and is over-ridden if a Format string is
*        specified for an axis explicitly.
*
*        The default Digits value for a Frame is 7. This attribute
*        normally applies to all the Frame's axes, but reference may
*        be made to a particular axis by adding an axis subscript
*        (e.g. Digits(1)). If a value is set for an individual axis,
*        it will over-ride the Digits value for the Frame as a whole
*        when formatting values for that axis.
*     Direction(axis) (integer)
*        A boolean value which specifies how coordinate values for
*        each Frame axis should be displayed (e.g. in graphs). By
*        default, it has the value one, indicating that they should be
*        shown in the conventional sense (i.e. increasing left to
*        right for an abscissa and bottom to top for an ordinate). If
*        set to zero, this attribute indicates that the direction
*        should be reversed (as would often be done for an
*        astronomical magnitude or a right ascension axis, for
*        example).
*     Epoch (double)
*        This value is used to qualify coordinate systems by
*        giving the moment in time when the coordinates are known to
*        be correct. Often, this will be the date of observation.
*     Format(axis) (string)
*        Specifies the format to be used to display coordinate values
*        for each Frame axis (i.e. to convert them from binary to
*        character form). The interpretation of this string (e.g. by
*        derived classes) is left to the astFormat method which, in
*        turn will invoke the astAxisFormat for the Axis object that
*        describes each axis. By default, the Frame class supplies an
*        Axis class object for each axis and this will interpret this
*        parameter as a C "printf" format string which should be
*        capable of formatting a single coordinate value stored as a
*        double (e.g. "%1.7G"). If no Format string is set, the
*        default format is based on the value of the Digits attribute.
*     Label(axis) (string)
*        Specifies the label to be attached to each Frame axis when it
*        is represented in (e.g.) a graph. It is intended purely for
*        interpretation by human readers and not by software. The
*        default supplied by the Frame class is the string "Axis <n>",
*        where <n> is 1, 2, etc. for each successive axis.
*     MatchEnd (integer)
*        A boolean value that controls how a Frame behaves when used
*        as a template to match another Frame. If it is zero and a
*        template Frame matches a target frame which has a different
*        number of axes, then the axes wich occur first in the target
*        frame will be matched and any trailing axes in either the
*        target or template will be discarded (if necessary). If it is
*        non-zero, however, the last axes in each frame will be
*        matched and any un-matched leading axes will be discarded
*        instead. The default value supplied by the Frame class is
*        zero.
*     MaxAxes (integer)
*        Specifies the maximum number of axes in a target Frame that
*        can be matched when using the Frame as a template. Normally,
*        by default, this value is equal to the number of Frame axes,
*        so that a Frame will only match another Frame with the same
*        number of axes as itself. By setting a different value,
*        however, Frames with different numbers of axes may be matched
*        (the MatchEnd attribute then determines which of the
*        individual axes are matched).
*
*        When setting this value, the value of the MinAxes attribute
*        may be silently changed so that it remains consistent with
*        (i.e. does not exceed) the new value. The default value may
*        also be reduced if necessary to remain consistent with the
*        MinAxes value.
*     MinAxes (integer)
*        Specifies the minimum number of axes in a target Frame that
*        can be matched when using the Frame as a template. Normally,
*        by default, this value is equal to the number of Frame axes,
*        so that a Frame will only match another Frame with the same
*        number of axes as itself. By setting a different value,
*        however, Frames with different numbers of axes may be matched
*        (the MatchEnd attribute then determines which of the
*        individual axes are matched).
*
*        When setting this value, the value of the MaxAxes attribute
*        may be silently changed so that it remains consistent with
*        (i.e. is not less than) the new value. The default value may
*        also be reduced if necessary to remain consistent with the
*        MaxAxes value.
*     Domain (string)
*        A string which may be used to identify the physical domain to
*        which a Frame applies and used as an additional key when
*        matching a target Frame with a template. If the Domain
*        attribute in the template Frame is set, then only target
*        frames with the same Domain value will be matched. If a
*        Domain is not set in the template Frame, the target Frame's
*        Domain value will be ignored and has no effect on
*        matching. The default value supplied by the Frame class is an
*        empty string. Domain values are automatically converted to
*        upper case and all white space is removed before use.
*     Naxes (integer)
*        A read-only attribute that gives the number of axes in a
*        Frame (i.e.  the number of dimensions of the space which the
*        Frame describes). This value is determined when the Frame is
*        created.
*     Permute (integer)
*        A boolean value which specifies whether the axis order of a
*        target Frame may be permuted in order to obtain a match with
*        a template. If this value is set to zero in the template
*        Frame, it will only match a target if it can do so without
*        changing the order of its axes. The default value supplied by
*        the Frame class is 1 (i.e. allow axis permutations).
*     PreserveAxes (integer)
*        A boolean value which determines how the "result" Frame is
*        produced whan a target frame is matched by a template. If
*        this value is zero in the template Frame, then the result
*        Frame will have the same number of axes as the template. If
*        it is non-zero, however, the axes of the target Frame will be
*        preserved, so that the result Frame will have the same number
*        of axes as the target. The default supplied by the Frame
*        class is zero (i.e. target axes are not preserved).
*
*        The main use for this attribute is when the MaxAxes and/or
*        MinAxes attributes have been set to search for a Frame which
*        may have a different number of axes from the template. For
*        example, if a 2-dimensional template Frame matches a
*        3-dimensional target Frame, then by default the result is
*        2-dimensional and the last axis (normally) will be
*        discarded. However, if the template's PreserveAxes value is
*        non-zero, the result will instead be 3-dimensional to
*        correspond with the target Frame.
*     Symbol(axis) (string)
*        Specifies the symbol to be used to represent coordinate
*        values for each Frame axis in "short form", such as in
*        algebraic expressions where a full description of the axis
*        would be inappropriate. Examples include "RA" and "Dec" (for
*        Right Ascension and Declination).
*
*        The default supplied by the Frame class is the string
*        "<Domain><n>", where <n> is 1, 2, etc. for successive axes,
*        and <Domain> is the value of the Frame's Domain attribute
*        (with any white space replaced by underscores and truncated
*        if necessary so that the final string does not exceed 15
*        characters). If no Domain value has been set, "x" is used as
*        the <Domain> value in constructing this default string.
*     System (string)
*        This attribute takes a value to identify the coordinate system
*        used to describe positions within the domain of the Frame.
*     Title (string)
*        Specifies a string to be used as a title on (e.g.) graphs to
*        describe the coordinate system which the Frame
*        represents. Examples would be "Detector Coordinates" or
*        "Galactic Coordinates". This string is intended solely for
*        interpretation by human readers and not by software. The
*        default supplied by the Frame class is "<n>-D Coordinate
*        System", where <n> is the number of Frame axes.
*     Unit(axis) (string)
*        Describes the units used to represent coordinate values on
*        each Frame axis. The default supplied by the Frame class is
*        an empty string.

*  Methods Over-Ridden:
*     Public:
*        astGetNin
*           Get the number of input coordinates for a Frame.
*        astGetNout
*           Get the number of output coordinates for a Frame.
*        astTransform
*           Use a Frame to transform a set of points.

*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Frame.
*        astGetAttrib
*           Get an attribute value for a Frame.
*        astReportPoints
*           Report the effect of transforming a set of points using a Frame.
*        astSetAttrib
*           Set an attribute value for a Frame.
*        astTestAttrib
*           Test if an attribute value has been set for a Frame.

*  New Methods Defined:
*     Public:
*        astAngle
*           Calculate the angle between three points.
*        astAxAngle
*           Find the angle from an axis to a line through two points.
*        astAxDistance
*           Calculate the distance between two axis values
*        astAxOffset
*           Calculate an offset along an axis
*        astConvert
*           Determine how to convert between two coordinate systems.
*        astDistance
*           Calculate the distance between two points.
*        astFindFrame
*           Find a coordinate system with specified characteristics
*        astFormat
*           Format a coordinate value for a Frame axis.
*        astNorm
*           Normalise a set of Frame coordinates.
*        astOffset
*           Calculate an offset along a geodesic curve.
*        astOffset2
*           Calculate an offset along a geodesic curve for a 2D Frame.
*        astPermAxes
*           Permute the order of a Frame's axes.
*        astPickAxes
*           Create a new Frame by picking axes from an existing one.
*        astResolve
*           Resolve a vector into two orthogonal components.
*        astUnformat
*           Read a formatted coordinate value for a Frame axis.

*     Protected:
*        astAbbrev
*           Abbreviate a formatted Frame axis value by skipping
*           leading fields.
*        astCheckPerm
*           Check that an array contains a valid permutation.
*        astClearDigits
*           Clear the Digits attribute for a Frame.
*        astClearDirection
*           Clear the Direction attribute for a Frame axis.
*        astClearDomain
*           Clear the Domain attribute for a Frame.
*        astClearFormat
*           Clear the Format attribute for a Frame axis.
*        astClearLabel
*           Clear the Label attribute for a Frame axis.
*        astClearMatchEnd
*           Clear the MatchEnd attribute for a Frame.
*        astClearMaxAxes
*           Clear the MaxAxes attribute for a Frame.
*        astClearMinAxes
*           Clear the MinAxes attribute for a Frame.
*        astClearPermute
*           Clear the Permute attribute for a Frame.
*        astClearPreserveAxes
*           Clear the PreserveAxes attribute for a Frame.
*        astClearSymbol
*           Clear the Symbol attribute for a Frame axis.
*        astClearSystem
*           Clear the value of the System attribute for a Frame.
*        astClearTitle
*           Clear the Title attribute for a Frame.
*        astClearUnit
*           Clear the Unit attribute for a Frame axis.
*        astConvertX
*           Determine how to convert between two coordinate systems.
*        astFields
*           Identify the fields within a formatted Frame axis value.
*        astGap
*           Find a "nice" gap for tabulating Frame axis values.
*        astGetAxis
*           Obtain a pointer to a specified Axis from a Frame.
*        astGetDigits
*           Get the value of the Digits attribute for a Frame.
*        astGetDirection
*           Get the value of the Direction attribute for a Frame axis.
*        astGetDomain
*           Get a pointer to the Domain attribute for a Frame.
*        astGetFormat
*           Get a pointer to the Format attribute for a Frame axis.
*        astGetLabel
*           Get a pointer to the Label attribute for a Frame axis.
*        astGetMatchEnd
*           Get the value of the MatchEnd attribute for a Frame.
*        astGetMaxAxes
*           Get the value of the MaxAxes attribute for a Frame.
*        astGetMinAxes
*           Get the value of the MinAxes attribute for a Frame.
*        astGetNaxes
*           Determine how many axes a Frame has.
*        astGetPerm
*           Access the axis permutation array for a Frame.
*        astGetPermute
*           Get the value of the Permute attribute for a Frame.
*        astGetPreserveAxes
*           Get the value of the PreserveAxes attribute for a Frame.
*        astGetSymbol
*           Get a pointer to the Symbol attribute for a Frame axis.
*        astGetSystem
*           Get the value of the System attribute for a Frame.
*        astGetTitle
*           Get a pointer to the Title attribute for a Frame.
*        astGetUnit
*           Get a pointer to the Unit attribute for a Frame axis.
*        astIsUnitFrame
*           Returns a flag indicating if a Frame is equivalent to a UnitMap.
*        astMatch
*           Determine if conversion is possible between two coordinate systems.
*        astOverlay
*           Overlay the attributes of a template Frame on to another Frame.
*        astPrimaryFrame
*           Uniquely identify a primary Frame and one of its axes.
*        astResolvePoints
*           Resolve many vectors into two orthogonal components.
*        astSetAxis
*           Set a new Axis for a Frame.
*        astSetDigits
*           Set the value of the Digits attribute for a Frame.
*        astSetDirection
*           Set the value of the Direction attribute for a Frame axis.
*        astSetDomain
*           Set the value of the Domain attribute for a Frame.
*        astSetFormat
*           Set the value of the Format attribute for a Frame axis.
*        astSetLabel
*           Set the value of the Label attribute for a Frame axis.
*        astSetMatchEnd
*           Set the value of the MatchEnd attribute for a Frame.
*        astSetMaxAxes
*           Set the value of the MaxAxes attribute for a Frame.
*        astSetMinAxes
*           Set the value of the MinAxes attribute for a Frame.
*        astSetPermute
*           Set the value of the Permute attribute for a Frame.
*        astSetPreserveAxes
*           Set the value of the PreserveAxes attribute for a Frame.
*        astSetSymbol
*           Set the value of the Symbol attribute for a Frame axis.
*        astSetSystem
*           Set the value of the System attribute for a Frame.
*        astSetTitle
*           Set the value of the Title attribute for a Frame.
*        astSetUnit
*           Set the value of the Unit attribute for a Frame axis.
*        astSubFrame
*           Select axes from a Frame and convert to the new coordinate system.
*        astTestDigits
*           Test whether a value has been set for the Digits attribute of a
*           Frame.
*        astTestDirection
*           Test whether a value has been set for the Direction attribute of a
*           Frame axis.
*        astTestDomain
*           Test whether a value has been set for the Domain attribute of a
*           Frame.
*        astTestFormat
*           Test whether a value has been set for the Format attribute of a
*           Frame axis.
*        astTestLabel
*           Test whether a value has been set for the Label attribute of a
*           Frame axis.
*        astTestMatchEnd
*           Test whether a value has been set for the MatchEnd attribute of a
*           Frame.
*        astTestMaxAxes
*           Test whether a value has been set for the MaxAxes attribute of a
*           Frame.
*        astTestMinAxes
*           Test whether a value has been set for the MinAxes attribute of a
*           Frame.
*        astTestPermute
*           Test whether a value has been set for the Permute attribute of a
*           Frame.
*        astTestPreserveAxes
*           Test whether a value has been set for the PreserveAxes attribute of
*           a Frame.
*        astTestSymbol
*           Test whether a value has been set for the Symbol attribute of a
*           Frame axis.
*        astTestSystem
*           Test whether a value has been set for the System attribute of a
*           Frame.
*        astTestTitle
*           Test whether a value has been set for the Title attribute of a
*           Frame.
*        astTestUnit
*           Test whether a value has been set for the Unit attribute of a Frame
*           axis.
*        astValidateAxis
*           Validate and permute a Frame's axis index.
*        astValidateAxisSelection
*           Check that a set of axes selected from a Frame is valid.
*        astValidateSystem
*           Validate a Frame's System attribute.
*        astSystemString
*           Return a string representation of a System code.
*        astSystemCode
*           Return a code for a string representation of a System value

*  Other Class Functions:
*     Public:
*        astFrame
*           Create a Frame.
*        astIsAFrame
*           Test class membership.

*     Protected:
*        astCheckFrame
*           Validate class membership.
*        astInitFrame
*           Initialise a Frame.
*        astInitFrameVtab
*           Initialise the virtual function table for the Frame class.
*        astLoadFrame
*           Load a Frame.

*  Macros:
*     Public:
*        None.

*     Protected:
*        AST__BADSYSTEM
*           A "bad" (undefined) value for the System attribute.

*  Type Definitions:
*     Public:
*        AstFrame
*           Frame object type.

*     Protected:
*        AstFrameVtab
*           Frame virtual function table type.
*        AstSystemType
*           Enumerated type used for the System attribute.

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
*        Tidied up, etc.
*     11-SEP-1996 (RFWS):
*        Added astGap (written by DSB).
*     10-JUN-1997 (RFWS):
*        Revised astConvert and added astFindFrame.
*     15-FEB-1998 (RFWS):
*        Added astUnformat.
*     21-JUN-2001 (DSB):
*        Added astAngle and astOffset2.
*     29-AUG-2001 (DSB):
*        Added astAxDistance and astAxOffset.
*     4-SEP-2001 (DSB):
*        Added astResolve.
*     9-SEP-2001 (DSB):
*        Added astBear.
*     21-SEP-2001 (DSB):
*        Replace astBear with astAxAngle.
*     15-NOV-2002 (DSB):
*        Moved System and Epoch attributes from SkyFrame into this class.
*        Added AlignSystem attribute.
*     8-JAN-2003 (DSB):
*        Added protected astInitFrameVtab method.
*     24-JAN-2004 (DSB):
*        o  Added astFields.
*        o  Added argument "fmt" to astAbbrev.
*     24-JUN-2004 (DSB):
*        Remove unused entry "void (* SetMatchRange)( AstFrame *, int, int );"
*        from AstFrameVtab structure.
*     9-NOV-2004 (DSB):
*        Added protected astIsAUnitFrame method.
*     12-AUG-2005 (DSB):
*        Added ObsLat and ObsLon attributes.
*     14-OCT-2006 (DSB):
*        Added dut1 to the Frame structure.
*        Added Dut1 accessor methods.
*     17-MAY-2007 (DSB):
*        Added NormUnit attribute.
*     14-JAN-2009 (DSB):
*        Added astIntersect method.
*     18-JUN-2009 (DSB):
*        Added ObsAlt attribute.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "axis.h"                /* Coordinate Axis class */
#include "mapping.h"             /* Coordinate mappings (parent class) */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#include <stddef.h>

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

/* A bad value for the System attribute. */
#define AST__BADSYSTEM -1

/* The legal System values recognized by this class of Frame. */
#define AST__CART 0

/* Flag bitmasks for use with astSetFrameFlags. */
# define AST__INTFLAG 1  /* FrameSet integrity is currently being restored */

/* Define constants used to size global arrays in this module. */
#define AST__FRAME_LABEL_BUFF_LEN 100       /* Max length of default axis Label string */
#define AST__FRAME_SYMBOL_BUFF_LEN 50       /* Max length of default axis Symbol string */
#define AST__FRAME_TITLE_BUFF_LEN 100       /* Max length of default title string */
#define AST__FRAME_GETATTRIB_BUFF_LEN 50    /* Max length of string returned by GetAttrib */
#define AST__FRAME_ASTFMTDECIMALYR_BUFF_LEN 50    /* Max length of string returned by GetAttrib */
#define AST__FRAME_ASTFORMATID_MAX_STRINGS 50     /* Number of string values buffer by astFormatID*/

#endif

/* Type Definitions. */
/* ================= */
/* Integer type used to store the System attribute values. */
typedef int AstSystemType;

/* Frame structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstFrame {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstAxis **axis;               /* Pointer to array of Axis objects */
   char *domain;                 /* Pointer to Domain string */
   char *title;                  /* Pointer to Title string */
   double epoch;                 /* Epoch as Modified Julian Date */
   double obslat;                /* Geodetic latitude of observer */
   double obslon;                /* Geodetic longitude of observer */
   double obsalt;                /* Height above reference spheroid (geodetic, metres) */
   double dut1;                  /* UT1-UTC in seconds */
   int *perm;                    /* Pointer to axis permutation array */
   int digits;                   /* Default digits of precision */
   int match_end;                /* Match final axes of target? */
   int active_unit;              /* Use Unit when aligning Frames? */
   int max_axes;                 /* Minimum no. axes matched */
   int min_axes;                 /* Max. no. axes matched */
   int naxes;                    /* Number of axes */
   int permute;                  /* Permute axes in order to match? */
   int preserve_axes;            /* Preserve target axes? */
   AstSystemType system;         /* Code identifying coordinate system */
   AstSystemType alignsystem;    /* Code for Alignment coordinate system */
   int flags;                    /* Bit mask containing various protected flags */
} AstFrame;

/* Cached Line structure. */
/* ---------------------- */
/* This structure contains information describing a line segment within a
   2D Frame. It is used by other classes to store intermediate cached values
   relating to the line in order to speed up repeated operations on the
   line. */

typedef struct AstLineDef {
   AstFrame *frame;            /* Pointer to Frame in which the line is defined */
   double length;              /* Line length */
   int infinite;               /* Disregard the start and end of the line? */
   double start[2];            /* Frame axis values at line start */
   double end[2];              /* Frame axis values at line end */
   double dir[2];              /* Unit vector defining line direction */
   double q[2];                /* Unit vector perpendicular to line */
} AstLineDef;

/* Virtual function table. */
/* ----------------------- */
/* The virtual function table makes a forward reference to the
   AstFrameSet structure which is not defined until "frameset.h" is
   included (below). Hence make a preliminary definition available
   now. */
struct AstFrameSet;

/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping;       /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstAxis *(* GetAxis)( AstFrame *, int, int * );
   AstFrame *(* PickAxes)( AstFrame *, int, const int[], AstMapping **, int * );
   AstLineDef *(* LineDef)( AstFrame *, const double[2], const double[2], int * );
   AstPointSet *(* ResolvePoints)( AstFrame *, const double [], const double [], AstPointSet *, AstPointSet *, int * );
   const char *(* Abbrev)( AstFrame *, int, const char *, const char *, const char *, int * );
   const char *(* Format)( AstFrame *, int, double, int * );
   const char *(* GetDomain)( AstFrame *, int * );
   const char *(* GetFormat)( AstFrame *, int, int * );
   const char *(* GetLabel)( AstFrame *, int, int * );
   const char *(* GetSymbol)( AstFrame *, int, int * );
   const char *(* GetTitle)( AstFrame *, int * );
   const char *(* GetNormUnit)( AstFrame *, int, int * );
   const char *(* GetUnit)( AstFrame *, int, int * );
   const int *(* GetPerm)( AstFrame *, int * );
   double (* Angle)( AstFrame *, const double[], const double[], const double[], int * );
   double (* Distance)( AstFrame *, const double[], const double[], int * );
   double (* Gap)( AstFrame *, int, double, int *, int * );
   int (* Fields)( AstFrame *, int, const char *, const char *, int, char **, int *, double *, int * );
   double (* AxDistance)( AstFrame *, int, double, double, int * );
   double (* AxOffset)( AstFrame *, int, double, double, int * );
   int (* AxIn)( AstFrame *, int, double, double, double, int, int * );
   int (* GetDigits)( AstFrame *, int * );
   int (* GetDirection)( AstFrame *, int, int * );
   int (* GetMatchEnd)( AstFrame *, int * );
   int (* GetMaxAxes)( AstFrame *, int * );
   int (* GetMinAxes)( AstFrame *, int * );
   int (* GetNaxes)( AstFrame *, int * );
   int (* GetPermute)( AstFrame *, int * );
   int (* GetPreserveAxes)( AstFrame *, int * );
   int (* IsUnitFrame)( AstFrame *, int * );
   int (* LineCrossing)( AstFrame *, AstLineDef *, AstLineDef *, double **, int * );
   int (* LineContains)( AstFrame *, AstLineDef *, int, double *, int * );
   int (* Match)( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
   int (* SubFrame)( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
   int (* TestDigits)( AstFrame *, int * );
   int (* TestDirection)( AstFrame *, int, int * );
   int (* TestDomain)( AstFrame *, int * );
   int (* TestFormat)( AstFrame *, int, int * );
   int (* TestLabel)( AstFrame *, int, int * );
   int (* TestMatchEnd)( AstFrame *, int * );
   int (* TestMaxAxes)( AstFrame *, int * );
   int (* TestMinAxes)( AstFrame *, int * );
   int (* TestPermute)( AstFrame *, int * );
   int (* TestPreserveAxes)( AstFrame *, int * );
   int (* TestSymbol)( AstFrame *, int, int * );
   int (* TestTitle)( AstFrame *, int * );
   int (* TestUnit)( AstFrame *, int, int * );
   int (* Unformat)( AstFrame *, int, const char *, double *, int * );
   int (* ValidateAxis)( AstFrame *, int, int, const char *, int * );
   AstSystemType (* ValidateSystem)( AstFrame *, AstSystemType, const char *, int * );
   AstSystemType (* SystemCode)( AstFrame *, const char *, int * );
   const char *(* SystemString)( AstFrame *, AstSystemType, int * );
   struct AstFrameSet *(* Convert)( AstFrame *, AstFrame *, const char *, int * );
   struct AstFrameSet *(* ConvertX)( AstFrame *, AstFrame *, const char *, int * );
   struct AstFrameSet *(* FindFrame)( AstFrame *, AstFrame *, const char *, int * );
   void (* MatchAxes)( AstFrame *, AstFrame *, int[], int * );
   void (* MatchAxesX)( AstFrame *, AstFrame *, int[], int * );
   void (* CheckPerm)( AstFrame *, const int *, const char *, int * );
   void (* ClearDigits)( AstFrame *, int * );
   void (* ClearDirection)( AstFrame *, int, int * );
   void (* ClearDomain)( AstFrame *, int * );
   void (* ClearFormat)( AstFrame *, int, int * );
   void (* ClearLabel)( AstFrame *, int, int * );
   void (* ClearMatchEnd)( AstFrame *, int * );
   void (* ClearMaxAxes)( AstFrame *, int * );
   void (* ClearMinAxes)( AstFrame *, int * );
   void (* ClearPermute)( AstFrame *, int * );
   void (* ClearPreserveAxes)( AstFrame *, int * );
   void (* ClearSymbol)( AstFrame *, int, int * );
   void (* ClearTitle)( AstFrame *, int * );
   void (* ClearUnit)( AstFrame *, int, int * );
   void (* Intersect)( AstFrame *, const double[2], const double[2], const double[2], const double[2], double[2], int * );
   void (* Norm)( AstFrame *, double[], int * );
   void (* NormBox)( AstFrame *, double *, double *, AstMapping *, int * );
   void (* Offset)( AstFrame *, const double[], const double[], double, double[], int * );
   double (* AxAngle)( AstFrame *, const double[2], const double[2], int, int * );
   double (* Offset2)( AstFrame *, const double[2], double, double, double[2], int * );
   void (* Overlay)( AstFrame *, const int *, AstFrame *, int * );
   void (* PermAxes)( AstFrame *, const int[], int * );
   void (* PrimaryFrame)( AstFrame *, int, AstFrame **, int *, int * );
   void (* Resolve)( AstFrame *, const double [], const double [], const double [], double [], double *, double *, int * );
   void (* SetAxis)( AstFrame *, int, AstAxis *, int * );
   void (* SetDigits)( AstFrame *, int, int * );
   void (* SetDirection)( AstFrame *, int, int, int * );
   void (* SetDomain)( AstFrame *, const char *, int * );
   void (* SetFormat)( AstFrame *, int, const char *, int * );
   void (* SetLabel)( AstFrame *, int, const char *, int * );
   void (* SetMatchEnd)( AstFrame *, int, int * );
   void (* SetMaxAxes)( AstFrame *, int, int * );
   void (* SetMinAxes)( AstFrame *, int, int * );
   void (* SetPermute)( AstFrame *, int, int * );
   void (* SetPreserveAxes)( AstFrame *, int, int * );
   void (* SetSymbol)( AstFrame *, int, const char *, int * );
   void (* SetTitle)( AstFrame *, const char *, int * );
   void (* SetUnit)( AstFrame *, int, const char *, int * );
   void (* ValidateAxisSelection)( AstFrame *, int, const int *, const char *, int * );
   void (* LineOffset)( AstFrame *, AstLineDef *, double, double, double[2], int * );
   AstPointSet *(* FrameGrid)( AstFrame *, int, const double *, const double *, int * );

   double (* GetTop)( AstFrame *, int, int * );
   int (* TestTop)( AstFrame *, int, int * );
   void (* ClearTop)( AstFrame *, int, int * );
   void (* SetTop)( AstFrame *, int, double, int * );

   double (* GetBottom)( AstFrame *, int, int * );
   int (* TestBottom)( AstFrame *, int, int * );
   void (* ClearBottom)( AstFrame *, int, int * );
   void (* SetBottom)( AstFrame *, int, double, int * );

   AstSystemType (* GetSystem)( AstFrame *, int * );
   int (* TestSystem)( AstFrame *, int * );
   void (* ClearSystem)( AstFrame *, int * );
   void (* SetSystem)( AstFrame *, AstSystemType, int * );

   AstSystemType (* GetAlignSystem)( AstFrame *, int * );
   int (* TestAlignSystem)( AstFrame *, int * );
   void (* ClearAlignSystem)( AstFrame *, int * );
   void (* SetAlignSystem)( AstFrame *, AstSystemType, int * );

   double (* GetEpoch)( AstFrame *, int * );
   int (* TestEpoch)( AstFrame *, int * );
   void (* ClearEpoch)( AstFrame *, int * );
   void (* SetEpoch)( AstFrame *, double, int * );

   int (* TestActiveUnit)( AstFrame *, int * );
   int (* GetActiveUnit)( AstFrame *, int * );
   void (* SetActiveUnit)( AstFrame *, int, int * );

   double (* GetObsLon)( AstFrame *, int * );
   int (* TestObsLon)( AstFrame *, int * );
   void (* ClearObsLon)( AstFrame *, int * );
   void (* SetObsLon)( AstFrame *, double, int * );

   double (* GetObsLat)( AstFrame *, int * );
   int (* TestObsLat)( AstFrame *, int * );
   void (* ClearObsLat)( AstFrame *, int * );
   void (* SetObsLat)( AstFrame *, double, int * );

   double (* GetObsAlt)( AstFrame *, int * );
   int (* TestObsAlt)( AstFrame *, int * );
   void (* ClearObsAlt)( AstFrame *, int * );
   void (* SetObsAlt)( AstFrame *, double, int * );

   double (* GetDut1)( AstFrame *, int * );
   int (* TestDut1)( AstFrame *, int * );
   void (* ClearDut1)( AstFrame *, int * );
   void (* SetDut1)( AstFrame *, double, int * );

   void (* SetFrameFlags)( AstFrame *, int, int * );
   int (* GetFrameFlags)( AstFrame *, int * );

} AstFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstFrameGlobals {
   AstFrameVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__FRAME_GETATTRIB_BUFF_LEN + 1 ];
   char *AstFormatID_Strings[ AST__FRAME_ASTFORMATID_MAX_STRINGS ];
   int AstFormatID_Istr;
   int AstFormatID_Init;
   char Label_Buff[ AST__FRAME_LABEL_BUFF_LEN + 1 ];
   char Symbol_Buff[ AST__FRAME_SYMBOL_BUFF_LEN + 1 ];
   char Title_Buff[ AST__FRAME_TITLE_BUFF_LEN + 1 ];
   char AstFmtDecimalYr_Buff[ AST__FRAME_ASTFMTDECIMALYR_BUFF_LEN + 1 ];
} AstFrameGlobals;

#endif
#endif

/* More include files. */
/* =================== */
/* The interface to the FrameSet class must be included here (after
   the type definitions for the Frame class) because "frameset.h"
   itself includes this file ("frame.h"), although "frameset.h" refers
   to the AstFrameSet structure above. This seems a little strange at
   first, but is simply analogous to making a forward reference to a
   structure type when recursively defining a normal C structure
   (except that here the definitions happen to be in separate include
   files). */
#include "frameset.h"

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Frame)            /* Check class membership */
astPROTO_ISA(Frame)              /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstFrame *astFrame_( int, const char *, int *, ...);
#else
AstFrame *astFrameId_( int, const char *, ... )__attribute__((format(printf,2,3)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFrame *astInitFrame_( void *, size_t, int, AstFrameVtab *, const char *,
                         int, int * );

/* Vtab initialiser. */
void astInitFrameVtab_( AstFrameVtab *, const char *, int * );

/* Loader. */
AstFrame *astLoadFrame_( void *, size_t, AstFrameVtab *,
                         const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitFrameGlobals_( AstFrameGlobals * );
#endif
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstFrameSet *astConvert_( AstFrame *, AstFrame *, const char *, int * );
AstFrameSet *astFindFrame_( AstFrame *, AstFrame *, const char *, int * );
double astAngle_( AstFrame *, const double[], const double[], const double[], int * );
double astAxAngle_( AstFrame *, const double[2], const double[2], int, int * );
double astAxDistance_( AstFrame *, int, double, double, int * );
double astAxOffset_( AstFrame *, int, double, double, int * );
double astDistance_( AstFrame *, const double[], const double[], int * );
double astOffset2_( AstFrame *, const double[2], double, double, double[2], int * );
int astGetActiveUnit_( AstFrame *, int * );
void astIntersect_( AstFrame *, const double[2], const double[2], const double[2], const double[2], double[2], int * );
void astMatchAxes_( AstFrame *, AstFrame *, int[], int * );
void astNorm_( AstFrame *, double[], int * );
void astOffset_( AstFrame *, const double[], const double[], double, double[], int * );
void astResolve_( AstFrame *, const double [], const double [], const double [], double [], double *, double *, int * );
void astSetActiveUnit_( AstFrame *, int, int * );

#if defined(astCLASS)            /* Protected */
void astNormBox_( AstFrame *, double *, double *, AstMapping *, int * );
AstFrame *astPickAxes_( AstFrame *, int, const int[], AstMapping **, int * );
const char *astFormat_( AstFrame *, int, double, int * );
int astUnformat_( AstFrame *, int, const char *, double *, int * );
void astPermAxes_( AstFrame *, const int[], int * );
#else
AstFrame *astPickAxesId_( AstFrame *, int, const int[], AstMapping **, int * );
const char *astFormatId_( AstFrame *, int, double, int * );
int astUnformatId_( AstFrame *, int, const char *, double *, int * );
void astPermAxesId_( AstFrame *, const int[], int * );
#endif

#if defined(astCLASS)            /* Protected */
int astAxIn_( AstFrame *, int, double, double, double, int, int * );
AstAxis * astGetAxis_( AstFrame *, int, int * );
AstFrameSet *astConvertX_( AstFrame *, AstFrame *, const char *, int * );
void astMatchAxesX_( AstFrame *, AstFrame *, int[], int * );
AstLineDef *astLineDef_( AstFrame *, const double[2], const double[2], int * );
AstPointSet *astResolvePoints_( AstFrame *, const double [], const double [], AstPointSet *, AstPointSet *, int * );
const char *astAbbrev_( AstFrame *, int, const char *, const char *, const char *, int * );
const char *astGetDomain_( AstFrame *, int * );
const char *astGetFormat_( AstFrame *, int, int * );
const char *astGetLabel_( AstFrame *, int, int * );
const char *astGetSymbol_( AstFrame *, int, int * );
const char *astGetTitle_( AstFrame *, int * );
const char *astGetUnit_( AstFrame *, int, int * );
const char *astGetNormUnit_( AstFrame *, int, int * );
const int *astGetPerm_( AstFrame *, int * );
double astGap_( AstFrame *, int, double, int *, int * );
int astFields_( AstFrame *, int, const char *, const char *, int, char **, int *, double *, int * );
int astGetDigits_( AstFrame *, int * );
int astGetDirection_( AstFrame *, int, int * );
int astGetMatchEnd_( AstFrame *, int * );
int astGetMaxAxes_( AstFrame *, int * );
int astGetMinAxes_( AstFrame *, int * );
int astGetNaxes_( AstFrame *, int * );
int astGetPermute_( AstFrame *, int * );
int astGetPreserveAxes_( AstFrame *, int * );
int astIsUnitFrame_( AstFrame *, int * );
int astLineCrossing_( AstFrame *, AstLineDef *, AstLineDef *, double **, int * );
int astLineContains_( AstFrame *, AstLineDef *, int, double *, int * );
int astMatch_( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
int astSubFrame_( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
int astTestDigits_( AstFrame *, int * );
int astTestDirection_( AstFrame *, int, int * );
int astTestDomain_( AstFrame *, int * );
int astTestFormat_( AstFrame *, int, int * );
int astTestLabel_( AstFrame *, int, int * );
int astTestMatchEnd_( AstFrame *, int * );
int astTestMaxAxes_( AstFrame *, int * );
int astTestMinAxes_( AstFrame *, int * );
int astTestPermute_( AstFrame *, int * );
int astTestPreserveAxes_( AstFrame *, int * );
int astTestSymbol_( AstFrame *, int, int * );
int astTestTitle_( AstFrame *, int * );
int astTestUnit_( AstFrame *, int, int * );
int astValidateAxis_( AstFrame *, int, int, const char *, int * );
AstSystemType astValidateSystem_( AstFrame *, AstSystemType, const char *, int * );
AstSystemType astSystemCode_( AstFrame *, const char *, int * );
const char *astSystemString_( AstFrame *, AstSystemType, int * );
void astCheckPerm_( AstFrame *, const int *, const char *, int * );
void astClearDigits_( AstFrame *, int * );
void astClearDirection_( AstFrame *, int, int * );
void astClearDomain_( AstFrame *, int * );
void astClearFormat_( AstFrame *, int, int * );
void astClearLabel_( AstFrame *, int, int * );
void astClearMatchEnd_( AstFrame *, int * );
void astClearMaxAxes_( AstFrame *, int * );
void astClearMinAxes_( AstFrame *, int * );
void astClearPermute_( AstFrame *, int * );
void astClearPreserveAxes_( AstFrame *, int * );
void astClearSymbol_( AstFrame *, int, int * );
void astClearTitle_( AstFrame *, int * );
void astClearUnit_( AstFrame *, int, int * );
void astOverlay_( AstFrame *, const int *, AstFrame *, int * );
void astPrimaryFrame_( AstFrame *, int, AstFrame **, int *, int * );
void astSetAxis_( AstFrame *, int, AstAxis *, int * );
void astSetDigits_( AstFrame *, int, int * );
void astSetDirection_( AstFrame *, int, int, int * );
void astSetDomain_( AstFrame *, const char *, int * );
void astSetFormat_( AstFrame *, int, const char *, int * );
void astSetLabel_( AstFrame *, int, const char *, int * );
void astSetMatchEnd_( AstFrame *, int, int * );
void astSetMaxAxes_( AstFrame *, int, int * );
void astSetMinAxes_( AstFrame *, int, int * );
void astSetPermute_( AstFrame *, int, int * );
void astSetPreserveAxes_( AstFrame *, int, int * );
void astSetSymbol_( AstFrame *, int, const char *, int * );
void astSetTitle_( AstFrame *, const char *, int * );
void astSetUnit_( AstFrame *, int, const char *, int * );
void astValidateAxisSelection_( AstFrame *, int, const int *, const char *, int * );
double astReadDateTime_( const char *, int * );
const char *astFmtDecimalYr_( double, int, int * );
void astLineOffset_( AstFrame *, AstLineDef *, double, double, double[2], int * );
AstPointSet *astFrameGrid_( AstFrame *, int, const double *, const double *, int * );

double astGetTop_( AstFrame *, int, int * );
int astTestTop_( AstFrame *, int, int * );
void astClearTop_( AstFrame *, int, int * );
void astSetTop_( AstFrame *, int, double, int * );

double astGetBottom_( AstFrame *, int, int * );
int astTestBottom_( AstFrame *, int, int * );
void astClearBottom_( AstFrame *, int, int * );
void astSetBottom_( AstFrame *, int, double, int * );

AstSystemType astGetSystem_( AstFrame *, int * );
int astTestSystem_( AstFrame *, int * );
void astClearSystem_( AstFrame *, int * );
void astSetSystem_( AstFrame *, AstSystemType, int * );

AstSystemType astGetAlignSystem_( AstFrame *, int * );
int astTestAlignSystem_( AstFrame *, int * );
void astClearAlignSystem_( AstFrame *, int * );
void astSetAlignSystem_( AstFrame *, AstSystemType, int * );

double astGetEpoch_( AstFrame *, int * );
int astTestEpoch_( AstFrame *, int * );
void astClearEpoch_( AstFrame *, int * );
void astSetEpoch_( AstFrame *, double, int * );

double astGetObsLon_( AstFrame *, int * );
int astTestObsLon_( AstFrame *, int * );
void astClearObsLon_( AstFrame *, int * );
void astSetObsLon_( AstFrame *, double, int * );

double astGetObsLat_( AstFrame *, int * );
int astTestObsLat_( AstFrame *, int * );
void astClearObsLat_( AstFrame *, int * );
void astSetObsLat_( AstFrame *, double, int * );

double astGetObsAlt_( AstFrame *, int * );
int astTestObsAlt_( AstFrame *, int * );
void astClearObsAlt_( AstFrame *, int * );
void astSetObsAlt_( AstFrame *, double, int * );

double astGetDut1_( AstFrame *, int * );
int astTestDut1_( AstFrame *, int * );
void astClearDut1_( AstFrame *, int * );
void astSetDut1_( AstFrame *, double, int * );

int astTestActiveUnit_( AstFrame *, int * );

void astSetFrameFlags_( AstFrame *, int, int * );
int astGetFrameFlags_( AstFrame *, int * );

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
#define astCheckFrame(this) astINVOKE_CHECK(Frame,this,0)
#define astVerifyFrame(this) astINVOKE_CHECK(Frame,this,1)

/* Test class membership. */
#define astIsAFrame(this) astINVOKE_ISA(Frame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astFrame astINVOKE(F,astFrame_)
#else
#define astFrame astINVOKE(F,astFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitFrame(mem,size,init,vtab,name,naxes) \
astINVOKE(O,astInitFrame_(mem,size,init,vtab,name,naxes,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitFrameVtab(vtab,name) astINVOKE(V,astInitFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckFrame to validate Frame pointers before
   use. This provides a contextual error report if a pointer to the
   wrong sort of Object is supplied. */
#define astConvert(from,to,domainlist) \
astINVOKE(O,astConvert_(astCheckFrame(from),astCheckFrame(to),domainlist,STATUS_PTR))
#define astAngle(this,a,b,c) \
astINVOKE(V,astAngle_(astCheckFrame(this),a,b,c,STATUS_PTR))
#define astDistance(this,point1,point2) \
astINVOKE(V,astDistance_(astCheckFrame(this),point1,point2,STATUS_PTR))
#define astFindFrame(target,template,domainlist) \
astINVOKE(O,astFindFrame_(astCheckFrame(target),astCheckFrame(template),domainlist,STATUS_PTR))
#define astMatchAxes(frm1,frm2,axes) \
astINVOKE(V,astMatchAxes_(astCheckFrame(frm1),astCheckFrame(frm2),axes,STATUS_PTR))
#define astNorm(this,value) \
astINVOKE(V,astNorm_(astCheckFrame(this),value,STATUS_PTR))
#define astAxDistance(this,axis,v1,v2) \
astINVOKE(V,astAxDistance_(astCheckFrame(this),axis,v1,v2,STATUS_PTR))
#define astAxOffset(this,axis,v1,dist) \
astINVOKE(V,astAxOffset_(astCheckFrame(this),axis,v1,dist,STATUS_PTR))
#define astOffset(this,point1,point2,offset,point3) \
astINVOKE(V,astOffset_(astCheckFrame(this),point1,point2,offset,point3,STATUS_PTR))
#define astAxAngle(this,a,b,axis) \
astINVOKE(V,astAxAngle_(astCheckFrame(this),a,b,axis,STATUS_PTR))
#define astIntersect(this,a1,a2,b1,b2,cross) \
astINVOKE(V,astIntersect_(astCheckFrame(this),a1,a2,b1,b2,cross,STATUS_PTR))
#define astOffset2(this,point1,angle,offset,point2) \
astINVOKE(V,astOffset2_(astCheckFrame(this),point1,angle,offset,point2,STATUS_PTR))
#define astResolve(this,point1,point2,point3,point4,d1,d2) \
astINVOKE(V,astResolve_(astCheckFrame(this),point1,point2,point3,point4,d1,d2,STATUS_PTR))
#define astGetActiveUnit(this) \
astINVOKE(V,astGetActiveUnit_(astCheckFrame(this),STATUS_PTR))
#define astSetActiveUnit(this,value) \
astINVOKE(V,astSetActiveUnit_(astCheckFrame(this),value,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astNormBox(this,lbnd,ubnd,reg) \
astINVOKE(V,astNormBox_(astCheckFrame(this),lbnd,ubnd,astCheckMapping(reg),STATUS_PTR))
#define astFormat(this,axis,value) \
astINVOKE(V,astFormat_(astCheckFrame(this),axis,value,STATUS_PTR))
#define astPermAxes(this,perm) \
astINVOKE(V,astPermAxes_(astCheckFrame(this),perm,STATUS_PTR))
#define astPickAxes(this,naxes,axes,map) \
astINVOKE(O,astPickAxes_(astCheckFrame(this),naxes,axes,(AstMapping **)(map),STATUS_PTR))
#define astUnformat(this,axis,string,value) \
astINVOKE(V,astUnformat_(astCheckFrame(this),axis,string,value,STATUS_PTR))
#else
#define astFormat(this,axis,value) \
astINVOKE(V,astFormatId_(astCheckFrame(this),axis,value,STATUS_PTR))
#define astPermAxes(this,perm) \
astINVOKE(V,astPermAxesId_(astCheckFrame(this),perm,STATUS_PTR))
#define astPickAxes(this,naxes,axes,map) \
astINVOKE(O,astPickAxesId_(astCheckFrame(this),naxes,axes,(AstMapping **)(map),STATUS_PTR))
#define astUnformat(this,axis,string,value) \
astINVOKE(V,astUnformatId_(astCheckFrame(this),axis,string,value,STATUS_PTR))
#endif

#if defined(astCLASS)            /* Protected */
#define astAxIn(this,axis,lo,hi,val,closed) \
astINVOKE(V,astAxIn_(astCheckFrame(this),axis,lo,hi,val,closed,STATUS_PTR))
#define astAbbrev(this,axis,fmt,str1,str2) \
astINVOKE(V,astAbbrev_(astCheckFrame(this),axis,fmt,str1,str2,STATUS_PTR))
#define astFields(this,axis,fmt,str,maxfld,fields,nc,val) \
astINVOKE(V,astFields_(astCheckFrame(this),axis,fmt,str,maxfld,fields,nc,val,STATUS_PTR))
#define astCheckPerm(this,perm,method) \
astINVOKE(V,astCheckPerm_(astCheckFrame(this),perm,method,STATUS_PTR))
#define astResolvePoints(this,p1,p2,in,out) \
astINVOKE(O,astResolvePoints_(astCheckFrame(this),p1,p2,astCheckPointSet(in),((out)?astCheckPointSet(out):NULL),STATUS_PTR))
#define astLineDef(this,p1,p2) \
astINVOKE(V,astLineDef_(astCheckFrame(this),p1,p2,STATUS_PTR))
#define astLineOffset(this,line,par,prp,point) \
astINVOKE(V,astLineOffset_(astCheckFrame(this),line,par,prp,point,STATUS_PTR))
#define astFrameGrid(this,size,lbnd,ubnd) \
astINVOKE(O,astFrameGrid_(astCheckFrame(this),size,lbnd,ubnd,STATUS_PTR))
#define astLineCrossing(this,l1,l2,cross) \
astINVOKE(V,astLineCrossing_(astCheckFrame(this),l1,l2,cross,STATUS_PTR))
#define astLineContains(this,l,def,point) \
astINVOKE(V,astLineContains_(astCheckFrame(this),l,def,point,STATUS_PTR))
#define astClearDigits(this) \
astINVOKE(V,astClearDigits_(astCheckFrame(this),STATUS_PTR))
#define astClearDirection(this,axis) \
astINVOKE(V,astClearDirection_(astCheckFrame(this),axis,STATUS_PTR))
#define astClearDomain(this) \
astINVOKE(V,astClearDomain_(astCheckFrame(this),STATUS_PTR))
#define astClearFormat(this,axis) \
astINVOKE(V,astClearFormat_(astCheckFrame(this),axis,STATUS_PTR))
#define astClearLabel(this,axis) \
astINVOKE(V,astClearLabel_(astCheckFrame(this),axis,STATUS_PTR))
#define astClearMatchEnd(this) \
astINVOKE(V,astClearMatchEnd_(astCheckFrame(this),STATUS_PTR))
#define astClearMaxAxes(this) \
astINVOKE(V,astClearMaxAxes_(astCheckFrame(this),STATUS_PTR))
#define astClearMinAxes(this) \
astINVOKE(V,astClearMinAxes_(astCheckFrame(this),STATUS_PTR))
#define astClearPermute(this) \
astINVOKE(V,astClearPermute_(astCheckFrame(this),STATUS_PTR))
#define astClearPreserveAxes(this) \
astINVOKE(V,astClearPreserveAxes_(astCheckFrame(this),STATUS_PTR))
#define astClearSymbol(this,axis) \
astINVOKE(V,astClearSymbol_(astCheckFrame(this),axis,STATUS_PTR))
#define astClearTitle(this) \
astINVOKE(V,astClearTitle_(astCheckFrame(this),STATUS_PTR))
#define astClearUnit(this,axis) \
astINVOKE(V,astClearUnit_(astCheckFrame(this),axis,STATUS_PTR))
#define astConvertX(to,from,domainlist) \
astINVOKE(O,astConvertX_(astCheckFrame(to),astCheckFrame(from),domainlist,STATUS_PTR))
#define astGap(this,axis,gap,ntick) \
astINVOKE(V,astGap_(astCheckFrame(this),axis,gap,ntick,STATUS_PTR))
#define astGetAxis(this,axis) \
astINVOKE(O,astGetAxis_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetDigits(this) \
astINVOKE(V,astGetDigits_(astCheckFrame(this),STATUS_PTR))
#define astGetDirection(this,axis) \
astINVOKE(V,astGetDirection_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetDomain(this) \
astINVOKE(V,astGetDomain_(astCheckFrame(this),STATUS_PTR))
#define astGetFormat(this,axis) \
astINVOKE(V,astGetFormat_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetLabel(this,axis) \
astINVOKE(V,astGetLabel_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetMatchEnd(this) \
astINVOKE(V,astGetMatchEnd_(astCheckFrame(this),STATUS_PTR))
#define astGetMaxAxes(this) \
astINVOKE(V,astGetMaxAxes_(astCheckFrame(this),STATUS_PTR))
#define astGetMinAxes(this) \
astINVOKE(V,astGetMinAxes_(astCheckFrame(this),STATUS_PTR))
#define astGetNaxes(this) \
astINVOKE(V,astGetNaxes_(astCheckFrame(this),STATUS_PTR))
#define astGetPerm(this) \
astINVOKE(V,astGetPerm_(astCheckFrame(this),STATUS_PTR))
#define astGetPermute(this) \
astINVOKE(V,astGetPermute_(astCheckFrame(this),STATUS_PTR))
#define astGetPreserveAxes(this) \
astINVOKE(V,astGetPreserveAxes_(astCheckFrame(this),STATUS_PTR))
#define astGetSymbol(this,axis) \
astINVOKE(V,astGetSymbol_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetTitle(this) \
astINVOKE(V,astGetTitle_(astCheckFrame(this),STATUS_PTR))
#define astGetUnit(this,axis) \
astINVOKE(V,astGetUnit_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetNormUnit(this,axis) \
astINVOKE(V,astGetNormUnit_(astCheckFrame(this),axis,STATUS_PTR))
#define astMatch(template,target,matchsub,template_axes,target_axes,map,result) \
astINVOKE(V,astMatch_(astCheckFrame(template),astCheckFrame(target),matchsub,template_axes,target_axes,(AstMapping **)(map),(AstFrame **)(result),STATUS_PTR))
#define astIsUnitFrame(this) \
astINVOKE(V,astIsUnitFrame_(astCheckFrame(this),STATUS_PTR))
#define astOverlay(template,template_axes,result) \
astINVOKE(V,astOverlay_(astCheckFrame(template),template_axes,astCheckFrame(result),STATUS_PTR))
#define astPrimaryFrame(this,axis1,frame,axis2) \
astINVOKE(V,astPrimaryFrame_(astCheckFrame(this),axis1,(AstFrame **)(frame),axis2,STATUS_PTR))
#define astSetAxis(this,axis,newaxis) \
astINVOKE(V,astSetAxis_(astCheckFrame(this),axis,astCheckAxis(newaxis),STATUS_PTR))
#define astSetDigits(this,digits) \
astINVOKE(V,astSetDigits_(astCheckFrame(this),digits,STATUS_PTR))
#define astSetDirection(this,axis,direction) \
astINVOKE(V,astSetDirection_(astCheckFrame(this),axis,direction,STATUS_PTR))
#define astSetDomain(this,domain) \
astINVOKE(V,astSetDomain_(astCheckFrame(this),domain,STATUS_PTR))
#define astSetFormat(this,axis,format) \
astINVOKE(V,astSetFormat_(astCheckFrame(this),axis,format,STATUS_PTR))
#define astSetLabel(this,axis,label) \
astINVOKE(V,astSetLabel_(astCheckFrame(this),axis,label,STATUS_PTR))
#define astSetMatchEnd(this,value) \
astINVOKE(V,astSetMatchEnd_(astCheckFrame(this),value,STATUS_PTR))
#define astSetMaxAxes(this,value) \
astINVOKE(V,astSetMaxAxes_(astCheckFrame(this),value,STATUS_PTR))
#define astSetMinAxes(this,value) \
astINVOKE(V,astSetMinAxes_(astCheckFrame(this),value,STATUS_PTR))
#define astSetPermute(this,value) \
astINVOKE(V,astSetPermute_(astCheckFrame(this),value,STATUS_PTR))
#define astSetPreserveAxes(this,value) \
astINVOKE(V,astSetPreserveAxes_(astCheckFrame(this),value,STATUS_PTR))
#define astSetSymbol(this,axis,symbol) \
astINVOKE(V,astSetSymbol_(astCheckFrame(this),axis,symbol,STATUS_PTR))
#define astSetTitle(this,title) \
astINVOKE(V,astSetTitle_(astCheckFrame(this),title,STATUS_PTR))
#define astSetUnit(this,axis,unit) \
astINVOKE(V,astSetUnit_(astCheckFrame(this),axis,unit,STATUS_PTR))
#define astSubFrame(target,template,result_naxes,target_axes,template_axes,map,result) \
astINVOKE(V,astSubFrame_(astCheckFrame(target),template?astCheckFrame(template):NULL,result_naxes,target_axes,template_axes,(AstMapping **)(map),(AstFrame **)(result),STATUS_PTR))
#define astTestDigits(this) \
astINVOKE(V,astTestDigits_(astCheckFrame(this),STATUS_PTR))
#define astTestDirection(this,axis) \
astINVOKE(V,astTestDirection_(astCheckFrame(this),axis,STATUS_PTR))
#define astTestDomain(this) \
astINVOKE(V,astTestDomain_(astCheckFrame(this),STATUS_PTR))
#define astTestFormat(this,axis) \
astINVOKE(V,astTestFormat_(astCheckFrame(this),axis,STATUS_PTR))
#define astTestLabel(this,axis) \
astINVOKE(V,astTestLabel_(astCheckFrame(this),axis,STATUS_PTR))
#define astTestMatchEnd(this) \
astINVOKE(V,astTestMatchEnd_(astCheckFrame(this),STATUS_PTR))
#define astTestMaxAxes(this) \
astINVOKE(V,astTestMaxAxes_(astCheckFrame(this),STATUS_PTR))
#define astTestMinAxes(this) \
astINVOKE(V,astTestMinAxes_(astCheckFrame(this),STATUS_PTR))
#define astTestPermute(this) \
astINVOKE(V,astTestPermute_(astCheckFrame(this),STATUS_PTR))
#define astTestPreserveAxes(this) \
astINVOKE(V,astTestPreserveAxes_(astCheckFrame(this),STATUS_PTR))
#define astTestSymbol(this,axis) \
astINVOKE(V,astTestSymbol_(astCheckFrame(this),axis,STATUS_PTR))
#define astTestTitle(this) \
astINVOKE(V,astTestTitle_(astCheckFrame(this),STATUS_PTR))
#define astTestUnit(this,axis) \
astINVOKE(V,astTestUnit_(astCheckFrame(this),axis,STATUS_PTR))
#define astValidateAxis(this,axis,fwd,method) \
astINVOKE(V,astValidateAxis_(astCheckFrame(this),axis,fwd,method,STATUS_PTR))
#define astValidateAxisSelection(this,naxes,axes,method) \
astINVOKE(V,astValidateAxisSelection_(astCheckFrame(this),naxes,axes,method,STATUS_PTR))

#define astMatchAxesX(frm2,frm1,axes) \
astINVOKE(V,astMatchAxesX_(astCheckFrame(frm2),astCheckFrame(frm1),axes,STATUS_PTR))

#define astFmtDecimalYr(year,digits) astFmtDecimalYr_(year,digits,STATUS_PTR)
#define astReadDateTime(value) astReadDateTime_(value,STATUS_PTR)

#define astValidateSystem(this,system,method) \
astINVOKE(V,astValidateSystem_(astCheckFrame(this),system,method,STATUS_PTR))
#define astSystemString(this,system) \
astINVOKE(V,astSystemString_(astCheckFrame(this),system,STATUS_PTR))
#define astSystemCode(this,system) \
astINVOKE(V,astSystemCode_(astCheckFrame(this),system,STATUS_PTR))

#define astClearTop(this,axis) \
astINVOKE(V,astClearTop_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetTop(this,axis) \
astINVOKE(V,astGetTop_(astCheckFrame(this),axis,STATUS_PTR))
#define astSetTop(this,axis,value) \
astINVOKE(V,astSetTop_(astCheckFrame(this),axis,value,STATUS_PTR))
#define astTestTop(this,axis) \
astINVOKE(V,astTestTop_(astCheckFrame(this),axis,STATUS_PTR))

#define astClearBottom(this,axis) \
astINVOKE(V,astClearBottom_(astCheckFrame(this),axis,STATUS_PTR))
#define astGetBottom(this,axis) \
astINVOKE(V,astGetBottom_(astCheckFrame(this),axis,STATUS_PTR))
#define astSetBottom(this,axis,value) \
astINVOKE(V,astSetBottom_(astCheckFrame(this),axis,value,STATUS_PTR))
#define astTestBottom(this,axis) \
astINVOKE(V,astTestBottom_(astCheckFrame(this),axis,STATUS_PTR))

#define astClearSystem(this) \
astINVOKE(V,astClearSystem_(astCheckFrame(this),STATUS_PTR))
#define astGetSystem(this) \
astINVOKE(V,astGetSystem_(astCheckFrame(this),STATUS_PTR))
#define astSetSystem(this,value) \
astINVOKE(V,astSetSystem_(astCheckFrame(this),value,STATUS_PTR))
#define astTestSystem(this) \
astINVOKE(V,astTestSystem_(astCheckFrame(this),STATUS_PTR))

#define astClearAlignSystem(this) \
astINVOKE(V,astClearAlignSystem_(astCheckFrame(this),STATUS_PTR))
#define astGetAlignSystem(this) \
astINVOKE(V,astGetAlignSystem_(astCheckFrame(this),STATUS_PTR))
#define astSetAlignSystem(this,value) \
astINVOKE(V,astSetAlignSystem_(astCheckFrame(this),value,STATUS_PTR))
#define astTestAlignSystem(this) \
astINVOKE(V,astTestAlignSystem_(astCheckFrame(this),STATUS_PTR))

#define astClearEpoch(this) \
astINVOKE(V,astClearEpoch_(astCheckFrame(this),STATUS_PTR))
#define astGetEpoch(this) \
astINVOKE(V,astGetEpoch_(astCheckFrame(this),STATUS_PTR))
#define astSetEpoch(this,value) \
astINVOKE(V,astSetEpoch_(astCheckFrame(this),value,STATUS_PTR))
#define astTestEpoch(this) \
astINVOKE(V,astTestEpoch_(astCheckFrame(this),STATUS_PTR))

#define astGetObsLon(this) \
astINVOKE(V,astGetObsLon_(astCheckFrame(this),STATUS_PTR))
#define astTestObsLon(this) \
astINVOKE(V,astTestObsLon_(astCheckFrame(this),STATUS_PTR))
#define astClearObsLon(this) \
astINVOKE(V,astClearObsLon_(astCheckFrame(this),STATUS_PTR))
#define astSetObsLon(this,value) \
astINVOKE(V,astSetObsLon_(astCheckFrame(this),value,STATUS_PTR))

#define astGetObsLat(this) \
astINVOKE(V,astGetObsLat_(astCheckFrame(this),STATUS_PTR))
#define astTestObsLat(this) \
astINVOKE(V,astTestObsLat_(astCheckFrame(this),STATUS_PTR))
#define astClearObsLat(this) \
astINVOKE(V,astClearObsLat_(astCheckFrame(this),STATUS_PTR))
#define astSetObsLat(this,value) \
astINVOKE(V,astSetObsLat_(astCheckFrame(this),value,STATUS_PTR))

#define astGetObsAlt(this) \
astINVOKE(V,astGetObsAlt_(astCheckFrame(this),STATUS_PTR))
#define astTestObsAlt(this) \
astINVOKE(V,astTestObsAlt_(astCheckFrame(this),STATUS_PTR))
#define astClearObsAlt(this) \
astINVOKE(V,astClearObsAlt_(astCheckFrame(this),STATUS_PTR))
#define astSetObsAlt(this,value) \
astINVOKE(V,astSetObsAlt_(astCheckFrame(this),value,STATUS_PTR))

#define astClearDut1(this) \
astINVOKE(V,astClearDut1_(astCheckFrame(this),STATUS_PTR))
#define astGetDut1(this) \
astINVOKE(V,astGetDut1_(astCheckFrame(this),STATUS_PTR))
#define astSetDut1(this,value) \
astINVOKE(V,astSetDut1_(astCheckFrame(this),value,STATUS_PTR))
#define astTestDut1(this) \
astINVOKE(V,astTestDut1_(astCheckFrame(this),STATUS_PTR))

#define astTestActiveUnit(this) \
astINVOKE(V,astTestActiveUnit_(astCheckFrame(this),STATUS_PTR))

#define astSetFrameFlags(this,flags) \
astINVOKE(V,astSetFrameFlags_(astCheckFrame(this),flags,STATUS_PTR))
#define astGetFrameFlags(this) \
astINVOKE(V,astGetFrameFlags_(astCheckFrame(this),STATUS_PTR))

#endif
#endif





