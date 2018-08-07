/*
*+
*  Name:
*     cgeneric.h

*  Purpose:
*     Include file for standardising generic C code.

*  Language:
*     C

*  Type of Module:
*     C include file

*  Description:
*     This include files defines macros that should be used when generating
*     generic code in C. To use it you need to follow the recipe described
*     here:
*
*        - Create your generic code routines in an include file with extension
*          ".cgen". There are several macros defined for use when defining
*          generic functions.
*
*          -- CGEN_BAD
*          -- CGEN_MAX
*          -- CGEN_MIN
*          -- CGEN_EPS
*          -- CGEN_FUNCTION
*          -- CGEN_FUNCTION2
*          -- CGEN_HDS_TYPE
*          -- CGEN_TYPE
*          -- CGEN_BIG_TYPE
*
*          You need to use CGEN_FUNCTION as part of the normal function
*          declaration so that generic forms of the function name can be
*          used. Each routine in a generic file should start:
*
*             return_type CGEN_FUNCTION(function_name) ( arg decs )
*
*          So a routine called kpg1_bad that returned an int and accepted a
*          CGEN_CTYPE pointer, called value, would be defined as:
*
*             int CGEN_FUNCTION(kpg1_bad) (CGEN_CTYPE *value) {
*                 if ( value[0] == CGEN_BAD ) {
*                    return 1;
*                 }
*                 return 0;
*             }
*
*          with the trivial job of testing the first element of an array
*          against the BAD value contant.
*
*          The CGEN_FUNCTION2 is like CGEN_FUNCTION except that it
*          appends a specified suffix to the end of the expanded function
*          name (i.e. after the type code).
*
*          The value of CGEN_TYPE will be set to the C type, that is
*          double, float, int, short int, unsigned short int, char,
*          unsigned char, and int64_t as appropriate.
*
*          The value of CGEN_BAD will be set to one of the PRM constants
*          VAL__BADD, VAL__BADR etc. as appropriate. Likewise, CGEN_MAX,
*          CGEN_MIN and CGEN_EPS will be set to the corresponding PRM constants.
*
*          The value of CGEN_HDS_TYPE will be set to the HDS type of the
*          current type, one of "_DOUBLE", "_REAL", "_INTEGER", "_WORD"
*          "_UWORD", "_BYTE", "_UBYTE" or "_INT64".
*
*          CGEN_BIG_TYPE is a type that is fundamentally the same type
*          as CGEN_TYPE but can have more precision. For example, int vs
*          long, or float vs double. Can be used when you need to accumulate
*          values of type CGEN_TYPE.
*
*        - Create a C file that includes the generic code, once for each of
*          the data types you want (this file can also contain related
*          non-generic code). To do this define the macro CGEN_CODE_TYPE to be
*          one of the values:
*
*             CGEN_DOUBLE_TYPE, CGEN_FLOAT_TYPE, CGEN_INT_TYPE,
*             CGEN_WORD_TYPE, CGEN_UWORD_TYPE, CGEN_BYTE_TYPE
*             CGEN_UBYTE_TYPE, CGEN_INT64_TYPE
*
*          to select the data type required. Then include the file
*          "cgeneric_defs.h" followed by your generic include file,
*          (called "mygenerics.cgen" in the following example):
*
*             #include <prm_par.h>
*             #include <cgeneric.h>
*
*             #define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE
*             #include "cgeneric_defs.h"
*             #include "mygenerics.cgen"
*             #undef CGEN_CODE_TYPE
*
*             #define CGEN_CODE_TYPE CGEN_INT_TYPE
*             #include "cgeneric_defs.h"
*             #include "mygenerics.cgen"
*             #undef CGEN_CODE_TYPE
*
*          Which generates code for double and int routines when
*          the C file is compiled.
*
*        - Use the generic routines. The function names will
*          have the character codes "D", "F", "I", "W", "UW",
*          "B", "UB", and "K" appended to their declared names. So
*          for our example "kpg1_bad", we have the two functions
*          "kpg1_badD" and "kpg1_badI".

*  Notes:
*     Should look at how to declare prototypes.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)
*     DSB: David S. Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-OCT-2005 (PWD):
*        Original version.
*     31-AUG-2006 (DSB):
*        Add CGEN_MAX and CGEN_MIN.
*     2012-04-23 (TIMJ):
*        Add INT64 support.
*     7-JUN-2018 (DSB):
*        Add CGEN_EPS
*     7-JUN-2018 (DSB):
*        Add CGEN_FUNCTION2
*     {enter_further_changes_here}

*-
------------------------------------------------------------------------------
*/
#ifndef _CGENERIC_H_
#define _CGENERIC_H_

/* Macros to join two strings in a defered sense. Need to defer as
 * we don't want the strings to be evaluated until after the file is
 * included, and the necessary macros are defined. */
#define DEFER_CGEN_JOIN_STRINGS(string1,string2) string1 ## string2
#define CGEN_JOIN_STRINGS(string1,string2) \
           DEFER_CGEN_JOIN_STRINGS(string1,string2)

/* Macro to define the generic function name based on the current
 * CGEN_CODE value. */
#define CGEN_FUNCTION(name) CGEN_JOIN_STRINGS(name,CGEN_CODE)

/* The CGEN_FUNCTION2 macro is like CGEN_FUNCTION, but adds a specified
   suffix to the end of the expanded function name (i.e. after the type
   code). */
#define DEFER_CGEN_JOIN_STRINGS2(string1,string2,suffix) string1 ## string2 ## suffix
#define CGEN_JOIN_STRINGS2(string1,string2,suffix) \
           DEFER_CGEN_JOIN_STRINGS2(string1,string2,suffix)
#define CGEN_FUNCTION2(name,suffix) CGEN_JOIN_STRINGS2(name,CGEN_CODE,suffix)

/* The CGEN_BAD value for the current data type, which is the PRM
 * type specified by CGEN_PRM_TYPE (VAL__BADD, VAL__BADR etc.). */
#define CGEN_BAD CGEN_JOIN_STRINGS(VAL__BAD,CGEN_PRM_TYPE)

/* The CGEN_MAX value for the current data type, which is the PRM
 * type specified by CGEN_PRM_TYPE (VAL__MAXD, VAL__MAXR etc.). */
#define CGEN_MAX CGEN_JOIN_STRINGS(VAL__MAX,CGEN_PRM_TYPE)

/* The CGEN_MIN value for the current data type, which is the PRM
 * type specified by CGEN_PRM_TYPE (VAL__MIND, VAL__MINR etc.). */
#define CGEN_MIN CGEN_JOIN_STRINGS(VAL__MIN,CGEN_PRM_TYPE)

/* The CGEN_EPS value for the current data type, which is the PRM
 * type specified by CGEN_PRM_TYPE (VAL__EPSD, VAL__EPSR etc.). */
#define CGEN_EPS CGEN_JOIN_STRINGS(VAL__EPS,CGEN_PRM_TYPE)

/* Enumeration of the known types, define CGEN_CODE_TYPE to one of these and
 * include cgeneric_defs.h, note these have to be defines, not enums, for
 * the preprocessor define checks to work. */
#define CGEN_DOUBLE_TYPE 1
#define CGEN_FLOAT_TYPE 2
#define CGEN_INT_TYPE 3
#define CGEN_WORD_TYPE 4
#define CGEN_UWORD_TYPE 5
#define CGEN_BYTE_TYPE 6
#define CGEN_UBYTE_TYPE 7
#define CGEN_INT64_TYPE 8

#endif /* _CGENERIC_H_ */
