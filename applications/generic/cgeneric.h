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
*          ".cgen". There are three macros defined for use when defining
*          generic functions.
*
*          -- CGEN_FUNCTION
*          -- CGEN_TYPE
*          -- CGEN_BAD
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
*          The value of CGEN_TYPE will be set to the C type, that is
*          double, float, int, short int, unsigned short int, char
*          and unsigned char, as appropriate.
*
*          The value of CGEN_BAD will be set to one of the PRM constants
*          VAL__BADD, VAL__BADR etc. as appropriate.
*
*        - Create a C file that includes the generic code, once for each of
*          the data types you want (this file can also contain related
*          non-generic code). To do this define the macro CGEN_CODE_TYPE to be
*          one of the values:
*
*             CGEN_DOUBLE_TYPE, CGEN_FLOAT_TYPE, CGEN_INT_TYPE,
*             CGEN_WORD_TYPE, CGEN_UWORD_TYPE, CGEN_BYTE_TYPE
*             CGEN_UBYTE_TYPE
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
*          Which generates code for double and int routines.
*
*        - Use the generic routines. The function names will
*          have the character codes "D", "F", "I", "W", "UW",
*          "B" and "UB" appended to their declared names. So
*          for our example "kpg1_bad", we have the two functions
*          "kpg1_badD" and "kpg1_badI".

*  Notes:
*     Should look at how to declare prototypes.

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     10-OCT-2005 (PWD):
*        Original version.
*     {enter_further_changes_here}
*

*  Bugs:
*     {note_any_bugs_here}

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

/* The CGEN_BAD value for the current data type, which is the PRM
 * type specified by CGEN_PRM_TYPE (VAL__BADD, VAL__BADR etc.). */
#define CGEN_BAD CGEN_JOIN_STRINGS(VAL__BAD,CGEN_PRM_TYPE)

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

#endif /* _CGENERIC_H_ */
