/*
*+
*  Name:
*     cgeneric_defs.h

*  Purpose:
*     Additional include file for standardising generic C code.

*  Language:
*     C

*  Type of Module:
*     C include file

*  Description:
*     See the description in "cgeneric.h". This is the file 
*     "cgenerics_defs.h". This include file defines the macros:
* 
*        CGEN_TYPE
*        CGEN_CODE
*        CGEN_PRM_TYPE
*
*     To match the type declared by the current value of CGEN_CODE_TYPE.
*     See "cgeneric.h" for the possible values.

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

/* This is intended to be included multiple times */

/* Unset previous defines */
#ifdef CGEN_TYPE
#undef CGEN_TYPE
#endif
#ifdef CGEN_CODE
#undef CGEN_CODE
#endif
#ifdef CGEN_PRM_TYPE
#undef CGEN_PRM_TYPE
#endif

/* double */
#if CGEN_CODE_TYPE == CGEN_DOUBLE_TYPE

#define CGEN_TYPE double
#define CGEN_CODE D
#define CGEN_PRM_TYPE D

/* float */
#elif CGEN_CODE_TYPE == CGEN_FLOAT_TYPE

#define CGEN_TYPE float
#define CGEN_CODE F
#define CGEN_PRM_TYPE R

/* int */
#elif CGEN_CODE_TYPE == CGEN_INT_TYPE

#define CGEN_TYPE int
#define CGEN_CODE I
#define CGEN_PRM_TYPE I

/* short int */
#elif CGEN_CODE_TYPE == CGEN_WORD_TYPE

#define CGEN_TYPE short int
#define CGEN_CODE W
#define CGEN_PRM_TYPE W

/* unsigned short int */
#elif CGEN_CODE_TYPE == CGEN_UWORD_TYPE

#define CGEN_TYPE unsigned short int
#define CGEN_CODE UW
#define CGEN_PRM_TYPE UW

/* signed char */
#elif CGEN_CODE_TYPE == CGEN_BYTE_TYPE

#define CGEN_TYPE char
#define CGEN_CODE B
#define CGEN_PRM_TYPE B

/* unsigned char */
#elif CGEN_CODE_TYPE == CGEN_UBYTE_TYPE

#define CGEN_TYPE unsigned char
#define CGEN_CODE UB
#define CGEN_PRM_TYPE UB

#endif
