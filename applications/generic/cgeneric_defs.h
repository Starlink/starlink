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
*        CGEN_BIG_TYPE
*        CGEN_HDS_TYPE
*        CGEN_CODE
*        CGEN_PRM_TYPE
*
*     To match the type declared by the current value of CGEN_CODE_TYPE.
*     See "cgeneric.h" for the possible values.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     25-OCT-2005 (DSB):
*        Added CGEN_HDS_TYPE.
*     12-DEC-2008 (TIMJ):
*        Added CGEN_BIG_TYPE
*     2012-04-23 (TIMJ):
*        Add INT64
*     {enter_further_changes_here}

*-
------------------------------------------------------------------------------
*/

/* This is intended to be included multiple times */

/* Unset previous defines */
#ifdef CGEN_TYPE
#undef CGEN_TYPE
#endif
#ifdef CGEN_BIG_TYPE
#undef CGEN_BIG_TYPE
#endif
#ifdef CGEN_CODE
#undef CGEN_CODE
#endif
#ifdef CGEN_PRM_TYPE
#undef CGEN_PRM_TYPE
#endif
#ifdef CGEN_HDS_TYPE
#undef CGEN_HDS_TYPE
#endif

/* double */
#if CGEN_CODE_TYPE == CGEN_DOUBLE_TYPE

#define CGEN_TYPE double
#define CGEN_BIG_TYPE double
#define CGEN_CODE D
#define CGEN_PRM_TYPE D
#define CGEN_HDS_TYPE "_DOUBLE"

/* float */
#elif CGEN_CODE_TYPE == CGEN_FLOAT_TYPE

#define CGEN_TYPE float
#define CGEN_BIG_TYPE double
#define CGEN_CODE F
#define CGEN_PRM_TYPE R
#define CGEN_HDS_TYPE "_REAL"

/* int */
#elif CGEN_CODE_TYPE == CGEN_INT_TYPE

#define CGEN_TYPE int
#define CGEN_BIG_TYPE long
#define CGEN_CODE I
#define CGEN_PRM_TYPE I
#define CGEN_HDS_TYPE "_INTEGER"

#elif CGEN_CODE_TYPE == CGEN_INT64_TYPE

#include <inttypes.h>
#define CGEN_TYPE int64_t
#define CGEN_BIG_TYPE int64_t
#define CGEN_CODE K
#define CGEN_PRM_TYPE K
#define CGEN_HDS_TYPE "_INT64"

/* short int */
#elif CGEN_CODE_TYPE == CGEN_WORD_TYPE

#define CGEN_TYPE short int
#define CGEN_BIG_TYPE int
#define CGEN_CODE W
#define CGEN_PRM_TYPE W
#define CGEN_HDS_TYPE "_WORD"

/* unsigned short int */
#elif CGEN_CODE_TYPE == CGEN_UWORD_TYPE

#define CGEN_TYPE unsigned short int
#define CGEN_BIG_TYPE unsigned int
#define CGEN_CODE UW
#define CGEN_PRM_TYPE UW
#define CGEN_HDS_TYPE "_UWORD"

/* signed char */
#elif CGEN_CODE_TYPE == CGEN_BYTE_TYPE

#define CGEN_TYPE char
#define CGEN_BIG_TYPE short int
#define CGEN_CODE B
#define CGEN_PRM_TYPE B
#define CGEN_HDS_TYPE "_BYTE"

/* unsigned char */
#elif CGEN_CODE_TYPE == CGEN_UBYTE_TYPE

#define CGEN_TYPE unsigned char
#define CGEN_BIG_TYPE unsigned short int
#define CGEN_CODE UB
#define CGEN_PRM_TYPE UB
#define CGEN_HDS_TYPE "_UBYTE"

#endif
