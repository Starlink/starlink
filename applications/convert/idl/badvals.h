/*
*+

*  Name:
*     badvals.h

*  Type of Module:
*     C header file

*  Purpose:
*     Define NDF bad values for IDL/NDF converters

*  Description:
*     This file defines the bad values used in NDF files.

*  Authors:
*     AJC: A J Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-APR-1999 (AJC):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef BADVALS_DEFINED
#define BADVALS_DEFINED

#include <float.h>
#include <limits.h>

/*  Bad value, used for flagging undefined data. */
/*  =========================================== */

#define VAL__BADUB UCHAR_MAX

#define VAL__BADB CHAR_MIN

#define VAL__BADUW USHRT_MAX

#define VAL__BADW SHRT_MIN

#define VAL__BADI INT_MIN

#define VAL__BADR -FLT_MAX

#define VAL__BADD -DBL_MAX


/*. */
#endif  /* BADVALS_DEFINED */
