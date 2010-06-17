/*
*+
*  Name:
*     smf_qual_str

*  Purpose:
*     Return a string name for a given quality bit

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char *smf_qual_str( int bit, int *status );

*  Arguments:
*     bit = int (Given)
*        Bit number in the range 0 to SMF__NQBITS-1
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to data type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string name corresponding to the given quality
*     Bit.  Returns NULL pointer if outside the allowable range.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-03-18 (EC):
*        Initial version, copied from smf_dtype_str.c
*     2010-03-19 (EC):
*        Rename SMF__Q_BADS to SMF__Q_BADDA, and added SMF__Q_COM

*  Notes:
*     Make sure these strings match descriptions in smf_create_qualname

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_qual_str"

const char *smf_qual_str( int bit, int *status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* now switch on bit mask */
  switch( BIT_TO_VAL(bit) ) {
  case SMF__Q_BADDA:
    retval = "BADDA";
    break;

  case SMF__Q_BADB:
    retval = "BADBOL";
    break;

  case SMF__Q_SPIKE:
    retval = "SPIKE";
    break;

  case SMF__Q_JUMP:
    retval = "DCJUMP";
    break;

  case SMF__Q_PAD:
    retval = "PAD";
    break;

  case SMF__Q_APOD:
    retval = "APOD";
    break;

  case SMF__Q_STAT:
    retval = "STAT";
    break;

  case SMF__Q_COM:
    retval = "COM";
    break;

  default:
    retval = NULL;
  }

  return retval;
}
