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
*     const char *smf_qual_str( int usebit, int bit_or_val,
*                  const char **descr, int *status );

*  Arguments:
*     usebit = int (Given)
*        If true "bit is used to determine the relevant quality. Else
*        "val" is used.
*     bit_or_val = int (Given)
*        If "usebit" is true this is a bit number in the range 0 to SMF__NQBITS-1.
*        Else it is the actual quality value to be tested.
*     descr = const char ** (Given)
*        Pointer to const string buffer describing the quality. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to data type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string name corresponding to the given quality
*     Bit or quality value. It can also be used to obtain a string description
*     of what the quality represents. Returns NULL pointer if outside
*     the allowable range.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-18 (EC):
*        Initial version, copied from smf_dtype_str.c
*     2010-03-19 (EC):
*        Rename SMF__Q_BADS to SMF__Q_BADDA, and added SMF__Q_COM
*     2010-06-17 (TIMJ):
*        Change API to allow the description to be in the same place
*        as the string identifier. Also allow a value to be given directly
*        as well as an alternative to the bit number.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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

const char *smf_qual_str( int usebit, int bit_or_val,
                          const char **descr, int *status ) {

  /* Set a default value */
  const char * retval = NULL;
  const char * ldescr = "No description available";
  int qval = 0;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* Choose how we have been given a value */
  if (usebit) {
    qval = BIT_TO_VAL(bit_or_val);
  } else {
    qval = bit_or_val;
  }

  /* now switch on bit mask */
  switch( qval ) {
  case SMF__Q_BADDA:
    retval = "BADDA";
    ldescr = "Set iff a sample is flagged by the DA";
    break;

  case SMF__Q_BADB:
    retval = "BADBOL";
    ldescr = "Set iff all data from bolo to be ignored";
    break;

  case SMF__Q_SPIKE:
    retval = "SPIKE";
    ldescr = "Set iff a spike is detected";
    break;

  case SMF__Q_JUMP:
    retval = "DCJUMP";
    ldescr = "Set iff a DC jump is present";
    break;

  case SMF__Q_PAD:
    retval = "PAD";
    ldescr = "Set iff data are padding";
    break;

  case SMF__Q_APOD:
    retval = "APOD";
    ldescr = "Set iff data are apodized/boundary";
    break;

  case SMF__Q_STAT:
    retval = "STAT";
    ldescr = "Set iff telescope was stationary";
    break;

  case SMF__Q_COM:
    retval = "COM";
    ldescr = "Set iff data common-mode rejected";
    break;

  default:
    retval = NULL;
  }

  if (descr) *descr = ldescr;

  return retval;
}
