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
*     const char *smf_qual_str( smf_qfam_t family, int usebit, int bit_or_val,
*                  const char **descr, int *status );

*  Arguments:
*     family = smf_qfam_t (Given)
*        Indicate which family of quality bits is represented in "qual".
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
*
*     The values returned depend on the quality family being requested. For
*     example we could be interested in time series quality or map quality.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Must keep in sync with smf_qual_str_to_val and smf_typ.h, and ensure that
*     the string representations do not exceed SMF_QSTR_MAX characters.

*  History:
*     2010-03-18 (EC):
*        Initial version, copied from smf_dtype_str.c
*     2010-03-19 (EC):
*        Rename SMF__Q_BADS to SMF__Q_BADDA, and added SMF__Q_COM
*     2010-06-17 (TIMJ):
*        Change API to allow the description to be in the same place
*        as the string identifier. Also allow a value to be given directly
*        as well as an alternative to the bit number.
*     2010-06-17 (TIMJ):
*        Add quality family.
*     2010-07-06 (TIMJ):
*        Add SMF__Q_NOISE
*     2011-04-15 (TIMJ):
*        Add SMF__Q_EXT
*     2011-04-26 (DSB):(TIMJ):
*        Add SMF__Q_LOWAP
*     2011-09-19 (DSB):
*        Add SMF__Q_BADEF
*     2015-010-12 (DSB):
*        Define SMF__Q_FILT and SMF__Q_BADEF to be the same value
*        (SMF__Q_GENERIC) in order to keep total number of bits below 17).
*     2020-06-08 (DSB):
*        Correct comments stored with FLT, COM and PCA mask quality bit
*        definitions.

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

const char *smf_qual_str( smf_qfam_t family, int usebit, int bit_or_val,
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

  if (family == SMF__QFAM_TSERIES) {
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

    case SMF__Q_GENERIC:
      retval = "GENERIC";
      ldescr = "Set if sample has some generic problem";
      break;

    case SMF__Q_NOISE:
      retval = "NOISE";
      ldescr = "Set if bolometer was too noisy";
      break;

    case SMF__Q_EXT:
      retval = "EXT";
      ldescr = "Set if extinction correction failed";
      break;

    case SMF__Q_LOWAP:
      retval = "LOWAP";
      ldescr = "Set iff apodisation factor is too low to invert";
      break;

    case SMF__Q_RING:
      retval = "RING";
      ldescr = "Set iff samples suffer from filter ringing";
      break;

    case SMF__Q_SSN:
      retval = "SSN";
      ldescr = "Set iff samples flag as bad by the SSN model";
      break;

    case SMF__Q_PCA:
      retval = "PCA";
      ldescr = "Set iff samples flag as bad by the PCA model";
      break;

    case SMF__Q_IP:
      retval = "IP";
      ldescr = "Set iff samples flag as bad by the IP model";
      break;

    default:
      retval = NULL;
    }
  } else if (family == SMF__QFAM_MAP) {
    /* now switch on bit mask */
    /*          012345678901234567890123456789012345678901234567890 << 50 characters */
    switch( qval ) {
    case SMF__MAPQ_AST:
      retval = "AST";
      ldescr = "Set iff AST model is zeroed at the output pixel";
      break;

    case SMF__MAPQ_FLT:
      retval = "FLT";
      ldescr = "Set iff FLT model is not blanked at the output pixel";
      break;

    case SMF__MAPQ_COM:
      retval = "COM";
      ldescr = "Set iff COM model is not blanked at the output pixel";
      break;

    case SMF__MAPQ_PCA:
      retval = "PCA";
      ldescr = "Set iff PCA model is not blanked at the output pixel";
      break;

    default:
      retval = NULL;
    }
  } else if (family == SMF__QFAM_TCOMP) {
    /* now switch on bit mask */
    /*          012345678901234567890123456789012345678901234567890 << 50 characters */
    switch( qval ) {
    case SMF__TCOMPQ_BAD:
      retval = "BADBOLO";
      ldescr = "Set if bolometer data were inherently bad";
      break;
    case SMF__TCOMPQ_ENDS:
      retval = "ENDS";
      ldescr = "Set if the ends of the time series are bad";
      break;
    case SMF__TCOMPQ_BLIP:
      retval = "BLIP";
      ldescr = "Set if bolometer went bad for a moment";
      break;
    case SMF__TCOMPQ_MATCH:
      retval = "MATCH";
      ldescr = "Set if time series did not look like others";
      break;
    case SMF__TCOMPQ_TEL:
      retval = "TEL";
      ldescr = "Set if telescope was not in useful state";
      break;
    case SMF__TCOMPQ_RING:
      retval = "RINGING";
      ldescr = "Set if the FLT model caused ringing";
      break;
    default:
      retval = NULL;
    }


  } else {
    *status = SAI__ERROR;
    errRep("", "Did not understand requested quality family",
           status );
  }

  if (descr) *descr = ldescr;

  return retval;
}
