/*
*+
*  Name:
*     smf_obstype_str

*  Purpose:
*     Return string representation of observation type

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char * smf_obstype_str( smf_obstype obstype, int * status );

*  Arguments:
*     obstype = smf_obstype (Given)
*        Obs type code to convert to string.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to obs type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string representation of the underlying
*     obs type. Returns NULL pointer if the data type is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-24 (TIMJ):
*        Initial version, copied from smf_dtype_str.
*     2010-03-16 (TIMJ):
*        Add fastflat
*     2010-12-06 (TIMJ):
*        Add engineering modes.

*  Notes:
*     - See also smf_dtype_str, smf_obsmode_str.

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
#define FUNC_NAME "smf_obstype_str"

const char * smf_obstype_str( smf_obstype type, int * status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* now switch on data type */
  switch( type ) {
  case SMF__TYP_NULL:
    retval = NULL;
    break;
  case SMF__TYP_SCIENCE:
    retval = "science";
    break;
  case SMF__TYP_POINTING:
    retval = "pointing";
    break;
  case SMF__TYP_FOCUS:
    retval = "focus";
    break;
  case SMF__TYP_SKYDIP:
    retval = "skydip";
    break;
  case SMF__TYP_FLATFIELD:
    retval = "flatfield";
    break;
  case SMF__TYP_NOISE:
    retval = "noise";
    break;
  case SMF__TYP_FASTFLAT:
    retval = "fastflat";
    break;
  case SMF__TYP_HEATRAMP:
    retval = "heatramp";
    break;
  case SMF__TYP_BIASRAMP:
    retval = "biasramp";
    break;
  case SMF__TYP_BIASSAW:
    retval = "biassaw";
    break;
  case SMF__TYP_NEP:
    retval = "nep";
    break;
  case SMF__TYP_RAMP:
    retval = "ramp";
    break;
  case SMF__TYP_IV_CURVES_M:
    retval = "iv curves m";
    break;
  case SMF__TYP_IV_CURVES_H:
    retval = "iv curves h";
    break;
  case SMF__TYP_OPEN_LOOP_G:
    retval = "open loop g";
    break;
  default:
    retval = NULL;
  }

  return retval;
}
