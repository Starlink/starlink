/*
*+
*  Name:
*     smf_model_getname

*  Purpose:
*     Return strings representation of a smf_modeltype

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     const char *smf_model_getname( smf_modeltype type, int *status);

*  Arguments:
*     type = smf_modeltype
*        Enumerated model type (Given)
*     name = const char *name (Returned)
*        String corresponding to model type
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-07-06 (EC):
*        Initial Version
*     2006-08-16 (EC):
*        Use group expressions for model names (expects _flat for igrp names)
*        Changed technique/interface to look like smf_dtype_string
*     2007-02-12 (EC):
*        Return simple string, form grpex in smf_model_create instead
*     2007-06-13 (EC):
*        Added LUT
*     2008-03-03 (EC):
*        Added QUA
*     2008-06-24 (TIMJ):
*        Return const
*     2009-03-12 (EC):
*        Added SMF__FLT
*     2010-05-13 (TIMJ):
*        Added SMF__PLN
*     2010-05-27 (TIMJ):
*        Add SMF__SMO
*     2010-06-08 (EC):
*        Add SMF__TWO
*     2014-12-18 (DSB):
*        Added SSN.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2006-2009 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Other includes */
#include <stdio.h>

#define FUNC_NAME "smf_model_getname"

const char *smf_model_getname( smf_modeltype type, int *status ) {

  /* Local Variables */
  const char *retval = NULL;

  /* Main routine */
  if (*status != SAI__OK) {
    retval = "nul";
    return retval;
  }

  switch( type ) {

  case SMF__CUM:
    retval = "cum";
    break;

  case SMF__RES:
    retval = "res";
    break;

  case SMF__AST:
    retval = "ast";
    break;

  case SMF__COM:
    retval = "com";
    break;

  case SMF__NOI:
    retval = "noi";
    break;

  case SMF__EXT:
    retval = "ext";
    break;

  case SMF__LUT:
    retval = "lut";
    break;

  case SMF__QUA:
    retval = "qua";
    break;

  case SMF__DKS:
    retval = "dks";
    break;

  case SMF__GAI:
    retval = "gai";
    break;

  case SMF__FLT:
    retval = "flt";
    break;

  case SMF__PLN:
    retval = "pln";
    break;

  case SMF__SMO:
    retval = "smo";
    break;

  case SMF__SSN:
    retval = "ssn";
    break;

  case SMF__TWO:
    retval = "two";
    break;

  case SMF__TMP:
    retval = "tmp";
    break;

  default:
    retval = "nul";
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Invalid smf_modeltype given.", status);
  }

  return retval;
}
