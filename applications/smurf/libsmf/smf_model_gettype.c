/*
*+
*  Name:
*     smf_model_gettype

*  Purpose:
*     Return smf_modeltype given string representation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_modeltype smf_model_gettype( const char *modelname, int *status);

*  Arguments:
*     modelname = const char *
*        Mixed case string with 3-letter model component name
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-05 (EC):
*        Initial Version
*     2007-08-21 (EC):
*        Added typedef SMF__NUL
*     2008-03-03 (EC):
*        Added QUA/LUT
*     2009-03-12 (EC):
*        Added SMF__FLT
*     2010-05-13 (TIMJ):
*        Added SMF__PLN
*     2010-05-27 (TIMJ):
*        Add SMF__SMO
*     2010-06-08 (EC):
*        Add SMF__TWO
*     2010-06-14 (TIMJ):
*        Alphabetize models and use a switch statement rather than loads
*        of strcmp calls.
*     2014-12-18 (DSB):
*        Added SSN.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007-2009 University of British Columbia.
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
#include <strings.h>

#define FUNC_NAME "smf_model_gettype"

smf_modeltype smf_model_gettype( const char *modelname, int *status ) {

  /* Local Variables */
  smf_modeltype retval = SMF__NUL;

  /* Main routine */
  if (*status != SAI__OK) return retval;

  /* Check first character since these models are pretty unique */
  switch( modelname[0] ) {
  case 'A':
  case 'a':
    retval = SMF__AST;
    break;
  case 'C':
  case 'c':
    if (strncasecmp( modelname, "COM", 3 ) == 0) {
      retval = SMF__COM;
    } else if (strncasecmp( modelname, "CUM", 3 ) == 0) {
      retval = SMF__CUM;
    }
    break;
  case 'D':
  case 'd':
    retval = SMF__DKS;
    break;
  case 'E':
  case 'e':
    retval = SMF__EXT;
    break;
  case 'F':
  case 'f':
    retval = SMF__FLT;
    break;
  case 'G':
  case 'g':
    retval = SMF__GAI;
    break;
  case 'L':
  case 'l':
    retval = SMF__LUT;
    break;
  case 'N':
  case 'n':
    retval = SMF__NOI;
    break;
  case 'P':
  case 'p':
    retval = SMF__PLN;
    break;
  case 'R':
  case 'r':
    retval = SMF__RES;
    break;
  case 'S':
  case 's':
    if (strncasecmp( modelname, "SMO", 3 ) == 0) {
      retval = SMF__SMO;
    } else if (strncasecmp( modelname, "SSN", 3 ) == 0) {
      retval = SMF__SSN;
    }
    break;
  case 'T':
  case 't':
    if (strncasecmp( modelname, "TMP", 3 ) == 0) {
      retval = SMF__TMP;
    } else if (strncasecmp( modelname, "TWO", 3 ) == 0) {
      retval = SMF__TWO;
    }
    break;
  case 'Q':
  case 'q':
    retval = SMF__QUA;
    break;
  default:
    retval = SMF__NUL;
  }

  if ( retval == SMF__NUL && *status == SAI__OK ) {
    *status = SAI__ERROR;
    errRepf("", FUNC_NAME ": Invalid model name '%s'", status, modelname);
  }

  return retval;
}
