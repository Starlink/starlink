#include <string.h>
#include <stdlib.h>

#include "dat1.h"
#include "sae_par.h"
#include "hds.h"
#include "dat_err.h"
#include "hds_types.h"
#include "ems.h"

/* Variable storing tuned state */

/* Indicates that we have looked at the environment */
static hdsbool_t HAVE_INITIALIZED_TUNING = 0;

/* Use version 5 or version 4? Default to
   use version 4 */
static int USE_VERSION5 = 0;

/* Parse tuning environment variables. Should only be called once the
   first time a tuning parameter is required */

static void hds1ReadTuneEnvironment () {

  if (HAVE_INITIALIZED_TUNING) return;
  dat1Getenv( "HDS_VERSION5", USE_VERSION5, &USE_VERSION5 );
  HAVE_INITIALIZED_TUNING = 1;
}

/*
*+
*  Name:
*     hds1TuneWrapper

*  Purpose:
*     Tune the HDS wrapper library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     hds1TuneWrapper( const char *param_str, int value, int * status );

*  Arguments:
*     param_str = const char * (Given)
*        Name of the tuning parameter. Allowed values are:
*        - VERSION5: Positive value and v5 will be called for new files,
*                    zero, v4 will be used to create new files.
*     value = int (Given)
*        New parameter value.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Allow the HDS wrapper library behavior to be modified.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Internal routine that should not be called directly. Call hdsTune
*       as normal.

*  History:
*     2014-11-17 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

int hds1TuneWrapper( const char * param_str, int value, int *status ) {
  if (*status != SAI__OK) return *status;

  if (strncmp( param_str, "VERSION5", 8) == 0 ) {
    USE_VERSION5 = ( value == 0 ? 0 : 1 );
  } else {
    *status = DAT__NAMIN;
    emsRepf("hdsTune_1", "hdsTune: Unknown tuning parameter '%s'",
            status, param_str );
  }

  return *status;
}

/*
*+
*  Name:
*     hds1GtuneWrapper

*  Purpose:
*     Retrieve tuning parameter value for HDS wrapper

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     hds1GtuneWrapper( const char * param_str, int *value, int * status );

*  Arguments:
*     param_str = const char * (Given)
*        Name of the tuning parameter whose value is required (case insensitive).
*        Supported parameter names are:
*        - VERSION5: Wrapper is using v5 for file creation if non-zero.
*     value = int * (Returned)
*        Current value of the parameter.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Retrieve the current value of a tuning parameter for the HDS wrapper
*     library.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Internal routine called by wrapper implementation of hdsGtune.

*  History:
*     2014-11-17 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

int
hds1GtuneWrapper(const char *param_str, int *value, int *status) {
  if (*status != SAI__OK) return *status;

  /* Ensure that defaults have been read */
  hds1ReadTuneEnvironment();

  if (strncasecmp( param_str, "VERSION5", 8 ) ) {
    *value = ( USE_VERSION5 ? 1 : 0 );
  } else {
    *status = DAT__NAMIN;
    emsRepf("hdsGtune", "hdsGtune: Do not know how to report on parameter %s",
           status, param_str);
  }

  return *status;
}

/* The following function is for retrieving tuning values directly when
   the exact parameter is known. This saves on the string parsing */

hdsbool_t hds1UseVersion5() {
  /* Ensure that defaults have been read */
  hds1ReadTuneEnvironment();
  return USE_VERSION5;
}
