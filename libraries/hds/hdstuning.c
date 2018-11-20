#include <pthread.h>
#include <string.h>
#include <stdlib.h>

#include "dat1.h"
#include "sae_par.h"
#include "hds.h"
#include "dat_err.h"
#include "hds_types.h"
#include "ems.h"
#include "star/hds_v4.h"

/* Variable storing tuned state */

/* Indicates that we have looked at the environment */
static hdsbool_t HAVE_INITIALIZED_TUNING = 0;

/* Use version 5 or version 4? Default to
   use version 5 */
static int USE_VERSION5 = 1;

/* Report an error if a thread lock function is used on a V4 locator?
   Otherwise, the function returns without action. Default is to return
   without action. Switch V4LOCK_ERROR on for debugging. */
static int V4LOCK_ERROR = 0;


/* A mutex used to serialise access to the getters and setters so that
   multiple threads do not try to access the global data simultaneously. */
static pthread_mutex_t hdstuning_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX pthread_mutex_lock( &hdstuning_mutex );
#define UNLOCK_MUTEX pthread_mutex_unlock( &hdstuning_mutex );

/* Configure HDS based on the given version number.  This includes
   setting the HDS-v4 tuning parameter 64BIT as appropriate for
   version 4 or 3. */

static void hds1SetHDSVersion(int version, int* status) {
  switch (version) {
    case 5:
      USE_VERSION5 = 1;
      break;
    case 4:
      USE_VERSION5 = 0;
      hdsTune_v4("64BIT", 1, status);
      break;
    case 3:
      USE_VERSION5 = 0;
      hdsTune_v4("64BIT", 0, status);
      break;
    default:
      *status = DAT__NAMIN;
      emsRepf("hdsTune_1", "hdsTune: Unknown HDS version '%d'",
              status, version);
  }
}

/* Parse tuning environment variables. Should only be called once the
   first time a tuning parameter is required */

static void hds1ReadTuneEnvironment () {
  LOCK_MUTEX;
  if(!HAVE_INITIALIZED_TUNING) {
    int status = SAI__OK;
    int version = 0;
    dat1GetEnv( "HDS_VERSION", USE_VERSION5 ? 5 : 4, &version );
    emsBegin(&status);
    hds1SetHDSVersion(version, &status);
    emsEnd(&status);
    dat1GetEnv( "HDS_V4LOCKERROR", V4LOCK_ERROR, &V4LOCK_ERROR );
    HAVE_INITIALIZED_TUNING = 1;
  }
  UNLOCK_MUTEX;
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
*        - VERSION: 5 and v5 will be called for new files,
*                   4, v4 will be used to create new files,
*                   3, v4 library is used for new files, with 64bit=0.
*        - V4LOCKERROR: Report an error if a thread lock function is used on
*                       a V4 locator (otherwise, do nothing).
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

  /* Ensure that defaults have been read first so that we can override them. */
  hds1ReadTuneEnvironment();

  if (strncmp( param_str, "VERSION", 7) == 0 ) {
    LOCK_MUTEX;
    hds1SetHDSVersion(value, status);
    UNLOCK_MUTEX;
  } else if (strncmp( param_str, "V4LOCKERROR", 11) == 0 ) {
    LOCK_MUTEX;
    V4LOCK_ERROR = ( value == 0 ? 0 : 1 );
    UNLOCK_MUTEX;
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
*        - VERSION: Wrapper is using v5 for file creation if equal to 5.
*        - V4LOCKERROR: Report an error if a thread lock function is used on
*                       a V4 locator (otherwise, do nothing).
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

  if (strncasecmp( param_str, "VERSION", 7 ) == 0) {
    LOCK_MUTEX;
    if (USE_VERSION5) {
      *value = 5;
    }
    else {
      int is64bit = 1;
      hdsGtune_v4("64BIT", &is64bit, status);
      *value = ( is64bit ? 4 : 3 );
    }
    UNLOCK_MUTEX;
  } else if (strncasecmp( param_str, "V4LOCKERROR", 11 ) == 0) {
    LOCK_MUTEX;
    *value = ( V4LOCK_ERROR ? 1 : 0 );
    UNLOCK_MUTEX;
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

hdsbool_t hds1V4LockError() {
  /* Ensure that defaults have been read */
  hds1ReadTuneEnvironment();
  return V4LOCK_ERROR;
}
