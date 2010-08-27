/*
*+
*  Name:
*     SC2FTS

*  Purpose:
*     Fourier Transform Spectrometer specialist routines (incomplete).

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2fts( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command can be used to process Fourier Transform Spectrometer
*     data for SCUBA-2. The commands are incomplete, should not be used
*     at this time and may look very different when finally released.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Name of input data file.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Name of processed output file.

*  Authors:
*     Baoshe Zhang (UoL)
*     {enter_new_authors_here}

*  Notes:
*      Do not use this command yet.

*  History:
*     2008-03-16 (BZ):
*        Create a test code skeleton
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of Lethbridge. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard incldues */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
/*
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
*/
/* SMURF includes */
/*
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
*/

#include "libsmurf/smurflib.h"
#include "libsc2fts/sc2fts.h"

#define FUNC_NAME "smurf_sc2fts"
#define TASK_NAME "SC2FTS"

void smurf_sc2fts ( int *status ) {

  /* enter the FTS-2 smurf data reduction */
  // sc2fts_entry( status );

}
