/*
*+
*  Name:
*     smurf_sim

*  Purpose:
*     Top-level SIMULATE implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sim( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the SIMULATE task.

*  ADAM Parameters:
*     TBD

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2005-06-06 (AGG/EC/JB):
*        Clone from smurf_makemap
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smurf_sim"
#define TASK_NAME "SIM"
#define LEN__METHOD 20

void smurf_sim( int *status ) {

  /* Local Variables */
  char obsxmlfile[LEN__METHOD];
  char simxmlfile[LEN__METHOD];
  char outdir[LEN__METHOD];
  int seed;
  int maxwrite;

  /* Main routine */
  ndfBegin();

  /* Get input parameters */

  parGet0c("OBSXMLFILE", obsxmlfile, LEN__METHOD, status);
  parGet0c("SIMXMLFILE", simxmlfile, LEN__METHOD, status);
  parGet0i("SEED", &seed, status);
  parGet0c("OUTDIR", outdir, LEN__METHOD, status);
  parGet0i("MAXWRITE", &maxwrite, status);
  
  ndfEnd( status );
  
  msgOutif(MSG__VERB, FUNC_NAME, "Simulation successful.", status);
}
