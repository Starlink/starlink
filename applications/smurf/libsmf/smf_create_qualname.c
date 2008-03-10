/*
*+
*  Name:
*     smf_create_qualname

*  Purpose:
*     Create the quality names extension in an NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_create_qualname( char *mode, int indf, IRQLocs *qlocs, int *status );

*  Arguments:
*     mode = char* (Given)
*        Access mode for file
*     indf = int (Given)
*        NDF identifier for file
*     qlocs = IRQLocs* (Given and Returned)
*        Pointer to array of IRQ locators for quality names
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates the Quality Names extension in the current
*     NDF file if permitted by the current access mode. By default
*     this is created under the SMURF extension which is also created
*     if it does not already exist. It is designed to be called after
*     an initial query to the file to see if the extension exists
*     (with irqFind) and the status should be checked and reset to
*     good before entry. If the file specified by the given NDF
*     identifier has read-only access, the routine will return and no
*     extension will be created. This is not considered to be an error.

*  Notes:
*     qlocs should be a null pointer on entry but there are no checks
*     so if called after a successful call to irqFind, this routine
*     will overwrite that pointer.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-10 (AGG):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia. All Rights Reserved.

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


/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_create_qualname"

void smf_create_qualname( char *mode, int indf, IRQLocs *qlocs, int *status ) {

  int fixed;                 /* Flag to denote whether quality bit is fixed */
  int value;                 /* Value of current quality bit */
  int there = 0;             /* Flag to denote presence of NDF extension */
  HDSLoc *smurfloc = NULL;   /* HDS locator for the SMURF extension */

  if ( *status != SAI__OK ) return;

  /* Check for access mode */
  if (strncmp(mode,"READ",4) == 0 ) {
    msgOutif(MSG__VERB, "", 
	     "Input file is read-only - unable to create quality names extension",
	     status);
    return;
  }

  msgOutif(MSG__VERB, "", "Creating quality names extension", status);
  ndfXstat( indf, "SMURF", &there, status );
  if (!there) {
    /* Create SMURF extension if it does not already exist */
    ndfXnew( indf, "SMURF", "SMURF", 0, NULL, &smurfloc, status );
  }

  /* Create new quality names extension */
  irqNew( indf, "SMURF", &qlocs, status );

  /* Add SMURF quality names */
  msgOutif(MSG__VERB, "", "Adding SMURF quality names", status);
  irqAddqn( qlocs, "BADSAM", 0, 
	    "Set iff a bolometer is flagged by the DA", status );
  irqAddqn( qlocs, "BADBOL", 0, 
	    "Set iff all data from a bolometer is to be ignored", 
	    status );
  irqAddqn( qlocs, "SPIKE", 0, "Set iff a spike is detected", 
	    status );
  irqAddqn( qlocs, "DCJUMP", 0, "Set iff a DC jump is present", 
	    status );

  /* Now fix the bits to the desired values */
  irqFxbit( qlocs, "BADSAM", SMF__Q_BADS, &fixed, status );
  irqFxbit( qlocs, "BADBOL", SMF__Q_BADB, &fixed, status );
  irqFxbit( qlocs, "SPIKE", SMF__Q_SPIKE, &fixed, status );
  irqFxbit( qlocs, "DCJUMP", SMF__Q_JUMP, &fixed, status );

  /* Set names to read only */
  irqRwqn( qlocs, "BADSAM", 1, 1, &value, status );
  irqRwqn( qlocs, "BADBOL", 1, 1, &value, status );
  irqRwqn( qlocs, "SPIKE",  1, 1, &value, status );
  irqRwqn( qlocs, "DCJUMP", 1, 1, &value, status );

  if ( smurfloc ) datAnnul( &smurfloc, status);


}
