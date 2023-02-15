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
*     smf_create_qualname( const char *mode, int indf, IRQLocs **qlocs,
*                          int *status );

*  Arguments:
*     mode = const char* (Given)
*        Access mode for file
*     indf = int (Given)
*        NDF identifier for file
*     qlocs = IRQLocs** (Given and Returned)
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
*     *qlocs should be a null pointer on entry but there are no checks
*     so if called after a successful call to irqFind, this routine
*     will overwrite that pointer.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-10 (AGG):
*        Initial version.
*     2008-04-02 (AGG):
*        Use bit numbers rather than values in fixing the bits,
*        determine numbers from values with private function
*     2008-04-09 (TIMJ):
*        Use const in input args where appropriate.
*        Fix pointer warning - we need a pointer to IRQLocs*
*     2008-06-25 (EC):
*        Added PAD for SMF__Q_PAD
*     2009-09-18 (TIMJ):
*        use c-preprocessor to define extension name.
*     2010-03-18 (EC):
*        Simplify using smf_qual_str
*     2010-03-19 (EC):
*        Added SMF__Q_COM
*     2010-06-17 (TIMJ):
*        Use new API for smf_qual_str to simplify code.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
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
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_create_qualname"

void smf_create_qualname( const char *mode, int indf, IRQLocs **qlocs,
                          int *status ) {

  int fixed;                 /* Flag to denote whether quality bit is fixed */
  int i;                     /* loop counter */
  int value;                 /* Value of current quality bit */
  int there = 0;             /* Flag to denote presence of NDF extension */
  HDSLoc *smurfloc = NULL;   /* HDS locator for the SMURF extension */

  if ( *status != SAI__OK ) return;

  /* Check for access mode */
  if (strncmp(mode,"READ",4) == 0 ) {
    msgOutif(MSG__DEBUG, "",
	     "Input file is read-only - unable to create quality names "
             "extension",
	     status);
    return;
  }

  msgOutif(MSG__DEBUG, "", "Creating quality names extension", status);
  ndfXstat( indf, SMURF__EXTNAME, &there, status );
  if (!there) {
    /* Create SMURF extension if it does not already exist */
    ndfXnew( indf, SMURF__EXTNAME, SMURF__EXTTYPE, 0, NULL, &smurfloc, status );
  }

  /* Create new quality names extension */
  irqNew( indf, SMURF__EXTNAME, qlocs, status );

  /* Add SMURF quality names -- check against smf_qual_str */
  msgOutif(MSG__DEBUG, "", "Adding SMURF quality names", status);
  for (i=0; i<SMF__NQBITS_TSERIES; i++) {
    const char * qdesc = NULL; /* Description of quality */
    const char * qstr = NULL;  /* Quality string identifier */
    qstr = smf_qual_str( SMF__QFAM_TSERIES, 1, i, &qdesc, status );

    /* Set the quality name */
    irqAddqn( *qlocs, qstr, 0, qdesc, status );

    /* Now fix the bits to the desired values */
    irqFxbit( *qlocs, qstr, i+1, &fixed, status );

    /* Set names to read only */
    irqRwqn( *qlocs, qstr, 1, 1, &value, status );
  }

  if ( smurfloc ) datAnnul( &smurfloc, status);
}
