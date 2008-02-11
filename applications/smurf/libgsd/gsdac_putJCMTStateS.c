/*
*+
*  Name:
*     gsdac_putJCMTStateS

*  Purpose:
*     Fill the subsystem-dependent JCMTState headers. 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_putJCMTStateS ( const gsdac_gsd_struct *gsd, 
*                           const unsigned int stepNum, 
*                           const unsigned int subsysNum, 
*                           struct JCMTState *record, 
*                           int *status );

*  Arguments:
*     gsd = const gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     stepNum = const unsigned int (Given)
*        time step of this spectrum
*     subsysNum = const unsigned int (Given)
*        Subsystem number
*     record = JCMTState** (Given and returned)
*        JCMTState headers
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Determines the headers required for a JCMTState header
*     in an ACSIS format file from a GSD file.  This routine determines
*     the values of the JCMTState elements which are dependent upon
*     this particular subsystem.  

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-01-29 (JB):
*        Original

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

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
*     Currently kludged with default values.
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "mers.h"
#include "ndf.h"
#include "gsd.h"
#include "sae_par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libacsis/specwrite.h"

#include "libgsd/gsdac.h"
#include "libgsd/gsdac_struct.h"

#include "jcmt/state.h"

void gsdac_putJCMTStateS ( const struct gsdac_gsd_struct *gsd, 
                           const unsigned int stepNum, 
                           const unsigned int subsysNum,
                           struct JCMTState *record, int *status )
{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Fill the JCMTState (KLUDGED with default vals). */

  /* Get the frontend LO frequency. */
  gsdac_getElemd ( gsd, "C3BEFENULO", subsysNum-1, 
                   &(record->fe_lofreq), status );

 

}
