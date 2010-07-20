/*
*+
*  Name:
*     smurf_fts2_flatfield.c

*  Purpose:
*     Corrects for the pixel-to-pixel variation in spectral responsivity, 
*     due to detector characteristics.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_flatfield(int *status)

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Corrects for the pixel-to-pixel variation in spectral responsivity, 
*     due to detector characteristics.
*     
*     Calibration Data:
*        Measured Spectral Flatfield, RESPONSIVITY
*
*        RESPONSIVITY  <NDF>
*           DATA_ARRAY     <ARRAY>         {structure}
*           DATA(40,32,6000)  <_DOUBLE>    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
*                                        ... 0.3931,0.3931,0.3931,0.3931,0.3931
*
*           MORE           <EXT>           {structure}
*              UNIT           <_CHAR*4>       '1/mm'
*              FACTOR         <_DOUBLE>       0.001

*  Authors:
*     COBA: Coskun (Josh) Oba, University of Lethbridge

*  History :
*     15-JUL-2010 (COBA):
*        Original version.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

// STARLINK includes
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_fts2_flatfield"
#define TASK_NAME "FTS2_FLATFIELD"

void smurf_fts2_flatfield(int *status) 
{
  // Requirement SUN/104: Do nothing if status is NOT SAI__OK
  if( *status != SAI__OK ) return;

  // TODO 
  // Place Holder
}
