/*
*     smf_math_fpderv

*  Purpose:
*     Wrapper routine for smf_math_functions, to just return the partial
*     derivatives of a fuction.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*       void smf_math_fpderv( int *fid, double *xdat, double *fpar, int *ncomp,
*                             double *epar, int *iopt, double *dopt );

*  Arguments:
*     fid  = int* Given
*        function identifier
*     xdat = double* Given
*        coordinate to evaluate function at
*     fpar = double* Given
*        function parameters (depends on function called)
*     ncomp = int* Given
*        number of components (see below)
*     epar = double* Returned
*        derivatives to parameters at coordinate
*     iopt = int* Given
*        optional integer parameter(s) to be passed-through to
*        the called function or derivate subroutine.
*     dopt = double* Given
*        optional double parameter(s) to be passed-through to
*        the called function or derivate subroutine.

*  Description:
*     This routine returns the partial derivatives to the parameters of
*     a function at position xdat. See smf_math_functions.

*  Notes:
*     Used by smf_lsqfit and smf_fit_profile.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-02-03 (RPT):
*        Starlink version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/kaplibs.h"
#include "star/util.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

void smf_math_fpderv( int *fid, double *xdat, double *fpar, int *ncomp,
		      double *epar, int *iopt, double *dopt )
/*--------------------------------------------------------------------
**
** Returns the value at xdat of the function identified by fid.
**
**-------------------------------------------------------------------- */
{

  smf_math_functions( fid, xdat, fpar, ncomp, NULL, epar, NULL,
                      iopt, dopt );

}

