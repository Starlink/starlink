/*
*     smf_math_fvalue

*  Purpose:
*     Wrapper routine for smf_math_functions, to just return the value
*     of a function.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*       double smf_math_fvalue( int fid, double xdat, const double fpar[],
*                               int ncomp, const int iopt[], double dopt[] );

*  Arguments:
*     fid  = int Given
*        function identifier
*     xdat = double Given
*        coordinate to evaluate function at
*     fpar = const double [] Given
*        function parameters (depends on function called)
*     ncomp = int Given
*        number of components (see below)
*     iopt = const int [] Given
*        optional integer parameter(s) to be passed-through to
*        the called function or derivate subroutine.
*     dopt = const double [] Given
*        optional double parameter(s) to be passed-through to
*        the called function or derivate subroutine.

*  Description:
*     This routine returns the function value at position xdat. See
*     smf_math_functions.

*  Notes:
*     Used by smf_lsqfit and smf_fit_profile.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-02-03 (RPT):
*        Starlink version
*     2012-04-10 (TIMJ):
*        Use const and stop using unnecessary pointers.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010, 2012 Science and Technology Facilities Council.
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

double smf_math_fvalue( int fid, double xdat, const double fpar[], int ncomp,
			const int iopt[], const double dopt[] )
/*--------------------------------------------------------------------
**
** Returns the value at xdat of the function identified by fid.
**
**-------------------------------------------------------------------- */
{
  double value;

  smf_math_functions( fid, xdat, fpar, ncomp, &value, NULL, NULL,
                      iopt, dopt );

  return( value );
}

