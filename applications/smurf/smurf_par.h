/*
*+
*  Name:
*     smurf_par.h

*  Purpose:
*     Constants for the smurf application

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "smurf_par.h"

*  Description:
*     Prototypes and constants used by the libsmurf functions.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

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

#ifndef SMURF_PAR_DEFINED
#define SMURF_PAR_DEFINED

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Check for math.h and then (preferably) gsl_math.h as we want to use
   a pre-defined value for PI and multiples/fractions thereof */
#if HAVE_MATH_H
#  include <math.h>
#endif
#if HAVE_GSL_GSL_MATH_H
#  include <gsl/gsl_math.h>
#endif

#ifndef M_PI_2
#  ifdef M_PI
#    define M_PI_2  (M_PI/2)
#  else
error can not determine PI
#  endif
#endif

#endif /* SMURF_PAR_DEFINED */
