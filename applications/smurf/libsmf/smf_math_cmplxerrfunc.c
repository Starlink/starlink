/*
*     smf_math_cmplxerrfunc.c

*  Purpose:
*     Calculates complex error function (Faddeeva function) "w". 
*          W(Z) = EXP(-Z*Z)ERFC(-iZ)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*       void  smf_math_cmplxerrfunc( double Rz, double Iz, 
*                                    double *Rr, double *Ir );

*  Arguments:
*     Rz, Iz  = double Given
*        Real and Imaginary argument
*     Rr, Ir = double* Returned
*        Real and Imaginary part of the result

*  Description:
*     Calculates complex error function (Faddeeva function) "w". 
*          W(Z) = EXP(-Z*Z)ERFC(-iZ)
*          see Abromowitz and Stegun (chapter 7)
*          |error|  <  2 * 10e-6

*  Notes:
*     Derived (by permission) from the xgaufit routine of the GIPSY
*     software package of the Kapteyn Institute, Groningen, The Netherlands.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     Kor Begeman, Hans Terlouw, Martin Vogelaar (Kapteyn Institute, Groningen)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-07-19 (RPT):
*        Starlink version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010, 2012 Science and Technology Facilities Council.
*     Copyright (C) Kapteyn Laboratorium Groningen 2001
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

/* Simple default string for errRep */
#define FUNC_NAME "smf_math_cmplxerrfunc"

void smf_math_cmplxerrfunc( double Rz,
			    double Iz,
			    double *Rr,
			    double *Ir )
/*------------------------------------------------------------*/
/* PURPOSE: Error function for complex arguments.             */
/*          W(Z) = EXP(-Z*Z)ERFC(-iZ)                         */
/*          see Abromowitz and Stegun (chapter 7)             */
/*          |error|  <  2 * 10e-6                             */
/*------------------------------------------------------------*/
{
#define EPS     ( 0.0000001 )
   double	wdx = 0.0;		/* real part result */
   double	wdy = 0.0;		/* imag part result */
   double	x, y;			/* real and imaginary arg */

   x = fabs( Rz );                      /* real part */
   y = fabs( Iz );                      /* imaginairy part */
   if ( x > 6.0 || y > 6.0 ) {		/* approximation for large arguments */
      static double	at[] = { 0.5124242, 0.05176536 };
      static double	bt[] = { 0.2752551, 2.72474500 };
      int		i;

      for ( i = 0; i < 2; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else if ( x > 3.9 || y > 3.0 ) {	/* approximation for intermediate arguments */
      static double	at[] = { 0.4613135, 0.09999216, 0.002883894 };
      static double	bt[] = { 0.1901635, 1.78449270, 5.525343700 };
      int		i;

      for ( i = 0; i < 3; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else {				/* no approximation */
      double	r;

      wdx = 1.0;
      r = sqrt( x * x + y * y );
      if ( r > 0.0 ) {
         static double	tt[] = { 1.0000000000, 0.5641895835 };
         double		tn[2];
         double		del, csp, snp, tcn, tsn;
         int		n = 0;

         csp = -y / r;
         snp = x / r;
         tcn = 1.0; tsn = 0.0;
         tn[0] = tt[0]; tn[1] = tt[1];
         do {
            double	tc, ts;
            int		i;

            n += 1;				/* increment interation number */
            tc = tcn;				/* save */
            ts = tsn;				/* save */
            tcn = ( tc * csp - ts * snp );	/* next cosine term */
            tsn = ( ts * csp + tc * snp );	/* next sine term */
            i = n%2;				/* argument */
            tn[i] *= ( 2.0 / (double) n );
            tn[0] *= r;				/* multiply with radius */
            tn[1] *= r;				/* multiply with radius */
            del = tn[i];			/* increment */
            wdx += ( tcn * del );		/* add increment */
            wdy += ( tsn * del );		/* add increment */
         } while ( del > EPS );			/* precision is reached */
      }
   }
   if ( Rz >= 0.0 && Iz >= 0.0 ) {
      *Rr = wdx; *Ir = wdy;
   } else if ( Rz >= 0.0 && Iz < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      *Rr = sav * csp - wdx;
      *Ir = sav * snp + wdy;
   } else if ( Rz < 0.0 && Iz >= 0.0 ) {
      *Rr = wdx;
      *Ir = -wdy;
   } else if ( Rz < 0.0 && Iz < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      *Rr = sav * csp - wdx;
      *Ir = -sav * snp - wdy;
   }
#undef EPS
}
