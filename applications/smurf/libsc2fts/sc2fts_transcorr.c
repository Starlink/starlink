/*
*+
*  Name:
*     sc2fts_transcorr.c

*  Purpose:
*     Mainly remove PWV effect

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_transcorr ( Grp* igrp, Grp* ogrp, AstKeyMap * parKeymap, int *status )

*  Arguments:
*     igrp = Grp* (Given)
*        the group of input files
*     ogrp = Grp* (Given)
*        the group of output files
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation. Currently, there are three parameters
*        in parKeymap:
*        AM:  Air Mass
*        PWV: Precipitable Water Vapor (in mm). 
*        TAU: the TAU data file name. The values of wet and dry components of TAU are for
*             PWV=1mm and airmass of 1.
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*
*

* Structure of TAU data file which stores dry and wet components of atmosphere.
* Output of hdstrace:

TAU  <NDF>

   DATA_ARRAY     <ARRAY>         {structure}
      DATA(1)        <_UBYTE>        *
      ORIGIN(1)      <_INTEGER>      1

   MORE           <EXT>           {structure}
      UNIT           <_CHAR*4>       '1/mm'
      FACTOR         <_REAL>         0.0001
      DRY(25001)     <_REAL>         0,1.680301E-9,6.7279533E-9,
                                     ... 0.02712704,0.02691713,0.02676998
      WET(25001)     <_REAL>         0,6.5415251E-10,2.6174807E-9,
                                     ... 238.8973,242.7828,246.7523,250.8079

End of Trace.
 
Here, the wavenumber (=1/wavelength) for DRY(n) or WET(n) is n*FACTOR (in UNIT)
(n starts from 0).

Combinative TAU = (PWV x AM x TAU_wet) + (AM x TAU_dry)

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
#include <math.h>

/* STARLINK includes */
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"

void sc2fts_transcorr
(
Grp *igrp,
Grp* ogrp,
AstKeyMap* parKeymap,
int *status          /* global status (given and returned) */
)
{
   int i, j, k;
   int indf;            /* NDF identifier for a NDF */
   const char* tau_fn;  /* the file name of TAU */
   float airmass;       /* Air Mass */
   float pwv;           /* Precipitable Water Vapor */
   HDSLoc *loc_tau = NULL;   /* root HDSLoc */
   HDSLoc *loc_more = NULL;  /* HDSLoc to More */
   HDSLoc *loc_dry  = NULL;  /* HDSLoc to More.DRY */
   HDSLoc *loc_wet  = NULL;  /* HDSLoc to More.WET */
   HDSLoc *loc_wn_factor = NULL; /* HDSLoc to More.FACTOR */
   float wn_factor; 
   float *dry_ptr;               /* pointer to Dry component */
   float *wet_ptr;               /* pointer to Wet component */
   double *tau_ptr;              /* pointer to combinative TAU */
   int drywet_size;

   printf("TransCorr operation!\n");
 
   /* get the TAU file name */
   if(astMapHasKey( parKeymap, "TAU" ) ==0)
   {
     printf("No TAU file!!!\n");
     return;
   }
   else
   {
     astMapGet0C(parKeymap, "TAU", &tau_fn);
   }
   /* get the value of airmass */
   if(astMapHasKey( parKeymap, "AM" ) ==0)
   {
     printf("No AirMass!!!\n");
     return;
   }
   else
   {
     astMapGet0F(parKeymap, "AM", &airmass);
   }
   /* get the value of PWV */
   if(astMapHasKey( parKeymap, "PWV" ) ==0)
   {
     printf("No PWV!!!\n");
     return;
   }
   else
   {
     astMapGet0F(parKeymap, "PWV", &pwv);
   }

   /* NDF start */
   ndfBegin();

   /* open TAU file */
   hdsOpen(tau_fn, "READ", &loc_tau, status);
   datFind(loc_tau, "MORE", &loc_more, status);
   datFind(loc_more, "DRY", &loc_dry, status);
   datFind(loc_more, "WET", &loc_wet, status);
   datFind(loc_more, "FACTOR", &loc_wn_factor, status);

   /* get wavenumber */
   datGet0R(loc_wn_factor, &wn_factor, status);

   /* get dry/wet components */
   datSize(loc_dry, &drywet_size, status);

   dry_ptr = (float*)smf_malloc(drywet_size, sizeof(float), 0, status);
   wet_ptr = (float*)smf_malloc(drywet_size, sizeof(float), 0, status);

   
   datGetVR(loc_dry, drywet_size, (double*)dry_ptr,
            &drywet_size, status);
   datGetVR(loc_wet, drywet_size, (double*)wet_ptr,
            &drywet_size, status);

   /* generate combinative TAU and e^(-tau) */
   tau_ptr = (double*)smf_malloc(drywet_size, sizeof(double), 0, status);
   for(i=0; i<drywet_size; i++)
   {
     *(tau_ptr+i) = airmass*(pwv*(*(wet_ptr+i)) + *(dry_ptr+i));
     *(tau_ptr+i) = exp(-(*(tau_ptr+i)));
   }

   /* release memories */
   smf_free(dry_ptr, status);
   smf_free(wet_ptr, status);

   /* close THETA file */
   datAnnul(&loc_wn_factor, status);
   datAnnul(&loc_wet,  status);
   datAnnul(&loc_dry,  status);
   datAnnul(&loc_more, status);
   datAnnul(&loc_tau, status);

   /* release memories */
   smf_free(tau_ptr, status);

   /* NDF end */
   ndfEnd(status);
}
