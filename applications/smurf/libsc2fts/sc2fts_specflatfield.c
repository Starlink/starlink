/*
*+
*  Name:
*     sc2fts_specflatfield.c

*  Purpose:
*     Calibrate the spectra by the detector's responsivity

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_specflatfield ( Grp* igrp, Grp* ogrp, AstKeyMap * parKeymap,
*                            int *status )

*  Arguments:
*     igrp = Grp* (Given)
*        the group of input files
*     ogrp = Grp* (Given)
*        the group of output files
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation. Currently, there is one
*        parameter in parKeymap:
*        RESP:  the responsivity data file name.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*
*
* Structure of pixel's spectral responsivity data file.
* Output of hdstrace:
RESPONSIVITY  <NDF>

   DATA_ARRAY     <ARRAY>         {structure}
      DATA(40,32,5000)  <_DOUBLE>    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                     ... 0.3931,0.3931,0.3931,0.3931,0.3931

   MORE           <EXT>           {structure}
      UNIT           <_CHAR*4>       '1/mm'
      FACTOR         <_DOUBLE>       0.001

End of Trace.

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2

*  Copyright:
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

/* Standard includes */
#include <math.h>

/* STARLINK includes */
#include "ast.h"
#include "star/hds.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "libsmf/smf.h"

#include "libsc2sim/sc2sim.h"   /* for constants: BOLCOL, BOLROW */

void sc2fts_specflatfield
(
Grp *igrp,
Grp* ogrp,
AstKeyMap* parKeymap,
int *status          /* global status (given and returned) */
)
{
   const char* resp_fn;              /* the file name of RESPONSIVITY */
   HDSLoc *loc_resp = NULL;          /* root HDSLoc */
   HDSLoc *loc_da = NULL;            /* HDSLoc to DATA_ARRAY */
   HDSLoc *loc_data = NULL;          /* HDSLoc to DATA of DATA_ARRAY */
   float *resp_vals = NULL;          /* RESPONSIVITY values */
   HDSLoc *loc_more = NULL;          /* HDSLoc to RESPONSIVITY More */
   HDSLoc *loc_wn_factor = NULL;     /* HDSLoc to RESPONSIVITY More.FACTOR */
   double resp_wnfactor;             /* wavenumber factor of RESPONSIVITY */
   size_t resp_size;                    /* size of RESPONSIVITY */

   /* get the Theta file name */
   if( astMapHasKey( parKeymap, "RESP" ) ==0)
   {
     printf("No RESPONSIVITY file!!!\n");
     return;
   }
   else
   {
     astMapGet0C(parKeymap, "RESP", &resp_fn);
   }

   /* NDF start */
   ndfBegin();

   /* open RESPONSIVITY file */
   hdsOpen(resp_fn, "READ", &loc_resp, status);
   datFind(loc_resp, "DATA_ARRAY", &loc_da, status);
   datFind(loc_da, "DATA", &loc_data, status);

   /* get the size of data_array */
   datSize(loc_data, &resp_size, status);

   resp_vals = (float*)astCalloc( resp_size, sizeof(float), 0 );
   /* get data_array */
   datGetVR(loc_data, resp_size, resp_vals,
            &resp_size, status);

   /* get wavenumber */
   datFind(loc_resp, "MORE", &loc_more, status);
   datFind(loc_more, "FACTOR", &loc_wn_factor, status);
   datGet0D(loc_wn_factor, &resp_wnfactor, status);

   /* close THETA file */
   datAnnul(&loc_wn_factor, status);
   datAnnul(&loc_more, status);
   datAnnul(&loc_data,  status);
   datAnnul(&loc_da,    status);
   datAnnul(&loc_resp, status);

  /* release momery */
  astFree( resp_vals );

  /* NDF end */
  ndfEnd(status);

  printf("SpecFlatfield operation!\n");
}
