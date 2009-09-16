/*
*+
*  Name:
*     sc2fts_freqcorr.c

*  Purpose:
*     Mainly remove the obliquity effect of the detector

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_freqcorr ( Grp* igrp, Grp* ogrp, AstKeyMap * parKeymap,
*                       int *status )

*  Arguments:
*     igrp = Grp* (Given)
*        the group of input files
*     ogrp = Grp* (Given)
*        the group of output files
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation. Currently, there is one 
*        parameter in parKeymap:
*        THETA:  the THETA data file name.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*
*
* Structure of THETA data file which stores off-axis angles of pixels.
* Output of hdstrace:

THETA  <NDF>

   DATA_ARRAY     <ARRAY>         {structure}
      DATA(40,32)    <_DOUBLE>       0,0.001,0.002,0.003,0.004,0.005,
                                     ... 3.926,3.927,3.928,3.929,3.93,3.931

End of Trace.

*  Authors:
*     BZ: B.Zhang (UoL)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2
*     2008-05-10 (BZ):
*        Complete first draft of FreqCorr module
*     2009-09-15 (TIMJ):
*        Must read size of THETA from THETA and not assume BOLROW and BOLCAL.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#include "libsc2sim/sc2sim.h"   /* for constants: BOLCOL, BOLROW */

/* FTS-2 includes */
#include "sc2fts_common.h"

#define FUNC_NAME "sc2fts_freqcorr"

void sc2fts_freqcorr 
(
Grp *igrp,
Grp* ogrp,
AstKeyMap* parKeymap,
int *status          /* global status (given and returned) */
)
{
   int i, j, k, index;
   const char* theta_fn;              /* the file name of THETA */
   int theta_size;                    /* size of THETA */
   int theta_dims[2];                 /* dimensionality of THETA */
   int theta_indf;                    /* NDF identifier of THETA */
   double *theta_vals = NULL;         /* THETA values */
   smfData *data;                     /* Pointer to in/output SCUBA2 data struct */
   float *tstream = NULL;             /* Pointer to input data stream */
   int nwn;                           /* number of spectral wavenumber in input data */
   float *spectrum_orig;              /* original spectrum */   
   float *spectrum_corr;              /* corrected spectrum */
   double *wn_corr;                   /* new wavenumber */

   /* get the Theta file name */
   if( astMapHasKey( parKeymap, "THETA" ) ==0)
   {
     printf("No THETA file!!!\n");
     return;
   }
   else
   {
     astMapGet0C(parKeymap, "THETA", &theta_fn);
   }

   /* NDF start */
   ndfBegin();

   /* open THETA file */
   ndfFind( NULL, theta_fn, &theta_indf, status );
   ndfMap( theta_indf, "DATA", "_DOUBLE", "READ", (void*)&theta_vals, &theta_size, status );
   ndfDim( theta_indf, 2, theta_dims, &i, status );

  /* verify that everything was found */
  if(*status != SAI__OK)
  {
	errRep(FUNC_NAME,	"Theta file format incorrect", status);

	return;
  }

   /* open spectrumcube file */
   smf_open_file( ogrp, 1, "UPDATE", SMF__NOCREATE_QUALITY, &data, status );

   if(data->ndims != 3 || 
      (data->dims)[0] != theta_dims[0] ||
      (data->dims)[1] != theta_dims[1])
   {
     if (*status == SAI__OK) {
       *status = SAI__ERROR;
       errRep( " ",  "structure of data array is wrong!!!", status);
     }
   } 
   else
   {
     size_t nbolx = theta_dims[0];
     size_t nboly = theta_dims[1];
     size_t nbols = nbolx * nboly;

     nwn = (data->dims)[2];

     /* retrieve pointer to the input data */
     tstream = (float*)((data->pntr)[0]);

     /* allocate memory */
     spectrum_orig = smf_malloc(nwn, sizeof(*spectrum_orig), 0, status);
     spectrum_corr = smf_malloc(nwn, sizeof(*spectrum_corr), 0, status);
     wn_corr = smf_malloc(nwn, sizeof(*wn_corr), 0, status);

     for(i=0; i<nboly; i++)
       for(j=0; j<nbolx; j++)
       {
	 int bindex = j + nboly*i;
         for(k=0; k<nwn; k++)
         {
           index =  bindex + nbols*k;
           *(spectrum_orig + k) = *(tstream+index);
           wn_corr[k] = k*cos(theta_vals[bindex]);
         }
         /* frequency shift by cubic spline */
         csi_simplified(spectrum_orig, nwn, wn_corr, nwn, spectrum_corr);
         for(k=0; k<nwn; k++)
         {
           index = bindex + nbols*k;
           *(tstream+index) = *(spectrum_corr + k);
         }
       }
     /* release memory */
     smf_free(spectrum_corr, status);
     smf_free(spectrum_orig, status);
   }
  
   /* close NDF file */
   smf_close_file(&data, status);
   ndfAnnul( &theta_indf, status );

   /* NDF end */
   ndfEnd( status );
}
