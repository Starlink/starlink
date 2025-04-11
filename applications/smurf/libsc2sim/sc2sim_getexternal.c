/*
 *+
 *  Name:
 *     sc2sim_getexternal

 *  Purpose:
 *     Get coordinates of an existing SCUBA2 scan pattern

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getexternal ( double samptime, int *scancount,
 *                            double *posptr, int *status )

 *  Arguments:
 *     samptime = double (Given)
 *        Sample interval in sec
 *     scancount = int* (Returned)
 *        Number of positions in pattern
 *     posptr = double** (Returned)
 *        list of positions
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine determines the scanning positions for a scan
 *     when provided an existing SCUBA2 observation file.

 *  Authors:
 *     Todd Mackenzie (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2010-06-22 (JB):
 *        Clone from sc2sim_getsinglescan.c

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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Standard includes */
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "libsmf/smf.h"

void sc2sim_getexternal
(
 char external[SC2SIM__FLEN], /* the name of the external observation */
 int *scancount,      /* number of positions in pattern (returned) */
 double **posptr,     /* list of positions (returned) */
 int *status          /* global status (given and returned) */
 )

{
  int frame;          /* frame counter */
  Grp *igrp=NULL;     /* declare a Grp for holding file names */
  smfData *data=NULL; /* declare a smfData pointer */
  AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
  AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
  double temp1a;                   /* store temporary values */
  double temp2a;                   /* store temporary values */
  double temp1b;                   /* store temporary values */
  double temp2b;                   /* store temporary values */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Put the external observation filename into igrp */
  igrp = grpNew("Files",status);
  grpPut1(igrp,external,0,status);

  /* Load in that file name */
  smf_open_file(NULL, igrp,1,"READ",SMF__NOCREATE_DATA,&data,status);

  /* Get the size of the scan */
  *scancount = data->hdr->nframes;

  /* Allocate memory for the list of positions */
  *posptr = astCalloc( (*scancount)*2, sizeof(**posptr) );


  /* Get the scan positions */
  for (frame = 0; frame < *scancount; frame++)
  {
    /* Calculate the coordinate transformation */
    fc = astFitsChan ( NULL, NULL, " " );
    sc2ast_makefitschan( 0.0, 0.0, AST__DR2D, AST__DR2D,
                         data->hdr->allState[frame].tcs_az_bc1*AST__DR2D,
                         data->hdr->allState[frame].tcs_az_bc2*AST__DR2D,
                         "CLON-TAN", "CLAT-TAN", fc, status );
    astClear( fc, "Card" );
    fs = astRead( fc );

    /* Get the azimulthal and elevation coordinates */
    temp1a = data->hdr->allState[frame].tcs_az_ac1;
    temp2a = data->hdr->allState[frame].tcs_az_ac2;

    /* Get the offsets from the base coordinates */
    astTran2( fs, 1, &temp1a, &temp2a, 0, &temp1b, &temp2b );

    /* Copy the coordinates to the position array */
    (*posptr)[frame*2]= temp1b / ERFA_DAS2R;
    (*posptr)[frame*2+1]= temp2b / ERFA_DAS2R;

//    if( frame == 0 )
//    {
//      printf("Base coordinates: %f   %f\n", data->hdr->allState[frame].tcs_az_bc1,
//            data->hdr->allState[frame].tcs_az_bc2);
//      printf("pointing:         %f   %f\n", temp1a, temp2a);
//      astShow( fs );
//      printf("offset:           %e   %e\n", (*posptr)[frame*2], (*posptr)[frame*2+1]);
//    }

    /* Annul sc2 frameset for this time slice */
    if( fs ) fs = astAnnul( fs );
    if( fc ) fs = astAnnul( fc );
  }

  /* clean up */
  if( igrp ) grpDelet(&igrp, status);
  if( data ) smf_close_file( NULL, &data, status);
}
