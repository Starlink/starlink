/*
*+
*  Name:
*     SC2THREADTEST

*  Purpose:
*     Task for testing speeds of different threading schemes

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2threadtest( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine tests schemes for visiting large quantities of
*     SCUBA-2 data using multiple threads.

*  ADAM Parameters:
*     NTHREAD = NDF (Read)
*          Number of threads to use

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-11-14 (EC):
*        Initial version -- copy thread infrastructure from makemap

*  Copyright:
*     Council. Copyright (C) 2005-2008 University of British Columbia.
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
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <math.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "star/atl.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "smurf_typ.h"
#include "libsmf/smf_threads.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_sc2threadtest"
#define TASK_NAME "SC2THREADTEST"

void smurf_sc2threadtest( int *status ) {

  /* Local Variables */
  smfArray **res=NULL;       /* array of smfArrays of test data */ 
  smfData *data=NULL;        /* Pointer to SCUBA2 data struct */
  dim_t datalen;             /* Number of data points */
  size_t i;                  /* Loop counter */
  size_t j;                  /* Loop counter */
  size_t k;                  /* Loop counter */
  size_t nchunks;            /* Number of chunks */
  size_t nsub;               /* Number of subarrays */
  int nthread;               /* Number of threads */
  int temp;                  /* Temporary integer */
  size_t tsteps;             /* How many time steps in chunk */
  struct timeval tv1, tv2;   /* Timers */
  smfWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  if (*status != SAI__OK) return;

  /* Get input parameters */
  parGet0i( "NTHREAD", &nthread, status );
  parGdr0i( "TSTEPS", 6000, 0, NUM__MAXI, 1, &temp, status );
  tsteps = (size_t) temp;
  parGdr0i( "NCHUNKS", 1, 1, NUM__MAXI, 1, &temp, status );
  nchunks = (size_t) temp;
  parGdr0i( "NSUB", 1, 1, 4, 1, &temp, status );
  nsub = (size_t) temp;

  msgSeti("N",nthread);
  msgOut( "", TASK_NAME ": Running test with ^N threads", status );

  /* Create some fake test data in the form of an array of smfArrays */

  msgSeti("T",tsteps);
  msgSeti("C",nchunks);
  msgSeti("NS",nsub);
  msgOut( "", TASK_NAME 
          ": Creating ^NS subarrays of data with ^C chunks * ^T samples", 
          status );

  res = smf_malloc( nchunks, sizeof(*res), 1, status );

  for( k=0; (*status==SAI__OK)&&(k<nchunks); k++ ) {

    res[k] = smf_create_smfArray( status );

    for( i=0; (*status==SAI__OK)&&(i<nsub); i++ ) {
      /* Create individual smfDatas and add to array */
      data = smf_create_smfData( SMF__NOCREATE_FILE |
                                 SMF__NOCREATE_DA, status );
      
      if( *status==SAI__OK ) {
        data->dtype=SMF__DOUBLE;
        data->ndims=3;
        data->dims[0]=40;
        data->dims[1]=32;
        data->dims[2]=(dim_t) tsteps;
        datalen=1;
        for( j=0; j<data->ndims; j++ ) datalen *= data->dims[j];
        
        data->hdr->steptime = 0.005;
        
        data->pntr[0] = smf_malloc( datalen, smf_dtype_sz(data->dtype,status),
                                    1, status ); 
      }
    }

    smf_addto_smfArray( res[k], data, status );
  }
  

  /* Create a pool of threads. */
  wf = smf_create_workforce( nthread, status );


  /* Clean up */
  if( res ) {
    for( i=0; i<nchunks; i++ ) {
      if( res[i] ) smf_close_related( &res[i], status );
    }
    res = smf_free( res, status );
  }
  if( wf ) wf = smf_destroy_workforce( wf );

}
