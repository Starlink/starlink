/*
*+
*  Name:
*     smf_calc_mapcoord

*  Purpose:
*     Generate a pointing LUT, and write it to a MAPCOORD extension if
*     data associated with a file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_calc_mapcoord( ThrWorkForce *wf, AstKeyMap *config, smfData *data,
*                        AstFrameSet *outfset, int moving, dim_t *lbnd_out,
*                        dim_t *ubnd_out, fts2Port fts_port, int flags,
*                        int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     config = AstKeyMap * (Given)
*        Pointer to a KeyMap holding configuration parameters. May
*        be NULL, in which case hard-wired defaults are used for any
*        configuration parameters that are needed (currently just
*        TSTEP=1 and EXPORTLONLAT=0).
*     data = smfData* (Given)
*        Pointer to smfData struct
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = dim_t * (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = dim_t * (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     fts_port = fts2Port (Given)
*        FTS-2 port.
*     flags = int (Given)
*        If set to SMF__NOCREATE_FILE don't attempt to write extension
*        to file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a MAPCOORD extension in the NDF associated
*     with data based on outfset & lbnd/ubnd_out. If a MAPCOORD
*     extension already exists and it uses the same mapping defined by
*     outfset/lbnd_out/ubnd_out it will not get re-calculated. If the routine
*     exits with clean status the smfData will have the LUT mapped and
*     the NDF ID of the table stored in the associated smfFile.
*
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)

*  History:
*     2006-05-16 (EC):
*        Initial version
*     2006-06-25 (EC):
*        Changed function name from smf_mapcoord to smf_calc_mapcoord
*     2006-07-10 (EC):
*        Only re-calculate LUT when necessary
*     2006-08-15 (EC):
*        Fixed off-by-one errors in *bnd_in
*     2007-07-12 (EC):
*        -adding moving to interface
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2007-11-26 (EC):
*        -Don't annul the NDF corresponding to the mapped LUT to leave it open
*        -If LUT doesn't need re-calculation, open with smf_open_mapcoord.
*        -Store the NDF ID of the LUT in the associated smfFile
*     2007-12-14 (EC):
*        -Added flags so that MAPCOORD extensions isn't always generated
*        -Assert ICD data order
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-25 (EC):
*        -explicitly close MAPCOORD extension before calculating new one
*     2008-04-21 (EC):
*        -Applied Andy's fix for off-by-one errors in nearest-neighbour calc
*     2009-04-30 (EC):
*        -Parellelized, borrowing code from smf_rebinmap and smf_rebinslices
*     2009-11-03 (TIMJ):
*        Skip bad bolo2map mappings.
*     2010-02-25 (DSB):
*        Only perform full Mapping calculations periodically.
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
*     2011-04-08 (DSB):
*        Ensure thrWait does not wait for jobs created earlier within the
*        calling function.
*     2011-12-13 (DSB):
*        Added exportlonlat to API.
*     2012-02-20 (DSB):
*        Added "config" to API, and removed "tstep" and "exportlonlat".
*     2014-02-06 (DSB):
*        Use "_lon" and "_lat" for exportlonlat file names, rather than the
*        AST symbols for the skyframe axes.
*     2018-08-02 (DSB):
*        Add "onmap" flag to smfData.

*  Notes:
*     This routines asserts ICD data order.

*  Copyright:
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
*     Copyright (C) 2005-2009 University of British Columbia.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/one.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "libsmf/smf.h"




/* ------------------------------------------------------------------------ */
/* Local variables and functions */

static double *smf1_calc_mapcoord1( smfData *data, dim_t nbolo,
                                    dim_t ntslice, AstSkyFrame *oskyfrm,
                                    int *indf, int axis, int *status );

/* Structure containing information about blocks of time slices that each
 thread will process*/
typedef struct smfCalcMapcoordData {
  AstMapping *sky2map;
  AstSkyFrame *abskyfrm;
  dim_t *lbnd_out;
  dim_t *ubnd_out;
  dim_t onmap;
  dim_t t1;               /* Index of first timeslice of block */
  dim_t t2;               /* Index of last timeslice of block */
  dim_t tstep;
  double *lat_ptr;
  double *lon_ptr;
  double *theta;
  fts2Port fts_port;
  int *lut;
  int ijob;                /* Job identifier */
  int moving;
  smfData *data;           /* Pointer to local smfData with copy of header */
} smfCalcMapcoordData;

/* Function to be executed in thread: coordinates for blocks of tslices*/

void smfCalcMapcoordPar( void *job_data_ptr, int *status );

void smfCalcMapcoordPar( void *job_data_ptr, int *status ) {
  AstSkyFrame *abskyfrm=NULL;
  smfData *data=NULL;
  fts2Port fts_port;
  dim_t *lbnd_out=NULL;
  int *lut=NULL;
  int moving;
  dim_t onmap;
  dim_t *ubnd_out=NULL;
  dim_t nbolo;             /* number of bolometers */
  dim_t ntslice;           /* number of time slices */
  smfCalcMapcoordData *pdata=NULL; /* Pointer to job data */
  AstMapping *sky2map=NULL;
  double *theta = NULL;
  struct timeval tv1;      /* Timer */
  struct timeval tv2;      /* Timer */

  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfCalcMapcoordPar: No job data supplied", status );
    return;
  }

  /* Extract values from pdata */
  abskyfrm = pdata->abskyfrm;
  data = pdata->data;
  lut = pdata->lut;
  theta = pdata->theta;
  lbnd_out = pdata->lbnd_out;
  moving = pdata->moving;
  sky2map = pdata->sky2map;
  ubnd_out = pdata->ubnd_out;
  fts_port = pdata->fts_port;

  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, NULL, NULL, status );

  /* if t1 past end of the work, nothing to do so we return */
  if( pdata->t1 >= ntslice ) {
    msgOutif( SMF__TIMER_MSG, "",
               "smfCalcMapcoordPar: nothing for thread to do, returning",
               status);
    return;
  }

  /* Debugging message indicating thread started work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfCalcMapcoordPar: thread starting on tslices %zu -- %zu",
             status, pdata->t1, pdata->t2 );
  smf_timerinit( &tv1, &tv2, status );

  /* Lock the supplied AST object pointers for exclusive use by this
     thread.  The invoking thread should have unlocked them before
     starting this job. */
  astLock( abskyfrm, 0 );
  astLock( sky2map, 0 );
  smf_lock_data( data, 1, status );

  /* Calculate and store the LUT values for the range of time slices
     being processed by this thread. A generic algorithm is used for
     moving targets, but a faster algorithm can be used for stationary
     targets. */
  smf_coords_lut( data, pdata->tstep, pdata->t1, pdata->t2,
                  abskyfrm, sky2map, moving, lbnd_out, ubnd_out, fts_port,
                  lut + pdata->t1*nbolo, theta + pdata->t1,
                  pdata->lon_ptr, pdata->lat_ptr, &onmap, status );

  /* Return the flag indicating if any data fell within the map bounds. */
  pdata->onmap = onmap;

  /* Unlock the supplied AST object pointers so that other threads can use
     them. */
  smf_lock_data( data, 0, status );
  astUnlock( abskyfrm, 1 );
  astUnlock( sky2map, 1 );

  msgOutiff( SMF__TIMER_MSG, "",
             "smfCalcMapcoordPar: thread finishing tslices %zu -- "
             "%zu (%.3f sec)", status, pdata->t1, pdata->t2,
             smf_timerupdate(&tv1, &tv2, status) );
}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_calc_mapcoord"

void smf_calc_mapcoord( ThrWorkForce *wf, AstKeyMap *config, smfData *data,
                        AstFrameSet *outfset, int moving, dim_t *lbnd_out,
                        dim_t *ubnd_out, fts2Port fts_port, int flags,
                        int *status ) {

  /* Local Variables */
  AstCmpMap *testcmpmap=NULL;  /* Combined forward/inverse mapping */
  AstFrameSet *oldfset=NULL;   /* Pointer to existing WCS info */
  AstMapping *bolo2map=NULL;   /* Combined mapping bolo->map coordinates */
  AstMapping *map2sky_old=NULL;/* Existing mapping map->celestial coord. */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  AstMapping *testsimpmap=NULL;/* Simplified testcmpmap */
  AstObject *fstemp = NULL;    /* AstObject version of outfset */
  AstSkyFrame *abskyfrm = NULL;/* Output SkyFrame (always absolute) */
  AstSkyFrame *oskyfrm = NULL; /* SkyFrame from the output WCS Frameset */
  HDSLoc *mapcoordloc=NULL;    /* HDS locator to the MAPCOORD extension */
  HDSLoc *tloc=NULL;           /* Temporary HDS locator */
  dim_t lbnd[1];               /* Pixel bounds for 1d pointing array */
  dim_t lbnd_old[2];           /* Pixel bounds for existing LUT */
  dim_t lbnd_temp[1];          /* Bounds for bounds NDF component */
  dim_t nbolo=0;               /* Number of bolometers */
  dim_t ndata=0;               /* Number of samples */
  dim_t ntslice=0;             /* Number of time slices */
  dim_t onmap;                 /* Number of samples that fall within the map */
  dim_t step;                  /* step size for dividing up work */
  dim_t tstep;                 /* Time slices between full Mapping calculations */
  dim_t ubnd[1];               /* Pixel bounds for 1d pointing array */
  dim_t ubnd_old[2];           /* Pixel bounds for existing LUT */
  dim_t ubnd_temp[1];          /* Bounds for bounds NDF component */
  double *lat_ptr = NULL;      /* Pointer to array to receive lat values */
  double *lon_ptr = NULL;      /* Pointer to array to receive lon values */
  double *theta = NULL;        /* Scan direction at each time slice */
  int *data_index;             /* Mapped DATA_ARRAY part of NDF */
  int *lut = NULL;             /* The lookup table */
  int bndndf=NDF__NOID;        /* NDF identifier for map bounds */
  int docalc=1;                /* If set calculate the LUT */
  int doextension=0;           /* Try to write LUT to MAPCOORD extension */
  int exportlonlat;            /* Dump longitude and latitude values? */
  int ii;                      /* loop counter */
  int indf_lat = NDF__NOID;    /* Identifier for NDF to receive lat values */
  int indf_lon = NDF__NOID;    /* Identifier for NDF to receive lon values */
  int lutndf=NDF__NOID;        /* NDF identifier for coordinates */
  int nw;                      /* Number of worker threads */
  int there;                   /* Does component exist? */
  size_t nmap;                 /* Number of mapped elements */
  smfCalcMapcoordData *job_data=NULL; /* Array of job */
  smfCalcMapcoordData *pdata=NULL; /* Pointer to job data */
  smfFile *file=NULL;          /* smfFile pointer */
  void *data_pntr[1];          /* Array of pointers to mapped arrays in ndf */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Initialize bounds to avoid compiler warnings */
  lbnd_old[0] = 0;
  lbnd_old[1] = 0;
  ubnd_old[0] = 0;
  ubnd_old[1] = 0;

  /* Check for pre-existing LUT and de-allocate it. This will only waste
     time if the MAPCOORD extension is found to be valid and it has
     to be re-loaded from disk. */
  smf_close_mapcoord( data, status );

  /* Assert ICD data order */
  smf_dataOrder( wf, data, 1, status );

  /* Get the data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, NULL, NULL, status );

  /* If SMF__NOCREATE_FILE is not set, and file associated with an NDF,
     map a new MAPCOORD extension (or verify an existing one) */

  if( !(flags & SMF__NOCREATE_FILE) && data->file ) {
    doextension = 1;
  } else {
    doextension = 0;
    docalc = 1;
  }

  /* Create / check for existing MAPCOORD extension */
  if( doextension ) {
    file = data->file;

    /* Check type of file before proceeding */
    if( file->isSc2store ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,
             "File was opened by sc2store library (raw data?)",
             status);
    }

    if( !file->isTstream ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,	"File does not contain time stream data",status);
    }

    /* Get HDS locator to the MAPCOORD extension */
    mapcoordloc = smf_get_xloc( data, "MAPCOORD", "MAP_PROJECTION", "UPDATE",
                                0, 0, status );

    /* Obtain NDF identifier/placeholder for LUT in MAPCOORD extension*/
    lbnd[0] = 0;
    ubnd[0] = nbolo*ntslice-1;
    lutndf = smf_get_ndfid( mapcoordloc, "LUT", "UPDATE", "UNKNOWN",
                            "_INTEGER", 1, lbnd, ubnd, status );

    if( *status == SAI__OK ) {
      /* store the NDF identifier */
      file->mapcoordid = lutndf;

      /* Create sky to output grid mapping using the base coordinates to
         get the coordinates of the tangent point if it hasn't been done
         yet. */
      sky2map = astGetMapping( outfset, AST__CURRENT, AST__BASE );
    }

    /* Before mapping the LUT, first check for existing WCS information
       and LBND/UBND for the output map. If they are already correct don't
       bother re-calculating the LUT! */

    if( *status == SAI__OK ) {

      /* Try reading in the WCS information */
      kpg1Wread( mapcoordloc, "WCS", &fstemp, status );
      oldfset = (AstFrameSet*)fstemp;

      if( *status == SAI__OK ) {

        /* Check that the old and new mappings are the same by
           checking that combining one with the inverse of the other
           reduces to a UnitMap. */

        map2sky_old = astGetMapping( oldfset, AST__BASE, AST__CURRENT );
        testcmpmap = astCmpMap( map2sky_old, sky2map, 1, " " );
        testsimpmap = astSimplify( testcmpmap );

        if( astIsAUnitMap( testsimpmap ) ) {

          /* The mappings are the same, now just check the pixel
             bounds in the output map */

          lbnd_temp[0] = 1;
          ubnd_temp[0] = 2;

          bndndf = smf_get_ndfid( mapcoordloc, "LBND", "READ", "UNKNOWN",
                                  "_INTEGER", 1, lbnd_temp, ubnd_temp,
                                  status );

          if( *status == SAI__OK ) {
            ndfMap( bndndf, "DATA", "_INTEGER", "READ", data_pntr, &nmap,
                    status );
            data_index = data_pntr[0];

            if( *status == SAI__OK ) {
              lbnd_old[0] = data_index[0];
              lbnd_old[1] = data_index[1];
            }
            ndfAnnul( &bndndf, status );
          }

          bndndf = smf_get_ndfid( mapcoordloc, "UBND", "READ", "UNKNOWN",
                                  "_INTEGER", 1, lbnd_temp, ubnd_temp,
                                  status );

          if( *status == SAI__OK ) {
            ndfMap( bndndf, "DATA", "_INTEGER", "READ", data_pntr, &nmap,
                    status );
            data_index = data_pntr[0];

            if( *status == SAI__OK ) {
              ubnd_old[0] = data_index[0];
              ubnd_old[1] = data_index[1];
            }
            ndfAnnul( &bndndf, status );
          }

          if( *status == SAI__OK ) {
            /* If we get this far finally do the bounds check! */
            if( (lbnd_old[0] == lbnd_out[0]) &&
                (lbnd_old[1] == lbnd_out[1]) &&
                (ubnd_old[0] == ubnd_out[0]) &&
                (ubnd_old[1] == ubnd_out[1]) ) {

              docalc = 0; /* We don't have to re-calculate the LUT */
              msgOutif(MSG__VERB," ",FUNC_NAME ": Existing LUT OK",
                       status);
            }
          }
        }

        /* Bad status / AST errors at this point due to problems with
           MAPCOORD. Annul and continue calculating new MAPCOORD extension. */
        astClearStatus;
        errAnnul(status);

      } else {
        /* Bad status due to non-existence of MAPCOORD. Annul and continue */
        errAnnul(status);
      }
    }

  }

  /* If we need to calculate the LUT do it here */
  if( docalc && (*status == SAI__OK) ) {
    msgOutif(MSG__VERB," ", FUNC_NAME ": Calculate new LUT",
             status);

    /* Get the increment in time slices between full Mapping calculations.
       The Mapping for intermediate time slices will be approximated. */
    tstep = 1;
    if( config ) {
       dim_t dimval;
       smf_get_nsamp( config, "TSTEP", data, &dimval, status );
       tstep = dimval;
    }

    /* Get space for the LUT */
    if( doextension ) {
      /* Map the LUT array */
      ndfMap( lutndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap,
              status );
      data_index = data_pntr[0];
      if( *status == SAI__OK ) {
        lut = data_index;
      } else {
        errRep( FUNC_NAME, "Unable to map LUT in MAPCOORD extension",
                status);
      }
    } else {
      /* alloc the LUT and THETA arrays */
      lut = astMalloc( (nbolo*ntslice)*sizeof(*(data->lut)) );
      theta = astMalloc( ntslice*sizeof(*(data->theta)) );
    }


    /* Retrieve the sky2map mapping from the output frameset (actually
       map2sky) */
    oskyfrm = astGetFrame( outfset, AST__CURRENT );
    sky2map = astGetMapping( outfset, AST__BASE, AST__CURRENT );

    /* If the longitude and latitude is being dumped, create new NDFs to
       hold them, and map them. */
    if( config ) {
       astMapGet0I( config, "EXPORTLONLAT", &exportlonlat );
       if( exportlonlat ) {
          lon_ptr = smf1_calc_mapcoord1( data, nbolo, ntslice, oskyfrm,
                                         &indf_lon, 1, status );
          lat_ptr = smf1_calc_mapcoord1( data, nbolo, ntslice, oskyfrm,
                                         &indf_lat, 2, status );
       }
    }

    /* Invert the mapping to get Output SKY to output map coordinates */
    astInvert( sky2map );

    /* Create a SkyFrame in absolute coordinates */
    abskyfrm = astCopy( oskyfrm );
    astClear( abskyfrm, "SkyRefIs" );
    astClear( abskyfrm, "SkyRef(1)" );
    astClear( abskyfrm, "SkyRef(2)" );

    if( *status == SAI__OK ) {

      /* --- Begin parellelized portion ------------------------------------ */

      /* Initially assume the smfData does not overlap the map. */
      onmap = 0;

      /* Start a new job context. Each call to thrWait within this
         context will wait until all jobs created within the context have
         completed. Jobs created in higher contexts are ignored by thrWait. */
      thrBeginJobContext( wf, status );

      /* Allocate job data for threads */
      job_data = astCalloc( nw, sizeof(*job_data) );
      if( *status == SAI__OK ) {

        /* Set up job data, and start calculating pointing for blocks of
           time slices in different threads */

        if( nw > (int) ntslice ) {
          step = 1;
        } else {
          step = ntslice/nw;
        }

        for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
          pdata = job_data + ii;

          /* Blocks of time slices */
          pdata->t1 = ii*step;
          pdata->t2 = (ii+1)*step-1;

          /* Ensure that the last thread picks up any left-over tslices */
          if( (ii==(nw-1)) && (pdata->t1<(ntslice-1)) ) {
            pdata->t2=ntslice-1;
          }

          pdata->ijob = -1;
          pdata->lut = lut;
          pdata->theta = theta;
          pdata->lbnd_out = lbnd_out;
          pdata->moving = moving;
          pdata->ubnd_out = ubnd_out;
          pdata->tstep = tstep;
          pdata->lat_ptr = lat_ptr;
          pdata->lon_ptr = lon_ptr;
          pdata->fts_port = fts_port;

          /* Make deep copies of AST objects and unlock them so that each
             thread can then lock them for their own exclusive use */

          pdata->abskyfrm = astCopy( abskyfrm );
          astUnlock( pdata->abskyfrm, 1 );
          pdata->sky2map = astCopy( sky2map );
          astUnlock( pdata->sky2map, 1 );

          /* Similarly, make a copy of the smfData, including only the header
             information which each thread will need in order to make calls to
             smf_rebin_totmap */

          pdata->data = smf_deepcopy_smfData( wf, data, 0, SMF__NOCREATE_FILE |
                                              SMF__NOCREATE_DA |
                                              SMF__NOCREATE_FTS |
                                              SMF__NOCREATE_DATA |
                                              SMF__NOCREATE_VARIANCE |
                                              SMF__NOCREATE_QUALITY, 0, 0,
                                              status );
          smf_lock_data( pdata->data, 0, status );
        }

        for( ii=0; ii<nw; ii++ ) {
          /* Submit the job */
          pdata = job_data + ii;
          pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                                     smfCalcMapcoordPar, 0, NULL, status );
        }

        /* Wait until all of the jobs submitted within the current job
           context have completed */
        thrWait( wf, status );

        /* Find the total number of samples that fall within the map. */
        for( ii=0; ii<nw; ii++ ) {
          pdata = job_data + ii;
          onmap += pdata->onmap;
        }

        /* Set a flag that causes the smfData to be ignored if fewer than
           5% of the samples fall within the map. */
        data->onmap = ( onmap > 0.05*ndata );

      }

      /* End the current job context. */
      thrEndJobContext( wf, status );

      /* --- End parellelized portion -------------------------------------- */

      /* Set the lut pointer in data to the buffer */
      data->lut = lut;
      data->theta = theta;

      /* Write the WCS for the projection to the extension */
      if( doextension ) {
        kpg1Wwrt( (AstObject*)outfset, "WCS", mapcoordloc, status );

        /* Write the pixel bounds for the map to the extension */

        lbnd_temp[0] = 1; /* Don't get confused! Bounds for NDF that will */
        ubnd_temp[0] = 2; /* contain the bounds for the output 2d map!    */

        bndndf = smf_get_ndfid( mapcoordloc, "LBND", "UPDATE", "UNKNOWN",
                                "_INTEGER", 1, lbnd_temp, ubnd_temp, status );

        ndfMap( bndndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap,
                status );
        data_index = data_pntr[0];
        if( *status == SAI__OK ) {
          data_index[0] = (int) lbnd_out[0];
          data_index[1] = (int) lbnd_out[1];
        } else {
          errRep( FUNC_NAME, "Unable to map LBND in MAPCOORD extension",
                  status);
        }

        ndfAnnul( &bndndf, status );

        bndndf = smf_get_ndfid( mapcoordloc, "UBND", "UPDATE", "UNKNOWN",
                                "_INTEGER", 1, lbnd_temp, ubnd_temp, status );
        ndfMap( bndndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap,
                status );
        data_index = data_pntr[0];
        if( *status == SAI__OK ) {
          data_index[0] = (int) ubnd_out[0];
          data_index[1] = (int) ubnd_out[1];
        } else {
          errRep( FUNC_NAME, "Unable to map UBND in MAPCOORD extension",
                  status);
        }
        ndfAnnul( &bndndf, status );

        /* Store a flag in the MAPCOORD extension indicating if the LUT
           has any  overlap with the map. */
        datNew0I( mapcoordloc, "ONMAP", status );
        datFind( mapcoordloc, "ONMAP", &tloc, status );
        datPut0I( tloc, data->onmap, status );
        datAnnul( &tloc, status );
      }
    }

  /* Check if the existing LUT overlaps the map, and store a flag in the
     smfData. */
  } else {
     datThere( mapcoordloc, "ONMAP", &there, status );
     if( there ){
        datFind( mapcoordloc, "ONMAP", &tloc, status );
        datGet0I( tloc, &(data->onmap), status );
        datAnnul( &tloc, status );
     } else {
        data->onmap = 1;
     }
  }

  /* Clean Up */

  if( testsimpmap ) testsimpmap = astAnnul( testsimpmap );
  if( testcmpmap ) testcmpmap = astAnnul( testcmpmap );
  if( map2sky_old ) map2sky_old = astAnnul( map2sky_old );
  if( oldfset ) oldfset = astAnnul( oldfset );
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if( abskyfrm ) abskyfrm = astAnnul( abskyfrm );
  if( oskyfrm ) oskyfrm = astAnnul( oskyfrm );
  if( mapcoordloc ) datAnnul( &mapcoordloc, status );
  if( indf_lat != NDF__NOID ) ndfAnnul( &indf_lat, status );
  if( indf_lon != NDF__NOID ) ndfAnnul( &indf_lon, status );


  /* If we get this far, docalc=0, and status is OK, there must be
     a good LUT in there already. Map it so that it is accessible to
     the caller; "UPDATE" so that the caller can modify it if desired. */
  if( (*status == SAI__OK) && (docalc == 0) ) {
    smf_open_mapcoord( data, "UPDATE", status );
  }

  /* Clean up job data */
  if( job_data ) {
    for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
      pdata = job_data + ii;

      if( pdata->data ) {
        smf_lock_data( pdata->data, 1, status );
        smf_close_file( wf, &(pdata->data), status );
      }
      astLock( pdata->abskyfrm, 0 );
      pdata->abskyfrm = astAnnul( pdata->abskyfrm );

      astLock( pdata->sky2map, 0 );
      pdata->sky2map = astAnnul( pdata->sky2map );
    }
    job_data = astFree( job_data );
  }

}


static double *smf1_calc_mapcoord1( smfData *data, dim_t nbolo,
                                    dim_t ntslice, AstSkyFrame *oskyfrm,
                                    int *indf, int axis, int *status ){
/*
*  Name:
*     smf1_calc_mapcoord1

*  Purpose:
*     Create and map an NDF to receive the longitude or latitude values
*     at every sample.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double *smf1_calc_mapcoord1( smfData *data, dim_t nbolo,
*                                  dim_t ntslice, AstFrame *oskyfrm,
*                                  int *indf, int axis, int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct
*     nbolo = dim_t (Given)
*        The number of bolometers.
*     ntslice = dim_t (Given)
*        The number of time slices.
*     oskyfrm = AstFrame * (Given)
*        Pointer to the SkyFrame describing the output spatial cords.
*     indf = int * (Returned)
*        Address ayt which to return the identifier for the new NDF.
*     axis = int (Given)
*        Axis of the SkyFrame to use (1 or 2).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Pointer to the mapped DATA array.

*  Description:
*     This function creates a new NDF with a named formed by appending
*     the axis symbol from oskyframe to the end of the file name associated
*     with the supplied smfData. The firts pixel axis spans bolometer
*     index and the second spans time slice index. The NDF character
*     components are set to describe the requested ais values.

*/

/* Local Variables: */
   char name[SMF_PATH_MAX+1];
   const char *label = NULL;
   const char *ttl = NULL;
   double *result = NULL;
   size_t el;
   int place;
   dim_t pos_lbnd[2];
   dim_t pos_ubnd[2];

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Check the input file path is known. */
   if( data->file ) {

/* Remove any DIMM suffix, and any leading directory from the file path. */
      smf_stripsuffix( data->file->name, SMF__DIMM_SUFFIX, name, status );

/* Get the Frame title, and axis label. */
      ttl = astGetC( oskyfrm, "Title" );
      label = astGetC( oskyfrm, ( axis == 1 ) ? "Label(1)" : "Label(2)" );

/* Append a suitable string to the file base name. */
       if( astGetI( oskyfrm, "LatAxis" ) == axis ) {
          one_strlcat( name, "_lat", SMF_PATH_MAX + 1, status );
       } else {
          one_strlcat( name, "_lon", SMF_PATH_MAX + 1, status );
       }

/* Store the pixel bounds for the NDF. */
       pos_lbnd[ 0 ] = pos_lbnd[ 1 ] = 0;
       pos_ubnd[ 0 ] = nbolo - 1;
       pos_ubnd[ 1 ] = ntslice - 1;

/* Create the NDF and map its Data array. */
       ndfPlace( NULL, name, &place, status );
       ndfNew( "_DOUBLE", 2, pos_lbnd, pos_ubnd, &place, indf, status );
       ndfMap( *indf, "DATA", "_DOUBLE", "WRITE", (void **) &result, &el,
               status );

/* Set the NDF character components. */
       ndfCput( ttl, *indf, "TITLE", status );
       ndfCput( label, *indf, "LABEL", status );
       ndfCput( "deg", *indf, "UNITS", status );
       ndfAcput( "Bolometer index", *indf, "LABEL", 1, status );
       ndfAcput( "Time slice index", *indf, "LABEL", 2, status );
    }

/* Return the pointer to the mapped data array. */
   return result;
}

