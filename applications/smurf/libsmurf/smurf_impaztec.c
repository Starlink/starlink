/*
*+
*  Name:
*     smurf_impaztec

*  Purpose:
*     Import AzTEC NETCDF files and produce SCUBA2 ICD-compliant files

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Use the netcdf library to import raw AzTEC data files and save
*     to NDF files in a format approximating the SCUBA2 ICD so that they
*     may subsequently be read by other SMURF routines to make maps.

*  ADAM Parameters:
*     ...


*  Authors:
*     Mitch Crowe (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-04 (MC):
*        Initial Version
*     2006-08-21 (EC):
*        - First version committed to CVS.
*        - style/formatting changes
*        - use smf memory allocation routines
*        - use JCMTState instead of sc2head struct
*        - modified netcdf error message wrapper
*        - modified calls to ndfwrdata to reflect new interface
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council and the University of British Columbia. All Rights
*     Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "jcmt/state.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "libsc2sim/fhead.h"
#include "libsc2sim/fhead_par.h"
#include "libsc2sim/dream_par.h"
#include "libsc2sim/dream.h"
#include "libsc2sim/dxml_struct.h"
#include "libsc2sim/sc2sim_par.h"
#include "libsc2sim/sc2sim_struct.h"
#include "libsc2sim/sc2sim.h"

/* netCDF includes */
#ifdef HAVE_LIBNETCDF
#include "netcdf.h"
#endif

#define FUNC_NAME "smurf_impaztec"
#define TASK_NAME "IMPAZTEC"

#define MAXSTRING 256

#ifdef HAVE_LIBNETCDF
/* Prototypes for local functions that wrap netcdf messages */
void nc_getSignal(int ncid, char* signalname, double* signal, int* status);
void nc_error(int nc_status, int *status);
#endif

void smurf_impaztec( int *status ) {
 
  /* Local Variables */

  double *bolosig=NULL;        /* holder for a single bolometer signal */
  double *full_bolosig=NULL;   /* all bolo signals [NBOLOSxNFRAMES] */
  JCMTState *frhead=NULL;      /* Pointer to SCUBA2 frame struct */
  double *t_signal=NULL;       /* Timestream signal holder */
  char ncfile[MAXSTRING];      /* Input NetCDF file name */
  char ndffile[MAXSTRING];     /* Output NDF file name */
  size_t nbolos;               /* Number of bolometers in netCDF data format */
  size_t nframes;              /* Number of time steps in netCDF data format */
  int ncid;                    /* ID of netCDF file */
  int varid_h1b1;              /* NetCDF variable id of bolo h1b1 signal */
  dim_t i,j;                   /* Loop counters */

  double ra;                   /* RA of observation in radians  */
  double dec;                  /* Dec of observation in radians  */
  int add_atm;                 /* flag for adding atmospheric emission  */
  int add_fnoise;              /* flag for adding 1/f noise  */
  int add_pns;                 /* flag for adding photon noise  */
  int flux2cur;                /* flag for converting flux to current  */
  double amstart;              /* Airmass at beginning  */
  double amend;                /* Airmass at end  */
  double meanwvm;              /* 225 GHz tau */
  double obslam;               /* Wavelength */
  int ncol;                    /* number of bolometers in column  */
  int nrow;                    /* number of bolometers in row  */
  double sample_t;             /* sample interval in msec  */
  int numsamples;              /* number of samples  */
  int nflat;                   /* number of flat coeffs per bol  */
  struct JCMTState *head;      /* header data for each frame  */
  int *dbuf=NULL;              /* simulated data  */
  int *dksquid=NULL;           /* dark SQUID time stream data  */
  double *fcal=NULL;           /* flatfield calibration  */
  double *fpar=NULL;           /* flat-field parameters  */
  double atstart;	       /* Ambient temperature at start (Celsius)  */
  double atend;		       /* Ambient temperature at end (Celsius)  */
  double *posptr=NULL;         /* Pointing offsets from map centre */
  char *obsmode=NULL;          /* Observing mode */
  
  struct dxml_struct inx;      /* Structures required for ndfwrdata calls */ 
  struct dxml_sim_struct sinx; /* Structures required for ndfwrdata calls */ 

  /* Main routine */

#ifdef HAVE_LIBNETCDF

  /* get the user defined input and output file names */
  parGet0c( "IN", ncfile, MAXSTRING, status);
  parGet0c( "OUT", ndffile, MAXSTRING, status);
  
  if( *status == SAI__OK ) {
    /* open the netCDF file and check for errors */
    nc_error( nc_open(ncfile,NC_NOWRITE,&ncid), status );
  }

  if( *status == SAI__OK ) {

    msgOutif(MSG__VERB, FUNC_NAME, 
	     "Reading netcdf file", status); 

    /* get the data dimensionality */
    nc_inq_dimlen(ncid,0,&nbolos);    
    size_t framespersecond, seconds;
    nc_inq_dimlen(ncid,3,&framespersecond);
    nc_inq_dimlen(ncid,4,&seconds);
    nframes = framespersecond*seconds; 

    /* get the bolometer signals */
    bolosig = smf_malloc( nframes, sizeof( *bolosig ), 0, status );
    full_bolosig = smf_malloc( nbolos*nframes, sizeof( *full_bolosig ), 0, 
			       status );

    nc_inq_varid(ncid, "h1b1",&varid_h1b1);

    for(i=0; i<nbolos ; i++){
      nc_error( nc_get_var_double(ncid,varid_h1b1+i,bolosig), status );
      for(j=0;j<nframes;j++){
	full_bolosig[i + j*nbolos] = bolosig[j];
      }
    }

    /* create and fill the header with rts/tcs data */
    frhead = smf_malloc( nframes, sizeof(*frhead), 1, status );
    t_signal = smf_malloc( nframes, sizeof(*t_signal), 1, status );

    nc_getSignal(ncid,"time",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].rts_end = (double)t_signal[i];
  
    nc_getSignal(ncid,"jcmt_airmass",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_airmass = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_trackActC1",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_ac1 = t_signal[i];  
  
    nc_getSignal(ncid,"jcmt_trackActC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_ac2 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_trackDemandC1",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_dc1 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_trackDemandC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_dc2 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_trackBaseC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_bc1 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_trackBaseC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_tr_bc2 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElActC1",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_ac1 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElActC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_ac2 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElDemandC1",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_dc1 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElDemandC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_dc2 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElBaseC1",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_bc1 = t_signal[i];
  
    nc_getSignal(ncid,"jcmt_azElBaseC2",t_signal,status);
    for(i=0;i<nframes;i++) frhead[i].tcs_az_bc2 = t_signal[i];
  

    /* fill ndf data */
    char ra_str[MAXSTRING];
    char dec_str[MAXSTRING];
    nc_get_att_text(ncid,NC_GLOBAL,"jcmt_header_C1",ra_str);
    ra = strtod(ra_str,NULL);
    nc_get_att_text(ncid,NC_GLOBAL,"jcmt_header_C2",dec_str);
    dec = strtod(dec_str,NULL);

    add_atm    = 0;
    add_fnoise = 0;
    add_pns    = 0;
    flux2cur   = 0;
    amstart = frhead[0].tcs_airmass;
    amend = frhead[nframes-1].tcs_airmass;
    meanwvm    = 0; /* fix */
    obslam     = 0;
    ncol       = nbolos;
    nrow       = 1;
    sample_t   = 0.015625; /* AzTEC sample time */
    numsamples = nframes;
    nflat      = 1;
    head       = frhead;

    dbuf = smf_malloc( nframes*nbolos, sizeof(*dbuf), 0, status );
    double digmean = ((double)pow(2,24))/2; /* mean of parameterisation */
    double digscale = 1000000;	   /* scale of digits */
    double digcurrent = 1.2;
    sc2sim_digitise(nframes*nbolos, full_bolosig, digmean, digscale, 
		    digcurrent, dbuf, status);

    dksquid = smf_malloc( nframes*nbolos, sizeof(*dksquid), 0, status );
    for(i=0;i<nframes;i++) dksquid[i] = 0;
    
    fcal = smf_malloc( nbolos, sizeof(*fcal), 0, status );
    for(i=0;i<nbolos;i++) fcal[i] = 1;
    
    fpar = smf_malloc( nflat, sizeof(*fpar), 0, status );
    for(i=0;i<nflat;i++) fpar[i] = 1;

    atstart    = 0;
    atend      = 0;
    posptr = smf_malloc( 2*nframes, sizeof(*posptr), 0, status );
    for(i=0;i<nframes;i++) {
      posptr[2*i +0] = frhead[i].tcs_az_ac1;
      posptr[2*i +1] = frhead[i].tcs_az_ac2;
    }
    obsmode = "";		   /* fix */

    /* Fill the inx and sinx arrays */
    inx.ra = ra;
    inx.dec = dec;
    inx.nbolx = ncol;
    inx.nboly = nrow;
    inx.sample_t = sample_t; 
    strncpy( inx.obsmode, obsmode, sizeof(inx.obsmode)-1 );
    
    sinx.add_atm = 0;
    sinx.add_fnoise = 0;
    sinx.add_pns = 0;
    sinx.flux2cur = 0;
    strncpy( sinx.subname, "AZTEC", sizeof(sinx.subname)-1 );
    sinx.airmass = (amstart + amend)/2.;
    sinx.atstart = atstart;
    sinx.atend = atend;
    
  }

  msgOutif(MSG__VERB, FUNC_NAME, 
	   "Writing NDF file", status); 

  /* write the ndf file */
  sc2sim_ndfwrdata( &inx, &sinx, meanwvm, ndffile, numsamples, nflat, 
		    "POLYNOMIAL", head, dbuf, dksquid, fcal, fpar, "1100",
		    posptr, 0, NULL, status ); 
		   
  /* call using the old ndfwrdata interface

    ra, dec, add_atm, add_fnoise, add_pns, flux2cur, amstart,
    amend, meanwvm, obslam, ndffile, ncol, nrow, sample_t, 
    "AZTEC", numsamples, nflat, "POLYNOMIAL", head, dbuf, 
    dksquid, fcal, fpar, "1100", atstart, atend, posptr, 
    obsmode, status);
  */

  /* close the netCDF file and check for errors */
  
  if( *status == SAI__OK ) {
    nc_error( nc_close(ncid), status );
  }

  /* Free memory */
  
  smf_free( bolosig, status );
  smf_free( full_bolosig, status ); 
  smf_free( frhead, status );
  smf_free( t_signal, status );
  smf_free( dbuf, status );
  smf_free( dksquid, status );
  smf_free( fcal, status );
  smf_free( fpar, status );
  smf_free( posptr, status );

#else

  *status = SAI__ERROR;
  errRep(FUNC_NAME, 
	 "SMURF built without libnetcdf. IMPAZTEC task not supported.", 
	 status);

#endif


  return;
}


#ifdef HAVE_LIBNETCDF

/* get a signal specified by name from the netCDF file identified by ncid */
void nc_getSignal(int ncid, char* signalname, double* signal,int* status){

  int sigid, nc_status;
  nc_inq_varid(ncid,signalname, &sigid);
  nc_status = nc_get_var_double(ncid,sigid,signal);
  if(nc_status != NC_NOERR){
    nc_error(nc_status,status);
  }
  return;
}

/* handle netCDF status errors. terminate if we have any errors */
void nc_error(int nc_status,int* status){

  if(nc_status != NC_NOERR){
    msgSetc("ERROR",nc_strerror(nc_status));
    *status = SAI__ERROR;
    errRep(FUNC_NAME,"libnetcdf reported: ^ERROR",status);
  }
  return;
}

#endif
