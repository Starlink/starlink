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
*     smurf_impaztec( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Use the netcdf library to import raw AzTEC data files and save
*     to NDF files in a format approximating the SCUBA2 ICD so that they
*     may subsequently be read by other SMURF routines to make maps.

*  ADAM Parameters:
*     IN = CHAR (Read)
*          Input NETCDF file
*     OUT = CHAR (Read)
*          Output NDF file

*  Authors:
*     Mitch Crowe (UBC)
*     Edward Chapin (UBC)
*     Jen Balfour (UBC)
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
*     2006-08-31 (JB)
*        Added modified julian date conversion
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
#include "star/slalib.h"

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

/* prototype for slalib routine that calculates calendar date -> njd */
void slaCaldj ( int iy, int im, int id, double *djm, int *j );

#endif

void smurf_impaztec( int *status ) {
 
  /* Local Variables */

  int add_atm;                 /* flag for adding atmospheric emission  */
  int add_fnoise;              /* flag for adding 1/f noise  */
  int add_pns;                 /* flag for adding photon noise  */
  double *airmass = NULL;      /* airmass of each frame */
  double amend;                /* airmass at end  */
  double amstart;              /* airmass at beginning  */
  double atend;		       /* ambient temperature at end (Celsius)  */
  double atstart;	       /* ambient temperature at start (Celsius)  */
  double *azelactc1 = NULL;    /* arrays for storing per-frame header data */
  double *azelactc2 = NULL;
  double *azeldemandc1 = NULL;
  double *azeldemandc2 = NULL;
  double *azelbasec1 = NULL;
  double *azelbasec2 = NULL; 
  double *bolosig=NULL;        /* holder for a single bolometer signal */
  char *curtok = NULL;         /* string tokenizer */
  int date_status;             /* status of date conversion */
  char date_str[MAXSTRING];    /* string rep. of date (MM/DD/YYYY) */
  int day;                     /* day of beginning of observation */
  int *dbuf=NULL;              /* simulated data  */
  double dec;                  /* dec of observation in radians  */
  char dec_str[MAXSTRING];     /* string rep. of dec */
  double digcurrent;           /* digitisation mean current */
  double digmean;              /* digitisation mean value */
  double digscale;             /* digitisation scale factore */
  double djm;                  /* modified julian start date */
  int *dksquid=NULL;           /* dark SQUID time stream data  */
  double *fcal=NULL;           /* flatfield calibration  */
  int flux2cur;                /* flag for converting flux to current  */
  double *fpar=NULL;           /* flat-field parameters  */
  size_t framespersecond;      /* frames per second */
  double *full_bolosig=NULL;   /* all bolo signals [NBOLOSxNFRAMES] */
  struct JCMTState *head;      /* header data for each frame  */
  dim_t i,j;                   /* loop counters */
  double meanwvm;              /* 225 GHz tau */
  double *mjuldate=NULL;       /* modified Julian date each sample */
  int month;                   /* month of beginning of observation */
  size_t nbolos;               /* number of bolometers in netCDF data format */
  char ncfile[MAXSTRING];      /* input NetCDF file name */
  int ncid;                    /* id of netCDF file */
  int ncol;                    /* number of bolometers in column  */
  char ndffile[MAXSTRING];     /* output NDF file name */
  int nflat;                   /* number of flat coeffs per bol  */
  size_t nframes;              /* number of time steps in netCDF data format */
  int nrow;                    /* number of bolometers in row */
  int numsamples;              /* number of samples  */
  double obslam;               /* wavelength */
  char *obsmode=NULL;          /* observing mode */
  double *posptr=NULL;         /* pointing offsets from map centre */
  double ra;                   /* ra of observation in radians  */
  char ra_str[MAXSTRING];      /* string rep. of ra */
  double sample_t;             /* sample interval in msec  */
  size_t seconds;              /* seconds in observation */
  double startmidtime;         /* seconds since midnight */
  double startnoontime;        /* seconds since noon */
  double *tempbuff = NULL;     /* throwaway buffer for using calctime */
  double *time = NULL;         /* arrays for storing per-frame header data */
  double *trackactc1 = NULL;
  double *trackactc2 = NULL;
  double *trackbasec1 = NULL;
  double *trackbasec2 = NULL;
  double *trackdemandc1 = NULL;
  double *trackdemandc2 = NULL;
  int varid_h1b1;              /* netCDF variable id of bolo h1b1 signal */
  int yr;                      /* year of beginning of observation */
 
  struct dxml_struct inx;      /* structures required for ndfwrdata calls */ 
  struct dxml_sim_struct sinx; /* structures required for ndfwrdata calls */ 

  /* Main routine */

#ifdef HAVE_LIBNETCDF

  /* Get the user defined input and output file names */
  parGet0c( "IN", ncfile, MAXSTRING, status);
  parGet0c( "OUT", ndffile, MAXSTRING, status);
  
  if( *status == SAI__OK ) {
    /* Open the netCDF file and check for errors */
    nc_error( nc_open(ncfile,NC_NOWRITE,&ncid), status );
  }

  if( *status == SAI__OK ) {

    /* Preset some required values */
    add_atm    = 0;
    add_fnoise = 0;
    add_pns    = 0;
    flux2cur   = 0;
    meanwvm    = 0; /* fix */
    obslam     = 0;
    nrow       = 1;
    sample_t   = 0.015625; /* AzTEC sample time */
    nflat      = 1;
    atstart    = 0;
    atend      = 0;
    obsmode = "";		   /* fix */

    /* Kludge for now - assign values for digitisation */
    digmean = ((double)pow(2,24))/2;
    digscale = 1000000;	
    digcurrent = 1.2;

    msgOutif(MSG__VERB, FUNC_NAME, 
	     "Reading netcdf file", status); 

    /* Get the data dimensionality */
    nc_inq_dimlen(ncid,0,&nbolos);    
    nc_inq_dimlen(ncid,3,&framespersecond);
    nc_inq_dimlen(ncid,4,&seconds);
    nframes = framespersecond*seconds; 

    /* Allocate memory for the bolometer signals */
    bolosig = smf_malloc( nframes, sizeof( *bolosig ), 0, status );
    full_bolosig = smf_malloc( nbolos*nframes, sizeof( *full_bolosig ), 0, 
			       status );

    /* Get the bolometer signals */
    nc_inq_varid(ncid, "h1b1",&varid_h1b1);

    for(i=0; i<nbolos ; i++){
      nc_error( nc_get_var_double(ncid,varid_h1b1+i,bolosig), status );
      for(j=0;j<nframes;j++){
	full_bolosig[j*nbolos + i] = bolosig[j];
      }
    }

    /* create and fill the header with rts/tcs data */
    head = smf_malloc( nframes, sizeof(*head), 1, status );

    /* Retrieve values to convert from the netcdf file */

    time = smf_malloc ( nframes, sizeof(*time), 1, status );  
    airmass = smf_malloc ( nframes, sizeof(*airmass), 1, status );  
    trackactc1 = smf_malloc ( nframes, sizeof(*trackactc1), 1, status );  
    trackactc2 = smf_malloc ( nframes, sizeof(*trackactc2), 1, status );  
    trackdemandc1 = smf_malloc ( nframes, sizeof(*trackdemandc1), 1, status );  
    trackdemandc2 = smf_malloc ( nframes, sizeof(*trackdemandc2), 1, status );  
    trackbasec1 = smf_malloc ( nframes, sizeof(*trackbasec1), 1, status );  
    trackbasec2 = smf_malloc ( nframes, sizeof(*trackbasec2), 1, status );  
    azelactc1 = smf_malloc ( nframes, sizeof(*azelactc1), 1, status );  
    azelactc2 = smf_malloc ( nframes, sizeof(*azelactc2), 1, status );  
    azeldemandc1 = smf_malloc ( nframes, sizeof(*azeldemandc1), 1, status );  
    azeldemandc2 = smf_malloc ( nframes, sizeof(*azeldemandc2), 1, status );  
    azelbasec1 = smf_malloc ( nframes, sizeof(*azelbasec1), 1, status );  
    azelbasec2 = smf_malloc ( nframes, sizeof(*azelbasec2), 1, status );

    dbuf = smf_malloc( nframes*nbolos, sizeof(*dbuf), 0, status );
    dksquid = smf_malloc( nframes*nbolos, sizeof(*dksquid), 0, status ); 
    posptr = smf_malloc( 2*nframes, sizeof(*posptr), 0, status );   
    fcal = smf_malloc( nbolos, sizeof(*fcal), 0, status );
    fpar = smf_malloc( nflat, sizeof(*fpar), 0, status );
    mjuldate = smf_malloc ( nframes, sizeof(*mjuldate), 1, status );
    tempbuff = smf_malloc ( nframes, sizeof(*tempbuff), 1, status );

    /* Calculate the time for each frame.  First, get the modified julian
       date (day) from the "date" attribute of the NETCDF file.  Then, 
       get the time of the first frame and convert it from 'seconds from
       midnight' to 'seconds from noon' by subtracting or adding fractions
       of a modified julian day as necessary. */

    /* Get the month, day, and year */
    nc_get_att_text ( ncid, NC_GLOBAL, "date", date_str );
    curtok = strtok ( date_str, "/");
    month = atoi ( curtok );
    curtok = strtok ( NULL, "/" );
    day = atoi ( curtok );
    curtok = strtok ( NULL, "/" );
    yr = atoi ( curtok );

    /* Calculate the base modified julian date */
    slaCaldj ( yr, month, day, &djm, &date_status );

    /* Retrieve the time for each frame */
    nc_getSignal ( ncid, "time", time, status );

    /* Adjust the mjd for the time */
    startmidtime = time[0];
    if ( startmidtime < 43200 ) {
       djm = djm - 1;
       startnoontime = 42300 + startmidtime;
       djm = djm + startnoontime / 86400;
    } else {
       startnoontime = startmidtime - 43200;
       djm = djm + startnoontime / 86400;
    }

    sc2sim_calctime( djm, sample_t, nframes,
                     mjuldate, tempbuff, status );       
    
    nc_getSignal ( ncid, "jcmt_airmass", airmass, status );
    nc_getSignal ( ncid, "jcmt_trackActC1", trackactc1, status );
    nc_getSignal ( ncid, "jcmt_trackActC2", trackactc2, status );
    nc_getSignal ( ncid, "jcmt_trackDemandC1", trackdemandc1, status );
    nc_getSignal ( ncid, "jcmt_trackDemandC1", trackdemandc1, status );
    nc_getSignal ( ncid, "jcmt_trackBaseC1", trackbasec1, status );
    nc_getSignal ( ncid, "jcmt_trackBaseC2", trackbasec2, status );
    nc_getSignal ( ncid, "jcmt_azElActC1", azelactc1, status );
    nc_getSignal ( ncid, "jcmt_azElActC2", azelactc2, status );
    nc_getSignal ( ncid, "jcmt_azElDemandC1", azeldemandc1, status);
    nc_getSignal ( ncid, "jcmt_azElDemandC2", azeldemandc2, status);
    nc_getSignal ( ncid, "jcmt_azElBaseC1", azelbasec1, status );
    nc_getSignal ( ncid, "jcmt_azElBaseC2", azelbasec2, status );

    for ( i = 0; i < nframes; i++ ) {
      
       head[i].rts_num = i;   
       head[i].rts_end = mjuldate[i];
       head[i].tcs_airmass = airmass[i];
       head[i].tcs_tr_ac1 = trackactc1[i];
       head[i].tcs_tr_ac2 = trackactc2[i];
       head[i].tcs_tr_dc1 = trackdemandc1[i];
       head[i].tcs_tr_dc2 = trackdemandc2[i];
       head[i].tcs_tr_bc1 = trackbasec1[i];
       head[i].tcs_tr_bc2 = trackbasec2[i];
       head[i].tcs_az_ac1 = azelactc1[i];
       head[i].tcs_az_ac2 = azelactc2[i];  
       head[i].tcs_az_dc1 = azeldemandc1[i];
       head[i].tcs_az_dc2 = azeldemandc2[i];
       head[i].tcs_az_bc1 = azelbasec1[i];
       head[i].tcs_az_bc2 = azelbasec2[i]; 

       dksquid[i] = 0; 

       posptr[2*i +0] = azelactc1[i];
       posptr[2*i +1] = azelactc2[i];

    } 

    amstart = head[0].tcs_airmass;
    amend = head[nframes-1].tcs_airmass;
    numsamples = nframes;
    ncol = nbolos;

    for(i=0;i<nbolos;i++) fcal[i] = 1;
    for(i=0;i<nflat;i++) fpar[i] = 1; 

    /* fill ndf data */
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C1", ra_str );
    ra = strtod ( ra_str, NULL );
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C2", dec_str );
    dec = strtod ( dec_str, NULL );

    sc2sim_digitise ( nframes*nbolos, full_bolosig, digmean, digscale, 
		      digcurrent, dbuf, status );


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
		    "POLYNOMIAL", head, dbuf, dksquid, fcal, fpar, 
		    "AZTEC", "1100",
		    posptr, 0, NULL, status ); 
		   
  /* call using the old ndfwrdata interface

    ra, dec, add_atm, add_fnoise, add_pns, flux2cur, amstart,
    amend, meanwvm, obslam, ndffile, ncol, nrow, sample_t, 
    "AZTEC", numsamples, nflat, "POLYNOMIAL", head, dbuf, 
    dksquid, fcal, fpar, "1100", atstart, atend, posptr, 
    obsmode, status);
  */

  /* close the netCDF file and check for errors */
  
  if ( *status == SAI__OK ) {
    nc_error ( nc_close(ncid), status );
  }

  /* Free memory */
  
  smf_free ( bolosig, status );
  smf_free ( full_bolosig, status ); 
  smf_free ( head, status );
  smf_free ( dbuf, status );
  smf_free ( dksquid, status );
  smf_free ( fcal, status );
  smf_free ( fpar, status );
  smf_free ( posptr, status );
  smf_free ( mjuldate, status );
  smf_free ( tempbuff, status );

  smf_free ( time, status );
  smf_free ( airmass, status );
  smf_free ( trackactc1, status );
  smf_free ( trackactc2, status );
  smf_free ( trackdemandc1, status );
  smf_free ( trackdemandc2, status );
  smf_free ( trackbasec1, status );
  smf_free ( trackbasec2, status );  
  smf_free ( azelactc1, status );
  smf_free ( azelactc2, status );
  smf_free ( azeldemandc1, status );
  smf_free ( azeldemandc2, status );
  smf_free ( azelbasec1, status );
  smf_free ( azelbasec2, status );

#else

  *status = SAI__ERROR;
  errRep(FUNC_NAME, 
	 "SMURF built without libnetcdf. IMPAZTEC task not supported.", 
	 status);

#endif

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB, FUNC_NAME, 
	   "Impaztec complete, NDF file written", status); 
  }

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
