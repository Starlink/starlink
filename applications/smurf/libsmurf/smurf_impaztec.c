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
*     2006-09-06 (EC)
*        - Modified ndfwrdata call to include INSTRUME keyword
*        - Pass telescope coordinates to sc2sim_calctime
*     2006-09-11 (EC):
*        Fixed pointer problem with callc to smf_calc_telpos
*     2006-09-12 (EC):
*        Use direct sc2store and ndf calls instead of sc2sim_ndfwrdata
*     2006-09-22 (JB):
*        Removed dream & dxml includes

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
#include "fitsio.h"
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
  double dec=0;                /* dec of observation in radians  */
  char dec_str[MAXSTRING];     /* string rep. of dec */
  double djm;                  /* modified julian start date */
  int *dksquid=NULL;           /* dark SQUID time stream data  */
  double *fcal=NULL;           /* flatfield calibration  */
  double *fpar=NULL;           /* flat-field parameters  */
  size_t framespersecond;      /* frames per second */
  double *full_bolosig=NULL;   /* all bolo signals [NBOLOSxNFRAMES] */
  struct JCMTState *head=NULL; /* header data for each frame  */
  dim_t i,j;                   /* loop counters */
  int k;
  double meanwvm;              /* 225 GHz tau */
  double *mjuldate=NULL;       /* modified Julian date each sample */
  int month;                   /* month of beginning of observation */
  size_t nbolos;               /* number of bolometers in netCDF data format */
  char ncfile[MAXSTRING];      /* input NetCDF file name */
  int ncid;                    /* id of netCDF file */
  int ncol;                    /* number of bolometers in column  */
  char ndffile[MAXSTRING];     /* output NDF file name */
  int nflat;                   /* number of flat coeffs per bol  */
  int nframes;                 /* number of time steps in netCDF data format */
  int nrow;                    /* number of bolometers in row */
  int numsamples;              /* number of samples  */
  double obslam;               /* wavelength */
  double *posptr=NULL;         /* pointing offsets from map centre */
  double ra=0;                 /* ra of observation in radians  */
  char ra_str[MAXSTRING];      /* string rep. of ra */
  double sample_t;             /* sample interval in msec  */
  size_t seconds;              /* seconds in observation */
  double startmidtime;         /* seconds since midnight */
  double startnoontime;        /* seconds since noon */
  double telpos[3];            /* Geodetic location of the telescope */
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


  double decd;                     /* Dec of observation in degrees */
  static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
  int nrec;                        /* number of FITS header records */
  double rad;                      /* RA of observation in degrees */
  double map_hght;                 /* Map height in arcsec */
  double map_wdth;                 /* Map width in arcsec  */
  double map_pa;                   /* Map PA in degrees  */
  double map_x = 0;                /* Map X offset in arcsec */
  double map_y = 0;                /* Map Y offset in arcsec */
  double x_min = 0;                /* Maximum extent of pointing offsets */
  double x_max = 0;
  double y_min = 0;
  double y_max = 0;

  int lbnd[3];                     /* Dimensions of the DATA component */
  int ubnd[3];                     /*   "               "       "      */
  int framesize=0;                 /* # data points per timeslice (nbolos) */
  int place=0;                     /* NDF placeholder */
  int indf=0;                      /* NDF id for the file */
  int nmap=0;                      /* Number of elements mapped */
  HDSLoc *jcmtstateloc = NULL;     /* HDS locator to JCMTSTATE structure */
  void *pntr=NULL;                 /* Temporary pointer */

  HDSLoc *fitsloc = NULL;    /* HDS locator to FITS headers */
  HDSLoc *loc2 = NULL;       /* HDS locator */
  

 
  /* Main routine */

#ifdef HAVE_LIBNETCDF

  /* Get the LON/LAT of JCMT */
  smf_calc_telpos( NULL, "JCMT", telpos, status );

  /* Get the user defined input and output file names */
  parGet0c( "IN", ncfile, MAXSTRING, status);
  parGet0c( "OUT", ndffile, MAXSTRING, status);

  if( *status == SAI__OK ) {
    /* Open the netCDF file and check for errors */
    nc_error( nc_open(ncfile,NC_NOWRITE,&ncid), status );
  }

  if( *status == SAI__OK ) {

    /* Preset some required values */
    meanwvm    = 0; /* fix */
    obslam     = 0;
    nrow       = 1;
    sample_t   = 0.015625; /* AzTEC sample time */
    nflat      = 1;
    atstart    = 0;
    atend      = 0;

    msgOutif(MSG__VERB, FUNC_NAME, 
	     "Reading netcdf file", status); 

    /* Get the data dimensionality */
    nc_inq_dimlen(ncid,0,&nbolos);    
    nc_inq_dimlen(ncid,3,&framespersecond);
    nc_inq_dimlen(ncid,4,&seconds);
    nframes = framespersecond*seconds; 

    /* Allocate temporary memory for the bolometer signals */
    bolosig = smf_malloc( nframes, sizeof( *bolosig ), 0, status );

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

    /* Use simulator routine to calculate array of UT for each timeslice */
    sc2sim_calctime( telpos[0]*DD2R, djm, sample_t, nframes,
                     mjuldate, tempbuff, status );       

    /* RA + Dec at centre of map */
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C1", ra_str );
    ra = strtod ( ra_str, NULL );
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C2", dec_str );
    dec = strtod ( dec_str, NULL );

    

    /* Calculate the JCMTState at each timeslice */

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
      head[i].tcs_tr_bc1 = trackactc1[0]; /* trackbasec1[i]; strange values? */
      head[i].tcs_tr_bc2 = trackactc2[0]; /* trackbasec2[i]; strange values? */
      head[i].tcs_az_ac1 = azelactc1[i];
      head[i].tcs_az_ac2 = azelactc2[i];  
      head[i].tcs_az_dc1 = azeldemandc1[i];
      head[i].tcs_az_dc2 = azeldemandc2[i];
      head[i].tcs_az_bc1 = azelbasec1[i];
      head[i].tcs_az_bc2 = azelbasec2[i]; 
      
      posptr[2*i +0] = azelactc1[i];
      posptr[2*i +1] = azelactc2[i];
      
    } 

    /* Additional FITS header information */

    amstart = head[0].tcs_airmass;
    amend = head[nframes-1].tcs_airmass;
    numsamples = nframes;
    ncol = nbolos;

  }

  msgOutif(MSG__VERB, FUNC_NAME, 
	   "Writing NDF file", status); 

  ndfBegin();

  /* Create HDS container file */
  ndfPlace ( NULL, ndffile, &place, status );

  /* Create an NDF inside the container */
  framesize = ncol * nrow;
  ubnd[0] = ncol;
  lbnd[0] = 1;
  ubnd[1] = nrow;
  lbnd[1] = 1;
  ubnd[2] = nframes;
  lbnd[2] = 1;
  
  ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &indf, status );
  ndfHcre ( indf, status );

  /* Map the data array */  
  ndfMap( indf, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, 
	   status );

  full_bolosig = pntr;

  /* Re-order the bolometer signals */
  nc_inq_varid(ncid, "h1b1",&varid_h1b1);
  
  for(i=0; i<nbolos ; i++){
    nc_error( nc_get_var_double(ncid,varid_h1b1+i,bolosig), status );
    
    if( *status == SAI__OK ) {
      for(j=0;j<nframes;j++) {
	full_bolosig[j*nbolos + i] = bolosig[j];
      }
    } else {
      /* Exit if bad status was set */
      i = nbolos;
    }
  }

  /* Format the FITS headers */
  
  fhead_init ( status );
  
  fhead_putfits ( TSTRING,
		  "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
		  "observation date", status );
  
  rad = ra * DR2D;
  
  fhead_putfits ( TDOUBLE,
		  "RA", &rad,
		  "Right Ascension of observation", status );
  
  decd = dec * DR2D;
  
  fhead_putfits ( TDOUBLE,
		  "DEC", &decd,
		  "Declination of observation", status );
  
  fhead_putfits ( TINT,
		  "NBOLX", &ncol,
		  "number of bolometers in X direction", status );

  fhead_putfits ( TINT,
		  "NBOLY", &nrow,
		  "number of bolometers in Y direction", status );
  
  fhead_putfits ( TDOUBLE,
		  "SAMPLE_T", &sample_t,
		  "The sample interval in msec", status );
  
  fhead_putfits ( TSTRING,
		  "SUBARRAY", "AZTEC",
		  "subarray name", status );
  
  fhead_putfits ( TINT,
		  "NUMSAMP", &numsamples,
		  "number of samples", status );

  /* Currently meaningless
  fhead_putfits ( TDOUBLE,
		  "AMSTART", &amstart,
		  "Air mass at start", status );
  fhead_putfits ( TDOUBLE,
		  "AMEND", &amend,
		  "Air mass at end", status );
  
  fhead_putfits ( TDOUBLE,
		  "MEANWVM", &meanwvm,
		  "Mean zenith tau at 225 GHz from WVM", status );
  */
  
  fhead_putfits ( TSTRING,
		  "FILTER", "1100",
		  "filter used", status );
  
  /* Currently meaningless
  fhead_putfits ( TDOUBLE,
		  "ATSTART", &atstart,
		  "Ambient temperature at start (C)", status );
  
  fhead_putfits ( TDOUBLE,
		  "ATEND", &atend,
		  "Ambient temperature at end (C)", status );
  */

  fhead_putfits ( TSTRING,
		  "INSTRUME", "AZTEC",
		  "Instrument name", status );

  fhead_putfits ( TSTRING,
		  "TELESCOP", "JCMT",
		  "Name of telescope", status );
   
  /* Determine extent of the map from posptr + known size of the arrays */
  for( i=0; i<numsamples; i++ ) {
    
    if( i == 0 ) {
      x_min = posptr[0];
      x_max = posptr[0];
      y_min = posptr[1];
      y_max = posptr[1];
    }
    
    if( posptr[i*2] < x_min ) x_min = posptr[i*2];
    if( posptr[i*2] > x_max ) x_max = posptr[i*2];
    if( posptr[i*2+1] < y_min ) y_min = posptr[i*2+1];
    if( posptr[i*2+1] > y_max ) y_max = posptr[i*2+1];
    
  }
  
  map_wdth = (x_max - x_min) + 1000; /* 1000 arcsec for array FOV */
  map_hght = (y_max - y_min) + 1000; /* 1000 arcsec for array FOV */
  map_pa = 0; /* kludge */
  map_x = (x_max + x_min)/2.;
  map_y = (y_max + y_min)/2.;
  
  fhead_putfits ( TDOUBLE,
		  "MAP_HGHT", &map_hght,
		  "Map height (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_WDTH", &map_wdth,
		  "Map height (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_PA", &map_pa,
		  "Map PA (degrees)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_X", &map_x,
		  "Map X offset (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_Y", &map_y,
		  "Map Y offset (arcsec)", status );
  
  /* Get the accumulated FITS headers */ 
  fhead_getfits ( &nrec, fitsrec, status );

  /* Create storage for Header values for each frame - store in JCMTSTATE */
  ndfXnew( indf, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0, &jcmtstateloc, status );

  sc2store_headcremap ( jcmtstateloc, nframes, INST__SCUBA2, status );

  /* Store the JCMState one frame at a time */
  for( j=0; j<numsamples; j++ ) {
    sc2store_headput ( j, head[j], status );
  }

  /* Store the FITS header */

  ndfXnew ( indf, "FITS", "_CHAR*80", 1, &(nrec), &fitsloc, status );
  
  for ( k=1; k<=nrec; k++ ) {
    datCell ( fitsloc, 1, &k, &loc2, status );
    datPut0C ( loc2, fitsrec[k-1], status );
    datAnnul ( &loc2, status );
  }
  datAnnul ( &fitsloc, status );

  /* Close the NDF */

  sc2store_headunmap( status );

  ndfAnnul ( &indf, status );

  ndfEnd ( status );

  /* close the netCDF file and check for errors */
  
  if ( *status == SAI__OK ) {
    nc_error ( nc_close(ncid), status );
  }

  /* Free memory */
  
  smf_free ( bolosig, status );
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

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB, FUNC_NAME, 
	   "Impaztec complete, NDF file written", status); 

#else

  *status = SAI__ERROR;
  errRep(FUNC_NAME, 
	 "SMURF built without libnetcdf. IMPAZTEC task not supported.", 
	 status);

#endif
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
