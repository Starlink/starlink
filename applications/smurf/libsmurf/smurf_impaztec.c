/*
*+
*  Name:
*     IMPAZTEC

*  Purpose:
*     Import AzTEC NetCDF files and produce SCUBA-2 ICD-compliant files

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
*     Uses the NetCDF library to import raw AzTEC data files and save
*     to NDF files in a format approximating the SCUBA-2 ICD so that they
*     may subsequently be read by other SMURF routines to make maps.

*  Notes:
*     - No base coordinates were stored in netcdf files.
*     - Time is presently inaccurate and requires an optimization routine
*     to calculate the time that makes Az/El and RA/Dec consistent.
*     - This command is untested and probably does not work.

*  ADAM Parameters:
*     IN = _CHAR (Read)
*          Name of the input NetCDF file to be converted.  This name
*          should include the .nc extension.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = _CHAR (Read)
*          Output NDF file.

*  Related Applications:
*     SMURF: MAKEMAP

*  Authors:
*     Mitch Crowe (UBC)
*     Edward Chapin (UBC)
*     Jen Balfour (UBC)
*     Christa VanLaerhoven (UBC)
*     Tim Jenness (JAC, Hawaii)
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
*     2006-09-21 (EC):
*        Minor bug prevented compile when netcdf not available.
*     2006-09-22 (JB):
*        Removed dream & dxml includes
*     2006-11-28 (JB):
*        Corrected calculation for modified julian date.
*     2007-07-20 (CVL):
*        Ignore data points until observation actually starts (when
*        ra, dec, az, el start updating).
*     2007-09-26 (CVL):
*        - rearranged placement of a print statement
*        - put a non-generic date in the fits header
*        - removed old commented-out diagnostic code
*     2007-10-02 (CVL):
*        Ignore data when pointing information has not updated for
*        more than 20 frames
*     2007-12-13 (EC):
*        -Added code to generate framesets (preparation for time fix)
*        -Obtain tracking system and set in the FITS header
*        -Set WAVELEN FITS header
*        -Use the base coordinates from the .nc file rather than the
*         first boresite coordinates in the file (although the base
*         coordinates are identically 0 and un-useable).
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-11 (TIMJ):
*        - Fix JCMT telescope position
*        - Calculate base positions in AZEL (except for planets)
*        - Attempt to calculate the time error
*     2008-04-09 (TIMJ):
*        API change to create_lutwcs
*     2008-12-03 (DSB):
*        Another API change to smf_create_lutwcs.
*     2011-01-11 (TIMJ):
*        Use sc2store_writejcmtstate
*     2011-05-12 (TIMJ):
*        Use one_strtod
*     2017-01-11 (GSB):
*        Pass dtai=VAL__BADD to smf_create_lutwcs.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008,2011 Science and Technology Facilities Council.
*     Copyright (C) 2005-2008 Univeristy of British Columbia.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2017 East Asian Observatory.
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/pal.h"
#include "star/palmac.h"
#include "star/kaplibs.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "jcmt/state.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "libsc2sim/sc2sim_par.h"
#include "libsc2sim/sc2sim_struct.h"
#include "libsc2sim/sc2sim.h"

#include "libaztec/aztec.h"

/* netCDF includes */
#if HAVE_LIBNETCDF
#include "netcdf.h"
#endif

#define FUNC_NAME "smurf_impaztec"
#define TASK_NAME "IMPAZTEC"

#define MAXSTRING 256


/* Ratio between solar and sidereal time (from SLA_AOPPA) */
#define SOLSID 1.00273790935

#if HAVE_LIBNETCDF
/* Prototypes for local functions that wrap netcdf messages */
static void nc_getSignal(int ncid, const char* signalname, double* signal,
                         int* status);
static void nc_error(int nc_status, int *status);

#endif

void smurf_impaztec( int *status ) {

#if HAVE_LIBNETCDF

  /* Local Variables */
  double *airmass = NULL;      /* airmass of each frame */
  double *tel_lst = NULL;      /* LST read from telescope */
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
  char dateobs[20];            /* string of date YYYY/MM/DDThh:mm:ss
                                  for fits header */
  int day;                     /* day of beginning of observation */
  int *dbuf=NULL;              /* simulated data  */
  double dec=0;                /* dec of observation in radians  */
  double decd;                 /* Dec of observation in degrees */
  char dec_str[MAXSTRING];     /* string rep. of dec */
  double djm;                  /* modified julian start date */
  int *dksquid=NULL;           /* dark SQUID time stream data  */
  double *fcal=NULL;           /* flatfield calibration  */
  AstFitsChan *fitschan;       /* FITS headers */
  double *fpar=NULL;           /* flat-field parameters  */
  int framesize=0;             /* # data points per timeslice (nbolos) */
  size_t framespersecond;      /* frames per second */
  double *full_bolosig=NULL;   /* all bolo signals [NBOLOSxNFRAMES] */
  double ha;                   /* hour angle */
  struct JCMTState *head=NULL; /* header data for each frame  */
  smfHead hdr;
  int hour;                    /* hour of beginning of observation */
  dim_t i;                     /* loop counter */
  int i_good;                  /* index for good frames */
  int indf=0;                  /* NDF id for the file */
  dim_t j;                     /* loop counter */
  HDSLoc *jcmtstateloc = NULL; /* HDS locator to JCMTSTATE structure */
  int leng=0;                  /* used to count the length of a string */
  int lbnd[3];                 /* Dimensions of the DATA component */
  double lst_coord=0;          /* LST derived from the coordinates ra,az,el
                                  for the current frame */
  double lst_coord_prev=0;     /* as above for the frame previous */
  double map_hght;             /* Map height in arcsec */
  double map_wdth;             /* Map width in arcsec  */
  double map_pa;               /* Map PA in degrees  */
  double map_x = 0;            /* Map X offset in arcsec */
  double map_y = 0;            /* Map Y offset in arcsec */
  double meanwvm;              /* 225 GHz tau */
  int min;                     /* minute of beginning of observation */
  double *mjuldate=NULL;       /* modified Julian date each sample */
  int month;                   /* month of beginning of observation */
  size_t nbolos;               /* number of bolometers in netCDF data format */
  char ncfile[MAXSTRING];      /* input NetCDF file name */
  int ncid;                    /* id of netCDF file */
  int ncol;                    /* number of bolometers in column  */
  int nconst=0;                /* number of sequential frames with constant
                                  pointing data */
  char ndffile[MAXSTRING];     /* output NDF file name */
  int nflat;                   /* number of flat coeffs per bol  */
  int nframes;                 /* number of time steps in netCDF data format */
  int ngframes=0;              /* number of good frames */
  int nmap=0;                  /* Number of elements mapped */
  int nrow;                    /* number of bolometers in row */
  int numsamples;              /* number of samples  */
  double obslam;               /* wavelength */
  int place=0;                 /* NDF placeholder */
  void *pntr=NULL;             /* Temporary pointer */
  double *posptr=NULL;         /* pointing offsets from map centre */
  int *quality=NULL;           /* for a given frame: 1 is good, 0 is bad */
  double ra=0;                 /* ra of observation in radians  */
  double rad;                  /* RA of observation in degrees */
  char ra_str[MAXSTRING];      /* string rep. of ra */
  double steptime;             /* sample interval in msec  */
  int sec;                     /* second of beginning of observation */
  size_t seconds;              /* seconds in observation */
  int starttime;               /* seconds since noon, UT */
  int startframe;               /* frame at which observation starts */
  double telpos[3];            /* Geodetic location of the telescope */
  double *tempbuff = NULL;     /* throwaway buffer for using calctime */
  double tmp;                  /* throwaway variable for using palDh2e */
  double *time = NULL;         /* arrays for storing per-frame header data */
  char time_str[MAXSTRING];
  double *trackactc1 = NULL;
  double *trackactc2 = NULL;
  double *trackbasec1 = NULL;
  double *trackbasec2 = NULL;
  double *trackdemandc1 = NULL;
  double *trackdemandc2 = NULL;
  char tracksys[MAXSTRING];
  int ubnd[3];                 /* Dimensions of the DATA component */
  int varid_h1b1;              /* netCDF variable id of bolo h1b1 signal */

  double x_min = 0;            /* Maximum extent of pointing offsets */
  double x_max = 0;
  double y_min = 0;
  double y_max = 0;
  int yr;                      /* year of beginning of observation */
  int jrslt = 0;
  int nstrt = 0;
  double lst_err;    /* Error in LST */

  double xtemp, ytemp, xout, yout;
  double* ra_app = NULL;
  double *dec_app = NULL;
  double obsgeo[3];

  /* DUT1 for end of 2005 is approx -0.6
     for first half of 2006 is about 0.2 */
  double dut1 = -0.6;

  /* Main routine */

  /* We should use the telescope location that the TCS actually used.
     We therefore use the OBSGEO coordinates known to be in use at that
     time (2005/2006 rather than relying on a name lookup. */

  obsgeo[0] = -5464594.335493;
  obsgeo[1] = -2492695.151639;
  obsgeo[2] = 2150964.058506;

  /* Get the LON/LAT of JCMT */
  smf_calc_telpos( obsgeo, "JCMT", telpos, status );

  /* Get the user defined input and output file names */
  parGet0c( "IN", ncfile, MAXSTRING, status);
  parGet0c( "OUT", ndffile, MAXSTRING, status);

  if( *status == SAI__OK ) {
    /* Open the netCDF file and check for errors */
    nc_error( nc_open(ncfile,NC_NOWRITE,&ncid), status );
  }

  if( *status == SAI__OK ) {

    /* Populate bolo LUT */
    memset( &hdr, 0, sizeof(hdr) );
    aztec_fill_smfHead( &hdr, 0, status );

    /* Preset some required values */
    meanwvm    = 0; /* fix */
    obslam     = 0;
    nrow       = 1;
    steptime   = 0.015625; /* AzTEC sample time */
    nflat      = 1;
    atstart    = 0;
    atend      = 0;

    msgOutif(MSG__VERB," ",
             "Reading netcdf file", status);

    /* Get the data dimensionality */
    nc_inq_dimlen(ncid,0,&nbolos);
    nc_inq_dimlen(ncid,3,&framespersecond);
    nc_inq_dimlen(ncid,4,&seconds);
    nframes = framespersecond*seconds;

    /* Allocate temporary memory for the bolometer signals */
    bolosig = astMalloc( nframes*sizeof( *bolosig ) );

    /* create and fill the header with rts/tcs data */
    head = astCalloc( nframes, sizeof(*head) );

    /* Retrieve values to convert from the netcdf file. */

    time = astCalloc( nframes, sizeof(*time) );
    airmass = astCalloc( nframes, sizeof(*airmass) );
    tel_lst = astCalloc( nframes, sizeof(*tel_lst) );
    trackactc1 = astCalloc( nframes, sizeof(*trackactc1) );
    trackactc2 = astCalloc( nframes, sizeof(*trackactc2) );
    trackdemandc1 = astCalloc( nframes, sizeof(*trackdemandc1) );
    trackdemandc2 = astCalloc( nframes, sizeof(*trackdemandc2) );
    trackbasec1 = astCalloc( nframes, sizeof(*trackbasec1) );
    trackbasec2 = astCalloc( nframes, sizeof(*trackbasec2) );
    azelactc1 = astCalloc( nframes, sizeof(*azelactc1) );
    azelactc2 = astCalloc( nframes, sizeof(*azelactc2) );
    azeldemandc1 = astCalloc( nframes, sizeof(*azeldemandc1) );
    azeldemandc2 = astCalloc( nframes, sizeof(*azeldemandc2) );
    azelbasec1 = astCalloc( nframes, sizeof(*azelbasec1) );
    azelbasec2 = astCalloc( nframes, sizeof(*azelbasec2) );
    dbuf = astMalloc( (nframes*nbolos)*sizeof(*dbuf) );
    dksquid = astMalloc( (nframes*nbolos)*sizeof(*dksquid) );
    posptr = astMalloc( (2*nframes)*sizeof(*posptr) );
    fcal = astMalloc( nbolos*sizeof(*fcal) );
    fpar = astMalloc( nflat*sizeof(*fpar) );
    mjuldate = astCalloc( nframes, sizeof(*mjuldate) );
    tempbuff = astCalloc( nframes, sizeof(*tempbuff) );
    ra_app = astCalloc( nframes, sizeof(*ra_app) );
    dec_app = astCalloc( nframes, sizeof(*dec_app) );

    quality = astCalloc( nframes, sizeof(*quality) );
    memset( quality, 0, nframes*sizeof(int) );

    /* Calculate the time for each frame.  First, get the modified julian
       date (day) from the "date" attribute of the NETCDF file.  Then,
       get the time of the first frame and convert it from 'seconds from
       midnight' to 'seconds from noon' by subtracting or adding fractions
       of a modified julian day as necessary. */

    dateobs[4]='-'; dateobs[7]='-'; dateobs[10]='T';
    dateobs[13]=':'; dateobs[16]=':'; dateobs[19]='\0';

    /* Get the month, day, and year */
    nc_get_att_text ( ncid, NC_GLOBAL, "date", date_str );
    curtok = strtok ( date_str, "/");
    for(i=0;i<2;i++) { dateobs[i+5]=curtok[i]; }
    month = atoi ( curtok );
    curtok = strtok ( NULL, "/" );
    for(i=0;i<2;i++) { dateobs[i+8]=curtok[i]; }
    day = atoi ( curtok );
    curtok = strtok ( NULL, "/" );
    for(i=0;i<4;i++) { dateobs[i]=curtok[i]; }
    yr = atoi ( curtok );
    printf("IMPAZTEC date yr: %i mo: %i day: %i\n", yr, month, day);

    /* Calculate the base modified julian date */
    palCaldj ( yr, month, day, &djm, &date_status );

    /* Get the hours, minutes, and seconds */

    nc_get_att_text ( ncid, NC_GLOBAL, "start_time", time_str );
    curtok = strtok ( time_str, ":");
    hour = atoi ( curtok );
    while( !curtok[leng]=='\0' ) { leng++; }
    if(leng==1) { dateobs[11]='0'; dateobs[12]=curtok[0]; }
    else if(leng<1) { dateobs[11]='0'; dateobs[12]='0'; }
    else { dateobs[11]=curtok[leng-2]; dateobs[12]=curtok[leng-1]; }
    curtok = strtok ( NULL, ":" );
    for(i=0;i<2;i++) { dateobs[i+14]=curtok[i]; }
    min = atoi ( curtok );
    curtok = strtok ( NULL, ":" );
    for(i=0;i<2;i++) { dateobs[i+17]=curtok[i]; }
    sec = atoi ( curtok );
    printf("IMPAZTEC date hr: %i min: %i sec: %i\n", hour, min, sec);

    starttime = (hour * 3600) + (min * 60) + sec;

    djm += (double)starttime / 86400;

    /* Use simulator routine to calculate array of UT for each
       timeslice. NOTE: this is UT1, not UTC. Also DUT1 passed in as 0
       since we don't have any way of inputting it. */
    sc2sim_calctime( telpos[0]*DD2R, djm, dut1, steptime, nframes,
                     mjuldate, tempbuff, status );

    /* RA + Dec at centre of map */
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C1", ra_str );
    ra = one_strtod( ra_str, status );
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_header_C2", dec_str );
    dec = one_strtod( dec_str, status );
    printf("IMPAZTEC ra,dec from jcmt_header: %g %g\n", ra, dec);

    /* Get tracking system */
    nc_get_att_text ( ncid, NC_GLOBAL, "jcmt_script_track_sys", tracksys );
    if( strncmp( tracksys, "Planet", 6 ) == 0 ) {
      strcpy( tracksys, "APP" );
    }

    /* Calculate the JCMTState at each timeslice
       NOTE: jcmt_*Base* are identically 0 and will need to be generated! */

    nc_getSignal ( ncid, "jcmt_lst", tel_lst, status );
    nc_getSignal ( ncid, "jcmt_airmass", airmass, status );
    nc_getSignal ( ncid, "jcmt_trackActC1", trackactc1, status );
    nc_getSignal ( ncid, "jcmt_trackActC2", trackactc2, status );
    nc_getSignal ( ncid, "jcmt_trackDemandC1", trackdemandc1, status );
    nc_getSignal ( ncid, "jcmt_trackDemandC1", trackdemandc1, status );
    nc_getSignal ( ncid, "jcmt_azElActC1", azelactc1, status );
    nc_getSignal ( ncid, "jcmt_azElActC2", azelactc2, status );
    nc_getSignal ( ncid, "jcmt_azElDemandC1", azeldemandc1, status);
    nc_getSignal ( ncid, "jcmt_azElDemandC2", azeldemandc2, status);

    /* Calculate the error in LST in radians */
    lst_err = tempbuff[nframes-1] - (DH2R * tel_lst[nframes-1]);

    {
      double range;
      range = tel_lst[nframes-1] - tel_lst[0]; /* range in decimal hours */
      range /= (int)nframes;  /* length of frame  */
      range /= SOLSID; /* UT */
      range /= 24.0;  /* fraction of day */
      range *= SPD;  /* seconds */
      printf("length of step = %f\n",range);
      range = tempbuff[nframes-1] - tempbuff[0]; /* range in decimal hours */
      range /= (int)nframes;  /* length of frame */
      range /= SOLSID; /* UT */
      range *= PAL__DR2H;
      range /= 24.0;  /* fraction of day */
      range *= SPD;  /* seconds */
      printf("length of step = %f\n",range);
      range = mjuldate[nframes-1] - mjuldate[0]; /* range in decimal hours */
      range /= (int)nframes;  /* length of frame  */
      /*      range /= SOLSID; */ /* UT */
      range *= SPD;  /* seconds */
      printf("length of step = %f\n",range);
    }

    /* this corresponds to a utc error that needs to be adjusted by the
       different length of the second */
    lst_err /= SOLSID;

    /* convert the error to seconds */
    lst_err *= PAL__DR2S;

    printf("Error in LST = %f sec\n", lst_err  );

    /* calculate offset for UT1 */
    lst_err += palDtt( mjuldate[0] );

    /* correct to TAI */
    lst_err -= 32.184;

    printf("Final correction = %f sec \n",lst_err);


    lst_err = 10;
    /* and then to a fraction of a day */
    lst_err /= SPD;

    /* with correction of lst_diff - palDtt =  -1058 arcsec error
    // with no correction error = 141 arcsec
    // with just DTT (64 sec) = -1108
    // with correction of 157 error = -2506
    // with correction of 32.184  error = -626
    // with correction of 10 error = -292
    */

    /* and correct the UTC values */
    for (i=0; i < nframes; i++) {
      mjuldate[i] += lst_err;
    }


    /* if this is a planet the base position needs to be recalculated.
       If this is a sidereal source we can read the base position from
       the header. Unfortunately the AZEL base is always moving */

    if (strcmp(tracksys, "APP" ) == 0 ) {

      /* calculate base for planet at each time slice */

    } else {
      size_t len;
      /* need to get tracking centre */
      nc_get_att_text ( ncid, NC_GLOBAL, "source_ra", ra_str );
      nc_get_att_text ( ncid, NC_GLOBAL, "source_dec", dec_str );

      len = strlen(ra_str);
      for (i=0; i <len; i++) {
        if (ra_str[i] == ':') {
          ra_str[i] = ' ';
        }
      }
      len = strlen(dec_str);
      for (i=0; i <len; i++) {
        if (dec_str[i] == ':') {
          dec_str[i] = ' ';
        }
      }

      /* convert to radians */
      nstrt = 1;
      palDafin( ra_str, &nstrt, &ra, &jrslt );
      ra *= 15.0;
      nstrt = 1;
      palDafin( dec_str, &nstrt, &dec, &jrslt );

      /* store base position in ra/dec and calculate apparent */
      for (i = 0; i < nframes ; i++ ) {
        trackbasec1[i] = ra;
        trackbasec2[i] = dec;

        palMap( ra, dec, 0., 0., 0., 0., 2000., mjuldate[i],
                &(ra_app[i]), &(dec_app[i]));

      }

    }

    /* now convert base RA/Dec to azel - there will be a slight
       error because AST and TCS take into account more effects
       than are handled by palDe2h */


    for (i=0; i< nframes; i++) {
      double hourangle;
      double phi = DD2R * telpos[1];

      /* Calculate hour angle */
      hourangle = tel_lst[i] - ra_app[i];

      /* Calculate the az/el corresponding to the map centre (base) */
      palDe2h ( hourangle, dec_app[i], phi, &(azelbasec1[i]),&(azelbasec2[i]));

    }


    /* Because AzTEC starts recording data before an observation
       acutally starts determine the frame at which the observation
       starts. This can be determined from the ra, az, el because
       their values do not update when the observation is not in
       progress. The easiest way to see when they are updating is to
       derive an LST from them which will be constant when they are
       not updating. */
    /* Additionally: Durring the scan the pointing information
       typically stays constant for 8 or 9 frames or sometimes 17 or
       18 frames. There can be chunks of data where the pointing info
       is constant for better than 100 frames. -> Say that the first 25
       frames in chunk of frames with constant pointing are good,
       the frames following that are 'bad'. In the future one might try
       interpolating the pointing information over the mid-scan 'bad'
       frames. */

    startframe = nframes;
    for ( i = 0; i < nframes; i++ ) {

      lst_coord_prev = lst_coord;
      palDh2e( azelactc1[i], azelactc2[i], telpos[1]*2*3.1415926536/360.0,
               &ha, &tmp);
      lst_coord = ha + trackactc1[i]; /* LST derived from coordinates */
      /* (az, el, and ra) */

      if( (lst_coord - lst_coord_prev)>0.0 && i!=0) {

        if(ngframes==0) { startframe=i; }
        quality[i]=1;
        ngframes++;
        /* printf("IMPAZTEC i: %5i nconst: %5i\n", i, nconst); */
        nconst=0;

      }
      else {
        nconst++;

        if(ngframes>0 && nconst<=20) {
          quality[i]=1;
          ngframes++;
        }

      }

    }

    printf("IMPAZTEC n_frames: %i n_good_frames: %i startframe: %i\n",
           nframes, ngframes, startframe);

    if (ngframes == 0) {
      printf("This file contains no good frames? LST(coords) is constant.\n");
      goto CLEANUP;
    }

    /* DIAGNOSTIC */
    double tel_lat, lst, ha_azel, ha_tr, dec_fr_azel;
    tel_lat = telpos[1]*2*3.1415926536/360.0; /* telescope lat in radians */
    palDh2e(azelactc1[startframe], azelactc2[startframe], tel_lat,
            &ha_azel, &dec_fr_azel);
    lst = tempbuff[startframe];
    ha_tr = lst - trackactc1[startframe];
    printf("IMPAZTEC ha(az,el): %g ha(lst,ra): %g\n", ha_azel, ha_tr);
    printf("IMPAZTEC dec(az,el): %g dec: %g\n",
           dec_fr_azel, trackactc2[startframe]);

    i_good=0;
    for( i=0; i<nframes; i++) {

      if(quality[i]==1) {

        head[i_good].rts_num = i_good;
        head[i_good].rts_end = mjuldate[i];
        head[i_good].tcs_tai = mjuldate[i];
        head[i_good].tcs_airmass = airmass[i];
        strcpy( head[i_good].tcs_tr_sys, tracksys );
        head[i_good].tcs_tr_ac1 = trackactc1[i];
        head[i_good].tcs_tr_ac2 = trackactc2[i];
        head[i_good].tcs_tr_dc1 = trackdemandc1[i];
        head[i_good].tcs_tr_dc2 = trackdemandc2[i];

        /*head[i_good].tcs_tr_bc1 = trackactc1[startframe];*/
        /*head[i_good].tcs_tr_bc2 = trackactc2[startframe];*/
        head[i_good].tcs_tr_bc1 = trackbasec1[i];
        head[i_good].tcs_tr_bc2 = trackbasec2[i];

        head[i_good].tcs_az_ac1 = azelactc1[i];
        head[i_good].tcs_az_ac2 = azelactc2[i];
        head[i_good].tcs_az_dc1 = azeldemandc1[i];
        head[i_good].tcs_az_dc2 = azeldemandc2[i];

        /*head[i_good].tcs_az_bc1 = azelactc1[startframe];*/
        /*head[i_good].tcs_az_bc2 = azelactc2[startframe];*/

        head[i_good].tcs_az_bc1 = azelbasec1[i];
        head[i_good].tcs_az_bc2 = azelbasec2[i];

        posptr[2*i_good +0] = azelactc1[i];
        posptr[2*i_good +1] = azelactc2[i];


        /* Create frameset */

        hdr.cache2 = smf_create_lutwcs( 0, hdr.fplanex, hdr.fplaney, hdr.ndet,
                                        &(head[i_good]), dut1, VAL__BADD,
                                        hdr.instap, telpos, &(hdr.wcs),
                                        hdr.cache2, status );

        if (i_good == 0) {
          double radiff;
          double decdiff;
          astSet( hdr.wcs, "SYSTEM=FK5" );
          xtemp = 3.;
          ytemp = 1.;

          astTran2( hdr.wcs, 1, &xtemp, &ytemp, 1, &xout, &yout );

          printf("RADEC head: %f %f out: %f %f\n",
                 head[i_good].tcs_tr_ac1, head[i_good].tcs_tr_ac2,
                 xout, yout );

          radiff = head[i_good].tcs_tr_ac1 - xout ;
          radiff *= cos(head[i_good].tcs_tr_ac2) * PAL__DR2AS;
          decdiff = head[i_good].tcs_tr_ac2 - yout ;
          decdiff *= PAL__DR2AS;

          printf("Diff = %f , %f \n", radiff, decdiff);

          astSet( hdr.wcs, "SYSTEM=AZEL" );

          astTran2( hdr.wcs, 1, &xtemp, &ytemp, 1, &xout, &yout );

          printf("AZEL head: %f %f out: %f %f\n",
                 head[i_good].tcs_az_ac1, head[i_good].tcs_az_ac2,
                 xout, yout );
        }

        i_good++;

        /* Free cache */
        hdr.cache2 = smf_create_lutwcs( -1, hdr.fplanex, hdr.fplaney, hdr.ndet,
                                        &(head[i_good]), 0.0,hdr.instap, telpos,
                                        &(hdr.wcs), hdr.cache2, status );

      }
    }
    if(i_good != ngframes) {
      printf("IMPAZTEC i_good after filling JCMTState != ngframes, i_good: %i\n",
             i_good);
    }

    /* Additional FITS header information */

    amstart = head[0].tcs_airmass;
    amend = head[ngframes-1].tcs_airmass;
    numsamples = ngframes;
    ncol = nbolos;

  }

  msgOutif(MSG__VERB," ",
           "Writing NDF file", status);

  ndfBegin();

  /* Create HDS container file */
  ndfPlace ( NULL, ndffile, &place, status );

  /* Create an NDF inside the container */
  framesize = ncol * nrow;
  lbnd[SC2STORE__COL_INDEX] = SC2STORE__BOL_LBND;
  ubnd[SC2STORE__COL_INDEX] = lbnd[SC2STORE__COL_INDEX] + ncol - 1;
  lbnd[SC2STORE__ROW_INDEX] = SC2STORE__BOL_LBND;
  ubnd[SC2STORE__ROW_INDEX] = lbnd[SC2STORE__ROW_INDEX] + nrow - 1;
  ubnd[2] = ngframes;
  lbnd[2] = 1;

  ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &indf, status );
  ndfHcre ( indf, status );

  /* Map the data array */
  ndfMap( indf, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap,
          status );

  full_bolosig = pntr;

  /* Re-order the bolometer signals */
  nc_inq_varid(ncid, "h1b1",&varid_h1b1);

  for(j=0; j<nbolos ; j++){
    nc_error( nc_get_var_double(ncid,varid_h1b1+j,bolosig), status );

    if( *status == SAI__OK ) {
      i_good=0;
      for(i=0;i<nframes;i++) {
        if(quality[i]==1) {
          full_bolosig[i_good*nbolos + j] = bolosig[i_good];
          i_good++;
        }
      }
    } else {
      /* Exit if bad status was set */
      j = nbolos;
    }
  }
  if(*status == SAI__OK && i_good != ngframes) {
    printf("IMPAZTEC i_good after filling bolo signals != ngframes,  i_good: %i\n",
           i_good);
  }

  /* Format the FITS headers */
  /* Add the FITS data to the output file */
  fitschan = astFitsChan ( NULL, NULL, " " );
  astSetFitsS ( fitschan, "DATE-OBS", dateobs, "observation date", 0 );
  rad = ra * AST__DR2D;
  astSetFitsF ( fitschan, "RA", rad, "Right Ascension of observation", 0 );
  decd = dec * AST__DR2D;
  astSetFitsF ( fitschan, "DEC", decd, "Declination of observation", 0 );
  astSetFitsI ( fitschan, "ROWSIZE", ncol, "number of bolometers in row", 0 );
  astSetFitsI ( fitschan, "COLSIZE", nrow, "number of bolometers in column", 0 );
  astSetFitsF ( fitschan, "STEPTIME", steptime, "sample interval in msec", 0 );
  astSetFitsS ( fitschan, "SUBARRAY", "AZTEC", "subarray name", 0 );
  astSetFitsI ( fitschan, "NUMSAMP", numsamples, "number of samples", 0 );
  astSetFitsS ( fitschan, "FILTER", "1100", "filter used", 0 );
  astSetFitsS ( fitschan, "INSTRUME", "AZTEC", "Instrument name", 0 );
  astSetFitsF ( fitschan, "WAVELEN", 1100, "Observing wavelength", 0 );
  astSetFitsS ( fitschan, "TELESCOP", "JCMT", "Name of telescope", 0 );

  /* Determine extent of the map from posptr + known size of the arrays */
  if (*status == SAI__OK) {
    for ( i=0; i<numsamples; i++ ) {

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
  }

  astSetFitsF ( fitschan, "MAP_HGHT", map_hght, "Map height (arcsec)", 0 );
  astSetFitsF ( fitschan, "MAP_WDTH", map_wdth, "Map width (arcsec)", 0 );
  astSetFitsF ( fitschan, "MAP_PA", map_pa, "Map PA (degrees)", 0 );
  astSetFitsF ( fitschan, "MAP_X", map_x, "Map X offset (arcsec)", 0 );
  astSetFitsF ( fitschan, "MAP_Y", map_y, "Map Y offset (arcsec)", 0 );

  /* Add the FITS headers as an extension on the NDF */
  kpgPtfts ( indf, fitschan, status );

  /* Create storage for Header values for each frame - store in JCMTSTATE */
  sc2store_writejcmtstate( indf, ngframes, head, status);

  /* Close the NDF */

  sc2store_headunmap( status );

  ndfAnnul ( &indf, status );

  ndfEnd ( status );

  /* close the netCDF file and check for errors */

  if ( *status == SAI__OK ) {
    nc_error ( nc_close(ncid), status );
  }

  /* Free memory */
 CLEANUP:
  bolosig = astFree( bolosig );
  head = astFree( head );
  dbuf = astFree( dbuf );
  dksquid = astFree( dksquid );
  fcal = astFree( fcal );
  fpar = astFree( fpar );
  posptr = astFree( posptr );
  mjuldate = astFree( mjuldate );
  tempbuff = astFree( tempbuff );

  time = astFree( time );
  airmass = astFree( airmass );
  trackactc1 = astFree( trackactc1 );
  trackactc2 = astFree( trackactc2 );
  trackdemandc1 = astFree( trackdemandc1 );
  trackdemandc2 = astFree( trackdemandc2 );
  trackbasec1 = astFree( trackbasec1 );
  trackbasec2 = astFree( trackbasec2 );
  azelactc1 = astFree( azelactc1 );
  azelactc2 = astFree( azelactc2 );
  azeldemandc1 = astFree( azeldemandc1 );
  azeldemandc2 = astFree( azeldemandc2 );
  azelbasec1 = astFree( azelbasec1 );
  azelbasec2 = astFree( azelbasec2 );

  quality = astFree( quality );

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ",
             "Impaztec complete, NDF file written", status);

  }

#else

  *status = SAI__ERROR;
  errRep(FUNC_NAME,
         "SMURF built without libnetcdf. IMPAZTEC task not supported.",
         status);
#endif


}


#ifdef HAVE_LIBNETCDF

/* get a signal specified by name from the netCDF file identified by ncid */
static void nc_getSignal(int ncid, const char* signalname, double* signal,int* status){

  int sigid, nc_status;
  if (*status != SAI__OK) return;
  nc_inq_varid(ncid,signalname, &sigid);
  nc_status = nc_get_var_double(ncid,sigid,signal);
  if(nc_status != NC_NOERR){
    nc_error(nc_status,status);
  }
  return;
}

/* handle netCDF status errors. terminate if we have any errors */
static void nc_error(int nc_status,int* status){

  if(nc_status != NC_NOERR){
    msgSetc("ERROR",nc_strerror(nc_status));
    *status = SAI__ERROR;
    errRep(FUNC_NAME,"libnetcdf reported: ^ERROR",status);
  }
  return;
}

#endif
