/*
*+
*  Name:
*     nanten_fill_acsis_hdr

*  Purpose:
*     Create ACSIS FITS header from NANTEN FITS header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*    void nanten_fill_acsis_hdr( AstFitsChan * nantenhdr, AstFitsChan * acsishdr,
*                                int * status );

*  Arguments:
*     nantenhdr = AstFitsChan * (Given)
*        Header from Supercam file
*     acsishdr = AstFitsChan * (Given and Returned)
*        Populated ACSIS FITS header. Should be created before
*        calling this routine.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Maps NANTEN FITS headers and content to the ACSIS standard.
*     Some of the items are left undef as they are filled in later
*     from table rows.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Uses knowledge of NANTEN and ACSIS FITS header standards.

*  History:
*     2014-04-07 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "nanten.h"
#include "ast.h"
#include "star/atl.h"
#include "star/one.h"
#include "sae_par.h"
#include "star/pal.h"

#include "libsmf/smf.h"
#include "smurf_par.h"

#include <string.h>
#include <math.h>



void
nanten_fill_acsis_hdr( AstFitsChan * supcamhdr, AstFitsChan * acsishdr, int * status ) {

  smfHead ihdr;         /* To allow us to use the SMURF FITS helper routines */
  char svalue[80];      /* Temporary buffer for string values from header */
  int ivalue;           /* Temporary int number for header */
  double dvalue;        /* Temporary float number for header */
  double telpos[3];     /* Telescope position */
  double obsgeo[3];     /* Telescope cartesian coordinates */
  double mjdobs;        /* MJD of observation */
  char utdate[9];       /* UT date in YYYYMMDD format */
  char obsid[80];       /* Observation identifier */
  int obsnum;           /* Observation/scan number */

  if (*status != SAI__OK) return;

  /* Need to clear the smfHead */
  memset( &ihdr, 0, sizeof(ihdr) );

  /* Put the fits chan in the SMURF header struct */
  ihdr.fitshdr = supcamhdr;

  /* The FITS header does not have a TELESCOP field */

  /* Start filling in the output header with TELESCOP and ORIGIN */
  atlPtfts( acsishdr, "TELESCOP", "NANTEN2", "Name of Telescope", status );
  atlPtfts( acsishdr, "ORIGIN", "NANTEN Submillimeter Observatory", "Origin of file", status );

  /* Telescope location */
  astSetFitsCM( acsishdr, "---- x,y,z triplet for NANTEN2 relative to centre of earth ----", 0 );

  /* PAL knows about NANTEN2 */
  smf_calc_telpos( NULL, "NANTEN2", telpos, status );
  atlPtftd( acsishdr, "ALT-OBS", telpos[2], "[m] Height of observatory above sea level", status );
  atlPtftd( acsishdr, "LAT-OBS", telpos[1], "[deg] Latitude of Observatory", status );

  telpos[0] *= -1.0; /* East positive required */
  atlPtftd( acsishdr, "LONG-OBS", telpos[0], "[deg] East longitude of observatory", status );

  /* Get the cartesian coordinates of the telescope's location. */
  smf_terr( AST__DD2R*telpos[1], telpos[2], AST__DD2R*telpos[0], obsgeo );
  atlPtftd( acsishdr, "OBSGEO-X", obsgeo[0], "[m]", status );
  atlPtftd( acsishdr, "OBSGEO-Y", obsgeo[1], "[m]", status );
  atlPtftd( acsishdr, "OBSGEO-Z", obsgeo[2], "[m]", status );

  atlPtftd( acsishdr, "ETAL", 1.0, "Telescope efficiency", status );

  astSetFitsCM( acsishdr, "---- OMP and ORAC-DR Specific ----", 0 );

  astSetFitsCM( acsishdr, "---- Obs Id, Date, Pointing Info ----", 0 );

  smf_fits_getS( &ihdr, "OBJECT", svalue, sizeof(svalue), status );
  atlPtfts( acsishdr, "OBJECT", svalue, "Object of interest", status );

  atlPtftl( acsishdr, "STANDARD", 0, "Is this a flux standard? Assume not.", status );

  smf_fits_getI(&ihdr, "SCAN", &obsnum, status );
  if (obsnum == 0) obsnum = 1;
  atlPtfti( acsishdr, "OBSNUM", obsnum, "Observation number", status );

  /* NSUBSCAN and OBSEND are filled in by specwrite */
  atlPtfti( acsishdr, "NSUBSCAN", 0, "Sub-scan number", status );
  atlPtftl( acsishdr, "OBSEND", 1, "True if file is last in current observation", status );

  /* Get the date of observation - refers to start of day */
  smf_fits_getS(&ihdr, "DATE-OBS", svalue, sizeof(svalue), status );

  /* Have to extract the UTDATE from DATE-OBS and convert it to an
     integer. First YYYY-MM-DD to YYYYMMDD */
  strncpy( utdate, svalue, 4 );
  utdate[4] = '\0';
  strncat( utdate, &(svalue)[5], 2 );
  utdate[6] = '\0';
  strncat( utdate, &(svalue)[8], 2 );
  utdate[8] = '\0';
  atlPtfti( acsishdr, "UTDATE",
            (int)one_strtod( utdate, status ),
            "UT as integer in YYYYMMDD format", status );

  /* The actual date is DATE-OBS plus the value of UT which is in
     seconds as an offset */
  /* Convert the DATE-OBS to MJD */
  smf_find_dateobs( &ihdr, &mjdobs, NULL, status );

  /* Get the offset */
  smf_fits_getD(&ihdr, "UT", &dvalue, status );

  /* Adjust it by the integration time */
  mjdobs += dvalue / SPD;

  /* Now create a TimeFrame to format the MJD back to ISO */
  AstTimeFrame * tf = astTimeFrame( "Format=iso.3T" );
  atlPtfts( acsishdr, "DATE-OBS", astFormat( tf, 1, mjdobs ),
            "UTC Datetime of end of observation", status );

  /* We do not yet know the DATE-END - so make it equal to DATE-OBS */
  atlPtfts( acsishdr, "DATE-END", astFormat( tf, 1, mjdobs ),
            "UTC Datetime of start of observation", status );
  astAnnul(tf);


  /* We do not know the DUT1 correction */
  atlPtftd( acsishdr, "DUT1", 0.0, "[d] UT1-UTC correction", status );

  /* Form an OBSID -- just anything for the moment */
  one_snprintf( obsid, sizeof(obsid), "%s_%06d_%s", status,
                "great", obsnum, utdate );
  atlPtfts( acsishdr, "OBSID", obsid, "Unique observation identifier", status );

  /* OBSIDSS is the obsid with a subsystem number. For now assume Supercam
     only ever has a single subsystem in the ACSIS sense */
  one_strlcat( obsid, "_1", sizeof(obsid), status );
  atlPtfts( acsishdr, "OBSIDSS", obsid, "Unique observation identifier", status );

  /* Assume no instrument aperture */
  astSetFitsU( acsishdr, "INSTAP", "Receptor at tracking centre (if any)", 0 );

  /* We do not know focal plane coordinates */
  atlPtftd( acsishdr, "INSTAP_X", 0.0, "[arcsec] Aperture X off. rel. to instr centre", status );
  atlPtftd( acsishdr, "INSTAP_Y", 0.0, "[arcsec] Aperture Y off. rel. to instr centre", status );

  /* Time varying data comes from table */
  smf_fits_getD(&ihdr, "ELEVATIO", &dvalue, status );
  atlPtftd( acsishdr, "ELSTART", dvalue, "[deg] Elevation at obs. start", status );
  atlPtftd( acsishdr, "ELEND", dvalue, "[deg] Elevation at obs. end", status );

  dvalue = palAirmas( AST__DPIBY2 - (dvalue*AST__DD2R) );
  atlPtftd( acsishdr, "AMSTART", dvalue, "Airmass at start of observation", status );
  atlPtftd( acsishdr, "AMEND", dvalue, "Airmass at end of observation", status );

  smf_fits_getD(&ihdr, "AZIMUTH", &dvalue, status );
  atlPtftd( acsishdr, "AZSTART", dvalue, "[deg] Azimuth at obs. start", status );
  atlPtftd( acsishdr, "AZEND", dvalue, "[deg] Azimuth at obs. end", status );

  /* Do not know options for tracking system */
  atlPtfts( acsishdr, "TRACKSYS", "J2000", "TCS Tracking coordinate system [guess]", status );

  smf_fits_getD(&ihdr, "CRVAL2", &dvalue, status );
  atlPtftd( acsishdr, "BASEC1", dvalue, "[deg] TCS BASE position (longitude) in TRACKSYS", status );

  smf_fits_getD(&ihdr, "CRVAL3", &dvalue, status );
  atlPtftd( acsishdr, "BASEC2", dvalue, "[deg] TCS BASE position (latiitude) in TRACKSYS", status );

  /* Correlator */
  astSetFitsCM( acsishdr, "---- Correlator Specific ----", 0 );

  /* This is a con */
  atlPtfts( acsishdr, "BACKEND", "SMART", "Name of the backend", status );

  smf_fits_getS( &ihdr, "LINE", svalue, sizeof(svalue), status );
  /* assume the line is "MOLECULE_TRANSITION" */
  ivalue = strlen(svalue);
  for (int i=0; i<ivalue; i++) {
    if ( svalue[i] == '_' ) {
      svalue[i] = '\0';
      atlPtfts( acsishdr, "MOLECULE", svalue, "Target molecular species", status);
      atlPtfts( acsishdr, "TRANSITI", &(svalue[i+1]), "Target transition for MOLECULE", status);
      break;
    }
  }

  /* XXX Need to guess some backend degration numbers */
  atlPtftd( acsishdr, "BEDEGFAC", 1.0, "Backend degradation factor", status );
  atlPtfts( acsishdr, "FFT_WIN", "truncate", "Type of window used for FFT", status );

  /* Frontend configuration */
  astSetFitsCM( acsishdr, "---- FE Specific ----", 0 );
  atlPtfts( acsishdr, "INSTRUME", "SMART", "Front-end receiver", status );
  atlPtfts( acsishdr, "BUNIT", "K", "Data units", status );
  atlPtfts( acsishdr, "SB_MODE", "DSB", "Sideband mode", status );

  {
    /* Read the RESTFREQ and IMAGFREQ headers to estimate the configuration. */
    double imagfreq;
    double restfreq;
    double lofreq;

    smf_fits_getD( &ihdr, "RESTFREQ", &restfreq, status );
    smf_fits_getD( &ihdr, "IMAGFREQ", &imagfreq, status );

    /* Convert to GHz */
    restfreq /= 1E9;
    imagfreq /= 1E9;

    /* Estimate the IF as the difference */
    atlPtftd( acsishdr, "IFFREQ", fabs(restfreq - imagfreq) / 2.0,
              "[GHz] IF Frequency", status );

    if (restfreq > imagfreq) {
      atlPtfts( acsishdr, "OBS_SB", "USB", "The observed sideband", status );
    } else {
      atlPtfts( acsishdr, "OBS_SB", "LSB", "The observed sideband", status );
    }

    /* We do not have enough information to work out how the LO shifted
       during the observation to track LSR so we just assume it was fixed
       for the moment rather than doing detailed modeling taking into
       account topocentric position. */
    /* Estimate the LO as the average of the two */
    lofreq = (restfreq + imagfreq) / 2.0;

    atlPtftd( acsishdr, "LOFREQS", lofreq, "[GHz] LO Frequency at start of obs.",
              status );
    atlPtftd( acsishdr, "LOFREQE", lofreq, "[GHz] LO Frequency at end of obs.",
              status );
  }


  atlPtfts( acsishdr, "TEMPSCAL", "TA*", "Temperature scale in use", status );

  /* "doppler" is actually the velocity definition */
  smf_fits_getS( &ihdr, "VELDEF", svalue, sizeof(svalue), status );
  if ( svalue[0] == 'R') {
    atlPtfts( acsishdr, "DOPPLER", "radio", "Doppler velocity definition", status );
  } else if ( svalue[0] == 'O') {
    atlPtfts( acsishdr, "DOPPLER", "optical", "Doppler velocity definition", status );
  } else {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRepf("", "Do not yet know how to extract velocity definition from '%s'",
              status, svalue );
    }
  }

  atlPtfts( acsishdr, "SSYSOBS", "TOPOCENT", "Spectral ref. frame during observation",
            status );

  astSetFitsCM( acsishdr, "---- Environmental Data ----", 0 );
  smf_fits_getD(&ihdr, "TOUTSIDE", &dvalue, status );
  dvalue -= 273.15;
  atlPtftd( acsishdr, "ATSTART", dvalue, "[degC] Air temp at start", status );
  atlPtftd( acsishdr, "ATEND", dvalue, "[degC] Air temp at end", status );

  smf_fits_getD(&ihdr, "PRESSURE", &dvalue, status );
  atlPtftd( acsishdr, "BPSTART", dvalue, "[mbar] Pressure at start", status );
  atlPtftd( acsishdr, "BPEND", dvalue, "[mbar] Pressure at end", status );

  astSetFitsCM( acsishdr, "---- Switching and Map setup for the observation ----", 0 );

  atlPtfts(acsishdr, "SAM_MODE", "scan", "Sampling mode", status );

  atlPtfts(acsishdr, "SW_MODE", "pssw", "Switch Mode: CHOP, PSSW, NONE, etc", status );
  astSetFitsU(acsishdr, "INBEAM", "Hardware in the beam", 0 );
  atlPtfts(acsishdr, "OBS_TYPE", "science", "Type of observation", status );

  /* Assume the raster is aligned with the tracking coordinate frame.
     Would need more files to work it out */
  atlPtftd( acsishdr, "MAP_PA", 0.0, "[deg] Requested PA of map", status );

  /* No header for scan velocity and would need to know it from the second file */
  astSetFitsU(acsishdr, "SCAN_VEL", "[arcsec/sec] Scan velocity along scan axis", 0 );

  /* Sequence parameters are essentially unknown so just put in the minimum */
  astSetFitsCM( acsishdr, "---- Sequence Parameters ----", 0 );

  smf_fits_getD(&ihdr, "OBSTIME", &dvalue, status );
  atlPtftd(acsishdr, "STEPTIME", dvalue, "[sec] RTS step time during an RTS sequence", status );

  /* Miscellaneous */
  astSetFitsCM( acsishdr, "---- Miscellaneous ----", 0 );
  atlPtftl( acsishdr, "SIMULATE", 0, "True if any data are simulated", status );

  /* Fix up the WCS */

  /* CTYPE2 and CTYPE3 do not have projection information but AST should handle that. */
  {
    char ctype[12];
    double cdelt;

    /* This seems to be required to let FITS-CLASS work */
    smf_fits_getS( &ihdr, "CTYPE2", ctype, sizeof(ctype), status );
    if (strcmp(ctype, "RA") == 0 ) {
      one_strlcat(ctype, "---GLS", sizeof(ctype), status );
      atlPtfts( ihdr.fitshdr, "CTYPE2", ctype, "Added GLS", status );
    }
    smf_fits_getS( &ihdr, "CTYPE3", ctype, sizeof(ctype), status );
    if (strcmp(ctype, "DEC") == 0 ) {
      one_strlcat(ctype, "--GLS", sizeof(ctype), status );
      atlPtfts( ihdr.fitshdr, "CTYPE3", ctype, "Added GLS", status );
    }

    /* Force CDELT2 and CDELT3 to be valid numbers */
    smf_fits_getD( &ihdr, "CDELT2", &cdelt, status );
    if (cdelt < 0.000001) atlPtftd( ihdr.fitshdr, "CDELT2", 1.0/3600., "Forced CDELT>0", status );
    smf_fits_getD( &ihdr, "CDELT3", &cdelt, status );
    if (cdelt < 0.000001) atlPtftd( ihdr.fitshdr, "CDELT3", 1.0/3600., "Forced CDELT>0", status );

    /* CRVAL4 */
    atlPtftd( ihdr.fitshdr, "CRVAL4", 1.0, "Forced to 1", status );

  }



  /* Copy all instrument headers over for reference. We have to skip headers that
     already exist in the ACSIS header. Note that part of this is to include the
     WCS which is required by the specwriter */
  {
    AstFitsChan * cleanhdr = NULL;
    AstFitsChan * mergedhdr = NULL;
    char card[81];
    int docopy = 0;

    /* Indicate where the Supercam header begins */
    astSetFitsCM( acsishdr, "---- NANTEN header ----", 0 );

    /* Copy the Supercam header to a new location skipping all the
       table headers and starting at CTYPE */
    cleanhdr = astFitsChan( NULL, NULL, " " );
    astClear( supcamhdr, "Card" );
    while ( astFindFits( supcamhdr, "%f", card, 1 )) {
      if ( !docopy ) {
        /* Requires that we know CTYPEn is first real keyword */
        if ( strncmp( card, "CTYPE", 5 ) == 0 ) docopy = 1;
      }
      if (docopy) {
        astPutFits( cleanhdr, card, 1 );
      }
    }

    /* Now append the cleaned header to the ACSIS format header */
    atlMgfts( 4 , acsishdr, cleanhdr, &mergedhdr, status );

    /* we now have to copy all the cards back to the requested FitsChan
       because a new one was created */
    astEmptyFits( acsishdr );
    astClear( mergedhdr, "Card" );
    while ( astFindFits( mergedhdr, "%f", card, 1) ) {
      astPutFits( acsishdr, card, 1 );
    }

  }


  //astShow(acsishdr);

}
