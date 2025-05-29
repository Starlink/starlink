/*
 *+
 *  Name:
 *     sc2sim_getobspar.c

 *  Purpose:
 *     Read observation parameters from keymap file and store in
 *     sc2sim_obs struct.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getobspar ( AstKeyMap *keymap, struct sc2sim_obs_struct *inx,
 *                        int *status )

 *  Arguments:
 *     keymap = AstKeyMap* (Given)
 *        Keymap containing obs parameters
 *     sinx = sc2sim_obs_struct* (Returned)
 *        Structure for values from obs keymap file
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Retrieve obs parameters and store in sc2sim_obs_struct.
 *     If a parameter is unspecified, set it to a reasonable default value.

 *  Authors:
 *     J. Balfour (UBC)
 *     A.G. Gibb (UBC)
 *     E. Chapin (UBC)
 *     C. VanLaerhoven (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History :
 *     2006-09-15 (JB):
 *        Original
 *     2006-10-04 (JB):
 *        Replace strcpy with strncpy, replace pong_gridcount with
 *        pong_width and pong_height
 *     2006-10-12 (AGG):
 *        - RA and Dec were not being stored in the inx struct
 *        - Delete wt0_name and wt1_name
 *     2006-10-13 (AGG):
 *        inx.nvert now set to a sensible (i.e non-zero) default value
 *     2006-10-16 (AGG):
 *        inx.jigvert[][] now stored correctly.
 *     2006-10-16 (JB):
 *        Add pong_type
 *     2006-10-23 (EC):
 *        Don't free constant memory used by AST
 *     2006-10-23 (AGG):
 *        Fix bug in Dec conversion to radians
 *     2006-10-25 (EC):
 *        Statically allocate memory for convert
 *     2006-11-21 (JB):
 *        Add lissajous parameters and remove bolfile (deprecated)
 *     2006-11-22 (JB):
 *        Add pong_nmaps and liss_nmaps.
 *     2006-12-18 (AGG):
 *        Add DUT1.
 *     2006-12-18 (JB):
 *        Replace pattern-specific parameters with general values.
 *     2006-12-21 (AGG):
 *        Add instap & instap_x/y
 *     2006-12-22 (AGG):
 *        Add planet and planetnum
 *     2007-01-26 (AGG):
 *        Add Venus to list of supported planets
 *     2007-02-01 (AGG):
 *        Might as well finish the job - Saturn and Neptune are now supported
 *     2007-08-15 (CV):
 *        Added microstepping parameters - nmicstep, mspat_x/y
 *     2007-08-20 (TIMJ):
 *        Can not use strtok on a const char*
 *     2007-09-05 (CV):
 *        Added a default microstep pattern which is used when nmicstep is set
 *        to any negative number
 *     2007-09-06 (AGG):
 *        Read HEATNUM as an integer
 *     2007-09-07 (AGG):
 *        - Set targetpow based on wavelength if not specified
 *        - Introduce flag to denote an 850 or 450 um simulation
 *     2007-10-31 (TIMJ):
 *        astMapGet0I uses int not dim_t
 *     2008-03-19 (AGG):
 *        Add obstype, limit map size, scan speed & duration for a
 *        pointing or focus observation
 *     2008-04-23 (AGG):
 *        - Set a more descriptive default obstype for heatrun simulations
 *        - Use SC2SIM__FLEN for string length rather than numerical value
 *     2008-04-24 (AGG)
 *        - use errRep when setting bad status
 *        - add keywords for focus observation
 *     2010-03-16 (TIMJ):
 *        Use one_strlcpy

 *  Copyright:
 *     Copyright (C) 2007, 2010 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
 *     Council. Copyright (C) 2005-2008 University of British Columbia.
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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* SC2SIM includes */
#include "sc2sim.h"

#include "smurf_par.h"
#include "libsmf/smf.h"

/* Starlink Includes */
#include "ast.h"
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"
#include "star/one.h"

void sc2sim_getobspar ( AstKeyMap *keymap, struct sc2sim_obs_struct *inx,
                        int *status ) {

  char convert[SC2SIM__FLEN]; /* String for converting values */
  double dec;            /* Double representation of Dec */
  int grid_max_x;        /* The reconstruction grid max X */
  int grid_max_y;        /* The reconstruction grid max Y */
  int grid_min_x;        /* The reconstruction grid min X */
  int grid_min_y;        /* The reconstruction grid min Y */
  int i = 0;             /* Loop counter */
  int ix;                /* grid offset */
  int iy;                /* grid offset */
  int j = 0;             /* Array index */
  double msdefault_x[4] = {0, 5.5, 7.0, 1.5}; /* default microstep pattern
                                                 in x in bolometers */
  double msdefault_y[4] = {0, 1.5, 7.0, 5.5}; /* default microstep pattern
                                                 in y in bolometers */
  int n = 0;             /* array index */
  int nvert_x=0;         /* Number of jig_x vertices */
  int nvert_y=0;         /* Number of jig_Y vertices */
  double ra;             /* Double representation of RA */
  const char *temp=NULL; /* Pointer to static strings created by ast */
  char *thischar=NULL;   /* Pointer to current character being upcased */
  int vert_x[SC2SIM__MXVERT]; /* Temporary array for x-vertices */
  int vert_y[SC2SIM__MXVERT]; /* Temporary array for y-vertices */
  int scuba2lon = 1;     /* Flag to denote whether the simulation is 850 or 450 um */
  int itemp;          /* Temporary int variable for ast */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* First check for wavelength */
  if ( !astMapGet0D ( keymap, "LAMBDA", &(inx->lambda) ) )
    inx->lambda = 0.85e-3;

  /* Set the scuba2lon flag to false - allow some leeway */
  if ( (8.5e-4 - inx->lambda) > 1.0e-4 ) {
    scuba2lon = 0;
  }

  if ( !astMapGet0D ( keymap, "BOL_DISTX", &(inx->bol_distx) ) )
    inx->bol_distx = 6.28;

  if ( !astMapGet0D ( keymap, "BOL_DISTY", &(inx->bol_disty) ) )
    inx->bol_disty = 6.28;

  /*   if ( !astMapGet0D ( keymap, "BOL_DISTX", &(inx->bol_distx) ) )
       inx->bol_distx = 5.8;

       if ( !astMapGet0D ( keymap, "BOL_DISTY", &(inx->bol_disty) ) )
       inx->bol_disty = 5.8; */

  if ( !astMapGet0D ( keymap, "BOUS_ANGLE", &(inx->bous_angle) ) )
    inx->bous_angle = 0.0;

  if ( !astMapGet0I ( keymap, "CONV_SHAPE", &(inx->conv_shape) ) )
    inx->conv_shape = 1;

  if ( !astMapGet0D ( keymap, "CONV_SIG", &(inx->conv_sig) ) )
    inx->conv_sig = 1.0;

  if ( !astMapGet0C ( keymap, "COORDFRAME", &temp ) )
    one_strlcpy ( inx->coordframe, "RADEC", sizeof(inx->coordframe), status );
  else {
    one_strlcpy ( convert, temp, sizeof(convert), status );
    /* Convert to uppercase */
    thischar = convert;
    while ( *thischar != '\0' ) {
      *thischar = toupper (*thischar);
      thischar++;
    }
    one_strlcpy ( inx->coordframe, convert, sizeof(inx->coordframe), status);
  }

  if ( !astMapGet0C ( keymap, "DEC", &temp ) )
    inx->dec = 0.0;
  else {
    /* Get the double representation of the sexagesimal string and
       convert from degrees to radians */
    one_strlcpy ( convert, temp, sizeof(convert), status );
    sc2sim_sex2double ( convert, &dec, status );
    dec *= ERFA_DD2R;
    inx->dec = dec;
  }

  if ( !astMapGet0D ( keymap, "DISTFAC", &(inx->distfac) ) )
    inx->distfac = 0.0;

  /* Ideally the user should look this up and supply it. If it's
     absent we have no choice but to assume a value of 0. */
  if ( !astMapGet0D ( keymap, "DUT1", &(inx->dut1) ) )
    inx->dut1 = 0.0;

  if ( !astMapGet0C ( keymap, "FLATNAME", &temp ) )
    one_strlcpy ( inx->flatname, "TABLE", sizeof(inx->flatname), status );
  else
    one_strlcpy ( inx->flatname, temp, sizeof(inx->flatname), status );

  if ( !astMapGet0D ( keymap, "FOCSTART", &(inx->focstart) ) )
    inx->focstart = -3.0;

  if ( !astMapGet0D ( keymap, "FOCSTEP", &(inx->focstep) ) )
    inx->focstep = 1.0;

  if ( !astMapGet0I ( keymap, "GRID_MAX_X", &grid_max_x ) )
    grid_max_x = 1;

  if ( !astMapGet0I ( keymap, "GRID_MAX_Y", &grid_max_y ) )
    grid_max_y = 1;

  if ( !astMapGet0I ( keymap, "GRID_MIN_X", &grid_min_x ) )
    grid_min_x = 1;

  if ( !astMapGet0I ( keymap, "GRID_MIN_Y", &grid_min_y ) )
    grid_min_y = 1;

  if ( !astMapGet0D ( keymap, "GRID_STEP_X", &(inx->grid_step_x) ) )
    inx->grid_step_x = 6.28;

  if ( !astMapGet0D ( keymap, "GRID_STEP_Y", &(inx->grid_step_y) ) )
    inx->grid_step_y = 6.28;

  if ( !astMapGet0I ( keymap, "HEATNUM", &(inx->heatnum) ) )
    inx->heatnum = 150;

  if ( !astMapGet0D ( keymap, "HEATSTART", &(inx->heatstart) ) )
    inx->heatstart = 24.0;

  if ( !astMapGet0D ( keymap, "HEATSTEP", &(inx->heatstep) ) )
    inx->heatstep = 0.01;

  if ( !astMapGet0D ( keymap, "HEIGHT", &(inx->height) ) )
    inx->height = 2000.0;

  if ( !astMapGet0C ( keymap, "INSTAP", &temp ) ) {
    one_strlcpy ( inx->instap, "", sizeof(inx->instap), status );
  } else {
    one_strlcpy ( inx->instap, temp, sizeof(inx->instap), status );
  }

  if ( !astMapGet0D ( keymap, "INSTAP_X", &(inx->instap_x) ) )
    inx->instap_x = 0.0;

  if ( !astMapGet0D ( keymap, "INSTAP_Y", &(inx->instap_y) ) )
    inx->instap_y = 0.0;

  if ( !astMapGet0D ( keymap, "JIG_STEP_X", &(inx->jig_step_x) ) )
    inx->jig_step_x = 6.28;

  if ( !astMapGet0D ( keymap, "JIG_STEP_Y", &(inx->jig_step_y) ) )
    inx->jig_step_y = 6.28;

  /* Retrieve the string representation of the jig_pos.x.  If
     there is no provided string, supply a default. */
  if ( !astMapGet1I ( keymap, "JIG_POS.X", SC2SIM__MXVERT, &nvert_x, vert_x ) ) {
    nvert_x = 8;
    vert_x[0] = 0;
    vert_x[1] = -1;
    vert_x[2] = 1;
    vert_x[3] = -1;
    vert_x[4] = 0;
    vert_x[5] = 1;
    vert_x[6] = -1;
    vert_x[7] = 1;
  }

  /* Retrieve the string representation of the jig_pos.y.  If
     there is no provided string, supply a default. */
  if ( !astMapGet1I ( keymap, "JIG_POS.Y", SC2SIM__MXVERT, &nvert_y, vert_y ) ) {
    nvert_y = 8;
    vert_y[0] = 1;
    vert_y[1] = -1;
    vert_y[2] = 0;
    vert_y[3] = 1;
    vert_y[4] = -1;
    vert_y[5] = 1;
    vert_y[6] = 0;
    vert_y[7] = -1;
  }

  /* Check to make sure the number of vertices is equal, and store
     them in the inx structure */
  if ( nvert_x != nvert_y ) {
    *status = SAI__ERROR;
    msgSeti("NX",nvert_x);
    msgSeti("NY",nvert_y);
    errRep(" ",
           "Number of vertices is not equal (nvert_x = ^NX, nvert_y = ^NY)",
           status);
    return;
  } else  if ( nvert_x == 0 || nvert_y == 0 ) {
    *status = SAI__ERROR;
    errRep(" ",
           "Error parsing jig vertices - no vertices given?", status);
    return;
  } else {
    for ( i = 0; i < nvert_x; i++ ) {
      (inx->jig_vert)[i][0] = vert_x[i];
      (inx->jig_vert)[i][1] = vert_y[i];
    }
  }

  if ( !astMapGet0D ( keymap, "LISS_ANGLE", &(inx->liss_angle) ) )
    inx->liss_angle = 0.0;

  if ( !astMapGet0D ( keymap, "MJDAYSTART", &(inx->mjdaystart) ) )
    inx->mjdaystart = 53795.0;

  if ( !astMapGet0I ( keymap, "COLSIZE", &itemp ) )
    itemp = 40;
  inx->colsize = itemp;

  if ( !astMapGet0I ( keymap, "ROWSIZE", &itemp ) )
    itemp= 32;
  inx->rowsize = itemp;

  if ( !astMapGet0I ( keymap, "NFOCSTEP", &(inx->nfocstep) ) )
    inx->nfocstep = 7;

  if ( !astMapGet0D ( keymap, "NMAPS", &(inx->nmaps) ) )
    inx->nmaps = 1;

  /* Calculate the relative grid coordinates */
  inx->ngrid =
    ( 1 + grid_max_y - grid_min_y ) * ( 1 + grid_max_x - grid_min_x );

  if ( inx->ngrid <= SC2SIM__MXGRID ) {
    for ( iy=grid_min_y; iy<=grid_max_y; iy++ ) {
      for ( ix=grid_min_x; ix<=grid_max_x; ix++ ) {
        (inx->gridpts)[j][0] = ix;
        (inx->gridpts)[j][1] = iy;
        j++;
      }
    }
  }
  else {
    *status = SAI__ERROR;
    msgSeti("N",inx->ngrid);
    msgSeti("MAX",SC2SIM__MXGRID);
    errRep(" ",
           "Number of reconstruction grid points requested, ^N, exceeds maximum, ^MAX ", status);
    return;
  }

  memset( inx->mspat_x, 0, SC2SIM__MXMSTP*sizeof(*(inx->mspat_x)) );
  memset( inx->mspat_y, 0, SC2SIM__MXMSTP*sizeof(*(inx->mspat_y)) );
  if ( !astMapGet0I ( keymap, "NMICSTEP", &(inx->nmicstep) ) ) {
    inx->nmicstep = 1;
  }
  else {
    /* check if number of microsteps is greater than allowed */
    if ( inx->nmicstep > SC2SIM__MXMSTP ) {
      *status = SAI__ERROR;
      msgSeti("NMIC",inx->nmicstep);
      msgSeti("MAX",SC2SIM__MXMSTP);
      errRep(" ",
             "Number of microsteps, ^NMIC, greater than allowed, ^MAX",
             status );
      return;
    }

    /* determine what microstep pattern to use */
    if ( inx->nmicstep < 0 ) {
      /* use default pattern */
      inx->nmicstep = 4;
      for( n=0; n<inx->nmicstep; n++ ) {
        inx->mspat_x[n] = msdefault_x[n];
        inx->mspat_y[n] = msdefault_y[n];
      }
    }
    else if ( inx->nmicstep == 0 || inx->nmicstep == 1 ) {
      /* Don't microstep: set nmicstep to 1 (so the for loop in
         sc2sim_simulate doesn't break), don't put any offsets in to
         mspat_x/y
      */
      inx->nmicstep = 1;
      inx->mspat_x[0] = 0.0;
      inx->mspat_y[0] = 0.0;
    }
    else {
      int nmicx = 0;
      int nmicy = 0;
      /* get custom pattern - first in x */
      astMapGet1D( keymap, "MSPAT_X", SC2SIM__MXMSTP, &nmicx, inx->mspat_x );
      /* get pattern in y */
      astMapGet1D( keymap, "MSPAT_X", SC2SIM__MXMSTP, &nmicy, inx->mspat_y );

      if (nmicx != nmicy && *status == SAI__OK) {
        *status = SAI__ERROR;
        msgSeti( "NX", nmicx );
        msgSeti( "NY", nmicy );
        errRep( " ",
                "Number of x offset in microstep (^NX) differs to number of y offsets (^NY)",
                status);
      }
      inx->nmicstep = nmicx;
    } /* end getting custom pattern */
  }

  if ( !astMapGet0I ( keymap, "NUMSAMPLES", &(inx->numsamples) ) )
    inx->numsamples = 128;

  if ( !astMapGet0I ( keymap, "NVERT", &itemp ) )
    itemp = nvert_x;
  inx->nvert = itemp;

  if ( !astMapGet0C ( keymap, "EXTERNOBS", &temp ) )
    one_strlcpy ( inx->externobs, "none", sizeof("none"), status );
  else
    one_strlcpy ( inx->externobs, temp, sizeof(inx->externobs), status );

  astMapGet0C ( keymap, "OBSMODE", &temp );

  if ( !astMapGet0C ( keymap, "OBSMODE", &temp ) )
    one_strlcpy ( inx->obsmode, "PONG", sizeof(inx->obsmode), status );
  else {
    /* Convert to uppercase */
    one_strlcpy ( convert, temp, sizeof(convert), status );
    thischar = convert;
    while ( *thischar != '\0' ) {
      *thischar = toupper (*thischar);
      thischar++;
    }
    one_strlcpy ( inx->obsmode, convert, sizeof(inx->obsmode), status );
  }

  astMapGet0C ( keymap, "OBSTYPE", &temp );

  if ( !astMapGet0C ( keymap, "OBSTYPE", &temp ) )
    one_strlcpy ( inx->obstype, "SCIENCE", sizeof(inx->obstype), status );
  else {
    /* Convert to uppercase */
    one_strlcpy ( convert, temp, sizeof(convert), status );
    thischar = convert;
    while ( *thischar != '\0' ) {
      *thischar = toupper (*thischar);
      thischar++;
    }
    one_strlcpy ( inx->obstype, convert, sizeof(inx->obstype), status );
  }
  /* Reset nfocstep if not a focus observation */
  if ( strncmp( inx->obstype, "FOCUS", 5 ) ) {
    inx->nfocstep = 1;
    inx->focstart = VAL__BADD;
  }

  /* For heatrun define the obstype as flatfield */
  if ( strncmp( inx->obsmode, "HEAT", 4) == 0 ) {
    one_strlcpy( inx->obstype, "FLATFIELD", sizeof(inx->obstype), status);
  }

  /* Check if a planet has been requested */
  if ( !astMapGet0C ( keymap, "PLANET", &temp) ) {
    /* Can't use zero is it is the sun, a valid `planet' */
    inx->planetnum = -1;
  } else {
    if ( strncmp( temp, "mars", 4 ) == 0
         || strncmp( temp, "MARS", 4 ) == 0 ) {
      inx->planetnum = 4;
    } else if ( strncmp( temp, "uranus", 6 ) == 0
                || strncmp( temp, "URANUS", 6 ) == 0 ) {
      inx->planetnum = 7;
    } else if ( strncmp( temp, "venus", 5 ) == 0
                || strncmp( temp, "VENUS", 5 ) == 0 ) {
      inx->planetnum = 2;
    } else if ( strncmp( temp, "jupiter", 7 ) == 0
                || strncmp( temp, "JUPITER", 7 ) == 0 ) {
      inx->planetnum = 5;
    } else if ( strncmp( temp, "moon", 4 ) == 0
                || strncmp( temp, "MOON", 4 ) == 0 ) {
      inx->planetnum = 3;
    } else if ( strncmp( temp, "saturn", 6 ) == 0
                || strncmp( temp, "SATURN", 6 ) == 0 ) {
      inx->planetnum = 6;
    } else if ( strncmp( temp, "neptune", 7 ) == 0
                || strncmp( temp, "NEPTUNE", 7 ) == 0 ) {
      inx->planetnum = 8;
    } else {
      /* Can't use zero is it is the sun, a valid `planet' */
      inx->planetnum = -1;
    }
  }

  if ( !astMapGet0I ( keymap, "PLATENUM", &(inx->platenum) ) )
    inx->platenum = 1;

  if ( !astMapGet0D ( keymap, "PLATEREV", &(inx->platerev) ) )
    inx->platerev = 2.0;

  if ( !astMapGet0D ( keymap, "PONG_ANGLE", &(inx->pong_angle) ) )
    inx->pong_angle = 0.0;

  if ( !astMapGet0C ( keymap, "PONG_TYPE", &temp ) )
    one_strlcpy ( inx->pong_type, "STRAIGHT", sizeof(inx->pong_type), status );
  else {
    one_strlcpy ( convert, temp, sizeof(convert), status );
    /* Convert to uppercase */
    thischar = convert;
    while ( *thischar != '\0' ) {
      *thischar = toupper (*thischar);
      thischar++;
    }
    one_strlcpy ( inx->pong_type, convert, sizeof(inx->pong_type), status );
  }

  if ( !astMapGet0C ( keymap, "RA", &temp ) )
    inx->ra = 0.0;
  else {
    /* Get the double representation of the sexagesimal string and
       convert from hours to radians */
    one_strlcpy ( convert, temp, sizeof(convert), status );
    sc2sim_sex2double ( convert, &ra, status );
    ra *= DH2R;
    inx->ra = ra;
  }

  if ( !astMapGet0D ( keymap, "STEPTIME", &(inx->steptime) ) )
    inx->steptime = 5.0e-3;

  if ( !astMapGet0D ( keymap, "SCAN_ANGLE", &(inx->scan_angle) ) )
    inx->scan_angle = 0.0;

  if ( !astMapGet0I ( keymap, "SMU_MOVE", &(inx->smu_move) ) )
    inx->smu_move = 8;

  if ( !astMapGet0D ( keymap, "SMU_OFFSET", &(inx->smu_offset) ) )
    inx->smu_offset = 0.0;

  if ( !astMapGet0I ( keymap, "SMU_SAMPLES", &(inx->smu_samples) ) )
    inx->smu_samples = 8;

  if ( !astMapGet0D ( keymap, "SPACING", &(inx->spacing) ) )
    inx->spacing = 240.0;

  if ( !astMapGet0I ( keymap, "SUBSYSNR", &(inx->subsysnr) ) )
    inx->subsysnr = 1;

  if ( !astMapGet0D ( keymap, "TARGETPOW", &(inx->targetpow) ) ) {
    /* Set a wavelength-dependent default */
    if ( scuba2lon == 1 ) {
      inx->targetpow = 25.0;
    } else {
      inx->targetpow = 150.0;
    }
  }

  if ( !astMapGet0D ( keymap, "VMAX", &(inx->vmax) ) )
    inx->vmax = 200.0;

  if ( !astMapGet0D ( keymap, "WIDTH", &(inx->width) ) ) {
    inx->width = 2000.0;
  }

  /* Check if we have a POINTING or FOCUS SCAN observation and if so
     reset the map size and limit the length of the observation. Note
     that these limits are somewhat arbitrary though reasonable based
     on simulations of making small maps. */
  if ( ((strncmp( inx->obstype, "POINT", 5) == 0) ||
        (strncmp( inx->obstype, "FOCUS", 5) == 0)) &&
       (strncmp( inx->obsmode, "DREAM", 5) ||
        strncmp( inx->obsmode, "STARE", 5)) ) {
    inx->height = 100.0;
    inx->width = 100.0;
    /* Limit number of map repeats to 10 if set to a high number */
    if ( inx->nmaps > 10 ) {
      inx->nmaps = 10;
    }
    /* Limit scanning speed to < 120 arcsec/s */
    if ( inx->vmax > 120.0 ) {
      inx->vmax = 120.0;
    }
  }

}

