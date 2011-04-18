/*
*+
*  Name:
*     smf_flat_params

*  Purpose:
*     Obtain flatfield reduction parameters

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_flat_params( const smfData * refdata,
*                      const char resistpar[], const char methpar[], const char orderpar[],
*                      const char snrminpar[], double * refres, double **resistance, int * nrows,
*                      int * ncols, smf_flatmeth *flatmeth,
*                      int * order, double * snrmin, int * status );

*  Arguments:
*     refdata = const smfData * (Given)
*        Reference smfData for checking dimensionality of resistance file and for
*        deciding on the correct reference resistance based on the subarray.
*     resistpar = const char [] (Given)
*        Name of the parameter to use for the resistor file. Also contains
*        reference resistance. (group).
*     methpar = const char [] (Given)
*        Name of the parameter to use to request the flatfield
*        method.
*     orderpar = const char [] (Given)
*        Name of the parameter to use to request the polynomial order.
*        Only used if methpar indicates a polynomial is to be used.
*     snrminpar = const char [] (Given)
*        Name of the parameter to use to request the minimum signal-to-noise
*        ratio for a fit. Can be NULL.
*     refres = double * (Returned)
*        Reference resistance in ohms.
*     resistance = double ** (Returned)
*        Will be returned pointing to an array of doubles (nrows * ncols)
*        with the resistance of each bolometer. Should be freed by the caller
*        using smf_free.
*     nrows = int * (Returned)
*        Number of rows in the resistance array. Can be null.
*     ncols = int * (Returned)
*        Number of colums in the resistance array. Can be null.
*     flatmeth = smf_flatmeth * (Returned)
*        Flatfield method.
*     order = int * (Returned)
*        Polynomial order.
*     snrmin = double * (Returned)
*        Signal-to-noise ratio minimum for a good fit. Will be unchanged if
*        "snrminpar" is NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*      Obtain flatfield reduction parameters from the environment. These
*      are needed whenever a flatfield is to be processed.

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-02-05 (TIMJ):
*        Original version.
*     2010-04-09 (TIMJ):
*        Add SNRMIN
*     2010-04-20 (TIMJ):
*        Columns in resistance file now start at 0 to be consistent
*        with all other nomenclature.
*     2010-07-15 (TIMJ):
*        Use kpg1Config to read resistor information based on subarray.
*        Remove refrespar since that information is now in the resistor file.
*     2011-04-18 (TIMJ):
*        Use ARRAYID rather than subarray name to handle resistor values.
*        Focal plane name is not reliable.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "par.h"
#include "star/kaplibs.h"
#include "prm_par.h"
#include "sae_par.h"

#include <ctype.h>

void
smf_flat_params( const smfData * refdata, const char resistpar[],
                 const char methpar[], const char orderpar[], const char snrminpar[],
                 double * refohms, double **resistance, int * outrows,
                 int * outcols, smf_flatmeth  *flatmeth,
                 int * order, double * snrmin, int * status ) {

  size_t j = 0;             /* Counter, index */
  char method[SC2STORE_FLATLEN]; /* flatfield method string */
  size_t nbols;              /* Number of bolometers */
  int ncols;                 /* number of columns read from resistor data */
  int nrows;                 /* number of rows read from resistor data */
  AstKeyMap * resmap = NULL; /* Resistor map */
  AstKeyMap * subarrays = NULL; /* Subarray lookup table */
  char thissub[32];          /* This sub-instrument string */

  if (resistance) *resistance = NULL;

  if (*status != SAI__OK) return;
  if (!resistance) {
    *status = SAI__ERROR;
    errRep( "", "Must provide a pointer for 'resistance' "
            "(possible programming error)", status );
    return;
  }

  if (!refdata) {
    *status = SAI__ERROR;
    errRep( "", "Must provide reference data file to calculate flatfield parameters"
            " (possible programming error)", status );
    return;
  }

  /* Based on refdata we now need to calculate the default reference
     resistance and retrieve the correct resistance entry for each bolometer.
     We need the unique subarray string so that we can set up a look up keymap.
     There is no code in SMURF to return all the known subarrays but
     we need to know all the options in order to use kpg1Config. */
  subarrays = astKeyMap( " " );
  astMapPut0I( subarrays, "CG450MK2_M0907D0501", 0, NULL );
  astMapPut0I( subarrays, "CG850MK2_M0904D0503", 0, NULL );
  astMapPut0I( subarrays, "SG850_M0906D1005", 0, NULL );
  astMapPut0I( subarrays, "SG850_M1002D1006", 0, NULL );
  astMapPut0I( subarrays, "SG850_M1005D1007", 0, NULL );
  astMapPut0I( subarrays, "SG850_M1003D1004", 0, NULL );
  astMapPut0I( subarrays, "SG450_M1004D1000", 0, NULL );
  astMapPut0I( subarrays, "SG450_M1007D1002", 0, NULL );
  astMapPut0I( subarrays, "SG450_M1006D1003", 0, NULL );
  astMapPut0I( subarrays, "SG450_M1009D1008", 0, NULL );

  /* and indicate which subarray we are interested in (uppercased) */
  smf_fits_getS( refdata->hdr, "ARRAYID", thissub, sizeof(thissub), status );
  { /* need to uppercase */
    size_t l = strlen(thissub);
    for (j=0;j<l;j++) {
      thissub[j] = toupper(thissub[j]);
    }
  }
  astMapPut0I( subarrays, thissub, 1, NULL );

  /* Read the config file */
  resmap = kpg1Config( resistpar, "$SMURF_DIR/smurf_calcflat.def", subarrays, status );
  subarrays = astAnnul( subarrays );

  if (*status != SAI__OK) goto CLEANUP;

  /* Read the reference resistance */
  astMapGet0D( resmap, "REFRES", refohms );

  msgOutiff(MSG__VERB, "",
            "Read reference resistance for subarray %s of %g ohms\n",
            status, thissub, *refohms );

  /* Parse the file and generate a 2d array of resistor settings */
  if (!astMapGet0I( resmap, "NROWS", &nrows ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Resistor file did not have an nrows entry", status );
    goto CLEANUP;
  }
  if (!astMapGet0I( resmap, "NCOLS", &ncols ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Resistor file did not have an ncols entry", status );
    goto CLEANUP;
  }

  nbols = ncols * nrows;
  *resistance = astCalloc( nbols, sizeof(**resistance), 0 );

  if (*status == SAI__OK) {
    for (j = 0; j < (size_t)ncols; j++) {
      int ngot;
      char colname[6];
      sprintf( colname, "COL%-d", (int)j );
      astMapGet1D( resmap, colname, nrows, &ngot, &((*resistance)[nrows*j]));
      if (ngot != nrows) {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          msgSeti( "NG", ngot);
          msgSetc( "COL", colname );
          msgSeti( "NR", nrows );
          errRep(" ", "Did not read ^NR resistor values from column ^COL, read ^NG", status );
        }
        goto CLEANUP;
      }
    }

    /* Check row vs column count */
    if ( (refdata->dims)[SC2STORE__COL_INDEX] != (size_t)ncols ||
         (refdata->dims)[SC2STORE__ROW_INDEX] != (size_t)nrows ) {
      *status = SAI__ERROR;
      msgSeti( "RC", ncols );
      msgSeti( "RR", nrows );
      msgSeti( "DC", (refdata->dims)[SC2STORE__COL_INDEX]);
      msgSeti( "DR", (refdata->dims)[SC2STORE__ROW_INDEX]);
      errRep( " ", "Dimensions of subarray from resistor file (^RC x ^RR)"
              " do not match those of data file (^DC x ^DR)", status );
      goto CLEANUP;
    }

    /* Replace small values with bad */
    for (j = 0; j < nbols; j++) {
      if ((*resistance)[j] < 0.1) {
        (*resistance)[j] = VAL__BADD;
      }
    }
  }

  /* See if we want to use TABLE or POLYNOMIAL mode */
  parChoic( methpar, "POLYNOMIAL", "POLYNOMIAL, TABLE", 1,
            method, sizeof(method), status );

  *flatmeth = smf_flat_methcode( method, status );

  if (*flatmeth == SMF__FLATMETH_POLY) {
    /* need an order for the polynomial */
    parGdr0i( orderpar, 1, 1, 3, 1, order, status );

    /* and if the order is 1 then we can ask for the snr min */
    if (snrminpar && *order == 1) {
      parGet0d( snrminpar, snrmin, status );
    }

  } else {
    /* need an snr min for table mode responsivities */
    if (snrminpar) parGet0d( snrminpar, snrmin, status );
  }

  if (outrows) *outrows = nrows;
  if (outcols) *outcols = ncols;

 CLEANUP:
  if (*status != SAI__OK) {
    if (*resistance) *resistance = astFree( *resistance );
  }

  return;

}

