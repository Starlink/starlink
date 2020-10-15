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
*                      int * order, double * snrmin, smfData **heateff, int *status );

*  Arguments:
*     refdata = const smfData * (Given)
*        Reference smfData for checking dimensionality of resistance file and for
*        deciding on the correct reference resistance based on the subarray.
*     resistpar = const char [] (Given)
*        Name of the parameter to use for the resistor file. Also contains
*        reference resistance. (group).
*     methpar = const char [] (Given)
*        Name of the parameter to use to request the flatfield
*        method. Can be NULL if flatmeth is NULL.
*     orderpar = const char [] (Given)
*        Name of the parameter to use to request the polynomial order.
*        Only used if methpar indicates a polynomial is to be used.
*     snrminpar = const char [] (Given)
*        Name of the parameter to use to request the minimum signal-to-noise
*        ratio for a fit. Can be NULL.
*     refres = double * (Returned)
*        Reference resistance in ohms. Can be NULL.
*     resistance = double ** (Returned)
*        Will be returned pointing to an array of doubles (nrows * ncols)
*        with the resistance of each bolometer. Should be freed by the caller
*        using smf_free. Can be NULL.
*     nrows = int * (Returned)
*        Number of rows in the resistance array. Can be null.
*     ncols = int * (Returned)
*        Number of colums in the resistance array. Can be null.
*     flatmeth = smf_flatmeth * (Returned)
*        Flatfield method. Can be NULL if methpar is NULL.
*     order = int * (Returned)
*        Polynomial order. Can be NULL if methpar is NULL.
*     snrmin = double * (Returned)
*        Signal-to-noise ratio minimum for a good fit. Will be unchanged if
*        "snrminpar" is NULL.
*     heateff = smfData** (Returned)
*        Heater efficiency data for this subarray. Can be NULL.
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
*     2011-08-31 (TIMJ):
*        - Stop using the per-bolometer resistance in resist.cfg. Force
*        a fixed value.
*        - Allow routine to be called without any ADAM parameters specified.
*        - Add heater efficiency reading from config file.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smf_err.h"
#include "smurf_par.h"

#include "mers.h"
#include "par.h"
#include "star/kaplibs.h"
#include "star/one.h"
#include "prm_par.h"
#include "sae_par.h"

#include <ctype.h>
#include <strings.h>

void
smf_flat_params( const smfData * refdata, const char resistpar[],
                 const char methpar[], const char orderpar[], const char snrminpar[],
                 double * refohms, double **resistance, int * outrows,
                 int * outcols, smf_flatmeth  *flatmeth,
                 int * order, double * snrmin, smfData ** heateff,
                 int * status ) {

  int datarows = 0;         /* Number of rows in refdata */
  int datacols = 0;         /* Number of columns in refdata */
  dim_t temp1;              /* Temporary value */
  dim_t temp2;              /* Temporary value */
  dim_t j = 0;              /* Counter, index */
  char method[SC2STORE_FLATLEN]; /* flatfield method string */
  dim_t nbols;              /* Number of bolometers */
  double refohmsval = 0.0;  /* Internal version of refohms */
  AstKeyMap * resmap = NULL;/* Resistor map */
  AstKeyMap * subarrays = NULL; /* Subarray lookup table */
  char thissub[32];         /* This sub-instrument string */

  if (resistance) *resistance = NULL;

  if (*status != SAI__OK) return;

  if (!refdata) {
    *status = SAI__ERROR;
    errRep( "", "Must provide reference data file to calculate flatfield parameters"
            " (possible programming error)", status );
    return;
  }

  /* Based on refdata we now need to calculate the default reference
     resistance and retrieve the correct heater efficiency file for each array.
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
    dim_t l = strlen(thissub);
    for (j=0;j<l;j++) {
      thissub[j] = toupper(thissub[j]);
    }
  }
  astMapPut0I( subarrays, thissub, 1, NULL );

  /* Read the config file */
  resmap = kpg1Config( resistpar, "$SMURF_DIR/smurf_calcflat.def",
                       subarrays, 1, status );
  subarrays = astAnnul( subarrays );

  if (*status != SAI__OK) goto CLEANUP;

  /* Read the reference resistance */
  astMapGet0D( resmap, "REFRES", &refohmsval );

  if (refohms && *status == SAI__OK) {
    *refohms = refohmsval;
    msgOutiff(MSG__VERB, "",
              "Read reference resistance for subarray %s of %g ohms\n",
              status, thissub, *refohms );
  }

  /* We no longer want to read per-bolometer resistor values from the
     config file. To retain backwards compatibility with the current
     implementation of smf_flat_standardpow we simply fill the
     per-bol resistance array with the reference resistance which
     effectively disables smf_flat_standardpow */

  smf_get_dims( refdata, &temp1, &temp2, NULL, NULL, NULL, NULL, NULL, status );
  datarows = (int) temp1;
  datacols = (int) temp2;
  nbols = datacols * datarows;

  if (*status == SAI__OK && resistance ) {
    *resistance = astMalloc( nbols*sizeof(**resistance) );
    for (j = 0; j < (dim_t)nbols; j++) {
      (*resistance)[j] = refohmsval;
    }
  }

  /* Get the heater efficiency file */
  if (heateff && astMapHasKey( resmap, "HEATEFF" ) ) {
    const char * heateffstr = NULL;
    if (astMapGet0C( resmap, "HEATEFF", &heateffstr )) {
      Grp * heateffgrp = NULL;
      smfData * heatefftmp = NULL;
      heateffgrp = grpNew( "heateff", status );
      grpPut1( heateffgrp, heateffstr, 0, status );
      smf_open_file( NULL, heateffgrp, 1, "READ", SMF__NOTTSERIES|SMF__NOFIX_METADATA, &heatefftmp, status );

      /* Divorce the smfData from the underlying file. This file stays open for the entire
         duration of the data processing and can some times lead to issues when we attempt
         to close it an hour after we opened it (it's usually on an NFS disk) */
      if (*status == SAI__OK) {
        *heateff = smf_deepcopy_smfData( NULL, heatefftmp, 0,
                                         SMF__NOCREATE_FILE | SMF__NOCREATE_FTS |
                                         SMF__NOCREATE_DA,
                                         0, 0, status );
        smf_close_file(NULL, &heatefftmp, status);
      }

      /* Check the dimensions */
      if (*status == SAI__OK) {
        dim_t heatrows = 0;
        dim_t heatcols = 0;
        smf_get_dims( *heateff, &heatrows, &heatcols, NULL, NULL, NULL, NULL, NULL, status );

        if (*status == SAI__OK) {
          if ( datarows != heatrows || datacols != heatcols ) {
            *status = SAI__ERROR;
            errRepf( "", "Dimensions of heater efficiency file %s are (%zu, %zu)"
                     " but flatfield has dimensions (%zu, %zu)",
                     status, heateffstr, (dim_t)heatrows, (dim_t)heatcols,
                     (dim_t)datarows, (dim_t)datacols);
          }
        }

        if (*status == SAI__OK) {
          smf_dtype_check_fatal( *heateff, NULL, SMF__DOUBLE, status );
          if (*status == SMF__BDTYP) {
            errRepf("", "Heater efficiency data in %s should be double precision",
                   status, heateffstr);
          }
        }

        if (*status == SAI__OK) {
          char heateffarrid[32];
          smf_fits_getS( refdata->hdr, "ARRAYID", heateffarrid, sizeof(heateffarrid), status );
          if (*status != SAI__OK) errAnnul( status );
          if (strcasecmp( thissub, heateffarrid ) != 0 ) {
            if (*status == SAI__OK) {
              *status = SAI__ERROR;
              errRepf("", "Subarray associated with heater efficiency image (%s)"
                     " does not match that of the data to be flatfielded (%s)",
                      status, heateffarrid, thissub );
            }
          }
        }
      }
      if (heateffgrp) grpDelet( &heateffgrp, status );
    }
  }

  if (methpar && flatmeth) {
    /* See if we want to use TABLE or POLYNOMIAL mode */
    parChoic( methpar, "POLYNOMIAL", "POLYNOMIAL, TABLE", 1,
              method, sizeof(method), status );

    *flatmeth = smf_flat_methcode( method, status );

    if (*flatmeth == SMF__FLATMETH_POLY) {
      /* need an order for the polynomial */
      if (order && orderpar) {
        parGdr0i( orderpar, 1, 1, 3, 1, order, status );

        /* and if the order is 1 then we can ask for the snr min */
        if (snrminpar && *order == 1) {
          parGet0d( snrminpar, snrmin, status );
        }
      }

    } else {
      /* need an snr min for table mode responsivities */
      if (snrminpar) parGet0d( snrminpar, snrmin, status );
    }
  }

  if (outrows) *outrows = datarows;
  if (outcols) *outcols = datacols;

 CLEANUP:
  resmap = astAnnul( resmap );
  if (*status != SAI__OK) {
    if (resistance && *resistance) *resistance = astFree( *resistance );
    if (heateff && *heateff) smf_close_file( NULL, heateff, status );
  }

  return;

}

