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
*     smf_flat_params( const smfData * refdata, const char refrespar[],
*                      const char resistpar[], const char methpar[], const char orderpar[],
*                      double * refres, double **resistance, int * nrows,
*                      int * ncols, smf_flatmeth *flatmeth,
*                      int * order, int * status );

*  Arguments:
*     refdata = const smfData * (Given)
*        Reference smfData for checking dimensionality of resistance file.
*        No check is done if NULL.
*     refrespar = const char [] (Given)
*        Name of the parameter to use for the reference resistance value.
*     resistpar = const char [] (Given)
*        Name of the parameter to use for the resistor file (group).
*     methpar = const char [] (Given)
*        Name of the parameter to use to request the flatfield
*        method.
*     orderpar = const char [] (Given)
*        Name of the parameter to use to request the polynomial order.
*        Only used if methpar indicates a polynomial is to be used.
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

void
smf_flat_params( const smfData * refdata, const char refrespar[], const char resistpar[],
                 const char methpar[], const char orderpar[],
                 double * refohms, double **resistance, int * outrows,
                 int * outcols, smf_flatmeth  *flatmeth,
                 int * order, int * status ) {

  size_t j = 0;             /* Counter, index */
  size_t ksize;             /* Size of key map group */
  char method[SC2STORE_FLATLEN]; /* flatfield method string */
  size_t nbols;              /* Number of bolometers */
  int ncols;                 /* number of columns read from resistor data */
  int nrows;                 /* number of rows read from resistor data */
  Grp *resgrp =  NULL;       /* Resistor group */
  AstKeyMap * resmap = NULL; /* Resistor map */


  if (resistance) *resistance = NULL;

  if (*status != SAI__OK) return;
  if (!resistance) {
    *status = SAI__ERROR;
    errRep( "", "Must provide a pointer for 'resistance' "
            "(possible programming error)", status );
    return;
  }

  /* get the reference pixel heater resistance */
  parGdr0d( refrespar, 2.0, 0, VAL__MAXD, 1, refohms, status );

  /* Get the bolometer resistor settings */
  kpg1Gtgrp( resistpar, &resgrp, &ksize, status );
  kpg1Kymap( resgrp, &resmap, status );
  if( resgrp ) grpDelet( &resgrp, status );

  if (*status != SAI__OK) goto CLEANUP;

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
  *resistance = smf_malloc( nbols, sizeof(**resistance),
                            0, status);

  if (*status == SAI__OK) {
    for (j = 0; j < (size_t)ncols; j++) {
      int ngot;
      char colname[6];
      sprintf( colname, "COL%-d", (int)(j+1) );
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
    if (refdata) {
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
  }

  if (outrows) *outrows = nrows;
  if (outcols) *outcols = ncols;

 CLEANUP:
  if (*status != SAI__OK) {
    if (*resistance) *resistance = smf_free( *resistance, status );
  }

  return;

}

