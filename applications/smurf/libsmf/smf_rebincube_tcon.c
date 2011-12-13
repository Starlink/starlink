/*
*+
*  Name:
*     smf_rebincube_tcon

*  Purpose:
*     Find the exposure times and Tsys pointer for a time slice.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const double *smf_rebincube_tcon( smfHead *hdr, dim_t itime, double fcon,
*                                       float *texp, float *teff, double *tcon,
*                                       int *status );

*  Arguments:
*     hdr = smfData * (Given)
*        Pointer to the input smfHead structure.
*     fcon = double (Given)
*        The ratio of the squared backend degradation factor to the spectral
*     itime = dim_t (Given)
*        Index of the time slice.
*     texp = float * (Returned)
*        Address of location at which to store the total exposure
*        time for the time slice.
*     teff = float * (Returned)
*        Address of location at which to store the effective exposure
*        time for the time slice.
*     tcon = double * (Returned)
*        Address of location at which to store the conversion factor from
*        Tsys value to Variance for spectra form the specified time slice.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Returned Value:
*     Pointer to the first Tsys value for the time slice.

*  Description:
*     Returns the total and effective exposure times for the current time
*     slice (described by the supplied header structure)a time slice, and
*     a pointer to the start of the Tsys array for the time slice.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2006 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_tcon"

const double *smf_rebincube_tcon( smfHead *hdr, dim_t itime, double fcon,
                                  float *texp, float *teff, double *tcon,
                                  int *status ){

/* Local Variables */
   float toff;                 /* Off time */
   float ton;                  /* On time */

/* Initialise */
   *teff = VAL__BADR;
   *texp = VAL__BADR;
   *tcon = VAL__BADD;

/* Check the inherited status. */
   if( *status != SAI__OK ) return NULL;

/* Assign state to the correct slice of allState */
   hdr->state = &((hdr->allState)[itime]);

/* Note the total exposure time (texp), and the total effective time
   (texp), for all the input spectra produced by this time slice. */
   ton = hdr->state->acs_exposure;
   if( ton == 0.0 ) ton = VAL__BADR;

   toff = hdr->state->acs_offexposure;
   if( toff == 0.0 ) toff = VAL__BADR;

   if( ton != VAL__BADR && toff != VAL__BADR ) {
      *texp = ton + toff;
      *teff = 4*ton*toff/( ton + toff );

/* Find the constant factor associated with the current time slice, that
   allows conversion between Tsys and variance. */
      if( fcon != VAL__BADD ) *tcon = 4*fcon/(*teff);
   }

/* Return a pointer to the start of the Tsys values for this time slice. */
   return hdr->tsys + hdr->ndet*itime;

}
