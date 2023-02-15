/*
*+
*  Name:
*     smf_calc_telres

*  Purpose:
*     Return an estimate of the telescope's angular resolution.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_calc_telres( AstFitsChan *hdr, int *status );

*  Arguments:
*     hdr = AstFitsChan * (Given)
*        The FITS header containing details of the observation.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     An estimate of the angular resolution of the telescope, in
*     arc-seconds, rounded up to the nearest half arc-second.

*  Description:
*     This function returns an estimate of the angular resolution of the
*     telescope, in arc-seconds. This is one quarter of the Airy disk
*     radius at the wavelength of the local oscillator, rounded up to the
*     nearest half arc-second.

*  Authors:
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     28-MAR-2008 (DSB)
*        Initial version.
*     2009-10-29 (TIMJ):
*        Add support for SCUBA-2
*     2014-03-28 (TIMJ):
*        Add SMT
*     2014-04-10 (TIMJ):
*        Add NANTEN2
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
*     Copyright (C) 2014 Cornell University.
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
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "smf.h"

#include <string.h>
#include <math.h>

float smf_calc_telres( AstFitsChan *hdr, int *status ) {

/* Local Variables: */
   char *value;
   double dlambda;
   double los;
   double loe;
   float diam = 1.0;
   float lambda = 0;
   float result;
   size_t nc;

/* Initialise the returned value to a safe non-zero value. */
   result = 1.0;

/* Check the inherited status */
   if ( *status != SAI__OK ) return result;

/* Get the name of the telescope. */
   if( astGetFitsS( hdr, "TELESCOP", &value ) ) {

/* Get the used length of the telescope name, excluding any trailing
   spaces. */
      nc = astChrLen( value );

/* Check the telescope is "JCMT". */
      if( !strncmp( value, "JCMT", nc ) ) {

/* Note the telescope diamter, in metres. */
         diam = 15.0;

/* Start by looking for a wavelength header */
         if ( astGetFitsF( hdr, "WAVELEN", &dlambda ) ) {
/* we have a wavelength directly */
           lambda = dlambda;

/* Find the local oscillator frequencies (GHz, topocentric) at the start
   and end of the observation. */
         } else if( ! astGetFitsF( hdr, "LOFREQS", &los ) ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( "", "The \"LOFREQS\" FITS header was not found.",
                       status );
            }


         } else if( ! astGetFitsF( hdr, "LOFREQE", &loe ) ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( "", "The \"LOFREQE\" FITS header was not found.",
                       status );
            }

/* Get the middle LO frequency and convert to a topocentric wavelength in
   metres. */
         } else {
            lambda = 2.0E-9*AST__C/( los + loe );
         }

/* SMT has Supercam */
      } else if ( !strncmp( value, "SMT", nc ) ) {

        /* Diameter in metres */
        diam = 10.0;

        /* Get the LO frequency */
        if (  ! astGetFitsF( hdr, "LOFREQS", &los ) ) {
          if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "The \"LOFREQS\" FITS header was not found.",
                    status );
          }
        }

        lambda = 2.0E-9 * AST__C / los;

      } else if ( !strncmp( value, "NANTEN2", nc ) ) {

        /* Diameter in metres */
        diam = 4.0;

        /* Get the LO frequency */
        if (  ! astGetFitsF( hdr, "LOFREQS", &los ) ) {
          if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "The \"LOFREQS\" FITS header was not found.",
                    status );
          }
        }

        lambda = 2.0E-9 * AST__C / los;

/* Report an error if we do not recognise the telescope. */
      } else {
         msgSetc( "T", value );
         *status = SAI__ERROR;
         errRep( "", "The properties of telescope \"^T\" are not known.",
                 status );
      }

/* Report an error if the telescope is not specified in the header. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "The \"TELESCOP\" FITS header was not found.", status );
   }

/* If all has gone OK, calculate one quarter of the Airy disk radius in
   radians, then convert to arc-seconds and round up to the nearest half
   arc-second. */
   if( *status == SAI__OK ) {
      result = 0.25*( 1.22*lambda/diam );
      result *= AST__DR2D*3600.0;
      result = 0.5*ceil( result/0.5 );

/* Add a context message and return a safe value if an error has occurred. */
   } else if( *status != SAI__OK ) {
      errRep( "", "smf_calc_telres: Could not determine the angular "
              "resolution of the telescope.", status );
      result = 1.0;
   }

/* Return the result. */
   return result;
}

