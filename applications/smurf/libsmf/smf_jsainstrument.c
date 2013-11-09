/*
*+
*  Name:
*     smf_jsainstrument

*  Purpose:
*     Allow the user to select an instrument and get its JSA tiling
*     parameters.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_jsainstrument( const char *param, AstFitsChan *fc,
*                        smf_inst_t def, smfJSATiling *tiling,
*                        int *status )

*  Arguments:
*     param = const char * (Given)
*        The name of the ADAM parameter to use - e.g. "INSTRUMENT". May
*        be NULL, in which case the tiling for the default instrument
*        implied by "fc" and "def" is returned.
*     fc = AstFitsChan (Given)
*        If not NULL, any INSTRUME and FILTER values in this FitsChan
*        will be used to determine the default instrument.
*     def = smf_inst_t (Given)
*        The default instrument. Only used if a default cannot be
*        determined from the supplied FitsChan.
*     tiling = smfJSATiling * (Returned)
*        Pointer to a structure in which to return the parameters
*        defining the layout of JSA tiles for the selected instrument.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This allows the user to choose a JCMT instrument using the
*     specified ADAM parameter, and returns a structure holding the
*     parameters that define the layout of JSA tiles for that instrument.
*     A FitsChan can be supplied to specify the default instrument.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2013 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ast.h"
#include "par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "jsatiles.h"

void smf_jsainstrument( const char *param, AstFitsChan *fc, smf_inst_t def,
                        smfJSATiling *tiling, int *status ){

/* Local Variables: */
   char text[ 200 ];
   const char *instrume;
   char *cval;
   smf_inst_t instrument;

/* Check the inherited status */
   if (*status != SAI__OK) return;

/* If a FitsChan was supplied, we use it to over-ride the supplied default
   instrument, if possible. */
   if( fc ) {

/* Get the INSTRUME header, and (if SCUBA-2) the FILTER header. */
      astGetFitsS( fc, "INSTRUME", &cval );

/* Compare to known values, and set the appropriate default instrument. */
      if( astChrMatch( cval, "HARP" ) ) {
         def = SMF__INST_HARP;

      } else if( astChrMatch( cval, "SCUBA-2" ) ) {
         astGetFitsS( fc, "FILTER", &cval );

         if( !strcmp( cval, "450" ) ) {
            def = SMF__INST_SCUBA_2_450;

         } else if( !strcmp( cval, "850" ) ) {
            def = SMF__INST_SCUBA_2_850;

         } else if( *status == SAI__OK ){
            *status = SAI__ERROR;
            errRepf( "", "The input SCUBA-2 NDF has an unknown value "
                    "'%s' for the FILTER keyword.", status, cval );
            errFlush( status );
         }

      } else if( astChrLen( cval ) > 0 ) {
         *status = SAI__ERROR;
         errRepf( "", "The input NDF is for a currently unsupported "
                 "instrument '%s'.", status, cval );
      }

/* Flush any error so that we can continue with no default. */
      if( *status != SAI__OK ) errFlush( status );
   }

/* If no parameter was supplied, just use the default instrument. */
   if( !param ) {
      instrument = def;

/* Otherwise, get the JSA instrument name to use, using the above selection
   as the default. */
   } else {

/* Get the string correspnding to the default instrument. */
      if( def == SMF__INST_SCUBA_2_450 ) {
         instrume = "SCUBA-2(450)";
      } else if( def == SMF__INST_SCUBA_2_850 ) {
         instrume = "SCUBA-2(850)";
      } else if( def == SMF__INST_HARP ) {
         instrume = "HARP";
      } else if( def == SMF__INST_RXA ) {
         instrume = "RXA";
      } else if( def == SMF__INST_RXWD ) {
         instrume = "RxWD";
      } else if( def == SMF__INST_RXWB ) {
         instrume = "RxWB";
      } else {
         instrume = "";
      }

/* Get the user's choice. */
      parChoic( param, instrume, "SCUBA-2(450),SCUBA-2(850),"
                "HARP,RxA,RxWD,RxWB", 1, text, sizeof(text), status );

/* Convert it to an integer identifier. */
      if( !strcmp( text, "SCUBA-2(450)" ) ) {
         instrument = SMF__INST_SCUBA_2_450;

      } else if( !strcmp( text, "SCUBA-2(850)" ) ) {
         instrument = SMF__INST_SCUBA_2_850;

      } else if( !strcmp( text, "HARP" ) ) {
         instrument = SMF__INST_HARP;

      } else if( !strcmp( text, "RXA" ) ) {
         instrument = SMF__INST_RXA;

      } else if( !strcmp( text, "RxWD" ) ) {
         instrument = SMF__INST_RXWD;

      } else if( !strcmp( text, "RxWB" ) ) {
         instrument = SMF__INST_RXWB;

      } else {
         instrument = SMF__INST_NONE;
      }
   }

/* Get the parameters of the tiling scheme used by the requested
   instrument. */
   smf_jsatiling( instrument, tiling, status );

}
