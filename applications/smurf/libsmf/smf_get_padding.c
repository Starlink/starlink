/*
*+
*  Name:
*     smf_get_padding

*  Purpose:
*     Obtain the amount of zero padding to add to start and end of each
*     bolometer.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_get_padding( AstKeyMap *keymap, int report,
*                               const smfHead *hdr, double steptime,
*                               int *status )

*  Arguments:
*     keymap = AstKeyMap* (Given)
*        keymap containing the user-supplied configuration.
*     report = int (Given)
*        If non-zero, report the default value if it is used.
*     hdr = smfHead *(Given)
*        Used to determine the step size, and the scan velocity when
*        converting spatial scales to frequencies.
*     steptime = double (Given)
*        The step time, in seconds.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function returns the number of zero values to add to the start
*     and end of each bolometer time stream. The returned value is
*     obtained from the PAD value in the supplied keymap. If the keymap
*     does not contain a PAD value, a default value is returned which is
*
*       1/(steptime*freq)
*
*     where "freq" is the lowest frequency specified for any filter edge
*     in the supplied configuration.

*     This function gets the lowest filter edge frequency from the
*     keymap, and returns a value equal to 200/freq. This is
*     approximately the width of the central peak in the sinc smoothing
*     kernel produced by the filtering.

* Returned Value:
*     The default padding, in time samples.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     21-JUL-2010 (DSB):
*        Initial version.
*     4-OCT-2010 (DSB):
*        Add support for spatial filter scales.
*     20-APR-2011 (DSB):
*        - Get the stepsize from the supplied header, rather than using an
*        argument to supply the stepsize.
*        - Take account of any downsampling.
*     2011-04-20 (TIMJ):
*        Trap for a stationary telescope when working out padding.
*     2011-06-22 (EC):
*        Don't need downsampscale since it is in the keymap
*     29-NOV-2011 (DSB):
*        - Correct handling of errors caused by missing items.
*        - Handle downsampli7ng correctly.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 2011 University of British Columbia
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

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "ast_err.h"

/* SMURF includes */
#include "libsmf/smf_err.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smf_get_padding"

dim_t smf_get_padding( AstKeyMap *keymap, int report, const smfHead *hdr,
                       double steptime, int *status ) {

/* Local Variables: */
   AstObject *obj;
   const char *key;
   dim_t pad;
   dim_t result;
   double f_low;
   double filt_edgehigh;
   double filt_edgelarge;
   double filt_edgelow;
   double filt_edgesmall;
   double filt_notchlow[ SMF__MXNOTCH ];
   int f_nnotch;
   int iel;
   int nel;
   int temp;
   double scalelen;
   double downsampscale;
   double downsampfreq;

/* Initialise */
   result = 0;

/* Main routine */
   if (*status != SAI__OK) return result;

/* If the keymap contains a PAD value, return it. */
   if( astMapGet0I( keymap, "PAD", &temp ) ) {
      if( temp < 0 && *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep( "", "PAD cannot be < 0.", status );

      } else {
        result = (dim_t) temp;
      }

/* If the keymap contains no PAD value, calculate a default on the basis
   of the FILT_ values. */
   } else {

/* If the keymap does not contain a PAD value, not even an "<undef>"
   value, an error will have been reported by astMapget0I above. In this
   case, annull the error and continue since the KeyMap may contain FILT_
   values that can be used to calculate a default PAD value. */
      if( *status == AST__MPKER ) errAnnul( status );

/* Search for filtering parameters in the keymap. None of these parameters
   represent a number of time clies, so we can set the smfData (the 2nd
   argument) to NULL. */
      f_nnotch = 0;
      smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, &filt_edgelow, &filt_edgehigh, &filt_edgesmall,
                        &filt_edgelarge, filt_notchlow, NULL, &f_nnotch, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, &downsampscale, &downsampfreq, NULL,
                        NULL, NULL, NULL, NULL, status );

      if( downsampscale && downsampfreq ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": both downsampscale and downsampfreq are set",
                status );
      }

/* If any were not found, annul the error and continue with those that
   were found (plus defaults for the others). */
      if( *status == AST__MPKER ) errAnnul( status );
      if( *status == SAI__OK ) {

/* Modify edge filters if spatial scales were requested */
         smf_scale2freq( filt_edgesmall, filt_edgelarge, hdr, &filt_edgelow,
                         &filt_edgehigh, status );
         if( *status == SMF__TELSTAT ) {
           errAnnul( status );
         }

/* Find the lowest of these frequencies. The lowest frequency will give
   the greatest padding. */
         f_low = ( filt_edgehigh > 0.0 ) ? filt_edgehigh : VAL__MAXD;
         if( filt_edgelow > 0.0 && filt_edgelow < f_low ) f_low = filt_edgelow;
         for( iel = 0; iel < f_nnotch; iel++ ) {
           if( filt_notchlow[ iel ] > 0.0 && filt_notchlow[ iel ] < f_low ) f_low = filt_notchlow[ iel ];
         }

/* If any down-sampling is occurring, set the step time to value implied by
   the requested downsampling factor. This should match the calculation in
   smf_grp_related */
         if( downsampscale || downsampfreq ) {
           if( downsampscale ) {
             scalelen = hdr->steptime * hdr->scanvel / downsampscale;
           } else {
             if( hdr->steptime ) {
               scalelen = downsampfreq / (1./hdr->steptime);
             } else {
               *status = SAI__ERROR;
               scalelen = VAL__BADD;
               errRep( "", FUNC_NAME ": can't resample because smfData has "
                       "unknown sample rate", status );
             }
           }

           if( (*status==SAI__OK) && (scalelen<=SMF__DOWNSAMPLIMIT) )
               steptime = hdr->steptime/scalelen;

/* If no downsampling is happening, use the steptime in the header unless
   another steptime was supplied. */
         } else if( steptime == VAL__BADD ) {
            steptime = hdr->steptime;
         }

/* Find the corresponding padding. */
         if( f_low != VAL__MAXD ) {
            result = 1.0/( steptime * f_low );
         } else {
            result = 0;
         }

/* Now check the supplied keymap for any nested keymaps. Assumes that each
   entry in the supplied KeyMap contain either a primitive value or a scalar
   KeyMap pointer. */
         nel = astMapSize( keymap );
         for( iel = 0; iel < nel; iel++ ) {
            key = astMapKey( keymap, iel );

/* If this entry is a KeyMap (assuming no other class of AST object is
   stored in the KeyMap)... */
            if( astMapType( keymap, key ) == AST__OBJECTTYPE ) {

/* Get a pointer to the KeyMap. */
               (void) astMapGet0A( keymap, key, &obj );

/* Call this function to get the padding implied by the sub-KeyMap. */
               pad = smf_get_padding( (AstKeyMap *) obj, report, hdr,
                                      steptime, status );

/* Use the larger of the two paddings. */
               if( pad > result ) result = pad;
            }
         }

/* If required, report the default value. */
         if( report ) {
            msgSeti( "P", (int) result );
            msgOutif( MSG__VERB, "", "Padding each time stream with ^P zero "
                      "values at start and end.", status );
         }
      }
   }

/* Return the result. */
   return result;
}
