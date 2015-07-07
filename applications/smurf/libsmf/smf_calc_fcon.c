
/*
*+
*  Name:
*     smf_calc_fcon

*  Purpose:
*     Calculate a factor needed for converting input Tsys values into
*     Variance values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double smf_calc_fcon( smfData *data, dim_t nchan, int report,
*                           AstMapping **specmap, AstFrame **specframe,
*                           int *status )

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     nchan= int (Given)
*        The number of spectral channels in the input.
*     report = int (Given)
*        If non-zero, then report an error if the conversion factor
*        cannot be found.
*     specmap = AstMapping ** (Returned)
*        GRID->Spectral Mapping for current input file.
*     specframe = AstFrame ** (Returned)
*        SpecFrame describing input spectral axis.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     This function returns the ratio of the squared backend degradation
*     factor to the input spectral channel width, needed for calculating
*     the variances from the Tsys value. It is independent of the time slice.
*     The total factor for converting Tsys to Variance is the product of the
*     factor returned by this function, and the time-slice dependent factor
*     returned by smf_rebincube_tcon.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     9-JUL-2008 (DSB):
*        Initial version (extracted from smf_rebincube).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

double smf_calc_fcon( smfData *data, dim_t nchan, int report,
                      AstMapping **specmap, AstFrame **specframe,
                      int *status ){

/* Local Variables */
   AstCmpMap *fmap = NULL;     /* Mapping from spectral grid to topo freq Hz */
   AstFitsChan *fc = NULL;     /* FitsChan used to get spectral WCS from input */
   AstFrame *specframe2 = NULL;/* Temporary copy of SpecFrame in input WCS */
   AstFrameSet *fs = NULL;     /* WCS FramesSet from input */
   AstMapping *fsmap = NULL;   /* Mapping extracted from FrameSet */
   char *fftwin = NULL;        /* Name of FFT windowing function */
   double at;                  /* Frequency at which to take the gradient */
   double dnu;                 /* Channel width in Hz */
   double fcon2;               /* Variance factor for file */
   double gin[2];              /* Input grid coords */
   double gout[2];             /* Output grid coords */
   double k;                   /* Back-end degradation factor */
   int gotbf;                  /* Have required FITS keywords been obtained? */
   int gotdnu = 0;             /* Has spectral channel width been obtained? */
   int pixax[ 3 ];             /* Pixel axis indices */
   int specax;                 /* The index of the input spectral axis */
   smfHead *hdr;

/* Check the inherited status. */
   *specframe = NULL;
   *specmap = NULL;
   fcon2 = AST__BAD;
   if( *status != SAI__OK ) return fcon2;

/* Begin an AST context.*/
   astBegin;

/* Get a pointer to the header. */
   hdr = data->hdr;

/* We want a description of the spectral WCS axis in the input file. If
   the input file has a WCS FrameSet containing a SpecFrame, use it,
   otherwise we will obtain it from the FITS header later. NOTE, if we knew
   that all the input NDFs would have the same spectral axis calibration,
   then the spectral WCS need only be obtained from the first NDF. However,
   in the general case, I presume that data files may be combined that use
   different spectral axis calibrations, and so these differences need to
   be taken into account. */
   if( hdr->tswcs ) {
      fs = astClone( hdr->tswcs );

/* The first axis should be a SpecFrame. See if this is so. If not annul
   the specframe pointer. */
      specax = 1;
      *specframe = astPickAxes( fs, 1, &specax, NULL );
      if( !astIsASpecFrame( *specframe ) ) *specframe = astAnnul( *specframe );
   }

/* If the above did not yield a SpecFrame, use the FITS-WCS headers in the
   FITS extension of the input NDF. Take a copy of the FITS header (so that
   the contents of the header are not changed), and then read a FrameSet
   out of it. */
   if( !*specframe ) {
      fc = astCopy( hdr->fitshdr );
      astClear( fc, "Card" );
      fs = astRead( fc );

/* Extract the SpecFrame that describes the spectral axis from the current
   Frame of this FrameSet. This is assumed to be the third WCS axis (NB
   the different axis number). */
      specax = 3;
      *specframe = astPickAxes( fs, 1, &specax, NULL );
   }

/* Split off the 1D Mapping for this single axis from the 3D Mapping for
   the whole WCS. This results in "specmap" holding the Mapping from
   SpecFrame value to GRID value. */
   fsmap = astGetMapping( fs, AST__CURRENT, AST__BASE );
   astMapSplit( fsmap, 1, &specax, pixax, specmap );

/* Invert the Mapping for the spectral axis so that it goes from input GRID
   coord to spectral coord. */
   astInvert( *specmap );

/* Find the constant factor associated with the current input file, used
   when converting Tsys values to variance values. This is the squared
   backend degradation factor, divided by the noise bandwidth. Get the
   required FITS headers, checking they were found. */
   if( astGetFitsF( hdr->fitshdr, "BEDEGFAC", &k ) &&
       astGetFitsS( hdr->fitshdr, "FFT_WIN", &fftwin ) ){
      gotbf = 1;

/* Get a Mapping that converts values in the input spectral system to
   topocentric frequency in Hz, and concatenate this Mapping with the
   Mapping from input GRID coord to the input spectral system. The result
   is a Mapping from input GRID coord to topocentric frequency in Hz. */
      specframe2 = astCopy( *specframe );
      astSet( specframe2, "system=freq,stdofrest=topo,unit=Hz" );
      fmap = astCmpMap( *specmap, astGetMapping( astConvert( *specframe,
                                                            specframe2,
                                                            "" ),
                                                AST__BASE, AST__CURRENT ),
                        1, " " );
      fmap = astSimplify( fmap );

/* Find the topocentric channel width in Hz at the mid channel. */
      at = 0.5*( 1.0 + nchan );
      gin[ 0 ] = at - 0.5;
      gin[ 1 ] = at + 0.5;
      astTran1( fmap, 2, gin, 1, gout );
      if( gout[ 0 ] != AST__BAD && gout[ 1 ] != AST__BAD ) {
         dnu = abs( gout[ 0 ] - gout[ 1 ] );
         gotdnu = 1;

/* Modify the channel width to take account of the effect of the FFT windowing
   function. Allow undef value because FFT_WIN for old data had a broken value
   in hybrid subband modes. */
         dnu = fabs( dnu );

         if( !strcmp( fftwin, "truncate" ) ) {
            dnu *= 1.0;

         } else if( !strcmp( fftwin, "hanning" ) ) {
            dnu *= 1.5;

         } else if( !strcmp( fftwin, "<undefined>" ) ) {
            dnu *= 1.0;

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "W", fftwin );
            smf_smfFile_msg( data->file, "F", 1, "<unknown file>");
            errRep( "", "FITS header FFT_WIN has unknown value "
                    "'^W' in ^F (programming error).", status );
         }

/* Form the required constant. */
         fcon2 = k*k/dnu;

      } else {
         gotdnu = 0;
         fcon2 = VAL__BADD;
      }

   } else {
      gotbf = 0;
      fcon2 = VAL__BADD;
   }

/* If we need the input variances, but we cannot calculate the input
   variances, report an error. */
   if( report && fcon2 == VAL__BADD ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         smf_smfFile_msg( data->file, "F", 1, "<unknown file>");
         errRep( "", "Cannot calculate input variances for ^F",
                 status );
         if( !gotbf ) {
            errRep( "", "BEDEGFAC and/or FFT_WIN is missing from the FITS "
                    "header", status );
         } else if( !gotdnu ) {
            errRep( "", "The topocentric spectral channel width cannot be "
                    "found.", status );
         }
      }
   }

/* Export the returned AST pointers so they are not annulled by the
   following call to astEnd. */
   astExport( *specmap );
   astExport( *specframe );

/* End the AST context.*/
   astEnd;

/* Return the Tsys->variance conversion factor. */
   return fcon2;
}
