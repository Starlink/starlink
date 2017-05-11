/*
*+
*  Name:
*     smf_check_detpos

*  Purpose:
*     Check that the RECEPPOS and FPLANEX/Y positions are consistent.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_check_detpos( smfData *data, float mxerr, int report,
*                                int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the smfData structure holding the data to be
*        checked.
*     mxerr = float (Given)
*        The maximum separation allowed, in arc-seconds.
*     report = int (Given)
*        If greater than zero, then a warning message is reported if the
*        RECEPPOS and FPLANEX/Y positions are not consistent (i.e. have a
*        separataion larger than "mxerr"). If less than zero, then an
*        error is reported if the RECEPPOS and FPLANEX/Y positions are
*        not consistent.
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     Non-zero if the RECEPPOS and FPLANEX/Y positions are consistent, and
*     zero otherwise.

*  Description:
*     This function checks that the detector positions implied by the
*     RECEPPOS and FLPANEX/Y values in the supplied data structure are
*     consistent. It converts the detector positions in the first time
*     slice into sky positions using the RECEPPOS values, and then does
*     the same again using the FPLANEX/Y values. It then finds the maximum
*     discrepancy on the sky between the converted detector positions.
*     If this discrepancy is more than "mxerr" arc-seconds, a warning
*     message or error is issued (if "report" is non-zero), and a zero value
*     is returned as the function value. If the detector positions in the
*     first time slice cannot be determined, subsequent time slices are
*     checked until one is found that can be checked succesfully.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-APR-2009 (DSB):
*        Initial version.
*     17-APR-2009 (DSB):
*        Avoid problems caused by breaking out of time slice loop.
*     25-NOV-2013 (DSB):
*        Allow an error to be reported instead of a warning.
*     11-MAY-2017 (DSB):
*        Add argument "mxerr". Previously the max error was hard-wired at
*        1 arc-second.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009,2013 Science & Technology Facilities Council.
*     Copyright (C) 2017 East Asian Observatory.
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
#include "smurf_par.h"
#include "smf_err.h"
#include "smf.h"

int smf_check_detpos( smfData *data, float mxerr, int report, int *status ){

/* Local Variables */
   AstFrame *frm = NULL;      /* Sky Frame from input WCS FrameSet */
   AstFrameSet *fs = NULL;    /* FPLANEX/Y WCS FrameSet matching RECEPPOS */
   AstMapping *tmap1 = NULL;  /* FPLANEX/Y WCS Mapping matching RECEPPOS */
   AstMapping *tmap2 = NULL;  /* FPLANEX/Y WCS Mapping matching RECEPPOS */
   double *old_detpos = NULL; /* Pointer to old detpos array */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout_f = NULL;/* Workspace for detector FPLANEX/Y sky positions */
   double *xout_r = NULL;/* Workspace for detector RECEPPOS sky positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout_f = NULL;/* Workspace for detector FPLANEX/Y sky positions */
   double *yout_r = NULL;/* Workspace for detector RECEPPOS sky positions */
   double a[ 2 ];        /* A RECEPPOS sky position */
   double b[ 2 ];        /* An FPLANEX/Y sky position */
   double dist;          /* Discrepancy between RECEPPOS and FPLANEX/Y */
   double max_dist;      /* Max discrepancy between RECEPPOS and FPLANEX/Y */
   int done;             /* Has a test been performed yet? */
   int ibase;            /* Original index of Base Frame in WCS FrameSet */
   int result;           /* The result of the check */
   size_t irec;          /* Index of current input detector */
   size_t itime;         /* Index of current time slice */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Initialise. */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Get a pointer to the data header structure. */
   hdr = data->hdr;

/* The consistency check is only possible if the supplied header contains
   RECEPPOS values. */
   if( hdr->detpos ) {

/* Begin an AST context. */
      astBegin;

/* Allocate work arrays to hold the GRID coords of all the detectors in the
   supplied data structure. */
      xin = astMalloc( (data->dims)[ 1 ]*sizeof( *xin ) );
      yin = astMalloc( (data->dims)[ 1 ]*sizeof( *yin ) );

/* Store the input GRID coords of the detectors. The second GRID axis is
   a dummy axis that is set to 1.0 for all detectors. */
      if( *status == SAI__OK ) {
         for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
            xin[ irec ] = irec + 1.0;
            yin[ irec ] = 1.0;
         }
      }

/* Allocate work arrays to hold the SKY coords of all the detectors, as
   determined using the FPLANEX/Y values. */
      xout_f = astMalloc( (data->dims)[ 1 ]*sizeof( *xout_f ) );
      yout_f = astMalloc( (data->dims)[ 1 ]*sizeof( *yout_f ) );

/* Allocate work arrays to hold the SKY coords of all the detectors, as
   determined using the RECEPPOS values. */
      xout_r = astMalloc( (data->dims)[ 1 ]*sizeof( *xout_r ) );
      yout_r = astMalloc( (data->dims)[ 1 ]*sizeof( *yout_r ) );

/* Loop round all the time slices in the input data structure until we
   have found one that can be checked. */
      done = 0;
      for( itime = 0; !done && itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* We first create a WCS FrameSet for the time slice, based on the RECEPPOS
   values. The smf_tslice_ast function uses the value of the hdr->detpos
   pointer (which points to an array holding the RECEPPOS values) to
   determine how the returned FrameSet should be created. If hdr->detpos is
   not NULL, the RECEPPOS values are used - otherwise the FPLANEX/Y values
   are used. The returned FrameSet describes the spatial coordinate systems
   associated with the current time slice. The base frame in the FrameSet
   will be a 2D Frame in which axis 1 is detector number and axis 2 is
   unused. The current Frame will be a SkyFrame (the SkyFrame System may be
   any of the JCMT supported systems). The Epoch will be set to the epoch of
   the time slice. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );

/* Use the FrameSet to transform the input receptor GRID positions into
   SKY coords. */
         if( hdr->wcs ) {
            astTran2( hdr->wcs, (data->dims)[ 1 ], xin, yin, 1, xout_r,
                                                                yout_r );

/* Note the current (sky) Frame. */
            frm = astGetFrame( hdr->wcs, AST__CURRENT );

/* Temporarily nullify the detpos array pointer in the smfHead structure.
   This will cause the next invocation of smf_tslice_ast to use the
   FPLANEX/Y values. */
            old_detpos = hdr->detpos;
            hdr->detpos = NULL;

/* We now create a second WCS FrameSet for the time slice based on the
   FPLANEX/Y values. */
            smf_tslice_ast( data, itime, 1, NO_FTS, status );

/* Create a FrameSet (fs) that gives sky coords in the same system as the first
   (RECEPPOS) FrameSet. Ensure the original FrameSet (hdr->wcs) is left
   unchanged. */
            if( hdr->wcs ) {
               ibase = astGetI( hdr->wcs, "Base" );
               astInvert( hdr->wcs );
               fs = astConvert( hdr->wcs, frm, " " );
               astInvert( hdr->wcs );
               astSetI( hdr->wcs, "Base", ibase );

/* Use the above FrameSet to transform the input receptor GRID positions into
   the required SKY coords. */
               if( fs ) {
                  tmap1 = astGetMapping( fs, AST__BASE, AST__CURRENT );
                  tmap2 = astSimplify( tmap1 );
                  astTran2( tmap2, (data->dims)[ 1 ], xin, yin, 1, xout_f,
                                                                   yout_f );

/* Loop round each detector, finding the maximum distance on the sky
   between the RECEPPOS and FPLANEX/Y positions. */
                  frm = astGetFrame( hdr->wcs, AST__CURRENT );
                  max_dist = VAL__MIND;
                  for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
                     a[ 0 ] = xout_r[ irec ];
                     a[ 1 ] = yout_r[ irec ];
                     b[ 0 ] = xout_f[ irec ];
                     b[ 1 ] = yout_f[ irec ];
                     dist = astDistance( hdr->wcs, a, b );
                     if( dist != AST__BAD && dist > max_dist ) max_dist = dist;
                  }

/* If this distance is more than "mxerr" arc-seconds, report a warning or error. */
                  if( max_dist != VAL__MIND ) {
                     result = ( max_dist*AST__DR2D <= mxerr/3600.0 );
                     if( ! result && report ) {
                        smf_smfFile_msg( data->file, "FILE", 1, "<unknown file>" );
                        msgSetr( "MAX", (float)( max_dist*AST__DR2D*3600.0) );
                        msgSeti( "T", itime + 1 );

                        if( report > 0 ) {
                           msgOutif( MSG__QUIET, " ", "   WARNING: The detector "
                                     "positions implied by the RECEPPOS and FPLANEX/Y "
                                     "values within '^FILE' (time slice ^T) differ "
                                     "by up to ^MAX arc-seconds.", status );
                           msgBlank( status );

                        } else if( *status == SAI__OK ) {
                           *status = SMF__RCPPOS;
                           errRep( " ", "The detector positions implied by "
                                   "the RECEPPOS and FPLANEX/Y values within "
                                   "'^FILE' (time slice ^T) differ by up "
                                   "to ^MAX arc-seconds.", status );
                        }
                     }

/* Leave the time slice loop once a test has been completed succesfully. */
                    done = 1;
                  }

/* For efficincy within this loop, annul AST objects explicitly. */
                  fs = astAnnul( fs );
                  tmap1 = astAnnul( tmap1 );
                  tmap2 = astAnnul( tmap2 );
               }
            }
            frm = astAnnul( frm );

/* Re-instate the detpos pointer in the header. */
            hdr->detpos = old_detpos;
         }
      }

/* Free memory. */
      xin = astFree( xin );
      yin = astFree( yin );
      xout_r = astFree( xout_r );
      yout_r = astFree( yout_r );
      xout_f = astFree( xout_f );
      yout_f = astFree( yout_f );

/* End the AST context. This will annul all AST objects created within the
   context. */
      astEnd;
   }

/* Return the result of the check. */
   return result;
}

