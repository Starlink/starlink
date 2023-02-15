/*
*+
*  Name:
*     smf_choosepolbins

*  Purpose:
*     Determine a set of polarisation angle bins.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     dim_t ***smf_choosepolbins( Grp *igrp, int size, float binsize,
*                                 float binzero, AstFrameSet *wcsout2d,
*                                 int *npbin, double **pangle, int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     binsize = float (Given)
*        The size of each angle bin, in radians. If this is zero, then
*        all time slices are put into a single bin, which is given an
*        angle value of AST__BAD, and a NULL pointer is returned as the
*        function value.
*     binzero = float (Given)
*        The angle at the centre of the first bin, in radians. A value of
*        zero corresponds to north in the celestial co-ordinate system
*        specified by "wcsout2d".
*     wcsout2d = AstFrameSet * (Given)
*        A pointer to the FrameSet describing the relationship between
*        2D GRID and 2D SKY coordinates in the output cube.
*     npbin = int * (Returned)
*        Pointer to an int in which to return the number of polarisation
*        angle bins used.
*     pangle = double ** (Returned)
*        Pointer to a location at which to return a pointer to a newly
*        allocated array of doubles. Memory for this array is allocated
*        within this function. The array will have "*npbin" elements holding
*        the polarisation angle associated with each bin. This is the angle
*        (in radians) from north in the current Frame of "wcsout2d", to the
*        effective analyser axis. Positive rotation is in the same sense as
*        rotation from the first spatial pixel axis to the second spatial
*        pixel axis.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Returned Value:
*     Pointer to an array with an element for every input NDF, or NULL. Each
*     element holds a pointer to another array that has an element for each
*     polarisation angle bin. Each of these elements holds a pointer to an
*     array of dim_t values which are the zero-based indices for the time slices
*     that contain data for the polarisatiom angle from the input NDF. The length
*     of these final arrays is unspecified, but a value of VAL__MAXK will be
*     stored to mark the end of each array. A NULL pointer is returned if
*     no input NDFs contain any valid POL_ANG values.

*  Description:
*     This function examines the POL_ANG values stored in the JCMTSTATE
*     extension in each input NDF (each time slice in the NDF has its own
*     POL_ANG value). These values are converted to position angles within
*     the celestial co-ordinate system given by "skyfrm". A set of bins is
*     then determined that cover the range of position angle values. The
*     angle associated with each bin is returned, together with lists of the
*     time slices from each input NDF that contribute to each angle bin.
*
*     The memory allocated within this function should be freed using a call
*     to smf_freepolbins.
*
*     If no POL_ANG values are available in the input NDFs, then all time
*     slices are assigned to a single bin that is given a "pangle" value of
*     AST__BAD, and a NULL pointer is returned as the function value. If one
*     or more POL_ANG values are available in the input NDFs, then any time
*     slices that do not have a valid POL_ANG value are excluded from the
*     returned list of time slices to be used.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     12-OCT-2007 (DSB):
*        Initial version.
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     12-FEB-2008 (DSB):
*        - Added argument binzero.
*        - POL_ANG is stored in degrees, not radians.
*     29-JAN-2009 (DSB):
*        Correct size of memory allocation for *pangle.

*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurf_par.h"

dim_t ***smf_choosepolbins( Grp *igrp, int size, float binsize, float binzero,
                            AstFrameSet *wcsout2d, int *npbin, double **pangle,
                            int *status ){

/* Local Variables */
   AstFrame *cfrm = NULL;/* Pointer to output sky frame */
   AstFrame *polfrm = NULL; /* Frame in which POL_ANG is given */
   AstFrameSet *fs = NULL;  /* FrameSet connecting POL_ANG frame and output sky frame */
   AstMapping *tmap1 = NULL;/* Mapping connecting POL_ANG frame and output sky frame */
   AstMapping *tmap2 = NULL;/* Mapping connecting POL_ANG frame and output sky frame */
   char *polcrd = NULL;  /* Name of frame in which POL_ANG values are defined */
   const JCMTState *state = NULL; /* Local pointer to STATE */
   dim_t ***result;      /* Returned array */
   dim_t **r;            /* Pointer to an output array element */
   dim_t *ndftimes = NULL; /* Holds the no. of times slices in each NDF */
   dim_t itime;          /* Index of current time slice */
   dim_t ntime;          /* Total no. of input time slices */
   double **polang_ptr = NULL; /* Holds pointers to arrays of POL_ANG values */
   double *angsum;       /* Pointer to array holding the bin angles */
   double *p;            /* Pointer to next POL_ANG value */
   double ang;           /* Angle subtended by 2 points at a 3rd point */
   double point1[ 2 ];   /* Telescope base pointing position */
   double point2[ 2 ];   /* Offset position along effective analyser axis */
   double point3[ 2 ];   /* Offset position along second frame axis */
   double xin[ 3 ];      /* Sky axis values  */
   double xout[ 3 ];     /* Grid axis values */
   double yin[ 3 ];      /* Sky axis values */
   double yout[ 3 ];     /* Grid axis values */
   int *angcnt;          /* Pointer to array holding the bin counts */
   int *pop;             /* Bin populations for a single NDF */
   int i;                /* Old bin index */
   int ibin;             /* Bin index */
   int ifile;            /* Index of current input file */
   int j;                /* New bin index */
   int maxbin;           /* Max number of bins required */
   int nang;             /* No. of usable POL_ANG values */
   int rot;              /* Are sky and GRID angles in opposite senses? */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Initialise the returned values */
   *npbin = 0;
   *pangle = NULL;
   result = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Begin an AST context. */
   astBegin;

/* If no bin size has been provided assign all time slices to a single
   bin with angle vaue AST__BAD and return a NULL pointer. */
   if( binsize <= 0.0 || binsize == AST__BAD ) goto L999;

/* Get a pointer to the current Frame in the supplied FrameSet (this will be a
   SkyFrame). */
   cfrm = astGetFrame( wcsout2d, AST__CURRENT );

/* Take a copy of the current Frame in the supplied FrameSet. We will later
   modify its attributes so that it describes the frame in which the POL_BIN
   angles are specified. */
   polfrm = astCopy( cfrm );

/* Allocate an array to hold the pointers to the arrays of pol_ang
   values for each NDF. */
   polang_ptr = astMalloc( sizeof( *polang_ptr )*size );
   if( polang_ptr ) {
      for( ifile = 0; ifile < size; ifile++ ) polang_ptr[ ifile ] = NULL;
   }

/* Initialise the number of usable POL_ANG values found so far. */
   nang = 0;

/* Initialise the total number of time slices checked so far. */
   ntime = 0;

/* Indicate that we have not yet determined if rotation from x to y is
   the same as rotation from north to east in the output cube. */
   rot = -1;

/* Allocate memory to hold the number of time slices in each input NDF. */
   ndftimes = astMalloc( size*sizeof(*ndftimes ) );

/* Loop round all the input NDFs. */
   for( ifile = 0; ifile < size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( NULL, igrp, ifile + 1, "READ", 0, &data, status );
      hdr = data->hdr;

/* Store the number of time slices in this input NDF. */
      ndftimes[ ifile ] = (data->dims)[ 2 ];

/* Increment the total number of input time slices. */
      ntime += (data->dims)[ 2 ];

/* Get the co-ordinate system in which the half-wave plate angles are
   given. This is specified by the POL_CRD FITS Header. Get its value,
   passing on if no value is available in the current NDF. */
      if( astGetFitsS( hdr->fitshdr, "POL_CRD", &polcrd ) &&
          astChrLen( polcrd ) > 0 ) {

/* Allocate an array to to store the POL_ANG values for each time slice. */
         p = astMalloc( sizeof( *p )*(data->dims)[ 2 ] );
         polang_ptr[ ifile ] = p;

/* Loop round all the time slices in the input file. */
         for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Get a pointer to the state for this time slice. */
            state = &( (hdr->allState)[ itime ] );

/* If this time slice has no POL_ANG value, store a bad value and pass on. */
            if( state->pol_ang == VAL__BADD ) {
               *(p++) = VAL__BADD;

/* Otherwise, we need to convert the stored value to the spatial co-ordinate
   system of the output cube. Handle each support POL_CRD system (currently
   only AZEL). */
            } else if( !strcmp( polcrd, "AZEL" ) ) {

/* Modify the attributes of "polfrm" so that it describes AZEL
   coordinates at the epoch of the current time slice. */
               astSet( polfrm, "System=AZEL,Epoch=MJD %.*g", DBL_DIG,
                       state->tcs_tai + 32.184/SPD );

/* Get a Mapping from AZEL coords to the output sky coordinate system. */
               fs = astConvert( polfrm, cfrm, "" );
               tmap1 = astGetMapping( fs, AST__BASE, AST__CURRENT );
               tmap2 = astSimplify( tmap1 );

/* Get two AZEL positions, the first is the base telescope pointing
   position and the second is offset by 1 arc-second from the base telescope
   pointing position along the line parallel to the effective analyser axis.
   The effective analyser is a rotating analyser that would have the same
   effect as the combination of fixed analyser and half wave plate that
   is present in the actual polarimeter. We record the effective analyser
   position, because otherwise we would have to record the positions of both
   the  halfwave plate and the fixed analyser. Since the analyser is
   fixed at the elevation axis (or so we assume), and the elevation axis
   rotates on the sky through the course of the observation, the fixed
   analyser also rotates on the sky. But POLPACK requires a fixed
   reference direction. So we choose north in the output sky frame as the
   fixed reference direction and record the effective analyser position with
   respect to north. The following assumes the POL_ANG value is measured
   from EL through AZ, in degrees. */
               point1[ 0 ] = state->tcs_az_bc1;
               point1[ 1 ] = state->tcs_az_bc2;
               (void) astOffset2( polfrm, point1,
                                  AST__DPI/2 - 2.0*AST__DD2R*state->pol_ang,
                                  AST__DD2R/3600.0, point2 );

/* Transform these two positions into the output sky coordinate system. */
               xin[ 0 ] = point1[ 0 ];
               xin[ 1 ] = point2[ 0 ];
               yin[ 0 ] = point1[ 1 ];
               yin[ 1 ] = point2[ 1 ];
               astTran2( tmap2, 2, xin, yin, 1, xout, yout );

/* Find the angle between north in the output frame, and the line joining
   these two positions. Measured positive north through east. */
               point1[ 0 ] = xout[ 1 ];
               point1[ 1 ] = yout[ 1 ];
               point2[ 0 ] = xout[ 0 ];
               point2[ 1 ] = yout[ 0 ];
               point3[ 0 ] = xout[ 0 ];
               point3[ 1 ] = yout[ 0 ] + AST__DD2R/3600.0;
               ang = astAngle( cfrm, point1, point2, point3 );

/* If we have not yet done so, see if rotation from grid axis 1 to grid axis
   2 in the output is in the same sense as rotation from north to east. */
               if( rot == -1 ) {

/* Transform the above 3 points into grid coords. */
                  xout[ 2 ] = point3[ 0 ];
                  yout[ 2 ] = point3[ 1 ];
                  astTran2( wcsout2d, 3, xout, yout, 0, xin, yin );

/* If the angle between these points in the grid frame has the same sign
   as the angle betwen them in the sky frame, then we do not need to negate
   all "ang" values. */
                  point1[ 0 ] = xin[ 1 ];
                  point1[ 1 ] = yin[ 1 ];
                  point2[ 0 ] = xin[ 0 ];
                  point2[ 1 ] = yin[ 0 ];
                  point3[ 0 ] = xin[ 2 ];
                  point3[ 1 ] = yin[ 2 ];
                  if( ang * astAngle( astGetFrame( wcsout2d, AST__BASE ),
                                      point1, point2, point3 ) < 0 ) {
                     rot = 1;
                  } else {
                     rot = 0;
                  }
               }

/* If good, negate the value if necessary, ensure the value is in the range
   0->2.PI, store it, and increment the number of usable POL_ANG values found
   so far. */
               if( ang != AST__BAD ) {
                  if( rot ) ang = -ang;
                  while( ang > 2*AST__DPI ) ang -= 2*AST__DPI;
                  while( ang < 0 ) ang += 2*AST__DPI;
                  *(p++) = ang;
                  nang++;

               } else {
                  *(p++) = AST__BAD;
               }

/* For efficiency, annul expplicitly AST objects created in this tight
   loop. */
               fs = astAnnul( fs );
               tmap1 = astAnnul( tmap1 );
               tmap2 = astAnnul( tmap2 );

/* Report an error for an unknown POL_CRD system. */
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               msgSetc( "POLCRD", polcrd );
               smf_smfFile_msg( data->file, "NDF", 1, "<unknown file>");
               errRep( "", "Unrecognised value '^POLCRD' for POL_CRD "
                       "header in ^NDF.", status );
            }
         }
      }

/* Close the current input data file. */
      smf_close_file( NULL, &data, status);
      data = NULL;

/* Next input NDF. */
   }

/* We now have arrays holding the effective analyser angle for every time
   slice of every input NDF, with respect to north in the output sky
   co-ordinate system. We now assign each time slice to an angle bin. The
   first bin (bin zero) start at the angle given by "binzero" and extends
   east for the angular size given by "binsize". All bins are equal sized
   and abut without overlap. If fewer that 10% of all time slices have usable
   POL_ANG values, issue a warning and skip to the end so that all time
   slices are assined to the same bin. */
   if( nang < 0.1*ntime ) {
      msgOut( "", "Warning: less than 10% of the input data has usable "
              "polarisation angle (POL_ANG) values. Therefore polarisation "
              "angles will be ignored and all input data will be put into "
              "a single output cube.", status );

/* Otherwise... */
   } else {

/* Fnd the maximum number of bins required, and modify the binsize so
   that this maximum number fits exactly into a whole circle. */
      maxbin = (int) 2*AST__DPI/binsize;
      binsize = 2*AST__DPI/maxbin;

/* Allocate memory to hold the sum of all angles in each bin. */
      angsum = astMalloc( maxbin*sizeof( *angsum ) );

/* Allocate memory to hold the count of angles in each bin. */
      angcnt = astMalloc( maxbin*sizeof( *angcnt ) );

/* Initialise the above arrays. */
      if( angcnt ) {
         for( ibin = 0; ibin < maxbin; ibin++ ) {
            angsum[ ibin ] = 0.0;
            angcnt[ ibin ] = 0;
         }
      }

/* Loop round all the input NDFs. */
      for( ifile = 0; ifile < size && *status == SAI__OK; ifile++ ) {

/* Pass on if no usable POL_ANG values were found in this NDF. */
         if( polang_ptr[ ifile ] ) {
            p = polang_ptr[ ifile ];

/* Loop round all the time slices in the input file. */
            for( itime = 0; itime <   ndftimes[ ifile ] && *status == SAI__OK; itime++ ) {

/* Get the bin index for this time slice. */
               ang = *(p++);
               if( ang != AST__BAD ) {
                  ibin = (int) ( ( ang - binzero + 0.5*binsize ) /binsize );
                  while( ibin >= maxbin ) ibin -= maxbin;
                  while( ibin < 0 ) ibin += maxbin;

/* Increment the sums for the bin. */
                  angsum[ ibin ] += ang;
                  angcnt[ ibin ]++;
               }
            }
         }
      }

/* Shuffle values down the "angsum" array to exclude empty bins, and
   convert the sum value into the mean value. Store a map from old
   bin numbers to new bin numbers in the "angcnt" array. */
      j = 0;
      for( i = 0; i < maxbin; i++ ) {
         if( angcnt[ i ] > 0 ) {
            angsum[ j ] = angsum[ i ]/angcnt[ i ];
            angcnt[ i ] = j++;
         } else {
            angcnt[ i ] = -1;
         }
      }

/* The "angsum" array is now the required "pangle" array so return it. */
      *pangle = angsum;
      *npbin = j;

/* Allocate memory to hold the population of each used bin */
      pop = astMalloc( sizeof( *pop )*( *npbin ) );

/* Create the returned arrays. */
      result = astMalloc( size*sizeof( *result ) );

/* Loop round all the input NDFs one more time. */
      for( ifile = 0; ifile < size && *status == SAI__OK; ifile++ ) {

/* Create an array for this input NDF that has one element for each
   angle bin. */
         r = astMalloc( sizeof(*r) * (*npbin) );
         result[ ifile ] = r;

/* Initialise the pointers to the arrays that hold the time slices indices
   and the bin populations. */
         if( r ) {
            for( ibin = 0; ibin < *npbin; ibin++ ) {
               r[ ibin ] = NULL;
               pop[ ibin ] = 0;
            }
         }

/* If this NDF has usable POL_ANG values... */
         if( polang_ptr[ ifile ] ) {
            p = polang_ptr[ ifile ];

/* Loop round all the time slices in the input file. */
            for( itime = 0; itime <   ndftimes[ ifile ] && *status == SAI__OK; itime++ ) {

/* Get the original bin index for this time slice. */
               ang = *(p++);
               if( ang != AST__BAD ) {
                  ibin = (int) ( ( ang - binzero + 0.5*binsize ) /binsize );
                  while( ibin >= maxbin ) ibin -= maxbin;
                  while( ibin < 0 ) ibin += maxbin;

/* Convert this bin index into the index into the list of non-empty bins. */
                  ibin = angcnt[ ibin ];

/* Extend the array holding the list of time slices indices allocated to
   this bin. */
                  r[ ibin ] = astGrow( r[ ibin ], pop[ ibin ] + 1, sizeof( **r) );

/* Store the current time slice index at the end of the list,
   incrementing the population at the same time. */
                  if( r[ ibin ] ) r[ ibin ][ pop[ ibin ]++ ] = itime;
               }
            }
         }

/* Add a final element to each list of time slice indices holding VAL__MAXK.
   This marks the end of the list. */
         if( r ) {
            for( ibin = 0; ibin < *npbin; ibin++ ) {
               r[ ibin ] = astGrow( r[ ibin ], pop[ ibin ] + 1, sizeof( **r ) );
               if( r[ ibin ] ) r[ ibin ][ pop[ ibin ] ] = VAL__MAXK;
            }
         }
      }

/* Free resources (not "angsum" since it is returned to the caller as
   "*pangle") */
      angcnt = astFree( angcnt );
      pop = astFree( pop );
   }

/* Arrive here if an error occurs, or if we are assigning all input data
   to a single pol angle bin. */
L999:;

/* Free resources. */
   if( polang_ptr ) {
      for( ifile = 0; ifile < size; ifile++ ) {
         polang_ptr[ ifile ] = astFree( polang_ptr[ ifile ] );
      }
      polang_ptr = astFree( polang_ptr );
   }
   ndftimes = astFree( ndftimes );

/* If no bins have been produced, assign all input data to a single bin
   with angle value AST__BAD, and return a NULL pointer. */
   if( !result ){
      *npbin = 1;
      *pangle = astMalloc( sizeof( **pangle ) );
      if( *pangle ) **pangle = AST__BAD;
   }

/* End the AST context. */
   astEnd;

/* Return the result. */
   return result;
}
