      SUBROUTINE KPS1_PRFMK( MODE, NDIM, DIM, INDAT, VAR, INVAR, IWCS,
     :                       NCAX, ARRDIM, NPOS, POS, GEO, ID0, IDS,
     :                       PARAM,TITLE, NP, IPPDAT, IPPVAR, BADD,
     :                       BADV, STATUS )
*+
*  Name:
*     KPS1_PRFMK

*  Purpose:
*     Create a vector holding a 1-D profile through a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PRFMK( MODE, NDIM, DIM, INDAT, VAR, INVAR, IWCS, NCAX, ARRDIM,
*                      NPOS, POS, GEO, ID0, IDS, PARAM, TITLE, NP, IPPDAT,
*                      IPPVAR, BADD, BADV, STATUS )

*  Description:
*     This routine produces a 1-D array of data and variances values by
*     sampling the supplied data using nearest neighbour interplation.
*     The sample positions are determined using the method specified by
*     argument MODE:
*
*     POINTS - NPOS samples are produced placed at the NPOS positions
*     supplied in array POS.
*
*     CURVE - Samples are placed at evenly spaced intervals along a 1-D
*     curve defined in the Current Frame of a supplied data array. The
*     curve is defined by the NPOS positions supplied in array POS. These
*     positions are joined in the fashion of a poly-curve, using either
*     geodesic line segments or Euclidean line segments. The total number
*     of samples along the entire profile can be specified by the caller,
*     or a default value equal to the appoximate length of the curve in
*     pixels can be found and used.
*
*     The supplied FrameSet describing the supplied data arrays is modified
*     by remapping its Base Frame so that the Base Frame on exit is a 1-D
*     GRID Frame corresponding to the single GRID co-ordinate in the returned
*     1-D data arrays.
*
*     An output positions list may be created holding the position at
*     each sample.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        The method by which sample positions are determined; "POINTS" or
*        "CURVE".
*     NDIM = INTEGER (Given)
*        The number of dimensions in the supplied arrays.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension in the supplied data arrays.
*     INDAT( * ) = DOUBLE PRECISION (Given)
*        The input data array. Its size should equal the product of the
*        values supplied in DIM.
*     VAR = LOGICAL (Given)
*        Are input variances available?
*     INVAR( * ) = DOUBLE PRECISION (Given)
*        The input variance array. Its size should equal the product of the
*        values supplied in DIM. Only accessed if VAR is .TRUE.
*     IWCS = INTEGER (Given)
*        An AST pointer to the WCS FrameSet associated with the input
*        arrays. The Base Frame should be GRID co-ordinates within the
*        input arrays. The Base Frame is re-mapped on exit so that it
*        corresponds to GRID co-ordinates in the returned 1-D profile
*        arrays. Also, the PIXEL Frame is given the new Domain PIXEL_ND
*        (so long as there is not already a Frame with this Domain in the
*        FrameSet).
*     NCAX = INTEGER (Given)
*        The number of axes in the Current Frame of IWCS.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of POS.
*     NPOS = INTEGER (Given)
*        The number of positions given along the profile. Less than or
*        equal to ARRDIM.
*     POS( ARRDIM, NCAX ) = DOUBLE PRECISION (Given)
*        The co-ordinates at NPOS positions along the profile, in the
*        Current Frame of IWCS.
*     GEO = LOGICAL (Given)
*        Should the supplied positions on the profile be connected using
*        geodesic curves in the Current Frame of IWCS? If not, the profile
*        is made up of straight line segments in the Current Frame of IWCS.
*        Not accessed if MODE is POINTS.
*     ID0 = INTEGER (Given)
*        The integer position identifier for the first position. Subsequent
*        identifiers increase by 1 for each position. If this is supplied
*        less than or equal to zero, position identidiers are read from
*        array IDS instead.
*     IDS( NPOS ) = INTEGER (Given)
*        The integer position identifiers for each position. Not accessed
*        if ID0 is less than or equal to zero.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of a parameter to use when creating an output positions
*        list holding the positions at each sample.
*     TITLE = CHARACTER * ( * ) (Given)
*        The NDFs title.
*     NP = INTEGER (Given and Returned)
*        The number of values in the returned array. If this is zero on
*        entry, then a default value is used equal to the approximate length
*        of the profile in pixels. Otherwise, the supplied value is used.
*        If MODE is POINTS then NP will always be returned equal to NPOS.
*     IPPDAT = INTEGER (Returned)
*        A pointer to a _DOUBLE array holding the NP profile data values.
*     IPPVAR = INTEGER (Returned)
*        A pointer to a _DOUBLE array holding the NP profile variance values.
*        Returned equal to IPPDAT if VAR is .FALSE.
*     BADD = LOGICAL (Returned)
*        Were any bad values included in the returned profile data array?
*     BADV = LOGICAL (Returned)
*        Were any bad values included in the returned profile variance data?
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER MODE*(*)
      INTEGER NDIM
      INTEGER DIM( NDIM )
      DOUBLE PRECISION INDAT( * )
      LOGICAL VAR
      DOUBLE PRECISION INVAR( * )
      INTEGER IWCS
      INTEGER NCAX
      INTEGER ARRDIM
      INTEGER NPOS
      DOUBLE PRECISION POS( ARRDIM, NCAX )
      LOGICAL GEO
      INTEGER ID0
      INTEGER IDS( NPOS )
      CHARACTER PARAM*(*)
      CHARACTER TITLE*(*)

*  Arguments Given and Returned:
      INTEGER NP

*  Arguments Returned:
      INTEGER IPPDAT
      INTEGER IPPVAR
      LOGICAL BADD
      LOGICAL BADV

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION KPG1_ASDIS! Distance between 2 positions

*  Local Constants:
      INTEGER NN                 ! No. of positions to use when finding
      PARAMETER ( NN = 500 )     ! length of curve in GRID Frame.

*  Local Variables:
      CHARACTER TTL*80           ! Positions list title
      DOUBLE PRECISION BLEN      ! Length of profile in GRID Frame
      DOUBLE PRECISION CLEN      ! Length of profile in Current Frame
      DOUBLE PRECISION DELTA     ! Increment between samples
      DOUBLE PRECISION INTVL     ! Interval between GRID Frame sample positions
      DOUBLE PRECISION PINTVL    ! Previous value of INTVL
      INTEGER BFRM               ! Pointer to Base Frame in supplied FrameSet
      INTEGER CFRM               ! Pointer to Current Frame in supplied FrameSet
      INTEGER CMPMAP             ! Pointer to the new total mapping
      INTEGER I                  ! Loop index
      INTEGER IAT                ! No. of characters in a string
      INTEGER IBASE              ! Index of Base Frame within supplied FrameSet
      INTEGER ICURR              ! Index of Current Frame within supplied FrameSet
      INTEGER IPCPOS             ! Pointer to array of Current Frame positions
      INTEGER IPGPOS             ! Pointer to array of Base Frame positions
      INTEGER IPIX               ! Pixel Frame index
      INTEGER J                  ! Axis index
      INTEGER LUTMAP             ! Pointer to a LutMap for an indidual axis
      INTEGER SMAP               ! Simplified Mapping from IWCS
      INTEGER MAP                ! Pointer to the "running total" Mapping
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation array
      INTEGER PMAP               ! Pointer to PermMap to feed 1 i/p to NDIM o/ps

*  Set up the output axis permutation array used to re-map the supplied Base
*  Frame so that the single 1-D GRID co-ordinate is fed to all output axes.
      DATA OUTPRM/NDF__MXDIM*1/
*.

*  Initialise.
      IPPDAT = 0
      IPPVAR = 0
      BADD = .FALSE.
      BADV = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise pointers.
      IPCPOS = 0
      IPGPOS = 0

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Check all supplied current Frame positions are good.
      DO J = 1, NCAX
         DO I = 1, NPOS

            IF( POS( I, J ) .EQ. AST__BAD ) THEN
               STATUS = SAI__ERROR

               IF( ID0 .GT. 0 ) THEN
                  CALL MSG_SETI( 'I', ID0 + I - 1 )
               ELSE
                  CALL MSG_SETI( 'I', IDS( I ) )
               END IF

               CALL ERR_REP( 'KPS1_PRFMK_ERR', 'Profile position #^I '//
     :                       'is invalid within the current '//
     :                       'co-ordinate Frame of the supplied NDF.',
     :                       STATUS )
               GO TO 999
            END IF

         END DO
      END DO

*  Get a simplified Mapping from Base to Current Frame.
      SMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                     AST__CURRENT, STATUS ),
     :                     STATUS )

*  Store a pointer to the Current Frame of the supplied FrameSet.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  In POINTS mode we always produce NPOS samples.
      IF( MODE .EQ. 'POINTS' ) THEN
         NP = NPOS

*  In CURVE mode, we need the total length of the lines joining the profile
*  positions in the Current Frame.
      ELSE
         CLEN = 0.0D0
         DO I = 2, NPOS
            DELTA = KPG1_ASDIS( CFRM, ARRDIM, NCAX, POS, I - 1,
     :                          I, GEO, STATUS )
            IF( DELTA .NE. AST__BAD ) CLEN = CLEN + DELTA
         END DO
      END IF

*  If the caller has not specified how many samples to place along the
*  profile, find the default value equal to the (approximate) length of
*  the profile in the GRID Frame (i.e. in pixels).
*  ====================================================================
      IF( NP .LE. 0 ) THEN

*  Allocate memory to hold the Current Frame co-ordinates at NN positions
*  evenly spaced (in the Current Frame) along the profile.
         CALL PSX_CALLOC( NCAX*NN, '_DOUBLE', IPCPOS, STATUS )

*  Allocate memory to hold the corresponding GRID Frame co-ordinates.
         CALL PSX_CALLOC( NDIM*NN, '_DOUBLE', IPGPOS, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find NN positions evenly spaced (in the Current Frame) along the profile.
         DELTA = CLEN/DBLE( NN - 1 )
         CALL KPG1_ASSMP( CFRM, ARRDIM, NCAX, NPOS, POS, GEO, NN,
     :                    DELTA, %VAL( CNF_PVAL( IPCPOS ) ), STATUS )

*  Transform these positions into the GRID Frame.
         CALL AST_TRANN( SMAP, NN, NCAX, NN, %VAL( CNF_PVAL( IPCPOS ) ),
     :                   .FALSE., NDIM, NN, %VAL( CNF_PVAL( IPGPOS ) ),
     :                   STATUS )

*  Store a pointer to the Base Frame of the supplied FrameSet.
         BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

*  Find the total length of the lines joining these GRID Frame positions.
*  As an attempt to reduce the effect of any discontinuities in the
*  Mapping, ignore any intervals which are greater than 20 times the length
*  of the previous interval.
         BLEN = 0.0D0
         PINTVL = 1.0D4

         DO I = 2, NN

            INTVL = KPG1_ASDIS( BFRM, NN, NDIM,
     :                          %VAL( CNF_PVAL( IPGPOS ) ),
     :                          I - 1, I, .FALSE., STATUS )

            IF( INTVL .NE. AST__BAD ) THEN
               IF( INTVL .LT. 20.0D0*PINTVL ) BLEN = BLEN + INTVL
               PINTVL = INTVL
            END IF

         END DO

*  The default number of samples to use is the length of the profile in
*  pixels.
         NP = MAX( 2, INT( BLEN ) + 1 )

*  Free the memory.
         CALL PSX_FREE( IPCPOS, STATUS )
         CALL PSX_FREE( IPGPOS, STATUS )

      END IF

*  Find the data and variance values at the required sample positions.
*  ===================================================================

*  Allocate memory to hold the sample GRID Frame co-ordinates.
      CALL PSX_CALLOC( NDIM*NP, '_DOUBLE', IPGPOS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If we are using POINTS mode, transform the supplied Current Frame
*  positions into GRID Frame.
      IF( MODE .EQ. 'POINTS' ) THEN
         CALL AST_TRANN( SMAP, NP, NCAX, ARRDIM, POS, .FALSE., NDIM, NP,
     :                   %VAL( CNF_PVAL( IPGPOS ) ), STATUS )

*  In CURVE mode we need to find the sample positions.
      ELSE

*  Allocate memory to hold the Current Frame co-ordinates at NP positions
*  evenly spaced (in the Current Frame) along the profile.
         CALL PSX_CALLOC( NCAX*NP, '_DOUBLE', IPCPOS, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the increment between each of the required NP positions.
         DELTA = CLEN / DBLE( NP - 1 )

*  Find the required NP positions in the Current Frame.
         CALL KPG1_ASSMP( CFRM, ARRDIM, NCAX, NPOS, POS, GEO, NP, DELTA,
     :                    %VAL( CNF_PVAL( IPCPOS ) ), STATUS )

*  Transform these positions into the GRID Frame.
         CALL AST_TRANN( SMAP, NP, NCAX, NP, %VAL( CNF_PVAL( IPCPOS ) ),
     :                   .FALSE., NDIM, NP, %VAL( CNF_PVAL( IPGPOS ) ),
     :                   STATUS )
      END IF

*  Allocate memory to hold the returned data values, at the NP positions.
      CALL PSX_CALLOC( NP, '_DOUBLE', IPPDAT, STATUS )

*  If required, allocate memory to hold the corresponding variance values.
      IF( VAR ) THEN
         CALL PSX_CALLOC( NP, '_DOUBLE', IPPVAR, STATUS )
      ELSE
         IPPVAR = IPPDAT
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Sample the input array using nearest-neighbour interpolation at each
*  of the GRID positions.
      CALL KPS1_PRFSM( NDIM, DIM, INDAT, VAR, INVAR, NP,
     :                 %VAL( CNF_PVAL( IPGPOS ) ),
     :                 %VAL( CNF_PVAL( IPPDAT ) ),
     :                 %VAL( CNF_PVAL( IPPVAR ) ), BADD, BADV,
     :                 STATUS )

*  Re-map the Base Frame of the supplied FrameSet so that it describes the
*  returned 1-D array.
*  =======================================================================

*  Create a LutMap which transforms 1-D GRID position in the returned 1-D
*  array into values on the first Base Frame axis. On return, the row
*  within %VAL( IPGPOS ) holding the LUT values is replaced by an array
*  of GRID co-ordinate values within the LUT (i.e. 1.0, 2.0, 3.0, etc).
      CALL KPS1_PRFLT( 1, NP, NDIM, %VAL( CNF_PVAL( IPGPOS ) ),
     :                 MAP, STATUS )

*  Loop round all remaining Base Frame axes.
      DO I = 2, NDIM

*  Create a LutMap which transforms 1-D GRID position in the returned 1-D
*  array into values on the current Base Frame axis.
         CALL KPS1_PRFLT( I, NP, NDIM, %VAL( CNF_PVAL( IPGPOS ) ),
     :                    LUTMAP, STATUS )

*  Combine the current total Mapping with this LutMaps in parallel, and then
*  annul the individual pointers.
         CMPMAP = AST_CMPMAP( MAP, LUTMAP, .FALSE., ' ', STATUS )
         CALL AST_ANNUL( MAP, STATUS )
         CALL AST_ANNUL( LUTMAP, STATUS )
         MAP = CMPMAP

      END DO

*  We now have a CmpMap with NDIM inputs all of which correspond to the
*  same single Grid co-ordinate in the returned 1-D array. Create a PermMap
*  with 1 input and NDIM outputs which will feed all the CmpMap inputs with
*  the same value.
      PMAP = AST_PERMMAP( 1, 1, NDIM, OUTPRM, 0.0D0, ' ', STATUS )

*  Combine the PermMap and the previous CmpMap in series to get the Mapping
*  from 1-D GRID co-ords to the Base Frame in the supplied FrameSet.
      CMPMAP = AST_CMPMAP( PMAP, MAP, .TRUE., ' ', STATUS )
      CALL AST_ANNUL( PMAP, STATUS )
      CALL AST_ANNUL( MAP, STATUS )
      MAP = CMPMAP

*  Note the index of the Base and Current Frames in the supplied FrameSet.
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Add a 1-D Frame with Domain GRID into the supplied FrameSet, using
*  the inverse of the above Mapping to connect the original Base Frame to
*  the new Frame. The new Frame becomes the Current Frame.
      CALL AST_INVERT( MAP, STATUS )
      CALL AST_ADDFRAME( IWCS, IBASE, MAP,
     :                   AST_FRAME( 1, 'DOMAIN=GRID', STATUS ), STATUS )

*  Make the new Frame the Base Frame within the FrameSet.
      CALL AST_SETI( IWCS, 'BASE', AST_GETI( IWCS, 'CURRENT', STATUS ),
     :               STATUS )

*  Re-instate the original Current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Remove the original Base Frame.
      CALL AST_REMOVEFRAME( IWCS, IBASE, STATUS )

*  Now find the PIXEL Frame and give it the new Domain "PIXEL_ND" so long
*  as no Frame in the FrameSet already has this Domain.
      CALL KPG1_ASFFR( IWCS, 'PIXEL_ND', IPIX, STATUS )
      IF( IPIX .EQ. AST__NOFRAME ) THEN

         CALL KPG1_ASFFR( IWCS, 'PIXEL_ND', IPIX, STATUS )

         IF( IPIX .NE. AST__NOFRAME ) THEN
            CALL AST_SETC( AST_GETFRAME( IWCS, IPIX, STATUS ), 'Domain',
     :                     'PIXEL_ND', STATUS )
         END IF

      END IF

*  Create an output positions list holding the position at each sample.
*  ====================================================================
*  Create the title for the positions list.
      TTL = 'Output from PROFILE'
      IAT = 19
      IF( TITLE .NE. ' ' ) THEN
         CALL CHR_APPND( ' -', TTL, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( TITLE, TTL, IAT )
      END IF

*  Create the positions list itself.
      CALL KPG1_WRLST( PARAM, NP, NP, 1, %VAL( CNF_PVAL( IPGPOS ) ),
     :                 AST__BASE,
     :                 IWCS, TTL( : IAT ), 1, 0, .TRUE., STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Free the memory holding the sub-sampled GRID and Current Frame profile
*  positions.
      IF( IPGPOS .NE. 0 ) CALL PSX_FREE( IPGPOS, STATUS )
      IF( IPCPOS .NE. 0 ) CALL PSX_FREE( IPCPOS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, free the returned arrays, etc.
      IF( STATUS .NE. SAI__OK ) THEN
         IF( IPPDAT .NE. 0 ) CALL PSX_FREE( IPPDAT, STATUS )
         IF( IPPVAR .NE. 0 .AND. VAR ) CALL PSX_FREE( IPPVAR, STATUS )
         IPPDAT = 0
         IPPVAR = 0
         BADD = .FALSE.
         BADV = .FALSE.
      END IF

      END
