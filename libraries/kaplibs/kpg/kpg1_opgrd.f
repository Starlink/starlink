      SUBROUTINE KPG1_OPGRD( NPOS, POS, WEST, PAR, STATUS )
*+
*  Name:
*     KPG1_OPGRD

*  Purpose:
*     Get the parameters of an optimal projection for a given set of sky
*     positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGRD( NPOS, POS, WEST, PAR, STATUS )

*  Description:
*     This routine calculates the parameters of a tangent plane projection
*     that gives an optimal representation of a set of supplied sky
*     positions. The projection parameters are the normal FITS CRPIX1/2,
*     CRVAL1/2, CDELT1/2 and CROTA2 parameters. They are chosen in order
*     to maximise the number of sky positions that fall close to the
*     centre of a pixel.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     POS( 2, NPOS ) = DOUBLE PRECISION (Given)
*        The sky positions. These should be (longitude,latitude) values, in 
*        radians, in any celestial coordinate system.
*     WEST = LOGICAL (Given)
*        If .TRUE., then it is assumed that the X grid axis increases
*        westwards (assuming north is parallel to +ve Y). This is the
*        case for most celestial coordinate systems such as (RA,Dec) etc. 
*        If .FALSE., then it is assumed that the X grid axis increases
*        eastwards (assuming north is parallel to +ve Y). This is the
*        case for a few systems such as (az,el) and geographic 
*        (longitude,latitude).
*     PAR( 7 ) = DOUBLE PRECISION (Returned)
*        The optimal projection parameters, in the order CRPIX1, CRPIX2,
*        CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2. CRPIX1 and CRPIX2 are 
*        in units of pixels. All the other projection parameters will be 
*        in units of radians, and refer to the celestial coodinate 
*        system in which the POS values are supplied. CROTA2 is the angle
*        from celestial north to the Y axis, measured through west if
*        WEST is .FALSE., and through east otherwise. Returned pixel
*        sizes are rounded to the nearest tenth of an arc-second.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Arguments Given:
      INTEGER NPOS
      DOUBLE PRECISION POS( 2, NPOS )
      LOGICAL WEST

*  Arguments Returned:
      DOUBLE PRECISION PAR( 7 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPAIN              ! Work space pointer
      INTEGER IPXOUT             ! Work space pointer
      INTEGER IPBIN              ! Work space pointer
      INTEGER IPYOUT             ! Work space pointer
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Allocate work arrays.
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPAIN, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPBIN, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPXOUT, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPYOUT, STATUS )

*  Call a subroutine to do the work.
      CALL KPG1_OPGR1( NPOS, POS, WEST, PAR, %VAL( CNF_PVAL( IPAIN ) ),
     :                 %VAL( CNF_PVAL( IPBIN ) ), 
     :                 %VAL( CNF_PVAL( IPXOUT ) ),
     :                 %VAL( CNF_PVAL( IPYOUT ) ), STATUS )

*  Free work arrays.
      CALL PSX_FREE( IPAIN, STATUS )
      CALL PSX_FREE( IPBIN, STATUS )
      CALL PSX_FREE( IPXOUT, STATUS )
      CALL PSX_FREE( IPYOUT, STATUS )

      END




      SUBROUTINE KPG1_OPGR1( NPOS, POS, WEST, PAR, AIN, BIN, XOUT, YOUT, 
     :                       STATUS )
*+
*  Name:
*     KPG1_OPGR1

*  Purpose:
*     Do the work for KPG1_OPGRD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGR1( NPOS, POS, WEST, PAR, AIN, BIN, XOUT, YOUT, STATUS )

*  Description:
*     This routine calculates the parameters of a tangent plane projection
*     that gives an optimal representation of a set of supplied sky
*     positions. The projection parameters are the normal FITS CRPIX1/2,
*     CRVAL1/2, CDELT1/2 and CROTA2 parameters. They are chosen in order
*     to maximise the number of sky positions that fall close to the
*     centre of a pixel.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     POS( 2, NPOS ) = DOUBLE PRECISION (Given)
*        The sky positions. These should be (longitude,latitude) values, in 
*        radians, in any celestial coordinate system.
*     WEST = LOGICAL (Given)
*        If .TRUE., then it is assumed that the X grid axis increases
*        westwards (assuming north is parallel to +ve Y). This is the
*        case for most celestial coordinate systems such as (RA,Dec) etc. 
*        If .FALSE., then it is assumed that the X grid axis increases
*        eastwards (assuming north is parallel to +ve Y). This is the
*        case for a few systems such as (az,el) and geographic 
*        (longitude,latitude).
*     PAR( 7 ) = DOUBLE PRECISION (Returned)
*        The optimal projection parameters, in the order CRPIX1, CRPIX2,
*        CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2. CRPIX1 and CRPIX2 are 
*        in units of pixels. All the other projection parameters will be 
*        in units of radians, and refer to the celestial coodinate 
*        system in which the POS values are supplied. CROTA2 is the angle
*        from celestial north to the Y axis, measured through west if
*        WEST is .FALSE., and through east otherwise. Returned pixel
*        sizes are rounded to the nearest tenth of an arc-second.
*     AIN( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     BIN( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     XOUT( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     YOUT( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NPOS
      DOUBLE PRECISION POS( 2, NPOS )
      LOGICAL WEST 

*  Arguments Returned:
      DOUBLE PRECISION PAR( 7 )

*  Arguments Given and Returned:
      DOUBLE PRECISION AIN( NPOS )
      DOUBLE PRECISION BIN( NPOS )
      DOUBLE PRECISION XOUT( NPOS )
      DOUBLE PRECISION YOUT( NPOS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PIXSCL    ! Initial pixel scale (rads per pixel)
      PARAMETER ( PIXSCL = 0.0003D0 )

*  Local Variables:
      DOUBLE PRECISION ANG
      DOUBLE PRECISION ANG0
      DOUBLE PRECISION DIAM
      DOUBLE PRECISION MXAMP
      DOUBLE PRECISION MXANG
      DOUBLE PRECISION MXWAVE
      DOUBLE PRECISION NPAR1
      DOUBLE PRECISION NPAR2
      DOUBLE PRECISION PAMP
      DOUBLE PRECISION PANG
      DOUBLE PRECISION PWAVE
      DOUBLE PRECISION SDX
      DOUBLE PRECISION SDX2
      DOUBLE PRECISION SDY
      DOUBLE PRECISION SDY2
      DOUBLE PRECISION SPC
      DOUBLE PRECISION SPC0
      DOUBLE PRECISION XC
      DOUBLE PRECISION XHI
      DOUBLE PRECISION XLO
      DOUBLE PRECISION YANG
      DOUBLE PRECISION YC
      DOUBLE PRECISION YHI
      DOUBLE PRECISION YLO
      INTEGER FC
      INTEGER FS
      INTEGER HISTSZ
      INTEGER I
      INTEGER IANG
      INTEGER IGOOD
      INTEGER IPHIST
      INTEGER MAP
      INTEGER ND
      LOGICAL MORE
      LOGICAL OPTY          
      REAL DX
      REAL DY
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      PWAVE = 0.0D0
      PANG = 0.0D0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Copy the longitude and latitude values into separate arrays, and find 
*  the index of the first good supplied sky position.
      IGOOD = 0
      DO I = 1, NPOS
         AIN( I ) = POS( 1, I )
         BIN( I ) = POS( 2, I )
         IF( IGOOD .EQ. 0 .AND. 
     :       POS( 1, I ) .NE. AST__BAD .AND.
     :       POS( 2, I ) .NE. AST__BAD ) THEN
            IGOOD = I
         END IF
      END DO

*  Report an error if no good positions were supplied.
      IF( IGOOD .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_OPGRD: No good sky positions found.',
     :                 STATUS )
         GO TO 999
      END IF

*  Set up an initial projection. This uses 1 arc-min square pixels with 
*  north upwards, and X either east or west. The first good supplied position 
*  is used as the reference point, and this point is placed at grid coords 
*  (1,1).
      PAR( 1 ) = 1.0D0
      PAR( 2 ) = 1.0D0
      PAR( 3 ) = POS( 1, IGOOD )
      PAR( 4 ) = POS( 2, IGOOD )

      IF( WEST ) THEN
         PAR( 5 ) = -PIXSCL
      ELSE
         PAR( 5 ) = PIXSCL
      END IF

      PAR( 6 ) = PIXSCL
      PAR( 7 ) = 0.0D0

*  Create an AST Mapping describing this initial projection. Use the above 
*  values to define a FITS-WCS header in a FitsChan (converting radian
*  values to degrees as needed by FITS), and then read a FrameSet from it, 
*  and recover the base->current Mapping from the FrameSet.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE1', 'RA---TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE2', 'DEC--TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX1', PAR( 1 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX2', PAR( 2 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL1', PAR( 3 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL2', PAR( 4 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT1', PAR( 5 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT2', PAR( 6 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CROTA2', PAR( 7 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_CLEAR( FC, 'Card', STATUS )

      FS = AST_READ( FC, STATUS )

      IF( FS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_OPGRD: Failed to read FrameSet from'//
     :                 ' FitsChan (programming error).', STATUS )
         GO TO 999
      END IF

      MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )

*  Use the AST Mapping to transform the supplied sky positions into the 
*  the grid coordinate system described by the initial projection. 
      CALL AST_TRAN2( MAP, NPOS, AIN, BIN, .FALSE., XOUT, YOUT, STATUS )
 
*  Find the bounding box of the supplied positions within the initial
*  grid coordinate system.
      XHI = VAL__MIND
      YHI = VAL__MIND
      XLO = VAL__MAXD
      YLO = VAL__MAXD

      DO I = 1, NPOS
         IF( XOUT( I ) .NE. AST__BAD .AND.
     :       YOUT( I ) .NE. AST__BAD ) THEN
            XHI = MAX( XHI, XOUT( I ) )
            YHI = MAX( YHI, YOUT( I ) )
            XLO = MIN( XLO, XOUT( I ) )
            YLO = MIN( YLO, YOUT( I ) )
         END IF
      END DO

*  Report an error if no good pixel positions found. 
      IF( XHI .EQ. VAL__MIND ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'KPG1_OPGRD: No good grid positions '//
     :                    'found (programming error).', STATUS )
         END IF
         GO TO 999
      END IF 

*  Find the diameter of the circle enclosing the bounding box, in units of 
*  initial grid pixels.
      DIAM = SQRT( ( XHI - XLO )**2 + ( YHI - YLO )**2 )

*  Calculate what the grid spacing would be (in units of initial grid
*  pixels) if the supplied positions were distributed on a regularly 
*  spaced grid over the bounding box.
      SPC = SQRT( ( XHI - XLO )*( YHI - YLO ) ) / 
     :            ( SQRT( REAL( NPOS ) ) - 1.0 )

*  Store the grid coords at the centre of the bounding box.
      XC = 0.5*( XHI + XLO )
      YC = 0.5*( YHI + YLO )

*  Allocate a 1D work array which spans the circle enclosing the bounding box
*  but using smaller pixels (10 pixels to each regular grid space).
      SPC = 0.1D0*SPC
      HISTSZ = DIAM/SPC
      CALL PSX_CALLOC( HISTSZ, '_REAL', IPHIST, STATUS )

*  Store the grid coordinate within this work array that will correspond to
*  the central position (XC,YC).
      SPC0 = DBLE( ( HISTSZ + 1 )/2 )

*  Imagine a line passing through the centre position (XC,YC). We step 
*  through all orientations of this line in units of 3 degrees. Zero angle
*  corresponds to the second axis in the initial projection (i.e. celestial 
*  north), and the first axis of the initial projection (either east or west) 
*  is at angle of +90 degrees.
      MXAMP = -1.0
      MXWAVE = 0.0
      DO IANG = 0, 177, 3
         ANG = IANG*AST__DD2R

*  For the current line orientation, project every initial grid position onto 
*  the line, and record where about the projected point falls on the line.
*  The 1D work array allocated earlier is used as a histogram to bin the 
*  number of projected points falling on each point of the line. Once all 
*  points have been projected onto the line, at the current orientation, 
*  the amplitude of any periodicity shown in the array is determined, together
*  with the wavelength of the periodicity (in units of initial grid pixels).
*  The details of the orientation with the strongest periodicity are
*  retained in MXAMP, MXANG and MXWAVE.
         CALL KPG1_OPGR2( NPOS, XOUT, YOUT, ANG, SPC, XC, YC, SPC0,
     :                    .TRUE., HISTSZ, %VAL( CNF_PVAL( IPHIST ) ),
     :                    MXAMP, MXWAVE, MXANG, STATUS )

*  Next orientation.
      END DO

*  Now do a finer search through a cone of angles centred on the rough angle
*  found above. The cone is 3 degrees wide and we use 0.1 degree intervals. 
*  In this loop the IANG variable is angular offset from ANG0 in units of 
*  0.1 degree. This time, we use linear interpolation to create the 
*  histogram, to get a more accurate result.
      ANG0 = MXANG
      MXAMP = -1.0
      MXWAVE = 0.0
	    
      DO IANG = -15, 15
         ANG = ANG0 + 0.1*DBLE( IANG )*AST__DD2R
         CALL KPG1_OPGR2( NPOS, XOUT, YOUT, ANG, SPC, XC, YC, SPC0, 
     :                    .TRUE., HISTSZ, %VAL( CNF_PVAL( IPHIST ) ),
     :                    MXAMP, MXWAVE, MXANG, STATUS )
      END DO

*  Calculate the wavelength of the periodicity at right angles to the above 
*  chosen direction.
      PAMP = -1.0
      CALL KPG1_OPGR2( NPOS, XOUT, YOUT, MXANG + AST__DPIBY2, 
     :                 SPC, XC, YC, SPC0, .TRUE., HISTSZ, 
     :                 %VAL( CNF_PVAL( IPHIST ) ), 
     :                 PAMP, PWAVE, PANG, STATUS )

*  The orientation with the most prominent periodicity can be used as
*  either the X or Y axis in the returned optimal grid, and can be either
*  the +ve or -ve direction along the axis. We choose the axis and
*  direction in order to put the +ve Y axis in the returned grid as close
*  as possible to celestial north. So get the position angle (+ve Y through 
*  +ve X in the initial grid) of the optimal Y axis, and set a flag
*  indicating if the periodicity is associated with the optimal X or Y
*  grid axis.
      IF( MXANG .LE. 0.5*AST__DPIBY2 ) THEN
         YANG = MXANG
         OPTY = .TRUE.

      ELSE IF( MXANG .LE. 1.5*AST__DPIBY2 ) THEN
         YANG = MXANG - AST__DPIBY2
         OPTY = .FALSE.

      ELSE IF( MXANG .LE. 2.5*AST__DPIBY2  ) THEN
         YANG = MXANG - 2*AST__DPIBY2
         OPTY = .TRUE.

      ELSE IF( MXANG .LE. 3.5*AST__DPIBY2 ) THEN
         YANG = MXANG - 3*AST__DPIBY2
         OPTY = .FALSE.

      ELSE
         YANG = MXANG 
         OPTY = .TRUE.

      END IF         

*  Find the celestial coordinates at the centre of the bounding box.
      CALL AST_TRAN2( MAP, 1, XC, YC, .TRUE., XOUT, YOUT, STATUS )

*  Report an error if the celestial coords are bad. 
      IF( XOUT( 1 ) .EQ. VAL__MIND ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'KPG1_OPGRD: Cannot transform central '//
     :                    'position (programming error).', STATUS )
         END IF
         GO TO 999
      END IF 

*  Store the parameters of the optimal grid. The reference point is the
*  centre of the bounding box found above, and is put at GRID coords (1,1)
      PAR( 1 ) = 1.0
      PAR( 2 ) = 1.0
      PAR( 3 ) = XOUT( 1 )
      PAR( 4 ) = YOUT( 1 )

      IF( OPTY ) THEN
         PAR( 5 ) = -MXWAVE*PIXSCL
         PAR( 6 ) = PWAVE*PIXSCL
      ELSE
         PAR( 5 ) = -PWAVE*PIXSCL
         PAR( 6 ) = MXWAVE*PIXSCL
      END IF


      PAR( 7 ) = YANG

      IF( .NOT. WEST ) THEN
         PAR( 5 ) = -PAR( 5 )
         PAR( 7 ) = -PAR( 7 )
      END IF

*  Ensure CROTA is in the range 0 -> 2*PI
      DO WHILE( PAR( 7 ) .LT. 0.0 ) 
         PAR( 7 ) = PAR( 7 ) + 2*AST__DPI
      END DO

      DO WHILE( PAR( 7 ) .GE. 360.0 ) 
         PAR( 7 ) = PAR( 7 ) - 2*AST__DPI
      END DO

*  Round the pixel sizes to the closest 10th of an arc-second.
      PAR( 5 ) = NINT( PAR( 5 )*AST__DR2D*36000.0 )/(36000.0*AST__DR2D)
      PAR( 6 ) = NINT( PAR( 6 )*AST__DR2D*36000.0 )/(36000.0*AST__DR2D)

*  We now find a small (less than one pixel) change to PAR(1) and
*  PAR(2) that causes more samples to be projected to the centre of the
*  corresponding pixel. First create a Mapping from the above projection
*  parameters and use it to map the supplied sky positions into grid coords.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE1', 'RA---TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE2', 'DEC--TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX1', PAR( 1 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX2', PAR( 2 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL1', PAR( 3 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL2', PAR( 4 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT1', PAR( 5 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT2', PAR( 6 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CROTA2', PAR( 7 )*AST__DR2D, ' ', .TRUE., 
     :                   STATUS )
      CALL AST_CLEAR( FC, 'Card', STATUS )

      FS = AST_READ( FC, STATUS )

      IF( FS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_OPGRD: Failed to read FrameSet from'//
     :                 ' FitsChan (programming error).', STATUS )
         GO TO 999
      END IF

      MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )
      CALL AST_TRAN2( MAP, NPOS, AIN, BIN, .FALSE., XOUT, YOUT, STATUS )

*  Find the fractional pixel offsets that result in the minimum squared
*  deviation between sample positions and the centres of the corresponding
*  pixels. 
      CALL KPG1_OPGR4( NPOS, XOUT, YOUT, DX, DY, STATUS )

*  Modify the CRPIX values ( PAR(1) and PAR(2) ) accordingly.
      NPAR1 = PAR( 1 ) - DX
      NPAR2 = PAR( 2 ) - DY

*  Find the sky coords corresponding to this position, and use them as
*  the new CRVAL1/2 values ( PAR(3) and PAR(4) ).
      CALL AST_TRAN2( MAP, 1, NPAR1, NPAR2, .TRUE., PAR( 3 ), PAR( 4 ), 
     :                STATUS )

*  Normalise them into range (0->2PI, -PI/2->PI/2)
      IF( PAR( 4 ) .GT. AST__DPIBY2 ) THEN
         PAR( 4 ) = AST__DPI - PAR( 4 ) 
         PAR( 3 ) = AST__DPI + PAR( 3 )

      ELSE IF( PAR( 4 ) .LT. -AST__DPIBY2 ) THEN
         PAR( 4 ) = -AST__DPI - PAR( 4 ) 
         PAR( 3 ) = AST__DPI + PAR( 3 )

      END IF

      IF( PAR( 3 ) .LT. 0.0 ) THEN
         PAR( 3 ) = PAR( 3 ) + 2*AST__DPI 

      ELSE IF( PAR( 3 ) .GT. 2*AST__DPI ) THEN
         PAR( 3 ) = PAR( 3 ) - 2*AST__DPI 

      END IF

*  Find the pixel bounds of the new bounding box.
      XHI = VAL__MIND
      YHI = VAL__MIND
      XLO = VAL__MAXD
      YLO = VAL__MAXD

      DO I = 1, NPOS
         IF( XOUT( I ) .NE. AST__BAD .AND.
     :       YOUT( I ) .NE. AST__BAD ) THEN
            XHI = MAX( XHI, XOUT( I ) )
            YHI = MAX( YHI, YOUT( I ) )
            XLO = MIN( XLO, XOUT( I ) )
            YLO = MIN( YLO, YOUT( I ) )
         END IF
      END DO

*  Put the reference point (which is close to the centre of the bounding
*  box on the sky) at the central pixel position.
      PAR( 1 ) = NINT( XHI - XLO )/2 + 1
      PAR( 2 ) = NINT( YHI - YLO )/2 + 1

*  Free resources
 999  CONTINUE
      CALL PSX_FREE( IPHIST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END



      SUBROUTINE KPG1_OPGR2( NPOS, X, Y, ANG, SPC, XC, YC, SPC0, LIN,
     :                       HISTSZ, HIST, MXAMP, MXWAVE, MXANG, 
     :                       STATUS )
*+
*  Name:
*     KPG1_OPGR2

*  Purpose:
*     Check for periodicity at a given orientation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGR2( NPOS, X, Y, ANG, SPC, XC, YC, SPC0, LIN, HISTSZ, 
*                      HIST, MXAMP, MXWAVE, MXANG, STATUS )

*  Description:
*     This routine projects the supplied grid position onto a line at the
*     orientation given by ANG, passing through the point given by (XC,YC).
*     The projected positions along this line are recorded in a histogram
*     in which each bin corresponds to a section of the line, and the
*     count in each bin is the number of points projected onto the section 
*     of the line corresponding to the bin.
*
*     Once all positions have been projected, the sum of the squared 
*     histogram values is found (this is the same as the auto-correlation
*     at zero shift), and used as a measure of the strength of the 
*     periodicity. 
*
*     If the periodicity at this angle is greater than the supplied value
*     in MXAMP, then the wavelength of the periodicity is found and
*     returned in MXWAVE. The wavelength is found by evaluating the
*     auto-correlation of the histogram at increasing shifts until the
*     first significant peak is found. The corresponding shift is taken
*     as the wavelength of the periodicity.
*
*     A square regular grid of positions will have strong periodicity when 
*     projected onto a line parallel to its X and Y directions. But it
*     will also have equally strong periodicity when projected onto a line 
*     at 45 degrees to either its X or Y directions. However, in this
*     second case the pixel sizes (i.e. the wavelength of the periodicity)
*     will be smaller than in the first case. We use this fact to
*     distinguish between the two cases. Once the periodicity of the
*     wavelength has been found, a further check is made before accepting
*     the orientation as better than the supplied MXANG orientation. This
*     check consists of multiplying the strength of the periodicity by
*     the area of the pixel, and only accepting the new orientation if
*     this value is greater than the supplied MXANG*MXWAVE*MXWAVE value.
*     Since the pixel area will be smaller if the line is oriented at 45
*     degrees to an axis, than if it is parallel to the axis, the pixel
*     area factor will result in the parallel case being selected rather
*     than the 45 degrees case.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     X( NPOS ) = DOUBLE PRECISION (Given)
*        The X grid coordinates.
*     Y( NPOS ) = DOUBLE PRECISION (Given)
*        The Y grid coordinates.
*     ANG = DOUBLE PRECISION (Given)
*        The angle of the line, in radians. Measured from the grid Y axis 
*        through the grid X axis.
*     SPC = DOUBLE PRECISION (Given)
*        The length of each bin in the histogram, in units of grid pixels.
*     XC = DOUBLE PRECISION (Given)
*        The X grid coord of a point on the line.
*     YC = DOUBLE PRECISION (Given)
*        The Y grid coord of a point on the line.
*     SPC0 = DOUBLE PRECISION (Given)
*        The grid coordinate within the HIST array, onto which the
*     LIN = LOGICAL (Given)
*        Should the histogram be formed using nearest neighbour or linear
*        interpolation?
*     HISTSZ = INTEGER (Given)
*        The length of the histogram array.
*     HIST( HISTSZ ) = REAL (Given and Returned)
*        The array to use to hold the histogram.
*     MXAMP = DOUBLE PRECISION (Given and Returned)
*        Measures the strength of the periodicity in the histogram. It is
*        updated on exit if the angle specified by "ANG" demonstrates 
*        stronger periodicity than the supplied value. 
*     MXWAVE = DOUBLE PRECISION (Given and Returned)
*        The wavelength (in grid pixels) of the spatial frequency at the 
*        first peak in the auto-correlation function. Updated if the 
*        value at the first peak of the auto-correlation function for the 
*        supplied angle, is greater than the supplied MXAMP value.
*     MXANG = DOUBLE PRECISION (Given and Returned)
*        The angle (in radians) which produced the maximum MXAMP value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER NPOS
      DOUBLE PRECISION X( NPOS )
      DOUBLE PRECISION Y( NPOS )
      DOUBLE PRECISION ANG
      DOUBLE PRECISION SPC
      DOUBLE PRECISION XC
      DOUBLE PRECISION YC
      DOUBLE PRECISION SPC0
      LOGICAL LIN
      INTEGER HISTSZ

*  Arguments Given and Returned:
      REAL HIST( HISTSZ )
      DOUBLE PRECISION MXAMP
      DOUBLE PRECISION MXWAVE
      DOUBLE PRECISION MXANG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COSANG
      DOUBLE PRECISION D
      DOUBLE PRECISION FBIN
      DOUBLE PRECISION LSUM2
      DOUBLE PRECISION NEWAMP
      DOUBLE PRECISION NEWSPA
      DOUBLE PRECISION NEWWAV
      DOUBLE PRECISION POW
      DOUBLE PRECISION SINANG
      DOUBLE PRECISION SUM2
      DOUBLE PRECISION WBIN
      DOUBLE PRECISION WBIN2
      INTEGER I
      INTEGER IBIN
      INTEGER IBIN2
      INTEGER IFREQ
      INTEGER J
      INTEGER SHIFT
      LOGICAL MORE             
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the cos and sin of the supplied angle.
      COSANG = COS( ANG )
      SINANG = SIN( ANG )

*  Initialise the histogram.
      DO I = 1, HISTSZ
         HIST( I ) = 0.0
      END DO

*  Loop round every good position.
      DO I = 1, NPOS
         IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN

*  Find the distance along the line from the projection of the centre
*  point (xc,yc) to the projection of the current point (x,y), in units
*  of pixels in the (x,y) system.
            D = ( X( I ) - XC )*SINANG + ( Y( I ) - YC )*COSANG

*  Find the index of the histogram bin containing this point.
            FBIN = SPC0 + D/SPC
            IBIN = NINT( FBIN )

*  We increment the histogram using either nearest neighbour or linear
*  interpoolation.
            IF( LIN ) THEN

*  Split the contribution from this point between the IBIN bin and the 
*  neighbouring bin, using linear interpolation.
               D = FBIN - IBIN
               IF( D .GT. 0.0 ) THEN
                  WBIN= 1.0 - D
                  IBIN2 = IBIN + 1
                  WBIN2 = D
               ELSE
                  WBIN= 1.0 + D
                  IBIN2 = IBIN - 1
                  WBIN2 = -D
               END IF

*  Increment the neighbour bin.
               IF( IBIN2 .GE. 1 .AND. IBIN2 .LE. HISTSZ ) THEN
                  HIST( IBIN2 ) = HIST( IBIN2 ) + WBIN2
               END IF

*  For nearest neighbour, put all the weight in one bin.
            ELSE
               WBIN = 1.0
            END IF

*  Increment the central bin.
            IF( IBIN .GE. 1 .AND. IBIN .LE. HISTSZ ) THEN
               HIST( IBIN ) = HIST( IBIN ) + WBIN 
            END IF

         END IF
      END DO

*  Find the sum of the squared values in the histogram.
      SUM2 = 0.0
      DO I = 1, HISTSZ
         SUM2 = SUM2 + HIST( I )**2
      END DO

*  If it looks like the new angle may be better than the supplied MXANG
*  angle, then we continue to evaluate the wavelength of the periodicity.
      IF( SUM2 .GT. MXAMP ) THEN

*  We now find the wavelength in grid pixels of any periodicity in the
*  histogram. This is determined by forming the auto-correlation of the
*  histogram, and looking for the first peak that is at least half the
*  size of the sum of the squared values found above. First evaluate the 
*  auto-correlation at increasing shifts, until a minimum is found.
         NEWAMP = SUM2
         LSUM2 = SUM2
         SHIFT = 1
   
         MORE = .TRUE.
         DO WHILE( MORE )
   
            J = 1
            SUM2 = 0.0
            DO I = SHIFT + 1, HISTSZ
               SUM2 = SUM2 + HIST( I )*HIST( J )
               J = J + 1
            END DO
   
            SHIFT = SHIFT + 1
   
            IF( SUM2 .GT. LSUM2 .OR. SHIFT .EQ. HISTSZ ) MORE = .FALSE.
            LSUM2 = SUM2
   
         END DO

*  Now continue to evaluate the auto-correlation at increasing shifts
*  until a maximum is found that is more than half the value at zero shift.
         MORE = .TRUE.
         DO WHILE( MORE .AND. SHIFT .LT. HISTSZ )
   
            J = 1
            SUM2 = 0.0
            DO I = SHIFT + 1, HISTSZ
               SUM2 = SUM2 + HIST( I )*HIST( J )
               J = J + 1
            END DO
   
            IF( SUM2 .LT. LSUM2 .AND. LSUM2 .GT. 0.5*NEWAMP ) THEN 
               MORE = .FALSE.
               SHIFT = SHIFT - 1
            ELSE
               SHIFT = SHIFT + 1
               LSUM2 = SUM2
            END IF
   
         END DO

*  SHIFT is left holding the shift at the first significant peak in the
*  auto-correlation function. Check a peak was found, and convert the
*  shift value to a wavelength in grid pixels.
         IF( SHIFT .LT. HISTSZ ) THEN
            NEWWAV = SHIFT*SPC

*  We use the new angle if the auto-correlation peak produces a greater
*  total squared value (over the entire area of the pixel) than the old 
*  angle.
            IF( NEWAMP*NEWWAV*NEWWAV .GT. MXAMP*MXWAVE*MXWAVE ) THEN
               MXANG = ANG
               MXAMP = NEWAMP
               MXWAVE = NEWWAV
            END IF
         END IF
      END IF

      END



      REAL FUNCTION KPG1_OPGR3( N, X, Y, A, B, STATUS )
*+
*  Name:
*     KPG1_OPGR3

*  Purpose:
*     Calculates the RMS deviation between the supplied positions and the
*     corresponding pixel centres.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_OPGR3( N, X, Y, A, B, STATUS )

*  Description:
*     This function returns the sum of the squared deviations between each
*     supplied fractional GRID positions, after being shifted by a small
*     amount (A,B), and the GRID position at the centre of the pixel 
*     containing the shifted position.

*  Arguments:
*     N = INTEGER (Given)
*        The length of the array.
*     X( N ) = DOUBLE PRECISION (Given)
*        The GRID X value at each position.
*     Y( N ) = DOUBLE PRECISION (Given)
*        The GRID Y value at each position.
*     A = REAL (Given)
*        The fractional pixel shift in X.
*     B = REAL (Given)
*        The fractional pixel shift in Y.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     The sum of the squared deviations.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-NOV-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      REAL A
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION XX
      DOUBLE PRECISION YY
*.

*  Initialise 
      KPG1_OPGR3 = 0.0D0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round all good supplied positions.
      DO I = 1, N
         IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN

*  Shift the supplied position by the requested amount.
            XX = X( I ) + A
            YY = Y( I ) + B

*  Calculate the squared deviation between this shifted position, and the 
*  centre of the pixel containing the shifted position, and increment the
*  returned value by this amount.
            KPG1_OPGR3 = KPG1_OPGR3 + 
     :                   ( XX - NINT( XX ) )**2 + ( YY - NINT( YY ) )**2

         END IF
      END DO

      END


      SUBROUTINE KPG1_OPGR4( N, X, Y, A, B, STATUS )
*+
*  Name:
*     KPG1_OPGR4

*  Purpose:
*     Find the optimal fractional pixel shift.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGR4( N, X, Y, A, B, STATUS )

*  Description:
*     This routine finds a small fractional shift in grid coordinates (less 
*     than 1 pixel on each axis) that results in the supplied set of grid
*     positions being close the centre of the corresponding pixels.

*  Arguments:
*     N = INTEGER (Given)
*        The length of the array.
*     X( N ) = DOUBLE PRECISION (Given)
*        The GRID X value at each position.
*     Y( N ) = DOUBLE PRECISION (Given)
*        The GRID Y value at each position.
*     A = REAL (Returned)
*        The fractional pixel shift in X to use.
*     B = REAL (Returned)
*        The fractional pixel shift in Y to use.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-NOV-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )

*  Arguments Returned:
      REAL A
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL KPG1_OPGR3

*  Local Constants:
      INTEGER HSIZE              ! Half-size of test grid
      PARAMETER ( HSIZE = 3 )

*  Local Variables:
      INTEGER I
      INTEGER ITER
      INTEGER J
      REAL AT
      REAL BT
      REAL PCENA
      REAL PCENB
      REAL PSTEP
      REAL S
      REAL SMIN        
*.

*  Initialise 
      A = 0.0
      B = 0.0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the size (as a fraction of a grid pixel) of each cell in the
*  test grid. The intial test grid covers a little more than one grid pixel.
      PSTEP = 1.1/( 2*HSIZE + 1 )

*  Set up the offset values at the centre of the test grid.
      PCENA = 0.0
      PCENB = 0.0

*  On each iteration the error function is found in each cell of the
*  current test grid. The test grid for the next iteration is then set 
*  up so that it covers the cell in the current grid with the smallest 
*  error function.
      DO ITER = 1, 2

*  Initialise the minimum error function found for any cell in the
*  current test grid.
         SMIN = VAL__MAXR

*  Loop round every cellin the current test grid, finding the pixel offsets 
*  corresponding to the centre of the cell.
         DO J = -HSIZE, HSIZE
            BT = PCENB + PSTEP*J
   
            DO I = -HSIZE, HSIZE
               AT = PCENA + PSTEP*I
   
*  Find the error function (always positive) at these pixel offsets.
               S = KPG1_OPGR3( N, X, Y, AT, BT, STATUS )
   
*  Note if this error function value is smaller than any other found so
*  far.
               IF( S .LT. SMIN ) THEN
                  A = AT
                  B = BT
                  SMIN = S
               END IF
   
            END DO
   
         END DO           
   
*  Set up the parameters defining the next test grid to be used. It
*  covers the cell that had the smallest error function.
         PCENA = A
         PCENB = B
         PSTEP = PSTEP/( 2*HSIZE + 1 )

      END DO

      END












*  Diagnostic routines...


      subroutine opgrd_dump( n, dat, fft, ang, status )
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'CNF_PAR'

      logical dumpit
      common /fred/ dumpit

      integer n, ang, status, place, indf, el, iat, pntr
      real dat( n )
      logical fft
      character name*30

      if( status .ne. sai__ok .or. .not. dumpit ) return

      if( fft ) then
         name = 'fft'
         iat = 3
      else
         name = 'hist'
         iat = 4
      end if

      call chr_puti( ang, name, iat )
      call ndf_place( DAT__ROOT, name( : iat ), place, status )
      call ndf_newp( '_REAL', 1, n, place, indf, status )
      call ndf_map( indf, 'data', '_REAL', 'WRITE', pntr, el, status )
      call opgrd_copy( n, dat, %val( cnf_pval( pntr ) ), status )
      call ndf_annul( indf, status )

      end

      subroutine opgrd_copy( n, datin, datout, status )
      implicit none
      include 'SAE_PAR'

      integer n, status, i
      real datin( n ), datout( n )

      if( status .ne. sai__ok ) return

      do i = 1, n
         datout( i ) = datin( i )
      end do

      end



      subroutine opgrd_autodump( ang, n, hist, status )
      implicit none       
      include 'SAE_PAR'   

      integer ang
      integer n, shift, i1, i2, iw
      real hist( n ), sum
      integer status 

      if ( status .ne. sai__ok ) return

      iw = n
      shift = 0
      do while( shift .lt. iw )

         i1 = 1
         sum = 0
         do i2 = i1 + shift, iw
            sum = sum + hist(i1)*hist(i2)
            i1 = i1 + 1
         end do
         hist( iw ) = sum

         iw = iw - 1
         shift = shift + 1

      end do

      do i1 = n, iw + 1, -1
         hist( n - i1 + 1 ) = hist( i1 )
      end do

      do i1 = n - iw + 1, n
         hist( i1 ) = 0
      end do

      call opgrd_dump( n, hist, .true., ang, status )

      end




