      SUBROUTINE KPG1_OPGRD( NPOS, POS, WEST, PAR, RDIAM, STATUS )
*+
*  Name:
*     KPG1_OPGRD

*  Purpose:
*     Gets the parameters of an optimal projection for a given set of
*     sky positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGRD( NPOS, POS, WEST, PAR, RDIAM, STATUS )

*  Description:
*     This routine calculates the parameters of a tangent-plane
*     projection that gives an optimal representation of a set of
*     supplied sky positions. The projection parameters are the normal
*     FITS CRPIX1/2, CRVAL1/2, CDELT1/2 and CROTA2 keywords. They are
*     chosen in order to maximise the number of sky positions that fall
*     close to the centre of a pixel.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     POS( 2, NPOS ) = DOUBLE PRECISION (Given)
*        The sky positions. These should be (longitude,latitude) values,
*        in radians, in any celestial co-ordinate system.
*     WEST = LOGICAL (Given)
*        If .TRUE., then it is assumed that the X grid axis increases
*        westwards (assuming north is parallel to +ve Y). This is the
*        case for most celestial co-ordinate systems such as (RA,Dec)
*        etc.  If .FALSE., then it is assumed that the X grid axis
*        increases eastwards (assuming north is parallel to +ve Y). This
*        is the case for a few systems such as (az,el) and geographic
*        (longitude,latitude).
*     PAR( 7 ) = DOUBLE PRECISION (Given and Returned)
*        The projection parameters. Each parameter that is supplied with
*        a value of AST__BAD on entry will be replaced on exit with the
*        optimal value. Non-bad supplied values will be left unchanged
*        on exit. The supplied values will also be left unchanged if
*        optimal values cannot be determined. They are stored in the
*        order CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2.
*        CRPIX1 and CRPIX2 are in units of pixels. All the other
*        projection parameters will be in units of radians, and refer to
*        the celestial co-ordinate system in which the POS values are
*        supplied. CROTA2 is the angle from the Y axis to celestial
*        north, measured north through east (no matter the value of
*        WEST). Returned pixel sizes are rounded to the nearest tenth of
*        an arcsecond.
*     RDIAM = DOUBLE PRECISION (Returned)
*        The diameter of the circle that just encloses all the supplied
*        sky positions, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2006 (DSB):
*        Original version.
*     30-NOV-2006 (DSB):
*        Return supplied parameter values unchanged if no periodicity
*        can be found in the supplied positions.
*     6-DEC-2006 (DSB):
*        Fix bug that gave erroneous CRPIX1/2 values.
*     7-DEC-2006 (DSB):
*        Fix bug that could swap order of CDELT1 and CDELT2.
*     20-DEC-2006 (DSB):
*        Retain the supplied parameter values if the optimal pixel size
*        is zero.
*     21-DEC-2006 (DSB):
*        Allow negative pixel sizes.
*     22-DEC-2006 (DSB):
*        Add argument RDIAM.
*     11-JAN-2007 (DSB):
*        Correct description of CROTA2 in prologue.
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

*  Arguments Given and Returned:
      DOUBLE PRECISION PAR( 7 )

*  Arguments Returned:
      DOUBLE PRECISION RDIAM

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
     :                 %VAL( CNF_PVAL( IPYOUT ) ), RDIAM, STATUS )

*  Free work arrays.
      CALL PSX_FREE( IPAIN, STATUS )
      CALL PSX_FREE( IPBIN, STATUS )
      CALL PSX_FREE( IPXOUT, STATUS )
      CALL PSX_FREE( IPYOUT, STATUS )

      END




      SUBROUTINE KPG1_OPGR1( NPOS, POS, WEST, PAR, AIN, BIN, XOUT, YOUT,
     :                       RDIAM, STATUS )
*+
*  Name:
*     KPG1_OPGR1

*  Purpose:
*     Does the work for KPG1_OPGRD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_OPGR1( NPOS, POS, WEST, PAR, AIN, BIN, XOUT, YOUT,
*                      RDIAM, STATUS )

*  Description:
*     This routine calculates the parameters of a tangent-plane
*     projection that gives an optimal representation of a set of
*     supplied sky positions. The projection parameters are the normal
*     FITS CRPIX1/2, CRVAL1/2, CDELT1/2 and CROTA2 keywords. They are
*     chosen in order to maximise the number of sky positions that fall
*     close to the centre of a pixel.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     POS( 2, NPOS ) = DOUBLE PRECISION (Given)
*        The sky positions. These should be (longitude,latitude) values,
*        in radians, in any celestial co-ordinate system.
*     WEST = LOGICAL (Given)
*        If .TRUE., then it is assumed that the X grid axis increases
*        westwards (assuming north is parallel to +ve Y). This is the
*        case for most celestial co-ordinate systems such as (RA,Dec)
*        etc. If .FALSE., then it is assumed that the X grid axis
*        increases eastwards (assuming north is parallel to +ve Y). This
*        is the case for a few systems such as (az,el) and geographic
*        (longitude,latitude).
*     PAR( 7 ) = DOUBLE PRECISION (Given and Returned)
*        The projection parameters. Each parameter that is supplied with
*        a value of AST__BAD on entry will be replaced on exit with the
*        optimal value. Non-bad supplied values will be left unchanged
*        on exit. The supplied values will also be left unchanged if no
*        periodicity is visible in the supplied positions. They are
*        stored in the order CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1,
*        CDELT2, CROTA2.  CRPIX1 and CRPIX2 are in units of pixels. All
*        the other projection parameters will be in units of radians,
*        and refer to the celestial co-ordinate system in which the POS
*        values are supplied. CROTA2 is the angle from the Y axis to
*        celestial north, measured north through east (no matter what
*        the value of WEST). Returned pixel sizes are rounded to the
*        nearest tenth of an arcsecond.
*     AIN( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     BIN( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     XOUT( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     YOUT( NPOS ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     RDIAM = DOUBLE PRECISION (Returned)
*        The diameter of the circle that just encloses all the supplied
*        sky positions, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2006 (DSB):
*        Original version.
*     27-NOV-2006 (DSB):
*        Round the returned CROTA value to 0.1 degree.
*     8-JAN-2006 (DSB):
*        Allow the reference poition to be optimised in cases where the
*        optimal pixel sizes and orientation cannot be determined, but
*        dont need to be because values were supplied by the caller.
*     9-JAN-2006 (DSB):
*        Check for insufficient points being supplied, and for all
*        supplied points being co-incident.
*     21-APR-2009 (DSB):
*        The mean sample spacing over the bounding box can give a gross
*        under-estimate of the pixel spacing if the samples are clustered
*        into groups where each group represents one pixel. For this
*        reason, loop round trying larger sample spacing until the pixel
*        size converges.
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

*  Arguments Given and Returned:
      DOUBLE PRECISION PAR( 7 )
      DOUBLE PRECISION AIN( NPOS )
      DOUBLE PRECISION BIN( NPOS )
      DOUBLE PRECISION XOUT( NPOS )
      DOUBLE PRECISION YOUT( NPOS )

*  Arguments Returned:
      DOUBLE PRECISION RDIAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PIXSCL    ! Initial pixel scale (rads per pixel)
      PARAMETER ( PIXSCL = 0.0003D0 ) ! (about 1 arc-min per pixel)

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
      DOUBLE PRECISION PAR0( 7 )
      DOUBLE PRECISION PWAVE
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
      INTEGER ALLOCD
      INTEGER HISTSZ
      INTEGER I
      INTEGER IANG
      INTEGER IGOOD
      INTEGER IPHIST
      INTEGER MAP
      LOGICAL OK
      LOGICAL OPTY
      REAL DX
      REAL DY


      logical dumpit
      integer pass
      common /fred/ dumpit,pass





*.

*  Initialise
      PWAVE = 0.0D0
      PANG = 0.0D0
      ALLOCD = 0
      IPHIST = 0
      OK = .TRUE.
      RDIAM = 0.0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Copy the longitude and latitude values into separate arrays, and
*  find the index of the first good supplied sky position.
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
*  north upwards, and X either east or west. Any supplied reference
*  point is used, but if no reference point is supplied the first good
*  supplied position is used as the reference point, and this point is
*  placed at grid co-ordinates (1,1).
      PAR0( 1 ) = 1.0D0
      PAR0( 2 ) = 1.0D0

      IF( PAR( 3 ) .NE. AST__BAD ) THEN
         PAR0( 3 ) = PAR( 3 )
      ELSE
         PAR0( 3 ) = POS( 1, IGOOD )
      END IF

      IF( PAR( 4 ) .NE. AST__BAD ) THEN
         PAR0( 4 ) = PAR( 4 )
      ELSE
         PAR0( 4 ) = POS( 2, IGOOD )
      END IF

      IF( WEST ) THEN
         PAR0( 5 ) = -PIXSCL
      ELSE
         PAR0( 5 ) = PIXSCL
      END IF

      PAR0( 6 ) = PIXSCL
      PAR0( 7 ) = 0.0D0

*  Create an AST Mapping describing this initial projection. Use the
*  above values to define a FITS-WCS header in a FitsChan (converting
*  radian values to degrees as needed by FITS), and then read a FrameSet
*  from it, and recover the base->current Mapping from the FrameSet.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE1', 'RA---TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSS( FC, 'CTYPE2', 'DEC--TAN', ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX1', PAR0( 1 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRPIX2', PAR0( 2 ), ' ', .TRUE., STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL1', PAR0( 3 )*AST__DR2D, ' ', .TRUE.,
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CRVAL2', PAR0( 4 )*AST__DR2D, ' ', .TRUE.,
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT1', PAR0( 5 )*AST__DR2D, ' ', .TRUE.,
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CDELT2', PAR0( 6 )*AST__DR2D, ' ', .TRUE.,
     :                   STATUS )
      CALL AST_SETFITSF( FC, 'CROTA2', PAR0( 7 )*AST__DR2D, ' ', .TRUE.,
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
*  the grid co-ordinate system described by the initial projection.
      CALL AST_TRAN2( MAP, NPOS, AIN, BIN, .FALSE., XOUT, YOUT, STATUS )

*  Find the bounding box of the supplied positions within the initial
*  grid co-ordinate system.
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

*  Find the diameter of the circle enclosing the bounding box, in units
*  of initial grid pixels.
      DIAM = SQRT( ( XHI - XLO )**2 + ( YHI - YLO )**2 )

*  Convert this to radians.
      RDIAM = DIAM*PIXSCL

*  Use zero spacing if the points are co-incident. Otherwise, calculate
*  what the grid spacing would be (in units of initial grid pixels) if
*  the supplied positions were distributed on a regularly spaced grid
*  over the bounding box.
      IF( DIAM .EQ. 0.0 ) THEN
         SPC = 0.0
      ELSE
         SPC = SQRT( ( XHI - XLO )*( YHI - YLO ) ) /
     :               ( SQRT( REAL( NPOS ) ) - 1.0 )
      END IF

*  Store the grid co-ordinates at the centre of the bounding box.
      XC = 0.5*( XHI + XLO )
      YC = 0.5*( YHI + YLO )

*  The next bit is to do with determining the optimal pixel sizes and
*  orientation, so we can skip it if these were supplied by the caller.
      IF( SPC .GT. 0.0 .AND. ( PAR( 5 ) .EQ. AST__BAD .OR.
     :                         PAR( 6 ) .EQ. AST__BAD .OR.
     :                         PAR( 7 ) .EQ. AST__BAD ) ) THEN
         OK = .FALSE.

         pass = ichar( 'A' ) - 1

*  If the positions are grouped into clusters of points, where each
*  cluster represents a single sky position with errors, then the mean
*  spacing between samples, SPC, will be much smaller than the required
*  spacing (i.e. the spacing between clusters). This can upset the
*  calculation of the best orientation. So we loop. On the first pass
*  we determine a likely cluster spacing in MXWAVE using the initial mean
*  sample spacing to determine the histogram spacing. If this cluster
*  spacing is much bigger than the mean sample spacing, we recalculate
*  the cluster spacing using a larger histogram spacing.
         MXWAVE = 0.5*SPC
         SPC = 0.0
         DO WHILE( MXWAVE .GT. 10.0*SPC .AND. STATUS .EQ. SAI__OK )
            pass = pass + 1

c      write(*,*) ' '
c      write(*,*) ' Cluster spacing: ',MXWAVE*pixscl*AST__DR2D*3600,
c     :           ' sample spacing: ', spc*pixscl*AST__DR2D*3600,
c     :           ' arcsec'

*  Allocate a one-dimensional work array which spans the circle
*  enclosing the bounding box, but using smaller pixels (one fifth of
*  the current estimate of the periodicity).
            SPC = 0.2*MXWAVE
            HISTSZ = DIAM/SPC
            IF( ALLOCD .LT. HISTSZ ) THEN
               IF( ALLOCD .GT. 0 ) CALL PSX_FREE( IPHIST, STATUS )
               CALL PSX_CALLOC( HISTSZ, '_REAL', IPHIST, STATUS )
               ALLOCD = HISTSZ
            END IF

*  Store the grid co-ordinate within this work array that will
*  correspond to the central position (XC,YC).
            SPC0 = DBLE( ( HISTSZ + 1 )/2 )

c      write(*,*) ' Course scanning angles with sample spacing of ',
c     :           spc*pixscl*AST__DR2D*3600,' arcsec'
c      write(*,*) ' '

*  Imagine a line passing through the centre position (XC,YC). We step
*  through all orientations of this line in units of 3 degrees. Zero
*  angle corresponds to the second axis in the initial projection (i.e.
*  celestial north), and the first axis of the initial projection
*  (either east or west) is at angle of +90 degrees.
            MXAMP = -1.0
            MXWAVE = 0.0
            DO IANG = 0, 177, 3
               ANG = IANG*AST__DD2R

*  For the current line orientation, project every initial grid position
*  onto the line, and record where about the projected point falls on
*  the line. The one-dimensional work array allocated earlier is used as
*  a histogram to bin the number of projected points falling on each
*  point of the line. Once all points have been projected onto the line,
*  at the current orientation, the amplitude of any periodicity shown in
*  the array is determined, together with the wavelength of the
*  periodicity (in units of initial grid pixels). The details of the
*  orientation with the strongest periodicity are retained in MXAMP,
*  MXANG and MXWAVE.
               CALL KPG1_OPGR2( NPOS, XOUT, YOUT, ANG, SPC, XC, YC,
     :                          SPC0, .TRUE., HISTSZ,
     :                          %VAL( CNF_PVAL( IPHIST ) ), MXAMP,
     :                          MXWAVE, MXANG, STATUS )

*  Next orientation.
            END DO

         END DO

c      write(*,*)
c      write(*,*) '----- Accurate angle scanning ------'
c      write(*,*)

*  Check a direction was found that shows some periodicity.
         IF( MXWAVE .GT. 0.0 ) THEN
            pass = pass + 1

*  Now do a finer search through a cone of angles centred on the rough
*  angle found above. The cone is 3 degrees wide and we use 0.1-degree
*  intervals. In this loop the IANG variable is angular offset from
*  ANG0 in units of 0.1 degree. This time, we use linear interpolation
*  to create the histogram, to get a more-accurate result.
            ANG0 = MXANG
            MXAMP = -1.0
            MXWAVE = 0.0

            DO IANG = -15, 15
               ANG = ANG0 + 0.1*DBLE( IANG )*AST__DD2R
               CALL KPG1_OPGR2( NPOS, XOUT, YOUT, ANG, SPC, XC, YC,
     :                          SPC0, .TRUE., HISTSZ,
     :                          %VAL( CNF_PVAL( IPHIST ) ), MXAMP,
     :                          MXWAVE, MXANG, STATUS )
            END DO

*  Calculate the wavelength of the periodicity at right angles to the
*  above chosen direction.
            PAMP = -1.0
            PWAVE = 0.0
            CALL KPG1_OPGR2( NPOS, XOUT, YOUT, MXANG + AST__DPIBY2,
     :                       SPC, XC, YC, SPC0, .TRUE., HISTSZ,
     :                       %VAL( CNF_PVAL( IPHIST ) ),
     :                       PAMP, PWAVE, PANG, STATUS )

*  Check that the chosen orientations have some periodicity.
            IF( MXWAVE .GT. 0.0 .AND. PWAVE .GT. 0.0 ) THEN
               OK = .TRUE.

*  The orientation with the most prominent periodicity can be used as
*  either the X or Y axis in the returned optimal grid, and can be
*  either the +ve or -ve direction along the axis. We choose the axis
*  and direction in order to put the +ve Y axis in the returned grid as
*  close as possible to celestial north. So get the position angle
*  (+ve Y through +ve X in the initial grid) of the optimal Y axis, and
*  set a flag indicating if the periodicity is associated with the
*  optimal X or Y grid axis.
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

*  Store the CDELT1/2 parameters of the optimal grid.
               IF( OPTY ) THEN
                  PAR0( 5 ) = -PWAVE*PIXSCL
                  PAR0( 6 ) = MXWAVE*PIXSCL
               ELSE
                  PAR0( 5 ) = -MXWAVE*PIXSCL
                  PAR0( 6 ) = PWAVE*PIXSCL
               END IF

               IF( .NOT. WEST ) PAR0( 5 ) = -PAR0( 5 )

*  Store the CROTA2 parameter.
               IF( WEST ) THEN
                  PAR0( 7 ) = YANG
               ELSE
                  PAR0( 7 ) = -YANG
               END IF

*  Round to the nearest 10th of a degree.
               PAR0( 7 ) = NINT( PAR0( 7 )*AST__DR2D*10.0 )/
     :                            (10.0*AST__DR2D)

*  Ensure CROTA is in the range 0 -> 2*PI
               DO WHILE( PAR0( 7 ) .LT. 0.0 )
                  PAR0( 7 ) = PAR0( 7 ) + 2*AST__DPI
               END DO

               DO WHILE( PAR0( 7 ) .GE. 360.0 )
                  PAR0( 7 ) = PAR0( 7 ) - 2*AST__DPI
               END DO

*  Round the pixel sizes to the closest 10th of an arc-second.
               PAR0( 5 ) = NINT( PAR0( 5 )*AST__DR2D*36000.0 )/
     :                            (36000.0*AST__DR2D)
               PAR0( 6 ) = NINT( PAR0( 6 )*AST__DR2D*36000.0 )/
     :                            (36000.0*AST__DR2D)

            END IF
         END IF
      END IF

*  Check that PAR0 now contains usable pixel size and orientation
*  values.
      IF( OK ) THEN

*  Store the INITIAL CRPIX1/2 parameters of the optimal grid.
         PAR0( 1 ) = 1.0
         PAR0( 2 ) = 1.0

*  If no reference point sky coords were supplied, use the centre of the
*  bounding box (if reference point sky coords were supplied they will
*  already be stored in PAR0(3) and PAR0(4) ).
         IF( PAR( 3 ) .EQ. AST__BAD .OR. PAR( 4 ) .EQ. AST__BAD ) THEN

*  Find the celestial co-ordinates at the centre of the bounding box.
            CALL AST_TRAN2( MAP, 1, XC, YC, .TRUE., XOUT, YOUT, STATUS )

*  Report an error if the celestial coords are bad.
            IF( XOUT( 1 ) .EQ. VAL__MIND ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'KPG1_OPGRD: Cannot transform '//
     :                          'central position (programming error).',
     :                          STATUS )
               END IF
               GO TO 999
            END IF

            PAR0( 3 ) = XOUT( 1 )
            PAR0( 4 ) = YOUT( 1 )
         END IF

*  Copy any supplied values into the projection parameters array.
         DO I = 1, 7
            IF( PAR( I ) .NE. AST__BAD ) PAR0( I ) = PAR( I )
         END DO

*  We now find a small (less than one pixel) change to PAR0(1) and
*  PAR0(2) that causes more samples to be projected to the centre of the
*  corresponding pixel. First create a Mapping from the above projection
*  parameters and use it to map the supplied sky positions into grid
*  co-ordinates.
         FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
         CALL AST_SETFITSS( FC, 'CTYPE1', 'RA---TAN', ' ', .TRUE.,
     :                      STATUS )
         CALL AST_SETFITSS( FC, 'CTYPE2', 'DEC--TAN', ' ', .TRUE.,
     :                      STATUS )
         CALL AST_SETFITSF( FC, 'CRPIX1', PAR0( 1 ), ' ', .TRUE.,
     :                      STATUS )
         CALL AST_SETFITSF( FC, 'CRPIX2', PAR0( 2 ), ' ', .TRUE.,
     :                      STATUS )
         CALL AST_SETFITSF( FC, 'CRVAL1', PAR0( 3 )*AST__DR2D, ' ',
     :                      .TRUE., STATUS )
         CALL AST_SETFITSF( FC, 'CRVAL2', PAR0( 4 )*AST__DR2D, ' ',
     :                      .TRUE., STATUS )
         CALL AST_SETFITSF( FC, 'CDELT1', PAR0( 5 )*AST__DR2D, ' ',
     :                      .TRUE., STATUS )
         CALL AST_SETFITSF( FC, 'CDELT2', PAR0( 6 )*AST__DR2D, ' ',
     :                      .TRUE., STATUS )
         CALL AST_SETFITSF( FC, 'CROTA2', PAR0( 7 )*AST__DR2D, ' ',
     :                      .TRUE., STATUS )
         CALL AST_CLEAR( FC, 'Card', STATUS )

         FS = AST_READ( FC, STATUS )

         IF( FS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'KPG1_OPGRD: Failed to read '//
     :                    'FrameSet from FitsChan (programming '//
     :                    'error).', STATUS )
            GO TO 999
         END IF

         MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )
         CALL AST_TRAN2( MAP, NPOS, AIN, BIN, .FALSE., XOUT, YOUT,
     :                   STATUS )

*  Find the fractional pixel offsets that result in the minimum squared
*  deviation between sample positions and the centres of the
*  corresponding pixels.
         CALL KPG1_OPGR4( NPOS, XOUT, YOUT, DX, DY, STATUS )

*  If the reference point sky coords were supplied, modify the CRPIX1/2
*  values to put the refence point at the right place, leaving CRVAL1/2
*  unchanged.
         IF( PAR( 3 ) .NE. AST__BAD .OR.
     :       PAR( 4 ) .NE. AST__BAD ) THEN
            PAR0( 1 ) = PAR0( 1 ) + DX
            PAR0( 2 ) = PAR0( 2 ) + DY

*  If the reference point sky coords were not supplied, modify the
*  CRVAL1/2 values so that CRPIX1/2 can retain the existing nice
*  integer values.
         ELSE

*  Modify the CRPIX values ( PAR0(1) and PAR0(2) ) accordingly.
            NPAR1 = PAR0( 1 ) - DX
            NPAR2 = PAR0( 2 ) - DY

*  Find the sky coords corresponding to this position, and use them as
*  the new CRVAL1/2 values ( PAR0(3) and PAR0(4) ).
            CALL AST_TRAN2( MAP, 1, NPAR1, NPAR2, .TRUE., PAR0( 3 ),
     :                      PAR0( 4 ), STATUS )
         END IF

*  Normalise CRVAL1/2 into range (0->2PI, -PI/2->PI/2)
         IF( PAR0( 4 ) .GT. AST__DPIBY2 ) THEN
            PAR0( 4 ) = AST__DPI - PAR0( 4 )
            PAR0( 3 ) = AST__DPI + PAR0( 3 )

         ELSE IF( PAR0( 4 ) .LT. -AST__DPIBY2 ) THEN
            PAR0( 4 ) = -AST__DPI - PAR0( 4 )
            PAR0( 3 ) = AST__DPI + PAR0( 3 )

         END IF

         IF( PAR0( 3 ) .LT. 0.0 ) THEN
            PAR0( 3 ) = PAR0( 3 ) + 2*AST__DPI

         ELSE IF( PAR0( 3 ) .GT. 2*AST__DPI ) THEN
            PAR0( 3 ) = PAR0( 3 ) - 2*AST__DPI

         END IF

*  Find the pixel bounds of the new bounding box.
         XHI = VAL__MIND
         YHI = VAL__MIND
         XLO = VAL__MAXD
         YLO = VAL__MAXD

         DO I = 1, NPOS
            IF( XOUT( I ) .NE. AST__BAD .AND.
     :          YOUT( I ) .NE. AST__BAD ) THEN
               XHI = MAX( XHI, XOUT( I ) )
               YHI = MAX( YHI, YOUT( I ) )
               XLO = MIN( XLO, XOUT( I ) )
               YLO = MIN( YLO, YOUT( I ) )
            END IF
         END DO

*  Choose the integer part of CRPIX1/2 so that it is close to the centre
*  of the bounding box.
         PAR0( 1 ) = PAR0( 1 ) + NINT( ( XHI - XLO )/2 ) + 1
     :               - NINT( PAR0( 1 ) )
         PAR0( 2 ) = PAR0( 2 ) + NINT( ( YHI - YLO )/2 ) + 1
     :               - NINT( PAR0( 2 ) )

*  Check pixel sizes are not zero.
         IF( PAR0( 5 ) .NE. 0.0 .AND. PAR0( 6 ) .NE. 0.0 ) THEN

*  Copy the projection parameters to the supplIed array.
            DO I = 1, 7
               IF( PAR( I ) .EQ. AST__BAD ) PAR( I ) = PAR0( I )
            END DO

         END IF
      END IF

*  Free resources
 999  CONTINUE
      IF( IPHIST .NE. 0 ) CALL PSX_FREE( IPHIST, STATUS )

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
*     This routine projects the supplied grid position onto a line at
*     the orientation given by ANG, passing through the point given by
*     (XC,YC). The projected positions along this line are recorded in
*     a histogram in which each bin corresponds to a section of the
*     line, and the count in each bin is the number of points projected
*     onto the section of the line corresponding to the bin.
*
*     Once all positions have been projected, the sum of the squared
*     histogram values is found (this is the same as the
*     auto-correlation at zero shift), and used as a measure of the
*     strength of the periodicity.
*
*     If the periodicity at this angle is greater than the supplied
*     value in MXAMP, then the wavelength of the periodicity is found
*     and returned in MXWAVE. The wavelength is found by evaluating the
*     auto-correlation of the histogram at increasing shifts until the
*     first significant peak is found. The corresponding shift is taken
*     as the wavelength of the periodicity.
*
*     A square regular grid of positions will have strong periodicity
*     when projected onto a line parallel to its X and Y directions. But
*     it will also have equally strong periodicity when projected onto
*     a line at 45 degrees to either its X or Y directions. However, in
*     this second case the pixel sizes (i.e. the wavelength of the
*     periodicity) will be smaller than in the first case. We use this
*     fact to distinguish between the two cases. Once the periodicity of
*     the wavelength has been found, a further check is made before
*     accepting the orientation as better than the supplied MXANG
*     orientation. This check consists of multiplying the strength of
*     the periodicity by the area of the pixel, and only accepting the
*     new orientation if this value is greater than the supplied
*     MXANG*MXWAVE*MXWAVE value. Since the pixel area will be smaller if
*     the line is oriented at 45 degrees to an axis, than if it is
*     parallel to the axis, the pixel area factor will result in the
*     parallel case being selected rather than the 45-degrees case.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of sky positions.
*     X( NPOS ) = DOUBLE PRECISION (Given)
*        The X grid co-ordinates.
*     Y( NPOS ) = DOUBLE PRECISION (Given)
*        The Y grid co-ordinates.
*     ANG = DOUBLE PRECISION (Given)
*        The angle of the line, in radians. Measured from the grid Y
*        axis through the grid X axis.
*     SPC = DOUBLE PRECISION (Given)
*        The length of each bin in the histogram, in units of grid
*        pixels.
*     XC = DOUBLE PRECISION (Given)
*        The X grid coord of a point on the line.
*     YC = DOUBLE PRECISION (Given)
*        The Y grid coord of a point on the line.
*     SPC0 = DOUBLE PRECISION (Given)
*        The grid co-ordinate within the HIST array, onto which the
*     LIN = LOGICAL (Given)
*        Should the histogram be formed using nearest neighbour or
*        linear interpolation?
*     HISTSZ = INTEGER (Given)
*        The length of the histogram array.
*     HIST( HISTSZ ) = REAL (Given and Returned)
*        The array to use to hold the histogram.
*     MXAMP = DOUBLE PRECISION (Given and Returned)
*        Measures the strength of the periodicity in the histogram. It
*        is updated on exit if the angle specified by "ANG"
*        demonstrates stronger periodicity than the supplied value.
*     MXWAVE = DOUBLE PRECISION (Given and Returned)
*        The wavelength (in grid pixels) of the spatial frequency at the
*        first peak in the auto-correlation function. Updated if the
*        value at the first peak of the auto-correlation function for
*        the supplied angle, is greater than the supplied MXAMP value.
*     MXANG = DOUBLE PRECISION (Given and Returned)
*        The angle (in radians) which produced the maximum MXAMP value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-2006 (DSB):
*        Original version.
*     22-NOV-2006 (DSB):
*        Impove location of peak in auto-correlation function by fitting
*        a quafratic to the 3 values closest to the peak.
*     10-JAN-2007 (DSB):
*        Change the scheme used to weight the auro-correlation peak
*        value when choosing whether to repalce the old "best angle"
*        with the new angle.
*     11-JAN-2007 (DSB):
*        Ignored insignificant peaks and troughs in the auto-correlation
*        when finding the first peak.
*     17-APR-2007 (DSB):
*        If no maximum is found in the auto-correlation function that is
*        more than half the value at zero shift, then consider using the
*        maximum auto-correlation value found to define the wavelength.
*        Only do this if the maximum auto-correlation value found is
*        more than ten times the mean auto-correlation value. All this
*        is to handle cases where the periodicity is defined by a peak
*        that is small in height compared with the zero-shift value, but
*        large compared with the noise in the auto-correlation. This may
*        happen for instance if there are very few positions are
*        supplied (e.g. a regular grid of five points).
*     12-JUL-2007 (DSB):
*        Modify the estimate of the noise in the auto-correlation value.
*        it is now the weighted mean of the change between adjacent
*        auto-correlation values (each weight is the reciprocal of the
*        auto-correlation value).
*     20-APR-2009 (DSB):
*        Relax the check that says that the sum of the (un-weighted) squared
*        values (SUM2) must be better than the previous maximum (MXAMP) for
*        the angle to be of any possible interest. This makes the algorithm
*        slightly less efficient, but more robust.
*     20-JUN-2009 (DSB):
*        - Only accept peaks in the autocorrelation function that occur
*        within the first third of the histogram width (i.e. that produce
*        at least three wavelengths within the extent of the data).
*        - When giving greater weight to longer wavelengths, only do this
*        up to a certain point (a factor 1.5) in order to avoid accepting
*        weak periodicities just because they have ridiculously large
*        wavelengths.
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
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION FBIN
      DOUBLE PRECISION FFBIN
      DOUBLE PRECISION LIMSUM
      DOUBLE PRECISION LLLSUM2
      DOUBLE PRECISION LLSUM2
      DOUBLE PRECISION LSUM2
      DOUBLE PRECISION MAXSUM
      DOUBLE PRECISION MINSUM
      DOUBLE PRECISION NEWAMP
      DOUBLE PRECISION NEWWAV
      DOUBLE PRECISION NEWWGT
      DOUBLE PRECISION OLDWGT
      DOUBLE PRECISION SINANG
      DOUBLE PRECISION SUM2
      DOUBLE PRECISION SUMINC
      DOUBLE PRECISION SUMW
      DOUBLE PRECISION USUM
      DOUBLE PRECISION W
      DOUBLE PRECISION WBIN
      DOUBLE PRECISION WBIN2
      DOUBLE PRECISION XSHIFT
      INTEGER COUNT
      INTEGER I
      INTEGER IBIN
      INTEGER IBIN2
      INTEGER II
      INTEGER J
      INTEGER MAXSH
      INTEGER MINSH
      INTEGER SHIFT
      LOGICAL MORE
      REAL OFFSET( 3 )


      DATA OFFSET /-0.5, 0.0, 0.5 /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise things to avoid compiler warnings.
      DSUM = 0.0
      USUM = 0.0
      COUNT = 0.0
      MINSH = 0.0
      MAXSUM = 0.0
      MAXSH = 0.0

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

*  Find the floating point centre of the histogram bin containing this point.
            FFBIN = SPC0 + D/SPC

*  It's possible to get aliasing between the sample spacing on the sky and
*  the histogram spacing. For instance, if a sample falls on or close
*  to the edge of two histogram bins, it will be evenly divided between
*  them, resulting in each bin having only half a sample contribution.
*  One the other hand if a sample falls in the middle of a histogram bin,
*  then the bin will receive the whole sample. Since the squared amplitude in
*  each bin is important, this will unduly favour bins that receive the whole
*  sample. To avoid this, we add the sample into the histogram several
*  times, with a slightly jiggled position each time. This causes the
*  effect of each sample to be spread out over the neighbouring bins.
            DO II = 1, 3

*  Select the fraction pixel offset to use.
               FBIN = FFBIN + OFFSET( II )

*  Find the index of the central bin using the current jigled sample
*  position.
               IBIN = NINT( FBIN )

*  We increment the histogram using either nearest neighbour or linear
*  interpolation.
               IF( LIN ) THEN

*  Split the contribution from this point between the IBIN bin and the
*  neighbouring bin, using linear interpolation.
                  D = FBIN - IBIN
                  IF( D .GT. 0.0 ) THEN
                     WBIN = 1.0 - D
                     IBIN2 = IBIN + 1
                     WBIN2 = D
                  ELSE
                     WBIN = 1.0 + D
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

            END DO
         END IF
      END DO

*  Find the sum of the squared values in the histogram.
      SUM2 = 0.0
      DO I = 1, HISTSZ
         SUM2 = SUM2 + HIST( I )**2
      END DO

c      write(*,*) 'Ang: ',ang*ast__dr2d,' sum2: ',sum2,' 0.5*mxamp: ',
c     :            0.5*mxamp

*  If it looks like the new angle may be better than the supplied MXANG
*  angle, then we continue to evaluate the wavelength of the periodicity.
*  Note, SUM only needs to be a significant fraction of MXAMP to prompt
*  this further check (a factor of 0.5 is chosen more or less arbitrarily
*  and may need tweaking). This is because, even if SUM2 is less than
*  MXAMP, the effect of inclusing the wavelength in the weighting may
*  result in this angle being preferable to the previous best angle. We
*  could in principle do away with this IF check altogether and rely on
*  the comparison of the fully wavelength-weighted values performed below
*  to select the best angle, but that would involve evaluating the
*  autocorrelation function for a lot of angles that cannot possibly be of
*  any interest because of their very low SUM2 values.
      IF( SUM2 .GT. 0.5*MXAMP ) THEN

*  We now find the wavelength in grid pixels of any periodicity in the
*  histogram. This is determined by forming the auto-correlation of the
*  histogram, and looking for the first peak that is at least half the
*  size of the sum of the squared values found above. First evaluate the
*  auto-correlation at increasing shifts, until a minimum is found which
*  is less than the two subsequent value.
         NEWAMP = SUM2
         LLLSUM2 = SUM2
         LLSUM2 = SUM2
         LSUM2 = SUM2
         SHIFT = 1
         MINSUM = SUM2
         SUMINC = 0.0
         SUMW = 0.0

*  Loop over increasing shifts until we have found the minimum.
         MORE = .TRUE.
         DO WHILE( MORE )

*  We record the SUM2 values for the three previous shifts. Shuffle them
*  down so that we can assign a new value to SUM2.
            LLLSUM2 = LLSUM2
            LLSUM2 = LSUM2
            LSUM2 = SUM2

*  Form the SUM2 value (the auto-correlation) for this shift.
            J = 1
            SUM2 = 0.0
            DO I = SHIFT + 1, HISTSZ
               SUM2 = SUM2 + HIST( I )*HIST( J )
               J = J + 1
            END DO

*  Increment sums used to find the weighted mean of the change between
*  adjacent auto-correlation values. The weight is the recprocal of the
*  auto-correlation value, thus giving greater weight to the lower,
*  background, values and tending to ignore the steep edges of the peaks
*  in the auto-correlation function.
            IF( SUM2 .GT. 1.0 ) THEN
               W = 1.0/SUM2
            ELSE
               W = 1.0
            END IF
            SUMINC = SUMINC + W*ABS( SUM2 - LSUM2 )
            SUMW = SUMW + W

*  If the SUM2 value is smaller than the smallest value found so far,
*  record it and reset the count of larger SUM2 values found since the
*  most recent minimum..
            IF( SUM2 .LT. MINSUM ) THEN
               MINSUM = SUM2
               MINSH = SHIFT
               COUNT = 0

*  Otherwise, increment the count of shifts that have produced a higher
*  SUM" value than the minimum.
            ELSE
               COUNT  = COUNT + 1

*  If this is the first shift after the minimum, record the SUM2 values
*  on either side of the minimum.
               IF( COUNT .EQ. 1 ) THEN
                  USUM = SUM2
                  DSUM = LLSUM2

*  If this is the second shift after the minimum, accept the minimum as
*  found.
               ELSE IF( COUNT .LE. 2 ) THEN
                  MORE = .FALSE.

c      write(*,*) 'Found minimum ',minsum,' in auto-correlation '//
c     :           'function at ',minsh, '(',minsum,')'

               END IF

            END IF

*  Increment the shift and abort if we have reached the end of the
*  array.
            SHIFT = SHIFT + 1
            IF( SHIFT .EQ. HISTSZ ) MORE = .FALSE.

         END DO

*  Now continue to evaluate the auto-correlation at increasing shifts
*  until a maximum is found that is more than half the value at zero
*  shift and is greater than the subsequent two values. In case no such
*  point is found, we also record the largest value found, and the mean
*  of all values (except for the first point at zero shift).
         IF( SHIFT .LT. HISTSZ ) THEN

            MAXSH = -1
            MAXSUM = MINSUM
            LIMSUM = 0.5*NEWAMP
            SHIFT = MINSH + 1
            SUM2 = MINSUM
            LSUM2 = DSUM
            LLSUM2 = DSUM
            LLLSUM2 = DSUM

            MORE = .TRUE.
            DO WHILE( MORE )

               LLLSUM2 = LLSUM2
               LLSUM2 = LSUM2
               LSUM2 = SUM2

               J = 1
               SUM2 = 0.0
               DO I = SHIFT + 1, HISTSZ
                  SUM2 = SUM2 + HIST( I )*HIST( J )
                  J = J + 1
               END DO

               IF( SUM2 .GT. 1.0 ) THEN
                  W = 1.0/SUM2
               ELSE
                  W = 1.0
               END IF
               SUMINC = SUMINC + W*ABS( SUM2 - LSUM2 )
               SUMW = SUMW + W

               IF( SUM2 .GT. MAXSUM ) THEN
                  MAXSUM = SUM2
                  MAXSH = SHIFT
                  COUNT = 0

               ELSE IF( MAXSH .NE. -1 ) THEN
                  COUNT  = COUNT + 1
                  IF( COUNT .EQ. 1 ) THEN
                     USUM = SUM2
                     DSUM = LLSUM2

                  ELSE IF( COUNT .LE. 2 .AND. MAXSUM .GT. LIMSUM ) THEN
                     MORE = .FALSE.
                  END IF

               END IF

               SHIFT = SHIFT + 1
               IF( SHIFT .EQ. HISTSZ ) MORE = .FALSE.

            END DO


*  If no peak was found that was more than half the value at zero shift,
*  use the maximum value found so long as it is more than 10 times the
*  noise in the auto-correlation function (this noise is estimated as
*  the weighted mean of the variation between adjacent auto-correlation
*  values), and it is shorter than one third of the histogram length (i.e.
*  we have at least 3 waves in the histogram).
            IF( SHIFT .EQ. HISTSZ .AND. MAXSH .NE. -1 ) THEN
               IF( MAXSUM .GT. 10.0*(SUMINC/SUMW) .AND.
     :             MAXSH .LT. HISTSZ/3 ) THEN
                  SHIFT = MAXSH
               END IF
            END IF

         END IF

c         call opgrd_dump( histsz, hist, .false., ang, status )
c         call opgrd_autodump( ang, histsz, hist, status )

*  Check a peak was found.
         IF( SHIFT .LT. HISTSZ ) THEN

*  Fit a quadratic to the AMP values at the highest point (MAXSH) and
*  its two neighbours, and then find the peak of the quadratic in order
*  to get a more accurate estimate of the peak position.
            XSHIFT = MAXSH + 0.5*( DSUM - USUM )/
     :                           ( DSUM + USUM - 2*MAXSUM )

*  Convert the shift value to a wavelength in grid pixels.
            NEWWAV = XSHIFT*SPC

*  We use the new angle if the auto-correlation peak produces a greater
*  value than the old angle. We include a weighting factor that gives
*  priority to larger wavelengths. This seems to distinguish succesfully
*  between the periodicity produced when viewing a grid parallel to an
*  axis and when viewing it at 45 degrees to an axis. However, the
*  exponent used in this weighting factor has been determined by trial
*  and error and may not be suitable in all cases. Time will tell. Set up
*  the weight for the old periodicity.
            OLDWGT = MXWAVE**0.7

*  Now set up the weight for the new periodicity. If the new periodicity
*  is weaker than the old, only accept it if the wavelength is within a
*  factor of 1.5 of the old wavelength. Rotation by 45 degrees will
*  change the wavelength by a factor of sqrt(2)=1.414 so any ratio larger
*  than this is unlikely to be ucased by a 45 degree rotation.
            IF( ( NEWAMP .LT. MXAMP  ) .AND.
     :          ( NEWWAV .lT. MXWAVE/1.5 .OR.
     :            NEWWAV .GT. MXWAVE*1.5 ) ) THEN
               NEWWGT = 0.0
            ELSE
               NEWWGT = NEWWAV**0.7
            END IF

c      write(*,*) '   new total: ',NEWAMP*NEWWGT,
c     :           '   old total: ',MXAMP*OLDWGT

            IF( NEWAMP*NEWWGT .GT. MXAMP*OLDWGT ) THEN

c      write(*,*) '   Using new total - setting MXAMP to ',NEWAMP

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
*     Calculates the RMS deviation between the supplied positions and
*     the corresponding pixel centres.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_OPGR3( N, X, Y, A, B, STATUS )

*  Description:
*     This function returns the sum of the squared deviations between
*     each supplied fractional GRID positions, after being shifted by a
*     small amount (A,B), and the GRID position at the centre of the
*     pixel containing the shifted position.

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
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

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

*  Calculate the squared deviation between this shifted position, and
*  the centre of the pixel containing the shifted position, and
*  increment the returned value by this amount.
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
*     This routine finds a small fractional shift in grid co-ordinates
*     less than one pixel on each axis) that results in the supplied set
*     of grid positions being close the centre of the corresponding
*     pixels.

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
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

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
*  test grid. The initial test grid covers a little more than one grid
*  pixel.
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

*  Loop round every cell in the current test grid, finding the pixel
*  offsets corresponding to the centre of the cell.
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
      include 'AST_PAR'

      logical dumpit
      integer pass
      common /fred/ dumpit,pass

      double precision ang, dang
      integer n, status, place, indf, el, iat, pntr
      real dat( n )
      logical fft
      character name*30

      dumpit = .true.

      if( status .ne. sai__ok .or. .not. dumpit ) return

      if( fft ) then
         name = 'fft_'
         iat = 4
      else
         name = 'hist_'
         iat = 5
      end if

      call chr_appnd( char(pass), name, iat )
      call chr_appnd( '_', name, iat )

      dang = AST__DR2D*ang


      call chr_puti( int(dang), name, iat )
      call chr_putc( '_', name, iat )
      call chr_puti( nint(10*(dang-int(dang))), name, iat )
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
      include 'AST_PAR'

      double precision ang
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
