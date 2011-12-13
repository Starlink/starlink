      SUBROUTINE KPS1_CREMG( IDIM1, IDIM2, MAX, MIN, BCKGRD, NGAUSS,
     :                       FWHM, DISTRB, BADPIX, FRACTN, BADCOL,
     :                       BADROW, SCREEN, PARAM1, PARAM2, VARS,
     :                       IMAGE, ERROR, WORK, STATUS )
*+
*  Name:
*     KPS1_CREMG

*  Purpose:
*     Generates 2-D gaussians for CREFRAME

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CREMG( IDIM1, IDIM2, MAX, MIN, BCKGRD, NGAUSS, FWHM,
*                      DISTRB, BADPIX, FRACTN, BADCOL, BADROW, SCREEN,
*                      PARAM1, PARAM2, VARS, IMAGE, ERROR, WORK, STATUS )

*  Description:
*     This routine generates a specified number of 2-D Gaussian images
*     with dimensions, intensities and so on as input. A pseudo-
*     Poissonian noise distribution is added, and the option to
*     simulate bad pixels and columns is included. The relevant Gaussian
*     parameters may be output via the screen or into a file.

*  Arguments:
*     IDIM1 = INTEGER (Given)
*         First dimension of array to be created
*     IDIM2 = INTEGER (Given)
*         Second dimension of array to be created
*     MAX = REAL (Given)
*         Maximum stellar intensity
*     MIN = REAL (Given)
*         Minimum stellar intensity
*     BCKGRD = REAL (Given)
*         Background value to be used
*     NGAUSS = INTEGER (Given)
*         Number of stars to be generated
*     FWHM = REAL (Given)
*         Full Width Half Maximum of Gaussian (equivalent to seeing)
*         in pixels
*     DISTRB = CHARACTER*3 (Given)
*         Radial distribution of Gaussians - default (RSQ) gives
*         1-over-r squared; FIX gives fixed distance
*     BADPIX = LOGICAL (Given)
*         True if bad pixels are to be included
*     FRACTN = REAL (Given)
*         Fraction of pixels that are to be made bad
*     BADCOL = INTEGER (Given)
*         Number of bad columns to be included
*     BADROW = INTEGER (Given)
*         Number of bad rows to be included
*     SCREEN = LOGICAL (Given)
*         True if Gaussian parameters are to be reported
*     PARAM1 = CHARACTER*(*) (Given)
*         Parameter name for the name of the log file to receive the
*         Gaussian parameters
*     PARAM2 = CHARACTER*(*) (Given)
*         Parameter name for the name of the catalogue to receive a
*         list of the Gaussian positions.
*     VARS = LOGICAL (Given)
*         Whether the variance component should be populated
*     IMAGE( IDIM1, IDIM2 ) = REAL (Returned)
*         Created image.
*     ERROR( IDIM1, IDIM2 ) = REAL (Returned)
*         Created variances.
*     WORK( NGAUSS, 2 ) = DOUBLE PRECISION (Returned)
*         Work array.
*     STATUS = INTEGER (Given and Returned)
*         Global status value

*  Copyright:
*     Copyright (C) 1985-1990, 1992-1993 Science & Engineering Research
*     Council. Copyright (C) 2001 Central Laboratory of the Research
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
*     MJM: Mark McCaughrean
*     MJC: Malcolm Currie (Starlink, RAL)
*     AALLAN: Alasdair Allan (Starlink, University of Exeter)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-OCT-1985 (MJM):
*        First Adam implementation (from Aspic MANYG)
*     09-DEC-1985 (MJM):
*        Changed to call POISSON noise subroutine
*     14-JAN-1986 (MJM):
*        Added display options
*     14-AUG-1986 (MJC): C
*        Changed argument order, renamed POISSON and reordered its argument,
*        completed the prologue, nearly conformed to Starlink programming
*        standards, open IOSTAT added.
*     04-SEP-1986 (MJC):
*        Renamed parameters section to arguments, applied bad-pixel handling.
*     30-NOV-1987 (MJC):
*        Bug fix - now uses SIGMA for Gaussian, and E format used for output
*        of peak values.
*     27-JUN-1988 (MJC):
*        Converted to FIO, added error reporting and restructured.
*     05-AUG-1988 (MJC):
*        Removed lingering astronomical references and SCALE argument.
*     25-JUL-1989 (MJC):
*        Removed DSPLAY argument --- output can now be to both, either, or
*        neither the screen and/or the file; passed array dimensions as
*        separate variables
*     27-JUL-1989 (MJC):
*        Used packaged access for obtaining the log file
*     20-FEB-1990 (MJC):
*        AIF_OPFIO renamed AIF_ASFIO
*     03-AUG-1990 (MJC):
*        Converted to use Starlink standard co-ordinates rather than pixel
*        indices; output file will now work with PHOTOM.
*     17-MAR-1992 (MJC):
*        Used portable random-number generation.
*     09-FEB-1993 (MJC):
*        Used the improved FIO_ASSOC and the new FIO_ANNUL.
*     26-AUG-2001 (AALLAN):
*        Renamed subroutine to confirm to KAPPA standard
*     01-SEP-2001 (AALLAN):
*        Changed BADCOL to an integer, added BADROW and handling of varainces
*     02-SEP-2001 (AALLAN):
*        Changed prologue to conform to Starlink standards
*     11-SEP-2001 (DSB):
*        Changed layout of code comments, and variable declarations.
*        Added args PARAM2 and WORK.
*     {enter_further_changes_here}

*-

*  Type definitions:
      IMPLICIT  NONE

*  Global constants :
      INCLUDE 'SAE_PAR'        ! SSE global definitionsffssdaw
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Magic-value constants
      INCLUDE 'AST_PAR'        ! AST constants and functions

*  Arguments Given:
      INTEGER IDIM1
      INTEGER IDIM2
      REAL MAX
      REAL MIN
      REAL BCKGRD
      INTEGER NGAUSS
      REAL FWHM
      CHARACTER DISTRB*3
      LOGICAL BADPIX
      REAL FRACTN
      INTEGER BADCOL
      INTEGER BADROW
      LOGICAL SCREEN
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)
      LOGICAL VARS

*  Arguments Returned:
      REAL IMAGE( IDIM1, IDIM2 )
      REAL ERROR( IDIM1, IDIM2 )
      DOUBLE PRECISION WORK( NGAUSS, 2 )

*  Global Status :
      INTEGER  STATUS

*  External References:
      REAL SLA_RANDOM           ! Random-number generator
      REAL KPG1_SEED            ! Random-number seed initialisation

*  Local Constants :
      INTEGER NCHLIN           ! maximum number of characters in an
      PARAMETER( NCHLIN = 44 ) ! output record

*  Local variables :
      CHARACTER BUFFER*(NCHLIN)! buffer to store output string
      INTEGER BOX              ! half size of box round each star
      INTEGER COLPOS           ! random bad column position
      INTEGER CURBAD           ! number of pixels currently set bad
      INTEGER FD               ! file description
      INTEGER I                ! General counter
      INTEGER IWCS             ! Temporary FrameSet defining pixel coords
      INTEGER J                ! General counter
      INTEGER K                ! General counter
      INTEGER L                ! General counter
      INTEGER N                ! General counter
      INTEGER NUMBAD           ! number required to be set bad
      INTEGER ROWPOS           ! random bad row position
      INTEGER XBAD             ! x position of random bad pixel
      INTEGER XFINSH           ! x finish co-ord of box round Gaussian
      INTEGER XSTART           ! x start co-ord of box round Gaussian
      INTEGER YBAD             ! y     "     "    "    "    "
      INTEGER YFINSH           ! y    "     "    "  "    "       "
      INTEGER YSTART           ! y   "     "    "  "    "       "
      LOGICAL LOGGP            ! Gaussian parameters to be logged?
      REAL CURRX               ! X distance of pixel from Gaussian centre
      REAL CURRY               ! Y distance of pixel from Gaussian centre
      REAL INTENS              ! Calculated intensity for current pixel
      REAL INTRVL              ! Interval between max. and min. Gaussian peaks
      REAL PEAK                ! Peak brightness of current Gaussian
      REAL SEED                ! Seed variable for random numbers
      REAL SIGMA               ! Standard deviation of Gaussian in pixels
      REAL VALUE               ! General random variable from SLA_RANDOM
      REAL XPOS                ! Randomly generated x centre of Gaussian
      REAL YPOS                !     "        "     y    "    "    "

*.

*  Check the global inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start by checking to see if output of the Gaussian parameters
*  is required, and act accordingly
      LOGGP = .FALSE.
      CALL FIO_ASSOC( PARAM1, 'WRITE', 'FORTRAN', NCHLIN, FD, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOGGP = .TRUE.
      END IF

      IF( LOGGP ) THEN
         CALL MSG_SETC( 'PARAM', PARAM1 )
         CALL MSG_OUT( 'LOG', 'Logging to $^PARAM', STATUS )
      END IF

*  Write caption.
      CALL KPG1_REPRT( '  Number   x centre  y centre      Peak',
     :                 .NOT. SCREEN, LOGGP, FD, STATUS )

*  Work out the interval between max and min
      INTRVL  =  MAX - MIN

*  Work out a box size to be used from the Gaussian FWHM
      BOX   =  NINT( FWHM * 5 )

*  Convert FWHM to standard error
      SIGMA = FWHM / 2.354

*  Fill array with the background value
      DO  J  =  1, IDIM2
         DO  I  =  1, IDIM1
            IMAGE( I, J )  =  BCKGRD

*  Populate the variance component if required
            IF( VARS ) THEN
               ERROR( I, J ) = IMAGE( I, J )
            ENDIF
         END DO
      END DO

*  Initialise the random-number generator seed.  It is taken as input
*  by SLA_RANDOM, which returns a pseudo-random number between 0 and
*  1, and is updated on return.
      SEED = KPG1_SEED( STATUS )

*  Loop round the requested number of Gaussians generating a random
*  x,y position and a peak intensity. Then create a Gaussian of that
*  intensity at that point
      DO  N  =  1, NGAUSS

*  Get a random, non-integral x,y position
         VALUE  =  SLA_RANDOM( SEED )
         XPOS   =  VALUE * REAL( IDIM1 )
         VALUE  =  SLA_RANDOM( SEED )
         YPOS   =  VALUE * REAL( IDIM2 )

*  Store the position.
         WORK( N, 1 ) = DBLE( XPOS )
         WORK( N, 2 ) = DBLE( YPOS )

*  Now get a random peak intensity within the specified range,
*  and according to the specified radial distribution
         VALUE  =  SLA_RANDOM( SEED )

*  Force the distribution string to upper case before checking it
         CALL CHR_UCASE( DISTRB )

         IF( DISTRB .EQ. 'FIX' ) THEN

*  We are generating all our Gaussian at the same radial
*  distance. The Gaussians should be more or less uniformly
*  distributed in brightness.
            PEAK  = ( VALUE * INTRVL ) + MIN
         ELSE

*  ! Warning - ASTRONOMICAL content !
*  The requested distribution is one over r-squared - thus we
*  have one factor of r-squared from the fact that we see more
*  stars the further away we look, and another factor of
*  r-squared as the brightness of a star dims as the square of
*  its distance.
            VALUE  =  VALUE ** 4
            PEAK   = ( VALUE * INTRVL ) + MIN
         END IF

*  Now we have the x,y position, peak intensity, and FWHM, we can
*  create the Gaussian star - first define a box in which the star
*  sits. BOX is the half-side dimension of the box in pixels.
         XSTART  =  NINT( XPOS -( BOX * FWHM ) + 0.5 )
         XFINSH  =  NINT( XPOS +( BOX * FWHM ) + 0.5 )
         YSTART  =  NINT( YPOS -( BOX * FWHM ) + 0.5 )
         YFINSH  =  NINT( YPOS +( BOX * FWHM ) + 0.5 )

*  Loop round the y dimension of the calculated box
         DO  L  =  YSTART, YFINSH

*  Check whether or not this pixel is in array - continue if so
            IF( L .GE. 1 .AND. L .LE. IDIM2 ) THEN

*  Work out y distance of pixel from Gaussian's centre
               CURRY  =  ABS( REAL( L ) - 0.5 - YPOS )

*  Loop round x dimension of box
               DO  K  =  XSTART, XFINSH

*  Check whether pixel in array - continue if so
                  IF( K .GE. 1 .AND. K .LE. IDIM1 ) THEN

*  Work out x distance of pixel from Gaussian's centre
                     CURRX  =  ABS( REAL( K ) - 0.5 - XPOS )

*  Work out intensity at this point from Gaussian profile formula
                     INTENS  =  PEAK *
     :                          EXP( -( CURRX*CURRX + CURRY*CURRY ) /
     :                         ( 2.0 * SIGMA * SIGMA ) )

*  Add this intensity to any existing flux
                     IMAGE( K, L )  =  IMAGE( K, L ) + INTENS

*  Populate the variance component if required
                     IF( VARS ) ERROR( K, L ) = IMAGE( K, L )

                  END IF
               END DO
            END IF
         END DO

*  Write out the Gaussian parameters if so requested
         IF( SCREEN .OR. LOGGP ) THEN
            WRITE( BUFFER, '( I6,2X,2F10.3,E15.6 )' ) N, XPOS, YPOS,
     :             PEAK
            CALL KPG1_REPRT( BUFFER, .NOT. SCREEN, LOGGP, FD, STATUS )
         END IF

*  Bottom of loop for Gaussians
      END DO

*  Close any open units at this point
      IF( LOGGP ) CALL FIO_ANNUL( FD, STATUS )

*  Create any output catalogue.
      CALL AST_BEGIN( STATUS )
      IWCS = AST_FRAMESET( AST_FRAME( 2, 'DOMAIN=PIXEL', STATUS ),
     :                     ' ', STATUS )
      CALL KPG1_WRLST( PARAM2, NGAUSS, NGAUSS, 2, WORK, 1, IWCS,
     :                 'Centres of Gaussians created by CREFRAME', 1,
     :                 0, .TRUE., STATUS )
      CALL AST_END( STATUS )

*  We have created the background and the Gaussians, we can add
*  the pseudo-Poissonian noise.  The routine KPG1_POISR takes the
*  array and adds or subtracts a semi-random number to the input
*  according to a Poisson statistical distribution.
      CALL KPG1_POISR( IDIM1 * IDIM2, IMAGE, SEED, STATUS )

*  Next, create some bad pixels if so requested - bad is defined as
*  dead in this context
      IF( BADPIX ) THEN

*  Work out number of bad pixels from input fraction
         NUMBAD  =  INT( FRACTN * REAL( IDIM1 * IDIM2 )  )

*  Initialise the current number of pixels set bad
         CURBAD  =  0

*  Loop until the requested number of pixels are set bad
         DO WHILE( CURBAD .LT. NUMBAD )

*  Using the SLA_RANDOM function again, get a random bad-pixel
*  position, making sure it is in the image
            VALUE  =  SLA_RANDOM( SEED )
            XBAD   =  NINT( VALUE * REAL( IDIM1 ) )
            IF( XBAD .EQ. 0 ) XBAD = 1
            VALUE  =  SLA_RANDOM( SEED )
            YBAD   =  NINT( VALUE * REAL( IDIM2 ) )
            IF( YBAD .EQ. 0 ) YBAD = 1

*  If this pixel is non-zero, set it to zero, and increment the
*  counter by one - else continue
            IF( IMAGE( XBAD, YBAD ) .NE. VAL__BADR ) THEN
               IMAGE( XBAD, YBAD )  =  VAL__BADR
               CURBAD  =  CURBAD + 1
            END IF

         END DO

*  End of IF( BADPIX ) condition

      END IF

*  Finally, include a bad columns if requested
      IF( BADCOL .NE. 0 ) THEN

*  Get a random column that is in the image
         DO I = 1, BADCOL
            VALUE   =  SLA_RANDOM( SEED )
            COLPOS  =  NINT( VALUE * REAL( IDIM1 ) )
            IF( COLPOS .EQ. 0 ) COLPOS =( IDIM1 + 1 )/ 2

*  Set all pixels in that column to zero
            DO  J  =  1, IDIM2
               IMAGE( COLPOS, J ) = VAL__BADR
              IF( VARS ) THEN
                 ERROR( COLPOS, J ) = VAL__BADR
              ENDIF
            END DO
         END DO

*  End of IF( BADCOL .NE. 0 ) condition

      END IF

*  Add a bad row if required
      IF( BADROW .NE. 0 ) THEN

*  Get a random column that is in the image
         DO I = 1, BADROW

            VALUE   =  SLA_RANDOM( SEED )
            ROWPOS  =  NINT( VALUE * REAL( IDIM2 ) )
            IF( ROWPOS .EQ. 0 ) COLPOS =( IDIM2 + 1 )/ 2

*  Set all pixels in that column to zero
            DO  J  =  1, IDIM1
              IMAGE( J, ROWPOS ) = VAL__BADR
              IF( VARS ) THEN
                 ERROR( J, ROWPOS ) = VAL__BADR
              ENDIF
            END DO
         END DO

*  End of IF( BADROW ) condition

      END IF

      END
