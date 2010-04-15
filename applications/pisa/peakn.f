      SUBROUTINE PEAKN( IMAGE, NCOLS, NLINES, XORIG, YORIG, GSIGM,
     :                  CROSS, COMIX, BACK, THRESH, IFS, IFO,
     :                  STATUS )
*+
*  Name:
*     PEAKN

*  Purpose:
*     To transform the PISAFIND parameters into values which can be used
*     by discrimination and thresholding routines to separate out
*     objects with parameters similar to stellar objects.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL PEAKN( IMAGE, NCOLS, NLINES, XORIG, YORIG, GSIGM,
*                  CROSS, COMIX, BACK, THRESH, IFS, IFO, STATUS )

*  Description:
*     The routine reads in the PISAFIND parameters from the file
*     attached to IFS ( using the FIO system). Using the PISA profiling
*     function it works out what the average value of a bin 3 by 3
*     pixels about the centre of the current object is. It then averages
*     the values in a 3 by 3 bin on the objects centre. The ratio of
*     these values gives the scaling factor from the PSF to the object
*     data. This 'pins' the PSF and object at about the peak value.
*     Using this the effective radius of the PISA function at the
*     detection threshold is derived (there is no analytic solution for
*     this so a recursive method is used). The semi-major axis of the
*     object is derived from the PISAFIND parameters. The ratio of these
*     values then forms a 'peakedness' measure. The intensity to peak
*     ratio of the object is formed and normalised by the value expected
*     from the profile function. The absolute value of the intensity
*     weighted cross moment is formed. The above three values are then
*     written to the output file together with the ellipticity.


*      CALL PEAKN( IMAGE, NCOLS, NLINES, XORIG, YORIG, GSIGM,
*                  CROSS, COMIX, BACK, THRESH, IFS, IFO, STATUS )

*  Arguments:
*     IMAGE( NCOLS, NLINES ) = REAL (Given)
*        The data array containing the detected objects
*     NCOLS = INTEGER (Given)
*        The first dimension of array IMAGE.
*     NCOLS = INTEGER (Given)
*        The second dimension of array IMAGE.
*     XORIG = INTEGER (Given)
*        The X origin offset of the coordinate system of IMAGE.
*     YORIG = INTEGER (Given)
*        The Y origin offset of the coordinate system of IMAGE.
*     GSIGM = REAL (Given)
*        Gaussian sigma value for profiling function.
*     CROSS = REAL (Given)
*        The cross point as a percentage of the peak intensity.
*     COMIX = REAL (Given)
*        The mixture fraction between the functions.
*     BACK = REAL (Given)
*        The background value of frame, subtracted before intensity
*        estimates are derived.
*     THRESH = REAL (Given)
*        The detection threshold value used by PISAFIND in deriving the
*        parameterisations. Used as threshold to estimate the model
*        radius at.
*     IFS = INTEGER (Given)
*        The FIO system descriptor of file containing the PISAFIND
*        parameterisations.
*     IFO = INTEGER (Given)
*        The FIO system descriptor of file to contain the results.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-MAR-1991 (PDRAPER):
*        Original version.
*     6-MAY-1992 (PDRAPER):
*        Added PRM_PAR.
*     14-AUG-2005 (TIMJ):
*        Fix standards uncompliant SQRT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  External References:
      INTEGER CHR_LEN            ! Length of string
      EXTERNAL CHR_LEN

*  Arguments Given:
      INTEGER NLINES
      INTEGER NCOLS
      REAL IMAGE( NCOLS, NLINES )
      INTEGER XORIG
      INTEGER YORIG
      REAL GSIGM
      REAL CROSS
      REAL COMIX
      REAL BACK
      REAL THRESH
      INTEGER IFS
      INTEGER IFO

*  Local Constants
      REAL NOTZER
      PARAMETER ( NOTZER = 1.0E-10)
      REAL PI
      PARAMETER ( PI = 3.1415926 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*132
      INTEGER I, J
      INTEGER INDEX
      INTEGER IX, IY
      INTEGER NACC
      INTEGER NOBJ
      INTEGER NPIX
      INTEGER NVALS
      INTEGER NCHAR
      INTEGER IRAD
      REAL ANGLE
      REAL B
      REAL CHANGE
      REAL COEF1
      REAL COEF2
      REAL ELLIP
      REAL FLOR
      REAL FRAC
      REAL FUP
      REAL FVAL
      REAL INTENS
      REAL MODIBP
      REAL MRATIO
      REAL OBJVAL
      REAL PEAK
      REAL PRATIO
      REAL PSFRAD
      REAL RAD
      REAL RADTHR
      REAL SCALE
      REAL SUM
      REAL SXX
      REAL SXY
      REAL SYY
      REAL UP, LOW, STEP
      REAL XFRAC
      REAL XINT
      REAL XPOS
      REAL XSQ
      REAL YFRAC
      REAL YPOS
      REAL YSQ

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up model parameters
      FRAC = MAX( NOTZER, CROSS * 0.01 )
      CHANGE = LOG( FRAC )
      COEF1 = -1.0 / GSIGM**2

* -COEF1 needs to be in parentheses to be standards compliant
      COEF2 = SQRT( ABS( -4.0 * CHANGE * ( -COEF1 ) ) )
      RADTHR = GSIGM * SQRT( ABS( -CHANGE ) )

*  Work out the model intensity-peak ratio
      MODIBP = PI *GSIGM *GSIGM *( 1.0 +FRAC/ ( 2.0 *LOG( 1.0 /FRAC )) )

*  Read the data - one line at a time from the PISAFIND data file
*  read in the data from this file until EOF is reached.
      NOBJ = 0
1     CONTINUE
         CALL RDPIFD( IFS, BUF, INDEX, XPOS, YPOS, INTENS, NPIX, PEAK,
     :                ELLIP, ANGLE, SXX, SYY, SXY, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 6
         NOBJ = NOBJ + 1

*  correct for origins
         XPOS = XPOS - ( XORIG - 1.5 )
         YPOS = YPOS - ( YORIG - 1.5 )

*  Check that this position is within the bounds of the image.
         IX = INT( XPOS )
         IY = INT( YPOS )
         IF ( IX .GE. 1  .AND.  IX .LE. NCOLS  .AND.
     :        IY .GE. 1  .AND.  IY .LE. NLINES ) THEN

*  Get the semi-major axis.
             B = SQRT( REAL( NPIX ) / ( PI * ( 1.0 - ELLIP ) ) )

*  Find the equivalent radius for the PSF at this threshold
*  First need to integrate the three * three peak pixel values
            XFRAC = XPOS - INT( XPOS )
            YFRAC = YPOS - INT( YPOS )
            SUM = 0.0
            DO 2 I = -5, 10
               YSQ = REAL( I ) / 5.0 - YFRAC
               YSQ = YSQ * YSQ
               DO 3 J = -5, 10
                  XSQ = REAL( I ) / 5.0 - XFRAC
                  XSQ = XSQ * XSQ
                  RAD = SQRT( XSQ + YSQ )

*              gaussian argument
                  XINT = COEF1 * RAD * RAD

*              use functions appropriate to whether we're above or
*              below the threshold value
                  FLOR = COMIX / ( 1.0 - XINT / LOG( 2.0 ) )
                  IF ( XINT .GE. CHANGE ) THEN
                     FUP = ( 1.0 - COMIX ) * EXP( XINT )
                  ELSE
                     FUP = ( 1.0 - COMIX )*
     :                     EXP( CHANGE + ( RADTHR - RAD )* COEF2 )
                  END IF
                  SUM = SUM + FLOR + FUP
 3             CONTINUE
 2          CONTINUE

*  the mean value in THREE*THREE pixelS with central brightness at
*  current position
            SUM = SUM / 256.0

*  find the intensity appropriate to scale this value by
            OBJVAL = 0.0
            NVALS = 0
            DO 7 I = -1, 1
               IF( IY + I .GT. 1 .AND. IY + I .LT. NLINES) THEN
                  DO 8 J = -1, 1
                     IF( IX + J .GT. 1 .AND. IX + J .LT. NCOLS  ) THEN
                        IF( IMAGE( IX + J, IY + I ) .NE. VAL__BADR )
     :                  THEN
                           OBJVAL = OBJVAL + IMAGE( IX + J, IY + I )
                           NVALS = NVALS + 1
                        END IF
                     END IF
8                 CONTINUE
               END IF
7           CONTINUE
            OBJVAL = OBJVAL / MAX( NVALS, 1 )
            SCALE = OBJVAL / MAX( SUM, NOTZER )

*  find the radius at which the psf scaled by this value equals the
*  threshold intensity, BIT ROUGH
            UP = 100.0
            LOW = 0.0
            STEP = 10.0
            NACC = 0

9           CONTINUE
            RAD = LOW
            DO 4 IRAD = 1, INT( ( UP - LOW ) / STEP ) + 1
               XINT = COEF1 * RAD * RAD
               FLOR = COMIX / ( 1.0 - XINT / LOG( 2.0 ) )
               IF ( XINT .GE. CHANGE ) THEN
                  FUP = ( 1.0 - COMIX ) * EXP( XINT )
               ELSE
                  FUP = ( 1.0 - COMIX ) *
     :                  EXP( CHANGE + ( RADTHR - RAD ) * COEF2 )
               END IF
               FVAL = FLOR + FUP
               FVAL = FVAL * SCALE
               IF( FVAL .LT. THRESH ) THEN
                  NACC = NACC + 1
                  IF( NACC .GT. 5 ) THEN
                     PSFRAD = RAD
                     GO TO 5
                  END IF

*  and now set up loop variables for next refinement of value
                  UP = RAD
                  LOW = RAD - STEP
                  STEP = STEP / 10.0
                  GO TO 9
               END IF

*  increment for next loop.
               RAD = RAD + STEP
4           CONTINUE

5           CONTINUE ! break out

*  form the ratio of semi-major to psf radius
            PRATIO = B / MAX( PSFRAD, NOTZER)

*  divide the actual intensity by peak ratio by the model value
*  to normalise to one
            MRATIO = INTENS / ( PEAK * MODIBP )

*  write out the results the output file, add other useful
*  classification values.
            BUF = ' '
            CALL CHR_ITOC( INDEX, BUF( 4: ), NCHAR )
            CALL CHR_RTOC( PRATIO, BUF( 12: ), NCHAR )
            CALL CHR_RTOC( MRATIO, BUF( 29: ), NCHAR )
            CALL CHR_RTOC( ELLIP, BUF( 46: ), NCHAR )
            CALL CHR_RTOC( ABS( SXY ), BUF( 63: ), NCHAR )
            CALL FIO_WRITE( IFO, BUF( :CHR_LEN( BUF ) ), STATUS )
         END IF

*  Return for next object.
         GO TO 1
6     CONTINUE

* may have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
          CALL ERR_ANNUL( STATUS )
      ENDIF

* if no objects have been found then exit
      IF (  NOBJ  .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NOENTRIES',
     :                 'input file contains no valid entries', STATUS )
      ENDIF
      END
* $Id$
