      SUBROUTINE KPG1_PSFSD( PSF, NPIX, NLIN, WORK, NPW, NLW, FRACT,
     :                       ILEVEL, XSIZE, YSIZE, STATUS )
*+
*  Name:
*     KPG1_PSFSx
 
*  Purpose:
*     Finds the approximate size of a 2-dimensional PSF.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_PSFSx( PSF, NPIX, NLIN, WORK, NPW, NLW, FRACT, ILEVEL,
*                      XSIZE, YSIZE, STATUS )
 
*  Description:
*     Marginal profiles are formed of the absolute PSF values along
*     both axes.  For each axis the maximum and minimum values in the
*     corresponding profile are found.  The first and last point at
*     which each profile reaches a specified fraction of its total
*     range is found, and the difference returned as the corresponding
*     PSF size.  N.B., it is assumed that the input PSF contains no bad
*     pixels.
 
*  Arguments:
*     PSF( NPIX, NLIN ) = ? (Given)
*        The PSF image.
*     NPIX = INTEGER (Given)
*        Number of pixels per line in the PSF image.
*     NLIN = INTEGER (Given)
*        Number of lines in the PSF image.
*     WORK( NPW, NLW ) = ? (Given)
*        Work space.
*     NPW = INTEGER (Given)
*        Number of elements per line in the work array.  This should be
*        at least equal to the maximum of NPIX and NLIN.
*     NLW = INTEGER (Given)
*        Number of lines in the work array.  This should be at least 2.
*     ILEVEL = INTEGER (Given)
*        The user information level.  If ILEVEL is 2 or more, then the
*        user is told what the calculated sizes are.
*     FRACT = REAL (Given)
*        The fraction of the PSF peak amplitude at which the PSF size
*        is determined.  It should be positive and less than 0.5.  If
*        it is outside this range one sixteenth is used.
*     XSIZE = INTEGER (Returned)
*        The width in x of the PSF, in units of pixels.
*     YSIZE = INTEGER (Returned)
*        The width in y of the PSF, in units of lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for processing single- and double-precision
*     arrays; replace "x" in the routine name by R or D as appropriate.
*     The data type of the PSF and WORK arguments must match the
*     routine used.
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*     4-MAR-1991 (DSB):
*        Name changed form PSFSIZ to KPS1_PSFS2
*     1991 July 18 (MJC):
*        Named changed from KPS1_PSFS2 to KPG1_PSFS2.  FRACT argument
*        added.
*     10-JAN-1995 (DSB):
*        Name changed from KPG1_PSFS2 to KPG1_PSFSR.  Re-formatted to
*        edstar-style. Expression for TARGET changed from:
*              TARGET = ( <X/Y>MAX - <X/Y>MIN ) * CFRACT
*        to:
*              TARGET = ( <X/Y>MAX - <X/Y>MIN ) * CFRACT + <X/Y>MIN
*
*        Also changed to use the absolute PSF values instead of the
*        supplied PSF values.
*     1995 March 22 (MJC):
*        Made generic, corrected typo's, removed long lines, various
*        other stylistic changes and used modern-style variable
*        declarations.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants
 
*  Arguments Given:
      INTEGER NPIX
      INTEGER NLIN
      DOUBLE PRECISION PSF( NPIX, NLIN )
      INTEGER NPW
      INTEGER NLW
      DOUBLE PRECISION WORK( NPW, NLW )
      REAL FRACT
      INTEGER ILEVEL
 
*  Arguments Returned:
      INTEGER XSIZE
      INTEGER YSIZE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      DOUBLE PRECISION CFRACT             ! Fraction of peak amplitude within a
                                 ! safe range
      INTEGER HILIM              ! Highest pixel no. at which the PSF is
                                 ! greater than the target value
      INTEGER LIN                ! Line counter
      INTEGER LOLIM              ! Lowest pixel no. at which the PSF is
                                 ! greater than the target value
      INTEGER OFFSET             ! Offset into the profile from either
                                 ! end
      INTEGER PIX                ! Pixel counter
      DOUBLE PRECISION PSFVAL             ! Current PSF value
      DOUBLE PRECISION T1                 ! Running sum for y profile
      DOUBLE PRECISION TARGET             ! Lowest significant PSF value
      DOUBLE PRECISION XMAX               ! Maximum value in the x profile
      DOUBLE PRECISION XMIN               ! Minimum value in the x profile
      DOUBLE PRECISION XVAL               ! Current x profile value
      DOUBLE PRECISION YMAX               ! Maximum value in the y profile
      DOUBLE PRECISION YMIN               ! Minimum value in the y profile
      DOUBLE PRECISION YVAL               ! Current y profile value
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Constrain the fraction to be a sensible value.
      IF ( FRACT .LT. VAL__SMLD .OR. FRACT .GT. 0.5 ) THEN
         CFRACT = 1.0D0 / 16.0D0
      ELSE
         CFRACT = FRACT
      END IF
 
*  Form marginal profiles in x and y and store them in the work array.
      DO PIX = 1, NPIX
         WORK( PIX, 2 ) = 0.0D0
      END DO
 
      DO LIN = 1, NLIN
 
         T1 = 0.0D0
 
         DO PIX = 1, NPIX
 
            PSFVAL = ABS( PSF( PIX, LIN ) )
            T1 = T1 + PSFVAL
            WORK( PIX, 2 ) = WORK( PIX, 2 ) + PSFVAL
 
         END DO
 
         WORK( LIN, 1 ) = T1
 
      END DO
 
*  Find the minimum and maximum of the y profile.
      YMAX = VAL__MIND
      YMIN = VAL__MAXD
 
      DO LIN = 1, NLIN
         YVAL = WORK( LIN, 1 )
         YMAX = MAX( YMAX, YVAL )
         YMIN = MIN( YMIN, YVAL )
      END DO
 
*  Find the width of the y profile at the specified fraction of the
*  maximum.
      TARGET = ( YMAX - YMIN ) * CFRACT + YMIN
 
      LOLIM = -1
      HILIM = -1
 
      DO OFFSET = 0, NLIN - 1
 
         IF ( LOLIM .EQ. -1 .AND.
     :        WORK( 1 + OFFSET, 1 ) .GE. TARGET )
     :      LOLIM = 1 + OFFSET
 
         IF ( HILIM .EQ. -1 .AND.
     :        WORK( NLIN - OFFSET, 1 ) .GE. TARGET )
     :      HILIM = NLIN - OFFSET
 
      END DO
 
*  If no width was found, something is wrong!
      IF ( LOLIM .EQ. -1 .OR. HILIM .EQ. -1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PSFSR_ERR1', 'Unable to find y extent '/
     :                 /'of the PSF.', STATUS )
         GO TO 999
 
*  Otherwise, set the output y width value.
      ELSE
         YSIZE = HILIM - LOLIM + 1
 
      END IF
 
*  Find the minimum and maximum of the x profile.
      XMAX = VAL__MIND
      XMIN = VAL__MAXD
 
      DO PIX = 1, NPIX
         XVAL = WORK( PIX, 2 )
         XMAX = MAX( XMAX, XVAL )
         XMIN = MIN( XMIN, XVAL )
      END DO
 
*  Find the width of the x profile at one sixteenth of the maximum.
      TARGET = ( XMAX - XMIN ) * CFRACT + XMIN
 
      LOLIM = -1
      HILIM = -1
 
      DO OFFSET = 0, NPIX-1
 
         IF ( LOLIM .EQ. -1 .AND.
     :       WORK( 1 + OFFSET, 2 ) .GE. TARGET )
     :      LOLIM = 1 + OFFSET
 
         IF ( HILIM .EQ. -1 .AND.
     :       WORK( NPIX - OFFSET, 2 ) .GE. TARGET )
     :      HILIM = NPIX - OFFSET
 
      END DO
 
*  If no width was found, something is wrong !
      IF ( LOLIM .EQ. -1 .OR. HILIM .EQ. -1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PSFSR_ERR2', 'Unable to find x extent '/
     :                 /'of PSF.', STATUS )
         GO TO 999
 
*  Otherwise, set the output x width value.
      ELSE
         XSIZE = HILIM - LOLIM + 1
 
      END IF
 
*  If required, display both widths.
      IF ( ILEVEL .GE. 2 ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_SETI( 'XW', XSIZE )
         CALL MSG_SETI( 'YW', YSIZE )
         CALL MSG_OUT( 'REPORT',
     :                '  PSF area is about ^XW by ^YW pixels.', STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
      END IF
 
 999  CONTINUE
 
      END
