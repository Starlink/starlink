      SUBROUTINE KPS1_DSCLB( BAD, DIM1, DIM2, INARR, INVERT, PARLOW,
     :                        PARUPP, LOWCI, HICI, BADCI, MINV, MAXV,
     :                        POSTIV, OUTARR, LOWER, UPPER, STATUS )
*+
*  Name:
*     KPS1_DSCLB
 
*  Purpose:
*     Scales a 2-d BYTE array between user-defined
*     limits into a cell array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation
*     CALL KPS1_DSCLx( BAD, DIM1, DIM2, INARR, INVERT, PARLOW, PARUPP,
*    :                 LOWCI, HICI, BADCI, MINV, MAXV, POSTIV, OUTARR,
*    :                 LOWER, UPPER, STATUS )
 
*  Description:
*     The user is given the minimum and maximum values found in a 2-d
*     BYTE array, and is then asked for the limits between which
*     it wants the image scaled. The image is then scaled such that
*     the lower limit and all values below it are set to the lowest pen
*     number; the upper limit and all values above are set to the
*     number of colour indices less one, and all valid values in
*     between are assigned values using linear interpolation between
*     the limits. The array may (normal) or may not be inverted on
*     display.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        If true bad pixels will be processed.  This should not be set
*        to false unless the input array contains no bad pixels.
*     DIM1 = INTEGER (Given)
*        The first dimension of the 2-d arrays.
*     DIM2 = INTEGER (Given)
*        The second dimension of the 2-d arrays.
*     INARR( DIM1, DIM2 ) = BYTE (Given)
*        The original, unscaled image data.
*     INVERT = LOGICAL (Given)
*        True if the image is to be inverted for display.
*     PARLOW = CHAR*(*) (Given)
*        The parameter name used to get the lower limit.
*     PARUPP = CHAR*(*) (Given)
*        The parameter name used to get the upper limit.
*     LOWCI = INTEGER (Given)
*        The lowest colour index to be used in case smaller values have
*        been reserved for annotation
*     HICI = INTEGER (Given)
*        The highest colour index to be used.  Normally this is the
*        number of greyscale intensities available on the chosen device.
*     BADCI = INTEGER (Given)
*         The colour index to be assigned to bad pixels in the scaled
*         array.
*     MINV = BYTE (Given)
*        The minimum value of the image.
*     MAXV = BYTE (Given)
*        The maximum value of the image.
*     POSTIV = LOGICAL (Given)
*        True if the defaults for %PARLOW and %PARUPP are the minimum
*        and maximum values respectively.
*     OUTARR( DIM1, DIM2 ) = INTEGER (Returned)
*        The scaled version of the image.
*     LOWER = BYTE (Returned)
*        The lower limit used for scaling the image.
*     UPPER = BYTE (Returned)
*        The upper limit used for scaling the image.
*     STATUS = INTEGER( READ, WRITE )
*        Value of the status on entering this subroutine.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by B, D, I, R or W as appropriate. The array and
*     limits supplied to the routine must have the data type specified.
*     -  The array is normally inverted so that the image will appear
*     the correct way round when displayed as the GKS routine
*     to display the image inverts it.
 
*  Algorithm:
*     - Report the maximum and minimum values in the array.
*     - Obtain the scaling limits, which must be different values.
*       The polarity of the defaults is controlled by the input switch.
*     - The scaled image is then produced with or without inversion,
*       and with or without bad-pixel checking via a subroutine. Bad
*       values are set to the defined value.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1990 July 12 (MJC):
*        Original version based on IMSCL.
*     1991 July 23 (MJC):
*        Added BADCI argument.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     25-AUG-1999 (DSB):
*        Do not cancel the PARLOW and PARUPP parameters after valid
*        values have been obtained.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
 
*  Status:
      INTEGER STATUS
 
*  Arguments Given:
      INTEGER
     :  DIM1, DIM2,
     :  LOWCI,
     :  HICI,
     :  BADCI
 
      LOGICAL
     :  BAD,
     :  INVERT,
     :  POSTIV
 
      CHARACTER*(*)
     :  PARLOW,
     :  PARUPP
 
      BYTE
     :  INARR( DIM1, DIM2 ),
     :  MAXV,
     :  MINV
 
*  Arguments Returned:
      BYTE
     :  LOWER,
     :  UPPER
 
      INTEGER
     :  OUTARR( DIM1, DIM2 )
 
*  Local Variables:
      INTEGER
     :  LP                      ! Lower pen (constrained LOWCI)
 
      LOGICAL 
     :  FIRST                   ! First time through loop?

      DOUBLE PRECISION
     :  DEFMAX,                 ! Default value for %PARUPP
     :  DEFMIN,                 ! Default value for %PARLOW
     :  DLOWER,                 ! Lower scaling limit
     :  DSML,                   ! Minimum difference between the limits
     :  DTMAX,                  ! Maximum data value for BYTE
     :  DTMIN,                  ! Minimum data value for BYTE
     :  DUPPER                  ! Upper scaling limit
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEC_R'    ! NUM declarations for functions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions
      INCLUDE 'NUM_DEF_R'    ! NUM definitions for functions
 
*.
 
*    Check inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Constrain the lower pen limit to give at least one positive pen
*    number.
 
      LP = MAX( 0, MIN( HICI - 1, LOWCI ) )
 
*    Set the initial lower and upper limits to ensure the main loop
*    is entered.
 
      DLOWER = NUM_BTOD( MINV )
      DUPPER = DLOWER
 
*    Convert the smallest value to double precision for later testing.
 
      DSML = NUM_BTOD( VAL__SMLB )
 
*    Convert the extreme values to double precision for later testing.
 
      DTMIN = NUM_BTOD( VAL__MINB )
      DTMAX = NUM_BTOD( VAL__MAXB )
 
*    Set defaults depending on required polarity of the scaling.
 
      IF ( POSTIV ) THEN
         DEFMIN = NUM_BTOD( MINV )
         DEFMAX = NUM_BTOD( MAXV )
      ELSE
         DEFMIN = NUM_BTOD( MAXV )
         DEFMAX = NUM_BTOD( MINV )
      END IF
 
*    Start a new error context.
 
      CALL ERR_MARK
 
*    Loop until the values are different or a bad status is encountered.

      FIRST = .TRUE.
      DO WHILE ( ABS( DUPPER - DLOWER ) .LT. DSML .AND.
     :           STATUS .EQ. SAI__OK )
 
*       Cancel the parameters if this is not the first time through the loop.

         IF( .NOT. FIRST ) THEN 
            CALL PAR_CANCL( PARUPP, STATUS )
            CALL PAR_CANCL( PARLOW, STATUS )
         END IF
 
*       Prompt the user for the lower and upper limits to be used in
*       the scaling using the maximum precision.
 
         CALL PAR_GDR0D( PARLOW, DEFMIN, DTMIN, DTMAX, .TRUE., DLOWER,
     :                   STATUS )
         CALL PAR_GDR0D( PARUPP, DEFMAX, DTMIN, DTMAX, .TRUE., DUPPER,
     :                   STATUS )
 
*       Check that there is indeed a range.
 
         IF ( STATUS .EQ. SAI__OK ) THEN
 
            IF ( ABS( DUPPER - DLOWER ) .LT. DSML ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_DSCLB_INVRNG',
     :           'Maximum equals minimum. Try again', STATUS )
 
*             Want to report the error now.
 
               CALL ERR_FLUSH( STATUS )
 
            ELSE
 
*             Convert to the required data type.
 
               LOWER = NUM_DTOB( DLOWER )
               UPPER = NUM_DTOB( DUPPER )
            END IF
         END IF

         FIRST = .FALSE.
 
      END DO
 
*    End the error context.
 
      CALL ERR_RLSE
 
*    Scale the values between the upper and lower pens.
 
      CALL KPG1_ISCLB( BAD, DIM1, DIM2, INARR, INVERT, LOWER, UPPER,
     :                   LP, HICI, BADCI, OUTARR, STATUS )
 
      END
