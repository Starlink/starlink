*+  USI_RANGES - Allows range selection on each dimension of the data array
	SUBROUTINE USI_RANGES  (NDIM,LABEL,START,STOP,WIDTH,
     :                            APP_DIM,AMIN,AMAX,STATUS)
*    Description :
*           This routine allows the user to select a range of values for
*        each dimension of the data array. A selection can also be made on
*        which dimension his application will operate on.
*    Method :
*        In the event of an axis dimension decreasing in value with bin number,
*        e.g. with the X co-ordinate of images, the routine will reset
*        the values of width,start and stop for that axis such that the axis
*        values will appear to increase from left to right. This could be
*        a problem so use this routine with caution.
*    Deficiencies :
*        This routine should use the BDA_ routines to find out things like
*        the axis label & start & end values directly, rather than require
*        them as input arguments as at present. However it is written like
*        this for modularity/atomicity reasons!
*    Author:
*        R.D.Saxton
*    History :
*     5-May 1988 original (LTVAD::RDS)
*    Parameters :
*      APP_DIM         _INTEGER        Axis dimension to run application on.
*      LOWER           _REAL           Used to obtain the lower edge of the
*                                      range for each axis.
*      UPPER           _REAL           Used to obtain the upper edge of the
*                                      range for each axis.
*    Type Definitions :
      IMPLICIT NONE
*    Global :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER       NDIM                     ! Number of dimensions of data array

      CHARACTER*(*) LABEL(NDIM)              ! Label for each axis

*    Import-Export :
      REAL          START(NDIM), STOP(NDIM)  ! Beginning and end values of each axis
      REAL          WIDTH(NDIM)              ! Width of data bin in each dimension
                                             ! NB: These may be altered by the routine
                                             ! if an axis is reversed e.g. X co-ord's
*    Export :
      INTEGER       APP_DIM                  ! Axis for the application to operate on
      INTEGER       AMIN(NDIM), AMAX(NDIM)   ! Selected extremes for each axis.

*    Local variables :
      INTEGER       LP                       ! Loop variable

      REAL          RMIN, RMAX               ! Lower and upper input values.
      REAL          SAVE                     ! Temp variable to hold START value

      LOGICAL       INPUT                    ! Controls input loops
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Display list of axes
      DO LP = 1, NDIM
        CALL MSG_SETI ('INC', LP)
        CALL MSG_SETC ('LABEL', LABEL(LP))
        CALL MSG_OUT (' ', '^INC.  ^LABEL', STATUS)
      END DO

*    Ask for the axis upon which the application is going to work.
      CALL USI_GET0I('APP_DIM',APP_DIM,STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Get ranges for each axis except this one.
      DO LP = 1, NDIM

*      Pretend axes increasing from right to left actually increase the
*      conventional way.
        IF (WIDTH(LP) .LT. 0.0) THEN
          WIDTH (LP) = -WIDTH (LP)
          SAVE       =  START (LP)
          START (LP) =  STOP  (LP)
          STOP  (LP) =  SAVE

        END IF

        IF (LP .EQ. APP_DIM) THEN
*        Take full range
          AMIN (LP) = 1
          AMAX(LP)  = NINT((STOP(LP) - START(LP)) / WIDTH(LP)) + 1

        ELSE
          CALL MSG_SETI ('INC', LP)
          CALL MSG_SETC ('LABEL', LABEL(LP))
          CALL MSG_OUT (' ', '^INC.  ^LABEL', STATUS)

*  Use the parameters LOWER and UPPER to get range values for ALL axes.
*  Would do it differently if ADAM allowed parameter arrays.
          INPUT = .TRUE.

          DO WHILE (INPUT)
            CALL USI_DEF0R ('LOWER', START(LP), STATUS)
            CALL USI_GET0R ('LOWER', RMIN,      STATUS)
            CALL USI_CANCL ('LOWER',            STATUS)

*          Check status
            IF (STATUS .NE. SAI__OK) GOTO 999

            IF (RMIN .LT. START(LP) .OR. RMIN .GT. STOP(LP)) THEN
              CALL MSG_SETR ('START', START(LP))
              CALL MSG_SETR ('STOP',  STOP(LP))
              CALL MSG_OUT (' ', 'ERROR: Value must be between ^START '
     :                                            //'and ^STOP', STATUS)

            ELSE
              INPUT = .FALSE.

            END IF
          END DO

          INPUT = .TRUE.

          DO WHILE (INPUT)
            CALL USI_DEF0R ('UPPER', STOP(LP), STATUS)
            CALL USI_GET0R ('UPPER', RMAX,     STATUS)
            CALL USI_CANCL ('UPPER',           STATUS)

*          Check status
            IF (STATUS .NE. SAI__OK) GOTO 999

            IF (RMAX .LT. START(LP) .OR. RMAX .GT. STOP(LP)) THEN
              CALL MSG_SETR ('START', START(LP))
              CALL MSG_SETR ('STOP',  STOP(LP))
              CALL MSG_OUT (' ', 'ERROR: Value must be between ^START '
     :                                            //'and ^STOP', STATUS)
*
            ELSE
              INPUT = .FALSE.
*
            END IF
          END DO

*          Calculate the bin positions of these lower and upper values
          AMIN(LP) = NINT((RMIN - START(LP)) / WIDTH(LP)) + 1
          AMAX(LP) = NINT((RMAX - START(LP)) / WIDTH(LP)) + 1

        END IF
      END DO

999   IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP ('E', '...from USI_RANGES', STATUS)

      END IF
      END
