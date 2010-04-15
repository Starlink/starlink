*-----------------------------------------------------------------------
*   History:
*      12/29/95  Created (Remo Tilanus, JACH, Hawaii)
*      8/05/00  (ajc):
*         Port to Linux
*         Replace 'TYPE *' with 'PRINT *'
*         Remove inaccessible error handling
*
*
*-----------------------------------------------------------------------

      SUBROUTINE BAD2BLANK (ITYPE, DATA, NELM, RBLANK, NBAD, IFAIL)

*  This routine remedies the omission of support for an undefined value
*  in the FIGARO FIT routines.
*  It takes the simple-minded approach to change undefined values
*  (badpix_val) to min(array values) - 1 and should be run before
*  determining the FITS scaling factors with FIT_SCALC.
*  The FITS BLANK keyword can be calculated as
*
*     BLANK = ( RBLANK - ZEROS(N) ) / SCALES(N) + .5
*
*  with SCALES and ZEROS as returned by FIT_SCALC.
*

      IMPLICIT  NONE

*     formal parameters:

      INTEGER   ITYPE       !  1: spectra, 2: map, 3: cube
      INTEGER   NELM        !  Number elements in DATA
      REAL      DATA(NELM)  !  Data array
      REAL      RBLANK      !  RETURNED: new value of bad pixels.
      INTEGER   NBAD        !  RETURNED: Number of BAD pixels
      INTEGER   IFAIL       !  Error status, returned

*     include files
      INCLUDE  'FLAGCOMM'   !  Badpix_value

*     Local and intermediate variables

      INTEGER   I
      REAL      DMIN, DMAX  ! Min, Max value in DATA
      REAL      DLIM        ! Flag zero for MAPs and CUBEs

*  Ok, go...

      IFAIL = 0

*     Find range of data array
*     MAP, CUBE: Flag zeroes as well which is a bit danegerous.
*     Maps should really be created with BADPIX_VAL in empty spots.

      DMIN=DATA(1)
      DMAX=DMIN
      NBAD = 0
      DLIM = -1.E+06                             ! All data
      IF (ITYPE .NE. 1)  DLIM = ABS(1./DLIM)     ! Only < -1.E-06 and > 1.E-06

      DO I=1,NELM
         IF ( (ABS(DATA(I)-BADPIX_VAL) .GE. 0.0001) .AND.
     &        (ABS(DATA(I)) .GE. DLIM)) THEN
            IF (DATA(I).GT.DMAX) DMAX=DATA(I)
            IF (DATA(I).LT.DMIN) DMIN=DATA(I)
         ELSE
            NBAD = NBAD + 1
         END IF
      END DO

      RBLANK = DMIN - 1.0   ! If the range is so wide that the scaling
                            ! maps RBLANK on valid pixels the data is
                            ! no good anyway.

CD     PRINT *, 'Min map: ', DMIN, '   Max map: ', DMAX
CD     PRINT *, 'Rblank: ', RBLANK

      IF (NBAD .NE. 0) THEN
         DO I=1,NELM
         IF ( (ABS(DATA(I)-BADPIX_VAL) .LT. 0.0001) .OR.
     &        (ABS(DATA(I)) .LT. DLIM))  DATA(I) = RBLANK
         END DO
         PRINT *, 'Flagged ',NBAD,' undefined pixels'
      END IF



      RETURN

      END

*-----------------------------------------------------------------------
