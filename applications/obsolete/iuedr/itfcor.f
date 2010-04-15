      SUBROUTINE ITFCOR(NSUB, DSUB, QSUB, RSUB, WSUB)

*+
*
*   Name:
*      SUBROUTINE ITFCOR
*
*   Description:
*      Perform ITF correction to image subset.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      If there is any ITF correction data, then this is used to
*      modify the pixel intensities in a position dependent way.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NSUB             ! number of subset pixels

*   Import-Export:
      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

*   External references:
      LOGICAL STR_SIMLR        ! caseless string equality

      REAL*8 LWRITF              ! LWR ITF correction
      REAL*8 SWPITF              ! SWP ITF correction

*   CMHEAD:
      INCLUDE 'CMHEAD'

*   CMEXTP:
      INCLUDE 'CMEXTP'

*   CMITFC:
      INCLUDE 'CMITFC'

*   Local variables:
      INTEGER I                ! loop index
      INTEGER STATUS           ! status

      REAL*8 FN                  ! flux

*   Only do SWP, LORES, GEOM, PHOT
      IF (NOITFC) THEN

         RETURN

      ELSE IF (.NOT.PHOT) THEN

         CALL LINE_WCONT('%p ITF adjustment for RAW not available.\\')

      ELSE IF (.NOT.STR_SIMLR('LORES\\', RESOL)) THEN

         CALL LINE_WCONT('%p HIRES ITF adjustment not available.\\')

      ELSE IF (STR_SIMLR('SWP\\', CAMERA)) THEN

         CALL LINE_WCONT('%p Adjusting Faulty (2nd) LORES SWP ITF.\\')

         DO 50 I = 1, NSUB

            IF (QSUB(I).LT.QMAX) THEN

               FN = SWPITF(WSUB(I), DBLE(DSUB(I)))
               DSUB(I) = MAX( -32766.0d0, MIN(32766.0d0, FN))

            END IF

 50      CONTINUE

      ELSE IF (STR_SIMLR('LWR\\', CAMERA)) THEN

         CALL LINE_WCONT(
     :      '%p Adjusting inaccurate (2nd) LORES LWR ITF.\\' )

         DO 100 I = 1, NSUB

            IF (QSUB(I).LT.QMAX) THEN

               FN = LWRITF(WSUB(I), DBLE(DSUB(I)))
               DSUB(I) = MAX( -32766.0d0, MIN(32766.0d0, FN))

            END IF

 100     CONTINUE

      ELSE

         CALL LINE_WRITS('%p LORES ITF adjustment for %s\\', CAMERA)
         CALL LINE_WCONT(' camera not available.\\')

      END IF

      STATUS = 0
      CALL PRTBUF( STATUS )

      END
