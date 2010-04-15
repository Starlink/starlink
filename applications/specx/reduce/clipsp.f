C  History:
C     19 Sept 2000 (ajc)
C        Declare CLIPVAL (removed from FLAGCOMM)
C-----------------------------------------------------------------------

      SUBROUTINE CLIPSP (IFAIL)

C  Routine to clip high points and set to bad channel value:

      IMPLICIT        NONE

*     Formal parameter(s):
      INTEGER         IFAIL

*     Include files:
      INCLUDE        'STACKCOMM'
      INCLUDE        'FLAGCOMM'

*     Functions:
      INTEGER         NTOT
      LOGICAL         DOQUAD

*     Local variables:
      INTEGER         I
      INTEGER         IOFF
      INTEGER         ISTAT
      INTEGER         NCLIP
      INTEGER         NQ
      REAL            CLIPVAL

*  Ok, go...

      IFAIL = 0

      CALL GEN_GETR4A ('Clip limits? ',
     &                  CLIPLIM, 2, 'E9.2,'','',E9.2', CLIPLIM, ISTAT)
      CALL GEN_GETR4  ('Set to? (value)',
     &                  CLIPVAL, 'E9.2', CLIPVAL, ISTAT)

      IF (CLIPLIM(2).lt.CLIPLIM(1)) THEN
        CALL SWAP2 (CLIPLIM(1), CLIPLIM(2))
      END IF

      DO NQ = 1, NQUAD
       IF (DOQUAD(NQ)) THEN

         NCLIP = 0
         IOFF  = NTOT (NQ-1)

         DO I = 1, NPTS(NQ)
           IF (DATA(IOFF+I).NE.BADPIX_VAL) THEN
             IF (DATA(IOFF+I).lt.CLIPLIM(1)
     &           .or. DATA(IOFF+I).gt.CLIPLIM(2)) THEN
               DATA(IOFF+I) = CLIPVAL
               NCLIP        = NCLIP +1
             END IF
           END IF
         END DO

         IF (NCLIP.ne.0) THEN
           WRITE (6, '('' Sector '',I2,'': # points clipped = '',I5)')
     &            NQ, NCLIP
         END IF

       END IF
      END DO

      RETURN
      END

C-----------------------------------------------------------------------
