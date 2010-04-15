      SUBROUTINE TSP_CCDPOLMERGE(SIZE,I1,I2,IO,QS,US,QV,UV,STATUS)
*+
*   Subroutine to do the merging of the data
*
*    Sum the intensities for the two datasets, and scale up the
*    Stokes arrays and variances to be correct for the new intensity
*
*    (>) SIZE    (Integer)  The number of spectral points
*    (>) I1      (Real array(SIZE))  The first intensity array
*    (>) I2      (Real array(SIZE))  The second intensity array
*    (!) IO      (Real array(SIZE))  The output intensity array
*    (!) QS      (Real array(SIZE))  The Q stokes parameter array
*    (!) US      (Real array(SIZE))  The U stokes parameter array
*    (!) QV      (Real array(SIZE))  The Q variance array
*    (!) UV      (Real array(SIZE))  The U variance array
*    (!) STATUS  (Integer)  Status value
*
*    Jeremy Bailey   11/8/1990
*
*    Modified:
*     11/20/1991    Handle vad values
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE),IO(SIZE),QS(SIZE),US(SIZE),
     :    QV(SIZE),UV(SIZE)
      INTEGER STATUS

*  Local variables
      REAL QFAC,UFAC,NEWINT,I1S,I2S
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Determine the sum of the intensities
         I1S = 0.0
         I2S = 0.0
         DO I=1,SIZE

*  Only sum channels for which both intensities are good
            IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
                I1S = I1S+I1(I)
                I2S = I2S+I2(I)
            ENDIF
         ENDDO

         NEWINT=I1S+I2S

*  Determine scaling factor for Stokes parameters
         IF (I1S .GT. 0.0) THEN
            QFAC=NEWINT/I2S
         ELSE
            QFAC=0.0
         ENDIF
         IF (I1S .GT. 0.0) THEN
            UFAC=NEWINT/I1S
         ELSE
            UFAC=0.0
         ENDIF
         DO I=1,SIZE

*  Scale up the Stokes parameters (if good data)

            IF (QS(I) .NE. VAL__BADR) QS(I)=QS(I)*QFAC
            IF (US(I) .NE. VAL__BADR) US(I)=US(I)*UFAC

*  Scale up the variances (if good) by the square of the factor

            IF (QV(I) .NE. VAL__BADR) QV(I)=QV(I)*QFAC*QFAC
            IF (UV(I) .NE. VAL__BADR) UV(I)=UV(I)*UFAC*UFAC

*  Add the intensities (set intensity to bad if either input value
*  is bad).

            IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
                IO(I)=I1(I)+I2(I)
            ELSE
                IO(I) = VAL__BADR
            ENDIF
         ENDDO
      ENDIF
      END

