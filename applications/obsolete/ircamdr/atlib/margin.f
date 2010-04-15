
      SUBROUTINE MARGIN(MLOW,MHIGH,ARRAY,NX,NY,SMARGE,NS,IFLAG)
*+
*   MARGIN
*
*   calculates marginal distribution
*
*   Given      (arguments)
*   MLOW    I   lower boundary of marginal calculation
*   MHIGH   I   upper boundary of marginal calculation
*   ARRAY   RA  2-D image array
*   NX      I   X-dimension of image array
*   NY      I   Y-dimension of image array
*   NS      I   dimension of array to hold marginal
*   IFLAG   I   = 0 for marginal sums along X-direction
*               = 1 for marginal sums along Y-direction
*
*
*   Returned   (arguments)
*   SMARGE  RA  marginal distribution
*
*   B.D.Kelly/ROE/1981
*-

      REAL ARRAY(NX,NY),SMARGE(NS)

      IF(IFLAG.EQ.0) THEN
        DO I=1,NX
          SMARGE(I)=0.0
          DO J=MLOW,MHIGH
            SMARGE(I)=SMARGE(I)+ARRAY(I,J)
          ENDDO
        ENDDO
      ELSE
        DO J=1,NY
          SMARGE(J)=0.0
          DO I=MLOW,MHIGH
            SMARGE(J)=SMARGE(J)+ARRAY(I,J)
          ENDDO
        ENDDO
      ENDIF

      END
