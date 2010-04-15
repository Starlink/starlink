      SUBROUTINE SSORT(NN,RA,RB,RC,RD,RE,RF)
C+
C
C Subroutine:
C
C           S S O R T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NN (<), RA (><), RB (><), RC (><), RD (><), RE (><), RF (><)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Sort routine employing indexing routine (sindex).
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER J,NN
      REAL  RA(*),RB(*),RC(*),RD(*),RE(*),RF(*)
      REAL WKSP(MAXPTS)
      INTEGER IWKSP(MAXPTS)
      CALL SINDEX(NN,RA,IWKSP)
      DO J=1,NN
        WKSP(J)=RA(J)
      ENDDO
      DO J=1,NN
        RA(J)=WKSP(IWKSP(J))
      ENDDO
      DO J=1,NN
        WKSP(J)=RB(J)
      ENDDO
      DO J=1,NN
        RB(J)=WKSP(IWKSP(J))
      ENDDO
      DO J=1,NN
        WKSP(J)=RC(J)
      ENDDO
      DO J=1,NN
        RC(J)=WKSP(IWKSP(J))
      ENDDO
      DO J=1,NN
        WKSP(J)=RD(J)
      ENDDO
      DO J=1,NN
        RD(J)=WKSP(IWKSP(J))
      ENDDO
      DO J=1,NN
        WKSP(J)=RE(J)
      ENDDO
      DO J=1,NN
        RE(J)=WKSP(IWKSP(J))
      ENDDO
      DO J=1,NN
        WKSP(J)=RF(J)
      ENDDO
      DO J=1,NN
        RF(J)=WKSP(IWKSP(J))
      ENDDO
      END
