

      SUBROUTINE PERIOD_READANTARES(IPARRAY, NUMROWS, MXCOL, HJD,
     :                              VEL, SIGVEL, IUNIT)

C=============================================================================
C Routine to input data into the PERIOD program. The data must be read from
C an ASCII file. Input of Y axis errors is optional.
C
C Written by Vikram Singh Dhillon @Sussex 31-May-1991.
C
C Unused parameter MXVEC removed - GJP June 1995
C
C Removed upper case conversion on file names - GJP October 1995
C
C GJP March 1997
C
C Removed variable NVEC
C Added some variable initialisation
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER NUMROWS, MXCOL
      DOUBLE PRECISION IPARRAY(NUMROWS,MXCOL)

C-----------------------------------------------------------------------------
C PERIOD_INPUT declarations.
C-----------------------------------------------------------------------------

      DOUBLE PRECISION HJD(NUMROWS)
      DOUBLE PRECISION VEL(NUMROWS), SIGVEL(NUMROWS)
      DOUBLE PRECISION DUMMY
      INTEGER IUNIT, IFAIL
      CHARACTER*1 FLAG*6, STRING


C-----------------------------------------------------------------------------
C Read Antares file into data area
C-----------------------------------------------------------------------------

      NUMROWS = 0
      IFAIL = 0

      DO WHILE ( IFAIL.EQ.0 )
         READ (IUNIT, '(A)', IOSTAT=IFAIL) FLAG
         IF ( FLAG.EQ.'0FRAME' ) THEN
            NUMROWS = NUMROWS + 1
            READ (IUNIT, *) DUMMY, DUMMY, HJD(NUMROWS),
     :                      DUMMY, DUMMY, DUMMY
            READ (IUNIT, '(A)') STRING
            READ (IUNIT, '(A)') STRING
            READ (IUNIT, '(A)') STRING
            READ (IUNIT, *) DUMMY, DUMMY, DUMMY,
     :                      VEL(NUMROWS), DUMMY, DUMMY,
     :                      DUMMY, DUMMY, DUMMY, DUMMY
            READ (IUNIT, *) DUMMY, DUMMY, SIGVEL(NUMROWS),
     :                      DUMMY, DUMMY, DUMMY, DUMMY,
     :                      DUMMY, DUMMY
            IPARRAY(NUMROWS, 1) = HJD(NUMROWS) - DINT(HJD(1))
            IPARRAY(NUMROWS, 2) = VEL(NUMROWS)
            IPARRAY(NUMROWS, 3) = SIGVEL(NUMROWS)
         END IF
      END DO

      RETURN
      END
