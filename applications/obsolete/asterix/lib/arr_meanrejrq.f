*+  ARR_MEANREJRQ - Finds mean and standard deviation of array
      SUBROUTINE ARR_MEANREJRQ( N, ARRAY, QUAL, MEDIAN, SDEV,
     :                                              STATUS )
*
*    History :
*     30 Aug 89 : ( BHVAD :: DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER               STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER               N
      REAL                  ARRAY(N)
      LOGICAL               QUAL(N)
*
*    Export :
*
      REAL                  MEDIAN, SDEV
*
*    Local constants :
*
      INTEGER               MAXNBIN
         PARAMETER          (MAXNBIN=60)
*
*    Local variables :
*
      REAL MINV, BINWID, MAXV, ARR95, MEAN

      INTEGER               NOC(MAXNBIN), COUNT, NGOOD, MAXOC, NIP
      INTEGER               I, IBIN, NBIN, N95, MEDPIX, IP(MAXNBIN)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find simple mean
      MEAN = 0.0
      MINV = 1.0E28
      MAXV = -1.0E28
      NGOOD = 0
      DO I = 1,N
         IF ( QUAL(I) ) THEN
            MEAN = MEAN + ARRAY(I)
            MINV = MIN( MINV, ARRAY(I) )
            MAXV = MAX( MAXV, ARRAY(I) )
            NGOOD = NGOOD + 1
         END IF
      END DO
      MEAN = MEAN / REAL(NGOOD)

*    Check for pathalogical case
      IF ( ABS(MEAN) .LT. 1.0E-6 ) THEN
        MEDIAN = 0.0
        SDEV = 1.0
        GOTO 99
      END IF

*    Choose number of bins
      NBIN = MIN( MAXNBIN, INT(MEAN*N)/5 )
      IF ( NBIN .EQ. 0 ) THEN
        IF ( ( MAXV - MINV ) .GE. 1.0 ) THEN
          NBIN = NINT(MAXV-MINV) + 1
        ELSE
          MEDIAN = 0.0
          SDEV = 1.0
          GOTO 99
        END IF
      END IF

*    Decide bin width
      BINWID = ( MIN( MAXV,3.0*( MEAN - MINV ))-MINV ) / REAL(NBIN)

*    Initialise bins
      DO I = 1,NBIN
        NOC(I) = 0
      END DO

*    Do histogram of all data
      MAXV = MINV + REAL( NBIN - 1 ) * BINWID
      DO I =1,N
        IF ( ( ARRAY(I).LT. MAXV ) .AND. QUAL(I) ) THEN
          IBIN = INT( ( ARRAY(I) - MINV ) / BINWID ) + 1
          NOC(IBIN) = NOC(IBIN) + 1
        END IF
      END DO

*    This may be sparse so squash it down
      NIP = 0
      DO I =1,NBIN
        IF ( NOC(I) .NE. 0 ) THEN
          NIP = NIP + 1
          NOC(NIP) = NOC(I)
          IP(NIP) = I
        END IF
      END DO

*    Find the peak
      MAXOC = -1
      DO I = 1, NIP
        IF ( NOC(I) .GT. MAXOC ) THEN
          MEDPIX = IP(I)
          MAXOC = NOC(I)
        END IF
      END DO

*    The median data value is
      MEDIAN = MINV + REAL(MEDPIX)* BINWID

*    Search histogram for 95% point, and find median
      N95 = INT(REAL(N)*0.95)
      COUNT = 0
      DO I = 1, NIP
        IF ( COUNT + NOC(I) .LT. N95 ) THEN
          COUNT = COUNT + NOC(I)
        ELSE
          IF ( I .EQ. 1 ) THEN
            ARR95 = MINV + REAL(IP(I)) * BINWID
          ELSE
            ARR95 = MINV + (REAL(IP(I-1))-0.5)*BINWID *
     :             REAL(N95 - COUNT) / REAL( NOC(I) ) /
     :                              REAL(IP(I)-IP(I-1))
          END IF
          GOTO 69
        END IF
      END DO
 69   CONTINUE

*    Find std devitaion wrt ARR95
      SDEV = 0.0
      NGOOD = 0
      DO I = 1, N
        IF ( ( ARRAY(I) .LE. ARR95 ) .AND. QUAL(I) ) THEN
          SDEV = SDEV + ( ARRAY(I) - ARR95 ) ** 2.0
          NGOOD = NGOOD + 1
        END IF
      END DO

      SDEV = SQRT( SDEV / NGOOD )

 99   CONTINUE

      END
