*+  FIT_REBMOD1 - Rebins 1D fit model
      SUBROUTINE FIT_REBMOD1 (INBIN,IVAL,IBOUND,REG,ONBIN,OLBOUND,
     :OUBOUND,OVAL,STATUS)
*    Description
*     Model with initial values IVAL in contiguous bins defined by bounds
*     IBOUND, is rebinned into output bins bounded by OLBOUND & OUBOUND.
*     This is necessary when interpolating over a grid of stored models, which
*     will not in general have been precalculated for the bins required by the
*     fitting program (to match data or instrument response bins).
*     No renormalisation is applied.
*     If REG is set true then the o/p bins are regularly spaced with spacing
*     of OUBOUND(1)-OLBOUND(1). In this case these arrays need contain only
*     one element each.
*     If the output range is not fully covered by the input data, then some
*     zero (or spuriously small) values will result at the edges of the output
*     array. STATUS is set bad and an error message queued to warn of this.
*     STATUS values used are:
*                STATUS=USER__001   Output not covered at bottom
*                STATUS=USER__002   Output not covered at top
*                STATUS=USER__003   Output not covered at either end
*    Method :
*     The contents of the input bins are assumed to be uniformly distributed
*     across each bin, and are partitioned accordingly into the output bins.
*     If the output array is regular, then time will be saved by setting REG
*     true, even if the full OBOUND array is available.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*     30 Sep 88: Original adapted from REB_SIMPLE1 (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'USER_ERR'
*    Global variables :
*    Import :
        INTEGER INBIN          ! number of donor bins
        REAL IVAL(INBIN)       ! values in donor array
        REAL IBOUND(INBIN+1)   ! donor bin boundaries
        LOGICAL REG            ! receptor bins to be regularly spaced
        INTEGER ONBIN          ! number of receptor bins
        REAL OLBOUND(*)        ! lower boundaries of receptor bins
        REAL OUBOUND(*)        ! upper boundaries of receptor bins
*    Import-Export :
*    Export :
        REAL OVAL(ONBIN)      ! values of receptor bins
*    Status :
        INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
        REAL OSIZE             ! (regular) bin size
        REAL DPOS              ! current donor bin position
        REAL IBOT              ! donor bin lower boundary
        REAL ITOP              ! donor bin upper boundary
        REAL ISIZE             ! donor bin size
        REAL FRAC              ! fractional part of donor bin
        REAL OBOT              ! receptor lower boundary
        REAL OTOP              ! receptor upper boundary
        REAL OMAX              ! upper boundary of receptor range

        INTEGER OIXL           ! lower receptor bin index
        INTEGER OIXH           ! upper receptor bin index
        INTEGER I,J            ! loop indices
*    Local data :
*-----------------------------------------------------------------

* Status check
        IF(STATUS.NE.SAI__OK) RETURN

* Initialisation
        DO I=1,ONBIN
           OVAL(I)=0.0
        ENDDO
        OIXH=1
        OIXL=1

* Loop through donor bins
        DO I=1,INBIN

*    Donor bin parameters
           IBOT=IBOUND(I)
           ITOP=IBOUND(I+1)
           ISIZE=ITOP-IBOT

*    Calculate range of receptor bin indices for this donor
           IF(REG) THEN

*      Regularly spaced o/p bins
              OSIZE=OUBOUND(1)-OLBOUND(1)
              OIXL=INT((IBOT-OLBOUND(1))/OSIZE)+1
              OIXH=INT((ITOP-OLBOUND(1))/OSIZE)+1
              IF(AMOD(ITOP-OLBOUND(1),OSIZE).EQ.0.) OIXH=OIXH-1
              OMAX=OLBOUND(1)+ONBIN*OSIZE

*      Ensure that range is within allowed bounds
              OIXL=MIN(OIXL,ONBIN)
              OIXL=MAX(OIXL,1)
              OIXH=MIN(OIXH,ONBIN)
              OIXH=MAX(OIXH,1)

*    Irregular output bins

           ELSE

              DO J=OIXH,ONBIN
                 IF(IBOT.GE.OLBOUND(J).AND.IBOT.LT.OUBOUND(J)) OIXL=J
                 IF(ITOP.GT.OLBOUND(J).AND.ITOP.LE.OUBOUND(J))THEN
                    OIXH=J
                    GOTO 50
                 ENDIF
              ENDDO
50            CONTINUE
              OMAX=OUBOUND(ONBIN)
           ENDIF

*   Loop through receptor bins overlapping this donor

           DO J=OIXL,OIXH
              OBOT=OLBOUND(J)
              OTOP=OUBOUND(J)

*    See if any of donor bin is covered (receptor bin could lie
*                                        outside donor range)
              IF(OTOP.GT.IBOT.AND.OBOT.LT.ITOP) THEN

*      Calculate fraction of donor bin covered by receptor
                 IF(OBOT.LE.IBOT) THEN
                    IF(OTOP.GE.ITOP) THEN
                       FRAC=1
                    ELSE
                       FRAC=(OTOP-IBOT)/ISIZE
                    ENDIF
                 ELSE
                    IF(OTOP.LT.ITOP) THEN
                       FRAC=(OTOP-OBOT)/ISIZE
                    ELSE
                       FRAC=(ITOP-OBOT)/ISIZE
                    ENDIF
                 ENDIF

*     Update receptor value
                 OVAL(J)=OVAL(J)+FRAC*IVAL(I)
              ENDIF
           ENDDO
        ENDDO


* Set status bad if output array range is not fully covered by input
        IF(OLBOUND(1).LT.IBOUND(1)) THEN
           IF(OMAX.LE.IBOUND(INBIN+1)) THEN
              CALL ERR_REP('BAD_BOT','Output range not covered at '//
     :        'bottom',STATUS)
              STATUS=USER__001
           ELSE
              CALL ERR_REP('BAD_BOTH','Output range no covered at '//
     :        'bottom or top',STATUS)
              STATUS=USER__003
           ENDIF
        ELSE IF(OMAX.GT.IBOUND(INBIN+1)) THEN
           CALL ERR_REP('BAD_TOP','Output range not covered at top'
     :     ,STATUS)
           STATUS=USER__002
        ENDIF

* Exit
        IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from FIT_REBMOD1'
     :  ,STATUS)

        END
