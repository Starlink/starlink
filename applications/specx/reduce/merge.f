C---------------------------------------------------------------------

      SUBROUTINE MERGE (XSCALE, IFAIL)

C  Routine to match up quadrants of correlator data by applying
C  a constant offset to quadrants other than the first..
C  A minimum Chi-square fit is made to the difference of the
C  overlapping spectra, assuming that the offset is constant
C  and that the noise temperature in each quadrant rises as 1/x
C  toward the end of the quadrant.

C  In order to simplify the programming the routine uses the
C  array XSCALE in a non-standard way. Hence the values in this
C  array at the end of the call may not be correct.

C  Modified : 20th April 1986, RP
C             2nd August 1987, RP
C             24th Jan   1992, RP
C             27th Jan   1992, RP
C              6th Jun   2000, AJC
C                Replace 'type *' with 'PRINT *'.
C                Unused i, istat
C                Initialize IQOLD so Linux behaves the same as other platforms
C-

      IMPLICIT  NONE

C  Formal parameters

      REAL     XSCALE(*)
      INTEGER  IFAIL

C  Include files

      INCLUDE 'SPECX_PARS'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

C  Local variables

      INTEGER  J,    K                ! Counting variables
      INTEGER  IF1,  IF2              ! Error returns from XSNART
      INTEGER  IQOLD                  ! Original value of IQCEN
      INTEGER  ISOURCE(NQMAX)         ! Vector to save subscan sort in
      INTEGER  NXSSAV,IRSAV           ! Save for XSCALE parameters
      INTEGER  NQ                     ! Counter for quadrant
      INTEGER  N1,   N2               ! 1st and 2nd chans of XSCALE in overlap
      INTEGER  NST1, NEND1            ! Start and end channels of first sector
      INTEGER  NST2, NEND2            ! Start and end channels of 2nd sector
      INTEGER  NOFF                   ! Offset # chans for second sector
      REAL     CSHIFT                 ! Number of channels to SHIFT subscan by
      REAL     ANCEN                  ! Shift to be applied to 2nd sector
      REAL     A12DIF                 ! Deduced vertical offset between sectors
      REAL     F                      ! Fraction of way across overlap region
      REAL     S                      ! Inverse noise weighting factor for pnt J
      REAL     SN,   SD               ! Numerator and denominator sums for offset
      REAL     SJSQ1, SJSQ2           ! Squared errors on point in Q1, Q2
      REAL     X1,   X2               ! Beg and end freqs of overlap region
      REAL     Y1,   Y2               ! Data values at matching pnts in overlap
      REAL     XJ                     ! Frequency value at point J
      REAL     XMIN                   ! Freq at beginning of overlap
      REAL     RANGE                  ! Freq range of overlap
      REAL     VELSAV                 ! Save value of VELOUT
      LOGICAL  FABSSAV                ! Save value of ABS_FREQ from FLAGCOMM
      LOGICAL  RRELSAV                ! Save value of REST_REL from FLAGCOMM
      LOGICAL  CHANGESAV              ! Save value of CHANGE_FRAME
      CHARACTER XTITSAV*6             ! Save current value of X-axis title
      CHARACTER XNAMSAV*10            ! Save current value of X-axis type
      CHARACTER VREFSAV*4, VDEFSAV*3  ! Save velocity reference and definition

C  Functions:

      INTEGER  NTOT
      REAL     XSNART

C  Find out whether offset between sectors is to be estimated or not

C  This question has been moved to DO_COMMAND.F
C      CALL GEN_YESNO ('Are sectors to be offset?', SECTOR_OFFSET,
C     &                 SECTOR_OFFSET, ISTAT)

C  First check that data is from correlator or similar instrument

      IF(NQUAD.EQ.1) THEN
        IFAIL=31
        RETURN
      ELSE
        DO NQ=2,NQUAD
          IF(ABS(JFINC(NQ)).NE.ABS(JFINC(1)))    THEN
            IFAIL=32
            RETURN
          END IF
        END DO
      END IF

C  Initialize any variables

      CSHIFT = 0.0

C  Save data and frequency related flags/variables before
C  changing the latter temporarily. (Will be restored before
C  returning to main program)

      CALL PUSH

      IFAIL     = 0

      NXSSAV    = NXS
      IRSAV     = IREST
      FABSSAV   = ABS_FREQ
      RRELSAV   = REST_REL
      XTITSAV   = XAXIS_UNITS
      XNAMSAV   = XAXIS_NAME
      VREFSAV   = VEL_REF
      VDEFSAV   = VEL_DEF
      VELSAV    = VELOUT
      CHANGESAV = CHANGE_FRAME

C  Now force SETX to give RELATIVE OBSERVED frequencies in XSCALE

      NXS         = 2
      IREST       = 0
      ABS_FREQ    = .FALSE.
      REST_REL    = .TRUE.
      XAXIS_UNITS = 'MHz'
      VEL_REF     = 'TELL'
      VEL_DEF     = 'RAD'
      VELOUT      =  0.0
      CHANGE_FRAME= .TRUE.

C  Get data arranged so that XSCALE(I) increases within quadrants and
C  by quadrants
C  ..First, by quadrants:

C     Sort into increasing frequency order, then
C     use this information to do a fast sort of other arrays

      CALL SPECX_JSORT   (NQUAD, JFCEN, ISOURCE)
      CALL SPECX_REORDER (NQUAD, ISOURCE)

C  ..Then within the quadrant if now necessary

      DO NQ = 1, NQUAD
        IF (JFINC(NQ).LT.0) THEN
          CALL INVERT (NQ)
        END IF
      END DO

C  Now set up new XSCALE to reflect re-ordering

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0)  GO TO 100

      PRINT *,'Frequency coverage of re-ordered original spectrum'
      PRINT *
      CALL DISPLX (XSCALE,IFAIL)
      PRINT *
      PRINT *,'(centre quadrant is',IQCEN,' )'

C  If IQCEN is set to zero (as it is for GSD data) then we don't have
C  a reference channel for the LSR velocity. Use the observing defaults
C  for now - fix up later

      IF (IQCEN.EQ.0) THEN
        IQOLD = IQCEN
        IQCEN = NQUAD/2
      END IF

C  Locate 'centre' channel before start of iteration

      ANCEN = FLOAT(NTOT(IQCEN-1))+0.5*(NPTS(IQCEN)+1)  ! channel to which VLSR refers
CD    PRINT *, 'Channel at original centre is ', ANCEN

C  Set up iteration by quadrant boundary

      NST2  = 1
      NEND2 = NPTS(1)

      DO NQ = 1, NQUAD-1
        NST1  = NST2             ! Start and end channels of first quadrant
        NEND1 = NEND2
        NST2  = NTOT(NQ)+1       ! Start and end channels of second quadrant
        NEND2 = NTOT(NQ+1)

C  Identify points involved in overlap

CD      PRINT *, 'NQ, NST1, NEND1, NST2, NEND2 ',
CD   &           NQ, NST1, NEND1, NST2, NEND2

        X1 = XSCALE(NST2)
        X1 = XSNART (XSCALE(NST1),X1,NPTS(NQ),IF1)    ! X1 is fractional points
        X2 = XSCALE(NEND1)                            ! corresponding to XSCALE(NST@)
        X2 = XSNART (XSCALE(NST2),X2,NPTS(NQ+1),IF2)

CD      PRINT *, 'X1, X2 = ', X1, X2

        IF (IF1+IF2.NE.0)   THEN
          IFAIL = 33
          GO TO 100                ! Tidy up and return with IFAIL set
        END IF

C       If overlap is less than 0.1 channels then snap in "wrong" direction,
C       otherwise discard an additional channel.

        N1 = NST1 + IFIX (X1+0.1) -1 ! First channel in XSCALE involved in overlap
        N2 = NST2 + IFIX (X2+0.1)    ! Last channel "

CD      PRINT *,'Overlap region channels ',N1,N2

C       Interpolate second quadrant onto sampling of first quadrant
C       using linear interpolation --- F is fraction channels to offset
C       Beware that "snap" may have pulled spectrum over by 1 channel

*       F =  X1 - IFIX(X1)                 ! Original
*       F = (X1+0.1) - IFIX(X1+0.1)        ! Even worse?
        F =  X1 - FLOAT(N1+1-NST1)         ! next try

CD      PRINT *, 'Fractional channel error = ', F

        DO J = 1, NPTS(NQ+1)-1
          K         = NEND2+1-J
          XSCALE(K) = (1.-F)*XSCALE(K) + F*XSCALE(K-1)
          IF (DATA(K).EQ.BADPIX_VAL .AND. DATA(K-1).EQ.BADPIX_VAL) THEN
            DATA(K) = BADPIX_VAL
          ELSE IF (DATA(K-1).EQ.BADPIX_VAL) THEN
            DATA(K) = DATA(K)
          ELSE IF (DATA(K).EQ.BADPIX_VAL) THEN
            DATA(K) = DATA(K-1)
          ELSE
            DATA(K) = (1.-F)*DATA(K) + F*DATA(K-1)
          END IF
        END DO

C  For the centre quadrant only, reflect shift of data relative
C  to sampling.

        IF (NQ.EQ.IQCEN-1) THEN
          CSHIFT = F
CD        PRINT *,'NQ, new CSHIFT',NQ,CSHIFT
        END IF

C  Now find the amount by which to shift the second quadrant. Assume a simple
C  offset is all that is required. Fit the existing offset using Chi-squared
C  statistic, with parabolic weighting across overlap region (i.e., max in
C  centre and zero at ends).
C  We could of course use the actual noise on the spectrum if we knew it!

C  Find total range of overlap

        XMIN  = XSCALE(NST2+1)
        RANGE = XSCALE(NEND1)-XMIN

C  In the case where sectors may be offset from each other, estimate
C  offset, and shift spectra to suit.

        IF (SECTOR_OFFSET) THEN

C  Initialize sums.......

          SN = 0.0
          SD = 0.0

C  ....and accumulate into SN and SD

          DO J = N1+1,NEND1-1
            IF (DATA(J).NE.BADPIX_VAL .AND.
     &          DATA(J+NST2+1-N1).NE.BADPIX_VAL) THEN
              Y1    = DATA(J)
              Y2    = DATA(J+NST2+1-N1)
              XJ    = XSCALE(J)
              F     = (XJ-XMIN)/RANGE
              SJSQ1 = (1.-F)**2
              SJSQ2 = F**2
              S     = SJSQ1 + SJSQ2
              SD    = SD+S
              SN    = SN+S*(Y1-Y2)
            END IF
          END DO

C  Calculate offset...

          IF (SD.NE.0.0) THEN
            A12DIF = SN/SD
          ELSE
            PRINT *, '*** Warning, error during MERGE -- not done ***'
            IFAIL = 35
            GO TO 100
          END IF

C  ...and add to second quadrant data

          DO J = NST2, NEND2
            IF (DATA(J).NE.BADPIX_VAL) DATA(J) = DATA(J) + A12DIF
          END DO

        END IF

C  Average data over region of overlap

        DO J = N1+1, NEND1
          XJ      = XSCALE(J)
          F       = (XJ-XMIN)/RANGE
          SJSQ1   = (1.-F)**2
          SJSQ2   = F**2
          IF (DATA(J).NE.BADPIX_VAL .AND.
     &        DATA(J+NST2+1-N1).NE.BADPIX_VAL) THEN
            DATA(J) = (DATA(J)*SJSQ1 + DATA(J+NST2-N1)*SJSQ2)
     &                   / (SJSQ1+SJSQ2)
            DATA(J+NST2-N1) = DATA(J)
          END IF
        END DO

C  Finally condense into a single spectrum/quadrant with new boundaries

        NOFF = (NEND1+1) - N1
        DO J = NST2+NOFF, NEND2
          K = J - (NST2-N1)
          DATA(K)   = DATA(J)
          XSCALE(K) = XSCALE(J)
        END DO

CD      PRINT *,'NQ,NOFF',nq,noff

        IF (NQ.EQ.IQCEN-1) THEN
          CSHIFT = CSHIFT + FLOAT(NST2-N1)
CD        PRINT *,'Total value of CSHIFT',CSHIFT
        END IF

C  Update boundaries of "new" quadrant (actually just RH edge of total now)

        NST2  = N1
        NEND2 = N1 + NPTS(NQ+1) - 1

      END DO

C  Before return tidy up header to give data for single quadrant.

      NPTS(1) = NEND2
      CSHIFT   = 0.5*(NPTS(1)+1) - (ANCEN-CSHIFT)

CD    PRINT *,'New mid-channel is ',0.5*(NPTS(1)+1)
CD    PRINT *,'CSHIFT',CSHIFT
CD    PRINT *,'ANCEN',ANCEN
CD    PRINT *,'Centre frequency needs shifting by ',CSHIFT,' channels'

      CALL LSRCOR (LSRFLG, VSL, VES, VTE, VLSR,
     &             IDATE, ITIME,IUTFLG, RA, DEC,
     &             JFREST(IQCEN), JFCEN(IQCEN), LOFREQ(IQCEN),
     &             IFFREQ(IQCEN), CSHIFT,       JFINC(IQCEN))

      JFREST(1) = JFREST(IQCEN)
      JFCEN(1)  = JFCEN(IQCEN)
      IFFREQ(1) = IFFREQ(IQCEN)
      LOFREQ(1) = LOFREQ(IQCEN)
      ITREC(1)  = ITREC(IQCEN)
      ITSKY(1)  = ITSKY(IQCEN)
      ITTEL(1)  = ITTEL(IQCEN)

      IF (IQOLD.EQ.0) THEN
        IQCEN = 0
      ELSE
      IQCEN = 1
      END IF

      NQUAD = 1

      PRINT *,'Frequency coverage of merged spectrum'
      PRINT *

      CALL DISPLX (XSCALE,IFAIL)

C  Restore common block FREQ

  100 CONTINUE

      NXS          = NXSSAV
      IREST        = IRSAV
      ABS_FREQ     = FABSSAV
      REST_REL     = RRELSAV
      XAXIS_UNITS  = XTITSAV
      XAXIS_NAME   = XNAMSAV
      CHANGE_FRAME = CHANGESAV
      VEL_REF      = VREFSAV
      VEL_DEF      = VDEFSAV
      VELOUT       = VELSAV

      IF (IFAIL.EQ.0) CALL XY

      CALL POP

CD    PRINT *, '-- merge --'
CD    PRINT *, '   frequency scale code = ', nxs
CD    PRINT *, '   xaxis_name  = ', xaxis_name
CD    PRINT *, '   xaxis_units = ', xaxis_units

      RETURN
      END

*------------------------------------------------------------------------
