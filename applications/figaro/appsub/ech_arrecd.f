C+
      SUBROUTINE ECH_ARRECD(IREC,CHANS,WAVES,WEIGHTS,CLASS,ORDER,
     :                           NLMAX,NLID,M1,M2,NX,DRMS,NCOEFF)
C
C     E C H _ A R R E C D
C
C     Performs a fit to the identified lines and records the results
C     on logical unit number IREC.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) IREC     (Integer)  The logical unit number for output.
C     (>) CHANS    (Real array CHANS(NLMAX)) The centers of the
C                  identified lines, in pixel numbers.
C     (>) WAVES    (Real array WAVES(NLMAX)) The wavelengths of the
C                  identified lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (>) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (>) ORDER    (Integer array ORDER(NLXMAX)) The order number for
C                  each of the fitted lines.
C     (>) NLMAX    (Integer) The maximum number of lines possible; i.e.,
C                  the dimension of CHANS,WAVES,WEIGHTS, and CLASS.
C     (>) NLID     (Integer) The number of identified lines.
C     (>) M1       (Integer) The first order number involved.
C     (>) M2       (Integer) The last order number involved.
C     (>) NX       (Integer) The number of pixels in the arc data.
C     (>) DRMS     (Logical) True if the "RMS if omitted" statistic
C                  is to be output for each line.
C     (>) NCOEFF   (Integer) The number of parameters to be used for
C                  the fit to each order.
C
C     ***  Originally ARFIT (output to terminal)  KS / CIT 14th June 1983
C
C     ARFIT Modified:
C
C     KS / AAO  4th Sept 1985 Option to repeat with a different NCOEFF
C               removed.  Now incorporated in main routine.  WEIGHTS
C               parameter added, and 'fit without this line' figure
C               added. DRMS parameter added. CLASS parameter added.
C     KS / AAO  30th June 1986. No longer modifies NCOEFF if not enough
C               lines have been selected.
C
C     Stolen & Modified ---> ECH_ARRECD         JKM / ESO 25. Nov. 1987.
C
C     WFL / AAO 29th May 1988. Prevent fall-over if too few lines have
C               been identified in a given order.
C     WFL / AAO 30th May 1988. Sort prior to fit.
C     WFL / AAO 31st May 1988. Attempt to avoid crashes with too few lines.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL DRMS
      INTEGER IREC,NLMAX,NLID,M1,M2,NX,NCOEFF
      INTEGER CLASS(NLMAX),ORDER(NLMAX)
      REAL CHANS(NLMAX),WAVES(NLMAX),WEIGHTS(NLMAX)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables for a single order
C
      INTEGER NLOMX
      PARAMETER (NLOMX=100)
      INTEGER NEWCL(NLOMX),NEWNL
      REAL NEWCH(NLOMX),NEWWV(NLOMX),NEWWT(NLOMX)
C
C     Local variables
C
      LOGICAL WARNING
      INTEGER NC
      PARAMETER (NC=11)
      INTEGER M,DM,I,INET,ILAST,IGNORE,NC1,NC4
      INTEGER J,NEWNCF
      REAL ADISP,END,RMS,RMSX,ST,VALUE,VALUE1
      DOUBLE PRECISION COEFFS(NC)
      CHARACTER CHARS*74
C
  99  FORMAT(7X,A)
C
C     Decide which way the order numbers run ...
C
      IF (M1.LT.M2) THEN
         DM=1
      ELSE
         DM=-1
      END IF
C
C     Initial value for counter at the end of last order
C
      ILAST=0
C
C     This is the main loop over all orders, selecting lines, fitting
C     them and listing the results ...
C
      DO M=M1,M2,DM
C
C        First we select the lines to use in the fit ...
C
         NEWNL=0
         DO J=1,NLID,1
            IF (ORDER(J).EQ.M) THEN
               NEWNL=NEWNL+1
               NEWCH(NEWNL)=CHANS(J)
               NEWWV(NEWNL)=WAVES(J)
               NEWWT(NEWNL)=WEIGHTS(J)
               NEWCL(NEWNL)=CLASS(J)
            END IF
         END DO
C
C        Next we output some header information
C
         WRITE(IREC,99) ' '
         CHARS=' '
         WRITE(CHARS,'(13X,A,I3)')
     :     'ECHARC Fit Record for Order Number = ',M
         WRITE(IREC,99) CHARS
         WRITE(CHARS,'(12X,A)')
     :     '=========================================='
         WRITE(IREC,99) CHARS
         WRITE(IREC,99) ' '
C
C        Finally we are ready to get to work performing the fit.
C
         DO I=1,NC
            COEFFS(I)=0.
         END DO
         IF (NEWNL.LT.NCOEFF) THEN
            WRITE(CHARS,'(A,I3,A)') 'Not enough points for',
     :                               NCOEFF-1,'th order fit.'
            IF (NCOEFF.EQ.2) CHARS(25:26)='st'
            IF (NCOEFF.EQ.3) CHARS(25:26)='nd'
            IF (NCOEFF.EQ.4) CHARS(25:26)='rd'
            WRITE(IREC,99) CHARS
            NEWNCF=NEWNL
         ELSE
            NEWNCF=NCOEFF
         END IF
C
C        If no lines were found, say no more. (Is it OK if only one line
C        was found?)
C
         IF (NEWNCF.GT.0) THEN
C
C        Sort on channel number and then perform the fit.
C
            CALL ARC_ARSORT(NEWCH,NEWWV,NEWWT,NEWCL,NEWNL)
            CALL FIG_WXYFIT(NEWCH,NEWWV,NEWWT,NEWNL,COEFFS,NEWNCF-1)
C
C        Print out the results so far...
C
            WRITE(IREC,99) ' '
            WRITE (CHARS,'(I3,A)') NEWNCF-1,'th order polynomial fit'
            IF (NEWNCF.EQ.2) CHARS(4:5)='st'
            IF (NEWNCF.EQ.3) CHARS(4:5)='nd'
            IF (NEWNCF.EQ.4) CHARS(4:5)='rd'
            WRITE(IREC,99) CHARS
            WRITE(IREC,99) ' '
            WRITE(IREC,99) 'Coefficients of fit are -'
            WRITE(IREC,99) ' '
C
C        Print out the coefficents
C
            NC4=0
            DO WHILE (NC4.LT.NEWNCF)
               NC1=NC4+1
               NC4=MIN(NC4+4,NEWNCF)
               WRITE(CHARS,'(4(1PE13.5))',ERR=320)(COEFFS(I),I=NC1,NC4)
  320          WRITE(IREC,99) CHARS
            END DO
            WRITE(IREC,99) ' '
C
C        Evaluate mean dispersion
C
            VALUE=FLOAT(NX)
            ST=GEN_EPOLYD(1.D0,COEFFS,NEWNCF)
            END=GEN_EPOLYD(DBLE(VALUE),COEFFS,NEWNCF)
            ADISP=(END-ST)/VALUE
            WRITE(CHARS,'(A,F9.3,A)',ERR=330) 'Mean dispersion    =',
     +                                 ADISP,' angstroms/channel'
  330       WRITE(IREC,99) CHARS
C
C        And also give start, end and central wavelengths
C
            VALUE=VALUE/2.
            VALUE=GEN_EPOLYD(DBLE(VALUE),COEFFS,NEWNCF)
            WRITE(CHARS,'(A,F9.3,A)',ERR=340) 'Start wavelength   =',
     :                                                ST,' angstroms'
  340       WRITE(IREC,99) CHARS
            WRITE(CHARS,'(A,F9.3,A)',ERR=350) 'End wavelength     =',
     :                                               END,' angstroms'
  350       WRITE(IREC,99) CHARS
            WRITE(CHARS,'(A,F9.3,A)',ERR=360) 'Central wavelength =',
     :                                             VALUE,' angstroms'
  360       WRITE(IREC,99) CHARS
C
C        Produce title for line-list table
C
            WRITE(IREC,99) ' '
            IF (DRMS) THEN
               WRITE(IREC,99)
     1         '  Line     Channel   Wavelength  Calculated'//
     2         '  Discrepancy   RMS if'
               WRITE(IREC,99)
     1         '                                 Wavelength'//
     2         '                omitted'
            ELSE
               WRITE(IREC,99)
     1         '  Line     Channel   Wavelength  Calculated'//
     2         '  Discrepancy'
               WRITE(IREC,99)
     1         '                                 Wavelength'
            END IF
            WRITE(IREC,99) ' '
C
C        Now print table and work out RMS error on the way
C
            WARNING=.FALSE.
            RMS=0.
            ADISP=1./(ADISP*ADISP*.25)
            DO I=1,NEWNL
               INET=I+ILAST
               VALUE=GEN_EPOLYD(DBLE(NEWCH(I)),COEFFS,NEWNCF)
               VALUE1=VALUE-NEWWV(I)
               RMS=RMS+VALUE1*VALUE1
               IF (DRMS) THEN
                  IF (NEWNL.LE.2) THEN
                     RMSX = 0.0
                  ELSE
                     CALL ARC_ARFITX(I,NEWCH,NEWWV,NEWWT,NEWNL,NEWNCF,
     :                                                            RMSX)
                  END IF
                  WRITE(CHARS,'(2X,I4,4F12.3,F11.4)',IOSTAT=IGNORE)
     :                      INET,NEWCH(I),NEWWV(I),VALUE,VALUE1,RMSX
               ELSE
                  WRITE(CHARS,'(2X,I4,4F12.3)',IOSTAT=IGNORE)
     :                      INET,NEWCH(I),NEWWV(I),VALUE,VALUE1
               END IF
               IF (I.GT.1) THEN
                  IF (NEWWV(I).LT.NEWWV(I-1)) THEN
                     WARNING=.TRUE.
                     CHARS(2:2)='*'
                  END IF
               END IF
               IF (NEWCL(I).EQ.1) CHARS(1:1)='a'
               IF (NEWCL(I).EQ.2) CHARS(1:1)='e'
               IF (NEWCL(I).GE.3) CHARS(1:1)='+'
               WRITE(IREC,99) CHARS
            END DO
            IF (WARNING) THEN
               WRITE(IREC,99) ' '
               WRITE(IREC,99) ' * WARNING - Wavelengths out of order'
            END IF
C
C        And print out RMS error as the final line on this page ...
C
            WRITE(IREC,99) ' '
            RMS=SQRT(RMS/FLOAT(NEWNL))
            WRITE(CHARS,'(A,F11.4)') 'RMS error: ',RMS
            WRITE(IREC,99) CHARS
            WRITE(IREC,99) CHAR(12)
         END IF
C
C        Update I at end of this order and we're all done !
C
         ILAST=ILAST+NEWNL
C
C        Now go and do the same for the next order
C
      END DO
C
      RETURN
      END
