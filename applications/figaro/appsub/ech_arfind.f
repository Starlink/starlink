C+
      SUBROUTINE ECH_ARFIND(ZDATA,NX,NORDERS,WDATA,INTFIT,NINTFIT,
     :                     NC,COEFFS,NCOEFF,PARMS,SIGMA,PRMS,NLMAX,
     :                      ORDER,CHANS,WAVES,CLASS,NLID,NARCS,ARC,
     :                     IOUT,FORDR,LORDR,XPTS,ZPTS,WPTS,WEIGHTS,
     :                     FITS,NFITS,MPTR,WPTR,OPTR,PTRS,WORK1,WW,
     :                                        XX,YY,DEVICE,MONITOR)
C
C     E C H _ A R F I N D
C
C     This is the second generation automatic line finder for ECHARC.
C     It takes the array WDATA filled with guesses for the wavelengths
C     at each NX x NORDER point, and attempts to locate as many lines
C     as it can from the ARCn tables in the ZDATA array.
C
C     Parameters -  (">" Input, "<" Output, "!" Modified, "W" Workspace)
C
C     (>) ZDATA    (Real Array ZDATA(NX,NORDERS))  The array containing
C                  the arc image in which lines are to be found.
C     (>) NX       (Integer)  The number of columns in ZDATA and WDATA;
C                  which equals the number of points in every order.
C     (>) NORDERS  (Integer)  The number of rows in ZDATA and WDATA;
C                  which equals the number of orders in the images.
C     (!) WDATA    (Double Precision Array WDATA(NX,NORDERS))  The array
C                  containing the wavelengths of each pixel of ZDATA. On
C                  input these are preliminary guesses from ECH_ARFILL,
C                  on output they are fitted wavelengths based on the arc
C                  lines actually found in each order.
C     (>) INTFIT   (Real Array INTFIT(NINTFIT)) The order numbers that
C                  were fit interactively; these will not be tampered
C                  with by ECH_ARFIND.
C     (>) NINTFIT  (Integer)  The number of orders fit interactively.
C     (>) NC       (Integer)  The maxium number of polynomial
C                  arc coefficients permissible.
C     (!) COEFFS   (Double Precision array COEFFS(NC))  The polynomial
C                  arc coefficients giving wavelength as a function of
C                  channel number for an echelle order.
C     (!) NCOEFF   (Integer)  The number of arc coefficients selected by
C                  the user for use in the fit.
C     (>) PARMS    (Real array PARMS(2)) Parameters for use in the auto
C                  fit routine.
C     (!) SIGMA    (Real)  The linewidth used when identifying arc lines.
C     (>) NLMAX    (Integer)  The maximum number of lines that can be
C                  identified (total -- in all orders).
C     (!) ORDER    (Integer Array ORDER(NLMAX))  The order numbers of arc
C                  lines identified so far, into which new line IDs are
C                  also recorded.
C     (!) CHANS    (Real Array CHANS(NLMAX))  The channel numbers of arc
C                  lines identified so far, into which new line IDs are
C                  also recorded.
C     (!) WAVES    (Real Array WAVES(NLMAX))  The wavelengths of arc lines
C                  identified so far, into which new line IDs are also
C                  recorded.
C     (!) CLASS    (Integer Array CLASS(NLMAX))  The class code of lines
C                  identified so far, into which new line IDs are also
C                  recorded.
C     (!) NLID     (Integer)  The number of lines identifed so far, which
C                  gets updated as new lines are found or incorrect lines
C                  are deleted from the above arrays.
C     (>) NARCS    (Integer)  The total number of arc lines in the catalog
C                  we are to try to locate in the image.
C     (>) ARC      (Real Array ARC(NARCS))  The standard wavelengths of all
C                  the arc lines to be matched to those found in the echelle
C                  order image data.
C     (>) IOUT     (Integer)  The unit number to be used to output the
C                  order number, channel number, and wavelength data for
C                  identified lines. The filename is ARLINES.ECH
C     (W) XPTS     (Real array XPTS(NX))  Workspace array for channel
C                  numbers 1...NX.
C     (W) ZPTS     (Real array ZPTS(NX))  Workspace array for ZDATA
C                  from a single order.
C     (W) WPTS     (Real array WPTS(NX))  Workspace array for weights of
C                  a single order.
C     (W) ARCTOT   (Real array ARCTOT(3*NLARCS))  Workspace array for
C                  the combined list of ARC1, ARC2, and ARC3.
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX))  An array giving the
C                  weight to be applied to a certain line when
C                  calculating a polynomial fit.
C     (W) PTRS     (Integer array PTRS(NX))  Workspace array for ECH_ARCFIT.
C     (W) WORK1    (Double precision array WORK1(*)) Workspace     "   "   .
C     (W) WW       (Double precision array WW(NX)) Workspace array "   "   .
C     (W) XX       (Double precision array XX(NX)) Workspace array "   "   .
C     (W) YY       (Double precision array YY(NX)) Workspace array "   "   .
C
C     Functions / subroutines used -
C
C                           ECHARC:  v. 1.0        JKM / ESO 19 Nov 1987
C
C     Modified:         --> ECHARC:  v. 1.5        JKM / ESO 8. Dez 1987
C
C        The automatic search sequence was changed, so that instead of
C        starting with order 1 and going to order NY, it starts mid-way
C        between the first and last order interactively fit and proceeds
C        from the center outward in four stages.  It then makes three
C        additional attempts to fit orders previously unsatisfactory.
C
C     30 May 1988 WFL/AAO Warn and don't save if located line matches one
C                 in the list of lines located so far. Alter use of FITS
C                 so 1 means interactively fit and 2 means satisfied so
C                 as to permit auto-identification of lines in the inter-
C                 actively fitted orders.
C     23 Jul 1993 HME/UoE, Starlink.  Disuse GKD_*.
C     18 Apr 1995 HME/UoE, Starlink.  Changed size of WORK1 in caller
C                 according to new needs in ECH_ARCFIT. Changed its
C                 dimension here to WORK1(1).
C     2005 May 31 MJC/Starlink  Use CNF_PVAL for pointers to mapped
C                 data.
C+
      IMPLICIT NONE

C    Global Constants

      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function
C
C     Functions
C
      INTEGER GEN_BSEARCH,ICH_ENCODE
      DOUBLE PRECISION GEN_EPOLYD
C
C     Parameters
C
      LOGICAL  MONITOR
      INTEGER  NX,NORDERS,NINTFIT,NC,NCOEFF,NLMAX,NARCS,NFITS
      INTEGER  ORDER(NLMAX),CLASS(NLMAX),NLID,IOUT,FORDR,LORDR
      INTEGER  FITS(NORDERS)
      REAL     ZDATA(NX,NORDERS),INTFIT(NINTFIT)
      REAL     PARMS(2),SIGMA,PRMS,CHANS(NLMAX),WAVES(NLMAX)
      REAL     ARC(NARCS),WEIGHTS(NLMAX)
      DOUBLE PRECISION   WDATA(NX,NORDERS),COEFFS(NC)
      CHARACTER*(*) DEVICE
C
C     Workspace arrays used by ECH_ARFIND
C
      INTEGER MPTR,WPTR,OPTR
      REAL WPTS(NX),XPTS(NX),ZPTS(NX)
C
C     Workspace arrays simply passed to ECH_ARCFIT
C
      INTEGER PTRS(NX)
      DOUBLE PRECISION WORK1(1)
      DOUBLE PRECISION WW(NX), XX(NX), YY(NX)
C
C     Local variables for new lines identified in each order
C
      INTEGER NLOMX
      PARAMETER(NLOMX=100)
      INTEGER NEWNL,NEWCL(NLOMX)
      REAL    NEWCH(NLOMX),NEWWV(NLOMX),NEWWT(NLOMX)
      REAL    XPLOT(NLOMX),YPLOT(NLOMX)
C
C     Local variables for zone fit sequence & multiple passes
C
      INTEGER NZONES
      PARAMETER (NZONES=4)
      INTEGER NZ,NZSEARCH,JFMIN,JFMAX
      INTEGER JSTART(NZONES+3),JEND(NZONES+3),JDELTA(NZONES+3)
C
C     Other local variables
C
      LOGICAL FINAL,FIT,NEWFILL,QSEARCH,SATISFIED
      INTEGER MAXITER
      PARAMETER (MAXITER=10)
      INTEGER I,J,IORDR,K,KWAVE1,KWAVEN,NFOUND,NREJEX,NTOTAL
      INTEGER STATUS,ISRCHK,IWAVEK,DJ,ORDERN,IGNORE,NEXT,ITER
      INTEGER NCFILL,MATCH,OLDNLID
      REAL CENTRD,STRNTH,CENTWAS,CHDELTA,WAVEK
      REAL PDELTA,WDELTA,RMS,STRNWAS
      DOUBLE PRECISION DWNEW,DWMAX
      CHARACTER STRING*78
C
C     As does ARAUTO, we will reject lines more than CHDELTA pixels from a
C        line already found, or one that disagrees by more than WDELTA with
C        the interactive fit RMS.  Below we determine these two limits from
C        the specified multiples CHFACT and SIGFACT of SIGMA and RMS values,
C        respectively (PRMS is in pixels, as is PDELTA therefore).
C
      CHDELTA=SIGMA*PARMS(1)
      PDELTA=PRMS*PARMS(2)
*+
      WRITE(STRING,*)
     :   'At start of automatic fit, CHDELTA,PDELTA=',CHDELTA,PDELTA
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Sigma in pixels',SIGMA
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   RMS in pixels',PRMS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   CHFACT',PARMS(1)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   SIGFACT',PARMS(2)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   CHDELTA in pixels (SIGMA * CHFACT)',CHDELTA
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   PDELTA in pixels (RMS * SIGFACT)',PDELTA
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER(
     :   'Lines closer than CHDELTA pixels to the previously '//
     :   'accepted line are rejected',STATUS)
      CALL PAR_WRUSER(
     :   'Lines more than PDELTA pixels from their expected '//
     :       'locations are rejected',STATUS)
*-
C
C     Fill the array XPTS with pixel numbers 1 ... NX
C
      CALL GEN_NFILLF(NX,XPTS)
C
C     See which way order numbers are incremented ...
C
      IF (FORDR.LT.LORDR) THEN
         DJ=1
      ELSE
         DJ=-1
      END IF
C
C     See if ARC and WDATA entries increase the same way, which will save
C        us much time later when matching lines to the arc image.
C
      I=NX/2
      IF (DJ.EQ.1) THEN
         J=INTFIT(2)-FORDR+1
      ELSE
         J=FORDR-INTFIT(2)+1
      END IF
C
      QSEARCH=.TRUE.
      IF ((ARC(1).LT.ARC(2)).AND.
     :    (WDATA(I,J).GT.WDATA(I+1,J))) THEN
             QSEARCH=.FALSE.
      ELSE IF ((ARC(1).GT.ARC(2)).AND.
     :    (WDATA(I,J).LT.WDATA(I+1,J))) THEN
             QSEARCH=.FALSE.
      END IF
C
C     Divide up NORDERS into four zones ...
C
      JFMIN=NORDERS
      JFMAX=1
      DO J=1,NORDERS,1
         IF (FITS(J).GT.0) THEN
            JFMIN=MIN(JFMIN,J)
            JFMAX=MAX(JFMAX,J)
         END IF
      END DO
      JSTART(1)=(JFMIN+JFMAX)/2
      JEND(1)=JFMIN
      JDELTA(1)=-1
      JSTART(2)=JSTART(1)+1
      JEND(2)=JFMAX
      JDELTA(2)=1
      NZSEARCH=2
      IF (JFMIN.GT.1) THEN
         JSTART(3)=JEND(1)-1
         JEND(3)=1
         JDELTA(3)=-1
         NZSEARCH=3
         IF (JFMAX.LT.NORDERS) THEN
            JSTART(4)=JEND(2)+1
            JEND(4)=NORDERS
            JDELTA(4)=1
            NZSEARCH=4
         END IF
      ELSE
         IF (JFMAX.LT.NORDERS) THEN
            JSTART(3)=JEND(2)+1
            JEND(3)=NORDERS
            JDELTA(3)=1
            NZSEARCH=3
         END IF
      END IF
C
C     Now arrange for three extra passes through the orders
C
      JSTART(NZSEARCH+1)=1
      JSTART(NZSEARCH+2)=NORDERS
      JSTART(NZSEARCH+3)=1
      JEND(NZSEARCH+1)=NORDERS
      JEND(NZSEARCH+2)=1
      JEND(NZSEARCH+3)=NORDERS
      JDELTA(NZSEARCH+1)=1
      JDELTA(NZSEARCH+2)=-1
      JDELTA(NZSEARCH+3)=1
C
C     Open the MONITOR plot if requested
C
      IF (MONITOR) THEN
         CALL PGBEGIN(7,DEVICE,1,1)
         CALL PGSCH(1.0)
         CALL PGENV(FLOAT(FORDR-DJ),FLOAT(LORDR+DJ),
     :              -0.1,0.1,0,0)
         CALL PGLABEL('Order Number',' ',
     :                'ECHARC Residuals in Angstroms')
         CALL PGSCH(1.33)
C        CALL PGLABEL(' ','\gl\darc\u-\gl\dfit',' ')
         CALL PGLABEL(' ',
     :      CHAR(92)//'gl'//CHAR(92)//'darc'//CHAR(92)//'u-'//
     :      CHAR(92)//'gl'//CHAR(92)//'dfit',
     :      ' ')
         CALL PGSCH(0.66)
         IF (NINTFIT.GE.1) THEN
C
C           Start by plotting interactive results
C
            DO J=1,NINTFIT,1
               K=0
               IORDR=INTFIT(J)
               DO I=1,NLID,1
                  IF (ORDER(I).EQ.IORDR) THEN
                     IF (K+1.LE.NLOMX) THEN
                        K=K+1
                        NEWCH(K)=CHANS(I)
                        NEWWV(K)=WAVES(I)
                        NEWWT(K)=WEIGHTS(I)
                     END IF
                  END IF
               END DO
               IF (K.GE.NCOEFF) THEN
                  CALL FIG_WXYFIT(NEWCH,NEWWV,NEWWT,K,COEFFS,NCOEFF-1)
                  DO I=1,K,1
                     XPLOT(I)=FLOAT(IORDR)
                     YPLOT(I)=NEWWV(I)-GEN_EPOLYD(DBLE(NEWCH(I)),
     :                                              COEFFS,NCOEFF)
                  END DO
                  CALL PGPOINT(K,XPLOT,YPLOT,3)
               END IF
            END DO
         END IF
      END IF
C
C     This is the main loop over orders of ZDATA and WDATA ...
C
      DO NZ=1,NZSEARCH+3,1
      DO J=JSTART(NZ),JEND(NZ),JDELTA(NZ)
C
         ORDERN=FORDR+DJ*(J-1)
C
C        Check to make this order hasn't been fit before ... (WFL 31/5/88,
C        comment out section that prevents interactively fitted orders from
C        being fitted in the first pass though the orders)
C
         FIT=.FALSE.
C        IF (NZ.LE.NZSEARCH) THEN
C           DO I=1,NINTFIT,1
C              IORDR=ABS(INTFIT(I)-FORDR)+1
C              IF (IORDR.EQ.J) THEN
C                 FIT=.TRUE.
C              END IF
C           END DO
C        ELSE
            FIT=(FITS(J).EQ.2)
            IF ((.NOT.FIT).AND.(NZ.EQ.NZSEARCH+1))
     :         CALL PAR_WRUSER(' *** 2nd PASS RESULTS ***',STATUS)
            IF ((.NOT.FIT).AND.(NZ.EQ.NZSEARCH+2))
     :         CALL PAR_WRUSER(' *** 3rd PASS RESULTS ***',STATUS)
            IF ((.NOT.FIT).AND.(NZ.EQ.NZSEARCH+3))
     :         CALL PAR_WRUSER(' *** 4th PASS RESULTS ***',STATUS)
            IF (FITS(J).EQ.0) THEN
               CALL ECH_ARDELE(ORDERN,0,NLMAX,ORDER,CHANS,WAVES,
     :                                       WEIGHTS,CLASS,NLID)
            END IF
C        END IF
C
         IF (.NOT.FIT) THEN
C
C           We must find arc lines automatically below.  Start by
C              filling the 1D arrays ZPTS and WPTS, and perform 1st fit...
C
            DO I=1,NX,1
               ZPTS(I)=WDATA(I,J)
               WPTS(I)=1.00
            END DO
C
C           First fit uses ECH_ARCFIT not FIG_WXYFIT to make use of
C           all NX pixels
C
            CALL ECH_ARCFIT(XPTS,ZPTS,WPTS,NX,
     :                      PTRS,WORK1,WW,XX,YY,
     :                      COEFFS,NCOEFF-1)
C
C           Now we can let ZPTS perform its real function as 1D arc data,
C              and reuse WPTS as single precision wavelengths...
C
            DO I=1,NX,1
               ZPTS(I)=ZDATA(I,J)
               WPTS(I)=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
            END DO
C
C           Convert PDELTA criterion into WDELTA based on central A/pix of this
C              current order ...
C
            WDELTA=PDELTA*(ABS(WPTS(NX/2+1)-WPTS(NX/2)))
*+
      WRITE(STRING,*) 'Attempting to find lines for new order'
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Order number',ORDERN
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   WDELTA in Angstroms (RMS * SIGFACT)',WDELTA
      CALL PAR_WRUSER(STRING,STATUS)
*-
C
C           Next we must decide which lines to look for ...
C
            KWAVE1=GEN_BSEARCH(ARC,NARCS,WPTS(1))
            IF (KWAVE1.EQ.0) THEN
               CALL PAR_WRUSER(
     :         ' First wavelength falls outside range of .ARC files',
     :                                                        STATUS)
               RETURN
            END IF
            KWAVEN=GEN_BSEARCH(ARC,NARCS,WPTS(NX))
            IF (KWAVEN.EQ.0) THEN
               CALL PAR_WRUSER(
     :         ' Last wavelength falls outside range of .ARC files',
     :                                                        STATUS)
               RETURN
            END IF
C
C           Here we begin a loop over all lines between IWAVE1 & IWAVEN
C              looking for arc lines near the line wavelengths...
C
            FINAL=.FALSE.
            ITER=1
            NCFILL=NINTFIT
            NEWFILL=.FALSE.
            SATISFIED=.FALSE.
C
            DO WHILE (((ITER.LE.MAXITER).AND.
     :                 (.NOT.SATISFIED))
     :           .OR. ((ITER.LE.MAXITER).AND.
     :                 (.NOT.FINAL)))
C
               IF (SATISFIED) FINAL=.TRUE.
               NEWNL=0
               NFOUND=0
               NREJEX=0
               NTOTAL=KWAVEN-KWAVE1+1
               ISRCHK=1
               CENTWAS=-10.0
               STRNWAS=-10.0
               DO K=KWAVE1,KWAVEN,1
C
C                 Find nearest pixel wavelength to Kth arc line wavelength
C                    (Notice how this is done ... we don't have to repeat the
C                     the last searches from WPTS(1) to WPTS(ISRCHK-1) as long
C                     as entries later in ARC are expected later in WPTS).
C
                  IF (QSEARCH) THEN
                     IWAVEK=GEN_BSEARCH(WPTS(ISRCHK),NX-ISRCHK+1,ARC(K))
     :                                      +ISRCHK-1
                  ELSE
                     IWAVEK=GEN_BSEARCH(WPTS,NX,ARC(K))
                  END IF
C
C                 See if an arc line is nearby in the data
C
                  CENTRD=FLOAT(IWAVEK)
                  CALL GEN_CENTROID(ZPTS,NX,SIGMA,CENTRD,STRNTH,STATUS)
                  IF (STATUS.EQ.0) THEN
C
C                    We found a line ... if it is farther than CHDELTA from
C                       the last line we accepted, add it to the tables ...
C
                     IF (ABS(CENTRD-CENTWAS).GT.CHDELTA) THEN
C
C                       ... if it is within WDELTA of where it ought to be,
C                          that is ...
C
                        WAVEK=GEN_EPOLYD(DBLE(CENTRD),COEFFS,NCOEFF)
                        IF (ABS(WAVEK-ARC(K)).LT.WDELTA) THEN
C
C                          Okay, add the line to the lists...
C
                           NFOUND=NFOUND+1
                           NEWNL=NEWNL+1
                           NEWCH(NEWNL)=CENTRD
                           NEWWV(NEWNL)=ARC(K)
                           NEWWT(NEWNL)=1.00
                           NEWCL(NEWNL)=2
                           CENTWAS=CENTRD
                           STRNWAS=STRNTH
C
                        ELSE
C
C                          Sorry but we've got to reject this as an unlikely
C                             candidate in the data for our line from the table.
C
*+
      CALL PAR_WRUSER(
     :   'Rejected line because it''s too far from its expected '//
     :       'location',STATUS)
      WRITE(STRING,*) '   Estimated wavelength',WAVEK
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Line-list wavelength',ARC(K)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s centroid',CENTRD
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s centroid',CENTWAS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s strength',STRNTH
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s strength',STRNWAS
      CALL PAR_WRUSER(STRING,STATUS)
*-
                           NREJEX=NREJEX+1
C
                        END IF
C
                     ELSE
C
C                       These two lines are too close together, so reject
C                          the one with weaker strnth ... if two are close,
C                          reject them both ... we might even have found the
C                          same feature twice, in which case we must reject
C                          it since we don't know what wavelength to give it.
C
                        IF (STRNTH.LT.(0.5*STRNWAS)) THEN
C
C                          Throw away the current line
C
*+
      CALL PAR_WRUSER(
     :   'Rejected line because it''s less than half the strength '//
     :       'of the too close previous line',STATUS)
      WRITE(STRING,*) '   Estimated wavelength',WAVEK
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Line-list wavelength',ARC(K)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s centroid',CENTRD
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s centroid',CENTWAS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s strength',STRNTH
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s strength',STRNWAS
      CALL PAR_WRUSER(STRING,STATUS)
*-
                           NREJEX=NREJEX+1
C
                        ELSE IF (STRNTH.GT.(2.0*STRNWAS)) THEN
C
C                          Throw away the previous line
C
*+
      CALL PAR_WRUSER(
     :   'Rejected previous line because it''s less than half the '//
     :       'strength of the too close current line',STATUS)
      WRITE(STRING,*) '   Estimated wavelength',WAVEK
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Line-list wavelength',ARC(K)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s centroid',CENTRD
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s centroid',CENTWAS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s strength',STRNTH
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s strength',STRNWAS
      CALL PAR_WRUSER(STRING,STATUS)
*-
                           NREJEX=NREJEX+1
C
C                          Keep the current line only if it passes the WDELTA
C                             criteria; otherwise throw it away too...
C
                           WAVEK=GEN_EPOLYD(DBLE(CENTRD),COEFFS,NCOEFF)
                           IF (ABS(WAVEK-ARC(K)).LT.WDELTA) THEN
                              NEWCH(NEWNL)=CENTRD
                              NEWWV(NEWNL)=ARC(K)
                              NEWWT(NEWNL)=1.00
                              NEWCL(NEWNL)=2
                           ELSE
*+
      CALL PAR_WRUSER(
     :   'Rejected line because it''s too far from its expected '//
     :       'location',STATUS)
      WRITE(STRING,*) '   Estimated wavelength',WAVEK
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Line-list wavelength',ARC(K)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s centroid',CENTRD
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s centroid',CENTWAS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s strength',STRNTH
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s strength',STRNWAS
      CALL PAR_WRUSER(STRING,STATUS)
*-
                              NREJEX=NREJEX+1
                              NFOUND=NFOUND-1
                              NEWNL=NEWNL-1
                           END IF
                           CENTWAS=CENTRD
                           STRNWAS=STRNTH
C
                        ELSE
C
C                          Throw away both lines
C
*+
      CALL PAR_WRUSER(
     :   'Rejected current and previous lines because neither is '//
     :       'dramatically stronger and they are too close',STATUS)
      WRITE(STRING,*) '   Estimated wavelength',WAVEK
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Line-list wavelength',ARC(K)
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s centroid',CENTRD
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s centroid',CENTWAS
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   This line''s strength',STRNTH
      CALL PAR_WRUSER(STRING,STATUS)
      WRITE(STRING,*) '   Last line''s strength',STRNWAS
      CALL PAR_WRUSER(STRING,STATUS)
*-
                           NREJEX=NREJEX+2
                           NFOUND=NFOUND-1
                           NEWNL=NEWNL-1
C
                        END IF
C
                     END IF
C
C                    Finally, continue search next time around where we left
C                       off this time
C
                     ISRCHK=MAX(1,IWAVEK-5)
C
                  ELSE
C
C                    No obvious line nearby, so keep looking next time from
C                       where we left off this time...
C
                     ISRCHK=MAX(1,IWAVEK-1)
C
                  END IF
C
               END DO
C
C              Done identifying lines in the current order "I".  To play it
C                 safe, sort the NEWxx arrays before going ahead to and re-
C                 computing the coefficients based on the lines we found ...
C
               IF (NEWNL.GT.4) THEN
                  CALL ARC_ARSORT(NEWCH,NEWWV,NEWWT,NEWCL,NEWNL)
                  CALL FIG_WXYFIT(NEWCH,NEWWV,NEWWT,NEWNL,
     :               COEFFS,NCOEFF-1)
C
C                 Now we want to know what the maximum new-old wavelength
C                    difference is ...
C
                  DWMAX=0.0D00
                  DO I=1,NX,1
                     DWNEW=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
                     DWMAX=DMAX1(DWMAX,DABS(DWNEW-WDATA(I,J)))
                  END DO
C
C                 Check to see if we are satisfied with this iteration
C
                  IF (NZ.NE.NZSEARCH+3) THEN
                     IF ((SNGL(DWMAX).LT.WDELTA).AND.
     :                   (NFOUND.GE.NREJEX))  THEN
                        SATISFIED=.TRUE.
                     END IF
                  ELSE
                     IF ((SNGL(DWMAX).LT.WDELTA).AND.
     :                   (ITER.GE.MAXITER/2)) SATISFIED=.TRUE.
                  END IF
C
C                 Calculate RMS to report to user below
C
                  CALL ARC_ARFITX(0,NEWCH,NEWWV,NEWWT,NEWNL,NCOEFF,RMS)
C
               ELSE
C
                  RMS=0
C
               END IF
C
C              Report results to the user:
C
               IF (ITER.NE.1) THEN
                  STRING='     lines found,     rejected,'//
     :                   ' of     total in order    ; RMS=         '
C
                  WRITE (STRING,
     :               '('' '',I4,'' lines found, '',I4,'//
     :               ' '' rejected, of '',I4,'' total in order '','//
     :               ' I3,''; RMS=            '')')
     :               NFOUND,NREJEX,NTOTAL,ORDERN
                  IGNORE=ICH_ENCODE(STRING,RMS,67,2,NEXT)
                  IF (NEXT.LE.78) THEN
                     STRING(NEXT:78)=' '
                  END IF
                  CALL PAR_WRUSER(STRING,STATUS)
               END IF
C
C              If not satisfied, see if we can get a better set of starting
C                 wavelengths for our search processes ...
C
               IF (.NOT.SATISFIED) THEN
C
                  IF (((NFOUND.GE.(NREJEX/2)).AND.
     :                 (NEWNL.GT.4)).OR.
     :                ((NEWFILL).AND.
     :                 (NEWNL.GE.4)))  THEN
C
C                    We accepted at least a third of the lines that we
C                       found centroids for, and we are able to make a fit
C                       (chances are that these fitted wavelengths are good);
C                    OR we refilled the WDATA array and are still not satis-
C                       fied with the agreement of the fit (in this case we
C                       wouldn't gain anything by refilling WDATA again, so
C                       we will take the new fitted wavelengths and try 'em);
C                    ... so we use the fitted wavelengths as new starting WPTS
C                    for the remaining iterations....
C
                     DO I=1,NX,1
                        WDATA(I,J)=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
                        WPTS(I)=WDATA(I,J)
                     END DO
C
                  ELSE
C
C                    Something more drastic will be required.  What we
C                       will do is go back to ECH_ARFILL and use the
C                       orders we've fit so far to get better starting
C                       wavelengths.
C
                     IF (NCFILL+3.LE.11) THEN
                        NCFILL=NCFILL+3
                     ELSE
                        NCFILL=11
                     END IF
                     IF (NCFILL.GT.NFITS) THEN
                        NCFILL=NFITS
                     END IF
C
                     CALL ECH_ARFILL(WDATA,NX,NORDERS,FORDR,LORDR,
     :                               FITS,NFITS,NCFILL,
     :                               %VAL( CNF_PVAL( MPTR ) ),
     :                               %VAL( CNF_PVAL( WPTR ) ),
     :                               %VAL( CNF_PVAL( OPTR ) ) )
C
C                    This ought to count for a lot when improving things
C
                     ITER=ITER+MAXITER/3
                     NEWFILL=.TRUE.
C
                     DO I=1,NX,1
                        WPTS(I)=WDATA(I,J)
                     END DO
C
                  END IF
C
               ELSE IF (.NOT.FINAL) THEN
C
C                 If we are satisfied with the fit, maybe we can find
C                    more lines by doing a final fit ...
C
                  DO I=1,NX,1
                     WDATA(I,J)=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
                     WPTS(I)=WDATA(I,J)
                  END DO
C
               END IF
C
C              Increment the number of iterations and repeat until satisfied
C                and the final fit has been completed for additional lines.
C
               ITER=ITER+1
C
            END DO
C
C           This is now the best we can do, so if these lines are not already
C              in the list for this order,save them with the rest we have
C              found....
C
            OLDNLID = NLID
            DO K=1,NEWNL,1
               I = 1
               MATCH = 0
               DO WHILE (I.LE.OLDNLID.AND.MATCH.EQ.0)
                  IF (ORDER(I).EQ.ORDERN.AND.
     :                ABS(WAVES(I)-NEWWV(K)).LT.0.05) THEN
                     MATCH = I
                  END IF
                  I = I+1
               END DO
               IF (MATCH.EQ.0) THEN
                  NLID=NLID+1
                  ORDER(NLID)=ORDERN
                  CHANS(NLID)=NEWCH(K)
                  WAVES(NLID)=NEWWV(K)
                  WEIGHTS(NLID)=NEWWT(K)
                  CLASS(NLID)=NEWCL(K)
               END IF
            END DO
C
C           ... and copy over final answers to WDATA array if satisfied
C
            IF (SATISFIED) THEN
C
               DO I=1,NX,1
                  WDATA(I,J)=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
               END DO
C
C              We can use these fitted results to improve starting values
C                 later, if need be ...
C
               FITS(J)=2
               NFITS=NFITS+1
C
C              Add these lines to the MONITOR plot
C
               IF (MONITOR) THEN
                  DO I=1,NEWNL,1
                     XPLOT(I)=FLOAT(ORDERN)
                     YPLOT(I)=NEWWV(I)-GEN_EPOLYD(DBLE(NEWCH(I)),
     :                                              COEFFS,NCOEFF)
                  END DO
                  CALL PGPOINT(NEWNL,XPLOT,YPLOT,5)
               END IF
C
            END IF
         END IF
      END DO
      END DO
C
C     Write out results to file 'ARLINES.ECH'
C
      CALL ECH_ARLIST(IOUT,NLMAX,ORDER,CHANS,WAVES,CLASS,
     :                NLID,NCOEFF,SIGMA)
C
C     Close the MONITOR plot if it exists
C
      IF (MONITOR) THEN
         CALL PGSCH(1.00)
         CALL PGEND
      END IF
C
C     That's all folks ...
C
      RETURN
      END
