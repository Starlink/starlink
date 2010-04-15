C+
      SUBROUTINE ARAUTO(ZVALS,NX,SIGMA,ARC1,ARC2,ARC3,NLARCS,NLMAX,
     :           ORDER,COEFFS,PARMS,NLID,CHANS,WAVES,WEIGHTS,CLASS)
C
C     A R A U T O
C
C     This routine is a first attempt at introducing an automatic
C     line finder into ARC.  It is relatively heavy-handed.  It
C     looks at each pixel in turn, and if there is no line yet
C     identified nearby, sees if it can find a peak in the region.
C     If it can, it works out the wavelength of the line it has found
C     and adds it to the tables if it can find a line in the arc
C     lists that is close to that wavelength.  Lines found by this
C     routine are classed as class 1.
C
C     Parameters (">" input, "!" modified, workspace, "<" output)
C
C     (>) ZVALS     (Real array ZVALS(NX)) The arc spectrum.
C     (>) NX        (Integer) The number of data values
C     (>) SIGMA     (Real) The current sigma value.
C     (>) ARC1      (Real array ARC1(NLARCS)) Holds the wavelengths
C                   for the first arc type.  Terminates with 0.
C     (>) ARC2      (Real array ARC2(NLARCS)) The wavelengths for the
C                   second arc type.
C     (>) ARC3      (Real array ARC3(NLARCS)) The wavelengths for the
C                   third arc type.
C     (>) NLARCS    (Integer) The dimension of the ARCn arrays.
C     (>) NLMAX     (Integer) Maximum possible number of arc lines
C     (>) ORDER     (Integer) The number of coefficients used for the
C                   fit. (Note: this is the usual meaning of 'order'
C                   plus 1)
C     (>) COEFFS    (Double precision array COEFFS(NC)) The
C                   coefficients of the current fit.
C     (>) PARMS     (Real array PARMS(2)) Parameters controlling the
C                   fit.  PARMS(1) is CHFACT, PARMS(2) is SIGFACT.
C                   (See comments below for further explanation.)
C     (!) NLID      (Integer) The number of identified lines.
C     (!) CHANS     (Real array CHANS(NLMAX)) The centers of the
C                   identified lines, in pixel numbers.
C     (!) WAVES     (Real array WAVES(NLMAX)) The wavelengths of the
C                   identified lines.
C     (!) WEIGHTS   (Real array WEIGHTS(NLMAX)) The weights for the
C                   identified arc lines.
C     (!) CLASS     (Integer array CLASS(NLMAX)) The class codes for
C                   the identified arc lines.
C
C                                                  KS / AAO 30th Sept 1985
C     Modified:
C
C     20th Mar 1991 KS / AAO. Now uses ICH_CF instead of ICH_ENCODE for
C                   wavelength values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NLARCS, NLMAX, NLID, ORDER, CLASS(NLMAX)
      REAL    ZVALS(NX), ARC1(NLARCS), ARC2(NLARCS), ARC3(NLARCS)
      REAL    SIGMA, PARMS(2), CHANS(NLMAX), WAVES(NLMAX)
      REAL    WEIGHTS(NLMAX)
      DOUBLE PRECISION COEFFS(ORDER)
C
C     Functions
C
      INTEGER ICH_LEN,ICH_ENCODE
      DOUBLE PRECISION GEN_EPOLYD
      CHARACTER ICH_CF*16
C
C     Local variables
C
      LOGICAL CLEAR, FOUND
      INTEGER COUNT, I, ICEN, ID, INVOKE, IX, NEXT, NLIDO, STATUS
      REAL    CENTER, CENTWAS, CHDELTA, RMS, STRENGTH, WAVEL, WDELTA
      CHARACTER STRING*64
C
C     The two main parameters for the algorithm are CHDELTA and WDELTA.
C     CHDELTA is the minimum distance in channels that a line must be
C     away from a pixel if that pixel is to considered as the starting
C     point for an attempt to find a new line, and is calculated as
C     a multiple (CHFACT) of the current SIGMA value.
C     WDELTA is the maximum allowed discrepancy between a calculated
C     wavelength for a found line and its tabulated value.  This is
C     calculated as a multiple (SIGFACT) of the RMS of the current
C     fit.  If the current fit is perfect, half of a channel in the
C     middle of the arc is used.
C
      CALL ARFITX(0,CHANS,WAVES,WEIGHTS,NLID,ORDER,RMS)
      IF (RMS.LE.0.) THEN
         ICEN=NX/2
         WDELTA=(GEN_EPOLYD(DBLE(ICEN),COEFFS,ORDER)
     :                -GEN_EPOLYD(DBLE(ICEN+1),COEFFS,ORDER))*0.5
      ELSE
         WDELTA=RMS*PARMS(2)
      END IF
      CHDELTA=SIGMA*PARMS(1)
C
C     A couple of initial values.
C
      CENTWAS=-1.0
      NLIDO=NLID
      COUNT=0
C
C     Loop through each pixel in the arc.
C
      DO IX=1,NX
         CLEAR=.TRUE.
C
C        See if a known line is too close to this pixel
C
         DO ID=1,NLIDO
            IF ((ABS(CHANS(ID)-IX)).LT.CHDELTA) THEN
               CLEAR=.FALSE.
               GO TO 340
            END IF
         END DO
  340    CONTINUE
         IF (CLEAR) THEN
C
C           See if we can find a peak nearby
C
            CENTER=IX
            CALL GEN_CENTROID(ZVALS,NX,SIGMA,CENTER,STRENGTH,STATUS)
            IF (STATUS.EQ.0) THEN
C
C              Calculate the wavelength and look for it in the three
C              arc line lists.  Make sure this isn't the same as the
C              last line we found (CENWAS)!
C
               WAVEL=GEN_EPOLYD(DBLE(CENTER),COEFFS,ORDER)
               IF (ABS(CENTER-CENTWAS).GT.WDELTA) THEN
                  FOUND=.FALSE.
                  DO I=1,NLARCS
                     IF (ARC1(I).LE.0.) GO TO 380
                     IF ((ABS(WAVEL-ARC1(I))).LT.WDELTA) THEN
                        FOUND=.TRUE.
                        WAVEL=ARC1(I)
                        GO TO 380
                     END IF
                  END DO
  380             CONTINUE
                  IF (.NOT.FOUND) THEN
                     DO I=1,NLARCS
                        IF (ARC2(I).LE.0.) GO TO 390
                        IF ((ABS(WAVEL-ARC2(I))).LT.WDELTA) THEN
                           FOUND=.TRUE.
                           WAVEL=ARC2(I)
                           GO TO 390
                        END IF
                     END DO
  390                CONTINUE
                     IF (.NOT.FOUND) THEN
                        DO I=1,NLARCS
                           IF (ARC3(I).LE.0.) GO TO 400
                           IF ((ABS(WAVEL-ARC3(I))).LT.WDELTA) THEN
                              WAVEL=ARC3(I)
                              FOUND=.TRUE.
                              GO TO 400
                           END IF
                        END DO
  400                   CONTINUE
                     END IF
                  END IF
C
C                 Well, well.  We have a line. Add it to the tables.
C
                  IF (FOUND) THEN
                     STRING='Line at '//ICH_CF(WAVEL)
                     NEXT=ICH_LEN(STRING)+1
                     STRING(NEXT:)=' found, channel '
                     INVOKE=ICH_ENCODE(STRING,CENTER,NEXT+16,2,NEXT)
                     CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
                     IF (NLID.GE.NLMAX) THEN
                        CALL PAR_WRUSER(
     :                     'Tables full.  Unable to add line',STATUS)
                     ELSE
                        NLID=NLID+1
                        CHANS(NLID)=CENTER
                        WAVES(NLID)=WAVEL
                        WEIGHTS(NLID)=1.0
                        CLASS(NLID)=1
                        CENTWAS=CENTER
                        COUNT=COUNT+1
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END DO
C
C     Final report
C
      IF (COUNT.EQ.0) THEN
         CALL PAR_WRUSER('Unable to find any additional lines.',STATUS)
      ELSE
         INVOKE=ICH_ENCODE(STRING,FLOAT(COUNT),1,0,NEXT)
         STRING(NEXT:)=' additional lines found'
         IF (COUNT.EQ.1) STRING(NEXT+16:NEXT+16)=' '
         CALL PAR_WRUSER(STRING(:NEXT+22),STATUS)
      END IF
C
      END
