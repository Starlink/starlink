C+
      SUBROUTINE ECH_ARINTR(X_ARRAY,Z_ARRAY,XLABEL,ZLABEL,NPTS,
     :                      NC,COEFFS,NCOEFF,PARMS,SIGMA,IORDR,
     :                      NLMAX,ORDER,CHANS,WAVES,CLASS,NLID,
     :                      NLARCS,ARC1,ARC2,ARC3,ARCS,IOUT,AP,
     :                      WEIGHTS,W1_ARRAY,W2_ARRAY,OUTRMS)
C
C     E C H _ A R I N T R
C
C     This is the main INTeRactive identify, fit, re-identify, and
C     refit routine for ECHARC, used only for a small number of the
C     total orders.  Given a single 1D order (Z_array vs. X_array),
C     it interactively associates arc lines with their wavelengths,
C     and writes the results along with order number into ARLINES.ECH
C     (logical unit number IOUT).
C
C     Parameters -       (">" Input, "<" Output, "!" Modified)
C
C     (>) X_ARRAY  (Real Array X_ARRAY(NPTS)  The X array for this
C                  echelle order.  Typically this is pixel numbers
C                  1 ... NPTS, but if original image had X.DATA,
C                  then that is used instead (not expected).
C     (>) Z_ARRAY  (Real Array Z_ARRAY(NPTS)  The Z data (counts)
C                  in this order at channels given by  X_ARRAY.
C     (>) XLABEL   (Character)  The X axis label for soft plots
C                  used to identify arc lines.
C     (>) ZLABEL   (Character)  The Y axis label for soft plots
C                  used to identify arc lines.
C     (>) NPTS     (Integer)  The number of X and Z data points.
C     (>) NC       (Integer)  The maxium number of polynomial
C                  arc coefficients permissible.
C     (!) COEFFS   (Double Precision)  The polynomial arc coeff
C                  giving wavelength as a function of channel
C                  number for this echelle order.
C     (!) NCOEFF   (Integer)  The number of arc coefficients
C                  selected by the user for use in the fit.
C     (>) PARMS    (Real array PARMS(2)) Parameters for use in
C                  the auto fit routine.
C     (!) SIGMA    (Real)  The linewidth used when identifying
C                  arc lines.
C     (>) IORDR    (Integer)  The number of the particular order
C                  sent to ECH_ARINTR to be fit.  This gets recorded
C                  along with channel number, wavelength, etc.,
C                  in the file ARLINES.ECH that is produced during
C                  the fitting process.
C     (>) NLMAX    (Integer)  The maximum number of lines that
C                  can be identified (total -- in all orders).
C     (!) ORDER    (Integer Array ORDER(NLMAX))  The order numbers
C                  of lines identified so far, into which new line
C                  IDs are also recorded.
C     (!) CHANS    (Real Array CHANS(NLMAX))  The channel numbers
C                  of lines identified so far, into which new line
C                  IDs are also recorded.
C     (!) WAVES    (Real Array WAVES(NLMAX))  The wavelengths of
C                  arc lines identified so far, into which new line
C                  IDs are also recorded.
C     (!) CLASS    (Integer Array CLASS(NLMAX))  The class codes
C                  of lines identified so far, into which new line
C                  IDs are also recorded.
C     (!) NLID     (Integer)  The number of lines identifed so far,
C                  which gets updated as new lines are found or
C                  incorrect lines deleted from the above arrays.
C     (>) NLARCS   (Integer)  The maximum number of arc line wave-
C                  lengths in each of the ARCn arrays.
C     (>) ARCn     (Real Arrays ARCn(NLARCS)  The standard wavelengths
C        n=1,2,3   of arc lines to be matched to those found in the
C                  echelle order data.
C     (>) ARCS     (Character)  The names of ARC1,ARC2,ARC3 separated
C                  by commas.  Blanks or "NONE" will designate names
C                  for no data present in ARCn.
C     (>) IOUT     (Integer)  The unit number to be used to output
C                  the order number, channel number, and wavelength
C                  data for identified lines. File is ARLINES.ECH
C     (>) AP       (Real) The dispersion in Angstroms/Pixel for this
C                  echelle order.
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX))  An array giving the
C                  weight to be applied to a certain line when
C                  calculating a polynomial fit.
C     (!) W1_ARRAY (Real Array W1_ARRAY(NPTS))  Work array for plots
C                  of the dispersion curve as determined by the fit.
C     (!) W2_ARRAY (Real Array W2_ARRAY(NPTS))  Work array for plots
C                  of the dispersion curve as determined by the fit.
C     (<) OUTRMS   (Real) The final RMS value of the middle interactive
C                  order fit with ECH_ARINTR.
C
C     Functions / subroutines used -
C
C     ECH_ARPLOT  (FIGARO)    Plots an order section by section and
C                             allows user to identify lines.
C     ECH_ARSLCT  (FIGARO)    Takes an identified line and matches its
C                             wavelength to one given in the ARCS.
C     ECH_ARMENU  (FIGARO)    Allows the user to fit identified lines
C                             with various polynomials, examine the
C                             dispersion plots of each, and edit lines.
C     ECH_ARDELE  (FIGARO)    Deletes data for a given arc line from
C                             arrays ORDER, CHANS, WAVES, WEIGHTS,
C                             and CLASS, then reduces NLID by one.
C     ECH_ARLIST  (FIGARO)    Outputs the results of line identifica-
C                             tions to ARLINES.ECH.
C     ICH_ENCODE  (ICH_ pckg) Encodes a number into a character string.
C
C                                              JKM / CIT 10 Dec 1986
C
C     31st May 1988 WFL/AAO Don't call ARFITX if # lines < 2
C     26th Jul 1993 HME/UoE, Starlink. Disuse GKD_* and PAR_Q*.
C                   Use PAR_ABORT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER   NPTS,NC,NCOEFF,IORDR,NLMAX,NLARCS
      INTEGER   ORDER(NLMAX),CLASS(NLMAX),NLID,IOUT
      REAL      X_ARRAY(NPTS),Z_ARRAY(NPTS),OUTRMS,AP
      REAL      WEIGHTS(NLMAX),W1_ARRAY(NPTS),W2_ARRAY(NPTS)
      REAL      PARMS(2),SIGMA,CHANS(NLMAX),WAVES(NLMAX)
      REAL      ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS)
      DOUBLE PRECISION   COEFFS(NC)
      CHARACTER*(*)      XLABEL,ZLABEL
      CHARACTER*(*)      ARCS
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE,ICH_LEN
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables for new lines identified
C
      INTEGER NLOMX
      PARAMETER(NLOMX=100)
      INTEGER NEWNL,NEWCL(NLOMX)
      REAL    NEWCH(NLOMX),NEWWV(NLOMX),NEWWT(NLOMX)
C
C     Other local variables
C
      INTEGER NCHAN,IXST,IXEN,I,INVOKE,NEXT,IX,STATUS
      LOGICAL FITTED,SHOWRMS,XS,REPEAT,COMPLETE,HPLOT,HARD
      REAL    HIGH,LOW,VALUE
      CHARACTER*27 TLABEL
      CHARACTER*64 ARFILE
C
C     Initialise local variables
C
      NEWNL=0
      FITTED=.FALSE.
      SHOWRMS=.FALSE.
      XS=.FALSE.
      REPEAT=.TRUE.
      NCHAN=200
      TLABEL=CHAR(92)//'(2248)'//CHAR(92)//'bechelle order '
      INVOKE=ICH_ENCODE(TLABEL,FLOAT(IORDR),24,0,NEXT)
      DO I=NEXT,27,1
         TLABEL(I:I)=' '
      END DO
      IF (NLID.GT.0) THEN
C
C        Search through ORDER(1..NLID) looking for existing IORDR lines
C
         DO IX=1,NLID,1
C
C           If we find one, we copy it to NEW**
C
            IF (ORDER(IX).EQ.IORDR) THEN
               NEWNL=NEWNL+1
               NEWCH(NEWNL)=CHANS(IX)
               NEWWV(NEWNL)=WAVES(IX)
               NEWWT(NEWNL)=WEIGHTS(IX)
               NEWCL(NEWNL)=CLASS(IX)
            END IF
C
         END DO
      END IF
C
      IF (NEWNL.GT.0) THEN
C
C        Found one or more matches above.  Delete from old arrays.
C
         CALL ECH_ARDELE(IORDR,NEWNL,NLMAX,ORDER,CHANS,WAVES,
     :                                    WEIGHTS,CLASS,NLID)
C
      END IF
C
C     Now we can send subsets of arrays w/ just IORDR lines to ECH_AR's
C
      DO WHILE (REPEAT)
C
C        Set initial section range for display
C
         IXST=1
         IXEN=MIN(NCHAN,NPTS)
C
C        This is the loop through the various sections of each order,
C        displaying a section, selecting a line with the cursor, giving
C        its wavelength, then picking another line or moving on to a
C        different section, until all the arc has been covered.
C
         COMPLETE=.FALSE.
         DO WHILE (.NOT.COMPLETE)
C
C           Display
C
            CALL ECH_ARPLOT(X_ARRAY,Z_ARRAY,NPTS,IXST,IXEN,
     :                      XLABEL,ZLABEL,TLABEL,NC,COEFFS,
     :                      NCOEFF,XS,NEWCH,NEWWV,NEWCL,NEWNL,
     :                      NLOMX,HIGH,LOW)
C
C           Line selection
C
         CALL ECH_ARSLCT(X_ARRAY,Z_ARRAY,NPTS,ARC1,ARC2,ARC3,NLARCS,
     :                 HIGH,LOW,SIGMA,NLOMX,ARCS,IXST,IXEN,NCHAN,NC,
     :                 COEFFS,NCOEFF,FITTED,SHOWRMS,XS,NEWCH,NEWWV,
     :                 NEWWT,NEWCL,NEWNL,COMPLETE)
         IF (PAR_ABORT()) RETURN
C
         END DO
C
C        Clear screen
C
         CALL PGADVANCE
C
C        Now move to the menu selection sequence where a fit is
C        performed and the user then has the option of some limited
C        editing etc and refitting the line.  This is all controlled
C        by the routINE ARC_ARMENU, which will return with REPEAT reset
C        if the program is to be exited.
C
         CALL ECH_ARMENU(Z_ARRAY,ARC1,ARC2,ARC3,ARCS,NLARCS,NPTS,NC,
     :              SIGMA,NLOMX,NEWCH,NEWWV,NEWWT,NEWCL,NEWNL,FITTED,
     :              NCOEFF,PARMS,W1_ARRAY,W2_ARRAY,REPEAT,COEFFS)
         IF (PAR_ABORT()) RETURN
C
      END DO
C
C     Close down soft plots
C
      CALL PGEND
C
C     We've gotten back an edited and sorted list of NEW** lines in
C        the current order, so we'll add them to the end of our
C        master listings...
C
      DO I=1,NEWNL,1
         NLID=NLID+1
         ORDER(NLID)=IORDR
         CHANS(NLID)=NEWCH(I)
         WAVES(NLID)=NEWWV(I)
         WEIGHTS(NLID)=NEWWT(I)
         CLASS(NLID)=NEWCL(I)
      END DO
C
C     Find value of OUTRMS and AP to return to calling program
C
      AP=DABS(GEN_EPOLYD(DBLE(NPTS/2+1),COEFFS,NCOEFF)
     :        -GEN_EPOLYD(DBLE(NPTS/2),COEFFS,NCOEFF))
C
      IF (NEWNL.LT.2) THEN
         OUTRMS = 0.0
      ELSE
         CALL ARC_ARFITX(0,NEWCH,NEWWV,NEWWT,NEWNL,NCOEFF,VALUE)
         OUTRMS=VALUE
      ENDIF
C
C     Next, we'll write all this out to ARLINES.ECH (in case there's
C        a problem the next time through, these lines won't be lost).
C
      CALL ECH_ARLIST(IOUT,NLMAX,ORDER,CHANS,WAVES,CLASS,NLID,
     :                                           NCOEFF,SIGMA)
C
C     What about a hard copy of the arc?
C
      HPLOT=.FALSE.
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_CNPAR('HARDARC')
      CALL PAR_RDKEY('HARDARC',.FALSE.,HPLOT)
      IF (PAR_ABORT()) RETURN
      IF (HPLOT) THEN
         HPLOT=.FALSE.
         CALL ARC_AHOPEN(2,STATUS)
         IF (STATUS.EQ.0) THEN
            COMPLETE=.FALSE.
            IXST=1
            DO WHILE(.NOT.COMPLETE)
               IXEN=IXST+NCHAN-1
               IF (IXEN.GT.NPTS) THEN
                  IXEN=NPTS
                  IXST=NPTS-NCHAN+1
               END IF
               CALL ECH_ARPLOT(X_ARRAY,Z_ARRAY,NPTS,IXST,IXEN,
     :                        XLABEL,ZLABEL,TLABEL,NC,COEFFS,
     :                        NCOEFF,XS,NEWCH,NEWWV,NEWCL,NEWNL,
     :                        NLOMX,HIGH,LOW)
               IXST=IXST+NCHAN
               COMPLETE=IXST.GE.NPTS-16
            END DO
            CALL PGSLW(1)
            CALL PGEND
            HPLOT=.TRUE.
            CALL PAR_WRUSER('Plot file created',STATUS)
         END IF
      END IF
C
C     Or the dispersion plot?
C
      CALL PAR_CNPAR('HARDISP')
      CALL PAR_RDKEY('HARDISP',.FALSE.,HARD)
      IF (PAR_ABORT()) RETURN
      IF (HARD) THEN
         CALL ARC_AHOPEN(1,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL ARC_ARDISC(NEWCH,NEWWV,NEWWT,NEWCL,NEWNL,COEFFS,NCOEFF,
     :                               NPTS,W1_ARRAY,W2_ARRAY)
            CALL PGSLW(1)
            CALL PGEND
            CALL PAR_WRUSER('Plot file created',STATUS)
            IF (HPLOT) THEN
               CALL PAR_WRUSER(
     :             'Note that two hard copy files have been created,',
     :                                                        STATUS)
               CALL PAR_WRUSER(
     :             'One for the arc data and one for the dispersion.',
     :                                                        STATUS)
             END IF
         END IF
      END IF
C
C     List name of output list file
C
      INQUIRE (UNIT=IOUT,NAME=ARFILE)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Details of order IDs output to '//
     :                            ARFILE(:ICH_LEN(ARFILE)),STATUS)
C
      RETURN
      END
