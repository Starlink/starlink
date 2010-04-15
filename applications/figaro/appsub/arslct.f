C+
      SUBROUTINE ARSLCT(XVALS,ZVALS,NX,ARC1,ARC2,ARC3,NLARCS,
     :                  HIGH,LOW,SIGMA,NLMAX,ARCS,IXST,IXEN,
     :                  NCHAN,COEFFS,ORDER,FITTED,SHOWRMS,XS,CHANS,
     :                  WAVES,WEIGHTS,CLASS,NLID,COMPLETE)
C
C     A R S L C T
C
C     ARC utility routine.  This is the routine that does the tricky
C     user interaction, getting the user to move the cursor across
C     the plot produced by ARPLOT to select lines and identify them.
C
C     When the user hits the cursor button, there are a number of
C     possibilities, depending on the key that was pressed.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) XVALS     (Real array XVALS(NX)) The x-values of the data
C     (>) ZVALS     (Real array ZVALS(NX)) The data values
C     (>) NX        (Integer) The number of data values
C     (>) ARC1      (Real array ARC1(NLARCS)) Holds the wavelengths
C                   for the first arc type.  Terminates with 0.
C     (>) ARC2      (Real array ARC2(NLARCS)) The wavelengths for the
C                   second arc type.
C     (>) ARC3      (Real array ARC3(NLARCS)) The wavelengths for the
C                   third arc type.
C     (>) NLARCS    (Integer) The dimension of the ARCn arrays.
C     (>) HIGH      (Real) The high value used for the current plot.
C     (>) LOW       (Real) The low value used for the current plot.
C     (>) SIGMA     (Real) The expected width for arc lines, in
C                   pixels.
C     (>) NLMAX     (Integer) The maximum number of arc lines that
C                   can be identified.
C     (>) ARCS      (Character) The command parameter ARCTYPE, ie
C                   the arc types represented by ARC1 etc, separated
C                   by commas.
C     (!) IXST      (Integer) Passed as the first data element currently
C                   displayed.  Returns as the first element to be
C                   displayed in the next display.
C     (!) IXEN      (Integer) Like IXST, but the last element.
C     (!) NCHAN     (Integer) Number of channels to be displayed as a
C                   section.
C     (!) COEFFS    (Double precision array COEFFS(ORDER)) The
C                   current wavelength coefficients.
C     (!) ORDER     (Integer) The number of coefficients used.
C     (!) FITTED    (Logical) True if a wavelength fit has been obtained.
C     (!) SHOWRMS   (Logical) True if an RMS is to be shown the first time
C                   through the loop.
C     (!) XS        (Logical) true if identified lines are to be
C                   indicated just by an X, instead of a wavelngth.
C     (!) CHANS     (Real array CHANS(NLMAX)) The channel numbers for
C                   the identified arc lines.
C     (!) WAVES     (Real array WAVES(NLMAX)) The wavelengths for the
C                   identified arc lines.
C     (!) WEIGHTS   (Real array WEIGHTS(NLMAX)) The weights for the
C                   identified arc lines.
C     (!) CLASS     (Integer array CLASS(NLMAX)) The class codes for
C                   the identified arc lines.
C     (!) NLID      (Integer) The number of identified arc lines.
C     (<) COMPLETE  (Logical) Returned as true if there are to be
C                   no more displays.
C
C                                       KS / CIT 13th June 1984
C     Modified:
C
C     5th Sept 1985  KS / AAO  SHOWRMS now a parameter.  RMS value wasn't
C                    being displayed following a 'C(og)' because of the
C                    need to leave this routine to redisplay, which lost
C                    GOTRMS value.  WEIGHTS and CLASS parameters added.
C     12th Sept 1985 KS / AAO  PAR_ routines replace WRUSER and RDUSER.
C     30th Sept 1986 KS / AAO  Now allows for possibility that ORDER may
C                    be greater than the number of lines identified.
C     11th Mar  1988 KS / AAO  Modified for the GKS version of PGPLOT.
C                    Now uses GKD_ routines.
C     20th Mar  1991 KS / AAO  Now uses ICH_CF rather than ICH_ENCODE
C                    for wavelength values.  Test for channel numbers made
C                    more stringent to allow for micron values.
C     31st Aug  1992 HME / UoE, Starlink.  GKD_QNUM was CALLed though it
C                    is declared as LOGICAL. Now assign its value to
C                    IGNORE.
C     23rd Jul  1993 HME / UoE, Starlink.  Disuse GKD_*, to a large
C                    extent. Disuse PAR_Q*.  Use PAR_ABORT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL COMPLETE,FITTED,SHOWRMS,XS
      INTEGER INVOKE,NX,NLARCS,NLMAX,IXST,IXEN,NCHAN,NLID,ORDER
      INTEGER CLASS(NLMAX)
      REAL XVALS(NX),ZVALS(NX),ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS)
      REAL SIGMA,CHANS(NLMAX),WAVES(NLMAX),WEIGHTS(NLMAX)
      REAL HIGH,LOW
      DOUBLE PRECISION COEFFS(ORDER)
      CHARACTER*(*) ARCS
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE,ICH_LEN,ICH_FOLD,GEN_BSEARCH
      INTEGER ARID
      DOUBLE PRECISION GEN_EPOLYD
      CHARACTER ICH_CF*16
C
C     Local variables
C
      LOGICAL CHANGE,DONE,FORGET,FOUND,GIVEN,GOTRMS,REANAL,WANTED,WAVED
      INTEGER CHLEN,ID,IX,IXMID,LENGTH,NCHWAS,NEXT,STATUS
      REAL CENT,RMS,STRENGTH,VALUE,VFIT,VINT,WAVEL,X,XCENT
      REAL XV,Y,YOFF,YP
      CHARACTER CH,CHARS*16,REPLY*72
C
      GOTRMS=SHOWRMS
      SHOWRMS=.FALSE.
      YOFF=(HIGH-LOW)*.05
      X=(XVALS(IXST)+XVALS(IXEN))*.5
      Y=(HIGH+LOW)*.5
C
C     See if XVALS represents wavelengths or pixel numbers
C
      WAVED=((XVALS(1).NE.1.0).AND.(XVALS(2).NE.2.0))
C
C     Note the way this works.  After a cursor position and character
C     are obtained, we essentially have a 'case' structure to handle
C     the various possibilities.  If program changes the display, it
C     can set DONE (but not COMPLETE) and this routine will exit in
C     such a way that the calling program redisplays the original
C     arc portion and re-calls this routine. WANTED is used to
C     indicate that a line is required (which is not true, for example,
C     if 'H' for Help was used.)  CHANGE indicates that the number of
C     identified lines has changed and a fit should be recalculated.
C     GOTRMS indicates that a fit has been made and an RMS is known.
C
      DONE=.FALSE.
      DO WHILE (.NOT.DONE)
C
C        Erase text and get cursor position, converting character
C        to upper case.
C
         WANTED=.TRUE.
         FOUND=.FALSE.
         CHANGE=.FALSE.
         CALL GEN_WAIT(1000)
         IF (GOTRMS) THEN
            REPLY='RMS now = '
            INVOKE=ICH_ENCODE(REPLY,RMS,12,2,NEXT)
            IF (NLID.GE.ORDER) THEN
               REPLY(NEXT:)=' Order of fit = '
               INVOKE=ICH_ENCODE(REPLY,FLOAT(ORDER-1),NEXT+16,0,NEXT)
            ELSE
               REPLY(NEXT:)=' Specified order = '
               INVOKE=ICH_ENCODE(REPLY,FLOAT(ORDER-1),NEXT+19,0,NEXT)
               REPLY(NEXT:)=', order used = '
               INVOKE=ICH_ENCODE(REPLY,FLOAT(NLID-1),NEXT+15,0,NEXT)
            END IF
            CALL GKD_WRITE_LINE(REPLY(:NEXT))
         END IF
         CALL GKD_WRITE_LINE('Use cursor to select an arc line')
         CALL PGCURSE(X,Y,CH)
         LENGTH=ICH_FOLD(CH)
C
C        Work out which pixel number the returned X value falls in
C        (gets returned as zero if out of displayed range).
C
         IX=GEN_BSEARCH(XVALS(IXST),IXEN-IXST+1,X)+IXST-1
C
C        (Start of 'CASE' structure, testing either character used, or
C        the cases where the cursor is out of the display area.)
C
         IF (CH.EQ.'Q') THEN
C
C           Q indicates 'QUIT'
C
            WANTED=.FALSE.
            CALL PAR_CNPAR('QUITSEL')
            CALL PAR_RDKEY('QUITSEL',.TRUE.,COMPLETE)
            IF (PAR_ABORT()) RETURN
            DONE=COMPLETE
C
         ELSE IF ((CH.EQ.'H').OR.(CH.EQ.'?')) THEN
C
C           'H' or '?' indicates 'HELP'
C
            CALL ARHELP
            DONE=.TRUE.
            WANTED=.FALSE.
C
         ELSE IF (CH.EQ.'L') THEN
C
C           'L' indicates 'set Length of displayed section'
C
            WANTED=.FALSE.
            NCHWAS=NCHAN
            CALL PAR_CNPAR('DISNCHAN')
            CALL PAR_RDVAL('DISNCHAN',10.,FLOAT(NX),FLOAT(NCHAN),
     :         'Pixels',VALUE)
            IF (PAR_ABORT()) RETURN
            NCHAN=VALUE
            IF (NCHAN.NE.NCHWAS) THEN
               IXST=MAX(IX-NCHAN/2,1)
               IF (IXST+NCHAN-1.GT.NX) THEN
                  IXEN=NX
                  IXST=MAX(1,IXEN-NCHAN+1)
               ELSE
                  IXEN=IXST+NCHAN-1
               END IF
               DONE=.TRUE.
            END IF
C
         ELSE IF (CH.EQ.'M') THEN
C
C           'M' indicates 'MOVE' to a specific x-value
C
            WANTED=.FALSE.
            IXMID=(IXST+IXEN)*.5
            CALL PAR_CNPAR('MOVETOX')
            CALL PAR_RDVAL('MOVETOX',XVALS(1),XVALS(NX),XVALS(IXMID),
     :         ' ',VALUE)
            IF (PAR_ABORT()) THEN
               RETURN
            ELSE
               IX=GEN_BSEARCH(XVALS,NX,VALUE)
               IXST=MAX(1,IX-NCHAN/2)
               IF ((IXST+NCHAN-1).GT.NX) THEN
                  IXEN=NX
                  IXST=MAX(1,IXEN-NCHAN+1)
               ELSE
                  IXEN=IXST+NCHAN-1
               END IF
               DONE=.TRUE.
            END IF
C
         ELSE IF (CH.EQ.'D') THEN
C
C           'D' indicates 'DELETE' line from line list
C
            WANTED=.FALSE.
            CENT=IX
            ID=ARID(CHANS,NLID,CENT,VALUE)
            IF ((ABS(VALUE-CENT).GT.(3.*SIGMA)).OR.(ID.EQ.0)) THEN
               CALL GKD_WRITE_LINE(
     :                     'No identified line at that position')
            ELSE
C
C              Have identified line to delete. Indicate which one by
C              rubbing out the indicator for it, then ask if this
C              was the correct line.
C
               CALL PGSCI(0)
               IF (.NOT.XS) THEN
                  IF (CLASS(ID).NE.0) THEN
                     CHARS='('//ICH_CF(WAVES(ID))
                     NEXT=ICH_LEN(CHARS)+1
                     CHARS(NEXT:NEXT)=')'
                     NEXT=NEXT+1
                  ELSE
                     CHARS=ICH_CF(WAVES(ID))
                     NEXT=ICH_LEN(CHARS)+1
                  END IF
               END IF
               CHLEN=NEXT-1
               IF (WAVED) THEN
                  IX=CHANS(ID)
                  XV=XVALS(IX)+(CHANS(ID)-FLOAT(IX))*
     :                                   (XVALS(IX+1)-XVALS(IX))
               ELSE
                  XV=CHANS(ID)
               END IF
               YP=MAX(ZVALS(IX-1),ZVALS(IX),ZVALS(IX+1))+YOFF
               IF (XS) THEN
                  IF (CLASS(ID).EQ.0) THEN
                     CALL PGPOINT(1,XV,YP,ICHAR('X'))
                  ELSE
                     CALL PGPOINT(1,XV,YP,ICHAR('+'))
                  END IF
               ELSE
                  CALL PGTEXT(XV,YP,CHARS(:CHLEN))
               END IF
               CALL PGSCI(1)
               CALL PAR_CNPAR('LINEOK')
               CALL PAR_RDKEY('LINEOK',.TRUE.,GIVEN)
               IF (PAR_ABORT()) THEN
                  RETURN
               ELSE IF (GIVEN) THEN
C
C                 User seems happy, so delete line from tables
C
                  CALL ARDELE(ID,CHANS,WAVES,WEIGHTS,CLASS,NLID)
                  CHANGE=.TRUE.
               ELSE
C
C                 OOps! Wrong line.  Restore identification.
C
                  CALL GKD_WRITE_LINE('Sorry')
                  IF (XS) THEN
                     IF (CLASS(ID).EQ.0) THEN
                        CALL PGPOINT(1,XV,YP,ICHAR('X'))
                     ELSE
                        CALL PGPOINT(1,XV,YP,ICHAR('+'))
                     END IF
                  ELSE
                     CALL PGTEXT(XV,YP,CHARS(:CHLEN))
                  END IF
               END IF
            END IF
C
         ELSE IF (CH.EQ.'S') THEN
C
C           'S' indicates 'SIGMA' and allows sigma to be changed.
C
            CALL PAR_CNPAR('SIGMA')
            CALL PAR_RDVAL('SIGMA',0.01,100.,SIGMA,' ',VALUE)
            IF (PAR_ABORT()) RETURN
            SIGMA=VALUE
C
         ELSE IF (CH.EQ.'O') THEN
C
C           'O' indicates 'ORDER'.  Change current fit order.
C
            CALL PAR_CNPAR('ORDER')
            CALL PAR_RDVAL('ORDER',0.,10.,FLOAT(ORDER-1),' ',VALUE)
            IF (PAR_ABORT()) RETURN
            ORDER=VALUE+1
            CHANGE=.TRUE.
            WANTED=.FALSE.
C
         ELSE IF (CH.EQ.'C') THEN
C
C           'C' indicates 'COG'.  Find center by center-of-gravity.
C
            CALL ARCENTR(XVALS,ZVALS,NX,IX,CENT,XCENT)
            IF (PAR_ABORT()) RETURN
            FOUND=.TRUE.
            DONE=.TRUE.
            WANTED=.FALSE.
            SHOWRMS=.TRUE.
C
         ELSE IF (CH.EQ.'W') THEN
C
C           'W' indicates 'WAVELENGTHS' - lines are to be indicated
C           by wavelength, not just by an X.
C
            XS=.FALSE.
            WANTED=.FALSE.
            DONE=.TRUE.
C
         ELSE IF (CH.EQ.'X') THEN
C
C           'X' indicates use X's to show lines, not wavelengths.
C           Gives a less cluttered, but less informative plot.
C
            XS=.TRUE.
            WANTED=.FALSE.
            DONE=.TRUE.
C
         ELSE IF (CH.EQ.'E') THEN
C
C           'E' indicates 'EXPAND'. Indicate center with cursor.
C
            CALL ARPOSN(XVALS,ZVALS,NX,IX,CENT,XCENT)
            FOUND=.TRUE.
            DONE=.TRUE.
            SHOWRMS=.TRUE.
            WANTED=.FALSE.
C
         ELSE IF (CH.EQ.'R') THEN
C
C           'R' indicates 'REPEAT' - redraw current section.
C
            WANTED=.FALSE.
            DONE=.TRUE.
C
         ELSE IF ((X.LT.XVALS(IXST)).OR.(CH.EQ.'B')) THEN
C
C           Out to the left, or 'B' for back to previous section.
C
            WANTED=.FALSE.
            IF (IXST.LE.1) THEN
               CALL PAR_CNPAR('QUITSEL')
               CALL PAR_RDKEY('QUITSEL',.FALSE.,COMPLETE)
               IF (PAR_ABORT()) RETURN
               DONE=COMPLETE
            ELSE
               IXST=MAX(1,IXST-NCHAN)
               IXEN=MIN(NX,IXST+NCHAN-1)
               DONE=.TRUE.
            END IF
         ELSE IF ((X.GT.XVALS(IXEN)).OR.(CH.EQ.'N')) THEN
C
C           Out to the right, or 'N' for Next section.
C
            WANTED=.FALSE.
            IF (IXEN.GE.NX) THEN
               CALL PAR_CNPAR('QUITSEL')
               CALL PAR_RDKEY('QUITSEL',.FALSE.,COMPLETE)
               IF (PAR_ABORT()) RETURN
               DONE=COMPLETE
            ELSE
               IXEN=MIN(NX,IXEN+NCHAN)
               IXST=MAX(1,IXEN-NCHAN+1)
               DONE=.TRUE.
            END IF
         END IF
C
C        (That was the end of the 'CASE' structure.)
C
C        At the end of trying the possible special cases, if
C        FOUND is set, we already know a line center.  If WANTED
C        is not set we don't have need one because some other
C        operation (like HELP) has been performed.  If IX is out
C        of range we simply aren't going to find a line.
C        Given all that, what do we do now?
C
         IF ((WANTED).AND.(.NOT.FOUND)) THEN
C
C           We need to try for a line.
C
            IF (IX.LE.0) THEN
               FOUND=.FALSE.
            ELSE
C
C              Look for the center of a line nearby.
C
               CENT=IX
               CALL GEN_CENTROID(ZVALS,NX,SIGMA,CENT,STRENGTH,STATUS)
               FOUND=STATUS.EQ.0
               IX=CENT
               XCENT=XVALS(IX)+(CENT-FLOAT(IX))*(XVALS(IX+1)-XVALS(IX))
            END IF
            IF (.NOT.FOUND) THEN
               CALL GKD_WRITE_LINE(
     :                    'No obvious line near cursor position')
            ELSE
C
C              Point it out on the display. This changes cursor
C              position in x but not in y.
C
               IX=CENT
               YP=MAX(ZVALS(IX-1),ZVALS(IX),ZVALS(IX+1))+YOFF
               IF (WAVED) THEN
                  XV=XVALS(IX)+(CENT-FLOAT(IX))*
     :                                (XVALS(IX+1)-XVALS(IX))
               ELSE
                  XV=CENT
               END IF
               CALL PGPOINT(1,XV,YP,ICHAR('X'))
               X=XV
            END IF
         END IF
C
C        Well, do we have a line?
C
         IF (FOUND) THEN
C
C           Yes. Give it's location and get its wavelength.
C
            REPLY='Line at pixel'
            STATUS=ICH_ENCODE(REPLY,CENT,15,3,NEXT)
            IF (FITTED) THEN
               REPLY(NEXT:)=' (fit: '
               VFIT=GEN_EPOLYD(DBLE(CENT),COEFFS,MIN(NLID,ORDER))
               REPLY(NEXT+8:)=ICH_CF(VFIT)
               NEXT=ICH_LEN(REPLY)+1
               REPLY(NEXT:)=')'
               NEXT=NEXT+1
            END IF
            VINT=0.
            IF (NLID.GE.2) THEN
               REPLY(NEXT:)=' (interp: '
               CALL ARGUESS(CHANS,WAVES,NLID,CENT,VINT)
               REPLY(NEXT+11:)=ICH_CF(VINT)
               NEXT=ICH_LEN(REPLY)+1
               REPLY(NEXT:)=')'
            END IF
            CALL GKD_WRITE_LINE(REPLY)
            CALL ARGETW(ARC1,ARC2,ARC3,ARCS,NLARCS,VINT,VFIT,
     :                                            FORGET,WAVEL)
            IF (PAR_ABORT()) RETURN
C
C           Insert line in tables - also check for multiple lines,
C           or too many lines.  May re-analyse all line centers.
C
            IF (.NOT.FORGET) THEN
               CALL ARLADD(CENT,WAVEL,NX,ZVALS,SIGMA,NLMAX,CHANS,
     :                      WAVES,WEIGHTS,CLASS,NLID,FORGET,REANAL)
               IF (PAR_ABORT()) RETURN
               IF (REANAL) THEN
                  DONE=.TRUE.
                  WANTED=.FALSE.
                  CHANGE=.TRUE.
               END IF
            END IF
C
            IF (FORGET) THEN
C
C              Ignore line.  If possible, clear out the marker.
C              Note that WANTED is set if the line was located using
C              the current plot, rather than an expanded ('P' or 'C')
C              one.
C
               IF (WANTED) THEN
                  CALL PGSCI(0)
                  CALL PGPOINT(1,XV,YP,ICHAR('X'))
                  CALL PGSCI(1)
               END IF
            ELSE
C
C              Line added. If possible, indicate line by wavelength
C
               CHANGE=.TRUE.
               IF (WANTED) THEN
                  CALL PGSCI(0)
                  CALL PGPOINT(1,XV,YP,ICHAR('X'))
                  CALL PGSCI(1)
                  REPLY=ICH_CF(WAVEL)
                  NEXT=ICH_LEN(REPLY)+1
                  CALL PGTEXT(XV,YP,REPLY(:NEXT-1))
               END IF
            END IF
         END IF
C
C        At end of loop, see if we can (or need to) calculate a
C        new running fit.
C
         IF (CHANGE.AND.(NLID.GT.2)) THEN
            CALL ARIFIT(CHANS,WAVES,WEIGHTS,NLID,COEFFS,
     :                                       MIN(NLID,ORDER),RMS)
            GOTRMS=.TRUE.
            FITTED=.TRUE.
         ELSE
            SHOWRMS=.FALSE.
         END IF
      END DO
C
      END
