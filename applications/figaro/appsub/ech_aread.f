C+
      SUBROUTINE ECH_AREAD(ARCS,NLARCS,LU,ARC1,ARC2,ARC3,N,STATUS)
C
C     E C H _ A R E A D
C
C     Utility routine for ECHARC.  Reads arc lines from files into
C     the ARCn arrays.  The files used are determined by the
C     value of ARCS - see description of ARCTYPE in listing of
C     the ARC routine - and have the extension '.arc'.  The program
C     searches directories in the following order for the arc files:
C     1) The default directory, 2) the Figaro user directory,
C     'FIGARO_PROG_U',  3) The Figaro local directory, 'FIGARO_PROG_L',
C     4) The Figaro system directory, 'FIGARO_PROG_S'
C
C     Parameters -    (">" input,  "<" output)
C
C     (>) ARCS     (Character) Defines the arcs to be used.  Can
C                  contain up to 3 arc names separated by commas,
C                  eg 'HELIUM,NEON,ARGON'.  In this case, the file
C                  HELIUM.ARC will be read into ARC1, NEON.ARC will
C                  be read into ARC2, and ARGON.ARC will be read
C                  into ARC3.  ARCS can also be blank, or 'NONE'.
C     (>) NLARCS   (Integer) The dimension of the ARCn arrays.
C     (>) LU       (Integer) A logical unit number that can be used
C                  by the subroutine.  (Closed on exit.)
C     (<) ARC1     (Real array ARC1(NLARCS)) The wavelengths held
C                  in the first arc file are read into the elements
C                  of ARC1.  The first unused element of the array
C                  (if any) is set to 0.
C     (<) ARC2     (Real array ARC2(NLARCS)). Like ARC1, but for the
C                  second arc file.
C     (<) ARC3     (Real array ARC3(NLARCS)). Like ARC1, but for the
C                  third arc file.
C     (<) N        (Integer)  The total sum of all lines in the three
C                  ARCn files.
C     (<) STATUS   Returns a status code.  0 => OK,
C                  non-zero => a serious error.  Error messages will
C                  be output via calls to PAR_WRUSER.
C
C     Functions / subroutines used -
C
C     ICH_DELIM    (ICH_ package) Look for next char in a given list.
C     ICH_VERIF    ( "     "    ) Look for char not in a given list.
C     ICH_NUMBR    ( "     "    ) Decode a free format number.
C     PAR_WRUSER                      Write message to user.
C
C                       -- originally AREAD:  KS / CIT 13th June 1984
C                       -- parameter N added JKM / ESO 20th Nov. 1987
C                       -- add binary arc file handling (extension ARCBIN)
C                                            WFL / AAO 30th May  1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLARCS,LU,N,STATUS
      REAL ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS)
      CHARACTER*(*) ARCS
C
C     Functions
C
      INTEGER ICH_DELIM,ICH_VERIF,ICH_NUMBR
C
C     Local variables
C
      LOGICAL ARCBIN,ENDFILE,MORE
      INTEGER I,IPTR,ISTAT,LPTR,NARC,NEXT
      REAL VALUE
      CHARACTER CARD*80
      CHARACTER STRING*80
C
C     Clear each ARC array
C
      DO I=1,NLARCS
         ARC1(I)=0.
         ARC2(I)=0.
         ARC3(I)=0.
      END DO
C
C     Initialise pointer to ARCS
C
      N=0
      IPTR=1
      NARC=1
C
      MORE=(ARCS.NE.' ').AND.(ARCS.NE.'NONE')
      DO WHILE (MORE)
C
C        Look for the name of an arc type in ARCS (use .ARCBIN if it exists
C        and .ARC if it doesn't - search for .ARCBIN everywhere first and
C        then for .ARC everywhere)
C
         LPTR=ICH_DELIM(ARCS,IPTR,' ,')-1
         IF (LPTR.LE.0) LPTR=LEN(ARCS)
         IF (LPTR.GE.IPTR) THEN
C
C           We have a name, try to open the file
C
            ARCBIN = .FALSE.
            CALL FIG_OPFILE(ARCS(IPTR:LPTR),'arc',LU,STATUS)
            IF (STATUS.NE.0)  GO TO 520
C
C           ASCII file. Any blank lines and lines beginning with "*" are
C           comments. All others must begin with valid numbers.
C
               IPTR=1
               ENDFILE=.FALSE.
               DO WHILE (.NOT.ENDFILE)
                  READ (LU,'(A)',ERR=510,END=320) CARD
C
C              Check for a comment.  If not try to decode.
C
                  IF (CARD(1:1).NE.'*') THEN
                     ISTAT=ICH_NUMBR(CARD,1,' ;,',VALUE,NEXT)
                     IF (ISTAT.EQ.0) THEN
C
C                       Valid number.  Put in the correct array.
C
                        IF (NARC.EQ.1) THEN
                           ARC1(IPTR)=VALUE
                        ELSE IF (NARC.EQ.2) THEN
                           ARC2(IPTR)=VALUE
                        ELSE IF (NARC.EQ.3) THEN
                           ARC3(IPTR)=VALUE
                        END IF
C
C                       Bump array ponter, total line counter
C
                        IF (IPTR.LT.NLARCS) THEN
                           N=N+1
                           IPTR=IPTR+1
                        ELSE
                           CALL PAR_WRUSER('Too many arc lines in file',
     :                                                           STATUS)
                           CALL PAR_WRUSER('Reading terminated.',STATUS)
                           STRING='File '//ARCS(IPTR:LPTR)
                           CALL PAR_WRUSER(STRING,STATUS)
                           ENDFILE=.TRUE.
                        END IF
                     ELSE IF (ISTAT.GT.0) THEN
C
C                       Not a valid number
C
                        CALL PAR_WRUSER('Bad record in line list file',
     :                                                          STATUS)
                        CALL PAR_WRUSER(CARD(1:64),STATUS)
                        STRING='File '//ARCS(IPTR:LPTR)
                        CALL PAR_WRUSER(STRING,STATUS)
                        ENDFILE=.TRUE.
                     END IF
                  END IF
               END DO
C
  320          CONTINUE
               CLOSE (UNIT=LU)
         END IF
C
C        Position on next file name in ARCS
C
         IF (LPTR.GE.LEN(ARCS)) THEN
            MORE=.FALSE.
         ELSE
            IPTR=ICH_VERIF(ARCS,LPTR+1,', ')
            MORE=IPTR.NE.0
         END IF
         IF (MORE) THEN
            NARC=NARC+1
            IF (NARC.GT.3) THEN
               CALL PAR_WRUSER('Too many arc types specified',STATUS)
               MORE=.FALSE.
            END IF
         END IF
      END DO
C
C     Normal end
C
      STATUS=0
      GO TO 600
C
C     I/O error from file.  Treat this as a serious error.
C
  510 CONTINUE
      STRING='I/O error from arc file '//ARCS(IPTR:LPTR)
      CALL PAR_WRUSER(STRING,STATUS)
      GO TO 530
C
C     File opening failure.  Treat this as a serious error.
C
  520 CONTINUE
      STRING='Failed to open arc file '//ARCS(IPTR:LPTR)
      CALL PAR_WRUSER(STRING,STATUS)
  530 CONTINUE
      STATUS=1
C
  600 CONTINUE
      RETURN
      END
