      SUBROUTINE  SORTER (WORK, MAXWRK, SORT, HIGH, LOW, INDEX,
     .     MAXSTR, WATCH)
C
C=======================================================================
C
C Subroutine to read in any of the data files created by DAOPHOT and
C sort the stars according to magnitude, position, ID number,
C or OTHER.
C
C              OFFICIAL DAO VERSION:  1991 April 18
C
C Argument
C
C  WATCH (INPUT) governs whether information relating to the progress
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C WATCH is a user-definable optional parameter.
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

C
C Parameters
C
C    MAX is the number of different sorts which are possible
C        (currently 5: by ID number, by X, by Y, by magnitude, and
C         by OTHER [e.g. SHARP, ROUND, CHI, number of iterations]).
C
C MAXSTR is the maximum number of stars permitted in a data file.
C        It is limited by the size of the WORK array in DAOPHOT:
C        WORK must contain 282 bytes per star:  4 bytes each for SORT,
C        INDEX, NLINE1, and, NLINE2, and 133 bytes each for LINE1 and
C        LINE2.
C
C MAXITM is the maximum number of output data per line of an output.
C        (currently 15, realized in PHOTOMETRY)
C
      INTEGER MAXWRK, MAXSTR, MAXITM
      PARAMETER (MAXITM=30)
C
      REAL DATUM(MAXITM), WORK(MAXWRK), SORT(MAXSTR)
      INTEGER LOW(MAXSTR), HIGH(MAXSTR), INDEX(MAXSTR)
C
      REAL ABS
      INTEGER NINT
C
      CHARACTER*133 TEXT
      CHARACTER*30 FILE, SWITCH
      CHARACTER CASE*4, ANSWER*1
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, FRAD
      REAL WATCH, WHICH, FLIP
      INTEGER I, J, K, L, N, NL, NCOL, NROW, ISTAT, ITEMS, NSTAR
      INTEGER NLO, MODE, MIN0
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready.
C
C Find out how the user wants to sort.
C
      WRITE (6,610) MAXITM
  610 FORMAT (//
     .     11X, '   The following sorts are currently possible:'//
     .     11X, '+/- 1  By increasing/decreasing star ID number'//
     .     11X, '+/- 2  By increasing/decreasing  X  coordinate'//
     .     11X, '+/- 3  By increasing/decreasing  Y  coordinate'//
     .     11X, '+/- 4  By increasing/decreasing magnitude'//
     .     11X, '+/- n  By increasing/decreasing OTHER (n <= ',
     .     I2, ')'///)
      CALL GETDAT ('Which do you want?', WHICH, 1)
      IF (WHICH .LE. VAL__MINR) RETURN                 ! CTRL-Z was entered
C
      MODE=NINT(WHICH)
      IF ((ABS(MODE) .LT. 1) .OR. (ABS(MODE) .GT. MAXITM))
     .     RETURN                                     ! Invalid response
C
      FLIP=FLOAT(MODE/ABS(MODE))
      MODE=ABS(MODE)
C
C Get input file name, open the file, and read its header.
C
      FILE=' '
  950 CALL GETNAM ('Input file name:', FILE)
      IF ((FILE .EQ. 'END OF FILE') .OR. (FILE .EQ. 'GIVE UP')) RETURN
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//FILE)
         FILE = 'GIVE UP'
         GO TO 950
      END IF
C
C Get output file name and open the file.
C
      FILE = SWITCH(FILE, CASE('.srt'))
  960 CALL GETNAM ('Output file name:', FILE)
      IF ((FILE .EQ. 'END OF FILE') .OR. (FILE .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         RETURN
      END IF
C
      CALL OUTFIL (3, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//FILE)
         FILE = 'GIVE UP'
         GO TO 960
      END IF
C
      CALL GETYN ('Do you want the stars renumbered?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL CLFILE (2)
      END IF
C
      NL=-1
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
      IF (NL .LE. 0) GO TO 1010                     ! No header in input
      IF (NL .GT. 3) NL=1
C
C Copy input file header to output file.
C
      ITEMS=6
      IF (FRAD .GT. 0.001) ITEMS=7
      CALL WRHEAD (3, NL, NCOL, NROW, ITEMS, LOBAD, HIBAD, THRESH,
     .     AP1, PHPADU, READNS, FRAD)
C
 1010 CONTINUE
      IF (WATCH .LT. 0.5) GO TO 1020
      IF (MODE .LE. 4) WRITE (6,611)
  611 FORMAT (/22X, 'STAR', 6X, 'X', 8X, 'Y', 4X, 'MAG.(1)')
      IF (MODE .GE. 5) WRITE (6,612) MODE
  612 FORMAT (/17X, 'STAR', 6X, 'X', 8X, 'Y', 4X, 'MAG.(1)',
     .     3X, 'ITEM', I3)
 1020 CONTINUE
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the input file in line by line, verbatim.  Pack the contents
C of the line into the REAL array WORK, keeping track of the
C lower and upper limits in the arrays LINELO and LINEHI.  At the
C same time, extract the the particular datum
C according to which we wish to sort.  Sort these data.  Then write
C the file out again, line by line, verbatim, but in the new order.
C
      I=0
      NLO = 0
C
 2000 CALL RDCHAR (2, TEXT, N, ISTAT)
      IF (ISTAT .EQ. 1) GO TO 2100             ! END OF FILE
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to read input file.')
         CALL CLFILE (2)
         RETURN
      END IF
C
      IF (N .LE. 1) GO TO 2000                 ! Blank line encountered
      READ (TEXT(2:N), 901, IOSTAT=ISTAT)
     .     (DATUM(J), J=1,MIN0(15,MODE))
  901 FORMAT (F5.0, 14F9.0)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to read data from input file.')
         CALL CLFILE (2)
         RETURN
      END IF
C
      I = I+1
      IF (I .GT. MAXSTR) THEN
         WRITE (TEXT,77) I-1
   77    FORMAT (I10, ' stars is all I have room for.  Sorry!')
         CALL STUPID (TEXT(1:50))
         WRITE (6,78)
   78    FORMAT ('Increase the MS parameter and try again')
         CALL CLFILE (2)
         RETURN
      END IF
      LOW(I) = NLO+1
      HIGH(I) = NLO + (N-2)/4 + 1
      IF (HIGH(I) .GT. MAXWRK) THEN
         WRITE (TEXT,77) I-1
         CALL STUPID (TEXT(1:50))
         CALL CLFILE (2)
         RETURN
      END IF
C
      NLO = HIGH(I)
      READ (TEXT(2:N),66) (WORK(J), J=LOW(I),NLO)
   66 FORMAT (33A4)
C
      IF (NL .EQ. 2) THEN
 2012    CALL RDCHAR (2, TEXT, N, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Unable to read input file.')
            CALL CLFILE (2)
            RETURN
         END IF
C
         IF (N .LE. 1) GO TO 2012
         IF (MODE .GT. 15) THEN
            READ (TEXT(2:N), 902, IOSTAT=ISTAT)
     .        (DATUM(J), J=16,MODE)
  902       FORMAT (F12.3, 2F6.3, 12(F8.3, 1X))
            IF (ISTAT .NE. 0) THEN
               CALL STUPID ('Unable to read data from input file.')
               CALL CLFILE (2)
               RETURN
            END IF
         END IF
C
         HIGH(I) = HIGH(I) + (N-2)/4+1
         IF (HIGH(I) .GT. MAXWRK) THEN
            WRITE (TEXT,77) I-1
            CALL STUPID (TEXT(1:50))
            CALL CLFILE (2)
            RETURN
         END IF
         READ (TEXT(2:N),66) (WORK(J), J=NLO+1,HIGH(I))
         NLO = HIGH(I)
      END IF
C
      SORT(I)=FLIP*DATUM(MODE)
      GO TO 2000                                     ! End of input loop
C
C Perform the sort.
C
 2100 NSTAR=I                                        ! Number of stars
      CALL CLFILE (2)
      IF (NSTAR .LE. 0) THEN
         CALL STUPID ('No stars in input file.')
         RETURN
      END IF
      CALL QUICK (SORT, NSTAR, INDEX)
C
C The vector SORT is now arranged in order of increasing or decreasing
C whatever, and the vector INDEX now contains the ordinal position in
C the input file of the stars, in order of increasing or decreasing
C whatever.
C
C Now write the data out again.
C
      DO 2110 I=1,NSTAR
      J = INDEX(I)
      IF (NL .NE. 2) THEN
         NLO = HIGH(J) - LOW(J) + 1
         K = 4*NLO+1
         WRITE (TEXT(1:K),67) (WORK(L), L=LOW(J),HIGH(J))
   67    FORMAT (1X, 33A4)
C
C If ID numbers are to be changed, insert the new ID into the text.
C
         IF (ANSWER .EQ. 'Y') WRITE (TEXT(2:6),68) I
   68    FORMAT (I5)
         WRITE (3,320) TEXT(1:K)
  320    FORMAT (A)
         IF (WATCH .LT. 0.5) GO TO 2110
         IF (MODE .LE. 4) THEN
            WRITE (6,620) TEXT(2:33)
  620       FORMAT (21X, A32)
         ELSE
            WRITE (6,621) TEXT(2:33), FLIP*SORT(I)
  621       FORMAT (16X, A32, 1X, F9.3)
         END IF
      ELSE
         NLO = (HIGH(J) - LOW(J) + 1)/2
         K = 4*NLO+1
         WRITE (TEXT(1:K),67) (WORK(L), L=LOW(J),LOW(J)+NLO-1)
         IF (ANSWER .EQ. 'Y') WRITE (TEXT(2:6),68) I
         WRITE (3,320) TEXT(1:K)
         IF (WATCH .GE. 0.5) THEN
            IF (MODE .LE. 4) THEN
               WRITE (6,620) TEXT(2:33)
            ELSE
               WRITE (6,621) TEXT(2:33), FLIP*SORT(I)
            END IF
         END IF
         WRITE (TEXT(1:K),67) (WORK(L), L=LOW(J)+NLO,HIGH(J))
         WRITE (3,320) TEXT(1:K)
         WRITE (3,320) ' '
      END IF
C
 2110 CONTINUE                                      ! End of output loop
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      CALL CLFILE (3)
      RETURN
C
      END!
