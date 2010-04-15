*   datasort.f
*
*   22-September-1994: original release (JACH::KEVIN)
*
*   sort ASCII file of ukirt data listing
*
      BYTE X(131), XMIN(131)
      VIRTUAL IBUF(2500,131), INDEX (2500)
      BYTE IBUF
      INTEGER NC
      INTEGER I
      INTEGER J
      INTEGER JJ
      INTEGER K
      INTEGER M
      INTEGER NN
      INTEGER NMAX

      DATA NC/131/

      OPEN ( UNIT=1, NAME='temp.out', TYPE='OLD', ERR=900 )

      OPEN ( UNIT=2, NAME='ukirt_data.out', TYPE='NEW', ERR=900 )

*     Read in all of input file

      I = 0
50    READ ( 1, 53, END=60, ERR=910 ) X
53    FORMAT ( 131A1)

      I = I + 1

      DO J = 1,NC
        IBUF(I,J) = X(J)
      ENDDO

      GOTO 50

60    CLOSE ( UNIT=1, ERR=200 )

      NMAX = I

      DO I = 1,NC
        XMIN(I) = '~'	! small ASCII value
      ENDDO

      DO I = 1,NMAX
        INDEX(I) = 0
      ENDDO


      DO J = 1,3
        WRITE ( 2, 153, ERR = 920 ) (IBUF(J,I), I=1,NC)
      ENDDO

      DO 170 NN = 4,NMAX
        DO 150 J = 4,NMAX
        IF ( INDEX(J) .NE. 0 ) GOTO 150

        DO 140 I = 1,5
          IF ( IBUF(J,I) .GT. XMIN(I) ) GOTO 150
          IF ( IBUF(J,I) .EQ. XMIN(I) ) GOTO 140

            DO 130 K = 1,NC
              XMIN(K) = IBUF(J,K)
130         CONTINUE
          JJ = J
          GOTO 150
140       CONTINUE
150     CONTINUE

        WRITE ( 2, 153, ERR=920 ) (XMIN(N), N= 1,NC)
153     FORMAT ( ' ', 131A1 )
        INDEX(JJ) = 1
        DO 160 M = 1,NC
          XMIN(M) = '~'
160     CONTINUE
170   CONTINUE

200   CLOSE ( UNIT=2, ERR=930 )
      GOTO 999

*     Error messages

900   WRITE ( 6, 903 )
903   FORMAT ( ' Error opening file' )
      GOTO 999

910   WRITE ( 6, 913 )
913   FORMAT ( ' Error accessing file temp' )
      CLOSE ( UNIT=1, ERR=930 )
      GOTO 999

920   WRITE ( 6, 923 )
923   FORMAT ( ' Error writing to file ukirt_data.out' )
      CLOSE ( UNIT=2, ERR=930 )
      GOTO 999

930   WRITE ( 6, 933 )
933   FORMAT ( ' Error closing file' )

999   CONTINUE

      END
