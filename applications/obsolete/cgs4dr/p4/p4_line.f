*+  P4_LINE - plot a line of data points with quality and errors
      SUBROUTINE P4_LINE (DIM, ISTART, IEND, AXIS, DATA, ERRDATA,
     :   QDATA, ERRORS, QUALITY, XSHIFT, YSHIFT, STATUS)
*    Description :
*    Invocation :
*     CALL P4_LINE (DIM, ISTART, IEND, AXIS, DATA, ERRDATA, QDATA,
*    :   ERRORS, QUALITY, XSHIFT, YSHIFT, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     25-Oct-1989: History added. The use of PGMOVE and PGDRAW to
*                  construct lines from many short vectors was
*                  inefficient and exacerbated a bug in GKS/UIS.
*                  Modified so that PGLINE is used instead       (SMB)
*      1-Nov-1989: Modified to test quality against a BYTE
*                  variable, to prevent unnecessary type
*                  conversion.                                   (SMB)
*     20-Feb-1990: Made to calculate X and Y positions only once,
*                  for efficiency. Also, do not add XSHIFT and
*                  YSHIFT every time if both are zero.           (SMB)
*     16-May-1990: Fix of bug revealed by array bounds checks:
*                  In the DO WHILE loops the bounds of QDATA
*                  was exceeded if I>IFIN. Tests reversed.       (SMB)
*     18-Feb-1993: Tidy code                                     (PND)
*      4-Aug-1994: Convert to I-task for Unix port               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      LOGICAL ERRORS                      ! T if error array meaningful
      LOGICAL QUALITY                     ! T if quality meaningful
      INTEGER DIM                         ! dimension of plot array
      INTEGER ISTART                      ! start position in array
      INTEGER IEND                        ! end position
      REAL AXIS (DIM)                     ! axis information
      REAL DATA (DIM)                     ! data values
      REAL ERRDATA (DIM)                  ! associated errors
      BYTE QDATA (DIM)                    ! associated quality (can be dummy
*                                         !  array if QUALITY is .FALSE.)
      REAL XSHIFT                         ! number added to axis before
*                                         !  plotting
      REAL YSHIFT                         ! number added to data before
*                                         !  plotting
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER BUFFSIZ                     ! Size of polyline buffer
      PARAMETER ( BUFFSIZ = 512 )
      BYTE GOOD                           ! Quality value for 'good'
      PARAMETER ( GOOD = 0 )
*    Local variables :
      INTEGER I                           ! DO loop
      INTEGER IBEG, IFIN                  ! safe limits of array
      INTEGER ITEMP
      INTEGER
     :  NLINES                            ! Number of polyline elements
      REAL
     :  XLINE( BUFFSIZ ),                 ! X coordinate polyline buffer
     :  YLINE( BUFFSIZ ),                 ! Y coordinate polyline buffer
     :  XPOS,                             ! X position
     :  YPOS                              ! Y position
*-

      IF (STATUS .NE. SAI__OK) RETURN

*    Check endpoints
      IF (ISTART .LT. 1) THEN
         IBEG = 1
      ELSE IF (ISTART .GT. DIM) THEN
         IBEG = DIM
      ELSE
         IBEG = ISTART
      ENDIF

      IF (IEND .GT. DIM) THEN
         IFIN = DIM
      ELSE IF (IEND .LT. 1) THEN
         IFIN = 1
      ELSE
         IFIN = IEND
      ENDIF

      IF (IFIN .LT. IBEG) THEN
         ITEMP = IBEG
         IBEG = IFIN
         IFIN = ITEMP
      ENDIF

*    now do the plotting for case where quality counts
      IF (QUALITY) THEN

*      Check whether both XSHIFT and YSHIFT are zero.
*      If so, there is no point adding them to the data for every point.
         IF ( ( ABS(XSHIFT) .LT. 1.0E-20 ) .AND.
     :        ( ABS(YSHIFT) .LT. 1.0E-20 ) ) THEN

*         XSHIFT and YSHIFT are zero.
*          plot data points first
            IF (IBEG .NE. IFIN) THEN

*            Initialise the number of polyline elements
               NLINES = 0
               I = IBEG

*            Scan through the points in the array to be plotted
               DO WHILE (I.LE.IFIN)

*               Work through each contiguous set of points for which quality
*               is good.
                  DO WHILE ( (I.LE.IFIN) .AND. (QDATA(I).EQ.GOOD) )

*                  Add this point to the polyline buffer
                     NLINES = NLINES + 1
                     XLINE(NLINES) = AXIS(I)
                     YLINE(NLINES) = DATA(I)

*                  If the buffer has been filled, draw this polyline
*                  and start filling again at the beginning.
                     IF ( NLINES .GE. BUFFSIZ ) THEN

                        CALL PGLINE( NLINES, XLINE, YLINE)
                        XLINE(1) = XLINE(NLINES)
                        YLINE(1) = XLINE(NLINES)
                        NLINES = 1
                     END IF

                     I = I + 1
                  END DO

*               If there is a polyline of at least 2 points waiting in the
*               buffer, plot it and flush the buffer. If there is only one
*               point just draw a dot.
                  IF ( NLINES .GE. 2 ) THEN

                     CALL PGLINE( NLINES, XLINE, YLINE )
                     NLINES = 0
                  ELSE IF ( NLINES .EQ. 1 ) THEN

                     CALL PGPOINT( 1, XLINE(1), YLINE(1), 1 )
                     NLINES = 0
                  END IF

*               Ignore each contiguous set of points for which the quality
*               is bad. (These do not need to be plotted)
                  DO WHILE ( (I.LE.IFIN) .AND. (QDATA(I).NE.GOOD) )

                     I = I + 1
                  END DO
               END DO

            ELSE

*             plot marker at position of single point
               IF (QDATA(IBEG) .EQ. GOOD) THEN

                  CALL PGPOINT (1, AXIS(IBEG), DATA(IBEG), 5)
               ENDIF
            ENDIF

*          now plot error bars, faster without PGERRY
            IF (ERRORS) THEN

               DO I = IBEG, IFIN

                  IF (QDATA(I) .EQ. GOOD) THEN

                     CALL PGMOVE ( AXIS(I), DATA(I)+ERRDATA(I) )
                     CALL PGDRAW ( AXIS(I), DATA(I)-ERRDATA(I) )
                  ENDIF
               END DO
            ENDIF
         ELSE

*         XSHIFT and YSHIFT are not zero.
*          plot data points first
            IF (IBEG .NE. IFIN) THEN

*            Initialise the number of polyline elements
               NLINES = 0
               I = IBEG

*            Scan through the points in the array to be plotted
               DO WHILE (I.LE.IFIN)

*               Work through each contiguous set of points for which quality
*               is good.
                  DO WHILE ( (I.LE.IFIN) .AND. (QDATA(I).EQ.GOOD) )

*                  Add this point to the polyline buffer
                     NLINES = NLINES + 1
                     XLINE(NLINES) = AXIS(I) + XSHIFT
                     YLINE(NLINES) = DATA(I) + YSHIFT

*                  If the buffer has been filled, draw this polyline
*                  and start filling again at the beginning.
                     IF ( NLINES .GE. BUFFSIZ ) THEN

                        CALL PGLINE( NLINES, XLINE, YLINE)
                        XLINE(1) = XLINE(NLINES)
                        YLINE(1) = XLINE(NLINES)
                        NLINES = 1
                     END IF

                     I = I + 1
                  END DO

*               If there is a polyline of at least 2 points waiting in the
*               buffer, plot it and flush the buffer. If there is only one
*               point just draw a dot.
                  IF ( NLINES .GE. 2 ) THEN

                     CALL PGLINE( NLINES, XLINE, YLINE )
                     NLINES = 0
                  ELSE IF ( NLINES .EQ. 1 ) THEN

                     CALL PGPOINT( 1, XLINE(1), YLINE(1), 1 )
                     NLINES = 0
                  END IF

*               Ignore each contiguous set of points for which the quality
*               is bad. (These do not need to be plotted)
                  DO WHILE ( (I.LE.IFIN) .AND. (QDATA(I).NE.GOOD) )

                     I = I + 1
                  END DO
               END DO

            ELSE

*             plot marker at position of single point
               IF (QDATA(IBEG) .EQ. GOOD) THEN

                  CALL PGPOINT( 1, AXIS(IBEG)+XSHIFT,
     :              DATA(IBEG)+YSHIFT, 5 )
               ENDIF
            ENDIF

*          now plot error bars, faster without PGERRY
            IF (ERRORS) THEN

               DO I = IBEG, IFIN

                  IF (QDATA(I) .EQ. GOOD) THEN

                     XPOS = AXIS(I) + XSHIFT
                     YPOS = DATA(I) + YSHIFT
                     CALL PGMOVE ( XPOS, YPOS+ERRDATA(I) )
                     CALL PGDRAW ( XPOS, YPOS-ERRDATA(I) )
                  ENDIF
               END DO
            ENDIF
         END IF
      ELSE

*      Now the same for where quality is not known
*      Check whether both XSHIFT and YSHIFT are zero.
*      If so, there is no point adding them to the data for every point.
         IF ( ( ABS(XSHIFT) .LT. 1.0E-20 ) .AND.
     :        ( ABS(YSHIFT) .LT. 1.0E-20 ) ) THEN

*         XSHIFT and YSHIFT are zero.
*          plot data points first
            IF (IBEG .NE. IFIN) THEN

*            Load up the polyline buffer with all the lines to be
*            plotted, and then plot them all
               NLINES = 0
               DO I = IBEG, IFIN

*               Add this point to the polyline buffer
                  NLINES = NLINES + 1
                  XLINE(NLINES) = AXIS(I)
                  YLINE(NLINES) = DATA(I)

*               If the buffer has been filled, draw this polyline
*               and start filling again at the beginning.
                  IF ( NLINES .GE. BUFFSIZ ) THEN

                     CALL PGLINE( NLINES, XLINE, YLINE)
                     XLINE(1) = XLINE(NLINES)
                     YLINE(1) = XLINE(NLINES)
                     NLINES = 1
                  END IF
               END DO

*            If there is a polyline of at least 2 points waiting in the
*            buffer, plot it and flush the buffer. If there is only one
*            point just draw a dot.
               IF ( NLINES .GE. 2 ) THEN

                  CALL PGLINE( NLINES, XLINE, YLINE )
                  NLINES = 0
               ELSE IF ( NLINES .EQ. 1 ) THEN

                  CALL PGPOINT( 1, XLINE(1), YLINE(1), 1 )
                  NLINES = 0
               END IF
            ELSE

*             plot marker at position of single point
               CALL PGPOINT (1, AXIS(I), DATA(I), 5)
            ENDIF

*          now plot error bars, faster without PGERRY
            IF (ERRORS) THEN

               DO I = IBEG, IFIN

                  CALL PGMOVE( AXIS(I), DATA(I)+ERRDATA(I) )
                  CALL PGDRAW( AXIS(I), DATA(I)-ERRDATA(I) )
               END DO
            END IF
         ELSE

*         XSHIFT and YSHIFT are not both zero.
*          plot data points first
            IF (IBEG .NE. IFIN) THEN

*            Load up the polyline buffer with all the lines to be
*            plotted, and then plot them all
               NLINES = 0
               DO I = IBEG, IFIN

*               Add this point to the polyline buffer
                  NLINES = NLINES + 1
                  XLINE(NLINES) = AXIS(I) + XSHIFT
                  YLINE(NLINES) = DATA(I) + YSHIFT

*               If the buffer has been filled, draw this polyline
*               and start filling again at the beginning.
                  IF ( NLINES .GE. BUFFSIZ ) THEN

                     CALL PGLINE( NLINES, XLINE, YLINE)
                     XLINE(1) = XLINE(NLINES)
                     YLINE(1) = XLINE(NLINES)
                     NLINES = 1
                  END IF
               END DO

*            If there is a polyline of at least 2 points waiting in the
*            buffer, plot it and flush the buffer. If there is only one
*            point just draw a dot.
               IF ( NLINES .GE. 2 ) THEN

                  CALL PGLINE( NLINES, XLINE, YLINE )
                  NLINES = 0
               ELSE IF ( NLINES .EQ. 1 ) THEN

                  CALL PGPOINT( 1, XLINE(1), YLINE(1), 1 )
                  NLINES = 0
               END IF
            ELSE

*             plot marker at position of single point
               CALL PGPOINT (1, AXIS(I)+XSHIFT, DATA(I)+YSHIFT, 5)
            ENDIF

*          now plot error bars, faster without PGERRY
            IF (ERRORS) THEN

               DO I = IBEG, IFIN

                  XPOS = AXIS(I) + XSHIFT
                  YPOS = DATA(I) + YSHIFT
                  CALL PGMOVE( XPOS, YPOS+ERRDATA(I) )
                  CALL PGDRAW( XPOS, YPOS-ERRDATA(I) )
               END DO
            END IF
         ENDIF
      ENDIF

      END
