
*+  MOFFXYSUB - calculates best fit x,y offset for a mosaic image pair

      SUBROUTINE MOFFXYSUB ( ARRAYA, DIMSA1, DIMSA2, ARRAYB, DIMSB1,
     :                       DIMSB2, NXOFF, NYOFF, BOXSIZE, BESTXOFF,
     :                       BESTYOFF, STATUS )

*    Description :
*
*     This subroutine calculates the best fit x,y offset for a mosaic image
*     pair. It does so as follows. For a given range of possible x,y offset
*     positions, the mean square deviation between the values in one array
*     and those in the other (supposedly) looking at the same piece of sky
*     is calculated for all the pixels in the overlap region. The offset
*     position giving the lowest mean square deviation is deemed to be the
*     correct one.
*
*    Invocation :
*
*     CALL MOFFXYSUB( ARRAYA, DIMSA, ARRAYB, DIMSB, NXOFF, NYOFF, BOXSIZE,
*                     BESTXOFF, BESTYOFF, STATUS )
*
*    Parameters :
*
*     ARRAYA( DIMSA(1), DIMSA(2) )  =  REAL( READ )
*         First input image
*     DIMSA( 2 )  =  INTEGER( READ )
*         Dimensions of first input image
*     ARRAYB( DIMSB(1), DIMSB(2) )  =  REAL( READ )
*         Second input image
*     DIMSB( 2 )  =  INTEGER( READ )
*         Dimensions of second input image
*     NXOFF  =  INTEGER( READ )
*         Nominal x offset from first to second image
*     NYOFF  =  INTEGER( READ )
*         Nominal y offset from first to second image
*     BOXSIZE  =  INTEGER( READ )
*         Size of offset box to be searched for best fit
*     BESTXOFF  =  INTEGER( WRITE )
*         Best fit x offset
*     BESTYOFF  =  INTEGER( WRITE )
*         Best fit y offset
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     06-01-1988 : First implementation (UKTH::MJM)
*     15-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMSA1, DIMSA2,             ! dimensions of first input image
     :    DIMSB1, DIMSB2,             !      "      " second  "     "
     :    NXOFF,                  ! nominal x offset from first to second
     :    NYOFF,                  !    "    y    "     "    "    "    "
     :    BOXSIZE                 ! size of offset box to be searched

      REAL
     :    ARRAYA( DIMSA1, DIMSA2 ), ! first input image
     :    ARRAYB( DIMSB1, DIMSB2 )  ! second  "     "

*    Export :

      INTEGER
     :    BESTXOFF,               ! best fit x offset
     :    BESTYOFF                !   "   "  y    "

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    HALFBOX,                ! half size of box to be searched for best fit
     :    MINXOFF,                ! minimum x offset
     :    MINYOFF,                !    "    y    "
     :    MAXXOFF,                ! maximum x    "
     :    MAXYOFF,                !    "    y    "
     :    NUMVAL,                 ! number of valid pixels in calculation
     :    I, J, II, JJ, IP, JP    ! counters

      REAL
     :    MINMSQ,                 ! minimum mean square deviation found so far
     :    CURMSQ                  ! value of current mean square deviation

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    initialise search parameters
      HALFBOX  =  ( BOXSIZE - 1 ) / 2
      MINXOFF  =  NXOFF - HALFBOX
      MINYOFF  =  NYOFF - HALFBOX
      MAXXOFF  =  NXOFF + HALFBOX
      MAXYOFF  =  NYOFF + HALFBOX
      BESTXOFF =  NXOFF
      BESTYOFF =  NYOFF
      MINMSQ   =  1.0E30

*    loop round all possible combinations of the likely offset
      DO  J  =  MINYOFF, MAXYOFF
         DO  I  =  MINXOFF, MAXXOFF

*          initialise search parameters for current offset position
            CURMSQ  =  0.0
            NUMVAL  =  0

*          method of looping around overlap pixels depends upon the relative
*          orientation of the mosaic images - first check for bottom-left to
*          top-right
            IF ( I .GE. 0 .AND. J .GE. 0 ) THEN   ! BL-TR

*             loop round all pixels in overlap region
               DO  JJ  =  1, DIMSA2 - J
                  DO  II  =  1, DIMSA1 - I

*                   add current square deviation to running total
                     CURMSQ  =  CURMSQ +
     :                      ((ARRAYB(II,JJ) - ARRAYA(II+I,JJ+J))**2)

*                   increment valid pixel counter
                     NUMVAL  =  NUMVAL + 1

                  END DO
               END DO

*          else check for top-right to bottom-left
            ELSE IF ( I .LT. 0 .AND. J .LT. 0 ) THEN   ! TR-BL

*             set up dummy counter variables
               IP  =  -I
               JP  =  -J

*             loop round all pixels in overlap region
               DO  JJ  =  1, DIMSA2 - JP
                  DO  II  =  1, DIMSA1 - IP

*                   add current square deviation to running total
                     CURMSQ  =  CURMSQ +
     :                    ((ARRAYB(II+IP,JJ+JP) - ARRAYA(II,JJ))**2)

*                   increment valid pixel counter
                     NUMVAL  =  NUMVAL + 1

                  END DO
               END DO

*          else check for top-left to bottom-right
            ELSE IF ( I .GE. 0 .AND. J .LT. 0 ) THEN   ! TL-BR

*             set up dummy counter
               JP  =  -J

*             loop round all pixels in overlap region
               DO  JJ  =  1, DIMSA2 - JP
                  DO  II  =  1, DIMSA1 - I

*                   add current square deviation to running total
                     CURMSQ  =  CURMSQ +
     :                     ((ARRAYB(II,JJ+JP) - ARRAYA(II+I,JJ))**2)

*                   increment valid pixel counter
                     NUMVAL  =  NUMVAL + 1

                  END DO
               END DO

*          else must be bottom-right to top-left
            ELSE   ! BR-TL

*             set up dummy counter
               IP  =  -I

*             loop round all pixels in overlap region
               DO  JJ  =  1, DIMSA2 - J
                  DO  II  =  1, DIMSA1 - IP

*                   add current square deviation to running total
                     CURMSQ  =  CURMSQ +
     :                     ((ARRAYB(II+IP,JJ) - ARRAYA(II,JJ+J))**2)

*                   increment valid pixel counter
                     NUMVAL  =  NUMVAL + 1

                  END DO
               END DO

*          end of check to see what mosaic orientation we are in
            END IF


*          now see if any valid pixel pairs were found
            IF ( NUMVAL .GT. 0 ) THEN

*             some were so calculate the mean square deviation
               CURMSQ  =  CURMSQ / REAL( NUMVAL )

            ELSE

*             none were so set mean to high value
               CURMSQ  =  1.0E30

            END IF

*          check to see if the current mean square deviation is the smallest
*          found so far
            IF ( CURMSQ .LT. MINMSQ ) THEN

*             it is so update best x,y offsets
               MINMSQ    =  CURMSQ
               BESTXOFF  =  I
               BESTYOFF  =  J

            END IF

*    end of loop round all possible offset positions
         END DO
      END DO


*    return and end
      END
