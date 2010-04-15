
*+  MOFFDCSUB - calculates d.c. sky offset between an image mosaic pair

      SUBROUTINE MOFFDCSUB ( ARRAYA, DIMSA1, DIMSA2, ARRAYB, DIMSB1,
     :                       DIMSB2, XOFF, YOFF, DCOFF, STATUS )

*    Description :
*
*     This routine calculates the d.c. sky offset between the two images of
*     a mosaic pair. The x,y spatial offset should have been located using
*     MOFFXYSUB, and this routine assumes that the input x,y spatial offset
*     input is correct. The value returned is the offset from the first to
*     second frame i.e. if the first frame has a mean level of 100 in the
*     overlap region, and the second has a mean level of 200 in the overlap
*     region, then the returned d.c. offset is +100.
*
*    Invocation :
*
*     CALL MOFFDCSUB( ARRAYA, DIMSA, ARRAYB, DIMSB, XOFF, YOFF, DCOFF, STATUS )
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
*     XOFF  =  INTEGER( READ )
*         x offset between the first and second image
*     YOFF  =  INTEGER( READ )
*         y offset between the first and second image
*     DCOFF  =  REAL( WRITE )
*         Calculated d.c. sky offset between first and second image
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
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
*     07-01-1988 : First implementation (UKTH::MJM)
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
     :    XOFF,                   ! x offset between first and second image
     :    YOFF                    ! y    "      "      "    "     "     "

      REAL
     :    ARRAYA( DIMSA1, DIMSA2 ),   ! first input image
     :    ARRAYB( DIMSB1, DIMSB2 )    ! second  "     "

*    Export :

      REAL
     :    DCOFF                   ! calculated d.c. sky offset

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    NUMVAL,                 ! number of valid pixels in calculation
     :    I, J, IP, JP            ! counters

      REAL
     :    CURTOT                  ! total sum deviation over overlap region


*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    initialise variables before entering loop round overlap pixels
      CURTOT  =  0.0
      NUMVAL  =  0

*    method of looping around overlap pixels depends upon the relative
*    orientation of the mosaic images - first check for bottom-left to
*    top-right
      IF ( XOFF .GE. 0 .AND. YOFF .GE. 0 ) THEN   ! BL-TR

*       loop round all pixels in overlap region
         DO  J  =  1, DIMSA2 - YOFF
            DO  I  =  1, DIMSA1 - XOFF

*             add current deviation to running total
               CURTOT  =  CURTOT + (ARRAYB(I,J) - ARRAYA(I+XOFF,J+YOFF))

*             increment valid pixel counter
               NUMVAL  =  NUMVAL + 1

            END DO
         END DO

*    else check for top-right to bottom-left
      ELSE IF ( XOFF .LT. 0 .AND. YOFF .LT. 0 ) THEN   ! TR-BL

*    set up dummy counter variables
      IP  =  -XOFF
      JP  =  -YOFF

*       loop round all pixels in overlap region
         DO  J  =  1, DIMSA2 - JP
            DO  I  =  1, DIMSA1 - IP

*             add current deviation to running total
               CURTOT  =  CURTOT + (ARRAYB(I+IP,J+JP) - ARRAYA(I,J))

*             increment valid pixel counter
               NUMVAL  =  NUMVAL + 1

            END DO
         END DO

*    else check for top-left to bottom-right
      ELSE IF ( XOFF .GE. 0 .AND. YOFF .LT. 0 ) THEN   ! TL-BR

*       set up dummy counter
         JP  =  -YOFF

*       loop round all pixels in overlap region
         DO  J  =  1, DIMSA2 - JP
            DO  I  =  1, DIMSA1 - XOFF

*             add current deviation to running total
               CURTOT  =  CURTOT + (ARRAYB(I,J+JP) - ARRAYA(I+XOFF,J))

*             increment valid pixel counter
               NUMVAL  =  NUMVAL + 1

            END DO
         END DO

*    else must be bottom-right to top-left
      ELSE   ! BR-TL

*       set up dummy counter
         IP  =  -XOFF

*       loop round all pixels in overlap region
         DO  J  =  1, DIMSA2 - YOFF
            DO  I  =  1, DIMSA1 - IP

*             add current deviation to running total
               CURTOT  =  CURTOT + (ARRAYB(I+IP,J) - ARRAYA(I,J+YOFF))

*             increment valid pixel counter
               NUMVAL  =  NUMVAL + 1

            END DO
         END DO

*    end of check to see what mosaic orientation we are in
      END IF


*    on completeion check that some valid pixels were done
      IF ( NUMVAL .GT. 0 ) THEN

*       some were so calculate mean offset
         DCOFF  =  CURTOT / NUMVAL

      ELSE

*       none were so set mean offset to large number
         DCOFF  =  1.0E30

      END IF


*    return and end
      END
