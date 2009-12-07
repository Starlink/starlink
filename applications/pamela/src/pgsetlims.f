      SUBROUTINE PGSETLIMS( XLFT, XRGT, YBOT, YTOP, IFAIL )
C     
C     INTERACTIVE SELECTION OF PLOT LIMITS
C     
C     Input:
C     XLFT, XRGT, YBOT, YTOP  = DEFAULT PLOT LIMITS
C     Output:
C     XLFT, XRGT, YBOT, YTOP  = DESIRED PLOT LIMITS
C     IFAIL   = 0 IF SUCCESSFUL
C     = 1 IF PLOT ABORT REQUESTED
C     
C     AUG 1984 BY KEITH HORNE AT IOA
C     
      CHARACTER*80 REPLY
C     
 10   WRITE(*,*)' '
      WRITE(*,*) 'Enter plot limits (Left,Right, Bottom,Top) :'
      WRITE(*,'(1X,1PG11.4,1X,1PG11.4,1X,1PG11.4,1X,1PG11.4)')
     &     XLFT, XRGT, YBOT, YTOP
      WRITE(*,*) '0, 0 for old pair, A(bort), <CR> to proceed'
      READ(*,'(A)') REPLY
      CALL UPPER_CASE(REPLY)
C     
C     NORMAL RETURN
C     
      IF( REPLY.EQ.' ') THEN
         IFAIL = 0
         RETURN
C     
C     ABORT PLOT
C     
      ELSE IF( REPLY.EQ.'A') THEN
         PRINT *,'** PLOT ABORTED'
         IFAIL = 1
         RETURN
C     
C     CHANGE PLOT LIMITS
C     
      ELSE
         READ(REPLY,*,IOSTAT=IFAIL) XLFT1, XRGT1, YBOT1, YTOP1
         IF(IFAIL.NE.0) GOTO 10
         IF(XLFT1.NE.XRGT1) THEN
            XLFT = XLFT1
            XRGT = XRGT1
         END IF
         IF(YBOT1.NE.YTOP1) THEN
            YBOT = YBOT1
            YTOP = YTOP1
         END IF
      END IF
      GOTO  10
      
      END
      
