      SUBROUTINE TGRIDA(IER)
C
C LATEST REVISION        July, 1985
C
C PURPOSE                To provide a simple demonstration of
C                        of all the entry points of the GRIDAL
C                        package.  Sixteen plots are produced.
C
C USAGE                  CALL TGRIDA (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                          GRIDAL TEST SUCCESSFUL  . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        id printed on unit 6.  In addition, 16
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        these plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN
C
C ALGORITHM              All of the entries of the GRIDAL package
C                        are invoked (GRID, GRIDL, PERIM, PERIML,
C                        HALFAX, TICK4, LABMOD, and GRIDAL) to
C                        produce plots.  The GRIDAL entry is called
C                        nine times, once for each legal value of
C                        IGPH.
C
C PORTABILITY            FORTRAN 77
C
C
C
      CHARACTER*2 BUFF
C
C SET NORMALIZATION TRANSFORMATION TO 1
C
      CALL GSWN(1,0.,1.,0.,1.)
      CALL GSVP(1,.2,.8,.2,.8)
C
C GRID
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR GRID',2,0,0)
      CALL GSELNT(1)
      CALL GRID(5,2,6,3)
      CALL FRAME
C
C GRIDL
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR GRIDL',2,0,0)
      CALL GSELNT(1)
      CALL GRIDL(5,2,6,3)
      CALL FRAME
C
C PERIM
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR PERIM',2,0,0)
      CALL GSELNT(1)
      CALL PERIM(5,2,6,3)
      CALL FRAME
C
C PERIML
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR PERIML',2,0,0)
      CALL GSELNT(1)
      CALL PERIML(5,2,6,3)
      CALL FRAME
C
C HALFAX
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR HALFAX',2,0,0)
      CALL GSELNT(1)
      CALL HALFAX(5,2,6,3,.3,.5,0,0)
      CALL FRAME
C
C TICK4
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR TICK4',2,0,0)
      CALL GSELNT(1)
      CALL TICK4(150,50,150,50)
      CALL PERIM(5,2,6,3)
      CALL FRAME
      CALL TICK4(12,8,12,8)
C
C LABMOD
C
      CALL GSELNT(0)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR LABMOD',2,0,0)
      CALL GSELNT(1)
      CALL LABMOD('(E10.2)','(F4.2)',10,4,15,15,0,0,0)
      CALL HALFAX(2,1,10,1,0.,0.,1,1)
      CALL FRAME
      CALL LABMOD('(E10.3)','(E10.3)',10,10,0,0,0,0,0)
C
C GRIDAL
C
      DO 100 I=0,10
      IF (I .EQ. 3 .OR. I .EQ. 7) GOTO 100
      IGPH = I
      WRITE(BUFF,1001)IGPH
      CALL GSELNT(0)
      CALL WTSTR(.5,.85,'IGPH = ',2,0,1)
      CALL WTSTR(.5,.85,BUFF,2,0,-1)
      CALL WTSTR(.5,.9,'DEMONSTRATION PLOT FOR GRIDAL',2,0,0)
      CALL GSELNT(1)
      CALL GRIDAL(5,2,6,3,1,1,IGPH,.3,.13)
      CALL FRAME
  100 CONTINUE
C
 1001 FORMAT(I2)
C
      WRITE(6,600)
  600 FORMAT(' GRIDAL SUCCESSFUL--SEE PLOTS TO VERIFY PERFORMANCE')
      RETURN
      END
