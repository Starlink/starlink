      SUBROUTINE PWRY (X,Y,ID,N,SIZE,THETA,ICNT)
C
C PWRY IS AN OLD ENTRY POINT AND HAS BEEN REMOVED - USE PWRITY
C ENTRY POINT
C
C 29-NOV-1991 P.C.T.Rees (STARLINK)
C    Assign I1MACH to the variable I1UNIT to comply with the Fortran 
C    standard.
C
      I1UNIT=I1MACH(4)
      WRITE (I1UNIT,1001)
      WRITE (I1UNIT,1002)
      STOP
C
 1001 FORMAT ('1'//////////)
 1002 FORMAT (' ****************************************'/
     1        ' *                                      *'/
     2        ' *                                      *'/
     3        ' *   THE ENTRY POINT PWRY IS NO LONGER  *'/
     4        ' *   SUPPORTED.  PLEASE USE THE MORE    *'/
     5        ' *   RECENT VERSION  PWRITY.            *'/
     6        ' *                                      *'/
     7        ' *                                      *'/
     8        ' ****************************************')
      END
