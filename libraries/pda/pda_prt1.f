
      SUBROUTINE PDA_PRT1
C  This subroutine prints intermediate output, as does PDA_PRT2 through
C  PDA_PRT10. Note that if PDA_SA is minimizing the function, the sign of the
C  function value and the directions (up/down) are reversed in all
C  output to correspond with the actual function optimization. This
C  correction is because PDA_SA was written to maximize functions and
C  it minimizes by maximizing the negative a function.

      WRITE(*,'(/,''  THE STARTING VALUE (X) IS OUTSIDE THE BOUNDS ''
     1          /,''  (LB AND UB). EXECUTION TERMINATED WITHOUT ANY''
     2          /,''  OPTIMIZATION. RESPECIFY X, UB OR LB SO THAT  ''
     3          /,''  LB(I) .LT. X(I) .LT. UB(I), I = 1, N. ''/)')

      RETURN
      END
