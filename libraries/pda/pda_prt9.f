
      SUBROUTINE PDA_PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,
     1   NNEW)

      DOUBLE PRECISION  XOPT(*), VM(*), T, FOPT
      INTEGER  N, NUP, NDOWN, NREJ, LNOBDS, NNEW, TOTMOV
      LOGICAL  MAX

      TOTMOV = NUP + NDOWN + NREJ

      WRITE(*,'(/,
     1  '' INTERMEDIATE RESULTS BEFORE NEXT TEMPERATURE REDUCTION'',/)')
      WRITE(*,'(''  CURRENT TEMPERATURE:            '',G12.5)') T
      IF (MAX) THEN
         WRITE(*,'(''  MAX FUNCTION VALUE SO FAR:  '',G25.18)') FOPT
         WRITE(*,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(*,'(''     UPHILL:                  '',I8)') NUP
         WRITE(*,'(''     ACCEPTED DOWNHILL:       '',I8)') NDOWN
         WRITE(*,'(''     REJECTED DOWNHILL:       '',I8)') NREJ
         WRITE(*,'(''  OUT OF BOUNDS TRIALS:       '',I8)') LNOBDS
         WRITE(*,'(''  NEW MAXIMA THIS TEMPERATURE:'',I8)') NNEW
      ELSE
         WRITE(*,'(''  MIN FUNCTION VALUE SO FAR:  '',G25.18)') -FOPT
         WRITE(*,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(*,'(''     DOWNHILL:                '',I8)')  NUP
         WRITE(*,'(''     ACCEPTED UPHILL:         '',I8)')  NDOWN
         WRITE(*,'(''     REJECTED UPHILL:         '',I8)')  NREJ
         WRITE(*,'(''  TRIALS OUT OF BOUNDS:       '',I8)')  LNOBDS
         WRITE(*,'(''  NEW MINIMA THIS TEMPERATURE:'',I8)')  NNEW
      END IF
      CALL PDA_PRTVEC(XOPT,N,'CURRENT OPTIMAL X')
      CALL PDA_PRTVEC(VM,N,'STEP LENGTH (VM)')
      WRITE(*,'('' '')')

      RETURN
      END
