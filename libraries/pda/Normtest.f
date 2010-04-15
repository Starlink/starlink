      PROGRAM NORMTEST
*
*  Test program for PDA normal deviates routine.
*
*  Written by: P.W. Draper 19th February 1997.
*

      DOUBLE PRECISION PDA_PPND16
      EXTERNAL PDA_PPND16
      DOUBLE PRECISION RESULT

      RESULT = PDA_PPND16( 0.5D0, IFAIL )
      IF ( RESULT .NE. 0.0 ) THEN
         WRITE(*,*) 'Normal deviate routine fails test', RESULT
      ELSE
         WRITE(*,*) 'Normal deviate routine passes test'
      END IF
      END
