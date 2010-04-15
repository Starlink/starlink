C
C PWD: not used. Checked C version of PDA_D1MACH against C version.
C
      PROGRAM D1MACHTEST

      DOUBLE PRECISION PDA_D1MACH
      DOUBLE PRECISION PDA_D1MACH_C
      INTEGER I
      LOGICAL PASS

      PRINT *, 'Test of D1MACH C replacement'
      PASS = .TRUE.
      DO I = 1, 5
         PRINT *,' Fortran:', PDA_D1MACH( I ), ' -> ',
     :           'C:', PDA_D1MACH_C( I )
         IF ( PDA_D1MACH( I ) .NE. PDA_D1MACH_C( I ) ) THEN
            PASS = .FALSE.
            PRINT *, 'failed'
         END IF
      END DO
      IF ( .NOT. PASS ) THEN
         PRINT *, 'FAILED'
      ELSE
         PRINT *, 'PASSED'
      END IF
      END
