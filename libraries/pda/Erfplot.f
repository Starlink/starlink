      PROGRAM ERFPLOT

      IMPLICIT NONE
      INTEGER IGNORE
      DOUBLE PRECISION X,Y
      INTEGER PGBEG
      DOUBLE PRECISION PDA_DERF

      IGNORE = PGBEG( 0, '?', 1, 1 )
      CALL PGSCF( 2 )
      CALL PGENV( -1., +1., -1., 1., 1, -2 )  ! square viewport
      CALL PGSWIN( -2.5, +2.5, -1.1, 1.1 )    ! coordinate range
      CALL PGBOX( 'BCNTS', 0., 0, 'BCNTS', 0., 0 )
      CALL PGLAB( 'x', 'erf(x)', ' ' )
      CALL PGMOVE( 0., -1. )
      CALL PGDRAW( 0., +1. )
      CALL PGMOVE( -2., 0. )
      CALL PGDRAW( +2., 0. )
      X = -2D0
      Y = PDA_DERF(X)
      CALL PGMOVE( SNGL(X), SNGL(Y) )
      DO 1 X = -2D0, 2D0, 5D-2
         Y = PDA_DERF(X)
         CALL PGDRAW( SNGL(X), SNGL(Y) )
 1    CONTINUE
      CALL PGEND

      END
