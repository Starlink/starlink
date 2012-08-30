      program testrebin
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status
      double precision params(2)
      character ch
      status = sai__ok

      call ast_begin( status )

      params(1) = 1.5
      params(2) = 1

      call tests( 1, AST__GAUSS, params, status )
      call tests( 2, AST__GAUSS, params, status )
      call tests( 3, AST__GAUSS, params, status )
      call tests( 1, AST__NEAREST, params, status )
      call tests( 2, AST__NEAREST, params, status )
      call tests( 3, AST__NEAREST, params, status )
      call tests( 1, AST__LINEAR, params, status )
      call tests( 2, AST__LINEAR, params, status )
      call tests( 3, AST__LINEAR, params, status )

      call ast_end( status )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All AST_REBINSEQ tests passed'
      else
         write(*,*) 'AST_REBINSEQ tests failed'
      end if

      end

      subroutine tests( NDIM, SPREAD, params, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer spread, status, ndim
      double precision params(2)

      if( status .ne. sai__ok ) return

      call test1( NDIM, SPREAD, params, status )
      call test2( NDIM, SPREAD, params, status )
      call test3( NDIM, SPREAD, params, status )
      call test4( NDIM, SPREAD, params, status )
      call test5( NDIM, SPREAD, params, status )
      call test6( NDIM, SPREAD, params, status )
      call test7( NDIM, SPREAD, params, status )
      call test8( NDIM, SPREAD, params, status )
      call test9( NDIM, SPREAD, params, status )
      call test10( NDIM, SPREAD, params, status )
      call test11( NDIM, SPREAD, params, status )
      call test12( NDIM, SPREAD, params, status )

      if( status .ne. SAI__OK ) then
         call msg_seti( 'N', ndim )

         if( spread .eq. AST__GAUSS ) then
            call msg_setc( 'S', 'AST__GAUSS' )
         else if( spread .eq. AST__NEAREST ) then
            call msg_setc( 'S', 'AST__NEAREST' )
         else if( spread .eq. AST__LINEAR ) then
            call msg_setc( 'S', 'AST__LINEAR' )
         endif
         call err_rep( ' ', 'Spread=^S (^N-dimensional)', status )
      endif

      end




      SUBROUTINE ADDNOISE( N, ARRAY, SIGMA, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER BUFSIZE
      PARAMETER ( BUFSIZE = 100 )

      INTEGER N, STATUS, MM, IAT, NUSED, I
      DOUBLE PRECISION ARRAY( N )
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION NOISE( BUFSIZE )
      DOUBLE PRECISION JUNK( BUFSIZE )

      CHARACTER FWD(1)*80
      CHARACTER INV(1)*80

      DATA JUNK/ BUFSIZE*0.0D0 /

      IF( STATUS .NE. SAI__OK ) RETURN

      FWD(1) = 'Y=Gauss(0.0,'
      IAT = 12
      CALL CHR_PUTD( SIGMA, FWD(1), IAT )
      CALL CHR_APPND( ')', FWD(1), IAT )
      INV(1) = 'X'

      MM = AST_MATHMAP( 1, 1, 1, FWD, 1, INV, ' ', STATUS )

      NUSED = BUFSIZE
      DO I = 1, N
         IF( NUSED .EQ. BUFSIZE ) THEN
            CALL AST_TRAN1( MM, BUFSIZE, JUNK, .TRUE., NOISE, STATUS )
            NUSED = 0
         END IF
         NUSED = NUSED + 1
         ARRAY( I ) = ARRAY( I ) + NOISE( NUSED )
      END DO

      CALL AST_ANNUL( MM, STATUS )

      END






      SUBROUTINE TEST1( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY, JHI
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )


      REAL IN( NX, NY )
      REAL OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY )
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      LBND_IN( 3 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY
      UBND_IN( 3 ) = 1

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND

      MAP = AST_UNITMAP( NDIM, ' ', STATUS )
      CALL AST_REBINSEQR( MAP, 0.0D0, NDIM, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADR,
     :                   NDIM, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   out, OUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      DO I = 1, NX
         DO J = 1, JHI
            IF( ABS( OUT( I, J ) - 1.0 ) .GT. 1.0E-6 .AND.
     :         STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'J', J )
               CALL MSG_SETR( 'V', OUT(I,J) )
               CALL ERR_REP( ' ', 'Output pixel (^I,^J) should be '//
     :                       '1.0 but is ^V', status )
            END IF
         END DO
      END DO

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test1 failed', STATUS )
      END IF

      END



      SUBROUTINE TEST2( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY, JHI
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )


      DOUBLE PRECISION IN( NX, NY )
      DOUBLE PRECISION OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY )
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0D0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT

      MAP = AST_UNITMAP( NDIM, ' ', STATUS )

      DO I = 1, 3
         IF( I .EQ. 3 ) FLAGS = AST__REBINEND
         CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )
      END DO

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      DO I = 1, NX
         DO J = 1, JHI
            IF( ABS( OUT( I, J ) - 1.0D0 ) .GT. 1.0E-6 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'J', J )
               CALL MSG_SETD( 'V', OUT(I,J) )
               CALL ERR_REP( ' ', 'Output pixel (^I,^J) should be '//
     :                       '1.0 but is ^V', status )
            END IF
         END DO
      END DO

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test2 failed', STATUS )
      END IF

      END





      SUBROUTINE TEST3( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )

      INTEGER IN( NX, NY )
      INTEGER OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY )
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, JHI

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0D0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__NONORM

      MAP = AST_UNITMAP( NDIM, ' ', STATUS )

      DO I = 1, 3
         CALL AST_REBINSEQI( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADI,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )
         FLAGS = AST__NONORM
      END DO

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      DO I = 1, NX
         DO J = 1, JHI
            IF( OUT( I, J ) .NE. 3 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'J', J )
               CALL MSG_SETI( 'V', OUT(I,J) )
               CALL ERR_REP( ' ', 'Output pixel (^I,^J) should be '//
     :                       '3 but is ^V', status )
            END IF
         END DO
      END DO

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test3 failed', STATUS )
      END IF

      END






      SUBROUTINE TEST4( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )

      REAL IN( NX, NY )
      REAL OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY ), INA(3),
     :                 INB(3), OUTA(3), OUTB(3), SUM, ANSWER
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, LBOXG(3), UBOXG(3), JHI

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__CONSERVEFLUX

      INA( 1 ) = LBND_IN( 1 )
      INA( 2 ) = LBND_IN( 2 )
      INA( 3 ) = LBND_IN( 3 )
      INB( 1 ) = UBND_IN( 1 )
      INB( 2 ) = UBND_IN( 2 )
      INB( 3 ) = UBND_IN( 3 ) + 1.0D0

      OUTA( 1 ) = 0.75*LBND_OUT( 1 ) + 0.25*UBND_OUT( 1 )
      OUTA( 2 ) = 0.75*LBND_OUT( 2 ) + 0.25*UBND_OUT( 2 )
      OUTA( 3 ) = INA( 3 )
      OUTB( 1 ) = 0.25*LBND_OUT( 1 ) + 0.75*UBND_OUT( 1 )
      OUTB( 2 ) = 0.25*LBND_OUT( 2 ) + 0.75*UBND_OUT( 2 )
      OUTB( 3 ) = INB( 3 )

      MAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )
      CALL AST_REBINSEQR( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 1000,
     :                   VAL__BADR,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )

      LBOXG( 1 ) = VAL__MAXI
      LBOXG( 2 ) = VAL__MAXI
      UBOXG( 1 ) = VAL__MINI
      UBOXG( 2 ) = VAL__MINI

      SUM = 0.0D0

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      DO I = 1, NX
         DO J = 1, JHI

            IF( OUT(I,J) .NE. VAL__BADR ) THEN
               SUM = SUM + OUT(I,J)
               IF( I .LT. LBOXG(1) ) THEN
                  LBOXG(1) = I
               ELSE IF( I .GT. UBOXG(1) ) THEN
                  UBOXG(1) = I
               ENDIF
               IF( J .LT. LBOXG(2) ) THEN
                  LBOXG(2) = J
               ELSE IF( J .GT. UBOXG(2) ) THEN
                  UBOXG(2) = J
               ENDIF
            ENDIF
         END DO
      END DO

      IF( NDIM .EQ. 1 ) THEN

         IF( ( ( SPREAD .EQ. AST__GAUSS ) .AND. (
     :            LBOXG( 1 ) .NE. 24 .OR.
     :            UBOXG( 1 ) .NE. 77 )) .OR. (
     :            ( SPREAD .EQ. AST__NEAREST ) .AND. (
     :            LBOXG( 1 ) .NE. 26 .OR.
     :            UBOXG( 1 ) .NE. 75 )) .OR. (
     :            ( SPREAD .EQ. AST__LINEAR ) .AND. (
     :            LBOXG( 1 ) .NE. 25 .OR.
     :            UBOXG( 1 ) .NE. 76 ) ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Good pixel bounding box is wrong',
     :                    STATUS )
            write(*,*) LBOXG, UBOXG
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            ANSWER = 100.0
         ELSE IF( SPREAD .EQ. AST__GAUSS ) THEN
            ANSWER = 108.0
         ELSE IF( SPREAD .EQ. AST__LINEAR ) THEN
            ANSWER = 104.0
         ELSE
            ANSWER = -1
         END IF

      else

         IF( ( ( SPREAD .EQ. AST__GAUSS ) .AND. (
     :            LBOXG( 1 ) .NE. 24 .OR.
     :            LBOXG( 2 ) .NE. 49 .OR.
     :            UBOXG( 1 ) .NE. 77 .OR.
     :            UBOXG( 2 ) .NE. 152 ) ) .OR. (
     :            ( SPREAD .EQ. AST__NEAREST ) .AND. (
     :            LBOXG( 1 ) .NE. 26 .OR.
     :            LBOXG( 2 ) .NE. 51 .OR.
     :            UBOXG( 1 ) .NE. 75 .OR.
     :            UBOXG( 2 ) .NE. 150 ) ) .OR. (
     :            ( SPREAD .EQ. AST__LINEAR ) .AND. (
     :            LBOXG( 1 ) .NE. 25 .OR.
     :            LBOXG( 2 ) .NE. 50 .OR.
     :            UBOXG( 1 ) .NE. 76 .OR.
     :            UBOXG( 2 ) .NE. 151 ) ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Good pixel bounding box is wrong',
     :                    STATUS )
            write(*,*) LBOXG, UBOXG
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            ANSWER = 20000.0
         ELSE IF( SPREAD .EQ. AST__GAUSS ) THEN
            ANSWER = 22464.0
         ELSE IF( SPREAD .EQ. AST__LINEAR ) THEN
            ANSWER = 21216.0
         ELSE
            ANSWER = -1
         END IF
      endif

      IF( ABS( SUM - ANSWER ) .GT. 0.01 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', SUM )
         CALL MSG_SETD( 'W', ANSWER )
         CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                 'be ^W).', STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test4 failed', STATUS )
      END IF

      END



      SUBROUTINE TEST5( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX_IN, NY_IN
      PARAMETER( NX_IN = 100 )
      PARAMETER( NY_IN = 200 )

      INTEGER BORDER_OUT
      PARAMETER( BORDER_OUT = 10 )

      INTEGER NX_OUT, NY_OUT
      PARAMETER( NX_OUT = NX_IN + 2*BORDER_OUT )
      PARAMETER( NY_OUT = NY_IN + 2*BORDER_OUT )

      DOUBLE PRECISION IN( NX_IN, NY_IN )
      DOUBLE PRECISION OUT( NX_OUT, NY_OUT )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX_OUT, NY_OUT ), INA(3),
     :                 INB(3), OUTA(3), OUTB(3), SUM, VA, VB
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        K, FLAGS, NUSED, I, J, LBOXG(3), UBOXG(3), JHI

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX_IN - 1
      UBND_IN( 2 ) = NY_IN

      LBND_OUT( 1 ) = LBND_IN( 1 ) - BORDER_OUT
      LBND_OUT( 2 ) = LBND_IN( 2 ) - BORDER_OUT
      UBND_OUT( 1 ) = UBND_IN( 1 ) + BORDER_OUT
      UBND_OUT( 2 ) = UBND_IN( 2 ) + BORDER_OUT

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__NONORM

      INA( 1 ) = LBND_IN( 1 )
      INA( 2 ) = LBND_IN( 2 )
      INA( 3 ) = LBND_IN( 3 )
      INB( 1 ) = UBND_IN( 1 )
      INB( 2 ) = UBND_IN( 2 )
      INB( 3 ) = UBND_IN( 3 ) + 1.0D0

      DO K = 1, 3

         DO I = 1, NX_IN
            DO J = 1, NY_IN
               IN( I, J ) = K
            END DO
         END DO

         VA = (k-1)*0.25
         VB = VA + 0.5

         OUTA( 1 ) = VB*LBND_IN( 1 ) + VA*UBND_IN( 1 )
         OUTA( 2 ) = VB*LBND_IN( 2 ) + VA*UBND_IN( 2 )
         OUTB( 1 ) = VA*LBND_IN( 1 ) + VB*UBND_IN( 1 )
         OUTB( 2 ) = VA*LBND_IN( 2 ) + VB*UBND_IN( 2 )
         OUTA( 3 ) = INA( 3 )
         OUTB( 3 ) = INB( 3 )

         MAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )
         CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )

         FLAGS = AST__NONORM
      END DO

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny_out
      ENDIF

      SUM = 0.0D0

      DO I = 1, NX_OUT
         DO J = 1, JHI
            IF( OUT(I,J) .NE. VAL__BADR ) THEN
               SUM = SUM + OUT(I,J)
            ENDIF
         END DO
      END DO

      IF( NDIM .EQ. 1 ) THEN
         IF( ABS( SUM - 600 ) .GT. 1.0E-3 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', SUM )
            CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                    'be 600).', STATUS )

         END IF

         IF( ABS( OUT(20,1) - 2.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(20,1) )
            CALL ERR_REP( ' ', 'Output pixel (20) should be 2, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(50,1) - 6.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(50,1) )
            CALL ERR_REP( ' ', 'Output pixel (50) should be 6, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(70,1) - 10.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(70,1) )
            CALL ERR_REP( ' ', 'Output pixel (70) should be 10, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(100,1) - 6.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(100,1) )
            CALL ERR_REP( ' ', 'Output pixel (100) should be 6, '//
     :                    'is ^V', STATUS )
         END IF

      ELSE IF( NDIM .EQ. 2 ) THEN
         IF( ABS( SUM - 120000 ) .GT. 1.0E-3 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', SUM )
            CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                    'be 120000).', STATUS )

         END IF

         IF( ABS( OUT(40,40) - 4.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(40,40) )
            CALL ERR_REP( ' ', 'Output pixel (40,40) should be 4, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(50,90) - 12.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(50,90) )
            CALL ERR_REP( ' ', 'Output pixel (50,90) should be 12, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(70,80) - 8.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(70,80) )
            CALL ERR_REP( ' ', 'Output pixel (70,80) should be 8, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(70,130) - 20.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(70,130) )
            CALL ERR_REP( ' ', 'Output pixel (70,130) should be 20, '//
     :                    'is ^V', STATUS )

         ELSE IF( ABS( OUT(20,130) - 0.0 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', OUT(20,130) )
            CALL ERR_REP( ' ', 'Output pixel (20,130) should be 0, '//
     :                    'is ^V', STATUS )

         END IF
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test5 failed', STATUS )
      END IF

      END




      SUBROUTINE TEST6( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NK
      PARAMETER( NK = 3 )

      INTEGER NX_IN, NY_IN
      PARAMETER( NX_IN = 100 )
      PARAMETER( NY_IN = 200 )

      INTEGER BORDER_OUT
      PARAMETER( BORDER_OUT = 10 )

      INTEGER NX_OUT, NY_OUT
      PARAMETER( NX_OUT = NX_IN + 2*BORDER_OUT )
      PARAMETER( NY_OUT = NY_IN + 2*BORDER_OUT )

      DOUBLE PRECISION IN( NX_IN, NY_IN ), ANSWER
      DOUBLE PRECISION OUT( NX_OUT, NY_OUT ), MNVAL, MXVAL
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX_OUT, NY_OUT ), INA(3),
     :                 INB(3), OUTA(3), OUTB(3), SUM, VA, VB
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        K, FLAGS, NUSED, I, J, LBOXG(3), UBOXG(3), JHI

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX_IN - 1
      UBND_IN( 2 ) = NY_IN

      LBND_OUT( 1 ) = LBND_IN( 1 ) - BORDER_OUT
      LBND_OUT( 2 ) = LBND_IN( 2 ) - BORDER_OUT
      UBND_OUT( 1 ) = UBND_IN( 1 ) + BORDER_OUT
      UBND_OUT( 2 ) = UBND_IN( 2 ) + BORDER_OUT

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      INA( 1 ) = LBND_IN( 1 )
      INA( 2 ) = LBND_IN( 2 )
      INA( 3 ) = LBND_IN( 3 )
      INB( 1 ) = UBND_IN( 1 )
      INB( 2 ) = UBND_IN( 2 )
      INB( 3 ) = UBND_IN( 3 ) + 1.0D0

      DO K = 1, NK

         FLAGS = AST__CONSERVEFLUX
         IF( K .EQ. 1 ) FLAGS = FLAGS + AST__REBININIT
         IF( K .EQ. NK ) FLAGS = FLAGS + AST__REBINEND

         DO I = 1, NX_IN
            DO J = 1, NY_IN
               IN( I, J ) = K
            END DO
         END DO

         VA = (k-1)*0.25
         VB = VA + 0.5

         OUTA( 1 ) = VB*LBND_IN( 1 ) + VA*UBND_IN( 1 )
         OUTA( 2 ) = VB*LBND_IN( 2 ) + VA*UBND_IN( 2 )
         OUTB( 1 ) = VA*LBND_IN( 1 ) + VB*UBND_IN( 1 )
         OUTB( 2 ) = VA*LBND_IN( 2 ) + VB*UBND_IN( 2 )
         OUTA( 3 ) = INA( 3 )
         OUTB( 3 ) = INB( 3 )

         MAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )
         CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )

      END DO

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny_out
      ENDIF

      SUM = 0.0D0
      MXVAL = VAL__MIND
      MNVAL = VAL__MAXD
      DO I = 1, NX_OUT
         DO J = 1, JHI
            IF( OUT(I,J) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT(I,J)
               IF( OUT(I,J) .GT. MXVAL ) MXVAL = OUT(I,J)
               IF( OUT(I,J) .lT. MnVAL ) MNVAL = OUT(I,J)
            ENDIF
         END DO
      END DO

      IF( NDIM .eq. 1 ) THEN
         IF( SPREAD .EQ. AST__GAUSS ) THEN
            ANSWER = 414.0D0
         ELSE IF( SPREAD .EQ. AST__NEAREST ) THEN
            ANSWER = 399.4D0
         ELSE IF( SPREAD .EQ. AST__LINEAR ) THEN
            ANSWER = 400.0D0
         ELSE
            ANSWER = -1.0
         END IF

         IF( ABS( SUM - ANSWER ) .GT. 1.0D-3 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', SUM )
            CALL MSG_SETD( 'W', ANSWER )
            CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                    'be ^W).', STATUS )

         ELSE IF( ABS( MXVAL - 6 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', MXVAL )
            CALL ERR_REP( ' ', 'Max value is ^V (should be 6).',
     :                    STATUS )

         ELSE IF( ABS( MNVAL - 2 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', MNVAL )
            CALL ERR_REP( ' ', 'Min value is ^V (should be 2).',
     :                    STATUS )

         END IF

      ELSE IF( NDIM .eq. 2 ) THEN
         IF( SPREAD .EQ. AST__GAUSS ) THEN
            ANSWER = 109011.729592723D0
         ELSE IF( SPREAD .EQ. AST__NEAREST ) THEN
            ANSWER = 100716.666666667D0
         ELSE IF( SPREAD .EQ. AST__LINEAR ) THEN
            ANSWER = 102816.0D0
         ELSE
            ANSWER = -1.0
         END IF

         IF( ABS( SUM - ANSWER ) .GT. 1.0D-3 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', SUM )
            CALL MSG_SETD( 'W', ANSWER )
            CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                    'be ^W).', STATUS )

         ELSE IF( ABS( MXVAL - 12 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', MXVAL )
            CALL ERR_REP( ' ', 'Max value is ^V (should be 12).',
     :                    STATUS )

         ELSE IF( ABS( MNVAL - 4 ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'V', MNVAL )
            CALL ERR_REP( ' ', 'Min value is ^V (should be 4).',
     :                    STATUS )

         END IF
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test6 failed', STATUS )
      END IF

      END


      SUBROUTINE TEST7( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )


      REAL IN( NX, NY )
      REAL OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY )
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        JHI, FLAGS, NUSED, I, J

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND

      MAP = AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS )
      CALL AST_REBINSEQR( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADR,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      DO I = 1, NX
         DO J = 1, JHI
            IF( ABS( OUT( I, J ) - 1.0 ) .GT. 1.0E-6 .AND.
     :          OUT( I, J ) .NE. VAL__BADR ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'J', J )
               CALL MSG_SETR( 'V', OUT(I,J) )
               CALL ERR_REP( ' ', 'Output pixel (^I,^J) should be '//
     :                       '1.0 but is ^V', status )
            END IF
         END DO
      END DO

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test7 failed', STATUS )
      END IF

      END


      SUBROUTINE TEST8( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER NX, NY
      PARAMETER( NX = 100 )
      PARAMETER( NY = 200 )


      REAL IN( NX, NY )
      REAL OUT( NX, NY )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( NX, NY ), SHIFTS(3), SUM
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, JHI

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, NX
         DO J = 1, NY
            IN( I, J ) = 1.0
         END DO
      END DO

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__NONORM

      SHIFTS(1) = 5.0D0
      SHIFTS(2) = 5.0D0

      if( ndim .lt. 3 ) then
         MAP = AST_CMPMAP( AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS ),
     :                     AST_SHIFTMAP( NDIM, SHIFTS, ' ', STATUS ),
     :                     .TRUE., ' ', STATUS )
      else
         MAP = AST_CMPMAP( AST_CMPMAP( AST_ZOOMMAP( 2, 0.5D0, ' ',
     :                                              STATUS ),
     :                                 AST_SHIFTMAP( 2, SHIFTS, ' ',
     :                                               STATUS ),
     :                                 .TRUE., ' ', STATUS ),
     :                     AST_UNITMAP( 1, ' ', STATUS ), .FALSE.,
     :                     ' ', STATUS )
      endif

      CALL AST_REBINSEQR( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, IN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADR,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, OUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         JHI = 1
      ELSE
         JHI = ny
      ENDIF

      SUM = 0.0D0
      DO I = 1, NX
         DO J = 1, JHI
            if( out(i,j) .ne. VAL__BADR ) then
               SUM = SUM + DBLE(OUT(I,J))
            end if
         END DO
      END DO

      IF( SUM .NE. SUM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Total output data sum is NaN', STATUS )

      ELSE IF( ABS( SUM - NX*JHI ) .GT. SUM*1.0D-7 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', SUM )
         CALL MSG_SETD( 'W', DBLE( NX*JHI) )
         CALL ERR_REP( ' ', 'Total output data sum is ^V (should '//
     :                 'be ^W).', STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test8 failed', STATUS )
      END IF

      END


      SUBROUTINE TEST9( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER SIZE
      PARAMETER( SIZE = 20000 )

      INTEGER NX1, NY1
      PARAMETER( NX1 = SIZE )
      PARAMETER( NY1 = 1 )

      INTEGER NX2, NY2
      PARAMETER( NX2 = SIZE/200 )
      PARAMETER( NY2 = 200 )

      DOUBLE PRECISION SIGMA
      PARAMETER ( SIGMA = 0.1 )

      REAL*8 IN( size ), VIN( size )
      REAL*8 OUT( size ), VOUT( size )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( size )
      DOUBLE PRECISION REALVAR,MEANVAR,SUM,SUM2,SUM3
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, NVAL, JHI, JLO, NX, NY, ILO, IHI, k

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, size
         IN( I ) = 1.0D0
         VIN( I ) = SIGMA**2
      END DO

      CALL ADDNOISE( size, IN, SIGMA, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         NX = NX1
         Ny = NY1
      ELSE
         NX = NX2
         Ny = NY2
      END IF

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__NONORM + AST__USEVAR

      MAP = AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS )
      CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, VIN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, VOUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         ILO = 6
         IHI = NINT(0.45*NX1)
         JLO = 1
         JHI = 1
      ELSE
         ILO = 6
         IHI = 41
         JLO = 8
         JHI = 91
      ENDIF

      SUM = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      NVAL = 0

      DO I = ILO, IHI
         DO J = JLO, JHI
            K = ( J - 1 )*NX + I
            IF( OUT(K) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT(K)
               SUM2 = SUM2 + OUT(K)**2
               SUM3 = SUM3 + VOUT(K)
               NVAL = NVAL + 1
            END IF
         END DO
      END DO

      SUM = SUM/NVAL
      REALVAR = SUM2/NVAL - SUM*SUM
      MEANVAR = SUM3/NVAL
      IF( ABS( REALVAR - MEANVAR ) .GT.
     :    0.05*( REALVAR + MEANVAR ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', REALVAR )
         CALL MSG_SETD( 'W', MEANVAR )
         CALL ERR_REP( ' ', 'Real variance is ^V - estimate is ^W.',
     :                 STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test9 failed', STATUS )
      END IF

      END


      SUBROUTINE TEST10( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER SIZE
      PARAMETER( SIZE = 20000 )

      INTEGER NX1, NY1
      PARAMETER( NX1 = SIZE )
      PARAMETER( NY1 = 1 )

      INTEGER NX2, NY2
      PARAMETER( NX2 = SIZE/200 )
      PARAMETER( NY2 = 200 )

      DOUBLE PRECISION SIGMA
      PARAMETER ( SIGMA = 0.1 )

      REAL*8 IN( size ), VIN( size )
      REAL*8 OUT( size ), VOUT( size )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( size )
      DOUBLE PRECISION REALVAR,MEANVAR,SUM,SUM2,SUM3
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, NVAL, JHI, JLO, ILO, IHI, NX, NY, K

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, SIZE
         IN( I ) = 1.0D0
         VIN( I ) = SIGMA**2
      END DO

      CALL ADDNOISE( SIZE, IN, SIGMA, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         NX = NX1
         Ny = NY1
      ELSE
         NX = NX2
         Ny = NY2
      END IF

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__CONSERVEFLUX
     :        + AST__USEVAR

      MAP = AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS )
      CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, VIN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, VOUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         ILO = 6
         IHI = NINT(0.45*NX1)
         JLO = 1
         JHI = 1
      ELSE
         ILO = 6
         IHI = 41
         JLO = 8
         JHI = 91
      ENDIF

      SUM = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      NVAL = 0

      DO I = ILO,IHI
         DO J = JLO, JHI
            K = ( J - 1 )*NX + I
            IF( OUT(K) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT(K)
               SUM2 = SUM2 + OUT(K)**2
               SUM3 = SUM3 + VOUT(K)
               NVAL = NVAL + 1
            END IF
         END DO
      END DO

      SUM = SUM/NVAL
      REALVAR = SUM2/NVAL - SUM*SUM
      MEANVAR = SUM3/NVAL

      IF( ABS( REALVAR - MEANVAR ) .GT.
     :    0.05*( REALVAR + MEANVAR ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', REALVAR )
         CALL MSG_SETD( 'W', MEANVAR )
         CALL ERR_REP( ' ', 'Real variance is ^V - estimate is ^W.',
     :                 STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test10 failed', STATUS )
      END IF

      END

      SUBROUTINE TEST11( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER SIZE
      PARAMETER( SIZE = 20000 )

      INTEGER NX1, NY1
      PARAMETER( NX1 = SIZE )
      PARAMETER( NY1 = 1 )

      INTEGER NX2, NY2
      PARAMETER( NX2 = SIZE/200 )
      PARAMETER( NY2 = 200 )

      DOUBLE PRECISION SIGMA
      PARAMETER ( SIGMA = 0.1 )

      REAL*8 IN( SIZE ), VIN( SIZE )
      REAL*8 OUT( SIZE ), VOUT( SIZE )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( SIZE, 2 )
      DOUBLE PRECISION REALVAR,MEANVAR,SUM,SUM2,SUM3
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, NVAL, JLO, JHI, NX, NY, ILO, IHI, K

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, SIZE
         IN( I ) = 1.0D0
         VIN( I ) = SIGMA**2
      END DO

      CALL ADDNOISE( SIZE, IN, SIGMA, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         NX = NX1
         Ny = NY1
      ELSE
         NX = NX2
         Ny = NY2
      END IF

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__GENVAR

      MAP = AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS )
      CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, VIN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, VOUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         ILO = 6
         IHI = NINT(0.45*NX1)
         JLO = 1
         JHI = 1
      ELSE
         ILO = 6
         IHI = 41
         JLO = 8
         JHI = 91
      ENDIF

      SUM = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      NVAL = 0

      DO I = ILO,IHI
         DO J = JLO, JHI
            K = ( J - 1 )*NX + I
            IF( OUT(K) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT(K)
               SUM2 = SUM2 + OUT(K)**2
               SUM3 = SUM3 + VOUT(K)
               NVAL = NVAL + 1
            END IF
         END DO
      END DO

      SUM = SUM/NVAL
      REALVAR = SUM2/NVAL - SUM*SUM
      MEANVAR = SUM3/NVAL
      IF( ABS( REALVAR - MEANVAR ) .GT.
     :    0.05*( REALVAR + MEANVAR ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', REALVAR )
         CALL MSG_SETD( 'W', MEANVAR )
         CALL ERR_REP( ' ', 'Real variance is ^V - estimate is ^W.',
     :                 STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test11 failed', STATUS )
      END IF

      END


      SUBROUTINE TEST12( NDIM, SPREAD, PARAMS, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER SPREAD, STATUS, NDIM

      INTEGER SIZE
      PARAMETER( SIZE = 20000 )

      INTEGER NX1, NY1
      PARAMETER( NX1 = SIZE )
      PARAMETER( NY1 = 1 )

      INTEGER NX2, NY2
      PARAMETER( NX2 = SIZE/200 )
      PARAMETER( NY2 = 200 )

      DOUBLE PRECISION SIGMA
      PARAMETER ( SIGMA = 0.1 )

      REAL*8 IN( SIZE ), VIN( SIZE )
      REAL*8 OUT( SIZE ), VOUT( SIZE )
      DOUBLE PRECISION PARAMS(2), WEIGHTS( SIZE, 2 )
      DOUBLE PRECISION REALVAR,MEANVAR,SUM,SUM2,SUM3
      INTEGER MAP, LBND_IN(3), UBND_IN(3), LBND_OUT(3), UBND_OUT(3),
     :        FLAGS, NUSED, I, J, NVAL,jlo, jhi,NX, NY, ILO, IHI, K

      IF( STATUS .NE. SAI__OK ) RETURN
      CALL AST_BEGIN( STATUS )

      DO I = 1, SIZE
         IN( I ) = 1.0D0
         VIN( I ) = SIGMA**2
      END DO

      CALL ADDNOISE( SIZE, IN, SIGMA, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         NX = NX1
         Ny = NY1
      ELSE
         NX = NX2
         Ny = NY2
      END IF

      LBND_IN( 1 ) = 0
      LBND_IN( 2 ) = 1
      UBND_IN( 1 ) = NX - 1
      UBND_IN( 2 ) = NY

      LBND_OUT( 1 ) = 0
      LBND_OUT( 2 ) = 1
      UBND_OUT( 1 ) = NX - 1
      UBND_OUT( 2 ) = NY

      LBND_IN( 3 ) = 1
      UBND_IN( 3 ) = 1
      LBND_OUT( 3 ) = 1
      UBND_OUT( 3 ) = 1

      FLAGS = AST__REBININIT + AST__REBINEND + AST__GENVAR +
     :        AST__CONSERVEFLUX + AST__VARWGT

      MAP = AST_ZOOMMAP( NDIM, 0.5D0, ' ', STATUS )
      CALL AST_REBINSEQD( MAP, 0.0D0, ndim, LBND_IN, UBND_IN, IN, VIN,
     :                   spread, PARAMS, FLAGS, 0.01D0, 50, VAL__BADD,
     :                   ndim, LBND_OUT, UBND_OUT, LBND_IN, UBND_IN,
     :                   OUT, VOUT, WEIGHTS, NUSED, STATUS )

      IF( NDIM .EQ. 1 ) THEN
         ILO = 6
         IHI = NINT(0.45*NX1)
         JLO = 1
         JHI = 1
      ELSE
         ILO = 6
         IHI = 41
         JLO = 8
         JHI = 91
      ENDIF

      SUM = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      NVAL = 0

      DO I = ILO,IHI
         DO J = JLO, JHI
            K = ( J - 1 )*NX + I
            IF( OUT(K) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT(K)
               SUM2 = SUM2 + OUT(K)**2
               SUM3 = SUM3 + VOUT(K)
               NVAL = NVAL + 1
            END IF
         END DO
      END DO

      SUM = SUM/NVAL
      REALVAR = SUM2/NVAL - SUM*SUM
      MEANVAR = SUM3/NVAL
      IF( ABS( REALVAR - MEANVAR ) .GT.
     :    0.05*( REALVAR + MEANVAR ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'V', REALVAR )
         CALL MSG_SETD( 'W', MEANVAR )
         CALL ERR_REP( ' ', 'Real variance is ^V - estimate is ^W.',
     :                 STATUS )
      END IF

      CALL AST_END( STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'test12 failed', STATUS )
      END IF

      END


