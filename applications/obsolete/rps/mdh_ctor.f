      REAL FUNCTION MDH_CTOR( CVAL )

      CHARACTER*(*) CVAL
      INTEGER I , PLACE , J
      LOGICAL NODOT

      NODOT = .TRUE.
      I = 1
      J = LEN( CVAL )
      PLACE = 0

      DO WHILE ( I .LE. J )

        IF ( CVAL( I:I ) .EQ. '.' ) THEN

          NODOT = .FALSE.
          I = J

        ELSE IF ( PLACE .NE. 0 .AND. CVAL( I:I ) .EQ. ' ' ) THEN

          PLACE = I - 1
          I = J

        ELSE IF ( PLACE .EQ. 0 .AND. CVAL( I:I ) .NE. ' ' ) THEN

          PLACE = I

        END IF

        I = I + 1

      END DO

      IF ( NODOT ) THEN

        PLACE = PLACE + 1
        CVAL( PLACE: ) = '.'

      END IF

      READ( CVAL , '( D )' ) MDH_CTOR

      END
