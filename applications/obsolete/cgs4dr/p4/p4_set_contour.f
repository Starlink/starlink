*+  P4_SET_CONTOUR - Calculates the contour array for PGCONT
      SUBROUTINE P4_SET_CONTOUR( PORT, STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     02-Dec-1993: Original version                               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'P4COM.INC'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT
*    Local Constants :
      REAL BASE_E, STEP, PLUS_RANGE, MINUS_RANGE
      INTEGER I, MPOS, MSTEP, PSTEP, PLUS_LEVELS, MINUS_LEVELS
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Linear contouring
      CALL CHR_UCASE( CONTOUR_TYPE(PORT) )
      IF ( CONTOUR_TYPE( PORT ) .EQ. 'LIN' ) THEN

         IF ( AUTOSCALE( PORT ) ) THEN
*          Do not include HIGH, LOW in contour because they are single pixels
            STEP = ( HIGH( PORT )-LOW( PORT ) ) / REAL( CONTOUR_LEVELS( PORT )+1 )
            DO I = 1, CONTOUR_LEVELS( PORT ), 1
              ARRAY_CONTOURS( I ) = LOW( PORT ) + I*STEP
            ENDDO
         ELSE
*          Include HIGH, LOW in contour because they are specific limits
            STEP = ( HIGH( PORT )-LOW( PORT ) ) / REAL( CONTOUR_LEVELS( PORT )-1 )
            DO I = 1, CONTOUR_LEVELS( PORT ), 1
              ARRAY_CONTOURS( I ) = LOW( PORT ) + (I-1)*STEP
            ENDDO
         ENDIF

*    Magnitude contouring (from LOW( PORT )est level up)
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'MAGI' ) THEN

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * 10.0**(0.4*I)
            ELSE
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * 10.0**(0.4*(I-1))
            END IF
            IF ( ARRAY_CONTOURS( I ) .GT. HIGH( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 10
            ENDIF
         ENDDO
 10      CONTINUE

*    Magnitude contouring (from HIGH( PORT )est level down)
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'MAGD' ) THEN

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * 10.0**(-0.4*I)
            ELSE
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * 10.0**(-0.4*(I-1))
            END IF
            IF ( ARRAY_CONTOURS( I ) .LT. LOW( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 20
            ENDIF
         ENDDO
 20      CONTINUE
         IF ( CONTOUR_LEVELS( PORT ) .GT. 0 ) THEN
            CALL GEN_SORTF( ARRAY_CONTOURS, CONTOUR_LEVELS( PORT ) )
         ENDIF

*    Logarithmic contouring (base 10, increment from LOW( PORT ))
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'LOGI' ) THEN

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * 10.0**I
            ELSE
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * 10.0**(I-1)
            END IF
            IF ( ARRAY_CONTOURS( I ) .GT. HIGH( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 30
            ENDIF
         ENDDO
 30      CONTINUE

*    Logarithmic contouring (base 10, decrement from HIGH( PORT ))
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'LOGD' ) THEN

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * 10.0**(-(I))
            ELSE
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * 10.0**(-(I-1))
            END IF
            IF ( ARRAY_CONTOURS( I ) .LT. LOW( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 40
            ENDIF
         ENDDO
 40      CONTINUE
         IF ( CONTOUR_LEVELS( PORT ) .GT. 0 ) THEN
            CALL GEN_SORTF( ARRAY_CONTOURS, CONTOUR_LEVELS( PORT ) )
         ENDIF

*    Logarithmic contouring (base e, increment from LOW( PORT ))
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'LNI' ) THEN

         BASE_E = EXP(1.0)

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * BASE_E**(I)
            ELSE
               ARRAY_CONTOURS( I ) = ABS(LOW( PORT )) * BASE_E**(I-1)
            END IF
            IF ( ARRAY_CONTOURS( I ) .GT. HIGH( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 50
            ENDIF
         ENDDO
 50      CONTINUE

*    Logarithmic contouring (base e, decrement from HIGH( PORT ))
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'LND' ) THEN

         BASE_E = EXP(1.0)

*       Set the LEVELS available within the range
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            IF ( AUTOSCALE( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * BASE_E**(-(I))
            ELSE
               ARRAY_CONTOURS( I ) = HIGH( PORT ) * BASE_E**(-(I-1))
            END IF
            IF ( ARRAY_CONTOURS( I ) .LT. LOW( PORT ) ) THEN
               ARRAY_CONTOURS( I ) = 0.0
               CONTOUR_LEVELS( PORT ) = I - 1
               GOTO 60
            ENDIF
         ENDDO
 60      CONTINUE
         IF ( CONTOUR_LEVELS( PORT ) .GT. 0 ) THEN
            CALL GEN_SORTF( ARRAY_CONTOURS, CONTOUR_LEVELS( PORT ) )
         ENDIF

*    SIGMA( PORT ) contouring
      ELSE IF ( CONTOUR_TYPE( PORT ) .EQ. 'SIG' ) THEN

*       Check to see if we can fit this number of LEVELS in above the MEAN
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            PLUS_RANGE = MEAN( PORT ) + I*SIGMA( PORT )
            IF ( PLUS_RANGE .GT. HIGH( PORT ) ) THEN
               PLUS_LEVELS = I - 1
               GOTO 70
            ENDIF
         ENDDO
 70      CONTINUE

*       Check to see if we can fit this number of LEVELS in below the MEAN
         DO I = 1, CONTOUR_LEVELS( PORT ), 1
            MINUS_RANGE = MEAN( PORT ) - I*SIGMA( PORT )
            IF ( MINUS_RANGE .LT. LOW( PORT ) ) THEN
               MINUS_LEVELS = I - 1
               GOTO 80
            ENDIF
         ENDDO
 80      CONTINUE

*       Set the negative LEVELS (number required or those that will fit)
         MSTEP = MIN( CONTOUR_LEVELS( PORT ), MINUS_LEVELS )
         DO I = 1, MSTEP, 1
            ARRAY_CONTOURS( I ) = MEAN( PORT ) - (MSTEP-I+1)*SIGMA( PORT )
         ENDDO

*       Set the MEAN( PORT ) level
         MPOS = MSTEP + 1
         ARRAY_CONTOURS( MPOS ) = MEAN( PORT )

*       Set the positive LEVELS (number required or those that will fit)
         PSTEP = MIN( CONTOUR_LEVELS( PORT ), PLUS_LEVELS )
         DO I = 1, PSTEP, 1
            ARRAY_CONTOURS( MPOS+I ) = MEAN( PORT ) + I*SIGMA( PORT )
         ENDDO
         CONTOUR_LEVELS( PORT ) = MSTEP + PSTEP + 1
      ENDIF

      END
