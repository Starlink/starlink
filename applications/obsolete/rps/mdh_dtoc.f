*+
*  14 JULY 1992	M. DUESTERHAUS		MODIFY FOR PORTABILITY
*****************************************************************
      CHARACTER*(*) FUNCTION MDH_DTOC( DNUM )

*INPUT:

      DOUBLE PRECISION DNUM

*LOCAL:

      CHARACTER*25 DUMMY
      CHARACTER*10 ZEROFILL /'0.000000'/
      CHARACTER*1 SIGN
      INTEGER I1 , I2 , I3, START, NEG

*FUNCTION CALLED:

      INTEGER MDH_ENDWORD, FIND_NOT_BLANK

*-

      IF ( DNUM .EQ. 0.0 ) THEN

        MDH_DTOC = '0.0'

      ELSE

        WRITE( DUMMY , '( D )' ) DNUM
        DUMMY = DUMMY( FIND_NOT_BLANK ( DUMMY ): )
        I1 = INDEX( DUMMY , 'D' ) - 1
        IF ( I1 .GT. 0 ) THEN

          START = INDEX(DUMMY,'.') +1
          NEG = INDEX (DUMMY(:START),'-')
          IF (NEG.GT.0) THEN
            SIGN = '-'
          ELSE
            SIGN = ' '
          END IF
          I2 = MDH_ENDWORD( DUMMY )
          READ( DUMMY( I1+2:I2 ) , '( I )' ) I3

          DO WHILE ( DUMMY( I1:I1 ) .EQ. '0' )

            I1 = I1 - 1

          END DO

	  IF (I3.GE.1) THEN

            MDH_DTOC = SIGN// DUMMY( START:I3+START-1) // '.'
     &         // DUMMY(I3+START:I1+1 )
	  ELSE

	    MDH_DTOC = SIGN//ZEROFILL(1:2-I3) // DUMMY( START:I1+1 )
	  END IF

        ELSE

          MDH_DTOC = DUMMY

        END IF

      END IF

      END
