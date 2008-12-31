*+  MDH_ENDWORD
*-  Author M.D.C.Harris ( R.A.L )                   5th February 1987.
*****************************************************************************
      INTEGER FUNCTION MDH_ENDWORD( WORD )

*  -----------
*  DESCRIPTION
*  -----------

*  This function finds the position of the last non space in a word.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) WORD     ! Word to examine.


      MDH_ENDWORD = LEN( WORD )

      DO WHILE ( MDH_ENDWORD .GT. 0 .AND.                                        ! Loop backwords through the word until a -
     &         ( WORD( MDH_ENDWORD : MDH_ENDWORD ) .EQ. ' ' .OR.                ! non space or -W
     &           WORD( MDH_ENDWORD : MDH_ENDWORD ) .EQ. CHAR( 0 ) ) )                ! null is found.

       MDH_ENDWORD = MDH_ENDWORD - 1

      END DO

      END
