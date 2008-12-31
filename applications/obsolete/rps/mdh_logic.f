*+  MDH_LOGIC
*  -----------
*  DESCRIPTION
*  -----------
*
* Checks input to see if it is positive, negative or undefined.
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992	M. Duesterhaus (GSFC)	removed VAX RTL calls
***********************************************************************
      INTEGER FUNCTION MDH_LOGIC( INPUT )


*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) INPUT        ! String to check.

*LOCAL:

      CHARACTER*3 ANSWER        ! Uppercase shortened version of INPUT.

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      ANSWER=INPUT
      CALL UPC (ANSWER)

      IF ( ANSWER(:1) .EQ. 'T'   .OR. ANSWER .EQ. 'JA ' .OR.                        ! If True , German
     &     ANSWER     .EQ. 'DA ' .OR. ANSWER .EQ. 'CI ' .OR.                        ! Russian , ItalianP
     &     ANSWER(:1) .EQ. 'Y'   .OR. ANSWER .EQ. 'OUI' ) THEN                        ! Yes , French.

        MDH_LOGIC = 1                                                                !  Return true.

      ELSE IF ( ANSWER(:1) .EQ. 'F' .OR. ANSWER(:1) .EQ. 'N' ) THEN                ! Else if answer is negative.

        MDH_LOGIC = -1                                                                !   Return true.

      ELSE                                                                        ! Else if undefined.

        MDH_LOGIC = 0                                                                !  Return neither.

      END IF                                                                        ! End if.

      END                                                                        ! End.
