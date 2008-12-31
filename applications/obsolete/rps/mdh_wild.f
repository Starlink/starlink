*+  MDH_WILD
*-  Author M.D.C.Harris ( R.A.L )                    10th March 1987.
*	9 Apr 1992	M. Duesterhaus (GSFC)	remove VAX RTL calls
*	7/29/93		P. Brisco		Explicitly declared i
**********************************************************************
      LOGICAL FUNCTION MDH_WILD( STRING1 , STRING2 )

*  -----------
*  DESCRIPTION
*  -----------

* Matches two strings containing wild card characters.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) STRING1
     & ,            STRING2

*LOCAL:

      CHARACTER*132 STRING3	! Uppercase version of STRING1.
     & ,            STRING4	! Uppercase version of STRING2.
      INTEGER       ST1 , ST2   ! Starts of fieldnames.
     & ,            FN1 , FN2   ! Ends of fieldnames.
	INTEGER i

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      INTEGER       MDH_ENDWORD                        ! Gets position of end of word.

*  --------------------------------------------------------
*  COMMON BLOCKS AND PARAMETER, DATA AND INCLUDE STATEMENTS
*  --------------------------------------------------------

      FN1 = MDH_ENDWORD( STRING1 )
      FN2 = MDH_ENDWORD( STRING2 )
      STRING3 = STRING1
      STRING4 = STRING2
      I = 1
      DO I=1,FN1
	ST1=I
	IF (STRING3(ST1:ST1) .NE. ' ') GOTO 45
      END DO
  45  DO  I=1,FN2
	ST2=I
	IF (STRING4(ST2:ST2) .NE. ' ') GOTO 55
      END DO

  55  continue
      CALL UPC( STRING3(st1:fn1))
      CALL UPC( STRING4 (st2:fn2))
      MDH_WILD = (STRING3( ST1 : FN1 ) .EQ. STRING4( ST2 : FN2 ))

      END                                                                        ! Return field number.
