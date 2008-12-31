*+.
*-  Author M.D.C.Harris ( R.A.L )                    9th June 1987.
*   Aug 1992   M. Duesterhaus	Remove VAX specific code
**********************************************************************
      SUBROUTINE MDH_COMM( COMM , MASKNAMES )
 
*  -----------
*  DESCRIPTION
*  -----------
 
*  This subroutine submits a command after setting up the relevant mask.
* To request a function switched on enter relevant letter except 'W'
* which switches it off. By default no symbols, logical names or keypad
* definitions are passed and wait is asked for.
 
*  ---------
*  VARIABLES
*  ---------
 
*INPUT:

      CHARACTER*(*) COMM     	! Command to be passed.
     & ,            MASKNAMES   ! Mask specification.

*LOCAL:

       INTEGER       STATUS      ! Error indicator.

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------
 
*      CALL SPAWN( COMM, LEN(COMM) ,STATUS) 
	write(*,*) ' cmd '//comm(:len(comm))
      call sys_docommand( comm(:len(comm)), status)
      IF (STATUS.GT.1) THEN
	WRITE(*,*)' ERROR OCCURRED IN MDH_COMM: ',STATUS
      END IF 
      END
