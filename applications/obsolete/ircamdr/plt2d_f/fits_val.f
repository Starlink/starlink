	SUBROUTINE FITS_VAL( NELEMENTS, ARRAY, ELEMENT,
     :                       STRING, STR_LENGTH, STATUS)
* Description :
*  Gets character value of a FITS header element
* Invocation :
*     CALL FITS_VAL( NELEMENTS, ARRAY, ELEMENT,
*     :               STRING, STR_LENGTH, STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
* Authors :
*     Sandy Leggett (SKL@JACH)
*
* History:
*     23-Feb-94
*
* Type definitions :
	IMPLICIT  NONE			! no default typing allowed

* Global constants :
	INCLUDE 'SAE_PAR'		! SSE global definitions
	INCLUDE 'CHR_ERR'		! SSE global definitions
*
* Status :
	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER
     :     NELEMENTS,                   ! number of elements of array
     :     ELEMENT,                     ! Required element of array
     :     EQUALS,                      ! Location of '=' in array
     :     QUOTE,                       ! Location of ' in array
     :     START,                       ! Start value string
     :     END,                         ! End value string
     :     STR_LENGTH,                  ! Length of value string
     :     PADST                        ! Pad string with blanks from here

        CHARACTER*(*) ARRAY(NELEMENTS)
	CHARACTER*(80)  STRING
	CHARACTER*(1)  CHR

*-
*      Force char string =
	CHR = CHAR(39)

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
           STRING = ' '
	   RETURN
	END IF

        STRING = ARRAY(ELEMENT)


        EQUALS = INDEX( STRING(1:80), '=' )
        START = EQUALS + 1
        END = INDEX( STRING(1:80), '/' )
        END = END - 1
        STR_LENGTH = END - START + 1


*      take away the single quotes from the character values
        QUOTE = 0
        QUOTE = INDEX( STRING(1:80), CHR )
        IF ( QUOTE .GT. 0 ) THEN
             START = QUOTE + 1
             QUOTE = INDEX( STRING(START:80), CHR )
             END = START + QUOTE - 2
             STR_LENGTH = END - START + 1
        END IF


        STRING(1:STR_LENGTH) = STRING(START:END)
        PADST = STR_LENGTH + 1
        STRING(PADST:80)= ' '


*      remove leading blanks
        CALL CHR_LDBLK( STRING )

	END
