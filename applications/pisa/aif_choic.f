*+  AIF_CHOIC - Obtain string with one of a number of possible values.

      SUBROUTINE AIF_CHOIC( PARNAM, OPTS, VALUE, STATUS )

*    Description :
*
*     A character variable, %VALUE, associated with the parameter name
*     %PARNAM, is obtained from the parameter system. A list of possible
*     values is given in the character variable %OPTS. Each possible
*     value is separated by a comma. The first of these possible values
*     is suggested to the parameter system as a default, if a default
*     has already been suggested for this parameter then this attempt
*     will fail and the old value will be kept.  The value returned is
*     in uppercase.
*
*     The minimum of characters that defines each option uniquely is
*     calculated and the test for membership of the list of options
*     uses only that number of characters.  However, the value returned
*     and the stored parameter is the full name in the list.
*
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL AIF_CHOIC( PARNAM, OPTS, VALUE, STATUS )
*
*    Parameters :
*
*     PARNAM = CHARACTER*(*)( READ )
*           Parameter name associated with value to be obtained.
*     OPTS   = CHARACTER*(*)( READ )
*           Contains the options, separated by commas ( , ), against
*           which VALUE will be checked.
*     VALUE  = CHARACTER*(*)( WRITE )
*           The selected option from the list in uppercase.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If an
*           error occurs during the execution of this routine STATUS
*           will be returned containing the appropriate error value.
*
*    Method :
*
*     If no error on entry then
*        Find the position of a comma in the options string.
*        If no comma is found then
*           Set comma index to length of options string.
*        Endif
*        Suggest first option to parameter system as a default value, if
*          the status is returned with a value corresponding to the
*          parameter being in an invalid state then just annul the error
*          and continue.
*        Turn the list of options into an array, and find the minimum
*          number of characters that define each option uniquely
*        Make an uppercase copy of the array
*        Do while no acceptable value is given and no errors occur
*           Get a value from parameter system.
*           If no error then
*              If the number of characters in the value is too few then
*                 Report the error
*                 Cancel the parameter and reset the default value
*              Else
*                 Set value to uppercase and remove leading blanks
*                 Loop through the array comparing the value with the
*                   (uppercase) options, using just the number of
*                   characters that define each option uniquely, and
*                   exiting the loop if the value is one of the options
*                 If the value was an option then
*                    Set the acceptable value flag.
*                    Assign the value to the uppercase full name of the
*                      option
*                    Put full name, as in the list of options, into the
*                      parameter
*                 Else
*                    Report the error
*                    Cancel the parameter and reset the default value
*                 Endif
*              Endif
*           Endif
*        Enddo
*     Endif
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*     Dave Baines (ROE::ASOC5)
*
*    History :
*
*     07/12/1983  : Original version                (ROE::ASOC5)
*     02/06/1984  : Handles variable length options (ROE::ASOC5)
*     1988 Jun 25 : Modified to KAPPA style and programming standards
*                   (RAL::CUR).
*     1990 Feb 27 : Modified to permit the shortest unambiguous
*                   abbreviation to be given and returns uppercase
*                   value (RAL::CUR).
*     1991 Sep 12 : Removed ERR_OUTs and ensured status is set before
*                   calling ERR_REP (RAL::CUR).
*     1991 Sep 13 : Fixed bug in searching (copied from PAR_CHOIC)
*                   (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! Switch off default typing

*    Global constants :

      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error constants

*    Import :

      CHARACTER*(*)
     :  PARNAM,                ! Parameter name corresponding to
                               ! variable VALUE
     :  OPTS                   ! List of possible options for VALUE

*    Export :

      CHARACTER*(*)
     :  VALUE                  ! Character variable for which value is
                               ! to be obtained

*    Status :

      INTEGER STATUS           ! Global status

*    External references :

      INTEGER CHR_INDEX        ! Index to character in string
      INTEGER CHR_LEN          ! String length ignoring trailing blanks

*    Local constants :

      INTEGER MAXOPT           ! Maximum number of options.
      PARAMETER( MAXOPT = 20 )

*    Local variables :

      LOGICAL                  ! True if:
     :  NOTOK                  ! No acceptable value obtained

      INTEGER
     :  CHCOMP,                ! Number of characters to compare
     :  COMMA,                 ! Index to comma in options string
     :  I,                     ! Loop counter
     :  MATCH,                 ! Index of the last matched from the list
     :  MINCH,                 ! Number of initial characters to compare
                               ! to obtain a option uniquely
     :  MAXNOC,                ! Maximum length of the array elements
     :  MCH( MAXOPT ),         ! Number of initial characters to compare
                               ! to obtain each option uniquely
     :  NCV,                   ! Number of characters in value
     :  NELM                   ! Number of options

      CHARACTER * 20
     :  ARRAY( MAXOPT ),       ! The array of options.
     :  UARRAY( MAXOPT )       ! The array of options (uppercase)

*-

*    Check for an error on entry.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Find first comma in options string.

         COMMA = CHR_INDEX( OPTS, ',' )

*       Check for comma not found.

         IF ( COMMA .EQ. 0 ) THEN

*          Set index to comma to be options string length plus 1.

            COMMA = CHR_LEN( OPTS ) + 1
         END IF

*       Suggest a default value to the parameter system---the first
*       option.

         CALL PAR_DEF0C( PARNAM, OPTS(1:COMMA-1), STATUS )

*       Check for a dynamic default having already been set.

         IF ( STATUS .EQ. PAR__INVST ) THEN

*           Clear the error and continue.

             CALL ERR_ANNUL( STATUS )
         END IF

*       Separate the strings into an array and find the minimum number
*       of characters to compare.

         CALL AIF_ABSET( ',', OPTS, ARRAY, NELM, MCH, MINCH, MAXNOC,
     :                   STATUS )

*       Get an uppercase set of the array for comparisons, but want to
*       keep the original case for later storage.

         IF ( STATUS .EQ. SAI__OK ) THEN
            DO  I = 1, NELM
               UARRAY( I ) = ARRAY( I )
               CALL CHR_UCASE( UARRAY( I ) )
            END DO
         END IF

*       Initialise NOTOK to start off the loop.

         NOTOK = .TRUE.

*       Repeat until an acceptable value obtained or an error occurs.

         DO WHILE ( ( NOTOK ) .AND. ( STATUS .EQ. SAI__OK ) )

*          Get a value from the parameter system.

            CALL PAR_GET0C( PARNAM, VALUE, STATUS )

*          Check for an error.

            IF ( STATUS .EQ. SAI__OK ) THEN

*             Convert the input value to uppercase and remove leading
*             blanks.

               CALL CHR_LDBLK( VALUE )
               CALL CHR_UCASE( VALUE )

*             Get the length of the value.
 
               NCV = CHR_LEN( VALUE )

*             Check if we have an acceptable value.  First to see if
*             the string identifies the item uniquely.

               IF ( NCV .LT. MINCH ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'AIF_CHOIC__INSCH',
     :              'AIF_CHOIC: insufficient characters to select '/
     :              /'an option.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*                Try again to obtain the value, so must cancel the
*                incorrect attempt.

                  CALL PAR_CANCL( PARNAM, STATUS )

*                Suggest the value in DEFAUL to the parameter system
*                as a dynamic default

                  CALL PAR_DEF0C( PARNAM, OPTS( 1:COMMA-1 ), STATUS )

               ELSE

                  I = 1
                  MATCH = 0

*                The comparison uses just the number of characters that
*                define each option uniquely, unless the given value is
*                longer.  The latter number of comparison characters
*                prevents acceptance of a value which begins with one
*                of the options, but actually isn't one.  FOr instance
*                if the options were '1', '10', and '13' was entered,
*                without this check the erroneous value '1' would be
*                returned. Do not exit the loop when a match has been
*                found in case there is an option which in full equals
*                the start of another option, e.g. integers.

                  DO WHILE ( I .LE. NELM )
                     CHCOMP = MAX( NCV, MCH( I ) )
                     IF ( VALUE( :CHCOMP ) .EQ.
     :                    UARRAY( I )( :CHCOMP ) ) MATCH = I

                     I = I + 1
                  END DO

                  IF ( MATCH .NE. 0 ) THEN

*                   Terminate the loop.

                     NOTOK = .FALSE.
                     VALUE = UARRAY( MATCH )

*                   Assign the value to the parameter.

                     CALL PAR_PUT0C( PARNAM, ARRAY( MATCH ), STATUS )
                  ELSE

*                   An unacceptable value was obtained. Report the
*                   error.

                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_SETC( 'OPTS', OPTS )
                     CALL ERR_REP( 'AIF_CHOIC__NOUNQ',
     :                 'AIF_CHOIC: the text given does not specify '/
     :                 /'the option uniquely.  Options are: ^OPTS',
     :                 STATUS )
                     CALL ERR_FLUSH( STATUS )

*                   Try again to obtain the value, so must cancel the
*                   incorrect attempt.

                     CALL PAR_CANCL( PARNAM, STATUS )

*                   Suggest the value in DEFAUL to the parameter system
*                   as a dynamic default.

                     CALL PAR_DEF0C( PARNAM, OPTS( 1:COMMA-1 ), STATUS )

                  END IF
               END IF
            END IF
         END DO
      END IF

      END
* $Id$
