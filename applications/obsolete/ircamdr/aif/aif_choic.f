*+  AIF_CHOIC - Obtain string with one of a number of possible values.
      SUBROUTINE AIF_CHOIC( PARNAM, OPTS, VALUE, STATUS )
*    Description :
*     A character variable, VALUE, associated with the parameter name
*     PARNAM, is obtained from the parameter system. A list of possible
*     values is given in the character variable OPTS. Each possible
*     value is separated by a comma. The first of these possible values
*     is suggested to the parameter system as a default, if a defualt
*     has already been suggested for this parameter then this attempt
*     will fail and the old value will be kept.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*    Invocation :
*     CALL AIF_CHOIC( PARNAM, OPTS, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHARACTER*(*)( READ )
*           Parameter name associated with value to be obtained.
*     OPTS   = CHARACTER*(*)( READ )
*           Contains the options, separated by commas ( , ), against
*           which VALUE will be checked.
*     VALUE  = CHARACTER*(*)( WRITE )
*           Value returned will be valid only if STATUS is not set to
*           an error value.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If an
*           error occurs during the execution of this routine STATUS
*           will be returned containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Find the position of a comma in the options string.
*        If no comma is found then
*           Set comma index to length of options string.
*        Endif
*        Suggest first option to parameter system as a default value, if
*        the status is returned with a value corresponding to the
*        parameter being in an invalid state then just annul the error
*        and continue.
*        Do while no acceptable value is given and no errors occur
*           Get a value from parameter sytem.
*           If no error then
*              If the value is acceptable then
*                 Set the acceptable value flag.
*              Else
*                 Report as an error and cancel the parameter value.
*              Endif
*           Endif
*        Enddo
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     07/12/1983 : Original version                (ROE::ASOC5)
*     02/06/1984 : Handles variable length options (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE ! switch off default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'PAR_ERR' ! parameter system error constants
*    Import :
      CHARACTER*(*)
     :  PARNAM, ! parameter name corresponding to variable VALUE
     :  OPTS    ! list of possible options for VALUE
*    Export :
      CHARACTER*(*)
     :  VALUE ! character variable for which value to be obtained
*    Status :
      INTEGER STATUS ! global status
*    External references :
      LOGICAL INSET ! checks for set membership
      INTEGER CHR_INDEX ! index to character in string
      INTEGER CHR_LEN ! string length ignoring trailing blanks
*    Local variables :
      LOGICAL
     :  NOTOK ! will be .TRUE. while no acceptable value obtained
      INTEGER
     :  COMMA ! index to comma in options string
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       find first comma in options string
         COMMA = CHR_INDEX( OPTS, ',' )

*       check for comma not found
         IF( COMMA .EQ. 0 ) THEN

*          set index to comma to be options string length plus 1
            COMMA = CHR_LEN( OPTS ) + 1
         ENDIF

*       suggest default value to the parameter system
         CALL PAR_DEF0C( PARNAM, OPTS(1:COMMA-1), STATUS )

*       check for a dynamic default having already been set
         IF( STATUS .EQ. PAR__INVST ) THEN

*           clear the error and continue
             CALL ERR_ANNUL( STATUS )
         ENDIF

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       repeat until acceptable value obtained or error occurs
         DO WHILE( ( NOTOK ) .AND. ( STATUS .EQ. SAI__OK ) )

*          get value from parameter system
            CALL PAR_GET0C( PARNAM, VALUE, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             check if have acceptable value
               IF( INSET( OPTS, VALUE ) ) THEN

*                terminate loop
                  NOTOK = .FALSE.
               ELSE

*                unacceptable value, report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'VALUE', VALUE )
                  CALL MSG_SETC( 'OPTS', OPTS )
                  CALL ERR_REP( 'ERR_CHOICE',
     :              '^VALUE is not acceptable. Options are : ^OPTS',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARNAM, STATUS )
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      END
