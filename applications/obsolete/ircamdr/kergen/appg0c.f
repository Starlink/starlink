*+  APPG0C - gets character variable with one of a number of possible values
      SUBROUTINE APPG0C( PARNAM, SIGNIF, OPTS, VALUE, STATUS )
*    Description :
*     A character variable, VALUE, associated with the parameter name PARNAM
*     is obtained from the parameter system. A list of possible values is given
*     in the character variable OPTS where each possible value is of length
*     SIGNIF characters and the values are separated by commas. The first of
*     these possible values is suggested to the parameter system as a default,
*     if a defualt has already been suggested for this parameter then this
*     attempt will fail and the old value will be kept. Only the first SIGNIF
*     characters of VALUE will be compared with the values given in OPTS when
*     determining if an acceptable value has been given.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL APPG0C( PARNAM, SIGNIF, OPTS, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHAR*(*)( READ )
*           Parameter name associated with VALUE.
*     SIGNIF = INTEGER( READ )
*           Gives the number of characters of the reply which will be compared
*           with the specified options.
*     OPTS   = CHAR*(*)( READ )
*           Contains the options, each of SIGNIF characters in length and
*           separated by commas ( , ), against which VALUE will be checked.
*     VALUE  = CHAR*(*)( UPDATE )
*           Value returned will be valid only if STATUS is not set to an error
*           value.
*     STATUS = INTEGER( UPDATE )
*      This is the global status, if this variable has an error value on entry
*      then an immediate return will occur. If an error occurs during the
*      execution of this routine STATUS will be returned containing the
*      appropriate error value.
*    Method :
*     If no error on entry then
*        Suggest first option to parameter system as a default value, if the
*          status is returned with a value corresponding to the parameter
*          being in an invalid state then just annul the error and continue.
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
*     07/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*)
     :  PARNAM, ! parameter name corresponding to variable VALUE
     :  OPTS    ! list of possible options for VALUE
      INTEGER
     :  SIGNIF ! only 1st SIGNIF chars of VALUE checked against options
*    Export :
      CHARACTER*(*)
     :  VALUE ! character variable for which value to be obtained
*    Status :
      INTEGER STATUS
*    External references :
      LOGICAL INSET
*    Local variables :
      LOGICAL
     :  NOTOK ! will be .TRUE. while no acceptable value obtained
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       suggest default value to the parameter system
         CALL PAR_DEF0C( PARNAM, OPTS(1:SIGNIF), STATUS )

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
               IF( INSET( OPTS, VALUE(1:SIGNIF) ) ) THEN

*                terminate loop
                  NOTOK = .FALSE.
               ELSE

*                unacceptable value, report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'VALUE', VALUE )
                  CALL MSG_SETC( 'OPTS', OPTS )
                  CALL ERR_REP( 'ERR_APPG0C',
     :              '^VALUE is not acceptable. Options are : ^OPTS',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARNAM, STATUS )
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      END
