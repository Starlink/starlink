*+  APPGIR - get two integer values from parameter system within given range
      SUBROUTINE APPGIR( PARNAM, RANGE, VALUES, STATUS )
*    Description :
*     Values will be obtained for the two element array VALUES, associated
*     with the parameter name PARNAM, which will lie in the  range RANGE(1)
*     to RANGE(2). The values given in RANGE will be suggested to the parameter
*     system as the default values for VALUES, if defaults values have already
*     been suggested for this parameter this attempt will fail and the previous
*     values will be kept.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL APPGIR( PARNAM, RANGE, VALUES, STATUS )
*    Parameters :
*     PARNAM = CHARACTER*(*)( READ )
*           Parameter name associated with the values to be returned in VALUES.
*     RANGE( 2 ) = INTEGER( READ )
*           Defines the limits which will be imposed on the values to be
*           returned in VALUES and will also be suggested to the parameter
*           system as the default values for VALUES if no default values
*           heva already been set.
*     VALUES( 2 ) = INTEGER( WRITE )
*           Will only contain valid values within the range RANGE(1) to
*           RANGE(2) if STATUS does not have an error value on exit.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Suggest default values to the parameter system
*        If status has an error value indicating parameter in an invalid state
*          then defaults must have already been set for this parameter so just
*          annul the error and continue
*        Do while no acceptable values obtained and no errors occur
*           Get values from parameter system
*           If no error then
*              If two values were input then
*                 If values are unacceptable then
*                    Report as an error and cancel parameter values
*                 Else
*                    Terminate loop
*                 Endif
*              Else
*                 Report as an error and cancl parameter values
*              Endif
*           Endif
*        Enddo
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     17/02/1984 : Original version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*)
     :  PARNAM ! parameter name associated with values to be obtained
      INTEGER
     :  RANGE( 2 ) ! minimum and maximum acceptable values to be returned
*    Export :
      INTEGER
     :  VALUES( 2 ) ! array of values to be obtained from the parameter system
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  NUMBER ! number of values returned by enviroment
      LOGICAL
     :  NOTOK ! true while obtaining acceptable values
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       suggest default values to the parameter system
         CALL PAR_DEF1I( PARNAM, 2, RANGE, STATUS )

*       check for defaults already set
         IF( STATUS .EQ. PAR__INVST ) THEN

            CALL ERR_ANNUL( STATUS )
         ENDIF

*       initialise NOTOK
         NOTOK = .TRUE.

*       enquire window limits
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*          get values for VALUES from parameter system
            CALL PAR_GETVI( PARNAM, 2, VALUES, NUMBER, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             check that two values were input
               IF( NUMBER .EQ. 2 ) THEN

*                check input values within range
                  IF( ( VALUES( 1 ) .LT. RANGE( 1 ) ) .OR.
     :              ( VALUES( 2 ) .GT. RANGE( 2 ) ) .OR.
     :              ( VALUES(1) .GT. VALUES(2) ) ) THEN

                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'START', VALUES( 1 ) )
                     CALL MSG_SETI( 'END', VALUES( 2 ) )
                     CALL MSG_SETI( 'MIN', RANGE( 1 ) )
                     CALL MSG_SETI( 'MAX', RANGE( 2 ) )
                     CALL ERR_REP( 'ERR_VALUES',
     :                 'Values of ^START to ^END not allowed, must be '/
     :                 /'in range ^MIN to ^MAX . Try again please.',
     :                 STATUS )
                     CALL ERR_FLUSH( STATUS )
                     CALL PAR_CANCL( PARNAM, STATUS )
                  ELSE

                     NOTOK = .FALSE.
                  ENDIF
               ELSE

*                here if other than two values entered
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'NUMBER', NUMBER )
                  CALL ERR_REP( 'ERR_NUMBER',
     :              '2 values expected, ^NUMBER entered. Try again '/
     :              /'please.', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARNAM, STATUS )
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      END
