*-----------------------------------------------------------------------
*+  IKNENI - Enable interaction

      SUBROUTINE IKNENI ( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
     :                    EXTRN, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIIENI.
*     The arguments are identical to those in IIIENI.
*
*    Invocation :
*     CALL IKNENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
*    :             EXTRN, STATUS )
*
*    Method :
*     Verify the input arguments and save them in the common blocks
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     December 1990  Changed name from IIIENI
*     April 1991  Added interaction flags
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     Interactor type
      INTEGER INTTY

*     Interactor identifier
      INTEGER INTID

*     Object type
      INTEGER OBJTY

*     Object identifier
      INTEGER OBJID

*     Interactive operation
      INTEGER INTOP

*     Exit trigger number
      INTEGER EXTRN

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMINT)'
*-

*   Recover the characterisitics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Check interactor types and interactor identifiers
      IF ( INTTY .EQ. 0 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNLOC - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSEIF ( INTTY .EQ. 1 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNREVA - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSEIF ( INTTY .EQ. 2 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNIEVA - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSEIF ( INTTY .EQ. 3 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNLEVA - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSEIF ( INTTY .EQ. 4 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNCEVA - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSEIF ( INTTY .EQ. 5 ) THEN
         IF ( ( INTID .LT. 0 ) .OR. ( INTID .GT. CNTRIG - 1 ) ) THEN
            STATUS = IDI__NOINT
            GOTO 99
         ENDIF
      ELSE
         STATUS = IDI__NOINT
         GOTO 99
      ENDIF

*   Check object types and identifiers
      IF ( OBJTY .EQ. 0 ) THEN

      ELSEIF ( OBJTY .EQ. 1 ) THEN
         IF ( ( OBJID .LT. 0 ) .OR. ( OBJID .GT. CURN - 1 ) ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF
      ELSEIF ( OBJTY .EQ. 2 ) THEN
         IF ( ( OBJID .LT. 0 ) .OR. ( OBJID .GT. CNITT - 1 ) ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF
      ELSEIF ( OBJTY .EQ. 3 ) THEN
         IF ( ( OBJID .LT. 0 ) .OR. ( OBJID .GT. CNLUT - 1 ) ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF
      ELSEIF ( OBJTY .EQ. 4 ) THEN
         IF ( ( OBJID .LT. 0 ) .OR. ( OBJID .GT. CNROI - 1 ) ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF
      ELSEIF ( OBJTY .EQ. 5 ) THEN
         IF ( ( OBJID .LT. 0 ) .OR. ( OBJID .GT. CNMEM - 1 ) ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF
      ELSEIF ( OBJTY .EQ. 6 ) THEN
         IF ( OBJID .NE. 0 ) THEN
            STATUS = IDI__NOOBJ
            GOTO 99
         ENDIF

      ELSE
         STATUS = IDI__NOOBJ
         GOTO 99
      ENDIF

*   Valid interactive operations are 0 to 7
      IF ( ( INTOP .LT. 0 ) .OR. ( INTOP .GT. 7 ) ) THEN
         STATUS = IDI__NOINT
         GOTO 99
      ENDIF

*   Check exit trigger
      IF ( ( EXTRN .LT. 0 ) .OR. ( EXTRN .GT. CNTRIG - 1 ) ) THEN
         STATUS = IDI__NOINT
         GOTO 99
      ENDIF

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Save the interaction in the common block if there is room
      IF ( CINTN .LT. MAXINT ) THEN

         CINTTY( CINTN ) = INTTY
         CINTID( CINTN ) = INTID
         COBJTY( CINTN ) = OBJTY
         COBJID( CINTN ) = OBJID
         CINTOP( CINTN ) = INTOP
         CEXTRN( CINTN ) = EXTRN
         CINTFL( CINTN ) = 1
         CINTN = CINTN + 1

*   Otherwise flag an error
      ELSE
         STATUS = IDI__COOVF
      ENDIF

  99  CONTINUE

      END

