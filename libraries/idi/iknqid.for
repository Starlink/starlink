*-----------------------------------------------------------------------
*+  IKNQID - Query Interactor Description

      SUBROUTINE IKNQID ( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )

*    Description :
*     Return a atring containing a description of how to perform the
*     given interaction.
*
*    Invocation :
*     CALL IKNQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     For each interactor type and identifier return a string containing
*     the description. The Ikon mouse is a locator with three triggers.
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
*     January 1989
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

*     Interactor number
      INTEGER INTID

*    Export :
*     Interactive device description
      CHARACTER * ( * ) MESSAG

*     Length of description string
      INTEGER MESLEN

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
*-

*   Check interactor types and identifiers
      IF ( INTTY .EQ. 0 ) THEN
         IF ( ( INTID .GE. 0 ) .AND. ( INTID .LT. CNLOC ) ) THEN
            MESSAG = 'Move Ikon mouse'
            MESLEN = 15
         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSEIF ( INTTY .EQ. 1 ) THEN
         IF ( ( INTID .GE. 0 ) .AND. ( INTID .LT. CNREVA ) ) THEN
            MESSAG = 'No real evaluators on Ikon'
            MESLEN = 26
         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSEIF ( INTTY .EQ. 2 ) THEN
         IF ( ( INTID .GE. 0 ) .AND. ( INTID .LT. CNIEVA ) ) THEN
            MESSAG = 'No integer evaluators on Ikon'
            MESLEN = 29
         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSEIF ( INTTY .EQ. 3 ) THEN
         IF ( ( INTID .GE. 0 ) .AND. ( INTID .LT. CNLEVA ) ) THEN
            MESSAG = 'No logical evaluators on Ikon'
            MESLEN = 29
         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSEIF ( INTTY .EQ. 4 ) THEN
         IF ( ( INTID .GE. 0 ) .AND. ( INTID .LT. CNCEVA ) ) THEN
            MESSAG = 'No character evaluators on Ikon'
            MESLEN = 31
         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSEIF ( INTTY .EQ. 5 ) THEN
         IF ( INTID .EQ. 0 ) THEN
            MESSAG = 'Push left hand button'
            MESLEN = 21

         ELSEIF ( INTID .EQ. 1 ) THEN
            MESSAG = 'Push centre button'
            MESLEN = 18

         ELSEIF ( INTID .EQ. 2 ) THEN
            MESSAG = 'Push right hand button'
            MESLEN = 22

         ELSE
            STATUS = IDI__NOINT
         ENDIF

      ELSE
         STATUS = IDI__NOINT
      ENDIF

      END

