*+  DYNCLR - Has the specified graphics workstation a dynamic colour 
*            representation?

      SUBROUTINE DYNCLR( WKID, DYNAMC, STATUS )
*
*    Description :
*
*     For the current GKS workstation determine whether or not the
*     colour representation is dynamic.  GKS must be in state WSOP or
*     WSAC.
*
*    Invocation :
*
*     CALL DYNCLR( WKID, DYNAMC, STATUS )
*
*    Arguments:
*
*     WKID   = INTEGER( READ )
*         GKS workstation identifier
*     DYNAMC  = LOGICAL( WRITE )
*         If true the colour representation is changeable for the
*           current GKS workstation
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Inquire the workstation type
*     Inquire whether GKS has reported an error
*     If so report context and exit
*     Inquire workstation dynamic attributes
*     Inquire whether GKS has reported an error
*     If no error occurred then
*        Set the dynamic colour table flag if appropriate
*     Else
*        Report context and exit
*     Endif
*     End
*
*    Authors :
*
*     Malcolm J. Currie  Starlink (RAL::CUR)
*
*    History :
*
*     1989 Apr 13: Original (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! global SSE definitions

*    Import :

      INTEGER
     :    WKID

*    Export :

      LOGICAL
     :    DYNAMC

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :    COLREP,              ! colour representation changeable?
     :    CONID,               ! connection identifier
     :    GSTAT,               ! graphics status
     :    FABUN,               ! dummy for GQDWKA
     :    PAREP,               ! dummy for GQDWKA
     :    PLBUN,               ! dummy for GQDWKA
     :    PMBUN,               ! dummy for GQDWKA
     :    TXBUN,               ! dummy for GQDWKA
     :    WKTR,                ! dummy for GQDWKA
     :    WTYPE                ! workstation type

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DYNAMC = .FALSE.

*    Inquire the workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Inquire whether GKS has reported an error

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_DYNCLR_WTYPE',
     :     'DYNCLR: Error while obtaining graphics workstation type',
     :     STATUS )

         GOTO 999
      END IF

*    Inquire workstation dynamic attributes

      CALL GQDWKA( WTYPE, GSTAT, PLBUN, PMBUN, TXBUN, FABUN, PAREP,
     :             COLREP, WKTR )

*    Inquire whether GKS has reported an error

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( COLREP .EQ. 1 ) DYNAMC = .TRUE.
      ELSE
         CALL ERR_REP( 'ERR_DYNCLR_QDWKA',
     :     'DYNCLR: Error while inquiring workstation dynamic '/
     :     /'attributes', STATUS )

         GOTO 999
      END IF

 999  CONTINUE
      END
