*+  SETCR - Sets the SGS cursor position even outside the current zone

      SUBROUTINE SETCR( X, Y, STATUS )
*+
*
*    Description :
*
*     This application sets the SGS cursor position even if this lies
*     outside the current zone or GKS window.  (SGS does not allow the
*     cursor to be moved outside the zone.)
*
*    Invocation :
*
*     CALL SETCR( X, Y, STATUS )
*
*    Arguments :
*
*     X = REAL( READ )
*         x cursor position in world coordinates of the current zone
*     Y = REAL( READ )
*         y cursor position in world coordinates of the current zone
*
*    Method :
*
*     The current workstation transformation is obtained from GKS . It
*     is used to convert the supplied x,y position to NDC and then to
*     viewport 0 (base zone) co-ordinates (unless current viewport is
*     zero).  The cursor is then set to the computed position within
*     viewport 0 via GKS calls.
*
*   Bugs :
*
*    None known.
*
*   Authors :
*
*     Malcolm J. Currie  Starlink (RAL::CUR)
*
*    History :
*
*     1989 Jun 28: Original based on SGS_SETCU code (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! No implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'GKS_PAR'       ! GKS parameters

*    Import :

      REAL
     :  X,
     :  Y

*    Status :

      INTEGER STATUS

*    Local variables :

      CHARACTER
     :  DATREC(10)*80          ! Data record return by GKS inquiry

      REAL
     :  EAREA( 4 ),            ! Graphics device echo area
     :  RILPX, RILPY,          ! Initial cursor position in world
                               ! co-ordinates
     :  WINDO( 4 ),            ! Window limits in world co-ordinates
     :  VIEWP( 4 ),            ! Viewport limits in NDC
     :  X0, Y0,                ! Input co-ordinates in viewport 0
     :  XNDC, YNDC             ! Input co-ordinates in NDC

      INTEGER
     :  GSTAT,                 ! GKS error status
     :  ICNTR,                 ! Input normalisation transformation no.
     :  IESW,                  ! Echo switch
     :  IPET,                  ! Prompt/echo type
     :  ISWKID,                ! Workstation identifier
     :  ITNR,                  ! Viewport 0 normalisation transformation
                               ! number
     :  ITNR2,                 ! Initial normalisation transformation
                               ! number
     :  LDR,                   ! Length of data types
     :  MODE                   ! Operating mode of the cursor 

      LOGICAL                  ! True if:
     :  AVAIL                  ! Cursor is available on the current
                               ! workstation

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      ITNR = 0

*   Check that cursor exists

      CALL SGS_ICUAV( AVAIL )
      IF ( AVAIL ) THEN

         CALL SGS_ICURW( ISWKID )

*       Store current workstation transformation

         CALL GQCNTN( GSTAT, ICNTR )
         IF ( GSTAT .NE. 0 ) THEN
            CALL GKS_GSTAT( STATUS )
            CALL ERR_REP( 'SETCR_NOWTN',
     :        'Error returned by GQCNTN', STATUS )
            GOTO 999
         END IF

*       Is a conversion required, i.e. will the cursor position always
*       lie within the current viewport? In general, only viewport 0
*       fills the whole display surface

         IF ( ICNTR .NE. ITNR ) THEN

*          Convert given position to NDC

            CALL GQNT( ICNTR, GSTAT, WINDO, VIEWP )
            IF ( GSTAT .NE. 0 ) THEN
               CALL GKS_GSTAT( STATUS )
               CALL ERR_REP( 'SETCR_NLIMC',
     :           'Error returned by GQNT for current viewport', STATUS )
               GOTO 999
            END IF

            XNDC = ( X - WINDO( 1 ) )/( WINDO( 2 ) - WINDO( 1 ) ) *
     :             ( VIEWP( 2 ) - VIEWP( 1 ) ) + VIEWP( 1 )
            YNDC = ( Y - WINDO( 3 ) )/( WINDO( 4 ) - WINDO( 3 ) ) *
     :             ( VIEWP( 4 ) - VIEWP( 3 ) ) + VIEWP( 3 )

*          Convert from NDC to viewport 0

            CALL GQNT( ITNR, GSTAT, WINDO, VIEWP )
            IF ( GSTAT .NE. 0 ) THEN
               CALL GKS_GSTAT( STATUS )
               CALL ERR_REP( 'SETCR_NLIMB',
     :           'Error returned by GQNT for base viewport', STATUS )
               GOTO 999
            END IF

            X0 = ( XNDC - VIEWP( 1 ) )/( VIEWP( 2 ) - VIEWP( 1 ) ) *
     :             ( WINDO( 2 ) - WINDO( 1 ) ) + WINDO( 1 )
            Y0 = ( YNDC - VIEWP( 3 ) )/( VIEWP( 4 ) - VIEWP( 3 ) ) *
     :             ( WINDO( 4 ) - WINDO( 3 ) ) + WINDO( 3 )

         END IF

*       Inquire current locator state

         CALL GQLCS( ISWKID, 1, GSET, 1, GSTAT, MODE, IESW, ITNR2, 
     :               RILPX, RILPY, IPET, EAREA, LDR, DATREC )
         IF ( GSTAT .NE. 0 ) THEN
            CALL GKS_GSTAT( STATUS )
            CALL ERR_REP( 'SETCR_LOCST',
     :           'Error returned by GQNT', STATUS )
            GO TO 999
         END IF

*       Set new position

         CALL GINLC( ISWKID, 1, ITNR, X0, Y0, IPET, 
     :               EAREA( 1 ),  EAREA( 2 ),  EAREA( 3 ),  EAREA( 4 ), 
     :               LDR, DATREC )

      END IF

  999 CONTINUE

      END
