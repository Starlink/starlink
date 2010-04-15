*+  P4_COLTONUM - subroutine to convert colour string to PGPLOT index
      SUBROUTINE P4_COLTONUM (COLOUR, CVAL, RD, GR, BL, STATUS)
*    Description :
*    Invocation :
*     CALL P4_COLTONUM (COLOUR, CVAL, RD, GR, BL, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     P.N.Daly (JACH::PND)
*     K.L.Krisciunas (JACH::KEVIN)
*    History :
*        ??      : Original version                         (JFL)
*     18-Feb-1993: Remove STR$ calls                        (PND)
*     21-Apr-1993: add RGB vals for return                  (KLK)
*     12-May-1993: default to very light grey               (PND)
*      4-Aug-1994: Port to Unix                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) COLOUR                ! desired colour
*    Export :
      INTEGER CVAL                        ! corresponding PGPLOT index
      REAL RD, GR, BL		! temp red, green, blue vals (0.0 to 1.0)
*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHR_UCASE( COLOUR )

      IF ( COLOUR .EQ. 'BLACK' ) THEN
         CVAL = 14
         RD = 0.0
         GR = 0.0
         BL = 0.0
      ELSE IF ( COLOUR .EQ. 'WHITE' ) THEN
         CVAL = 15
         RD = 1.0
         GR = 1.0
         BL = 1.0
      ELSE IF ( COLOUR .EQ. 'RED' ) THEN
         CVAL = 2
         RD = 1.0
         GR = 0.0
         BL = 0.0
      ELSE IF ( COLOUR .EQ. 'ORANGE' ) THEN
         CVAL = 8
         RD = 1.0
         GR = 0.5
         BL = 0.0
      ELSE IF ( COLOUR .EQ. 'YELLOW' ) THEN
         CVAL = 7
         RD = 1.0
         GR = 1.0
         BL = 0.0
      ELSE IF ( COLOUR .EQ. 'GREEN' ) THEN
         CVAL = 3
         RD = 0.0
         GR = 1.0
         BL = 0.0
      ELSE IF ( COLOUR .EQ. 'BLUE' ) THEN
         CVAL = 4
         RD = 0.0
         GR = 0.0
         BL = 1.0
      ELSE
*       just set it to white if colour unknown
         CVAL = 1
         RD = 1.0
         GR = 1.0
         BL = 1.0
      ENDIF

      END
