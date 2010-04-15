*+  P4_NUMTOCOL - subroutine to convert colour index to PGPLOT representation
      SUBROUTINE P4_NUMTOCOL( COLOUR, CVAL, RD, GR, BL, STATUS )
*    Description :
*     This routine converts CVAL into a COLOUR and respresentation.
*     The argument order is preserved for compatability with P4_COLTONUM
*    Invocation :
*     CALL P4_NUMTOCOL( COLOUR, CVAL, RD, GR, BL, STATUS )
*    Parameters :
*     COLOUR     = CHARACTER*(*)( WRITE )
*        The returned pen colour
*     CVAL       = INTEGER( READ )
*        The input pen number
*     RD         = INTEGER( WRITE )
*        The gun level for red
*     GR         = INTEGER( WRITE )
*        The gun level for green
*     BL         = INTEGER( WRITE )
*        The gun level for blue
*     STATUS     = INTEGER( UPDATE )
*        The inherited ADAM status return
*    Method :
*    Deficiencies :
*     The SECOND argument is the input argument NOT the first.
*     This is to maintain compatability with P4_COLTONUM.
*    Bugs :
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     12-May-1993: Original version                         (PND)
*      4-Aug-1994: Port to Unix                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER CVAL                        ! corresponding PGPLOT index
*    Export :
      CHARACTER*(*) COLOUR                ! desired colour
      REAL RD, GR, BL		          ! red, green, blue vals (0.0 to 1.0)
*    Status :
      INTEGER STATUS                      ! inherited status return
*-

*    Return if status on entry is not ADAM__OK
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Now set the gun levels according to the pen index
      IF ( CVAL .EQ. 1 ) THEN

         COLOUR = 'DARK'
         RD = 0.1
         GR = 0.1
         BL = 0.1
      ELSE IF ( CVAL .EQ. 2 ) THEN

         COLOUR = 'RED'
         RD = 1.0
         GR = 0.0
         BL = 0.0
      ELSE IF ( CVAL .EQ. 3 ) THEN

         COLOUR = 'GREEN'
         RD = 0.0
         GR = 1.0
         BL = 0.0
      ELSE IF ( CVAL .EQ. 4 ) THEN

         COLOUR = 'BLUE'
         RD = 0.0
         GR = 0.0
         BL = 1.0
      ELSE IF ( CVAL .EQ. 5 ) THEN

         COLOUR = 'CYAN'
         RD = 0.0
         GR = 1.0
         BL = 1.0
      ELSE IF ( CVAL .EQ. 6 ) THEN

         COLOUR = 'MAGENTA'
         RD = 1.0
         GR = 0.0
         BL = 1.0
      ELSE IF ( CVAL .EQ. 7 ) THEN

         COLOUR = 'YELLOW'
         RD = 1.0
         GR = 1.0
         BL = 0.0
      ELSE IF ( CVAL .EQ. 8 ) THEN

         COLOUR = 'ORANGE'
         RD = 1.0
         GR = 0.5
         BL = 0.0
      ELSE IF ( CVAL .EQ. 9 ) THEN

         COLOUR = 'GR-YLW'
         RD = 0.5
         GR = 1.0
         BL = 0.0
      ELSE IF ( CVAL .EQ. 10 ) THEN

         COLOUR = 'GR-CYN'
         RD = 0.0
         GR = 1.0
         BL = 0.5
      ELSE IF ( CVAL .EQ. 11 ) THEN

         COLOUR = 'BL-CYN'
         RD = 0.0
         GR = 0.5
         BL = 1.0
      ELSE IF ( CVAL .EQ. 12 ) THEN

         COLOUR = 'BL-MAG'
         RD = 0.5
         GR = 0.0
         BL = 1.0
      ELSE IF ( CVAL .EQ. 13 ) THEN

         COLOUR = 'RD-MAG'
         RD = 1.0
         GR = 0.0
         BL = 0.5
      ELSE IF ( CVAL .EQ. 14 ) THEN

         COLOUR = 'DARK'
         RD = 0.1
         GR = 0.1
         BL = 0.1
      ELSE IF ( CVAL .EQ. 15 ) THEN

         COLOUR = 'LIGHT'
         RD = 0.9
         GR = 0.9
         BL = 0.9
      ELSE

         COLOUR = 'LIGHT'
         RD = 0.95
         GR = 0.95
         BL = 0.95
      ENDIF

      END
