      SUBROUTINE KPG1_NMCOL( NAME, R, G, B, STATUS )
*+
*  Name:
*     KPG1_NMCOL

*  Purpose:
*     Finds the RGB intensities of a named colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NMCOL( NAME, R, G, B, STATUS )

*  Description:
*     Given the name of a colour this routine searches the standard
*     colour set for it, and if it exists returns its R-G-B intensities.
*     An error is returned if the named colour is not in the colour set.
*     All comparisons are performed in uppercase with the blanks
*     removed.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the nearest colour in the named colour set to the
*        input RGB colour.  Note at least eighteen characters are
*        required to avoid truncation.
*     R = REAL (Returned)
*        The red intensity of the named colour to be identified.  It is
*        in the range 0.0 to 1.0.
*     G = REAL (Returned)
*        The green intensity of the named colour to be identified.  It
*        is in the range 0.0 to 1.0.
*     B = REAL (Returned)
*        The blue intensity of the named colour to be identified.  It is
*        in the range 0.0 to 1.0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     1992 March 27 (MJC):
*        Used MIT colour set with new common-variable names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management definitions

*  Global Variables:
      INCLUDE 'CTM_COM'          ! RGBs and names of colours
*        CTM_RGB( 3, CTM__NAMED ) = REAL (Read)
*           The normalised RGB intensities of the named colours.
*        CTM_NAM( CTM__NAMED ) = CHARACTER * 20 (Read)
*           The names of each of the predefined colours.

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      REAL R
      REAL G
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CTM_XCOLS         ! Needed on some linkers to acces the
                                 ! CTM block data
      INTEGER CHR_LEN            ! Length of a character string
                                 ! excluding trailing blanks

*  Local Variables:
      INTEGER
     :  I,                       ! Loop counter
     :  NC,                      ! Number of characters in the colour's
                                 ! name
     :  PC                       ! Percentage for grey level

      REAL
     :  GLEVEL                   ! A grey level

      CHARACTER * ( 24 )
     :  CNAME                    ! The colour name in uppercase and sans
                                 ! blanks

      LOGICAL                    ! True if:
     :  MATCH                    ! A colour match is found.

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make a version of the name in uppercase less blanks.

      CNAME = NAME
      CALL CHR_UCASE( CNAME )
      CALL CHR_RMBLK( CNAME )
      NC = CHR_LEN( CNAME )
      
*    First test for a grey level.  These have percentage suffices except
*    for just Grey or Gray.

      IF ( CNAME( 1:4 ) .EQ. 'GREY' .OR. CNAME( 1:4 ) .EQ. 'GRAY' ) THEN

*       Look for the grey without a percentage.

         IF ( NC .EQ. 4 ) THEN
            R = 0.75
            G = 0.75
            B = 0.75
         ELSE

*          Find percentage suffix.

            CALL CHR_CTOI( CNAME( 5:MIN( NC, 7 ) ), PC, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'CNAME', CNAME )
               CALL ERR_REP( 'KPG1_NMCOL_NSGREY',
     :           'An unknown grey level ^CNAME has been specified.',
     :           STATUS )
            ELSE
               GLEVEL = REAL( PC ) / 100.0
               R = GLEVEL
               G = GLEVEL
               B = GLEVEL
            END IF
         END IF

*    Look for the colour in the colour set.

      ELSE

*       Loop to find the colour and hence its RGB.
*       ==========================================

         MATCH = .FALSE.
         I = 1
         DO WHILE ( .NOT. MATCH .AND. I .LE. CTM__NAMED )

*          Look for the match. The colour set names are stored in
*          uppercase case with no blanks.  Read the corresponding
*          RGB values, otherwise just increment the colour counter.

            MATCH = CNAME( :NC ) .EQ. CTM_NAM( I )( :NC )
            IF ( MATCH ) THEN
               R = CTM_RGB( 1, I )
               G = CTM_RGB( 2, I )
               B = CTM_RGB( 3, I )
            ELSE
               I = I + 1
            END IF
         END DO

*       Report an error if there was no match.

         IF ( .NOT. MATCH ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CNAME', CNAME )
            CALL ERR_REP( 'KPG1_NMCOL_UNKNOWN',
     :        'An unknown colour ^CNAME has been specified.', STATUS )
         END IF
      END IF
      
      END
