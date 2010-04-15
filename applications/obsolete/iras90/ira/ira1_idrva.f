      SUBROUTINE IRA1_IDRVA( VALUE, NAME, NC, XPOS, YPOS, STYLE, ACC,
     :                       CONTXT, STATUS )
*+
*  Name:
*     IRA1_IDRVA

*  Purpose:
*     Plot a formatted longitude or latitude value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IDRVA( VALUE, NAME, NC, XPOS, YPOS, STYLE, ACC,
*                      CONTXT, STATUS )

*  Description:
*     Does the work for IRA_DRVAL.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        Longitude or latitude value to display.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the sky coordinate system (without equinox).
*     NC = INTEGER (Given)
*        The axis index; 1 for longitude values, 2 for latitude values.
*     XPOS = REAL (Given)
*        The X coordinate of the text string reference positions.
*     YPOS = REAL (Given)
*        The Y coordinate of the text string reference positions.
*     STYLE = INTEGER (Given)
*        The style no. required. Zero and three not allowed.
*     ACC = DOUBLE PRECISION (Given)
*        The accuracy required for the displayed value.
*     CONTXT = CHARACTER * ( * ) (Given and Returned)
*        On entry, holds information about the previous value plotted.
*        On exit, holds information about the value just plotted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     9-NOV-1992 (DSB):
*        Graphics options added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Read)
*           Graphics options values.

*  Arguments Given:
      DOUBLE PRECISION VALUE
      CHARACTER NAME*(*)
      INTEGER NC
      REAL XPOS
      REAL YPOS
      INTEGER STYLE
      DOUBLE PRECISION ACC

*  Arguments Given and Returned:
      CHARACTER CONTXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a string.

*  Local Constants:
      REAL SUPOFF                ! Offset as a fraction of a character
                                 ! height from the top of a full size
                                 ! character to the bottom of the
                                 ! superscript unit symbol.
      PARAMETER ( SUPOFF = 0.15 )

      REAL SUPSIZ                ! Size of a superscript unit symbol, as
                                 ! a fraction of a full size character.
      PARAMETER ( SUPSIZ = 0.9 )

      REAL MARSIZ                ! Width of margin around the print box
                                 ! as a fraction of a full size
                                 ! character.
      PARAMETER ( MARSIZ = 0.55 )

      INTEGER MAXDP              ! Maximum no. of decimal places.
      PARAMETER ( MAXDP = 6 )

*  Local Variables:
      REAL             AR
      INTEGER          DP
      INTEGER          FEND( 4 )
      INTEGER          FLD
      INTEGER          FLEN( 4 )
      CHARACTER        FTEXT( 4 )*15
      INTEGER          FIELDS(4)
      CHARACTER        FMT*10
      REAL             H
      REAL             HT
      REAL             LOGACC
      INTEGER          LTDP
      INTEGER          LTEXT
      DOUBLE PRECISION LACC
      DOUBLE PRECISION LVALUE
      INTEGER          NF
      INTEGER          NPEN
      INTEGER          NPR
      REAL             P10
      LOGICAL          PFLAGS( 5 )
      REAL             REFX
      REAL             REFY
      CHARACTER        SIGN*1
      REAL             SP
      CHARACTER        TDP*3
      CHARACTER        TEXT*50
      CHARACTER        TXJ*2
      REAL             TXTHGT    ! Text height.
      CHARACTER        UNIT( 3 )*1
      REAL             UX
      REAL             UY
      INTEGER          WKID
      REAL             X1        ! Lower X limit of SGS zone.
      REAL             X2        ! Upper X limit of SGS zone.
      REAL             XM        ! X size (in metres) of SGS zone.
      REAL             Y1        ! Lower Y limit of SGS zone.
      REAL             Y2        ! Upper Y limit of SGS zone.
      REAL             YM        ! Y size (in metres) of SGS zone.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if text height is zero or negative.
      IF( ACM_DROPT( 2 ) .LE. 0.0D0 ) GO TO 999

*  Inquire current SGS text attributes and pen.
      CALL SGS_ITXA( NF, NPR, HT, AR, UX, UY, SP, TXJ )
      CALL SGS_IPEN( NPEN )

*  Inquire the current GKS workstation ID.
      CALL SGS_ICURW( WKID )

*  Set the current SGS text justification to CL.
      CALL SGS_STXJ( 'CL' )

*  Set up the text height.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      TXTHGT = ACM_DROPT( 2 )*MAX( X2 - X1, Y2 - Y1 )
      CALL SGS_SHTX( TXTHGT )

*  Set up the SGS pen.
      CALL SGS_SPEN( NINT( ACM_DROPT( 12 ) ) )

*  Save the total height of the box enclosing the whole string. The
*  character height is increased to add a margin at top and bottom and
*  room for any superscript units symbols.
      IF( STYLE .EQ. 2 ) THEN
         H = TXTHGT*( 1.0 - SUPOFF + SUPSIZ + 2.0*MARSIZ )
      ELSE
         H = TXTHGT*( 1.0 + 2.0*MARSIZ )
      END IF

*  First deal with styles other than 5.
      IF( STYLE .NE. 5 ) THEN

*  If the supplied coordinate is a Right Ascension value, convert it to
*  four separate integer fields holding hours, minutes, seconds, and
*  fraction of a second. The fourth field represents MAXDP decimal
*  places and is the integer to the left of the decimal point. The units
*  symbols for the first three fields are also set up.
         IF( NC .EQ. 1 .AND. NAME .EQ. 'EQUATORIAL' ) THEN
            CALL SLA_DR2TF( MAXDP, VALUE, SIGN, FIELDS )
            UNIT( 1 ) = 'h'
            UNIT( 2 ) = 'm'
            UNIT( 3 ) = 's'

*  Do the same for non-RA values. Fields are degrees, arc-minutes,
*  arc-seconds, and fraction of an arc-second.
         ELSE
            CALL SLA_DR2AF( MAXDP, VALUE, SIGN, FIELDS )
            UNIT( 1 ) = 'o'
            UNIT( 2 ) = ''''
            UNIT( 3 ) = '"'

         END IF

*  Round the field values to give the required accuracy and indicate
*  which fields can be printed. The number of decimal places represented
*  by the fourth field is also changed to give the required accuracy.
         CALL IRA1_FTOL( ACC, NAME, NC, MAXDP, FIELDS, DP, PFLAGS,
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Produce strings holding the first three field values.
         DO FLD = 1, 3
            WRITE( FTEXT( FLD ), * ) FIELDS( FLD )
            CALL CHR_RMBLK( FTEXT( FLD ) )
            FLEN( FLD ) = CHR_LEN( FTEXT( FLD ) )
         END DO

*  Generate a format statement which will format the fourth field with
*  the correct number of leading zeros.
         IF (DP .NE. 0) THEN
            WRITE( TDP, '(I3)' ) DP
            CALL CHR_RMBLK( TDP )
            LTDP = CHR_LEN( TDP )
            FMT = '(I'//TDP( :LTDP )//'.'//TDP( :LTDP )//')'
         ELSE
            FMT = '(I1)'
         END IF

*  Produce a string holding the fourth field value.
         WRITE( FTEXT( 4 ), FMT ) FIELDS( 4 )
         CALL CHR_RMBLK( FTEXT( 4 ) )
         FLEN( 4 ) = CHR_LEN( FTEXT( 4 ) )

*  If required, suppress the printing of redundant fields and save the
*  current context.
         CALL IRA1_SFLD( FIELDS, SIGN, FTEXT, FLEN, NC, CONTXT, PFLAGS,
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the total string to be printed to hold the sign if the
*  sign is to be printed, and to be blank otherwise.
         IF( PFLAGS( 5 ) ) THEN
            TEXT( 1:1 ) = SIGN
            LTEXT = 2
         ELSE
            TEXT = ' '
            LTEXT = 1
         END IF

*  Append the text of each printable field to the total string, with a
*  single space between each field. Also store the offset into the
*  total string at which the last character in each field is stored.
         DO FLD = 1, 4
            IF( PFLAGS( FLD ) ) THEN

               TEXT( LTEXT: ) = FTEXT( FLD )( :FLEN( FLD ) )//' '
               LTEXT = LTEXT + FLEN( FLD ) + 1
               FEND( FLD ) = LTEXT - 2

            END IF
         END DO

*  Correct the total number of character of the string
         LTEXT = MAX( 1, LTEXT - 1 )

*  If the fourth field was included, replace the space between the third
*  and fourth fields by a dot (the decimal point), and reduce the total
*  length to exclude the trailing space.
         IF( PFLAGS( 4 ) ) THEN
            TEXT( FEND( 3 ) + 1 : FEND( 3 ) + 1 ) = '.'
            LTEXT = LTEXT - 1
         END IF

*  This routine plots text with CL justification. Since this may not be
*  what the user wants, modify the supplied reference position to make
*  it appear that the string is justified according to the current SGS
*  text justification settings. IRA1_TREF also attempt to clear any
*  existing graphics from the area covered by the character.
         REFX = XPOS
         REFY = YPOS
         CALL IRA1_TREF( TEXT( : LTEXT ), TXJ, H, UX, UY, TXTHGT*MARSIZ,
     :                   WKID, REFX, REFY, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Output the text string at the modified reference position.
         CALL SGS_TX( REFX, REFY, TEXT( :LTEXT ) )

*  If superscript units symbols are to be displayed...
         IF( STYLE .EQ. 2 ) THEN

*  Loop round each of the three units symbols, displaying each if the
*  corresponding field was displayed.
            DO FLD = 1, 3
               IF( PFLAGS( FLD ) ) THEN
                  CALL IRA1_USYM( WKID, UNIT( FLD ), REFX, REFY, TEXT,
     :                            FEND( FLD ), TXTHGT, SUPSIZ, SUPOFF,
     :                            UX, UY, STATUS )
               END IF
            END DO

         END IF

*  Now deal with style 5.
      ELSE

*  If the supplied coordinate is a Right Ascension value, convert it
*  and the accuracy to hours, otherwise convert it and the accuracy
*  to degrees.
         IF( NAME .EQ. 'EQUATORIAL' .AND. NC .EQ. 1 ) THEN
            LVALUE = VALUE*IRA__R2TH
            LACC = ACC*IRA__R2TH
            UNIT( 1 ) = 'h'
         ELSE
            LVALUE = VALUE*IRA__RTOD
            LACC = ACC*IRA__RTOD
            UNIT( 1 ) = 'o'
         END IF

*  Find the number of decimal places needed to give the required
*  accuracy
         IF( ABS( LACC ) .GT. 0.0 ) THEN
            LOGACC = -LOG10( ABS( LACC ) )
            DP = INT( LOGACC )
            IF( LOGACC .NE. REAL( DP ) ) DP = DP + 1
            DP = MAX( 0, MIN( MAXDP, DP ) )
         ELSE
            DP = 0
         END IF

*  Round the value.
         P10 = REAL( 10**DP )
         LVALUE = NINT( LVALUE*P10 )/P10
         IF( LVALUE .GE. 360.0 ) LVALUE = LVALUE - 360.0
         IF( LVALUE .LE. -360.0 ) LVALUE = LVALUE + 360.0

*  Produce a string holding the value with the correct number of
*  decimal places, with a trailing blank where the units symbol will
*  be written.
         IF( DP .GT. 0 ) THEN
            WRITE( TDP, '(I3)' ) DP
            CALL CHR_RMBLK( TDP )
            LTDP = CHR_LEN( TDP )
            FMT = '(F50.'//TDP( :LTDP )//')'
            WRITE( TEXT, FMT ) LVALUE
         ELSE
            WRITE( TEXT, * ) NINT( LVALUE )
         END IF

         CALL CHR_RMBLK( TEXT )
         LTEXT = CHR_LEN( TEXT ) + 1

*  This routine plots text with CL justification. Since this may not be
*  what the user wants, modify the supplied reference position to make
*  it appear that the string is justified according to the current SGS
*  text justification settings.
         REFX = XPOS
         REFY = YPOS
         CALL IRA1_TREF( TEXT( : LTEXT ), TXJ, H, UX, UY, TXTHGT*MARSIZ,
     :                   WKID, REFX, REFY, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Plot the text string at the modified reference position.
         CALL SGS_TX( REFX, REFY, TEXT( : LTEXT ) )

*  Plot the superscript unit symbol.
         CALL IRA1_USYM( WKID, UNIT( 1 ), REFX, REFY, TEXT,
     :                   LTEXT - 1, TXTHGT, SUPSIZ, SUPOFF, UX, UY,
     :                   STATUS )

*  Save the context.
         IF( NC .EQ. 1 ) THEN
            CONTXT = 'A'//TEXT
         ELSE
            CONTXT = 'B'//TEXT
         END IF

      END IF

*  Restore the original SGS text justification, height and pen number.
      CALL SGS_STXJ( TXJ )
      CALL SGS_SHTX( HT )
      CALL SGS_SPEN( NPEN )

 999  CONTINUE

      END
