      SUBROUTINE SWRIF1( FID, DIRX, DIRY, HGT, RAT, JUST, SPACE, FONT,
     :                   PEN, SDIRX, SDIRY, SHGT, SRAT, SJUST, SSPACE,
     :                   SFONT, SPEN, STATUS )
*+
*  Name:
*     SWRIF1

*  Purpose:
*     Record changed text attributes in the log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIF1( FID, DIRX, DIRY, HGT, RAT, JUST, SPACE, FONT, PEN,
*                  SDIRX, SDIRY, SHGT, SRAT, SJUST, SSPACE, SFONT,
*                  SPEN, STATUS )

*  Description:
*     Adds keywords to the log file in order to set up the attributes
*     for the next text string. Only changed attributes are recorded.

*  Arguments:
*     CALL SWRIF1( FID, DIRX, DIRY, HGT, RAT, JUST, SPACE, FONT, PEN,
*                  SDIRX, SDIRY, SHGT, SRAT, SJUST, SSPACE, SFONT,
*                  SPEN, STATUS )

*     FID = INTEGER (Given)
*        File descriptor for output log file.
*     DIRX = REAL (Given)
*        X component of up vector for current string.
*     DIRY = REAL (Given)
*        Y component of up vector for current string.
*     HGT = REAL (Given)
*        Text height for current string.
*     RAT = REAL (Given)
*        Aspect ratio for current string.
*     JUST = CHARACTER * ( * ) (Given)
*        Justification for current string.
*     SPACE = REAL (Given)
*        Space beteen characters in current string.
*     FONT = INTEGER (Given)
*        Font for current string.
*     PEN = INTEGER (Given)
*        Pen for current string.
*     SDIRX = REAL (Given and Returned)
*        X component of up vector for previous string.
*     SDIRY = REAL (Given and Returned)
*        Y component of up vector for previous string.
*     SHGT = REAL (Given and Returned)
*        Text height for previous string.
*     SRAT = REAL (Given and Returned)
*        Aspect ratio for previous string.
*     SJUST = CHARACTER * ( * ) (Given and Returned)
*        Justification for previous string.
*     SSPACE = REAL (Given and Returned)
*        Space beteen characters in previous string.
*     SFONT = INTEGER (Given and Returned)
*        Font for previous string.
*     SPEN = INTEGER (Given and Returned)
*        Pen for previous string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SKY_PAR'          ! SKT_ constants

*  Arguments Given:
      INTEGER FID
      REAL DIRX
      REAL DIRY
      REAL HGT
      REAL RAT
      CHARACTER JUST*(*)
      REAL SPACE
      INTEGER FONT
      INTEGER PEN

*  Arguments Given and Returned:
      REAL SDIRX
      REAL SDIRY
      REAL SHGT
      REAL SRAT
      CHARACTER SJUST*(*)
      REAL SSPACE
      INTEGER SFONT
      INTEGER SPEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*255          ! Text buffer
      INTEGER BLEN               ! Used length of BUF.
      INTEGER NC                 ! No. of attributes which have changed
      INTEGER ND                 ! No. of non-default attributes

      LOGICAL                    ! Flags indicating which attributes
     :       CDIR,               ! have changed value since the previous
     :       CHGT,               ! text string.
     :       CJUST,
     :       CRAT,
     :       CSPACE,
     :       CFONT,
     :       CPEN

      LOGICAL                    ! Flags indicating which attributes
     :       DDIR,               ! are not at there default settings.
     :       DHGT,
     :       DJUST,
     :       DRAT,
     :       DSPACE,
     :       DFONT,
     :       DPEN

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of attributes which are different to those used
*  for the previous text string.
      NC = 0

*  Check each attribute for the current text string in turn. Note those
*  which are different to the values used for the previous string.
      CDIR = DIRX .NE. SDIRX .OR. DIRY .NE. SDIRY
      IF( CDIR ) NC = NC + 1

      CHGT = HGT .NE. SHGT
      IF( CHGT ) NC = NC + 1

      CJUST = JUST .NE. SJUST
      IF( CJUST ) NC = NC + 1

      CRAT = RAT .NE. SRAT
      IF( CRAT ) NC = NC + 1

      CSPACE = SPACE .NE. SSPACE
      IF( CSPACE ) NC = NC + 1

      CFONT = FONT .NE. SFONT
      IF( CFONT ) NC = NC + 1

      CPEN = PEN .NE. SPEN
      IF( CPEN ) NC = NC + 1

*  If no attributes have changed, dont do anything else.
      IF( NC .EQ. 0 ) GO TO 999

*  Initialise the number of attributes which are different to the
*  default values.
      ND = 0

*  Check each attribute for the current text string in turn. Note those
*  which are different to the default attributes.
      DDIR = DIRX .NE. SKY__DFDR1 .OR. DIRY .NE. SKY__DFDR2
      IF( DDIR ) ND = ND + 1

      DHGT = HGT .NE. SKY__DFHT
      IF( DHGT ) ND = ND + 1

      DJUST = JUST .NE. SKY__DFJST
      IF( DJUST ) ND = ND + 1

      DRAT = RAT .NE. SKY__DFRTO
      IF( DRAT ) ND = ND + 1

      DSPACE = SPACE .NE. SKY__DFSPC
      IF( DSPACE ) ND = ND + 1

      DFONT = FONT .NE. SKY__DFFNT
      IF( DFONT ) ND = ND + 1

      DPEN = PEN .NE. SKY__DFPEN
      IF( DPEN ) ND = ND + 1

*  Output a blank line.
      CALL FIO_WRITE( FID, ' ', STATUS )

*  There are two ways of setting the new attributes. Firstly, we could
*  revert to the default values using a DEFAULT keyword, and then set up
*  any attributes which are not at there default values, or secondly
*  we could just set up values for any which have changed from there
*  previous values. Use the method which results in least number of
*  lines being added to the log file.
      IF( NC .LT. ND + 1 ) THEN

         IF( CDIR ) THEN
            CALL MSG_SETR( 'DX', DIRX )
            CALL MSG_SETR( 'DY', DIRY )
            CALL MSG_LOAD( ' ', 'DIRECTION, ^DX, ^DY', BUF, BLEN,
     :                     STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CHGT ) THEN
            CALL MSG_SETR( 'H', HGT )
            CALL MSG_LOAD( ' ', 'HEIGHT, ^H', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CJUST ) THEN
            CALL MSG_SETC( 'J', JUST )
            CALL MSG_LOAD( ' ', 'JUSTIFICATION, ^J', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CRAT ) THEN
            CALL MSG_SETR( 'R', RAT )
            CALL MSG_LOAD( ' ', 'ASPECT RATIO, ^R', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CSPACE ) THEN
            CALL MSG_SETR( 'S', SPACE )
            CALL MSG_LOAD( ' ', 'SPACE, ^S', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CFONT ) THEN
            CALL MSG_SETI( 'F', FONT )
            CALL MSG_LOAD( ' ', 'FONT, ^F', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( CPEN ) THEN
            CALL MSG_SETI( 'P', PEN )
            CALL MSG_LOAD( ' ', 'PEN, ^P', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

*  If reverting to default values is cheaper, add the line to do it
*  before adding all the lines to change the newly established default
*  values.
      ELSE

         CALL FIO_WRITE( FID, 'DEFAULT', STATUS )

         IF( DDIR ) THEN
            CALL MSG_SETR( 'DX', DIRX )
            CALL MSG_SETR( 'DY', DIRY )
            CALL MSG_LOAD( ' ', 'DIRECTION, ^DX, ^DY', BUF, BLEN,
     :                     STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DHGT ) THEN
            CALL MSG_SETR( 'H', HGT )
            CALL MSG_LOAD( ' ', 'HEIGHT, ^H', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DJUST ) THEN
            CALL MSG_SETC( 'J', JUST )
            CALL MSG_LOAD( ' ', 'JUSTIFICATION, ^J', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DRAT ) THEN
            CALL MSG_SETR( 'R', RAT )
            CALL MSG_LOAD( ' ', 'ASPECT RATIO, ^R', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DSPACE ) THEN
            CALL MSG_SETR( 'S', SPACE )
            CALL MSG_LOAD( ' ', 'SPACE, ^S', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DFONT ) THEN
            CALL MSG_SETI( 'F', FONT )
            CALL MSG_LOAD( ' ', 'FONT, ^F', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

         IF( DPEN ) THEN
            CALL MSG_SETI( 'P', PEN )
            CALL MSG_LOAD( ' ', 'PEN, ^P', BUF, BLEN, STATUS )
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )
         END IF

      END IF

*  Output a blank line.
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Save the current attributes.
      SDIRX = DIRX
      SDIRY = DIRY
      SHGT = HGT
      SRAT = RAT
      SJUST = JUST
      SSPACE = SPACE
      SFONT = FONT
      SPEN = PEN

 999  CONTINUE

      END
