      SUBROUTINE SWRIC0( IGRP, SIZE, SCS, EPOCH, MXNTXT, NTXT, LON,
     :                   LAT, TXT, DIRX, DIRY, HEIGHT, RATIO, JSTFCT,
     :                   SPACE, FONT, PEN, STATUS )
*+
*  Name:
*     SWRIC0

*  Purpose:
*     Write definition of texts in to their corresponding arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIC0( IGRP, SIZE, SCS, EPOCH, MXNTXT, NTXT, LON, LAT,
*                  DIRX, DIRY, HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN,
*                  STATUS )

*  Description:
*     This subroutine write the definitions of text stored in a GRP
*     group into their own arrays.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the input group.
*     SIZE = INTEGER (Given)
*        The number of elements in the group.
*     SCS = CHARACTER (Given)
*        The ID of the IRA system associated with the image.
*     EPOCH = DOUBLE PRECISION (Given)
*        The epoch of observation to assume for coordinates supplied in
*        text files.
*     MXNTXT = INTEGER (Given)
*        The max. number of text can be handled.
*     NTXT = INTEGER (Given and Returned)
*        The number of texts has been written on to the iamge.
*     LON( MXNTXT ), LAT( MXNTXT ) = DOUBLE PRECISION (Given and Returned)
*        The image coordinates of each text on the image.
*     TXT( MXNTXT ) = CHARACTER*( * ) (Given and Returned)
*        Text strings written to the positions.
*     DIRX( MXNTXT ), DIRY( MXNTXT ) = REAL (Given and Returned)
*        The up direction vectors.
*     HEIGHT( MXNTXT ), RATIO( NTXT ) = REAL (Given and Returned)
*        The height and aspect ratio of the texts.
*     JSTFCT( MXNTXT ) = CHARACTER*( * ) (Given and Returned)
*        The justification of the texts.
*     SPACE( MXNTXT ) = REAL (Given and Returned)
*        The space between characters of texts.
*     FONT( MXNTXT ), PEN( MXNTXT ) = INTEGER (Given and Returned)
*        The font and pen number of the texts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-AUG-1992 (WG):
*        Original version.
*     23-FEB-1993 (DSB):
*        Changes to use GRP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRA_ERR'          ! IRA_ error constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'SKY_PAR'          ! SKY_ constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER SIZE
      CHARACTER*( * ) SCS
      DOUBLE PRECISION EPOCH
      INTEGER MXNTXT

*  Arguments Given and Returned:
      INTEGER NTXT
      DOUBLE PRECISION LON( MXNTXT ), LAT( MXNTXT )
      CHARACTER*( * ) TXT( MXNTXT )
      REAL DIRX( MXNTXT ), DIRY( MXNTXT )
      REAL HEIGHT( MXNTXT ), RATIO( MXNTXT )
      CHARACTER*( * ) JSTFCT( MXNTXT )
      REAL SPACE( MXNTXT )
      INTEGER FONT( MXNTXT ), PEN( MXNTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER ARGS( 2 )*(GRP__SZNAM)! Arguments.
      CHARACTER BJ*1             ! Type of epoch.
      CHARACTER C1*1             ! First character
      CHARACTER C2*1             ! Last character
      CHARACTER ELEM*(GRP__SZNAM)! Current element
      CHARACTER NAME*(IRA__SZSCS)! Name field only, from SCS.
      CHARACTER FSCS*(GRP__SZNAM)! SCS from file.
      CHARACTER SJUST*2          ! Current text justidfication

      DOUBLE PRECISION A         ! Longitude value.
      DOUBLE PRECISION B         ! Latitude value.
      DOUBLE PRECISION EQU       ! SCS equinox epoch.

      INTEGER ELEN               ! Used length of ELEM
      INTEGER FIRST              ! Index of first character in string
      INTEGER INDX               ! Index of next name to be read from
                                 ! the group.
      INTEGER LAST               ! Index of last character in string
      INTEGER NMATCH             ! No. of keyword matches found.
      INTEGER SFONT              ! Current font
      INTEGER SPEN               ! Current pen

      REAL SDIRX                 ! Current up-vector X component.
      REAL SDIRY                 ! Current up-vector Y component.
      REAL SHGT                  ! Current text height.
      REAL SRAT                  ! Current text aspect ratio.
      REAL SSPACE                ! Current space between characters.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the first element from the group. This should be the sky
*  coordinate system in which the starting points of the curves are
*  given.
      CALL GRP_GET( IGRP, 1, 1, FSCS, STATUS )

*  See if the specified coordinate system is a valid sky coordinate
*  system.
      CALL IRA_GETEQ( FSCS, EQU, BJ, NAME, STATUS )

*  If it is not a valid sky coordinate system, add a context report and
*  abort.
      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL MSG_SETC( 'C', FSCS )
         CALL ERR_REP( 'SWRIC0_ERR1',
     :'SWRIC0: First non-comment record in file is ^C. This is '//
     :'not a valid sky coordinate system', STATUS )
         GO TO 999
      END IF

*  Initialise the index of the next name to read from the group.
      INDX = 2

*  Save the text attributes used to draw the last text string. If none
*  have yet been drawn save the default attributes.
      IF( NTXT .GT. 0 ) THEN
         SDIRX  = DIRX( NTXT )
         SDIRY  = DIRY( NTXT )
         SHGT   = HEIGHT( NTXT )
         SRAT   = RATIO( NTXT )
         SJUST  = JSTFCT( NTXT )
         SSPACE = SPACE( NTXT )
         SFONT  = FONT( NTXT )
         SPEN   = PEN( NTXT )

      ELSE
         SDIRX  = SKY__DFDR1
         SDIRY  = SKY__DFDR2
         SHGT   = SKY__DFHT
         SRAT   = SKY__DFRTO
         SJUST  = SKY__DFJST
         SSPACE = SKY__DFSPC
         SFONT  = SKY__DFFNT
         SPEN   = SKY__DFPEN

      END IF

*  Set the group case sensitive so that the written text retains the
*  supplied case.
      CALL GRP_SETCS( IGRP, .TRUE., STATUS )

*  Loop round until the end of the group has been reached, or an error
*  has occurred.
      DO WHILE( INDX .LE. SIZE .AND. STATUS .EQ. SAI__OK )

*  Get the next element, convert to upper case, and get its used length.
         CALL GRP_GET( IGRP, INDX, 1, ELEM, STATUS )
         CALL CHR_UCASE( ELEM )
         CALL CHR_LDBLK( ELEM )
         ELEN = CHR_LEN( ELEM )
         INDX = INDX + 1

*  Check it against each of the recognised keywords. If it matches a
*  keyword, get the required number of arguments from the group, and
*  modify the current text attributes.
         NMATCH = 0

         IF( INDEX( 'DIRECTION', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = 1
            CALL GRP_GET( IGRP, INDX, 2, ARGS, STATUS )
            INDX = INDX + 2
            CALL CHR_CTOR( ARGS( 1 ), SDIRX, STATUS )
            CALL CHR_CTOR( ARGS( 2 ), SDIRY, STATUS )
         END IF

         IF( INDEX( 'HEIGHT', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_CTOR( ARGS( 1 ), SHGT, STATUS )
            END IF
         END IF

         IF( INDEX( 'ASPECT RATIO', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_CTOR( ARGS( 1 ), SRAT, STATUS )
            END IF
         END IF

         IF( INDEX( 'JUSTIFICATION', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_LDBLK( ARGS( 1 ) )
               SJUST = ARGS( 1 )( : 2 )
            END IF
         END IF

         IF( INDEX( 'FONT', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_CTOI( ARGS( 1 ), SFONT, STATUS )
            END IF
         END IF

         IF( INDEX( 'SPACE', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_CTOR( ARGS( 1 ), SSPACE, STATUS )
            END IF
         END IF

         IF( INDEX( 'PEN', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               CALL GRP_GET( IGRP, INDX, 1, ARGS, STATUS )
               INDX = INDX + 1
               CALL CHR_CTOI( ARGS( 1 ), SPEN, STATUS )
            END IF
         END IF

         IF( INDEX( 'DEFAULT', ELEM( : ELEN ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            IF( NMATCH .EQ. 1 ) THEN
               SDIRX  = SKY__DFDR1
               SDIRY  = SKY__DFDR2
               SHGT   = SKY__DFHT
               SRAT   = SKY__DFRTO
               SJUST  = SKY__DFJST
               SSPACE = SKY__DFSPC
               SFONT  = SKY__DFFNT
               SPEN   = SKY__DFPEN
            END IF
         END IF

*  If the element matched more than one keyword, report an error.
         IF( NMATCH .GT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'E', ELEM )
            CALL ERR_REP( 'SWRIC0_ERR2',
     :                    'SWRIC0: Ambiguous keyword "^E" found',
     :                    STATUS )
            GO TO 999

*  If the element did not match any of the keywords, attempt to
*  convert it to a longitude value.
         ELSE IF( NMATCH .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL IRA_CTOD1( ELEM, FSCS, 1, A, STATUS )

*  If succesful, get the next two elements, which should be the latitude
*  and the text string.
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL GRP_GET( IGRP, INDX, 2, ARGS, STATUS )
               INDX = INDX + 2

*  Attempt to convert the first string to a latitude value.
               CALL IRA_CTOD1( ARGS( 1 ), FSCS, 2, B, STATUS )

*  If the text string is delimited by quotes, remove them.
               CALL CHR_FANDL( ARGS( 2 ), FIRST, LAST )
               C1 = ARGS( 2 )( FIRST : FIRST )
               C2 = ARGS( 2 )( LAST : LAST )

               IF( ( C1 .EQ. '"' .AND. C2 .EQ. '"' ) .OR.
     :             ( C1 .EQ. '''' .AND. C2 .EQ. '''' ) ) THEN
                  FIRST = FIRST + 1
                  LAST = LAST - 1
               END IF

*  If there is room left in the arrays, store the text position
*  (converted to the required SCS), text string and text attributes.
               IF( NTXT .LT. MXNTXT ) THEN
                  NTXT = NTXT + 1

                  CALL IRA_CONVT( 1, A, B, FSCS, SCS, EPOCH, LON( NTXT),
     :                            LAT( NTXT ), STATUS )

                  IF( LAST .GE. FIRST ) THEN
                     TXT( NTXT ) = ARGS( 2 )( FIRST : LAST )
                  ELSE
                     TXT( NTXT ) = ' '
                  END IF

                  DIRX( NTXT ) = SDIRX
                  DIRY( NTXT ) = SDIRY
                  HEIGHT( NTXT ) = SHGT
                  RATIO( NTXT ) = SRAT
                  JSTFCT( NTXT ) = SJUST
                  SPACE( NTXT ) = SSPACE
                  FONT( NTXT ) = SFONT
                  PEN( NTXT ) = SPEN

* If the arrays are full, report an error.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'M', MXNTXT )
                  CALL ERR_REP( 'SWRIC0_ERR3',
     :     'SWRIC0: Maximum no. of text strings (^M) has been exceeded',
     :                          STATUS )
                  GO TO 999
               END IF

*  If the current element did not have the format of a longitude value,
*  add a context message.
            ELSE
               CALL MSG_SETC( 'C', ELEM )
               CALL ERR_REP( 'SWRIC0_ERR4',
     :     'SWRIC0: Blank text string, bad keyword, or bad longitude '//
     :     'value found "^C".', STATUS )
               GO TO 999
            END IF

         END IF

      END DO

 999  CONTINUE

      END
