      SUBROUTINE CAP_GSCNF (SWID, SHT, SEQNO, NLIST, ANGRPN, ANGRF,
     :  GUI, STATUS)
*+
*  Name:
*     CAP_GSCNF
*  Purpose:
*     Set the screen configuration.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCNF (SWID, SHT, SEQNO, NLIST, ANGRPN, ANGRF, GUI;
*       STATUS)
*  Description:
*     Set the screen configuration.
*  Arguments:
*     SWID  =  INTEGER (Given)
*        Screen width in characters.
*     SHT  =  INTEGER (Given)
*        Screen height in characters.
*     SEQNO  =  LOGICAL (Given)
*        Flag indicating whether listed rows are preceeded by a running
*        sequence number, coded as follows:
*        .TRUE.  - include a sequence number,
*        .FALSE. - do not include a sequence number.
*     NLIST  =  INTEGER (Given)
*        Number of lines to display in a single invocation of action
*        'LIST'.
*     ANGRPN  =  CHARACTER*(*) (Given)
*        Flag indicating whether columns of angles are to be displayed
*        as hours or degrees formatted as sexagesimal values or as
*        radians.  The permitted values are:
*        'RADIANS'     -  output angles in radians,
*        'SEXAGESIMAL' -  output angles in hours or degrees, formatted
*           as sexagesimal values.
*     ANGRF  =  LOGICAL (Given)
*        Flag determining whether a UNITS attribute is recognised as
*        describing a column of angles is displayed unaltered or
*        reformatted prior to display.  If it is reformatted then the
*        contents of the attribute are replaced (for display only) by
*        either the sexagesimal format specifier (extracted from the
*        attribute) or the string 'Radians', depending on whether or not
*        angles are currently being output as sexagesimal values or
*        radians.  The values are:
*        .TRUE.  -  reformat the UNITS attribute,
*        .FALSE. -  do not reformat the UNITS attribute.
*     GUI  =  LOGICAL (Given)
*        Flag indicating whether or not the application is being
*        driven from a GUI.  It is coded as follows:
*        .TRUE.  -  driven from a GUI,
*        .FALSE. -  driven from the command line.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the screen width if the given value lies in the permitted
*     range.
*     Set the screen height.
*     If the catalogue is open then
*       If each row is to be preceded by a sequence number then
*         Get the number of rows in the catalogue.
*         Compute the width required to hold the sequence number.
*       else
*         Set the width for the sequence number to zero.
*       end if
*    else
*      Set the width for the sequence number to zero.
*    end if
*    Set the number of lines to be displayed by 'LIST'.
*    Set the external representation of columns of angles.
*    Set the flag determining whether a UNITS attribute is recognised as
*    describing a column of angles is displayed unaltered or
*    reformatted.
*    Set the GUI flag.
*    Set the flag saying that the details of the set of components
*    have changed.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94 (ACD): Original version.
*     28/3/95 (ACD): First stable version.
*     26/2/96 (ACD): Added ANGRF.
*     1/7/99  (ACD): Modified the handling of the screen line width to
*       correspond to the ADAM parameter system.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
*  Arguments Given:
      INTEGER
     :  SWID,
     :  SHT,
     :  NLIST
      LOGICAL
     :  SEQNO,
     :  ANGRF,
     :  GUI
      CHARACTER
     :  ANGRPN*(*)
*  Status:
      INTEGER STATUS      ! Global status.
*  Local Variables:
      INTEGER
     :  ROWS              ! Number of rows in the catalogue.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the screen width if the given value lies in the permitted
*       range.  Otherwise report a message and leave the screen width
*       unchanged.

         IF (SWID .GT. 0  .AND.  SWID .LE. SGZ__SZOMS) THEN
            SWID__SGZ = SWID

         ELSE
            CALL MSG_SETI ('SWID', SWID)
            CALL CAP_WARN (GUI, ' ', 'The screen width is unchanged; '/
     :        /'an invalid value was supplied: ^SWID.', STATUS)

            CALL MSG_SETI ('SZOMS', SGZ__SZOMS)
            CALL CAP_WARN (GUI, ' ', 'The permitted range is 1 - '/
     :        /'^SZOMS.', STATUS)
         END IF

*
*       Set the screen width the flag indicating whether or not listings
*       are to be paged.

         SHT__SGZ = SHT

*
*       If there is a catalogue open and each row is to be preceded by
*       a sequence number then calculate and set the space required for
*       it.  Otherwise set the space required to zero.

         IF (COPEN__SGZ) THEN
            IF (SEQNO) THEN
               CALL CAT_TROWS (CI__SGZ, ROWS, STATUS)

               RUN__SGZ = INT(LOG10(REAL(ROWS) ) ) + 2
               RUN__SGZ = MAX(RUN__SGZ, 3)

            ELSE
               RUN__SGZ = 0

            END IF
         ELSE
            RUN__SGZ = 0

         END IF

*
*       Set the number of lines to be displayed by 'LIST'.

         NLIST__SGZ = NLIST

*
*       Set the external representation of columns of angles (the
*       options are to display in radians or to display in hours or
*       degrees formatted as sexagesimal values).

         CALL CAT_TUNES ('ANGLE_LIST', ANGRPN, STATUS)

*
*       Set the flag determining whether a UNITS attribute is
*       recognised as describing a column of angles is displayed unaltered
*       or reformatted prior to display.

         ANGRF__SGZ = ANGRF

*
*       Set the flag indicating whether or not the application is being
*       driven from a GUI.

         GUI__SGZ = GUI

*
*       Set the flag to say that the details of the set of components
*       have changed.

         CMPCG__SGZ = .TRUE.

      END IF

      END
