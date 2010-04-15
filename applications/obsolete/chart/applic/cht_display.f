      SUBROUTINE CHT_DISPLAY( STATUS )
*+
*  Name:
*     CHT_DISPLAY

*  Purpose:
*     Display the current CHART parameters

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_DISPLAY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Generates a reasonably readable account of the current values of
*     all the CHART parameters.

*  Usage:
*     DSIPLAY

*  ADAM Parameters:
*     PAUSE = _CHAR (Read)
*        Pause until the user presses return.

*  Examples:
*     DISPLAY
*        This display the current value of the CHART parameters

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     KPT: Keith Tritton (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     10-Jan-1983 (KFH):
*        Original version.
*     26-APR-1983 (KPT):
*        {changes}
*     10-DEC-1991 (PMA):
*        Converted to ADAM A-task.
*     8-JUN-1992 (PMA):
*        Changed calls to INTERIM routines to ADAM calls.
*     17-JUN-1992 (PMA):
*        Remove tabs from character strings.
*     26-FEB-1993 (PMA):
*        Tidy up prologue.
*        Rename the routine to CHT_DISPLAY.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.dat' to filenames.
*     9-JUN-1993 (AJJB):
*        Changed the section which reports which device will be used for
*        plotting. It now just tells the user which GKS device, using
*        the string in the DEVICE parameter, whether it's a name or a
*        number, without attempting to
*        translate it to the actual name, eg. Printronix, as was done
*        before.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! The length of a character string


*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! [local_variable_description]
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 70 ) TEXT    ! [local_variable_description]
      CHARACTER * ( 70 ) TEMP    ! [local_variable_description]
      LOGICAL FLAG               ! [local_variable_description]
      INTEGER NVAL               ! [local_variable_description]
      INTEGER NPOS               ! [local_variable_description]
      INTEGER L                  !
      INTEGER L1                 !
      INTEGER I                  ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the parameter file into the array PARAMS
      CALL GETPARAMS( PARAMS, NVAL, STATUS )
      CALL MSG_OUT( ' ', '   ******************************', STATUS )
      CALL MSG_OUT( ' ', '   *                            *', STATUS )
      CALL MSG_OUT( ' ', '   *  Current Parameter Values  *', STATUS )
      CALL MSG_OUT( ' ', '   *                            *', STATUS )
      CALL MSG_OUT( ' ', '   ******************************', STATUS )
      CALL MSG_BLANK( STATUS )
C
C   This section handles the Catalogue Search parameters.
C
      CALL MSG_OUT( ' ', 'Search (S) parameters:-', STATUS )
      CALL MSG_OUT( ' ', '=====================', STATUS )
      CALL GETDEFLT(PARAMS,NVAL,'LABEL',VALUE,NPOS, STATUS )
      IF (NPOS.NE.0.AND.VALUE.NE.' ') THEN
         TEXT = '       Label is :- ' // VALUE
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
      CALL GETDEFLT(PARAMS,NVAL,'SAREA',VALUE,NPOS, STATUS )
      IF (NPOS.NE.0) THEN
         L = CHR_LEN( VALUE )
         TEXT='        The search area is a square of side ' //
     :      VALUE( 1:L ) // ' degrees'
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
      CALL GETDEFLT(PARAMS,NVAL,'EQUINOX',VALUE,NPOS, STATUS )
      IF (NPOS.NE.0) THEN
         TEXT = '        Equinox of output positions is ' // VALUE
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
      CALL GETDEFLT(PARAMS,NVAL,'EPOCH',VALUE,NPOS, STATUS)
      IF (NPOS.NE.0) THEN
         TEXT = '        Epoch of output positions is ' // VALUE
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
C
C   handle the catalogue parameters, including the current options.
C
      CALL GETDEFLT(PARAMS,NVAL,'MODE',VALUE,NPOS, STATUS)
      IF (NPOS.NE.0) THEN
         TEXT = '        The mode is ' // VALUE
         CALL MSG_OUT( ' ', TEXT, STATUS )
         IF (VALUE(1:1).EQ.'N') THEN
            TEXT = '        Up to 1000 objects will be stored'
            CALL MSG_OUT( ' ', TEXT, STATUS )
         ELSEIF (VALUE(1:1).EQ.'P') THEN
            CALL GETDEFLT(PARAMS,NVAL,'INPUT',TEMP,NPOS, STATUS)
            IF (NPOS.NE.0) THEN
             TEXT='         Private catalogue to be used is '//TEMP
                CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF
         ELSE
            CALL GETDEFLT(PARAMS,NVAL,'SELECTION',TEMP,NPOS, STATUS)
            IF (INDEX(TEMP,'ALL').GT.0) THEN
               CALL MSG_OUT( ' ',
     :            '        All stars found will be used', STATUS )
            ELSE IF (INDEX(TEMP,'N').GT.0) THEN
               CALL GETDEFLT(PARAMS,NVAL,'NUMBER',VALUE,NPOS, STATUS)
               L=CHR_LEN(VALUE)
               TEXT = '        Only ' // VALUE( 1:L ) //
     :            ' stars will be stored'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ELSE IF (INDEX(TEMP,'M').GT.0) THEN
               CALL GETDEFLT(PARAMS,NVAL,'MAGNITUDE',VALUE,NPOS, STATUS)
               L=CHR_LEN(VALUE)
               TEXT = '        Only stars brighter than ' //
     :            VALUE( 1:L ) // ' will be stored'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ELSE IF (INDEX(TEMP,'C').GT.0) THEN
               CALL GETDEFLT( PARAMS, NVAL, 'CATALOGUES', VALUE, NPOS,
     :            STATUS )
               TEXT = '        Stars in these catalogues will be used:-'
               CALL MSG_OUT( ' ', TEXT, STATUS )
               TEXT = '                ' // VALUE
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF
         END IF
      END IF
C
C   Handle the field centers.
C
      CALL GETDEFLT(PARAMS,NVAL,'FIELDS',VALUE,NPOS, STATUS)
      IF (NPOS.GT.0) THEN
         IF (VALUE(1:4).EQ.'TERM') THEN
            TEXT = '        Field centres will be input at the terminal'
            CALL MSG_OUT( ' ', TEXT, STATUS )
         ELSE
            L=CHR_LEN(VALUE)
            TEXT = '        The field centres are stored in a file ' //
     :         'called ' // VALUE( 1:L )
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = '        The file may be examined in the normal way'
            CALL MSG_OUT( ' ', TEXT, STATUS )
         END IF
      END IF
C
C   Explain how to modify these parameters.
C
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',
     :   '>>>> Use the program ''sset'' to change any of the above ',
     :   STATUS)
      CALL MSG_OUT( ' ',
     : '>>>> Use the program ''fcreate'' to create a file of field ' //
     : 'centres', STATUS )
      CALL MSG_BLANK( STATUS )
C
C   Wait for the user to hit return.
C
      CALL PAR_GET0C( 'PAUSE', VALUE, STATUS )
      CALL PAR_CANCL( 'PAUSE', STATUS )
      CALL MSG_BLANK( STATUS )
C
      CALL MSG_OUT( ' ', 'Output (O) parameters:-', STATUS )
      CALL MSG_OUT( ' ', '=====================', STATUS )
C
C   handle the plot parameters.
C
      CALL GETDEFLT( PARAMS, NVAL, 'DEVICE', VALUE, NPOS, STATUS )
      IF (NPOS.GT.0.AND.VALUE.NE.' ') THEN
         I=1
         DO WHILE ((VALUE(I:I).NE.'_').AND.(VALUE(I:I).NE.' '))
            I=I+1
         ENDDO
         IF (INDEX(VALUE,'_A').GT.1) THEN
            FLAG=.TRUE.
         ELSE
            FLAG=.FALSE.
         END IF
         TEXT = '        Plotting will be done on the GKS device '
     :          // VALUE(1:I-1)
         CALL MSG_OUT( ' ', TEXT, STATUS )
         IF (FLAG) THEN
            CALL GETDEFLT(PARAMS,NVAL,'RATIO',VALUE,NPOS, STATUS)
            IF (NPOS.GT.0) THEN
               TEMP=VALUE
               L=CHR_LEN(TEMP)
               TEXT = '        An aspect ratio of ' // TEMP( 1:L )
            END IF
            CALL GETDEFLT(PARAMS,NVAL,'FACTOR',VALUE,NPOS, STATUS)
            IF (NPOS.GT.0) THEN
               TEMP=VALUE
               L = CHR_LEN( TEMP )
               L1 = CHR_LEN( TEXT )
               TEXT( L1+1: ) = ' and a scale factor of ' // TEMP( 1:L )
            END IF
            L=CHR_LEN( TEXT)
            TEXT( L1+1: ) = ' will be used'
            CALL MSG_OUT( ' ', TEXT, STATUS )
         END IF
         CALL GETDEFLT(PARAMS,NVAL,'PAREA',VALUE,NPOS, STATUS)
         L=CHR_LEN(VALUE)
         TEXT = '        The plot area is a square of size ' //
     :      VALUE( 1:L ) // ' degrees'
         CALL MSG_OUT( ' ', TEXT, STATUS )
         CALL GETDEFLT(PARAMS,NVAL,'SCALE',VALUE,NPOS, STATUS)
         L=CHR_LEN(VALUE)
         TEXT = '        The scale is ' // VALUE( 1:L ) // ' arcsec/mm'
         CALL MSG_OUT( ' ', TEXT, STATUS )
         CALL GETDEFLT(PARAMS,NVAL,'SYMBOL',VALUE,NPOS, STATUS)
         IF (VALUE(1:6).EQ.'CIRCLE') THEN
            TEXT = '        Circles will be used to represent stars '
     :           // 'on the plot'
         ELSE IF (VALUE(1:4).EQ.'SPOT') THEN
            TEXT = '        Filled circles will be used to represent '
     :           // 'stars on the plot'
         ELSE
            TEXT = '        Exploded crosses will be used to represent '
     :           // 'stars on the plot'
         ENDIF
         CALL MSG_OUT( ' ', TEXT, STATUS )
         CALL GETDEFLT(PARAMS,NVAL,'KEY',VALUE,NPOS, STATUS)
         IF (NPOS.GT.0.AND.((VALUE(1:1).EQ.'Y').OR.
     :      (VALUE(1:6).EQ.'SCALES'))) THEN
            CALL MSG_OUT( ' ', '        A scale key will be plotted',
     :         STATUS )
         END IF
         IF (NPOS.GT.0.AND.((VALUE(1:1).EQ.'Y').OR.
     :      (VALUE(1:7).EQ.'NUMBERS'))) THEN
            CALL MSG_OUT( ' ', '        Stars plotted will be numbered',
     :         STATUS )
         END IF
         CALL GETDEFLT(PARAMS,NVAL,'GRID',VALUE,NPOS, STATUS)
         IF (NPOS.GT.0.AND.VALUE(1:1).EQ.'Y') THEN
            CALL MSG_OUT( ' ', '        An RA/DEC grid will be plotted',
     :         STATUS )
         ELSE IF (VALUE(1:1).EQ.'M') THEN
            CALL MSG_OUT( ' ',
     :         '        An abbreviated grid will be plotted', STATUS )
         END IF
         CALL GETDEFLT(PARAMS,NVAL,'ERRBOX',VALUE,NPOS, STATUS)
         IF (NPOS.GT.0.AND.VALUE(1:2).NE.'NO') THEN
            IF (INDEX(VALUE,'C').GT.0) THEN
               CALL GETDEFLT(PARAMS,NVAL,'RADIUS',TEMP,NPOS, STATUS)
               TEXT = '        A circular error box will be drawn'
               CALL MSG_OUT( ' ', TEXT, STATUS )
               L=CHR_LEN(TEMP)
               TEXT = '        of radius ' // TEMP( 1:L ) // ' arcmins.'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            END IF
            IF (INDEX(VALUE,'P').GT.0) THEN
               TEXT = '        Error box will be prompted for'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            END IF
            IF (INDEX(VALUE,'Q').GT.0) THEN
               CALL GETDEFLT(PARAMS,NVAL,'COORDS',TEMP,NPOS, STATUS)
               TEXT = '        A quadrilateral error box will be drawn'
               CALL MSG_OUT( ' ', TEXT, STATUS )
               L=CHR_LEN(TEMP)
               TEXT = '        The vertices are in the file ' //
     :            TEMP( 1:L )
               CALL MSG_OUT( ' ', TEXT, STATUS )
               TEXT='>>>> Use the program QCREATE to enter vertices'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            END IF
         END IF
      END IF
C
C   Now handle the extra objects.
C
      CALL GETDEFLT(PARAMS,NVAL,'EXTRA',VALUE,NPOS, STATUS)
      IF (NPOS.GT.0) THEN
         IF (VALUE(1:2).EQ.'NO') THEN
            CALL MSG_OUT( ' ',
     :         '        There are no extra objects', STATUS )
         ELSE
            L=CHR_LEN(VALUE)
            TEXT = '        The file containing extra objects is ' //
     :           VALUE( 1:L )
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = '        The file may be examined in the normal way'
            CALL MSG_OUT( ' ', TEXT, STATUS )
         END IF
      END IF
      CALL GETDEFLT(PARAMS,NVAL,'DIRECT',VALUE,NPOS, STATUS)
      IF (VALUE(1:3).EQ.'REV') THEN
         CALL MSG_OUT( ' ', '        The plot will be reversed',
     :      STATUS )
      END IF
      CALL GETDEFLT(PARAMS,NVAL,'CROSS',VALUE,NPOS, STATUS )
      IF (VALUE(1:2).EQ.'NO') THEN
         CALL MSG_OUT( ' ',
     :    '        The central cross wil NOT be plotted', STATUS )
      END IF
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',
     :   '>>>> Use the program ''oset'' to change any of the above ',
     :   STATUS )
      CALL MSG_OUT( ' ',
     :  '>>>> Use the program ''ecreate'' to create a file of extra '//
     :  'objects', STATUS )
      CALL MSG_BLANK( STATUS )
      TEXT='>>>> Use the program ''setone'' to change any one of ' //
     :     'these parameters'
      CALL MSG_OUT( ' ', TEXT, STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'For a more concise listing of the above '//
     :   'parameters, type the command DISP', STATUS )
      CALL MSG_BLANK( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DISPLAY_ERR',
     :   'DISPLAY: Error displaying CHART parameters.',
     :   STATUS )
      END IF

      END
