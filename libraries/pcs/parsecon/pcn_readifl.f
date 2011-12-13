      SUBROUTINE PARSECON_READIFL ( LUCON, NUMERR, STATUS )
*+
*  Name:
*     PARSECON_READIFL

*  Purpose:
*     Read, interpret and store interface file.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_READIFL ( LUCON, NUMERR, STATUS )

*  Description:
*     Read interface file from logical unit LUCON and store it in
*     internal form.

*  Arguments:
*     LUCON=INTEGER (given)
*        logical unit number of interface file
*     NUMERR=INTEGER (returned)
*        number of errors detected
*     STATUS=INTEGER

*  Algorithm:
*     A new error reporting context is set up
*     The interface file is read, its contents being returned one token
*     at a time. The type of the token is determined and coded as an
*     integer. The current parse-state is also represented by an
*     integer. The combination of token-type and parse-state is used to
*     access a look-up table which gives a code for the action to be
*     taken and for the new parse state.
*     The error reporting context is released

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1990, 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     W.F.Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     11.09.1984:  VAX version (REVAD::BDK)
*     23.08.1985:  handle monoliths (REVAD::BDK)
*     13.05.1986:  handle menus (REVAD::BDK)
*     05.05.1987:  add PPATH (REVAD::BDK)
*     26.05.1987:  add ACTEND (REVAD::BDK)
*     23.07.1987:  allow TOKEN to be 132 long (REVAD::BDK)
*     15.05.1990:  add HELPKEY (RLVAD::AJC)
*     04.07.1990:  add HELPLIB (RLVAD::AJC)
*     16.08.1990:  add new actions for end of action/interface
*        and parameter (RLVAD::AJC)
*     19.09.1990:  re-organize
*        handling status set on readerr in GPBTOK
*        correct error message when TOKLEN -ve (RLVAD::AJC)
*     17.10.1990:  correct argument mismatch NEWACCORDS (RLVAD::AJC)
*     20.01.1992:  Use renamed COORDS routines (RLVAD::AJC)
*     19.02.1991:  special action on non-contiguous positions
*        add action on INTERFACE
*        Set error context (RLVAD::AJC)
*     26.02.1991:  Retain unaltered token, _ARRCHAR and _TOKTYPE
*        altered  (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'


*  Arguments Given:
      INTEGER LUCON                  ! logical unit number connected to
                                     ! interface file


*  Arguments Returned:
      INTEGER NUMERR                 ! number of errors detected


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON_CMN'
      INCLUDE 'PARSECON2_CMN'


*  Local Variables:
      INTEGER STATE                  ! current parse-state

      CHARACTER*132 TOKEN            ! token from the interface file

      INTEGER TOKLEN                 ! number of significant characters
                                     ! in TOKEN

      CHARACTER*132 WORD             ! un-capitalised TOKEN

      INTEGER TOKTYP                 ! type of the TOKEN

      INTEGER LINENUM                ! number of current line being read
                                     ! from interface file

      INTEGER ACTCODE                ! code for the action to be taken

      INTEGER NEWSTATE               ! new parse-state

      LOGICAL ERRFLAG                ! .TRUE. => in error state. Do not
                                     ! bother outputting error messages.
                                     ! When the parser 'picks-up' again,
                                     ! this gets reset to .FALSE.

      LOGICAL MONFLAG                ! .TRUE. => parsing a monolith, try
                                     ! for more INTERFACE declarations.


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set new error context
      CALL EMS_MARK
*
*   Initialise state table, error message names, pointers etc
*
      CALL PARSECON_TABINIT ( STATUS )
      CALL PARSECON_PTRINIT ( STATUS )
*
*   Initialise names for error reports etc.
      STATE = START
      NUMERR = 0
      ERRFLAG = .FALSE.
      MONFLAG = .FALSE.
*  initialise help library string
      HLBLEN = 0

*   Now loop processing tokens.
*   Do until FINISHED.
*   FINISHED may be set by - the closing field (ENDINTERFACE etc.)
*                          - run out of value storage space
*                          - end of file
*                          - read error
      DO WHILE ( STATE .NE. FINISHED )
*      Get next token
         CALL PARSECON_GPBTOK ( LUCON, WORD, TOKLEN, LINENUM, STATUS )

*      determine token type and provide capitalised form in TOKEN
         CALL PARSECON_TOKTYP ( WORD(:TOKLEN), TOKEN, TOKTYP, STATUS )

*      retrieve ( action code, next state) from table
*      ensure no action unless properly returned ACTCODE
         ACTCODE = NONE
         CALL PARSECON_TABENT ( STATE, TOKTYP, ACTCODE,
     :     NEWSTATE, STATUS )

*      Perform action, if required.
         IF ( ACTCODE .EQ. NONE ) THEN
            CONTINUE

         ELSE IF ( ACTCODE .EQ. NEWACT ) THEN
*         The token is the name of a new action.
*         Create a new entry on the action list.
            CALL PARSECON_NEWACT ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. ACTEND ) THEN
*         The token is ENDACTION.
*         Complete the new entry on the action list.
            CALL PARSECON_ACTEND ( STATUS )

         ELSE IF ( ACTCODE .EQ. NEWPAR ) THEN
*         The token is the name of a new parameter.
*         Create a new entry on the parameter list, and set it to
*         WRITE access. This may be overwritten if there is
*         subsequently an ACCESS restriction on the parameter.
             CALL PARSECON_NEWPAR ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. PAREND ) THEN
*         Clear the parameter name from the error report common block
             CALL PARSECON_PAREND ( STATUS )

         ELSE IF ( ACTCODE .EQ. MESTEXT ) THEN
*         The current parameter is a MESSAGE. The current token contains
*         the message text. Mark the parameter for READ access, set
*         VPATH to INTERNAL, set the type to character, and put
*         the text string as the static default.
            CALL PARSECON_MESTEXT ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. NEWOB ) THEN
*         Mark OBEY as valid for the action currently being
*         defined
            CALL PARSECON_SETOB ( .TRUE., STATUS )

         ELSE IF ( ACTCODE .EQ. NEWCAN ) THEN
*         Mark CANCEL as valid for the action currently being
*         defined
            CALL PARSECON_SETCAN ( .TRUE., STATUS )

         ELSE IF ( ACTCODE .EQ. SETTYP ) THEN
*         set up type field for parameter value
            CALL PARSECON_SETTYP ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETVAL ) THEN
*         set up a default value field for current parameter.
            CALL PARSECON_SETDEF ( WORD(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. OREQ ) THEN
*         Set up new entry on required value list for OBEY.
*         The parameter MUST already be on the name list.
            CALL PARSECON_OREQ ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. CREQ ) THEN
*         Set up new entry on required value list for CANCEL.
*         The parameter MUST already be on the name list.
            CALL PARSECON_CREQ ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. NEWRANGE ) THEN
*         Range restriction on parameter value.
            CALL PARSECON_SETRES ( .TRUE., STATUS )

         ELSE IF ( ACTCODE .EQ. NEWIN ) THEN
*         List restriction on parameter value.
            CALL PARSECON_SETRES ( .FALSE., STATUS )

         ELSE IF ( ACTCODE .EQ. DISCVALS ) THEN
*         list restriction on parameter value required by current
*         action
            CALL PARSECON_ACTRES ( .FALSE., STATUS )

         ELSE IF ( ACTCODE .EQ. CONTVALS ) THEN
*         range restriction on parameter value required by current
*         action
            CALL PARSECON_ACTRES ( .TRUE., STATUS )

         ELSE IF ( ACTCODE .EQ. PARLIST ) THEN
*         add new entry to parameter constraint list
            CALL PARSECON_PARLIST ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. ACTLIST ) THEN
*         add new entry to parameter constraint list required by
*         an action
            CALL PARSECON_ACTLIST ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETKEY ) THEN
*         insert a keyword for the parameter
            CALL PARSECON_SETKEY ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETPOS ) THEN
*         insert the command-line position for the parameter
            CALL PARSECON_SETPOS ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETACC ) THEN
*         set the parameter access-type
            CALL PARSECON_SETACC ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETVP ) THEN
*         set the parameter VPATH - the search path for obtaining values
            CALL PARSECON_SETVP ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETHEL ) THEN
*         store one-line help text
            CALL PARSECON_SETHEL ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETHKEY ) THEN
*         store multi-line help specifier
            CALL PARSECON_SETHKEY ( WORD(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETPTY ) THEN
*         Store PTYPE for the parameter
            CALL PARSECON_SETPTY ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETASS ) THEN
*         store the name of the structure to be associated with the
*         parameter
            CALL PARSECON_SETASS ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETPROM ) THEN
*         store the parameter's prompt string
            CALL PARSECON_SETPROM ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. STFACE ) THEN
*         Check for previous missing ENDINTERFACE
            CALL PARSECON_STFACE ( STATUS )

         ELSE IF ( ACTCODE .EQ. INAME ) THEN
*         Store interface name
            CALL PARSECON_SETFACE ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. FACEND ) THEN
*         Clear interface name from error report common block
*         and check for contiguous position numbers
            CALL PARSECON_FACEND ( STATUS )

         ELSE IF ( ACTCODE .EQ. SETMON ) THEN
*         a monolith is being declared
            CALL PARSECON_SETMON ( TOKEN(:TOKLEN), STATUS )
            MONFLAG = .TRUE.

         ELSE IF ( ACTCODE .EQ. ENDMON ) THEN
*         end of monolith declaration
            MONFLAG = .FALSE.

         ELSE IF ( ACTCODE .EQ. NEWPCOORDS ) THEN
*         start of parameter menu coordinates
            CALL PARSECON_NEWPCRDS ( STATUS )

         ELSE IF ( ACTCODE .EQ. SETPCOORDS ) THEN
*         a parameter menu coordinate
            CALL PARSECON_SETPCRDS ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETPMENU ) THEN
*         a parameter menu name
            CALL PARSECON_SETPMENU ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. NEWACOORDS ) THEN
*         start of action menu coordinates
            CALL PARSECON_NEWACRDS ( STATUS )

         ELSE IF ( ACTCODE .EQ. SETACOORDS ) THEN
*         an action menu coordinate
            CALL PARSECON_SETACRDS ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETAKEY ) THEN
*         an action keyword
            CALL PARSECON_SETAKEY ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETAHEL ) THEN
*         help line for an action
            CALL PARSECON_SETAHEL ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETAMENU ) THEN
*         an action menu name
            CALL PARSECON_SETAMENU ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETPP ) THEN
*         set the parameter PPATH - the search path for obtaining prompt
*         values
            CALL PARSECON_SETPP ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. SETHLIB ) THEN
*         set the help library specifier
            CALL PARSECON_SETHLIB ( WORD(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. ERROR ) THEN
*         unexpected token read.
            STATUS = PARSE__IVSYN
            CALL EMS_REP( 'PCN_READIFL1',
     :      'PARSECON: Unexpected token read', STATUS )

         ELSE IF ( ACTCODE .EQ. PROG ) THEN
*         Store program name
            CALL PARSECON_SETPROG ( TOKEN(:TOKLEN), STATUS )

         ELSE IF ( ACTCODE .EQ. EPDEC ) THEN
*         Store search-path for directory containing execution module
            CALL PARSECON_SETEPATH ( TOKEN(:TOKLEN), STATUS )

         ENDIF

*      Now take stock of what has happened
*      First look for the non-contiguous positions error or missing
*      ENDINTERFACE. This is so that the RESET action can be skipped
         IF ( ( STATUS .EQ. PARSE__NCPOS ) .OR.
     :        ( STATUS .EQ. PARSE__MISEND ) ) THEN
            IF ( .NOT. ERRFLAG ) THEN
               CALL PARSECON_ERROR ( LINENUM, ' ', STATUS )
               NUMERR = NUMERR + 1
            ENDIF
*        Now continue as if there was no error
            CALL EMS_ANNUL( STATUS )
         ENDIF

         IF ( STATUS .EQ. SAI__OK ) THEN
*         All OK - set new state and reset flag which prevented further
*         error messages being output
            STATE = NEWSTATE
            ERRFLAG = .FALSE.
*         Trap case where ENDINTERFACE has been found in a monolith -
*         that is not the end of the interface file.
            IF (( STATE .EQ. FINISHED ) .AND. MONFLAG ) THEN
               STATE = START
            ENDIF

         ELSE
*       Error detected or end of file
*       reset state to a likely recovery route.
            CALL PARSECON_RESET( STATE, STATUS )

*         Always report Special cases when TOKLEN -ve
*         but only report others if they are the first in a sequence.
            IF ( TOKLEN .EQ. -2 ) THEN
*            End-of-File
               STATUS = PARSE__NOEND
               CALL EMS_REP( 'PCN_READIFL2',
     :         'PARSECON: No "END" on interface file', STATUS )
               CALL PARSECON_ERROR( LINENUM, 'End_of_File', STATUS )
               NUMERR = NUMERR + 1

            ELSEIF ( TOKLEN .EQ. -1 ) THEN
*            Other read or lexing error
               CALL PARSECON_ERROR( LINENUM, ' ', STATUS )
               NUMERR = NUMERR + 1

            ELSEIF ( .NOT. ERRFLAG ) THEN
*            First in a sequence, count and report it and flag sequence
               CALL PARSECON_ERROR( LINENUM, WORD(:TOKLEN), STATUS )
               ERRFLAG = .TRUE.
               NUMERR = NUMERR + 1

            ENDIF

*         Now clean up depending upon status
*         Reset other status values
            IF (( STATUS .EQ. PARSE__NOMEM ) .OR.
     :          ( STATUS .EQ. PARSE__READERR )) THEN
*         Errors which should cause closedown with error status
               STATE = FINISHED

            ELSEIF ( STATUS .EQ. PARSE__NOEND ) THEN
*         Missing END statement, FINISH parsing but probably OK
*         to use resultant module.
               STATE = FINISHED
               CALL EMS_ANNUL( STATUS )

            ELSE
               CALL EMS_ANNUL( STATUS )

            ENDIF

         ENDIF

      ENDDO

*   Release the error context
      CALL EMS_RLSE

      END
