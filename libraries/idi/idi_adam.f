*-----------------------------------------------------------------------
*+  IDI1_PINIT - Initialise the parameter common blocks

      SUBROUTINE IDI1_PINIT

*    Description :
*     <description of what the subroutine does>
*
*    Invocation :
*     CALL IDI1_PINIT
*
*    Method :
*     <description of how the subroutine works>
*
*    Deficiencies :
*     <description of any deficiencies>
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     June 1989
*     March 1991  Use MAXPAR constant
*     February 1992  Unix version
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

*    Global variables :
      INCLUDE 'idi_compar'

*    Local Constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER J

*    Internal References :
*     <declarations for internal functions>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*   Initialise the common block entries
      DO J = 1, MAXPAR
         PTFREE( J ) = .TRUE.
         PTDIDS( J ) = 0
         PTNAME( J ) = ' '
      ENDDO

      END

************************************************************************

      SUBROUTINE IDI_ANNUL ( DISPID, STATUS )

*+
*  Name:
*     IDI_ANNUL
*
*  Purpose:
*     Annul the given display identifier
*
*  Invocation:
*     CALL IDI_ANNUL( DISPID, STATUS )
*
*  Description:
*     Annul the given display identifer. The device is closed.
*
*  Arguments
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Do not check the inherited status.
*     Verify the display identifier.
*     Clear the corresponding entries in the common blocks
*     and decrement the reference counter.
*     Close the device.
*
*  Copyright:
*     Copyright (C) 1989, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton  (Durham University)
*
*  History:
*     June 1989 (NE):
*        Original version
*     April 1990 (NE):
*        Proper error handling
*     March 1991 (NE):
*        Remove references specific to the Ikon driver
*     February 1992 (NE):
*        Unix version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'IDI_ERR'

*  Global variables :
      INCLUDE 'idi_did'
      INCLUDE 'idi_compar'

*  Arguments Given :
      INTEGER DISPID

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER I, LENS, LSTAT

      CHARACTER STRING * 72
*.

*   Do not check status on entry, and use a local status flag
      LSTAT = IDI__OK

*   Check that this identifier is valid
      IF ( ( DISPID .GE. 1 ) .AND. ( DISPID .LE. NUMIDS ) ) THEN

*   Check this identifier against all entries in the common block
         DO I = 1, MAXPAR
            IF ( DISPID .EQ. PTDIDS( I ) ) THEN

*   Free the common block entries
               PTFREE( I ) = .TRUE.
               PTDIDS( I ) = 0
               PTNAME( I ) = ' '

*   Decrement the reference counter
               IF ( PTREF .GT. 0 ) THEN
                  PTREF = PTREF - 1
               ENDIF
            ENDIF
         ENDDO

*   Close the display
         CALL IIDCLO( DISPID, LSTAT )

*   If an error occured then report it
         IF ( LSTAT .NE. IDI__OK ) THEN
            CALL IIDERR( LSTAT, STRING, LENS )
            CALL ERR_REP( 'IDI_ANNUL_IDCLO', STRING(:LENS),LSTAT )
         ENDIF

*   Otherwise indicate that the picture id is invalid
      ELSE
         LSTAT = IDI__INDID
         CALL IIDERR( LSTAT, STRING, LENS )
         CALL ERR_REP( 'IDI_ANNUL_INDID', STRING(:LENS), LSTAT )
      ENDIF

*   Return the local error status if the global status is zero
      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = LSTAT
      ENDIF

      END

************************************************************************

      SUBROUTINE IDI_ASSOC ( PNAME, ACMODE, DISPID, STATUS )

*+
*  Name:
*     IDI_ASSOC
*
*  Purpose:
*     Open IDI in the ADAM environment
*
*  Invocation:
*     CALL IDI_ASSOC( PNAME, ACMODE, DISPID, STATUS )
*
*  Description:
*     Associate an IDI device with a parameter in the ADAM environment
*     and return a display identifer for that device. If the access
*     mode is 'WRITE' the device is reset, otherwise the device is
*     opened without being reset.
*
*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Name of parameter used for accessing device name
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode: 'READ', 'WRITE' or 'UPDATE'
*     DISPID = INTEGER (Returned)
*        Display identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     If the reference counter is zero initialise the common blocks.
*     If this parameter has been used before then return the identifier.
*     Search for an empty slot in the common blocks.
*     Prompt for a device through the parameter system.
*     Try opening this device with IDI.
*     If the access mode is 'WRITE' reset the device.
*     Store the parameter information in the common block.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     June 1989 (NE):
*        Original version
*     April 1990 (NE):
*        Proper error handling
*     March 1991 (NE):
*        Remove references specific to Ikon driver
*     February 1992 (NE):
*        Unix version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'IDI_ERR'

*  Global variables :
      INCLUDE 'idi_compar'

*  Arguments Given :
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) ACMODE

*  Arguments Returned :
      INTEGER DISPID

*  Status :
      INTEGER STATUS

*    External references :
      EXTERNAL IDI_DATID

*  Local variables :
      LOGICAL DONE, GOTIT, GOTONE

      INTEGER I, LENS, NMCODE

      CHARACTER DEVNAM * 80, LACMOD * 6, STRING * 72
      CHARACTER * ( PAR__SZNAM ) LPNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ENDIF

*   If the reference count is zero the do some initialisation
      IF ( PTREF .EQ. 0 ) THEN
         CALL IDI1_PINIT
      ENDIF

*   Copy parameter name to local variable and convert to upper case
      LPNAME = PNAME
      CALL CHR_UCASE( LPNAME )

*   See if this parameter has been used already
      GOTIT = .FALSE.
      I = 1
      DO WHILE ( ( .NOT. GOTIT ) .AND. ( I .LE. MAXPAR ) )
         IF ( LPNAME .EQ. PTNAME( I ) ) THEN
            GOTIT = .TRUE.
         ELSE
            I = I + 1
         ENDIF
      ENDDO

*   If the parameter has already been used then return identifier
      IF ( GOTIT ) THEN
         DISPID = PTDIDS( I )

*   Otherwise read the name from the parameter system
      ELSE

*   Find a free element in the parameter storage
         GOTONE = .FALSE.
         I = 1
         DO WHILE ( ( .NOT. GOTONE ) .AND. ( I .LE. MAXPAR ) )
            IF ( PTFREE( I ) ) THEN
               GOTONE = .TRUE.
            ELSE
               I = I + 1
            ENDIF
         ENDDO

*   Flag an error if there is no more room
         IF ( .NOT. GOTONE ) THEN
            STATUS = IDI__COOVF
            CALL IIDERR( STATUS, STRING, LENS )
            CALL ERR_REP( 'IDI_ASSOC_COOVF', STRING(:LENS), STATUS )
            GOTO 99
         ENDIF

*   Inquire the parameter name from the system
         CALL SUBPAR_FINDPAR( LPNAME, NMCODE, STATUS )

*   Loop trying to get the name of the graphics device
         DONE = .FALSE.
         DO WHILE ( ( .NOT. DONE ) .AND. ( STATUS .EQ. SAI__OK ) )

*   Inquire the name from the parameter system
            CALL SUBPAR_GETNAME( NMCODE, DEVNAM, STATUS )

*   If the user wants to abort then finish
            IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :           ( STATUS .EQ. PAR__ABORT ) .OR.
     :           ( STATUS .EQ. PAR__NOUSR ) ) THEN
               DONE = .TRUE.

*   If some other error has occured then finish as well
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               DONE = .TRUE.

*   If the parameter system has got a name then...
            ELSE

*   Try opening IDI on this device
               CALL IIDOPN( DEVNAM, DISPID, STATUS )

*   If context recovery failed then reset status
               IF ( STATUS .EQ. IDI__NOREC ) THEN
                  STATUS = IDI__OK
               ENDIF

*   Perform a reset if access mode is 'WRITE'
*   Use a local variable for the access mode and convert to upper case
               IF ( STATUS .EQ. SAI__OK ) THEN
                  LACMOD = ACMODE
                  CALL CHR_UCASE( LACMOD )
                  IF ( LACMOD .EQ. 'WRITE' ) THEN
                     CALL IIDRST( DISPID, STATUS )

*   Check that the reset worked
                     IF ( STATUS .NE. IDI__OK ) THEN
                        CALL IIDERR( STATUS, STRING, LENS )
                        CALL ERR_REP( 'IDI_ASSOC_IDRST',
     :                                STRING(:LENS), STATUS )
                        GOTO 99
                     ENDIF
                  ENDIF

*   Copy the parameter details into the common block
                  PTNAME( I ) = LPNAME
                  PTDIDS( I ) = DISPID
                  PTFREE( I ) = .FALSE.

*   Increment the reference counter
                  PTREF = PTREF + 1

*   Everything is OK so do not repeat the loop
                  DONE = .TRUE.

*   Given device name failed to open IDI so try again
               ELSE
                  CALL IIDERR( STATUS, STRING, LENS )
                  CALL ERR_REP( 'IDI_ASSOC_IDOPN', STRING(:LENS),
     :                          STATUS )
                  CALL PAR_CANCL( PNAME, STATUS )
                  CALL ERR_FLUSH( STATUS )
               ENDIF
            ENDIF
         ENDDO
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IDI_CANCL ( PNAME, STATUS )
*+
*  Name:
*     IDI_CANCL
*
*  Purpose:
*     Cancel the ADAM device parameter
*
*  Invocation:
*     CALL IDI_CANCL( PNAME, STATUS )
*
*  Description:
*     Cancel the association of an ADAM parameter with an IDI device.
*     Any devices associated with the parameter are closed.
*
*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Parameter name
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Do not check the inherited status.
*     Close any device associated with this parameter.
*     Clear the parameter common blocks
*     and decrement the reference counter.
*     Cancel the parameter.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     June 1989 (NE):
*        Original version
*     April 1990 (NE):
*        Proper error handling
*     March 1991 (NE):
*        Remove references specific to the Ikon driver
*     February 1992 (NE):
*        Remove reference to GOTIT variable
*        Unix version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'IDI_ERR'

*  Global variables :
      INCLUDE 'idi_did'
      INCLUDE 'idi_compar'

*  Arguments Given :
      CHARACTER * ( * ) PNAME

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER I, LENS, LSTAT

      CHARACTER STRING * 72
      CHARACTER * ( PAR__SZNAM ) LPNAME
*.

*   Do not check status on entry, and use a local status flag
      LSTAT = IDI__OK

*   Copy parameter name to local variable and convert to upper case
      LPNAME = PNAME
      CALL CHR_UCASE( LPNAME )

*   Look to see if this parameter name has been used already
      DO I = 1, MAXPAR
         IF ( LPNAME .EQ. PTNAME( I ) ) THEN

*   Close this device
            IF ( DTYPE( PTDIDS( I ) ) .NE. 0 ) THEN
               CALL IIDCLO( PTDIDS( I ), LSTAT )

*   If an error occured then report it
               IF ( LSTAT .NE. IDI__OK ) THEN
                  CALL IIDERR( LSTAT, STRING, LENS )
                  CALL ERR_REP( 'IDI_CANCL_IDCLO', STRING(:LENS),
     :                          LSTAT )
               ENDIF
            ENDIF

*   Release the storage associated with this entry in the common blocks
            PTFREE( I ) = .TRUE.
            PTDIDS( I ) = 0
            PTNAME( I ) = ' '

*   Decrement the reference counter
            IF ( PTREF .GT. 0 ) THEN
               PTREF = PTREF - 1
            ENDIF
         ENDIF
      ENDDO

*   Cancel the parameter
      CALL PAR_CANCL( LPNAME, LSTAT )

*   If the inherited status is OK then return the local status
      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = LSTAT
      ENDIF

      END

************************************************************************
*   IDI_DATID - Block data for initialising the common blocks

      BLOCK DATA IDI_DATID

*   Include the IDI parameters and the common block definitions
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'idi_compar'

*   Initialise the ADAM reference counter
      DATA PTREF / 0 /

      END

