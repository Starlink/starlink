      SUBROUTINE FIO_OPEN( FILE, ACMODE, FORM, RECSZ, FD, STATUS )
*+
*  Name:
*     FIO_OPEN

*  Purpose:
*     Create/open a sequential file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_OPEN( FILE, ACMODE, FORM, RECSZ, FD, STATUS )

*  Description:
*     Open a sequential file with the specified access mode.
*     When the file is created, the specified carriage control mode and
*     maximum record size will be used.
*     Return a file descriptor which can be used to access the file.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Expression giving the name of the file to be opened.
*     ACMODE = CHARACTER * ( * ) (Given)
*        Expression giving the required access mode.
*        Valid modes are:
*        'READ' - Open the file READONLY. The file must exist.
*        'WRITE' - Create a new file and open it to write.
*        'UPDATE' - Open a file to write. The file must exist.
*        'APPEND' - Open a file to append. The file need not exist.
*     FORM = CHARACTER * ( * ) (Given)
*        Expression giving the required formatting of the file.
*        Valid formats are:
*        'FORTRAN' - Formatted file, normal Fortran interpretation
*                    of the first character of each record.
*        'LIST' - Formatted file, single spacing between records.
*        'NONE' - Formatted file, no implied carriage control.
*        'UNFORMATTED' - Unformatted, no implied carriage control.
*     RECSZ = INTEGER (Given)
*        Expression giving the maximum record size in bytes.
*        Set it to zero if the Fortran default is required.
*     FD = INTEGER (Returned)
*        Variable to contain the file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     See if there is a free file descriptor and Fortran unit number.
*     If so, check the access mode and if OK, open the file with
*     the FORTRAN 77 OPEN statement with appropriate specifiers.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     JHF: Jon Fairclough (IPMAF, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1984 (JHF):
*        Original FIO_CROP. From FIO_OPEN.
*     13-OCT-1986 (AJC):
*        Ensure FIO is started.
*     15-OCT-1987 (AJC):
*        Rename back to FIO_OPEN from FIO_CROP (RAL::AJC)
*     16-FEB-1988 (AJC):
*        Rationalize include files.
*     22-FEB-1988 (AJC):
*        Include recsz for 'UPDATE' and 'READ' .
*     29-FEB-1988 (AJC):
*        Improve prologue.
*        Convert acmode to upper case.
*     27-JUN-1988 (AJC):
*        Add FORM argument.
*        Allow unformatted files.
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     28-JAN-1992 (PMA):
*        Alter code to make it portable.
*        Move all of the OPEN statements to the end of the code so
*        they are grouped together as these need to be changed on
*        different machines.
*     29-JAN-1992 (PMA):
*        Fix bug whereby file opened for 'READ' access had the record
*        length incorrectly specified. The bug was that the two OPEN
*        statements were interchanged.
*     9-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     26-MAR-1992 (PMA):
*        Change the length of CC from 7 to 12 to prevent truncation.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     24-AUG-1992 (PMA):
*        Change the layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     4-AUG-1993 (PMA):
*        Add an error report if this routine fails to open the file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! Information on FIO files
*        FNAME( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Write)
*           File name
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Write)
*           File access mode
*        FREE( FIO__MXFIL ) = LOGICAL (Write)
*           File descriptor available ?
*        FUNIT( FIO__MXFIL ) = INTEGER (Write)
*           Fortran unit number for file
*        FRECSZ( FIO__MXFIL ) = INTEGER (Write)
*           Record size
      INCLUDE 'FIOGO_CMN'        ! FIO Initialisation Switches
*        FIOINT = LOGICAL (Read)
*           Whether FIO is started

*  Arguments Given:
      CHARACTER*(*) FILE
      CHARACTER*(*) ACMODE
      CHARACTER*(*) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIO_BLK           ! Block data subprogram

*  Local Variables:
      INTEGER RFD                ! Relative file descriptor
      INTEGER UNIT               ! Fortran unit number
      CHARACTER * ( FIO__SZMOD ) ACMODU
                                 ! ACMODE in upper case
      CHARACTER * ( 12 ) FORMU   ! FORM in upper case
      CHARACTER * ( 12 ) FORMAT  ! format specifier
      CHARACTER * ( 7 ) FSTAT    ! FORTRAN open status
      CHARACTER * ( 10 ) ACCESS  ! FORTRAN file access mode
      CHARACTER * ( 12 ) CC      ! Carriage control specifier
      LOGICAL RDONLY             ! File to be opened read-only
      LOGICAL CCNTL              ! Carriage control is specified
      LOGICAL RECLEN             ! Record length is to be specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize logical flags for which OPEN qualifers are to be used.
      RDONLY = .FALSE.
      CCNTL = .FALSE.
      RECLEN = .FALSE.

*  Ensure FIO is started
      IF ( .NOT. FIOINT ) THEN
         CALL FIO_START( STATUS )
      ENDIF

*  Convert ACMODE and FORM to upper case in ACMODU and FORMU.
      ACMODU = ACMODE
      CALL CHR_UCASE( ACMODU )
      FORMU = FORM
      CALL CHR_UCASE( FORMU )

*  Determine required format and carriage control
      IF ( FORMU .EQ. 'UNFORMATTED' ) THEN
         CC = 'NONE'
         FORMAT = 'UNFORMATTED'
      ELSE
         CC = FORMU
         FORMAT = 'FORMATTED'
      ENDIF

*  Get a File Descriptor
      CALL FIO1_GETFD( FD, RFD, STATUS )

*  Get a Fortran unit number
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL FIO_GUNIT( UNIT, STATUS )

*  Select the appropriate options for opeing the file.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( ACMODU .EQ. 'READ' ) THEN
               IF ( RECSZ .GT. 0 ) THEN
*  Specify record length
                  FSTAT = 'OLD'
                  ACCESS = 'SEQUENTIAL'
                  RDONLY = .TRUE.
                  RECLEN = .TRUE.
               ELSE
*  Accept default record length
                  FSTAT = 'OLD'
                  ACCESS = 'SEQUENTIAL'
                  RDONLY = .TRUE.
               ENDIF

            ELSE IF ( ACMODU .EQ. 'WRITE' ) THEN
               IF ( RECSZ .GT. 0 ) THEN
*  Specify record length
                  FSTAT = 'NEW'
                  ACCESS = 'SEQUENTIAL'
                  CCNTL = .TRUE.
                  RECLEN = .TRUE.
               ELSE
*  Accept default record length
                  FSTAT = 'NEW'
                  ACCESS = 'SEQUENTIAL'
                  CCNTL = .TRUE.
               ENDIF

            ELSE IF ( ACMODU .EQ. 'UPDATE' ) THEN
               IF ( RECSZ .GT. 0 ) THEN
*  Specify the record length
                  FSTAT = 'OLD'
                  ACCESS = 'SEQUENTIAL'
                  RECLEN = .TRUE.
               ELSE
*  Accept default record length
                  FSTAT = 'OLD'
                  ACCESS = 'SEQUENTIAL'
               ENDIF

            ELSE IF ( ACMODU .EQ. 'APPEND' ) THEN
               IF ( RECSZ .GT. 0 ) THEN
*  Specify record length
                  FSTAT = 'UNKNOWN'
                  ACCESS = 'APPEND'
                  CCNTL = .TRUE.
                  RECLEN = .TRUE.
               ELSE
*  Accept default record length
                  FSTAT = 'UNKNOWN'
                  ACCESS = 'APPEND'
                  CCNTL = .TRUE.
               ENDIF

            ELSE
               STATUS = FIO__IVACM
               CALL EMS_REP( 'FIO_OPEN_IVACM', 'Invalid access mode',
     :            STATUS )
            ENDIF

*  Do the actual opening of the file.
            CALL FIO1_OPEN( UNIT, FILE, FSTAT, FORMAT, ACCESS, RDONLY,
     :         CCNTL, CC, RECLEN, RECSZ, STATUS )

*  Record the current state.
            IF ( STATUS .EQ. SAI__OK ) THEN
               FNAME( RFD ) = FILE
               FACMOD( RFD ) = ACMODU
*  Mark the buffer as in use.
               FREE( RFD ) = .FALSE.
               FUNIT( RFD ) = UNIT
               FRECSZ( RFD ) = 0
            ELSE

* Report an error if we failed to open the file.
               CALL EMS_SETC( 'FNAME', FILE )
               CALL EMS_REP('FIO_OPEN_FAILED',
     :            'Failed to open file ^FNAME', STATUS )
            ENDIF
         ENDIF
      ENDIF

      END
