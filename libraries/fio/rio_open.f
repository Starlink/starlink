      SUBROUTINE RIO_OPEN( FILE, ACMODE, FORM, RECSZ, FD, STATUS )
*+
*  Name:
*     RIO_OPEN

*  Purpose:
*     Open a direct access file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_OPEN( FILE, ACMODE, FORM, RECSZ, FD, STATUS )

*  Description:
*     Open a direct access file with the specified access mode and
*     record size.
*     Return a file descriptor which can be used to access the file.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Expression giving the name of the file to be opened.
*     ACMODE = CHARACTER * ( * ) (Given)
*        Expression giving the required access mode.
*        Valid modes are:
*        'READ' - Open the file READONLY. The file must exist.
*        'WRITE' - Create a new file and open it to write/read.
*        'UPDATE' - Open a file to read/write. The file must exist.
*        'APPEND' - Open a file to write/read.
*                   If the file does not already exist, create it.
*                   (APPEND has no other effect for direct access)
*     FORM = CHARACTER * ( * ) (Given)
*        Expression giving the required record formatting.
*        'FORMATTED' or  'UNFORMATTED'
*     RECSZ = INTEGER (Given)
*        Expression giving the record size in bytes.
*     FD = INTEGER (Returned)
*        Variable to contain the file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  RECSZ is used to specify the length of a record in bytes.
*     -  If ACMODE is 'WRITE' this is the record length of the new file.
*     -  If ACMODE is 'APPEND' and the file does not already exist, then
*        this is the record length of the new file.
*     -  If ACMODE is 'APPEND' and the file already exists, then RECSZ
*        must match the record length of the existing file.
*     -  If ACMODE is 'UPDATE' then RECSZ must match the record length
*        of the existing file.
*     -  If ACMODE is 'READ' then RECSZ must match the record length of
*        the existing file.
*     -  An exception to the last point is allowed on VMS, where it is
*        possible to set RECSZ to zero when opening a file for read
*        access.

*  Algorithm:
*     See if there is a free file descriptor and Fortran unit number.
*     If so, check the access mode and if OK, call RIO1_OPEN to open
*     the file.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     SLW: Sid Wright (Starlink, UCL)
*     JHF: Jon Fairclough (IPMAF, RAL)
*     KFH: Ken Hartley (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980: Original. (SLW)
*     10-May-1983: Tidy up for Starlink version. (SLW)
*     02-Nov-1984: Organization set to RELATIVE when 'NEW'.
*        Recsz added to the argument list.
*        Blocksize now an FIO constant.(JHF)
*     17-Feb-1988: Rationalize include files  (AJC)
*     25-Feb-1988: Improve prologue  (AJC)
*     26-Feb-1988: Convert ACMODE to upper case
*        Ensure FIO is started  (AJC)
*     29-Feb-1988: Correct description of RECSZ
*        Add organization RELATIVE and blocksize
*        to OPEN APPEND  (AJC)
*     28-Jun-1988: Add the FORM argument (AJC)
*     01-Nov_1991: Change to new style prologue (KFH)
*        Insert IMPLCIT NONE  (KFH)
*        Change variable vmserr to ioerr (KFH)
*        Change fac_$name to fac1_name (KFH)
*        Replace tabs with spaces in end-of-line comments (KFH)
*     26-FEB-1992 (PMA):
*        Tidy up the prologue.
*     17-MAR-1992 (PMA):
*        Take out the machine specific OPEN statements and put them into
*        RIO1_OPEN.
*        Remove the variable IOERR.
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
*     21-JAN-1994 (PMA):
*        Change the description of RECSZ and add some notes to describe
*        its function.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO symbolic constants
      INCLUDE 'FIO_SYS'          ! FIO internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOGO_CMN'        ! FIO initialization
*        FIOINT = LOGICAL (Read)
*           Whether Fio is started
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Write)
*           File access mode
*        FNAME( FIO__MXFIL ) = CHARACTER * ( FIO__SZFNM ) (Write)
*           File names
*        FRECSZ( FIO__MXFIL ) = INTEGER (Write)
*           Record size
*        FREE( FIO__MXFIL ) = LOGICAL (Write)
*           File descriptor available ?
*        FUNIT( FIO__MXFIL ) = INTEGER (Write)
*           Fortran unit number for file

*  Arguments Given:
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD

*  External References:
      EXTERNAL FIO_BLK           ! Block data subprogram

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 12 ) FORMU   ! Format in upper case
      CHARACTER * ( FIO__SZMOD ) ACMODU ! ACMODE in upper case
      INTEGER RECLEN             ! Record length in longwords.
      INTEGER RFD                ! Relative file descriptor
      INTEGER UNIT               ! Fortran unit number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure FIO is started
      IF ( .NOT. FIOINT ) THEN
         CALL FIO_START( STATUS )
      ENDIF

*  Convert ACMODE to upper case in ACMODU
      ACMODU = ACMODE
      CALL CHR_UCASE( ACMODU )

*  Convert FORM to upper case
      FORMU = FORM
      CALL CHR_UCASE( FORMU )

*  Get a File Descriptor
      CALL FIO1_GETFD( FD, RFD, STATUS )

*  Get a Fortran unit number
      CALL FIO_GUNIT( UNIT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check the access mode is correct.
         IF ( ACMODU .NE. 'READ' .AND.
     :       ACMODU .NE. 'WRITE' .AND.
     :       ACMODU .NE. 'UPDATE' .AND.
     :       ACMODU .NE. 'APPEND' ) THEN
            STATUS = FIO__IVACM
            CALL EMS_REP( 'RIO_OPEN_IVACM', 'Invalid file access mode',
     :         STATUS )
         ENDIF

*  Do the actual opening of the file.
         CALL RIO1_OPEN( UNIT, FILE, ACMODU, FORMU, RECSZ, RECLEN,
     :      STATUS )

*  Record current state
         IF ( STATUS .EQ. SAI__OK ) THEN
            FNAME( RFD ) = FILE
            FACMOD( RFD ) = ACMODU
*  Mark buffer as in use
            FREE( RFD ) = .FALSE.
            FUNIT( RFD ) = UNIT
            FRECSZ( RFD ) = RECLEN
            ELSE

* Report an error if we failed to open the file.
               CALL EMS_SETC( 'FNAME', FILE )
               CALL EMS_REP('RIO_OPEN_FAILED',
     :            'Failed to open file ^FNAME', STATUS )
         ENDIF
      ENDIF

      END
