      SUBROUTINE FTS1_SCTAB( HEADER, PNDSCF, PNTAB, MEDIUM, MD, NCARD,
     :                       SCARD, NDIM, DIMS, LOGHDR, FD, CFN, SUBFIL,
     :                       PREFIX, AUTO, BLKSIZ, ACTSIZ, BFPNTR,
     :                       OFFSET, CURREC, RCPNTR, STATUS )
*+
*  Name:
*     FTS1_SCTAB

*  Purpose:
*     Creates an ASCII catalogue and description file in SCAR format
*     from a FITS tape or disk file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_SCTAB( HEADER, PNDSCF, PNTAB, MEDIUM, MD, NCARD, SCARD,
*    :                 NDIM, DIMS, LOGHDR, FD, CFN, SUBFIL, PREFIX,
*    :                 AUTO, BLKSIZ, ACTSIZ, BFPNTR, OFFSET, CURREC,
*    :                 RCPNTR, STATUS )

*  Description:
*     This is a server routine for FITSIN/FITSDIN.  It packages up the
*     operations required to handle a FITS ASCII table file and turn it
*     into an ASCII catalogue and SCAR description file.  The names of
*     the output files are recorded in the logfile.

*  Arguments:
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The FITS headers in 80-character records.
*     PNDSCF = CHARACTER * ( * ) (Given)
*        The name of the parameter by which the filename of the SCAR
*        description file will be obtained.
*     PNTAB = CHARACTER * ( * ) (Given)
*        The name of the parameter by which the filename of the
*        table will be obtained.
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported are
*        'DISK' for a disk file, and 'TAPE' for standard magnetic tape.
*     MD = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     SCARD = INTEGER (Given)
*        The number of the card from where the searches of the header
*        will begin.  This is needed because the headers make contain a
*        dummy header prior to an extension.
*     NDIM = INTEGER (Given)
*        Dimensionality of the table.  At present only 2-d is supported.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the table.
*     LOGHDR = LOGICAL (Given)
*        If true there is a log file open and records of the output file
*        names will be written to it.
*     FD = INTEGER (Given)
*        The file descriptor for the log file.  It is ignored if %LOGHDR
*        is false.
*     CFN = CHARACTER * ( * ) (Given)
*        The number on the tape of the FITS file being processed if
*        MEDIUM is 'TAPE', or the input disk-FITS filename if MEDIUM
*        is 'TAPE'.
*     SUBFIL = INTEGER (Given)
*        The number of the sub-file/extension within the current FITS
*        file being processed.
*     PREFIX = CHARACTER * ( * ) (Given)
*        The prefix to be given to the file name of catalogues produced
*        in automatic mode. It is ignored when %MEDIUM = 'DISK'.
*     AUTO = LOGICAL (Given)
*         If true the processing should be done in automatic mode, where
*         the user is not prompted for file names, since these are
*         generated from the prefix, file and sub-file numbers in the
*         case where the medium is tape.  The form is prefix_file or
*         prefix_file_subfile if the subfile is greater than 1.
*         %MEDIUM = 'DISK' the prefix is ignored.
*     BLKSIZ = CHARACTER * ( * ) (Given and Returned)
*        The maximum block size and dimension of %BUFFER.
*     ACTSIZ = CHARACTER * ( * ) (Given and Returned)
*        The actual block size on tape or disk (a multiple of the FITS
*        record length of 2880 bytes). It is only an input argument for
*        %MEDIUM = 'DISK'.
*     BFPNTR = INTEGER (Given)
*        Pointer to the buffer containing catalogue data, the buffer
*        itself will be updated each time a tape block is read.
*        If the offset equals the block size then the existing data
*        in the buffer will not be used.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted
*        as the header plus any earlier FITS files.
*     CURREC = LOGICAL (Given and Returned)
*        If true the current FITS record is to be used immediately, i.e.
*        it has alrady been read from tape or disk into %RECORD.
*     RCPNTR = INTEGER (Given)
*        A pointer to the buffer to hold the current FITS record of 36
*        80-character card images.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The magnetic tape or disk file must already be associated, and
*     any log file must be opened.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 November 18 (MJC):
*        Original version.
*     1990 November 30 (MJC):
*        Added MEDIUM, CURREC and RECORD arguments; disk-file access
*        with disk-filename handling via character file argument; call
*        revised table-creating subroutine.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument for revised FTS1_SDSCF call.
*     1992 December (RDS):
*        Portability mods including: BUFFER and RECORD passed in as
*        pointers.
*     1992 December 30 (MJC):
*        Inquire the operating system in order to find the file name.
*     1992 April 23 (MJC):
*        Allow a null table file, skipping over it.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     2010 August 22 (MJC):
*        Modern commenting style.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NCARD
      CHARACTER*80  HEADER( NCARD )
      CHARACTER * ( * ) PNDSCF
      CHARACTER * ( * ) PNTAB
      CHARACTER * ( * ) MEDIUM
      INTEGER MD
      INTEGER SCARD
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      LOGICAL LOGHDR
      INTEGER FD
      CHARACTER * ( * ) CFN
      INTEGER SUBFIL
      CHARACTER * ( * ) PREFIX
      LOGICAL AUTO
      INTEGER BLKSIZ

*  Arguments Given and Returned:
      INTEGER ACTSIZ
      INTEGER BFPNTR
      INTEGER OFFSET
      LOGICAL CURREC
      INTEGER RCPNTR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length less trailing blanks

*  Local Constants:
      INTEGER RECLEN             ! FITS record length
      PARAMETER ( RECLEN = 2880 )

*  Local Variables:
      CHARACTER*204 DSCFNM       ! Description file's name
      CHARACTER*80 DUMMY         ! Buffer for output of message
      CHARACTER*6 FILNO          ! Number of file/subfile used to
                                 ! generate output file names
      INTEGER IBR                ! Index of a right-hand bracket
      INTEGER IEX                ! Index of a period (first character in
                                 ! a file extension)
      CHARACTER*24 MACHIN        ! Machine name
      INTEGER NCFILN             ! Number of characters in file number
      INTEGER NCPREF             ! Number of characters in file prefix
      INTEGER NCROOT             ! Number of characters in file root
                                 ! name
      CHARACTER*20 NODE          ! Node name
      LOGICAL OVMS               ! The operating system is VMS or RSX?
      LOGICAL PATH               ! There is a path to search through
      INTEGER RDISP              ! The displacement within the current
                                 ! FITS record
      CHARACTER*10 RELEAS        ! Release of operating system
      CHARACTER*10 SYSNAM        ! Operating system
      CHARACTER*20 TABNAM        ! Table's name
      CHARACTER*50 VERSIO        ! Sub-version of operating system
      CHARACTER*(DAT__SZLOC) WKLOC ! Locator to workspace for data
                                 ! conversion
      INTEGER WKPNTR( 1 )        ! Pointer to work array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the record-displacement pointer.
*  ===========================================

*  The FITS data must start in a new record.  Now this record may
*  already be the current FITS record; in this case the record should
*  be procesed from its start, hence a zero displacement.  The flag
*  can be switched off as this positioning information is imparted
*  to the record displacement.  If the current record is not the
*  first FITS record containing the data, set the the displacement
*  to the end of the FITS record.  This will cause a new record be
*  read.  For simple FITS the displacement will be either 0 or 2880.
*  This is only needed in thisd routine to permit tables to be skipped.
      IF ( CURREC ) THEN
         RDISP = 0
         CURREC = .FALSE.
      ELSE
         RDISP = RECLEN
      END IF

*  Automatic mode
*  ==============

*  In automatic mode we have to generate a file name for the table.
      IF ( AUTO ) THEN

*  Get the length of the file name or number.
         NCFILN = CHR_LEN( CFN )

*  The file is specified by a number for a tape.
         IF ( MEDIUM .EQ. 'TAPE' ) THEN

*  The table's name is the prefix followed by the file number.
            NCPREF = CHR_LEN( PREFIX )
            TABNAM = PREFIX( :NCPREF )//CFN( :NCFILN )

*  Find the new length of the name string.
            NCROOT = NCPREF + NCFILN

*  The file is specified by name for disk file.
         ELSE IF ( MEDIUM .EQ. 'DISK' ) THEN

*  Find the operating system.
            CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN,
     :                      STATUS )
            CALL CHR_UCASE( SYSNAM )

            OVMS = INDEX( SYSNAM, 'VMS' ) .NE. 0 .OR.
     :             INDEX( SYSNAM, 'RSX' ) .NE. 0

*  Extract the filename.
*  =====================
            IF ( OVMS ) THEN

*  First look for a right bracket for a directory.  The filename will
*  start one character after that or from the first character.
               IBR = INDEX( CFN, ']' )

*  Look for the start of the file extension.
               IEX = INDEX( CFN( MAX( 1, IBR ): ), '.' )

            ELSE

*  It is a UNIX name. Remove any path present in the command
*  to derive the command name by looking for the last slash.
               PATH = .TRUE.
               IEX = 0
               IBR = 0
               DO WHILE ( PATH )
                  IEX = INDEX( CFN( IBR+1: ), '/' )
                  PATH = IEX .NE. 0
                  IBR = IBR + IEX
               END DO

*  Look for the start of the file extension.
               IEX = INDEX( CFN( MAX( 1, IBR ): ), '.' )
            END IF

*  The name extends to one character less than the extension.
*  Find the new length of the name string and extract the name.
            IF ( IEX .EQ. 0 ) THEN
               NCROOT = NCFILN
               TABNAM = CFN
            ELSE IF ( IBR .EQ. 0 ) THEN
               NCROOT = IEX - 1
               TABNAM = CFN( 1:NCROOT )
            ELSE
               NCROOT = IEX - 2
               TABNAM = CFN( IBR+1:IBR+NCROOT )
            END IF
         END IF

*  Special case when more than one `data unit' (n-d array, table)
*  per FITS file so have to distinguish between sub-files.
         IF ( SUBFIL .GT. 1 ) THEN
            CALL CHR_ITOC( SUBFIL, FILNO, NCFILN )
            CALL CHR_APPND( '_'//FILNO( :NCFILN ), TABNAM, NCROOT )

         END IF
      END IF

*  Generate the FACTS file from the header cards.
*  ==============================================
      CALL FTS1_SDSCF( NCARD, HEADER, SCARD, PNDSCF, AUTO, TABNAM,
     :                 DSCFNM, NDIM, DIMS, STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( LOGHDR ) THEN

*  Write output filename to the log file.
            CALL FIO_WRITE( FD, ' ', STATUS )
            IF ( DSCFNM .EQ. ' ' ) THEN
               DUMMY = 'No description file has been created.'
            ELSE
               DUMMY = 'The created description file is '//DSCFNM
            END IF
            CALL FIO_WRITE( FD, DUMMY, STATUS )
         END IF

*  Create the table file and copy data into it.
*  ============================================

*  Get some workspace to store a line of the table, and map it.
         CALL AIF_GETVM( '_BYTE', 1, DIMS( 1 ) + 1, WKPNTR( 1 ), WKLOC,
     :                   STATUS)

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'FITSIN_WSP',
     :        'Unable to get workspace for storing a line in the '/
     :        /'table.', STATUS )
            GOTO 999
         END IF

*  Start a new error context.
         CALL ERR_MARK

*  Create the table file and copy data into it.  Note the dummy
*  length argument after the status is needed for passing the
*  mapped character array on UNIX platforms.  It is ignored on VMS.
         CALL FTS1_RSTAB( %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                    MEDIUM, MD, PNTAB,
     :                    TABNAM, AUTO, DIMS( 1 ), DIMS( 2 ), BLKSIZ,
     :                    ACTSIZ, %VAL( CNF_PVAL( BFPNTR ) ),
     :                    OFFSET, CURREC,
     :                    %VAL( CNF_PVAL( RCPNTR ) ), STATUS,
     :                    %VAL( DIMS( 1 ) + 1 ) )

*  Check for option not to create file.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( 'NOOUT',
     :        'Null parameter -- no output file created', STATUS )

*  If there may be extensions following the data in this FITS sub-file
*  to be processed, so the data in the current sub-file must be skipped
*  over.  Offset on exit is at the end of a record.  The size is the
*  number of characters per line times the number of lines in the table.
*  There is one byte per value, a GCOUNT of 1, and PCOUNT of 0.
            CALL FTS1_SKIP( MEDIUM, MD, DIMS( 1 ) * DIMS( 2 ), 1, 1, 0,
     :                      BLKSIZ, ACTSIZ, %VAL( CNF_PVAL( BFPNTR ) ),
     :                      OFFSET,
     :                      %VAL( CNF_PVAL( RCPNTR ) ), RDISP, STATUS )

*  Release the error context.
            CALL ERR_RLSE

*  Tidy the workspace.
            CALL AIF_ANTMP( WKLOC, STATUS )

            GO TO 999

*  Any errors creating the table?
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  Report errors and abort.
            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'FTS1_SCTAB_NOFILE',
     :           'FITSIN: No table file created.', STATUS )
            END IF
            CALL ERR_RLSE
            CALL AIF_ANTMP( WKLOC, STATUS )

            GOTO 999
         END IF

*  Release the new error context.
         CALL ERR_RLSE

         IF ( LOGHDR .AND. TABNAM .NE. ' ' ) THEN

*  Write output filename to the log file.
            CALL FIO_WRITE( FD, ' ', STATUS )
            DUMMY = 'The created table file is '//TABNAM//'.'
            CALL FIO_WRITE( FD, DUMMY, STATUS )
         END IF

*  Tidy the workspace.
         CALL AIF_ANTMP( WKLOC, STATUS )

      END IF

  999 CONTINUE

      END
