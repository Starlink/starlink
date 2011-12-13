      SUBROUTINE FTS1_NDF( HEADER, BFPNTR, RCPNTR, AUTO, PNNDF, MEDIUM,
     :                     MD, VMS, LENDIA, SIZE, NDIM, DIMS, BPV,
     :                     FMTCNV, FMTIN, FMTOUT, IEEE, BADPIX, BLANK,
     :                     BSCALE, BZERO, DARRAY, NONSDA, GCOUNT,
     :                     PCOUNT, MXPARM, PTYPE, PSCALE, PZERO, FILROO,
     :                     LOGHDR, FD, CFN, SUBFIL, GEXTND, NCARD,
     :                     SCARD, NENCOD, ENCODS, BLKSIZ, ACTSIZ,
     :                     OFFSET, CURREC, NEXT, PARAMS, STATUS )
*+
*  Name:
*     FTS1_NDF

*  Purpose:
*     Makes an NDF from a simple or group-format FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_NDF( HEADER, BFPNTR, RCPNTR, AUTO, PNNDF, MEDIUM, MD,
*                    VMS, LENDIA, SIZE, NDIM, DIMS, BPV, FMTCNV, FMTIN,
*                    FMTOUT, IEEE, BADPIX, BLANK, BSCALE, BZERO, DARRAY,
*                    NONSDA, GCOUNT, PCOUNT, MXPARM, PTYPE, PSCALE,
*                    PZERO, FILROO, LOGHDR, FD, CFN, SUBFIL, GEXTND,
*                    NCARD, HEADER, SCARD, NENCOD, ENCODS, BLKSIZ,
*                    ACTSIZ, OFFSET, CURREC, NEXT, PARAMS, STATUS )

*  Description:
*
*     This is a server routine for FITSIN/FITSDIN, hence the large
*     argument list.  It packages up the operations required to create
*     and name an NDF; copy the FITS data to the NDF's data array,
*     performing a data conversion if requested and flagging blank data
*     with the standard bad-pixel values; generate the other
*     components: title, units, WCS, axis structure and the FITS
*     extension.  For group-format FITS data, a series of NDFs are
*     created, one per group, each with a generated filename.  A null
*     NDF may be given and this routine will exit, but permit the
*     calling routine to continue to the next FITS file.

*  Arguments:
*     HEADER( * ) = CHARACTER * 80 (Given)
*        The FITS headers in 80-character records.
*     BFPNTR = INTEGER (Given)
*        Pointer to BUFFER( BLKSIZ ) = CHARACTER * ( 1 ) (Given and
*        Returned).  The buffer containing the block of data.  This is
*        only read when %OFFSET does not equal %ACTSIZ, i.e. there are
*        some non-header data within it.
*     RCPNTR = INTEGER (Given)
*        Pointer to RECORD( 36 ) = CHARACTER * ( 80 ) (Given and
*        Returned).  The buffer to hold the current FITS record.
*     AUTO = LOGICAL (Given)
*        If true the processing should be done in automatic mode, where
*        the user is not prompted for file names, since these are
*        generated from %FILROO with the sub-file or group numbers
*        appended.
*     PNNDF = CHARACTER * ( * ) (Given)
*        The name of the parameter by which the filename of the output
*        NDF will be obtained.
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported are
*        'DISK' for a disk file, and 'TAPE' for standard magnetic tape.
*     MD = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     VMS = LOGICAL (Given)
*        If true, the operating system is VMS or RSX.  If false, the
*        operating system is assumed to be UNIX.
*     LENDIA = LOGICAL (Given)
*        If true, the machine uses Little Endian byte order (bytes
*        swapped compared to FITS).  LENDIA is ignored when
*        VMS = .TRUE..
*     SIZE = INTEGER (Given)
*        The number of elements in the data array.
*     NDIM = INTEGER (Given)
*        Dimensionality of the NDF.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the NDF.
*     BPV = INTEGER (Given)
*        The number of bytes per data value.
*     FMTCNV = LOGICAL (Given)
*        If true, format conversion from the integer FITS data to
*        the real output data array is required.  This is ignored
*        when BADPIX is false (which should be the case for IEEE
*        floating-point data).
*     FMTIN = CHARACTER * ( * ) (Given)
*        The HDS format of the data in the FITS file.  It will be
*        ignored if there is no format conversion.
*     FMTOUT = CHARACTER * ( * ) (Given)
*        The destination HDS format of the data array in the output
*        file.
*     IEEE = LOGICAL (Given)
*        If true the FITS data are in IEEE floating-point format.
*     BADPIX = LOGICAL (Given)
*        If true the data-blank was defined in the FITS header.  It
*        will be ignored if the data are in IEEE format.
*     BLANK = INTEGER (Given)
*        The data-blank value equivalent to the bad-pixel flag.  It
*        should be ignored if %BADPIX is false.
*     BSCALE = REAL (Given)
*        The scale factor of the FITS integer data for their conversion
*        to the true floating-point values.
*     BZERO = REAL (Given)
*        The offset of the FITS integer data for their conversion to
*        the true floating-point values.
*     DARRAY = LOGICAL (Given)
*        If true there is a data array present if the FITS file.
*     NONSDA = LOGICAL (Given)
*        If true the data array is not standard, i.e. in group format.
*        It is ignored if %DARRAY is false.
*     GCOUNT = INTEGER (Given)
*        The number of groups in the file.
*     PCOUNT = INTEGER (Given)
*        The number of parameters per group in the file.
*     MXPARM = INTEGER (Given)
*        The maximum number of group parameters, and the dimension size
*        for the various group arrays.
*     PTYPE( MXPARM ) = CHARACTER * ( * ) (Given)
*        The type (descriptive name) of each group parameter.
*     PSCALE( MXPARM ) = DOUBLE PRECISION (Given)
*        The scale factors of the group parameters so that the
*        parameters may be converted to the true floating-point values.
*     PZERO( MXPARM ) = DOUBLE PRECISION (Given)
*        The offsets of the group parameters so that the parameters may
*        be converted to the true floating-point values.
*     FILROO = CHARACTER * ( * ) (Given)
*        The rootname of the output NDF.  The suffix Gn, where n=%NG, is
*        appended in group-format mode to generate the filename.
*        Otherwise in automatic mode the NDF filename is the rootname.
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
*     GEXTND = LOGICAL (Given)
*        If true there may be extensions in the FITS sub-file.
*     NCARD = INTEGER (Given)
*        The number of header 80-character cards in the header.  Note
*        the size of the structure will not do because it will normally
*        have unfilled elements at the end, because of the way the
*        work space is obtained in quanta.  It should be the sum of the
*        headers and the group parameters.
*     SCARD = INTEGER (Given)
*        The number of the card from where the searches of the header
*        will begin.  This is needed because the headers make contain a
*        dummy header prior to an extension.
*     NENCOD = INTEGER (Given)
*        The number of AST encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The user's preferred AST encodings.  If NENCOD is zero, then
*        this is ignored, and an intelligent guess is made as to which
*        encoding to use.  The encoding determines which FITS headers
*        are used to create the NDF WCS component.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the tape/disk buffer.
*     ACTSIZ = INTEGER (Given and Returned)
*        The actual block size (a multiple of the FITS record length of
*        2880 bytes).  It is only an input argument for
*        %MEDIUM = 'DISK'.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted.
*     CURREC = LOGICAL (Given and Returned)
*        If true the current FITS record is to be used immediately, i.e.
*        it has alrady been read from tape or disk into %RECORD.
*     NEXT = LOGICAL (Returned)
*        This qualifies the status.  If true it instructs the calling
*        routine to go to the next FITS sub-file, and if status is bad
*        the calling routine should flush the error messages.
*     PARAMS( MXPARM * BPV ) = BYTE (Returned)
*        Numerical values of parameters associated with a group-format
*        array.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:

*  Copyright:
*     Copyright (C) 1990, 1991, 1992, 1993 Science & Engineering
*                   Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research
*                   Councils.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 November 26 (MJC):
*        Original version.
*     1990 November 30 (MJC):
*        Added MEDIUM, CURREC and RECORD arguments; disk-file access
*        with no automatic mode; call revised subroutines for filling
*        the data arrays.
*     1991 July 10 (MJC):
*        Enabled IEEE 64-bit.  Added the byte reversal flag to calls
*        to KPS1_RDATA and KPS1_RGRDA, and set them to .NOT. IEEE.
*     1992 December (RDS):
*        Now pass in the pointers to BUFFER and RECORD rather than the
*        character arrays. Arguments reordered
*     1993 January 5 (MJC):
*        Added the VMS argument to indicate whether or not a VAX/VMS
*        system was calling the routine.  Do not byte-swap for a non-VAX
*        architecture.
*     1993 January 20 (MJC):
*        Added the LENDIA argument to indicate whether or not the
*        application is being run on a little-endian operating system,
*        where byte-swapping is required.
*     9-JUN-1998 (DSB):
*        Added support for NDF WCS component.
*     8-OCT-1998 (DSB):
*        KPG1_CHVAx changed to FTS1_CHVAx so that they can be in
*         libfits.a.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'PRM_PAR'        ! Bad-pixel definitions
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Arguments Given:
      INTEGER
     :  BLANK,
     :  BLKSIZ,
     :  BPV,
     :  FD,
     :  GCOUNT,
     :  MD,
     :  MXPARM,
     :  NCARD,
     :  NDIM,
     :  DIMS( NDIM ),
     :  PCOUNT,
     :  SCARD,
     :  NENCOD,
     :  SIZE,
     :  SUBFIL

      INTEGER
     :  BFPNTR,
     :  RCPNTR

      LOGICAL
     :  AUTO,
     :  BADPIX,
     :  DARRAY,
     :  LENDIA,
     :  FMTCNV,
     :  GEXTND,
     :  LOGHDR,
     :  IEEE,
     :  NONSDA,
     :  VMS

      CHARACTER * ( * )
     :  CFN,
     :  FILROO,
     :  FMTIN,
     :  FMTOUT,
     :  MEDIUM,
     :  PNNDF,
     :  HEADER( NCARD ) * 80,
     :  PTYPE( MXPARM ),
     :  ENCODS( NENCOD )*(*)

      REAL
     :  BSCALE,
     :  BZERO

      DOUBLE PRECISION
     :  PSCALE( MXPARM ),
     :  PZERO( MXPARM )

*  Arguments Given and Returned:
      INTEGER
     :  ACTSIZ,                ! The actual blocksizes on the FITS tape
                               ! or disk
     :  OFFSET                 ! The number of bytes of the input
                               ! block that must be skipped. Equal to
                               ! ACTSIZ means read a new block.

      LOGICAL                  ! True if:
     :  CURREC                 ! The current FITS record is to be used
                               ! immediately

*  Arguments Returned:
      LOGICAL                  ! True if:
     :  NEXT                   ! Instructs calling routine to go on to
                               ! the next FITS sub-file even if status
                               ! is bad otherwise abort the application

      BYTE
     :  PARAMS( MXPARM * BPV ) ! Numerical values of parameters
                               ! associated with a group-format array

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                ! Number characters excluding trailing
                               ! blanks

      BYTE
     :  VAL_ITOUB              ! Integer-to-byte conversion
      EXTERNAL VAL_ITOUB

      INTEGER*2
     :  VAL_ITOW               ! Integer-to-word conversion
      EXTERNAL VAL_ITOW

*  Local Constants:
      INTEGER RECLEN           ! FITS record length
      PARAMETER ( RECLEN = 2880 )

*  Local Variables:
      INTEGER
     :  BSWAP,                 ! Logical for byte swapping in IEEE-to-
                               ! Vax conversions
     :  DAPNTR( 1 ),           ! Pointer to data array
     :  EL,                    ! Number of elements in the data array
     :  IERR,                  ! Location of first conversion error
     :  N,                     ! Sub-file loop counter
     :  NBAD,                  ! Number of bad conversions found by
                               ! VEC routine
     :  NCF,                   ! Number of characters in file name or
                               ! number
     :  NDF,                   ! NDF identifier
     :  PNTR( 1 ),             ! Pointer to array used by data-reading
                               ! subroutines
     :  RDISP,                 ! The displacement within the current
                               ! FITS record
     :  WKPNTR( 1 ),           ! Pointer to work array
     :  WSWAP                  ! Logical for word swapping in IEEE-to-
                               ! Vax conversions

      CHARACTER * (DAT__SZLOC) ! Locators for:
     :  WKLOC                  !   workspace for data conversion

      CHARACTER * 80
     :  DUMMY,                 ! Buffer for output of labels to logfile
     :  FILNAM,                ! Name of HDS container file for groups
                               ! format
     :  FMT1 * 62,             ! FORMAT specification
     :  LMED                   ! Lower case version of the medium

      LOGICAL                  ! True if:
     :  ASSOC,                 ! The NDF filename is not generated
                               ! automatically
     :  SWAPBY,                ! Bytes are to be swapped during the
                               ! reading of the data array or arrays
     :  VALID                  ! The named locator or NDF identifier is
                               ! valid
*.


*    Check for an error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Go to the next sub-file on exit?

      NEXT = .FALSE.

*    Make a lowercase version of the medium for use in messages.

      LMED = MEDIUM
      CALL CHR_LCASE( LMED )

*    Get the length of the filename.

      NCF = CHR_LEN( CFN )

*    Initialise the record-displacement pointer.
*    ===========================================

*    The FITS data must start in a new record.  Now this record may
*    already be the current FITS record; in this case the record should
*    be procesed from its start, hence a zero displacement.  The flag
*    can be switched off as this positioninig information is imparted
*    to the record displacement.  If the current record is not the
*    first FITS record containing the data, set the the displacement
*    to the end of the FITS record.  This will cause a new record be
*    read.  For simple FITS the displacement will be either 0 or 2880.
*    For group-format data the same will apply only to the first group,
*    thereafter the group parameters and data are abutted, and ignore
*    FITS record boundaries.  Therefore the displacement will be
*    arbitrary between 0 and 2880.

      IF ( CURREC ) THEN
         RDISP = 0
         CURREC = .FALSE.
      ELSE
         RDISP = RECLEN
      END IF

*
*    Loop for each data array.
*    =========================

      DO N = 1, GCOUNT

*       Start a new error context.

         CALL ERR_MARK

*       Create the output NDF.

         CALL FTS1_CRNDF( PNNDF, FILROO, GCOUNT, N, AUTO, FMTOUT, NDIM,
     :                    DIMS, NDF, FILNAM, ASSOC, STATUS )

*       Check for option not to create file.

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( 'NOOUT',
     :        'Null parameter -- no output file created', STATUS )

*          If there may be extensions following the data in this
*          FITS sub-file to be processed, so the data in the current
*          sub-file must be skipped over.  Offset on exit is at the
*          end of a record.

            IF ( GEXTND ) THEN
               CALL FTS1_SKIP( MEDIUM, MD, SIZE, BPV, GCOUNT, PCOUNT,
     :                         BLKSIZ, ACTSIZ,
     :                         %VAL( CNF_PVAL( BFPNTR ) ), OFFSET,
     :                         %VAL( CNF_PVAL( RCPNTR ) ),
     :                         RDISP, STATUS )
            END IF

*          Ask calling application to go on to the next sub-file or
*          file.

            NEXT = .TRUE.
            CALL ERR_RLSE
            GO TO 999

*       Any errors creating the NDF?

         ELSE IF ( STATUS .NE. SAI__OK ) THEN

*          Report errors and abort.

            IF ( STATUS .NE. PAR__ABORT ) THEN

               CALL MSG_SETC ( 'FILNAM', FILNAM )
               CALL ERR_REP( 'FTS1_NDF_NOFILE',
     :           'FITSIN: No file ^FILNAM created.', STATUS )
            END IF
            CALL ERR_RLSE
            GOTO 999
         END IF

*       Release the new error context.

         CALL ERR_RLSE


         IF ( N .EQ. 1 .AND. LOGHDR ) THEN

*          First sub-file and logging.  Therefore write the output
*          filename to the log file so that descriptors and file
*          can be matched later.

            CALL FIO_WRITE( FD, ' ', STATUS )
            DUMMY = 'The above descriptors and their '/
     :              /'associated data array are stored in:'
            CALL FIO_WRITE( FD, DUMMY, STATUS )
            CALL FIO_WRITE( FD, FILNAM, STATUS )
            CALL FIO_WRITE( FD, ' ', STATUS )

*          Abort and tidy if something has gone wrong writing to the
*          log file.

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'FTS1_NDF_FILNAM',
     :           'FITSIN: Unable to obtain name of '/
     :           /'HDS output NDF.', STATUS )
               GOTO 999
            END IF
         END IF

         IF ( DARRAY ) THEN

*          If there is a format conversion to be made or BLANK-flagged
*          data are to be converted to the standard magic values, there
*          must a work array of the same dimensions as the data array
*          because the conversions are not done it situ.

            IF ( FMTCNV .OR. BADPIX ) THEN

*             Create a work array for the input data.  For bad-data
*             conversions where there is no format conversion the input
*             format equals the output so the same type may be used as
*             for format conversion.  Map the workspace.

               CALL AIF_GETVM( FMTIN, NDIM, DIMS, WKPNTR, WKLOC,
     :                         STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN

                  CALL ERR_REP( 'FTS1_NDF_WSP',
     :              'FITSIN: Unable to get workspace '/
     :              /'for data conversion', STATUS )
                  GOTO 999
               END IF
            END IF

*          Map the data array.

            CALL NDF_MAP( NDF, 'Data', FMTOUT, 'WRITE', DAPNTR, EL,
     :                    STATUS )

*          Clear up after an error.

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'FTS1_NDF_NOMPO',
     :           'FITSIN: Error in mapping NDF', STATUS )
               GOTO 999
            END IF

*          Select pointer to where the data are to be stored.

            IF ( FMTCNV .OR. BADPIX ) THEN
               PNTR( 1 ) = WKPNTR( 1 )
            ELSE
               PNTR( 1 ) = DAPNTR( 1 )
            END IF

*          Determine whether or not bytes are to be swapped during the
*          reading of the data array or arrays.
            SWAPBY = ( .NOT. IEEE .AND. VMS ) .OR. LENDIA

*          Group format
*          ============

            IF ( NONSDA ) THEN

*             Read the data into the data array, and read the
*             parameters.  Only byte swap on VMS integer data.

               CALL FTS1_RGRDA( MEDIUM, MD, SIZE, BPV, SWAPBY, PCOUNT,
     :                          BLKSIZ, ACTSIZ,
     :                          %VAL( CNF_PVAL( BFPNTR ) ), OFFSET,
     :                          %VAL( CNF_PVAL( RCPNTR ) ),
     :                          RDISP, PARAMS,
     :                          %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*             Tidy up after an error.

               IF ( STATUS .NE. SAI__OK ) GOTO 999

*             Create a heading in the logfile for the current group.
*             The maximum width of the logfile is 132 characters.
*             Subtracting the rest gives 94.

               IF ( LOGHDR ) THEN
                  DUMMY = ' '
                  CALL FIO_WRITE( FD, DUMMY, STATUS )
                  FMT1 = '(''** File: '', A,''('', I3, '' );  Group'','/
     :                   /'I6, '' parameters'')'
                  WRITE( DUMMY, FMT1 ) CFN( MAX(1, NCF-94):NCF ),
     :                                 SUBFIL, N
                  CALL FIO_WRITE( FD, DUMMY, STATUS )
               END IF

*             Store the group parameters with the header information.
*             Allow for undefined parameters via data blank.

               CALL FTS1_GPARM( NCARD-PCOUNT, HEADER, PCOUNT, PARAMS,
     :                          PTYPE, PSCALE, PZERO, BADPIX, BLANK,
     :                          LOGHDR, FD, STATUS )

               IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Simple format
*          =============

            ELSE

*             Read the data into the data array.  Only byte swap on VMS
*             integer data.

               CALL FTS1_RDATA( MEDIUM, MD, SIZE, BPV, SWAPBY, BLKSIZ,
     :                          ACTSIZ, %VAL( CNF_PVAL( BFPNTR ) ),
     :                          OFFSET,
     :                          %VAL( CNF_PVAL( RCPNTR ) ), RDISP,
     :                          %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*             Tidy up after an error.

               IF ( STATUS .NE. SAI__OK ) GOTO 999

*          ^^^^^^^^^^^^^

            END IF

*          Perform the format conversion.
*          ==============================

            IF ( FMTCNV ) THEN

*             Now the work array must be converted to floating point.
*             Call the appropriate routine depending on the data type of
*             the FITS array.

               IF ( FMTIN .EQ. '_INTEGER' ) THEN
                  CALL VEC_ITOR( .FALSE., SIZE,
     :                           %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                           %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                           IERR, NBAD,
     :                           STATUS )

               ELSE IF ( FMTIN .EQ. '_WORD' ) THEN
                  CALL VEC_WTOR( .FALSE., SIZE,
     :                           %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                           %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                           IERR, NBAD,
     :                           STATUS )

               ELSE IF ( FMTIN .EQ. '_UBYTE' ) THEN
                  CALL VEC_UBTOR( .FALSE., SIZE,
     :                            %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                           %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                           IERR, NBAD,
     :                           STATUS )
               END IF

*             Tidy the workspace.

               CALL AIF_ANTMP( WKLOC, STATUS )

*             Apply scale and zero, and correct undefined (data-blank)
*             pixels to the standard bad value.

               CALL FTS1_SCOFB( BSCALE, BZERO, BADPIX, BLANK, SIZE,
     :                          %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                          STATUS )

*          Process data blank.
*          ===================

            ELSE IF ( BADPIX ) THEN

*             Modify flagged data to the standard bad values.  Call the
*             appropriate routine to process the data type in the FITS
*             array.  By definition the BLANK card, which defines
*             whether or not there is bad data value, should be ignored
*             for floating-point data, as this information is stored in
*             the IEEE numbers.

               IF ( BPV .EQ. 1 ) THEN
                  CALL FTS1_CHVAUB( EL, %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                              VAL_ITOUB( .FALSE., BLANK, STATUS ),
     :                              VAL__BADUB,
     :                              %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                              NBAD, STATUS )
               ELSE IF ( BPV .EQ. 2 ) THEN
                  CALL FTS1_CHVAW( EL, %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                             VAL_ITOW( .FALSE., BLANK, STATUS ),
     :                             VAL__BADW,
     :                             %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                             NBAD, STATUS )
               ELSE IF ( BPV .EQ. 4 ) THEN
                  CALL FTS1_CHVAI( EL, %VAL( CNF_PVAL( WKPNTR( 1 ) ) ),
     :                             BLANK,
     :                             VAL__BADI,
     :                             %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                             NBAD, STATUS )
               END IF

*             Tidy the workspace.

               CALL AIF_ANTMP( WKLOC, STATUS )

            END IF

*       Make dummy data array defined.
*       ==============================
*
*       The data array must been defined state, so in the case where the
*       FITS files has no data array, only headers, need to put in some
*       bad-pixel values.

         ELSE

*          Map the data array, filling the array with bad values.

            CALL NDF_MAP( NDF, 'Data', FMTOUT, 'WRITE/BAD', DAPNTR, EL,
     :                    STATUS )

*          Clear up after an error.

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'FTS1_NDF_NOMPO',
     :           'FITSIN: Error in mapping NDF', STATUS )
               GOTO 999
            END IF

         END IF

*       IEEE to VAX conversion.
*       =======================

         IF ( IEEE .AND. VMS .AND. DARRAY ) THEN

*          Byte swapping has not already been performed for VMS.
*          Note the use of variables and not expressions
*          because the R conversion routine is written in C.

            BSWAP = 1
            WSWAP = 0

*          The conversions are made in situ.

*          32-bit reals.

            IF ( BPV .EQ. 4 ) THEN
               CALL FTS1_I2VXR( BSWAP, WSWAP, EL,
     :                          %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                          STATUS )

*          64-bit double precision.  Here word swap also swaps
*          longwords, so in effect all the bytes are reversed.

            ELSE IF ( BPV .EQ. 8 ) THEN
               CALL FTS1_I2VXD( .TRUE., EL,
     :                          %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                          STATUS )
            END IF


*       Replace IEEE NaNs with bad values.
*       ==================================

         ELSE IF ( IEEE .AND. DARRAY ) THEN

*          The conversions are made in situ.

*          32-bit reals.

            IF ( BPV .EQ. 4 ) THEN
               CALL FTS1_RNANR( EL, %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                          STATUS )

*          64-bit reals.

            ELSE IF ( BPV .EQ. 8 ) THEN
               CALL FTS1_RNAND( EL, %VAL( CNF_PVAL( DAPNTR( 1 ) ) ),
     :                          STATUS )

            END IF

         END IF

*       ^^^^^^^^^^^^^^^^^^^^^^^

*       Unmap the data array ready for the next file.

         CALL NDF_UNMAP( NDF, 'Data', STATUS )

*       Complete the NDF by creating and assigning values to the other
*       top-level components, the axis structure and the FITS extension.
*       The first argumentis the number of header cards from the start
*       of the headers up to the end of the current header which now
*       contains the group parameters.

         CALL FTS1_NDFCM( SCARD - 1 + NCARD, HEADER, SCARD, NDF,
     :                    NENCOD, ENCODS, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'FTS1_NDF_COPHDR',
     :        'Error creating other NDF components from the FITS '/
     :        /'header.', STATUS )
            GOTO 999
         END IF

*       Tidy the NDF.

         CALL NDF_ANNUL( NDF, STATUS )

*       Just in case, because there is a contextual error message to
*       follow.

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Need to find the prefix for NDFs to store group-format data
*       arrays.  This need only be done once and not at all if the
*       application is in automatic mode.

         IF ( ASSOC .AND. GCOUNT .GT. 1 ) THEN

*          Obtain the file name associated with the parameter OUTPUT,
*          i.e. the parameter to be used for the sequence of NDFs.

            CALL AIF_FLNAM( PNNDF, FILROO, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'FTS1_NDF_FILROO',
     :           'FITSIN: Unable to obtain name of root file for '/
     :           /'series of NDFs to hold group data', STATUS )
               GOTO 999
            END IF
         END IF

*       Keep the user posted about the current state of the NDFs
*       created.
*       =========================================================

         CALL MSG_SETC( 'CFN', CFN )
         CALL MSG_SETC( 'MED', LMED )

         IF ( GCOUNT .GT. 1 ) THEN

*          First for group-format.

            CALL MSG_SETI( 'GRN', N )

*          Filename is found from the parameter if it was
*          obtained by association...

            IF ( ASSOC ) THEN
               CALL MSG_SETC( 'FILNAM', FILROO )
            ELSE

*             or from the generated name if there is one.

               CALL MSG_SETC( 'FILNAM', FILNAM )
            END IF

            CALL MSG_OUT( 'INFUSER',
     :        'Completed processing of ^MED file '/
     :        /'^CFN group ^GRN to ^FILNAM', STATUS )

*       Now for the automatic mode, but no groups, and
*       no-groups and no automatic mode.

         ELSE
            CALL MSG_SETC( 'FILNAM', FILNAM )
            CALL MSG_OUT( 'INFUSER',
     :        'Completed processing of ^MED file ^CFN to ^FILNAM.',
     :        STATUS )
         END IF

*       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*       Cancel the parameter association with the current NDF ready for
*       the next NDF in the loop.

         CALL DAT_CANCL( PNNDF, STATUS )

*    End of group-count loop.

      END DO

*    No error so go to the next sub-file on exit.

      NEXT = .TRUE.

*    Tidy up the workspace and the NDF.
*    ==================================

  999 CONTINUE

*    First the NDF identifier.

      CALL NDF_VALID( NDF, VALID, STATUS )
      IF ( VALID ) CALL NDF_ANNUL( NDF, STATUS )

*    Now the parameter assoicated with the NDF.

      CALL PAR_CANCL( PNNDF, STATUS )

*    Tidy the workspace used to convert the data array if the workspace
*    exists.

      IF ( FMTCNV .OR. BADPIX ) THEN

         CALL DAT_VALID( WKLOC, VALID, STATUS )
         IF ( VALID ) CALL AIF_ANTMP( WKLOC, STATUS )
      END IF

      END
