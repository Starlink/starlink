      SUBROUTINE ADI2_FCOMIT( FID, STATUS )
*+
*  Name:
*     ADI2_FCOMIT

*  Purpose:
*     Commit buffer changes to a FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT( FID, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*8		IDXSTR			! Index string
      CHARACTER*6		MODE			! File access mode
      CHARACTER*200		THISFILE		! Current file name

      INTEGER			BSIZE			! Block size
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HCID			! HDU container
      INTEGER			HDUID			! HDU identifier
      INTEGER			HDUTAB			! HDU table
      INTEGER			HDUTYPE			! FITSIO hdu type
      INTEGER			IHDU			! HDU loop variable
      INTEGER			ILUN			! Logical unit for i/p
      INTEGER			NHDU			! HDU count
      INTEGER			OIHDU			! O/p HDU counter
      INTEGER			OLUN			! Logical unit for o/p
      INTEGER			TLEN			! Length of THISFILE

      LOGICAL			SCANNEDALL		! Scanned all HDUs?
      LOGICAL			WRITE			! Write access?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Choose the file identifier for output. In write mode chose existing
*  opened file, but in update mode create new file
      CALL ADI_CGET0C( FID, 'MODE', MODE, STATUS )
      WRITE = ((MODE(1:1).EQ.'W') .OR. (MODE(1:1).EQ.'w'))
      IF ( WRITE ) THEN
        CALL ADI2_GETLUN( FID, OLUN, STATUS )

      ELSE

*    Extract input logical unit
        CALL ADI2_GETLUN( FID, ILUN, STATUS )

*    Create temporary file name
        INQUIRE( UNIT=ILUN, NAME=THISFILE, IOSTAT=FSTAT )
        TLEN = CHR_LEN(THISFILE)
        THISFILE(TLEN+1:) = '.tmp'

*    Get logical unit
        CALL FIO_GUNIT( OLUN, STATUS )

*    Try to open file
        FSTAT = 0
        CALL FTINIT( OLUN, THISFILE(:TLEN+4), 1, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Failed to create temporary file - '/
     :                /'FITS data not written to output', STATUS )
          GOTO 99
        END IF

      END IF

*  Locate the HDU container
      CALL ADI_FIND( FID, 'Hdus', HCID, STATUS )
      CALL ADI_FIND( HCID, 'HduTable', HDUTAB, STATUS )

*  Loop over HDU store, writing out stuff
      CALL ADI_CGET0I( HCID, 'HduCount', NHDU, STATUS )
      OIHDU = 0
      DO IHDU = 1, NHDU

*    Make the HDU index entry
        IDXSTR = ADI2_MKIDX( 'Obj_', IHDU )

*    Index the IHDU'th HDU
        CALL ADI_FIND( HDUTAB, IDXSTR, HDUID, STATUS )

*    Write the HDU
        CALL ADI2_FCOMIT_HDU( HDUID, WRITE, ILUN, IHDU, OLUN, OIHDU,
     :                        STATUS )

*    Release the hdu
        CALL ADI_ERASE( HDUID, STATUS )

      END DO

*  Update mode?
      IF ( .NOT. WRITE ) THEN

*    Scanned all the HDU's
        CALL ADI_CGET0L( HCID, 'ScannedAll', SCANNEDALL, STATUS )
        IF ( .NOT. SCANNEDALL ) THEN

*      While more HDUs
          CALL ADI_CGET0I( HCID, 'MaxScan', IHDU, STATUS )
          FSTAT = 0
          DO WHILE ( FSTAT .EQ. 0 )

*        Move to next input HDU
            IHDU = IHDU + 1
            CALL FTMAHD( ILUN, IHDU, HDUTYPE, FSTAT )

*        Good HDU?
            IF ( FSTAT .EQ. 0 ) THEN

*          Create output HDU
              CALL FTCRHD( OLUN, FSTAT )
              OIHDU = OIHDU + 1
              CALL FTMAHD( OLUN, OIHDU, HDUTYPE, FSTAT )

*          Copy data from input
              CALL FTCOPY( ILUN, OLUN, 0, FSTAT )

            END IF

          END DO

        END IF

*    Close the temporary file and return its logical unit to the system
        FSTAT = 0
        CALL FTCLOS( OLUN, FSTAT )
        CALL FIO_PUNIT( OLUN, STATUS )

*    Close original file
        FSTAT = 0
        CALL FTCLOS( ILUN, FSTAT )

*    Rename the temporary file
        CALL UTIL_RENAME( THISFILE(:TLEN+4), THISFILE(:TLEN), STATUS )

*    Re-open the temp file so that it can be closed by the closure routine
        FSTAT = 0
        CALL FTOPEN( ILUN, THISFILE(:TLEN), 0, BSIZE, FSTAT )

      END IF

*  Release HDU container
      CALL ADI_ERASE( HDUTAB, STATUS )
      CALL ADI_ERASE( HCID, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCOMIT', STATUS )

      END



      SUBROUTINE ADI2_FCOMIT_HDU( HDUID, WRITE, ILUN, IHDU, OLUN,
     :                            OIHDU, STATUS )
*+
*  Name:
*     ADI2_FCOMIT_HDU

*  Purpose:
*     Commit buffer changes to a FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT_HDU( HDUID, WRITE, ILUN, IHDU, OLUN, OIHDU, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
*        Original version.
*     18 Dec 1996 (RB):
*        Add code to output data for bintable extensions too.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			HDUID, ILUN, IHDU, OLUN, OIHDU
      LOGICAL			WRITE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*1		AXC			! Axis # in char
      CHARACTER*72		CMNT			! Keyword comment
      CHARACTER*8		TYPE			! Basic data type

      INTEGER			BITPIX			! Bits per pixel
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HDUTYPE			! FITSIO hdu type
      INTEGER			IAX			! Loop over axes
      INTEGER			IMID			! Image cache object
      INTEGER			IPTR			! Image data
      INTEGER			NDIG			! # digits
      INTEGER			NDIM,DIMS(ADI__MXDIM)	! Data shape
      INTEGER			NELM			! Total # elements
      INTEGER			PCOUNT, GCOUNT		! Group counters
      INTEGER			IREAD

      LOGICAL			EXTEND			! Dataset has extensions?
      LOGICAL			ISTABLE			! Table HDU?
      LOGICAL			MODIFIED		! HDU is modified?
      LOGICAL			MRKDEL			! Object to be deleted?
      LOGICAL			SIMPLE			! HDU is standard?
      LOGICAL			THERE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  HDU isn't marked for delete?
      CALL ADI2_ISDEL( HDUID, MRKDEL, STATUS )
      IF ( .NOT. MRKDEL ) THEN

*    Increment output HDU counter
        OIHDU = OIHDU + 1

*    Move to this hdu
        FSTAT = 0
        IF ( IHDU .GT. 1 ) THEN
          CALL FTCRHD( OLUN, FSTAT )
        END IF
        CALL FTMAHD( OLUN, OIHDU, HDUTYPE, FSTAT )

*    Modified?
        CALL ADI_CGET0L( HDUID, 'Modified', MODIFIED, STATUS )

*    Simple copy of input?
        IF ( .NOT. WRITE .AND. .NOT. MODIFIED ) THEN
          CALL FTMAHD( ILUN, IHDU, HDUTYPE, FSTAT )
          CALL FTCOPY( ILUN, OLUN, 0, FSTAT )

*    Full write required
        ELSE

*      Table or image?
          CALL ADI_CGET0L( HDUID, 'IsTable', ISTABLE, STATUS )
          IF ( ISTABLE ) THEN

*        Shall we add some code here too? - rb
            CALL FTPKYS( OLUN, 'XTENSION', 'BINTABLE',
     :                   'binary table extension', FSTAT )
            CALL ADI2_IMGTSHP( HDUID, .TRUE., BITPIX, NDIM, DIMS,
     :                   STATUS )
            CALL FTPKYJ( OLUN, 'BITPIX', BITPIX, 'bits per pixel',
     :                   FSTAT )
            CALL FTPKYJ( OLUN, 'NAXIS', NDIM,
     :                   '2-dimensional binary table', FSTAT)
            CALL FTPKYJ( OLUN, 'NAXIS1', DIMS(1),
     :                   'width of table in bytes', FSTAT)
            CALL FTPKYJ( OLUN, 'NAXIS2', DIMS(2),
     :                   'number of rows in table', FSTAT)
c           CALL FTPKYJ( OLUN, 'PCOUNT', 0,
c    :                   'size of special data  area', FSTAT)
c           CALL FTPKYJ( OLUN, 'GCOUNT', 1,
c    :                   'one data group (required keyword)', FSTAT)

*        Define the data size
            CALL FTPDEF( OLUN, BITPIX, NDIM, DIMS, PCOUNT, GCOUNT,
     :                   FSTAT )

*        Write remaining header cards
            CALL ADI2_FCOMIT_CARDS( OLUN, HDUID, NDIM + 1, STATUS )

*      Image HDU
          ELSE

*        Gather keywords for image extension
            CALL ADI2_IMGTSHP( HDUID, .TRUE., BITPIX, NDIM, DIMS,
     :                         STATUS )

*        Simple HDU?
            CALL ADI2_HGKYL( HDUID, '>SIMPLE', SIMPLE, CMNT, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              SIMPLE = .TRUE.
            END IF

*        Get grouping values
            CALL ADI2_HGKYI( HDUID, '>PCOUNT', PCOUNT, CMNT, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              PCOUNT = 0
              CALL ERR_ANNUL( STATUS )
            END IF
            CALL ADI2_HGKYI( HDUID, '>GCOUNT', GCOUNT, CMNT, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              GCOUNT = 1
              CALL ERR_ANNUL( STATUS )
            END IF

*        Define the extension
            FSTAT = 0
            IF ( IHDU .EQ. 1 ) THEN
              IF ( SIMPLE ) THEN
                CMNT = 'file does conform to FITS standard'
              ELSE
                CMNT = 'file does not conform to FITS standard'
              END IF
              CALL FTPKYL( OLUN, 'SIMPLE', SIMPLE, CMNT, FSTAT )
            ELSE
              CALL FTPKYS( OLUN, 'XTENSION', 'IMAGE',
     :                     'IMAGE extension', FSTAT )
            END IF

*        Write BITPIX
            CALL FTPKYJ( OLUN, 'BITPIX', BITPIX, 'bits per pixel',
     :                   FSTAT )

*        Write shape info
            CALL FTPKYJ( OLUN, 'NAXIS', NDIM, 'number of data axes',
     :                   FSTAT )
            DO IAX = 1, NDIM
              CALL CHR_ITOC( IAX, AXC, NDIG )
              CALL FTPKYJ( OLUN, 'NAXIS'//AXC, DIMS(IAX),
     :                     'length of data axis '//AXC, FSTAT )
            END DO

*        Write other stuff
            IF ( IHDU .EQ. 1 ) THEN

*          Extensions allowed?
              CALL ADI2_HGKYL( HDUID, '>EXTEND', EXTEND, CMNT, STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL ERR_ANNUL( STATUS )
                EXTEND = .TRUE.
              END IF
              IF ( EXTEND ) THEN
                CALL FTPKYL( OLUN, 'EXTEND', EXTEND,
     :                   'FITS dataset may contain extensions', FSTAT )
              END IF

*          Write the PCOUNT and GCOUNT values if nonstandard
              IF ( (PCOUNT .GT. 0) .OR. (GCOUNT .GT. 1) ) THEN
                CMNT = 'random group records are present'
                CALL FTPKYL( OLUN, 'GROUPS', .TRUE., CMNT, FSTAT )
                CMNT = 'number of random group parameters'
                CALL FTPKYJ( OLUN, 'PCOUNT', PCOUNT, CMNT, FSTAT )
                CMNT = 'number of random groups'
                CALL FTPKYJ( OLUN, 'GCOUNT', GCOUNT, CMNT, FSTAT )
              END IF

*        2nd or subsequent HDU
            ELSE
              CMNT = 'number of random group parameters'
              CALL FTPKYJ( OLUN, 'PCOUNT', PCOUNT, CMNT, FSTAT )
              CMNT = 'number of random groups'
              CALL FTPKYJ( OLUN, 'GCOUNT', GCOUNT, CMNT, FSTAT )

            END IF

*        Define the data size
            CALL FTPDEF( OLUN, BITPIX, NDIM, DIMS, PCOUNT, GCOUNT,
     :                   FSTAT )

*        Write remaining header cards
            CALL ADI2_FCOMIT_CARDS( OLUN, HDUID, NDIM + 1, STATUS )

*        Non-zero extension size
            IF ( NDIM .GT. 0 ) THEN

*          Write the data
              CALL ADI_FIND( HDUID, 'Image', IMID, STATUS )

*          Has the data been modified?
              CALL ADI_CGET0L( IMID, 'Modified', MODIFIED, STATUS )
              IF ( .NOT. MODIFIED .AND. .NOT. WRITE ) THEN

*            Move to the input HDU
                CALL FTMAHD( ILUN, IHDU, HDUTYPE, FSTAT )

*            Copy the data
                CALL FTCPDT( ILUN, OLUN, FSTAT )

*          Write new data
              ELSE

*            Map the image value in its basic type (any way we can! RB)
                CALL ADI_THERE( IMID, 'Value', THERE, STATUS )
                IF ( THERE ) THEN
                  CALL ADI_CTYPE( IMID, 'Value', TYPE, STATUS )
                  CALL ADI_CMAP( IMID, 'Value', TYPE, 'READ', IPTR,
     :                           STATUS )
                ELSE
                  CALL ADI_CTYPE( HDUID, 'Image', TYPE, STATUS )
                  CALL ADI_CMAP( HDUID, 'Image', TYPE, 'READ', IPTR,
     :                           STATUS )
c                 CALL ADI_NEW1( TYPE, DIMS(1), IPTR, STATUS )
c                 CALL ADI_MAP( IMID, TYPE, 'READ', IPTR, STATUS )
c                 CALL ADI_GET1( IMID, TYPE, DIMS(1), %VAL(IPTR),
c    :                           IREAD, STATUS )
                END IF

                IF ( STATUS .NE. SAI__OK ) GOTO 99

*            Total number of elements
                CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*            Copy data to FITS
                IF ( TYPE .EQ. 'DOUBLE' ) THEN
                  CALL FTPPRD( OLUN, 1, 1, NELM, %VAL(IPTR), FSTAT )
                ELSE IF ( TYPE .EQ. 'REAL' ) THEN
                  CALL FTPPRE( OLUN, 1, 1, NELM, %VAL(IPTR), FSTAT )
                ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
                  CALL FTPPRJ( OLUN, 1, 1, NELM, %VAL(IPTR), FSTAT )
                ELSE IF ( TYPE .EQ. 'WORD' ) THEN
                  CALL FTPPRI( OLUN, 1, 1, NELM, %VAL(IPTR), FSTAT )
                ELSE IF ( TYPE .EQ. 'BYTE' ) THEN
                  CALL FTPPRB( OLUN, 1, 1, NELM, %VAL(IPTR), FSTAT )
                END IF

*            Unmap data
c               CALL ADI_CUNMAP( IMID, 'Value', IPTR, STATUS )

              END IF

*          Release the data
              CALL ADI_ERASE( IMID, STATUS )

*        End of switch on image/table hdu
            END IF

*      End of switch on write/update mode
          END IF

*    End of modified test
        END IF

*  End of marked for delete test
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_FCOMIT_HDU', STATUS )
      END IF

      END



      SUBROUTINE ADI2_FCOMIT_CARDS( LUN, HDUID, NDONE, STATUS )
*+
*  Name:
*     ADI2_FCOMIT_CARDS

*  Purpose:
*     Commit HDU cards to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT_CARDS( LUN, HDUID, NDONE, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			LUN, HDUID, NDONE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*72		CMNT			! Comment string
      CHARACTER*80		CVAL			! Keyword value
      CHARACTER*8		IDXSTR			! Index string
      CHARACTER*8		KEYWRD			! Keyword name
      CHARACTER*20		TYPE			! Basic data type
      CHARACTER*8		VTYPE			! Keyword value type

      DOUBLE PRECISION		DVAL			! Keyword value

      REAL			RVAL			! Keyword value

      INTEGER			CRDID			! Card cache object
      INTEGER			CRDTAB			! HDU table
      INTEGER			FSTAT			! FITSIO status
      INTEGER			ICARD			! Loop over cards
      INTEGER			IVAL			! Keyword value
      INTEGER			NCARD			! # cards in HDU
      INTEGER			NDEC			! # decimals used
      INTEGER			VID			! Value object

      LOGICAL			LVAL			! Keyword value
      LOGICAL			MRKDEL			! Object to be deleted?
      LOGICAL			SFMT			! Scientific notation?
      LOGICAL			THERE			! Object exists?
      LOGICAL			WRITTEN			! Already written?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  How many cards in HDU
      CALL ADI_CGET0I( HDUID, 'CrdCount', NCARD, STATUS )

*  Allocate more space?
      IF ( NCARD .GT. NDONE ) THEN
        FSTAT = 0
        CALL FTHDEF( LUN, NCARD - NDONE, FSTAT )
      END IF

*  Locate card table
      CALL ADI_FIND( HDUID, 'CrdTable', CRDTAB, STATUS )

*  Loop over cards
      DO ICARD = 1, NCARD

*    Locate the ICARD'th card
        IDXSTR = ADI2_MKIDX( 'Obj_', ICARD )
        CALL ADI_FIND( CRDTAB, IDXSTR, CRDID, STATUS )

*    Card isn't already written?
        CALL ADI_CGET0L( CRDID, 'Written', WRITTEN, STATUS )
        IF ( .NOT. WRITTEN ) THEN

*      Marked for delete?
          CALL ADI2_ISDEL( CRDID, MRKDEL, STATUS )
          IF ( .NOT. MRKDEL ) THEN

*        What is the card object?
            CALL ADI_TYPE( CRDID, TYPE, STATUS )
            IF ( TYPE .EQ. 'FITSkeyCache' ) THEN

*          Get keyword name
              CALL ADI_CGET0C( CRDID, 'Name', KEYWRD, STATUS )

*          Locate keyword value
              CALL ADI_FIND( CRDID, 'Value', VID, STATUS )
              CALL ADI_TYPE( VID, VTYPE, STATUS )

*          Get keyword comment
              CALL ADI_THERE( CRDID, 'Comment', THERE, STATUS )
              IF ( THERE ) THEN
                CALL ADI_CGET0C( CRDID, 'Comment', CMNT, STATUS )
              ELSE
                CALL ADI2_STDCMT( KEYWRD, CMNT, STATUS )
              END IF

*          DOUBLE or REAL keyword
              FSTAT = 0
              IF ( (VTYPE .EQ. 'DOUBLE') .OR. (VTYPE.EQ.'REAL') ) THEN

*            Format control constants
                CALL ADI_THERE( VID, '.Scientific', THERE, STATUS )
                IF ( THERE ) THEN
                  CALL ADI_CGET0L( VID, '.Scientific', SFMT, STATUS )
                ELSE
                  SFMT = .FALSE.
                END IF
                CALL ADI_THERE( VID, '.Ndecimal', THERE, STATUS )
                IF ( THERE ) THEN
                  CALL ADI_CGET0I( VID, '.Ndecimal', NDEC, STATUS )
                ELSE IF ( VTYPE(1:1) .EQ. 'D' ) THEN
                  NDEC = 14
                ELSE
                  NDEC = 7
                END IF

*            Real value
                IF ( VTYPE(1:1) .EQ. 'R' ) THEN
                  CALL ADI_GET0R( VID, RVAL, STATUS )
                  IF ( SFMT ) THEN
                    CALL FTPKYE( LUN, KEYWRD, RVAL, NDEC, CMNT, FSTAT )
                  ELSE
                    CALL FTPKYF( LUN, KEYWRD, RVAL, NDEC, CMNT, FSTAT )
                  END IF

*            Double precision
                ELSE
                  CALL ADI_GET0D( VID, DVAL, STATUS )
                  IF ( SFMT ) THEN
                    CALL FTPKYD( LUN, KEYWRD, DVAL, NDEC, CMNT, FSTAT )
                  ELSE
                    CALL FTPKYG( LUN, KEYWRD, DVAL, NDEC, CMNT, FSTAT )
                  END IF

                END IF

*          CHAR keyword
              ELSE IF ( VTYPE .EQ. 'CHAR' ) THEN

*            Get and write value
                CALL ADI_GET0C( VID, CVAL, STATUS )
                CALL FTPKYS( LUN, KEYWRD, CVAL(:MAX(1,CHR_LEN(CVAL))),
     :                       CMNT, FSTAT )

*          LOGICAL keyword
              ELSE IF ( VTYPE .EQ. 'LOGICAL' ) THEN

*            Get and write value
                CALL ADI_GET0L( VID, LVAL, STATUS )
                CALL FTPKYL( LUN, KEYWRD, LVAL, CMNT, FSTAT )

*          Treat everything else as INTEGER
              ELSE

*            Get and write value
                CALL ADI_GET0I( VID, IVAL, STATUS )
                CALL FTPKYJ( LUN, KEYWRD, IVAL, CMNT, FSTAT )

              END IF

*          Release keyword value
              CALL ADI_ERASE( VID, STATUS )

*        FITS comment card
            ELSE IF ( TYPE .EQ. 'FITScommCache' ) THEN

*          Get comment value
              CALL ADI_CGET0C( CRDID, 'Value', CMNT, STATUS )

*          Write it out
              CALL FTPCOM( LUN, CMNT(:MAX(1,CHR_LEN(CMNT))), FSTAT )

*        FITS history card
            ELSE IF ( TYPE .EQ. 'FITShistCache' ) THEN

*          Get history value
              CALL ADI_CGET0C( CRDID, 'Value', CMNT, STATUS )

*          Write it out
              CALL FTPHIS( LUN, CMNT(:MAX(1,CHR_LEN(CMNT))), FSTAT )

*        FITS crap card
            ELSE IF ( TYPE .EQ. 'FITScrapCache' ) THEN

*          Get crap
              CALL ADI_CGET0C( CRDID, 'Value', CMNT, STATUS )

*          Write raw low level card (yeuch)
              CALL FTPREC( LUN, CMNT(:MAX(1,CHR_LEN(CMNT))), FSTAT )

            END IF

          END IF

        END IF

*    Release the card
        CALL ADI_ERASE( CRDID, STATUS )

      END DO

*  Release HDU container
      CALL ADI_ERASE( CRDTAB, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_FCOMIT_CARDS', STATUS )
      END IF

      END
