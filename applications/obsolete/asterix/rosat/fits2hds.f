      SUBROUTINE FITS2HDS( STATUS )
*+
*  Name:
*     FITS2HDS

*  Purpose:
*     Convert ascii or binary FITS data to HDS

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL FITS2HDS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Converts FITS files into HDS, usIng the William Pence FIO
*     library.  Based on the previous AFITS2HDS this program supports
*     all other major data types. If the ASTERIX parameter is specified
*     (default) image fits files are converted with the standard ASTERIX
*     header information appended.

*  Usage:
*     fits2hds {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

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
*     Determine file type.
*     Create an HDS file
*     Read FITS array names
*     Create correspondIng HDS arrays and map them
*     Write FITS DATa into HDS arrays

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     Handles I*2 as I*4

*  References:
*     {task_references}...

*  Keywords:
*     fitshds, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JKA: Jeremy Ashley (University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10-Nov-1993 V1.0-0 (JKA):
*        Original version
*     17 Jan 1994 V1.0-1 (JKA):
*        Auto detects FITS file type
*     17 Jan 1994 V1.0-2 (JKA):
*        Bug fixed in character mapping
*     17 Jan 1994 V1.0-3 (JKA):
*        Bug fixed in joining similar tables
*     17 Jan 1994 V1.0-4 (JKA):
*        Uses extname for file extensions
*     23 Jan 1994 V1.0-5 (JKA):
*        JOIN defaults to TRUE and EXTNAME is compared on adjacent tables
*     22 Apr 1994 V1.0-6 (JKA):
*        Converts ' ' & '.' into '_' for column headings & HDS extension names
*     24 Apr 1994 V1.7-0 (JKA):
*        Version update for new release of asterix
*     28 Apr 1994 V1.7-1 (JKA):
*        Handles 0 rows in a table
*      5 May 1994 V1.7-2 (JKA):
*        Dummy parameter added to fitsio call to fool compiler with string
*        lengths. FTGCVS(...
*     23 May 1994 V1.7-3 (JKA):
*        Close BDA common areas for images
*      6 Jun 1994 V1.7-4 (JKA):
*        Correct error in calculating total exposure time for image and duration.
*     31 Oct 1994 V1.7-5 (JKA):
*        Adds axis information for non-asterix images.
*     15 Dec 1995 V2.0-0 (DJA):
*        ADI port. Split in two for cleaner XRTCONV interface
*     {enter_changes_here}

*  Bugs:
*     Doesn't work correctly when updating CHAR*nn tables
*        - this is a possible FITSIO problem.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      CHARACTER*132		FNAME			! File to convert
      CHARACTER*132		HNAME			! Output file root
      CHARACTER*5		ORIGIN			! Input file origin

      LOGICAL			ASTERIX			! ASTERIX style file?
      LOGICAL			JOIN			! Table join mode
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input FITS file name
      CALL USI_GET0C( 'INPUT', FNAME, STATUS )
      CALL USI_GET0C( 'ORIGIN', ORIGIN, STATUS )
      CALL CHR_UCASE( ORIGIN )
      CALL USI_GET0L( 'ASTERIX', ASTERIX, STATUS )
      CALL USI_GET0L( 'JOIN', JOIN, STATUS )
      CALL USI_GET0C( 'OUTPUT', HNAME, STATUS )

*  Invoke internal routine
      CALL FITS2HDS_INT( FNAME, ASTERIX, JOIN, HNAME, ORIGIN, STATUS )

*  Write back origin to environment
      CALL USI_PUT0C( 'ORIGIN', ORIGIN, STATUS )

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE FITS2HDS_INT( FNAME, ASTERIX, JOIN, HNAME,
     :                         ORIGIN, STATUS )
*+
*  Name:
*     FITS2HDS_INT

*  Purpose:
*     {routine_purpose}

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FITS2HDS_INT( FNAME, ASTERIX, JOIN, HNAME, ORIGIN, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
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
*     {task_references}...

*  Keywords:
*     fitshds, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Structure Definitions:
      INCLUDE 'INC_XRTHEAD'
      INCLUDE 'INC_XRTSRT'


*  Arguments Given:
      CHARACTER*(*)		FNAME, HNAME
      LOGICAL			ASTERIX, JOIN

*  Arguments Given and Returned:
      CHARACTER*(*)		ORIGIN

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  CHR_LEN
        INTEGER                 CHR_LEN
      EXTERNAL                  CHR_INDEX
        INTEGER                 CHR_INDEX

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'FITS2HDS Version 2.2-0' )

      INTEGER                   MXRECS
        PARAMETER               ( MXRECS = 10000 )

      INTEGER                   MXCOL
        PARAMETER               ( MXCOL = 512 )

*  Local Variables:
      RECORD /XRT_HEAD/         HEAD 	    		! Header structure
      RECORD /XRT_SCFDEF/       SRT

      CHARACTER*(DAT__SZLOC) 	LOC        		! Locator to HDS file
      CHARACTER*(DAT__SZLOC) 	TLOC       		! Temporary locator to HDS
      CHARACTER*(DAT__SZLOC) 	ALOC(MXCOL)  		! Table columns
      CHARACTER*10 		FTYPE(MXCOL)		! Field types of arrays
      CHARACTER*12 TTYPE(MXCOL),TTSAVE(MXCOL)
      CHARACTER*10 TUNIT(MXCOL),TFORM(MXCOL)
      CHARACTER*6 		MAPMODE			! Column map mode
      CHARACTER*80 COMMENT
      CHARACTER*80 CTYPE(2)
      CHARACTER*80 CDUM			! Dummy character value
      CHARACTER*20 EXTNAME,S_EXTNAME    ! Table extension name

      REAL 			CRVAL(2),CRPIX(2),CDELT(2)
      REAL			SPARR(2,2)		! Spaced array data

      INTEGER 			FILENO			! File counter
      INTEGER 			HTYPE			! FITS header type
      INTEGER 			ISTATUS                 ! FITSIO status code
      INTEGER 			IUNIT			! Logical unit for FITS file
      INTEGER			OFID			! Output file id
      INTEGER 			PNTR(MXCOL),TPNTR	! Mapped HDS data

      LOGICAL 			SAME                    ! Same arrays as the last time
      LOGICAL 			FNEW                    ! Create NEW HDS file ?
      LOGICAL 			HDSOPEN                 ! HDS file open?
      LOGICAL SIMPLE,EXTEND,ANYF

      INTEGER NKEYS,NMORE,BLOCK
      INTEGER BITPIX,PCOUNT,GCOUNT
      INTEGER ROWLEN
      INTEGER HROWS			! Number of elements in HDS array
      INTEGER NROWS                     ! Number of rows in a table
      INTEGER TFIELDS                   ! Number of fields in a table
      INTEGER NAXIS                     ! Number of axes in an image
      INTEGER NAXES(MXCOL)                ! Dimensions of an image
      INTEGER DTYPE(MXCOL)                ! datatype of a field
      INTEGER TBCOL(MXCOL)
      INTEGER WIDTH(MXCOL)
      INTEGER VARIDAT,REPEAT,COL,NELEMS,II,NLEN
      INTEGER HBEG,HEND,FBEG,FNUM,HSAVE,NSAVE
      INTEGER NDUM			! Dummy integer value
      INTEGER NMAT,JJ			! Duplicate column names
      INTEGER CPIX			! Coords / Pixel
      INTEGER IND                       ! Index into names
      INTEGER SLEN                      ! String Length
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise variables
      ISTATUS = 0
      HDSOPEN = .FALSE.
      FILENO = 0
      FNEW = .TRUE.
      S_EXTNAME = ' '

*  Open the FITS fIle
      CALL FIO_GUNIT(IUNIT,ISTATUS)
      CALL FTOPEN(IUNIT,FNAME,0,BLOCK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
	 CALL MSG_SETC('FNAM',FNAME)
         CALL MSG_PRNT('** Error opening file ^FNAM **')
         GOTO 99
      ENDIF

*  Read image header information for image data
      CALL FTGKYJ( IUNIT, 'NAXIS', NAXIS, CDUM, ISTATUS )
      IF ( ASTERIX .AND. (NAXIS.NE.0) ) THEN

*    Auto detect data origins?
        IF ( ORIGIN .EQ. 'AUTO' ) THEN
          CALL RAT_FITSSTYLE(IUNIT,ORIGIN,STATUS)
        END IF

*    Is header information of a known type
        IF ( ORIGIN .EQ. ' ' ) THEN
          ASTERIX = .FALSE.
        ELSE
          CALL MSG_SETC( 'ORIG', ORIGIN )
          CALL MSG_PRNT( 'FITS file type is ^ORIG' )
        END IF

*     ASTERIX style file
        IF ( ASTERIX ) THEN
          CALL RAT_RDIMAGE( IUNIT, ORIGIN, HEAD, ISTATUS )
          IF ( ISTATUS .NE. 0 ) THEN
            CALL MSG_SETC( 'FNAM', FNAME )
            CALL MSG_PRNT( '** Error reading image header '//
     :                       'information from file ^FNAM **')

*        Cancel error and set ignore asterix header information
            ISTATUS = 0
            ASTERIX = .FALSE.

          END IF

        END IF

      END IF

*  Move to first header (default)
      CALL FTMAHD( IUNIT, 1, HTYPE, ISTATUS )

*  Loop for each header
      DO WHILE ( ISTATUS .EQ. 0 )

*    For primary or image DATa
        IF ( HTYPE .EQ. 0 ) THEN
          CALL FTGPRH(IUNIT,SIMPLE,BITPIX,NAXIS,
     :       NAXES,PCOUNT,GCOUNT,EXTEND,ISTATUS)

*    For ascii table data
        ELSE IF ( HTYPE .EQ. 1 ) THEN
          CALL FTGTBH(IUNIT,ROWLEN,NROWS,TFIELDS,
     :	     TTYPE,TBCOL,TFORM,TUNIT,EXTNAME,ISTATUS)

*    For binary table data
        ELSE IF ( HTYPE .EQ. 2 ) THEN
          CALL FTGBNH(IUNIT,NROWS,TFIELDS,TTYPE,
     :       TFORM,TUNIT,EXTNAME,VARIDAT,ISTATUS)

*    Otherwise error
        ELSE IF ((HTYPE.LT.0).OR.(HTYPE.GT.2)) THEN
          CALL MSG_PRNT('** Unknown FITS header type **')
          GOTO 99

        END IF

*    Make sure extension name is lower case
        CALL CHR_LCASE( EXTNAME )

*    For non-empty primary images
        IF ( (HTYPE.EQ.0) .AND. (NAXIS.NE.0) ) THEN

*      Open hds output fIle
          CALL FITS2HDS_OPHDS( HNAME, ' ', FILENO, FNEW, OFID, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
          HDSOPEN = .TRUE.

*      Create interface object
          CALL BDI_LINK( 'BinDS', NAXIS, NAXES, 'REAL', OFID, STATUS )

*      Create and map data array
          CALL BDI_MAPR( OFID, 'Data', 'WRITE', TPNTR, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( 'Error creating and mapping real array' )
            GOTO 99
          END IF

*      Calculate total number of elements in array
          CALL ARR_SUMDIM( NAXIS, NAXES, NELEMS )

*      Read this column and fill the array
          CALL FTGPVE( IUNIT, 0, 1, NELEMS, 0, %VAL(TPNTR), ANYF,
     :                 ISTATUS )

*      Unmap the data array
          CALL BDI_UNMAP( OFID, 'Data', TPNTR, STATUS )

*      New ASTERIX file?
          IF ( FNEW .AND. ASTERIX ) THEN

*        Update the SRT so the exposure time is calculated
            SRT.NTIME = 1
            SRT.MIN_T(1) = HEAD.TSTART(1)
            SRT.MAX_T(1) = HEAD.TEND(HEAD.NTRANGE)

*        Write sort info
            CALL XRTSORT_CRE_ASTERIX( OFID, SRT, HEAD, STATUS )
            IF (STATUS.NE.SAI__OK) GOTO 99

*        Sort axes
            CPIX  = 1.0

*        Multiply scale by 30 (coord increment/pixel) for MPE
            IF ( ORIGIN .EQ. 'MPE' ) CPIX = 30
            SPARR(1,1) =  CPIX * HEAD.PIXEL / 3600. * (NAXES(1) / 2 )
            SPARR(2,1) = - CPIX * HEAD.PIXEL / 3600.0
            SPARR(1,2) = -CPIX * HEAD.PIXEL / 3600. * (NAXES(2) / 2 )
            SPARR(2,2) = - SPARR(2,1)
            DO II = 1,2
              CALL BDI_AXPUT0C( OFID, II, 'units', 'degrees', STATUS )
              CALL BDI_AXPUT1R( OFID, II, 'SpacedData', 2,
     :                            SPARR(1,II), STATUS )
            END DO

*      New non-ASTERIX file
          ELSE IF ( FNEW ) THEN

*        Read axis keywords
	    ISTATUS = 0
	    CALL FTGKYS(IUNIT,'CTYPE1',CTYPE(1),COMMENT,ISTATUS)
	    CALL FTGKYS(IUNIT,'CTYPE2',CTYPE(2),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CRVAL1',CRVAL(1),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CRVAL2',CRVAL(2),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CRPIX1',CRPIX(1),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CRPIX2',CRPIX(2),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CDELT1',CDELT(1),COMMENT,ISTATUS)
	    CALL FTGKYE(IUNIT,'CDELT2',CDELT(2),COMMENT,ISTATUS)

	    IF (ISTATUS.NE.0) THEN
	      CALL MSG_PRNT('*** Cannot add axis information ***')
              ISTATUS = 0
            ELSE

	      DO II = 1,NAXIS
                SPARR(1,II) = -(CDELT(II) * CRPIX(II))
                SPARR(2,II) = CDELT(II)
		CALL BDI_AXPUT0C( OFID, II, 'Units', CTYPE(II), STATUS )
                CALL BDI_AXPUT1R( OFID, II, 'SpacedData', 2,
     :                              SPARR(1,II), STATUS )
              END DO

            END IF
          END IF

       END IF

*    For table data
       IF ((HTYPE.EQ.1).OR.(HTYPE.EQ.2)) THEN

*     Check if the column names have changed from the last header
         SAME = .TRUE.
         DO II = 1, TFIELDS

*       Replace any ' ' in field name
           SLEN = CHR_LEN(TTYPE(II))
           DO WHILE (CHR_INDEX(TTYPE(II)(1:SLEN),' ').NE.0)
             IND = CHR_INDEX(TTYPE(II)(1:SLEN),' ')
             TTYPE(II)(IND:IND) = '_'
           END DO

*       Compare with last times column name
           IF ( TTYPE(II) .NE. TTSAVE(II) ) SAME = .FALSE.

*       Check no duplicates exist
           NMAT = 0
           DO JJ = 1, II-1
             IF (TTYPE(II).EQ.TTYPE(JJ)) NMAT = NMAT + 1
           END DO
           IF (NMAT.NE.0) THEN
***               Should write nmat here
              TTYPE(II)=TTYPE(II)(1:CHR_LEN(TTYPE(II)))//'_1'
           END IF
           TTSAVE(II) = TTYPE(II)

         END DO

*     Replace and '.' in extension name
         DO WHILE (CHR_INDEX(EXTNAME,'.').NE.0)
           IND = CHR_INDEX(EXTNAME,'.')
           EXTNAME(IND:IND) = '_'
         END DO

*     Check if this is a new file/extension
         FNEW = .NOT.(SAME.AND.JOIN.AND.(EXTNAME.EQ.S_EXTNAME))
         S_EXTNAME = EXTNAME

*     Create an output HDS fIle
	 CALL FITS2HDS_OPHDS( HNAME, EXTNAME, FILENO, FNEW, OFID,
     :                        STATUS )
         CALL ADI1_GETLOC( OFID, LOC, STATUS )
         IF (STATUS.NE.SAI__OK) goto 99
         HDSOPEN = .TRUE.

         DO COL = 1, TFIELDS

*       Calculate length of fieldname
           NLEN = CHR_LEN(TTYPE(COL))

*       Get information for new fields
           IF ( FNEW ) THEN
             HSAVE = 0
             HROWS = NROWS

*         Trap case of empty table
             IF ( HROWS .EQ. 0 ) THEN
               CALL MSG_PRNT('** Table has no rows **')
               GOTO 10
             END IF

***
             CALL FTBNFM( TFORM(COL), DTYPE(COL), REPEAT,
     :                              WIDTH(COL), ISTATUS )

*         Create hds structure
             MAPMODE = 'WRITE'
             IF ( DTYPE(COL).EQ.21) THEN
               CALL DAT_NEW1I(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
               FTYPE(COL) = '_INTEGER'

             ELSE IF ( DTYPE(COL) .EQ. 41 ) THEN
               CALL DAT_NEW1I(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
               FTYPE(COL) = '_INTEGER'

             ELSE IF ( DTYPE(COL) .EQ. 42 ) THEN
               CALL DAT_NEW1R(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
               FTYPE(COL) = '_REAL'

             ELSE IF (DTYPE(COL).EQ.82) THEN
               CALL DAT_NEW1D(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
               FTYPE(COL) = '_DOUBLE'

             ELSEIF (DTYPE(COL).EQ.16) THEN
               CALL DAT_NEW1C(LOC,TTYPE(COL)(1:NLEN),
     :                                     WIDTH(COL),NROWS,STATUS)
               FTYPE(COL) = '_CHAR*'
               CALL CHR_ITOC(WIDTH(COL),FTYPE(COL)(7:),NDUM)

             ELSEIF (DTYPE(COL).EQ.14) THEN
               CALL DAT_NEW1L(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
               FTYPE(COL) = '_LOGICAL'

             END IF

*         Get a locator to the new array
             CALL DAT_FIND(LOC,TTYPE(COL)(1:NLEN),ALOC(COL),STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('ANAM',TTYPE(COL)(1:NLEN))
               CALL MSG_PRNT('Error creating array ^ANAM')
               GOTO 99
             END IF

           ELSE
             HROWS = NROWS + HSAVE
             MAPMODE = 'UPDATE'
             CALL DAT_FIND(LOC,TTYPE(COL)(1:NLEN),ALOC(COL),STATUS)
             CALL DAT_ALTER(ALOC(COL),1,HROWS,STATUS)

           END IF
         END DO

         CALL MSG_SETI('ROWS',NROWS)
         CALL MSG_SETI('COLS',TFIELDS)
         CALL MSG_PRNT('Converting ^COLS columns and ^ROWS rows')

*     Read from the FITS file in groups of MAXRECS records
         NSAVE = 0
         DO WHILE ( HSAVE .LT. HROWS )

           FNUM = MIN(NROWS-NSAVE,MXRECS)
           HBEG = 1 + HSAVE
           HEND = HSAVE + FNUM
           FBEG = 1 + NSAVE
           HSAVE = HSAVE + FNUM
           NSAVE = NSAVE + FNUM

           CALL MSG_SETI('BEG',HBEG)
           CALL MSG_SETI('END',HEND)
           CALL MSG_PRNT('Reading/mapping from ^BEG to ^END')

*       For each column
           DO COL = 1,TFIELDS

*         Map the array segment
             CALL DAT_SLICE(ALOC(COL),1,HBEG,HEND,TLOC,STATUS)
             CALL DAT_MAPV(TLOC,FTYPE(COL),MAPMODE,PNTR(COL),
     :                                                 NDUM,STATUS)
             IF (STATUS.NE.SAI__OK) GOTO 99

*         Read the data
             IF (DTYPE(COL).EQ.21) THEN
               CALL FTGCVJ(IUNIT,COL,FBEG,1,FNUM,0,
     :                                  %val(PNTR(COL)),ANYF,ISTATUS)

             ELSE IF (DTYPE(COL).EQ.41) THEN
               CALL FTGCVJ(IUNIT,COL,FBEG,1,FNUM,0,
     :                                  %val(PNTR(COL)),ANYF,ISTATUS)

             ELSE IF (DTYPE(COL).EQ.42) THEN
               CALL FTGCVE(IUNIT,COL,FBEG,1,FNUM,0.0,
     :                                 %val(PNTR(COL)),ANYF,ISTATUS)

             ELSE IF (DTYPE(COL).EQ.82) THEN
               CALL FTGCVD(IUNIT,COL,FBEG,1,FNUM,0.D0,
     :                                 %val(PNTR(COL)),ANYF,ISTATUS)

             ELSE IF (DTYPE(COL).EQ.16) THEN
* A dummy argument needs to be tacked onto the end to fool the compiler
* with the length of the "string" passed by pointer, because there is already
* a string used in the function call, a dummy call will need to be made
* to correct the order.
                     CALL RAT_FTGCVS(IUNIT,COL,FBEG,1,FNUM,
     :                %val(PNTR(COL)),' ',ANYF,ISTATUS,%val(WIDTH(COL)))

*         Logical data
             ELSE IF (DTYPE(COL).EQ.14) THEN
               CALL FTGCL(IUNIT,COL,FBEG,1,FNUM,
     :                                    %val(PNTR(COL)),ISTATUS)

             END IF

*         Unmap the array segment
             CALL DAT_UNMAP( TLOC, STATUS )
             CALL DAT_ANNUL( TLOC, STATUS )

*       Next segment
           END DO

*     Next row
         END DO

*     Unmap the columns/arrays
         DO COL = 1, TFIELDS
           CALL DAT_ANNUL( ALOC(COL), STATUS )
         END DO

       END IF

*  Continuation point after zero length table
 10    CONTINUE

*   Write FITS header informatIon to HDS file
       IF ( FNEW .AND. HDSOPEN ) THEN

*      Create more box
          CALL ADI1_LOCMORE( OFID, .TRUE., TLOC, STATUS )

*      Get header details
          CALL FTGHSP( IUNIT, NKEYS, NMORE, ISTATUS )

*      Create structure in HDS file
          CALL DAT_NEWC( TLOC, 'FITS', 80, 1, NKEYS, STATUS )
          CALL CMP_MAPV( TLOC, 'FITS', '_CHAR*80', 'WRITE',
     :                                TPNTR, NDUM, STATUS )

*      Read header record in FITS file
          CALL FXRDHD( IUNIT, %VAL(TPNTR), NKEYS, ISTATUS )
          CALL CMP_UNMAP( TLOC, 'FITS', STATUS )

*      Produce a history record
          CALL HSI_ADD( OFID, VERSION, STATUS )

        END IF

        IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error writing FITS header info.')
          GOTO 99
        END IF

*    Close the HDS file
        IF ( HDSOPEN ) THEN
          CALL ADI_FCLOSE( OFID, STATUS )
        END IF
        HDSOPEN = .FALSE.

*    Move to next extension and loop back
        CALL FTMRHD( IUNIT, 1, HTYPE, ISTATUS )

      END DO

*  If end of header/fIle clear error flag
      IF ( ISTATUS .EQ. 107 ) ISTATUS = 0

*  Close FITS fIle
      CALL FTCLOS( IUNIT, ISTATUS )
      CALL FIO_PUNIT( IUNIT, ISTATUS )

*  Display any error messages
 99   IF (ISTATUS.NE.0) THEN
        CALL ADI2_FITERP( ISTATUS, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FITS2HDS_INT', STATUS )
      END IF

      END


*+ FXRDHD - Read header records into an array
        SUBROUTINE FXRDHD(IUNIT,HDARR,NHDR,ISTATUS)
*
        INTEGER IUNIT,NHDR,ISTATUS
        CHARACTER*80 HDARR(NHDR)
        CHARACTER*132 LINE
        INTEGER I

        DO I=1,NHDR
           CALL FTGREC(IUNIT,I,LINE,ISTATUS)
           HDARR(I) = LINE(1:80)
        ENDDO
        END


*+ FITS2HDS_OPHDS - Open or create a HDS file
      SUBROUTINE FITS2HDS_OPHDS( HNAME, EXTNAME, FCNT, FNEW, FID,
     :                           STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    FunctIon declaratIons :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Import:
      CHARACTER*(*) HNAME,EXTNAME
      LOGICAL		FNEW
*    Import/Export:
      INTEGER			FCNT
*    Export:
      INTEGER			FID

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		CFCNT			! Char of FCNT
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*132		PNAME
        SAVE                    PNAME

      INTEGER			NCHAR			! # characters used
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  New file?
      IF ( FNEW ) THEN

*    Create an output HDS fIle
        IF ( EXTNAME .NE. ' ' ) THEN
          PNAME = HNAME(1:CHR_LEN(HNAME))//'_'//extname

*    Indexed file
        ELSE IF ( FCNT .GT. 0 ) THEN
          CALL CHR_ITOC(FCNT, CFCNT, NCHAR)
          PNAME = HNAME(1:CHR_LEN(HNAME))//'_'//CFCNT(1:NCHAR)
          FCNT = FCNT + 1

        ELSE
          PNAME = HNAME(1:CHR_LEN(HNAME))
          FCNT = FCNT + 1

        END IF

*    Create the file
        CALL HDS_NEW(PNAME, 'FITSCONV', 'FITSCONV', 0, 0, LOC, STATUS )

*    Report opening
        IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error opening output fIle')
        ELSE
          CALL MSG_SETC('PNAM', PNAME)
          CALL MSG_PRNT('Writing to ^PNAM')
        END IF

*  Open existing file
      ELSE

*    Open the old fIle
        CALL HDS_OPEN( PNAME,'UPDATE', LOC, STATUS )
        IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error opening existing output file')
        ELSE
          CALL MSG_SETC('PNAM', PNAME)
          CALL MSG_PRNT('Updating ^PNAM')
        END IF

      END IF

*  Make identifier from locator
      IF ( FNEW ) THEN
        CALL ADI1_MKFILE( LOC, 'WRITE', FID, STATUS )
      ELSE
        CALL ADI1_MKFILE( LOC, 'UPDATE', FID, STATUS )
      END IF

      END


*+ RAT_FTGCVS A dummy FITSIO procedure to pass character strings pointers
      SUBROUTINE RAT_FTGCVS(IUNIT,COL,FBEG,N,FNUM,PNTR,NVAL,
     :                                                ANYF,ISTATUS)
*     A dummy procedure to allow a character pointer to be passed.
*     This procedure is called with one extra parameter, the size of
*     the character string.
      INTEGER IUNIT,COL,FBEG,FNUM,ISTATUS,N
      CHARACTER*(*) NVAL
      CHARACTER*(*) PNTR(*)

      CALL FTGCVS(IUNIT,COL,FBEG,N,FNUM,NVAL,PNTR,ANYF,ISTATUS)
      END
