      SUBROUTINE DYN0_MAP( NDIM, DIMS, MTYPE, PTR, STATUS )
*+
*  Name:
*     DYN0_MAP

*  Purpose:
*     Map a dynamic array of elements of type MTYPE

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN0_MAP( NDIM, DIMS, MTYPE, PTR, STATUS )

*  Description:
*     Tries to allocate an area of mapped memory from either the process
*     heap or by mapping an HDS file.

*  Arguments:
*     NDIM = INTEGER (given)
*        Dimensionality of required dynamic array
*     DIMS[] = INTEGER (given)
*        Dimensions of the required dynamic array
*     MTYPE = CHARACTER*(*) (given)
*        The type of the elements (HDS style type name)
*     PTR = INTEGER (returned)
*        Address of the newly allocated dynamic memory section
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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 Jun 1992 (DJA):
*        Use PSX call to get small amounts of memory. Chooses
*        scratch file name depending on OS.
*      4 Jan 1993 (DJA):
*        Use process pid to create uniquely named files. Removes
*        restriction on multiple processes sharing scratch space.
*      7 Apr 1993 (DJA):
*        Delete file if created but not mapped. Error reporting tidied up.
*     20 Mar 1995 (DJA):
*        Renamed to from DYN_MAP. No longer works in pages.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYN_ISINIT = LOGICAL (given)
*         DYN class definitions loaded?
*       DYS_ISEQ = INTEGER (given and returned)
*         File name sequence number

*  Arguments Given:
      INTEGER			NDIM			! See above
      INTEGER			DIMS(*)
      CHARACTER*(*)		MTYPE

*  Arguments Returned:
      INTEGER			PTR			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

      EXTERNAL                  DYN0_BLK                ! Ensures inclusion

*  Local Constants:
      INTEGER			MAXSYS
        PARAMETER		( MAXSYS = 1024*1024*4 )

*  Local Variables:
      CHARACTER*132           	FNAME      		! Scratch file name
      CHARACTER*8            	HPID       		! Process identifier
							! in zero padded hex
      CHARACTER*(DAT__SZLOC) 	LOC			! Temp file locator
      CHARACTER*80           	SDIR       		! Scratch directory path

      INTEGER			FID			! ADI file identifier
      INTEGER                	FLEN       		! Length of FNAME used
      INTEGER                	NBYTE			! Total # bytes needed
      INTEGER			NELM			! Total # elements
      INTEGER                	PID        		! Process identifier
      INTEGER                	SIZE       		! Size in elements

      LOGICAL                	GOTSCR			! Got scratch area yet?
      LOGICAL                	GOTPID    		! Got process pid?
      LOGICAL                	GOTSYS     		! Got VM from heap?
      LOGICAL                	VALID      		! LOC a valid locator?

*  Local Data:
      DATA 			GOTSCR, GOTPID/.FALSE., .FALSE./
      SAVE 			SDIR,GOTSCR,GOTPID,HPID
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. DYN_ISINIT ) CALL DYN0_INIT()

*  Determine element size in bytes
      IF ( (MTYPE(2:2) .EQ. 'I') .OR. (MTYPE(2:2) .EQ. 'L') ) THEN
        SIZE = VAL__NBI
      ELSE IF ( MTYPE(2:2) .EQ. 'R' ) THEN
        SIZE = VAL__NBR
      ELSE IF ( MTYPE(2:2) .EQ. 'D' ) THEN
        SIZE = VAL__NBD
      ELSE IF ( MTYPE(2:2) .EQ. 'W' ) THEN
        SIZE = VAL__NBW
      ELSE IF ( MTYPE(2:2) .EQ. 'B' ) THEN
        SIZE = VAL__NBB
      ELSE
        CALL MSG_SETC( 'TYPE', MTYPE )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unrecognised element type /^TYPE/', STATUS )
      END IF

*  Work out number of bytes
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      NBYTE = SIZE * NELM

*  User requested valid amount of memory?
      IF ( (NBYTE .GT. 0) .AND. (STATUS .EQ. SAI__OK) ) THEN

*    Initialise
        GOTSYS = .FALSE.
        FID = ADI__NULLID

*    Get small amounts from system pool
        IF ( NBYTE .LE. MAXSYS ) THEN
          CALL PSX_MALLOC( NBYTE, PTR, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
          ELSE
            GOTSYS = .TRUE.
          END IF
        END IF

*    If bigger or system call was unsuccesful create mapped disk section
        IF ( (NBYTE.GT.MAXSYS) .OR. .NOT. GOTSYS ) THEN

*      Know scratch area yet?
          IF ( .NOT. GOTSCR ) THEN
            CALL PSX_GETENV( 'AST_SCRATCH', SDIR, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              SDIR = ' '
            END IF
            GOTSCR = .TRUE.
          END IF

*      Know process id yet?
          IF ( .NOT. GOTPID ) THEN
            CALL PSX_GETPID( PID, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              PID = 1
            END IF
            GOTPID = .TRUE.

*        Format in hex
            WRITE( HPID, '(Z8.8)' ) PID

          END IF

*      Create unique filename
          CALL MSG_FMTI( 'N', 'I4.4', DYS_ISEQ )
          DYS_ISEQ = DYS_ISEQ + 1
          CALL MSG_SETC( 'PID', HPID//'_' )
          CALL MSG_MAKE( 'DYN_^PID^N.TMP', FNAME, FLEN )
          IF ( SDIR .GT. ' ' ) THEN
            FNAME = SDIR(:CHR_LEN(SDIR))//FNAME(:FLEN)
            FLEN = FLEN + CHR_LEN(SDIR)
          END IF

*      Try to create and map the file
          CALL HDS_NEW( FNAME(:FLEN), 'DYN', '_BYTE', 1, NBYTE, LOC,
     :                                                      STATUS )
          CALL DAT_MAPV( LOC, '_BYTE', 'WRITE', PTR, NBYTE, STATUS )
          CALL ADI1_PUTLOC( FID, LOC, STATUS )

*      Failed? If so, create new error context and try deleting the
*      file. HDS unfortunately can create files which it cannot then
*      erase.
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_BEGIN( STATUS )

*        Locator is valid? Don't even try deleting the file otherwise
            CALL DAT_VALID( LOC, VALID, STATUS )
            IF ( VALID ) THEN
              CALL HDS_ERASE( LOC, STATUS )
            END IF

*        Restore error context
            CALL ERR_END( STATUS )

          END IF

*      Report error if failure
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'Unable to create dynamic memory',
     :                                                   STATUS )
            CALL MSG_SETI('NBYTE',NBYTE)
            CALL ERR_REP( ' ', '^NBYTE bytes were requested'//
     :                     ' - check space on AST_SCRATCH', STATUS )
          END IF

        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP(' ', 'Dynamic memory of zero length requested',
     :                                                      STATUS )
      END IF

*  Add to internal list
      CALL DYN0_MAPADD( PTR, NELM, NBYTE, FID, STATUS )

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'DYN0_MAP', STATUS )
      END IF

      END



      SUBROUTINE DYN0_MAPADD( PTR, NITEM, NBYTE, FID, STATUS )
*+
*  Name:
*     DYN0_MAPADD

*  Purpose:
*     Add details of a mapped memory section to the internal store

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN0_MAPADD( PTR, NITEM, NBYTE, FID, STATUS )

*  Description:
*     Tries to allocate an area of mapped memory from either the process
*     heap or by mapping an HDS file.

*  Arguments:
*     PTR = INTEGER (given)
*        Address of the section
*     NITEM = INTEGER (given)
*        Number of mapped elements
*     NBYTE = INTEGER (given)
*        Number of mapped bytes
*     FID = INTEGER (GIVEN)
*        ADI file identifier if file section
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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
*        Renamed to DYN_MAP_ADD. No longer works in pages.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYS_PTR = INTEGER (given and returned)
*         DYN memory addresses
*       DYS_NBYTE = INTEGER (returned)
*         DYN memory sizes
*       DYS_NITEM = INTEGER (returned)
*         DYN memory number of elements
*       DYS_FID = INTEGER (returned)
*         DYN file identifiers

*  Arguments Given:
      INTEGER			PTR			! See above
      INTEGER			NITEM, NBYTE, FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over slots

      LOGICAL                	FOUND			! Found a free slot?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for a free slot
      I = 1
      FOUND = .FALSE.
      DO WHILE ( (I.LE.DYN__NMAX) .AND. .NOT. FOUND )
        IF ( DYS_PTR(I) .EQ. 0 ) THEN
          FOUND = .TRUE.
        ELSE
          I = I + 1
        END IF
      END DO

*    Use next slot
      IF ( FOUND ) THEN

*      Initialise fields of slot
        DYS_PTR(I) = PTR
        DYS_NBYTE(I) = NBYTE
        DYS_NITEM(I) = NITEM
        DYS_FID(I) = FID

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Run out of internal DYN slots', STATUS )

      END IF

      END
