      SUBROUTINE BINLIST( STATUS )
*+
*  Name:
*     BINLIST

*  Purpose:
*     Lists data stored in a 1D binned dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL BINLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     binlist {parameter_usage}

*  Environment Parameters:
*     INP = LITERAL (read)
*        NDF to be listed
*     DEV = CHAR (read)
*        Ascii output device name

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     binlist, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Jun 1989 V1.0-1 (RJV):
*        Now indicates where quantities are defaulted
*     29 Oct 1991 V1.5-0 (DJA):
*        Uses PRS_GETSLICE for slice parse. Doesn't display data which
*        doesn't exist. Handles asymmetric data and axis errors.
*      3 Mar 1994 V1.7-0 (DJA):
*        Use sensible format for real numbers
*      4 May 1994 V1.7-1 (DJA):
*        Use AIO to do i/o
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     12 Jan 1995 V1.8-1 (DJA):
*        Updated data interface
*     30 Aug 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'BINLIST Version 2.1-0' )

*  Local Variables:
      CHARACTER*132		FILE, PATH
      CHARACTER*20           	SLICE       		! Data slice specification

      INTEGER                	AEPTR(2)    		! Asymmetric data errors
      INTEGER                	AWPTR(2)    		! Asymmetric axis errors

      INTEGER			FID			! File identifier
      INTEGER 			NLEV
      INTEGER			OCH			! Output channel
      INTEGER 			DEFWIDTH                ! Width of output
      INTEGER 			NDIM            	! I/p dimensionality
      INTEGER 			DIMS(ADI__MXDIM)	! I/p dimensions
      INTEGER RANGES(2,ADI__MXDIM)       ! Ranges of data to be output
      INTEGER DPTR,VPTR,QPTR,APTR,WPTR

      LOGICAL                	DOK
      LOGICAL VOK,AOK,WOK,QOK,AWOK(2),AEOK(2)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

      DOK = .FALSE.

*  Input dataset
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', FID, STATUS )

*  Get dimensionality and whether data is ok
      CALL BDI_GETSHP( FID, ADI__MXDIM, DIMS, NDIM, STATUS )
      CALL BDI_CHK( FID, 'Data', DOK, STATUS )

*  Must be 1-D
      IF ( DOK.AND. (NDIM.EQ.1) ) THEN

*    Set up output channel
        CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEFWIDTH, STATUS )

*    Write dataset name
        CALL ADI_FTRACE( FID, NLEV, PATH, FILE, STATUS )
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_WRITE( OCH, 'Dataset:-', STATUS )
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_IWRITE( OCH, 2, FILE(:CHR_LEN(FILE)), STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*    Axis data?
        CALL BDI_AXCHK( FID, 1, 'Data', AOK, STATUS )
        CALL BDI_AXMAPR( FID, 1, 'Data', 'READ', APTR, STATUS )

*    Asymmetric axis errors?
        CALL BDI_AXCHK( FID, 1, 'LoWidth,HiWidth', AWOK, STATUS )
        IF ( AWOK(1) .AND. AWOK(2) ) THEN
          WOK = .FALSE.
          CALL BDI_AXMAPR( FID, 1, 'LoWidth,HiWidth', 'READ',
     :                     AWPTR, STATUS )
        ELSE
          CALL BDI_AXCHK( FID, 1, 'Width', WOK, STATUS )
          IF ( WOK ) THEN
            CALL BDI_AXMAPR( FID, 1, 'Width', 'READ', WPTR, STATUS )
          END IF
        END IF

*    The data
        CALL BDI_MAPR( FID, 'Data', 'READ', DPTR, STATUS )

*    Asymmetric data errors?
        CALL BDI_CHK( FID, 'LoError,HiError', AEOK, STATUS )
        IF ( AEOK(1) .AND. AEOK(2) ) THEN
          VOK = .FALSE.
          CALL BDI_MAPR( FID, 'LoError,HiError', 'READ', AEPTR, STATUS )

*      Otherwise look for variance
        ELSE
          CALL BDI_CHK( FID, 'Variance', VOK, STATUS )
          IF ( VOK ) THEN
            CALL BDI_MAPR( FID, 'Variance', 'READ', VPTR, STATUS )
          END IF

        END IF

*    Quality
        CALL BDI_CHK( FID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPUB( FID, 'Quality', 'READ', QPTR, STATUS )
        END IF

*    Get slice of dataset
        CALL USI_GET0C( 'SLICE', SLICE, STATUS )
        CALL PRS_GETSLICE( NDIM, DIMS(1), SLICE, RANGES, STATUS )

*    Output the data
        CALL BINLIST_OUT( %VAL(DPTR), %VAL(VPTR), VOK, %VAL(AEPTR(1)),
     :                    %VAL(AEPTR(2)), AEOK, %VAL(QPTR), QOK,
     :                    %VAL(AWPTR(1)), %VAL(AWPTR(2)), AWOK,
     :                    %VAL(APTR), AOK, %VAL(WPTR), WOK, RANGES,
     :                                      OCH, DEFWIDTH, STATUS )


*    Close output channel
        CALL AIO_CANCL( 'DEV', STATUS )

      ELSE IF ( .NOT. DOK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP(' ','AST_ERR: no data present',STATUS)

      ELSE IF ( DOK .AND. (NDIM.NE.1) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP(' ','AST_ERR: dataset not 1D',STATUS)

      END IF

*  Close down ASTERIX
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END




      SUBROUTINE BINLIST_OUT( D, V, VOK, AEL, AEU, AEOK, Q, QOK,
     :                        AWL, AWU, AWOK, A, AOK, W, WOK,
     :                        RANGES, OCH, OUTWID, STATUS )
*+
*  Name:
*     BINLIST_OUT

*  Purpose:
*     Write 1-D binned dataset values to text file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BINLIST_OUT( [p]... )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     OCH = INTEGER (given)
*        Output channel id
*     OUTWID = INTEGER (given)
*        Width of output page
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
*     {task_references}...

*  Keywords:
*     binlist, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL 			D(*), V(*), AEL(*), AEU(*)
      REAL 			A(*), W(*), AWL(*), AWU(*)
      BYTE 			Q(*)
      LOGICAL 			VOK, QOK, AOK, WOK, AEOK, AWOK
      INTEGER 			RANGES(2), OCH, OUTWID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*10 		FMT2,FMT3
        PARAMETER 		( FMT2='(I6)', FMT3='(1PG14.6)' )

      CHARACTER*1 		BLANK, KET, DASH, VLIN
        PARAMETER 		( BLANK=' ', KET='>', DASH='-',
     :                            VLIN='|' )

*  Local Variables:
      CHARACTER*80     		LINE                	! Output buffer

      INTEGER          		COL                   	! Column counter
      INTEGER          		ACOL,WCOL,DCOL,VCOL,QCOL ! Columns for output
      INTEGER          		I                       ! Index to vector component
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Print header
      LINE=BLANK
      CALL CHR_FILL(DASH,LINE(8:OUTWID-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWID), STATUS )
      LINE=BLANK
      LINE(7:7)=VLIN
      LINE(OUTWID:OUTWID)=VLIN

*  Write out headers
      COL = 9
      IF ( AOK ) THEN
        ACOL = COL
        COL = COL + 15
        LINE(ACOL+5:) = 'AXIS'
      END IF
      IF ( AWOK ) THEN
        WCOL = COL
        COL = COL + 30
        LINE(WCOL+4:) = 'LOWIDTH'
        LINE(WCOL+19:) = 'UPWIDTH'
      ELSE IF ( WOK ) THEN
        WCOL = COL
        COL = COL + 15
        LINE(WCOL+5:) = 'WIDTH'
      END IF
      DCOL = COL
      COL = COL + 15
      LINE(DCOL+5:)='DATA'
      IF ( AEOK ) THEN
        VCOL = COL
        COL = COL + 30
        LINE(VCOL+4:) = 'LOERROR'
        LINE(VCOL+19:) = 'UPERROR'
      ELSE IF ( VOK ) THEN
        VCOL = COL
        COL = COL + 15
        LINE(VCOL+2:) = 'VARIANCE'
      END IF
      IF ( QOK ) THEN
        QCOL=COL
        LINE(QCOL+1:) = 'QUALITY'
      END IF
      CALL AIO_WRITE( OCH, LINE(:OUTWID), STATUS )

*  Loop over data
      DO I = RANGES(1), RANGES(2)

*    Blank the buffer
        LINE = BLANK

*    The line number
        IF (MOD(I,5).EQ.0) THEN
          WRITE(LINE(:6),FMT2) I
          LINE(7:7)=KET
        ELSE
          LINE(7:7)=VLIN
        END IF

*    Write data to buffer
        IF ( AOK) WRITE(LINE(ACOL:),FMT3)  A(I)
        IF ( AWOK ) THEN
          WRITE(LINE(WCOL:),FMT3) AWL(I)
          WRITE(LINE(WCOL+15:),FMT3) AWU(I)
        ELSE IF ( WOK) THEN
          WRITE(LINE(WCOL:),FMT3) W(I)
        END IF

        WRITE(LINE(DCOL:),FMT3) D(I)
        IF ( AEOK ) THEN
          WRITE(LINE(VCOL:),FMT3) AEL(I)
          WRITE(LINE(VCOL+15:),FMT3) AEU(I)
        ELSE IF ( VOK) THEN
          WRITE(LINE(VCOL:),FMT3) V(I)
        END IF
        IF ( QOK) CALL STR_BTOC(Q(I),LINE(QCOL:),STATUS)

*    End vertical bar
        LINE(OUTWID:OUTWID) = VLIN

*    Write buffer
        CALL AIO_WRITE( OCH, LINE(:OUTWID), STATUS )

*  Next data point
      END DO

*  Footer
      LINE = BLANK
      CALL CHR_FILL(DASH,LINE(8:OUTWID-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWID), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BINLIST_OUT', STATUS )

      END
