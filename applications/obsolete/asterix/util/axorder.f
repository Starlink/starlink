      SUBROUTINE AXORDER( STATUS )
*+
*  Name:
*     AXORDER

*  Purpose:
*     Rearranges specified axes in a dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AXORDER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     axorder {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input object - may be primitive
*     SELAX[] = INTEGER (read)
*        New order of axes
*     OUT = CHAR (read)
*        Output dataset

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
*     axorder, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Dec 1989 V1.0-0 (DJA):
*        Original version.
*     12 Mar 1990 V1.2-0 (DJA):
*        Was copying output axes incorrectly
*      4 Jul 1990 V1.2-1 (DJA):
*        Swaps 1d data with axis values. Bit of a dodgy thing to do.
*      8 May 1991 V1.4-0 (DJA):
*        Re-named from AXSWAP
*      6 Mar 1992 V1.6-0 (DJA):
*        OUT prompt moved back to pre-processing
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     26 Mar 1995 V1.8-1 (DJA):
*        Use new data interface
*     17 Jul 1995 V1.8-2 (DJA):
*        Corrected bug copying 1-D axis widths
*      1 Dec 1995 V2.0-0 (DJA):
*        ADI port
*      4 Jan 1996 V2.0-1 (DJA):
*        Use USI_NAMES for history update
*      5 Mar 1996 V2.0-2 (DJA):
*        Added support for Grouping item
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

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AXORDER Version 2.1-0' )

*  Local Variables:
      CHARACTER*80              HTXT			! History text

      INTEGER                   DIMS(ADI__MXDIM)  	! Input dimensions
      INTEGER                   IDPTR,IGPTR,IVPTR,IQPTR ! Input data pointers
      INTEGER                   ODIMS(ADI__MXDIM) 	! Output dimensions
      INTEGER                   SELAX(ADI__MXDIM) 	! New axis order
      INTEGER                   I                 	! Loop over dimensions
      INTEGER                   IAPTR      		! Input & output axis data
      INTEGER			IFID		  	! Input dataset id
      INTEGER			IFILES			! USI input files list
      INTEGER                   IWPTR             	! Input axis widths
      INTEGER                   NDIM              	! Dimensionality
      INTEGER                   ODPTR,OGPTR,OVPTR,OQPTR ! Output data pointers
      INTEGER			OFID			! Output dataset id
      INTEGER                   OWPTR             	! Output axis widths
      INTEGER                   NSEL              	! # axes specified

      LOGICAL                   IWOK              	! Input widths ok?
      LOGICAL                   OK, GOK, VOK, QOK      	! Input objects ok?
      LOGICAL                   SPEC(ADI__MXDIM)  	! Axis specified in output?
      LOGICAL                   SWAP_1D           	! Special 1D swap?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input object
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check and map the data
      SWAP_1D = .FALSE.
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR : Invalid data', STATUS )
      ELSE IF ( NDIM .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object is scalar - no axes to swap!',
     :                STATUS )
      ELSE IF ( NDIM .EQ. 1 ) THEN
        SWAP_1D = .TRUE.
        CALL MSG_PRNT( 'Swapping data and axis values' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )

*  Variance
      CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
      END IF

*  Grouping
      CALL BDI_CHK( IFID, 'Grouping', GOK, STATUS )
      IF ( GOK ) THEN
        CALL BDI_MAPI( IFID, 'Grouping', 'READ', IGPTR, STATUS )
      END IF

*  Quality
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK .AND. .NOT. SWAP_1D ) THEN
        CALL BDI_MAPUB( IFID, 'Quality', 'READ', IQPTR, STATUS )
      END IF

*  Get new axis order if NDIM > 2
      IF ( NDIM .GT. 2 ) THEN

*    Tell user about different axes
        CALL MSG_PRNT('Axes in input object are :')
        CALL MSG_PRNT(' ')
        CALL AXIS_TLIST( IFID, NDIM, STATUS )

        NSEL = 0
        CALL PRS_GETLIST( 'SELAX', NDIM, SELAX, NSEL, STATUS )
        IF ( ( STATUS .NE. SAI__OK ) .OR. ( NSEL .EQ. 0 ) ) GOTO 99

      ELSE IF ( NDIM .EQ. 2 ) THEN
        NSEL = 2
        SELAX(1) = 2
        SELAX(2) = 1
      ELSE
        NSEL = 1
      END IF

*  Check contents of swap array
      IF ( NSEL .NE. NDIM ) THEN
        CALL MSG_SETI( 'ND', NDIM )
        CALL MSG_PRNT( 'You must specify the positions of all'/
     :                                           /' ^ND axes' )
      ELSE IF ( SWAP_1D ) THEN
        HTXT = 'Data array and axis values swapped'
        ODIMS(1) = DIMS(1)

      ELSE

*    Donor dimensions not set yet
        CALL ARR_INIT1L( .FALSE., ADI__MXDIM, SPEC, STATUS )

*    Loop over input and create output dimensions
        DO I =1, NSEL
          IF ( ( SELAX(I) .GT. NDIM ) .OR. ( SELAX(I) .LT. 0 )  ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR : No such axis in input'/
     :                                            /' file!', STATUS )
          ELSE IF ( SPEC(I) ) THEN
            CALL MSG_SETI( 'AX', I )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR : Axis ^AX is multiply'/
     :                             /' specified in output', STATUS )
          ELSE
            SPEC(I) = .TRUE.
            ODIMS(I) = DIMS(SELAX(I))
          END IF
          IF ( STATUS .NE. SAI__OK ) GOTO 99
        END DO

*    Make history string
        HTXT = 'New axis order '
        CALL STR_DIMTOC( NSEL, SELAX, HTXT(16:) )

      END IF

*  Define output file
      CALL BDI_LINK( 'BinDS', NDIM, ODIMS, 'REAL', OFID, STATUS )

*  Create and map output data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  One dimensional swap
      IF ( SWAP_1D ) THEN

*    Map input axis
        CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', IAPTR, STATUS )

*    Create irregular output axis and map
        CALL BDI_PUT1R( OFID, 'Data', ODIMS(1), %VAL(IAPTR), STATUS )
        CALL BDI_AXPUT1R( OFID, 1, 'Data', ODIMS(1), %VAL(IDPTR),
     :                    STATUS )

*    Variances become axis widths and vice versa
        IF ( VOK ) THEN

*      Create output widths
          CALL BDI_AXMAPR( OFID, 1, 'Width', 'WRITE', OWPTR, STATUS )

*      Copy and square root
          CALL ARR_COP1R( ODIMS(1), %VAL(IVPTR), %VAL(OWPTR), STATUS )
          CALL ARR_SQRT1R( %VAL(OWPTR), ODIMS(1), STATUS )

        END IF

*    Input widths exist?
        CALL BDI_AXCHK( IFID, 1, 'Width', IWOK, STATUS )
        IF ( IWOK ) THEN

*      Map input widths
          CALL BDI_AXMAPR( OFID, 1, 'Width', 'READ', IWPTR, STATUS )

*      Create output variance
          CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )

*      Copy widths, divide by 2 and square
          CALL ARR_COP1R( ODIMS(1), %VAL(IWPTR), %VAL(OVPTR), STATUS )
          CALL ARR_MULT1R( ODIMS(1), %VAL(OVPTR), 0.5, %VAL(OVPTR),
     :                     STATUS )
          CALL ARR_SQR1R( %VAL(OVPTR), ODIMS(1), STATUS )

        END IF

*    Just copy quality
        IF ( QOK ) THEN
          CALL BDI_COPY( IFID, 'Quality,QualityMask', OFID, ' ',
     :                   STATUS )
        END IF

*    Copy text strings
        CALL BDI_COPY( IFID, 'Label', OFID, 'Axis_1_Label', STATUS )
        CALL BDI_COPY( IFID, 'Units', OFID, 'Axis_1_Units', STATUS )
        CALL BDI_COPY( IFID, 'Axis_1_Label', OFID, 'Label', STATUS )
        CALL BDI_COPY( IFID, 'Axis_1_Units', OFID, 'Units', STATUS )

*  normal swap
      ELSE

*    Copy axes
        DO I = 1, NDIM
          CALL BDI_AXCOPY( IFID, SELAX(I), ' ', OFID, I, STATUS )
        END DO

*    Copy top-level text
        CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )

*    Copy quality mask
        IF ( QOK ) THEN
          CALL BDI_COPY( IFID, 'QualityMask', OFID, ' ', STATUS )
        END IF

*    Map output variance and quality
        IF ( VOK ) THEN
          CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )
        END IF
        IF ( GOK ) THEN
          CALL BDI_MAPI( OFID, 'Grouping', 'WRITE', OGPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', OQPTR, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Pad out dimensions for 7D
        CALL AR7_PAD( NDIM, DIMS, STATUS )
        CALL AR7_PAD( NDIM, ODIMS, STATUS )
        DO I = NDIM + 1, ADI__MXDIM
          SELAX(I) = I
        END DO

*    Swap the appropriate axes
        CALL AR7_AXSWAP_R( DIMS, %VAL(IDPTR), SELAX, ODIMS,
     :                                %VAL(ODPTR), STATUS )
        IF ( VOK ) THEN
          CALL AR7_AXSWAP_R( DIMS, %VAL(IVPTR), SELAX, ODIMS,
     :                                  %VAL(OVPTR), STATUS )
        END IF
        IF ( GOK ) THEN
          CALL AR7_AXSWAP_I( DIMS, %VAL(IGPTR), SELAX, ODIMS,
     :                                  %VAL(OGPTR), STATUS )
        END IF
        IF ( QOK ) THEN
          CALL AR7_AXSWAP_B( DIMS, %VAL(IQPTR), SELAX, ODIMS,
     :                                  %VAL(OQPTR), STATUS )
        END IF

      END IF

*  Copy ancillary stuff
      CALL UDI_COPANC( IFID, 'grf,grp', OFID, STATUS )

*  History
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )
      CALL HSI_PTXT( OFID, 1, HTXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
