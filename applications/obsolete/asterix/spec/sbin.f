      SUBROUTINE SBIN( STATUS )
*+
*  Name:
*     SBIN

*  Purpose:
*     Rebin a spectrum

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SBIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Rebins a spectrum to give approximately equal population in each bin.

*  Usage:
*     sbin {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input spectrum name
*     OPT = INTEGER (read)
*        Rebinning option
*     NBIN = INTEGER (read)
*        Number of bins for option 1
*     MINVAL = REAL (read)
*        Minimum bin value for option 2
*     OUT = CHAR (read)
*        Output spectrum name

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
*     sbin, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Robert Vallance (ROSAT,University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*        Nov 1992 V1.5-0 (RJV):
*        Original version.
*      6 Nov 1992 V1.5-1 (RJV):
*        Max bin content changed to min (RJV)
*     18 Nov 1993 V1.7-0 (DJA):
*        Added missing AST_INIT call (DJA)
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface (DJA)
*     27 Mar 1995 V1.8-1 (RJV):
*        BIT_ used
*     21 Apr 1995 V1.8-2 (DJA):
*        Updated data interface
*      4 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     11 Mar 1996 V2.0-1 (DJA):
*        Fixed silly quality bug from ADI port
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
        PARAMETER		( VERSION = 'SBIN Version 2.1-0' )

*  Local Variables:
      REAL 			SUM			! Data sum
      REAL 			MINVAL			! Min bin value

      INTEGER			IFID			! Input dataset id
      INTEGER 			NDIM,DIMS(ADI__MXDIM)	! I/p dimensions
      INTEGER 			INVAL,ONVAL		! I/p & o/p sizes
      INTEGER 			IDPTR,IAPTR,IWPTR,	! I/p data, axis, widths
     :                            IVPTR,IQPTR		! variance and quality
      INTEGER 			TDPTR,TAPTR,TWPTR,TVPTR ! Temp data,axis,widths
							! and variance
      INTEGER 			OPT			! Rebinning option
      INTEGER			OFID			! Output dataset id

      BYTE 			MASK			! I/p quality mask

      LOGICAL 			DOK,VOK,QOK		! Input data bits ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input & output files
      CALL USI_ASSOC( 'INP', 'Spectrum', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )

*  Check input
      CALL BDI_CHK( IFID, 'Data', DOK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF (DOK.AND.NDIM.EQ.1) THEN

*    Get pointers to input components
        INVAL = DIMS(1)
        CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
        CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
        IF ( VOK ) THEN
          CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
        END IF
        CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', IAPTR, STATUS )
        CALL BDI_AXMAPR( IFID, 1, 'Width', 'READ', IWPTR, STATUS )

*    Get total counts taking account of any QUALITY
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPUB( IFID, 'Quality', 'READ', IQPTR, STATUS )
          CALL BDI_GET0UB( IFID, 'QualityMask', MASK, STATUS )
          CALL ARR_SUM1RQ( INVAL, %VAL(IDPTR), %VAL(IQPTR), MASK, SUM,
     :                     STATUS )
        ELSE
          CALL ARR_SUM1R( INVAL, %VAL(IDPTR), SUM, STATUS )
        END IF

*    Get some temporary storage for rebined data
        CALL DYN_MAPR( 1, INVAL, TDPTR, STATUS )
        CALL DYN_MAPR( 1, INVAL, TAPTR, STATUS )
        CALL DYN_MAPR( 1, INVAL, TWPTR, STATUS )
        IF ( VOK ) THEN
          CALL DYN_MAPR( 1, INVAL, TVPTR, STATUS )
        END IF

*    Which mode of rebinning
        CALL USI_GET0I( 'OPT', OPT, STATUS )
        IF ( OPT .EQ. 1 ) THEN
          CALL USI_GET0I( 'NBIN', ONVAL, STATUS )
          MINVAL=SUM/REAL(ONVAL)
        ELSE IF ( OPT .EQ. 2 ) THEN
          CALL USI_GET0R( 'MIN', MINVAL, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP(' ','AST_ERR: invalid mode option',STATUS)
        END IF

*    Rebin
        CALL SBIN_DOIT(INVAL,%VAL(IDPTR),%VAL(IAPTR),%VAL(IWPTR),
     :                   VOK,%VAL(IVPTR),QOK,%VAL(IQPTR),MASK,
     :                   MINVAL,ONVAL,%VAL(TDPTR),%VAL(TAPTR),
     :                   %VAL(TWPTR),%VAL(TVPTR),STATUS)

*    Create output object
        CALL BDI_LINK( 'Spectrum', 1, ONVAL, 'REAL', OFID, STATUS )

*    Copy data from temporary storage to output file
        CALL BDI_PUT1R( OFID, 'Data', ONVAL, %val(TDPTR), STATUS )
        IF ( VOK ) THEN
          CALL BDI_PUT1R( OFID, 'Variance', ONVAL, %val(TVPTR), STATUS )
        ENDIF
        CALL BDI_AXPUT1R( OFID, 1, 'Data', ONVAL, %val(TAPTR), STATUS )
        CALL BDI_AXPUT1R( OFID, 1, 'Width', ONVAL, %val(TWPTR), STATUS )
        CALL BDI_AXCOPY( IFID, 1, 'Label,Units', OFID, 1, STATUS )

*    Update history
        CALL HSI_COPY( IFID, OFID, STATUS )
        CALL HSI_ADD( OFID, VERSION, STATUS )

*    Copy ancilliary stuff
        CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )
        CALL UDI_COPANC( IFID, ' ', OFID, STATUS )

*    Release temporary storage
        CALL DYN_UNMAP(TDPTR,STATUS)
        CALL DYN_UNMAP(TAPTR,STATUS)
        CALL DYN_UNMAP(TWPTR,STATUS)
        IF (VOK) THEN
          CALL DYN_UNMAP(TVPTR,STATUS)
        ENDIF

      ENDIF

*  Tidy up
      CALL USI_ANNUL('INP',STATUS)
      CALL USI_ANNUL('OUT',STATUS)
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  SBIN_DOIT
      SUBROUTINE SBIN_DOIT(IN,ID,IA,IW,VOK,IV,QOK,IQ,MASK,MINVAL,
     :                                      ON,OD,OA,OW,OV,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER IN
      REAL ID(*),IA(*),IW(*),IV(*)
      REAL MINVAL
      BYTE IQ(*),MASK
      LOGICAL VOK,QOK
*    Import-Export :
*    Export :
      INTEGER ON
      REAL OD(*),OA(*),OW(*),OV(*)
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      REAL TOT,VARTOT
      REAL LO,HI
      INTEGER I
      LOGICAL GOOD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        ON=0
        I=1

        LO=IA(I)-IW(I)/2.0
        TOT=0.0
        IF (VOK) THEN
          VARTOT=0.0
        ENDIF
        DO WHILE (I.LE.IN)
          IF (QOK) THEN
            GOOD=(BIT_ANDUB(IQ(I),MASK).EQ.QUAL__GOOD)
          ELSE
            GOOD=.TRUE.
          ENDIF
          IF (GOOD) THEN
            TOT=TOT+ID(I)
            IF (VOK) THEN
              VARTOT=VARTOT+IV(I)
            ENDIF

            IF (TOT.GE.MINVAL.OR.I.EQ.IN) THEN
*  output bin
              ON=ON+1
              OD(ON)=TOT
              IF (VOK) THEN
                OV(ON)=VARTOT
              ENDIF
              HI=IA(I)+IW(I)/2.0
              OA(ON)=(HI+LO)/2.0
              OW(ON)=ABS(HI-LO)
              LO=HI
              TOT=0.0
              VARTOT=0.0
            ENDIF

          ENDIF

          I=I+1

        ENDDO

      ENDIF

      END
