      SUBROUTINE SPCONVOL( STATUS )
*+
*  Name:
*     SPCONVOL

*  Purpose:
*     Convolve the spatial response attached to an ASTERIX dataset with
*     a cube or image containing an intrinsic source profile. If a cube
*     is supplied then the energy axis of the cube must agree with that
*     of the spatial response.

*  Language:
*     FORTRAN

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SPCONVOL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Convolves each psf element of a spatial response structure with a
*     user supplied mask. This dataset may be either 2 or 3 dimensional.

*  Usage:
*     {routine_name} {parameter_usage}

*  ADAM Parameters:
*     INP = UNIV (Read)
*        Input dataset whose spatial response will be convolved
*     SOURCE = UNIV (Read)
*        The source model

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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}

*  History:
*     16-Aug-94 (DJA):
*        V1.8-0  Original version.
*     25-NOV-94 (DJA):
*        V1.8-1  User interface now uses USI exclusively
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              		! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants
      INCLUDE 'DAT_PAR'          		! Standard HDS constants
      INCLUDE 'PRM_PAR'				! Standard PRM constants
      INCLUDE 'PSF_PAR'				! Asterix PSF constants
      INCLUDE 'MATH_PAR'			! Asterix MATH constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER*(DAT__SZNAM)  	SRESP         		! Spatial response
							! object name
        PARAMETER 		( SRESP = 'SPATIAL_RESP' )
      INTEGER			MXHLINE			! Max lines of history
        PARAMETER		( MXHLINE = 4 )

*  Local Variables:
      CHARACTER*(DAT__SZLOC) 	ALOC           		! Input ASTERIX structure
      CHARACTER*80		HTXT(MXHLINE)		! History text
      CHARACTER*(DAT__SZLOC) 	ILOC           		! Input dataset
      CHARACTER*(DAT__SZLOC) 	RLOC           		! Response object
      CHARACTER*(DAT__SZLOC) 	SLOC           		! Source profile dataset

      REAL                   	CUTOFF			! Cutoff amplitude

      INTEGER		     	CP_PTR			! Cursor over spatial response
      INTEGER 		     	EBIN      		! Loop over energy axis
      INTEGER			INDX(3)			! Index triplet
      INTEGER                	IPSF           		! PSF handle
      INTEGER			NELM			! Product of dimensions
      INTEGER			NERBIN			! Response energy bin
      INTEGER			NLINE			! Amount of history text
      INTEGER                	NRDIMS(5)        	! Old response dims
      INTEGER			NRDPTR			! New response data
      INTEGER			NRNELM			! Product of dimensions
      INTEGER                	NPSF            	! Number of psfs in response
      INTEGER                	NSPBIN            	! Number of spatial
							! bins in response
      INTEGER                	NUSED            	! Length of compressed
							! response
      INTEGER                	PSFSIZ			! New psf size in bytes
      INTEGER                	RDIMS(5)        	! Old response dims
      INTEGER			RDPTR			! Old response data
      INTEGER			RIPTR			! Old response index
      INTEGER                	RNDIM           	! Dimensionality of response
      INTEGER 		     	SBIN      		! Loop over spatial bins
      INTEGER                	SDIMS(5)        	! Source model dims
      INTEGER			SDPTR			! Source model data
      INTEGER			SEBIN			! Source model E bin
      INTEGER			SEDPTR			! Source model E slice
      INTEGER			SNDIM			! Source model dim'ality
      INTEGER                	X_AX,Y_AX,E_AX,T_AX 	! Axis numbers
      INTEGER			XSMAX, YSMAX		! Psf extreme sizes
      LOGICAL                	EDEP           		! Response is energy dependent?
      LOGICAL                	IPRIM          		! Input primitive?
      LOGICAL                	OK             		! General validity check
      LOGICAL                	RADIAL         		! Psf is only function of R
      LOGICAL                	THERE          		! Component exists?

*  Version
      CHARACTER*30       VERSION
        PARAMETER        ( VERSION = 'SPCONVOL Version 1.8-1' )
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise ASTERIX
      CALL AST_INIT()

*    Version annoucement
      CALL MSG_PRNT( VERSION )

*    Get dataset
      CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, IPRIM, STATUS )

*    Locate ASTERIX structure
      CALL BDA_LOCAST( ILOC, ALOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check to see if response exists
      CALL DAT_THERE( ALOC, SRESP, THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Dataset does not contain a spatial'/
     :          /' response - attach one using SPRESP', STATUS )
        GOTO 99
      END IF

*    Locate spatial response structure
      CALL DAT_FIND( ALOC, SRESP, RLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get intrinsic source profile dataset
      CALL USI_ASSOCI( 'SOURCE', 'READ', SLOC, IPRIM, STATUS )

*    Introduce to the psf system
      CALL PSF_GETSLOT( IPSF, STATUS )
      CALL PSF_CHKAXES( SLOC, IPSF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get its axis identifiers
      CALL PSF_QAXES( IPSF, X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Energy axis must be last at the moment
      IF ( (E_AX .GT. 0) .AND. (E_AX.NE.3) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Energy dependent source models must have'/
     :       /' energy has the 3rd dimension - reorder using AXORDER',
     :       STATUS )
        GOTO 99
      END IF

*    Get intrinsic profile dimensions
      CALL BDA_CHKDATA( SLOC, OK, SNDIM, SDIMS, STATUS )

*    Check source model is pixel centred
      IF ( (((SDIMS(1)/2)*2).EQ.SDIMS(1)) .OR.
     :     (((SDIMS(2)/2)*2).EQ.SDIMS(2)) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Source model must be pixel centred',
     :                STATUS )
        GOTO 99
      END IF

*    Map source profile data
      CALL BDA_MAPDATA( SLOC, 'READ', SDPTR, STATUS )

*    Get expanded response dimensions
      CALL CMP_GET1I( RLOC, 'DIMS', 5, RDIMS, RNDIM, STATUS )

*    Map the index
      CALL CMP_MAPV( RLOC, 'INDEX', '_INTEGER', 'UPDATE',
     :               RIPTR, NELM, STATUS )

*    Map the data
      CALL CMP_MAPV( RLOC, 'DATA_ARRAY', '_REAL', 'READ',
     :               RDPTR, NELM, STATUS )

*    Get radial cutoff
      CALL CMP_GET0R( RLOC, 'CUTOFF', CUTOFF, STATUS )

*    Is it a radial response?
      CALL CMP_GET0L( RLOC, 'RADIAL', RADIAL, STATUS )

*    Is this an energy dependent response?
      IF ( RADIAL ) THEN
        EDEP = (RNDIM.GT.3)
      ELSE
        EDEP = (RNDIM.GT.4)
      END IF

*    Check energy axes are compatible if both the source profile and response
*    are energy dependent
      IF ( (E_AX.GT.0) .AND. EDEP ) THEN

        IF ( SDIMS(E_AX) .NE. RDIMS(RNDIM) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Source model energy dimension differs'/
     :                        /' from that of the response', STATUS )
          GOTO 99
        END IF

*    If the response is not energy dependent, but the source model is, then
*    abort. In the fullness of time we'll do something better here, such as
*    uprating the response to energy dependence.
      ELSE IF ( (E_AX.GT.0) .AND. .NOT. EDEP ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'SPCONVOL cannot yet handle energy '/
     :                /'dependent source models for energy '/
     :                /'independent responses', STATUS )
        GOTO 99
      END IF

*    Create new response dimensions - these are the same as the old ones
*    except the first are are increased to handle the increased size of
*    source profile.
      CALL ARR_COP1I( RNDIM, RDIMS, NRDIMS, STATUS )
      NRDIMS(1) = NRDIMS(1) + (SDIMS(1)/2)*2
      NRDIMS(2) = NRDIMS(2) + (SDIMS(2)/2)*2
      CALL ARR_SUMDIM( RNDIM, NRDIMS, NRNELM )

*    Map workspace for convolved psfs
      CALL DYN_MAPR( RNDIM, NRDIMS, NRDPTR, STATUS )
      CALL ARR_INIT1R( 0.0, NRNELM, %VAL(NRDPTR), STATUS )

*    If not energy dependent source model, pad 3rd dimension
      IF ( E_AX .EQ. 0 ) SDIMS(3) = 1

*    Size of energy axis of response
      IF ( EDEP ) THEN
        NERBIN = NRDIMS(RNDIM)
      ELSE
        NERBIN = 1
        NRDIMS(RNDIM+1) = 1
      END IF

*    Fill it with data
      PSFSIZ = NRDIMS(1)*NRDIMS(2)*VAL__NBR
      CP_PTR = NRDPTR

*    Number of psfs in response
      CALL ARR_SUMDIM( RNDIM-2, NRDIMS(3), NPSF )

*    Number of spatial bins
      NSPBIN = NPSF / NERBIN

*    Loop over energy axis of response
      IPSF = 1
      DO EBIN = 1, NERBIN

*      Choose energy slice of source model if appropriate
        IF ( E_AX .GT. 0 ) THEN
          SEBIN = EBIN
        ELSE
          SEBIN = 1
        END IF

*      Locate slice of source model
        SEDPTR = SDPTR + (SEBIN-1)*SDIMS(1)*SDIMS(2)*VAL__NBR

*      Loop over spatially resolved psfs
        DO SBIN = 1, NSPBIN

*        Locate the index data
          CALL ARR_COP1I( 3, %VAL(RIPTR+3*(IPSF-1)*VAL__NBI),
     :                    INDX, STATUS )

*        The input psf starts at INDX(1) in the response data array, and
*        has size INDX(2)*2+1 by INDX(3)*2+1. Convolve that with the
*        appropriate source model
          CALL SPCONVOL_CONV( INDX(2)*2+1, INDX(3)*2+1, %VAL(RDPTR),
     :                        SDIMS(1), SDIMS(2), %VAL(SEDPTR),
     :                        NRDIMS(1), NRDIMS(2), %VAL(CP_PTR),
     :                        STATUS )

*        Advance to next psf
          IPSF = IPSF + 1
          CP_PTR = CP_PTR + PSFSIZ

        END DO

      END DO

*    Re-compress given CUTOFF. We process the array in memory sequential order
*    which means that we can overwrite the SP_PTR array.
      CALL SPRESP_COMP( NRDIMS(1), NRDIMS(2), NPSF, %VAL(NRDPTR),
     :                  CUTOFF, %VAL(RIPTR), %VAL(NRDPTR),
     :                  NUSED, STATUS )

*    Find maximum sizes used in compressed index. Adjust expanded
*    dimensions so fit the largest psf in the index. Doesn't save
*    any disk space but saves memory in the psf system when the
*    response has to be uncompressed
      CALL SPRESP_SQSH( NPSF, %VAL(RIPTR), XSMAX, YSMAX, STATUS )
      NRDIMS(1) = XSMAX*2 + 1
      NRDIMS(2) = YSMAX*2 + 1

*    Write the expanded dimensions
      CALL HDX_PUTI( RLOC, 'DIMS', RNDIM, NRDIMS, STATUS )

*    Report compression factor
      CALL MSG_SETR( 'FAC', REAL(NRNELM)/REAL(NUSED) )
      CALL MSG_PRNT( 'Response compressed by a factor ^FAC' )

*    Unmap the old data
      CALL CMP_UNMAP( RLOC, 'DATA_ARRAY', STATUS )

*    Write compressed data to file
      CALL HDX_PUTR( RLOC, 'DATA_ARRAY', NUSED, %VAL(NRDPTR), STATUS )

*    Unmap the index
      CALL CMP_UNMAP( RLOC, 'INDEX', STATUS )

*    Add a bit of history
      CALL HIST_ADD( ILOC, VERSION, STATUS )
      HTXT(1) = 'Convolved with : {SOURCE}'
      NLINE = MXHLINE
      CALL USI_TEXT( 1, HTXT, NLINE, STATUS )
      CALL HIST_PTXT( ILOC, NLINE, HTXT, STATUS )

*    Release response
      CALL BDA_RELEASE( SLOC, STATUS )
      CALL DAT_ANNUL( RLOC, STATUS )
      CALL BDA_RELEASE( ILOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE SPCONVOL_CONV( INX, INY, IDATA, MNX, MNY, MASK,
     :                          ONX, ONY, ODATA, STATUS )
*+
*  Name:
*     SPCONVOL_CONV

*  Purpose:
*     Convolve the array IDATA with the mask MASK to produce ODATA. The
*     output array has already been zeroed, and the dimensions are
*     large enough that no checking has to be done for mask overflow.

*  Language:
*     FORTRAN

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SPCONVOL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Attach a spatial response structure to a dataset. A spatial response
*     is stored as a function of X,Y offset from the source position, in
*     either pixel centred or vertex centred form, as a function of detector
*     position and/or energy.
*

*  Usage:
*     {routine_name} {parameter_usage}

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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}

*  History:
*     16-Aug-94 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              		! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Import:
      INTEGER			INX, INY
      REAL			IDATA(INX,INY)
      INTEGER			MNX, MNY
      REAL  			MASK(-MNX/2:MNX/2,-MNY/2:MNY/2)

*  Export:
      INTEGER			ONX, ONY
      REAL			ODATA(ONX,ONY)

*  Local Variables:
      INTEGER			I,J			! Loop over IDATA
      INTEGER			II,JJ			! Loop over MASK
      INTEGER			OX,OY			! IDATA->ODATA offset
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Offsets to bring IDATA into centre of ODATA
      OX = (ONX-INX)/2
      OY = (ONY-INY)/2

*    Loop over input data pixels
      DO J = 1, INY
        DO I = 1, INX

*        Loop over mask
          DO JJ = -MNY/2, MNY/2
            DO II = -MNX/2, MNX/2
              ODATA(I+OX+II,J+OY+JJ) =
     :         ODATA(I+OX+II,J+OY+JJ) + MASK(II,JJ)*IDATA(I,J)
            END DO
          END DO

*      End of loop over input pixels
        END DO
      END DO

      END
