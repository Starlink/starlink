      SUBROUTINE ERI1_WRITRMF( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI1_WRITRMF

*  Purpose:
*     Write simple energy response to a HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITRMF( NARG, ARGS, OARG, STATUS )

*  Description:
*     This method writes simple format energy respones to a HDS file.
*     Such a response is simply a 2D array of values. It writes only
*     that information which is contained within the RMFID passed as
*     the second argument. Other data of interest must be written by
*     surrounding methods which have access to the dataset to which
*     this response is "attached".
*
*     The response is written in one of two formats. If the input
*     compression method is NONE, the the response is written as a simple
*     2D array in a fixed size BINTABLE extension. If the method is ASTERIX
*     or OGIP_CMP then the response is written to a variable field size
*     BINTABLE.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     ADI:
*        ADI2_POGIPK	- Write OGIP classification keywords

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER                   STATUS                  ! Global status

*  External References:
      EXTERNAL			ADI2_POGIPK

*  Local Variables:
      CHARACTER*5		STR			! NCHAN in characters
      CHARACTER*8		TTYPE(3)		! Column names
      CHARACTER*8		TFORM(3)		! Column types
      CHARACTER*3		TUNIT(3)		! Column units

      INTEGER			EBPTR			! Energy bounds ptr
      INTEGER			FID			! FITSfile object
      INTEGER			FSTAT			! FITSIO status
      INTEGER			IDX			! Response number
      INTEGER			NRESP			! # responses in file
      INTEGER			LUN			! Logical unit
      INTEGER			NCHAN			! # channel bins
      INTEGER			NDIG			! Chars used in STR
      INTEGER			NENER			! # energy bins
      INTEGER			RMFID			! Response object
      INTEGER			RPTR			! Mapped RMF

*  Local data;
      DATA    TUNIT/'keV', 'keV', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      FID = ARGS(1)
      CALL ADI_CGET0I( ARGS(2), IDX, STATUS )
      CALL ADI_CGET0I( ARGS(3), NRESP, STATUS )
      RMFID = ARGS(4)

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Get file's logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Write keywords rather than fields for the N_GRP, F_CHAN and N_CHAN
*  fields, as their values are constant
      CALL ADI2_PKEY0I( FID, 'MATRIX', 'N_GRP', 1, ' ', STATUS )
      CALL ADI2_PKEY0I( FID, 'MATRIX', 'F_CHAN', 1, ' ', STATUS )
      CALL ADI2_PKEY0I( FID, 'MATRIX', 'N_CHAN', NCHAN, ' ', STATUS )

*  Construct the field descriptions for the BINTABLE
      TTYPE(1) = 'ENERG_LO'
      TFORM(1) = '1E'
      TTYPE(2) = 'ENERG_HI'
      TFORM(2) = '1E'
      TTYPE(3) = 'MATRIX'
      CALL CHR_ITOC( NCHAN, STR, NDIG )
      TFORM(3) = STR(:NDIG)//'E'

*  Define the HDU data area
      CALL ADI2_DEFBTB( FID, 'MATRIX', NENER, 3, TTYPE, TFORM,
     :                  TUNIT, 0, STATUS )

*  Write keywords for response extension
      CALL ADI2_POGIPK( FID, 'MATRIX', 'RESPONSE', '1.0.0',
     :                  'RSP_MATRIX', '1.1.0', 'REDIST', ' ', STATUS )

*  Write the energy boundaries into the table
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )
      FSTAT = 0
      CALL FTPCLE( LUN, 1, 1, 1, NENER, %VAL(EBPTR), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NENER, %VAL(EBPTR+4), FSTAT )
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )

*  Map the matrix data
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )

*  Simply write the data
      CALL FTPCLE( LUN, 3, 1, 1, NENER*NCHAN, %VAL(RPTR), STATUS )

*  Release the matrix data
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_WRITRMF', STATUS )
      END IF

      END



      SUBROUTINE ERI1_WRITE_OGIP_RMFARF( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI1_WRITE_OGIP_RMFARF

*  Purpose:
*     Write OGIP format energy response with ancillary response to an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITE_OGIP_RMFARF( NARG, ARGS, OARG, STATUS )

*  Description:
*     Writes the RMF component to HDS. Remaps response array and scales
*     each value by the corresponding value in the ancillary response.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     ADI:
*        ADI2_POGIPK	- Write OGIP classification keywords

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'ADI_PAR'					! ADI constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ELOC			! ENERGY_RESP object
      CHARACTER*(DAT__SZLOC)	ENLOC			! .ENERGY
      CHARACTER*(DAT__SZLOC)	RELOC			! .RESPONSE

      INTEGER			EIPTR			! Energy indices
      INTEGER			NENER			! # energy bins
      INTEGER			NMAP			! # mapped elements
      INTEGER			RPTR			! Mapped RMF
      INTEGER			SCPTR			! Mapped ARF values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the RMF bit by calling the method routine for RMF alone
      CALL ERI1_WRITRMF_OGIP( 4, ARGS, OARG, STATUS )

*  Locate the response
      CALL ERI1_LOCRESP( ARGS, .FALSE., ELOC, STATUS )

*  Now fold in the scale factors contained in the ancillary response. For
*  this we need the response values and their energy indices.
      CALL DAT_FIND( ELOC, 'ENERGY', ENLOC, STATUS )
      CALL DAT_FIND( ELOC, 'RESPONSE', RELOC, STATUS )
      CALL CMP_MAPV( ENLOC, 'DATA_ARRAY', '_INTEGER', 'READ', EIPTR,
     :               NMAP, STATUS )
      CALL CMP_MAPV( RELOC, 'DATA_ARRAY', '_REAL', 'UPDATE', RPTR,
     :               NMAP, STATUS )

*  Map the scale factors in the ancillary response
      CALL ADI_CGET0I( ARGS(5), 'NENERGY', NENER, STATUS )
      CALL ADI_CMAPR( ARGS(5), 'Response', 'READ', SCPTR, STATUS )

*  Scale the response
      CALL ERI1_WRITE_OGIP_RMFARF1( NENER, %VAL(SCPTR), NMAP,
     :                      %VAL(EIPTR), %VAL(RPTR), STATUS )

*  Release ADI object
      CALL ADI_CUNMAP( ARGS(5), 'Response', SCPTR, STATUS )

*  Free the HDS objects we've mapped and found
      CALL CMP_UNMAP( ENLOC, 'DATA_ARRAY', STATUS )
      CALL CMP_UNMAP( RELOC, 'DATA_ARRAY', STATUS )
      CALL DAT_ANNUL( ENLOC, STATUS )
      CALL DAT_ANNUL( RELOC, STATUS )
      CALL DAT_ANNUL( ELOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_WRITE_OGIP_RMFARF', STATUS )
      END IF

      END

      SUBROUTINE ERI1_WRITE_OGIP_RMFARF1( NE, SCALE, NRMF, EIND, RSP,
     :               STATUS )
      IMPLICIT NONE
      INTEGER NE,NRMF,EIND(*),STATUS
      REAL	SCALE(NE), RSP(*)
      INTEGER I
      IF ( STATUS .EQ. 0 ) THEN
      DO I = 1, NRMF
         RSP(I) = RSP(I) * SCALE(EIND(I))
      END DO
      END IF

      END



      SUBROUTINE ERI1_WRITRMF_OGIP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI1_WRITRMF_OGIP

*  Purpose:
*     Write OGIP format energy response to an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITRMF_OGIP( NARG, ARGS, OARG, STATUS )

*  Description:
*     This method provides the low level mechanism of writing a energy
*     response structure to a dataset. It writes only that information
*     which is contained within the RMFID passed as the second argument.
*     Other data of interest must be written by surrounding methods
*     which have access to the dataset to which this response is
*     "attached".
*
*     The response is written in one of two formats. If the input
*     compression method is NONE, the the response is written as a simple
*     2D array in a fixed size BINTABLE extension. If the method is ASTERIX
*     or OGIP_CMP then the response is written to a variable field size
*     BINTABLE.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     ADI:
*        ADI2_POGIPK	- Write OGIP classification keywords

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'ADI_PAR'					! ADI constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER                   STATUS                  ! Global status

*  External References:
      EXTERNAL			ADI2_POGIPK

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ELOC			! ENERGY_RESP object
      CHARACTER*(DAT__SZLOC)	CHLOC			! .CHANNEL
      CHARACTER*(DAT__SZLOC)	ENLOC			! .ENERGY
      CHARACTER*(DAT__SZLOC)	RELOC			! .RESPONSE

      INTEGER			CIPTR			! Channel indices
      INTEGER			CSPTR			! Channel spec ptr
      INTEGER			EIPTR			! Energy indices
      INTEGER			FCPTR			! F_chan data
      INTEGER			NCHAN			! # channel bins
      INTEGER			NCPTR			! N_chan data
      INTEGER			NENER			! # energy bins
      INTEGER			NGPTR			! N_grp data
      INTEGER			NMAP			! # mapped elements
      INTEGER			NRMF			! # response elements
      INTEGER			RMFID			! Response object

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      RMFID = ARGS(4)

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Locate the response
      CALL ERI1_LOCRESP( ARGS, .TRUE., ELOC, STATUS )

*  Ensure the three structures are present in ELOC
      CALL DAT_THERE( ELOC, 'ENERGY', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'ENERGY', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'ENERGY', ENLOC, STATUS )
      CALL DAT_THERE( ELOC, 'CHANNEL', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'CHANNEL', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'CHANNEL', CHLOC, STATUS )
      CALL DAT_THERE( ELOC, 'RESPONSE', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'RESPONSE', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'RESPONSE', RELOC, STATUS )

*  Write energy bounds
      CALL ADI1_CCA2HR( RMFID, 'Energy', ENLOC, 'ENERGY_BOUNDS',
     :                  STATUS )

*  Invent the ENERGY_SPEC array by taking the centre values of the bounds
      CALL ERI1_WRITRMF_EB2C( RMFID, NENER, ENLOC, STATUS )

*  Write the channel bounds
      CALL ADI1_CCA2HR( RMFID, 'Channels', CHLOC, 'CHANNEL_BOUNDS',
     :                  STATUS )

*  OGIP responses are always 1 channel wide, so create a dummy CHANNEL_SPEC
*  array
      CALL DAT_NEW1R( CHLOC, 'CHANNEL_SPEC', NCHAN, STATUS )
      CALL CMP_MAPV( CHLOC, 'CHANNEL_SPEC', '_REAL', 'WRITE', CSPTR,
     :               NMAP, STATUS )
      CALL ARR_REG1R( 1.0, 1.0, NCHAN, %VAL(CSPTR), STATUS )
      CALL CMP_UNMAP( CHLOC, 'CHANNEL_SPEC', STATUS )

*  Write the response values
      CALL ADI1_CCA2HR( RMFID, 'RMF', RELOC, 'DATA_ARRAY', STATUS )

*  Convert the N_grp, F_chan and N_chan members to energy and channel
*  indices. Create the latter as _WORD arrays, but use integer in code
*  for portability
      CALL ADI_CMAPI( RMFID, 'Ngrp', 'READ', NGPTR, STATUS )
      CALL ADI_CMAPI( RMFID, 'Fchan', 'READ', FCPTR, STATUS )
      CALL ADI_CMAPI( RMFID, 'Nchan', 'READ', NCPTR, STATUS )
      CALL ADI_CSIZE( RMFID, 'RMF', NRMF, STATUS )
      CALL DAT_NEW( ENLOC, 'DATA_ARRAY', '_WORD', 1, NRMF, STATUS )
      CALL CMP_MAPV( ENLOC, 'DATA_ARRAY', '_INTEGER', 'WRITE',
     :               EIPTR, NMAP, STATUS )
      CALL DAT_NEW( CHLOC, 'DATA_ARRAY', '_WORD', 1, NRMF, STATUS )
      CALL CMP_MAPV( CHLOC, 'DATA_ARRAY', '_INTEGER', 'WRITE',
     :               CIPTR, NMAP, STATUS )
      CALL ERI1_WRITRMF_OGIP1( NENER, %VAL(NGPTR), %VAL(FCPTR),
     :                         %VAL(NCPTR), %VAL(EIPTR),
     :                         %VAL(CIPTR), STATUS )
      CALL ADI_CUNMAP( RMFID, 'Nchan', NCPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'Fchan', FCPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'Ngrp', NGPTR, STATUS )

*  Free the response object
      CALL CMP_UNMAP( ENLOC, 'DATA_ARRAY', STATUS )
      CALL CMP_UNMAP( CHLOC, 'DATA_ARRAY', STATUS )
      CALL DAT_ANNUL( RELOC, STATUS )
      CALL DAT_ANNUL( CHLOC, STATUS )
      CALL DAT_ANNUL( ENLOC, STATUS )
      CALL DAT_ANNUL( ELOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_WRITRMF_OGIP', STATUS )
      END IF

      END

      SUBROUTINE ERI1_WRITRMF_OGIP1( NE, NGRP, FC, NC,
     :                               EI, CI, STATUS )
      INTEGER NE,NGRP(*),FC(*),NC(*),EI(*),CI(*),STATUS
	INTEGER I,IG,R

      IG = 1
      R =  1
      DO I = 1, NE
        DO J = 1, NGRP(I)
          DO K = 1, NC(IG)
            EI(R) = I
            CI(R) = FC(IG) + K - 1
            R = R + 1
          END DO
          IG = IG + 1
        END DO
      END DO

      END


      SUBROUTINE ERI1_WRITRMF_AST( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI1_WRITRMF_AST

*  Purpose:
*     Write ASTERIX type energy response to a HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITRMF_AST( NARG, ARGS, OARG, STATUS )

*  Description:
*     This method provides the low level mechanism of writing a energy
*     response structure to a dataset. It writes only that information
*     which is contained within the RMFID passed as the second argument.
*     Other data of interest must be written by surrounding methods
*     which have access to the dataset to which this response is
*     "attached".

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     ADI:
*        ADI2_POGIPK	- Write OGIP classification keywords

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ELOC			! ENERGY_RESP object
      CHARACTER*(DAT__SZLOC)	CHLOC			! .CHANNEL
      CHARACTER*(DAT__SZLOC)	ENLOC			! .ENERGY
      CHARACTER*(DAT__SZLOC)	RELOC			! .RESPONSE

      INTEGER			NCHAN			! # channel bins
      INTEGER			NENER			! # energy bins
      INTEGER			RMFID			! Response object

      LOGICAL			THERE			!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      RMFID = ARGS(4)

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Locate the response
      CALL ERI1_LOCRESP( ARGS, .TRUE., ELOC, STATUS )

*  Ensure the three structures are present in ELOC
      CALL DAT_THERE( ELOC, 'ENERGY', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'ENERGY', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'ENERGY', ENLOC, STATUS )
      CALL DAT_THERE( ELOC, 'CHANNEL', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'CHANNEL', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'CHANNEL', CHLOC, STATUS )
      CALL DAT_THERE( ELOC, 'RESPONSE', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL DAT_NEW( ELOC, 'RESPONSE', 'LIST', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( ELOC, 'RESPONSE', RELOC, STATUS )

*  Write the arrays
      CALL ADI1_CCA2HR( RMFID, 'Energy', ENLOC, 'ENERGY_BOUNDS',
     :                  STATUS )
      CALL ADI1_CCA2HI( RMFID, 'EnergyIndices', ENLOC, 'DATA_ARRAY',
     :                  STATUS )
      CALL ADI1_CCA2HR( RMFID, 'Channels', CHLOC, 'CHANNEL_BOUNDS',
     :                  STATUS )
      CALL ADI1_CCA2HI( RMFID, 'ChannelIndices', CHLOC, 'DATA_ARRAY',
     :                  STATUS )
      CALL ADI1_CCA2HR( RMFID, 'ChannelSpec', CHLOC, 'CHANNEL_SPEC',
     :                  STATUS )
      CALL ADI1_CCA2HR( RMFID, 'RMF', RELOC, 'DATA_ARRAY', STATUS )

*  Invent the ENERGY_SPEC array by taking the centre values of the bounds
      CALL ERI1_WRITRMF_EB2C( RMFID, NENER, ENLOC, STATUS )

*  Free the response object
      CALL DAT_ANNUL( ENLOC, STATUS )
      CALL DAT_ANNUL( CHLOC, STATUS )
      CALL DAT_ANNUL( RELOC, STATUS )
      CALL DAT_ANNUL( ELOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_WRITRMF_AST', STATUS )
      END IF

      END



      SUBROUTINE ERI1_WRITRMF_AST1( FID, NE, MAXE, NCH, NRMF, EBND, CI,
     :                              EI, RSP, WRK1, WRK2, STATUS )
*+
*  Name:
*     ERI1_WRITRMF_AST1

*  Purpose:
*     Write ASTERIX energy response elements to a HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITRMF_AST1( FID, NE, NCH, NRMF, EBND, CI, EI,
*                             RSP, WRK1, WRK2, STATUS )

*  Description:
*

*  Arguments:
*     LUN = INTEGER (given)
*        Logical unit for output
*     NE = INTEGER (given)
*        Number of energy bins (ie number of rows in table)
*     NCH = INTEGER (given)
*        Number of channel bins
*     NRMF = INTEGER (given)
*        Number of specified response elements
*     EBND[] = REAL (given)
*        Energy bounds array
*     CI[] = INTEGER (given)
*        Channel indices of non-zero elements
*     EI[] = INTEGER (given)
*        Energy indices of non-zero elements
*     RSP[] = REAL (given)
*        Response elements
*     WRK1[] = INTEGER (given)
*        Workspace as big as NE
*     WRK2[] = INTEGER (given)
*        Workspace as big as NRMF
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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			FID			! See above
      INTEGER			NE, MAXE, NCH, NRMF     !
      REAL			EBND(*)
      INTEGER			CI(*), EI(*)		!
      REAL			RSP(*)			!
      INTEGER			WRK1(NE,2), WRK2(NRMF,2)

*  Status:
      INTEGER                   STATUS                  ! Global status

*  External References:
      EXTERNAL			ADI2_POGIPK

*  Local Variables:
      CHARACTER*10		STR
      CHARACTER*8		TTYPE(6)		! Column names
      CHARACTER*8		TFORM(6)		! Column types
      CHARACTER*3		TUNIT(6)		! Column units

      INTEGER			ACTHEAP			! Actual heap size
      INTEGER			CS
      INTEGER			E			! Loop over energy
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			!
      INTEGER			LASTR			!
      INTEGER			LC			! Last channel bin
      INTEGER			LE			! Last energy bin
      INTEGER			LUN			! Logical unit
      INTEGER			MAX_NGRP		! Max value of N_GRP
      INTEGER			MAX_SMAT		! Max width of matrix
      INTEGER			NDIG			!
      INTEGER			NFIXED			!
      INTEGER			NS			! Number of subsets
      INTEGER			R			! Loop over RMF
      INTEGER			SMAT			! Elements != 0 in row

      LOGICAL			FNVAR, MATVAR		! Use variable lengths?

*  Local data;
      DATA    TUNIT/'keV', 'keV', ' ', ' ', ' ', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0
      DO E = 1, NE
        WRK1(E,1) = 0
      END DO

*  Get file's logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Count number of channel subsets
      R = 1
      LE = -1
      LC = -1
      NS = 0
      DO WHILE ( R .LE. NRMF )

*    Same energy as before?
        IF ( EI(R) .EQ. LE ) THEN

*      Channel index has not advanced by one?
          IF ( CI(R) .NE. (LC+1) ) THEN

*        Increment number of subsets in this energy bin
            WRK1(LE,1) = WRK1(LE,1) + 1

*      Mark length of current subset
            IF ( NS .GT. 0 ) WRK2(NS,2) = CI(R-1) - WRK2(NS,1) + 1

*        Start new subset
            NS = NS + 1
            WRK2(NS,1) = CI(R)

          END IF

        ELSE

*      Mark length of current subset
          WRK2(NS,2) = CI(R-1) - WRK2(NS,1) + 1

*      Advance to next energy bin, start new subset
          LE = EI(R)

          NS = NS + 1
          WRK1(LE,1) = 1
          WRK1(LE,2) = R
          WRK2(NS,1) = CI(R)

        END IF

*    Next element
        LC = CI(R)
        R = R + 1

      END DO
      WRK2(NS,2) = CI(NRMF) - WRK2(NS,1) + 1

*  Find maximum size of N_GRP
      MAX_NGRP = 0
      MAX_SMAT = 0
      DO E = 1, NE
        MAX_NGRP = MAX( MAX_NGRP, WRK1(E,1) )
        IF ( (WRK1(E,1) .EQ. 0) .OR. (E.GT.LE) ) THEN
          SMAT = 0
        ELSE IF ( E .EQ. LE ) THEN
          SMAT = NRMF - WRK1(E,2)
        ELSE
          SMAT = WRK1(E+1,2) - WRK1(E,2)
        END IF
        MAX_SMAT = MAX( MAX_SMAT, SMAT )
      END DO

*  Use a variable length array for the F_CHAN and N_CHAN columns? Six is
*  factor by which a single element of F_CHAN or N_CHAN is smaller than
*  the descriptor required to store a variable length array element.
      NFIXED = MAX_NGRP * MAXE
      IF ( (MAX_NGRP .GT. 6) .AND. (NS .NE. NFIXED) .AND.
     :                              (NS .LE. 6*NFIXED) ) THEN
        ACTHEAP = 2*2*NS
        FNVAR = .TRUE.
      ELSE
        ACTHEAP = 0
        FNVAR = .FALSE.
      END IF

*  Use a variable length array for the matrix column? Three is the
*  factor by which a single element of the response is smaller than
*  the descriptor required to store a variable length array element.
      NFIXED = MAX_SMAT * MAXE
      IF ( (MAX_SMAT .GT. 3) .AND. (NRMF .NE. NFIXED) .AND.
     :                              (NRMF .le. 3* NFIXED) ) THEN
        ACTHEAP = ACTHEAP + 4*NRMF
        MATVAR = .TRUE.
      ELSE
        MATVAR = .FALSE.
      END IF

*  Construct the field descriptions for the BINTABLE
      TTYPE(1) = 'ENERG_LO'
      TFORM(1) = '1E'
      TTYPE(2) = 'ENERG_HI'
      TFORM(2) = '1E'
      TTYPE(3) = 'N_GRP'
      TFORM(3) = '1I'
      TTYPE(4) = 'F_CHAN'
      TTYPE(5) = 'N_CHAN'
      IF ( FNVAR ) THEN
        TFORM(4) = 'PI'
        TFORM(5) = 'PI'
      ELSE
        CALL CHR_ITOC( MAX_NGRP, STR, NDIG )
        TFORM(4) = STR(:NDIG)//'I'
        TFORM(5) = STR(:NDIG)//'I'
      END IF
      TTYPE(6) = 'MATRIX'
      IF ( MATVAR ) THEN
        TFORM(6) = 'PE'
      ELSE
        CALL CHR_ITOC( MAX_SMAT, STR, NDIG )
        TFORM(6) = STR(:NDIG)//'E'
      END IF

*  Define the HDU data area
      CALL ADI2_DEFBTB( FID, 'MATRIX', MAXE, 6, TTYPE, TFORM,
     :                  TUNIT, ACTHEAP, STATUS )

*  Other mandatory keywords
      CALL ADI2_PKEY0I( FID, 'MATRIX', 'DETCHANS', NCH,
     :       'Total number of raw PHA channels', STATUS )

*  Write keywords for response extension
      CALL ADI2_POGIPK( FID, 'MATRIX', 'RESPONSE', '1.0.0',
     :                  'RSP_MATRIX', '1.1.0', 'REDIST', ' ', STATUS )

*  Write energy lower and upper bounds
      CALL FTPCLE( LUN, 1, 1, 1, MAXE, EBND(1), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, MAXE, EBND(2), FSTAT )

*  The N_GRP field
      CALL FTPCLJ( LUN, 3, 1, 1, MAXE, WRK1(1,1), FSTAT )

*  Write the table data
      R = 1
      CS = 1
      DO E = 1, MAXE

*    Subsets in this energy bin?
        IF ( WRK1(E,1) .GT. 0 ) THEN

*      Write the F_CHAN fields
          CALL FTPCLJ( LUN, 4, E, 1, WRK1(E,1), WRK2(CS,1), FSTAT )
          IF ( .NOT. FNVAR ) THEN
            DO I = WRK1(E,1) + 1, MAX_NGRP
              CALL FTPCLJ( LUN, 4, E, 1, I, 0, FSTAT )
            END DO
          END IF

*      Write the N_CHAN field
          CALL FTPCLJ( LUN, 5, E, 1, WRK1(E,1), WRK2(CS,2), FSTAT )
          IF ( .NOT. FNVAR ) THEN
            DO I = WRK1(E,1) + 1, MAX_NGRP
              CALL FTPCLJ( LUN, 5, E, 1, I, 0, FSTAT )
            END DO
          END IF

*      Write the channel values
          IF ( E .LE. LE ) THEN
            IF ( E .EQ. LE ) THEN
              LASTR = NRMF
            ELSE
              LASTR = WRK1(E+1,2)
            END IF
            CALL FTPCLE( LUN, 6, E, 1, LASTR - R + 1, RSP(R), FSTAT )
            IF ( .NOT. MATVAR ) THEN
              DO I = (LASTR - R + 1) + 1, MAX_SMAT
                CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
              END DO
            END IF
          ELSE
            IF ( .NOT. MATVAR ) THEN
              DO I = 1, MAX_SMAT
                CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
              END DO
            END IF
          END IF

*      Advance the element counter
          R = LASTR + 1
          CS = CS + WRK1(E,1)

*    Fill in zeroes if not vector columns
        ELSE IF ( .NOT. FNVAR ) THEN

          DO I = 1, MAX_NGRP
            CALL FTPCLJ( LUN, 4, E, 1, I, 0, FSTAT )
            CALL FTPCLJ( LUN, 5, E, 1, I, 0, FSTAT )
          END DO
          IF ( .NOT. MATVAR ) THEN
            DO I = 1, MAX_SMAT
              CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
            END DO
          END IF

        END IF

      END DO

*   Define the length of data area properly
      CALL ADI2_DEFHP( FID, 'MATRIX', ACTHEAP, STATUS )

      END



      SUBROUTINE ERI1_WRITRMF_CEBND( FID, RMFID, STATUS )
*+
*  Name:
*     ERI1_WRITRMF_CEBND

*  Purpose:
*     Write channel energy bounds to EBOUNDS extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_WRITRMF_CEBND( FID, RMFID, STATUS )

*  Description:
*

*  Arguments:
*     FID = INTEGER (given)
*        FITSfile object to which bounds will be written
*     RMFID = INTEGER (given)
*        Response containing
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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			FID			! See above
      INTEGER			RMFID			!

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      CHARACTER*8		TTYPE(3)		! Column names
      CHARACTER*8		TFORM(3)		! Column types
      CHARACTER*3		TUNIT(3)		! Column units

      INTEGER			CBPTR			! Channel energies
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			!
      INTEGER			LUN			! Logical unit
      INTEGER			NCHAN			! Number of channels

      LOGICAL			THERE			! Bounds specified?

*  Local data:
      DATA    			TUNIT/' ', ' ', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of channels
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )

*  Does the Channels member exist?
      CALL ADI_THERE( RMFID, 'Channels', THERE, STATUS )
      IF ( THERE ) THEN

*    Other mandatory keywords
        CALL ADI2_PKEY0I( FID, 'EBOUNDS', 'DETCHANS', NCHAN,
     :          'Total number of raw PHA channels', STATUS )

*    Get file's logical unit
        CALL ADI2_GETLUN( FID, LUN, STATUS )

*    Construct the field descriptions for the BINTABLE
        TTYPE(1) = 'CHANNEL'
        TFORM(1) = '1I'
        TTYPE(2) = 'E_MIN'
        TFORM(2) = '1E'
        TTYPE(3) = 'E_MAX'
        TFORM(3) = '1E'

*    Define the HDU data area
        CALL ADI2_DEFBTB( FID, 'EBOUNDS', NCHAN, 3, TTYPE, TFORM,
     :                    TUNIT, 0, STATUS )

*    Other mandatory keywords
        CALL ADI2_PKEY0I( FID, 'EBOUNDS', 'DETCHANS', NCHAN,
     :          'Total number of raw PHA channels', STATUS )

*    Write the channels column
        FSTAT = 0
        DO I = 1, NCHAN
          CALL FTPCLJ( LUN, 1, I, 1, 1, I, FSTAT )
        END DO

*  Map the channel bounds array
        CALL ADI_CMAPR( RMFID, 'Channels', 'READ', CBPTR, STATUS )

*  Write the bounds columns
        CALL FTPCLE( LUN, 2, 1, 1, NCHAN, %VAL(CBPTR), FSTAT )
        CALL FTPCLE( LUN, 3, 1, 1, NCHAN, %VAL(CBPTR+4), FSTAT )

*  End of Channels presence test
      END IF

      END



      SUBROUTINE ERI1_WRITRMF_EB2C( RMFID, NE, ENLOC, STATUS )
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'

      INTEGER	RMFID
      CHARACTER*(DAT__SZLOC)	ENLOC
	INTEGER NE, STATUS, NMAP
      INTEGER	EBPTR, ESPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Map the energy bounds
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )

*  Create and map the ENERGY_SPEC array
      CALL DAT_NEW1R( ENLOC, 'ENERGY_SPEC', NE, STATUS )
      CALL CMP_MAPV( ENLOC, 'ENERGY_SPEC', '_REAL', 'WRITE', ESPTR,
     :               NMAP, STATUS )

*  Convert bounds to centres
      CALL ERI1_WRITRMF_EB2C_INT( NE, %VAL(EBPTR), %VAL(ESPTR),
     :                            STATUS )

*  Release ADI and HDS objects
      CALL CMP_UNMAP( ENLOC, 'ENERGY_SPEC', STATUS )
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI1_WRITRMF_EB2C', STATUS )
      END IF

      END


      SUBROUTINE ERI1_WRITRMF_EB2C_INT( NE, BNDS, CEN, STATUS )
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

	INTEGER NE, STATUS,I
      REAL	CEN(*),BNDS(*)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NE
        CEN(I) = (BNDS(I)+BNDS(I+1))/2.0
      END DO

      END


      SUBROUTINE ERI1_LOCRESP( ARGS, CREATE, ELOC, STATUS )
*+
*  Name:
*     ERI1_LOCRESP

*  Purpose:
*     Locate response structure, creating if specified

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI1_LOCRESP( ARGS, CREATE, ELOC, STATUS )

*  Description:
*     This method writes simple format energy respones to a HDS file.
*     Such a response is simply a 2D array of values. It writes only
*     that information which is contained within the RMFID passed as
*     the second argument. Other data of interest must be written by
*     surrounding methods which have access to the dataset to which
*     this response is "attached".
*
*     The response is written in one of two formats. If the input
*     compression method is NONE, the the response is written as a simple
*     2D array in a fixed size BINTABLE extension. If the method is ASTERIX
*     or OGIP_CMP then the response is written to a variable field size
*     BINTABLE.

*  Arguments:
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     CREATE = LOGICAL (given)
*        Create response if it doesn't exist
*     ELOC = CHARACTER*(DAT__SZLOC) (returned)
*        Energy response locator
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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! ADI constants

*  Arguments Given:
      INTEGER                   ARGS(*)
      LOGICAL			CREATE

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	ELOC

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ALOC			! File ASTERIX box
      CHARACTER*(DAT__SZLOC)	ECLOC			! Vector response
      CHARACTER*(DAT__SZLOC)	LOC			! File locator
      CHARACTER*(DAT__SZNAM)	NAME			! File object name

      INTEGER			IDX			! Response number
      INTEGER			NRESP			! # responses in file

      LOGICAL			THERE			! Response exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the HDS object. If it is not ENERGY_RESP, then we assume
*  we are in an NDF, so MORE.ASTERIX.ENERGY_RESP must be present
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )
      CALL DAT_NAME( LOC, NAME, STATUS )
      IF ( NAME(1:11) .EQ. 'ENERGY_RESP' ) THEN
        CALL DAT_CLONE( LOC, ELOC, STATUS )
      ELSE

*    Locate ASTERIX box, creating if we're allowed to do so
        CALL ADI1_LOCAST( ARGS(1), CREATE, ALOC, STATUS )

*    Extract the response number
        CALL ADI_GET0I( ARGS(3), NRESP, STATUS )

*    Does ENERGY_RESP exist?
        CALL DAT_THERE( ALOC, 'ENERGY_RESP', THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_FIND( ALOC, 'ENERGY_RESP', ELOC, STATUS )

*    Create if allowed to do so
        ELSE IF ( CREATE ) THEN

*      Create as vector if more than one response in file
          IF ( NRESP .GT. 1 ) THEN
            CALL DAT_NEW( ALOC, 'ENERGY_RESP', 'EXT', 1, NRESP, STATUS )
          ELSE
            CALL DAT_NEW( ALOC, 'ENERGY_RESP', 'EXT', 0, 0, STATUS )
          END IF
          CALL DAT_FIND( ALOC, 'ENERGY_RESP', ELOC, STATUS )

        END IF

*    Create as vector if more than one response in file
        IF ( NRESP .GT. 1 ) THEN

*      Get the number of the response we want
          CALL ADI_GET0I( ARGS(2), IDX, STATUS )
          ECLOC = ELOC
          CALL DAT_CELL( ECLOC, 1, IDX, ELOC, STATUS )
          CALL DAT_ANNUL( ECLOC, STATUS )
        END IF

      END IF

      END
