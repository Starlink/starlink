      SUBROUTINE ERI2_WRITRMF( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI2_WRITRMF

*  Purpose:
*     Write simple energy response to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_WRITRMF( NARG, ARGS, OARG, STATUS )

*  Description:
*     This method writes simple format energy respones to a FITS file.
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
      INCLUDE 'PRM_PAR'

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
      INTEGER			LUN			! Logical unit
      INTEGER			NCHAN			! # channel bins
      INTEGER			NDIG			! Chars used in STR
      INTEGER			NENER			! # energy bins
      INTEGER			RMFID			! Response object
      INTEGER			RPTR			! Mapped RMF
      INTEGER			I
      INTEGER			CRPTR
      INTEGER			BWIDTH			! Width of binary table in bytes

*  Local data;
      DATA    TUNIT/'keV', 'keV', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      FID = ARGS(1)
      RMFID = ARGS(2)

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
      BWIDTH = 0
      TTYPE(1) = 'ENERG_LO'
      TFORM(1) = '1E'
      BWIDTH = BWIDTH + 4
      TTYPE(2) = 'ENERG_HI'
      TFORM(2) = '1E'
      BWIDTH = BWIDTH + 4
      TTYPE(3) = 'MATRIX'
      CALL CHR_ITOC( NCHAN, STR, NDIG )
      TFORM(3) = STR(:NDIG)//'E'
      BWIDTH = BWIDTH + (NCHAN * 4)

*  Define the HDU data area
      CALL ADI2_MKBTB( FID, 'MATRIX', NENER, 3, BWIDTH,
     :                 TTYPE, TFORM, TUNIT, 0, STATUS )

*  Write keywords for response extension
      CALL ADI2_POGIPK( FID, 'MATRIX', 'RESPONSE', '1.0.0',
     :                  'RSP_MATRIX', '1.1.0', 'REDIST', ' ', STATUS )

*  Write the energy boundaries into the table
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )
      FSTAT = 0
      CALL FTPCLE( LUN, 1, 1, 1, NENER, %VAL(EBPTR), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NENER, %VAL(EBPTR+VAL__NBR), FSTAT )
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )

*  Map the matrix data
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )

*  Simply write the data
      CRPTR = RPTR
      DO I = 1, NENER
        CALL FTPCLE( LUN, 3, I, 1, NCHAN, %VAL(CRPTR), STATUS )
        CRPTR = CRPTR + NCHAN * VAL__NBR
      END DO

*  Release the matrix data
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_WRITRMF', STATUS )
      END IF

      END



      SUBROUTINE ERI2_WRITRMF_OGIP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI2_WRITRMF_OGIP

*  Purpose:
*     Write OGIP format energy response to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_WRITRMF_OGIP( NARG, ARGS, OARG, STATUS )

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
      CHARACTER*8		TTYPE(6)		! Column names
      CHARACTER*8		TFORM(6)		! Column types
      CHARACTER*3		TUNIT(6)		! Column units

      INTEGER			CPTR			!
      INTEGER			DIMS(2)
      INTEGER			EBPTR			! Energy bounds ptr
      INTEGER			FID			! FITSfile object
      INTEGER			FSTAT			! FITSIO status
      INTEGER			LUN			! Logical unit
      INTEGER			NCHAN			! # channel bins
      INTEGER			NDIM			! Dimensionality
      INTEGER			NENER			! # energy bins
      INTEGER			RMFID			! Response object
      INTEGER			RPTR			! Mapped RMF
      INTEGER			BWIDTH			! Width of binary table in bytes (wrong? - rb)

*  Local data;
      DATA    TUNIT/'keV', 'keV', ' ', ' ', ' ', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      FID = ARGS(1)
      RMFID = ARGS(2)

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Get file's logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Construct the field descriptions for the BINTABLE
      BWIDTH = 0
      TTYPE(1) = 'ENERG_LO'
      TFORM(1) = '1E'
      BWIDTH = BWIDTH + 4
      TTYPE(2) = 'ENERG_HI'
      TFORM(2) = '1E'
      BWIDTH = BWIDTH + 4
      TTYPE(3) = 'N_GRP'
      TFORM(3) = '1I'
      BWIDTH = BWIDTH + 2
      TTYPE(4) = 'F_CHAN'
      TFORM(4) = 'PI'
      BWIDTH = BWIDTH + 2
      TTYPE(5) = 'N_CHAN'
      TFORM(5) = 'PI'
      BWIDTH = BWIDTH + 2
      TTYPE(6) = 'MATRIX'
      TFORM(6) = 'PE'
      BWIDTH = BWIDTH + 4

*  Define the HDU data area
      CALL ADI2_MKBTB( FID, 'MATRIX', NENER, 6, BWIDTH,
     :                 TTYPE, TFORM, TUNIT, 0, STATUS )

*  Write keywords for response extension
      CALL ADI2_POGIPK( FID, 'MATRIX', 'RESPONSE', '1.0.0',
     :                    'RSP_MATRIX', '1.1.0', 'REDIST', ' ', STATUS )

*  Write the energy boundaries into the table
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )
      FSTAT = 0
      CALL FTPCLE( LUN, 1, 1, 1, NENER, %VAL(EBPTR), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NENER, %VAL(EBPTR+4), FSTAT )
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )

*  Map and write the N_grp field
      CALL ADI_CMAPW( RMFID, 'N_grp', 'READ', CPTR, STATUS )
      CALL FTPCLI( LUN, 3, 1, 1, NENER, %VAL(CPTR), FSTAT )
      CALL ADI_CUNMAP( RMFID, 'N_grp', CPTR, STATUS )

*  Map the matrix data
      CALL ADI_CSHAPE( RMFID, 'RMF', 2, DIMS, NDIM, STATUS )
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_WRITRMF_OGIP', STATUS )
      END IF

      END



      SUBROUTINE ERI2_WRITRMF_AST( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI2_WRITRMF_AST

*  Purpose:
*     Write ASTERIX type energy response to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_WRITRMF_AST( NARG, ARGS, OARG, STATUS )

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
*     14 Aug 1995 (DJA):
*        Divide ASTERIX response by geometrical area if available.
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

*  Local Variables:
      REAL			AREA			! Geometrical area

      INTEGER			CIPTR			! Channel indices
      INTEGER			EBPTR			! Energy bounds ptr
      INTEGER			EIPTR			! Energy indices
      INTEGER			FID			! FITSfile object
      INTEGER			NCHAN			! # channel bins
      INTEGER			NENER			! # energy bins
      INTEGER			NRMF			! # response elements
      INTEGER			RMFID			! Response object
      INTEGER			RPTR			! Mapped RMF
      INTEGER			WPTR1, WPTR2		!
      INTEGER			NRESP
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments (these were wrong - rb)
      FID = ARGS(1)
      CALL ADI_GET0I( ARGS(3), NRESP, STATUS)
      RMFID = ARGS(4)

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Get size of matrix
      CALL ADI_CSIZE( RMFID, 'RMF', NRMF, STATUS )

*  Write the energy boundaries into the table
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )

*  Map the matrix data
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )

*   Map the energy and channel index arrays
      CALL ADI_CMAPI( RMFID, 'ChannelIndices', 'READ', CIPTR, STATUS )
      CALL ADI_CMAPI( RMFID, 'EnergyIndices', 'READ', EIPTR, STATUS )

*   Map some workspace
      CALL DYN_MAPI( 1, NENER*2, WPTR1, STATUS )
      CALL DYN_MAPI( 1, NRMF*2, WPTR2, STATUS )

*   Get geometrical area
      CALL ADI_CGET0R( RMFID, 'GeometricalArea', AREA, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        AREA = 1.0
      END IF

*   Write the variable length fields
      CALL ERI2_WRITRMF_AST1( FID, NENER, NCHAN, NRMF, %VAL(EBPTR),
     :                        %VAL(CIPTR), %VAL(EIPTR), %VAL(RPTR),
     :                      AREA, %VAL(WPTR1), %VAL(WPTR2), STATUS )

*   Release workspace
      CALL DYN_UNMAP( WPTR1, STATUS )
      CALL DYN_UNMAP( WPTR2, STATUS )

*   Release index arrays
      CALL ADI_CUNMAP( RMFID, 'ChannelIndices', CIPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'EnergyIndices', EIPTR, STATUS )

*  Release the matrix data
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Write channel energy bounds extension
      CALL ERI2_WRITRMF_CEBND( FID, RMFID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_WRITRMF_AST', STATUS )
      END IF

      END



      SUBROUTINE ERI2_WRITRMF_AST1( FID, NE, NCH, NRMF, EBND, CI,
     :                        EI, RSP, AREA, WRK1, WRK2, STATUS )
*+
*  Name:
*     ERI2_WRITRMF_AST1

*  Purpose:
*     Write ASTERIX energy response elements to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_WRITRMF_AST1( FID, NE, NCH, NRMF, EBND, CI, EI,
*                             RSP, AREA, WRK1, WRK2, STATUS )

*  Description:
*

*  Arguments:
*     FID = INTEGER (given)
*        ADI indentifier of FITS object
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
*     AREA = REAL (given)
*        Geometrical area in cm**2
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
      INTEGER			FID, NE, NCH, NRMF, CI(*), EI(*)
      REAL			EBND(*),AREA,RSP(*)
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
      INTEGER			NRPTR			! Workspace
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
      NFIXED = MAX_NGRP * NE
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
      NFIXED = MAX_SMAT * NE
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

*  Workspace for normalised response
      CALL DYN_MAPR( 1, MAX_SMAT, NRPTR, STATUS )

*  Define the HDU data area
      CALL ADI2_MKBTB( FID, 'MATRIX', NE, 6, NFIXED / NE,
     :                 TTYPE, TFORM, TUNIT, ACTHEAP, STATUS )

*  Other mandatory keywords
      CALL ADI2_PKEY0C( FID, 'MATRIX', 'EXTNAME', 'MATRIX',
     :       'name of this binary table extension', STATUS )
      CALL ADI2_PKEY0I( FID, 'MATRIX', 'DETCHANS', NCH,
     :       'Total number of raw PHA channels', STATUS )
      CALL ADI2_PKEY0C( FID, 'MATRIX', 'CHANTYPE', 'PHA',
     :       'Channels assigned by detector electronics', STATUS )
      CALL ADI2_PKEY0C( FID, 'MATRIX', 'RMFVERSN', '1992a',
     :       'OGIP classification of FITS format style', STATUS )

*  Write keywords for response extension
      CALL ADI2_POGIPK( FID, 'MATRIX', 'RESPONSE', '1.0.0',
     :                  'RSP_MATRIX', '1.1.0', 'FULL', ' ', STATUS )

*  Write energy lower and upper bounds
      CALL FTPCLE( LUN, 1, 1, 1, NE, EBND(1), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NE, EBND(2), FSTAT )

*  The N_GRP field
      CALL FTPCLJ( LUN, 3, 1, 1, NE, WRK1(1,1), FSTAT )

*  Write the table data
      R = 1
      CS = 1
      DO E = 1, NE

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
          IF ( CS .LE. NS ) THEN
            IF ( CS .EQ. NS ) THEN
              LASTR = NRMF
            ELSE
              LASTR = WRK1(E+1,2) - 1
            END IF

*        Copy response values and normalise
            CALL ARR_COP1R( LASTR - R + 1, RSP(R), %VAL(NRPTR), STATUS )
            CALL ARR_MULT1R( LASTR - R + 1, %VAL(NRPTR), 1.0/AREA,
     :                       %VAL(NRPTR), STATUS )

            CALL FTPCLE( LUN, 6, E, 1, LASTR - R + 1, %VAL(NRPTR),
     :                   FSTAT )
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

*   Free workspace
      CALL DYN_UNMAP( NRPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_WRITRMF_AST1', STATUS )
      END IF

      END



      SUBROUTINE ERI2_WRITRMF_CEBND( FID, RMFID, STATUS )
*+
*  Name:
*     ERI2_WRITRMF_CEBND

*  Purpose:
*     Write channel energy bounds to EBOUNDS extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_WRITRMF_CEBND( FID, RMFID, STATUS )

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
      INTEGER			BWIDTH			! Width of binary table in bytes

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

*    Get file's logical unit
        CALL ADI2_GETLUN( FID, LUN, STATUS )

*    Construct the field descriptions for the BINTABLE
        BWIDTH = 0
        TTYPE(1) = 'CHANNEL'
        TFORM(1) = '1I'
        BWIDTH = BWIDTH + 2
        TTYPE(2) = 'E_MIN'
        TFORM(2) = '1E'
        BWIDTH = BWIDTH + 4
        TTYPE(3) = 'E_MAX'
        TFORM(3) = '1E'
        BWIDTH = BWIDTH + 4

*    Define the HDU data area
        CALL ADI2_MKBTB( FID, 'EBOUNDS', NCHAN, 3, BWIDTH,
     :                   TTYPE, TFORM, TUNIT, 0, STATUS )

*    Other mandatory keywords
        CALL ADI2_PKEY0C( FID, 'EBOUNDS', 'EXTNAME', 'EBOUNDS',
     :          'name of this binary table extension', STATUS )
        CALL ADI2_PKEY0I( FID, 'EBOUNDS', 'DETCHANS', NCHAN,
     :          'Total number of raw PHA channels', STATUS )
        CALL ADI2_PKEY0C( FID, 'EBOUNDS', 'RMFVERSN', '1992a',
     :         'OGIP classification of FITS format style', STATUS )

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
