      SUBROUTINE ERI2_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ERI2_READ

*  Purpose:
*     Read the energy response from a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI2_READ( NARG, ARGS, OARG, STATUS )

*  Description:
*     Constructs the data objects required by ERI from the supplied FITS
*     dataset. This must be an OGIP format response.

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
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private, FITS, read, responses

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'PRM_PAR'
      INCLUDE 'ADI_PAR'					! ADI constants

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Variables:
      CHARACTER*50		COMM			! Keyword comment
      CHARACTER*20		HDUTYPE			!
      CHARACTER*20		VERSION			! Version of format

      REAL			ENULL			! Null value
      REAL			RSUM			! Total # subsets
      REAL			THRESH			! Threshold

      INTEGER			AID			! Ancillary response
      INTEGER			CBPTR			! Channel energy bounds
      INTEGER			CURPTR			! Cursor pointer
      INTEGER			EBPTR			! Energy bounds
      INTEGER			FCPTR			! Mapped F_chan member
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			! Loop variable
      INTEGER			ICOL			! FITS table column no.
      INTEGER			JNULL			! Null value
      INTEGER			LNRMF			! Value of NRMF for I-1
      INTEGER			LUN			! Logical unit number
      INTEGER			NCHAN			! Size of channel axis
      INTEGER			NCPTR			! Mapped N_chan member
      INTEGER			NELEM			! # RMF values in E bin
      INTEGER			NENER			! Size of energy axis
      INTEGER			NGPTR			! Mapped N_grp member
      INTEGER			NGRP			! # groups per E bin
      INTEGER			NRMF			! # response elements
      INTEGER			RID			! Redistribution
      INTEGER			RPTR			! RMF values
      INTEGER			WPTR			! Workspace

      LOGICAL			ANYFLG			! Any duff values?
      LOGICAL			ISARF			! Input is an ARF
      LOGICAL			ISRMF			! Input is an RMF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Extract the logical unit
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )

*  Move to the MATRIX extension
      FSTAT = 0
      CALL FTMAHD( LUN, 2, HDUTYPE, FSTAT )

*  Look for RMFVERSN or ARFVERSN.
      ISARF = .FALSE.
      ISRMF = .FALSE.
      CALL FTGKYS( LUN, 'RMFVERSN', VERSION, COMM, FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        ISRMF = .TRUE.
      ELSE
        FSTAT = 0
        CALL FTGKYS( LUN, 'ARFVERSN', VERSION, COMM, FSTAT )
        IF ( FSTAT .EQ. 0 ) THEN
          ISARF = .TRUE.
        ELSE
          ISRMF = .TRUE.
        END IF
      END IF

*  Read in an ancillary response?
      IF ( ISARF ) THEN

*    New ancillary response
        CALL ADI_NEW0( 'AncillaryResponse', AID, STATUS )

*    Size of response
        CALL FTGKYJ( LUN, 'NAXIS2', NENER, COMM, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Unable to determine size of ancillary '/
     :                  /'response energy axis', STATUS )
          GOTO 99
        END IF
        CALL ADI_CPUT0I( AID, 'NENERGY', NENER, STATUS )

*    Locate the ENERG_LO column
        CALL FTGCNO( LUN, .FALSE., 'ENERG_LO', ICOL, FSTAT )

*    Create and map an energy bounds object. Get the first NENER bounds from
*    the ENERG_LO column, and the last bound from the last element of the
*    ENERG_HI column.
        CALL ADI_CNEW1R( AID, 'Energy', NENER+1, STATUS )
        CALL ADI_CMAPR( AID, 'Energy', 'WRITE', EBPTR, STATUS )
        ENULL = 0.0
        CALL FTGCVE( LUN, ICOL, 1, 1, NENER, ENULL, %VAL(EBPTR), ANYFLG,
     :               FSTAT )
        CALL FTGCNO( LUN, .FALSE., 'ENERG_HI', ICOL, FSTAT )
        CALL FTGCVE( LUN, ICOL, NENER, 1, 1, ENULL,
     :               %VAL(EBPTR+NENER*VAL__NBR), ANYFLG, FSTAT )
        CALL ADI_CUNMAP( AID, 'Energy', EBPTR, STATUS )

*    Locate the SPECRESP column
        CALL FTGCNO( LUN, .FALSE., 'SPECRESP', ICOL, FSTAT )

*    Create and map the output object array
        CALL ADI_CNEW1R( AID, 'Response', NENER, STATUS )
        CALL ADI_CMAPR( AID, 'Response', 'WRITE', RPTR, STATUS )

*    Load data from column of table
        CALL FTGCVE( LUN, ICOL, 1, 1, NENER, ENULL, %VAL(RPTR), ANYFLG,
     :               FSTAT )

*    Unmap the output array
        CALL ADI_CUNMAP( AID, 'Response', RPTR, STATUS )

*  Read in a matrix?
      ELSE IF ( ISRMF ) THEN

*    An OGIP specific response
        CALL ADI_NEW0( 'OGIPcmpRMF', RID, STATUS )

*    Size of energy axis of response
        CALL FTGKYJ( LUN, 'NAXIS2', NENER, COMM, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Unable to determine size of response'/
     :                  /' energy axis', STATUS )
          GOTO 99
        END IF
        CALL ADI_CPUT0I( RID, 'NENERGY', NENER, STATUS )

*    Size of channel axis of response
        CALL FTGKYJ( LUN, 'DETCHANS', NCHAN, COMM, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Unable to determine size of response'/
     :                  /' channel axis', STATUS )
          GOTO 99
        END IF
        CALL ADI_CPUT0I( RID, 'NCHAN', NCHAN, STATUS )

*    Probability threshold (not mandatory)
        CALL FTGKYE( LUN, 'LO_THRES', THRESH, COMM, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          FSTAT = 0
        ELSE
          CALL ADI_CPUT0R( RID, 'Threshold', THRESH, STATUS )
        END IF

*    Locate the ENERG_LO column
        CALL FTGCNO( LUN, .FALSE., 'ENERG_LO', ICOL, FSTAT )

*    Create and map an energy bounds object. Get the first NENER bounds from
*    the ENERG_LO column, and the last bound from the last element of the
*    ENERG_HI column.
        CALL ADI_CNEW1R( RID, 'Energy', NENER+1, STATUS )
        CALL ADI_CMAPR( RID, 'Energy', 'WRITE', EBPTR, STATUS )
        ENULL = 0.0
        CALL FTGCVE( LUN, ICOL, 1, 1, NENER, ENULL, %VAL(EBPTR), ANYFLG,
     :               FSTAT )
        CALL FTGCNO( LUN, .FALSE., 'ENERG_HI', ICOL, FSTAT )
        CALL FTGCVE( LUN, ICOL, NENER, 1, 1, ENULL,
     :               %VAL(EBPTR+NENER*VAL__NBR), ANYFLG, FSTAT )
        CALL ADI_CUNMAP( RID, 'Energy', EBPTR, STATUS )

*    Create the Ngrp object, and copy the response column
        CALL ADI_CNEW1I( RID, 'Ngrp', NENER, STATUS )
        CALL ADI_CMAPI( RID, 'Ngrp', 'WRITE', NGPTR, STATUS )
        CALL FTGCNO( LUN, .FALSE., 'N_GRP', ICOL, FSTAT )
        JNULL = 0
        CALL FTGCVJ( LUN, ICOL, 1, 1, NENER, JNULL, %VAL(NGPTR),
     :               ANYFLG, FSTAT )

*    That defines the size of the Fchan and Nchan arrays
        CALL ADI_CNEW1I( RID, 'Fchan', NENER, STATUS )
        CALL ADI_CNEW1I( RID, 'Nchan', NENER, STATUS )

*    Map the Fchan member, and copy the F_CHAN table field into it
        CALL FTGCNO( LUN, .FALSE., 'F_CHAN', ICOL, FSTAT )
        CALL ADI_CMAPI( RID, 'Fchan', 'WRITE', FCPTR, STATUS )
        CURPTR = FCPTR
        DO I = 1, NENER
          CALL ARR_ELEM1I( NGPTR, NENER, I, NGRP, STATUS )
          IF ( NGRP .GT. 0 ) THEN
            CALL FTGCVJ( LUN, ICOL, I, 1, NGRP, JNULL,
     :                   %VAL(CURPTR), ANYFLG, FSTAT )
            IF ( FSTAT .NE. 0 ) THEN
              CALL ADI2_FITERP( FSTAT, STATUS )
              CALL ERR_REP( ' ', 'Error reading F_CHAN table column',
     :                      STATUS )
            END IF
            CURPTR = CURPTR + NGRP * VAL__NBI
          END IF
        END DO
        CALL ADI_CUNMAP( RID, 'Fchan', FCPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*     Map a workspace array to hold the number of RMF values per row
*     in the table
        CALL DYN_MAPI( 1, NENER, WPTR, STATUS )

*    Map the Nchan member, and copy the N_CHAN table field into it
        CALL FTGCNO( LUN, .FALSE., 'N_CHAN', ICOL, FSTAT )
        CALL ADI_CMAPI( RID, 'Nchan', 'WRITE', NCPTR, STATUS )
        CURPTR = NCPTR
        NRMF = 0
        LNRMF = 0
        DO I = 1, NENER
          CALL ARR_ELEM1I( NGPTR, NENER, I, NGRP, STATUS )
          IF ( NGRP .GT. 0 ) THEN
            CALL FTGCVJ( LUN, ICOL, I, 1, NGRP, JNULL,
     :                   %VAL(CURPTR), ANYFLG, FSTAT )
            IF ( FSTAT .NE. 0 ) THEN
              CALL ADI2_FITERP( FSTAT, STATUS )
              CALL ERR_REP( ' ', 'Error reading N_CHAN table column',
     :                      STATUS )
            ELSE
              CALL ARR_SUM1I( NGRP, %VAL(CURPTR), RSUM, STATUS )
              NRMF = NRMF + NINT(RSUM)
            END IF
            CURPTR = CURPTR + NGRP * VAL__NBI
          END IF

*      Store number of RMF values in this row
          CALL ARR_SELEM1I( WPTR, NENER, I, NRMF - LNRMF, STATUS )
          LNRMF = NRMF

        END DO
        CALL ADI_CUNMAP( RID, 'Nchan', FCPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Release the group information
        CALL ADI_CUNMAP( RID, 'Ngrp', NGPTR, STATUS )

*    NRMF now holds the size of the response. Create the data member and copy
*    the data from the MATRIX column of the table
        CALL ADI_CNEW1R( RID, 'RMF', NRMF, STATUS )
        CALL FTGCNO( LUN, .FALSE., 'MATRIX', ICOL, FSTAT )
        CALL ADI_CMAPR( RID, 'RMF', 'WRITE', RPTR, STATUS )
        CURPTR = RPTR
        NRMF = 0
        DO I = 1, NENER

*      Number of elements to copy for this row
          CALL ARR_ELEM1I( WPTR, NENER, I, NELEM, STATUS )
          IF ( NELEM .GT. 0 ) THEN
            CALL FTGCVE( LUN, ICOL, I, 1, NELEM, ENULL,
     :                   %VAL(CURPTR), ANYFLG, FSTAT )
            IF ( FSTAT .NE. 0 ) THEN
              CALL ADI2_FITERP( FSTAT, STATUS )
              CALL ERR_REP( ' ', 'Error reading MATRIX table column',
     :                      STATUS )
            END IF
            CURPTR = CURPTR + NELEM * VAL__NBR
          END IF

        END DO
        CALL ADI_CUNMAP( RID, 'RMF', RPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Release workspace
        CALL DYN_UNMAP( WPTR, STATUS )

*    Now read the EBOUNDS extension to set up the Channels member. If this
*    extension is not present simply leave the Channels unset.
        CALL FTMRHD( LUN, 1, HDUTYPE, FSTAT )
        IF ( FSTAT .EQ. 0 ) THEN

*      Create and map a channel bounds object. Get the first NENER bounds from
*      the E_MIN column, and the last bound from the last element of the
*      E_MAX column.
          CALL FTGCNO( LUN, .FALSE., 'E_MIN', ICOL, FSTAT )
          CALL ADI_CNEW1R( RID, 'Channels', NCHAN+1, STATUS )
          CALL ADI_CMAPR( RID, 'Channels', 'WRITE', CBPTR, STATUS )
          CALL FTGCVE( LUN, ICOL, 1, 1, NCHAN, ENULL, %VAL(CBPTR),
     :                 ANYFLG, FSTAT )
          CALL FTGCNO( LUN, .FALSE., 'E_MAX', ICOL, FSTAT )
          CALL FTGCVE( LUN, ICOL, NCHAN, 1, 1, ENULL,
     :                 %VAL(CBPTR+NCHAN*VAL__NBR), ANYFLG, FSTAT )
          CALL ADI_CUNMAP( RID, 'Channels', CBPTR, STATUS )

        END IF

      END IF

*  Structure containing results
      CALL ADI_NEW0( 'STRUC', OARG, STATUS )
      IF ( ISRMF ) THEN
        CALL ADI_CPUTID( OARG, 'RMF', RID, STATUS )
      END IF
      IF ( ISARF ) THEN
        CALL ADI_CPUTID( OARG, 'ARF', AID, STATUS )
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_READ', STATUS )
      END IF

      END
