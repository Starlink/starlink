      SUBROUTINE KSTAT( STATUS )
*+
*  Name:
*     KSTAT

*  Purpose:
*     Calculates Kendall's K statistic for a 1D data array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL KSTAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calculates Kendall's K statistic (a measure of correlation) for the
*     data array and axis data of a 1D dataset.
*     The statistic is unit normal to a good approximation (so long as each
*     array contains at least 10 elements) if the two arrays are uncorrelated.
*     Significant +ve values indicate correlation, and negative values
*     correspond to anticorrelation.
*     Can also be used to find the correlation between any two arrays of the
*     same length.

*  Usage:
*     kstat {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input data object
*     INP2 = CHAR (read)
*        Optional input data object

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
*     The K statistic is a distribution-free test for correlation - see
*     Ponman, MNRAS, 201, p769, 1982.

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
*     kstat, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Jul 1986 V0.5-1 (TJP):
*        Original version.
*     16 Jul 1986 V0.5-2 (TJP):
*        Double precision accumulation
*     13 Sep 1988 V1.0-1 (TJP):
*        Rewritten for ASTERIX88 BDA_ etc...
*     24 Nov 1994 V1.8-0 (TJP):
*        Now use USI for user interface
*     15 Jan 1995 V1.8-1 (DJA):
*        Use new data interfaces
*     16 Nov 1995 V2.0-0 (DJA):
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

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'KSTAT Version V2.0-0' )

*  Local Variables:
      DOUBLE PRECISION		K                   	! Kendall's K statistic

      INTEGER			IFID			! Input file identifier
      INTEGER			IFID2			! Input file identifier
      INTEGER                NDIMS               ! Number of dimensions in dataset
      INTEGER                NDIMS2              ! Number of dimensions in 2nd dataset
      INTEGER                DIM(ADI__MXDIM)     ! Size of each dimension
      INTEGER                DIM2(ADI__MXDIM)    ! Size of each dimension of 2nd input
      INTEGER                NDAT                ! Number of data points
      INTEGER                NDAT2               ! Number of data points in 2nd array
      INTEGER                NTD                 ! # tied ranks in data array
      INTEGER                NTX                 ! # tied ranks in axis array
      INTEGER                NBAD                ! # bad quality data

      INTEGER                DPTR                ! Pointer to input data array
      INTEGER                XPTR                ! Pointer to AXIS1 data
      INTEGER                QPTR                ! Pointer to data quality

      LOGICAL                OK                  ! Component present & filled?
      LOGICAL                QOK                 ! Data quality available?
      LOGICAL                CONTINUE            ! Used to control input
      LOGICAL                INPRIM              ! Is input object primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain data object, access and check it
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL ADI_DERVD( IFID, 'Array', INPRIM, STATUS )
      INPRIM = (.NOT.INPRIM)
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIM, NDIMS, STATUS )
      NDAT = DIM(1)

      IF ( OK ) THEN
        IF (NDIMS .EQ. 1) THEN
          CALL BDI_MAPR(IFID, 'Data', 'READ', DPTR, STATUS )

          IF (.NOT. INPRIM) THEN

*        Quality present?
            CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
            IF ( QOK ) THEN
              CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QPTR,
     :                       STATUS )
            END IF

            CALL BDI_AXMAPR(IFID, 1, 'Data', 'READ', XPTR, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL MSG_PRNT ('FATAL ERROR: No axis information')
            END IF

          ELSE ! Primitive
            CONTINUE = .TRUE.

            DO WHILE ( CONTINUE )
              CALL USI_ASSOC( 'INP2', 'BinDS|Array', 'READ', IFID2,
     :                 STATUS )
              CALL ADI_DERVD( IFID2, 'Array', INPRIM, STATUS )
              INPRIM = (.NOT.INPRIM)
              IF (STATUS .NE. SAI__OK) GOTO 99

              IF ( INPRIM ) THEN
                CALL BDI_CHK( IFID2, 'Data', OK, STATUS )
                CALL BDI_GETSHP( IFID2, ADI__MXDIM, DIM2, NDIMS2,
     :                           STATUS )
                NDAT2 = DIM2(1)

                IF ( OK ) THEN
                  IF (NDIMS2 .EQ. 1) THEN
                    CALL BDI_MAPR( IFID2, 'Data', 'READ', XPTR, STATUS )

                    IF (NDAT2 .EQ. NDAT ) THEN
                      CONTINUE = .FALSE.

                    ELSE
                      CALL MSG_PRNT ('ERROR: Arrays are not the '//
     :                                                    'same length')

                    END IF
                  ELSE
                    CALL MSG_PRNT ('ERROR: Input is not primitive')

                  END IF
                ELSE
                  CALL MSG_PRNT ('ERROR: Invalid input')

                END IF
              ELSE
                CALL MSG_PRNT ('ERROR: Input is not 1 dimensional')

              END IF

              IF ( CONTINUE ) THEN
                CALL USI_CANCL ('INP2', STATUS)
                IFID2 = ADI__NULLID

              END IF
            END DO
          END IF
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'AST_ERR: Data is not 1 dimensional',
     :                  STATUS )

        END IF
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'AST_ERR: No data', STATUS )

      END IF

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Pass to main subroutine to calculate K statistic
      CALL KSTAT_DOIT (NDAT, %VAL(DPTR), %VAL(XPTR), QOK, %VAL(QPTR),
     :                                             K, NTD, NTX, NBAD)

*    Output results.
      CALL MSG_PRNT (' ')
      CALL MSG_PRNT ('***********************************************'//
     :                                                        '*******')

      IF ( QOK ) THEN
        IF (NBAD .GT. 0) THEN
          CALL MSG_SETI ('NBAD', NBAD)
          CALL MSG_PRNT ('    ^NBAD bad pairs omitted')

        ELSE
          CALL MSG_PRNT ('    No bad quality data')

        END IF
        CALL MSG_PRNT (' ')

      END IF
      CALL MSG_SETD ('K', K)
      CALL MSG_PRNT ('    Kendall''s Statistic = ^K')

      IF (.NOT. INPRIM) THEN
        IF (NTD .GT. 0) THEN
          CALL MSG_SETI ('NTD', NTD)
          CALL MSG_PRNT ('    Corrected for ^NTD tied data values')

        END IF

        IF (NTX .GT. 0) THEN
          CALL MSG_SETI ('NTX', NTX)
          CALL MSG_PRNT ('    Corrected for ^NTX tied axis values')

        END IF

      ELSE
        IF ( NTD .GT. 0 ) THEN
          CALL MSG_SETI ('NTD', NTD)
          CALL MSG_PRNT ('    Corrected for ^NTD tied input 1 values')

        END IF

        IF (NTX .GT. 0) THEN
          CALL MSG_SETI ('NTX', NTX)
          CALL MSG_PRNT ('    Corrected for ^NTX tied input 2 values')

        END IF
      END IF

      CALL MSG_PRNT ('***********************************************'//
     :                                                        '*******')
      CALL MSG_PRNT (' ')

      IF (DIM(1) .LE. 10) THEN
        CALL MSG_PRNT ('WARNING: Less than 11 data points. Carefull '//
     :                                     ' interpretation required !')
      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END




*+ KSTAT_DOIT - Calculate Kendall's statistic
      SUBROUTINE KSTAT_DOIT( NDAT, DATA, AXIS, QOK, QUAL, K, NTD, NTX,
     :                                                          NBAD )
*    Description :
*     Calculates K statistic.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman
*    History :
*     14 Jul 86: Original (v.1)
      IMPLICIT NONE
*    Import :
      INTEGER                NDAT                ! Number of dimensions

      REAL                   DATA(NDAT)          ! Data array
      REAL                   AXIS(NDAT)          ! Axis data

      LOGICAL                QOK                 ! Quality OK?
      LOGICAL                QUAL(NDAT)          ! Quality data available?

*    Export :
      DOUBLE PRECISION       K                   ! Kendall's statistic

      INTEGER                NTD                 ! # tied ranks in D
      INTEGER                NTX                 ! # tied ranks in X
      INTEGER                NBAD                ! # bad values omitted

*    Local variables :
      INTEGER                I, J                ! Loop counters

      REAL                   A                   ! Temporary value used in calc of SUM

      DOUBLE PRECISION       NGOOD               ! Number of good data
      DOUBLE PRECISION       SUM                 ! Sum of correlation terms
      DOUBLE PRECISION       TEMP                ! Temporary value used in calc of K statistik
*-

*    Initialisation
      NBAD = 0
      NTD  = 0
      NTX  = 0
      SUM  = 0.0D0

*    See if Quality is to be used
      IF ( QOK ) THEN
*      Loop over data
        DO I = 1, NDAT
          IF ( QUAL(I) ) THEN
            DO J = I + 1, NDAT
              IF ( QUAL(J) ) THEN
                A = ( DATA(I) - DATA(J)) * ( AXIS(I) - AXIS(J) )

                IF ( A .GT. 0.0 ) THEN
                  SUM = SUM + 1

                ELSE IF ( A .LT. 0.0 ) THEN
                  SUM = SUM - 1

                ELSE
*                Tie
                  IF ( DATA(I) .EQ. DATA(J) ) THEN
                    NTD = NTD + 1

                  ELSE
                    NTX = NTX + 1

                  END IF
                END IF
              END IF
            END DO
          ELSE
            NBAD = NBAD + 1

          END IF
        END DO
      ELSE
*      Loop over data
        DO I = 1, NDAT
          DO J = I + 1, NDAT
            A = ( DATA(I) - DATA(J)) * ( AXIS(I) - AXIS(J) )

            IF ( A .GT. 0.0 ) THEN
              SUM = SUM + 1

            ELSE IF ( A .LT. 0.0 ) THEN
              SUM = SUM - 1

            ELSE
*            Tie
              IF ( DATA(I) .EQ. DATA(J) ) THEN
                NTD = NTD + 1

              END IF
C              ELSE

              IF ( AXIS(I) .EQ. AXIS(J) ) THEN
                NTX = NTX + 1

              END IF
            END IF
          END DO
        END DO
      END IF

*    Calculate K statistic
      NGOOD = DBLE(NDAT - NBAD)
      K = SUM * SQRT(18.0D0 / (NGOOD * (NGOOD - 1.0D0) *
     :                                       ((2.0D0 * NGOOD) + 5.0D0)))

*    Correct for ties if necessary
      IF ( NTD .GT. 0 .OR. NTX .GT. 0 ) THEN
        TEMP = NGOOD * (NGOOD - 1.0D0) / 2.0D0
        K    = TEMP * K / SQRT((TEMP - DBLE(NTD)) * (TEMP - DBLE(NTX)))

      END IF

      END
