      SUBROUTINE MEANDAT( STATUS )
*+
*  Name:
*     MEANDAT

*  Purpose:
*     Averages a series of files of any dimensionality

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL MEANDAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Averages upto 20 datafiles using a weighted or non-weighted mean.
*     Elements with bad quality are ignored.

*  Usage:
*     meandat {parameter_usage}

*  Environment Parameters:
*     NFILES = INTEGER (read)
*        Number of files to average
*     INP<n> = CHAR (read)
*        Name of the <n>th input file
*     WEIGHT<n> = REAL (read)
*        Weight of the <n>th file
*     AVERAGE = LOGICAL (read)
*        Average or sum the files ?

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
*     meandat, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RDS: Richard Saxton (Starlink,University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 May 1990 V1.2-1 (RDS):
*        Original version.
*     28 Feb 1994 V1.7-0 (DJA):
*        Use BIT_ routines for quality manipulation
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     11 Dec 1995 V2.0-0 (DJA):
*        ADI port. Use logical quality
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER 			MAXFIL
        PARAMETER 		( MAXFIL = 20 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'MEANDAT Version V2.0-0' )

*  Local Variables:
      INTEGER LP,AXLP
      INTEGER NDIM                           ! Number of dimensions in array
      INTEGER DIMS(ADI__MXDIM)               ! Dimensions of data array
      INTEGER TNDIM                          ! Test number of dimensions
      INTEGER TDIMS(ADI__MXDIM)              ! Test dimensions of data array
      INTEGER NFILES                         ! Number of files to be averaged
      INTEGER			IFID(MAXFIL)		! Input dataset ids
      INTEGER			OFID			! Output dataset id

      LOGICAL 			LVAR                    ! Are variances present in all file
      LOGICAL 			LQUAL                   ! Is quality present in all files ?
      LOGICAL 			OK                      ! Is data array present

      INTEGER DP(MAXFIL)                     ! Pointer to data arrays
      INTEGER VP(MAXFIL)                     ! Pointer to variance arrays
      INTEGER QP(MAXFIL)                     ! Pointer to quality arrays
      INTEGER ODP                            ! Pointer to output arrays
      INTEGER OVP                            ! Pointer to output variance array
      INTEGER OQP                            ! Pointer to output quality array
      INTEGER NELS                           ! Total number of elements in array
      LOGICAL AVERAGE                        ! Average the files or sum ?
      INTEGER CPTR                           ! Pointer to dynamic workspace
      CHARACTER*80 PATH(42)                  ! Historty records
      INTEGER NLINES                         ! Number of lines of history text
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Initialise variables
      LVAR = .TRUE.
      LQUAL = .TRUE.
      DO LP = 1, ADI__MXDIM
         DIMS(LP)=1
         TDIMS(LP)=1
      ENDDO

*  Find how many files are wanted
      CALL USI_GET0I('NFILES', NFILES, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99
      IF (NFILES .LT. 2 .OR. NFILES .GT. 20) THEN
         CALL MSG_PRNT('Number of files must be between 2 and 20')
         GOTO 99
      ENDIF

*  Open files
      DO LP = 1, NFILES
        CALL USI_IASSOC( 'INP', LP, 'BinDS|Array', 'READ', IFID(LP),
     :                   STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99
      END DO

*  Get dimensions of data array in first file
      CALL BDI_GETSHP( IFID(1), ADI__MXDIM, DIMS, NDIM, STATUS )

*  Map data array, variance and quality from all files
      DO LP = 1, NFILES

        CALL MSG_SETI('FNO', LP)

*    Get data array dimensions
        CALL BDI_CHK( IFID(LP), 'Data', OK, STATUS )
        CALL BDI_GETSHP( IFID(LP), ADI__MXDIM, TDIMS, TNDIM, STATUS )
        IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error looking for data array ^FNO')
          GOTO 99

        ELSE

*      Test if dimensions are the same as for file 1
          DO AXLP=1,7
            IF (DIMS(AXLP) .NE. TDIMS(AXLP)) THEN
              CALL MSG_PRNT('File ^FNO has different dimensions'/
     :                         /' to file 1')
              GOTO 99
            END IF
          ENDDO

*      Map data array
          CALL BDI_MAPR( IFID(LP), 'Data', 'READ', DP(LP), STATUS )
          IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping data array ^FNO')
            GOTO 99
          END IF

        END IF

*    Attempt to map variance from file if not already missing
        IF ( LVAR ) THEN

          CALL BDI_CHK( IFID(LP), 'Variance', LVAR, STATUS )
          IF (STATUS .NE. SAI__OK .OR. .NOT. LVAR) THEN
            CALL MSG_PRNT('Variance missing - will not be used')
            LVAR = .FALSE.
            CALL ERR_ANNUL(STATUS)
          ELSE
            CALL BDI_MAPR( IFID(LP), 'Variance', 'READ', VP(LP),
     :                     STATUS )
          END IF
        END IF

*    Attempt to map quality from file if not already missing
        IF ( LQUAL ) THEN

          CALL BDI_CHK( IFID(LP), 'Quality', LQUAL, STATUS )
          IF (STATUS .NE. SAI__OK .OR. .NOT. LQUAL) THEN
            CALL MSG_PRNT('Quality missing - will not be used')
            LQUAL = .FALSE.
            CALL ERR_ANNUL(STATUS)
          ELSE

            CALL BDI_MAPL( IFID(LP), 'LogicalQuality', 'READ',
     :                     QP(LP), STATUS )
          END IF
        END IF

      END DO

*  Calculate total number of elements in output array
      CALL ARR_SUMDIM( NDIM, DIMS, NELS )

*  Create an output file
      CALL USI_CLONE( 'INP1', 'OUT', 'BinDS', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map the data array in the output file and zero it.
      CALL BDI_MAPR( OFID, 'Data', 'UPDATE', ODP, STATUS )
      CALL ARR_INIT1R(0.0, NELS, %val(ODP), STATUS )

*  Map the variance array if variances being used, otherwise delete any
*  variance array
      IF ( LVAR ) THEN

        CALL BDI_MAPR( OFID, 'Variance', 'UPDATE', OVP, STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99
        CALL ARR_INIT1R(0.0, NELS, %val(OVP), STATUS)

      ELSE
c         CALL BDI_DELETE( OFID, 'Variance', STATUS )
        IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)

      END IF

*  Map the quality array if quality being used, otherwise delete any quality
*  structure
      IF ( LQUAL ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'UPDATE', OQP, STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99

*    Zero the quality array
        CALL ARR_INIT1B( QUAL__GOOD, NELS, %val(OQP), STATUS )

      ELSE

c        CALL BDI_DELETE( OFID, 'Quality', STATUS )
        IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)

      END IF

*  By default the files are averaged, if the user sets average to NO on
*  the command line then the output will be the sum of these files.
      CALL USI_GET0L('AVERAGE', AVERAGE, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Create a workspace array and zero it
      CALL DYN_MAPR( NDIM, DIMS, CPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating dynamic space')
         GOTO 99
      END IF
      CALL ARR_INIT1R( 0.0, NELS, %val(CPTR), STATUS )

*  Average these files
      CALL MEANDAT_AVERAGE( NFILES, LVAR, LQUAL, AVERAGE, NELS, DP,
     :                      VP, QP, %VAL(CPTR), %VAL(ODP), %VAL(OVP),
     :                      %VAL(OQP), STATUS)

*  Add standard record to new history structure
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Put names of input datafiles used into history
      CALL USI_NAMEI( NLINES, PATH, STATUS )
      CALL HSI_PTXT( OFID, NLINES, PATH, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  MEANDAT_AVERAGE - Averages several datafiles
      SUBROUTINE MEANDAT_AVERAGE( NFILES, LVAR, LQUAL,
     :                     AVERAGE, NELS, DP, VP, QP, WTSUM, ODATA,
     :                     OVAR, OQUAL, STATUS)
*    Description :
*      Averages several datafiles weighting them either by variance,
*      or a user defined value. Each array is used as a 1-d vector regardless
*      of how many intrinsic dimensions it has.
*    Environment parameters :
*      WEIGHT        REAL        How to weight the datafiles
*    Method :
*      Get weighting mechanism from user
*      Loop around each file
*        Add the contents of each element into the output element
*            if the quality is good using the apropriate weight
*      End loop
*      Average the output arrays using a counter for each element
*
*    Deficiencies :
*    Bugs :
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*    History :
*       15-MAY-1990     Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER NFILES                     ! Number of files being averaged
      LOGICAL LVAR                       ! Use variances ?
      LOGICAL LQUAL                      ! Use quality ?
      LOGICAL AVERAGE                    ! Average files or leave as sum ?
      INTEGER NELS                       ! Array length
      INTEGER DP(*)                 ! Pointers to data arrays
      INTEGER VP(*)                 ! Pointers to variance arrays
      INTEGER QP(*)                 ! Pointers to quality arrays
*    Import-Export :
      REAL WTSUM(NELS)                   ! Number to normalise output bins
*    Export :
      REAL ODATA(NELS)                   ! Output data array
      REAL OVAR(NELS)                    ! Output variance array
      BYTE OQUAL(NELS)                   ! Output quality array
*    Status :
      INTEGER STATUS
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*20 WTMETH                ! Subtraction method. 'V'=inverse
*                                        ! variance, 'N'=no weight, 'U'=
*                                        ! user defined weights
      CHARACTER*2 CNUM                   ! Character string of observation
      CHARACTER*8 PARAM                  ! Character string for weight parameter
      REAL WEIGHT                        ! Weight for each file
      INTEGER FLP,LP,IDUM
      LOGICAL JUMPOUT
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Ask which weighting mechanism is wanted
      JUMPOUT=.FALSE.
      DO WHILE (.NOT. JUMPOUT)
*
         CALL USI_GET0C('WTMETH', WTMETH, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 99
*
         CALL CHR_UCASE(WTMETH)
*
         IF ( INDEX('VNU', WTMETH(1:1)) .EQ. 0 ) THEN
            CALL MSG_PRNT('Weighting operation not recognised')
            CALL USI_CANCL('WTMETH', STATUS)
         ELSE
            JUMPOUT=.TRUE.
         ENDIF
      ENDDO
*
* If no weighting mechanism wanted - set weight to 1.0
      IF (WTMETH(1:1) .EQ. 'N') THEN
         WEIGHT=1.0
*
* Check variances available if variance weighting requested
      ELSEIF (WTMETH(1:1) .EQ. 'V' .AND. .NOT. LVAR) THEN
*
         CALL MSG_PRNT('Variances not available for all files - '/
     &                /'producing an unweighted mean')
*
         WEIGHT=1.0
         WTMETH='N    '
      ENDIF

*  Loop over files
      DO FLP = 1, NFILES

*    Weights supplied by user ?
        IF (WTMETH(1:1) .EQ. 'U') THEN

*    Create prompt for the weight parameter
          CALL CHR_ITOC(FLP, CNUM, IDUM)
          PARAM='WEIGHT'//CNUM
          CALL USI_PROMT(PARAM, 'Enter weighting for file '/
     :                                             /CNUM, STATUS)

*      Ask for the weighting for this file
          CALL USI_GET0R( PARAM, WEIGHT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END IF

*    Add the elements from this data array into the output arrays
        CALL MEANDAT_SUM(WTMETH, WEIGHT, LVAR, LQUAL, NELS,
     :                    %VAL(DP(FLP)),  %VAL(VP(FLP)), %VAL(QP(FLP)),
     :                                              ODATA, OVAR, WTSUM)

      END DO

*  By default the files should be averaged but give the user a chance to
*  have them summed by not dividing by the number of elements used in each
*  output bin.
      DO LP=1,NELS

*    Test if this element had any contribution from the input file
        IF (WTSUM(LP) .GT. 0.0) THEN

*    Average the arrays if producing a mean rather than a sum
          IF ( AVERAGE ) THEN

            ODATA(LP) = ODATA(LP) / WTSUM(LP)
            IF (LVAR) THEN
              OVAR(LP) = OVAR(LP) / ( WTSUM(LP) **2 )
            END IF
          END IF

*      Set quality good
          IF (LQUAL) OQUAL(LP) = QUAL__GOOD

*    Element is bad
        ELSE
          IF (LQUAL) OQUAL(LP) = QUAL__BAD

        END IF

      END DO

*    Tidy up
 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'MEANDAT_AVERAGE', STATUS )
      END IF

      END



*+  MEANDAT_SUM - Adds an input array to an output array
      SUBROUTINE MEANDAT_SUM(WTMETH, WEIGHT, LVAR, LQUAL,
     &                 NELS, IDATA, IVAR, IQUAL, ODATA, OVAR, WTSUM)
*    Description :
*    Environment parameters :
*    Method :
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*    History :
*       15-MAY-1990     Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'QUAL_PAR'
*    Import :
      CHARACTER*(*) WTMETH               ! Method of calculating weighting
*                                        ! factor 'V'=inverse variance,
*                                        ! 'N'=No weighting
      REAL WEIGHT                        ! Weight if WTMETH .ne. 'V'
      LOGICAL LVAR                       ! Use variances ?
      LOGICAL LQUAL                      ! Use quality ?
      INTEGER NELS                       ! Number of elements in arrays
      REAL IDATA(NELS)                   ! Input data array
      REAL IVAR(NELS)                    ! Input variance array
      LOGICAL IQUAL(NELS)                   ! Input quality array
*    Import-Export :
      REAL ODATA(NELS)
      REAL OVAR(NELS)
      REAL WTSUM(NELS)                   ! Sum of weights used in pixel
*
*    Local variables :
*
      INTEGER LP
      LOGICAL			LGOOD			! Pixel is good?
*-


*  Initialise
      LGOOD = .TRUE.

*  Loop over all data
      DO LP=1,NELS

*    Calculate weight from variance if wanted
        IF (WTMETH(1:1) .EQ. 'V') THEN
          IF (IVAR(LP) .GT. 0.0) THEN
            WEIGHT = 1.0 / IVAR(LP)
          ELSE
            WEIGHT = 0.0
          END IF
        ENDIF

*    Is quality of this element ok ?
        IF ( LQUAL ) LGOOD = IQUAL(LP)

*    Good point?
        IF ( LGOOD ) THEN

*      Add weighted data array element
          ODATA(LP) = ODATA(LP) + IDATA(LP) * WEIGHT

*      Add variance if possible
          IF (LVAR)  OVAR(LP) = OVAR(LP) + IVAR(LP) * ( WEIGHT**2 )

*      Tot up the normalising factor
          WTSUM(LP) = WTSUM(LP) + WEIGHT

        END IF

      END DO

      END
