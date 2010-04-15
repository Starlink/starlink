      SUBROUTINE DSA_MATCH_AXIS( DSARE1, AXIS1, DSARE2, AXIS2, STATUS )
*+
*  Name:
*     DSA_MATCH_AXIS

*  Purpose:
*     Check that the axis information for two specific axes matches.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MATCH_AXIS( DSARE1, AXIS1, DSARE2, AXIS2, STATUS )

*  Description:
*     This routine checks that the axis information for a specific axis
*     in one NDF matches that for a specific axis in another
*     (or the same) NDF. It checks
*        a) that the units are the same for each axis,
*        b) that the centre values are the same for each axis.
*     If there are any discrepancies, an error message is put out, and
*     bad status is returned. If all
*     axes in a structure are to be compared with the corresponding axes
*     in another, then DSA_MATCH_AXES should be used.
*
*     Contrary to earlier implementations, the axis centre arrays should
*     not be mapped while this routine is called.

*  Arguments:
*     DSARE1 = CHARACTER * ( * ) (Given)
*        The reference name associated with the first NDF.
*     AXIS1 = INTEGER (Given)
*        The axis number in the first NDF.
*     DSARE2 = CHARACTER * ( * ) (Given)
*        The reference name associated with the second NDF.
*     AXIS1 = INTEGER (Given)
*        The axis number in the first NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15 Jul 1987 (ks):
*        Original version.
*     08 Dec 1989 (ks):
*        Text of error message fixed (was using same ref name for both
*        structures when values differed).
*     15 Feb 1991 (ks):
*        Missing \N added to DSA_WRUSER call.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     07 Oct 1992 (hme):
*        When removing spaces from the 2nd units this routine used to
*        look for spaces in UNITS2 but then fiddled with UNITS,
*        corrupting the latter and making them unequal all the time.
*     02 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Move action into DSA1_MATAX.
*        Translate between application-side status and Starlink status.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSARE1
      INTEGER AXIS1
      CHARACTER * ( * ) DSARE2
      INTEGER AXIS2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLO1, SLO2         ! The reference slots
      INTEGER NERR               ! Number of discrepant pixel centres

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up both references.
      CALL DSA1_RFND( DSARE1, SLO1, STATUS )
      CALL DSA1_RFND( DSARE2, SLO2, STATUS )

*  Call the work routine.
      CALL DSA1_MATAX( SLO1, AXIS1, SLO2, AXIS2, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_MATAX( SLO1, AXIS1, SLO2, AXIS2, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLO1
      INTEGER AXIS1
      INTEGER SLO2
      INTEGER AXIS2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      LOGICAL EXST1, EXST2       ! Whether structures exists
      LOGICAL MATCH              ! Whether unit strings match
      LOGICAL OK                 ! Whether dimensions match
      INTEGER I                  ! Loop index
      INTEGER LEN1, LEN2         ! Unit string lengths
      INTEGER IPT1, IPT2         ! Pointers into strings
      INTEGER NDIM1, NDIM2       ! Centre array dimensionalities
      INTEGER NELM1, NELM2       ! Centre array sizes
      INTEGER DIM1( NDF__MXDIM ) ! Centre array dimensions
      INTEGER DIM2( NDF__MXDIM ) ! Centre array dimensions
      INTEGER MSLOT1, MSLOT2     ! The map slots
      INTEGER PNTR1, PNTR2       ! The array pointers
      INTEGER NERR               ! Number of discrepant pixel centres
      INTEGER ERROR1             ! The first discrepand pixel centre
      CHARACTER * ( 32 ) UNITS1, UNITS2 ! The axis units

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clear mapping flags.
      MSLOT1 = 0
      MSLOT2 = 0

*  Get units for both axes and compare them. (The comparison is a little
*  crude - the strings are converted to upper case, and blanks removed
*  before they are compared).
      UNITS1 = ' '
      UNITS2 = ' '
      CALL NDF_ASTAT( DSA__REFID1(SLO1), 'UNITS', AXIS1, EXST1, STATUS )
      CALL NDF_ASTAT( DSA__REFID1(SLO2), 'UNITS', AXIS2, EXST2, STATUS )
      IF ( EXST1 ) CALL NDF_ACGET( DSA__REFID1(SLO1), 'UNITS', AXIS1,
     :   UNITS1, STATUS )
      IF ( EXST2 ) CALL NDF_ACGET( DSA__REFID1(SLO2), 'UNITS', AXIS2,
     :   UNITS2, STATUS )
      CALL CHR_UCASE( UNITS1 )
      CALL CHR_UCASE( UNITS2 )
      LEN1 = CHR_LEN( UNITS1 )
      LEN2 = CHR_LEN( UNITS2 )
      IPT1 = 0
      DO 1 I = 1, LEN1
         IF ( UNITS1(I:I) .NE. ' ' ) THEN
            IPT1 = IPT1 + 1
            UNITS1(IPT1:IPT1) = UNITS1(I:I)
         END IF
 1    CONTINUE
      IPT2 = 0
      DO 2 I = 1, LEN2
         IF ( UNITS2(I:I) .NE. ' ' ) THEN
            IPT2 = IPT2 + 1
            UNITS2(IPT2:IPT2) = UNITS2(I:I)
         END IF
 2    CONTINUE
      IF ( IPT1 .NE. IPT2 ) THEN
         MATCH = .FALSE.
      ELSE
         IF (IPT1 .GT. 0 ) THEN
            MATCH = UNITS1(:IPT1) .EQ. UNITS2(:IPT2)
         ELSE
            MATCH = .TRUE.
         END IF
      END IF
      IF ( .NOT. MATCH ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E026', 'DSA1_MATAX: The axis ' //
     :      '^FDA_T002 in reference ^FDA_T001 and the axis ' //
     :      '^FDA_T008 in reference ^FDA_T007 differ ' //
     :      'in their unit strings.', STATUS )
         GO TO 500
      END IF

*  See if the axis data arrays exist.
      CALL DSA1_SKAX( SLO1, AXIS1, EXST1, STATUS )
      CALL DSA1_SKAX( SLO2, AXIS2, EXST2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If neither exist, we can quit now. If both exist, we need to compare
*  their data. If only one exists, we could just check it against the
*  numbers 1..N, but in the interests of keeping this routine simple (!)
*  we map both anyway, knowing that DSA_MAP_AXIS_DATA will give us the
*  numbers 1..N, and trying not to care about the overheads.
      IF ( .NOT. EXST1 .AND. .NOT. EXST2 ) GO TO 500

*  If we get here, at least one exists. Get dimensions of both arrays.
      CALL DSA1_AXSIZ( SLO1, AXIS1, NDF__MXDIM,
     :   NDIM1, DIM1, NELM1, STATUS )
      CALL DSA1_AXSIZ( SLO2, AXIS2, NDF__MXDIM,
     :   NDIM2, DIM2, NELM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Compare them. First, the number of dimensions.
      IF ( NDIM2 .NE. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E027', 'DSA1_MATAX: The axis ' //
     :      '^FDA_T002 in reference ^FDA_T001 and the axis ' //
     :      '^FDA_T008 in reference ^FDA_T007 differ ' //
     :      'in their dimensionality.', STATUS )
         GO TO 500
      END IF

*  Now the actual dimensions.
      OK = .TRUE.
      DO 3 I = 1, NDIM1
         IF ( DIM1(I) .NE. DIM2(I) ) OK = .FALSE.
 3    CONTINUE
      IF ( .NOT. OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E028', 'DSA1_MATAX: The axis ' //
     :      '^FDA_T002 in reference ^FDA_T001 and the axis ' //
     :      '^FDA_T008 in reference ^FDA_T007 differ ' //
     :      'in one or more dimensions.', STATUS )
         GO TO 500
      END IF

*  OK, they're the same size. Now see if they're the same data.
      CALL DSA1_MAPCEN( SLO1, AXIS1, 'READ', 'FLOAT',
     :   PNTR1, MSLOT1, STATUS )
      CALL DSA1_MAPCEN( SLO2, AXIS2, 'READ', 'FLOAT',
     :   PNTR2, MSLOT2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA2_COMPAF( %VAL( CNF_PVAL(PNTR1) ),
     :                  %VAL( CNF_PVAL(PNTR2) ), NELM1, NERR, ERROR1 )
      IF ( NERR .GT. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E029', 'DSA1_MATAX: The axis ' //
     :      '^FDA_T002 in reference ^FDA_T001 and the axis ' //
     :      '^FDA_T008 in reference ^FDA_T007 differ ' //
     :      'in one or more pixel centre values.', STATUS )
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( MSLOT1 .NE. 0 ) CALL DSA1_UNMAP( MSLOT1, STATUS )
      IF ( MSLOT2 .NE. 0 ) CALL DSA1_UNMAP( MSLOT2, STATUS )
      END




      SUBROUTINE DSA2_COMPAF( ARRAY1, ARRAY2, NELM, NERR, ERROR1 )
C
C                           D S A _ C O M P A F
C
C  Routine name:
C     DSA_COMPAF
C
C  Function:
C     Compares the contents of two floating point arrays.
C
C  Description:
C     This routine compares two floating point arrays, reporting the
C     number of elements that differ and the number of the first
C     discrepant element.  Some allowance is made for rounding error.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_COMPAF (ARRAY1,ARRAY2,NELM,NERR,ERROR1)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ARRAY1        (Real array,ref) First of the two arrays.
C     (>) ARRAY2        (Real array,ref) Second of the two arrays.
C     (>) NELM          (Integer,ref) Number of array elements.
C     (<) NERR          (Integer,ref) Number of discrepant elements.
C     (<) ERROR1        (Integer,ref) Number of first discrepant element.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     16th July 1987   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+

      IMPLICIT NONE

      INTEGER NELM
      REAL ARRAY1( NELM )
      REAL ARRAY2( NELM )
      INTEGER NERR
      INTEGER ERROR1

      REAL DIFF                   ! Absolute discrepancy
      REAL DIFLIM                 ! Allowed discrepancy
      INTEGER I                   ! Loop index

      NERR   = 0
      ERROR1 = 0
      DO 1 I = 1, NELM
         IF ( ARRAY1(I) .NE. ARRAY2(I) ) THEN
            DIFF   = ABS(ARRAY1(I)-ARRAY2(I))
            DIFLIM = ABS(ARRAY1(I)+ARRAY2(I)) * 0.00005
            IF ( DIFF .GT. DIFLIM ) THEN
               NERR = NERR + 1
               IF ( NERR .EQ. 1 ) ERROR1 = I
            END IF
         END IF
 1    CONTINUE

      END
