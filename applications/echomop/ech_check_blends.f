      SUBROUTINE ECH_CHECK_BLENDS(
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           MAX_PERM_FTRS,
     :           IDENTIFIED_FTRS,
     :           WAVELENGTHS,
     :           FEATURE_STATUS,
     :           DELTA,
     :           BLENDS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CHECK_BLENDS

*  Purpose:
*     Check for potential blends amongst identified arc lines.

*  Description:
*     This routine uses the feature lists to check for potential blends which
*     have so far been treated as single-point features.  The status of the
*     features it recorded by setting flag bits in the array FEATURE_STATUS()
*     for each feature.

*  Invocation:
*     CALL ECH_CHECK_BLENDS(
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    MAX_PERM_FTRS,
*     :    IDENTIFIED_FTRS,
*     :    WAVELENGTHS,
*     :    FEATURE_STATUS,
*     :    DELTA,
*     :    BLENDS,
*     :    STATUS
*     :   )

*  Arguments:
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features per order.
*     IDENTIFIED_FTRS = INTEGER (Given)
*        Number of identified features.
*     WAVELENGTHS = REAL( MAX_PERM_FEATURES ) (Given)
*        Wavelengths of features from fit.
*     FEATURE_STATUS = INTEGER( MAX_PERM_FEATURES ) (Given)
*        Flag words for feature stati.
*     DELTA = REAL (Given)
*        Maximum difference in wavelength for unblended features.
*     BLENDS = INTEGER (Given)
*        Number of potential blends found.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     FTR_LIST = REAL( MAX_FEATURES ) (Given)
*        List of known arc line wavelengths.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Find the maximum and minimum indices into ftr_list referenced
*     Loop through the identified features wavelengths
*         Loop through feature list entries
*             If two features in the list are so close to each other
*                   in wavelength, that they may be blended then
*                Increment possibly matched feature count
*             Endif
*         End loop
*         Clear 'not-processed' flag
*         If no possible identifications for this feature in the list then
*            Flag observed feature as 'Not automatically identifiable'
*         Else if only 1 possible match in feature list then
*            Flag observed feature as 'Automatically identified'
*         Else  (its a possible blend)
*            Increment count, and flag feature as 'Possible blend'
*            List potential components
*         Endif
*     End loop
*     Report on success rate

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_FEATURE.INC'

*  Arguments Given:
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )
      INTEGER MAX_PERM_FTRS
      INTEGER IDENTIFIED_FTRS
      REAL WAVELENGTHS( MAX_PERM_FTRS )       ! Fitted wavelengths of features.
      INTEGER FEATURE_STATUS( MAX_PERM_FTRS ) ! Status flags for features.
      REAL DELTA
      INTEGER BLENDS

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER II
      INTEGER COUNT
      INTEGER MIN_INDEX
      INTEGER MAX_INDEX
      INTEGER TOTLIN
*.

*  Find the maximum and minimum indices into ftr_list referenced.
      STATUS = 0
      MIN_INDEX = 1
      MAX_INDEX = 1
      DO I = 1, MAX_FEATURES
         IF ( FTR_LIST( I ) .GT. 0.0 ) THEN
            IF ( FTR_LIST( I ) .LT. WAVELENGTHS( 1 ) ) MIN_INDEX = I

*  MJC 22-APR-1996.
*
*  I believe this is a bug - but the line ID task is broken somewhere
*  else so I can't check it.
*
*  I think the following code line should be ".GE." -
*  the code line above should probably be ".LE." as well...
            IF ( FTR_LIST( I ) .LE. WAVELENGTHS( IDENTIFIED_FTRS ) )
     :         MAX_INDEX = I
         END IF
      END DO
      TOTLIN = IDENTIFIED_FTRS

*  Loop through the identified features wavelengths.
      I = 1
      DO WHILE ( I .LE. MAX_PERM_FTRS .AND. WAVELENGTHS( I ) .GT. 0.0 )

*      Loop through feature list entries.
          COUNT = 0
          DO II = MIN_INDEX, MAX_INDEX

*         If two features in the list are so close to each other
*         in wavelength, that they may be blended then increment
*         possibly-matched feature count.
             IF ( ABS ( FTR_LIST( II ) -
     :            WAVELENGTHS( I ) ) .LE. DELTA ) THEN
                COUNT = COUNT + 1
                BLENDS = COUNT
             END IF
          END DO

*      Clear 'not-processed' flag.
          FEATURE_STATUS( I ) = IAND( FEATURE_STATUS( I ),
     :                          NOT( FTR_POSIB_BLEND ) )

*      If no possible identifications for this feature in the list then.
          IF ( COUNT .EQ. 0 ) THEN

*         Flag observed feature as 'Not automatically identifiable'.
             FEATURE_STATUS( I ) = IOR( FEATURE_STATUS( I ),
     :                             FTR_NOAUTO_IDEN )

*      Else if only 1 possible match in feature list then.
          ELSE IF ( count .EQ. 1 ) THEN

*         Flag observed feature as 'Automatically identified'.
             FEATURE_STATUS( I ) = IOR( FEATURE_STATUS( I ),
     :                             FTR_AUTO_IDENT )

*      Otherwise its a possible blend - increment count, and flag feature.
          ELSE
             FEATURE_STATUS( I ) = IOR( FEATURE_STATUS( I ),
     :                             FTR_POSIB_BLEND )
             TOTLIN = TOTLIN + COUNT - 1
          END IF
          I = I + 1
      END DO

      END
