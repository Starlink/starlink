      SUBROUTINE IMG_FGET( FILEN, MODE, MAXX, MAXY, GDAT, DATA,
     :                     BADVAL, GERR, ERROR, GQUAL, QUAL,
     :                     GAXES, XAX, YAX, GTEXT, TEXT,
     :                     NX, NY, ID, STATUS )
*+
*  Name:
*     IMG_FGET

*  Purpose:
*     Get selected data components from an image

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL IMG_FGET( FILEN, MODE, MAXX, MAXY, GDAT, DATA,
*                    BADVAL, GERR, ERROR, GQUAL, QUAL,
*                    GAXES, XAX, YAX, NX, NY, ID, STATUS )

*  Description:
*     High level access to images. Extracts selected bits of image data
*     into user arrays.

*  Arguments:
*     FILEN = CHARACTER*(*) (given)
*        The filename of the image to open. May be of the form $<n> where
*        <n> is the number of a command line argument.
*     MODE = CHARACTER*(*) (given)
*        The file access mode. Must be READ or UPDATE.
*     MAXX = INTEGER (given)
*        Maximum size of X axis of image
*     MAXY = INTEGER (given)
*        Maximum size of Y axis of image
*     GDAT = LOGICAL (given)
*        Get the primary data in the image?
*     DATA[MAXX,MAXY] = (returned)
*        The primary image data, if GDAT is true. It is an error for data
*        not to be present if GDAT is true.
*     BADVAL = REAL (given)
*        Bad value used to flag data and errors
*     GERR = LOGICAL (given)
*        Get the data errors?
*     ERROR[MAXX,MAXY] = (returned)
*        The image errors, if GERR is true. If none are present then
*        then returned array is filled with BADVAL.
*     GQUAL = LOGICAL (given)
*        Get the data quality?
*     QUAL[MAXX,MAXY] = LOGICAL (returned)
*        The data quality values, if GQUAL is true. True means a pixel is
*        ok, false means bad pixel. If no quality is present all values
*        are set to true.
*     GAXES = LOGICAL (given)
*        Get the bin centres?
*     XAX[MAXX] = REAL (returned)
*        The X axis bin centres, if GAXES is true. If not present then filled
*        with sequence 1..NX
*     YAX[MAXY] = REAL (returned)
*        The Y axis bin centres, if GAXES is true. If not present then filled
*        with sequence 1..NY
*     GTEXT = LOGICAL (given)
*        Get text associated with image
*     TEXT[] = CHARACTER*(*) (given)
*        Text strings associated with the image. If GTEXT is specified true
*        then TEXT should be an array at least 7 elements long. The strings
*        returned are, 1) dataset title, 2) pixel data label, 3) pixel data
*        units, 4) X axis label, 5) X axis units, 6) Y axis label and 7) Y
*        axis units. If GTEXT is false simply pass any old string - it will
*        not be accessed.
*     NX = INTEGER (returned)
*        Actual size of image X axis
*     NY = INTEGER (returned)
*        Actual size of image Y axis
*     ID = INTEGER (returned)
*        The ADI identifier to the file
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
*     IMG Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/img.html

*  Keywords:
*     package:img, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		FILEN, MODE
      INTEGER			MAXX, MAXY
      LOGICAL			GDAT, GERR, GQUAL, GAXES, GTEXT
      REAL			BADVAL

*  Arguments Returned:
      INTEGER			NX, NY, ID
      REAL			DATA(MAXX,MAXY),ERROR(MAXX,MAXY)
      REAL			XAX(MAXX), YAX(MAXY)
      LOGICAL			QUAL(MAXX,MAXY)
      CHARACTER*(*)		TEXT(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER			NTXT			! # text items
        PARAMETER		( NTXT = 7 )

*  Local Variables:
      INTEGER			DIMS(2)			! Actual dimensions
      INTEGER			I, J			! Loop variables
      INTEGER			LMAXD(2)		! Buffer size
      INTEGER			NDIM			! Dimensionality

      LOGICAL			OK			! Component present?

*  Local Data:
      CHARACTER*20              TITEM(NTXT)
      DATA			TITEM/'Title','Label','Units',
     :                                'X_Axis_Label', 'X_Axis_Units',
     :                                'Y_Axis_Label', 'Y_Axis_Units'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure data models are loaded
      CALL ADI_REQPKG( 'dsmodels', STATUS )

*  Open high level data file
      CALL UDI0_FOPEN( FILEN, 'XYimage', MODE, ID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'IMG_FGET: Unable to open image', STATUS )
        GOTO 99
      END IF

*  Extract dimensions
      CALL BDI_GETSHP( ID, 2, DIMS, NDIM, STATUS )
      NX = DIMS(1)
      NY = DIMS(2)

*  Check shape will fit into user buffers. Not an issue if caller doesn't
*  require the data, errors, quality or axes
      IF ( GDAT .OR. GERR .OR. GQUAL .OR. GAXES ) THEN

        IF ( STATUS .EQ. SAI__OK ) THEN
          IF ( (DIMS(1) .GT. MAXX) .OR. (DIMS(2).GT.MAXY) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'BX', MAXX )
            CALL MSG_SETI( 'BY', MAXY )
            CALL MSG_SETI( 'NX', NX )
            CALL MSG_SETI( 'NY', NY )
            CALL ERR_REP( ' ', 'IMG_FGET: Buffer supplied (^BX x ^BY)'/
     :                 /' is too small for image (^NX x ^NY)', STATUS )
          END IF
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END IF

*  Move buffer sizes to local array
      LMAXD(1) = MAXX
      LMAXD(2) = MAXY

*  Get the various bits the caller has requested
      IF ( GDAT ) THEN
        CALL BDI_CHK( ID, 'Data', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GETR( ID, 'Data', 2, LMAXD, DATA, DIMS, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'IMG_FGET: No data present in image',
     :                  STATUS )
        END IF
      END IF

*  Caller wants errors?
      IF ( GERR ) THEN
        CALL BDI_CHK( ID, 'Error', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GETR( ID, 'Error', 2, LMAXD, ERROR, DIMS, STATUS )
        ELSE
          DO J = 1, NY
            DO I = 1, NX
              ERROR(I,J) = BADVAL
            END DO
          END DO
        END IF
      END IF

*  Caller wants quality?
      IF ( GQUAL ) THEN
        CALL BDI_CHK( ID, 'LogicalQuality', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GETL( ID, 'LogicalQuality', 2, LMAXD, QUAL, DIMS,
     :                   STATUS )
        ELSE
          DO J = 1, NY
            DO I = 1, NX
              QUAL(I,J) = .TRUE.
            END DO
          END DO
        END IF
      END IF

*  Dataset axes
      IF ( GAXES ) THEN

*    X axis first
        CALL BDI_CHK( ID, 'X_Axis_Data', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GET1R( ID, 'X_Axis_Data', MAXX, XAX, DIMS(1),
     :                    STATUS )
        ELSE
          CALL ARR_REG1R( 1.0, 1.0, NX, XAX, STATUS )
        END IF

*    Then Y axis
        CALL BDI_CHK( ID, 'Y_Axis_Data', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GET1R( ID, 'Y_Axis_Data', MAXY, YAX, DIMS(2),
     :                    STATUS )
        ELSE
          CALL ARR_REG1R( 1.0, 1.0, NY, YAX, STATUS )
        END IF

      END IF

*  Dataset text items. Return blanks if missing
      IF ( GTEXT ) THEN
        DO I = 1, NTXT
          CALL BDI_CHK( ID, TITEM(I), OK, STATUS )
          IF ( OK ) THEN
            CALL BDI_GET0C( ID, TITEM(I), TEXT(I), STATUS )
          ELSE
            TEXT(I) = ' '
          END IF
        END DO
      END IF

*  Exit point
 99   CONTINUE

      END
