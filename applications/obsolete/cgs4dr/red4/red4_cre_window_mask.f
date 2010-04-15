*+  RED4_CRE_WINDOW_MASK - Create a bad pixel mask by windowing
      SUBROUTINE RED4_CRE_WINDOW_MASK( STATUS )
*    Description :
*     This routine creates a bad pixel mask by flagging all the
*     locations outside a specified window as bad. The routine
*     is useful for masking off areas which are unilluminated.
*    Invocation :
*     CALL RED4_CRE_WINDOW_MASK( STATUS )
*    Parameters :
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     23-May-1990: Original version, from RED4_CRE_THRESH_MASK.  (SMB)
*      7-Aug-1990: Bug fix. Ensure IMIN, IMAX, JMIN and JMAX are
*                  not outside either boundary of the array.     (SMB)
*      3-Sep-1990: Modified to make use of FITS structure.       (SMB)
*     25-Sep-1990: The specification for handling bad pixel
*                  masks has now changed. Masks will now NOT
*                  all be the same size as the array, as in
*                  some circumstances only part of the array
*                  may be read out. Code modified so the
*                  required size for the pixel mask is obtained
*                  from parameters. Superfluous declarations
*                  removed.                                      (SMB)
*     12-Nov-1990: Modified to write axis info to mask.          (SMB)
*     28-Nov-1990: Silly typing mistake fixed.                   (SMB)
*     18-Feb-1993: Conform to error strategy                     (PND)
*     11-Jan-1994: Allow NDFs by RESHAPE_DATA                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER NINFO              ! Number of information items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      INTEGER
     :  WINDOW_IMIN,             ! Minimum column for window
     :  WINDOW_IMAX,             ! Maximum column for window
     :  WINDOW_JMIN,             ! Minimum row for window
     :  WINDOW_JMAX,             ! Maximum row for window
     :  NDIM,                    ! Number of dimensions in mask data array
     :  DIMS( MAXDIM ),          ! Dimensions of mask data array
     :  ADDRESS,                 ! Address of mapped arrays
     :  MASK_SLOT,               ! Reference slot for mapped mask data
     :  MASK_PTR,                ! Address of mask data.
     :  GOOD,                    ! Data quality value for good data
     :  BAD,                     ! Data quality value for bad data
     :  NGOOD,                   ! Number of good pixels
     :  NBAD                     ! Number of bad pixels
      CHARACTER*80
     :  MASK                     ! Name of output data structure for mask
      CHARACTER*32
     :  CHAR_ARRAY( NINFO ),     ! Labels and units for the axes.
     :  DATA_INFO( NINFO )       ! Labels and units for the data.
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the output structure.
      CALL PAR_GET0C( 'MASK', MASK, STATUS )

*   Obtain the required size for the mask. (This should be the same
*   as the size of the detector array).
      NDIM = 2
      CALL PAR_GET0I( 'NCOLUMNS', DIMS(1), STATUS )
      CALL PAR_GET0I( 'NROWS', DIMS(2), STATUS )

*   Obtain the values to be used for good and bad data quality.
      CALL PAR_GET0I( 'GOOD', GOOD, STATUS )
      CALL PAR_GET0I( 'BAD', BAD, STATUS )

*   Check these parameters have been obtained succesfully
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA.
         CALL DSA_OPEN( STATUS )

*      Open the mask template file CGS4_TEMPLATES:MASK_TEMPLATE.DST
*      and use this as a basis for creating the new mask structure
         CALL DSA_NAMED_INPUT( 'MASK_TEMP', MASK_TEMPLATE, STATUS )
         CALL DSA_NAMED_OUTPUT( 'MASK', MASK, 'MASK_TEMP', 0, 1, STATUS )
         CALL DSA_USE_QUALITY( 'MASK', STATUS )

*      Force the data array in the mask structure to be the specified size.
         CALL DSA_RESHAPE_DATA( 'MASK', 'MASK', NDIM,
     :     DIMS, STATUS )

*      Map the data array in the MASK structure as a byte array.
         CALL DSA_MAP_DATA( 'MASK', 'WRITE', 'BYTE', ADDRESS,
     :     MASK_SLOT, STATUS )
         MASK_PTR = ADDRESS

*      Check this has worked
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Use the size of the data array as defaults for obtaining
*         the extent of the window. (The default window would fill
*         the whole mask with "good" values).
            CALL PAR_DEF0I( 'WINDOW_IMIN', 1, STATUS )
            CALL PAR_DEF0I( 'WINDOW_IMAX', DIMS(1), STATUS )
            CALL PAR_DEF0I( 'WINDOW_JMIN', 1, STATUS )
            CALL PAR_DEF0I( 'WINDOW_JMAX', DIMS(2), STATUS )

*         Obtain the required window extent.
            CALL PAR_GET0I( 'WINDOW_IMIN', WINDOW_IMIN, STATUS )
            CALL PAR_GET0I( 'WINDOW_IMAX', WINDOW_IMAX, STATUS )
            CALL PAR_GET0I( 'WINDOW_JMIN', WINDOW_JMIN, STATUS )
            CALL PAR_GET0I( 'WINDOW_JMAX', WINDOW_JMAX, STATUS )

*         Check this has worked
            IF ( STATUS .EQ. ADAM__OK ) THEN

*            Ensure the given window extents are within the
*            limits of the array. If any are outside, correct
*            them and issue a warning.
               IF ( WINDOW_IMIN .LT. 1 ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Left hand column '/
     :              /'number was outside left array bounds - Reset',
     :              STATUS )

                  WINDOW_IMIN = 1
               END IF

               IF ( WINDOW_IMIN .GT. DIMS(1) ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Left hand column '/
     :              /'number was outside right array bounds - Reset',
     :              STATUS )

                  WINDOW_IMIN = DIMS(1)
               END IF

               IF ( WINDOW_IMAX .LT. 1 ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Right hand column '/
     :              /'number was outside left array bounds - Reset',
     :              STATUS )

                  WINDOW_IMAX = 1
               END IF

               IF ( WINDOW_IMAX .GT. DIMS(1) ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Right hand column '/
     :              /'number was outside right array bounds - Reset',
     :              STATUS )

                  WINDOW_IMAX = DIMS(1)
               END IF

               IF ( WINDOW_JMIN .LT. 1 ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - bottom column '/
     :              /'number was outside bottom array bounds - '/
     :              /'Reset', STATUS )

                  WINDOW_JMIN = 1
               END IF

               IF ( WINDOW_JMIN .GT. DIMS(2) ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - bottom column '/
     :              /'number was outside top array bounds - Reset',
     :              STATUS )

                  WINDOW_JMIN = DIMS(2)
               END IF

               IF ( WINDOW_JMAX .LT. 1 ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Top column '/
     :              /'number was outside bottom array bounds - '/
     :              /'Reset', STATUS )

                  WINDOW_JMAX = 1
               END IF

               IF ( WINDOW_JMAX .GT. DIMS(2) ) THEN

                  CALL MSG_OUT( ' ', 'WARNING - Top column '/
     :              /'number was outside top array bounds - Reset',
     :              STATUS )

                  WINDOW_JMAX = DIMS(2)
               END IF

*            Set the elements in the mask array inside the window
*            to "good" and those outside the window to "bad".
               CALL RED4_QWINDOW( DIMS(1), DIMS(2), WINDOW_IMIN,
     :           WINDOW_IMAX, WINDOW_JMIN, WINDOW_JMAX,
     :           GOOD, BAD, %val(MASK_PTR), NGOOD, NBAD, STATUS )

*            Write the object name 'Bad pixel mask', into
*            the output data structure.
               CALL DSA_SET_OBJECT( 'MASK', 'Bad pixel mask', STATUS )

*            Write the data label 'Data Quality' into the
*            output data structure.
               DATA_INFO(1) = ' '
               DATA_INFO(2) = 'Data Quality'
               CALL DSA_SET_DATA_INFO( 'MASK', NINFO, DATA_INFO,
     :           0, 0.0D0, STATUS )

*           Set up the label and units for the axes:
*            X axis
               CHAR_ARRAY(1) = ' '
               CHAR_ARRAY(2) = 'Detector columns'
               CALL DSA_SET_AXIS_INFO( 'MASK', 1, NINFO,
     :           CHAR_ARRAY, 0, 0.0D0, STATUS )

*            Y axis
               CHAR_ARRAY(1) = ' '
               CHAR_ARRAY(2) = 'Detector rows'
               CALL DSA_SET_AXIS_INFO( 'MASK', 2, NINFO,
     :           CHAR_ARRAY, 0, 0.0D0, STATUS )

*            Record how this mask was generated in the FITS structure.
               CALL DSA_PUT_FITS_C( 'MASK', 'COMMENT', 'Created by '/
     :           /'defining a window.', ' ', STATUS )

*            Record the window limits in the output structure.
               CALL DSA_PUT_FITS_I( 'MASK', 'WIMIN', WINDOW_IMIN,
     :           ' ', STATUS )
               CALL DSA_PUT_FITS_I( 'MASK', 'WIMAX', WINDOW_IMAX,
     :           ' ', STATUS )
               CALL DSA_PUT_FITS_I( 'MASK', 'WJMIN', WINDOW_JMIN,
     :           ' ', STATUS )
               CALL DSA_PUT_FITS_I( 'MASK', 'WJMAX', WINDOW_JMAX,
     :           ' ', STATUS )

*            Record the number of good and bad pixels contained.
               CALL DSA_PUT_FITS_I( 'MASK', 'NGOOD', NGOOD,
     :           ' ', STATUS )
               CALL DSA_PUT_FITS_I( 'MASK', 'NBAD', NBAD,
     :           ' ', STATUS )

*            If everything has worked, report the numbers of
*            good and bad pixels. (Note that the following will
*            only produce output if STATUS is ok).
               CALL MSG_SETI( 'NGOOD', NGOOD )
               CALL MSG_SETI( 'NBAD', NBAD )
               CALL MSG_OUT( ' ', 'Bad pixel mask '/
     :           /'created. ^NGOOD good pixels, ^NBAD '/
     :           /'bad pixels.', STATUS )
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_CRE_WINDOW_MASK: '/
     :           /'Failed to get window extent parameters', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_CRE_WINDOW_MASK: '/
     :        /'Failed to open and map data '/
     :        /'structures', STATUS )
         END IF

*      Close DSA. It will create the new mask structure and will
*      also unmap all the arrays mapped above.
*      (The routine will attempt to work even if the status on entry is bad).
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CRE_WINDOW_MASK: '/
     :     /'Failed to get %MASK, %NCOLUMNS, '/
     :     /'%NROWS, %GOOD and %BAD parameters', STATUS )
      END IF

      END
