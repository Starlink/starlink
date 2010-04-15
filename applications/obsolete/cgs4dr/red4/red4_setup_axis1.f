*+ RED4_SETUP_AXIS1
      SUBROUTINE RED4_SETUP_AXIS1 (AXIS1, INDEX, AXIS1_DIM,
     :   DCOLUMNS, DETINCR, DETNINCR, GLAMBDA, GDISP, STATUS)
*    Description :
*     This routine sets up the X axis array for a data set which is
*     oversampled in the X direction. An INDEX array is also filled with
*     a look-up table indicating how pixels in the individual
*     integrations at each detector position map onto pixels in the main
*     data set. The X axis array may be filled with pixel co-ordinates
*     or wavelength values according to the values of GLAMBDA and GDISP.
*    Invocation :
*     CALL RED4_SETUP_AXIS1 (AXIS1, INDEX, AXIS1_DIM, DCOLUMNS, DETINCR,
*    :   DETNINCR, GLAMBDA, GDISP, STATUS)
*    Parameters :
*     AXIS1( AXIS1_DIM )               = DOUBLE PRECISION( WRITE )
*           The X axis array to be set up.
*     INDEX( DCOLUMNS, DETNINCR )      = INTEGER( WRITE )
*           Index array containing a look-up table to convert integration
*           element + detector position into observation element.
*     RANK( AXIS1_DIM )                = INTEGER( WRITE )
*           A work array to hold ordering information.
*     AXIS1_DIM                        = INTEGER( READ )
*           Dimension of the AXIS1 array.
*     DCOLUMNS                         = INTEGER( READ )
*           The number of columns on the detector.
*     DETINCR                          = REAL( READ )
*           The co-ordinate increment required per step in pixels
*           (normally 1.0/NUMBER_STEPS, but can have other values)
*     DETNINCR                         = INTEGER( READ )
*           The number of detector positions (i.e. the oversampling
*           factor).
*     GLAMBDA                          = REAL( READ )
*           The wavelength corresponding to pixel co-ordinate 31.0.
*           (If this parameter is set to 31.0, the axis1 array will
*           be filled with pixel co-ordinates).
*     GDISP                            = REAL( READ )
*           The wavelength increment per increment of 1.0 in the
*           pixel co-ordinate.
*           (If this parameter is set to 1.0, the axis1 array will
*           be filled with pixel co-ordinates).
*     STATUS                           = INTEGER( UPDATE )
*           Global status
*    Method :
*    Deficiencies :
*     The Starlink convention on the labelling of axes, described in
*     SSN/22, states that the left hand side of the first bin should
*     be labelled 0.0.
*     However, the Figaro convention, described in the Figaro user guide,
*     states that the left hand side of the first bin should be 0.5.
*     This routine now uses the Figaro convention.
*
*     It would probably have been better if the setting up of the AXIS1
*     and INDEX arrays had been done in separate routines.
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*           1989?: Original version.                                   (JFL)
*      9-Nov-1990: Source code and NAG manual examined, to determine
*                  what this routine does! Description, Parameters,
*                  Method, Deficiencies and History sections added.
*                  Source comments improved and code spaced out. Checks
*                  on the value of IFAIL returned by the NAG routines
*                  added. "REAL*8" changed to "DOUBLE PRECISION". POS_1
*                  declared DOUBLE PRECISION for consistency.          (SMB)
*      9-Nov-1990: Tentatively modified so that the axis is labelled
*                  according to the Figaro co-ordinate convention.     (SMB)
*     21-Nov-1990: GLAMBDA and GDISP parameters added, so this routine
*                  can also be used to fill the axis1 array with
*                  wavelength values. This same routine is used because
*                  it is necessary to call it to fill the index array
*                  anyway.                                             (SMB)
*     22-Feb-1991: Loading up the X axis, using a NAG routine to sort
*                  it and produce a RANK array, which then magically
*                  contains exactly the right numbers for the INDEX
*                  array, is not only obscure, but totally incompatible
*                  with "supersampling" (scanning the detector over
*                  more than one pixel to remove bad pixels). At last
*                  this is a good excuse to throw away the contents of
*                  this routine and start again from scratch !
*                  Completely rewritten (at summit - please check).   (SMB)
*     23-Feb-1991: Code checked. Some mistakes corrected.             (SMB)
*     23-Feb-1991: More mistakes corrected!                           (SMB)
*     23-Feb-1993: Conform to error strategy                          (PND)
*     17-Apr-1995: Add CP  for central wavelength                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
*    Import :
      INTEGER
     :  AXIS1_DIM,                 ! Dimension of full observation axis1 array
     :  DCOLUMNS,                  ! Number of detector columns
     :  DETNINCR                   ! Number of detector positions
      REAL
     :  DETINCR,                   ! Increment of X pixel coordinate per
     :                             !   detector step
     :  GLAMBDA,                   ! Wavelength corresponding to pixel
     :                             !   co-ordinate 31.0
     :  GDISP                      ! Wavelength increment per pixel
*    Export :
      REAL
     :  AXIS1( AXIS1_DIM )               ! Main axis1 array of reduced observation
      INTEGER
     :  INDEX( DCOLUMNS, DETNINCR )      ! Index array for detector pixels
*                                        !   at all the detector positions
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER
     :  OVERSAMPLING,                    ! Oversampling factor
     :  SUPERSAMPLING,                   ! Supersampling factor
     :  OFFSET,                          ! Detector position offset
     :  I, J, K                          ! Loop variables
      REAL
     :  CP,                              ! Central pixel
     :  FIRST_BIN                        ! The X-coordinate of the first bin
*                                        !   according to the Figaro convention
*-

*   Check for an error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Determine central pixel
      CP = REAL( DCOLUMNS / 2 )

*   Determine if "supersampling" is being used (i.e. scanning the
*   detector over more than one pixel  to remove bad pixels).
*   For normal operation this will be 1. For scanning over N pixels
*   it will be N. (If DETNINCR is 1 there is no SUPERSAMPLING,
*   regardless of DETINCR).
      IF ( DETNINCR .GT. 1 ) THEN

         SUPERSAMPLING = NINT( DETNINCR * DETINCR )
      ELSE

         SUPERSAMPLING = 1
      END IF

*   Calculate the oversampling factor (i.e. the number of detector
*   positions in between whole pixels). (This has already been checked
*   to be a whole number).
      OVERSAMPLING = DETNINCR / SUPERSAMPLING

*   Calculate the X co-ordinate of the first bin in pixels, such that
*   centre of the Nth group of NUMBER_STEPS bins will have X co-ordinate
*   REAL(N), which is the Figaro convention.
      FIRST_BIN = REAL( OVERSAMPLING + 1 ) / REAL( 2 * OVERSAMPLING )

*   Fill the X axis array with the required values. The increment between
*   X axis values will be the same as that between detector positions.
      DO I = 1, AXIS1_DIM

         AXIS1(I) = FIRST_BIN + (I-1) * DETINCR
      END DO

*   If necessary, convert all the AXIS1 values from pixels into wavelength.
*   (This is only necessary if GLAMBDA differs significantly from CP
*   and GDISP from 1.0).
      IF ( ( ABS(GLAMBDA - CP) .GT. 1.0E-10 ) .AND.
     :     ( ABS(GDISP   - 1.0)  .GT. 1.0E-10 ) ) THEN

         DO I = 1, AXIS1_DIM

            AXIS1(I) = GLAMBDA + (AXIS1(I) - CP) * GDISP
         END DO
      END IF

*   The following loop deals with the extra index array entries
*   which are necessary for "supersampling". Without "supersampling"
*   it will only be executed once.
      DO K = 0, SUPERSAMPLING-1

*      Determine the offset to add to the detector position index
*      at this supersampling level.
         OFFSET = K * OVERSAMPLING

*      Scan through the ordinary sampled detector positions.
         DO J = 1, OVERSAMPLING

*         Scan through each integration column number, and load the
*         index array with the corresponding observation column number.
            DO I = 1, DCOLUMNS

               INDEX( I, OFFSET+J ) = J + OVERSAMPLING * (K+I-1)
            END DO
         END DO
      END DO

      END
