      SUBROUTINE RDNDF( COMM, INDF, WORV, TITLE, STATUS )
*+
*  Name:
*     RDNDF

*  Purpose:
*     Read data from an NDF into the current arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDNDF( COMM, INDF, WORV, TITLE, STATUS )

*  Description:
*     If the supplied NDF is not 1-dimensional, an error is
*     reported. If the DATA array is too large to fit into the common
*     data arrays, a warning is given and as much as there is room for
*     is copied to the Y array. The associated AXIS CENTRE values are
*     copied to the X array. If an extension called DIPSO_EXTRA is
*     found, then it can contain the following components:
*
*     1) Integer BREAKS[ NBREAK ] - An array of pixel indices at which
*        breaks occur in the spectrum. The last pixel in the array
*        should always be marked as a break.
*
*     2) Real WORV - If the AXIS units are km/s, WORV is the rest
*        wavelength (in Angstroms) to which the velocities are
*        referenced, divided by the speed of light (in km/s). If the
*        AXIS units are not km/s, then WORV should be set to 1.0. If
*        the DIPSO_EXTRA extension does not contain this component,
*        then a warning message is givem, and a value of 1.0 is
*        assumed.
*
*     If the NDF does not contain a DIPSO_EXTRA extension, then a
*     warning is given, and default values are used; the BREAKS array
*     is assumed to contain a single value equal to the index of the
*     last pixel, and WORV is assumed to be 1.0.
*     
*     If the supplied NDF contains any bad pixels, these are removed,
*     and the remaining data shunted down to fill the gaps left by the
*     bad pixels. An extra break points is added to the BREAK array for
*     each contiguous section of bad pixels.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The DIPSO command which invoked this routine.
*     INDF = INTEGER (Given)
*        An identifier for the NDF to be read.
*     WORV = REAL (Returned)
*        The WORV value.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The title obtained from the NDF, or a blank if the NDF had no
*        title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the supplied NDF does not contain an AXIS CENTRE component
*     for axis 1, then pixel co-ordinates will be used as "wavelength"
*     values for each pixel.
*     -  The X label stored in common is set to the label associated
*     with AXIS 1.
*     -  The size of the current arrays is set to zero if an error occurs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)  
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        MAXBRK = INTEGER (Read)  
*           The declared size of the break current array.
*        BREAK( MAXBRK ) = INTEGER (Write)
*           The pixel indices at which breaks occur in the X and Y
*           arrays.
*        FLUX( ASIZE1 ) = REAL (Write)
*           The data value at each element.
*        NBREAK = INTEGER (Write)
*           The number of breaks stored in the common BREAK array. The
*           first is stored in BREAK(1) and the last in BREAK(NBREAK).
*        NPOINT = INTEGER (Write)
*           The number of data elements used in FLUX and WAVE, starting
*           at element 1.
*        WAVE( ASIZE1 ) = REAL (Write)
*           The X value (usually wavelength or velocity) at the
*           corresponding element in the FLUX array.

*  Arguments Given:
      CHARACTER COMM*(*)
      INTEGER INDF

*  Arguments Returned:
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER 
     :        XLOC*(DAT__SZLOC)  ! Locator to the DIPSO_EXTRA extension

      INTEGER
     :        I,                 ! Loop count
     :        IERR,              ! Index of first conversion error
     :        IPAXIS,            ! Pointer to mapped AXIS CENTRE array
     :        IPDATA,            ! Pointer to mapped DATA array
     :        LBND,              ! Pixel index at lower bound of NDF
     :        NDIM,              ! No. of dimensions in the NDF
     :        NERR,              ! No. of conversion errors
     :        UBND,              ! Pixel index at upper bound of NDF
     :        WORKI( MAXBRK )    ! Work array

      LOGICAL
     :        THERE              ! Does the named component exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the NDF. An error will be reported if the NDF is
*  not 1 dimensional.
      CALL NDF_BOUND( INDF, 1, LBND, UBND, NDIM, STATUS )

*  Abort if an error has occurred. This ensure that the STATUS check
*  following the call to NDF_XLOC will only pick up errors reported by
*  NDF_XLOC.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  The first task is to obtain the information from the DIPSO_EXTRA
*  extension. Try and obtain a locator to the DIPSO_EXTRA extension.
      CALL NDF_XLOC( INDF, 'DIPSO_EXTRA', 'READ', XLOC, STATUS )

*  If the extension was found succesfully...
      IF( STATUS .EQ. SAI__OK ) THEN

*  See if a component called NSTNPT exists. If it does, then this NDF
*  contains stack data. Annul the extension locator, report an error 
*  and abort if this is the case.
         CALL DAT_THERE( XLOC, 'NSTNPT', THERE, STATUS )
         IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL DAT_ANNUL( XLOC, STATUS )
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'RDNDF_ERR2', 'The NDF ''^NDF'' '//
     :                    'contains DIPSO stack data.', STATUS )
            GO TO 999
         END IF               

*  Get the size and contents of the BREAKS array. An error will be
*  reported if the NDF contains too many breaks to store (i.e. more than
*  MAXBRK).
         CALL CMP_GET1I( XLOC, 'BREAKS', MAXBRK, BREAK, NBREAK, STATUS )

*  See if a component called WORV exists.
         CALL DAT_THERE( XLOC, 'WORV', THERE, STATUS )

*  If it does, get it's value.
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'WORV', WORV, STATUS )

*  Otherwise, assume a value of 1. Warn the user.
         ELSE
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSGOUT( COMM, 'No WORV value found in NDF ''^NDF''. '//
     :                   'Assuming a value of 1.0.', .TRUE., STATUS )
            WORV = 1.0
         END IF

*  Annul the locator to the DIPSO_EXTRA extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  If an error occurred accessing the DIPSO_EXTRA extension, cancel it
*  and store default values for the data items which should have been
*  read from the extension. Warn the user that the NDF was not created
*  by DIPSO. Some commands are used to read non-DIPSO NDFs so don't
*  issue the warning if we are currently performing one of these commands.
      ELSE
         CALL ERR_ANNUL( STATUS )

         IF( COMM .NE. 'SP0RD' .AND.
     :       COMM .NE. 'ATLASRD' ) THEN
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSGOUT( COMM, 'The NDF ''^NDF'' was not created by '//
     :                   'DIPSO. Assuming no breaks and WORV = 1.0', 
     :                   .TRUE., STATUS )
         END IF

         NBREAK = 1
         BREAK( 1 ) = UBND
         WORV = 1.0

      END IF

*  If the lower bound of the NDF is not 1 (for instance if the user
*  specified an NDF section instead of the whole of an NDF), shift the 
*  break points to refer to a lower bound of 1.
      IF( LBND .NE. 1 ) THEN

         DO I = 1, NBREAK
            BREAK( I ) = BREAK( I ) - LBND + 1
         END DO

      END IF

*  Now map the NDFs DATA array.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPDATA, NPOINT,
     :              STATUS )

*  Issue a warning if the DATA array is too big, and restrict the 
*  number of elements transferred.
      IF( NPOINT .GT. ASIZE1 ) THEN
         CALL MSG_SETI( 'NP', NPOINT )
         CALL MSG_SETI( 'AS', ASIZE1)
         CALL MSGOUT( COMM, 'The NDF DATA array has ^NP elements. '//
     :                'The current arrays can only hold ^AS elements.',
     :                .TRUE., STATUS )
         NPOINT = ASIZE1
      END IF

*  Copy the data into the common array. Note, element LBND of the NDFs 
*  DATA array is always placed in element 1 of the returned array. This
*  means that the break points (after the above modification) will refer
*  to the correct elements.
      CALL VEC_RTOR( .FALSE., NPOINT, %VAL( IPDATA ), FLUX, IERR, NERR, 
     :               STATUS )

*  Map the AXIS CENTRE array. This should hold the wavelength (or
*  velocity) at the centre of each data pixel. If there is no AXIS
*  CENTRE array in the NDF, the NDF system will return an array holding
*  the pixel co-ordinate at the centre of each pixel (i.e. ..., 0.5,
*  1.5, 2.5... etc ).
      CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'READ', IPAXIS, NPOINT,
     :               STATUS )

*  Restrict the number of elements transferred.
      IF( NPOINT .GT. ASIZE1 ) NPOINT = ASIZE1

*  Copy the values to the common X array.
      CALL VEC_RTOR( .FALSE., NPOINT, %VAL( IPAXIS ), WAVE, IERR, NERR,
     :               STATUS )

*  Return the NDF title, ensuring that a blank string gets returned if
*  the title is undefined.
      TITLE = ' '
      CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS )

*  Tell the user how many elements were transferred.
      CALL MSG_SETI( 'NP', NPOINT )
      CALL MSGOUT( COMM, '^NP data values read.', .FALSE., STATUS )

*  Call a routine which will remove any pixels which have been flagged
*  as invalid in the NDFS DATA array.
      CALL BADCHK( COMM, VAL__BADR, ASIZE1, MAXBRK, NPOINT, WAVE, FLUX, 
     :             NBREAK, BREAK, WORKI, STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  Unmap the mapped arrays.
      CALL NDF_UNMAP( INDF, 'DATA', STATUS )
      CALL NDF_AUNMP( INDF, 'CENTRE', 1, STATUS )

*  If an error has occurred, set the current array size to zero.
      IF( STATUS .NE. SAI__OK ) NPOINT = 0

      END 
