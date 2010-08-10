      SUBROUTINE RDNDF( COMM, INDF, USYS, UUNIT, WORV, TITLE, STATUS )
*+
*  Name:
*     RDNDF

*  Purpose:
*     Read data from an NDF into the current arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDNDF( COMM, INDF, USYS, UUNIT, WORV, TITLE, STATUS )

*  Description:
*     If the supplied NDF has more than 1 significant pixel axis, an error
*     is reported. If the DATA array is too large to fit into the common
*     data arrays, a warning is given and as much as there is room for
*     is copied to the Y array. If an extension called DIPSO_EXTRA is
*     found, it is assumed that the NDF was created by DIPSO, and that
*     the extension contains the following components:
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
*     If a DIPSO_EXTRA extension is found, the dipso X array is filled
*     with values copied directly from the AXIS CENTRE values for the
*     significant pixel axis (whch are assumed to be wavelength or
*     optical velocity, as indicated by the value of WORV.
*
*     If the NDF does not contain a DIPSO_EXTRA extension, then a
*     warning is given, and default values are used; the BREAKS array
*     is assumed to contain a single value equal to the index of the
*     last pixel, and WORV is assumed to be 1.0. The X array is filled
*     with values obtained from the NDFs WCS component. If the WCS
*     FrameSet contains an AST SpecFrame then it is used to define the
*     X axis scale. Otherwise, the current Frame is searched for an axis
*     which has a label indicative of a spectral axis, and appropriate
*     assumptions are made about the nature of the axis. These
*     assumptions are displayed to the user.
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
*     USYS = CHARACTER * ( * ) (Given)
*        The System in which the X axis is supplied within the NDF. If
*        blank, an attempt is made to guess the system from the
*        attributes of the NDFs WCS FrameSet.
*     UUNIT = CHARACTER * ( * ) (Given)
*        The Unit in which the X axis is supplied within the NDF. If
*        blank, an attempt is made to guess the unit from the
*        attributes of the NDFs WCS FrameSet.
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
*     12-FEB-2003 (DSB):
*        - Allow access to multi-dimensional NDFs so long as they only have
*        one significant pixel axis.
*        - Obtain spectral calibrartion for X axis from NDF WCS component
*        if the NDF was not created by DIPSO.
*     13-DEC-2003 (DSB):
*        - Check for usable AXIS structures before using FITS WCS headers.
*        - Check for unit plurals ("Angstroms" instead of "Angstrom")
*     29-SEP-2004 (DSB):
*        Use CNF_PVAL
*     10-AUG-2010 (DSB):
*        Simplify ASMAP so that equal, inverted, non-monotonic, AXIS LutMaps
*        cancel out. Otherwise, such LutMaps prevent the Mapping being defined
*        in either direction.
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
      INCLUDE 'AST_PAR'          ! AST__ constants and functions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

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
      CHARACTER USYS*(*)
      CHARACTER UUNIT*(*)

*  Arguments Returned:
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL UNITOK             ! Is the Unit string OK?

*  Local Variables:
      CHARACTER SYS*40           ! System string
      CHARACTER TEXT*50          ! Label string
      CHARACTER UNIT*20          ! Unit string
      CHARACTER DEFUNI*20        ! Default unit string
      CHARACTER XLOC*(DAT__SZLOC)! Locator to the DIPSO_EXTRA extension
      INTEGER AGMAP              ! Pointer to AXIS->GRID Mapping
      INTEGER ASMAP              ! Pointer to AXIS->SPECTRAL Mapping
      INTEGER DEFFRM             ! Pointer to default SpecFrame
      INTEGER DEFMAP             ! Pointer to default Mapping
      INTEGER FS                 ! Pointer to results FrameSet
      INTEGER GSMAP              ! Pointer to GRID->SPECTRAL Mapping
      INTEGER I                  ! Loop count
      INTEGER IERR               ! Index of first conversion error
      INTEGER IPAXIS             ! Pointer to mapped AXIS CENTRE array
      INTEGER IPDATA             ! Pointer to mapped DATA array
      INTEGER IPW1               ! Pointer to work space
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER JUNK               ! Pointer to an unused FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Pixel index at lower bound of NDF
      INTEGER MAP                ! Pointer to replacement Mapping
      INTEGER NAX                ! No. of current Frame axes
      INTEGER NDIM               ! No. of dimensions in the NDF
      INTEGER NERR               ! No. of conversion errors
      INTEGER NEWFRM             ! Pointer to a replacement Frame
      INTEGER SDIM               ! Index of significant pixel axis
      INTEGER SFRM               ! Pointer to spectral Frame
      INTEGER TEMPLT             ! Pointer to a template Frame
      INTEGER UBND( NDF__MXDIM ) ! Pixel index at upper bound of NDF
      INTEGER ULEN               ! Used length of UNIT
      INTEGER WORKI( MAXBRK )    ! Work array
      LOGICAL DIPNDF             ! Is this a DIPSO NDF?
      LOGICAL GOTAX              ! Got an NDF AXIS structure?
      LOGICAL MORE               ! Continue looping?
      LOGICAL REPORT             ! Report system and units?
      LOGICAL THERE              ! Does the named component exist?
      REAL RF                    ! Rest frequency (GHz)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Find the significant axis (i.e. the axis spanned by more than a single
*  pixel). Report an error if there is more than one significant axis.
      SDIM = 0
      DO I = 1, NDIM
         IF( LBND( I ) .LT. UBND( I ) ) THEN
            IF( SDIM .EQ. 0 ) THEN
               SDIM = I
            ELSE
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'The input NDF has more '//
     :                          'than one significant dimension.',
     :                          STATUS )
               END IF
               GO TO 999
            END IF
         END IF
      END DO

      IF( SDIM .EQ. 0 ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'The input NDF has no '//
     :                    'significant dimensions.', STATUS )
         END IF
         GO TO 999
      END IF

*  Abort if an error has occurred. This ensure that the STATUS check
*  following the call to NDF_XLOC will only pick up errors reported by
*  NDF_XLOC.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  The first task is to obtain the information from the DIPSO_EXTRA
*  extension. Try and obtain a locator to the DIPSO_EXTRA extension.
      CALL NDF_XLOC( INDF, 'DIPSO_EXTRA', 'READ', XLOC, STATUS )

*  If the extension was found succesfully...
      IF( STATUS .EQ. SAI__OK ) THEN

*  Record that we have a DIPSO_EXTRA extension.
         DIPNDF = .TRUE.

*  See if a component called NSTNPT exists. If it does, then this NDF
*  contains stack data. Annul the extension locator, report an error
*  and abort if this is the case.
         CALL DAT_THERE( XLOC, 'NSTNPT', THERE, STATUS )
         IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL DAT_ANNUL( XLOC, STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'RDNDF_ERR2', 'The input NDF '//
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
            CALL MSGOUT( COMM, 'No WORV value found in the input NDF.'//
     :                   ' Assuming a value of 1.0.', .TRUE., STATUS )
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
            CALL MSGOUT( COMM, 'The input NDF was not created by '//
     :                   'DIPSO. Assuming no breaks.', .TRUE., STATUS )
         END IF

         NBREAK = 1
         BREAK( 1 ) = UBND( SDIM )
         WORV = 1.0
         DIPNDF = .FALSE.

      END IF

*  If the lower bound of the NDF is not 1 (for instance if the user
*  specified an NDF section instead of the whole of an NDF), shift the
*  break points to refer to a lower bound of 1.
      IF( LBND( SDIM ) .NE. 1 ) THEN

         DO I = 1, NBREAK
            BREAK( I ) = BREAK( I ) - LBND( SDIM ) + 1
         END DO

      END IF

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  If the NDF has a WCS FrameSet, get it.
      CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )
      IF( THERE ) THEN
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Otherwise, if the Axis structure is defined, use the default WCS
*  FrameSet in which the AXIS Frame is the current Frame.
      ELSE
         CALL NDF_ASTAT( INDF, 'CENTRE', SDIM, THERE, STATUS )
         IF( THERE ) THEN
            CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Otherwise, try to get a WCS FrameSet from the FITS headers in the FITS
*  extension.
         ELSE
            CALL KPG1_GTWCS( INDF, IWCS, STATUS )
         END IF
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
      CALL VEC_RTOR( .FALSE., NPOINT, %VAL( CNF_PVAL( IPDATA ) ), FLUX,
     :               IERR, NERR, STATUS )

*  Map the AXIS CENTRE array. This should hold the wavelength (or
*  velocity) at the centre of each data pixel. If there is no AXIS
*  CENTRE array in the NDF, the NDF system will return an array holding
*  the pixel co-ordinate at the centre of each pixel (i.e. ..., 0.5,
*  1.5, 2.5... etc ).
      CALL NDF_AMAP( INDF, 'CENTRE', SDIM, '_REAL', 'READ', IPAXIS,
     :               NPOINT, STATUS )

*  Restrict the number of elements transferred.
      IF( NPOINT .GT. ASIZE1 ) NPOINT = ASIZE1

*  Copy the values to the common X array.
      CALL VEC_RTOR( .FALSE., NPOINT, %VAL( CNF_PVAL( IPAXIS ) ), WAVE,
     :               IERR, NERR, STATUS )

*  If the NDF was created by DIPSO, the X array will now be correct. If
*  the NDF was not created by DIPSO, then the X array may not contain
*  velocity or wavelength. We now see if we can correct this. Assume the
*  X array is OK if the NDF contains a DIPSO_EXTRA extension and the
*  AXIS array was defined.
      CALL NDF_ASTAT( INDF, 'CENTRE', SDIM, GOTAX, STATUS )
      IF( .NOT. DIPNDF .OR. .NOT. GOTAX ) THEN

*  Modify it so that it has a 1-d Base (GRID) Frame corresponding to the
*  significant NDF pixel axis.
         CALL AST_INVERT( IWCS, STATUS )
         NEWFRM = AST_PICKAXES( IWCS, 1, SDIM, MAP, STATUS )
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP, NEWFRM, STATUS )
         CALL AST_INVERT( IWCS, STATUS )

*  Attempt to find a SpecFrame within the WCS FrameSet.
         TEMPLT = AST_SPECFRAME( ' ', STATUS )
         FS = AST_FINDFRAME( IWCS, TEMPLT, ' ', STATUS )

*  If no SpecFrame was found, search the current Frame for an axis which
*  looks like a spectral axis.
         IF( FS .EQ. AST__NULL ) THEN
            NAX = AST_GETI( IWCS, 'NAXES', STATUS )
            DEFFRM = AST__NULL

            I = 0
            MORE = .TRUE.
            DO WHILE( MORE .AND. I .LT. NAX )
               I = I + 1
               NEWFRM = AST_PICKAXES( IWCS, 1, I, MAP, STATUS )
               MORE = .FALSE.

*  Get the axis unit.
               UNIT = AST_GETC( NEWFRM, 'UNIT(1)', STATUS )

*  Get the upper case axis label.
               TEXT = AST_GETC( NEWFRM, 'LABEL(1)', STATUS )
               CALL CHR_UCASE( TEXT )
 10            CONTINUE

*  Create a candidate SpecFrame to describe the current axis.
               SFRM = AST_SPECFRAME( ' ', STATUS )

*  If the axis label contains the string "wave" set the SpecFrame system to
*  wavelength.
               IF( INDEX( TEXT, 'WAVE' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'WAVE', STATUS )

*  If it contains the string "freq" set the SpecFrame system to wavelength.
               ELSE IF( INDEX( TEXT, 'FREQ' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'FREQ', STATUS )

*  If it contains a "v" assume it is some form of velocity.
               ELSE IF( INDEX( TEXT, 'V' ) .NE. 0 ) THEN

*  If it also contains "RAD" assume it is radio velocity.
                  IF( INDEX( TEXT, 'RAD' ) .NE. 0 ) THEN
                     CALL AST_SETC( SFRM, 'SYSTEM', 'VRAD', STATUS )

*  If it also contains "OP" assume it is optical velocity.
                  ELSE IF( INDEX( TEXT, 'OP' ) .NE. 0 ) THEN
                     CALL AST_SETC( SFRM, 'SYSTEM', 'VOPT', STATUS )

*  Otherwise assume it is relativistic velocity.
                  ELSE
                     CALL AST_SETC( SFRM, 'SYSTEM', 'VELO', STATUS )
                  END IF

*  Check for the other FITS systems.
               ELSE IF( INDEX( TEXT, 'ENER' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'ENERGY', STATUS )

               ELSE IF( INDEX( TEXT, 'WAVN' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'WAVN', STATUS )

               ELSE IF( INDEX( TEXT, 'AWAV' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'AWAV', STATUS )

               ELSE IF( INDEX( TEXT, 'ZOPT' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'ZOPT', STATUS )

               ELSE IF( INDEX( TEXT, 'BETA' ) .NE. 0 ) THEN
                  CALL AST_SETC( SFRM, 'SYSTEM', 'BETA', STATUS )

*  If none of the above checks produced a system, see if the Unit value
*  suggests a system.
               ELSE

*  Check no error has occurred.
                  IF( STATUS .NE. SAI__OK ) GO TO 999

*  First try wavelength.
                  CALL AST_SETC( SFRM, 'SYSTEM', 'WAVE', STATUS )
                  CALL AST_SETC( SFRM, 'UNIT', UNIT, STATUS )

*  If the unit string is not OK, and try frequency.
                  IF( .NOT. UNITOK( SFRM, STATUS ) ) THEN
                     CALL AST_SETC( SFRM, 'SYSTEM', 'FREQ', STATUS )
                     CALL AST_SETC( SFRM, 'UNIT', UNIT, STATUS )

*  If the unit was not OK, try optical velocity.
                     IF( .NOT. UNITOK( SFRM, STATUS ) ) THEN
                        CALL AST_SETC( SFRM, 'SYSTEM', 'VOPT', STATUS )
                        CALL AST_SETC( SFRM, 'UNIT', UNIT, STATUS )

*  If the Unit was not OK, indicate that we need to try the next axis in the
*  current Frame.
                        IF( .NOT. UNITOK( SFRM, STATUS ) ) THEN
                            MORE = .TRUE.

*  If this is the axis which has the same index as the significant pixel
*  axis, keep the current SpecFrame as a default in case no better axis is
*  found. Set it to represent wavelength in Angstroms.
                            IF( I .EQ. SDIM ) THEN
                               CALL AST_SETC( SFRM, 'SYSTEM', 'WAVE',
     :                                        STATUS )
                               CALL AST_SETC( SFRM, 'UNIT', 'Angstrom',
     :                                       STATUS )
                               DEFFRM = AST_COPY( SFRM, STATUS )
                               DEFMAP = AST_COPY( MAP, STATUS )
                               DEFUNI = UNIT
                            END IF

*  Annul the current SpecFrame.
                            CALL AST_ANNUL( SFRM, STATUS )

                        END IF
                     END IF
                  END IF
               END IF

*  If we have not found a good SpecFrame System, and the unit ends with the
*  letter "S", try using the Unit again without the "S" (in case the S is
*  just a plural - e.g. "Angstroms" instead of "Angstrom" ).
               IF( MORE ) THEN
                  ULEN = CHR_LEN( UNIT )
                  IF( UNIT( ULEN : ULEN ) .EQ. 'S' .OR.
     :                UNIT( ULEN : ULEN ) .EQ. 's' ) THEN
                     UNIT( ULEN : ULEN ) = ' '
                     GO TO 10
                  END IF
               END IF

            END DO

*  Use the default SpecFrame if no more appropriate one was found.
            IF( SFRM .EQ. AST__NULL ) THEN
               SFRM = DEFFRM
               MAP = DEFMAP
               UNIT = DEFUNI
            END IF

*  If the user specified a system and/or unit, use them.
            IF( USYS .NE. ' ' ) CALL AST_SETC( SFRM, 'SYSTEM', USYS,
     :                                         STATUS )
            IF( UUNIT .NE. ' ' ) UNIT = UUNIT

            REPORT = ( USYS .EQ. ' ' .OR. UUNIT .EQ. ' ' )

*  Attempt to set the axis unit. If an error occurs, annul it and clear
*  the Unit attribute.
            IF( STATUS .EQ. SAI__OK .AND. UNIT .NE. ' ' ) THEN
               CALL AST_SETC( SFRM, 'UNIT', UNIT, STATUS )
               IF( .NOT. UNITOK( SFRM, STATUS ) ) THEN
                  CALL AST_CLEAR( SFRM, 'UNIT', STATUS )
                  REPORT = .TRUE.
               END IF
            END IF

*  Tell the user about the above assumptions.
            IF( REPORT ) THEN
               SYS = AST_GETC( SFRM, 'SYSTEM', STATUS )
               IF( SYS .EQ. 'FREQ' ) THEN
                  SYS = 'frequency'
               ELSE IF( SYS .EQ. 'ENER' ) THEN
                  SYS = 'energy'
               ELSE IF( SYS .EQ. 'WAVN' ) THEN
                  SYS = 'wave-number'
               ELSE IF( SYS .EQ. 'WAVE' ) THEN
                  SYS = 'wavelength'
               ELSE IF( SYS .EQ. 'AWAV' ) THEN
                  SYS = 'wavelength in air'
               ELSE IF( SYS .EQ. 'VRAD' ) THEN
                  SYS = 'radio velocity'
               ELSE IF( SYS .EQ. 'VOPT' ) THEN
                  SYS = 'optical velocity'
               ELSE IF( SYS .EQ. 'ZOPT' ) THEN
                  SYS = 'redshift'
               ELSE IF( SYS .EQ. 'BETA' ) THEN
                  SYS = 'beta factor'
               ELSE IF( SYS .EQ. 'VELO' ) THEN
                  SYS = 'relativistic velocity'
               END IF
               CALL MSG_SETC( 'SYS', SYS )

               UNIT = AST_GETC( SFRM, 'UNIT', STATUS )
               IF( UNIT .NE. ' ' ) THEN
                  CALL MSG_SETC( 'UN', AST_GETC( SFRM, 'UNIT', STATUS) )
                  CALL MSGOUT( COMM, 'Unsure about the spectral '//
     :                     'system in the input NDF: assuming it is '//
     :                     '^SYS in units of ''^UN''.', .TRUE., STATUS )
               ELSE
                  CALL MSGOUT( COMM, 'Unsure about the spectral '//
     :                      'system in the input NDF: assuming it is '//
     :                      '^SYS (in dimensionless units).', .TRUE.,
     :                      STATUS )
               END IF
            END IF

*  Add this SpecFrame into the FrameSet using a UnitMap to connect it to
*  the current Frame.
            CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP, SFRM, STATUS )
            FS = IWCS

         END IF

*  We now know that the current Frame in the "FS" FrameSet is a SpecFrame.
*  Make sure it represents wavelength in units of Angstroms, or optical
*  velocity in units of km/s. Store the corresponding WORV value. All the
*  Mappings within the FrameSet will be modified appropriately to maintain
*  the correct relationships between Frames.
         UNIT = AST_GETC( FS, 'UNIT', STATUS )
         SYS = AST_GETC( FS, 'SYSTEM', STATUS )
         IF( SYS .EQ. 'VRAD' .OR. SYS .EQ. 'VOPT' .OR.
     :       SYS .EQ. 'ZOPT' .OR. SYS .EQ. 'BETA' .OR.
     :       SYS .EQ. 'VELO' ) THEN

            IF( SYS .NE. 'VOPT' .OR. UNIT .NE. 'km/s' ) THEN
               CALL MSGOUT( COMM, 'Converting spectral system to '//
     :                      'optical velocity in units of ''km/s''.',
     :                      .FALSE., STATUS )
            END IF

            CALL AST_SETC( FS, 'SYSTEM', 'VOPT', STATUS )
            CALL AST_SETC( FS, 'UNIT', 'km/s', STATUS )
            RF = AST_GETR( FS, 'RESTFREQ', STATUS )
            IF( RF .NE. AST__BAD .AND. RF .NE. 0.0 ) THEN
               WORV = 1.0E4/RF
            ELSE
               WORV = 1.0
               CALL MSGOUT( COMM, 'Undefined rest wavelength in NDF '//
     :                      '''^NDF''. Assuming a WORV value of 1.0.',
     :                      .TRUE., STATUS )
            END IF

         ELSE

            IF( SYS .NE. 'WAVE' .OR. UNIT .NE. 'Angstrom' ) THEN
               CALL MSGOUT( COMM, 'Converting spectral system to '//
     :                      'wavelength in units of ''Angstrom''.',
     :                      .FALSE., STATUS )
            END IF

            CALL AST_SETC( FS, 'SYSTEM', 'WAVE', STATUS )
            CALL AST_SETC( FS, 'UNIT', 'Angstrom', STATUS )
            WORV = 1.0
         END IF

*  The Base Frame of the "FS" FrameSet will be the 1-D GRID Frame and the
*  Current Frame will be the required 1-dimensional SpecFrame. Extract the
*  1-D Base->Current Mapping.
         GSMAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )

*  Now get a FrameSet connecting the 1-d GRID Frame to the NDF AXIS Frame.
         TEMPLT = AST_FRAME( NDIM, 'Domain=AXIS', STATUS )
         FS = AST_FINDFRAME( IWCS, TEMPLT, ' ', STATUS )

*  Modify the AXIS Frame to be 1-d.
         NEWFRM = AST_PICKAXES( FS, 1, SDIM, MAP, STATUS )
         CALL AST_ADDFRAME( FS, AST__CURRENT, MAP, NEWFRM, STATUS )

*  Extract the AXIS->GRID Mapping.
         AGMAP = AST_GETMAPPING( FS, AST__CURRENT, AST__BASE, STATUS )

*  Concatenate this with the GRID->SPECTRUM Mapping to get the
*  AXIS->SPECTRUM Mapping.
         ASMAP = AST_SIMPLIFY( AST_CMPMAP( AGMAP, GSMAP, .TRUE., ' ',
     :                                     STATUS ), STATUS )

*  Unmap the NDF AXIS array (since we need double precision access to it
*  and we currently only have single precision).
         CALL NDF_AUNMP( INDF, 'CENTRE', SDIM, STATUS )

*  Now map it in double precision.
         CALL NDF_AMAP( INDF, 'CENTRE', SDIM, '_DOUBLE', 'READ', IPAXIS,
     :                  NPOINT, STATUS )

*  Allocate some memory to store the corresponding double precision
*  spectral values.
         CALL PSX_CALLOC( NPOINT, '_DOUBLE', IPW1, STATUS )

*  Check the pointer can be used.
         IF( STATUS .EQ. SAI__OK ) THEN

*  Use the Mapping to transform the AXIS Centre values into spectral values,
*  putting the results in the work space just allocated.
            CALL AST_TRAN1( ASMAP, NPOINT, %VAL( CNF_PVAL( IPAXIS ) ),
     :                      .TRUE., %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Copy these to the X array.
           CALL VEC_DTOR( .TRUE., NPOINT, %VAL( CNF_PVAL( IPW1 ) ),
     :                    WAVE, IERR, NERR, STATUS )

         END IF

*  Free work space.
         CALL PSX_FREE( IPW1, STATUS )

      END IF

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
      CALL NDF_AUNMP( INDF, 'CENTRE', SDIM, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occurred, set the current array size to zero.
      IF( STATUS .NE. SAI__OK ) NPOINT = 0

      END





      LOGICAL FUNCTION UNITOK( SF, STATUS )
*+
*  Name:
*     UNITOK

*  Purpose:
*     See if the Unit and System attributes of hte supplied SpecFrame are
*     consistent.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = UNITOK( SF, STATUS )

*  Description:
*     Returns .TRUE. if the Unit attribute of the supplied SpecFrame is a
*     valid unit for the spectral system specified by the System attribute
*     of the supplied SpecFrame. Returns .FALSE. otherwise.

*  Arguments:
*     SF = INTEGER (Given)
*        An identifier for a SpecFrame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     .TRUE if the Unit is OK.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-2003 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants and functions
      INCLUDE 'AST_ERR'          ! AST__ error constants

*  Arguments Given:
      INTEGER SF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER OPTS*50
      INTEGER IAT
      INTEGER SF2

*  Initialise
      UNITOK = .FALSE.

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Attempt to create a new SpecFrame with the same System as the given
*  SpecFrame, and the supplied unit.
      OPTS = 'SYSTEM='
      IAT = 7
      CALL CHR_APPND( AST_GETC( SF, 'System', STATUS ), OPTS, IAT )
      CALL CHR_APPND( ',Unit(1)=', OPTS, IAT )
      CALL CHR_APPND( AST_GETC( SF, 'Unit(1)', STATUS ), OPTS, IAT )

      SF2 = AST_SPECFRAME( OPTS( : IAT ), STATUS )
      IF( STATUS .EQ. AST__BADUN ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         UNITOK = .TRUE.
         CALL AST_ANNUL( SF2, STATUS )

      END IF

      END
