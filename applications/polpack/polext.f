      SUBROUTINE POLEXT( STATUS )
*+
*  Name:
*     POLEXT

*  Purpose:
*     Sets explicit values in the POLPACK extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLEXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application can be used to store information within the POLPACK
*     extensions of a group of data files so that they may subsequently be
*     processed with POLPACK. The values to store in the POLPACK extension
*     are supplied directly by the user in response to application
*     parameter prompts. If the required information is present within
*     each data file in the form of header cards in a FITS extension, then
*     application POLIMP may be used in place of POLEXT (POLIMP reads the
*     required values from the FITS extension instead of obtaining values
*     from the user).
*
*     New values for the POLPACK extension items are obtained using the
*     parameters described below. If supplied, these new values are stored
*     in the POLPACK extension items of the supplied data files. New
*     POLPACK extensions are created if necessary. If no new values are
*     supplied for an item, the existing item values (if any) are retained.
*     The final contents of the POLPACK extension are then listed. The
*     values (on exit) of the extension items in the last specified data
*     file are written to a set of output parameters.

*  Usage:
*     polext in

*  ADAM Parameters:
*     ANGROT = _REAL (Read)
*        The anti-clockwise angle from the first (X) pixel axis of each
*        image to the polarimeter reference direction, in degrees. The
*        supplied value is not stored explicitly within the POLPACK
*        extension. Instead, it is used to create a new Frame within the WCS
*        component of the NDF. This Frame is given the Domain name POLANAL
*        and its first axis corresponds to the reference direction. If a
*        null (!) value is supplied for this parameter, any image which
*        already has a POLANAL Frame retains the existing Frame. Otherwise
*        a POLANAL Frame is created using an ANGROT value of zero (i.e. it
*        is assumed that the reference direction corresponds to the X
*        pixel axis).
*
*        The reference direction depends on the type of polarimeter; in a
*        rotating half-wave plate polarimeter, it should correspond to the
*        direction of the fixed analyser; in polarimeters with multiple
*        fixed analysers or a single rotating analyser, it should
*        correspond to the direction specified by a zero value of ANLANG. [!]
*     ANLANG = _REAL (Read)
*        Specifies the anti-clockwise angle in degrees from the reference
*        direction (established using the ANGROT parameter) to the
*        analyser. This parameter should only be used with polarimeters
*        which have either a rotating analyser or a set of fixed
*        analysers. If your polarimetry has a rotating half-wave plate
*        instead, then you should use WPLATE instead. The given value is
*        stored in all supplied data files. If a null (!) value is
*        supplied, any image which already has an ANLANG value retains its
*        existing value, and any other images are left with an undefined
*        ANLANG value. [!]
*     EPS = _REAL (Read)
*        The analyser efficiency. This gives the efficiency with which the
*        analyser rejects light polarised across its axis. A perfect
*        polariser has a value of 1.0. A perfect piece of glass would have
*        a value of 0.0. The stored value is used only when processing
*        single-beam data. The given value is stored in all supplied data
*        files. If a null (!) value is supplied, any image which already
*        has an EPS value retains its existing value, and any other images
*        are left with an undefined EPS value (this will cause a value of
*        1.0 to be used by POLCAL). [!]
*     FILTER = LITERAL (Read)
*        The filter name. The value of extension item WPLATE or ANLANG
*        (whichever is available) is appended to the supplied filter value
*        before being stored, unless the filter value already contains the
*        WPLATE or ANLANG value. If a null (!) value is supplied, then
*        any existing FILTER value in the POLPACK extension is retained. If
*        there is no value in the POLPACK extension, then any value in the
*        CCDPACK extension is used instead. If there is no value in the
*        CCDPACK extension, then a default value equal to the value of WPLATE
*        or ANLANG is used. [!]
*     IMGID = LITERAL (Read)
*        A group of image identifier strings. These are arbitrary strings
*        used to identify each original intensity frame. They are used
*        when processing dual-beam data to associate O and E ray images.
*        They are ignored when processing single-beam data. The supplied
*        group may take the form of a comma separated list of identifiers,
*        or any of the other forms described in the help on "Group
*        Expressions". A separate, non-blank identifier should be
*        supplied for each data file specified by parameter IN, in the
*        same order as the data files. If a null (!) value is supplied,
*        then any existing IMGID values in the POLPACK extensions are
*        retained. Default values equal to the name of the data file are
*        used if there is no existing value. [!]
*     IN = LITERAL (Read)
*        A group of data files. This may take the form of a comma separated
*        list of file names, or any of the other forms described in the help
*        on "Group Expressions".
*     NAMELIST = LITERAL (Read)
*        The name of a file to create containing a list of the successfully
*        processed data files. This file can be used when specifying the input
*        data files for subsequent applications. No file is created if a null
*        (!) value is given. [!]
*     RAY = LITERAL (Read)
*        You should use this parameter only if the images contain either O
*        or E ray images obtained by a dual-beam polarimeter. You should
*        not use this parameter if your data is from a single-beam
*        polarimeter, or if the O and E ray images have not yet been
*        extracted into separate images. If used, the supplied value must
*        be either "O" or "E". The same value is stored in all supplied
*        data files. If a null (!) value is supplied, any existing RAY
*        values are left unchanged, but no new ones are added. [!]
*     STOKES = LITERAL (Read)
*        You should use this parameter only if the supplied data files
*        are 3D cubes containing Stokes vectors. It should be a string in
*        which each character indicates the quantity stored in the
*        corresponding plane of the cube (I, Q, U or V). The length of
*        the string should equal the number of planes in the cube. If a
*        null (!) value is supplied, any existing STOKES values are left
*        unchanged, but no new ones are added. [!]
*     T = _REAL (Read)
*        The analyser transmission. This gives the transparency of the
*        analyser to unpolarised light. A perfect polariser has a value of
*        1.0. A perfect piece of glass would have a value of 2.0. The
*        stored value is only used when processing single-beam data. The
*        given value is stored in all supplied data files. If a null (!)
*        value is supplied, any image which already has a T value retains
*        its existing value, and any other images are left with an
*        undefined T value (this will cause a value of 1.0 to be used by
*        POLCAL). [!]
*     VANGROT = _REAL (Write)
*        The ANGROT value stored in the last data file on exit will be
*        written to this output parameter.
*     VANLANG = _REAL (Write)
*        The ANLANG value stored in the last data file on exit will be
*        written to this output parameter.
*     VEPS = _REAL (Write)
*        The EPS value stored in the last data file on exit will be
*        written to this output parameter.
*     VFILTER = LITERAL (Write)
*        The FILTER value stored in the last data file on exit will be
*        written to this output parameter.
*     VIMGID = LITERAL (Write)
*        The IMGID value stored in the last data file on exit will be
*        written to this output parameter.
*     VRAY = LITERAL (Write)
*        The RAY value stored in the last data file on exit will be
*        written to this output parameter.
*     VSTOKES = LITERAL (Write)
*        The STOKES value stored in the last data file on exit will be
*        written to this output parameter.
*     VT = _REAL (Write)
*        The T value stored in the last data file on exit will be
*        written to this output parameter.
*     VWPLATE = _REAL (Write)
*        The WPLATE value stored in the last data file on exit will be
*        written to this output parameter.
*     VVERSION = LITERAL (Write)
*        The POLPACK version number which created the last data file will be
*        written to this output parameter.
*     WPLATE = _REAL (Read)
*        The half-wave plate position, in degrees. Use parameter ANLANG if
*        your polarimeter does not have a half-wave plate. The given value
*        is stored in all supplied data files. If a null (!) value is
*        supplied, any image which already has an WPLATE value retains its
*        existing value, and any other images are left with an undefined
*        WPLATE value. Note, when using dual-beam data, Stokes vectors can
*        only be calculated for WPLATE values of 0.0, 22.5, 45.0 and 67.5.
*        The POLCAL application will fail if any other values are
*        supplied. There are no such restrictions on the value of WPLATE
*        when using single-beam data. [!]

*  Examples:
*     polext in=cube
*        Displays the contents of the POLPACK extension of data file
*        "cube", leaving the values unchanged.
*     polext in=^files_0.txt wplate=0 filter=V angrot=45
*        This example processes all the data files listed in the text file
*        "files_0.txt", setting WPLATE to zero and ANGROT to 45. FILTER
*        is set to "V_0.0", and IMGID values are set to the name of the data
*        file.

*  Notes:
*     -  Errors are reported if the final POLPACK extension in a data file
*     is illegal in any way.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1997-1999 Central Laboratory of the Research Councils
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-JUL-1997 (DSB):
*        Original version.
*     18-NOV-1998 (DSB):
*        Added RAY parameter.
*     12-FEB-1999 (DSB):
*        Added T, EPS and ANLANG. Removed restrictions on values of WPLATE.
*     30-MAR-1999 (DSB):
*        Fixed bug which prevented IMGID being changed.
*     12-MAY-1999 (DSB):
*        Added parameter STOKES, and output parameters.
*     31-JUL-2009 (TIMJ):
*        QUIET handling is done via MSG_IFGET now.
*     27-SEP-2012 (DSB):
*        Cater for cases where there is a POLANAL Frame but no POLPACK extension.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      CHARACTER CHR_NTH*2        ! Returns 'st', 'nd', 'rd', 'th', etc.

*  Local Constants:
      INTEGER IDLEN              ! Max. length of an image identifier string
      PARAMETER ( IDLEN = 30 )

*  Local Variables:
      CHARACTER BUF*80                 ! Buffer for screen output
      CHARACTER CLOC*( DAT__SZLOC )    ! Locator to structure component
      CHARACTER CNAME*( DAT__SZNAM )   ! Component HDS name
      CHARACTER CVAL*80                ! Component value
      CHARACTER FILT*( GRP__SZNAM )    ! The FILTER value to store
      CHARACTER FILTER*( GRP__SZNAM )  ! The supplied FILTER value
      CHARACTER IMG*( IDLEN )          ! The IMGID value to store
      CHARACTER IMGJ*( IDLEN )         ! A comparison IMGID value
      CHARACTER NDFNAM*( GRP__SZNAM )  ! Name of current NDF
      CHARACTER POLLOC*( DAT__SZLOC )  ! Locator to POLPACK extension
      CHARACTER RAY*1                  ! Supplied ray indicator; "O" or "E"
      CHARACTER RY*1                   ! Ray indicator to store; "O" or "E"
      CHARACTER STO*5                  ! Current value of STOKES component
      CHARACTER STOKES*5               ! Supplied value of STOKES component
      CHARACTER VER*20                 ! VERSION value
      INTEGER ADDED              ! No. of elements added to a group
      INTEGER I                  ! Loop counter
      INTEGER IAT                ! No. of characters in buffer
      INTEGER IGRP1              ! Input NDF group identifier
      INTEGER IGRP2              ! Id for group of names of NDF's processed OK
      INTEGER IGRP3              ! Id for group of supplied image identifiers
      INTEGER IGRP4              ! Id for group of used IMGID values
      INTEGER INDF               ! Current NDF identifier
      INTEGER INDX               ! Loop variable
      INTEGER IWCS               ! WCS FrameSet pointer
      INTEGER J                  ! Loop count
      INTEGER LNDF               ! Length of NDF name
      INTEGER NCOMP              ! No. of component in POLPACK extension
      INTEGER NGOOD              ! No. of NDFs processed successfully
      INTEGER NNDF               ! No. of input NDFs
      INTEGER SIZE               ! No. of elements in a group
      LOGICAL CFLAG              ! Should more values be obtained?
      LOGICAL CPRIM              ! Is component a primative?
      LOGICAL GOTANA             ! Was an ANLANG value given?
      LOGICAL GOTANG             ! Was a ANGROT value given?
      LOGICAL GOTCCD             ! Does NDF contain a CCDPACK extension?
      LOGICAL GOTEPS             ! Was an EPS value given?
      LOGICAL GOTFIL             ! Was a FILTER value given?
      LOGICAL GOTIMG             ! Were any IMGID values given?
      LOGICAL GOTPOL             ! Does NDF contain a POLPACK extension?
      LOGICAL GOTRAY             ! Was a RAY value given?
      LOGICAL GOTSTO             ! Was a STOKES value given?
      LOGICAL GOTT               ! Was a T value given?
      LOGICAL GOTWPL             ! Was a WPLATE value given?
      LOGICAL RDONLY             ! Read-only access required?
      REAL ANA                   ! The ANLANG value to store
      REAL ANG                   ! The ANGROT value to store
      REAL ANGROT                ! The supplied ANGROT value
      REAL ANLANG                ! The supplied ANLANG value
      REAL EPS                   ! The supplied EPS value
      REAL EPS0                  ! The EPS value to store
      REAL T                     ! The supplied T value
      REAL T0                    ! The T value to store
      REAL WPL                   ! The WPLATE value to store
      REAL WPLATE                ! The supplied WPLATE value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Access a group of NDFs for processing.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more image names...', IGRP1,
     :            NNDF, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( NNDF .GT. 1 ) THEN
         CALL MSG_SETI( 'N', NNDF )
         CALL MSG_OUT( 'POLEXT_MSG1', '  ^N input images to '//
     :        'process... ', STATUS )
      ELSE IF( NNDF .EQ. 1 ) THEN
         CALL MSG_OUT( 'POLEXT_MSG2', '  1 input image to '//
     :        'process... ',STATUS )
      ELSE
         CALL MSG_OUT( 'POLEXT_MSG3', '  NO input images to '//
     :        'process. ',STATUS )
         GO TO 999
      END IF

      CALL MSG_BLANK( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set a flag indicating that nothing is to be changed in the POLPACK
*  extensions. Clear this flag if any parameter supplies a new value
*  for an extension item.
      RDONLY = .TRUE.

*  Get the ANGROT value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the ANGROT value.
      CALL PAR_GET0R( 'ANGROT', ANGROT, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTANG = .FALSE.
      ELSE
         GOTANG = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the FILTER value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the FILTER value.
      CALL PAR_GET0C( 'FILTER', FILTER, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTFIL = .FALSE.
      ELSE
         GOTFIL = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get a group of image identifiers. First create a new GRP group to hold
*  them.
      CALL GRP_NEW( 'IMGID', IGRP3, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the group of strings from the environment.
         CALL GRP_GROUP( 'IMGID', GRP__NOID, IGRP3, SIZE, ADDED,
     :                   CFLAG, STATUS )

*  Cancel the parameter association in order to get more strings
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( 'IMGID', STATUS )
      END DO

*  Annul the error if a null (!) value was supplied.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTIMG = .FALSE.

*  Report an error if the number of supplied values was not equal to the number
*  of supplied NDFs.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         IF( SIZE .NE. NNDF ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ACTVAL', SIZE )
            CALL MSG_SETI( 'NVAL', NNDF )
            CALL ERR_REP( 'POLEXT_1', 'Only ^ACTVAL values '//
     :                    'supplied for parameter %IMGID. ^NVAL are '//
     :                    'required.', STATUS )

*  Check that all the IMGID values are non-blank.
         ELSE
            DO I = 1, NNDF
               CALL GRP_GET( IGRP3, I, 1, IMG, STATUS )

               IF( IMG .EQ. ' ' ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ITH', I )
                  CALL ERR_REP( 'POLEXT_2', 'The ^ITH value supplied '//
     :                          'for parameter %IMGID is blank.',
     :                          STATUS )
                  GO TO 999
               END IF

            END DO

            GOTIMG = .TRUE.
            RDONLY = .FALSE.

         END IF
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the WPLATE value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the WPLATE value.
      CALL PAR_GET0R( 'WPLATE', WPLATE, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTWPL = .FALSE.
      ELSE
         GOTWPL = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Get the ANLANG value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the ANLANG value.
      CALL PAR_GET0R( 'ANLANG', ANLANG, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTANA = .FALSE.
      ELSE
         GOTANA = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Report an error if both WPLATE and ANLANG were supplied.
      IF( GOTANA .AND. GOTWPL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLEXT_3', 'Values were supplied for both '//
     :                 '%WPLATE and %ANLANG. Only one of these should'//
     :                 ' be supplied, depending on the type of '//
     :                 'polarimeter.', STATUS )
         GO TO 999
      END IF

*  Get the T value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the T value.
      CALL PAR_GET0R( 'T', T, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTT = .FALSE.
      ELSE
         GOTT = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Get the EPS value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the EPS value.
      CALL PAR_GET0R( 'EPS', EPS, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTEPS = .FALSE.
      ELSE
         GOTEPS = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Get the RAY value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the RAY value.
      CALL PAR_CHOIC( 'RAY', ' ', 'O,E', .FALSE., RAY, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTRAY = .FALSE.
      ELSE
         GOTRAY = .TRUE.
         RDONLY = .FALSE.
      END IF

*  Get the STOKES value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the STOKES value.
      CALL PAR_GET0C( 'STOKES', STOKES, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTSTO = .FALSE.
      ELSE
         GOTSTO = .TRUE.
         CALL CHR_RMBLK( STOKES )
         CALL CHR_UCASE( STOKES )
         RDONLY = .FALSE.
      END IF

*  Create a group to hold the names of the NDFs which were processed
*  successfully.
      CALL GRP_NEW( 'Good NDFs', IGRP2, STATUS )

*  Check that everything is ok so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the identifier for the group holding used IMGID values.
      IGRP4 = GRP__NOID

*  Process each NDF in turn.
      CALL MSG_BLANK( STATUS )
      DO INDX = 1, NNDF

*  Get the name of the NDF now, while we know that no error has occurred.
         CALL GRP_GET( IGRP1, INDX, 1, NDFNAM, STATUS )
         LNDF = CHR_LEN( NDFNAM )

*  Write out name of this NDF.
         CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
         CALL MSG_OUT( 'POLEXT_MSG4', '  Processing '//
     :        '''^CURRENT_NDF''', STATUS )

*  Get the input NDF identifier
         IF( RDONLY ) THEN
            CALL NDG_NDFAS( IGRP1, INDX, 'READ', INDF, STATUS )
         ELSE
            CALL NDG_NDFAS( IGRP1, INDX, 'UPDATE', INDF, STATUS )
         END IF

*  Get the NDFs WCS FrameSet.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  See if there is already a CCDPACK extension.
         CALL NDF_XSTAT( INDF, 'CCDPACK', GOTCCD, STATUS )

*  If so, get the FILTER value from the CCDPACK extension (if any).
         FILT = ' '
         IF( GOTCCD ) THEN
            CALL NDF_XGT0C( INDF, 'CCDPACK', 'FILTER', FILT, STATUS )
         END IF

*  See if there is already a POLPACK extension.
         CALL NDF_XSTAT( INDF, 'POLPACK', GOTPOL, STATUS )

*  If so, get the values of various components, establishing suitable
*  defaults first.
         STO = ' '
         ANG = 0.0
         IMG = NDFNAM( MAX( 1, LNDF - IDLEN + 1 ) : LNDF )
         WPL = VAL__BADR
         RY = ' '
         T0 = VAL__BADR
         EPS0 = VAL__BADR
         ANA = VAL__BADR
         VER = ' '

         IF( GOTPOL ) THEN
            CALL NDF_XGT0C( INDF, 'POLPACK', 'STOKES', STO, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'FILTER', FILT, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'IMGID', IMG, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'WPLATE', WPL, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'ANLANG', ANA, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'T', T0, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'EPS', EPS0, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'RAY', RY, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'VERSION', VER, STATUS )

*  The existing ANGROT value is obtained from the POLANAL WCS Frame.
            CALL POL1_GTANG( INDF, 0, IWCS, ANG, STATUS )

*  If there is no POLPACK extension, attempt to read the ANGROT value
*  from a polanal Frame. If it cannot be obtained, annul the error.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL POL1_GTANG( INDF, 0, IWCS, ANG, STATUS )
            IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  If there is no POLPACK extension, and no POLANAL Frame, report an error
*  if the extension is not being changed.
            IF( RDONLY ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'POLEXT_4', 'No POLPACK '//
     :                          'extension found.', STATUS )
               END IF

*  If any new values have been supplied to be stored in the extension,
*  create an extension.
            ELSE
               CALL NDF_XNEW( INDF, 'POLPACK', 'POLPACK', 0, 0,
     :                        POLLOC, STATUS )
               CALL DAT_ANNUL( POLLOC, STATUS )
            END IF
         END IF

*  Replace these defaults with any supplied values.
         IF( GOTANG ) ANG = ANGROT
         IF( GOTFIL ) FILT = FILTER
         IF( GOTIMG ) CALL GRP_GET( IGRP3, INDX, 1, IMG, STATUS )
         IF( GOTWPL ) WPL = WPLATE
         IF( GOTRAY ) RY = RAY
         IF( GOTT ) T0 = T
         IF( GOTEPS ) EPS0 = EPS
         IF( GOTANA ) ANA = ANLANG
         IF( GOTSTO ) STO = STOKES

*  If any changes are to made, store the values in the NDF. First do
*  Stokes vector cubes.
         IF( .NOT. RDONLY ) THEN
            IF( STO .NE. ' ' ) THEN
               CALL POL1_PTANG( ANG, IWCS, STATUS )

               IF( FILT .NE. ' ' ) THEN
                  CALL NDF_XPT0C( FILT, INDF, 'POLPACK', 'FILTER',
     :                            STATUS )
               END IF

               CALL NDF_XPT0C( STO( : CHR_LEN( STO ) ), INDF, 'POLPACK',
     :                         'STOKES', STATUS )

*  Now do intensity images.
            ELSE

               CALL POL1_PTANG( ANG, IWCS, STATUS )

               IF( FILT .NE. ' ' ) THEN
                  CALL NDF_XPT0C( FILT, INDF, 'POLPACK', 'FILTER',
     :                            STATUS )
               END IF

               IF( IMG .NE. ' ' ) THEN
                  CALL NDF_XPT0C( IMG, INDF, 'POLPACK', 'IMGID',
     :                            STATUS )
               END IF

               IF( WPL .NE. VAL__BADR ) THEN
                  CALL NDF_XPT0R( WPL, INDF, 'POLPACK', 'WPLATE',
     :                            STATUS )
               END IF

               IF( RY .NE. ' ' ) THEN
                  CALL NDF_XPT0C( RY, INDF, 'POLPACK', 'RAY', STATUS )
               END IF

               IF( ANA .NE. VAL__BADR ) THEN
                  CALL NDF_XPT0R( ANA, INDF, 'POLPACK', 'ANLANG',
     :                            STATUS )
               END IF

               IF( T0 .NE. VAL__BADR ) THEN
                  CALL NDF_XPT0R( T0, INDF, 'POLPACK', 'T', STATUS )
               END IF

               IF( EPS0 .NE. VAL__BADR ) THEN
                  CALL NDF_XPT0R( EPS0, INDF, 'POLPACK', 'EPS', STATUS )
               END IF

            END IF

         END IF

*  Now store the parameter values.
         CALL PAR_PUT0R( 'VANGROT', ANG, STATUS )
         CALL PAR_PUT0R( 'VWPLATE', WPL, STATUS )
         CALL PAR_PUT0C( 'VRAY', RY, STATUS )
         CALL PAR_PUT0R( 'VANLANG', ANA, STATUS )
         CALL PAR_PUT0R( 'VT', T0, STATUS )
         CALL PAR_PUT0R( 'VEPS', EPS0, STATUS )
         CALL PAR_PUT0C( 'VFILTER', FILT( : MAX( 1, CHR_LEN( FILT ) ) ),
     :                   STATUS )
         CALL PAR_PUT0C( 'VSTOKES', STO( : MAX( 1, CHR_LEN( STO ) ) ),
     :                   STATUS )
         CALL PAR_PUT0C( 'VIMGID', IMG( : MAX( 1, CHR_LEN( IMG ) ) ),
     :                   STATUS )
         CALL PAR_PUT0C( 'VVERSION', VER( : MAX( 1, CHR_LEN( VER ) ) ),
     :                   STATUS )

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', POLLOC, STATUS )

*  Check the values in the POLPACK extension are usable.
         IF( .NOT. RDONLY) CALL POL1_CHKEX( INDF, POLLOC, IGRP4,
     :                                      .TRUE., STATUS )

*  If required, display the contents of the POLPACK extension.
         IF( MSG_FLEVOK( MSG__NORM, STATUS ) ) THEN
            CALL ERR_BEGIN( STATUS )

*  Obtain and display the ANGROT value.
            ANGROT = VAL__BADR
            CALL POL1_GTANG( INDF, 0, IWCS, ANGROT, STATUS )

            IF( ANGROT .NE. VAL__BADR ) THEN
               BUF = '   ANGROT'
               BUF( DAT__SZNAM + 4 : DAT__SZNAM + 4 ) = ':'
               IAT = DAT__SZNAM + 5
               CALL CHR_PUTR( ANGROT, BUF, IAT )
               CALL MSG_OUT( 'POLEXT_MSG5', BUF, STATUS )
            END IF

*  Get the number of components in it.
            CALL DAT_NCOMP( POLLOC, NCOMP, STATUS)

*  Tell the user if the extension is empty.
            IF( NCOMP .EQ. 0 ) THEN
               IF( ANGROT .EQ. VAL__BADR ) THEN
                  CALL MSG_OUT( 'POLEXT_MSG6', '   The POLPACK '//
     :                          'extension is empty.', STATUS )
               END IF

*  Otherwise, index through the structure's components, obtaining locators
*  and the required information.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               DO I = 1, NCOMP

*  Get a locator to the I'th component.
                  CALL DAT_INDEX( POLLOC, I, CLOC, STATUS )

*  Obtain its name.
                  CALL DAT_NAME( CLOC, CNAME, STATUS )

*  Initialise the output buffer to contain the component name.
                  BUF = ' '
                  BUF = '   '//CNAME
                  BUF( DAT__SZNAM + 4 : DAT__SZNAM + 4 ) = ':'
                  IAT = DAT__SZNAM + 5

*  Is it primitive?
                  CALL DAT_PRIM( CLOC, CPRIM, STATUS )

*  If so, get its value as a string. Otherwise, use the string "<structure>".
                  IF( CPRIM ) THEN
                     CALL DAT_GET0C( CLOC, CVAL, STATUS )
                     CALL CHR_APPND( CVAL, BUF, IAT )
                  ELSE
                     CALL CHR_APPND( '<structure>', BUF, IAT )
                  END IF

*  Display the output buffer.
                  CALL MSG_OUT( 'POLEXT_MSG7', BUF, STATUS )

*  Annul the component locator.
                  CALL DAT_ANNUL( CLOC, STATUS )

               END DO

            END IF

            CALL ERR_END( STATUS )

         END IF

*  Annul the locator to the POLPACK extension.
         CALL DAT_ANNUL( POLLOC, STATUS )

*  Store the new WCS FrameSet.
         IF( .NOT. RDONLY ) CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  Annul the pointer to the WCS FrameSet.
         CALL AST_ANNUL( IWCS, STATUS )

*  If an error occurred, delete the extensions if they have just been
*  created, flush the error, and continue to process the next NDF.
         IF ( STATUS .NE. SAI__OK ) THEN

            IF( .NOT. RDONLY ) THEN
               CALL ERR_BEGIN( STATUS )
               IF( .NOT. GOTPOL ) CALL NDF_XDEL( INDF, 'POLPACK',
     :                                           STATUS )
               IF( .NOT. GOTCCD ) CALL NDF_XDEL( INDF, 'CCDPACK',
     :                                           STATUS )
               CALL ERR_END( STATUS )
            END IF

            CALL ERR_FLUSH( STATUS )

*  Otherwise, add the name of the NDF to the group of successfully
*  processed NDFs.
         ELSE
            CALL GRP_PUT( IGRP2, 1, NDFNAM, 0, STATUS )
         END IF

         CALL MSG_BLANK( STATUS )

*  Release the NDF.
         CALL NDF_ANNUL( INDF, STATUS )

      END DO

*  Report an error if no NDFs were processed successfully.
      CALL GRP_GRPSZ( IGRP2, NGOOD, STATUS )
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLEXT_5', 'None of the input images '//
     :                 'were processed successfully.', STATUS )
      END IF

*  Write an output list of the NDF names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL POL1_LNAM( 'NAMELIST', 1, NGOOD,
     :                   '# POLEXT - NDF name list', IGRP2, .FALSE.,
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Break out here if status set BAD.
 999  CONTINUE

*  Free GRP groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )
      IF( IGRP4 .NE. GRP__NOID ) CALL GRP_DELET( IGRP4, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLEXT_6', 'POLEXT: Error examining or '//
     :                 'setting POLPACK extension values.', STATUS )
      END IF

      END
