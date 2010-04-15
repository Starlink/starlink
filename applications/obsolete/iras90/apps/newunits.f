      SUBROUTINE NEWUNITS( STATUS )
*+
*  Name:
*     NEWUNITS

*  Purpose:
*     Scale data into a new system of units.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NEWUNITS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The data and (if defined) variance values stored in the input
*     NDFs are scaled into the units specified by parameter UNITS, and
*     written to the output NDFs. The NDF component UNITS is modified to
*     hold the name of the new units. NDFs holding either CRDD files or
*     IRAS90 images (but not CPC images) may be given as input.

*  Usage:
*     NEWUNITS IN OUT UNITS

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90" for more
*        information on history.               [current history setting]
*     IN = NDF (Read)
*        A group of input NDFs. This should be in the form of a group
*        expression (see help on "Group_expressions").
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN.  This should be in the
*        form of a group expression (see help on "Group_expressions").
*        Expressions such as "*_NEW" are expanded by replacing the "*"
*        character with each input NDF in turn.
*     UNITS = LITERAL (Read)
*        The units in which the output values are required.  See
*        help on "Data_units" for a list of the available units.

*  Examples:
*     NEWUNITS M51* *_SB MJy/sr
*        This example copies all NDFs starting with the string "M51" to
*        a set of corresponding output NDFs. The name of each output
*        NDF is formed by extending the name of the input NDF with the
*        string "_SB". The data values are converted into Mega-Janskys
*        per steradian. If any of the inputs contain variance values,
*        they are converted into units of (MJy/sr)**2.

*  Notes:
*     -  If the conversion requires the solid angle of an image pixel
*     to be known (eg from Jy/sr to Jy/PIXEL) the same nominal size is
*     used for all pixels. If the image covers a large area of the sky
*     this will not be appropriate unless the image is in an equal area
*     projection.
*     -  If the conversion requires the solid angle of a CRDD sample
*     to be used (eg from Jy to MJy/sr), the effective detector solid
*     angles listed in the help topic "Detector_solid_angles" are used.
*     -  If the conversion requires the effective bandwidth of the
*     detectors to be used (eg from pW/(M**2) to Jy), the values listed
*     in help topic "Detector_bandwidths" are used. This includes no
*     colour correction.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER CLIST*8               ! List of NDF components to be
                                      ! mapped.
      CHARACTER COMC*1                ! Output group comment character.
      CHARACTER HSTORY(2)*(MSG__SZMSG)! Lines of history to be added to
                                      ! each output NDF.
      CHARACTER IILOC*(DAT__SZLOC)    ! Locator to the IMAGE_INFO
                                      ! extension.
      CHARACTER INSTRM*(IRI__SZINS)   ! IRAS instrument.
      CHARACTER OLDUN*80              ! Input units.
      CHARACTER OUTNDF*(GRP__SZNAM)   ! The name of the current output
                                      ! NDF.
      CHARACTER TEXT*(GRP__SZNAM)     ! Text for output group list file.
      CHARACTER TYPE*(IRI__SZTYP)     ! IRAS image type.
      CHARACTER CULIST*255            ! List of CRDD units.
      CHARACTER UNITS*80              ! Output units.
      CHARACTER XLOC*(DAT__SZLOC)     ! Locator to the IRAS extension.


      DOUBLE PRECISION PIXDIM( 2 )! Nominal pixel dimensions, in
                                 ! radians.
      DOUBLE PRECISION PIXSOL    ! Nominal pixel solid angle, in
                                 ! steradians.


      INTEGER BAND               ! IRAS waveband index.
      INTEGER EL                 ! No. of elements in the NDF.
      INTEGER I                  ! Index into input and output groups.
      INTEGER IDA                ! IRA identifier for input image.
      INTEGER IDC                ! IRC identifier for input CRDD file.
      INTEGER IGRP1              ! Identifier for group holding input
                                 ! NDFs.
      INTEGER IGRP2              ! Identifier for group holding all
                                 ! candidate output NDFs.
      INTEGER INDF1              ! Identifier for the input NDF.
      INTEGER INDF2              ! Identifier for the output NDF.
      INTEGER IPIN( 2 )          ! Pointers to mapped input arrays.
      INTEGER IPOUT( 2 )         ! Pointers to mapped output arrays.
      INTEGER LBND( 2 )          ! NDF lower bounds.
      INTEGER LUNIT              ! Used length of UNITS
      INTEGER NDIM               ! No. of dimensions in the input NDF.
      INTEGER NOUT               ! No. of good output NDFs.
      INTEGER SIZE               ! Total size of the input group.
      INTEGER SIZEO              ! Total size of the output group.
      INTEGER TLEN               ! Used length of text string.
      INTEGER UBND( 2 )          ! NDF upper bounds.


      LOGICAL CRDD               ! True if input is a CRDD file, false
                                 ! if it is an image.
      LOGICAL DBAD               ! True if there are any bad values in
                                 ! the data array.
      LOGICAL THERE              ! True if an object is found.
      LOGICAL VAR                ! True if VARIANCE is in a defined
                                 ! state.
      LOGICAL VBAD               ! True if there are any bad values in
                                 ! the variance array.



*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CRDD = .FALSE.

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...',
     :                IGRP1, SIZE, STATUS )

*  Similarly, get a group containing the names of the output NDFs.
*  Base modification elements on the group containing the input NDFs.
      CALL IRM_WRNDF( 'OUT', IGRP1, SIZE, SIZE,
     :                '  Give more NDF names...',
     :                 IGRP2, SIZEO, STATUS )

*  Get the new units required for the output NDFs, and store its used
*  length.
      CALL NEWUA0( 'UNITS', UNITS, STATUS )
      LUNIT = CHR_LEN( UNITS )

*  Get the character used to introduce comments into group expressions
*  relating to the output group.
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRA and IRC systems.
      CALL IRA_INIT( STATUS )
      CALL IRC_INIT( STATUS )

*  Get a list of legal CRDD units. Append commas to the start and end
*  of the list.
      CALL IRC_IUNIT( CULIST( 2: ), STATUS )
      CULIST( 1 : 1 ) = ','
      CULIST( CHR_LEN( CULIST ) + 1 : ) = ','

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the number of good output NDFs produced so far.
      NOUT = 0

*  Loop round each NDF to be processed.
      DO I = 1, SIZE
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__NORM, 'NEWUNITS_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Obtain an identifier for the output NDF, propagating all components
*  from the input to the output except DATA, VARIANCE and UNITS (the
*  HISTORY, LABEL, TITLE and all extensions are propagated by default).
         CALL NDG_NDFPR( INDF1, 'QUALITY,AXIS', IGRP2, I, INDF2,
     :                   STATUS )

*  Store the new units in the output NDF.
         CALL NDF_CPUT( UNITS, INDF2, 'UNITS', STATUS )

*  Get a locator to the IRAS extension in the input NDF.
         CALL NDF_XLOC( INDF1, 'IRAS', 'READ', XLOC, STATUS )

*  See if it contains a component called CRDD_INFO.
         CALL DAT_THERE( XLOC, 'CRDD_INFO', THERE, STATUS )

*  If it does, assume it is a CRDD file. Report an error if the
*  suplied units are not legal CRDD units.
         IF( THERE ) THEN
            CRDD = .TRUE.

            IF( INDEX( CULIST, ','//UNITS( : LUNIT )//',' ) .EQ. 0
     :          .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'U', UNITS )
               CALL ERR_REP( 'NEWUNITS_ERR1',
     :                'NEWUNITS: Cannot convert CRDD files to ^U units',
     :                       STATUS )
            END IF

*  If it is not a CRDD file, see if the IRAS extension contains a
*  component called IMAGE_INFO.
         ELSE
            CALL DAT_THERE( XLOC, 'IMAGE_INFO', THERE, STATUS )

*  If it does assume it is an IRAS90 image. Report an error if the
*  suplied units are not legal image units.
            IF( THERE ) THEN
               CRDD = .FALSE.

               IF( INDEX( ','//IRI__UNITS//',',
     :                    ','//UNITS( : LUNIT )//',' ) .EQ. 0
     :             .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'U', UNITS )
                  CALL ERR_REP( 'NEWUNITS_ERR2',
     :             'NEWUNITS: Cannot convert IRAS90 images to ^U units',
     :                          STATUS )
               END IF

*  Report an error if neither component can be found.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NEWUNITS_ERR3',
     :  'NEWUNITS: The NDF is neither a CRDD file nor an IRAS90 image',
     :                       STATUS )
            END IF

         END IF

*  Construct a list of the components of the input and output NDFs
*  which are to be mapped. This will always include the DATA array and
*  will also include the VARIANCE array if it is defined in the input.
         CALL NDF_STATE( INDF1, 'VAR', VAR, STATUS )
         IF( VAR ) THEN
            CLIST = 'DATA,VAR'
         ELSE
            CLIST = 'DATA'
         END IF

*  Map the required arrays from from the input and output NDFs.
         CALL NDF_MAP( INDF1, CLIST, '_REAL', 'READ', IPIN, EL, STATUS )
         CALL NDF_MAP( INDF2, CLIST, '_REAL', 'WRITE', IPOUT, EL,
     :                 STATUS )

*  If the input is a CRDD file, attempt to import it into the IRC
*  system.
         IF( CRDD ) THEN
            CALL IRC_IMPRT( INDF1, IDC, STATUS )

*  Get the units of the data array.
            CALL NDF_CGET( INDF1, 'UNITS', OLDUN, STATUS )

*  Get the bounds of the NDF.
            CALL NDF_BOUND( INDF1, 2, LBND, UBND, NDIM, STATUS )

*  Modify the data and (if defined) variance components of the output
*  NDF.
            CALL NEWUA1( VAR, IDC, UNITS, OLDUN, LBND( 1 ), UBND( 1 ),
     :                   LBND( 2 ), UBND( 2 ), %VAL( IPIN( 1 ) ),
     :                   %VAL( IPIN( 2 ) ), %VAL( IPOUT( 1 ) ),
     :                   %VAL( IPOUT( 2 ) ), DBAD, VBAD, STATUS )

*  Release the IRC identifier.
            CALL IRC_ANNUL( IDC, STATUS )

*  If the input is an IRAS90 image, check the IMAGE_INFO structure. A
*  locator to it is returned, but is not needed, so annul it.
         ELSE
            CALL IRI_OLD( INDF1, INSTRM, BAND, TYPE, OLDUN, IILOC,
     :                    STATUS )
            CALL DAT_ANNUL( IILOC, STATUS )

*  Report an error if the image is a CPC image.
            IF( INSTRM .EQ. 'CPC' .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NEWUNITS_ERR4',
     :      'NEWUNITS: Input is a CPC image, which cannot be processed',
     :                       STATUS )
            END IF

*  Import the image into the IRA system, get the nominal pixel
*  dimensions, convert to a pixel solid angle, and release the input
*  from the IRA system.
            CALL IRA_IMPRT( INDF1, IDA, STATUS )
            CALL IRA_PIXSZ( IDA, PIXDIM, STATUS )
            PIXSOL = PIXDIM(1)*PIXDIM(2)
            CALL IRA_ANNUL( IDA, STATUS )

*  Modify the data and (if defined) variance components.
            CALL NEWUA2( VAR, PIXSOL, BAND, UNITS, OLDUN, EL,
     :                   %VAL( IPIN( 1 ) ), %VAL( IPIN( 2 ) ),
     :                   %VAL( IPOUT( 1 ) ), %VAL( IPOUT( 2 ) ),
     :                   DBAD, VBAD, STATUS )

         END IF

*  Set the output bad pixel flags.
         CALL NDF_SBAD( DBAD, INDF2, 'DATA', STATUS )
         IF( VAR ) CALL NDF_SBAD( VBAD, INDF2, 'VAR', STATUS )

*  Add a history record to the output NDF.
         CALL NDF_MSG( 'IN', INDF1 )
         CALL NDF_MSG( 'OUT', INDF2 )
         CALL MSG_LOAD( ' ', '^OUT propagated from ^IN',
     :                  HSTORY( 1 ), TLEN, STATUS )

         CALL MSG_SETC( 'U1', OLDUN )
         CALL MSG_SETC( 'U2', UNITS )
         CALL MSG_LOAD( ' ', 'Data units changed from ^U1 to ^U2',
     :                  HSTORY( 2 ), TLEN, STATUS )

         CALL IRM_HIST( 'HISTORY', INDF2, 'IRAS90:NEWUNITS', 2,
     :                  HSTORY, STATUS )

*  Annul the locator to the input IRAS extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( INDF2, STATUS )
         ELSE
            CALL NDF_ANNUL( INDF2, STATUS )
         END IF

*  If an error occured processing the current input NDF, flush the
*  error.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )

*  Give a warning telling the user that no output NDF will be created
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, OUTNDF, STATUS )
            CALL MSG_SETC( 'NDF', OUTNDF )
            CALL MSG_OUTIF( MSG__QUIET, 'NEWUNITS_MSG2',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//OUTNDF( : CHR_LEN( OUTNDF ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, I, STATUS )

*  If no error occurred, increment the number of good output NDFs.
         ELSE
            NOUT = NOUT + 1
         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Assign a group expression to the output parameter NDFLIST which
*  specifies all the output NDFs. NDFLIST should normally be associated
*  with a suitable global parameter to cause its value to be passed on
*  to the next application.  The output parameter NDFLIST is not
*  advertised as a user parameter since users will normally not be
*  aware of the existence of global parameter, and so will not know
*  how to assign a value to it.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP2, 'NEWUNITS',
     :                                   STATUS )

*  Delete all groups.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  Close the IRA and IRC systems.
      CALL IRA_CLOSE( STATUS )
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested,
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'NEWUNITS_ERR5',
     :    'NEWUNITS: Unable to convert data to a new system of units.',
     :                    STATUS )
         END IF

      END IF

      END
