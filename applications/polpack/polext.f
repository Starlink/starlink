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
*     This application can be used to prepare data files prior to 
*     processing them with POLPACK in cases where POLIMP cannot be
*     used (for instance, because the data files do not have any suitable 
*     FITS headers). The values to be stored are supplied explicitly 
*     by means of the application parameters listed below.
*
*     New values for the POLPACK extension items are obtained using the
*     parameters described below. If supplied, these new values are stored
*     in the POLPACK extension items of the supplied data files. New
*     POLPACK extensions are created if necessary. If no new values are
*     supplied for an item, the existing item values (if any) are retained. 
*     The final contents of the POLPACK extension are then listed.

*  Usage:
*     polext in 

*  ADAM Parameters:
*     ANGROT = _REAL (Read)
*        The anti-clockwise angle from the first (X) axis of each image to 
*        the polarimeter reference direction, in degrees. The given value is 
*        stored in all the supplied images, over-writing any existing value. 
*        If a null (!) value is supplied, any image which already has an
*        ANGROT value retains its existing value, and an ANGROT value of zero 
*        is stored otherwise. [!]
*     FILTER = LITERAL (Read)
*        The filter name. The value of extension item WPLATE is appended to 
*        the supplied value (unless the value already includes the WPLATE 
*        value) before being stored. If a null (!) value is supplied, then
*        any existing FILTER value in the POLPACK extension is retained. If 
*        there is no value in the POLPACK extension, then any value in the 
*        CCDPACK extension is used instead. If there is no value in the 
*        CCDPACK extension, then a default value equal to the value of WPLATE 
*        is used. [!]
*     IMGID = LITERAL (Read)
*        A group of image identifier strings. These are arbitrary strings 
*        assigned to intensity frames containing both O and E ray images.
*        They are used to associate the O and E ray images once they have
*        been extracted into separate data files. The supplied group may
*        take the form of a comma separated list of identifiers, or any of 
*        the other forms described in the help on "Group Expressions". A 
*        separate, unique, non-blank identifier should be supplied for each 
*        data file specified by parameter IN, in the same order as the data 
*        files. If a null (!) value is supplied, then any existing IMGID
*        values in the POLPACK extensions are retained. Default values 
*        equal to the name of the data file are used if there is no 
*        existing value. [!]
*     IN = LITERAL (Read)
*        A group of data files. This may take the form of a comma separated 
*        list of file names, or any of the other forms described in the help on "Group 
*        Expressions".
*     NAMELIST = LITERAL (Read)
*        The name of a file to create containing a list of the successfully 
*        processed data files. This file can be used when specifying the input 
*        data files for subsequent applications. No file is created if a null
*        (!) value is given. [!]
*     QUIET = _LOGICAL (Read)
*        If FALSE, then the contents of the POLPACK extension in each data 
*        file are listed before being modified. Otherwise, nothing is written 
*        to the screen. [FALSE] 
*     WPLATE = LITERAL (Read)
*        The half-wave plate position, in degrees, as a string. This must be 
*        one of "0.0", "22.5", "45.0", "67.5" (abbreviations are allowed).
*        The given value is stored in all supplied data files. If a null (!) 
*        value is supplied, any image which already has an WPLATE value 
*        retains its existing value, and an error is reported otherwise
*        (unless the data file contains Stokes vectors in which case no
*        WPLATE value is needed). [!]

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
*     -  If a new POLPACK extension is created, a new Frame is also added to 
*     the WCS component of the NDF and is given the Domain "POLANAL". This 
*     Frame is formed by rotating the pixel coordinates Frame so that the 
*     first axis is parallel to the analyser axis. The angle of rotation is 
*     given by the ANGROT item in the POLPACK extension.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUL-1997 (DSB):
*        Original version.
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
      CHARACTER STOKES*5               ! Current value of STOKES component
      CHARACTER WPL*5                  ! The WPLATE value to store
      CHARACTER WPLATE*5               ! The supplied WPLATE value
      INTEGER ADDED              ! No. of elements added to a group
      INTEGER I                  ! Loop counter
      INTEGER IAT                ! No. of characters in buffer
      INTEGER IGRP1              ! Input NDF group identifier
      INTEGER IGRP2              ! Id for group of names of NDF's processed OK 
      INTEGER IGRP3              ! Id for group of supplied image identifiers
      INTEGER IGRP4              ! Id for group of used IMGID values
      INTEGER INDF               ! Current NDF identifier
      INTEGER INDX               ! Loop variable
      INTEGER J                  ! Loop count
      INTEGER LNDF               ! Length of NDF name
      INTEGER NCOMP              ! No. of component in POLPACK extension
      INTEGER NGOOD              ! No. of NDFs processed successfully
      INTEGER NNDF               ! No. of input NDFs
      INTEGER SIZE               ! No. of elements in a group
      LOGICAL CFLAG              ! Should more values be obtained?
      LOGICAL CPRIM              ! Is component a primative?
      LOGICAL GOTANG             ! Was a ANGROT value given?
      LOGICAL GOTCCD             ! Does NDF contain a CCDPACK extension?
      LOGICAL GOTFIL             ! Was a FILTER value given?
      LOGICAL GOTIMG             ! Were any IMGID values given?
      LOGICAL GOTPOL             ! Does NDF contain a POLPACK extension?
      LOGICAL GOTWPL             ! Was a WPLATE value given?
      LOGICAL QUIET              ! Run silently?
      REAL ANG                   ! The ANGROT value to store
      REAL ANGROT                ! The supplied ANGROT value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we are running quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Access a group of NDFs for processing.
      CALL RDNDF( 'IN', 0, 1, '  Give more image names...', IGRP1, 
     :            NNDF, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( .NOT. QUIET ) THEN
         IF( NNDF .GT. 1 ) THEN
            CALL MSG_SETI( 'N', NNDF )
            CALL MSG_OUT( ' ', '  ^N input images to process... ',
     :                    STATUS )
         ELSE IF( NNDF .EQ. 1 ) THEN
            CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
         ELSE
            CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
            GO TO 999
         END IF
   
         CALL MSG_BLANK( STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the ANGROT value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the ANGROT value.
      CALL PAR_GET0R( 'ANGROT', ANGROT, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )    
         GOTANG = .FALSE.
      ELSE
         GOTANG = .TRUE.
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
            CALL ERR_REP( 'POLEXT_3', 'Only ^ACTVAL values '//
     :                    'supplied for parameter %IMGID. ^NVAL are '//
     :                    'required.', STATUS )

*  Check that all the IMGID values are different and non-blank.
         ELSE
            DO I = 1, NNDF
               CALL GRP_GET( IGRP3, I, 1, IMG, STATUS )

               IF( IMG .EQ. ' ' ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ITH', I )      
                  CALL ERR_REP( 'POLEXT_1', 'The ^ITH value supplied '//
     :                          'for parameter %IMGID is blank.', 
     :                          STATUS )
                  GO TO 999
               END IF

               DO J = I + 1, NNDF
                  CALL GRP_GET( IGRP3, J, 1, IMGJ, STATUS )
                  IF( IMG .EQ. IMGJ .AND. STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'ID', IMG )
                     CALL MSG_SETI( 'ITH', I )      
                     CALL MSG_SETC( 'ITH', CHR_NTH( I ) )      
                     CALL MSG_SETI( 'JTH', J )      
                     CALL MSG_SETC( 'JTH', CHR_NTH( J ) )      
                     CALL ERR_REP( 'POLEXT_1', 'The ^ITH and the '//
     :                             '^JTH values supplied for '//
     :                             'parameter %IMGID are equal '//
     :                             '("^ID").', STATUS )
                     GO TO 999
                  END IF
               END DO
            END DO           
         END IF
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the WPLATE value. Annul the error if a null (!) value was supplied,
*  and set a flag indicating whether to store the WPLATE value.
      CALL PAR_CHOIC( 'WPLATE', ' ', '0.0,22.5,45.0,67.5', .FALSE.,
     :                 WPLATE, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )    
         GOTWPL = .FALSE.
      ELSE
         GOTWPL = .TRUE.
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
         IF( .NOT. QUIET ) THEN
            CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
            CALL MSG_OUT( ' ', '  Processing ''^CURRENT_NDF''',
     :                    STATUS )
         END IF

*  Get the input NDF identifier
         CALL NDG_NDFAS( IGRP1, INDX, 'UPDATE', INDF, STATUS )

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
         STOKES = ' '
         ANG = 0.0
         IMG = NDFNAM( LNDF - IDLEN + 1 : LNDF )
         WPL = ' '

         IF( GOTPOL ) THEN
            CALL NDF_XGT0C( INDF, 'POLPACK', 'STOKES', STOKES, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'ANGROT', ANG, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'FILTER', FILT, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'IMGID', IMG, STATUS )
            CALL NDF_XGT0C( INDF, 'POLPACK', 'WPLATE', WPL, STATUS )

*  Create the POLPACK extension if there isn't one.
         ELSE
            CALL NDF_XNEW( INDF, 'POLPACK', 'POLPACK', 0, 0, POLLOC, 
     :                     STATUS )
            CALL DAT_ANNUL( POLLOC, STATUS )
         END IF

*  Replace these defaults with any supplied values.
         IF( GOTANG ) ANG = ANGROT
         IF( GOTFIL ) FILT = FILTER
         IF( GOTIMG ) CALL GRP_GET( IGRP3, INDX, 1, IMG, STATUS )
         IF( GOTWPL ) WPL = WPLATE

*  Store the values. First do Stokes vector cubes.
         IF( STOKES .NE. ' ' ) THEN
            CALL NDF_XPT0R( ANG, INDF, 'POLPACK', 'ANGROT', STATUS )
            IF( FILT .NE. ' ' ) CALL NDF_XPT0C( FILT, INDF, 'POLPACK', 
     :                                          'FILTER', STATUS )

*  Now do intensity images. Report an error if the WPLATE value
*  is blank.
         ELSE

            IF( WPL .EQ. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POLEXT_1', 'No WPLATE value supplied.',
     :                       STATUS )
            END IF

            CALL NDF_XPT0R( ANG, INDF, 'POLPACK', 'ANGROT', STATUS )
            CALL NDF_XPT0C( FILT, INDF, 'POLPACK', 'FILTER', STATUS )
            CALL NDF_XPT0C( IMG, INDF, 'POLPACK', 'IMGID', STATUS )
            CALL NDF_XPT0C( WPL, INDF, 'POLPACK', 'WPLATE', STATUS )

         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', POLLOC, STATUS )

*  Check the values in the POLPACK extension are usable.
         CALL POL1_CHKEX( INDF, POLLOC, IGRP4, .TRUE., STATUS )

*  If required, display the contents of the POLPACK extension. 
         IF( .NOT. QUIET ) THEN
            CALL ERR_BEGIN( STATUS )

*  Get the number of components in it.
            CALL DAT_NCOMP( POLLOC, NCOMP, STATUS)

*  Tell the user if the extension is empty.
            IF( NCOMP .EQ. 0 ) THEN
               CALL MSG_OUT( ' ', '   The POLPACK extension is empty',
     :                       STATUS )
           
*  Otherwise, index through the structure's components, obtaining locators 
*  and the required information.
            ELSE
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
                  CALL MSG_OUT( ' ', BUF, STATUS )

*  Annul the component locator.
                  CALL DAT_ANNUL( CLOC, STATUS )

               END DO
         
            END IF

            CALL ERR_END( STATUS )

         END IF

*  Annul the locator to the POLPACK extension.
         CALL DAT_ANNUL( POLLOC, STATUS )

*  If an error occurred, delete the extensions if they have just been 
*  created, flush the error, and continue to process the next NDF.
         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_BEGIN( STATUS )
            IF( .NOT. GOTPOL ) CALL NDF_XDEL( INDF, 'POLPACK', STATUS )
            IF( .NOT. GOTCCD ) CALL NDF_XDEL( INDF, 'CCDPACK', STATUS )
            CALL ERR_END( STATUS )

            CALL ERR_FLUSH( STATUS )

*  Otherwise, add the name of the NDF to the group of successfully
*  processed NDFs.
         ELSE
            CALL GRP_PUT( IGRP2, 1, NDFNAM, 0, STATUS )
         END IF

         IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )

*  Release the NDF.
         CALL NDF_ANNUL( INDF, STATUS )

      END DO

*  Report an error if no NDFs were processed successfully.
      CALL GRP_GRPSZ( IGRP2, NGOOD, STATUS )
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLEXT_ALLBAD', 'None of the input images '//
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
         CALL ERR_REP( 'POLEXT_ERR', 'POLEXT: Error examining or '//
     :                 'setting POLPACK extension values.', STATUS )
      END IF

      END
