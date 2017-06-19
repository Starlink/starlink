      SUBROUTINE POLZCONV( STATUS )
*+
*  Name:
*     POLZCONV

*  Purpose:
*     Convert a Z axis value to a Z column value

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLZCONV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a supplied Z axis value into the corresponding
*     Z column value within a given catalogue, returning the nearest
*     value for which some data exists within the catalogue. The resulting
*     Z column value and Z axis value are written to output parameters. This
*     application is primarily for use within scripts.

*  Usage:
*     polzconv cat

*  ADAM Parameters:
*     CAT = LITERAL (Read)
*        The name of the input catalogue. This may be in any format
*        supported by the CAT library (see SUN/181). A file type of .FIT
*        is assumed if no file type is supplied.
*     COLX = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along the first axis. A list of available column names is
*        displayed if a non-existent column name is given. See the "Notes"
*        section below for further details of how these positions are
*        interpreted. [X]
*     COLY = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along the second axis. A list of available column names is
*        displayed if a non-existent column name is given. See the "Notes"
*        section below for further details of how these positions are
*        interpreted. [Y]
*     COLZ = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along a third axis. A list of available column names is
*        displayed if a non-existent column name is given. A null (!)
*        value should be supplied if no third axis is to be used. The dynamic
*        default is 'Z' if the catalogue contains a Z column, and null
*        (!) otherwise. See also parameter ZAXVAL. []
*     ZAXVAL = LITERAL (Read)
*        Specifies the Z axis value to be converted. The
*        given value should be in the current coordinate Frame of the
*        supplied catalogue (see parameter COLZ). For instance, if the
*        current coordinate Frame contains a calibrated wavelength axis,
*        the value should be given in the units specified in that frame
*        (anstroms, nanometres, etc.). If the wavelength axis has not been
*        calibrated, the value will probably need to be supplied in units
*        of pixels. Entering a colon (":") for the parameter will result in
*        a description of the current coordinate Frame being shown. This may
*        help to determine the units in which a value is expected. The
*        value actually used is the closest available value within the
*        catalogue. This value is displayed on the screen and written to
*        parameter ZVALUE. The ZAXVAL parameter is only accessed if a
*        null (!) value is supplied for parameter ZCOLVAL. See also
*        parameter COLZ.
*     ZCOLVAL = _REAL (Read)
*        Specifies the Z column value for the vectors to be displayed.
*        The given value should be in the same coordinate system as the
*        values stored in the Z column of the catalogue (usually pixels).
*        This parameter provides an alternative to the ZAXVAL parameter.
*        Use the ZCOLVAL parameter to specify the Z value in pixels, and
*        the ZAXVAL parameter to specify the Z value in Hertz, angstroms,
*        nanometres, etc (if the Z axis has been calibrated). If a null
*        value is supplied for ZCOLVAL, then ZAXVAL is used to determine
*        the Z value to display. [!]
*     ZCOLUSED = LITERAL (Write)
*        The formatted Z column value to use. This is the nearest value to the
*        supplied ZCOLVAL or ZAXVAL for which data exists in the catalogue.
*        The string "***" is stored if the catalogue does not include a Z
*        axis.
*     ZAXUSED = LITERAL (Write)
*        The formatted Z axis value to use. This is the nearest value to the
*        supplied ZAXVAL for which data exists in the catalogue.
*        The string "***" is stored if the catalogue does not include a Z
*        axis, or if a vcalue for supplied for ZCOLVAL.

*  Notes:
*     -  The columns specified by parameters COLX and COLY should hold
*     coordinates in the "Base Frame" of the WCS information stored as
*     an AST FrameSet (see SUN/210) in the supplied catalogue. If the
*     catalogue has been produced by one of the POLPACK application polvec
*     or polbin, then the Base Frame will be pixel co-ordinates within the
*     aligned intensity images, and these will be stored in columns with
*     names "X" and "Y".

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-FEB-2001 (DSB):
*        Original version.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     19-JUN-2017 (DSB):
*        Ensure double precision values are written out with an E exponent,
*        rather than a D exponent (TCL does not understand D exponents).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER COLNM*30         ! External column name
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER ZTEXT*80         ! Text giving Z value
      DOUBLE PRECISION ZAX       ! Z axis value
      DOUBLE PRECISION ZCOL      ! Z column value
      INTEGER CI                 ! CAT catalogue identifier
      INTEGER GIS( 3 )           ! CAT identifiers for X,Y,Z catalogue columns
      INTEGER IAT                ! Used length of a string
      INTEGER IPZ                ! Pointer to array holding vector Z positions
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER MAPS( 3 )          ! Individual axis mappings
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER NBAD               ! No. of bad values
      INTEGER NGZ                ! No. of good vector Z values
      INTEGER NVAL               ! No. of values obtained
      INTEGER NVEC               ! No. of vectors in catalogue
      LOGICAL VERB               ! Verose errors required?
      REAL SZHI                  ! Max Z value in catalogue
      REAL SZLO                  ! Min Z value in catalogue
      REAL ZUSE                  ! Z column value to display

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned Z axis and column values.
      ZAX = AST__BAD
      ZCOL = AST__BAD

*  Initialise workspace pointers.
      IPZ = 0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'CAT', VERB, 'READ', CI, FIELDS, STATUS )

*  Get the external column name used for the Z column.
      CALL POL1_COLNM( 'Z', .FALSE., COLNM, STATUS )

*  Attempt to get an identifier for a column with this name (COLNM).
      IF( COLNM .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIDNT( CI, COLNM, GIS( 3 ), STATUS )

*  If found, release the CAT identifier and use a dynamic default of COLNM
*  for parameter COLZ. Otherwise, annul the error and use no dynamic
*  default.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TRLSE( GIS( 3 ), STATUS )
            CALL PAR_DEF0C( 'COLZ', COLNM, STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an identifier for the Z column. If a null (!) value is
*  given, annul the error and leave ZAX and ZCOL indicating that no Z
*  column is available.
      CALL POL1_GTCTC( 'COLZ', CI, CAT__FITYP, ' ', GIS( 3 ), STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If a Z column is available...
      ELSE

*  Set up dynamic defaults for the Z and Y column names.
         CALL POL1_COLNM( 'X', .FALSE., COLNM, STATUS )
         IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLX', COLNM, STATUS )

         CALL POL1_COLNM( 'Y', .FALSE., COLNM, STATUS )
         IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLY', COLNM, STATUS )

*  Get CAT identifiers for the columns which are to be used to define the
*  X and Y coordinates.
         CALL POL1_GTCTC( 'COLX', CI, CAT__FITYP, ' ', GIS( 1 ),
     :                    STATUS )
         CALL POL1_GTCTC( 'COLY', CI, CAT__FITYP, ' ', GIS( 2 ),
     :                    STATUS )

*  Find the number of rows in the catalogue.
         CALL CAT_TROWS( CI, NVEC, STATUS )

*  Allocate workspace.
         CALL PSX_CALLOC( NVEC, '_REAL', IPZ, STATUS )

*  Copy the Z column into the corresponding array.
         CALL POL1_CPCTR( CI, GIS( 3 ), NVEC, %VAL( CNF_PVAL( IPZ ) ),
     :                    NGZ,
     :                    STATUS )

*  Report an error if there were no good Z values.
         IF( NGZ .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TIQAC( GIS( 3 ), 'NAME', NAME, STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'POLPLOT_1', 'The Z coordinate column '//
     :                    '(^NAME) contains no data.', STATUS )
         END IF

*  Attempt to read an AST FrameSet from the catalogue. The Base Frame of
*  this FrameSet will be spanned by axes corresponding to the columns
*  in GIS.
         CALL POL1_GTCTA( CI, 3, GIS, IWCS, STATUS )

*  Get the name of the Z column.
         CALL CAT_TIQAC( GIS( 3 ), 'NAME', NAME, STATUS )

*  Split the Base Frame to Current Frame Mapping up into 3 separate
*  1D Mappings.
         CALL KPG1_ASSPL( IWCS, 3, MAPS, STATUS )

*  Find the max and min Z values in the Z column.
         CALL KPG1_MXMNR( .TRUE., NVEC, %VAL( CNF_PVAL( IPZ ) ),
     :                    NBAD, SZHI,
     :                    SZLO, MAXPOS, MINPOS, STATUS )

*  If there is only a single Z value available, use it.
         IF( SZHI .LE. SZLO ) THEN
            ZCOL = DBLE( SZLO )

*  Otherwise, allow the user to choose a Z value.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  First see if the user wants to give the Z value in pixels.
            CALL PAR_GET0D( 'ZCOLVAL', ZCOL, STATUS )

*  If not, annul the error and get a current Frame Z value.
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  Get the required Z value in the current Frame.
               CALL KPG1_GTAXV( 'ZAXVAL', 1, .TRUE., IWCS, 3, ZAX,
     :                          NVAL, STATUS )

*  Use the third Mapping to transform the current Frame Z value into a
*  base Frame Z value.
               IF( MAPS( 3 ) .NE. AST__NULL ) THEN
                  CALL AST_TRAN1( MAPS( 3 ), 1, ZAX, .FALSE., ZCOL,
     :                            STATUS )
               ELSE
                  ZCOL = AST__BAD
               END IF

*  If the result was undefined, issue an error, flush it, and use the
*  lowest availabel Z value.
               IF( ZCOL .EQ. AST__BAD ) THEN
                  CALL PAR_GET0C( 'ZAXVAL', ZTEXT, STATUS )

                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', NAME )
                  CALL MSG_SETC( 'Z', ZTEXT )
                  CALL MSG_SETR( 'ZL', SZLO )
                  CALL ERR_REP( 'POLPLOT_ERR1', 'The supplied '//
     :                    'Z axis value (^Z) could not be converted '//
     :                    'into a value for the ^NAME column in the '//
     :                    'catalogue. The lowest available column '//
     :                    'value (^ZL) will be used instead.', STATUS )

                  CALL ERR_FLUSH( STATUS )
                  ZCOL = DBLE( SZLO )
               END IF

            END IF

*  Find the closest available value to the requested Z value.
            CALL POL1_FCLOS( NVEC, %VAL( CNF_PVAL( IPZ ) ),
     :                       REAL( ZCOL ), ZUSE,
     :                       STATUS )
            ZCOL = DBLE( ZUSE )
         END IF

*  Find the Z axis value corresponding to the Z column value to be used.
         IF( MAPS( 3 ) .NE. AST__NULL ) THEN
            CALL AST_TRAN1( MAPS( 3 ), 1, ZCOL, .TRUE., ZAX, STATUS )
         ELSE
            ZAX = AST__BAD
         END IF
      END IF

*  If we have a Z axis value, format it, and write to the output
*  parameter.
      IF( ZAX .NE. AST__BAD ) THEN
         ZTEXT = AST_FORMAT( IWCS, 3, ZAX, STATUS )
      ELSE
         ZTEXT = '***'
      END IF

      CALL PAR_PUT0C( 'ZAXUSE', ZTEXT, STATUS )

*  If we have a Z column value, format it, and write to the output
*  parameter.
      IF( ZCOL .NE. AST__BAD ) THEN
         IAT = 0
         CALL POL1_PUTD( ZCOL, ZTEXT, IAT )
      ELSE
         IAT = 3
         ZTEXT = '***'
      END IF

      CALL PAR_PUT0C( 'ZCOLUSE', ZTEXT( : IAT ), STATUS )

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release work space.
      IF( IPZ .NE. 0 ) CALL PSX_FREE( IPZ, STATUS )

*  Release the catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLZCONV_ERR', 'POLZCONV: Error finding a Z '//
     :                 'column value.', STATUS )
      END IF

      END
