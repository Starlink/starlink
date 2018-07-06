      SUBROUTINE POLSELECT( STATUS )
*+
*  Name:
*     POLSELECT

*  Purpose:
*     Select vectors from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLSELECT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates an output catalogue that contains a subset
*     of the vectors in the specified input catalogue. The subset can be
*     specified in various ways (see parameter MODE):
*
*     - using the bad pixels in a reference image to mask out unwanted
*     vectors
*
*     - using ARD or AST region specifications to select the required
*     vectors
*
*     - using an algebraic expression to select the required vectors on
*     the basis of their column values

*  Usage:
*     polselect in out mode

*  ADAM Parameters:
*     EXP = LITERAL (Read)
*        This parameter is only used if parameter MODE is set to
*        "EXPRESSION". It should be set to a boolean expression that uses
*        column names from the input catalogue as variables. It should
*        have the format used by the CURSA 'CATSELECT' command (see sun/190).
*        In general, the arithmetic and boolean operators available in
*        either C and Fortran can be used (e.g. '.ge.', '>=', '.and.',
*        '&&', etc.), as can the usual mathematical functions (e.g. 'abs',
*        'tan', 'pow', etc). A vector is selected if the expression
*        evaluates to a  true value.
*     IN = LITERAL (Read)
*        The name of the input catalogue. A file type of .FIT is
*        assumed if none is provided.
*     INVERT = _LOGICAL (Read)
*        If FALSE, all selected vectors are copied to the output
*        catalogue. If TRUE, all unselected vectors are copied to the
*        output catalogue. [FALSE]
*     MASK = NDF (Read)
*        This parameter is only used if parameter MODE is set to "MASK".
*        It specifies the two-dimensional NDF to be used as a mask to
*        select the required vectors. Vectors corresponding to good pixel
*        values in the mask are selected. The mask is assumed to be
*        aligned with the catalogue in pixel coordinates.
*     MODE = LITERAL (Read)
*        Specifies the manner in which the vectors are selected:
*
*        - "MASK": The image specified by parameter MASK is used to
*        select the vectors. If a vector has a position that corresponds
*        to a good pixel in MASK, then the vector is selected.
*
*        - "REGION": The ARD or AST region specified by parameter REGION
*        is used to select the vectors. If a vector falls within the
*        region, then it is selected.
*
*        - "EXPRESSION": The boolean expression specified by parameter
*        EXP is used to select the vectors. If the boolean expression
*        evaluates to TRUE for a vector, then the vector is selected.
*     OUT = LITERAL (Write)
*        The name of the output catalogue. A file type of .FIT is
*        assumed if none is provided.
*     REF = NDF (Read)
*        This parameter is only used if parameter MODE is set to "REGION".
*        It specifies the NDF to which the ARD description refers. It is
*        used to define the pixel coordinate system used by the ARD
*        description. If null (!) is supplied, it is assumed that the
*        pixel coordinate system used by the ARD description is the
*        pixel coordinate system of the input catalogue. [!]
*     REGION = FILENAME (Read)
*        This parameter is only used if parameter MODE is set to "REGION".
*        It should be set to the name of a text file containing a
*        description of the region containing the vectors to be selected.
*        This can either be in the form of an 'ARD description' (see
*        SUN/183), or an 'AST Region' (see SUN/210).

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     22-MAY-2018 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CNF_PAR'          ! CNF_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER NSYM
      PARAMETER ( NSYM = 4 )

*  Local Variables:
      CHARACTER CSYM(NSYM)*6     ! C operators to be replaced
      CHARACTER EXP*255          ! Selection expression
      CHARACTER FIELDS(5)*1024   ! Fields of input catalogue path
      CHARACTER FILNAM*(GRP__SZFNM)! Name of ARD file
      CHARACTER FSYM(NSYM)*5     ! Equivalent F77 operators
      CHARACTER MODE*10          ! How to define selected vectors
      CHARACTER NEWEXP*255       ! Modified selection expression
      CHARACTER ONAME*250        ! Path to output catalogue
      CHARACTER PAT*20           ! Substitution pattern
      INTEGER BADVAL             ! Mask value for unselected pixels
      INTEGER CIIN               ! CAT identifier for input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER EI                 ! Expression identifier
      INTEGER EL                 ! No. of mapped elements
      INTEGER FD                 ! File descriptor
      INTEGER FS                 ! FrameSet connectig region and pixel
      INTEGER GI(2)              ! Identifiers for X and Y columns
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of string
      INTEGER IBASE              ! Original index of base Frame
      INTEGER IGRP               ! Group identifier
      INTEGER INDFM              ! Identifier for mask NDF
      INTEGER INDFR              ! Identifier for reference NDF
      INTEGER INDFS              ! Identifier for mask section NDF
      INTEGER IPIX               ! Index of PIXEL Frame
      INTEGER IPLIST             ! Pointer to selected row list
      INTEGER IPMASK             ! Pointer to pixle mask
      INTEGER IPXY               ! Pointer to X and Y column values
      INTEGER IREG               ! AST pointer for supplied Region
      INTEGER ISYM               ! Symbol index
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER IWCSARD            ! Pointer to AST FrameSet used with ARD_WORK
      INTEGER IWCSR              ! Reference FrameSet
      INTEGER LBNDE(2)           ! Lower pixel bounds of external box
      INTEGER LBNDI(2)           ! Lower pixel bounds of internal box
      INTEGER MAXPOS             ! Position of max value
      INTEGER MINPOS             ! Position of min value
      INTEGER MLBND(2)           ! Lower pixel bounds of catalogue
      INTEGER MUBND(2)           ! Upper pixel bounds of catalogue
      INTEGER NBAD               ! No. of bad values
      INTEGER NFRM               ! No. of frames in FrameSet
      INTEGER NROWIN             ! No. of input rows
      INTEGER NUMREJ             ! No. of rejected rows
      INTEGER NUMSEL             ! No. of selected rows
      INTEGER NX                 ! Pixels per row
      INTEGER NY                 ! Pixels per column
      INTEGER REGVAL             ! Value used ot represent region
      INTEGER SDIM(2)            ! Indicies of significiant pixel axes
      INTEGER SI                 ! Selection identifier
      INTEGER SIR                ! Rejection identifier
      INTEGER SLBND(2)           ! Lower bounds of significiant pixel axes
      INTEGER SUBND(2)           ! Upper bounds of significiant pixel axes
      INTEGER UBNDE(2)           ! Upper pixel bounds of external box
      INTEGER UBNDI(2)           ! Upper pixel bounds of internal box
      INTEGER XOFF               ! Offset to start of X values
      INTEGER YOFF               ! Offset to start of Y values
      LOGICAL CONT               ! ARD description to continue?
      LOGICAL INVERT             ! Invert the selection?
      LOGICAL MATCH              ! Was a substitution performed?
      LOGICAL VERB               ! Verose errors required?
      REAL MAXVAL                ! Max X or Y value
      REAL MINVAL                ! Min X or Y value

      DATA CSYM /    '&&',   '\|\|',   '!=',     '!' /,
     :     FSYM / '.AND.', '.OR.', '.NE.', '.NOT.' /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start AST and NDF contexts.
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CIIN, FIELDS, STATUS )

*  Get the number of rows in the input catalogue.
      CALL CAT_TROWS( CIIN, NROWIN, STATUS )

*  Should we invert the selection?
      CALL PAR_GET0L( 'INVERT', INVERT, STATUS )

*  Open the output catalogue.
      CALL CTG_CREA1( 'OUT', CIOUT, ONAME, STATUS )

*  Read an AST FrameSet from the input catalogue.
      CALL POL1_GTCTW( CIIN, IWCS, STATUS )
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'N', FIELDS(5) )
         CALL ERR_REP( ' ', 'No WCS information found in input '//
     :                 'catalogue "^N".', STATUS )
      END IF

*  See how select vectors are to be specified.
      CALL PAR_CHOIC( 'MODE', 'Expression', 'Mask,Region,Expression',
     :                .TRUE., MODE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

* First deal with selection by boolean expression.
      IF( MODE .EQ. 'EXPRESSION' ) THEN

*  Get the text of the expression from the user.
         CALL PAR_GET0C( 'EXP', EXP, STATUS )

*  Convert the subset of C-like operators that are not supported by CAT
*  to Fortran.
         DO ISYM = 1, NSYM

            IAT = 1
            PAT = '('
            CALL CHR_APPND( CSYM(ISYM), PAT, IAT )
            CALL CHR_APPND( ')=', PAT, IAT )
            CALL CHR_APPND( FSYM(ISYM), PAT, IAT )

            MATCH = AST_CHRSUB( EXP, PAT, NEWEXP, STATUS )
            DO WHILE( MATCH )
               EXP = NEWEXP
               MATCH = AST_CHRSUB( EXP, PAT, NEWEXP, STATUS )
            END DO

         END DO

*  Parse it.
         CALL CAT_EIDNT( CIIN, EXP, EI, STATUS)

*  Use it to select rows from the input catalogue.
         CALL CAT_SELCT( CIIN, EI, INVERT, SI, NUMSEL, SIR, NUMREJ,
     :                   STATUS)

*  Release the expression identifier.
         CALL CAT_TRLSE( EI, STATUS )

*  The other two modes create row selection using CAT_SLIST.
      ELSE

*  Initialise "no group used".
         IGRP = GRP__NOID

*  Allocate work space to hold the data from the (X,Y) columns.
         CALL PSX_CALLOC( NROWIN*2, '_REAL', IPXY, STATUS )

*  Read the (X,Y) columns from the catalogue into the work arrays
*  allocated above. This is done in a single pass through the catalogue
*  in order to speed it up a bit.
         CALL CAT_TIDNT( CIIN, 'X', GI(1), STATUS )
         CALL CAT_TIDNT( CIIN, 'Y', GI(2), STATUS )
         CALL POL1_CTCLM( CIIN, NROWIN, 2, GI,
     :                    %VAL( CNF_PVAL( IPXY ) ), STATUS )

*  Get the max and min X and Y values, with a safety marging of 2 pixels.
         XOFF = 0
         CALL KPG1_MXMNR( .TRUE., NROWIN,
     :                    %VAL( CNF_PVAL( IPXY ) + XOFF ),
     :                    NBAD, MAXVAL, MINVAL, MAXPOS,
     :                    MINPOS, STATUS )
         MUBND( 1 ) = NINT( MAXVAL + 0.5 ) + 2
         MLBND( 1 ) = NINT( MINVAL + 0.5 ) - 2

         YOFF = NROWIN*VAL__NBR
         CALL KPG1_MXMNR( .TRUE., NROWIN,
     :                    %VAL( CNF_PVAL( IPXY ) + YOFF ),
     :                    NBAD, MAXVAL, MINVAL, MAXPOS,
     :                    MINPOS, STATUS )
         MUBND( 2 ) = NINT( MAXVAL + 0.5 ) + 2
         MLBND( 2 ) = NINT( MINVAL + 0.5 ) - 2

*  First deal with selection by region description.
         IF( MODE .EQ. 'REGION' ) THEN

*  Allocate space for the pixel mask array.
            NX = MUBND(1) - MLBND(1) + 1
            NY = MUBND(2) - MLBND(2) + 1
            CALL PSX_CALLOC( NX*NY, '_INTEGER', IPMASK, STATUS )

*  First of all, attempt to get an ARD description...

*  Use a literal parameter to obtain the ARD file to avoid having to give
*  the indirection and continuation.  Call FIO to open the file to ensure
*  that the obtained file exists.  Get the name and add the indirection
*  symbol so that ARD does not treat the filename as a literal ARD
*  description.
            CALL FIO_ASSOC( 'REGION', 'READ', 'LIST', 0, FD, STATUS )
            CALL AIF_FLNAM( 'REGION', FILNAM( 2: ), STATUS )
            FILNAM( 1:1 ) = '^'
            CALL FIO_ANNUL( FD, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the ARD description from the file, into a GRP group.
            CALL ARD_GRPEX( FILNAM, GRP__NOID, IGRP, CONT, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN

*  Take a copy of the WCS FrameSet so that the original is not modified.
               IWCSARD = AST_COPY( IWCS, STATUS )

*  See if a reference NDF is to be used - this defines the pixel coord
*  system associated with the  ARD description. If no reference NDF is
*  supplied, the ARD description pixel coord system is assumed to be the
*  same as the catalogue (X,Y) system. The current Frame in IWCS is used
*  as the default system if there is no COFRAME or WCS keyword in the ARD
*  description. So ensure it is PIXEL coords (in either the catalogue or
*  the reference NDF).
               CALL NDF_ASSOC( 'REF', 'READ', INDFR, STATUS )
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL KPG1_ASGET( INDFR, 2, .FALSE., .TRUE., .TRUE.,
     :                             SDIM, SLBND, SUBND, IWCSR,
     :                             STATUS )
                  CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIX, STATUS )
                  NFRM = AST_GETI( IWCSARD, 'NFrame', STATUS )
                  CALL KPG1_ASMRG( IWCSARD, IWCSR, ' ', .FALSE., 3,
     :                             STATUS )
                  CALL AST_SETI( IWCSARD, 'Current', IPIX+NFRM, STATUS )

                  CALL NDF_ANNUL( INDFR, STATUS )

               ELSE IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL KPG1_ASFFR( IWCSARD, 'PIXEL', IPIX, STATUS )
                  CALL AST_SETI( IWCSARD, 'Current', IPIX, STATUS )
               END IF

*  Use this as the ARD description's WCS FrameSet.
               CALL ARD_WCS( IWCSARD, ' ', STATUS )

*  Create the pixel mask.
               REGVAL = 2
               CALL ARD_WORK( IGRP, 2, MLBND, MUBND, VAL__BADR, .FALSE.,
     :                        REGVAL, %VAL( CNF_PVAL( IPMASK ) ), LBNDI,
     :                        UBNDI, LBNDE, UBNDE, STATUS )
               BADVAL = 0

*  If a non-fatal error occurred accessing the region as an ARD
*  description, annull the error and then try to access it as an
*  AST Region.
            ELSE IF( STATUS .NE. PAR__NULL .AND.
     :               STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL KPG1_GTOBJ( 'REGION', 'Region', AST_ISAREGION, IREG,
     :                          STATUS )

*  If an AST Region was obtained successfully, use it to create a pixel
*  mask.
               CALL AST_INVERT( IWCS, STATUS )
               IBASE = AST_GETI( IWCS, 'Base', STATUS )
               FS = AST_CONVERT( IREG, IWCS, ' ', STATUS )
               CALL AST_SETI( IWCS, 'Base', IBASE, STATUS )
               CALL AST_INVERT( IWCS, STATUS )

               IF( FS .NE. AST__NULL ) THEN
                  NBAD = AST_MASKI( IREG, FS, .TRUE., 2, MLBND, MUBND,
     :                              %VAL( CNF_PVAL( IPMASK ) ), 2,
     :                              STATUS )

               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  CALL ERR_REP( ' ', 'Cannot align the supplied '//
     :                          'Region with the given input '//
     :                          'catalogue - no common coordinate '//
     :                          'system was found.', STATUS )
               END IF
            END IF

* Now deal with selection by mask.
         ELSE IF( MODE .EQ. 'MASK' ) THEN
            CALL NDF_ASSOC( 'MASK', 'READ', INDFM, STATUS )
            CALL NDF_SECT( INDFM, 2, MLBND, MUBND, INDFS, STATUS )
            CALL NDF_MAP( INDFS, 'Data', '_INTEGER', 'Read', IPMASK, EL,
     :                    STATUS )
            BADVAL = VAL__BADI
         END IF

*  Create a list holding the indices of the rows that are inside the mask.
         CALL PSX_CALLOC( NROWIN, '_INTEGER', IPLIST, STATUS )
         CALL POL1_MASK( MLBND(1), MUBND(1), MLBND(2), MUBND(2),
     :                   %VAL( CNF_PVAL( IPMASK ) ), NROWIN,
     :                   %VAL( CNF_PVAL( IPXY ) + XOFF ),
     :                   %VAL( CNF_PVAL( IPXY ) + YOFF ), BADVAL,
     :                   %VAL( CNF_PVAL( IPLIST ) ), NUMSEL, STATUS )

*  Free resources.
         IF( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )
         CALL PSX_FREE( IPXY, STATUS )
         IF( MODE. NE. 'MASK' ) CALL PSX_FREE( IPMASK, STATUS )
         CALL CAT_TRLSE( GI( 1 ), STATUS )
         CALL CAT_TRLSE( GI( 2 ), STATUS )

*  Use the row list created above to select rows from the input catalogue.
         IF( NUMSEL .GT. 0 .OR. INVERT  ) THEN
            CALL CAT_SLIST( NUMSEL, %VAL( CNF_PVAL( IPLIST ) ), ' ',
     :                      INVERT, CIIN, SI, SIR, NUMREJ, STATUS)
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'The output catalogue would be empty.',
     :                    STATUS )
         END IF

*  Free the row list.
         CALL PSX_FREE( IPLIST, STATUS )

      END IF

*  Copy the polpack version number from input to output.
      CALL POL1_CPVRC( CIIN, CIOUT, STATUS )

*  If the selection is to be inverted, use the rejected rows identifier in
*  place of the selected rows identifier.
      IF( INVERT ) THEN
         CALL CAT_TRLSE( SI, STATUS )
         SI = SIR
         NUMSEL = NUMREJ
      END  IF

      IF( NUMSEL .EQ. NROWIN ) THEN
         CALL MSG_OUT( ' ', '   All input vectors were copied without'//
     :                 ' any rejections', STATUS )
      ELSE IF( NUMSEL .EQ. 1 ) THEN
         CALL MSG_OUT( ' ', '   1 vector copied', STATUS )
      ELSE
         CALL MSG_SETI( 'N', NUMSEL )
         CALL MSG_OUT( ' ', '   ^N vectors copied', STATUS )
      END IF

* Copy the selected rows to the output catalogue, excluding textual
* information. Then release the selection identifier.
      CALL POL1_CPCAT( SI, CIOUT, AST__NULL, STATUS )
      CALL CAT_TRLSE( SI, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Close the output catalogue, storing a copy of the input WCS information.
      CALL POL1_CLCAT( IWCS, CIOUT, STATUS )

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CIIN, STATUS )

*  End the NDF and AST contexts.
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLSELECT_ERR', 'POLSELECT: Error selecting '//
     :                 'vectors from a polarization catalogue.',
     :                 STATUS )
      END IF

      END
