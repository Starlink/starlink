      SUBROUTINE MOCGEN(STATUS )
*+
*  Name:
*     MOCGEN

*  Purpose:
*     Creates a Multi-Order Coverage map describing regions of an image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MOCGEN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a Multi-Order Coverage (MOC) map describing
*     selected regions of the sky, using the scheme described in version
*     1.1 of the MOC recommendation published by the Internatiobal
*     Virtual Observatory Alliance (IVOA).
*
*     The regions of sky to be included in the MOC may be specified in
*     several ways (see parameter MODE).

*  Usage:
*     mocgen in out mode

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF component to be used if Parameter MODE is "Good" or
*        "Bad".  It may be "Data" or "Variance". ["Data"]
*     FORMAT = LITERAL (Read)
*        The format to use when generating the output MOC specified by
*        Parameter OUT.
*
*        - "FITS" -- The output MOC is stored as a binary table extension
*        in a FITS file, using the conventions described in version 1.1
*        of the IVOA's MOC recommendation.
*
*        - "AST" -- The output MOC is stored as a text file using native
*        AST encoding.
*
*        - "String" -- The output MOC is stored as a text file using the
*        "string" encoding described in version 1.1 of the IVOA's MOC
*        recommendation.

*        - "JSON" -- The output MOC is stored as a text file using the
*        JSON encoding described in version 1.1 of the IVOA's MOC
*        recommendation.
*
*        ["FITS"]
*     IN = NDF (Read)
*        The input NDF. Must be two-dimensional.
*     MAXRES = _REAL (Read)
*        The size of the smallest cells in the returned MOC, in
*        arc-seconds. The nearest of the valid values defined in the MOC
*        recommendation is used. The default value is the largest legal
*        value that results in the cells in the Moc being no larger than
*        the size of the pixels in the centre of the supplied NDF. [!]
*     MINRES = _REAL (Read)
*        The size of the largest feature that may be missed in the
*        supplied NDF, in arc-seconds. It gives the resolution of the
*        initial grid used to identify areas that are inside the MOC.
*        Bounded "holes" or "islands" in the NDF that are smaller than
*        one cell of this initial grid may be missed (i.e. such holes may
*        be "filled in" and islands omitted in the resulting Moc). The
*        default value 16 times Parameter MaxRes. [!]
*     MODE = LITERAL (Read)
*        The mode used to specify the sky regions to include in the
*        output MOC.
*
*        - "Good" -- The output MOC contains the good pixels in the input
*        NDF specified by Parameter IN.
*
*        - "Bad" -- The output MOC contains the bad pixels in the input
*        NDF specified by Parameter IN.
*
*        - "Qual" -- The output MOC contains the pixels that have the
*        quality specified by Parameter QEXP within the input NDF
*        specified by parameter IN.
*
*        ["Good"]
*     OUT = FILENAME (Write)
*        Name of the file in which to store the MOC description of the
*        selected regions. The format to use is specified by Parameter
*        FORMAT. If Parameter is "FITS", ".fits" is appended to the
*        supplied file name if no other file type is included in the
*        supplied string.
*     QEXP = LITERAL (Read)
*        The quality expression. Only used if Parameter MOD is "QUAL".

*  Examples:
*     mocgen m31 m31.fits
*        Generates a a MOC description of the good pixels in NDF "m31",
*        storing the MOC as a binary table in FITS file "m31.fits".

*  Related Applications:
*     KAPPA: REGIONMASK.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     14-MAY-2019 (DSB)
*        Original version
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Array dimensionality
      PARAMETER ( NDIM = 2 )     ! 2-d arrays only

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER COMP*10          ! NDF component from IN1 to be unmapped
      CHARACTER DTYPE*( NDF__SZFTP ) ! Full data type name
      CHARACTER FILE*(GRP__SZFNM)! Output file name
      CHARACTER FMT*6            ! FOrmat of output MOC
      CHARACTER ITYPE*( NDF__SZTYP ) ! HDS Data type name
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to quality information
      CHARACTER MCOMP*10         ! NDF component from IN1 to be mapped
      CHARACTER MODE*4           ! Region selection mode
      CHARACTER QEXP*(IRQ__SZQEX)! Quality expression
      CHARACTER UNDEF(IRQ__QNREF)*(IRQ__SZQNM)! List of undefined quality
                                 !  names referenced in the quality expression
      CHARACTER XNAME*(DAT__SZNAM)! Name of NDF extension containing quality
                                 ! name information
      INTEGER DIMS( NDIM )       ! DImensions of significant pixel axes
      INTEGER ERRPNT             ! Offset to error within quality exprs.
      INTEGER IDQ                ! Identifier for compiled quality exprs
      INTEGER INDF               ! Input NDF identifier
      INTEGER IP1                ! Pointer to pixel array
      INTEGER IWCS               ! Input NDF WCS FrameSet
      INTEGER MOC                ! AST identifier for output MOC
      INTEGER NC                 ! Number of characters used
      INTEGER NEL                ! Number of pixels in array
      INTEGER NUNDEF             ! Number of undefined quality names
                                 ! referenced in the quality expression
      INTEGER OPER               ! Operator for comparing values with bad
      INTEGER SDIM( NDIM )       ! Indicies of significant pixel axes
      INTEGER SLBND( NDIM )      ! Lower bounds of significant pixel axes
      INTEGER SUBND( NDIM )      ! Upper bounds of significant pixel axes
      LOGICAL ALLBAD             ! All pixels bad?
      LOGICAL NOBAD              ! No pixels bad?
      REAL MAXRES                ! Max resolution of MOC in arc-seconds
      REAL MINRES                ! Min resolution of MOC in arc-seconds
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF, STATUS )

*  Get the WCS FrameSet from the NDF, reporting an error if the NDF is
*  not two dimensional.
      CALL KPG1_ASGET( INDF, NDIM, .FALSE., .TRUE., .FALSE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get the corresponding dimensions. */
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  Get output format for the MOC.
      CALL PAR_CHOIC( 'FORMAT', 'FITS', 'FITS,AST,String,JSON', .TRUE.,
     :                FMT, STATUS )

*  Get the maximum and minimum resolutions, supplying suitable defaults
*  if a null (!) value is supplied by the user.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0R( 'MAXRES', MAXRES, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MAXRES = VAL__BADR
         END IF
      END IF

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0R( 'MINRES', MINRES, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MINRES = VAL__BADR
         END IF
      END IF

*  Get the mode for selecting the regions.
      CALL PAR_CHOIC( 'MODE', 'Good', 'Good,Bad,Qual', .TRUE., MODE,
     :                STATUS )

*  If "Good" or "Bad", find which NDF component to use, get the best data
*  type to use (out of those supported by this application) and map it.
      IF( MODE .EQ. 'GOOD' .OR. MODE .EQ. 'BAD' ) THEN
         CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )
         CALL NDF_MTYPN( '_INTEGER,_REAL,_DOUBLE', 1, INDF,
     :                   MCOMP, ITYPE, DTYPE, STATUS )
         CALL NDF_MAP( INDF, MCOMP, ITYPE, 'READ', IP1, NEL, STATUS )

*  Select the boolean operator to use when checking array values for
*  inclusion in the MOC. Each value is compared to the appropriate bad
*  value ising this operator.
         IF( MODE .EQ. 'GOOD' ) THEN
            OPER = AST__NE
         ELSE
            OPER = AST__EQ
         END IF

*  If we are selecting regions using a quality expression, attempt to
*  locate any existing quality name information in the input NDF. If such
*  information is found, LOCS is returned holding a set of five HDS
*  locators which identify the NDF and the various components of the
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      ELSE IF( MODE .EQ. 'QUAL' ) THEN
         CALL IRQ_FIND( INDF, LOCS, XNAME, STATUS )

*  Get a syntactically correct quality expression from the environment.
         CALL IRQ_GETQX( 'QEXP', QEXP, STATUS )

*  Attempt to compile the quality expression. An IRQ identifier is
*  returned for the compiled expression if it compiles succesfully.
         CALL IRQ_COMP( LOCS, IRQ__QNREF, .FALSE., QEXP, UNDEF, NUNDEF,
     :                  ERRPNT, IDQ, STATUS )

*  Allocate an array of _INTEGERs that is the same shape and size as
*  the NDF, initialising them all to zero.
         CALL NDF_SIZE( INDF, NEL, STATUS )
         ITYPE = '_INTEGER'
         CALL PSX_CALLOC( NEL, ITYPE, IP1, STATUS )

*  Set pixels bad in this array if they do not have the specified quality.
         CALL IRQ_SBADI( IDQ, .FALSE., NEL, %VAL( CNF_PVAL(IP1) ),
     :                   ALLBAD, NOBAD, STATUS )

*  Indicate the good pixels in this array are to be included in the output
*  MOC.
         OPER = AST__NE

*  Release the quality name information in the input NDF.
         CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the identifier for the compiled quality expression.
         CALL IRQ_ANNUL( IDQ, STATUS )

*  Close down the IRQ identifier system.
         CALL IRQ_CLOSE( STATUS )

      END IF

*  Create an empty MOC.
      MOC = AST_MOC( ' ', STATUS );

*  Set the maximum and minimum resolution of the MOC. If no values were
*  supplied, retain the default values supplied by AST.
      IF( MAXRES .NE. VAL__BADR ) CALL AST_SETR( MOC, 'MaxRes', MAXRES,
     :                                           STATUS )
      IF( MINRES .NE. VAL__BADR ) CALL AST_SETR( MOC, 'MinRes', MINRES,
     :                                           STATUS )

*  Add the pixel array into the MOC.
      IF( ITYPE .EQ. '_INTEGER' ) THEN
         CALL AST_ADDPIXELMASKI( MOC, AST__OR, IWCS, VAL__BADI, OPER,
     :                           0, VAL__BADI, %VAL( CNF_PVAL(IP1) ),
     :                           DIMS, STATUS )

      ELSE IF( ITYPE .EQ. '_REAL' ) THEN
         CALL AST_ADDPIXELMASKR( MOC, AST__OR, IWCS, VAL__BADR, OPER,
     :                           0, VAL__BADR, %VAL( CNF_PVAL(IP1) ),
     :                           DIMS, STATUS )

      ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL AST_ADDPIXELMASKD( MOC, AST__OR, IWCS, VAL__BADD, OPER,
     :                           0, VAL__BADD, %VAL( CNF_PVAL(IP1) ),
     :                           DIMS, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', ITYPE )
         CALL ERR_REP( ' ', 'Unsupported data type: ^T.', STATUS );
      END IF


*  Create a FITS file if required.
      IF( FMT .EQ. 'FITS' ) THEN
         CALL PAR_GET0C( 'OUT', FILE, STATUS )
         IF( INDEX( FILE, '.' ) .EQ. 0 ) THEN
            NC = CHR_LEN( FILE )
            CALL CHR_APPND( '.fits', FILE, NC )
         END IF
         CALL ATL_MOCFT( MOC, FILE, STATUS )

*  Otherwise, create a text file.
      ELSE IF( FMT .EQ. 'JSON' ) THEN
         CALL ATL_CREAT( 'MOC-JSON:OUT', MOC, STATUS )
      ELSE IF( FMT .EQ. 'STRING' ) THEN
         CALL ATL_CREAT( 'MOC:OUT', MOC, STATUS )
      ELSE
         CALL ATL_CREAT( 'AST:OUT', MOC, STATUS )
      END IF

*  Free any temporary array.
      IF( MODE .EQ. 'QUAL' ) CALL PSX_FREE( IP1, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MOCGEN_ERR', 'MOCGEN: Failed to create a '//
     :                 'Multi-Order Coverage (MOC) file.', STATUS )
      END IF

      END
