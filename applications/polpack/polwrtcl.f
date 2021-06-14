      SUBROUTINE POLWRTCL( STATUS )
*+
*  Name:
*     POLWRTCL

*  Purpose:
*     Creates a text file holding the contents of a specified catalogue in
*     the form of a Tcl code fragment which can be used in a Tcl applications
*     such as GAIA.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLWRTCL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a description of a POLPACK catalogue which
*     can be used by the Tcl applications such as the GAIA polarimetry
*     toolbox. The description includes the bulk data. Rows that contain
*     any bad values are omitted from the output catalogue.
*
*     The desciption of the catalogue is written to an output text file
*     and takes the form of a Tcl code fragment which assigns values to
*     the following Tcl variables:
*
*      gotwcs_  : Set to 1 if RA/DEC columns are available, or 0 if
*                 not.
*      headings_: A Tcl list holding the column headings.
*      uses_    : A Tcl list holding the quantity stored in each column.
*                 These will chosen from X, Y, Z, RA, DEC, I, Q, U, V, DI,
*                 DQ, DU, DV, P, ANG, PI, DP, DANG, DPI, ID (or will be null
*                 if the quantity in the column is not known).
*      xlo_     : The minimum X pixel index value in the data
*      ylo_     : The minimum Y pixel index value in the data
*      zlo_     : The minimum Z column value in the data (only set
*                 if the catalogue has a Z column).
*      xhi_     : The maximum X pixel index value in the data
*      yhi_     : The maximum Y pixel index value in the data
*      zhi_     : The maximum Z column value in the data (only set
*                 if the catalogue has a Z column).
*      ncol_    : The number of columns in the catalogue
*      nrow_    : The number of good rows in the catalogue
*      data_    : A Tcl list of rows. Each row is itself a Tcl list of
*                 column values.
*      ra_      : A central RA value in h:m:s format (may be blank)
*      dec_     : A central DEC value in d:m:s format (may be blank)
*      xrefpix_ : The pixel offset to ra_/dec_ from the bottom-left corner of
*                 the bounding box.
*      yrefpix_ : The pixel offset to ra_/dec_ from the bottom-left corner of
*                 the bounding box.
*      nxpix_   : No of pixels in X in bounding box
*      nypix_   : No of pixels in Y in bounding box
*      secpix_  : An estimate of the pixel size in arcseconds
*      equinox_ : The equinox for the RA and DEC values in the file
*                 (e.g. "2000" - may be blank)
*      epoch_   : The epoch of observation for the RA and DEC values
*      fmts_    : A list of Tcl formats specifications, one for each column.
*                 Column values are formatted with this format.
*      hfmts_   : A list of Tcl formats specifications, one for each column.
*                 Column headings are formatted with this format.
*      zaunit_  : The units associated with the Z axis in the current
*                 Frame of the catalogues WCS FrameSet. Not written if
*                 the catalogue does not have a Z axis.
*      zcunit_  : The units associated with the Z column in the catalogue.
*                 Not written if the catalogue does not have a Z column.
*      refrot_  : The angle in degrees, from the Declination axis (FK5
*                 J2000) through the RA axis, to the referene direction.
*                 Assumed to be constant across the map.


*  Usage:
*     polwrtcl in out

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        The name of the input catalogue.
*        if none is provided.
*     OUT = LITERAL (Read)
*        The name of the output text file.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-2000 (DSB):
*        Original version.
*     13-FEB-2001 (DSB):
*        Modified to support 3D data.
*     2-MAR-2001 (DSB):
*        Added zcunit_ and zaunit_,
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     15-APR-2005 (PWD):
*        Parameterize use of backslashes to improve portability.
*     12-JUN-2017 (DSB):
*        Added the position angle of the reference direction ("refrot_") to the
*        output TCL file.
*     19-JUN-2017 (DSB):
*        Ensure double precision values are written out with an E exponent,
*        rather than a D exponent (TCL does not understand D exponents).
*     14-JUN-2021 (DSB):
*        Replace bad values in AST and PCA columns with zeros, so that
*        they do not prevent the row being written out.
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
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CAT_ERR'          ! CAT_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      CHARACTER CONTIN*1         ! The Tcl line continuation character
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( CONTIN = '\\' )

*  External References:
      DOUBLE PRECISION SLA_DBEAR

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MXCOL
      PARAMETER ( MXCOL = 30 )

      INTEGER MXBAT
      PARAMETER ( MXBAT = 40000 )

      CHARACTER SYSTEM*3
      PARAMETER ( SYSTEM = 'FK5' )

      CHARACTER EQUINOX*5
      PARAMETER ( EQUINOX = 'J2000' )

      DOUBLE PRECISION DEQNOX
      PARAMETER ( DEQNOX = 2000.0 )

      CHARACTER RDHFMT*5
      PARAMETER ( RDHFMT = '%-16s' )

      CHARACTER RDFMT*7
      PARAMETER ( RDFMT = '%-16.8g' )

      CHARACTER XYFMT*6
      PARAMETER ( XYFMT = '%-8.1f' )

      CHARACTER XYHFMT*4
      PARAMETER ( XYHFMT = '%-8s' )

      CHARACTER FMT*7
      PARAMETER ( FMT = '%-13.6g' )

      CHARACTER HFMT*5
      PARAMETER ( HFMT = '%-13s' )


*  Local Variables:
      CHARACTER BJ*1             ! Besselian or Julian epoch
      CHARACTER BUF*10           ! Text buffer
      CHARACTER COLNM*20         ! External column name
      CHARACTER DECCNM*20        ! Name of the DEC column
      CHARACTER EPOCH*50         ! Epoch specifier in i/p catalogue
      CHARACTER EQN*50           ! Equinox specifier in i/p catalogue
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER FNAME*80
      CHARACTER HEAD( MXCOL + 2 )*15! Column names within the output catalogue
      CHARACTER IDCNM*20         ! Name of the ID column
      CHARACTER QUANT*10         ! Name of the column quantity
      CHARACTER RACNM*20         ! Name of the RA column
      CHARACTER TEXT*512         ! O/p text buffer
      CHARACTER XCNM*20          ! Name of the X column
      CHARACTER YCNM*20          ! Name of the Y column
      CHARACTER ZAUNIT*20         ! Units for Z axis
      CHARACTER ZCNM*20          ! Name of the Z column
      CHARACTER ZCUNIT*20        ! Units for Z column
      DOUBLE PRECISION DEQN      ! Input equinox
      DOUBLE PRECISION P1( 3 )   ! PIXEL coords at pixel origin
      DOUBLE PRECISION P2( 3 )   ! POLANAL coords at pixel origin
      DOUBLE PRECISION P3( 3 )   ! (RA,Dec) coords at pixel origin
      DOUBLE PRECISION P4( 3 )   ! (RA,Dec) coords at northern position
      DOUBLE PRECISION P5( 3 )   ! POLANAL coords at northern position
      DOUBLE PRECISION P6( 3 )   ! (RA,Dec) coords at projected northern position
      DOUBLE PRECISION REFROT    ! Angle form north to ref direction
      INTEGER BFRM               ! Base Frame from input WCS FrameSet
      INTEGER CATFRM             ! Frame describe catalogue RA/Dec columns
      INTEGER CI                 ! CAT identifier for input catalogue
      INTEGER DECCOL             ! Index of DEC column within output catalogue
      INTEGER FD                 ! FIO identifier for output file
      INTEGER FS                 ! An AST FrameSet pointer
      INTEGER GA( 3 )            ! CAT id.s for i/p cols giving o/p RA/DEC
      INTEGER GCOL( MXCOL + 2 )  ! CAT id.s for o/p columns within i/p catalogue
      INTEGER GI                 ! A CAT component identifier
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of TEXT
      INTEGER IBASE              ! Original index of WCS Base Frame
      INTEGER ICURR              ! Original index of WCS Current Frame
      INTEGER ICOL               ! Column index
      INTEGER IDAST              ! Column index of AST column
      INTEGER IDCOL              ! Column index of ID column
      INTEGER IDLEN              ! Length of identifier strings
      INTEGER IDPCA              ! Column index of PCA column
      INTEGER IDTYPE             ! Data type of ID column
      INTEGER IFRMPA             ! Index of POLANAL Frame
      INTEGER IFRMPX             ! Index of PIXEL Frame
      INTEGER IFRM               ! Index of SkyFrame
      INTEGER IPW1               ! Pointer to work space
      INTEGER IPW2               ! Pointer to work space
      INTEGER IPW3               ! Pointer to work space
      INTEGER IPW4               ! Pointer to work space
      INTEGER ITEMP              ! Temporary storage
      INTEGER IWCS               ! The WCS FrameSet from the input catalogue
      INTEGER MAP                ! AST Mapping used to create new RA/DEC values
      INTEGER NAXB               ! No. of base frame axes
      INTEGER NAXC               ! No. of current frame axes
      INTEGER NCIN               ! No. of columns in input catalogue
      INTEGER NCOL               ! No. of columns in output catalogue
      INTEGER NDIM               ! No. of dimensions in WCS Base Frame
      INTEGER NROW               ! No. of rows in input catalogue
      INTEGER NROWGD             ! No. of rows in output catalogue
      INTEGER NZC                ! Used length of array ZERCOL
      INTEGER PXAMAP             ! Mapping from PIXEL to POLANAL
      INTEGER RACOL              ! Index of RA column within output catalogue
      INTEGER SKYFRM             ! An AST SkyFrame pointer
      INTEGER SZBAT              ! Size of each batch
      INTEGER XCOL               ! Index of X column within output catalogue
      INTEGER YCOL               ! Index of Y column within output catalogue
      INTEGER ZCOL               ! Index of Z column within output catalogue
      INTEGER ZERCOL( MXCOL )    ! Column indices for which bad->zero
      LOGICAL GOTRD              ! Will o/p catalogue have RA and DEC columns?
      LOGICAL MAKERD             ! Will we be creating new RA/DEC o/p columns?
      LOGICAL NULL               ! Is the stored value null?
      LOGICAL VERB               ! Verose errors required?
      REAL LBND( 3 )             ! Lower bounds of X/Y/Z bounding box
      REAL UBND( 3 )             ! Upper bounds of X/Y/Z bounding box

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CI, FIELDS, STATUS )

*  Get the number of rows in the catalogue.
      CALL CAT_TROWS( CI, NROW, STATUS )

*  Get the number of columns in the supplied catalogue.
      CALL CAT_TCOLS( CI, CAT__GPHYS, NCIN, STATUS )
      IF( NCIN .GT. MXCOL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NCIN )
         CALL MSG_SETI( 'M', MXCOL )
         CALL ERR_REP( 'POLWRTCL_ERR1', 'Too many columns (^N) in '//
     :                 'catalogue ''$IN''. No more than ^M are '//
     :                 'allowed.', STATUS )
      END IF

*  Get the names of the X, Y, Z, Ra, Dec and ID columns in the input catalogue.
      CALL POL1_COLNM( 'X', .FALSE., XCNM, STATUS )
      CALL POL1_COLNM( 'Y', .FALSE., YCNM, STATUS )
      CALL POL1_COLNM( 'Z', .FALSE., ZCNM, STATUS )
      CALL POL1_COLNM( 'RA', .FALSE., RACNM, STATUS )
      CALL POL1_COLNM( 'DEC', .FALSE., DECCNM, STATUS )
      CALL POL1_COLNM( 'ID', .FALSE., IDCNM, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get a list of column identifiers and headings from the input catalogue.
*  Note, the indices of the X, Y, RA, DEC and ID columns.
      XCOL = 0
      YCOL = 0
      ZCOL = 0
      RACOL = 0
      DECCOL = 0
      IDCOL = 0
      IDAST = 0
      IDPCA = 0

      DO ICOL = 1, NCIN

         CALL CAT_TNDNT( CI, CAT__FITYP, ICOL, GCOL( ICOL ), STATUS  )
         CALL CAT_TIQAC( GCOL( ICOL ), 'NAME', HEAD( ICOL ), STATUS )

         IF( HEAD( ICOL ) .EQ. XCNM ) THEN
            XCOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. YCNM ) THEN
            YCOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. ZCNM ) THEN
            ZCOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. RACNM ) THEN
            RACOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. DECCNM ) THEN
            DECCOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. IDCNM ) THEN
            IDCOL = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. "AST" ) THEN
            IDAST = ICOL

         ELSE IF( HEAD( ICOL ) .EQ. "PCA" ) THEN
            IDPCA = ICOL

         END IF

      END DO

*  If the input catalogue has no ID column, we need to add one.
      IF( IDCOL .EQ. 0 ) THEN
         NCOL = NCIN + 1
         IDCOL = NCOL
         GCOL( IDCOL ) = CAT__NOID
         HEAD( IDCOL ) = IDCNM
         CALL CHR_ITOC( NROW, BUF, IDLEN )
      ELSE
         NCOL = NCIN
         CALL CAT_TIQAI( GCOL( IDCOL ), 'DTYPE', IDTYPE, STATUS )
         IF( IDTYPE .EQ. CAT__TYPEC ) THEN
            CALL CAT_TIQAI( GCOL( IDCOL ), 'CSIZE', IDLEN, STATUS )
         ELSE
            IDLEN = 8
         END IF

      END IF

*  Construct a list of columns that are to have bad values replaced by
*  zero.
      NZC = 0
      IF( IDAST .NE. 0 ) THEN
         NZC = NZC + 1
         ZERCOL( NZC ) = IDAST
      END IF
      IF( IDPCA .NE. 0 ) THEN
         NZC = NZC + 1
         ZERCOL( NZC ) = IDPCA
      END IF

*  Abort if no X or Y column was found.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF( XCOL .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', XCNM )
            CALL ERR_REP( 'POLWRTCL_ERR2', 'Input catalogue ''$IN'' '//
     :                    'has no ''^C'' column.', STATUS )
            GO TO 999

         ELSE IF( YCOL .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', YCNM )
            CALL ERR_REP( 'POLWRTCL_ERR3', 'Input catalogue ''$IN'' '//
     :                    'has no ''^C'' column.', STATUS )
            GO TO 999
         END IF
      END IF

*  Re-arrange them so that X become column 1 in the output catalogue.
      ITEMP = GCOL( XCOL )
      DO ICOL = XCOL, 2, -1
         HEAD( ICOL ) = HEAD( ICOL - 1 )
         GCOL( ICOL ) = GCOL( ICOL - 1 )
      END DO
      HEAD( 1 ) = XCNM
      GCOL( 1 ) = ITEMP

      IF( YCOL .LT. XCOL ) YCOL = YCOL + 1
      IF( RACOL .GT. 0 .AND. RACOL .LT. XCOL ) RACOL = RACOL + 1
      IF( DECCOL .GT. 0 .AND. DECCOL .LT. XCOL ) DECCOL = DECCOL + 1
      IF( ZCOL .GT. 0 .AND. ZCOL .LT. XCOL ) ZCOL = ZCOL + 1
      IF( IDCOL .LT. XCOL ) IDCOL = IDCOL + 1
      XCOL = 1

*  Re-arrange them so that Y become column 2 in the output catalogue.
      ITEMP = GCOL( YCOL )
      DO ICOL = YCOL, 3, -1
         HEAD( ICOL ) = HEAD( ICOL - 1 )
         GCOL( ICOL ) = GCOL( ICOL - 1 )
      END DO
      HEAD( 2 ) = YCNM
      GCOL( 2 ) = ITEMP

      IF( RACOL .GT. 0 .AND. RACOL .LT. YCOL ) RACOL = RACOL + 1
      IF( DECCOL .GT. 0 .AND. DECCOL .LT. YCOL ) DECCOL = DECCOL + 1
      IF( ZCOL .GT. 0 .AND. ZCOL .LT. YCOL ) ZCOL = ZCOL + 1
      IF( IDCOL .LT. YCOL ) IDCOL = IDCOL + 1
      YCOL = 2

*  Get a WCS FrameSet from the catalogue with either 2 or 3 base Frame
*  axes (depending on whether a Z column is present in the catalogue or not).
      GA( 1 ) = GCOL( XCOL )
      GA( 2 ) = GCOL( YCOL )
      IF( ZCOL .GT. 0 ) THEN
         GA( 3 ) = GCOL( ZCOL )
         NDIM = 3
      ELSE
         NDIM = 2
      END IF
      CALL POL1_GTCTA( CI, NDIM, GA, IWCS, STATUS )

*  Get the number of axes in the base and current Frames.
      NAXB = AST_GETI( IWCS, 'NIN', STATUS )
      NAXC = AST_GETI( IWCS, 'NOUT', STATUS )

*  If a Z column is present, get the units strings from the 3rd axis in
*  both base and current Frames.
      IF( ZCOL .GT. 0 ) THEN

         IF( NAXC .GT. 2 ) THEN
            ZAUNIT = AST_GETC( AST_GETFRAME( IWCS, AST__CURRENT,
     :                                       STATUS ),
     :                         'UNIT(3)', STATUS )
         ELSE
            ZAUNIT = ' '
         END IF

         IF( NAXB .GT. 2 ) THEN
            ZCUNIT = AST_GETC( AST_GETFRAME( IWCS, AST__BASE, STATUS ),
     :                         'UNIT(3)', STATUS )
         ELSE
            ZCUNIT = ' '
         END IF

      END IF

*  Record the index of the original current and Base Frames in the WCS
*  FrameSet.
      IBASE = AST_GETI( IWCS, 'Base', STATUS )
      ICURR = AST_GETI( IWCS, 'Current', STATUS )

*  Find the indices of the POLANAL and PIXEL Frames in the WCA FrameSet.
      CALL KPG1_ASFFR( IWCS, 'POLANAL', IFRMPA, STATUS )
      IF( IFRMPA .NE. AST__NOFRAME ) THEN
         CALL AST_SETI( IWCS, 'Current', IFRMPA, STATUS )
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Cannot find a POLANAL Frame in the WCS '//
     :                 'FrameSet of catalogue ''$IN''. The reference '//
     :                 'direction is undefined.',  STATUS )
      END IF

      CALL KPG1_ASFFR( IWCS, 'PIXEL', IFRMPX, STATUS )
      IF( IFRMPX .NE. AST__NOFRAME ) THEN
         CALL AST_SETI( IWCS, 'Current', IFRMPX, STATUS )
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Cannot find a PIXEL Frame in the WCS '//
     :                 'FrameSet of catalogue ''$IN''.', STATUS )
      END IF

*  Get the Mapping from PIXEL to POLANAL.
      PXAMAP = AST_GETMAPPING( IWCS, IFRMPA, IFRMPX, STATUS )

*  Create a SkyFrame describing the system of the RA/Dec columns in the
*  output catalogue.
      CATFRM = AST_SKYFRAME( 'System='//SYSTEM//',Equinox='//EQUINOX,
     :                       STATUS )

*  Find a FrameSet that converts from POLANAL to RA/Dec.
      CALL AST_SETI( IWCS, 'Current', IFRMPA, STATUS )
      FS = AST_CONVERT( IWCS, CATFRM, 'SKY', STATUS )
      IF( FS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Cannot find Mapping from POLANAL Frame '//
     :                 'to RA/De in catalogue ''$IN''.',  STATUS )

*  Use this FrameSet to find the angle from the Dec axis to the reference
*  direction, in the sense of rotation from Dec to RA.
      ELSE

*  Get the POLANAL positions corresponding to the pixel origin.
         DO I = 1, NAXB
            P1( I ) = 0.0D0
         END DO
         CALL AST_TRANN( PXAMAP, 1, NAXB, 1, P1, .TRUE., NAXB, 1, P2,
     :                   STATUS )

*  Convert this position to (RA,Dec)
         CALL AST_TRANN( FS, 1, NAXB, 1, P2, .TRUE., 2, 1, P3,
     :                   STATUS )

*  Get the POLANAL coords of a position 1 arc-min to the north of the
*  pixel origin.
         P4( 1 ) = P3( 1 )
         P4( 2 ) = P3( 2 ) + AST__DD2R/60.0D0
         P4( 3 ) = P3( 3 )
         CALL AST_TRANN( FS, 1, 2, 1, P4, .FALSE., NAXB, 1, P5,
     :                   STATUS )

*  The first POLANAL axis defines the reference direction. Set the values
*  on the other POLANAL axes back to their original values.
         P5( 2 ) = P2( 2 )
         P5( 3 ) = P2( 3 )

*  Convert this position to (RA,Dec).
         CALL AST_TRANN( FS, 1, NAXB, 1, P5, .TRUE., 2, 1, P6,
     :                   STATUS )

*  Get the position angle of P6 as seen from P3. This is the angle from Dec to the
*  reference direction (the first polanal axis), going through RA.
         REFROT = SLA_DBEAR( P3(1), P3(2), P6(1), P6(2) )*AST__DR2D
      END IF

*  Re-instate the original Base and current Frames in the WCS FrameSet.
      CALL AST_SETI( IWCS, 'Base', IBASE, STATUS )
      CALL AST_SETI( IWCS, 'Current', ICURR, STATUS )

*  Assume for the moment that the output catalogue will contain RA/DEC
*  columns.
      GOTRD = .TRUE.

*  Assume for the moment that we do not need to create RA DEC columns
*  because usable ones already exist in the catalogue.
      MAKERD = .FALSE.

*  Assume no epoch available.
      EPOCH = ' '

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the catalogue contains columns holding RA/DEC coordinates...
      IF( RACOL .GT. 0 .AND. DECCOL .GT. 0 ) THEN

*  See if the catalogue contains an EPOCH parameter.
         CALL CAT_TIDNT( CI, 'EPOCH', GI, STATUS )
         IF( STATUS .EQ. CAT__NOCMP ) THEN
            CALL ERR_ANNUL( STATUS )
            EPOCH = ' '
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_EGT0F( GI, EPOCH, NULL, STATUS )
            CALL CAT_TRLSE( GI, STATUS )
         END IF

*  Get their equinox from the catalogue EQUINOX parameter. Assume default if
*  no value is available.
         CALL CAT_TIDNT( CI, 'EQUINOX', GI, STATUS )
         IF( STATUS .EQ. CAT__NOCMP ) THEN
            CALL ERR_ANNUL( STATUS )

*  If available, get its formatted value.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_EGT0F( GI, EQN, NULL, STATUS )
            CALL CAT_TRLSE( GI, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  If null, continue with the assumption of the default.
            IF( .NOT. NULL ) THEN

*  Extract any B/J specifier in the first character.
               BJ = EQN( 1:1 )
               CALL CHR_UCASE( BJ )
               IF( BJ .EQ. 'B' .OR. BJ .EQ. 'J' ) THEN
                  EQN( 1:1 ) = ' '
               ELSE
                  BJ = ' '
               END IF

*  Extract the numerical value.
               CALL CHR_CTOD( EQN, DEQN, STATUS )

*  Assume default if the numerical EQUNOX string was bad.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'EQ', EQUINOX )
                  CALL ERR_REP( 'POLWRTCL_ERR3', 'Bad EQUINOX value '//
     :                          '''^E'' found in catalogue ''$IN''. '//
     :                          'Assuming a value of ^EQ.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*  Otherwise...
               ELSE

*  If no B/J specifier was included, use the 1984 rule.
                  IF( BJ .EQ. ' ' ) THEN
                     IF( DEQN .LT. 1984.0 ) THEN
                        BJ = 'B'
                     ELSE
                        BJ = 'J'
                     END IF
                  END IF

*  The existing RA/DEC values are only directly usable if they have the
*  required equinox.  Otherwise we create new RA/DEC columns by mapping the
*  existing RA/DEC columns. Set flags to indicate this.
                  IF( BJ .NE. 'J' .OR. DEQN .NE. DEQNOX ) THEN
                     MAKERD = .TRUE.

*  Save identifiers for the columns within the input catalogue from which
*  the new RA and DEC columns will be derived.
                     GA( 1 ) = GCOL( RACOL )
                     GA( 2 ) = GCOL( DECCOL )
                     NDIM = 2

*  Create a default AST SkyFrame.
                     SKYFRM = AST_SKYFRAME( ' ', STATUS )

*  Assume supplied RA/DEC values are FK5 if a Julian equinox was supplied.
*  FK4 otherwise.
                     IF( BJ .EQ. 'J' ) THEN
                        CALL AST_SETC( SKYFRM, 'SYSTEM', 'FK5', STATUS )
                     ELSE
                        CALL AST_SETC( SKYFRM, 'SYSTEM', 'FK5', STATUS )
                     END IF

*  Set the equinox of the SkyFrame.
                     CALL AST_SETC( SKYFRM, 'EQUINOX', EQN, STATUS )

*  If the catalogue contains an EPOCH parameter, copy its value to the
*  SkyFrame. Otherwise, retrieve the default epoch value.
                     IF( EPOCH .NE. ' ' ) THEN
                        CALL AST_SETC( SKYFRM, 'EPOCH', EPOCH, STATUS )
                     ELSE
                        EPOCH = AST_GETC( SKYFRM, 'EPOCH', STATUS )
                     END IF

*  Find a Mapping from the supplied system to the required system.
                     FS = AST_FINDFRAME( SKYFRM, CATFRM, ' ', STATUS )
                     MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT,
     :                                     STATUS )

                  END IF
               END IF
            END IF
         END IF

*  If the catalogue does not contain RA/DEC columns...
      ELSE

*  Assume for the moment that the output catalogue will not contain
*  RA/DEC columns.
         GOTRD = .FALSE.

*  See if the WCS FrameSet contains a SKY Frame. If the input has more than 2
*  axes this call will look for a SkyFrame within the CmpFrame and append a
*  copy of it to the end of the FrameSet so that the following call to
*  AST_CONVERT will be able to use it.
         CALL KPG1_ASFFR( IWCS, 'SKY', IFRM, STATUS )

*  If the FrameSet passed the above tests...
         IF( IFRM .NE. AST__NOFRAME ) THEN

*  Attempt to get a FrameSet connecting X/Y(Z) to RA/DEC.
            CALL AST_SETI( IWCS, 'CURRENT',
     :                     AST_GETI( IWCS, 'BASE', STATUS ),
     :                     STATUS )
            FS = AST_CONVERT( IWCS, CATFRM, ' ', STATUS )

*  If succesfull...
            IF( FS .NE. AST__NULL ) THEN

*  Get the epoch of the SkyFrame.
               EPOCH = AST_GETC( FS, 'EPOCH', STATUS )

*  Get the Mapping from X/Y(/Z) to RA/DEC.
               MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT,
     :                               STATUS )

*  Set the flags to indicate that new RA/DEC columns should be created on
*  the basis of existing X/Y(/Z) columns.
               MAKERD = .TRUE.
               GOTRD = .TRUE.

*  Append RA DEC columns to the column headings arrays.
               HEAD( NCOL + 1 ) = RACNM
               GCOL( NCOL + 1 ) = CAT__NOID
               RACOL = NCOL + 1

               HEAD( NCOL + 2 ) = DECCNM
               GCOL( NCOL + 2 ) = CAT__NOID
               DECCOL = NCOL + 2

               NCOL = NCOL + 2
            END IF

         END IF

      END IF

*  If the output catalogue will contain RA and DEC columns, re-arrange the
*  column headings so that RA and DEC are columns 3 and 4.
      IF( GOTRD ) THEN
         ITEMP = GCOL( RACOL )
         DO ICOL = RACOL, 4, -1
            HEAD( ICOL ) = HEAD( ICOL - 1 )
            GCOL( ICOL ) = GCOL( ICOL - 1 )
         END DO
         HEAD( 3 ) = RACNM
         GCOL( 3 ) = ITEMP

         IF( DECCOL .LT. RACOL ) DECCOL = DECCOL + 1
         IF( IDCOL .LT. RACOL ) IDCOL = IDCOL + 1
         IF( ZCOL .GT. 0 .AND. ZCOL .LT. RACOL ) ZCOL = ZCOL + 1
         RACOL = 3

         ITEMP = GCOL( DECCOL )
         DO ICOL = DECCOL, 5, -1
            HEAD( ICOL ) = HEAD( ICOL - 1 )
            GCOL( ICOL ) = GCOL( ICOL - 1 )
         END DO
         HEAD( 4 ) = DECCNM
         GCOL( 4 ) = ITEMP

         IF( IDCOL .LT. DECCOL ) IDCOL = IDCOL + 1
         IF( ZCOL .GT. 0 .AND. ZCOL .LT. DECCOL ) ZCOL = ZCOL + 1
         DECCOL = 4

      END IF

*  Open the output text file.
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'LIST', 256, FD, STATUS )
      CALL FIO_FNAME( FD, FNAME, STATUS )
      CALL MSG_SETC( 'F', FNAME )

*  Write a flag to the output text file indicating if RA/DEC values
*  are available.
      IF( GOTRD ) THEN
         CALL FIO_WRITE( FD, 'set gotwcs_ 1', STATUS )
      ELSE
         CALL FIO_WRITE( FD, 'set gotwcs_ 0', STATUS )
      END IF

*  Write a list of the column headings out to the text file.
      CALL FIO_WRITE( FD, 'set headings_ { '//CONTIN, STATUS )

      DO ICOL = 1, NCOL
         TEXT = '   '
         IAT = 3
         CALL CHR_APPND( HEAD( ICOL ), TEXT, IAT )
         CALL CHR_APPND( ' '//CONTIN, TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      END DO

      CALL FIO_WRITE( FD, ' }', STATUS )

*  Write a list of the quantities stored in each column out to the text file.
      CALL FIO_WRITE( FD, 'set uses_ { '//CONTIN, STATUS )

      DO ICOL = 1, NCOL
         TEXT = '   '
         IAT = 3
         CALL POL1_COLNM( HEAD( ICOL ), .TRUE., QUANT, STATUS )
         IF( QUANT .NE. ' ' ) THEN
            CALL CHR_APPND( QUANT, TEXT, IAT )
         ELSE
            CALL CHR_APPND( '""', TEXT, IAT )
         END IF
         CALL CHR_APPND( ' '//CONTIN, TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      END DO

      CALL FIO_WRITE( FD, ' }', STATUS )

*  Write a list of the heading formats out to the text file.
      CALL FIO_WRITE( FD, 'set hfmts_ { '//CONTIN, STATUS )

      DO ICOL = 1, NCOL
         TEXT = '   '
         IAT = 3
         IF( ICOL .EQ. XCOL .OR. ICOL .EQ. YCOL ) THEN
            CALL CHR_APPND( XYHFMT, TEXT, IAT )
         ELSE IF( ICOL .EQ. RACOL .OR. ICOL .EQ. DECCOL ) THEN
            CALL CHR_APPND( RDHFMT, TEXT, IAT )
         ELSE
            CALL CHR_APPND( HFMT, TEXT, IAT )
         END IF
         CALL CHR_APPND( ' '//CONTIN, TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      END DO

      CALL FIO_WRITE( FD, '}', STATUS )

*  Write a list of the column formats out to the text file.
      CALL FIO_WRITE( FD, 'set fmts_ { '//CONTIN, STATUS )

      DO ICOL = 1, NCOL
         TEXT = '   '
         IAT = 3
         IF( ICOL .EQ. XCOL .OR. ICOL .EQ. YCOL ) THEN
            CALL CHR_APPND( XYFMT, TEXT, IAT )
         ELSE IF( ICOL .EQ. RACOL .OR. ICOL .EQ. DECCOL ) THEN
            CALL CHR_APPND( RDFMT, TEXT, IAT )
         ELSE IF( ICOL .EQ. IDCOL ) THEN
            IF( IDTYPE .EQ. CAT__TYPEC ) THEN
               CALL CHR_APPND( '%-', TEXT, IAT )
               CALL CHR_PUTI( IDLEN + 1, TEXT, IAT )
               CALL CHR_APPND( 's', TEXT, IAT )
            ELSE
               CALL CHR_APPND( FMT, TEXT, IAT )
            END IF
         ELSE
            CALL CHR_APPND( FMT, TEXT, IAT )
         END IF
         CALL CHR_APPND( ' '//CONTIN, TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      END DO

      CALL FIO_WRITE( FD, '}', STATUS )

*  Write out the equinox.
      TEXT = 'set equinox_ '
      IAT = 13
      CALL POL1_PUTD( DEQNOX, TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  Write out the epoch.
      TEXT = 'set epoch_ '
      IAT = 12
      IF( EPOCH .NE. ' ' ) THEN
         CALL CHR_APPND( EPOCH, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '""', TEXT, IAT )
      END IF
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  Determine the size of each batch.
      SZBAT = MIN( NROW, MXBAT )

*  Allocate work space.
      CALL PSX_CALLOC( SZBAT*NDIM, '_DOUBLE', IPW1, STATUS )
      CALL PSX_CALLOC( SZBAT*2, '_DOUBLE', IPW2, STATUS )
      IF( GOTRD ) THEN
         CALL PSX_CALLOC( SZBAT*( NCOL - 4 ), '_REAL', IPW3, STATUS )
      ELSE
         CALL PSX_CALLOC( SZBAT*( NCOL - 2 ), '_REAL', IPW3, STATUS )
      END IF
      CALL PSX_CALLOC( SZBAT*IDLEN, '_CHAR', IPW4, STATUS )

*  Write values to the output file.
      CALL POL1_WRTCL( CI, GOTRD, MAKERD, NDIM, GA, MAP, NCOL, GCOL,
     :                 NROW, IDCOL, ZCOL, FD, SZBAT, NZC, ZERCOL, LBND,
     :                 UBND, %VAL( CNF_PVAL( IPW1 ) ),
     :                 %VAL( CNF_PVAL( IPW2 ) ),
     :                 %VAL( CNF_PVAL( IPW3 ) ),
     :                 %VAL( CNF_PVAL( IPW4 ) ), NROWGD, STATUS,
     :                 %VAL( CNF_CVAL( IDLEN ) ) )

*  Write out the number of good rows and columns. Add one on for the ID column
*  added by this program.
      TEXT = 'set nrow_ '
      IAT = 10
      CALL CHR_PUTI( NROWGD, TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      TEXT = 'set ncol_ '
      IAT = 10
      CALL CHR_PUTI( NCOL, TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  Write out the position angle of the reference direction within (RA,Dec).
      TEXT = 'set refrot_ '
      IAT = 12
      CALL POL1_PUTD( REFROT, TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  Free the work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPW3, STATUS )
      CALL PSX_FREE( IPW4, STATUS )

*  Write the bounding box out to the output text file.
      TEXT = 'set xlo_ '
      IAT = 9
      CALL CHR_PUTI( NINT( 0.5 + LBND( 1 ) ), TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      TEXT = 'set xhi_ '
      IAT = 9
      CALL CHR_PUTI( NINT( 0.5 + UBND( 1 ) ), TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      TEXT = 'set ylo_ '
      IAT = 9
      CALL CHR_PUTI( NINT( 0.5 + LBND( 2 ) ), TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      TEXT = 'set yhi_ '
      IAT = 9
      CALL CHR_PUTI( NINT( 0.5 + UBND( 2 ) ), TEXT, IAT )
      CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      IF( ZCOL .GT. 0 ) THEN
         TEXT = 'set zlo_ '
         IAT = 9
         CALL CHR_PUTR( LBND( 3 ), TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

         TEXT = 'set zhi_ '
         IAT = 9
         CALL CHR_PUTR( UBND( 3 ), TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

         TEXT = 'set zaunit_ "'
         IAT = 13
         CALL CHR_APPND( ZAUNIT, TEXT, IAT )
         CALL CHR_APPND( '"', TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

         TEXT = 'set zcunit_ "'
         IAT = 13
         CALL CHR_APPND( ZCUNIT, TEXT, IAT )
         CALL CHR_APPND( '"', TEXT, IAT )
         CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Close the output text file.
      CALL FIO_ANNUL( FD, STATUS )

*  Close the input catalogue.
      CALL CAT_TRLSE( CI, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLWRTCL_ERR', 'POLWRTCL: Error producing a '//
     :                 'Tcl description of a polarization catalogue.',
     :                 STATUS )
      END IF

      END
