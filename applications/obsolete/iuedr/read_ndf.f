      SUBROUTINE READ_NDF( SUBCMD, NAME, N, IMDATA, IMQUAL, STATUS )
*+
*  Name:
*     SUBROUTINE READ_NDF

*  Purpose:
*     Write IUEDR data to Starlink NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL READ_NDF( SUBCMD, NAME, N, X, Y, STATUS )

*  Arguments:
*     SUBCMD = CHARACTER* ( * ) (Given)
*        Type of data to be read:
*           'IMAGE'     IUE Image data from tape/file.
*           'SPECTRUM'  Net Spectrum/spectra.
*           'MSPECTRUM' Mean Spectrum.
*     NAME = CHARACTER* ( * ) (Given)
*        Name of the file to be read.
*     N = INTEGER (Returned)
*        Number of data points read.
*     IMDATA = INTEGER*2( N ) (Given and Returned)
*        Image data array for 'IMAGE' command, ignored otherwise.
*     IMQUAL = BYTE( N ) (Given and Returned)
*        Image quality array for 'IMAGE' command, ignored otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (DMILLS):
*       IUEDR Vn. 3.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CHR_ERR'

*  Global Variables:
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMNDF'

*  Local Constants:
      INTEGER MAXLABEL       ! Maximum length of label.
      INTEGER MAXTITLE       ! Maximum length of title.
      PARAMETER ( MAXLABEL = 40, MAXTITLE = 80 )

*  Arguments Given:
      CHARACTER*( * ) SUBCMD
      CHARACTER*( * ) NAME

*  Arguments Returned:
      INTEGER N

*  Arguments Given and Returned:
      REAL*8 IMDATA( N )
      REAL*8 IMQUAL( N )

*  Status:
      INTEGER STATUS   ! Global status.

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER*80 TITLE
      CHARACTER*40 LSTRING
      CHARACTER*40 L2STRING

      INTEGER IMDIMS( 2 )
      INTEGER AXPNTR
      INTEGER AX2PNTR
      INTEGER DATPNTR
      INTEGER IOS
      INTEGER NDIM
      INTEGER NDUM
      INTEGER NORD
      INTEGER SLEN
      INTEGER QUALPNTR
      INTEGER WPNTR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IOS = SAI__OK

      IMDIMS( 1 ) = 768
      IMDIMS( 2 ) = 768

      CALL HDS_OPEN( NAME, 'READ', MAINLOC, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL NDF_BEGIN

      IF ( SUBCMD .EQ. 'IMAGE' ) THEN
         CALL NDF_IMPRT( MAINLOC, IAMNDF, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'DATA', '_WORD', 'READ', DATPNTR, N,
     :                 IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'QUALITY', '_UBYTE', 'READ', QUALPNTR,
     :                 N, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         CALL MOVE_WORD( N, %VAL( DATPNTR ), IMDATA )
         CALL MOVE_BYTE( N, %VAL( QUALPNTR ), IMQUAL )

         CALL NDF_UNMAP( IAMNDF, 'DATA,QUALITY', IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_END( IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL HDS_CLOSE( MAINLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         GO TO 999

      ELSE
         CALL NDF_IMPRT( MAINLOC, IAMNDF, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'DATA', '_REAL', 'READ', DATPNTR, N,
     :                 IOS )
         IF ( SUBCMD.EQ.'SPECTRUM' .OR. SUBCMD.EQ.'MSPECTRUM') THEN
            CALL NDF_MAP( IAMNDF, 'QUALITY', '_UBYTE', 'READ',
     :                    QUALPNTR, N, IOS )
         END IF
      END IF

      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL NDF_CGET( IAMNDF, 'TITLE', TITLE, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
         CALL GEN_CTOS( TITLE, MAXTITLE, MTITLE, NDUM )
         SLEN = CHR_LEN( TITLE )
         CALL STR_TERM( SLEN, MAXTITLE, MTITLE )
      END IF

      IF ( SUBCMD .EQ. 'SPECTRUM' .OR. SUBCMD.EQ.'MSPECTRUM' ) THEN
         CALL NDF_XLOC( IAMNDF, 'IUEDR_EXTRA', 'READ', IUEDRLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         IF ( SUBCMD .EQ. 'SPECTRUM' ) THEN
            CALL CMP_SIZE( IUEDRLOC, 'ORDERS', NORD, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_GETVI( IUEDRLOC, 'ORDERS', NORD, ORDERS, NORD,
     :                      IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_GETVI( IUEDRLOC, 'NWAVS', NORD, NWAVS, NORD, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

         ELSE IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
            CALL CMP_GET0D( IUEDRLOC, 'XCOMB1', XCOMB1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_GET0D( IUEDRLOC, 'DXCOMB', DXCOMB, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL NDF_CGET( IAMNDF, 'LABEL', L2STRING, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL GEN_CTOS( L2STRING, MAXLABEL, YMLAB, NDUM )
            SLEN = CHR_LEN( L2STRING )
            CALL STR_TERM( SLEN, MAXLABEL, YMLAB )

            CALL NDF_CGET( IAMNDF, 'UNITS', LSTRING, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL GEN_CTOS( LSTRING, MAXLABEL, YMUN, NDUM )
            SLEN = CHR_LEN( LSTRING )
            CALL STR_TERM( SLEN, MAXLABEL, YMUN )
         END IF
      END IF

      CALL CMP_SHAPE( IUEDRLOC, 'WAVES', 2, IMDIMS, NDIM, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL CMP_MAPN( IUEDRLOC, 'WAVES', '_REAL', 'READ', NDIM, AXPNTR,
     :               IMDIMS, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      IF ( SUBCMD .EQ. 'SPECTRUM' ) THEN
         CALL NDF_DIM( IAMNDF, 2, IMDIMS, NDIM, IOS )
         N = IMDIMS( 1 )
         NORD = IMDIMS( 2 )
         CALL READ_SPEC( N, NORD, %VAL( DATPNTR ), %VAL( QUALPNTR ),
     :                   %VAL( AXPNTR ) )
         NORDER =  NORD
         IF ( NORD .EQ. 1 ) THEN
            NWAVS( 1 ) = N
         END IF

      ELSE IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
         CALL NDF_DIM( IAMNDF, 1, IMDIMS, NDIM, IOS )
         N = IMDIMS( 1 )
         NORD = 1
         CALL CMP_MAPN( IUEDRLOC, 'WEIGHTS', '_REAL', 'READ', NDIM,
     :                  WPNTR, IMDIMS, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL READ_MSPEC( N, %VAL( DATPNTR ), %VAL( QUALPNTR ),
     :                    %VAL( AXPNTR ), %VAL( WPNTR ) )
         NCOMB = N
      END IF

      CALL NDF_AMAP( IAMNDF, 'CENTRE', 1, '_REAL', 'READ', AX2PNTR, N,
     :               IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL NDF_ACGET( IAMNDF, 'LABEL', 1, LSTRING, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL GEN_CTOS( LSTRING, MAXLABEL, XMLAB, NDUM )
      SLEN = CHR_LEN( LSTRING )
      CALL STR_TERM( SLEN, MAXLABEL, XMLAB )

      CALL NDF_ACGET( IAMNDF, 'UNITS', 1, L2STRING, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL GEN_CTOS( L2STRING, MAXLABEL, XMUN, NDUM )
      SLEN = CHR_LEN( L2STRING )
      CALL STR_TERM( SLEN, MAXLABEL, XMUN )

      CALL NDF_AUNMP( IAMNDF, 'CENTRE', 1, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL CMP_UNMAP( IUEDRLOC, 'WAVES', IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
         CALL CMP_UNMAP( IUEDRLOC, 'WEIGHTS', IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
      END IF

      CALL NDF_UNMAP( IAMNDF, 'DATA', IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL DAT_ANNUL( IUEDRLOC, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL NDF_END( IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL HDS_CLOSE( MAINLOC, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      GO TO 999

 998  CALL ERR_FLUSH( IOS )
      CALL ERROUT( '\\', STATUS )

 999  CONTINUE

      END



      SUBROUTINE READ_SPEC( N, NORD, DATA, QUAL, WAVES )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMSAVE'

*  Arguments Given:
      INTEGER N
      INTEGER NORD

      REAL DATA( N, NORD )
      REAL WAVES( N, NORD )

      BYTE QUAL( N, NORD )

*  Local Variables:
      INTEGER I, IORD
*.

      DO IORD = 1, NORD
         DO I = 1, N
            SNETS( I, IORD ) = DBLE( DATA( I, IORD ) )
            WAVS( I, IORD ) = DBLE( WAVES( I, IORD ) )
            QNETS( I, IORD ) = QUAL( I, IORD )
         END DO
         WAV1S( IORD ) = WAVS( 1, IORD )
         WAV2S( IORD ) = WAVS( NWAVS( IORD ), IORD )

         DO I = N + 1, 1200
            SNETS( I, IORD ) = 0.0
            WAVS( I, IORD ) = 0.0
            QNETS( I, IORD ) = 0
         END DO
      END DO

      END


      SUBROUTINE READ_MSPEC( N, DATA, QUAL, WAVES, WEIGHTS )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMCOMB'

*  Arguments Given:
      INTEGER N

      REAL DATA( N )
      REAL WAVES( N )
      REAL WEIGHTS( N )

      BYTE QUAL( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         QCOMB( I ) = QUAL( I )
         WCOMB( I ) = DBLE( WEIGHTS( I ) )
         XCOMB( I ) = DBLE( WAVES( I ) )
         YCOMB( I ) = DBLE( DATA( I ) )
      END DO

      DO I = N + 1, 27800
         QCOMB( I ) = 0
         WCOMB( I ) = 0.0
         XCOMB( I ) = 0.0
         YCOMB( I ) = 0.0
      END DO

      END
