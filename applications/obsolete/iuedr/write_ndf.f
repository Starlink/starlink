      SUBROUTINE WRITE_NDF( SUBCMD, NAME, N, X, Y, XLAB, YLAB, TITLE,
     :                      NORD, STATUS )
*+
*  Name:
*     SUBROUTINE WRITE_NDF

*  Purpose:
*     Write IUEDR data to Starlink NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRITE_NDF( SUBCMD, NAME, N, X, Y, XLAB, YLAB, TITLE,
*    :                NORD, STATUS )

*  Arguments:
*     SUBCMD = CHARACTER* ( * ) (Given)
*        Type of data to be written:
*           'IMAGE'     IUE Image data from tape/file.
*           'SPECTRUM'  Net Spectrum/spectra.
*           'MSPECTRUM' Mean Spectrum.
*           'SP0WR'     Pseudo-SPECTRUM ZERO fromat NDF (for DIPSO).
*     NAME = CHARACTER* ( * ) (Given)
*        Name of the file to be written.
*     N = INTEGER (Given)
*        First data dimension.
*     X = REAL*8( N, NORD ) (Given)
*        Wavelength data for 'SP0WR' mode, ignored otherwise.
*     Y = REAL*8( N, NORD ) (Given)
*        Flux data for 'SP0WR' mode, ignored otherwise.
*     XLAB = CHARACTER* ( * ) (Given)
*        Label for the X-axis.
*     YLAB = CHARACTER* ( * ) (Given)
*        Label for the Y-axis.
*     TITLE = CHARACTER*80 (Given)
*        Title for the NDF.
*     NORD = INTEGER (Given)
*        Second data dimension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (DMILLS):
*       IUEDR Vn. 3.0
*     19-AUG-94 (MJC):
*       IUEDR Vn. 3.1
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

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMDATA'
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

      INTEGER N
      INTEGER NORD

      REAL*8 X( N, NORD )
      REAL*8 Y( N, NORD )

      CHARACTER*( * ) XLAB
      CHARACTER*( * ) YLAB
      CHARACTER*80 TITLE

*  Status:
      INTEGER STATUS    ! Global status.

*  Local Variables:
      CHARACTER*80 LLSTRING
      CHARACTER*40 LSTRING

      INTEGER IMDIMS( 2 )
      INTEGER AXPTR1
      INTEGER AXPTR2
      INTEGER DATAPTR
      INTEGER QUALPTR
      INTEGER WPNTR
      INTEGER IOS
      INTEGER NNN
      INTEGER NDUM
      INTEGER NCHAR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IOS = SAI__OK

*  Create new HDS file.
      CALL HDS_NEW( NAME, 'IUEDR', 'NDF', 0, 0, MAINLOC, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

*  Start NDF context.
      CALL NDF_BEGIN

*  Set up NDF for raw image data (_UED file).
      IF ( SUBCMD .EQ. 'IMAGE' ) THEN
         IMDIMS( 1 ) = 768
         IMDIMS( 2 ) = 768
         CALL DAT_NEW( MAINLOC, 'DATA_ARRAY', '_WORD', 2, IMDIMS, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL DAT_NEW( MAINLOC, 'QUALITY', 'QUALITY', 0, 1, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL DAT_FIND( MAINLOC, 'QUALITY', QUALLOC, IOS )
         CALL DAT_NEW( QUALLOC, 'QUALITY', '_UBYTE', 2, IMDIMS, IOS )

*  Set up NDF for spectrum file (_UES or _UEM file).
      ELSE IF ( SUBCMD.EQ.'SPECTRUM' .OR. SUBCMD.EQ.'MSPECTRUM' ) THEN
         IF ( NORD .EQ. 1 ) THEN
            CALL DAT_NEW( MAINLOC, 'DATA_ARRAY', '_REAL', 1, N, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL DAT_NEW( MAINLOC , 'QUALITY', 'QUALITY', 0, 1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL DAT_FIND( MAINLOC, 'QUALITY', QUALLOC, IOS )
            CALL DAT_NEW( QUALLOC, 'QUALITY', '_UBYTE', 1, N, IOS )

         ELSE
            IMDIMS( 1 ) = N
            IMDIMS( 2 ) = NORD
            CALL DAT_NEW( MAINLOC, 'DATA_ARRAY', '_REAL', 2, IMDIMS,
     :                    IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL DAT_NEW( MAINLOC, 'QUALITY', 'QUALITY', 0, 1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL DAT_FIND( MAINLOC, 'QUALITY', QUALLOC, IOS )
            CALL DAT_NEW( QUALLOC, 'QUALITY', '_UBYTE', 2, IMDIMS,
     :                    IOS )
         END IF

*  Set up NDF for "SP0" format.
      ELSE IF ( SUBCMD .EQ. 'SP0WR' ) THEN
         CALL DAT_NEW( MAINLOC, 'DATA_ARRAY', '_REAL', 1, N, IOS )
      END IF

*  Check status of last DAT_NEW call.
      IF ( IOS .NE. SAI__OK ) GO TO 998

*  Write _UED.
      IF ( SUBCMD .EQ. 'IMAGE' ) THEN

*     Imoprt HDS descriptor into NDF context and map components.
         CALL NDF_IMPRT( MAINLOC, IAMNDF, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'DATA', '_WORD', 'WRITE', DATAPTR, NNN,
     :                 IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'QUALITY', '_UBYTE', 'WRITE', QUALPTR,
     :                 NNN, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

*     Write data to NDF.
         CALL MOVE_WORD( NNN, %VAL( DATA_VM ), %VAL( DATAPTR ) )
         CALL MOVE_BYTE( NNN, %VAL( QUAL_VM ), %VAL( QUALPTR ) )

*     Label the NDF.
         CALL NDF_CPUT( 'IUE image', IAMNDF, 'TITLE', IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

*     Unmap/annul descriptors and shut down NDF context.
         CALL NDF_UNMAP( IAMNDF, 'DATA,QUALITY', IOS )
         CALL DAT_ANNUL( QUALLOC , IOS )
         CALL NDF_END( IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL HDS_CLOSE( MAINLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         GO TO 999

*  Extend _UES or _UEM.
      ELSE IF ( SUBCMD.EQ.'SPECTRUM' .OR. SUBCMD.EQ.'MSPECTRUM' ) THEN
         NNN = N
         CALL NDF_IMPRT( MAINLOC, IAMNDF, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'DATA', '_REAL', 'WRITE', DATAPTR, N,
     :                 IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_MAP( IAMNDF, 'QUALITY', '_UBYTE', 'WRITE', QUALPTR,
     :                 N, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_XNEW( IAMNDF, 'IUEDR_EXTRA', 'EXTENSION', 0, 0,
     :                  IUEDRLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_XLOC( IAMNDF, 'IUEDR_EXTRA', 'WRITE', IUEDRLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         IF ( NORD .GT. 1 ) THEN
            CALL CMP_MOD( IUEDRLOC, 'WAVES', '_REAL', 2, IMDIMS, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_MAPN( IUEDRLOC, 'WAVES', '_REAL', 'WRITE', 2,
     :                     AXPTR1, IMDIMS, IOS )

            IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
               CALL CMP_MOD( IUEDRLOC, 'WEIGHTS', '_REAL', 2, IMDIMS,
     :                       IOS )
               IF ( IOS .NE. SAI__OK ) GO TO 998
               CALL CMP_MAPN( IUEDRLOC, 'WEIGHTS', '_REAL', 'WRITE',
     :                        2, WPNTR, IMDIMS, IOS )
            END IF

         ELSE
            CALL CMP_MOD( IUEDRLOC, 'WAVES', '_REAL', 1, N, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_MAPN( IUEDRLOC, 'WAVES', '_REAL', 'WRITE', 1,
     :                     AXPTR1, NDUM, IOS )

            IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
               CALL CMP_MOD( IUEDRLOC, 'WEIGHTS', '_REAL', 1, N, IOS )
               IF ( IOS .NE. SAI__OK ) GO TO 998
               CALL CMP_MAPN( IUEDRLOC, 'WEIGHTS', '_REAL', 'WRITE',
     :                        1, WPNTR, NDUM, IOS )
            END IF

            CALL NDF_AMAP( IAMNDF, 'CENTRE', 1, '_REAL', 'WRITE',
     :                     AXPTR2, N, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL NDF_ACPUT( 'Angstroms', IAMNDF, 'UNITS', 1, IOS )
         END IF

*     Check status of last HDS/NDF call.
         IF ( IOS .NE. SAI__OK ) GO TO 998

*     Put _UES labels and order numbers in NDF.
         IF ( SUBCMD .EQ. 'SPECTRUM' ) THEN
            CALL NDF_CPUT( TITLE, IAMNDF, 'TITLE', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_MOD( IUEDRLOC, 'ORDERS', '_INTEGER', 1, NORD,
     :                    IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_PUTVI( IUEDRLOC, 'ORDERS', NORD, ORDERS, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_MOD( IUEDRLOC, 'NWAVS', '_INTEGER', 1, NORD, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_PUTVI( IUEDRLOC, 'NWAVS', NORD, NWAVS, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL NDF_CPUT( YLAB, IAMNDF, 'LABEL', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

*     Label _UEM parts and enter wavelength scaling.
         ELSE IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
            CALL CMP_MOD( IUEDRLOC, 'XCOMB1', '_DOUBLE', 0, 0, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_PUT0D( IUEDRLOC, 'XCOMB1', XCOMB1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_MOD( IUEDRLOC, 'DXCOMB', '_DOUBLE', 0, 0, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
            CALL CMP_PUT0D( IUEDRLOC, 'DXCOMB', DXCOMB, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL GEN_STOC( MTITLE, MAXTITLE, LLSTRING, NCHAR )
            CALL NDF_CPUT( LLSTRING, IAMNDF, 'TITLE', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL GEN_STOC( YMUN, MAXLABEL, LSTRING, NCHAR )
            CALL NDF_CPUT( LSTRING, IAMNDF, 'UNITS', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL GEN_STOC( XMUN, MAXLABEL, LSTRING, NCHAR )
            CALL NDF_ACPUT( LSTRING, IAMNDF, 'UNITS', 1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL GEN_STOC( XMLAB, MAXLABEL, LSTRING, NCHAR )
            CALL NDF_ACPUT( LSTRING, IAMNDF, 'LABEL', 1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998

            CALL GEN_STOC( YMLAB, MAXLABEL, LSTRING, NCHAR )
            CALL NDF_CPUT( LSTRING, IAMNDF, 'LABEL', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
         END IF

*     Write data to NDF.
         IF ( SUBCMD .EQ. 'SPECTRUM' ) THEN
            CALL MOVE_SPEC( NNN, NORD, %VAL( DATAPTR ), %VAL( QUALPTR ),
     :                      %VAL( AXPTR1 ) )

         ELSE
            CALL MOVE_MSPEC( NNN, %VAL( DATAPTR ), %VAL( QUALPTR ),
     :                       %VAL( AXPTR1 ), %VAL( WPNTR ) )
         END IF

*     Write axis elements to NDF.
         IF ( NORD .EQ. 1 ) THEN
            IF ( SUBCMD .EQ. 'SPECTRUM' ) THEN
                CALL MAKE_SREAL( NNN, %VAL( AXPTR2 ) )

            ELSE
                CALL MAKE_MREAL( NNN, %VAL( AXPTR2 ) )
            END IF
            CALL NDF_AUNMP( IAMNDF, 'CENTRE', 1, IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
         END IF

*     Unmap/annul pointers.
         CALL CMP_UNMAP( IUEDRLOC, 'WAVES', IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         IF ( SUBCMD .EQ. 'MSPECTRUM' ) THEN
            CALL CMP_UNMAP( IUEDRLOC, 'WEIGHTS', IOS )
            IF ( IOS .NE. SAI__OK ) GO TO 998
         END IF

         CALL DAT_ANNUL( IUEDRLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
         CALL NDF_UNMAP( IAMNDF, 'DATA,QUALITY', IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

*     End NDF context.
         CALL NDF_END( IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         CALL DAT_ANNUL( QUALLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         CALL HDS_CLOSE( MAINLOC , IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998

         GO TO 999

*  Write "SP0" file.
      ELSE IF ( SUBCMD .EQ. 'SP0WR' ) THEN
          CALL NDF_IMPRT( MAINLOC, IAMNDF, IOS )
          IF ( IOS .NE. SAI__OK ) GO TO 998
          CALL NDF_MAP( IAMNDF, 'DATA', '_REAL', 'WRITE', DATAPTR,
     :                  N, IOS )
      END IF

      IF ( IOS .NE. SAI__OK ) GO TO 998

*  Labelling.
      CALL NDF_CPUT( TITLE, IAMNDF, 'TITLE', IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL NDF_CPUT( YLAB, IAMNDF, 'LABEL', IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998


      CALL NDF_AMAP ( IAMNDF, 'CENTRE', 1, '_REAL', 'WRITE', AXPTR1,
     :                N, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL NDF_ACPUT ( XLAB, IAMNDF, 'LABEL', 1, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

*  Load spectrum into NDF.
      CALL D_TO_REAL( N, X, %VAL( AXPTR1 ) )
      CALL D_TO_REAL( N, Y, %VAL( DATAPTR ) )

*  Last unmap/annul calls.
      CALL NDF_AUNMP( IAMNDF, 'CENTRE', 1, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      CALL NDF_UNMAP( IAMNDF, 'DATA', IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      IF ( SUBCMD .NE. 'SP0WR' ) THEN
         CALL DAT_ANNUL( IUEDRLOC, IOS )
         IF ( IOS .NE. SAI__OK ) GO TO 998
      END IF

      CALL NDF_END ( IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998

      CALL HDS_CLOSE ( MAINLOC, IOS )
      IF ( IOS .NE. SAI__OK ) GO TO 998
      GO TO 999

 998  CALL ERR_FLUSH( IOS )
      CALL ERROUT( '\\', STATUS )

 999  CONTINUE

      END



      SUBROUTINE MOVE_REAL ( N, FROM, TO )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER N
      REAL FROM( N )

*  Arguments Given and Returned:
      REAL TO( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         TO( I ) = FROM( I )
      END DO

      END



      SUBROUTINE MAKE_MREAL( N, TO )
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

*  Arguments Returned:
      REAL TO( N )

*  Local Variables:
      REAL RTO
      INTEGER I
*.

      DO I = 1, N
        RTO = REAL( XCOMB( I ) )
        TO( I ) = RTO
      END DO

      END



      SUBROUTINE MAKE_SREAL( N, TO )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMSAVE'

*  Arguments given:
      INTEGER N

*  Arguments Returned:
      REAL TO( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         TO( I ) = REAL( WAVS( I, 1 ) )
      END DO

      END


      SUBROUTINE MOVE_WORD( N, FROM, TO )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER N
      INTEGER*2 FROM( N )

*  Arguments Given and Returned:
      INTEGER*2 TO( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         TO( I ) = FROM( I )
      END DO

      END


      SUBROUTINE MOVE_BYTE( N, FROM, TO )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER N
      BYTE FROM( N )

*  Arguments Given and Returned:
      BYTE TO( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         TO( I ) = FROM( I )
      END DO

      END


      SUBROUTINE MOVE_SPEC( N, NORD, ODATA, OQUAL, OWAVES )
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

*  Arguments Returned:
      REAL ODATA( N, NORD )
      REAL OWAVES( N, NORD )

      BYTE OQUAL( N, NORD )

*  Local Variables:
      INTEGER I
      INTEGER IORD
*.

      DO IORD = 1, NORD
         DO I = 1, N
            ODATA( I, IORD ) = REAL( SNETS( I, IORD ) )
            OWAVES( I, IORD ) = REAL( WAVS( I, IORD ) )
            OQUAL( I, IORD ) = QNETS( I, IORD )
         END DO
      END DO

      END


      SUBROUTINE MOVE_MSPEC( N, ODATA, OQUAL, OWAVES, OWEIGHTS )
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

*  Arguments Returned:
      REAL ODATA( N )

      BYTE OQUAL( N )

      REAL OWAVES( N )
      REAL OWEIGHTS( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         ODATA( I ) = REAL( YCOMB( I ) )
         OWAVES( I ) = REAL( XCOMB( I ) )
         OWEIGHTS( I ) = REAL( WCOMB( I ) )
         OQUAL( I ) = QCOMB( I )
      END DO

      END


      SUBROUTINE D_TO_REAL( N, FROM, TO )
*+
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER N
      REAL*8 FROM( N )

*  Arguments Returned:
      REAL TO( N )

*  Local Variables:
      INTEGER I
*.

      DO I = 1, N
         IF ( FROM( I ) .EQ. VAL__BADD ) THEN
            TO( I ) = VAL__BADR

         ELSE
            TO( I ) = REAL( FROM( I ) )
         END IF
      END DO

      END
