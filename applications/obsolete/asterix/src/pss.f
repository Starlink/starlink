*+  PSS - Locates sources in a dataset
      SUBROUTINE PSS( STATUS )
*
*    Description :
*
*     Locates sources in an Asterix binned dataset using a weighted cross-
*     correlation.
*
*     Output is sent to an SSDS ( source search dataset ) - relevant info
*     can then be extracted by other applications.
*
*     PSS has an associated Asterix User Note (USER_004). Be sure that
*     any changes made are reflected in this document.
*
*    Environment parameters :
*
*
*    Deficiencies :
*
*      PARAM mode when no variance?
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Jun 89 : V1.0-0   Original (DJA)
*     20 Oct 89 : V1.0-1   Freezes PSF array wrt data array when test source
*                          position spacing becomes less than FREESP.
*                          This prevents discontinuities at high zoom factors
*     31 Jan 90 : V1.2-2   Flux calculation changed to functional form based
*                          on statistic. Conversion to SSO storage (DJA)
*     06 Feb 90 : V1.2-3   Now divides data by variance in GET_BDS, if
*                          variance present. BDS.SIGPTR points to this
*                          data. CRE_SMAP(Q) routines now calculate PSF
*                          data squared in this case of constant PSF. (DJA)
*     22 Feb 90 : V1.2-6   When specifying SLICE to be some non-default area
*                          of the image, a border is added. This border is
*                          only used for calculation, the user gets only the
*                          slice asked for. Error handling extended to work
*                          with multiple confidence levels (DJA)
*     23 Feb 90 : V1.2-8   Significance map axes wrong when sub-pixelling.
*      2 Mar 90 : V1.2-9   Corrected bug where equatorial positions in images
*                          with x-coordinate increasing left to right were
*                          wrong. See PSS_XY2_EQU.
*     14 Apr 90 : V1.2-11  Now writes BOOK structure into results file (DJA)
*     29 Apr 90 : V1.2-12  Bilinear interpolation to resample psf after 1st
*                          iteration. PARAM mode added. BDS object moved to
*                          common block. Value of RAWC produced is corrected
*                          for fraction of psf enclosed. Now uses enclosed
*                          energy function rather than psf amplitude. (DJA)
*     11 May 90 : V1.2-14  Various bug fixes. Determines too many sources by
*                          comparing area of psf to area of search box. Writes
*                          image units to output. Elliptical errors added (DJA)
*     16 May 90 : V1.2-15  Added goodness of fit. (DJA)
*     24 May 90 : V1.2-16  Writes data units to significance map (DJA)
*     25 May 90 : V1.2-17  More internal consistency checks at SQRTs (DJA)
*     26 May 90 : V1.2-18  PSF_CON now controls constancy between zooms. First
*                          iteration is always constant. (DJA)
*     15 Jun 90 : V1.3-3   Only signifs within the area defined by SLICE (DJA)
*     26 Jun 90 : V1.3-4   Fixed problems storing positional error data,
*                          and zero variances in slice mode (DJA)
*     10 Aug 90 : V1.3-7   PSS_POSIN accepts SSDS as input. Default PSFPIX
*                          set using 68% enclosed energy (DJA)
*     18 Sep 90 : V1.3-8   Bug in param mode output RA,DEC fixed. Updated
*                          handling of multiple detections (DJA)
*      2 Oct 90 : V1.3-11  Cash statistic added (DJA)
*     20 Oct 90 : V1.4-0   Fitting added (DJA)
*     23 Oct 90 : V1.4-1   Minor mods to fitting and FLXCHI_INT (DJA)
*     27 Oct 90 : V1.4-2   Now accepts terminal typed RA,DEC and various
*                          formats. Removed stuff above not relevant (DJA)
*     14 Nov 90 : V1.4-3   Changes to upper limits (DJA)
*     18 Nov 90 : V1.4-4   More changes to upper limits (DJA)
*     11 Mar 91 : V1.4-5   Major re-write - see APP_001 (DJA)
*     30 Aug 91 : V1.5-3   Non-rescaling bug fixed (DJA)
*     25 Feb 92 : V1.6-1   Various improvements. General speed up, recognises
*                          psf models. Document now USER_004 (DJA)
*     15 Jun 92 : V1.6-3   Fitting by FIT_ routines. Sensitivity mapping (DJA)
*      5 Feb 94 : V1.7-3   Changes to way psf is evaluated in non-constant
*                          mode. More crash protection in PSS_FIT (DJA)
*     16 Feb 94 : V1.7-4   Removed most non-F77 structures from PSS. Only
*                          FIT_STRUC and POI_STR structures remain. (DJA)
*      3 Jun 94 : V1.7-5   A few changes to allow spectral images with one
*                          energy bin to be searched (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                    IFILE           ! Loop over files
      INTEGER                    NFILE           ! # input files
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( PSS__VERSION )

*    Initialise sub-systems
      CALL AST_INIT()
      CALL SSO_INIT()
      CALL PSF_INIT( STATUS )

*    Zero the common block
      CALL PSS_CMN_INIT( STATUS )

*    Set up output stream
      CALL PSS_OP_INIT( STATUS )

*    Multiple file mode?
      CALL PAR_GET0L( 'MULTI', CP_MULTI, STATUS )
      IF ( CP_MULTI ) THEN
        CALL PSS_MUL_INIT( NFILE, STATUS )
      ELSE
        NFILE = 1
      END IF

*    Process the data files
      DO IFILE = 1, NFILE

*      Open the next input
        CALL PSS_INP_OPEN( IFILE, NFILE, STATUS )

*      Report file?
        IF ( CP_MULTI ) THEN
          CALL MSG_SETC( 'FILE', MU_IMG )
          CALL MSG_PRNT( 'Processing ^FILE...' )
        END IF

*      First file?
        IF ( IFILE .EQ. 1 ) THEN

*        Get processing mode
          CALL PSS_GET_MODE( 'MODE', STATUS )

*        Expert mode?
          CALL PAR_GET0L( 'EXPERT', CP_EXPERT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Diagnostic mode?
          IF ( CP_EXPERT ) THEN
            CALL PAR_GET0L( 'DIAG', DI_ON, STATUS )
            IF ( DI_ON ) THEN
              CALL PSS_DIAG_GETSEL( STATUS )
            END IF
          END IF

        END IF

*      Get input dataset data
        CALL PSS_INP_LOAD( IFILE, STATUS )

*      Master control
        CALL PSS_CONTROL( IFILE, STATUS )

*      Find source extension too?
        IF ( CP_OPT .AND. (IFILE.EQ.1) ) THEN
          IF ( .NOT. CP_EXPERT ) THEN
            CALL PAR_DEF0L( 'EXTEN', .FALSE., STATUS )
          END IF
          CALL PAR_GET0L( 'EXTEN', CP_FITWIDTH, STATUS )
        END IF

*      Choose routine based on mode
        IF ( CP_MODE .EQ. PSS__M_UPMAP ) THEN
          CALL PSS_M_UPMAP( NFILE, IFILE, STATUS )
        ELSE IF ( CP_MODE .EQ. PSS__M_SENMAP ) THEN
          CALL PSS_M_SENMAP( NFILE, IFILE, STATUS )
        ELSE IF ( CP_MODE .EQ. PSS__M_SEARCH ) THEN
          CALL PSS_M_SEARCH( NFILE, IFILE, STATUS )
        ELSE IF ( CP_MODE .EQ. PSS__M_SIGVAR ) THEN
          CALL PSS_M_SIGVAR( NFILE, IFILE, STATUS )
        ELSE
          CALL PSS_M_SPOTS( NFILE, IFILE, STATUS )
        END IF

*      Close input files
        CALL PSS_INP_CLOSE( (IFILE.EQ.NFILE), STATUS )

*      Trap dodgy statuys
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
        END IF

      END DO

*    MULTI mode?
      IF ( CP_MULTI ) THEN

*      Close file list
        CALL PSS_MUL_CLOSE( STATUS )

*      Close dangling bgnd file
        IF ( MU_SAME_BCK ) THEN
          CALL PSS_BGND_CLOSE( .TRUE., STATUS )
        END IF

      END IF

*    Close psf access
      CALL PSS_PSF_CLOSE( STATUS )

*    Close output stream
      CALL PSS_OP_CLOSE( STATUS )

*    Inform environment of errors
 99   CALL SSO_CLOSE()
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
