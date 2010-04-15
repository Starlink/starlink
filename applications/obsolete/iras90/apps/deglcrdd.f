      SUBROUTINE DEGLCRDD( STATUS )
*+
*  Name:
*     DEGLCRDD

*  Purpose:
*     Deglitch the CRDD data

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DEGLCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application deglitches the detector data traces of a group
*     CRDD files.
*
*     Glitches are defined as spikes in the data which exceed the local
*     noise level, and which are of duration less than a specified
*     width, see parameter GLWID.
*
*     The samples in the detected glitches can either be replaced by a
*     Starlink bad value or be flaged by a specified quality name, see
*     parameter QNAME, (GLITCH by default).
*
*     In case the samples in the glitches being replaced by Starlink
*     bad values, there will be a new CRDD file created for each input
*     CRDD file. Only the DATA component of each NDF is modified; all
*     other components are copied from the input CRDD file to the output
*     without change. If the samples in the glitches are being flagged,
*     the QUALITY component of the input CRDD file is updated and there
*     will no new CRDD file created.

*  ADAM Parameters:
*     BOX = REAL (Read)
*        The size of the smoothing box, given in times of the width of
*        the point source profile, used when rejecting samples stay
*        too far away from its local average, see section Algorithm.
*        [2.0]
*     CLIP( NITER ) = REAL (Read)
*        The times of standard deviations at which samples will
*        regarded as staying too far away from its local average and
*        being rejected hence in each iteration, see section Algorithm.
*        If the given number of CLIP less then the number of iteration,
*        the last CLIP value will be repeated for the remaining CLIP.
*        [3.0]
*     COMMENT = LITERAL (Read)
*        A comment to store with the quality name assigned to the
*        samples in the detected glitches. If parameter will only be
*        used when parameter SETBAD has a value of FALSE and the
*        specified quality name (by parameter QNAME) is not currently
*        defined within the input CRDD file.
*        [Samples in the detected glitches]
*     GLWID = REAL (Read)
*        The spikes whose duration less than this value times the size
*        of a point source are regarded as a glitch and are either
*        replaced by BAD values or flagged by a specified quality name.
*        A value of zero will suppress all deglitching, whereas a value
*        of 1.0 will give very strong deglitching which will probably
*        reject point sources as well. [0.3]
*     HISTORY = LOGICAL (Read)
*        Determines if history information is to be added to the input
*        or output CRDD files. See help on "History_in_IRAS90" for more
*        information on history.
*        [Current history setting]
*     IN = NDF (Read)
*        Specifies a group of input CRDD files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        There is no limit on the number of CRDD files which can be
*        specified.
*     MSG_FILTER = LITERAL (Given)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*        [current message filter setting]
*     NITER = INTEGER (Given)
*        The number of iterations to perform the "soomth-reject"
*        procedure. [8]
*     OUT = NDF (Write)
*        When the samples in the detected glitches are replaced by
*        Starlink bad values, this parameter is used to give a group of
*        output CRDD files corresponding one-for-one with the list of
*        input CRDD files given for parameter IN. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        Expressions such as "*_DG" are expanded by replacing the "*"
*        character with each input CRDD file in turn.
*     PROFILES = NDF (Read)
*        An NDF holding in-scan profiles of ideal point sources. The
*        width of a point source in a waveband are difined as the width
*        of the profile, given here,  at its half max. value.
*        The default value is the files "profiles.sdf" contained in
*        the main IRAS90 directory, which contains profiles taken from
*        the IRAS Catalogs and Atlases Explanatory Supplement, page
*        V-14. [ ]
*     QNAME = LITERAL (Read)
*        The quality name to be assigned to the samples in detected
*        glitched. If the supplied name is not already defined within
*        the input NDF, then a definition of the name is added to the
*        NDF. The user is warned if the quality name is already defined
*        with the NDF. [GLITCH]
*     SETBAD = LOGICAL (Read)
*        If it is true, the samples in the detected glitches will be
*        replaced by Starlink BAD values. And a output CRDD file will
*        created for each input CRDD file to hold the modified data. If
*        it is false, the quality components of the input CRDD files
*        will be modified to hold specified quality name for these
*        samples in the glitches. [TRUE]
*     XNAME = LITERAL (Read)
*        If the NDF already contains any quality name definitions then
*        new quality names are put in the same extension as the old
*        names. If no previous quality names have been stored in the NDF
*        then this parameter will be used to obtain the name of an NDF
*        extension in which to store new quality name. The extension
*        will be created if it does not already exit (see parameter
*        XTYPE ). [QUALITY_NAMES]
*     XTYPE = LITERAL (Read)
*        If a new NDF extension is created to hold quality names (see
*        parameter), then this parameter is used to obtained the HDS
*        data type for the created extension. The default is to give the
*        extension a type identical to its name. [ ]

*  Algorithm:
*     The deglitching is based on an iterative scheme in which the input
*     detector data stream is smoothed with a box filter of width
*     specified by parameter BOX. An estimate of the RMS residual
*     between input and smoothed data is formed, and input data
*     corresponding to residuals of greater than CLIP (see parameter
*     CLIP ) times the RMS residual are rejected. This "smooth-reject"
*     procedure is repeaded NITER (see parameter NITER ) times. Glitches
*     are then identified by looking for blocks of adjacent rejected
*     output pixels. Any such block with sizes less than or equal to
*     GLWID (see parameter GLWID ) times a point source width are
*     regarded as a glitch. The samples in the detected glitches are
*     either replaced by Starlink BAD values or have the quality being
*     flagged by a specified quality name (see parameter SETBAD).
*     All other input data is retained in its original form.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT package constants
      INCLUDE 'MSG_PAR'          ! MSG package constants
      INCLUDE 'PRM_PAR'          ! Primitive constants
      INCLUDE 'IRQ_PAR'          ! IRQ package constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants
      INCLUDE 'IRA_PAR'          ! IRA package constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXITER             ! Max number of iterations
      PARAMETER ( MXITER = 10 )

*  Local Variables:
      INTEGER BAND               ! Waveband number of input CRDD file
      REAL BOX                   ! Size of box in times of PSF width
      INTEGER BOXSZ              ! Size of box in samples
      REAL CLIP( MXITER )        ! Clip threthood when filter data
      INTEGER EL                 ! Number of mapped elements
      INTEGER GINID, GOUTID      ! Input and output CRDD group ID
      REAL GLWID                 ! Width of glitches
      INTEGER GLHWID             ! Width of glitches in sample numbers
      CHARACTER*( 80 ) HSTORY    ! History comment written to output NDF
      INTEGER I                  ! Do loop index
      INTEGER IRCID              ! ID for IRC package
      INTEGER LBND( 2 ), UBND( 2 ) ! Bounds of input CRDD data array
      CHARACTER*( DAT__SZLOC ) LOCS( 5 )
                                 ! HDS locators used by IRQ routines
      INTEGER NBAD               ! Bad sample flag
      INTEGER NCLIP              ! Number of clip value obtained
      INTEGER NCRDD              ! Number of input CRDD files
      INTEGER NDFIN, NDFOUT, NDFPF ! IDs for in/output & profile NDFs
      INTEGER NDIM               ! Number of dimension of input CRDD NDF
      INTEGER NITER              ! Number of iteration when filter data
      INTEGER NOCRDD             ! Number of output CRDD files
      INTEGER NPSMP              ! Number. of samples in point source
      INTEGER NPROF              ! No. of point source profiles obtained
      CHARACTER*( 30 ) NSTR      ! String holding input NDF name
      INTEGER NSTRLN             ! Used length of NSTR
      INTEGER OSIZE, WSIZE       ! Size of temporary arrays
      INTEGER OUT, WORK1, WORK2, WORK3
                                 ! Pointer to temporary arrays
      INTEGER PDIN, PDOUT        ! Pointers to mapped in/output data
      INTEGER PPROFD             ! Pointer to point source profile data
      INTEGER PPROFX             ! Pointer to point source profile axis
      REAL PRFWID( I90__BANDS )  ! Width of profile for each waveband
                                 ! in arcmins
      LOGICAL QADD               ! Flag showes quality name added to NDF
      CHARACTER*( IRQ__SZQNM ) QNAME ! Name of quality assigned to
                                     ! samples in detected glitches
      INTEGER QNAMLN             ! Used length of QNAME
      DOUBLE PRECISION REFRA, REFDEC ! Reference position of input CRDD
      LOGICAL SETBAD             ! Set glitch samples as bad flag
      REAL SPD                   ! Speed of CRDD scan
      INTEGER SOP, OBS           ! SOP and Observation number of CRDD

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level.
      CALL MSG_IFGET( STATUS )

*  See what to do about the samples in the detected glitches.
      CALL PAR_GET0L( 'SETBAD', SETBAD, STATUS )

*  Get the size of smooth box.
      CALL PAR_GET0R( 'BOX', BOX, STATUS )

*  Get the number of iterations.
      CALL PAR_GET0I( 'NITER', NITER, STATUS )

*  Get the clip threshood for each iteration.
      CALL PAR_GET1R( 'CLIP', MXITER, CLIP, NCLIP, STATUS )

*  If number of clip threshood given by the user less than the number
*  of the iteration, repeat the last clip.
      DO I = NCLIP + 1, NITER
         CLIP( I ) = CLIP( NCLIP )
      END DO

*  Get a group of CRDD file names to be deglitched.
      CALL IRM_RDNDF( 'IN', 0, 1, 'More CRDD file names', GINID, NCRDD,
     :                 STATUS )

*  If the samples in the detected glitches will be replaced by bad
*  value, get a group of NDFs to contain the output CRDD.
      IF ( SETBAD ) THEN
         CALL IRM_WRNDF( 'OUT', GINID, NCRDD, NCRDD,
     :                   'More output CRDD file names', GOUTID, NOCRDD,
     :                   STATUS )
      END IF

*  If unable to get access to the input and/or output CRDD files, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Creat 4 temporary arrays. The size of the arrays will be changed
*  later according to the size of each input CRDD data array.
      OSIZE = 600 * 16
      WSIZE = 600
      CALL PSX_MALLOC( OSIZE * VAL__NBR, OUT, STATUS )
      CALL PSX_MALLOC( WSIZE * VAL__NBR, WORK1, STATUS )
      CALL PSX_MALLOC( WSIZE * VAL__NBR, WORK2, STATUS )
      CALL PSX_MALLOC( WSIZE * VAL__NBI, WORK3, STATUS )

*  If error happened when getting temporary arrays, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the point source profile from the environment.
      CALL IRM_PROFL( 'PROFILES', NPSMP, NPROF, PPROFD, PPROFX, NDFPF,
     :                 STATUS )

*  Find the width of the point source profile.
      CALL DGCRA0( NPSMP, NPROF, %VAL( PPROFD ), %VAL( PPROFX ), PRFWID,
     :             STATUS )

*  Annul the id to the point source profile NDF.
      CALL NDF_ANNUL( NDFPF, STATUS )

*  Get the relative width of spikes belowe which they will be regarded
*  as  glitches.
      CALL PAR_GET0R( 'GLWID', GLWID, STATUS )

*  Conditionaly report the width of glitches.
      IF ( NPROF .EQ. 4 ) THEN
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL MSG_SETR( 'WID', PRFWID( 1 ) * GLWID )
         CALL MSG_OUTIF( MSG__VERB, 'DEGLCRDD_MSG1',
     :                  'Spikes of ^WID arcmin width or less in 12um '/
     :                 /'waveband will be regarded as glitches',
     :                   STATUS )
         CALL MSG_SETR( 'WID', PRFWID( 2 ) * GLWID )
         CALL MSG_OUTIF( MSG__VERB, 'DEGLCRDD_MSG1',
     :                  'Spikes of ^WID arcmin width or less in 25um '/
     :                 /'waveband will be regarded as glitches',
     :                   STATUS )
         CALL MSG_SETR( 'WID', PRFWID( 3 ) * GLWID )
         CALL MSG_OUTIF( MSG__VERB, 'DEGLCRDD_MSG1',
     :                  'Spikes of ^WID arcmin width or less in 60um '/
     :                 /'waveband will be regarded as glitches',
     :                   STATUS )
         CALL MSG_SETR( 'WID', PRFWID( 4 ) * GLWID )
         CALL MSG_OUTIF( MSG__VERB, 'DEGLCRDD_MSG1',
     :                  'Spikes of ^WID arcmin width or less in 100um '/
     :                 /'waveband will be regarded as glitches',
     :                   STATUS )
      ELSE
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL MSG_SETR( 'WID', PRFWID( 1 ) * GLWID )
         CALL MSG_OUTIF( MSG__VERB, 'DEGLCRDD_MSG1',
     :                  'Spikes of ^WID arcmin width or less in all '/
     :                 /'wavebands will be regarded as glitches',
     :                   STATUS )

      END IF

*  Initialise the IRC package.
      CALL IRC_INIT( STATUS )

*  Processing input CRDD one by one.
      DO I = 1, NCRDD

*  Get the NDF ID for this CRDD file. If set samples in glitches as BAD,
*  a new NDF will created to contain the degliched data and hence the
*  input NDF will only be read.
         IF ( SETBAD ) THEN
            CALL NDG_NDFAS( GINID, I, 'READ', NDFIN, STATUS )

*  Otherwise the input NDF will be updated to contain a new extension or
*  a new quality name.
         ELSE
            CALL NDG_NDFAS( GINID, I, 'UPDATE', NDFIN, STATUS )
         END IF

*  Get IRC ID for this CRDD file.
         CALL IRC_IMPRT( NDFIN, IRCID, STATUS )

*  Conditionaly tell the user which CRDD file is being processed.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'DEGLCRDD_MSG2',
     :                  '   Deglitch ^NDF ...' , STATUS )

*  Get the waveband number of this CRDD file.
         CALL IRC_INFO( IRCID, BAND, REFRA, REFDEC, SPD, SOP, OBS,
     :                  STATUS )

*  Get the box size of the smoothing filter in sample numbers.
         BOXSZ = NINT( BOX * PRFWID( BAND )
     :                / ( SPD * REAL( IRA__RTOD ) * 60.0 )
     :                * I90__SRATE( BAND ) )

*  Find the width of glitches in sample numbers.
         GLHWID = NINT( GLWID * PRFWID( BAND )
     :                 / ( SPD * REAL( IRA__RTOD ) * 60.0 )
     :                 * I90__SRATE( BAND ) )

*  Find the bounds of this CRDD file.
         CALL NDF_BOUND( NDFIN, 2, LBND, UBND, NDIM, STATUS )

*  If necessary, increase the size of the temporary arrays.
         IF ( OSIZE .LT. ( UBND( 1 ) - LBND( 1 ) + 1 ) *
     :                   ( UBND( 2 ) - LBND( 2 ) + 1 ) ) THEN
            OSIZE = ( UBND( 1 ) - LBND( 1 ) + 1 )
     :              * ( UBND( 2 ) - LBND( 2 ) + 1 )
            CALL PSX_REALLOC( OSIZE * VAL__NBR, OUT, STATUS )
         END IF
         IF ( WSIZE .LT. UBND( 1 ) - LBND( 1 ) + 1 ) THEN
            WSIZE = UBND( 1 ) - LBND( 1 ) + 1
            CALL PSX_REALLOC( WSIZE * VAL__NBR, WORK1, STATUS )
            CALL PSX_REALLOC( WSIZE * VAL__NBR, WORK2, STATUS )
            CALL PSX_REALLOC( WSIZE * VAL__NBI, WORK3, STATUS )
         END IF

*  If unable to increase the size of the temporary arrays, exit.
         IF ( STATUS .NE. SAI__OK ) GOTO 970

*  Map the NDF data array.
         CALL NDF_MAP( NDFIN, 'DATA', '_REAL', 'READ', PDIN, EL,
     :                 STATUS )

*  Reject the samples which stay too far away from the local average.
         CALL DGCRA1( UBND( 1 ) - LBND( 1 ) + 1,
     :                UBND( 2 ) - LBND( 2 ) + 1, %VAL( PDIN ), BOXSZ,
     :                NITER, CLIP, %VAL( OUT ),  %VAL( WORK1 ),
     :               %VAL( WORK2 ),  %VAL( WORK3 ),  STATUS )

*  If error happened while rejecting, exit.
         IF ( STATUS .NE. SAI__OK ) GOTO 970

*  If the samples in the glitch is to be set as bad, create a new output
*  CRDD file.
         IF ( SETBAD ) THEN
            CALL NDG_NDFPR( NDFIN, 'UNITS,VARIANCE,QUALITY,AXIS',
     :                      GOUTID, I, NDFOUT, STATUS )

*  Map the data component of the output NDF.
            CALL NDF_MAP( NDFOUT, 'DATA', '_REAL', 'WRITE', PDOUT, EL,
     :                    STATUS )

*  Detect glitches in the clearned input data, set set the samples in
*  the glitches as bad.
            CALL DGCRA2( UBND( 1 ) - LBND( 1 ) + 1,
     :                   UBND( 2 ) - LBND( 2 ) + 1, %VAL( PDIN ),
     :                  %VAL( OUT ), GLHWID, NBAD, %VAL( PDOUT ),
     :                   STATUS )

*  If some samples have detected as in glitches and set as bad, set the
*  bad pixel flag for the output NDF. Otherwise keep the flag unchenged.
            IF ( NBAD .GT. 0 ) THEN
               CALL NDF_SBAD( .TRUE., NDFOUT, 'DATA', STATUS )
            END IF

*  If error happened, exit.
            IF ( STATUS .NE. SAI__OK ) GOTO 970

*  Conditionaly tell the user an output CRDD file is created.
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_OUTIF( MSG__NORM, 'DEGLCRDD_MSG3',
     :               '      ^NBAD samples are set as bad',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL NDF_MSG( 'NDF', NDFOUT )
            CALL MSG_OUTIF( MSG__NORM, 'DEGLCRDD_MSG3',
     :                  '   Deglitched CRDD data are stored in ^NDF',
     :                      STATUS )

*  Unmap the input and output NDFs.
            CALL NDF_UNMAP( NDFIN, 'DATA', STATUS )
            CALL NDF_UNMAP( NDFOUT, 'DATA', STATUS )

*  Add a history record to the output NDF.
            CALL NDF_MSG( 'NDF', NDFIN )
            CALL MSG_LOAD( 'DEGLCRDD_MSG4', '^NDF', NSTR, NSTRLN,
     :                      STATUS )
            HSTORY = 'Created from '//NSTR( : NSTRLN )//' with samples'/
     :              /' in detected glitches set as BAD'
            CALL IRM_HIST( 'HISTORY', NDFOUT, 'IRAS90:DEGLCRDD', 1,
     :                      HSTORY, STATUS )

*  Annul the id for the output file.
            CALL NDF_ANNUL( NDFOUT, STATUS )

*  If the samples in the glitches will have their quality set, find or
*  create (if not existing ) a extension which is used to contain
*  quality name information and add the supplied quality name (if not
*  yet exits ) to the extension.
         ELSE
            CALL DGCRA3( NDFIN, 'XNAME', 'XTYPE', 'QNAME', 'COMMENT',
     :                   QNAME, LOCS, QADD, STATUS )

*  Assign the named quality to the samples in the glitches.
            CALL DGCRA4( LOCS, QNAME, UBND( 1 ) - LBND( 1 ) + 1,
     :                   UBND( 2 ) - LBND( 2 ) + 1, %VAL( PDIN ),
     :                  %VAL( OUT ), 2, GLHWID, %VAL( WORK3 ),
     :                   STATUS )

*  Add a history record to the NDF.
            QNAMLN = CHR_LEN( QNAME )
            HSTORY = 'Quality "'//QNAME( : QNAMLN )/
     :              /'" assigned to the samples in detected glitches'
            CALL IRM_HIST( 'HISTORY', NDFIN, 'IRAS90:DEGLCRDD', 1,
     :                      HSTORY, STATUS )

*  If an error has occured, attempt to remove the quality name if it has
*  been created by this application.
            IF ( QADD .AND. STATUS .NE. SAI__OK ) THEN
               CALL ERR_BEGIN( STATUS )
               CALL IRQ_REMQN( LOCS, QNAME, STATUS )
               CALL ERR_END( STATUS )
            END IF

*  Release the quality name information.
            CALL IRQ_RLSE( LOCS, STATUS )
         END IF

*  Annul the IRC ID of the current CRDD file.
         CALL IRC_ANNUL( IRCID, STATUS )

*  Annul the ID of the current CRDD file.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  Process next CRDD file.
      END DO

 970  CONTINUE

*  End IRC context.
      CALL IRC_CLOSE( STATUS )

*  End NDF Context.
      CALL NDF_END( STATUS )

 980  CONTINUE

*  Release the temporary arrays.
      CALL PSX_FREE( WORK3, STATUS )
      CALL PSX_FREE( WORK2, STATUS )
      CALL PSX_FREE( WORK1, STATUS )
      CALL PSX_FREE( OUT, STATUS )

 999  CONTINUE

*  Delete the input and output groups.
      CALL GRP_DELET( GINID, STATUS )
      IF ( SETBAD ) CALL GRP_DELET( GOUTID, STATUS )

      END
