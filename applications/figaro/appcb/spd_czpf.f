      SUBROUTINE SPD_CZPF( STATUS )
*+
*  Name:
*     SPD_CZPF

*  Purpose:
*     Resample and average several spectra.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZPF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine does all the action for RESAMP in SPECTRUM mode.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1992 (hme):
*        Original version as RESAMP.
*     28 Apr 1992 (hme):
*        Set title and bad pixel flags.
*        Switch to Dave Berry's IRG/IRH for list of input NDFs. All IRG
*        dealing except anulling the group identifier is done in SPAAA.
*        Use axis label, unit, centres and widths from Specdre
*        Extension if such information exists there.
*        Be a bit more chatty.
*     10 Jul 1992 (hme):
*        Use SPE-routines and SPEPAR include.
*     09 Sep 1992 (hme):
*        Don't set title.
*     29 Jan 1993 (hme):
*        Add PROPCO switch which prevents superfluous calculation of
*        covariances. When only one input spectrum without variances is
*        available, the output covariance is undefined anyway.
*     31 Jan 1993 (hme):
*        Add MODE parameter; former main routine becomes SPACX.
*     20 Jan 1994 (hme):
*        Fix bug whereby for the first spectrum the Extension locator
*        was annulled before SPECVALS/WIDS were mapped, so for the first
*        spectrum, AXIS(i) would always be used, SPECVALS/WIDS never.
*        This bug must have been in version 0.7, too.
*        Apart from spaab{dr} disuse message string. Report errors as
*        soon as this routine knows about them.
*     25 Jan 1995 (hme):
*        Rename from SPACX.
*     17 Nov 1995 (hme):
*        With SPD_CZPD using GRP instead of IRG/IRH, there is no HDS
*        locator coming back, and no file to close with HDS_CLOSE.
*     15 Oct 1997 (acc):
*        Change name RESAMPLE to RESAMP due to clash of names with FIGARO.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      LOGICAL VARUSE
      LOGICAL PROPCO             ! True if covariance to be propagated
      LOGICAL EOF                ! True whence input list exhausted
      LOGICAL NEEDC              ! True if covar. row sum to be output
      LOGICAL GOODV              ! True if variance to be output
      LOGICAL USEEXT             ! True if Specdre Extension exists
      LOGICAL USEVAL             ! True if SPECVALS exists
      LOGICAL USEWID             ! True if SPECWIDS exists
      INTEGER IDUMMY             ! Dummy integer returned value
      INTEGER N                  ! Loop index
      INTEGER NVALID             ! Counts valid input NDFs
      INTEGER PLACE1             ! NDF placeholder
      INTEGER PLACE2             ! NDF placeholder
      INTEGER PLACE3             ! NDF placeholder
      INTEGER GID                ! NDF group identifier
      INTEGER NDF1               ! Current input NDF identifier
      INTEGER XNDF1              ! Input centres NDF identifier
      INTEGER XNDF2              ! Input widths NDF identifier
      INTEGER NDF2               ! Output NDF identifier
      INTEGER NDFX               ! Output COVRS NDF identifier
      INTEGER NDFCOV             ! Square 2-D workspace NDF identifier
      INTEGER NDFRES             ! 1-D workspace NDF identifier
      INTEGER NDFOVL             ! Overlap workspace NDF identifier
      INTEGER SPAXIS             ! Number of spectroscopic axis in input
      INTEGER AXIS               ! Number of non-degenerate input axis
      INTEGER KMAX               ! Number of input elements
      INTEGER LMAX               ! Number of output elements
      INTEGER NMAX               ! Number of input NDFs
      INTEGER XNK                ! Pointer to input pixel positions
      INTEGER WNK                ! Pointer to input pixel width
      INTEGER INK                ! Pointer to input data values
      INTEGER VNK                ! Pointer to input data variances
      INTEGER XL                 ! Pointer to output pixel positions
      INTEGER WL                 ! Pointer to would-be output widths
      INTEGER INL                ! Pointer to resampled data values
      INTEGER VEC2               ! Pointer to output data values
      INTEGER VEC3               ! Pointer to output data variances
      INTEGER CRSL               ! Pointer to output covariance row sums
      INTEGER VECI               ! Pointer to counter array
      INTEGER MAT1               ! Pointer to overlap matrix
      INTEGER MAT2               ! Pointer to post-resample covariance
      INTEGER MAT3               ! Pointer to output covariance
      CHARACTER * ( 64 ) MESSAG  ! Contextual error message
      CHARACTER * ( 64 ) DLABEL  ! Data label (of first input)
      CHARACTER * ( 64 ) DUNITS  ! Data unit (of first input)
      CHARACTER * ( 64 ) ALABEL  ! Axis label (of first input)
      CHARACTER * ( 64 ) AUNITS  ! Axis unit (of first input)
      CHARACTER * ( 64 ) TDLAB   ! Data label of current input
      CHARACTER * ( 64 ) TDUNIT  ! Data unit of current input
      CHARACTER * ( 64 ) TALAB   ! Axis label of current input
      CHARACTER * ( 64 ) TAUNIT  ! Axis unit of current input
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Data type in file
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Axis type in file
      CHARACTER * ( NDF__SZTYP ) MTYPE ! Type in memory
      CHARACTER * ( NDF__SZFTP ) FTYPE ! Unused
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator of .MORE.SPECDRE

*.

*  Check inherited global status.
*  Set default error message.
      IF ( STATUS .NE. SAI__OK ) RETURN
      MESSAG = 'Unspecified failure.'

*  Open NDF.
      CALL NDF_BEGIN

*  Get control parameters INFO and VARUSE.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If there is only one spectrum and variances are not available or to
*  be ignored, then we need not keep score of covariances. Here we set
*  the switch to the default. The final value will be determined after
*  the first valid input NDF was found.
      PROPCO = .TRUE.

*  Also we set to default the switches GOODV and NEEDC. These will be
*  updated later only if PROPCO turned out to be true. So in case it
*  will be false, we set these two switches false as well. They are used
*  in tidying up.
      GOODV = .FALSE.
      NEEDC = .FALSE.

*  This is in effect a loop over valid input NDFs (NVALID) and over all
*  input NDFs (N).
*  While input NDF group not yet exhausted.
      EOF = .FALSE.
      N      = 0
      NVALID = 0
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT. EOF ) THEN

*     Forward to next valid input NDF.
*     SPD_CZPD will use or not use IRG
         N = N + 1
         CALL SPD_CZPD( INFO, VARUSE, GID, NMAX, N, NDF1, AXIS,
     :                  EOF, STATUS )

*     If NDF group exhausted and no valid NDF dealt with so far.
         IF ( EOF .AND. NVALID .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error reading ' //
     :         'input list. List is empty.', STATUS )
            GO TO 500

*     Else if some other failure occured.
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 500

*     Else if NDF group exhausted.
         ELSE IF ( EOF ) THEN
            IF ( INFO ) CALL MSG_OUT( 'SPD_CZPF_LASTIN',
     :         'Last input processed.', STATUS )
            GO TO 1
         END IF

*     If none of the above conditions fulfilled, the next valid NDF has
*     been accessed. Increment the counter.
         NVALID = NVALID + 1

*     Find out if the Specdre Extension exists and which is the
*     spectroscopic axis.
*     (We may want to use information from the Extension instead of the
*     standard axis information. So we have to look if such information
*     exists in the Extension.)
         CALL SPD_EAAA( NDF1, 'READ', USEEXT, XLOC, STATUS )
         CALL SPD_EABA( NDF1, USEEXT, SPAXIS, STATUS )

*     If the relevant axis here is not the spectroscopic axis as known
*     to the input's Extension, then we need not use the Extension at
*     all.
         IF ( AXIS .NE. SPAXIS ) THEN
            IF ( USEEXT ) CALL DAT_ANNUL( XLOC, STATUS )
            USEEXT = .FALSE.
         END IF

*     If so far we intend to use the Extension, then look if the pixel
*     centre and width information respectively exist.
*     For convenience, we keep the locator created here until after we
*     got the labels and units.
         IF ( USEEXT ) THEN
            CALL DAT_THERE( XLOC, XCMP6, USEVAL, STATUS )
            CALL DAT_THERE( XLOC, XCMP7, USEWID, STATUS )
            USEEXT = USEVAL .OR. USEWID
            IF ( .NOT. USEEXT ) CALL DAT_ANNUL( XLOC, STATUS )
         ELSE
            USEVAL = .FALSE.
            USEWID = .FALSE.
         END IF

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( XLOC, STATUS )
            CALL ERR_REP(
     :         'SPD_CZPF_ERR', 'RESAMP: Error exploring ' //
     :         'Specdre Extension.', STATUS )
            GO TO 500
         END IF

*     Get labels and units for data and axis.
*     The axis information is got either from the Extension's SPECVALS
*     or from the main AXIS structure.
         IF ( USEVAL ) THEN
            CALL NDF_FIND( XLOC, XCMP6, XNDF1, STATUS )
            CALL NDF_CGET( XNDF1, 'LABEL', TALAB,  STATUS )
            CALL NDF_CGET( XNDF1, 'UNITS', TAUNIT, STATUS )
            CALL NDF_ANNUL( XNDF1, STATUS )
         ELSE
            CALL NDF_ACGET( NDF1, 'LABEL', AXIS, TALAB, STATUS )
            CALL NDF_ACGET( NDF1, 'UNITS', AXIS, TAUNIT,  STATUS )
         END IF
         CALL NDF_CGET( NDF1, 'LABEL', TDLAB,  STATUS )
         CALL NDF_CGET( NDF1, 'UNITS', TDUNIT, STATUS )

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error getting ' //
     :         'label information.', STATUS )
            GO TO 500
         END IF

*     If this is the very first valid input.
         IF ( NVALID .EQ. 1 ) THEN

*        Set PROPCO switch appropriately.
*        If there is only one spectrum and variances are not available
*        or to be ignored, then we need not keep score of covariances.
            IF ( NMAX .EQ. 1 .AND. .NOT. VARUSE ) PROPCO = .FALSE.
            IF ( INFO .AND. .NOT. PROPCO )
     :         CALL MSG_OUT( 'SPD_CZPF_NOCOVP',
     :            'Warning: Cannot propagate covariances.', STATUS )

*        Get labels and units for axis and data for future reference.
            ALABEL = TALAB
            AUNITS = TAUNIT
            DLABEL = TDLAB
            DUNITS = TDUNIT

*        Establish the data type used.
*        DTYPE is used in output for values, variances, covar row sums.
*        ATYPE is used in output for pixel positions.
*        MTYPE is used in memory for all these arrays.
            CALL NDF_TYPE( NDF1, 'DATA,VARIANCE', DTYPE, STATUS )

*        If centres and widths are in Extension.
            IF ( USEVAL .AND. USEWID ) THEN
               CALL NDF_FIND( XLOC, XCMP6, XNDF1, STATUS )
               CALL NDF_FIND( XLOC, XCMP7, XNDF2, STATUS )
               CALL NDF_MTYPE( '_REAL,_DOUBLE', XNDF1, XNDF2, 'DATA',
     :                         ATYPE, FTYPE, STATUS )
               CALL NDF_ANNUL( XNDF1, STATUS )
               CALL NDF_ANNUL( XNDF2, STATUS )

*        Else if only centres are in Extension.
            ELSE IF ( USEVAL ) THEN
               CALL NDF_FIND( XLOC, XCMP6, XNDF1, STATUS )
               CALL NDF_TYPE( XNDF1, 'DATA', ATYPE, STATUS )
               CALL NDF_ANNUL( XNDF1, STATUS )

*        Else if only widths are in Extension.
            ELSE IF ( USEWID ) THEN
               CALL NDF_FIND( XLOC, XCMP7, XNDF2, STATUS )
               CALL NDF_TYPE( XNDF2, 'DATA', ATYPE, STATUS )
               CALL NDF_ANNUL( XNDF2, STATUS )

*        Else (no information in Extension).
            ELSE
               CALL NDF_ATYPE( NDF1, 'CENTRE,WIDTH', AXIS, ATYPE,
     :                         STATUS )
            END IF

*        Check status.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               CALL ERR_REP(
     :            'SPD_CZPF_ERR', 'RESAMP: Error getting ' //
     :            'data type information.', STATUS )
               GO TO 500
            END IF

*        Each type is at least _REAL, possibly _DOUBLE.
            IF ( DTYPE .NE. '_REAL' .AND. DTYPE .NE. '_DOUBLE' )
     :         DTYPE = '_REAL'
            IF ( ATYPE .NE. '_REAL' .AND. ATYPE .NE. '_DOUBLE' )
     :         ATYPE = '_REAL'

*        MTYPE is _DOUBLE if either of the others is _DOUBLE.
            IF ( DTYPE .EQ. '_DOUBLE' .OR. ATYPE .EQ. '_DOUBLE' ) THEN
               MTYPE = '_DOUBLE'
            ELSE
               MTYPE = '_REAL'
            END IF

*     Else if INFO (and not first input), check labels and units.
*     Issue warning only.
         ELSE IF ( INFO ) THEN
            IF ( TDLAB .NE. DLABEL )  CALL MSG_OUT( 'SPD_CZPF_DEVLAB',
     :            'Deviating data label: ' // TDLAB,  STATUS )
            IF ( TDUNIT .NE. DUNITS ) CALL MSG_OUT( 'SPD_CZPF_DEVLAB',
     :            'Deviating data unit:  ' // TDUNIT, STATUS )
            IF ( TALAB .NE. ALABEL )  CALL MSG_OUT( 'SPD_CZPF_DEVLAB',
     :            'Deviating axis label: ' // TALAB,  STATUS )
            IF ( TAUNIT .NE. AUNITS ) CALL MSG_OUT( 'SPD_CZPF_DEVLAB',
     :            'Deviating axis unit:  ' // TAUNIT, STATUS )
         END IF

*     Map input centre and width.
         CALL SPD_EAEA( NDF1, XLOC, AXIS, 'READ', MTYPE, TALAB, TAUNIT,
     :                  XNK, XNDF1, KMAX, STATUS )
         CALL SPD_EAFA( NDF1, XLOC, AXIS, 'READ', MTYPE,
     :                  WNK, XNDF2, KMAX, STATUS )

*     Map the input data and variances.
         CALL NDF_MAP( NDF1, 'DATA', MTYPE, 'READ',
     :                 INK, IDUMMY, STATUS )
         IF ( VARUSE ) CALL NDF_MAP( NDF1, 'VARIANCE', MTYPE, 'READ',
     :                               VNK, IDUMMY, STATUS )

*     Annul the Extension locator.
         IF ( USEEXT ) CALL DAT_ANNUL( XLOC, STATUS )

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error mapping ' //
     :         'input arrays.', STATUS )
            GO TO 500
         END IF

*     If very first valid input.
         IF ( NVALID .EQ. 1 ) THEN

*        Generate the output NDF and propagate auxiliaries.
*        This includes setting the output pixel positions.
            IF ( MTYPE .EQ. '_DOUBLE' ) THEN
               CALL SPD_CZPED( INFO, PROPCO, KMAX,
     :                         %VAL( CNF_PVAL( XNK ) ), DTYPE,
     :                         ATYPE, NDF1, AXIS, LMAX, XL, NDF2,
     :                         NDFX, MESSAG, STATUS )
            ELSE
               CALL SPD_CZPER( INFO, PROPCO, KMAX,
     :                         %VAL( CNF_PVAL( XNK ) ), DTYPE,
     :                         ATYPE, NDF1, AXIS, LMAX, XL, NDF2,
     :                         NDFX, MESSAG, STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'SPD_CZPF_ERR',
     :            'RESAMP: ' // MESSAG, STATUS )
               GO TO 500
            END IF

*        The axis label and unit have not been propagated.
            IF ( ALABEL .NE. ' ' )
     :         CALL NDF_ACPUT( ALABEL, NDF2, 'LABEL', 1, STATUS )
            IF ( AUNITS .NE. ' ' )
     :         CALL NDF_ACPUT( AUNITS, NDF2, 'UNITS', 1, STATUS )

*        Map the corresponding width array. This is for temporary use by
*        this routine only. Thus it is mapped read-only.
            CALL NDF_AMAP( NDF2, 'Width', 1, MTYPE, 'READ', WL, IDUMMY,
     :         STATUS )

*        Map output values, variances, covariance row-sums.
*        The arrays VEC2 and VEC3 are not called IL and VL because they
*        are used temporarily for other, related vectors.
            CALL NDF_MAP( NDF2, 'Data', MTYPE, 'WRITE', VEC2, LMAX,
     :                    STATUS )
            CALL NDF_MAP( NDF2, 'Variance', MTYPE, 'WRITE', VEC3,
     :                    IDUMMY, STATUS )
            IF ( PROPCO ) CALL NDF_MAP( NDFX, 'Data', MTYPE, 'WRITE',
     :                                  CRSL, IDUMMY, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP(
     :            'SPD_CZPF_ERR', 'RESAMP: Error mapping ' //
     :            'output arrays.', STATUS )
               GO TO 500
            END IF

*        Values and variances are used for adding up contributions to an
*        average. Thus they must be zero-initialised. We prefer to do
*        this explicitly ourselves. The option of mapping WRITE/ZERO
*        creates a BAD_PIXEL flag equal to FALSE along with the array,
*        which is not desired.
            IF ( MTYPE .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAJD( 0D0, 0D0, LMAX, %VAL( CNF_PVAL( VEC2 ) ),
     :                         STATUS )
               CALL SPD_UAAJD( 0D0, 0D0, LMAX, %VAL( CNF_PVAL( VEC3 ) ),
     :                         STATUS )
            ELSE
               CALL SPD_UAAJR( 0E0, 0E0, LMAX, %VAL( CNF_PVAL( VEC2 ) ),
     :                         STATUS )
               CALL SPD_UAAJR( 0E0, 0E0, LMAX, %VAL( CNF_PVAL( VEC3 ) ),
     :                         STATUS )
            END IF

*        Get a workspace NDF for the covariance matrices or variance
*        vectors. These are LMAX by LMAX or LMAX resp.
*        We can use the data and variance components of one NDF.
            IF ( PROPCO ) THEN

*           Since this workspace is rather unsophisticated, we create it
*           as one-dimensional.
               CALL NDF_TEMP( PLACE1, STATUS )
               CALL NDF_NEW( MTYPE, 1, 1, LMAX*LMAX, PLACE1, NDFCOV,
     :                       STATUS )

*           Get workspace for post-resample covariance and output
*           covariance (zero-initialised).
*           The arrays MAT2 and MAT3 are not called CNLM and CLM because
*           they are used temporarily for other, related matrices.
               CALL NDF_MAP( NDFCOV, 'Variance', MTYPE, 'WRITE', MAT2,
     :                       IDUMMY, STATUS )
               CALL NDF_MAP( NDFCOV, 'Data', MTYPE, 'WRITE/ZERO', MAT3,
     :                       IDUMMY, STATUS )
            END IF

*        Get a workspace NDF for the good pixel counter and the
*        resampled data values. These are LMAX in length.
*        We can use the data and variance components of one
*        NDF.
            CALL NDF_TEMP( PLACE2, STATUS )
            CALL NDF_NEW( MTYPE, 1, 1, LMAX, PLACE2, NDFRES, STATUS )
            CALL NDF_STYPE( '_INTEGER', NDFRES, 'Data', STATUS )

*        Get workspace for counter array (zero-initialized) and
*        resampled data.
*        The counter array is needed to work out the output variance
*        if VARUSE is false. It is also needed to know whether an
*        output value or variance should be bad.
            CALL NDF_MAP( NDFRES, 'Data', '_INTEGER', 'WRITE/ZERO',
     :                    VECI, IDUMMY, STATUS )
            CALL NDF_MAP( NDFRES, 'Variance', MTYPE, 'WRITE', INL,
     :                    IDUMMY, STATUS )

*        Check status.
            IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error ' //
     :               'mapping output-related workspaces.', STATUS )
               GO TO 500
            END IF
         END IF

*     Get workspace for overlap matrix.
*     These are the overlaps between pixels in the n-th (NVALID) valid
*     input NDF and pixels in the output NDF. There is no need to
*     zero-initialise this array.
*     Since this workspace NDF is rather unsophisticated, we create it
*     as one-dimensional.
*     We create is only at the beginning. For the second and further
*     input NDFs we just reshape the NDF.
         IF ( NVALID .EQ. 1 ) THEN
            CALL NDF_TEMP( PLACE3, STATUS )
            CALL NDF_NEW( MTYPE, 1, 1, KMAX*LMAX, PLACE3, NDFOVL,
     :                    STATUS )
         ELSE
            CALL NDF_UNMAP( NDFOVL, 'DATA', STATUS )
            CALL NDF_RESET( NDFOVL, '*', STATUS )
            CALL NDF_SBND( 1, 1, KMAX*LMAX, NDFOVL, STATUS )
         END IF
         CALL NDF_MAP( NDFOVL, 'DATA', MTYPE, 'WRITE', MAT1,
     :                 IDUMMY, STATUS )

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error mapping' //
     :         'overlap workspace.', STATUS )
            GO TO 500
         END IF

*     Process the n-th input. If mapped DOUBLE.
         IF ( MTYPE .EQ. '_DOUBLE' ) THEN

*        Calculate the matrix of overlaps between pixels of n-th
*        input and output pixels.
            CALL SPD_WZPCD( INFO, KMAX, LMAX, %VAL( CNF_PVAL( XNK ) ),
     :                      %VAL( CNF_PVAL( WNK ) ),
     :                      %VAL( CNF_PVAL( XL ) ),
     :                      %VAL( CNF_PVAL( WL ) ),
     :                      %VAL( CNF_PVAL( MAT1 ) ), STATUS )

*        Resample data values of n-th input.
*        This also turns the overlaps into resampling coefficients. I.e.
*        the matrix will be set zero where an input intensity is bad,
*        and the matrix will be normalised so that the sum along a row
*        is 1 (or 0).
*        Furthermore, an input value is ignored also if its variance is
*        bad. This is also reflected in the returned coefficient matrix.
            CALL SPD_WZPDD( VARUSE, KMAX, LMAX, %VAL( CNF_PVAL( INK ) ),
     :                      %VAL( CNF_PVAL( VNK ) ),
     :                      %VAL( CNF_PVAL( MAT1 ) ),
     :                      %VAL( CNF_PVAL( INL ) ), STATUS )

*        Post-resample covariance.
*        We don't care about post-resample variance, because it is
*        contained as diagonal in the covariance.
*        If VARUSE is false, this will be known except for a factor V0.
            IF ( PROPCO ) CALL SPD_WZPED( VARUSE, KMAX, LMAX,
     :                                    %VAL( CNF_PVAL( VNK ) ),
     :                                    %VAL( CNF_PVAL( MAT1 ) ),
     :                                    %VAL( CNF_PVAL( MAT2 ) ),
     :                                    STATUS )

*     Process the n-th input. Else (mapped _REAL).
         ELSE

*        Calculate the matrix of overlaps between pixels of n-th
*        input and output pixels.
            CALL SPD_WZPCR( INFO, KMAX, LMAX, %VAL( CNF_PVAL( XNK ) ),
     :                      %VAL( CNF_PVAL( WNK ) ),
     :                      %VAL( CNF_PVAL( XL ) ),
     :                      %VAL( CNF_PVAL( WL ) ),
     :                      %VAL( CNF_PVAL( MAT1 ) ), STATUS )

*        Resample data values of n-th input.
*        This also turns the overlaps into resampling coefficients. I.e.
*        the matrix will be set zero where an input intensity is bad,
*        and the matrix will be normalised so that the sum along a row
*        is 1 (or 0).
*        Furthermore, an input value is ignored also if its variance is
*        bad. This is also reflected in the returned coefficient matrix.
            CALL SPD_WZPDR( VARUSE, KMAX, LMAX, %VAL( CNF_PVAL( INK ) ),
     :                      %VAL( CNF_PVAL( VNK ) ),
     :                      %VAL( CNF_PVAL( MAT1 ) ),
     :                      %VAL( CNF_PVAL( INL ) ), STATUS )

*        Post-resample covariance.
*        We don't care about post-resample variance, because it is
*        contained as diagonal in the covariance.
*        If VARUSE is false, this will be known except for a factor V0.
            IF ( PROPCO ) CALL SPD_WZPER( VARUSE, KMAX, LMAX,
     :                                    %VAL( CNF_PVAL( VNK ) ),
     :                                    %VAL( CNF_PVAL( MAT1 ) ),
     :                                    %VAL( CNF_PVAL( MAT2 ) ),
     :                                    STATUS )
         END IF

*     Check if input data processed ok.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SPD_CZPF_ERR',
     :         'RESAMP: Error resampling input data.', STATUS )
            GO TO 500
         END IF

*     Enter n-th input into the average buffers.
*     This adds up in VECI the good input spectra counter, in VEC2 the
*     resampled intensity weighted with its inverse variance, in VEC3
*     the squared intensity weighted with the
*     inverse variance, and in MAT3 the covariance weighted with the
*     reciprocal of the two related variances (sum_n{C_nlm/C_nll/C_nmm})
         IF ( MTYPE .EQ. '_DOUBLE' ) THEN
            CALL SPD_WZPFD( PROPCO, LMAX, %VAL( CNF_PVAL( INL ) ),
     :                      %VAL( CNF_PVAL( MAT2 ) ),
     :                      %VAL( CNF_PVAL( VECI ) ),
     :                      %VAL( CNF_PVAL( VEC2 ) ),
     :                      %VAL( CNF_PVAL( VEC3 ) ),
     :                      %VAL( CNF_PVAL( MAT3 ) ), STATUS )
         ELSE
            CALL SPD_WZPFR( PROPCO, LMAX, %VAL( CNF_PVAL( INL ) ),
     :                      %VAL( CNF_PVAL( MAT2 ) ),
     :                      %VAL( CNF_PVAL( VECI ) ),
     :                      %VAL( CNF_PVAL( VEC2 ) ),
     :                      %VAL( CNF_PVAL( VEC3 ) ),
     :                      %VAL( CNF_PVAL( MAT3 ) ), STATUS )
         END IF

*     Release input NDFs.
         IF ( XNDF1 .NE. NDF__NOID ) CALL NDF_ANNUL( XNDF1, STATUS )
         IF ( XNDF2 .NE. NDF__NOID ) CALL NDF_ANNUL( XNDF2, STATUS )
         CALL NDF_ANNUL( NDF1, STATUS )

*     Progress report.
         IF ( INFO ) THEN
            CALL MSG_SETI( 'INT', NVALID )
            CALL MSG_OUT( 'SPD_CZPF_NTHIN',
     :         '^INT input NDFs processed so far.', STATUS )
         END IF

*     Next input?
         GO TO 1                 ! End of 'DO WHILE' loop
      END IF

*  Now all input NDFs have been processed. Have to do final processing
*  of output NDF.

*  Release overlap matrix.
      CALL NDF_ANNUL( NDFOVL, STATUS )

*  Final processing of output. If mapped DOUBLE.
      IF ( MTYPE .EQ. '_DOUBLE' ) THEN

*     Get final value from preliminary value (intensity).
*     This does not depend on VARUSE. If VARUSE is false, we will have
*     assumed a globally constant pre-resample variance above. As long
*     as this hypothesis is ok, the scaling error of assuming V0 = 1
*     cancels in the calculation of the intensity.
*     If PROPCO is false, we have only one spectrum in the average and
*     we used weights of 1 in SPD_WZPFD above. So the operation of
*     SPD_WZPGD is different. It must be, because MAT3 is not available.
         CALL SPD_WZPGD( PROPCO, LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                   %VAL( CNF_PVAL( MAT3 ) ),
     :                   %VAL( CNF_PVAL( VEC2 ) ), STATUS )

*     Final variance and covariance processing.
         IF ( PROPCO ) THEN

*        If VARUSE then get final covariance and variance.
*        This will divide each element of MAT3 by the two related
*        diagonal elements. It will also extract the new diagonal into
*        VEC3.
            IF ( VARUSE ) THEN
               NEEDC = .TRUE.
               GOODV = .TRUE.
               CALL SPD_WZPHD( LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                         %VAL( CNF_PVAL( MAT3 ) ),
     :                         %VAL( CNF_PVAL( VEC3 ) ),
     :                         STATUS )

*        Else get final variance and covariance.
*        This routine is more complicated, because it has to work out
*        V0.
            ELSE
               CALL SPD_WZPJD( INFO, LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                         %VAL( CNF_PVAL( VEC2 ) ),
     :                         %VAL( CNF_PVAL( VEC3 ) ),
     :                         %VAL( CNF_PVAL( MAT3 ) ), NEEDC, GOODV,
     :                         STATUS )
            END IF

*        Add up the covariance row sums.
            IF ( NEEDC ) CALL SPD_WZPKD( LMAX,
     :                                   %VAL( CNF_PVAL( MAT3 ) ),
     :                                   %VAL( CNF_PVAL( CRSL ) ),
     :                                   NEEDC, STATUS )
         END IF

*  Final processing of output. Else.
      ELSE

*     Get final value from preliminary value.
         CALL SPD_WZPGR( PROPCO, LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                   %VAL( CNF_PVAL( MAT3 ) ),
     :                   %VAL( CNF_PVAL( VEC2 ) ), STATUS )

*     Final variance and covariance processing.
         IF ( PROPCO ) THEN

*        If VARUSE then get final covariance and variance.
            IF ( VARUSE ) THEN
               NEEDC = .TRUE.
               GOODV = .TRUE.
               CALL SPD_WZPHR( LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                         %VAL( CNF_PVAL( MAT3 ) ),
     :                         %VAL( CNF_PVAL( VEC3 ) ),
     :                         STATUS )

*        Else get final variance and covariance.
            ELSE
               CALL SPD_WZPJR( INFO, LMAX, %VAL( CNF_PVAL( VECI ) ),
     :                         %VAL( CNF_PVAL( VEC2 ) ),
     :                         %VAL( CNF_PVAL( VEC3 ) ),
     :                         %VAL( CNF_PVAL( MAT3 ) ), NEEDC, GOODV,
     :                         STATUS )
            END IF

*        Add up the covariance row sums.
            IF ( NEEDC ) CALL SPD_WZPKR( LMAX,
     :                                   %VAL( CNF_PVAL( MAT3 ) ),
     :                                   %VAL( CNF_PVAL( CRSL ) ),
     :                                   NEEDC, STATUS )
         END IF
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPF_ERR', 'RESAMP: Error in final ' //
     :      'processing of output data.', STATUS )
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE

*  Release covariance matrices, resampled values, counter array.
      IF ( PROPCO ) CALL NDF_ANNUL( NDFCOV, STATUS )
      CALL NDF_ANNUL( NDFRES, STATUS )

*  Tidy up the extension.
      IF ( NEEDC ) THEN

*     Release covariance row sums.
*     (This definitely does not contain bad values.)
         CALL NDF_SBAD( .FALSE., NDFX, 'DATA', STATUS )
         CALL NDF_ANNUL( NDFX, STATUS )
      ELSE

*     Delete covariance row sums.
         IF ( PROPCO ) CALL NDF_DELET( NDFX, STATUS )

*     Delete Specdre Extension if empty.
         CALL NDF_XLOC( NDF2, XNAME, 'UPDATE', XLOC, STATUS )
         CALL DAT_NCOMP( XLOC, IDUMMY, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
         IF ( IDUMMY .LE. 0 ) THEN
            CALL NDF_XDEL( NDF2, XNAME, STATUS )
         END IF
      END IF

*  Tidy up the variance.
*  (It may contain bad values.)
      IF ( GOODV ) THEN
         CALL NDF_SBAD( .TRUE., NDF2, 'VARIANCE', STATUS )
      ELSE
         CALL NDF_UNMAP( NDF2, 'Variance', STATUS )
         CALL NDF_RESET( NDF2, 'Variance', STATUS )
      END IF

*  Release output.
*  (Its data component is definitely still there and may contain bad
*  values.)
      CALL NDF_SBAD( .TRUE., NDF2, 'DATA', STATUS )
      CALL NDF_ANNUL( NDF2, STATUS )

*  If something went wrong, delete output.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_DELET( NDFX, STATUS )
         CALL NDF_DELET( NDF2, STATUS )
      END IF

*  Annul the group of input NDFs. Only GID and -1 are important in this
*  context.
      CALL SPD_CZPD( INFO, VARUSE, GID, NMAX, -1,
     :               NDF1, AXIS, EOF, STATUS )

*  Close NDF.
      CALL NDF_END( STATUS )

*  Return.
      END
