      SUBROUTINE LUTSAVE( STATUS )
*+
*  Name:
*     LUTSAVE

*  Purpose:
*     Saves the current colour table of an image-display device in an
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTSAVE( STATUS )

*  Description:
*     This routine reads the colour table of a nominated plane of
*     an IDI-supported image display, such as X-windows, and then copies
*     it to an NDF LUT file.

*  Usage:
*     lutsave lut [device] [plane]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose colour table is to
*        be saved.  The name of the base plane should be given even if
*        the overlay colour table is to be saved.
*        [Current image display]
*     FULL = _LOGICAL (Read)
*        If TRUE the whole colour-table for the device is stored
*        including the reserved pens.  This is necessary to save a
*        colour table written by another package that does not reserve
*        colour indices.  For colour tables produced by KAPPA this
*        should be FALSE. [FALSE]
*     LUT = NDF (Write)
*        The output NDF into which the colour table is to be stored.
*        Its second dimension equals the number of colour-table
*        entries that are stored.  This will be less than the
*        total number of colour indices on the device if FULL is FALSE.
*     PLANE = _INTEGER (Read)
*        The number of the memory plane whose colour table is to be
*        saved.  If it is null the base (image) memory's colour table
*        is reversed.  The base memory is 0 and overlays are numbered
*        consecutively from 1.  For an Ikon the only overlay plane is 1.
*        PLANE is only permitted to have a value in the range 0 to the
*        number of planes minus one. [0]
*     TITLE = LITERAL (Read)
*       The title for the output NDF. ["KAPPA - Lutsave"]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     lutsave pizza
*        This saves the current colour table on the current
*        image-display device to an NDF called pizza.
*     lutsave ramps ikon 1
*        This saves the current colour table on the Ikon overlay plane
*        an NDF called ramps.
*     lutsave redshift full
*        This saves in full the current colour table on the current
*        image-display device to an NDF called redshift.

*  Notes:
*     -  Only Ikons and X-windows are supported.
*     -  Only the non-reserved portion of the colour table is saved.

*  Algorithm:
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Check the attributes of the device, i.e. that it has at least
*        two triggers.
*     -  Obtain the number of memory planes and the maximum number of
*        LUT entries.
*     -  Obtain the plane number whose LUT is to be saved and ascertain
*        whether or not the full LUT is required.
*     -  Read the input LUT.
*     -  Start an NDF context.
*     -  Create the output NDF and map its data.
*     -  Copy the LUT into the NDF's data array
*     -  Close the NDF context
*     -  Annul the device.

*  Related Applications:
*     KAPPA: CRELUT, LUTFLIP, LUTHILITE, LUTREAD, LUTROT, LUTTWEAK.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Apr 13 (MJC):
*        Original version.
*     1991 April 10 (MJC):
*        Modified for colour-table management, specifically the reserved
*        pens are not saved.
*     1991 June 5 (MJC):
*        Altered for new, X-windows, IDI.  Capability 14 is 2**n rather
*        than n.
*     1992 March 26 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 August 5 (MJC):
*        Used new capability 18 to obtain the true number of
*        colour-table entries, which may not be a power of 2, as
*        returned by capability 14.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table-management constants
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'PAR_ERR'          ! Parameter-system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXLUTE
      PARAMETER ( MXLUTE = 1024 )! Maximum number of LUT pens
      INTEGER NDIMS              ! Dimensionality of the LUT
      PARAMETER ( NDIMS = 2 )
      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )
     
*  Local Variables:

      INTEGER
     :  ACTVAL,                  ! Number of values returned
     :  DID,                     ! Display identifier
     :  EL,                      ! Total no. of pixels in output data 
                                 ! array returned by mapping call
     :  DIMS( NDIMS ),           ! LUT dimensions
     :  IDSTAT,                  ! IDI status
     :  IERR,                    ! Index to first error . in VEC_RTOR
                                 ! (not used)
     :  ITTDEP( NMEMAX ),        ! ITT depths of available memories
     :  LBND( NDIMS ),           ! LUT lower bounds
     :  LUTCAP,                  ! Max. depth of LUTS capability code
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
     :  MEMID,                   ! Memory identifier whose LUT is to be
                                 ! rotated
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON                   ! Configuration mode

      INTEGER
     :  NCONF,                   ! Configuration number
     :  NDF,                     ! NDF identifier
     :  NERR,                    ! No. of errors in VEC_RTOR (not used)
     :  NINTS( 1 ),              ! Number of LUT entries
     :  NLUTE,                   ! Actual number of LUT entries to be
                                 ! reversed.
     :  NMEM,                    ! Number of image memories
     :  NTRIG( 1 ),              ! Number of available triggers
     :  NVAL,                    ! Number of values expected
     :  PNTR,                    ! Pointer to mapped data array
     :  TRICAP,                  ! No. of triggers capability code
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YSIZ( NMEMAX )           ! Y sizes of available memories

      REAL
     :  VLUT( 3, MXLUTE )        ! Video lookup table

      LOGICAL                    ! True if:
     :  DEVCAN,                  ! Device is to be cancelled on exit
     :  FULL                     ! Full colour table is to be saved

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Associate the device.
*    =====================
*    
*    Assume that the application will work and the chosen device is
*    to be annulled rather than cancelled upon completion.

      DEVCAN = .FALSE.

*    Just want to rotate what is already there.  It may have
*    been produced by GKS without updating the IDI workstation
*    status table.  Without IDI context, IDI would normally
*    reset the device.  The following call prevents the reset.

      CALL IDI_CLRFG( 1 )

*    Open IDI for a device obtained through the parameter system.

      CALL IDI_ASSOC( 'DEVICE', 'READ', DID, STATUS )

      IF ( STATUS .NE. IDI__OK ) THEN

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Make sure we have selected the base configuration.
*    ==================================================

      NCONF = 0
      CALL IIDSEL( DID, NCONF, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDSEL_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Find the number of image (MEMTYP=1) planes available for the
*    image-display device.  Only NMEM and MEMIDS are required.
*    ============================================================

      MEMTYP = 1
      CALL IIDQDC( DID, NCONF, MEMTYP, NMEMAX, MODCON, MEMIDS, XSIZ,
     :             YSIZ, MEMDEP, ITTDEP, NMEM, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQDC_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Get the memory required within the allowed range.
*    =================================================

      CALL PAR_GDR0I( 'PLANE', 0, 0, NMEM-1, .FALSE., MEMID, STATUS )

*    A null status indicates that the base image plane is to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         MEMID = 0
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 999
      END IF

*    Full colour table required?
*    ===========================

      CALL PAR_GTD0L( 'FULL', .FALSE., .TRUE., FULL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Get the number of triggers.
*    ===========================

      TRICAP = 55
      NVAL = 1
      CALL IIDQCI( DID, TRICAP, NVAL, NTRIG, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Check that the image-display device has at least two triggers.
*    These are numbered 0, 1, 2 etc.
*    ==============================================================

      IF ( NTRIG( 1 ) .LT. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'LUTSAVE_NOCURSOR',
     :     'LUTSAVE: The chosen device has insufficient triggers.',
     :     STATUS )

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Select the current lookup table for the specified memory plane.
*    ===============================================================
*    
*    The use of VLUT = memory identifier is arbitrary.  It does work
*    for GKS, and will be changed once an IDI problem is resolved.

      CALL IIMSLT( DID, MEMID, MEMID, 0, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSLT_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Find the maximum number of lookup-table entries.
*    ================================================

      LUTCAP = 18
      NVAL = 1
      CALL IIDQCI( DID, LUTCAP, NVAL, NINTS, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .FALSE.
         GOTO 999
      END IF

*    Read current colour table.
*    ==========================
*    
*    Read the current LUT, only storing what is possible.  The buffer
*    is generous as most devices have 256 levels.  Colour-table entries 
*    start at 0 if there were no reserved pens, therefore there is no
*    plus-one term.

      IF ( FULL ) THEN
         NLUTE = MIN( NINTS( 1 ), MXLUTE )
         CALL IILRLT( DID, MEMID, 0, NLUTE, VLUT, IDSTAT )
      ELSE
         NLUTE = MIN( NINTS( 1 ), MXLUTE ) - CTM__RSVPN
         CALL IILRLT( DID, MEMID, CTM__RSVPN, NLUTE, VLUT, IDSTAT )
      END IF

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IILRLT_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Store the LUT in an NDF.
*    ========================

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Create the output simple NDF structure.

      LBND( 1 ) = 1
      LBND( 2 ) = 1
      DIMS( 1 ) = 3
      DIMS( 2 ) = NLUTE
      CALL NDF_CREAT( 'LUT', '_REAL', NDIMS, LBND, DIMS, NDF, STATUS )

*    Map the NDF's data component for WRITE access.

      CALL NDF_MAP( NDF, 'DATA', '_REAL', 'WRITE', PNTR, EL, STATUS )

*    Move the contents of the LUT to the NDF data component.

      CALL VEC_RTOR( .FALSE., EL, VLUT, %VAL( PNTR ), IERR, NERR,
     :               STATUS )

*    Unmap the data component.

      CALL NDF_UNMAP( NDF, 'DATA', STATUS )

*    There are no bad pixels in a lookup table, so record this fact in
*    the NDF. 

      CALL NDF_SBAD( .FALSE., NDF, 'DATA', STATUS )


*    Obtain values for the title component of the NDF.

      CALL NDF_CINP( 'TITLE', NDF, 'TITLE', STATUS )

*    End the NDF context.

      CALL NDF_END( STATUS )

*    Closedown sequence.
*    ===================

*    Errors, either obtaining the device or because the device does not
*    support the required capabilities, fall to this point.

 999  CONTINUE

      IF ( DEVCAN ) THEN
      
*       Close down IDI using the parameter system.

         CALL IDI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL IDI_ANNUL( DID, STATUS )
      END IF

      END
