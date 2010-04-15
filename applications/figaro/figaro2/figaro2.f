      SUBROUTINE FIGARO2( STATUS )
*+
*  Name:
*     FIGARO2

*  Purpose:
*     Top-level ADAM monolith routine for the FIGARO package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIGARO2( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     ACD: Clive Davenhall (UoE, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (HME):
*        Original version. SUBSET only. (NFIGARO.)
*     26-JUN-1991 (HME):
*        SPECFIT inserted.
*     28-JUN-1991 (HME):
*        EXTRACT inserted. Monolith called SPECDRE now.
*     5-JUL-1991 (HME):
*        ASCIN, ASCOUT, BBODY, GROW inserted.
*     24-JUL-1991 (HME):
*        CORREL, GOODVAR inserted.
*     13-SEP-1991 (HME):
*        Access the essential ELSE-IF structure from IFBLOCK.FOR.
*     15-JUL-1992 (HME):
*        Get 0-th argument (name of the executable, or rather the link
*        used) and extract the action from it.
*     18-AUG-1992 (HME):
*        Adapt from Specdre to Figaro.
*     23-NOV-1992 (HME):
*        Change PAR_INIT call to set batch flag false.
*        Use INDEX and ICH_FOLD rather than CHR_DELIM and CHR_UCASE.
*     12-MAR-1993 (HME):
*        Now find out batch flag from environment variable FIGARO_MODE.
*     14-JAN-1994 (HME):
*        Split monolith into three. Use get_task_name.
*     25-JUL-1995 (HME):
*        Add FITSET application.
*     30-OCT-2001 (ACD):
*        Add IMPOS application.
*     24-APR-2006 (TIMJ):
*        Force inclusion of DSA block data
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Standard PAR constants

*  External Block Data:
      EXTERNAL DSA_BLOCK

*  Arguments Given and Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BATCH
      INTEGER IGNORE
      CHARACTER * ( PAR__SZNAM ) ACTION
      CHARACTER * ( 8 ) ENVVAR

*  Internal References:
      INTEGER ICH_FOLD

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )
      IGNORE = ICH_FOLD( ACTION )

*  Find out about the batch mode.
      CALL PSX_GETENV( 'FIGARO_MODE', ENVVAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         BATCH = .FALSE.
      ELSE
         IGNORE = ICH_FOLD( ENVVAR )
         IF ( ENVVAR .EQ. 'BATCH' ) THEN
            BATCH = .TRUE.
         ELSE
            BATCH = .FALSE.
         END IF
      END IF

*  Initialise the (F)PAR common block.
      CALL PAR_INIT( ACTION, ' ', 0, BATCH, IGNORE )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF (ACTION.EQ.'ABCONV') THEN
         CALL ABCONV
      ELSE IF (ACTION.EQ.'ABLINE') THEN
         CALL ABLINE
      ELSE IF (ACTION.EQ.'ALASIN') THEN
         CALL ALASIN
      ELSE IF (ACTION.EQ.'ALASOUT') THEN
         CALL ALASOUT
      ELSE IF (ACTION.EQ.'APERTURE') THEN
         CALL APERTURE
      ELSE IF (ACTION.EQ.'BFFT') THEN
         CALL FFT
      ELSE IF (ACTION.EQ.'BSMULT') THEN
         CALL BSMULT
      ELSE IF (ACTION.EQ.'CALDIV') THEN
         CALL CALDIV
      ELSE IF (ACTION.EQ.'CENTERS') THEN
         CALL CENTERS
      ELSE IF (ACTION.EQ.'CMPLX2I') THEN
         CALL CMPLX2R
      ELSE IF (ACTION.EQ.'CMPLX2M') THEN
         CALL CMPLX2R
      ELSE IF (ACTION.EQ.'CMPLX2R') THEN
         CALL CMPLX2R
      ELSE IF (ACTION.EQ.'CMPLXADD') THEN
         CALL CMPLXADD
      ELSE IF (ACTION.EQ.'CMPLXCONJ') THEN
         CALL CMPLXCONJ
      ELSE IF (ACTION.EQ.'CMPLXDIV') THEN
         CALL CMPLXADD
      ELSE IF (ACTION.EQ.'CMPLXFILT') THEN
         CALL CMPLXFILT
      ELSE IF (ACTION.EQ.'CMPLXMULT') THEN
         CALL CMPLXADD
      ELSE IF (ACTION.EQ.'CMPLXSUB') THEN
         CALL CMPLXADD
      ELSE IF (ACTION.EQ.'COSBELL') THEN
         CALL COSBELL
      ELSE IF (ACTION.EQ.'CSPIKE') THEN
         CALL CSPIKE
      ELSE IF (ACTION.EQ.'EMLT') THEN
         CALL EMLT
      ELSE IF (ACTION.EQ.'EXTIN') THEN
         CALL EXTIN
      ELSE IF (ACTION.EQ.'FET321') THEN
         CALL FET321
      ELSE IF (ACTION.EQ.'FF') THEN
         CALL FF
      ELSE IF (ACTION.EQ.'FFCROSS') THEN
         CALL FFCROSS
      ELSE IF (ACTION.EQ.'FFT') THEN
         CALL FFT
      ELSE IF (ACTION.EQ.'FIGS321') THEN
         CALL FIGS32N
      ELSE IF (ACTION.EQ.'FIGS322') THEN
         CALL FIGS32N
      ELSE IF (ACTION.EQ.'FIGS422') THEN
         CALL FIGS422
      ELSE IF (ACTION.EQ.'FIGS423') THEN
         CALL FIGS423
      ELSE IF (ACTION.EQ.'FIGS424') THEN
         CALL FIGS424
      ELSE IF (ACTION.EQ.'FIGSEE') THEN
         CALL FIGSEE
      ELSE IF (ACTION.EQ.'FIGSFLUX') THEN
         CALL FIGSFLUX
      ELSE IF (ACTION.EQ.'FITSET') THEN
         CALL FITSET
      ELSE IF (ACTION.EQ.'FITSKEYS') THEN
         CALL FITSKEYS
      ELSE IF (ACTION.EQ.'FLCONV') THEN
         CALL ABCONV
      ELSE IF (ACTION.EQ.'FOTO') THEN
         CALL FOTO
      ELSE IF (ACTION.EQ.'FWCONV') THEN
         CALL FWCONV
      ELSE IF (ACTION.EQ.'GAUSS') THEN
         CALL GAUSS
      ELSE IF (ACTION.EQ.'GSPIKE') THEN
         CALL GSPIKE
      ELSE IF (ACTION.EQ.'I2CMPLX') THEN
         CALL I2CMPLX
      ELSE IF (ACTION.EQ.'IMPOS') THEN
         CALL IMPOS (STATUS)
      ELSE IF (ACTION.EQ.'INTERP') THEN
         CALL INTERP
      ELSE IF (ACTION.EQ.'IRCONV') THEN
         CALL ABCONV
      ELSE IF (ACTION.EQ.'IRFLAT') THEN
         CALL IRFLAT
      ELSE IF (ACTION.EQ.'IRFLUX') THEN
         CALL IRFLUX
      ELSE IF (ACTION.EQ.'LINTERP') THEN
         CALL INTERP
      ELSE IF (ACTION.EQ.'MASK') THEN
         CALL MASK
      ELSE IF (ACTION.EQ.'MCFIT') THEN
         CALL MCFIT
      ELSE IF (ACTION.EQ.'PEAK') THEN
         CALL PEAK
      ELSE IF (ACTION.EQ.'R2CMPLX') THEN
         CALL R2CMPLX
      ELSE IF (ACTION.EQ.'RCGS2') THEN
         CALL RCGS2
      ELSE IF (ACTION.EQ.'RDFITS') THEN
         CALL RDFITS
      ELSE IF (ACTION.EQ.'RDIPSO') THEN
         CALL RDIPSO
      ELSE IF (ACTION.EQ.'SPFLUX') THEN
         CALL SPFLUX
      ELSE IF (ACTION.EQ.'SPIED') THEN
         CALL SPIED
      ELSE IF (ACTION.EQ.'SPIFIT') THEN
         CALL INTERP
      ELSE IF (ACTION.EQ.'TABLE') THEN
         CALL TABLE
      ELSE IF (ACTION.EQ.'WDFITS') THEN
         CALL WIFITS
      ELSE IF (ACTION.EQ.'WDIPSO') THEN
         CALL WDIPSO
      ELSE
         CALL FIG_HELP( 'diversion', STATUS )
      END IF

      END
