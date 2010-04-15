      SUBROUTINE FIGARO1( STATUS )
*+
*  Name:
*     FIGARO1

*  Purpose:
*     Top-level ADAM monolith routine for the FIGARO package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIGARO1( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME:  Horst Meyerdierks (UoE, Starlink)
*     MJCL: Martin Clayton (Starlink, UCL)
*     MBT:  Mark Taylor (Starlink, IoA)
*     ACD:  Clive Davenhall (Starlink, Edinburgh)
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
*     20-JUL-1995 (HME):
*        Add YC* applications.
*     26-JUL-1995 (HME):
*        Add EXTLIST, FIGINFO, FLAG2QUAL, IGCONV, MEDFILTR, QUAL2FLAG,
*        RESAMPLE applications.
*        Also prepare for SURFIT call.
*     29-MAY-1997 (MJCL):
*        Add IALOG application.
*     9-NOV-1998 (MBT, ACD)
*        Add SCLEAN.
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
      IF (ACTION.EQ.'ADJOIN') THEN
         CALL ADJOIN
      ELSE IF (ACTION.EQ.'BCLEAN') THEN
         CALL BCLEAN
      ELSE IF (ACTION.EQ.'CCDLIN') THEN
         CALL CCDLIN
      ELSE IF (ACTION.EQ.'CCUR') THEN
         CALL CCUR
      ELSE IF (ACTION.EQ.'CFIT') THEN
         CALL CFIT
      ELSE IF (ACTION.EQ.'CLEAN') THEN
         CALL CLEAN
      ELSE IF (ACTION.EQ.'CLIP') THEN
         CALL CLIP
      ELSE IF (ACTION.EQ.'COADD') THEN
         CALL COADD
      ELSE IF (ACTION.EQ.'COLOUR') THEN
         CALL COLOUR
      ELSE IF (ACTION.EQ.'COMBINE') THEN
         CALL COMBINE
      ELSE IF (ACTION.EQ.'COPOBJ') THEN
         CALL COPOBJ(STATUS)
      ELSE IF (ACTION.EQ.'COSREJ') THEN
         CALL COSREJ
      ELSE IF (ACTION.EQ.'CREOBJ') THEN
         CALL CREOBJ(STATUS)
      ELSE IF (ACTION.EQ.'CSET') THEN
         CALL CSET
      ELSE IF (ACTION.EQ.'DELOBJ') THEN
         CALL DELOBJ(STATUS)
      ELSE IF (ACTION.EQ.'DVDPLOT') THEN
         CALL DVDPLOT
      ELSE IF (ACTION.EQ.'ELSPLOT') THEN
         CALL LSPLOT
      ELSE IF (ACTION.EQ.'ERRCON') THEN
         CALL ERRCON
      ELSE IF (ACTION.EQ.'ESPLOT') THEN
         CALL SPLOT
      ELSE IF (ACTION.EQ.'EXTRACT') THEN
         CALL EXTRACT
      ELSE IF (ACTION.EQ.'EXTLIST') THEN
         CALL EXTLIST
      ELSE IF (ACTION.EQ.'FIGINFO') THEN
         CALL FIGINFO
      ELSE IF (ACTION.EQ.'FLAG2QUAL') THEN
         CALL FLAG2QUAL
      ELSE IF (ACTION.EQ.'FSCRUNCH') THEN
         CALL FSCRUNCH
      ELSE IF (ACTION.EQ.'GOODVAR') THEN
         CALL GOODVAR(STATUS)
      ELSE IF (ACTION.EQ.'GROWX') THEN
         CALL GROW
      ELSE IF (ACTION.EQ.'GROWXT') THEN
         CALL GROW3
      ELSE IF (ACTION.EQ.'GROWXY') THEN
         CALL GROW3
      ELSE IF (ACTION.EQ.'GROWY') THEN
         CALL GROW
      ELSE IF (ACTION.EQ.'GROWYT') THEN
         CALL GROW3
      ELSE IF (ACTION.EQ.'HARD') THEN
         CALL SOFT
      ELSE IF (ACTION.EQ.'HCROSS') THEN
         CALL HCROSS
      ELSE IF (ACTION.EQ.'HIST') THEN
         CALL HIST
      ELSE IF (ACTION.EQ.'HOPT') THEN
         CALL HOPT
      ELSE IF (ACTION.EQ.'IADD') THEN
         CALL IADSUB
      ELSE IF (ACTION.EQ.'IALOG') THEN
         CALL ILOG
      ELSE IF (ACTION.EQ.'ICADD') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'ICDIV') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'ICMULT') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'ICONT') THEN
         CALL ICONT
      ELSE IF (ACTION.EQ.'ICONV3') THEN
         CALL ICONV3
      ELSE IF (ACTION.EQ.'ICOR16') THEN
         CALL ICOR16
      ELSE IF (ACTION.EQ.'ICSET') THEN
         CALL ICSET
      ELSE IF (ACTION.EQ.'ICSUB') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'ICUR') THEN
         CALL ICUR
      ELSE IF (ACTION.EQ.'IDEV') THEN
         CALL SOFT
      ELSE IF (ACTION.EQ.'IDIFF') THEN
         CALL IDIFF
      ELSE IF (ACTION.EQ.'IDIV') THEN
         CALL IADSUB
      ELSE IF (ACTION.EQ.'IGCONV') THEN
         CALL IGCONV
      ELSE IF (ACTION.EQ.'IGCUR') THEN
         CALL IGCUR
      ELSE IF (ACTION.EQ.'IGREY') THEN
         CALL ICONT
      ELSE IF (ACTION.EQ.'ILIST') THEN
         CALL ILIST
      ELSE IF (ACTION.EQ.'ILOG') THEN
         CALL ILOG
      ELSE IF (ACTION.EQ.'IMAGE') THEN
         CALL IMAGE
      ELSE IF (ACTION.EQ.'IMULT') THEN
         CALL IADSUB
      ELSE IF (ACTION.EQ.'IPLOTS') THEN
         CALL IPLOTS
      ELSE IF (ACTION.EQ.'IPOWER') THEN
         CALL IPOWER
      ELSE IF (ACTION.EQ.'IREVX') THEN
         CALL IREV
      ELSE IF (ACTION.EQ.'IREVY') THEN
         CALL IREV
      ELSE IF (ACTION.EQ.'IROT90') THEN
         CALL IROT90
      ELSE IF (ACTION.EQ.'ISEDIT') THEN
         CALL ISEDIT
      ELSE IF (ACTION.EQ.'ISHIFT') THEN
         CALL ISHIFT
      ELSE IF (ACTION.EQ.'ISMOOTH') THEN
         CALL ISMOOTH
      ELSE IF (ACTION.EQ.'ISPLOT') THEN
         CALL ISPLOT
      ELSE IF (ACTION.EQ.'ISTAT') THEN
         CALL ISTAT
      ELSE IF (ACTION.EQ.'ISTRETCH') THEN
         CALL ISTRETCH
      ELSE IF (ACTION.EQ.'ISUB') THEN
         CALL IADSUB
      ELSE IF (ACTION.EQ.'ISUBSET') THEN
         CALL ISUBSET
      ELSE IF (ACTION.EQ.'ISUPER') THEN
         CALL ISUPER
      ELSE IF (ACTION.EQ.'ISXADD') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISXDIV') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISXMUL') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISXSUB') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISYADD') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISYDIV') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISYMUL') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'ISYSUB') THEN
         CALL ISOPS
      ELSE IF (ACTION.EQ.'IXSMOOTH') THEN
         CALL IXSMOOTH
      ELSE IF (ACTION.EQ.'LSPLOT') THEN
         CALL LSPLOT
      ELSE IF (ACTION.EQ.'LXSET') THEN
         CALL LSET
      ELSE IF (ACTION.EQ.'LYSET') THEN
         CALL LSET
      ELSE IF (ACTION.EQ.'MEDFILT') THEN
         CALL MEDFILT
      ELSE IF (ACTION.EQ.'MEDFILTR') THEN
         CALL MEDFILT
      ELSE IF (ACTION.EQ.'MEDSKY') THEN
         CALL MEDSKY
      ELSE IF (ACTION.EQ.'MSPLOT') THEN
         CALL MSPLOT
      ELSE IF (ACTION.EQ.'NCSET') THEN
         CALL NCSET
      ELSE IF (ACTION.EQ.'OPTEXTRACT') THEN
         CALL OPTEXTRACT
      ELSE IF (ACTION.EQ.'POLYSKY') THEN
         CALL POLYSKY
      ELSE IF (ACTION.EQ.'PROFILE') THEN
         CALL PROFILE
      ELSE IF (ACTION.EQ.'Q2BAD') THEN
         CALL Q2BAD(STATUS)
      ELSE IF (ACTION.EQ.'QUAL2FLAG') THEN
         CALL QUAL2FLAG
      ELSE IF (ACTION.EQ.'REMBAD') THEN
         CALL REMBAD
      ELSE IF (ACTION.EQ.'RENOBJ') THEN
         CALL RENOBJ(STATUS)
      ELSE IF (ACTION.EQ.'RESAMPLE') THEN
         CALL RESAMPLE
      ELSE IF (ACTION.EQ.'RESCALE') THEN
         CALL RESCALE
      ELSE IF (ACTION.EQ.'RETYPE') THEN
         CALL RETYPE
      ELSE IF (ACTION.EQ.'ROTX') THEN
         CALL ROTX
      ELSE IF (ACTION.EQ.'SCLEAN') THEN
         CALL SCLEAN
      ELSE IF (ACTION.EQ.'SCNSKY') THEN
         CALL SCNSKY
      ELSE IF (ACTION.EQ.'SCROSS') THEN
         CALL SCROSS
      ELSE IF (ACTION.EQ.'SCRUNCH') THEN
         CALL SCRUNCH
      ELSE IF (ACTION.EQ.'SETOBJ') THEN
         CALL SETOBJ(STATUS)
      ELSE IF (ACTION.EQ.'SFIT') THEN
         CALL SFIT
      ELSE IF (ACTION.EQ.'SLICE') THEN
         CALL SLICE
      ELSE IF (ACTION.EQ.'SOFT') THEN
         CALL SOFT
      ELSE IF (ACTION.EQ.'SPLOT') THEN
         CALL SPLOT
      ELSE IF (ACTION.EQ.'SQRTERR') THEN
         CALL SQRTERR
*     ELSE IF (ACTION.EQ.'SURFIT') THEN     ! calls NAG
*        CALL SURFIT                        ! calls NAG
      ELSE IF (ACTION.EQ.'TIPPEX') THEN
         CALL TIPPEX
      ELSE IF (ACTION.EQ.'TRIMFILE') THEN
         CALL TRIMFILE(STATUS)
      ELSE IF (ACTION.EQ.'XCADD') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'XCDIV') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'XCMULT') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'XCSUB') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'YCADD') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'YCDIV') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'YCMULT') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'YCSUB') THEN
         CALL ICONST
      ELSE IF (ACTION.EQ.'XCUR') THEN
         CALL XCUR
      ELSE IF (ACTION.EQ.'XTPLANE') THEN
         CALL PLANE
      ELSE IF (ACTION.EQ.'XYPLANE') THEN
         CALL PLANE
      ELSE IF (ACTION.EQ.'YSTRACT') THEN
         CALL YSTRACT
      ELSE IF (ACTION.EQ.'YTPLANE') THEN
         CALL PLANE
      ELSE
         CALL FIG_HELP( 'diversion', STATUS )
      END IF

      END
