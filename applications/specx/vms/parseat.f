      SUBROUTINE PARSEAT( FILNAM )
*+
*  Name:
*     PARSEAT

*  Purpose:
*     Translate @ file name according to environment variables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PARSEAT( FILNAM )

*  Description:
*     This routine takes a file name and translates any leading
*     environment variable. Thus $SOME_DIR/some_file.spx might be
*     translated into /home/user/some_dir/some_file.spx.
*
*     An internal buffer is used, its length of 256 characters limits
*     the used length of FILNAM that can be processed successfully.
*
*     No precautions are taken or warnings given if the translation
*     fails. In case of failure FILNAM is left unchanged.

*  Arguments:
*     FILNAM = CHARACTER * ( * ) (Given and Returned)
*        The filename, translated in situ. It must be long enough to
*        hold the translated name.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JACH)
*     {enter_new_authors_here}

*  History:
*     07 Feb 1994 (hme):
*        Original version.
*     14 Dec 1999 (timj):
*        Note that this is unix version
*     {enter_further_changes_here}

*  Notes:
*     This routine will not work on VMS. Use the parseat.f_vms routine
*     for that purpose (environment variables are different to VMS symbols).

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * ( * ) FILNAM

*  Local Variables:
      INTEGER STATUS
      INTEGER ENVLEN
      INTEGER FILLEN
      INTEGER TRNLEN
      CHARACTER * ( 256 ) WORK

*  Internal References:
      INTEGER GEN_ILEN

*.

      ENVLEN = INDEX( FILNAM, '/' )
      IF ( ENVLEN .GT. 2 .AND. FILNAM(:1) .EQ. '$' ) THEN
         FILLEN = GEN_ILEN( FILNAM(ENVLEN:) )
         STATUS = 0
         CALL UTRNLOG( FILNAM(2:ENVLEN-1), WORK, STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            TRNLEN = GEN_ILEN( WORK )
            FILLEN = TRNLEN + FILLEN
            WORK(TRNLEN+1:) = FILNAM(ENVLEN:)
            IF ( FILLEN .LE. LEN(WORK) .AND.
     :           FILLEN .LE. LEN(FILNAM) ) THEN
               FILNAM = WORK
            END IF
         END IF
      END IF

      END
