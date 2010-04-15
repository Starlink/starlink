*+  RED4_GET_PREFIX - Get prefix for the specified observation type
      SUBROUTINE RED4_GET_PREFIX( TYPE, OUTPUT, STATUS )
*    Description :
*     Given a file type (I, O, RI, RO, RG, ST, CA, MASK, INDEX, TEMPLATE,
*     NDF_TEMPLATE, DST_TEMPLATE, CONFIG) in TYPE return in OUTPUT the
*     directory this file type should be in, as a logical name terminated by a
*     colon (VMS) or as an environment varible started with a "$" and
*     terminated by a "/".
*    Invocation :
*     CALL RED4_GET_PREFIX( TYPE, OUTPUT, STATUS )
*    Parameters :
*     TYPE  = CHARACTER*(*)( READ )
*         The type of file to be prefixed
*     OUTPUT = CHARACTER*(*)( WRITE )
*         The prefix to be used (e.g. RGDIR: or $RGDIR/)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     A.Bridger (JACH::AB)
*     P.N.Daly (JACH::PND)
*    History :
*      7-Nov-1994: Original version.                          (AB)
*      9-Nov-1994: Extend to include MASK and INDEX           (AB)
*     10-Nov-1994: Further extended to *TEMPLATE and CONFIG   (AB)
*     19-Jan-1994: Add ENG                                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'
*    Import :
      CHARACTER*(*) TYPE                ! type of file
*    Export:
      CHARACTER*(*) OUTPUT              ! prefix to apply
*    Status :
      INTEGER STATUS
*-

*    Check for error on entry.
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Determine the prefix according to the type of file
      IF ( TYPE.EQ.'I' ) THEN
        OUTPUT = PREFIX // 'IDIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'O' ) THEN
        OUTPUT = PREFIX // 'ODIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'RI' ) THEN
        OUTPUT = PREFIX // 'RIDIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'RO' ) THEN
        OUTPUT = PREFIX // 'RODIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'RG' ) THEN
        OUTPUT = PREFIX // 'RGDIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'ST' ) THEN
        OUTPUT = PREFIX // 'RGDIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'CA' ) THEN
        OUTPUT = PREFIX // 'RODIR' // SEPARATOR
      ELSE IF ( TYPE.EQ.'MASK' ) THEN
        OUTPUT = PREFIX // 'CGS4_MASKS' // SEPARATOR
      ELSE IF ( TYPE.EQ.'INDEX' ) THEN
        OUTPUT = PREFIX // 'CGS4_INDEX' // SEPARATOR
      ELSE IF ( TYPE.EQ.'NDF_TEMPLATE' ) THEN
        OUTPUT = PREFIX // 'CGS4_NDF_TEMPLATES' // SEPARATOR
      ELSE IF ( TYPE.EQ.'DST_TEMPLATE' ) THEN
        OUTPUT = PREFIX // 'CGS4_DST_TEMPLATES' // SEPARATOR
      ELSE IF ( TYPE.EQ.'CONFIG' ) THEN
        OUTPUT = PREFIX // 'CGS4_CONFIG' // SEPARATOR
      ELSE IF ( TYPE.EQ.'ENG' ) THEN
        OUTPUT = PREFIX // 'CGS4_ENG' // SEPARATOR
      ENDIF
      CALL CHR_RMBLK( OUTPUT )
      END
