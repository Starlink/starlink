      SUBROUTINE ECH_PACK_RESULTS( AAA_INDEX, SUPPLIED_AAA, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_PACK_RESULTS

*  Purpose:
*     Invoke systems level procedure to request-for-archive.

*  Description:
*     This routine call the system procedure for sending a request-for-archive
*     message to the central archive site. The message specifies the object,
*     reduction file, and contains the extracted results file.

*  Invocation:
*     CALL ECH_PACK_RESULTS( AAA_INDEX, SUPPLIED_AAA, STATUS )

*  Arguments:
*     AAA_INDEX = LOGICAL (Given and Returned)
*        TRUE if aaa indexing is to be used
*     SUPPLIED_AAA = INTEGER (Given and Returned)
*        AAA category code to use
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions

*  Method:
*     If indexing by object category is enabled then
*       Make sure we have indexing information available
*       If user has supplied a category code via the tunable parameter then
*          Check it is known to us
*       If we don't already have a category code then
*          List known major categories
*          Let user pick one
*          List known sub-categories
*          Let user pick one
*     Endif
*       Tell user what's going on next
*       Construct procedure argument list
*       Pass procedure invocation to SYSTEM hook routine to do the work

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'AAA_COMMON.INC'
      INCLUDE 'ECH_PSX.INC'

*  Arguments Given and Returned:
      INTEGER SUPPLIED_AAA
      LOGICAL AAA_INDEX

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL VALUE

      INTEGER I
      INTEGER II
      LOGICAL FOUND
      INTEGER IACT
      INTEGER MIN_CAT
      INTEGER MAJ_CAT
      INTEGER CAT_INDEX

      CHARACTER*255 MSG_STRING
      CHARACTER*4 CAAA

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL ECH_REPORT ( 0, ' ARCHIVING functions are enabled.' )

*     If indexing by object category is enabled then
      caaa = '0'
      IF ( aaa_index ) THEN

*       Make sure we have indexing information available
        CALL AAA_INDEXING
        found = .FALSE.

*       If user has supplied a category code via the tunable parameter then
        IF ( supplied_aaa .GT. 0 ) THEN
           DO i = 1, aaa_minor_cats
              IF ( aaa_numbers ( i ) .EQ. supplied_aaa ) THEN
                 found = .TRUE.
                 iact = i
              ENDIF
           END DO

*          Check it is known to us
           IF ( found ) THEN
              REPORT_STRING = ' Using AAA category: '//
     :              AAA_MINORCAT_DESCRIP( IACT )
              CALL ECH_REPORT( 0, REPORT_STRING )
           ELSE
              CALL ECH_REPORT( 0, ' Unknown AAA category provided.' )
           ENDIF
        ENDIF

*       If we don't already have a category code then
        IF ( .NOT. found ) THEN

*          List known major categories
           CALL ECH_REPORT ( 0, ' ' )
           CALL ECH_REPORT ( 0,
     :          ' The following major categories are defined: ' )
           CALL ECH_REPORT ( 0, ' ' )
           DO i = 1, aaa_main_cats
              WRITE ( report_string, 1000 ) i,
     :                                  aaa_maincat_descrip ( i )
              CALL ECH_REPORT ( 0, report_string )
           END DO
           CALL ECH_REPORT ( 0, ' ' )


*          Let user pick one
           value = 1.
           maj_cat = 0
           DO WHILE ( maj_cat .EQ. 0 )
             CALL ECH_REPORT( 0,
     :    ' Please enter the most relevant category for your object' )
             CALL ECH_GET_PARAMETER(
     :            'INSTANT-PROMPT=Category number',
     :            'INT', value, .FALSE., ' ', 0, status )
             IF ( INT( value ) .GT. 0 .AND. INT( value ) .LE.
     :            aaa_main_cats ) maj_cat = INT( value )
           END DO


*          List known sub-categories
           REPORT_STRING = ' For category: ' //
     :           AAA_MAINCAT_DESCRIP( MAJ_CAT )
           CALL ECH_REPORT( 0, REPORT_STRING )
           CALL ECH_REPORT( 0,
     :          ' The following sub-categories are defined: ' )
           CALL ECH_REPORT( 0, ' ' )
           value = 1.
           ii = 0
           DO i = 1, aaa_minor_cats
              IF ( aaa_minor_class(i) .EQ. maj_cat ) THEN
                 ii = ii + 1
                 WRITE ( report_string, 1000 ) ii,
     :                                  aaa_minorcat_descrip ( i )
                 CALL ECH_REPORT ( 0, report_string )
              ENDIF
           END DO
           CALL ECH_REPORT ( 0, ' ' )

*          Let user pick one
           min_cat = 0
           value = 1.
           DO WHILE ( min_cat .EQ. 0 )
             CALL ECH_REPORT ( 0,
     : ' Please enter the most relevant sub-category for your object' )

             CALL ECH_GET_PARAMETER(
     :            'INSTANT-PROMPT=Sub-category number',
     :            'INT', value, .FALSE., ' ', 0, status )
             IF ( INT( value ) .GT. 0 .AND. INT( value ) .LE. ii )
     :            min_cat = INT( value )
           END DO

           ii = 0
           DO i = 1, aaa_minor_cats
              IF ( aaa_minor_class(i) .EQ. maj_cat ) THEN
                 ii = ii + 1
                 IF ( ii .EQ. min_cat ) THEN
                     supplied_aaa = aaa_numbers(i)
                     cat_index = i
                 ENDIF
              ENDIF
           END DO
           CALL ECH_REPORT( 0, ' ' )
           REPORT_STRING = ' ' //
     :           AAA_MINORCAT_DESCRIP( CAT_INDEX ) // ' SELECTED'
           CALL ECH_REPORT( 0, REPORT_STRING )
           CALL ECH_REPORT ( 0, ' ' )
           WRITE ( report_string, 1001 ) supplied_aaa
           CALL ECH_REPORT ( 0, report_string )
        ENDIF

        WRITE ( caaa(1:4), '(I4)' ) supplied_aaa
      ENDIF

*       Tell user what's going on next
      REPORT_STRING = ' Creating job to Package ' //
     :     'results file for addition to archive.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*       Construct procedure argument list
      IF ( SYSNAME .EQ. 'VMS' ) THEN
          msg_string = 'SUBMIT/NOLOG/AFTER="+00:02:00"/PARAM=('//
     :               rdctn_file_main ( 1 :
     :                    CHR_LEN ( rdctn_file_main ) ) // ',' //
     :               rdctn_file_rducd ( 1 :
     :                    CHR_LEN ( rdctn_file_rducd ) ) // ',' //
     :               cstr_rdctn_inptim ( 1 :
     :                    CHR_LEN ( cstr_rdctn_inptim ) ) // ','//
     :               caaa // ') ' //
     :                    'ECHOMOP_SOURCE:ECH_PACK_RESULTS'
      ELSE
          msg_string =
     :                  'sh $ECHOMOP_SOURCE/ech_toarchive ' //
     :               rdctn_file_main ( 1 :
     :                    CHR_LEN ( rdctn_file_main ) ) // ' ' //
     :               rdctn_file_rducd ( 1 :
     :                    CHR_LEN ( rdctn_file_rducd ) ) // ' ' //
     :               cstr_rdctn_inptim ( 1 :
     :                    CHR_LEN ( cstr_rdctn_inptim ) ) // ' '
      ENDIF

*  Pass procedure invocation to SYSTEM hook routine to do the work.
      CALL ECH_SYSTEM( msg_string, status )
      CALL ECH_REPORT( 0,
     :       ' An archiving request has now been created.' )

 1000 FORMAT ( 1X, I2, 2X, A )
 1001 FORMAT ( 1X, 'Setting TUNE_AAACODE =',I4,
     :' would achieve the same categorisation automatically.' )

      END
