*+  USI0_OUTLOG - Write records to log file for current context
      SUBROUTINE USI0_OUTLOG( STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Functions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*80	        BUF			! Output buffer
      CHARACTER*1		LC			! Line continuation

      INTEGER			BPOS			! Character index
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check we are logging
      IF ( USI_LOGGING ) THEN

*      New error context
        CALL ERR_MARK

*      Switch on mode
        IF ( USI_LOGIMODE .EQ. USI__L_ICL ) THEN
          LC = '~'

        ELSE IF ( USI_LOGIMODE .EQ. USI__L_CSH ) THEN
          LC = CHAR(92)

        ELSE IF ( USI_LOGIMODE .EQ. USI__L_CALL ) THEN

        END IF

*      Command line only
        IF ( USI_CTX(USI_ICTX).PLEN .EQ. 0 ) THEN

          IF ( USI_LOGIMODE .EQ. USI__L_CALL ) THEN
            BUF = '      CALL AST_EXEC( '''
            BPOS = CHR_LEN(BUF) + 1
            CALL USI0_OUTLOG_F77( BUF, BPOS, USI_CTX(USI_ICTX).
     :              CSTRING(1:USI_CTX(USI_ICTX).CLEN), .FALSE.,
     :                            STATUS )
            CALL USI0_OUTLOG_F77( BUF, BPOS, ''', STATUS )', .TRUE.,
     :                            STATUS )

          ELSE
            CALL FIO_WRITE( USI_LOGFID,
     :            USI_CTX(USI_ICTX).CSTRING(:USI_CTX(USI_ICTX).CLEN),
     :            STATUS )

          END IF

*      Command line and subsequent parameter accesses
        ELSE

          IF ( USI_LOGIMODE .EQ. USI__L_CALL ) THEN
            BUF = '      CALL AST_EXEC( '''
            BPOS = CHR_LEN(BUF)
            CALL USI0_OUTLOG_F77( BUF, BPOS, USI_CTX(USI_ICTX).
     :                 CSTRING(1:USI_CTX(USI_ICTX).CLEN), .FALSE.,
     :                            STATUS )
            CALL USI0_OUTLOG_F77( BUF, BPOS, ' '//USI_CTX(USI_ICTX).
     :           PSTRING(1:USI_CTX(USI_ICTX).PLEN), .FALSE.,
     :                            STATUS )
            CALL USI0_OUTLOG_F77( BUF, BPOS, ''', STATUS )', .TRUE.,
     :                            STATUS )

          ELSE
            CALL FIO_WRITE( USI_LOGFID,
     :            USI_CTX(USI_ICTX).CSTRING(:USI_CTX(USI_ICTX).CLEN)/
     :                   /' '//LC, STATUS )
            CALL FIO_WRITE( USI_LOGFID,
     :            USI_CTX(USI_ICTX).PSTRING(:USI_CTX(USI_ICTX).PLEN),
     :            STATUS )

          END IF

        END IF

*      Restore error context
        CALL ERR_RLSE

      END IF

      END



*+  USI0_OUTLOG_F77 - Write records to Fortran log file
      SUBROUTINE USI0_OUTLOG_F77( BUF, BPOS, STRING, FLUSH, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Import :
*
      CHARACTER*(*)		STRING			! String to append
      LOGICAL			FLUSH			! Flush last record
*
*    Import / Export :
*
      CHARACTER*(*)		BUF
      INTEGER			BPOS
*
*    Local variables :
*
      INTEGER			ILINE			! Loop over whole lines
      INTEGER			NCUT			! Amount of STR output
      INTEGER			NLINE			! Number of whole lines
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Simply add the string if it will not make the line too long
      IF ( (LEN(STRING)+BPOS) .LE. 71 ) THEN

*      Tack the string on
        BUF(BPOS:) = STRING
        BPOS = BPOS + LEN(STRING)

*    Line too long
      ELSE

*      Fill the remainder of the buffer - allow 3 chars for '//
        NCUT = 71 - BPOS - 3
        BUF(BPOS:) = STRING(:NCUT)//'''//'
        BPOS = BPOS + NCUT + 3
        CALL FIO_WRITE( USI_LOGFID, BUF(:BPOS-1), STATUS )

*      Write out any full continuation lines
        NLINE = (LEN(STRING)-NCUT)/65
        NCUT = NCUT + 1
        DO ILINE = 1, NLINE
          CALL FIO_WRITE( USI_LOGFID, '     : '''//STRING(NCUT:NCUT+64)
     :                    //'''//', STATUS )
          NCUT = NCUT + 65
        END DO

*      Fill buffer with remainder
        BUF = '     : '''//STRING(NCUT:)
        BPOS = 8 + LEN(STRING) - NCUT

      END IF

*    Flush the buffer?
      IF ( FLUSH ) THEN
        CALL FIO_WRITE( USI_LOGFID, BUF(:BPOS-1), STATUS )
      END IF

      END
