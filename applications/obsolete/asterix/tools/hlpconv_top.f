*+  HLPCONV_TOP - Convert pseudo-HTML to HLP, true HTML or LaTeX
      SUBROUTINE HLPCONV_TOP( HTML, MODE, HLP )
*
*    Description :
*
*     Takes in HTML file, formats text according to recognised tokens, 
*     strips out those which have no effect, and leaves in that information
*     required by HLPREAD to render text correctly (ie. hyperlink info). If
*     any line exceeds 70 characters then it is broken and a continuation
*     character inserted.
*
*    Method :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     16 Nov 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*
*    Global variables :
*
      INCLUDE 'HLPCONV_CMN'
*
*    Import :
*
      CHARACTER*128           	HTML          		! Raw text
      CHARACTER*4		MODE			! Convertor mode
      CHARACTER*128		HLP			! Output root name
*
*
*
      INTEGER			CHR_LEN
      LOGICAL			CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*200		IBUF			! Input buffer
      CHARACTER*132		TREC

      INTEGER			IBUFLEN			! Characters in buffer
      INTEGER			I
      INTEGER			IFD			! Input FIO descriptor
      INTEGER			STATUS			! Inherited status
      INTEGER			TFD			! Tags
*-

*    Start error reporting
      CALL ERR_BEGIN( STATUS )

*    Decide on mode
      IF ( CHR_SIMLR( MODE, 'HLP' ) ) THEN
        IMODE = HCO__HLP
      ELSE IF ( CHR_SIMLR( MODE, 'TEX' ) ) THEN
        IMODE = HCO__TEX
      ELSE IF ( CHR_SIMLR( MODE, 'HTML' ) ) THEN
        IMODE = HCO__HTML
      ELSE
        PRINT *,'Unrecognised convertor mode '//MODE
        GOTO 99
      END IF
     
*    Initialise
      CURIND = 0
      CURLEV = -1
      IN_PREFORMAT = .FALSE.
      IN_PARSECT = .FALSE.
      HTXTP(1) = 1

*    Open the files
      I = CHR_LEN(HLP)
      CALL FIO_OPEN( HTML, 'READ', 'LIST', 0, IFD, STATUS )
      IF ( IMODE .EQ. HCO__HLP ) THEN
        CALL OUTOPEN( HLP(:I), 'hlp', STATUS )
      ELSE IF ( IMODE .EQ. HCO__HTML ) THEN
        FROOT = HLP(:I)
        FRLEN = I
      ELSE IF ( IMODE .EQ. HCO__TEX ) THEN
        CALL OUTOPEN( HLP(:I), 'tex', STATUS )
      END IF

*    Open the tags file
      NTAGS = 0
      IF ( IMODE .EQ. HCO__HLP ) THEN
        CALL FIO_OPEN( HLP(:I)//'.tags', 'WRITE', 'LIST', 0, TFD, 
     :                 STATUS )
      END IF

*    While more input
      DO WHILE ( STATUS .EQ. SAI__OK )

*      Read a line
        CALL FIO_READ( IFD, IBUF, IBUFLEN, STATUS )

*      Convert it
        CALL HLPCONV_OUT( IBUF(:IBUFLEN), STATUS )

      END DO
      IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*    Write tags
      IF ( IMODE .EQ. HCO__HLP ) THEN
        DO I = 1, NTAGS
          WRITE( TREC, '(A40,A)' ) HT_ADDR(I), HT_TEXT(I)
          CALL FIO_WRITE( TFD, TREC(:40+HT_TLEN(I)), STATUS )
        END DO
        CALL FIO_CLOSE( TFD, STATUS )
      END IF

*    Close the files
      CALL FIO_CLOSE( IFD, STATUS )
      DO WHILE ( (CURLEV.GE.0) .AND. (STATUS.EQ.SAI__OK) )
        CALL OUTCLOS( STATUS )
      END DO

*    Flush errors
 99   IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END


*+  HLPCONV_OUT
      SUBROUTINE HLPCONV_OUT( IBUF, STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*)		IBUF			! Input line
*
*    Function declarations :
*
      INTEGER CHR_LEN
*
*    Status :
*
      INTEGER	STATUS
*
*    Global variables :
*
      INCLUDE 'HLPCONV_CMN'
*
*    Local constants :
*
      CHARACTER*1		BS			! The backslash 
c       PARAMETER 		( BS = CHAR(92) )
*
*    Local variables :
*
      CHARACTER*3 		LIC			! Encoded list number
      CHARACTER*6		TAG
      CHARACTER*80		TITLE

      INTEGER 			I,CP,LLIC,L,ITAG,NBLNK,ILEV
      INTEGER                   NEWIND,NEWOBJ
      INTEGER			U1,U2,T1,T2

      LOGICAL			HTML,HLP,TEX
*
*    Local data:
*
      CHARACTER*20		TEXHEADS(5)
      DATA			TEXHEADS/'chapter','section',
     :                        'subsection','subsubsection','paragraph'/
*-

*    Initialise scan over input buffer
      I = 1
      NEWIND = CURIND
      BS = CHAR(92)

*    Mode logicals
      HTML = (IMODE.EQ.HCO__HTML)
      HLP = (IMODE.EQ.HCO__HLP)
      TEX = (IMODE.EQ.HCO__TEX)

*    Leading digit - has to be hierarchy number
      IF ( (IBUF(1:1).GE.'0') .AND. (IBUF(1:1).LE.'9') ) THEN

*      Length of IBUF
        L = CHR_LEN(IBUF)

*      Flush the buffer
        CALL OUTLINE( 0, 1, CURIND, 0, STATUS )

*      Close parameter section?
        IF ( IN_PARSECT ) THEN
          IN_PREFORMAT = .FALSE.
          IN_PARSECT = .FALSE.
          IF ( TEX ) THEN
            CALL TEX_END( 'verbatim' )
          ELSE IF ( HTML ) THEN
            CALL PUTBUF( '</pre>' )
          ELSE
            CALL OUTLINE( 0, 1, CURIND, 0, STATUS )
          END IF
        END IF

*      Adjust maximum output line length to allow HLP system to function
*      correctly with an 80 column display
        ILEV = ICHAR(IBUF(1:1))-ICHAR('0')
        MAXWID = 78 - 2*(ICHAR(IBUF(1:1))-ICHAR('0'))
        NEWOBJ = 1

*      HTML mode
        IF ( HTML ) THEN

*        Close scopes to parent of the file we're about to create
          DO WHILE ( (CURLEV .GE. ILEV) .AND. (STATUS.EQ.SAI__OK) )
            CALL OUTCLOS( STATUS )
          END DO

*        Construct file name
          IBUF = IBUF(3:L)
          L = L - 2

*        Strip trailing dots and translate ampersands
          CALL REM_TDOT( IBUF, L )
          CALL TRN_AMP( IBUF, 'and', L )
          CALL REM_CH( IBUF, '(', L )
          CALL REM_CH( IBUF, ')', L )

*        Get title before we legalise the filename
          TITLE = IBUF
          CALL REM_UND( TITLE(:L) )

*        Open the new file
          IF ( IBUF(:L) .EQ. '*' ) THEN
            CALL OUTOPEN( 'base', 'html', STATUS )
          ELSE

*          Trap trailing asterisks or plus signs
            IF ( (IBUF(L:L) .EQ. '*') .OR. 
     :           (IBUF(L:L) .EQ. '+') ) IBUF(L:L) = '_'

            CALL OUTOPEN( IBUF(:L), 'html', STATUS )

          END IF

*        No sub-topics at this level yet
          FIRSTSUB(CURLEV) = .TRUE.

*        If not base level write hyperlink to parent
          IF ( CURLEV .GT. 0 ) THEN

*          Mark as sub-topic of parent
            IF ( FIRSTSUB(CURLEV-1) ) THEN
              CALL PUTBUFI( '<br>Subtopics:<br><ul>', -1 )
              FIRSTSUB(CURLEV-1) = .FALSE.
            END IF
            CALL PUTBUFI( '<li><a href="', -1 )
            CALL PUTBUFI( IBUF(:L), -1 )
            CALL PUTBUFI( '/">', -1 )

          END IF

*        Write header
          CALL PUTBUF( '<html><head><title>' )
          CALL PUTBUF( TITLE(:L) )
          CALL PUTBUF( '</title></head><body><h1>' )
          CALL PUTBUF( TITLE(:L) )
          CALL PUTBUF( '</h1>' )
          IF ( CURLEV .GT. 0 ) THEN
            CALL PUTBUFI( TITLE(:L)//'</a>', -1 )
          END IF

*      In TEX mode write section header
        ELSE IF ( (ILEV .GT. 0) .AND. TEX ) THEN

          IBUF = IBUF(3:L)
          L = L - 2
          IF ( IBUF(1:1).GT.'0' .AND. IBUF(2:2) .EQ. '_' ) THEN
            IBUF = IBUF(3:L)
            L = L - 2
          END IF
          CALL PUTBUF( BS//TEXHEADS(MIN(ILEV,5))
     :          (:CHR_LEN(TEXHEADS(MIN(ILEV,5)))) )
          CALL REM_UND( IBUF(:L) )
          CP = INDEX(IBUF(:L),'&amp')
          IF ( CP .GT. 0 ) THEN
            IBUF(CP:) = BS//'&'//IBUF(CP+4:)
            L = L - 3
          END IF
          CALL REM_TDOT( IBUF, L )
          CALL PUTBUF( '{' )
          CALL PUTBUF( IBUF(:L) )
          CALL PUTBUF( '}' )

*      Help source mode
        ELSE IF ( HLP ) THEN

          CALL TRN_AMP( IBUF, '&', L )
          CALL PUTBUF( IBUF(:L) )
          IBUF = IBUF(3:L)
          L = L - 2

        END IF

*    Parameters section?
        IF ( IBUF(:L) .EQ. 'Parameters' ) THEN

          IN_PREFORMAT = .TRUE.
          IN_PARSECT = .TRUE.

          IF ( HLP .OR. TEX ) THEN

*          Flush data in buffer
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )

*          Start pre-formatted environment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'verbatim' )
            END IF

          ELSE IF ( HTML ) THEN
            CALL PUTBUF( '<pre>' ) 

          END IF

        END IF

*      This line is now processed
        GOTO 99

*    File inclusion
      ELSE IF ( IBUF(1:1) .EQ. '@' ) THEN

*      Flush the buffer
        CALL OUTLINE( 0, 1, CURIND, 0, STATUS )

*    Starlink help input should have nothing in the first column
      ELSE IF ( HLP ) THEN
        NEWOBJ = 2
        OBUF(CUROLEV)(1:1) = ' '

      END IF
      OBP(CUROLEV) = NEWOBJ

*    Remove leading blanks
      IF ( .NOT. IN_PARSECT ) THEN
        IF ( IN_PREFORMAT ) THEN
          IBUF = IBUF(2:)
        ELSE
          CALL CHR_LDBLK( IBUF )
        END IF
      END IF

*    Length of IBUF
      L = CHR_LEN(IBUF)

*    Look for highlight markers and replace with ANSI escape sequences
      DO WHILE ( I .LE. L )

*      Old style hypertext markers
        IF ( IBUF(I:I+1) .EQ. '-<' ) THEN

          CP = I + INDEX(IBUF(I:),'>-') - 1
          CALL ADDTAG( IBUF(I+2:CP-1), IBUF(I+2:CP-1), ITAG )
          WRITE( TAG, '(A,I4.4)' ) '@@',ITAG
          IF ( HLP ) THEN
            CALL PUTBUF( TAG )
          ELSE
            CALL PUTBUF( HT_TEXT(ITAG)(:HT_TLEN(ITAG)) )
          END IF
          I = CP + 2

*      Start of an HTML tag?
        ELSE IF ( IBUF(I:I) .EQ. '<' ) THEN

          I = I + 1

*        Anchor point?
          IF ( IBUF(I:I+7).EQ.'a href="' ) THEN
            I = I + 8
            CP = INDEX( IBUF(I:), '"' )
            U1 = I 
            U2 = I+CP-2
            I = I + CP
            CP = I + INDEX(IBUF(I:),'</a>') - 1
            T1 = I+1
            T2 = CP - 1
            IF ( HLP .OR. TEX ) THEN
              CALL PUTBUF( IBUF(T1:T2) )
            ELSE IF ( HTML ) THEN
              IF ( IBUF(U1:U1+3) .EQ. 'STAR' ) THEN
                CALL PUTBUF( '<a href="http://star-www.rl.ac.uk' )
                CALL PUTBUF( IBUF(U1+4:U2) )
                CALL PUTBUF( '">' )
                CALL PUTBUF( IBUF(T1:T2) )
                CALL PUTBUF( '</a>' )
              ELSE
                CALL PUTBUF( '<a href="' )
                CALL PUTBUF( IBUF(U1:U2) )
                CALL PUTBUF( '">' )
                CALL PUTBUF( IBUF(T1:T2) )
                CALL PUTBUF( '</a>' )
              END IF
            END IF
            I = CP + 4

*        Begin unordered list?
          ELSE IF ( IBUF(I:I+2) .EQ. 'ul>' ) THEN
            I = I + 3
            HST_L_LEV = HST_L_LEV + 1
            HST_L_FORM(HST_L_LEV) = HTML__UL
            HST_L_CNT(HST_L_LEV) = 0
                              
*          Flush data in buffer
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )

*          Start TEX mode enviroment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'itemize' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<ul>' )
            END IF

*        Begin description list?
          ELSE IF ( IBUF(I:I+2) .EQ. 'dl>' ) THEN
            I = I + 3
            HST_L_LEV = HST_L_LEV + 1
            HST_L_FORM(HST_L_LEV) = HTML__DL
            HST_L_CNT(HST_L_LEV) = 0

*          Flush data in buffer
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 1, STATUS )
                              
*          Start TEX mode enviroment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'description' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<dl>' )
            END IF

*        Begin ordered list?
          ELSE IF ( IBUF(I:I+2) .EQ. 'ol>' ) THEN
            I = I + 3
            HST_L_LEV = HST_L_LEV + 1
            HST_L_FORM(HST_L_LEV) = HTML__OL
            HST_L_CNT(HST_L_LEV) = 0

*          Flush data in buffer
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 1, STATUS )
                               
*          Start TEX mode enviroment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'enumerate' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<ol>' )
            END IF

*        Definition list item 
          ELSE IF ( (IBUF(I:I+2) .EQ. 'dt>') ) THEN
            I = I + 3
            HST_L_CNT(HST_L_LEV) = HST_L_CNT(HST_L_LEV) + 1
            CALL SKIP( IBUF(:L), I )
            IF ( I .LE. L ) THEN

              IF ( HST_L_CNT(HST_L_LEV) .EQ. 1 ) THEN
                NEWIND = CURIND + INDENT__DL
              ELSE
                NEWIND = CURIND
                CURIND = CURIND - INDENT__DL
              END IF
         
*          Start TEX mode enviroment
              IF ( TEX ) THEN
                CALL PUTBUF( BS//'item[' )
                DO CP = I, L
                  IF ( IBUF(CP:CP) .EQ. '_' ) THEN
                    CALL PUTBUF( BS//'_' )
                  ELSE
                    CALL PUTBUF( IBUF(CP:CP) )
                  END IF
                END DO
                CALL PUTBUF( ']' )
                I = L + 1
              ELSE IF ( HTML ) THEN
                CALL PUTBUF( '<dt>' )
              END IF

            END IF
        
*        Definition list definition tags
          ELSE IF ( (IBUF(I:I+2) .EQ. 'dd>') ) THEN
            I = I + 3
            CALL SKIP( IBUF(:L), I )
        
*          Flush data in buffer
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )

            IF ( HTML ) THEN
              CALL PUTBUF( '<dd>' )
            END IF

*        Example start
          ELSE IF ( (IBUF(I:I+4) .EQ. 'exam>') ) THEN
            I = I + 5
            IN_PREFORMAT = .TRUE.
          
*          Flush data in buffer
            NEWIND = NEWIND + 2
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )

*          Start pre-formatted environment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'quote' )
              CALL TEX_BEGIN( 'verbatim' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<blockquote><pre>' )
            END IF
                               
*        Example end
          ELSE IF ( (IBUF(I:I+5) .EQ. '/exam>') ) THEN
            I = I + 6
            NEWIND = NEWIND - 2
          
            IF ( TEX ) THEN
              CALL TEX_END( 'verbatim' )
              CALL TEX_END( 'quote' )
              NBLNK = 0
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '</pre></blockquote>' )
            ELSE
              NBLNK = 1
            END IF

            CALL OUTLINE( 0, NEWOBJ, NEWIND, NBLNK, STATUS )
            IN_PREFORMAT = .FALSE.

*        Preformat start?
          ELSE IF ( IBUF(I:I+3) .EQ. 'pre>' ) THEN
            I = I + 4
            IN_PREFORMAT = .TRUE.
          
*          Start pre-formatted environment
            IF ( TEX ) THEN
              CALL TEX_BEGIN( 'verbatim' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<pre>' )
            END IF
                               
*        Preformat end?
          ELSE IF ( IBUF(I:I+4) .EQ. '/pre>' ) THEN
            I = I + 5
            IN_PREFORMAT = .FALSE.
          
*          End pre-formatted environment
            IF ( TEX ) THEN
              CALL TEX_END( 'verbatim' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '</pre>' )
            END IF

*        Math mode start in TEX mode
          ELSE IF ( TEX .AND. (IBUF(I:I+4) .EQ. 'math>') ) THEN
            I = I + 5
            CALL PUTBUF( '$' )
                                        
*        Math mode end in TEX mode
          ELSE IF ( TEX .AND. (IBUF(I:I+5) .EQ. '/math>') ) THEN
            I = I + 6
            CALL PUTBUF( '$' )
                                        
*        Code start
          ELSE IF ( IBUF(I:I+4) .EQ. 'code>' ) THEN
            I = I + 5

            IF ( TEX ) THEN
              CALL PUTBUF( '{'//BS//'tt ' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<code>' )
            ELSE
              CALL PUTBUF( '''' )
            END IF
                                        
*        Code end
          ELSE IF ( IBUF(I:I+5) .EQ. '/code>' ) THEN
            I = I + 6

            IF ( TEX ) THEN
              CALL PUTBUF( '}' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '</code>' )
            ELSE
              CALL PUTBUF( '''' )
            END IF
                                        
*        Emphasis start
          ELSE IF ( IBUF(I:I+2) .EQ. 'em>' ) THEN
            I = I + 3

            IF ( TEX ) THEN
              CALL PUTBUF( '{'//BS//'em ' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<em>' )
            ELSE
              CALL PUTBUF( '@@B' )
            END IF
                                        
*        Emphasis end
          ELSE IF ( IBUF(I:I+3) .EQ. '/em>' ) THEN
            I = I + 4

            IF ( TEX ) THEN
              CALL PUTBUF( '}' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '</em>' )
            ELSE
              CALL PUTBUF( '@@b' )
            END IF
                                        
*        Keyname start
          ELSE IF ( .NOT. HTML .AND. (IBUF(I:I+3) .EQ. 'key>') ) THEN
            I = I + 4

            IF ( TEX .AND. .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( BS//'verb+<' )
            ELSE
              CALL PUTBUF( '<' )
            END IF
                                        
*        Keyname end
          ELSE IF ( .NOT. HTML .AND. (IBUF(I:I+4) .EQ. '/key>') ) THEN
            I = I + 5

            IF ( TEX .AND. .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( '>+' )
            ELSE
              CALL PUTBUF( '>' )
            END IF
                                        
*        Meta-variable start
          ELSE IF ( IBUF(I:I+3) .EQ. 'var>' ) THEN
            I = I + 4

            IF ( TEX ) THEN
              CALL PUTBUF( '$' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '<var>' )
            ELSE
              CALL PUTBUF( '<' )
            END IF
                                        
*        Meta-variable end
          ELSE IF ( IBUF(I:I+4) .EQ. '/var>' ) THEN
            I = I + 5

            IF ( TEX ) THEN
              CALL PUTBUF( '$' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '</var>' )
            ELSE
              CALL PUTBUF( '>' )
            END IF
                                        
*        Math mode end in TEX mode
          ELSE IF ( TEX .AND. (IBUF(I:I+5) .EQ. '/math>') ) THEN
            I = I + 6
            CALL PUTBUF( '$' )
                                        
*        Cite mode start in TEX mode
          ELSE IF ( TEX .AND. (IBUF(I:I+4) .EQ. 'cite>') ) THEN
            I = I + 5
            CALL PUTBUF( '{'//BS//'em ' )
                                        
*        Cite mode end in TEX mode
          ELSE IF ( TEX .AND. (IBUF(I:I+5) .EQ. '/cite>') ) THEN
            I = I + 6
            CALL PUTBUF( '}' )
                                        
*        Meta-variable start
          ELSE IF ( (IBUF(I:I+3) .EQ. 'var>') ) THEN
            I = I + 4
            IF ( .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( '<' )
            END IF

*        Meta-variable end
          ELSE IF ( (IBUF(I:I+4) .EQ. '/var>') ) THEN
            I = I + 5
            IF ( .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( '>' )
            END IF

*        Line flush
          ELSE IF ( (IBUF(I:I+2) .EQ. 'br>') ) THEN

            I = I + 3

*          Flush data in buffer
            CALL PUTBUF( ' ' )
            CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )

*        End of paragraph
          ELSE IF ( IBUF(I:I+2) .EQ. 'p>' ) THEN

            I = I + 2

*          Flush data in buffer
            IF ( HTML ) THEN
              CALL PUTBUF( '<p>' )
            ELSE
              CALL OUTLINE( 0, NEWOBJ, NEWIND, 1, STATUS )
            END IF

*        Ordered/unordered list item
          ELSE IF ( IBUF(I:I+2) .EQ. 'li>' ) THEN
            I = I + 3

            CALL SKIP( IBUF(:L), I )
            HST_L_CNT(HST_L_LEV) = HST_L_CNT(HST_L_LEV) + 1

            IF ( I .LE. L ) THEN

              IF ( HST_L_FORM(HST_L_LEV) .EQ. HTML__OL ) THEN
                IF ( HST_L_CNT(HST_L_LEV) .EQ. 1 ) THEN
                  NEWIND = CURIND + INDENT__OL
                ELSE
                  NEWIND = CURIND
                  CURIND = CURIND - INDENT__OL
                END IF

                CALL CHR_ITOC( HST_L_CNT(HST_L_LEV), LIC, LLIC )
                LIC(LLIC+1:LLIC+1) = ') '
                LLIC = LLIC + 2

              ELSE

                IF ( HST_L_CNT(HST_L_LEV) .EQ. 1 ) THEN
                  NEWIND = CURIND + INDENT__UL
                ELSE
                  NEWIND = CURIND
                  CURIND = CURIND - INDENT__UL
                END IF

                LIC = '. '
                LLIC = 2

              END IF
         
              IF ( TEX ) THEN
                CALL PUTBUF( BS//'item ' )
              ELSE IF ( HTML ) THEN
                CALL PUTBUF( '<li>' )
              ELSE
                CALL PUTBUF( LIC(:LLIC) )
              END IF

            END IF

*        End list environments?
          ELSE IF ( (IBUF(I:I+3) .EQ. '/ul>') .OR.
     :              (IBUF(I:I+3) .EQ. '/dl>') .OR.
     :              (IBUF(I:I+3) .EQ. '/ol>') ) THEN
            I = I + 4
            IF ( HST_L_FORM(HST_L_LEV).EQ.HTML__DL) THEN
              NEWIND = CURIND - INDENT__DL
              IF ( TEX ) THEN
                CALL TEX_END( 'description' )
              ELSE IF ( HTML ) THEN
                CALL PUTBUF( '</dl>' )
              END IF
            ELSE IF ( HST_L_FORM(HST_L_LEV).EQ.HTML__UL) THEN
              NEWIND = CURIND - INDENT__UL
              IF ( TEX ) THEN
                CALL TEX_END( 'itemize' )
              ELSE IF ( HTML ) THEN
                CALL PUTBUF( '</ul>' )
              END IF
            ELSE IF ( HST_L_FORM(HST_L_LEV).EQ.HTML__OL) THEN
              NEWIND = CURIND - INDENT__OL
              IF ( TEX ) THEN
                CALL TEX_END( 'enumerate' )
              ELSE IF ( HTML ) THEN
                CALL PUTBUF( '</ol>' )
              END IF
            END IF
            HST_L_LEV = HST_L_LEV - 1
               
*        Ignore HTML markers we don't know about
          ELSE
            CP = I + INDEX(IBUF(I:),'>') - 1
            I = CP + 1
                   
          END IF

*      Underscores, precents & dollars in non-verbatim TEX mode must be 
*      preceded by a slash
        ELSE IF ( (.NOT.IN_PREFORMAT) .AND. TEX .AND. 
     :            (INDEX( '_$%', IBUF(I:I) ) .GT. 0) ) THEN
          CALL PUTBUF( BS )
          CALL PUTBUF( IBUF(I:I) )
          I = I + 1

*      Slashes and carats in non-verbatim TEX mode must be converted
        ELSE IF ( (.NOT.IN_PREFORMAT) .AND. TEX .AND. 
     :            ((IBUF(I:I) .EQ. BS) .OR. (IBUF(I:I) .EQ. '^')) ) THEN
          CALL PUTBUF( BS )
          CALL PUTBUF( 'verb+' )
          CALL PUTBUF( IBUF(I:I) )
          CALL PUTBUF( '+' )
          I = I + 1

*      Whitespace - more than one space makes no difference here
        ELSE IF ( IBUF(I:I) .EQ. ' ' ) THEN
          CALL PUTBUF( ' ' )
          IF ( IN_PREFORMAT ) THEN
            I = I + 1
          ELSE
            CALL SKIP( IBUF(:L), I )
          END IF

*      Special code?
        ELSE IF ( IBUF(I:I) .EQ. '&' ) THEN
          I = I + 1
          IF ( IBUF(I:I+2) .EQ. 'amp' .AND. .NOT. HTML ) THEN
            IF ( TEX .AND. .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( BS//'&' )
            ELSE
              CALL PUTBUF( '&' )
            END IF
            I = I + 3

          ELSE IF ( IBUF(I:I+1) .EQ. 'gt' .AND. .NOT. HTML ) THEN
            CALL PUTBUF( '>' )
            I = I + 2

          ELSE IF ( IBUF(I:I+1) .EQ. 'lt' .AND. .NOT. HTML ) THEN
            CALL PUTBUF( '<' )
            I = I + 2

          ELSE IF ( IBUF(I:I+2) .EQ. 'inf' ) THEN
            IF ( TEX .AND. .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( '$'//BS//'infty$' )
            ELSE
              CALL PUTBUF( 'infinity' )
            END IF
            I = I + 3

          ELSE IF ( IBUF(I:I+2) .EQ. 'sim' ) THEN
            IF ( TEX .AND. .NOT. IN_PREFORMAT ) THEN
              CALL PUTBUF( '$'//BS//'sim$' )
            ELSE
              CALL PUTBUF( '~' )
            END IF
            I = I + 3

          ELSE IF ( IBUF(I:I+2) .EQ. 'rar' ) THEN
            IF ( TEX ) THEN
              CALL PUTBUF( '$'//BS//'rightarrow$' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '-&gt' )
            ELSE
              CALL PUTBUF( '->' )
            END IF
            I = I + 3

          ELSE IF ( IBUF(I:I+2) .EQ. 'lar' ) THEN
            IF ( TEX ) THEN
              CALL PUTBUF( '$'//BS//'leftarrow$' )
            ELSE IF ( HTML ) THEN
              CALL PUTBUF( '&lt-' )
            ELSE
              CALL PUTBUF( '<-' )
            END IF
            I = I + 3

          ELSE IF ( IBUF(I:I+3) .EQ. 'quot' .AND. .NOT. HTML ) THEN
            CALL PUTBUF( '"' )
            I = I + 4

          ELSE IF ( IBUF(I:I+3) .EQ. 'ouml' .AND. .NOT. HTML ) THEN
            CALL PUTBUF( 'o' )
            I = I + 4

          ELSE
            CALL PUTBUF( '&' )

          END IF
                     
        ELSE
          CALL PUTBUF( IBUF(I:I) )
          I = I + 1

        END IF

*    End loop over data
      END DO

*    Blank lines get echoed in preformatted input
 99   IF ( (L.EQ.0) .AND. IN_PREFORMAT ) THEN
        CALL OUTLINE( 0, NEWOBJ, NEWIND, 1, STATUS )
      ELSE
        CALL OUTLINE( 0, NEWOBJ, NEWIND, 0, STATUS )
      END IF

      END


      SUBROUTINE OUTLINE( OFFSET, NEWOBJ, NEWIND, INSBLNK, STATUS)
      IMPLICIT NONE
      INCLUDE 'HLPCONV_CMN'
      INTEGER STATUS,INSBLNK,I,OFFSET,NEWIND,NEWOBJ,FL
*
*    Local data :
*
      CHARACTER*80 SPACES
      DATA      SPACES/'                                        '/

*    File level
      FL = CUROLEV + OFFSET

*    Write data
      IF ( OBP(FL) .GT. NEWOBJ ) THEN
        IF ( (CURIND .GT. 0) .AND. (IMODE.NE.HCO__TEX) ) THEN
          CALL FIO_WRITE( OFD(FL), 
     :                    SPACES(:CURIND)//OBUF(FL)(:OBP(FL)-1),
     :                    STATUS )
        ELSE 
          CALL FIO_WRITE( OFD(FL), OBUF(FL)(:OBP(FL)-1), STATUS )
        END IF
      END IF
      OBP(FL) = NEWOBJ
      HTXTP(1) = CURIND + 80 - MAXWID

*    Extra blank lines?
      IF ( INSBLNK .GT. 0 .AND. IMODE.NE.HCO__HTML) THEN
        DO I = 1, INSBLNK
          CALL FIO_WRITE( OFD(FL), ' ', STATUS )
        END DO
      END IF

*    Update indentation
      CURIND = NEWIND

      END

*+
      SUBROUTINE TEX_BEGIN( STR )
      IMPLICIT NONE
      CHARACTER*(*) STR
      CALL PUTBUF( char(92) )
      CALL PUTBUF( 'begin{' )
      CALL PUTBUF( STR )
      CALL PUTBUF( '}' )
      END
      
      SUBROUTINE TEX_END( STR )
      IMPLICIT NONE
      CHARACTER*(*) STR
      CALL PUTBUF( char(92) )
      CALL PUTBUF( 'end{' )
      CALL PUTBUF( STR )
      CALL PUTBUF( '}' )
      END
      
      SUBROUTINE PUTBUFI( STR, OFFSET )
      IMPLICIT NONE
      INCLUDE 'HLPCONV_CMN'
      CHARACTER*(*) STR
      INTEGER OFFSET,FL
      FL = CUROLEV+OFFSET
      OBUF(FL)(OBP(FL):OBP(FL) + LEN(STR)-1) = STR
      OBP(FL) = OBP(FL) + LEN(STR)
      END

      SUBROUTINE PUTBUF( STR )
      IMPLICIT NONE
      CHARACTER*(*) STR
      CALL PUTBUFI( STR, 0 )
      END

      SUBROUTINE SKIP( STR, POS )
      CHARACTER*(*) STR
      INTEGER POS
 10   IF ( POS .LE. LEN(STR) ) THEN
        IF ( STR(POS:POS) .EQ. ' ' ) THEN
          POS = POS + 1
          GOTO 10
        END IF
      END IF
      END

      SUBROUTINE ADDTAG( ADDR, TEXT, ITAG )
      IMPLICIT NONE
      CHARACTER*(*) ADDR,TEXT
      LOGICAL 	FOUND
      CHARACTER*132	LTEXT
      INTEGER	CHR_LEN,I,ALEN,TLEN,ITAG
     
      INCLUDE 'HLPCONV_CMN'

      LTEXT = TEXT
      TLEN = CHR_LEN(LTEXT)
      CALL REM_UND( LTEXT(:TLEN) )

      ALEN = CHR_LEN(ADDR)

      FOUND = .FALSE.
      I = 1
      DO WHILE ( (I.LE.NTAGS) .AND. .NOT. FOUND )
        IF ( (HT_ALEN(I) .EQ. ALEN) .AND. (HT_TLEN(I) .EQ. TLEN)) THEN
          IF ( (HT_ADDR(I)(:ALEN) .EQ. ADDR(:ALEN)) .AND.
     :         (HT_TEXT(I)(:TLEN) .EQ. LTEXT(:TLEN)) ) THEN
            FOUND = .TRUE.
            ITAG = I
          ELSE
            I = I + 1
          END IF
        ELSE
          I = I + 1
        END IF
      END DO

      IF ( .NOT. FOUND ) THEN
        IF ( NTAGS .LT. MAXTAGS ) THEN
          NTAGS = NTAGS + 1
          HT_ADDR(NTAGS) = ADDR
          HT_ALEN(NTAGS) = ALEN
          HT_TEXT(NTAGS) = LTEXT(:TLEN)
          HT_TLEN(NTAGS) = TLEN
          ITAG = NTAGS
        ELSE
          PRINT *,'Out of tag space - increase MAXTAGS'
          STOP
        END IF
      END IF

      END

*+
      SUBROUTINE REM_UND( STR )
      CHARACTER*(*) STR
      INTEGER J
      DO J = 1, LEN(STR)
        IF (STR(J:J).EQ.'_') STR(J:J) = ' '
      END DO
      END

      SUBROUTINE REM_CH( STR, CH, L )
      CHARACTER*(*) STR,CH
      INTEGER J,NL
      NL = 0
      DO J = 1, L
        IF (STR(J:J).NE.CH) THEN
          NL = NL + 1
          STR(NL:NL) = STR(J:J)
        END IF
      END DO
      L = NL
      END

      SUBROUTINE REM_TDOT( STR, L )
      CHARACTER*(*) STR
      INTEGER L
      DO WHILE ( STR(L:L) .EQ. '.' )
        L = L - 1
      END DO
      END

      SUBROUTINE TRN_AMP( STR, SUB, L )
      CHARACTER*(*) STR, SUB
      INTEGER	IPOS
 10   IPOS = INDEX( STR(:L), '&amp' )
      IF ( IPOS .GT. 0 ) THEN
        STR = STR(:IPOS-1)//SUB//STR(IPOS+4:)
        L = L - 4 + LEN(SUB)
        GOTO 10
      END IF

      END


      SUBROUTINE OUTOPEN( FNAME, EXT, STATUS )
      IMPLICIT NONE
*
*    Global variables :
*
      INCLUDE 'HLPCONV_CMN'
      CHARACTER*(*) FNAME
      CHARACTER*(*) EXT
      INTEGER STATUS,FLEN,I
      LOGICAL	THERE

      INTEGER CHR_LEN

*  Warn if illegal characters in name
      IF ( INDEX(FNAME,'/') .GT. 0 ) THEN
        PRINT *,'Illegal filename : ',FNAME
        STOP
      ENDIF

      CURLEV = CURLEV + 1

      FLEN=CHR_LEN(FNAME)

*    Start sub-topic counter in next level
      SECTID(CURLEV+1) = 1

      IF ( IMODE .EQ. HCO__HTML ) THEN
        CUROLEV = CURLEV
        FROOT = FROOT(:FRLEN)//'/'//FNAME(:FLEN)
        FRLEN = FRLEN + FLEN + 1
        INQUIRE( FILE=FROOT(:FRLEN), EXIST=THERE )
        IF ( .NOT. THERE ) THEN
          CALL SYSTEM( 'mkdir '//FROOT(:FRLEN) )
        END IF
        INQUIRE( FILE=FROOT(:FRLEN)//'/index.'//EXT, EXIST=THERE )
        IF ( THERE ) THEN
          CALL UTIL_DELETE( FROOT(:FRLEN)//'/index.'//EXT, STATUS )
        END IF
        CALL FIO_OPEN( FROOT(:FRLEN)//'/index.'//EXT, 'WRITE', 
     :                 'LIST', 0, OFD(CURLEV), STATUS )
        IF ( CURLEV .EQ. 1 ) THEN
  	  PRINT *,'Opening ',FROOT(:FRLEN)//'/index.'//EXT
        END IF
      ELSE
        CUROLEV = 1 
        CALL FIO_OPEN( FNAME(:FLEN)//'.'//EXT, 'WRITE', 'LIST', 0, 
     :                 OFD(CUROLEV), STATUS )
      END IF

      OBP(CUROLEV) = 1

      END 


      SUBROUTINE OUTCLOS( STATUS )
      IMPLICIT NONE
*
*    Global variables :
*
      INCLUDE 'HLPCONV_CMN'
      INTEGER STATUS,I

      IF ( IMODE .EQ. HCO__HTML ) THEN
            IF ( .NOT. FIRSTSUB(CURLEV) ) THEN
              CALL PUTBUF( '</ul>' )
            END IF
            CALL PUTBUF( '</html>' )
            CALL OUTLINE( 0, 0, CURIND, 0, STATUS )
      END IF

      CALL FIO_CLOSE( OFD(CUROLEV), STATUS )
      CURLEV = CURLEV - 1
      CUROLEV = CUROLEV - 1
      IF ( IMODE .EQ. HCO__HTML ) THEN
        I = FRLEN
        DO WHILE ( (I.GT.0) .AND. (FROOT(I:I) .NE. '/') )
          I = I - 1
        END DO
        FRLEN = I - 1
      END IF

      END 
