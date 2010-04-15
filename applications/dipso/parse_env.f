       sUBROUTINE PARSE_ENV ( string , length, env )

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments:
       CHARACTER*(*)       string
       INTEGER             length
       CHARACTER*(*)       env

*  Local Variables:
       CHARACTER*255       xlate
       CHARACTER*255       lstring
       INTEGER             imark
       INTEGER             ibegin
       INTEGER             xlen
       INTEGER             status

       CHARACTER*32   sysname
       CHARACTER*32   nodename
       CHARACTER*32   release
       CHARACTER*32   machine
       CHARACTER*32   version

*  Initialise the local status.
      STATUS = SAI__OK

       CALL PSX_UNAME ( sysname , nodename , release ,
     :                  version , machine , status )

       status = SAI__OK
       IF ( string(1:1) .EQ. '$' .AND.
     :      INDEX(string,'/') .GT. 0 ) THEN
          imark = INDEX(string,'/')
	  lstring = string(2:imark-1)
          IF ( sysname .EQ. 'VMS' ) CALL CHR_UCASE(lstring)
          CALL GTENV ( lstring, env, xlate, status )
          IF ( status .EQ. SAI__OK ) THEN
             xlen = INDEX(xlate,' ') -1

             IF ( sysname .EQ. 'VMS' ) THEN
                string = xlate(1:xlen)//string(imark+1:)
                length = length - imark + xlen
             ELSE
                string = xlate(1:xlen)//string(imark:)
                length = length - imark+1 + xlen
             ENDIF
          ELSE
             CALL ERR_FLUSH ( status )
          ENDIF

       ELSE IF ( INDEX(string,':') .GT. 0 ) THEN
          imark = INDEX(string,':')
          ibegin = imark-1
          DO WHILE ( ibegin .GT. 1 .AND.
     :               string(ibegin:ibegin) .NE. ' ' )
            ibegin = ibegin - 1
          END DO
	  lstring = string(ibegin:imark-1)
          IF ( sysname .EQ. 'VMS' ) CALL CHR_UCASE(lstring)
          CALL GTENV ( lstring, env, xlate, status )
          IF ( status .EQ. SAI__OK ) THEN
             xlen = INDEX(xlate,' ') -1
             IF ( sysname .EQ. 'VMS' ) THEN
                string = xlate(1:xlen)//string(imark+1:)
                length = length - imark -1 + ibegin + xlen
             ELSE
                string = xlate(1:xlen) // '/' // string(imark+1:)
                length = length - imark+ibegin + xlen
             ENDIF
          ELSE
             CALL ERR_FLUSH ( status )
          ENDIF


       ENDIF

       END
