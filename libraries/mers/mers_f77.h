

F77_SUBROUTINE(msg_setr)( CHARACTER(TOKEN), REAL(RVALUE) TRAIL(TOKEN) );
F77_SUBROUTINE(msg_seti)( CHARACTER(TOKEN), INTEGER(IVALUE) TRAIL(TOKEN) );
F77_SUBROUTINE(msg_setk)( CHARACTER(TOKEN), INTEGER8(KVALUE) TRAIL(TOKEN) );
F77_SUBROUTINE(msg_setd)( CHARACTER(TOKEN), DOUBLE(DVALUE) TRAIL(TOKEN) );
F77_SUBROUTINE(msg_setl)( CHARACTER(TOKEN), LOGICAL(LVALUE) TRAIL(TOKEN) );
F77_SUBROUTINE(msg_setc)( CHARACTER(TOKEN), CHARACTER(CVALUE)
                          TRAIL(TOKEN) TRAIL(CVALUE) );

F77_SUBROUTINE(err_annul)( INTEGER(STATUS) );
F77_SUBROUTINE(err_begin)( INTEGER(STATUS) );
F77_SUBROUTINE(err_end)( INTEGER(STATUS) );

F77_SUBROUTINE(err_clear)( INTEGER(status) );

F77_SUBROUTINE(err_facer)( CHARACTER(token),
                           INTEGER(status)
                           TRAIL(token) );


F77_SUBROUTINE(err_fioer)( CHARACTER(token),
                           INTEGER(iostat)
                           TRAIL(token) );


F77_SUBROUTINE(err_flbel)( INTEGER(status) );


F77_SUBROUTINE(err_flush)( INTEGER(status) );

F77_SUBROUTINE(err_level)( INTEGER(level) );

F77_SUBROUTINE(err_load)( CHARACTER(param),
                          INTEGER(parlen),
                          CHARACTER(opstr),
                          INTEGER(oplen),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(opstr) );

F77_SUBROUTINE(err_mark)( void );


F77_SUBROUTINE(err_out)( CHARACTER(param),
                         CHARACTER(text),
                         INTEGER(status)
                         TRAIL(param)
                         TRAIL(text) );

F77_SUBROUTINE(err_rep)( CHARACTER(param),
                         CHARACTER(text),
                         INTEGER(status)
                         TRAIL(param)
                         TRAIL(text) );

F77_SUBROUTINE(err_rlse)( void );

F77_SUBROUTINE(err_stat)( INTEGER(status) );

F77_SUBROUTINE(err_start)( void );

F77_SUBROUTINE(err_stop)( INTEGER(status) );

F77_SUBROUTINE(err_syser)( CHARACTER(token),
                           INTEGER(systat)
                           TRAIL(token) );
F77_SUBROUTINE(err_tune)( CHARACTER(param),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(param) );

F77_SUBROUTINE(msg_bell)( INTEGER(status) );

F77_SUBROUTINE(msg_blank)( INTEGER(status) );

F77_SUBROUTINE(msg_blankif)( INTEGER(PRIOR), INTEGER(status) );

F77_LOGICAL_FUNCTION(msg_flevok)( INTEGER(FILTER), INTEGER(STATUS) );

F77_SUBROUTINE(msg_flusherr)( INTEGER(status) );

F77_SUBROUTINE(msg_fmtc)( CHARACTER(token),
                          CHARACTER(format),
                          CHARACTER(cvalue)
                          TRAIL(token)
                          TRAIL(format)
                          TRAIL(cvalue) );

F77_SUBROUTINE(msg_fmtd)( CHARACTER(token),
                          CHARACTER(format),
                          DOUBLE(dvalue)
                          TRAIL(token)
                          TRAIL(format) );

F77_SUBROUTINE(msg_fmti)( CHARACTER(token),
                          CHARACTER(format),
                          INTEGER(ivalue)
                          TRAIL(token)
                          TRAIL(format) );

F77_SUBROUTINE(msg_fmtl)( CHARACTER(token),
                          CHARACTER(format),
                          LOGICAL(lvalue)
                          TRAIL(token)
                          TRAIL(format) );

F77_SUBROUTINE(msg_fmtr)( CHARACTER(token),
                          CHARACTER(format),
                          REAL(rvalue)
                          TRAIL(token)
                          TRAIL(format) );

F77_SUBROUTINE(msg_ifget)( INTEGER(status) );

F77_SUBROUTINE(msg_ifgetenv)( INTEGER(STATUS) );

F77_SUBROUTINE(msg_iflev)( INTEGER(FILTER), CHARACTER(STRING),
                           INTEGER(STATUS) TRAIL(STRING) );

F77_SUBROUTINE(msg_ifset)( INTEGER(filter),
                           INTEGER(status) );

F77_SUBROUTINE(msg_load)( CHARACTER(param),
                          CHARACTER(text),
                          CHARACTER(opstr),
                          INTEGER(oplen),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(text)
                          TRAIL(opstr) );

F77_SUBROUTINE(msg_out)( CHARACTER(param),
                         CHARACTER(text),
                         INTEGER(status)
                         TRAIL(param)
                         TRAIL(text) );

F77_SUBROUTINE(msg_outif)( INTEGER(prior),
                           CHARACTER(param),
                           CHARACTER(text),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(text) );

F77_SUBROUTINE(msg_renew)( void );

F77_SUBROUTINE(msg_sync)( INTEGER(status) );

F77_SUBROUTINE(msg_tune)( CHARACTER(param),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(param) );
