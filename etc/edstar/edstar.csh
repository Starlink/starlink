#  This is now the non-X display version...

     set EDSTAR_exists = ''
     if ( -e /tmp/edstar_$$_pid ) then
        set EDSTAR_pid = `cat /tmp/edstar_$$_pid`
        set EDSTAR_exists = `kill -0 $EDSTAR_pid >&/dev/null && echo 1`
        if ( $EDSTAR_exists == 1 ) then
           pwd > /tmp/edstar_$$_init
           if ( ! $?EDSTAR_DISPLAY ) then
              %'( $EDSTAR )'
           else
              kill -HUP $EDSTAR_pid
           endif
        else
           rm -f /tmp/edstar_$$_pid
        endif
     endif

     if ( $EDSTAR_exists != 1 ) then
        echo 'Starting edstar job'
        set EDSTAR = 'eval setenv EDSTAR_JOB '"$$"'; \emacs -nw -l $EDSTAR_DIR/startup.el '"!*"'; rm -f /tmp/edstar_'"$$"'_pid'

        $EDSTAR_DIR/cleantmp.sh
        pwd > /tmp/edstar_$$_init
        if ( $?EDSTAR_DISPLAY ) unset EDSTAR_DISPLAY
        eval '( $EDSTAR )'
     endif
