{ PROCEDURE CLINE : plots a line between two cursor positions
proc cline
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    print "Define START/END of LINE with CURSOR ..."
    send plt2d set cursor_cross 'NO'
    obeyw plt2d curlin
  else
    print "You CANNOT use the CURSOR on this device "
    print "Try LINE instead"
  end if
end proc

