{ PROCEDURE SETVARGREY : sets X and Y percentage cut for vargrey plot
proc setvargrey value1 value2
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  if yn1 or yn2
    print "Give new percentage X CUT ? "
    asknum (value3) "Percentage X \20\ : "
    print "Give new percentage Y CUT ? "
    asknum (value4) "Percentage Y \80\ : "
  else
    value3 = value1
    value4 = value2
  end if
  send plt2d set vargrey_xpc (value3)
  send plt2d set vargrey_ypc (value4)
  print "O.K. low " (value3) "% of intensity range will be mapped"
  print "  onto bottom " (value4) "% of colour table ..."
end proc
