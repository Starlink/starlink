{ PROCEDURE MCURSOR : repeatedly display cursor
proc mcursor value1
  yn = undefined(value1)
  if yn
    another_cursor = 1
    loop while another_cursor = 1
      CURSOR
      asklog (another_cursor) "Another Cursor (Yes or No) \Y\ : "
    end loop
  else
    loop for loopy = 1 to value1
      CURSOR
    end loop
  end if
end proc

