{ PROCEDURE HARDPRINT : prints hardcopy to right queue
proc hardprint hardy delit
  yn = undefined(hardy)
  yn2 = undefined(delit)
  if yn
    asknum (junkwork) "Hardcopy Device Number \17\ ? "
  end if
  if yn2
    delgks = "N"
  else
    delgks = delit
  end if
  delgks = upcase(delgks)
  if hardy = 9
    print "Sorry, don''t know where to print file GKS_3500_1.DAT"
{    print "Sending the PLOT to the LN03 laser printer for plotting ..."
{    $ ircam_ln03print gks_3500_1.dat
  else if hardy = 10
    print "Sorry, don''t know where to print file GKS_3501_1.DAT"
{    print "Sending the PLOT to the LN03 laser printer for plotting ..."
{    $ ircam_ln03print gks_3501_1.dat
  else if hardy = 11
    print "Sorry, don''t know where to print file GKS_2010_1.DAT"
{    print "Sending the PLOT to the QMS laser printer for plotting ..."
{    $ ircam_qmsprint gks_2010_1.dat
  else if hardy = 12
    print "Sorry, don''t know where to print file GKS_2011_1.DAT"
{    print "Sending the PLOT to the QMS laser printer for plotting ..."
{    $ ircam_qmsprint gks_2011_1.dat
  else if hardy = 13
    print "Sending the PLOT to the CANON laser printer for plotting ..."
    ! ircam_canonprint gks_3200_1.dat
  else if hardy = 16
    print "Sending the PLOT to the PS laser printer for plotting ..."
    if delgks = "Y"
      ! ircam_postprintd gks_2700_1.dat
    else
      ! ircam_postprint gks_2700_1.dat
    end if
  else if hardy = 17
    print "Sending the PLOT to the PS laser printer for plotting ..."
    if delgks = "Y"
      ! ircam_postprintd gks_2701_1.dat
    else
      ! ircam_postprint gks_2701_1.dat
    end if
  else if hardy = 21
    print "Sending the PLOT to the COLOR PS printer for plotting ..."
    if delgks = "Y"
      ! ircam_colprintd gks_2708_1.dat
    else
      ! ircam_colprint gks_2708_1.dat
    end if
  else if hardy = 22
    print "Sending the PLOT to the COLOR PS printer for plotting ..."
    if delgks = "Y"
      ! ircam_colprintd gks_2709_1.dat
    else
      ! ircam_colprint gks_2709_1.dat
    end if
  end if
end proc

