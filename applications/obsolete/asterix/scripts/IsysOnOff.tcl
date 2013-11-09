#
proc AutoSaveSettings_OnOff {name element op} {
  global AutoSaveSettings

    UpdateSetting AutoSaveSettings $AutoSaveSettings
}
#
#
#
proc ChangeCatServer {name element op} {
  global AutoSaveSettings
  global CatServer

  if {$AutoSaveSettings == 1} {
    UpdateSetting CatServer $CatServer
  }

}
#
#
proc AutoRefresh_OnOff {name element op} {
  global AutoSaveSettings
  global AutoRefresh

  if {$AutoSaveSettings == 1} {
    UpdateSetting AutoRefresh $AutoRefresh
  }

}
#
#
proc Ext1DWnd_OnOff {name element op} {
  global AutoSaveSettings
  global Ext1DWnd

  if {$AutoSaveSettings == 1} {
    UpdateSetting Ext1DWnd $Ext1DWnd
  }

}
#

#
proc Msg_OnOff {name element op} {
  global ShowMsg
  global AutoSaveSettings

  if {$ShowMsg == 1} {
    pack .top.msg -after .top.dummyb -anchor w -fill x -side bottom

  } else {
    pack forget .top.msg
  }

  if {$AutoSaveSettings == 1} {
    UpdateSetting ShowMsg $ShowMsg
  }
}
#
#
proc Attrib_OnOff {name element op} {
  global  ShowAttrib
  global  AutoSaveSettings

  if {$ShowAttrib == 1} {
    pack .top.attrib -after .top.dummyl -side left

  } else {
    pack forget .top.attrib
  }

  if {$AutoSaveSettings == 1} {
    UpdateSetting ShowAttrib $ShowAttrib
  }
}
#

#
proc Shape_OnOff {name element op} {
  global  ShowShape
  global AutoSaveSettings

  if {$ShowShape == 1} {
    pack .top.stbar -after .top.dummyt1 -side top
  } else {
    pack forget .top.stbar
  }
  if {$AutoSaveSettings == 1} {
    UpdateSetting ShowShape $ShowShape
  }
}
#
#
proc Cache_OnOff {name element op} {
  global  ShowCache
  global AutoSaveSettings

  if {$ShowCache == 1} {
    pack .top.cbar -after .top.dummyt2 -side top
  } else {
    pack forget .top.cbar
  }
  if {$AutoSaveSettings == 1} {
    UpdateSetting ShowCache $ShowCache
  }
}
#
#
#
proc Cache_onoff    {name element op} {
  global Cache cbar
  if {$Cache} {
    EnableButton $cbar.pop 
    EnableButton $cbar.toggle 
    EnableButton $cbar.clear 
    EnableButton $cbar.add 
    EnableButton $cbar.subtract 
  } else {
    DisableButton $cbar.pop
    DisableButton $cbar.toggle 
    DisableButton $cbar.clear 
    DisableButton $cbar.add 
    DisableButton $cbar.subtract 
  }
}
#  
proc Buffer_onoff    {name element op} {
  global Buffer cbar
  if {$Buffer} {
    EnableButton $cbar.undo 
  } else {
    DisableButton $cbar.undo 
  }
}
#
#
#
proc ShowTips_OnOff {name element op} {
  global ShowTips

  UpdateSetting ShowTips $ShowTips

}
#
