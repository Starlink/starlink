proc CCDSetOptionFromMenu { target menuopt } {

  if {"[winfo class $target]" == "Label"} {
     $target config -text \
        [ lindex [ $menuopt entryconfig [ $menuopt index active ] -label ] 4 ]
  } else {

    if {"[winfo class $target]" == "Entry"} {
      $target delete 0 end
      $target insert 0 \
         [ lindex [ $menuopt entryconfig [ $menuopt index active ] -label ] 4 ]
    }
  }
}
# $Id$
