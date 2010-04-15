proc qmanUpdate {item value} {
  global env
  global QmanWidgets
  if {[string trim ${item}] == "qmanERange"} {
    $QmanWidgets(END_RANGE_FIRST) delete 0 end
    $QmanWidgets(END_RANGE_FIRST) insert 0 1
    $QmanWidgets(END_RANGE_LAST) delete 0 end
    $QmanWidgets(END_RANGE_LAST) insert 0 1

  } elseif {[string trim ${item}] == "qmanIRange"} {
    $QmanWidgets(OBS_NUMBER) delete 0 end
    $QmanWidgets(OBS_NUMBER) insert 0 1
    $QmanWidgets(INT_RANGE_FIRST) delete 0 end
    $QmanWidgets(INT_RANGE_FIRST) insert 0 1
    $QmanWidgets(INT_RANGE_LAST) delete 0 end
    $QmanWidgets(INT_RANGE_LAST) insert 0 1

  } elseif {[string trim ${item}] == "buildQmanWidgets"} {
    set QmanWidgets(QUEUE_POSITION) oldest
    $QmanWidgets(OBS_RANGE_FIRST) delete 0 end
    $QmanWidgets(OBS_RANGE_FIRST) insert 0 1
    $QmanWidgets(OBS_RANGE_LAST) delete 0 end
    $QmanWidgets(OBS_RANGE_LAST) insert 0 1

  } else {
    set QmanWidgets(${item}) [string trim ${value}]
  }
  update idletasks
}
