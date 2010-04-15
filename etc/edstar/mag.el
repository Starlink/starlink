(new-routine "MAG_ALOC"
             "Allocate a tape drive for continued use"
             '("PARAM" "STATUS"))

(new-routine "MAG_ANNUL"
             "Annul tape descriptor, releasing any associated tape drive"
             '("TD" "STATUS"))

(new-routine "MAG_ASSOC"
             "Open a tape device and return a descriptor"
             '("PARAM" "MODE" "TD" "STATUS"))

(new-routine "MAG_CANCL"
             "Close tape device"
             '("PARAM" "STATUS"))

(new-routine "MAG_DEACT"
             "Deactivate MAG package at end of application"
             '("STATUS"))

(new-routine "MAG_DEAL"
             "De-allocate a tape drive"
             '("PARAM" "STATUS"))

(new-routine "MAG_DISM"
             "Dismount a tape on a drive"
             '("PARAM" "UNLOAD" "STATUS"))

(new-routine "MAG_JEOV"
             "Jump over an EOV condition (2 consecutive tape marks)"
             '("TD" "STATUS"))

(new-routine "MAG_JUMP"
             "Skip a specified number of physical blocks"
             '("TD" "NBLOCK" "STATUS"))

(new-routine "MAG_MOUNT"
             "Mount a tape on a drive"
             '("PARAM" "MODE" "STATUS"))

(new-routine "MAG_MOVE"
             "Move to a specified file and block on a tape"
             '("TD" "FILE" "START" "BLOCK" "STATUS"))

(new-routine "MAG_POS"
             "Enquire current tape file/block position"
             '("TD" "FILE" "START" "BLOCK" "MOVED" "STATUS"))

(new-routine "MAG_READ"
             "Read a block from a tape"
             '("TD" "MAXVAL" "VALUES" "ACTVAL" "STATUS"))

(new-routine "MAG_REW"
             "Rewind a tape"
             '("TD" "STATUS"))

(new-routine "MAG_SET"
             "Set current tape file/block positions"
             '("TD" "FILE" "START" "BLOCK" "STATUS"))

(new-routine "MAG_SKIP"
             "Skip a specified number of tape marks"
             '("TD" "NTM" "STATUS"))

(new-routine "MAG_WRITE"
             "Write a block to tape"
             '("TD" "NVAL" "VALUES" "ACTVAL" "STATUS"))

(new-routine "MAG_WTM"
             "Write a tape mark"
             '("TD" "STATUS"))
