
proc blt::InitDrag&DropBindings { widget button cmd } {
    bind $widget <ButtonPress-$button> [list $cmd drag %W %X %Y]
    bind $widget <B$button-Motion> [list $cmd drag %W %X %Y]
    bind $widget <ButtonRelease-$button> [list $cmd drop %W %X %Y]
}

