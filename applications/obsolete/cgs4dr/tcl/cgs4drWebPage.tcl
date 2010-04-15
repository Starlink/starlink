proc cgs4drWebPage {taskname url} {
    global env
    global cgs4drWebBrowser
    set lock "$env(HOME)/.netscape/lock"
    catch {glob "$lock*"} err
    if {$err == $lock} {
        cgs4drInform $taskname "Re-using $cgs4drWebBrowser with URL $url in new window"
	catch {eval exec $cgs4drWebBrowser -remote \"openURL($url,new-window)\" &} errs
    } else {
        cgs4drInform $taskname "Starting $cgs4drWebBrowser with URL $url"
	catch {eval exec $cgs4drWebBrowser -ncols 128 $url &} errs
    }
}
