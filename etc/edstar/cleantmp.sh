
files=`echo /tmp/edstar_*_pid`
if test ! "${files}" = "/tmp/edstar_*_pid"; then
   for file in $files; do
      kill -0 "`cat "${file}"`" 2>/dev/null || rm -f "${file}"
   done
fi
