;;; Create a virtual function to break a line at point. This default
;;; implementation just uses the native newline function.
(edstar-virtual edstar-breakline (&optional arg)
                (edstar-native-newline arg))

