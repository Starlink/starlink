
tags:
	etags --lang=scheme --regex='/(\(element\|mode\)[ \t]+\([^ \t]+\)/\2/' */*.dsl
#	etags --lang=scheme --regex='/(element[ \t]+[^ \t]+/' */*.dsl

clean:
	for i in `find . -type d`; do \
		rm $$i/*~ ; \
	done
