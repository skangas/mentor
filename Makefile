EMACS=emacs -q --no-site-file

mentor:
	$(EMACS) -batch -L . \
		-f batch-byte-compile mentor-data.el mentor-rpc.el \
		mentor.el url-scgi.el

clean:
	rm -f *.elc
