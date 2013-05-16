EMACS=emacs -q --no-site-file

mentor:
	$(EMACS) -batch -L . \
		-f batch-byte-compile mentor.el url-scgi.el
