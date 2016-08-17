COMP=pandoc
COPTS=--highlight-style tango

%.pdf: %.markdown
				$(COMP) -s $(COPTS) -o $@ $<

%.html: %.markdown
				$(COMP) -s $(COPTS) -o $@ $<
