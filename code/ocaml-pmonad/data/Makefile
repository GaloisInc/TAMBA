plotpdf:
	@make plot
	@make pdf
	@make move
	@make clean

plot:
	@for p in *.gnu ; do \
		echo "plotting: $$p" ; \
		gnuplot $$p ; \
	done

pdf:
	@for p in *.eps ; do \
		echo "converting to pdf: $$p" ; \
		ps2pdf -dEPSCrop $$p ; \
	done

move: *.pdf
	mv *.pdf ../figures

clean:
	rm -Rf *.eps