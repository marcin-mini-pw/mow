FILES=dokumentacja_wtepna.tex doc_summary.tex
CC = pdflatex

all: $(FILES:.tex=.pdf)
	echo $(FILES:.tex=.pdf)

%.pdf: %.tex 
	$(CC) $<
clean:
	-rm $(FILES:.tex=.aux) $(FILES:.tex=.log) $(FILES:.tex=.out) $(FILES:.tex=.blt)

cleanall: clean
	-rm $(FILES:.tex=.pdf) 

