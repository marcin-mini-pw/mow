FILE=dokumentacja_wstepna
all:
	pdflatex ".tex"
clean:
	rm "${FILE}.aux ${FILE}.log ${FILE}.out"
cleanall: clean
	rm "${FILE}.pdf" 

