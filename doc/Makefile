FILE=dokumentacja_wtepna
${FILE}.pdf: ${FILE}.tex 
	pdflatex ${FILE}.tex
	pdflatex ${FILE}.tex
clean:
	rm "${FILE}.aux ${FILE}.log ${FILE}.out"
cleanall: clean
	rm "${FILE}.pdf" 

