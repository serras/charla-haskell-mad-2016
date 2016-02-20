all: slides.ltx
	lhs2TeX slides.ltx > slidex.tex
	latexmk -pdf slides.tex
