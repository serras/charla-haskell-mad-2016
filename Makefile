all: slides.ltx
	lhs2TeX --poly slides.ltx > slides.tex
	latexmk -pdf slides.tex
