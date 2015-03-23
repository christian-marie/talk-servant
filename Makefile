MARKDOWNS=$(wildcard *.md)
DOTS=$(wildcard *.dot)
PDFS=$(MARKDOWNS:md=pdf)
PNGS=$(DOTS:dot=png)

all: $(PDFS) $(PNGS)
clean:
	rm -f $(PDFS) $(PNGS)

%.pdf: %.md template.tex $(PNGS)
	pandoc -H template.tex --filter columnfilter.py -i -t beamer -s --highlight-style=espresso $< -o $@

%.png: %.dot
	dot -Tpng $< -o $@
