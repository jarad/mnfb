# Usually, only these lines need changing
TEXFILE= report
RDIR= ./R
FIGDIR= ./figs

# list R files
RFILES := $(wildcard $(RDIR)/*.R)
# pdf figures created by R
PDFFIGS := $(wildcard $(FIGDIR)/*.pdf)
# Indicator files to show R file has run
OUT_FILES:= $(RFILES:.R=.Rout)
# Indicator files to show pdfcrop has run
CROP_FILES:= $(PDFFIGS:.pdf=.pdfcrop)


Rcmd := R CMD BATCH --vanilla


all: $(TEXFILE).pdf $(OUT_FILES) $(CROP_FILES)

# May need to add something here if some R files depend on others.

# RUN EVERY R FILE
$(RDIR)/%.Rout: $(RDIR)/%.R $(RDIR)/common/functions.R
	$(Rcmd) $< $<out

# CROP EVERY PDF FIG FILE
$(FIGDIR)/%.pdfcrop: $(FIGDIR)/%.pdf
	pdfcrop $< $< && touch $@

# Compile main tex file
$(TEXFILE).pdf: $(TEXFILE).tex $(OUT_FILES) $(CROP_FILES)
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)
	#latexmk -pdf -quiet $(TEXFILE)
#	rubber --pdf $(TEXFILE)
#	rubber-info $(TEXFILE)
	
# Run R files
R: $(OUT_FILES)

# View main tex file
view: $(TEXFILE).pdf
	open $(TEXFILE).pdf &

# Clean up stray files
clean:
	rm -fv $(OUT_FILES) 
	rm -fv $(CROP_FILES)
	rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.out *.bcf *blx.bib *.run.xml
	rm -fv *.fdb_latexmk *.fls
	rm -fv $(TEXFILE).pdf

.PHONY: all clean

