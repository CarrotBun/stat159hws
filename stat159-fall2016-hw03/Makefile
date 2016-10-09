.PHONY: all data clean

data: Advertising.csv
	curl -O "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
	mv Advertising.csv data/.

tests: test-that.R Advertising.csv
	cd code; Rscript test-that.R

eda: eda-script.R Advertising.csv
	cd code; Rscript eda-script.R

regression: regression-script.R
	cd code; Rscript regression-script.R

report: report.Rmd regression.RData scatterplot-tv-sales.png
	cd report; pandoc report.Rmd -s -o Report.pdf
clean:
	rm -f *.pdf *.html

all: report.pdf eda-output.txt regression.RData

eda-output.txt: eda-script.R Advertising.csv
	cd code; Rscript eda-script.R

regression.RData: regression-script.R Advertising.csv
	cd code; Rscript regression-script.R



