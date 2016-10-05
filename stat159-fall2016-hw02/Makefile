.PHONY: all data clean

all: report.pdf eda-output.txt regression.RData

data: Advertising.csv
	curl -O "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
	mv Advertising.csv data/.

clean:
	rm *.pdf *.html

report.pdf: report.Rmd regression.RData scatterplot-tv-sales.png

regression.RData: regression-script.R Advertising.csv

eda-output.txt: eda-script.R Advertising.csv

