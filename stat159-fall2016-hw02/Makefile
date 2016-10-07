.PHONY: all data clean

all: report.pdf eda-output.txt regression.RData
data: Advertising.csv
clean:
	rm -f *.pdf *.html

#download file	
Advertising.csv:
	curl -O "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
	mv Advertising.csv data/.

#summary stats about data
eda-output.txt: eda-script.R Advertising.csv
	cd code; Rscript eda-script.R

regression.RData: regression-script.R Advertising.csv
	cd code; Rscript regression-script.R

report.pdf: report.Rmd regression.RData scatterplot-tv-sales.png
	cd report; pandoc report.Rmd -s -o Report.pdf




