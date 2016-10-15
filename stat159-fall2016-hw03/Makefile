.PHONY: data tests eda regression report clean all

# downloads data into data folder
data: Advertising.csv
	curl -O "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
	mv Advertising.csv data/.

# run unit tests on regression functions
tests: test-that.R Advertising.csv
	cd code; Rscript test-that.R

# perform exploratory data analysis in eda
eda: eda-script.R Advertising.csv
	cd code; Rscript scripts/eda-script.R

# performs regression analysis
regression: regression-script.R
	cd code; Rscript scripts/regression-script.R

# converts report from rmd to pdf file
report: report.Rmd
	cd report; pandoc report.Rmd -s -o Report.pdf

# delete the generated report
clean:
	rm -f *.pdf *.html

all: eda regression report




