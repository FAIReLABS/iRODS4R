ROOT=$(shell pwd)

all: rest http

rest:
	Rscript run-bench.R $@ $(ROOT)/data https://cran.r-project.org/src/contrib/rirods_ 0.1.2 http://localhost/irods-rest/0.9.3

http:
	Rscript run-bench.R $@ $(ROOT)/data /home/nicola/Documents/work/products/rirods_ 0.2.0 http://localhost:9001/irods-http-api/0.2.0

clean:
	rm -f $(ROOT)/data/rest-files.csv $(ROOT)/data/rest-files.csv
