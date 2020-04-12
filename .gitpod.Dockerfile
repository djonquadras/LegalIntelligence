FROM rocker/shiny-verse

RUN apt-get update -qq && apt-get install -y \
	sudo \
	pandoc \
	pandoc-citeproc \
	libcurl4-gnutls-dev \
	libcairo2-dev \
	libxt-dev \
	libssl-dev \
	libssh2-1-dev \
	libxml2-dev
 
COPY library.R server.R ui.R metodologia.png ./

RUN R -e "source('library.R')"
