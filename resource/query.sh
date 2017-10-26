#!/bin/sh -x
#
URL="http://localhost:8983/solr/ethereum/select"
Q1="?facet=true"
Q2="&facet.query={!tag p1}value:[100 TO *]" 
Q3="&facet.pivot={!query p1}from,to,value" 

`curl "$URL$Q1$Q2$Q3"`
