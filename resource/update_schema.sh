#!/bin/sh
#
curl -X POST -H 'Content-Type: application/json' http://localhost:8983/solr/ethereum/schema -d @schema.json

