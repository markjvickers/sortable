# sortable
Sortable Challenge met in Scala

ENVIRONMENT
===========
scalac 2.11.8
It's fast! (<900 ms on my laptop)
Leverages parallel processing over immutable data structures.

Give execution rights to build.sh and go.sh

BUILDING
========
Type ./build.sh in /sortable

TO RUN
======
Type ./go.sh path_to_product_file path_to_listing_file path_to_output_file

The directory that will contain the result file must exist.

Eg.

./go.sh data/products.txt data/listings.txt data/results.txt
