# First parameter is products, second is listings, third is output.

# Eg.
#
# ./go.sh products.txt listings.txt results.txt

scala -cp lib/spray-json_2.11-1.3.2.jar:lib/shapeless_2.11-2.2.3.jar -sourcepath bin sort.Main $1 $2 $3