#!/bin/bash
# acinetobacter.sh
echo Working with data file ${1}
NumberOfEntires=$(grep "Acinetobacter" ${1}| wc -l)
echo "The total number of Acinetobacter entries is" $NumberOfEntires.
NumberOfEntriesWithCode=$(grep 'Acinetobacter' ${1}| grep 'WP_005'| wc -l)
echo "The number of Acinetobacter entries with an accession code containing WP_005 is" $NumberOfEntriesWithCode.
LargestTaxid=$(grep "Acinetobacter" ${1}| sort -n -k 4 -t ',' -r| head -1| cut -c 1-14)
echo "The accession code for the Acinetobacter entry with the largest taxid is" $LargestTaxid.
