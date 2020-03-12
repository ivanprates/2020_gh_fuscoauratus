### The goal of this Unix shell script is to run Fastqc to access raw read quality.
### By Ivan Prates, June 2018.
### Smithsonian National Museum of Natural History, Washington DC, USA.

#fastqc ~/Documents/Ivan_documents/2019-02-12_ipyrad_Anolis_denovo/ANOLIS1_t70_s10_fastqs/*.fastq.gz

#echo "Done running Fastqc for ANOLIS1!"

#fastqc ~/Documents/Ivan_documents/2019-02-12_ipyrad_Anolis_denovo/ANOLIS2_t70_s10_fastqs/*.fastq.gz

#echo "Done running Fastqc for ANOLIS2!"

#echo "Done running Fastqc for all files!"

# For trimmed sequences:

fastqc ~/Documents/Ivan_documents/2019-02-12_ipyrad_Anolis_denovo/ANOLIS1_t70_s10_edits/*.fastq.gz

echo "Done running Fastqc for ANOLIS1!"

fastqc ~/Documents/Ivan_documents/2019-02-12_ipyrad_Anolis_denovo/ANOLIS2_t70_s10_edits/*.fastq.gz

echo "Done running Fastqc for ANOLIS2!"