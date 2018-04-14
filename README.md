# ShelveR: Automated Planning for Large-Scale Re-Shelving in Public/Academic Libraries 

## About the ShelveR Project

Libraries occasionally need to re-shelve their books on a large scale, e.g. when the whole library is relocated or during a massive weeding, when shelf space is to be allocated for new items. 

ShelveR provides librarians with an easy way of planning their book shelves at those situations, so that they can decide how many shelves they need to allocate for book of a certain classification number. 

ShelveR is proudly powered by R, an open-source project for statistical computing. You will need the following R packages to run this project: 

* rebus
* dplyr
* stringr
* readxl
* readr

## The Automated Planning Pipeline

There will be 3 steps in the automated planning process: 

* Ordering the table of bibliographic records with their call numbers
* Estimating the thickness of each book, based on the number of pages each book has
* And plan the shelving space for each classification number

### Ordering with Call Numbers

This project is based on the call number schemes of books at the Lending Section of Sichuan Provincial Library, and you can preview the sample data provided in this GitHub repo. 

In this call number scheme, we use the Chinese Library Classification (5th edition) as the classification scheme. For books processed and cataloged before 2016, the call number will basically be like "G949.2/4721/T01", where "G949.2" is the classification number, "4721" is the author's cutter number and the optional "T01" a further subdivision. 

For books cataloged in and after 2016, the call number will be like "2018/K0/4:v.2", where "2018" is the year the item is processed, "K0" the classification number, "4" the item serial number and the optional "v.2" a further subdivision. 

You will need to replace this function to fit your library's own call number scheme.

The input data file will be: "source_data.xlsx"

And the output data file: "ordered_data.csv" 

### Generating Thickness of Each Book 

In this step, we will use a linear model to generate the thickness of each book. 

Input data file: "ordered_data.xlsx"

Output data file: "ordered_data_with_thickness.csv"

### Automated Planning 

In this step, you will need to set a few parameters of your library's shelves and your shelving. The parameters includes: How much un-used space is to be left for each shelf, what is the length of each shelf, etc. 

After the planning, your will get a shelving summary: 

| A           | B           | C           | D           | E          | F           | G           | K           | N          | O          | P          | Q          |
| :---------- | :---------- | :---------- | :---------- | :--------- | :---------- | :---------- | :---------- | :--------- | :--------- | :--------- | :--------- |
| 40.0000000  | 487.0000000 | 194.0000000 | 575.1111111 | 71.2222222 | 657.0000000 | 317.6666667 | 920.6666667 | 25.6666667 | 34.5555556 | 25.3333333 | 23.5555556 |
| R           | S           | T           | TB          | TD         | TE          | TF          | TG          | TH         | TJ         | TK         | TL         |
| 221.5555556 | 35.7777778  | 0.3333333   | 8.4444444   | 0.3333333  | 0.6666667   | 0.4444444   | 6.5555556   | 5.5555556  | 0.5555556  | 2.1111111  | 0.8888889  |
| TM          | TN          | TP          | TQ          | TS         | TU          | TV          | U           | V          | X          | Z          |            |
| 10.0000000  | 18.3333333  | 43.4444444  | 8.7777778   | 81.5555556 | 37.7777778  | 3.0000000   | 21.6666667  | 3.7777778  | 26.3333333 | 47.0000000 |            |

Where A, B, C, etc. are the starting letters of the classification numbers and 40, 487, etc. are the numbers of shelves allocated to each range of classification numbers. 

To generate the above summary, we used a dataset that has over 180,000 books. View the source code for more parameters and details. 

## TODOs

* To be added.

## Acknowledgements

At Sichuan Provincial Library, Ms. WU Xia, Vice-Head of Access Services Department provided on-going support for this project and my everyday work at the access services; Mr. XU Zhixi, a senior librarian from the Information Technology Department provided me with valuable help in extracting the source data needed for the ShelveR project. Thank you! 

## Disclaimer

USE THIS PROJECT AT YOUR OWN RISK AND NO WARRANTY IS PROVIDED.

## Any improvements and suggestions for ShelveR are welcomed :-)

