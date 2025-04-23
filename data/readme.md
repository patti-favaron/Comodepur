# Contents

Data files are:

- IMPIANTO_NO_CALMET.CON (splitted in binary files xaa, xab, xac, xad, xae; instruction for reconstructing in followinf section).
- SONICO_NO_CALMET.CON

# Procedure for reconstructing file IMPIANTO_NO_CALMET.CON

Get all data files and copy them to a directory of your choice. Then open a terminal session, navigate to the directory, and give the command

cat xa? > IMPIANTO_NO_CALMET.CON

This applies to UNIX systems, and in particular to Linux or Max OS/X. If using another operating system please consult your site documentation to discover how to unite files in binary mode, or ask your system administrator.

# Licensing

The data are binary, and do not contain any licensing text; we (Cristiana Morosini, Patrizia Favaron, Elisabetta Zanardini and Vincenzo Torretta, authors of the paper mentioned in the repository README.md file) declare data have been obtained by us starting from meteorological and emission data under our possession.

In addition we declare all the data released in this directory are covered by the MIT license.
