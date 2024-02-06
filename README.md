This repository contains an R script needed for performing a replication of "[The Long Term Benefits of Vaccination Campaigns: Evidence from Measles](https://web.archive.org/web/20230502002136/https://www.haverford.edu/sites/default/files/Department/Economics/econ-colloquium-Measles_Mexico_Feb2023.pdf)", Atwood and Pearlman (2023).

There are three sources of data.
First, I use Mexican census data to measure adult economic outcomes.
To access this data, apply for an account from [IPUMS](https://international.ipums.org/international/), then run the following query:
- country: Mexico
- years: 1970, 1990, 1995, 2000, 2010, 2015
- variables:
    - household: geo1_mx (state), urban
    - person: age, sex, marst, nativity, bplmx, yrschool, lit, empstat, inctot, incearn  
Save the xml file in 'data/' as 'ipumsi_00025.xml'.

Note that the IPUMS extract includes an `.R` file with functions to read the data: `read_ipums_ddi()`, which reads an `.xml` file, and `read_ipums_micro()`, which reads the micro data. 
You first need to install the R library `ipumsr`.

Second, I use CPI data from INEGI to adjust for inflation. Download the CSV file [here](https://web.archive.org/web/20230502002120/https://www.inegi.org.mx/app/tabulados/default.aspx?nc=ca55_2018&idrt=137&opc=t) (years 1969-2023).

Third, I hand-collect disease data from PDFs of the annual epidemiology bulletins. For this replication, I collected data on measles, syphilis, tetanus, and pertussis.

[1965](https://web.archive.org/web/20230501235756/https://saludpublica.mx/index.php/spm/article/view/3720/3604)
[1966](https://web.archive.org/web/20230501235741/https://saludpublica.mx/index.php/spm/article/view/3497/3382)
[1967](https://web.archive.org/web/20230501235510/https://saludpublica.mx/index.php/spm/article/view/3061/2947)
[1968](https://web.archive.org/web/20230501235706/https://saludpublica.mx/index.php/spm/article/view/2849/2736)
[1969](https://web.archive.org/web/20230501235652/https://saludpublica.mx/index.php/spm/article/view/2236/2126)
[1970](https://web.archive.org/web/20230501235427/https://saludpublica.mx/index.php/spm/article/view/2025/1915)
[1971](https://web.archive.org/web/20230501235419/https://saludpublica.mx/index.php/spm/article/view/1817/1781)
[1972](https://web.archive.org/web/20230501235408/https://saludpublica.mx/index.php/spm/article/view/1649/1632)
[1973](https://web.archive.org/web/20230501235317/https://saludpublica.mx/index.php/spm/article/view/1505/1488)
[1974](https://web.archive.org/web/20230501235305/https://saludpublica.mx/index.php/spm/article/view/1363/1347)
[1975](https://web.archive.org/web/20230501235251/https://saludpublica.mx/index.php/spm/article/view/1234/1219)
[1976](https://web.archive.org/web/20230501234755/https://saludpublica.mx/index.php/spm/article/view/1126/1111)
[1977](https://web.archive.org/web/20230501234734/https://saludpublica.mx/index.php/spm/article/view/994/980)
[1978](https://web.archive.org/web/20230501234823/https://saludpublica.mx/index.php/spm/article/view/899/885)

The latter two datasets are included in `data/`.

To rerun the analyses, execute the R script `run.r`. Note that you need to set the path in `run.r` on line 10 to define the location of the folder that contains this script.