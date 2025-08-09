
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readhid

<!-- badges: start -->

<!-- badges: end -->

The goal of readhid is to read Applied Biosystems Inc. Format (ABIF)
files. These files are created by Applied Biosystems Genetic Analyzers
and have file extensions such as .fsa and .hid.

There are other packages that read ABIF format files, however none of
them will correctly extract the `DATA` and `Peak` fields from the newer
file format, such as the .hid files produced by the 3500 Genetic
Analyzer.

## Installation

You can install the development version of readhid from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SAR85/readhid")
```

## Example

Read ABIF files by calling `hid()` and provide a path to the file:

``` r
library(readhid)
my_hid <- hid("path/to/my.hid")

print(my_hid)
```

The resulting `hid` object will contain data from the ABIF file,
including various instrument and run parameters, as well as the raw
sequencing or fragment analysis data. All of the data can be accessed in
the `data` element of the object. The available data fields vary by
instrument model used for data collection.

The data elements can be retrieved using `dir_data()`:

``` r
my_hid_data <- dir_data(my_hid)

my_hid_data |> head(5)
#> $AAct.1
#> ABIF data object
#>  Data type: 13 (logical)
#>  Num. elements: 1 
#>  Raw data: FALSE
#> $ABED.1
#> ABIF data object
#>  Data type: 19 (cString)
#>  Num. elements: 1 
#>  Raw data: FALSE
#> $ABID.1
#> ABIF data object
#>  Data type: 19 (cString)
#>  Num. elements: 1 
#>  Raw data: FALSE
#> $ABLt.1
#> ABIF data object
#>  Data type: 19 (cString)
#>  Num. elements: 1 
#>  Raw data: FALSE
#> $ABRn.1
#> ABIF data object
#>  Data type: 5 (long)
#>  Num. elements: 1 
#>  Raw data: FALSE
```

The raw sequencing or fragment analysis data are found in the `DATA`
fields:

``` r
dir_data(my_hid, "DATA") |> names()
#>  [1] "DATA.1"   "DATA.2"   "DATA.3"   "DATA.4"   "DATA.5"   "DATA.6"  
#>  [7] "DATA.7"   "DATA.8"   "DATA.9"   "DATA.10"  "DATA.11"  "DATA.12" 
#> [13] "DATA.105" "DATA.106" "DATA.205" "DATA.206"
```

Analyzed peak data is extracted, if present, and organized into a data
frame. The dataframe can be obtained using `hid_peaks()`.

``` r
my_hid_peaks <- abif_peaks(my_hid)
str(my_hid_peaks)
#> Classes 'abif_peaks' and 'data.frame':   93 obs. of  24 variables:
#>  $ dye_index        : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ data_point       : int  4560 4613 4669 5260 5358 6145 6192 6639 6819 7291 ...
#>  $ begin_data_point : int  4548 4600 4657 5248 5346 6133 6176 6626 6804 7272 ...
#>  $ end_data_point   : int  4573 4634 4693 5278 5374 6167 6213 6660 6831 7303 ...
#>  $ fwhm             : int  8 8 8 8 8 8 8 8 8 8 ...
#>  $ corrected_fwhm   : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ height           : int  209 2660 2635 1473 732 589 551 807 438 331 ...
#>  $ begin_height     : int  0 3 2 15 4 0 0 0 0 0 ...
#>  $ end_height       : int  3 0 0 0 0 0 0 0 2 2 ...
#>  $ area             : int  1897 24596 23144 12749 6345 5476 5113 7081 3866 3128 ...
#>  $ corrected_area   : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ size             : num  117 121 126 173 181 ...
#>  $ begin_size       : num  116 120 125 172 180 ...
#>  $ end_size         : num  118 123 128 174 182 ...
#>  $ fwhm_bp          : num  0.649 0.659 0.632 0.654 0.667 ...
#>  $ corrected_fwhm_bp: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ area_bp          : num  147 1915 1810 1041 522 ...
#>  $ corrected_area_bp: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ size_matched     : chr  "0" "0" "0" "0" ...
#>  $ bp_size_match    : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ offscale         : chr  "0" "0" "0" "0" ...
#>  $ broad            : chr  "0" "0" "0" "0" ...
#>  $ pullup           : chr  "0" "0" "0" "0" ...
#>  $ dye_name         : chr  "6-FAM" "6-FAM" "6-FAM" "6-FAM" ...
```
