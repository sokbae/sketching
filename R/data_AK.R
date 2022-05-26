#' AK
#'
#' Angrist-Krueger (AK) dataset is a data extract from US Censuses that was analyzed in Angrist and Krueger (1991).
#' In particular, the current dataset is from the 1970 Census, consisting of men born 1920-1929 
#' (Year 1929 is the omitted cohort group).
#' 
#' @references Angrist, J.D. and Krueger, A.B., 1991. Does compulsory school attendance affect schooling and earnings?
#' Quarterly Journal of Economics, 106(4), pp.979--1014.
#' \doi{10.2307/2937954}
#'
#' @format A data frame with 247,199 rows and 42 variables:
#' \describe{
#' \item{LWKLYWGE}{Outcome: log weekly wages}
#' \item{EDUC}{Covariate of interest: years of education}
#' \item{YR20}{Indicator variable for the year of birth: equals 1 if yob = 1920}
#' \item{YR21}{Indicator variable for the year of birth: equals 1 if yob = 1921}
#' \item{YR22}{Indicator variable for the year of birth: equals 1 if yob = 1922}
#' \item{YR23}{Indicator variable for the year of birth: equals 1 if yob = 1923}
#' \item{YR24}{Indicator variable for the year of birth: equals 1 if yob = 1924}
#' \item{YR25}{Indicator variable for the year of birth: equals 1 if yob = 1925}
#' \item{YR26}{Indicator variable for the year of birth: equals 1 if yob = 1926}
#' \item{YR27}{Indicator variable for the year of birth: equals 1 if yob = 1927}
#' \item{YR28}{Indicator variable for the year of birth: equals 1 if yob = 1928}
#' \item{QTR120}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR121}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR122}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR123}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR124}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR125}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR126}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR127}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR128}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR129}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR220}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR221}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR222}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR223}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR224}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR225}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR226}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR227}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR228}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR229}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR320}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR321}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR322}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR323}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR324}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR325}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR326}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR327}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR328}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{QTR329}{Quarter-of-birth indicator interacted with year-of-birth indicator}
#' \item{CNST}{Constant}
#' }
#' 
#' @source The dataset is publicly available on Joshua Angrist's website at 
#' \url{https://economics.mit.edu/faculty/angrist/data1/data/angkru1991/}.
"AK"
