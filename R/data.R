#' Data set for frequently used regressors
#' 
#' Daily time series in xts format containing many regressors for holidays potentially used in the adjustment of daily time series
#' @format An xts data set containing 131 regressors for the time span 1950 to 2075:
#' \describe{
#' \item{AllSaints}{AllSaints, Nov 1}
#' \item{Ascension}{Ascension}
#' \item{AscensionAft1Day}{Captures the first day after Ascension}
#' \item{AscensionBef1Day}{Captures the last day before Ascension}
#' \item{AssumptionOfMary}{Assumption of Mary, Aug 15}
#' \item{Aug15ZZZ}{Captures if Assumption of Mary, Aug 15, is a certain weekday (Monday to Sunday)}
#' \item{Base}{Regressor made up of 0s, can be used to create other regressors}
#' \item{BoxingDay}{Boxing Day, Dec 26}
#' \item{CarnivalMonday}{Carnival Monday}
#' \item{ChristmasDay}{Christmas Day, Dec 25}
#' \item{ChristmasEve}{Christmas Eve, Dec 24}
#' \item{CorpusChristi}{Corpus Christi}
#' \item{CorpusChristiAft1Day}{Captures the first day after Corpus Christi}
#' \item{CorpusChristiBef1Day}{Captures the last day before Corpus Christi}
#' \item{Dec24ZZZ}{Captures if Dec 24 is a certain weekday (Monday to Sunday)}
#' \item{Dec25ZZZ}{Captures if Dec 25 is a certain weekday (Monday to Sunday)}
#' \item{Dec26ZZZ}{Captures if Dec 26 is a certain weekday (Monday to Sunday)}
#' \item{Dec31ZZZ}{Captures if Dec 31 is a certain weekday (Monday to Sunday)}
#' \item{Dst}{Daylight Saving Time, Spring=-1, Autumn=1}
#' \item{DstAutumn}{Daylight Saving Time, Autumn=1}
#' \item{DstSpring}{Daylight Saving Time, Spring=1}
#' \item{EasterMonday}{Easter Monday}
#' \item{EasterMondayAft1Day}{Captures the first day after Easter Monday}
#' \item{EasterPeriod}{Captures all days from Holy Thursday to Tuesday after Easter Monday}
#' \item{EasterSunday}{Easter Sunday}
#' \item{Epiphany}{Epiphany, Jan 6}
#' \item{GermanUnity}{German Unity, Oct 3}
#' \item{GoodFriday}{Good Friday}
#' \item{HolyThursday}{Holy Thursday}
#' \item{HolySaturday}{Holy Saturday}
#' \item{Jan1ZZZ}{Captures if Jan 1 is a certain weekday (Monday to Sunday)}
#' \item{Jan6ZZZ}{Captures if Jan 1 is a certain weekday (Monday to Sunday)}
#' \item{LabourDay}{Labour Day, May 1}
#' \item{MardiGras}{Mardi Gras}
#' \item{May1Bridge}{Captures the bridge days created by May 1, i.e. if surrounding days are either a Monday or Friday}
#' \item{May1ZZZ}{Captures if Labour Day, May 1, is a certain weekday (Monday to Sunday)}
#' \item{NewYearsDay}{New Years Day, Jan 1}
#' \item{NewYearsEve}{New Years Eve, Dec 31}
#' \item{Nov1ZZZ}{Captures if Nov 1 is a certain weekday (Monday to Sunday)}
#' \item{Nov1Bridge}{Captures the bridge days created by Nov 1, i.e. if surrounding days are either a Monday or Friday}
#' \item{Oct3ZZZ}{Captures if German Unity, Oct 3, is a certain weekday (Monday to Sunday)}
#' \item{Oct3Bridge}{Captures the bridge days created by Oct 3, i.e. if surrounding days are either a Monday or Friday}
#' \item{Oct31ZZZ}{Captures if Reformation Day, Oct 31, is a certain weekday (Monday to Sunday)}
#' \item{Oct31Bridge}{Captures the bridge days created by Reformation Day, i.e. if surrounding days are either a Monday or Friday}
#' \item{Pentecost}{Pentecost Sunday}
#' \item{PentecostAft1Day}{Captures the first day after Pentecost Sunday}
#' \item{PentecostBef1Day}{Captures the last day before Pentecost Sunday}
#' \item{PentecostMonday}{Alias for Pentecost Monday}
#' \item{PentecostPeriod}{Period spanning three days from Pentecost Sunday to Tuesday after Pentecost Monday}
#' \item{PostNewEveSat1w}{Captures Saturdays in the period from Dec 31 to Jan 6}
#' \item{PostNewEveSun1w}{Captures Sundays in the period from Dec 31 to Jan 6}
#' \item{PostXmasSat1w}{Captures Saturdays in the period from Dec 27 to Jan 2}
#' \item{PostXmasSun1w}{Captures Sundays in the period from Dec 27 to Jan 2}
#' \item{PostXmasSat10d}{Captures Saturdays in the period from Dec 27 to Jan 5}
#' \item{PostXmasSun10d}{Captures Sundays in the period from Dec 27 to Jan 5}
#' \item{PreXmasSat3d}{Captures Saturdays in the three days leading up to Christmas}
#' \item{PreXmasSun3d}{Captures Sundays in the three days leading up to Christmas}
#' \item{ReformationDay}{Reformation Day, Oct 31}
#' \item{ReformationDay2017}{Reformation Day, Oct 31 2017 (National holiday that year)}
#' \item{USLabourDay}{US Labour Day}
#' \item{XmasPeriodZZZ}{Captures weekdays (Monday to Sunday) in the Christmas period from Dec 21 to Jan 5}
#' }
#' @source Own calculations
#' @author Daniel Ollech
"holidays"



#' Exemplary time series
#' 
#' Three time series that have been analysed by Ollech (2021) and their seasonally and calendar adjusted variants.
#' @format An xts data set containing 3 time series:
#' \describe{
#' \item{currency_circulation}{Currency in circulation in Germany, in billion Euros, sum of small denominations: i.e. 5 Euro + 10 Euro + 20 Euro + 50 Euro. Series compiled by Deutsche Bundesbank}
#' \item{elec_consumption}{Electricity consumption in Germany in GWh. Compiled by Bundesnetzagentur (German Federal Network Agency)}
#' \item{no2}{Nitrogen dioxide (NO2) immissions averaged over all available measuring stations in Europe that are made available by the European Environment Agency (EEA)}
#' #' \item{currency_circulation_sa}{Seasonally and calendar adjusted version using dsa of currency_circulation}
#' \item{elec_consumption_sa}{Seasonally and calendar adjusted version using dsa of elec_consumption}
#' \item{no2_sa}{Seasonally and calendar adjusted version using dsa of no2}
#' }
#' @source Own calculations, Deutsche Bundesbank, Bundesnetzagentur, EEA
#' @references Ollech, Daniel (2021). Seasonal Adjustment of Daily Time Series. Journal of Time Series Econometrics 13 (2), 235-264.
#' @author Daniel Ollech
"daily_data"

#' Centered version of xts of all German regressors used for daily time series
#' 
#' This is the centered version of the holidays regressors. See ?dsa2::holidays for details
#' @author Daniel Ollech
"holidaysCentered"

