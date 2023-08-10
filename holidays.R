library(s45)
# Basisregressor
dates <- seq(as.Date("1950-01-01"), as.Date("2100-12-31"), by = "days")
Base  <- xts::xts(rep(0, length(dates)), order.by = dates)

### Bundeseinheitlich

# Neujahr
NewYearsEve <- NewYearsDay <- Base
NewYearsEve[as.Date(timeDate::NewYearsDay(1950:2100)) - 1] <- 1
NewYearsEve[length(NewYearsEve)] <- 1
NewYearsDay[as.Date(timeDate::NewYearsDay(1950:2100))] <- 1

# Ostern (Karfreitag, Ostersonntag, Ostermontag)
GoodFriday <- EasterSunday <- EasterMonday <- Base
GoodFriday[as.Date(timeDate::GoodFriday(1950:2100))] <- 1
EasterSunday[as.Date(timeDate::EasterSunday(1950:2100))] <- 1
EasterMonday[as.Date(timeDate::EasterMonday(1950:2100))] <- 1
HolySaturday <- s45:::lag0(EasterSunday,-1)
HolyThursday <- s45:::lag0(EasterSunday,-3)
EasterMondayAft1Day <- s45:::lag0(EasterMonday, 1)

# Rosenmontag
CarnivalMonday <- s45:::lag0(EasterSunday,-48)
MardiGras <- s45:::lag0(EasterSunday,-47)

# Tag der Arbeit
LabourDay <- Base
LabourDay[as.Date(timeDate::LaborDay(1950:2100))] <- 1

# US, Tag der Arbeit
USLabourDay <- Base
USLabourDay[as.Date(timeDate::USLaborDay(1950:2100))] <- 1

# Christi Himmelfahrt
Ascension <- Base
Ascension[as.Date(timeDate::Ascension(1950:2100))] <- 1
AscensionBef1Day <- s45:::lag0(Ascension,-1)
AscensionAft1Day <- s45:::lag0(Ascension, 1)

# Pfingsten (Sonntag und Montag)
Pentecost <- PentecostMonday <- Base
Pentecost[as.Date(timeDate::Pentecost(1950:2100))] <- 1
PentecostMonday[as.Date(timeDate::PentecostMonday(1950:2100))] <- 1
PentecostAft1Day <- s45:::lag0(PentecostMonday, 1)
PentecostBef1Day <- s45:::lag0(Pentecost,-1)

# Tag der Deutschen Einheit
GermanUnity <- Base
GermanUnity[as.Date(timeDate::DEGermanUnity(1950:2100))] <- 1

# Weihnachten (Heiligabend, 1. und 2. Weihnachtsferiertag)
ChristmasEve <- ChristmasDay <- BoxingDay <- Base
ChristmasEve[as.Date(timeDate::ChristmasEve(1950:2100))] <- 1
ChristmasDay[as.Date(timeDate::ChristmasDay(1950:2100))] <- 1
BoxingDay[as.Date(timeDate::BoxingDay(1950:2100))] <- 1

# Reformationstag (2017)
ReformationDay2017 <- ReformationDay <- Base
ReformationDay2017["2017-10-31"] <- 1

ReformationDay[format(zoo::index(ReformationDay), "%m-%d") == "10-31"] <-
  1

### Regionalfeiertage

# Heilige Drei Koenige (BW, BY, ST)
Epiphany <- Base
Epiphany[as.Date(timeDate::Epiphany(1950:2100))] <- 1


# Fronleichnam (BW, BY, HE, NW, RP, SL)
CorpusChristi <- Base
CorpusChristi[as.Date(timeDate::DECorpusChristi(1950:2100))] <- 1
CorpusChristiAft1Day <- s45:::lag0(CorpusChristi)
CorpusChristiBef1Day <- s45:::lag0(CorpusChristi,-1)

# Maria Himmelfahrt (BY, SL)
AssumptionOfMary <- Base
AssumptionOfMary[as.Date(timeDate::AssumptionOfMary(1950:2100))] <-
  1



# Allerheiligen (BW, BY, NW, RP, SL)
AllSaints <- Base
AllSaints[as.Date(timeDate::AllSaints(1950:2100))] <- 1



# Cross-Seasonal Effects
PreXmasSun3d <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = 3,
    Day = c("0"),
    output = "xts"
  ) # So in der Woche nach Weihnachten (inkl Heiligabend)
PreXmasSat3d <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = 3,
    Day = c("6"),
    output = "xts"
  ) # Sa in der woche nach Weihnachten (inkl Heiligabend)

PostXmasSun1w <-
  cross_seasonal(
    y = Base,
    reference = "12-27",
    span = 7,
    Day = c("0"),
    output = "xts"
  ) # So in der Woche nach Weihnachten (inkl Heiligabend)
PostXmasSat1w <-
  cross_seasonal(
    y = Base,
    reference = "12-27",
    span = 7,
    Day = c("6"),
    output = "xts"
  ) # Sa in der woche nach Weihnachten (inkl Heiligabend)
PostXmasSun10d <-
  cross_seasonal(
    y = Base,
    reference = "12-27",
    span = 10,
    Day = c("0"),
    output = "xts"
  ) # So in der Woche nach Weihnachten (inkl Heiligabend)
PostXmasSat10d <-
  cross_seasonal(
    y = Base,
    reference = "12-27",
    span = 10,
    Day = c("6"),
    output = "xts"
  ) # Sa in der woche nach Weihnachten (inkl Heiligabend)
PostNewEveSun1w <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 7,
    Day = c("0"),
    output = "xts"
  ) # So in der zweiten Woche nach Weihnachten (inkl Silvester)
PostNewEveSat1w <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 7,
    Day = c("6"),
    output = "xts"
  ) # Sa in der zweiten Woche nach Weihnachten (inkl Silvester)


Aug15Sun <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Aug15Sat <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Aug15Fri <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Aug15Thu <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Aug15Wed <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Aug15Tue <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Aug15Mon <-
  cross_seasonal(
    y = Base,
    reference = "08-15",
    span = 1,
    Day = c("1"),
    output = "xts"
  )

May1Sun <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
May1Sat <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
May1Fri <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
May1Thu <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
May1Wed <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
May1Tue <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
May1Mon <-
  cross_seasonal(
    y = Base,
    reference = "05-01",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
May1Bridge <-
  dsa:::.add(s45:::lag0(May1Tue,-1), s45:::lag0(May1Thu, 1))

Oct3Sun <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Oct3Sat <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Oct3Fri <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Oct3Thu <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Oct3Wed <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Oct3Tue <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Oct3Mon <-
  cross_seasonal(
    y = Base,
    reference = "10-03",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
Oct3Bridge <-
  dsa:::.add(s45:::lag0(Oct3Tue,-1), s45:::lag0(Oct3Thu, 1))

Oct31Sun <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Oct31Sat <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Oct31Fri <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Oct31Thu <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Oct31Wed <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Oct31Tue <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Oct31Mon <-
  cross_seasonal(
    y = Base,
    reference = "10-31",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
Oct31Bridge <-
  dsa:::.add(s45:::lag0(Oct31Tue,-1), s45:::lag0(Oct31Thu, 1))

Nov1Sun <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Nov1Sat <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Nov1Fri <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Nov1Thu <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Nov1Wed <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Nov1Tue <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Nov1Mon <-
  cross_seasonal(
    y = Base,
    reference = "11-01",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
Nov1Bridge <-
  dsa:::.add(s45:::lag0(Nov1Tue,-1), s45:::lag0(Nov1Thu, 1))



periodlength <- as.Date("2020-01-05") - as.Date("2019-12-21") + 1
XmasPeriodSun <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("0"),
    output = "xts"
  ) # Alle Sonntage in den 16 Tage ab 21.12
Dec24Sun <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Dec25Sun <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Dec26Sun <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Dec24Sat <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Dec25Sat <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Dec26Sat <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Dec24Fri <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Dec25Fri <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Dec26Fri <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Dec24Thu <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Dec25Thu <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Dec26Thu <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Dec24Wed <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Dec25Wed <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Dec26Wed <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Dec24Tue <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Dec25Tue <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Dec26Tue <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Dec24Mon <-
  cross_seasonal(
    y = Base,
    reference = "12-24",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
Dec25Mon <-
  cross_seasonal(
    y = Base,
    reference = "12-25",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
Dec26Mon <-
  cross_seasonal(
    y = Base,
    reference = "12-26",
    span = 1,
    Day = c("1"),
    output = "xts"
  )
XmasPeriodMon <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("1"),
    output = "xts"
  )
XmasPeriodTue <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("2"),
    output = "xts"
  )
XmasPeriodWed <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("3"),
    output = "xts"
  )
XmasPeriodThu <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("4"),
    output = "xts"
  )
XmasPeriodFri <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("5"),
    output = "xts"
  )
XmasPeriodSat <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("6"),
    output = "xts"
  )
XmasPeriodSun <-
  cross_seasonal(
    y = Base,
    reference = "12-21",
    span = periodlength,
    Day = c("0"),
    output = "xts"
  )

Dec31Sun <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Dec31Sat <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Dec31Fri <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Dec31Thu <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Dec31Wed <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Dec31Tue <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Dec31Mon <-
  cross_seasonal(
    y = Base,
    reference = "12-31",
    span = 1,
    Day = c("1"),
    output = "xts"
  )

Jan1Sun <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Jan1Sat <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Jan1Fri <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Jan1Thu <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Jan1Wed <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Jan1Tue <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Jan1Mon <-
  cross_seasonal(
    y = Base,
    reference = "01-01",
    span = 1,
    Day = c("1"),
    output = "xts"
  )

Jan6Sun <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("0"),
    output = "xts"
  )
Jan6Sat <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("6"),
    output = "xts"
  )
Jan6Fri <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("5"),
    output = "xts"
  )
Jan6Thu <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("4"),
    output = "xts"
  )
Jan6Wed <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("3"),
    output = "xts"
  )
Jan6Tue <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("2"),
    output = "xts"
  )
Jan6Mon <-
  cross_seasonal(
    y = Base,
    reference = "01-06",
    span = 1,
    Day = c("1"),
    output = "xts"
  )



PentecostPeriod <-
  dsa:::.add(s45:::lag0(PentecostMonday,-1),
             PentecostMonday,
             s45:::lag0(PentecostMonday, 1)) # A negative value will create a dummy before the holiday, a positive number after the holiday


EasterPeriod <-
  dsa:::.add(
    s45:::lag0(EasterMonday,-4),
    s45:::lag0(EasterMonday,-3),
    s45:::lag0(EasterMonday,-2),
    s45:::lag0(EasterMonday,-1),
    EasterMonday,
    s45:::lag0(EasterMonday, 1)
  )

# Zeitumstellung
DstSpring <- s45:::create_dst_regressor(Base, spring = 1, autumn = 0)
DstAutumn <- s45:::create_dst_regressor(Base, spring = 0, autumn = 1)
Dst <- s45:::create_dst_regressor(Base, spring = -1, autumn = 1)



# Saving
All <-
  merge(
    AllSaints,
    Ascension,
    AscensionAft1Day,
    AscensionBef1Day,
    AssumptionOfMary,
    Aug15Mon,
    Aug15Tue,
    Aug15Wed,
    Aug15Thu,
    Aug15Fri,
    Aug15Sat,
    Aug15Sun,
    Base,
    BoxingDay,
    CarnivalMonday,
    ChristmasDay,
    ChristmasEve,
    CorpusChristi,
    CorpusChristiAft1Day,
    CorpusChristiBef1Day,
    Dec24Mon,
    Dec24Tue,
    Dec24Wed,
    Dec24Thu,
    Dec24Fri,
    Dec24Sat,
    Dec24Sun,
    Dec25Mon,
    Dec25Tue,
    Dec25Wed,
    Dec25Thu,
    Dec25Fri,
    Dec25Sat,
    Dec25Sun,
    Dec26Mon,
    Dec26Tue,
    Dec26Wed,
    Dec26Thu,
    Dec26Fri,
    Dec26Sat,
    Dec26Sun,
    Dec31Mon,
    Dec31Tue,
    Dec31Wed,
    Dec31Thu,
    Dec31Fri,
    Dec31Sat,
    Dec31Sun,
    EasterMonday,
    EasterMondayAft1Day,
    EasterPeriod,
    EasterSunday,
    Epiphany,
    GermanUnity,
    GoodFriday,
    HolyThursday,
    HolySaturday,
    Jan1Mon,
    Jan1Tue,
    Jan1Wed,
    Jan1Thu,
    Jan1Fri,
    Jan1Sat,
    Jan1Sun,
    Jan6Mon,
    Jan6Tue,
    Jan6Wed,
    Jan6Thu,
    Jan6Fri,
    Jan6Sat,
    Jan6Sun,
    LabourDay,
    MardiGras,
    May1Mon,
    May1Tue,
    May1Wed,
    May1Thu,
    May1Fri,
    May1Sat,
    May1Sun,
    May1Bridge,
    NewYearsDay,
    NewYearsEve,
    Nov1Mon,
    Nov1Tue,
    Nov1Wed,
    Nov1Thu,
    Nov1Fri,
    Nov1Sat,
    Nov1Sun,
    Nov1Bridge,
    Oct3Mon,
    Oct3Tue,
    Oct3Wed,
    Oct3Thu,
    Oct3Fri,
    Oct3Sat,
    Oct3Sun,
    Oct3Bridge,
    Oct31Mon,
    Oct31Tue,
    Oct31Wed,
    Oct31Thu,
    Oct31Fri,
    Oct31Sat,
    Oct31Sun,
    Oct31Bridge,
    Pentecost,
    PentecostAft1Day,
    PentecostBef1Day,
    PentecostMonday,
    PentecostPeriod,
    PostNewEveSat1w,
    PostNewEveSun1w,
    PostXmasSat1w,
    PostXmasSun1w,
    PostXmasSat10d,
    PostXmasSun10d,
    PreXmasSat3d,
    PreXmasSun3d,
    ReformationDay,
    ReformationDay2017,
    XmasPeriodMon,
    XmasPeriodTue,
    XmasPeriodWed,
    XmasPeriodThu,
    XmasPeriodFri,
    XmasPeriodSat,
    XmasPeriodSun,
    DstSpring,
    DstAutumn,
    USLabourDay
  )

holidays <- All
saveRDS(holidays, file = "data/holidays.rds")
save(holidays, file = "data/holidays.rda")


holidaysCentered <- s45::center_regressor(All)
saveRDS(holidaysCentered, file = "data/holidaysCentered.rds")
save(holidaysCentered, file = "data/holidaysCentered.rda")
