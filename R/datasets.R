#' Penn World Tables 9.0
#'
#' A dataset containing data from the Penn World Tables version 9.0. It
#' includes data from 178 countries. The names of each country are listed
#' in the 'country' data set.
#'
#' @format A tibble with 11830 rows and 47 variables:
#' \describe{
#'   \item{countrycode}{3-letter ISO country code}
#'   \item{country}{Country name}
#'   \item{currency_unit}{Currency unit}
#'   \item{year}{Year}
#'   \item{rgdpe}{Expenditure-side real GDP at chained PPPs (in mil. 2011US$)}
#'   \item{rgdpo}{Output-side real GDP at chained PPPs (in mil. 2011US$)}
#'   \item{pop}{Population (in millions)}
#'   \item{emp}{Number of persons engaged (in millions)}
#'   \item{avh}{Average annual hours worked by persons engaged}
#'   \item{hc}{Human capital index}
#'   \item{ccon}{Real consumption of households and government, at current PPPs (in mil. 2011US$)}
#'   \item{cda}{Real domestic absorption (consumption + investment), at current PPPs (in mil. 2011US$)}
#'   \item{cgdpe}{Expenditure-side real GDP at current PPPs (in mil. 2011US$)}
#'   \item{cgdpo}{Output-side real GDP at current PPPs (in mil. 2011US$)}
#'   \item{ck}{Capital stock at current PPPs (in mil. 2011US$)}
#'   \item{ctfp}{TFP level at current PPPs (USA=1)}
#'   \item{cwtfp}{Welfare-relevant TFP levels at current PPPs (USA=1)}
#'   \item{rgdpna}{Real GDP at constant 2011 national prices (in mil. 2011US$)}
#'   \item{rconna}{Real consumption at constant 2011 national prices (in mil. 2011US$)}
#'   \item{rdana}{Real domestic absorption at constant 2011 national prices (in mil. 2011US$)}
#'   \item{rkna}{Capital stock at constant 2011 national prices (in mil. 2011US$)}
#'   \item{rtfpna}{TFP at constant national prices (2011=1)}
#'   \item{rwtfpna}{Welfare-relevant TFP at constant national prices (2011=1)}
#'   \item{labsh}{Share of labour compensation in GDP at current national prices}
#'   \item{delta}{Average depreciation rate of the capital stock}
#'   \item{xr}{Exchange rate, national currency/USD}
#'   \item{pl_con}{Price level of CCON (PPP/XR), price level of USA GDPo in 2011=1}
#'   \item{pl_da}{Price level of CDA (PPP/XR), price level of USA GDPo in 2011=1}
#'   \item{pl_gdpo}{Price level of CGDPo (PPP/XR), price level of USA GDPo in 2011=1}
#'   \item{i_cig}{Is relative price data for consumption, investment, and government extrapolated (0), benchmark (1), or interpolated (2)?}
#'   \item{i_xm}{Is relative price data for exports and imports extrapolated (0), benchmark (1), or interpolated (2)?}
#'   \item{i_xr}{Is the exchange rate market-based (0) or estimated (1)?}
#'   \item{i_outlier}{Is the observation on pl_gdpe or pl_gdpo an outlier? (1)}
#'   \item{cor_exp}{Correlation between expenditure shares of the country and the US}
#'   \item{statcap}{Statistical capacity indicator}
#'   \item{csh_c}{Share of household consumption at current PPPs}
#'   \item{csh_i}{Share of gross capital formation at current PPPs}
#'   \item{csh_g}{Share of government consumption at current PPPs}
#'   \item{csh_x}{Share of merchandise exports at current PPPs}
#'   \item{csh_m}{Share of merchandise imports at current PPPs}
#'   \item{csh_r}{Share of residual trade and GDP statistical discrepancy at current PPPs}
#'   \item{pl_c}{Price level of household consumption, price level of USA in 2011=1}
#'   \item{pl_i}{Price level of capital formation, price level of USA in 2011=1}
#'   \item{pl_g}{Price level of government consumption, price level of USA in 2011=1}
#'   \item{pl_x}{Price level of exports, price level of USA in 2011=1}
#'   \item{pl_m}{Price level of imports, price level of USA in 2011=1}
#'   \item{pl_k}{Price level of the capital stock, price level of USA in 2011=1}
#' }
#' @source \url{http://www.rug.nl/ggdc/productivity/pwt/}
"pwt9"
#' Penn World Tables 9.0 Country List
#'
#' @format A tibble with 182 rows and 2 variables:
#' \describe{
#'   \item{countrycode}{countrycode}
#'   \item{country}{country}
#' }
"country"
#' Homework 1 Data Set
#'
#' @format A tibble with 282 rows and 3 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{Cons}{Real personal consumption expenditures per capita}
#'   \item{Price}{Consumer Price Index for All Urban Consumers: All Items}
#' }
"hw1"
#' Homework 2 Data Set
#'
#' @format A tibble with 295 rows and 9 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{Foo}{Consumer Price Index for All Urban Consumers: Food and Beverages}
#'   \item{Hou}{Consumer Price Index for All Urban Consumers: Housing}
#'   \item{App}{Consumer Price Index for All Urban Consumers: Apparel}
#'   \item{Tra}{Consumer Price Index for All Urban Consumers: Transportation}
#'   \item{Med}{Consumer Price Index for All Urban Consumers: Medical Care}
#'   \item{Rec}{Consumer Price Index for All Urban Consumers: Recreation}
#'   \item{Edu}{Consumer Price Index for All Urban Consumers: Education and Communication}
#'   \item{Oth}{Consumer Price Index for All Urban Consumers: Other Goods and Services}
#' }
"hw2"
#' Homework 5 Data Set
#'
#' @format A tibble with 234 rows and 4 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{w}{Compensation of Employees, Received: Wage and Salary Disbursements}
#'   \item{u}{Civilian Unemployment Rate}
#'   \item{un}{Natural Rate of Unemployment (Long-Term)}
#' }
"hw5"
#' Homework 6 Data Set
#'
#' @format A tibble with 234 rows and 4 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{P}{Consumer Price Index for All Urban Consumers: All Items}
#'   \item{M}{M2 Money Stock}
#'   \item{Y}{Real Gross Domestic Product}
#' }
"hw6"
#' Homework 7 Data Set
#'
#' @format A tibble with 257 rows and 6 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{P}{Consumer Price Index for All Urban Consumers: All Items}
#'   \item{i}{10-Year Treasury Constant Maturity Rate}
#'   \item{Y}{Real Gross Domestic Product}
#'   \item{pY}{Real Potential Gross Domestic Product}
#'   \item{G}{Real Government Consumption Expenditures and Gross Investment}
#' }
"hw7"
#' Homework 8 Data Set
#'
#' @format A tibble with 230 rows and 4 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{pi}{Inflation Rate from Personal Consumption Expenditures Chain-Type Price Index Less Food and Energy}
#'   \item{y}{GDP Gap}
#'   \item{i}{Federal Funds Rate}
#' }
"hw8"
#' Labor Market Data
#'
#' @format A tibble with 136 rows and 38 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{rec}{NBER based Recession Indicators for the United States from the Period following the Peak through the Trough}
#'   \item{lmci}{Change in Labor Market Conditions Index (DISCONTINUED)}
#'   \item{u3}{Civilian Unemployment Rate}
#'   \item{u1}{Persons Unemployed 15 weeks or longer, as a percent of the civilian labor force}
#'   \item{u2}{Unemployment Rate: Job Losers}
#'   \item{u4}{Special Unemployment Rate: Unemployed and Discouraged Workers}
#'   \item{u5}{Special Unemployment Rate: Unemployed and Marginally Attached Workers}
#'   \item{u6}{Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons}
#'   \item{unemp}{Unemployment Level}
#'   \item{emp}{Civilian Employment Level}
#'   \item{pr}{Civilian Labor Force Participation Rate}
#'   \item{pr.men}{Civilian Labor Force Participation Rate: Men}
#'   \item{pr.fem}{Civilian Labor Force Participation Rate: Women}
#'   \item{vac}{Job Openings: Total Private}
#'   \item{hires}{Hires: Total Private}
#'   \item{sep}{Total Separations: Total Private}
#'   \item{quits}{Quits: Total Private}
#'   \item{layoff}{Layoffs and Discharges: Total Private}
#'   \item{earn}{Average Hourly Earnings of All Employees: Total Private}
#'   \item{hours}{Average Weekly Hours of All Employees: Total Private}
#'   \item{hrns}{Average Hourly Earnings of Production and Nonsupervisory Employees: Total Private}
#'   \item{hrman}{Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing}
#'   \item{dur}{Average (Mean) Duration of Unemployment}
#'   \item{dur.med}{Median Duration of Unemployment}
#'   \item{em}{All Employees: Total Private Industries}
#'   \item{emcon}{All Employees: Construction}
#'   \item{emgov}{All Employees: Government}
#'   \item{emtrade}{All Employees: Retail Trade}
#'   \item{emwtrade}{All Employees: Wholesale Trade}
#'   \item{emfin}{All Employees: Financial Activities}
#'   \item{emmine}{All Employees: Mining and logging}
#'   \item{emehs}{All Employees: Education and Health Services}
#'   \item{empbs}{All Employees: Professional and Business Services}
#'   \item{eminf}{All Employees: Information Services}
#'   \item{emlh}{All Employees: Leisure and Hospitality}
#'   \item{emttu}{All Employees: Trade, Transportation and Utilities}
#'   \item{emman}{All Employees: Manufacturing}
#' }
"labor"
#' General Macroeconomics Data
#'
#' @format A tibble with 205 rows and 29 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{rec}{NBER based Recession Indicators for the United States from the Period following the Peak through the Trough}
#'   \item{gdp}{Gross Domestic Product}
#'   \item{rgdp}{Real Gross Domestic Product}
#'   \item{pgdp}{Real Potential Gross Domestic Product}
#'   \item{deflator}{Gross Domestic Product: Implicit Price Deflator}
#'   \item{cpi}{Consumer Price Index for All Urban Consumers: All Items}
#'   \item{pce}{Personal Consumption Expenditures Excluding Food and Energy (Chain-Type Price Index)}
#'   \item{m2}{M2 Money Stock}
#'   \item{m2v}{Velocity of M2 Money Stock}
#'   \item{u}{Civilian Unemployment Rate}
#'   \item{un}{Natural Rate of Unemployment (Long-Term)}
#'   \item{emp}{Civilian Employment Level}
#'   \item{unemp}{Unemployment Level}
#'   \item{workpop}{Working Age Population: Aged 15-64: All Persons for the United States©}
#'   \item{ffr}{Effective Federal Funds Rate}
#'   \item{i10}{10-Year Treasury Constant Maturity Rate}
#'   \item{c}{Personal Consumption Expenditures}
#'   \item{i}{Gross Private Domestic Investment}
#'   \item{g}{Government Consumption Expenditures and Gross Investment}
#'   \item{ex}{Exports of Goods and Services}
#'   \item{im}{Imports of Goods and Services}
#'   \item{rc}{Real Personal Consumption Expenditures}
#'   \item{ri}{Real Gross Private Domestic Investment}
#'   \item{rg}{Real Government Consumption Expenditures and Gross Investment}
#'   \item{rex}{Real Exports of Goods and Services}
#'   \item{rim}{Real imports of goods and services}
#'   \item{debt}{Federal Debt: Total Public Debt}
#'   \item{pop}{Population (midperiod)}
#' }
"macro"
#' National Income and Products Accounts
#'
#' @format A tibble with 282 rows and 15 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{rec}{NBER based Recession Indicators for the United States from the Period following the Peak through the Trough}
#'   \item{GDP}{Gross Domestic Product}
#'   \item{Deflator}{Gross Domestic Product: Implicit Price Deflator}
#'   \item{C}{Personal Consumption Expenditures}
#'   \item{I}{Gross Private Domestic Investment}
#'   \item{G}{Government Consumption Expenditures and Gross Investment}
#'   \item{EX}{Exports of Goods and Services}
#'   \item{IM}{Imports of Goods and Services}
#'   \item{dg}{Personal Consumption Expenditures: Durable Goods}
#'   \item{nd}{Personal Consumption Expenditures: Nondurable Goods}
#'   \item{sv}{Personal Consumption Expenditures: Services}
#'   \item{cbi}{Change in Private Inventories}
#'   \item{nres}{Private Nonresidential Fixed Investment}
#'   \item{res}{Private Residential Fixed Investment}
#' }
"nipa"
#' Real National Income and Products Accounts
#'
#' @format A tibble with 282 rows and 8 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{rec}{NBER based Recession Indicators for the United States from the Period following the Peak through the Trough}
#'   \item{Y}{Real Gross Domestic Product}
#'   \item{C}{Real Personal Consumption Expenditures}
#'   \item{I}{Real Gross Private Domestic Investment}
#'   \item{G}{Real Government Consumption Expenditures and Gross Investment}
#'   \item{X}{Real Exports of Goods and Services}
#'   \item{M}{Real imports of goods and services}
#' }
"rnipa"
#' New Keynesian Model Data
#'
#' @format A tibble with 234 rows and 7 variables:
#' \describe{
#'   \item{Date}{Date}
#'   \item{Y}{Real Gross Domestic Product}
#'   \item{pY}{Real Potential Gross Domestic Product}
#'   \item{U}{Civilian Unemployment Rate}
#'   \item{Un}{Natural Rate of Unemployment (Long-Term)}
#'   \item{P}{Personal Consumption Expenditures Excluding Food and Energy (Chain-Type Price Index)}
#'   \item{FFR}{Effective Federal Funds Rate}
#' }
"nk"
