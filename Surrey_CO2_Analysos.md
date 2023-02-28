Surrey Climate Commission
================
Prado
2023-02-05

Read the data from the UK Local authority

``` r
datos<-read.csv("C:/Users/pradi/OneDrive/Desktop/ESG/uk-local-authority-ghg-emissions-2020-dataset.csv",header = T,sep=",")
head(datos)
```

    ##   Country Country.Code        Region Region.Code Second.Tier.Authority
    ## 1 England    E92000001 East Midlands   E12000004                 Derby
    ## 2 England    E92000001 East Midlands   E12000004                 Derby
    ## 3 England    E92000001 East Midlands   E12000004                 Derby
    ## 4 England    E92000001 East Midlands   E12000004                 Derby
    ## 5 England    E92000001 East Midlands   E12000004                 Derby
    ## 6 England    E92000001 East Midlands   E12000004                 Derby
    ##   Local.Authority Local.Authority.Code Calendar.Year LA.GHG.Sector
    ## 1           Derby            E06000015          2005   Agriculture
    ## 2           Derby            E06000015          2005   Agriculture
    ## 3           Derby            E06000015          2005   Agriculture
    ## 4           Derby            E06000015          2005   Agriculture
    ## 5           Derby            E06000015          2005   Agriculture
    ## 6           Derby            E06000015          2005   Agriculture
    ##         LA.GHG.Sub.sector Greenhouse.gas Territorial.emissions..kt.CO2e.
    ## 1 Agriculture Electricity            CH4                     0.010736914
    ## 2 Agriculture Electricity            CO2                     0.380289221
    ## 3 Agriculture Electricity            N2O                     0.001710305
    ## 4         Agriculture Gas            CH4                     0.021120723
    ## 5         Agriculture Gas            CO2                     0.700437780
    ## 6         Agriculture Gas            N2O                     0.000557627
    ##   CO2.emissions.within.the.scope.of.influence.of.LAs..kt.CO2e.
    ## 1                                                    0.0000000
    ## 2                                                    0.3802892
    ## 3                                                    0.0000000
    ## 4                                                    0.0000000
    ## 5                                                    0.7004378
    ## 6                                                    0.0000000
    ##   Mid.year.Population..thousands. Area..km2.
    ## 1                          236.47    78.0311
    ## 2                          236.47    78.0311
    ## 3                          236.47    78.0311
    ## 4                          236.47    78.0311
    ## 5                          236.47    78.0311
    ## 6                          236.47    78.0311

``` r
str(datos)
```

    ## 'data.frame':    467443 obs. of  15 variables:
    ##  $ Country                                                     : chr  "England" "England" "England" "England" ...
    ##  $ Country.Code                                                : chr  "E92000001" "E92000001" "E92000001" "E92000001" ...
    ##  $ Region                                                      : chr  "East Midlands" "East Midlands" "East Midlands" "East Midlands" ...
    ##  $ Region.Code                                                 : chr  "E12000004" "E12000004" "E12000004" "E12000004" ...
    ##  $ Second.Tier.Authority                                       : chr  "Derby" "Derby" "Derby" "Derby" ...
    ##  $ Local.Authority                                             : chr  "Derby" "Derby" "Derby" "Derby" ...
    ##  $ Local.Authority.Code                                        : chr  "E06000015" "E06000015" "E06000015" "E06000015" ...
    ##  $ Calendar.Year                                               : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
    ##  $ LA.GHG.Sector                                               : chr  "Agriculture" "Agriculture" "Agriculture" "Agriculture" ...
    ##  $ LA.GHG.Sub.sector                                           : chr  "Agriculture Electricity" "Agriculture Electricity" "Agriculture Electricity" "Agriculture Gas" ...
    ##  $ Greenhouse.gas                                              : chr  "CH4" "CO2" "N2O" "CH4" ...
    ##  $ Territorial.emissions..kt.CO2e.                             : num  0.01074 0.38029 0.00171 0.02112 0.70044 ...
    ##  $ CO2.emissions.within.the.scope.of.influence.of.LAs..kt.CO2e.: num  0 0.38 0 0 0.7 ...
    ##  $ Mid.year.Population..thousands.                             : num  236 236 236 236 236 ...
    ##  $ Area..km2.                                                  : num  78 78 78 78 78 ...

``` r
colnames(datos)<-c("Country","Country_Code","Region","Region_Code","Second_Tier_Authority",
                    "Local_Authority","Local_Authority_Code","Calendar_Year","LA_GHG_Sector",
                     "LA_GH_SubSector","Greenhouse_Gas","Territorial_Emissions_Kt_Co2",
                    "CO2_Emissions_Within_LA_Scope_KtCO2e",
                     "Mid_Year_Population_Thousands","Area_Km2")
```

\###Analysis of Missing Data

1)  Filtering data for Surrey. I take the “Second Tier Authority” at
    Surrey Level data set and analysis missing data in the CO2 Emissions
    data.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
surreyData<-datos%>%filter(Second_Tier_Authority=="Surrey")%>%select(Second_Tier_Authority,Local_Authority,Calendar_Year,LA_GHG_Sector,LA_GH_SubSector,Territorial_Emissions_Kt_Co2,CO2_Emissions_Within_LA_Scope_KtCO2e,Mid_Year_Population_Thousands,Area_Km2)

str(surreyData)
```

    ## 'data.frame':    13531 obs. of  9 variables:
    ##  $ Second_Tier_Authority               : chr  "Surrey" "Surrey" "Surrey" "Surrey" ...
    ##  $ Local_Authority                     : chr  "Elmbridge" "Elmbridge" "Elmbridge" "Elmbridge" ...
    ##  $ Calendar_Year                       : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
    ##  $ LA_GHG_Sector                       : chr  "Agriculture" "Agriculture" "Agriculture" "Agriculture" ...
    ##  $ LA_GH_SubSector                     : chr  "Agriculture Electricity" "Agriculture Electricity" "Agriculture Electricity" "Agriculture Gas" ...
    ##  $ Territorial_Emissions_Kt_Co2        : num  0.04057 1.43703 0.00646 0.03243 1.0756 ...
    ##  $ CO2_Emissions_Within_LA_Scope_KtCO2e: num  0 1.44 0 0 1.08 ...
    ##  $ Mid_Year_Population_Thousands       : num  127 127 127 127 127 ...
    ##  $ Area_Km2                            : num  96.3 96.3 96.3 96.3 96.3 ...

``` r
#df_status(surreyData)
```

There are no missing values or zeros so the database seems tidy.

2)  aggregate total accumulated CO2 Emissions for each Second Tier
    Authority cross all the years Give me Surrey’s position ranked from
    top emiters to down

``` r
library(ggplot2)
ordered<-datos%>%filter(Calendar_Year==2020, !(Second_Tier_Authority %in% c("Scotland","Wales","Northern Ireland")) )%>%group_by(Second_Tier_Authority)%>%summarise(Total=sum(Territorial_Emissions_Kt_Co2))%>%arrange(desc(Total))%>%mutate(ranked_LA=rank(-Total,))

Surrey_ranking<-subset(ordered[,c("Second_Tier_Authority","Total","ranked_LA")],Second_Tier_Authority=="Surrey")
Surrey_ranking
```

    ## # A tibble: 1 × 3
    ##   Second_Tier_Authority Total ranked_LA
    ##   <chr>                 <dbl>     <dbl>
    ## 1 Surrey                5484.        14

``` r
ggplot(head(ordered,20), aes(x=reorder(Second_Tier_Authority,-Total), y=Total,fill=Second_Tier_Authority))+geom_bar(stat="identity")+
  scale_x_discrete(breaks = levels(ordered$Second_Tier_Authority))+
       xlab("Second Authority Level") + 
       ylab ("Total CO2 emissions") 
```

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Aggregate Emissions per Second tier Authority per Years 2018, 2019 and
2020

``` r
emission_sector<-datos%>%filter(Calendar_Year %in% c(2018,2019,2020),Second_Tier_Authority=="Surrey")%>%
               group_by(Calendar_Year,Second_Tier_Authority,LA_GHG_Sector)%>%
               summarise(Total_CO2=sum(Territorial_Emissions_Kt_Co2))  
```

    ## `summarise()` has grouped output by 'Calendar_Year', 'Second_Tier_Authority'.
    ## You can override using the `.groups` argument.

``` r
emission_sector
```

    ## # A tibble: 24 × 4
    ## # Groups:   Calendar_Year, Second_Tier_Authority [3]
    ##    Calendar_Year Second_Tier_Authority LA_GHG_Sector    Total_CO2
    ##            <int> <chr>                 <chr>                <dbl>
    ##  1          2018 Surrey                Agriculture           164.
    ##  2          2018 Surrey                Commercial            575.
    ##  3          2018 Surrey                Domestic             2013.
    ##  4          2018 Surrey                Industry              436.
    ##  5          2018 Surrey                LULUCF               -218.
    ##  6          2018 Surrey                Public Sector         296.
    ##  7          2018 Surrey                Transport            3094.
    ##  8          2018 Surrey                Waste management      182.
    ##  9          2019 Surrey                Agriculture           160.
    ## 10          2019 Surrey                Commercial            495.
    ## # … with 14 more rows

``` r
datos%>%filter(Calendar_Year %in% c(2018,2019,2020),Second_Tier_Authority=="Surrey")%>%
               group_by(Calendar_Year)%>%
               summarise(Total_CO2=sum(Territorial_Emissions_Kt_Co2)) 
```

    ## # A tibble: 3 × 2
    ##   Calendar_Year Total_CO2
    ##           <int>     <dbl>
    ## 1          2018     6542.
    ## 2          2019     6209.
    ## 3          2020     5484.

Plot the difference in CO2 emitted per GHG Sector for the three years
2018, 2019,2020

``` r
femission_sector<-emission_sector%>%filter(LA_GHG_Sector %in% c("Industry", "Commercial","Agriculture","Domestic","Transport","LULUCF"))%>% mutate(year=factor(Calendar_Year), FLA_GHG_Sector=factor(LA_GHG_Sector))

ggplot(femission_sector,aes(x=year,y=Total_CO2,group=FLA_GHG_Sector,color=FLA_GHG_Sector,alpha=1))+geom_line(size=2)+
    theme_classic()+
    geom_point( size = 2)+ geom_label(aes(label = round(Total_CO2,1)), 
             size = 3, 
             label.padding = unit(0.1, "lines"), 
             label.size = 0.0)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(femission_sector,aes(x=FLA_GHG_Sector,y=Total_CO2,fill=year))+
   geom_bar(stat="identity", position = "dodge")+  
  geom_label(aes(label = round(Total_CO2,0)),colour = "black", size = 3,vjust = 0, position = position_dodge(1))+labs(title="Surrey Sector CO2 Emissions 2018-2020", x="Sector",y="Total CO2 Emissions",colour = "Cylinders", caption = "(based on data from  ONS Local Authority CO2 emissions estimates 2005-2020 (kt CO2) )")+theme_classic()+scale_fill_brewer()
```

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

# total CO2 aggregated by Year and energy type (LA_GHG_Subsector),

``` r
#Filtramos los datos para Surrey
Energy_Type<-surreyData%>%filter(Calendar_Year %in% c(2018,2019,2020))%>%group_by(Calendar_Year,LA_GHG_Sector,LA_GH_SubSector)%>%summarise(Total_energy_Type=round(sum(Territorial_Emissions_Kt_Co2),2))
```

    ## `summarise()` has grouped output by 'Calendar_Year', 'LA_GHG_Sector'. You can
    ## override using the `.groups` argument.

``` r
#Create a new variable with the type of Energy consumed, Electricity, Gas, Fuel or Others

Energy_filtered<-Energy_Type%>%mutate(electricity_type=ifelse(grepl("Electricity",LA_GH_SubSector)==TRUE,"Electricity",
                                            ifelse(grepl("Gas",LA_GH_SubSector)==TRUE,"Gas", 
                                                   ifelse(LA_GH_SubSector %in% c("Road Transport (A roads)",  "Road Transport (Minor roads)","Road Transport (Motorways)"),"Fuel", ifelse(grepl("'Other'",LA_GH_SubSector)==TRUE,"Others_1","Others_2" )))))

Energy_filtered
```

    ## # A tibble: 93 × 5
    ## # Groups:   Calendar_Year, LA_GHG_Sector [24]
    ##    Calendar_Year LA_GHG_Sector LA_GH_SubSector         Total_energy_Type elect…¹
    ##            <int> <chr>         <chr>                               <dbl> <chr>  
    ##  1          2018 Agriculture   Agriculture 'Other'                 24.2  Others…
    ##  2          2018 Agriculture   Agriculture Electricity              9.32 Electr…
    ##  3          2018 Agriculture   Agriculture Gas                     19.9  Gas    
    ##  4          2018 Agriculture   Agriculture Livestock               85.2  Others…
    ##  5          2018 Agriculture   Agriculture Soils                   25.4  Others…
    ##  6          2018 Commercial    Commercial 'Other'                   4.73 Others…
    ##  7          2018 Commercial    Commercial Electricity             387.   Electr…
    ##  8          2018 Commercial    Commercial Gas                     184.   Gas    
    ##  9          2018 Domestic      Domestic 'Other'                   103.   Others…
    ## 10          2018 Domestic      Domestic Electricity               519.   Electr…
    ## # … with 83 more rows, and abbreviated variable name ¹​electricity_type

``` r
#AGroup by Sector so we drop the subsectors.

Energy_aggregated<-Energy_filtered%>%group_by(Calendar_Year,LA_GHG_Sector,electricity_type)%>%summarise(Total=sum(Total_energy_Type))
```

    ## `summarise()` has grouped output by 'Calendar_Year', 'LA_GHG_Sector'. You can
    ## override using the `.groups` argument.

``` r
#Finally, Agregamos por Year y por Tipo de electricidad. It coincides with the report!
energ_type_agg<-aggregate(Total~as.factor(Calendar_Year)+as.factor(electricity_type),data =Energy_aggregated,sum)
```

``` r
#Using Separate function we can create a better variable as above but faster and more efficiently and accurate
library(tidytable)
```

    ## As of tidytable v0.9.0 dotless versions of functions are exported.
    ## You can now use `arrange()`/`mutate()`/etc. directly.

    ## Warning: tidytable was loaded after dplyr.
    ## This can lead to most dplyr functions being overwritten by tidytable functions.

    ## 
    ## Attaching package: 'tidytable'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     across, add_count, add_tally, anti_join, arrange, between,
    ##     bind_cols, bind_rows, c_across, case_when, coalesce, count,
    ##     cume_dist, cur_column, cur_data, cur_group_id, cur_group_rows,
    ##     dense_rank, desc, distinct, filter, first, full_join, group_by,
    ##     group_cols, group_split, group_vars, if_all, if_any, if_else,
    ##     inner_join, is_grouped_df, lag, last, lead, left_join, min_rank,
    ##     mutate, n, n_distinct, na_if, nest_by, nest_join, nth,
    ##     percent_rank, pull, recode, relocate, rename, rename_with,
    ##     right_join, row_number, rowwise, select, semi_join, slice,
    ##     slice_head, slice_max, slice_min, slice_sample, slice_tail,
    ##     summarise, summarize, tally, top_n, transmute, ungroup

    ## The following objects are masked from 'package:stats':
    ## 
    ##     dt, filter, lag

    ## The following object is masked from 'package:base':
    ## 
    ##     %in%

``` r
enerFilt<-Energy_Type%>%separate(LA_GH_SubSector,into = c("GHG_Sector","Energy_Type"),sep=" ")
head(enerFilt)
```

    ## # A tidytable: 6 × 5
    ##   Calendar_Year LA_GHG_Sector Total_energy_Type GHG_Sector  Energy_Type
    ##           <int> <chr>                     <dbl> <chr>       <chr>      
    ## 1          2018 Agriculture               24.2  Agriculture 'Other'    
    ## 2          2018 Agriculture                9.32 Agriculture Electricity
    ## 3          2018 Agriculture               19.9  Agriculture Gas        
    ## 4          2018 Agriculture               85.2  Agriculture Livestock  
    ## 5          2018 Agriculture               25.4  Agriculture Soils      
    ## 6          2018 Commercial                 4.73 Commercial  'Other'

``` r
EnerAgg<-enerFilt%>%group_by(Calendar_Year,LA_GHG_Sector,Energy_Type)%>%summarise(Total=sum(Total_energy_Type))
EnerAgg
```

    ## # A tidytable: 66 × 4
    ## # Groups:      Calendar_Year, LA_GHG_Sector
    ##    Calendar_Year LA_GHG_Sector Energy_Type  Total
    ##            <int> <chr>         <chr>        <dbl>
    ##  1          2018 Agriculture   'Other'      24.2 
    ##  2          2018 Agriculture   Electricity   9.32
    ##  3          2018 Agriculture   Gas          19.9 
    ##  4          2018 Agriculture   Livestock    85.2 
    ##  5          2018 Agriculture   Soils        25.4 
    ##  6          2018 Commercial    'Other'       4.73
    ##  7          2018 Commercial    Electricity 387.  
    ##  8          2018 Commercial    Gas         184.  
    ##  9          2018 Domestic      'Other'     103.  
    ## 10          2018 Domestic      Electricity 519.  
    ## # … with 56 more rows

``` r
Energy_agg<-EnerAgg%>%filter(Energy_Type %in% c("Electricity","Gas","'Other'","Transport"))%>%group_by(Energy_Type,Calendar_Year)%>%summarise(Total_CO2_EnergyType=sum(Total,na.rm=T))
Energy_agg
```

    ## # A tidytable: 12 × 3
    ## # Groups:      Energy_Type
    ##    Energy_Type Calendar_Year Total_CO2_EnergyType
    ##    <chr>               <int>                <dbl>
    ##  1 'Other'              2018                 391.
    ##  2 'Other'              2019                 368.
    ##  3 'Other'              2020                 370.
    ##  4 Electricity          2018                1054.
    ##  5 Electricity          2019                 937.
    ##  6 Electricity          2020                 816.
    ##  7 Gas                  2018                1663.
    ##  8 Gas                  2019                1603.
    ##  9 Gas                  2020                1584.
    ## 10 Transport            2018                3034.
    ## 11 Transport            2019                2942.
    ## 12 Transport            2020                2393.

``` r
##Create a ggplot

ggplot(Energy_agg,aes(x=Energy_Type,y=Total_CO2_EnergyType,fill=as.factor(Calendar_Year)))+
   geom_bar(stat="identity", position = "dodge")+ labs(title="Surrey CO2 Emissions by Energy Type 2018-2020", x="Energy Type",y="Total CO2 Emissions",colour = "Cylinders", caption = "(based on data from  ONS Local Authority CO2 emissions estimates 2005-2020 (kt CO2)", fill="Year")+ 
  geom_label(aes(label = round(Total_CO2_EnergyType,0)),colour = "black", size = 3,vjust = 0, position = position_dodge(1))+theme_classic()+scale_fill_brewer()
```

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

\#Surrey Emission by District and Borough area

``` r
distEmissions<-surreyData%>%filter(Calendar_Year %in% c(2020))%>%group_by(Calendar_Year,Local_Authority)%>%summarise(Total_CO2=sum(Territorial_Emissions_Kt_Co2))

ggplot(distEmissions,aes(x=Total_CO2,y=reorder(Local_Authority,Total_CO2)))+
   geom_bar(stat="identity",color="blue", fill="blue")+  labs(title="Surrey CO2 Emissions by Local Authority 2020 (Total = 5494 KtCO2)", x="Total CO2 Emissions",y="Local Authority",colour = "Cylinders", caption = "(based on data from  ONS Local Authority CO2 emissions estimates 2005-2020 (kt CO2))", fill="Local Authority")+ 
  geom_label(aes(label = round(Total_CO2,0)),colour = "black", size = 3,vjust = 0)+theme_classic()
```

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
distEmissionsTogether<-surreyData%>%filter(Calendar_Year %in% c(2018,2020))%>%group_by(Calendar_Year,Local_Authority)%>%summarise(Total_CO2=sum(Territorial_Emissions_Kt_Co2))


ggplot(distEmissionsTogether,aes(x=Total_CO2,y=reorder(Local_Authority,Total_CO2),fill=Calendar_Year))+
   geom_bar(stat="identity", position = position_dodge(width=.9))+ 
  geom_label(aes(label = round(Total_CO2,0)),colour = "white", size = 3,vjust = 0, position = position_dodge(1))+labs(title="Surrey LA CO2 Emissions 2020 vs. 2018", x="Total CO2",y="Local Authority",colour = "Year")+guides(fill = FALSE) +theme_classic()
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.

![](Surrey_CO2_Analysos_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
