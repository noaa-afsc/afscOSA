# Run OSA residuals

Run OSA residuals

## Usage

``` r
run_osa(
  obs,
  exp,
  N,
  fleet = "Fleet1",
  index,
  years,
  index_label = "Age or Length",
  seed = 99801,
  res = NULL,
  theta = NULL
)
```

## Arguments

- obs:

  matrix of observed ages or lengths (nrow=years, ncol=index of age or
  length bin)

- exp:

  matrix of predicted/expected ages or lengths (same dimension as obs)

- N:

  vector of input sample sizes with length equal to the nrow of obs and
  exp. For multinomial models N will be the sample size used in the
  likelihood; for the 'Dirichlet-multinomial', N will be the input
  sample sizes which are then weighted by the dispersion parameter
  `theta`). The aggregate effective sample size is calculated
  internally. See details.

- fleet:

  character name for fishery or survey fleet, could also identify sex

- index:

  vector giving the index of ages or length bins

- years:

  vector of years associated with the observed ages or lengths

- index_label:

  character value indicating 'age' or 'length bin' depending on comp
  type

- seed:

  A random seed (integer) used to `set.seed` for reproducibility. If
  unspecified a default of 99801 is used. Random values are necessary
  for integer observations.

- res:

  A vector of OSA residuals calculated in the same way as described
  above, meaning the same row/column orientation and the final bin
  removed. This can be used to pass in OSA residuals calculated from
  another source, such as internally as is done in some assessments. If
  NULL the residuals are calculated inside the function, implicitly
  assuming no correlations among ages.

- theta:

  scalar for using the linear Dirichlet-multinomial, if no value is
  provided (the default) the function assumes a multinomial
  distribution, otherwise alpha is calcluated as the sample size N times
  the expected probabilities times theta.

## Value

a list with two elements: (1) `res`: a long-format dataframe with
columns fleet, index_label (indicates whether the comp is age or
length), year, index (age or length bin), resid (osa), and (2) `agg`: a
dataframe of aggregated fits of the composition data with columns fleet,
index_label, index, obs, and exp

## Details

The effective sample size is calculated on the aggregate fit for the
multinomial as sum(e\*(N-e))/sum((o-e)^2). The Dirichlet multinomial is
calculated as (1+theta\*N)/(1+theta) which assumes the linear form from
Thorson et al. (2017).

## Examples

``` r
# GOA pollock info
repfile <- afscOSA::goapkrep
datfile <- afscOSA::goapkdat

# ages and years for age comp data
ages <- 3:10
yrs <- datfile$srv_acyrs1
# observed age comps
myobs <- repfile$Survey_1_observed_and_expected_age_comp[ ,ages]
# predicted age comps from assessment model
myexp <- repfile$Survey_1_observed_and_expected_age_comp[ ,10+ages]
# assumed effective sample sizes
myN <- datfile$multN_srv1 # this gets rounded
#
myfleet='Survey1'
run_osa(obs = myobs, exp = myexp, N = myN, index = ages, years = yrs, index_label = 'Age')
#> $res
#>      fleet index_label year index        resid
#> 1   Fleet1         Age 1992     3 -1.932225851
#> 2   Fleet1         Age 1993     3 -1.090731631
#> 3   Fleet1         Age 1994     3  0.114895721
#> 4   Fleet1         Age 1995     3 -0.604048749
#> 5   Fleet1         Age 1996     3 -0.558059234
#> 6   Fleet1         Age 1997     3  2.010111787
#> 7   Fleet1         Age 1998     3 -0.678634010
#> 8   Fleet1         Age 2000     3  1.536959933
#> 9   Fleet1         Age 2001     3  1.784360025
#> 10  Fleet1         Age 2002     3  1.116773619
#> 11  Fleet1         Age 2003     3 -1.583798680
#> 12  Fleet1         Age 2004     3 -0.063339373
#> 13  Fleet1         Age 2005     3  0.212816050
#> 14  Fleet1         Age 2006     3 -0.122559757
#> 15  Fleet1         Age 2007     3  1.234610327
#> 16  Fleet1         Age 2008     3  0.870214256
#> 17  Fleet1         Age 2009     3 -0.758237780
#> 18  Fleet1         Age 2010     3  1.066135805
#> 19  Fleet1         Age 2012     3 -0.268658365
#> 20  Fleet1         Age 2013     3  1.822619420
#> 21  Fleet1         Age 2014     3 -1.094597477
#> 22  Fleet1         Age 2015     3 -0.523336947
#> 23  Fleet1         Age 2016     3 -0.784310327
#> 24  Fleet1         Age 2017     3 -1.159607893
#> 25  Fleet1         Age 2018     3 -0.738811643
#> 26  Fleet1         Age 2019     3  0.007423564
#> 27  Fleet1         Age 2020     3 -0.782600475
#> 28  Fleet1         Age 2021     3 -1.397326991
#> 29  Fleet1         Age 2022     3 -1.098510844
#> 30  Fleet1         Age 1992     4 -2.643609382
#> 31  Fleet1         Age 1993     4 -1.118959978
#> 32  Fleet1         Age 1994     4 -1.288414524
#> 33  Fleet1         Age 1995     4  0.210223333
#> 34  Fleet1         Age 1996     4 -0.464496505
#> 35  Fleet1         Age 1997     4 -1.565712164
#> 36  Fleet1         Age 1998     4  0.731924700
#> 37  Fleet1         Age 2000     4 -2.243528678
#> 38  Fleet1         Age 2001     4  0.759080117
#> 39  Fleet1         Age 2002     4  0.549732500
#> 40  Fleet1         Age 2003     4  1.768094324
#> 41  Fleet1         Age 2004     4 -0.894489139
#> 42  Fleet1         Age 2005     4  0.298558516
#> 43  Fleet1         Age 2006     4 -0.724859417
#> 44  Fleet1         Age 2007     4  0.954794858
#> 45  Fleet1         Age 2008     4  0.779840367
#> 46  Fleet1         Age 2009     4 -0.142172997
#> 47  Fleet1         Age 2010     4 -0.315189840
#> 48  Fleet1         Age 2012     4  0.271545176
#> 49  Fleet1         Age 2013     4 -0.787995248
#> 50  Fleet1         Age 2014     4  0.431651332
#> 51  Fleet1         Age 2015     4 -0.936773635
#> 52  Fleet1         Age 2016     4  0.866759502
#> 53  Fleet1         Age 2017     4 -0.912911516
#> 54  Fleet1         Age 2018     4  0.114015887
#> 55  Fleet1         Age 2019     4 -0.792019380
#> 56  Fleet1         Age 2020     4  0.435982605
#> 57  Fleet1         Age 2021     4 -1.165259210
#> 58  Fleet1         Age 2022     4 -0.667496773
#> 59  Fleet1         Age 1992     5  0.549424000
#> 60  Fleet1         Age 1993     5 -0.193983476
#> 61  Fleet1         Age 1994     5 -0.625973555
#> 62  Fleet1         Age 1995     5 -0.094417944
#> 63  Fleet1         Age 1996     5  0.415037571
#> 64  Fleet1         Age 1997     5  0.171092960
#> 65  Fleet1         Age 1998     5  0.149226778
#> 66  Fleet1         Age 2000     5 -0.113130623
#> 67  Fleet1         Age 2001     5  0.423036237
#> 68  Fleet1         Age 2002     5 -1.364881128
#> 69  Fleet1         Age 2003     5 -2.073440980
#> 70  Fleet1         Age 2004     5  0.479563726
#> 71  Fleet1         Age 2005     5 -0.295992592
#> 72  Fleet1         Age 2006     5  0.452478130
#> 73  Fleet1         Age 2007     5 -0.337406604
#> 74  Fleet1         Age 2008     5  0.470160700
#> 75  Fleet1         Age 2009     5  1.558811740
#> 76  Fleet1         Age 2010     5  0.174750825
#> 77  Fleet1         Age 2012     5  0.818546282
#> 78  Fleet1         Age 2013     5 -1.005474848
#> 79  Fleet1         Age 2014     5  0.532141462
#> 80  Fleet1         Age 2015     5  0.429113455
#> 81  Fleet1         Age 2016     5  1.125952925
#> 82  Fleet1         Age 2017     5  0.674170201
#> 83  Fleet1         Age 2018     5 -0.560445801
#> 84  Fleet1         Age 2019     5  0.096023976
#> 85  Fleet1         Age 2020     5  0.823362195
#> 86  Fleet1         Age 2021     5 -0.125207239
#> 87  Fleet1         Age 2022     5 -0.797658279
#> 88  Fleet1         Age 1992     6  0.362473978
#> 89  Fleet1         Age 1993     6  0.632387990
#> 90  Fleet1         Age 1994     6 -1.556366992
#> 91  Fleet1         Age 1995     6  0.592823317
#> 92  Fleet1         Age 1996     6  0.276333034
#> 93  Fleet1         Age 1997     6  1.188695734
#> 94  Fleet1         Age 1998     6  0.179341525
#> 95  Fleet1         Age 2000     6  1.159206409
#> 96  Fleet1         Age 2001     6  0.092238048
#> 97  Fleet1         Age 2002     6  0.604651797
#> 98  Fleet1         Age 2003     6  1.285429051
#> 99  Fleet1         Age 2004     6  2.005885557
#> 100 Fleet1         Age 2005     6  0.327055906
#> 101 Fleet1         Age 2006     6 -0.569099821
#> 102 Fleet1         Age 2007     6 -1.093218029
#> 103 Fleet1         Age 2008     6  1.448602092
#> 104 Fleet1         Age 2009     6 -0.243690848
#> 105 Fleet1         Age 2010     6 -0.867310208
#> 106 Fleet1         Age 2012     6  0.609438229
#> 107 Fleet1         Age 2013     6  0.234088754
#> 108 Fleet1         Age 2014     6  0.058372129
#> 109 Fleet1         Age 2015     6  0.991022014
#> 110 Fleet1         Age 2016     6  1.325444372
#> 111 Fleet1         Age 2017     6 -0.593948167
#> 112 Fleet1         Age 2018     6  1.477069707
#> 113 Fleet1         Age 2019     6  0.478617759
#> 114 Fleet1         Age 2020     6 -0.861601433
#> 115 Fleet1         Age 2021     6  0.182941590
#> 116 Fleet1         Age 2022     6 -0.628855123
#> 117 Fleet1         Age 1992     7  0.449104172
#> 118 Fleet1         Age 1993     7 -1.962939556
#> 119 Fleet1         Age 1994     7 -0.138116968
#> 120 Fleet1         Age 1995     7  0.202436472
#> 121 Fleet1         Age 1996     7  1.115478647
#> 122 Fleet1         Age 1997     7  0.443780844
#> 123 Fleet1         Age 1998     7 -0.454978154
#> 124 Fleet1         Age 2000     7 -0.944846014
#> 125 Fleet1         Age 2001     7  0.426167122
#> 126 Fleet1         Age 2002     7 -0.026387676
#> 127 Fleet1         Age 2003     7  0.754511618
#> 128 Fleet1         Age 2004     7  0.750576036
#> 129 Fleet1         Age 2005     7  0.722462386
#> 130 Fleet1         Age 2006     7  0.514466824
#> 131 Fleet1         Age 2007     7  0.131090658
#> 132 Fleet1         Age 2008     7  0.496298959
#> 133 Fleet1         Age 2009     7  0.474085550
#> 134 Fleet1         Age 2010     7 -0.553876887
#> 135 Fleet1         Age 2012     7  0.385779008
#> 136 Fleet1         Age 2013     7 -0.118333216
#> 137 Fleet1         Age 2014     7 -0.067504453
#> 138 Fleet1         Age 2015     7 -0.102235274
#> 139 Fleet1         Age 2016     7  0.269277198
#> 140 Fleet1         Age 2017     7  1.372461133
#> 141 Fleet1         Age 2018     7  1.254717717
#> 142 Fleet1         Age 2019     7  0.699958863
#> 143 Fleet1         Age 2020     7 -0.164211059
#> 144 Fleet1         Age 2021     7 -0.011162996
#> 145 Fleet1         Age 2022     7  0.977448943
#> 146 Fleet1         Age 1992     8 -0.209503399
#> 147 Fleet1         Age 1993     8 -1.093396996
#> 148 Fleet1         Age 1994     8  0.233710826
#> 149 Fleet1         Age 1995     8  0.982723300
#> 150 Fleet1         Age 1996     8  1.184886077
#> 151 Fleet1         Age 1997     8 -0.441911482
#> 152 Fleet1         Age 1998     8 -0.550764345
#> 153 Fleet1         Age 2000     8 -0.046976227
#> 154 Fleet1         Age 2001     8  2.254247682
#> 155 Fleet1         Age 2002     8 -0.540682883
#> 156 Fleet1         Age 2003     8 -0.224445122
#> 157 Fleet1         Age 2004     8  1.405967413
#> 158 Fleet1         Age 2005     8  1.260574738
#> 159 Fleet1         Age 2006     8  1.090308176
#> 160 Fleet1         Age 2007     8 -0.263142263
#> 161 Fleet1         Age 2008     8  1.875405210
#> 162 Fleet1         Age 2009     8 -1.015240677
#> 163 Fleet1         Age 2010     8  0.215557904
#> 164 Fleet1         Age 2012     8  0.446120484
#> 165 Fleet1         Age 2013     8  0.359162948
#> 166 Fleet1         Age 2014     8  0.941404015
#> 167 Fleet1         Age 2015     8 -0.696830738
#> 168 Fleet1         Age 2016     8  0.415422714
#> 169 Fleet1         Age 2017     8 -1.470747208
#> 170 Fleet1         Age 2018     8 -0.910543573
#> 171 Fleet1         Age 2019     8 -1.447382859
#> 172 Fleet1         Age 2020     8 -0.322870527
#> 173 Fleet1         Age 2021     8  0.215901580
#> 174 Fleet1         Age 2022     8 -0.265073713
#> 175 Fleet1         Age 1992     9 -1.094573692
#> 176 Fleet1         Age 1993     9  0.279075518
#> 177 Fleet1         Age 1994     9  0.167921101
#> 178 Fleet1         Age 1995     9 -0.319014014
#> 179 Fleet1         Age 1996     9  1.108032096
#> 180 Fleet1         Age 1997     9 -0.705574815
#> 181 Fleet1         Age 1998     9  1.005417673
#> 182 Fleet1         Age 2000     9  2.527709462
#> 183 Fleet1         Age 2001     9  1.306159826
#> 184 Fleet1         Age 2002     9  0.011811896
#> 185 Fleet1         Age 2003     9  1.244348022
#> 186 Fleet1         Age 2004     9 -0.670388130
#> 187 Fleet1         Age 2005     9 -0.818614309
#> 188 Fleet1         Age 2006     9  0.711226526
#> 189 Fleet1         Age 2007     9 -2.567530680
#> 190 Fleet1         Age 2008     9 -0.199313113
#> 191 Fleet1         Age 2009     9 -1.061893083
#> 192 Fleet1         Age 2010     9 -0.987609059
#> 193 Fleet1         Age 2012     9 -0.300179811
#> 194 Fleet1         Age 2013     9 -0.261089912
#> 195 Fleet1         Age 2014     9  0.410576709
#> 196 Fleet1         Age 2015     9  0.333850002
#> 197 Fleet1         Age 2016     9  1.301015489
#> 198 Fleet1         Age 2017     9  0.461998881
#> 199 Fleet1         Age 2018     9 -0.260700203
#> 200 Fleet1         Age 2019     9 -0.874791500
#> 201 Fleet1         Age 2020     9 -0.142875671
#> 202 Fleet1         Age 2021     9 -0.128146972
#> 203 Fleet1         Age 2022     9  0.752917255
#> 
#> $pearson
#>      fleet index_label year index         resid
#> 1   Fleet1         Age 1992     3 -1.7506991951
#> 2   Fleet1         Age 1993     3 -1.3514195477
#> 3   Fleet1         Age 1994     3 -0.2900370726
#> 4   Fleet1         Age 1995     3 -0.3644330543
#> 5   Fleet1         Age 1996     3 -0.2621727407
#> 6   Fleet1         Age 1997     3  2.2551467787
#> 7   Fleet1         Age 1998     3 -1.0037472187
#> 8   Fleet1         Age 2000     3  1.4764287557
#> 9   Fleet1         Age 2001     3  1.8308129945
#> 10  Fleet1         Age 2002     3  1.3852733996
#> 11  Fleet1         Age 2003     3 -1.6293151867
#> 12  Fleet1         Age 2004     3 -0.1016524847
#> 13  Fleet1         Age 2005     3 -0.1114698377
#> 14  Fleet1         Age 2006     3 -0.1211028281
#> 15  Fleet1         Age 2007     3  1.4610123292
#> 16  Fleet1         Age 2008     3  1.1112546451
#> 17  Fleet1         Age 2009     3 -0.8293440471
#> 18  Fleet1         Age 2010     3  1.1567495876
#> 19  Fleet1         Age 2012     3 -0.2117204658
#> 20  Fleet1         Age 2013     3  1.8453100255
#> 21  Fleet1         Age 2014     3 -1.1459029170
#> 22  Fleet1         Age 2015     3 -0.1814717835
#> 23  Fleet1         Age 2016     3 -1.0361835365
#> 24  Fleet1         Age 2017     3 -0.2430964347
#> 25  Fleet1         Age 2018     3 -0.0881387723
#> 26  Fleet1         Age 2019     3 -0.4074316309
#> 27  Fleet1         Age 2020     3 -1.0187820327
#> 28  Fleet1         Age 2021     3 -1.6712671395
#> 29  Fleet1         Age 2022     3 -0.6011716131
#> 30  Fleet1         Age 1992     4 -1.2995877031
#> 31  Fleet1         Age 1993     4 -0.9316364369
#> 32  Fleet1         Age 1994     4 -1.1920713158
#> 33  Fleet1         Age 1995     4 -0.1730815526
#> 34  Fleet1         Age 1996     4 -0.9509523148
#> 35  Fleet1         Age 1997     4 -0.8387930349
#> 36  Fleet1         Age 1998     4  1.1185244272
#> 37  Fleet1         Age 2000     4 -1.0739920962
#> 38  Fleet1         Age 2001     4 -0.2897436208
#> 39  Fleet1         Age 2002     4  0.0077653423
#> 40  Fleet1         Age 2003     4  2.8907048285
#> 41  Fleet1         Age 2004     4 -1.0865358346
#> 42  Fleet1         Age 2005     4 -0.1062001452
#> 43  Fleet1         Age 2006     4 -0.8726653092
#> 44  Fleet1         Age 2007     4  0.2314113323
#> 45  Fleet1         Age 2008     4  0.0805806454
#> 46  Fleet1         Age 2009     4  0.4054189537
#> 47  Fleet1         Age 2010     4 -0.6885231965
#> 48  Fleet1         Age 2012     4  0.2008560041
#> 49  Fleet1         Age 2013     4 -0.7698894723
#> 50  Fleet1         Age 2014     4  0.7629315984
#> 51  Fleet1         Age 2015     4 -0.5634368560
#> 52  Fleet1         Age 2016     4  1.5586429821
#> 53  Fleet1         Age 2017     4 -1.0944554938
#> 54  Fleet1         Age 2018     4 -0.2893674813
#> 55  Fleet1         Age 2019     4 -0.2336387790
#> 56  Fleet1         Age 2020     4  0.2491699929
#> 57  Fleet1         Age 2021     4 -0.4209819055
#> 58  Fleet1         Age 2022     4 -0.6258465503
#> 59  Fleet1         Age 1992     5  2.3539207709
#> 60  Fleet1         Age 1993     5  1.0365190940
#> 61  Fleet1         Age 1994     5  0.1761153885
#> 62  Fleet1         Age 1995     5 -0.1647316223
#> 63  Fleet1         Age 1996     5  0.3704810809
#> 64  Fleet1         Age 1997     5 -0.6277708316
#> 65  Fleet1         Age 1998     5  0.5565285870
#> 66  Fleet1         Age 2000     5 -0.2240091175
#> 67  Fleet1         Age 2001     5  0.1034445432
#> 68  Fleet1         Age 2002     5 -0.6999646099
#> 69  Fleet1         Age 2003     5 -0.7405965345
#> 70  Fleet1         Age 2004     5  1.5303281630
#> 71  Fleet1         Age 2005     5 -0.2620126013
#> 72  Fleet1         Age 2006     5  0.2806640624
#> 73  Fleet1         Age 2007     5 -0.7183915526
#> 74  Fleet1         Age 2008     5 -0.5076226205
#> 75  Fleet1         Age 2009     5  1.9455332908
#> 76  Fleet1         Age 2010     5 -0.1258666363
#> 77  Fleet1         Age 2012     5  0.5817302381
#> 78  Fleet1         Age 2013     5 -0.8907751037
#> 79  Fleet1         Age 2014     5  0.3553748509
#> 80  Fleet1         Age 2015     5  1.6068537625
#> 81  Fleet1         Age 2016     5 -0.6693569749
#> 82  Fleet1         Age 2017     5  1.7209417103
#> 83  Fleet1         Age 2018     5 -0.2925143457
#> 84  Fleet1         Age 2019     5 -0.2633149063
#> 85  Fleet1         Age 2020     5 -0.2106675961
#> 86  Fleet1         Age 2021     5  0.6724293040
#> 87  Fleet1         Age 2022     5 -0.2496551873
#> 88  Fleet1         Age 1992     6  1.3948241457
#> 89  Fleet1         Age 1993     6  1.8888895336
#> 90  Fleet1         Age 1994     6 -0.4517128371
#> 91  Fleet1         Age 1995     6  1.1274856936
#> 92  Fleet1         Age 1996     6  0.3566766245
#> 93  Fleet1         Age 1997     6 -0.5919637607
#> 94  Fleet1         Age 1998     6 -0.5723716151
#> 95  Fleet1         Age 2000     6  0.7073296050
#> 96  Fleet1         Age 2001     6 -1.1275908120
#> 97  Fleet1         Age 2002     6 -0.5777906349
#> 98  Fleet1         Age 2003     6 -0.4822384848
#> 99  Fleet1         Age 2004     6  0.6853417005
#> 100 Fleet1         Age 2005     6  0.7027622587
#> 101 Fleet1         Age 2006     6 -0.1229981703
#> 102 Fleet1         Age 2007     6 -0.7962006378
#> 103 Fleet1         Age 2008     6 -0.3889572209
#> 104 Fleet1         Age 2009     6 -0.3994703535
#> 105 Fleet1         Age 2010     6 -0.5519323804
#> 106 Fleet1         Age 2012     6 -0.3549623792
#> 107 Fleet1         Age 2013     6  0.1269761629
#> 108 Fleet1         Age 2014     6  0.1780356753
#> 109 Fleet1         Age 2015     6 -0.2557408877
#> 110 Fleet1         Age 2016     6 -0.4520054918
#> 111 Fleet1         Age 2017     6 -0.7933115102
#> 112 Fleet1         Age 2018     6  1.2709702796
#> 113 Fleet1         Age 2019     6  0.0151224783
#> 114 Fleet1         Age 2020     6 -0.2061813250
#> 115 Fleet1         Age 2021     6 -0.1984463462
#> 116 Fleet1         Age 2022     6 -0.7640409702
#> 117 Fleet1         Age 1992     7  1.0692551519
#> 118 Fleet1         Age 1993     7 -0.4406775408
#> 119 Fleet1         Age 1994     7  0.6783873817
#> 120 Fleet1         Age 1995     7 -0.3787738227
#> 121 Fleet1         Age 1996     7  1.0718816539
#> 122 Fleet1         Age 1997     7 -0.6015766313
#> 123 Fleet1         Age 1998     7 -0.5343644056
#> 124 Fleet1         Age 2000     7 -0.6142129765
#> 125 Fleet1         Age 2001     7 -0.1290526656
#> 126 Fleet1         Age 2002     7 -0.6323722841
#> 127 Fleet1         Age 2003     7 -0.4031024607
#> 128 Fleet1         Age 2004     7 -0.4739130406
#> 129 Fleet1         Age 2005     7  0.5759409381
#> 130 Fleet1         Age 2006     7  0.5594542609
#> 131 Fleet1         Age 2007     7 -0.4614796772
#> 132 Fleet1         Age 2008     7 -0.4411190308
#> 133 Fleet1         Age 2009     7 -0.3037821057
#> 134 Fleet1         Age 2010     7 -0.2830174738
#> 135 Fleet1         Age 2012     7  0.1507347333
#> 136 Fleet1         Age 2013     7 -0.7936162108
#> 137 Fleet1         Age 2014     7 -0.0696920729
#> 138 Fleet1         Age 2015     7 -0.2641421056
#> 139 Fleet1         Age 2016     7 -0.2532296801
#> 140 Fleet1         Age 2017     7 -0.4464052090
#> 141 Fleet1         Age 2018     7 -0.9664750000
#> 142 Fleet1         Age 2019     7  1.1626944836
#> 143 Fleet1         Age 2020     7  0.6536861057
#> 144 Fleet1         Age 2021     7 -0.1778985502
#> 145 Fleet1         Age 2022     7 -0.2200479409
#> 146 Fleet1         Age 1992     8  0.5866978251
#> 147 Fleet1         Age 1993     8 -0.4791158672
#> 148 Fleet1         Age 1994     8 -0.4079412757
#> 149 Fleet1         Age 1995     8  0.6946892247
#> 150 Fleet1         Age 1996     8  0.2101195544
#> 151 Fleet1         Age 1997     8 -0.6988675450
#> 152 Fleet1         Age 1998     8 -0.5315847865
#> 153 Fleet1         Age 2000     8 -0.3912884765
#> 154 Fleet1         Age 2001     8 -0.5958447223
#> 155 Fleet1         Age 2002     8 -0.6260669925
#> 156 Fleet1         Age 2003     8 -0.4266447987
#> 157 Fleet1         Age 2004     8 -0.3889996444
#> 158 Fleet1         Age 2005     8 -0.4782166465
#> 159 Fleet1         Age 2006     8  0.9560870719
#> 160 Fleet1         Age 2007     8  0.0004597897
#> 161 Fleet1         Age 2008     8 -0.6798475485
#> 162 Fleet1         Age 2009     8 -0.3365504654
#> 163 Fleet1         Age 2010     8 -0.2089467353
#> 164 Fleet1         Age 2012     8 -0.5207240098
#> 165 Fleet1         Age 2013     8 -0.6052186080
#> 166 Fleet1         Age 2014     8  0.3994138361
#> 167 Fleet1         Age 2015     8 -0.2897033439
#> 168 Fleet1         Age 2016     8 -0.2439427446
#> 169 Fleet1         Age 2017     8 -0.2630734942
#> 170 Fleet1         Age 2018     8 -0.4661597440
#> 171 Fleet1         Age 2019     8 -0.8743600820
#> 172 Fleet1         Age 2020     8  0.3244612405
#> 173 Fleet1         Age 2021     8  1.1962213483
#> 174 Fleet1         Age 2022     8 -0.1740848880
#> 175 Fleet1         Age 1992     9 -0.5314382076
#> 176 Fleet1         Age 1993     9  1.1850650532
#> 177 Fleet1         Age 1994     9  1.9075570922
#> 178 Fleet1         Age 1995     9 -0.4780209676
#> 179 Fleet1         Age 1996     9 -0.6149903118
#> 180 Fleet1         Age 1997     9 -0.6240780360
#> 181 Fleet1         Age 1998     9  1.1401266865
#> 182 Fleet1         Age 2000     9 -0.4305577909
#> 183 Fleet1         Age 2001     9 -0.4563489925
#> 184 Fleet1         Age 2002     9 -0.3964716349
#> 185 Fleet1         Age 2003     9 -0.4266251842
#> 186 Fleet1         Age 2004     9 -0.4096770725
#> 187 Fleet1         Age 2005     9 -0.4295786038
#> 188 Fleet1         Age 2006     9 -0.4327860094
#> 189 Fleet1         Age 2007     9 -0.5900746119
#> 190 Fleet1         Age 2008     9 -0.5500695600
#> 191 Fleet1         Age 2009     9 -0.4825699465
#> 192 Fleet1         Age 2010     9 -0.2419075940
#> 193 Fleet1         Age 2012     9 -0.3070640827
#> 194 Fleet1         Age 2013     9 -0.3485913282
#> 195 Fleet1         Age 2014     9 -0.6428793967
#> 196 Fleet1         Age 2015     9 -0.2282869573
#> 197 Fleet1         Age 2016     9 -0.2627499970
#> 198 Fleet1         Age 2017     9 -0.2478267664
#> 199 Fleet1         Age 2018     9 -0.3225302749
#> 200 Fleet1         Age 2019     9 -0.3892130435
#> 201 Fleet1         Age 2020     9  0.9626960513
#> 202 Fleet1         Age 2021     9  1.2824488259
#> 203 Fleet1         Age 2022     9  1.5902967700
#> 204 Fleet1         Age 1992    10  1.5432450781
#> 205 Fleet1         Age 1993    10 -0.5310405665
#> 206 Fleet1         Age 1994    10  1.0981884112
#> 207 Fleet1         Age 1995    10 -0.6727547061
#> 208 Fleet1         Age 1996    10 -0.5949126244
#> 209 Fleet1         Age 1997    10 -0.5247262480
#> 210 Fleet1         Age 1998    10 -0.6448602211
#> 211 Fleet1         Age 2000    10 -0.6438335592
#> 212 Fleet1         Age 2001    10 -0.6853246602
#> 213 Fleet1         Age 2002    10 -0.4497118433
#> 214 Fleet1         Age 2003    10 -0.3883986223
#> 215 Fleet1         Age 2004    10 -0.4821286511
#> 216 Fleet1         Age 2005    10 -0.5731418433
#> 217 Fleet1         Age 2006    10 -0.5409443699
#> 218 Fleet1         Age 2007    10 -0.5395683454
#> 219 Fleet1         Age 2008    10 -0.4015129932
#> 220 Fleet1         Age 2009    10 -0.4477420298
#> 221 Fleet1         Age 2010    10 -0.3778731748
#> 222 Fleet1         Age 2012    10 -0.4030384647
#> 223 Fleet1         Age 2013    10 -0.2988679390
#> 224 Fleet1         Age 2014    10 -0.4654623736
#> 225 Fleet1         Age 2015    10 -0.1921987225
#> 226 Fleet1         Age 2016    10 -0.2502319978
#> 227 Fleet1         Age 2017    10 -0.3170787624
#> 228 Fleet1         Age 2018    10 -0.3946406030
#> 229 Fleet1         Age 2019    10 -0.3999712000
#> 230 Fleet1         Age 2020    10 -0.3491009104
#> 231 Fleet1         Age 2021    10  1.2374688727
#> 232 Fleet1         Age 2022    10  1.6868080116
#> 
#> $agg
#>    fleet index_label index obs       exp   obs_prop   exp_prop lwr upr
#> 1 Fleet1         Age     3  67 68.260479 0.31162791 0.31749060  55  82
#> 2 Fleet1         Age     4  42 46.239235 0.19534884 0.21506621  35  58
#> 3 Fleet1         Age     5  40 32.606567 0.18604651 0.15165845  23  43
#> 4 Fleet1         Age     6  27 25.179822 0.12558140 0.11711545  16  35
#> 5 Fleet1         Age     7  18 17.961800 0.08372093 0.08354326  10  26
#> 6 Fleet1         Age     8   9 10.819054 0.04186047 0.05032118   5  17
#> 7 Fleet1         Age     9   7  6.783048 0.03255814 0.03154906   2  12
#> 8 Fleet1         Age    10   5  7.149994 0.02325581 0.03325579   3  13
#>      lwr_prop   upr_prop ISS   ESS
#> 1 0.255813953 0.38139535 215 434.9
#> 2 0.162790698 0.26976744 215 434.9
#> 3 0.106976744 0.20000000 215 434.9
#> 4 0.074418605 0.16279070 215 434.9
#> 5 0.046511628 0.12093023 215 434.9
#> 6 0.023255814 0.07906977 215 434.9
#> 7 0.009302326 0.05581395 215 434.9
#> 8 0.013953488 0.06046512 215 434.9
#> 
out1$res # osa residual for each age and year
#> Error: object 'out1' not found
out1$agg # observed and expected value for each age aggregated across all yrs
#> Error: object 'out1' not found
```
